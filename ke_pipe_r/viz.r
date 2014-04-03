require(rCharts)
require(httr)
require(RJSONIO)
require(ldatools)
require(tm)

nvd3viz <- function(proj_docs, cluster, titles) {
  x <- as.data.frame(cbind(as.matrix(proj_docs), cluster))
  names(x) <- c("PC1", "PC2", "group")

  nP <- nPlot(
    PC2 ~ PC1,
    group = "group",
    data = x,
    type = "scatterChart")
  #nP$xAxis(tickFormat = "#!d3.format('.2%')!#")
  #nP$yAxis(tickFormat = "#!d3.format('.2%')!#")
  nP$chart(
    showDistX = TRUE,
    showDistY = TRUE,
    tooltipContent = "#!function(key, y, e, graph) {
                     return '<h3>Group: ' + key + '<br>' +
                     graph.point.titles + '</a></h3>';
                     }!#")
  nP
}

clean_up_date_string <- function(ds) {
  ret <- gsub("NULL", "", ds)
  gsub("[ ]+", " ", ret)
}

clean_up_entry <- function(entry, max_lines=3, width=30) {
  entry_vector <- strwrap(entry, width=width)
  if (length(entry_vector) > max_lines) {
    title_vector <- entry_vector[1:max_lines]
    entry_vector[max_lines] <- paste(entry_vector[max_lines], "...", sep="")
  }
  paste(entry_vector, collapse="<br>")
}

clean_up_entries <- function(entries, max_lines=3, width=30) {
  foreach (entry=entries, .combine=c) %do% {
    clean_up_entry(entry, max_lines, width)
  }
}

create_html_caption <- function(doc_title, author, date, journal) {
  str <- paste("<b>%s</b><table>",
    "<tr><td align='left'><b>Author:</b></td><td>%s</td></tr>",
    "<td align='left'><b>Date:</b></td><td>%s</td></tr>",
    "<td align='left'><b>Journal:</b></td><td>%s</td></tr></table>")
  sprintf(str, 
    clean_up_entries(doc_title, width=40), 
    clean_up_entries(author), 
    clean_up_date_string(date), 
    clean_up_entries(journal))
}

highcharts_viz <- function(data, x_name="x", y_name="y", by="", xlab="", 
                           ylab="", title="", 
                           legend_title="", subtitle="") {
  names(data)[match(c(x_name, y_name), names(data))] <- c("x", "y")

  data_list <- lapply(split(data, data[,by]), function(x) {
    res <- lapply(split(x, rownames(x)), as.list)
    names(res) <- NULL
    res
  })

  viz <- rCharts::Highcharts$new()
  invisible(sapply(data_list, function(x) {
      apply_cat <- eval(parse(text=paste("x[[1]]$", by, sep="")))
      viz$series(data=x, type="scatter", name=apply_cat) 
    }))

  viz$plotOptions(
    scatter = list(
      cursor = "pointer", 
      point = list(
        events = list(
          click = "#! function() { window.open(this.options.url); } !#")), 
      marker = list(
        symbol = "circle", 
        radius = 5
      )
    )
  )

  viz$xAxis(title = list(text=xlab), 
    labels = list(format="{value} "))
  viz$yAxis(title = list(text=ylab), 
    labels = list(format="{value} "))
  viz$tooltip(useHTML=TRUE,
    formatter="#! function() { return this.point.name; } !#")
 
  viz$legend(
    align = 'right', 
    verticalAlign = 'middle', 
    layout = 'vertical', 
    title = list(text=legend_title)
  )
  viz$title(text=title)
  viz$subtitle(text=subtitle) 
  viz
}

get_session_id <- function() {
  if (exists("session"))
    as.character(session$clientData$url_hostname)
  else
    "127.0.0.1"
}

get_docs_and_procs <- function(query, max_ids=Inf, verbose=FALSE) {
  session_id <- get_session_id()
  doc_key <- paste(session_id, query, "docs")
  doc_proc_key <- paste(session_id, query, "doc_proc")
  max_id_key <- paste(session_id, query, "max_id_key")
  if (verbose) {
    cat("Session id is", session_id, "\n", 
        "doc_key is", doc_key, "\n",
        "doc_proc_key is", doc_proc_key, "\n",
        "max_id_key is", max_id_key, "\n")
  }
  if (redisExists(doc_key) && redisExists(doc_proc_key) && 
      redisGet(max_id_key) == max_ids) {
    if (verbose)
      cat("Docs and doc procs found in Redis.\n")
    docs <- redisGet(doc_key)
    doc_proc <- redisGet(doc_proc_key)
  } else {
    if (verbose)
      cat("Docs and doc procs not found in Redis.\n")
    docs <- pm_query(query, max_ids)
    doc_proc <- ldatools::preprocess(data=docs$title_and_abstract, 
      stopwords=stopwords(), stem=TRUE)
    if (any(doc_proc$category != 0)) {
      docs <- docs[doc_proc$category == 0,]
      doc_proc <- ldatools::preprocess(data=docs$title_and_abstract, 
        stopwords=stopwords(), stem=TRUE)
    }
    redisSet(max_id_key, max_ids)
    redisSet(doc_key, docs)
    redisSet(doc_proc_key, doc_proc)
  }
  ret <- list("docs"=docs, "doc_proc"=doc_proc)
  redisExpireAt(doc_key, 
    as.integer(as.POSIXct(Sys.time())+options()$expire_time))
  redisExpireAt(doc_proc_key, 
    as.integer(as.POSIXct(Sys.time())+options()$expire_time))
  redisExpireAt(max_id_key, 
    as.integer(as.POSIXct(Sys.time())+options()$expire_time))
  ret
}

# This could be done slighly better by caching the entire document basis.
get_proj_docs <- function(doc_proc, query, max_ids, components, verbose=FALSE) {
  proj_doc_key <- paste(get_session_id(), query, max_ids, 
                       paste(components, collapse= " "))
  if (verbose)
    cat("proj_doc_key is", proj_doc_key, "\n")
  if (redisExists(proj_doc_key)) {
    proj_docs <- redisGet(proj_doc_key)
  } else {
    tdm <- create_tdm(doc_proc)
    document_basis <- irlb_lsa(tdm, dims=max(components))$tk

    components <- 1:2
    proj_docs <- t(tdm) %*% document_basis[,components]

    if (sum(proj_docs[,1] > 0) < nrow(proj_docs))
      proj_docs <- -1 * proj_docs

    redisSet(proj_doc_key, proj_docs)
  }
  redisExpireAt(proj_doc_key,
    as.integer(as.POSIXct(Sys.time())+options()$expire_time))
  proj_docs
}

memoize <- function(expr, key=NULL, expire_time=Inf, verbose=FALSE, 
                    envir=parent.frame()) {
  if (is.null(key)) {
    key <- paste(substitute(expr), collapse="")
  }
  if (redisExists(key)) {
    ret <- redisGet(key)
  } else {
    ret <- eval(substitute(expr), envir=envir)
    redisSet(key, ret)
  }
  if (expire_time < Inf) {
    redisExpireAt(proj_doc_key,
      as.integer(as.POSIXct(Sys.time())+expire_time))
  }
  ret
}

create_viz <- function(query, max_ids, num_clusters, cluster_algo="lsa", 
  components=1:2, verbose=FALSE, ...) {

  cat("getting docs and procs\n")
  l <- get_docs_and_procs(query, max_ids, verbose=verbose)
  doc_proc <- l$doc_proc
  docs <- l$docs

  if (cluster_algo == "lsa") {
    cluster_obj <- memoize(
      run_lsa(doc_proc, num_topics=num_clusters, ...),
      paste(get_session_id(), query, "lsa", num_clusters))
  } else if (cluster_algo == "lda") {
    cluster_obj <- memoize(
      run_lda(doc_proc, num_topics=num_clusters, ...),
      paste(get_session_id(), query, "lda", num_clusters))
  } else {
    stop("Unsupported clustering algorithm")
  }

  proj_docs <- get_proj_docs(doc_proc, query, max_ids, components, 
                             verbose=verbose)

  data <- data.frame(list(x=proj_docs[,1], y=proj_docs[,2], 
    cluster=cluster_obj$doc_cluster$cluster, url=docs$url), 
    stringsAsFactors=FALSE)

  data$name <- create_html_caption(docs$title, docs$author, docs$date, 
    docs$journal)

  highcharts_viz(data, by="cluster")
}

