library(rCharts)
library(httr)
library(RJSONIO)
library(data.table)

source("dc.r")

#docs <- readRDS("docs.rds")[1:1000]

docs <- pm_doc_info("rift valley fever", max_ids=1000)

doc_proc <- ldatools::preprocess(data=docs$title_and_abstract, 
  stopwords=stopwords(), stem=TRUE)
if (any(doc_proc$category != 0)) {
  docs <- docs[doc_proc$category == 0,]
  doc_proc <- ldatools::preprocess(data=docs$title_and_abstract, 
    stopwords=stopwords(), stem=TRUE)
}

lsa_cluster <- run_lsa(doc_proc, num_topics=6)
lda_cluster <- run_lda(doc_proc, num_topics=6)

#new_clusters <- register_clusters(lda_cluster$doc_cluster$cluster,
#                                  lsa_cluster$doc_cluster$cluster)
#lsa_cluster$doc_cluster$cluster <- new_clusters$cluster2

tdm <- create_tdm(doc_proc)
document_basis <- irlb_lsa(tdm, dims=5)$tk

components <- 1:2
proj_docs <- t(tdm) %*% document_basis[,components]


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

data <- data.frame(list(x=proj_docs[,1], y=proj_docs[,2], 
  cluster=lsa_cluster$doc_cluster$cluster, url=docs$url), 
  stringsAsFactors=FALSE)

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
    clean_up_title(entry, max_lines, width)
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

data$name <- create_html_caption(docs$title, docs$author, docs$date, 
  docs$journal)

highcharts_viz(data, by="cluster")
