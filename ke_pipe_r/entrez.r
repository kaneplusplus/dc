require(XML)
require(itertools)
require(foreach)

query_ids <- function(query) {
  esearch=paste('http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&retmode=xml&retmax=10000000&term=', query, sep="")
  html_doc <- htmlParse(esearch)
  ids <- xmlSApply(getNodeSet(html_doc, "//idlist//id"), xmlValue)
}

pm_title_abstracts <- function(query, max_ids=Inf, verbose=TRUE, 
                               chunkSize=200) {
  ids <- query_ids(query)
  if (verbose)
    cat(length(ids), "ID's found\n")
  foreach(it=isplitIndices(min(length(ids), max_ids), chunkSize=chunkSize), 
          .combine=c) %do% {
    if (verbose)
      cat(it[1], ":", it[length(it)], "\n", sep="")
    id_string <- paste(ids[it], sep="", collapse=",")
    abstract_query <- paste('http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?&db=pubmed&retmode=xml&id=', id_string, sep="")
    doc <- htmlParse(abstract_query)
    foreach(element=getNodeSet(doc, "//medlinecitation/article"), 
            .combine=c) %do% {
      abs <- xmlSApply(getNodeSet(element, ".//abstracttext"), xmlValue)
      if (length(abs) > 1) 
        abs <- paste(abs, collapse=" ")
      title <- xmlSApply(getNodeSet(element, ".//articletitle"), xmlValue)
      if (length(title) > 1) 
        title <- paste(title, collapse=" ")
      # We'll only take articles with titles and abstracts.
      ret <- ""
      if (length(abs) > 0 && length(title) > 0)
        ret <- paste(title, abs)
      ret
    }
  }
}
