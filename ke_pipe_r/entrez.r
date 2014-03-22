require(XML)
require(itertools)
require(foreach)

query_ids <- function(query) {
  esearch=paste('http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&retmode=xml&retmax=10000000&term=', query, sep="")
  html_doc <- htmlParse(esearch)
  ids <- xmlSApply(getNodeSet(html_doc, "//idlist//id"), xmlValue)
}

get_author_list <- function(element) {
  lasts <- xmlSApply(getNodeSet(element, ".//authorlist//author//lastname"),
    xmlValue)
  initials <- xmlSApply(getNodeSet(element, ".//authorlist//author//initials"),
    xmlValue)
  firsts <- xmlSApply(getNodeSet(element, ".//authorlist//author/forename"),
    xmlValue)
  names <- paste(firsts, initials, lasts)
  if (length(names) > 3) {
    names <- paste(names[1], "et al.")
  } else {
    names <- paste(names, collapse=" and ")
  }
  names
}

get_publication_type <- function(element) {
  paste(xmlSApply(getNodeSet(element, ".//publicationtype"), xmlValue),
        collapse=",")
}

get_journal <- function(element) {
  xmlSApply(getNodeSet(element, ".//journal//title"), xmlValue)[1]
}

get_date <- function(element) {
  year <- xmlSApply(getNodeSet(element, 
    ".//journal//journalissue//pubdate//year"), xmlValue)[1]
  month <- xmlSApply(getNodeSet(element, 
    ".//journal//journalissue//pubdate//month"), xmlValue)[1]
  day <- xmlSApply(getNodeSet(element, 
    ".//journal//journalissue//pubdate//day"), xmlValue)[1]
  paste(month, day, year)
}

pm_doc_info <- function(query, max_ids=Inf, verbose=TRUE, 
                               chunkSize=200) {
  ids <- query_ids(query)
  if (verbose)
    cat(length(ids), "ID's found\n")
  foreach(it=isplitIndices(min(length(ids), max_ids), chunkSize=chunkSize), 
          .combine=rbind) %do% {
    if (verbose)
      cat(it[1], ":", it[length(it)], "\n", sep="")
    id_string <- paste(ids[it], sep="", collapse=",")
    abstract_query <- paste('http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?&db=pubmed&retmode=xml&id=', id_string, sep="")
    doc <- htmlParse(abstract_query)
    foreach(element=getNodeSet(doc, "//medlinecitation/article"), 
            pmid=ids[1:length(it)],
            .combine=rbind) %do% {
      url <- paste("http://www.ncbi.nlm.nih.gov/pubmed/?term=",
        pmid, "%5Buid%5D", sep="")
      abstract <- xmlSApply(getNodeSet(element, ".//abstracttext"), xmlValue)
      if (length(abstract) > 1) 
        abstract <- paste(abstract, collapse=" ")
      title <- xmlSApply(getNodeSet(element, ".//articletitle"), xmlValue)
      if (length(title) > 1) 
        title <- paste(title, collapse=" ")
      # We'll only take articles with titles and abstracts.
      title_and_abstract <- ""
      if (length(abstract) > 0 && length(title) > 0)
        title_and_abstract <- paste(title, abstract)
      author <- get_author_list(element)
      publication_type <- get_publication_type(element)
      journal <- get_journal(element)
      date <- get_date(element)
      data.frame(list(title=title, author=author, date=date, journal=journal,
        publication_type=publication_type, 
        title_and_abstract=title_and_abstract), stringsAsFactors=FALSE)
    }
  }
}

pm_title_abstracts <- function(query, max_ids=Inf, verbose=TRUE,
                               chunkSize=200) {
  pm_doc_info(query, max_ids, verbose, chunkSize)$title_and_abstract
}
