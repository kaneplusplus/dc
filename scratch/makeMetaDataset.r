require(XML)
require(foreach)

doc <- xmlParse("rvf_result.xml", error=function(...){})

x <- foreach(n=getNodeSet(doc, "//PubmedArticle"), .combine=rbind) %do% {
  numAuth <- length(getNodeSet(n, ".//Author"))
  if (length(numAuth) == 0)
    numAuth <- NA
  pmid <- xmlValue(getNodeSet(n, ".//PMID")[[1]])
  if (length(pmid) == 0)
    pmid <- NA
  lasts <- xmlSApply(getNodeSet(n, ".//AuthorList//Author//LastName"), xmlValue)
  firsts <- xmlSApply(getNodeSet(n, ".//AuthorList//Author//ForeName"), 
    xmlValue)
  authSet <- paste(firsts, lasts, collapse=";")
  if (length(authSet) == 0)
    authSet <- NA
  lang <- xmlValue(getNodeSet(n, ".//Language")[[1]])
  if (length(lang) == 0)
    lang <- NA
  pubTypeSet <- paste(xmlSApply(getNodeSet(n, ".//PublicationType"), xmlValue),
    collapse=";")
  if (length(pubTypeSet) == 0)
    pubTypeSet <- NA
  eArticleYear <- xmlValue(
    getNodeSet(n, './/ArticleDate[@DateType="Electronic"]//Year')[[1]])
  eArticleMonth <- xmlValue(
    getNodeSet(n, './/ArticleDate[@DateType="Electronic"]//Month')[[1]])
  eArticleDay <- xmlValue(
    getNodeSet(n, './/ArticleDate[@DateType="Electronic"]//Day')[[1]])
  date <- strptime(paste(eArticleYear, eArticleMonth, eArticleDay, sep="-"), 
    format = "%Y-%m-%d")
  if (length(date) == 0)
    date <- NA
  journal <- xmlValue(getNodeSet(n, './/MedlineJournalInfo')[[1]])
  if (length(journal) == 0)
    journal <- NA
  keywordSet <- paste(xmlSApply(getNodeSet(n, './/Keyword'), xmlValue), 
    collapse=";")
  if (length(keywordSet) == 0)
    keywordSet <- NA
  l <- list(pmid=pmid, authSet=authSet, lang=lang, pubTypeSet=pubTypeSet,
    date=date, journal=journal, keywordSet=keywordSet)
  data.frame(l, stringsAsFactors=TRUE)
}

x$pmid <- as.integer(x$pmid)
x$authSet <- toupper(as.character(x$authSet))
x$lang <- toupper(x$lang)
x$pubTypeSet <- toupper(x$pubTypeSet)
x$journal <- toupper(x$journal)
x$keywordSet <- toupper(as.character(x$keywordSet))
write.csv(x, "pubmed_rvf.csv", row.names=FALSE)
