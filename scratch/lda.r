library(ldatools)
library(tm)
library(doMC)
library(proxy)
library(RJSONIO)

registerDoMC()

# token table
tt <- function(t) {
  input <- preprocess(data=t, exact=NULL, partial=NULL, subs=NULL,
    stopwords=stopwords(), verbose=FALSE, quiet=TRUE, stem=TRUE)
  data.frame(Token=input$vocab, Freq=as.numeric(table(input$token.id)))
}

#x <- read.csv("title_abstract.csv", as.is=TRUE)
x <- read.csv("query_return.csv", as.is=TRUE)
x$abstract[is.na(x$abstract)] <- ""

ta <- paste(x$title, x$abstract)
ta <- tolower(ta)

#excludes <- c("virus", "fever", "rift", "valley", "rvfv", "rvf", "viral", ",",
#  ")", "\\(", "\\.", as.character(0:9), "es")
#for (e in excludes) 
#  ta <- gsub(e, " ", ta)

#ta <- gsub("[ ]+", " ", ta)

#cutoff <- 10 

input <- preprocess(data=ta, exact=NULL, partial=NULL, subs=NULL,
  stopwords=stopwords(), verbose=TRUE, quiet=FALSE, stem=TRUE)
#  stopwords=stopwords(), cutoff=cutoff, verbose=TRUE, quiet=FALSE, stem=TRUE)

if (any(input$category < 0)) {
  ta <- ta[input$category >= 0]

  input <- preprocess(data=ta, exact=NULL, partial=NULL, subs=NULL,
    stopwords=stopwords(), verbose=TRUE, quiet=FALSE, stem=TRUE)
#    stopwords=stopwords(), cutoff=cutoff, verbose=TRUE, quiet=FALSE, stem=TRUE)
}

N <- length(input$token.id)  # total number of tokens in the data
D <- max(input$doc.id)  # total number of documents that will go 
  # into the topic model
W <- length(input$vocab)  # number of tokens in the 
  # vocabulary (i.e. number of unique tokens)

K <- 6 # number of topics
G <- 1000 # number of iterations

alpha <- 1/K # add one pseudo-observation to each document-topic distribution
beta <- 0.01*N/(W*K)

keep <- seq(550, G, by=50)  # Here, keep every 50 iterations, to reduce autocorrelation in the saved samples:

fit <- fitLDA(token.id=input$token.id, doc.id=input$doc.id, K=K,   
  n.chains=1, n.iter=G, topics.init=NULL, alpha=alpha, beta=beta, 
  keep=keep, print=20)

docProb <- getProbs(token.id=input$token.id, doc.id=input$doc.id, 
  topic.id=fit$topics, vocab=input$vocab,
  alpha=alpha, beta=beta, sort.topics="byDocs", K=K)

doc_clusters = data.frame(list(doc_id=unique(input$doc.id), 
  cluster=docProb$main.topic, title_abstract=ta))

z <- jsviz(text=ta, doc.id=input$doc.id, token.id=input$token.id, 
  topic.id=docProb$topic.id, vocab=input$vocab, K=K, k.clusters=1, lambda=0.5, 
  n.tokens=30, n.docs=10, alpha=alpha, beta=beta, sort.topics="None")

z.out <- toJSON(z)
cat(z.out, file="web_app/lda.json")

# See terms in topic 1.
head(tt(ta[docProb$main.topic == 1]), 20)

