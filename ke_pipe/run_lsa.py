import gensim

#logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s', 
#  level=logging.INFO)

if __name__ == '__main__':

  # load id->word mapping (the dictionary), one of the results of step 2 above
  id2word = gensim.corpora.Dictionary.load('d.dict')
  # load corpus iterator
  mm = gensim.corpora.MmCorpus('corpus_tfidf.mm')
  tfidf = gensim.models.TfidfModel(mm)
  tfidf.save("tfidf.model")
  corpus_tfidf = tfidf[mm]
  
  lsi = gensim.models.lsimodel.LsiModel(corpus=corpus_tfidf, id2word=id2word, 
    num_topics=6)

  lsi.save("lsi.model")
  
  # TODO: perform k-means clustering to get the actual topics
  topics = lsi[corpus_tfidf]
  for t in topics:
    memberships = [str(x[1]) for x in t]
    print ",".join(memberships)
