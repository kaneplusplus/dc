import gensim



#if __name__ == '__main__':

# load id->word mapping (the dictionary), one of the results of step 2 above
id2word = gensim.corpora.Dictionary.load('d.dict')
# load corpus iterator
mm = gensim.corpora.MmCorpus('corpus.mm')
tfidf = gensim.models.TfidfModel(mm)
#tfidf.save("tfidf.model")
corpus_tfidf = tfidf[mm]

lda = gensim.models.ldamodel.LdaModel(corpus=corpus_tfidf, id2word=id2word, 
  num_topics=5, update_every=1, chunksize=10000., passes=10, decay=0.)

#lda = gensim.models.ldamodel.LdaModel.load("lda.model")
lda.save("lda.model")
#topics = lda[corpus_tfidf]
topics = lda[corpus_tfidf]
for t in topics:
  memberships = [str(x[1]) for x in t]
  m=[float(x) for x in memberships]
  print(m.index(max(m)))
  #print ",".join(memberships)
