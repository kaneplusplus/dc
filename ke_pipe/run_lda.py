import logging, gensim

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
# mm = gensim.corpora.MmCorpus(bz2.BZ2File('wiki_en_tfidf.mm.bz2')) # use this if you compressed the TFIDF output

lda = gensim.models.ldamodel.LdaModel(corpus=corpus_tfidf, id2word=id2word, 
  num_topics=6, update_every=0, passes=20)

lda.save("lda.model")
topics = lda[corpus_tfidf]
for t in topics:
  memberships = [str(x[1]) for x in t]
  print ",".join(memberships)
