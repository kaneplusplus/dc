import gensim, nltk

def token_gen(lines):
  for line in lines:
    yield nltk.word_tokenize(line)

def doc2bow_gen(dictionary, lines):
  for line in lines:
    yield dictionary.doc2bow(line)

if __name__ == '__main__':
  # Create the dictionary:
  docs = token_gen(open("cas.txt"))
  dictionary = gensim.corpora.Dictionary(docs)

  # Serialize the dictionary
  dictionary.save("d.dict")

  # Write out the corpus
  lines= token_gen(open("cas.txt"))
  gensim.corpora.MmCorpus.serialize("corpus.mm", doc2bow_gen(dictionary, lines))

  # load it like this:
  #corpus = gensim.corpora.MmCorpus("corpus.mm")
