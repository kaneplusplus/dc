import re
import nltk
import string
from nltk.corpus import stopwords

# Add query words as stopwords.
def clean_and_stem(lines, sw=stopwords.words("english")):
  for line in lines:  
    line = line.lower()
    for punct in string.punctuation:
      line = line.replace(punct, " ") #remove_dashes(line.lower())
    tokens = line.split() #nltk.word_tokenize(line)
    sw = stopwords.words("english")
    tokens = [word for word in tokens if word not in sw and len(word) > 3]
    stemmer = nltk.PorterStemmer()
    doc = [stemmer.stem(word) for word in tokens]
    yield " ".join(doc)

# test with cat query_return.csv | sed '1d' | awk -F, '{print $9}' | python clean_and_stem.py
if __name__ == '__main__':
  import sys
  for line in clean_and_stem(sys.stdin):
    print(line)
