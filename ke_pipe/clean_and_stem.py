import re
import nltk
import string
from nltk.corpus import stopwords

def remove_dashes(text):
  rx = re.compile('-')
  new_text = rx.sub(" ", text)
  return new_text

def is_not_punct(char):
  # I handle dashes separately
  if char == '-': return True
  elif char in string.punctuation: return False
  else: return True

def clean_and_stem(lines):
  for line in lines:  
    line = remove_dashes(line.lower())
    line = filter(is_not_punct, line)
    tokens = nltk.word_tokenize(line)
    sw = stopwords.words("english")
    tokens = [word for word in tokens if word not in sw and len(word) > 3]
    stemmer = nltk.PorterStemmer()
    doc = [stemmer.stem(word) for word in tokens]
    yield " ".join(doc)

# test with cat doc_clusters.csv | sed '1d' | awk '{FS=","; print $3}' | python clean_and_stem.py
if __name__ == '__main__':
  import sys
  for line in clean_and_stem(sys.stdin):
    print(line)
