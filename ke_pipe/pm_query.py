import requests
import sys
import optparse
import xml.etree.ElementTree as ET
#import logging
import string
#logging.basicConfig(format='%(asctime)s %(levelname)-7s %(message)s [%(pathname)s]', level=logging.INFO)

reload(sys)
sys.setdefaultencoding('utf-8')

usage = "usage:python %prog -q [query] -o [output] -s [flag for steming]"
parser = optparse.OptionParser(usage=usage)
parser.add_option("-q", "--query", action="store", dest="query", help="Enter the PubMed query (PubMed style queries supported)", metavar="QUERY")
parser.add_option("-o", "--output", action="store", dest="outfile", help="Enter the output file name to store result", metavar="OFILE")
(options, args) = parser.parse_args()

if len(options.query) < 0:
    parser.print_help()

def chunker(seq, size):
  return (seq[pos:pos + size] for pos in xrange(0, len(seq), size))

def pm_query_gen(query):
  esearch = 'http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&retmode=xml&retmax=10000000&term={}'.format(query)
  response = requests.get(esearch)
  root = ET.fromstring(response.text)
  ids = [x.text for x in root.findall("IdList/Id")]
  #logging.info('Got {} articles'.format(len(ids)))

  for group in chunker(ids, 500):
    efetch = "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?&db=pubmed&retmode=xml&id={}".format((','.join(group)))
    response = requests.get(efetch)
    root = ET.fromstring(response.text)
    for article in root.findall("PubmedArticle"):
      abstract_text = ''
      pmid = article.find("MedlineCitation/PMID").text
      title = article.findtext("MedlineCitation/Article/ArticleTitle")
      abstract = article.findall(
        "MedlineCitation/Article/Abstract/AbstractText")
      if len(abstract) > 0:
        abstract_text = ""
        for abst_part in abstract:
          abstract_text += ' ' + str(abst_part.text)
        #title = title.encode('utf-8', 'replace')
        title = title.encode('ascii', 'replace')
        #abstract_text = abstract_text.encode('utf-8', 'replace')
        abstract_text = abstract_text.encode('ascii', 'replace')
      else:
        yield None
      title = title.replace(',', " ")
      title = title.replace('"', "")
      abstract_text = abstract_text.replace(',', " ")
      abstract_text = abstract_text.replace('"', "")
      title = '"' + title + '"'
      abstract_text = '"' + abstract_text + '"'
      lasts = [x.text for x in 
        article.findall(".//AuthorList//Author//LastName")]
      lasts = ";".join(lasts)
      firsts = [x.text for x in 
        article.findall(".//AuthorList//Author//ForeName")]
      firsts = ";".join(firsts)
      lang = article.findtext(".//Language")
      pub_type=article.findtext(".//PublicationType")
      year = article.findtext('.//ArticleDate[@DateType="Electronic"]//Year')
      year = year if year != None else ""
      month = article.findtext('.//ArticleDate[@DateType="Electronic"]//Month')
      month = month if month != None else ""
      day = article.findtext('.//ArticleDate[@DateType="Electronic"]//Day')
      day = day if day != None else ""
      date = "/".join([month, day, year])
      kw = [x.text for x in article.findall(".//Keyword")]
      kw = ";".join(kw)
      journal = article.findtext('.//MedlineJournalInfo')
      title_abstract = '"' + (title+abstract_text).replace('"', "") + '"'
      yield ",".join([title, abstract_text, date, lasts, firsts, lang, 
        pub_type, kw, title_abstract])

if __name__ == "__main__":
  query = options.query
  print('"title","abstract","date","lasts","firsts","lang","type","kw","title_abstract"')
  for article in pm_query_gen(query):
    if article != None:
      print(article)
