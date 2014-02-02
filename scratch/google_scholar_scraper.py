
from __future__ import print_function
from selenium import webdriver
from pybtex.database.input import bibtex
from StringIO import StringIO
from time import sleep
from numpy.random import exponential
import lxml.html as LH
import lxml.html.clean as clean
import string
import nltk

def wait(wait_time=1.5):
  sleep(min([exponential(wait_time,1)[0], 12.756]))

def string_similarity(s1, s2):
  remove_punctuation_map=dict((ord(char), None) for char in string.punctuation)
  ss1 = s1.split()
  u1 = [s.translate(remove_punctuation_map).lower() for s in ss1]
  u1 = filter(lambda x: len(x) > 0, u1)
  u1 = list(set(u1).difference(set(nltk.corpus.stopwords.words("english"))))
  ss2 = s2.split()
  u2 = [s.translate(remove_punctuation_map).lower() for s in ss2]
  u2 = filter(lambda x: len(x) > 0, u2)
  u2 = list(set(u2).difference(set(nltk.corpus.stopwords.words("english"))))
  if len(u1) == 0:
    ret = 0.0
  else:
    ret = float(len(set(u1).intersection(set(u2))))/float(len(set(u1)))
  return(ret)
  
def bibtex_links_from_elements(elements):
  bibtex_refs = []
  for element in elements:
    lower_links = element.find_elements_by_xpath( './/div[@class="gs_fl"]//a')
    for ll in lower_links:
      ref = ""
      if ll.text == u'Import into BibTeX':
        bibtex_refs.append(ll.get_attribute("href"))
  return(bibtex_refs)

def article_links_and_abstract_fragment(elements, bibtex_entries):
  ret = []
  for i in range(len(elements)):
    element = elements[i]
    be = bibtex_entries[i]
    link_elements = element.find_elements_by_xpath('.//a[contains(@class, "yC")]')
    article_link = ""
    if len(link_elements) > 0:
      article_link = link_elements[1 if len(link_elements) == 2 else 0].get_attribute("href")
    abs_frag = element.text.split("\n")
    author_lasts = be[5].split(";")
    abs_row_index = 4
    if len(author_lasts) > 0:
      try:
        author_last = author_lasts[0]
        abs_row_index = [author_last in af for af in abs_frag].index(True)
      except ValueError:
        abs_row_index = 0
    abs_word_string = ""
    if (len(abs_frag) > 4 and abs_row_index > 0):
      abs_word_string = " ".join(abs_frag[(abs_row_index+1):(len(abs_frag)-1)])
    ret.append([article_link, abs_word_string])
  return(ret)

def abstract_from_alaf(alaf):
  if alaf[0].endswith("pdf") or alaf[0].startswith("http://books.google.com"):
    return(alaf[1])
  wait()
  browser.get(alaf[0])
  check_bot_detected(browser)
  content=browser.page_source
  cleaner=clean.Cleaner()
  content=cleaner.clean_html(content)
  doc=LH.fromstring(content)
  ignore_tags=('script','noscript','style')
  doc_text = []
  for elt in doc.iterdescendants():
    if elt.tag in ignore_tags: continue
    doc_text.append(unicode(elt.text or ''))
  remove_punctuation_map=dict((ord(char), None) for char in string.punctuation)
  doc_text = [s.translate(remove_punctuation_map).lower() for s in doc_text]
  # The entry with the highest string similarity corresponds to the abstract.
  ss = [string_similarity(alaf[1], x) for x in doc_text]
  abstract = alaf[1]
  if (len(ss) > 0):
    if max(ss) > 0.75:
      abstract = doc_text[ss.index(max(ss))]
  return(abstract)

# Take the BibTeX and return the type, publisher, year, title, first names
# middle names, and last names.
def bibtex_fields(ref_text):
  parser = bibtex.Parser()
  bib = parser.parse_stream(StringIO(ref_text))
  be = bib.entries[bib.entries.keys()[0]]
  ret = [be.type]
  ret.extend(
    [be.fields[field] if field in be.fields else "" 
    for field in ["publisher", "year", "title"]])
  ps = be.persons['author']
  firsts = [ p.first()[0] if len(p.first()) > 0 else "" for p in ps ]
  firsts = [";".join(firsts)]
  lasts = [ p.last()[0] if len(p.last()) > 0 else "" for p in ps ]
  lasts = [";".join(lasts)]
  middles = [ p.middle()[0] if len(p.middle()) > 0 else "" for p in ps ]
  middles = [";".join(middles)]
  ret.extend(firsts)
  ret.extend(lasts)
  ret.extend(middles)
  return(ret)

def browser_next(browser):
  elements = browser.find_elements_by_xpath('//span[@class="gs_ico gs_ico_nav_next"]')
  ret = False
  if (len(elements) > 0):
    wait()
    elements[0].click()
    ret = True
  return(ret)

wait_time = 1
browser = webdriver.Firefox()
wait()
#browser.delete_all_cookies()

start_page=None

def check_bot_detected(browser):
  detected_string = u"Our systems have detected unusual traffic from you"
  while detected_string in browser.page_source:
    sleep(5)

if start_page is not None:
  browser.get(start_page)
  check_bot_detected(browser)
else:
  # Go to google scholar
  browser.get("http://scholar.google.com")
  check_bot_detected(browser) and wait()

  include_patents = False

  # Select settings
  element = browser.find_elements_by_xpath('//a[@class="gs_btnP gs_in_ib"]')[0]
  wait()
  element.click()
  check_bot_detected(browser)

  if not include_patents:
    element = browser.find_elements_by_xpath('//input[@id="as_sdtp"]')[0]
    if not element.is_selected():
      wait()
      element.click()
      check_bot_detected(browser)

  # 10 results per page. This will make it easier to restart when we get 
  # blocked.
  #element = browser.find_elements_by_xpath('//button[@id="gs_num-bd"]')[0]
  #check_bot_detected(browser) and wait()
  #element = browser.find_elements_by_xpath('//li[@data-v="10"]')[0]
  #if check_bot_detected(browser): 
  #  element = browser.find_elements_by_xpath('//li[@data-v="10"]')[0]
  #wait()
  #element.click()

  # Return BibTex citations.
  element = browser.find_elements_by_xpath('//input[@id="scis1"]')[0]
  element.click()
  element = browser.find_elements_by_xpath('//button[@id="gs_scisf-bd"]')[0]
  element.click()

  element = browser.find_elements_by_xpath('//li[@id="gs_scisf-0"]')[0]
  element.click()
  element = browser.find_elements_by_xpath('//button[@class=" gs_btn_act"]')[0]
  wait()
  element.click()

  query_string = "Rift Valley Fever"

  # Fill in the query string.
  element = browser.find_elements_by_xpath('//input[@id="gs_hp_tsi"]')[0]
  element.send_keys(query_string)

  # Click on search.
  element = browser.find_elements_by_xpath('//button[@id="gs_hp_tsb"]')[0]
  wait()
  element.click()

done = False
print("type,publisher,year,title,firsts,middles,lasts,abstract")
while (not done):

  current_url = browser.current_url

  # Grab the individual results.
  elements = browser.find_elements_by_xpath( '//div[@class="gs_r"]')

  if (len(elements) == 0):
    done=True
    break

  bibtex_links = bibtex_links_from_elements(elements)
  bibtex_entries = []
  for bibtex_link in bibtex_links:
    wait()
    browser.get(bibtex_link)
    check_bot_detected(browser)
    entry_text = browser.find_elements_by_xpath("//pre")[0].text
    bibtex_entries.append(bibtex_fields(entry_text))

  # Go back 
  wait()
  browser.get(current_url)
  check_bot_detected(browser)
  elements = browser.find_elements_by_xpath( '//div[@class="gs_r"]')
  alafs = article_links_and_abstract_fragment(elements, bibtex_entries)

  abstracts = [abstract_from_alaf(alaf) for alaf in alafs]
  doc_data = [ bibtex_entries[i] + [abstracts[i]] for i in range(10) ]

  # Get rid of all commas and print
  doc_data_no_comma = []
  for s in doc_data:
    doc_data_no_comma.append([x.replace(u',', u'') for x in s])
  [print(u",".join(d)) for d in doc_data_no_comma]

  wait()
  browser.get(current_url)
  check_bot_detected(browser)
  done = not browser_next(browser)

browser.close()
