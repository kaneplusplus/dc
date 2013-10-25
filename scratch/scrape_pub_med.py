from selenium import webdriver

driver = webdriver.Firefox()

query_string = "rift+valley+fever"
driver.get("http://www.ncbi.nlm.nih.gov/pubmed/?term="+query_string)

