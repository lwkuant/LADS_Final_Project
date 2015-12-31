import requests
from bs4 import BeautifulSoup
import re
import time
import os

os.chdir("C:/Users/Kile/Desktop")

url_list = []
count_page = 0
count_article = []

for page in range(264, 291):
    payload = {"from":"/bbs/Gossiping/index"+str(page)+".html", "yes":"yes"}
    rs = requests.session()
    res = rs.post("https://www.ptt.cc/ask/over18", verify = False, data = payload)
    res = rs.get("https://www.ptt.cc/bbs/Gossiping/index"+str(page)+".html", verify = False)
    
    
    bsobj = BeautifulSoup(res.text)
    article_content = bsobj.findAll("a", {"href":re.compile("^(/bbs/Gossiping/M)")})
    url_list.extend(article_content)
    count_page += 1     
    count_article.append(len(article_content))
    # pause for second to get all the data, avoid getting too fast to obtain all the data
    time.sleep(0.1)
    
for i in url_list:
    print(i.attrs["href"])
print(len(url_list))    
print(count_page)
print(count_article)   

url_store = []

for i in url_list:
    url_store.append(i.attrs["href"]+"\n" )
    
ptt_url_list_file = open("ptt_url_list_file.txt", "w")
ptt_url_list_file.writelines(url_store)
ptt_url_list_file.close()
