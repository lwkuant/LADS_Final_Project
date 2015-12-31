import requests
import urllib.parse
import urllib.request
from bs4 import BeautifulSoup
import re
import os
import math
import time
from urllib.error import HTTPError

os.chdir("C:/Users/Kile/Desktop")

### test

article_list = []
article_count = 0

url = open("ptt_url_list_file_HatePolitics.txt")
url_list = url.readlines()
for i in range(len(url_list)):
    url_list[i] = url_list[i][:-1]

    
for page in url_list:
    try:
        payload = {"from":page, "yes":"yes"}
        rs = requests.session()
        res = rs.post("https://www.ptt.cc/ask/over18", verify = False, data = payload)
        res = rs.get("https://www.ptt.cc" + page, verify = False)   

        bsobj = BeautifulSoup(res.text)

        if bsobj.findAll("div", {"class":"article-metaline"})  == []:
            continue
        elif "公告" in bsobj.findAll("div", {"class":"article-metaline"})[1].find("span",{"class":"article-meta-value"}).get_text():
            continue
        else:
            article_count +=1
            # title 
            article_title = bsobj.findAll("div", {"class":"article-metaline"})[1].find("span",{"class":"article-meta-value"}).get_text()
            if "]" in article_title:
                article_title = article_title.split("]")[1][1:]
            # time
            article_time = bsobj.findAll("div", {"class":"article-metaline"})[2].find("span",{"class":"article-meta-value"}).get_text().split(" ")
            if len(article_time) == 6:
                article_time = article_time[5]+"-"+article_time[1]+"-"+article_time[3]
            else:
                article_time = article_time[4]+"-"+article_time[1]+"-"+article_time[2]              
            # content
            atricle_content = bsobj.findAll("div", {"class":"article-metaline"})[2].next_siblings
            span_word = ""
            atricle_word = ""
            for t in  atricle_content:
                if t.name == None:
                    if "From" in str(t):
                        continue
                    else:                    
                        atricle_word += str(t)
                elif t.name == "span":
                    if "發信站" in t.get_text() or "編輯" in t.get_text():
                        continue
                    else:
                        span_word += t.get_text()
            # comment
            article_comment = bsobj.findAll("div", {"class":"push"})
            
            atricle_str = ""
            atricle_str += article_title+"&&&"+article_time+"&&&"+ atricle_word+" "+span_word+"&&&"
            for i in article_comment:
                atricle_str += i.findAll("span")[0].get_text().strip()+i.findAll("span")[2].get_text().strip()+"@@@"
            atricle_str = atricle_str.replace("\n","")
            atricle_str += "\n"                   
            article_list.append(atricle_str)
            print(article_count)
            time.sleep(1)
            
    except HTTPError as e:
        continue
    except IndexError:
        continue


ptt_HatePolitics_article = open("ptt_HatePolitics_article.txt", "w",errors='ignore')
ptt_HatePolitics_article.writelines(article_list)
ptt_HatePolitics_article.close()
