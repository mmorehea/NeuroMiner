from bs4 import BeautifulSoup
from openpyxl import load_workbook
import code
import requests
import re
import sys
from selenium import webdriver
import time
import numpy as np
import cPickle as pickle
import os
import csv
import string
import webbrowser

#This script grabs all new neurons from "What's New?" (version 7.2)
#section from NeuroMorpho.org. Can be edited to look at earlier/later versions
#First get the links to archives for the recently uploaded neurons
search_url = 'http://neuromorpho.org/WIN.jsp'
response = requests.get(search_url)
soup = BeautifulSoup(response.text, 'lxml')
link_1 = 'NeuroMorpho_ArchiveLinkout.jsp?'
pea_soup = soup.findAll('a', href=re.compile(link_1))
links22 = []
while len(pea_soup)>0:
	pea_soup_string = str(pea_soup.pop())
	link_ending = re.search('a href="(.*?)"', pea_soup_string)
	links22.append(link_ending.group(1))

links_good = links22[350:432]
#add page 2/page 3 links manually
links_good.append("NeuroMorpho_ArchiveLinkout.jsp?pageNumber=2&pageNumber=1&ARCHIVE=Denk&DATE=2017-07-19")
links_good.append("NeuroMorpho_ArchiveLinkout.jsp?pageNumber=3&pageNumber=2&pageNumber=1&ARCHIVE=Denk&DATE=2017-07-19")
links_good.append("NeuroMorpho_ArchiveLinkout.jsp?pageNumber=2&ARCHIVE=Kougias_Juraska&DATE=2017-07-19")
links_good.append("NeuroMorpho_ArchiveLinkout.jsp?pageNumber=2&ARCHIVE=Wanner_Friedrich&DATE=2017-07-19")
links_good.append("NeuroMorpho_ArchiveLinkout.jsp?pageNumber=3&pageNumber=2&ARCHIVE=Wanner_Friedrich&DATE=2017-07-19")

#now go through links and extract neuron names
nm = []
for j in range(0,len(links_good)):
    nms = []
    links_good1 = links_good[j].replace("amp;","")
    search_url_1 = 'http://neuromorpho.org/' + links_good1
    response1 = requests.get(search_url_1)
    soup1 = BeautifulSoup(response1.text, 'lxml')
    pea_soup1 = soup1.findAll("input")
    while len(pea_soup1)>0:
        pea_soup1_string = str(pea_soup1.pop())
        link_ending1 = re.search('type="(.*?)"', pea_soup1_string)
        nms.append(link_ending1.group(1))
    nms = np.array(nms)    
    inds = len(nms) - (np.where(nms == "checkbox")[0]+1)
    pea_soup1 = soup1.findAll("input")
    pea_soup2 = list(pea_soup1[i] for i in inds)
    while len(pea_soup2)>0:
        pea_soup2_string = str(pea_soup2.pop())
        link_ending2 = re.search('value="(.*?)"', pea_soup2_string)
        nm.append(link_ending2.group(1))

pickle.dump(nm, open('nms.p','wb'))
