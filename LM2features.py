"""Take the output of LM.jar and rewrite the data in an appropriate format."""
# -*- coding: utf-8 -*-

import numpy as np
import pandas as pd
import csv
import webbrowser
import code
from bs4 import BeautifulSoup
import requests
import urllib2
import os
import pickle

fromcsv = pd.read_csv('./lms/test', delimiter='\t', names=xrange(8), header=None)

names = fromcsv.index
nameSet = set(names)

newFrame = pd.DataFrame(columns=fromcsv.iloc[:,0].values[:43])

for i, name in enumerate(nameSet):
	values = fromcsv.ix[name, 1].values
	newFrame.loc[i] = values

newFrame.index = list(nameSet)

newFrame.to_csv('fixedTest.csv')
