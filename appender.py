"""Append the three datasets together in which the first column of each dataset is the neuron name."""
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


minedSet = pd.read_csv('neuroData.csv', index_col=0)
#note to self - make the index of neurodata the same as that of fixedLM

lmSet = pd.read_csv('fixedLM.csv', index_col=0, encoding='UTF-8')

#treeSet = pd.read_csv('gstats.csv', header=None, encoding='UTF-16')

code.interact(local=locals())
