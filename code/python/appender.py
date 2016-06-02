"""Append the three datasets together in which the index of each pandas dataset is the url snippet including the name for each neuron."""
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

os.chdir('..'); os.chdir('..')
os.chdir('./data_sets/raw')

minedSet = pd.read_csv('neuroData.csv', index_col=0)

lmSet = pd.read_csv('fixedLmResult.csv', index_col=0, encoding='UTF-8')

treeSet = pd.read_csv('gstats.csv', header=None, encoding='UTF-8')

# This changes the index of treeSet to be the list of neuron names so that it will play nice with minedSet and lmSet:
treeSet.index = ['neuron_info.jsp?neuron_name=' + x for x in treeSet[0]]
# This makes a slight change to treeSet's columns
treeSet.columns = ['gstats ' + str(x) for x in xrange(66)]

toMerge = [minedSet, lmSet, treeSet]

while len(toMerge) > 1:
	left = toMerge[0]
	right = toMerge[1]

	merged = left.join(right, how='inner')

	toMerge[0] = merged

	del toMerge[1]

result = toMerge[0]

os.chdir('..')

if os.path.exists('appended.csv'):
    os.remove('appended.csv')

result.to_csv('appended.csv', index=False)