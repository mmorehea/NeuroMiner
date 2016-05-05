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

toMerge = []

minedSet = pd.read_csv('neuroData.csv', index_col=0)

lmSet = pd.read_csv('fixedLmResult.csv', index_col=0, encoding='UTF-8')

#treeSet = pd.read_csv('gstats.csv', header=None, encoding='UTF-16')

toMerge = [minedSet, lmSet]

while len(toMerge) > 1:
	left = toMerge[0]
	right = toMerge[1]

	merged = left.join(right, how='outer')
	# Note: this will join the datasets using the union of the two keys of each dataframe.
	# The keys (aka indices) are the neuron name url snippets. Therefore, to make sure that there are no differences between the sets of cells, scroll quickly through the result.
	# If you see blocks of "NaN" or empty values, this means that there are some cells in one dataset that are not in another.

	toMerge[0] = merged

	del toMerge[1]

result = toMerge[0]

if os.path.exists('appended.csv'):
    os.remove('appended.csv')

result.to_csv('appended.csv')