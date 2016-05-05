"""Glob all the swc files and break them up into separate directories of 1500 each."""
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
import glob
import shutil

path = './swcs/*.swc'
swcs = glob.iglob(path)

directory = []
totalAdded = 0

while True:
	try:
		directory.append(swcs.next())
	except:

		name = str(totalAdded - ((totalAdded % 1500) - 1)) + ' - ' + str(totalAdded) 
		os.mkdir('./swcs/' + name)
		for f in directory:
			shutil.move(f, './swcs/' + name)
		directory = []

		break

	totalAdded += 1

	print 'Processed '  + str(totalAdded) + ' files.'

	if totalAdded % 1500 == 0:
		name = str(totalAdded - 1499) + ' - ' + str(totalAdded) 
		os.mkdir('./swcs/' + name)
		for f in directory:
			shutil.move(f, './swcs/' + name)
		directory = []
