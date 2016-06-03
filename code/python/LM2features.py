"""Take the output of LM.jar and rewrite the data in an appropriate format. Delete any entries which failed. Drop any duplicate rows at the end."""
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

f = raw_input('\n(Run Lm.jar first). Please enter the name of the output file from the LM tool you just ran: ')
fromcsv = pd.read_csv('./lms/' + f, delimiter='\t', names=xrange(8), header=None)

names = fromcsv.index
nameSet = set(names)
nameList = list(nameSet)
original_number_of_rows = len(nameList)

newFrame = pd.DataFrame(columns=fromcsv.iloc[:,0].values[:43])

lostRowCount = 0
for i, name in enumerate(nameList):
	try:
		values = fromcsv.ix[name, 1].values
	except:
		del nameList[nameList.index(name)]
		lostRowCount += 1

	newFrame.loc[i] = values

newFrame.index = ['neuron_info.jsp?neuron_name=' + x[x.index(x.split('/')[-1]):-4] for x in nameList]

tryAgain = True
while tryAgain:
	choice = raw_input('\nWhich would you like to do:\n\n1. Append to existing \'fixedLmResult.csv\' file \n2. Create a new \'fixedLmResult.csv\' file and overwrite if it already exists (Choose this the first time)\n')
	if choice == '1':
		with open('fixedLmResult.csv', 'a') as fi:
			newFrame.to_csv(fi, header=False)
		tryAgain = False

	elif choice == '2':
		if os.path.exists('fixedLmResult.csv'):
	 		os.remove('fixedLmResult.csv')
		newFrame.to_csv('fixedLmResult.csv')
		tryAgain=False

print '\nNumber of rows entered: ' + str(original_number_of_rows)
print '\nNumber of bad rows: ' + str(lostRowCount)
print '\nNumber of outputted rows: ' + str(original_number_of_rows - lostRowCount)
print '\nLast file that went through: ' + str(names[-1]) 
print '^If there are more files to run through the LM tool, start from the one after this one.'

#code.interact(local=locals())
# tryAgain = True
# while tryAgain:
# 	choice = raw_input('\nAre you done? (any duplicate rows will be deleted) (y/n)')
# 	if choice.lower() == 'y':
# 		data = pd.read_csv('fixedLmResult.csv', index_col=0)
# 		data = data.drop_duplicates()
# 		if os.path.exists('fixedLmResult.csv'):
# 			os.remove('fixedLmResult.csv')
# 		data.to_csv('fixedLmResult.csv')
# 		tryAgain = False

# 	elif choice.lower() == 'n':
# 		tryAgain=False