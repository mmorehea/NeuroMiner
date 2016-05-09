"""Make various subsets out of appended.csv, the output of appender.py"""
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

ogSet = pd.read_csv('appended.csv')

totalRowNum = len(ogSet.index)

s1 = False; s2 = False; s3 = False; s4 = False; s5 = False; s6 = False; s7 = False; s8 = False
runWhich = 'runWhich'
while runWhich not in '123456789': runWhich = raw_input("""\nWhich subset do you want to make? 

1. Set with only the cells where ages are reported.

2. Set with only the cells aged 1-18 days, mouse only (requires running option 1 first).

3. Set with only the cells aged above 18 days, mouse only (requires running option 1 first).

4. Set with only the pyramidal cells.

5. Set with only stuff done by Jacobs.

6. Set with only the ganglion cells.

7. Set with only the granule cells.

8. Set with only the cells from drosophila.

9. Make them all.

""")
if runWhich == '1': s1 = True
elif runWhich == '2': s2 = True
elif runWhich == '3': s3 = True
elif runWhich == '4': s4 = True
elif runWhich == '5': s5 = True
elif runWhich == '6': s6 = True
elif runWhich == '7': s7 = True
elif runWhich == '8': s8 = True
elif runWhich == '9': s1 = True; s2 = True; s3 = True; s4 = True; s5 = True; s6 = True; s7 =True; s8 = True


# 1. Set with only the cells where ages are reported.
if s1: 
	all_ages_reported_set = ogSet
	for rowNum in xrange(len(ogSet.index)):
		print 'Checking row ' + str(rowNum + 1) + '/' + str(totalRowNum)

		if ogSet.ix[rowNum, 'Min Age'] == 'Not reported' or ogSet.ix[rowNum, 'Min Age'] == 0 or ogSet.ix[rowNum, 'Min Age'] == '0' \
		or ogSet.ix[rowNum, 'Max Age'] == 'Not reported' or ogSet.ix[rowNum, 'Max Age'] == 0 or ogSet.ix[rowNum, 'Max Age'] == '0' \
		or ogSet.ix[rowNum, 'Min Age'] == 0.0 or ogSet.ix[rowNum, 'Min Age'] == '0.0' \
		or ogSet.ix[rowNum, 'Max Age'] == 0.0 or ogSet.ix[rowNum, 'Max Age'] == '0.0':

			all_ages_reported_set = all_ages_reported_set.drop(rowNum)
			print '\nDropped row ' + str(rowNum + 1) + '\n'

	all_ages_reported_set.reset_index(drop=True, inplace=True)
	if os.path.exists('all_ages_reported.csv'):
	    os.remove('all_ages_reported.csv')

	all_ages_reported_set.to_csv('all_ages_reported.csv', index=False)


# 2. Set with only the cells aged 1-18 days, mouse only (requires running option 1 first).
if s2:
	only_1to18days_mouse_set = pd.read_csv('all_ages_reported.csv')
	totalRowNum = len(only_1to18days_mouse_set.index)

	for rowNum in xrange(len(only_1to18days_mouse_set.index)):
		print 'Checking row ' + str(rowNum + 1) + '/' + str(totalRowNum)

		if int(only_1to18days_mouse_set.ix[rowNum, 'Min Age']) > 18 or int(only_1to18days_mouse_set.ix[rowNum, 'Max Age']) > 18 or only_1to18days_mouse_set.ix[rowNum, 'Species Name'].lower() != 'mouse':
			only_1to18days_mouse_set = only_1to18days_mouse_set.drop(rowNum)
			print '\nDropped row ' + str(rowNum + 1) + '\n' 

	only_1to18days_mouse_set.reset_index(drop=True, inplace=True)
	if os.path.exists('only_1to18days_mouse.csv'):
	    os.remove('only_1to18days_mouse.csv')

	only_1to18days_mouse_set.to_csv('only_1to18days_mouse.csv', index=False) 


# 3. Set with only the cells aged above 18 days, mouse only (requires running option 1 first).
if s3:
	only_above18days_mouse_set = pd.read_csv('all_ages_reported.csv')
	totalRowNum = len(only_above18days_mouse_set.index)

	for rowNum in xrange(len(only_above18days_mouse_set.index)):
		print 'Checking row ' + str(rowNum + 1) + '/' + str(totalRowNum)

		if int(only_above18days_mouse_set.ix[rowNum, 'Min Age']) < 18 or int(only_above18days_mouse_set.ix[rowNum, 'Max Age']) < 18 or only_above18days_mouse_set.ix[rowNum, 'Species Name'].lower() != 'mouse':
			only_above18days_mouse_set = only_above18days_mouse_set.drop(rowNum)
			print '\nDropped row ' + str(rowNum + 1) + '\n' 

	only_above18days_mouse_set.reset_index(drop=True, inplace=True)
	if os.path.exists('only_above18days_mouse.csv'):
	    os.remove('only_above18days_mouse.csv')

	only_above18days_mouse_set.to_csv('only_above18days_mouse.csv', index=False)


# 4. Set with only the pyramidal cells.
if s4:

	choice = 'choice'
	while choice not in '1234': choice = raw_input("""\nMake the pyramidal cell set a subset of which dataset? 
	
1. The original set.
	
2. The set with only the cells where ages are reported (requires running option 1 in main menu first).

3. The set with only the cells aged 1-18 days, mouse only (requires running options 1 and 2 in main menu first).

4. The set with only the cells aged above 18 days, mouse only (requires running options 1 and 3 in main menu first).
	
""")

	if choice == '1': only_pyramidal_set = ogSet
	elif choice == '2': only_pyramidal_set = pd.read_csv('all_ages_reported.csv')
	elif choice == '3': only_pyramidal_set = pd.read_csv('only_1to18days_mouse.csv')
	elif choice == '4': only_pyramidal_set = pd.read_csv('only_above18days_mouse.csv')

	totalRowNum = len(only_pyramidal_set.index)

	for rowNum in xrange(len(only_pyramidal_set.index)):
		print 'Checking row ' + str(rowNum + 1) + '/' + str(totalRowNum)

		if 'pyramidal' not in only_pyramidal_set.ix[rowNum, 'Secondary Cell Class'].lower():
			only_pyramidal_set = only_pyramidal_set.drop(rowNum)
			print '\nDropped row ' + str(rowNum + 1) + '\n' 

	only_pyramidal_set.reset_index(drop=True, inplace=True)
	if os.path.exists('only_pyramidal.csv'):
	    os.remove('only_pyramidal.csv')

	only_pyramidal_set.to_csv('only_pyramidal.csv', index=False)


# 5. Set with only stuff done by Jacobs.
if s5:

	choice = 'choice'
	while choice not in '1234': choice = raw_input("""\nMake the Jacobs set a subset of which dataset? 
	
1. The original set.
	
2. The set with only the cells where ages are reported (requires running option 1 in main menu first).

3. The set with only the cells aged 1-18 days, mouse only (requires running options 1 and 2 in main menu first).

4. The set with only the cells aged above 18 days, mouse only (requires running options 1 and 3 in main menu first).
	
""")

	if choice == '1': only_jacobs_set = ogSet
	elif choice == '2': only_jacobs_set = pd.read_csv('all_ages_reported.csv')
	elif choice == '3': only_jacobs_set = pd.read_csv('only_1to18days_mouse.csv')
	elif choice == '4': only_jacobs_set = pd.read_csv('only_above18days_mouse.csv')

	totalRowNum = len(only_jacobs_set.index)

	for rowNum in xrange(len(only_jacobs_set.index)):
		print 'Checking row ' + str(rowNum + 1) + '/' + str(totalRowNum)

		if 'jacobs' not in only_jacobs_set.ix[rowNum, 'Archive Name'].lower():
			only_jacobs_set = only_jacobs_set.drop(rowNum)
			print '\nDropped row ' + str(rowNum + 1) + '\n' 

	only_jacobs_set.reset_index(drop=True, inplace=True)
	if os.path.exists('only_jacobs.csv'):
	    os.remove('only_jacobs.csv')

	only_jacobs_set.to_csv('only_jacobs.csv', index=False)


# 6. Set with only the ganglion cells.
if s6:

	choice = 'choice'
	while choice not in '1234': choice = raw_input("""\nMake the ganglion cell set a subset of which dataset? 
	
1. The original set.
	
2. The set with only the cells where ages are reported (requires running option 1 in main menu first).

3. The set with only the cells aged 1-18 days, mouse only (requires running options 1 and 2 in main menu first).

4. The set with only the cells aged above 18 days, mouse only (requires running options 1 and 3 in main menu first).
	
""")

	if choice == '1': only_ganglion_set = ogSet
	elif choice == '2': only_ganglion_set = pd.read_csv('all_ages_reported.csv')
	elif choice == '3': only_ganglion_set = pd.read_csv('only_1to18days_mouse.csv')
	elif choice == '4': only_ganglion_set = pd.read_csv('only_above18days_mouse.csv')

	totalRowNum = len(only_ganglion_set.index)

	for rowNum in xrange(len(only_ganglion_set.index)):
		print 'Checking row ' + str(rowNum + 1) + '/' + str(totalRowNum)

		if 'ganglion' not in only_ganglion_set.ix[rowNum, 'Secondary Cell Class'].lower():
			only_ganglion_set = only_ganglion_set.drop(rowNum)
			print '\nDropped row ' + str(rowNum + 1) + '\n' 

	only_ganglion_set.reset_index(drop=True, inplace=True)
	if os.path.exists('only_ganglion.csv'):
	    os.remove('only_ganglion.csv')

	only_ganglion_set.to_csv('only_ganglion.csv', index=False)


# 7. Set with only the granule cells.
if s7:

	choice = 'choice'
	while choice not in '1234': choice = raw_input("""\nMake the granule cell set a subset of which dataset? 
	
1. The original set.
	
2. The set with only the cells where ages are reported (requires running option 1 in main menu first).

3. The set with only the cells aged 1-18 days, mouse only (requires running options 1 and 2 in main menu first).

4. The set with only the cells aged above 18 days, mouse only (requires running options 1 and 3 in main menu first).
	
""")

	if choice == '1': only_granule_set = ogSet
	elif choice == '2': only_granule_set = pd.read_csv('all_ages_reported.csv')
	elif choice == '3': only_granule_set = pd.read_csv('only_1to18days_mouse.csv')
	elif choice == '4': only_granule_set = pd.read_csv('only_above18days_mouse.csv')

	totalRowNum = len(only_granule_set.index)

	for rowNum in xrange(len(only_granule_set.index)):
		print 'Checking row ' + str(rowNum + 1) + '/' + str(totalRowNum)

		if 'granule' not in only_granule_set.ix[rowNum, 'Secondary Cell Class'].lower():
			only_granule_set = only_granule_set.drop(rowNum)
			print '\nDropped row ' + str(rowNum + 1) + '\n' 

	only_granule_set.reset_index(drop=True, inplace=True)
	if os.path.exists('only_granule.csv'):
	    os.remove('only_granule.csv')

	only_granule_set.to_csv('only_granule.csv', index=False)


# 8. Set with only the cells from drosophila.
if s8:

	choice = 'choice'
	while choice not in '1234': choice = raw_input("""\nMake the drosophila cell set a subset of which dataset? 
	
1. The original set.
	
2. The set with only the cells where ages are reported (requires running option 1 in main menu first).

3. The set with only the cells aged 1-18 days, mouse only (requires running options 1 and 2 in main menu first).

4. The set with only the cells aged above 18 days, mouse only (requires running options 1 and 3 in main menu first).
	
""")

	if choice == '1': only_drosophila_set = ogSet
	elif choice == '2': only_drosophila_set = pd.read_csv('all_ages_reported.csv')
	elif choice == '3': only_drosophila_set = pd.read_csv('only_1to18days_mouse.csv')
	elif choice == '4': only_drosophila_set = pd.read_csv('only_above18days_mouse.csv')

	totalRowNum = len(only_drosophila_set.index)

	for rowNum in xrange(len(only_drosophila_set.index)):
		print 'Checking row ' + str(rowNum + 1) + '/' + str(totalRowNum)

		if 'drosophila' not in only_drosophila_set.ix[rowNum, 'Species Name'].lower():
			only_drosophila_set = only_drosophila_set.drop(rowNum)
			print '\nDropped row ' + str(rowNum + 1) + '\n' 

	only_drosophila_set.reset_index(drop=True, inplace=True)
	if os.path.exists('only_drosophila.csv'):
	    os.remove('only_drosophila.csv')

	only_drosophila_set.to_csv('only_drosophila.csv', index=False)
