"""Make various subsets out of appended.csv, the output of appender.py. Scroll down to main method for options."""
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


# Scroll down to main method for options.


def makeSubset(csv_path, new_subset_path, if_string):

	data_set = pd.read_csv(csv_path)
	totalRowNum = len(data_set.index)

	if_string += '\n\tdata_set = data_set.drop(rowNum)\nprint \'\\nDropped row \' + str(rowNum + 1) + \'\\n\''

	for rowNum in xrange(len(data_set.index)):
		print 'Checking row ' + str(rowNum + 1) + '/' + str(totalRowNum)
		exec(if_string)

	data_set.reset_index(drop=True, inplace=True)
	if os.path.exists(new_subset_path):
	    os.remove(new_subset_path)

	data_set.to_csv(new_subset_path, index=False) 



def main():
	# Uncomment Premade Subsets below as desired
	# Parameters for makeSubset:
	#	1. csv_path: the path to the data set from which the subset should be made
	#	2. new_subset_path: the path where you want to make the new subset
	#		Naming convention for new subsets (except for first 3): './subsets/uniqueName_' + filename of the set from which it's made 
	#	3. if_string: the conditions under which particular cells will be dropped. Denote the original data set as 'data_set'.


	# Generic template:
	# makeSubset(csv_path, new_subset_path, if_string)


	# ---------------Premade Subsets---------------
	# (if changing anything, better to copy/paste and leave these defaults alone)

	# Contents:

	# 1. Set with only the cells where ages are reported.
	# 2. Set with only the cells aged 1-18 days (requires premade subset #1).
	# 3. Set with only the cells aged above 18 days (requires premade subset #1).

	# 4. Set with only the cells from the Jacobs archive.
	# 5. Set with only the pyramidal cells.
	# 6. Set with only the ganglion cells.
	# 7. Set with only the granule cells.
	# 8. Set with only the drosophila cells.

	# 9. Set with only the mouse cells aged 1-18 days (requires premade subset #2). 
	# 10. Set with only the mouse cells aged above 18 days (requires premade subset #3).
	# 11. Set with only the rat cells aged 1-18 days (requires premade subset #2). 
	# 12. Set with only the rat cells aged above 18 days (requires premade subset #3).

	# 13. Set with only the pyramidal cells above 18 days, mouse only, excluding the archives 
	#     ['Brown', 'Buzsaki', 'DeFelipe', 'Flores', 'Hamad', 'Henckens', 'Lewis', 'Long', 'Svoboda'] 
	#     (requires premade subset #10).
	# 14. Set with only the pyramidal cells above 18 days, rat only, excluding the archives 
	#     ['Brown', 'Buzsaki', 'DeFelipe', 'Flores', 'Gonzalez-Burgos', 'Hamad', 'Henckens', 'Lewis', 'Long', 'Svoboda']
	#     (requires premade subset #12).
	# 15. Same as 13 but aged 1 to 18 days (requires premade subset #10).
	# 16. Same as 14 but aged 1 to 18 days (requires premade subset #12). 

	try:
		
		# 1.  
		# makeSubset('appended.csv', './subsets/all_ages_reported.csv', 'if data_set.ix[rowNum, \'Min Age\'] == \'Not reported\' or data_set.ix[rowNum, \'Min Age\'] == 0 or data_set.ix[rowNum, \'Min Age\'] == \'0\' or data_set.ix[rowNum, \'Max Age\'] == \'Not reported\' or data_set.ix[rowNum, \'Max Age\'] == 0 or data_set.ix[rowNum, \'Max Age\'] == \'0\' or data_set.ix[rowNum, \'Min Age\'] == 0.0 or data_set.ix[rowNum, \'Min Age\'] == \'0.0\' or data_set.ix[rowNum, \'Max Age\'] == 0.0 or data_set.ix[rowNum, \'Max Age\'] == \'0.0\':')

		# 2. 
		# makeSubset('./subsets/all_ages_reported.csv', './subsets/1to18days.csv', 'if int(data_set.ix[rowNum, \'Min Age\']) > 18 or int(data_set.ix[rowNum, \'Max Age\']) > 18:')

		# 3. 
		# makeSubset('./subsets/all_ages_reported.csv', './subsets/above18days.csv', 'if int(data_set.ix[rowNum, \'Min Age\']) < 18 or int(data_set.ix[rowNum, \'Max Age\']) < 18:')

		# 4. 
		# makeSubset('appended.csv', './subsets/jacobs_appended.csv', 'if \'jacobs\' not in data_set.ix[rowNum, \'Archive Name\'].lower():')

		# 5. 
		# makeSubset('appended.csv', './subsets/pyramidal_appended.csv', 'if \'pyramidal\' not in data_set.ix[rowNum, \'Secondary Cell Class\'].lower():')

		# 6. 
		# makeSubset('appended.csv', './subsets/ganglion_appended.csv', 'if \'ganglion\' not in data_set.ix[rowNum, \'Secondary Cell Class\'].lower():')

		# 7.
		# makeSubset('appended.csv', './subsets/granule_appended.csv', 'if \'granule\' not in data_set.ix[rowNum, \'Secondary Cell Class\'].lower():')

		# 8.
		# makeSubset('appended.csv', './subsets/drosophila_appended.csv', 'if \'drosophila\' not in data_set.ix[rowNum, \'Species Name\'].lower():')

		# 9.
		# makeSubset('./subsets/1to18days.csv', './subsets/mouse_1to18days.csv', 'if \'mouse\' not in data_set.ix[rowNum, \'Species Name\'].lower():')

		# 10.
		# makeSubset('./subsets/above18days.csv', './subsets/mouse_above18days.csv', 'if \'mouse\' not in data_set.ix[rowNum, \'Species Name\'].lower():')

		# 11.
		# makeSubset('./subsets/1to18days.csv', './subsets/rat_1to18days.csv', 'if \'rat\' not in data_set.ix[rowNum, \'Species Name\'].lower():')

		# 12.
		# makeSubset('./subsets/above18days.csv', './subsets/rat_above18days.csv', 'if \'rat\' not in data_set.ix[rowNum, \'Species Name\'].lower():')

		# 13.
		#makeSubset('./subsets/mouse_above18days.csv', './subsets/excludeArchiveList_pyramidal_mouse_above18days.csv', 'if data_set.ix[rowNum, \'Archive Name\'] in [\'Brown\', \'Buzsaki\', \'DeFelipe\', \'Flores\', \'Hamad\', \'Henckens\', \'Lewis\', \'Long\', \'Svoboda\'] or \'pyramidal\' not in data_set.ix[rowNum, \'Secondary Cell Class\'].lower():')

		# 14.
		#makeSubset('./subsets/rat_above18days.csv', './subsets/excludeArchiveList_pyramidal_rat_above18days.csv', 'if data_set.ix[rowNum, \'Archive Name\'] in [\'Brown\', \'Buzsaki\', \'DeFelipe\', \'Flores\', \'Gonzalez-Burgos\', \'Hamad\', \'Henckens\', \'Lewis\', \'Long\', \'Svoboda\'] or \'pyramidal\' not in data_set.ix[rowNum, \'Secondary Cell Class\'].lower():')

		# 15.
		makeSubset('./subsets/mouse_1to18days.csv', './subsets/excludeArchiveList_pyramidal_mouse_1to18days.csv', 'if data_set.ix[rowNum, \'Archive Name\'] in [\'Brown\', \'Buzsaki\', \'DeFelipe\', \'Flores\', \'Hamad\', \'Henckens\', \'Lewis\', \'Long\', \'Svoboda\'] or \'pyramidal\' not in data_set.ix[rowNum, \'Secondary Cell Class\'].lower():')

		# 16.
		makeSubset('./subsets/rat_1to18days.csv', './subsets/excludeArchiveList_pyramidal_rat_1to18days.csv', 'if data_set.ix[rowNum, \'Archive Name\'] in [\'Brown\', \'Buzsaki\', \'DeFelipe\', \'Flores\', \'Gonzalez-Burgos\', \'Hamad\', \'Henckens\', \'Lewis\', \'Long\', \'Svoboda\'] or \'pyramidal\' not in data_set.ix[rowNum, \'Secondary Cell Class\'].lower():')




		print '\nFor options, please go to the main method of this script.\n'
	except:
		print 'Could not find one or more of the required precursors for making the subsets. Please check the main method and try again.'


# check for strings ko and tko - these are the knockouts we prolly don't want them


if __name__ == "__main__":
	main()