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

os.chdir('..'); os.chdir('..')
def main():
	parameterList = []


	# Parameters for makeSubset:
	#	1. csv_path: the path to the data set from which the subset should be made
	#	2. new_subset_path: the path where you want to make the new subset
	#		Naming convention for new subsets (except for first 3): './subsets/uniqueName_' + filename of the set from which it's made 
	#	3. if_string: the conditions under which particular cells will be dropped. Denote the original data set as 'data_set'.


	# 1. Set with only the cells where ages are reported.
	# parameterList.append(('./data_sets/NeuronDataMaster.csv', './data_sets/Neuron_subsets/all_ages_reported.csv', 'if data_set.ix[rowNum, \'Min Age\'] == \'Not reported\' or data_set.ix[rowNum, \'Min Age\'] == 0 or data_set.ix[rowNum, \'Min Age\'] == \'0\' or data_set.ix[rowNum, \'Max Age\'] == \'Not reported\' or data_set.ix[rowNum, \'Max Age\'] == 0 or data_set.ix[rowNum, \'Max Age\'] == \'0\' or data_set.ix[rowNum, \'Min Age\'] == 0.0 or data_set.ix[rowNum, \'Min Age\'] == \'0.0\' or data_set.ix[rowNum, \'Max Age\'] == 0.0 or data_set.ix[rowNum, \'Max Age\'] == \'0.0\':'))

	# 2. Set with only the cells aged 1-18 days (requires #1).
	# parameterList.append(('./data_sets/Neuron_subsets/all_ages_reported.csv', './data_sets/Neuron_subsets/1to18days.csv', 'if int(data_set.ix[rowNum, \'Min Age\']) > 18 or int(data_set.ix[rowNum, \'Max Age\']) > 18:'))

	# 3. Set with only the cells aged above 18 days (requires #1).
	# parameterList.append(('./data_sets/Neuron_subsets/all_ages_reported.csv', './data_sets/Neuron_subsets/above18days.csv', 'if int(data_set.ix[rowNum, \'Min Age\']) < 18 or int(data_set.ix[rowNum, \'Max Age\']) < 18:'))

	# 4. Set with only the cells from the Jacobs archive.
	# parameterList.append(('./data_sets/NeuronDataMaster.csv', './data_sets/Neuron_subsets/jacobs_NeuronDataMaster.csv', 'if \'jacobs\' not in data_set.ix[rowNum, \'Archive Name\'].lower():'))

	# 5. Set with only the pyramidal cells.
	# parameterList.append(('./data_sets/NeuronDataMaster.csv', './data_sets/Neuron_subsets/pyramidal_NeuronDataMaster.csv', 'if \'pyramidal\' not in data_set.ix[rowNum, \'Secondary Cell Class\'].lower():'))

	# 6. Set with only the ganglion cells.
	# parameterList.append(('./data_sets/NeuronDataMaster.csv', './data_sets/Neuron_subsets/ganglion_NeuronDataMaster.csv', 'if \'ganglion\' not in data_set.ix[rowNum, \'Secondary Cell Class\'].lower():'))

	# 7. Set with only the granule cells.
	# parameterList.append(('./data_sets/NeuronDataMaster.csv', './data_sets/Neuron_subsets/granule_NeuronDataMaster.csv', 'if \'granule\' not in data_set.ix[rowNum, \'Secondary Cell Class\'].lower():'))

	# 8. Set with only the drosophila cells.
	# parameterList.append(('./data_sets/NeuronDataMaster.csv', './data_sets/Neuron_subsets/drosophila_NeuronDataMaster.csv', 'if \'drosophila\' not in data_set.ix[rowNum, \'Species Name\'].lower():'))

	# 9. Set with only the mouse cells aged 1-18 days (requires #2).
	# parameterList.append(('./data_sets/Neuron_subsets/1to18days.csv', './data_sets/Neuron_subsets/mouse_1to18days.csv', 'if \'mouse\' not in data_set.ix[rowNum, \'Species Name\'].lower():'))

	# 10. Set with only the mouse cells aged above 18 days (requires #3).
	# parameterList.append(('./data_sets/Neuron_subsets/above18days.csv', './data_sets/Neuron_subsets/mouse_above18days.csv', 'if \'mouse\' not in data_set.ix[rowNum, \'Species Name\'].lower():'))

	# 11. Set with only the rat cells aged 1-18 days (requires #2).
	# parameterList.append(('./data_sets/Neuron_subsets/1to18days.csv', './data_sets/Neuron_subsets/rat_1to18days.csv', 'if \'rat\' not in data_set.ix[rowNum, \'Species Name\'].lower():'))

	# 12. Set with only the rat cells aged above 18 days (requires #3).
	# parameterList.append(('./data_sets/Neuron_subsets/above18days.csv', './data_sets/Neuron_subsets/rat_above18days.csv', 'if \'rat\' not in data_set.ix[rowNum, \'Species Name\'].lower():'))

	# 13. Set with only the pyramidal cells above 18 days, mouse only, excluding the archives #     ['Brown', 'Buzsaki', 'DeFelipe', 'Flores', 'Hamad', 'Henckens', 'Lewis', 'Long', 'Svoboda'] #     (requires #10).
	# parameterList.append(('./data_sets/Neuron_subsets/mouse_above18days.csv', './data_sets/Neuron_subsets/excludeArchiveList_pyramidal_mouse_above18days.csv', 'if data_set.ix[rowNum, \'Archive Name\'] in [\'Brown\', \'Buzsaki\', \'DeFelipe\', \'Flores\', \'Hamad\', \'Henckens\', \'Lewis\', \'Long\', \'Svoboda\'] or \'pyramidal\' not in data_set.ix[rowNum, \'Secondary Cell Class\'].lower():'))

	# 14. Set with only the pyramidal cells above 18 days, rat only, excluding the archives #     ['Brown', 'Buzsaki', 'DeFelipe', 'Flores', 'Gonzalez-Burgos', 'Hamad', 'Henckens', 'Lewis', 'Long', 'Svoboda']#     (requires #12).
	# parameterList.append(('./data_sets/Neuron_subsets/rat_above18days.csv', './data_sets/Neuron_subsets/excludeArchiveList_pyramidal_rat_above18days.csv', 'if data_set.ix[rowNum, \'Archive Name\'] in [\'Brown\', \'Buzsaki\', \'DeFelipe\', \'Flores\', \'Gonzalez-Burgos\', \'Hamad\', \'Henckens\', \'Lewis\', \'Long\', \'Svoboda\'] or \'pyramidal\' not in data_set.ix[rowNum, \'Secondary Cell Class\'].lower():'))

	# 15. Same as 13 but aged 1 to 18 days (requires #10).
	# parameterList.append(('./data_sets/Neuron_subsets/mouse_1to18days.csv', './data_sets/Neuron_subsets/excludeArchiveList_pyramidal_mouse_1to18days.csv', 'if data_set.ix[rowNum, \'Archive Name\'] in [\'Brown\', \'Buzsaki\', \'DeFelipe\', \'Flores\', \'Hamad\', \'Henckens\', \'Lewis\', \'Long\', \'Svoboda\'] or \'pyramidal\' not in data_set.ix[rowNum, \'Secondary Cell Class\'].lower():'))

	# 16. Same as 14 but aged 1 to 18 days (requires #12).
	# parameterList.append(('./data_sets/Neuron_subsets/rat_1to18days.csv', './data_sets/Neuron_subsets/excludeArchiveList_pyramidal_rat_1to18days.csv', 'if data_set.ix[rowNum, \'Archive Name\'] in [\'Brown\', \'Buzsaki\', \'DeFelipe\', \'Flores\', \'Gonzalez-Burgos\', \'Hamad\', \'Henckens\', \'Lewis\', \'Long\', \'Svoboda\'] or \'pyramidal\' not in data_set.ix[rowNum, \'Secondary Cell Class\'].lower():'))
	
	# 17. Set with only those species that appear more than 100 times in the original dataset.
	# parameterList.append(('./data_sets/NeuronDataMaster.csv', './data_sets/Neuron_subsets/speciesCountOver100_NeuronDataMaster.csv', 'if data_set.ix[rowNum, \'Species Name\'] in [\'elephant\', \'pouched lamprey\', \'salamander\', \'minke whale\', \'goldfish\', \'blowfly\', \'bottlenose dolphin\', \'chicken\', \'frog\', \'Siberian tiger\', \'clouded leopard\', \'guinea pig\', \'zebrafish\', \'proechimys\', \'drosophila melanogaster\', \'manatee\', \'moth\', \'spiny lobster\', \'rabbit\']:'))

	# 18. Set with only those species that appear less than 100 times in the original dataset. 
	# parameterList.append(('./data_sets/NeuronDataMaster.csv', './data_sets/Neuron_subsets/speciesCountUnder100_NeuronDataMaster.csv', 'if data_set.ix[rowNum, \'Species Name\'] in [\'rat\', \'mouse\', \'human\', \'chimpanzee\', \'monkey\', \'giraffe\', \'C. elegans\', \'sheep\', \'domestic pig\', \'humpback whale\', \'cat\']:'))

	# 19. Set from Jesse's email, #1.
	# parameterList.append(('./data_sets/NeuronDataMaster.csv', './data_sets/Neuron_subsets/subset1_NeuronDataMaster.csv', 'if data_set.ix[rowNum, \'Species Name\'].lower() not in [\'rat\', \'mouse\'] or data_set.ix[rowNum, \'Secondary Cell Class\'].lower() not in [\'pyramidal\'] or data_set.ix[rowNum, \'Primary Brain Region\'].lower() not in [\'neocortex\', \'cerebellum\', \'amygdala\']:'))

	# 20. Set from Jesse's email, #2. 
	parameterList.append(('./data_sets/NeuronDataMaster.csv', './data_sets/Neuron_subsets/subset2_NeuronDataMaster.csv', 'if data_set.ix[rowNum, \'Species Name\'].lower() not in [\'rat\'] or not ((data_set.ix[rowNum, \'Primary Brain Region\'].lower() in [\'hippocampus\'] and data_set.ix[rowNum, \'Secondary Cell Class\'].lower() in [\'pyramidal\']) or (data_set.ix[rowNum, \'Primary Brain Region\'].lower() in [\'brainstem\'] and data_set.ix[rowNum, \'Secondary Cell Class\'].lower() in [\'motoneuron\']) or (data_set.ix[rowNum, \'Primary Brain Region\'].lower() in [\'neocortex\'] and data_set.ix[rowNum, \'Secondary Cell Class\'].lower() in [\'pyramidal\', \'basket\'])):'))


	
	for each in parameterList:
		makeSubset(each[0], each[1], each[2])
	print '\nFor options, please go to the main method of this script.\n'


# check for strings ko and tko - these are the knockouts we prolly don't want them


if __name__ == "__main__":
	main()