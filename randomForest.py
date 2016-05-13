"""Machine learning with scikit-learn. Scroll down to main method for options."""
# -*- coding: utf-8 -*-

import numpy as np
import pandas as pd
import csv
import code
import os
import pickle
from sklearn.ensemble import RandomForestClassifier
import random
from sklearn.preprocessing import scale
from sklearn import cross_validation

# Scroll down to main method for options.

class color:
   PURPLE = '\033[95m'
   CYAN = '\033[96m'
   DARKCYAN = '\033[36m'
   BLUE = '\033[94m'
   GREEN = '\033[92m'
   YELLOW = '\033[93m'
   RED = '\033[91m'
   BOLD = '\033[1m'
   UNDERLINE = '\033[4m'
   END = '\033[0m'

def rf(csv_path, test_size, y_name, y_name_excludeList, x_name_range, x_name_range_excludeList, scale_excludeList): # string, float, string, list of strings, tuple, list, list  
	# Variable naming note - pandas objects are designated with the word 'set', while numpy arrays contain the word 'array'.

	data_set = pd.read_csv(csv_path)

	desired_x_columns = [c for c in data_set.columns[list(data_set.columns).index(x_name_range[0]):list(data_set.columns).index(x_name_range[1]) + 1] if c not in x_name_range_excludeList]

	data_set.dropna(axis=0, inplace=True, subset=desired_x_columns)

	groupList = [(name, group) for name, group in data_set.groupby(y_name)]
	deleteList = y_name_excludeList
	
	outGroup = [each for each in groupList if each[0] in deleteList]
	groupList = [each for each in groupList if each[0] not in deleteList]

	valueCounts = data_set.ix[:, y_name].value_counts()
	minCount = min([each for n, each in enumerate(valueCounts) if valueCounts.index[n] not in y_name_excludeList])
	random_samples_indices = []

	for group in groupList: random_samples_indices.extend(random.sample(group[1].index, minCount))

	X_set = data_set.ix[random_samples_indices, desired_x_columns]
	y_set = data_set.ix[random_samples_indices, y_name]

	X_array_scaledPart = scale(X_set.ix[:, [c for c in X_set.columns if c not in scale_excludeList]].values.astype(np.float32))
	X_array = np.c_[X_array_scaledPart, X_set.ix[:, [c for c in X_set.columns if c in scale_excludeList]].values.astype(np.float32)]

	y_array = y_set.values

	x_train, x_test, y_train, y_test = cross_validation.train_test_split(X_array, y_array, test_size=test_size, random_state=0)

	n_estimators = 20
	clf = RandomForestClassifier(n_estimators=n_estimators, n_jobs=-1).fit(x_train, y_train)

	# print clf.score(x_test, y_test)

	scores = cross_validation.cross_val_score(clf, X_array, y_array, cv=5)

	resultString = '\n\nData set: ' + csv_path + '\nTarget: ' + y_name  + '\nTest Size: ' + str(test_size) + '\nn_estimators: ' + str(n_estimators)
	resultString += '\nGroups: ' + str([group[0] + ' (' + str(data_set.ix[:,y_name].value_counts().ix[group[0], 0]).replace('\'','').replace('[','').replace(']','') + ')' for group in groupList]).replace('\'','').replace('[','').replace(']','') + '\nDiscarded Groups: ' + str([group[0] + ' (' + str(data_set.ix[:,y_name].value_counts().ix[group[0], 0]).replace('\'','').replace('[','').replace(']','') + ')' for group in outGroup]).replace('\'','').replace('[','').replace(']','') + '\nNumber of Samples per Group: ' + str(minCount)
	resultString += '\n\nAccuracy: ' + '%0.2f (+/- %0.2f)' %(scores.mean(), scores.std() * 2) + '\n----------------------------------'

	print resultString

	q = raw_input('Would you like to save the results of this run? (y/n)')
	if q == 'y':
		if not os.path.exists('rfResults.txt'):
			with open('rfResults.txt', 'w') as output:
				output.write(resultString)
		else:
			with open('rfResults.txt', 'a') as output:
				output.write(resultString)


def main():
	# Important parameters that can be tweaked within the rf function:
	# 	Cross validation settings
	#		number of iterations
	#	Random forest settings
	#		n_estimators
	#		n_jobs

	print '\nPlease go to the main method of this script for options.\n'
	
	# print pd.read_csv('./subsets/excludeArchiveList_pyramidal_mouse_1to18days.csv').ix[:, 'Archive Name'].value_counts()
	# code.interact(local=locals())
	# This will allow you to check the value counts for your path and target column to see what you want to exclude.


	rf('./subsets/excludeArchiveList_pyramidal_rat_above18days.csv', 0.4, 'Primary Brain Region', ['entorhinal cortex'], ('Soma Surface', 'gstats 63'), ['gstats 0'], ['gstats ' + str(x) for x in xrange(66)])
	# Parameters:
	# 1. path to data set
	# 2. test size
	# 3. name of target column
	# 4. list of names to exclude from target. Suggestions:
	# 		for Jacobs on Species Name: ['Siberian tiger', 'clouded leopard', 'manatee']
	# 		for Jacobs on Secondary Cell Class: ['sternzelle', 'Crab-like', 'neurogliaform', 'Aspiny', 'Golgi']
	#		for pyramidal rat above 18 days, excluding a list of archives (see subsetMaker.py) on Archive Name: ['Lee_LJ', 'Kole', 'Brown', 'Rice-Baylor', 'Yuan', 'Claiborne', 'Barrionuevo', 'Bikson', 'Groen', 'Spruston', 'Dendritica', 'Helmstaedter', 'Markram', 'Johnston', 'Turner,Ascoli,Buzsaki', 'Barbour', 'Gonzalez-Burgos', 'Turner,Ascoli,Wittner,Buzsaki', 'Palmer', 'Bergstrom', 'Cauli']
	#			same but on Primary Brain Region - ['entorhinal cortex']
	#			same but mouse - ['Lee', 'Smit-Rigter', 'Kimura', 'Margrie', 'Korte', 'Soltesz', 'Wu', 'Cohen,Mizrahi', 'Anton', 'Calabresi', 'Gonzalez-Burgos', 'Prince', 'Bacci']
	# 5. tuple indicating the range of columns to use for X
	# 6. list of column names to exclude from that range - probably don't need to change default: ['gstats 0']
	# 7. list of column names to exclude from scaling - probably don't need to change default: ['gstats ' + str(x) for x in xrange(66)]  


	rf('./subsets/excludeArchiveList_pyramidal_rat_above18days.csv', 0.4, 'Archive Name', ['Lee_LJ', 'Kole', 'Brown', 'Rice-Baylor', 'Yuan', 'Claiborne', 'Barrionuevo', 'Bikson', 'Groen', 'Spruston', 'Dendritica', 'Helmstaedter', 'Markram', 'Johnston', 'Turner,Ascoli,Buzsaki', 'Barbour', 'Gonzalez-Burgos', 'Turner,Ascoli,Wittner,Buzsaki', 'Palmer', 'Bergstrom', 'Cauli'], ('Soma Surface', 'gstats 63'), ['gstats 0'], ['gstats ' + str(x) for x in xrange(66)])

	rf('./subsets/excludeArchiveList_pyramidal_mouse_above18days.csv', 0.4, 'Primary Brain Region', ['entorhinal cortex'], ('Soma Surface', 'gstats 63'), ['gstats 0'], ['gstats ' + str(x) for x in xrange(66)])

	rf('./subsets/excludeArchiveList_pyramidal_mouse_above18days.csv', 0.25, 'Archive Name', ['Lee', 'Smit-Rigter', 'Kimura', 'Margrie', 'Korte', 'Soltesz', 'Wu', 'Cohen,Mizrahi', 'Anton', 'Calabresi', 'Gonzalez-Burgos', 'Prince', 'Bacci'], ('Soma Surface', 'gstats 63'), ['gstats 0'], ['gstats ' + str(x) for x in xrange(66)])



	rf('./subsets/excludeArchiveList_pyramidal_rat_1to18days.csv', 0.4, 'Primary Brain Region', [], ('Soma Surface', 'gstats 63'), ['gstats 0'], ['gstats ' + str(x) for x in xrange(66)])

	rf('./subsets/excludeArchiveList_pyramidal_rat_1to18days.csv', 0.4, 'Archive Name', ['Hay,Markram', 'Cauli'], ('Soma Surface', 'gstats 63'), ['gstats 0'], ['gstats ' + str(x) for x in xrange(66)])

	rf('./subsets/excludeArchiveList_pyramidal_mouse_1to18days.csv', 0.4, 'Primary Brain Region', [], ('Soma Surface', 'gstats 63'), ['gstats 0'], ['gstats ' + str(x) for x in xrange(66)])

	rf('./subsets/excludeArchiveList_pyramidal_mouse_1to18days.csv', 0.25, 'Archive Name', ['Tagawa', 'Smit-Rigter', 'Krieger', 'Anton', 'Poorthuis', 'Heistek_Mansvelder'], ('Soma Surface', 'gstats 63'), ['gstats 0'], ['gstats ' + str(x) for x in xrange(66)])


if __name__ == "__main__":
	main()
