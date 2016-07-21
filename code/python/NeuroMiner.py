"""Take a list of neuron names, and obtain the rest of the information for each one found on http://neuromorpho.org/neuron_info.jsp?neuron_name='name'. Also grab the standardized swc file for each cell."""
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
import string
import shutil


def mine(url):

    a = pd.read_html(url, attrs={'id': 'NeuronInfotable12'})[0]
    a = a.fillna(value='')
    b = pd.DataFrame(a[1].values, index=a[0].values, columns=['Vals'])
    b.index = [x[:-2] for x in b.index]

    c = pd.read_html(url, attrs={'id': 'NeuronInfotable11'})[0]
    c = c.fillna(value='')
    d = pd.DataFrame(c[1].values, index=c[0].values, columns=['Vals'])
    d.index = [x[:-2] for x in d.index]
    k = pd.concat([b, d])

    vals = []
    vals.append(url[23:])
    printable = set(string.printable)
    for column in columns:
        val = k.ix[column, 'Vals']

        #Change months and years to days
        if column == 'Min Age' or 'Max Age':
            if 'months' in val or 'month' in val:
                number_of_days = 365 * float(val.split()[0]) / 12
                val = str(number_of_days)
            if 'years' in val or 'year' in val:
                number_of_days = 365 * float(val.split()[0])
                val = str(number_of_days)
            if 'days' in val or 'day' in val:
                val = val[:-4]

        #Get rid of the microns
        if u'\xa0\u03bcm' in val:
            val = val[:val.index(u'\xa0\u03bcm')]

        #Get rid of degree signs and any other annoying character
        val = filter(lambda x: x in printable, val)

        vals.append(val)

    rows.append(vals)

    return 

def grabFile(url, name):

    links = BeautifulSoup(requests.get(url).text, 'lxml').find_all('a')
    swc_link = [x for x in links if 'Morphology File (Standardized)' in str(x)][0].get('href')
    swc_link = 'http://neuromorpho.org/' + swc_link

    rq = urllib2.Request(swc_link)
    res = urllib2.urlopen(rq)
    swc = open('swcs/' + name[28:] + '.swc', 'wb')
    swc.write(res.read())
    swc.close()

    return

os.chdir('..'); os.chdir('..')

url_template = 'http://neuromorpho.org/{name}'

names = []

# From prior set:
# names_complete = pickle.load(open('./prior/names/names_list.p', 'rb'))
# names_somadend = pickle.load(open('./prior/names/names_list_somadend.p', 'rb'))
# names = names_complete + names_somadend

names = pickle.load(open('./code/python/names/names_list_complete.p', 'rb'))

# TESTING-------------------------------------------------------------------
#testFirst = 25
#names = names[:testFirst]
# --------------------------------------------------------------------------

names = np.array(names).transpose()

total_cell_number = str(len(names))

columns = []

# columns += ["Name"]

columns += ["NeuroMorpho.Org ID", "Neuron Name", "Archive Name",
            "Species Name", "Strain", "Structural Domains", "Physical Integrity",
            "Morphological Attributes", "Min Age", "Max Age", "Gender", "Min Weight",
            "Max Weight", "Development", "Primary Brain Region",
            "Secondary Brain Region", "Tertiary Brain Region", "Primary Cell Class",
            "Secondary Cell Class", "Tertiary Cell Class", "Original Format",
            "Experiment Protocol", "Experimental Condition", "Staining Method",
            "Slicing Direction", "Slice Thickness", "Tissue Shrinkage",
            "Objective Type", "Magnification", "Reconstruction Method",
            "Date of Deposition", "Date of Upload",
            "Soma Surface", "Number of Stems", "Number of Bifurcations",
            "Number of Branches", "Overall Width", "Overall Height",
            "Overall Depth", "Average Diameter", "Total Length", "Total Surface",
            "Total Volume", "Max Euclidean Distance", "Max Path Distance",
            "Max Branch Order", "Average Contraction", "Total Fragmentation",
            "Partition Asymmetry", "Average Rall\'s Ratio",
            "Average Bifurcation Angle Local",
            "Average Bifurcation Angle Remote", "Fractal Dimension"]

choice = 'choice'
grabbing = False; mining = False
while choice not in '123': choice = raw_input("""What do you want to do?
1. Both mine and grab the swcs.
2. Only mine.
3. Only grab swcs. 
""")
if choice == '1': grabbing = True; mining = True
elif choice == '2': grabbing = False; mining = True
elif choice == '3': grabbing = True; mining = False

if not os.path.exists('swcs/'):
        os.makedirs('swcs/')

pleaseRun = False
if grabbing:

    walk = os.walk('./swcs/').next()

    if len(walk[1]) > 0:
    # This is for in case swcGrouper.py has been run, which modifies the directory structure
        existing_swcs=[]
        for dirr in walk[1]:
            globby = glob.glob('./swcs/' + dirr + '/*.swc')
            existing_swcs.extend(globby)
            for each in globby:
                shutil.move(each, './swcs')
            os.rmdir('./swcs/' + dirr + '/')
        pleaseRun = True

    else:
        existing_swcs = glob.glob('./swcs/*.swc')

    start_index_swc = len(existing_swcs)

    for cell_number, name in enumerate(names):
        if cell_number < start_index_swc:
            print 'Cell ' + str(cell_number + 1) + ' has already been grabbed.'
            continue

        url = url_template.format(name=name)
        print "Grabbing " + name + ', cell ' + str(cell_number + 1) + ' / ' + total_cell_number
        
        grabFile(url, name)

#----------------------------------------------------------------------------------------------
os.chdir('./data_sets/raw/')
if mining:

    if os.path.exists('neuroData.csv'):
        existing_neuroData = pd.read_csv('neuroData.csv', index_col=0).index
        start_index_neuroData = len(existing_neuroData)
    else:
        start_index_neuroData = 0

    rows = []
    save_every = 100 #change this to modify how often to save while mining
    for cell_number, name in enumerate(names):

        if cell_number < start_index_neuroData:
            print 'Cell ' + str(cell_number + 1) + ' has already been mined.'
            continue
        print "Mining " + name + ', cell ' + str(cell_number + 1) + ' / ' + total_cell_number
        url = url_template.format(name=name)
        
        mine(url)

        if len(rows) % save_every == 0 or str(cell_number + 1) == total_cell_number:

            frame = pd.DataFrame(np.array(rows)[:,1:], index=np.array(rows)[:,0], columns=columns)
            if not os.path.exists('neuroData.csv'):
                frame.to_csv('neuroData.csv')
            else:
                with open('neuroData.csv', 'a') as fi:
                    frame.to_csv(fi, header=False)
            rows = []
            print '\n' + str(cell_number + 1) + ' cells have been saved.\n'


if pleaseRun:
    print '\nPlease run swcGrouper.py again.'