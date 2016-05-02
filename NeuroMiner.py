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
<<<<<<< HEAD
import time
=======
import glob
>>>>>>> 61dae9ee8ef6f41d1ad4950a9bace30d4d9806bb

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
    for column in columns:
        vals.append(k.ix[column, 'Vals'])
    rows.append(vals)

    return rows

def grabFile(url, name):

    links = BeautifulSoup(requests.get(url).text, 'lxml').find_all('a')
    swc_link = [x for x in links if 'Morphology File (Standardized)' in str(x)][0].get('href')
    swc_link = 'http://neuromorpho.org/' + swc_link

    if not os.path.exists('swcs/'):
        os.makedirs('swcs/')

    rq = urllib2.Request(swc_link)
    res = urllib2.urlopen(rq)
    swc = open('swcs/' + name[28:] + '.swc', 'wb')
    swc.write(res.read())
    swc.close()

    return

url_template = 'http://neuromorpho.org/{name}'

names = []
names_complete = pickle.load(open('names/names_list.p', 'rb'))
names_somadend = pickle.load(open('names/names_list_somadend.p', 'rb'))
names = names_complete + names_somadend

# with open('names.csv', 'rb') as f:
#     reader = csv.reader(f)
#     for row in reader:
#         names.append(row[0].split('.')[0])

# TESTING-------------------------------------------------------------------
testFirst = 12
names = names[:testFirst]
# --------------------------------------------------------------------------

names = np.array(names).transpose()

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

existing_swcs = glob.glob('./swcs/*.swc')
start_index = len(existing_swcs)

total_cell_number = str(len(names))
rows = []

mineav = []
grabav = []
for cell_number, name in enumerate(names):
    
    print "Getting " + name + ', cell ' + str(cell_number + 1) + ' / ' + total_cell_number
    url = url_template.format(name=name)

    Astart = time.time()
    rows = mine(url)
    Aend = time.time()
    timez = Aend - Astart
    print '\nmine time: ' + str(timez)
    mineav.append(timez)

<<<<<<< HEAD
    Bstart = time.time()
=======
    if cell_number < start_index:
        print 'Cell ' + str(cell_number + 1) + ' has already been grabbed.'
        continue

>>>>>>> 61dae9ee8ef6f41d1ad4950a9bace30d4d9806bb
    grabFile(url, name)
    Bend = time.time()
    timez = Bend - Bstart
    print 'grab file time: ' + str(timez) + '\n'
    grabav.append(timez)

print "mine average: " + str(sum(mineav) / len(mineav))

print "grab file average: " + str(sum(grabav) / len(grabav))

frame = pd.DataFrame(np.array(rows), columns=columns)

frame.to_csv('neuroData.csv', encoding='UTF-16')
