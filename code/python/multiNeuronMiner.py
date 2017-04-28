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
import multiprocessing
from multiprocessing.dummy import Pool as ThreadPool

NUMBERCORES = multiprocessing.cpu_count()
print "Found " + str(NUMBERCORES) + " number of cores. Using " + str(NUMBERCORES - 1) + "."
NUMBERCORES -= 1


def mine(url):
    print url
    rows = []
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

    return rows

def grabFile(url, name):

    links = BeautifulSoup(requests.get(url).text, 'lxml').find_all('a')
    swc_link = [x for x in links if 'Morphology File (Standardized)' in str(x)][0].get('href')
    swc_link = 'http://neuromorpho.org/' + swc_link


    rq = urllib2.Request(swc_link)
    try:
        res = urllib2.urlopen(rq)
    except:
        return
    swc = open('swcs/' + name + '.swc', 'wb')
    swc.write(res.read())
    swc.close()

    return

def poolMiner(listOfURLS):
    pool = ThreadPool(NUMBERCORES)
    result = pool.map(mine, listOfURLS)
    return result

url_template = 'http://neuromorpho.org/neuron_info.jsp?neuron_name='

names = []

# From prior set:
# names_complete = pickle.load(open('./prior/names/names_list.p', 'rb'))
# names_somadend = pickle.load(open('./prior/names/names_list_somadend.p', 'rb'))
# names = names_complete + names_somadend

with open('new_list.p', 'r') as f:
    names = f.readlines()

names = [i.strip() for i in names]

# TESTING-------------------------------------------------------------------
#testFirst = 25
#names = names[:testFirst]
# --------------------------------------------------------------------------

#names = np.array(names).transpose()

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


with open("./NeuroMiner/prior/names/all_neuron_list.txt") as f:
    listOfURLs = f.readlines()

listOfURLs = [url_template + n for n in listOfURLs]
poolMiner(listOfURLs)
