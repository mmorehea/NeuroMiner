from bs4 import BeautifulSoup
import urllib
import code
import string
import sys
import urllib2
import numpy as np
import cPickle as pickle
import os
import csv

with open('fullLinks.txt', "r") as text_file:
    hotLinks = text_file.read().split('\n')[0:-1]

hotWords = ["NeuroMorpho.Org ID", "Neuron Name", "Archive Name", \
"Species Name", "Strain", "Structural Domains", "Physical Integrity", \
"Morphological Attributes", "Min Age", "Max Age", "Gender", "Min Weight",\
"Max Weight", "Development", "Primary Brain Region", \
"Secondary Brain Region", "Tertiary Brain Region" , "Primary Cell Class", \
"Secondary Cell Class", "Tertiary Cell Class", "Original Format", \
"Experiment Protocol", "Experimental Condition", "Staining Method", \
"Slicing Direction", "Slice Thickness", "Tissue Shrinkage", \
"Objective Type", "Magnification", "Reconstruction Method", \
"Date of Deposition", "Date of Upload", \
"Soma Surface", "Number of Stems", "Number of Bifurcations", \
"Number of Branches", "Overall Width", "Overall Height", \
"Overall Depth", "Average Diameter", "Total Length", "Total Surface",\
"Total Volume", "Max Euclidean Distance", "Max Path Distance", \
"Max Branch Order", "Average Contraction", "Total Fragmentation", \
"Partition Asymmetry", "Average Rall\'s Ratio", \
"Average Bifurcation Angle Local", \
"Average Bifurcation Angle Remote", "Fractal Dimension"]


def checkLine(stringLine):
    if len(str(stringLine.contents)) > 100:
        return -1

    resp = [1 if x in str(stringLine.contents) else 0 for x in hotWords]

    try:
        indx = resp.index(1)
    except ValueError:
        return -1
    return indx



def getNeuron(htmlLink):
    r = urllib.urlopen(htmlLink)
    p = BeautifulSoup(r)
    l = p.find_all('td')
    row = [-1] * len(hotWords)
    links = p.find_all('a')

    for each in links:
        if "Morphology File (Standardized)" in each.contents[0].encode('utf-8'):
            f = each['href']

    for xx in range(26, len(l)):
        indx = checkLine(l[xx])
        if indx == -1:
            continue
        if len(l[xx+1].contents) == 3:
            try:
                ss = l[xx+1].contents[1].string.encode('utf-8')
            except AttributeError:
                ss = 'Failed to mine'
        else:
            try:
                ss = l[xx+1].contents[0].encode('utf-8').strip()
            except AttributeError:
                code.interact(local=locals())
            ss = filter(lambda x: x in string.printable, ss)


        row[indx] = ss
        row.append(f)
    return row

def getLinks(httpLink):
    r = urllib.urlopen(httpLink).read()
    soup = BeautifulSoup(r)
    be = soup.find_all("input", {"name": "neuron_name"})
    names = []

    for each in be:
    	names.append(each['value'])

    return names

def getAllLinks():
    # getAllLinks will go through each of the hotLinks, and produce a binary file
    # which contains a list of every unique neuron name
    if os.path.isfile('./links.p'):
        allNames = pickle.load(open( "./links.p", "rb" ))
    else:
        for each in hotLinks:
            print each
            allNames.append(getLinks(each))
        allNames = [item for sublist in allNames for item in sublist]
        pickle.dump(allNames, open( "./links.p", "wb" ))
    return allNames


def main():
    allNames = getAllLinks()
    allFiles = []
    bigMatrix = np.array(hotWords)

    if os.path.isfile('./bm.p'):

        bigMatrix = pickle.load(open( "./bm.p", "rb" ))
        xBM, yBM = bigMatrix.shape
        print 'BM file found -- length == ' + str(xBM)
    else:
        xBM = 0

    for ii, each in enumerate(allNames, start = xBM):
        if ii == len(allNames):
            break
        print allNames[ii], ii, len(allNames)
        d = 'http://neuromorpho.org/neuroMorpho/neuron_info.jsp?neuron_name='
        dd = d + allNames[ii]
        row = getNeuron(dd)
        row = np.array(row)
        bigMatrix = np.vstack((bigMatrix, row))
        if ii % 30 == 0:
            pickle.dump(bigMatrix, open( "./bm.p", "wb" ))
    with open('bigMatrix.csv', 'w') as ff:
        csv.writer(ff).writerows(bigMatrix)
    pickle.dump(bigMatrix, open( "./bm.p", "wb" ))


if __name__ == "__main__":
    main()
