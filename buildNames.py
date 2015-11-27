import code
import sys
import urllib2
import numpy as np
import cPickle as pickle
import os
import csv
import glob

bigMatrix = pickle.load(open('bm.p', 'rb'))
bigMatrix = bigMatrix[1:, :]
l = []
xx, yy = bigMatrix.shape
fileList = glob.glob('./swcs/*')
numFiles = len(fileList)


for ii in xrange(numFiles, xx):
    print 'Fetching SWC #' + str(ii) + ' out of ' + str(len(bigMatrix))
    front = 'http://neuromorpho.org/neuroMorpho/dableFiles/'
    author = bigMatrix[ii][2].split(',')[0].lower()
    middle = '/CNG%20version/'
    name = bigMatrix[ii][1] + '.CNG.swc'

    httpLink = front + author + middle + name
    print httpLink
    
    try:
        gg = urllib2.urlopen(httpLink)
    except urllib2.HTTPError:
        name = bigMatrix[ii][1] + 'm.CNG.swc'
        httpLink = front + author + middle + name
        gg = urllib2.urlopen(httpLink)

    with open('./swcs/' + bigMatrix[ii][1] + '.swc', 'w') as ff:
        ff.write(gg.read())
