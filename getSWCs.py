import os
import sys
import glob
import code
import numpy
import cPickle as pickle
import urllib2

f = pickle.load(open("bigFiles.p", "rb"))
for each in xrange(500):
    print 'http://neuromorpho.org/neuroMorpho/' + f[each]
    gg = urllib2.urlopen('http://neuromorpho.org/neuroMorpho/' + f[each])
    name = f[each].split('/')[-1]
    with open('./swcs/' + name, 'w') as ff:
        ff.write(gg.read())


code.interact(local=locals())
