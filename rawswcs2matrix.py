import os
import sys
import glob
import code
import numpy
import csv

l = glob.glob('./swcs/*.swc')

allLines = []

for each in l:
    print each
    with open(each, 'r') as f:
        fileText = f.readlines()

    fileText = [e.strip() for e in fileText if len(e) > 0 and e[0] != '#']
    fileText = [each.split('/')[-1], ' '.join(fileText)]
    #code.interact(local=locals())
    allLines.append(fileText)

with open("raw_CSV_Matrix.csv", "w") as f:
    writer = csv.writer(f)
    writer.writerows(allLines)
