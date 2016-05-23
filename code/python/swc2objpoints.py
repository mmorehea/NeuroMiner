import os
import sys
import glob
import code
import numpy
l = glob.glob("./files/*.swc")

for each in l:
    print each
    numbs = []
    with open(each, 'r') as f:
        s = f.readlines()
    for row in s:
        if row[0] != '#':
            numbs.append(row.split())

    numbs = numpy.array(numbs)
    numbs = numbs[:,2:5]
    ss = each.split('/')[-1][:-4] + '.obj'

    with open('./objs/' + ss, 'w') as ff:
        for row in numbs:
            ff.write('v ' + " ".join(row) + "\r\n")

code.interact(local=locals())
