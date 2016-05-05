"""Glob all the swc files and break them up into separate directories of 1000 each."""
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

path = raw_input('Please input the path to the directory with all the swc files: ')
swcs = glob.iglob(path)

code.interact(local=locals())

totalGroups = 1 + len(swcs)/1000 

code.interact(local=locals())

# for swc in swcs:
