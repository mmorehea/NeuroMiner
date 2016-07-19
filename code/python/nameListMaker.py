# -*- coding: utf-8 -*-

import numpy as np
import pandas as pd
import csv
import webbrowser

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

baseHtmlLink = 'http://neuromorpho.org/MetaDataBrowseView.jsp?neuron.species_id=defaultValue&neuron.strain_id=defaultValue&neuron.gender=defaultValue&neuron.min_weight_operation=defaultValue&neuron.min_weight=defaultValue&neuron.max_weight_operation=defaultValue&neuron.max_weight=defaultValue&neuron.age_classification_id=defaultValue&neuron.min_age_operation=defaultValue&neuron.min_age=defaultValue&neuron.min_age_scale=defaultValue&neuron.max_age_operation=defaultValue&neuron.max_age=defaultValue&neuron.max_age_scale=defaultValue&neuron.stain_id=defaultValue&neuron.reconstruction_id=defaultValue&neuron.protocol_id=defaultValue&neuron.objective_id=defaultValue&neuron.magnification_id=defaultValue&neuron.thickness_id=defaultValue&neuron.slice_direction_id=defaultValue&neuron.expercond_id=defaultValue&Tissue_shrinkage.shrinkage_reported=defaultValue&neuron.region1_id=defaultValue&neuron.region2_id=defaultValue&neuron.region3_id=defaultValue&neuron.class1_id=defaultValue&neuron.class2_id=defaultValue&neuron.class3_id=defaultValue&neuron.archive_id=defaultValue&neuron_article.PMID=defaultValue&neuron.neuron_id=defaultValue&neuron.format_id=defaultValue&deposition.deposition_date=defaultValue&deposition.upload_date=defaultValue&neuron_completeness.domain_id=7:&neuron_completeness.attributes_id=defaultValue&integritySearchCriteria=5:&neuron_completeness.integrity_id=defaultValue&neuron_completeness.den_ax_integrity_id=defaultValue&browseBy=1'
baseHtmlLink = 'http://neuromorpho.org/MetaDataBrowseView.jsp?neuron.species_id=defaultValue&neuron.strain_id=defaultValue&neuron.gender=defaultValue&neuron.min_weight_operation=defaultValue&neuron.min_weight=defaultValue&neuron.max_weight_operation=defaultValue&neuron.max_weight=defaultValue&neuron.age_classification_id=defaultValue&neuron.min_age_operation=defaultValue&neuron.min_age=defaultValue&neuron.min_age_scale=defaultValue&neuron.max_age_operation=defaultValue&neuron.max_age=defaultValue&neuron.max_age_scale=defaultValue&neuron.stain_id=defaultValue&neuron.reconstruction_id=defaultValue&neuron.protocol_id=defaultValue&neuron.objective_id=defaultValue&neuron.magnification_id=defaultValue&neuron.thickness_id=defaultValue&neuron.slice_direction_id=defaultValue&neuron.expercond_id=defaultValue&Tissue_shrinkage.shrinkage_reported=defaultValue&neuron.region1_id=defaultValue&neuron.region2_id=defaultValue&neuron.region3_id=defaultValue&neuron.class1_id=defaultValue&neuron.class2_id=defaultValue&neuron.class3_id=defaultValue&neuron.archive_id=defaultValue&neuron_article.PMID=defaultValue&neuron.neuron_id=defaultValue&neuron.format_id=defaultValue&deposition.deposition_date=defaultValue&deposition.upload_date=defaultValue&neuron_completeness.domain_id=5:,4:,7:,6:,1:,3:&neuron_completeness.attributes_id=defaultValue&integritySearchCriteria=5:&neuron_completeness.integrity_id=defaultValue&neuron_completeness.den_ax_integrity_id=defaultValue&browseBy=1'

list_of_names = []




r = urllib.urlopen(baseHtmlLink)
p = BeautifulSoup(r, "lxml")
l = p.find_all('td')

links = p.find_all('a')

neuron_links = [x.get('href') for x in links if 'neuron_name' in str(x)]

list_of_names = list_of_names + neuron_links

if not os.path.exists('names/'):
        os.makedirs('names/')

pickle.dump(list_of_names, open('names/names_list_complete.p', 'wb'))
