Go to code/python and open in terminal. Run each of the following scripts in order.

# nameListMaker.py
Input: URL for page with all of the cells - hardcoded in the script
+ If you change the URL, you MUST also delete the file NeuroMiner/data_sets/neuroData.csv AND delete all contents of the folder NeuroMiner/swcs (but keep the empty folder)
Output: NeuroMiner/code/python/names/names_list_complete.p

# NeuroMiner.py
Input: NeuroMiner/code/python/names/names_list_complete.p - the output of nameListMaker.py
Output:
+ Cell data in NeuroMiner/data_sets/neuroData.csv
+ swc files in NeuroMiner/swcs

Required packages: 

The following packages should be installed by default as part of Anaconda:
+ pandas
+ requests
+ beautifulsoup4

This one is probably not included for you:
+ html5lib

WARNING: the newest version of html5lib contains a bug that causes the program to fail. Use an older version of html5lib until the bug is fixed.

Any that weren't included can be installed using:
```
sudo pip install beautifulsoup4
sudo pip install html5lib
sudo pip install requests
sudo pip install pandas
```

If you are still having problems, run the install commands again with --upgrade after each one to ensure you have the latest version.

#swcGrouper.py
Input: ./swcs/ folder containing all the swcs
Output: ./swcs/ folder with the swcs grouped neatly into folders of 1500 each

#LM.jar and LM2features.py
Input: swcs in the ./swcs/ folder
Ouptput: NeuroMiner/data_sets/raw/fixedLmResult.csv

Directions:
LM.jar can effectively handle only 1500 files at a time. Therefore, run swcGrouper.py to glob all the swcs in ./swcs/ and put them in directories of 1500 each. Then point LM.jar at each one, and name the output files in such a way that you can discern the order, e.g. 1, 2, 3, etc. Then run LM2features.py on each one in turn.

#appender.py
Input: neuroData.csv, fixedLmResult.csv, and gstats.csv, which should all be in NeuroMiner/data_sets/raw/
Output: appended.csv, the finished appended product

#R things todo
Maher: stream-line the process of the saving of time and accuracy info, not sure its necessary. I'm sure there is other stuff to do. just ask mike.
Jesse: abbreviation of tables.
Mike: Look at the files and come up with more shit for us to do. I know there is more to do before we start looking at the data. We might be approaching the point where we need to come up with metrics derived from other analyses that are as effective as other metrics. ie does the sum of sholl2-sholl7 equal a better metric than depth from l-measure?
