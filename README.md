# NeuroMiner
The following packages should be installed by default as part of Anaconda:
+ pandas
+ requests
+ beautifulsoup4

This one is probably not included for you:
+ html5lib

Any that weren't included can be installed using:
```
sudo pip install beautifulsoup4
sudo pip install html5lib
sudo pip install requests
sudo pip install pandas
```


beautifulsoup4 and html5lib are integral for the "read_html()" function in the program to work.

#LM.jar and LM2features.py
LM.jar can effectively handle only 1500 files at a time. Therefore, run swcGrouper.py to glob all the swcs in ./swcs/ and put them in directories of 1500 each. Then point LM.jar at each one, and name the output files in such a way that you can discern the order, e.g. 1, 2, 3, etc. Then run LM2features.py on each one in turn.

#R things todo
Maher: stream-line the process of the saving of time and accuracy info, not sure its necessary. I'm sure there is other stuff to do. just ask mike.
Jesse: abbreviation of tables.
Mike: Look at the files and come up with more shit for us to do. I know there is more to do before we start looking at the data. We might be approaching the point where we need to come up with metrics derived from other analyses that are as effective as other metrics. ie does the sum of sholl2-sholl7 equal a better metric than depth from l-measure?
