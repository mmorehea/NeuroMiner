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
split nx2 into schull and gstat

test
