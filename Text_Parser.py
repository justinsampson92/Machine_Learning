import nltk
from sys import argv
import string
import csv
import numpy as np
import collections


common_words = ['a','able','about','across','after','all','almost','also','am','among','an','and','any','are','as','at','be','because','been','but','by','can','cannot','could','dear','did','do','does','either','else','ever','every','for','from','get','got','had','has','have','he','her','hers','him','his','how','however','i','if','in','into','is','it','its','just','least','let','like','likely','may','me','might','most','must','my','neither','no','nor','not','of','off','often','on','only','or','other','our','own','rather','said','say','says','she','should','since','so','some','than','that','the','their','them','then','there','these','they','this','tis','to','too','twas','us','wants','was','we','were','what','when','where','which','while','who','whom','why','will','with','would','yet']


script, input_file = argv

loc1 = '/Users/justinsampson/testing/df_test.csv'
loc2 = '/Users/justinsampson/testing/unique_test.csv'
current_file = open(input_file)

##get the words
def get_data(f):
	global text_list
	all_lines = f.readlines()
	for sentence in all_lines:
		tokens = sentence.split()
		words = [s.translate(None, string.punctuation) for s in tokens]
		clean = [w for w in words if not w in common_words]
		text_list.append(clean)
	

##get unique list
text_list = []
get_data(current_file)


unique = sum(text_list, [])
unique = [x.lower() for x in unique]
unique_list = [] 
for i in unique:
      if not i in unique_list:
         unique_list.append(i)


df = []
for line in range(0,len(text_list)):
	row_count = []
	row_perc = []
	for word in unique_list:
		counts = text_list[line].count(word)
		row_count.append(counts)
	df.append(row_count)





with open(loc1, "w") as output:
   writer = csv.writer(output, lineterminator='\n')
   writer.writerows(df)

with open(loc2, "w") as output:
   writer = csv.writer(output, lineterminator='\n')
   writer.writerows([unique_list])
