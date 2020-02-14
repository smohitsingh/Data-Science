# -*- coding: utf-8 -*-
"""
Created on Thu Feb  6 16:02:23 2020

@author: MS00612816
"""





# Loading required libraries
import pandas as pd
# Scikit-learn
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import LabelEncoder
# Keras
import tensorflow
import keras
from keras.preprocessing.text import Tokenizer
from keras.preprocessing.sequence import pad_sequences
from keras.models import Sequential
from keras.layers import Activation, Dense, Dropout, Embedding, Flatten, Conv1D, MaxPooling1D, LSTM
from keras import utils
from keras.callbacks import ReduceLROnPlateau, EarlyStopping
from keras.callbacks import EarlyStopping, ModelCheckpoint
from keras.utils import to_categorical
# nltk
import nltk
from nltk.corpus import stopwords
# Utility
import re
import numpy as np
import os
from collections import Counter
import logging
import time
import pickle
import itertools
import matplotlib.pyplot as plt
import seaborn as sns
from keras.layers import Dense, Embedding,LSTM,Dropout
from keras.layers import Bidirectional
from pandas import DataFrame
import emoji
from collections import defaultdict



import pandas as pd
file1 = r'D:\sentiment analysis\Customer_Chat_Model\training_data_v1.xlsx'
raw_data_1  = pd.read_excel(file1,sheet_name='Sheet1')


import pandas as pd
file2 = r'D:\sentiment analysis\Customer_Chat_Model\training_data_v2.xlsx'
raw_data_2 = pd.read_excel(file2,sheet_name='Sheet1')


####----------- Combining all the data to get the final data frame "tweet_data"

raw_data = pd.concat([raw_data_1,raw_data_2])


raw_data.info()
raw_data.columns
# [ 'chat_id', 'tidy_text', 'issue_label', 'labels_v1', 'vlookup' ]

raw_data.shape
#   (18371, 5)

raw_data = raw_data.loc[:, 'chat_id' :'issue_label']

raw_data = raw_data[raw_data['issue_label'].notnull()]
raw_data = raw_data[raw_data['tidy_text'].notnull()]
raw_data = raw_data[raw_data['chat_id'].notnull()]

raw_data.shape 

raw_data = raw_data.drop_duplicates()
raw_data = raw_data.reset_index(drop = True)

raw_data.shape 

raw_data['chat_id'].nunique()
raw_data['issue_label'].nunique()


raw_data = raw_data.astype(str)


raw_data['tidy_text_1'] =  raw_data['tidy_text'].str.lower()

# strip the blank spaces in the starting and end of the  "tidy_summary"
raw_data['tidy_text_1'] = raw_data['tidy_text_1'].apply(lambda x: x.strip())


# checking the distribution of the labels & unique labels on our data.

issue_labels_1 = raw_data.issue_label.value_counts(dropna=True, sort=True).reset_index(drop = False)
issue_labels_1.columns = ["issue","count"]


print(" Let's check Number of Labels in our data  : {}".format(len(issue_labels_1)))

issue_labels = issue_labels_1

round(raw_data['issue_label'].value_counts(normalize=True)*100,2)[0:len(issue_labels_1)-1]

# now removing the data with the "remove_label"
raw_data = raw_data[raw_data.issue_label.isin(issue_labels.issue)]

print("Dimension of raw_data data : {}".format(raw_data.shape))

raw_data['issue_label'].value_counts()

round(raw_data['issue_label'].value_counts(normalize=True)*100,2)






