# -*- coding: utf-8 -*-
"""
Created on Wed Jan  1 23:50:48 2020

@author: Mohit Singh
"""


# loading the best model saved & tokenizer
import pandas as pd
# Scikit-learn
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import LabelEncoder
# Keras
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
from collections import defaultdict
from keras.models import load_model



model_test = load_model(r'C:\Users\Mohit Singh\Downloads\BestModel_01.h5')
with open(r'C:\Users\Mohit Singh\Downloads\tokenizer_1.pickle', 'rb') as handle:
    tok = pickle.load(handle)


test_data = pd.read_excel (r'C:\Users\Mohit Singh\Downloads\test data -dec31st.xlsx',sheet_name= "Sheet1") #(use "r" before the path string to address special character, such as '\'). Don't forget to put the file name at the end of the path + '.xlsx'


print(test_data.shape)
print(test_data.columns)


# changing the data type to "string" data type
test_data["Summary"] = test_data.Summary.apply(str)
#test_data["LABEL"] = test_data.LABEL.apply(str)


# Convert to lower
test_data['tidy_summary']= test_data.Summary.str.lower()

# strip the blank spaces in the starting and end of the  "tidy_summary"
test_data['tidy_summary'] = test_data['tidy_summary'].apply(lambda x: x.strip())

# Removal of ASCII, Punctuation, special character, @user_id, hyperlinks, numbers. Special case not handled previously,      
test_data['tidy_summary'] = test_data['tidy_summary'].str.replace("(@[A-Za-z0-9]+)|([^0-9A-Za-z \t])|(\w+:\/\/\S+)|[\.\,\!\?\:\;\-\=]|\x92|[0-9]+|www.[^ ]+|[^\x00-\x7F]+|\d+", ' ')
test_data['tidy_summary'] = test_data['tidy_summary'].apply(lambda x: re.sub('  ',' ', x))
 
# remove https hyperlinks
test_data['tidy_summary'] = test_data['tidy_summary'].str.replace("http\S+|[^\x00-\x7F]+|\d+", '')
test_data['tidy_summary'] = test_data['tidy_summary'].apply(lambda x: re.sub('  ', ' ', x))

# removing the word's with length less than 2
test_data['tidy_summary'] = test_data['tidy_summary'].apply(lambda x: ' '.join([w for w in x.split() if len(w)>2]))
test_data['tidy_summary'] = test_data['tidy_summary'].apply(lambda x: re.sub('  ', ' ', x))


import nltk
nltk.download('stopwords')
  
#stop_words = stopwords.words("english")
cachedStopWords = set(stopwords.words("english"))

# add custom words
cachedStopWords.update(('and','are','but','can','could','did','does','doing','for','from',
                        'had','has','have','having','here','into','its','itself',
                        'out','she','should','some','such','than','that',
                        'their','theirs','them','then','there','these','they','this','those','until',
                        'while','will','would','you','your','yours',
                        'with','our','you', 'all','new','such','day','one','the','for','with','and',
                        'from','are','can','could','does','but','has','have','was','into','onto',
                        'this','that','were','upon','may','you','been','wrt','should','our','goes',
                        'hello','your','isn','invcstinterfacejob','edyg','doesnt','val','bkx','also',
                        'shcjbj','xla','xlsx','koninklijk',
                        'january','jan','february','feb','march','mar','april','apr','may','june','jun',
                        'july','jul','august','aug','september',	'sep','sept',
                        'october','oct','november','nov','december','dec'))


test_data['tidy_summary'] = test_data['tidy_summary'].apply(lambda x: " ".join(x for x in x.split() if x not in cachedStopWords))


#
#from nltk.stem.wordnet import WordNetLemmatizer
#lemmatizer = WordNetLemmatizer()

#print(short_data['Step2_SentimentText'])
#print('-------Lemmazation--------')


#test_data['tidy_summary_1'] = test_data['tidy_summary'].apply(lambda x: ' '.join([lemmatizer.lemmatize(word) for word in x.split() ]))




max_len = 50

twt = tok.texts_to_sequences(test_data['tidy_summary'])
print(twt)

#padding the tweet to have exactly the same shape as `embedding_2` input
twt = pad_sequences(twt, maxlen= max_len, dtype='int32', value=0)
#print(twt)

predicted_label = model_test.predict(twt)

print(predicted_label)

test_labels = ['accrual-error', 'cogs-error', 'cost-capd-error', 'costing-item',
                'create-accounting-error', 'general-query', 'inventory-vr',
                'month-end-error', 'receipt-accounting', 'rollup-error',
                'standard-cost-error', 'tsr-order-error', 'work-order-error']


test_data["predit"] = ""


for i in range(len(predicted_label)):
    test_data["predit"][i] = test_labels[np.argmax(predicted_label[i])]



#test_data = test_data.assign(predicted_labels = y)

test_data.to_excel("label_output_file_1.xlsx",index =False)

