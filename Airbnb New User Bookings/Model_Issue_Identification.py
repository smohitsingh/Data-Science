# -*- coding: utf-8 -*-
"""
Created on Sat Dec 28 19:58:01 2019

@author: Mohit Singh
"""

#--------------------------- lOADING REQUIRED LIBRARIES -----------------------

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



#------------------------- lOADING REQUIRED TRAINING DATA ---------------------

# loading training data 
df = pd.read_excel (r'C:\Users\Mohit Singh\Downloads\purchase issue -dec21-v5-S.xlsx',sheet_name= "final") #(use "r" before the path string to address special character, such as '\'). Don't forget to put the file name at the end of the path + '.xlsx'

# 'Summary', 'Filters', 'Phase 1', 'Phase 2', 'Phase 3', 'Phase 4'
df.columns
 
# selecting important Columns only
final_data = df[['Summary','Phase 4']]


# renaming columnsfor easy understanding for future use 
final_data.columns = ["Summary_Data", "Issue_Label"]
final_data.columns   # ['Summary_Data', 'Issue_Label']

final_data.dtypes

print("Dimension of final_data data : {}".format(final_data.shape))
# Dimension of final_data data : (1662, 2)

# changing the data type to "string" data type
final_data["Summary_Data"] = final_data.Summary_Data.apply(str)
final_data["Issue_Label"] = final_data.Issue_Label.apply(str)

# checking the distribution of the labels & unique labels on our data.
final_data['Issue_Label'].value_counts()

np.unique(final_data["Issue_Label"])

# Removing the data where "Issue_Label" has null values as "nan"
final_data = final_data[final_data['Issue_Label']!="nan"]

print("Dimension of final_data data : {}".format(final_data.shape))

# Removing the data where "Summary_Data" has null values as "nan"
final_data = final_data[final_data['Summary_Data']!="nan"]

print("Dimension of final_data data : {}".format(final_data.shape))

final_data['Issue_Label'].value_counts()

# putting all lables to be removed in "remove_label", 
# for which the data to be removed for such labels
remove_label = ['interorg-transfer','date-error','overhead-error','PO-costing-error',
                'mapping-error','product-gross-margin','Drop ship-error','invalid-transaction','ess-error']

# now removing the data with the "remove_label"
final_data = final_data[~final_data.Issue_Label.isin(remove_label)]

print("Dimension of final_data data : {}".format(final_data.shape))

final_data['Issue_Label'].value_counts()

round(final_data['Issue_Label'].value_counts(normalize=True)*100,2)
##---- here we getting our final labels 

#general-query              297
#month-end-error            254
#inventory-vr               120
#cost-capd-error            109
#receipt-accounting         106
#costing-item               102
#create-accounting-error     91
#accrual-error               81
#rollup-error                73
#work-order-error            72
#tsr-order-error             62
#COGS-error                  59
#standard-cost-error         40


# "Org_summary_Length"  storing the number of words in each string summary.
final_data["Org_summary_Length"] = final_data["Summary_Data"].str.split().str.len() 


##--------------- data formatting
# Convert to lower
final_data.Issue_Label = final_data.Issue_Label.str.lower()
final_data['tidy_summary']= final_data.Summary_Data.str.lower()


#-------- Checking the distribution of "Issue_Label" --------------------------

plt.figure(figsize=(10,6))
sns.countplot(x = "Issue_Label", data = final_data)
plt.title("Dataset labels distribution")



#------------------------------- DATA CLEANING --------------------------------

# strip the blank spaces in the starting and end of the  "tidy_summary"
final_data['tidy_summary'] = final_data['tidy_summary'].apply(lambda x: x.strip())

# Removal of ASCII, Punctuation, special character, @user_id, hyperlinks, numbers. Special case not handled previously,      
final_data['tidy_summary'] = final_data['tidy_summary'].str.replace("(@[A-Za-z0-9]+)|([^0-9A-Za-z \t])|(\w+:\/\/\S+)|[\.\,\!\?\:\;\-\=]|\x92|[0-9]+|www.[^ ]+|[^\x00-\x7F]+|\d+", ' ')
final_data['tidy_summary'] = final_data['tidy_summary'].apply(lambda x: re.sub('  ',' ', x))
 
# remove https hyperlinks
final_data['tidy_summary'] = final_data['tidy_summary'].str.replace("http\S+|[^\x00-\x7F]+|\d+", '')
final_data['tidy_summary'] = final_data['tidy_summary'].apply(lambda x: re.sub('  ', ' ', x))

# removing the word's with length less than 2
final_data['tidy_summary'] = final_data['tidy_summary'].apply(lambda x: ' '.join([w for w in x.split() if len(w)>2]))
final_data['tidy_summary'] = final_data['tidy_summary'].apply(lambda x: re.sub('  ', ' ', x))

# final_data["summary_Length"]= final_data["tidy_summary"].str.len() 


# here we get the frequency of each word in our data and storing in "BOW_count_list"
# then using it to make our stopword dictionary
BOW_count_list = final_data.tidy_summary.str.split(expand=True).stack().value_counts()


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

final_data['tidy_summary'] = final_data['tidy_summary'].apply(lambda x: " ".join(x for x in x.split() if x not in cachedStopWords))


# "tidy_summary_Length"  storing the number of words in each string summary.
final_data["tidy_summary_Length"] = final_data["tidy_summary"].str.split().str.len() 



from nltk.stem.wordnet import WordNetLemmatizer
lemmatizer = WordNetLemmatizer()

final_data['tidy_summary_1'] = final_data['tidy_summary'].apply(lambda x: ' '.join([lemmatizer.lemmatize(word) for word in x.split() ]))


##  -----------------------------  TEXT ANALYSIS  -----------------------------

# understanding the word frequency in data,
# and then removing words with less meaning in making text context
# this is the bag of words in each summary

final_data['Bag_Of_Words'] = final_data.tidy_summary_1.str.strip().str.split('[\W_]+')
final_data.head()
    

BOW_count_list = final_data.tidy_summary_1.str.split(expand=True).stack().value_counts()

#BOW_count_list.to_excel("bow_stpw.xlsx",index =True)


# remove ing `


#final_data.to_excel("Final_Summary_Data.xlsx",index =False)



# putting all unique labels of data in "unique_lables"
unique_lables = np.unique(final_data["Issue_Label"])
print(unique_lables)

# encoding the labels of the data into integer type from 0 to 12 for our 13 labels.
final_data['Issue_Label'].replace('accrual-error',0,inplace=True)
final_data['Issue_Label'].replace('cogs-error',1,inplace=True)
final_data['Issue_Label'].replace('cost-capd-error',2,inplace=True)
final_data['Issue_Label'].replace('costing-item',3,inplace=True)
final_data['Issue_Label'].replace('create-accounting-error',4,inplace=True)
final_data['Issue_Label'].replace('general-query',5,inplace=True)
final_data['Issue_Label'].replace('inventory-vr',6,inplace=True)
final_data['Issue_Label'].replace('month-end-error',7,inplace=True)
final_data['Issue_Label'].replace('receipt-accounting',8,inplace=True)
final_data['Issue_Label'].replace('rollup-error',9,inplace=True)
final_data['Issue_Label'].replace('standard-cost-error',10,inplace=True)
final_data['Issue_Label'].replace('tsr-order-error',11,inplace=True)
final_data['Issue_Label'].replace('work-order-error',12,inplace=True)


# "label" has all labels of the data
label= final_data['Issue_Label'].values

# converting the lables into categorical form for the model building.
from keras.utils import np_utils
Y = np_utils.to_categorical(label)
print(Y)

# defining the parameters of model.
max_len= 50
max_features= 20000

# creating vectorized corpus and padding
tokenizer = Tokenizer(num_words= max_features)

tokenizer.fit_on_texts(final_data['tidy_summary_1'].values)

# storing the "tokenizer" in the system for the future use at different system
#with open('C:\\Users\\Mohit Singh\\Downloads\\tokenizer_1.pickle', 'wb') as handle:
#    pickle.dump(tokenizer, handle, protocol=pickle.HIGHEST_PROTOCOL)                


X = pad_sequences(tokenizer.texts_to_sequences(final_data['tidy_summary_1'].values), maxlen = max_len)

# train test split
X_train, X_test, y_train, y_test = train_test_split(X,Y, test_size=0.3, random_state=22, stratify=Y)

model= Sequential()
model.add(Embedding(max_features,100,mask_zero=True))
model.add(LSTM(200,dropout=0.3,recurrent_dropout=0.3,return_sequences=False))
model.add(Dense(13, activation='softmax'))
model.compile(loss = 'categorical_crossentropy', optimizer='adam',metrics = ['accuracy'])
model.summary()


callback = [EarlyStopping(monitor='val_loss', patience=2), ModelCheckpoint(filepath='C:\\Users\\Mohit Singh\\Downloads\\BestModel_01.h5', monitor='val_loss', save_best_only=True)]
baseline_history = model.fit(X_train, y_train, batch_size= 8, epochs= 15, callbacks=callback ,validation_data=(X_test, y_test))

accr = model.evaluate(X_test,y_test)

print('Test set\n Loss: {:0.3f}\n Accuracy: {:0.3f}'.format(accr[0],accr[1]))


##--  Test set 
#      Loss: 1.416
#      Accuracy: 0.632


##-----------------------------------------------------------------------------

##-----  Checking accuracy & loss in both train and test datasets.

plt.title('Loss')
plt.plot(baseline_history.history['loss'], label='train')
plt.plot(baseline_history.history['val_loss'], label='test')
plt.legend()
plt.show();


plt.title('Accuracy')
plt.plot(baseline_history.history['acc'], label='train')
plt.plot(baseline_history.history['val_acc'], label='test')
plt.legend()
plt.show();



##---------------------------- PROBLEM PREDICTION -----------------------------

#
## loading the best model saved & tokenizer
#model_test = load_model(r'C:\Users\Mohit Singh\Downloads\BestModel_01.h5')
#with open(r'C:\Users\Mohit Singh\Downloads\tokenizer_1.pickle', 'rb') as handle:
#    tok = pickle.load(handle)
#
#
###---- Now check for the given below labels in the Summary data.
#
#print(unique_lables)
## Below are 13 labels on which the Case study of the Data is done.
##'accrual-error' 
##'cogs-error' 
##'cost-capd-error' 
##'costing-item'
##'create-accounting-error'
##'general-query' 
##'inventory-vr'
##'month-end-error' 
##'receipt-accounting'
##'rollup-error'
##'standard-cost-error' 
##'tsr-order-error' 
##'work-order-error'
#
#
#twt = ["Wrong Receipt Accounting captured"]
#
#twt = tok.texts_to_sequences(twt)
#print(twt)
#
##padding the tweet to have exactly the same shape as `embedding_2` input
#twt = pad_sequences(twt, maxlen= max_len, dtype='int32', value=0)
##print(twt)
#
#predicted_label = model_test.predict(twt)
##print(predicted_label)
#
#print(unique_lables[np.argmax(predicted_label)])
#
