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
df = pd.read_excel (r'F:\Summary_data\purchase issue -dec21-v5-S.xlsx',sheet_name= "final") #(use "r" before the path string to address special character, such as '\'). Don't forget to put the file name at the end of the path + '.xlsx'

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
final_data = final_data.astype(str)

final_data["Issue_Label"] = final_data["Issue_Label"].str.strip()
final_data["Summary_Data"] = final_data["Summary_Data"].str.strip()


final_data["Summary_Data"] = final_data["Summary_Data"].str.lower()
final_data["Issue_Label"] = final_data["Issue_Label"].str.lower()


# checking the distribution of the labels & unique labels on our data.
final_data['Issue_Label'].value_counts()

np.unique(final_data["Issue_Label"])


# Removing the data where "Issue_Label" has null values as "nan"
final_data = final_data[final_data['Issue_Label']!="nan"]

print("Dimension of final_data data : {}".format(final_data.shape))


# Removing the data where "Summary_Data" has null values as "nan"
final_data = final_data[final_data['Summary_Data']!="nan"]

print("Dimension of final_data data : {}".format(final_data.shape))


label_count = final_data['Issue_Label'].value_counts().reset_index()
label_count.columns = ["issue_labels","count"]

label_count_1 = label_count[label_count["count"]>=20]


# putting all lables to be removed in "remove_label", 
# for which the data to be removed for such labels
#remove_label = ['interorg-transfer','date-error','overhead-error','PO-costing-error',
#                'mapping-error','product-gross-margin','Drop ship-error','invalid-transaction','ess-error']


# now removing the data with the "remove_label"
final_data = final_data[final_data.Issue_Label.isin(label_count_1["issue_labels"])]

print("Dimension of final_data data : {}".format(final_data.shape))

final_data['Issue_Label'].value_counts()

round(final_data['Issue_Label'].value_counts(normalize=True)*100,2)


# "Org_summary_Length"  storing the number of words in each string summary.
final_data["Org_summary_Length"] = final_data["Summary_Data"].str.split().str.len() 


#-------- Checking the distribution of "Issue_Label" --------------------------

plt.figure(figsize=(10,6))
sns.countplot(x = "Issue_Label", data = final_data)
plt.title("Dataset labels distribution")



#------------------------------- DATA CLEANING --------------------------------

# strip the blank spaces in the starting and end of the  "tidy_summary"


# Removal of ASCII, Punctuation, special character, @user_id, hyperlinks, numbers. Special case not handled previously,      
final_data['tidy_summary'] = final_data['Summary_Data'].str.replace("(@[A-Za-z0-9]+)|([^0-9A-Za-z \t])|(\w+:\/\/\S+)|[\.\,\!\?\:\;\-\=]|\x92|[0-9]+|www.[^ ]+|[^\x00-\x7F]+|\d+", ' ')
final_data['tidy_summary'] = final_data['tidy_summary'].apply(lambda x: re.sub('  ',' ', x))
 
# remove https hyperlinks
final_data['tidy_summary'] = final_data['tidy_summary'].str.replace("http\S+|[^\x00-\x7F]+|\d+", '')
final_data['tidy_summary'] = final_data['tidy_summary'].apply(lambda x: re.sub('  ', ' ', x))

# removing the word's with length less than 2
final_data['tidy_summary'] = final_data['tidy_summary'].apply(lambda x: ' '.join([w for w in x.split() if len(w)>=2]))
final_data['tidy_summary'] = final_data['tidy_summary'].apply(lambda x: re.sub('  ', ' ', x))

# final_data["summary_Length"]= final_data["tidy_summary"].str.len() 


final_data = final_data[final_data['tidy_summary']!= ""]



#negations_dic = {
#        "ain't":"is not","amn't":"am not","aren't":"are not","'cause":"because",        "couldn't":"could not","couldn't've":"could not have","could've":"could have","daren't":"dare not","daresn't":"dare not",
#        "dasn't":"dare not","didn't":"did not","doesn't":"does not","don't":"do not","e'er":"ever",        "em":"them","everyone's":"everyone is","finna":"fixing to","gimme":"give me",
#        "gonna":"going to","gon't":"go not","gotta":"got to","hadn't":"had not","hasn't":"has not",        "haven't":"have not","he'd":"he would","he'll":"he will","he's":"he is","he've":"he have","how'd":"how would",
#        "how'll":"how will","how're":"how are","how's":"how is","i'd":"i would",        "i'll":"i will","i'm":"i am","i'm'a":"i am about to","i'm'o":"i am going to",
#        "isn't":"is not","it'd":"it would","it'll":"it will","it's":"it is","its":"it is","i've":"i have",        "kinda":"kind of","let's":"let us","mayn't":"may not","may've":"may have","mightn't":"might not",
#        "might've":"might have","mustn't":"must not","mustn't've":"must not have",        "must've":"must have","needn't":"need not","needn":"need not","ne'er":"never","o'":"of",
#        "o'er":"over","ol'":"old","oughtn't":"ought not","shalln't":"shall not","shan't":"shall not",        "she'd":"she would","she'll":"she will","she's":"she is","shouldn't":"should not","shouldn":"should not",
#        "shouldn't've":"should not have","should've":"should have","somebody's":"somebody is",        "someone's":"someone is","something's":"something is","that'd":"that would","that'll":"that will",
#        "that're":"that are","that's":"that is","there'd":"there would",        "there'll":"there will","there're":"there are","there's":"there is",
#        "these're":"these are","they'd":"they would","they'll":"they will","they're":"they are",        "they've":"they have","this's":"this is","those're":"those are","'tis":"it is",
#        "'its":"it is","'twas":"it was","wanna":"want to","wasn't":"was not",        "wasn":"was not","we'd":"we would","we'd've":"we would have","we'll":"we will","we're":"we are","weren't":"were not",
#        "we've":"we have","what'd":"what did","what'll":"what will","what're":"what are",        "what's":"what is","what've":"what have","when's":"when is","where'd":"where did",
#        "where're":"where are","where's":"where is","where've":"where have","which's":"which is",        "who'd":"who would","who'd've":"who would have","who'll":"who will",
#        "who're":"who are","who's":"who is","who've":"who have","why'd":"why did",        "why're":"why are","why's":"why is","won't":"will not","wouldn't":"would not",
#        "would've":"would have","y'all":"you all","you'd":"you would","you'll":"you will",        "you're":"you are","you've":"you have","Whatcha":"What are you","luv":"love","sux":"sucks","isn't":"is not"
#}   
#
#
#neg_pattern = re.compile(r'\b(' + '|'.join(negations_dic.keys()) + r')\b') 
#
#clean_tweets = []
#for t in final_data['tidy_summary']:
##    print(t)
#    t = neg_pattern.sub(lambda x: negations_dic[x.group()], t)
#    clean_tweets.extend(t,t)
#        


#
##clean_tweets
#final_data['tidy_summary_1'] = pd.DataFrame(clean_tweets,columns = ['tidy_tweet'])
#
#final_data.shape


# here we get the frequency of each word in our data and storing in "BOW_count_list"
# then using it to make our stopword dictionary
#BOW_count_list = final_data.tidy_summary.str.split(expand=True).stack().value_counts()


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


#
#from nltk.stem.wordnet import WordNetLemmatizer
#lemmatizer = WordNetLemmatizer()
#
#final_data['tidy_summary_1'] = final_data['tidy_summary'].apply(lambda x: ' '.join([lemmatizer.lemmatize(word) for word in x.split() ]))


##  -----------------------------  TEXT ANALYSIS  -----------------------------

# understanding the word frequency in data,
# and then removing words with less meaning in making text context
# this is the bag of words in each summary

final_data['Bag_Of_Words'] = final_data.tidy_summary.str.strip().str.split('[\W_]+')
final_data.head()
    

#BOW_count_list = final_data.tidy_summary_1.str.split(expand=True).stack().value_counts()

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

tokenizer.fit_on_texts(final_data['tidy_summary'].values)

# storing the "tokenizer" in the system for the future use at different system
#with open('C:\\Users\\Mohit Singh\\Downloads\\tokenizer_1.pickle', 'wb') as handle:
#    pickle.dump(tokenizer, handle, protocol=pickle.HIGHEST_PROTOCOL)                


X = pad_sequences(tokenizer.texts_to_sequences(final_data['tidy_summary'].values), maxlen = max_len)

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
