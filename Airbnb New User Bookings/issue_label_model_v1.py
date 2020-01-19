# -*- coding: utf-8 -*-
"""
Created on Sat Jan 18 00:21:05 2020

@author: Mohit Singh
"""

# -*- coding: utf-8 -*-
"""
Created on Thu Jan 16 09:16:22 2020

@author: MS00612816
"""

#combine all data
#first two lines of chats 
#combine these two lines

#data cleaning :
#stop words / neg. word dictionary

#manually labels
#model building LSTM
#correct words dict.




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
from collections import defaultdict




#------------------------------------  LOADING DATASETS

##----- Loading datset 1 

DATASET_ENCODING = "ISO-8859-1"
#
#filename_1 = r'D:\sentiment analysis\Customer_Chat_Model\Chat_Data\12K chat lines_Dual Tagged_Oct_2019.xlsx'
#chat_Data_1 = pd.read_excel(filename_1,sheet_name='Sheet1',encoding = DATASET_ENCODING)
#chat_Data_1.info()
#chat_Data_1.columns
#
#for i in ['Srno.', 'Department', 'Date', 'Time',
#       'Final tagging', 'Chat polarity']: del chat_Data_1[i]
#
#chat_Data_1.columns 
#
#chat_Data_1.columns = ['Chat_ID', 'Name', 'Text']
#
#chat_Data_1['Chat_ID'] = chat_Data_1['Chat_ID'].astype('Int64')
#
#
###----- Loading datset 2
#filename_2 = r"D:\sentiment analysis\Customer_Chat_Model\Chat_Data\chat_dataAnalysis.xlsx"
#chat_Data_2 = pd.read_excel(filename_2,sheet_name='Sheet1',encoding = DATASET_ENCODING)
#chat_Data_2.info()
#chat_Data_2.columns
#
#for i in ['sr. no.', 'sentiment']: del chat_Data_2[i]
#
#chat_Data_2.columns 
#
#chat_Data_2.columns = ['Chat_ID', 'Name', 'Text']
#
#
###----- Loading datset 3
#filename_3 = r"D:\sentiment analysis\Customer_Chat_Model\Chat_Data\chat_pol1.xlsx"
#chat_Data_3 = pd.read_excel(filename_3,sheet_name='Sheet1',encoding = DATASET_ENCODING)
#chat_Data_3.info()
#chat_Data_3.columns
#
#for i in ['Srno.', 'Linewise tagging','Chat_polarity']: del chat_Data_3[i]
#
#chat_Data_3.columns 
#
#chat_Data_3.columns = ['Chat_ID', 'Name', 'Text']
#
#
###----- Loading datset 4
#filename_4 = r"D:\sentiment analysis\Customer_Chat_Model\Chat_Data\chat_pol2.xlsx"
#chat_Data_4 = pd.read_excel(filename_4,sheet_name='Sheet1',encoding = DATASET_ENCODING)
#chat_Data_4.info()
#chat_Data_4.columns
#
#for i in ['Srno.', 'Department', 'Date', 'Time',
#       'Final tagging', 'Chat_polarity']: del chat_Data_4[i]
#
#chat_Data_4.columns 
#
#chat_Data_4.columns = ['Chat_ID', 'Name', 'Text']
#
#chat_Data_4['Chat_ID'] = chat_Data_4['Chat_ID'].astype('Int64')
#
###----- Loading datset 5
#filename_5 = r"D:\sentiment analysis\Customer_Chat_Model\Chat_Data\chat_pol3.xlsx"
#chat_Data_5 = pd.read_excel(filename_5,sheet_name='Sheet1',encoding = DATASET_ENCODING)
#chat_Data_5.info()
#chat_Data_5.columns
#
#for i in [ 'Department', 'Final linewise tag','Chat_polarity']: del chat_Data_5[i]
#
#chat_Data_5.columns 
#
#chat_Data_5.columns = ['Chat_ID', 'Name', 'Text']
#
#
###----- Loading datset 6
#filename_6 = r"D:\sentiment analysis\Customer_Chat_Model\Chat_Data\testing.xlsx"
#chat_Data_6 = pd.read_excel(filename_6,sheet_name='Sheet1',encoding = DATASET_ENCODING)
#chat_Data_6.info()
#chat_Data_6.columns
#
#for i in ['Department', 'Date', 'Time']: del chat_Data_6[i]
#
#chat_Data_6.columns 
#chat_Data_6.columns = ['Chat_ID', 'Name', 'Text']
#
#
###----- Loading datset 7
#filename_7 = r"D:\sentiment analysis\Customer_Chat_Model\Chat_Data\testing_sample_25_Neutral_chat_polarities.xlsx"
#chat_Data_7 = pd.read_excel(filename_7,sheet_name='Sheet1',encoding = DATASET_ENCODING)
#chat_Data_7.info()
#chat_Data_7.columns
#
#for i in ['Department','Date', 'Time',
#       'Line wise polarity', 'Chat wise polarity']: del chat_Data_7[i]
#
#chat_Data_7.columns 
#
#chat_Data_7.columns = ['Chat_ID', 'Name', 'Text']
#
#
###----- Loading datset 8
#filename_8 = r"D:\sentiment analysis\Customer_Chat_Model\Chat_Data\testing_Sample_200_chats.xlsx"
#chat_Data_8 = pd.read_excel(filename_8,sheet_name='Sheet1',encoding = DATASET_ENCODING)
#chat_Data_8.info()
#chat_Data_8.columns
#
#for i in [ 'Department',  'Date', 'Time','Line wise polarity', 'Chat wise polarity']: del chat_Data_8[i]
#
#chat_Data_8.columns 
#
#chat_Data_8.columns = ['Chat_ID', 'Name', 'Text']
#
#
###----- Loading datset 9
#filename_9 = r"D:\sentiment analysis\Customer_Chat_Model\Chat_Data\testing_sample_1000_chat_sessions.xlsx"
#chat_Data_9 = pd.read_excel(filename_9,sheet_name='Sheet1',encoding = DATASET_ENCODING)
#chat_Data_9.info()
#chat_Data_9.columns
#
#for i in ['Department', 'Date', 'Time', 'Linewise tag',
#       'Chatwise tag']: del chat_Data_9[i]
#
#chat_Data_9.columns 
#
#chat_Data_9.columns = ['Chat_ID', 'Name', 'Text']
#
#
#
#
###------------  MERGING DATA 
#combine_chat = pd.concat([chat_Data_1,chat_Data_2,chat_Data_3,
#                          chat_Data_4,chat_Data_5,chat_Data_6,
#                          chat_Data_7,chat_Data_8,chat_Data_9])



DATASET_ENCODING = "ISO-8859-1"

filename = r"F:\Chat_data_model\combine_chat.xlsx"
combine_chat = pd.read_excel(filename,sheet_name='combine_chat',encoding = DATASET_ENCODING)
combine_chat.columns 
combine_chat.info()

#combine_chat['tidy_text'] =  final_chat_data['Text'].str.lower()

combine_chat.Chat_ID

combine_chat['Chat_ID'] = combine_chat['Chat_ID'].fillna(0).astype(np.int64)

combine_chat = combine_chat.drop_duplicates()

combine_chat = combine_chat[combine_chat['Chat_ID']!=0] 

print(combine_chat.columns)
print("Dimension of combine_chat is now : {}".format(combine_chat.shape))

combine_chat = combine_chat[combine_chat["Name"]=="Customer"]

combine_chat.drop('Name', axis=1, inplace=True)

combine_chat = combine_chat.drop_duplicates()
print("Dimension of final_data data : {}".format(combine_chat.shape))

#combine_chat['Text'] = combine_chat['Text'].str.split('question:').str[1]
#combine_chat['Text'] = combine_chat['Text'].str.split('chat url is ok').str[0]


fetch_first_two = combine_chat.groupby('Chat_ID').head(2)

fetch_first_two["Text"] = fetch_first_two.Text.apply(str)

fetch_first_two['Text'] =  fetch_first_two['Text'].str.lower()

fetch_first_two = fetch_first_two.reset_index(drop = True)

# strip the blank spaces in the starting and end of the  "tidy_summary"
fetch_first_two['Text'] = fetch_first_two['Text'].apply(lambda x: x.strip())


fetch_first_two['pattern_1']  = fetch_first_two['Text'][['question:' in x for x in fetch_first_two['Text']]].str.split('question:').str[1]
fetch_first_two['pattern_1'] = fetch_first_two['pattern_1'].astype(str)

fetch_first_two['pattern_1'][fetch_first_two['pattern_1']=='nan'] = fetch_first_two['Text'][fetch_first_two['pattern_1']=='nan']

fetch_first_two['tidy_text']  = fetch_first_two['pattern_1'][['chat url is ok' in x for x in fetch_first_two['pattern_1']]].str.split('chat url is ok').str[0]
fetch_first_two['tidy_text'] = fetch_first_two['tidy_text'].astype(str)

fetch_first_two['tidy_text'][fetch_first_two['tidy_text']=='nan'] = fetch_first_two['pattern_1'][fetch_first_two['tidy_text']=='nan']

fetch_first_two = fetch_first_two[fetch_first_two['tidy_text']!= ' '] 

#fetch_first_two = fetch_first_two.groupby('Chat_ID').head(2)

fetch_first_two["tidy_text"] = fetch_first_two.tidy_text.apply(str)

fetch_first_two['tidy_text'] = fetch_first_two['tidy_text'].apply(lambda x: x.strip())


p = fetch_first_two.groupby(['Chat_ID']).apply(lambda x: "%s" % ' '.join(x.tidy_text))

final_chat_data = pd.DataFrame(p,columns=['tidy_text']).reset_index(inplace = False)

final_chat_data.Chat_ID.nunique()
final_chat_data.columns


final_chat_data['tidy_text'] = final_chat_data['tidy_text'].apply(lambda x: re.sub('@[\w]*', '', str(x).lower()))
final_chat_data['tidy_text'] = final_chat_data['tidy_text'].apply(lambda x: re.sub('  ', ' ', x))

# remove https hyperlinks
final_chat_data['tidy_text'] = final_chat_data['tidy_text'].str.replace("http\S+|[^\x00-\x7F]+|\d+", '')
final_chat_data['tidy_text'] = final_chat_data['tidy_text'].apply(lambda x: re.sub('  ', ' ', x))

# Removal of ASCII, Punctuation, special character, @user_id, hyperlinks, numbers. Special case not handled previously,      
final_chat_data['tidy_text'] = final_chat_data['tidy_text'].str.replace("(@[A-Za-z0-9]+)|([^0-9A-Za-z \t])|(\w+:\/\/\S+)|[\.\,\!\?\:\;\-\=]|\x92|[0-9]+|www.[^ ]+|[^\x00-\x7F]+|\d+", ' ')
final_chat_data['tidy_text'] = final_chat_data['tidy_text'].apply(lambda x: re.sub('  ', ' ', x))

## removing the word's with length less than 2
final_chat_data['tidy_text'] = final_chat_data['tidy_text'].apply(lambda x: ' '.join([w for w in x.split() if len(w)>2]))
final_chat_data['tidy_text'] = final_chat_data['tidy_text'].apply(lambda x: re.sub('  ', ' ', x))

final_chat_data = final_chat_data[final_chat_data['tidy_text'] != ""]


final_chat_data['tidy_text'] = final_chat_data['tidy_text'].apply(lambda x: x.strip())

final_chat_data['Bag_Of_Words'] = final_chat_data.tidy_text.str.strip().str.split('[\W_]+')




#
## then using it to make our stopword dictionary
#BOW_count_list = final_chat_data.tidy_text.str.split(expand=True).stack().value_counts()
#
#pd.DataFrame(BOW_count_list).to_excel('BOW_count_list_1.xlsx', 'BOW_count_list')



#---------------  combining both custon and pre-stopwords list
# 
#import pandas as pd
#from nltk.corpus import stopwords
##stop_words = set(stopwords.words('english'))
# 
#new_stopwords = set(stopwords.words('english')) - {'against'}
# 
#stopwords_filename = r"F:\Chat_data_model\BOW_count_list.xlsx"
#custom_stopwords = pd.read_excel(stopwords_filename,sheet_name='stop_words',encoding = DATASET_ENCODING)
#


##=============================================================================


#
## union of two sets
#stop_words = new_stopwords | set(custom_stopwords['stopword'])
#

#print(combine_chat.columns)
#print("Dimension of combine_chat is now : {}".format(combine_chat.shape))
#combine_chat = combine_chat[combine_chat["Name"]=="Customer"]
#combine_chat.drop('Name', axis=1, inplace=True)
#combine_chat = combine_chat.drop_duplicates()
#print("Dimension of final_data data : {}".format(combine_chat.shape))
#fetch_first_two = fetch_first_two.reset_index(drop = True)
#
#
#
#combine_chat['Text'] = combine_chat['Text'].str.split('question:').str[1]
#combine_chat['Text'] = combine_chat['Text'].str.split('chat url is ok').str[0]
#
#
#fetch_first_two = combine_chat.groupby('Chat_ID').head(2)
#
#fetch_first_two["Text"] = fetch_first_two.Text.apply(str)
#
#
#p = fetch_first_two.groupby(['Chat_ID']).apply(lambda x: "%s" % ' '.join(x.Text))
#
#final_chat_data = pd.DataFrame(p,columns=['Text']).reset_index(inplace = True)
#
##final_chat_data.Chat_ID.unique()
#final_chat_data.Chat_ID.nunique()
#
#final_chat_data['tidy_text'] =  final_chat_data['Text'].str.lower()
#

#final_chat_data['tidy_text'] = final_chat_data['tidy_text'].str.split('question:').str[1]
#final_chat_data['tidy_text'] = final_chat_data['tidy_text'].str.split('chat url is ok').str[0]
#
#
#final_chat_data['tidy_text'] = final_chat_data['tidy_text'].apply(lambda x: x.str.split('question:').str[1]) if )

#p = fetch_first_two.groupby(['Chat_ID']).apply(lambda x: "%s" % ' '.join(x.Text))
#final_chat_data = pd.DataFrame(p,columns=['Text']).reset_index()


#==============================================================================


# find a pattern 
# take their index values
#
#combine_chat['pattern'] =  combine_chat['Text'].apply(lambda x: "true" if re.findall("question:", x) == "question:" else "false" )
#
#combine_chat['Te_1xt'] = combine_chat['Text'].str.split('question:').str[1]
#
#combine_chat['pattern'] =  combine_chat['Text'].apply(lambda x: "true" if re.findall("question:", x) == "question:" else "false" )

s
#final_chat_data['tidy_text'] = final_chat_data['tidy_text'].apply(lambda x: x.str.split('question:').str[1]) if )
#


#==============================================================================


#combine_chat['Chat_ID'] = combine_chat['Chat_ID'].astype('Int64')
#
#sample = pd.DataFrame({'text':["Three Number: 07427934580firstname: Mareklastname: Pollakpostcode: Sy111rrDate Of Birth: 05/05/1982Initial Question: I have canceled this number ",
#"Three Number: 07455977874firstname: Kylelastname: Phillipspostcode: SA15 3RLDate Of Birth: 12/02/1991Initial Question: Just trying to find out if iv been cut off and when my next bill is due please",
#"Three Number: 07429373438firstname: Gavinlastname: Matherspostcode: AL10 9NYDate Of Birth: 03/04/1991Initial Question: Ive just joined but have concerns",
#"Three Number: 07599397728firstname: Caralastname: Murraypostcode: G46 7RZDate Of Birth: 13/04/81Initial Question: Difficulties keeping up with my monthly paymentsCHAT URL IS OK (Three.co.uk verified domain): https://www.three.co.uk/support/contact-us#bill",
#"Three Number: 07378270788firstname: Andradalastname: Denisapostcode: eh14 2bnDate Of Birth: 20/10/1998Initial Question: Hello!CHAT URL IS OK (Three.co.uk verified domain): http://support.three.co.uk/SRVS/CGI-BIN/WEBISAPI.dll/,/?New,Kb&#61;Mobile,Ts&#61;Mobile,T&#61;CaseDoc,Case&#61;Obj(42783)",
#"sdvdfvdfk",
#"sdvdfvdfk"]})
#
#
#sample['text'] =  sample['text'].str.lower()
#sample['text'] = sample['text'].astype(str)
#
#sample = sample.drop_duplicates()



#sample['pattern_1']  = sample['text'][['question:' in x for x in sample['text']]].str.split('question:').str[1]
#sample['pattern_1'] = sample['pattern_1'].astype(str)
#
#
#sample['pattern_1'][sample['pattern_1']=='nan'] = sample['text'][sample['pattern_1']=='nan']
#
#sample['tidy_text']  = sample['pattern_1'][['chat url is ok' in x for x in sample['pattern_1']]].str.split('chat url is ok').str[0]
#sample['tidy_text'] = sample['tidy_text'].astype(str)
#
#sample['tidy_text'][sample['tidy_text']=='nan'] = sample['pattern_1'][sample['tidy_text']=='nan']


#sample['tidy_text'] = sample["text"][sample['pattern_1'] =="nan"]
#sample['tidy_text'] = sample["pattern_2"][sample['tidy_text'] =="nan"]
#
#for row in sample:
#    if(('question:' in row[0])== True):
#        print(row)
#
#sample['pattern'] = sample['pattern'].astype(str)
#sample.info()
#sample['tidy_text'] = sample['text'][sample['pattern'] == True].str.split('question:').str[1]
#sample['tidy_text'] = sample['text'][sample['pattern'] == False]
#
##sample['tidy_text'] = ""
#if('question:' in sample['text']):
#    sample['tidy_text_1'] = sample['text'].str.split('question:').str[1]
#
#if( sample['tidy_text_1'] =='nan' ):    
#    sample['tidy_text_2'] = sample['text']    
    
#if(sample['pattern']==False):
#    sample['tidy_text'] = sample['text']
#else:
#    sample['tidy_text'] = sample['text'].str.split('question:').str[1]    



#
#if (sample['text'].str.contains('chat url is ok', regex=False)):
#    sample['tidy_text'] = sample['text'].str.split('chat url is ok').str[0]
#else:    
#    sample['tidy_text'] = sample['text']




#
##sample['text_11']  = sample.apply(lambda x: sample['text'] if 'question:' in sample['text'] else sample['text'].str.split('question:').str[1])
#
#
#sample['text_11'] = sample.text.apply(lambda x: sample['text'].split('question:').str[0] if sample['text'].contains('question:', regex=False)==True else x)
#
#
#for row in sample['text']:
#    if 'question:' in row:
#     print(1)
#    
#    
#
#'question:' in x[x]
#
#sample['tidy_text'] = sample['text'][sample['pattern'] == True].str.split('question:').str[1]
#
#sample['tidy_text'] = sample['tidy_text'] 
#
#
#sample['tidy_text'] = sample['text'].str.split('question:').str[1]
#
##sample['pattern'] =  sample['text'].apply(lambda x: 1 if x.==True else 0)
#
#
#sample['tidy_text'] = sample["text"][sample['text'].str.contains('question:', regex=False)].str.split('question:').str[1]
#
#
#sample['tidy_text'] = sample['tidy_text'].str.split('chat url is ok').str[0]
#
#x = sample[['question:' in x for x in sample['text']]]
#
#
#
#
###result = sample.str.extract(pat = '([Question:].)') 
##
##sample['text'].rpartition("Question:")
#
##sample['tidy_text']  = y
##sample.text.str.replace('chat url is ok ', 'XX', case=False)
##sample.text.replace(to_replace=r'Three Number:', value='XX', regex=True)
##sample['tidy_text']  = sample['text'].str.replace(r'\([^Number:]*\)', '').str.strip()
##sample['tidy_text']  = sample['text'].str.extract(r'Question:\s*([^\CHAT]*)\s*\.', expand=False)
#
#
#print(combine_chat.columns)
#print("Dimension of combine_chat is now : {}".format(combine_chat.shape))
#
#combine_chat = combine_chat[combine_chat["Name"]=="Customer"]
#
#combine_chat.drop('Name', axis=1, inplace=True)
#
#combine_chat = combine_chat.drop_duplicates()
#print("Dimension of final_data data : {}".format(combine_chat.shape))
#
#
#combine_chat['Text'] = combine_chat['Text'].str.split('question:').str[1]
#combine_chat['Text'] = combine_chat['Text'].str.split('chat url is ok').str[0]
#
#
#fetch_first_two = combine_chat.groupby('Chat_ID').head(2)
#
#fetch_first_two["Text"] = fetch_first_two.Text.apply(str)
#
#
#p = fetch_first_two.groupby(['Chat_ID']).apply(lambda x: "%s" % ' '.join(x.Text))
#
#final_chat_data = pd.DataFrame(p,columns=['Text']).reset_index()
#
##final_chat_data.Chat_ID.unique()
#final_chat_data.Chat_ID.nunique()
#
#final_chat_data['tidy_text'] =  final_chat_data['Text'].str.lower()
#
##
##final_chat_data['tidy_text'] = final_chat_data['tidy_text'].str.split('question:').str[1]
##final_chat_data['tidy_text'] = final_chat_data['tidy_text'].str.split('chat url is ok').str[0]
##
##
##final_chat_data['tidy_text'] = final_chat_data['tidy_text'].apply(lambda x: x.str.split('question:').str[1]) if )
##
#
##
##
##
##
#
#negations_dic = {
#        "ain't":"is not","amn't":"am not","aren't":"are not","'cause":"because",
#        "couldn't":"could not","couldn't've":"could not have","could've":"could have","daren't":"dare not","daresn't":"dare not",
#        "dasn't":"dare not","didn't":"did not","doesn't":"does not","don't":"do not","e'er":"ever",
#        "em":"them","everyone's":"everyone is","finna":"fixing to","gimme":"give me",
#        "gonna":"going to","gon't":"go not","gotta":"got to","hadn't":"had not","hasn't":"has not",
#        "haven't":"have not","he'd":"he would","he'll":"he will","he's":"he is","he've":"he have","how'd":"how would",
#        "how'll":"how will","how're":"how are","how's":"how is","i'd":"i would",
#        "i'll":"i will","i'm":"i am","i'm'a":"i am about to","i'm'o":"i am going to",
#        "isn't":"is not","it'd":"it would","it'll":"it will","it's":"it is","its":"it is","i've":"i have",
#        "kinda":"kind of","let's":"let us","mayn't":"may not","may've":"may have","mightn't":"might not",
#        "might've":"might have","mustn't":"must not","mustn't've":"must not have",
#        "must've":"must have","needn't":"need not","needn":"need not","ne'er":"never","o'":"of",
#        "o'er":"over","ol'":"old","oughtn't":"ought not","shalln't":"shall not","shan't":"shall not",
#        "she'd":"she would","she'll":"she will","she's":"she is","shouldn't":"should not","shouldn":"should not",
#        "shouldn't've":"should not have","should've":"should have","somebody's":"somebody is",
#        "someone's":"someone is","something's":"something is","that'd":"that would","that'll":"that will",
#        "that're":"that are","that's":"that is","there'd":"there would",
#        "there'll":"there will","there're":"there are","there's":"there is",
#        "these're":"these are","they'd":"they would","they'll":"they will","they're":"they are",
#        "they've":"they have","this's":"this is","those're":"those are","'tis":"it is",
#        "'its":"it is","'twas":"it was","wanna":"want to","wasn't":"was not",
#        "wasn":"was not","we'd":"we would","we'd've":"we would have","we'll":"we will","we're":"we are","weren't":"were not",
#        "we've":"we have","what'd":"what did","what'll":"what will","what're":"what are",
#        "what's":"what is","what've":"what have","when's":"when is","where'd":"where did",
#        "where're":"where are","where's":"where is","where've":"where have","which's":"which is",
#        "who'd":"who would","who'd've":"who would have","who'll":"who will",
#        "who're":"who are","who's":"who is","who've":"who have","why'd":"why did",
#        "why're":"why are","why's":"why is","won't":"will not","wouldn't":"would not",
#        "would've":"would have","y'all":"you all","you'd":"you would","you'll":"you will",
#        "you're":"you are","you've":"you have","Whatcha":"What are you","luv":"love","sux":"sucks","isn't":"is not", 
#        "aren't":"are not", "wasn't":"was not", "weren't":"were not","haven't":"have not","hasn't":"has not",
#        "hadn't":"had not","wouldn't":"would not", "don't":"do not","doesn't":"does not","didn't":"did not","can't":"cannot",
#        "can not":"cannot","couldn't":"could not","shouldn't":"should not",
#        "mightn't":"might not","mustn't":"must not","wouldn":"would not","won't":"would not","weren":"were not"
#        }
#
#
#
#neg_pattern = re.compile(r'\b(' + '|'.join(negations_dic.keys()) + r')\b') 
#
#clean_issues = []
#for t in final_chat_data['tidy_text']:
#    t = neg_pattern.sub(lambda x: negations_dic[x.group()], t)
#    clean_issues.append(t)
#
#
## Putting the clean tweets in separate column "tidy_text" 
## clean_issues
#final_chat_data['tidy_text'] = pd.DataFrame(clean_issues,columns = ['tidy_text'])
#
#
#final_chat_data['tidy_text'] = final_chat_data['tidy_text'].apply(lambda x: re.sub('@[\w]*', '', str(x).lower()))
#final_chat_data['tidy_text'] = final_chat_data['tidy_text'].apply(lambda x: re.sub('  ', ' ', x))
#
## remove https hyperlinks
#final_chat_data['tidy_text'] = final_chat_data['tidy_text'].str.replace("http\S+|[^\x00-\x7F]+|\d+", '')
#final_chat_data['tidy_text'] = final_chat_data['tidy_text'].apply(lambda x: re.sub('  ', ' ', x))
#
## Removal of ASCII, Punctuation, special character, @user_id, hyperlinks, numbers. Special case not handled previously,      
#final_chat_data['tidy_text'] = final_chat_data['tidy_text'].str.replace("(@[A-Za-z0-9]+)|([^0-9A-Za-z \t])|(\w+:\/\/\S+)|[\.\,\!\?\:\;\-\=]|\x92|[0-9]+|www.[^ ]+|[^\x00-\x7F]+|\d+", ' ')
#final_chat_data['tidy_text'] = final_chat_data['tidy_text'].apply(lambda x: re.sub('  ', ' ', x))
#
### removing the word's with length less than 2
#final_chat_data['tidy_text'] = final_chat_data['tidy_text'].apply(lambda x: ' '.join([w for w in x.split() if len(w)>2]))
#final_chat_data['tidy_text'] = final_chat_data['tidy_text'].apply(lambda x: re.sub('  ', ' ', x))
#
#final_chat_data['tidy_text'].isnull().sum()
#final_chat_data = final_chat_data[final_chat_data['tidy_text'] != ""]
#
#
#
#
###---------- Stopwords - Method I
#
## Import stopwords with nltk.
#from nltk.corpus import stopwords
#stop = stopwords.words('english')
#
## Exclude stopwords with Python's list comprehension and pandas.DataFrame.apply.
#final_chat_data['tidy_text'] = final_chat_data['tidy_text'].apply(lambda x: ' '.join([word for word in x.split() if word not in (stop)]))
#print(final_chat_data.head)
#
#
###---------- Stopwords - Method II
#
#import nltk
#nltk.download('stopwords')
# 
##stop_words = stopwords.words("english")
#cachedStopWords = set(stopwords.words("english"))
#
## add custom words
#cachedStopWords.update(( ))
#
#
#final_chat_data['tidy_text'] = final_chat_data['tidy_text'].apply(lambda x: ' '.join(x for x in x.split() if x not in cachedStopWords))
#
#
## then using it to make our stopword dictionary
#BOW_count_list = final_chat_data.tidy_text.str.split(expand=True).stack().value_counts()
#
#
#pd.DataFrame(BOW_count_list).to_excel('BOW_count_list_1.xlsx', 'BOW_count_list')
#
#
#
#
#
#
#
#
#from nltk.stem.wordnet import WordNetLemmatizer
#lemmatizer = WordNetLemmatizer()
#
##print('-------Lemmazation--------')
#final_chat_data['tidy_text'] = final_chat_data['tidy_text'].apply(lambda x: ' '.join([lemmatizer.lemmatize(word) for word in x.split() ]))
#
#
#
#
#
#
#final_chat_data[final_chat_data["tidy_text"]=="aldate"]
#
#
#
#
#
#
##
##import nltk
##nltk.download('stopwords')
##  
###stop_words = stopwords.words("english")
##cachedStopWords = set(stopwords.words("english"))
##
##
### add custom words
##cachedStopWords.update(('hello','welcome','three','hi','hey','my','us''about','again','and','any','are',
##'been','being','both','but','can','could','did','does',
##'doing','each','for','from','further','had','has','have','having','her','here','hers','because','therefore','either'
##'herself','him','himself','his','into','its','itself','just','myself','now','once','one','get',
##'only','other','our','ours','ourselves','out','own','same','she','should','some','such','than','that','the',
##'their','theirs','them','themselves','then','there','these','they','this','those','through',
##'until','very','was','were','while','will','with','would','you','let','pre','use','your',
##'yours','yourself','yourselves','thats','hi','hello','welcome','you','okay','ok','sure','shot','yeah','yes','hour',
##'hours','my','my','me','mine','job','log','away','pin','indeed','way','per','ing','from',
##'on','off','don','youre','outlook', 'com','yes', 'bye','app','ahh','thx','yah','yea','please','off','yaa'
##))
##
##
##final_chat_data['tidy_text'] = final_chat_data['tidy_text'].apply(lambda x: " ".join(x for x in x.split() if x not in cachedStopWords))
##
##
##
##
##
#### removing the word's with length less than 2
##final_chat_data['tidy_text'] = final_chat_data['tidy_text'].apply(lambda x: ' '.join([w for w in x.split() if len(w)>2]))
##final_chat_data['tidy_text'] = final_chat_data['tidy_text'].apply(lambda x: re.sub('  ', ' ', x))
##
##
##
###
###g = final_chat_data.groupby(['Chat_ID'])
###d = g.first().append(g.first()).sort_index().reset_index().drop_duplicates()
###
###d.head()
###
###
###
###for index, row in final_chat_data.iterrows():
###    df2 = pd.DataFrame(final_chat_data.groupby(['Chat_ID','Name']).reset_index().ix[0])
###   
###   
###   
###df = pd.DataFrame({'id' : [1,1,1,2,2,3,3,3,3,4,4],
###            'value'  : ["first","second","third", np.NaN,
###                        "second","first","second","third",
###                        "fourth","first","second"]})
###df
###    
###x=df.groupby('id').nth(0)
###y=x.append(df.groupby('id').nth(1))
###y
###
###
##
##
#
