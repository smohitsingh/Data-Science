# -*- coding: utf-8 -*-
"""
Created on Wed Aug 21 17:48:21 2019

@author: PH00613755
"""

import pandas as pd
import pyodbc
# Scikit-learn
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import LabelEncoder
# Keras
from keras.preprocessing.text import Tokenizer
from keras.preprocessing.sequence import pad_sequences
from keras.models import Sequential
from keras.layers import Activation, Dense, Dropout, Embedding, Flatten, Conv1D, MaxPooling1D, LSTM
from keras import utils
from keras.callbacks import ReduceLROnPlateau, EarlyStopping
from keras.callbacks import EarlyStopping, ModelCheckpoint
# nltk
import nltk
from nltk.corpus import stopwords
from keras.utils import to_categorical
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
import h5py
from keras.models import load_model
from keras.models import model_from_json
import tensorflow as tf



## ------------    Loading model      ---------------- ##
def model_tokenizer():
    model_test = load_model('D:\\sentiment analysis\\Customer_Chat_Model\\BestModel_7k.h5')
    
    with open('D:\\sentiment analysis\\Customer_Chat_Model\\tokenizer_7k.pickle', 'rb') as handle:
        tok = pickle.load(handle)

        
class SentimentAnalysis():       

    def get_sentiment(self,twt):
        
        df = pd.DataFrame(columns=['raw_tweet'])
#        db_twitter_data = 
        
        #### --------- Starting data cleaning section
        def remove_pattern(input_txt, pattern):
            r = re.findall(pattern, input_txt)
            for i in r:
                input_txt = re.sub(i, '', input_txt)
                
            return input_txt
        
        # remove twitter handles (@user)
        db_twitter_data['tidy_tweet'] = db_twitter_data['raw_tweet'].apply(lambda x: re.sub('@[\w]*', '', x.lower()))
        db_twitter_data['tidy_tweet'] = db_twitter_data['tidy_tweet'].apply(lambda x: re.sub('  ', ' ', x))

        # remove https hyperlinks
        db_twitter_data['tidy_tweet'] = db_twitter_data['tidy_tweet'].str.replace("http\S+|[^\x00-\x7F]+|\d+", '')
        db_twitter_data['tidy_tweet'] = db_twitter_data['tidy_tweet'].apply(lambda x: re.sub('  ', ' ', x))

        # Convert to lower
        db_twitter_data['tidy_tweet'] = db_twitter_data['tidy_tweet'].str.lower()
 
        
        # tweet_data['tidy_tweet']
        # Convert to lower
        db_twitter_data['tidy_tweet'] = db_twitter_data['tidy_tweet'].str.lower()
        
        
        ######   encoding the correct spelling for certain words 
        negations_dic = {
                "ain't":"is not",
                "amn't":"am not",
                "aren't":"are not",
                "'cause":"because",
                "couldn't":"could not",
                "couldn't've":"could not have",
                "could've":"could have",
                "daren't":"dare not",
                "daresn't":"dare not",
                "dasn't":"dare not",
                "didn't":"did not",
                "doesn't":"does not",
                "don't":"do not",
                "e'er":"ever",
                "em":"them",
                "everyone's":"everyone is",
                "finna":"fixing to",
                "gimme":"give me",
                "gonna":"going to",
                "gon't":"go not",
                "gotta":"got to",
                "hadn't":"had not",
                "hasn't":"has not",
                "haven't":"have not",
                "he'd":"he would",
                "he'll":"he will",
                "he's":"he is",
                "he've":"he have",
                "how'd":"how would",
                "how'll":"how will",
                "how're":"how are",
                "how's":"how is",
                "i'd":"i would",
                "i'll":"i will",
                "i'm":"i am",
                "i'm'a":"i am about to",
                "i'm'o":"i am going to",
                "isn't":"is not",
                "it'd":"it would",
                "it'll":"it will",
                "it's":"it is",
                "its":"it is",
                "i've":"i have",
                "kinda":"kind of",
                "let's":"let us",
                "mayn't":"may not",
                "may've":"may have",
                "mightn't":"might not",
                "might've":"might have",
                "mustn't":"must not",
                "mustn't've":"must not have",
                "must've":"must have",
                "needn't":"need not",
                "ne'er":"never",
                "o'":"of",
                "o'er":"over",
                "ol'":"old",
                "oughtn't":"ought not",
                "shalln't":"shall not",
                "shan't":"shall not",
                "she'd":"she would",
                "she'll":"she will",
                "she's":"she is",
                "shouldn't":"should not",
                "shouldn't've":"should not have",
                "should've":"should have",
                "somebody's":"somebody is",
                "someone's":"someone is",
                "something's":"something is",
                "that'd":"that would",
                "that'll":"that will",
                "that're":"that are",
                "that's":"that is",
                "there'd":"there would",
                "there'll":"there will",
                "there're":"there are",
                "there's":"there is",
                "these're":"these are",
                "they'd":"they would",
                "they'll":"they will",
                "they're":"they are",
                "they've":"they have",
                "this's":"this is",
                "those're":"those are",
                "'tis":"it is",
                "'its":"it is",
                "'twas":"it was",
                "wanna":"want to",
                "wasn't":"was not",
                "we'd":"we would",
                "we'd've":"we would have",
                "we'll":"we will",
                "we're":"we are",
                "weren't":"were not",
                "we've":"we have",
                "what'd":"what did",
                "what'll":"what will",
                "what're":"what are",
                "what's":"what is",
                "what've":"what have",
                "when's":"when is",
                "where'd":"where did",
                "where're":"where are",
                "where's":"where is",
                "where've":"where have",
                "which's":"which is",
                "who'd":"who would",
                "who'd've":"who would have",
                "who'll":"who will",
                "who're":"who are",
                "who's":"who is",
                "who've":"who have",
                "why'd":"why did",
                "why're":"why are",
                "why's":"why is",
                "won't":"will not",
                "wouldn't":"would not",
                "would've":"would have",
                "y'all":"you all",
                "you'd":"you would",
                "you'll":"you will",
                "you're":"you are",
                "you've":"you have",
                "Whatcha":"What are you",
                "luv":"love",
                "sux":"sucks",
                "isn't":"is not", 
                "aren't":"are not", "wasn't":"was not", 
                "weren't":"were not",   
                "haven't":"have not","hasn't":"has not",
                "hadn't":"had not","won't":"will not",
                "wouldn't":"would not", "don't":"do not","doesn't":"does not","didn't":"did not","can't":"cannot",
                "can not":"cannot","couldn't":"could not","shouldn't":"should not",
                "mightn't":"might not",
                "mustn't":"must not"}
        
        
        
        pat1 = '[0-9]+'
        # remove twitter handles www.xyz website names
        pat2 = 'www.[^ ]+'
        pattern = '|'.join((pat1,pat2))
        
        neg_pattern = re.compile(r'\b(' + '|'.join(negations_dic.keys()) + r')\b') 
        
        clean_tweets = []
        for t in db_twitter_data['tidy_tweet']:
            t = neg_pattern.sub(lambda x: negations_dic[x.group()], t)
            clean_tweets.append(t)
        
        # Putting the clean tweets in separate column "tidy_tweet" 
        clean_tweets
        db_twitter_data['tidy_tweet'] = pd.DataFrame(clean_tweets,columns = ['tidy_tweet'])
        
        
        # Removal of ASCII, Punctuation, special character, @user_id, hyperlinks, numbers. Special case not handled previously,      
        db_twitter_data['tidy_tweet'] = db_twitter_data['tidy_tweet'].str.replace("(@[A-Za-z0-9]+)|([^0-9A-Za-z \t])|(\w+:\/\/\S+)|[\.\,\!\?\:\;\-\=]|\x92|[^\x00-\x7F]+|\d+", " ")
        db_twitter_data['tidy_tweet']
        
        # removing the word's with length less than 2
        db_twitter_data['tidy_tweet'] = db_twitter_data['tidy_tweet'].apply(lambda x: ' '.join([w for w in x.split() if len(w)>2]))
#        tidy_tweet =  db_twitter_data['tidy_tweet'].str.get()
        
        twt = tok.texts_to_sequences(db_twitter_data['tidy_tweet'])
#        print(twt)
        
        
        max_len= 50
        max_features= 50000
        
        #padding the tweet to have exactly the same shape as `embedding_2` input
        twt = pad_sequences(twt, maxlen= max_len, dtype='int32', value=0)
#        print(twt)
        
        sentiment = model_test.predict(twt)[0]
        x = np.argmax(sentiment)
        sentiment[x]*100
        if(np.argmax(sentiment) == 0):
            pred_sentiment = "Neutral"
        elif (np.argmax(sentiment) == 1):
            pred_sentiment = "Negative"
        elif (np.argmax(sentiment) == 2):
            pred_sentiment = "Positive"
            
        return pred_sentiment,sentiment[x]*100

                

    
    
    
    
    #	 creating object of TwitterClient Class
sentiment_class = SentimentAnalysis()

test_data = pd.read_excel (r'D:\\sentiment analysis\\Customer_Chat_Model\\testing_data_01.xlsx',sheet_name= "Sheet1") 
#(use "r" before the path string to address special character, such as '\'). Don't forget to put the file name at the end of the path + '.xlsx'


print(test_data.shape)
print(test_data.columns)



for results in test_data.testing_text:
    twt = results
    prediction = sentiment_class.get_sentiment(twt)
    print(prediction[0],prediction[1])
    
    sentiment = prediction[0]
    polarity =  round(prediction[1],2)
#    tidy_tweet = prediction[2]
  
          