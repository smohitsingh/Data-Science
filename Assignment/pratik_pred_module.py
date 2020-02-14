# -*- coding: utf-8 -*-
"""
Created on Mon Aug 26 11:51:22 2019

@author: PH00613755
"""
import tweepy
from tweepy import OAuthHandler
import pyodbc 
import time
from datetime import timedelta,datetime,date
import pandas as pd
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
from keras import backend as K
# nltk
import nltk
#nltk.download('stopwords')
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
#import tensorflow.compat.v1 as tf
#tf.disable_v2_behavior() 
import emoji
from collections import defaultdict
from nltk.corpus import stopwords

#Flask

from flask import Flask, request
from flask_restful import Resource, Api
from sqlalchemy import create_engine
from json import dumps
#from flask.ext.jsonpify import jsonify

global graph

graph = tf.get_default_graph()

model_test = load_model('best_model_lstm_128.h5')
model_test._make_predict_function()

with open('tokenizer.pickle', 'rb') as handle:
    tok = pickle.load(handle)
    

def parameter(title,from_date,to_date):
    count = 50
    get_tweets(title,title +"-filter:retweets",count,from_date,to_date)



def daterange(date1, date2):
    for n in range(int ((date2 - date1).days)+1):
        if(date1 != date1 + timedelta(n)):
            yield date1 + timedelta(n),date1 + timedelta(n+1)
        else:
            yield date1,date1 + timedelta(n+1)
       
#class SentimentAnalysis():
    
def get_sentiment(twt):      
#        twt = "#TECHM: Tech Mahindra Ltd has lost a star, now a 3 star stock. (https://t.co/izkttPMzJT)"      
    df = pd.DataFrame(columns=['raw_tweet'])
    db_twitter_data = df.append({'raw_tweet': twt}, ignore_index=True)
    db_twitter_data
    
    #### --------- Starting data cleaning section
    def remove_pattern(input_txt, pattern):
        r = re.findall(pattern, input_txt)
        for i in r:
            input_txt = re.sub(i, '', input_txt)
            
        return input_txt
    
    # remove twitter handles (@user)
    db_twitter_data['tidy_tweet'] = np.vectorize(remove_pattern)(db_twitter_data['raw_tweet'], "@[\w]*")
    
    db_twitter_data['tidy_tweet'] = db_twitter_data['tidy_tweet'].str.replace("http\S+|[^\x00-\x7F]+|\d+", " ")
    db_twitter_data['tidy_tweet']
    
    db_twitter_data['tidy_tweet'] = db_twitter_data['tidy_tweet'].str.lower()

    # removing the word's with length less than 2
    db_twitter_data['tidy_tweet'] = db_twitter_data['tidy_tweet'].apply(lambda x: ' '.join([w for w in x.split() if len(w)>2]))

    
# Deal with emojis           
    emoji_tweet = []
    for t in db_twitter_data['tidy_tweet']:
        t = emoji.demojize(t)
        t = t.replace(":"," ")
        t = ' '.join(t.split())
        emoji_tweet.append(t)
    
    db_twitter_data['tidy_tweet'] = pd.DataFrame(emoji_tweet,columns = ['tidy_tweet'])        
    
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
    
            
    neg_pattern = re.compile(r'\b(' + '|'.join(negations_dic.keys()) + r')\b') 
    
    clean_tweets = []
    for t in db_twitter_data['tidy_tweet']:
        t = neg_pattern.sub(lambda x: negations_dic[x.group()], t)
        clean_tweets.append(t)
    
    # Putting the clean tweets in separate column "tidy_tweet" 
    db_twitter_data['tidy_tweet'] = pd.DataFrame(clean_tweets,columns = ['tidy_tweet'])
            
    # Removal of ASCII, Punctuation, special character, @user_id, hyperlinks, numbers. Special case not handled previously,      
    db_twitter_data['tidy_tweet'] = db_twitter_data['tidy_tweet'].str.replace("(@[A-Za-z0-9]+)|([^0-9A-Za-z \t])|(\w+:\/\/\S+)|[\.\,\!\?\:\;\-\=]|\x92|[0-9]+|www.[^ ]+|[^\x00-\x7F]+|\d+", "")

    cachedStopWords = set(stopwords.words("english"))
    #add custom words
    
    cachedStopWords.update(('about','again','and','any','are','been','being','both','but','can','could','did','does',
    'doing','each','for','from','further','had','has','have','having','her','here','hers','because','therefore','either'
    'herself','him','himself','his','into','its','itself','just','myself','now','once',
    'only','other','our','ours','ourselves','out','own','same','she','should','some','such','than','that','the',
    'their','theirs','them','themselves','then','there','these','they','this','those','through',
    'until','very','was','were','while','will','with','would','you','your','yours','yourself','yourselves', 'tech mahindra', 'amp', 'gaia', 'for', 'with', 'our',
     'you', 'dontbeplastic', 'use', 'via', 'all', 'part', 'new ', 'iot', 'way', 'hrs', 'such', 'know', 'soin', 'rpa', 'aws', 'sir',
     'cea','day','exp','launches','give','giving','harassment','news','one','take','taking','techm','techmahindra', 'telecom','telensa','testers', 'tomorrow','win','wins'))
    
    db_twitter_data['tidy_tweet'] = db_twitter_data['tidy_tweet'].apply(lambda x: " ".join(x for x in x.split() if x not in cachedStopWords))
    
    # removing the word's with length less than 2
    db_twitter_data['tidy_tweet'] = db_twitter_data['tidy_tweet'].apply(lambda x: ' '.join([w for w in x.split() if len(w)>2]))
    
    tidy_tweet =  db_twitter_data['tidy_tweet']
    
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
        
    return pred_sentiment,sentiment[x]*100,tidy_tweet[0]          


def get_tweets(title,query,count,from_date,to_date):
    start_dt = from_date
    end_dt = to_date
    
    # Entering the required keys and tokens from the Twitter Dev Console
    consumer_key = 'ZCSqJsTZjC4Wkr199ezEZnvpq'
    consumer_secret = 'dKr9PEqndTnY5oAPgcqUo71rl0JyUnGJAZBcaAuPGoEcX6SRk1'
    access_key = '950428383007182849-Lp4MtTjdIDXHOcKpaiglTMJhm3zR3zr'
    access_secret = 'Tm1EIxTCjfhsZFsL3lzBgEgZePPJ0cKKstBqt9Y5P03gY'

    

    
#    conn = pyodbc.connect('Driver={SQL Server};'
#                  'Server=INPUHJPC10109\SQLEXPRESS;'
#                  'Database=twitter;'
#                  'Trusted_Connection=yes;')
#    cursor = conn.cursor();
    
       
    conn = pyodbc.connect("Driver={SQL Server Native Client 11.0};"
                        "Server=172.28.48.16,1401;"
                        "Database=BPS_Twitter;"
                        "uid=bpstwitter;pwd=bpstwitter")
    cursor = conn.cursor();
    

    
    
    
    
    # Authorization to consumer key and consumer secret 
    auth = tweepy.OAuthHandler(consumer_key, consumer_secret) 
    # Access to user's access key and access secret 
    auth.set_access_token(access_key, access_secret) 
    # Calling api 
    api = tweepy.API(auth) 
#    api = tweepy.API(auth,proxy='http://10.254.40.122:8080') 
    
#    sentiment_class = SentimentAnalysis()
#        cursor.execute('truncate table twitter_raw_data')
    
    for dt,tdt in daterange(start_dt, end_dt):
        
        for tweet in tweepy.Cursor(api.search, query,since=dt.strftime("%Y-%m-%d"), until=tdt.strftime("%Y-%m-%d"),tweet_mode='extended').items(count):
                        
            title = title
            tweet_user = tweet.user.name
            tweet_handle = tweet.user.screen_name
            twt = tweet.full_text
            tweet_date = tweet.created_at
            followers_count = tweet.user.followers_count
            retweet_count = tweet.retweet_count
            favorite_count = tweet.favorite_count
            source = tweet.source
            user_location =tweet.user.location
            language = tweet.lang
            link = "https://twitter.com/"+tweet_handle+"/status/"+tweet.id_str
            prediction = get_sentiment(twt)
            sentiment = prediction[0]
            polarity =  round(prediction[1],2)
            tidy_tweet = prediction[2]
     
            cursor.execute('INSERT INTO twitter_pol_data_new(Title,user_name, handle, tweet, date, followers_count, retweet_count, favourite_count,source,user_loc,language,link, sentiment, polarity,tidy_tweet) Values (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)', (title,tweet_user,tweet_handle,twt,tweet_date,followers_count,retweet_count,favorite_count,source,user_location,language,link,sentiment,polarity,tidy_tweet))
        conn.commit()
        

# Driver code 
#if __name__ == '__main__':
#    sentiment_class = SentimentAnalysis()
#    #    get_tweets("Narendra Modi-filter:retweets",100)
#    title = "Iran"
#    sentiment_class.get_tweets(title,title +"-filter:retweets",250)
