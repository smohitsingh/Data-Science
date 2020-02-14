# -*- coding: utf-8 -*-
"""
Created on Thu Jan 30 10:57:37 2020

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
raw_data  = pd.read_excel(file1,sheet_name='Sheet1')


raw_data.info()
raw_data.columns
# [ 'chat_id', 'tidy_text', 'issue_label', 'labels_v1', 'vlookup' ]

raw_data.shape
#   (10000, 5)


raw_data = raw_data.loc[:, 'chat_id' :'issue_label']

raw_data = raw_data[raw_data['issue_label'].notnull()]
#raw_data = raw_data[raw_data['tidy_text'].notnull()]
#raw_data = raw_data[raw_data['chat_id'].notnull()]

raw_data.shape 
#    (6218, 3)


raw_data = raw_data.drop_duplicates()
raw_data = raw_data.reset_index(drop = True)
raw_data.shape 

raw_data['chat_id'].nunique()
#   6218
raw_data['issue_label'].nunique()
#   24

raw_data = raw_data.astype(str)


raw_data['tidy_text_1'] =  raw_data['tidy_text'].str.lower()

# strip the blank spaces in the starting and end of the  "tidy_summary"
raw_data['tidy_text_1'] = raw_data['tidy_text_1'].apply(lambda x: x.strip())
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


#-------- Checking the distribution of "Issue_Label" --------------------------


plt.figure(figsize=(15,8))
sns.countplot(x = "issue_label", data = raw_data)
plt.title("Dataset labels distribution")




#raw_data['tidy_text_1'] =  raw_data['tidy_text'].str.lower()

# strip the blank spaces in the starting and end of the  "tidy_summary"
#raw_data['tidy_text_1'] = raw_data['tidy_text_1'].apply(lambda x: x.strip())
#raw_data['issue_label'] = raw_data['issue_label'].apply(lambda x: x.strip())

#
#raw_data['tidy_text_1'] = raw_data['tidy_text_1'].apply(lambda x: re.sub('@[\w]*', '', str(x).lower()))
#raw_data['tidy_text_1'] = raw_data['tidy_text_1'].apply(lambda x: re.sub('  ', ' ', x))
#
## remove https hyperlinks
#raw_data['tidy_text_1'] = raw_data['tidy_text_1'].str.replace("http\S+|[^\x00-\x7F]+|\d+", '')
#raw_data['tidy_text_1'] = raw_data['tidy_text_1'].apply(lambda x: re.sub('  ', ' ', x))


######   encoding the correct spelling for certain words 
#
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
#        "mightn't":"might not","mustn't":"must not","wouldn":"would not","won't":"would not","weren":"were not",
#        "yea":"yeah","hasnt":"has not","aint":"are not","arent":"are not","cant":"cannot","dnt":"did not",
#        "wont":"would not","wnt":"was not","werent":"were not","wernt":"were not","txt":"text",
#        "abored" : "aboard",
#"abour" : "about",
#"abroard" : "abroad",
#"abroat" : "abroad",
#"abroud" : "abroad",
#"absolulty" : "absolute",
#"absolutely" : "absolute",
#"absorbing" : "absorb",
#"acceptable" : "accept",
#"accepted" : "accept",
#"accepting" : "accept",
#"acces" : "access",
#"accedeint" : "accident",
#"accidentally" : "accident",
#"accont" : "account",
#"accounts" : "account",
#"accout" : "account",
#"qccount" : "account",
#"activated" : "activate",
#"activateit" : "activate",
#"activating" : "activate",
#"activation" : "activate",
#"activiated" : "activate",
#"activ" : "active",
#"adding" : "added",
#"additionally" : "additional",
#"additonal" : "additional",
#"addons" : "addon",
#"addresses" : "address",
#"adress" : "address",
#"admitted" : "admit",
#"addult" : "adult",
#"adukt" : "adult",
#"adul" : "adult",
#"adults" : "adult",
#"advanced" : "advance",
#"advertised" : "advertise",
#"advertises" : "advertise",
#"adverts" : "advertise",
#"advice" : "advise",
#"advicer" : "advise",
#"advised" : "advise",
#"adviser" : "advise",
#"advisor" : "advise",
#"advisors" : "advise",
#"affected" : "affect",
#"affecting" : "affect",
#"affordable" : "afford",
#"afrod" : "afford",
#"afterwards" : "afterward",
#"ahain" : "again",
#"agains" : "against",
#"ages" : "age",
#"agents" : "agent",
#"agoi" : "ago",
#"agreed" : "agree",
#"alllow" : "allow",
#"allowed" : "allow",
#"allowing" : "allow",
#"allows" : "allow",
#"allowances" : "allowance",
#"allowence" : "allowance",
#"allownce" : "allowance",
#"allways" : "always",
#"alves" : "always",
#"alwsys" : "always",
#"amgetting" : "am getting",
#"ammont" : "amount",
#"amounts" : "amount",
#"snd" : "and",
#"annoyed" : "annoy",
#"annoying" : "annoy",
#"ans" : "answer",
#"answers" : "answer",
#"apologies" : "apology",
#"apologised" : "apology",
#"apologises" : "apology",
#"apps" : "app",
#"appwhen" : "app when",
#"appareantly" : "apparently",
#"apperently" : "apparently",
#"appeared" : "appear",
#"appearing" : "appear",
#"appears" : "appear",
#"happeared" : "appeared",
#"aplied" : "applied",
#"applied" : "apply",
#"appreciated" : "appreciate",
#"aprroved" : "approved",
#"approx" : "approximately",
#"aint" : "are not",
#"arent" : "are not",
#"arnt" : "are not",
#"areangment" : "arrangement",
#"arrange" : "arrangement",
#"arranged" : "arrangement",
#"arrangeme" : "arrangement",
#"arrangemements" : "arrangement",
#"arrangment" : "arrangement",
#"arrangments" : "arrangement",
#"arrnagement" : "arrangement",
#"arrived" : "arrive",
#"arrives" : "arrive",
#"arriving" : "arrive",
#"aswel" : "as well",
#"aswell" : "as well",
#"asked" : "ask",
#"asking" : "ask",
#"asks" : "ask",
#"assumed" : "assume",
#"assuming" : "assume",
#"attached" : "attach",
#"attax" : "attach",
#"attempted" : "attempt",
#"attempting" : "attempt",
#"attempts" : "attempt",
#"aug" : "august",
#"augusts" : "august",
#"australian" : "australia",
#"authorised" : "authorize",
#"auto" : "automatic",
#"automated" : "automatic",
#"automatically" : "automatic",
#"availability" : "available",
#"availabke" : "available",
#"availible" : "available",
#"avilable" : "available",
#"bck" : "back",
#"backdate" : "back date",
#"backpacking" : "back pack",
#"badly" : "bad",
#"baffled" : "baffle",
#"balamce" : "balance",
#"ballace" : "balance",
#"banking" : "bank",
#"bars" : "bar",
#"basucally" : "basically",
#"becasue" : "because",
#"becuase" : "because",
#"bevause" : "because",
#"bez" : "because",
#"becoming" : "become",
#"begging" : "beg",
#"began" : "begin",
#"begining" : "begin",
#"beginning" : "begin",
#"begins" : "begin",
#"begum" : "begin",
#"beeing" : "being",
#"belive" : "believe",
#"benefits" : "benefit",
#"bigger" : "big",
#"biils" : "bill",
#"bilk" : "bill",
#"billed" : "bill",
#"billing" : "bill",
#"billl" : "bill",
#"bills" : "bill",
#"tbing" : "bing",
#"birthday" : "birth",
#"birthwas" : "birth",
#"bites" : "bit",
#"blk" : "block",
#"blocke" : "block",
#"blocked" : "block",
#"blocker" : "block",
#"blockes" : "block",
#"blocking" : "block",
#"bloked" : "block",
#"bloking" : "block",
#"boos" : "boost",
#"booster" : "boost",
#"booted" : "boost",
#"bottles" : "bottle",
#"bounced" : "bounce",
#"bounces" : "bounce",
#"brinda" : "bring",
#"bringing" : "bring",
#"beoadband" : "broadband",
#"butnon" : "button",
#"buttons" : "button",
#"buxtons" : "button",
#"bough" : "buy",
#"bought" : "buy",
#"bougjt" : "buy",
#"buying" : "buy",
#"cahnge" : "change",
#"called" : "call",
#"calling" : "call",
#"calls" : "call",
#"canadian" : "canada",
#"cancelation" : "cancel",
#"canceled" : "cancel",
#"cancell" : "cancel",
#"cancellation" : "cancel",
#"cancelled" : "cancel",
#"cancelling" : "cancel",
#"cancle" : "cancel",
#"candad" : "cancel",
#"canelled" : "cancel",
#"cannot" : "can not",
#"canon" : "can not",
#"cant" : "can not",
#"cantect" : "contact",
#"cantmake" : "can not make",
#"capped" : "cap",
#"capping" : "cap",
#"cards" : "card",
#"careful" : "care",
#"carefully" : "care",
#"carnt" : "can not",
#"carried" : "carry",
#"carrier" : "carry",
#"cashback" : "cash",
#"caused" : "cause",
#"causes" : "cause",
#"causing" : "cause",
#"celuar" : "cellular",
#"certainly" : "certain",
#"cewo" : "ceo",
#"chages" : "change",
#"changed" : "change",
#"changes" : "change",
#"changing" : "change",
#"changw" : "change",
#"chargeable" : "charge",
#"charged" : "charge",
#"chargers" : "charge",
#"charges" : "charge",
#"charging" : "charge",
#"chargw" : "charge",
#"charring" : "charge",
#"xharges" : "charge",
#"chasing" : "chase",
#"chating" : "chat",
#"chatline" : "chat",
#"chats" : "chat",
#"chatted" : "chat",
#"chatting" : "chat",
#"chcek" : "check",
#"cheaper" : "cheap",
#"cheapest" : "cheap",
#"checked" : "check",
#"checking" : "check",
#"checks" : "check",
#"cheek" : "check",
#"chenge" : "change",
#"chose" : "choose",
#"chosen" : "choose",
#"claimed" : "claim",
#"claiming" : "claim",
#"clainmimg" : "claim",
#"cleaning" : "clean",
#"cleared" : "clear",
#"clearly" : "clear",
#"clicked" : "click",
#"clients" : "client",
#"closed" : "close",
#"closes" : "close",
#"cobtract" : "contract",
#"codes" : "code",
#"colegue" : "colleague",
#"colleage" : "colleague",
#"colleagues" : "colleague",
#"collected" : "collect",
#"collection" : "collect",
#"collections" : "collect",
#"collectors" : "collect",
#"colleuges" : "colleague",
#"comection" : "connect",
#"comes" : "come",
#"comfirmation" : "comfirm",
#"coming" : "come",
#"communications" : "communication",
#"companies" : "company",
#"companys" : "company",
#"compares" : "compare",
#"comparing" : "compare",
#"compatable" : "compatible",
#"compensated" : "compensate",
#"compensation" : "compensate",
#"competence" : "competition",
#"competitions" : "competition",
#"complain" : "complaint",
#"complained" : "complaint",
#"complaining" : "complaint",
#"complaints" : "complaint",
#"compleatly" : "complete",
#"completed" : "complete",
#"completely" : "complete",
#"compliant" : "complaint",
#"concerned" : "concern",
#"cone" : "come",
#"confirmation" : "confirm",
#"confirmed" : "confirm",
#"confirmig" : "confirm",
#"confirming" : "confirm",
#"confused" : "confuse",
#"confusion" : "confuse",
#"conmection" : "connect",
#"connected" : "connect",
#"connecting" : "connect",
#"connection" : "connect",
#"connections" : "connect",
#"conned" : "connect",
#"conntact" : "contact",
#"consideration" : "consider",
#"considering" : "consider",
#"constantly" : "constant",
#"contacted" : "contact",
#"contacting" : "contact",
#"contacts" : "contact",
#"contect" : "contact",
#"continued" : "continue",
#"continues" : "continue",
#"contracta" : "contract",
#"contracted" : "contract",
#"contractes" : "contract",
#"contracts" : "contract",
#"controle" : "control",
#"conversations" : "conversation",
#"copied" : "copy",
#"correctly" : "correct",
#"cos" : "cost",
#"costa" : "cost",
#"costing" : "cost",
#"costs" : "cost",
#"coukd" : "could",
#"couldnt" : "could not",
#"countries" : "country",
#"covered" : "cover",
#"covers" : "cover",
#"cracked" : "crack",
#"cracks" : "crack",
#"crashing" : "crash",
#"crdit" : "credit",
#"creadit" : "credit",
#"credited" : "credit",
#"crediti" : "credit",
#"crete" : "create",
#"crying" : "cry",
#"csnnot" : "can not",
#"aicubeb" : "cube",
#"currently" : "current",
#"customerim" : "customer",
#"customers" : "customer",
#"customerwhy" : "customer",
#"customeryes" : "customer",
#"customeryou" : "customer",
#"custumer" : "customer",
#"cutomer" : "customer",
#"cuts" : "cut",
#"cutting" : "cut",
#"damaged" : "damage",
#"dat" : "data",
#"datapass" : "data pass",
#"dated" : "date",
#"dates" : "date",
#"datt" : "date",
#"daya" : "day",
#"days" : "day",
#"dealing" : "deal",
#"deals" : "deal",
#"dealt" : "deal",
#"debited" : "debit",
#"debits" : "debit",
#"decided" : "decide",
#"declined" : "decline",
#"deducted" : "deduct",
#"deduction" : "deduct",
#"defiantly" : "definitely",
#"delayed" : "delay",
#"deleted" : "delete",
#"delievered" : "deliver",
#"deliveey" : "deliver",
#"delivered" : "deliver",
#"delivery" : "deliver",
#"deliveted" : "deliver",
#"deparment" : "department",
#"depends" : "depend",
#"dept" : "department",
#"destinations" : "destination",
#"detailed" : "detail",
#"details" : "detail",
#"devices" : "device",
#"dialing" : "dial",
#"dialled" : "dial",
#"dialling" : "dial",
#"didnt" : "did not",
#"died" : "die",
#"dying" : "die",
#"differance" : "different",
#"differances" : "different",
#"difference" : "different",
#"differing" : "different",
#"diffrent" : "different",
#"difficulties" : "difficult",
#"difficulty" : "difficult",
#"digits" : "digit",
#"directly" : "direct",
#"disabled" : "disable",
#"disabling" : "disable",
#"disappeared" : "disappear",
#"dissappeared" : "disappear",
#"disappointed" : "disappoint",
#"disappointing" : "disappoint",
#"disconnecting" : "disconnected",
#"disconneted" : "disconnected",
#"disconected" : "disconnected",
#"discounted" : "discount",
#"discounts" : "discount",
#"discovered" : "discover",
#"discussed" : "discuss",
#"discussing" : "discuss",
#"discuu" : "discuss",
#"dissproportunate" : "disproportionate",
#"disputed" : "dispute",
#"dioont" : "do not",
#"dont" : "do not",
#"dony" : "do not",
#"documents" : "document",
#"doe" : "does",
#"doesn" : "does not",
#"doesnt" : "does not",
#"doest" : "does not",
#"dowload" : "download",
#"downloaded" : "download",
#"downloading" : "download",
#"downloads" : "download",
#"dropped" : "drop",
#"dsactiva" : "deactivate",
#"erlier" : "early",
#"eary" : "early",
#"earr" : "early",
#"earliest" : "early",
#"earlier" : "early",
#"ealier" : "early",
#"easily" : "easy",
#"easiest" : "easy",
#"easier" : "easy",
#"easyjet" : "easy jet",
#"eaten" : "eat",
#"effective" : "effect",
#"effected" : "effect",
#"ither" : "either",
#"elligible" : "eligible",
#"eligable" : "eligible",
#"elsewhere" : "else where",
#"emails" : "email",
#"emailed" : "email",
#"emergancy" : "emergency",
#"emargency" : "emergency",
#"employer" : "employee",
#"employed" : "employee",
#"enabling" : "enable",
#"enabled" : "enable",
#"ends" : "end",
#"ending" : "end",
#"ended" : "end",
#"enquiring" : "enquiry",
#"enquired" : "enquiry",
#"enquire" : "enquiry",
#"entering" : "enter",
#"entered" : "enter",
#"enitre" : "entire",
#"errors" : "error",
#"erroneously" : "error",
#"escalated" : "escalate",
#"essentially" : "essential",
#"europeit" : "europe",
#"european" : "europe",
#"eventhough" : "even though",
#"events" : "event",
#"excellence" : "excellent",
#"exited" : "excited",
#"expecting" : "expect",
#"expected" : "expect",
#"expensive" : "expense",
#"experiencing" : "experience",
#"expiry" : "expire",
#"expiring" : "expire",
#"expires" : "expire",
#"expired" : "expire",
#"expiration" : "expire",
#"explanation" : "explain",
#"explaining" : "explain",
#"explained" : "explain",
#"exploring" : "explore",
#"extesion" : "extend",
#"extentions" : "extend",
#"extention" : "extend",
#"extension" : "extend",
#"extending" : "extend",
#"extended" : "extend",
#"extsension" : "extension",
#"exstension" : "extension",
#"extremly" : "extreme",
#"extremely" : "extreme",
#"eyes" : "eye",
#"failed" : "fail",
#"failing" : "fail",
#"fairly" : "fair",
#"faster" : "fast",
#"fastest" : "fast",
#"faulty" : "fault",
#"features" : "feature",
#"fees" : "fee",
#"feeling" : "feel",
#"feels" : "feel",
#"filled" : "fill",
#"filling" : "fill",
#"filme" : "film",
#"filtering" : "filter",
#"filters" : "filter",
#"finally" : "final",
#"finacially" : "financial",
#"financially" : "financial",
#"finder" : "find",
#"finding" : "find",
#"finished" : "finish",
#"finishes" : "finish",
#"firstly" : "first",
#"fitted" : "fit",
#"fixable" : "fix",
#"fixed" : "fix",
#"followed" : "follow",
#"following" : "follow",
#"follwing" : "follow",
#"forgetting" : "forget",
#"forgot" : "forget",
#"forgotten" : "forget",
#"forwarded" : "forward",
#"forwarding" : "forward",
#"forwards" : "forward",
#"found" : "find",
#"freezes" : "freeze",
#"froze" : "freeze",
#"frozen" : "freeze",
#"feom" : "from",
#"frustrated" : "frustrate",
#"frustrating" : "frustrate",
#"frustration" : "frustrate",
#"fustrating" : "frustrate",
#"fucker" : "fuck",
#"fucking" : "fuck",
#"fully" : "full",
#"funny" : "fun",
#"funds" : "fund",
#"furthermore" : "further",
#"gadgets" : "gadget",
#"games" : "game",
#"gave" : "give",
#"geting" : "get",
#"gets" : "get",
#"gettin" : "get",
#"getting" : "get",
#"hetting" : "get",
#"gifted" : "gift",
#"gigsupposed" : "gig supposed",
#"gig" : "gigabyte",
#"gigabytes" : "gigabyte",
#"gige" : "gigabyte",
#"girlfriends" : "girlfriend",
#"given" : "give",
#"gives" : "give",
#"giving" : "give",
#"goroam" : "go roam",
#"gud" : "good",
#"goole" : "google",
#"gotnit" : "got",
#"gotten" : "got",
#"tgradehe" : "gradeup",
#"greatest" : "great",
#"gray" : "grey",
#"guaranteed" : "guarantee",
#"guessing" : "guess",
#"guys" : "guy",
#"gwt" : "gwf",
#"hadnt" : "had not",
#"halfway" : "half",
#"han" : "hand",
#"hands" : "hand",
#"hamdset" : "handset",
#"handsets" : "handset",
#"hapend" : "happen",
#"happend" : "happen",
#"happened" : "happen",
#"happening" : "happen",
#"happens" : "happen",
#"happily" : "happy",
#"harassed" : "harass",
#"hardly" : "hard",
#"hasnt" : "has not",
#"hassle" : "haste",
#"having" : "have",
#"hawe" : "have",
#"havent" : "have not",
#"havnt" : "have not",
#"headphones" : "headphone",
#"hallo" : "hello",
#"helli" : "hello",
#"helll" : "hello",
#"helllo" : "hello",
#"hellohaving" : "hello have",
#"helped" : "help",
#"helpful" : "help",
#"helpfull" : "help",
#"heres" : "here is",
#"higot" : "hi got",
#"higher" : "high",
#"holding" : "hold",
#"holders" : "holder",
#"holidays" : "holiday",
#"homeneed" : "home need",
#"honoured" : "honor",
#"honorview" : "honor view",
#"hopefully" : "hope",
#"hoping" : "hope",
#"hopital" : "hospital",
#"hospita" : "hospital",
#"hospitalised" : "hospital",
#"hosptal" : "hospital",
#"hotspots" : "hotspot",
#"hotspotcan" : "hotspot can",
#"hours" : "hour",
#"hrs" : "hour",
#"houses" : "house",
#"housre" : "house",
#"howeer" : "however",
#"hrsdue" : "hrs due",
#"huawai" : "huawei",
#"huwawei" : "huawei",
#"husbands" : "husband",
#"ihave" : "I have",
#"ive" : "I have",
#"lve" : "I have",
#"iwona" : "I want to ",
#"ideas" : "idea",
#"ignored" : "ignore",
#"illness" : "ill",
#"illnesses" : "ill",
#"unlitetet" : "illiterate",
#"imerror" : "im error",
#"imgood" : "im good",
#"immediately" : "immediate",
#"importance" : "important",
#"improvement" : "improve",
#"included" : "include",
#"includes" : "include",
#"including" : "include",
#"inconvienience" : "inconvenience",
#"increased" : "increase",
#"increasing" : "increase",
#"incredibly" : "incredible",
#"incured" : "incur",
#"incurred" : "incur",
#"incurring" : "incur",
#"indicated" : "indicate",
#"indication" : "indicate",
#"indoors" : "indoor",
#"informed" : "inform",
#"informing" : "inform",
#"info" : "information",
#"initially" : "initial",
#"inquiring" : "inquiry",
#"inserted" : "insert",
#"installed" : "install",
#"installment" : "install",
#"installments" : "install",
#"instructions" : "instruction",
#"insurnace" : "insurance",
#"intresting" : "interesting",
#"internatinal" : "international",
#"internationally" : "international",
#"btinternet" : "internet",
#"interbet" : "internet",
#"investigated" : "investigate",
#"investigation" : "investigate",
#"invoices" : "invoice",
#"ipadshow" : "ipad show",
#"isit" : "is it",
#"isnt" : "is not",
#"issued" : "issue",
#"issuer" : "issue",
#"issues" : "issue",
#"its" : "it is",
#"itlast" : "it last",
#"itplease" : "it please",
#"itemised" : "item",
#"jobs" : "job",
#"jon" : "job",
#"joined" : "join",
#"joining" : "join",
#"jul" : "july",
#"juli" : "july",
#"julys" : "july",
#"jumped" : "jump",
#"jun" : "june",
#"junes" : "june",
#"jus" : "just",
#"keeping" : "keep",
#"keeps" : "keep",
#"kept" : "keep",
#"kind" : "kindly",
#"kinda" : "kindly",
#"knowing" : "know",
#"known" : "know",
#"knw" : "know",
#"landing" : "landed",
#"lately" : "late",
#"lateri" : "later",
#"laughing" : "laugh",
#"leaving" : "leave",
#"lelf" : "left",
#"letting" : "let",
#"letters" : "letter",
#"lies" : "lie",
#"lifted" : "lift",
#"limited" : "limit",
#"limiter" : "limit",
#"lmit" : "limit",
#"lineas" : "line",
#"lines" : "line",
#"linked" : "link",
#"links" : "link",
#"living" : "live",
#"loading" : "load",
#"loads" : "load",
#"locked" : "lock",
#"logged" : "lodge",
#"logging" : "lodge",
#"logs" : "log",
#"lols" : "lol",
#"longer" : "long",
#"longstanding" : "long standing",
#"looked" : "look",
#"looking" : "look",
#"looks" : "look",
#"loose" : "lose",
#"losing" : "lose",
#"lost" : "loss",
#"lots" : "lot",
#"lower" : "low",
#"loyality" : "loyal",
#"mfourtune" : "m fortune",
#"mailed" : "mail",
#"maintinance" : "maintenance",
#"making" : "make",
#"makes" : "make",
#"manager" : "manage",
#"managed" : "manage",
#"marks" : "mark",
#"matching" : "match",
#"mates" : "mate",
#"materialise" : "materialize",
#"max" : "maximum",
#"maw" : "maximum",
#"maybe" : "may be",
#"meknow" : "me know",
#"meant" : "mean",
#"means" : "mean",
#"ment" : "meant",
#"members" : "member",
#"mentions" : "mention",
#"mentioning" : "mention",
#"mentioned" : "mention",
#"messed" : "mess",
#"imessages" : "message",
#"msssage" : "message",
#"messgaes" : "message",
#"messaging" : "message",
#"messagethe" : "message",
#"messages" : "message",
#"messaged" : "message",
#"midnight" : "mid night",
#"mid" : "middle",
#"middleton" : "middle ton",
#"myfi" : "mifi",
#"min" : "minimum",
#"mim" : "minimum",
#"minutes" : "minute",
#"minuets" : "minute",
#"minuet" : "minute",
#"mins" : "minute",
#"miniutes" : "minute",
#"missing" : "miss",
#"missed" : "miss",
#"mistakes" : "mistake",
#"mobiles" : "mobile",
#"mob" : "mobile",
#"mobilewifi" : "mobile wifi",
#"moments" : "moment",
#"mtonhs" : "month",
#"montly" : "month",
#"months" : "month",
#"monthly" : "month",
#"monthlt" : "month",
#"montgly" : "month",
#"montb" : "month",
#"monta" : "month",
#"mons" : "month",
#"monrhly" : "month",
#"momth" : "month",
#"mobth" : "month",
#"morenotice" : "more notice",
#"morningi" : "morning",
#"moving" : "move",
#"moved" : "move",
#"movies" : "movie",
#"muxh" : "much",
#"mich" : "much",
#"nuch" : "much",
#"mums" : "mum",
#"names" : "name",
#"nan" : "nano",
#"nearest" : "near",
#"nearly" : "near",
#"nee" : "need",
#"needed" : "need",
#"needs" : "need",
#"anither" : "neither",
#"natwork" : "network",
#"networks" : "network",
#"netwotk" : "network",
#"neu" : "new",
#"newer" : "new",
#"nights" : "night",
#"nimber" : "number",
#"nimbernever" : "number never",
#"non" : "none",
#"nooone" : "noon",
#"normally" : "normal",
#"normaly" : "normal",
#"nota" : "not",
#"note" : "not",
#"notbe" : "not be",
#"notshowung" : "not showing",
#"noto" : "not to",
#"nithing" : "nothing",
#"noticed" : "notice",
#"notices" : "notice",
#"notiched" : "notice",
#"notifications" : "notification",
#"notified" : "notification",
#"nuber" : "number",
#"numbers" : "number",
#"numerous" : "number",
#"nunber" : "number",
#"obliged" : "oblige",
#"obtaining" : "obtain",
#"obviously" : "obvious",
#"obviusly" : "obvious",
#"occasional" : "occassion",
#"occasionally" : "occassion",
#"occasions" : "occassion",
#"occurred" : "occur",
#"offered" : "offer",
#"offering" : "offer",
#"offers" : "offer",
#"offi" : "office",
#"older" : "old",
#"once" : "one",
#"ones" : "one",
#"onns" : "ons",
#"operators" : "operator",
#"options" : "option",
#"ordered" : "order",
#"ordering" : "order",
#"orginal" : "original",
#"originally" : "original",
#"othwr" : "other",
#"oustanding" : "outstanding",
#"outstabding" : "outstanding",
#"purstanding" : "outstanding",
#"oversea" : "overseas",
#"owed" : "owe",
#"pac" : "pack",
#"pak" : "pack",
#"pages" : "page",
#"painfully" : "pain",
#"paperwork" : "paper work",
#"partially" : "partial",
#"partners" : "partner",
#"pasg" : "pass",
#"passed" : "pass",
#"passord" : "passport",
#"passports" : "passport",
#"pastdue" : "past due",
#"pastduecredit" : "past due credit",
#"patiently" : "patience",
#"payd" : "pay",
#"payed" : "pay",
#"payg" : "pay",
#"paying" : "pay",
#"payinh" : "pay",
#"pays" : "pay",
#"payforit" : "pay for it",
#"payfotit" : "pay for it",
#"paym" : "payment",
#"paymant" : "payment",
#"payments" : "payment",
#"perfectly" : "perfect",
#"permanently" : "permanent",
#"phome" : "phone",
#"phoned" : "phone",
#"phones" : "phone",
#"phonesaregood" : "phone are good",
#"phonebill" : "phone bill",
#"phonecall" : "phone call",
#"phoneplease" : "phone please",
#"photos" : "photo",
#"picked" : "pick",
#"picking" : "pick",
#"pickpocketed" : "pickpocket",
#"pictures" : "picture",
#"piss" : "pissed",
#"pixels" : "pixel",
#"placed" : "place",
#"places" : "place",
#"planned" : "plan",
#"planning" : "plan",
#"plans" : "plan",
#"playlists" : "playlist",
#"pleaae" : "please",
#"pleas" : "please",
#"pleaseres" : "please",
#"pleassb" : "please",
#"plesse" : "please",
#"pls" : "please",
#"plse" : "please",
#"plz" : "please",
#"plugged" : "plug",
#"poind" : "point",
#"pointtv" : "point tv",
#"porno" : "porn",
#"ported" : "port",
#"porting" : "port",
#"positions" : "position",
#"posaible" : "possible",
#"possibel" : "possible",
#"possibly" : "possible",
#"posted" : "post",
#"posting" : "post",
#"pounds" : "pound",
#"powering" : "power",
#"pressed" : "press",
#"pressing" : "press",
#"presuming" : "presume",
#"previously" : "previous",
#"prices" : "price",
#"prizemy" : "prize my",
#"problems" : "problem",
#"probs" : "problem",
#"promble" : "problem",
#"proce" : "procedure",
#"proceeded" : "proceed",
#"processed" : "process",
#"processing" : "process",
#"prof" : "profile",
#"promised" : "promise",
#"promises" : "promise",
#"promoted" : "promote",
#"promotion" : "promote",
#"rpoof" : "proof",
#"properly" : "proper",
#"orovided" : "provide",
#"providing" : "provide",
#"purchased" : "purchase",
#"purchass" : "purchase",
#"purchaseaddons" : "purchase addon",
#"purposes" : "purpose",
#"putting" : "put",
#"querry" : "query",
#"querying" : "query",
#"quirey" : "query",
#"quest" : "question",
#"questions" : "question",
#"quesutons" : "question",
#"queued" : "queue",
#"quicker" : "quick",
#"quickest" : "quick",
#"quickly" : "quick",
#"quite" : "quit",
#"quoted" : "quote",
#"rais" : "raise",
#"raised" : "raise",
#"rating" : "rate",
#"reached" : "reach",
#"reaching" : "reach",
#"reactivating" : "reactivate",
#"reactivation" : "reactivate",
#"reactived" : "reactivate",
#"reading" : "read",
#"realised" : "realise",
#"realized" : "realise",
#"realy" : "really",
#"reasons" : "reason",
#"receieved" : "receive",
#"received" : "receive",
#"receiving" : "receive",
#"recieve" : "receive",
#"recieved" : "receive",
#"recieving" : "receive",
#"recive" : "receive",
#"recived" : "receive",
#"reciving" : "receive",
#"resiving" : "receive",
#"recently" : "recent",
#"recognise" : "recognize",
#"recognised" : "recognize",
#"recognising" : "recognize",
#"recognition" : "recognize",
#"recognized" : "recognize",
#"recommended" : "recommend",
#"reconnected" : "reconnect",
#"recorded" : "record",
#"records" : "record",
#"recovering" : "recover",
#"recovery" : "recover",
#"rectified" : "rectify",
#"reduced" : "reduce",
#"reducing" : "reduce",
#"ref" : "refer",
#"referal" : "refer",
#"referance" : "refer",
#"reference" : "refer",
#"refernce" : "refer",
#"referral" : "refer",
#"referred" : "refer",
#"refferal" : "refer",
#"reffered" : "refer",
#"refreshes" : "refresh",
#"refunded" : "refund",
#"refurbished" : "refurbish",
#"refused" : "refuse",
#"reg" : "regarding",
#"registered" : "register",
#"registering" : "register",
#"registration" : "register",
#"registrer" : "register",
#"regualr" : "regular",
#"regularly" : "regular",
#"reinstalled" : "reinstall",
#"reinstalling" : "reinstall",
#"reinstated" : "reinstate",
#"reinstating" : "reinstate",
#"rejected" : "reject",
#"relacion" : "relation",
#"relaions" : "relation",
#"related" : "relation",
#"relating" : "relation",
#"relations" : "relation",
#"relationship" : "relation",
#"relative" : "relation",
#"released" : "realize",
#"releasing" : "realize",
#"reliablity" : "reliable",
#"reliably" : "reliable",
#"remaining" : "remain",
#"removal" : "remove",
#"removed" : "remove",
#"removing" : "remove",
#"renewal" : "renew",
#"renewals" : "renew",
#"renewed" : "renew",
#"renewel" : "renew",
#"renewing" : "renew",
#"renews" : "renew",
#"repaired" : "repair",
#"repairs" : "repair",
#"replaced" : "replace",
#"replacement" : "replace",
#"replacements" : "replace",
#"replacing" : "replace",
#"replacment" : "replace",
#"replied" : "reply",
#"reported" : "report",
#"reporting" : "report",
#"reports" : "report",
#"representatives" : "representative",
#"requested" : "request",
#"requesting" : "request",
#"requset" : "request",
#"rewuest" : "request",
#"required" : "require",
#"resetting" : "reset",
#"resolved" : "resolve",
#"resolver" : "resolve",
#"respond" : "response",
#"responders" : "response",
#"restarted" : "restart",
#"restored" : "restore",
#"restiction" : "restrict",
#"restricted" : "restrict",
#"restricting" : "restrict",
#"restriction" : "restrict",
#"restrictions" : "restrict",
#"results" : "result",
#"retrieved" : "retrieve",
#"retun" : "return",
#"returned" : "return",
#"reveived" : "review",
#"reveiving" : "review",
#"reviewed" : "review",
#"revords" : "rewards",
#"rget" : "get",
#"rhat" : "that",
#"rhe" : "the",
#"rhis" : "this",
#"ridic" : "ridiculous",
#"ridiculously" : "ridiculous",
#"rediculously" : "ridiculously",
#"rights" : "right",
#"ringing" : "ring",
#"rings" : "ring",
#"roamed" : "roam",
#"roaming" : "roam",
#"roming" : "roam",
#"rolling" : "roll",
#"roughly" : "rough",
#"routers" : "router",
#"rudely" : "rude",
#"ran" : "run",
#"running" : "run",
#"runs" : "run",
#"sadly" : "sad",
#"sales" : "sale",
#"sat" : "saturday",
#"satisfaction" : "satisfy",
#"satisfactory" : "satisfy",
#"saved" : "save",
#"saver" : "save",
#"saves" : "save",
#"savings" : "save",
#"savingexpert" : "save expert",
#"sayin" : "say",
#"saying" : "say",
#"says" : "say",
#"scammers" : "scam",
#"scaner" : "scan",
#"scared" : "scare",
#"scheduled" : "schedule",
#"sec" : "second",
#"seccond" : "second",
#"seeking" : "seek",
#"seemed" : "seem",
#"seems" : "seem",
#"selected" : "select",
#"selling" : "sell",
#"sending" : "send",
#"senthil" : "sent hil",
#"sep" : "seperate",
#"separately" : "seperate",
#"sept" : "september",
#"seriously" : "serious",
#"served" : "serve",
#"services" : "service",
#"sevices" : "service",
#"settings" : "setting",
#"settled" : "settle",
#"settling" : "settle",
#"sharing" : "share",
#"shifting" : "shift",
#"shocked" : "shock",
#"shocking" : "shock",
#"shops" : "shop",
#"shopsafe" : "shop safe",
#"ahould" : "should",
#"sholud" : "should",
#"shouldnt" : "should not",
#"showed" : "show",
#"showing" : "show",
#"shown" : "show",
#"shows" : "show",
#"signals" : "signal",
#"sognal" : "signal",
#"silvio" : "silver",
#"sime" : "sim",
#"sims" : "sim",
#"simcard" : "sim card",
#"similiar" : "similar",
#"simply" : "simple",
#"sites" : "site",
#"sittes" : "site",
#"sized" : "size",
#"slightly" : "slight",
#"slower" : "slow",
#"sloly" : "slow",
#"smaller" : "small",
#"smashed" : "smash",
#"solved" : "solve",
#"sone" : "some",
#"somthing" : "something",
#"somwthing" : "something",
#"sometimes" : "sometime",
#"sons" : "son",
#"sooner" : "soon",
#"sorted" : "sort",
#"sorting" : "sort",
#"sorts" : "sort",
#"sorty" : "sort",
#"sounds" : "sound",
#"speake" : "speak",
#"speaker" : "speak",
#"speaking" : "speak",
#"specifically" : "specific",
#"speeds" : "speed",
#"spending" : "spend",
#"spent" : "spend",
#"spewd" : "spend",
#"spnding" : "spend",
#"spoken" : "spoke",
#"spotify" : "spot",
#"standing" : "stand",
#"started" : "start",
#"starting" : "start",
#"starts" : "start",
#"statement" : "stated",
#"states" : "stated",
#"stating" : "stated",
#"stayed" : "stay",
#"staying" : "stay",
#"stays" : "stay",
#"stephen" : "steph",
#"stocks" : "stock",
#"stopped" : "stop",
#"stopping" : "stop",
#"stores" : "store",
#"streaming" : "stream",
#"streams" : "stream",
#"struggling" : "struggle",
#"stuggling" : "struggle",
#"stupidly" : "stupid",
#"sub" : "subject",
#"submitted" : "submit",
#"subscribed" : "subscribe",
#"successful" : "success",
#"suddenly" : "sudden",
#"suffers" : "suffer",
#"suggestion" : "suggest",
#"suggests" : "suggest",
#"suiting" : "suit",
#"supplied" : "supply",
#"supplier" : "supply",
#"suppirt" : "support",
#"supported" : "support",
#"supposed" : "suppose",
#"suspended" : "suspend",
#"suspending" : "suspend",
#"swalloed" : "swallowed",
#"swapped" : "swap",
#"swapping" : "swap",
#"swich" : "switch",
#"switched" : "switch",
#"switches" : "switch",
#"switching" : "switch",
#"systems" : "system",
#"taje" : "take",
#"taken" : "take",
#"takes" : "take",
#"taking" : "take",
#"talked" : "talk",
#"talkin" : "talk",
#"talking" : "talk",
#"tariffs" : "tariff",
#"tarrif" : "tariff",
#"tbat" : "that",
#"telling" : "tell",
#"tells" : "tell",
#"temporarily" : "temporary",
#"teneife" : "tenerife",
#"teeminate" : "terminate",
#"terminated" : "terminate",
#"terminating" : "terminate",
#"terms" : "term",
#"tested" : "test",
#"txt" : "text",
#"txtd" : "text",
#"txts" : "text",
#"texted" : "text",
#"texting" : "text",
#"texts" : "text",
#"thaks" : "thank",
#"thanks" : "thank",
#"thankss" : "thank",
#"thankyou" : "thank",
#"thanms" : "thank",
#"thanx" : "thank",
#"thax" : "thank",
#"thnks" : "thank",
#"thx" : "thank",
#"tjanks" : "thank",
#"tnx" : "thank",
#"thati" : "that",
#"thats" : "that",
#"tjat" : "that",
#"tge" : "the",
#"tje" : "the",
#"thsy" : "they",
#"things" : "thing",
#"thinkshe" : "think",
#"thinking" : "think",
#"thinks" : "think",
#"threes" : "three",
#"throughdo" : "through",
#"thru" : "through",
#"througout" : "throughout",
#"thurs" : "Thursday",
#"til" : "till",
#"tild" : "tilled",
#"timethe" : "time the",
#"timesboth" : "times both",
#"tll" : "till",
#"tiday" : "today",
#"todays" : "today",
#"todo" : "to do ",
#"tommorow" : "tomorrow",
#"tomorrows" : "tomorrow",
#"tookout" : "took out",
#"topcashback" : "top cashback",
#"toped" : "topped",
#"totally" : "total",
#"trackable" : "track",
#"tracked" : "track",
#"tracking" : "track",
#"traffif" : "traffic",
#"transactions" : "transaction",
#"trabsfer" : "transfer",
#"transfered" : "transfer",
#"transferred" : "transfer",
#"transferring" : "transfer",
#"traveled" : "travel",
#"traveling" : "travel",
#"travelling" : "travel",
#"troubles" : "trouble",
#"tryed" : "try",
#"trying" : "try",
#"turned" : "turn",
#"turning" : "turn",
#"turns" : "turn",
#"typed" : "type",
#"typing" : "type",
#"unacceptablethat" : "unacceptable that",
#"undersatnd" : "understand",
#"understanding" : "understand",
#"understood" : "understand",
#"unfairly" : "unfair",
#"unfortunatly" : "unfortunately",
#"unlimitet" : "unlimited",
#"unlimted" : "unlimited",
#"unlocked" : "unlock",
#"unnecessory" : "unnecessary",
#"unitl" : "until",
#"untill" : "until",
#"updated" : "update",
#"updates" : "update",
#"updating" : "update",
#"upgdade" : "upgrade",
#"uograde" : "upgrade",
#"upgarde" : "upgrade",
#"upgrafed" : "upgrade",
#"upgrate" : "upgrade",
#"upgrdaed" : "upgrade",
#"upgrde" : "upgrade",
#"upgraded" : "upgrade",
#"upgrades" : "upgrade",
#"upgrading" : "upgrade",
#"uploaded" : "upload",
#"upnp" : "upon",
#"urgently" : "urgent",
#"uage" : "usage",
#"usuage" : "usage",
#"useage" : "use",
#"used" : "use",
#"usseless" : "useless",
#"users" : "user",
#"uses" : "use",
#"using" : "use",
#"usually" : "usual",
#"valuable" : "value",
#"valued" : "value",
#"veda" : "vedea",
#"verification" : "verify",
#"verified" : "verify",
#"vede" : "video",
#"vedios" : "video",
#"visiting" : "visit",
#"vodaphone" : "vodafone",
#"vocimails" : "voice mail",
#"voicemail" : "voice mail",
#"voicemails" : "voice mail",
#"waited" : "wait",
#"waiting" : "wait",
#"waivered" : "waived",
#"waiving" : "waived",
#"wand" : "want",
#"wanna" : "want",
#"wanted" : "want",
#"wanting" : "want",
#"wants" : "want",
#"wany" : "want",
#"warned" : "warn",
#"warning" : "warn",
#"wasing" : "washing",
#"wasnt" : "was not",
#"websites" : "website",
#"wed" : "wednesday",
#"weekly" : "week",
#"weeks" : "week",
#"werent" : "were not",
#"whant" : "what",
#"whates" : "what",
#"whst" : "what",
#"whats" : "what is",
#"whassap" : "whatsapp",
#"whens" : "when is",
#"whereas" : "where as",
#"wheres" : "where is",
#"whist" : "whilst",
#"whit" : "white",
#"swhy" : "why",
#"wint" : "winter",
#"wirh" : "with",
#"wifes" : "wife",
#"wondered" : "wonder",
#"wondering" : "wonder",
#"woundering" : "wonder",
#"wont" : "would not",
#"warking" : "work",
#"wirk" : "work",
#"worked" : "work",
#"working" : "work",
#"works" : "work",
#"worried" : "worry",
#"worries" : "worry",
#"worrying" : "worry",
#"worst" : "worse",
#"woukd" : "would",
#"woulf" : "would",
#"woulld" : "would",
#"woupd" : "would",
#"wouldlike" : "would like",
#"wouldnt" : "would not",
#"writes" : "write",
#"writing" : "write",
#"written" : "write",
#"wrongly" : "wrong",
#"wrote" : "write",
#"years" : "year",
#"yea" : "yes",
#"yeah" : "yes",
#"year" : "yes",
#"yed" : "yes",
#"yeh" : "yes",
#"yeaterday" : "yesterday",
#"yep" : "yesterday",
#"yesterdays" : "yesterday",
#"yesyerday" : "yesterday",
#"yiu" : "you",
#"yoi" : "you",
#"yous" : "you",
#"youre" : "you are",
#"yougo" : "you go",
#"youve" : "you have",
#"youll" : "you will",
#"youd" : "you would",
#"yours" : "your",
#"yourselves" : "yourself",
#"youselves" : "yourself",
#"yousue" : "youtube", "£" : "currency" ,"earlyupgrade" : "early upgrade",
# "£" : "currency_money"   }
#
#
#neg_pattern = re.compile(r'\b(' + '|'.join(negations_dic.keys()) + r')\b') 
#
#
#clean_chats = []
#for t in raw_data['tidy_text_1']:
#    t = neg_pattern.sub(lambda x: negations_dic[x.group()], t)
#    clean_chats.append(t)
#
## Putting the clean tweets in separate column "tidy_tweet" 
## clean_chats
#
#raw_data['tidy_text_1'] = pd.DataFrame(clean_chats,columns = ['tidy_text_1'])
#




###----------------------------------------------------------   DATA CLEANING



raw_data = raw_data.astype(str)


# Removal of ASCII, Punctuation, special character, @user_id, hyperlinks, numbers. Special case not handled previously,      
raw_data['tidy_text_1'] = raw_data['tidy_text_1'].str.replace("(@[A-Za-z0-9]+)|([^0-9A-Za-z \t])|(\w+:\/\/\S+)|[\.\,\!\?\:\;\-\=]|\x92|[0-9]+|www.[^ ]+|[^\x00-\x7F]+|\d+", ' ')
raw_data['tidy_text_1'] = raw_data['tidy_text_1'].apply(lambda x: re.sub('  ', ' ', x))


## removing the word's with length less than 2
raw_data['tidy_text_1'] = raw_data['tidy_text_1'].apply(lambda x: ' '.join([w for w in x.split() if len(w)>1]))
raw_data['tidy_text_1'] = raw_data['tidy_text_1'].apply(lambda x: re.sub('  ', ' ', x))



###------------------------------   STOPWORDS


from nltk.corpus import stopwords
stop = stopwords.words('english')
#raw_data['tidy_text_1'] = raw_data['tidy_text_1'].apply(lambda x: " ".join(x for x in x.split() if x not in stop))

DATASET_ENCODING = "ISO-8859-1"
excel = r"D:\sentiment analysis\Customer_Chat_Model\model_BOW.xlsx"
dataframe = pd.read_excel(excel,sheet_name='stop_words',encoding = DATASET_ENCODING)
data_to_string = pd.DataFrame.to_string(dataframe) 

raw_data['tidy_text_1'] = raw_data['tidy_text_1'].apply(lambda x: " ".join(x for x in x.split() if x not in data_to_string))

raw_data = raw_data[raw_data['tidy_text_1'] != ""]

raw_data['tidy_text_1'] = raw_data['tidy_text_1'].apply(lambda x: x.strip())


raw_data['bag_of_words'] = raw_data.tidy_text_1.str.strip().str.split('[\W_]+')
raw_data["text_length"] = raw_data["tidy_text_1"].str.split().str.len()
raw_data = raw_data.astype(str)


print("Number of unique chat sessions are : {}".format(raw_data.chat_id.nunique()))
#   Number of unique chat sessions are : 3110

raw_data = raw_data[raw_data['tidy_text_1'].notnull()]

print(issue_labels.issue)
issue_labels.issue[1]

len(np.unique(raw_data["issue_label"]))


# encoding the labels of the data into integer type from 0 to 12 for our 13 labels.


raw_data['model_issue'] = raw_data['issue_label']


raw_data['model_issue'].replace(issue_labels.issue[0],0,inplace=True)
raw_data['model_issue'].replace(issue_labels.issue[1],1,inplace=True)
raw_data['model_issue'].replace(issue_labels.issue[2],2,inplace=True)
raw_data['model_issue'].replace(issue_labels.issue[3],3,inplace=True)
raw_data['model_issue'].replace(issue_labels.issue[4],4,inplace=True)
raw_data['model_issue'].replace(issue_labels.issue[5],5,inplace=True)
raw_data['model_issue'].replace(issue_labels.issue[6],6,inplace=True)
raw_data['model_issue'].replace(issue_labels.issue[7],7,inplace=True)
raw_data['model_issue'].replace(issue_labels.issue[8],8,inplace=True)
raw_data['model_issue'].replace(issue_labels.issue[9],9,inplace=True)
raw_data['model_issue'].replace(issue_labels.issue[10],10,inplace=True)
raw_data['model_issue'].replace(issue_labels.issue[11],11,inplace=True)
raw_data['model_issue'].replace(issue_labels.issue[12],12,inplace=True)
raw_data['model_issue'].replace(issue_labels.issue[13],13,inplace=True)
raw_data['model_issue'].replace(issue_labels.issue[14],14,inplace=True)
raw_data['model_issue'].replace(issue_labels.issue[15],15,inplace=True)
raw_data['model_issue'].replace(issue_labels.issue[16],16,inplace=True)
raw_data['model_issue'].replace(issue_labels.issue[17],17,inplace=True)
raw_data['model_issue'].replace(issue_labels.issue[18],18,inplace=True)
raw_data['model_issue'].replace(issue_labels.issue[19],19,inplace=True)
raw_data['model_issue'].replace(issue_labels.issue[20],20,inplace=True)
raw_data['model_issue'].replace(issue_labels.issue[21],21,inplace=True)
raw_data['model_issue'].replace(issue_labels.issue[22],22,inplace=True)
raw_data['model_issue'].replace(issue_labels.issue[23],23,inplace=True)
#raw_data['model_issue'].replace(issue_labels.issue[24],24,inplace=True)


# 
np.unique(raw_data["model_issue"])
len(np.unique(raw_data["model_issue"]))


# "label" has all labels of the data
label= raw_data['model_issue'].values
len(np.unique(label))

# converting the lables into categorical form for the model building.
from keras.utils import np_utils
Y = np_utils.to_categorical(label)
print(Y)


# defining the parameters of model.

max_len= 50
max_features= 20000

# creating vectorized corpus and padding
tokenizer = Tokenizer(num_words= max_features)

tokenizer.fit_on_texts(raw_data['tidy_text_1'].values)


# storing the "tokenizer" in the system for the future use at different system
with open('D:\\sentiment analysis\\Customer_Chat_Model\\Chat_Data\\tokenizer_1.pickle', 'wb') as handle:
    pickle.dump(tokenizer, handle, protocol=pickle.HIGHEST_PROTOCOL)                


X = pad_sequences(tokenizer.texts_to_sequences(raw_data['tidy_text_1'].values), maxlen = max_len)


# train test split
X_train, X_test, y_train, y_test = train_test_split(X,Y, test_size=0.3, random_state=77, stratify=Y)



model= Sequential()
model.add(Embedding(max_features,100,mask_zero=True))
model.add(LSTM(200,dropout=0.3,recurrent_dropout=0.3,return_sequences=False))
model.add(Dense(24, activation='softmax'))
model.compile(loss = 'categorical_crossentropy', optimizer='adam',metrics = ['accuracy'])
model.summary()


callback = [EarlyStopping(monitor='val_loss', patience=2), ModelCheckpoint(filepath='D:\\sentiment analysis\\Customer_Chat_Model\\BestModel_1.h5', monitor='val_loss', save_best_only=True)]
baseline_history = model.fit(X_train, y_train, batch_size= 8, epochs= 20, callbacks=callback ,validation_data=(X_test, y_test))


accr = model.evaluate(X_test,y_test)


print('Test set\n Loss: {:0.3f}\n Accuracy: {:0.3f}'.format(accr[0],accr[1]))




##=============================================================================

#predicted_label  = model.predict(X_test)
#test_labels = issue_labels.issue
#
#y=[]
#for pre in predicted_label:
#    x = test_labels[np.argmax(pre)]
##    print(x)
#    y.append(x)
#        



##=============================  PREDICTION CODE  =============================



text = ["no not at all been in the uk no worries"]

test = pad_sequences(tokenizer.texts_to_sequences(text), maxlen=max_len)
print(test)
pred = model.predict(test)
#print(pred)
np.argmax(pred)


labels = issue_labels.issue

print(pred, labels[np.argmax(pred)])













fetched_tweets_1 = pd.read_excel(r'D:\sentiment analysis\Customer_Chat_Model\testing_data_01.xlsx')
#fetched_tweets = fetched_tweets_1[:100]   
fetched_tweets_1.columns 
#   ['chat_id', 'testing_text']
fetched_tweets = fetched_tweets_1[:100]


from keras.models import load_model

model_test = load_model(r'D:\sentiment analysis\Customer_Chat_Model\BestModel_7k.h5')
with open(r'D:\sentiment analysis\Customer_Chat_Model\tokenizer_7k.pickle', 'rb') as handle:
    tok = pickle.load(handle)



fetched_tweets = fetched_tweets.astype(str)

# Removal of ASCII, Punctuation, special character, @user_id, hyperlinks, numbers. Special case not handled previously,      
fetched_tweets['tidy_text_1'] = fetched_tweets['testing_text'].str.replace("(@[A-Za-z0-9]+)|([^0-9A-Za-z \t])|(\w+:\/\/\S+)|[\.\,\!\?\:\;\-\=]|\x92|[0-9]+|www.[^ ]+|[^\x00-\x7F]+|\d+", ' ')
fetched_tweets['tidy_text_1'] = fetched_tweets['tidy_text_1'].apply(lambda x: re.sub('  ', ' ', x))


## removing the word's with length less than 2
fetched_tweets['tidy_text_1'] = fetched_tweets['tidy_text_1'].apply(lambda x: ' '.join([w for w in x.split() if len(w)>1]))
fetched_tweets['tidy_text_1'] = fetched_tweets['tidy_text_1'].apply(lambda x: re.sub('  ', ' ', x))



from nltk.corpus import stopwords
stop = stopwords.words('english')
#raw_data['tidy_text_1'] = raw_data['tidy_text_1'].apply(lambda x: " ".join(x for x in x.split() if x not in stop))


DATASET_ENCODING = "ISO-8859-1"
excel = r"D:\sentiment analysis\Customer_Chat_Model\model_BOW.xlsx"
dataframe = pd.read_excel(excel,sheet_name='stop_words',encoding = DATASET_ENCODING)
data_to_string = pd.DataFrame.to_string(dataframe) 

fetched_tweets['tidy_text_1'] = fetched_tweets['tidy_text_1'].apply(lambda x: " ".join(x for x in x.split() if x not in data_to_string))

#raw_data = raw_data[raw_data['tidy_text_1'] != ""]

fetched_tweets['tidy_text_1'] = fetched_tweets['tidy_text_1'].apply(lambda x: x.strip())

#raw_data['bag_of_words'] = raw_data.tidy_text_1.str.strip().str.split('[\W_]+')
#raw_data["text_length"] = raw_data["tidy_text_1"].str.split().str.len()

fetched_tweets = fetched_tweets.astype(str)

twt = tok.texts_to_sequences(fetched_tweets['tidy_text_1'])
print(twt)

max_len= 50
max_features= 20000

#padding the tweet to have exactly the same shape as `embedding_2` input
twt = pad_sequences(twt, maxlen= max_len, dtype='int32', value=0)
#print(twt)

predicted_label = model_test.predict(twt)

print(predicted_label[1])


predicted_label.shape
fetched_tweets.shape
issue_labels.shape

np.argmax(predicted_label[8])

test_labels = issue_labels.issue   

fetched_tweets["predict"] = ""

#fetched_tweets[:,2:3]


for i in range(len(predicted_label)):
    fetched_tweets["predict"][i] = test_labels[np.argmax(predicted_label[i])]


#test_labels[0]
#predicted_label  = model_test.predict(X_test)
#test_labels = pd.

#
#y=[]
#for pre in predicted_label:
#    x = test_labels[np.argmax(pre)]
#    y.append(x)
#
#test_data = test_data.assign(predicted_labels = y)


twt = tok.texts_to_sequences(raw_data['tidy_text_1'])
#        print(twt)
max_len= 50
max_features= 20000

#padding the tweet to have exactly the same shape as `embedding_2` input
twt = pad_sequences(twt, maxlen= max_len, dtype='int32', value=0)
#       print(twt)

sentiment = model.predict(twt)

#
test_data = test_data.astype(str)
test_data.to_excel("label_output_file.xlsx",index =False)
#
#
#
#prediction = model.predict(test_features)
#
##
##id_to_category = {0: issue_labels.issue[0],
##                  1: issue_labels.issue[0]
##                  2: issue_labels.issue[0]
##                  3: issue_labels.issue[0]
##                  4: issue_labels.issue[0]}
#
#
#for i in range(len(predicted_label)):
#    test_data.iloc[i,3] = id_to_category[predicted_label[i]]
#
##
##
##from sklearn.metrics import classification_report
##
##y_pred = model.predict(X_test, batch_size=8, verbose=1)
##y_test_pred = np.argmax(y_pred, axis=1)
##
##
### from categorial to lable indexing
##y_test_true = y_test.argmax(1)
##target_names = test_labels
##print(classification_report(y_test_true, y_test_pred,target_names=target_names))
##
##
##from sklearn.metrics import confusion_matrix 
##cm = confusion_matrix(y_test_true, y_test_pred, labels= [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24])
##print(cm)
##
##
##
##recall = np.diag(cm) / np.sum(cm, axis = 1)
##precision = np.diag(cm) / np.sum(cm, axis = 0)
##
##
##print("Recall :",round(np.mean(recall),2))
##print("Precision :",round(np.mean(precision),2))
##F1_Score = 2 * (precision * recall) / (precision + recall)
##print("F1 Score :",round(np.mean(F1_Score),2))
##
