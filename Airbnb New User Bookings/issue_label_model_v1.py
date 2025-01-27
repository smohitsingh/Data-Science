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




######   encoding the correct spelling for certain words 
negations_dic = {
        "ain't":"is not","amn't":"am not","aren't":"are not","'cause":"because",        "couldn't":"could not","couldn't've":"could not have","could've":"could have","daren't":"dare not","daresn't":"dare not",
        "dasn't":"dare not","didn't":"did not","doesn't":"does not","don't":"do not","e'er":"ever",        "em":"them","everyone's":"everyone is","finna":"fixing to","gimme":"give me",
        "gonna":"going to","gon't":"go not","gotta":"got to","hadn't":"had not","hasn't":"has not",        "haven't":"have not","he'd":"he would","he'll":"he will","he's":"he is","he've":"he have","how'd":"how would",
        "how'll":"how will","how're":"how are","how's":"how is","i'd":"i would",        "i'll":"i will","i'm":"i am","i'm'a":"i am about to","i'm'o":"i am going to",
        "isn't":"is not","it'd":"it would","it'll":"it will","it's":"it is","its":"it is","i've":"i have",        "kinda":"kind of","let's":"let us","mayn't":"may not","may've":"may have","mightn't":"might not",
        "might've":"might have","mustn't":"must not","mustn't've":"must not have",        "must've":"must have","needn't":"need not","needn":"need not","ne'er":"never","o'":"of",
        "o'er":"over","ol'":"old","oughtn't":"ought not","shalln't":"shall not","shan't":"shall not",        "she'd":"she would","she'll":"she will","she's":"she is","shouldn't":"should not","shouldn":"should not",
        "shouldn't've":"should not have","should've":"should have","somebody's":"somebody is",        "someone's":"someone is","something's":"something is","that'd":"that would","that'll":"that will",
        "that're":"that are","that's":"that is","there'd":"there would",        "there'll":"there will","there're":"there are","there's":"there is",
        "these're":"these are","they'd":"they would","they'll":"they will","they're":"they are",        "they've":"they have","this's":"this is","those're":"those are","'tis":"it is",
        "'its":"it is","'twas":"it was","wanna":"want to","wasn't":"was not",        "wasn":"was not","we'd":"we would","we'd've":"we would have","we'll":"we will","we're":"we are","weren't":"were not",
        "we've":"we have","what'd":"what did","what'll":"what will","what're":"what are",        "what's":"what is","what've":"what have","when's":"when is","where'd":"where did",
        "where're":"where are","where's":"where is","where've":"where have","which's":"which is",        "who'd":"who would","who'd've":"who would have","who'll":"who will",
        "who're":"who are","who's":"who is","who've":"who have","why'd":"why did",        "why're":"why are","why's":"why is","won't":"will not","wouldn't":"would not",
        "would've":"would have","y'all":"you all","you'd":"you would","you'll":"you will",        "you're":"you are","you've":"you have","Whatcha":"What are you","luv":"love","sux":"sucks","isn't":"is not", 
        "aren't":"are not", "wasn't":"was not", "weren't":"were not","haven't":"have not","hasn't":"has not",        "hadn't":"had not","wouldn't":"would not", "don't":"do not","doesn't":"does not","didn't":"did not","can't":"cannot",
        "can not":"cannot","couldn't":"could not","shouldn't":"should not",        "mightn't":"might not","mustn't":"must not","wouldn":"would not","won't":"would not","weren":"were not",
	"abored : aboard","abour : about","abroard : abroad","abroat : abroad","abroud : abroad","absolulty : absolute",	"absolutely : absolute","absorbing : absorb","acceptable : accept","accepted : accept","accepting : accept",
	"acces : access","accedeint : accident","accidentally : accident","accont : account","accounts : account","accout : account","qccount : account",	"activated : activate","activateit : activate","activating : activate","activation : activate",	"activiated : activate","activ : active","adding : added","additionally : additional",
	"additonal : additional","addons : addon","addresses : address","adress : address","admitted : admit",	"addult : adult","adukt : adult","adul : adult","adults : adult","advanced : advance","advertised : advertise","advertises : advertise","adverts : advertise",
	"advice : advise","advicer : advise","advised : advise","adviser : advise","advisor : advise","advisors : advise","affected : affect",	"affecting : affect","affordable : afford","afrod : afford","afterwards : afterward","ahain : again","agains : against""ages : age","agents : agent",
	"agoi : ago","agreed : agree""alllow : allow","allowed : allow","allowing : allow","allows : allow","allowances : allowance","allowence : allowance",	"allownce : allowance","allways : always","alves : always","alwsys : always","amgetting : am getting","ammont : amount","amounts : amount","snd : and",
	"annoyed : annoy","annoying : annoy","ans : answer","answers : answer","apologies : apology","apologised : apology","apologises : apology","apps : app",	"appwhen : app when","appareantly : apparently","apperently : apparently","appeared : appear","appearing : appear",
	"appears : appear","happeared : appeared","aplied : applied","applied : apply",	"appreciated : appreciate","aprroved : approved","approx : approximately",
	"aint : are not","arent : are not","arnt : are not","areangment : arrangement",	"arrange : arrangement","arranged : arrangement","arrangeme : arrangement","arrangemements : arrangement",
	"arrangment : arrangement","arrangments : arrangement","arrnagement : arrangement","arrived : arrive",	"arrives : arrive","arriving : arrive","aswel : as well","aswell : as well","asked : ask","asking : ask""asks : ask","assumed : assume",
	"assuming : assume","attached : attach","attax : attach","attempted : attempt","attempting : attempt","attempts : attempt",	"aug : august","augusts : august","australian : australia","authorised : authorize","auto : automatic","automated : automatic",
	"automatically : automatic","availability : available","availabke : available","availible : available","avilable : available",	"bck : back","backdate : back date","backpacking : back pack","badly : bad","baffled : baffle","balamce : balance","ballace : balance","banking : bank",
	"bars : bar","basucally : basically","becasue : because","becuase : because","bevause : because","bez : because","becoming : become","begging : beg","began : begin","begining : begin","beginning : begin","begins : begin",
"begum : begin","beeing : being","belive : believe","benefits : benefit","bigger : big","biils : bill","bilk : bill","billed : bill","billing : bill","billl : bill","bills : bill","tbing : bing","birthday : birth","birthwas : birth",
"bites : bit","blk : block","blocke : block","blocked : block","blocker : block","blockes : block","blocking : block","bloked : block","bloking : block","boos : boost","booster : boost","booted : boost","bottles : bottle","bounced : bounce","bounces : bounce","brinda : bring",
"bringing : bring","beoadband : broadband","butnon : button","buttons : button","buxtons : button","bough : buy","bought : buy","bougjt : buy","buying : buy","cahnge : change","called : call","calling : call","calls : call","canadian : canada","cancelation : cancel","canceled : cancel",
"cancell : cancel","cancellation : cancel","cancelled : cancel","cancelling : cancel","cancle : cancel","candad : cancel","canelled : cancel","cannot : can not","canon : can not","cant : can not","cantect : contact","cantmake : can not make",
"capped : cap","capping : cap","cards : card","careful : care","carefully : care","carnt : can not","carried : carry","carrier : carry","cashback : cash","caused : cause","causes : cause","causing : cause","celuar : cellular","certainly : certain","cewo : ceo","chages : change",
"changed : change","changes : change","changing : change","changw : change","chargeable : charge","charged : charge","chargers : charge","charges : charge","charging : charge","chargw : charge","charring : charge","xharges : charge","chasing : chase","chating : chat","chatline : chat","chats : chat","chatted : chat","chatting : chat","chcek : check","cheaper : cheap","cheapest : cheap","checked : check",
"checking : check","checks : check","cheek : check","chenge : change","chose : choose","chosen : choose","claimed : claim","claiming : claim","clainmimg : claim","cleaning : clean","cleared : clear","clearly : clear","clicked : click","clients : client","closed : close","closes : close","cobtract : contract","codes : code","colegue : colleague","colleage : colleague","colleagues : colleague","collected : collect","collection : collect","collections : collect",
"collectors : collect","colleuges : colleague","comection : connect","comes : come","comfirmation : comfirm","coming : come","communications : communication","companies : company","companys : company","compares : compare","comparing : compare","compatable : compatible","compensated : compensate",
"compensation : compensate","competence : competition","competitions : competition","complain : complaint","complained : coplaint","complaining : complaint","complaints : complaint","compleatly : complete",
"completed : complete","completely : complete","compliant : complaint","concerned : concern","cone : come","confirmation : confirm",
"confirmed : confirm",
"confirmig : confirm",
"confirming : confirm",
"confused : confuse",
"confusion : confuse",
"conmection : connect",
"connected : connect",
"connecting : connect",
"connection : connect",
"connections : connect",
"conned : connect",
"conntact : contact",
"consideration : consider",
"considering : consider",
"constantly : constant",
"contacted : contact",
"contacting : contact",
"contacts : contact",
"contect : contact",
"continued : continue",
"continues : continue",
"contracta : contract",
"contracted : contract",
"contractes : contract",
"contracts : contract",
"controle : control",
"conversations : conversation",
"copied : copy",
"correctly : correct",
"cos : cost",
"costa : cost",
"costing : cost",
"costs : cost",
"coukd : could",
"couldnt : could not",
"countries : country",
"covered : cover",
"covers : cover",
"cracked : crack",
"cracks : crack",
"crashing : crash",
"crdit : credit",
"creadit : credit",
"credited : credit",
"crediti : credit",
"crete : create",
"crying : cry",
"csnnot : can not",
"aicubeb : cube",
"currently : current",
"customerim : customer",
"customers : customer",
"customerwhy : customer",
"customeryes : customer",
"customeryou : customer",
"custumer : customer",
"cutomer : customer",
"cuts : cut",
"cutting : cut",
"damaged : damage",
"dat : data",
"datapass : data pass",
"dated : date",
"dates : date",
"datt : date",
"daya : day",
"days : day",
"dealing : deal",
"deals : deal",
"dealt : deal",
"debited : debit",
"debits : debit",
"decided : decide",
"declined : decline",
"deducted : deduct",
"deduction : deduct",
"defiantly : definitely",
"delayed : delay",
"deleted : delete",
"delievered : deliver",
"deliveey : deliver",
"delivered : deliver",
"delivery : deliver",
"deliveted : deliver",
"deparment : department",
"depends : depend",
"dept : department",
"destinations : destination",
"detailed : detail",
"details : detail",
"devices : device",
"dialing : dial",
"dialled : dial",
"dialling : dial",
"didnt : did not",
"died : die",
"dying : die",
"differance : different",
"differances : different",
"difference : different",
"differing : different",
"diffrent : different",
"difficulties : difficult",
"difficulty : difficult",
"digits : digit",
"directly : direct",
"disabled : disable",
"disabling : disable",
"disappeared : disappear",
"dissappeared : disappear",
"disappointed : disappoint",
"disappointing : disappoint",
"disconnecting : disconnected",
"disconneted : disconnected",
"disconected : disconnected",
"discounted : discount",
"discounts : discount",
"discovered : discover",
"discussed : discuss",
"discussing : discuss",
"discuu : discuss",
"dissproportunate : disproportionate",
"disputed : dispute",
"dioont : do not",
"dont : do not",
"dony : do not",
"documents : document",
"doe : does",
"doesn : does not",
"doesnt : does not",
"doest : does not",
"dowload : download",
"downloaded : download",
"downloading : download",
"downloads : download",
"dropped : drop",
"dsactiva : deactivate",
"erlier : early",
"eary : early",
"earr : early",
"earliest : early",
"earlier : early",
"ealier : early",
"easily : easy",
"easiest : easy",
"easier : easy",
"easyjet : easy jet",
"eaten : eat",
"effective : effect",
"effected : effect",
"ither : either",
"elligible : eligible",
"eligable : eligible",
"elsewhere : else where",
"emails : email",
"emailed : email",
"emergancy : emergency",
"emargency : emergency",
"employer : employee",
"employed : employee",
"enabling : enable",
"enabled : enable",
"ends : end",
"ending : end",
"ended : end",
"enquiring : enquiry",
"enquired : enquiry",
"enquire : enquiry",
"entering : enter",
"entered : enter",
"enitre : entire",
"errors : error",
"erroneously : error",
"escalated : escalate",
"essentially : essential",
"europeit : europe",
"european : europe",
"eventhough : even though",
"events : event",
"excellence : excellent",
"exited : excited",
"expecting : expect",
"expected : expect",
"expensive : expense",
"experiencing : experience",
"expiry : expire",
"expiring : expire",
"expires : expire",
"expired : expire",
"expiration : expire",
"explanation : explain",
"explaining : explain",
"explained : explain",
"exploring : explore",
"extesion : extend",
"extentions : extend",
"extention : extend",
"extension : extend",
"extending : extend",
"extended : extend",
"extsension : extension",
"exstension : extension",
"extremly : extreme",
"extremely : extreme",
"eyes : eye",
"failed : fail",
"failing : fail",
"fairly : fair",
"faster : fast",
"fastest : fast",
"faulty : fault",
"features : feature",
"fees : fee",
"feeling : feel",
"feels : feel",
"filled : fill",
"filling : fill",
"filme : film",
"filtering : filter",
"filters : filter",
"finally : final",
"finacially : financial",
"financially : financial",
"finder : find",
"finding : find",
"finished : finish",
"finishes : finish",
"firstly : first",
"fitted : fit",
"fixable : fix",
"fixed : fix",
"followed : follow",
"following : follow",
"follwing : follow",
"forgetting : forget",
"forgot : forget",
"forgotten : forget",
"forwarded : forward",
"forwarding : forward",
"forwards : forward",
"found : find",
"freezes : freeze",
"froze : freeze",
"frozen : freeze",
"feom : from",
"frustrated : frustrate",
"frustrating : frustrate",
"frustration : frustrate",
"fustrating : frustrate",
"fucker : fuck",
"fucking : fuck",
"fully : full",
"funny : fun",
"funds : fund",
"furthermore : further",
"gadgets : gadget",
"games : game",
"gave : give",
"geting : get",
"gets : get",
"gettin : get",
"getting : get",
"hetting : get",
"gifted : gift",
"gigsupposed : gig supposed",
"gig : gigabyte",
"gigabytes : gigabyte",
"gige : gigabyte",
"girlfriends : girlfriend",
"given : give",
"gives : give",
"giving : give",
"goroam : go roam",
"gud : good",
"goole : google",
"gotnit : got",
"gotten : got",
"tgradehe : gradeup",
"greatest : great",
"gray : grey",
"guaranteed : guarantee",
"guessing : guess",
"guys : guy",
"gwt : gwf",
"hadnt : had not",
"halfway : half",
"han : hand",
"hands : hand",
"hamdset : handset",
"handsets : handset",
"hapend : happen",
"happend : happen",
"happened : happen",
"happening : happen",
"happens : happen",
"happily : happy",
"harassed : harass",
"hardly : hard",
"hasnt : has not",
"hassle : haste",
"having : have",
"hawe : have",
"havent : have not",
"havnt : have not",
"headphones : headphone",
"hallo : hello",
"helli : hello",
"helll : hello",
"helllo : hello",
"hellohaving : hello have",
"helped : help",
"helpful : help",
"helpfull : help",
"heres : here is",
"higot : hi got",
"higher : high",
"holding : hold",
"holders : holder",
"holidays : holiday",
"homeneed : home need",
"honoured : honor",
"honorview : honor view",
"hopefully : hope",
"hoping : hope",
"hopital : hospital",
"hospita : hospital",
"hospitalised : hospital",
"hosptal : hospital",
"hotspots : hotspot",
"hotspotcan : hotspot can",
"hours : hour",
"hrs : hour",
"houses : house",
"housre : house",
"howeer : however",
"hrsdue : hrs due",
"huawai : huawei",
"huwawei : huawei",
"husbands : husband",
"ihave : I have",
"ive : I have",
"lve : I have",
"iwona : I want to ",
"ideas : idea",
"ignored : ignore",
"illness : ill",
"illnesses : ill",
"unlitetet : illiterate",
"imerror : im error",
"imgood : im good",
"immediately : immediate",
"importance : important",
"improvement : improve",
"included : include",
"includes : include",
"including : include",
"inconvienience : inconvenience",
"increased : increase",
"increasing : increase",
"incredibly : incredible",
"incured : incur",
"incurred : incur",
"incurring : incur",
"indicated : indicate",
"indication : indicate",
"indoors : indoor",
"informed : inform",
"informing : inform",
"info : information",
"initially : initial",
"inquiring : inquiry",
"inserted : insert",
"installed : install",
"installment : install",
"installments : install",
"instructions : instruction",
"insurnace : insurance",
"intresting : interesting",
"internatinal : international",
"internationally : international",
"btinternet : internet",
"interbet : internet",
"investigated : investigate",
"investigation : investigate",
"invoices : invoice",
"ipadshow : ipad show",
"isit : is it",
"isnt : is not",
"issued : issue",
"issuer : issue",
"issues : issue",
"its : it is",
"itlast : it last",
"itplease : it please",
"itemised : item",
"jobs : job",
"jon : job",
"joined : join",
"joining : join",
"jul : july",
"juli : july",
"julys : july",
"jumped : jump",
"jun : june",
"junes : june",
"jus : just",
"keeping : keep",
"keeps : keep",
"kept : keep",
"kind : kindly",
"kinda : kindly",
"knowing : know",
"known : know",
"knw : know",
"landing : landed",
"lately : late",
"lateri : later",
"laughing : laugh",
"leaving : leave",
"lelf : left",
"letting : let",
"letters : letter",
"lies : lie",
"lifted : lift",
"limited : limit",
"limiter : limit",
"lmit : limit",
"lineas : line",
"lines : line",
"linked : link",
"links : link",
"living : live",
"loading : load",
"loads : load",
"locked : lock",
"logged : lodge",
"logging : lodge",
"logs : log",
"lols : lol",
"longer : long",
"longstanding : long standing",
"looked : look",
"looking : look",
"looks : look",
"loose : lose",
"losing : lose",
"lost : loss",
"lots : lot",
"lower : low",
"loyality : loyal",
"mfourtune : m fortune",
"mailed : mail",
"maintinance : maintenance",
"making : make",
"makes : make",
"manager : manage",
"managed : manage",
"marks : mark",
"matching : match",
"mates : mate",
"materialise : materialize",
"max : maximum",
"maw : maximum",
"maybe : may be",
"meknow : me know",
"meant : mean",
"means : mean",
"ment : meant",
"members : member",
"mentions : mention",
"mentioning : mention",
"mentioned : mention",
"messed : mess",
"imessages : message",
"msssage : message",
"messgaes : message",
"messaging : message",
"messagethe : message",
"messages : message",
"messaged : message",
"midnight : mid night",
"mid : middle",
"middleton : middle ton",
"myfi : mifi",
"min : minimum",
"mim : minimum",
"minutes : minute",
"minuets : minute",
"minuet : minute",
"mins : minute",
"miniutes : minute",
"missing : miss",
"missed : miss",
"mistakes : mistake",
"mobiles : mobile",
"mob : mobile",
"mobilewifi : mobile wifi",
"moments : moment",
"mtonhs : month",
"montly : month",
"months : month",
"monthly : month",
"monthlt : month",
"montgly : month",
"montb : month",
"monta : month",
"mons : month",
"monrhly : month",
"momth : month",
"mobth : month",
"morenotice : more notice",
"morningi : morning",
"moving : move",
"moved : move",
"movies : movie",
"muxh : much",
"mich : much",
"nuch : much",
"mums : mum",
"names : name",
"nan : nano",
"nearest : near",
"nearly : near",
"nee : need",
"needed : need",
"needs : need",
"anither : neither",
"natwork : network",
"networks : network",
"netwotk : network",
"neu : new",
"newer : new",
"nights : night",
"nimber : number",
"nimbernever : number never",
"non : none",
"nooone : noon",
"normally : normal",
"normaly : normal",
"nota : not",
"note : not",
"notbe : not be",
"notshowung : not showing",
"noto : not to",
"nithing : nothing",
"noticed : notice",
"notices : notice",
"notiched : notice",
"notifications : notification",
"notified : notification",
"nuber : number",
"numbers : number",
"numerous : number",
"nunber : number",
"obliged : oblige",
"obtaining : obtain",
"obviously : obvious",
"obviusly : obvious",
"occasional : occassion",
"occasionally : occassion",
"occasions : occassion",
"occurred : occur",
"offered : offer",
"offering : offer",
"offers : offer",
"offi : office",
"older : old",
"once : one",
"ones : one",
"onns : ons",
"operators : operator",
"options : option",
"ordered : order",
"ordering : order",
"orginal : original",
"originally : original",
"othwr : other",
"oustanding : outstanding",
"outstabding : outstanding",
"purstanding : outstanding",
"oversea : overseas",
"owed : owe",
"pac : pack",
"pak : pack",
"pages : page",
"painfully : pain",
"paperwork : paper work",
"partially : partial",
"partners : partner",
"pasg : pass",
"passed : pass",
"passord : passport",
"passports : passport",
"pastdue : past due",
"pastduecredit : past due credit",
"patiently : patience",
"payd : pay",
"payed : pay",
"payg : pay",
"paying : pay",
"payinh : pay",
"pays : pay",
"payforit : pay for it",
"payfotit : pay for it",
"paym : payment",
"paymant : payment",
"payments : payment",
"perfectly : perfect",
"permanently : permanent",
"phome : phone",
"phoned : phone",
"phones : phone",
"phonesaregood : phone are good",
"phonebill : phone bill",
"phonecall : phone call",
"phoneplease : phone please",
"photos : photo",
"picked : pick",
"picking : pick",
"pickpocketed : pickpocket",
"pictures : picture",
"piss : pissed",
"pixels : pixel",
"placed : place",
"places : place",
"planned : plan",
"planning : plan",
"plans : plan",
"playlists : playlist",
"pleaae : please",
"pleas : please",
"pleaseres : please",
"pleassb : please",
"plesse : please",
"pls : please",
"plse : please",
"plz : please",
"plugged : plug",
"poind : point",
"pointtv : point tv",
"porno : porn",
"ported : port",
"porting : port",
"positions : position",
"posaible : possible",
"possibel : possible",
"possibly : possible",
"posted : post",
"posting : post",
"pounds : pound",
"powering : power",
"pressed : press",
"pressing : press",
"presuming : presume",
"previously : previous",
"prices : price",
"prizemy : prize my",
"problems : problem",
"probs : problem",
"promble : problem",
"proce : procedure",
"proceeded : proceed",
"processed : process",
"processing : process",
"prof : profile",
"promised : promise",
"promises : promise",
"promoted : promote",
"promotion : promote",
"rpoof : proof",
"properly : proper",
"orovided : provide",
"providing : provide",
"purchased : purchase",
"purchass : purchase",
"purchaseaddons : purchase addon",
"purposes : purpose",
"putting : put",
"querry : query",
"querying : query",
"quirey : query",
"quest : question",
"questions : question",
"quesutons : question",
"queued : queue",
"quicker : quick",
"quickest : quick",
"quickly : quick",
"quite : quit",
"quoted : quote",
"rais : raise",
"raised : raise",
"rating : rate",
"reached : reach",
"reaching : reach",
"reactivating : reactivate",
"reactivation : reactivate",
"reactived : reactivate",
"reading : read",
"realised : realise",
"realized : realise",
"realy : really",
"reasons : reason",
"receieved : receive",
"received : receive",
"receiving : receive",
"recieve : receive",
"recieved : receive",
"recieving : receive",
"recive : receive",
"recived : receive",
"reciving : receive",
"resiving : receive",
"recently : recent",
"recognise : recognize",
"recognised : recognize",
"recognising : recognize",
"recognition : recognize",
"recognized : recognize",
"recommended : recommend",
"reconnected : reconnect",
"recorded : record",
"records : record",
"recovering : recover",
"recovery : recover",
"rectified : rectify",
"reduced : reduce",
"reducing : reduce",
"ref : refer",
"referal : refer",
"referance : refer",
"reference : refer",
"refernce : refer",
"referral : refer",
"referred : refer",
"refferal : refer",
"reffered : refer",
"refreshes : refresh",
"refunded : refund",
"refurbished : refurbish",
"refused : refuse",
"reg : regarding",
"registered : register",
"registering : register",
"registration : register",
"registrer : register",
"regualr : regular",
"regularly : regular",
"reinstalled : reinstall",
"reinstalling : reinstall",
"reinstated : reinstate",
"reinstating : reinstate",
"rejected : reject",
"relacion : relation",
"relaions : relation",
"related : relation",
"relating : relation",
"relations : relation",
"relationship : relation",
"relative : relation",
"released : realize",
"releasing : realize",
"reliablity : reliable",
"reliably : reliable",
"remaining : remain",
"removal : remove",
"removed : remove",
"removing : remove",
"renewal : renew",
"renewals : renew",
"renewed : renew",
"renewel : renew",
"renewing : renew",
"renews : renew",
"repaired : repair",
"repairs : repair",
"replaced : replace",
"replacement : replace",
"replacements : replace",
"replacing : replace",
"replacment : replace",
"replied : reply",
"reported : report",
"reporting : report",
"reports : report",
"representatives : representative",
"requested : request",
"requesting : request",
"requset : request",
"rewuest : request",
"required : require",
"resetting : reset",
"resolved : resolve",
"resolver : resolve",
"respond : response",
"responders : response",
"restarted : restart",
"restored : restore",
"restiction : restrict",
"restricted : restrict",
"restricting : restrict",
"restriction : restrict",
"restrictions : restrict",
"results : result",
"retrieved : retrieve",
"retun : return",
"returned : return",
"reveived : review",
"reveiving : review",
"reviewed : review",
"revords : rewards",
"rget : get",
"rhat : that",
"rhe : the",
"rhis : this",
"ridic : ridiculous",
"ridiculously : ridiculous",
"rediculously : ridiculously",
"rights : right",
"ringing : ring",
"rings : ring",
"roamed : roam",
"roaming : roam",
"roming : roam",
"rolling : roll",
"roughly : rough",
"routers : router",
"rudely : rude",
"ran : run",
"running : run",
"runs : run",
"sadly : sad",
"sales : sale",
"sat : saturday",
"satisfaction : satisfy",
"satisfactory : satisfy",
"saved : save",
"saver : save",
"saves : save",
"savings : save",
"savingexpert : save expert",
"sayin : say",
"saying : say",
"says : say",
"scammers : scam",
"scaner : scan",
"scared : scare",
"scheduled : schedule",
"sec : second",
"seccond : second",
"seeking : seek",
"seemed : seem",
"seems : seem",
"selected : select",
"selling : sell",
"sending : send",
"senthil : sent hil",
"sep : seperate",
"separately : seperate",
"sept : september",
"seriously : serious",
"served : serve",
"services : service",
"sevices : service",
"settings : setting",
"settled : settle",
"settling : settle",
"sharing : share",
"shifting : shift",
"shocked : shock",
"shocking : shock",
"shops : shop",
"shopsafe : shop safe",
"ahould : should",
"sholud : should",
"shouldnt : should not",
"showed : show",
"showing : show",
"shown : show",
"shows : show",
"signals : signal",
"sognal : signal",
"silvio : silver",
"sime : sim",
"sims : sim",
"simcard : sim card",
"similiar : similar",
"simply : simple",
"sites : site",
"sittes : site",
"sized : size",
"slightly : slight",
"slower : slow",
"sloly : slow",
"smaller : small",
"smashed : smash",
"solved : solve",
"sone : some",
"somthing : something",
"somwthing : something",
"sometimes : sometime",
"sons : son",
"sooner : soon",
"sorted : sort",
"sorting : sort",
"sorts : sort",
"sorty : sort",
"sounds : sound",
"speake : speak",
"speaker : speak",
"speaking : speak",
"specifically : specific",
"speeds : speed",
"spending : spend",
"spent : spend",
"spewd : spend",
"spnding : spend",
"spoken : spoke",
"spotify : spot",
"standing : stand",
"started : start",
"starting : start",
"starts : start",
"statement : stated",
"states : stated",
"stating : stated",
"stayed : stay",
"staying : stay",
"stays : stay",
"stephen : steph",
"stocks : stock",
"stopped : stop",
"stopping : stop",
"stores : store",
"streaming : stream",
"streams : stream",
"struggling : struggle",
"stuggling : struggle",
"stupidly : stupid",
"sub : subject",
"submitted : submit",
"subscribed : subscribe",
"successful : success",
"suddenly : sudden",
"suffers : suffer",
"suggestion : suggest",
"suggests : suggest",
"suiting : suit",
"supplied : supply",
"supplier : supply",
"suppirt : support",
"supported : support",
"supposed : suppose",
"suspended : suspend",
"suspending : suspend",
"swalloed : swallowed",
"swapped : swap",
"swapping : swap",
"swich : switch",
"switched : switch",
"switches : switch",
"switching : switch",
"systems : system",
"taje : take",
"taken : take",
"takes : take",
"taking : take",
"talked : talk",
"talkin : talk",
"talking : talk",
"tariffs : tariff",
"tarrif : tariff",
"tbat : that",
"telling : tell",
"tells : tell",
"temporarily : temporary",
"teneife : tenerife",
"teeminate : terminate",
"terminated : terminate",
"terminating : terminate",
"terms : term",
"tested : test",
"txt : text",
"txtd : text",
"txts : text",
"texted : text",
"texting : text",
"texts : text",
"thaks : thank",
"thanks : thank",
"thankss : thank",
"thankyou : thank",
"thanms : thank",
"thanx : thank",
"thax : thank",
"thnks : thank",
"thx : thank",
"tjanks : thank",
"tnx : thank",
"thati : that",
"thats : that",
"tjat : that",
"tge : the",
"tje : the",
"thsy : they",
"things : thing",
"thinkshe : think",
"thinking : think",
"thinks : think",
"threes : three",
"throughdo : through",
"thru : through",
"througout : throughout",
"thurs : Thursday",
"til : till",
"tild : tilled",
"timethe : time the",
"timesboth : times both",
"tll : till",
"tiday : today",
"todays : today",
"todo : to do ",
"tommorow : tomorrow",
"tomorrows : tomorrow",
"tookout : took out",
"topcashback : top cashback",
"toped : topped",
"totally : total",
"trackable : track",
"tracked : track",
"tracking : track",
"traffif : traffic",
"transactions : transaction",
"trabsfer : transfer",
"transfered : transfer",
"transferred : transfer",
"transferring : transfer",
"traveled : travel",
"traveling : travel",
"travelling : travel",
"troubles : trouble",
"tryed : try",
"trying : try",
"turned : turn",
"turning : turn",
"turns : turn",
"typed : type",
"typing : type",
"unacceptablethat : unacceptable that",
"undersatnd : understand",
"understanding : understand",
"understood : understand",
"unfairly : unfair",
"unfortunatly : unfortunately",
"unlimitet : unlimited",
"unlimted : unlimited",
"unlocked : unlock",
"unnecessory : unnecessary",
"unitl : until",
"untill : until",
"updated : update",
"updates : update",
"updating : update",
"upgdade : upgrade",
"uograde : upgrade",
"upgarde : upgrade",
"upgrafed : upgrade",
"upgrate : upgrade",
"upgrdaed : upgrade",
"upgrde : upgrade",
"upgraded : upgrade",
"upgrades : upgrade",
"upgrading : upgrade",
"uploaded : upload",
"upnp : upon",
"urgently : urgent",
"uage : usage",
"usuage : usage",
"useage : use",
"used : use",
"usseless : useless",
"users : user",
"uses : use",
"using : use",
"usually : usual",
"valuable : value",
"valued : value",
"veda : vedea",
"verification : verify",
"verified : verify",
"vede : video",
"vedios : video",
"visiting : visit",
"vodaphone : vodafone",
"vocimails : voice mail",
"voicemail : voice mail",
"voicemails : voice mail",
"waited : wait",
"waiting : wait",
"waivered : waived",
"waiving : waived",
"wand : want",
"wanna : want",
"wanted : want",
"wanting : want",
"wants : want",
"wany : want",
"warned : warn",
"warning : warn",
"wasing : washing",
"wasnt : was not",
"websites : website",
"wed : wednesday",
"weekly : week",
"weeks : week",
"werent : were not",
"whant : what",
"whates : what",
"whst : what",
"whats : what is",
"whassap : whatsapp",
"whens : when is",
"whereas : where as",
"wheres : where is",
"whist : whilst",
"whit : white",
"swhy : why",
"wint : winter",
"wirh : with",
"wifes : wife",
"wondered : wonder",
"wondering : wonder",
"woundering : wonder",
"wont : would not",
"warking : work",
"wirk : work",
"worked : work",
"working : work",
"works : work",
"worried : worry",
"worries : worry",
"worrying : worry",
"worst : worse",
"woukd : would",
"woulf : would",
"woulld : would",
"woupd : would",
"wouldlike : would like",
"wouldnt : would not",
"writes : write",
"writing : write",
"written : write",
"wrongly : wrong",
"wrote : write",
"years : year",
"yea : yes","yeah : yes","year : yes",
"yed : yes","yeh : yes","yeaterday : yesterday",
"yep : yesterday","yesterdays : yesterday","yesyerday : yesterday",
"yiu : you","yoi : you","yous : you",
"youre : you are","yougo : you go",
"youve : you have","youll : you will",
"youd : you would","yours : your",
"yourselves : yourself","youselves : yourself",
"yousue : youtube"
  }


neg_pattern = re.compile(r'\b(' + '|'.join(negations_dic.keys()) + r')\b') 

clean_chats = []
for t in final_chat_data['tidy_text'] 
    t = neg_pattern.sub(lambda x: negations_dic[x.group()], t)
    clean_chats (t)



# Putting the clean tweets in separate column  "tidy_text"
# clean_chats 
final_chat_data['tidy_text'] = pd.DataFrame(clean_chats, columns = ['tidy_text'])



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
