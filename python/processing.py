
import re
import string
import pandas as pd
import numpy as np
from nltk.corpus import stopwords



def processing():
  
  def cleaning(s):
    s = str(s)
    s = s.lower()
    s = re.sub('\s\W', ' ', s)
    s = re.sub('\W,\s', ' ', s)
    s = re.sub(r'[^\w]', ' ', s)
    s = re.sub("\d+", "", s)
    s = re.sub('\s+',' ', s)
    s = re.sub('[!@#$_]', '', s)
    s = s.replace("co", "")
    s = s.replace("https", "")
    s = s.replace(",", "")
    s = s.replace("[\w*", " ")
    return s
  
  data = pd.read_csv("data/gender_tweets.csv")
  
  data['Tweets'] = [cleaning(s) for s in data['text']]
  data['Description'] = [cleaning(s) for s in data['description']]
  
  stop = set(stopwords.words('english'))
  
  data['Tweets'] = data['Tweets'].str.lower().str.split()
  data['Tweets'] = data['Tweets'].apply(lambda x : [item for item in x if item not in stop])
    
  return data
    

