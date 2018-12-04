import glob
import os
import pandas as pd
import numpy as np

filename = glob.glob("*.csv")

df = pd.concat([pd.read_csv(i).assign(Year = os.path.basename(i)[16:20])  
                for i in filename],ignore_index = True)

#convert column names to lowercase
df.columns = map(str.lower, df.columns)

#create county comission district
def ccdcat(x):
    if x < 1000:
        return str(x)[:1]
    else:
        return str(x)[:2]

df['ccd'] = df['precinctcode'].apply(ccdcat)

#create school board district
def sbdcat(x):
    if x in ('1','2'):
        return '1'
    elif x in ('3','4'):
        return '2'
    elif x in ('5','6'):
        return '3'
    elif x in ('7','8'):
        return '4'
    elif x in ('9','10'):
        return '5'
    elif x in ('11','12'):
        return '6'
    elif x in ('13','14'):
        return '7'

df['sbd'] = df['ccd'].apply(sbdcat)


df['party'] = np.where(df.party.isin(['DEM','REP']), df.party,'OTH')
df['candidate_issue'] = np.where(df.party.isin(['DEM','REP']), df.candidate_issue,'OTH')

df.contest.replace(["GOVERNOR AND LIEUTENANT GOVERNOR","MAYOR","United States President and Vice President"],
                   ["Governor","Mayoral","Presidental"], inplace=True)

df = df.query('contest == "Governor" | contest == "Presidental" \
               |contest == "Mayoral"')

df = df[['year','party', 'contest','ccd','sbd', 'candidate_issue', 'totalvotes']]

#combine year and contest for unique election type
df['election'] = df['year'] + " " + df['contest'] + " " + "Election"

df.to_csv("df.csv", index = False)

