import pandas as pd
import requests
from bs4 import BeautifulSoup
from selenium import webdriver
import clipboard
import os
import time
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.keys import Keys
import numpy as np

########################################
######### CHANGE THE INFO HERE #########
########################################

lg = 'KKD'
lg_id = '1w03tfpd5nc6woo8j2nc9239w' # This is the league ID from Opta.. Can find it in the html of a league's page
lg_schedule_location = "/Users/robinkoetsier/Downloads"
lg_schedule_location = f"/Users/robinkoetsier/Documents/ScraperWhoScored/{lg}" ## This is where you want the schedule of matches to download to
chromium_location = "C:/Users/Ben/Coding/chromedriver.exe"  ## This is the file path to where your Chromium program is
match_download_location = "/Users/robinkoetsier/Documents/ScraperWhoScored"  ## This is the base folder that will house your league folders. Create a folder named the same as the lg variable in this filepath folder
#from pathlib import Path
#downloads_path = str(Path.home() / "Downloads")
##################################################################################
######### TO SAVE TIME, I MAKE SURE TO PUT A FILTERING MECHANISM #################
######### IN THE CODE SO THAT IT ONLY GETS GAMES AFTER THE LAST RUN DATE #########
######### ONCE YOU RUN A LEAGUE, ADD IT AND THE LAST RUN DATE ####################
######### (two examples shown commented) #########################################
##################################################################################
from datetime import datetime, timedelta
d = datetime.today() - timedelta(days=2)



if lg == 'Segunda División':
     last_run = "2023-03-12"
if lg == 'Indian Super League':
     last_run = "2023-03-11"
else:
    last_run = d.strftime('%Y-%m-%d')
    last_run = '2023-03-22'

##########################################################################

url = "https://api.performfeeds.com/soccerdata/match/qxcx5jmswgto1qeqcjzghtddt/?_rt=c&tmcl=%s&live=yes&_pgSz=400&_lcl=en&_fmt=jsonp&_clbk=W360b23205d13a5cdd46c315ae9c2097b829cbc909" %lg_id

page = requests.get(url, headers={'referer': 'scoresway.com'})
soup = BeautifulSoup(page.content, "html.parser")


raw = soup.getText()
start = raw.find('{"match"')+9
output = raw[start:-2]

#from pandas import json_normalize
#dict = json.loads(output)
#df2 = json_normalize(dict)
#print(df2)
#df2['matchInfo.localDate']
options = webdriver.ChromeOptions()
prefs = {"download.default_directory": lg_schedule_location}
options.add_experimental_option("prefs",prefs)

driver = webdriver.Chrome(chrome_options=options)
driver.get("https://konklone.io/json")
input_css = 'body > section.json > div.areas > textarea'

# Connecting to it with our driver
#input_area = driver.find_element_by_css_selector(input_css)
input_area = driver.find_element(By.CSS_SELECTOR, input_css)#
# Set the sentence into the clipboard
clipboard.copy(output)
# Making sure that there is no previous text
input_area.clear()
# Pasting the copied sentence into the input_area
input_area.send_keys(Keys.SHIFT, Keys.INSERT)

# CSS of the download button
click_css = 'body > section.csv > p > span.rendered > a.download'

# Click it
#driver.find_element_by_css_selector(click_css).click()
driver.find_element(By.CSS_SELECTOR, click_css).click()
time.sleep(2)
driver.close()

path = f'{lg_schedule_location}'
os.chdir(path)
files = sorted(os.listdir(os.getcwd()), key=os.path.getmtime)
os.rename(files[len(files)-1], '%s Matches.csv' %lg)
print('DONE WITH MATCHES FILE')


################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################


df = pd.read_csv('%s/%s Matches.csv' %(lg_schedule_location,lg))
df.to_csv('%s/%s Matches.csv' %(lg_schedule_location,lg), encoding='utf-8-sig')


df = pd.read_csv('%s/%s Matches.csv' %(lg_schedule_location,lg))

df.dropna(subset=['liveData/matchDetails/scores/ft/home'],inplace=True)
df.reset_index(drop=True,inplace=True)
df = df[::-1].reset_index(drop=True)

cols = ['matchInfo/localDate','matchInfo/id',
        'matchInfo/contestant/0/officialName','liveData/matchDetails/scores/ft/home',
        'liveData/matchDetails/scores/ft/away','matchInfo/contestant/1/officialName',
        'matchInfo/contestant/0/id','matchInfo/contestant/1/id']
matches = df[cols]

matches['matchName'] = ''
for i in range(len(matches)):
    #matches['matchName'][i] = '%s %i-%i %s' %(matches['matchInfo/contestant/0/officialName'][i], matches['liveData/matchDetails/scores/ft/home'][i], matches['liveData/matchDetails/scores/ft/away'][i], matches['matchInfo/contestant/1/officialName'][i])
    #matches['matchName'][i] = '%s - %s' % (matches['matchInfo/contestant/0/officialName'][i], matches['matchInfo/contestant/1/officialName'][i])
    matches['matchName'][i] = f"{matches['matchInfo/localDate'][i]}_{matches['matchInfo/contestant/0/officialName'][i]} - {matches['matchInfo/contestant/1/officialName'][i]}"
if last_run != '':
    matches['matchInfo/localDate'] = pd.to_datetime(matches['matchInfo/localDate'])
    matches = matches[matches['matchInfo/localDate']>pd.to_datetime(last_run)].reset_index(drop=True)
idd = '57d4g9sd8u1salvv4eip2jhn8'
for i in range(len(matches)):
    url = "https://api.performfeeds.com/soccerdata/matchevent/qxcx5jmswgto1qeqcjzghtddt/%s?_rt=c&_lcl=en&_fmt=jsonp&_clbk=W3bd804271ec7f49caa576ff8367109721760953b4" %matches['matchInfo/id'][i]
    #url = f"https://api.performfeeds.com/soccerdata/matchevent/qxcx5jmswgto1qeqcjzghtddt/{idd}?_rt=c&_lcl=en&_fmt=jsonp&_clbk=W3bd804271ec7f49caa576ff8367109721760953b4"

    page = requests.get(url, headers={'referer': 'scoresway.com'})
    soup = BeautifulSoup(page.content, "html.parser")


    raw = soup.getText()
    start = raw.find("liveData")+10
    output = raw[start:-2]

    options = webdriver.ChromeOptions()
    prefs = {"download.default_directory": "%s/%s" %(match_download_location,lg)}
    options.add_experimental_option("prefs",prefs)

    # driver = webdriver.Chrome(chrome_options=options)
    # driver.get("https://konklone.io/json")
    # input_css = 'body > section.json > div.areas > textarea'
    #
    # # Connecting to it with our driver
    # #input_area = driver.find_element_by_css_selector(input_css)
    # input_area = driver.find_element(By.CSS_SELECTOR, input_css)
    # # Set the sentence into the clipboard
    # clipboard.copy(output)
    # # Making sure that there is no previous text
    # input_area.clear()
    # # Pasting the copied sentence into the input_area
    # input_area.send_keys(Keys.SHIFT, Keys.INSERT)
    #
    # # CSS of the download button
    # click_css = 'body > section.csv > p > span.rendered > a.download'
    #
    # # Click it
    # #driver.find_element_by_css_selector(click_css).click()
    # driver.find_element(By.CSS_SELECTOR, click_css).click()
    # time.sleep(2)
    # driver.close()
    #
    path = '%s/%s' %(match_download_location,lg)
    os.chdir(path)
    files = sorted(os.listdir(os.getcwd()), key=os.path.getmtime)
    #os.rename(files[len(files)-1], '%s.csv' %matches.matchName[i])
    import json

    output1 = json.loads(output)

    with open('data.json', 'w', encoding='utf-8') as f:
        json.dump(output1, f, ensure_ascii=True, indent=4)
    os.rename("data.json", '%s.json' % matches.matchName[i])
    print('downloaded - %s' %matches.matchName[i])
print('DONE WITH EVERYTHING')
