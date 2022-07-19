import glob
import os
import urllib.request
import webbrowser
from datetime import datetime
from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.by import By
import time

def append_id(filename,id):
    return "{0}_{2}.{1}".format(*filename.rsplit('.', 1) + [id])


#$$$$$$$$$$$$$$$$
# MAIN
#$$$$$$$$$$$$$$$$

if __name__ == "__main__":
    
    # Open log file
    log = open('history.log', "a")
    
    # get current time
    now = datetime.now().strftime("%m/%d/%Y %H:%M")  
    log.write('{} Starting script'.format(now))
    log.write('\n')
    
    #=================================
    # Setup Chromedriver
    #=================================
    
    # disable pesky browser notifications
    chrome_options = webdriver.ChromeOptions()
    prefs = {"profile.default_content_setting_values.notifications" : 2}
    chrome_options.add_experimental_option("prefs",prefs)
    
    # get the path of ChromeDriverServer
    s=Service(".\chromedriver.exe")
    
    # create a new Chrome session
    driver = webdriver.Chrome(service=s, options=chrome_options)

    
    # get the page text
    driver.get("https://udot.iteris-clearguide.com/");  
    
    time.sleep(60); #wait so user can log in manually
    print ('1 Min Passed')
    time.sleep(60); #wait so user can log in manually
    print ('2 Min Passed')
    time.sleep(60); #wait so user can log in manually
    print ('3 Min Passed')

    key = ""
    # get username and password from text file
    f=open(r".\key.txt","r")
    #f=open("login2.txt", "r")
    lines = f.readlines()
    key = lines[0]

    

    for x in range(1, len(lines)-1):
        if (x==1):
            _list_of_files = glob.glob(r'C:\Users\bhereth\Downloads\*.kml') # * means all if need specific format then *.kml
            _latest_file = max(_list_of_files, key=os.path.getctime)
            _last_latest_file = _latest_file
        _address = "https://geo.iteris-clearguide.com/v1/routing/user_route/" + lines[x] + "/shape/?customer_key=udot&network_info=true&jwt=" + key +  "&format=kml"
        time.sleep(1)
        print(_address )
        driver.get(_address );
        time.sleep(1)
        print(lines[x])
        _list_of_files = glob.glob(r'C:\Users\bhereth\Downloads\*.kml') # * means all if need specific format then *.kml
        _latest_file = max(_list_of_files, key=os.path.getctime)
        print(_latest_file)
        if (_last_latest_file != _latest_file) :
            os.rename(_latest_file,append_id(_latest_file,x))
            _last_latest_file = _latest_file

    f.close()

#============
# scrapyard     
#============

## get cookies and store them
#cookies = driver.get_cookies()
#for cookie in cookies:
    #driver.add_cookie(cookie)
