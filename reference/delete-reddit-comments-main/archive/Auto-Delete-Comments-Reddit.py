import os
import urllib.request
import bs4 as bs
import webbrowser
from datetime import datetime
from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.by import By
import time
#from selenium.webdriver.chrome.options import Options
#from selenium.webdriver.support.ui import Select
 
"""
delete all those comments that you made throughout the years
"""

# Open log file
log = 'history.log'
f = open(log, "a")

# get current time
now = datetime.now().strftime("%m/%d/%Y %H:%M")  
f.write('{} Starting script'.format(now))
f.write('\n')
f.write('\n')


#=================================
# Setup
#=================================

# get the path of ChromeDriverServer
s=Service(".\chromedriver.exe")

# create a new Chrome session
driver = webdriver.Chrome(service=s)

#enter username and pw
username = 'xxxx'
password = 'yyyy'

# get the page text
url = os.path.join('https://www.reddit.com/user', username)
driver.get(url)
html = driver.page_source
soup = bs.BeautifulSoup(html, 'lxml')

# find the login button and click it
login_button = driver.find_element(By.XPATH,'//*[@id="SHORTCUT_FOCUSABLE_DIV"]/div[1]/header/div/div[2]/div/div[1]/a[1]')
login_button.click()
driver.implicitly_wait(3) # seconds

# clicking login opens a new frame, switch to it
driver.switch_to.frame(driver.find_element(By.XPATH,'//*[@id="SHORTCUT_FOCUSABLE_DIV"]/div[3]/div/div/iframe'))
#driver.switch_to.frame(driver.find_element(By.TAG_NAME,'iframe'))


# find the username and password boxes
un_field = driver.find_element(By.CSS_SELECTOR,'input[name="username"]')
pw_field = driver.find_element(By.CSS_SELECTOR,'input[name="password"]')
#un_field = driver.find_element(By.XPATH,'//*[@id="loginUsername"]')
#pw_field = driver.find_element(By.XPATH,'//*[@id="loginPassword"]')
#driver.find_element_by_name('username')
#driver.find_element_by_id('loginUsername')

# login with username and password
un_field.send_keys(username)
pw_field.send_keys(password)

# find login button and click
login_button = driver.find_element(By.XPATH,'/html/body/div/main/div[1]/div/div[2]/form/fieldset[5]/button')
login_button.click()

# get cookies and store them
cookies = driver.get_cookies()
for cookie in cookies:
    driver.add_cookie(cookie)

# track number of comments deleted in session
comments_deleted = 0
posts_deleted = 0

# number of times to refresh the page for more comments
number_of_cycles = 30

# time to refresh if erro

# main sequence
times_ran = 0
times_refreshed = 0
while times_ran < number_of_cycles:
    
    print('Starting Cycle #{}'.format(times_ran+1))
    
    #try: 
    # find the context buttons for all available comments
    buttons = driver.find_elements(By.CLASS_NAME, '_38GxRFSqSC-Z2VLi5Xzkjy')
    
    for button in buttons:
        button.click()
        
        try:
            div_height = driver.find_element(By.XPATH,'/html/body/div[4]/div').size['height'] 
              
        except:
            pass
            
        else:
            div_height = driver.find_element(By.XPATH,'/html/body/div[5]/div').size['height']  
        
        try:
            if div_height < 236:    
                delete_button = driver.find_element(By.XPATH, '/html/body/div[4]/div/button[3]') 
                delete_button.click()
                delete_button2 = driver.find_element(By.XPATH, '/html/body/div[1]/div/div[2]/div[4]/div/div/section/footer/button[2]')
                delete_button2.click()
                comments_deleted = comments_deleted + 1
                times_refreshed = 0
    
            else:
                print("Encountered a post. Trying another method...")
                delete_button = driver.find_element(By.XPATH, '/html/body/div[5]/div/div/button[6]')
                delete_button.click()     
                delete_button2 = driver.find_element(By.XPATH, '/html/body/div[1]/div/div[2]/div[4]/div/div/section/footer/button[2]')
                delete_button2.click()
                posts_deleted = posts_deleted + 1 
                times_refreshed = 0
    
            
        except Exception as e: 
            print(e)
    
        #time.sleep(1) 
    #except:
        #while times_refreshed < 3:
            #print('Something unexpected happened. Refreshing and trying again...')
            #driver.refresh()
            #times_refreshed = times_refreshed + 1
        #else:
            #f.write('The script was unable to finish')
            #times_ran = 9999999999999999999999999999999999999
            
    times_ran = times_ran + 1
    driver.refresh()
    

# close the browser window
#driver.quit()

# logging
f.write('Number of comments deleted: {}'.format(comments_deleted))
f.write('\n')
f.write('Number of posts deleted: {}'.format(posts_deleted))
f.write('\n')
f.write('\n')
now = datetime.now().strftime("%m/%d/%Y %H:%M") 
f.write('{} Ending script'.format(now))
f.write('\n')
f.write('\n')

# close the log file
f.close()






