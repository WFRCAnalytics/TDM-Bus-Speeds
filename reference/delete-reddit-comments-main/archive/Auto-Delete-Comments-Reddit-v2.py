import os
import urllib.request
import bs4 as bs
import webbrowser
from datetime import datetime
from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.by import By
import time

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

# disable pesky browser notifications
chrome_options = webdriver.ChromeOptions()
prefs = {"profile.default_content_setting_values.notifications" : 2}
chrome_options.add_experimental_option("prefs",prefs)


# get the path of ChromeDriverServer
s=Service(".\chromedriver.exe")

# create a new Chrome session
driver = webdriver.Chrome(service=s, options=chrome_options)

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

# login with username and password
un_field.send_keys(username)
pw_field.send_keys(password)

# find login button and click
login_button = driver.find_element(By.XPATH,'/html/body/div/main/div[1]/div/div[2]/form/fieldset[5]/button')
login_button.click()

## get cookies and store them
#cookies = driver.get_cookies()
#for cookie in cookies:
    #driver.add_cookie(cookie)


mode = 0

# mode 1 sets a pre determined amount of comments to destroy (e.g. n = 100)
# mode 2 uses a determined amount of iterations
# mode 3 keeps deleting until failure


comments_deleted = 0


# delete comments function
def del_comments():
    
    # used for incrementing
    global comments_deleted
    
    # find comments
    comments = driver.find_elements_by_class_name('Comment')
    
    # loop through gathered comments
    for c in comments:
        
        # find the buttons of the comment
        more_options = c.find_elements(By.TAG_NAME, 'button')
        
        # if the comment is interactable, otherwise its non-user
        if len(more_options) > 0:

            # click
            more_options[-1].click()
            
            # find the menu div
            menu =  driver.find_element(By.CSS_SELECTOR, "._2uYY-KeuYHKiwl-9aF0UiL.ehsOqYO6dxn_Pf9Dzwu37")
                    
            # find the delete button
            buttons = menu.find_elements(By.CSS_SELECTOR, "._10K5i7NW6qcm-UoCtpB3aK._3LwUIE7yX7CZQKmD2L87vf._2LNy1r5iuFMrf0PLh4UdV-._1oYEKCssGFjqxQ9jJMNj5G")
            
            # click, if the text of the button is 'delete'
            for b in buttons:
                name = b.text
                if name == 'Delete':
                    b.click()
                #else:
                    #print('found {} button'.format(name))
                
             # find the confirm delete button and click  
            confirm_button = driver.find_element(By.CSS_SELECTOR, '._17UyTSs2atqnKg9dIq5ERg.ogOEj4x-0BpDZWeccJwxx._2iuoyPiKHN3kfOoeIQalDT._10BQ7pjWbeYP63SAPNS8Ts.HNozj_dKjQZ59ZsfEegz8._2nelDm85zKKmuD94NequP0')
            confirm_button.click()
            
            # tracking
            print('I deleted a comment...')
            comments_deleted = comments_deleted + 1
            
            
        #else:
            #print('Skipping non-user comment...')
        
        
    print(comments_deleted)
    return comments_deleted

# mode 1 sets a pre determined amount of comments to destroy (e.g. n = 100)
# mode 2 uses a determined amount of iterations
# mode 3 keeps deleting until failure

def delete_comments(mode, n):
    comments_deleted = 0
    
    
    
    if mode == 1:
        print('mode is set to 1')
        
    if mode == 2:
        print('mode is set to 2')

  
del_comments()
driver.refresh()
del_comments()


# logging
f.write('Number of comments deleted: {}'.format(comments_deleted))
f.write('\n')
now = datetime.now().strftime("%m/%d/%Y %H:%M") 
f.write('{} Ending script'.format(now))
f.write('\n')
f.write('\n')

# close the log file
f.close()