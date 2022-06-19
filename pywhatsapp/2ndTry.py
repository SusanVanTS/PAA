from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import StaleElementReferenceException
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
import time
import sys

#def find(driver):
#    element = driver.find_elements_by_id("data")
#    if element:
#        return element
#    else:
#        return False
# Replace below path with the absolute path of the \
#chromedriver in your computer
driver = webdriver.Chrome('C:/Users/Asus/Desktop/PAA/pywhatsapp/chromedriver.exe')

driver.get("https://web.whatsapp.com/")
# time.sleep()
ignored_exceptions=(NoSuchElementException,StaleElementReferenceException)
#element = WebDriverWait(driver, 600).until(find)
wait = WebDriverWait(driver, 6000)
#
wait = WebDriverWait(driver, 6000, ignored_exceptions = ignored_exceptions)
# Replace 'My Bsnl' with the name of your friend or group name
target = '"Baby"'
text="Im going to spam you from Python, SusanVan\n Testing in Progress\n <3"
# Replace the below string with your own message
string = text

x_arg = '//span[contains(@title,' + target + ')]'
group_title = wait.until(EC.presence_of_element_located((By.XPATH, x_arg)))

#group_title = wait.until(EC.presence_of_element_located((By.XPATH, x_arg)))
                        
print (group_title)
print ("Wait for few seconds")

#group_title.click()
if len(group_title) > 0:
    group_title.click()
#driver.find_element_by_id(group_title).click()
message = driver.find_elements_by_xpath('//*[@id="main"]/footer/div[1]/div[2]/div/div[2]')[0]
message.send_keys(string)
sendbutton = driver.find_elements_by_xpath('//*[@id="main"]/footer/div[1]/div[3]/button')[0]
sendbutton.click()
driver.close()