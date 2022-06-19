#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Aug 16 13:56:42 2019

@author: susanvan
"""

from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import StaleElementReferenceException
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
from time import sleep
import sys


def readTargetContact():
    filepath = '/home/susanvan/Desktop/target.txt'
    pdf_path = '/home/susanvan/Desktop/DEEP_LEARNING_WITH_TENSORFLOW.pdf'
    f=open(filepath, "r")
#    if f.mode == 'r':
#        contents =f.read()
#        print (contents)
    fl =f.readlines()
    for x in fl:    
        print(x)   


#--------------------MAIN--------------------------------------------------------
#get driver, this is Chrome v75
driver = webdriver.Chrome('/home/susanvan/Downloads/chromedriver')
#get into webpage
driver.get("https://web.whatsapp.com/")
ignored_exceptions=(NoSuchElementException,StaleElementReferenceException)
#wait for the respond
wait = WebDriverWait(driver, 6000)
#set target contact and message
target = input()
text="Im going to spam you from Python, SusanVan\n Testing in Progress\n <3"
string = text
x_arg = '//*[@id="side"]/div[1]'
group_title = wait.until(EC.presence_of_element_located((By.XPATH, x_arg)))
print (group_title)
print ("Wait for few seconds")
#search name to select the target, select the target person
search = driver.find_element_by_class_name('ZP8RM')
search.click()
search1 = search.find_element_by_class_name('_2zCfw')
search1.send_keys(target)
search1.send_keys(u'\ue007')

#below are code to send specified message to target after selection part
message = driver.find_elements_by_xpath('//*[@id="main"]/footer/div[1]/div[2]/div/div[2]')[0]
#message.send_keys(string)
#sendbutton = driver.find_elements_by_xpath('//*[@id="main"]/footer/div[1]/div[3]/button')[0]
#sendbutton.click()

#send pdf worh
driver.find_element_by_css_selector('span[data-icon="clip"]').click()
attach=driver.find_element_by_css_selector('input[type="file"]')
attach.send_keys(pdf_path)
sleep(3)
driver.find_element_by_class_name('NOJWi').click()                                             
#driver.close()
 


























