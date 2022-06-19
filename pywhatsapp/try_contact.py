#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Aug 19 15:06:32 2019

@author: susanvan
"""

from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import StaleElementReferenceException
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
import tkinter
from time import sleep


string ="Testing testing \n1\n2\n3"
x_arg = '//*[@id="side"]/div[1]'
filepath = 'C:/Users/Public/Documents/target.txt'
pdf_path = 'C:/Users/Public/Documents/tobesend.pdf'
f=open(filepath, "r")
#get driver, this is Chrome v76, get into webpage, wait for the respond
driver = webdriver.Chrome('C:/Users/Public/Documents/chromedriver.exe')
driver.maximize_window()
driver.get("https://web.whatsapp.com/")
wait = WebDriverWait(driver, 18000)
root = tkinter.Tk()
label = tkinter.Label(root)
label.place(x=35, y=15)

#group_title = wait.until(EC.presence_of_element_located((By.XPATH, x_arg)))
ignored_exceptions=(NoSuchElementException,StaleElementReferenceException)
if (wait.until(EC.presence_of_element_located((By.XPATH, x_arg)))):
    driver.minimize_window()

for f1 in f:
    x = f1.split(', ')
    
for contact in x:
    
    print(contact)    
    #--- -----------------MAIN-------------------------------------------------
    #set target contact and message
    target = contact
   
    if target == '':
        continue
    else:
        search = driver.find_element_by_css_selector('span[data-icon="chat"]').click()
        search = driver.find_element_by_class_name('ZP8RM').click()
        search1 = driver.find_element_by_class_name('_2zCfw')
        search1.send_keys(target)
        sleep(5)
        search1.send_keys(Keys.ENTER)
        sleep(5)
        try:
            driver.find_element_by_class_name('_3dwyT')
            
        except:
            #below are code to send specified message to target after selection part
            #message = driver.find_elements_by_xpath('//*[@id="main"]/footer/div[1]/div[2]/div/div[2]')[0]
            #message.send_keys(string)
            #sendbutton = driver.find_elements_by_xpath('//*[@id="main"]/footer/div[1]/div[3]/button')[0]
            #sendbutton.click()
            #send pdf worh
            driver.find_element_by_css_selector('span[data-icon="clip"]').click()
            attach=driver.find_element_by_css_selector('input[type="file"]')
            attach.send_keys(pdf_path)
            sleep(3)
            driver.find_element_by_class_name('NOJWi').click()
            sleep(3)
            root.destroy()
        else:
            print("No such contact")
            sleep(5)
        driver.refresh()
        sleep(5)
        continue
sleep(3)
label['text'] = 'Message sent.'
root.mainloop()
f.close()
driver.close()

