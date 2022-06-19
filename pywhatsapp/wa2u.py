# -*- coding: utf-8 -*-
"""
Created on Fri Aug 23 16:48:05 2019

@author: Asus
"""
from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import StaleElementReferenceException
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
import tkinter as tk 
from tkinter import messagebox
from time import sleep
import sys 

#initialize variables
x_arg = '//*[@id="side"]/div[1]'
#with open('C:/pa/wa2u/live/wa2u.txt') as f:
with open('C:/pa/wa2u/live/wa2u.txt') as f:
   lines = f.readlines()
Receiver = []
Message = []
FinalMessage = []
#read .txt
for s in lines: 
    if s[:3] == 'M00':
        MessageApp = s[4:-1]
        MessageApp = MessageApp[:-1]
    elif s[:3] == 'M10':
        Sender = s[4:-1]
        Sender = Sender[:-1]
    elif s[:3] == 'M20':
        SenderPass = s[4:-1]
        SenderPass = SenderPass[:-1]
    elif s[:3] == 'M30':
        r = s[4:-1]
        if (r[10] == ' ' or r[10] == '\t' or r[10] == ''):
            Receiver.append(r[:10])
        else:
            Receiver.append(r[:11])      
    elif s[:3] == 'M40':
        m = s[4:-1]
        Message.append(m[:-1])
    elif s[:3] == 'M50':
        pdfPath = s[4:-1]
        pdfPath = pdfPath[:-1]
for l in Message:  
    abc = l.replace('   ', '')
    FinalMessage.append(abc)
#open Chrome        
driver = webdriver.Chrome('C:/Users/Public/Documents/chromedriver.exe')
driver.maximize_window()
driver.get("https://web.whatsapp.com/")
wait = WebDriverWait(driver, 18000)
ignored_exceptions=(NoSuchElementException,StaleElementReferenceException)
wait.until(EC.presence_of_element_located((By.XPATH, x_arg)))
for contact in Receiver:
    if contact == '':
        continue
    else:
        #searching target in new chat
        search = driver.find_element_by_css_selector('span[data-icon="chat"]').click()
        search = driver.find_element_by_class_name('_2MSJr').click()
        search1 = driver.find_element_by_class_name('_2S1VP')
        search1.send_keys(contact)
        sleep(3)
        search1.send_keys(Keys.ENTER)
        sleep(2)
        try:
            #catch error if no such contact
            driver.find_element_by_class_name('_3dwyT')            
        except:
            #below are code to send specified message to target
            message = driver.find_elements_by_xpath('//*[@id="main"]/footer/div[1]/div[2]/div/div[2]')[0]
            for lines in FinalMessage:
                message.send_keys(lines)
                message.send_keys(Keys.SHIFT, Keys.ENTER)
            sendbutton = driver.find_elements_by_xpath('//*[@id="main"]/footer/div[1]/div[3]/button')[0]
            sendbutton.click()
            #send pdf
            try:
                driver.find_element_by_css_selector('span[data-icon="clip"]').click()
            except EC.NoSuchElementException:
                sleep(3)
            try:
                attach = driver.find_element_by_css_selector('input[type="file"]')
            except EC.NoSuchElementException:
                sleep(3)
            attach.send_keys(pdfPath)
            try:
                driver.find_element_by_class_name('_3hV1n').click()
            except EC.NoSuchElementException:
                sleep(3)
                driver.find_element_by_class_name('_3hV1n').click()
            sleep(7)
        else:
            print("No such contact")    
        driver.refresh()
        if EC.alert_is_present == True:
            obj = driver.switch_to.alert
            obj.accept()
        sleep(7)
        continue
#End of operations
f.close()
driver.close()
sleep(2)
root = tk.Tk()
lbl = messagebox.showinfo("Done", "Message sent.")
button1 = tk.Button (root, text='OK',command=lbl)
root.destroy()  

root.mainloop()
driver.quit()

sys.exit()