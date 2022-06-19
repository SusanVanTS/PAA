# -*- coding: utf-8 -*-
"""
Created on Thu Sep 26 09:25:57 2019

@author: Asus
"""

import tkinter as tk 
from tkinter import messagebox
from time import sleep
import sys 
import time
from zalo.sdk.oa import ZaloOaInfo, ZaloOaClient
import pdf2image
import os
import glob


def pdftopil():
#    start_time = time.time()
    pil_images = pdf2image.convert_from_path(PDF_PATH, dpi=DPI, output_folder=OUTPUT_FOLDER, first_page=FIRST_PAGE, last_page=LAST_PAGE, fmt=FORMAT, thread_count=THREAD_COUNT, userpw=USERPWD, use_cropbox=USE_CROPBOX, strict=STRICT)
#    print ("Time taken : " + str(time.time() - start_time))
    return pil_images
    
def save_images(pil_images):
    #This method helps in converting the images in PIL Image file format to the required image format
    index = 1
    for image in pil_images:
        image.save("page_" + str(index) + ".jpg")
        index += 1

#with open('C:/pa/zl2u/live/zl2u.txt') as f:
with open('C:/pa/zl2u/live/zl2u.txt') as f:
   lines = f.readlines()
print("Extracting .txt file")   
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
        if (r[11] == ' ' or r[11] == '\t' or r[11] == ''):
            Receiver.append(r[:11])
        else:
            Receiver.append(r[:12])      
    elif s[:3] == 'M40':
        m = s[4:-1]
        Message.append(m[:-1])
    elif s[:3] == 'M50':
        pdfPath = s[4:-1]
        pdfPath = pdfPath[:-1]
for l in Message:  
    abc = l.replace('   ', '')
    FinalMessage.append(abc)
print("Getting .pdf file")
#DECLARE CONSTANTS for pdf2image
PDF_PATH = pdfPath
DPI = 200
OUTPUT_FOLDER = "C:/pa/zl2u/pdf"
FIRST_PAGE = None
LAST_PAGE = None
FORMAT = 'jpg'
THREAD_COUNT = 1
USERPWD = None
USE_CROPBOX = False
STRICT = False

imagepath =[]    
if __name__ == "__main__":
    pil_images = pdftopil()
    save_images(pil_images)
    imglen = len(pil_images)
    for i in range(imglen):
        imagepath.append(pil_images[i].filename)
folder = 'C:/pa/zl2u/live'  
#Create an instance of the ZaloOA class
zalo_info = ZaloOaInfo(oa_id="ss", secret_key="ss")
zalo_oa_client = ZaloOaClient(zalo_info)

for contact in Receiver:
    if contact == '':
        continue
    else:
        PH = contact
        print("Sending to "+PH)
        TXT = "\n".join(FinalMessage)
        #Get Profile Follower
        profile = zalo_oa_client.get('getprofile', {'uid': PH})
        #----getTIMEfordisplay----
        HH = str(time.localtime().tm_hour)
        MM = str(time.localtime().tm_min)
        SS = str(time.localtime().tm_sec)
        TIME = HH+':'+MM+':'+SS
        #----getUserID----
        userdata =  profile["data"]
        print(profile)
        USERID = userdata["userId"]
        #----sendMessage---
        data = {
                'uid': USERID,
                'message': TXT
                }
        params = {'data': data}
        send_text_message = zalo_oa_client.post('sendmessage/text', params)
        #Get message status
        message_status = zalo_oa_client.get('getmessagestatus', {'msgid': 'message_id'})
        #Upload image
        for i in imagepath:
            ipath = i
            #bin_data = open(ipath, 'rb').read()
            #print(bin_data)
            ACCT = 'xx'  ## account token
            uI = zalo_oa_client.post('../../v2.0/oa/upload/image?access_token='+ACCT, {'file': ipath})
            data = {
                'uid': USERID,
                'imageid': uI['data']['attachment_id'],
                'message': '-'
            }
            params = {
                'data': data
            }
            sI = zalo_oa_client.post('sendmessage/image', params)
        
sleep(1)
#get a recursive list of file paths that matches pattern including sub directories
fileList = glob.glob('**/page_*.jpg', recursive=True)
# Iterate over the list of filepaths & remove each file.
for filePath in fileList:
    try:
        os.remove(filePath)
    except OSError:
        print("Error while deleting file")
print("File Removed!")
root = tk.Tk()
lbl = messagebox.showinfo("Done", "Message sent at "+TIME)
button1 = tk.Button (root, text='OK',command=lbl)
root.destroy()  
root.mainloop()
sys.exit()