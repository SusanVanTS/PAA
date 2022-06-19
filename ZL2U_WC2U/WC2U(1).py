import win32api as w 
import win32clipboard as wc
import win32con
import win32gui
import win32com
import time
import os
import sys 

def setText(aString):
    wc.OpenClipboard()
    wc.EmptyClipboard()
    wc.SetClipboardData(win32con.CF_UNICODETEXT, aString)
    wc.CloseClipboard()


def setImage(data):  # 写入剪切板  
    
    wc.OpenClipboard()  
    try:  
        # Unicode tests  
        wc.EmptyClipboard()  
        wc.SetClipboardData(win32con.CF_DIB, data)  
    except:  
        wc.traceback.print_exc()  
    finally:  
        wc.CloseClipboard()  


def ctrlV():
    w.keybd_event(17,0,0,0)  #ctrl键位码是17
    w.keybd_event(86,0,0,0)  #v键位码是86
    w.keybd_event(86,0,win32con.KEYEVENTF_KEYUP,0) #释放按键
    w.keybd_event(17,0,win32con.KEYEVENTF_KEYUP,0)

def altS(): 
    w.keybd_event(18, 0, 0, 0)    #Alt  
    w.keybd_event(83,0,0,0) #s
    w.keybd_event(83,0,win32con.KEYEVENTF_KEYUP,0) #释放按键
    w.keybd_event(18,0,win32con.KEYEVENTF_KEYUP,0)

os.startfile("C:\Program Files (x86)\Tencent\WeChat\WeChat.exe")
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


abcd = '\n'.join(FinalMessage)
setText(abcd)
hwnd = win32gui.FindWindow(None, "WeChat")
time.sleep(0.1)
friends = win32gui.GetClassName(hwnd)
print(friends)
ctrlV()
altS()
win32gui.CloseWindow(hwnd)