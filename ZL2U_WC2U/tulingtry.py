# -*- coding: utf-8 -*-
"""
Created on Wed Sep 25 11:59:39 2019

@author: Asus
"""

#import itchat
#import requests
#
def getResponse(_info):
	#print(_info)
	apiUrl = 'http://www.tuling123.com/openapi/api'
	data = {
    	'key'    : 'xxx', # 如果这个Tuling Key不能用，那就换一个
    	'info'   : 'Hello, Testing here', # 这是我们发出去的消息
    	'userid' : 'xxx', # 这里你想改什么都可以
	}
	# 我们通过如下命令发送一个post请求
	r = requests.post(apiUrl, data=data).json()

	# 让我们打印一下返回的值，看一下我们拿到了什么
	return r
#
#
#@itchat.msg_register(itchat.content.TEXT)
#def text_reply(msg):
#	#print(msg)
#    return "" + getResponse(msg["Text"])["text"]
##itchat.auto_login(enableCmdQR=True)
#
#itchat.auto_login(hotReload=True)
#itchat.run()


import time, datetime, requests, itchat
from itchat.content import *


itchat.login() # hotReload=True: 退出程序后暂存登陆状态。即使程序关闭，一定时间内重新开启也可以不用重新扫码。
# itchat.auto_login(enableCmdQR=-2) # enableCmdQR=-2: 命令行显示QR图片
friend = itchat.search_friends('xxx')
print(friend)
itchat.logout()

