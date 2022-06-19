# -*- coding: utf-8 -*-
"""
Created on Mon Sep 30 15:53:09 2019

@author: Asus
"""

from pywxclient.core import Session, SyncClient


s1 = Session()

c1 = SyncClient(s1)

c1.get_authorize_url()  # Open the url in web browser

c1.authorize()  # Continue authorize when returning False

c1.login()

c1.sync_check()

msgs = c1.sync_message()  # Here are your wechat messages

c1.flush_sync_key()