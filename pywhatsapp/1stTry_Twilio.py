import PyPDF2

from twilio.rest import Client

# client credentials are read from TWILIO_ACCOUNT_SID and AUTH_TOKEN
client = Client()

# this is the Twilio sandbox testing number
from_whatsapp_number='whatsapp:+14155238886'
#from_whatsapp_number='whatsapp:+60168241897'
# replace this number with your own WhatsApp Messaging number
to_whatsapp_number='whatsapp:+60178645174'

#message = input("Please type your message here:")

# pdf file object
# you can find find the pdf file with complete code in below
pdfFileObj = open('/home/susanvan/Desktop/FYP_CrowdEstimation_Understanding/perspective aware.pdf', 'rb')
# pdf reader object
pdfReader = PyPDF2.PdfFileReader(pdfFileObj)
# number of pages in pdf
print(pdfReader.numPages)
# a page object
pageObj = pdfReader.getPage(5)
# extracting text from page.
# this will print the text you can also save that into String

message = pageObj.extractText()

client.messages.create(body=message,
                       from_=from_whatsapp_number,
                       to=to_whatsapp_number)

print("Sending to "+to_whatsapp_number)

