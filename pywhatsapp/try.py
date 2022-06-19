import tkinter as tk
from tkinter import messagebox

# create window

root = tk.Tk()
    

lbl = messagebox.showinfo("Done", "Message sent.")
button1 = tk.Button (root, text='OK',command=lbl)
#canvas1.create_window(125, 80, window=button1)
root.destroy()  


root.mainloop()
