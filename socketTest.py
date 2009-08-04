import socket
import thread
##Create a message class that has a method for each type of xml-like message packet which takes in the data and forms the message for the programmer
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect(("localhost", 1800))

def listen():
    while 1:
        r = s.recv(1024)
        if r == "quit":
            s.close()
            break
        else:
            print r + "\n"

thread.start_new_thread(listen, ())
while 1:
    s.send(raw_input("message to send:"))


