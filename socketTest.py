import socket
import thread

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


