import socket
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect(("localhost", 1800))

while 1:
    s.send(raw_input("message to send:"))
    r = s.recv(1024)
    if r == "quit":
        s.close()
        break
    else:
        print r


