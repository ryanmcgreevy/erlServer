import socket
import thread
import re
xml_pack = re.compile("<([A-Z][A-Z0-9]*)\\b[^>]*>(.*?)</\\1>") 
def listen(sock):
    while True:
        recieved = sock.recv(1024)
        manage_message(recieved)
def start():
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.connect(("localhost", 1800))
    thread.start_new_thread(listen, (sock,))
    Name = raw_input("Name: \n")
    send_message(sock, "Name", Name)
    raw_input("hit enter for player list....")
    send_message(sock, "Players", "")

def manage_message(msg):
    parsed_message = xml_pack.findall(msg)
    print parsed_message

def send_message(sock, Tag, Data):
    sock.send("<%(tag)s>%(data)s</%(tag)s>"%{"tag": Tag, "data": Data})

start()
