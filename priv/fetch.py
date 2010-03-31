#!/usr/bin/env python
import os, sys
import imaplib, email
curr_dir = os.path.dirname(__file__) + '/'
sys.path.append(curr_dir)
from erlport import Port, Protocol, Atom, String
import pickle

class Fetcher(Protocol):
    conn = None
    port = None

    def __init__(self):
        self.port = Port(use_stdio=True, packet=4)
        self.run(self.port)

    def handle_connect(self, host, username, password, mailbox):
        self.host = String(host)
        self.username = String(username)
        self.password = String(password)
        self.mailbox = String(mailbox)
        pickle.dump( (self.host, self.username, self.password, self.mailbox), open('dump.p', 'wb'))
        try:
            self.reconnect()
            return( Atom('ok') )
        except Exception, e:
            return( (Atom('error'), str(e)) )

    def reconnect(self):
        self.handle_logout()
        self.conn = imaplib.IMAP4_SSL(host=self.host)
        self.conn.login(self.username, self.password)
        self.conn.select(self.mailbox, readonly=0)
    
    def handle_logout(self):
        if self.conn:
            self.conn.close()
            self.conn.logout()

    def handle_get_new(self):
        if not self.conn:
            self.reconnect()
        (r, m_ids) = self.conn.search('UTF-8', 'UNSEEN')
        if r == 'OK':
            self.port.write(Atom('initiating_get_new'))
            for message in m_ids[0].split(' '):
                if message == '': continue
                (r, msginfo) = self.conn.fetch(message, 'RFC822')
                if r == 'OK':
                    msg = email.message_from_string(msginfo[0][1])
                    #self.port.write( (Atom('email'), msg.items(), msg.get_payload()) )
                    if msg.is_multipart():
                        body = [a.as_string() for a in msg.get_payload()]
                    else:
                        body = [msg.get_payload()]
                    self.port.write( (Atom('email'), msg.items(), body) )
                    self.conn.store(message, '+FLAGS', '\\Seen')
            return Atom('done_getting_new')
        else:
            return( (Atom('error'), r) )

if __name__ == '__main__':
    fetcher = Fetcher()
