#!/usr/bin/env python
import os, sys
import imaplib, email
curr_dir = os.path.dirname(__file__) + '/'
sys.path.append(curr_dir)
from erlport import Port, Protocol, Atom, String


class Fetcher(Protocol):
    conn = None
    port = None

    def __init__(self):
        self.port = Port(use_stdio=True)
        self.run(self.port)

    def handle_connect(self, host, username, password, mailbox):
        try:
            self.handle_logout()
            self.conn = imaplib.IMAP4_SSL(host=host)
            conn.login(username, password)
            conn.select(mailbox, readonly=0)
        except Exception, e:
            return( (Atom('error'), str(e)) )
    
    def handle_logout(self):
        if self.conn:
            self.conn.close()
            self.conn.logout()

    def handle_get_new(self):
        if not self.conn:
            return( (Atom('error'), 'not connected') )
        res_messages = []
        (r, m_ids) = conn.search('UTF-8', 'UNSEEN')
        if r == 'OK':
            for message in m_ids[0].split(' '):
                print 'Processing :', message
                (r, msginfo) = conn.fetch(message, 'RFC822')
                if r == 'OK':
                    msg = email.message_from_string(msginfo[0][1])
                    self.port.write( (Atom('email'), msg.items(), msg.get_payload()) )
                    print msg.get('From')
                #conn.store(message, '+FLAGS', '\\Seen')
        return Atom('done')


if __name__ == '__main__':
    fetcher = Fetcher()
