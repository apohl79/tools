#!/usr/bin/env python3

import smtplib, sys

if len(sys.argv) != 7:
    print("Usage: {} <server> <port> <from> <password> <to> <subject>".format(sys.argv[0]))
    sys.exit(1)

content = sys.stdin.read()
mail = smtplib.SMTP(sys.argv[1], sys.argv[2])
mail.ehlo()
mail.starttls()
mail.login(sys.argv[3], sys.argv[4])
mail.sendmail(sys.argv[3], sys.argv[5],
              'To:' + sys.argv[5] + '\n' +
              'From:' + sys.argv[3] + '\n' +
              'Subject: ' + sys.argv[6] + '\n\n' + content)
mail.close()
