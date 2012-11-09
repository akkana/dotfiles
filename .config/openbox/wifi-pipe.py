#! /usr/bin/env python

#
# Openbox pipe menu script to show wifi connections
# Inspired by http://tuxtraining.com/2009/08/26/scan-for-and-connect-to-networks-from-an-openbox-pipe-menu
#
# Copyright 2009 by Akkana Peck akkana@shallowsky.com
# ... share and enjoy under the GPLv2 or (at your option) later.
#

import subprocess
import re
import sys

if len(sys.argv) > 1 :
    interface = sys.argv[1]
else :
    interface = "eth0"

print "<openbox_pipe_menu>"

proc = subprocess.Popen('iwlist scan 2>/dev/null', shell=True, stdout=subprocess.PIPE, )
stdout_str = proc.communicate()[0]
stdout_list = stdout_str.split('\n')

class AccessPoint :

    """ One Cell or AccessPoint from iwlist output"""

    def __init__(self) :
        self.address = ""
        self.essid = ""
        self.protocol = ""
        self.encryptionType = ""

aplist=[]
ap = None

for line in stdout_list:
    line=line.strip()

    match = re.search('Cell ', line)
    if match :
        print "Creating a new cell:", line
        ap = AccessPoint()
        aplist.append(ap)

    match = re.search('ESSID:"(\S+)"', line)
    if match and match.group(1) != "<hidden>":
        ap.essid = match.group(1)

    match = re.search('Address: (\S+)', line)
    if match:
        ap.address = match.group(1)

    match = re.search('Encryption key:([onf]+)', line)
    if match:
        if match.group(1) == "off" :
            ap.encrypted = False
        else :
            ap.encrypted = True

    match = re.search('Protocol:(.+)', line)
    if match:
        ap.protocol = match.group(1)

    match = re.search('WPA', line)
    if match:
        ap.encryptionType = "WPA"

for ap in aplist :
    print ap.essid + ":", ap.address, ap.protocol
#     encflag = ""
#     if ap.encrypted :
#         encflag = " -s"
    essidstr = ""
    essidlabel = "(Hidden)"
    if ap.essid != "" :
        # XXX Should look for a known scheme matching the essid
        essidstr = "essid " + ap.essid
        essidlabel = ap.essid

    print "<item label=\"" + essidlabel + "\">"
    if essidstr != "" :
        print "  <action name=\"Execute\">"
        cmdstr = "    <command>echo sudo /etc/network/schemes/netscheme -g " \
            + essidstr
        if ap.encrypted :
            cmdstr += " -p"
        cmdstr += "</command>"
        print cmdstr
#         if ap.encrypted :
#             print "    <command>echo /home/akkana/bin/wifi-connect " \
#                 + interface + " " + essidstr + "</command>"
#         else :
#             print "    <command>echo sudo iwconfig " + interface \
#                 + " " + essidstr + "</command>"
        print "  </action>"
    print "</item>"

print "</openbox_pipe_menu>"



