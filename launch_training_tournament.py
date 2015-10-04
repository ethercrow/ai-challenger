#!/usr/bin/env python

import json
import os
import urllib
import urllib2
import subprocess
import sys

HOST = 'http://127.0.0.1:8081'
SERVER_BINARY = './ai-challenger-grid/ai-challenger-grid'

def main():

    dirname = os.path.dirname(os.path.realpath(__file__))
    os.chdir(dirname)

    if not os.path.exists(SERVER_BINARY):
        print "Server binary {0} must exist".format(os.path.abspath(SERVER_BINARY))
        print "Please run 'make release' or extract already compiled package in {0}".format(dirname)
        sys.exit(1)

    if len(sys.argv) != 3:
        print "Usage:"
        print " {0} <map_name> <my bot executable>".format(sys.argv[0])
        print "Available maps: 10x10, 40x40"
        sys.exit(2)

    mybot_exe = os.path.abspath(sys.argv[2])

    subprocess.call(['killall', 'ai-challenger-grid'])
    print 'Launching training tournament for {0}'.format(mybot_exe)
    server = subprocess.Popen('./ai-challenger-grid/ai-challenger-grid')

    try:
        add_bot('my_bot', mybot_exe)
        add_bot('greedy', 'game-grid/greedy.py')
        add_bot('randy', 'game-grid/randy.py')
        launch_tournament(sys.argv[1])
        open_in_browser(HOST + '/dashboard')
        print
        print 'Press Enter to shutdown server'
        print
        sys.stdin.readline()
    finally:
        server.terminate()


def launch_tournament(map_name):
    req = urllib2.Request(HOST + '/launch-training/' + map_name + '/my_bot')
    req.add_header('Accept', 'application/json')
    req.get_method = lambda: 'POST'
    urllib2.urlopen(req)


def add_bot(name, exe):
    exe = os.path.abspath(exe)
    print 'Adding bot {0}: {1}'.format(name, exe)
    try:
        data = json.dumps(
            {'botName':name,
             'botCommunication':
                 {'tag': 'ExecutableBot'
                 ,'contents': exe
                 }
            })
        req = urllib2.Request(HOST + '/add-bot')
        req.add_header('Content-Type', 'application/json')
        req.add_data(data)
        urllib2.urlopen(req)
    except urllib2.HTTPError as e:
        print e.reason
        raise


def open_in_browser(f):
    if sys.platform.startswith('darwin'):
        cmd = 'open'
    elif sys.platform.startswith('win'):
        cmd = 'start'
    else: # it probably has X
        cmd = 'xdg-open'
    subprocess.call([cmd, f])


if __name__ == '__main__':
    main()