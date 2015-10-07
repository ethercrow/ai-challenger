#!/usr/bin/env python

import random
import sys

def main():
    with open('/tmp/randy.log', 'w') as logfile:
        log = lambda line: logfile.write(line + '\n')

        for _ in range(201):
            id_line = sys.stdin.readline().rstrip().split(' ')
            assert id_line[0] == 'Y'
            my_id = id_line[1]
            log('My id: {0}'.format(my_id))

            energy_line = sys.stdin.readline().rstrip().split(' ')
            assert energy_line[0] == 'E'
            budget = int(energy_line[1])

            world = list(read_world())

            height = len(world)
            assert height > 0

            width = len(world[0])
            assert width > 0

            print 'S ' + random.choice(
                ( 'Before you jump you must look'
                , 'Before you look you must think'
                , 'Before you think you must feel'
                ))
            print '.'
            sys.stdout.flush()


def read_world():
    while True:
        l = sys.stdin.readline()
        if l == '.\n':
            return
        else:
            yield l.rstrip()


if __name__ == '__main__':
    main()