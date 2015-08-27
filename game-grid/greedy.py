#!/usr/bin/env python

import sys

def main():
    with open('/tmp/greedy.log', 'w') as logfile:
        log = lambda line: logfile.write(line + '\n')

        for _ in range(101):
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

            def neighborhood4(x, y):
                for dx in (-1, 0, 1):
                    for dy in (-1, 0, 1):
                        if abs(dx + dy) == 1:
                            if 0 <= x + dx < width and 0 <= y + dy < height:
                                yield world[y + dy][x + dx]

            def targets(budget):
                for x in range(width):
                    for y in range(height):
                        if budget <= 0:
                            return
                        if my_id in neighborhood4(x, y) and world[y][x] != my_id:
                            yield (x, y)
                            budget -= 1

            for (x, y) in targets(budget):
                print 'C {0} {1}'.format(x, y)
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
