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

            def neighborhood4(x, y):
                for dx in (-1, 0, 1):
                    for dy in (-1, 0, 1):
                        if abs(dx + dy) == 1:
                            if 0 <= x + dx < width and 0 <= y + dy < height:
                                yield world[y + dy][x + dx]

            def targets(budget):
                for _ in range(1000):
                    if budget <= 0:
                        return
                    x = random.choice(range(width))
                    y = random.choice(range(height))
                    if my_id in neighborhood4(x, y) and world[y][x] != my_id:
                        yield (x, y)
                        budget -= 1

            for (x, y) in targets(budget):
                print 'C {0} {1}'.format(x, y)

            print 'S ' + random.choice(
                ( 'Hadouken!'
                , 'Shoryuken!'
                , 'Crushit!'
                , 'Scrum!'
                , 'Agile!'
                , 'Continuous delivery!'
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