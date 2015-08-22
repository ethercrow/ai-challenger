#!/usr/bin/env python

import sys

for _ in range(101):
    x = random.choice(range(10))
    y = random.choice(range(10))
    print 'C {0} {1}'.format(x, y)
    print '.'
    sys.stdout.flush()