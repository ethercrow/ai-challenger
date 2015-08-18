#!/usr/bin/env python

import random
import sys

for _ in range(101):
    print random.choice("RPS")
    print "."
    sys.stdout.flush()