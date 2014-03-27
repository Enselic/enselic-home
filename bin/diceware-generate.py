#!/usr/bin/env python

import sys;
from os import path;
import random;
import math;

if len(sys.argv) < 2:
    desired_count = 5
else:
    desired_count=int(sys.argv[1])

# Credit goes to Magnus Bodin for creating diceware8k-sv.txt, downloaded from
# http://x42.com/diceware/diceware8k-sv.txt
diceware8k_sv = path.join(path.dirname(sys.argv[0]), "..", "x42-com-diceware-diceware8k-sv-magnus-bodin.txt")
diceware = open(diceware8k_sv).readlines()

random = random.SystemRandom();
diceware_len = len(diceware)
result = ""
print "Picking", desired_count, "random words from a list of", diceware_len, "which results in an entropy of", math.log(diceware_len, 2) * desired_count
for _ in range(0, desired_count):
    index_this_time = random.randint(0, diceware_len -1)
    print "Picking word #", index_this_time
    result += str(diceware[index_this_time]).rstrip() + " "

print result
