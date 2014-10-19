#!/usr/bin/python

import sys
import fileinput

sha1_to_file = {}
for line in fileinput.input():
    line = line.rstrip('\n')
    sha1 = line[0:43]
    filename = line[44:]
    if sha1 in sha1_to_file:
        print(filename + " SAMEAS " + sha1_to_file[sha1])
    else:
        sha1_to_file[sha1] = filename;


