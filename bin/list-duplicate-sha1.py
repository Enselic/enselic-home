#!/usr/bin/python

import sys
import fileinput


class FileNode:
    def __init__(self, name):
        self.name = name
        self.children = {};
        self.full_name = None
        self.duplicates = None

root = FileNode("<root>")

sha1_to_file = {}
for line in fileinput.input():
    line = line.rstrip('\n')
    sha1 = line[0:43]
    filename = line[44:]

    if sha1 in sha1_to_file:
        sha1_to_file[sha1].append(filename)
    else:
        sha1_to_file[sha1] = [filename]
    duplicates_list = sha1_to_file[sha1]

    segments = filename.split("/")
    current_node = root
    for segment in segments:
        if not segment in current_node.children:
            current_node.children[segment] = FileNode(segment)
        current_node = current_node.children[segment]

    current_node.full_name = filename
    current_node.duplicates = duplicates_list


def print_node(node, indent=""):
    # Print name
    sys.stdout.write("\n")
    sys.stdout.write(indent + node.name)

    # Depth first recurse
    for child in node.children:
        print_node(node.children[child], indent + "  ")

    if node.duplicates is not None:
        duplicates_without_self = list(node.duplicates)
        duplicates_without_self.remove(node.full_name)
        if len(duplicates_without_self) > 0:
            sys.stdout.write("  [" + ' '.join(duplicates_without_self) + "]")

print_node(root)
