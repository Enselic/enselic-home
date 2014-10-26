#!/usr/bin/python

# Generate a file with "SHA1 dir/filename.ext" pairs on each line with this command (OS X):
# time find . -name '.Trash' -prune -o -iname '*.jpg' -exec shasum {} + | /Users/martin/enselic-home/bin/list-duplicate-sha1.py

import sys
import fileinput


class FileNode:
    def __init__(self, name):
        self.name = name
        self.children = {};
        self.n_children_with_duplicates = 0
        self.full_name = None
        self.parent = None
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
    current_full_name = ""
    for segment in segments:
        current_full_name += segment + "/"
        if not segment in current_node.children:
            new_node = FileNode(segment)
            new_node.full_name = current_full_name
            new_node.parent = current_node
            current_node.children[segment] = new_node
        current_node = current_node.children[segment]

    current_node.full_name = filename
    current_node.duplicates = duplicates_list


dirs_with_duplicates = []
def print_node(node, indent=""):
    # Print name
    sys.stdout.write("\n")
    sys.stdout.write(indent + node.name)

    # Depth first recurse
    for child in node.children:
        print_node(node.children[child], indent + "  ")

    if len(node.children) > 0 and node.n_children_with_duplicates == len(node.children):
        dirs_with_duplicates.append(node.full_name)
        if node.parent is not None:
            node.parent.n_children_with_duplicates += 1

    if node.duplicates is not None:
        duplicates_without_self = list(node.duplicates)
        duplicates_without_self.remove(node.full_name)
        n_duplicates = len(duplicates_without_self)
        if n_duplicates > 0:
            sys.stdout.write(" [" + str(n_duplicates) + "] [" + ' '.join(duplicates_without_self) + "]")
            if node.parent is not None:
                node.parent.n_children_with_duplicates += 1

print_node(root)

print("\n\nDirs with only duplicates:")
for d in dirs_with_duplicates:
    print d

