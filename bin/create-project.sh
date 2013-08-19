#!/bin/sh

project_name="$1"
if [ -z "$project_name" ]; then
    project_name=$(basename `pwd`)
fi

emacs -batch -l ~/.emacs -f programming-project-batch-create "$project_name"

