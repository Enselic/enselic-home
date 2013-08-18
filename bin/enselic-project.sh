#!/bin/bash

project_name="$2"
if [ -z "$project_name" ]; then
    project_name=$(basename `pwd`)
fi

case $1 in
    "create" )
        create-project.sh "$project_name" ;;
    "update" )
        update-project.sh "$project_name" ;;
esac
