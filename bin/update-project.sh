#!/bin/sh

# Defaults

default_projects_dir="$HOME/.emacs-projects/"


# Read arguments

name="$1"
if [ -z "$name" ]; then
    name=$(basename `pwd`)
fi
projects_dir="${2:-${default_projects_dir}}"


# Create a temporary project

temp_name="${name}-update-project-temp"
create-project.sh $temp_name


# Now update the target project

temp_project_dir="${projects_dir}${temp_name}/"
project_dir="${projects_dir}${name}/"
id_source="${temp_project_dir}ID"
id_dest="${project_dir}ID"
tags_source="${temp_project_dir}TAGS"
tags_dest="${project_dir}TAGS"
filecache_source="${temp_project_dir}FILECACHE"
filecache_dest="${project_dir}FILECACHE"


echo "Moving ${id_source} to ${id_dest}"
mv "${id_source}" "${id_dest}"

echo "Moving ${tags_source} to ${tags_dest}"
mv "${tags_source}" "${tags_dest}"

echo "Moving ${filecache_source} to ${filecache_dest}"
mv "${filecache_source}" "${filecache_dest}"


# And finally cleanup

echo "Removing ${temp_project_dir}"
rm -rf "${temp_project_dir}"
echo "Done"

exit 0
