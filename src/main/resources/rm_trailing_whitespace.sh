#!/usr/bin/env bash

SRC_DIR=$1

echo $1 > /tmp/rm_result

if [[ -z "$1" ]]; then
    SRC_DIR="../.."
fi

# find all java files and remove trailing ws (using system dependent sed command)
OS=`uname -s`
if [[ $OS == "Linux" ]]; then
    find $SRC_DIR -type f -name "*.java" -print0 | xargs -0 sed -i -r -e 's/\s+$//g'
elif [[ $OS == "Darwin" ]]; then
    find "$SRC_DIR" -type f -name "*.java" -print0 | xargs -0 sed -E -e "s/[[:space:]]+$//" -i ''
fi