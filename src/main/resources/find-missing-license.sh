#!/bin/bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
DIR="$SCRIPT_DIR/../../[mt]*/"

for i in $(find $DIR -iname *.java)
do
  grep "General Public License" $i > /dev/null
  STATUS=$?

  if [ $STATUS -ne 0 ]
  then
    REALPATH=$(readlink -m $i)
    echo "[WARNING] $REALPATH lacks license header"
  fi
done
