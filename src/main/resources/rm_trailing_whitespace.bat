@echo off
bash -c "/usr/bin/find ./src -type f -name "*.java" -print0 | xargs -0 sed -E -b -e \"s/\s+$//g\" -i "