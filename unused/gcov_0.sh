#!/bin/bash
for file in *.gcov
do
cat $file|grep -q "^ *[0-9]*[0-9]\:" || echo $file
done
