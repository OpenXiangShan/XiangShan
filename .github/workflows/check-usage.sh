#!/bin/bash

echo $1
echo $2
grep -rn $1 $2/src/main/scala/xiangshan
if [[ $? == 0 ]];
then
	exit 1
fi
exit 0
