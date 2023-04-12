#!/bin/bash

mem_script=$1
conf_file=$2
output_dir=$3

IFS=$'\n'
for line in `cat $conf_file`; do
  file=`echo "$line" | grep -oP '(?<=name )[^ ]*(?= .*)'`
  echo $line >${conf_file}.tmp
  ${mem_script} ${conf_file}.tmp -o ${output_dir}/${file}.v
done

rm ${conf_file}.tmp
