#!/bin/bash

# This program will only work with bash(1)

# configs
spec_name=spec06_rv64gcb_o2_20m
spec_dir=/nfs-nvme/home/share/checkpoints_profiles/${spec_name}/take_cpt
thread_num=16
emu=$1

# environment preparation
dir=$(dirname $(readlink -f "$0"))
mkdir -p ${dir}/${spec_name}/${emu}.dir/csv
mkdir -p ${dir}/${spec_name}/${emu}.dir/html

# check python
python=python
[ -z "`whereis python3 | grep /`" ] || python=python3

# setup fifo
fifo_file=/tmp/$$.fifo
mkfifo "${fifo_file}"
exec 6<>"${fifo_file}"
for i in $(seq 1 ${thread_num}); do echo; done >&6

# run emus
i=0
for file in $(cat file.f); do
  gz=$(ls ${spec_dir}/${file}/0/)
  j=$(($i % 128))
  read -u6
  {
    ./xsrun ${dir}/emus/${emu} -W 20000000 -I 40000000 -i ${spec_dir}/${file}/0/${gz} -s 7541 --diff=${NOOP_HOME}/ready-to-run/riscv64-nemu-interpreter-so 2>${dir}/${spec_name}/${emu}.dir/${file}.log
    if [ $? -eq 0 ]; then
      sed "1,$(($(cat ${dir}/${spec_name}/${emu}.dir/${file}.log | wc -l) / 2))d" ${dir}/${spec_name}/${emu}.dir/${file}.log >${dir}/${spec_name}/${emu}.dir/csv/${file}.log
      ${dir}/top-down.sh ${dir}/${spec_name}/${emu}.dir/csv/${file}.log
      rm ${dir}/${spec_name}/${emu}.dir/csv/${file}.log
      $python ${dir}/top_down.py ${file} ${dir}/${spec_name}/${emu}.dir ${emu} # python ./top_down.py title dir suffix
    fi
    echo >&6
  } &
  sleep 2s
  i=$(($i + 8))
done

wait
exec 6>&-
rm -f ${fifo_file}
