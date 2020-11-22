#!/bin/bash

log_dir=$1
tage_w_sc_w=$(grep "scUpdate" $log_dir | grep "sc(1), tage(1)" -c)
tage_w_sc_r=$(grep "scUpdate" $log_dir | grep "sc(0), tage(1)" -c)
tage_r_sc_w=$(grep "scUpdate" $log_dir | grep "sc(1), tage(0)" -c)
tage_r_sc_r=$(grep "scUpdate" $log_dir | grep "sc(0), tage(0)" -c)

echo $tage_r_sc_w tage right but mispredicted by sc
echo $tage_w_sc_r tage wrong and rectified by sc
echo `expr $tage_w_sc_w + $tage_r_sc_r` branches remain unchanged, in which $tage_w_sc_w are wrong

