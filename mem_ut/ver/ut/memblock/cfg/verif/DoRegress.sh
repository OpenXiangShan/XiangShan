#!/bin/bash

#source /etc/profile
source ~/.bash_profile

cmd_info=$1

CurrTime=`date +%m%d_%H%M`
logName=regress_$CurrTime.log

if [ "$cmd_info" == "help" ]; then
    echo "===> ./DoRegress.sh help             >>>>Display the help"
    echo "===> ./DoRegress.sh status           >>>>Get the regress_result.log of current path without coverage infomation"
    echo "===> ./DoRegress.sh status_cov       >>>>Get the regress_result.log of current path with coverage information"
    echo "===> ./DoRegress.sh total            >>>>Get the regress_result.log of total regress results with coverage information"
    echo "===> ./DoRegress.sh /xxx/xxx.ini     >>>>Do regress with /xxx/xxx.ini"
    echo "===> ./DoRegress.sh                  >>>>Do regress with ./regress.ini"
elif [ "$cmd_info" == "status" ]; then
    $PROJ_ROOT/scr/verif/DoRegress.py regress_status
elif [ "$cmd_info" == "status_cov" ]; then
    $PROJ_ROOT/scr/verif/DoRegress.py regress_status_cov
elif [ "$cmd_info" == "total" ]; then
    $PROJ_ROOT/scr/verif/DoRegress.py total_status
elif [ "$cmd_info" == "" ]; then
    $PROJ_ROOT/scr/verif/DoRegress.py ./regress.ini $CurrTime
else
    $PROJ_ROOT/scr/verif/DoRegress.py $cmd_info $CurrTime
fi
