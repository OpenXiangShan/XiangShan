#database -open waves -into ./wave/$env(TC_NAME_S)_$env(SEED_S).shm -default
#probe -create -shm -all -depth all -memories
#run
#exit
#
database -open -shm shmdb -incsize 6G  -into $env(WAVE_FILE).shm -default
probe -create -shm -all -depth all -memories
run
exit
#
#run 1000ns
#database -open -shm shmdb -into ./wave/$env(TC_NAME_S)_$env(SEED_S).shm -default
#probe -create -shm -all -depth all
#run
#exit
#
#database -open waves -into ./wave/$env(TC_NAME_S)_$env(SEED_S).shm -default
#probe -create top_tb.u_fpu_top.u_fpu_knll_top -shm -all -depth all
#run
#exit
#
