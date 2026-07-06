global env
#fsdbAutoSwitchDumpfile 500 "aaa.fsdb"
fsdbDumpfile "$env(WAVE_FILE).fsdb"
fsdbDumpvars 0 "top_tb"
fsdbDumpMDA 0 "top_tb"
fsdbDumpflush
#fsdbDumpoff
#run 10ns
fsdbDumpon
#run 20us
#fsdbDumpoff
run
quit -sim
