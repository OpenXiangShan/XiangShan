#-----------------------------------------#
#XRUN TIMINT DEFINE                       #
#-----------------------------------------#
#+warn=none \u53ef\u4ee5disable\u6240\u6709warning\u4fe1\u606f\u7684\u6253\u5370
wave_tools = simvision
ifeq ($(timing),rtl)
TIMING_OPTIONS = -notimingchecks
TIMING_OPTIONS += -nospecify
TIMING_OPTIONS += +define+RTL_SIM
endif
ifeq ($(timing),gate)
TIMING_OPTIONS = -notimingchecks
TIMING_OPTIONS += -nospecify
TIMING_OPTIONS += +define+GATE_SIM
endif
ifeq ($(timing),min)
TIMING_OPTIONS = +neg_tchk #TODO for min|max|typical
TIMING_OPTIONS += +define+MIN_SDF
endif
ifeq ($(timing),max)
TIMING_OPTIONS = +neg_tchk
TIMING_OPTIONS += +define+MAX_SDF
endif
ifeq ($(timing),typical)
TIMING_OPTIONS = +neg_tchk
TIMING_OPTIONS += +define+TYPICAL_SDF
endif

##coverage option
COVER_OPTIONS = B:E:F:T    #block:expr:fsm:toggle
COVER_DEFINE = +define+FCOV
COVER_DIR    = ${mode}/cov
COV_CM_NAME  = ${tc}_$(strip $(seed_tmp))
XRUN_COV_CFG_FILE = cov_xrun.cfg

#modify for xrun
ifeq ($(simv_by_tc),on)
CMP_OPTIONS += -xmlibdirname  ./${mode}/exec/$(tc)_$(seed)
else
CMP_OPTIONS += -xmlibdirname ./${mode}/exec/tc_exec
endif
CMP_OPTIONS += ${CMPRTL_LOG}

CMP_OPTIONS += -top ${TOP_NAME}
CMP_OPTIONS += ${COMPILE_MACRO}
CMP_OPTIONS += -c -$(BITS)  -sv -sysv -uvmhome CDNS-1.2 -access rwc -fsmdebug +libext+.v

#for print statistics mem and cpu usage xnf @20210922
CMP_OPTIONS  += -STATUS -STATUS3
SIMV_OPTIONS += -STATUS -STATUS3

#for basic profiler debug xnf @20210922
#ifeq ($(prof_b_on),on)
ifeq ($(time_mem_chk),on)
CMP_OPTIONS   += -profile
SIMV_OPTIONS  += -profile
endif

#for advanced profiler debug xnf @20210922
#ifeq ($(prof_a_on),on)
ifeq ($(time_mem_chk),on)
CMP_OPTIONS   += -xprof
SIMV_OPTIONS  += -xprof
endif

ifeq ($(gui_on),on)
CMP_OPTIONS    += -linedebug
SIMV_OPTIONS   += -gui
endif

ifeq ($(wave_tools),indago)
CMP_OPTIONS += -lwdgen -source_debug -persistent_sources_debug -abvevalnochange
DUMP_WAVE_CFG_FILE = dump_wave_cfg_xrun_indago.tcl
else
DUMP_WAVE_CFG_FILE = dump_wave_cfg_xrun.tcl
endif
DEBUG_OPTS      = -input ../cfg/xrun_mk/$(DUMP_WAVE_CFG_FILE)
WAVE_OPTS =
ifeq ($(wave),on)
WAVE_OPTS +=  $(DEBUG_OPTS)
endif

ifneq ($(note),)
WAVE_DB = ./${mode}/wave/${tc}_$(strip $(seed_tmp))_${timing}_${note}.db
else
WAVE_DB = ./${mode}/wave/${tc}_$(strip $(seed_tmp))_${timing}.db
endif

CMP_OPTIONS += -timescale ${TIMESCALE}
CMP_OPTIONS += ${TIMING_OPTIONS}
CMP_OPTIONS += ${udf}
CMP_OPTIONS += ${DPILIB}

ifeq ($(ccov),on)
CMP_OPTIONS += -coverage  ${COVER_OPTIONS}
endif
ifeq ($(fcov),on)
CMP_OPTIONS += ${COVER_DEFINE}
CMP_OPTIONS += -coverage  U  #functional
endif
ifneq ($(findstring on,$(ccov)$(fcov)),)
CMP_OPTIONS  += -covdut $(TOP_NAME)
CMP_OPTIONS += -covfile ../cfg/xrun_mk/$(XRUN_COV_CFG_FILE)
endif


# Add udp output check
CMP_OPTIONS += -sequdp_nba_delay

# add for macro outside ``
CMP_OPTIONS += -vlogcontrolrelax UMINQT

# Add wait lic
CMP_OPTIONS += -licqueue
SIMV_OPTIONS += -licqueue

ifeq ($(simv_by_tc),on)
SIMV_OPTIONS += -R -xmlibdirname ./${mode}/exec/$(tc)_$(seed_tmp)
else
SIMV_OPTIONS += -R -xmlibdirname ./${mode}/exec/tc_exec
endif
SIMV_OPTIONS += ${SIMV_LOG}

SIMV_OPTIONS += -svseed $(seed_tmp)

SIMV_OPTIONS += +UVM_TESTNAME=${tc}
SIMV_OPTIONS += +UVM_VERBOSITY=${pl}
SIMV_OPTIONS += +TEST_MODE=${mode}
SIMV_OPTIONS += ${udr}
SIMV_OPTIONS += +UVM_TIMEOUT=${timeout_ns}
SIMV_OPTIONS += +VSEQ_MAIN=${ts}
ifneq ($(strip $(plus_file)),)
ifneq ($(wildcard $(plus_file)/$(cfg).cfg),)
SIMV_OPTIONS += $(shell awk -v plus_arg='$(plus_arg)' 'BEGIN { split(plus_arg, raw, /[ \t]+/); for (i in raw) { if (raw[i] ~ /^\+[A-Za-z0-9_]+=/) { key=raw[i]; sub(/=.*/, "", key); override[key]=1 } } ORS=" " } /^[ \t]*\+[A-Za-z0-9_]+=/ { arg=$$1; key=arg; sub(/=.*/, "", key); if (!(key in override)) print arg }' $(plus_file)/$(cfg).cfg)
endif
endif
SIMV_OPTIONS += ${plus_arg}

ifneq ($(findstring on,$(ccov)$(fcov)),)
SIMV_OPTIONS += -covworkdir ${COVER_DIR}
SIMV_OPTIONS += -covoverwrite
SIMV_OPTIONS += -covscope $(TOP_NAME)
SIMV_OPTIONS += -covtest  ${COV_CM_NAME}
SIMV_OPTIONS += -write_metrics
endif

ifeq ($(initreg),on)
CMP_OPTIONS  += -always_trigger
CMP_OPTIONS  += -xminitialize worklib.top
SIMV_OPTIONS += -xminitialize rand_2state:$(seed_tmp) -xmhierarchy $(INSTANCE_NAME)
endif

ifeq ($(xprop),on)
CMP_OPTIONS += -xprop F
endif

CMP_OPTIONS += +define+USE_CADENCE_TOOL

RUN_CMD = ${EXPORT_OPTS} && xrun ${SIMV_OPTIONS} ${WAVE_OPTS}
WAV_CMD = ln -sf ./$(mode)/exec/tc_exec xcelium.d && simvision -$(BITS) -layout rtldesign -snapshot worklib.top_tb:sv &
WAV0_CMD = make compile && ln -sf ./$(mode)/exec/tc_exec xcelium.d && simvision -$(BITS) -layout rtldesign &
WAV_EP = tesimvision -$(BITS) -layout rtldesign &
INDAGO_CMD = indago -db $(WAVE_DB) &

COV_MERGE_MODE = imc -execcmd "merge ./${mode}/cov/top_tb/* -out TB_ALL -overwrite"
COV_MERGE_ALL = imc -execcmd "merge */cov/top_tb/* -out TB_ALL -overwrite"
COV_LOAD_CMD = load ./cov_work/scope/TB_ALL;
COV_EL_CMD =

COV_TXT_MODE = ${mode}_cov_report.txt
COV_TXT_ALL = cov_report.txt
COV_GENTXT_CMD = report -summary -metrics overall|block|expression|toggle|fsm|functional -cumulative on -grading covered -local off -text -out
COV_GENCSV_CMD = csv_export -bins -overwrite -out
COV_CSV_MODE = ${mode}_all_bins
COV_CSV_ALL = all_bins

COV_GUI_CMD = ${COV_MERGE_MODE} && imc -load cov_work/scope/TB_ALL  -load_refinement ${COV_EX_OPTION}/"*.vRefine" &
COV_TXT_CMD = ${COV_MERGE_MODE} && imc -execcmd "${COV_LOAD_CMD}; ${COV_EL_CMD}; ${COV_GENTXT_CMD} ${COV_TXT_MODE}; ${COV_GENCSV_CMD} ${COV_CSV_MODE}"
COV_TXT_SUM_CMD = ${COV_MERGE_ALL} && imc -execcmd "${COV_LOAD_CMD}; ${COV_EL_CMD}; ${COV_GENTXT_CMD} ${COV_TXT_ALL}; ${COV_GENCSV_CMD} ${COV_CSV_ALL}"
