VCS_COV_TOOLS = verdi

#-----------------------------------------#
#VCS TIMING DEFINE                        #
#-----------------------------------------#
#+warn=none 可以disable所有warning信息的打印
ifeq ($(timing),rtl)
TIMING_OPTIONS = +notimingcheck
TIMING_OPTIONS += +nospecify      #IP lib maybe use specify
TIMING_OPTIONS += +define+RTL_SIM
endif
ifeq ($(timing),gate)
TIMING_OPTIONS = +notimingcheck +nospecify
TIMING_OPTIONS += +define+GATE_SIM
endif
ifeq ($(timing),min)
TIMING_OPTIONS = +neg_tchk -negdelay
TIMING_OPTIONS += +define+MIN_SDF
endif
ifeq ($(timing),max)
TIMING_OPTIONS = +neg_tchk -negdelay
TIMING_OPTIONS += +define+MAX_SDF
endif
ifeq ($(timing),typical)
TIMING_OPTIONS = +neg_tchk -negdelay
TIMING_OPTIONS += +define+TYPICAL_SDF
endif

##coverage option
COVER_OPTIONS = -cm line+cond+fsm+tgl
COVER_TYPE = -cm_line contassign -cm_cond allops
COVER_DEFINE = +define+FCOV
COVER_DIR = -cm_dir ${mode}/cov/simv.vdb/vcs_cov/simv_rtl.cm
COV_CM_NAME = -cm_name tc=${tc}_ts=${ts}_cfg=${cfg}_seed=$(strip $(seed_tmp))

ifeq ($(simv_by_tc),on)
CMP_OUT_SIMV = ./${mode}/exec/${tc}_simv
CSRC_FILE = ./${mode}/exec/${tc}_csrc
else
CMP_OUT_SIMV = ./${mode}/exec/simv
CSRC_FILE = ./${mode}/exec/csrc
endif

#CMP_OPTIONS += -top vcs_topcfg
ifneq ($(partcmp_op),on)
CMP_OPTIONS += -top ${TOP_NAME}
endif
CMP_OPTIONS += ${COMPILE_MACRO}
#-kdb时用于生产verdi GUI debug时使用的数据库，如果不需要关联design debug以及interactive debug，可以不适用该选项，会影响compile性能
#如果只需要下载波形文件，使用-debug_access即可
#CMP_OPTIONS += -full64 +vpi -sverilog +v2k -lca -debug_access+all -kdb +libext+.v -LDFLAGS -Wl,--no-as-needed
#CMP_OPTIONS += -full64 +vpi -sverilog +v2k -lca -debug_access+all +libext+.v -LDFLAGS -Wl,--no-as-needed
CMP_OPTIONS += -full64 +vpi -sverilog +v2k -debug_access+all +libext+.v -LDFLAGS -Wl,--no-as-needed
CMP_OPTIONS +=+vcs+lic+wait
ifeq ($(wave),on)
CMP_OPTIONS += -kdb -lca
endif
CMP_OPTIONS += -timescale=${TIMESCALE}
#CMP_OPTIONS += -override_timescale=${TIMESCALE}
CMP_OPTIONS += +lint=TFIPC-L +lint=PCWM
CMP_OPTIONS += -Mdir=${CSRC_FILE}
CMP_OPTIONS += ${TIMING_OPTIONS}
CMP_OPTIONS += -ntb_opts ${UVM_VER}
CMP_OPTIONS += ${udf}
CMP_OPTIONS += ${DPILIB}
VRD_OPTIONS += ${udf}
ifeq ($(ccov),on)
CMP_OPTIONS += ${COVER_OPTIONS} ${COVER_TYPE}
endif
ifeq ($(fcov),on)
CMP_OPTIONS += ${COVER_DEFINE}
endif
CMP_OPTIONS += ${COVER_DIR}
CMP_OPTIONS += -o ${CMP_OUT_SIMV} ${CMPRTL_LOG}
#-----------------------------------------#
#Initreg sim
#-----------------------------------------#
ifeq ($(initreg),on)
CMP_OPTIONS += +vcs+initreg+config+$(INITREG_CFG_FILE)
SIMV_OPTIONS += +vcs+initreg+config+$(INITREG_CFG_FILE)
endif
#-----------------------------------------#
#Xprop sim
#default is tmerge, but xmerge is more pessimistic than a standard gate-level simulation, X can transmit to interface
#-----------------------------------------#
ifeq ($(xprop),on)
CMP_OPTIONS += -xprop=xmerge
endif

#-----------------------------------------#
#Partition Compile
#-----------------------------------------#
ifeq ($(partcmp_op),on)
CMP_OPTIONS += -partcomp -fastpartcomp=j4 -pcmakeprof ../cfg/vcs_topcfg.v -top vcs_topcfg
CMP_OPTIONS += -partcomp_dir="./${mode}/partitionlib"
endif

#-----------------------------------------#
#Simprofile Time and Memory Check
#-----------------------------------------#
ifeq ($(time_mem_chk),on)
CMP_OPTIONS += -simprofile
SIMV_OPTIONS += -simprofile time+mem
#### you can get html report by:
#### profrpt -output report.dir -view mem_summary simprofile_dir
endif

#CLI_SEL = +cli+3
#SIMV_OPTIONS += ${CLI_SEL}
SIMV_OPTIONS += +ntb_random_seed=$(seed_tmp)
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

SIMV_OPTIONS += ${COVER_OPTIONS}
SIMV_OPTIONS += ${COVER_DIR}
SIMV_OPTIONS += ${COV_CM_NAME}
SIMV_OPTIONS += +fsdb+gate=off #close

DUMP_WAVE_CFG_FILE = dump_wave_cfg_vcs.tcl
ifeq ($(wave),on)
SIMV_OPTIONS += +wave_type=fsdb
SIMV_OPTIONS += +gen_wave=${timing}
SIMV_OPTIONS += -ucli -i ../cfg/vcs_mk/$(DUMP_WAVE_CFG_FILE)
endif

RUN_CMD = ${EXPORT_OPTS} && ${CMP_OUT_SIMV} ${SIMV_OPTIONS} ${SIMV_LOG}
WAV_CMD = verdi -dbdir ${CMP_OUT_SIMV}.daidir
WAV0_CMD = verdi ${VRD_OPTIONS} ${CMP_OPTIONS} ${RTL_LIST} ${TB_LIST} -top ${TOP_NAME} -2001 -ssf &
WAV_EP_CMD = verdi -2001 -ssf &
INDAGO_CMD = @echo "This is vcs@Synopsy, cannot open wave by indago@Cadence!!!"

COV_GUI_CMD = dve -full64 -covdir ${mode}/cov/simv.vdb/vcs_cov/*.vdb ${COV_ADD_MERGE} ${COV_EX_OPTION}  &
ifeq ($(VCS_COV_TOOLS),verdi)
COV_GUI_CMD = verdi -cov -covdir ${mode}/cov/simv.vdb/vcs_cov/simv_rtl.cm.vdb ${COV_ADD_MERGE} ${COV_EX_OPTION}  &
endif
ifeq ($(VCS_COV_TOOLS),dve)
COV_GUI_CMD = dve -full64 -covdir ${mode}/cov/simv.vdb/vcs_cov/*.vdb ${COV_ADD_MERGE} ${COV_EX_OPTION} &
endif
COV_TXT_CMD = urg -full64 -dir ${mode}/cov/simv.vdb/vcs_cov/*.vdb ${COV_ADD_MERGE} -metric line+cond+fsm+tgl+group+assert  -format text ${COV_EX_OPTION}
COV_TXT_SUM_CMD = urg -full64 -dir */cov/simv.vdb/vcs_cov/*.vdb ${COV_ADD_MERGE} -metric line+cond+fsm+tgl+group -format text
