#-----------------------------------------#
#PROJECT NAME                             #
#-----------------------------------------#
CURR_DIR = $(shell pwd)
PROJECT_NAME =
PROJECT_PATH = ${CURR_DIR}/../../../..
SCR_PATH = ${CURR_DIR}/../cfg/verif

#-------------------------------------------------------------------------------------#
#USER OPTIONS>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#seed                : [rand {fixed value}]
#                    :     assign a fixed seed or set the seed random
#tc                  : assign the simulation TC name
#ts                  : assign the main virtual sequence name
#pl                  : [UVM_DEBUG UVM_FULL UVM_HIGH UVM_MEDIUM UVM_LOW UVM_NONE]
#                    :     assign the uvm info print level
#mode                : assign the env mode
#wave                : [fsdb null]
#                    :     assign dump waveform or not
#timing              : [rtl gate min max typical] assign the simulation timing
#                    :     assign the simulation timing
#ccov                : [on off]
#                    :     assign the code coverage collection enable
#fcov                : [on off]
#                    :     assign the function coverage collection enable
#udr                 : user define option
#udf                 : user micro define
#cfg                 : assign the plus cfg file name without .cfg suffix
#plus_file           : assign the plus cfg file directory
#plus_arg            : user plusarg options
#xprop_op            : [on off] vcs xprop_sim
#timeout_ns          : assing when is the the simulation timeout
#dly_100us_dump_fsdb : while wave=fsdb, assign the start time to dump fsdb
#simv_by_tc          : [on off]
#                    :     assign name of simv named by tc or not
#-------------------------------------------------------------------------------------#
seed     := 666666
tc       := basicTest
ts       := virtual_base_sequence
pl       := UVM_MEDIUM
mode     := base_fun
wave     := on
timing   := rtl
ccov     := off
fcov     := off
udr      :=
udf      :=
note     :=
plus_file:= ../seq/plus_cfg
cfg      := default
plus_arg :=
timeout_ns := 100000000
simv_by_tc := off
xprop := off
partcmp_op := on
initreg := off
regr_ini := regress
time_mem_chk := off
gui_on := off


ifeq ($(seed),rand)
seed_tmp = $$$$
else
seed_tmp = ${seed}
endif

#-----------------------------------------#
#top tb name                              #
#-----------------------------------------#
SIM_TOOLS = xrun
TOP_NAME = top_tb

TIMESCALE = 1ns/1ps

#-----------------------------------------#
#FILELIST PATH                            #
#-----------------------------------------#
ifeq ($(timing),rtl)
RTL_LIST = -f ../cfg/rtl.f
endif
ifeq ($(timing),gate)
RTL_LIST = -f ../cfg/netlist.f
endif
ifeq ($(timing),min)
RTL_LIST = -f ../cfg/netlist.f
endif
ifeq ($(timing),max)
RTL_LIST = -f ../cfg/netlist.f
endif
ifeq ($(timing),typical)
RTL_LIST = -f ../cfg/netlist.f
endif

TB_LIST  = -f ../cfg/tb.f

#-----------------------------------------#
#plat OPTIONS                             #
#-----------------------------------------#
SHELL           = /bin/bash
BITS            = 64
UVM_VER         = uvm-1.2

#-----------------------------------------#
#LOG                                      #
#-----------------------------------------#
CMPRTL_LOG = -l ./${mode}/log/$(SIM_TOOLS)_compile_$(timing).log
EXPORT_OPTS =
ifneq ($(note),)
SIMV_LOG = -l ./${mode}/log/tc=${tc}_ts=${ts}_cfg=${cfg}_seed=$(strip $(seed_tmp))_${timing}_${note}.log
EXPORT_OPTS += export WAVE_FILE=./${mode}/wave/tc=${tc}_ts=${ts}_cfg=${cfg}_seed=$(strip $(seed_tmp))_${timing}_${note}
else
SIMV_LOG = -l ./${mode}/log/tc=${tc}_ts=${ts}_cfg=${cfg}_seed=$(strip $(seed_tmp))_${timing}.log
EXPORT_OPTS += export WAVE_FILE=./${mode}/wave/tc=${tc}_ts=${ts}_cfg=${cfg}_seed=$(strip $(seed_tmp))_${timing}
endif

COMPILE_MACRO += +define+TCNT_USE_UVM12
IF_ADD_DLY_OPTIONS =

SYSC_COMP_OPTS =

include ${SCR_PATH}/project_cfg_$(SIM_TOOLS).mk

# interface delay define
ifeq ($(SIM_TOOLS),vcs)
CMP_OPTIONS += +define+DEF_SETUP_TIME=1step
CMP_OPTIONS += +define+DEF_HOLD_TIME=1step
VRD_OPTIONS += +define+DEF_SETUP_TIME=1step
VRD_OPTIONS += +define+DEF_HOLD_TIME=1step
else
CMP_OPTIONS += +define+DEF_SETUP_TIME=1
CMP_OPTIONS += +define+DEF_HOLD_TIME=1
VRD_OPTIONS += +define+DEF_SETUP_TIME=1
VRD_OPTIONS += +define+DEF_HOLD_TIME=1
endif
CMP_OPTIONS += $(IF_ADD_DLY_OPTIONS)
VRD_OPTIONS += $(IF_ADD_DLY_OPTIONS)

include ../cfg/extern_declare_cfg.mk
ifeq ($(SIM_TOOLS),xrun)
ifneq ($(wildcard $(COV_EX_OPTION)/*.vRefine),)
COV_EL_CMD = load -refinement ${COV_EX_OPTION}/*.vRefine
endif
endif

# common command
# compile & simulation
run: compile batch_run

sysc_comp:
	$(SYSC_COMP_OPTS)

compile:test_dir sysc_comp
	${SIM_TOOLS} ${CMP_OPTIONS} ${RTL_LIST} ${TB_LIST}

batch_run:test_dir
	${RUN_CMD}

rtl:test_dir
	${SIM_TOOLS} ${CMP_OPTIONS} ${RTL_LIST}

clean :
	rm -rf ${mode}

test_dir :
	@mkdir -p ${mode}
	@mkdir -p ${mode}/log
	@mkdir -p ${mode}/exec
	@mkdir -p ${mode}/wave
	@mkdir -p ${mode}/cov

# wave
wave :test_dir
	${WAV_CMD}

wave0 :test_dir
	${WAV0_CMD}

wave_ep:
	${WAV_EP_CMD}

indago:
	${INDAGO_CMD}

# coverage
cov_gui:test_dir
	${COV_GUI_CMD}

cov_txt:test_dir
	${COV_TXT_CMD}

cov_txt_sum:test_dir
	${COV_TXT_SUM_CMD}

# regress
do_regr:
	ln -sf ${SCR_PATH}/DoRegress.py ./
	ln -sf ../regress/${regr_ini}.ini ./
	./DoRegress.py ./${regr_ini}.ini

get_regr:
	ln -sf ${SCR_PATH}/DoRegress.py ./
	./DoRegress.py regress_status

get_total:
	ln -sf ${SCR_PATH}/DoRegress.py ./
	./DoRegress.py total_status

get_regr_cov:
	ln -sf ${SCR_PATH}/DoRegress.py ./
	./DoRegress.py regress_status_cov

include ../cfg/extern_cfg.mk
