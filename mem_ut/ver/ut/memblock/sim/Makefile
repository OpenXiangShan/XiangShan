##=========================================================
##File name    : Makefile
##Author       : OpenAI_Codex
##Module name  : Makefile
##Discribution : makefile script
##Date         : 2026-04-12
##=========================================================

#include ../../../../scr/verif/project_cfg.mk
include ../cfg/verif/project_cfg.mk

USER_CFG_LOCAL ?= ../cfg/user_cfg.local.sv
USER_CFG_LOCAL_TEMPLATE ?= ../cfg/user_cfg.local.default.sv

REMOTE_HOST ?= eda01
REMOTE_WORKDIR ?= $(CURR_DIR)
REMOTE_ENTRY ?= $(REMOTE_WORKDIR)/eda01_entry.sh
REMOTE_STATE_DIR ?= $(CURR_DIR)/.eda_remote
REMOTE_PID_FILE ?= $(REMOTE_STATE_DIR)/$(tc)_$(ts)_$(mode).pid
REMOTE_LAUNCH_LOG ?= $(REMOTE_STATE_DIR)/$(tc)_$(ts)_$(mode).launch.log
REMOTE_BOOTSTRAP ?= source /usr/share/Modules/init/bash >/dev/null 2>&1 || true; module load synopsys/vcs/Q-2020.03-SP2 license; module load synopsys/verdi/R-2020.12-SP1 license
REMOTE_WRAPPER := $(CURR_DIR)/remote_eda_make.sh
REMOTE_SIM_TOOLS ?= vcs

.PHONY: eda_compile eda_batch_run eda_run eda_run_bg eda_status eda_tail eda_kill ensure_user_cfg_local

compile batch_run rtl: ensure_user_cfg_local

ensure_user_cfg_local:
	@if [ ! -f "$(USER_CFG_LOCAL)" ]; then \
		cp "$(USER_CFG_LOCAL_TEMPLATE)" "$(USER_CFG_LOCAL)"; \
		echo "Created $(USER_CFG_LOCAL) from $(USER_CFG_LOCAL_TEMPLATE)"; \
	fi

eda_compile:
	@mkdir -p $(REMOTE_STATE_DIR)
	@REMOTE_HOST='$(REMOTE_HOST)' REMOTE_WORKDIR='$(REMOTE_WORKDIR)' REMOTE_ENTRY='$(REMOTE_ENTRY)' \
	REMOTE_PID_FILE='$(REMOTE_PID_FILE)' REMOTE_LAUNCH_LOG='$(REMOTE_LAUNCH_LOG)' \
	REMOTE_BOOTSTRAP='$(REMOTE_BOOTSTRAP)' \
	seed='$(seed)' tc='$(tc)' ts='$(ts)' pl='$(pl)' mode='$(mode)' wave='$(wave)' timing='$(timing)' \
	ccov='$(ccov)' fcov='$(fcov)' udr='$(udr)' udf='$(udf)' note='$(note)' \
	cfg='$(cfg)' plus_file='$(plus_file)' plus_arg='$(plus_arg)' \
	timeout_ns='$(timeout_ns)' simv_by_tc='$(simv_by_tc)' xprop='$(xprop)' \
	partcmp_op='$(partcmp_op)' initreg='$(initreg)' regr_ini='$(regr_ini)' \
	time_mem_chk='$(time_mem_chk)' gui_on='$(gui_on)' SIM_TOOLS='$(REMOTE_SIM_TOOLS)' \
	'$(REMOTE_WRAPPER)' sync compile

eda_batch_run:
	@mkdir -p $(REMOTE_STATE_DIR)
	@REMOTE_HOST='$(REMOTE_HOST)' REMOTE_WORKDIR='$(REMOTE_WORKDIR)' REMOTE_ENTRY='$(REMOTE_ENTRY)' \
	REMOTE_PID_FILE='$(REMOTE_PID_FILE)' REMOTE_LAUNCH_LOG='$(REMOTE_LAUNCH_LOG)' \
	REMOTE_BOOTSTRAP='$(REMOTE_BOOTSTRAP)' \
	seed='$(seed)' tc='$(tc)' ts='$(ts)' pl='$(pl)' mode='$(mode)' wave='$(wave)' timing='$(timing)' \
	ccov='$(ccov)' fcov='$(fcov)' udr='$(udr)' udf='$(udf)' note='$(note)' \
	cfg='$(cfg)' plus_file='$(plus_file)' plus_arg='$(plus_arg)' \
	timeout_ns='$(timeout_ns)' simv_by_tc='$(simv_by_tc)' xprop='$(xprop)' \
	partcmp_op='$(partcmp_op)' initreg='$(initreg)' regr_ini='$(regr_ini)' \
	time_mem_chk='$(time_mem_chk)' gui_on='$(gui_on)' SIM_TOOLS='$(REMOTE_SIM_TOOLS)' \
	'$(REMOTE_WRAPPER)' sync batch_run

eda_run:
	@mkdir -p $(REMOTE_STATE_DIR)
	@REMOTE_HOST='$(REMOTE_HOST)' REMOTE_WORKDIR='$(REMOTE_WORKDIR)' REMOTE_ENTRY='$(REMOTE_ENTRY)' \
	REMOTE_PID_FILE='$(REMOTE_PID_FILE)' REMOTE_LAUNCH_LOG='$(REMOTE_LAUNCH_LOG)' \
	REMOTE_BOOTSTRAP='$(REMOTE_BOOTSTRAP)' \
	seed='$(seed)' tc='$(tc)' ts='$(ts)' pl='$(pl)' mode='$(mode)' wave='$(wave)' timing='$(timing)' \
	ccov='$(ccov)' fcov='$(fcov)' udr='$(udr)' udf='$(udf)' note='$(note)' \
	cfg='$(cfg)' plus_file='$(plus_file)' plus_arg='$(plus_arg)' \
	timeout_ns='$(timeout_ns)' simv_by_tc='$(simv_by_tc)' xprop='$(xprop)' \
	partcmp_op='$(partcmp_op)' initreg='$(initreg)' regr_ini='$(regr_ini)' \
	time_mem_chk='$(time_mem_chk)' gui_on='$(gui_on)' SIM_TOOLS='$(REMOTE_SIM_TOOLS)' \
	'$(REMOTE_WRAPPER)' sync run

eda_run_bg:
	@mkdir -p $(REMOTE_STATE_DIR)
	@REMOTE_HOST='$(REMOTE_HOST)' REMOTE_WORKDIR='$(REMOTE_WORKDIR)' REMOTE_ENTRY='$(REMOTE_ENTRY)' \
	REMOTE_PID_FILE='$(REMOTE_PID_FILE)' REMOTE_LAUNCH_LOG='$(REMOTE_LAUNCH_LOG)' \
	REMOTE_BOOTSTRAP='$(REMOTE_BOOTSTRAP)' \
	seed='$(seed)' tc='$(tc)' ts='$(ts)' pl='$(pl)' mode='$(mode)' wave='$(wave)' timing='$(timing)' \
	ccov='$(ccov)' fcov='$(fcov)' udr='$(udr)' udf='$(udf)' note='$(note)' \
	cfg='$(cfg)' plus_file='$(plus_file)' plus_arg='$(plus_arg)' \
	timeout_ns='$(timeout_ns)' simv_by_tc='$(simv_by_tc)' xprop='$(xprop)' \
	partcmp_op='$(partcmp_op)' initreg='$(initreg)' regr_ini='$(regr_ini)' \
	time_mem_chk='$(time_mem_chk)' gui_on='$(gui_on)' SIM_TOOLS='$(REMOTE_SIM_TOOLS)' \
	'$(REMOTE_WRAPPER)' async run

eda_status:
	@REMOTE_HOST='$(REMOTE_HOST)' REMOTE_WORKDIR='$(REMOTE_WORKDIR)' REMOTE_ENTRY='$(REMOTE_ENTRY)' \
	REMOTE_PID_FILE='$(REMOTE_PID_FILE)' REMOTE_LAUNCH_LOG='$(REMOTE_LAUNCH_LOG)' \
	REMOTE_BOOTSTRAP='$(REMOTE_BOOTSTRAP)' \
	'$(REMOTE_WRAPPER)' status

eda_tail:
	@REMOTE_HOST='$(REMOTE_HOST)' REMOTE_WORKDIR='$(REMOTE_WORKDIR)' REMOTE_ENTRY='$(REMOTE_ENTRY)' \
	REMOTE_PID_FILE='$(REMOTE_PID_FILE)' REMOTE_LAUNCH_LOG='$(REMOTE_LAUNCH_LOG)' \
	REMOTE_BOOTSTRAP='$(REMOTE_BOOTSTRAP)' \
	'$(REMOTE_WRAPPER)' tail

eda_kill:
	@REMOTE_HOST='$(REMOTE_HOST)' REMOTE_WORKDIR='$(REMOTE_WORKDIR)' REMOTE_ENTRY='$(REMOTE_ENTRY)' \
	REMOTE_PID_FILE='$(REMOTE_PID_FILE)' REMOTE_LAUNCH_LOG='$(REMOTE_LAUNCH_LOG)' \
	REMOTE_BOOTSTRAP='$(REMOTE_BOOTSTRAP)' \
	'$(REMOTE_WRAPPER)' kill
