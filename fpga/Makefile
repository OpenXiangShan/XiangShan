include Makefile.check

default: project

# vivado project name
PRJ ?= myproject
PRJ_FULL = $(PRJ)-$(BOARD)
VIVADO_FLAG = -nolog -nojournal -notrace

#--------------------------------------------------------------------
# Project building and implementation
#--------------------------------------------------------------------

PRJ_ROOT = board/$(BOARD)/build/$(PRJ_FULL)
XPR_FILE = $(PRJ_ROOT)/$(PRJ_FULL).xpr
$(XPR_FILE):
	make -C ..
	vivado $(VIVADO_FLAG) -mode batch -source board/$(BOARD)/mk.tcl -tclargs $(PRJ_FULL)

project: $(XPR_FILE)

bootgen:
	cd boot && hsi $(VIVADO_FLAG) -source mk.tcl -tclargs $(abspath $(PRJ_ROOT)/$(PRJ_FULL).sdk/system_top.hdf) $(PRJ_FULL)

vivado: $(XPR_FILE)
	vivado $(VIVADO_FLAG) $(XPR_FILE) &

.PHONY: default project vivado bootgen
