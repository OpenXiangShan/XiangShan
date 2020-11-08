#=======================================================================
# Makefile for riscv-tests/isa
#-----------------------------------------------------------------------

act_dir := .
src_dir := $(act_dir)/src
work_dir := $(ROOTDIR)/work
work_dir_isa := $(work_dir)/$(RISCV_ISA)

include $(act_dir)/Makefrag
ifneq ($(RISCV_TEST),)
    target_tests = $(RISCV_TEST).elf
endif

default: all

#--------------------------------------------------------------------
# Build rules
#--------------------------------------------------------------------

vpath %.S $(act_dir)

INCLUDE=$(TARGETDIR)/$(RISCV_TARGET)/device/$(RISCV_DEVICE)/Makefile.include
ifeq ($(wildcard $(INCLUDE)),)
    $(error Cannot find '$(INCLUDE)`. Check that RISCV_TARGET and RISCV_DEVICE are set correctly.)
endif
-include $(INCLUDE)

#------------------------------------------------------------
# Build and run assembly tests

%.log: %.elf
	$(V) echo "Execute $(@)"
	$(V) $(RUN_TARGET)


define compile_template

$(work_dir_isa)/%.elf: $(src_dir)/%.S
	$(V) echo "Compile $$(@)"
	@mkdir -p $$(@D)
	$(V) $(COMPILE_TARGET)

.PRECIOUS: $(work_dir_isa)/%.elf

endef

$(eval $(call compile_template,-march=rv32ia -mabi=ilp32))

target_elf = $(foreach e,$(target_tests),$(work_dir_isa)/$(e))
target_log = $(patsubst %.elf,%.log,$(target_elf))

run: $(target_log)

#------------------------------------------------------------
# Clean up

clean:
	rm -rf $(work_dir)
