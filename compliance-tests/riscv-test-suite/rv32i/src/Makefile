work_space=/home/tjz/work/XiangShan/compliance-tests/work

RISCV_PREFIX   ?= riscv64-linux-gnu-
RISCV_GCC      ?= $(RISCV_PREFIX)gcc
RISCV_GNU      ?= $(RISCV_PREFIX)gnu
RISCV_OBJCOPY  ?= $(RISCV_PREFIX)objcopy
RISCV_ld       ?= $(RISCV_PREFIX)ld
RISCV_OBJDUMP  ?= $(RISCV_PREFIX)objdump

SRC=$(wildcard *.S)
OBJ_1=$(patsubst %.S,%.bin,$(SRC))
OBJ_2=$(patsubst %.S,%.objdump,$(SRC))
OBJ_3=$(basename $(SRC))

all:$(OBJ_1) $(OBJ_2)
	@echo -e '/*\nBin_files  are ready!\n*/'
	@echo -e '/*\nObjdump_files are ready!\n*/'

$(OBJ_2):%.objdump:%.elf
	@$(RISCV_OBJDUMP) -D $^ > $@

$(OBJ_1):%.bin:%.elf
	@$(RISCV_OBJCOPY) -S --set-section-flags .bss=alloc,contents -O binary $^ $@


%.elf:%.o
	@$(RISCV_ld) -melf64lriscv \
		-L $(AM_HOME)/am/src/nemu/ldscript \
		-T $(AM_HOME)/am/src/nemu/isa/riscv/boot/loader64.ld \
		--gc-sections -o \
		$@ --start-group \
		$^ $(AM_HOME)/am/build/am-riscv64-noop.a \
		$(AM_HOME)/libs/klib/build/klib-riscv64-noop.a \
		--end-group
	
%.o:%.S
	@mkdir -p $(work_space)/$(OBJ_3)
	@$(RISCV_GCC) -fno-pic -march=rv64g \
		-mcmodel=medany -O0 -DMAINARGS=\"\" -MMD \
		-I/home/tjz/work/XiangShan/compliance-tests/riscv-test-env \
		-I/home/tjz/work/XiangShan/compliance-tests/riscv-test-env/p/ \
		-I/home/tjz/work/XiangShan/compliance-tests/riscv-target/Xiangshan \
		-T/home/tjz/work/XiangShan/compliance-tests/riscv-test-env/p/link.ld \
		-I$(AM_HOME)am/include/ \
		-I$(AM_HOME)/libs/klib/include/ \
		-I$(AM_HOME)/am/ -I$(AM_HOME)/am/include -c -o \
		$@ $^
	
.PRECIOUS: %.o

.PHONY:clean
clean:
	rm -r *.o *.d *.elf *.bin *.objdump $(OBJ_3)


                                                                                                                                                                                                                                                                                                                         		
