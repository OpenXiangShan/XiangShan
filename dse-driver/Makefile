AS = riscv64-unknown-linux-gnu-as
GCC = riscv64-unknown-linux-gnu-gcc
LD = riscv64-unknown-linux-gnu-ld
OBJCOPY = riscv64-unknown-linux-gnu-objcopy
OBJDUMP = riscv64-unknown-linux-gnu-objdump

ASFLAGS = -mno-arch-attr -march=rv64ima_zicsr_zifencei
CFLAGS = -nostdlib -nostartfiles -ffreestanding -falign-functions=16 -mstrict-align \
         -fno-common -fno-builtin -fno-stack-protector -fno-exceptions -fno-rtti \
         -fno-threadsafe-statics -fno-strict-aliasing -fno-omit-frame-pointer \
         -fno-optimize-sibling-calls -fno-PIE -fno-pic \
         -march=rv64ima_zicsr_zifencei -mabi=lp64


BUILD_DIR = build
BUILD_NUM ?= 1

all: $(BUILD_DIR)/dse_$(BUILD_NUM).bin $(BUILD_DIR)/dse_$(BUILD_NUM).txt

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

$(BUILD_DIR)/start.o: start.S | $(BUILD_DIR)
	$(AS) $(ASFLAGS) start.S -o $(BUILD_DIR)/start.o

$(BUILD_DIR)/dse.o: dse.c | $(BUILD_DIR)
	$(GCC) $(CFLAGS) -c dse.c -o $(BUILD_DIR)/dse.o

$(BUILD_DIR)/dse.elf: $(BUILD_DIR)/start.o $(BUILD_DIR)/dse.o | $(BUILD_DIR)
	$(LD) -o $(BUILD_DIR)/dse.elf $(BUILD_DIR)/start.o $(BUILD_DIR)/dse.o -T linker.ld -nostdlib

$(BUILD_DIR)/dse_$(BUILD_NUM).bin: $(BUILD_DIR)/dse.elf | $(BUILD_DIR)
	$(OBJCOPY) -O binary $(BUILD_DIR)/dse.elf $(BUILD_DIR)/dse_$(BUILD_NUM).bin

$(BUILD_DIR)/dse_$(BUILD_NUM).txt: $(BUILD_DIR)/dse.elf | $(BUILD_DIR)
	$(OBJDUMP) -d $(BUILD_DIR)/dse.elf > $(BUILD_DIR)/dse_$(BUILD_NUM).txt

clean:
	rm -rf $(BUILD_DIR)