AS = riscv64-unknown-linux-gnu-as
GCC = riscv64-unknown-linux-gnu-gcc
LD = riscv64-unknown-linux-gnu-ld
OBJCOPY = riscv64-unknown-linux-gnu-objcopy
OBJDUMP = riscv64-unknown-linux-gnu-objdump

BUILD_DIR = build

all: $(BUILD_DIR)/dse.bin $(BUILD_DIR)/dse.txt

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

$(BUILD_DIR)/start.o: start.S | $(BUILD_DIR)
	$(AS) start.S -o $(BUILD_DIR)/start.o

$(BUILD_DIR)/dse.o: dse.c | $(BUILD_DIR)
	$(GCC) -c dse.c -o $(BUILD_DIR)/dse.o -nostdlib -nostartfiles -ffreestanding

$(BUILD_DIR)/dse.elf: $(BUILD_DIR)/start.o $(BUILD_DIR)/dse.o | $(BUILD_DIR)
	$(LD) -o $(BUILD_DIR)/dse.elf $(BUILD_DIR)/start.o $(BUILD_DIR)/dse.o -T linker.ld -nostdlib

$(BUILD_DIR)/dse.bin: $(BUILD_DIR)/dse.elf | $(BUILD_DIR)
	$(OBJCOPY) -O binary $(BUILD_DIR)/dse.elf $(BUILD_DIR)/dse.bin

$(BUILD_DIR)/dse.txt: $(BUILD_DIR)/dse.elf | $(BUILD_DIR)
	$(OBJDUMP) -d $(BUILD_DIR)/dse.elf > $(BUILD_DIR)/dse.txt

clean:
	rm -rf $(BUILD_DIR)