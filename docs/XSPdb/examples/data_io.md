# Data Load/Export Examples

```text
# Load a binary into memory or flash
xload /abs/path/to/bin
xflash /abs/path/to/flash.bin
xreset_flash

# Export memory/flash images
xexport_bin /abs/path/to/mem_dump.bin
xexport_flash /abs/path/to/flash_dump.bin
xexport_ram /abs/path/to/ram_dump.bin

# Direct memory writes
xmem_write 0x80000000 4 0xdeadbeef

# Instruction list helpers
xparse_instr_file /abs/path/to/instr.txt
xload_instr_file /abs/path/to/instr.txt

# Byte/number helpers
xbytes_to_bin 01 02 03 04
xbytes2number 01 02 03 04
xnumber2bytes 0x12345678 4

# Insert NOPs at PC (if supported)
xnop_insert 0x80000000 4
```
