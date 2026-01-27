# Register & Flash Examples

```text
# Load register file
xparse_reg_file /abs/path/to/regs.txt
xload_reg_file /abs/path/to/regs.txt

# Set individual registers
xset_ireg x1 0x1234
xset_freg f0 0x3f800000
xset_mpc 0x80000000

# List flash registers
xlist_flash_iregs
xlist_flash_fregs
xlist_freg_map
```
