# Data Load/Export & Memory Utilities (Design)

## Purpose
Load binaries or instruction lists, and export memory/flash for analysis.

## Notes
- Supports loading binaries into memory or flash, exporting memory/flash images, and direct memory writes.
- Utility helpers exist for converting between byte streams and integers.

## Related commands
- `xload`, `xflash`, `xreset_flash`
- `xexport_bin`, `xexport_flash`, `xexport_ram`
- `xmem_write`
- `xbytes_to_bin`, `xbytes2number`, `xnumber2bytes`
- `xparse_instr_file`, `xload_instr_file`, `xnop_insert`
