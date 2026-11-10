# Data Load/Export & Memory Utilities (Design)

## Purpose
Load binaries or instruction lists into memory/flash, export memory/flash images for analysis, and provide direct memory manipulation capabilities.

## Design Overview

### Memory Loading
XSPdb supports multiple methods for loading data into simulation memory:
- **Binary Loading**: Load binary files directly into memory or flash regions
- **Instruction Lists**: Parse and load instruction sequences from text files
- **Direct Writes**: Write arbitrary values to specific memory addresses
- **Flash Initialization**: Configure flash contents for boot scenarios

### Memory Export
Extract simulation state for offline analysis:
- **Memory Dump**: Export entire memory region to binary file
- **Flash Dump**: Export flash contents for verification
- **RAM Export**: Export RAM region separately for focused analysis
- **Unified Export**: Combine multiple regions into single file

### Data Conversion
Utilities for format conversion and manipulation:
- **Byte Operations**: Convert between byte sequences and integers
- **Number Conversion**: Convert between number formats
- **Instruction Parsing**: Parse instruction lists with NOP insertion
- **Binary Utilities**: Handle endianness and alignment

### Memory Regions
XSPdb manages multiple memory regions:
- **Main Memory**: RAM region starting from PMEM_BASE (default 0x80000000)
- **Flash Memory**: Flash region starting from FLASH_BASE (default 0x10000000)
- **Special Regions**: Memory-mapped I/O and control registers

## Notes
- Supports loading binaries into memory or flash, exporting memory/flash images, and direct memory writes
- Utility helpers exist for converting between byte streams and integers
- Binary loading respects memory region boundaries and alignments
- Export operations preserve exact memory state including uninitialized regions
- Instruction lists support comments and empty lines for readability

## Related commands
- `xload` - Load binary into memory
- `xflash` - Load binary into flash
- `xreset_flash` - Reset flash to initial state
- `xexport_bin` - Export memory to binary file
- `xexport_flash` - Export flash to binary file
- `xexport_ram` - Export RAM to binary file
- `xexport_unified_bin` - Export unified memory image
- `xmem_write` - Direct memory write
- `xbytes_to_bin` - Convert bytes to binary
- `xbytes2number` - Convert bytes to number
- `xnumber2bytes` - Convert number to bytes
- `xparse_instr_file` - Parse instruction file
- `xload_instr_file` - Load instruction file
- `xnop_insert` - Insert NOPs at PC

## Implementation Details
- Memory operations use direct memory access for efficiency
- Binary loading supports automatic address alignment
- Export operations include all memory regions between specified addresses
- Byte conversion handles endianness automatically
- Instruction parsing supports multiple instruction formats
