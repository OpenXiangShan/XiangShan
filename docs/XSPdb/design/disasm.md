# Disassembly (Design)

## Purpose
Translate memory content, flash data, or raw bytes into human-readable assembly instructions for code inspection and debugging.

## Design Overview

### Disassembly Engines
XSPdb supports multiple disassembly backends with fallback mechanism:
1. **Primary**: `spike-dasm` - Provides accurate RISC-V disassembly with symbol information
2. **Fallback**: `capstone` - Limited ISA coverage, used when spike-dasm is unavailable

### Memory Regions
Disassembly can operate on different memory regions:
- **Main Memory** (`xdasm`) - RAM region starting from PMEM_BASE (default 0x80000000)
- **Flash Memory** (`xdasmflash`) - Flash region starting from FLASH_BASE (default 0x10000000)
- **Raw Bytes** (`xdasmbytes`) - Disassemble arbitrary byte sequences
- **Numbers** (`xdasmnumber`) - Disassemble a single instruction word

### Caching Mechanism
- Disassembly results are cached to improve performance
- Cache can be cleared with `xclear_dasm_cache` to force re-disassembly
- Useful when memory content changes or when debugging code modifications

### Symbol Integration
- When available, disassembly includes symbol names and offset information
- Symbols are loaded from ELF files automatically
- Improves readability by showing function names instead of raw addresses

## Notes
- `xdasm*` commands operate on memory, flash, bytes, or numbers
- Disassembly prefers `spike-dasm` with a `capstone` fallback (limited ISA coverage)
- Cache is maintained per session and cleared on restart unless manually cleared
- Disassembly output shows address, instruction bytes, and assembly mnemonic

## Related commands
- `xdasm` - Disassemble main memory
- `xdasmflash` - Disassemble flash memory
- `xdasmbytes` - Disassemble raw bytes
- `xdasmnumber` - Disassemble a single instruction number
- `xclear_dasm_cache` - Clear disassembly cache
- `xload_elf` - Load ELF file for symbol resolution

## Implementation Details
- Disassembly engine selection is automatic based on availability
- Cache key includes address and length to ensure correctness
- Symbol resolution uses loaded ELF files or symbol databases
- Output format is optimized for terminal readability
