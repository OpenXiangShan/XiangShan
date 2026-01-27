# Disassembly (Design)

## Purpose
Translate memory or raw bytes to human-readable instructions.

## Notes
- `xdasm*` commands operate on memory, flash, bytes, or numbers.
- Disassembly prefers `spike-dasm` with a `capstone` fallback (limited ISA coverage).

## Related commands
- `xdasm`, `xdasmflash`, `xdasmbytes`, `xdasmnumber`, `xclear_dasm_cache`
