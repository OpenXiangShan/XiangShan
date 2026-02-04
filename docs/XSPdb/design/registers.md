# Register Initialization & Flash Utilities (Design)

## Purpose
Configure CPU registers and flash contents before or during simulation for reproducible testing and debugging scenarios.

## Design Overview

### Register Management
XSPdb provides comprehensive register manipulation capabilities:
- **Integer Registers** (x0-x31): General-purpose registers for integer operations
- **Floating-Point Registers** (f0-f31): Registers for floating-point operations
- **Control Registers**: Special-purpose registers including MPC (Program Counter)
- **Flash Registers**: Boot-time register initialization for simulation startup

### File-Based Configuration
- **Register Files**: Text files containing register values in standardized format
- **Instruction Lists**: Lists of instructions to initialize registers
- **Parsing**: Automatic parsing of various register file formats
- **Validation**: Range checking and type validation for register values

### Flash Integration
- **Boot Configuration**: Initialize registers before simulation starts
- **Reset Values**: Set default register values for reset scenarios
- **MPC Control**: Manage Program Counter for starting execution at specific addresses

### Reproducibility
- Register initialization ensures consistent starting conditions
- File-based configuration enables version control of test setups
- Batch operations allow efficient multi-register configuration

## Notes
- Utilities parse/load instruction lists and register files
- Convenience setters for integer/floating registers and MPC
- Flash registers are set during DUT initialization
- Register files support both decimal and hexadecimal formats
- Floating-point registers accept IEEE 754 format values

## Related commands
- `xparse_reg_file` - Parse and validate register file
- `xload_reg_file` - Load register values from file
- `xset_iregs` - Set multiple integer registers
- `xset_fregs` - Set multiple floating-point registers
- `xset_ireg` - Set single integer register
- `xset_freg` - Set single floating-point register
- `xset_mpc` - Set MPC (Program Counter)
- `xget_mpc` - Get current MPC value
- `xlist_flash_iregs` - List flash integer registers
- `xlist_flash_fregs` - List flash floating-point registers
- `xlist_freg_map` - List floating-point register mapping

## Implementation Details
- Register values are validated before being applied
- Flash registers are applied during DUT reset
- MPC changes take effect on next instruction fetch
- Floating-point values are converted to IEEE 754 format
- Register files support comments and empty lines for readability
