# Register Initialization & Flash Utilities (Design)

## Purpose
Configure registers and flash contents before or during simulation for reproducibility.

## Notes
- Utilities parse/load instruction lists and register files.
- Convenience setters for integer/floating registers and MPC.

## Related commands
- `xparse_reg_file`, `xload_reg_file`
- `xset_iregs`, `xset_fregs`, `xset_ireg`, `xset_freg`
- `xset_mpc`, `xget_mpc`
- `xlist_flash_iregs`, `xlist_flash_fregs`, `xlist_freg_map`
