set device xczu3cg-sfvc784-1-e

set script_dir  [file dirname [info script]]

# Add files for system top
set src_files [list \
  "[file normalize "${script_dir}/rtl/system_top.v"]" \
  "[file normalize "${script_dir}/rtl/addr_mapper.v"]" \
]

# Add files for constraint
#set xdc_files [list \
#]

source ${script_dir}/../common.tcl
