set device xczu3cg-sfvc784-1-e

set script_dir  [file dirname [info script]]

# Add files for system top
set src_files [list \
  "[file normalize "${script_dir}/rtl/system_top.v"]" \
  "[file normalize "${script_dir}/rtl/addr_mapper.v"]" \
  "[file normalize "${script_dir}/rtl/hdmi/i2c_config.v"]" \
  "[file normalize "${script_dir}/rtl/hdmi/i2c_master_bit_ctrl.v"]" \
  "[file normalize "${script_dir}/rtl/hdmi/i2c_master_byte_ctrl.v"]" \
  "[file normalize "${script_dir}/rtl/hdmi/i2c_master_defines.v"]" \
  "[file normalize "${script_dir}/rtl/hdmi/i2c_master_top.v"]" \
]

# Add files for constraint
set xdc_files [list \
  "[file normalize "${script_dir}/constr/hdmi.xdc"]" \
]

source ${script_dir}/../common.tcl
