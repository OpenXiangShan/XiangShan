set device xc7z020-1-clg484
set board em.avnet.com:zed:part0:1.3

set script_dir  [file dirname [info script]]

# Add files for system top
set src_files [list \
  "[file normalize "${script_dir}/rtl/system_top.v"]" \
  "[file normalize "${script_dir}/rtl/addr_mapper.v"]" \
  "[file normalize "${script_dir}/../../../src/test/vsrc/monitor.v"]" \
]

# Add files for constraint
set xdc_files [list \
  "[file normalize "${script_dir}/constr/constr.xdc"]" \
  "[file normalize "${script_dir}/constr/vga.xdc"]" \
]

source ${script_dir}/../common.tcl
