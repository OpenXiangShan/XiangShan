if {[llength $argv] > 0} {
  set project_name [lindex $argv 0]
  set s [split $project_name -]
  set prj [lindex $s 0]
  set brd [lindex $s 1]
} else {
  puts "project full name is not given!"
  return 1
}

proc add_bd {tcl_file} {
  source ${tcl_file}
  save_bd_design
  close_bd_design $design_name
  set_property synth_checkpoint_mode Hierarchical [get_files *${design_name}.bd]
}

set topmodule system_top

set fpga_dir    ${script_dir}/../..
set project_dir ${script_dir}/build/$project_name
set rtl_dir     ${script_dir}/rtl
set lib_dir     ${fpga_dir}/lib
set bd_dir      ${script_dir}/bd
set constr_dir  ${script_dir}/constr
set data_dir    ${script_dir}/data
set ip_dir      ${script_dir}/ip

create_project $project_name -force -dir $project_dir/ -part ${device}
set_property board_part $board [current_project]

# lib files
set inc_files [list \
  "[file normalize "${lib_dir}/include/axi.vh"]" \
]
add_files -norecurse -fileset sources_1 $inc_files
set_property is_global_include true [get_files $inc_files]

# Add files for noop
lappend src_files "[file normalize "${fpga_dir}/../build/TopMain.v"]"

add_files -norecurse -fileset sources_1 $src_files
add_files -norecurse -fileset constrs_1 $xdc_files

# Block Designs
add_bd ${fpga_dir}/noop.tcl
add_bd ${bd_dir}/prm.tcl

# setting top module for FPGA flow and simulation flow
set_property "top" $topmodule [current_fileset]

# setting Synthesis options
set_property strategy {Vivado Synthesis defaults} [get_runs synth_1]
# keep module port names in the netlist
set_property STEPS.SYNTH_DESIGN.ARGS.FLATTEN_HIERARCHY {none} [get_runs synth_1]

# setting Implementation options
set_property steps.phys_opt_design.is_enabled true [get_runs impl_1]

# update compile order 
update_compile_order -fileset sources_1
