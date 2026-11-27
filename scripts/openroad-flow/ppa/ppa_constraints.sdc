# Module-level synthesis & P&R constraints on ASAP7 (1,000 ps = 1.0 ns reference period).
#
# Follows bazel-orfs's macro-constraints discipline:
# - Register-to-register (and SRAM-to-register / register-to-SRAM) paths use
#   the real 1,000 ps clock cycle; period_ps = 1000 - slack_ps.
# - Boundary paths (in2reg, reg2out, in2out) are optimization targets expressed
#   with set_max_delay rather than set_input_delay / set_output_delay, which
#   would assume a clock-tree insertion latency the module does not yet have.

set sdc_period 1000.0
set clk_ports [get_ports -quiet {clock io_clock clk}]
if { [llength $clk_ports] == 0 } {
  foreach p [all_inputs] {
    set pn [get_full_name $p]
    if { [string match "*clock*" $pn] || [string match "*clk*" $pn] } {
      lappend clk_ports $p
    }
  }
}

if { [llength $clk_ports] > 0 } {
  create_clock -name core_clock -period $sdc_period [lindex $clk_ports 0]
  set non_clk_inputs [all_inputs -no_clocks]
} else {
  create_clock -name core_clock -period $sdc_period
  set non_clk_inputs [all_inputs]
}

set_max_fanout 32 [current_design]

set all_regs [all_registers]
set all_outs [all_outputs]

if { [llength $non_clk_inputs] > 0 && [llength $all_regs] > 0 } {
  set_max_delay 800.0 -from $non_clk_inputs -to $all_regs
  group_path -name in2reg -from $non_clk_inputs -to $all_regs
}
if { [llength $all_regs] > 0 && [llength $all_outs] > 0 } {
  set_max_delay 800.0 -from $all_regs -to $all_outs
  group_path -name reg2out -from $all_regs -to $all_outs
}
if { [llength $non_clk_inputs] > 0 && [llength $all_outs] > 0 } {
  set_max_delay 700.0 -from $non_clk_inputs -to $all_outs
  group_path -name in2out -from $non_clk_inputs -to $all_outs
}
if { [llength $all_regs] > 0 } {
  group_path -name reg2reg -from $all_regs -to $all_regs
}
