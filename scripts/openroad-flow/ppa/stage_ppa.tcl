# Extract per-module PPA & timing metrics at a given ORFS stage into JSON.
#
# Environment variables:
#   PPA_STAGE       one of: synth, floorplan, place, cts, grt
#   PPA_MODULE      top module name
#   PPA_OUT_JSON    output JSON path

set ppa_t0 [clock milliseconds]

source $::env(SCRIPTS_DIR)/load.tcl
set stage $::env(PPA_STAGE)

switch -exact -- $stage {
  synth     { set odb_name "1_synth.odb" }
  floorplan { set odb_name "2_floorplan.odb" }
  place     { set odb_name "3_place.odb" }
  cts       { set odb_name "4_cts.odb" }
  grt       { set odb_name "5_1_grt.odb" }
  default   { error "Unknown PPA_STAGE: $stage" }
}

load_design $odb_name
source $::env(PLATFORM_DIR)/setRC.tcl
# Guarantee picosecond units even when an SRAM macro liberty uses ns.
set_cmd_units -time ps

# Async reset recovery/removal is settled via reset synchronizers, not single-cycle
# in2reg timing (matches scripts/openroad-flow/xstile/constraints.sdc).
foreach p [get_ports -quiet {reset io_reset}] {
  set_false_path -from $p
}

set block [ord::get_db_block]
set dbu [$block getDbUnitsPerMicron]

proc count_buffers { block } {
  set n 0
  foreach inst [$block getInsts] {
    set mname [[$inst getMaster] getName]
    if { [string match "BUF*" $mname] || [string match "INV*" $mname] || [string match "CLKBUF*" $mname] } {
      incr n
    }
  }
  return $n
}

# Measure net fanout before buffering (excluding supply/clock/reset nets)
set max_fanout 0
set top_fanout_nets {}
foreach net [$block getNets] {
  set sig_type [$net getSigType]
  if { $sig_type eq "POWER" || $sig_type eq "GROUND" || $sig_type eq "CLOCK" } {
    continue
  }
  set nname [$net getName]
  if { $nname eq "clock" || $nname eq "reset" || $nname eq "io_clock" } {
    continue
  }
  set sinks 0
  foreach iterm [$net getITerms] {
    if { [$iterm isInputSignal] } {
      incr sinks
    }
  }
  if { $sinks > $max_fanout } {
    set max_fanout $sinks
  }
  if { $sinks >= 16 } {
    lappend top_fanout_nets [list $sinks $nname]
  }
}
set top_fanout_nets [lrange [lsort -integer -decreasing -index 0 $top_fanout_nets] 0 4]

set bufs_before [count_buffers $block]
if { $stage eq "synth" || $stage eq "floorplan" } {
  # Buffer high-fanout and high-slew nets on pin-capacitance model so unbuffered
  # ABC / post-remove_buffers nets do not produce artificial multi-nanosecond slews.
  catch { repair_design }
} elseif { $stage eq "place" } {
  estimate_parasitics -placement
} elseif { $stage eq "cts" } {
  set_propagated_clock [all_clocks]
  estimate_parasitics -placement
} elseif { $stage eq "grt" } {
  set_propagated_clock [all_clocks]
  if { ![catch { grt::have_routes } routed] && $routed } {
    estimate_parasitics -global_routing
  } else {
    estimate_parasitics -placement
  }
}
set bufs_after [count_buffers $block]
set bufs_added [expr { $bufs_after - $bufs_before }]

# Cell census: flops, combinational cells, ICGs, buffers, SRAM macros, area
set flops 0
set comb_cells 0
set icgs 0
set sram_count 0
set stdcell_area_dbu 0.0
set macro_area_dbu 0.0
array set is_block_master {}

foreach inst [$block getInsts] {
  set master [$inst getMaster]
  set mname [$master getName]
  set mtype [$master getType]
  if { [$master isBlock] } {
    incr sram_count
    set is_block_master($mname) 1
    set macro_area_dbu [expr { $macro_area_dbu + double([$master getWidth]) * double([$master getHeight]) }]
  } elseif { [string match "TAPCELL*" $mname] || [string match "FILL*" $mname] || [string match "DECAP*" $mname] || $mtype eq "ENDCAP" } {
    continue
  } elseif { [string match "ICG*" $mname] } {
    incr icgs
    set stdcell_area_dbu [expr { $stdcell_area_dbu + double([$master getWidth]) * double([$master getHeight]) }]
  } elseif { [$master isSequential] } {
    incr flops
    set stdcell_area_dbu [expr { $stdcell_area_dbu + double([$master getWidth]) * double([$master getHeight]) }]
  } else {
    incr comb_cells
    set stdcell_area_dbu [expr { $stdcell_area_dbu + double([$master getWidth]) * double([$master getHeight]) }]
  }
}

set stdcell_area_um2 [expr { $stdcell_area_dbu / ($dbu * $dbu) }]
set macro_area_um2 [expr { $macro_area_dbu / ($dbu * $dbu) }]

# Separate SRAM macro path groups from standard-cell reg2reg when macros exist
set macro_cells {}
set flop_cells {}
foreach c [all_registers -cells] {
  set ref [get_property $c ref_name]
  if { [info exists is_block_master($ref)] } {
    lappend macro_cells $c
  } else {
    lappend flop_cells $c
  }
}
if { $sram_count > 0 } {
  if { [llength $flop_cells] > 0 && [llength $macro_cells] > 0 } {
    group_path -name reg2reg -from $flop_cells -to $flop_cells
    group_path -name mem2reg -from $macro_cells -to $flop_cells
    group_path -name reg2mem -from $flop_cells -to $macro_cells
  }
  if { [llength $macro_cells] > 0 } {
    group_path -name mem2mem -from $macro_cells -to $macro_cells
  }
}

set clks [all_clocks]
set clk_period 1000.0
if { [llength $clks] > 0 } {
  set clk_period [get_property [lindex $clks 0] period]
}

set all_reg_cells [llength [all_registers -cells]]
set clocked_reg_cells 0
if { [llength $clks] > 0 } {
  set clocked_reg_cells [llength [all_registers -cells -clock $clks]]
}
set unclocked_regs [expr { $all_reg_cells - $clocked_reg_cells }]

proc json_escape { s } {
  return [string map {\\ \\\\ \" \\\" \n \\n \r \\r \t \\t} $s]
}

# Extract timing summary per path group (only query groups defined in SDC / above;
# querying an undefined group triggers STA-0527 and falls back to all path groups).
set group_budgets [dict create \
  reg2reg $clk_period \
  mem2reg $clk_period \
  reg2mem $clk_period \
  mem2mem $clk_period \
  in2reg  800.0 \
  reg2out 800.0 \
  in2out  700.0 \
]

set query_groups {}
set has_regs [expr { [llength [all_registers]] > 0 }]
set has_ins  [expr { [llength [all_inputs -no_clocks]] > 0 }]
set has_outs [expr { [llength [all_outputs]] > 0 }]
if { [llength $flop_cells] > 0 } {
  lappend query_groups reg2reg
}
if { $sram_count > 0 && [llength $flop_cells] > 0 && [llength $macro_cells] > 0 } {
  lappend query_groups mem2reg reg2mem
}
if { $sram_count > 0 && [llength $macro_cells] > 0 } {
  lappend query_groups mem2mem
}
if { $has_ins && $has_regs } {
  lappend query_groups in2reg
}
if { $has_regs && $has_outs } {
  lappend query_groups reg2out
}
if { $has_ins && $has_outs } {
  lappend query_groups in2out
}

set group_json_entries {}
set top_endpoints_json {}

set max_ep_query [sta::endpoint_count]
if { $max_ep_query < 10 } {
  set max_ep_query 10
}

foreach g $query_groups {
  set all_ends [find_timing_paths -path_group $g -sort_by_slack -endpoint_path_count 1 -group_path_count $max_ep_query -unique_paths_to_endpoint]
  if { [llength $all_ends] == 0 } {
    continue
  }
  set worst [lindex $all_ends 0]
  set wns [get_property $worst slack]
  if { $wns eq "INF" || $wns eq "-INF" || $wns eq "" } {
    continue
  }
  set budget [dict get $group_budgets $g]
  set delay_ps [expr { $budget - $wns }]

  # Count logic levels (output pin transitions of combinational/sequential cells along path)
  set pts [get_property $worst points]
  set levels 0
  for { set k 1 } { $k < [llength $pts] } { incr k } {
    set pin [get_property [lindex $pts $k] pin]
    if { [get_property $pin direction] eq "output" } {
      incr levels
    }
  }

  set sp_name [get_full_name [get_property $worst startpoint]]
  set ep_name [get_full_name [get_property $worst endpoint]]

  # Endpoint count, violating count, and TNS in this group (from the same PathEnd list)
  set ep_count [llength $all_ends]
  set viol_count 0
  set tns 0.0
  foreach ep_path $all_ends {
    set s [get_property $ep_path slack]
    if { [string is double -strict $s] && $s < 0.0 } {
      incr viol_count
      set tns [expr { $tns + $s }]
    }
  }

  lappend group_json_entries [format \
    "\"%s\": {\"period_ps\": %.2f, \"wns_ps\": %.2f, \"tns_ps\": %.1f, \"levels\": %d, \"endpoints\": %d, \"violating\": %d, \"startpoint\": \"%s\", \"endpoint\": \"%s\"}" \
    $g $delay_ps $wns $tns $levels $ep_count $viol_count [json_escape $sp_name] [json_escape $ep_name]]

  foreach p [lrange $all_ends 0 2] {
    set ps_slack [get_property $p slack]
    if { [string is double -strict $ps_slack] } {
      lappend top_endpoints_json [format \
        "{\"group\": \"%s\", \"period_ps\": %.2f, \"slack_ps\": %.2f, \"startpoint\": \"%s\", \"endpoint\": \"%s\"}" \
        $g [expr { $budget - $ps_slack }] $ps_slack \
        [json_escape [get_full_name [get_property $p startpoint]]] \
        [json_escape [get_full_name [get_property $p endpoint]]]]
    }
  }
}

set fanout_json {}
foreach item $top_fanout_nets {
  lassign $item fo nname
  lappend fanout_json [format "{\"net\": \"%s\", \"fanout\": %d}" [json_escape $nname] $fo]
}

set probe_s [expr { ([clock milliseconds] - $ppa_t0) / 1000.0 }]

set out_json $::env(PPA_OUT_JSON)
if { ![file exists [file dirname $out_json]] && [info exists ::env(WORK_HOME)] } {
  set out_json [file join $::env(WORK_HOME) $out_json]
}
file mkdir [file dirname $out_json]
set fh [open $out_json w]
puts $fh "{"
puts $fh [format "  \"module\": \"%s\"," [json_escape $::env(PPA_MODULE)]]
puts $fh [format "  \"stage\": \"%s\"," [json_escape $stage]]
puts $fh [format "  \"sdc_period_ps\": %.1f," $clk_period]
puts $fh [format "  \"flops\": %d," $flops]
puts $fh [format "  \"comb_cells\": %d," $comb_cells]
puts $fh [format "  \"icgs\": %d," $icgs]
puts $fh [format "  \"buffers_total\": %d," $bufs_after]
puts $fh [format "  \"buffers_added\": %d," $bufs_added]
puts $fh [format "  \"sram_count\": %d," $sram_count]
puts $fh [format "  \"area_um2\": %.2f," $stdcell_area_um2]
puts $fh [format "  \"macro_area_um2\": %.2f," $macro_area_um2]
puts $fh [format "  \"max_fanout\": %d," $max_fanout]
puts $fh [format "  \"unclocked_regs\": %d," $unclocked_regs]
puts $fh [format "  \"probe_elapsed_s\": %.3f," $probe_s]
puts $fh "  \"top_fanout_nets\": \[[join $fanout_json {, }]\],"
puts $fh "  \"top_endpoints\": \[[join $top_endpoints_json {, }]\],"
puts $fh "  \"path_groups\": {[join $group_json_entries {, }]}"
puts $fh "}"
close $fh
