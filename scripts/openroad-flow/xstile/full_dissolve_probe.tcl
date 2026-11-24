# Phase 4b time-capped full-dissolve probe on XSTile_parent_pm_floorplan.
#
# Starting from 2_floorplan.odb (where L2Top, Backend, MemBlock, Frontend, and
# XSTile_parent have all been dissolved into one flat netlist), keep all BLOCK
# macros (SRAMs + RegFiles) and physical-only well-tap/endcap cells LOCKED at
# their dissolved coordinates, set every logic and sequential standard cell to
# PLACED (not dont_touch), clear all signal guides/wires, and run flat
# global_placement (timing + routability driven) -> repair_design -> diamond DPL
# -> CTS -> diamond DPL -> global_route (route-0, no timing repair at CTS/GRT).
source $::env(SCRIPTS_DIR)/load.tcl

proc probe_cells_out_of_blocks {} {
  set block [ord::get_db_block]
  set boxes {}
  foreach inst [$block getInsts] {
    if { [[$inst getMaster] isBlock] && [$inst getPlacementStatus] in {FIRM LOCKED} } {
      set b [$inst getBBox]
      lappend boxes [list [$b xMin] [$b yMin] [$b xMax] [$b yMax]]
    }
  }
  foreach blockage [$block getBlockages] {
    if { ![$blockage isSoft] } {
      set b [$blockage getBBox]
      lappend boxes [list [$b xMin] [$b yMin] [$b xMax] [$b yMax]]
    }
  }
  set total_moved 0
  for { set pass 0 } { $pass < 4 } { incr pass } {
    set moved 0
    foreach inst [$block getInsts] {
      if { [[$inst getMaster] isBlock] || [$inst getPlacementStatus] ne "PLACED" } { continue }
      set b [$inst getBBox]
      set x [$b xMin]; set y [$b yMin]
      set w [expr { [$b xMax] - $x }]; set h [expr { [$b yMax] - $y }]
      foreach box $boxes {
        lassign $box x0 y0 x1 y1
        if { [expr { $x + $w }] <= $x0 || $x >= $x1 || [expr { $y + $h }] <= $y0 || $y >= $y1 } { continue }
        set d [list [expr { $x - $x0 + $w }] [expr { $x1 - $x }] [expr { $y - $y0 + $h }] [expr { $y1 - $y }]]
        switch [lsearch -exact $d [tcl::mathfunc::min {*}$d]] {
          0 { set x [expr { $x0 - $w }] }
          1 { set x $x1 }
          2 { set y [expr { $y0 - $h }] }
          3 { set y $y1 }
        }
        $inst setLocation $x $y
        incr moved
        break
      }
    }
    incr total_moved $moved
    if { $moved == 0 } { break }
  }
  utl::info FLW 9001 "probe_cells_out_of_blocks: $total_moved movable cell(s) moved out of [llength $boxes] hard blocks"
}

set probe_t0 [clock seconds]
proc probe_log { step msg } {
  set elapsed [expr { [clock seconds] - $::probe_t0 }]
  set rss_gb 0.0
  set hwm_gb 0.0
  set f [open /proc/self/status]
  foreach line [split [read $f] "\n"] {
    if { [string match "VmRSS:*" $line] } {
      set rss_gb [expr { [lindex $line 1] / 1048576.0 }]
    } elseif { [string match "VmHWM:*" $line] } {
      set hwm_gb [expr { [lindex $line 1] / 1048576.0 }]
    }
  }
  close $f
  set line [format "full_dissolve_probe: (%5d s | rss %.1f GB | peak %.1f GB) %-16s %s" \
    $elapsed $rss_gb $hwm_gb $step $msg]
  puts $line
  flush stdout
  if { [info exists ::env(RUN_OUTPUT_DIR)] && $::env(RUN_OUTPUT_DIR) ne "" } {
    file mkdir $::env(RUN_OUTPUT_DIR)
    set pf [open "$::env(RUN_OUTPUT_DIR)/probe_progress.txt" a]
    puts $pf $line
    close $pf
  }
}

probe_log "start" "loading 2_floorplan.odb"
load_design 2_floorplan.odb -backing_file $::env(OBJECTS_DIR)/full_dissolve.db
probe_log "loaded" "2_floorplan.odb loaded"

set block [ord::get_db_block]
array unset master_kind
set n_locked_macros 0
set n_locked_phys 0
set n_movable_cells 0
foreach inst [$block getInsts] {
  set master [$inst getMaster]
  if { ![info exists master_kind($master)] } {
    set mname [$master getName]
    if { [$master isBlock] } {
      set master_kind($master) macro
    } elseif { [$master isFiller] || [$master isEndCap] || ![$master isCoreAutoPlaceable] || [$master getType] ne "CORE" } {
      set master_kind($master) phys
    } else {
      set lib [lindex [get_lib_cells -quiet $mname] 0]
      if { $lib ne "" && [get_property $lib is_memory] } {
        set master_kind($master) macro
      } elseif { $lib eq "" || [string match "TAPCELL*" $mname] || [string match "DECAP*" $mname] || [string match "FILLER*" $mname] } {
        set master_kind($master) phys
      } else {
        set master_kind($master) stdcell
      }
    }
  }
  switch $master_kind($master) {
    macro {
      $inst setPlacementStatus LOCKED
      incr n_locked_macros
    }
    phys {
      $inst setPlacementStatus LOCKED
      incr n_locked_phys
    }
    stdcell {
      $inst setDoNotTouch 0
      $inst setPlacementStatus PLACED
      incr n_movable_cells
    }
  }
}

set n_cleared_nets 0
foreach net [$block getNets] {
  if { [$net getSigType] in { POWER GROUND } } {
    continue
  }
  $net setDoNotTouch 0
  $net clearGuides
  set wire [$net getWire]
  if { $wire ne "NULL" } {
    odb::dbWire_destroy $wire
  }
  incr n_cleared_nets
}

probe_log "unfirmed" "$n_movable_cells movable stdcells, $n_locked_macros locked macros, $n_locked_phys locked tap/phys cells, $n_cleared_nets signal nets cleared"

set_dont_use $::env(DONT_USE_CELLS)
set pad $::env(CELL_PAD_IN_SITES_GLOBAL_PLACEMENT)
set gpl_t0 [clock seconds]
probe_log "gpl_begin" "global_placement -density 0.65 -timing_driven -routability_driven -keep_resize_below_overflow 0"
log_cmd global_placement \
  -density 0.65 \
  -pad_left $pad \
  -pad_right $pad \
  -timing_driven \
  -routability_driven \
  -min_phi_coef $::env(MIN_PLACE_STEP_COEF) \
  -max_phi_coef $::env(MAX_PLACE_STEP_COEF) \
  -keep_resize_below_overflow 0
probe_log "gpl_done" "global_placement finished in [expr { [clock seconds] - $gpl_t0 }] s"

set rsz_t0 [clock seconds]
log_cmd estimate_parasitics -placement
log_cmd repair_design
probe_log "repair_done" "repair_design finished in [expr { [clock seconds] - $rsz_t0 }] s"

set dpl_t0 [clock seconds]
probe_cells_out_of_blocks
log_cmd detailed_placement -use_diamond_legalizer -max_displacement {150 150}
log_cmd check_placement -placeable -verbose
probe_log "place_dpl_done" "post-GPL diamond DPL finished in [expr { [clock seconds] - $dpl_t0 }] s"

set cts_t0 [clock seconds]
repair_clock_inverters
log_cmd clock_tree_synthesis -sink_clustering_enable -repair_clock_nets -sink_clustering_size 10
sta::network_changed_non_sdc
probe_cells_out_of_blocks
log_cmd detailed_placement -use_diamond_legalizer -max_displacement {150 150}
log_cmd check_placement -placeable -verbose
probe_log "cts_done" "CTS + diamond DPL finished in [expr { [clock seconds] - $cts_t0 }] s"

set grt_t0 [clock seconds]
log_cmd global_route -congestion_iterations 0 -no_incremental \
  -backing_file $::env(OBJECTS_DIR)/grt.grid -allow_congestion -verbose
probe_log "grt_done" "global_route finished in [expr { [clock seconds] - $grt_t0 }] s"

set sta_t0 [clock seconds]
set_propagated_clock [all_clocks]
log_cmd estimate_parasitics -placement
report_worst_slack -max
report_tns -max
probe_log "sta_done" "placement-parasitics STA finished in [expr { [clock seconds] - $sta_t0 }] s"

if { [info exists ::env(RUN_OUTPUT_DIR)] && $::env(RUN_OUTPUT_DIR) ne "" } {
  orfs_write_db $::env(RUN_OUTPUT_DIR)/5_full_dissolve_grt.odb
  file copy -force $::env(RESULTS_DIR)/2_floorplan.sdc $::env(RUN_OUTPUT_DIR)/5_full_dissolve_grt.sdc
}
probe_log "complete" "full dissolve probe completed cleanly"
