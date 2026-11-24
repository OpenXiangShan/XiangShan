# Wrapper around top_level.tcl for Pass 2 XSTile_parent_pm_top: moves any
# movable (PLACED) unfirmed/repair/CTS cells out of LOCKED/FIRM SRAM and
# RegFile footprints (bazel-orfs #1188, legalize_out_of_blocks.tcl) right
# before detailed_placement so the diamond legalizer does not spend hours
# searching 450x450 um rings from inside 380x249 um RegFile boxes.
source $::env(SCRIPTS_DIR)/load.tcl

proc pm_cells_out_of_blocks {} {
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
  utl::info FLW 9001 "pm_cells_out_of_blocks: $total_moved movable cell(s) moved out of [llength $boxes] hard blocks before legalization"
}

set steps {floorplan place tree cts grt}
set from [env_var_or_empty TOP_FROM]
if { $from eq "" } {
  set from floorplan
}
if { $from ni $steps } {
  utl::error FLW 27 "top_level: TOP_FROM $from is none of $steps"
}
set todo [lrange $steps [expr { [lsearch -exact $steps $from] + 1 }] end]
set to [env_var_or_empty TOP_TO]
if { $to ne "" } {
  if { $to ni $steps } {
    utl::error FLW 29 "top_level: TOP_TO $to is none of $steps"
  }
  if { $to ni $todo } {
    utl::error FLW 139 "top_level: TOP_TO $to is not after TOP_FROM $from"
  }
  set todo [lrange $todo 0 [lsearch -exact $todo $to]]
}
set last_step [lindex $todo end]

set unfirm [env_var_or_empty TOP_UNFIRM]
if { $unfirm ne "" } {
  source $::env(UNFIRM_TCL)
} elseif { $last_step eq "place" } {
  utl::error FLW 137 "top_level: TOP_TO place without TOP_UNFIRM, the first pass has no place step"
}
if { "place" in $todo && $unfirm ne "" && $last_step ne "place" } {
  utl::error FLW 138 "top_level: the place step runs alone (TOP_TO place), not\
    with the steps after it, $todo"
}

set odbs {floorplan 2_floorplan.odb place 3_top_place.odb tree 4_top_cts_tree.odb cts 4_top_cts.odb grt 5_top_grt.odb}
if { [env_var_exists_and_non_empty TOP_DIR] } {
  set ::env(RESULTS_DIR) $::env(TOP_DIR)
}
set sdc [sdc_file_of [dict get $odbs $from]]
load_design [dict get $odbs $from] -backing_file $::env(OBJECTS_DIR)/top.db

proc top_checkpoint { step odb } {
  if { [env_var_exists_and_non_empty RUN_OUTPUT_DIR] } {
    if { $step eq $::last_step } {
      orfs_write_db $::env(RUN_OUTPUT_DIR)/$odb
      file copy -force $::env(RESULTS_DIR)/$::sdc $::env(RUN_OUTPUT_DIR)/[sdc_file_of $odb]
    }
    return
  }
  orfs_write_db $::env(RESULTS_DIR)/$odb
  file copy -force $::env(RESULTS_DIR)/$::sdc $::env(RESULTS_DIR)/[sdc_file_of $odb]
}

if { "place" in $todo && $unfirm ne "" } {
  set before [unfirm::apply [unfirm::read_ends $unfirm] \
    [unfirm::env_or TOP_UNFIRM_DEEP 0] [unfirm::env_or TOP_UNFIRM_SIDE both]]
  unfirm::placed [unfirm::place] $before
  top_checkpoint place 3_top_place.odb
}
if { "tree" in $todo } {
  repair_clock_inverters
  log_cmd clock_tree_synthesis -sink_clustering_enable -repair_clock_nets \
    -sink_clustering_size $::env(CTS_CLUSTER_SIZE)
  sta::network_changed_non_sdc
  top_checkpoint tree 4_top_cts_tree.odb
}
if { "cts" in $todo } {
  pm_cells_out_of_blocks
  log_cmd detailed_placement -use_diamond_legalizer {*}[env_var_or_empty TOP_DPL_ARGS]
  log_cmd check_placement -placeable -verbose
  top_checkpoint cts 4_top_cts.odb
}
if { "grt" in $todo } {
  if { ![env_var_equals SKIP_PIN_ACCESS 1] } {
    log_cmd pin_access
  }
  set iterations [env_var_or_empty TOP_GRT_ITERATIONS]
  if { $iterations eq "" } {
    set iterations 0
  }
  log_cmd global_route -congestion_iterations $iterations -no_incremental \
    -backing_file $::env(OBJECTS_DIR)/grt.grid {*}$::env(GLOBAL_ROUTE_ARGS)
  top_checkpoint grt 5_top_grt.odb
  if { $unfirm ne "" } {
    unfirm::unrouted
  }
}

set parasitics [env_var_or_empty TOP_PARASITICS]
if { $parasitics eq "" } {
  set parasitics global_routing
}

if {
  ![env_var_exists_and_non_empty CROSSING_SETS]
  && ![env_var_exists_and_non_empty CROSSING_TXT]
} {
  return
}
if { ![env_var_exists_and_non_empty CROSSING_SETS] } {
  set_propagated_clock [all_clocks]
  log_cmd estimate_parasitics -$parasitics
  source $::env(CLOCK_CHECK)
  set ::crossing_paths_loaded 1
  source $::env(CROSSING_PATHS)
  return
}
