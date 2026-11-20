# PRE_DETAIL_PLACE_TCL, PRE_CTS_TCL and PRE_GLOBAL_ROUTE_TCL of the planned
# parent: every legalization those stages run (detailed_placement_helper,
# after placement, after CTS's buffering and after global route's repairs)
# first moves each movable cell the resizer or CTS left inside a hard
# block's box to just outside that block's nearest edge. There is no row
# under such a cell, and the legaliser can only find it a site within its
# window of where it is: on XSTile it left thousands there and failed on
# the few deeper than 100 um (DPL-0036).
proc cells_out_of_blocks {} {
  set block [ord::get_db_block]
  set boxes {}
  foreach inst [$block getInsts] {
    if { [[$inst getMaster] isBlock] && [$inst getPlacementStatus] in {FIRM LOCKED} } {
      set b [$inst getBBox]
      lappend boxes [list [$b xMin] [$b yMin] [$b xMax] [$b yMax]]
    }
  }
  # A parent macro's holes (tools/carve/parent_holes.tcl) are hard
  # blockages with the rows cut, not instances: XSTile_parent's resizer
  # left 7,105 cells in them, 3,967 deeper than 100 um.
  foreach blockage [$block getBlockages] {
    if { ![$blockage isSoft] } {
      set b [$blockage getBBox]
      lappend boxes [list [$b xMin] [$b yMin] [$b xMax] [$b yMax]]
    }
  }
  set moved 0
  foreach inst [$block getInsts] {
    if { [[$inst getMaster] isBlock] || [$inst getPlacementStatus] ne "PLACED" } { continue }
    set b [$inst getBBox]
    set x [$b xMin]; set y [$b yMin]
    foreach box $boxes {
      lassign $box x0 y0 x1 y1
      if { $x < $x0 || $x >= $x1 || $y < $y0 || $y >= $y1 } { continue }
      set w [expr { [$b xMax] - $x }]; set h [expr { [$b yMax] - $y }]
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
  utl::info FLW 9001 "cells_out_of_blocks: $moved cell(s) moved out of hard blocks before legalization"
}
# The plan's placement regions (place_macros.tcl) guide global placement
# only: before the first legalization they are dissolved, so the legalizer
# works without the fence. Inside it, XSTile_parent's detailed placement
# was still searching after 75 min; without it, it took 6
# (ideas/xiangshan-timing.md, entry 60).
proc regions_dissolved {} {
  set block [ord::get_db_block]
  set n 0
  foreach group [$block getGroups] {
    if { [$group getRegion] ne "NULL" } {
      odb::dbGroup_destroy $group
      incr n
    }
  }
  foreach region [$block getRegions] {
    odb::dbRegion_destroy $region
  }
  if { $n > 0 } {
    utl::info FLW 9002 "regions_dissolved: $n placement region(s) dissolved before legalization"
  }
}
if { [info procs detailed_placement_helper_inner] eq "" } {
  rename detailed_placement_helper detailed_placement_helper_inner
  proc detailed_placement_helper { args } {
    regions_dissolved
    cells_out_of_blocks
    detailed_placement_helper_inner {*}$args
  }
}
