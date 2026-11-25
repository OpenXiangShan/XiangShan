# A part's KPI, read in the build: its reg2reg period (the SDC period minus the
# group's worst slack) and the worst path of each group, written to KPI_OUT.
# Every part, the parent and each block alone, is read at global route with the
# route's parasitics and its clock tree, as the README measures them
# (KPI_STAGE grt; place reads a place-stage checkpoint against an ideal clock).
source $::env(SCRIPTS_DIR)/load.tcl
if { $::env(KPI_STAGE) eq "grt" } {
  load_design 5_1_grt.odb
  set_propagated_clock [all_clocks]
  estimate_parasitics -global_routing
} else {
  load_design 3_place.odb
  estimate_parasitics -placement
}
set p [get_property [get_clocks] period]
set out {}
foreach g {reg2reg in2reg reg2out in2out} {
  set ps [find_timing_paths -path_group $g -sort_by_slack -group_path_count 1]
  if { [llength $ps] } {
    set t [lindex $ps 0]
    lappend out [format "%-8s %8.0f ps  %s -> %s" $g [get_property $t slack] \
      [get_full_name [get_property $t startpoint]] [get_full_name [get_property $t endpoint]]]
    if { $g eq "reg2reg" } { set r2r [get_property $t slack] }
  }
}
# Every reg2reg endpoint's period (the SDC period minus its slack), in 10 ps
# bins from the first occupied one: the mass above a target, not only the
# worst path. Fine enough to rebin when drawing.
set bin 10
array set hist {}
set ends [find_timing_paths -path_group reg2reg -endpoint_path_count 1 \
  -group_path_count [sta::endpoint_count] -unique_paths_to_endpoint]
foreach t $ends {
  set b [expr { int(floor(($p - [get_property $t slack]) / $bin)) }]
  if { [info exists hist($b)] } { incr hist($b) } else { set hist($b) 1 }
}
set bins [lsort -integer [array names hist]]
set counts {}
for { set b [lindex $bins 0] } { $b <= [lindex $bins end] } { incr b } {
  lappend counts [expr { [info exists hist($b)] ? $hist($b) : 0 }]
}
lappend out [format "reg2reg endpoints bin %d ps from %d ps: %s" $bin \
  [expr { [lindex $bins 0] * $bin }] [join $counts " "]]
lappend out [format "reg2reg min period %.0f ps" [expr { $p - $r2r }]]
set f [open $::env(KPI_OUT) w]
puts $f [join $out "\n"]
close $f

# KPI_PATHS: what the worst reg2reg paths are made of. Each path's period
# split into the launch and capture clock arrivals (their difference is the
# skew the path gets), cell delay and wire delay; the buffers and inverters
# on it, repair's or synthesis's; the distance between its two registers and
# the length of its wires, pin to pin, in a straight line; and the scopes it
# runs through with the arrival on entering each. Then the endpoints over the
# target by the module of their register.
proc kpi_xy { pin } {
  set box [[[sta::sta_to_db_pin $pin] getInst] getBBox]
  return [list [expr { ([$box xMin] + [$box xMax]) / 2 }] [expr { ([$box yMin] + [$box yMax]) / 2 }]]
}
proc kpi_um { dbu } {
  return [expr { double($dbu) / [[ord::get_db_block] getDbUnitsPerMicron] }]
}
proc kpi_dist { a b } {
  return [expr { abs([lindex $a 0] - [lindex $b 0]) + abs([lindex $a 1] - [lindex $b 1]) }]
}
if { [info exists ::env(KPI_PATHS)] } {
  set n [expr { [info exists ::env(KPI_PATH_COUNT)] ? $::env(KPI_PATH_COUNT) : 20 }]
  set target [expr { [info exists ::env(KPI_TARGET_PS)] ? $::env(KPI_TARGET_PS) : 2000 }]
  set keyed {}
  foreach t $ends { lappend keyed [list [get_property $t slack] $t] }
  set sorted {}
  foreach k [lsort -real -index 0 $keyed] { lappend sorted [lindex $k 1] }
  # everything the paths say is read first: asking a pin for its clock
  # arrival afterwards updates the search, and the paths found before it
  # are not to be touched again
  array set over {}
  # who talks to whom: the endpoints over the target by the scope of the
  # register their worst path starts at and the one it ends at, cut to
  # KPI_CENSUS_DEPTH levels, with the excess over the target summed
  set depth [expr { [info exists ::env(KPI_CENSUS_DEPTH)] ? $::env(KPI_CENSUS_DEPTH) : 4 }]
  array set pair {}
  array set excess {}
  foreach t $sorted {
    set period [expr { $p - [get_property $t slack] }]
    if { $period <= $target } { break }
    set ends2 {}
    foreach pin [list [get_property $t startpoint] [get_property $t endpoint]] {
      set s [lrange [split [get_full_name $pin] /] 0 end-2]
      set s [join [lrange $s 0 [expr { $depth - 1 }]] /]
      lappend ends2 [expr { $s eq "" ? "." : $s }]
    }
    set key [join $ends2 " -> "]
    if { [info exists pair($key)] } {
      incr pair($key)
      set excess($key) [expr { $excess($key) + $period - $target }]
    } else {
      set pair($key) 1
      set excess($key) [expr { $period - $target }]
    }
    set parts [split [get_full_name [get_property $t endpoint]] /]
    set m [join [lrange $parts 0 end-2] /]
    if { [info exists over($m)] } { incr over($m) } else { set over($m) 1 }
  }
  set rows {}
  foreach t [lrange $sorted 0 [expr { $n - 1 }]] {
    set pts {}
    foreach pt [get_property $t points] {
      set pin [get_property $pt pin]
      set master [get_property [get_cells -of_objects $pin] ref_name]
      lappend pts [list [get_full_name $pin] [get_property $pt arrival] \
        [get_property $pin direction] $master [kpi_xy $pin]]
    }
    lappend rows [list [expr { $p - [get_property $t slack] }] $pts]
  }
  set registers [llength [all_registers -cells]]
  set clocked [llength [all_registers -cells -clock [all_clocks]]]
  set nends [llength $ends]
  unset ends sorted keyed
  set f [open $::env(KPI_PATHS) w]
  # registers the clock does not reach have no reg2reg path, and no number
  # here counts them: say how many there are
  puts $f [format "registers %d, clocked %d, reg2reg endpoints %d" $registers $clocked $nends]
  puts $f "path  period  launch capture   skew   cell   wire  bufs/cells  regs_um  wire_um  start -> end"
  set i 0
  foreach row $rows {
    lassign $row period pts
    set first [lindex $pts 0]
    set last [lindex $pts end]
    set arrivals {}
    foreach end [list $first $last] {
      set inst [join [lrange [split [lindex $end 0] /] 0 end-1] /]
      set a 0
      foreach c [get_pins -of_objects [get_cells $inst]] {
        if { [get_property $c is_register_clock] } { set a [get_property $c arrival_max_rise] }
      }
      lappend arrivals $a
    }
    lassign $arrivals launch capture
    # clk-to-Q is the first point's arrival after the launch clock's
    set cell [expr { [lindex $first 1] - $launch }]
    set wire 0.0
    set bufs 0
    set cells 0
    set wire_dbu 0
    for { set k 1 } { $k < [llength $pts] } { incr k } {
      lassign [lindex $pts [expr { $k - 1 }]] - a0 - - xy0
      lassign [lindex $pts $k] - a1 dir master xy1
      if { $dir eq "input" } {
        set wire [expr { $wire + $a1 - $a0 }]
        incr wire_dbu [kpi_dist $xy0 $xy1]
      } else {
        set cell [expr { $cell + $a1 - $a0 }]
        incr cells
        if { [string match "BUF*" $master] || [string match "INV*" $master] } { incr bufs }
      }
    }
    puts $f [format "%4d %7.0f %7.0f %7.0f %6.0f %6.0f %6.0f %5d/%-5d %8.0f %8.0f  %s -> %s" \
      [incr i] $period $launch $capture [expr { $capture - $launch }] $cell $wire \
      $bufs $cells [kpi_um [kpi_dist [lindex $first 4] [lindex $last 4]]] [kpi_um $wire_dbu] \
      [lindex $first 0] [lindex $last 0]]
  }
  set i 0
  foreach row $rows {
    puts $f "path [incr i] scopes, arrival on entering:"
    set scope {}
    foreach pt [lindex $row 1] {
      set s [join [lrange [split [lindex $pt 0] /] 0 end-2] /]
      if { $s eq "" } { set s . }
      if { $s ne $scope } {
        puts $f [format "  %8.0f %s" [lindex $pt 1] $s]
        set scope $s
      }
    }
  }
  set total 0
  set mods {}
  foreach m [array names over] {
    incr total $over($m)
    lappend mods [list $over($m) $m]
  }
  puts $f [format "endpoints over %d ps: %d" $target $total]
  foreach r [lsort -integer -decreasing -index 0 $mods] {
    puts $f [format "  %6d %s" {*}$r]
  }
  set pairs {}
  foreach k [array names pair] {
    lappend pairs [list $pair($k) [expr { $excess($k) / 1000.0 }] $k]
  }
  puts $f [format "endpoints over %d ps by launch -> capture scope, %d levels (endpoints, excess ns):" \
    $target $depth]
  foreach r [lsort -integer -decreasing -index 0 $pairs] {
    puts $f [format "  %6d %9.1f  %s" {*}$r]
  }
  close $f
}
