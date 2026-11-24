# The Top's second pass (top_level.tcl's place step, parent_macro.bzl):
# the cells of the first pass's crossing paths over the budget are
# unfirmed, made movable in the Top's floorplan with their registers, and
# placed again among the cells that stay where their macros put them.
# Procs in the namespace unfirm; sourcing this file does nothing else.
#
# The selection is from the netlist alone, no timing. A macro is an
# instance name's first level (u_parent, u_Rob, ...). From each end pin
# the walk goes backward through drivers to the first stop: a register,
# latch, clock gate, memory or macro, as crossing_cones has it. A cell is
# selected when a path through it to the end pin has a net with cells of
# two macros on it:
#
#   every cell behind such a net, back to the stops, which are the start
#   registers;
#   every cell between such a net and the end pin.
#
# The part of an end pin's fan-in that reaches no such net is the end's
# own macro's and is left out. It is an upper bound on the cells of the
# crossing paths over a budget into those pins: every path into a pin is
# taken, failing or not. Each register deeper (TOP_UNFIRM_DEEP) adds the
# whole fan-in of the start side's registers and the whole fan-out of the
# end side's (TOP_UNFIRM_SIDE: both, back or fwd).
#
# The unfirm is a Tcl mirror of crossing_cones' flatten mode (patch
# 0174), which moves cone logic only, with the selected registers moving
# too:
#
#   a selected cell is PLACED and not dont_touch; every other cell is fixed;
#   a net with a selected cell on it loses its route and its dont_touch,
#   and so does every cell on it;
#   every other net is dont_touch;
#   the clock network is left as it is, a moved register's clock included:
#   the Top's tree step builds one tree over every register.
#
# The stops:
#   FLW 130  an end pin that is not in the design, or no end at all
#   FLW 132  a broken rule of the unfirm (unfirm::check)
#   FLW 133  a bad TOP_UNFIRM_DEEP or TOP_UNFIRM_SIDE, or the timer's clock
#            nets not the database's
#   FLW 134  a placement that did not place (unfirm::did_place)
#   FLW 135  a net without a route after global route (unfirm::unrouted)
#   FLW 22   cells global placement left far from any site, as ORFS's
#            global_place.tcl stops on them (unfirm::report_stranded_cells)
namespace eval unfirm {
  variable stop
  variable clkpins
  variable area
  variable drv
  variable up
  variable dn
  variable dnx
  variable fan
  variable on
  variable starts
  variable ends_on
  variable isend
  variable orphans 0
  variable clkterms
  variable clknet
  variable hard
  # the cells to move, and where each was, by name
  variable mov
  variable was
  # 1 when the floorplan had a routed net: the check wants a route to drop
  variable guided 0
  variable by_place
  variable by_place_built 0
  variable holes {}
  variable parent u_parent
  variable t0 [clock seconds]
}

proc unfirm::memory { phase } {
  variable t0
  set f [open /proc/self/status]
  foreach line [split [read $f] "\n"] {
    if { [string match "VmRSS:*" $line] || [string match "VmHWM:*" $line] } {
      lappend m [lrange $line 1 1]
    }
  }
  close $f
  puts [format "memory: %-12s rss %.1f GB, peak %.1f GB, at %d s" $phase \
    [expr { [lindex $m 1] / 1048576.0 }] [expr { [lindex $m 0] / 1048576.0 }] \
    [expr { [clock seconds] - $t0 }]]
}

proc unfirm::say { msg } {
  variable t0
  puts "unfirm: [format %5d [expr { [clock seconds] - $t0 }]] s  $msg"
}

proc unfirm::env_or { name default } {
  if { [info exists ::env($name)] && $::env($name) ne "" } {
    return $::env($name)
  }
  return $default
}

# a line of /proc/self/status, in kB: VmHWM is the peak resident memory
proc unfirm::status_kb { key } {
  set f [open /proc/self/status]
  set kb ""
  foreach line [split [read $f] "\n"] {
    if { [string match "$key:*" $line] } {
      set kb [lindex $line 1]
    }
  }
  close $f
  return $kb
}

# an instance name's first level, at the first divider not escaped
proc unfirm::macro_of { name } {
  set slash [string first / $name]
  while { $slash > 0 && [string index $name [expr { $slash - 1 }]] eq "\\" } {
    set slash [string first / $name [expr { $slash + 1 }]]
  }
  if { $slash < 0 } {
    return ""
  }
  return [string range $name 0 [expr { $slash - 1 }]]
}

# a stop as crossing_cones has it: a macro, a cell without liberty, a
# memory, or a cell with a clock pin (register, latch, clock gate)
proc unfirm::is_stop { inst } {
  variable stop
  variable clkpins
  variable area
  set master [$inst getMaster]
  if { [info exists stop($master)] } {
    return $stop($master)
  }
  set mname [$master getName]
  set lib [lindex [get_lib_cells -quiet $mname] 0]
  set s [expr { [$master isBlock] || $lib eq "" }]
  set clk {}
  if { $lib ne "" } {
    if { [get_property $lib is_memory] } {
      set s 1
    }
    foreach pin [get_lib_pins -quiet -of_objects $lib] {
      if { [get_property $pin is_clock] } {
        set s 1
        lappend clk [get_property $pin name]
      }
    }
  }
  set clkpins($master) $clk
  set area($master) [expr { [$master getWidth] / 1000.0 * [$master getHeight] / 1000.0 }]
  set stop($master) $s
  return $s
}

proc unfirm::area_of { inst } {
  variable area
  set master [$inst getMaster]
  if { ![info exists area($master)] } {
    is_stop $inst
  }
  return $area($master)
}

# a stop's pins a data path ends at: its inputs but the clock
proc unfirm::data_pins { inst } {
  variable clkpins
  is_stop $inst
  set clk $clkpins([$inst getMaster])
  set pins {}
  foreach it [$inst getITerms] {
    if { [$it isInputSignal] && [[$it getMTerm] getName] ni $clk } {
      lappend pins $it
    }
  }
  return $pins
}

proc unfirm::driver { net } {
  variable drv
  if { ![info exists drv($net)] } {
    set o [$net getFirstOutput]
    set drv($net) [expr { $o eq "NULL" ? "" : [$o getInst] }]
  }
  return $drv($net)
}

# end_iterms: the end pins. Fills up (behind a crossing net), on (between
# a crossing net and an end), starts and ends_on.
proc unfirm::select { end_iterms } {
  variable up
  variable dn
  variable dnx
  variable fan
  variable on
  variable starts
  variable ends_on
  variable isend
  variable orphans
  foreach a { up dn dnx fan on starts ends_on isend } {
    array unset $a
  }
  set orphans 0
  set frontier {}
  foreach it $end_iterms {
    set inst [$it getInst]
    lappend frontier $inst 0 [list $it]
    set isend($inst) 1
  }
  while { [llength $frontier] } {
    set next {}
    foreach { c crossed its } $frontier {
      set mc [macro_of [$c getName]]
      if { $its eq "" } {
        set its [$c getITerms]
      }
      foreach it $its {
        if { ![$it isInputSignal] } {
          continue
        }
        set net [$it getNet]
        if { $net eq "NULL" } {
          continue
        }
        set d [driver $net]
        if { $d eq "" } {
          continue
        }
        set md [macro_of [$d getName]]
        if { $md eq "" || $mc eq "" } {
          incr orphans
        }
        set x [expr { $crossed || ($md ne $mc && $md ne "" && $mc ne "") }]
        if { $x && !$crossed } {
          # c is right behind a net of two macros
          set dnx($c) 1
        }
        if { [is_stop $d] } {
          if { $x } {
            set starts($d) 1
          }
          continue
        }
        if { $x } {
          if { ![info exists up($d)] } {
            set up($d) 1
            lappend next $d 1 {}
          }
        } else {
          lappend fan($d) $c
          if { ![info exists dn($d)] } {
            set dn($d) 1
            lappend next $d 0 {}
          }
        }
      }
    }
    set frontier $next
  }
  # forward from the cells right behind a net of two macros, inside the
  # ends' fan-in
  set queue [array names dnx]
  while { [llength $queue] } {
    set next {}
    foreach c $queue {
      if { [info exists isend($c)] } {
        set ends_on($c) 1
        continue
      }
      if { [info exists on($c)] } {
        continue
      }
      set on($c) 1
      if { [info exists fan($c)] } {
        foreach l $fan($c) {
          lappend next $l
        }
      }
    }
    set queue $next
  }
}

# the whole fan-in of the registers' data pins, to the stops: logicVar and
# stopVar name arrays in the caller that gain what was not in them
proc unfirm::cone_back { regs logicVar stopVar } {
  upvar 1 $logicVar logic $stopVar stops
  set frontier {}
  foreach r $regs {
    lappend frontier $r [data_pins $r]
  }
  set n 0
  while { [llength $frontier] } {
    set next {}
    foreach { c its } $frontier {
      if { $its eq "" } {
        set its [$c getITerms]
      }
      foreach it $its {
        if { ![$it isInputSignal] } {
          continue
        }
        set net [$it getNet]
        if { $net eq "NULL" } {
          continue
        }
        set d [driver $net]
        if { $d eq "" } {
          continue
        }
        if { [is_stop $d] } {
          set stops($d) 1
          continue
        }
        if { ![info exists logic($d)] } {
          set logic($d) 1
          incr n
          lappend next $d {}
        }
      }
    }
    set frontier $next
  }
  return $n
}

# the whole fan-out of the registers, to the stops
proc unfirm::cone_fwd { regs logicVar stopVar } {
  upvar 1 $logicVar logic $stopVar stops
  variable clkpins
  set frontier $regs
  set n 0
  while { [llength $frontier] } {
    set next {}
    foreach c $frontier {
      foreach ot [$c getITerms] {
        if { ![$ot isOutputSignal] } {
          continue
        }
        set net [$ot getNet]
        if { $net eq "NULL" } {
          continue
        }
        foreach lt [$net getITerms] {
          if { ![$lt isInputSignal] } {
            continue
          }
          set l [$lt getInst]
          if { [is_stop $l] } {
            if { [[$lt getMTerm] getName] ni $clkpins([$l getMaster]) } {
              set stops($l) 1
            }
            continue
          }
          if { ![info exists logic($l)] } {
            set logic($l) 1
            incr n
            lappend next $l
          }
        }
      }
    }
    set frontier $next
  }
  return $n
}

# cells and area (um2) of the instances that are an array's names, by macro
proc unfirm::tally { arrVar } {
  upvar 1 $arrVar arr
  set t [dict create]
  foreach inst [array names arr] {
    set m [macro_of [$inst getName]]
    if { $m eq "" } {
      set m top
    }
    dict incr t "$m n"
    if { ![dict exists $t "$m a"] } {
      dict set t "$m a" 0.0
    }
    dict set t "$m a" [expr { [dict get $t "$m a"] + [area_of $inst] }]
  }
  return $t
}

# one line: cells and mm2 in all, then by macro
proc unfirm::tally_text { t } {
  set n 0
  set a 0.0
  set by {}
  foreach key [lsort [dict keys $t]] {
    lassign $key m what
    if { $what ne "n" } {
      continue
    }
    set mn [dict get $t "$m n"]
    set ma [dict get $t "$m a"]
    incr n $mn
    set a [expr { $a + $ma }]
    lappend by [format "%s %d (%.4f mm2)" $m $mn [expr { $ma / 1e6 }]]
  }
  return [format "%d cells, %.4f mm2: %s" $n [expr { $a / 1e6 }] [join $by {, }]]
}

# a master's clock pins: liberty's, and the LEF's USE CLOCK
proc unfirm::clock_terms { inst } {
  variable clkterms
  variable clkpins
  set master [$inst getMaster]
  if { ![info exists clkterms($master)] } {
    is_stop $inst
    set names $clkpins($master)
    foreach mt [$master getMTerms] {
      if { [$mt getSigType] eq "CLOCK" && [$mt getName] ni $names } {
        lappend names [$mt getName]
      }
    }
    set clkterms($master) $names
  }
  return $clkterms($master)
}

# The clock network's nets: sta's, the database's CLOCK nets and every net
# on a stop's clock pin. Returns how many came from each source alone and
# the total.
proc unfirm::clock_nets { } {
  variable clknet
  array unset clknet
  set block [ord::get_db_block]
  foreach net [sta::find_all_clk_nets] {
    set clknet($net) 1
    set of_sta($net) 1
  }
  set from_sta [array size of_sta]
  # every lookup here is by handle: sta's handles have to be the
  # database's, as strings, or its clock nets are strangers
  set found 0
  set from_db 0
  foreach net [$block getNets] {
    if { [info exists of_sta($net)] } {
      incr found
    }
    if { [$net getSigType] eq "CLOCK" } {
      set clknet($net) 1
      incr from_db
    }
  }
  set from_pins 0
  foreach inst [$block getInsts] {
    if { ![is_stop $inst] } {
      continue
    }
    foreach name [clock_terms $inst] {
      set it [$inst findITerm $name]
      if { $it eq "NULL" } {
        continue
      }
      set net [$it getNet]
      if { $net ne "NULL" } {
        if { ![info exists clknet($net)] } {
          incr from_pins
        }
        set clknet($net) 1
      }
    }
  }
  set strangers [expr { $from_sta - $found }]
  return [list $from_sta $strangers $from_db $from_pins [array size clknet]]
}

# "" when the cell may move, else why it stays: a macro, a memory or a cell
# without liberty (hard), a stop that drives the clock network (a clock
# gate, a divider), logic on the clock network
proc unfirm::why_fixed { inst } {
  variable clknet
  variable hard
  if { [is_stop $inst] } {
    set master [$inst getMaster]
    if { ![info exists hard($master)] } {
      set lib [lindex [get_lib_cells -quiet [$master getName]] 0]
      set hard($master) [expr { [$master isBlock] || $lib eq "" || [get_property $lib is_memory] }]
    }
    if { $hard($master) } {
      return hard
    }
    foreach it [$inst getITerms] {
      if { [$it isOutputSignal] } {
        set net [$it getNet]
        if { $net ne "NULL" && [info exists clknet($net)] } {
          return clock_driver
        }
      }
    }
    return ""
  }
  foreach it [$inst getITerms] {
    set net [$it getNet]
    if { $net ne "NULL" && [info exists clknet($net)] } {
      return clock_logic
    }
  }
  return ""
}

# movVar: an array in the caller, its names the cells to move. Returns the
# cells made movable and the nets that lost their route.
proc unfirm::unfirm { movVar } {
  upvar 1 $movVar mov
  variable clknet
  set block [ord::get_db_block]
  set movable 0
  foreach inst [$block getInsts] {
    if { [info exists mov($inst)] } {
      $inst setDoNotTouch 0
      $inst setPlacementStatus PLACED
      incr movable
    } elseif { ![$inst isFixed] } {
      $inst setPlacementStatus FIRM
    }
  }
  foreach inst [array names mov] {
    foreach it [$inst getITerms] {
      set net [$it getNet]
      if { $net eq "NULL" || [info exists clknet($net)] } {
        continue
      }
      set moving($net) 1
    }
  }
  set nets 0
  foreach net [$block getNets] {
    if { [$net getSigType] in { POWER GROUND } } {
      continue
    }
    if { [info exists moving($net)] } {
      foreach it [$net getITerms] {
        [$it getInst] setDoNotTouch 0
      }
      $net setDoNotTouch 0
      $net clearGuides
      set wire [$net getWire]
      if { $wire ne "NULL" } {
        odb::dbWire_destroy $wire
      }
      incr nets
    } elseif { ![info exists clknet($net)] } {
      $net setDoNotTouch 1
    }
  }
  return [list $movable $nets]
}

proc unfirm::fail { msg } {
  utl::error FLW 132 "unfirm: $msg"
}

# The unfirm's rules: crossing_cones' flatten mode's, with the selected
# registers allowed to move. guided: the nets that had guides before the
# unfirm. Returns moved registers, moved logic, fixed stops, fixed logic,
# dont_touch nets.
proc unfirm::check { movVar guided } {
  upvar 1 $movVar mov
  variable clknet
  set block [ord::get_db_block]
  if { [array size mov] == 0 } {
    fail "no cell is movable"
  }
  set n 0
  set regs 0
  set logic 0
  set fixed_stops 0
  set fixed_logic 0
  foreach inst [$block getInsts] {
    set s [is_stop $inst]
    if { [$inst isFixed] } {
      if { [info exists mov($inst)] } {
        fail "[$inst getName] is selected and fixed"
      }
      if { !$s } {
        incr fixed_logic
        continue
      }
      incr fixed_stops
      foreach name [clock_terms $inst] {
        set it [$inst findITerm $name]
        if { $it eq "NULL" } {
          continue
        }
        set net [$it getNet]
        if { $net ne "NULL" && [$net isDoNotTouch] } {
          fail "the clock of [$inst getName] is dont_touch"
        }
      }
      continue
    }
    incr n
    set name [$inst getName]
    if { ![info exists mov($inst)] } {
      fail "$name moves and is not selected"
    }
    if { [$inst isDoNotTouch] } {
      fail "$name moves and is dont_touch"
    }
    set why [why_fixed $inst]
    if { $why ne "" } {
      fail "$name moves and is $why"
    }
    if { $s } {
      incr regs
    } else {
      incr logic
    }
    foreach it [$inst getITerms] {
      set net [$it getNet]
      if { $net eq "NULL" || [info exists seen($net)] } {
        continue
      }
      set seen($net) 1
      if { [$net getSigType] in { POWER GROUND } } {
        continue
      }
      if { [info exists clknet($net)] } {
        # a moved register's clock: the Top's tree drives it
        if { [$net isDoNotTouch] } {
          fail "the clock of $name is dont_touch"
        }
        continue
      }
      if { [$net isDoNotTouch] } {
        fail "net [$net getName] of $name is dont_touch"
      }
      foreach other [$net getITerms] {
        if { [[$other getInst] isDoNotTouch] } {
          fail "[[$other getInst] getName], on a net of $name, is dont_touch:\
            no buffer can drive its pins"
        }
      }
      if { [llength [$net getGuides]] > 0 || [$net getWire] ne "NULL" } {
        fail "net [$net getName] of $name kept its route"
      }
    }
  }
  if { $n != [array size mov] } {
    fail "$n cells move, [array size mov] were selected"
  }
  if { $fixed_logic == 0 } {
    fail "no combinational cell stays fixed"
  }
  set fixed_nets 0
  foreach net [$block getNets] {
    if { ![$net isDoNotTouch] } {
      continue
    }
    incr fixed_nets
    foreach it [$net getITerms] {
      if { ![[$it getInst] isFixed] } {
        fail "[[$it getInst] getName] moves and its net [$net getName] is dont_touch"
      }
    }
  }
  if { $fixed_nets == 0 } {
    fail "no net is dont_touch"
  }
  if { $guided == 0 } {
    fail "no net had guides: nothing was routed to drop"
  }
  return [list $regs $logic $fixed_stops $fixed_logic $fixed_nets]
}

# the report's names: first level, last level, no escapes
proc unfirm::short_name { name } {
  set parts {}
  set from 0
  set at [string first / $name]
  while { $at >= 0 } {
    if { $at == 0 || [string index $name [expr { $at - 1 }]] ne "\\" } {
      lappend parts [string range $name $from [expr { $at - 1 }]]
      set from [expr { $at + 1 }]
    }
    set at [string first / $name [expr { $at + 1 }]]
  }
  lappend parts [string range $name $from end]
  if { [llength $parts] > 1 } {
    set name "[lindex $parts 0]/[lindex $parts end]"
  }
  return [string map { \\ {} } $name]
}

# a stop by the report's name and place: the name alone is not unique
proc unfirm::find_inst { name place } {
  variable by_place
  variable by_place_built
  set block [ord::get_db_block]
  if { !$by_place_built } {
    foreach i [$block getInsts] {
      if { [is_stop $i] } {
        set b [$i getBBox]
        set loc "[expr { [$b xMin] / 1000.0 }] [expr { [$b yMin] / 1000.0 }]"
        set by_place([short_name [$i getName]]|$loc) $i
        set by_place([string map { \\ {} } [$i getName]]|$loc) $i
      }
    }
    set by_place_built 1
  }
  foreach key [list "$name|$place" "[short_name $name]|$place"] {
    if { [info exists by_place($key)] } {
      return $by_place($key)
    }
  }
  return NULL
}

# The end pins of an ends file, "group<TAB>end pin<TAB>x y" per line
# (crossing_ends): a dict, group to its pins, each pin by its report name
# and place.
proc unfirm::read_ends { path } {
  set groups [dict create]
  set n 0
  set missing {}
  set f [open $path]
  while { [gets $f line] >= 0 } {
    if { [string trim $line] eq "" } {
      continue
    }
    lassign [split $line "\t"] group pin place
    set at [string last / $pin]
    set inst [find_inst [string range $pin 0 [expr { $at - 1 }]] $place]
    set it NULL
    if { $inst ne "NULL" } {
      set it [$inst findITerm [string range $pin [expr { $at + 1 }] end]]
    }
    if { $it eq "NULL" } {
      lappend missing $pin
      continue
    }
    dict lappend groups $group $it
    incr n
  }
  close $f
  if { [llength $missing] } {
    utl::error FLW 130 "unfirm: [llength $missing] end pins are not in the design,\
      the first [lrange $missing 0 4]"
  }
  if { $n == 0 } {
    utl::error FLW 130 "unfirm: no end to unfirm in $path"
  }
  say "$n end pins in [dict size $groups] groups"
  return $groups
}

# the clock network, for why_fixed: the timer's and the database's
proc unfirm::clocks { } {
  lassign [clock_nets] from_sta strangers from_db from_pins clock_nets
  say "clock nets: $from_sta from sta ($strangers handles not the database's),\
    $from_db CLOCK in the database, $from_pins more on clock pins, $clock_nets in all"
  if { $strangers } {
    utl::error FLW 133 "unfirm: sta's clock nets are not the database's handles"
  }
}

# The cells to unfirm, into mov: the crossing paths into the end pins and
# their registers, then deep registers deeper on side; a cell on the
# clock network, a macro or a memory stays (why_fixed, after clocks).
# Returns the counts.
proc unfirm::selection { all_ends deep side } {
  variable mov
  variable up
  variable on
  variable starts
  variable ends_on
  if { ![string is integer -strict $deep] || $deep < 0 } {
    utl::error FLW 133 "unfirm: TOP_UNFIRM_DEEP is a count of registers, not $deep"
  }
  if { $side ni { both back fwd } } {
    utl::error FLW 133 "unfirm: TOP_UNFIRM_SIDE is both, back or fwd, not $side"
  }
  set block [ord::get_db_block]
  # the paths' cells and registers, then deep registers deep
  select $all_ends
  array set logic [array get up]
  array set logic [array get on]
  array set regs [array get starts]
  array set regs [array get ends_on]
  say "the paths: [array size logic] logic, [array size starts] starts,\
    [array size ends_on] ends"
  set paths_logic [array size logic]
  set paths_starts [array size starts]
  set paths_ends [array size ends_on]
  # One register deeper: the whole fan-in of the registers the walk back
  # reached last, the whole fan-out of those the walk forward reached. Each
  # walk has its own visited set: a cell the paths selected still has side
  # inputs and loads the paths did not take, and a walk that stopped at it
  # would leave them out. A register's fan-in is walked once, and so is its
  # fan-out.
  set back [array names starts]
  set fwd [array names ends_on]
  foreach r $back {
    set walked_back($r) 1
  }
  foreach r $fwd {
    set walked_fwd($r) 1
  }
  for { set depth 1 } { $depth <= $deep } { incr depth } {
    array unset new_back
    array unset new_fwd
    if { $side ne "fwd" } {
      array unset seen
      cone_back $back seen new_back
      array set logic [array get seen]
    }
    if { $side ne "back" } {
      array unset seen
      cone_fwd $fwd seen new_fwd
      array set logic [array get seen]
    }
    array unset seen
    set back {}
    foreach r [array names new_back] {
      set regs($r) 1
      if { ![info exists walked_back($r)] } {
        set walked_back($r) 1
        lappend back $r
      }
    }
    set fwd {}
    foreach r [array names new_fwd] {
      set regs($r) 1
      if { ![info exists walked_fwd($r)] } {
        set walked_fwd($r) 1
        lappend fwd $r
      }
    }
    say "$depth deep ($side): [array size logic] logic, [array size regs] registers"
  }

  array unset mov
  set n_regs 0
  set n_logic 0
  # dont_touch in the floorplan is a block's own, its generated storage
  # among it; the unfirm lifts it from a selected cell
  array set dont_touch { register 0 logic 0 }
  foreach inst [concat [array names logic] [array names regs]] {
    set why [why_fixed $inst]
    if { $why ne "" } {
      incr excluded($why)
      continue
    }
    set mov($inst) 1
    if { [is_stop $inst] } {
      incr n_regs
      set kind register
    } else {
      incr n_logic
      set kind logic
    }
    if { [$inst isDoNotTouch] } {
      incr dont_touch($kind)
    }
  }
  set total [llength [$block getInsts]]
  say "selection: [array size mov] of $total cells\
    ([format %.1f [expr { 100.0 * [array size mov] / $total }]] %): $n_logic logic,\
    $n_regs registers; kept fixed: [array get excluded]"
  say "selection by macro: [tally_text [tally mov]]"
  say "selected and dont_touch in the floorplan: $dont_touch(register) registers,\
    $dont_touch(logic) logic"
  memory selected
  return [dict create cells $total unfirmed [array size mov] \
    unfirmed_registers $n_regs unfirmed_logic $n_logic paths_logic $paths_logic \
    paths_starts $paths_starts paths_ends $paths_ends]
}

# The place step's unfirm, on the Top's floorplan: the selection (groups:
# read_ends'), made movable, its nets' routes dropped, the rules checked,
# and the timing scope the placement is driven by: the clocks at
# TOP_CLOCK_PERIOD when it is set, and false paths off the cells that
# stay. Returns the counts placed records, deep and side among them.
proc unfirm::apply { groups deep side } {
  variable mov
  variable was
  variable guided
  set block [ord::get_db_block]
  # what the selection and the timing ran with, one setting a line: a
  # setting left out is its default, named here
  set period [env_or TOP_CLOCK_PERIOD ""]
  foreach { name value } [list TOP_UNFIRM_DEEP $deep TOP_UNFIRM_SIDE $side \
    TOP_CLOCK_PERIOD [expr { $period eq "" ? "(the SDC's)" : "$period ps" }]] {
    puts "unfirm: setting $name $value"
  }
  set all_ends {}
  dict for { group its } $groups {
    lappend all_ends {*}$its
  }
  clocks
  set counts [selection $all_ends $deep $side]

  # where each selected cell is, by name: repair may delete a cell
  array unset was
  foreach inst [array names mov] {
    set was([$inst getName]) [list {*}[$inst getLocation] \
      [expr { [is_stop $inst] ? "register" : "logic" }]]
  }

  # 1 when the design has a routed net: the check wants a route to drop
  set guided 0
  foreach net [$block getNets] {
    if { [llength [$net getGuides]] > 0 } {
      set guided 1
      break
    }
  }
  lassign [unfirm mov] movable nets
  say "unfirm: $movable cells movable, $nets nets lost their route"
  lassign [check mov $guided] c_regs c_logic c_stops c_fixed c_nets
  say "check: $c_regs registers and $c_logic logic cells move, $c_stops stops and\
    $c_fixed logic cells stay, $c_nets nets dont_touch"
  memory unfirmed

  # the period the placement is timed at: TOP_CLOCK_PERIOD is in ps,
  # create_clock takes the SDC's time unit
  if { $period ne "" } {
    set period_ui [sta::time_sta_ui [expr { $period * 1e-12 }]]
    foreach clock [all_clocks] {
      create_clock -name [get_name $clock] -period $period_ui [get_property $clock sources]
    }
    say "clocks at $period ps"
  }

  # The timing scope: paths through a movable cell, a moved register's far
  # side included. Starts: the movable registers and the stops behind a
  # movable cell; ends: the movable registers and the stops in front of one.
  foreach inst [array names mov] {
    if { [is_stop $inst] } {
      set t_start($inst) 1
      set t_end($inst) 1
    }
  }
  cone_back [array names mov] scope_back t_start
  cone_fwd [array names mov] scope_fwd t_end
  set drop_from {}
  foreach pin [all_registers -clock_pins] {
    if { ![info exists t_start([[sta::sta_to_db_pin $pin] getInst])] } {
      lappend drop_from $pin
    }
  }
  set drop_to {}
  foreach pin [all_registers -data_pins] {
    if { ![info exists t_end([[sta::sta_to_db_pin $pin] getInst])] } {
      lappend drop_to $pin
    }
  }
  say "timing scope: [array size t_start] starts, [array size t_end] ends;\
    false paths from [llength $drop_from] clock pins, to [llength $drop_to] data pins"
  if { [llength $drop_from] } {
    set_false_path -from $drop_from
  }
  if { [llength $drop_to] } {
    set_false_path -to $drop_to
  }
  memory scoped
  return [dict merge $counts [dict create deep $deep side $side unrouted_nets $nets \
    scope_starts [array size t_start] scope_ends [array size t_end]]]
}

# ORFS's report_stranded_cells, as carried patch 0086 adds it to
# global_place.tcl, copied: global_place.tcl cannot be sourced without
# running its stage.
proc unfirm::report_stranded_cells { } {
  set mode [expr { [info exists ::env(GPL_STRANDED_CELLS_CHECK)] ?
                   $::env(GPL_STRANDED_CELLS_CHECK) : "error" }]
  if { $mode eq "none" } {
    return
  }
  set block [ord::get_db_block]
  set dbu [[ord::get_db_tech] getDbUnitsPerMicron]
  # The rows as bands: {yMin yMax {x0 x1} {x0 x1} ...}, sorted by yMin. A
  # cell is on a site when its origin falls inside a band's y range and one
  # of that band's x spans. Global placement does not align cells to rows,
  # so the test is containment, not equality.
  set by_y [dict create]
  foreach row [$block getRows] {
    set bb [$row getBBox]
    dict lappend by_y [$bb yMin] [list [$bb yMax] [$bb xMin] [$bb xMax]]
  }
  set ys [lsort -integer [dict keys $by_y]]
  set n [llength $ys]
  if { $n == 0 } {
    return
  }
  # The distance beyond which a relocation is worth reporting, in microns.
  # The search is an expanding ring over rows and sites, so its cost grows
  # with the distance: a cell a fraction of a micron past a row's edge is
  # seated immediately, one a millimetre out is not.
  set limit_um [expr { [info exists ::env(GPL_STRANDED_CELLS_UM)] ?
                       $::env(GPL_STRANDED_CELLS_UM) : 10 }]
  set limit [expr { int($limit_um * $dbu) }]
  set stranded 0
  set worst 0
  set example ""
  foreach inst [$block getInsts] {
    if { [$inst isFixed] || [[$inst getMaster] isBlock] } {
      continue
    }
    lassign [$inst getLocation] x y
    # the last band whose yMin is at or below y
    set lo 0
    set hi [expr { $n - 1 }]
    set idx -1
    while { $lo <= $hi } {
      set mid [expr { ($lo + $hi) / 2 }]
      if { [lindex $ys $mid] <= $y } {
        set idx $mid
        set lo [expr { $mid + 1 }]
      } else {
        set hi [expr { $mid - 1 }]
      }
    }
    set on_site 0
    if { $idx >= 0 } {
      foreach span [dict get $by_y [lindex $ys $idx]] {
        lassign $span y1 x0 x1
        if { $y < $y1 && $x >= $x0 && $x < $x1 } {
          set on_site 1
          break
        }
      }
    }
    if { $on_site } {
      continue
    }
    # How far the relocation search has to go, which is what it costs. A
    # cell just past a row's edge is off by a fraction of a micron and is
    # seated at once; only a cell that has to travel is worth a word.
    set d -1
    foreach ry $ys {
      set dy [expr { abs($ry - $y) }]
      if { $d < 0 || $dy < $d } {
        set d $dy
      }
    }
    if { $d < $limit } {
      continue
    }
    incr stranded
    if { $d > $worst } {
      set worst $d
      set example [$inst getName]
    }
  }
  if { $stranded == 0 } {
    return
  }
  set msg [format "global placement left %d movable cell(s) more than %g um\
    from any site; the farthest is %.1f um out (%s). Detailed placement relocates\
    each by a search whose cost grows with that distance. Give those areas\
    rows, or keep the placer out of them with a blockage.\
    GPL_STRANDED_CELLS_CHECK=warn to continue, none to skip the check." \
    $stranded $limit_um [expr { $worst / double($dbu) }] $example]
  if { $mode eq "warn" } {
    utl::warn FLW 22 $msg
  } else {
    utl::error FLW 22 $msg
  }
}

# Global placement of the unfirmed cells with the density, pad, step and
# keep-overflow variables as global_place.tcl takes them, and its
# GLOBAL_PLACEMENT_ARGS and dont_use cells; of the rest of that script
# only the stranded-cells stop runs here. Both drivers always on,
# initial placement first; then repair_design, and the stranded-cells
# stop over the placed cells and the repair's. Returns what placed reads:
# global placement's log (gpl) and repair_design's (rsz), each captured
# as it prints, and the seconds it all took.
proc unfirm::place { } {
  set t0 [clock seconds]
  set_dont_use $::env(DONT_USE_CELLS)
  set density [place_density_with_lb_addon]
  set pad $::env(CELL_PAD_IN_SITES_GLOBAL_PLACEMENT)
  set gpl_args [list -density $density -pad_left $pad -pad_right $pad \
    -routability_driven -timing_driven \
    -min_phi_coef $::env(MIN_PLACE_STEP_COEF) -max_phi_coef $::env(MAX_PLACE_STEP_COEF)]
  set keep "(not set)"
  if { [info exists ::env(GPL_KEEP_OVERFLOW)] } {
    set keep $::env(GPL_KEEP_OVERFLOW)
    lappend gpl_args -keep_resize_below_overflow $keep
  }
  set more [env_var_or_empty GLOBAL_PLACEMENT_ARGS]
  lappend gpl_args {*}$more
  # what the placement ran with, one setting a line
  foreach { name value } [list \
    DONT_USE_CELLS $::env(DONT_USE_CELLS) \
    density $density \
    pad_left $pad \
    pad_right $pad \
    routability_driven on \
    timing_driven on \
    initial_place on \
    min_phi_coef $::env(MIN_PLACE_STEP_COEF) \
    max_phi_coef $::env(MAX_PLACE_STEP_COEF) \
    keep_resize_below_overflow $keep \
    GLOBAL_PLACEMENT_ARGS $more \
    after "estimate_parasitics -placement, repair_design"] {
    if { $value eq "" } {
      set value "(none)"
    }
    puts "unfirm: setting $name $value"
  }
  set gpl_t0 [clock seconds]
  # tee, not redirect: the run log keeps placement's progress lines, and
  # the capture is what placed checks the drivers in
  utl::teeStringBegin
  set rc [catch { log_cmd global_placement {*}$gpl_args } msg opts]
  set gpl_log [utl::teeStringEnd]
  if { $rc } {
    return -options $opts $msg
  }
  say "global placement took [expr { [clock seconds] - $gpl_t0 }] s"
  memory placed
  log_cmd estimate_parasitics -placement
  set rsz_t0 [clock seconds]
  utl::teeStringBegin
  set rc [catch { log_cmd repair_design } msg opts]
  set rsz_log [utl::teeStringEnd]
  if { $rc } {
    return -options $opts $msg
  }
  say "repair_design took [expr { [clock seconds] - $rsz_t0 }] s"
  memory repaired
  report_stranded_cells
  return [dict create gpl $gpl_log rsz $rsz_log seconds [expr { [clock seconds] - $t0 }]]
}

# The drivers rule, stated once: a placement is data only when its
# routability-driven and timing-driven passes ran and an unfirmed cell
# changed place; else FLW 134, naming what is missing. log: global
# placement's captured log. In gpl:
#   GPL-0040 "Routability pass N: weighted congestion C (target T), ..."
#     is each routability-driven pass's congestion (RouteBase::routability),
#     printed when the pass ends at once under the target congestion too;
#   GPL-0100 "Timing-driven iteration N/M" opens each timing-driven pass
#     (NesterovPlace), virtual or not, before the timer is called;
#   GPL-1001 "Global placement finished at iteration N" ends the placement.
# GPL-1005 and GPL-1017 print whenever routability-driven mode is on, and
# show nothing ran. Returns the passes and iterations found.
proc unfirm::did_place { log moved unfirmed } {
  set routability 0
  set congestion ""
  set timing 0
  set iterations ""
  foreach line [split $log "\n"] {
    if { [regexp {\[INFO GPL-0040\][^0-9]*([0-9]+)} $line -> n] } {
      set routability $n
      regexp {weighted congestion ([0-9.]+)} $line -> congestion
    } elseif { [regexp {\[INFO GPL-0100\][^0-9]*([0-9]+)} $line -> n] } {
      set timing $n
    } elseif { [regexp {\[INFO GPL-1001\][^0-9]*([0-9]+)} $line -> n] } {
      set iterations $n
    }
  }
  set missing {}
  if { $routability == 0 } {
    lappend missing "no routability-driven pass ran (no GPL-0040)"
  }
  if { $timing == 0 } {
    lappend missing "no timing-driven pass ran (no GPL-0100)"
  }
  if { $moved == 0 } {
    lappend missing "none of the $unfirmed unfirmed cells changed place"
  }
  if { [llength $missing] } {
    utl::error FLW 134 "unfirm: the placement did not place: [join $missing {; }]"
  }
  return [dict create nesterov_iterations $iterations routability_passes $routability \
    last_congestion $congestion timing_passes $timing]
}

# where a place is: the hole of the block it is in, else the parent
proc unfirm::region { x y } {
  variable holes
  variable parent
  foreach { hname x0 y0 x1 y1 } $holes {
    if { $x >= $x0 && $x < $x1 && $y >= $y0 && $y < $y1 } {
      return u_$hname
    }
  }
  return $parent
}

# After place: how far the unfirmed cells went and where they ended, in
# their own macro's region (HOLES, PARENT_INST) or out of it, by macro and
# kind (moved.tsv); the drivers rule (did_place); and the place record
# (place.txt, "key value" lines). Both beside the design the step writes.
# No clock period and no slack: the place step's timing drives the
# placement and is not a result. before: apply's counts.
proc unfirm::placed { place before } {
  variable was
  variable holes
  variable parent
  set block [ord::get_db_block]
  if { [env_var_exists_and_non_empty RUN_OUTPUT_DIR] } {
    set out $::env(RUN_OUTPUT_DIR)
  } else {
    set out $::env(RESULTS_DIR)
  }
  set holes {}
  foreach { hname hx hy hw hh } [env_or HOLES ""] {
    lappend holes $hname $hx $hy [expr { $hx + $hw }] [expr { $hy + $hh }]
  }
  set parent [env_or PARENT_INST u_parent]
  set f [open [file join $out moved.tsv] w]
  puts $f "macro\tkind\tcells\tgone\tmoved_over_1um\tleft_its_region\tmedian_um\tp90_um\tmax_um"
  set gone_all 0
  set all {}
  foreach name [array names was] {
    lassign $was($name) x0 y0 kind
    set macro [macro_of $name]
    set key $macro,$kind
    set rows($key) 1
    set inst [$block findInst $name]
    if { $inst eq "NULL" } {
      incr gone($key)
      incr gone_all
      continue
    }
    lassign [$inst getLocation] x1 y1
    set d [expr { (abs($x1 - $x0) + abs($y1 - $y0)) / 1000.0 }]
    lappend dist($key) $d
    lappend all $d
    if { [region [expr { $x1 / 1000.0 }] [expr { $y1 / 1000.0 }]] ne $macro } {
      incr left($key)
    }
  }
  # a row a macro and kind, one whose cells are all gone too
  foreach key [lsort [array names rows]] {
    lassign [split $key ,] macro kind
    set ds {}
    if { [info exists dist($key)] } {
      set ds [lsort -real $dist($key)]
    }
    set n [llength $ds]
    set over 0
    foreach d $ds {
      if { $d > 1.0 } {
        incr over
      }
    }
    set median 0.0
    set p90 0.0
    set max 0.0
    if { $n } {
      set median [lindex $ds [expr { $n / 2 }]]
      set p90 [lindex $ds [expr { $n * 9 / 10 }]]
      set max [lindex $ds end]
    }
    set line [format "%s\t%s\t%d\t%d\t%d\t%d\t%.1f\t%.1f\t%.1f" $macro $kind $n \
      [expr { [info exists gone($key)] ? $gone($key) : 0 }] $over \
      [expr { [info exists left($key)] ? $left($key) : 0 }] $median $p90 $max]
    puts $f $line
    say "moved: $line"
  }
  close $f
  say "$gone_all selected cells are gone after repair"

  set all [lsort -real $all]
  set moved 0
  set over 0
  foreach d $all {
    if { $d > 0 } {
      incr moved
    }
    if { $d > 1.0 } {
      incr over
    }
  }
  set median 0.0
  if { [llength $all] } {
    set median [lindex $all [expr { [llength $all] / 2 }]]
  }
  set drivers [did_place [dict get $place gpl] $moved [dict get $before unfirmed]]
  set buffers 0
  foreach line [split [dict get $place rsz] "\n"] {
    if { [regexp {\[INFO RSZ-[0-9]+\] Inserted ([0-9]+) buffers} $line -> n] } {
      incr buffers $n
    }
  }
  set f [open [file join $out place.txt] w]
  foreach key { deep side cells unfirmed unfirmed_registers unfirmed_logic \
    unrouted_nets scope_starts scope_ends } {
    puts $f "$key [dict get $before $key]"
  }
  puts $f "moved $moved"
  puts $f "moved_over_1um $over"
  puts $f [format "median_move_um %.1f" $median]
  puts $f "gone $gone_all"
  dict for { key value } $drivers {
    puts $f "$key $value"
  }
  puts $f "repair_buffers $buffers"
  puts $f "place_seconds [dict get $place seconds]"
  puts $f "peak_memory_kb [status_kb VmHWM]"
  close $f
  say "placed: $moved of [dict get $before unfirmed] unfirmed cells moved, $over over 1 um,\
    median [format %.1f $median] um; [dict get $drivers routability_passes] routability\
    and [dict get $drivers timing_passes] timing-driven passes"
}

# 1 when global route must route net: a signal or clock net of two or
# more pins; global route leaves a supply net, a net with special wires and
# one connected by abutment to their own wiring
proc unfirm::needs_route { net } {
  if { [$net getSigType] in { POWER GROUND } } {
    return 0
  }
  if { [llength [$net getITerms]] + [llength [$net getBTerms]] < 2 } {
    return 0
  }
  if { [llength [$net getSWires]] || [$net isConnectedByAbutment] } {
    return 0
  }
  return 1
}

# The nets global route must route (needs_route) with neither guides nor
# wire, by name, and how many signal and clock nets it must route
proc unfirm::unrouted_nets { } {
  set block [ord::get_db_block]
  set names {}
  set signal 0
  set clock 0
  foreach net [$block getNets] {
    if { ![needs_route $net] } {
      continue
    }
    if { [$net getSigType] eq "CLOCK" } {
      incr clock
    } else {
      incr signal
    }
    if { [llength [$net getGuides]] == 0 && [$net getWire] eq "NULL" } {
      lappend names [$net getName]
    }
  }
  return [list $names $signal $clock]
}

# FLW 135 when a net global route must route has neither guides nor wire:
# after global route, every net the unfirm dropped the route of is routed
# again
proc unfirm::unrouted { } {
  set block [ord::get_db_block]
  lassign [unrouted_nets] names signal clock
  foreach inst [$block getInsts] {
    incr status([$inst getPlacementStatus])
  }
  set parts {}
  foreach s [lsort [array names status]] {
    lappend parts "$s $status($s)"
  }
  say "routed: $signal signal and $clock clock nets of two or more pins,\
    [llength $names] unrouted; cells [join $parts {, }]"
  if { [llength $names] } {
    utl::error FLW 135 "unfirm: [llength $names] nets of two or more pins have no route\
      after global route, the first [join [lrange $names 0 4] {, }]"
  }
}

# The selection's counts by group of ends, at each deep and side of rows:
# one line each, "group deep side registers logic", the group "all" for
# every end together. After clocks; mov is left as the last row's.
proc unfirm::census { groups rows } {
  set all {}
  dict for { group its } $groups {
    lappend all {*}$its
  }
  set lines {}
  foreach { deep side } $rows {
    foreach { group its } [list all $all {*}$groups] {
      set c [selection $its $deep $side]
      lappend lines "$group $deep $side [dict get $c unfirmed_registers]\
        [dict get $c unfirmed_logic]"
    }
  }
  return $lines
}
