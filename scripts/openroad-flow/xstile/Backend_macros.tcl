# Explicit deterministic placement of Backend's 5 generated register-file macros
# inside Backend (DIE_AREA 0 0 1436.400 984.960, CORE_AREA 10.800 10.800 1425.600 974.160).
#
# Backend's pins sit on its North (top) edge: MemBlock (4,878 pins) across the
# West/Centre-North (x = 5..1003 um) and Frontend (2,096 pins) across the
# East-North (x = 1003..1431 um). Placing the 5 register files in dataflow order
# along the South wall (y = 32.400 um = 60 double-rows, >= 20.8 um channels
# between every pair, >= 19.9 um core clearance) leaves a continuous 1,414 x 555 um
# unobstructed logic region across the North half facing MemBlock and Frontend,
# and eliminates any narrow slit between RobEntryFile and RenameBufferFile.

set block [ord::get_db_block]
set dbu [[ord::get_db_tech] getDbUnitsPerMicron]

proc backend_place_regfile { inst_path master_expect x y w_expect h_expect } {
  set block [ord::get_db_block]
  set dbu [[ord::get_db_tech] getDbUnitsPerMicron]
  set found "NULL"
  foreach cand [list $inst_path $inst_path/ram $inst_path.ram] {
    set i [$block findInst $cand]
    if { $i ne "NULL" } {
      set found $i
      break
    }
  }
  if { $found eq "NULL" } {
    foreach m [$block getInsts] {
      if { [[$m getMaster] isBlock] && [string map {\\ ""} [$m getName]] eq $inst_path } {
        set found $m
        break
      }
    }
  }
  if { $found eq "NULL" } {
    utl::error FLW 91 "Backend_macros.tcl: macro instance $inst_path ($master_expect) not found"
  }
  set master [$found getMaster]
  set bw [expr { double([$master getWidth]) / $dbu }]
  set bh [expr { double([$master getHeight]) / $dbu }]
  if { abs($bw - $w_expect) > 1.0 || abs($bh - $h_expect) > 1.0 } {
    utl::error FLW 92 [format "Backend_macros.tcl: %s size %.3f x %.3f um differs from expected %.3f x %.3f um" \
      $master_expect $bw $bh $w_expect $h_expect]
  }
  place_macro -macro_name [$found getName] -location [list $x $y] -orientation R0 -exact
  $found setPlacementStatus FIRM
  puts [format "Backend_macros.tcl: %-18s (%s) at (%.3f, %.3f) size %.3f x %.3f um R0 FIRM" \
    $master_expect [$found getName] $x $y $bw $bh]
}

# West -> East along South wall (y = 32.400 um = 60 * 0.54 um, all x multiples of 0.432 um = 8 sites):
#   1. VfRegFile        [  30.672 ..  186.896], gap = 20.896 um
#   2. FpRegFile        [ 207.792 ..  510.140], gap = 21.220 um
#   3. IntRegFile       [ 531.360 ..  910.960], gap = 21.296 um
#   4. RenameBufferFile [ 932.256 .. 1151.986], gap = 21.326 um (>= 20 um, no slit!)
#   5. RobEntryFile     [1173.312 .. 1405.678], East core clearance = 19.922 um
backend_place_regfile "inner_vecRegion/vfRegFile"                "VfRegFile"          30.672 32.400 156.224 559.820
backend_place_regfile "inner_fpRegion/fpRegFile"                 "FpRegFile"         207.792 32.400 302.348 281.180
backend_place_regfile "inner_intRegion/intDataPath/intRegFile"   "IntRegFile"        531.360 32.400 379.600 248.620
backend_place_regfile "inner_ctrlBlock/rob/rab/renameBuffer"     "RenameBufferFile"  932.256 32.400 219.730 385.780
backend_place_regfile "inner_ctrlBlock/rob/robEntryFile"         "RobEntryFile"     1173.312 32.400 232.366 385.780

foreach inst [$block getInsts] {
  if { [[$inst getMaster] isBlock] && ![$inst isFixed] } {
    utl::error FLW 93 "Backend_macros.tcl: unplaced block macro [$inst getName] ([[$inst getMaster] getName])"
  }
}
