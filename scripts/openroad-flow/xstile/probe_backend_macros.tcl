read_db $::env(RESULTS_DIR)/1_synth.odb
set block [ord::get_db_block]
set dbu [[ord::get_db_tech] getDbUnitsPerMicron]
set f [open $::env(PROBE_OUT) w]
foreach inst [$block getInsts] {
  set m [$inst getMaster]
  if { [$m isBlock] } {
    puts $f [format "%s\t%s\t%.3f\t%.3f" [$m getName] [$inst getName] [expr { double([$m getWidth]) / $dbu }] [expr { double([$m getHeight]) / $dbu }]]
  }
}
close $f
