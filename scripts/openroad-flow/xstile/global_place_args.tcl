# The parent's PRE_GLOBAL_PLACE_TCL, set only when xiangshan.bzl's
# XS_GPL_COHESION is on: its option added to the timing-driven global
# placement (3_3) alone. GLOBAL_PLACEMENT_ARGS also reaches
# global_place_skip_io.tcl's timing-blind pass (3_1); global_place.tcl
# reads it after this hook, when it calls global_placement.
set gpl_args [env_var_or_empty GLOBAL_PLACEMENT_ARGS]
if { [env_var_exists_and_non_empty XS_GPL_CLUSTERS_FILE] } {
  lappend gpl_args -clusters_file $::env(XS_GPL_CLUSTERS_FILE)
}
if { $gpl_args eq [env_var_or_empty GLOBAL_PLACEMENT_ARGS] } {
  error "global_place_args.tcl: set as PRE_GLOBAL_PLACE_TCL with no switch on"
}
puts "global_place_args.tcl: GLOBAL_PLACEMENT_ARGS $gpl_args"
set ::env(GLOBAL_PLACEMENT_ARGS) $gpl_args
