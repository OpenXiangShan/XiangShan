# The parent as a macro, cut out in synthesis: each block, a blackbox
# here (SYNTH_BLACKBOXES), becomes ports of the parent, one per block pin,
# named u_<master>__<pin>, and the block instance goes. Its clock pins
# (CARVE_CLOCKS) get no port: the top level's clock tree drives them.
# Nothing of a block's build is read, so the parent builds beside them.
#
# Run by yosys through SYNTH_POST_HIERARCHY_SCRIPTS (a .ys that runs this
# with tcl), after hierarchy and before the kept modules are marked. Only
# the modules between the top and a block are inlined, so each block's
# instance is the top's; every other module is left to synthesis to keep
# (SYNTH_KEEP_MODULES, each its own partition) or flatten.
#
#   CARVE_TOP     the parent's top module: a partition of another kept
#                 module runs this with that module as DESIGN_NAME, and
#                 holds no block
#   CARVE_BLOCKS  the blocks, by module: one instance each
#   CARVE_CLOCKS  the blocks' clock pins, by name
set top $::env(DESIGN_NAME)
if { $top ne $::env(CARVE_TOP) } {
  yosys log "evert: $top is a partition of $::env(CARVE_TOP)'s, no block to cut out"
  return
}
set ::carve_list carve_evert.list

# what a selection lists, one object per element
proc listing { args } {
  yosys tee -q -o $::carve_list select -list {*}$args
  set f [open $::carve_list]
  set objects [lsearch -all -inline -not -exact [split [string trim [read $f]] "\n"] {}]
  close $f
  return $objects
}

# slang names a module elaborated per instance <m>$<path>
proc both_names { m } {
  return [list $m "$m\$*"]
}

# every module between the top and a block: the modules holding a
# block's instance, then the modules holding an instance of one of those
set blocks {}
foreach m $::env(CARVE_BLOCKS) {
  lappend blocks {*}[lmap p [both_names $m] { string cat "*/t:$p" }]
}
set union [lindex $blocks 0]
foreach p [lrange $blocks 1 end] {
  lappend union $p %u
}
yosys select -set carve_up {*}$union %m
set n 0
while { [set up [llength [listing @carve_up]]] != $n } {
  set n $up
  yosys select -set carve_up @carve_up @carve_up %C %m %u
}
yosys log "evert: [join [listing @carve_up] { }] inlined into $top"
# the rest held back from flatten, and let go after it
yosys select -set carve_kept A:keep_hierarchy=1
yosys setattr -mod -set keep_hierarchy 1 * @carve_up %d
# the inlined modules' paths as the plan, OpenROAD and the kept modules'
# boundaries name them, a/b/c, whatever SYNTH_HIER_SEPARATOR flattens
# the rest with: a register file inside a kept module is then
# core/backend/inner_ctrlBlock/rob/robEntryFile, not core.backend.inner_ctrlBlock/rob/...
yosys scratchpad -set flatten.separator /
yosys flatten
yosys setattr -mod -unset keep_hierarchy * @carve_kept %d

foreach m $::env(CARVE_BLOCKS) {
  set cells [listing {*}[lmap p [both_names $m] { string cat "$top/t:$p" }] %u]
  if { [llength $cells] != 1 } {
    error "evert: $m has [llength $cells] instances in $top, the parent macro cuts out one; a kept module (SYNTH_KEEP_MODULES) holding it hides it"
  }
  set cell [string range [lindex $cells 0] [string length "$top/"] end]
  yosys cd $top
  yosys rename $cell u_$m
  yosys cd
  yosys log "evert: $m, instance $cell, is now ports u_${m}__*"
}
file delete $::carve_list
yosys expose -evert -sep __ {*}[lmap m $::env(CARVE_BLOCKS) { string cat "$top/u_$m" }]
foreach m $::env(CARVE_BLOCKS) {
  foreach c $::env(CARVE_CLOCKS) {
    yosys delete -port "$top/w:u_${m}__$c"
  }
}
yosys opt_clean
