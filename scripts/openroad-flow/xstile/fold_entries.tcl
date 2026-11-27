# Fold slang's per-instance copies of each module in XS_FOLD_MODULES back
# onto one module of the bare name, so synthesis maps it once.
#
# Run as SYNTH_CANONICALIZE_TCL, before the canonical checkpoint is
# written: bazel-orfs's partitioned synthesis resolves a kept module to
# one copy in that checkpoint (synth_partition.sh), so the fold must come
# first for every instance to be that partition. The slang frontend
# elaborates one module per instance and names it after the instance
# path (RobEntry$<path>): the Rob's 352 entry cells would be 352 modules
# and 352 ABC runs. The modules listed are the arrays front end's entry
# cells (arrays/extract.py), which take no parameters, so every copy is
# the same module: one copy is renamed to the bare name, every instance
# retyped onto it, and the rest pruned.
#
# On a Verilog slice (SYNTH_CANONICALIZE_SLICE) a module may have no
# copies, the slice holding only the modules under its kept module; the
# top may be a copy, and so may a kept module's stub, a black box. The
# whole design's canonicalize still requires every module listed.
#
# SPDX-License-Identifier: Apache-2.0
set top $::env(DESIGN_NAME)
foreach base $::env(XS_FOLD_MODULES) {
  set copies {}
  foreach line [split [yosys tee -q -s result.string select -list-mod "=$base\\\$*"] "\n"] {
    set line [regsub {^\\} [string trim $line] {}]
    if { $line ne "" && ![string match "Warning: *" $line] } {
      lappend copies $line
    }
  }
  if { [llength $copies] == 0 } {
    if { [info exists ::env(SYNTH_CANONICALIZE_SLICE)] } {
      continue
    }
    error "fold_entries: no copies of $base in the design"
  }
  if { [lsearch -exact $copies $top] >= 0 } {
    set top $base
  }
  set copies [lsort $copies]
  yosys rename [lindex $copies 0] $base
  yosys chtype -set $base "t:$base\\\$*"
  puts "fold_entries: $base, [llength $copies] copies folded onto one"
}
yosys hierarchy -check -top $top
