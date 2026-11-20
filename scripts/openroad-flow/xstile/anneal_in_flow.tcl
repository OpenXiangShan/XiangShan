# MACRO_PLACEMENT_TCL for XSCore: place the bank groups, leave the rest.
#
# ORFS sources this from macro_place_util.tcl with the floorplan
# initialised and every macro unplaced, then runs rtl_macro_placer on
# whatever is still unplaced. The inventory is taken from the block this
# script is standing in, so the placement is a function of the current
# die rather than of one committed for some other outline, and the
# annealer is seeded, so the same die gives the same bytes.
#
# The tool and its inventory dump live in
# @bazel-orfs//tools/macro_anneal and arrive here as
# user_sources (ANNEAL_DUMP_TCL, ANNEAL_PY). The knobs are config.mk
# variables, which is where their reasons are written.

source $::env(ANNEAL_DUMP_TCL)

set anneal_work $::env(OBJECTS_DIR)/macro_anneal
file mkdir $anneal_work
set anneal_inventory $anneal_work/inventory.txt
set anneal_out $anneal_work/placement.tcl
set anneal_metrics $anneal_work/metrics.json

dump_macro_inventory $anneal_inventory

set anneal_cmd [list $::env(PYTHON_EXE) $::env(ANNEAL_PY) \
    --inventory $anneal_inventory \
    --out $anneal_out \
    --metrics $anneal_metrics \
    --seed $::env(ANNEAL_SEED) \
    --seeds [expr { [info exists ::env(ANNEAL_SEEDS)] && $::env(ANNEAL_SEEDS) ne "" ? $::env(ANNEAL_SEEDS) : 1 }] \
    --depth $::env(ANNEAL_DEPTH) \
    --min-cluster $::env(ANNEAL_MIN_CLUSTER) \
    --channel-um $::env(ANNEAL_CHANNEL_UM) \
    --block-gap-um $::env(ANNEAL_BLOCK_GAP_UM) \
    --strap-pitch-um $::env(ANNEAL_STRAP_PITCH_UM) \
    --strap-offset-um $::env(ANNEAL_STRAP_OFFSET_UM) \
    --strap-pair-um $::env(ANNEAL_STRAP_PAIR_UM) \
    --fill $::env(ANNEAL_FILL) \
    --channel-check [expr { [info exists ::env(ANNEAL_CHANNEL_CHECK)] ? $::env(ANNEAL_CHANNEL_CHECK) : "warn" }]]
if { [info exists ::env(ANNEAL_CHANNEL_AUTO)] && $::env(ANNEAL_CHANNEL_AUTO) == 1 } {
    lappend anneal_cmd --channel-auto
}
if { [info exists ::env(ANNEAL_CHANNEL_MIN_UM)] && $::env(ANNEAL_CHANNEL_MIN_UM) ne "" } {
    lappend anneal_cmd --channel-min-um $::env(ANNEAL_CHANNEL_MIN_UM)
}
puts "anneal_in_flow: [join $anneal_cmd { }]"
if { [catch { exec {*}$anneal_cmd 2>@1 } anneal_log] } {
    puts $anneal_log
    error "anneal_in_flow: macro_anneal.py failed; see $anneal_metrics"
}
puts $anneal_log

source $anneal_out
