# 473 ps: the synthesis target for a core that should route at 591 ps,
# which is XiangShan's 333 ps on 7 nm in fanouts of four. The published
# ASAP7 RVT FO4 is 8.1 ps (Clark et al., "Design Flows and Collateral for
# the ASAP7 7nm FinFET Predictive Process Design Kit", MSE 2017), which
# its authors call realistic for industrial 7 nm: 333 ps is 41.1 FO4.
# The libraries this flow times with, RVT at the FF corner, have an FO4
# of 14.37 ps (fo4.py), so 41.1 FO4 is 591 ps at global route, and 0.8
# of that is the synthesis period. LVT (6.8 ps published) and SLVT (6 ps)
# give the same period within 0.3 percent: the libraries are slower than
# the published figures by one factor in every Vt class, so the target
# does not depend on the Vt mix. period_fo4_test checks the arithmetic.
set clk_name clk
set clk_port_name clock
set clk_period 473

# The IO budget, as optimization targets. set_input_delay and
# set_output_delay are deliberately not used anywhere in this study:
# see §3.8 of the paper, which is where the model is argued.
#
# The short form is that a CPU core's frequency is set by its
# register-to-register paths and by nothing else, because the thing on
# the other side of its pins is a clock-crossing bridge, a bus register
# or a GPIO pad -- something that terminates the path rather than
# continuing it. So assume a register immediately outside every port.
# For XSTile that thing is the asynchronous crossing in XSTileWrap,
# immediately outside it -- the CHI bridge, the CLINT time queue, the
# reset synchronisers -- which registers every signal it takes.
#
# $PLATFORM_DIR/constraints.sdc implements exactly that with
# set_max_delay, and defaults each budget to 80 ps when the design says
# nothing -- a figure written for a small macro, and an impossible
# target on a CPU. An optimiser chasing an impossible target upsizes
# cells and inserts buffers whose power is then charged to the core.
#
# The ratios are ORFS's own, from its asap7/swerv_wrapper: 0.8 of the
# period for a path with a register at one end, leaving 0.2 for the
# clock-to-q and setup of the register assumed outside, and 0.6 for a
# combinational path straight through.
set in2reg_max  [expr { $clk_period * 0.8 }]
set reg2out_max [expr { $clk_period * 0.8 }]
set in2out_max  [expr { $clk_period * 0.6 }]

# Reset is a false path here. XiangShan's reset is asynchronous -- 1005 of
# XSCore's always blocks are `posedge clock or posedge reset` -- and the one
# `reset` port reaches the async-reset pin of every one of those flops.
# Timed, that is a recovery and removal check at every flop against the
# arrival through a buffer tree the flow builds for the net, and
# repair_timing at every stage would fight it. A chip de-asserts such a
# reset through a synchroniser and lets the tree settle; the check is
# not what sets the core's frequency. repair_design still buffers the
# net for slew and load, as a real reset tree is.
foreach p [get_ports -quiet reset] {
  set_false_path -from $p
}

# CoupledL2's data SRAMs are read over two cycles, as the RTL states:
# XSCache's DataStorage.scala builds them with readMCP2 = true ("read
# data is set MultiCycle Path 2 ... s3 read, s4 pass and s5 to
# destination"), holds the request for two cycles and asserts that no
# request follows another ("Continuous SRAM req prohibited under
# MCP2!"). The constraint is the designers' contract, not an exception
# made for a tool. It covers what the SRAM launches only: the request and
# write data reach the SRAM at the next edge of its gated clock, one
# cycle, and stay single-cycle paths. In the parent, where CoupledL2 is
# a macro, nothing matches.
foreach c [get_cells -quiet -hierarchical \
             -filter "full_name =~ *dataStorage.array.*array_ext" *] {
  set_multicycle_path -setup 2 -from $c
  set_multicycle_path -hold 1 -from $c
}

# A fanout cap for the resizer. Without one, broadcast nets -- valids,
# enables, decoded control -- come out of repair_design as long serial
# repeater chains that global route then has to carry. 32 is a starting
# point, textbook order of magnitude; the right value is a sweep against
# the PDK's max_transition, and this one has not been swept.
set_max_fanout 32 [current_design]

# The platform's constraints add group_path targets between inputs,
# registers and outputs. VectorDecodeChannel, a block of this design, is
# pure decode with no register, and group_path refuses an empty -from or
# -to (STA-0391); a design without registers has no such groups to name.
if { [llength [all_registers]] > 0 } {
  source $::env(PLATFORM_DIR)/constraints.sdc
}
