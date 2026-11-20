/* XiangShan's clock gate, mapped onto ASAP7's integrated clock gate.
 *
 * XiangShan's Utility library defines ClockGate as a transparent latch
 * on the low phase of CK holding TE | E, ANDed with CK. ICGx1 is that
 * function as one cell: its liberty says latch_posedge_precontrol, a
 * latch on the low phase of CLK holding ENA | SE, and GCLK = CLK & IQ.
 * So E goes to ENA and TE, the test enable, to SE: equivalent by the
 * cell's own definition, not a change of behaviour.
 *
 * Left behavioural, the latch lets the enable borrow only half a cycle
 * and gives CTS a latch and an AND gate to build through instead of a
 * gating cell. xs_icg.ys applies this template; the VeeR flow maps its
 * clockhdr the same way (bazel-orfs, test/coremark_joule/flow/cmj_veer_icg_map.v).
 *
 * Ports are XiangShan's, body is ASAP7's.
 *
 * SPDX-License-Identifier: Apache-2.0
 */
module ClockGate (
	input  wire TE,
	input  wire E,
	input  wire CK,
	output wire Q
);
	ICGx1_ASAP7_75t_R latch (
		.CLK (CK),
		.ENA (E),
		.SE  (TE),
		.GCLK(Q)
	);
endmodule
