//=========================================================
//File name    : memblock_compile_params.svh
//Author       : OpenAI_Codex
//Module name  : memblock_compile_params
//Discribution : compile-time parameters for memblock UT
//Date         : 2026-05-24
//=========================================================
`ifndef MEMBLOCK_COMPILE_PARAMS__SVH
`define MEMBLOCK_COMPILE_PARAMS__SVH

// L2TLB connect-time takeover switch.
// 1: mem_ut L2TLB_agent owns the DTLB <-> L2TLB response path.
// 0: keep the RTL internal PTW/L2TLB path observable through the agent interface.
// This is a compile-time connection decision; runtime sequence enable remains in plus/seq_csr_common.
`ifndef MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN
    `define MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN 1
`endif

`endif
