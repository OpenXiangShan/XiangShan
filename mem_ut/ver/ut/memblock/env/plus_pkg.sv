//=========================================================
//File name    : plus_pkg.sv
//Author       : OpenAI_Codex
//Module name  : plus_pkg
//Discribution : package wrapper for memblock plus parameters
//Date         : 2026-05-14
//=========================================================
`ifndef MEMBLOCK_PLUS_PKG__SV
`define MEMBLOCK_PLUS_PKG__SV

`ifndef TCNT_HAD_INCLUDE_UVM_MACROS
`define TCNT_HAD_INCLUDE_UVM_MACROS
    `include "uvm_macros.svh"
`endif

package plus_pkg;
    import uvm_pkg::*;
    import tcnt_realtime::*;
    import tcnt_dec_base::*;
    import tcnt_common_method::*;
    import tcnt_base_pkg::*;

    `include "../env/plus.sv"
endpackage

import plus_pkg::*;

`endif
