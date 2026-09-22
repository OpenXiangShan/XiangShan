//=========================================================
// File name    : memblock_ldu_scalar_sva.sv
// Module name  : memblock_ldu_scalar_sva
// Description  : scalar LoadUnit assertion and cover-property shell
//=========================================================
`ifndef MEMBLOCK_LDU_SCALAR_SVA__SV
`define MEMBLOCK_LDU_SCALAR_SVA__SV

`ifdef MEMBLOCK_UT_FCOV
module memblock_ldu_scalar_sva (
    input wire clock,
    input wire reset,
    input wire reset_backend_done
);

    // 标量 LDU assert/cover property 后续统一在本模块中实现。property 的
    // disable 条件必须覆盖 reset 和 backend 尚未完成初始化的阶段。
    wire scalar_sva_sample_enable =
        (reset === 1'b0) && (reset_backend_done === 1'b1);

endmodule: memblock_ldu_scalar_sva
`endif

`endif
