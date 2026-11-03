//=========================================================
// File name    : memblock_storeunit_scalar_sva.sv
// Description  : StoreUnit scalar assertion/cover shell
//=========================================================
`ifndef MEMBLOCK_STOREUNIT_SCALAR_SVA__SV
`define MEMBLOCK_STOREUNIT_SCALAR_SVA__SV

`include "memblock_compile_params.svh"

`ifdef MEMBLOCK_UT_FCOV
`ifdef MEMBLOCK_FCOV_STORE_UNIT_EN
module memblock_storeunit_scalar_sva (
    input wire clock,
    input wire reset,
    input wire reset_backend_done
);

    wire store_sva_sample_enable =
        (reset === 1'b0) && (reset_backend_done === 1'b1);

endmodule: memblock_storeunit_scalar_sva
`endif // MEMBLOCK_FCOV_STORE_UNIT_EN
`endif

`endif
