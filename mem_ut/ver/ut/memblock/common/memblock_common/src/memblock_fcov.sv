//=========================================================
//File name    : memblock_fcov.sv
//Author       : OpenAI_Codex
//Module name  : memblock_fcov
//Discribution : memblock_fcov : function coverage
//Date         : 2026-04-12
//=========================================================
`ifndef MEMBLOCK_FCOV__SV
`define MEMBLOCK_FCOV__SV

class memblock_fcov;
    //bit [31:0] abc;
    //bit [1:0] ddd;
    //covergroup aaa_cg;
    //    abc_cp : coverpoint abc;
    //    ddd_cp : ddd data_type {bins value[]={1,2};}
    //    abc_cp_X_ddd_cp : cross abc_cp,ddd_cp{
    //        ignore_bins ddd_1=binsof(ddd_cp)intersect{1};
    //    }
    //endgroup
    extern function new();
    //extern function void aaa_sp(bit [31:0] abc,bit [1:0] ddd);
endclass
function memblock_fcov::new();
    //aaa_sp = new();
endfunction
//function void memblock_fcov::aaa_sp(bit [31:0] abc,bit [1:0] ddd);
//    this.abc = abc;
//    this.ddd = ddd;
//    aaa_cg.sample();
//endfunction

`endif
