`ifndef TCNT_MACRO_DEFINE_BASE__SV
`define TCNT_MACRO_DEFINE_BASE__SV

`define TCNT_CHECK_SIG_XZ(SIG,VAR,WID) if(^(VAR)===1'bx|^(VAR)===1'bz) `uvm_error(get_type_name(),$psprintf(`"MON_XZ_CHECK: SIG %0d'h%0x`",(WID),(VAR)));
//`define TCNT_CHECK_SIG_XZ(SIG,VAR,WID) if($isunknown(VAR)) `uvm_error(get_type_name(),$psprintf(`"MON_XZ_CHECK: SIG %0d'h%0x`",(WID),(VAR)));

`endif

