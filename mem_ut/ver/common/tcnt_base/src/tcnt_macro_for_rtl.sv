`ifndef TCNT_HAD_INCLUDE_UVM_MACROS
`define TCNT_HAD_INCLUDE_UVM_MACROS
    `include "uvm_macros.svh"
`endif

`ifndef TCNT_HAD_IMPORT_UVM_PKG
`define TCNT_HAD_IMPORT_UVM_PKG
    import uvm_pkg::*;
`endif

`define TCNT_T_DISPLAY(severity,message) \
    if(severity == "ERROR")begin \
        `uvm_error("ERROR", $sformatf("%s",message)) \
    end \
    else if(severity == "INFO")begin \
        `uvm_info("INFO", $sformatf("%s",message),UVM_HIGH) \
    end \

