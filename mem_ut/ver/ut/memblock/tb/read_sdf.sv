//=========================================================
//File name    : read_sdf.sv
//Author       : OpenAI_Codex
//Module name  : read_sdf
//Discribution : read_sdf : read the sdf
//Date         : 2026-04-12
//=========================================================
`ifndef READ_SDF__SV
`define READ_SDF__SV


`ifdef RTL_SIM
initial begin
    $display("doing rtl simulation");
end
`endif

`ifdef GATE_SIM
initial begin
    $display("doing gate simulation without sdf");
end
`endif


`ifdef MAX_SDF
initial begin
    $display("doing net simulation with setup sdf");
end
initial begin
    if(!($value$plusargs("sdf_path=%s",sdf_path))) begin
        sdf_path = "TODO";
        if(sdf_path=="TODO") begin
            `uvm_fatal("SDF_ANNOTATE",$sformatf("HAVE NOT SDF PATH ASSIGN, please give a default path"))
        end
        `uvm_info("SDF_ANNOTATE",$sformatf("HAVE NOT SDF PATH ASSIGN, use the default sdf path : ",sdf_path),UVM_NONE)
    end
    $sdf_annotate(sdf_path,//""/*sdf path*/,
                  top_tb.dut,
                  /*config_file*/,
                  "max_sdf.log"/*"Log_file"*/,
                  "MAXIMUM"/*"(Mtm_spec):MINIMUM,TYPICAL,MAXIMUM,TOOL_CONTROL"*/,
                  /*"Scale_factors"----min:type:max=1.0:1.0:1.0*/,
                  "FROM_MAXIMUM"/*"(Scale_type)FROM_MINIMUM,FROM_TYPICAL,FROM_MAXIMUM,FROM_MTM"*/
                 );
end
`endif

`ifdef MIN_SDF
initial begin
    $display("doing net simulation with hold sdf");
end
initial begin
    if(!($value$plusargs("sdf_path=%s",sdf_path))) begin
        sdf_path = "TODO";
        if(sdf_path=="TODO") begin
            `uvm_fatal("SDF_ANNOTATE",$sformatf("HAVE NOT SDF PATH ASSIGN, please give a default path"))
        end
        `uvm_info("SDF_ANNOTATE",$sformatf("HAVE NOT SDF PATH ASSIGN, use the default sdf path : ",sdf_path),UVM_NONE)
    end
    $sdf_annotate(sdf_path,//""/*sdf path*/,
                  top_tb.dut,
                  /*config_file*/,
                  "min_sdf.log"/*"Log_file"*/,
                  "MINIMUM"/*"(Mtm_spec):MINIMUM,TYPICAL,MAXIMUM,TOOL_CONTROL"*/,
                  /*"Scale_factors"----min:type:max=1.0:1.0:1.0*/,
                  "FROM_MINIMUM"/*"(Scale_type)FROM_MINIMUM,FROM_TYPICAL,FROM_MAXIMUM,FROM_MTM"*/
                 );
end
`endif

`ifdef TYPICAL_SDF
initial begin
    $display("doing net simulation with typical sdf");
end
initial begin
    if(!($value$plusargs("sdf_path=%s",sdf_path))) begin
        sdf_path = "TODO";
        if(sdf_path=="TODO") begin
            `uvm_fatal("SDF_ANNOTATE",$sformatf("HAVE NOT SDF PATH ASSIGN, please give a default path"))
        end
        `uvm_info("SDF_ANNOTATE",$sformatf("HAVE NOT SDF PATH ASSIGN, use the default sdf path : ",sdf_path),UVM_NONE)
    end
    $sdf_annotate(sdf_path,//""/*sdf path*/,
                  top_tb.dut,
                  /*config_file*/,
                  "typical_sdf.log"/*"Log_file"*/,
                  "TYPICAL"/*"(Mtm_spec):MINIMUM,TYPICAL,MAXIMUM,TOOL_CONTROL"*/,
                  /*"Scale_factors"----min:type:max=1.0:1.0:1.0*/,
                  "FROM_TYPICAL"/*"(Scale_type)FROM_MINIMUM,FROM_TYPICAL,FROM_MAXIMUM,FROM_MTM"*/
                 );
end
`endif

`endif
