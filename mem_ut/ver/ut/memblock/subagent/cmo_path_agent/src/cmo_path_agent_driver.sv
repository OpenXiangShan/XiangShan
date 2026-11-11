`ifndef CMO_PATH_AGENT_DRIVER_SV
`define CMO_PATH_AGENT_DRIVER_SV
// 完整被动agent保留driver类型，但顶层不例化、不启动，不驱动任何接口。
class cmo_path_agent_driver extends uvm_driver#(cmo_path_agent_xaction);
    `uvm_component_utils(cmo_path_agent_driver)
    function new(string name,uvm_component parent); super.new(name,parent); endfunction
endclass
`endif
