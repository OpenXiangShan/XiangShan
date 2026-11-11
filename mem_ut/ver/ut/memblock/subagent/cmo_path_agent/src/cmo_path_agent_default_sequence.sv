`ifndef CMO_PATH_AGENT_DEFAULT_SEQUENCE_SV
`define CMO_PATH_AGENT_DEFAULT_SEQUENCE_SV
class cmo_path_agent_default_sequence extends uvm_sequence#(cmo_path_agent_xaction);
    `uvm_object_utils(cmo_path_agent_default_sequence)
    function new(string name="cmo_path_agent_default_sequence"); super.new(name); endfunction
    // 被动观察没有responder生命周期，禁止向DUT发送item。
    task body(); endtask
endclass
`endif
