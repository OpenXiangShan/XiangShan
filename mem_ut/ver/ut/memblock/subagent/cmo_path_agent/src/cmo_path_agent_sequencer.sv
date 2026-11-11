`ifndef CMO_PATH_AGENT_SEQUENCER_SV
`define CMO_PATH_AGENT_SEQUENCER_SV
class cmo_path_agent_sequencer extends uvm_sequencer#(cmo_path_agent_xaction);
    `uvm_component_utils(cmo_path_agent_sequencer)
    function new(string name,uvm_component parent); super.new(name,parent); endfunction
endclass
`endif
