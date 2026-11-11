`ifndef CMO_PATH_AGENT_CFG_SV
`define CMO_PATH_AGENT_CFG_SV
class cmo_path_agent_cfg extends uvm_object;
    `uvm_object_utils(cmo_path_agent_cfg)
    // 该agent固定被动，无激励开关；cfg仅满足完整agent结构和统一config_db入口。
    function new(string name="cmo_path_agent_cfg"); super.new(name); endfunction
endclass
`endif
