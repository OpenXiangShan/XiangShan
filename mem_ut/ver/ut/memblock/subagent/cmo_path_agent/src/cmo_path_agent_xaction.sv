`ifndef CMO_PATH_AGENT_XACTION_SV
`define CMO_PATH_AGENT_XACTION_SV
class cmo_path_agent_xaction extends uvm_sequence_item;
    `uvm_object_utils(cmo_path_agent_xaction)
    // 每拍monitor在CSR全局sample锚定后唯一发布；空events也构成完成水位。
    longint unsigned sample_seq;
    longint unsigned reset_generation;
    bit reset_active;
    // 完整payload仅保存在此批次一次；RM唯一consumer建立历史引用，禁止第二消费者pop。
    cmo_path_probe_t events[$];
    function new(string name="cmo_path_agent_xaction"); super.new(name); endfunction
endclass
`endif
