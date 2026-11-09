//=========================================================
//File name    : memblock_dispatch_real_cbo_directed_vseq.sv
//Author       : OpenAI_Codex
//Module name  : memblock_dispatch_real_cbo_directed_vseq
//Discribution : directed real dispatch CBO virtual sequence
//Date         : 2026-11-09
//=========================================================
`ifndef MEMBLOCK_DISPATCH_REAL_CBO_DIRECTED_VSEQ__SV
`define MEMBLOCK_DISPATCH_REAL_CBO_DIRECTED_VSEQ__SV

// 中文注释：仅替换 real-smoke 的主表 owner；LSQ、issue、commit、L2TLB、
// DCache、SBuffer 和 redirect worker 继续由父类唯一启动。
class memblock_dispatch_real_cbo_directed_vseq extends
    memblock_dispatch_real_smoke_vseq;

    `uvm_object_utils(memblock_dispatch_real_cbo_directed_vseq)

    extern function new(string name = "memblock_dispatch_real_cbo_directed_vseq");
    extern virtual task start_main_dispatch_sequence();

endclass:memblock_dispatch_real_cbo_directed_vseq

function memblock_dispatch_real_cbo_directed_vseq::new(
    string name = "memblock_dispatch_real_cbo_directed_vseq"
);
    super.new(name);
endfunction:new

task memblock_dispatch_real_cbo_directed_vseq::start_main_dispatch_sequence();
    memblock_main_dispatch_cbo_directed_sequence main_seq;

    if (memblock_sync_pkg::get_control_worker_topology_mode() !=
        memblock_sync_pkg::MEMBLOCK_CONTROL_TOPOLOGY_DISABLED) begin
        `uvm_fatal(get_type_name(),
                   "directed CBO VSEQ requires MEMBLOCK_CONTROL_WORKER_TOPOLOGY_MODE=0")
    end
    `uvm_do_on(main_seq, p_sequencer)
endtask:start_main_dispatch_sequence

`endif
