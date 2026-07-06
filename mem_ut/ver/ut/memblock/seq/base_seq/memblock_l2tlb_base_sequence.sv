//=========================================================
//File name    : memblock_l2tlb_base_sequence.sv
//Author       : OpenAI_Codex
//Module name  : memblock_l2tlb_base_sequence
//Discribution : L2TLB/PTW responder sequence backed by by-key TLB table
//Date         : 2026-05-18
//=========================================================
`ifndef MEMBLOCK_L2TLB_BASE_SEQUENCE__SV
`define MEMBLOCK_L2TLB_BASE_SEQUENCE__SV

class memblock_l2tlb_base_sequence extends L2tlb_agent_agent_default_sequence;

    common_data_transaction data;
    virtual L2tlb_agent_agent_interface l2tlb_vif;
    dispatch_monitor_event_adapter monitor_adapter;

    bit          enable;
    int unsigned min_latency;
    int unsigned max_latency;
    int unsigned idle_stop_cycle;

    `uvm_object_utils(memblock_l2tlb_base_sequence)

    extern function new(string name = "memblock_l2tlb_base_sequence");
    extern virtual task pre_body();
    extern virtual task body();
    extern virtual task drive_l2tlb_loop();
    extern virtual task send_l2tlb_cycle(input int unsigned send_count,
                                         output bit has_progress);
    extern virtual task send_l2tlb_item(input L2tlb_agent_agent_xaction tr);
    extern function void configure_from_plus();
    extern function void ensure_context();
    extern function void drain_csr_runtime_events();
    extern function bit request_fire();
    extern function bit request_valid();
    extern function L2tlb_agent_agent_xaction create_l2tlb_xaction(input string name);
    extern function void clear_l2tlb_xaction(input L2tlb_agent_agent_xaction tr);
    extern function void sample_request_fields(output bit [37:0] vpn,
                                               output bit [1:0] s2xlate);
    extern function void fill_dtlb_resp_from_entry(input memblock_tlb_entry entry,
                                                   ref L2tlb_agent_agent_xaction resp);
    extern function int unsigned choose_latency();

endclass:memblock_l2tlb_base_sequence

function memblock_l2tlb_base_sequence::new(string name = "memblock_l2tlb_base_sequence");
    super.new(name);
    enable = 1'b0;
    min_latency = 1;
    max_latency = 4;
    idle_stop_cycle = 5000;
endfunction:new

task memblock_l2tlb_base_sequence::pre_body();
    super.pre_body();
endtask:pre_body

task memblock_l2tlb_base_sequence::body();
    seq_csr_common::init();
    configure_from_plus();
    if (!enable) begin
        return;
    end
    ensure_context();
    if (!memblock_sync_pkg::l2tlb_responder_active) begin
        `uvm_fatal(get_type_name(),
                   "MEMBLOCK_L2TLB_SEQ_EN is enabled but L2TLB connect takeover is not active; enable compile macro MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN")
    end
    `uvm_info(get_type_name(),
              $sformatf("L2TLB responder loop start: idle_stop_cycle=%0d latency=%0d..%0d",
                        idle_stop_cycle, min_latency, max_latency),
              UVM_LOW)
    drive_l2tlb_loop();
endtask:body

task memblock_l2tlb_base_sequence::drive_l2tlb_loop();
    int unsigned idle_count;
    int unsigned send_count;

    idle_count = 0;
    send_count = 0;
    forever begin
        bit has_progress;

        @(l2tlb_vif.drv_cb);
        send_l2tlb_cycle(send_count, has_progress);
        if (has_progress) begin
            send_count++;
            idle_count = 0;
        end else begin
            idle_count++;
            if (idle_count > idle_stop_cycle) begin
                `uvm_info(get_type_name(),
                          $sformatf("stop after %0d idle L2TLB cycles", idle_count),
                          UVM_LOW)
                break;
            end
        end
    end
endtask:drive_l2tlb_loop

task memblock_l2tlb_base_sequence::send_l2tlb_cycle(input int unsigned send_count,
                                                    output bit has_progress);
    L2tlb_agent_agent_xaction ready_tr;
    L2tlb_agent_agent_xaction resp_tr;
    bit [37:0]                vpn;
    bit [1:0]                 s2xlate;
    memblock_tlb_lookup_key_t key;
    memblock_tlb_entry        entry;
    bit                       created;
    int unsigned              record_update_count;
    int unsigned              latency;

    has_progress = 1'b0;
    if (request_valid()) begin
        sample_request_fields(vpn, s2xlate);
        `uvm_info(get_type_name(),
                  $sformatf("accept L2TLB request send_count=%0d vpn=0x%0h s2xlate=%0d",
                            send_count, vpn, s2xlate),
                  UVM_LOW)
        ready_tr = create_l2tlb_xaction($sformatf("l2tlb_ready_tr_send_%0d", send_count));
        ready_tr.io_ptw_req_0_ready = 1'b1;
        send_l2tlb_item(ready_tr);
        latency = choose_latency();
        resp_tr = create_l2tlb_xaction($sformatf("l2tlb_resp_tr_send_%0d", send_count));
        resp_tr.pre_pkt_gap = latency;
        resp_tr.io_ptw_req_0_valid = 1'b1;
        resp_tr.io_ptw_req_0_bits_vpn = vpn;
        resp_tr.io_ptw_req_0_bits_s2xlate = s2xlate;
        drain_csr_runtime_events();
        if (data.get_or_create_tlb_entry_by_req(vpn, s2xlate, key, entry, created)) begin
            fill_dtlb_resp_from_entry(entry, resp_tr);
            record_update_count = data.update_uid_tlb_records_by_entry(key, entry);
            `uvm_info(get_type_name(),
                      $sformatf("respond L2TLB request send_count=%0d vpn=0x%0h key_vpn=0x%0h created=%0d paddr_ppn=0x%0h uid_records=%0d",
                                send_count, vpn, key.vpn, created, entry.ppn, record_update_count),
                      UVM_LOW)
        end else begin
            `uvm_fatal(get_type_name(),
                       $sformatf("L2TLB entry miss for vpn=0x%0h s2xlate=%0d", vpn, s2xlate))
        end
        send_l2tlb_item(resp_tr);
        has_progress = 1'b1;
    end
endtask:send_l2tlb_cycle

task memblock_l2tlb_base_sequence::send_l2tlb_item(input L2tlb_agent_agent_xaction tr);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "send_l2tlb_item got null xaction")
    end
    start_item(tr);
    finish_item(tr);
endtask:send_l2tlb_item

function void memblock_l2tlb_base_sequence::configure_from_plus();
    enable = seq_csr_common::get_l2tlb_seq_en();
    min_latency = seq_csr_common::get_l2tlb_min_latency();
    max_latency = seq_csr_common::get_l2tlb_max_latency();
    idle_stop_cycle = seq_csr_common::get_l2tlb_idle_stop_cycle();
endfunction:configure_from_plus

function void memblock_l2tlb_base_sequence::ensure_context();
    data = common_data_transaction::get();
    if (monitor_adapter == null) begin
        monitor_adapter = dispatch_monitor_event_adapter::type_id::create("monitor_adapter");
    end
    if (data == null) begin
        `uvm_fatal(get_type_name(), "failed to get common_data_transaction")
    end
    if (monitor_adapter == null) begin
        `uvm_fatal(get_type_name(), "failed to create dispatch_monitor_event_adapter")
    end
    if (!uvm_config_db#(virtual L2tlb_agent_agent_interface)::get(null, get_full_name(), "vif", l2tlb_vif) &&
        !uvm_config_db#(virtual L2tlb_agent_agent_interface)::get(null, "uvm_test_top.env.u_L2tlb_agent_agent*", "vif", l2tlb_vif)) begin
        `uvm_fatal(get_type_name(), "L2TLB virtual interface is not set")
    end
endfunction:ensure_context

function void memblock_l2tlb_base_sequence::drain_csr_runtime_events();
    ensure_context();
    monitor_adapter.drain_csr_events();
endfunction:drain_csr_runtime_events

function bit memblock_l2tlb_base_sequence::request_fire();
    if (l2tlb_vif == null) begin
        return 1'b0;
    end
    return l2tlb_vif.rst_n === 1'b1 &&
           memblock_sync_pkg::reset_backend_done === 1'b1 &&
           l2tlb_vif.io_ptw_req_0_valid === 1'b1 &&
           l2tlb_vif.io_ptw_req_0_ready === 1'b1;
endfunction:request_fire

function bit memblock_l2tlb_base_sequence::request_valid();
    if (l2tlb_vif == null) begin
        return 1'b0;
    end
    return l2tlb_vif.rst_n === 1'b1 &&
           memblock_sync_pkg::reset_backend_done === 1'b1 &&
           l2tlb_vif.io_ptw_req_0_valid === 1'b1;
endfunction:request_valid

function L2tlb_agent_agent_xaction memblock_l2tlb_base_sequence::create_l2tlb_xaction(input string name);
    L2tlb_agent_agent_xaction tr;

    tr = L2tlb_agent_agent_xaction::type_id::create(name);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "failed to create L2TLB xaction")
    end
    clear_l2tlb_xaction(tr);
    return tr;
endfunction:create_l2tlb_xaction

function void memblock_l2tlb_base_sequence::clear_l2tlb_xaction(input L2tlb_agent_agent_xaction tr);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "clear_l2tlb_xaction got null xaction")
    end
    tr.io_ptw_req_0_ready = 1'b0;
    tr.io_ptw_req_0_valid = 1'b0;
    tr.io_ptw_req_0_bits_vpn = '0;
    tr.io_ptw_req_0_bits_s2xlate = '0;
    tr.io_ptw_resp_valid = 1'b0;
    tr.io_ptw_resp_bits_s2xlate = '0;
    tr.io_ptw_resp_bits_s1_entry_tag = '0;
    tr.io_ptw_resp_bits_s1_entry_asid = '0;
    tr.io_ptw_resp_bits_s1_entry_vmid = '0;
    tr.io_ptw_resp_bits_s1_entry_n = '0;
    tr.io_ptw_resp_bits_s1_entry_pbmt = '0;
    tr.io_ptw_resp_bits_s1_entry_perm_d = '0;
    tr.io_ptw_resp_bits_s1_entry_perm_a = '0;
    tr.io_ptw_resp_bits_s1_entry_perm_g = '0;
    tr.io_ptw_resp_bits_s1_entry_perm_u = '0;
    tr.io_ptw_resp_bits_s1_entry_perm_x = '0;
    tr.io_ptw_resp_bits_s1_entry_perm_w = '0;
    tr.io_ptw_resp_bits_s1_entry_perm_r = '0;
    tr.io_ptw_resp_bits_s1_entry_level = '0;
    tr.io_ptw_resp_bits_s1_entry_v = '0;
    tr.io_ptw_resp_bits_s1_entry_ppn = '0;
    tr.io_ptw_resp_bits_s1_addr_low = '0;
    tr.io_ptw_resp_bits_s1_ppn_low_0 = '0;
    tr.io_ptw_resp_bits_s1_ppn_low_1 = '0;
    tr.io_ptw_resp_bits_s1_ppn_low_2 = '0;
    tr.io_ptw_resp_bits_s1_ppn_low_3 = '0;
    tr.io_ptw_resp_bits_s1_ppn_low_4 = '0;
    tr.io_ptw_resp_bits_s1_ppn_low_5 = '0;
    tr.io_ptw_resp_bits_s1_ppn_low_6 = '0;
    tr.io_ptw_resp_bits_s1_ppn_low_7 = '0;
    tr.io_ptw_resp_bits_s1_valididx_0 = '0;
    tr.io_ptw_resp_bits_s1_valididx_1 = '0;
    tr.io_ptw_resp_bits_s1_valididx_2 = '0;
    tr.io_ptw_resp_bits_s1_valididx_3 = '0;
    tr.io_ptw_resp_bits_s1_valididx_4 = '0;
    tr.io_ptw_resp_bits_s1_valididx_5 = '0;
    tr.io_ptw_resp_bits_s1_valididx_6 = '0;
    tr.io_ptw_resp_bits_s1_valididx_7 = '0;
    tr.io_ptw_resp_bits_s1_pteidx_0 = '0;
    tr.io_ptw_resp_bits_s1_pteidx_1 = '0;
    tr.io_ptw_resp_bits_s1_pteidx_2 = '0;
    tr.io_ptw_resp_bits_s1_pteidx_3 = '0;
    tr.io_ptw_resp_bits_s1_pteidx_4 = '0;
    tr.io_ptw_resp_bits_s1_pteidx_5 = '0;
    tr.io_ptw_resp_bits_s1_pteidx_6 = '0;
    tr.io_ptw_resp_bits_s1_pteidx_7 = '0;
    tr.io_ptw_resp_bits_s1_pf = '0;
    tr.io_ptw_resp_bits_s1_af = '0;
    tr.io_ptw_resp_bits_s2_entry_tag = '0;
    tr.io_ptw_resp_bits_s2_entry_vmid = '0;
    tr.io_ptw_resp_bits_s2_entry_n = '0;
    tr.io_ptw_resp_bits_s2_entry_pbmt = '0;
    tr.io_ptw_resp_bits_s2_entry_ppn = '0;
    tr.io_ptw_resp_bits_s2_entry_perm_d = '0;
    tr.io_ptw_resp_bits_s2_entry_perm_a = '0;
    tr.io_ptw_resp_bits_s2_entry_perm_x = '0;
    tr.io_ptw_resp_bits_s2_entry_perm_w = '0;
    tr.io_ptw_resp_bits_s2_entry_perm_r = '0;
    tr.io_ptw_resp_bits_s2_entry_level = '0;
    tr.io_ptw_resp_bits_s2_gpf = '0;
    tr.io_ptw_resp_bits_s2_gaf = '0;
    tr.pre_pkt_gap = 0;
    tr.post_pkt_gap = 0;
endfunction:clear_l2tlb_xaction

function void memblock_l2tlb_base_sequence::sample_request_fields(output bit [37:0] vpn,
                                                                  output bit [1:0] s2xlate);
    vpn = l2tlb_vif.io_ptw_req_0_bits_vpn;
    s2xlate = l2tlb_vif.io_ptw_req_0_bits_s2xlate;
endfunction:sample_request_fields

function void memblock_l2tlb_base_sequence::fill_dtlb_resp_from_entry(input memblock_tlb_entry entry,
                                                                      ref L2tlb_agent_agent_xaction resp);
    if (resp == null || entry == null) begin
        `uvm_fatal(get_type_name(), "fill_dtlb_resp_from_entry got null input")
    end

    resp.io_ptw_resp_valid = 1'b1;
    resp.io_ptw_resp_bits_s2xlate = entry.s2xlate;
    resp.io_ptw_resp_bits_s1_entry_tag = entry.lookup_key.vpn[34:0];
    resp.io_ptw_resp_bits_s1_entry_asid = entry.asid[15:0];
    resp.io_ptw_resp_bits_s1_entry_vmid = entry.vmid[13:0];
    resp.io_ptw_resp_bits_s1_entry_n = entry.pte_n;
    resp.io_ptw_resp_bits_s1_entry_pbmt = entry.pbmt;
    resp.io_ptw_resp_bits_s1_entry_perm_d = entry.pte_d;
    resp.io_ptw_resp_bits_s1_entry_perm_a = entry.pte_a;
    resp.io_ptw_resp_bits_s1_entry_perm_g = entry.pte_g;
    resp.io_ptw_resp_bits_s1_entry_perm_u = entry.pte_u;
    resp.io_ptw_resp_bits_s1_entry_perm_x = entry.pte_x;
    resp.io_ptw_resp_bits_s1_entry_perm_w = entry.pte_w;
    resp.io_ptw_resp_bits_s1_entry_perm_r = entry.pte_r;
    resp.io_ptw_resp_bits_s1_entry_level = entry.level;
    resp.io_ptw_resp_bits_s1_entry_v = entry.pte_v;
    resp.io_ptw_resp_bits_s1_entry_ppn = entry.ppn[40:0];
    resp.io_ptw_resp_bits_s1_addr_low = entry.addr_low;
    resp.io_ptw_resp_bits_s1_ppn_low_0 = entry.ppn_low[0];
    resp.io_ptw_resp_bits_s1_ppn_low_1 = entry.ppn_low[1];
    resp.io_ptw_resp_bits_s1_ppn_low_2 = entry.ppn_low[2];
    resp.io_ptw_resp_bits_s1_ppn_low_3 = entry.ppn_low[3];
    resp.io_ptw_resp_bits_s1_ppn_low_4 = entry.ppn_low[4];
    resp.io_ptw_resp_bits_s1_ppn_low_5 = entry.ppn_low[5];
    resp.io_ptw_resp_bits_s1_ppn_low_6 = entry.ppn_low[6];
    resp.io_ptw_resp_bits_s1_ppn_low_7 = entry.ppn_low[7];
    resp.io_ptw_resp_bits_s1_valididx_0 = entry.valididx[0];
    resp.io_ptw_resp_bits_s1_valididx_1 = entry.valididx[1];
    resp.io_ptw_resp_bits_s1_valididx_2 = entry.valididx[2];
    resp.io_ptw_resp_bits_s1_valididx_3 = entry.valididx[3];
    resp.io_ptw_resp_bits_s1_valididx_4 = entry.valididx[4];
    resp.io_ptw_resp_bits_s1_valididx_5 = entry.valididx[5];
    resp.io_ptw_resp_bits_s1_valididx_6 = entry.valididx[6];
    resp.io_ptw_resp_bits_s1_valididx_7 = entry.valididx[7];
    resp.io_ptw_resp_bits_s1_pteidx_0 = (entry.pteidx[0] != 0);
    resp.io_ptw_resp_bits_s1_pteidx_1 = (entry.pteidx[1] != 0);
    resp.io_ptw_resp_bits_s1_pteidx_2 = (entry.pteidx[2] != 0);
    resp.io_ptw_resp_bits_s1_pteidx_3 = (entry.pteidx[3] != 0);
    resp.io_ptw_resp_bits_s1_pteidx_4 = (entry.pteidx[4] != 0);
    resp.io_ptw_resp_bits_s1_pteidx_5 = (entry.pteidx[5] != 0);
    resp.io_ptw_resp_bits_s1_pteidx_6 = (entry.pteidx[6] != 0);
    resp.io_ptw_resp_bits_s1_pteidx_7 = (entry.pteidx[7] != 0);
    resp.io_ptw_resp_bits_s1_pf = entry.tlbPF;
    resp.io_ptw_resp_bits_s1_af = entry.tlbAF || entry.pmaAF;
    resp.io_ptw_resp_bits_s2_entry_tag = entry.lookup_key.vpn[37:0];
    resp.io_ptw_resp_bits_s2_entry_vmid = entry.vmid[13:0];
    resp.io_ptw_resp_bits_s2_entry_n = entry.pte_n;
    resp.io_ptw_resp_bits_s2_entry_pbmt = entry.pbmt;
    resp.io_ptw_resp_bits_s2_entry_ppn = entry.ppn[37:0];
    resp.io_ptw_resp_bits_s2_entry_perm_d = entry.pte_d;
    resp.io_ptw_resp_bits_s2_entry_perm_a = entry.pte_a;
    resp.io_ptw_resp_bits_s2_entry_perm_x = entry.pte_x;
    resp.io_ptw_resp_bits_s2_entry_perm_w = entry.pte_w;
    resp.io_ptw_resp_bits_s2_entry_perm_r = entry.pte_r;
    resp.io_ptw_resp_bits_s2_entry_level = entry.level;
    resp.io_ptw_resp_bits_s2_gpf = entry.tlbGPF;
    resp.io_ptw_resp_bits_s2_gaf = 1'b0;
endfunction:fill_dtlb_resp_from_entry

function int unsigned memblock_l2tlb_base_sequence::choose_latency();
    if (max_latency <= min_latency) begin
        return min_latency;
    end
    return $urandom_range(max_latency, min_latency);
endfunction:choose_latency

`endif
