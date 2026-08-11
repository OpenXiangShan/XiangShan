//=========================================================
//File name    : L2tlb_agent_agent_monitor.sv
//Author       : OpenAI_Codex
//Module name  : L2tlb_agent_agent_monitor
//Discribution : L2tlb_agent_agent_monitor : monitor
//Date         : 2026-04-12
//=========================================================
`ifndef L2TLB_AGENT_AGENT_MONITOR__SV
`define L2TLB_AGENT_AGENT_MONITOR__SV

class L2tlb_agent_agent_monitor  extends tcnt_monitor_base#(virtual L2tlb_agent_agent_interface,L2tlb_agent_agent_cfg,L2tlb_agent_agent_xaction);

    `uvm_component_utils(L2tlb_agent_agent_monitor)

    uvm_analysis_imp #(L2tlb_agent_agent_transport_sample,
                       L2tlb_agent_agent_monitor) transport_sample_imp;
    local longint unsigned last_transport_sample_seq;
    local longint unsigned monitor_active_epoch;
    local longint unsigned monitor_reset_pending_epoch;
    local longint unsigned monitor_reset_sample_processed_epoch;
    local longint unsigned monitor_reset_sample_processed_transport_sample_seq;
    local longint unsigned monitor_reset_ack_floor_transport_sample_seq;
    local longint unsigned monitor_last_reset_ack_transport_sample_seq;
    local longint unsigned monitor_reset_ack_epoch;
    local longint unsigned monitor_reset_ack_transport_sample_seq;
    local bit monitor_processing;
    local int unsigned reset_sample_count;

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern task run_phase(uvm_phase phase);
    extern virtual function void write(L2tlb_agent_agent_transport_sample sample);
    extern virtual function void write_transport_sample(
        L2tlb_agent_agent_transport_sample sample);
    extern virtual function void begin_reset_epoch(
        input longint unsigned reset_epoch);
    extern virtual function bit reset_ackable(
        input longint unsigned reset_epoch,
        input longint unsigned reset_sample_transport_seq);
endclass:L2tlb_agent_agent_monitor

function L2tlb_agent_agent_monitor::new(string name, uvm_component parent);
    super.new(name,parent);
    last_transport_sample_seq = 0;
    monitor_active_epoch = 0;
    monitor_reset_pending_epoch = 0;
    monitor_reset_sample_processed_epoch = 0;
    monitor_reset_sample_processed_transport_sample_seq = 0;
    monitor_reset_ack_floor_transport_sample_seq = 0;
    monitor_last_reset_ack_transport_sample_seq = 0;
    monitor_reset_ack_epoch = 0;
    monitor_reset_ack_transport_sample_seq = 0;
    monitor_processing = 1'b0;
    reset_sample_count = 0;
endfunction:new

function void L2tlb_agent_agent_monitor::build_phase(uvm_phase phase);
    super.build_phase(phase);
    transport_sample_imp = new("transport_sample_imp", this);
endfunction:build_phase

task L2tlb_agent_agent_monitor::run_phase(uvm_phase phase);
    super.run_phase(phase);
    // Transport sampling is owned by the driver.  Keeping the old mon_cb
    // loop alive would create a second, potentially different observation of
    // request fire and response timing.
endtask:run_phase

function void L2tlb_agent_agent_monitor::write(
    L2tlb_agent_agent_transport_sample sample);
    write_transport_sample(sample);
endfunction:write

function void L2tlb_agent_agent_monitor::write_transport_sample(
    L2tlb_agent_agent_transport_sample sample);
    memblock_l2tlb_drv_sample_t payload;

    if (sample == null || !sample.get_payload(payload)) begin
        `uvm_fatal(get_type_name(), "received invalid frozen L2TLB transport sample")
    end
    if (payload.transport_sample_seq == 0 ||
        payload.transport_sample_seq != last_transport_sample_seq + 1) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("transport sample sequence is not consecutive old=%0d new=%0d",
                             last_transport_sample_seq,
                             payload.transport_sample_seq))
    end
    last_transport_sample_seq = payload.transport_sample_seq;
    monitor_processing = 1'b1;

    if (payload.sampled_reset_active) begin
        // A physical reset can be observed before the CSR coordinator has
        // published epoch 1.  Such a sample is diagnostic-only; it cannot
        // acknowledge a runtime epoch with tuple {epoch=0, sample_seq}.
        if (payload.sampled_reset_epoch == 0) begin
            monitor_processing = 1'b0;
            return;
        end
        if (monitor_active_epoch > payload.sampled_reset_epoch) begin
            // Old reset samples are harmless after a newer epoch is active.
            monitor_processing = 1'b0;
            return;
        end
        if (monitor_active_epoch < payload.sampled_reset_epoch) begin
            begin_reset_epoch(payload.sampled_reset_epoch);
        end
        // After this epoch has produced its exact reset tuple, remaining
        // reset-active samples are quiescent analysis-only observations.
        if (monitor_reset_pending_epoch == payload.sampled_reset_epoch) begin
            reset_sample_count++;
            if (reset_sample_count >
                `MEMBLOCK_L2TLB_RESET_WATCHDOG_MAX_SAMPLES) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("L2TLB monitor reset ack watchdog expired epoch=%0d sample=%0d count=%0d pending=%0d processed=%0d/%0d floor=%0d ack=%0d/%0d",
                                     payload.sampled_reset_epoch,
                                     payload.transport_sample_seq,
                                     reset_sample_count,
                                     monitor_reset_pending_epoch,
                                     monitor_reset_sample_processed_epoch,
                                     monitor_reset_sample_processed_transport_sample_seq,
                                     monitor_reset_ack_floor_transport_sample_seq,
                                     monitor_reset_ack_epoch,
                                     monitor_reset_ack_transport_sample_seq))
            end
        end
        monitor_reset_sample_processed_epoch = payload.sampled_reset_epoch;
        monitor_reset_sample_processed_transport_sample_seq =
            payload.transport_sample_seq;
        monitor_processing = 1'b0;
        if (reset_ackable(payload.sampled_reset_epoch,
                          payload.transport_sample_seq)) begin
            memblock_sync_pkg::acknowledge_l2tlb_monitor_reset(
                payload.sampled_reset_epoch,
                payload.transport_sample_seq,
                monitor_reset_sample_processed_epoch,
                monitor_reset_sample_processed_transport_sample_seq,
                get_full_name());
            monitor_reset_ack_epoch = payload.sampled_reset_epoch;
            monitor_reset_ack_transport_sample_seq =
                payload.transport_sample_seq;
            monitor_last_reset_ack_transport_sample_seq =
                payload.transport_sample_seq;
            monitor_reset_pending_epoch = 0;
        end
        return;
    end

    if (monitor_active_epoch != payload.sampled_reset_epoch &&
        monitor_active_epoch != 0) begin
        if (payload.sampled_reset_epoch > monitor_active_epoch) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("future non-reset L2TLB transport sample epoch=%0d active=%0d sample=%0d",
                                 payload.sampled_reset_epoch,
                                 monitor_active_epoch,
                                 payload.transport_sample_seq))
        end
        // A normal sample from an old epoch cannot be used as a reset ack or
        // final proof.  Keep transport sequence continuity and drop it.
        monitor_processing = 1'b0;
        return;
    end
    reset_sample_count = 0;
    if (!payload.sampled_reset_active &&
        $isunknown({payload.sampled_req_valid,
                    payload.sampled_req_ready,
                    payload.sampled_resp_valid})) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("L2TLB transport handshake contains X/Z sample=%0d valid=%b ready=%b resp_valid=%b",
                             payload.transport_sample_seq,
                             payload.sampled_req_valid,
                             payload.sampled_req_ready,
                             payload.sampled_resp_valid))
    end
    if (memblock_sync_pkg::l2tlb_testcase_lifecycle_initialized &&
        !memblock_sync_pkg::l2tlb_responder_enabled() &&
        payload.sampled_req_valid === 1'b1) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("NO_OWNER observed L2TLB request transport=%0d dut_sample=%0d vpn=0x%0h s2xlate=0x%0h topology=%s",
                             payload.transport_sample_seq,
                             payload.dut_sample_seq,
                             payload.sampled_req_vpn,
                             payload.sampled_req_s2xlate,
                             memblock_sync_pkg::l2tlb_testcase_topology_name))
    end
    if (payload.sampled_final_inactive_proof_valid) begin
        if (payload.sampled_final_inactive_proof_epoch !=
                payload.sampled_reset_epoch ||
            payload.sampled_final_inactive_proof_transport_sample_seq !=
                payload.transport_sample_seq) begin
            `uvm_fatal(get_type_name(), "invalid frozen final inactive proof")
        end
        memblock_sync_pkg::mark_l2tlb_monitor_final_sample_settled(
            payload.sampled_final_inactive_proof_epoch,
            payload.sampled_final_inactive_proof_transport_sample_seq);
    end
    monitor_processing = 1'b0;
endfunction:write_transport_sample

// Abstract responsibility: arm this monitor for one reset epoch and retain
// the transport sequence floor that makes an old reset sample ineligible.
function void L2tlb_agent_agent_monitor::begin_reset_epoch(
    input longint unsigned reset_epoch);
    if (reset_epoch == 0) begin
        `uvm_fatal(get_type_name(), "cannot arm L2TLB monitor reset epoch zero")
    end
    monitor_active_epoch = reset_epoch;
    monitor_reset_pending_epoch = reset_epoch;
    monitor_reset_ack_floor_transport_sample_seq =
        monitor_last_reset_ack_transport_sample_seq;
    monitor_reset_sample_processed_epoch = 0;
    monitor_reset_sample_processed_transport_sample_seq = 0;
    reset_sample_count = 0;
    memblock_sync_pkg::reset_l2tlb_monitor_runtime_state(reset_epoch);
endfunction:begin_reset_epoch

// Abstract responsibility: decide whether the exact frozen reset sample can
// close the monitor's reset obligation; it never mutates package state.
function bit L2tlb_agent_agent_monitor::reset_ackable(
    input longint unsigned reset_epoch,
    input longint unsigned reset_sample_transport_seq);
    return !monitor_processing &&
           monitor_reset_pending_epoch == reset_epoch &&
           monitor_reset_sample_processed_epoch == reset_epoch &&
           monitor_reset_sample_processed_transport_sample_seq ==
               reset_sample_transport_seq &&
           reset_sample_transport_seq >
               monitor_reset_ack_floor_transport_sample_seq &&
           reset_sample_transport_seq > monitor_last_reset_ack_transport_sample_seq;
endfunction:reset_ackable

/*
task L2tlb_agent_agent_monitor::mon_data();

    logic io_ptw_req_0_ready           ;
    logic io_ptw_req_0_valid           ;
    logic [37:0] io_ptw_req_0_bits_vpn ;
    logic [1:0] io_ptw_req_0_bits_s2xlate;
    logic io_ptw_resp_valid            ;
    logic [1:0] io_ptw_resp_bits_s2xlate;
    logic [34:0] io_ptw_resp_bits_s1_entry_tag;
    logic [15:0] io_ptw_resp_bits_s1_entry_asid;
    logic [13:0] io_ptw_resp_bits_s1_entry_vmid;
    logic io_ptw_resp_bits_s1_entry_n  ;
    logic [1:0] io_ptw_resp_bits_s1_entry_pbmt;
    logic io_ptw_resp_bits_s1_entry_perm_d;
    logic io_ptw_resp_bits_s1_entry_perm_a;
    logic io_ptw_resp_bits_s1_entry_perm_g;
    logic io_ptw_resp_bits_s1_entry_perm_u;
    logic io_ptw_resp_bits_s1_entry_perm_x;
    logic io_ptw_resp_bits_s1_entry_perm_w;
    logic io_ptw_resp_bits_s1_entry_perm_r;
    logic [1:0] io_ptw_resp_bits_s1_entry_level;
    logic io_ptw_resp_bits_s1_entry_v  ;
    logic [40:0] io_ptw_resp_bits_s1_entry_ppn;
    logic [2:0] io_ptw_resp_bits_s1_addr_low;
    logic [2:0] io_ptw_resp_bits_s1_ppn_low_0;
    logic [2:0] io_ptw_resp_bits_s1_ppn_low_1;
    logic [2:0] io_ptw_resp_bits_s1_ppn_low_2;
    logic [2:0] io_ptw_resp_bits_s1_ppn_low_3;
    logic [2:0] io_ptw_resp_bits_s1_ppn_low_4;
    logic [2:0] io_ptw_resp_bits_s1_ppn_low_5;
    logic [2:0] io_ptw_resp_bits_s1_ppn_low_6;
    logic [2:0] io_ptw_resp_bits_s1_ppn_low_7;
    logic io_ptw_resp_bits_s1_valididx_0;
    logic io_ptw_resp_bits_s1_valididx_1;
    logic io_ptw_resp_bits_s1_valididx_2;
    logic io_ptw_resp_bits_s1_valididx_3;
    logic io_ptw_resp_bits_s1_valididx_4;
    logic io_ptw_resp_bits_s1_valididx_5;
    logic io_ptw_resp_bits_s1_valididx_6;
    logic io_ptw_resp_bits_s1_valididx_7;
    logic io_ptw_resp_bits_s1_pteidx_0 ;
    logic io_ptw_resp_bits_s1_pteidx_1 ;
    logic io_ptw_resp_bits_s1_pteidx_2 ;
    logic io_ptw_resp_bits_s1_pteidx_3 ;
    logic io_ptw_resp_bits_s1_pteidx_4 ;
    logic io_ptw_resp_bits_s1_pteidx_5 ;
    logic io_ptw_resp_bits_s1_pteidx_6 ;
    logic io_ptw_resp_bits_s1_pteidx_7 ;
    logic io_ptw_resp_bits_s1_pf       ;
    logic io_ptw_resp_bits_s1_af       ;
    logic [37:0] io_ptw_resp_bits_s2_entry_tag;
    logic [13:0] io_ptw_resp_bits_s2_entry_vmid;
    logic io_ptw_resp_bits_s2_entry_n  ;
    logic [1:0] io_ptw_resp_bits_s2_entry_pbmt;
    logic [37:0] io_ptw_resp_bits_s2_entry_ppn;
    logic io_ptw_resp_bits_s2_entry_perm_d;
    logic io_ptw_resp_bits_s2_entry_perm_a;
    logic io_ptw_resp_bits_s2_entry_perm_g;
    logic io_ptw_resp_bits_s2_entry_perm_u;
    logic io_ptw_resp_bits_s2_entry_perm_x;
    logic io_ptw_resp_bits_s2_entry_perm_w;
    logic io_ptw_resp_bits_s2_entry_perm_r;
    logic [1:0] io_ptw_resp_bits_s2_entry_level;
    logic io_ptw_resp_bits_s2_gpf      ;
    logic io_ptw_resp_bits_s2_gaf      ;

    L2tlb_agent_agent_xaction  mon_tr;
    while(1) begin
        @this.vif.mon_mp.mon_cb;
        io_ptw_req_0_ready = this.vif.mon_mp.mon_cb.io_ptw_req_0_ready;
        io_ptw_req_0_valid = this.vif.mon_mp.mon_cb.io_ptw_req_0_valid;
        io_ptw_req_0_bits_vpn = this.vif.mon_mp.mon_cb.io_ptw_req_0_bits_vpn;
        io_ptw_req_0_bits_s2xlate = this.vif.mon_mp.mon_cb.io_ptw_req_0_bits_s2xlate;
        io_ptw_resp_valid = this.vif.mon_mp.mon_cb.io_ptw_resp_valid;
        io_ptw_resp_bits_s2xlate = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s2xlate;
        io_ptw_resp_bits_s1_entry_tag = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_entry_tag;
        io_ptw_resp_bits_s1_entry_asid = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_entry_asid;
        io_ptw_resp_bits_s1_entry_vmid = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_entry_vmid;
        io_ptw_resp_bits_s1_entry_n = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_entry_n;
        io_ptw_resp_bits_s1_entry_pbmt = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_entry_pbmt;
        io_ptw_resp_bits_s1_entry_perm_d = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_entry_perm_d;
        io_ptw_resp_bits_s1_entry_perm_a = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_entry_perm_a;
        io_ptw_resp_bits_s1_entry_perm_g = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_entry_perm_g;
        io_ptw_resp_bits_s1_entry_perm_u = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_entry_perm_u;
        io_ptw_resp_bits_s1_entry_perm_x = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_entry_perm_x;
        io_ptw_resp_bits_s1_entry_perm_w = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_entry_perm_w;
        io_ptw_resp_bits_s1_entry_perm_r = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_entry_perm_r;
        io_ptw_resp_bits_s1_entry_level = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_entry_level;
        io_ptw_resp_bits_s1_entry_v = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_entry_v;
        io_ptw_resp_bits_s1_entry_ppn = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_entry_ppn;
        io_ptw_resp_bits_s1_addr_low = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_addr_low;
        io_ptw_resp_bits_s1_ppn_low_0 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_ppn_low_0;
        io_ptw_resp_bits_s1_ppn_low_1 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_ppn_low_1;
        io_ptw_resp_bits_s1_ppn_low_2 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_ppn_low_2;
        io_ptw_resp_bits_s1_ppn_low_3 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_ppn_low_3;
        io_ptw_resp_bits_s1_ppn_low_4 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_ppn_low_4;
        io_ptw_resp_bits_s1_ppn_low_5 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_ppn_low_5;
        io_ptw_resp_bits_s1_ppn_low_6 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_ppn_low_6;
        io_ptw_resp_bits_s1_ppn_low_7 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_ppn_low_7;
        io_ptw_resp_bits_s1_valididx_0 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_valididx_0;
        io_ptw_resp_bits_s1_valididx_1 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_valididx_1;
        io_ptw_resp_bits_s1_valididx_2 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_valididx_2;
        io_ptw_resp_bits_s1_valididx_3 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_valididx_3;
        io_ptw_resp_bits_s1_valididx_4 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_valididx_4;
        io_ptw_resp_bits_s1_valididx_5 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_valididx_5;
        io_ptw_resp_bits_s1_valididx_6 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_valididx_6;
        io_ptw_resp_bits_s1_valididx_7 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_valididx_7;
        io_ptw_resp_bits_s1_pteidx_0 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_pteidx_0;
        io_ptw_resp_bits_s1_pteidx_1 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_pteidx_1;
        io_ptw_resp_bits_s1_pteidx_2 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_pteidx_2;
        io_ptw_resp_bits_s1_pteidx_3 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_pteidx_3;
        io_ptw_resp_bits_s1_pteidx_4 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_pteidx_4;
        io_ptw_resp_bits_s1_pteidx_5 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_pteidx_5;
        io_ptw_resp_bits_s1_pteidx_6 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_pteidx_6;
        io_ptw_resp_bits_s1_pteidx_7 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_pteidx_7;
        io_ptw_resp_bits_s1_pf = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_pf;
        io_ptw_resp_bits_s1_af = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_af;
        io_ptw_resp_bits_s2_entry_tag = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s2_entry_tag;
        io_ptw_resp_bits_s2_entry_vmid = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s2_entry_vmid;
        io_ptw_resp_bits_s2_entry_n = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s2_entry_n;
        io_ptw_resp_bits_s2_entry_pbmt = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s2_entry_pbmt;
        io_ptw_resp_bits_s2_entry_ppn = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s2_entry_ppn;
        io_ptw_resp_bits_s2_entry_perm_d = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s2_entry_perm_d;
        io_ptw_resp_bits_s2_entry_perm_a = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s2_entry_perm_a;
        io_ptw_resp_bits_s2_entry_perm_g = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s2_entry_perm_g;
        io_ptw_resp_bits_s2_entry_perm_u = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s2_entry_perm_u;
        io_ptw_resp_bits_s2_entry_perm_x = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s2_entry_perm_x;
        io_ptw_resp_bits_s2_entry_perm_w = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s2_entry_perm_w;
        io_ptw_resp_bits_s2_entry_perm_r = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s2_entry_perm_r;
        io_ptw_resp_bits_s2_entry_level = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s2_entry_level;
        io_ptw_resp_bits_s2_gpf = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s2_gpf;
        io_ptw_resp_bits_s2_gaf = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s2_gaf;

        if(this.cfg.xz_sw==tcnt_dec_base::ON && this.vif.rst_n==1'b1 && memblock_sync_pkg::reset_backend_done==1'b1) begin
            `TCNT_CHECK_SIG_XZ(io_ptw_req_0_ready,io_ptw_req_0_ready,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_req_0_valid,io_ptw_req_0_valid,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_req_0_bits_vpn,io_ptw_req_0_bits_vpn,38);
            `TCNT_CHECK_SIG_XZ(io_ptw_req_0_bits_s2xlate,io_ptw_req_0_bits_s2xlate,2);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_valid,io_ptw_resp_valid,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s2xlate,io_ptw_resp_bits_s2xlate,2);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_entry_tag,io_ptw_resp_bits_s1_entry_tag,35);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_entry_asid,io_ptw_resp_bits_s1_entry_asid,16);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_entry_vmid,io_ptw_resp_bits_s1_entry_vmid,14);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_entry_n,io_ptw_resp_bits_s1_entry_n,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_entry_pbmt,io_ptw_resp_bits_s1_entry_pbmt,2);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_entry_perm_d,io_ptw_resp_bits_s1_entry_perm_d,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_entry_perm_a,io_ptw_resp_bits_s1_entry_perm_a,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_entry_perm_g,io_ptw_resp_bits_s1_entry_perm_g,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_entry_perm_u,io_ptw_resp_bits_s1_entry_perm_u,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_entry_perm_x,io_ptw_resp_bits_s1_entry_perm_x,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_entry_perm_w,io_ptw_resp_bits_s1_entry_perm_w,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_entry_perm_r,io_ptw_resp_bits_s1_entry_perm_r,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_entry_level,io_ptw_resp_bits_s1_entry_level,2);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_entry_v,io_ptw_resp_bits_s1_entry_v,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_entry_ppn,io_ptw_resp_bits_s1_entry_ppn,41);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_addr_low,io_ptw_resp_bits_s1_addr_low,3);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_ppn_low_0,io_ptw_resp_bits_s1_ppn_low_0,3);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_ppn_low_1,io_ptw_resp_bits_s1_ppn_low_1,3);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_ppn_low_2,io_ptw_resp_bits_s1_ppn_low_2,3);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_ppn_low_3,io_ptw_resp_bits_s1_ppn_low_3,3);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_ppn_low_4,io_ptw_resp_bits_s1_ppn_low_4,3);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_ppn_low_5,io_ptw_resp_bits_s1_ppn_low_5,3);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_ppn_low_6,io_ptw_resp_bits_s1_ppn_low_6,3);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_ppn_low_7,io_ptw_resp_bits_s1_ppn_low_7,3);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_valididx_0,io_ptw_resp_bits_s1_valididx_0,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_valididx_1,io_ptw_resp_bits_s1_valididx_1,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_valididx_2,io_ptw_resp_bits_s1_valididx_2,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_valididx_3,io_ptw_resp_bits_s1_valididx_3,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_valididx_4,io_ptw_resp_bits_s1_valididx_4,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_valididx_5,io_ptw_resp_bits_s1_valididx_5,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_valididx_6,io_ptw_resp_bits_s1_valididx_6,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_valididx_7,io_ptw_resp_bits_s1_valididx_7,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_pteidx_0,io_ptw_resp_bits_s1_pteidx_0,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_pteidx_1,io_ptw_resp_bits_s1_pteidx_1,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_pteidx_2,io_ptw_resp_bits_s1_pteidx_2,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_pteidx_3,io_ptw_resp_bits_s1_pteidx_3,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_pteidx_4,io_ptw_resp_bits_s1_pteidx_4,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_pteidx_5,io_ptw_resp_bits_s1_pteidx_5,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_pteidx_6,io_ptw_resp_bits_s1_pteidx_6,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_pteidx_7,io_ptw_resp_bits_s1_pteidx_7,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_pf,io_ptw_resp_bits_s1_pf,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s1_af,io_ptw_resp_bits_s1_af,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s2_entry_tag,io_ptw_resp_bits_s2_entry_tag,38);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s2_entry_vmid,io_ptw_resp_bits_s2_entry_vmid,14);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s2_entry_n,io_ptw_resp_bits_s2_entry_n,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s2_entry_pbmt,io_ptw_resp_bits_s2_entry_pbmt,2);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s2_entry_ppn,io_ptw_resp_bits_s2_entry_ppn,38);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s2_entry_perm_d,io_ptw_resp_bits_s2_entry_perm_d,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s2_entry_perm_a,io_ptw_resp_bits_s2_entry_perm_a,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s2_entry_perm_g,io_ptw_resp_bits_s2_entry_perm_g,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s2_entry_perm_u,io_ptw_resp_bits_s2_entry_perm_u,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s2_entry_perm_x,io_ptw_resp_bits_s2_entry_perm_x,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s2_entry_perm_w,io_ptw_resp_bits_s2_entry_perm_w,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s2_entry_perm_r,io_ptw_resp_bits_s2_entry_perm_r,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s2_entry_level,io_ptw_resp_bits_s2_entry_level,2);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s2_gpf,io_ptw_resp_bits_s2_gpf,1);
            `TCNT_CHECK_SIG_XZ(io_ptw_resp_bits_s2_gaf,io_ptw_resp_bits_s2_gaf,1);

        end
        //if(xxxTODOxxx==1'b1) begin
        //    mon_tr = L2tlb_agent_agent_xaction::type_id::create("mon_tr");
        //    mon_tr.io_ptw_req_0_ready = io_ptw_req_0_ready;
        //    mon_tr.io_ptw_req_0_valid = io_ptw_req_0_valid;
        //    mon_tr.io_ptw_req_0_bits_vpn = io_ptw_req_0_bits_vpn;
        //    mon_tr.io_ptw_req_0_bits_s2xlate = io_ptw_req_0_bits_s2xlate;
        //    mon_tr.io_ptw_resp_valid = io_ptw_resp_valid;
        //    mon_tr.io_ptw_resp_bits_s2xlate = io_ptw_resp_bits_s2xlate;
        //    mon_tr.io_ptw_resp_bits_s1_entry_tag = io_ptw_resp_bits_s1_entry_tag;
        //    mon_tr.io_ptw_resp_bits_s1_entry_asid = io_ptw_resp_bits_s1_entry_asid;
        //    mon_tr.io_ptw_resp_bits_s1_entry_vmid = io_ptw_resp_bits_s1_entry_vmid;
        //    mon_tr.io_ptw_resp_bits_s1_entry_n = io_ptw_resp_bits_s1_entry_n;
        //    mon_tr.io_ptw_resp_bits_s1_entry_pbmt = io_ptw_resp_bits_s1_entry_pbmt;
        //    mon_tr.io_ptw_resp_bits_s1_entry_perm_d = io_ptw_resp_bits_s1_entry_perm_d;
        //    mon_tr.io_ptw_resp_bits_s1_entry_perm_a = io_ptw_resp_bits_s1_entry_perm_a;
        //    mon_tr.io_ptw_resp_bits_s1_entry_perm_g = io_ptw_resp_bits_s1_entry_perm_g;
        //    mon_tr.io_ptw_resp_bits_s1_entry_perm_u = io_ptw_resp_bits_s1_entry_perm_u;
        //    mon_tr.io_ptw_resp_bits_s1_entry_perm_x = io_ptw_resp_bits_s1_entry_perm_x;
        //    mon_tr.io_ptw_resp_bits_s1_entry_perm_w = io_ptw_resp_bits_s1_entry_perm_w;
        //    mon_tr.io_ptw_resp_bits_s1_entry_perm_r = io_ptw_resp_bits_s1_entry_perm_r;
        //    mon_tr.io_ptw_resp_bits_s1_entry_level = io_ptw_resp_bits_s1_entry_level;
        //    mon_tr.io_ptw_resp_bits_s1_entry_v = io_ptw_resp_bits_s1_entry_v;
        //    mon_tr.io_ptw_resp_bits_s1_entry_ppn = io_ptw_resp_bits_s1_entry_ppn;
        //    mon_tr.io_ptw_resp_bits_s1_addr_low = io_ptw_resp_bits_s1_addr_low;
        //    mon_tr.io_ptw_resp_bits_s1_ppn_low_0 = io_ptw_resp_bits_s1_ppn_low_0;
        //    mon_tr.io_ptw_resp_bits_s1_ppn_low_1 = io_ptw_resp_bits_s1_ppn_low_1;
        //    mon_tr.io_ptw_resp_bits_s1_ppn_low_2 = io_ptw_resp_bits_s1_ppn_low_2;
        //    mon_tr.io_ptw_resp_bits_s1_ppn_low_3 = io_ptw_resp_bits_s1_ppn_low_3;
        //    mon_tr.io_ptw_resp_bits_s1_ppn_low_4 = io_ptw_resp_bits_s1_ppn_low_4;
        //    mon_tr.io_ptw_resp_bits_s1_ppn_low_5 = io_ptw_resp_bits_s1_ppn_low_5;
        //    mon_tr.io_ptw_resp_bits_s1_ppn_low_6 = io_ptw_resp_bits_s1_ppn_low_6;
        //    mon_tr.io_ptw_resp_bits_s1_ppn_low_7 = io_ptw_resp_bits_s1_ppn_low_7;
        //    mon_tr.io_ptw_resp_bits_s1_valididx_0 = io_ptw_resp_bits_s1_valididx_0;
        //    mon_tr.io_ptw_resp_bits_s1_valididx_1 = io_ptw_resp_bits_s1_valididx_1;
        //    mon_tr.io_ptw_resp_bits_s1_valididx_2 = io_ptw_resp_bits_s1_valididx_2;
        //    mon_tr.io_ptw_resp_bits_s1_valididx_3 = io_ptw_resp_bits_s1_valididx_3;
        //    mon_tr.io_ptw_resp_bits_s1_valididx_4 = io_ptw_resp_bits_s1_valididx_4;
        //    mon_tr.io_ptw_resp_bits_s1_valididx_5 = io_ptw_resp_bits_s1_valididx_5;
        //    mon_tr.io_ptw_resp_bits_s1_valididx_6 = io_ptw_resp_bits_s1_valididx_6;
        //    mon_tr.io_ptw_resp_bits_s1_valididx_7 = io_ptw_resp_bits_s1_valididx_7;
        //    mon_tr.io_ptw_resp_bits_s1_pteidx_0 = io_ptw_resp_bits_s1_pteidx_0;
        //    mon_tr.io_ptw_resp_bits_s1_pteidx_1 = io_ptw_resp_bits_s1_pteidx_1;
        //    mon_tr.io_ptw_resp_bits_s1_pteidx_2 = io_ptw_resp_bits_s1_pteidx_2;
        //    mon_tr.io_ptw_resp_bits_s1_pteidx_3 = io_ptw_resp_bits_s1_pteidx_3;
        //    mon_tr.io_ptw_resp_bits_s1_pteidx_4 = io_ptw_resp_bits_s1_pteidx_4;
        //    mon_tr.io_ptw_resp_bits_s1_pteidx_5 = io_ptw_resp_bits_s1_pteidx_5;
        //    mon_tr.io_ptw_resp_bits_s1_pteidx_6 = io_ptw_resp_bits_s1_pteidx_6;
        //    mon_tr.io_ptw_resp_bits_s1_pteidx_7 = io_ptw_resp_bits_s1_pteidx_7;
        //    mon_tr.io_ptw_resp_bits_s1_pf = io_ptw_resp_bits_s1_pf;
        //    mon_tr.io_ptw_resp_bits_s1_af = io_ptw_resp_bits_s1_af;
        //    mon_tr.io_ptw_resp_bits_s2_entry_tag = io_ptw_resp_bits_s2_entry_tag;
        //    mon_tr.io_ptw_resp_bits_s2_entry_vmid = io_ptw_resp_bits_s2_entry_vmid;
        //    mon_tr.io_ptw_resp_bits_s2_entry_n = io_ptw_resp_bits_s2_entry_n;
        //    mon_tr.io_ptw_resp_bits_s2_entry_pbmt = io_ptw_resp_bits_s2_entry_pbmt;
        //    mon_tr.io_ptw_resp_bits_s2_entry_ppn = io_ptw_resp_bits_s2_entry_ppn;
        //    mon_tr.io_ptw_resp_bits_s2_entry_perm_d = io_ptw_resp_bits_s2_entry_perm_d;
        //    mon_tr.io_ptw_resp_bits_s2_entry_perm_a = io_ptw_resp_bits_s2_entry_perm_a;
        //    mon_tr.io_ptw_resp_bits_s2_entry_perm_g = io_ptw_resp_bits_s2_entry_perm_g;
        //    mon_tr.io_ptw_resp_bits_s2_entry_perm_u = io_ptw_resp_bits_s2_entry_perm_u;
        //    mon_tr.io_ptw_resp_bits_s2_entry_perm_x = io_ptw_resp_bits_s2_entry_perm_x;
        //    mon_tr.io_ptw_resp_bits_s2_entry_perm_w = io_ptw_resp_bits_s2_entry_perm_w;
        //    mon_tr.io_ptw_resp_bits_s2_entry_perm_r = io_ptw_resp_bits_s2_entry_perm_r;
        //    mon_tr.io_ptw_resp_bits_s2_entry_level = io_ptw_resp_bits_s2_entry_level;
        //    mon_tr.io_ptw_resp_bits_s2_gpf = io_ptw_resp_bits_s2_gpf;
        //    mon_tr.io_ptw_resp_bits_s2_gaf = io_ptw_resp_bits_s2_gaf;

        //    mon_tr.channel_id = this.cfg.channel_id;
        //    mon_tr.unpack();
        //    this.mon_item_port.write(mon_tr);
        //end
    end
endtask:mon_data
*/

`endif
