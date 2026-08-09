//=========================================================
//File name    : L2tlb_agent_agent_driver.sv
//Author       : OpenAI_Codex
//Module name  : L2tlb_agent_agent_driver
//Discribution : L2tlb_agent_agent_driver : driver
//Date         : 2026-04-12
//=========================================================
`ifndef L2TLB_AGENT_AGENT_DRIVER__SV
`define L2TLB_AGENT_AGENT_DRIVER__SV

typedef class L2tlb_agent_agent_sequencer;

class L2tlb_agent_agent_driver  extends tcnt_driver_base#(virtual L2tlb_agent_agent_interface,L2tlb_agent_agent_cfg,L2tlb_agent_agent_xaction);

    `uvm_component_utils(L2tlb_agent_agent_driver)

    uvm_analysis_port #(L2tlb_agent_agent_transport_sample) transport_sample_ap;
    local longint unsigned transport_sample_seq;
    local memblock_sync_pkg::memblock_l2tlb_release_item_kind_e last_item_kind;
    local longint unsigned last_item_generation;
    local longint unsigned last_item_reset_epoch;
    local string last_item_owner_name;
    local bit last_item_is_post_reset_baseline;
    local bit post_reset_baseline_pending;
    local longint unsigned baseline_sent_sample_seq;
    local L2tlb_agent_agent_sequencer transport_slot_owner;
    local bit suppress_semantic_samples_after_final;
    local bit reset_quiescent;
    local longint unsigned reset_quiescent_epoch;
    local longint unsigned reset_semantic_sample_published_epoch;
    local longint unsigned driver_reset_applied_epoch;
    local int unsigned reset_active_sample_count;
    local bit response_reset_ack_sent;

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern virtual task reset_phase(uvm_phase phase);
    extern task main_phase(uvm_phase phase);
    extern virtual function void phase_ended(uvm_phase phase);
    extern task send_pkt(L2tlb_agent_agent_xaction tr);
    extern virtual task sample_previous_vif(
        output memblock_l2tlb_drv_sample_t sample);
    extern virtual function void publish_transport_sample(
        input memblock_l2tlb_drv_sample_t sample);
    extern virtual function void bind_transport_slot_owner(
        input L2tlb_agent_agent_sequencer sequencer);
    extern virtual function void recycle_transport_sample_at_drv_cb();
    extern virtual function void update_last_driven_metadata(
        input L2tlb_agent_agent_xaction tr,
        input bit got_item,
        input longint unsigned sampled_reset_epoch,
        input bit sampled_reset_active);
    extern virtual function bit physical_reset_active();
    extern virtual function bit transport_slot_empty();
    extern virtual function void update_reset_quiescent(
        input memblock_l2tlb_drv_sample_t sample);
    extern task drive_idle(tcnt_dec_base::drv_mode_e drv_mode);
endclass:L2tlb_agent_agent_driver

function L2tlb_agent_agent_driver::new(string name, uvm_component parent);
    super.new(name,parent);
    transport_sample_seq = 0;
    last_item_kind = memblock_sync_pkg::MEMBLOCK_L2TLB_ITEM_NORMAL;
    last_item_generation = 0;
    last_item_reset_epoch = 0;
    last_item_owner_name = "";
    last_item_is_post_reset_baseline = 1'b0;
    post_reset_baseline_pending = 1'b1;
    baseline_sent_sample_seq = 0;
    transport_slot_owner = null;
    suppress_semantic_samples_after_final = 1'b0;
    reset_quiescent = 1'b0;
    reset_quiescent_epoch = 0;
    reset_semantic_sample_published_epoch = 0;
    driver_reset_applied_epoch = 0;
    reset_active_sample_count = 0;
    response_reset_ack_sent = 1'b0;
endfunction:new

function void L2tlb_agent_agent_driver::build_phase(uvm_phase phase);
    super.build_phase(phase);
    transport_sample_ap = new("transport_sample_ap", this);
endfunction:build_phase

function void L2tlb_agent_agent_driver::phase_ended(uvm_phase phase);
    // phase callbacks are diagnostic only.  Releasing here would bypass the
    // final inactive sample and grant gate owned by the sequence/parent.
    if (memblock_sync_pkg::l2tlb_lifecycle_owner_claimed) begin
        `uvm_error(get_type_name(),
                   $sformatf("phase ended while L2TLB lifecycle owner remains claimed: %s",
                             memblock_sync_pkg::l2tlb_lifecycle_owner_name))
    end
    super.phase_ended(phase);
endfunction:phase_ended

task L2tlb_agent_agent_driver::reset_phase(uvm_phase phase);
    memblock_l2tlb_drv_sample_t reset_sample;
    int unsigned bootstrap_wait_samples;

    super.reset_phase(phase);
    if (memblock_sync_pkg::l2tlb_responder_active &&
        this.cfg.drv_mode != tcnt_dec_base::DRV_0) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("active L2TLB responder requires DRV_0, got drv_mode=%0d",
                             this.cfg.drv_mode))
    end
    phase.raise_objection(this);
    bootstrap_wait_samples = 0;

    repeat(2) begin
        @this.vif.drv_mp.drv_cb;
        this.drive_idle(this.cfg.drv_mode);
    end
    // The L2TLB driver is also the reset transport sampler.  Other agents keep
    // UVM reset_phase open until reset_backend_done, so sample the physical
    // reset directly here; otherwise the monitor would have no reset tuple
    // before the shared coordinator watchdog expires.
    while (memblock_sync_pkg::reset_backend_done !== 1'b1) begin
        @this.vif.drv_mp.drv_cb;
        bootstrap_wait_samples++;
        if (bootstrap_wait_samples >
            `MEMBLOCK_L2TLB_RESET_BACKEND_WAIT_MAX_SAMPLES) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("L2TLB reset backend bootstrap watchdog expired wait_samples=%0d rst_n=%b epoch=%0d ack=0x%0h",
                                 bootstrap_wait_samples,
                                 this.vif.rst_n,
                                 memblock_sync_pkg::get_l2tlb_current_reset_epoch(),
                                 memblock_sync_pkg::l2tlb_runtime_reset_ack_mask))
        end
        this.recycle_transport_sample_at_drv_cb();
        this.sample_previous_vif(reset_sample);
        this.update_reset_quiescent(reset_sample);
        this.publish_transport_sample(reset_sample);
        this.drive_idle(this.cfg.drv_mode);
    end
    @this.vif.drv_mp.drv_cb;
    this.recycle_transport_sample_at_drv_cb();
    this.sample_previous_vif(reset_sample);
    this.update_reset_quiescent(reset_sample);
    this.publish_transport_sample(reset_sample);
    this.drive_idle(this.cfg.drv_mode);
    phase.drop_objection(this);
endtask:reset_phase

task L2tlb_agent_agent_driver::main_phase(uvm_phase phase);
    memblock_l2tlb_drv_sample_t sample;
    bit got_item;

    super.main_phase(phase);
    //while(1) begin
    if(this.cfg.sqr_sw==tcnt_dec_base::ON && this.cfg.drv_sw==tcnt_dec_base::ON) begin
        while(1) begin
            @this.vif.drv_mp.drv_cb;
            this.recycle_transport_sample_at_drv_cb();
            if (suppress_semantic_samples_after_final &&
                !memblock_sync_pkg::l2tlb_lifecycle_owner_claimed &&
                this.transport_slot_empty() &&
                memblock_sync_pkg::l2tlb_transport_sample_mailbox_empty()) begin
                // The final sample was recycled and the owner has consumed
                // its grant.  Stop the physical sampler at this clean handoff
                // instead of relying on phase termination to kill it.
                return;
            end
            this.sample_previous_vif(sample);
            this.update_reset_quiescent(sample);
            this.publish_transport_sample(sample);
            // Publish wakes the semantic consumer, which may immediately
            // enqueue the next cycle item.  Stay within this time slot; do
            // not wait for another clock edge.
            #0;
            req = null;
            seq_item_port.try_next_item(req);
            got_item = (req != null);
            if (got_item &&
                (sample.sampled_reset_active ||
                 req.item_reset_epoch != sample.sampled_reset_epoch)) begin
                seq_item_port.item_done();
                req = null;
                got_item = 1'b0;
                this.drive_idle(this.cfg.drv_mode);
            end
            else if (req == null) begin
                this.drive_idle(this.cfg.drv_mode);
            end
            else begin
                if (req.pre_pkt_gap != 0 || req.post_pkt_gap != 0) begin
                    `uvm_fatal(get_type_name(),
                               $sformatf("L2TLB cycle item requires gap=0, got pre=%0d post=%0d",
                                         req.pre_pkt_gap, req.post_pkt_gap))
                end
                this.send_pkt(req);
                seq_item_port.item_done();
                if (req.is_post_reset_baseline) begin
                    post_reset_baseline_pending = 1'b1;
                    baseline_sent_sample_seq = sample.dut_sample_seq;
                end
                got_item = 1'b1;
            end
            this.update_last_driven_metadata(req, got_item,
                                             sample.sampled_reset_epoch,
                                             sample.sampled_reset_active);
        end
    end
    else if (this.cfg.drv_sw==tcnt_dec_base::ON) begin
        while(1) begin
            @this.vif.drv_mp.drv_cb;
            `uvm_fatal(get_type_name(), $sformatf("sqr_sw==OFF & drv_sw==ON, please give a driver send task!"))
            //send task
        end
    end
endtask:main_phase

task L2tlb_agent_agent_driver::sample_previous_vif(
    output memblock_l2tlb_drv_sample_t sample);
    string owner_name;
    int unsigned probe_count;

    sample = '{default:'0,
               sample_ready_result:
                   memblock_sync_pkg::MEMBLOCK_L2TLB_SAMPLE_NOT_READY,
               sampled_item_kind:
                   memblock_sync_pkg::MEMBLOCK_L2TLB_ITEM_NORMAL};
    transport_sample_seq++;
    if (transport_sample_seq == 0) begin
        `uvm_fatal(get_type_name(), "transport_sample_seq wrapped to zero")
    end
    sample.transport_sample_seq = transport_sample_seq;
    probe_count = 0;
    while (probe_count <= `MEMBLOCK_L2TLB_SAMPLE_PROBE_MAX_DELTA) begin
        if (memblock_sync_pkg::dut_sample_time_valid &&
            memblock_sync_pkg::dut_sample_time == $time) begin
            break;
        end
        if (probe_count != `MEMBLOCK_L2TLB_SAMPLE_PROBE_MAX_DELTA) begin
            uvm_wait_for_nba_region();
            #0;
        end
        probe_count++;
    end
    sample.dut_sample_seq = memblock_sync_pkg::peek_current_dut_global_sample();
    sample.sample_valid = memblock_sync_pkg::dut_sample_time_valid &&
                          memblock_sync_pkg::dut_sample_time == $time &&
                          sample.dut_sample_seq != 0;
    sample.sampled_time = $time;
    sample.sampled_reset_active = this.physical_reset_active() ||
                                  memblock_sync_pkg::l2tlb_reset_active();
    sample.sampled_reset_epoch =
        memblock_sync_pkg::get_l2tlb_current_reset_epoch();
    if (sample.sample_valid && !sample.sampled_reset_active &&
        (memblock_sync_pkg::csr_history_published_seq > sample.dut_sample_seq ||
         memblock_sync_pkg::lifecycle_event_published_seq > sample.dut_sample_seq)) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("transport sample producer watermark is from the future sample=%0d csr=%0d event=%0d",
                             sample.dut_sample_seq,
                             memblock_sync_pkg::csr_history_published_seq,
                             memblock_sync_pkg::lifecycle_event_published_seq))
    end
    sample.sample_ready_result =
        (sample.sample_valid && !sample.sampled_reset_active &&
         memblock_sync_pkg::l2tlb_sample_ready(sample.dut_sample_seq)) ?
        memblock_sync_pkg::MEMBLOCK_L2TLB_SAMPLE_READY :
        memblock_sync_pkg::MEMBLOCK_L2TLB_SAMPLE_NOT_READY;
    if (sample.sampled_reset_active &&
        memblock_sync_pkg::l2tlb_reset_active() &&
        sample.sampled_reset_epoch != 0 &&
        driver_reset_applied_epoch != sample.sampled_reset_epoch &&
        this.transport_slot_empty()) begin
        memblock_sync_pkg::reset_l2tlb_driver_runtime_state(
            sample.sampled_reset_epoch);
        driver_reset_applied_epoch = sample.sampled_reset_epoch;
        reset_semantic_sample_published_epoch = 0;
        reset_active_sample_count = 0;
        response_reset_ack_sent = 1'b0;
        suppress_semantic_samples_after_final = 1'b0;
        post_reset_baseline_pending = 1'b1;
        baseline_sent_sample_seq = 0;
    end

    sample.sampled_req_valid = this.vif.mon_mp.mon_cb.io_ptw_req_0_valid;
    sample.sampled_req_ready = this.vif.mon_mp.mon_cb.io_ptw_req_0_ready;
    sample.sampled_req_vpn = this.vif.mon_mp.mon_cb.io_ptw_req_0_bits_vpn;
    sample.sampled_req_s2xlate = this.vif.mon_mp.mon_cb.io_ptw_req_0_bits_s2xlate;
    sample.sampled_resp_valid = this.vif.mon_mp.mon_cb.io_ptw_resp_valid;
    if (!sample.sampled_reset_active &&
        $isunknown({sample.sampled_req_valid,
                    sample.sampled_req_ready,
                    sample.sampled_resp_valid})) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("L2TLB transport handshake contains X/Z at time=%0t valid=%b ready=%b resp_valid=%b",
                             $time, sample.sampled_req_valid,
                             sample.sampled_req_ready,
                             sample.sampled_resp_valid))
    end
    sample.sampled_req_fire =
        (sample.sampled_req_valid === 1'b1) &&
        (sample.sampled_req_ready === 1'b1);
    sample.sampled_resp_bits_s2xlate = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s2xlate;
    sample.sampled_resp_bits_s1_entry_tag = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_entry_tag;
    sample.sampled_resp_bits_s1_entry_asid = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_entry_asid;
    sample.sampled_resp_bits_s1_entry_vmid = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_entry_vmid;
    sample.sampled_resp_bits_s1_entry_n = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_entry_n;
    sample.sampled_resp_bits_s1_entry_pbmt = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_entry_pbmt;
    sample.sampled_resp_bits_s1_entry_perm_d = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_entry_perm_d;
    sample.sampled_resp_bits_s1_entry_perm_a = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_entry_perm_a;
    sample.sampled_resp_bits_s1_entry_perm_g = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_entry_perm_g;
    sample.sampled_resp_bits_s1_entry_perm_u = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_entry_perm_u;
    sample.sampled_resp_bits_s1_entry_perm_x = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_entry_perm_x;
    sample.sampled_resp_bits_s1_entry_perm_w = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_entry_perm_w;
    sample.sampled_resp_bits_s1_entry_perm_r = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_entry_perm_r;
    sample.sampled_resp_bits_s1_entry_level = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_entry_level;
    sample.sampled_resp_bits_s1_entry_v = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_entry_v;
    sample.sampled_resp_bits_s1_entry_ppn = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_entry_ppn;
    sample.sampled_resp_bits_s1_addr_low = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_addr_low;
    sample.sampled_resp_bits_s1_ppn_low_0 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_ppn_low_0;
    sample.sampled_resp_bits_s1_ppn_low_1 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_ppn_low_1;
    sample.sampled_resp_bits_s1_ppn_low_2 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_ppn_low_2;
    sample.sampled_resp_bits_s1_ppn_low_3 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_ppn_low_3;
    sample.sampled_resp_bits_s1_ppn_low_4 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_ppn_low_4;
    sample.sampled_resp_bits_s1_ppn_low_5 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_ppn_low_5;
    sample.sampled_resp_bits_s1_ppn_low_6 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_ppn_low_6;
    sample.sampled_resp_bits_s1_ppn_low_7 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_ppn_low_7;
    sample.sampled_resp_bits_s1_valididx_0 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_valididx_0;
    sample.sampled_resp_bits_s1_valididx_1 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_valididx_1;
    sample.sampled_resp_bits_s1_valididx_2 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_valididx_2;
    sample.sampled_resp_bits_s1_valididx_3 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_valididx_3;
    sample.sampled_resp_bits_s1_valididx_4 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_valididx_4;
    sample.sampled_resp_bits_s1_valididx_5 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_valididx_5;
    sample.sampled_resp_bits_s1_valididx_6 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_valididx_6;
    sample.sampled_resp_bits_s1_valididx_7 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_valididx_7;
    sample.sampled_resp_bits_s1_pteidx_0 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_pteidx_0;
    sample.sampled_resp_bits_s1_pteidx_1 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_pteidx_1;
    sample.sampled_resp_bits_s1_pteidx_2 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_pteidx_2;
    sample.sampled_resp_bits_s1_pteidx_3 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_pteidx_3;
    sample.sampled_resp_bits_s1_pteidx_4 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_pteidx_4;
    sample.sampled_resp_bits_s1_pteidx_5 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_pteidx_5;
    sample.sampled_resp_bits_s1_pteidx_6 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_pteidx_6;
    sample.sampled_resp_bits_s1_pteidx_7 = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_pteidx_7;
    sample.sampled_resp_bits_s1_pf = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_pf;
    sample.sampled_resp_bits_s1_af = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s1_af;
    sample.sampled_resp_bits_s2_entry_tag = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s2_entry_tag;
    sample.sampled_resp_bits_s2_entry_vmid = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s2_entry_vmid;
    sample.sampled_resp_bits_s2_entry_n = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s2_entry_n;
    sample.sampled_resp_bits_s2_entry_pbmt = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s2_entry_pbmt;
    sample.sampled_resp_bits_s2_entry_ppn = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s2_entry_ppn;
    sample.sampled_resp_bits_s2_entry_perm_d = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s2_entry_perm_d;
    sample.sampled_resp_bits_s2_entry_perm_a = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s2_entry_perm_a;
    sample.sampled_resp_bits_s2_entry_perm_g = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s2_entry_perm_g;
    sample.sampled_resp_bits_s2_entry_perm_u = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s2_entry_perm_u;
    sample.sampled_resp_bits_s2_entry_perm_x = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s2_entry_perm_x;
    sample.sampled_resp_bits_s2_entry_perm_w = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s2_entry_perm_w;
    sample.sampled_resp_bits_s2_entry_perm_r = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s2_entry_perm_r;
    sample.sampled_resp_bits_s2_entry_level = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s2_entry_level;
    sample.sampled_resp_bits_s2_gpf = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s2_gpf;
    sample.sampled_resp_bits_s2_gaf = this.vif.mon_mp.mon_cb.io_ptw_resp_bits_s2_gaf;

    sample.sampled_item_kind = last_item_kind;
    sample.sampled_item_generation = last_item_generation;
    sample.sampled_item_reset_epoch = last_item_reset_epoch;
    sample.sampled_item_owner_name = last_item_owner_name;
    sample.sampled_item_is_post_reset_baseline = last_item_is_post_reset_baseline;

    if (sample.sampled_reset_active) begin
        // A reset sample must not expose the previous stop/final metadata as
        // a new transport proof.  The already fetched item is handled by the
        // main loop's stale-item path and is completed exactly once there.
        sample.sampled_item_kind = memblock_sync_pkg::MEMBLOCK_L2TLB_ITEM_NORMAL;
        sample.sampled_item_generation = 0;
        sample.sampled_item_reset_epoch = 0;
        sample.sampled_item_owner_name = "";
        sample.sampled_item_is_post_reset_baseline = 1'b0;
        last_item_kind = memblock_sync_pkg::MEMBLOCK_L2TLB_ITEM_NORMAL;
        last_item_generation = 0;
        last_item_reset_epoch = 0;
        last_item_owner_name = "";
        last_item_is_post_reset_baseline = 1'b0;
    end

    // A baseline is proved only by a later real transport sample.  The proof
    // is local to the driver and is copied into this immutable sample before
    // publication; sequence/monitor consumers never infer it from a live item.
    if (!sample.sampled_reset_active && post_reset_baseline_pending &&
        last_item_is_post_reset_baseline) begin
        if (last_item_kind != memblock_sync_pkg::MEMBLOCK_L2TLB_ITEM_NORMAL ||
            last_item_reset_epoch != sample.sampled_reset_epoch) begin
            `uvm_fatal(get_type_name(),
                       "post-reset baseline metadata has invalid item kind/epoch")
        end
        if (sample.sample_valid && sample.dut_sample_seq > baseline_sent_sample_seq) begin
            if (sample.sampled_req_ready !== 1'b0 ||
                sample.sampled_req_fire || sample.sampled_resp_valid) begin
                `uvm_fatal(get_type_name(),
                           "post-reset baseline proof observed active transport")
            end
            if (sample.dut_sample_seq - baseline_sent_sample_seq >
                `MEMBLOCK_L2TLB_BASELINE_MAX_SAMPLE_DISTANCE) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("post-reset baseline proof timed out sent=%0d current=%0d",
                                     baseline_sent_sample_seq, sample.dut_sample_seq))
            end
            if (sample.sampled_reset_epoch != 0) begin
                memblock_sync_pkg::mark_l2tlb_post_reset_baseline_done(
                    sample.sampled_reset_epoch,
                    sample.dut_sample_seq);
            end
            post_reset_baseline_pending = 1'b0;
        end
    end
    sample.baseline_required = post_reset_baseline_pending;
    sample.baseline_proof_pending = post_reset_baseline_pending &&
                                    (baseline_sent_sample_seq != 0);
    sample.baseline_sent_sample_seq = baseline_sent_sample_seq;

    owner_name = (last_item_owner_name != "") ?
                 last_item_owner_name :
                 memblock_sync_pkg::l2tlb_lifecycle_owner_name;
    if (!sample.sampled_reset_active && sample.sample_valid &&
        last_item_reset_epoch == sample.sampled_reset_epoch &&
        last_item_kind == memblock_sync_pkg::MEMBLOCK_L2TLB_ITEM_RELEASE_STOP) begin
        memblock_sync_pkg::confirm_l2tlb_admission_closed_at_drv_cb(
            owner_name,
            sample.dut_sample_seq,
            sample.sampled_reset_epoch,
            sample.sampled_item_owner_name,
            sample.sampled_item_kind,
            sample.sampled_item_generation,
            sample.sampled_item_reset_epoch,
            sample.sampled_req_fire,
            sample.sampled_req_ready);
    end
    if (!sample.sampled_reset_active && sample.sample_valid &&
        last_item_reset_epoch == sample.sampled_reset_epoch &&
        last_item_kind == memblock_sync_pkg::MEMBLOCK_L2TLB_ITEM_RELEASE_FINAL_INACTIVE) begin
        memblock_sync_pkg::mark_l2tlb_final_inactive_at_drv_cb(
            owner_name,
            sample.dut_sample_seq,
            sample.transport_sample_seq,
            sample.sampled_reset_epoch,
            sample.sampled_item_owner_name,
            sample.sampled_item_kind,
            sample.sampled_item_generation,
            sample.sampled_item_reset_epoch,
            sample.sampled_req_ready,
            sample.sampled_req_fire,
            sample.sampled_resp_valid);
        sample.sampled_final_inactive_proof_valid = 1'b1;
        sample.sampled_final_inactive_proof_epoch = sample.sampled_reset_epoch;
        sample.sampled_final_inactive_proof_transport_sample_seq =
            sample.transport_sample_seq;
    end
endtask:sample_previous_vif

// Abstract responsibility: combine the physical VIF reset and the shared
// runtime-reset coordinator into one driver-side wire-safety predicate.
function bit L2tlb_agent_agent_driver::physical_reset_active();
    return (this.vif.rst_n !== 1'b1) ||
           (memblock_sync_pkg::reset_backend_done !== 1'b1);
endfunction:physical_reset_active

// Abstract responsibility: expose whether the single semantic sample slot is
// reusable.  A package EMPTY proof alone is insufficient while the sequencer
// still owns a terminal/PUBLISHED handle.
function bit L2tlb_agent_agent_driver::transport_slot_empty();
    if (transport_slot_owner == null) begin
        return 1'b1;
    end
    return transport_slot_owner.transport_sample_slot_empty() &&
           memblock_sync_pkg::l2tlb_transport_sample_mailbox_empty();
endfunction:transport_slot_empty

// Abstract responsibility: transition the driver through one reset epoch's
// quiescent state.  The transition is made only after the owner cleanup and
// the physical semantic slot cleanup are both observable; the driver then
// writes the RESPONSE ack once and suppresses further semantic samples.
function void L2tlb_agent_agent_driver::update_reset_quiescent(
    input memblock_l2tlb_drv_sample_t sample);
    bit response_ack_required;
    bit response_owner_reset_done;

    if (!sample.sampled_reset_active) begin
        if (reset_quiescent) begin
            reset_quiescent = 1'b0;
            reset_quiescent_epoch = 0;
            reset_active_sample_count = 0;
            response_reset_ack_sent = 1'b0;
            reset_semantic_sample_published_epoch = 0;
            post_reset_baseline_pending = 1'b1;
            baseline_sent_sample_seq = 0;
            suppress_semantic_samples_after_final = 1'b0;
        end
        return;
    end

    // Do not turn a physical reset observation into a runtime-reset state
    // transition before the coordinator has published an epoch.
    if (!memblock_sync_pkg::l2tlb_reset_active()) begin
        return;
    end

    // Physical reset may be visible before the runtime coordinator allocates
    // epoch 1. It is analysis-only and must not consume the runtime-reset
    // watchdog budget.
    if (sample.sampled_reset_epoch == 0) begin
        return;
    end
    // A new runtime epoch invalidates an older local quiescent proof before
    // the driver can acknowledge the new epoch.
    if (reset_quiescent &&
        reset_quiescent_epoch != sample.sampled_reset_epoch) begin
        reset_quiescent = 1'b0;
        reset_quiescent_epoch = 0;
        reset_active_sample_count = 0;
        response_reset_ack_sent = 1'b0;
        reset_semantic_sample_published_epoch = 0;
        post_reset_baseline_pending = 1'b1;
        baseline_sent_sample_seq = 0;
        suppress_semantic_samples_after_final = 1'b0;
    end
    if (reset_quiescent) begin
        return;
    end
    reset_active_sample_count++;
    if (reset_active_sample_count >
        `MEMBLOCK_L2TLB_RESET_WATCHDOG_MAX_SAMPLES) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("L2TLB driver reset watchdog expired epoch=%0d transport=%0d dut_sample=%0d count=%0d quiescent=%0d owner_reset_done=%0d driver_reset_done=%0d mailbox_empty=%0d",
                             sample.sampled_reset_epoch,
                             sample.transport_sample_seq,
                             sample.dut_sample_seq,
                             reset_active_sample_count,
                             reset_quiescent,
                             memblock_sync_pkg::l2tlb_response_owner_reset_done_epoch,
                             memblock_sync_pkg::l2tlb_driver_reset_done_epoch,
                             this.transport_slot_empty()))
    end
    response_ack_required =
        (memblock_sync_pkg::l2tlb_runtime_reset_required_ack_mask &
         memblock_sync_pkg::MEMBLOCK_L2TLB_RESET_ACK_RESPONSE) != '0;
    response_owner_reset_done =
        memblock_sync_pkg::l2tlb_response_owner_reset_done_epoch ==
            sample.sampled_reset_epoch;
    if (driver_reset_applied_epoch != sample.sampled_reset_epoch ||
        !this.transport_slot_empty() ||
        !memblock_sync_pkg::l2tlb_transport_sample_mailbox_empty() ||
        (response_ack_required && !response_owner_reset_done) ||
        (response_ack_required &&
         memblock_sync_pkg::l2tlb_driver_reset_done_epoch !=
             sample.sampled_reset_epoch)) begin
        return;
    end

    reset_quiescent = 1'b1;
    reset_quiescent_epoch = sample.sampled_reset_epoch;
    if (response_ack_required && !response_reset_ack_sent) begin
        memblock_sync_pkg::acknowledge_l2tlb_response_reset_if_quiescent(
            memblock_sync_pkg::l2tlb_lifecycle_owner_name,
            sample.sampled_reset_epoch);
        response_reset_ack_sent = 1'b1;
    end
endfunction:update_reset_quiescent

function void L2tlb_agent_agent_driver::publish_transport_sample(
    input memblock_l2tlb_drv_sample_t sample);
    L2tlb_agent_agent_transport_sample wrapper;
    bit publish_semantic_sample;

    wrapper = L2tlb_agent_agent_transport_sample::type_id::create(
        $sformatf("l2tlb_transport_sample_%0d", sample.transport_sample_seq));
    if (wrapper == null) begin
        `uvm_fatal(get_type_name(), "failed to create transport sample")
    end
    wrapper.fill_payload(sample);
    wrapper.freeze();
    publish_semantic_sample = memblock_sync_pkg::l2tlb_lifecycle_owner_claimed &&
                              !suppress_semantic_samples_after_final &&
                              !reset_quiescent &&
                              this.transport_slot_empty() &&
                              (!sample.sampled_reset_active ||
                               (memblock_sync_pkg::l2tlb_reset_active() &&
                                sample.sampled_reset_epoch != 0 &&
                                reset_semantic_sample_published_epoch !=
                                    sample.sampled_reset_epoch));
    if (publish_semantic_sample) begin
        if (transport_slot_owner == null ||
            !transport_slot_owner.publish_transport_sample(wrapper)) begin
            `uvm_fatal(get_type_name(), "failed to reserve L2TLB transport sample slot")
        end
        if (sample.sampled_reset_active) begin
            reset_semantic_sample_published_epoch = sample.sampled_reset_epoch;
        end
    end
    transport_sample_ap.write(wrapper);
    // Analysis-only samples remain outside the semantic mailbox, but they are
    // still part of the single physical transport timeline.
    if (transport_slot_owner != null) begin
        transport_slot_owner.note_transport_sample_observed(
            sample.transport_sample_seq);
    end
    // The monitor analysis consumer has completed synchronously at this point.
    // Only now wake the semantic owner and publish the mailbox non-empty proof.
    if (publish_semantic_sample) begin
        memblock_sync_pkg::mark_l2tlb_transport_sample_mailbox_nonempty();
        if (transport_slot_owner == null ||
            !transport_slot_owner.notify_transport_sample_published()) begin
            `uvm_fatal(get_type_name(),
                       "failed to notify L2TLB semantic owner after analysis write")
        end
    end
endfunction:publish_transport_sample

function void L2tlb_agent_agent_driver::bind_transport_slot_owner(
    input L2tlb_agent_agent_sequencer sequencer);
    if (sequencer == null || transport_slot_owner != null) begin
        `uvm_fatal(get_type_name(), "invalid or duplicate L2TLB transport slot owner bind")
    end
    transport_slot_owner = sequencer;
endfunction:bind_transport_slot_owner

function void L2tlb_agent_agent_driver::recycle_transport_sample_at_drv_cb();
    longint unsigned recycle_seq;

    if (transport_slot_owner == null ||
        !transport_slot_owner.get_recyclable_transport_sample_seq(recycle_seq)) begin
        return;
    end
    if (!transport_slot_owner.recycle_transport_sample(recycle_seq)) begin
        `uvm_fatal(get_type_name(), "failed to recycle L2TLB transport sample")
    end
    memblock_sync_pkg::mark_l2tlb_transport_sample_recycled(recycle_seq);
    // The package helper records the special final sequence; ordinary samples
    // use the same physical slot and must also return the generic proof to
    // EMPTY after the sequencer recycle has completed.
    memblock_sync_pkg::mark_l2tlb_transport_sample_mailbox_empty();
    if (recycle_seq == memblock_sync_pkg::l2tlb_release_final_inactive_transport_sample_seq) begin
        suppress_semantic_samples_after_final = 1'b1;
        // The final proof has already been frozen and consumed.  Clear its
        // provenance only after the physical sample is recycled, preventing a
        // later callback from issuing a duplicate final confirmation while
        // retaining it long enough for the proof callback itself.
        last_item_kind = memblock_sync_pkg::MEMBLOCK_L2TLB_ITEM_NORMAL;
        last_item_generation = 0;
        last_item_reset_epoch = 0;
        last_item_owner_name = "";
        last_item_is_post_reset_baseline = 1'b0;
    end
endfunction:recycle_transport_sample_at_drv_cb

function void L2tlb_agent_agent_driver::update_last_driven_metadata(
    input L2tlb_agent_agent_xaction tr,
    input bit got_item,
    input longint unsigned sampled_reset_epoch,
    input bit sampled_reset_active);
    if (got_item && tr != null &&
        post_reset_baseline_pending &&
        last_item_is_post_reset_baseline &&
        !tr.is_post_reset_baseline &&
        tr.item_kind == memblock_sync_pkg::MEMBLOCK_L2TLB_ITEM_NORMAL &&
        last_item_reset_epoch == sampled_reset_epoch &&
        tr.item_reset_epoch == sampled_reset_epoch &&
        !sampled_reset_active) begin
        // An unanchored/NOT_READY semantic sample may still send a NORMAL
        // inactive item.  It must not erase the previous baseline tag before
        // a later real sample can prove that baseline.
        if (tr.io_ptw_req_0_ready !== 1'b0 ||
            tr.io_ptw_resp_valid !== 1'b0) begin
            `uvm_fatal(get_type_name(),
                       "baseline pending NORMAL item must remain transport inactive")
        end
        return;
    end
    if (got_item && tr != null) begin
        last_item_kind = tr.item_kind;
        last_item_generation = tr.item_generation;
        last_item_reset_epoch = tr.item_reset_epoch;
        last_item_owner_name = tr.item_owner_name;
        last_item_is_post_reset_baseline = tr.is_post_reset_baseline;
    end else if (post_reset_baseline_pending &&
                 last_item_is_post_reset_baseline &&
                 last_item_reset_epoch == sampled_reset_epoch &&
                 !sampled_reset_active) begin
        // An idle/not-ready item is transport activity, not a replacement for
        // the baseline provenance.  Retain the last baseline tag until a later
        // real sample proves it or a reset starts a new epoch.
        return;
    end else if (last_item_kind ==
                 memblock_sync_pkg::MEMBLOCK_L2TLB_ITEM_RELEASE_FINAL_INACTIVE &&
                 !suppress_semantic_samples_after_final &&
                 last_item_reset_epoch == sampled_reset_epoch &&
                 !sampled_reset_active) begin
        // Keep final provenance until recycle_transport_sample_at_drv_cb()
        // clears it.  A NOT_READY sample cannot replace a terminal item tag.
        return;
    end else begin
        last_item_kind = memblock_sync_pkg::MEMBLOCK_L2TLB_ITEM_NORMAL;
        last_item_generation = 0;
        last_item_reset_epoch = 0;
        last_item_owner_name = "";
        last_item_is_post_reset_baseline = 1'b0;
    end
endfunction:update_last_driven_metadata

task L2tlb_agent_agent_driver::send_pkt(L2tlb_agent_agent_xaction tr);
    vif.drv_mp.drv_cb.io_ptw_req_0_ready <= tr.io_ptw_req_0_ready;
    vif.drv_mp.drv_cb.io_ptw_resp_valid <= tr.io_ptw_resp_valid;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s2xlate <= tr.io_ptw_resp_bits_s2xlate;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_tag <= tr.io_ptw_resp_bits_s1_entry_tag;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_asid <= tr.io_ptw_resp_bits_s1_entry_asid;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_vmid <= tr.io_ptw_resp_bits_s1_entry_vmid;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_n <= tr.io_ptw_resp_bits_s1_entry_n;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_pbmt <= tr.io_ptw_resp_bits_s1_entry_pbmt;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_d <= tr.io_ptw_resp_bits_s1_entry_perm_d;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_a <= tr.io_ptw_resp_bits_s1_entry_perm_a;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_g <= tr.io_ptw_resp_bits_s1_entry_perm_g;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_u <= tr.io_ptw_resp_bits_s1_entry_perm_u;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_x <= tr.io_ptw_resp_bits_s1_entry_perm_x;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_w <= tr.io_ptw_resp_bits_s1_entry_perm_w;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_r <= tr.io_ptw_resp_bits_s1_entry_perm_r;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_level <= tr.io_ptw_resp_bits_s1_entry_level;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_v <= tr.io_ptw_resp_bits_s1_entry_v;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_ppn <= tr.io_ptw_resp_bits_s1_entry_ppn;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_addr_low <= tr.io_ptw_resp_bits_s1_addr_low;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_0 <= tr.io_ptw_resp_bits_s1_ppn_low_0;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_1 <= tr.io_ptw_resp_bits_s1_ppn_low_1;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_2 <= tr.io_ptw_resp_bits_s1_ppn_low_2;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_3 <= tr.io_ptw_resp_bits_s1_ppn_low_3;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_4 <= tr.io_ptw_resp_bits_s1_ppn_low_4;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_5 <= tr.io_ptw_resp_bits_s1_ppn_low_5;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_6 <= tr.io_ptw_resp_bits_s1_ppn_low_6;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_7 <= tr.io_ptw_resp_bits_s1_ppn_low_7;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_0 <= tr.io_ptw_resp_bits_s1_valididx_0;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_1 <= tr.io_ptw_resp_bits_s1_valididx_1;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_2 <= tr.io_ptw_resp_bits_s1_valididx_2;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_3 <= tr.io_ptw_resp_bits_s1_valididx_3;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_4 <= tr.io_ptw_resp_bits_s1_valididx_4;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_5 <= tr.io_ptw_resp_bits_s1_valididx_5;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_6 <= tr.io_ptw_resp_bits_s1_valididx_6;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_7 <= tr.io_ptw_resp_bits_s1_valididx_7;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_0 <= tr.io_ptw_resp_bits_s1_pteidx_0;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_1 <= tr.io_ptw_resp_bits_s1_pteidx_1;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_2 <= tr.io_ptw_resp_bits_s1_pteidx_2;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_3 <= tr.io_ptw_resp_bits_s1_pteidx_3;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_4 <= tr.io_ptw_resp_bits_s1_pteidx_4;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_5 <= tr.io_ptw_resp_bits_s1_pteidx_5;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_6 <= tr.io_ptw_resp_bits_s1_pteidx_6;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_7 <= tr.io_ptw_resp_bits_s1_pteidx_7;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pf <= tr.io_ptw_resp_bits_s1_pf;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_af <= tr.io_ptw_resp_bits_s1_af;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_tag <= tr.io_ptw_resp_bits_s2_entry_tag;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_vmid <= tr.io_ptw_resp_bits_s2_entry_vmid;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_n <= tr.io_ptw_resp_bits_s2_entry_n;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_pbmt <= tr.io_ptw_resp_bits_s2_entry_pbmt;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_ppn <= tr.io_ptw_resp_bits_s2_entry_ppn;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_d <= tr.io_ptw_resp_bits_s2_entry_perm_d;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_a <= tr.io_ptw_resp_bits_s2_entry_perm_a;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_g <= tr.io_ptw_resp_bits_s2_entry_perm_g;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_u <= tr.io_ptw_resp_bits_s2_entry_perm_u;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_x <= tr.io_ptw_resp_bits_s2_entry_perm_x;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_w <= tr.io_ptw_resp_bits_s2_entry_perm_w;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_r <= tr.io_ptw_resp_bits_s2_entry_perm_r;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_level <= tr.io_ptw_resp_bits_s2_entry_level;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_gpf <= tr.io_ptw_resp_bits_s2_gpf;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_gaf <= tr.io_ptw_resp_bits_s2_gaf;

endtask:send_pkt

task L2tlb_agent_agent_driver::drive_idle(tcnt_dec_base::drv_mode_e drv_mode);

    if(drv_mode==tcnt_dec_base::DRV_0) begin
        // 中文注释：idle/reset/sequence退出时固定关闭ready和response。
        // 只有唯一lifecycle owner发送的逐拍cycle item可以授权接收DTLB request。
        vif.drv_mp.drv_cb.io_ptw_req_0_ready <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_valid <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2xlate <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_tag <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_asid <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_vmid <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_n <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_pbmt <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_d <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_a <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_g <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_u <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_x <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_w <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_r <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_level <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_v <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_ppn <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_addr_low <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_0 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_1 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_2 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_3 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_4 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_5 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_6 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_7 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_0 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_1 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_2 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_3 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_4 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_5 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_6 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_7 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_0 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_1 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_2 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_3 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_4 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_5 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_6 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_7 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pf <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_af <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_tag <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_vmid <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_n <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_pbmt <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_ppn <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_d <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_a <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_g <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_u <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_x <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_w <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_r <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_level <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_gpf <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_gaf <= '0;

    end
    else if(drv_mode==tcnt_dec_base::DRV_1) begin
        vif.drv_mp.drv_cb.io_ptw_req_0_ready <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_valid <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2xlate <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_tag <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_asid <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_vmid <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_n <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_pbmt <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_d <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_a <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_g <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_u <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_x <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_w <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_r <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_level <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_v <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_ppn <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_addr_low <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_0 <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_1 <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_2 <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_3 <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_4 <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_5 <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_6 <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_7 <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_0 <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_1 <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_2 <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_3 <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_4 <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_5 <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_6 <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_7 <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_0 <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_1 <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_2 <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_3 <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_4 <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_5 <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_6 <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_7 <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pf <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_af <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_tag <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_vmid <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_n <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_pbmt <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_ppn <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_d <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_a <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_g <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_u <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_x <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_w <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_r <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_level <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_gpf <= '1;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_gaf <= '1;

    end
    else if(drv_mode==tcnt_dec_base::DRV_X) begin
        vif.drv_mp.drv_cb.io_ptw_req_0_ready <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_valid <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2xlate <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_tag <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_asid <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_vmid <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_n <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_pbmt <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_d <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_a <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_g <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_u <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_x <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_w <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_r <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_level <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_v <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_ppn <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_addr_low <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_0 <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_1 <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_2 <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_3 <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_4 <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_5 <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_6 <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_7 <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_0 <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_1 <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_2 <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_3 <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_4 <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_5 <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_6 <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_7 <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_0 <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_1 <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_2 <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_3 <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_4 <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_5 <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_6 <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_7 <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pf <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_af <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_tag <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_vmid <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_n <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_pbmt <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_ppn <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_d <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_a <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_g <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_u <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_x <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_w <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_r <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_level <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_gpf <= 'x;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_gaf <= 'x;

    end
    else if(drv_mode==tcnt_dec_base::DRV_RAND) begin
        vif.drv_mp.drv_cb.io_ptw_req_0_ready <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_valid <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2xlate <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_tag <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_asid <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_vmid <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_n <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_pbmt <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_d <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_a <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_g <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_u <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_x <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_w <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_r <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_level <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_v <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_ppn <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_addr_low <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_0 <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_1 <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_2 <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_3 <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_4 <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_5 <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_6 <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_7 <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_0 <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_1 <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_2 <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_3 <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_4 <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_5 <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_6 <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_7 <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_0 <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_1 <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_2 <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_3 <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_4 <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_5 <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_6 <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_7 <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pf <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_af <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_tag <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_vmid <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_n <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_pbmt <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_ppn <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_d <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_a <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_g <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_u <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_x <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_w <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_r <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_level <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_gpf <= $urandom;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_gaf <= $urandom;

    end
    else if(drv_mode==tcnt_dec_base::DRV_LST) begin
        vif.drv_mp.drv_cb.io_ptw_req_0_ready <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_valid <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2xlate <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_tag <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_asid <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_vmid <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_n <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_pbmt <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_d <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_a <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_g <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_u <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_x <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_w <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_perm_r <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_level <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_v <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_entry_ppn <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_addr_low <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_0 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_1 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_2 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_3 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_4 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_5 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_6 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_ppn_low_7 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_0 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_1 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_2 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_3 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_4 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_5 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_6 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_valididx_7 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_0 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_1 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_2 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_3 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_4 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_5 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_6 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pteidx_7 <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_pf <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s1_af <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_tag <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_vmid <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_n <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_pbmt <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_ppn <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_d <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_a <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_g <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_u <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_x <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_w <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_r <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_level <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_gpf <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_gaf <= '0;

    end

endtask:drive_idle

`endif
