//=========================================================
//File name    : io_mem_to_ooo_ctrl_agent_agent_monitor.sv
//Author       : OpenAI_Codex
//Module name  : io_mem_to_ooo_ctrl_agent_agent_monitor
//Discribution : io_mem_to_ooo_ctrl_agent_agent_monitor : monitor
//Date         : 2026-04-12
//=========================================================
`ifndef IO_MEM_TO_OOO_CTRL_AGENT_AGENT_MONITOR__SV
`define IO_MEM_TO_OOO_CTRL_AGENT_AGENT_MONITOR__SV

`include "memblock_compile_params.svh"

class io_mem_to_ooo_ctrl_agent_agent_monitor  extends tcnt_monitor_base#(virtual io_mem_to_ooo_ctrl_agent_agent_interface,io_mem_to_ooo_ctrl_agent_agent_cfg,io_mem_to_ooo_ctrl_agent_agent_xaction);

    `uvm_component_utils(io_mem_to_ooo_ctrl_agent_agent_monitor)

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern task run_phase(uvm_phase phase);
    extern task mon_data();
    extern function bit read_rollback_hdl(input string path,
                                          output uvm_hdl_data_t value);
    extern function bit probe_rollback_hdl(input string path,
                                           output uvm_hdl_data_t value,
                                           output bit known);
    extern function void sample_rollback_diagnostics(
        ref memblock_sync_pkg::dispatch_raw_ctrl_t raw);
    extern function void log_uncache_resource_snapshot(
        input longint unsigned sample_seq,
        input bit rob_flag,
        input bit [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] rob_value);

    bit uncache_snapshot_valid;
    int unsigned uncache_nack_diag_count;
    logic [15:0] previous_uncache_lq_req_valid;
    logic [15:0][1:0] previous_uncache_lq_state;
    logic [15:0] previous_uncache_lq_need_flush_reg;
    logic [15:0] previous_uncache_top_valid;
    logic [15:0] previous_uncache_top_inflight;
    logic [15:0] previous_uncache_top_wait_return;
endclass:io_mem_to_ooo_ctrl_agent_agent_monitor

function io_mem_to_ooo_ctrl_agent_agent_monitor::new(string name, uvm_component parent);
    super.new(name,parent);
    uncache_snapshot_valid = 1'b0;
    uncache_nack_diag_count = 0;
endfunction:new

function void io_mem_to_ooo_ctrl_agent_agent_monitor::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

function bit io_mem_to_ooo_ctrl_agent_agent_monitor::read_rollback_hdl(
    input string path,
    output uvm_hdl_data_t value);
    // uvm_hdl_read writes the referenced signal into the low bits.  Start the
    // unused upper bits at zero so X/Z checking reflects the signal itself.
    value = '0;
    if (!uvm_hdl_read(path, value)) begin
        `uvm_error("ROLLBACK_XMR", $sformatf("cannot read %s", path))
        return 1'b0;
    end
    if ($isunknown(value)) begin
        `uvm_error("ROLLBACK_XMR", $sformatf("X/Z sampled from %s", path))
        return 1'b0;
    end
    return 1'b1;
endfunction:read_rollback_hdl

function bit io_mem_to_ooo_ctrl_agent_agent_monitor::probe_rollback_hdl(
    input string path,
    output uvm_hdl_data_t value,
    output bit known);
    value = '0;
    known = 1'b0;
    if (!uvm_hdl_read(path, value)) begin
        return 1'b0;
    end
    known = !$isunknown(value);
    return 1'b1;
endfunction:probe_rollback_hdl

function void io_mem_to_ooo_ctrl_agent_agent_monitor::sample_rollback_diagnostics(
    ref memblock_sync_pkg::dispatch_raw_ctrl_t raw);
    int selected_idx;
    int selected_count;

    raw.rollback_diag_valid = 1'b1;
    raw.rollback_diag_complete = 1'b1;

    // All mirrors are sampled by the same clocking block as memoryViolation.
    // This keeps the arbiter proof in one simulator sampling region.
    for (int idx = 0; idx < 6; idx++) begin
        if (!$isunknown(this.vif.mon_mp.mon_cb.rollback_winner_vec[idx])) begin
            raw.rollback_winner_known[idx] = 1'b1;
            raw.rollback_winner[idx] =
                &this.vif.mon_mp.mon_cb.rollback_winner_vec[idx];
        end
    end

    selected_idx = -1;
    selected_count = 0;
    for (int idx = 0; idx < 6; idx++) begin
        if (raw.rollback_winner_known[idx] && raw.rollback_winner[idx]) begin
            selected_idx = idx;
            selected_count++;
        end
    end
    if (selected_count == 0) begin
        raw.rollback_diag_valid = 1'b0;
        raw.rollback_diag_complete = 1'b0;
        return;
    end
    if (selected_count != 1 || selected_idx < 0) begin
        raw.rollback_diag_complete = 1'b0;
        `uvm_error("ROLLBACK_XMR",
                   $sformatf("cannot prove unique rollback winner known=0x%0h winner=0x%0h",
                             raw.rollback_winner_known, raw.rollback_winner))
        return;
    end

    // Candidate valid remains optional for inactive non-winners.  The selected
    // lane and every known-active lane retain strict payload checks.
    for (int idx = 0; idx < 6; idx++) begin
        if (!$isunknown(this.vif.mon_mp.mon_cb.rollback_candidate_valid[idx])) begin
            raw.rollback_candidate_known[idx] = 1'b1;
            raw.rollback_candidate_valid[idx] =
                this.vif.mon_mp.mon_cb.rollback_candidate_valid[idx];
        end
    end
    if (!raw.rollback_candidate_known[selected_idx]) begin
        raw.rollback_diag_complete = 1'b0;
        `uvm_error("ROLLBACK_XMR",
                   $sformatf("selected rollback input %0d valid is X/Z at clocking sample",
                             selected_idx))
        return;
    end
    if (!raw.rollback_candidate_valid[selected_idx]) begin
        raw.rollback_diag_complete = 1'b0;
        `uvm_error("ROLLBACK_XMR",
                   $sformatf("selected rollback input %0d is not valid", selected_idx))
        return;
    end

    // Only candidates known active own meaningful ROB/level payload.  The
    // selected one is therefore always covered; known non-selected actives
    // remain visible for arbitration diagnostics.
    for (int idx = 0; idx < 6; idx++) begin
        if (!raw.rollback_candidate_known[idx] ||
            !raw.rollback_candidate_valid[idx]) begin
            continue;
        end

        if ($isunknown({
                this.vif.mon_mp.mon_cb.rollback_candidate_rob_flag[idx],
                this.vif.mon_mp.mon_cb.rollback_candidate_rob_value[idx],
                this.vif.mon_mp.mon_cb.rollback_candidate_level[idx]})) begin
            raw.rollback_diag_complete = 1'b0;
            `uvm_error("ROLLBACK_XMR",
                       $sformatf("active rollback input %0d payload is X/Z at clocking sample",
                                 idx))
            return;
        end
        raw.rollback_candidate_rob_flag[idx] =
            this.vif.mon_mp.mon_cb.rollback_candidate_rob_flag[idx];
        raw.rollback_candidate_rob_value[idx] =
            this.vif.mon_mp.mon_cb.rollback_candidate_rob_value[idx];
        raw.rollback_candidate_level[idx] =
            this.vif.mon_mp.mon_cb.rollback_candidate_level[idx];
    end

    if (selected_idx < 3) begin
        if ($isunknown({
                this.vif.mon_mp.mon_cb.rollback_lu_vp_match_fail[selected_idx],
                this.vif.mon_mp.mon_cb.rollback_lu_ldld_resp_valid[selected_idx],
                this.vif.mon_mp.mon_cb.rollback_lu_rep_frm_fetch[selected_idx],
                this.vif.mon_mp.mon_cb.rollback_lu_ldld_rep_inst[selected_idx],
                this.vif.mon_mp.mon_cb.rollback_lu_frm_mis_flush[selected_idx]})) begin
            raw.rollback_diag_complete = 1'b0;
            `uvm_error("ROLLBACK_XMR",
                       $sformatf("selected LoadUnit %0d reason is X/Z at clocking sample",
                                 selected_idx))
            return;
        end
        raw.rollback_lu_vp_match_fail[selected_idx] =
            this.vif.mon_mp.mon_cb.rollback_lu_vp_match_fail[selected_idx];
        raw.rollback_lu_ldld_resp_valid[selected_idx] =
            this.vif.mon_mp.mon_cb.rollback_lu_ldld_resp_valid[selected_idx];
        raw.rollback_lu_rep_frm_fetch[selected_idx] =
            this.vif.mon_mp.mon_cb.rollback_lu_rep_frm_fetch[selected_idx];
        raw.rollback_lu_ldld_rep_inst[selected_idx] =
            this.vif.mon_mp.mon_cb.rollback_lu_ldld_rep_inst[selected_idx];
        raw.rollback_lu_frm_mis_flush[selected_idx] =
            this.vif.mon_mp.mon_cb.rollback_lu_frm_mis_flush[selected_idx];
        raw.rollback_lu_reason_known[selected_idx] = 1'b1;
    end else if (selected_idx == 3) begin
        if ($isunknown(this.vif.mon_mp.mon_cb.rollback_uncache_can_allocate)) begin
            raw.rollback_diag_complete = 1'b0;
            `uvm_error("ROLLBACK_XMR",
                       "selected uncache resource state is X/Z at clocking sample")
            return;
        end
        raw.rollback_uncache_known = '1;
        raw.rollback_uncache_can_allocate =
            this.vif.mon_mp.mon_cb.rollback_uncache_can_allocate;
    end else begin
        for (int idx = 0; idx < 2; idx++) begin
            if ($isunknown(this.vif.mon_mp.mon_cb.rollback_raw_store_valid[idx])) begin
                continue;
            end
            raw.rollback_raw_store_known[idx] = 1'b1;
            raw.rollback_raw_store_valid[idx] =
                this.vif.mon_mp.mon_cb.rollback_raw_store_valid[idx];
            if (!raw.rollback_raw_store_valid[idx]) begin
                continue;
            end
            if ($isunknown({
                    this.vif.mon_mp.mon_cb.rollback_raw_store_rob_flag[idx],
                    this.vif.mon_mp.mon_cb.rollback_raw_store_rob_value[idx],
                    this.vif.mon_mp.mon_cb.rollback_raw_store_paddr[idx],
                    this.vif.mon_mp.mon_cb.rollback_raw_store_mask[idx]})) begin
                continue;
            end
            raw.rollback_raw_store_payload_known[idx] = 1'b1;
            raw.rollback_raw_store_rob_flag[idx] =
                this.vif.mon_mp.mon_cb.rollback_raw_store_rob_flag[idx];
            raw.rollback_raw_store_rob_value[idx] =
                this.vif.mon_mp.mon_cb.rollback_raw_store_rob_value[idx];
            raw.rollback_raw_store_paddr[idx] =
                this.vif.mon_mp.mon_cb.rollback_raw_store_paddr[idx];
            raw.rollback_raw_store_mask[idx] =
                this.vif.mon_mp.mon_cb.rollback_raw_store_mask[idx];
        end
    end
    raw.memory_violation_valid = 1'b1;
    raw.memory_violation_rob_valid = 1'b1;
    raw.memory_violation_rob_flag = raw.rollback_candidate_rob_flag[selected_idx];
    raw.memory_violation_rob_value = raw.rollback_candidate_rob_value[selected_idx];
    raw.memory_violation_level = raw.rollback_candidate_level[selected_idx];
endfunction:sample_rollback_diagnostics

function void io_mem_to_ooo_ctrl_agent_agent_monitor::log_uncache_resource_snapshot(
    input longint unsigned sample_seq,
    input bit rob_flag,
    input bit [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] rob_value);
    bit changed;
    int unsigned state_count[0:3];
    string slots;
    logic [15:0] lq_req_valid;
    logic [15:0][1:0] lq_state;
    logic [15:0] lq_rob_flag;
    logic [15:0][`MEMBLOCK_DUT_ROB_VALUE_W-1:0] lq_rob_value;
    logic [15:0] lq_slave_valid;
    logic [15:0][3:0] lq_slave_id;
    logic [15:0] lq_need_flush;
    logic [15:0] lq_need_flush_reg;
    logic [15:0] lq_flush;
    logic [15:0] lq_nc_out_valid;
    logic [15:0] lq_mmio_out_valid;
    logic [15:0] anchor_match_mask;
    logic [15:0] top_valid;
    logic [15:0] top_inflight;
    logic [15:0] top_wait_same;
    logic [15:0] top_wait_return;
    memblock_sync_pkg::memblock_lsq_pending_diag_t pending_diag;
    bit pending_diag_valid;

    // Empty RTL entries commonly leave payload registers X.  The lifecycle
    // snapshot is meaningful only after the corresponding valid bit is known
    // and asserted; inactive payload X/Z is deliberately masked to zero.
    lq_req_valid = this.vif.mon_mp.mon_cb.uncache_lq_req_valid;
    top_valid    = this.vif.mon_mp.mon_cb.uncache_top_valid;
    if ($isunknown(lq_req_valid) || $isunknown(top_valid)) begin
        `uvm_error("UNCACHE_PROGRESS",
                   "Uncache lifecycle valid mask contains X/Z")
        return;
    end

    lq_state          = '0;
    lq_rob_flag       = '0;
    lq_rob_value      = '0;
    lq_slave_valid    = '0;
    lq_slave_id      = '0;
    lq_need_flush     = '0;
    lq_need_flush_reg = '0;
    lq_flush          = '0;
    lq_nc_out_valid   = '0;
    lq_mmio_out_valid = '0;
    anchor_match_mask  = '0;
    pending_diag_valid = memblock_sync_pkg::peek_lsq_pending_diag(pending_diag);
    for (int idx = 0; idx < 16; idx++) begin
        if (lq_req_valid[idx] !== 1'b1) begin
            continue;
        end
        if ($isunknown({
                this.vif.mon_mp.mon_cb.uncache_lq_state[idx],
                this.vif.mon_mp.mon_cb.uncache_lq_rob_flag[idx],
                this.vif.mon_mp.mon_cb.uncache_lq_rob_value[idx],
                this.vif.mon_mp.mon_cb.uncache_lq_slave_valid[idx],
                this.vif.mon_mp.mon_cb.uncache_lq_need_flush[idx],
                this.vif.mon_mp.mon_cb.uncache_lq_need_flush_reg[idx],
                this.vif.mon_mp.mon_cb.uncache_lq_flush[idx],
                this.vif.mon_mp.mon_cb.uncache_lq_nc_out_valid[idx],
                this.vif.mon_mp.mon_cb.uncache_lq_mmio_out_valid[idx]})) begin
            `uvm_error("UNCACHE_PROGRESS",
                       $sformatf("active LQ slot %0d contains X/Z", idx))
            return;
        end
        if (this.vif.mon_mp.mon_cb.uncache_lq_slave_valid[idx] &&
            $isunknown(this.vif.mon_mp.mon_cb.uncache_lq_slave_id[idx])) begin
            `uvm_error("UNCACHE_PROGRESS",
                       $sformatf("active LQ slot %0d slaveId contains X/Z", idx))
            return;
        end
        lq_state[idx]          = this.vif.mon_mp.mon_cb.uncache_lq_state[idx];
        lq_rob_flag[idx]       = this.vif.mon_mp.mon_cb.uncache_lq_rob_flag[idx];
        lq_rob_value[idx]      = this.vif.mon_mp.mon_cb.uncache_lq_rob_value[idx];
        lq_slave_valid[idx]    = this.vif.mon_mp.mon_cb.uncache_lq_slave_valid[idx];
        lq_slave_id[idx]       = this.vif.mon_mp.mon_cb.uncache_lq_slave_id[idx];
        lq_need_flush[idx]     = this.vif.mon_mp.mon_cb.uncache_lq_need_flush[idx];
        lq_need_flush_reg[idx] = this.vif.mon_mp.mon_cb.uncache_lq_need_flush_reg[idx];
        lq_flush[idx]          = this.vif.mon_mp.mon_cb.uncache_lq_flush[idx];
        lq_nc_out_valid[idx]   = this.vif.mon_mp.mon_cb.uncache_lq_nc_out_valid[idx];
        lq_mmio_out_valid[idx] = this.vif.mon_mp.mon_cb.uncache_lq_mmio_out_valid[idx];
        anchor_match_mask[idx] =
            (lq_rob_flag[idx] == rob_flag) && (lq_rob_value[idx] == rob_value);
    end

    top_inflight   = '0;
    top_wait_same  = '0;
    top_wait_return = '0;
    for (int idx = 0; idx < 16; idx++) begin
        if (top_valid[idx] !== 1'b1) begin
            continue;
        end
        if ($isunknown({
                this.vif.mon_mp.mon_cb.uncache_top_inflight[idx],
                this.vif.mon_mp.mon_cb.uncache_top_wait_same[idx],
                this.vif.mon_mp.mon_cb.uncache_top_wait_return[idx]})) begin
            `uvm_error("UNCACHE_PROGRESS",
                       $sformatf("active top Uncache source %0d contains X/Z", idx))
            return;
        end
        top_inflight[idx]    = this.vif.mon_mp.mon_cb.uncache_top_inflight[idx];
        top_wait_same[idx]   = this.vif.mon_mp.mon_cb.uncache_top_wait_same[idx];
        top_wait_return[idx] = this.vif.mon_mp.mon_cb.uncache_top_wait_return[idx];
    end

    if ($isunknown(this.vif.mon_mp.mon_cb.uncache_lq_free_mask)) begin
        `uvm_error("UNCACHE_PROGRESS", "Uncache LQ free mask contains X/Z")
        return;
    end
    if ($isunknown(this.vif.mon_mp.mon_cb.uncache_top_lsq_id_resp_valid) ||
        $isunknown(this.vif.mon_mp.mon_cb.uncache_top_lsq_resp_valid) ||
        $isunknown(this.vif.mon_mp.mon_cb.uncache_top_a_valid) ||
        $isunknown(this.vif.mon_mp.mon_cb.uncache_top_d_valid)) begin
        `uvm_error("UNCACHE_PROGRESS", "Uncache handshake valid contains X/Z")
        return;
    end
    if (this.vif.mon_mp.mon_cb.uncache_top_lsq_id_resp_valid &&
        $isunknown({this.vif.mon_mp.mon_cb.uncache_top_lsq_id_resp_mid,
                   this.vif.mon_mp.mon_cb.uncache_top_lsq_id_resp_sid})) begin
        `uvm_error("UNCACHE_PROGRESS", "active idResp payload contains X/Z")
        return;
    end
    if (this.vif.mon_mp.mon_cb.uncache_top_lsq_resp_valid &&
        $isunknown({this.vif.mon_mp.mon_cb.uncache_top_lsq_resp_ready,
                   this.vif.mon_mp.mon_cb.uncache_top_lsq_resp_id})) begin
        `uvm_error("UNCACHE_PROGRESS", "active LSQ response payload contains X/Z")
        return;
    end
    if (this.vif.mon_mp.mon_cb.uncache_top_a_valid &&
        $isunknown({this.vif.mon_mp.mon_cb.uncache_top_a_ready,
                   this.vif.mon_mp.mon_cb.uncache_top_a_source})) begin
        `uvm_error("UNCACHE_PROGRESS", "active TL A payload contains X/Z")
        return;
    end
    if (this.vif.mon_mp.mon_cb.uncache_top_d_valid &&
        $isunknown(this.vif.mon_mp.mon_cb.uncache_top_d_source)) begin
        `uvm_error("UNCACHE_PROGRESS", "active TL D payload contains X/Z")
        return;
    end

    uncache_nack_diag_count++;
    changed = !uncache_snapshot_valid ||
              previous_uncache_lq_req_valid != lq_req_valid ||
              previous_uncache_lq_state != lq_state ||
              previous_uncache_lq_need_flush_reg != lq_need_flush_reg ||
              previous_uncache_top_valid != top_valid ||
              previous_uncache_top_inflight != top_inflight ||
              previous_uncache_top_wait_return != top_wait_return;
    if (!changed && (uncache_nack_diag_count % 256) != 1) begin
        return;
    end

    slots = "";
    foreach (state_count[idx]) begin
        state_count[idx] = 0;
    end
    for (int idx = 0; idx < 16; idx++) begin
        if (!lq_req_valid[idx]) begin
            continue;
        end
        state_count[lq_state[idx]]++;
        slots = $sformatf(
            "%s slot%0d=state%0d rob%0d/%0d sid=%s%0d flush=%0d/%0d nc/mmio=%0d/%0d;",
            slots,
            idx,
            lq_state[idx],
            lq_rob_flag[idx],
            lq_rob_value[idx],
            lq_slave_valid[idx] ? "" : "invalid/",
            lq_slave_id[idx],
            lq_need_flush[idx],
            lq_need_flush_reg[idx],
            lq_nc_out_valid[idx],
            lq_mmio_out_valid[idx]);
    end

    `uvm_info("UNCACHE_PROGRESS",
              $sformatf("nack=%0d sample=%0d rob=%0d/%0d anchor_match=0x%04h lq_valid=0x%04h lq_free_mask=0x%04h state_count=%0d/%0d/%0d/%0d pendingDiag=%0d/%0d ptr=%0d/%0d mmio/st=%0d/%0d commit=%0d batch/fault=%0d/%0d needFlush=0x%04h needFlushReg=0x%04h flush=0x%04h nc/mmio=0x%04h/0x%04h top_valid=0x%04h inflight=0x%04h waitSame=0x%04h waitReturn=0x%04h idResp=%0d/mid=%0d/sid=%0d lsqResp=%0d/%0d/id=%0d/fire=%0d A=%0d/%0d/fire=%0d/src=%0d D=%0d/src=%0d%s",
                        uncache_nack_diag_count,
                        sample_seq,
                        rob_flag,
                        rob_value,
                        anchor_match_mask,
                        lq_req_valid,
                        this.vif.mon_mp.mon_cb.uncache_lq_free_mask,
                        state_count[0], state_count[1],
                        state_count[2], state_count[3],
                        pending_diag_valid,
                        pending_diag_valid && (pending_diag.sample_seq == sample_seq),
                        pending_diag_valid ? pending_diag.pending_ptr_flag : 1'b0,
                        pending_diag_valid ? pending_diag.pending_ptr_value : '0,
                        pending_diag_valid ? pending_diag.pending_mmio_old : 1'b0,
                        pending_diag_valid ? pending_diag.pending_st : 1'b0,
                        pending_diag_valid ? pending_diag.commit : 1'b0,
                        pending_diag_valid ? pending_diag.has_commit : 1'b0,
                        pending_diag_valid ? pending_diag.has_fault_head : 1'b0,
                        lq_need_flush,
                        lq_need_flush_reg,
                        lq_flush,
                        lq_nc_out_valid,
                        lq_mmio_out_valid,
                        top_valid,
                        top_inflight,
                        top_wait_same,
                        top_wait_return,
                        this.vif.mon_mp.mon_cb.uncache_top_lsq_id_resp_valid,
                        this.vif.mon_mp.mon_cb.uncache_top_lsq_id_resp_mid,
                        this.vif.mon_mp.mon_cb.uncache_top_lsq_id_resp_sid,
                        this.vif.mon_mp.mon_cb.uncache_top_lsq_resp_valid,
                        this.vif.mon_mp.mon_cb.uncache_top_lsq_resp_ready,
                        this.vif.mon_mp.mon_cb.uncache_top_lsq_resp_id,
                        this.vif.mon_mp.mon_cb.uncache_top_lsq_resp_valid &&
                        this.vif.mon_mp.mon_cb.uncache_top_lsq_resp_ready,
                        this.vif.mon_mp.mon_cb.uncache_top_a_valid,
                        this.vif.mon_mp.mon_cb.uncache_top_a_ready,
                        this.vif.mon_mp.mon_cb.uncache_top_a_valid &&
                        this.vif.mon_mp.mon_cb.uncache_top_a_ready,
                        this.vif.mon_mp.mon_cb.uncache_top_a_source,
                        this.vif.mon_mp.mon_cb.uncache_top_d_valid,
                        this.vif.mon_mp.mon_cb.uncache_top_d_source,
                        slots),
              UVM_LOW)

    uncache_snapshot_valid = 1'b1;
    previous_uncache_lq_req_valid = lq_req_valid;
    previous_uncache_lq_state = lq_state;
    previous_uncache_lq_need_flush_reg = lq_need_flush_reg;
    previous_uncache_top_valid = top_valid;
    previous_uncache_top_inflight = top_inflight;
    previous_uncache_top_wait_return = top_wait_return;
endfunction:log_uncache_resource_snapshot

task io_mem_to_ooo_ctrl_agent_agent_monitor::run_phase(uvm_phase phase);
    super.run_phase(phase);
    this.mon_data();
endtask:run_phase

task io_mem_to_ooo_ctrl_agent_agent_monitor::mon_data();

    logic [5:0] io_mem_to_ooo_topToBackendBypass_hartId;
    logic io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip;
    logic io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip;
    logic io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip;
    logic io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip;
    logic io_mem_to_ooo_topToBackendBypass_externalInterrupt_debug;
    logic io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31;
    logic io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43;
    logic io_mem_to_ooo_topToBackendBypass_msiInfo_valid;
    logic [11:0] io_mem_to_ooo_topToBackendBypass_msiInfo_bits;
    logic io_mem_to_ooo_topToBackendBypass_clintTime_valid;
    logic [63:0] io_mem_to_ooo_topToBackendBypass_clintTime_bits;
    logic io_mem_to_ooo_topToBackendBypass_l2FlushDone;
    logic [`MEMBLOCK_LQ_CANCEL_COUNT_W-1:0] io_mem_to_ooo_lqCancelCnt;
    logic [`MEMBLOCK_SQ_CANCEL_COUNT_W-1:0] io_mem_to_ooo_sqCancelCnt;
    logic [`MEMBLOCK_SQ_DEQ_COUNT_W-1:0] io_mem_to_ooo_sqDeq;
    logic [3:0] io_mem_to_ooo_lqDeq    ;
    logic io_mem_to_ooo_lqDeqPtr_flag  ;
    logic [`MEMBLOCK_DUT_LQ_VALUE_W-1:0] io_mem_to_ooo_lqDeqPtr_value;
    logic io_mem_to_ooo_memoryViolation_valid;
    logic io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag;
    logic [`MEMBLOCK_DUT_FTQ_PTR_VALUE_W-1:0] io_mem_to_ooo_memoryViolation_bits_ftqIdx_value;
    logic [`MEMBLOCK_DUT_FTQ_OFFSET_W-1:0] io_mem_to_ooo_memoryViolation_bits_ftqOffset;
    logic io_mem_to_ooo_memoryViolation_bits_isRVC;
    logic io_mem_to_ooo_memoryViolation_bits_level;
    logic io_mem_to_ooo_memoryViolation_bits_robIdx_flag;
    logic [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_mem_to_ooo_memoryViolation_bits_robIdx_value;
    logic io_mem_to_ooo_sbIsEmpty      ;
    logic [63:0] io_mem_to_ooo_lsqio_vaddr;
    logic [63:0] io_mem_to_ooo_lsqio_gpaddr;
    logic io_mem_to_ooo_lsqio_isForVSnonLeafPTE;
    logic io_mem_to_ooo_ldCancel_0_ld2Cancel;
    logic io_mem_to_ooo_ldCancel_1_ld2Cancel;
    logic io_mem_to_ooo_ldCancel_2_ld2Cancel;

    logic [`MEMBLOCK_DUT_MMIO_LOAD_PORT_NUM-1:0] load_mmio_valid;
    logic [`MEMBLOCK_DUT_MMIO_LOAD_PORT_NUM-1:0][`MEMBLOCK_DUT_ROB_VALUE_W-1:0]
          load_mmio_rob_value;
    logic store_mmio_valid;
    logic [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] store_mmio_rob_value;
    logic sq_deq_ptr_valid;
    logic sq_deq_ptr_flag;
    logic [`MEMBLOCK_DUT_SQ_VALUE_W-1:0] sq_deq_ptr_value;
    io_mem_to_ooo_ctrl_agent_agent_xaction  mon_tr;
    memblock_sync_pkg::dispatch_raw_ctrl_t raw_ctrl;
    memblock_sync_pkg::memblock_control_level_observation_t sb_is_empty_observation;
    longint unsigned sample_seq;
    while(1) begin
        @this.vif.mon_mp.mon_cb;
        io_mem_to_ooo_topToBackendBypass_hartId = this.vif.mon_mp.mon_cb.io_mem_to_ooo_topToBackendBypass_hartId;
        io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip = this.vif.mon_mp.mon_cb.io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip;
        io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip = this.vif.mon_mp.mon_cb.io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip;
        io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip = this.vif.mon_mp.mon_cb.io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip;
        io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip = this.vif.mon_mp.mon_cb.io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip;
        io_mem_to_ooo_topToBackendBypass_externalInterrupt_debug = this.vif.mon_mp.mon_cb.io_mem_to_ooo_topToBackendBypass_externalInterrupt_debug;
        io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31;
        io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43;
        io_mem_to_ooo_topToBackendBypass_msiInfo_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_topToBackendBypass_msiInfo_valid;
        io_mem_to_ooo_topToBackendBypass_msiInfo_bits = this.vif.mon_mp.mon_cb.io_mem_to_ooo_topToBackendBypass_msiInfo_bits;
        io_mem_to_ooo_topToBackendBypass_clintTime_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_topToBackendBypass_clintTime_valid;
        io_mem_to_ooo_topToBackendBypass_clintTime_bits = this.vif.mon_mp.mon_cb.io_mem_to_ooo_topToBackendBypass_clintTime_bits;
        io_mem_to_ooo_topToBackendBypass_l2FlushDone = this.vif.mon_mp.mon_cb.io_mem_to_ooo_topToBackendBypass_l2FlushDone;
        io_mem_to_ooo_lqCancelCnt = this.vif.mon_mp.mon_cb.io_mem_to_ooo_lqCancelCnt;
        io_mem_to_ooo_sqCancelCnt = this.vif.mon_mp.mon_cb.io_mem_to_ooo_sqCancelCnt;
        io_mem_to_ooo_sqDeq = this.vif.mon_mp.mon_cb.io_mem_to_ooo_sqDeq;
        io_mem_to_ooo_lqDeq = this.vif.mon_mp.mon_cb.io_mem_to_ooo_lqDeq;
        io_mem_to_ooo_lqDeqPtr_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_lqDeqPtr_flag;
        io_mem_to_ooo_lqDeqPtr_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_lqDeqPtr_value;
        io_mem_to_ooo_memoryViolation_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_memoryViolation_valid;
        io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag;
        io_mem_to_ooo_memoryViolation_bits_ftqIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_memoryViolation_bits_ftqIdx_value;
        io_mem_to_ooo_memoryViolation_bits_ftqOffset = this.vif.mon_mp.mon_cb.io_mem_to_ooo_memoryViolation_bits_ftqOffset;
        io_mem_to_ooo_memoryViolation_bits_isRVC = this.vif.mon_mp.mon_cb.io_mem_to_ooo_memoryViolation_bits_isRVC;
        io_mem_to_ooo_memoryViolation_bits_level = this.vif.mon_mp.mon_cb.io_mem_to_ooo_memoryViolation_bits_level;
        io_mem_to_ooo_memoryViolation_bits_robIdx_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_memoryViolation_bits_robIdx_flag;
        io_mem_to_ooo_memoryViolation_bits_robIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_memoryViolation_bits_robIdx_value;
        io_mem_to_ooo_sbIsEmpty = this.vif.mon_mp.mon_cb.io_mem_to_ooo_sbIsEmpty;
        io_mem_to_ooo_lsqio_vaddr = this.vif.mon_mp.mon_cb.io_mem_to_ooo_lsqio_vaddr;
        io_mem_to_ooo_lsqio_gpaddr = this.vif.mon_mp.mon_cb.io_mem_to_ooo_lsqio_gpaddr;
        io_mem_to_ooo_lsqio_isForVSnonLeafPTE = this.vif.mon_mp.mon_cb.io_mem_to_ooo_lsqio_isForVSnonLeafPTE;
        io_mem_to_ooo_ldCancel_0_ld2Cancel = this.vif.mon_mp.mon_cb.io_mem_to_ooo_ldCancel_0_ld2Cancel;
        io_mem_to_ooo_ldCancel_1_ld2Cancel = this.vif.mon_mp.mon_cb.io_mem_to_ooo_ldCancel_1_ld2Cancel;
        io_mem_to_ooo_ldCancel_2_ld2Cancel = this.vif.mon_mp.mon_cb.io_mem_to_ooo_ldCancel_2_ld2Cancel;

        this.vif.sample_mmio_outputs(load_mmio_valid,
                                     load_mmio_rob_value,
                                     store_mmio_valid,
                                     store_mmio_rob_value);
        this.vif.sample_sq_deq_ptr(io_mem_to_ooo_sqDeq,
                                   sq_deq_ptr_valid,
                                   sq_deq_ptr_flag,
                                   sq_deq_ptr_value);

        if(this.cfg.xz_sw==tcnt_dec_base::ON && this.vif.rst_n==1'b1 && memblock_sync_pkg::reset_backend_done==1'b1) begin
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_topToBackendBypass_hartId,io_mem_to_ooo_topToBackendBypass_hartId,6);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip,io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip,io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip,io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip,io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_topToBackendBypass_externalInterrupt_debug,io_mem_to_ooo_topToBackendBypass_externalInterrupt_debug,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31,io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43,io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_topToBackendBypass_msiInfo_valid,io_mem_to_ooo_topToBackendBypass_msiInfo_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_topToBackendBypass_msiInfo_bits,io_mem_to_ooo_topToBackendBypass_msiInfo_bits,12);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_topToBackendBypass_clintTime_valid,io_mem_to_ooo_topToBackendBypass_clintTime_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_topToBackendBypass_clintTime_bits,io_mem_to_ooo_topToBackendBypass_clintTime_bits,64);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_topToBackendBypass_l2FlushDone,io_mem_to_ooo_topToBackendBypass_l2FlushDone,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_lqCancelCnt,io_mem_to_ooo_lqCancelCnt,`MEMBLOCK_LQ_CANCEL_COUNT_W);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_sqCancelCnt,io_mem_to_ooo_sqCancelCnt,`MEMBLOCK_SQ_CANCEL_COUNT_W);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_sqDeq,io_mem_to_ooo_sqDeq,`MEMBLOCK_SQ_DEQ_COUNT_W);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_lqDeq,io_mem_to_ooo_lqDeq,4);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_lqDeqPtr_flag,io_mem_to_ooo_lqDeqPtr_flag,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_lqDeqPtr_value,io_mem_to_ooo_lqDeqPtr_value,`MEMBLOCK_DUT_LQ_VALUE_W);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_memoryViolation_valid,io_mem_to_ooo_memoryViolation_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag,io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_memoryViolation_bits_ftqIdx_value,io_mem_to_ooo_memoryViolation_bits_ftqIdx_value,`MEMBLOCK_DUT_FTQ_PTR_VALUE_W);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_memoryViolation_bits_ftqOffset,io_mem_to_ooo_memoryViolation_bits_ftqOffset,`MEMBLOCK_DUT_FTQ_OFFSET_W);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_memoryViolation_bits_isRVC,io_mem_to_ooo_memoryViolation_bits_isRVC,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_memoryViolation_bits_level,io_mem_to_ooo_memoryViolation_bits_level,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_memoryViolation_bits_robIdx_flag,io_mem_to_ooo_memoryViolation_bits_robIdx_flag,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_memoryViolation_bits_robIdx_value,io_mem_to_ooo_memoryViolation_bits_robIdx_value,`MEMBLOCK_DUT_ROB_VALUE_W);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_sbIsEmpty,io_mem_to_ooo_sbIsEmpty,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_lsqio_vaddr,io_mem_to_ooo_lsqio_vaddr,64);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_lsqio_gpaddr,io_mem_to_ooo_lsqio_gpaddr,64);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_lsqio_isForVSnonLeafPTE,io_mem_to_ooo_lsqio_isForVSnonLeafPTE,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_ldCancel_0_ld2Cancel,io_mem_to_ooo_ldCancel_0_ld2Cancel,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_ldCancel_1_ld2Cancel,io_mem_to_ooo_ldCancel_1_ld2Cancel,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_ldCancel_2_ld2Cancel,io_mem_to_ooo_ldCancel_2_ld2Cancel,1);
            `TCNT_CHECK_SIG_XZ(load_mmio_valid,load_mmio_valid,`MEMBLOCK_DUT_MMIO_LOAD_PORT_NUM);
            `TCNT_CHECK_SIG_XZ(store_mmio_valid,store_mmio_valid,1);
            foreach (load_mmio_valid[port]) begin
                if (load_mmio_valid[port] === 1'b1) begin
                    `TCNT_CHECK_SIG_XZ(load_mmio_rob_value[port],load_mmio_rob_value[port],`MEMBLOCK_DUT_ROB_VALUE_W);
                end
            end
            if (store_mmio_valid === 1'b1) begin
                `TCNT_CHECK_SIG_XZ(store_mmio_rob_value,store_mmio_rob_value,`MEMBLOCK_DUT_ROB_VALUE_W);
            end
            if (sq_deq_ptr_valid === 1'b1) begin
                `TCNT_CHECK_SIG_XZ(sq_deq_ptr_flag,sq_deq_ptr_flag,1);
                `TCNT_CHECK_SIG_XZ(sq_deq_ptr_value,sq_deq_ptr_value,`MEMBLOCK_DUT_SQ_VALUE_W);
            end
        end
        if(this.vif.rst_n==1'b1 &&
           memblock_sync_pkg::reset_backend_done==1'b1 &&
           !memblock_sync_pkg::l2tlb_reset_active()) begin
            bit any_mmio_valid;
            bit control_reset_ack_sample;
            bit rollback_cache_conflicting;
            int unsigned control_reset_epoch;
            longint unsigned rollback_input_sample_seq;
            memblock_sync_pkg::dispatch_raw_ctrl_t rollback_input;
            memblock_sync_pkg::dispatch_raw_ctrl_t rollback_cached;

            // This monitor is a same-edge consumer/producer only. The CSR
            // monitor owns reset re-arm and advancement; ctrl data is only
            // anchored after the shared reset boundary has closed.
            memblock_sync_pkg::wait_for_l2tlb_sample_anchor($time, sample_seq);

            rollback_input = memblock_sync_pkg::make_empty_raw_ctrl();
            sample_rollback_diagnostics(rollback_input);
            if (rollback_input.rollback_diag_valid &&
                rollback_input.rollback_diag_complete) begin
                memblock_sync_pkg::cache_rollback_input_provenance(
                    rollback_input, sample_seq);
            end

            // 中文注释：首个 post-reset sample 只完成 ctrl monitor ack；ready 后的
            // 下一 sample 才发布 sbIsEmpty，使 sendover 不会消费 request 前的高电平。
            control_reset_ack_sample =
                memblock_sync_pkg::control_ctrl_monitor_reset_ack_needed(control_reset_epoch);
            if (control_reset_ack_sample) begin
                memblock_sync_pkg::ack_control_ctrl_monitor_reset(control_reset_epoch);
            end else begin
                memblock_sync_pkg::publish_control_sb_is_empty_observation(
                    io_mem_to_ooo_sbIsEmpty, sample_seq);
            end

            any_mmio_valid = store_mmio_valid === 1'b1;
            foreach (load_mmio_valid[port]) begin
                any_mmio_valid |= load_mmio_valid[port] === 1'b1;
            end
            if (memblock_sync_pkg::is_hard_xz_check_en() &&
                $isunknown(sq_deq_ptr_valid)) begin
                memblock_sync_pkg::report_hard_xz_error(
                    "CTRL_MONITOR", "sqDeq pointer-valid sampled as X/Z");
            end
            if (!`MEMBLOCK_DUT_HAS_SQ_DEQ_PTR && sq_deq_ptr_valid === 1'b1) begin
                `uvm_fatal("CTRL_MONITOR", "V2 count-only sqDeq unexpectedly sampled a pointer")
            end
            if (`MEMBLOCK_DUT_HAS_SQ_DEQ_PTR && io_mem_to_ooo_sqDeq != '0 &&
                sq_deq_ptr_valid === 1'b0) begin
                `uvm_fatal("CTRL_MONITOR", "pointer-capable sqDeq sample is missing its pointer")
            end
            begin
                memblock_sync_pkg::dispatch_raw_cancel_snapshot_t cancel_snapshot;

                if (io_mem_to_ooo_lqCancelCnt > `MEMBLOCK_DUT_LQ_SIZE ||
                    io_mem_to_ooo_sqCancelCnt > `MEMBLOCK_DUT_SQ_SIZE) begin
                    `uvm_fatal("CTRL_MONITOR",
                               $sformatf("DUT cancel count exceeds capacity lq=%0d/%0d sq=%0d/%0d",
                                         io_mem_to_ooo_lqCancelCnt, `MEMBLOCK_DUT_LQ_SIZE,
                                         io_mem_to_ooo_sqCancelCnt, `MEMBLOCK_DUT_SQ_SIZE))
                end

                // 中文伪代码：每个可见 DUT sample 都记录 held cancel level，零值也保留；
                // adapter/LSQ owner 只在对应 redirect target sample 消费一次。
                cancel_snapshot.lq_cancel_count = io_mem_to_ooo_lqCancelCnt;
                cancel_snapshot.sq_cancel_count = io_mem_to_ooo_sqCancelCnt;
                cancel_snapshot.sample_seq = sample_seq;
                cancel_snapshot.cycle = $time;
                memblock_sync_pkg::push_raw_cancel_snapshot(cancel_snapshot);
            end
            if (io_mem_to_ooo_lqDeq != '0 ||
                io_mem_to_ooo_sqDeq != '0 ||
                io_mem_to_ooo_memoryViolation_valid ||
                memblock_sync_pkg::dispatch_flushsb_waiting_empty ||
                any_mmio_valid) begin
                raw_ctrl = memblock_sync_pkg::make_empty_raw_ctrl();
                raw_ctrl.valid = 1'b1;
                raw_ctrl.lq_deq = io_mem_to_ooo_lqDeq;
                raw_ctrl.sq_deq = io_mem_to_ooo_sqDeq;
                raw_ctrl.lq_deq_ptr_flag = io_mem_to_ooo_lqDeqPtr_flag;
                raw_ctrl.lq_deq_ptr_value = io_mem_to_ooo_lqDeqPtr_value;
                raw_ctrl.sq_deq_ptr_valid = sq_deq_ptr_valid;
                raw_ctrl.sq_deq_ptr_flag = sq_deq_ptr_flag;
                raw_ctrl.sq_deq_ptr_value = sq_deq_ptr_value;
                foreach (load_mmio_valid[port]) begin
                    if (load_mmio_valid[port] === 1'b1) begin
                        raw_ctrl.load_mmio_valid[port] = 1'b1;
                        raw_ctrl.load_mmio_rob_value[port] = load_mmio_rob_value[port];
                    end
                end
                if (store_mmio_valid === 1'b1) begin
                    raw_ctrl.store_mmio_valid = 1'b1;
                    raw_ctrl.store_mmio_rob_value = store_mmio_rob_value;
                end
                if (any_mmio_valid) begin
                    raw_ctrl.mmio_flush_epoch = memblock_sync_pkg::dispatch_flush_epoch;
                    // 中文伪代码：MMIO valid 时把本次 monitor sample 的单调序号
                    // 固定到 raw；后续 adapter 不得用消费时刻的序号覆盖它。
                    raw_ctrl.mmio_sample_seq = sample_seq;
                end
                raw_ctrl.memory_violation_valid = io_mem_to_ooo_memoryViolation_valid;
                raw_ctrl.memory_violation_rob_valid = io_mem_to_ooo_memoryViolation_valid;
                raw_ctrl.memory_violation_rob_flag = io_mem_to_ooo_memoryViolation_bits_robIdx_flag;
                raw_ctrl.memory_violation_rob_value = io_mem_to_ooo_memoryViolation_bits_robIdx_value;
                raw_ctrl.memory_violation_ftq_flag = io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag;
                raw_ctrl.memory_violation_ftq_value = io_mem_to_ooo_memoryViolation_bits_ftqIdx_value;
                raw_ctrl.memory_violation_ftq_offset = io_mem_to_ooo_memoryViolation_bits_ftqOffset;
                raw_ctrl.memory_violation_is_rvc = io_mem_to_ooo_memoryViolation_bits_isRVC;
                raw_ctrl.memory_violation_level = io_mem_to_ooo_memoryViolation_bits_level;
                if (io_mem_to_ooo_memoryViolation_valid === 1'b1) begin
                    if (!memblock_sync_pkg::resolve_rollback_output_provenance(
                            io_mem_to_ooo_memoryViolation_bits_robIdx_flag,
                            io_mem_to_ooo_memoryViolation_bits_robIdx_value,
                            io_mem_to_ooo_memoryViolation_bits_level,
                            sample_seq,
                            rollback_cached,
                            rollback_input_sample_seq,
                            rollback_cache_conflicting)) begin
                        raw_ctrl.rollback_diag_valid = 1'b1;
                        raw_ctrl.rollback_diag_complete = 1'b0;
                        `uvm_error("ROLLBACK_XMR",
                                   $sformatf("cannot correlate rollback output rob=%0d/%0d level=%0d sample=%0d conflict=%0d",
                                             io_mem_to_ooo_memoryViolation_bits_robIdx_flag,
                                             io_mem_to_ooo_memoryViolation_bits_robIdx_value,
                                             io_mem_to_ooo_memoryViolation_bits_level,
                                             sample_seq,
                                             rollback_cache_conflicting))
                    end else begin
                        raw_ctrl.rollback_diag_valid = rollback_cached.rollback_diag_valid;
                        raw_ctrl.rollback_diag_complete = rollback_cached.rollback_diag_complete;
                        raw_ctrl.rollback_candidate_known = rollback_cached.rollback_candidate_known;
                        raw_ctrl.rollback_candidate_valid = rollback_cached.rollback_candidate_valid;
                        raw_ctrl.rollback_candidate_rob_flag = rollback_cached.rollback_candidate_rob_flag;
                        raw_ctrl.rollback_candidate_rob_value = rollback_cached.rollback_candidate_rob_value;
                        raw_ctrl.rollback_candidate_level = rollback_cached.rollback_candidate_level;
                        raw_ctrl.rollback_winner_known = rollback_cached.rollback_winner_known;
                        raw_ctrl.rollback_winner = rollback_cached.rollback_winner;
                        raw_ctrl.rollback_lu_reason_known = rollback_cached.rollback_lu_reason_known;
                        raw_ctrl.rollback_lu_vp_match_fail = rollback_cached.rollback_lu_vp_match_fail;
                        raw_ctrl.rollback_lu_ldld_resp_valid = rollback_cached.rollback_lu_ldld_resp_valid;
                        raw_ctrl.rollback_lu_rep_frm_fetch = rollback_cached.rollback_lu_rep_frm_fetch;
                        raw_ctrl.rollback_lu_ldld_rep_inst = rollback_cached.rollback_lu_ldld_rep_inst;
                        raw_ctrl.rollback_lu_frm_mis_flush = rollback_cached.rollback_lu_frm_mis_flush;
                        raw_ctrl.rollback_uncache_can_allocate = rollback_cached.rollback_uncache_can_allocate;
                        raw_ctrl.rollback_uncache_known = rollback_cached.rollback_uncache_known;
                        raw_ctrl.rollback_raw_store_known = rollback_cached.rollback_raw_store_known;
                        raw_ctrl.rollback_raw_store_valid = rollback_cached.rollback_raw_store_valid;
                        raw_ctrl.rollback_raw_store_payload_known = rollback_cached.rollback_raw_store_payload_known;
                        raw_ctrl.rollback_raw_store_rob_flag = rollback_cached.rollback_raw_store_rob_flag;
                        raw_ctrl.rollback_raw_store_rob_value = rollback_cached.rollback_raw_store_rob_value;
                        raw_ctrl.rollback_raw_store_paddr = rollback_cached.rollback_raw_store_paddr;
                        raw_ctrl.rollback_raw_store_mask = rollback_cached.rollback_raw_store_mask;
                        if (rollback_cached.rollback_winner_known[3] &&
                            rollback_cached.rollback_winner[3]) begin
                            log_uncache_resource_snapshot(
                                sample_seq,
                                io_mem_to_ooo_memoryViolation_bits_robIdx_flag,
                                io_mem_to_ooo_memoryViolation_bits_robIdx_value);
                        end
                        `uvm_info("ROLLBACK_XMR",
                                  $sformatf("correlate rollback input_sample=%0d output_sample=%0d age=%0d rob=%0d/%0d level=%0d",
                                            rollback_input_sample_seq,
                                            sample_seq,
                                            sample_seq - rollback_input_sample_seq,
                                            io_mem_to_ooo_memoryViolation_bits_robIdx_flag,
                                            io_mem_to_ooo_memoryViolation_bits_robIdx_value,
                                            io_mem_to_ooo_memoryViolation_bits_level),
                                  UVM_LOW)
                    end
                end
                raw_ctrl.sb_is_empty = io_mem_to_ooo_sbIsEmpty;
                if (!memblock_sync_pkg::get_latest_control_sb_is_empty_observation(
                        sb_is_empty_observation)) begin
                    if (memblock_sync_pkg::dispatch_flushsb_waiting_empty) begin
                        `uvm_fatal("CTRL_MONITOR",
                                   "ctrl raw is missing same-sample sbIsEmpty observation")
                    end
                end else begin
                    raw_ctrl.sb_is_empty_observation_seq =
                        sb_is_empty_observation.observation_seq;
                end
                raw_ctrl.cycle = $time;
                memblock_sync_pkg::push_raw_ctrl(raw_ctrl);
            end
        end else begin
            memblock_sync_pkg::clear_rollback_provenance_cache();
            uncache_snapshot_valid = 1'b0;
            uncache_nack_diag_count = 0;
        end
        //if(xxxTODOxxx==1'b1) begin
        //    mon_tr = io_mem_to_ooo_ctrl_agent_agent_xaction::type_id::create("mon_tr");
        //    mon_tr.io_mem_to_ooo_topToBackendBypass_hartId = io_mem_to_ooo_topToBackendBypass_hartId;
        //    mon_tr.io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip = io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip;
        //    mon_tr.io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip = io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip;
        //    mon_tr.io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip = io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip;
        //    mon_tr.io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip = io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip;
        //    mon_tr.io_mem_to_ooo_topToBackendBypass_externalInterrupt_debug = io_mem_to_ooo_topToBackendBypass_externalInterrupt_debug;
        //    mon_tr.io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31 = io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31;
        //    mon_tr.io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43 = io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43;
        //    mon_tr.io_mem_to_ooo_topToBackendBypass_msiInfo_valid = io_mem_to_ooo_topToBackendBypass_msiInfo_valid;
        //    mon_tr.io_mem_to_ooo_topToBackendBypass_msiInfo_bits = io_mem_to_ooo_topToBackendBypass_msiInfo_bits;
        //    mon_tr.io_mem_to_ooo_topToBackendBypass_clintTime_valid = io_mem_to_ooo_topToBackendBypass_clintTime_valid;
        //    mon_tr.io_mem_to_ooo_topToBackendBypass_clintTime_bits = io_mem_to_ooo_topToBackendBypass_clintTime_bits;
        //    mon_tr.io_mem_to_ooo_topToBackendBypass_l2FlushDone = io_mem_to_ooo_topToBackendBypass_l2FlushDone;
        //    mon_tr.io_mem_to_ooo_lqCancelCnt = io_mem_to_ooo_lqCancelCnt;
        //    mon_tr.io_mem_to_ooo_sqCancelCnt = io_mem_to_ooo_sqCancelCnt;
        //    mon_tr.io_mem_to_ooo_sqDeq = io_mem_to_ooo_sqDeq;
        //    mon_tr.io_mem_to_ooo_lqDeq = io_mem_to_ooo_lqDeq;
        //    mon_tr.io_mem_to_ooo_lqDeqPtr_flag = io_mem_to_ooo_lqDeqPtr_flag;
        //    mon_tr.io_mem_to_ooo_lqDeqPtr_value = io_mem_to_ooo_lqDeqPtr_value;
        //    mon_tr.io_mem_to_ooo_memoryViolation_valid = io_mem_to_ooo_memoryViolation_valid;
        //    mon_tr.io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag = io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag;
        //    mon_tr.io_mem_to_ooo_memoryViolation_bits_ftqIdx_value = io_mem_to_ooo_memoryViolation_bits_ftqIdx_value;
        //    mon_tr.io_mem_to_ooo_memoryViolation_bits_ftqOffset = io_mem_to_ooo_memoryViolation_bits_ftqOffset;
        //    mon_tr.io_mem_to_ooo_memoryViolation_bits_isRVC = io_mem_to_ooo_memoryViolation_bits_isRVC;
        //    mon_tr.io_mem_to_ooo_memoryViolation_bits_level = io_mem_to_ooo_memoryViolation_bits_level;
        //    mon_tr.io_mem_to_ooo_memoryViolation_bits_robIdx_flag = io_mem_to_ooo_memoryViolation_bits_robIdx_flag;
        //    mon_tr.io_mem_to_ooo_memoryViolation_bits_robIdx_value = io_mem_to_ooo_memoryViolation_bits_robIdx_value;
        //    mon_tr.io_mem_to_ooo_sbIsEmpty = io_mem_to_ooo_sbIsEmpty;
        //    mon_tr.io_mem_to_ooo_lsqio_vaddr = io_mem_to_ooo_lsqio_vaddr;
        //    mon_tr.io_mem_to_ooo_lsqio_gpaddr = io_mem_to_ooo_lsqio_gpaddr;
        //    mon_tr.io_mem_to_ooo_lsqio_isForVSnonLeafPTE = io_mem_to_ooo_lsqio_isForVSnonLeafPTE;
        //    mon_tr.io_mem_to_ooo_ldCancel_0_ld2Cancel = io_mem_to_ooo_ldCancel_0_ld2Cancel;
        //    mon_tr.io_mem_to_ooo_ldCancel_1_ld2Cancel = io_mem_to_ooo_ldCancel_1_ld2Cancel;
        //    mon_tr.io_mem_to_ooo_ldCancel_2_ld2Cancel = io_mem_to_ooo_ldCancel_2_ld2Cancel;

        //    mon_tr.channel_id = this.cfg.channel_id;
        //    mon_tr.unpack();
        //    this.mon_item_port.write(mon_tr);
        //end
    end
endtask:mon_data

`endif
