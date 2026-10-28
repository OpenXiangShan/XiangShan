//=========================================================
//File name    : memblock_l2tlb_pbmt_toggle_csr_sequence.sv
//Module name  : memblock_l2tlb_pbmt_toggle_csr_sequence
//Discribution : unique PBMT response-fault CSR producer
//=========================================================
`ifndef MEMBLOCK_L2TLB_PBMT_TOGGLE_CSR_SEQUENCE__SV
`define MEMBLOCK_L2TLB_PBMT_TOGGLE_CSR_SEQUENCE__SV

class memblock_l2tlb_pbmt_toggle_csr_sequence extends memblock_mmu_sv39_csr_sequence;
    `uvm_object_utils(memblock_l2tlb_pbmt_toggle_csr_sequence)

    extern function new(string name = "memblock_l2tlb_pbmt_toggle_csr_sequence");
    extern virtual task body();
    extern virtual function bit uid_terminal_retired(input memblock_uid_t uid);
    extern virtual function void configure_toggle_xaction(
        input bit [43:0] root_ppn,
        input int unsigned item_index,
        input bit m_pbmt_en,
        output csr_ctrl_agent_agent_xaction tr);
endclass:memblock_l2tlb_pbmt_toggle_csr_sequence

function memblock_l2tlb_pbmt_toggle_csr_sequence::new(
    string name = "memblock_l2tlb_pbmt_toggle_csr_sequence");
    super.new(name);
endfunction:new

task memblock_l2tlb_pbmt_toggle_csr_sequence::body();
    bit [43:0] root_ppn;
    int unsigned item_index;
    int unsigned enable_baseline_seq;
    int unsigned latest_seq;
    int unsigned terminal_wait_count;
    bit enabled;
    bit b_terminal_published;
    csr_ctrl_agent_agent_xaction tr;
    memblock_sync_pkg::dispatch_raw_csr_t latest_csr;

    seq_csr_common::init();
    data = common_data_transaction::get();
    if (data == null || memblock_sync_pkg::uses_control_barrier_topology()) begin
        `uvm_fatal(get_type_name(), "PBMT RF CSR producer has invalid context/topology")
        return;
    end
    wait_for_main_table_or_stop();
    if (data.is_global_stop_requested()) begin
        return;
    end
    while (!data.is_global_stop_requested() &&
           memblock_sync_pkg::get_pbmt_rf_phase() == memblock_sync_pkg::PBMT_RF_RESET) begin
        #1;
    end
    if (data.is_global_stop_requested()) begin
        return;
    end
    if (memblock_sync_pkg::get_pbmt_rf_phase() !=
            memblock_sync_pkg::PBMT_RF_ANCHOR_PUBLISHED) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("PBMT RF CSR bootstrap entered at invalid phase=%0d",
                             memblock_sync_pkg::get_pbmt_rf_phase()))
        return;
    end

    root_ppn = get_sv39_root_ppn();
    item_index = 0;
    enabled = 1'b0;
    b_terminal_published = 1'b0;
    enable_baseline_seq = 0;
    terminal_wait_count = 0;
    while (!data.is_global_stop_requested()) begin
        if (!b_terminal_published &&
            memblock_sync_pkg::get_pbmt_rf_phase() == memblock_sync_pkg::PBMT_RF_B_COMPLETED &&
            uid_terminal_retired(0)) begin
            if (!memblock_sync_pkg::publish_pbmt_rf_phase(
                    memblock_sync_pkg::PBMT_RF_B_COMPLETED,
                    memblock_sync_pkg::PBMT_RF_B_TERMINAL, this)) return;
            if (memblock_sync_pkg::get_latest_runtime_csr_snapshot(
                    latest_csr, enable_baseline_seq)) begin
                // sequence number is the only required pre-enable watermark.
            end else begin
                enable_baseline_seq = 0;
            end
            enabled = 1'b1;
            b_terminal_published = 1'b1;
        end

        configure_toggle_xaction(root_ppn, item_index, enabled, tr);
        start_item(tr);
        finish_item(tr);
        item_index++;
        // Keep scenario polling time-consuming even if a driver implementation
        // completes an item in the same delta cycle.
        #1;

        if (enabled &&
            memblock_sync_pkg::get_pbmt_rf_phase() == memblock_sync_pkg::PBMT_RF_B_TERMINAL &&
            memblock_sync_pkg::get_latest_runtime_csr_snapshot(
                latest_csr, latest_seq) && latest_seq > enable_baseline_seq &&
            latest_csr.valid && latest_csr.m_pbmt_en && !latest_csr.h_pbmt_en) begin
            if (!memblock_sync_pkg::publish_pbmt_rf_phase(
                    memblock_sync_pkg::PBMT_RF_B_TERMINAL,
                    memblock_sync_pkg::PBMT_RF_C_ENABLE_MONITORED,
                    this)) return;
        end
    end

    // Global stop can become visible before the main-sequence owner returns
    // from responder drain and publishes C_TERMINAL.  Stop producing CSR items,
    // but keep this child alive for that one-way phase acknowledgement.
    while (memblock_sync_pkg::get_pbmt_rf_phase() ==
               memblock_sync_pkg::PBMT_RF_C_COMPLETED &&
           terminal_wait_count <=
               seq_csr_common::get_active_seq_no_progress_warn_cycles()) begin
        #1;
        terminal_wait_count++;
    end
    if (memblock_sync_pkg::get_pbmt_rf_phase() < memblock_sync_pkg::PBMT_RF_C_TERMINAL) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("PBMT RF CSR producer stopped early phase=%0d items=%0d terminal_wait=%0d",
                             memblock_sync_pkg::get_pbmt_rf_phase(), item_index,
                             terminal_wait_count))
        return;
    end
endtask:body

function bit memblock_l2tlb_pbmt_toggle_csr_sequence::uid_terminal_retired(
    input memblock_uid_t uid);
    return data != null && data.is_valid_uid(uid) &&
           data.get_status(uid).terminal_done;
endfunction:uid_terminal_retired

function void memblock_l2tlb_pbmt_toggle_csr_sequence::configure_toggle_xaction(
    input bit [43:0] root_ppn,
    input int unsigned item_index,
    input bit m_pbmt_en,
    output csr_ctrl_agent_agent_xaction tr);
    configure_static_sv39_xaction(root_ppn, item_index, tr);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "failed to build PBMT RF CSR xaction")
        return;
    end
    // Item0/1 are the two monitored PMP bootstrap writes.  Starting with item2,
    // bypass the common 64-item Bare settle and hold a stable Sv39/U baseline;
    // B readiness still requires the responder to consume that exact history.
    tr.io_ooo_to_mem_tlbCsr_satp_mode =
        item_index < 2 ? MEMBLOCK_SATP_MODE_BARE : MEMBLOCK_SATP_MODE_SV39;
    tr.io_ooo_to_mem_tlbCsr_priv_imode = MEMBLOCK_PRIV_MODE_U;
    tr.io_ooo_to_mem_tlbCsr_priv_dmode = MEMBLOCK_PRIV_MODE_U;
    tr.io_ooo_to_mem_tlbCsr_mPBMTE = m_pbmt_en;
    tr.io_ooo_to_mem_tlbCsr_hPBMTE = 1'b0;
    tr.io_ooo_to_mem_tlbCsr_satp_changed = 1'b0;
    tr.io_ooo_to_mem_tlbCsr_vsatp_changed = 1'b0;
    tr.io_ooo_to_mem_tlbCsr_hgatp_changed = 1'b0;
    tr.io_ooo_to_mem_tlbCsr_priv_virt_changed = 1'b0;
    case (item_index)
        0: begin
            tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid = 1'b1;
            tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr =
                memblock_sync_pkg::PBMT_RF_PMPADDR0_CSR_ADDR;
            tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data =
                memblock_sync_pkg::PBMT_RF_PMPADDR0_TOR_TOP;
        end
        1: begin
            tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid = 1'b1;
            tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr =
                memblock_sync_pkg::PBMT_RF_PMPCFG0_CSR_ADDR;
            tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data =
                memblock_sync_pkg::PBMT_RF_PMPCFG0_TOR_RWX;
        end
        default: begin
            tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid = 1'b0;
            tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr = '0;
            tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data = '0;
        end
    endcase
endfunction:configure_toggle_xaction

`endif
