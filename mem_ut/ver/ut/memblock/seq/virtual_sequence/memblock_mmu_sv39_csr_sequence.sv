//=========================================================
//File name    : memblock_mmu_sv39_csr_sequence.sv
//Author       : OpenAI_Codex
//Module name  : memblock_mmu_sv39_csr_sequence
//Discribution : static V2 Sv39 CSR driver sequence
//Date         : 2026-08-27
//=========================================================
`ifndef MEMBLOCK_MMU_SV39_CSR_SEQUENCE__SV
`define MEMBLOCK_MMU_SV39_CSR_SEQUENCE__SV

// 中文注释：该 sequence 是 real-smoke 禁用 control worker topology 时 csr_ctrl_sqr 的唯一
// producer。它持续驱动完整 Scala reset baseline，只把 satp.MODE/PPN 改为 Sv39、
// 将当前 instruction/data privilege 显式切到 U 态，并使用配置物理基地址对应的
// root PPN；所有 changed pulse 保持为 0，不能和动态 CSR control worker 或 SFence
// producer 并发。
class memblock_mmu_sv39_csr_sequence extends uvm_sequence #(csr_ctrl_agent_agent_xaction);

    localparam bit [3:0] MEMBLOCK_SATP_MODE_SV39 = 4'd8;
    localparam bit [1:0] MEMBLOCK_PRIV_MODE_U = 2'd0;

    common_data_transaction data;

    `uvm_object_utils(memblock_mmu_sv39_csr_sequence)

    extern function new(string name = "memblock_mmu_sv39_csr_sequence");
    extern virtual task body();
    extern virtual task wait_for_main_table_or_stop();
    extern virtual function bit [43:0] get_sv39_root_ppn();
    extern virtual function void configure_static_sv39_xaction(
        input bit [43:0] root_ppn,
        input int unsigned item_index,
        output csr_ctrl_agent_agent_xaction tr
    );

endclass:memblock_mmu_sv39_csr_sequence

function memblock_mmu_sv39_csr_sequence::new(
    string name = "memblock_mmu_sv39_csr_sequence"
);
    super.new(name);
    data = null;
endfunction:new

task memblock_mmu_sv39_csr_sequence::body();
    bit [43:0] root_ppn;
    int unsigned item_index;
    csr_ctrl_agent_agent_xaction tr;

    seq_csr_common::init();
    data = common_data_transaction::get();
    if (data == null) begin
        `uvm_fatal(get_type_name(), "failed to get common_data_transaction")
    end
    if (memblock_sync_pkg::uses_control_barrier_topology()) begin
        `uvm_fatal(get_type_name(),
                   "static Sv39 CSR sequence cannot run with an active control worker topology")
    end

    // reset_all_tables() 会重置 mmu_csr_state；必须等主表初始化完成后再交付第一笔
    // static CSR，确保 CSR monitor 发布的 Sv39 snapshot 不会被建表 reset 覆盖。
    wait_for_main_table_or_stop();
    if (data.is_global_stop_requested()) begin
        return;
    end

    root_ppn = get_sv39_root_ppn();
    item_index = 0;
    while (!data.is_global_stop_requested()) begin
        configure_static_sv39_xaction(root_ppn, item_index, tr);
        start_item(tr);
        finish_item(tr);
        item_index++;
    end
endtask:body

task memblock_mmu_sv39_csr_sequence::wait_for_main_table_or_stop();
    int unsigned wait_count;

    wait_count = 0;
    while (!data.main_table_ready && !data.is_global_stop_requested()) begin
        if (wait_count != 0 && (wait_count % 5000) == 0) begin
            `uvm_warning(get_type_name(),
                         $sformatf("still waiting for main table before static Sv39 CSR drive: wait_count=%0d main_trans_num=%0d next_uid=%0d",
                                   wait_count,
                                   data.main_trans_num,
                                   data.next_uid))
        end
        #1;
        wait_count++;
    end
endtask:wait_for_main_table_or_stop

function bit [43:0] memblock_mmu_sv39_csr_sequence::get_sv39_root_ppn();
    bit [63:0] paddr_base;

    paddr_base = seq_csr_common::get_paddr_base();
    if (paddr_base[11:0] != '0) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("MEMBLOCK_PADDR_BASE=0x%0h must be 4KiB aligned for satp.ppn",
                             paddr_base))
    end
    if (paddr_base[63:56] != '0) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("MEMBLOCK_PADDR_BASE=0x%0h exceeds the 44-bit satp.ppn address range",
                             paddr_base))
    end
    return paddr_base[55:12];
endfunction:get_sv39_root_ppn

// 抽象职责：构造一笔稳定的 CSR payload。该函数不随机化、不产生 change pulse，
// 只把 Scala V2 reset 值映射到 csr_ctrl transaction，并覆盖 satp 的 Sv39 root 与
// 当前 instruction/data privilege 的 U 态选择。
function void memblock_mmu_sv39_csr_sequence::configure_static_sv39_xaction(
    input bit [43:0] root_ppn,
    input int unsigned item_index,
    output csr_ctrl_agent_agent_xaction tr
);
    tr = csr_ctrl_agent_agent_xaction::type_id::create(
        $sformatf("static_sv39_csr_%0d", item_index));
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "failed to create static Sv39 CSR xaction")
    end

    tr.pre_pkt_gap = 0;
    tr.post_pkt_gap = 0;

    tr.io_ooo_to_mem_tlbCsr_satp_mode = MEMBLOCK_SATP_MODE_SV39;
    tr.io_ooo_to_mem_tlbCsr_satp_asid = '0;
    tr.io_ooo_to_mem_tlbCsr_satp_ppn = root_ppn;
    tr.io_ooo_to_mem_tlbCsr_satp_changed = 1'b0;
    tr.io_ooo_to_mem_tlbCsr_vsatp_mode = '0;
    tr.io_ooo_to_mem_tlbCsr_vsatp_asid = '0;
    tr.io_ooo_to_mem_tlbCsr_vsatp_ppn = '0;
    tr.io_ooo_to_mem_tlbCsr_vsatp_changed = 1'b0;
    tr.io_ooo_to_mem_tlbCsr_hgatp_mode = '0;
    tr.io_ooo_to_mem_tlbCsr_hgatp_vmid = '0;
    tr.io_ooo_to_mem_tlbCsr_hgatp_ppn = '0;
    tr.io_ooo_to_mem_tlbCsr_hgatp_changed = 1'b0;
    tr.io_ooo_to_mem_tlbCsr_mbmc_BME = 1'b0;
    tr.io_ooo_to_mem_tlbCsr_mbmc_CMODE = 1'b0;
    tr.io_ooo_to_mem_tlbCsr_mbmc_BCLEAR = 1'b0;
    tr.io_ooo_to_mem_tlbCsr_mbmc_BMA = '0;

    tr.io_ooo_to_mem_tlbCsr_priv_mxr = 1'b0;
    tr.io_ooo_to_mem_tlbCsr_priv_sum = 1'b0;
    tr.io_ooo_to_mem_tlbCsr_priv_vmxr = 1'b0;
    tr.io_ooo_to_mem_tlbCsr_priv_vsum = 1'b0;
    tr.io_ooo_to_mem_tlbCsr_priv_virt = 1'b0;
    tr.io_ooo_to_mem_tlbCsr_priv_virt_changed = 1'b0;
    tr.io_ooo_to_mem_tlbCsr_priv_spvp = 1'b0;
    // 中文注释：U 态使 NewCSR 不再以 M 态优先输出 Bare，satp.Sv39 才能成为当前
    // instruction/data 地址翻译模式；其余 privilege 相关位仍保持 Scala reset 值。
    tr.io_ooo_to_mem_tlbCsr_priv_imode = MEMBLOCK_PRIV_MODE_U;
    tr.io_ooo_to_mem_tlbCsr_priv_dmode = MEMBLOCK_PRIV_MODE_U;
    tr.io_ooo_to_mem_tlbCsr_mPBMTE = 1'b0;
    tr.io_ooo_to_mem_tlbCsr_hPBMTE = 1'b0;
    tr.io_ooo_to_mem_tlbCsr_pmm_mseccfg = '0;
    tr.io_ooo_to_mem_tlbCsr_pmm_menvcfg = '0;
    tr.io_ooo_to_mem_tlbCsr_pmm_henvcfg = '0;
    tr.io_ooo_to_mem_tlbCsr_pmm_hstatus = '0;
    tr.io_ooo_to_mem_tlbCsr_pmm_senvcfg = '0;

    tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable = 1'b1;
    tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable = 1'b1;
    tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable = 1'b1;
    tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit = 1'b0;
    tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt = 1'b1;
    tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht = 1'b1;
    tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold = 4'd12;
    tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride = 6'd30;
    tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride = 1'b1;
    tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only = 1'b0;
    tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable = 1'b1;
    tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable = 1'b1;
    tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable = 1'b1;
    tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable = 1'b1;
    tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency = '0;
    tr.io_ooo_to_mem_csrCtrl_sbuffer_timeout = 22'h10_0000;
    tr.io_ooo_to_mem_csrCtrl_ldld_vio_check_enable = 1'b1;
    tr.io_ooo_to_mem_csrCtrl_cache_error_enable = 1'b1;
    tr.io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable = 1'b0;
    tr.io_ooo_to_mem_csrCtrl_power_down_enable = 1'b0;
    tr.io_ooo_to_mem_csrCtrl_flush_l2_enable = 1'b0;

    tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid = 1'b0;
    tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr = '0;
    tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data = '0;
    tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid = 1'b0;
    tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr = '0;
    tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType = '0;
    tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select = 1'b0;
    tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action = '0;
    tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain = 1'b0;
    tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2 = '0;
    tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0 = 1'b0;
    tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1 = 1'b0;
    tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2 = 1'b0;
    tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3 = 1'b0;
    tr.io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp = 1'b0;
    tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid = 1'b0;
    tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr = '0;
    tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType = '0;
    tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select = 1'b0;
    tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action = '0;
    tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain = 1'b0;
    tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store = 1'b0;
    tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load = 1'b0;
    tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2 = '0;
    tr.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0 = 1'b0;
    tr.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1 = 1'b0;
    tr.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2 = 1'b0;
    tr.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3 = 1'b0;
    tr.io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp = 1'b0;
    tr.io_ooo_to_mem_csrCtrl_fsIsOff = 1'b1;

    tr.io_ooo_to_mem_csrCtrl_bp_ctrl_btb_enable = 1'b1;
    tr.io_ooo_to_mem_csrCtrl_bp_ctrl_ras_enable = 1'b1;
    tr.io_ooo_to_mem_csrCtrl_bp_ctrl_sc_enable = 1'b1;
    tr.io_ooo_to_mem_csrCtrl_bp_ctrl_tage_enable = 1'b1;
    tr.io_ooo_to_mem_csrCtrl_bp_ctrl_ubtb_enable = 1'b1;
    tr.io_ooo_to_mem_csrCtrl_frontend_trigger_debugMode = 1'b0;
    tr.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable = 1'b1;
    tr.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable = 1'b1;
    tr.io_ooo_to_mem_csrCtrl_mem_trigger_debugMode = 1'b0;
    tr.io_ooo_to_mem_tlbCsr_priv_debug = 1'b0;

    tr.control_l2_flush_metadata_valid = 1'b0;
    tr.control_l2_flush_baseline_valid = 1'b0;
    tr.control_l2_flush_action_kind =
        csr_ctrl_agent_agent_xaction::CONTROL_L2_FLUSH_ACTION_NONE;
    tr.control_l2_flush_owner_uid = 0;
    tr.control_l2_flush_owner_dynamic_epoch = 0;
    tr.control_l2_flush_owner_action_generation = 0;
    tr.control_l2_flush_owner_kind_code = 0;
    tr.control_l2_flush_control_reset_epoch = 0;
endfunction:configure_static_sv39_xaction

`endif
