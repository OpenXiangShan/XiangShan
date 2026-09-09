//=========================================================
// CSR configuration state and common transaction builders
//=========================================================
`ifndef MEMBLOCK_CSR_CONFIG_STATE__SV
`define MEMBLOCK_CSR_CONFIG_STATE__SV

class memblock_csr_config_state extends uvm_object;
    localparam bit [3:0] SATP_SV39 = 4'd8;
    localparam bit [3:0] SATP_SV48 = 4'd9;
    localparam bit [1:0] PRIV_U = 2'b00;
    localparam bit [1:0] PRIV_S = 2'b01;
    localparam bit [1:0] PRIV_M = 2'b11;

    `uvm_object_utils(memblock_csr_config_state)

    function new(string name = "memblock_csr_config_state");
        super.new(name);
    endfunction:new

    // 中文注释：函数 A 显式写入 transaction 的全部 92 个 DUT 字段和 TP 兼容字段；
    // agent transaction 的 rand/default constraint 不得充当隐式初值来源。
    static function void configure_static_defaults(input csr_ctrl_agent_agent_xaction tr);
        if (tr == null) `uvm_fatal("CSR_CONFIG", "static builder got null transaction")
        tr.io_ooo_to_mem_tlbCsr_satp_mode = 4'h0;
        tr.io_ooo_to_mem_tlbCsr_satp_asid = '0;
        tr.io_ooo_to_mem_tlbCsr_satp_ppn = '0;
        tr.io_ooo_to_mem_tlbCsr_satp_changed = 1'b0;
        tr.io_ooo_to_mem_tlbCsr_vsatp_mode = 4'h0;
        tr.io_ooo_to_mem_tlbCsr_vsatp_asid = '0;
        tr.io_ooo_to_mem_tlbCsr_vsatp_ppn = '0;
        tr.io_ooo_to_mem_tlbCsr_vsatp_changed = 1'b0;
        tr.io_ooo_to_mem_tlbCsr_hgatp_mode = 4'h0;
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
        tr.io_ooo_to_mem_tlbCsr_priv_imode = PRIV_U;
        tr.io_ooo_to_mem_tlbCsr_priv_dmode = PRIV_U;
        tr.io_ooo_to_mem_tlbCsr_mPBMTE = 1'b0;
        tr.io_ooo_to_mem_tlbCsr_hPBMTE = 1'b0;
        tr.io_ooo_to_mem_tlbCsr_pmm_mseccfg = '0;
        tr.io_ooo_to_mem_tlbCsr_pmm_menvcfg = '0;
        tr.io_ooo_to_mem_tlbCsr_pmm_henvcfg = '0;
        tr.io_ooo_to_mem_tlbCsr_pmm_hstatus = '0;
        tr.io_ooo_to_mem_tlbCsr_pmm_senvcfg = '0;

        // 中文注释：配置指南定义的正常功能 enable 基线；函数 B 只在这 13 项的
        // allowlist 内按 plus 权重求解一次，dynamic sequence 随后始终复制该快照。
        tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable = 1'b0;
        tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable = 1'b1;
        tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable = 1'b1;
        tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit = 1'b0;
        tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt = 1'b1;
        tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht = 1'b1;
        tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold = 4'hc;
        tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride = 6'h1e;
        tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride = 1'b1;
        tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only = 1'b0;
        tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable = 1'b1;
        tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable = 1'b1;
        tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable = 1'b1;
        tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable = 1'b0;
        tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency = '0;
        tr.io_ooo_to_mem_csrCtrl_sbuffer_timeout = 22'h10_0000;
        tr.io_ooo_to_mem_csrCtrl_ldld_vio_check_enable = 1'b1;
        tr.io_ooo_to_mem_csrCtrl_cache_error_enable = 1'b1;
        tr.io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable = 1'b1;
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
        tr.io_ooo_to_mem_csrCtrl_fsIsOff = 1'b0;
        tr.io_ooo_to_mem_csrCtrl_bp_ctrl_btb_enable = 1'b0;
        tr.io_ooo_to_mem_csrCtrl_bp_ctrl_ras_enable = 1'b0;
        tr.io_ooo_to_mem_csrCtrl_bp_ctrl_sc_enable = 1'b0;
        tr.io_ooo_to_mem_csrCtrl_bp_ctrl_tage_enable = 1'b0;
        tr.io_ooo_to_mem_csrCtrl_bp_ctrl_ubtb_enable = 1'b0;
        tr.io_ooo_to_mem_csrCtrl_frontend_trigger_debugMode = 1'b0;
        tr.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable = 1'b1;
        tr.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable = 1'b1;
        tr.io_ooo_to_mem_csrCtrl_mem_trigger_debugMode = 1'b0;
        tr.io_ooo_to_mem_tlbCsr_priv_debug = 1'b0;
        clear_protocol_metadata(tr);
    endfunction:configure_static_defaults

    static function void clear_protocol_metadata(input csr_ctrl_agent_agent_xaction tr);
        tr.pre_pkt_gap = 0;
        tr.post_pkt_gap = 0;
        tr.io_ooo_to_mem_tlbCsr_satp_changed = 1'b0;
        tr.io_ooo_to_mem_tlbCsr_vsatp_changed = 1'b0;
        tr.io_ooo_to_mem_tlbCsr_hgatp_changed = 1'b0;
        tr.io_ooo_to_mem_tlbCsr_priv_virt_changed = 1'b0;
        tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid = 1'b0;
        tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr = '0;
        tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data = '0;
        tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid = 1'b0;
        tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid = 1'b0;
        tr.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0 = 1'b0;
        tr.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1 = 1'b0;
        tr.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2 = 1'b0;
        tr.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3 = 1'b0;
        tr.io_ooo_to_mem_csrCtrl_flush_l2_enable = 1'b0;
        tr.io_ooo_to_mem_csrCtrl_power_down_enable = 1'b0;
        tr.io_ooo_to_mem_tlbCsr_mbmc_BCLEAR = 1'b0;
        tr.control_l2_flush_metadata_valid = 1'b0;
        tr.control_l2_flush_baseline_valid = 1'b0;
        tr.control_l2_flush_action_kind = csr_ctrl_agent_agent_xaction::CONTROL_L2_FLUSH_ACTION_NONE;
        tr.control_l2_flush_owner_uid = 0;
        tr.control_l2_flush_owner_dynamic_epoch = 0;
        tr.control_l2_flush_owner_action_generation = 0;
        tr.control_l2_flush_owner_kind_code = 0;
        tr.control_l2_flush_control_reset_epoch = 0;
    endfunction:clear_protocol_metadata

    static function void apply_fixed_roots(input csr_ctrl_agent_agent_xaction tr);
        if (tr.io_ooo_to_mem_tlbCsr_satp_mode == 4'h0) tr.io_ooo_to_mem_tlbCsr_satp_ppn = '0;
        else tr.io_ooo_to_mem_tlbCsr_satp_ppn = 44'h000_0008_0000;
        if (tr.io_ooo_to_mem_tlbCsr_vsatp_mode == 4'h0) tr.io_ooo_to_mem_tlbCsr_vsatp_ppn = '0;
        else tr.io_ooo_to_mem_tlbCsr_vsatp_ppn = 44'h000_0008_0004;
        if (tr.io_ooo_to_mem_tlbCsr_hgatp_mode == 4'h0) tr.io_ooo_to_mem_tlbCsr_hgatp_ppn = '0;
        else tr.io_ooo_to_mem_tlbCsr_hgatp_ppn = 44'h000_0008_0008;
        if (tr.io_ooo_to_mem_tlbCsr_satp_mode == 4'h0) tr.io_ooo_to_mem_tlbCsr_satp_asid = '0;
        if (tr.io_ooo_to_mem_tlbCsr_vsatp_mode == 4'h0) tr.io_ooo_to_mem_tlbCsr_vsatp_asid = '0;
        if (tr.io_ooo_to_mem_tlbCsr_hgatp_mode == 4'h0) tr.io_ooo_to_mem_tlbCsr_hgatp_vmid = '0;
        tr.io_ooo_to_mem_tlbCsr_hgatp_vmid[15:14] = 2'b00;
    endfunction:apply_fixed_roots

    static function bit differs_from(
        input csr_ctrl_agent_agent_xaction lhs,
        input csr_ctrl_agent_agent_xaction rhs
    );
        if (lhs == null || rhs == null) return 1'b0;
        return lhs.io_ooo_to_mem_tlbCsr_satp_mode != rhs.io_ooo_to_mem_tlbCsr_satp_mode ||
               lhs.io_ooo_to_mem_tlbCsr_satp_asid != rhs.io_ooo_to_mem_tlbCsr_satp_asid ||
               lhs.io_ooo_to_mem_tlbCsr_vsatp_mode != rhs.io_ooo_to_mem_tlbCsr_vsatp_mode ||
               lhs.io_ooo_to_mem_tlbCsr_vsatp_asid != rhs.io_ooo_to_mem_tlbCsr_vsatp_asid ||
               lhs.io_ooo_to_mem_tlbCsr_hgatp_mode != rhs.io_ooo_to_mem_tlbCsr_hgatp_mode ||
               lhs.io_ooo_to_mem_tlbCsr_hgatp_vmid != rhs.io_ooo_to_mem_tlbCsr_hgatp_vmid ||
               lhs.io_ooo_to_mem_tlbCsr_priv_mxr != rhs.io_ooo_to_mem_tlbCsr_priv_mxr ||
               lhs.io_ooo_to_mem_tlbCsr_priv_sum != rhs.io_ooo_to_mem_tlbCsr_priv_sum ||
               lhs.io_ooo_to_mem_tlbCsr_priv_vmxr != rhs.io_ooo_to_mem_tlbCsr_priv_vmxr ||
               lhs.io_ooo_to_mem_tlbCsr_priv_vsum != rhs.io_ooo_to_mem_tlbCsr_priv_vsum ||
               lhs.io_ooo_to_mem_tlbCsr_priv_virt != rhs.io_ooo_to_mem_tlbCsr_priv_virt ||
               lhs.io_ooo_to_mem_tlbCsr_priv_imode != rhs.io_ooo_to_mem_tlbCsr_priv_imode ||
               lhs.io_ooo_to_mem_tlbCsr_priv_dmode != rhs.io_ooo_to_mem_tlbCsr_priv_dmode ||
               lhs.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable != rhs.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable ||
               lhs.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable != rhs.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable;
    endfunction:differs_from

    static function bit [memblock_sync_pkg::MEMBLOCK_CSR_FULL_PAYLOAD_BITS-1:0]
        pack_full_payload(input csr_ctrl_agent_agent_xaction tr);
        if (tr == null) `uvm_fatal("CSR_CONFIG", "full payload pack got null transaction")
        return {
            tr.io_ooo_to_mem_tlbCsr_satp_mode,
            tr.io_ooo_to_mem_tlbCsr_satp_asid,
            tr.io_ooo_to_mem_tlbCsr_satp_ppn,
            tr.io_ooo_to_mem_tlbCsr_satp_changed,
            tr.io_ooo_to_mem_tlbCsr_vsatp_mode,
            tr.io_ooo_to_mem_tlbCsr_vsatp_asid,
            tr.io_ooo_to_mem_tlbCsr_vsatp_ppn,
            tr.io_ooo_to_mem_tlbCsr_vsatp_changed,
            tr.io_ooo_to_mem_tlbCsr_hgatp_mode,
            tr.io_ooo_to_mem_tlbCsr_hgatp_vmid,
            tr.io_ooo_to_mem_tlbCsr_hgatp_ppn,
            tr.io_ooo_to_mem_tlbCsr_hgatp_changed,
            tr.io_ooo_to_mem_tlbCsr_mbmc_BME,
            tr.io_ooo_to_mem_tlbCsr_mbmc_CMODE,
            tr.io_ooo_to_mem_tlbCsr_mbmc_BCLEAR,
            tr.io_ooo_to_mem_tlbCsr_mbmc_BMA,
            tr.io_ooo_to_mem_tlbCsr_priv_mxr,
            tr.io_ooo_to_mem_tlbCsr_priv_sum,
            tr.io_ooo_to_mem_tlbCsr_priv_vmxr,
            tr.io_ooo_to_mem_tlbCsr_priv_vsum,
            tr.io_ooo_to_mem_tlbCsr_priv_virt,
            tr.io_ooo_to_mem_tlbCsr_priv_virt_changed,
            tr.io_ooo_to_mem_tlbCsr_priv_spvp,
            tr.io_ooo_to_mem_tlbCsr_priv_imode,
            tr.io_ooo_to_mem_tlbCsr_priv_dmode,
            tr.io_ooo_to_mem_tlbCsr_mPBMTE,
            tr.io_ooo_to_mem_tlbCsr_hPBMTE,
            tr.io_ooo_to_mem_tlbCsr_pmm_mseccfg,
            tr.io_ooo_to_mem_tlbCsr_pmm_menvcfg,
            tr.io_ooo_to_mem_tlbCsr_pmm_henvcfg,
            tr.io_ooo_to_mem_tlbCsr_pmm_hstatus,
            tr.io_ooo_to_mem_tlbCsr_pmm_senvcfg,
            tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable,
            tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable,
            tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable,
            tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit,
            tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt,
            tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht,
            tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold,
            tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride,
            tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride,
            tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only,
            tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable,
            tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable,
            tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable,
            tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency,
            tr.io_ooo_to_mem_csrCtrl_sbuffer_timeout,
            tr.io_ooo_to_mem_csrCtrl_ldld_vio_check_enable,
            tr.io_ooo_to_mem_csrCtrl_cache_error_enable,
            tr.io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable,
            tr.io_ooo_to_mem_csrCtrl_power_down_enable,
            tr.io_ooo_to_mem_csrCtrl_flush_l2_enable,
            tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid,
            tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr,
            tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data,
            tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid,
            tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr,
            tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType,
            tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select,
            tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action,
            tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain,
            tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2,
            tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0,
            tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1,
            tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2,
            tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3,
            tr.io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp,
            tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid,
            tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr,
            tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType,
            tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select,
            tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action,
            tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain,
            tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store,
            tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load,
            tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2,
            tr.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0,
            tr.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1,
            tr.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2,
            tr.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3,
            tr.io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp,
            tr.io_ooo_to_mem_csrCtrl_fsIsOff,
            tr.io_ooo_to_mem_csrCtrl_bp_ctrl_btb_enable,
            tr.io_ooo_to_mem_csrCtrl_bp_ctrl_ras_enable,
            tr.io_ooo_to_mem_csrCtrl_bp_ctrl_sc_enable,
            tr.io_ooo_to_mem_csrCtrl_bp_ctrl_tage_enable,
            tr.io_ooo_to_mem_csrCtrl_bp_ctrl_ubtb_enable,
            tr.io_ooo_to_mem_csrCtrl_frontend_trigger_debugMode,
            tr.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable,
            tr.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable,
            tr.io_ooo_to_mem_csrCtrl_mem_trigger_debugMode,
            tr.io_ooo_to_mem_tlbCsr_priv_debug
        };
    endfunction:pack_full_payload

    static function memblock_sync_pkg::dispatch_raw_csr_t make_runtime_payload(
        input csr_ctrl_agent_agent_xaction tr
    );
        memblock_sync_pkg::dispatch_raw_csr_t raw;
        if (tr == null) `uvm_fatal("CSR_CONFIG", "runtime payload builder got null transaction")
        raw = memblock_sync_pkg::make_empty_raw_csr();
        raw.valid = 1'b1;
        raw.satp_mode = tr.io_ooo_to_mem_tlbCsr_satp_mode;
        raw.satp_asid = tr.io_ooo_to_mem_tlbCsr_satp_asid;
        raw.satp_ppn = tr.io_ooo_to_mem_tlbCsr_satp_ppn;
        raw.satp_changed = tr.io_ooo_to_mem_tlbCsr_satp_changed;
        raw.vsatp_mode = tr.io_ooo_to_mem_tlbCsr_vsatp_mode;
        raw.vsatp_asid = tr.io_ooo_to_mem_tlbCsr_vsatp_asid;
        raw.vsatp_ppn = tr.io_ooo_to_mem_tlbCsr_vsatp_ppn;
        raw.vsatp_changed = tr.io_ooo_to_mem_tlbCsr_vsatp_changed;
        raw.hgatp_mode = tr.io_ooo_to_mem_tlbCsr_hgatp_mode;
        raw.hgatp_vmid = tr.io_ooo_to_mem_tlbCsr_hgatp_vmid;
        raw.hgatp_ppn = tr.io_ooo_to_mem_tlbCsr_hgatp_ppn;
        raw.hgatp_changed = tr.io_ooo_to_mem_tlbCsr_hgatp_changed;
        raw.priv_mxr = tr.io_ooo_to_mem_tlbCsr_priv_mxr;
        raw.priv_sum = tr.io_ooo_to_mem_tlbCsr_priv_sum;
        raw.priv_vmxr = tr.io_ooo_to_mem_tlbCsr_priv_vmxr;
        raw.priv_vsum = tr.io_ooo_to_mem_tlbCsr_priv_vsum;
        raw.priv_virt = tr.io_ooo_to_mem_tlbCsr_priv_virt;
        raw.priv_virt_changed = tr.io_ooo_to_mem_tlbCsr_priv_virt_changed;
        raw.priv_spvp = tr.io_ooo_to_mem_tlbCsr_priv_spvp;
        raw.priv_imode = tr.io_ooo_to_mem_tlbCsr_priv_imode;
        raw.priv_dmode = tr.io_ooo_to_mem_tlbCsr_priv_dmode;
        raw.hd_misalign_ld_enable = tr.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable;
        raw.hd_misalign_st_enable = tr.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable;
        raw.priv_debug = tr.io_ooo_to_mem_tlbCsr_priv_debug;
        raw.m_pbmt_en = tr.io_ooo_to_mem_tlbCsr_mPBMTE;
        raw.h_pbmt_en = tr.io_ooo_to_mem_tlbCsr_hPBMTE;
        return raw;
    endfunction:make_runtime_payload
endclass:memblock_csr_config_state

`endif
