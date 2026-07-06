//=========================================================
//File name    : csr_ctrl_agent_connect.sv
//Author       : OpenAI_Codex
//Module name  : csr_ctrl_agent_connect
//Discribution : csr_ctrl_agent_connect : csr_ctrl_agent Interface connection macro
//Date         : 2026-04-12
//=========================================================
`ifndef CSR_CTRL_AGENT_CONNECT__SV
`define CSR_CTRL_AGENT_CONNECT__SV

`define MEMBLOCK__CSR_CTRL_AGENT_CONNECT(U_IF_NAME,AGENT_PATH,RTL_PATH) \
    csr_ctrl_agent_agent_interface  U_IF_NAME (clk,tc_if.rst_n); \
    initial begin \
        uvm_config_db#(virtual csr_ctrl_agent_agent_interface)::set(null,`"*AGENT_PATH*`", "vif", U_IF_NAME); \
    end \
    `ifdef MEMBLOCK_UT \
    initial begin \
        force RTL_PATH.io_ooo_to_mem_tlbCsr_satp_mode = U_IF_NAME.io_ooo_to_mem_tlbCsr_satp_mode; \
        force RTL_PATH.io_ooo_to_mem_tlbCsr_satp_asid = U_IF_NAME.io_ooo_to_mem_tlbCsr_satp_asid; \
        force RTL_PATH.io_ooo_to_mem_tlbCsr_satp_ppn = U_IF_NAME.io_ooo_to_mem_tlbCsr_satp_ppn; \
        force RTL_PATH.io_ooo_to_mem_tlbCsr_satp_changed = U_IF_NAME.io_ooo_to_mem_tlbCsr_satp_changed; \
        force RTL_PATH.io_ooo_to_mem_tlbCsr_vsatp_mode = U_IF_NAME.io_ooo_to_mem_tlbCsr_vsatp_mode; \
        force RTL_PATH.io_ooo_to_mem_tlbCsr_vsatp_asid = U_IF_NAME.io_ooo_to_mem_tlbCsr_vsatp_asid; \
        force RTL_PATH.io_ooo_to_mem_tlbCsr_vsatp_ppn = U_IF_NAME.io_ooo_to_mem_tlbCsr_vsatp_ppn; \
        force RTL_PATH.io_ooo_to_mem_tlbCsr_vsatp_changed = U_IF_NAME.io_ooo_to_mem_tlbCsr_vsatp_changed; \
        force RTL_PATH.io_ooo_to_mem_tlbCsr_hgatp_mode = U_IF_NAME.io_ooo_to_mem_tlbCsr_hgatp_mode; \
        force RTL_PATH.io_ooo_to_mem_tlbCsr_hgatp_vmid = U_IF_NAME.io_ooo_to_mem_tlbCsr_hgatp_vmid; \
        force RTL_PATH.io_ooo_to_mem_tlbCsr_hgatp_ppn = U_IF_NAME.io_ooo_to_mem_tlbCsr_hgatp_ppn; \
        force RTL_PATH.io_ooo_to_mem_tlbCsr_hgatp_changed = U_IF_NAME.io_ooo_to_mem_tlbCsr_hgatp_changed; \
        force RTL_PATH.io_ooo_to_mem_tlbCsr_mbmc_BME = U_IF_NAME.io_ooo_to_mem_tlbCsr_mbmc_BME; \
        force RTL_PATH.io_ooo_to_mem_tlbCsr_mbmc_CMODE = U_IF_NAME.io_ooo_to_mem_tlbCsr_mbmc_CMODE; \
        force RTL_PATH.io_ooo_to_mem_tlbCsr_mbmc_BCLEAR = U_IF_NAME.io_ooo_to_mem_tlbCsr_mbmc_BCLEAR; \
        force RTL_PATH.io_ooo_to_mem_tlbCsr_mbmc_BMA = U_IF_NAME.io_ooo_to_mem_tlbCsr_mbmc_BMA; \
        force RTL_PATH.io_ooo_to_mem_tlbCsr_priv_mxr = U_IF_NAME.io_ooo_to_mem_tlbCsr_priv_mxr; \
        force RTL_PATH.io_ooo_to_mem_tlbCsr_priv_sum = U_IF_NAME.io_ooo_to_mem_tlbCsr_priv_sum; \
        force RTL_PATH.io_ooo_to_mem_tlbCsr_priv_vmxr = U_IF_NAME.io_ooo_to_mem_tlbCsr_priv_vmxr; \
        force RTL_PATH.io_ooo_to_mem_tlbCsr_priv_vsum = U_IF_NAME.io_ooo_to_mem_tlbCsr_priv_vsum; \
        force RTL_PATH.io_ooo_to_mem_tlbCsr_priv_virt = U_IF_NAME.io_ooo_to_mem_tlbCsr_priv_virt; \
        force RTL_PATH.io_ooo_to_mem_tlbCsr_priv_virt_changed = U_IF_NAME.io_ooo_to_mem_tlbCsr_priv_virt_changed; \
        force RTL_PATH.io_ooo_to_mem_tlbCsr_priv_spvp = U_IF_NAME.io_ooo_to_mem_tlbCsr_priv_spvp; \
        force RTL_PATH.io_ooo_to_mem_tlbCsr_priv_imode = U_IF_NAME.io_ooo_to_mem_tlbCsr_priv_imode; \
        force RTL_PATH.io_ooo_to_mem_tlbCsr_priv_dmode = U_IF_NAME.io_ooo_to_mem_tlbCsr_priv_dmode; \
        force RTL_PATH.io_ooo_to_mem_tlbCsr_mPBMTE = U_IF_NAME.io_ooo_to_mem_tlbCsr_mPBMTE; \
        force RTL_PATH.io_ooo_to_mem_tlbCsr_hPBMTE = U_IF_NAME.io_ooo_to_mem_tlbCsr_hPBMTE; \
        force RTL_PATH.io_ooo_to_mem_tlbCsr_pmm_mseccfg = U_IF_NAME.io_ooo_to_mem_tlbCsr_pmm_mseccfg; \
        force RTL_PATH.io_ooo_to_mem_tlbCsr_pmm_menvcfg = U_IF_NAME.io_ooo_to_mem_tlbCsr_pmm_menvcfg; \
        force RTL_PATH.io_ooo_to_mem_tlbCsr_pmm_henvcfg = U_IF_NAME.io_ooo_to_mem_tlbCsr_pmm_henvcfg; \
        force RTL_PATH.io_ooo_to_mem_tlbCsr_pmm_hstatus = U_IF_NAME.io_ooo_to_mem_tlbCsr_pmm_hstatus; \
        force RTL_PATH.io_ooo_to_mem_tlbCsr_pmm_senvcfg = U_IF_NAME.io_ooo_to_mem_tlbCsr_pmm_senvcfg; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable = U_IF_NAME.io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable = U_IF_NAME.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable = U_IF_NAME.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit = U_IF_NAME.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt = U_IF_NAME.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht = U_IF_NAME.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold = U_IF_NAME.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride = U_IF_NAME.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride = U_IF_NAME.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only = U_IF_NAME.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable = U_IF_NAME.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable = U_IF_NAME.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable = U_IF_NAME.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable = U_IF_NAME.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency = U_IF_NAME.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_bp_ctrl_ubtbEnable = U_IF_NAME.io_ooo_to_mem_csrCtrl_bp_ctrl_ubtbEnable; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_bp_ctrl_abtbEnable = U_IF_NAME.io_ooo_to_mem_csrCtrl_bp_ctrl_abtbEnable; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_bp_ctrl_mbtbEnable = U_IF_NAME.io_ooo_to_mem_csrCtrl_bp_ctrl_mbtbEnable; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_bp_ctrl_tageEnable = U_IF_NAME.io_ooo_to_mem_csrCtrl_bp_ctrl_tageEnable; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_bp_ctrl_scEnable = U_IF_NAME.io_ooo_to_mem_csrCtrl_bp_ctrl_scEnable; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_bp_ctrl_ittageEnable = U_IF_NAME.io_ooo_to_mem_csrCtrl_bp_ctrl_ittageEnable; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_sbuffer_timeout = U_IF_NAME.io_ooo_to_mem_csrCtrl_sbuffer_timeout; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_ldld_vio_check_enable = U_IF_NAME.io_ooo_to_mem_csrCtrl_ldld_vio_check_enable; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_cache_error_enable = U_IF_NAME.io_ooo_to_mem_csrCtrl_cache_error_enable; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable = U_IF_NAME.io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_power_down_enable = U_IF_NAME.io_ooo_to_mem_csrCtrl_power_down_enable; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_flush_l2_enable = U_IF_NAME.io_ooo_to_mem_csrCtrl_flush_l2_enable; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid = U_IF_NAME.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr = U_IF_NAME.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data = U_IF_NAME.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid = U_IF_NAME.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr = U_IF_NAME.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType = U_IF_NAME.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select = U_IF_NAME.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_timing = U_IF_NAME.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_timing; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action = U_IF_NAME.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain = U_IF_NAME.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_execute = U_IF_NAME.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_execute; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_store = U_IF_NAME.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_store; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_load = U_IF_NAME.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_load; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2 = U_IF_NAME.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0 = U_IF_NAME.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1 = U_IF_NAME.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2 = U_IF_NAME.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3 = U_IF_NAME.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp = U_IF_NAME.io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid = U_IF_NAME.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr = U_IF_NAME.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType = U_IF_NAME.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select = U_IF_NAME.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_timing = U_IF_NAME.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_timing; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action = U_IF_NAME.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain = U_IF_NAME.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store = U_IF_NAME.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load = U_IF_NAME.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2 = U_IF_NAME.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0 = U_IF_NAME.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1 = U_IF_NAME.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2 = U_IF_NAME.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3 = U_IF_NAME.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp = U_IF_NAME.io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp; \
        force RTL_PATH.io_ooo_to_mem_csrCtrl_fsIsOff = U_IF_NAME.io_ooo_to_mem_csrCtrl_fsIsOff; \
    end \
    `else \
    initial begin \
        force U_IF_NAME.io_ooo_to_mem_tlbCsr_satp_mode = RTL_PATH.io_ooo_to_mem_tlbCsr_satp_mode; \
        force U_IF_NAME.io_ooo_to_mem_tlbCsr_satp_asid = RTL_PATH.io_ooo_to_mem_tlbCsr_satp_asid; \
        force U_IF_NAME.io_ooo_to_mem_tlbCsr_satp_ppn = RTL_PATH.io_ooo_to_mem_tlbCsr_satp_ppn; \
        force U_IF_NAME.io_ooo_to_mem_tlbCsr_satp_changed = RTL_PATH.io_ooo_to_mem_tlbCsr_satp_changed; \
        force U_IF_NAME.io_ooo_to_mem_tlbCsr_vsatp_mode = RTL_PATH.io_ooo_to_mem_tlbCsr_vsatp_mode; \
        force U_IF_NAME.io_ooo_to_mem_tlbCsr_vsatp_asid = RTL_PATH.io_ooo_to_mem_tlbCsr_vsatp_asid; \
        force U_IF_NAME.io_ooo_to_mem_tlbCsr_vsatp_ppn = RTL_PATH.io_ooo_to_mem_tlbCsr_vsatp_ppn; \
        force U_IF_NAME.io_ooo_to_mem_tlbCsr_vsatp_changed = RTL_PATH.io_ooo_to_mem_tlbCsr_vsatp_changed; \
        force U_IF_NAME.io_ooo_to_mem_tlbCsr_hgatp_mode = RTL_PATH.io_ooo_to_mem_tlbCsr_hgatp_mode; \
        force U_IF_NAME.io_ooo_to_mem_tlbCsr_hgatp_vmid = RTL_PATH.io_ooo_to_mem_tlbCsr_hgatp_vmid; \
        force U_IF_NAME.io_ooo_to_mem_tlbCsr_hgatp_ppn = RTL_PATH.io_ooo_to_mem_tlbCsr_hgatp_ppn; \
        force U_IF_NAME.io_ooo_to_mem_tlbCsr_hgatp_changed = RTL_PATH.io_ooo_to_mem_tlbCsr_hgatp_changed; \
        force U_IF_NAME.io_ooo_to_mem_tlbCsr_mbmc_BME = RTL_PATH.io_ooo_to_mem_tlbCsr_mbmc_BME; \
        force U_IF_NAME.io_ooo_to_mem_tlbCsr_mbmc_CMODE = RTL_PATH.io_ooo_to_mem_tlbCsr_mbmc_CMODE; \
        force U_IF_NAME.io_ooo_to_mem_tlbCsr_mbmc_BCLEAR = RTL_PATH.io_ooo_to_mem_tlbCsr_mbmc_BCLEAR; \
        force U_IF_NAME.io_ooo_to_mem_tlbCsr_mbmc_BMA = RTL_PATH.io_ooo_to_mem_tlbCsr_mbmc_BMA; \
        force U_IF_NAME.io_ooo_to_mem_tlbCsr_priv_mxr = RTL_PATH.io_ooo_to_mem_tlbCsr_priv_mxr; \
        force U_IF_NAME.io_ooo_to_mem_tlbCsr_priv_sum = RTL_PATH.io_ooo_to_mem_tlbCsr_priv_sum; \
        force U_IF_NAME.io_ooo_to_mem_tlbCsr_priv_vmxr = RTL_PATH.io_ooo_to_mem_tlbCsr_priv_vmxr; \
        force U_IF_NAME.io_ooo_to_mem_tlbCsr_priv_vsum = RTL_PATH.io_ooo_to_mem_tlbCsr_priv_vsum; \
        force U_IF_NAME.io_ooo_to_mem_tlbCsr_priv_virt = RTL_PATH.io_ooo_to_mem_tlbCsr_priv_virt; \
        force U_IF_NAME.io_ooo_to_mem_tlbCsr_priv_virt_changed = RTL_PATH.io_ooo_to_mem_tlbCsr_priv_virt_changed; \
        force U_IF_NAME.io_ooo_to_mem_tlbCsr_priv_spvp = RTL_PATH.io_ooo_to_mem_tlbCsr_priv_spvp; \
        force U_IF_NAME.io_ooo_to_mem_tlbCsr_priv_imode = RTL_PATH.io_ooo_to_mem_tlbCsr_priv_imode; \
        force U_IF_NAME.io_ooo_to_mem_tlbCsr_priv_dmode = RTL_PATH.io_ooo_to_mem_tlbCsr_priv_dmode; \
        force U_IF_NAME.io_ooo_to_mem_tlbCsr_mPBMTE = RTL_PATH.io_ooo_to_mem_tlbCsr_mPBMTE; \
        force U_IF_NAME.io_ooo_to_mem_tlbCsr_hPBMTE = RTL_PATH.io_ooo_to_mem_tlbCsr_hPBMTE; \
        force U_IF_NAME.io_ooo_to_mem_tlbCsr_pmm_mseccfg = RTL_PATH.io_ooo_to_mem_tlbCsr_pmm_mseccfg; \
        force U_IF_NAME.io_ooo_to_mem_tlbCsr_pmm_menvcfg = RTL_PATH.io_ooo_to_mem_tlbCsr_pmm_menvcfg; \
        force U_IF_NAME.io_ooo_to_mem_tlbCsr_pmm_henvcfg = RTL_PATH.io_ooo_to_mem_tlbCsr_pmm_henvcfg; \
        force U_IF_NAME.io_ooo_to_mem_tlbCsr_pmm_hstatus = RTL_PATH.io_ooo_to_mem_tlbCsr_pmm_hstatus; \
        force U_IF_NAME.io_ooo_to_mem_tlbCsr_pmm_senvcfg = RTL_PATH.io_ooo_to_mem_tlbCsr_pmm_senvcfg; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable = RTL_PATH.io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable = RTL_PATH.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable = RTL_PATH.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit = RTL_PATH.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt = RTL_PATH.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht = RTL_PATH.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold = RTL_PATH.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride = RTL_PATH.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride = RTL_PATH.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only = RTL_PATH.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable = RTL_PATH.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable = RTL_PATH.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable = RTL_PATH.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable = RTL_PATH.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency = RTL_PATH.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_bp_ctrl_ubtbEnable = RTL_PATH.io_ooo_to_mem_csrCtrl_bp_ctrl_ubtbEnable; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_bp_ctrl_abtbEnable = RTL_PATH.io_ooo_to_mem_csrCtrl_bp_ctrl_abtbEnable; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_bp_ctrl_mbtbEnable = RTL_PATH.io_ooo_to_mem_csrCtrl_bp_ctrl_mbtbEnable; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_bp_ctrl_tageEnable = RTL_PATH.io_ooo_to_mem_csrCtrl_bp_ctrl_tageEnable; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_bp_ctrl_scEnable = RTL_PATH.io_ooo_to_mem_csrCtrl_bp_ctrl_scEnable; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_bp_ctrl_ittageEnable = RTL_PATH.io_ooo_to_mem_csrCtrl_bp_ctrl_ittageEnable; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_sbuffer_timeout = RTL_PATH.io_ooo_to_mem_csrCtrl_sbuffer_timeout; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_ldld_vio_check_enable = RTL_PATH.io_ooo_to_mem_csrCtrl_ldld_vio_check_enable; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_cache_error_enable = RTL_PATH.io_ooo_to_mem_csrCtrl_cache_error_enable; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable = RTL_PATH.io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_power_down_enable = RTL_PATH.io_ooo_to_mem_csrCtrl_power_down_enable; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_flush_l2_enable = RTL_PATH.io_ooo_to_mem_csrCtrl_flush_l2_enable; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid = RTL_PATH.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr = RTL_PATH.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data = RTL_PATH.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid = RTL_PATH.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr = RTL_PATH.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType = RTL_PATH.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select = RTL_PATH.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_timing = RTL_PATH.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_timing; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action = RTL_PATH.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain = RTL_PATH.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_execute = RTL_PATH.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_execute; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_store = RTL_PATH.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_store; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_load = RTL_PATH.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_load; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2 = RTL_PATH.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0 = RTL_PATH.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1 = RTL_PATH.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2 = RTL_PATH.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3 = RTL_PATH.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp = RTL_PATH.io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid = RTL_PATH.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr = RTL_PATH.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType = RTL_PATH.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select = RTL_PATH.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_timing = RTL_PATH.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_timing; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action = RTL_PATH.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain = RTL_PATH.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store = RTL_PATH.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load = RTL_PATH.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2 = RTL_PATH.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0 = RTL_PATH.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1 = RTL_PATH.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2 = RTL_PATH.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3 = RTL_PATH.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp = RTL_PATH.io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp; \
        force U_IF_NAME.io_ooo_to_mem_csrCtrl_fsIsOff = RTL_PATH.io_ooo_to_mem_csrCtrl_fsIsOff; \
    end \
    `endif

`endif
