//=========================================================
//File name    : csr_ctrl_agent_agent_interface.sv
//Author       : OpenAI_Codex
//Module name  : csr_ctrl_agent_agent_interface
//Discribution : csr_ctrl_agent_agent_interface : signal interface
//Date         : 2026-04-12
//=========================================================
`ifndef CSR_CTRL_AGENT_AGENT_INTERFACE__SV
`define CSR_CTRL_AGENT_AGENT_INTERFACE__SV

`ifndef DEF_SETUP_TIME
    `define DEF_SETUP_TIME 1
`endif
`ifndef DEF_HOLD_TIME
    `define DEF_HOLD_TIME 1
`endif

interface csr_ctrl_agent_agent_interface  (input bit clk,input bit rst_n);

    logic [3:0] io_ooo_to_mem_tlbCsr_satp_mode;
    logic [15:0] io_ooo_to_mem_tlbCsr_satp_asid;
    logic [43:0] io_ooo_to_mem_tlbCsr_satp_ppn;
    logic io_ooo_to_mem_tlbCsr_satp_changed;
    logic [3:0] io_ooo_to_mem_tlbCsr_vsatp_mode;
    logic [15:0] io_ooo_to_mem_tlbCsr_vsatp_asid;
    logic [43:0] io_ooo_to_mem_tlbCsr_vsatp_ppn;
    logic io_ooo_to_mem_tlbCsr_vsatp_changed;
    logic [3:0] io_ooo_to_mem_tlbCsr_hgatp_mode;
    logic [15:0] io_ooo_to_mem_tlbCsr_hgatp_vmid;
    logic [43:0] io_ooo_to_mem_tlbCsr_hgatp_ppn;
    logic io_ooo_to_mem_tlbCsr_hgatp_changed;
    logic io_ooo_to_mem_tlbCsr_mbmc_BME;
    logic io_ooo_to_mem_tlbCsr_mbmc_CMODE;
    logic io_ooo_to_mem_tlbCsr_mbmc_BCLEAR;
    logic [57:0] io_ooo_to_mem_tlbCsr_mbmc_BMA;
    logic io_ooo_to_mem_tlbCsr_priv_mxr;
    logic io_ooo_to_mem_tlbCsr_priv_sum;
    logic io_ooo_to_mem_tlbCsr_priv_vmxr;
    logic io_ooo_to_mem_tlbCsr_priv_vsum;
    logic io_ooo_to_mem_tlbCsr_priv_virt;
    logic io_ooo_to_mem_tlbCsr_priv_virt_changed;
    logic io_ooo_to_mem_tlbCsr_priv_spvp;
    logic [1:0] io_ooo_to_mem_tlbCsr_priv_imode;
    logic [1:0] io_ooo_to_mem_tlbCsr_priv_dmode;
    logic io_ooo_to_mem_tlbCsr_mPBMTE  ;
    logic io_ooo_to_mem_tlbCsr_hPBMTE  ;
    logic [1:0] io_ooo_to_mem_tlbCsr_pmm_mseccfg;
    logic [1:0] io_ooo_to_mem_tlbCsr_pmm_menvcfg;
    logic [1:0] io_ooo_to_mem_tlbCsr_pmm_henvcfg;
    logic [1:0] io_ooo_to_mem_tlbCsr_pmm_hstatus;
    logic [1:0] io_ooo_to_mem_tlbCsr_pmm_senvcfg;
    logic io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable;
    logic io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable;
    logic io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable;
    logic io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit;
    logic io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt;
    logic io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht;
    logic [3:0] io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold;
    logic [5:0] io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride;
    logic io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride;
    logic io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only;
    logic io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable;
    logic io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable;
    logic io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable;
    logic io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable;
    logic [9:0] io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency;
    logic io_ooo_to_mem_csrCtrl_bp_ctrl_ubtbEnable;
    logic io_ooo_to_mem_csrCtrl_bp_ctrl_abtbEnable;
    logic io_ooo_to_mem_csrCtrl_bp_ctrl_mbtbEnable;
    logic io_ooo_to_mem_csrCtrl_bp_ctrl_tageEnable;
    logic io_ooo_to_mem_csrCtrl_bp_ctrl_scEnable;
    logic io_ooo_to_mem_csrCtrl_bp_ctrl_ittageEnable;
    logic [21:0] io_ooo_to_mem_csrCtrl_sbuffer_timeout;
    logic io_ooo_to_mem_csrCtrl_ldld_vio_check_enable;
    logic io_ooo_to_mem_csrCtrl_cache_error_enable;
    logic io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable;
    logic io_ooo_to_mem_csrCtrl_power_down_enable;
    logic io_ooo_to_mem_csrCtrl_flush_l2_enable;
    logic io_ooo_to_mem_csrCtrl_distribute_csr_w_valid;
    logic [11:0] io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr;
    logic [63:0] io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data;
    logic io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid;
    logic [1:0] io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr;
    logic [1:0] io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType;
    logic io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select;
    logic io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_timing;
    logic [3:0] io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action;
    logic io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain;
    logic io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_execute;
    logic io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_store;
    logic io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_load;
    logic [63:0] io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2;
    logic io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0;
    logic io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1;
    logic io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2;
    logic io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3;
    logic io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp;
    logic io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid;
    logic [1:0] io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr;
    logic [1:0] io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType;
    logic io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select;
    logic io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_timing;
    logic [3:0] io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action;
    logic io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain;
    logic io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store;
    logic io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load;
    logic [63:0] io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2;
    logic io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0;
    logic io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1;
    logic io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2;
    logic io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3;
    logic io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp;
    logic io_ooo_to_mem_csrCtrl_fsIsOff;

    clocking drv_cb @(posedge clk);
        `ifdef INTERFACE_ADD_DELAY
            default input #`DEF_SETUP_TIME output #`DEF_HOLD_TIME;
        `endif
        output io_ooo_to_mem_tlbCsr_satp_mode;
        output io_ooo_to_mem_tlbCsr_satp_asid;
        output io_ooo_to_mem_tlbCsr_satp_ppn;
        output io_ooo_to_mem_tlbCsr_satp_changed;
        output io_ooo_to_mem_tlbCsr_vsatp_mode;
        output io_ooo_to_mem_tlbCsr_vsatp_asid;
        output io_ooo_to_mem_tlbCsr_vsatp_ppn;
        output io_ooo_to_mem_tlbCsr_vsatp_changed;
        output io_ooo_to_mem_tlbCsr_hgatp_mode;
        output io_ooo_to_mem_tlbCsr_hgatp_vmid;
        output io_ooo_to_mem_tlbCsr_hgatp_ppn;
        output io_ooo_to_mem_tlbCsr_hgatp_changed;
        output io_ooo_to_mem_tlbCsr_mbmc_BME;
        output io_ooo_to_mem_tlbCsr_mbmc_CMODE;
        output io_ooo_to_mem_tlbCsr_mbmc_BCLEAR;
        output io_ooo_to_mem_tlbCsr_mbmc_BMA;
        output io_ooo_to_mem_tlbCsr_priv_mxr;
        output io_ooo_to_mem_tlbCsr_priv_sum;
        output io_ooo_to_mem_tlbCsr_priv_vmxr;
        output io_ooo_to_mem_tlbCsr_priv_vsum;
        output io_ooo_to_mem_tlbCsr_priv_virt;
        output io_ooo_to_mem_tlbCsr_priv_virt_changed;
        output io_ooo_to_mem_tlbCsr_priv_spvp;
        output io_ooo_to_mem_tlbCsr_priv_imode;
        output io_ooo_to_mem_tlbCsr_priv_dmode;
        output io_ooo_to_mem_tlbCsr_mPBMTE;
        output io_ooo_to_mem_tlbCsr_hPBMTE;
        output io_ooo_to_mem_tlbCsr_pmm_mseccfg;
        output io_ooo_to_mem_tlbCsr_pmm_menvcfg;
        output io_ooo_to_mem_tlbCsr_pmm_henvcfg;
        output io_ooo_to_mem_tlbCsr_pmm_hstatus;
        output io_ooo_to_mem_tlbCsr_pmm_senvcfg;
        output io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable;
        output io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable;
        output io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable;
        output io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit;
        output io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt;
        output io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht;
        output io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold;
        output io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride;
        output io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride;
        output io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only;
        output io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable;
        output io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable;
        output io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable;
        output io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable;
        output io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency;
        output io_ooo_to_mem_csrCtrl_bp_ctrl_ubtbEnable;
        output io_ooo_to_mem_csrCtrl_bp_ctrl_abtbEnable;
        output io_ooo_to_mem_csrCtrl_bp_ctrl_mbtbEnable;
        output io_ooo_to_mem_csrCtrl_bp_ctrl_tageEnable;
        output io_ooo_to_mem_csrCtrl_bp_ctrl_scEnable;
        output io_ooo_to_mem_csrCtrl_bp_ctrl_ittageEnable;
        output io_ooo_to_mem_csrCtrl_sbuffer_timeout;
        output io_ooo_to_mem_csrCtrl_ldld_vio_check_enable;
        output io_ooo_to_mem_csrCtrl_cache_error_enable;
        output io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable;
        output io_ooo_to_mem_csrCtrl_power_down_enable;
        output io_ooo_to_mem_csrCtrl_flush_l2_enable;
        output io_ooo_to_mem_csrCtrl_distribute_csr_w_valid;
        output io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr;
        output io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data;
        output io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid;
        output io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr;
        output io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType;
        output io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select;
        output io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_timing;
        output io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action;
        output io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain;
        output io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_execute;
        output io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_store;
        output io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_load;
        output io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2;
        output io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0;
        output io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1;
        output io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2;
        output io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3;
        output io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp;
        output io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid;
        output io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr;
        output io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType;
        output io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select;
        output io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_timing;
        output io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action;
        output io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain;
        output io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store;
        output io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load;
        output io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2;
        output io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0;
        output io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1;
        output io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2;
        output io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3;
        output io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp;
        output io_ooo_to_mem_csrCtrl_fsIsOff;

    endclocking:drv_cb

    clocking mon_cb @(posedge clk);
        `ifdef INTERFACE_ADD_DELAY
            default input #`DEF_SETUP_TIME output #`DEF_HOLD_TIME;
        `endif
        input  io_ooo_to_mem_tlbCsr_satp_mode;
        input  io_ooo_to_mem_tlbCsr_satp_asid;
        input  io_ooo_to_mem_tlbCsr_satp_ppn;
        input  io_ooo_to_mem_tlbCsr_satp_changed;
        input  io_ooo_to_mem_tlbCsr_vsatp_mode;
        input  io_ooo_to_mem_tlbCsr_vsatp_asid;
        input  io_ooo_to_mem_tlbCsr_vsatp_ppn;
        input  io_ooo_to_mem_tlbCsr_vsatp_changed;
        input  io_ooo_to_mem_tlbCsr_hgatp_mode;
        input  io_ooo_to_mem_tlbCsr_hgatp_vmid;
        input  io_ooo_to_mem_tlbCsr_hgatp_ppn;
        input  io_ooo_to_mem_tlbCsr_hgatp_changed;
        input  io_ooo_to_mem_tlbCsr_mbmc_BME;
        input  io_ooo_to_mem_tlbCsr_mbmc_CMODE;
        input  io_ooo_to_mem_tlbCsr_mbmc_BCLEAR;
        input  io_ooo_to_mem_tlbCsr_mbmc_BMA;
        input  io_ooo_to_mem_tlbCsr_priv_mxr;
        input  io_ooo_to_mem_tlbCsr_priv_sum;
        input  io_ooo_to_mem_tlbCsr_priv_vmxr;
        input  io_ooo_to_mem_tlbCsr_priv_vsum;
        input  io_ooo_to_mem_tlbCsr_priv_virt;
        input  io_ooo_to_mem_tlbCsr_priv_virt_changed;
        input  io_ooo_to_mem_tlbCsr_priv_spvp;
        input  io_ooo_to_mem_tlbCsr_priv_imode;
        input  io_ooo_to_mem_tlbCsr_priv_dmode;
        input  io_ooo_to_mem_tlbCsr_mPBMTE;
        input  io_ooo_to_mem_tlbCsr_hPBMTE;
        input  io_ooo_to_mem_tlbCsr_pmm_mseccfg;
        input  io_ooo_to_mem_tlbCsr_pmm_menvcfg;
        input  io_ooo_to_mem_tlbCsr_pmm_henvcfg;
        input  io_ooo_to_mem_tlbCsr_pmm_hstatus;
        input  io_ooo_to_mem_tlbCsr_pmm_senvcfg;
        input  io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable;
        input  io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable;
        input  io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable;
        input  io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit;
        input  io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt;
        input  io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht;
        input  io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold;
        input  io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride;
        input  io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride;
        input  io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only;
        input  io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable;
        input  io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable;
        input  io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable;
        input  io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable;
        input  io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency;
        input  io_ooo_to_mem_csrCtrl_bp_ctrl_ubtbEnable;
        input  io_ooo_to_mem_csrCtrl_bp_ctrl_abtbEnable;
        input  io_ooo_to_mem_csrCtrl_bp_ctrl_mbtbEnable;
        input  io_ooo_to_mem_csrCtrl_bp_ctrl_tageEnable;
        input  io_ooo_to_mem_csrCtrl_bp_ctrl_scEnable;
        input  io_ooo_to_mem_csrCtrl_bp_ctrl_ittageEnable;
        input  io_ooo_to_mem_csrCtrl_sbuffer_timeout;
        input  io_ooo_to_mem_csrCtrl_ldld_vio_check_enable;
        input  io_ooo_to_mem_csrCtrl_cache_error_enable;
        input  io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable;
        input  io_ooo_to_mem_csrCtrl_power_down_enable;
        input  io_ooo_to_mem_csrCtrl_flush_l2_enable;
        input  io_ooo_to_mem_csrCtrl_distribute_csr_w_valid;
        input  io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr;
        input  io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data;
        input  io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid;
        input  io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr;
        input  io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType;
        input  io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select;
        input  io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_timing;
        input  io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action;
        input  io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain;
        input  io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_execute;
        input  io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_store;
        input  io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_load;
        input  io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2;
        input  io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0;
        input  io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1;
        input  io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2;
        input  io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3;
        input  io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp;
        input  io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid;
        input  io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr;
        input  io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType;
        input  io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select;
        input  io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_timing;
        input  io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action;
        input  io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain;
        input  io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store;
        input  io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load;
        input  io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2;
        input  io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0;
        input  io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1;
        input  io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2;
        input  io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3;
        input  io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp;
        input  io_ooo_to_mem_csrCtrl_fsIsOff;

    endclocking:mon_cb

    modport drv_mp (clocking drv_cb);
    modport mon_mp (clocking mon_cb);

endinterface:csr_ctrl_agent_agent_interface

`endif
