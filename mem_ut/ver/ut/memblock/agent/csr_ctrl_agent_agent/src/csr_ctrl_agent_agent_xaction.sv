//=========================================================
//File name    : csr_ctrl_agent_agent_xaction.sv
//Author       : OpenAI_Codex
//Module name  : csr_ctrl_agent_agent_xaction
//Discribution : csr_ctrl_agent_agent_xaction : agent transaction
//Date         : 2026-04-12
//=========================================================
`ifndef CSR_CTRL_AGENT_AGENT_XACTION__SV
`define CSR_CTRL_AGENT_AGENT_XACTION__SV

class csr_ctrl_agent_agent_xaction  extends tcnt_data_base;
    rand bit [3:0] io_ooo_to_mem_tlbCsr_satp_mode;
    rand bit [15:0] io_ooo_to_mem_tlbCsr_satp_asid;
    rand bit [43:0] io_ooo_to_mem_tlbCsr_satp_ppn;
    rand bit io_ooo_to_mem_tlbCsr_satp_changed;
    rand bit [3:0] io_ooo_to_mem_tlbCsr_vsatp_mode;
    rand bit [15:0] io_ooo_to_mem_tlbCsr_vsatp_asid;
    rand bit [43:0] io_ooo_to_mem_tlbCsr_vsatp_ppn;
    rand bit io_ooo_to_mem_tlbCsr_vsatp_changed;
    rand bit [3:0] io_ooo_to_mem_tlbCsr_hgatp_mode;
    rand bit [15:0] io_ooo_to_mem_tlbCsr_hgatp_vmid;
    rand bit [43:0] io_ooo_to_mem_tlbCsr_hgatp_ppn;
    rand bit io_ooo_to_mem_tlbCsr_hgatp_changed;
    rand bit io_ooo_to_mem_tlbCsr_mbmc_BME;
    rand bit io_ooo_to_mem_tlbCsr_mbmc_CMODE;
    rand bit io_ooo_to_mem_tlbCsr_mbmc_BCLEAR;
    rand bit [57:0] io_ooo_to_mem_tlbCsr_mbmc_BMA;
    rand bit io_ooo_to_mem_tlbCsr_priv_mxr;
    rand bit io_ooo_to_mem_tlbCsr_priv_sum;
    rand bit io_ooo_to_mem_tlbCsr_priv_vmxr;
    rand bit io_ooo_to_mem_tlbCsr_priv_vsum;
    rand bit io_ooo_to_mem_tlbCsr_priv_virt;
    rand bit io_ooo_to_mem_tlbCsr_priv_virt_changed;
    rand bit io_ooo_to_mem_tlbCsr_priv_spvp;
    rand bit [1:0] io_ooo_to_mem_tlbCsr_priv_imode;
    rand bit [1:0] io_ooo_to_mem_tlbCsr_priv_dmode;
    rand bit io_ooo_to_mem_tlbCsr_mPBMTE;
    rand bit io_ooo_to_mem_tlbCsr_hPBMTE;
    rand bit [1:0] io_ooo_to_mem_tlbCsr_pmm_mseccfg;
    rand bit [1:0] io_ooo_to_mem_tlbCsr_pmm_menvcfg;
    rand bit [1:0] io_ooo_to_mem_tlbCsr_pmm_henvcfg;
    rand bit [1:0] io_ooo_to_mem_tlbCsr_pmm_hstatus;
    rand bit [1:0] io_ooo_to_mem_tlbCsr_pmm_senvcfg;
    rand bit io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable;
    rand bit io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable;
    rand bit io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable;
    rand bit io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit;
    rand bit io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt;
    rand bit io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht;
    rand bit [3:0] io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold;
    rand bit [5:0] io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride;
    rand bit io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride;
    rand bit io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only;
    rand bit io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable;
    rand bit io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable;
    rand bit io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable;
    rand bit io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable;
    rand bit [9:0] io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency;
    rand bit io_ooo_to_mem_csrCtrl_bp_ctrl_ubtbEnable;
    rand bit io_ooo_to_mem_csrCtrl_bp_ctrl_abtbEnable;
    rand bit io_ooo_to_mem_csrCtrl_bp_ctrl_mbtbEnable;
    rand bit io_ooo_to_mem_csrCtrl_bp_ctrl_tageEnable;
    rand bit io_ooo_to_mem_csrCtrl_bp_ctrl_scEnable;
    rand bit io_ooo_to_mem_csrCtrl_bp_ctrl_ittageEnable;
    rand bit [21:0] io_ooo_to_mem_csrCtrl_sbuffer_timeout;
    rand bit io_ooo_to_mem_csrCtrl_ldld_vio_check_enable;
    rand bit io_ooo_to_mem_csrCtrl_cache_error_enable;
    rand bit io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable;
    rand bit io_ooo_to_mem_csrCtrl_power_down_enable;
    rand bit io_ooo_to_mem_csrCtrl_flush_l2_enable;
    rand bit io_ooo_to_mem_csrCtrl_distribute_csr_w_valid;
    rand bit [11:0] io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr;
    rand bit [63:0] io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data;
    rand bit io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid;
    rand bit [1:0] io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr;
    rand bit [1:0] io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType;
    rand bit io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select;
    rand bit io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_timing;
    rand bit [3:0] io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action;
    rand bit io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain;
    rand bit io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_execute;
    rand bit io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_store;
    rand bit io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_load;
    rand bit [63:0] io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2;
    rand bit io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0;
    rand bit io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1;
    rand bit io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2;
    rand bit io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3;
    rand bit io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp;
    rand bit io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid;
    rand bit [1:0] io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr;
    rand bit [1:0] io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType;
    rand bit io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select;
    rand bit io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_timing;
    rand bit [3:0] io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action;
    rand bit io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain;
    rand bit io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store;
    rand bit io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load;
    rand bit [63:0] io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2;
    rand bit io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0;
    rand bit io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1;
    rand bit io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2;
    rand bit io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3;
    rand bit io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp;
    rand bit io_ooo_to_mem_csrCtrl_fsIsOff;

    extern constraint default_io_ooo_to_mem_tlbCsr_satp_mode_cons;
    extern constraint default_io_ooo_to_mem_tlbCsr_satp_asid_cons;
    extern constraint default_io_ooo_to_mem_tlbCsr_satp_ppn_cons;
    extern constraint default_io_ooo_to_mem_tlbCsr_satp_changed_cons;
    extern constraint default_io_ooo_to_mem_tlbCsr_vsatp_mode_cons;
    extern constraint default_io_ooo_to_mem_tlbCsr_vsatp_asid_cons;
    extern constraint default_io_ooo_to_mem_tlbCsr_vsatp_ppn_cons;
    extern constraint default_io_ooo_to_mem_tlbCsr_vsatp_changed_cons;
    extern constraint default_io_ooo_to_mem_tlbCsr_hgatp_mode_cons;
    extern constraint default_io_ooo_to_mem_tlbCsr_hgatp_vmid_cons;
    extern constraint default_io_ooo_to_mem_tlbCsr_hgatp_ppn_cons;
    extern constraint default_io_ooo_to_mem_tlbCsr_hgatp_changed_cons;
    extern constraint default_io_ooo_to_mem_tlbCsr_mbmc_BME_cons;
    extern constraint default_io_ooo_to_mem_tlbCsr_mbmc_CMODE_cons;
    extern constraint default_io_ooo_to_mem_tlbCsr_mbmc_BCLEAR_cons;
    extern constraint default_io_ooo_to_mem_tlbCsr_mbmc_BMA_cons;
    extern constraint default_io_ooo_to_mem_tlbCsr_priv_mxr_cons;
    extern constraint default_io_ooo_to_mem_tlbCsr_priv_sum_cons;
    extern constraint default_io_ooo_to_mem_tlbCsr_priv_vmxr_cons;
    extern constraint default_io_ooo_to_mem_tlbCsr_priv_vsum_cons;
    extern constraint default_io_ooo_to_mem_tlbCsr_priv_virt_cons;
    extern constraint default_io_ooo_to_mem_tlbCsr_priv_virt_changed_cons;
    extern constraint default_io_ooo_to_mem_tlbCsr_priv_spvp_cons;
    extern constraint default_io_ooo_to_mem_tlbCsr_priv_imode_cons;
    extern constraint default_io_ooo_to_mem_tlbCsr_priv_dmode_cons;
    extern constraint default_io_ooo_to_mem_tlbCsr_mPBMTE_cons;
    extern constraint default_io_ooo_to_mem_tlbCsr_hPBMTE_cons;
    extern constraint default_io_ooo_to_mem_tlbCsr_pmm_mseccfg_cons;
    extern constraint default_io_ooo_to_mem_tlbCsr_pmm_menvcfg_cons;
    extern constraint default_io_ooo_to_mem_tlbCsr_pmm_henvcfg_cons;
    extern constraint default_io_ooo_to_mem_tlbCsr_pmm_hstatus_cons;
    extern constraint default_io_ooo_to_mem_tlbCsr_pmm_senvcfg_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_bp_ctrl_ubtbEnable_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_bp_ctrl_abtbEnable_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_bp_ctrl_mbtbEnable_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_bp_ctrl_tageEnable_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_bp_ctrl_scEnable_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_bp_ctrl_ittageEnable_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_sbuffer_timeout_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_ldld_vio_check_enable_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_cache_error_enable_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_power_down_enable_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_flush_l2_enable_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_distribute_csr_w_valid_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_timing_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_execute_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_store_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_load_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_timing_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp_cons;
    extern constraint default_io_ooo_to_mem_csrCtrl_fsIsOff_cons;

    extern function new(string name="csr_ctrl_agent_agent_xaction");
    extern function void pack();
    extern function void unpack();
    extern function void pre_randomize();
    extern function void post_randomize();
    extern function string psdisplay(string prefix = "");
    extern function bit compare(uvm_object rhs, uvm_comparer comparer=null);

    `uvm_object_utils_begin(csr_ctrl_agent_agent_xaction)
        `uvm_field_int(io_ooo_to_mem_tlbCsr_satp_mode, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_tlbCsr_satp_asid, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_tlbCsr_satp_ppn, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_tlbCsr_satp_changed, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_tlbCsr_vsatp_mode, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_tlbCsr_vsatp_asid, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_tlbCsr_vsatp_ppn, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_tlbCsr_vsatp_changed, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_tlbCsr_hgatp_mode, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_tlbCsr_hgatp_vmid, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_tlbCsr_hgatp_ppn, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_tlbCsr_hgatp_changed, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_tlbCsr_mbmc_BME, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_tlbCsr_mbmc_CMODE, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_tlbCsr_mbmc_BCLEAR, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_tlbCsr_mbmc_BMA, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_tlbCsr_priv_mxr, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_tlbCsr_priv_sum, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_tlbCsr_priv_vmxr, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_tlbCsr_priv_vsum, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_tlbCsr_priv_virt, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_tlbCsr_priv_virt_changed, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_tlbCsr_priv_spvp, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_tlbCsr_priv_imode, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_tlbCsr_priv_dmode, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_tlbCsr_mPBMTE, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_tlbCsr_hPBMTE, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_tlbCsr_pmm_mseccfg, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_tlbCsr_pmm_menvcfg, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_tlbCsr_pmm_henvcfg, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_tlbCsr_pmm_hstatus, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_tlbCsr_pmm_senvcfg, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_bp_ctrl_ubtbEnable, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_bp_ctrl_abtbEnable, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_bp_ctrl_mbtbEnable, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_bp_ctrl_tageEnable, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_bp_ctrl_scEnable, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_bp_ctrl_ittageEnable, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_sbuffer_timeout, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_ldld_vio_check_enable, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_cache_error_enable, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_power_down_enable, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_flush_l2_enable, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_distribute_csr_w_valid, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_timing, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_execute, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_store, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_load, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_timing, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_csrCtrl_fsIsOff, UVM_ALL_ON);

    `uvm_object_utils_end

endclass:csr_ctrl_agent_agent_xaction

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_tlbCsr_satp_mode_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_tlbCsr_satp_asid_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_tlbCsr_satp_ppn_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_tlbCsr_satp_changed_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_tlbCsr_vsatp_mode_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_tlbCsr_vsatp_asid_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_tlbCsr_vsatp_ppn_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_tlbCsr_vsatp_changed_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_tlbCsr_hgatp_mode_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_tlbCsr_hgatp_vmid_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_tlbCsr_hgatp_ppn_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_tlbCsr_hgatp_changed_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_tlbCsr_mbmc_BME_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_tlbCsr_mbmc_CMODE_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_tlbCsr_mbmc_BCLEAR_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_tlbCsr_mbmc_BMA_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_tlbCsr_priv_mxr_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_tlbCsr_priv_sum_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_tlbCsr_priv_vmxr_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_tlbCsr_priv_vsum_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_tlbCsr_priv_virt_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_tlbCsr_priv_virt_changed_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_tlbCsr_priv_spvp_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_tlbCsr_priv_imode_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_tlbCsr_priv_dmode_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_tlbCsr_mPBMTE_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_tlbCsr_hPBMTE_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_tlbCsr_pmm_mseccfg_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_tlbCsr_pmm_menvcfg_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_tlbCsr_pmm_henvcfg_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_tlbCsr_pmm_hstatus_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_tlbCsr_pmm_senvcfg_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_bp_ctrl_ubtbEnable_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_bp_ctrl_abtbEnable_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_bp_ctrl_mbtbEnable_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_bp_ctrl_tageEnable_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_bp_ctrl_scEnable_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_bp_ctrl_ittageEnable_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_sbuffer_timeout_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_ldld_vio_check_enable_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_cache_error_enable_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_power_down_enable_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_flush_l2_enable_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_distribute_csr_w_valid_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_timing_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_execute_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_store_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_load_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_timing_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp_cons{

}

constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_fsIsOff_cons{

}

function csr_ctrl_agent_agent_xaction::new(string name = "csr_ctrl_agent_agent_xaction");
    super.new();
endfunction:new

function void csr_ctrl_agent_agent_xaction::pack();
    super.pack();
endfunction:pack
function void csr_ctrl_agent_agent_xaction::unpack();
    super.unpack();
endfunction:unpack
function void csr_ctrl_agent_agent_xaction::pre_randomize();
    super.pre_randomize();
endfunction:pre_randomize
function void csr_ctrl_agent_agent_xaction::post_randomize();
    super.post_randomize();
    //this.pack();
endfunction:post_randomize

function string csr_ctrl_agent_agent_xaction::psdisplay(string prefix = "");
    string pkt_str;
    pkt_str = $sformatf("%s for packet[%0d] >>>>",prefix,this.pkt_index);
    pkt_str = $sformatf("%schannel_id=%0d ",pkt_str,this.channel_id);
    pkt_str = $sformatf("%sstart=%0f finish=%0f >>>>\n",pkt_str,this.start,this.finish);
    //foreach(this.pload_q[i]) begin
    //    pkt_str = $sformatf("%spload_q[%0d]=0x%2h  ",pkt_str,i,this.pload_q[i]);
    //end
    pkt_str = $sformatf("%sio_ooo_to_mem_tlbCsr_satp_mode = 0x%0h ",pkt_str,this.io_ooo_to_mem_tlbCsr_satp_mode);
    pkt_str = $sformatf("%sio_ooo_to_mem_tlbCsr_satp_asid = 0x%0h ",pkt_str,this.io_ooo_to_mem_tlbCsr_satp_asid);
    pkt_str = $sformatf("%sio_ooo_to_mem_tlbCsr_satp_ppn = 0x%0h ",pkt_str,this.io_ooo_to_mem_tlbCsr_satp_ppn);
    pkt_str = $sformatf("%sio_ooo_to_mem_tlbCsr_satp_changed = 0x%0h ",pkt_str,this.io_ooo_to_mem_tlbCsr_satp_changed);
    pkt_str = $sformatf("%sio_ooo_to_mem_tlbCsr_vsatp_mode = 0x%0h ",pkt_str,this.io_ooo_to_mem_tlbCsr_vsatp_mode);
    pkt_str = $sformatf("%sio_ooo_to_mem_tlbCsr_vsatp_asid = 0x%0h ",pkt_str,this.io_ooo_to_mem_tlbCsr_vsatp_asid);
    pkt_str = $sformatf("%sio_ooo_to_mem_tlbCsr_vsatp_ppn = 0x%0h ",pkt_str,this.io_ooo_to_mem_tlbCsr_vsatp_ppn);
    pkt_str = $sformatf("%sio_ooo_to_mem_tlbCsr_vsatp_changed = 0x%0h ",pkt_str,this.io_ooo_to_mem_tlbCsr_vsatp_changed);
    pkt_str = $sformatf("%sio_ooo_to_mem_tlbCsr_hgatp_mode = 0x%0h ",pkt_str,this.io_ooo_to_mem_tlbCsr_hgatp_mode);
    pkt_str = $sformatf("%sio_ooo_to_mem_tlbCsr_hgatp_vmid = 0x%0h ",pkt_str,this.io_ooo_to_mem_tlbCsr_hgatp_vmid);
    pkt_str = $sformatf("%sio_ooo_to_mem_tlbCsr_hgatp_ppn = 0x%0h ",pkt_str,this.io_ooo_to_mem_tlbCsr_hgatp_ppn);
    pkt_str = $sformatf("%sio_ooo_to_mem_tlbCsr_hgatp_changed = 0x%0h ",pkt_str,this.io_ooo_to_mem_tlbCsr_hgatp_changed);
    pkt_str = $sformatf("%sio_ooo_to_mem_tlbCsr_mbmc_BME = 0x%0h ",pkt_str,this.io_ooo_to_mem_tlbCsr_mbmc_BME);
    pkt_str = $sformatf("%sio_ooo_to_mem_tlbCsr_mbmc_CMODE = 0x%0h ",pkt_str,this.io_ooo_to_mem_tlbCsr_mbmc_CMODE);
    pkt_str = $sformatf("%sio_ooo_to_mem_tlbCsr_mbmc_BCLEAR = 0x%0h ",pkt_str,this.io_ooo_to_mem_tlbCsr_mbmc_BCLEAR);
    pkt_str = $sformatf("%sio_ooo_to_mem_tlbCsr_mbmc_BMA = 0x%0h ",pkt_str,this.io_ooo_to_mem_tlbCsr_mbmc_BMA);
    pkt_str = $sformatf("%sio_ooo_to_mem_tlbCsr_priv_mxr = 0x%0h ",pkt_str,this.io_ooo_to_mem_tlbCsr_priv_mxr);
    pkt_str = $sformatf("%sio_ooo_to_mem_tlbCsr_priv_sum = 0x%0h ",pkt_str,this.io_ooo_to_mem_tlbCsr_priv_sum);
    pkt_str = $sformatf("%sio_ooo_to_mem_tlbCsr_priv_vmxr = 0x%0h ",pkt_str,this.io_ooo_to_mem_tlbCsr_priv_vmxr);
    pkt_str = $sformatf("%sio_ooo_to_mem_tlbCsr_priv_vsum = 0x%0h ",pkt_str,this.io_ooo_to_mem_tlbCsr_priv_vsum);
    pkt_str = $sformatf("%sio_ooo_to_mem_tlbCsr_priv_virt = 0x%0h ",pkt_str,this.io_ooo_to_mem_tlbCsr_priv_virt);
    pkt_str = $sformatf("%sio_ooo_to_mem_tlbCsr_priv_virt_changed = 0x%0h ",pkt_str,this.io_ooo_to_mem_tlbCsr_priv_virt_changed);
    pkt_str = $sformatf("%sio_ooo_to_mem_tlbCsr_priv_spvp = 0x%0h ",pkt_str,this.io_ooo_to_mem_tlbCsr_priv_spvp);
    pkt_str = $sformatf("%sio_ooo_to_mem_tlbCsr_priv_imode = 0x%0h ",pkt_str,this.io_ooo_to_mem_tlbCsr_priv_imode);
    pkt_str = $sformatf("%sio_ooo_to_mem_tlbCsr_priv_dmode = 0x%0h ",pkt_str,this.io_ooo_to_mem_tlbCsr_priv_dmode);
    pkt_str = $sformatf("%sio_ooo_to_mem_tlbCsr_mPBMTE = 0x%0h ",pkt_str,this.io_ooo_to_mem_tlbCsr_mPBMTE);
    pkt_str = $sformatf("%sio_ooo_to_mem_tlbCsr_hPBMTE = 0x%0h ",pkt_str,this.io_ooo_to_mem_tlbCsr_hPBMTE);
    pkt_str = $sformatf("%sio_ooo_to_mem_tlbCsr_pmm_mseccfg = 0x%0h ",pkt_str,this.io_ooo_to_mem_tlbCsr_pmm_mseccfg);
    pkt_str = $sformatf("%sio_ooo_to_mem_tlbCsr_pmm_menvcfg = 0x%0h ",pkt_str,this.io_ooo_to_mem_tlbCsr_pmm_menvcfg);
    pkt_str = $sformatf("%sio_ooo_to_mem_tlbCsr_pmm_henvcfg = 0x%0h ",pkt_str,this.io_ooo_to_mem_tlbCsr_pmm_henvcfg);
    pkt_str = $sformatf("%sio_ooo_to_mem_tlbCsr_pmm_hstatus = 0x%0h ",pkt_str,this.io_ooo_to_mem_tlbCsr_pmm_hstatus);
    pkt_str = $sformatf("%sio_ooo_to_mem_tlbCsr_pmm_senvcfg = 0x%0h ",pkt_str,this.io_ooo_to_mem_tlbCsr_pmm_senvcfg);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_bp_ctrl_ubtbEnable = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_bp_ctrl_ubtbEnable);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_bp_ctrl_abtbEnable = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_bp_ctrl_abtbEnable);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_bp_ctrl_mbtbEnable = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_bp_ctrl_mbtbEnable);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_bp_ctrl_tageEnable = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_bp_ctrl_tageEnable);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_bp_ctrl_scEnable = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_bp_ctrl_scEnable);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_bp_ctrl_ittageEnable = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_bp_ctrl_ittageEnable);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_sbuffer_timeout = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_sbuffer_timeout);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_ldld_vio_check_enable = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_ldld_vio_check_enable);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_cache_error_enable = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_cache_error_enable);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_power_down_enable = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_power_down_enable);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_flush_l2_enable = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_flush_l2_enable);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_distribute_csr_w_valid = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_timing = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_timing);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_execute = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_execute);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_store = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_store);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_load = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_load);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2 = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0 = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1 = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2 = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3 = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_timing = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_timing);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2 = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0 = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1 = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2 = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3 = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_fsIsOff = 0x%0h ",pkt_str,this.io_ooo_to_mem_csrCtrl_fsIsOff);

    return pkt_str;
endfunction:psdisplay

function bit csr_ctrl_agent_agent_xaction::compare(uvm_object rhs, uvm_comparer comparer=null);
    bit super_result;
    csr_ctrl_agent_agent_xaction  rhs_;
    if(!$cast(rhs_, rhs)) begin
        `uvm_fatal(get_type_name(),$sformatf("rhs is not a csr_ctrl_agent_agent_xaction or its extend"))
    end
    super_result = super.compare(rhs_,comparer);
    if(super_result==0) begin
        super_result = 1;
        //foreach(this.pload_q[i]) begin
        //    if(this.pload_q[i]!=rhs_.pload_q[i]) begin
        //        super_result = 0;
        //        `uvm_info(get_type_name(),$sformatf("compare fail for this.pload[%0d]=0x%2h while the rhs_.pload[%0d]=0x%2h",i,this.pload_q[i],i,rhs_.pload_q[i]),UVM_NONE)
        //    end
        //end

        if(this.io_ooo_to_mem_tlbCsr_satp_mode!=rhs_.io_ooo_to_mem_tlbCsr_satp_mode) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_tlbCsr_satp_mode=0x%0h while the rhs_.io_ooo_to_mem_tlbCsr_satp_mode=0x%0h",this.io_ooo_to_mem_tlbCsr_satp_mode,rhs_.io_ooo_to_mem_tlbCsr_satp_mode),UVM_NONE)
        end

        if(this.io_ooo_to_mem_tlbCsr_satp_asid!=rhs_.io_ooo_to_mem_tlbCsr_satp_asid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_tlbCsr_satp_asid=0x%0h while the rhs_.io_ooo_to_mem_tlbCsr_satp_asid=0x%0h",this.io_ooo_to_mem_tlbCsr_satp_asid,rhs_.io_ooo_to_mem_tlbCsr_satp_asid),UVM_NONE)
        end

        if(this.io_ooo_to_mem_tlbCsr_satp_ppn!=rhs_.io_ooo_to_mem_tlbCsr_satp_ppn) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_tlbCsr_satp_ppn=0x%0h while the rhs_.io_ooo_to_mem_tlbCsr_satp_ppn=0x%0h",this.io_ooo_to_mem_tlbCsr_satp_ppn,rhs_.io_ooo_to_mem_tlbCsr_satp_ppn),UVM_NONE)
        end

        if(this.io_ooo_to_mem_tlbCsr_satp_changed!=rhs_.io_ooo_to_mem_tlbCsr_satp_changed) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_tlbCsr_satp_changed=0x%0h while the rhs_.io_ooo_to_mem_tlbCsr_satp_changed=0x%0h",this.io_ooo_to_mem_tlbCsr_satp_changed,rhs_.io_ooo_to_mem_tlbCsr_satp_changed),UVM_NONE)
        end

        if(this.io_ooo_to_mem_tlbCsr_vsatp_mode!=rhs_.io_ooo_to_mem_tlbCsr_vsatp_mode) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_tlbCsr_vsatp_mode=0x%0h while the rhs_.io_ooo_to_mem_tlbCsr_vsatp_mode=0x%0h",this.io_ooo_to_mem_tlbCsr_vsatp_mode,rhs_.io_ooo_to_mem_tlbCsr_vsatp_mode),UVM_NONE)
        end

        if(this.io_ooo_to_mem_tlbCsr_vsatp_asid!=rhs_.io_ooo_to_mem_tlbCsr_vsatp_asid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_tlbCsr_vsatp_asid=0x%0h while the rhs_.io_ooo_to_mem_tlbCsr_vsatp_asid=0x%0h",this.io_ooo_to_mem_tlbCsr_vsatp_asid,rhs_.io_ooo_to_mem_tlbCsr_vsatp_asid),UVM_NONE)
        end

        if(this.io_ooo_to_mem_tlbCsr_vsatp_ppn!=rhs_.io_ooo_to_mem_tlbCsr_vsatp_ppn) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_tlbCsr_vsatp_ppn=0x%0h while the rhs_.io_ooo_to_mem_tlbCsr_vsatp_ppn=0x%0h",this.io_ooo_to_mem_tlbCsr_vsatp_ppn,rhs_.io_ooo_to_mem_tlbCsr_vsatp_ppn),UVM_NONE)
        end

        if(this.io_ooo_to_mem_tlbCsr_vsatp_changed!=rhs_.io_ooo_to_mem_tlbCsr_vsatp_changed) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_tlbCsr_vsatp_changed=0x%0h while the rhs_.io_ooo_to_mem_tlbCsr_vsatp_changed=0x%0h",this.io_ooo_to_mem_tlbCsr_vsatp_changed,rhs_.io_ooo_to_mem_tlbCsr_vsatp_changed),UVM_NONE)
        end

        if(this.io_ooo_to_mem_tlbCsr_hgatp_mode!=rhs_.io_ooo_to_mem_tlbCsr_hgatp_mode) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_tlbCsr_hgatp_mode=0x%0h while the rhs_.io_ooo_to_mem_tlbCsr_hgatp_mode=0x%0h",this.io_ooo_to_mem_tlbCsr_hgatp_mode,rhs_.io_ooo_to_mem_tlbCsr_hgatp_mode),UVM_NONE)
        end

        if(this.io_ooo_to_mem_tlbCsr_hgatp_vmid!=rhs_.io_ooo_to_mem_tlbCsr_hgatp_vmid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_tlbCsr_hgatp_vmid=0x%0h while the rhs_.io_ooo_to_mem_tlbCsr_hgatp_vmid=0x%0h",this.io_ooo_to_mem_tlbCsr_hgatp_vmid,rhs_.io_ooo_to_mem_tlbCsr_hgatp_vmid),UVM_NONE)
        end

        if(this.io_ooo_to_mem_tlbCsr_hgatp_ppn!=rhs_.io_ooo_to_mem_tlbCsr_hgatp_ppn) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_tlbCsr_hgatp_ppn=0x%0h while the rhs_.io_ooo_to_mem_tlbCsr_hgatp_ppn=0x%0h",this.io_ooo_to_mem_tlbCsr_hgatp_ppn,rhs_.io_ooo_to_mem_tlbCsr_hgatp_ppn),UVM_NONE)
        end

        if(this.io_ooo_to_mem_tlbCsr_hgatp_changed!=rhs_.io_ooo_to_mem_tlbCsr_hgatp_changed) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_tlbCsr_hgatp_changed=0x%0h while the rhs_.io_ooo_to_mem_tlbCsr_hgatp_changed=0x%0h",this.io_ooo_to_mem_tlbCsr_hgatp_changed,rhs_.io_ooo_to_mem_tlbCsr_hgatp_changed),UVM_NONE)
        end

        if(this.io_ooo_to_mem_tlbCsr_mbmc_BME!=rhs_.io_ooo_to_mem_tlbCsr_mbmc_BME) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_tlbCsr_mbmc_BME=0x%0h while the rhs_.io_ooo_to_mem_tlbCsr_mbmc_BME=0x%0h",this.io_ooo_to_mem_tlbCsr_mbmc_BME,rhs_.io_ooo_to_mem_tlbCsr_mbmc_BME),UVM_NONE)
        end

        if(this.io_ooo_to_mem_tlbCsr_mbmc_CMODE!=rhs_.io_ooo_to_mem_tlbCsr_mbmc_CMODE) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_tlbCsr_mbmc_CMODE=0x%0h while the rhs_.io_ooo_to_mem_tlbCsr_mbmc_CMODE=0x%0h",this.io_ooo_to_mem_tlbCsr_mbmc_CMODE,rhs_.io_ooo_to_mem_tlbCsr_mbmc_CMODE),UVM_NONE)
        end

        if(this.io_ooo_to_mem_tlbCsr_mbmc_BCLEAR!=rhs_.io_ooo_to_mem_tlbCsr_mbmc_BCLEAR) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_tlbCsr_mbmc_BCLEAR=0x%0h while the rhs_.io_ooo_to_mem_tlbCsr_mbmc_BCLEAR=0x%0h",this.io_ooo_to_mem_tlbCsr_mbmc_BCLEAR,rhs_.io_ooo_to_mem_tlbCsr_mbmc_BCLEAR),UVM_NONE)
        end

        if(this.io_ooo_to_mem_tlbCsr_mbmc_BMA!=rhs_.io_ooo_to_mem_tlbCsr_mbmc_BMA) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_tlbCsr_mbmc_BMA=0x%0h while the rhs_.io_ooo_to_mem_tlbCsr_mbmc_BMA=0x%0h",this.io_ooo_to_mem_tlbCsr_mbmc_BMA,rhs_.io_ooo_to_mem_tlbCsr_mbmc_BMA),UVM_NONE)
        end

        if(this.io_ooo_to_mem_tlbCsr_priv_mxr!=rhs_.io_ooo_to_mem_tlbCsr_priv_mxr) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_tlbCsr_priv_mxr=0x%0h while the rhs_.io_ooo_to_mem_tlbCsr_priv_mxr=0x%0h",this.io_ooo_to_mem_tlbCsr_priv_mxr,rhs_.io_ooo_to_mem_tlbCsr_priv_mxr),UVM_NONE)
        end

        if(this.io_ooo_to_mem_tlbCsr_priv_sum!=rhs_.io_ooo_to_mem_tlbCsr_priv_sum) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_tlbCsr_priv_sum=0x%0h while the rhs_.io_ooo_to_mem_tlbCsr_priv_sum=0x%0h",this.io_ooo_to_mem_tlbCsr_priv_sum,rhs_.io_ooo_to_mem_tlbCsr_priv_sum),UVM_NONE)
        end

        if(this.io_ooo_to_mem_tlbCsr_priv_vmxr!=rhs_.io_ooo_to_mem_tlbCsr_priv_vmxr) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_tlbCsr_priv_vmxr=0x%0h while the rhs_.io_ooo_to_mem_tlbCsr_priv_vmxr=0x%0h",this.io_ooo_to_mem_tlbCsr_priv_vmxr,rhs_.io_ooo_to_mem_tlbCsr_priv_vmxr),UVM_NONE)
        end

        if(this.io_ooo_to_mem_tlbCsr_priv_vsum!=rhs_.io_ooo_to_mem_tlbCsr_priv_vsum) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_tlbCsr_priv_vsum=0x%0h while the rhs_.io_ooo_to_mem_tlbCsr_priv_vsum=0x%0h",this.io_ooo_to_mem_tlbCsr_priv_vsum,rhs_.io_ooo_to_mem_tlbCsr_priv_vsum),UVM_NONE)
        end

        if(this.io_ooo_to_mem_tlbCsr_priv_virt!=rhs_.io_ooo_to_mem_tlbCsr_priv_virt) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_tlbCsr_priv_virt=0x%0h while the rhs_.io_ooo_to_mem_tlbCsr_priv_virt=0x%0h",this.io_ooo_to_mem_tlbCsr_priv_virt,rhs_.io_ooo_to_mem_tlbCsr_priv_virt),UVM_NONE)
        end

        if(this.io_ooo_to_mem_tlbCsr_priv_virt_changed!=rhs_.io_ooo_to_mem_tlbCsr_priv_virt_changed) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_tlbCsr_priv_virt_changed=0x%0h while the rhs_.io_ooo_to_mem_tlbCsr_priv_virt_changed=0x%0h",this.io_ooo_to_mem_tlbCsr_priv_virt_changed,rhs_.io_ooo_to_mem_tlbCsr_priv_virt_changed),UVM_NONE)
        end

        if(this.io_ooo_to_mem_tlbCsr_priv_spvp!=rhs_.io_ooo_to_mem_tlbCsr_priv_spvp) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_tlbCsr_priv_spvp=0x%0h while the rhs_.io_ooo_to_mem_tlbCsr_priv_spvp=0x%0h",this.io_ooo_to_mem_tlbCsr_priv_spvp,rhs_.io_ooo_to_mem_tlbCsr_priv_spvp),UVM_NONE)
        end

        if(this.io_ooo_to_mem_tlbCsr_priv_imode!=rhs_.io_ooo_to_mem_tlbCsr_priv_imode) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_tlbCsr_priv_imode=0x%0h while the rhs_.io_ooo_to_mem_tlbCsr_priv_imode=0x%0h",this.io_ooo_to_mem_tlbCsr_priv_imode,rhs_.io_ooo_to_mem_tlbCsr_priv_imode),UVM_NONE)
        end

        if(this.io_ooo_to_mem_tlbCsr_priv_dmode!=rhs_.io_ooo_to_mem_tlbCsr_priv_dmode) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_tlbCsr_priv_dmode=0x%0h while the rhs_.io_ooo_to_mem_tlbCsr_priv_dmode=0x%0h",this.io_ooo_to_mem_tlbCsr_priv_dmode,rhs_.io_ooo_to_mem_tlbCsr_priv_dmode),UVM_NONE)
        end

        if(this.io_ooo_to_mem_tlbCsr_mPBMTE!=rhs_.io_ooo_to_mem_tlbCsr_mPBMTE) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_tlbCsr_mPBMTE=0x%0h while the rhs_.io_ooo_to_mem_tlbCsr_mPBMTE=0x%0h",this.io_ooo_to_mem_tlbCsr_mPBMTE,rhs_.io_ooo_to_mem_tlbCsr_mPBMTE),UVM_NONE)
        end

        if(this.io_ooo_to_mem_tlbCsr_hPBMTE!=rhs_.io_ooo_to_mem_tlbCsr_hPBMTE) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_tlbCsr_hPBMTE=0x%0h while the rhs_.io_ooo_to_mem_tlbCsr_hPBMTE=0x%0h",this.io_ooo_to_mem_tlbCsr_hPBMTE,rhs_.io_ooo_to_mem_tlbCsr_hPBMTE),UVM_NONE)
        end

        if(this.io_ooo_to_mem_tlbCsr_pmm_mseccfg!=rhs_.io_ooo_to_mem_tlbCsr_pmm_mseccfg) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_tlbCsr_pmm_mseccfg=0x%0h while the rhs_.io_ooo_to_mem_tlbCsr_pmm_mseccfg=0x%0h",this.io_ooo_to_mem_tlbCsr_pmm_mseccfg,rhs_.io_ooo_to_mem_tlbCsr_pmm_mseccfg),UVM_NONE)
        end

        if(this.io_ooo_to_mem_tlbCsr_pmm_menvcfg!=rhs_.io_ooo_to_mem_tlbCsr_pmm_menvcfg) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_tlbCsr_pmm_menvcfg=0x%0h while the rhs_.io_ooo_to_mem_tlbCsr_pmm_menvcfg=0x%0h",this.io_ooo_to_mem_tlbCsr_pmm_menvcfg,rhs_.io_ooo_to_mem_tlbCsr_pmm_menvcfg),UVM_NONE)
        end

        if(this.io_ooo_to_mem_tlbCsr_pmm_henvcfg!=rhs_.io_ooo_to_mem_tlbCsr_pmm_henvcfg) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_tlbCsr_pmm_henvcfg=0x%0h while the rhs_.io_ooo_to_mem_tlbCsr_pmm_henvcfg=0x%0h",this.io_ooo_to_mem_tlbCsr_pmm_henvcfg,rhs_.io_ooo_to_mem_tlbCsr_pmm_henvcfg),UVM_NONE)
        end

        if(this.io_ooo_to_mem_tlbCsr_pmm_hstatus!=rhs_.io_ooo_to_mem_tlbCsr_pmm_hstatus) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_tlbCsr_pmm_hstatus=0x%0h while the rhs_.io_ooo_to_mem_tlbCsr_pmm_hstatus=0x%0h",this.io_ooo_to_mem_tlbCsr_pmm_hstatus,rhs_.io_ooo_to_mem_tlbCsr_pmm_hstatus),UVM_NONE)
        end

        if(this.io_ooo_to_mem_tlbCsr_pmm_senvcfg!=rhs_.io_ooo_to_mem_tlbCsr_pmm_senvcfg) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_tlbCsr_pmm_senvcfg=0x%0h while the rhs_.io_ooo_to_mem_tlbCsr_pmm_senvcfg=0x%0h",this.io_ooo_to_mem_tlbCsr_pmm_senvcfg,rhs_.io_ooo_to_mem_tlbCsr_pmm_senvcfg),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable!=rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable=0x%0h",this.io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable,rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable!=rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable=0x%0h",this.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable,rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable!=rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable=0x%0h",this.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable,rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit!=rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit=0x%0h",this.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit,rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt!=rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt=0x%0h",this.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt,rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht!=rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht=0x%0h",this.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht,rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold!=rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold=0x%0h",this.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold,rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride!=rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride=0x%0h",this.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride,rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride!=rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride=0x%0h",this.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride,rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only!=rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only=0x%0h",this.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only,rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable!=rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable=0x%0h",this.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable,rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable!=rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable=0x%0h",this.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable,rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable!=rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable=0x%0h",this.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable,rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable!=rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable=0x%0h",this.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable,rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency!=rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency=0x%0h",this.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency,rhs_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_bp_ctrl_ubtbEnable!=rhs_.io_ooo_to_mem_csrCtrl_bp_ctrl_ubtbEnable) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_bp_ctrl_ubtbEnable=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_bp_ctrl_ubtbEnable=0x%0h",this.io_ooo_to_mem_csrCtrl_bp_ctrl_ubtbEnable,rhs_.io_ooo_to_mem_csrCtrl_bp_ctrl_ubtbEnable),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_bp_ctrl_abtbEnable!=rhs_.io_ooo_to_mem_csrCtrl_bp_ctrl_abtbEnable) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_bp_ctrl_abtbEnable=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_bp_ctrl_abtbEnable=0x%0h",this.io_ooo_to_mem_csrCtrl_bp_ctrl_abtbEnable,rhs_.io_ooo_to_mem_csrCtrl_bp_ctrl_abtbEnable),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_bp_ctrl_mbtbEnable!=rhs_.io_ooo_to_mem_csrCtrl_bp_ctrl_mbtbEnable) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_bp_ctrl_mbtbEnable=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_bp_ctrl_mbtbEnable=0x%0h",this.io_ooo_to_mem_csrCtrl_bp_ctrl_mbtbEnable,rhs_.io_ooo_to_mem_csrCtrl_bp_ctrl_mbtbEnable),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_bp_ctrl_tageEnable!=rhs_.io_ooo_to_mem_csrCtrl_bp_ctrl_tageEnable) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_bp_ctrl_tageEnable=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_bp_ctrl_tageEnable=0x%0h",this.io_ooo_to_mem_csrCtrl_bp_ctrl_tageEnable,rhs_.io_ooo_to_mem_csrCtrl_bp_ctrl_tageEnable),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_bp_ctrl_scEnable!=rhs_.io_ooo_to_mem_csrCtrl_bp_ctrl_scEnable) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_bp_ctrl_scEnable=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_bp_ctrl_scEnable=0x%0h",this.io_ooo_to_mem_csrCtrl_bp_ctrl_scEnable,rhs_.io_ooo_to_mem_csrCtrl_bp_ctrl_scEnable),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_bp_ctrl_ittageEnable!=rhs_.io_ooo_to_mem_csrCtrl_bp_ctrl_ittageEnable) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_bp_ctrl_ittageEnable=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_bp_ctrl_ittageEnable=0x%0h",this.io_ooo_to_mem_csrCtrl_bp_ctrl_ittageEnable,rhs_.io_ooo_to_mem_csrCtrl_bp_ctrl_ittageEnable),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_sbuffer_timeout!=rhs_.io_ooo_to_mem_csrCtrl_sbuffer_timeout) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_sbuffer_timeout=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_sbuffer_timeout=0x%0h",this.io_ooo_to_mem_csrCtrl_sbuffer_timeout,rhs_.io_ooo_to_mem_csrCtrl_sbuffer_timeout),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_ldld_vio_check_enable!=rhs_.io_ooo_to_mem_csrCtrl_ldld_vio_check_enable) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_ldld_vio_check_enable=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_ldld_vio_check_enable=0x%0h",this.io_ooo_to_mem_csrCtrl_ldld_vio_check_enable,rhs_.io_ooo_to_mem_csrCtrl_ldld_vio_check_enable),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_cache_error_enable!=rhs_.io_ooo_to_mem_csrCtrl_cache_error_enable) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_cache_error_enable=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_cache_error_enable=0x%0h",this.io_ooo_to_mem_csrCtrl_cache_error_enable,rhs_.io_ooo_to_mem_csrCtrl_cache_error_enable),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable!=rhs_.io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable=0x%0h",this.io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable,rhs_.io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_power_down_enable!=rhs_.io_ooo_to_mem_csrCtrl_power_down_enable) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_power_down_enable=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_power_down_enable=0x%0h",this.io_ooo_to_mem_csrCtrl_power_down_enable,rhs_.io_ooo_to_mem_csrCtrl_power_down_enable),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_flush_l2_enable!=rhs_.io_ooo_to_mem_csrCtrl_flush_l2_enable) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_flush_l2_enable=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_flush_l2_enable=0x%0h",this.io_ooo_to_mem_csrCtrl_flush_l2_enable,rhs_.io_ooo_to_mem_csrCtrl_flush_l2_enable),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid!=rhs_.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid=0x%0h",this.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid,rhs_.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr!=rhs_.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr=0x%0h",this.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr,rhs_.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data!=rhs_.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data=0x%0h",this.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data,rhs_.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid!=rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid=0x%0h",this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid,rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr!=rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr=0x%0h",this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr,rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType!=rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType=0x%0h",this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType,rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select!=rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select=0x%0h",this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select,rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_timing!=rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_timing) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_timing=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_timing=0x%0h",this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_timing,rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_timing),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action!=rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action=0x%0h",this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action,rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain!=rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain=0x%0h",this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain,rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_execute!=rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_execute) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_execute=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_execute=0x%0h",this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_execute,rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_execute),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_store!=rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_store) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_store=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_store=0x%0h",this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_store,rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_store),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_load!=rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_load) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_load=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_load=0x%0h",this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_load,rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_load),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2!=rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2=0x%0h",this.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2,rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0!=rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0=0x%0h",this.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0,rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1!=rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1=0x%0h",this.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1,rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2!=rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2=0x%0h",this.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2,rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3!=rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3=0x%0h",this.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3,rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp!=rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp=0x%0h",this.io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp,rhs_.io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid!=rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid=0x%0h",this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid,rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr!=rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr=0x%0h",this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr,rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType!=rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType=0x%0h",this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType,rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select!=rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select=0x%0h",this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select,rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_timing!=rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_timing) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_timing=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_timing=0x%0h",this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_timing,rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_timing),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action!=rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action=0x%0h",this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action,rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain!=rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain=0x%0h",this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain,rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store!=rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store=0x%0h",this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store,rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load!=rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load=0x%0h",this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load,rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2!=rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2=0x%0h",this.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2,rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0!=rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0=0x%0h",this.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0,rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1!=rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1=0x%0h",this.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1,rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2!=rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2=0x%0h",this.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2,rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3!=rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3=0x%0h",this.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3,rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp!=rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp=0x%0h",this.io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp,rhs_.io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp),UVM_NONE)
        end

        if(this.io_ooo_to_mem_csrCtrl_fsIsOff!=rhs_.io_ooo_to_mem_csrCtrl_fsIsOff) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_csrCtrl_fsIsOff=0x%0h while the rhs_.io_ooo_to_mem_csrCtrl_fsIsOff=0x%0h",this.io_ooo_to_mem_csrCtrl_fsIsOff,rhs_.io_ooo_to_mem_csrCtrl_fsIsOff),UVM_NONE)
        end

    end
    return super_result;
endfunction:compare

`endif
