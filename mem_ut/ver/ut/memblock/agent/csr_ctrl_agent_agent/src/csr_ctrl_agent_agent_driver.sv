//=========================================================
//File name    : csr_ctrl_agent_agent_driver.sv
//Author       : OpenAI_Codex
//Module name  : csr_ctrl_agent_agent_driver
//Discribution : csr_ctrl_agent_agent_driver : driver
//Date         : 2026-04-12
//=========================================================
`ifndef CSR_CTRL_AGENT_AGENT_DRIVER__SV
`define CSR_CTRL_AGENT_AGENT_DRIVER__SV

class csr_ctrl_agent_agent_driver  extends tcnt_driver_base#(virtual csr_ctrl_agent_agent_interface,csr_ctrl_agent_agent_cfg,csr_ctrl_agent_agent_xaction);

    `uvm_component_utils(csr_ctrl_agent_agent_driver)

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern virtual task reset_phase(uvm_phase phase);
    extern task main_phase(uvm_phase phase);
    extern task send_pkt(csr_ctrl_agent_agent_xaction tr);
    extern task drive_idle(tcnt_dec_base::drv_mode_e drv_mode);
endclass:csr_ctrl_agent_agent_driver

function csr_ctrl_agent_agent_driver::new(string name, uvm_component parent);
    super.new(name,parent);
endfunction:new

function void csr_ctrl_agent_agent_driver::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

task csr_ctrl_agent_agent_driver::reset_phase(uvm_phase phase);

    super.reset_phase(phase);
    phase.raise_objection(this);

    repeat(2) begin
        @this.vif.drv_mp.drv_cb;
        this.drive_idle(this.cfg.drv_mode);
    end
    wait(vif.rst_n == 1'b1);
    wait(memblock_sync_pkg::reset_backend_done == 1'b1);
    repeat(20) begin
        @this.vif.drv_mp.drv_cb;
        this.drive_idle(this.cfg.drv_mode);
    end

    phase.drop_objection(this);
endtask:reset_phase

task csr_ctrl_agent_agent_driver::main_phase(uvm_phase phase);
    super.main_phase(phase);
    //while(1) begin
    if(this.cfg.sqr_sw==tcnt_dec_base::ON && this.cfg.drv_sw==tcnt_dec_base::ON) begin
        while(1) begin
            seq_item_port.try_next_item(req);
            if(req!=null) begin
                repeat(req.pre_pkt_gap) begin
                    @this.vif.drv_mp.drv_cb;
                    this.drive_idle(this.cfg.drv_mode);
                end
                @this.vif.drv_mp.drv_cb;
                this.send_pkt(req);
                repeat(req.post_pkt_gap) begin
                    @this.vif.drv_mp.drv_cb;
                    this.drive_idle(this.cfg.drv_mode);
                end
                seq_item_port.item_done();
            end
            else begin
                @this.vif.drv_mp.drv_cb;
                this.drive_idle(this.cfg.drv_mode);
            end
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

task csr_ctrl_agent_agent_driver::send_pkt(csr_ctrl_agent_agent_xaction tr);
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_mode <= tr.io_ooo_to_mem_tlbCsr_satp_mode;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_asid <= tr.io_ooo_to_mem_tlbCsr_satp_asid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_ppn <= tr.io_ooo_to_mem_tlbCsr_satp_ppn;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_changed <= tr.io_ooo_to_mem_tlbCsr_satp_changed;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_mode <= tr.io_ooo_to_mem_tlbCsr_vsatp_mode;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_asid <= tr.io_ooo_to_mem_tlbCsr_vsatp_asid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_ppn <= tr.io_ooo_to_mem_tlbCsr_vsatp_ppn;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_changed <= tr.io_ooo_to_mem_tlbCsr_vsatp_changed;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_mode <= tr.io_ooo_to_mem_tlbCsr_hgatp_mode;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_vmid <= tr.io_ooo_to_mem_tlbCsr_hgatp_vmid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_ppn <= tr.io_ooo_to_mem_tlbCsr_hgatp_ppn;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_changed <= tr.io_ooo_to_mem_tlbCsr_hgatp_changed;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BME <= tr.io_ooo_to_mem_tlbCsr_mbmc_BME;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_CMODE <= tr.io_ooo_to_mem_tlbCsr_mbmc_CMODE;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BCLEAR <= tr.io_ooo_to_mem_tlbCsr_mbmc_BCLEAR;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BMA <= tr.io_ooo_to_mem_tlbCsr_mbmc_BMA;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_mxr <= tr.io_ooo_to_mem_tlbCsr_priv_mxr;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_sum <= tr.io_ooo_to_mem_tlbCsr_priv_sum;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_vmxr <= tr.io_ooo_to_mem_tlbCsr_priv_vmxr;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_vsum <= tr.io_ooo_to_mem_tlbCsr_priv_vsum;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_virt <= tr.io_ooo_to_mem_tlbCsr_priv_virt;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_virt_changed <= tr.io_ooo_to_mem_tlbCsr_priv_virt_changed;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_spvp <= tr.io_ooo_to_mem_tlbCsr_priv_spvp;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_imode <= tr.io_ooo_to_mem_tlbCsr_priv_imode;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_dmode <= tr.io_ooo_to_mem_tlbCsr_priv_dmode;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mPBMTE <= tr.io_ooo_to_mem_tlbCsr_mPBMTE;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hPBMTE <= tr.io_ooo_to_mem_tlbCsr_hPBMTE;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_mseccfg <= tr.io_ooo_to_mem_tlbCsr_pmm_mseccfg;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_menvcfg <= tr.io_ooo_to_mem_tlbCsr_pmm_menvcfg;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_henvcfg <= tr.io_ooo_to_mem_tlbCsr_pmm_henvcfg;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_hstatus <= tr.io_ooo_to_mem_tlbCsr_pmm_hstatus;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_senvcfg <= tr.io_ooo_to_mem_tlbCsr_pmm_senvcfg;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable <= tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable <= tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable <= tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit <= tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt <= tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht <= tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold <= tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride <= tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride <= tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only <= tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable <= tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable <= tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable <= tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable <= tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency <= tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_ubtbEnable <= tr.io_ooo_to_mem_csrCtrl_bp_ctrl_ubtbEnable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_abtbEnable <= tr.io_ooo_to_mem_csrCtrl_bp_ctrl_abtbEnable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_mbtbEnable <= tr.io_ooo_to_mem_csrCtrl_bp_ctrl_mbtbEnable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_tageEnable <= tr.io_ooo_to_mem_csrCtrl_bp_ctrl_tageEnable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_scEnable <= tr.io_ooo_to_mem_csrCtrl_bp_ctrl_scEnable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_ittageEnable <= tr.io_ooo_to_mem_csrCtrl_bp_ctrl_ittageEnable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_sbuffer_timeout <= tr.io_ooo_to_mem_csrCtrl_sbuffer_timeout;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_ldld_vio_check_enable <= tr.io_ooo_to_mem_csrCtrl_ldld_vio_check_enable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_cache_error_enable <= tr.io_ooo_to_mem_csrCtrl_cache_error_enable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable <= tr.io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_power_down_enable <= tr.io_ooo_to_mem_csrCtrl_power_down_enable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_flush_l2_enable <= tr.io_ooo_to_mem_csrCtrl_flush_l2_enable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid <= tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr <= tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data <= tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid <= tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr <= tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType <= tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select <= tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_timing <= tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_timing;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action <= tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain <= tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_execute <= tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_execute;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_store <= tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_store;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_load <= tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_load;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2 <= tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0 <= tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1 <= tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2 <= tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3 <= tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp <= tr.io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid <= tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr <= tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType <= tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select <= tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_timing <= tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_timing;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action <= tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain <= tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store <= tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load <= tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2 <= tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0 <= tr.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1 <= tr.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2 <= tr.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3 <= tr.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp <= tr.io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_fsIsOff <= tr.io_ooo_to_mem_csrCtrl_fsIsOff;

endtask:send_pkt

task csr_ctrl_agent_agent_driver::drive_idle(tcnt_dec_base::drv_mode_e drv_mode);

    if(drv_mode==tcnt_dec_base::DRV_0) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_mode <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_asid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_ppn <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_changed <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_mode <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_asid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_ppn <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_changed <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_mode <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_vmid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_ppn <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_changed <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BME <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_CMODE <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BCLEAR <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BMA <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_mxr <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_sum <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_vmxr <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_vsum <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_virt <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_virt_changed <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_spvp <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_imode <= memblock_sync_pkg::dispatch_real_smoke_active ? 2'd3 : '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_dmode <= memblock_sync_pkg::dispatch_real_smoke_active ? 2'd3 : '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mPBMTE <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hPBMTE <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_mseccfg <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_menvcfg <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_henvcfg <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_hstatus <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_senvcfg <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_ubtbEnable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_abtbEnable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_mbtbEnable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_tageEnable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_scEnable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_ittageEnable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_sbuffer_timeout <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_ldld_vio_check_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_cache_error_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_power_down_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_flush_l2_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_timing <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_execute <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_store <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_load <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_timing <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_fsIsOff <= '0;

    end
    else if(drv_mode==tcnt_dec_base::DRV_1) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_mode <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_asid <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_ppn <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_changed <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_mode <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_asid <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_ppn <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_changed <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_mode <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_vmid <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_ppn <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_changed <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BME <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_CMODE <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BCLEAR <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BMA <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_mxr <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_sum <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_vmxr <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_vsum <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_virt <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_virt_changed <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_spvp <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_imode <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_dmode <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mPBMTE <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hPBMTE <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_mseccfg <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_menvcfg <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_henvcfg <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_hstatus <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_senvcfg <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_ubtbEnable <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_abtbEnable <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_mbtbEnable <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_tageEnable <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_scEnable <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_ittageEnable <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_sbuffer_timeout <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_ldld_vio_check_enable <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_cache_error_enable <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_power_down_enable <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_flush_l2_enable <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_timing <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_execute <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_store <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_load <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_timing <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_fsIsOff <= '1;

    end
    else if(drv_mode==tcnt_dec_base::DRV_X) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_mode <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_asid <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_ppn <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_changed <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_mode <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_asid <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_ppn <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_changed <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_mode <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_vmid <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_ppn <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_changed <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BME <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_CMODE <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BCLEAR <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BMA <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_mxr <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_sum <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_vmxr <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_vsum <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_virt <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_virt_changed <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_spvp <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_imode <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_dmode <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mPBMTE <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hPBMTE <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_mseccfg <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_menvcfg <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_henvcfg <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_hstatus <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_senvcfg <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_ubtbEnable <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_abtbEnable <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_mbtbEnable <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_tageEnable <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_scEnable <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_ittageEnable <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_sbuffer_timeout <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_ldld_vio_check_enable <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_cache_error_enable <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_power_down_enable <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_flush_l2_enable <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_timing <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_execute <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_store <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_load <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_timing <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_fsIsOff <= 'x;

    end
    else if(drv_mode==tcnt_dec_base::DRV_RAND) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_mode <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_asid <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_ppn <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_changed <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_mode <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_asid <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_ppn <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_changed <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_mode <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_vmid <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_ppn <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_changed <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BME <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_CMODE <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BCLEAR <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BMA <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_mxr <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_sum <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_vmxr <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_vsum <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_virt <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_virt_changed <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_spvp <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_imode <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_dmode <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mPBMTE <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hPBMTE <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_mseccfg <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_menvcfg <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_henvcfg <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_hstatus <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_senvcfg <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_ubtbEnable <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_abtbEnable <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_mbtbEnable <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_tageEnable <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_scEnable <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_ittageEnable <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_sbuffer_timeout <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_ldld_vio_check_enable <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_cache_error_enable <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_power_down_enable <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_flush_l2_enable <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_timing <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_execute <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_store <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_load <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_timing <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_fsIsOff <= $urandom;

    end
    else if(drv_mode==tcnt_dec_base::DRV_LST) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_mode <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_asid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_ppn <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_changed <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_mode <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_asid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_ppn <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_changed <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_mode <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_vmid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_ppn <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_changed <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BME <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_CMODE <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BCLEAR <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BMA <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_mxr <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_sum <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_vmxr <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_vsum <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_virt <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_virt_changed <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_spvp <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_imode <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_dmode <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mPBMTE <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hPBMTE <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_mseccfg <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_menvcfg <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_henvcfg <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_hstatus <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_senvcfg <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_ubtbEnable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_abtbEnable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_mbtbEnable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_tageEnable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_scEnable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_ittageEnable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_sbuffer_timeout <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_ldld_vio_check_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_cache_error_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_power_down_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_flush_l2_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_timing <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_execute <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_store <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_load <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_timing <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_fsIsOff <= '0;

    end

endtask:drive_idle

`endif
