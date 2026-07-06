//=========================================================
//File name    : user_cfg.sv
//Author       : OpenAI_Codex
//Module name  : memblock_user_cfg
//Discribution : public user control fields for personal mem_ut overrides
//Date         : 2026-05-14
//=========================================================
`ifndef MEMBLOCK_USER_CFG__SV
`define MEMBLOCK_USER_CFG__SV

class memblock_user_agent_ctrl extends uvm_object;

    bit sqr_sw_valid;
    tcnt_dec_base::switch_mode_e sqr_sw;
    bit drv_sw_valid;
    tcnt_dec_base::switch_mode_e drv_sw;
    bit mon_sw_valid;
    tcnt_dec_base::switch_mode_e mon_sw;
    bit xz_sw_valid;
    tcnt_dec_base::switch_mode_e xz_sw;
    bit drv_mode_valid;
    tcnt_dec_base::drv_mode_e drv_mode;
    bit channel_id_valid;
    int channel_id;

    `uvm_object_utils_begin(memblock_user_agent_ctrl)
        `uvm_field_int(sqr_sw_valid, UVM_ALL_ON)
        `uvm_field_enum(tcnt_dec_base::switch_mode_e, sqr_sw, UVM_ALL_ON)
        `uvm_field_int(drv_sw_valid, UVM_ALL_ON)
        `uvm_field_enum(tcnt_dec_base::switch_mode_e, drv_sw, UVM_ALL_ON)
        `uvm_field_int(mon_sw_valid, UVM_ALL_ON)
        `uvm_field_enum(tcnt_dec_base::switch_mode_e, mon_sw, UVM_ALL_ON)
        `uvm_field_int(xz_sw_valid, UVM_ALL_ON)
        `uvm_field_enum(tcnt_dec_base::switch_mode_e, xz_sw, UVM_ALL_ON)
        `uvm_field_int(drv_mode_valid, UVM_ALL_ON)
        `uvm_field_enum(tcnt_dec_base::drv_mode_e, drv_mode, UVM_ALL_ON)
        `uvm_field_int(channel_id_valid, UVM_ALL_ON)
        `uvm_field_int(channel_id, UVM_ALL_ON)
    `uvm_object_utils_end

    extern function new(string name = "memblock_user_agent_ctrl");
    extern function void set_default();
    extern function void apply_to(tcnt_agent_cfg_base cfg);

endclass:memblock_user_agent_ctrl

function memblock_user_agent_ctrl::new(string name = "memblock_user_agent_ctrl");
    super.new(name);
    this.set_default();
endfunction:new

function void memblock_user_agent_ctrl::set_default();
    this.sqr_sw_valid = 1'b0;
    this.sqr_sw = tcnt_dec_base::ON;
    this.drv_sw_valid = 1'b0;
    this.drv_sw = tcnt_dec_base::ON;
    this.mon_sw_valid = 1'b0;
    this.mon_sw = tcnt_dec_base::ON;
    this.xz_sw_valid = 1'b0;
    this.xz_sw = tcnt_dec_base::ON;
    this.drv_mode_valid = 1'b0;
    this.drv_mode = tcnt_dec_base::DRV_0;
    this.channel_id_valid = 1'b0;
    this.channel_id = 0;
endfunction:set_default

function void memblock_user_agent_ctrl::apply_to(tcnt_agent_cfg_base cfg);
    if(cfg == null) begin
        return;
    end
    if(this.sqr_sw_valid) begin
        cfg.sqr_sw = this.sqr_sw;
    end
    if(this.drv_sw_valid) begin
        cfg.drv_sw = this.drv_sw;
    end
    if(this.mon_sw_valid) begin
        cfg.mon_sw = this.mon_sw;
    end
    if(this.xz_sw_valid) begin
        cfg.xz_sw = this.xz_sw;
    end
    if(this.drv_mode_valid) begin
        cfg.drv_mode = this.drv_mode;
    end
    if(this.channel_id_valid) begin
        cfg.channel_id = this.channel_id;
    end
endfunction:apply_to

class memblock_user_cfg extends uvm_object;

    bit enable;
    memblock_user_agent_ctrl all_agent_ctrl;
    memblock_user_agent_ctrl u_backendToTopBypass_agent_agent_ctrl;
    memblock_user_agent_ctrl u_fence_agent_agent_ctrl;
    memblock_user_agent_ctrl u_csr_ctrl_agent_agent_ctrl;
    memblock_user_agent_ctrl u_lsqcommit_agent_agent_ctrl;
    memblock_user_agent_ctrl u_lsqenq_agent_agent_ctrl;
    memblock_user_agent_ctrl u_lintsissue_agent_agent_ctrl;
    memblock_user_agent_ctrl u_vecissue_agent_agent_ctrl;
    memblock_user_agent_ctrl u_redirect_agent_agent_ctrl;
    memblock_user_agent_ctrl u_sbuffer_agent_agent_ctrl;
    memblock_user_agent_ctrl u_dcache_agent_agent_ctrl;
    memblock_user_agent_ctrl u_int_sink_agent_agent_ctrl;
    memblock_user_agent_ctrl u_L2tlb_agent_agent_ctrl;
    memblock_user_agent_ctrl u_itlb_agent_agent_ctrl;
    memblock_user_agent_ctrl u_prefetch_agent_agent_ctrl;
    memblock_user_agent_ctrl u_io_mem_to_ooo_ctrl_agent_agent_ctrl;
    memblock_user_agent_ctrl u_io_mem_to_ooo_int_wb_agent_agent_ctrl;
    memblock_user_agent_ctrl u_io_mem_to_ooo_vec_wb_agent_agent_ctrl;
    memblock_user_agent_ctrl u_io_mem_to_ooo_wakeup_agent_agent_ctrl;
    memblock_user_agent_ctrl u_io_mem_to_ooo_iq_feedback_agent_agent_ctrl;
    memblock_user_agent_ctrl u_other_ctrl_agent_agent_ctrl;

    `uvm_object_utils_begin(memblock_user_cfg)
        `uvm_field_int(enable, UVM_ALL_ON)
        `uvm_field_object(all_agent_ctrl, UVM_ALL_ON)
        `uvm_field_object(u_backendToTopBypass_agent_agent_ctrl, UVM_ALL_ON)
        `uvm_field_object(u_fence_agent_agent_ctrl, UVM_ALL_ON)
        `uvm_field_object(u_csr_ctrl_agent_agent_ctrl, UVM_ALL_ON)
        `uvm_field_object(u_lsqcommit_agent_agent_ctrl, UVM_ALL_ON)
        `uvm_field_object(u_lsqenq_agent_agent_ctrl, UVM_ALL_ON)
        `uvm_field_object(u_lintsissue_agent_agent_ctrl, UVM_ALL_ON)
        `uvm_field_object(u_vecissue_agent_agent_ctrl, UVM_ALL_ON)
        `uvm_field_object(u_redirect_agent_agent_ctrl, UVM_ALL_ON)
        `uvm_field_object(u_sbuffer_agent_agent_ctrl, UVM_ALL_ON)
        `uvm_field_object(u_dcache_agent_agent_ctrl, UVM_ALL_ON)
        `uvm_field_object(u_int_sink_agent_agent_ctrl, UVM_ALL_ON)
        `uvm_field_object(u_L2tlb_agent_agent_ctrl, UVM_ALL_ON)
        `uvm_field_object(u_itlb_agent_agent_ctrl, UVM_ALL_ON)
        `uvm_field_object(u_prefetch_agent_agent_ctrl, UVM_ALL_ON)
        `uvm_field_object(u_io_mem_to_ooo_ctrl_agent_agent_ctrl, UVM_ALL_ON)
        `uvm_field_object(u_io_mem_to_ooo_int_wb_agent_agent_ctrl, UVM_ALL_ON)
        `uvm_field_object(u_io_mem_to_ooo_vec_wb_agent_agent_ctrl, UVM_ALL_ON)
        `uvm_field_object(u_io_mem_to_ooo_wakeup_agent_agent_ctrl, UVM_ALL_ON)
        `uvm_field_object(u_io_mem_to_ooo_iq_feedback_agent_agent_ctrl, UVM_ALL_ON)
        `uvm_field_object(u_other_ctrl_agent_agent_ctrl, UVM_ALL_ON)
    `uvm_object_utils_end

    extern function new(string name = "memblock_user_cfg");
    extern function void load_local();

endclass:memblock_user_cfg

function memblock_user_cfg::new(string name = "memblock_user_cfg");
    super.new(name);
    this.enable = 1'b1;
    this.all_agent_ctrl = memblock_user_agent_ctrl::type_id::create("all_agent_ctrl");
    this.u_backendToTopBypass_agent_agent_ctrl = memblock_user_agent_ctrl::type_id::create("u_backendToTopBypass_agent_agent_ctrl");
    this.u_fence_agent_agent_ctrl = memblock_user_agent_ctrl::type_id::create("u_fence_agent_agent_ctrl");
    this.u_csr_ctrl_agent_agent_ctrl = memblock_user_agent_ctrl::type_id::create("u_csr_ctrl_agent_agent_ctrl");
    this.u_lsqcommit_agent_agent_ctrl = memblock_user_agent_ctrl::type_id::create("u_lsqcommit_agent_agent_ctrl");
    this.u_lsqenq_agent_agent_ctrl = memblock_user_agent_ctrl::type_id::create("u_lsqenq_agent_agent_ctrl");
    this.u_lintsissue_agent_agent_ctrl = memblock_user_agent_ctrl::type_id::create("u_lintsissue_agent_agent_ctrl");
    this.u_vecissue_agent_agent_ctrl = memblock_user_agent_ctrl::type_id::create("u_vecissue_agent_agent_ctrl");
    this.u_redirect_agent_agent_ctrl = memblock_user_agent_ctrl::type_id::create("u_redirect_agent_agent_ctrl");
    this.u_sbuffer_agent_agent_ctrl = memblock_user_agent_ctrl::type_id::create("u_sbuffer_agent_agent_ctrl");
    this.u_dcache_agent_agent_ctrl = memblock_user_agent_ctrl::type_id::create("u_dcache_agent_agent_ctrl");
    this.u_int_sink_agent_agent_ctrl = memblock_user_agent_ctrl::type_id::create("u_int_sink_agent_agent_ctrl");
    this.u_L2tlb_agent_agent_ctrl = memblock_user_agent_ctrl::type_id::create("u_L2tlb_agent_agent_ctrl");
    this.u_itlb_agent_agent_ctrl = memblock_user_agent_ctrl::type_id::create("u_itlb_agent_agent_ctrl");
    this.u_prefetch_agent_agent_ctrl = memblock_user_agent_ctrl::type_id::create("u_prefetch_agent_agent_ctrl");
    this.u_io_mem_to_ooo_ctrl_agent_agent_ctrl = memblock_user_agent_ctrl::type_id::create("u_io_mem_to_ooo_ctrl_agent_agent_ctrl");
    this.u_io_mem_to_ooo_int_wb_agent_agent_ctrl = memblock_user_agent_ctrl::type_id::create("u_io_mem_to_ooo_int_wb_agent_agent_ctrl");
    this.u_io_mem_to_ooo_vec_wb_agent_agent_ctrl = memblock_user_agent_ctrl::type_id::create("u_io_mem_to_ooo_vec_wb_agent_agent_ctrl");
    this.u_io_mem_to_ooo_wakeup_agent_agent_ctrl = memblock_user_agent_ctrl::type_id::create("u_io_mem_to_ooo_wakeup_agent_agent_ctrl");
    this.u_io_mem_to_ooo_iq_feedback_agent_agent_ctrl = memblock_user_agent_ctrl::type_id::create("u_io_mem_to_ooo_iq_feedback_agent_agent_ctrl");
    this.u_other_ctrl_agent_agent_ctrl = memblock_user_agent_ctrl::type_id::create("u_other_ctrl_agent_agent_ctrl");
endfunction:new

function void memblock_user_cfg::load_local();
    // Local user overrides are part of the default user_ctrl flow.
    // The sim Makefile creates this file from user_cfg.local.default.sv when it is missing.
    `include "user_cfg.local.sv"
endfunction:load_local

`endif
