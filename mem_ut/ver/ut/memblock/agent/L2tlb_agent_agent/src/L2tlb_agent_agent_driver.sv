//=========================================================
//File name    : L2tlb_agent_agent_driver.sv
//Author       : OpenAI_Codex
//Module name  : L2tlb_agent_agent_driver
//Discribution : L2tlb_agent_agent_driver : driver
//Date         : 2026-04-12
//=========================================================
`ifndef L2TLB_AGENT_AGENT_DRIVER__SV
`define L2TLB_AGENT_AGENT_DRIVER__SV

class L2tlb_agent_agent_driver  extends tcnt_driver_base#(virtual L2tlb_agent_agent_interface,L2tlb_agent_agent_cfg,L2tlb_agent_agent_xaction);

    `uvm_component_utils(L2tlb_agent_agent_driver)

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern virtual task reset_phase(uvm_phase phase);
    extern task main_phase(uvm_phase phase);
    extern task send_pkt(L2tlb_agent_agent_xaction tr);
    extern task drive_idle(tcnt_dec_base::drv_mode_e drv_mode);
endclass:L2tlb_agent_agent_driver

function L2tlb_agent_agent_driver::new(string name, uvm_component parent);
    super.new(name,parent);
endfunction:new

function void L2tlb_agent_agent_driver::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

task L2tlb_agent_agent_driver::reset_phase(uvm_phase phase);

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

task L2tlb_agent_agent_driver::main_phase(uvm_phase phase);
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
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_x <= tr.io_ptw_resp_bits_s2_entry_perm_x;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_w <= tr.io_ptw_resp_bits_s2_entry_perm_w;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_r <= tr.io_ptw_resp_bits_s2_entry_perm_r;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_level <= tr.io_ptw_resp_bits_s2_entry_level;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_gpf <= tr.io_ptw_resp_bits_s2_gpf;
    vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_gaf <= tr.io_ptw_resp_bits_s2_gaf;

endtask:send_pkt

task L2tlb_agent_agent_driver::drive_idle(tcnt_dec_base::drv_mode_e drv_mode);

    if(drv_mode==tcnt_dec_base::DRV_0) begin
        vif.drv_mp.drv_cb.io_ptw_req_0_ready <= memblock_sync_pkg::l2tlb_responder_active ? '0 : '1;
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
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_x <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_w <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_perm_r <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_entry_level <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_gpf <= '0;
        vif.drv_mp.drv_cb.io_ptw_resp_bits_s2_gaf <= '0;

    end

endtask:drive_idle

`endif
