//=========================================================
//File name    : io_mem_to_ooo_vec_wb_agent_agent_driver.sv
//Author       : OpenAI_Codex
//Module name  : io_mem_to_ooo_vec_wb_agent_agent_driver
//Discribution : io_mem_to_ooo_vec_wb_agent_agent_driver : driver
//Date         : 2026-04-12
//=========================================================
`ifndef IO_MEM_TO_OOO_VEC_WB_AGENT_AGENT_DRIVER__SV
`define IO_MEM_TO_OOO_VEC_WB_AGENT_AGENT_DRIVER__SV

class io_mem_to_ooo_vec_wb_agent_agent_driver  extends tcnt_driver_base#(virtual io_mem_to_ooo_vec_wb_agent_agent_interface,io_mem_to_ooo_vec_wb_agent_agent_cfg,io_mem_to_ooo_vec_wb_agent_agent_xaction);

    `uvm_component_utils(io_mem_to_ooo_vec_wb_agent_agent_driver)

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern virtual task reset_phase(uvm_phase phase);
    extern task main_phase(uvm_phase phase);
    extern task send_pkt(io_mem_to_ooo_vec_wb_agent_agent_xaction tr);
    extern task drive_idle(tcnt_dec_base::drv_mode_e drv_mode);
endclass:io_mem_to_ooo_vec_wb_agent_agent_driver

function io_mem_to_ooo_vec_wb_agent_agent_driver::new(string name, uvm_component parent);
    super.new(name,parent);
endfunction:new

function void io_mem_to_ooo_vec_wb_agent_agent_driver::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

task io_mem_to_ooo_vec_wb_agent_agent_driver::reset_phase(uvm_phase phase);

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

task io_mem_to_ooo_vec_wb_agent_agent_driver::main_phase(uvm_phase phase);
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

task io_mem_to_ooo_vec_wb_agent_agent_driver::send_pkt(io_mem_to_ooo_vec_wb_agent_agent_xaction tr);
    vif.drv_mp.drv_cb.io_mem_to_ooo_vecWriteback_1_0_ready <= tr.io_mem_to_ooo_vecWriteback_1_0_ready;
    vif.drv_mp.drv_cb.io_mem_to_ooo_vecWriteback_0_0_ready <= tr.io_mem_to_ooo_vecWriteback_0_0_ready;

endtask:send_pkt

task io_mem_to_ooo_vec_wb_agent_agent_driver::drive_idle(tcnt_dec_base::drv_mode_e drv_mode);

    if(drv_mode==tcnt_dec_base::DRV_0) begin
        vif.drv_mp.drv_cb.io_mem_to_ooo_vecWriteback_1_0_ready <= '0;
        vif.drv_mp.drv_cb.io_mem_to_ooo_vecWriteback_0_0_ready <= '0;

    end
    else if(drv_mode==tcnt_dec_base::DRV_1) begin
        vif.drv_mp.drv_cb.io_mem_to_ooo_vecWriteback_1_0_ready <= '1;
        vif.drv_mp.drv_cb.io_mem_to_ooo_vecWriteback_0_0_ready <= '1;

    end
    else if(drv_mode==tcnt_dec_base::DRV_X) begin
        vif.drv_mp.drv_cb.io_mem_to_ooo_vecWriteback_1_0_ready <= 'x;
        vif.drv_mp.drv_cb.io_mem_to_ooo_vecWriteback_0_0_ready <= 'x;

    end
    else if(drv_mode==tcnt_dec_base::DRV_RAND) begin
        vif.drv_mp.drv_cb.io_mem_to_ooo_vecWriteback_1_0_ready <= $urandom;
        vif.drv_mp.drv_cb.io_mem_to_ooo_vecWriteback_0_0_ready <= $urandom;

    end
    else if(drv_mode==tcnt_dec_base::DRV_LST) begin
        vif.drv_mp.drv_cb.io_mem_to_ooo_vecWriteback_1_0_ready <= '0;
        vif.drv_mp.drv_cb.io_mem_to_ooo_vecWriteback_0_0_ready <= '0;

    end

endtask:drive_idle

`endif
