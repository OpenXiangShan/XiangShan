//=========================================================
//File name    : backendToTopBypass_agent_agent_driver.sv
//Author       : OpenAI_Codex
//Module name  : backendToTopBypass_agent_agent_driver
//Discribution : backendToTopBypass_agent_agent_driver : driver
//Date         : 2026-04-12
//=========================================================
`ifndef BACKENDTOTOPBYPASS_AGENT_AGENT_DRIVER__SV
`define BACKENDTOTOPBYPASS_AGENT_AGENT_DRIVER__SV

class backendToTopBypass_agent_agent_driver  extends tcnt_driver_base#(virtual backendToTopBypass_agent_agent_interface,backendToTopBypass_agent_agent_cfg,backendToTopBypass_agent_agent_xaction);

    `uvm_component_utils(backendToTopBypass_agent_agent_driver)

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern virtual task reset_phase(uvm_phase phase);
    extern task main_phase(uvm_phase phase);
    extern task send_pkt(backendToTopBypass_agent_agent_xaction tr);
    extern task drive_idle(tcnt_dec_base::drv_mode_e drv_mode);
endclass:backendToTopBypass_agent_agent_driver

function backendToTopBypass_agent_agent_driver::new(string name, uvm_component parent);
    super.new(name,parent);
endfunction:new

function void backendToTopBypass_agent_agent_driver::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

task backendToTopBypass_agent_agent_driver::reset_phase(uvm_phase phase);

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

task backendToTopBypass_agent_agent_driver::main_phase(uvm_phase phase);
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

task backendToTopBypass_agent_agent_driver::send_pkt(backendToTopBypass_agent_agent_xaction tr);
    vif.drv_mp.drv_cb.io_ooo_to_mem_backendToTopBypass_cpuWfi <= tr.io_ooo_to_mem_backendToTopBypass_cpuWfi;
    vif.drv_mp.drv_cb.io_ooo_to_mem_backendToTopBypass_cpuCriticalError <= tr.io_ooo_to_mem_backendToTopBypass_cpuCriticalError;
    vif.drv_mp.drv_cb.io_ooo_to_mem_backendToTopBypass_msiAck <= tr.io_ooo_to_mem_backendToTopBypass_msiAck;

endtask:send_pkt

task backendToTopBypass_agent_agent_driver::drive_idle(tcnt_dec_base::drv_mode_e drv_mode);

    if(drv_mode==tcnt_dec_base::DRV_0) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_backendToTopBypass_cpuWfi <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_backendToTopBypass_cpuCriticalError <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_backendToTopBypass_msiAck <= '0;

    end
    else if(drv_mode==tcnt_dec_base::DRV_1) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_backendToTopBypass_cpuWfi <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_backendToTopBypass_cpuCriticalError <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_backendToTopBypass_msiAck <= '1;

    end
    else if(drv_mode==tcnt_dec_base::DRV_X) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_backendToTopBypass_cpuWfi <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_backendToTopBypass_cpuCriticalError <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_backendToTopBypass_msiAck <= 'x;

    end
    else if(drv_mode==tcnt_dec_base::DRV_RAND) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_backendToTopBypass_cpuWfi <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_backendToTopBypass_cpuCriticalError <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_backendToTopBypass_msiAck <= $urandom;

    end
    else if(drv_mode==tcnt_dec_base::DRV_LST) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_backendToTopBypass_cpuWfi <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_backendToTopBypass_cpuCriticalError <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_backendToTopBypass_msiAck <= '0;

    end

endtask:drive_idle

`endif
