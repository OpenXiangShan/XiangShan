//=========================================================
//File name    : dcache_agent_agent_driver.sv
//Author       : OpenAI_Codex
//Module name  : dcache_agent_agent_driver
//Discribution : dcache_agent_agent_driver : driver
//Date         : 2026-04-12
//=========================================================
`ifndef DCACHE_AGENT_AGENT_DRIVER__SV
`define DCACHE_AGENT_AGENT_DRIVER__SV

class dcache_agent_agent_driver  extends tcnt_driver_base#(virtual dcache_agent_agent_interface,dcache_agent_agent_cfg,dcache_agent_agent_xaction);

    `uvm_component_utils(dcache_agent_agent_driver)

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern virtual task reset_phase(uvm_phase phase);
    extern task main_phase(uvm_phase phase);
    extern task send_pkt(dcache_agent_agent_xaction tr);
    extern task drive_idle(tcnt_dec_base::drv_mode_e drv_mode);
endclass:dcache_agent_agent_driver

function dcache_agent_agent_driver::new(string name, uvm_component parent);
    super.new(name,parent);
endfunction:new

function void dcache_agent_agent_driver::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

task dcache_agent_agent_driver::reset_phase(uvm_phase phase);

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

task dcache_agent_agent_driver::main_phase(uvm_phase phase);
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

task dcache_agent_agent_driver::send_pkt(dcache_agent_agent_xaction tr);
    vif.drv_mp.drv_cb.auto_inner_dcache_client_out_a_ready <= tr.auto_inner_dcache_client_out_a_ready;
    vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_valid <= tr.auto_inner_dcache_client_out_b_valid;
    vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_opcode <= tr.auto_inner_dcache_client_out_b_bits_opcode;
    vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_param <= tr.auto_inner_dcache_client_out_b_bits_param;
    vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_size <= tr.auto_inner_dcache_client_out_b_bits_size;
    vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_source <= tr.auto_inner_dcache_client_out_b_bits_source;
    vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_address <= tr.auto_inner_dcache_client_out_b_bits_address;
    vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_mask <= tr.auto_inner_dcache_client_out_b_bits_mask;
    vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_data <= tr.auto_inner_dcache_client_out_b_bits_data;
    vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_corrupt <= tr.auto_inner_dcache_client_out_b_bits_corrupt;
    vif.drv_mp.drv_cb.auto_inner_dcache_client_out_c_ready <= tr.auto_inner_dcache_client_out_c_ready;
    vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_valid <= tr.auto_inner_dcache_client_out_d_valid;
    vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_opcode <= tr.auto_inner_dcache_client_out_d_bits_opcode;
    vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_param <= tr.auto_inner_dcache_client_out_d_bits_param;
    vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_size <= tr.auto_inner_dcache_client_out_d_bits_size;
    vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_source <= tr.auto_inner_dcache_client_out_d_bits_source;
    vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_sink <= tr.auto_inner_dcache_client_out_d_bits_sink;
    vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_denied <= tr.auto_inner_dcache_client_out_d_bits_denied;
    vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_echo_isKeyword <= tr.auto_inner_dcache_client_out_d_bits_echo_isKeyword;
    vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_data <= tr.auto_inner_dcache_client_out_d_bits_data;
    vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_corrupt <= tr.auto_inner_dcache_client_out_d_bits_corrupt;
    vif.drv_mp.drv_cb.auto_inner_dcache_client_out_e_ready <= tr.auto_inner_dcache_client_out_e_ready;

endtask:send_pkt

task dcache_agent_agent_driver::drive_idle(tcnt_dec_base::drv_mode_e drv_mode);

    if(drv_mode==tcnt_dec_base::DRV_0) begin
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_a_ready <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_valid <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_opcode <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_param <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_size <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_source <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_address <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_mask <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_data <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_corrupt <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_c_ready <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_valid <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_opcode <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_param <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_size <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_source <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_sink <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_denied <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_echo_isKeyword <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_data <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_corrupt <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_e_ready <= '0;

    end
    else if(drv_mode==tcnt_dec_base::DRV_1) begin
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_a_ready <= '1;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_valid <= '1;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_opcode <= '1;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_param <= '1;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_size <= '1;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_source <= '1;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_address <= '1;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_mask <= '1;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_data <= '1;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_corrupt <= '1;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_c_ready <= '1;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_valid <= '1;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_opcode <= '1;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_param <= '1;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_size <= '1;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_source <= '1;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_sink <= '1;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_denied <= '1;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_echo_isKeyword <= '1;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_data <= '1;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_corrupt <= '1;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_e_ready <= '1;

    end
    else if(drv_mode==tcnt_dec_base::DRV_X) begin
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_a_ready <= 'x;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_valid <= 'x;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_opcode <= 'x;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_param <= 'x;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_size <= 'x;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_source <= 'x;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_address <= 'x;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_mask <= 'x;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_data <= 'x;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_corrupt <= 'x;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_c_ready <= 'x;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_valid <= 'x;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_opcode <= 'x;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_param <= 'x;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_size <= 'x;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_source <= 'x;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_sink <= 'x;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_denied <= 'x;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_echo_isKeyword <= 'x;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_data <= 'x;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_corrupt <= 'x;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_e_ready <= 'x;

    end
    else if(drv_mode==tcnt_dec_base::DRV_RAND) begin
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_a_ready <= $urandom;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_valid <= $urandom;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_opcode <= $urandom;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_param <= $urandom;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_size <= $urandom;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_source <= $urandom;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_address <= $urandom;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_mask <= $urandom;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_data <= $urandom;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_corrupt <= $urandom;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_c_ready <= $urandom;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_valid <= $urandom;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_opcode <= $urandom;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_param <= $urandom;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_size <= $urandom;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_source <= $urandom;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_sink <= $urandom;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_denied <= $urandom;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_echo_isKeyword <= $urandom;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_data <= $urandom;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_corrupt <= $urandom;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_e_ready <= $urandom;

    end
    else if(drv_mode==tcnt_dec_base::DRV_LST) begin
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_a_ready <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_valid <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_opcode <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_param <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_size <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_source <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_address <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_mask <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_data <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_b_bits_corrupt <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_c_ready <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_valid <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_opcode <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_param <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_size <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_source <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_sink <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_denied <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_echo_isKeyword <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_data <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_d_bits_corrupt <= '0;
        vif.drv_mp.drv_cb.auto_inner_dcache_client_out_e_ready <= '0;

    end

endtask:drive_idle

`endif
