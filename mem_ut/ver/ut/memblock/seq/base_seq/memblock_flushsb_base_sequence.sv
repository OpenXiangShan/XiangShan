//=========================================================
//File name    : memblock_flushsb_base_sequence.sv
//Author       : OpenAI_Codex
//Module name  : memblock_flushsb_base_sequence
//Discribution : periodic flushSb request producer
//Date         : 2026-06-24
//=========================================================
`ifndef MEMBLOCK_FLUSHSB_BASE_SEQUENCE__SV
`define MEMBLOCK_FLUSHSB_BASE_SEQUENCE__SV

class memblock_flushsb_base_sequence extends uvm_sequence #(uvm_sequence_item);

    common_data_transaction data;
    virtual lintsissue_agent_agent_interface service_vif;

    bit          enable;
    int unsigned request_cycle;

    localparam int unsigned FLUSHSB_SOURCE_PERIODIC = 1;

    `uvm_object_utils(memblock_flushsb_base_sequence)

    extern function new(string name = "memblock_flushsb_base_sequence");
    extern virtual task body();
    extern virtual task wait_clock_tick();
    extern virtual function void configure_from_plus();
    extern virtual function void ensure_handles();

endclass:memblock_flushsb_base_sequence

function memblock_flushsb_base_sequence::new(string name = "memblock_flushsb_base_sequence");
    super.new(name);
    data = null;
    service_vif = null;
    enable = 1'b0;
    request_cycle = 0;
endfunction:new

task memblock_flushsb_base_sequence::body();
    int unsigned cycle_count;

    seq_csr_common::init();
    configure_from_plus();
    if (!enable) begin
        `uvm_info(get_type_name(), "MEMBLOCK_FLUSHSB_SEQ_EN=0, periodic flushSb producer stays idle", UVM_LOW)
        return;
    end
    if (request_cycle == 0) begin
        `uvm_info(get_type_name(),
                  "MEMBLOCK_FLUSHSB_REQUEST_CYCLE=0, periodic flushSb producer stays idle; directed producers can still push requests",
                  UVM_LOW)
        return;
    end
    if (!seq_csr_common::get_lsqcommit_seq_en()) begin
        `uvm_info(get_type_name(),
                  "MEMBLOCK_LSQCOMMIT_SEQ_EN=0, periodic flushSb producer cannot be consumed and stays idle",
                  UVM_LOW)
        return;
    end

    ensure_handles();
    cycle_count = 0;
    forever begin
        if (data.is_global_stop_requested()) begin
            `uvm_info(get_type_name(),
                      $sformatf("stop periodic flushSb producer by global_stop_requested after %0d cycles",
                                cycle_count),
                      UVM_LOW)
            break;
        end
        wait_clock_tick();
        if (data.is_global_stop_requested()) begin
            `uvm_info(get_type_name(),
                      $sformatf("stop periodic flushSb producer by global_stop_requested after %0d cycles",
                                cycle_count),
                      UVM_LOW)
            break;
        end
        if (service_vif.rst_n !== 1'b1 ||
            memblock_sync_pkg::reset_backend_done !== 1'b1 ||
            !data.main_table_ready) begin
            continue;
        end
        cycle_count++;
        if ((cycle_count % request_cycle) == 0) begin
            data.push_flushsb_request(FLUSHSB_SOURCE_PERIODIC);
        end
    end
endtask:body

task memblock_flushsb_base_sequence::wait_clock_tick();
    ensure_handles();
    @(posedge service_vif.clk);
endtask:wait_clock_tick

function void memblock_flushsb_base_sequence::configure_from_plus();
    enable = seq_csr_common::get_flushsb_seq_en();
    request_cycle = seq_csr_common::get_flushsb_request_cycle();
endfunction:configure_from_plus

function void memblock_flushsb_base_sequence::ensure_handles();
    if (data == null) begin
        data = common_data_transaction::get();
    end
    if (service_vif == null) begin
        if (!uvm_config_db#(virtual lintsissue_agent_agent_interface)::get(null, get_full_name(), "vif", service_vif) &&
            !uvm_config_db#(virtual lintsissue_agent_agent_interface)::get(null, "uvm_test_top.env.u_lintsissue_agent_agent*", "vif", service_vif)) begin
            `uvm_fatal(get_type_name(), "failed to get service clock vif for periodic flushSb producer")
        end
    end
endfunction:ensure_handles

`endif
