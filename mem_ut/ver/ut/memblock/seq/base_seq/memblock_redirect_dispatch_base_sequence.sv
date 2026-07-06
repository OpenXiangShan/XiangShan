//=========================================================
//File name    : memblock_redirect_dispatch_base_sequence.sv
//Author       : OpenAI_Codex
//Module name  : memblock_redirect_dispatch_base_sequence
//Discribution : redirect dispatch drive sequence
//Date         : 2026-05-22
//=========================================================
`ifndef MEMBLOCK_REDIRECT_DISPATCH_BASE_SEQUENCE__SV
`define MEMBLOCK_REDIRECT_DISPATCH_BASE_SEQUENCE__SV

class memblock_redirect_dispatch_base_sequence extends redirect_agent_agent_default_sequence;

    common_data_transaction data;

    bit          enable;
    int unsigned drive_timeout;

    `uvm_object_utils(memblock_redirect_dispatch_base_sequence)

    extern function new(string name = "memblock_redirect_dispatch_base_sequence");
    extern virtual task pre_body();
    extern virtual task body();
    extern virtual task post_body();
    extern function void configure_from_plus();
    extern function void ensure_helpers();
    extern task drive_idle_once(input string tr_name = "redirect_idle_tr");
    extern task drive_redirect_payload(input memblock_redirect_payload_t payload);
    extern function void clear_redirect_xaction(input redirect_agent_agent_xaction tr);
    extern function void assign_redirect_xaction(input redirect_agent_agent_xaction tr,
                                                 input memblock_redirect_payload_t payload);

endclass:memblock_redirect_dispatch_base_sequence

function memblock_redirect_dispatch_base_sequence::new(string name = "memblock_redirect_dispatch_base_sequence");
    super.new(name);
    enable = 1'b1;
    drive_timeout = 1000;
endfunction:new

task memblock_redirect_dispatch_base_sequence::pre_body();
    // This responder is driven by testcase/main-sequence objections. Holding a
    // separate objection here would keep main_phase alive forever.
endtask:pre_body

task memblock_redirect_dispatch_base_sequence::body();
    int unsigned idle_count;

    seq_csr_common::init();
    configure_from_plus();
    ensure_helpers();
    if (!enable) begin
        drive_idle_once("redirect_seq_disabled_idle_tr");
        return;
    end

    idle_count = 0;
    forever begin
        memblock_redirect_payload_t payload;

        if (data.try_pop_redirect_drive(payload)) begin
            drive_redirect_payload(payload);
            idle_count = 0;
        end else begin
            drive_idle_once($sformatf("redirect_idle_tr_%0d", idle_count));
            idle_count++;
            if (drive_timeout != 0 && idle_count >= drive_timeout &&
                data.active_redirect.valid && !data.redirect_drive_done_for(data.active_redirect)) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("timeout waiting redirect payload rob=%0d/%0d",
                                     data.active_redirect.rob_key.flag,
                                     data.active_redirect.rob_key.value))
            end
        end
    end
endtask:body

task memblock_redirect_dispatch_base_sequence::post_body();
    // Keep objection ownership in the testcase that starts the dispatch flow.
endtask:post_body

function void memblock_redirect_dispatch_base_sequence::configure_from_plus();
    enable = seq_csr_common::get_redirect_seq_en();
    drive_timeout = seq_csr_common::get_redirect_drive_timeout();
endfunction:configure_from_plus

function void memblock_redirect_dispatch_base_sequence::ensure_helpers();
    data = common_data_transaction::get();
    if (data == null) begin
        `uvm_fatal(get_type_name(), "failed to get common_data_transaction")
    end
endfunction:ensure_helpers

task memblock_redirect_dispatch_base_sequence::drive_idle_once(input string tr_name = "redirect_idle_tr");
    redirect_agent_agent_xaction tr;

    tr = redirect_agent_agent_xaction::type_id::create(tr_name);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "failed to create redirect idle xaction")
    end
    clear_redirect_xaction(tr);
    start_item(tr);
    finish_item(tr);
endtask:drive_idle_once

task memblock_redirect_dispatch_base_sequence::drive_redirect_payload(input memblock_redirect_payload_t payload);
    redirect_agent_agent_xaction tr;

    if (!payload.valid) begin
        `uvm_fatal(get_type_name(), "drive_redirect_payload requires valid payload")
    end
    tr = redirect_agent_agent_xaction::type_id::create("redirect_dispatch_tr");
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "failed to create redirect xaction")
    end
    assign_redirect_xaction(tr, payload);
    start_item(tr);
    finish_item(tr);
    data.mark_redirect_drive_done(payload);
endtask:drive_redirect_payload

function void memblock_redirect_dispatch_base_sequence::clear_redirect_xaction(input redirect_agent_agent_xaction tr);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "clear_redirect_xaction got null xaction")
    end
    tr.io_redirect_valid = 1'b0;
    tr.io_redirect_bits_level = 1'b0;
    tr.io_redirect_bits_robIdx_flag = 1'b0;
    tr.io_redirect_bits_robIdx_value = '0;
endfunction:clear_redirect_xaction

function void memblock_redirect_dispatch_base_sequence::assign_redirect_xaction(input redirect_agent_agent_xaction tr,
                                                                           input memblock_redirect_payload_t payload);
    clear_redirect_xaction(tr);
    tr.io_redirect_valid = 1'b1;
    tr.io_redirect_bits_level = payload.level;
    tr.io_redirect_bits_robIdx_flag = payload.rob_key.flag;
    tr.io_redirect_bits_robIdx_value = payload.rob_key.value;
endfunction:assign_redirect_xaction

`endif
