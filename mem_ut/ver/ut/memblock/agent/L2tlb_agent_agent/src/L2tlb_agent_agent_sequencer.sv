//=========================================================
//File name    : L2tlb_agent_agent_sequencer.sv
//Author       : OpenAI_Codex
//Module name  : L2tlb_agent_agent_sequencer
//Discribution : L2tlb_agent_agent_sequencer : sequencer
//Date         : 2026-04-12
//=========================================================
`ifndef L2TLB_AGENT_AGENT_SEQUENCER__SV
`define L2TLB_AGENT_AGENT_SEQUENCER__SV

class L2tlb_agent_agent_sequencer  extends tcnt_sequencer_base #(L2tlb_agent_agent_xaction);
    `uvm_component_utils(L2tlb_agent_agent_sequencer)

    local L2tlb_agent_agent_transport_sample sample_slot;
    local longint unsigned sample_slot_seq;
    local longint unsigned sample_slot_terminal_seq;
    // Physical transport samples include analysis-only reset/passive samples.
    // Keep their sequence separate from the semantic slot state so a later
    // semantic sample is checked against the complete physical stream.
    local longint unsigned last_observed_transport_sample_seq;
    typedef enum bit [1:0] {
        MEMBLOCK_L2TLB_SLOT_EMPTY,
        MEMBLOCK_L2TLB_SLOT_PUBLISHED,
        MEMBLOCK_L2TLB_SLOT_CONSUMED,
        MEMBLOCK_L2TLB_SLOT_DROPPED
    } memblock_l2tlb_transport_slot_state_e;
    local memblock_l2tlb_transport_slot_state_e sample_slot_state;
    local uvm_event sample_published_ev;
    local uvm_event sample_recycled_ev;

    extern function new(string name, uvm_component parent);
    extern task main_phase(uvm_phase phase);
    extern virtual function bit publish_transport_sample(
        L2tlb_agent_agent_transport_sample sample);
    // Abstract responsibility: advance the physical transport sequence after
    // the driver's synchronous analysis fanout has consumed a sample.
    extern virtual function void note_transport_sample_observed(
        input longint unsigned transport_sample_seq);
    // Abstract responsibility: notify the semantic owner only after the
    // driver's synchronous analysis fanout has returned for this sample.
    extern virtual function bit notify_transport_sample_published();
    extern virtual task wait_transport_sample(
        output L2tlb_agent_agent_transport_sample sample);
    extern virtual function bit try_peek_transport_sample(
        output L2tlb_agent_agent_transport_sample sample);
    extern virtual function bit ack_transport_sample(
        input longint unsigned transport_sample_seq,
        input memblock_sync_pkg::memblock_l2tlb_transport_terminal_e terminal_kind);
    extern virtual function bit get_recyclable_transport_sample_seq(
        output longint unsigned transport_sample_seq);
    extern virtual function bit recycle_transport_sample(
        input longint unsigned transport_sample_seq);
    extern virtual function bit transport_sample_slot_empty();
endclass:L2tlb_agent_agent_sequencer

function L2tlb_agent_agent_sequencer::new(string name, uvm_component parent);
    super.new(name, parent);
    sample_slot = null;
    sample_slot_seq = 0;
    sample_slot_terminal_seq = 0;
    last_observed_transport_sample_seq = 0;
    sample_slot_state = MEMBLOCK_L2TLB_SLOT_EMPTY;
    sample_published_ev = new("sample_published_ev");
    sample_recycled_ev = new("sample_recycled_ev");
endfunction:new

task L2tlb_agent_agent_sequencer::main_phase(uvm_phase phase);
    super.main_phase(phase);
    phase.raise_objection(this);
    if(!(uvm_config_db#(uvm_object_wrapper)::exists(this, "main_phase", "default_sequence", 0))) begin
        tcnt_default_sequence_base#(seq_item_t) seq;
        `uvm_warning(get_type_name(),"had no get the default_sequence, please check!!")
        seq = tcnt_default_sequence_base#(seq_item_t)::type_id::create("seq");
        seq.starting_phase = phase;
        seq.start(this);
    end
    phase.drop_objection(this);
endtask:main_phase

function bit L2tlb_agent_agent_sequencer::publish_transport_sample(
    L2tlb_agent_agent_transport_sample sample);
    memblock_l2tlb_drv_sample_t payload;

    if (sample == null) begin
        `uvm_fatal(get_type_name(), "publish_transport_sample got null sample")
        return 1'b0;
    end
    if (!sample.get_payload(payload)) begin
        return 1'b0;
    end

    if (sample_slot_state != MEMBLOCK_L2TLB_SLOT_EMPTY) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("transport sample slot overwrite state=%0d old_seq=%0d new_seq=%0d",
                             sample_slot_state, sample_slot_seq,
                             payload.transport_sample_seq))
        return 1'b0;
    end
    if (payload.transport_sample_seq == 0 ||
        payload.transport_sample_seq != last_observed_transport_sample_seq + 1) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("transport sample sequence is not consecutive old=%0d new=%0d",
                             last_observed_transport_sample_seq,
                             payload.transport_sample_seq))
        return 1'b0;
    end
    sample_slot = sample;
    sample_slot_seq = payload.transport_sample_seq;
    sample_slot_terminal_seq = 0;
    sample_slot_state = MEMBLOCK_L2TLB_SLOT_PUBLISHED;
    return 1'b1;
endfunction:publish_transport_sample

function void L2tlb_agent_agent_sequencer::note_transport_sample_observed(
    input longint unsigned transport_sample_seq);
    if (transport_sample_seq == 0 ||
        transport_sample_seq != last_observed_transport_sample_seq + 1) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("physical L2TLB transport sample sequence is not consecutive old=%0d new=%0d",
                             last_observed_transport_sample_seq,
                             transport_sample_seq))
    end
    last_observed_transport_sample_seq = transport_sample_seq;
endfunction:note_transport_sample_observed

function bit L2tlb_agent_agent_sequencer::notify_transport_sample_published();
    if (sample_slot_state != MEMBLOCK_L2TLB_SLOT_PUBLISHED ||
        sample_slot == null || sample_slot_seq == 0) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("cannot notify unpublished transport sample state=%0d seq=%0d",
                             sample_slot_state, sample_slot_seq))
        return 1'b0;
    end
    sample_published_ev.trigger();
    return 1'b1;
endfunction:notify_transport_sample_published

task L2tlb_agent_agent_sequencer::wait_transport_sample(
    output L2tlb_agent_agent_transport_sample sample);
    while (sample_slot_state != MEMBLOCK_L2TLB_SLOT_PUBLISHED) begin
        sample_published_ev.wait_trigger();
    end
    sample = sample_slot;
endtask:wait_transport_sample

function bit L2tlb_agent_agent_sequencer::try_peek_transport_sample(
    output L2tlb_agent_agent_transport_sample sample);
    sample = sample_slot;
    return sample_slot_state == MEMBLOCK_L2TLB_SLOT_PUBLISHED;
endfunction:try_peek_transport_sample

function bit L2tlb_agent_agent_sequencer::ack_transport_sample(
    input longint unsigned transport_sample_seq,
    input memblock_sync_pkg::memblock_l2tlb_transport_terminal_e terminal_kind);
    if (sample_slot_state != MEMBLOCK_L2TLB_SLOT_PUBLISHED ||
        sample_slot_seq != transport_sample_seq) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("transport sample terminal ack mismatch state=%0d slot=%0d ack=%0d",
                             sample_slot_state, sample_slot_seq,
                             transport_sample_seq))
        return 1'b0;
    end
    sample_slot_terminal_seq = transport_sample_seq;
    case (terminal_kind)
        memblock_sync_pkg::MEMBLOCK_L2TLB_SAMPLE_CONSUMED:
            sample_slot_state = MEMBLOCK_L2TLB_SLOT_CONSUMED;
        memblock_sync_pkg::MEMBLOCK_L2TLB_SAMPLE_DROPPED:
            sample_slot_state = MEMBLOCK_L2TLB_SLOT_DROPPED;
        default: begin
            `uvm_fatal(get_type_name(), "invalid transport sample terminal kind")
            return 1'b0;
        end
    endcase
    return 1'b1;
endfunction:ack_transport_sample

function bit L2tlb_agent_agent_sequencer::get_recyclable_transport_sample_seq(
    output longint unsigned transport_sample_seq);
    transport_sample_seq = 0;
    if (!(sample_slot_state inside {MEMBLOCK_L2TLB_SLOT_CONSUMED,
                                    MEMBLOCK_L2TLB_SLOT_DROPPED})) begin
        return 1'b0;
    end
    if (sample_slot_terminal_seq != sample_slot_seq || sample_slot_seq == 0) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("invalid terminal transport sample slot=%0d terminal=%0d",
                             sample_slot_seq, sample_slot_terminal_seq))
        return 1'b0;
    end
    transport_sample_seq = sample_slot_seq;
    return 1'b1;
endfunction:get_recyclable_transport_sample_seq

function bit L2tlb_agent_agent_sequencer::recycle_transport_sample(
    input longint unsigned transport_sample_seq);
    if (!(sample_slot_state inside {MEMBLOCK_L2TLB_SLOT_CONSUMED,
                                    MEMBLOCK_L2TLB_SLOT_DROPPED}) ||
        sample_slot_seq != transport_sample_seq ||
        sample_slot_terminal_seq != transport_sample_seq) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("invalid transport sample recycle state=%0d slot=%0d terminal=%0d request=%0d",
                             sample_slot_state, sample_slot_seq,
                             sample_slot_terminal_seq, transport_sample_seq))
        return 1'b0;
    end
    sample_slot = null;
    sample_slot_seq = 0;
    sample_slot_terminal_seq = 0;
    sample_slot_state = MEMBLOCK_L2TLB_SLOT_EMPTY;
    sample_recycled_ev.trigger();
    return 1'b1;
endfunction:recycle_transport_sample

function bit L2tlb_agent_agent_sequencer::transport_sample_slot_empty();
    return sample_slot_state == MEMBLOCK_L2TLB_SLOT_EMPTY;
endfunction:transport_sample_slot_empty

`endif
