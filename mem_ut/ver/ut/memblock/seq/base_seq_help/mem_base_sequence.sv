//=========================================================
//File name    : mem_base_sequence.sv
//Author       : OpenAI_Codex
//Module name  : mem_access_base_sequence
//Discribution : Shared sparse memory access sequences
//Date         : 2026-05-16
//=========================================================
`ifndef MEM_BASE_SEQUENCE__SV
`define MEM_BASE_SEQUENCE__SV

class mem_access_base_sequence extends uvm_sequence;

    typedef bit [47:0]   mem_addr_t;
    typedef bit [37:0]   mem_line_addr_t;
    typedef bit [8191:0] mem_line_data_t;
    typedef bit [1023:0] mem_line_mask_t;

    typedef struct {
        mem_addr_t base;
        mem_addr_t limit;
    } mem_range_t;

    mem_line_data_t main_mem[mem_line_addr_t];
    mem_line_data_t prog_mem[mem_line_addr_t];
    mem_line_mask_t prog_mem_byte_valid[mem_line_addr_t];
    mem_range_t     main_mem_ranges[$];
    bit             main_mem_range_configured;

    `uvm_object_utils(mem_access_base_sequence)

    extern function new(string name = "mem_access_base_sequence");
    extern virtual function void init_main_mem_range(input mem_addr_t base, input longint unsigned capacity);
    extern virtual function void clear_main_mem_ranges();
    extern virtual function bit is_main_mem_access_in_range(input mem_addr_t addr, input mem_line_mask_t byte_mask);
    extern virtual function void paddr_to_error(input mem_addr_t addr, output bit corrupt, output bit denied);
    extern virtual function mem_line_data_t build_lazy_line(input mem_line_addr_t line_addr);
    extern virtual function void ensure_main_line(input mem_line_addr_t line_addr);
    extern virtual function void ensure_prog_line(input mem_line_addr_t line_addr);
    extern virtual task main_mem_access_task(
        input  mem_addr_t       addr,
        input  bit              is_store,
        input  mem_line_mask_t  byte_mask,
        input  mem_line_data_t  store_data,
        output bit              corrupt,
        output bit              denied,
        output mem_line_data_t  load_data
    );
    extern virtual task prog_mem_access_task(
        input  mem_addr_t       addr,
        input  bit              is_store,
        input  mem_line_mask_t  byte_mask,
        input  mem_line_data_t  store_data,
        output bit              corrupt,
        output bit              denied,
        output mem_line_data_t  load_data
    );

endclass:mem_access_base_sequence

function mem_access_base_sequence::new(string name = "mem_access_base_sequence");
    super.new(name);
    main_mem_range_configured = 1'b0;
endfunction:new

function void mem_access_base_sequence::init_main_mem_range(input mem_addr_t base, input longint unsigned capacity);
    mem_range_t       range;
    mem_addr_t        limit;
    longint unsigned max_addr;
    longint unsigned base_addr;

    max_addr  = 64'h0000_ffff_ffff_ffff;
    base_addr = {16'h0, base};
    main_mem_range_configured = 1'b1;

    if (capacity == 0) begin
        return;
    end
    else if (capacity - 1 > max_addr - base_addr) begin
        limit = 48'hffff_ffff_ffff;
    end
    else begin
        limit = base + mem_addr_t'(capacity - 1);
    end

    range.base  = base;
    range.limit = limit;
    main_mem_ranges.push_back(range);
endfunction:init_main_mem_range

function void mem_access_base_sequence::clear_main_mem_ranges();
    main_mem_ranges.delete();
    main_mem_range_configured = 1'b0;
endfunction:clear_main_mem_ranges

function bit mem_access_base_sequence::is_main_mem_access_in_range(input mem_addr_t addr, input mem_line_mask_t byte_mask);
    mem_addr_t byte_addr;
    bit        byte_in_range;

    foreach (byte_mask[i]) begin
        if (byte_mask[i]) begin
            if (mem_addr_t'(i) > 48'hffff_ffff_ffff - addr) begin
                return 1'b0;
            end

            byte_addr = addr + mem_addr_t'(i);
            if (main_mem_range_configured) begin
                byte_in_range = 1'b0;
                foreach (main_mem_ranges[j]) begin
                    if ((byte_addr >= main_mem_ranges[j].base) && (byte_addr <= main_mem_ranges[j].limit)) begin
                        byte_in_range = 1'b1;
                    end
                end
                if (!byte_in_range) begin
                    return 1'b0;
                end
            end
        end
    end
    return 1'b1;
endfunction:is_main_mem_access_in_range

function void mem_access_base_sequence::paddr_to_error(input mem_addr_t addr, output bit corrupt, output bit denied);
    corrupt = 1'b0;
    denied  = 1'b0;
endfunction:paddr_to_error

function mem_access_base_sequence::mem_line_data_t mem_access_base_sequence::build_lazy_line(input mem_line_addr_t line_addr);
    mem_line_data_t line_data;
    bit [31:0]      seed;

    line_data = '0;
    for (int unsigned i = 0; i < 1024; i++) begin
        seed = {line_addr[15:0], line_addr[31:16]} ^ (32'h9e37_79b9 + (i * 32'h45d9_f3b));
        line_data[(i * 8) +: 8] = seed[7:0] ^ seed[15:8] ^ seed[23:16] ^ seed[31:24];
    end
    return line_data;
endfunction:build_lazy_line

function void mem_access_base_sequence::ensure_main_line(input mem_line_addr_t line_addr);
    if (!main_mem.exists(line_addr)) begin
        main_mem[line_addr] = build_lazy_line(line_addr);
    end
endfunction:ensure_main_line

function void mem_access_base_sequence::ensure_prog_line(input mem_line_addr_t line_addr);
    if (!prog_mem.exists(line_addr)) begin
        prog_mem[line_addr]            = '0;
        prog_mem_byte_valid[line_addr] = '0;
    end
endfunction:ensure_prog_line

task mem_access_base_sequence::main_mem_access_task(
    input  mem_addr_t       addr,
    input  bit              is_store,
    input  mem_line_mask_t  byte_mask,
    input  mem_line_data_t  store_data,
    output bit              corrupt,
    output bit              denied,
    output mem_line_data_t  load_data
);
    mem_addr_t      byte_addr;
    mem_line_addr_t line_addr;
    bit [9:0]       byte_offset;
    bit             addr_corrupt;
    bit             addr_denied;

    corrupt   = 1'b0;
    denied    = 1'b0;
    load_data = '0;

    foreach (byte_mask[i]) begin
        if (byte_mask[i]) begin
            byte_addr = addr + mem_addr_t'(i);
            paddr_to_error(byte_addr, addr_corrupt, addr_denied);
            corrupt |= addr_corrupt;
            denied  |= addr_denied;
        end
    end

    if (!is_main_mem_access_in_range(addr, byte_mask)) begin
        denied = 1'b1;
    end

    if (!(corrupt || denied)) begin
        foreach (byte_mask[i]) begin
            if (byte_mask[i]) begin
                byte_addr    = addr + mem_addr_t'(i);
                line_addr    = byte_addr[47:10];
                byte_offset  = byte_addr[9:0];
                ensure_main_line(line_addr);

                if (is_store) begin
                    main_mem[line_addr][(byte_offset * 8) +: 8] = store_data[(i * 8) +: 8];
                end
                else begin
                    load_data[(i * 8) +: 8] = main_mem[line_addr][(byte_offset * 8) +: 8];
                end
            end
        end
    end

    if (corrupt || denied) begin
        load_data = '0;
    end

endtask:main_mem_access_task

task mem_access_base_sequence::prog_mem_access_task(
    input  mem_addr_t       addr,
    input  bit              is_store,
    input  mem_line_mask_t  byte_mask,
    input  mem_line_data_t  store_data,
    output bit              corrupt,
    output bit              denied,
    output mem_line_data_t  load_data
);
    mem_addr_t      byte_addr;
    mem_line_addr_t line_addr;
    bit [9:0]       byte_offset;
    mem_line_data_t main_load_data;
    bit             main_corrupt;
    bit             main_denied;

    corrupt   = 1'b0;
    denied    = 1'b0;
    load_data = '0;

    main_mem_access_task(addr, 1'b0, byte_mask, '0, main_corrupt, main_denied, main_load_data);
    corrupt = main_corrupt;
    denied  = main_denied;
    if (corrupt || denied) begin
        load_data = '0;
        return;
    end

    foreach (byte_mask[i]) begin
        if (byte_mask[i]) begin
            byte_addr   = addr + mem_addr_t'(i);
            line_addr   = byte_addr[47:10];
            byte_offset = byte_addr[9:0];

            if (is_store) begin
                ensure_prog_line(line_addr);
                prog_mem[line_addr][(byte_offset * 8) +: 8] = store_data[(i * 8) +: 8];
                prog_mem_byte_valid[line_addr][byte_offset] = 1'b1;
            end
            else if (prog_mem_byte_valid.exists(line_addr) && prog_mem_byte_valid[line_addr][byte_offset]) begin
                load_data[(i * 8) +: 8] = prog_mem[line_addr][(byte_offset * 8) +: 8];
            end
            else begin
                load_data[(i * 8) +: 8] = main_load_data[(i * 8) +: 8];
            end
        end
    end
endtask:prog_mem_access_task

class dcache_mem__access_base_sequence extends mem_access_base_sequence;

    int unsigned default_pre_pkt_gap;
    int unsigned default_post_pkt_gap;
    virtual dcache_agent_agent_interface dcache_vif;

    `uvm_object_utils(dcache_mem__access_base_sequence)

    extern function new(string name = "dcache_mem__access_base_sequence");
    extern virtual function void build_dcache_idle_xaction(output dcache_agent_agent_xaction rsp_xact);
    extern virtual function void capture_dcache_a_xaction(output dcache_agent_agent_xaction req_xact);
    extern virtual function bit is_store_opcode(input bit [3:0] opcode);
    extern virtual function bit is_acquire_opcode(input bit [3:0] opcode);
    extern virtual function bit [3:0] dcache_d_opcode(input bit [3:0] opcode);
    extern virtual function int unsigned dcache_d_beats(input bit [3:0] opcode,
                                                        input bit [2:0] size);
    extern virtual function bit [31:0] dcache_beat_mask(input bit [3:0] opcode,
                                                        input bit [31:0] req_mask);
    extern virtual function bit [47:0] dcache_beat_addr(input bit [47:0] addr);
    extern virtual task send_dcache_xaction(input dcache_agent_agent_xaction rsp_xact);
    extern virtual task dcache_mem_access_task(
        input  bit [47:0]  addr,
        input  bit         is_store,
        input  bit [31:0]  byte_mask,
        input  bit [255:0] store_data,
        output bit         corrupt,
        output bit         denied,
        output bit [255:0] load_data
    );
    extern virtual task dcache_mem_access_xaction(
        input  dcache_agent_agent_xaction req_xact,
        output dcache_agent_agent_xaction rsp_xact
    );
    extern virtual task body();

endclass:dcache_mem__access_base_sequence

function dcache_mem__access_base_sequence::new(string name = "dcache_mem__access_base_sequence");
    super.new(name);
    default_pre_pkt_gap  = 0;
    default_post_pkt_gap = 0;
endfunction:new

function void dcache_mem__access_base_sequence::build_dcache_idle_xaction(output dcache_agent_agent_xaction rsp_xact);
    rsp_xact = dcache_agent_agent_xaction::type_id::create("dcache_idle_xact");
    rsp_xact.auto_inner_dcache_client_out_a_ready        = 1'b0;
    rsp_xact.auto_inner_dcache_client_out_b_valid        = 1'b0;
    rsp_xact.auto_inner_dcache_client_out_c_ready        = 1'b1;
    rsp_xact.auto_inner_dcache_client_out_d_valid        = 1'b0;
    rsp_xact.auto_inner_dcache_client_out_e_ready        = 1'b1;
endfunction:build_dcache_idle_xaction

function void dcache_mem__access_base_sequence::capture_dcache_a_xaction(output dcache_agent_agent_xaction req_xact);
    req_xact = dcache_agent_agent_xaction::type_id::create("dcache_a_req_xact");
    req_xact.auto_inner_dcache_client_out_a_valid                     = dcache_vif.auto_inner_dcache_client_out_a_valid;
    req_xact.auto_inner_dcache_client_out_a_ready                     = dcache_vif.auto_inner_dcache_client_out_a_ready;
    req_xact.auto_inner_dcache_client_out_a_bits_opcode               = dcache_vif.auto_inner_dcache_client_out_a_bits_opcode;
    req_xact.auto_inner_dcache_client_out_a_bits_param                = dcache_vif.auto_inner_dcache_client_out_a_bits_param;
    req_xact.auto_inner_dcache_client_out_a_bits_size                 = dcache_vif.auto_inner_dcache_client_out_a_bits_size;
    req_xact.auto_inner_dcache_client_out_a_bits_source               = dcache_vif.auto_inner_dcache_client_out_a_bits_source;
    req_xact.auto_inner_dcache_client_out_a_bits_address              = dcache_vif.auto_inner_dcache_client_out_a_bits_address;
    req_xact.auto_inner_dcache_client_out_a_bits_user_alias           = dcache_vif.auto_inner_dcache_client_out_a_bits_user_alias;
    req_xact.auto_inner_dcache_client_out_a_bits_user_memPageType_NC  = dcache_vif.auto_inner_dcache_client_out_a_bits_user_memPageType_NC;
    req_xact.auto_inner_dcache_client_out_a_bits_user_memBackType_MM  = dcache_vif.auto_inner_dcache_client_out_a_bits_user_memBackType_MM;
    req_xact.auto_inner_dcache_client_out_a_bits_user_vaddr           = dcache_vif.auto_inner_dcache_client_out_a_bits_user_vaddr;
    req_xact.auto_inner_dcache_client_out_a_bits_user_reqSource       = dcache_vif.auto_inner_dcache_client_out_a_bits_user_reqSource;
    req_xact.auto_inner_dcache_client_out_a_bits_user_needHint        = dcache_vif.auto_inner_dcache_client_out_a_bits_user_needHint;
    req_xact.auto_inner_dcache_client_out_a_bits_echo_isKeyword       = dcache_vif.auto_inner_dcache_client_out_a_bits_echo_isKeyword;
    req_xact.auto_inner_dcache_client_out_a_bits_mask                 = dcache_vif.auto_inner_dcache_client_out_a_bits_mask;
    req_xact.auto_inner_dcache_client_out_a_bits_data                 = dcache_vif.auto_inner_dcache_client_out_a_bits_data;
    req_xact.auto_inner_dcache_client_out_a_bits_corrupt              = dcache_vif.auto_inner_dcache_client_out_a_bits_corrupt;
endfunction:capture_dcache_a_xaction

function bit dcache_mem__access_base_sequence::is_store_opcode(input bit [3:0] opcode);
    return (opcode == 4'd0) || (opcode == 4'd1);
endfunction:is_store_opcode

function bit dcache_mem__access_base_sequence::is_acquire_opcode(input bit [3:0] opcode);
    return (opcode == 4'd6) || (opcode == 4'd7);
endfunction:is_acquire_opcode

function bit [3:0] dcache_mem__access_base_sequence::dcache_d_opcode(input bit [3:0] opcode);
    case (opcode)
        4'd0, 4'd1: return 4'd0; // PutFullData/PutPartialData -> AccessAck
        4'd4:       return 4'd1; // Get -> AccessAckData
        4'd5:       return 4'd2; // Hint -> HintAck
        4'd6:       return 4'd5; // AcquireBlock -> GrantData
        4'd7:       return 4'd4; // AcquirePerm -> Grant
        default:    return 4'd1;
    endcase
endfunction:dcache_d_opcode

function int unsigned dcache_mem__access_base_sequence::dcache_d_beats(input bit [3:0] opcode,
                                                                       input bit [2:0] size);
    int unsigned bytes;
    int unsigned beats;

    if (!is_acquire_opcode(opcode) || opcode == 4'd7) begin
        return 1;
    end

    bytes = 1 << size;
    beats = (bytes + 31) / 32;
    if (beats == 0) begin
        beats = 1;
    end
    return beats;
endfunction:dcache_d_beats

function bit [31:0] dcache_mem__access_base_sequence::dcache_beat_mask(input bit [3:0] opcode,
                                                                       input bit [31:0] req_mask);
    if (is_acquire_opcode(opcode)) begin
        return 32'hffff_ffff;
    end
    return req_mask;
endfunction:dcache_beat_mask

function bit [47:0] dcache_mem__access_base_sequence::dcache_beat_addr(input bit [47:0] addr);
    return {addr[47:5], 5'b0};
endfunction:dcache_beat_addr

task dcache_mem__access_base_sequence::send_dcache_xaction(input dcache_agent_agent_xaction rsp_xact);
    start_item(rsp_xact);
    finish_item(rsp_xact);
endtask:send_dcache_xaction

task dcache_mem__access_base_sequence::dcache_mem_access_task(
    input  bit [47:0]  addr,
    input  bit         is_store,
    input  bit [31:0]  byte_mask,
    input  bit [255:0] store_data,
    output bit         corrupt,
    output bit         denied,
    output bit [255:0] load_data
);
    mem_line_mask_t line_mask;
    mem_line_data_t line_store_data;
    mem_line_data_t line_load_data;
    bit [47:0]      beat_addr;

    line_mask       = '0;
    line_store_data = '0;
    load_data       = '0;
    beat_addr       = dcache_beat_addr(addr);

    line_mask[31:0]        = byte_mask;
    line_store_data[255:0] = store_data;

    main_mem_access_task(beat_addr, is_store, line_mask, line_store_data, corrupt, denied, line_load_data);
    load_data = line_load_data[255:0];
endtask:dcache_mem_access_task

task dcache_mem__access_base_sequence::dcache_mem_access_xaction(
    input  dcache_agent_agent_xaction req_xact,
    output dcache_agent_agent_xaction rsp_xact
);
    bit         corrupt;
    bit         denied;
    bit [255:0] load_data;
    bit         is_store;

    rsp_xact = dcache_agent_agent_xaction::type_id::create("rsp_xact");

    is_store = is_store_opcode(req_xact.auto_inner_dcache_client_out_a_bits_opcode);
    dcache_mem_access_task(
        req_xact.auto_inner_dcache_client_out_a_bits_address,
        is_store,
        dcache_beat_mask(req_xact.auto_inner_dcache_client_out_a_bits_opcode,
                         req_xact.auto_inner_dcache_client_out_a_bits_mask),
        req_xact.auto_inner_dcache_client_out_a_bits_data,
        corrupt,
        denied,
        load_data
    );

    rsp_xact.auto_inner_dcache_client_out_a_ready                  = 1'b1;
    rsp_xact.auto_inner_dcache_client_out_b_valid                  = 1'b0;
    rsp_xact.auto_inner_dcache_client_out_b_bits_opcode            = '0;
    rsp_xact.auto_inner_dcache_client_out_b_bits_param             = '0;
    rsp_xact.auto_inner_dcache_client_out_b_bits_size              = '0;
    rsp_xact.auto_inner_dcache_client_out_b_bits_source            = '0;
    rsp_xact.auto_inner_dcache_client_out_b_bits_address           = '0;
    rsp_xact.auto_inner_dcache_client_out_b_bits_mask              = '0;
    rsp_xact.auto_inner_dcache_client_out_b_bits_data              = '0;
    rsp_xact.auto_inner_dcache_client_out_b_bits_corrupt           = 1'b0;
    rsp_xact.auto_inner_dcache_client_out_c_ready                  = 1'b1;
    rsp_xact.auto_inner_dcache_client_out_d_valid        = 1'b1;
    rsp_xact.auto_inner_dcache_client_out_d_bits_opcode  =
        dcache_d_opcode(req_xact.auto_inner_dcache_client_out_a_bits_opcode);
    rsp_xact.auto_inner_dcache_client_out_d_bits_param   = '0;
    rsp_xact.auto_inner_dcache_client_out_d_bits_size    = req_xact.auto_inner_dcache_client_out_a_bits_size;
    rsp_xact.auto_inner_dcache_client_out_d_bits_source  = req_xact.auto_inner_dcache_client_out_a_bits_source;
    rsp_xact.auto_inner_dcache_client_out_d_bits_sink    = '0;
    rsp_xact.auto_inner_dcache_client_out_d_bits_denied  = denied;
    rsp_xact.auto_inner_dcache_client_out_d_bits_echo_isKeyword = 1'b0;
    rsp_xact.auto_inner_dcache_client_out_d_bits_data    = is_store ? '0 : load_data;
    rsp_xact.auto_inner_dcache_client_out_d_bits_corrupt = corrupt;
    rsp_xact.auto_inner_dcache_client_out_e_ready        = 1'b1;
    rsp_xact.pre_pkt_gap                                 = default_pre_pkt_gap;
    rsp_xact.post_pkt_gap                                = default_post_pkt_gap;
endtask:dcache_mem_access_xaction

task dcache_mem__access_base_sequence::body();
    dcache_agent_agent_xaction idle_xact;
    dcache_agent_agent_xaction req_xact;
    dcache_agent_agent_xaction rsp_xact;

    if (!uvm_config_db#(virtual dcache_agent_agent_interface)::get(null, get_full_name(), "vif", dcache_vif) &&
        !uvm_config_db#(virtual dcache_agent_agent_interface)::get(null, "uvm_test_top.env.u_dcache_agent_agent*", "vif", dcache_vif)) begin
        `uvm_fatal(get_type_name(), "dcache virtual interface is not set for memory access sequence")
    end

    forever begin
        int unsigned beats;

        build_dcache_idle_xaction(idle_xact);
        send_dcache_xaction(idle_xact);

        if (dcache_vif.rst_n == 1'b1 &&
            memblock_sync_pkg::reset_backend_done == 1'b1 &&
            dcache_vif.auto_inner_dcache_client_out_a_valid === 1'b1) begin
            capture_dcache_a_xaction(req_xact);
            `uvm_info(get_type_name(),
                      $sformatf("accept DCache A opcode=%0d size=%0d source=%0d addr=0x%0h mask=0x%0h",
                                req_xact.auto_inner_dcache_client_out_a_bits_opcode,
                                req_xact.auto_inner_dcache_client_out_a_bits_size,
                                req_xact.auto_inner_dcache_client_out_a_bits_source,
                                req_xact.auto_inner_dcache_client_out_a_bits_address,
                                req_xact.auto_inner_dcache_client_out_a_bits_mask),
                      UVM_LOW)
            build_dcache_idle_xaction(idle_xact);
            idle_xact.auto_inner_dcache_client_out_a_ready = 1'b1;
            send_dcache_xaction(idle_xact);
            beats = dcache_d_beats(req_xact.auto_inner_dcache_client_out_a_bits_opcode,
                                   req_xact.auto_inner_dcache_client_out_a_bits_size);
            for (int unsigned beat_idx = 0; beat_idx < beats; beat_idx++) begin
                dcache_agent_agent_xaction beat_req_xact;
                bit [47:0] beat_addr;

                beat_req_xact = dcache_agent_agent_xaction::type_id::create(
                    $sformatf("dcache_beat_req_xact_%0d", beat_idx));
                beat_req_xact.copy(req_xact);
                beat_addr = dcache_beat_addr(req_xact.auto_inner_dcache_client_out_a_bits_address) +
                            (mem_addr_t'(beat_idx) * 48'd32);
                beat_req_xact.auto_inner_dcache_client_out_a_bits_address = beat_addr;
                dcache_mem_access_xaction(beat_req_xact, rsp_xact);
                rsp_xact.auto_inner_dcache_client_out_a_ready = 1'b0;

                do begin
                    send_dcache_xaction(rsp_xact);
                end while (dcache_vif.auto_inner_dcache_client_out_d_ready !== 1'b1);
                `uvm_info(get_type_name(),
                          $sformatf("send DCache D beat=%0d/%0d opcode=%0d source=%0d denied=%0d corrupt=%0d",
                                    beat_idx + 1,
                                    beats,
                                    rsp_xact.auto_inner_dcache_client_out_d_bits_opcode,
                                    rsp_xact.auto_inner_dcache_client_out_d_bits_source,
                                    rsp_xact.auto_inner_dcache_client_out_d_bits_denied,
                                    rsp_xact.auto_inner_dcache_client_out_d_bits_corrupt),
                          UVM_LOW)
            end
        end
    end
endtask:body

class sbuffer_mem_access_base_sequence extends mem_access_base_sequence;

    int unsigned default_pre_pkt_gap;
    int unsigned default_post_pkt_gap;
    virtual sbuffer_agent_agent_interface sbuffer_vif;

    `uvm_object_utils(sbuffer_mem_access_base_sequence)

    extern function new(string name = "sbuffer_mem_access_base_sequence");
    extern virtual function void build_sbuffer_idle_xaction(output sbuffer_agent_agent_xaction rsp_xact);
    extern virtual function void capture_sbuffer_a_xaction(output sbuffer_agent_agent_xaction req_xact);
    extern virtual function bit is_store_opcode(input bit [3:0] opcode);
    extern virtual function bit [47:0] sbuffer_beat_addr(input bit [47:0] addr);
    extern virtual task send_sbuffer_xaction(input sbuffer_agent_agent_xaction rsp_xact);
    extern virtual task sbuffer_mem_access_task(
        input  bit [47:0] addr,
        input  bit        is_store,
        input  bit [7:0]  byte_mask,
        input  bit [63:0] store_data,
        output bit        corrupt,
        output bit        denied,
        output bit [63:0] load_data
    );
    extern virtual task sbuffer_mem_access_xaction(
        input  sbuffer_agent_agent_xaction req_xact,
        output sbuffer_agent_agent_xaction rsp_xact
    );
    extern virtual task body();

endclass:sbuffer_mem_access_base_sequence

function sbuffer_mem_access_base_sequence::new(string name = "sbuffer_mem_access_base_sequence");
    super.new(name);
    default_pre_pkt_gap  = 0;
    default_post_pkt_gap = 0;
endfunction:new

function void sbuffer_mem_access_base_sequence::build_sbuffer_idle_xaction(output sbuffer_agent_agent_xaction rsp_xact);
    rsp_xact = sbuffer_agent_agent_xaction::type_id::create("sbuffer_idle_xact");
    rsp_xact.auto_inner_buffers_out_a_ready = 1'b0;
    rsp_xact.auto_inner_buffers_out_d_valid = 1'b0;
endfunction:build_sbuffer_idle_xaction

function void sbuffer_mem_access_base_sequence::capture_sbuffer_a_xaction(output sbuffer_agent_agent_xaction req_xact);
    req_xact = sbuffer_agent_agent_xaction::type_id::create("sbuffer_a_req_xact");
    req_xact.auto_inner_buffers_out_a_valid                    = sbuffer_vif.auto_inner_buffers_out_a_valid;
    req_xact.auto_inner_buffers_out_a_ready                    = sbuffer_vif.auto_inner_buffers_out_a_ready;
    req_xact.auto_inner_buffers_out_a_bits_opcode              = sbuffer_vif.auto_inner_buffers_out_a_bits_opcode;
    req_xact.auto_inner_buffers_out_a_bits_param               = sbuffer_vif.auto_inner_buffers_out_a_bits_param;
    req_xact.auto_inner_buffers_out_a_bits_size                = sbuffer_vif.auto_inner_buffers_out_a_bits_size;
    req_xact.auto_inner_buffers_out_a_bits_source              = sbuffer_vif.auto_inner_buffers_out_a_bits_source;
    req_xact.auto_inner_buffers_out_a_bits_address             = sbuffer_vif.auto_inner_buffers_out_a_bits_address;
    req_xact.auto_inner_buffers_out_a_bits_user_memBackType_MM = sbuffer_vif.auto_inner_buffers_out_a_bits_user_memBackType_MM;
    req_xact.auto_inner_buffers_out_a_bits_user_memPageType_NC = sbuffer_vif.auto_inner_buffers_out_a_bits_user_memPageType_NC;
    req_xact.auto_inner_buffers_out_a_bits_mask                = sbuffer_vif.auto_inner_buffers_out_a_bits_mask;
    req_xact.auto_inner_buffers_out_a_bits_data                = sbuffer_vif.auto_inner_buffers_out_a_bits_data;
    req_xact.auto_inner_buffers_out_a_bits_corrupt             = sbuffer_vif.auto_inner_buffers_out_a_bits_corrupt;
endfunction:capture_sbuffer_a_xaction

function bit sbuffer_mem_access_base_sequence::is_store_opcode(input bit [3:0] opcode);
    return (opcode == 4'd0) || (opcode == 4'd1);
endfunction:is_store_opcode

function bit [47:0] sbuffer_mem_access_base_sequence::sbuffer_beat_addr(input bit [47:0] addr);
    return {addr[47:3], 3'b0};
endfunction:sbuffer_beat_addr

task sbuffer_mem_access_base_sequence::send_sbuffer_xaction(input sbuffer_agent_agent_xaction rsp_xact);
    start_item(rsp_xact);
    finish_item(rsp_xact);
endtask:send_sbuffer_xaction

task sbuffer_mem_access_base_sequence::sbuffer_mem_access_task(
    input  bit [47:0] addr,
    input  bit        is_store,
    input  bit [7:0]  byte_mask,
    input  bit [63:0] store_data,
    output bit        corrupt,
    output bit        denied,
    output bit [63:0] load_data
);
    mem_line_mask_t line_mask;
    mem_line_data_t line_store_data;
    mem_line_data_t line_load_data;
    bit [47:0]      beat_addr;

    line_mask       = '0;
    line_store_data = '0;
    load_data       = '0;
    beat_addr       = sbuffer_beat_addr(addr);

    line_mask[7:0]       = byte_mask;
    line_store_data[63:0] = store_data;

    main_mem_access_task(beat_addr, is_store, line_mask, line_store_data, corrupt, denied, line_load_data);
    load_data = line_load_data[63:0];
endtask:sbuffer_mem_access_task

task sbuffer_mem_access_base_sequence::sbuffer_mem_access_xaction(
    input  sbuffer_agent_agent_xaction req_xact,
    output sbuffer_agent_agent_xaction rsp_xact
);
    bit        corrupt;
    bit        denied;
    bit [63:0] load_data;
    bit        is_store;

    rsp_xact = sbuffer_agent_agent_xaction::type_id::create("rsp_xact");

    is_store = is_store_opcode(req_xact.auto_inner_buffers_out_a_bits_opcode);
    sbuffer_mem_access_task(
        req_xact.auto_inner_buffers_out_a_bits_address,
        is_store,
        req_xact.auto_inner_buffers_out_a_bits_mask,
        req_xact.auto_inner_buffers_out_a_bits_data,
        corrupt,
        denied,
        load_data
    );

    rsp_xact.auto_inner_buffers_out_a_ready        = 1'b1;
    rsp_xact.auto_inner_buffers_out_d_valid        = 1'b1;
    rsp_xact.auto_inner_buffers_out_d_bits_opcode  = is_store ? 4'd0 : 4'd1;
    rsp_xact.auto_inner_buffers_out_d_bits_param   = '0;
    rsp_xact.auto_inner_buffers_out_d_bits_size    = req_xact.auto_inner_buffers_out_a_bits_size;
    rsp_xact.auto_inner_buffers_out_d_bits_source  = req_xact.auto_inner_buffers_out_a_bits_source;
    rsp_xact.auto_inner_buffers_out_d_bits_sink    = '0;
    rsp_xact.auto_inner_buffers_out_d_bits_denied  = denied;
    rsp_xact.auto_inner_buffers_out_d_bits_data    = is_store ? '0 : load_data;
    rsp_xact.auto_inner_buffers_out_d_bits_corrupt = corrupt;
    rsp_xact.pre_pkt_gap                           = default_pre_pkt_gap;
    rsp_xact.post_pkt_gap                          = default_post_pkt_gap;
endtask:sbuffer_mem_access_xaction

task sbuffer_mem_access_base_sequence::body();
    sbuffer_agent_agent_xaction idle_xact;
    sbuffer_agent_agent_xaction req_xact;
    sbuffer_agent_agent_xaction rsp_xact;

    if (!uvm_config_db#(virtual sbuffer_agent_agent_interface)::get(null, get_full_name(), "vif", sbuffer_vif) &&
        !uvm_config_db#(virtual sbuffer_agent_agent_interface)::get(null, "uvm_test_top.env.u_sbuffer_agent_agent*", "vif", sbuffer_vif)) begin
        `uvm_fatal(get_type_name(), "sbuffer virtual interface is not set for memory access sequence")
    end

    forever begin
        build_sbuffer_idle_xaction(idle_xact);
        send_sbuffer_xaction(idle_xact);

        if (sbuffer_vif.rst_n == 1'b1 &&
            memblock_sync_pkg::reset_backend_done == 1'b1 &&
            sbuffer_vif.auto_inner_buffers_out_a_valid === 1'b1) begin
            capture_sbuffer_a_xaction(req_xact);
            build_sbuffer_idle_xaction(idle_xact);
            idle_xact.auto_inner_buffers_out_a_ready = 1'b1;
            send_sbuffer_xaction(idle_xact);
            sbuffer_mem_access_xaction(req_xact, rsp_xact);
            rsp_xact.auto_inner_buffers_out_a_ready = 1'b0;

            do begin
                send_sbuffer_xaction(rsp_xact);
            end while (sbuffer_vif.auto_inner_buffers_out_d_ready !== 1'b1);
        end
    end
endtask:body

`endif
