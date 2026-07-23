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

    localparam bit [3:0] TL_A_OPCODE_ACQUIRE_BLOCK = 4'd6;
    localparam bit [3:0] TL_A_OPCODE_ACQUIRE_PERM  = 4'd7;
    localparam bit [3:0] TL_A_OPCODE_CBO_CLEAN     = 4'd12;
    localparam bit [3:0] TL_A_OPCODE_CBO_FLUSH     = 4'd13;
    localparam bit [3:0] TL_A_OPCODE_CBO_INVAL     = 4'd14;

    localparam bit [2:0] TL_B_OPCODE_PROBE         = 3'd6;
    localparam bit [2:0] TL_C_OPCODE_PROBE_ACK     = 3'd4;
    localparam bit [2:0] TL_C_OPCODE_PROBE_ACKDATA = 3'd5;
    localparam bit [2:0] TL_C_OPCODE_RELEASE       = 3'd6;
    localparam bit [2:0] TL_C_OPCODE_RELEASEDATA   = 3'd7;

    localparam bit [3:0] TL_D_OPCODE_GRANT         = 4'd4;
    localparam bit [3:0] TL_D_OPCODE_GRANT_DATA    = 4'd5;
    localparam bit [3:0] TL_D_OPCODE_RELEASE_ACK   = 4'd6;
    localparam bit [3:0] TL_D_OPCODE_CBO_ACK       = 4'd8;

    localparam bit [2:0] TL_GROW_NTOB              = 3'd0;
    localparam bit [2:0] TL_GROW_NTOT              = 3'd1;
    localparam bit [2:0] TL_GROW_BTOT              = 3'd2;

    localparam bit [1:0] TL_CAP_TOT                = 2'd0;
    localparam bit [1:0] TL_CAP_TOB                = 2'd1;
    localparam bit [1:0] TL_CAP_TON                = 2'd2;

    localparam bit [2:0] TL_REPORT_TTON            = 3'd1;
    localparam bit [2:0] TL_REPORT_BTON            = 3'd2;
    localparam bit [2:0] TL_REPORT_NTON            = 3'd5;

    localparam bit [2:0] TL_LINE_SIZE              = 3'd6;
    localparam bit [5:0] TL_CBO_SOURCE             = 6'd17;
    localparam bit [9:0] TL_FIXED_SINK             = 10'd0;

    typedef enum int unsigned {
        DCACHE_PENDING_D_NONE        = 0,
        DCACHE_PENDING_D_GRANT       = 1,
        DCACHE_PENDING_D_GRANT_DATA  = 2,
        DCACHE_PENDING_D_CBO_ACK     = 3,
        DCACHE_PENDING_D_RELEASE_ACK = 4
    } dcache_pending_d_kind_e;

    typedef enum int unsigned {
        DCACHE_C_OWNER_NONE    = 0,
        DCACHE_C_OWNER_PROBE   = 1,
        DCACHE_C_OWNER_RELEASE = 2
    } dcache_c_owner_e;

    virtual dcache_agent_agent_interface dcache_vif;

    // 中文注释：service loop 的统一拍计数，只在 body() 单一入口递增。
    // 设置：每发送一个 cycle_xact 后自增 1；清零：body() 启动时。
    // 作用：pending D delay、hint 排期和调试日志都只使用这个单一时间基准。
    longint unsigned service_cycle;
    longint unsigned last_drive_cycle;
    dcache_agent_agent_xaction last_cycle_xact;
    bit last_cycle_valid;

    // 中文注释：A/C request 在当前拍看到 valid 时先保存 payload，并在下一驱动边沿确认真实 fire。
    // 设置：当拍 arm ready 前保存 sampled request；清零：fire 成功、valid 撤销或 reset。
    // 作用：避免还没真正经过 driver clocking edge 就提前宣称 A/C 已被接受。
    bit a_accept_armed;
    bit c_accept_armed;
    dcache_agent_agent_xaction armed_a_req_xact;
    dcache_agent_agent_xaction armed_c_req_xact;

    // 中文注释：唯一在途 D reply 的完整生命周期状态。
    // 设置：A Acquire/CBO 或 C Release/ReleaseData 完成后建立；清零：最后一拍 D.fire 或 reset。
    // 作用：统一管理 response delay、Grant/GrantData/CBOAck/ReleaseAck 的 hold 和后续 GrantAck owner。
    bit pending_d_valid;
    dcache_pending_d_kind_e pending_d_kind;
    bit [3:0] pending_d_cbo_opcode;
    longint unsigned pending_d_due_cycle;
    int unsigned pending_d_beat_count;
    int unsigned pending_d_beat_idx;
    bit [1:0] pending_d_param;
    bit [2:0] pending_d_size;
    bit [5:0] pending_d_source;
    bit [9:0] pending_d_sink;
    bit pending_d_denied;
    bit pending_d_corrupt;
    bit pending_d_echo_isKeyword;
    bit [47:0] pending_d_line_addr;
    bit [1:0] pending_d_alias;
    bit [255:0] pending_d_data_low;
    bit [255:0] pending_d_data_high;

    // 中文注释：Grant/GrantData 最后一拍 D.fire 后等待 E GrantAck 的唯一 owner。
    // 设置：Grant 或 GrantData 完成时保存 line/alias/sink；清零：匹配 sink 的 E.fire 或 reset。
    // 作用：只有收到真实 GrantAck 后，才允许把 line 加入 Probe 候选地址表。
    bit waiting_grant_ack;
    bit [47:0] pending_grant_line;
    bit [1:0] pending_grant_alias;
    bit [9:0] pending_grant_expected_sink;

    // 中文注释：每个 AcquireBlock 最多发一次 hint 的排期状态。
    // 设置：accept_dcache_a_request() 选中 GrantData hint 后保存；清零：hint 发出、D reply 完成或 reset。
    // 作用：保证 hint 只由专用 responder 产生，且与已接受的 GrantData 生命周期绑定。
    bit hint_selected;
    bit hint_sent;
    longint unsigned hint_due_cycle;
    bit [3:0] hint_source_id;
    bit hint_isKeyword;

    // 中文注释：DCache 已完成 GrantAck 的 cache line 候选表，只保存 line 对齐地址和 alias。
    // 设置：GrantAck fire 后插入/覆盖；清零：ProbeAck 完成、Release 完成、CBOFlush/Inval 完成或 reset。
    // 作用：轻量 Probe 只能从这个表里随机挑选，主内存 data 仍由 main_mem 唯一持有。
    bit [1:0] cached_alias_by_line[mem_addr_t];

    // 中文注释：同一时刻只允许一个 Probe launch 和一个等待中的 Probe C reply。
    // 设置：try_start_probe() 选中 map entry 后置 launch，B.fire 后切到 waiting_probe_c。
    // 清零：ProbeAck/ProbeAckData 完整结束或 reset。
    // 作用：避免引入多 Probe 并发和第二份 directory owner。
    bit pending_probe_b_valid;
    bit waiting_probe_c;
    bit [47:0] pending_probe_line;
    bit [1:0] pending_probe_alias;

    // 中文注释：当前正在收集的 C-channel 多拍 transaction。
    // 设置：首拍 ProbeAckData/ReleaseData fire 后建 owner 和 beat0 缓冲；清零：完整 2 beat 收齐或 reset。
    // 作用：统一管理 ProbeAckData/ReleaseData 的字段稳定性检查、corrupt 聚合和完成时副作用。
    dcache_c_owner_e c_assembly_owner;
    bit [2:0] c_assembly_opcode;
    int unsigned c_assembly_received_beats;
    bit [47:0] c_assembly_line;
    bit [5:0] c_assembly_source;
    bit [2:0] c_assembly_size;
    bit [2:0] c_assembly_param;
    bit c_assembly_corrupt_seen;
    bit [511:0] c_assembly_data;

    `uvm_object_utils(dcache_mem__access_base_sequence)

    extern function new(string name = "dcache_mem__access_base_sequence");
    extern virtual function void clear_pending_d_state();
    extern virtual function void clear_c_assembly_state();
    extern virtual function void clear_hint_state();
    extern virtual function void clear_runtime_state(bit clear_cache_map = 1'b1);
    extern virtual function void build_dcache_idle_xaction(output dcache_agent_agent_xaction rsp_xact);
    extern virtual function void capture_dcache_a_xaction(output dcache_agent_agent_xaction req_xact);
    extern virtual function void capture_dcache_c_xaction(output dcache_agent_agent_xaction req_xact);
    extern virtual function void check_a_payload_stable(
        input dcache_agent_agent_xaction expected_xact,
        input dcache_agent_agent_xaction observed_xact
    );
    extern virtual function void check_c_payload_stable(
        input dcache_agent_agent_xaction expected_xact,
        input dcache_agent_agent_xaction observed_xact
    );
    extern virtual function bit [47:0] line_addr64(input bit [47:0] addr);
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
    extern virtual function void check_l2_model_cfg();
    extern virtual function void check_line_range(input bit [47:0] line_addr, input string ctx);
    extern virtual task load_grant_line(
        input  bit [47:0]  line_addr,
        output bit [255:0] data_low,
        output bit [255:0] data_high
    );
    extern virtual function void record_cached_line(input bit [47:0] addr, input bit [1:0] line_alias);
    extern virtual function void remove_cached_line(input bit [47:0] addr, input string reason);
    extern virtual function int unsigned sample_l2_response_delay();
    extern virtual function bit sample_hint_enable();
    extern virtual function bit sample_probe_enable();
    extern virtual function bit select_random_cached_line(output bit [47:0] line_addr, output bit [1:0] line_alias);
    extern virtual task accept_dcache_a_request(
        input dcache_agent_agent_xaction req_xact,
        input longint unsigned           accept_cycle
    );
    extern virtual function void build_pending_d_xaction(inout dcache_agent_agent_xaction cycle_xact);
    extern virtual function void process_d_fire();
    extern virtual function void process_e_fire();
    extern virtual task complete_probe_c_assembly(input longint unsigned complete_cycle);
    extern virtual task complete_release_c_assembly(input longint unsigned complete_cycle);
    extern virtual task consume_c_beat(
        input dcache_agent_agent_xaction c_req_xact,
        input longint unsigned           accept_cycle
    );
    extern virtual task start_c_assembly(
        input dcache_agent_agent_xaction c_req_xact,
        input longint unsigned           accept_cycle
    );
    extern virtual function void try_start_probe(input bit allow_new_probe = 1'b1);
    extern virtual function void service_hint(
        input longint unsigned           current_cycle,
        inout dcache_agent_agent_xaction cycle_xact
    );
    extern virtual task body();

endclass:dcache_mem__access_base_sequence

function dcache_mem__access_base_sequence::new(string name = "dcache_mem__access_base_sequence");
    super.new(name);
    service_cycle        = 0;
    last_drive_cycle     = 0;
    last_cycle_xact      = null;
    clear_runtime_state(1'b1);
endfunction:new

function void dcache_mem__access_base_sequence::clear_pending_d_state();
    pending_d_valid         = 1'b0;
    pending_d_kind          = DCACHE_PENDING_D_NONE;
    pending_d_cbo_opcode    = '0;
    pending_d_due_cycle     = 0;
    pending_d_beat_count    = 0;
    pending_d_beat_idx      = 0;
    pending_d_param         = '0;
    pending_d_size          = '0;
    pending_d_source        = '0;
    pending_d_sink          = '0;
    pending_d_denied        = 1'b0;
    pending_d_corrupt       = 1'b0;
    pending_d_echo_isKeyword = 1'b0;
    pending_d_line_addr     = '0;
    pending_d_alias         = '0;
    pending_d_data_low      = '0;
    pending_d_data_high     = '0;
endfunction:clear_pending_d_state

function void dcache_mem__access_base_sequence::clear_c_assembly_state();
    c_assembly_owner          = DCACHE_C_OWNER_NONE;
    c_assembly_opcode         = '0;
    c_assembly_received_beats = 0;
    c_assembly_line           = '0;
    c_assembly_source         = '0;
    c_assembly_size           = '0;
    c_assembly_param          = '0;
    c_assembly_corrupt_seen   = 1'b0;
    c_assembly_data           = '0;
endfunction:clear_c_assembly_state

function void dcache_mem__access_base_sequence::clear_hint_state();
    hint_selected  = 1'b0;
    hint_sent      = 1'b0;
    hint_due_cycle = 0;
    hint_source_id = '0;
    hint_isKeyword = 1'b0;
endfunction:clear_hint_state

function void dcache_mem__access_base_sequence::clear_runtime_state(bit clear_cache_map = 1'b1);
    a_accept_armed           = 1'b0;
    c_accept_armed           = 1'b0;
    armed_a_req_xact         = null;
    armed_c_req_xact         = null;
    waiting_grant_ack        = 1'b0;
    pending_grant_line       = '0;
    pending_grant_alias      = '0;
    pending_grant_expected_sink = '0;
    pending_probe_b_valid    = 1'b0;
    waiting_probe_c          = 1'b0;
    pending_probe_line       = '0;
    pending_probe_alias      = '0;
    last_cycle_valid         = 1'b0;
    last_cycle_xact          = null;
    clear_pending_d_state();
    clear_c_assembly_state();
    clear_hint_state();
    if (clear_cache_map) begin
        cached_alias_by_line.delete();
    end
endfunction:clear_runtime_state

function void dcache_mem__access_base_sequence::build_dcache_idle_xaction(output dcache_agent_agent_xaction rsp_xact);
    rsp_xact = dcache_agent_agent_xaction::type_id::create("dcache_idle_xact");
    rsp_xact.auto_inner_dcache_client_out_a_ready            = 1'b0;
    rsp_xact.auto_inner_dcache_client_out_b_valid            = 1'b0;
    rsp_xact.auto_inner_dcache_client_out_b_bits_opcode      = '0;
    rsp_xact.auto_inner_dcache_client_out_b_bits_param       = '0;
    rsp_xact.auto_inner_dcache_client_out_b_bits_size        = '0;
    rsp_xact.auto_inner_dcache_client_out_b_bits_source      = '0;
    rsp_xact.auto_inner_dcache_client_out_b_bits_address     = '0;
    rsp_xact.auto_inner_dcache_client_out_b_bits_mask        = '0;
    rsp_xact.auto_inner_dcache_client_out_b_bits_data        = '0;
    rsp_xact.auto_inner_dcache_client_out_b_bits_corrupt     = 1'b0;
    rsp_xact.auto_inner_dcache_client_out_c_ready            = 1'b0;
    rsp_xact.auto_inner_dcache_client_out_d_valid            = 1'b0;
    rsp_xact.auto_inner_dcache_client_out_d_bits_opcode      = '0;
    rsp_xact.auto_inner_dcache_client_out_d_bits_param       = '0;
    rsp_xact.auto_inner_dcache_client_out_d_bits_size        = '0;
    rsp_xact.auto_inner_dcache_client_out_d_bits_source      = '0;
    rsp_xact.auto_inner_dcache_client_out_d_bits_sink        = '0;
    rsp_xact.auto_inner_dcache_client_out_d_bits_denied      = 1'b0;
    rsp_xact.auto_inner_dcache_client_out_d_bits_echo_isKeyword = 1'b0;
    rsp_xact.auto_inner_dcache_client_out_d_bits_data        = '0;
    rsp_xact.auto_inner_dcache_client_out_d_bits_corrupt     = 1'b0;
    // E 只能在 D 最后一拍完成、GrantAck owner 已建立后开放；否则 DUT
    // 可能提前消费 E，导致 responder 尚未切换 owner 就丢失 GrantAck。
    rsp_xact.auto_inner_dcache_client_out_e_ready            = 1'b0;
    rsp_xact.io_l2_hint_valid                                = 1'b0;
    rsp_xact.io_l2_hint_bits_sourceId                        = '0;
    rsp_xact.io_l2_hint_bits_isKeyword                       = 1'b0;
    rsp_xact.io_l2_flush_done                                = 1'b0;
    rsp_xact.pre_pkt_gap                                     = 0;
    rsp_xact.post_pkt_gap                                    = 0;
endfunction:build_dcache_idle_xaction

function void dcache_mem__access_base_sequence::capture_dcache_a_xaction(output dcache_agent_agent_xaction req_xact);
    req_xact = dcache_agent_agent_xaction::type_id::create("dcache_a_req_xact");
    req_xact.auto_inner_dcache_client_out_a_valid               = dcache_vif.drv_cb.auto_inner_dcache_client_out_a_valid;
    req_xact.auto_inner_dcache_client_out_a_ready               = 1'b0;
    req_xact.auto_inner_dcache_client_out_a_bits_opcode         = dcache_vif.drv_cb.auto_inner_dcache_client_out_a_bits_opcode;
    req_xact.auto_inner_dcache_client_out_a_bits_param          = dcache_vif.drv_cb.auto_inner_dcache_client_out_a_bits_param;
    req_xact.auto_inner_dcache_client_out_a_bits_size           = dcache_vif.drv_cb.auto_inner_dcache_client_out_a_bits_size;
    req_xact.auto_inner_dcache_client_out_a_bits_source         = dcache_vif.drv_cb.auto_inner_dcache_client_out_a_bits_source;
    req_xact.auto_inner_dcache_client_out_a_bits_address        = dcache_vif.drv_cb.auto_inner_dcache_client_out_a_bits_address;
    req_xact.auto_inner_dcache_client_out_a_bits_user_alias     = dcache_vif.drv_cb.auto_inner_dcache_client_out_a_bits_user_alias;
    req_xact.auto_inner_dcache_client_out_a_bits_user_vaddr     = dcache_vif.drv_cb.auto_inner_dcache_client_out_a_bits_user_vaddr;
    req_xact.auto_inner_dcache_client_out_a_bits_user_reqSource = dcache_vif.drv_cb.auto_inner_dcache_client_out_a_bits_user_reqSource;
    req_xact.auto_inner_dcache_client_out_a_bits_user_needHint  = dcache_vif.drv_cb.auto_inner_dcache_client_out_a_bits_user_needHint;
    req_xact.auto_inner_dcache_client_out_a_bits_echo_isKeyword = dcache_vif.drv_cb.auto_inner_dcache_client_out_a_bits_echo_isKeyword;
    req_xact.auto_inner_dcache_client_out_a_bits_mask           = dcache_vif.drv_cb.auto_inner_dcache_client_out_a_bits_mask;
    req_xact.auto_inner_dcache_client_out_a_bits_data           = dcache_vif.drv_cb.auto_inner_dcache_client_out_a_bits_data;
    req_xact.auto_inner_dcache_client_out_a_bits_corrupt        = dcache_vif.drv_cb.auto_inner_dcache_client_out_a_bits_corrupt;
endfunction:capture_dcache_a_xaction

function void dcache_mem__access_base_sequence::capture_dcache_c_xaction(output dcache_agent_agent_xaction req_xact);
    req_xact = dcache_agent_agent_xaction::type_id::create("dcache_c_req_xact");
    req_xact.auto_inner_dcache_client_out_c_valid               = dcache_vif.drv_cb.auto_inner_dcache_client_out_c_valid;
    req_xact.auto_inner_dcache_client_out_c_ready               = 1'b0;
    req_xact.auto_inner_dcache_client_out_c_bits_opcode         = dcache_vif.drv_cb.auto_inner_dcache_client_out_c_bits_opcode;
    req_xact.auto_inner_dcache_client_out_c_bits_param          = dcache_vif.drv_cb.auto_inner_dcache_client_out_c_bits_param;
    req_xact.auto_inner_dcache_client_out_c_bits_size           = dcache_vif.drv_cb.auto_inner_dcache_client_out_c_bits_size;
    req_xact.auto_inner_dcache_client_out_c_bits_source         = dcache_vif.drv_cb.auto_inner_dcache_client_out_c_bits_source;
    req_xact.auto_inner_dcache_client_out_c_bits_address        = dcache_vif.drv_cb.auto_inner_dcache_client_out_c_bits_address;
    req_xact.auto_inner_dcache_client_out_c_bits_user_alias     = dcache_vif.drv_cb.auto_inner_dcache_client_out_c_bits_user_alias;
    req_xact.auto_inner_dcache_client_out_c_bits_user_vaddr     = dcache_vif.drv_cb.auto_inner_dcache_client_out_c_bits_user_vaddr;
    req_xact.auto_inner_dcache_client_out_c_bits_user_reqSource = dcache_vif.drv_cb.auto_inner_dcache_client_out_c_bits_user_reqSource;
    req_xact.auto_inner_dcache_client_out_c_bits_user_needHint  = dcache_vif.drv_cb.auto_inner_dcache_client_out_c_bits_user_needHint;
    req_xact.auto_inner_dcache_client_out_c_bits_echo_isKeyword = dcache_vif.drv_cb.auto_inner_dcache_client_out_c_bits_echo_isKeyword;
    req_xact.auto_inner_dcache_client_out_c_bits_data           = dcache_vif.drv_cb.auto_inner_dcache_client_out_c_bits_data;
    req_xact.auto_inner_dcache_client_out_c_bits_corrupt        = dcache_vif.drv_cb.auto_inner_dcache_client_out_c_bits_corrupt;
endfunction:capture_dcache_c_xaction

function void dcache_mem__access_base_sequence::check_a_payload_stable(
    input dcache_agent_agent_xaction expected_xact,
    input dcache_agent_agent_xaction observed_xact
);
    if (expected_xact == null || observed_xact == null) begin
        `uvm_fatal(get_type_name(), "A payload stability check received a null snapshot")
    end
    if (expected_xact.auto_inner_dcache_client_out_a_bits_opcode != observed_xact.auto_inner_dcache_client_out_a_bits_opcode ||
        expected_xact.auto_inner_dcache_client_out_a_bits_param != observed_xact.auto_inner_dcache_client_out_a_bits_param ||
        expected_xact.auto_inner_dcache_client_out_a_bits_size != observed_xact.auto_inner_dcache_client_out_a_bits_size ||
        expected_xact.auto_inner_dcache_client_out_a_bits_source != observed_xact.auto_inner_dcache_client_out_a_bits_source ||
        expected_xact.auto_inner_dcache_client_out_a_bits_address != observed_xact.auto_inner_dcache_client_out_a_bits_address ||
        expected_xact.auto_inner_dcache_client_out_a_bits_user_alias != observed_xact.auto_inner_dcache_client_out_a_bits_user_alias ||
        expected_xact.auto_inner_dcache_client_out_a_bits_user_vaddr != observed_xact.auto_inner_dcache_client_out_a_bits_user_vaddr ||
        expected_xact.auto_inner_dcache_client_out_a_bits_user_reqSource != observed_xact.auto_inner_dcache_client_out_a_bits_user_reqSource ||
        expected_xact.auto_inner_dcache_client_out_a_bits_user_needHint != observed_xact.auto_inner_dcache_client_out_a_bits_user_needHint ||
        expected_xact.auto_inner_dcache_client_out_a_bits_echo_isKeyword != observed_xact.auto_inner_dcache_client_out_a_bits_echo_isKeyword ||
        expected_xact.auto_inner_dcache_client_out_a_bits_mask != observed_xact.auto_inner_dcache_client_out_a_bits_mask ||
        expected_xact.auto_inner_dcache_client_out_a_bits_data != observed_xact.auto_inner_dcache_client_out_a_bits_data ||
        expected_xact.auto_inner_dcache_client_out_a_bits_corrupt != observed_xact.auto_inner_dcache_client_out_a_bits_corrupt) begin
        `uvm_fatal(get_type_name(), "DCache A payload changed while valid was waiting for ready")
    end
endfunction:check_a_payload_stable

function void dcache_mem__access_base_sequence::check_c_payload_stable(
    input dcache_agent_agent_xaction expected_xact,
    input dcache_agent_agent_xaction observed_xact
);
    if (expected_xact == null || observed_xact == null) begin
        `uvm_fatal(get_type_name(), "C payload stability check received a null snapshot")
    end
    if (expected_xact.auto_inner_dcache_client_out_c_bits_opcode != observed_xact.auto_inner_dcache_client_out_c_bits_opcode ||
        expected_xact.auto_inner_dcache_client_out_c_bits_param != observed_xact.auto_inner_dcache_client_out_c_bits_param ||
        expected_xact.auto_inner_dcache_client_out_c_bits_size != observed_xact.auto_inner_dcache_client_out_c_bits_size ||
        expected_xact.auto_inner_dcache_client_out_c_bits_source != observed_xact.auto_inner_dcache_client_out_c_bits_source ||
        expected_xact.auto_inner_dcache_client_out_c_bits_address != observed_xact.auto_inner_dcache_client_out_c_bits_address ||
        expected_xact.auto_inner_dcache_client_out_c_bits_user_alias != observed_xact.auto_inner_dcache_client_out_c_bits_user_alias ||
        expected_xact.auto_inner_dcache_client_out_c_bits_user_vaddr != observed_xact.auto_inner_dcache_client_out_c_bits_user_vaddr ||
        expected_xact.auto_inner_dcache_client_out_c_bits_user_reqSource != observed_xact.auto_inner_dcache_client_out_c_bits_user_reqSource ||
        expected_xact.auto_inner_dcache_client_out_c_bits_user_needHint != observed_xact.auto_inner_dcache_client_out_c_bits_user_needHint ||
        expected_xact.auto_inner_dcache_client_out_c_bits_echo_isKeyword != observed_xact.auto_inner_dcache_client_out_c_bits_echo_isKeyword ||
        expected_xact.auto_inner_dcache_client_out_c_bits_data != observed_xact.auto_inner_dcache_client_out_c_bits_data ||
        expected_xact.auto_inner_dcache_client_out_c_bits_corrupt != observed_xact.auto_inner_dcache_client_out_c_bits_corrupt) begin
        `uvm_fatal(get_type_name(), "DCache C payload changed while valid was waiting for ready")
    end
endfunction:check_c_payload_stable

function bit [47:0] dcache_mem__access_base_sequence::line_addr64(input bit [47:0] addr);
    return {addr[47:6], 6'b0};
endfunction:line_addr64

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

function void dcache_mem__access_base_sequence::check_l2_model_cfg();
    void'(seq_csr_common::get_l2_rsp_delay_small_wt());
    void'(seq_csr_common::get_l2_rsp_delay_medium_wt());
    void'(seq_csr_common::get_l2_rsp_delay_large_wt());
    void'(seq_csr_common::get_l2_hint_valid_wt());
    void'(seq_csr_common::get_l2_probe_enable_wt());
endfunction:check_l2_model_cfg

function void dcache_mem__access_base_sequence::check_line_range(input bit [47:0] line_addr, input string ctx);
    mem_line_mask_t full_line_mask;

    full_line_mask       = '0;
    full_line_mask[63:0] = 64'hffff_ffff_ffff_ffff;
    if (!is_main_mem_access_in_range(line_addr, full_line_mask)) begin
        `uvm_fatal(get_type_name(), $sformatf("%s line_addr=0x%0h is outside configured main memory range", ctx, line_addr))
    end
endfunction:check_line_range

task dcache_mem__access_base_sequence::load_grant_line(
    input  bit [47:0]  line_addr,
    output bit [255:0] data_low,
    output bit [255:0] data_high
);
    bit low_corrupt;
    bit low_denied;
    bit high_corrupt;
    bit high_denied;

    dcache_mem_access_task(line_addr, 1'b0, 32'hffff_ffff, '0, low_corrupt, low_denied, data_low);
    dcache_mem_access_task(line_addr + 48'd32, 1'b0, 32'hffff_ffff, '0, high_corrupt, high_denied, data_high);
    if (low_corrupt || low_denied || high_corrupt || high_denied) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("GrantData line load failed for line_addr=0x%0h low(c=%0d d=%0d) high(c=%0d d=%0d)",
                             line_addr, low_corrupt, low_denied, high_corrupt, high_denied))
    end
endtask:load_grant_line

function void dcache_mem__access_base_sequence::record_cached_line(input bit [47:0] addr, input bit [1:0] line_alias);
    cached_alias_by_line[line_addr64(addr)] = line_alias;
endfunction:record_cached_line

function void dcache_mem__access_base_sequence::remove_cached_line(input bit [47:0] addr, input string reason);
    bit [47:0] key;

    key = line_addr64(addr);
    if (cached_alias_by_line.exists(key)) begin
        cached_alias_by_line.delete(key);
    end else begin
        `uvm_info(get_type_name(),
                  $sformatf("remove_cached_line miss for line=0x%0h reason=%s", key, reason),
                  UVM_DEBUG)
    end
endfunction:remove_cached_line

function int unsigned dcache_mem__access_base_sequence::sample_l2_response_delay();
    int unsigned delay_class;
    int unsigned delay_value;
    int unsigned small_wt;
    int unsigned medium_wt;
    int unsigned large_wt;

    small_wt  = seq_csr_common::get_l2_rsp_delay_small_wt();
    medium_wt = seq_csr_common::get_l2_rsp_delay_medium_wt();
    large_wt  = seq_csr_common::get_l2_rsp_delay_large_wt();

    if (!std::randomize(delay_class) with {
            delay_class dist {
                0 := small_wt,
                1 := medium_wt,
                2 := large_wt
            };
        }) begin
        `uvm_fatal(get_type_name(), "failed to randomize DCache L2 response delay class")
    end
    case (delay_class)
        0: begin
            if (!std::randomize(delay_value) with { delay_value inside {[3:5]}; }) begin
                `uvm_fatal(get_type_name(), "failed to randomize SMALL DCache L2 delay")
            end
        end
        1: begin
            if (!std::randomize(delay_value) with { delay_value inside {[6:15]}; }) begin
                `uvm_fatal(get_type_name(), "failed to randomize MEDIUM DCache L2 delay")
            end
        end
        2: begin
            if (!std::randomize(delay_value) with { delay_value inside {[16:50]}; }) begin
                `uvm_fatal(get_type_name(), "failed to randomize LARGE DCache L2 delay")
            end
        end
        default: begin
            `uvm_fatal(get_type_name(), $sformatf("unexpected delay_class=%0d", delay_class))
        end
    endcase
    return delay_value;
endfunction:sample_l2_response_delay

function bit dcache_mem__access_base_sequence::sample_hint_enable();
    int unsigned valid_wt;
    bit          enable;

    valid_wt = seq_csr_common::get_l2_hint_valid_wt();
    if (valid_wt == 0) begin
        return 1'b0;
    end
    if (valid_wt >= 100) begin
        return 1'b1;
    end
    if (!std::randomize(enable) with {
            enable dist {
                1'b1 := valid_wt,
                1'b0 := (100 - valid_wt)
            };
        }) begin
        `uvm_fatal(get_type_name(), "failed to randomize DCache hint enable")
    end
    return enable;
endfunction:sample_hint_enable

function bit dcache_mem__access_base_sequence::sample_probe_enable();
    int unsigned probe_wt;
    bit          enable;

    probe_wt = seq_csr_common::get_l2_probe_enable_wt();
    if (probe_wt == 0) begin
        return 1'b0;
    end
    if (probe_wt >= 100) begin
        return 1'b1;
    end
    if (!std::randomize(enable) with {
            enable dist {
                1'b1 := probe_wt,
                1'b0 := (100 - probe_wt)
            };
        }) begin
        `uvm_fatal(get_type_name(), "failed to randomize DCache probe enable")
    end
    return enable;
endfunction:sample_probe_enable

function bit dcache_mem__access_base_sequence::select_random_cached_line(output bit [47:0] line_addr, output bit [1:0] line_alias);
    mem_addr_t    key;
    int unsigned  entry_count;
    int unsigned  ordinal;

    line_addr  = '0;
    line_alias = '0;
    entry_count = cached_alias_by_line.num();
    if (entry_count == 0) begin
        return 1'b0;
    end
    if (!std::randomize(ordinal) with { ordinal inside {[0:entry_count-1]}; }) begin
        `uvm_fatal(get_type_name(), "failed to randomize cached-line probe ordinal")
    end
    if (!cached_alias_by_line.first(key)) begin
        return 1'b0;
    end
    for (int unsigned i = 0; i < ordinal; i++) begin
        if (!cached_alias_by_line.next(key)) begin
            `uvm_fatal(get_type_name(), $sformatf("cached_alias_by_line.next() failed at ordinal=%0d", ordinal))
        end
    end
    line_addr = key;
    line_alias = cached_alias_by_line[key];
    return 1'b1;
endfunction:select_random_cached_line

task dcache_mem__access_base_sequence::accept_dcache_a_request(
    input dcache_agent_agent_xaction req_xact,
    input longint unsigned           accept_cycle
);
    bit [47:0] line_addr;
    bit [255:0] line_data_low;
    bit [255:0] line_data_high;
    line_addr = line_addr64(req_xact.auto_inner_dcache_client_out_a_bits_address);
    clear_hint_state();

    if (req_xact.auto_inner_dcache_client_out_a_bits_size != TL_LINE_SIZE) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("DCache coherent A size must be 6, got %0d",
                             req_xact.auto_inner_dcache_client_out_a_bits_size))
    end
    if (req_xact.auto_inner_dcache_client_out_a_bits_address[5:0] != 6'b0) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("DCache coherent A addr must be 64B aligned, got 0x%0h",
                             req_xact.auto_inner_dcache_client_out_a_bits_address))
    end
    check_line_range(line_addr, "dcache coherent A");

    clear_pending_d_state();
    pending_d_due_cycle = accept_cycle + sample_l2_response_delay();
    pending_d_line_addr = line_addr;
    pending_d_size      = req_xact.auto_inner_dcache_client_out_a_bits_size;
    pending_d_source    = req_xact.auto_inner_dcache_client_out_a_bits_source;

    case (req_xact.auto_inner_dcache_client_out_a_bits_opcode)
        TL_A_OPCODE_ACQUIRE_BLOCK: begin
            if (req_xact.auto_inner_dcache_client_out_a_bits_source > 6'd15) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("AcquireBlock source must be within [0:15], got %0d",
                                     req_xact.auto_inner_dcache_client_out_a_bits_source))
            end
            case (req_xact.auto_inner_dcache_client_out_a_bits_param)
                TL_GROW_NTOB: pending_d_param = TL_CAP_TOB;
                TL_GROW_NTOT,
                TL_GROW_BTOT: pending_d_param = TL_CAP_TOT;
                default: begin
                    `uvm_fatal(get_type_name(),
                               $sformatf("AcquireBlock param=%0d is unsupported",
                                         req_xact.auto_inner_dcache_client_out_a_bits_param))
                end
            endcase
            load_grant_line(line_addr, line_data_low, line_data_high);
            pending_d_valid          = 1'b1;
            pending_d_kind           = DCACHE_PENDING_D_GRANT_DATA;
            pending_d_beat_count     = 2;
            pending_d_beat_idx       = 0;
            pending_d_sink           = TL_FIXED_SINK;
            pending_d_echo_isKeyword = req_xact.auto_inner_dcache_client_out_a_bits_echo_isKeyword;
            pending_d_alias          = req_xact.auto_inner_dcache_client_out_a_bits_user_alias;
            pending_d_data_low       = line_data_low;
            pending_d_data_high      = line_data_high;
            if (sample_hint_enable()) begin
                hint_selected  = 1'b1;
                hint_sent      = 1'b0;
                hint_source_id = req_xact.auto_inner_dcache_client_out_a_bits_source[3:0];
                hint_isKeyword = req_xact.auto_inner_dcache_client_out_a_bits_echo_isKeyword;
                if ((pending_d_due_cycle - accept_cycle) == 3) begin
                    hint_due_cycle = pending_d_due_cycle - 2;
                end else begin
                    hint_due_cycle = pending_d_due_cycle - 3;
                end
            end
        end
        TL_A_OPCODE_ACQUIRE_PERM: begin
            if (req_xact.auto_inner_dcache_client_out_a_bits_source > 6'd15) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("AcquirePerm source must be within [0:15], got %0d",
                                     req_xact.auto_inner_dcache_client_out_a_bits_source))
            end
            case (req_xact.auto_inner_dcache_client_out_a_bits_param)
                TL_GROW_NTOT,
                TL_GROW_BTOT: pending_d_param = TL_CAP_TOT;
                default: begin
                    `uvm_fatal(get_type_name(),
                               $sformatf("AcquirePerm param=%0d is unsupported",
                                         req_xact.auto_inner_dcache_client_out_a_bits_param))
                end
            endcase
            pending_d_valid      = 1'b1;
            pending_d_kind       = DCACHE_PENDING_D_GRANT;
            pending_d_beat_count = 1;
            pending_d_sink       = TL_FIXED_SINK;
            pending_d_alias      = req_xact.auto_inner_dcache_client_out_a_bits_user_alias;
        end
        TL_A_OPCODE_CBO_CLEAN,
        TL_A_OPCODE_CBO_FLUSH,
        TL_A_OPCODE_CBO_INVAL: begin
            if (req_xact.auto_inner_dcache_client_out_a_bits_source != TL_CBO_SOURCE) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("CBO source must be %0d, got %0d",
                                     TL_CBO_SOURCE,
                                     req_xact.auto_inner_dcache_client_out_a_bits_source))
            end
            pending_d_valid      = 1'b1;
            pending_d_kind       = DCACHE_PENDING_D_CBO_ACK;
            pending_d_beat_count = 1;
            pending_d_cbo_opcode = req_xact.auto_inner_dcache_client_out_a_bits_opcode;
        end
        default: begin
            `uvm_fatal(get_type_name(),
                       $sformatf("unsupported DCache coherent A opcode=%0d on dcache responder",
                                 req_xact.auto_inner_dcache_client_out_a_bits_opcode))
        end
    endcase
endtask:accept_dcache_a_request

function void dcache_mem__access_base_sequence::build_pending_d_xaction(inout dcache_agent_agent_xaction cycle_xact);
    bit [255:0] grant_data;

    if (!pending_d_valid) begin
        return;
    end
    cycle_xact.auto_inner_dcache_client_out_d_valid            = 1'b1;
    cycle_xact.auto_inner_dcache_client_out_d_bits_param       = pending_d_param;
    cycle_xact.auto_inner_dcache_client_out_d_bits_size        = pending_d_size;
    cycle_xact.auto_inner_dcache_client_out_d_bits_source      = pending_d_source;
    cycle_xact.auto_inner_dcache_client_out_d_bits_sink        = pending_d_sink;
    cycle_xact.auto_inner_dcache_client_out_d_bits_denied      = pending_d_denied;
    cycle_xact.auto_inner_dcache_client_out_d_bits_echo_isKeyword = pending_d_echo_isKeyword;
    cycle_xact.auto_inner_dcache_client_out_d_bits_corrupt     = pending_d_corrupt;
    cycle_xact.auto_inner_dcache_client_out_d_bits_data        = '0;

    case (pending_d_kind)
        DCACHE_PENDING_D_GRANT: begin
            cycle_xact.auto_inner_dcache_client_out_d_bits_opcode = TL_D_OPCODE_GRANT;
        end
        DCACHE_PENDING_D_GRANT_DATA: begin
            cycle_xact.auto_inner_dcache_client_out_d_bits_opcode = TL_D_OPCODE_GRANT_DATA;
            if (pending_d_beat_idx == 0) begin
                grant_data = pending_d_echo_isKeyword ? pending_d_data_high : pending_d_data_low;
            end else begin
                grant_data = pending_d_echo_isKeyword ? pending_d_data_low : pending_d_data_high;
            end
            cycle_xact.auto_inner_dcache_client_out_d_bits_data = grant_data;
        end
        DCACHE_PENDING_D_CBO_ACK: begin
            cycle_xact.auto_inner_dcache_client_out_d_bits_opcode = TL_D_OPCODE_CBO_ACK;
        end
        DCACHE_PENDING_D_RELEASE_ACK: begin
            cycle_xact.auto_inner_dcache_client_out_d_bits_opcode = TL_D_OPCODE_RELEASE_ACK;
        end
        default: begin
            `uvm_fatal(get_type_name(), $sformatf("unexpected pending_d_kind=%0d", pending_d_kind))
        end
    endcase
endfunction:build_pending_d_xaction

function void dcache_mem__access_base_sequence::process_d_fire();
    case (pending_d_kind)
        DCACHE_PENDING_D_GRANT_DATA: begin
            if ((pending_d_beat_idx + 1) < pending_d_beat_count) begin
                pending_d_beat_idx++;
            end else begin
                waiting_grant_ack         = 1'b1;
                pending_grant_line        = pending_d_line_addr;
                pending_grant_alias       = pending_d_alias;
                pending_grant_expected_sink = pending_d_sink;
                clear_pending_d_state();
                clear_hint_state();
            end
        end
        DCACHE_PENDING_D_GRANT: begin
            waiting_grant_ack         = 1'b1;
            pending_grant_line        = pending_d_line_addr;
            pending_grant_alias       = pending_d_alias;
            pending_grant_expected_sink = pending_d_sink;
            clear_pending_d_state();
            clear_hint_state();
        end
        DCACHE_PENDING_D_CBO_ACK: begin
            if (pending_d_cbo_opcode == TL_A_OPCODE_CBO_FLUSH) begin
                remove_cached_line(pending_d_line_addr, "cbo_flush");
            end else if (pending_d_cbo_opcode == TL_A_OPCODE_CBO_INVAL) begin
                remove_cached_line(pending_d_line_addr, "cbo_inval");
            end
            clear_pending_d_state();
            clear_hint_state();
        end
        DCACHE_PENDING_D_RELEASE_ACK: begin
            clear_pending_d_state();
            clear_hint_state();
        end
        default: begin
            `uvm_fatal(get_type_name(), $sformatf("process_d_fire with invalid kind=%0d", pending_d_kind))
        end
    endcase
endfunction:process_d_fire

function void dcache_mem__access_base_sequence::process_e_fire();
    if (!waiting_grant_ack) begin
        `uvm_fatal(get_type_name(), "unexpected E.valid when no GrantAck is pending")
    end
    if (dcache_vif.drv_cb.auto_inner_dcache_client_out_e_bits_sink !== pending_grant_expected_sink) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("GrantAck sink mismatch expected=%0d got=%0d",
                             pending_grant_expected_sink,
                             dcache_vif.drv_cb.auto_inner_dcache_client_out_e_bits_sink))
    end
    record_cached_line(pending_grant_line, pending_grant_alias);
    waiting_grant_ack          = 1'b0;
    pending_grant_line         = '0;
    pending_grant_alias        = '0;
    pending_grant_expected_sink = '0;
endfunction:process_e_fire

task dcache_mem__access_base_sequence::complete_probe_c_assembly(input longint unsigned complete_cycle);
    bit corrupt;
    bit denied;
    bit [255:0] load_data_unused;

    if (!c_assembly_corrupt_seen) begin
        dcache_mem_access_task(c_assembly_line, 1'b1, 32'hffff_ffff, c_assembly_data[255:0], corrupt, denied, load_data_unused);
        if (corrupt || denied) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("ProbeAckData store low beat failed line=0x%0h corrupt=%0d denied=%0d",
                                 c_assembly_line, corrupt, denied))
        end
        dcache_mem_access_task(c_assembly_line + 48'd32, 1'b1, 32'hffff_ffff, c_assembly_data[511:256], corrupt, denied, load_data_unused);
        if (corrupt || denied) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("ProbeAckData store high beat failed line=0x%0h corrupt=%0d denied=%0d",
                                 c_assembly_line, corrupt, denied))
        end
    end
    remove_cached_line(c_assembly_line, "probe_toN");
    waiting_probe_c = 1'b0;
    clear_c_assembly_state();
endtask:complete_probe_c_assembly

task dcache_mem__access_base_sequence::complete_release_c_assembly(input longint unsigned complete_cycle);
    bit corrupt;
    bit denied;
    bit [255:0] load_data_unused;

    if (!c_assembly_corrupt_seen) begin
        dcache_mem_access_task(c_assembly_line, 1'b1, 32'hffff_ffff, c_assembly_data[255:0], corrupt, denied, load_data_unused);
        if (corrupt || denied) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("ReleaseData store low beat failed line=0x%0h corrupt=%0d denied=%0d",
                                 c_assembly_line, corrupt, denied))
        end
        dcache_mem_access_task(c_assembly_line + 48'd32, 1'b1, 32'hffff_ffff, c_assembly_data[511:256], corrupt, denied, load_data_unused);
        if (corrupt || denied) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("ReleaseData store high beat failed line=0x%0h corrupt=%0d denied=%0d",
                                 c_assembly_line, corrupt, denied))
        end
    end
    remove_cached_line(c_assembly_line, "release_or_writeback");
    clear_pending_d_state();
    pending_d_valid      = 1'b1;
    pending_d_kind       = DCACHE_PENDING_D_RELEASE_ACK;
    pending_d_due_cycle  = complete_cycle + sample_l2_response_delay();
    pending_d_beat_count = 1;
    pending_d_param      = '0;
    pending_d_size       = c_assembly_size;
    pending_d_source     = c_assembly_source;
    pending_d_sink       = '0;
    pending_d_line_addr  = c_assembly_line;
    clear_c_assembly_state();
endtask:complete_release_c_assembly

task dcache_mem__access_base_sequence::consume_c_beat(
    input dcache_agent_agent_xaction c_req_xact,
    input longint unsigned           accept_cycle
);
    if (c_assembly_owner == DCACHE_C_OWNER_NONE) begin
        `uvm_fatal(get_type_name(), "consume_c_beat called without active C assembly")
    end
    if (c_req_xact.auto_inner_dcache_client_out_c_bits_opcode != c_assembly_opcode) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("C multi-beat opcode changed old=%0d new=%0d",
                             c_assembly_opcode,
                             c_req_xact.auto_inner_dcache_client_out_c_bits_opcode))
    end
    if (c_req_xact.auto_inner_dcache_client_out_c_bits_address != c_assembly_line) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("C multi-beat address changed old=0x%0h new=0x%0h",
                             c_assembly_line,
                             c_req_xact.auto_inner_dcache_client_out_c_bits_address))
    end
    if (c_req_xact.auto_inner_dcache_client_out_c_bits_source != c_assembly_source ||
        c_req_xact.auto_inner_dcache_client_out_c_bits_size   != c_assembly_size ||
        c_req_xact.auto_inner_dcache_client_out_c_bits_param  != c_assembly_param) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("C multi-beat fields changed source/size/param old=%0d/%0d/%0d new=%0d/%0d/%0d",
                             c_assembly_source, c_assembly_size, c_assembly_param,
                             c_req_xact.auto_inner_dcache_client_out_c_bits_source,
                             c_req_xact.auto_inner_dcache_client_out_c_bits_size,
                             c_req_xact.auto_inner_dcache_client_out_c_bits_param))
    end
    if (c_assembly_received_beats >= 2) begin
        `uvm_fatal(get_type_name(), "C multi-beat assembly received more than 2 beats")
    end
    if (c_assembly_received_beats == 0) begin
        c_assembly_data[255:0] = c_req_xact.auto_inner_dcache_client_out_c_bits_data;
    end else begin
        c_assembly_data[511:256] = c_req_xact.auto_inner_dcache_client_out_c_bits_data;
    end
    c_assembly_corrupt_seen |= c_req_xact.auto_inner_dcache_client_out_c_bits_corrupt;
    c_assembly_received_beats++;

    if (c_assembly_received_beats == 2) begin
        case (c_assembly_owner)
            DCACHE_C_OWNER_PROBE: begin
                complete_probe_c_assembly(accept_cycle);
            end
            DCACHE_C_OWNER_RELEASE: begin
                complete_release_c_assembly(accept_cycle);
            end
            default: begin
                `uvm_fatal(get_type_name(), $sformatf("unexpected c_assembly_owner=%0d", c_assembly_owner))
            end
        endcase
    end
endtask:consume_c_beat

task dcache_mem__access_base_sequence::start_c_assembly(
    input dcache_agent_agent_xaction c_req_xact,
    input longint unsigned           accept_cycle
);
    bit [47:0] line_addr;

    line_addr = line_addr64(c_req_xact.auto_inner_dcache_client_out_c_bits_address);

    case (c_req_xact.auto_inner_dcache_client_out_c_bits_opcode)
        TL_C_OPCODE_PROBE_ACK: begin
            if (!waiting_probe_c) begin
                `uvm_fatal(get_type_name(), "ProbeAck arrived without a pending Probe owner")
            end
            if (line_addr != pending_probe_line) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("ProbeAck line mismatch expected=0x%0h got=0x%0h",
                                     pending_probe_line, line_addr))
            end
            if (c_req_xact.auto_inner_dcache_client_out_c_bits_size != TL_LINE_SIZE) begin
                `uvm_fatal(get_type_name(), "ProbeAck size must be 6")
            end
            if (!(c_req_xact.auto_inner_dcache_client_out_c_bits_param inside {TL_REPORT_TTON, TL_REPORT_BTON, TL_REPORT_NTON})) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("ProbeAck param=%0d is unsupported",
                                     c_req_xact.auto_inner_dcache_client_out_c_bits_param))
            end
            remove_cached_line(line_addr, "probe_toN");
            waiting_probe_c = 1'b0;
        end
        TL_C_OPCODE_PROBE_ACKDATA: begin
            if (!waiting_probe_c) begin
                `uvm_fatal(get_type_name(), "ProbeAckData arrived without a pending Probe owner")
            end
            if (line_addr != pending_probe_line) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("ProbeAckData line mismatch expected=0x%0h got=0x%0h",
                                     pending_probe_line, line_addr))
            end
            if (c_req_xact.auto_inner_dcache_client_out_c_bits_size != TL_LINE_SIZE) begin
                `uvm_fatal(get_type_name(), "ProbeAckData size must be 6")
            end
            if (!(c_req_xact.auto_inner_dcache_client_out_c_bits_param inside {TL_REPORT_TTON, TL_REPORT_BTON, TL_REPORT_NTON})) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("ProbeAckData param=%0d is unsupported",
                                     c_req_xact.auto_inner_dcache_client_out_c_bits_param))
            end
            clear_c_assembly_state();
            c_assembly_owner          = DCACHE_C_OWNER_PROBE;
            c_assembly_opcode         = c_req_xact.auto_inner_dcache_client_out_c_bits_opcode;
            c_assembly_line           = line_addr;
            c_assembly_source         = c_req_xact.auto_inner_dcache_client_out_c_bits_source;
            c_assembly_size           = c_req_xact.auto_inner_dcache_client_out_c_bits_size;
            c_assembly_param          = c_req_xact.auto_inner_dcache_client_out_c_bits_param;
            consume_c_beat(c_req_xact, accept_cycle);
        end
        TL_C_OPCODE_RELEASE: begin
            if (c_req_xact.auto_inner_dcache_client_out_c_bits_size != TL_LINE_SIZE) begin
                `uvm_fatal(get_type_name(), "Release size must be 6")
            end
            if (c_req_xact.auto_inner_dcache_client_out_c_bits_address[5:0] != 6'b0) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("Release addr must be 64B aligned, got 0x%0h",
                                     c_req_xact.auto_inner_dcache_client_out_c_bits_address))
            end
            check_line_range(line_addr, "Release");
            remove_cached_line(line_addr, "release_or_writeback");
            clear_pending_d_state();
            pending_d_valid      = 1'b1;
            pending_d_kind       = DCACHE_PENDING_D_RELEASE_ACK;
            pending_d_due_cycle  = accept_cycle + sample_l2_response_delay();
            pending_d_beat_count = 1;
            pending_d_param      = '0;
            pending_d_size       = c_req_xact.auto_inner_dcache_client_out_c_bits_size;
            pending_d_source     = c_req_xact.auto_inner_dcache_client_out_c_bits_source;
            pending_d_sink       = '0;
            pending_d_line_addr  = line_addr;
        end
        TL_C_OPCODE_RELEASEDATA: begin
            if (c_req_xact.auto_inner_dcache_client_out_c_bits_size != TL_LINE_SIZE) begin
                `uvm_fatal(get_type_name(), "ReleaseData size must be 6")
            end
            if (c_req_xact.auto_inner_dcache_client_out_c_bits_address[5:0] != 6'b0) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("ReleaseData addr must be 64B aligned, got 0x%0h",
                                     c_req_xact.auto_inner_dcache_client_out_c_bits_address))
            end
            check_line_range(line_addr, "ReleaseData");
            clear_c_assembly_state();
            c_assembly_owner          = DCACHE_C_OWNER_RELEASE;
            c_assembly_opcode         = c_req_xact.auto_inner_dcache_client_out_c_bits_opcode;
            c_assembly_line           = line_addr;
            c_assembly_source         = c_req_xact.auto_inner_dcache_client_out_c_bits_source;
            c_assembly_size           = c_req_xact.auto_inner_dcache_client_out_c_bits_size;
            c_assembly_param          = c_req_xact.auto_inner_dcache_client_out_c_bits_param;
            consume_c_beat(c_req_xact, accept_cycle);
        end
        default: begin
            `uvm_fatal(get_type_name(),
                       $sformatf("unsupported DCache C opcode=%0d", c_req_xact.auto_inner_dcache_client_out_c_bits_opcode))
        end
    endcase
endtask:start_c_assembly

function void dcache_mem__access_base_sequence::try_start_probe(input bit allow_new_probe = 1'b1);
    bit [47:0] selected_line;
    bit [1:0]  selected_alias;

    // Probe 只能在完全空闲且未进入 stop drain 时新建；已有 B/C owner 由主循环继续消费。
    if (!allow_new_probe || pending_d_valid || waiting_grant_ack ||
        pending_probe_b_valid || waiting_probe_c ||
        (c_assembly_owner != DCACHE_C_OWNER_NONE) ||
        a_accept_armed || c_accept_armed) begin
        return;
    end
    if (!sample_probe_enable()) begin
        return;
    end
    if (!select_random_cached_line(selected_line, selected_alias)) begin
        return;
    end
    pending_probe_b_valid = 1'b1;
    pending_probe_line    = selected_line;
    pending_probe_alias   = selected_alias;
endfunction:try_start_probe

function void dcache_mem__access_base_sequence::service_hint(
    input longint unsigned           current_cycle,
    inout dcache_agent_agent_xaction cycle_xact
);
    if (hint_selected && !hint_sent && current_cycle == hint_due_cycle) begin
        cycle_xact.io_l2_hint_valid          = 1'b1;
        cycle_xact.io_l2_hint_bits_sourceId  = hint_source_id;
        cycle_xact.io_l2_hint_bits_isKeyword = hint_isKeyword;
        hint_sent                            = 1'b1;
    end
endfunction:service_hint

task dcache_mem__access_base_sequence::body();
    dcache_agent_agent_xaction cycle_xact;
    dcache_agent_agent_xaction sampled_req_xact;
    dcache_agent_agent_xaction fired_a_req_xact;
    dcache_agent_agent_xaction fired_c_req_xact;
    common_data_transaction    data;
    logic                      sampled_a_valid_raw;
    logic                      sampled_b_ready_raw;
    logic                      sampled_c_valid_raw;
    logic                      sampled_d_ready_raw;
    logic                      sampled_e_valid_raw;
    bit                        sampled_a_valid;
    bit                        sampled_b_ready;
    bit                        sampled_c_valid;
    bit                        sampled_d_ready;
    bit                        sampled_e_valid;
    bit                        reset_active;
    bit                        a_fire;
    bit                        b_fire;
    bit                        c_fire;
    bit                        d_fire;
    bit                        e_fire;
    int unsigned               stop_wait_cycles;

    if (!uvm_config_db#(virtual dcache_agent_agent_interface)::get(null, get_full_name(), "vif", dcache_vif) &&
        !uvm_config_db#(virtual dcache_agent_agent_interface)::get(null, "uvm_test_top.env.u_dcache_agent_agent*", "vif", dcache_vif)) begin
        `uvm_fatal(get_type_name(), "dcache virtual interface is not set for memory access sequence")
    end
    data = common_data_transaction::get();
    if (data == null) begin
        `uvm_fatal(get_type_name(), "failed to get common_data_transaction for DCache responder")
    end

    memblock_sync_pkg::dcache_responder_done = 1'b0;

    seq_csr_common::init();
    // 中文注释：DCache A/C 地址是物理地址；把共享 PADDR 窗口绑定到本 sequence
    // 的主存 range，确保完整 64B line 的边界检查真正生效。该 range 只约束
    // DCache responder 的主存访问，不改变主表的虚拟地址生成策略。
    clear_main_mem_ranges();
    init_main_mem_range(mem_addr_t'(seq_csr_common::get_paddr_base()),
                        seq_csr_common::get_paddr_range());
    check_l2_model_cfg();
    service_cycle    = 0;
    last_drive_cycle = 0;
    stop_wait_cycles = 0;
    clear_runtime_state(1'b1);

    forever begin
        // 中文注释：上一轮 item 已在前一个 drv_cb 边界更新到 clocking output；
        // 当前边界先采样它的真实握手，再提交下一周期 item。
        @(dcache_vif.drv_cb);
        sampled_a_valid_raw = dcache_vif.drv_cb.auto_inner_dcache_client_out_a_valid;
        sampled_b_ready_raw = dcache_vif.drv_cb.auto_inner_dcache_client_out_b_ready;
        sampled_c_valid_raw = dcache_vif.drv_cb.auto_inner_dcache_client_out_c_valid;
        sampled_d_ready_raw = dcache_vif.drv_cb.auto_inner_dcache_client_out_d_ready;
        sampled_e_valid_raw = dcache_vif.drv_cb.auto_inner_dcache_client_out_e_valid;
        reset_active    = (dcache_vif.rst_n !== 1'b1) || (memblock_sync_pkg::reset_backend_done !== 1'b1);
        sampled_a_valid = (sampled_a_valid_raw === 1'b1);
        sampled_b_ready = (sampled_b_ready_raw === 1'b1);
        sampled_c_valid = (sampled_c_valid_raw === 1'b1);
        sampled_d_ready = (sampled_d_ready_raw === 1'b1);
        sampled_e_valid = (sampled_e_valid_raw === 1'b1);
        a_fire          = 1'b0;
        b_fire          = 1'b0;
        c_fire          = 1'b0;
        d_fire          = 1'b0;
        e_fire          = 1'b0;

        if (!reset_active &&
            ((sampled_a_valid_raw !== 1'b0 && sampled_a_valid_raw !== 1'b1) ||
             (sampled_b_ready_raw !== 1'b0 && sampled_b_ready_raw !== 1'b1) ||
             (sampled_c_valid_raw !== 1'b0 && sampled_c_valid_raw !== 1'b1) ||
             (sampled_d_ready_raw !== 1'b0 && sampled_d_ready_raw !== 1'b1) ||
             (sampled_e_valid_raw !== 1'b0 && sampled_e_valid_raw !== 1'b1))) begin
            `uvm_fatal(get_type_name(),
                       "DCache channel valid/ready sampled as X/Z outside reset")
        end

        build_dcache_idle_xaction(cycle_xact);

        if (reset_active) begin
            clear_runtime_state(1'b1);
            send_dcache_xaction(cycle_xact);
            last_drive_cycle = service_cycle;
            service_cycle++;
            continue;
        end

        if (last_cycle_valid && (last_cycle_xact != null)) begin
            a_fire = (last_cycle_xact.auto_inner_dcache_client_out_a_ready == 1'b1) && sampled_a_valid;
            b_fire = (last_cycle_xact.auto_inner_dcache_client_out_b_valid == 1'b1) && sampled_b_ready;
            c_fire = (last_cycle_xact.auto_inner_dcache_client_out_c_ready == 1'b1) && sampled_c_valid;
            d_fire = (last_cycle_xact.auto_inner_dcache_client_out_d_valid == 1'b1) && sampled_d_ready;
            e_fire = (last_cycle_xact.auto_inner_dcache_client_out_e_ready == 1'b1) && sampled_e_valid;

            if (d_fire) begin
                process_d_fire();
            end
            if (e_fire) begin
                process_e_fire();
            end
            if (sampled_e_valid && !e_fire && !waiting_grant_ack) begin
                `uvm_fatal(get_type_name(), "E.valid observed without a pending GrantAck owner")
            end

            if (c_fire) begin
                if (!c_accept_armed || (armed_c_req_xact == null)) begin
                    `uvm_fatal(get_type_name(), "C.fire observed without an armed C snapshot")
                end
                capture_dcache_c_xaction(fired_c_req_xact);
                check_c_payload_stable(armed_c_req_xact, fired_c_req_xact);
                if (c_assembly_owner == DCACHE_C_OWNER_NONE) begin
                    start_c_assembly(armed_c_req_xact, last_drive_cycle);
                end else begin
                    consume_c_beat(armed_c_req_xact, last_drive_cycle);
                end
                c_accept_armed = 1'b0;
                armed_c_req_xact = null;
            end else if (c_accept_armed && !sampled_c_valid) begin
                c_accept_armed = 1'b0;
                armed_c_req_xact = null;
            end

            if (b_fire) begin
                if (!pending_probe_b_valid) begin
                    `uvm_fatal(get_type_name(), "B.fire observed without a pending Probe launch")
                end
                pending_probe_b_valid = 1'b0;
                waiting_probe_c       = 1'b1;
            end

            if (a_fire) begin
                if (!a_accept_armed || (armed_a_req_xact == null)) begin
                    `uvm_fatal(get_type_name(), "A.fire observed without an armed A snapshot")
                end
                capture_dcache_a_xaction(fired_a_req_xact);
                check_a_payload_stable(armed_a_req_xact, fired_a_req_xact);
                accept_dcache_a_request(armed_a_req_xact, last_drive_cycle);
                a_accept_armed = 1'b0;
                armed_a_req_xact = null;
            end else if (a_accept_armed && !sampled_a_valid) begin
                a_accept_armed = 1'b0;
                armed_a_req_xact = null;
            end
        end

        // global stop 表示主表已经全部终态；只允许本拍已经通过上一 item
        // ready 形成的 A.fire 进入 drain。stop 后新出现、未握手的 A 请求没有
        // 合法 owner，直接报错，避免重新打开 A.ready 或永久等待。
        if (data.is_global_stop_requested() && sampled_a_valid && !a_fire) begin
            `uvm_fatal(get_type_name(),
                       "new DCache A.valid observed after global stop without a sampled fire")
        end

        // 中文注释：global stop 不能抢退有 inflight 的 L2 responder。
        // 只有 A/C/B/D/E、GrantAck、Probe 和 assembly 生命周期都自然归零后，才发最后一拍 safe idle 并退出；
        // 已完成 GrantAck 的 cached line map 是稳定历史状态，不属于 in-flight，也不阻塞退出。
        if (data.is_global_stop_requested() &&
            !pending_d_valid &&
            !waiting_grant_ack &&
            !pending_probe_b_valid &&
            !waiting_probe_c &&
            (c_assembly_owner == DCACHE_C_OWNER_NONE) &&
            !a_accept_armed &&
            !c_accept_armed &&
            !sampled_a_valid &&
            !sampled_c_valid) begin
            `uvm_info(get_type_name(),
                      $sformatf("DCache responder draining complete at service_cycle=%0d cached_lines=%0d",
                                service_cycle, cached_alias_by_line.num()),
                      UVM_LOW)
            send_dcache_xaction(cycle_xact);
            last_cycle_xact  = cycle_xact;
            last_cycle_valid = 1'b1;
            last_drive_cycle = service_cycle;
            service_cycle++;
            memblock_sync_pkg::dcache_responder_done = 1'b1;
            `uvm_info(get_type_name(), "DCache responder published terminal idle and stopped", UVM_LOW)
            break;
        end
        else if (data.is_global_stop_requested()) begin
            stop_wait_cycles++;
            if ((stop_wait_cycles % 1000) == 0) begin
                `uvm_warning(get_type_name(),
                             $sformatf("DCache responder still draining after global stop: cycles=%0d pending_d=%0d grant_ack=%0d probe_b=%0d probe_c=%0d c_owner=%0d a_armed=%0d c_armed=%0d a_valid=%0d c_valid=%0d",
                                       stop_wait_cycles,
                                       pending_d_valid,
                                       waiting_grant_ack,
                                       pending_probe_b_valid,
                                       waiting_probe_c,
                                       c_assembly_owner,
                                       a_accept_armed,
                                       c_accept_armed,
                                       sampled_a_valid,
                                       sampled_c_valid))
            end
        end
        else begin
            stop_wait_cycles = 0;
        end

        if (pending_d_valid && (service_cycle >= pending_d_due_cycle)) begin
            build_pending_d_xaction(cycle_xact);
        end
        else if (pending_d_valid) begin
            // delay count-down 期间保持 A/C/B backpressure，只等待 due cycle。
        end
        else if (waiting_grant_ack) begin
            // 只开放 E.ready；A/C/B 继续 blocked。
            cycle_xact.auto_inner_dcache_client_out_e_ready = 1'b1;
        end
        else if (pending_probe_b_valid) begin
            cycle_xact.auto_inner_dcache_client_out_b_valid        = 1'b1;
            cycle_xact.auto_inner_dcache_client_out_b_bits_opcode  = TL_B_OPCODE_PROBE;
            cycle_xact.auto_inner_dcache_client_out_b_bits_param   = TL_CAP_TON;
            cycle_xact.auto_inner_dcache_client_out_b_bits_size    = TL_LINE_SIZE;
            cycle_xact.auto_inner_dcache_client_out_b_bits_source  = 6'd0;
            cycle_xact.auto_inner_dcache_client_out_b_bits_address = pending_probe_line;
            cycle_xact.auto_inner_dcache_client_out_b_bits_mask    = 32'hffff_ffff;
            cycle_xact.auto_inner_dcache_client_out_b_bits_data    = '0;
            cycle_xact.auto_inner_dcache_client_out_b_bits_data[2:1] = pending_probe_alias;
            cycle_xact.auto_inner_dcache_client_out_b_bits_corrupt = 1'b0;
        end
        // 当前采样拍已经 fire 的 A/C payload 不能在状态更新后再次被当作新请求 arm。
        else if (!c_fire && (c_assembly_owner != DCACHE_C_OWNER_NONE)) begin
            if (sampled_c_valid) begin
                capture_dcache_c_xaction(sampled_req_xact);
                cycle_xact.auto_inner_dcache_client_out_c_ready = 1'b1;
                c_accept_armed = 1'b1;
                armed_c_req_xact = sampled_req_xact;
            end
        end
        else if (!c_fire && waiting_probe_c) begin
            if (sampled_c_valid) begin
                capture_dcache_c_xaction(sampled_req_xact);
                case (sampled_req_xact.auto_inner_dcache_client_out_c_bits_opcode)
                    TL_C_OPCODE_PROBE_ACK,
                    TL_C_OPCODE_PROBE_ACKDATA,
                    TL_C_OPCODE_RELEASE,
                    TL_C_OPCODE_RELEASEDATA: begin
                        cycle_xact.auto_inner_dcache_client_out_c_ready = 1'b1;
                        c_accept_armed = 1'b1;
                        armed_c_req_xact = sampled_req_xact;
                    end
                    default: begin
                        `uvm_fatal(get_type_name(),
                                   $sformatf("waiting_probe_c only accepts ProbeAck/Data or Release/Data, got opcode=%0d",
                                             sampled_req_xact.auto_inner_dcache_client_out_c_bits_opcode))
                    end
                endcase
            end
        end
        else if (!c_fire && sampled_c_valid) begin
            capture_dcache_c_xaction(sampled_req_xact);
            case (sampled_req_xact.auto_inner_dcache_client_out_c_bits_opcode)
                TL_C_OPCODE_RELEASE,
                TL_C_OPCODE_RELEASEDATA: begin
                    cycle_xact.auto_inner_dcache_client_out_c_ready = 1'b1;
                    c_accept_armed = 1'b1;
                    armed_c_req_xact = sampled_req_xact;
                end
                default: begin
                    `uvm_fatal(get_type_name(),
                               $sformatf("idle DCache C path only accepts Release/Data, got opcode=%0d",
                                         sampled_req_xact.auto_inner_dcache_client_out_c_bits_opcode))
                end
            endcase
        end
        else if (c_fire) begin
            // 本拍 C.fire 已完成或推进了 C owner；禁止同拍 arm A 或启动 Probe。
            // 下一拍由 C assembly/Probe owner 分支继续消费后续 beat。
        end
        else if (!a_fire && !data.is_global_stop_requested() && sampled_a_valid) begin
            capture_dcache_a_xaction(sampled_req_xact);
            case (sampled_req_xact.auto_inner_dcache_client_out_a_bits_opcode)
                TL_A_OPCODE_ACQUIRE_BLOCK,
                TL_A_OPCODE_ACQUIRE_PERM,
                TL_A_OPCODE_CBO_CLEAN,
                TL_A_OPCODE_CBO_FLUSH,
                TL_A_OPCODE_CBO_INVAL: begin
                    cycle_xact.auto_inner_dcache_client_out_a_ready = 1'b1;
                    a_accept_armed = 1'b1;
                    armed_a_req_xact = sampled_req_xact;
                end
                default: begin
                    `uvm_fatal(get_type_name(),
                               $sformatf("unsupported DCache coherent A opcode=%0d before accept",
                                         sampled_req_xact.auto_inner_dcache_client_out_a_bits_opcode))
                end
            endcase
        end
        else begin
            try_start_probe(!data.is_global_stop_requested());
            if (pending_probe_b_valid) begin
                cycle_xact.auto_inner_dcache_client_out_b_valid        = 1'b1;
                cycle_xact.auto_inner_dcache_client_out_b_bits_opcode  = TL_B_OPCODE_PROBE;
                cycle_xact.auto_inner_dcache_client_out_b_bits_param   = TL_CAP_TON;
                cycle_xact.auto_inner_dcache_client_out_b_bits_size    = TL_LINE_SIZE;
                cycle_xact.auto_inner_dcache_client_out_b_bits_source  = 6'd0;
                cycle_xact.auto_inner_dcache_client_out_b_bits_address = pending_probe_line;
                cycle_xact.auto_inner_dcache_client_out_b_bits_mask    = 32'hffff_ffff;
                cycle_xact.auto_inner_dcache_client_out_b_bits_data    = '0;
                cycle_xact.auto_inner_dcache_client_out_b_bits_data[2:1] = pending_probe_alias;
                cycle_xact.auto_inner_dcache_client_out_b_bits_corrupt = 1'b0;
            end
        end

        service_hint(service_cycle, cycle_xact);
        cycle_xact.io_l2_flush_done = 1'b0;
        send_dcache_xaction(cycle_xact);
        last_cycle_xact  = cycle_xact;
        last_cycle_valid = 1'b1;
        last_drive_cycle = service_cycle;
        service_cycle++;
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
    common_data_transaction data;

    if (!uvm_config_db#(virtual sbuffer_agent_agent_interface)::get(null, get_full_name(), "vif", sbuffer_vif) &&
        !uvm_config_db#(virtual sbuffer_agent_agent_interface)::get(null, "uvm_test_top.env.u_sbuffer_agent_agent*", "vif", sbuffer_vif)) begin
        `uvm_fatal(get_type_name(), "sbuffer virtual interface is not set for memory access sequence")
    end
    data = common_data_transaction::get();
    if (data == null) begin
        `uvm_fatal(get_type_name(), "failed to get common_data_transaction for SBuffer responder")
    end

    forever begin
        // 中文注释：SBuffer response 在本轮发送完成后才回到循环顶部；global stop 且没有
        // 尚未接受的 A 请求时发送安全 idle 并自然退出，不依赖 phase kill。
        if (data.is_global_stop_requested() &&
            sbuffer_vif.auto_inner_buffers_out_a_valid === 1'b0) begin
            build_sbuffer_idle_xaction(idle_xact);
            send_sbuffer_xaction(idle_xact);
            break;
        end

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
