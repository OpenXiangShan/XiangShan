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
    typedef bit [41:0]   dcache_line_addr_t;
    typedef bit [8191:0] mem_line_data_t;
    typedef bit [1023:0] mem_line_mask_t;

    typedef struct {
        mem_addr_t base;
        mem_addr_t limit;
    } mem_range_t;

    typedef enum bit [1:0] {
        // 无写入来源，仅供 load 路径调用 shared memory helper。
        SHARED_MEM_WRITE_NONE    = 2'd0,
        // DCache C-channel 的完整 ReleaseData/ProbeAckData 写回。
        SHARED_MEM_WRITE_DCACHE  = 2'd1,
        // Uncache A-channel 的真实 store fire。
        SHARED_MEM_WRITE_UNCACHE = 2'd2
    } shared_mem_write_owner_e;

    // 中文注释：L2 D-channel 错误账本按 64B physical line 保存。PENDING 只存在
    // 于 response 建立到最后一个 GrantData D.fire 之间；live map 只保存 NONE、
    // CORRUPT 或 DENIED。DENIED 语义上同时包含 corrupt，状态不支持运行期清除。
    typedef enum bit [1:0] {
        L2_D_ERROR_NONE    = 2'd0,
        L2_D_ERROR_PENDING = 2'd1,
        L2_D_ERROR_CORRUPT = 2'd2,
        L2_D_ERROR_DENIED  = 2'd3
    } l2_d_error_state_e;

    typedef struct packed {
        bit                  valid;
        l2_d_error_state_e   state;
        longint unsigned     generation;
        longint unsigned     corrupt_activation_sample;
        longint unsigned     denied_activation_sample;
        bit                  source_valid;
        bit [9:0]            source;
    } l2_d_error_line_record_t;

    typedef struct packed {
        bit                  valid;
        l2_d_error_state_e   proposed_state;
        longint unsigned     accept_sample;
        longint unsigned     generation;
        bit [9:0]            source;
    } l2_d_error_pending_record_t;

    typedef struct {
        mem_addr_t                addr;
        mem_line_mask_t           byte_mask;
        mem_line_data_t           store_data;
    } shared_mem_write_event_t;

    // 所有 memory-facing responder 共用 backing/overlay；DUT 写只进入 overlay。
    static mem_line_data_t          main_mem[mem_line_addr_t];
    static mem_line_data_t          write_overlay_mem[mem_line_addr_t];
    static mem_line_mask_t          write_overlay_byte_valid[mem_line_addr_t];
    // 中文注释：每条 overlay line 的 byte 级不可信标记。置位表示该 byte 的
    // DCache 数据型 C response 已被 observer 观察为 corrupt；只有既有正常提交
    // 事实覆盖到对应 byte 后才允许清除。该表只供只读 API/observer 使用，不参与 DUT 读写。
    static mem_line_mask_t          write_overlay_corrupt_byte_mask[mem_line_addr_t];
    static mem_range_t              main_mem_ranges[$];
    static bit                      main_mem_range_configured = 1'b0;
    // 中文注释：同一物理采样时刻的 memory-facing 写先进入两个来源队列。
    // 设置：DCache C data 或 Uncache store 完成真实握手后入队；清空：下一采样时刻首次访问
    // shared store 时按 DCache、Uncache 顺序提交，或 testcase 生命周期初始化时删除。
    // 作用：同拍读只能看到上一轮 committed overlay，且同 byte 冲突固定由 Uncache 覆盖 DCache。
    static shared_mem_write_event_t dcache_write_batch[$];
    static shared_mem_write_event_t uncache_write_batch[$];
    static bit                      shared_mem_sample_valid = 1'b0;
    static longint unsigned         shared_mem_sample_time = 0;
    // 中文注释：唯一 lifecycle owner 已完成本 testcase shared memory 清空和 range 配置。
    // 设置：real-smoke virtual sequence 在 fork responder 前完成初始化后置位；清零：下一次
    // initialize_shared_memory_state() 先清空旧状态。legacy default topology 仅在该位为 0 时兜底初始化。
    static bit                      shared_mem_lifecycle_initialized = 1'b0;

    // 中文注释：DCache aggregate 是给 RM API 的值型旁路快照，不是 DCache 私有 map 的
    // 第二份真源。resident/pending/assembly/corrupt 均由既有动作完成后的 observer 更新；
    // snapshot 一次性发布，避免查询读到字段混合的中间状态。
    typedef struct packed {
        bit                  published;
        bit                  owner_valid;
        longint unsigned     generation;
        longint unsigned     resident_line_count;
        longint unsigned     pending_writeback_count;
        longint unsigned     observed_corrupt_line_count;
        bit                  c_assembly_pending;
        bit                  observer_ready;
        bit                  dcache_drain_complete;
        bit                  dcache_overlay_read_ready;
        longint unsigned     drain_epoch;
        longint unsigned     drain_transition_sample;
        longint unsigned     drain_transition_time;
    } dcache_aggregate_snapshot_t;

    static dcache_aggregate_snapshot_t dcache_aggregate_snapshot;
    static bit                         dcache_owner_claimed = 1'b0;
    static longint unsigned            dcache_owner_generation = 0;
    // 中文注释：observer 只保存已入队/已提交 fragment 的 byte 覆盖事实。
    // 一条 64 B line 的低、高 32 B 均提交后才结束该 line 的未完成观察窗口；不创建 ticket。
    static bit [63:0]                  dcache_fragment_pending_bytes[dcache_line_addr_t];
    static bit [63:0]                  dcache_fragment_committed_bytes[dcache_line_addr_t];
    static longint unsigned            dcache_incomplete_fragment_line_count = 0;
    // 中文注释：DCache C-data corrupt 的权威旁路范围，以 64 B DCache line 为粒度
    // 保存仍不可比较的 byte。置位：corrupt ProbeAckData/ReleaseData 已收齐；清零：
    // 同 line 的完整 DCache writeback 或 Uncache 已提交 byte 覆盖。该 map 与 1KiB
    // overlay corrupt mask 并存，前者保证 aggregate 的 corrupt line 计数不会因同一
    // 1KiB backing line 内有多条 64 B line 而漂移。
    static bit [63:0]                  dcache_corrupt_byte_mask_by_line[dcache_line_addr_t];

    // 中文注释：sticky ledger 与 DCache alias、C-channel corrupt observer、shared
    // memory overlay 完全隔离。它只在 testcase/shared-memory 初始化时清空，DCache
    // runtime reset、CBO、Probe、Release 和 Uncache 都不得修改这两张表。
    static l2_d_error_line_record_t    l2_d_error_live_by_line[dcache_line_addr_t];
    static l2_d_error_pending_record_t l2_d_error_pending_by_line[dcache_line_addr_t];
    static longint unsigned             l2_d_error_next_generation = 1;
    static bit                          l2_d_error_ledger_ready = 1'b0;
    static bit                          l2_d_error_sticky_enabled = 1'b0;

    `uvm_object_utils(mem_access_base_sequence)

    extern function new(string name = "mem_access_base_sequence");
    extern static function void init_main_mem_range(input mem_addr_t base, input longint unsigned capacity);
    extern static function void clear_main_mem_ranges();
    extern static function bit is_main_mem_access_in_range(input mem_addr_t addr, input mem_line_mask_t byte_mask);
    extern virtual function void paddr_to_error(input mem_addr_t addr, output bit corrupt, output bit denied);
    extern function bit sample_d_error_enable(
        input int unsigned weight,
        input string       error_name
    );
    extern static function mem_line_data_t build_lazy_line(input mem_line_addr_t line_addr);
    extern static function void ensure_main_line(input mem_line_addr_t line_addr);
    extern static function void ensure_write_overlay_line(input mem_line_addr_t line_addr);
    extern static function void clear_shared_memory_state();
    extern static function void initialize_shared_memory_state(
        input bit              ranges_en,
        input mem_addr_t       base,
        input longint unsigned capacity
    );
    extern static function bit is_shared_memory_lifecycle_initialized();
    extern static function void report_l2_d_error_fatal(input string message);
    extern static function void reset_l2_d_error_ledger(input bit sticky_enabled);
    extern static function bit prepare_l2_d_error_for_grant(
        input  mem_addr_t            line_addr,
        input  longint unsigned      accept_sample,
        input  bit [9:0]             source,
        input  bit                   candidate_valid,
        input  bit                   candidate_denied,
        input  bit                   candidate_corrupt,
        output bit                   denied,
        output bit                   corrupt,
        output bit                   needs_candidate,
        output longint unsigned      reservation_generation
    );
    extern static function void commit_l2_d_error_on_grant_d_fire(
        input mem_addr_t        line_addr,
        input bit               denied,
        input bit               corrupt,
        input longint unsigned  sample,
        input bit [9:0]         source,
        input longint unsigned  reservation_generation
    );
    extern static function bit query_l2_d_error_at_sample(
        input  mem_addr_t            line_addr,
        input  longint unsigned      sample,
        output bit                   sticky_enabled,
        output l2_d_error_line_record_t record
    );
    extern static function void clear_dcache_observer_state();
    extern static function bit claim_dcache_observer_owner();
    extern static function void release_dcache_observer_owner();
    extern static function void publish_dcache_aggregate_snapshot();
    extern static function bit peek_dcache_aggregate_snapshot(
        output dcache_aggregate_snapshot_t snapshot
    );
    extern static function void publish_dcache_owner_baseline();
    extern static function void invalidate_dcache_runtime_observer();
    extern static function void observe_dcache_line_transition(
        input bit old_alias_valid,
        input bit new_alias_valid
    );
    extern static function void observe_dcache_map_cleared();
    extern static function void observe_dcache_c_assembly_start();
    extern static function void observe_dcache_c_assembly_complete();
    extern static function void observe_dcache_corrupt_line(input mem_addr_t line_addr);
    extern static function void observe_dcache_write_enqueued(
        input shared_mem_write_event_t write_event
    );
    extern static function void observe_dcache_write_committed(
        input shared_mem_write_event_t write_event
    );
    extern static function void observe_uncache_write_committed(
        input shared_mem_write_event_t write_event
    );
    extern static function void apply_shared_mem_write(input shared_mem_write_event_t write_event);
    extern static function void commit_shared_mem_write_batch();
    extern static function void begin_shared_mem_sample(input longint unsigned sample_time);
    extern virtual task main_mem_access_task(
        input  mem_addr_t       addr,
        input  bit              is_store,
        input  mem_line_mask_t  byte_mask,
        input  mem_line_data_t  store_data,
        output bit              corrupt,
        output bit              denied,
        output mem_line_data_t  load_data
    );
    extern virtual task shared_mem_access_task(
        input  mem_addr_t       addr,
        input  bit              is_store,
        input  mem_line_mask_t  byte_mask,
        input  mem_line_data_t  store_data,
        output bit              corrupt,
        output bit              denied,
        output mem_line_data_t  load_data,
        input  shared_mem_write_owner_e write_owner
    );

endclass:mem_access_base_sequence

function mem_access_base_sequence::new(string name = "mem_access_base_sequence");
    super.new(name);
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

function bit mem_access_base_sequence::sample_d_error_enable(
    input int unsigned weight,
    input string       error_name
);
    bit enable;

    if (weight > 100) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("%s weight=%0d must be within [0:100]", error_name, weight))
    end
    if (weight == 0) begin
        return 1'b0;
    end
    if (weight == 100) begin
        return 1'b1;
    end
    if (!std::randomize(enable) with {
            enable dist {
                1'b1 := weight,
                1'b0 := (100 - weight)
            };
        }) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("failed to randomize %s with weight=%0d", error_name, weight))
    end
    return enable;
endfunction:sample_d_error_enable

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

function void mem_access_base_sequence::ensure_write_overlay_line(input mem_line_addr_t line_addr);
    if (!write_overlay_mem.exists(line_addr)) begin
        write_overlay_mem[line_addr]            = '0;
        write_overlay_byte_valid[line_addr]     = '0;
    end
endfunction:ensure_write_overlay_line

function void mem_access_base_sequence::clear_shared_memory_state();
    main_mem.delete();
    write_overlay_mem.delete();
    write_overlay_byte_valid.delete();
    write_overlay_corrupt_byte_mask.delete();
    dcache_corrupt_byte_mask_by_line.delete();
    l2_d_error_live_by_line.delete();
    l2_d_error_pending_by_line.delete();
    l2_d_error_next_generation  = 1;
    l2_d_error_ledger_ready     = 1'b0;
    l2_d_error_sticky_enabled   = 1'b0;
    dcache_write_batch.delete();
    uncache_write_batch.delete();
    shared_mem_sample_valid          = 1'b0;
    shared_mem_sample_time           = 0;
    shared_mem_lifecycle_initialized = 1'b0;
    clear_main_mem_ranges();
    clear_dcache_observer_state();
endfunction:clear_shared_memory_state

function void mem_access_base_sequence::initialize_shared_memory_state(
    input bit              ranges_en,
    input mem_addr_t       base,
    input longint unsigned capacity
);
    clear_shared_memory_state();
    if (ranges_en) begin
        init_main_mem_range(base, capacity);
    end
    reset_l2_d_error_ledger(seq_csr_common::get_l2_d_error_sticky_en());
    shared_mem_lifecycle_initialized = 1'b1;
endfunction:initialize_shared_memory_state

function bit mem_access_base_sequence::is_shared_memory_lifecycle_initialized();
    return shared_mem_lifecycle_initialized;
endfunction:is_shared_memory_lifecycle_initialized

// 静态账本 helper 没有 sequence 实例，必须使用 UVM package 级报告函数，不能展开
// 会隐式调用 this.uvm_report_fatal() 的 `uvm_fatal 宏。
function void mem_access_base_sequence::report_l2_d_error_fatal(input string message);
    uvm_pkg::uvm_report_fatal("L2_D_ERROR_LEDGER", message, UVM_NONE);
endfunction:report_l2_d_error_fatal

// 抽象职责：在 testcase/shared-memory 初始化边界建立空的 sticky ledger。该函数
// 只负责状态初始化，不读取 DCache runtime map，也不在 CBO/Probe/reset service 中调用。
function void mem_access_base_sequence::reset_l2_d_error_ledger(input bit sticky_enabled);
    l2_d_error_live_by_line.delete();
    l2_d_error_pending_by_line.delete();
    l2_d_error_next_generation = 1;
    l2_d_error_sticky_enabled  = sticky_enabled;
    l2_d_error_ledger_ready    = 1'b1;
endfunction:reset_l2_d_error_ledger

// 抽象职责：在 GrantData response 创建点解析同一 physical line 的已有 live/pending
// 错误，或者为调用方声明需要一次新的随机候选。调用方仅在 needs_candidate=1 时采样
// 权重并以 candidate_valid=1 再次调用，因此同线 sticky 命中不会额外消耗随机数。
function bit mem_access_base_sequence::prepare_l2_d_error_for_grant(
    input  mem_addr_t            line_addr,
    input  longint unsigned      accept_sample,
    input  bit [9:0]             source,
    input  bit                   candidate_valid,
    input  bit                   candidate_denied,
    input  bit                   candidate_corrupt,
    output bit                   denied,
    output bit                   corrupt,
    output bit                   needs_candidate,
    output longint unsigned      reservation_generation
);
    dcache_line_addr_t          line_key;
    l2_d_error_line_record_t    live_record;
    l2_d_error_pending_record_t pending_record;

    denied                 = 1'b0;
    corrupt                = 1'b0;
    needs_candidate        = 1'b0;
    reservation_generation = 0;
    if (!l2_d_error_ledger_ready || !l2_d_error_sticky_enabled) begin
        report_l2_d_error_fatal(
            "sticky GrantData preparation requires an initialized enabled ledger");
        return 1'b0;
    end
    line_key = line_addr[47:6];
    if (l2_d_error_live_by_line.exists(line_key)) begin
        live_record = l2_d_error_live_by_line[line_key];
        if (!live_record.valid ||
            (live_record.state != L2_D_ERROR_CORRUPT &&
             live_record.state != L2_D_ERROR_DENIED)) begin
            report_l2_d_error_fatal(
                $sformatf("invalid live L2 D error record line=0x%0h state=%0d valid=%0d",
                          line_addr, live_record.state, live_record.valid));
            return 1'b0;
        end
        denied                 = live_record.state == L2_D_ERROR_DENIED;
        corrupt                = 1'b1;
        reservation_generation = live_record.generation;
        return 1'b1;
    end
    if (l2_d_error_pending_by_line.exists(line_key)) begin
        pending_record = l2_d_error_pending_by_line[line_key];
        if (!pending_record.valid ||
            (pending_record.proposed_state != L2_D_ERROR_CORRUPT &&
             pending_record.proposed_state != L2_D_ERROR_DENIED)) begin
            report_l2_d_error_fatal(
                $sformatf("invalid pending L2 D error record line=0x%0h state=%0d valid=%0d",
                          line_addr,
                          pending_record.proposed_state,
                          pending_record.valid));
            return 1'b0;
        end
        denied                 = pending_record.proposed_state == L2_D_ERROR_DENIED;
        corrupt                = 1'b1;
        reservation_generation = pending_record.generation;
        return 1'b1;
    end
    if (!candidate_valid) begin
        needs_candidate = 1'b1;
        return 1'b1;
    end
    if (candidate_denied && !candidate_corrupt) begin
        report_l2_d_error_fatal(
            $sformatf("GrantData denied requires corrupt line=0x%0h source=%0d",
                      line_addr, source));
        return 1'b0;
    end
    denied  = candidate_denied;
    corrupt = candidate_corrupt;
    if (!corrupt) begin
        return 1'b1;
    end
    if (l2_d_error_next_generation == 0) begin
        report_l2_d_error_fatal("L2 D error generation wrapped to zero");
        return 1'b0;
    end
    pending_record = '{default:'0};
    pending_record.valid          = 1'b1;
    pending_record.proposed_state = denied ? L2_D_ERROR_DENIED : L2_D_ERROR_CORRUPT;
    pending_record.accept_sample  = accept_sample;
    pending_record.generation     = l2_d_error_next_generation;
    pending_record.source         = source;
    l2_d_error_next_generation++;
    l2_d_error_pending_by_line[line_key] = pending_record;
    reservation_generation = pending_record.generation;
    return 1'b1;
endfunction:prepare_l2_d_error_for_grant

// 抽象职责：在最后一个 GrantData D.fire 后把 response 固定的错误结果提交到
// live ledger。它只进行单 line 关联数组访问；早于该时刻的 RM 查询绝不会看到 pending。
function void mem_access_base_sequence::commit_l2_d_error_on_grant_d_fire(
    input mem_addr_t        line_addr,
    input bit               denied,
    input bit               corrupt,
    input longint unsigned  sample,
    input bit [9:0]         source,
    input longint unsigned  reservation_generation
);
    dcache_line_addr_t          line_key;
    l2_d_error_line_record_t    live_record;
    l2_d_error_pending_record_t pending_record;
    bit                         pending_valid;
    l2_d_error_state_e          proposed_state;
    longint unsigned            activation_generation;

    if (!l2_d_error_sticky_enabled) begin
        return;
    end
    if (!l2_d_error_ledger_ready) begin
        report_l2_d_error_fatal("GrantData D.fire observed before sticky ledger initialization");
    end
    if (denied && !corrupt) begin
        report_l2_d_error_fatal(
            $sformatf("GrantData denied without corrupt line=0x%0h source=%0d",
                      line_addr, source));
    end
    if (!corrupt) begin
        return;
    end
    line_key = line_addr[47:6];
    proposed_state = denied ? L2_D_ERROR_DENIED : L2_D_ERROR_CORRUPT;
    pending_valid = l2_d_error_pending_by_line.exists(line_key);
    pending_record = '{default:'0};
    if (pending_valid) begin
        pending_record = l2_d_error_pending_by_line[line_key];
        if (!pending_record.valid ||
            pending_record.proposed_state != proposed_state ||
            (reservation_generation != 0 &&
             pending_record.generation != reservation_generation)) begin
            report_l2_d_error_fatal(
                $sformatf("GrantData pending reservation mismatch line=0x%0h response_gen=%0d pending_gen=%0d response_state=%0d pending_state=%0d",
                          line_addr,
                          reservation_generation,
                          pending_record.generation,
                          proposed_state,
                          pending_record.proposed_state));
        end
    end
    if (!l2_d_error_live_by_line.exists(line_key)) begin
        if (pending_valid) begin
            activation_generation = pending_record.generation;
        end else begin
            if (reservation_generation != 0) begin
                report_l2_d_error_fatal(
                    $sformatf("GrantData response references missing reservation line=0x%0h generation=%0d",
                              line_addr, reservation_generation));
            end
            if (l2_d_error_next_generation == 0) begin
                report_l2_d_error_fatal("L2 D error generation wrapped to zero");
            end
            activation_generation = l2_d_error_next_generation;
            l2_d_error_next_generation++;
        end
        live_record = '{default:'0};
        live_record.valid                      = 1'b1;
        live_record.state                      = proposed_state;
        live_record.generation                 = activation_generation;
        live_record.corrupt_activation_sample  = sample;
        live_record.denied_activation_sample   = denied ? sample : 0;
        live_record.source_valid               = 1'b1;
        live_record.source                     = source;
        l2_d_error_live_by_line[line_key] = live_record;
    end else begin
        live_record = l2_d_error_live_by_line[line_key];
        if (!live_record.valid ||
            (live_record.state != L2_D_ERROR_CORRUPT &&
             live_record.state != L2_D_ERROR_DENIED)) begin
            report_l2_d_error_fatal(
                $sformatf("invalid live state on GrantData commit line=0x%0h state=%0d valid=%0d",
                          line_addr, live_record.state, live_record.valid));
        end
        if (live_record.state == L2_D_ERROR_CORRUPT &&
            proposed_state == L2_D_ERROR_DENIED) begin
            if (pending_valid) activation_generation = pending_record.generation;
            else begin
                if (l2_d_error_next_generation == 0) begin
                    report_l2_d_error_fatal("L2 D error generation wrapped to zero");
                end
                activation_generation = l2_d_error_next_generation;
                l2_d_error_next_generation++;
            end
            live_record.state                    = L2_D_ERROR_DENIED;
            live_record.generation               = activation_generation;
            live_record.denied_activation_sample = sample;
            l2_d_error_live_by_line[line_key] = live_record;
        end
    end
    if (pending_valid) begin
        l2_d_error_pending_by_line.delete(line_key);
    end
endfunction:commit_l2_d_error_on_grant_d_fire

// 抽象职责：为 RM 提供一条 physical line 在指定 sample 的不可变值型错误视图。
// 查询不创建 backing line、不消费 pending，也不会因后续 CBO/Probe 改变历史结果。
function bit mem_access_base_sequence::query_l2_d_error_at_sample(
    input  mem_addr_t            line_addr,
    input  longint unsigned      sample,
    output bit                   sticky_enabled,
    output l2_d_error_line_record_t record
);
    dcache_line_addr_t       line_key;
    l2_d_error_line_record_t live_record;

    record = '{default:'0};
    sticky_enabled = l2_d_error_sticky_enabled;
    if (!l2_d_error_ledger_ready) begin
        return 1'b0;
    end
    if (!l2_d_error_sticky_enabled) begin
        return 1'b1;
    end
    line_key = line_addr[47:6];
    if (!l2_d_error_live_by_line.exists(line_key)) begin
        return 1'b1;
    end
    live_record = l2_d_error_live_by_line[line_key];
    if (!live_record.valid) begin
        report_l2_d_error_fatal(
            $sformatf("invalid empty live record line=0x%0h", line_addr));
        return 1'b0;
    end
    if (live_record.state == L2_D_ERROR_DENIED &&
        live_record.denied_activation_sample <= sample) begin
        record = live_record;
        return 1'b1;
    end
    if (live_record.corrupt_activation_sample <= sample) begin
        record = live_record;
        record.state = L2_D_ERROR_CORRUPT;
        record.denied_activation_sample = 0;
        return 1'b1;
    end
    return 1'b1;
endfunction:query_l2_d_error_at_sample

function void mem_access_base_sequence::clear_dcache_observer_state();
    // 中文注释：生命周期初始化清除旧 owner 的旁路快照；不触碰 DCache sequence 私有 map。
    // runtime reset/owner 退出由 DCache responder 通过同一 helper 失效快照，RM 只能看到 invalid。
    dcache_aggregate_snapshot = '{default:'0};
    dcache_owner_claimed      = 1'b0;
    dcache_owner_generation   = 0;
    dcache_fragment_pending_bytes.delete();
    dcache_fragment_committed_bytes.delete();
    dcache_incomplete_fragment_line_count = 0;
endfunction:clear_dcache_observer_state

function bit mem_access_base_sequence::claim_dcache_observer_owner();
    // 中文注释：basicTest 只允许一个 DCache responder 发布公共 aggregate。重复 claim
    // 不清理、不覆盖旧状态，调用者可据返回值停止发布；shared memory 未初始化时不兜底初始化。
    if (!shared_mem_lifecycle_initialized) begin
        return 1'b0;
    end
    if (dcache_owner_claimed) begin
        return 1'b0;
    end
    dcache_owner_claimed    = 1'b1;
    dcache_owner_generation++;
    dcache_aggregate_snapshot = '{default:'0};
    dcache_aggregate_snapshot.owner_valid = 1'b1;
    dcache_aggregate_snapshot.generation  = dcache_owner_generation;
    dcache_aggregate_snapshot.observer_ready = 1'b1;
    dcache_aggregate_snapshot.published = 1'b0;
    return 1'b1;
endfunction:claim_dcache_observer_owner

function void mem_access_base_sequence::release_dcache_observer_owner();
    // 中文注释：owner 退出只使 DCache aggregate 不再可读；已经提交的 backing/overlay
    // 仍由 shared-memory lifecycle 管理，不能在这里清除或重建。
    dcache_aggregate_snapshot.published  = 1'b0;
    dcache_aggregate_snapshot.owner_valid = 1'b0;
    dcache_aggregate_snapshot.observer_ready = 1'b0;
    dcache_aggregate_snapshot.dcache_drain_complete = 1'b0;
    dcache_aggregate_snapshot.dcache_overlay_read_ready = 1'b0;
    dcache_owner_claimed = 1'b0;
endfunction:release_dcache_observer_owner

function void mem_access_base_sequence::publish_dcache_aggregate_snapshot();
    bit was_drain_complete;
    bit new_drain_complete;

    was_drain_complete = dcache_aggregate_snapshot.dcache_drain_complete;
    new_drain_complete =
        dcache_aggregate_snapshot.owner_valid &&
        dcache_aggregate_snapshot.observer_ready &&
        (dcache_aggregate_snapshot.resident_line_count == 0) &&
        (dcache_aggregate_snapshot.pending_writeback_count == 0) &&
        (dcache_incomplete_fragment_line_count == 0) &&
        !dcache_aggregate_snapshot.c_assembly_pending &&
        (dcache_aggregate_snapshot.observed_corrupt_line_count == 0);
    dcache_aggregate_snapshot.dcache_drain_complete = new_drain_complete;
    // 中文注释：只在已发布快照从未 drain 变为 drain 的边沿记录诊断时间；它是
    // DCache 可读门槛的转换时刻，不冒充最近一次 overlay 写入的提交时间。
    if (dcache_aggregate_snapshot.published && new_drain_complete && !was_drain_complete) begin
        dcache_aggregate_snapshot.drain_epoch++;
        dcache_aggregate_snapshot.drain_transition_sample = shared_mem_sample_time;
        dcache_aggregate_snapshot.drain_transition_time   = $time;
    end
    // 中文注释：RM 的唯一读取门槛从同一 snapshot 推导，不让调用方自行拼接多个 live 字段。
    // 新 Acquire/C-data/batch/corrupt 观察由后续 observer 更新输入字段并再次调用本函数。
    dcache_aggregate_snapshot.dcache_overlay_read_ready =
        dcache_aggregate_snapshot.published &&
        dcache_aggregate_snapshot.dcache_drain_complete;
endfunction:publish_dcache_aggregate_snapshot

function bit mem_access_base_sequence::peek_dcache_aggregate_snapshot(
    output dcache_aggregate_snapshot_t snapshot
);
    snapshot = dcache_aggregate_snapshot;
    return dcache_aggregate_snapshot.published &&
           dcache_aggregate_snapshot.owner_valid &&
           dcache_aggregate_snapshot.observer_ready;
endfunction:peek_dcache_aggregate_snapshot

function void mem_access_base_sequence::publish_dcache_owner_baseline();
    if (!dcache_owner_claimed || !shared_mem_lifecycle_initialized) begin
        return;
    end
    dcache_aggregate_snapshot.owner_valid = 1'b1;
    dcache_aggregate_snapshot.published = 1'b1;
    dcache_aggregate_snapshot.generation = dcache_owner_generation;
    dcache_aggregate_snapshot.resident_line_count = 0;
    dcache_aggregate_snapshot.pending_writeback_count = 0;
    dcache_aggregate_snapshot.observed_corrupt_line_count = dcache_corrupt_byte_mask_by_line.num();
    dcache_aggregate_snapshot.c_assembly_pending = 1'b0;
    dcache_aggregate_snapshot.observer_ready = 1'b1;
    dcache_fragment_pending_bytes.delete();
    dcache_fragment_committed_bytes.delete();
    dcache_incomplete_fragment_line_count = 0;
    publish_dcache_aggregate_snapshot();
endfunction:publish_dcache_owner_baseline

function void mem_access_base_sequence::invalidate_dcache_runtime_observer();
    // 中文注释：runtime reset/owner 暂停只使旁路快照暂不可读，并清除未完成 fragment
    // 观察；已提交 overlay/corrupt byte mask 仍归 shared-memory lifecycle 管理。
    // 仅在已发布快照第一次失效时推进 generation；reset 保持期间重复调用不重复递增。
    if (dcache_aggregate_snapshot.published) begin
        dcache_owner_generation++;
    end
    dcache_aggregate_snapshot.published = 1'b0;
    dcache_aggregate_snapshot.owner_valid = dcache_owner_claimed;
    dcache_aggregate_snapshot.generation = dcache_owner_generation;
    dcache_aggregate_snapshot.observer_ready = 1'b0;
    dcache_aggregate_snapshot.dcache_drain_complete = 1'b0;
    dcache_aggregate_snapshot.dcache_overlay_read_ready = 1'b0;
    dcache_aggregate_snapshot.pending_writeback_count = 0;
    dcache_aggregate_snapshot.c_assembly_pending = 1'b0;
    dcache_fragment_pending_bytes.delete();
    dcache_fragment_committed_bytes.delete();
    dcache_incomplete_fragment_line_count = 0;
endfunction:invalidate_dcache_runtime_observer

function void mem_access_base_sequence::observe_dcache_line_transition(
    input bit old_alias_valid,
    input bit new_alias_valid
);
    if (!dcache_owner_claimed) begin
        return;
    end
    if (!old_alias_valid && new_alias_valid) begin
        dcache_aggregate_snapshot.resident_line_count++;
    end
    else if (old_alias_valid && !new_alias_valid) begin
        if (dcache_aggregate_snapshot.resident_line_count == 0) begin
            dcache_aggregate_snapshot.observer_ready = 1'b0;
        end
        else begin
            dcache_aggregate_snapshot.resident_line_count--;
        end
    end
    publish_dcache_aggregate_snapshot();
endfunction:observe_dcache_line_transition

function void mem_access_base_sequence::observe_dcache_map_cleared();
    if (!dcache_owner_claimed) begin
        return;
    end
    dcache_aggregate_snapshot.resident_line_count = 0;
    dcache_fragment_pending_bytes.delete();
    dcache_fragment_committed_bytes.delete();
    dcache_incomplete_fragment_line_count = 0;
    dcache_aggregate_snapshot.pending_writeback_count = 0;
    dcache_aggregate_snapshot.c_assembly_pending = 1'b0;
    dcache_aggregate_snapshot.observer_ready = 1'b0;
    publish_dcache_aggregate_snapshot();
endfunction:observe_dcache_map_cleared

function void mem_access_base_sequence::observe_dcache_c_assembly_start();
    if (dcache_owner_claimed) begin
        dcache_aggregate_snapshot.c_assembly_pending = 1'b1;
        publish_dcache_aggregate_snapshot();
    end
endfunction:observe_dcache_c_assembly_start

function void mem_access_base_sequence::observe_dcache_c_assembly_complete();
    if (dcache_owner_claimed) begin
        dcache_aggregate_snapshot.c_assembly_pending = 1'b0;
        publish_dcache_aggregate_snapshot();
    end
endfunction:observe_dcache_c_assembly_complete

function void mem_access_base_sequence::observe_dcache_corrupt_line(input mem_addr_t line_addr);
    mem_addr_t           byte_addr;
    mem_line_addr_t      backing_line;
    dcache_line_addr_t   dcache_line;
    bit [9:0]            byte_offset;
    mem_line_mask_t      line_mask;

    if (!dcache_owner_claimed) begin
        return;
    end
    line_mask = '0;
    for (int unsigned byte_idx = 0; byte_idx < 64; byte_idx++) begin
        byte_addr   = line_addr + mem_addr_t'(byte_idx);
        backing_line = byte_addr[47:10];
        byte_offset = byte_addr[9:0];
        line_mask[byte_offset] = 1'b1;
    end
    dcache_line = line_addr[47:6];
    if (!dcache_corrupt_byte_mask_by_line.exists(dcache_line)) begin
        dcache_corrupt_byte_mask_by_line[dcache_line] = 64'hffff_ffff_ffff_ffff;
        dcache_aggregate_snapshot.observed_corrupt_line_count++;
    end
    else begin
        dcache_corrupt_byte_mask_by_line[dcache_line] |= 64'hffff_ffff_ffff_ffff;
    end
    write_overlay_corrupt_byte_mask[line_addr[47:10]] |= line_mask;
    publish_dcache_aggregate_snapshot();
endfunction:observe_dcache_corrupt_line

function void mem_access_base_sequence::observe_dcache_write_enqueued(
    input shared_mem_write_event_t write_event
);
    mem_addr_t           byte_addr;
    dcache_line_addr_t   dcache_line;
    bit [5:0]             line_byte;
    bit [63:0]            event_bytes;
    bit                   had_line;

    if (!dcache_owner_claimed) begin
        return;
    end
    if (dcache_aggregate_snapshot.pending_writeback_count == '1) begin
        dcache_aggregate_snapshot.observer_ready = 1'b0;
    end
    else begin
        dcache_aggregate_snapshot.pending_writeback_count++;
    end
    foreach (write_event.byte_mask[i]) begin
        if (!write_event.byte_mask[i]) begin
            continue;
        end
        byte_addr = write_event.addr + mem_addr_t'(i);
        dcache_line = byte_addr[47:6];
        line_byte = byte_addr[5:0];
        event_bytes = '0;
        event_bytes[line_byte] = 1'b1;
        had_line = dcache_fragment_pending_bytes.exists(dcache_line);
        if (!had_line) begin
            dcache_fragment_pending_bytes[dcache_line] = '0;
            dcache_fragment_committed_bytes[dcache_line] = '0;
            dcache_incomplete_fragment_line_count++;
        end
        dcache_fragment_pending_bytes[dcache_line] |= event_bytes;
    end
    publish_dcache_aggregate_snapshot();
endfunction:observe_dcache_write_enqueued

function void mem_access_base_sequence::observe_dcache_write_committed(
    input shared_mem_write_event_t write_event
);
    mem_addr_t         byte_addr;
    dcache_line_addr_t dcache_line;
    bit [5:0]           line_byte;
    bit [63:0]          event_bytes;
    mem_line_mask_t     corrupt_mask;
    mem_addr_t           corrupt_byte_addr;
    mem_line_addr_t      corrupt_backing_line;
    dcache_line_addr_t   committed_line;
    dcache_line_addr_t   touched_line_q[$];
    bit                  touched_line_seen[dcache_line_addr_t];

    if (!dcache_owner_claimed) begin
        return;
    end
    if (dcache_aggregate_snapshot.pending_writeback_count == 0) begin
        dcache_aggregate_snapshot.observer_ready = 1'b0;
    end
    else begin
        dcache_aggregate_snapshot.pending_writeback_count--;
    end
    foreach (write_event.byte_mask[i]) begin
        if (!write_event.byte_mask[i]) begin
            continue;
        end
        byte_addr = write_event.addr + mem_addr_t'(i);
        dcache_line = byte_addr[47:6];
        line_byte = byte_addr[5:0];
        event_bytes = '0;
        event_bytes[line_byte] = 1'b1;
        if (!touched_line_seen.exists(dcache_line)) begin
            touched_line_seen[dcache_line] = 1'b1;
            touched_line_q.push_back(dcache_line);
        end
        if (!dcache_fragment_committed_bytes.exists(dcache_line)) begin
            dcache_aggregate_snapshot.observer_ready = 1'b0;
        end
        else begin
            dcache_fragment_committed_bytes[dcache_line] |= event_bytes;
        end
    end
    foreach (touched_line_q[touched_idx]) begin
        committed_line = touched_line_q[touched_idx];
        if (!dcache_fragment_pending_bytes.exists(committed_line)) begin
            dcache_aggregate_snapshot.observer_ready = 1'b0;
        end
        else if ((dcache_fragment_committed_bytes[committed_line] == 64'hffff_ffff_ffff_ffff) &&
                 (dcache_fragment_pending_bytes[committed_line] == 64'hffff_ffff_ffff_ffff)) begin
            if (dcache_incomplete_fragment_line_count == 0) begin
                dcache_aggregate_snapshot.observer_ready = 1'b0;
            end
            else begin
                dcache_incomplete_fragment_line_count--;
            end
            // 仅在低/高两个既有 fragment 均提交后清除该 DCache line 的 corrupt byte mask。
            corrupt_mask = '0;
            corrupt_backing_line = mem_line_addr_t'(committed_line[41:4]);
            for (int unsigned byte_idx = 0; byte_idx < 64; byte_idx++) begin
                corrupt_byte_addr = {committed_line, 6'b0} + mem_addr_t'(byte_idx);
                corrupt_mask[corrupt_byte_addr[9:0]] = 1'b1;
            end
            if (write_overlay_corrupt_byte_mask.exists(corrupt_backing_line) &&
                ((write_overlay_corrupt_byte_mask[corrupt_backing_line] & corrupt_mask) != '0)) begin
                write_overlay_corrupt_byte_mask[corrupt_backing_line] &= ~corrupt_mask;
                if (write_overlay_corrupt_byte_mask[corrupt_backing_line] == '0) begin
                    write_overlay_corrupt_byte_mask.delete(corrupt_backing_line);
                end
            end
            if (dcache_corrupt_byte_mask_by_line.exists(committed_line)) begin
                dcache_corrupt_byte_mask_by_line.delete(committed_line);
                if (dcache_aggregate_snapshot.observed_corrupt_line_count == 0) begin
                    dcache_aggregate_snapshot.observer_ready = 1'b0;
                end
                else begin
                    dcache_aggregate_snapshot.observed_corrupt_line_count--;
                end
            end
            dcache_fragment_pending_bytes.delete(committed_line);
            dcache_fragment_committed_bytes.delete(committed_line);
        end
    end
    publish_dcache_aggregate_snapshot();
endfunction:observe_dcache_write_committed

function void mem_access_base_sequence::observe_uncache_write_committed(
    input shared_mem_write_event_t write_event
);
    mem_addr_t         byte_addr;
    mem_line_addr_t    backing_line;
    dcache_line_addr_t dcache_line;
    bit [9:0]          byte_offset;
    bit [5:0]          dcache_byte_offset;

    foreach (write_event.byte_mask[i]) begin
        if (!write_event.byte_mask[i]) begin
            continue;
        end
        byte_addr = write_event.addr + mem_addr_t'(i);
        backing_line = byte_addr[47:10];
        byte_offset = byte_addr[9:0];
        dcache_line = byte_addr[47:6];
        dcache_byte_offset = byte_addr[5:0];
        if (write_overlay_corrupt_byte_mask.exists(backing_line)) begin
            write_overlay_corrupt_byte_mask[backing_line][byte_offset] = 1'b0;
            if (write_overlay_corrupt_byte_mask[backing_line] == '0) begin
                write_overlay_corrupt_byte_mask.delete(backing_line);
            end
        end
        if (dcache_corrupt_byte_mask_by_line.exists(dcache_line)) begin
            dcache_corrupt_byte_mask_by_line[dcache_line][dcache_byte_offset] = 1'b0;
            if (dcache_corrupt_byte_mask_by_line[dcache_line] == '0) begin
                dcache_corrupt_byte_mask_by_line.delete(dcache_line);
                if (dcache_aggregate_snapshot.observed_corrupt_line_count == 0) begin
                    dcache_aggregate_snapshot.observer_ready = 1'b0;
                end
                else begin
                    dcache_aggregate_snapshot.observed_corrupt_line_count--;
                end
            end
        end
    end
    publish_dcache_aggregate_snapshot();
endfunction:observe_uncache_write_committed

function void mem_access_base_sequence::apply_shared_mem_write(input shared_mem_write_event_t write_event);
    mem_addr_t      byte_addr;
    mem_line_addr_t line_addr;
    bit [9:0]       byte_offset;

    foreach (write_event.byte_mask[i]) begin
        if (write_event.byte_mask[i]) begin
            byte_addr   = write_event.addr + mem_addr_t'(i);
            line_addr   = byte_addr[47:10];
            byte_offset = byte_addr[9:0];
            ensure_write_overlay_line(line_addr);
            write_overlay_mem[line_addr][(byte_offset * 8) +: 8] = write_event.store_data[(i * 8) +: 8];
            write_overlay_byte_valid[line_addr][byte_offset] = 1'b1;
        end
    end
endfunction:apply_shared_mem_write

function void mem_access_base_sequence::commit_shared_mem_write_batch();
    // 中文注释：先提交 DCache C 写回，再提交 Uncache store；同拍同 byte 冲突固定以后者为准。
    foreach (dcache_write_batch[i]) begin
        apply_shared_mem_write(dcache_write_batch[i]);
        observe_dcache_write_committed(dcache_write_batch[i]);
    end
    dcache_write_batch.delete();
    foreach (uncache_write_batch[i]) begin
        apply_shared_mem_write(uncache_write_batch[i]);
        observe_uncache_write_committed(uncache_write_batch[i]);
    end
    uncache_write_batch.delete();
endfunction:commit_shared_mem_write_batch

function void mem_access_base_sequence::begin_shared_mem_sample(input longint unsigned sample_time);
    if (!shared_mem_sample_valid) begin
        shared_mem_sample_valid = 1'b1;
        shared_mem_sample_time  = sample_time;
        return;
    end
    if (sample_time < shared_mem_sample_time) begin
        $fatal(1, "shared memory sample time moved backwards: current=%0d previous=%0d",
               sample_time, shared_mem_sample_time);
    end
    if (sample_time != shared_mem_sample_time) begin
        commit_shared_mem_write_batch();
        shared_mem_sample_time = sample_time;
    end
endfunction:begin_shared_mem_sample

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

    // backing memory 只保存确定性懒初始化数据；DUT memory-facing store 必须经 overlay batch。
    if (is_store) begin
        `uvm_fatal(get_type_name(), "main_mem_access_task only provides backing reads; use shared_mem_access_task for DUT writes")
        return;
    end

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

                load_data[(i * 8) +: 8] = main_mem[line_addr][(byte_offset * 8) +: 8];
            end
        end
    end

    if (corrupt || denied) begin
        load_data = '0;
    end

endtask:main_mem_access_task

task mem_access_base_sequence::shared_mem_access_task(
    input  mem_addr_t       addr,
    input  bit              is_store,
    input  mem_line_mask_t  byte_mask,
    input  mem_line_data_t  store_data,
    output bit              corrupt,
    output bit              denied,
    output mem_line_data_t  load_data,
    input  shared_mem_write_owner_e write_owner
);
    mem_addr_t      byte_addr;
    mem_line_addr_t line_addr;
    bit [9:0]       byte_offset;
    mem_line_data_t main_load_data;
    bit             main_corrupt;
    bit             main_denied;
    shared_mem_write_event_t write_event;

    corrupt   = 1'b0;
    denied    = 1'b0;
    load_data = '0;

    // 中文注释：同一物理采样时刻的所有访问先固定在上一轮 committed view；下一时刻首次访问时
    // 才统一提交上一拍 DCache/Uncache 写队列，避免两个 responder 的执行 delta 决定读写可见性。
    begin_shared_mem_sample($time);
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
                if (write_owner == SHARED_MEM_WRITE_NONE) begin
                    `uvm_fatal(get_type_name(), "shared memory store requires a DCache or Uncache write owner")
                end
            end
            else if (write_overlay_byte_valid.exists(line_addr) && write_overlay_byte_valid[line_addr][byte_offset]) begin
                load_data[(i * 8) +: 8] = write_overlay_mem[line_addr][(byte_offset * 8) +: 8];
            end
            else begin
                load_data[(i * 8) +: 8] = main_load_data[(i * 8) +: 8];
            end
        end
    end
    if (is_store) begin
        write_event.addr        = addr;
        write_event.byte_mask   = byte_mask;
        write_event.store_data  = store_data;
        case (write_owner)
            SHARED_MEM_WRITE_DCACHE: begin
                dcache_write_batch.push_back(write_event);
                observe_dcache_write_enqueued(write_event);
            end
            SHARED_MEM_WRITE_UNCACHE: uncache_write_batch.push_back(write_event);
            default: begin
                `uvm_fatal(get_type_name(), $sformatf("unsupported shared memory write owner=%0d", write_owner))
            end
        endcase
    end
endtask:shared_mem_access_task

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

    localparam bit [2:0] TL_REPORT_TTOB            = 3'd0;
    localparam bit [2:0] TL_REPORT_TTON            = 3'd1;
    localparam bit [2:0] TL_REPORT_BTON            = 3'd2;
    localparam bit [2:0] TL_REPORT_BTOB            = 3'd4;
    localparam bit [2:0] TL_REPORT_NTON            = 3'd5;

    localparam bit [2:0] TL_LINE_SIZE              = 3'd6;
    localparam bit [5:0] TL_CBO_SOURCE             = 6'd17;

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

    typedef enum int unsigned {
        DCACHE_LINE_INVALID        = 0,
        DCACHE_LINE_ACTIVE         = 1,
        DCACHE_LINE_GRANT_WAIT_E   = 2,
        DCACHE_LINE_ALIAS_CONFLICT = 3,
        DCACHE_LINE_PROBE_PENDING  = 4
    } dcache_line_lifecycle_e;

    typedef enum int unsigned {
        DCACHE_PROBE_OWNER_RANDOM         = 0,
        DCACHE_PROBE_OWNER_FLUSH          = 1,
        DCACHE_PROBE_OWNER_CBO            = 2,
        DCACHE_PROBE_OWNER_ALIAS_CONFLICT = 3
    } dcache_probe_owner_e;

    typedef enum int unsigned {
        DCACHE_PROBE_STATE_QUEUED     = 0,
        DCACHE_PROBE_STATE_B_HOLD     = 1,
        DCACHE_PROBE_STATE_WAIT_C     = 2,
        DCACHE_PROBE_STATE_C_ASSEMBLY = 3
    } dcache_probe_state_e;

    typedef enum int unsigned {
        // 未观察到 L2 flush request，允许普通 A 入口和随机 Probe batch。
        DCACHE_L2_FLUSH_IDLE  = 0,
        // 已观察到 level request，先自然收敛 request 前已建立的 D/E/B/C owner。
        DCACHE_L2_FLUSH_DRAIN = 1,
        // 已建立固定 snapshot，逐条提交 Probe(toN) 并等待全部 C reply。
        DCACHE_L2_FLUSH_PROBE = 2,
        // snapshot 已完成，done 保持为 1，直到外部 level request 撤销。
        DCACHE_L2_FLUSH_DONE  = 3
    } dcache_l2_flush_state_e;

    typedef longint unsigned dcache_probe_token_t;

    localparam int unsigned DCACHE_MAX_PROBE_RECORDS = 16;

    virtual dcache_agent_agent_interface dcache_vif;
    // 中文注释：L2 flush request 由顶层 other_ctrl 接口输出，DCache responder 直接在自己的
    // drv_cb 边界读取同拍快照。这样不会把 monitor 的上一拍 level 当作当前 request，也不驱动该接口。
    virtual other_ctrl_agent_agent_interface other_ctrl_vif;

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

    // 中文注释：DCache response record 保存已真实 fire、尚未完成最后一个 D beat 的回复。
    // 同一张表同时服务 Grant/GrantData、CBOAck 和 ReleaseAck；current_d_record 是从表中
    // 选出后正在 D channel 上保持的唯一记录，仍属于这张表的 capacity。
    typedef struct {
        dcache_pending_d_kind_e kind;
        bit [3:0]               cbo_opcode;
        longint unsigned        eligible_cycle;
        int unsigned            beat_count;
        int unsigned            beat_idx;
        bit [1:0]               param;
        bit [2:0]               size;
        bit [5:0]               source;
        bit [9:0]               sink;
        bit                     denied;
        bit                     corrupt;
        // 中文注释：sticky ledger 的 reservation generation 在 AcquireBlock A.fire
        // 固定，并随两拍 GrantData 一起保持；最后一个 D.fire 用它审计 pending/live
        // 转换，不能在 D.ready hold 期间重新生成。
        longint unsigned        l2_d_error_reservation_generation;
        bit                     echo_isKeyword;
        bit [47:0]              line_addr;
        bit [1:0]               line_alias;
        bit [255:0]             data_low;
        bit [255:0]             data_high;
        bit                     hint_pending;
        bit [3:0]               hint_source_id;
        bit                     hint_isKeyword;
    } dcache_response_record_t;

    // 中文注释：GrantAck wait record 只保存已经完成最后一个 D beat 的 Grant owner。
    // D response record 在最后一个 D.fire 时释放；同一个 sink 直到 E.fire 匹配后才可复用。
    typedef struct {
        bit [47:0] line_addr;
        bit [1:0]  line_alias;
        bit [9:0]  sink;
    } dcache_grant_ack_record_t;

    typedef struct {
        longint unsigned due_cycle;
        bit [3:0]        source_id;
        bit              isKeyword;
    } dcache_hint_record_t;

    dcache_response_record_t  dcache_rsp_q[$];
    dcache_response_record_t  current_d_record;
    bit                       current_d_valid;
    bit                       dcache_rsp_timer_active;
    longint unsigned          dcache_rsp_timer_due_cycle;
    dcache_grant_ack_record_t grant_ack_wait_q[$];
    dcache_hint_record_t      dcache_hint_q[$];

    // 中文注释：line record 是 physical line 的唯一轻量 alias 生命周期真源。
    // 设置：Grant 的 D.fire 标记 GRANT_WAIT_E，GrantAck 后转 ACTIVE；Probe/Release/CBO 后更新或删除。
    // 作用：alias 不参与 memory key，只用于判断同一 physical line 的 DCache 副本和 Probe B payload。
    typedef struct {
        bit [47:0]                line_addr;
        bit [1:0]                 active_alias;
        bit                       alias_valid;
        bit                       may_return_data;
        bit                       data_valid;
        dcache_line_lifecycle_e   lifecycle_state;
        bit                       deferred_acquire_valid;
        dcache_agent_agent_xaction deferred_acquire;
        bit                       deferred_response_reserved;
        bit [9:0]                 deferred_sink;
        longint unsigned          deferred_accept_cycle;
    } dcache_cached_line_record_t;

    // 中文注释：Probe record 保存一笔 B Probe 的稳定身份，C response 通过 line 唯一定位后再回到 token。
    // 同一 physical line 只允许一笔未收敛 record；不同 line 可同时 WAIT_C，B channel 仍一次只保持一笔。
    typedef struct {
        dcache_probe_token_t token;
        // batch_id 仅记录随机 batch 归属；0 表示 alias/CBO/flush 等非随机 owner。
        longint unsigned     batch_id;
        bit [47:0]           line_addr;
        bit [1:0]            probe_alias;
        bit [1:0]            target_cap;
        dcache_probe_owner_e owner;
        dcache_probe_state_e state;
    } dcache_probe_record_t;

    dcache_cached_line_record_t cached_line_by_addr[mem_addr_t];
    dcache_probe_record_t       probe_record_q[$];
    dcache_probe_token_t        next_probe_token;
    longint unsigned            next_probe_batch_id;
    bit                         probe_b_hold_valid;
    dcache_probe_token_t        probe_b_hold_token;
    int unsigned                deferred_response_reservation_count;
    bit                         grant_sink_reserved[MEMBLOCK_DUT_DCACHE_A_MAX_OUTSTANDING];

    // 中文注释：CBO context 是单笔 CBO A.fire 到对应 CBOAck D.fire 的唯一 owner。
    // 命中路径先保留 response slot 并等待 Probe C 收敛；miss 路径立即转为 CBOAck record。
    bit                         cbo_context_valid;
    bit                         cbo_response_reserved;
    bit                         pending_cbo_probe_valid;
    bit [3:0]                   pending_cbo_probe_opcode;
    bit [47:0]                  pending_cbo_probe_line;
    bit [5:0]                   pending_cbo_probe_source;
    bit [1:0]                   pending_cbo_probe_cap;
    dcache_probe_token_t        pending_cbo_probe_token;
    bit                         pending_cbo_ack_denied;
    bit                         pending_cbo_ack_corrupt;

    // 中文注释：轻量 L2 flush 只拥有 request level、snapshot 和 Probe 调度状态。
    // 设置：观察到 io_outer_l2_flush_en 后进入 DRAIN；清零：DONE 观察到 request 撤销或 reset。
    // 作用：DRAIN/PROBE 阶段关闭新 A.ready，DONE 才拉高 io_l2_flush_done；不清正常 D/E 或 shared memory。
    dcache_l2_flush_state_e l2_flush_state;
    bit [47:0]              l2_flush_snapshot_line_q[$];

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
    dcache_probe_token_t c_assembly_probe_token;
    // 中文注释：ReleaseData 首个 C.fire 已经占用一个未来 ReleaseAck response slot。
    // 该 reservation 防止 16 笔表接近满时第二 beat 收齐后没有空间建立 ReleaseAck。
    bit c_assembly_response_reserved;

    `uvm_object_utils(dcache_mem__access_base_sequence)

    extern function new(string name = "dcache_mem__access_base_sequence");
    extern virtual function void clear_current_d_state();
    extern virtual function void clear_dcache_response_state();
    extern virtual function void clear_c_assembly_state();
    extern virtual function void clear_cbo_context();
    extern virtual function void clear_runtime_state(bit clear_cache_map = 1'b1);
    extern virtual function void build_dcache_idle_xaction(output dcache_agent_agent_xaction rsp_xact);
    extern virtual function void capture_dcache_a_xaction(output dcache_agent_agent_xaction req_xact);
    extern virtual function void check_dcache_c_payload_known();
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
    extern virtual function void mark_cached_line_grant_wait(
        input bit [47:0] addr,
        input bit [1:0]  line_alias
    );
    extern virtual function bit is_alias_conflict_request(
        input dcache_agent_agent_xaction req_xact
    );
    extern virtual function bit has_probe_for_line(input bit [47:0] line_addr);
    extern virtual function bit has_waiting_probe_c();
    extern virtual function int find_probe_record_by_token(input dcache_probe_token_t probe_token);
    extern virtual function int find_waiting_probe_record_by_line(input bit [47:0] line_addr);
    extern virtual function bit submit_probe(
        input bit [47:0]           line_addr,
        input bit [1:0]            target_cap,
        input dcache_probe_owner_e probe_owner,
        input longint unsigned     batch_id,
        output dcache_probe_token_t probe_token
    );
    extern virtual function void service_probe_b_hold();
    extern virtual function void build_probe_b_xaction(inout dcache_agent_agent_xaction cycle_xact);
    extern virtual function void process_probe_b_fire();
    extern virtual function void check_probe_response_param(
        input dcache_probe_record_t probe_record,
        input bit [2:0]             response_param,
        input string                response_name
    );
    extern virtual function bit reserve_deferred_acquire_resources(output bit [9:0] reserved_sink);
    extern virtual function void release_deferred_acquire_resources(
        input dcache_cached_line_record_t line_record
    );
    extern virtual task start_alias_conflict(
        input dcache_agent_agent_xaction req_xact,
        input longint unsigned           accept_cycle
    );
    extern virtual task complete_probe_record(
        input int              probe_index,
        input bit              data_response_seen,
        input bit              data_valid,
        input longint unsigned complete_cycle
    );
    extern virtual task enqueue_pending_cbo_ack(input longint unsigned complete_cycle);
    extern virtual task complete_cbo_probe(
        input dcache_probe_record_t probe_record,
        input longint unsigned      complete_cycle
    );
    extern virtual function int unsigned sample_dcache_response_delay();
    extern virtual function int unsigned get_dcache_response_count();
    extern virtual function bit has_dcache_response_capacity();
    extern virtual function bit is_grant_sink_in_use(input bit [9:0] sink);
    extern virtual function bit has_free_grant_sink();
    extern virtual function bit allocate_grant_sink(output bit [9:0] sink);
    extern virtual function bit can_accept_dcache_a_request(
        input dcache_agent_agent_xaction req_xact
    );
    extern virtual function bit can_accept_dcache_release_c_request(
        input dcache_agent_agent_xaction req_xact
    );
    extern virtual function int find_dcache_eligible_response(
        input longint unsigned current_cycle,
        input int unsigned      visible_count
    );
    extern virtual function void service_dcache_response_scheduler(
        input longint unsigned current_cycle,
        input int unsigned      visible_count
    );
    extern virtual function void enqueue_dcache_response(input dcache_response_record_t response_record);
    extern virtual function bit sample_hint_enable();
    extern virtual function bit sample_probe_batch_start();
    extern virtual function int unsigned sample_probe_batch_count();
    extern virtual function bit [1:0] sample_probe_target_cap();
    extern virtual function bit select_random_cached_line(output bit [47:0] line_addr);
    extern virtual function bit is_l2_flush_drain_complete();
    extern virtual function bit has_flush_probe_record();
    extern virtual function void capture_l2_flush_snapshot();
    extern virtual function void service_l2_flush(input bit sampled_l2_flush_en);
    extern virtual function bit l2_flush_blocks_a_request(input bit sampled_l2_flush_en);
    extern virtual task accept_dcache_a_request(
        input dcache_agent_agent_xaction req_xact,
        input longint unsigned           accept_cycle
    );
    extern virtual function void build_current_d_xaction(inout dcache_agent_agent_xaction cycle_xact);
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

function void dcache_mem__access_base_sequence::clear_current_d_state();
    current_d_valid  = 1'b0;
    current_d_record = '{default:'0};
endfunction:clear_current_d_state

function void dcache_mem__access_base_sequence::clear_dcache_response_state();
    dcache_rsp_q.delete();
    grant_ack_wait_q.delete();
    dcache_hint_q.delete();
    dcache_rsp_timer_active    = 1'b0;
    dcache_rsp_timer_due_cycle = 0;
    clear_current_d_state();
endfunction:clear_dcache_response_state

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
    c_assembly_probe_token    = '0;
    c_assembly_response_reserved = 1'b0;
endfunction:clear_c_assembly_state

function void dcache_mem__access_base_sequence::clear_cbo_context();
    cbo_context_valid        = 1'b0;
    cbo_response_reserved    = 1'b0;
    pending_cbo_probe_valid  = 1'b0;
    pending_cbo_probe_opcode = '0;
    pending_cbo_probe_line   = '0;
    pending_cbo_probe_source = '0;
    pending_cbo_probe_cap    = '0;
    pending_cbo_probe_token  = '0;
    pending_cbo_ack_denied   = 1'b0;
    pending_cbo_ack_corrupt  = 1'b0;
endfunction:clear_cbo_context

function void dcache_mem__access_base_sequence::clear_runtime_state(bit clear_cache_map = 1'b1);
    a_accept_armed           = 1'b0;
    c_accept_armed           = 1'b0;
    armed_a_req_xact         = null;
    armed_c_req_xact         = null;
    probe_record_q.delete();
    next_probe_token                    = 1;
    next_probe_batch_id                 = 1;
    probe_b_hold_valid                  = 1'b0;
    probe_b_hold_token                  = '0;
    deferred_response_reservation_count = 0;
    foreach (grant_sink_reserved[i]) begin
        grant_sink_reserved[i] = 1'b0;
    end
    l2_flush_state = DCACHE_L2_FLUSH_IDLE;
    l2_flush_snapshot_line_q.delete();
    last_cycle_valid         = 1'b0;
    last_cycle_xact          = null;
    clear_dcache_response_state();
    clear_c_assembly_state();
    clear_cbo_context();
    if (clear_cache_map) begin
        observe_dcache_map_cleared();
        cached_line_by_addr.delete();
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

function void dcache_mem__access_base_sequence::check_dcache_c_payload_known();
    logic [2:0] c_opcode_raw;

    // 中文注释：C payload 会复制到二态 xaction；先只检查当前 responder 真实消费的 header，
    // data/corrupt 仅对有数据的 C opcode 检查，避免无数据 ProbeAck/Release 的 don't-care data 误报。
    c_opcode_raw = dcache_vif.drv_cb.auto_inner_dcache_client_out_c_bits_opcode;
    if ($isunknown({c_opcode_raw,
                    dcache_vif.drv_cb.auto_inner_dcache_client_out_c_bits_param,
                    dcache_vif.drv_cb.auto_inner_dcache_client_out_c_bits_size,
                    dcache_vif.drv_cb.auto_inner_dcache_client_out_c_bits_source,
                    dcache_vif.drv_cb.auto_inner_dcache_client_out_c_bits_address})) begin
        `uvm_fatal(get_type_name(), "DCache C header sampled as X/Z outside reset")
    end

    case (c_opcode_raw)
        TL_C_OPCODE_PROBE_ACKDATA,
        TL_C_OPCODE_RELEASEDATA: begin
            if ($isunknown({dcache_vif.drv_cb.auto_inner_dcache_client_out_c_bits_data,
                            dcache_vif.drv_cb.auto_inner_dcache_client_out_c_bits_corrupt})) begin
                `uvm_fatal(get_type_name(), "DCache C data/corrupt sampled as X/Z outside reset")
            end
        end
        default: begin
            // 无数据 C opcode 的 data/corrupt 不参与当前 responder 语义；后续 opcode 合法性由现有分支检查。
        end
    endcase
endfunction:check_dcache_c_payload_known

function void dcache_mem__access_base_sequence::capture_dcache_c_xaction(output dcache_agent_agent_xaction req_xact);
    check_dcache_c_payload_known();
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

    shared_mem_access_task(beat_addr,
                           is_store,
                           line_mask,
                           line_store_data,
                           corrupt,
                           denied,
                           line_load_data,
                           is_store ? SHARED_MEM_WRITE_DCACHE : SHARED_MEM_WRITE_NONE);
    load_data = line_load_data[255:0];
endtask:dcache_mem_access_task

function void dcache_mem__access_base_sequence::check_l2_model_cfg();
    void'(seq_csr_common::get_l2_rsp_delay_zero_wt());
    void'(seq_csr_common::get_l2_rsp_delay_small_wt());
    void'(seq_csr_common::get_l2_rsp_delay_medium_wt());
    void'(seq_csr_common::get_l2_rsp_delay_large_wt());
    void'(seq_csr_common::get_l2_hint_valid_wt());
    void'(seq_csr_common::get_l2_probe_en());
    void'(seq_csr_common::get_l2_probe_pre_start_wt());
    void'(seq_csr_common::get_l2_probe_count_one_wt());
    void'(seq_csr_common::get_l2_probe_count_mid_wt());
    void'(seq_csr_common::get_l2_probe_count_large_wt());
    void'(seq_csr_common::get_l2_probe_to_b_wt());
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
    bit [47:0] line_addr;
    bit old_alias_valid;
    dcache_cached_line_record_t line_record;

    line_addr = line_addr64(addr);
    old_alias_valid = cached_line_by_addr.exists(line_addr) &&
                      cached_line_by_addr[line_addr].alias_valid;
    line_record.line_addr              = line_addr;
    line_record.active_alias           = line_alias;
    line_record.alias_valid            = 1'b1;
    line_record.may_return_data        = 1'b1;
    line_record.data_valid             = 1'b1;
    line_record.lifecycle_state        = DCACHE_LINE_ACTIVE;
    line_record.deferred_acquire_valid = 1'b0;
    line_record.deferred_acquire       = null;
    line_record.deferred_response_reserved = 1'b0;
    line_record.deferred_sink          = '0;
    line_record.deferred_accept_cycle  = '0;
    cached_line_by_addr[line_addr]     = line_record;
    observe_dcache_line_transition(old_alias_valid, line_record.alias_valid);
endfunction:record_cached_line

function void dcache_mem__access_base_sequence::remove_cached_line(input bit [47:0] addr, input string reason);
    bit [47:0] key;
    bit old_alias_valid;

    key = line_addr64(addr);
    if (cached_line_by_addr.exists(key)) begin
        old_alias_valid = cached_line_by_addr[key].alias_valid;
        cached_line_by_addr.delete(key);
        observe_dcache_line_transition(old_alias_valid, 1'b0);
    end else begin
        `uvm_info(get_type_name(),
                  $sformatf("remove_cached_line miss for line=0x%0h reason=%s", key, reason),
                  UVM_DEBUG)
    end
endfunction:remove_cached_line

function void dcache_mem__access_base_sequence::mark_cached_line_grant_wait(
    input bit [47:0] addr,
    input bit [1:0]  line_alias
);
    bit [47:0] line_addr;
    bit old_alias_valid;
    dcache_cached_line_record_t line_record;

    line_addr = line_addr64(addr);
    old_alias_valid = cached_line_by_addr.exists(line_addr) &&
                      cached_line_by_addr[line_addr].alias_valid;
    line_record.line_addr              = line_addr;
    line_record.active_alias           = line_alias;
    line_record.alias_valid            = 1'b0;
    line_record.may_return_data        = 1'b0;
    line_record.data_valid             = 1'b0;
    line_record.lifecycle_state        = DCACHE_LINE_GRANT_WAIT_E;
    line_record.deferred_acquire_valid = 1'b0;
    line_record.deferred_acquire       = null;
    line_record.deferred_response_reserved = 1'b0;
    line_record.deferred_sink          = '0;
    line_record.deferred_accept_cycle  = '0;
    cached_line_by_addr[line_addr]     = line_record;
    observe_dcache_line_transition(old_alias_valid, line_record.alias_valid);
endfunction:mark_cached_line_grant_wait

function int unsigned dcache_mem__access_base_sequence::sample_dcache_response_delay();
    int unsigned delay_class;
    int unsigned delay_value;
    int unsigned zero_wt;
    int unsigned small_wt;
    int unsigned medium_wt;
    int unsigned large_wt;

    zero_wt   = seq_csr_common::get_l2_rsp_delay_zero_wt();
    small_wt  = seq_csr_common::get_l2_rsp_delay_small_wt();
    medium_wt = seq_csr_common::get_l2_rsp_delay_medium_wt();
    large_wt  = seq_csr_common::get_l2_rsp_delay_large_wt();

    if (!std::randomize(delay_class) with {
            delay_class dist {
                0 := zero_wt,
                1 := small_wt,
                2 := medium_wt,
                3 := large_wt
            };
        }) begin
        `uvm_fatal(get_type_name(), "failed to randomize DCache L2 response delay class")
    end
    case (delay_class)
        0: begin
            delay_value = 0;
        end
        1: begin
            if (!std::randomize(delay_value) with { delay_value inside {[1:10]}; }) begin
                `uvm_fatal(get_type_name(), "failed to randomize SMALL DCache L2 delay")
            end
        end
        2: begin
            if (!std::randomize(delay_value) with { delay_value inside {[10:100]}; }) begin
                `uvm_fatal(get_type_name(), "failed to randomize MEDIUM DCache L2 delay")
            end
        end
        3: begin
            if (!std::randomize(delay_value) with { delay_value inside {[101:1000]}; }) begin
                `uvm_fatal(get_type_name(), "failed to randomize LARGE DCache L2 delay")
            end
        end
        default: begin
            `uvm_fatal(get_type_name(), $sformatf("unexpected delay_class=%0d", delay_class))
        end
    endcase
    return delay_value;
endfunction:sample_dcache_response_delay

function int unsigned dcache_mem__access_base_sequence::get_dcache_response_count();
    return dcache_rsp_q.size() + (current_d_valid ? 1 : 0) +
           (c_assembly_response_reserved ? 1 : 0) +
           (cbo_response_reserved ? 1 : 0) +
           deferred_response_reservation_count;
endfunction:get_dcache_response_count

function bit dcache_mem__access_base_sequence::has_dcache_response_capacity();
    return get_dcache_response_count() < MEMBLOCK_DUT_DCACHE_A_MAX_OUTSTANDING;
endfunction:has_dcache_response_capacity

function bit dcache_mem__access_base_sequence::is_grant_sink_in_use(input bit [9:0] sink);
    if ((sink < MEMBLOCK_DUT_DCACHE_A_MAX_OUTSTANDING) && grant_sink_reserved[sink]) begin
        return 1'b1;
    end
    if (current_d_valid &&
        ((current_d_record.kind == DCACHE_PENDING_D_GRANT) ||
         (current_d_record.kind == DCACHE_PENDING_D_GRANT_DATA)) &&
        (current_d_record.sink == sink)) begin
        return 1'b1;
    end
    foreach (dcache_rsp_q[i]) begin
        if (((dcache_rsp_q[i].kind == DCACHE_PENDING_D_GRANT) ||
             (dcache_rsp_q[i].kind == DCACHE_PENDING_D_GRANT_DATA)) &&
            (dcache_rsp_q[i].sink == sink)) begin
            return 1'b1;
        end
    end
    foreach (grant_ack_wait_q[i]) begin
        if (grant_ack_wait_q[i].sink == sink) begin
            return 1'b1;
        end
    end
    return 1'b0;
endfunction:is_grant_sink_in_use

function bit dcache_mem__access_base_sequence::has_free_grant_sink();
    bit [9:0] sink;

    for (int unsigned i = 0; i < MEMBLOCK_DUT_DCACHE_A_MAX_OUTSTANDING; i++) begin
        sink = i;
        if (!is_grant_sink_in_use(sink)) begin
            return 1'b1;
        end
    end
    return 1'b0;
endfunction:has_free_grant_sink

function bit dcache_mem__access_base_sequence::allocate_grant_sink(output bit [9:0] sink);
    sink = '0;
    for (int unsigned i = 0; i < MEMBLOCK_DUT_DCACHE_A_MAX_OUTSTANDING; i++) begin
        sink = i;
        if (!is_grant_sink_in_use(sink)) begin
            return 1'b1;
        end
    end
    sink = '0;
    return 1'b0;
endfunction:allocate_grant_sink

function bit dcache_mem__access_base_sequence::can_accept_dcache_a_request(
    input dcache_agent_agent_xaction req_xact
);
    bit [47:0] line_addr;
    dcache_cached_line_record_t line_record;

    case (req_xact.auto_inner_dcache_client_out_a_bits_opcode)
        TL_A_OPCODE_ACQUIRE_BLOCK,
        TL_A_OPCODE_ACQUIRE_PERM: begin
            line_addr = line_addr64(req_xact.auto_inner_dcache_client_out_a_bits_address);
            if (cached_line_by_addr.exists(line_addr)) begin
                line_record = cached_line_by_addr[line_addr];
                if (line_record.deferred_acquire_valid ||
                    (line_record.lifecycle_state != DCACHE_LINE_ACTIVE)) begin
                    return 1'b0;
                end
            end
            // Alias conflict 的 A.fire 必须同时预留后续 Probe 和 deferred Grant 资源。
            // 不在 ready 打开后才发现 Probe queue 满，避免已经接受的 A 无法建立 owner。
            if (is_alias_conflict_request(req_xact) &&
                (has_probe_for_line(line_addr) ||
                 (probe_record_q.size() >= DCACHE_MAX_PROBE_RECORDS))) begin
                return 1'b0;
            end
            return has_dcache_response_capacity() && has_free_grant_sink();
        end
        TL_A_OPCODE_CBO_CLEAN,
        TL_A_OPCODE_CBO_FLUSH,
        TL_A_OPCODE_CBO_INVAL: begin
            line_addr = line_addr64(req_xact.auto_inner_dcache_client_out_a_bits_address);
            if (cbo_context_valid) begin
                return 1'b0;
            end
            if (cached_line_by_addr.exists(line_addr)) begin
                line_record = cached_line_by_addr[line_addr];
                if (!line_record.alias_valid ||
                    (line_record.lifecycle_state != DCACHE_LINE_ACTIVE) ||
                    line_record.deferred_acquire_valid ||
                    has_probe_for_line(line_addr)) begin
                    return 1'b0;
                end
                // 命中 CBO 必须创建共享 Probe record；队列满时反压 A，不能在真实 A.fire 后
                // 才发现没有 token 容量而留下无法收敛的 CBO context。
                if (probe_record_q.size() >= DCACHE_MAX_PROBE_RECORDS) begin
                    return 1'b0;
                end
            end
            return has_dcache_response_capacity();
        end
        default: begin
            `uvm_fatal(get_type_name(),
                       $sformatf("unsupported DCache coherent A opcode=%0d before accept",
                                 req_xact.auto_inner_dcache_client_out_a_bits_opcode))
        end
    endcase
    return 1'b0;
endfunction:can_accept_dcache_a_request

function bit dcache_mem__access_base_sequence::can_accept_dcache_release_c_request(
    input dcache_agent_agent_xaction req_xact
);
    case (req_xact.auto_inner_dcache_client_out_c_bits_opcode)
        TL_C_OPCODE_RELEASE,
        TL_C_OPCODE_RELEASEDATA: return has_dcache_response_capacity();
        default: return 1'b1;
    endcase
endfunction:can_accept_dcache_release_c_request

function bit dcache_mem__access_base_sequence::is_alias_conflict_request(
    input dcache_agent_agent_xaction req_xact
);
    bit [47:0] line_addr;
    dcache_cached_line_record_t line_record;

    if (!(req_xact.auto_inner_dcache_client_out_a_bits_opcode inside {
            TL_A_OPCODE_ACQUIRE_BLOCK,
            TL_A_OPCODE_ACQUIRE_PERM
        })) begin
        return 1'b0;
    end
    line_addr = line_addr64(req_xact.auto_inner_dcache_client_out_a_bits_address);
    if (!cached_line_by_addr.exists(line_addr)) begin
        return 1'b0;
    end
    line_record = cached_line_by_addr[line_addr];
    return line_record.alias_valid &&
           (line_record.lifecycle_state == DCACHE_LINE_ACTIVE) &&
           (line_record.active_alias != req_xact.auto_inner_dcache_client_out_a_bits_user_alias);
endfunction:is_alias_conflict_request

function bit dcache_mem__access_base_sequence::has_probe_for_line(input bit [47:0] line_addr);
    bit [47:0] line_key;

    line_key = line_addr64(line_addr);
    foreach (probe_record_q[i]) begin
        if (probe_record_q[i].line_addr == line_key) begin
            return 1'b1;
        end
    end
    return 1'b0;
endfunction:has_probe_for_line

function bit dcache_mem__access_base_sequence::has_waiting_probe_c();
    foreach (probe_record_q[i]) begin
        if (probe_record_q[i].state == DCACHE_PROBE_STATE_WAIT_C) begin
            return 1'b1;
        end
    end
    return 1'b0;
endfunction:has_waiting_probe_c

function int dcache_mem__access_base_sequence::find_probe_record_by_token(input dcache_probe_token_t probe_token);
    foreach (probe_record_q[i]) begin
        if (probe_record_q[i].token == probe_token) begin
            return i;
        end
    end
    return -1;
endfunction:find_probe_record_by_token

function int dcache_mem__access_base_sequence::find_waiting_probe_record_by_line(input bit [47:0] line_addr);
    bit [47:0] line_key;
    int found_index;

    line_key = line_addr64(line_addr);
    found_index = -1;
    foreach (probe_record_q[i]) begin
        if ((probe_record_q[i].line_addr == line_key) &&
            (probe_record_q[i].state == DCACHE_PROBE_STATE_WAIT_C)) begin
            if (found_index >= 0) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("multiple WAIT_C Probe records match line=0x%0h", line_key))
            end
            found_index = i;
        end
    end
    return found_index;
endfunction:find_waiting_probe_record_by_line

function void dcache_mem__access_base_sequence::check_probe_response_param(
    input dcache_probe_record_t probe_record,
    input bit [2:0]             response_param,
    input string                response_name
);
    case (probe_record.target_cap)
        TL_CAP_TON: begin
            if (!(response_param inside {TL_REPORT_TTON, TL_REPORT_BTON})) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("%s param=%0d is invalid for Probe(toN) token=%0d line=0x%0h",
                                     response_name,
                                     response_param,
                                     probe_record.token,
                                     probe_record.line_addr))
            end
        end
        TL_CAP_TOB: begin
            if (!(response_param inside {TL_REPORT_TTOB, TL_REPORT_BTOB})) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("%s param=%0d is invalid for Probe(toB) token=%0d line=0x%0h",
                                     response_name,
                                     response_param,
                                     probe_record.token,
                                     probe_record.line_addr))
            end
        end
        default: begin
            `uvm_fatal(get_type_name(),
                       $sformatf("Probe token=%0d line=0x%0h uses unsupported target_cap=%0d",
                                 probe_record.token,
                                 probe_record.line_addr,
                                 probe_record.target_cap))
        end
    endcase
endfunction:check_probe_response_param

function bit dcache_mem__access_base_sequence::reserve_deferred_acquire_resources(output bit [9:0] reserved_sink);
    reserved_sink = '0;
    if (!has_dcache_response_capacity() || !has_free_grant_sink()) begin
        return 1'b0;
    end
    if (!allocate_grant_sink(reserved_sink)) begin
        return 1'b0;
    end
    if (grant_sink_reserved[reserved_sink]) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("deferred Acquire tries to reserve an occupied sink=%0d", reserved_sink))
    end
    grant_sink_reserved[reserved_sink] = 1'b1;
    deferred_response_reservation_count++;
    return 1'b1;
endfunction:reserve_deferred_acquire_resources

function void dcache_mem__access_base_sequence::release_deferred_acquire_resources(
    input dcache_cached_line_record_t line_record
);
    if (!line_record.deferred_response_reserved) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("deferred Acquire line=0x%0h has no response reservation", line_record.line_addr))
    end
    if ((line_record.deferred_sink >= MEMBLOCK_DUT_DCACHE_A_MAX_OUTSTANDING) ||
        !grant_sink_reserved[line_record.deferred_sink]) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("deferred Acquire line=0x%0h owns invalid sink=%0d",
                             line_record.line_addr,
                             line_record.deferred_sink))
    end
    if (deferred_response_reservation_count == 0) begin
        `uvm_fatal(get_type_name(), "deferred Acquire response reservation counter underflow")
    end
    grant_sink_reserved[line_record.deferred_sink] = 1'b0;
    deferred_response_reservation_count--;
endfunction:release_deferred_acquire_resources

function bit dcache_mem__access_base_sequence::submit_probe(
    input bit [47:0]           line_addr,
    input bit [1:0]            target_cap,
    input dcache_probe_owner_e probe_owner,
    input longint unsigned     batch_id,
    output dcache_probe_token_t probe_token
);
    bit [47:0] line_key;
    dcache_cached_line_record_t line_record;
    dcache_probe_record_t probe_record;

    probe_token = '0;
    line_key = line_addr64(line_addr);
    if (!cached_line_by_addr.exists(line_key)) begin
        return 1'b0;
    end
    line_record = cached_line_by_addr[line_key];
    if (probe_owner == DCACHE_PROBE_OWNER_ALIAS_CONFLICT) begin
        if (!line_record.alias_valid ||
            (line_record.lifecycle_state != DCACHE_LINE_ALIAS_CONFLICT)) begin
            return 1'b0;
        end
    end else if (!line_record.alias_valid ||
                 (line_record.lifecycle_state != DCACHE_LINE_ACTIVE)) begin
        return 1'b0;
    end
    if (has_probe_for_line(line_key) || (probe_record_q.size() >= DCACHE_MAX_PROBE_RECORDS)) begin
        return 1'b0;
    end
    if (!(target_cap inside {TL_CAP_TON, TL_CAP_TOB})) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("submit_probe line=0x%0h uses unsupported target_cap=%0d", line_key, target_cap))
    end
    if (next_probe_token == '0) begin
        next_probe_token = 1;
    end
    probe_record             = '{default:'0};
    probe_record.token       = next_probe_token;
    probe_record.batch_id    = batch_id;
    probe_record.line_addr   = line_key;
    probe_record.probe_alias = line_record.active_alias;
    probe_record.target_cap  = target_cap;
    probe_record.owner       = probe_owner;
    probe_record.state       = DCACHE_PROBE_STATE_QUEUED;
    next_probe_token++;
    probe_token = probe_record.token;
    probe_record_q.push_back(probe_record);
    if (probe_owner != DCACHE_PROBE_OWNER_ALIAS_CONFLICT) begin
        line_record.lifecycle_state = DCACHE_LINE_PROBE_PENDING;
        cached_line_by_addr[line_key] = line_record;
    end
    return 1'b1;
endfunction:submit_probe

function void dcache_mem__access_base_sequence::service_probe_b_hold();
    if (probe_b_hold_valid) begin
        return;
    end
    foreach (probe_record_q[i]) begin
        if (probe_record_q[i].state == DCACHE_PROBE_STATE_QUEUED) begin
            probe_record_q[i].state = DCACHE_PROBE_STATE_B_HOLD;
            probe_b_hold_token      = probe_record_q[i].token;
            probe_b_hold_valid      = 1'b1;
            return;
        end
    end
endfunction:service_probe_b_hold

function void dcache_mem__access_base_sequence::build_probe_b_xaction(inout dcache_agent_agent_xaction cycle_xact);
    int probe_index;
    dcache_probe_record_t probe_record;

    if (!probe_b_hold_valid) begin
        return;
    end
    probe_index = find_probe_record_by_token(probe_b_hold_token);
    if ((probe_index < 0) ||
        (probe_record_q[probe_index].state != DCACHE_PROBE_STATE_B_HOLD)) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("B hold token=%0d does not identify a B_HOLD Probe record", probe_b_hold_token))
    end
    probe_record = probe_record_q[probe_index];
    cycle_xact.auto_inner_dcache_client_out_b_valid            = 1'b1;
    cycle_xact.auto_inner_dcache_client_out_b_bits_opcode      = TL_B_OPCODE_PROBE;
    cycle_xact.auto_inner_dcache_client_out_b_bits_param       = probe_record.target_cap;
    cycle_xact.auto_inner_dcache_client_out_b_bits_size        = TL_LINE_SIZE;
    cycle_xact.auto_inner_dcache_client_out_b_bits_source      = 6'd0;
    cycle_xact.auto_inner_dcache_client_out_b_bits_address     = probe_record.line_addr;
    cycle_xact.auto_inner_dcache_client_out_b_bits_mask        = 32'hffff_ffff;
    cycle_xact.auto_inner_dcache_client_out_b_bits_data        = '0;
    cycle_xact.auto_inner_dcache_client_out_b_bits_data[2:1]  = probe_record.probe_alias;
    cycle_xact.auto_inner_dcache_client_out_b_bits_corrupt     = 1'b0;
endfunction:build_probe_b_xaction

function void dcache_mem__access_base_sequence::process_probe_b_fire();
    int probe_index;

    if (!probe_b_hold_valid) begin
        `uvm_fatal(get_type_name(), "B.fire observed without a Probe B hold")
    end
    probe_index = find_probe_record_by_token(probe_b_hold_token);
    if ((probe_index < 0) ||
        (probe_record_q[probe_index].state != DCACHE_PROBE_STATE_B_HOLD)) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("B.fire token=%0d does not identify a B_HOLD Probe record", probe_b_hold_token))
    end
    probe_record_q[probe_index].state = DCACHE_PROBE_STATE_WAIT_C;
    probe_b_hold_valid                = 1'b0;
    probe_b_hold_token                = '0;
endfunction:process_probe_b_fire

task dcache_mem__access_base_sequence::start_alias_conflict(
    input dcache_agent_agent_xaction req_xact,
    input longint unsigned           accept_cycle
);
    bit [47:0] line_addr;
    bit [9:0] reserved_sink;
    dcache_probe_token_t probe_token;
    dcache_cached_line_record_t old_line_record;
    dcache_cached_line_record_t line_record;

    if (!is_alias_conflict_request(req_xact)) begin
        `uvm_fatal(get_type_name(), "start_alias_conflict called for a non-conflicting A request")
    end
    line_addr = line_addr64(req_xact.auto_inner_dcache_client_out_a_bits_address);
    if (has_probe_for_line(line_addr) || !reserve_deferred_acquire_resources(reserved_sink)) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("A.fire alias conflict cannot reserve Probe/Acquire resources line=0x%0h", line_addr))
    end
    old_line_record = cached_line_by_addr[line_addr];
    line_record = old_line_record;
    line_record.deferred_acquire = dcache_agent_agent_xaction::type_id::create("deferred_alias_acquire");
    line_record.deferred_acquire.copy(req_xact);
    line_record.deferred_acquire_valid      = 1'b1;
    line_record.deferred_response_reserved  = 1'b1;
    line_record.deferred_sink               = reserved_sink;
    line_record.deferred_accept_cycle       = accept_cycle;
    line_record.lifecycle_state             = DCACHE_LINE_ALIAS_CONFLICT;
    cached_line_by_addr[line_addr]          = line_record;
    if (!submit_probe(line_addr,
                      TL_CAP_TON,
                      DCACHE_PROBE_OWNER_ALIAS_CONFLICT,
                      0,
                      probe_token)) begin
        cached_line_by_addr[line_addr] = old_line_record;
        release_deferred_acquire_resources(line_record);
        `uvm_fatal(get_type_name(),
                   $sformatf("A.fire alias conflict cannot create Probe record line=0x%0h", line_addr))
    end
endtask:start_alias_conflict

task dcache_mem__access_base_sequence::enqueue_pending_cbo_ack(input longint unsigned complete_cycle);
    dcache_response_record_t response_record;

    if (!cbo_context_valid || !cbo_response_reserved) begin
        `uvm_fatal(get_type_name(), "CBOAck creation requires an active CBO response reservation")
    end
    response_record                = '{default:'0};
    response_record.kind           = DCACHE_PENDING_D_CBO_ACK;
    response_record.eligible_cycle = complete_cycle + 3;
    response_record.beat_count     = 1;
    response_record.size           = TL_LINE_SIZE;
    response_record.source         = pending_cbo_probe_source;
    response_record.line_addr      = pending_cbo_probe_line;
    response_record.cbo_opcode     = pending_cbo_probe_opcode;
    response_record.denied         = pending_cbo_ack_denied;
    response_record.corrupt        = pending_cbo_ack_corrupt;

    // 将 CBO A.fire 时已经占用的一笔容量原子转换为可调度的 D response record。
    cbo_response_reserved = 1'b0;
    enqueue_dcache_response(response_record);
endtask:enqueue_pending_cbo_ack

task dcache_mem__access_base_sequence::complete_cbo_probe(
    input dcache_probe_record_t probe_record,
    input longint unsigned      complete_cycle
);
    if ((probe_record.owner != DCACHE_PROBE_OWNER_CBO) ||
        !cbo_context_valid || !pending_cbo_probe_valid ||
        (probe_record.token != pending_cbo_probe_token) ||
        (probe_record.line_addr != pending_cbo_probe_line) ||
        (probe_record.target_cap != pending_cbo_probe_cap)) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("CBO Probe completion does not match context token=%0d line=0x%0h cap=%0d",
                             probe_record.token,
                             probe_record.line_addr,
                             probe_record.target_cap))
    end
    pending_cbo_probe_valid = 1'b0;
    enqueue_pending_cbo_ack(complete_cycle);
endtask:complete_cbo_probe

task dcache_mem__access_base_sequence::complete_probe_record(
    input int              probe_index,
    input bit              data_response_seen,
    input bit              data_valid,
    input longint unsigned complete_cycle
);
    dcache_probe_record_t probe_record;
    dcache_cached_line_record_t line_record;
    dcache_agent_agent_xaction deferred_acquire;

    if ((probe_index < 0) || (probe_index >= probe_record_q.size())) begin
        `uvm_fatal(get_type_name(), $sformatf("invalid Probe record index=%0d on completion", probe_index))
    end
    probe_record = probe_record_q[probe_index];
    if (!cached_line_by_addr.exists(probe_record.line_addr)) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("Probe token=%0d completed without a line record line=0x%0h",
                             probe_record.token,
                             probe_record.line_addr))
    end
    line_record = cached_line_by_addr[probe_record.line_addr];
    if ((probe_record.owner == DCACHE_PROBE_OWNER_CBO) &&
        (!cbo_context_valid || !pending_cbo_probe_valid ||
         (probe_record.token != pending_cbo_probe_token) ||
         (probe_record.line_addr != pending_cbo_probe_line) ||
         (probe_record.target_cap != pending_cbo_probe_cap))) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("CBO Probe record token=%0d line=0x%0h cannot update an unmatched CBO context",
                             probe_record.token,
                             probe_record.line_addr))
    end
    if (data_response_seen) begin
        line_record.data_valid = data_valid;
    end

    case (probe_record.target_cap)
        TL_CAP_TOB: begin
            bit old_alias_valid;
            old_alias_valid = line_record.alias_valid;
            line_record.alias_valid     = 1'b1;
            line_record.lifecycle_state = DCACHE_LINE_ACTIVE;
            cached_line_by_addr[probe_record.line_addr] = line_record;
            observe_dcache_line_transition(old_alias_valid, line_record.alias_valid);
            probe_record_q.delete(probe_index);
            if (probe_record.owner == DCACHE_PROBE_OWNER_CBO) begin
                complete_cbo_probe(probe_record, complete_cycle);
            end
        end
        TL_CAP_TON: begin
            if (probe_record.owner == DCACHE_PROBE_OWNER_ALIAS_CONFLICT) begin
                if (!line_record.deferred_acquire_valid) begin
                    `uvm_fatal(get_type_name(),
                               $sformatf("alias Probe token=%0d completed without deferred Acquire", probe_record.token))
                end
                deferred_acquire = line_record.deferred_acquire;
                release_deferred_acquire_resources(line_record);
                probe_record_q.delete(probe_index);
                cached_line_by_addr.delete(probe_record.line_addr);
                observe_dcache_line_transition(line_record.alias_valid, 1'b0);
                accept_dcache_a_request(deferred_acquire, complete_cycle);
            end else begin
                probe_record_q.delete(probe_index);
                remove_cached_line(probe_record.line_addr, "probe_toN");
                if (probe_record.owner == DCACHE_PROBE_OWNER_CBO) begin
                    complete_cbo_probe(probe_record, complete_cycle);
                end
            end
        end
        default: begin
            `uvm_fatal(get_type_name(),
                       $sformatf("Probe token=%0d completed with unsupported target_cap=%0d",
                                 probe_record.token,
                                 probe_record.target_cap))
        end
    endcase
endtask:complete_probe_record

function int dcache_mem__access_base_sequence::find_dcache_eligible_response(
    input longint unsigned current_cycle,
    input int unsigned      visible_count
);
    int unsigned eligible_count;
    int unsigned selected_ordinal;
    int unsigned seen;
    int unsigned scan_count;

    scan_count = (visible_count < dcache_rsp_q.size()) ? visible_count : dcache_rsp_q.size();
    if (!seq_csr_common::get_l2_rsp_reorder_en()) begin
        for (int unsigned i = 0; i < scan_count; i++) begin
            if (dcache_rsp_q[i].eligible_cycle <= current_cycle) begin
                return i;
            end
        end
        return -1;
    end

    eligible_count = 0;
    for (int unsigned i = 0; i < scan_count; i++) begin
        if (dcache_rsp_q[i].eligible_cycle <= current_cycle) begin
            eligible_count++;
        end
    end
    if (eligible_count == 0) begin
        return -1;
    end
    if (!std::randomize(selected_ordinal) with { selected_ordinal inside {[0:eligible_count-1]}; }) begin
        `uvm_fatal(get_type_name(), "failed to randomize DCache ready response ordinal")
    end
    seen = 0;
    for (int unsigned i = 0; i < scan_count; i++) begin
        if (dcache_rsp_q[i].eligible_cycle <= current_cycle) begin
            if (seen == selected_ordinal) begin
                return i;
            end
            seen++;
        end
    end
    `uvm_fatal(get_type_name(), "DCache eligible response ordinal was not found")
    return -1;
endfunction:find_dcache_eligible_response

function void dcache_mem__access_base_sequence::service_dcache_response_scheduler(
    input longint unsigned current_cycle,
    input int unsigned      visible_count
);
    int selected_index;
    int unsigned response_delay;
    dcache_response_record_t selected_record;
    dcache_hint_record_t hint_record;

    if (current_d_valid) begin
        return;
    end

    selected_index = find_dcache_eligible_response(current_cycle, visible_count);
    if (!dcache_rsp_timer_active) begin
        if (selected_index < 0) begin
            return;
        end
        response_delay = sample_dcache_response_delay();
        dcache_rsp_timer_active    = 1'b1;
        dcache_rsp_timer_due_cycle = current_cycle + response_delay;
    end

    if (current_cycle < dcache_rsp_timer_due_cycle) begin
        return;
    end
    selected_index = find_dcache_eligible_response(current_cycle, visible_count);
    if (selected_index < 0) begin
        dcache_rsp_timer_active = 1'b0;
        return;
    end
    selected_record = dcache_rsp_q[selected_index];
    dcache_rsp_q.delete(selected_index);
    current_d_record       = selected_record;
    current_d_valid        = 1'b1;
    // Hint 与其所属 GrantData 在同一轮 D response 调度后生效；不能仅因某条
    // record 启动 timer 就提前发送，否则 REORDER 时可能和另一条最终选中的 D response 脱钩。
    if (current_d_record.hint_pending) begin
        hint_record.due_cycle = current_cycle;
        hint_record.source_id = current_d_record.hint_source_id;
        hint_record.isKeyword = current_d_record.hint_isKeyword;
        dcache_hint_q.push_back(hint_record);
        current_d_record.hint_pending = 1'b0;
    end
    dcache_rsp_timer_active = 1'b0;
endfunction:service_dcache_response_scheduler

function void dcache_mem__access_base_sequence::enqueue_dcache_response(
    input dcache_response_record_t response_record
);
    if (!has_dcache_response_capacity()) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("DCache response record overflow count=%0d max=%0d",
                             get_dcache_response_count(),
                             MEMBLOCK_DUT_DCACHE_A_MAX_OUTSTANDING))
    end
    dcache_rsp_q.push_back(response_record);
endfunction:enqueue_dcache_response

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

function bit dcache_mem__access_base_sequence::sample_probe_batch_start();
    int unsigned start_wt;
    bit          start_batch;

    if (!seq_csr_common::get_l2_probe_en()) begin
        return 1'b0;
    end
    start_wt = seq_csr_common::get_l2_probe_pre_start_wt();
    if (start_wt == 0) begin
        return 1'b0;
    end
    if (start_wt >= 10000) begin
        return 1'b1;
    end
    if (!std::randomize(start_batch) with {
            start_batch dist {
                1'b1 := start_wt,
                1'b0 := (10000 - start_wt)
            };
        }) begin
        `uvm_fatal(get_type_name(), "failed to randomize DCache Probe batch start")
    end
    return start_batch;
endfunction:sample_probe_batch_start

function int unsigned dcache_mem__access_base_sequence::sample_probe_batch_count();
    int unsigned count_class;
    int unsigned batch_count;
    int unsigned one_wt;
    int unsigned mid_wt;
    int unsigned large_wt;

    one_wt   = seq_csr_common::get_l2_probe_count_one_wt();
    mid_wt   = seq_csr_common::get_l2_probe_count_mid_wt();
    large_wt = seq_csr_common::get_l2_probe_count_large_wt();
    if (!std::randomize(count_class) with {
            count_class dist {
                0 := one_wt,
                1 := mid_wt,
                2 := large_wt
            };
        }) begin
        `uvm_fatal(get_type_name(), "failed to randomize DCache Probe batch count class")
    end
    case (count_class)
        0: batch_count = 1;
        1: begin
            if (!std::randomize(batch_count) with { batch_count inside {[2:6]}; }) begin
                `uvm_fatal(get_type_name(), "failed to randomize DCache mid Probe batch count")
            end
        end
        2: begin
            if (!std::randomize(batch_count) with { batch_count inside {[7:15]}; }) begin
                `uvm_fatal(get_type_name(), "failed to randomize DCache large Probe batch count")
            end
        end
        default: begin
            `uvm_fatal(get_type_name(), $sformatf("invalid DCache Probe count class=%0d", count_class))
        end
    endcase
    return batch_count;
endfunction:sample_probe_batch_count

function bit [1:0] dcache_mem__access_base_sequence::sample_probe_target_cap();
    int unsigned to_b_wt;
    bit          choose_to_b;

    to_b_wt = seq_csr_common::get_l2_probe_to_b_wt();
    if (to_b_wt == 0) begin
        return TL_CAP_TON;
    end
    if (to_b_wt >= 10000) begin
        return TL_CAP_TOB;
    end
    if (!std::randomize(choose_to_b) with {
            choose_to_b dist {
                1'b1 := to_b_wt,
                1'b0 := (10000 - to_b_wt)
            };
        }) begin
        `uvm_fatal(get_type_name(), "failed to randomize DCache Probe(toB/toN) target")
    end
    return choose_to_b ? TL_CAP_TOB : TL_CAP_TON;
endfunction:sample_probe_target_cap

function bit dcache_mem__access_base_sequence::select_random_cached_line(output bit [47:0] line_addr);
    mem_addr_t    key;
    int unsigned  entry_count;
    int unsigned  ordinal;
    int unsigned  seen;
    dcache_cached_line_record_t line_record;

    line_addr  = '0;
    entry_count = 0;
    foreach (cached_line_by_addr[key]) begin
        line_record = cached_line_by_addr[key];
        if (line_record.alias_valid &&
            (line_record.lifecycle_state == DCACHE_LINE_ACTIVE) &&
            !has_probe_for_line(key)) begin
            entry_count++;
        end
    end
    if (entry_count == 0) begin
        return 1'b0;
    end
    if (!std::randomize(ordinal) with { ordinal inside {[0:entry_count-1]}; }) begin
        `uvm_fatal(get_type_name(), "failed to randomize cached-line probe ordinal")
    end
    if (!cached_line_by_addr.first(key)) begin
        return 1'b0;
    end
    seen = 0;
    do begin
        line_record = cached_line_by_addr[key];
        if (line_record.alias_valid &&
            (line_record.lifecycle_state == DCACHE_LINE_ACTIVE) &&
            !has_probe_for_line(key)) begin
            if (seen == ordinal) begin
                line_addr  = key;
                return 1'b1;
            end
            seen++;
        end
    end while (cached_line_by_addr.next(key));
    `uvm_fatal(get_type_name(), $sformatf("cached_line_by_addr selection failed at ordinal=%0d", ordinal))
    return 1'b0;
endfunction:select_random_cached_line

task dcache_mem__access_base_sequence::accept_dcache_a_request(
    input dcache_agent_agent_xaction req_xact,
    input longint unsigned           accept_cycle
);
    bit [47:0] line_addr;
    bit [255:0] line_data_low;
    bit [255:0] line_data_high;
    bit [9:0]  grant_sink;
    bit [1:0]  probe_cap;
    bit        l2_d_error_needs_candidate;
    bit        l2_d_error_candidate_denied;
    bit        l2_d_error_candidate_corrupt;
    dcache_probe_token_t        probe_token;
    dcache_cached_line_record_t line_record;
    dcache_response_record_t response_record;

    line_addr = line_addr64(req_xact.auto_inner_dcache_client_out_a_bits_address);
    if (!can_accept_dcache_a_request(req_xact)) begin
        `uvm_fatal(get_type_name(), "DCache A.fire occurred without response-record or Grant sink capacity")
    end
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
    if (is_alias_conflict_request(req_xact)) begin
        start_alias_conflict(req_xact, accept_cycle);
        return;
    end

    response_record                = '{default:'0};
    response_record.eligible_cycle = accept_cycle + 3;
    response_record.line_addr      = line_addr;
    response_record.size           = req_xact.auto_inner_dcache_client_out_a_bits_size;
    response_record.source         = req_xact.auto_inner_dcache_client_out_a_bits_source;

    case (req_xact.auto_inner_dcache_client_out_a_bits_opcode)
        TL_A_OPCODE_ACQUIRE_BLOCK: begin
            if (req_xact.auto_inner_dcache_client_out_a_bits_source > 6'd15) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("AcquireBlock source must be within [0:15], got %0d",
                                     req_xact.auto_inner_dcache_client_out_a_bits_source))
            end
            case (req_xact.auto_inner_dcache_client_out_a_bits_param)
                TL_GROW_NTOB: response_record.param = TL_CAP_TOB;
                TL_GROW_NTOT,
                TL_GROW_BTOT: response_record.param = TL_CAP_TOT;
                default: begin
                    `uvm_fatal(get_type_name(),
                               $sformatf("AcquireBlock param=%0d is unsupported",
                                         req_xact.auto_inner_dcache_client_out_a_bits_param))
                end
            endcase
            if (!allocate_grant_sink(grant_sink)) begin
                `uvm_fatal(get_type_name(), "AcquireBlock accepted without an available Grant sink")
            end
            load_grant_line(line_addr, line_data_low, line_data_high);
            response_record.kind           = DCACHE_PENDING_D_GRANT_DATA;
            response_record.beat_count     = 2;
            response_record.beat_idx       = 0;
            response_record.sink           = grant_sink;
            response_record.echo_isKeyword = req_xact.auto_inner_dcache_client_out_a_bits_echo_isKeyword;
            response_record.line_alias     = req_xact.auto_inner_dcache_client_out_a_bits_user_alias;
            response_record.data_low       = line_data_low;
            response_record.data_high      = line_data_high;
            // 中文注释：sticky 模式下先查询同 line 的 live/pending 状态。只有完全
            // 无状态时才采样一次候选并创建 reservation；关闭 sticky 时保持原有每笔
            // response 独立采样行为。两拍 D 和 D.ready hold 始终只搬运该快照。
            if (seq_csr_common::get_l2_d_error_sticky_en()) begin
                if (!prepare_l2_d_error_for_grant(
                        line_addr,
                        $time,
                        {4'b0, response_record.source},
                        1'b0,
                        1'b0,
                        1'b0,
                        response_record.denied,
                        response_record.corrupt,
                        l2_d_error_needs_candidate,
                        response_record.l2_d_error_reservation_generation)) begin
                    `uvm_fatal(get_type_name(),
                               $sformatf("failed to prepare sticky GrantData error line=0x%0h",
                                         line_addr))
                end
                if (l2_d_error_needs_candidate) begin
                    l2_d_error_candidate_denied = sample_d_error_enable(
                        seq_csr_common::get_l2_grantdata_denied_wt(),
                        "DCache GrantData denied"
                    );
                    l2_d_error_candidate_corrupt = l2_d_error_candidate_denied ? 1'b1 :
                        sample_d_error_enable(
                            seq_csr_common::get_l2_grantdata_corrupt_wt(),
                            "DCache GrantData corrupt"
                        );
                    if (!prepare_l2_d_error_for_grant(
                            line_addr,
                            $time,
                            {4'b0, response_record.source},
                            1'b1,
                            l2_d_error_candidate_denied,
                            l2_d_error_candidate_corrupt,
                            response_record.denied,
                            response_record.corrupt,
                            l2_d_error_needs_candidate,
                            response_record.l2_d_error_reservation_generation) ||
                        l2_d_error_needs_candidate) begin
                        `uvm_fatal(get_type_name(),
                                   $sformatf("failed to reserve sticky GrantData error line=0x%0h",
                                             line_addr))
                    end
                end
            end else begin
                response_record.denied = sample_d_error_enable(
                    seq_csr_common::get_l2_grantdata_denied_wt(),
                    "DCache GrantData denied"
                );
                response_record.corrupt = response_record.denied ? 1'b1 : sample_d_error_enable(
                    seq_csr_common::get_l2_grantdata_corrupt_wt(),
                    "DCache GrantData corrupt"
                );
            end
            response_record.hint_pending   = sample_hint_enable();
            response_record.hint_source_id = req_xact.auto_inner_dcache_client_out_a_bits_source[3:0];
            response_record.hint_isKeyword = req_xact.auto_inner_dcache_client_out_a_bits_echo_isKeyword;
        end
        TL_A_OPCODE_ACQUIRE_PERM: begin
            if (req_xact.auto_inner_dcache_client_out_a_bits_source > 6'd15) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("AcquirePerm source must be within [0:15], got %0d",
                                     req_xact.auto_inner_dcache_client_out_a_bits_source))
            end
            case (req_xact.auto_inner_dcache_client_out_a_bits_param)
                TL_GROW_NTOT,
                TL_GROW_BTOT: response_record.param = TL_CAP_TOT;
                default: begin
                    `uvm_fatal(get_type_name(),
                               $sformatf("AcquirePerm param=%0d is unsupported",
                                         req_xact.auto_inner_dcache_client_out_a_bits_param))
                end
            endcase
            if (!allocate_grant_sink(grant_sink)) begin
                `uvm_fatal(get_type_name(), "AcquirePerm accepted without an available Grant sink")
            end
            response_record.kind       = DCACHE_PENDING_D_GRANT;
            response_record.beat_count = 1;
            response_record.sink       = grant_sink;
            response_record.line_alias = req_xact.auto_inner_dcache_client_out_a_bits_user_alias;
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
            if (req_xact.auto_inner_dcache_client_out_a_bits_param != '0) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("CBO param must be 0, got %0d",
                                     req_xact.auto_inner_dcache_client_out_a_bits_param))
            end
            if (cbo_context_valid) begin
                `uvm_fatal(get_type_name(), "CBO A.fire occurred while a prior CBO context is still active")
            end

            // 中文注释：D-error 与 response capacity 均在真实 A.fire 固定；Probe completion
            // 只把 reservation 转成 CBOAck record，不能第二次随机错误位或接受第二笔 CBO。
            clear_cbo_context();
            cbo_context_valid        = 1'b1;
            cbo_response_reserved    = 1'b1;
            pending_cbo_probe_opcode = req_xact.auto_inner_dcache_client_out_a_bits_opcode;
            pending_cbo_probe_line   = line_addr;
            pending_cbo_probe_source = req_xact.auto_inner_dcache_client_out_a_bits_source;
            pending_cbo_ack_denied = sample_d_error_enable(
                seq_csr_common::get_l2_cbo_ack_denied_wt(),
                "DCache CBOAck denied"
            );
            pending_cbo_ack_corrupt = sample_d_error_enable(
                seq_csr_common::get_l2_cbo_ack_corrupt_wt(),
                "DCache CBOAck corrupt"
            );

            if (!cached_line_by_addr.exists(line_addr)) begin
                enqueue_pending_cbo_ack(accept_cycle);
                return;
            end

            line_record = cached_line_by_addr[line_addr];
            if (!line_record.alias_valid ||
                (line_record.lifecycle_state != DCACHE_LINE_ACTIVE) ||
                line_record.deferred_acquire_valid ||
                has_probe_for_line(line_addr)) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("CBO A.fire accepted an unresolved line=0x%0h state=%0d alias_valid=%0d",
                                     line_addr,
                                     line_record.lifecycle_state,
                                     line_record.alias_valid))
            end

            case (req_xact.auto_inner_dcache_client_out_a_bits_opcode)
                TL_A_OPCODE_CBO_CLEAN: begin
                    probe_cap = TL_CAP_TOB;
                end
                TL_A_OPCODE_CBO_FLUSH,
                TL_A_OPCODE_CBO_INVAL: begin
                    probe_cap = TL_CAP_TON;
                end
                default: begin
                    `uvm_fatal(get_type_name(), "unexpected CBO opcode while selecting Probe target")
                end
            endcase
            if (!submit_probe(line_addr,
                              probe_cap,
                              DCACHE_PROBE_OWNER_CBO,
                              0,
                              probe_token)) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("CBO A.fire cannot create Probe record line=0x%0h", line_addr))
            end
            pending_cbo_probe_valid = 1'b1;
            pending_cbo_probe_cap   = probe_cap;
            pending_cbo_probe_token = probe_token;
            return;
        end
        default: begin
            `uvm_fatal(get_type_name(),
                       $sformatf("unsupported DCache coherent A opcode=%0d on dcache responder",
                                 req_xact.auto_inner_dcache_client_out_a_bits_opcode))
        end
    endcase
    enqueue_dcache_response(response_record);
endtask:accept_dcache_a_request

function void dcache_mem__access_base_sequence::build_current_d_xaction(inout dcache_agent_agent_xaction cycle_xact);
    bit [255:0] grant_data;

    if (!current_d_valid) begin
        return;
    end
    cycle_xact.auto_inner_dcache_client_out_d_valid            = 1'b1;
    cycle_xact.auto_inner_dcache_client_out_d_bits_param       = current_d_record.param;
    cycle_xact.auto_inner_dcache_client_out_d_bits_size        = current_d_record.size;
    cycle_xact.auto_inner_dcache_client_out_d_bits_source      = current_d_record.source;
    cycle_xact.auto_inner_dcache_client_out_d_bits_sink        = current_d_record.sink;
    cycle_xact.auto_inner_dcache_client_out_d_bits_denied      = current_d_record.denied;
    cycle_xact.auto_inner_dcache_client_out_d_bits_echo_isKeyword = current_d_record.echo_isKeyword;
    cycle_xact.auto_inner_dcache_client_out_d_bits_corrupt     = current_d_record.corrupt;
    cycle_xact.auto_inner_dcache_client_out_d_bits_data        = '0;

    case (current_d_record.kind)
        DCACHE_PENDING_D_GRANT: begin
            cycle_xact.auto_inner_dcache_client_out_d_bits_opcode = TL_D_OPCODE_GRANT;
        end
        DCACHE_PENDING_D_GRANT_DATA: begin
            cycle_xact.auto_inner_dcache_client_out_d_bits_opcode = TL_D_OPCODE_GRANT_DATA;
            if (current_d_record.beat_idx == 0) begin
                grant_data = current_d_record.echo_isKeyword ? current_d_record.data_high : current_d_record.data_low;
            end else begin
                grant_data = current_d_record.echo_isKeyword ? current_d_record.data_low : current_d_record.data_high;
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
            `uvm_fatal(get_type_name(), $sformatf("unexpected current D kind=%0d", current_d_record.kind))
        end
    endcase
endfunction:build_current_d_xaction

function void dcache_mem__access_base_sequence::process_d_fire();
    dcache_response_record_t completed_record;
    dcache_grant_ack_record_t grant_ack_record;

    if (!current_d_valid) begin
        `uvm_fatal(get_type_name(), "D.fire observed without a current DCache response record")
    end
    if ((current_d_record.kind == DCACHE_PENDING_D_GRANT_DATA) &&
        ((current_d_record.beat_idx + 1) < current_d_record.beat_count)) begin
        current_d_record.beat_idx++;
        return;
    end

    completed_record = current_d_record;
    case (completed_record.kind)
        DCACHE_PENDING_D_GRANT_DATA: begin
            commit_l2_d_error_on_grant_d_fire(
                completed_record.line_addr,
                completed_record.denied,
                completed_record.corrupt,
                $time,
                {4'b0, completed_record.source},
                completed_record.l2_d_error_reservation_generation
            );
            grant_ack_record.line_addr = completed_record.line_addr;
            grant_ack_record.line_alias = completed_record.line_alias;
            grant_ack_record.sink      = completed_record.sink;
            grant_ack_wait_q.push_back(grant_ack_record);
            mark_cached_line_grant_wait(completed_record.line_addr, completed_record.line_alias);
        end
        DCACHE_PENDING_D_GRANT: begin
            grant_ack_record.line_addr = completed_record.line_addr;
            grant_ack_record.line_alias = completed_record.line_alias;
            grant_ack_record.sink      = completed_record.sink;
            grant_ack_wait_q.push_back(grant_ack_record);
            mark_cached_line_grant_wait(completed_record.line_addr, completed_record.line_alias);
        end
        DCACHE_PENDING_D_CBO_ACK: begin
            if (!cbo_context_valid || cbo_response_reserved || pending_cbo_probe_valid ||
                (completed_record.cbo_opcode != pending_cbo_probe_opcode) ||
                (completed_record.line_addr != pending_cbo_probe_line) ||
                (completed_record.source != pending_cbo_probe_source)) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("CBOAck D.fire does not match active CBO context source=%0d line=0x%0h opcode=%0d",
                                     completed_record.source,
                                     completed_record.line_addr,
                                     completed_record.cbo_opcode))
            end
            // CBO hit 的 toB/toN line 更新已在 Probe C 完成时完成；direct miss 没有 line 可删除。
            clear_cbo_context();
        end
        DCACHE_PENDING_D_RELEASE_ACK: begin
        end
        default: begin
            `uvm_fatal(get_type_name(), $sformatf("process_d_fire with invalid kind=%0d", completed_record.kind))
        end
    endcase
    clear_current_d_state();
endfunction:process_d_fire

function void dcache_mem__access_base_sequence::process_e_fire();
    bit [9:0] observed_sink;

    if (grant_ack_wait_q.size() == 0) begin
        `uvm_fatal(get_type_name(), "unexpected E.valid when no GrantAck is pending")
    end
    if ($isunknown(dcache_vif.drv_cb.auto_inner_dcache_client_out_e_bits_sink)) begin
        `uvm_fatal(get_type_name(), "GrantAck E.bits.sink sampled as X/Z on E.fire")
    end
    observed_sink = dcache_vif.drv_cb.auto_inner_dcache_client_out_e_bits_sink;
    foreach (grant_ack_wait_q[i]) begin
        if (grant_ack_wait_q[i].sink == observed_sink) begin
            record_cached_line(grant_ack_wait_q[i].line_addr, grant_ack_wait_q[i].line_alias);
            grant_ack_wait_q.delete(i);
            return;
        end
    end
    `uvm_fatal(get_type_name(),
               $sformatf("GrantAck sink=%0d does not match any pending Grant owner", observed_sink))
endfunction:process_e_fire

task dcache_mem__access_base_sequence::complete_probe_c_assembly(input longint unsigned complete_cycle);
    bit corrupt;
    bit denied;
    bit [255:0] load_data_unused;
    int probe_index;

    probe_index = find_probe_record_by_token(c_assembly_probe_token);
    if ((probe_index < 0) ||
        (probe_record_q[probe_index].state != DCACHE_PROBE_STATE_C_ASSEMBLY)) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("ProbeAckData completion cannot find C assembly token=%0d", c_assembly_probe_token))
    end
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
    else begin
        observe_dcache_corrupt_line(c_assembly_line);
        `uvm_error(get_type_name(),
                   $sformatf("ProbeAckData is corrupt; skip writeback but still close Probe token=%0d line=0x%0h",
                             c_assembly_probe_token, c_assembly_line))
    end
    complete_probe_record(probe_index,
                          1'b1,
                          !c_assembly_corrupt_seen,
                          complete_cycle);
    observe_dcache_c_assembly_complete();
    clear_c_assembly_state();
endtask:complete_probe_c_assembly

task dcache_mem__access_base_sequence::complete_release_c_assembly(input longint unsigned complete_cycle);
    bit corrupt;
    bit denied;
    bit [255:0] load_data_unused;
    dcache_response_record_t response_record;

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
    else begin
        observe_dcache_corrupt_line(c_assembly_line);
    end
    remove_cached_line(c_assembly_line, "release_or_writeback");
    if (!c_assembly_response_reserved) begin
        `uvm_fatal(get_type_name(), "ReleaseData completed without a reserved ReleaseAck response slot")
    end
    response_record                = '{default:'0};
    response_record.kind           = DCACHE_PENDING_D_RELEASE_ACK;
    response_record.eligible_cycle = complete_cycle + 3;
    response_record.beat_count     = 1;
    response_record.param          = '0;
    response_record.size           = c_assembly_size;
    response_record.source         = c_assembly_source;
    response_record.sink           = '0;
    response_record.line_addr      = c_assembly_line;
    // 将 assembly reservation 原子转换为真正 record；它不应额外占用第二个 capacity。
    c_assembly_response_reserved = 1'b0;
    enqueue_dcache_response(response_record);
    observe_dcache_c_assembly_complete();
    clear_c_assembly_state();
endtask:complete_release_c_assembly

task dcache_mem__access_base_sequence::consume_c_beat(
    input dcache_agent_agent_xaction c_req_xact,
    input longint unsigned           accept_cycle
);
    int probe_index;

    if (c_assembly_owner == DCACHE_C_OWNER_NONE) begin
        `uvm_fatal(get_type_name(), "consume_c_beat called without active C assembly")
    end
    if (c_assembly_owner == DCACHE_C_OWNER_PROBE) begin
        probe_index = find_probe_record_by_token(c_assembly_probe_token);
        if ((probe_index < 0) ||
            (probe_record_q[probe_index].state != DCACHE_PROBE_STATE_C_ASSEMBLY)) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("C beat does not belong to an active Probe assembly token=%0d",
                                 c_assembly_probe_token))
        end
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
    int probe_index;
    dcache_probe_record_t probe_record;

    line_addr = line_addr64(c_req_xact.auto_inner_dcache_client_out_c_bits_address);

    // 同一 physical line 的 Probe owner 尚未收到 C reply 时，Release/ReleaseData 若先删除
    // line record，会让随后到来的 ProbeAck/Data 失去唯一生命周期 owner。轻量模型没有合并
    // 两类同线 C transaction 的 directory 语义，因此明确 fail-fast，而不同 line 仍可并行收敛。
    if ((c_req_xact.auto_inner_dcache_client_out_c_bits_opcode inside {
            TL_C_OPCODE_RELEASE,
            TL_C_OPCODE_RELEASEDATA
        }) && has_probe_for_line(line_addr)) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("Release/ReleaseData conflicts with an active Probe owner line=0x%0h",
                             line_addr))
    end

    case (c_req_xact.auto_inner_dcache_client_out_c_bits_opcode)
        TL_C_OPCODE_PROBE_ACK: begin
            probe_index = find_waiting_probe_record_by_line(line_addr);
            if (probe_index < 0) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("ProbeAck arrived without a unique pending Probe owner line=0x%0h", line_addr))
            end
            probe_record = probe_record_q[probe_index];
            if (line_addr != probe_record.line_addr) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("ProbeAck line mismatch expected=0x%0h got=0x%0h",
                                     probe_record.line_addr, line_addr))
            end
            if (c_req_xact.auto_inner_dcache_client_out_c_bits_size != TL_LINE_SIZE) begin
                `uvm_fatal(get_type_name(), "ProbeAck size must be 6")
            end
            check_probe_response_param(probe_record,
                                       c_req_xact.auto_inner_dcache_client_out_c_bits_param,
                                       "ProbeAck");
            complete_probe_record(probe_index, 1'b0, 1'b0, accept_cycle);
        end
        TL_C_OPCODE_PROBE_ACKDATA: begin
            probe_index = find_waiting_probe_record_by_line(line_addr);
            if (probe_index < 0) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("ProbeAckData arrived without a unique pending Probe owner line=0x%0h", line_addr))
            end
            probe_record = probe_record_q[probe_index];
            if (line_addr != probe_record.line_addr) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("ProbeAckData line mismatch expected=0x%0h got=0x%0h",
                                     probe_record.line_addr, line_addr))
            end
            if (c_req_xact.auto_inner_dcache_client_out_c_bits_size != TL_LINE_SIZE) begin
                `uvm_fatal(get_type_name(), "ProbeAckData size must be 6")
            end
            check_probe_response_param(probe_record,
                                       c_req_xact.auto_inner_dcache_client_out_c_bits_param,
                                       "ProbeAckData");
            clear_c_assembly_state();
            c_assembly_owner          = DCACHE_C_OWNER_PROBE;
            c_assembly_opcode         = c_req_xact.auto_inner_dcache_client_out_c_bits_opcode;
            c_assembly_line           = line_addr;
            c_assembly_source         = c_req_xact.auto_inner_dcache_client_out_c_bits_source;
            c_assembly_size           = c_req_xact.auto_inner_dcache_client_out_c_bits_size;
            c_assembly_param          = c_req_xact.auto_inner_dcache_client_out_c_bits_param;
            c_assembly_probe_token    = probe_record.token;
            probe_record_q[probe_index].state = DCACHE_PROBE_STATE_C_ASSEMBLY;
            observe_dcache_c_assembly_start();
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
            begin
                dcache_response_record_t response_record;
                if (!has_dcache_response_capacity()) begin
                    `uvm_fatal(get_type_name(), "Release C.fire occurred without ReleaseAck response capacity")
                end
                response_record                = '{default:'0};
                response_record.kind           = DCACHE_PENDING_D_RELEASE_ACK;
                response_record.eligible_cycle = accept_cycle + 3;
                response_record.beat_count     = 1;
                response_record.param          = '0;
                response_record.size           = c_req_xact.auto_inner_dcache_client_out_c_bits_size;
                response_record.source         = c_req_xact.auto_inner_dcache_client_out_c_bits_source;
                response_record.sink           = '0;
                response_record.line_addr      = line_addr;
                enqueue_dcache_response(response_record);
            end
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
            if (!has_dcache_response_capacity()) begin
                `uvm_fatal(get_type_name(), "ReleaseData C.fire occurred without a reservable ReleaseAck response slot")
            end
            c_assembly_response_reserved = 1'b1;
            observe_dcache_c_assembly_start();
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
    bit [1:0]  target_cap;
    dcache_probe_token_t probe_token;
    longint unsigned batch_id;
    int unsigned batch_target;
    int unsigned created_count;

    // 中文注释：随机 policy 只在没有其它 Probe owner 的空闲窗口建立一个完整 batch。
    // 每个选中 line 立即转为独立 record，因此同一 batch 不会重复；B/C 发送和收敛仍由共享 service 接管。
    if (!allow_new_probe || current_d_valid || (dcache_rsp_q.size() != 0) ||
        (grant_ack_wait_q.size() != 0) ||
        cbo_context_valid || cbo_response_reserved || pending_cbo_probe_valid ||
        (probe_record_q.size() != 0) || probe_b_hold_valid || has_waiting_probe_c() ||
        (c_assembly_owner != DCACHE_C_OWNER_NONE) ||
        a_accept_armed || c_accept_armed) begin
        return;
    end
    if (!sample_probe_batch_start()) begin
        return;
    end
    if (next_probe_batch_id == '0) begin
        next_probe_batch_id = 1;
    end
    batch_id     = next_probe_batch_id;
    batch_target = sample_probe_batch_count();
    created_count = 0;
    for (int unsigned i = 0;
         (i < batch_target) && (probe_record_q.size() < DCACHE_MAX_PROBE_RECORDS);
         i++) begin
        if (!select_random_cached_line(selected_line)) begin
            break;
        end
        target_cap = sample_probe_target_cap();
        if (!submit_probe(selected_line,
                          target_cap,
                          DCACHE_PROBE_OWNER_RANDOM,
                          batch_id,
                          probe_token)) begin
            if (probe_record_q.size() >= DCACHE_MAX_PROBE_RECORDS) begin
                break;
            end
            `uvm_fatal(get_type_name(),
                       $sformatf("random Probe batch=%0d cannot create record line=0x%0h",
                                 batch_id,
                                 selected_line))
        end
        created_count++;
    end
    if (created_count != 0) begin
        next_probe_batch_id++;
        service_probe_b_hold();
    end
endfunction:try_start_probe

function bit dcache_mem__access_base_sequence::is_l2_flush_drain_complete();
    return !current_d_valid &&
           (dcache_rsp_q.size() == 0) &&
           !dcache_rsp_timer_active &&
           (grant_ack_wait_q.size() == 0) &&
           (dcache_hint_q.size() == 0) &&
           !cbo_context_valid &&
           !cbo_response_reserved &&
           !pending_cbo_probe_valid &&
           !probe_b_hold_valid &&
           (probe_record_q.size() == 0) &&
           (c_assembly_owner == DCACHE_C_OWNER_NONE) &&
           !c_assembly_response_reserved &&
           !a_accept_armed &&
           !c_accept_armed;
endfunction:is_l2_flush_drain_complete

function bit dcache_mem__access_base_sequence::has_flush_probe_record();
    foreach (probe_record_q[i]) begin
        if (probe_record_q[i].owner == DCACHE_PROBE_OWNER_FLUSH) begin
            return 1'b1;
        end
    end
    return 1'b0;
endfunction:has_flush_probe_record

function void dcache_mem__access_base_sequence::capture_l2_flush_snapshot();
    mem_addr_t key;
    dcache_cached_line_record_t line_record;

    // 中文注释：snapshot 只在 DRAIN 完成的低频边界扫描一次；之后新 Grant 不会被回填进本轮 flush。
    l2_flush_snapshot_line_q.delete();
    foreach (cached_line_by_addr[key]) begin
        line_record = cached_line_by_addr[key];
        if (line_record.alias_valid &&
            (line_record.lifecycle_state == DCACHE_LINE_ACTIVE)) begin
            l2_flush_snapshot_line_q.push_back(key);
        end
    end
endfunction:capture_l2_flush_snapshot

function void dcache_mem__access_base_sequence::service_l2_flush(input bit sampled_l2_flush_en);
    bit [47:0] flush_line;
    dcache_probe_token_t probe_token;

    case (l2_flush_state)
        DCACHE_L2_FLUSH_IDLE: begin
            if (sampled_l2_flush_en) begin
                l2_flush_state = DCACHE_L2_FLUSH_DRAIN;
            end
        end
        DCACHE_L2_FLUSH_DRAIN: begin
            if (!sampled_l2_flush_en) begin
                `uvm_fatal(get_type_name(), "L2 flush request was withdrawn before DRAIN completed")
            end
            if (is_l2_flush_drain_complete()) begin
                capture_l2_flush_snapshot();
                l2_flush_state = DCACHE_L2_FLUSH_PROBE;
            end
        end
        DCACHE_L2_FLUSH_PROBE: begin
            if (!sampled_l2_flush_en) begin
                `uvm_fatal(get_type_name(), "L2 flush request was withdrawn before Probe snapshot completed")
            end
            if ((l2_flush_snapshot_line_q.size() != 0) &&
                (probe_record_q.size() < DCACHE_MAX_PROBE_RECORDS)) begin
                flush_line = l2_flush_snapshot_line_q[0];
                if (!submit_probe(flush_line,
                                  TL_CAP_TON,
                                  DCACHE_PROBE_OWNER_FLUSH,
                                  0,
                                  probe_token)) begin
                    `uvm_fatal(get_type_name(),
                               $sformatf("L2 flush cannot create Probe(toN) for snapshot line=0x%0h", flush_line))
                end
                l2_flush_snapshot_line_q.delete(0);
            end
            if ((l2_flush_snapshot_line_q.size() == 0) &&
                !has_flush_probe_record() &&
                !probe_b_hold_valid &&
                (c_assembly_owner == DCACHE_C_OWNER_NONE)) begin
                l2_flush_state = DCACHE_L2_FLUSH_DONE;
            end
        end
        DCACHE_L2_FLUSH_DONE: begin
            if (!sampled_l2_flush_en) begin
                l2_flush_snapshot_line_q.delete();
                l2_flush_state = DCACHE_L2_FLUSH_IDLE;
            end
        end
        default: begin
            `uvm_fatal(get_type_name(), $sformatf("invalid L2 flush state=%0d", l2_flush_state))
        end
    endcase
endfunction:service_l2_flush

function bit dcache_mem__access_base_sequence::l2_flush_blocks_a_request(input bit sampled_l2_flush_en);
    case (l2_flush_state)
        DCACHE_L2_FLUSH_DRAIN,
        DCACHE_L2_FLUSH_PROBE: return 1'b1;
        DCACHE_L2_FLUSH_IDLE:  return sampled_l2_flush_en;
        DCACHE_L2_FLUSH_DONE:  return 1'b0;
        default: begin
            `uvm_fatal(get_type_name(), $sformatf("invalid L2 flush state=%0d", l2_flush_state))
        end
    endcase
    return 1'b1;
endfunction:l2_flush_blocks_a_request

function void dcache_mem__access_base_sequence::service_hint(
    input longint unsigned           current_cycle,
    inout dcache_agent_agent_xaction cycle_xact
);
    foreach (dcache_hint_q[i]) begin
        if (dcache_hint_q[i].due_cycle <= current_cycle) begin
            cycle_xact.io_l2_hint_valid          = 1'b1;
            cycle_xact.io_l2_hint_bits_sourceId  = dcache_hint_q[i].source_id;
            cycle_xact.io_l2_hint_bits_isKeyword = dcache_hint_q[i].isKeyword;
            dcache_hint_q.delete(i);
            return;
        end
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
    logic                      sampled_l2_flush_en_raw;
    bit                        sampled_a_valid;
    bit                        sampled_b_ready;
    bit                        sampled_c_valid;
    bit                        sampled_d_ready;
    bit                        sampled_e_valid;
    bit                        sampled_l2_flush_en;
    bit                        reset_active;
    bit                        a_fire;
    bit                        b_fire;
    bit                        c_fire;
    bit                        d_fire;
    bit                        e_fire;
    int unsigned               stop_wait_cycles;
    int unsigned               response_visible_count;

    if (!uvm_config_db#(virtual dcache_agent_agent_interface)::get(null, get_full_name(), "vif", dcache_vif) &&
        !uvm_config_db#(virtual dcache_agent_agent_interface)::get(null, "uvm_test_top.env.u_dcache_agent_agent*", "vif", dcache_vif)) begin
        `uvm_fatal(get_type_name(), "dcache virtual interface is not set for memory access sequence")
    end
    if (!uvm_config_db#(virtual other_ctrl_agent_agent_interface)::get(null, get_full_name(), "other_ctrl_vif", other_ctrl_vif) &&
        !uvm_config_db#(virtual other_ctrl_agent_agent_interface)::get(null, "uvm_test_top.env.u_other_ctrl_agent_agent*", "vif", other_ctrl_vif)) begin
        `uvm_fatal(get_type_name(), "other_ctrl virtual interface is not set for DCache L2 flush responder")
    end
    data = common_data_transaction::get();
    if (data == null) begin
        `uvm_fatal(get_type_name(), "failed to get common_data_transaction for DCache responder")
    end

    memblock_sync_pkg::dcache_responder_done = 1'b0;

    seq_csr_common::init();
    // 中文注释：real-smoke 由 virtual sequence 在 fork responder 前初始化 shared store。
    // legacy default topology 没有该入口时才在 DCache 首次启动兜底，避免两个 responder 分别清空同一份状态。
    if (!is_shared_memory_lifecycle_initialized()) begin
        initialize_shared_memory_state(seq_csr_common::get_main_mem_ranges_en(),
                                       mem_addr_t'(seq_csr_common::get_paddr_base()),
                                       seq_csr_common::get_paddr_range());
    end
    if (!claim_dcache_observer_owner()) begin
        `uvm_error(get_type_name(),
                   "DCache observer owner claim failed; responder will not publish RM aggregate state")
        return;
    end
    check_l2_model_cfg();
    service_cycle    = 0;
    last_drive_cycle = 0;
    stop_wait_cycles = 0;
    clear_runtime_state(1'b1);

    forever begin
        // 中文注释：上一轮 item 已在前一个 drv_cb 边界更新到 clocking output；
        // 当前边界先推进 shared-store 采样代次，使上一拍已确认的写即使后续没有新 memory
        // access 也会稳定提交；随后再采样真实握手并提交下一周期 item。
        @(dcache_vif.drv_cb);
        begin_shared_mem_sample($time);
        sampled_a_valid_raw = dcache_vif.drv_cb.auto_inner_dcache_client_out_a_valid;
        sampled_b_ready_raw = dcache_vif.drv_cb.auto_inner_dcache_client_out_b_ready;
        sampled_c_valid_raw = dcache_vif.drv_cb.auto_inner_dcache_client_out_c_valid;
        sampled_d_ready_raw = dcache_vif.drv_cb.auto_inner_dcache_client_out_d_ready;
        sampled_e_valid_raw = dcache_vif.drv_cb.auto_inner_dcache_client_out_e_valid;
        sampled_l2_flush_en_raw = other_ctrl_vif.mon_mp.mon_cb.io_outer_l2_flush_en;
        reset_active    = (dcache_vif.rst_n !== 1'b1) || (memblock_sync_pkg::reset_backend_done !== 1'b1);
        sampled_a_valid = (sampled_a_valid_raw === 1'b1);
        sampled_b_ready = (sampled_b_ready_raw === 1'b1);
        sampled_c_valid = (sampled_c_valid_raw === 1'b1);
        sampled_d_ready = (sampled_d_ready_raw === 1'b1);
        sampled_e_valid = (sampled_e_valid_raw === 1'b1);
        sampled_l2_flush_en = (sampled_l2_flush_en_raw === 1'b1);
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
             (sampled_e_valid_raw !== 1'b0 && sampled_e_valid_raw !== 1'b1) ||
             (sampled_l2_flush_en_raw !== 1'b0 && sampled_l2_flush_en_raw !== 1'b1))) begin
            `uvm_fatal(get_type_name(),
                       "DCache channel valid/ready or L2 flush request sampled as X/Z outside reset")
        end

        build_dcache_idle_xaction(cycle_xact);

        if (reset_active) begin
            clear_runtime_state(1'b1);
            invalidate_dcache_runtime_observer();
            send_dcache_xaction(cycle_xact);
            last_drive_cycle = service_cycle;
            service_cycle++;
            continue;
        end

        if (!mem_access_base_sequence::dcache_aggregate_snapshot.published) begin
            publish_dcache_owner_baseline();
        end

        // 本拍返回调度只能看见进入本拍前已经存在的 record；本拍 A/C.fire 新建的
        // record 会在后续周期才允许被 scheduler 选择。
        response_visible_count = dcache_rsp_q.size();

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
            if (sampled_e_valid && !e_fire && (grant_ack_wait_q.size() == 0)) begin
                `uvm_fatal(get_type_name(), "E.valid observed without a pending GrantAck owner")
            end

            if (c_fire) begin
                if (!c_accept_armed || (armed_c_req_xact == null)) begin
                    `uvm_fatal(get_type_name(), "C.fire observed without an armed C snapshot")
                end
                capture_dcache_c_xaction(fired_c_req_xact);
                check_c_payload_stable(armed_c_req_xact, fired_c_req_xact);
                // 中文注释：C.fire 的状态推进必须消费当前 sample 已完成四态检查的 fired snapshot。
                // armed snapshot 只用于 valid 等待 ready 期间的稳定性比较，不能作为最终 data/corrupt 的写回来源。
                if (c_assembly_owner == DCACHE_C_OWNER_NONE) begin
                    start_c_assembly(fired_c_req_xact, last_drive_cycle);
                end else begin
                    consume_c_beat(fired_c_req_xact, last_drive_cycle);
                end
                c_accept_armed = 1'b0;
                armed_c_req_xact = null;
            end else if (c_accept_armed && !sampled_c_valid) begin
                c_accept_armed = 1'b0;
                armed_c_req_xact = null;
            end

            if (b_fire) begin
                process_probe_b_fire();
            end

            if (a_fire) begin
                if (!a_accept_armed || (armed_a_req_xact == null)) begin
                    `uvm_fatal(get_type_name(), "A.fire observed without an armed A snapshot")
                end
                capture_dcache_a_xaction(fired_a_req_xact);
                check_a_payload_stable(armed_a_req_xact, fired_a_req_xact);
                // A.fire 的建表和 alias 判断消费当前 sample 的完整 payload；armed snapshot
                // 只负责 valid 等待 ready 期间的稳定性检查，不能成为后续 owner 的数据来源。
                accept_dcache_a_request(fired_a_req_xact, last_drive_cycle);
                a_accept_armed = 1'b0;
                armed_a_req_xact = null;
            end else if (a_accept_armed && !sampled_a_valid) begin
                a_accept_armed = 1'b0;
                armed_a_req_xact = null;
            end
        end

        // 中文注释：flush request 是 level sideband。先结算上一拍已 fire 的 A/C/D/E/B，
        // 再推进本地 flush 状态，保证 request 到来前已经接受的 owner 自然 drain，之后才关闭新 A.ready。
        service_l2_flush(sampled_l2_flush_en);

        // global stop 表示主表已经全部终态；只允许本拍已经通过上一 item
        // ready 形成的 A.fire 进入 drain。stop 后新出现、未握手的 A 请求没有
        // 合法 owner，直接报错，避免重新打开 A.ready 或永久等待。
        if (data.is_global_stop_requested() &&
            (l2_flush_state == DCACHE_L2_FLUSH_IDLE) &&
            !sampled_l2_flush_en &&
            sampled_a_valid && !a_fire) begin
            `uvm_fatal(get_type_name(),
                       "new DCache A.valid observed after global stop without a sampled fire")
        end

        // 中文注释：global stop 不能抢退有 inflight 的 L2 responder。
        // 只有 A/C/B/D/E、GrantAck、Probe 和 assembly 生命周期都自然归零后，才发最后一拍 safe idle 并退出；
        // 已完成 GrantAck 的 cached line map 是稳定历史状态，不属于 in-flight，也不阻塞退出。
        if (data.is_global_stop_requested() &&
            (l2_flush_state == DCACHE_L2_FLUSH_IDLE) &&
            !sampled_l2_flush_en &&
            !current_d_valid &&
            (dcache_rsp_q.size() == 0) &&
            !dcache_rsp_timer_active &&
            (grant_ack_wait_q.size() == 0) &&
            (dcache_hint_q.size() == 0) &&
            !cbo_context_valid &&
            !cbo_response_reserved &&
            !pending_cbo_probe_valid &&
            !probe_b_hold_valid &&
            (probe_record_q.size() == 0) &&
            (c_assembly_owner == DCACHE_C_OWNER_NONE) &&
            !c_assembly_response_reserved &&
            !a_accept_armed &&
            !c_accept_armed &&
            !sampled_a_valid &&
            !sampled_c_valid) begin
            `uvm_info(get_type_name(),
                      $sformatf("DCache responder draining complete at service_cycle=%0d cached_lines=%0d",
                                service_cycle, cached_line_by_addr.num()),
                      UVM_LOW)
            send_dcache_xaction(cycle_xact);
            last_cycle_xact  = cycle_xact;
            last_cycle_valid = 1'b1;
            last_drive_cycle = service_cycle;
            service_cycle++;
            memblock_sync_pkg::dcache_responder_done = 1'b1;
            release_dcache_observer_owner();
            `uvm_info(get_type_name(), "DCache responder published terminal idle and stopped", UVM_LOW)
            break;
        end
        else if (data.is_global_stop_requested()) begin
            stop_wait_cycles++;
            if ((stop_wait_cycles % 1000) == 0) begin
                `uvm_warning(get_type_name(),
                             $sformatf("DCache responder still draining after global stop: cycles=%0d current_d=%0d queued_rsp=%0d timer=%0d grant_ack=%0d hint=%0d cbo_ctx=%0d cbo_resv=%0d cbo_probe=%0d probe_hold=%0d probe_records=%0d c_owner=%0d c_resv=%0d a_armed=%0d c_armed=%0d a_valid=%0d c_valid=%0d",
                                       stop_wait_cycles,
                                       current_d_valid,
                                       dcache_rsp_q.size(),
                                       dcache_rsp_timer_active,
                                       grant_ack_wait_q.size(),
                                       dcache_hint_q.size(),
                                       cbo_context_valid,
                                       cbo_response_reserved,
                                       pending_cbo_probe_valid,
                                       probe_b_hold_valid,
                                       probe_record_q.size(),
                                       c_assembly_owner,
                                       c_assembly_response_reserved,
                                       a_accept_armed,
                                       c_accept_armed,
                                       sampled_a_valid,
                                       sampled_c_valid))
            end
        end
        else begin
            stop_wait_cycles = 0;
        end

        service_dcache_response_scheduler(service_cycle, response_visible_count);
        if (current_d_valid) begin
            build_current_d_xaction(cycle_xact);
        end
        if (grant_ack_wait_q.size() != 0) begin
            cycle_xact.auto_inner_dcache_client_out_e_ready = 1'b1;
        end

        // 已建立的 Probe record 由单一 B hold 驱动。C assembly 和已到达的 C reply
        // 优先于新的 B launch，避免两拍 ProbeAckData 被另一个 Probe 抢占。
        service_probe_b_hold();
        // 当前采样拍已经 fire 的 A/C payload 不能在状态更新后再次被当作新请求 arm。
        if (!c_fire && (c_assembly_owner != DCACHE_C_OWNER_NONE)) begin
            if (sampled_c_valid) begin
                capture_dcache_c_xaction(sampled_req_xact);
                cycle_xact.auto_inner_dcache_client_out_c_ready = 1'b1;
                c_accept_armed = 1'b1;
                armed_c_req_xact = sampled_req_xact;
            end
        end
        else if (!c_fire && has_waiting_probe_c()) begin
            if (sampled_c_valid) begin
                capture_dcache_c_xaction(sampled_req_xact);
                case (sampled_req_xact.auto_inner_dcache_client_out_c_bits_opcode)
                    TL_C_OPCODE_PROBE_ACK,
                    TL_C_OPCODE_PROBE_ACKDATA,
                    TL_C_OPCODE_RELEASE,
                    TL_C_OPCODE_RELEASEDATA: begin
                        if (can_accept_dcache_release_c_request(sampled_req_xact)) begin
                            cycle_xact.auto_inner_dcache_client_out_c_ready = 1'b1;
                            c_accept_armed = 1'b1;
                            armed_c_req_xact = sampled_req_xact;
                        end
                    end
                    default: begin
                        `uvm_fatal(get_type_name(),
                                   $sformatf("pending Probe records only accept ProbeAck/Data or Release/Data, got opcode=%0d",
                                             sampled_req_xact.auto_inner_dcache_client_out_c_bits_opcode))
                    end
                endcase
            end
            else if (probe_b_hold_valid) begin
                build_probe_b_xaction(cycle_xact);
            end
        end
        else if (!c_fire && sampled_c_valid) begin
            capture_dcache_c_xaction(sampled_req_xact);
            case (sampled_req_xact.auto_inner_dcache_client_out_c_bits_opcode)
                TL_C_OPCODE_RELEASE,
                TL_C_OPCODE_RELEASEDATA: begin
                    if (can_accept_dcache_release_c_request(sampled_req_xact)) begin
                        cycle_xact.auto_inner_dcache_client_out_c_ready = 1'b1;
                        c_accept_armed = 1'b1;
                        armed_c_req_xact = sampled_req_xact;
                    end
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
        else if (probe_b_hold_valid) begin
            build_probe_b_xaction(cycle_xact);
        end
        else if (!a_fire && !data.is_global_stop_requested() && sampled_a_valid &&
                 !l2_flush_blocks_a_request(sampled_l2_flush_en)) begin
            capture_dcache_a_xaction(sampled_req_xact);
            case (sampled_req_xact.auto_inner_dcache_client_out_a_bits_opcode)
                TL_A_OPCODE_ACQUIRE_BLOCK,
                TL_A_OPCODE_ACQUIRE_PERM,
                TL_A_OPCODE_CBO_CLEAN,
                TL_A_OPCODE_CBO_FLUSH,
                TL_A_OPCODE_CBO_INVAL: begin
                    if (can_accept_dcache_a_request(sampled_req_xact)) begin
                        cycle_xact.auto_inner_dcache_client_out_a_ready = 1'b1;
                        a_accept_armed = 1'b1;
                        armed_a_req_xact = sampled_req_xact;
                    end
                end
                default: begin
                    `uvm_fatal(get_type_name(),
                               $sformatf("unsupported DCache coherent A opcode=%0d before accept",
                                         sampled_req_xact.auto_inner_dcache_client_out_a_bits_opcode))
                end
            endcase
        end
        else begin
            try_start_probe(!data.is_global_stop_requested() &&
                            (l2_flush_state == DCACHE_L2_FLUSH_IDLE) &&
                            !sampled_l2_flush_en);
            if (probe_b_hold_valid) begin
                build_probe_b_xaction(cycle_xact);
            end
        end

        service_hint(service_cycle, cycle_xact);
        cycle_xact.io_l2_flush_done = (l2_flush_state == DCACHE_L2_FLUSH_DONE);
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

    localparam bit [3:0] UNCACHE_A_OPCODE_PUT_FULL      = 4'd0;
    localparam bit [3:0] UNCACHE_A_OPCODE_PUT_PARTIAL   = 4'd1;
    localparam bit [3:0] UNCACHE_A_OPCODE_GET           = 4'd4;
    localparam bit [3:0] UNCACHE_D_OPCODE_ACCESS_ACK    = 4'd0;
    localparam bit [3:0] UNCACHE_D_OPCODE_ACCESS_ACKDATA = 4'd1;
    localparam int unsigned UNCACHE_D_READY_WARN_CYCLES = 1000;

    typedef enum int unsigned {
        UNCACHE_RESPONSE_STORE_ACK = 0,
        UNCACHE_RESPONSE_LOAD_DATA = 1
    } uncache_response_kind_e;

    typedef struct {
        uncache_response_kind_e kind;
        longint unsigned        eligible_cycle;
        longint unsigned        accept_cycle;
        bit [2:0]               size;
        bit [3:0]               source;
        bit [47:0]              address;
        bit                     denied;
        bit                     corrupt;
        bit [63:0]              data;
    } uncache_response_record_t;

    // 中文注释：Uncache A request 的 pending handshake owner。
    // 设置：当前 sample 观察到 A.valid 后准备驱动 A.ready 时；清零：下一 sample 确认 A.fire、
    // valid 撤销或 reset。作用：只有 A.fire 后才允许生成 response 或把 store 写入 shared batch。
    bit a_accept_armed;
    sbuffer_agent_agent_xaction armed_a_req_xact;
    // 中文注释：Uncache response record 队列与当前 D hold 分离。A.fire 后先创建 record；
    // 只有 scheduler 选中后才成为 current D hold。D.ready=0 时保持 current payload 不重采样。
    uncache_response_record_t uncache_rsp_q[$];
    uncache_response_record_t current_d_record;
    bit                       current_d_valid;
    bit                       uncache_rsp_timer_active;
    longint unsigned          uncache_rsp_timer_due_cycle;
    longint unsigned          service_cycle;
    int unsigned              d_hold_cycles;
    bit                       d_hold_timeout_reported;
    sbuffer_agent_agent_xaction last_cycle_xact;
    bit last_cycle_valid;

    `uvm_object_utils(sbuffer_mem_access_base_sequence)

    extern function new(string name = "sbuffer_mem_access_base_sequence");
    extern virtual function void clear_runtime_state();
    extern virtual function void build_sbuffer_idle_xaction(output sbuffer_agent_agent_xaction rsp_xact);
    extern virtual function void capture_sbuffer_a_xaction(output sbuffer_agent_agent_xaction req_xact);
    extern virtual function void check_sbuffer_a_payload_stable(
        input sbuffer_agent_agent_xaction expected_xact,
        input sbuffer_agent_agent_xaction observed_xact
    );
    extern virtual function uncache_response_kind_e decode_uncache_a_opcode(
        input sbuffer_agent_agent_xaction req_xact
    );
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
    extern virtual function int unsigned get_uncache_response_count();
    extern virtual function bit has_uncache_response_capacity();
    extern virtual function int unsigned sample_uncache_response_delay();
    extern virtual function int find_uncache_eligible_response(
        input longint unsigned current_cycle,
        input int unsigned      visible_count
    );
    extern virtual function void service_uncache_response_scheduler(
        input longint unsigned current_cycle,
        input int unsigned      visible_count
    );
    extern virtual function void apply_uncache_d_error_injection(
        input uncache_response_kind_e response_kind,
        input bit                     backend_denied,
        input bit                     backend_corrupt,
        output bit                    d_denied,
        output bit                    d_corrupt
    );
    extern virtual task create_uncache_response_record(
        input sbuffer_agent_agent_xaction req_xact,
        input longint unsigned            accept_cycle
    );
    extern virtual function void build_current_uncache_d_xaction(
        inout sbuffer_agent_agent_xaction rsp_xact
    );
    extern virtual function void process_uncache_d_fire();
    extern virtual function void service_uncache_d_hold_watchdog(input bit d_fire);
    extern virtual task body();

endclass:sbuffer_mem_access_base_sequence

function sbuffer_mem_access_base_sequence::new(string name = "sbuffer_mem_access_base_sequence");
    super.new(name);
    default_pre_pkt_gap  = 0;
    default_post_pkt_gap = 0;
    service_cycle        = 0;
    clear_runtime_state();
endfunction:new

function void sbuffer_mem_access_base_sequence::clear_runtime_state();
    a_accept_armed = 1'b0;
    armed_a_req_xact = null;
    uncache_rsp_q.delete();
    current_d_valid            = 1'b0;
    current_d_record           = '{default:'0};
    uncache_rsp_timer_active   = 1'b0;
    uncache_rsp_timer_due_cycle = 0;
    d_hold_cycles              = 0;
    d_hold_timeout_reported    = 1'b0;
    last_cycle_xact = null;
    last_cycle_valid = 1'b0;
endfunction:clear_runtime_state

function void sbuffer_mem_access_base_sequence::build_sbuffer_idle_xaction(output sbuffer_agent_agent_xaction rsp_xact);
    rsp_xact = sbuffer_agent_agent_xaction::type_id::create("sbuffer_idle_xact");
    rsp_xact.auto_inner_buffers_out_a_ready = 1'b0;
    rsp_xact.auto_inner_buffers_out_d_valid = 1'b0;
endfunction:build_sbuffer_idle_xaction

function void sbuffer_mem_access_base_sequence::capture_sbuffer_a_xaction(output sbuffer_agent_agent_xaction req_xact);
    req_xact = sbuffer_agent_agent_xaction::type_id::create("sbuffer_a_req_xact");
    // 中文注释：A.fire 的 valid 在 drv_cb sample 确认，payload 必须来自同一个 clocking-block
    // snapshot；不能读取 edge 后可能已经切换到下一笔请求的裸 interface。xaction payload 是二态 bit，
    // 因此复制前必须拒绝任一 X/Z，避免未知值静默折叠为 0 并污染 response 或 overlay。
    if ($isunknown({sbuffer_vif.drv_cb.auto_inner_buffers_out_a_bits_opcode,
                    sbuffer_vif.drv_cb.auto_inner_buffers_out_a_bits_param,
                    sbuffer_vif.drv_cb.auto_inner_buffers_out_a_bits_size,
                    sbuffer_vif.drv_cb.auto_inner_buffers_out_a_bits_source,
                    sbuffer_vif.drv_cb.auto_inner_buffers_out_a_bits_address,
                    sbuffer_vif.drv_cb.auto_inner_buffers_out_a_bits_mask,
                    sbuffer_vif.drv_cb.auto_inner_buffers_out_a_bits_data,
                    sbuffer_vif.drv_cb.auto_inner_buffers_out_a_bits_corrupt})) begin
        `uvm_fatal(get_type_name(), "Uncache A payload sampled as X/Z outside reset")
    end
    req_xact.auto_inner_buffers_out_a_valid                    = sbuffer_vif.drv_cb.auto_inner_buffers_out_a_valid;
    req_xact.auto_inner_buffers_out_a_ready                    = 1'b0;
    req_xact.auto_inner_buffers_out_a_bits_opcode              = sbuffer_vif.drv_cb.auto_inner_buffers_out_a_bits_opcode;
    req_xact.auto_inner_buffers_out_a_bits_param               = sbuffer_vif.drv_cb.auto_inner_buffers_out_a_bits_param;
    req_xact.auto_inner_buffers_out_a_bits_size                = sbuffer_vif.drv_cb.auto_inner_buffers_out_a_bits_size;
    req_xact.auto_inner_buffers_out_a_bits_source              = sbuffer_vif.drv_cb.auto_inner_buffers_out_a_bits_source;
    req_xact.auto_inner_buffers_out_a_bits_address             = sbuffer_vif.drv_cb.auto_inner_buffers_out_a_bits_address;
    req_xact.auto_inner_buffers_out_a_bits_mask                = sbuffer_vif.drv_cb.auto_inner_buffers_out_a_bits_mask;
    req_xact.auto_inner_buffers_out_a_bits_data                = sbuffer_vif.drv_cb.auto_inner_buffers_out_a_bits_data;
    req_xact.auto_inner_buffers_out_a_bits_corrupt             = sbuffer_vif.drv_cb.auto_inner_buffers_out_a_bits_corrupt;
endfunction:capture_sbuffer_a_xaction

function void sbuffer_mem_access_base_sequence::check_sbuffer_a_payload_stable(
    input sbuffer_agent_agent_xaction expected_xact,
    input sbuffer_agent_agent_xaction observed_xact
);
    if (expected_xact == null || observed_xact == null) begin
        `uvm_fatal(get_type_name(), "cannot check a null Uncache A payload")
    end
    if (expected_xact.auto_inner_buffers_out_a_bits_opcode  != observed_xact.auto_inner_buffers_out_a_bits_opcode ||
        expected_xact.auto_inner_buffers_out_a_bits_param   != observed_xact.auto_inner_buffers_out_a_bits_param ||
        expected_xact.auto_inner_buffers_out_a_bits_size    != observed_xact.auto_inner_buffers_out_a_bits_size ||
        expected_xact.auto_inner_buffers_out_a_bits_source  != observed_xact.auto_inner_buffers_out_a_bits_source ||
        expected_xact.auto_inner_buffers_out_a_bits_address != observed_xact.auto_inner_buffers_out_a_bits_address ||
        expected_xact.auto_inner_buffers_out_a_bits_mask    != observed_xact.auto_inner_buffers_out_a_bits_mask ||
        expected_xact.auto_inner_buffers_out_a_bits_data    != observed_xact.auto_inner_buffers_out_a_bits_data ||
        expected_xact.auto_inner_buffers_out_a_bits_corrupt != observed_xact.auto_inner_buffers_out_a_bits_corrupt) begin
        `uvm_fatal(get_type_name(), "Uncache A payload changed while valid was waiting for ready")
    end
endfunction:check_sbuffer_a_payload_stable

function sbuffer_mem_access_base_sequence::uncache_response_kind_e
sbuffer_mem_access_base_sequence::decode_uncache_a_opcode(
    input sbuffer_agent_agent_xaction req_xact
);
    case (req_xact.auto_inner_buffers_out_a_bits_opcode)
        UNCACHE_A_OPCODE_PUT_FULL,
        UNCACHE_A_OPCODE_PUT_PARTIAL: return UNCACHE_RESPONSE_STORE_ACK;
        UNCACHE_A_OPCODE_GET: return UNCACHE_RESPONSE_LOAD_DATA;
        default: begin
            `uvm_fatal(get_type_name(),
                       $sformatf("unsupported Uncache A opcode=%0d source=%0d address=0x%0h size=%0d param=%0d",
                                 req_xact.auto_inner_buffers_out_a_bits_opcode,
                                 req_xact.auto_inner_buffers_out_a_bits_source,
                                 req_xact.auto_inner_buffers_out_a_bits_address,
                                 req_xact.auto_inner_buffers_out_a_bits_size,
                                 req_xact.auto_inner_buffers_out_a_bits_param))
        end
    endcase
    return UNCACHE_RESPONSE_LOAD_DATA;
endfunction:decode_uncache_a_opcode

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

    shared_mem_access_task(beat_addr,
                           is_store,
                           line_mask,
                           line_store_data,
                           corrupt,
                           denied,
                           line_load_data,
                           is_store ? SHARED_MEM_WRITE_UNCACHE : SHARED_MEM_WRITE_NONE);
    load_data = line_load_data[63:0];
endtask:sbuffer_mem_access_task

function int unsigned sbuffer_mem_access_base_sequence::get_uncache_response_count();
    return uncache_rsp_q.size() + (current_d_valid ? 1 : 0);
endfunction:get_uncache_response_count

function bit sbuffer_mem_access_base_sequence::has_uncache_response_capacity();
    return get_uncache_response_count() < MEMBLOCK_DUT_UNCACHE_MAX_OUTSTANDING;
endfunction:has_uncache_response_capacity

function int unsigned sbuffer_mem_access_base_sequence::sample_uncache_response_delay();
    int unsigned delay_class;
    int unsigned delay_value;
    int unsigned zero_wt;
    int unsigned small_wt;
    int unsigned medium_wt;
    int unsigned large_wt;

    zero_wt   = seq_csr_common::get_uncache_rsp_delay_zero_wt();
    small_wt  = seq_csr_common::get_uncache_rsp_delay_small_wt();
    medium_wt = seq_csr_common::get_uncache_rsp_delay_medium_wt();
    large_wt  = seq_csr_common::get_uncache_rsp_delay_large_wt();
    if (!std::randomize(delay_class) with {
            delay_class dist {
                0 := zero_wt,
                1 := small_wt,
                2 := medium_wt,
                3 := large_wt
            };
        }) begin
        `uvm_fatal(get_type_name(), "failed to randomize Uncache response delay class")
    end
    case (delay_class)
        0: delay_value = 0;
        1: begin
            if (!std::randomize(delay_value) with { delay_value inside {[1:10]}; }) begin
                `uvm_fatal(get_type_name(), "failed to randomize SMALL Uncache delay")
            end
        end
        2: begin
            if (!std::randomize(delay_value) with { delay_value inside {[10:100]}; }) begin
                `uvm_fatal(get_type_name(), "failed to randomize MEDIUM Uncache delay")
            end
        end
        3: begin
            if (!std::randomize(delay_value) with { delay_value inside {[101:1000]}; }) begin
                `uvm_fatal(get_type_name(), "failed to randomize LARGE Uncache delay")
            end
        end
        default: begin
            `uvm_fatal(get_type_name(), $sformatf("unexpected Uncache delay class=%0d", delay_class))
        end
    endcase
    return delay_value;
endfunction:sample_uncache_response_delay

function int sbuffer_mem_access_base_sequence::find_uncache_eligible_response(
    input longint unsigned current_cycle,
    input int unsigned      visible_count
);
    int unsigned eligible_count;
    int unsigned selected_ordinal;
    int unsigned seen;
    int unsigned scan_count;

    scan_count = (visible_count < uncache_rsp_q.size()) ? visible_count : uncache_rsp_q.size();
    if (!seq_csr_common::get_uncache_rsp_reorder_en()) begin
        for (int unsigned i = 0; i < scan_count; i++) begin
            if (uncache_rsp_q[i].eligible_cycle <= current_cycle) begin
                return i;
            end
        end
        return -1;
    end
    eligible_count = 0;
    for (int unsigned i = 0; i < scan_count; i++) begin
        if (uncache_rsp_q[i].eligible_cycle <= current_cycle) begin
            eligible_count++;
        end
    end
    if (eligible_count == 0) begin
        return -1;
    end
    if (!std::randomize(selected_ordinal) with { selected_ordinal inside {[0:eligible_count-1]}; }) begin
        `uvm_fatal(get_type_name(), "failed to randomize Uncache ready response ordinal")
    end
    seen = 0;
    for (int unsigned i = 0; i < scan_count; i++) begin
        if (uncache_rsp_q[i].eligible_cycle <= current_cycle) begin
            if (seen == selected_ordinal) begin
                return i;
            end
            seen++;
        end
    end
    `uvm_fatal(get_type_name(), "Uncache eligible response ordinal was not found")
    return -1;
endfunction:find_uncache_eligible_response

function void sbuffer_mem_access_base_sequence::service_uncache_response_scheduler(
    input longint unsigned current_cycle,
    input int unsigned      visible_count
);
    int selected_index;
    int unsigned response_delay;
    uncache_response_record_t selected_record;

    if (current_d_valid) begin
        return;
    end
    selected_index = find_uncache_eligible_response(current_cycle, visible_count);
    if (!uncache_rsp_timer_active) begin
        if (selected_index < 0) begin
            return;
        end
        response_delay = sample_uncache_response_delay();
        uncache_rsp_timer_active    = 1'b1;
        uncache_rsp_timer_due_cycle = current_cycle + response_delay;
    end
    if (current_cycle < uncache_rsp_timer_due_cycle) begin
        return;
    end
    selected_index = find_uncache_eligible_response(current_cycle, visible_count);
    if (selected_index < 0) begin
        uncache_rsp_timer_active = 1'b0;
        return;
    end
    selected_record = uncache_rsp_q[selected_index];
    uncache_rsp_q.delete(selected_index);
    current_d_record        = selected_record;
    current_d_valid         = 1'b1;
    uncache_rsp_timer_active = 1'b0;
endfunction:service_uncache_response_scheduler

function void sbuffer_mem_access_base_sequence::apply_uncache_d_error_injection(
    input uncache_response_kind_e response_kind,
    input bit                     backend_denied,
    input bit                     backend_corrupt,
    output bit                    d_denied,
    output bit                    d_corrupt
);
    bit inject_denied;
    bit inject_corrupt;

    // 中文注释：该 helper 只在 Uncache response record 创建点调用一次，合并 backend 与
    // runtime 注入错误；scheduler/D hold 之后只搬运已固定的 record 字段，不可再次随机。
    inject_denied = sample_d_error_enable(
        seq_csr_common::get_uncache_denied_wt(),
        "Uncache denied"
    );
    d_denied = backend_denied || inject_denied;
    d_corrupt = 1'b0;

    case (response_kind)
        UNCACHE_RESPONSE_LOAD_DATA: begin
            inject_corrupt = sample_d_error_enable(
                seq_csr_common::get_uncache_corrupt_wt(),
                "Uncache corrupt"
            );
            d_corrupt = backend_corrupt || inject_corrupt || d_denied;
        end
        UNCACHE_RESPONSE_STORE_ACK: begin
            if (backend_corrupt) begin
                `uvm_fatal(get_type_name(),
                           "Uncache AccessAck cannot carry backend corrupt=1")
            end
        end
        default: begin
            `uvm_fatal(get_type_name(),
                       $sformatf("unsupported Uncache response kind=%0d for D error injection", response_kind))
        end
    endcase
endfunction:apply_uncache_d_error_injection

task sbuffer_mem_access_base_sequence::create_uncache_response_record(
    input sbuffer_agent_agent_xaction req_xact,
    input longint unsigned            accept_cycle
);
    bit        corrupt;
    bit        denied;
    bit [63:0] load_data;
    uncache_response_kind_e response_kind;
    uncache_response_record_t response_record;

    if (!has_uncache_response_capacity()) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("Uncache A.fire exceeded response capacity count=%0d max=%0d",
                             get_uncache_response_count(),
                             MEMBLOCK_DUT_UNCACHE_MAX_OUTSTANDING))
    end
    response_kind = decode_uncache_a_opcode(req_xact);
    sbuffer_mem_access_task(
        req_xact.auto_inner_buffers_out_a_bits_address,
        response_kind == UNCACHE_RESPONSE_STORE_ACK,
        req_xact.auto_inner_buffers_out_a_bits_mask,
        req_xact.auto_inner_buffers_out_a_bits_data,
        corrupt,
        denied,
        load_data
    );

    response_record                = '{default:'0};
    response_record.kind           = response_kind;
    response_record.eligible_cycle = accept_cycle + 1;
    response_record.accept_cycle   = accept_cycle;
    response_record.size           = req_xact.auto_inner_buffers_out_a_bits_size;
    response_record.source         = req_xact.auto_inner_buffers_out_a_bits_source;
    response_record.address        = req_xact.auto_inner_buffers_out_a_bits_address;
    response_record.data           = (response_kind == UNCACHE_RESPONSE_STORE_ACK) ? '0 : load_data;
    apply_uncache_d_error_injection(
        response_kind,
        denied,
        corrupt,
        response_record.denied,
        response_record.corrupt
    );
    uncache_rsp_q.push_back(response_record);
endtask:create_uncache_response_record

function void sbuffer_mem_access_base_sequence::build_current_uncache_d_xaction(
    inout sbuffer_agent_agent_xaction rsp_xact
);
    if (!current_d_valid) begin
        return;
    end
    rsp_xact.auto_inner_buffers_out_d_valid        = 1'b1;
    rsp_xact.auto_inner_buffers_out_d_bits_opcode  =
        (current_d_record.kind == UNCACHE_RESPONSE_STORE_ACK) ?
        UNCACHE_D_OPCODE_ACCESS_ACK : UNCACHE_D_OPCODE_ACCESS_ACKDATA;
    rsp_xact.auto_inner_buffers_out_d_bits_param   = '0;
    rsp_xact.auto_inner_buffers_out_d_bits_size    = current_d_record.size;
    rsp_xact.auto_inner_buffers_out_d_bits_source  = current_d_record.source;
    rsp_xact.auto_inner_buffers_out_d_bits_sink    = '0;
    rsp_xact.auto_inner_buffers_out_d_bits_denied  = current_d_record.denied;
    rsp_xact.auto_inner_buffers_out_d_bits_data    = current_d_record.data;
    rsp_xact.auto_inner_buffers_out_d_bits_corrupt = current_d_record.corrupt;
    rsp_xact.pre_pkt_gap                           = default_pre_pkt_gap;
    rsp_xact.post_pkt_gap                          = default_post_pkt_gap;
endfunction:build_current_uncache_d_xaction

function void sbuffer_mem_access_base_sequence::process_uncache_d_fire();
    if (!current_d_valid) begin
        `uvm_fatal(get_type_name(), "Uncache D.fire observed without a current response record")
    end
    current_d_valid  = 1'b0;
    current_d_record = '{default:'0};
    d_hold_cycles = 0;
    d_hold_timeout_reported = 1'b0;
endfunction:process_uncache_d_fire

function void sbuffer_mem_access_base_sequence::service_uncache_d_hold_watchdog(input bit d_fire);
    if (!current_d_valid || d_fire) begin
        d_hold_cycles           = 0;
        d_hold_timeout_reported = 1'b0;
        return;
    end
    if (last_cycle_valid && (last_cycle_xact != null) &&
        (last_cycle_xact.auto_inner_buffers_out_d_valid == 1'b1)) begin
        d_hold_cycles++;
        if ((d_hold_cycles >= UNCACHE_D_READY_WARN_CYCLES) && !d_hold_timeout_reported) begin
            `uvm_warning(get_type_name(),
                         $sformatf("Uncache D hold exceeds %0d cycles: source=%0d opcode=%0d size=%0d address=0x%0h denied=%0d corrupt=%0d accept_cycle=%0d",
                                   UNCACHE_D_READY_WARN_CYCLES,
                                   current_d_record.source,
                                   (current_d_record.kind == UNCACHE_RESPONSE_STORE_ACK) ?
                                   UNCACHE_D_OPCODE_ACCESS_ACK : UNCACHE_D_OPCODE_ACCESS_ACKDATA,
                                   current_d_record.size,
                                   current_d_record.address,
                                   current_d_record.denied,
                                   current_d_record.corrupt,
                                   current_d_record.accept_cycle))
            d_hold_timeout_reported = 1'b1;
        end
    end
endfunction:service_uncache_d_hold_watchdog

task sbuffer_mem_access_base_sequence::body();
    sbuffer_agent_agent_xaction idle_xact;
    sbuffer_agent_agent_xaction req_xact;
    sbuffer_agent_agent_xaction fired_a_req_xact;
    logic sampled_a_valid_raw;
    logic sampled_d_ready_raw;
    bit sampled_a_valid;
    bit sampled_d_ready;
    bit reset_active;
    bit a_fire;
    bit d_fire;
    common_data_transaction data;
    int unsigned response_visible_count;

    if (!uvm_config_db#(virtual sbuffer_agent_agent_interface)::get(null, get_full_name(), "vif", sbuffer_vif) &&
        !uvm_config_db#(virtual sbuffer_agent_agent_interface)::get(null, "uvm_test_top.env.u_sbuffer_agent_agent*", "vif", sbuffer_vif)) begin
        `uvm_fatal(get_type_name(), "sbuffer virtual interface is not set for memory access sequence")
    end
    data = common_data_transaction::get();
    if (data == null) begin
        `uvm_fatal(get_type_name(), "failed to get common_data_transaction for SBuffer responder")
    end
    seq_csr_common::init();
    // 中文注释：legacy default topology 未经过 real-smoke vseq 时由首个 responder 兜底初始化。
    // real-smoke 已提前完成初始化时只读取静态状态，绝不重复清空 shared backing/overlay。
    if (!is_shared_memory_lifecycle_initialized()) begin
        initialize_shared_memory_state(seq_csr_common::get_main_mem_ranges_en(),
                                       mem_addr_t'(seq_csr_common::get_paddr_base()),
                                       seq_csr_common::get_paddr_range());
    end
    clear_runtime_state();
    service_cycle = 0;

    forever begin
        // 中文注释：先在 drv_cb 边界确认上一轮驱动的 A.ready/D.valid 是否真实握手，
        // 并推进 shared-store 采样代次；store overlay 只能由 a_fire 分支建立写批次，
        // 下一拍即使没有新的 memory access 也会由这里统一提交。
        @(sbuffer_vif.drv_cb);
        begin_shared_mem_sample($time);
        sampled_a_valid_raw = sbuffer_vif.drv_cb.auto_inner_buffers_out_a_valid;
        sampled_d_ready_raw = sbuffer_vif.drv_cb.auto_inner_buffers_out_d_ready;
        reset_active        = (sbuffer_vif.rst_n !== 1'b1) ||
                              (memblock_sync_pkg::reset_backend_done !== 1'b1);
        sampled_a_valid     = (sampled_a_valid_raw === 1'b1);
        sampled_d_ready     = (sampled_d_ready_raw === 1'b1);
        a_fire              = 1'b0;
        d_fire              = 1'b0;

        if (!reset_active &&
            ((sampled_a_valid_raw !== 1'b0 && sampled_a_valid_raw !== 1'b1) ||
             (sampled_d_ready_raw !== 1'b0 && sampled_d_ready_raw !== 1'b1))) begin
            `uvm_fatal(get_type_name(), "Uncache A.valid/D.ready sampled as X/Z outside reset")
        end

        if (reset_active) begin
            clear_runtime_state();
        end
        else begin
            // scheduler 只看见进入本拍前已有的 record。本拍 A.fire 新建的 record
            // 即使 eligible_cycle 已满足，也要在下一拍才可能进入返回仲裁。
            response_visible_count = uncache_rsp_q.size();
            if (a_accept_armed) begin
                if (!last_cycle_valid ||
                    last_cycle_xact.auto_inner_buffers_out_a_ready !== 1'b1) begin
                    `uvm_fatal(get_type_name(), "armed Uncache A request lost its driven A.ready")
                end
                if (sampled_a_valid) begin
                    capture_sbuffer_a_xaction(fired_a_req_xact);
                    check_sbuffer_a_payload_stable(armed_a_req_xact, fired_a_req_xact);
                    a_fire = 1'b1;
                    create_uncache_response_record(fired_a_req_xact, service_cycle);
                end
                a_accept_armed = 1'b0;
                armed_a_req_xact = null;
            end

            if (current_d_valid && last_cycle_valid &&
                last_cycle_xact.auto_inner_buffers_out_d_valid === 1'b1 &&
                sampled_d_ready) begin
                d_fire = 1'b1;
                process_uncache_d_fire();
            end
            service_uncache_d_hold_watchdog(d_fire);
        end

        // 中文注释：global stop 后只能 drain 已由前一拍 A.ready 接受的请求；若此时出现
        // 新的未 fire A.valid，继续保持 A.ready=0 会让 DUT 与 responder 永久互等，必须 fail-fast。
        if (!reset_active && data.is_global_stop_requested() && sampled_a_valid && !a_fire) begin
            `uvm_fatal(get_type_name(),
                       "new Uncache A.valid observed after global stop without a sampled fire")
        end

        build_sbuffer_idle_xaction(idle_xact);
        if (reset_active) begin
            // reset 周期保持所有 responder output 为零。
        end
        else if (data.is_global_stop_requested() && !current_d_valid &&
                 (uncache_rsp_q.size() == 0) && !uncache_rsp_timer_active &&
                 !a_accept_armed && !sampled_a_valid) begin
            send_sbuffer_xaction(idle_xact);
            last_cycle_xact  = idle_xact;
            last_cycle_valid = 1'b1;
            service_cycle++;
            break;
        end
        else begin
            service_uncache_response_scheduler(service_cycle, response_visible_count);
            build_current_uncache_d_xaction(idle_xact);
            if (!a_fire && !data.is_global_stop_requested() && sampled_a_valid) begin
                capture_sbuffer_a_xaction(req_xact);
                void'(decode_uncache_a_opcode(req_xact));
                if (has_uncache_response_capacity()) begin
                    armed_a_req_xact = req_xact;
                    a_accept_armed   = 1'b1;
                    idle_xact.auto_inner_buffers_out_a_ready = 1'b1;
                end
            end
        end

        send_sbuffer_xaction(idle_xact);
        last_cycle_xact  = idle_xact;
        last_cycle_valid = 1'b1;
        service_cycle++;
    end
endtask:body

`endif
