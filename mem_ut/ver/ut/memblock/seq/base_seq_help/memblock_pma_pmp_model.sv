`ifndef MEMBLOCK_PMA_PMP_MODEL__SV
`define MEMBLOCK_PMA_PMP_MODEL__SV

typedef enum bit [1:0] {
    PMA_PMP_A_OFF   = 2'd0,
    PMA_PMP_A_TOR   = 2'd1,
    PMA_PMP_A_NA4   = 2'd2,
    PMA_PMP_A_NAPOT = 2'd3
} pma_pmp_a_mode_e;

typedef enum bit [2:0] {
    PMA_PMP_CMD_READ       = 3'd0,
    PMA_PMP_CMD_WRITE      = 3'd1,
    PMA_PMP_CMD_EXEC       = 3'd2,
    PMA_PMP_CMD_READ_EXEC  = 3'd3,
    PMA_PMP_CMD_ATOM_READ  = 3'd4,
    PMA_PMP_CMD_ATOM_WRITE = 3'd5
} pma_pmp_cmd_e;

typedef enum bit [1:0] {
    PMA_PMP_DCACHE_FACT_UNKNOWN = 2'd0,
    PMA_PMP_DCACHE_FACT_YES     = 2'd1,
    PMA_PMP_DCACHE_FACT_NO      = 2'd2
} pma_pmp_dcache_fact_e;

// 中文注释：一次动态标量访问的不可变身份。UID 相同但 redirect 后的
// dynamic_epoch 不同，必须使用不同上下文；access_seq 由 PMA/PMP owner 单调分配，
// 用于避免同一 UID 的旧 DCache 事实覆盖重发实例。
typedef struct packed {
    memblock_uid_t       uid;
    int unsigned         dynamic_epoch;
    longint unsigned     access_seq;
} pma_pmp_access_key_t;

typedef struct packed {
    bit                  valid;
    int unsigned         index;
    bit                  lock;
    bit                  c;
    bit                  atomic;
    pma_pmp_a_mode_e     a;
    bit                  x;
    bit                  w;
    bit                  r;
    bit [45:0]           addr_raw;
    bit [47:0]           match_mask;
    bit [47:0]           compare_addr;
    longint unsigned     update_generation;
    bit [1:0]            origin;
} pma_pmp_entry_t;

typedef struct packed {
    bit                  valid;
    memblock_uid_t       uid;
    int unsigned         dynamic_epoch;
    longint unsigned     access_seq;
    bit [1:0]            priv_mode;
    bit                  debug;
    bit                  keyid_enable;
    bit                  cmode;
    bit [7:0]            keyid;
    int unsigned         csr_update_seq;
    longint unsigned     pmp_generation;
    longint unsigned     pma_generation;
    longint unsigned     capture_sample;
} pma_pmp_uid_context_t;

typedef struct packed {
    bit                  valid;
    bit                  translation_success;
    bit [47:0]           paddr;
    int unsigned         size_bytes;
    pma_pmp_cmd_e        cmd;
    bit [1:0]            priv_mode;
    bit                  debug;
    bit                  keyid_enable;
    bit                  cmode;
    bit [7:0]            keyid;
    int unsigned         csr_update_seq;
    longint unsigned     pmp_generation;
    longint unsigned     pma_generation;
    longint unsigned     capture_sample;
} pma_pmp_request_context_t;

typedef struct packed {
    bit                  valid;
    bit                  translation_eligible;
    pma_pmp_cmd_e        cmd;
    bit                  pmp_hit;
    bit                  pma_hit;
    int signed           pmp_hit_index;
    int signed           pma_hit_index;
    bit                  pmp_ld_fault;
    bit                  pmp_st_fault;
    bit                  pmp_instr_fault;
    bit                  pma_ld_fault;
    bit                  pma_st_fault;
    bit                  pma_instr_fault;
    bit                  keyid_fault;
    bit                  pma_atomic_fault;
    bit                  ld_fault;
    bit                  st_fault;
    bit                  instr_fault;
    bit                  mmio;
    bit                  atomic_allowed;
    bit                  cacheable;
    longint unsigned     pmp_generation;
    longint unsigned     pma_generation;
    longint unsigned     capture_sample;
} pma_pmp_eval_t;

typedef struct packed {
    bit                  valid;
    bit                  translation_eligible;
    bit                  base_ld_access_fault;
    bit                  base_st_access_fault;
    bit                  base_instr_access_fault;
    bit                  dcache_fact_needed_for_c;
    bit                  af_decided;
    bit                  pmp_ld_fault;
    bit                  pmp_st_fault;
    bit                  pmp_instr_fault;
    bit                  pma_ld_fault;
    bit                  pma_st_fault;
    bit                  pma_instr_fault;
    bit                  keyid_fault;
    bit                  pma_atomic_fault;
    bit                  atomic_noncache_path_fault;
    bit                  pma_cache_path_fault;
    bit                  ld_access_fault;
    bit                  st_access_fault;
    bit                  instr_access_fault;
    int signed           pmp_hit_index;
    int signed           pma_hit_index;
    longint unsigned     pmp_generation;
    longint unsigned     pma_generation;
    longint unsigned     capture_sample;
} pma_pmp_af_view_t;

class pma_pmp_table_snapshot extends uvm_object;
    localparam int unsigned PMA_PMP_ENTRY_COUNT = 32;
    pma_pmp_entry_t entries[PMA_PMP_ENTRY_COUNT];

    `uvm_object_utils(pma_pmp_table_snapshot)

    function new(string name = "pma_pmp_table_snapshot");
        super.new(name);
    endfunction:new

    function void copy_entries(input pma_pmp_entry_t source[PMA_PMP_ENTRY_COUNT]);
        for (int unsigned i = 0; i < PMA_PMP_ENTRY_COUNT; i++) begin
            entries[i] = source[i];
        end
    endfunction:copy_entries
endclass:pma_pmp_table_snapshot

class memblock_pma_pmp_model extends uvm_object;
    localparam int unsigned PMA_PMP_ENTRY_COUNT = 32;
    localparam int unsigned PMA_PMP_CFG_PER_CSR = 8;
    localparam int unsigned PMA_PMP_PADDR_BITS = 48;
    localparam int unsigned PMA_PMP_OFF_BITS = 2;
    localparam int unsigned PMA_PMP_GRAIN_BITS = 12;
    localparam bit [11:0] PMA_PMP_PMPCFG_BASE = 12'h3a0;
    localparam bit [11:0] PMA_PMP_PMPADDR_BASE = 12'h3b0;
    localparam bit [11:0] PMA_PMP_PMACFG_BASE = 12'h7c0;
    localparam bit [11:0] PMA_PMP_PMAADDR_BASE = 12'h7c8;
    localparam bit [47:0] PMA_PMP_DEBUG_START = 48'h0000_3802_0000;
    localparam bit [47:0] PMA_PMP_DEBUG_END = 48'h0000_3802_0fff;

    static memblock_pma_pmp_model m_inst;

    // 中文注释：两张表分别对应 V2 PMP/PMA CSR 状态。CSR 写是唯一更新者，
    // 每次有效更新都会保留 generation 对应的值型快照，供 outstanding request 冻结使用。
    pma_pmp_entry_t pmp_entries[PMA_PMP_ENTRY_COUNT];
    pma_pmp_entry_t pma_entries[PMA_PMP_ENTRY_COUNT];
    pma_pmp_table_snapshot pmp_history[longint unsigned];
    pma_pmp_table_snapshot pma_history[longint unsigned];
    // 中文注释：context 以完整动态访问 key 保存；latest key 只作为 RM 从 UID
    // 进入当前 dynamic instance 的索引，真正评估仍使用冻结的 generation snapshot。
    pma_pmp_uid_context_t uid_context_by_access_key[pma_pmp_access_key_t];
    pma_pmp_access_key_t  latest_context_key_by_uid[memblock_uid_t];
    longint unsigned       next_access_seq;
    longint unsigned pmp_generation;
    longint unsigned pma_generation;
    bit ready;

    `uvm_object_utils(memblock_pma_pmp_model)

    extern function new(string name = "memblock_pma_pmp_model");
    extern static function memblock_pma_pmp_model get();
    extern function void reset_and_init_v2_profile();
    extern function bit is_ready();
    extern function void apply_csr_write(
        input bit [11:0] addr,
        input bit [63:0] data,
        input longint unsigned sample
    );
    extern function bit capture_uid_context(
        input memblock_uid_t uid,
        input int unsigned dynamic_epoch,
        input bit [1:0] priv_mode,
        input bit debug,
        input bit keyid_enable,
        input bit cmode,
        input bit [7:0] keyid,
        input int unsigned csr_update_seq,
        input longint unsigned sample
    );
    extern function bit read_uid_context(
        input memblock_uid_t uid,
        output pma_pmp_uid_context_t context_view
    );
    extern function bit read_uid_context_for_epoch(
        input memblock_uid_t uid,
        input int unsigned   dynamic_epoch,
        output pma_pmp_uid_context_t context_view
    );
    extern function bit evaluate_for_uid(
        input memblock_uid_t uid,
        input bit translation_success,
        input bit [47:0] paddr,
        input int unsigned size_bytes,
        input pma_pmp_cmd_e cmd,
        output pma_pmp_eval_t result
    );
    extern function bit evaluate_for_uid_epoch(
        input memblock_uid_t uid,
        input int unsigned   dynamic_epoch,
        input bit translation_success,
        input bit [47:0] paddr,
        input int unsigned size_bytes,
        input pma_pmp_cmd_e cmd,
        output pma_pmp_eval_t result
    );
    extern function bit evaluate(
        input pma_pmp_request_context_t request,
        output pma_pmp_eval_t result
    );
    extern function void make_base_af_view(
        input pma_pmp_eval_t result,
        output pma_pmp_af_view_t view
    );
    extern function bit finalize_cache_path_af(
        input pma_pmp_eval_t result,
        input pma_pmp_af_view_t base_view,
        input pma_pmp_dcache_fact_e dcache_fact,
        output pma_pmp_af_view_t final_view
    );
    extern function bit read_entry(
        input bit is_pma,
        input int unsigned index,
        output pma_pmp_entry_t entry
    );
    extern function bit check_invariants(output string message);
    extern function void load_v2_pma_profile();
    extern function void set_pma_source_entry(
        input int unsigned source_index,
        input bit [47:0] base_addr,
        input bit [48:0] range,
        input bit c,
        input bit atomic,
        input bit lock,
        input pma_pmp_a_mode_e a,
        input bit x,
        input bit w,
        input bit r
    );
    extern local function void init_entry(
        output pma_pmp_entry_t entry,
        input int unsigned index
    );
    extern local function void refresh_entry_match(inout pma_pmp_entry_t entry);
    extern local function void save_table_snapshot(input bit is_pma);
    extern local function pma_pmp_table_snapshot get_table_snapshot(
        input bit is_pma,
        input longint unsigned generation
    );
    extern local function bit apply_config_write(
        input bit is_pma,
        input int unsigned first_index,
        input bit [63:0] data
    );
    extern local function bit apply_addr_write(
        input bit is_pma,
        input int unsigned index,
        input bit [63:0] data
    );
    extern local function bit find_first_match(
        input pma_pmp_table_snapshot snapshot,
        input bit [47:0] paddr,
        input int unsigned size_bytes,
        input bit debug,
        output int signed hit_index,
        output pma_pmp_entry_t hit_entry
    );
    extern local function bit entry_matches(
        input pma_pmp_entry_t entry,
        input pma_pmp_entry_t previous,
        input bit [47:0] paddr,
        input int unsigned size_bytes
    );
    extern local function bit cmd_is_load(input pma_pmp_cmd_e cmd);
    extern local function bit cmd_is_store(input pma_pmp_cmd_e cmd);
    extern local function bit cmd_is_instr(input pma_pmp_cmd_e cmd);
    extern local function bit cmd_is_atomic(input pma_pmp_cmd_e cmd);

endclass:memblock_pma_pmp_model

function memblock_pma_pmp_model::new(string name = "memblock_pma_pmp_model");
    super.new(name);
    reset_and_init_v2_profile();
endfunction:new

function memblock_pma_pmp_model memblock_pma_pmp_model::get();
    if (m_inst == null) begin
        m_inst = memblock_pma_pmp_model::type_id::create("memblock_pma_pmp_model");
    end
    return m_inst;
endfunction:get

function void memblock_pma_pmp_model::init_entry(
    output pma_pmp_entry_t entry,
    input int unsigned index
);
    entry = '{default:'0};
    entry.index = index;
    entry.a = PMA_PMP_A_OFF;
endfunction:init_entry

function void memblock_pma_pmp_model::refresh_entry_match(inout pma_pmp_entry_t entry);
    longint unsigned match_addr;
    longint unsigned mask;

    entry.valid = entry.a != PMA_PMP_A_OFF;
    entry.compare_addr = {entry.addr_raw, 2'b00} & ~48'h0000_0000_0fff;
    match_addr = (entry.addr_raw << 1) |
                 entry.a[0] |
                 ((64'd1 << PMA_PMP_GRAIN_BITS) - 1) >> PMA_PMP_OFF_BITS;
    mask = ((match_addr & ~(match_addr + 1)) << PMA_PMP_OFF_BITS) |
           ((64'd1 << PMA_PMP_OFF_BITS) - 1);
    entry.match_mask = mask[47:0];
endfunction:refresh_entry_match

function void memblock_pma_pmp_model::set_pma_source_entry(
    input int unsigned source_index,
    input bit [47:0] base_addr,
    input bit [48:0] range,
    input bit c,
    input bit atomic,
    input bit lock,
    input pma_pmp_a_mode_e a,
    input bit x,
    input bit w,
    input bit r
);
    int unsigned target_index;
    pma_pmp_entry_t entry;
    longint unsigned napot_addr;

    if (source_index >= PMA_PMP_ENTRY_COUNT) begin
        `uvm_fatal(get_type_name(), $sformatf("PMA source index %0d exceeds profile capacity", source_index))
    end
    target_index = PMA_PMP_ENTRY_COUNT - 1 - source_index;
    init_entry(entry, target_index);
    entry.lock = lock;
    entry.c = c;
    entry.atomic = atomic;
    entry.a = a;
    entry.x = x;
    entry.w = w;
    entry.r = r;
    entry.origin = 2'd1;
    if (a >= PMA_PMP_A_NA4) begin
        if (a == PMA_PMP_A_NAPOT) begin
            if (range == 0 || range[11:0] != '0 || base_addr[11:0] != '0) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("invalid V2 PMA NAPOT source=%0d base=0x%0h range=0x%0h",
                                     source_index, base_addr, range))
            end
            napot_addr = (base_addr + (range / 2 - 1)) >> PMA_PMP_OFF_BITS;
            entry.addr_raw = napot_addr[45:0];
        end else begin
            entry.addr_raw = base_addr[47:2];
        end
    end else begin
        entry.addr_raw = base_addr[47:2];
    end
    refresh_entry_match(entry);
    pma_entries[target_index] = entry;
endfunction:set_pma_source_entry

function void memblock_pma_pmp_model::save_table_snapshot(input bit is_pma);
    pma_pmp_table_snapshot snapshot;
    longint unsigned generation;

    snapshot = pma_pmp_table_snapshot::type_id::create(
        is_pma ? $sformatf("pma_snapshot_%0d", pma_generation) :
                 $sformatf("pmp_snapshot_%0d", pmp_generation));
    if (snapshot == null) begin
        `uvm_fatal(get_type_name(), "failed to allocate PMA/PMP table snapshot")
    end
    if (is_pma) begin
        generation = pma_generation;
        snapshot.copy_entries(pma_entries);
        pma_history[generation] = snapshot;
    end else begin
        generation = pmp_generation;
        snapshot.copy_entries(pmp_entries);
        pmp_history[generation] = snapshot;
    end
endfunction:save_table_snapshot

function pma_pmp_table_snapshot memblock_pma_pmp_model::get_table_snapshot(
    input bit is_pma,
    input longint unsigned generation
);
    if (is_pma) begin
        if (!pma_history.exists(generation)) return null;
        return pma_history[generation];
    end
    if (!pmp_history.exists(generation)) return null;
    return pmp_history[generation];
endfunction:get_table_snapshot

function void memblock_pma_pmp_model::reset_and_init_v2_profile();
    for (int unsigned i = 0; i < PMA_PMP_ENTRY_COUNT; i++) begin
        init_entry(pmp_entries[i], i);
        init_entry(pma_entries[i], i);
    end
    pmp_history.delete();
    pma_history.delete();
    uid_context_by_access_key.delete();
    latest_context_key_by_uid.delete();
    next_access_seq = 1;
    pmp_generation = 0;
    pma_generation = 0;
    load_v2_pma_profile();
    save_table_snapshot(1'b0);
    save_table_snapshot(1'b1);
    ready = 1'b1;
endfunction:reset_and_init_v2_profile

function bit memblock_pma_pmp_model::is_ready();
    return ready;
endfunction:is_ready

function bit memblock_pma_pmp_model::apply_config_write(
    input bit is_pma,
    input int unsigned first_index,
    input bit [63:0] data
);
    bit changed;
    pma_pmp_entry_t entry;
    bit [7:0] raw_cfg;
    pma_pmp_a_mode_e raw_a;

    changed = 1'b0;
    for (int unsigned slot = 0; slot < PMA_PMP_CFG_PER_CSR; slot++) begin
        int unsigned index;
        index = first_index + slot;
        if (index >= PMA_PMP_ENTRY_COUNT) begin
            continue;
        end
        entry = is_pma ? pma_entries[index] : pmp_entries[index];
        if (entry.lock) begin
            continue;
        end
        raw_cfg = data[(slot * 8) +: 8];
        raw_a = pma_pmp_a_mode_e'(raw_cfg[4:3]);
        // V2 PlatformGrain=4KiB, exactly matching the RTL Cat(a(1), a.orR) WARL.
        entry.a = {raw_a[1], |raw_a};
        entry.lock = raw_cfg[7];
        entry.c = raw_cfg[6];
        entry.atomic = raw_cfg[5];
        entry.x = raw_cfg[2];
        entry.r = raw_cfg[0];
        entry.w = raw_cfg[1] && raw_cfg[0];
        entry.origin = 2'd2;
        refresh_entry_match(entry);
        if (is_pma) pma_entries[index] = entry;
        else pmp_entries[index] = entry;
        changed = 1'b1;
    end
    return changed;
endfunction:apply_config_write

function bit memblock_pma_pmp_model::apply_addr_write(
    input bit is_pma,
    input int unsigned index,
    input bit [63:0] data
);
    pma_pmp_entry_t entry;
    pma_pmp_entry_t next_entry;

    if (index >= PMA_PMP_ENTRY_COUNT) return 1'b0;
    entry = is_pma ? pma_entries[index] : pmp_entries[index];
    if (index + 1 < PMA_PMP_ENTRY_COUNT) begin
        next_entry = is_pma ? pma_entries[index + 1] : pmp_entries[index + 1];
    end else begin
        init_entry(next_entry, PMA_PMP_ENTRY_COUNT);
    end
    if (entry.lock || (next_entry.lock && next_entry.a == PMA_PMP_A_TOR)) begin
        return 1'b0;
    end
    entry.addr_raw = data[45:0];
    entry.origin = 2'd2;
    refresh_entry_match(entry);
    if (is_pma) pma_entries[index] = entry;
    else pmp_entries[index] = entry;
    return 1'b1;
endfunction:apply_addr_write

function void memblock_pma_pmp_model::apply_csr_write(
    input bit [11:0] addr,
    input bit [63:0] data,
    input longint unsigned sample
);
    bit changed;
    bit is_pma;
    int unsigned index;

    if (!ready) begin
        reset_and_init_v2_profile();
    end
    changed = 1'b0;
    is_pma = 1'b0;
    for (int unsigned group = 0; group < PMA_PMP_ENTRY_COUNT / PMA_PMP_CFG_PER_CSR; group++) begin
        if (addr == PMA_PMP_PMPCFG_BASE + (group * 2)) begin
            changed = apply_config_write(1'b0, group * PMA_PMP_CFG_PER_CSR, data);
            is_pma = 1'b0;
        end
        if (addr == PMA_PMP_PMACFG_BASE + (group * 2)) begin
            changed = apply_config_write(1'b1, group * PMA_PMP_CFG_PER_CSR, data);
            is_pma = 1'b1;
        end
    end
    for (index = 0; index < PMA_PMP_ENTRY_COUNT; index++) begin
        if (addr == PMA_PMP_PMPADDR_BASE + index) begin
            changed = apply_addr_write(1'b0, index, data);
            is_pma = 1'b0;
        end
        if (addr == PMA_PMP_PMAADDR_BASE + index) begin
            changed = apply_addr_write(1'b1, index, data);
            is_pma = 1'b1;
        end
    end
    if (changed) begin
        if (is_pma) begin
            pma_generation++;
            save_table_snapshot(1'b1);
        end else begin
            pmp_generation++;
            save_table_snapshot(1'b0);
        end
        `uvm_info(get_type_name(),
                  $sformatf("PMA/PMP CSR write addr=0x%0h sample=%0d pmp_gen=%0d pma_gen=%0d",
                            addr, sample, pmp_generation, pma_generation),
                  UVM_HIGH)
    end
endfunction:apply_csr_write

function bit memblock_pma_pmp_model::capture_uid_context(
    input memblock_uid_t uid,
    input int unsigned dynamic_epoch,
    input bit [1:0] priv_mode,
    input bit debug,
    input bit keyid_enable,
    input bit cmode,
    input bit [7:0] keyid,
    input int unsigned csr_update_seq,
    input longint unsigned sample
);
    pma_pmp_uid_context_t uid_context;
    pma_pmp_access_key_t access_key;

    if (!ready) return 1'b0;
    if (latest_context_key_by_uid.exists(uid) &&
        latest_context_key_by_uid[uid].dynamic_epoch == dynamic_epoch &&
        uid_context_by_access_key.exists(latest_context_key_by_uid[uid])) begin
        // 同一动态实例可能因 DTLB replay 再次命中该 helper。PMPChecker 的 request
        // context 必须保持首次 fire 的 generation，不能被后续 CSR 写重采样。
        return 1'b1;
    end
    if (next_access_seq == 0) begin
        `uvm_fatal(get_type_name(), "PMA/PMP access sequence wrapped to zero")
    end
    access_key.uid = uid;
    access_key.dynamic_epoch = dynamic_epoch;
    access_key.access_seq = next_access_seq;
    next_access_seq++;
    uid_context = '{default:'0};
    uid_context.valid = 1'b1;
    uid_context.uid = uid;
    uid_context.dynamic_epoch = dynamic_epoch;
    uid_context.access_seq = access_key.access_seq;
    uid_context.priv_mode = priv_mode;
    uid_context.debug = debug;
    uid_context.keyid_enable = keyid_enable;
    uid_context.cmode = cmode;
    uid_context.keyid = keyid;
    uid_context.csr_update_seq = csr_update_seq;
    uid_context.pmp_generation = pmp_generation;
    uid_context.pma_generation = pma_generation;
    uid_context.capture_sample = sample;
    uid_context_by_access_key[access_key] = uid_context;
    latest_context_key_by_uid[uid] = access_key;
    return 1'b1;
endfunction:capture_uid_context

function bit memblock_pma_pmp_model::read_uid_context(
    input memblock_uid_t uid,
    output pma_pmp_uid_context_t context_view
);
    pma_pmp_access_key_t access_key;

    context_view = '{default:'0};
    if (!latest_context_key_by_uid.exists(uid)) return 1'b0;
    access_key = latest_context_key_by_uid[uid];
    if (!uid_context_by_access_key.exists(access_key)) return 1'b0;
    context_view = uid_context_by_access_key[access_key];
    return context_view.valid;
endfunction:read_uid_context

function bit memblock_pma_pmp_model::read_uid_context_for_epoch(
    input memblock_uid_t uid,
    input int unsigned dynamic_epoch,
    output pma_pmp_uid_context_t context_view
);
    pma_pmp_access_key_t access_key;

    context_view = '{default:'0};
    if (!latest_context_key_by_uid.exists(uid)) return 1'b0;
    access_key = latest_context_key_by_uid[uid];
    if (access_key.dynamic_epoch != dynamic_epoch ||
        !uid_context_by_access_key.exists(access_key)) begin
        return 1'b0;
    end
    context_view = uid_context_by_access_key[access_key];
    return context_view.valid && context_view.dynamic_epoch == dynamic_epoch;
endfunction:read_uid_context_for_epoch

function bit memblock_pma_pmp_model::cmd_is_load(input pma_pmp_cmd_e cmd);
    return cmd == PMA_PMP_CMD_READ || cmd == PMA_PMP_CMD_READ_EXEC ||
           cmd == PMA_PMP_CMD_ATOM_READ;
endfunction:cmd_is_load

function bit memblock_pma_pmp_model::cmd_is_store(input pma_pmp_cmd_e cmd);
    return cmd == PMA_PMP_CMD_WRITE || cmd == PMA_PMP_CMD_ATOM_WRITE;
endfunction:cmd_is_store

function bit memblock_pma_pmp_model::cmd_is_instr(input pma_pmp_cmd_e cmd);
    return cmd == PMA_PMP_CMD_EXEC || cmd == PMA_PMP_CMD_READ_EXEC;
endfunction:cmd_is_instr

function bit memblock_pma_pmp_model::cmd_is_atomic(input pma_pmp_cmd_e cmd);
    return cmd == PMA_PMP_CMD_ATOM_READ || cmd == PMA_PMP_CMD_ATOM_WRITE;
endfunction:cmd_is_atomic

function bit memblock_pma_pmp_model::entry_matches(
    input pma_pmp_entry_t entry,
    input pma_pmp_entry_t previous,
    input bit [47:0] paddr,
    input int unsigned size_bytes
);
    bit [47:0] na4_base;

    if (!entry.valid || entry.a == PMA_PMP_A_OFF) return 1'b0;
    // V2 scalar PMPChecker 的 lgMaxSize=3 小于 PlatformGrain=12，RTL 的
    // boundMatch()/napotMatch() 只比较起始 PA；不能自行按 byte end 地址扩展
    // 匹配范围，否则跨 4KiB 的标量访问会与硬件 permission 选择不同。
    if (size_bytes == 0 || size_bytes > 8) return 1'b0;
    case (entry.a)
        PMA_PMP_A_TOR: begin
            return paddr >= previous.compare_addr && paddr < entry.compare_addr;
        end
        PMA_PMP_A_NA4: begin
            na4_base = {entry.addr_raw, 2'b00};
            return paddr >= na4_base && paddr < na4_base + 4;
        end
        PMA_PMP_A_NAPOT: begin
            return (paddr & ~entry.match_mask) ==
                   (entry.compare_addr & ~entry.match_mask);
        end
        default: return 1'b0;
    endcase
endfunction:entry_matches

function bit memblock_pma_pmp_model::find_first_match(
    input pma_pmp_table_snapshot snapshot,
    input bit [47:0] paddr,
    input int unsigned size_bytes,
    input bit debug,
    output int signed hit_index,
    output pma_pmp_entry_t hit_entry
);
    pma_pmp_entry_t previous;

    hit_index = -1;
    init_entry(hit_entry, 0);
    if (snapshot == null) return 1'b0;
    for (int unsigned i = 0; i < PMA_PMP_ENTRY_COUNT; i++) begin
        if (i == 0) init_entry(previous, 0);
        else previous = snapshot.entries[i - 1];
        if (paddr >= PMA_PMP_DEBUG_START && paddr <= PMA_PMP_DEBUG_END && !debug) begin
            continue;
        end
        if (entry_matches(snapshot.entries[i], previous, paddr, size_bytes)) begin
            hit_index = i;
            hit_entry = snapshot.entries[i];
            return 1'b1;
        end
    end
    return 1'b0;
endfunction:find_first_match

function bit memblock_pma_pmp_model::evaluate(
    input pma_pmp_request_context_t request,
    output pma_pmp_eval_t result
);
    pma_pmp_table_snapshot pmp_table;
    pma_pmp_table_snapshot pma_table;
    pma_pmp_entry_t pmp_entry;
    pma_pmp_entry_t pma_entry;
    bit pmp_default_allow;

    result = '{default:'0};
    result.pmp_hit_index = -1;
    result.pma_hit_index = -1;
    result.pmp_generation = request.pmp_generation;
    result.pma_generation = request.pma_generation;
    result.capture_sample = request.capture_sample;
    result.cmd = request.cmd;
    if (!ready || !request.valid || !request.translation_success) begin
        return 1'b1;
    end
    pmp_table = get_table_snapshot(1'b0, request.pmp_generation);
    pma_table = get_table_snapshot(1'b1, request.pma_generation);
    if (pmp_table == null || pma_table == null) begin
        return 1'b0;
    end

    result.valid = 1'b1;
    result.translation_eligible = 1'b1;
    result.pmp_hit = find_first_match(pmp_table, request.paddr, request.size_bytes,
                                      request.debug, result.pmp_hit_index, pmp_entry);
    result.pma_hit = find_first_match(pma_table, request.paddr, request.size_bytes,
                                      request.debug, result.pma_hit_index, pma_entry);

    pmp_default_allow = request.priv_mode > 2'd1;
    if (!result.pmp_hit) begin
        init_entry(pmp_entry, PMA_PMP_ENTRY_COUNT);
        pmp_entry.r = pmp_default_allow;
        pmp_entry.w = pmp_default_allow;
        pmp_entry.x = pmp_default_allow;
    end else if (pmp_default_allow && !pmp_entry.lock) begin
        pmp_entry.r = 1'b1;
        pmp_entry.w = 1'b1;
        pmp_entry.x = 1'b1;
    end
    if (!result.pma_hit) begin
        init_entry(pma_entry, PMA_PMP_ENTRY_COUNT);
    end

    result.pmp_ld_fault = cmd_is_load(request.cmd) && !pmp_entry.r;
    result.pmp_st_fault = cmd_is_store(request.cmd) && !pmp_entry.w;
    result.pmp_instr_fault = cmd_is_instr(request.cmd) && !pmp_entry.x;
    result.pma_atomic_fault = cmd_is_atomic(request.cmd) && !pma_entry.atomic;
    result.pma_ld_fault = (cmd_is_load(request.cmd) && !pma_entry.r) ||
                          (request.cmd == PMA_PMP_CMD_ATOM_READ && !pma_entry.atomic);
    result.pma_st_fault = (request.cmd == PMA_PMP_CMD_WRITE && !pma_entry.w) ||
                          (request.cmd == PMA_PMP_CMD_ATOM_WRITE &&
                           (!pma_entry.w || !pma_entry.atomic));
    result.pma_instr_fault = cmd_is_instr(request.cmd) && !pma_entry.x;
    // V2 PMPKeyIDBits is zero. Keep the generic result field so a future
    // profile can enable it without changing the RM-facing contract.
    result.keyid_fault = 1'b0;
    result.ld_fault = result.pmp_ld_fault || result.pma_ld_fault || result.keyid_fault;
    result.st_fault = result.pmp_st_fault || result.pma_st_fault || result.keyid_fault;
    result.instr_fault = result.pmp_instr_fault || result.pma_instr_fault || result.keyid_fault;
    result.cacheable = pma_entry.c;
    result.mmio = !pma_entry.c;
    result.atomic_allowed = pma_entry.atomic;
    return 1'b1;
endfunction:evaluate

function bit memblock_pma_pmp_model::evaluate_for_uid(
    input memblock_uid_t uid,
    input bit translation_success,
    input bit [47:0] paddr,
    input int unsigned size_bytes,
    input pma_pmp_cmd_e cmd,
    output pma_pmp_eval_t result
);
    pma_pmp_uid_context_t uid_context;
    pma_pmp_request_context_t request;

    result = '{default:'0};
    if (!read_uid_context(uid, uid_context)) return 1'b0;
    request = '{default:'0};
    request.valid = uid_context.valid;
    request.translation_success = translation_success;
    request.paddr = paddr;
    request.size_bytes = size_bytes;
    request.cmd = cmd;
    request.priv_mode = uid_context.priv_mode;
    request.debug = uid_context.debug;
    request.keyid_enable = uid_context.keyid_enable;
    request.cmode = uid_context.cmode;
    request.keyid = uid_context.keyid;
    request.csr_update_seq = uid_context.csr_update_seq;
    request.pmp_generation = uid_context.pmp_generation;
    request.pma_generation = uid_context.pma_generation;
    request.capture_sample = uid_context.capture_sample;
    return evaluate(request, result);
endfunction:evaluate_for_uid

function bit memblock_pma_pmp_model::evaluate_for_uid_epoch(
    input memblock_uid_t uid,
    input int unsigned dynamic_epoch,
    input bit translation_success,
    input bit [47:0] paddr,
    input int unsigned size_bytes,
    input pma_pmp_cmd_e cmd,
    output pma_pmp_eval_t result
);
    pma_pmp_uid_context_t uid_context;
    pma_pmp_request_context_t request;

    result = '{default:'0};
    if (!read_uid_context_for_epoch(uid, dynamic_epoch, uid_context)) return 1'b0;
    request = '{default:'0};
    request.valid = uid_context.valid;
    request.translation_success = translation_success;
    request.paddr = paddr;
    request.size_bytes = size_bytes;
    request.cmd = cmd;
    request.priv_mode = uid_context.priv_mode;
    request.debug = uid_context.debug;
    request.keyid_enable = uid_context.keyid_enable;
    request.cmode = uid_context.cmode;
    request.keyid = uid_context.keyid;
    request.csr_update_seq = uid_context.csr_update_seq;
    request.pmp_generation = uid_context.pmp_generation;
    request.pma_generation = uid_context.pma_generation;
    request.capture_sample = uid_context.capture_sample;
    return evaluate(request, result);
endfunction:evaluate_for_uid_epoch

function void memblock_pma_pmp_model::make_base_af_view(
    input pma_pmp_eval_t result,
    output pma_pmp_af_view_t view
);
    view = '{default:'0};
    view.pmp_hit_index = result.pmp_hit_index;
    view.pma_hit_index = result.pma_hit_index;
    view.pmp_generation = result.pmp_generation;
    view.pma_generation = result.pma_generation;
    view.capture_sample = result.capture_sample;
    if (!result.valid || !result.translation_eligible) begin
        return;
    end
    view.valid = 1'b1;
    view.translation_eligible = 1'b1;
    view.pmp_ld_fault = result.pmp_ld_fault;
    view.pmp_st_fault = result.pmp_st_fault;
    view.pmp_instr_fault = result.pmp_instr_fault;
    view.pma_ld_fault = result.pma_ld_fault;
    view.pma_st_fault = result.pma_st_fault;
    view.pma_instr_fault = result.pma_instr_fault;
    view.keyid_fault = result.keyid_fault;
    view.pma_atomic_fault = result.pma_atomic_fault;
    view.base_ld_access_fault = result.ld_fault;
    view.base_st_access_fault = result.st_fault;
    view.base_instr_access_fault = result.instr_fault;
    view.ld_access_fault = result.ld_fault;
    view.st_access_fault = result.st_fault;
    view.instr_access_fault = result.instr_fault;
    if (view.base_ld_access_fault || view.base_st_access_fault ||
        view.base_instr_access_fault) begin
        view.af_decided = 1'b1;
    end else if ((result.cmd == PMA_PMP_CMD_READ || result.cmd == PMA_PMP_CMD_WRITE) &&
                 !result.cacheable) begin
        view.dcache_fact_needed_for_c = 1'b1;
        view.af_decided = 1'b0;
    end else begin
        view.af_decided = 1'b1;
    end
endfunction:make_base_af_view

function bit memblock_pma_pmp_model::finalize_cache_path_af(
    input pma_pmp_eval_t result,
    input pma_pmp_af_view_t base_view,
    input pma_pmp_dcache_fact_e dcache_fact,
    output pma_pmp_af_view_t final_view
);
    final_view = base_view;
    if (!base_view.valid || !base_view.dcache_fact_needed_for_c ||
        (dcache_fact != PMA_PMP_DCACHE_FACT_YES &&
         dcache_fact != PMA_PMP_DCACHE_FACT_NO)) begin
        return 1'b0;
    end
    final_view.dcache_fact_needed_for_c = 1'b0;
    final_view.af_decided = 1'b1;
    if (dcache_fact == PMA_PMP_DCACHE_FACT_YES) begin
        final_view.pma_cache_path_fault = 1'b1;
        if (result.cmd == PMA_PMP_CMD_READ) final_view.ld_access_fault = 1'b1;
        else if (result.cmd == PMA_PMP_CMD_WRITE) final_view.st_access_fault = 1'b1;
    end
    return 1'b1;
endfunction:finalize_cache_path_af

function bit memblock_pma_pmp_model::read_entry(
    input bit is_pma,
    input int unsigned index,
    output pma_pmp_entry_t entry
);
    entry = '{default:'0};
    if (!ready || index >= PMA_PMP_ENTRY_COUNT) return 1'b0;
    entry = is_pma ? pma_entries[index] : pmp_entries[index];
    return 1'b1;
endfunction:read_entry

function bit memblock_pma_pmp_model::check_invariants(output string message);
    message = "";
    if (!ready || !pmp_history.exists(pmp_generation) ||
        !pma_history.exists(pma_generation)) begin
        message = "PMA/PMP model is not ready or current generation has no snapshot";
        return 1'b0;
    end
    for (int unsigned i = 0; i < PMA_PMP_ENTRY_COUNT; i++) begin
        if (pmp_entries[i].w && !pmp_entries[i].r) begin
            message = $sformatf("PMP entry %0d violates W implies R", i);
            return 1'b0;
        end
        if (pma_entries[i].w && !pma_entries[i].r) begin
            message = $sformatf("PMA entry %0d violates W implies R", i);
            return 1'b0;
        end
    end
    return 1'b1;
endfunction:check_invariants

`include "memblock_pma_pmp_profile_v2.svh"

`endif
