`ifndef RM_LS_CORE_SV
`define RM_LS_CORE_SV

// 纯值型、无 UVM 依赖的标量 Load/Store 程序序核心。
// 该核心只拥有 expected 状态；框架 owner、DUT request 和 overlay 均不写入这里。
typedef enum int unsigned {
    RM_LS_KIND_OTHER = 0,
    RM_LS_KIND_LOAD,
    RM_LS_KIND_STORE,
    RM_LS_KIND_ATOMIC
} rm_ls_kind_e;

typedef enum int unsigned {
    RM_ATOMIC_NONE = 0,
    RM_ATOMIC_LR,
    RM_ATOMIC_SC,
    RM_ATOMIC_AMO,
    RM_ATOMIC_CAS
} rm_atomic_kind_e;

typedef enum int unsigned {
    RM_MEMORY_EFFECT_STORE = 0,
    RM_MEMORY_EFFECT_AMO,
    RM_MEMORY_EFFECT_SC_SUCCESS,
    RM_MEMORY_EFFECT_CAS_SUCCESS
} rm_memory_effect_kind_e;

typedef enum bit {
    RM_RESV_NONE,
    RM_RESV_POSSIBLE
} rm_reservation_state_e;

typedef enum bit {
    RM_SC_NOWRITE_PENDING,
    RM_SC_NOWRITE_FINAL_CHECK_STORE
} rm_sc_no_write_evidence_e;

typedef enum int unsigned {
    RM_LS_ERR_NONE = 0,
    RM_LS_ERR_CONFIG_NOT_READY,
    RM_LS_ERR_MAIN_NOT_READY,
    RM_LS_ERR_MAIN_VADDR_MISMATCH,
    RM_LS_ERR_DUT_ROB_MISMATCH,
    RM_LS_ERR_ACTUAL_ROB_MISMATCH,
    RM_LS_ERR_TRANSLATION_NOT_READY,
    RM_LS_ERR_TLB_CONTEXT_NOT_READY,
    RM_LS_ERR_TLB_MODE_UNSUPPORTED,
    RM_LS_ERR_TLB_ENTRY_NOT_READY,
    RM_LS_ERR_BARE_PA_OUT_OF_RANGE,
    RM_LS_ERR_TLB_ENTRY_INCONSISTENT,
    RM_LS_ERR_ORIGINAL_L2_NOT_READY,
    RM_LS_ERR_D_ERROR_NOT_READY,
    RM_LS_ERR_LOAD_ACTUAL_NOT_READY,
    RM_LS_ERR_STORE_MASK_MISMATCH,
    RM_LS_ERR_EXCEPTION_MISMATCH,
    RM_LS_ERR_LOAD_MISMATCH,
    RM_LS_ERR_OVERLAY_NOT_READY,
    RM_LS_ERR_OVERLAY_BYTE_MISSING,
    RM_LS_ERR_OVERLAY_BYTE_CORRUPT,
    RM_LS_ERR_OVERLAY_MISMATCH,
    RM_LS_ERR_ATOMIC_OPERAND_NOT_READY,
    RM_LS_ERR_ATOMIC_WB_NOT_READY,
    RM_LS_ERR_ATOMIC_WB_COUNT_MISMATCH,
    RM_LS_ERR_ATOMIC_WB_IDENTITY_MISMATCH,
    RM_LS_ERR_ATOMIC_DATA_MISMATCH,
    RM_LS_ERR_ATOMIC_RESULT_MISMATCH,
    RM_LS_ERR_ATOMIC_MEMORY_EFFECT_MISMATCH,
    RM_LS_ERR_ATOMIC_SIZE_UNSUPPORTED,
    RM_LS_ERR_ATOMIC_ALIGNMENT_MISMATCH,
    RM_LS_ERR_SC_SUCCESS_WITHOUT_RESERVATION,
    RM_LS_ERR_SC_RESULT_MISMATCH,
    RM_LS_ERR_SC_FAILURE_MEMORY_EFFECT,
    RM_LS_ERR_CAS_RESULT_MISMATCH,
    RM_LS_ERR_CASQ_WB_PAIR_MISMATCH
} rm_ls_error_e;

function automatic bit rm_ls_decode_translation_path(
    input bit [1:0] s2xlate,
    input bit       is_hypervisor_inst,
    input bit       priv_virt,
    input bit [1:0] priv_dmode,
    input bit [3:0] satp_mode,
    input bit [3:0] vsatp_mode,
    input bit [3:0] hgatp_mode,
    output bit      s1_active,
    output bit      s2_active,
    output bit      bare_identity,
    output rm_ls_error_e error_code
);
    bit [1:0] expected_s2xlate;
    bit [3:0] selected_s1_mode;
    bit [3:0] selected_s2_mode;
    bit       s1_selected;
    bit       s2_selected;

    s1_active = 1'b0;
    s2_active = 1'b0;
    bare_identity = 1'b0;
    error_code = RM_LS_ERR_NONE;
    selected_s1_mode = '0;
    selected_s2_mode = '0;
    s1_selected = 1'b0;
    s2_selected = 1'b0;

    // Normal scalar memory accesses bypass address translation in M-mode.
    // Hypervisor load/store instructions derive their effective mode from
    // SPVP in the DUT and therefore keep the existing stage decode below.
    if (!is_hypervisor_inst && priv_dmode == 2'b11) begin
        bare_identity = 1'b1;
        return 1'b1;
    end

    if (!(priv_virt || is_hypervisor_inst)) expected_s2xlate = 2'd0;
    else if (vsatp_mode != 4'd0 && hgatp_mode != 4'd0) expected_s2xlate = 2'd3;
    else if (vsatp_mode == 4'd0) expected_s2xlate = 2'd2;
    else if (hgatp_mode == 4'd0) expected_s2xlate = 2'd1;
    else expected_s2xlate = 2'd0;

    if (s2xlate != expected_s2xlate) begin
        error_code = RM_LS_ERR_TLB_ENTRY_INCONSISTENT;
        return 1'b0;
    end

    case (s2xlate)
        2'd0: begin
            s1_selected = 1'b1;
            selected_s1_mode = satp_mode;
        end
        2'd1: begin
            s1_selected = 1'b1;
            selected_s1_mode = vsatp_mode;
        end
        2'd2: begin
            s2_selected = 1'b1;
            selected_s2_mode = hgatp_mode;
        end
        2'd3: begin
            s1_selected = 1'b1;
            s2_selected = 1'b1;
            selected_s1_mode = vsatp_mode;
            selected_s2_mode = hgatp_mode;
        end
        default: begin
            error_code = RM_LS_ERR_TLB_ENTRY_INCONSISTENT;
            return 1'b0;
        end
    endcase

    if ((s1_selected && !(selected_s1_mode inside {4'd0, 4'd8, 4'd9})) ||
        (s2_selected && !(selected_s2_mode inside {4'd0, 4'd8, 4'd9}))) begin
        error_code = RM_LS_ERR_TLB_MODE_UNSUPPORTED;
        return 1'b0;
    end

    s1_active = s1_selected && selected_s1_mode != 4'd0;
    s2_active = s2_selected && selected_s2_mode != 4'd0;
    bare_identity = !s1_active && !s2_active;
    return 1'b1;
endfunction

typedef struct packed {
    bit        flag;
    bit [15:0] value;
} rm_ls_rob_key_t;

class rm_ls_program_item_t;
    bit             valid;
    int unsigned    uid;
    rm_ls_kind_e    kind;
    bit [8:0]       op;
    bit [63:0]      computed_vaddr;
    bit [63:0]      main_vaddr;
    longint unsigned pa_by_byte[16];
    bit [15:0]      pa_valid_mask;
    bit [85:0]      translation_key;
    int unsigned    size_bytes;
    bit             is_signed;
    // Main-table FP_LOAD is the independent expected source for uop.fpWen.
    // Do not derive expected NaN boxing from the DUT writeback metadata.
    bit             fp_wen;
    rm_ls_rob_key_t rob;
    bit             translation_valid;
    bit [23:0]      expected_exception;
    // 中文注释：这些字段只保存 PMA/PMP 与 DCache ledger 的来源诊断，不参与
    // transaction owner 生命周期；最终 architectural cause 仍由 exception vector 选择。
    bit             expected_pmp_fault;
    bit             expected_pma_fault;
    bit             expected_keyid_fault;
    bit             expected_pma_atomic_fault;
    bit             expected_pma_cache_path_fault;
    bit             expected_dcache_denied;
    bit             expected_dcache_corrupt;
    // 中文注释：Uncache异常来源与L2/DCache分开保存，避免日志把TL D错误
    // 误称为DCache错误。路径分类在commit item构建时冻结，写回比较时只读。
    bit             expected_uncache_denied;
    bit             expected_uncache_corrupt;
    bit             all_pma_mmio;
    bit             all_normal_cacheable;
    bit             uncache_path;
    bit             lq_flag;
    bit [MEMBLOCK_LQ_VALUE_W-1:0] lq_value;
    // 中文注释：当前 Load/Store 的任一翻译 byte 是否因 PBMT 非零且对应
    // PBMTE 关闭而额外产生 S1 PF/S2 GPF。由 RM 在 commit 构建期设置，
    // 仅用于异常期望诊断，不参与 live TLB entry 生命周期。
    bit             expected_pbmt_forced_s1_pf;
    bit             expected_pbmt_forced_s2_gpf;
    int unsigned    dynamic_epoch;
    longint unsigned access_sample;
    bit [127:0]     store_data;
    bit [15:0]      store_byte_mask;
    rm_atomic_kind_e atomic_kind;
    bit [3:0]       atomic_std_valid_mask;
    bit [63:0]      atomic_std_data[4];
    bit [127:0]     atomic_operand;
    bit [127:0]     atomic_compare_value;
    bit [127:0]     atomic_new_value;
    bit [127:0]     expected_old_value;
    bit [127:0]     expected_memory_value;
    bit             expected_memory_write;
    bit             sc_success_allowed;
    bit             expected_cas_success;
    int unsigned    expected_atomic_wb_count;
    bit [7:0]       atomic_expected_pdest[2];
    bit [1:0]       atomic_expected_pdest_valid;
    longint unsigned atomic_first_issue_sample;

    function new(int unsigned uid_i = 0);
        valid = 0; uid = uid_i; kind = RM_LS_KIND_OTHER; op = '0;
        computed_vaddr = '0; main_vaddr = '0; pa_valid_mask = '0;
        translation_key = '0; size_bytes = 0; is_signed = 0; fp_wen = 0;
        rob = '{default:'0}; translation_valid = 0; expected_exception = '0;
        expected_pmp_fault = 1'b0; expected_pma_fault = 1'b0;
        expected_keyid_fault = 1'b0; expected_pma_atomic_fault = 1'b0;
        expected_pma_cache_path_fault = 1'b0;
        expected_dcache_denied = 1'b0; expected_dcache_corrupt = 1'b0;
        expected_uncache_denied = 1'b0; expected_uncache_corrupt = 1'b0;
        all_pma_mmio = 1'b0; all_normal_cacheable = 1'b0;
        uncache_path = 1'b0; lq_flag = 1'b0; lq_value = '0;
        expected_pbmt_forced_s1_pf = 1'b0;
        expected_pbmt_forced_s2_gpf = 1'b0;
        dynamic_epoch = 0; access_sample = 0;
        store_data = '0; store_byte_mask = '0;
        atomic_kind = RM_ATOMIC_NONE; atomic_std_valid_mask = '0;
        atomic_operand = '0; atomic_compare_value = '0; atomic_new_value = '0;
        expected_old_value = '0; expected_memory_value = '0;
        expected_memory_write = 1'b0; sc_success_allowed = 1'b0;
        expected_cas_success = 1'b0; expected_atomic_wb_count = 0;
        atomic_expected_pdest_valid = '0; atomic_first_issue_sample = 0;
        foreach (atomic_std_data[i]) atomic_std_data[i] = '0;
        foreach (atomic_expected_pdest[i]) atomic_expected_pdest[i] = '0;
        foreach (pa_by_byte[i]) pa_by_byte[i] = '0;
    endfunction

    function bit set_bare_identity_geometry(output int unsigned bad_byte_index);
        bit [63:0] byte_va;

        bad_byte_index = 0;
        pa_valid_mask = '0;
        if (size_bytes == 0 || size_bytes > 16) return 1'b0;
        for (int unsigned i = 0; i < size_bytes; i++) begin
            byte_va = computed_vaddr + i;
            if (byte_va < computed_vaddr || |byte_va[63:48]) begin
                bad_byte_index = i;
                return 1'b0;
            end
            pa_by_byte[i] = {16'b0, byte_va[47:0]};
            pa_valid_mask[i] = 1'b1;
        end
        return 1'b1;
    endfunction
endclass

// 每笔正常 Store 的 transaction-level cache entry。逐 PA byte 表仍负责最终内存状态和
// Load forwarding；该 entry 保存主表给出的长度和身份，供 check_store 整笔比较。
class rm_ls_store_cache_entry_t;
    bit              valid;
    int unsigned     uid;
    rm_ls_rob_key_t  rob;
    rm_memory_effect_kind_e source_kind;
    bit [63:0]       vaddr;
    longint unsigned pa_by_byte[16];
    int unsigned     size_bytes;
    bit [15:0]       byte_mask;
    bit [127:0]      committed_data;

    function new();
        valid = 1'b0;
        uid = 0;
        rob = '{default:'0};
        source_kind = RM_MEMORY_EFFECT_STORE;
        vaddr = '0;
        size_bytes = 0;
        byte_mask = '0;
        committed_data = '0;
        foreach (pa_by_byte[i]) pa_by_byte[i] = '0;
    endfunction

    function void capture_effect(rm_ls_program_item_t item,
                                 rm_memory_effect_kind_e source_kind_i,
                                 bit [127:0] data_i,
                                 bit [15:0] mask_i);
        valid = 1'b0;
        if (item == null) return;
        if (!item.valid || !(item.kind inside {RM_LS_KIND_STORE,
                                               RM_LS_KIND_ATOMIC})) return;
        valid = 1'b1;
        uid = item.uid;
        rob = item.rob;
        source_kind = source_kind_i;
        vaddr = item.computed_vaddr;
        size_bytes = item.size_bytes;
        byte_mask = mask_i;
        committed_data = data_i;
        foreach (pa_by_byte[i]) pa_by_byte[i] = item.pa_by_byte[i];
    endfunction:capture_effect

    function void copy_from(rm_ls_store_cache_entry_t source);
        if (source == null) return;
        valid = source.valid;
        uid = source.uid;
        rob = source.rob;
        source_kind = source.source_kind;
        vaddr = source.vaddr;
        size_bytes = source.size_bytes;
        byte_mask = source.byte_mask;
        committed_data = source.committed_data;
        foreach (pa_by_byte[i]) pa_by_byte[i] = source.pa_by_byte[i];
    endfunction
endclass

class rm_atomic_actual_t;
    bit                  valid;
    int unsigned         uid;
    rm_ls_rob_key_t      rob;
    int unsigned         dynamic_epoch;
    int unsigned         wb_count;
    logic                data_valid[2];
    logic [63:0]         data[2];
    logic [23:0]         exception_vec[2];
    logic                pdest_valid[2];
    logic [7:0]          pdest[2];
    logic                rf_wen_valid[2];
    logic                rf_wen[2];
    longint unsigned     sample_seq[2];
    longint unsigned     sample_cycle[2];
    function new();
        valid = 0; uid = 0; rob = '{default:'0}; dynamic_epoch = 0; wb_count = 0;
        foreach (data[i]) begin
            data_valid[i] = '0; data[i] = '0; exception_vec[i] = '0;
            pdest_valid[i] = '0; pdest[i] = '0;
            rf_wen_valid[i] = '0; rf_wen[i] = '0;
            sample_seq[i] = 0; sample_cycle[i] = 0;
        end
    endfunction
endclass

class rm_sc_no_write_obligation_t;
    bit                  valid;
    int unsigned         uid;
    rm_ls_rob_key_t      rob;
    int unsigned         dynamic_epoch;
    longint unsigned     pa_by_byte[16];
    bit [15:0]           byte_mask;
    int unsigned         size_bytes;
    bit [127:0]          expected_before_value;
    bit [15:0]           expected_before_valid_mask;
    bit [15:0]           shadowed_mask;
    bit [15:0]           checked_mask;
    bit                  resolved;
    rm_sc_no_write_evidence_e evidence_kind;
    function new();
        valid = 0; uid = 0; rob = '{default:'0}; dynamic_epoch = 0;
        byte_mask = '0; size_bytes = 0; expected_before_value = '0;
        expected_before_valid_mask = '0;
        shadowed_mask = '0; checked_mask = '0; resolved = 0;
        evidence_kind = RM_SC_NOWRITE_PENDING;
        foreach (pa_by_byte[i]) pa_by_byte[i] = '0;
    endfunction
endclass

class rm_ls_load_actual_t;
    bit             valid;
    int unsigned    uid;
    rm_ls_rob_key_t rob;
    bit             data_valid;
    logic [63:0]    data;
    logic [23:0]    exception_vec;
    longint unsigned cycle;
    function new();
        valid = 0; uid = 0; rob = '{default:'0}; data_valid = 0;
        data = '0; exception_vec = '0; cycle = 0;
    endfunction
endclass

class rm_ls_history_record_t;
    int unsigned    uid;
    rm_ls_kind_e    kind;
    rm_ls_rob_key_t rob;
    bit [63:0]      vaddr;
    longint unsigned first_pa;
    bit [63:0]      expected_data;
    logic [63:0]    actual_data;
    bit [23:0]      expected_exception;
    logic [23:0]    actual_exception;
    bit             compare_pass;
    rm_ls_error_e   error_code;
    string          detail;
    function new();
        uid = 0; kind = RM_LS_KIND_OTHER; rob = '{default:'0};
        vaddr = '0; first_pa = '0; expected_data = '0; actual_data = '0;
        expected_exception = '0; actual_exception = '0; compare_pass = 0;
        error_code = RM_LS_ERR_NONE; detail = "";
    endfunction
endclass

class rm_ls_model_t;
    bit             configured;
    int unsigned    main_count;
    int unsigned    current_uid;
    int unsigned    consumed_commit_count;
    int unsigned    error_count;
    rm_ls_error_e   last_error;
    string          last_error_text;

    rm_ls_program_item_t items[int unsigned];
    rm_ls_load_actual_t pending_load_actual[int unsigned];
    rm_atomic_actual_t pending_atomic_actual[int unsigned];
    bit                 backing_valid[longint unsigned];
    byte unsigned       backing_byte[longint unsigned];
    bit                 expected_store_valid[longint unsigned];
    byte unsigned       expected_store_byte[longint unsigned];
    rm_ls_store_cache_entry_t expected_store_entries[$];
    rm_sc_no_write_obligation_t sc_no_write_obligations[$];
    rm_reservation_state_e reservation_state;
    longint unsigned reservation_block_addr;
    int unsigned reservation_size_bytes;
    int unsigned reservation_lr_uid;
    rm_ls_rob_key_t reservation_lr_rob;
    longint unsigned reservation_create_sample;
    rm_ls_history_record_t history[$];

    function new(); reset(); endfunction

    function void reset();
        configured = 0; main_count = 0; current_uid = 0;
        consumed_commit_count = 0; error_count = 0;
        last_error = RM_LS_ERR_NONE; last_error_text = "";
        items.delete(); pending_load_actual.delete(); pending_atomic_actual.delete();
        backing_valid.delete(); backing_byte.delete();
        expected_store_valid.delete(); expected_store_byte.delete();
        expected_store_entries.delete(); sc_no_write_obligations.delete(); history.delete();
        reservation_state = RM_RESV_NONE; reservation_block_addr = '0;
        reservation_size_bytes = 0; reservation_lr_uid = 0;
        reservation_lr_rob = '{default:'0}; reservation_create_sample = 0;
    endfunction

    function bit configure(int unsigned count);
        reset();
        if (count == 0) begin
            set_error(RM_LS_ERR_CONFIG_NOT_READY, "main stimulus count is zero");
            return 0;
        end
        main_count = count; configured = 1; return 1;
    endfunction

    function void set_error(rm_ls_error_e code, string text);
        last_error = code; last_error_text = text; error_count++;
    endfunction

    // 已确认 ROB commit 的观测错误必须消费当前 uid，避免单笔错误阻断后续独立 uid。
    function bit consume_current_commit_error(input rm_ls_rob_key_t commit_rob,
                                              input rm_ls_error_e error_code,
                                              input string error_text);
        rm_ls_program_item_t item;

        if (!configured || current_uid >= main_count || !items.exists(current_uid)) begin
            set_error(RM_LS_ERR_MAIN_NOT_READY,
                      "cannot consume failed commit because current item is unavailable");
            return 1'b0;
        end
        item = items[current_uid];
        if (commit_rob != item.rob) begin
            set_error(RM_LS_ERR_DUT_ROB_MISMATCH,
                      $sformatf("cannot consume failed uid %0d: expected ROB %0d/%0d got %0d/%0d",
                                current_uid, item.rob.flag, item.rob.value,
                                commit_rob.flag, commit_rob.value));
            return 1'b0;
        end

        // 缺失 LDA 没有 DUT actual；不要向 history 写入默认值，避免把零值误读为
        // 真实 data/exception。失败原因由 last_error_text 和 UVM_ERROR 保留。
        set_error(error_code, error_text);
        if (item.kind == RM_LS_KIND_ATOMIC &&
            item.atomic_kind == RM_ATOMIC_SC &&
            (item.expected_exception == '0 ||
             item.expected_dcache_denied || item.expected_dcache_corrupt)) begin
            reservation_state = RM_RESV_NONE;
        end
        current_uid++;
        consumed_commit_count++;
        return 1'b1;
    endfunction

    function bit add_item(rm_ls_program_item_t item);
        if (!configured || item == null || !item.valid || item.uid >= main_count) begin
            set_error(RM_LS_ERR_MAIN_NOT_READY, "invalid item or uid outside main table");
            return 0;
        end
        if (item.computed_vaddr != item.main_vaddr) begin
            set_error(RM_LS_ERR_MAIN_VADDR_MISMATCH,
                      $sformatf("uid %0d computed VA 0x%0h main VA 0x%0h",
                                item.uid, item.computed_vaddr, item.main_vaddr));
            return 0;
        end
        items[item.uid] = item; return 1;
    endfunction

    function bit set_backing_byte(longint unsigned pa, byte unsigned value);
        if (!configured) return 0;
        backing_valid[pa] = 1; backing_byte[pa] = value; return 1;
    endfunction

    function bit push_load_actual(rm_ls_load_actual_t actual);
        if (!configured || actual == null || !actual.valid) return 0;
        pending_load_actual[actual.uid] = actual; return 1;
    endfunction

    function bit push_atomic_actual(rm_atomic_actual_t actual);
        if (!configured || actual == null || !actual.valid) return 0;
        pending_atomic_actual[actual.uid] = actual;
        return 1;
    endfunction

    function bit expected_store_byte_exists(longint unsigned pa);
        return expected_store_valid.exists(pa) && expected_store_valid[pa];
    endfunction

    function bit read_expected_store_byte(longint unsigned pa,
                                          output byte unsigned value);
        value = '0;
        if (!expected_store_byte_exists(pa)) return 0;
        value = expected_store_byte[pa];
        return 1;
    endfunction

    function bit read_expected_memory_byte(longint unsigned pa,
                                           output byte unsigned value);
        value = '0;
        if (read_expected_store_byte(pa, value)) return 1'b1;
        if (backing_valid.exists(pa) && backing_valid[pa]) begin
            value = backing_byte[pa];
            return 1'b1;
        end
        return 1'b0;
    endfunction

    function bit build_raw_memory_value(rm_ls_program_item_t item,
                                        output bit [127:0] value);
        value = '0;
        if (item == null || item.size_bytes == 0 || item.size_bytes > 16 ||
            !item.translation_valid) return 1'b0;
        for (int unsigned i = 0; i < item.size_bytes; i++) begin
            if (!item.pa_valid_mask[i] ||
                !read_expected_memory_byte(item.pa_by_byte[i], value[(i * 8) +: 8])) begin
                set_error(RM_LS_ERR_ORIGINAL_L2_NOT_READY,
                          $sformatf("uid %0d expected memory byte %0d PA=0x%0h unavailable",
                                    item.uid, i, item.pa_by_byte[i]));
                return 1'b0;
            end
        end
        return 1'b1;
    endfunction

    function void mark_sc_obligations_shadowed(rm_ls_program_item_t item,
                                                bit [15:0] write_mask);
        foreach (sc_no_write_obligations[obligation_index]) begin
            rm_sc_no_write_obligation_t obligation;
            obligation = sc_no_write_obligations[obligation_index];
            if (obligation == null || !obligation.valid) continue;
            for (int unsigned write_index = 0; write_index < item.size_bytes; write_index++) begin
                if (!write_mask[write_index]) continue;
                for (int unsigned watch_index = 0;
                     watch_index < obligation.size_bytes; watch_index++) begin
                    if (obligation.byte_mask[watch_index] &&
                        item.pa_by_byte[write_index] == obligation.pa_by_byte[watch_index]) begin
                        obligation.shadowed_mask[watch_index] = 1'b1;
                    end
                end
            end
        end
    endfunction

    function bit commit_memory_effect(rm_ls_program_item_t item,
                                      rm_memory_effect_kind_e source_kind,
                                      bit [127:0] data,
                                      bit [15:0] byte_mask);
        rm_ls_store_cache_entry_t effect_entry;
        bit [15:0] required_mask;

        if (item == null || !item.translation_valid || item.size_bytes == 0 ||
            item.size_bytes > 16) return 1'b0;
        required_mask = 16'hffff >> (16 - item.size_bytes);
        if (byte_mask != required_mask ||
            (item.pa_valid_mask & required_mask) != required_mask) begin
            set_error(RM_LS_ERR_ATOMIC_MEMORY_EFFECT_MISMATCH,
                      $sformatf("uid %0d invalid memory effect mask pa=0x%0h data=0x%0h required=0x%0h",
                                item.uid, item.pa_valid_mask, byte_mask, required_mask));
            return 1'b0;
        end
        mark_sc_obligations_shadowed(item, byte_mask);
        for (int unsigned i = 0; i < item.size_bytes; i++) begin
            expected_store_valid[item.pa_by_byte[i]] = 1'b1;
            expected_store_byte[item.pa_by_byte[i]] = data[(i * 8) +: 8];
        end
        effect_entry = new();
        effect_entry.capture_effect(item, source_kind, data, byte_mask);
        expected_store_entries.push_back(effect_entry);
        return 1'b1;
    endfunction

    function void create_sc_no_write_obligation(rm_ls_program_item_t item,
                                                 bit [127:0] expected_before);
        rm_sc_no_write_obligation_t obligation;
        byte unsigned expected_byte;
        obligation = new();
        obligation.valid = 1'b1;
        obligation.uid = item.uid;
        obligation.rob = item.rob;
        obligation.dynamic_epoch = item.dynamic_epoch;
        obligation.size_bytes = item.size_bytes;
        obligation.byte_mask = 16'hffff >> (16 - item.size_bytes);
        obligation.expected_before_value = expected_before;
        foreach (obligation.pa_by_byte[i]) begin
            obligation.pa_by_byte[i] = item.pa_by_byte[i];
            if (i < item.size_bytes &&
                read_expected_memory_byte(item.pa_by_byte[i], expected_byte)) begin
                obligation.expected_before_value[(i * 8) +: 8] = expected_byte;
                obligation.expected_before_valid_mask[i] = 1'b1;
            end
        end
        sc_no_write_obligations.push_back(obligation);
    endfunction

    function automatic bit [127:0] compute_amo_new_value(
        input bit [8:0] op,
        input int unsigned size_bytes,
        input bit [127:0] old_value,
        input bit [127:0] operand
    );
        bit [127:0] result;
        result = old_value;
        if (size_bytes == 4) begin
            case (op)
                MEMBLOCK_LSUOP_AMOSWAP_W: result[31:0] = operand[31:0];
                MEMBLOCK_LSUOP_AMOADD_W:  result[31:0] = old_value[31:0] + operand[31:0];
                MEMBLOCK_LSUOP_AMOXOR_W:  result[31:0] = old_value[31:0] ^ operand[31:0];
                MEMBLOCK_LSUOP_AMOAND_W:  result[31:0] = old_value[31:0] & operand[31:0];
                MEMBLOCK_LSUOP_AMOOR_W:   result[31:0] = old_value[31:0] | operand[31:0];
                MEMBLOCK_LSUOP_AMOMIN_W:  result[31:0] =
                    $signed(old_value[31:0]) < $signed(operand[31:0]) ?
                    old_value[31:0] : operand[31:0];
                MEMBLOCK_LSUOP_AMOMAX_W:  result[31:0] =
                    $signed(old_value[31:0]) > $signed(operand[31:0]) ?
                    old_value[31:0] : operand[31:0];
                MEMBLOCK_LSUOP_AMOMINU_W: result[31:0] =
                    old_value[31:0] < operand[31:0] ? old_value[31:0] : operand[31:0];
                MEMBLOCK_LSUOP_AMOMAXU_W: result[31:0] =
                    old_value[31:0] > operand[31:0] ? old_value[31:0] : operand[31:0];
                default: result = old_value;
            endcase
        end else if (size_bytes == 8) begin
            case (op)
                MEMBLOCK_LSUOP_AMOSWAP_D: result[63:0] = operand[63:0];
                MEMBLOCK_LSUOP_AMOADD_D:  result[63:0] = old_value[63:0] + operand[63:0];
                MEMBLOCK_LSUOP_AMOXOR_D:  result[63:0] = old_value[63:0] ^ operand[63:0];
                MEMBLOCK_LSUOP_AMOAND_D:  result[63:0] = old_value[63:0] & operand[63:0];
                MEMBLOCK_LSUOP_AMOOR_D:   result[63:0] = old_value[63:0] | operand[63:0];
                MEMBLOCK_LSUOP_AMOMIN_D:  result[63:0] =
                    $signed(old_value[63:0]) < $signed(operand[63:0]) ?
                    old_value[63:0] : operand[63:0];
                MEMBLOCK_LSUOP_AMOMAX_D:  result[63:0] =
                    $signed(old_value[63:0]) > $signed(operand[63:0]) ?
                    old_value[63:0] : operand[63:0];
                MEMBLOCK_LSUOP_AMOMINU_D: result[63:0] =
                    old_value[63:0] < operand[63:0] ? old_value[63:0] : operand[63:0];
                MEMBLOCK_LSUOP_AMOMAXU_D: result[63:0] =
                    old_value[63:0] > operand[63:0] ? old_value[63:0] : operand[63:0];
                default: result = old_value;
            endcase
        end
        return result;
    endfunction

    function automatic bit [63:0] format_atomic_wb(bit [127:0] old_value,
                                                    int unsigned size_bytes);
        if (size_bytes == 4) return {{32{old_value[31]}}, old_value[31:0]};
        return old_value[63:0];
    endfunction

    function int unsigned expected_store_entry_count();
        return expected_store_entries.size();
    endfunction

    function bit read_expected_store_entry(input int unsigned index,
                                           output rm_ls_store_cache_entry_t entry);
        entry = null;
        if (index >= expected_store_entries.size()) return 0;
        if (expected_store_entries[index] == null) return 0;
        if (!expected_store_entries[index].valid) return 0;
        entry = new();
        entry.copy_from(expected_store_entries[index]);
        return entry.valid;
    endfunction

    function bit build_store_entry_expected(input rm_ls_store_cache_entry_t entry,
                                            output bit [127:0] expected_data,
                                            output bit [15:0] expected_valid_mask);
        expected_data = '0;
        expected_valid_mask = '0;
        if (!configured || entry == null) begin
            set_error(RM_LS_ERR_OVERLAY_NOT_READY, "invalid Store cache entry");
            return 0;
        end
        if (!entry.valid || entry.size_bytes == 0 || entry.size_bytes > 16) begin
            set_error(RM_LS_ERR_OVERLAY_NOT_READY, "invalid Store cache entry");
            return 0;
        end
        for (int unsigned i = 0; i < entry.size_bytes; i++) begin
            if (!read_expected_store_byte(entry.pa_by_byte[i],
                                          expected_data[(i * 8) +: 8])) begin
                set_error(RM_LS_ERR_OVERLAY_MISMATCH,
                          $sformatf("Store uid %0d ROB %0d/%0d PA 0x%0h is absent from RM Store cache",
                                    entry.uid, entry.rob.flag, entry.rob.value,
                                    entry.pa_by_byte[i]));
                return 0;
            end
            expected_valid_mask[i] = 1'b1;
        end
        return expected_valid_mask == entry.byte_mask;
    endfunction

    function bit build_load_expected(rm_ls_program_item_t item,
                                     output bit [63:0] expected,
                                     output bit [23:0] expected_exception);
        expected = '0; expected_exception = item.expected_exception;
        if (item.expected_exception != '0) return 1;
        if (!item.translation_valid || item.size_bytes == 0 || item.size_bytes > 8) begin
            set_error(RM_LS_ERR_TRANSLATION_NOT_READY, $sformatf("uid %0d translation", item.uid));
            return 0;
        end
        for (int unsigned i = 0; i < item.size_bytes; i++) begin
            longint unsigned pa;
            if (!item.pa_valid_mask[i]) begin
                set_error(RM_LS_ERR_TRANSLATION_NOT_READY,
                          $sformatf("uid %0d byte %0d PA is invalid", item.uid, i));
                return 0;
            end
            pa = item.pa_by_byte[i];
            if (expected_store_byte_exists(pa))
                expected[(i * 8) +: 8] = expected_store_byte[pa];
            else if (backing_valid.exists(pa) && backing_valid[pa])
                expected[(i * 8) +: 8] = backing_byte[pa];
            else begin
                set_error(RM_LS_ERR_ORIGINAL_L2_NOT_READY,
                          $sformatf("uid %0d PA 0x%0h", item.uid, pa));
                return 0;
            end
        end
        expected = memblock_rm_format_load_data(
            expected, item.op, item.fp_wen, item.is_signed, item.size_bytes);
        return 1;
    endfunction

    // 抽象职责：按 V2 ExceptionNO.priorities 选择 Load/Store 的架构 trap cause。
    // 该函数只读 vector，不折叠或修改 raw bits；当没有本类访存异常时返回 -1。
    function automatic int signed select_arch_exception_cause(
        input logic [23:0] exception_vec,
        input bit          store_access
    );
        if (store_access) begin
            if (exception_vec[6]  === 1'b1) return 6;  // store address misaligned
            if (exception_vec[15] === 1'b1) return 15; // store page fault
            if (exception_vec[23] === 1'b1) return 23; // store guest page fault
            if (exception_vec[7]  === 1'b1) return 7;  // store access fault
            if (exception_vec[19] === 1'b1) return 19; // hardware error
        end else begin
            if (exception_vec[4]  === 1'b1) return 4;  // load address misaligned
            if (exception_vec[13] === 1'b1) return 13; // load page fault
            if (exception_vec[21] === 1'b1) return 21; // load guest page fault
            if (exception_vec[5]  === 1'b1) return 5;  // load access fault
            if (exception_vec[19] === 1'b1) return 19; // hardware error
        end
        return -1;
    endfunction

    // 抽象职责：比较架构可见 cause，同时保留 raw vector 是否完全相等供诊断。
    // 当 expected/actual 至少一方包含 PF/GPF 时，V2 允许同一 cause 携带额外 raw AF；
    // 没有 page/guest-page fault 时继续严格比较，避免掩盖普通 vector 差异。
    function automatic bit compare_arch_exception(
        input  logic [23:0] expected_exception,
        input  logic [23:0] actual_exception,
        input  bit           store_access,
        output bit           raw_match,
        output bit           cause_match
    );
        int signed expected_cause;
        int signed actual_cause;

        raw_match = expected_exception === actual_exception;
        expected_cause = select_arch_exception_cause(expected_exception, store_access);
        actual_cause = select_arch_exception_cause(actual_exception, store_access);
        cause_match = expected_cause == actual_cause;
        if ((expected_exception[store_access ? 15 : 13] === 1'b1) ||
            (expected_exception[store_access ? 23 : 21] === 1'b1) ||
            (actual_exception[store_access ? 15 : 13] === 1'b1) ||
            (actual_exception[store_access ? 23 : 21] === 1'b1)) begin
            return cause_match;
        end
        return raw_match;
    endfunction

    // 只读预检 Load 的 RM/LDA/status 异常，不记录错误、history 或推进 uid。
    // any_exception 仅控制是否需要构造正常数据期望，不能反写 RM expectation。
    function bit precheck_load_exception(
        input rm_ls_program_item_t item,
        input logic [23:0] terminal_exception,
        output bit exception_match,
        output bit any_exception
    );
        rm_ls_load_actual_t actual;
        bit raw_exception_match_actual;
        bit cause_exception_match_actual;
        bit raw_exception_match_terminal;
        bit cause_exception_match_terminal;
        bit actual_exception_ok;
        bit terminal_exception_ok;

        exception_match = 1'b0;
        any_exception = 1'b1;
        if (item == null || item.kind != RM_LS_KIND_LOAD ||
            !pending_load_actual.exists(item.uid)) return 1'b0;

        actual = pending_load_actual[item.uid];
        actual_exception_ok = compare_arch_exception(
            item.expected_exception, actual.exception_vec, 1'b0,
            raw_exception_match_actual, cause_exception_match_actual);
        terminal_exception_ok = compare_arch_exception(
            item.expected_exception, terminal_exception, 1'b0,
            raw_exception_match_terminal, cause_exception_match_terminal);
        exception_match = actual_exception_ok && terminal_exception_ok;
        any_exception = item.expected_exception != '0 ||
                        actual.exception_vec !== '0 ||
                        terminal_exception !== '0;
        return 1'b1;
    endfunction

    function bit compare_load(rm_ls_program_item_t item,
                              logic [23:0] terminal_exception);
        rm_ls_history_record_t rec;
        bit [63:0] expected;
        bit [23:0] expected_exception;
        rm_ls_load_actual_t actual;
        bit raw_exception_match_actual;
        bit cause_exception_match_actual;
        bit raw_exception_match_terminal;
        bit cause_exception_match_terminal;
        bit exception_match;
        bit any_exception;

        rec = new();
        rec.uid = item.uid; rec.kind = item.kind; rec.rob = item.rob;
        rec.vaddr = item.computed_vaddr;
        if (item.pa_valid_mask[0]) rec.first_pa = item.pa_by_byte[0];
        if (!pending_load_actual.exists(item.uid)) begin
            set_error(RM_LS_ERR_LOAD_ACTUAL_NOT_READY,
                      $sformatf("uid %0d Load actual missing", item.uid));
            return 0;
        end
        actual = pending_load_actual[item.uid];
        if (actual.rob != item.rob) begin
            set_error(RM_LS_ERR_ACTUAL_ROB_MISMATCH,
                      $sformatf("uid %0d main ROB %0d/%0d Load actual ROB %0d/%0d",
                                item.uid, item.rob.flag, item.rob.value,
                                actual.rob.flag, actual.rob.value));
            return 0;
        end
        expected_exception = item.expected_exception;
        rec.actual_data = actual.data;
        rec.expected_exception = expected_exception;
        rec.actual_exception = actual.exception_vec;
        if (!precheck_load_exception(item, terminal_exception,
                                     exception_match, any_exception)) begin
            set_error(RM_LS_ERR_LOAD_ACTUAL_NOT_READY,
                      $sformatf("uid %0d Load exception precheck unavailable", item.uid));
            return 0;
        end
        if (!exception_match) begin
            rec.error_code = RM_LS_ERR_EXCEPTION_MISMATCH;
            rec.detail = $sformatf("uid %0d Load exception expected=0x%0h status=0x%0h actual=0x%0h",
                                   item.uid, expected_exception, terminal_exception,
                                   actual.exception_vec);
            set_error(rec.error_code, rec.detail);
            history.push_back(rec);
            return 0;
        end

        // 仅用于保留 raw-vector 诊断；通过/失败结论统一来自上面的只读 precheck。
        void'(compare_arch_exception(
            expected_exception, actual.exception_vec, 1'b0,
            raw_exception_match_actual, cause_exception_match_actual));
        void'(compare_arch_exception(
            expected_exception, terminal_exception, 1'b0,
            raw_exception_match_terminal, cause_exception_match_terminal));
        if ((!raw_exception_match_actual || !raw_exception_match_terminal) &&
            cause_exception_match_actual && cause_exception_match_terminal) begin
            `uvm_info("RM_LS_EXCEPTION_DIAG",
                      $sformatf("uid %0d Load raw exception differs but architectural cause matches: expected=0x%06h status=0x%06h actual=0x%06h cause=%0d",
                                item.uid, expected_exception, terminal_exception,
                                actual.exception_vec,
                                select_arch_exception_cause(actual.exception_vec, 1'b0)),
                      UVM_LOW)
        end
        if (any_exception) begin
            rec.compare_pass = !actual.data_valid;
        end else begin
            if (!build_load_expected(item, expected, expected_exception)) return 0;
            rec.expected_data = expected;
            rec.compare_pass = actual.data_valid && (actual.data === expected);
        end
        rec.error_code = rec.compare_pass ? RM_LS_ERR_NONE : RM_LS_ERR_LOAD_MISMATCH;
        rec.detail = rec.compare_pass ? "Load match" : "Load data/data_valid mismatch";
        if (!rec.compare_pass) set_error(rec.error_code, rec.detail);
        history.push_back(rec); return rec.compare_pass;
    endfunction

    function bit commit_store(rm_ls_program_item_t item,
                              logic [23:0] commit_exception);
        rm_ls_history_record_t rec;
        bit [15:0] required_mask;
        bit raw_exception_match;
        bit cause_exception_match;

        rec = new(); rec.uid = item.uid; rec.kind = item.kind; rec.rob = item.rob;
        rec.vaddr = item.computed_vaddr;
        if (item.pa_valid_mask[0]) rec.first_pa = item.pa_by_byte[0];
        rec.expected_exception = item.expected_exception;
        rec.actual_exception = commit_exception;
        if (!compare_arch_exception(item.expected_exception, commit_exception,
                                    1'b1, raw_exception_match,
                                    cause_exception_match)) begin
            rec.error_code = RM_LS_ERR_EXCEPTION_MISMATCH;
            rec.detail = $sformatf("uid %0d Store exception expected=0x%0h status=0x%0h",
                                   item.uid, item.expected_exception, commit_exception);
            set_error(rec.error_code, rec.detail); history.push_back(rec); return 0;
        end
        if (!raw_exception_match) begin
            `uvm_info("RM_LS_EXCEPTION_DIAG",
                      $sformatf("uid %0d Store raw exception differs but architectural cause matches: expected=0x%06h actual=0x%06h cause=%0d",
                                item.uid, item.expected_exception, commit_exception,
                                select_arch_exception_cause(commit_exception, 1'b1)),
                      UVM_LOW)
        end
        if (item.expected_exception != '0) begin
            rec.compare_pass = 1'b1; rec.error_code = RM_LS_ERR_NONE;
            rec.detail = "fault Store exception match; no Store effect";
            history.push_back(rec); return 1;
        end
        if (!item.translation_valid || item.size_bytes == 0 || item.size_bytes > 16) begin
            set_error(RM_LS_ERR_TRANSLATION_NOT_READY,
                      $sformatf("uid %0d Store translation", item.uid));
            return 0;
        end
        required_mask = 16'hffff >> (16 - item.size_bytes);
        if (item.store_byte_mask != required_mask) begin
            set_error(RM_LS_ERR_STORE_MASK_MISMATCH,
                      $sformatf("uid %0d main Store mask expected=0x%0h actual=0x%0h",
                                item.uid, required_mask, item.store_byte_mask));
            return 0;
        end
        for (int unsigned i = 0; i < item.size_bytes; i++) begin
            if (!item.pa_valid_mask[i]) begin
                set_error(RM_LS_ERR_TRANSLATION_NOT_READY,
                          $sformatf("uid %0d Store byte %0d PA is invalid", item.uid, i));
                return 0;
            end
        end
        if (!commit_memory_effect(item, RM_MEMORY_EFFECT_STORE,
                                  item.store_data, item.store_byte_mask)) return 1'b0;
        rec.compare_pass = 1'b1; rec.error_code = RM_LS_ERR_NONE;
        rec.detail = "normal Store cached"; history.push_back(rec); return 1;
    endfunction

    function bit commit_atomic(rm_ls_program_item_t item,
                               logic [23:0] commit_exception);
        rm_ls_history_record_t rec;
        rm_atomic_actual_t actual;
        bit store_access;
        bit raw_exception_match;
        bit cause_exception_match;
        bit exception_ok;
        bit [127:0] old_value;
        bit [127:0] expected_wb;
        bit [127:0] actual_wb;
        bit [127:0] new_value;
        bit [127:0] width_mask;
        bit [15:0] required_mask;
        bit block_match;
        bit sc_success_allowed;
        bit sc_success;
        bit cas_success;
        bit normal_path;
        int low_index;
        int high_index;

        rec = new();
        rec.uid = item.uid; rec.kind = item.kind; rec.rob = item.rob;
        rec.vaddr = item.computed_vaddr;
        if (item.pa_valid_mask[0]) rec.first_pa = item.pa_by_byte[0];
        rec.expected_exception = item.expected_exception;
        rec.actual_exception = commit_exception;
        store_access = item.atomic_kind != RM_ATOMIC_LR;
        exception_ok = compare_arch_exception(item.expected_exception,
                                              commit_exception,
                                              store_access,
                                              raw_exception_match,
                                              cause_exception_match);
        if (!pending_atomic_actual.exists(item.uid)) begin
            set_error(RM_LS_ERR_ATOMIC_WB_NOT_READY,
                      $sformatf("uid %0d atomic writeback actual missing", item.uid));
            return 1'b0;
        end
        actual = pending_atomic_actual[item.uid];
        if (actual.rob != item.rob || actual.dynamic_epoch != item.dynamic_epoch) begin
            set_error(RM_LS_ERR_ATOMIC_WB_IDENTITY_MISMATCH,
                      $sformatf("uid %0d atomic actual identity mismatch", item.uid));
            if (item.atomic_kind == RM_ATOMIC_SC) reservation_state = RM_RESV_NONE;
            return 1'b0;
        end
        if (actual.wb_count != item.expected_atomic_wb_count) begin
            set_error(RM_LS_ERR_ATOMIC_WB_COUNT_MISMATCH,
                      $sformatf("uid %0d atomic WB expected=%0d actual=%0d",
                                item.uid, item.expected_atomic_wb_count,
                                actual.wb_count));
            if (item.atomic_kind == RM_ATOMIC_SC) reservation_state = RM_RESV_NONE;
            return 1'b0;
        end
        for (int unsigned wb_index = 0; wb_index < actual.wb_count; wb_index++) begin
            bit wb_raw_match;
            bit wb_cause_match;
            if (!compare_arch_exception(item.expected_exception,
                                        actual.exception_vec[wb_index],
                                        store_access,
                                        wb_raw_match,
                                        wb_cause_match)) exception_ok = 1'b0;
        end
        if (!exception_ok) begin
            rec.error_code = RM_LS_ERR_EXCEPTION_MISMATCH;
            rec.detail = $sformatf("uid %0d atomic exception expected=0x%0h status=0x%0h wb0=0x%0h",
                                   item.uid, item.expected_exception,
                                   commit_exception, actual.exception_vec[0]);
            set_error(rec.error_code, rec.detail);
            if (item.atomic_kind == RM_ATOMIC_SC) reservation_state = RM_RESV_NONE;
            history.push_back(rec);
            return 1'b0;
        end

        normal_path = item.expected_exception == '0;
        if (!normal_path) begin
            if (item.atomic_kind == RM_ATOMIC_SC &&
                (item.expected_dcache_denied || item.expected_dcache_corrupt)) begin
                reservation_state = RM_RESV_NONE;
            end
            rec.compare_pass = 1'b1; rec.error_code = RM_LS_ERR_NONE;
            rec.detail = "fault atomic exception match; no memory effect";
            history.push_back(rec);
            return 1'b1;
        end
        if (!item.translation_valid || item.size_bytes == 0 || item.size_bytes > 16) begin
            set_error(RM_LS_ERR_TRANSLATION_NOT_READY,
                      $sformatf("uid %0d atomic translation unavailable", item.uid));
            return 1'b0;
        end
        required_mask = 16'hffff >> (16 - item.size_bytes);
        width_mask = {128{1'b1}} >> ((16 - item.size_bytes) * 8);
        old_value = '0;
        actual_wb = '0;
        expected_wb = '0;
        low_index = -1;
        high_index = -1;

        if (item.expected_atomic_wb_count == 2) begin
            for (int unsigned wb_index = 0; wb_index < actual.wb_count; wb_index++) begin
                if (actual.pdest_valid[wb_index] && item.atomic_expected_pdest_valid[0] &&
                    actual.pdest[wb_index] == item.atomic_expected_pdest[0]) low_index = wb_index;
                if (actual.pdest_valid[wb_index] && item.atomic_expected_pdest_valid[1] &&
                    actual.pdest[wb_index] == item.atomic_expected_pdest[1]) high_index = wb_index;
            end
            if (low_index < 0 || high_index < 0 || low_index == high_index) begin
                set_error(RM_LS_ERR_CASQ_WB_PAIR_MISMATCH,
                          $sformatf("uid %0d CAS.Q WB pdest pair missing low=%0d high=%0d",
                                    item.uid, low_index, high_index));
                return 1'b0;
            end
            if (!actual.data_valid[low_index] || !actual.data_valid[high_index] ||
                !actual.rf_wen_valid[low_index] || !actual.rf_wen[low_index] ||
                !actual.rf_wen_valid[high_index] || !actual.rf_wen[high_index]) begin
                set_error(RM_LS_ERR_ATOMIC_WB_IDENTITY_MISMATCH,
                          $sformatf("uid %0d CAS.Q WB data/rfWen invalid", item.uid));
                return 1'b0;
            end
            actual_wb = {actual.data[high_index], actual.data[low_index]};
        end else begin
            if (!actual.data_valid[0] ||
                (item.atomic_expected_pdest_valid[0] &&
                 (!actual.pdest_valid[0] ||
                  actual.pdest[0] != item.atomic_expected_pdest[0])) ||
                !actual.rf_wen_valid[0] || !actual.rf_wen[0]) begin
                set_error(RM_LS_ERR_ATOMIC_WB_IDENTITY_MISMATCH,
                          $sformatf("uid %0d atomic WB data/pdest/rfWen invalid", item.uid));
                if (item.atomic_kind == RM_ATOMIC_SC) reservation_state = RM_RESV_NONE;
                return 1'b0;
            end
            actual_wb[63:0] = actual.data[0];
        end

        // SC returns only success/failure and never returns the old memory
        // value.  A failed standalone SC may not access DCache at all, so it
        // legitimately has no initialized backing line.  LR/AMO/CAS still
        // require the old value for their architectural writeback/compare.
        if (item.atomic_kind != RM_ATOMIC_SC) begin
            if (!build_raw_memory_value(item, old_value)) return 1'b0;
            item.expected_old_value = old_value;
        end

        case (item.atomic_kind)
            RM_ATOMIC_LR: begin
                expected_wb[63:0] = format_atomic_wb(old_value, item.size_bytes);
                reservation_state = RM_RESV_POSSIBLE;
                reservation_block_addr = item.pa_by_byte[0] & ~64'h3f;
                reservation_size_bytes = item.size_bytes;
                reservation_lr_uid = item.uid;
                reservation_lr_rob = item.rob;
                reservation_create_sample = actual.sample_seq[0];
                rec.compare_pass = actual_wb[63:0] === expected_wb[63:0];
            end
            RM_ATOMIC_SC: begin
                block_match = reservation_state == RM_RESV_POSSIBLE &&
                              reservation_block_addr ==
                                  (item.pa_by_byte[0] & ~64'h3f);
                sc_success_allowed = block_match;
                item.sc_success_allowed = sc_success_allowed;
                sc_success = actual_wb[63:0] === 64'd0;
                if (!(actual_wb[63:0] inside {64'd0, 64'd1})) begin
                    rec.compare_pass = 1'b0;
                    rec.error_code = RM_LS_ERR_SC_RESULT_MISMATCH;
                    rec.detail = $sformatf("uid %0d SC returned illegal value 0x%0h",
                                           item.uid, actual_wb[63:0]);
                end else if (sc_success && !sc_success_allowed) begin
                    rec.compare_pass = 1'b0;
                    rec.error_code = RM_LS_ERR_SC_SUCCESS_WITHOUT_RESERVATION;
                    rec.detail = $sformatf("uid %0d SC succeeded without matching possible reservation",
                                           item.uid);
                end else if (sc_success) begin
                    new_value = '0;
                    new_value[63:0] = item.atomic_std_data[0];
                    rec.compare_pass = commit_memory_effect(
                        item, RM_MEMORY_EFFECT_SC_SUCCESS, new_value, required_mask);
                    item.expected_memory_write = rec.compare_pass;
                    item.expected_memory_value = new_value;
                end else begin
                    create_sc_no_write_obligation(item, old_value);
                    rec.compare_pass = 1'b1;
                end
                reservation_state = RM_RESV_NONE;
            end
            RM_ATOMIC_CAS: begin
                expected_wb = item.size_bytes == 16 ? old_value :
                              {{64{1'b0}}, format_atomic_wb(old_value,
                                                           item.size_bytes)};
                cas_success = (old_value & width_mask) ==
                              (item.atomic_compare_value & width_mask);
                item.expected_cas_success = cas_success;
                rec.compare_pass = actual_wb === expected_wb;
                if (rec.compare_pass && cas_success) begin
                    rec.compare_pass = commit_memory_effect(
                        item, RM_MEMORY_EFFECT_CAS_SUCCESS,
                        item.atomic_new_value, required_mask);
                    item.expected_memory_write = rec.compare_pass;
                    item.expected_memory_value = item.atomic_new_value;
                end
            end
            RM_ATOMIC_AMO: begin
                expected_wb[63:0] = format_atomic_wb(old_value, item.size_bytes);
                new_value = compute_amo_new_value(item.op, item.size_bytes,
                                                  old_value,
                                                  item.atomic_operand);
                rec.compare_pass = actual_wb[63:0] === expected_wb[63:0];
                if (rec.compare_pass) begin
                    rec.compare_pass = commit_memory_effect(
                        item, RM_MEMORY_EFFECT_AMO, new_value, required_mask);
                    item.expected_memory_write = rec.compare_pass;
                    item.expected_memory_value = new_value;
                end
            end
            default: begin
                rec.compare_pass = 1'b0;
                rec.error_code = RM_LS_ERR_ATOMIC_SIZE_UNSUPPORTED;
                rec.detail = $sformatf("uid %0d unsupported atomic subtype", item.uid);
            end
        endcase
        if (!rec.compare_pass) begin
            if (rec.error_code == RM_LS_ERR_NONE) begin
                rec.error_code = item.atomic_kind == RM_ATOMIC_CAS ?
                                 RM_LS_ERR_CAS_RESULT_MISMATCH :
                                 RM_LS_ERR_ATOMIC_RESULT_MISMATCH;
                rec.detail = $sformatf("uid %0d atomic result mismatch expected=0x%0h actual=0x%0h",
                                       item.uid, expected_wb, actual_wb);
            end
            set_error(rec.error_code, rec.detail);
        end else begin
            rec.error_code = RM_LS_ERR_NONE;
            rec.detail = "atomic result and memory effect match";
        end
        rec.expected_data = expected_wb[63:0];
        rec.actual_data = actual_wb[63:0];
        history.push_back(rec);
        return rec.compare_pass;
    endfunction

    function bit commit(rm_ls_rob_key_t commit_rob,
                        bit [23:0] commit_exception = '0);
        rm_ls_program_item_t item;
        bit semantic_ok;
        bit wait_for_fact;

        if (!configured || current_uid >= main_count || !items.exists(current_uid)) begin
            set_error(RM_LS_ERR_MAIN_NOT_READY, "commit item unavailable"); return 0;
        end
        item = items[current_uid];
        if (commit_rob != item.rob) begin
            set_error(RM_LS_ERR_DUT_ROB_MISMATCH,
                      $sformatf("uid %0d expected ROB %0d/%0d got %0d/%0d",
                                current_uid, item.rob.flag, item.rob.value,
                                commit_rob.flag, commit_rob.value));
            return 0;
        end
        case (item.kind)
            RM_LS_KIND_LOAD:   semantic_ok = compare_load(item, commit_exception);
            RM_LS_KIND_STORE:  semantic_ok = commit_store(item, commit_exception);
            RM_LS_KIND_ATOMIC: semantic_ok = commit_atomic(item, commit_exception);
            default: begin
                semantic_ok = commit_exception == item.expected_exception;
                if (!semantic_ok)
                    set_error(RM_LS_ERR_EXCEPTION_MISMATCH,
                              $sformatf("uid %0d non-LS exception mismatch", item.uid));
            end
        endcase
        wait_for_fact = !semantic_ok &&
                        (last_error == RM_LS_ERR_LOAD_ACTUAL_NOT_READY ||
                         last_error == RM_LS_ERR_ATOMIC_WB_NOT_READY ||
                         last_error == RM_LS_ERR_TRANSLATION_NOT_READY ||
                         last_error == RM_LS_ERR_ORIGINAL_L2_NOT_READY);
        if (wait_for_fact) return 0;
        current_uid++; consumed_commit_count++;
        return semantic_ok;
    endfunction

    function bit consume_if_rob_commit(bit rob_commit,
                                       rm_ls_rob_key_t commit_rob,
                                       bit [23:0] commit_exception = '0);
        if (!rob_commit) return 1'b1;
        return commit(commit_rob, commit_exception);
    endfunction

    function bit compare_overlay_store_entry(
        input rm_ls_store_cache_entry_t entry,
        input bit [15:0] actual_valid_mask,
        input bit [15:0] actual_corrupt_mask,
        input bit [127:0] actual_data,
        output bit [127:0] expected_data
    );
        bit [15:0] expected_valid_mask;
        bit [15:0] required_mask;
        bit [127:0] data_mask;

        expected_data = '0;
        if (!build_store_entry_expected(entry, expected_data, expected_valid_mask)) return 0;
        required_mask = 16'hffff >> (16 - entry.size_bytes);
        data_mask = {128{1'b1}} >> ((16 - entry.size_bytes) * 8);
        if ((actual_corrupt_mask & required_mask) != '0) begin
            set_error(RM_LS_ERR_OVERLAY_BYTE_CORRUPT,
                      $sformatf("Store uid %0d ROB %0d/%0d size=%0d PA=0x%0h corrupt_mask=0x%0h",
                                entry.uid, entry.rob.flag, entry.rob.value,
                                entry.size_bytes, entry.pa_by_byte[0],
                                actual_corrupt_mask & required_mask));
            return 0;
        end
        if ((actual_valid_mask & required_mask) != required_mask) begin
            set_error(RM_LS_ERR_OVERLAY_BYTE_MISSING,
                      $sformatf("Store uid %0d ROB %0d/%0d size=%0d PA=0x%0h expected_valid=0x%0h actual_valid=0x%0h",
                                entry.uid, entry.rob.flag, entry.rob.value,
                                entry.size_bytes, entry.pa_by_byte[0],
                                required_mask, actual_valid_mask & required_mask));
            return 0;
        end
        if ((actual_data & data_mask) != (expected_data & data_mask)) begin
            set_error(RM_LS_ERR_OVERLAY_MISMATCH,
                      $sformatf("Store uid %0d ROB %0d/%0d size=%0d PA=0x%0h expected=0x%0h actual=0x%0h",
                                entry.uid, entry.rob.flag, entry.rob.value,
                                entry.size_bytes, entry.pa_by_byte[0],
                                expected_data & data_mask, actual_data & data_mask));
            return 0;
        end
        return 1;
    endfunction

    function int unsigned sc_no_write_obligation_count();
        return sc_no_write_obligations.size();
    endfunction

    function bit read_sc_no_write_obligation(
        input int unsigned index,
        output rm_sc_no_write_obligation_t obligation
    );
        obligation = null;
        if (index >= sc_no_write_obligations.size() ||
            sc_no_write_obligations[index] == null ||
            !sc_no_write_obligations[index].valid) return 1'b0;
        obligation = sc_no_write_obligations[index];
        return 1'b1;
    endfunction
endclass

`endif
