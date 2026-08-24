`ifndef RM_LS_CORE_SV
`define RM_LS_CORE_SV

// 纯值型、无 UVM 依赖的标量 Load/Store 程序序核心。
// 该核心只拥有 expected 状态；框架 owner、DUT request 和 overlay 均不写入这里。
typedef enum int unsigned {
    RM_LS_KIND_OTHER = 0,
    RM_LS_KIND_LOAD,
    RM_LS_KIND_STORE
} rm_ls_kind_e;

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
    RM_LS_ERR_LOAD_ACTUAL_NOT_READY,
    RM_LS_ERR_STORE_MASK_MISMATCH,
    RM_LS_ERR_EXCEPTION_MISMATCH,
    RM_LS_ERR_LOAD_MISMATCH,
    RM_LS_ERR_OVERLAY_NOT_READY,
    RM_LS_ERR_OVERLAY_BYTE_MISSING,
    RM_LS_ERR_OVERLAY_BYTE_CORRUPT,
    RM_LS_ERR_OVERLAY_MISMATCH
} rm_ls_error_e;

function automatic bit rm_ls_decode_translation_path(
    input bit [1:0] s2xlate,
    input bit       is_hypervisor_inst,
    input bit       priv_virt,
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
    longint unsigned pa_by_byte[8];
    bit [7:0]       pa_valid_mask;
    bit [85:0]      translation_key;
    int unsigned    size_bytes;
    bit             is_signed;
    rm_ls_rob_key_t rob;
    bit             translation_valid;
    bit [23:0]      expected_exception;
    bit [63:0]      store_data;
    bit [7:0]       store_byte_mask;

    function new(int unsigned uid_i = 0);
        valid = 0; uid = uid_i; kind = RM_LS_KIND_OTHER; op = '0;
        computed_vaddr = '0; main_vaddr = '0; pa_valid_mask = '0;
        translation_key = '0; size_bytes = 0; is_signed = 0;
        rob = '{default:'0}; translation_valid = 0; expected_exception = '0;
        store_data = '0; store_byte_mask = '0;
        foreach (pa_by_byte[i]) pa_by_byte[i] = '0;
    endfunction

    function bit set_bare_identity_geometry(output int unsigned bad_byte_index);
        bit [63:0] byte_va;

        bad_byte_index = 0;
        pa_valid_mask = '0;
        if (size_bytes == 0 || size_bytes > 8) return 1'b0;
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
    bit [63:0]       vaddr;
    longint unsigned pa_by_byte[8];
    int unsigned     size_bytes;
    bit [7:0]        byte_mask;
    bit [63:0]       committed_data;

    function new();
        valid = 1'b0;
        uid = 0;
        rob = '{default:'0};
        vaddr = '0;
        size_bytes = 0;
        byte_mask = '0;
        committed_data = '0;
        foreach (pa_by_byte[i]) pa_by_byte[i] = '0;
    endfunction

    function void capture_store(rm_ls_program_item_t item);
        valid = 1'b0;
        if (item == null) return;
        if (!item.valid || item.kind != RM_LS_KIND_STORE) return;
        valid = 1'b1;
        uid = item.uid;
        rob = item.rob;
        vaddr = item.computed_vaddr;
        size_bytes = item.size_bytes;
        byte_mask = item.store_byte_mask;
        committed_data = item.store_data;
        foreach (pa_by_byte[i]) pa_by_byte[i] = item.pa_by_byte[i];
    endfunction

    function void copy_from(rm_ls_store_cache_entry_t source);
        if (source == null) return;
        valid = source.valid;
        uid = source.uid;
        rob = source.rob;
        vaddr = source.vaddr;
        size_bytes = source.size_bytes;
        byte_mask = source.byte_mask;
        committed_data = source.committed_data;
        foreach (pa_by_byte[i]) pa_by_byte[i] = source.pa_by_byte[i];
    endfunction
endclass

class rm_ls_load_actual_t;
    bit             valid;
    int unsigned    uid;
    rm_ls_rob_key_t rob;
    bit             data_valid;
    bit [63:0]      data;
    bit [23:0]      exception_vec;
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
    bit [63:0]      actual_data;
    bit [23:0]      expected_exception;
    bit [23:0]      actual_exception;
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
    bit                 backing_valid[longint unsigned];
    byte unsigned       backing_byte[longint unsigned];
    bit                 expected_store_valid[longint unsigned];
    byte unsigned       expected_store_byte[longint unsigned];
    rm_ls_store_cache_entry_t expected_store_entries[$];
    rm_ls_history_record_t history[$];

    function new(); reset(); endfunction

    function void reset();
        configured = 0; main_count = 0; current_uid = 0;
        consumed_commit_count = 0; error_count = 0;
        last_error = RM_LS_ERR_NONE; last_error_text = "";
        items.delete(); pending_load_actual.delete();
        backing_valid.delete(); backing_byte.delete();
        expected_store_valid.delete(); expected_store_byte.delete();
        expected_store_entries.delete(); history.delete();
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
                                            output bit [63:0] expected_data,
                                            output bit [7:0] expected_valid_mask);
        expected_data = '0;
        expected_valid_mask = '0;
        if (!configured || entry == null) begin
            set_error(RM_LS_ERR_OVERLAY_NOT_READY, "invalid Store cache entry");
            return 0;
        end
        if (!entry.valid || entry.size_bytes == 0 || entry.size_bytes > 8) begin
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
        if (item.is_signed) begin
            case (item.size_bytes)
                1: expected = {{56{expected[7]}}, expected[7:0]};
                2: expected = {{48{expected[15]}}, expected[15:0]};
                4: expected = {{32{expected[31]}}, expected[31:0]};
                default: ;
            endcase
        end
        return 1;
    endfunction

    function bit compare_load(rm_ls_program_item_t item,
                              bit [23:0] terminal_exception);
        rm_ls_history_record_t rec;
        bit [63:0] expected;
        bit [23:0] expected_exception;
        rm_ls_load_actual_t actual;

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
        if (!build_load_expected(item, expected, expected_exception)) return 0;
        rec.expected_data = expected; rec.actual_data = actual.data;
        rec.expected_exception = expected_exception; rec.actual_exception = actual.exception_vec;
        if (terminal_exception != actual.exception_vec ||
            actual.exception_vec != expected_exception) begin
            rec.error_code = RM_LS_ERR_EXCEPTION_MISMATCH;
            rec.detail = $sformatf("uid %0d Load exception expected=0x%0h status=0x%0h actual=0x%0h",
                                   item.uid, expected_exception, terminal_exception,
                                   actual.exception_vec);
            set_error(rec.error_code, rec.detail);
            history.push_back(rec);
            return 0;
        end
        if (expected_exception != '0)
            rec.compare_pass = !actual.data_valid;
        else
            rec.compare_pass = actual.data_valid && actual.data == expected;
        rec.error_code = rec.compare_pass ? RM_LS_ERR_NONE : RM_LS_ERR_LOAD_MISMATCH;
        rec.detail = rec.compare_pass ? "Load match" : "Load data/data_valid mismatch";
        if (!rec.compare_pass) set_error(rec.error_code, rec.detail);
        history.push_back(rec); return rec.compare_pass;
    endfunction

    function bit commit_store(rm_ls_program_item_t item,
                              bit [23:0] commit_exception);
        rm_ls_history_record_t rec;
        rm_ls_store_cache_entry_t store_entry;
        bit [7:0] required_mask;

        rec = new(); rec.uid = item.uid; rec.kind = item.kind; rec.rob = item.rob;
        rec.vaddr = item.computed_vaddr;
        if (item.pa_valid_mask[0]) rec.first_pa = item.pa_by_byte[0];
        rec.expected_exception = item.expected_exception;
        rec.actual_exception = commit_exception;
        if (commit_exception != item.expected_exception) begin
            rec.error_code = RM_LS_ERR_EXCEPTION_MISMATCH;
            rec.detail = $sformatf("uid %0d Store exception expected=0x%0h status=0x%0h",
                                   item.uid, item.expected_exception, commit_exception);
            set_error(rec.error_code, rec.detail); history.push_back(rec); return 0;
        end
        if (item.expected_exception != '0) begin
            rec.compare_pass = 1'b1; rec.error_code = RM_LS_ERR_NONE;
            rec.detail = "fault Store exception match; no Store effect";
            history.push_back(rec); return 1;
        end
        if (!item.translation_valid || item.size_bytes == 0 || item.size_bytes > 8) begin
            set_error(RM_LS_ERR_TRANSLATION_NOT_READY,
                      $sformatf("uid %0d Store translation", item.uid));
            return 0;
        end
        required_mask = 8'hff >> (8 - item.size_bytes);
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
        // 所有证据先检查完成，再原子提交到 RM 自己的 Store byte 表。
        for (int unsigned i = 0; i < item.size_bytes; i++) begin
            expected_store_valid[item.pa_by_byte[i]] = 1'b1;
            expected_store_byte[item.pa_by_byte[i]] = item.store_data[(i * 8) +: 8];
        end
        store_entry = new();
        store_entry.capture_store(item);
        expected_store_entries.push_back(store_entry);
        rec.compare_pass = 1'b1; rec.error_code = RM_LS_ERR_NONE;
        rec.detail = "normal Store cached"; history.push_back(rec); return 1;
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
            default: begin
                semantic_ok = commit_exception == item.expected_exception;
                if (!semantic_ok)
                    set_error(RM_LS_ERR_EXCEPTION_MISMATCH,
                              $sformatf("uid %0d non-LS exception mismatch", item.uid));
            end
        endcase
        wait_for_fact = !semantic_ok &&
                        (last_error == RM_LS_ERR_LOAD_ACTUAL_NOT_READY ||
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
        input bit [7:0] actual_valid_mask,
        input bit [7:0] actual_corrupt_mask,
        input bit [63:0] actual_data,
        output bit [63:0] expected_data
    );
        bit [7:0] expected_valid_mask;
        bit [7:0] required_mask;
        bit [63:0] data_mask;

        expected_data = '0;
        if (!build_store_entry_expected(entry, expected_data, expected_valid_mask)) return 0;
        required_mask = 8'hff >> (8 - entry.size_bytes);
        data_mask = 64'hffff_ffff_ffff_ffff >> ((8 - entry.size_bytes) * 8);
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
endclass

`endif
