// Read-only observer contract for the scalar Load/Store RM.
// Producers may publish copied facts into this cache; the RM only calls read_*.
`ifndef MEMBLOCK_RM_OBSERVER_API__SV
`define MEMBLOCK_RM_OBSERVER_API__SV

class memblock_rm_observer_cache extends uvm_object;
    // Producer-defined 16-bit owner code; zero means unspecified/not ready.
    localparam bit [15:0] SOURCE_OWNER_UNSPECIFIED = 16'h0000;

    typedef struct packed {
        bit                  valid;
        memblock_uid_t       uid;
        bit                  rob_valid;
        bit                  rob_flag;
        bit [MEMBLOCK_ROB_VALUE_W-1:0] rob_value;
        bit                  lq_valid;
        bit                  lq_flag;
        bit [MEMBLOCK_LQ_VALUE_W-1:0] lq_value;
        bit                  data_valid;
        bit [63:0]           data;
        bit [23:0]           exception_vec;
        longint unsigned     cycle;
        bit [7:0]            source_kind;
        bit [15:0]           source_owner;
    } load_actual_view_t;

    typedef struct packed {
        bit                  valid;
        memblock_uid_t       uid;
        bit                  rob_valid;
        bit                  rob_flag;
        bit [MEMBLOCK_ROB_VALUE_W-1:0] rob_value;
        bit                  sq_valid;
        bit                  sq_flag;
        bit [MEMBLOCK_SQ_VALUE_W-1:0] sq_value;
        bit [63:0]           data;
        bit [7:0]            byte_mask;
        longint unsigned     sta_cycle;
        longint unsigned     std_cycle;
        bit [7:0]            source_kind;
        bit [15:0]           source_owner;
    } store_input_view_t;

    typedef struct packed {
        bit                  valid;
        memblock_uid_t       uid;
        bit [63:0]           vaddr;
        bit [63:0]           first_pa;
        int unsigned         size_bytes;
        bit                  translation_valid;
        bit                  payload_valid;
        bit [23:0]           exception_vec;
        longint unsigned     cycle;
        bit [7:0]            source_kind;
        bit [15:0]           source_owner;
    } translation_snapshot_view_t;

    typedef struct packed {
        bit                  valid;
        memblock_uid_t       uid;
        bit                  rob_valid;
        bit                  rob_flag;
        bit [MEMBLOCK_ROB_VALUE_W-1:0] rob_value;
        bit                  fault;
        bit                  retire;
        longint unsigned     cycle;
        bit [7:0]            source_kind;
        bit [15:0]           source_owner;
    } commit_rob_view_t;

    typedef struct packed {
        bit                  valid;
        longint unsigned     pa;
        byte unsigned        value;
        bit                  byte_valid;
        bit                  corrupt;
        longint unsigned     observed_cycle;
        bit [7:0]            source_kind;
        bit [15:0]           source_owner;
    } store_final_entry_view_t;

    typedef struct packed {
        bit                  valid;
        bit                  load_actual;
        bit                  store_input;
        bit                  translation_snapshot;
        bit                  commit_rob;
        bit                  store_final_table;
        bit                  store_final_table_complete;
        longint unsigned     publication_epoch;
    } capability_view_t;

    load_actual_view_t          load_actual_by_uid[memblock_uid_t];
    store_input_view_t          store_input_by_uid[memblock_uid_t];
    translation_snapshot_view_t translation_by_uid[memblock_uid_t];
    commit_rob_view_t           commit_history[$];
    store_final_entry_view_t    final_store_by_pa[longint unsigned];
    longint unsigned            final_store_order[$];
    capability_view_t           capability;
    longint unsigned            next_publication_epoch;

    `uvm_object_utils(memblock_rm_observer_cache)

    function new(string name = "memblock_rm_observer_cache");
        super.new(name);
        // An empty observer is a valid bound object with zero capabilities.
        // Individual fact reads still fail closed when their entry is absent.
        capability = '{default:'0};
        capability.valid = 1'b1;
        next_publication_epoch = 0;
    endfunction

    // These publish methods are for a future passive monitor/adapter only.
    // They copy values into RM-owned storage and never mutate framework owners.
    function void publish_load_actual(input load_actual_view_t value);
        if (!value.valid) return;
        load_actual_by_uid[value.uid] = value;
        capability.load_actual = 1'b1;
        bump_epoch();
    endfunction

    function void publish_store_input(input store_input_view_t value);
        if (!value.valid) return;
        store_input_by_uid[value.uid] = value;
        capability.store_input = 1'b1;
        bump_epoch();
    endfunction

    function void publish_translation(input translation_snapshot_view_t value);
        if (!value.valid) return;
        translation_by_uid[value.uid] = value;
        capability.translation_snapshot = 1'b1;
        bump_epoch();
    endfunction

    function void publish_commit(input commit_rob_view_t value);
        if (!value.valid) return;
        commit_history.push_back(value);
        capability.commit_rob = 1'b1;
        bump_epoch();
    endfunction

    function void publish_store_final(input store_final_entry_view_t value);
        if (!value.valid) return;
        if (!final_store_by_pa.exists(value.pa)) begin
            final_store_order.push_back(value.pa);
        end
        final_store_by_pa[value.pa] = value;
        capability.store_final_table = 1'b1;
        bump_epoch();
    endfunction

    // Call only after the passive producer has frozen the whole final snapshot.
    // An empty final-byte table is valid for a program with no committed stores.
    function void mark_store_final_table_complete();
        capability.store_final_table = 1'b1;
        capability.store_final_table_complete = 1'b1;
        bump_epoch();
    endfunction

    function void set_capability(input capability_view_t value);
        capability = value;
        capability.valid = 1'b1;
        bump_epoch();
    endfunction

    function bit read_capability(output capability_view_t value);
        value = capability;
        return capability.valid;
    endfunction

    function bit read_load_actual(input memblock_uid_t uid,
                                  output load_actual_view_t value);
        value = '{default:'0};
        if (!load_actual_by_uid.exists(uid)) return 1'b0;
        value = load_actual_by_uid[uid];
        return value.valid;
    endfunction

    function bit read_store_input(input memblock_uid_t uid,
                                  output store_input_view_t value);
        value = '{default:'0};
        if (!store_input_by_uid.exists(uid)) return 1'b0;
        value = store_input_by_uid[uid];
        return value.valid;
    endfunction

    function bit read_translation(input memblock_uid_t uid,
                                  output translation_snapshot_view_t value);
        value = '{default:'0};
        if (!translation_by_uid.exists(uid)) return 1'b0;
        value = translation_by_uid[uid];
        return value.valid;
    endfunction

    function bit read_commit(input int unsigned cursor,
                             output commit_rob_view_t value);
        value = '{default:'0};
        if (cursor >= commit_history.size()) return 1'b0;
        value = commit_history[cursor];
        return value.valid;
    endfunction

    function bit read_store_final(input longint unsigned pa,
                                  output store_final_entry_view_t value);
        value = '{default:'0};
        if (!final_store_by_pa.exists(pa)) return 1'b0;
        value = final_store_by_pa[pa];
        return value.valid;
    endfunction

    function int unsigned commit_count();
        return commit_history.size();
    endfunction

    function int unsigned final_store_count();
        return final_store_order.size();
    endfunction

    function bit read_store_final_by_index(input int unsigned index,
                                           output store_final_entry_view_t value);
        value = '{default:'0};
        if (index >= final_store_order.size()) return 1'b0;
        return read_store_final(final_store_order[index], value);
    endfunction

    function void clear();
        load_actual_by_uid.delete();
        store_input_by_uid.delete();
        translation_by_uid.delete();
        commit_history.delete();
        final_store_by_pa.delete();
        final_store_order.delete();
        capability = '{default:'0};
        capability.valid = 1'b1;
        next_publication_epoch = 0;
    endfunction

    local function void bump_epoch();
        next_publication_epoch++;
        capability.publication_epoch = next_publication_epoch;
        capability.valid = 1'b1;
    endfunction
endclass

`endif
