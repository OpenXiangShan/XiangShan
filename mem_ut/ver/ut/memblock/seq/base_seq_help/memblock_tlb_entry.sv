//=========================================================
//File name    : memblock_tlb_entry.sv
//Module name  : memblock_tlb_entry
//Discribution : V2 L2TLB live/pending/UID payload records
//=========================================================
`ifndef MEMBLOCK_TLB_ENTRY__SV
`define MEMBLOCK_TLB_ENTRY__SV

class memblock_tlb_entry extends uvm_object;

    typedef enum int unsigned {
        MEMBLOCK_TLB_PTE_MODE_LEGAL            = 0,
        MEMBLOCK_TLB_PTE_MODE_MIXED            = 1,
        MEMBLOCK_TLB_PTE_MODE_EXCEPTION_BIASED = 2
    } memblock_tlb_pte_mode_e;

    typedef enum int unsigned {
        MEMBLOCK_TLB_ACCESS_UNKNOWN = 0,
        MEMBLOCK_TLB_ACCESS_LOAD    = 1,
        MEMBLOCK_TLB_ACCESS_STORE   = 2
    } memblock_tlb_access_e;

    typedef enum int unsigned {
        MEMBLOCK_TLB_FAULT_STAGE_NONE = 0,
        MEMBLOCK_TLB_FAULT_STAGE_S1   = 1,
        MEMBLOCK_TLB_FAULT_STAGE_S2   = 2
    } memblock_tlb_fault_stage_e;

    memblock_tlb_lookup_key_t lookup_key;
    // 中文注释：仅 canonical live entry 保存自己已注册的 range index key，
    // 供统一 delete helper 精确反注册。pending/UID snapshot 复制 raw payload
    // 时必须清空该队列，不能把 live-index ownership 带到 request 私有对象。
    memblock_tlb_range_index_key_t range_index_keys[$];
    // The stored widths deliberately match the V2 PtwRespS2 wires.  The
    // lookup key remains wider because it is a framework lookup identity,
    // not a direct copy of either response tag.
    bit [34:0] s1_tag;
    bit [15:0] s1_asid;
    bit [15:0] s1_vmid;
    bit [37:0] s2_tag;
    bit [15:0] s2_vmid;
    bit [1:0]  s2xlate;

    // 中文注释：stage_active、mode/root 和 CSR sequence 只在 lookup miss 建表时冻结。
    // 命中 entry、pending snapshot 和 UID 回填只能复制，不能用当前 CSR 重建。
    bit        s1_stage_active;
    bit        s2_stage_active;
    bit [3:0]  s1_translation_mode_at_build;
    bit [3:0]  s2_translation_mode_at_build;
    // 中文注释：两个 PTE profile mode 在 lookup miss 时从 testcase 参数冻结。
    // pending/UID 只复制该值，后续 PTE.N/NAPOT 判定不得重新读取可变全局配置。
    bit [1:0]  s1_pte_mode_at_build;
    bit [1:0]  s2_pte_mode_at_build;
    bit [43:0] s1_root_ppn_at_build;
    bit [43:0] s2_root_ppn_at_build;
    int unsigned csr_context_seq_at_build;
    longint unsigned entry_generation;

    // 中文注释：四个 raw fault 每次 miss 都独立随机；effective fault 只保留按 s2xlate
    // 和权重优先级收敛后的唯一 DUT 可见 fault。pmaAF 是不参与该收敛的 legacy sideband。
    bit fault_raw_s1_pf;
    bit fault_raw_s1_af;
    bit fault_raw_s2_gpf;
    bit fault_raw_s2_gaf;
    bit fault_effective_s1_pf;
    bit fault_effective_s1_af;
    bit fault_effective_s2_gpf;
    bit fault_effective_s2_gaf;
    memblock_tlb_fault_stage_e fault_stage_selected;
    bit pmaAF;

    // 中文注释：S1 PPN 按 V2 sector 接口拆分；pteidx 是 one-hot Bool 数组，
    // 不能作为数值 index 或通过 != 0 投影到 response wire。
    bit [40:0] s1_entry_ppn_raw;
    bit [43:0] s1_resolved_ppn;
    bit        s1_resolved_ppn_valid;
    bit [1:0]  s1_level;
    bit        s1_pte_n;
    bit [1:0]  s1_entry_pbmt;
    bit        s1_pte_r;
    bit        s1_pte_w;
    bit        s1_pte_x;
    bit        s1_pte_u;
    bit        s1_pte_g;
    bit        s1_pte_a;
    bit        s1_pte_d;
    bit        s1_pte_v;
    bit [2:0]  s1_addr_low;
    bit [2:0]  s1_ppn_low[8];
    bit        s1_valididx[8];
    bit        s1_pteidx[8];

    // 中文注释：V2 S2 response 没有 entry_v；下列字段只保存和驱动实际接口存在的 payload。
    bit [37:0] s2_entry_ppn_raw;
    bit [43:0] s2_resolved_ppn;
    bit        s2_resolved_ppn_valid;
    bit [1:0]  s2_level;
    bit        s2_pte_n;
    bit [1:0]  s2_entry_pbmt;
    bit        s2_pte_r;
    bit        s2_pte_w;
    bit        s2_pte_x;
    bit        s2_pte_u;
    bit        s2_pte_g;
    bit        s2_pte_a;
    bit        s2_pte_d;

    longint unsigned create_cycle;
    longint unsigned last_hit_cycle;

    `uvm_object_utils(memblock_tlb_entry)

    function new(string name = "memblock_tlb_entry");
        super.new(name);
        reset();
    endfunction:new

    function void reset();
        lookup_key = '{default:'0};
        range_index_keys.delete();
        s1_tag = '0; s1_asid = '0; s1_vmid = '0; s2_tag = '0; s2_vmid = '0; s2xlate = '0;
        s1_stage_active = 1'b0; s2_stage_active = 1'b0;
        s1_translation_mode_at_build = '0; s2_translation_mode_at_build = '0;
        s1_pte_mode_at_build = '0; s2_pte_mode_at_build = '0;
        s1_root_ppn_at_build = '0; s2_root_ppn_at_build = '0;
        csr_context_seq_at_build = 0; entry_generation = 0;
        fault_raw_s1_pf = 0; fault_raw_s1_af = 0; fault_raw_s2_gpf = 0; fault_raw_s2_gaf = 0;
        fault_effective_s1_pf = 0; fault_effective_s1_af = 0;
        fault_effective_s2_gpf = 0; fault_effective_s2_gaf = 0;
        fault_stage_selected = MEMBLOCK_TLB_FAULT_STAGE_NONE; pmaAF = 0;
        s1_entry_ppn_raw = '0; s1_resolved_ppn = '0; s1_resolved_ppn_valid = 0;
        s1_level = '0; s1_pte_n = 0; s1_entry_pbmt = '0;
        s1_pte_r = 0; s1_pte_w = 0; s1_pte_x = 0; s1_pte_u = 0; s1_pte_g = 0;
        s1_pte_a = 0; s1_pte_d = 0; s1_pte_v = 0; s1_addr_low = '0;
        s2_entry_ppn_raw = '0; s2_resolved_ppn = '0; s2_resolved_ppn_valid = 0;
        s2_level = '0; s2_pte_n = 0; s2_entry_pbmt = '0;
        s2_pte_r = 0; s2_pte_w = 0; s2_pte_x = 0; s2_pte_u = 0; s2_pte_g = 0;
        s2_pte_a = 0; s2_pte_d = 0;
        create_cycle = 0; last_hit_cycle = 0;
        foreach (s1_ppn_low[idx]) begin
            s1_ppn_low[idx] = '0;
            s1_valididx[idx] = 1'b0;
            s1_pteidx[idx] = 1'b0;
        end
    endfunction:reset

    function bit has_effective_fault();
        return fault_effective_s1_pf || fault_effective_s1_af ||
               fault_effective_s2_gpf || fault_effective_s2_gaf;
    endfunction:has_effective_fault

    // 中文注释：在 build/copy/drive 边界验证冻结的 S1 sector payload，不修改 entry。
    function bit [43:0] get_s1_selected_canonical_ppn();
        return {s1_entry_ppn_raw, s1_ppn_low[s1_addr_low]};
    endfunction:get_s1_selected_canonical_ppn

    function bit s1_pteidx_is_onehot();
        int unsigned onehot_count;

        onehot_count = 0;
        foreach (s1_pteidx[idx]) begin
            onehot_count += s1_pteidx[idx];
        end
        return onehot_count == 1;
    endfunction:s1_pteidx_is_onehot

    // 中文注释：检查未参与翻译的 stage 是否保持 reset 默认 payload。
    // raw fault history 是独立 debug 数据，允许保留随机结果，不在本校验中检查。
    function void check_inactive_stage_defaults(input string phase);
        if (!s1_stage_active) begin
            if (s1_tag != '0 || s1_asid != '0 || s1_vmid != '0 ||
                s1_translation_mode_at_build != '0 || s1_root_ppn_at_build != '0 ||
                s1_pte_mode_at_build != '0 ||
                s1_entry_ppn_raw != '0 || s1_resolved_ppn != '0 ||
                s1_resolved_ppn_valid || s1_level != '0 || s1_pte_n ||
                s1_entry_pbmt != '0 || s1_pte_r || s1_pte_w || s1_pte_x ||
                s1_pte_u || s1_pte_g || s1_pte_a || s1_pte_d || s1_pte_v ||
                s1_addr_low != '0) begin
                `uvm_fatal("L2TLB_INACTIVE_STAGE_PAYLOAD",
                           $sformatf("phase=%s inactive S1 carries payload", phase))
            end
            foreach (s1_ppn_low[idx]) begin
                if (s1_ppn_low[idx] != '0 || s1_valididx[idx] || s1_pteidx[idx]) begin
                    `uvm_fatal("L2TLB_INACTIVE_STAGE_PAYLOAD",
                               $sformatf("phase=%s inactive S1 sector=%0d carries payload", phase, idx))
                end
            end
        end
        if (!s2_stage_active &&
            (s2_tag != '0 || s2_vmid != '0 || s2_translation_mode_at_build != '0 ||
             s2_pte_mode_at_build != '0 ||
             s2_root_ppn_at_build != '0 || s2_entry_ppn_raw != '0 ||
             s2_resolved_ppn != '0 || s2_resolved_ppn_valid || s2_level != '0 ||
             s2_pte_n || s2_entry_pbmt != '0 || s2_pte_r || s2_pte_w ||
             s2_pte_x || s2_pte_u || s2_pte_g || s2_pte_a || s2_pte_d)) begin
            `uvm_fatal("L2TLB_INACTIVE_STAGE_PAYLOAD",
                       $sformatf("phase=%s inactive S2 carries payload", phase))
        end
    endfunction:check_inactive_stage_defaults

    function void validate_s1_sector_payload_consistency(
        input string phase,
        input memblock_tlb_entry source = null,
        input bit [43:0] build_canonical_ppn = '0,
        input bit build_canonical_ppn_valid = 1'b0);
        int unsigned onehot_count;
        bit          superpage_or_napot;
        bit [43:0]   expected_selected_ppn;

        if (!s1_stage_active) begin
            return;
        end
        superpage_or_napot = s1_level != 0 || s1_pte_n;
        onehot_count = 0;
        foreach (s1_pteidx[idx]) begin
            onehot_count += s1_pteidx[idx];
        end
        if (onehot_count != 1 || !s1_pteidx[s1_addr_low] ||
            !s1_valididx[s1_addr_low]) begin
            `uvm_fatal("L2TLB_S1_SECTOR_PAYLOAD",
                       $sformatf("phase=%s invalid sector onehot=%0d addr_low=%0d valid=%0d",
                                 phase, onehot_count, s1_addr_low,
                                 s1_valididx[s1_addr_low]))
        end
        foreach (s1_ppn_low[idx]) begin
            bit expected_valid;
            bit [2:0] expected_ppn_low;

            expected_valid = superpage_or_napot || idx == s1_addr_low;
            expected_ppn_low = s1_ppn_low[s1_addr_low];
            if (s1_pteidx[idx] != (idx == s1_addr_low)) begin
                `uvm_fatal("L2TLB_S1_SECTOR_PAYLOAD",
                           $sformatf("phase=%s pteidx is not one-hot at idx=%0d addr_low=%0d",
                                     phase, idx, s1_addr_low))
            end
            if (s1_valididx[idx] != expected_valid) begin
                `uvm_fatal("L2TLB_S1_SECTOR_PAYLOAD",
                           $sformatf("phase=%s valididx shape mismatch idx=%0d got=%0d expected=%0d level=%0d n=%0d",
                                     phase, idx, s1_valididx[idx], expected_valid,
                                     s1_level, s1_pte_n))
            end
            if (expected_valid && s1_ppn_low[idx] != expected_ppn_low) begin
                `uvm_fatal("L2TLB_S1_SECTOR_PAYLOAD",
                           $sformatf("phase=%s split PPN drift idx=%0d got=0x%0h expected=0x%0h level=%0d n=%0d",
                                     phase, idx, s1_ppn_low[idx], expected_ppn_low,
                                     s1_level, s1_pte_n))
            end
            if (!expected_valid && s1_ppn_low[idx] != '0) begin
                `uvm_fatal("L2TLB_S1_SECTOR_PAYLOAD",
                           $sformatf("phase=%s invalid sector carries ppn_low idx=%0d value=0x%0h",
                                     phase, idx, s1_ppn_low[idx]))
            end
        end
        if (build_canonical_ppn_valid) begin
            expected_selected_ppn = build_canonical_ppn;
            if (get_s1_selected_canonical_ppn() != expected_selected_ppn) begin
                `uvm_fatal("L2TLB_S1_SECTOR_PAYLOAD",
                           $sformatf("phase=%s selected split PPN mismatch encoded=0x%0h expected=0x%0h addr_low=%0d",
                                     phase,
                                     get_s1_selected_canonical_ppn(),
                                     expected_selected_ppn,
                                     s1_addr_low))
            end
        end
        if (source != null) begin
            if (s1_addr_low != source.s1_addr_low ||
                s1_entry_ppn_raw != source.s1_entry_ppn_raw) begin
                `uvm_fatal("L2TLB_S1_SECTOR_PAYLOAD",
                           $sformatf("phase=%s snapshot sector payload drift", phase))
            end
            foreach (s1_ppn_low[idx]) begin
                if (s1_ppn_low[idx] != source.s1_ppn_low[idx] ||
                    s1_valididx[idx] != source.s1_valididx[idx] ||
                    s1_pteidx[idx] != source.s1_pteidx[idx]) begin
                    `uvm_fatal("L2TLB_S1_SECTOR_PAYLOAD",
                               $sformatf("phase=%s snapshot sector payload drift at idx=%0d",
                                         phase, idx))
                end
            end
        end
    endfunction:validate_s1_sector_payload_consistency

    // 中文注释：pending/UID 只能经本函数取得 live entry 的逐字段冻结副本。
    // 复制完成立即检查 sector one-hot 和 split PPN，禁止在 driver 或 UID 回填时重随机。
    function void copy_from(input memblock_tlb_entry source);
        if (source == null) begin
            `uvm_fatal("TLB_ENTRY", "copy_from got null source")
        end
        source.check_inactive_stage_defaults("COPY_SOURCE");
        source.validate_s1_sector_payload_consistency("COPY_SOURCE");
        // range_index_keys 是 canonical table 的删除所有权，snapshot 只能复制
        // response payload/provenance，不能取得 index ownership。
        range_index_keys.delete();
        lookup_key = source.lookup_key; s1_tag = source.s1_tag; s1_asid = source.s1_asid;
        s1_vmid = source.s1_vmid; s2_tag = source.s2_tag; s2_vmid = source.s2_vmid;
        s2xlate = source.s2xlate; s1_stage_active = source.s1_stage_active;
        s2_stage_active = source.s2_stage_active;
        s1_translation_mode_at_build = source.s1_translation_mode_at_build;
        s2_translation_mode_at_build = source.s2_translation_mode_at_build;
        s1_pte_mode_at_build = source.s1_pte_mode_at_build;
        s2_pte_mode_at_build = source.s2_pte_mode_at_build;
        s1_root_ppn_at_build = source.s1_root_ppn_at_build;
        s2_root_ppn_at_build = source.s2_root_ppn_at_build;
        csr_context_seq_at_build = source.csr_context_seq_at_build;
        entry_generation = source.entry_generation;
        fault_raw_s1_pf = source.fault_raw_s1_pf; fault_raw_s1_af = source.fault_raw_s1_af;
        fault_raw_s2_gpf = source.fault_raw_s2_gpf; fault_raw_s2_gaf = source.fault_raw_s2_gaf;
        fault_effective_s1_pf = source.fault_effective_s1_pf;
        fault_effective_s1_af = source.fault_effective_s1_af;
        fault_effective_s2_gpf = source.fault_effective_s2_gpf;
        fault_effective_s2_gaf = source.fault_effective_s2_gaf;
        fault_stage_selected = source.fault_stage_selected; pmaAF = source.pmaAF;
        s1_entry_ppn_raw = source.s1_entry_ppn_raw; s1_resolved_ppn = source.s1_resolved_ppn;
        s1_resolved_ppn_valid = source.s1_resolved_ppn_valid; s1_level = source.s1_level;
        s1_pte_n = source.s1_pte_n; s1_entry_pbmt = source.s1_entry_pbmt;
        s1_pte_r = source.s1_pte_r; s1_pte_w = source.s1_pte_w; s1_pte_x = source.s1_pte_x;
        s1_pte_u = source.s1_pte_u; s1_pte_g = source.s1_pte_g; s1_pte_a = source.s1_pte_a;
        s1_pte_d = source.s1_pte_d; s1_pte_v = source.s1_pte_v; s1_addr_low = source.s1_addr_low;
        s2_entry_ppn_raw = source.s2_entry_ppn_raw; s2_resolved_ppn = source.s2_resolved_ppn;
        s2_resolved_ppn_valid = source.s2_resolved_ppn_valid; s2_level = source.s2_level;
        s2_pte_n = source.s2_pte_n; s2_entry_pbmt = source.s2_entry_pbmt;
        s2_pte_r = source.s2_pte_r; s2_pte_w = source.s2_pte_w; s2_pte_x = source.s2_pte_x;
        s2_pte_u = source.s2_pte_u; s2_pte_g = source.s2_pte_g; s2_pte_a = source.s2_pte_a;
        s2_pte_d = source.s2_pte_d; create_cycle = source.create_cycle;
        last_hit_cycle = source.last_hit_cycle;
        foreach (s1_ppn_low[idx]) begin
            s1_ppn_low[idx] = source.s1_ppn_low[idx];
            s1_valididx[idx] = source.s1_valididx[idx];
            s1_pteidx[idx] = source.s1_pteidx[idx];
        end
        check_inactive_stage_defaults("COPY");
        validate_s1_sector_payload_consistency("COPY", source);
    endfunction:copy_from

endclass:memblock_tlb_entry

class memblock_uid_tlb_record extends uvm_object;
    typedef enum int unsigned {
        MEMBLOCK_UID_TLB_WAITING   = 0,
        MEMBLOCK_UID_TLB_COMPLETED = 1,
        MEMBLOCK_UID_TLB_CANCELED  = 2
    } memblock_uid_tlb_wait_state_e;

    memblock_uid_t uid;
    bit record_valid;
    bit pte_valid;
    bit [51:0] vpn;
    bit [1:0] s2xlate;
    bit is_hypervisor_inst;
    memblock_tlb_lookup_key_t lookup_key;
    mmu_csr_runtime_state csr_snapshot;
    // 中文注释：每次真实 issue 建立新的 WAITING epoch；response 或 flush/reset 只转换
    // 当前状态，不能以 pte_valid=0 让历史 record 自动重新等待。
    int unsigned uid_tlb_wait_epoch;
    memblock_uid_tlb_wait_state_e uid_tlb_wait_state;
    longint unsigned uid_wait_start_sample_seq;
    longint unsigned uid_tlb_first_request_fire_sample_seq;
    longint unsigned pte_update_cycle;
    memblock_tlb_entry payload;
    bit request_derived_valid;
    bit [43:0] request_s1_resolved_ppn;
    bit [43:0] request_s2_resolved_ppn;
    bit [51:0] request_gvpn;

    `uvm_object_utils(memblock_uid_tlb_record)

    function new(string name = "memblock_uid_tlb_record");
        super.new(name);
        csr_snapshot = mmu_csr_runtime_state::type_id::create({name, "_csr_snapshot"});
        payload = memblock_tlb_entry::type_id::create({name, "_payload"});
        reset();
    endfunction:new

    function void reset();
        uid = 0; record_valid = 0; pte_valid = 0; vpn = '0; s2xlate = '0;
        is_hypervisor_inst = 0; lookup_key = '{default:'0};
        if (csr_snapshot == null) csr_snapshot = mmu_csr_runtime_state::type_id::create("csr_snapshot");
        if (payload == null) payload = memblock_tlb_entry::type_id::create("payload");
        csr_snapshot.reset(); payload.reset(); uid_tlb_wait_epoch = 0;
        uid_tlb_wait_state = MEMBLOCK_UID_TLB_CANCELED;
        uid_wait_start_sample_seq = 0; uid_tlb_first_request_fire_sample_seq = 0;
        pte_update_cycle = 0; request_derived_valid = 0; request_s1_resolved_ppn = '0;
        request_s2_resolved_ppn = '0; request_gvpn = '0;
    endfunction:reset

    function void init_context(input memblock_uid_t uid_i,
                               input bit [51:0] vpn_i,
                               input bit [1:0] s2xlate_i,
                               input bit is_hypervisor_inst_i,
                               input mmu_csr_runtime_state csr_snapshot_i,
                               input longint unsigned sample_seq_i);
        if (csr_snapshot_i == null || sample_seq_i == 0) begin
            `uvm_fatal("UID_TLB_RECORD", "init_context needs valid CSR and sample sequence")
        end
        if (uid_tlb_wait_epoch == '1) begin
            `uvm_fatal("UID_TLB_RECORD", "uid_tlb_wait_epoch overflow")
        end
        uid = uid_i; record_valid = 1; pte_valid = 0; vpn = vpn_i; s2xlate = s2xlate_i;
        is_hypervisor_inst = is_hypervisor_inst_i; csr_snapshot.copy_from(csr_snapshot_i);
        lookup_key = csr_snapshot.make_lookup_key(vpn_i, s2xlate_i);
        uid_tlb_wait_epoch++; uid_tlb_wait_state = MEMBLOCK_UID_TLB_WAITING;
        uid_wait_start_sample_seq = sample_seq_i; uid_tlb_first_request_fire_sample_seq = 0;
        pte_update_cycle = 0; request_derived_valid = 0; request_s1_resolved_ppn = '0;
        request_s2_resolved_ppn = '0; request_gvpn = '0;
    endfunction:init_context

    function void copy_entry_fields(input memblock_tlb_entry entry);
        if (entry == null || !is_waiting()) begin
            `uvm_fatal("UID_TLB_RECORD", "copy_entry_fields requires a WAITING record and entry")
        end
        payload.copy_from(entry);
        pte_update_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
    endfunction:copy_entry_fields

    function void mark_request_fire(input longint unsigned sample_seq_i);
        if (!is_waiting() || sample_seq_i == 0) begin
            `uvm_fatal("UID_TLB_RECORD", "mark_request_fire requires WAITING record and sample")
        end
        if (uid_tlb_first_request_fire_sample_seq == 0) begin
            uid_tlb_first_request_fire_sample_seq = sample_seq_i;
        end
    endfunction:mark_request_fire

    function void mark_completed();
        if (!is_waiting()) `uvm_fatal("UID_TLB_RECORD", "only WAITING record can complete")
        uid_tlb_wait_state = MEMBLOCK_UID_TLB_COMPLETED;
        pte_valid = 1'b1;
    endfunction:mark_completed

    function void mark_canceled();
        if (is_waiting()) begin
            uid_tlb_wait_state = MEMBLOCK_UID_TLB_CANCELED;
            pte_valid = 1'b0;
        end
    endfunction:mark_canceled

    function bit is_waiting();
        return record_valid && uid_tlb_wait_state == MEMBLOCK_UID_TLB_WAITING;
    endfunction:is_waiting
endclass:memblock_uid_tlb_record

`endif
