//=========================================================
// memblock_dtlb_storage_fcov：DTLB V2 只读功能覆盖率。
// 普通信号端口、时钟事件采样、逐特性直接建仓；没有结构体库、function 或采样分发器。
// 时延与长度采用范围仓；物理 way/slot/子页采用逐项仓。
//=========================================================
`ifndef MEMBLOCK_DTLB_STORAGE_FCOV__SV
`define MEMBLOCK_DTLB_STORAGE_FCOV__SV

`ifdef MEMBLOCK_UT_FCOV
module memblock_dtlb_storage_fcov #(
    parameter integer GROUP_ID = 0,
    parameter integer PORT_NUM = 4
) (
    input wire clock,
    input wire reset,
    input wire reset_backend_done,
    input wire [47:0] entry_valid,
    input wire [47:0][34:0] entry_tag,
    input wire [47:0][15:0] entry_asid,
    input wire [47:0][13:0] entry_vmid,
    input wire [47:0][1:0] entry_stage,
    input wire [47:0][1:0] entry_level,
    input wire [47:0] entry_n,
    input wire [47:0][23:0] entry_ppn_low,
    input wire [47:0][7:0] entry_valididx,
    input wire [47:0][7:0] entry_pteidx,
    input wire [47:0] entry_s1_pf,
    input wire [47:0] entry_s1_g,
    input wire [3:0] lookup_valid,
    input wire [3:0][37:0] lookup_vpn,
    input wire [3:0][1:0] lookup_stage,
    input wire [3:0][47:0] lookup_hit_vector,
    input wire [15:0] csr_asid,
    input wire [15:0] csr_vasid,
    input wire [15:0] csr_vmid,
    input wire csr_virt,
    input wire csr_bme,
    input wire csr_cmode,
    input wire fence_valid,
    input wire fence_rs1,
    input wire fence_rs2,
    input wire [49:0] fence_va,
    input wire [15:0] fence_id,
    input wire fence_hv,
    input wire fence_hg,
    input wire write_valid,
    input wire [1:0] write_stage,
    input wire [34:0] write_s1tag,
    input wire [15:0] write_s1asid,
    input wire [13:0] write_s1vmid,
    input wire [40:0] write_s1ppn,
    input wire [7:0] write_valididx,
    input wire [7:0] write_pteidx,
    input wire [37:0] write_s2tag,
    input wire [13:0] write_s2vmid,
    input wire [37:0] write_s2ppn,
    input wire [5:0] write_way,
    input wire [1:0] effective_level,
    input wire effective_napot,
    input wire [3:0] touch_valid,
    input wire [3:0][5:0] touch_way
);

    reg [3:0] lookup_valid_d1;
    reg [3:0][37:0] lookup_vpn_d1;
    reg [3:0][1:0] lookup_stage_d1;
    reg [47:0] entry_valid_d1;
    reg [47:0][34:0] entry_tag_d1;
    reg [47:0][15:0] entry_asid_d1;
    reg [47:0][13:0] entry_vmid_d1;
    reg [47:0][1:0] entry_stage_d1;
    reg [47:0][1:0] entry_level_d1;
    reg [47:0] entry_n_d1;
    reg [47:0] entry_s1_g_d1;
    reg [47:0][7:0] entry_valididx_d1;
    reg [47:0][7:0] entry_pteidx_d1;
    reg [15:0] csr_asid_d1;
    reg [15:0] csr_vasid_d1;
    reg [15:0] csr_vmid_d1;
    reg fence_valid_d1;
    reg fence_hv_d1;
    reg fence_hg_d1;
    reg fence_rs1_d1;
    reg fence_rs2_d1;
    reg [49:0] fence_va_d1;
    reg [15:0] fence_id_d1;
    reg csr_virt_d1;
    reg csr_bme_d1;
    reg csr_cmode_d1;
    reg write_valid_d1;
    reg [5:0] write_way_d1;
    reg [3:0] touch_valid_d1;
    reg [47:0] fence_selected_d1;
    reg [47:0] fence_selected;
    reg [3:0][47:0] expected_hit;
    reg [3:0][47:0] tag_candidates;
    reg [3:0][47:0] asid_candidates;
    reg [3:0][47:0] vmid_candidates;
    reg [3:0][47:0] index_candidates;
    reg [3:0][47:0] pte_candidates;
    reg [3:0] cold_lookup;
    reg [3:0] invalid_rejected;
    reg [3:0] tag_rejected;
    reg [3:0] asid_hit;
    reg [3:0] asid_rejected;
    reg [3:0] global_hit;
    reg [3:0] vmid_hit;
    reg [3:0] vmid_rejected;
    reg [3:0] stage_rejected;
    reg [3:0] onlys2_asid_ignored;
    reg [3:0] host_vmid_ignored;
    reg [3:0] pte_hit;
    reg [3:0] pte_rejected;
    reg [3:0] pte_ignored_s1;
    reg [3:0] pte_ignored_large;
    reg [3:0] noncontiguous_page;
    reg [3:0] coexist_access;
    reg [3:0][7:0] selected_valididx;
    reg [3:0][1:0] selected_level;
    reg [3:0] selected_napot;
    reg [3:0][2:0] selected_ppn_low;
    reg [3:0][5:0] selected_way;
    reg [3:0] hit_comparison;
    reg all_fence_transitions_match;
    reg selected_invalidated;
    reg retained_valid;
    reg [34:0] saved_write_tag;
    reg [15:0] saved_write_asid;
    reg [13:0] saved_write_vmid;
    reg [1:0] saved_write_stage;
    reg [1:0] saved_write_level;
    reg saved_write_napot;
    reg [7:0] saved_write_valididx;
    reg [7:0] saved_write_pteidx;
    reg overlap_write;
    reg disjoint_pair;
    reg [3:0] subpage_lookup;
    reg any_sector_hit;
    reg any_napot_hit;
    reg any_noncontiguous_hit;
    reg any_coexist_access;
    reg [3:0] sector_valid_count;
    reg [3:0] napot_subpage;
    wire sample_enable = (reset === 1'b0) && (reset_backend_done === 1'b1);
    wire lookup_sample = |lookup_valid_d1;
    wire any_lookup_hit = |lookup_hit_vector;
    wire [5:0] actual_hit_count = $countones(lookup_hit_vector);
    wire [5:0] entry_occupancy = $countones(entry_valid);
    wire [5:0] invalid_victim = write_way < 48 ? !entry_valid[write_way] : 1'b0;
    wire [1:0] page_index_shape = write_valididx == 8'hff ? 2'd2 : $countones(write_valididx) == 1 ? 2'd0 : 2'd1;
    wire [7:0] expected_write_valididx = effective_level != 0 || effective_napot ? 8'hff :
        write_stage == 2 ? (8'b1 << write_s2tag[2:0]) : write_valididx;
    wire [7:0] expected_write_pteidx = write_stage == 2 ? (8'b1 << write_s2tag[2:0]) : write_pteidx;
    wire write_fields_match = write_way_d1 < 48 && entry_valid[write_way_d1] &&
        entry_tag[write_way_d1] == saved_write_tag && entry_asid[write_way_d1] == saved_write_asid &&
        entry_vmid[write_way_d1] == saved_write_vmid && entry_stage[write_way_d1] == saved_write_stage &&
        entry_level[write_way_d1] == saved_write_level && entry_n[write_way_d1] == saved_write_napot &&
        entry_valididx[write_way_d1] == saved_write_valididx && entry_pteidx[write_way_d1] == saved_write_pteidx;
    // 查询最多扫描本实例48个物理way；失效匹配只在 fence 生效拍进行。
    always @* begin
        expected_hit = '0;
        tag_candidates = '0;
        asid_candidates = '0;
        vmid_candidates = '0;
        index_candidates = '0;
        pte_candidates = '0;
        cold_lookup = '0;
        invalid_rejected = '0;
        tag_rejected = '0;
        asid_hit = '0;
        asid_rejected = '0;
        global_hit = '0;
        vmid_hit = '0;
        vmid_rejected = '0;
        stage_rejected = '0;
        onlys2_asid_ignored = '0;
        host_vmid_ignored = '0;
        pte_hit = '0;
        pte_rejected = '0;
        pte_ignored_s1 = '0;
        pte_ignored_large = '0;
        selected_valididx = '0;
        selected_level = '0;
        selected_napot = '0;
        selected_ppn_low = '0;
        selected_way = '0;
        hit_comparison = '0;
        noncontiguous_page = '0;
        coexist_access = '0;
        for (integer port = 0; port < PORT_NUM; port = port + 1) begin
            cold_lookup[port] = entry_valid_d1 == 0;
            for (integer way = 0; way < 48; way = way + 1) begin
                tag_candidates[port][way] = lookup_vpn_d1[port][37:27] == entry_tag_d1[way][34:24] &&
                    (entry_level_d1[way] == 3 || lookup_vpn_d1[port][26:18] == entry_tag_d1[way][23:15]) &&
                    (entry_level_d1[way] >= 2 || lookup_vpn_d1[port][17:9] == entry_tag_d1[way][14:6]) &&
                    (entry_level_d1[way] != 0 || (entry_n_d1[way] ? lookup_vpn_d1[port][8:4] == entry_tag_d1[way][5:1] :
                    lookup_vpn_d1[port][8:3] == entry_tag_d1[way][5:0]));
                asid_candidates[port][way] = lookup_stage_d1[port] == 2 || entry_s1_g_d1[way] ||
                    entry_asid_d1[way] == (lookup_stage_d1[port] == 0 ? csr_asid_d1 : csr_vasid_d1);
                vmid_candidates[port][way] = lookup_stage_d1[port] == 0 || {2'd0, entry_vmid_d1[way]} == csr_vmid_d1;
                index_candidates[port][way] = entry_level_d1[way] != 0 || entry_n_d1[way] ||
                    entry_valididx_d1[way][lookup_vpn_d1[port][2:0]];
                pte_candidates[port][way] = lookup_stage_d1[port] < 2 || entry_level_d1[way] != 0 || entry_n_d1[way] ||
                    entry_pteidx_d1[way][lookup_vpn_d1[port][2:0]];
                expected_hit[port][way] = entry_valid_d1[way] && entry_stage_d1[way] == lookup_stage_d1[port] &&
                    tag_candidates[port][way] && asid_candidates[port][way] && vmid_candidates[port][way] &&
                    index_candidates[port][way] && pte_candidates[port][way] && !(write_valid_d1 &&
                        write_way_d1 == way);
                if (tag_candidates[port][way] && asid_candidates[port][way] && vmid_candidates[port][way] &&
                    index_candidates[port][way] && pte_candidates[port][way]) begin
                    invalid_rejected[port] = invalid_rejected[port] || !entry_valid_d1[way];
                    stage_rejected[port] = stage_rejected[port] || (entry_valid_d1[way] &&
                        entry_stage_d1[way] != lookup_stage_d1[port]);
                end
                if (entry_valid_d1[way] && tag_candidates[port][way] &&
                    entry_stage_d1[way] == lookup_stage_d1[port]) begin
                    asid_hit[port] = asid_hit[port] || asid_candidates[port][way];
                    asid_rejected[port] = asid_rejected[port] || !asid_candidates[port][way];
                    global_hit[port] = global_hit[port] || (entry_s1_g_d1[way] && entry_asid_d1[way] != csr_asid_d1);
                    vmid_hit[port] = vmid_hit[port] || vmid_candidates[port][way];
                    vmid_rejected[port] = vmid_rejected[port] || !vmid_candidates[port][way];
                    onlys2_asid_ignored[port] = onlys2_asid_ignored[port] || lookup_stage_d1[port] == 2;
                    host_vmid_ignored[port] = host_vmid_ignored[port] || lookup_stage_d1[port] == 0;
                    pte_hit[port] = pte_hit[port] || (lookup_stage_d1[port] >= 2 && pte_candidates[port][way]);
                    pte_rejected[port] = pte_rejected[port] || (lookup_stage_d1[port] >= 2 &&
                        !pte_candidates[port][way]);
                    pte_ignored_s1[port] = pte_ignored_s1[port] || (lookup_stage_d1[port] < 2 &&
                        !entry_pteidx_d1[way][lookup_vpn_d1[port][2:0]]);
                    pte_ignored_large[port] = pte_ignored_large[port] || ((entry_level_d1[way] != 0 ||
                        entry_n_d1[way]) &&
                        !entry_pteidx_d1[way][lookup_vpn_d1[port][2:0]]);
                end
                tag_rejected[port] = tag_rejected[port] || (entry_valid_d1[way] && !tag_candidates[port][way] &&
                    entry_stage_d1[way] == lookup_stage_d1[port] && asid_candidates[port][way] &&
                        vmid_candidates[port][way]);
                if (lookup_hit_vector[port][way]) begin
                    selected_valididx[port] = selected_valididx[port] | entry_valididx_d1[way];
                    selected_level[port] = selected_level[port] | entry_level_d1[way];
                    selected_napot[port] = selected_napot[port] | entry_n_d1[way];
                    selected_ppn_low[port] = selected_ppn_low[port] | entry_ppn_low[way][lookup_vpn_d1[port][2:0]*3 +: 3];
                    selected_way[port] = way;
                    noncontiguous_page[port] = noncontiguous_page[port] ||
                        entry_ppn_low[way][2:0] != entry_ppn_low[way][5:3];
                    coexist_access[port] = coexist_access[port] || disjoint_pair;
                end
            end
            hit_comparison[port] = lookup_hit_vector[port] == expected_hit[port];
        end
        fence_selected = 0;
        overlap_write = 0;
        disjoint_pair = 0;
        for (integer way = 0; way < 48; way = way + 1) begin
            if (write_valid && entry_valid[way] && entry_tag[way] == write_s1tag &&
                entry_stage[way] == write_stage) begin
                overlap_write = overlap_write || ((entry_valididx[way] & write_valididx) != 0);
                disjoint_pair = disjoint_pair || ((entry_valididx[way] & write_valididx) == 0);
            end
            if (fence_hg) fence_selected[way] = entry_stage[way] != 0 && (fence_rs2 ||
                {2'd0, entry_vmid[way]} == fence_id);
            else if (fence_hv || csr_virt) fence_selected[way] = entry_stage[way] != 0 &&
                {2'd0, entry_vmid[way]} == csr_vmid &&
                (fence_rs2 || (entry_asid[way] == fence_id && !entry_s1_g[way]));
            else if (fence_rs1 || (csr_bme && !csr_cmode)) fence_selected[way] = entry_stage[way] == 0 &&
                (fence_rs2 || (entry_asid[way] == fence_id && !entry_s1_g[way]));
            else fence_selected[way] = fence_va[49:39] == entry_tag[way][34:24] &&
                (entry_level[way] == 3 || fence_va[38:30] == entry_tag[way][23:15]) &&
                (entry_level[way] >= 2 || fence_va[29:21] == entry_tag[way][14:6]) &&
                (entry_level[way] != 0 ||
                    (entry_n[way] ? fence_va[20:16] == entry_tag[way][5:1] : fence_va[20:15] ==
                        entry_tag[way][5:0])) &&
                (entry_level[way] != 0 || entry_n[way] || entry_valididx[way][fence_va[14:12]]) &&
                (fence_rs2 || (entry_asid[way] == fence_id && !entry_s1_g[way]));
        end
        all_fence_transitions_match = 1;
        selected_invalidated = 0;
        retained_valid = 0;
        for (integer way = 0; way < 48; way = way + 1) begin
            if (entry_valid_d1[way]) begin
                all_fence_transitions_match = all_fence_transitions_match &&
                    (entry_valid[way] == !fence_selected_d1[way]);
                selected_invalidated = selected_invalidated || (fence_selected_d1[way] && !entry_valid[way]);
                retained_valid = retained_valid || (!fence_selected_d1[way] && entry_valid[way]);
            end
        end
    end
    wire any_cold_lookup = |(cold_lookup & lookup_valid_d1);
    wire any_invalid_candidate = |(invalid_rejected & lookup_valid_d1);
    wire any_tag_rejection = |(tag_rejected & lookup_valid_d1);
    wire any_asid_hit = |(asid_hit & lookup_valid_d1);
    wire any_asid_rejection = |(asid_rejected & lookup_valid_d1);
    wire any_global_hit = |(global_hit & lookup_valid_d1);
    wire any_vmid_hit = |(vmid_hit & lookup_valid_d1);
    wire any_vmid_rejection = |(vmid_rejected & lookup_valid_d1);
    wire any_stage_rejection = |(stage_rejected & lookup_valid_d1);
    wire any_s2_asid_ignored = |(onlys2_asid_ignored & lookup_valid_d1);
    wire any_host_vmid_ignored = |(host_vmid_ignored & lookup_valid_d1);
    wire any_pte_hit = |(pte_hit & lookup_valid_d1);
    wire any_pte_rejection = |(pte_rejected & lookup_valid_d1);
    wire any_pte_ignored_s1 = |(pte_ignored_s1 & lookup_valid_d1);
    wire any_pte_ignored_large = |(pte_ignored_large & lookup_valid_d1);
    wire all_lookup_matches = (hit_comparison | ~lookup_valid_d1) == 4'hf;
    wire all_entries_known = !$isunknown({entry_valid, entry_tag, entry_asid, entry_vmid});
    wire [47:0] any_way_hit = lookup_hit_vector[0] | lookup_hit_vector[1] | lookup_hit_vector[2] | lookup_hit_vector[3];
    wire invalid_write_way = write_valid && write_way >= 48;
    always @* begin
        subpage_lookup = 0;
        any_sector_hit = 0;
        any_napot_hit = 0;
        any_noncontiguous_hit = 0;
        any_coexist_access = 0;
        sector_valid_count = 0;
        napot_subpage = 0;
        for (integer port = 0; port < PORT_NUM; port = port + 1) begin
            if (lookup_valid_d1[port] && |lookup_hit_vector[port]) begin
                subpage_lookup = {1'b0, lookup_vpn_d1[port][2:0]};
                any_sector_hit = any_sector_hit || (selected_level[port] == 0 && !selected_napot[port]);
                any_napot_hit = any_napot_hit || (selected_level[port] == 0 && selected_napot[port]);
                napot_subpage = lookup_vpn_d1[port][3:0];
                sector_valid_count = $countones(selected_valididx[port]);
                any_noncontiguous_hit = any_noncontiguous_hit || noncontiguous_page[port];
                any_coexist_access = any_coexist_access || coexist_access[port];
            end
        end
    end
    always @(posedge clock) begin
        if (reset) begin
            lookup_valid_d1 <= '0;
            lookup_vpn_d1 <= '0;
            lookup_stage_d1 <= '0;
            entry_valid_d1 <= '0;
            entry_tag_d1 <= '0;
            entry_asid_d1 <= '0;
            entry_vmid_d1 <= '0;
            entry_stage_d1 <= '0;
            entry_level_d1 <= '0;
            entry_n_d1 <= '0;
            entry_s1_g_d1 <= '0;
            entry_valididx_d1 <= '0;
            entry_pteidx_d1 <= '0;
            csr_asid_d1 <= '0;
            csr_vasid_d1 <= '0;
            csr_vmid_d1 <= '0;
            fence_valid_d1 <= '0;
            fence_hv_d1 <= '0;
            fence_hg_d1 <= '0;
            fence_rs1_d1 <= '0;
            fence_rs2_d1 <= '0;
            fence_va_d1 <= '0;
            fence_id_d1 <= '0;
            csr_virt_d1 <= '0;
            csr_bme_d1 <= '0;
            csr_cmode_d1 <= '0;
            write_valid_d1 <= '0;
            write_way_d1 <= '0;
            touch_valid_d1 <= '0;
            fence_selected_d1 <= 0;
            saved_write_tag <= 0;
            saved_write_asid <= 0;
            saved_write_vmid <= 0;
            saved_write_stage <= 0;
            saved_write_level <= 0;
            saved_write_napot <= 0;
            saved_write_valididx <= 0;
            saved_write_pteidx <= 0;
        end else begin
            lookup_valid_d1 <= lookup_valid;
            lookup_vpn_d1 <= lookup_vpn;
            lookup_stage_d1 <= lookup_stage;
            entry_valid_d1 <= entry_valid;
            entry_tag_d1 <= entry_tag;
            entry_asid_d1 <= entry_asid;
            entry_vmid_d1 <= entry_vmid;
            entry_stage_d1 <= entry_stage;
            entry_level_d1 <= entry_level;
            entry_n_d1 <= entry_n;
            entry_s1_g_d1 <= entry_s1_g;
            entry_valididx_d1 <= entry_valididx;
            entry_pteidx_d1 <= entry_pteidx;
            csr_asid_d1 <= csr_asid;
            csr_vasid_d1 <= csr_vasid;
            csr_vmid_d1 <= csr_vmid;
            fence_valid_d1 <= fence_valid;
            fence_hv_d1 <= fence_hv;
            fence_hg_d1 <= fence_hg;
            fence_rs1_d1 <= fence_rs1;
            fence_rs2_d1 <= fence_rs2;
            fence_va_d1 <= fence_va;
            fence_id_d1 <= fence_id;
            csr_virt_d1 <= csr_virt;
            csr_bme_d1 <= csr_bme;
            csr_cmode_d1 <= csr_cmode;
            write_valid_d1 <= write_valid;
            write_way_d1 <= write_way;
            touch_valid_d1 <= touch_valid;
            fence_selected_d1 <= fence_selected;
            if (write_valid) begin
                saved_write_tag <= write_stage == 2 ? write_s2tag[37:3] : write_s1tag;
                saved_write_asid <= write_s1asid;
                saved_write_vmid <= write_stage == 2 ? write_s2vmid : write_s1vmid;
                saved_write_stage <= write_stage;
                saved_write_level <= effective_level;
                saved_write_napot <= effective_napot;
                saved_write_valididx <= expected_write_valididx;
                saved_write_pteidx <= expected_write_pteidx;
            end
        end
    end
    // 诊断独立统计，不计入功能分母，不调用UVM错误报告。
    covergroup cg_interface_diagnostics @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_lookup_result : coverpoint ($isunknown(all_lookup_matches) ? 2'd2 : (all_lookup_matches) ? 2'd0 : 2'd1)
            iff (sample_enable && (lookup_sample)) {
            bins b_mismatch = {2'd1};
            bins b_unknown = {2'd2};
        }
        cp_refill_save : coverpoint ($isunknown(write_fields_match) ? 2'd2 : (write_fields_match) ? 2'd0 : 2'd1)
            iff (sample_enable && (write_valid_d1)) {
            bins b_mismatch = {2'd1};
            bins b_unknown = {2'd2};
        }
        cp_fence_effect : coverpoint ($isunknown(all_fence_transitions_match) ? 2'd2 : (all_fence_transitions_match)
            ? 2'd0 : 2'd1)
            iff (sample_enable && (fence_valid_d1)) {
            bins b_mismatch = {2'd1};
            bins b_unknown = {2'd2};
        }
    endgroup
    cg_interface_diagnostics interface_diagnostics;
    initial begin
        interface_diagnostics = new();
    end

    // 03.01 有效项与Tag命中
    covergroup cg_dtlb_03_01 @(posedge clock);
        option.per_instance = 1;
        // 03.01.001 LOOKUP_冷启动miss
        cp_03_01_001 : coverpoint any_lookup_hit
            iff (sample_enable && (any_cold_lookup) && ((!any_lookup_hit) === 1'b1)) {
            bins b_expected_clear = {1'b0};
        }
        // 03.01.003 LOOKUP_valid0不命中
        cp_03_01_003 : coverpoint all_lookup_matches
            iff (sample_enable && (any_invalid_candidate)) {
            bins b_expected_set = {1'b1};
        }
        // 03.01.004 LOOKUP_多个tag片段不匹配
        cp_03_01_004 : coverpoint all_lookup_matches
            iff (sample_enable && (any_tag_rejection)) {
            bins b_expected_set = {1'b1};
        }
        // 03.01.002 LOOKUP_各way命中
        cp_03_01_002 : coverpoint any_way_hit
            iff (sample_enable && (lookup_sample && any_lookup_hit) && ((all_lookup_matches) === 1'b1)) {
            wildcard bins b_way_0 = {48'b???????????????????????????????????????????????1};
            wildcard bins b_way_1 = {48'b??????????????????????????????????????????????1?};
            wildcard bins b_way_2 = {48'b?????????????????????????????????????????????1??};
            wildcard bins b_way_3 = {48'b????????????????????????????????????????????1???};
            wildcard bins b_way_4 = {48'b???????????????????????????????????????????1????};
            wildcard bins b_way_5 = {48'b??????????????????????????????????????????1?????};
            wildcard bins b_way_6 = {48'b?????????????????????????????????????????1??????};
            wildcard bins b_way_7 = {48'b????????????????????????????????????????1???????};
            wildcard bins b_way_8 = {48'b???????????????????????????????????????1????????};
            wildcard bins b_way_9 = {48'b??????????????????????????????????????1?????????};
            wildcard bins b_way_10 = {48'b?????????????????????????????????????1??????????};
            wildcard bins b_way_11 = {48'b????????????????????????????????????1???????????};
            wildcard bins b_way_12 = {48'b???????????????????????????????????1????????????};
            wildcard bins b_way_13 = {48'b??????????????????????????????????1?????????????};
            wildcard bins b_way_14 = {48'b?????????????????????????????????1??????????????};
            wildcard bins b_way_15 = {48'b????????????????????????????????1???????????????};
            wildcard bins b_way_16 = {48'b???????????????????????????????1????????????????};
            wildcard bins b_way_17 = {48'b??????????????????????????????1?????????????????};
            wildcard bins b_way_18 = {48'b?????????????????????????????1??????????????????};
            wildcard bins b_way_19 = {48'b????????????????????????????1???????????????????};
            wildcard bins b_way_20 = {48'b???????????????????????????1????????????????????};
            wildcard bins b_way_21 = {48'b??????????????????????????1?????????????????????};
            wildcard bins b_way_22 = {48'b?????????????????????????1??????????????????????};
            wildcard bins b_way_23 = {48'b????????????????????????1???????????????????????};
            wildcard bins b_way_24 = {48'b???????????????????????1????????????????????????};
            wildcard bins b_way_25 = {48'b??????????????????????1?????????????????????????};
            wildcard bins b_way_26 = {48'b?????????????????????1??????????????????????????};
            wildcard bins b_way_27 = {48'b????????????????????1???????????????????????????};
            wildcard bins b_way_28 = {48'b???????????????????1????????????????????????????};
            wildcard bins b_way_29 = {48'b??????????????????1?????????????????????????????};
            wildcard bins b_way_30 = {48'b?????????????????1??????????????????????????????};
            wildcard bins b_way_31 = {48'b????????????????1???????????????????????????????};
            wildcard bins b_way_32 = {48'b???????????????1????????????????????????????????};
            wildcard bins b_way_33 = {48'b??????????????1?????????????????????????????????};
            wildcard bins b_way_34 = {48'b?????????????1??????????????????????????????????};
            wildcard bins b_way_35 = {48'b????????????1???????????????????????????????????};
            wildcard bins b_way_36 = {48'b???????????1????????????????????????????????????};
            wildcard bins b_way_37 = {48'b??????????1?????????????????????????????????????};
            wildcard bins b_way_38 = {48'b?????????1??????????????????????????????????????};
            wildcard bins b_way_39 = {48'b????????1???????????????????????????????????????};
            wildcard bins b_way_40 = {48'b???????1????????????????????????????????????????};
            wildcard bins b_way_41 = {48'b??????1?????????????????????????????????????????};
            wildcard bins b_way_42 = {48'b?????1??????????????????????????????????????????};
            wildcard bins b_way_43 = {48'b????1???????????????????????????????????????????};
            wildcard bins b_way_44 = {48'b???1????????????????????????????????????????????};
            wildcard bins b_way_45 = {48'b??1?????????????????????????????????????????????};
            wildcard bins b_way_46 = {48'b?1??????????????????????????????????????????????};
            wildcard bins b_way_47 = {48'b1???????????????????????????????????????????????};
        }
    endgroup
    cg_dtlb_03_01 cg_dtlb_03_01_inst;

    // 03.02 ASID、VMID与翻译阶段隔离
    covergroup cg_dtlb_03_02 @(posedge clock);
        option.per_instance = 1;
        // 03.02.001 CTX_ASID命中
        cp_03_02_001 : coverpoint all_lookup_matches
            iff (sample_enable && (any_asid_hit)) {
            bins b_expected_set = {1'b1};
        }
        // 03.02.002 CTX_ASID不匹配
        cp_03_02_002 : coverpoint all_lookup_matches
            iff (sample_enable && (any_asid_rejection)) {
            bins b_expected_set = {1'b1};
        }
        // 03.02.003 CTX_global跨ASID
        cp_03_02_003 : coverpoint all_lookup_matches
            iff (sample_enable && (any_global_hit)) {
            bins b_expected_set = {1'b1};
        }
        // 03.02.004 CTX_onlyS2忽略ASID
        cp_03_02_004 : coverpoint all_lookup_matches
            iff (sample_enable && (any_s2_asid_ignored)) {
            bins b_expected_set = {1'b1};
        }
        // 03.02.005 CTX_guest_VM匹配_匹配
        cp_03_02_005 : coverpoint all_lookup_matches
            iff (sample_enable && (any_vmid_hit)) {
            bins b_expected_set = {1'b1};
        }
        // 03.02.006 CTX_guest_VM匹配_不匹配
        cp_03_02_006 : coverpoint all_lookup_matches
            iff (sample_enable && (any_vmid_rejection)) {
            bins b_expected_set = {1'b1};
        }
        // 03.02.007 CTX_host忽略VMID
        cp_03_02_007 : coverpoint all_lookup_matches
            iff (sample_enable && (any_host_vmid_ignored)) {
            bins b_expected_set = {1'b1};
        }
        // 03.02.008 CTX_stage编码隔离
        cp_03_02_008 : coverpoint all_lookup_matches
            iff (sample_enable && (any_stage_rejection)) {
            bins b_expected_set = {1'b1};
        }
        // 03.02.009 CTX_同页多上下文共存
        cp_03_02_009 : coverpoint all_lookup_matches
            iff (sample_enable && (lookup_sample && any_lookup_hit && csr_asid != csr_asid_d1)) {
            bins b_expected_set = {1'b1};
        }
    endgroup
    cg_dtlb_03_02 cg_dtlb_03_02_inst;

    // 03.02 ASID、VMID与翻译阶段隔离
    covergroup cg_dtlb_diag_03_02 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        // 03.02.010：结果偏离或未知单列，正常命中不进入诊断仓。
        rs_03_02_010 : coverpoint ($isunknown(1'b1) ? 2'd2 : (1'b1) ? 2'd0 : 2'd1)
            iff (sample_enable && (lookup_sample && csr_vmid[15:14] != 0)) {
            bins b_mismatch = {2'd1};
            bins unknown  = {2'd2};
        }
        // 03.02.010 CTX_VMID宽度
        cp_03_02_010 : coverpoint (lookup_sample && csr_vmid[15:14] != 0)
            iff (sample_enable && (lookup_sample && csr_vmid[15:14] != 0)) {
            bins b_observed = {1'b1};
        }
    endgroup
    cg_dtlb_diag_03_02 cg_dtlb_diag_03_02_inst;

    // 03.03 基础页与超级页地址拼接
    covergroup cg_dtlb_03_03 @(posedge clock);
        option.per_instance = 1;
    endgroup
    cg_dtlb_03_03 cg_dtlb_03_03_inst;

    // 03.03 基础页与超级页地址拼接
    covergroup cg_dtlb_diag_03_03 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        // 03.03.005：结果偏离或未知单列，正常命中不进入诊断仓。
        rs_03_03_005 : coverpoint ($isunknown(1'b1) ? 2'd2 : (1'b1) ? 2'd0 : 2'd1)
            iff (sample_enable && (write_valid && (write_s1ppn[40:33] != 0 || write_s2ppn[37:36] != 0))) {
            bins b_mismatch = {2'd1};
            bins unknown  = {2'd2};
        }
        // 03.03.005 LOOKUP_PPN截位
        cp_03_03_005 : coverpoint (write_valid && (write_s1ppn[40:33] != 0 || write_s2ppn[37:36] != 0))
            iff (sample_enable && (write_valid && (write_s1ppn[40:33] != 0 || write_s2ppn[37:36] != 0))) {
            bins b_observed = {1'b1};
        }
    endgroup
    cg_dtlb_diag_03_03 cg_dtlb_diag_03_03_inst;

    // 03.04 Sector有效子页与PPN选择
    covergroup cg_dtlb_03_04 @(posedge clock);
        option.per_instance = 1;
        // 03.04.010 SECTOR_非连续物理页低位
        cp_03_04_010 : coverpoint all_lookup_matches
            iff (sample_enable && (any_sector_hit && any_noncontiguous_hit)) {
            bins b_expected_set = {1'b1};
        }
        // 03.04.011 SECTOR_同tag不同有效子页共存
        cp_03_04_011 : coverpoint all_lookup_matches
            iff (sample_enable && (any_coexist_access)) {
            bins b_expected_set = {1'b1};
        }
        // 03.04.012 SECTOR_onlyS2生成索引
        cp_03_04_012 : coverpoint write_fields_match
            iff (sample_enable && (write_valid_d1 && saved_write_stage == 2 && saved_write_level == 0 &&
                !saved_write_napot)) {
            bins b_expected_set = {1'b1};
        }
        // 03.04.001 SECTOR_子页0
        cp_03_04_001 : coverpoint all_lookup_matches
            iff (sample_enable && (any_sector_hit && subpage_lookup == 0)) {
            bins b_expected_set = {1'b1};
        }
        // 03.04.002 SECTOR_子页1
        cp_03_04_002 : coverpoint all_lookup_matches
            iff (sample_enable && (any_sector_hit && subpage_lookup == 1)) {
            bins b_expected_set = {1'b1};
        }
        // 03.04.003 SECTOR_子页2
        cp_03_04_003 : coverpoint all_lookup_matches
            iff (sample_enable && (any_sector_hit && subpage_lookup == 2)) {
            bins b_expected_set = {1'b1};
        }
        // 03.04.004 SECTOR_子页3
        cp_03_04_004 : coverpoint all_lookup_matches
            iff (sample_enable && (any_sector_hit && subpage_lookup == 3)) {
            bins b_expected_set = {1'b1};
        }
        // 03.04.005 SECTOR_子页4
        cp_03_04_005 : coverpoint all_lookup_matches
            iff (sample_enable && (any_sector_hit && subpage_lookup == 4)) {
            bins b_expected_set = {1'b1};
        }
        // 03.04.006 SECTOR_子页5
        cp_03_04_006 : coverpoint all_lookup_matches
            iff (sample_enable && (any_sector_hit && subpage_lookup == 5)) {
            bins b_expected_set = {1'b1};
        }
        // 03.04.007 SECTOR_子页6
        cp_03_04_007 : coverpoint all_lookup_matches
            iff (sample_enable && (any_sector_hit && subpage_lookup == 6)) {
            bins b_expected_set = {1'b1};
        }
        // 03.04.008 SECTOR_子页7
        cp_03_04_008 : coverpoint all_lookup_matches
            iff (sample_enable && (any_sector_hit && subpage_lookup == 7)) {
            bins b_expected_set = {1'b1};
        }
        // 03.04.009 SECTOR_valididx形状
        cp_03_04_009 : coverpoint sector_valid_count
            iff (sample_enable && (any_sector_hit) && ((all_lookup_matches) === 1'b1)) {
            bins b_single = {1};
            bins b_multiple = {[2:7]};
            bins b_all = {8};
        }
    endgroup
    cg_dtlb_03_04 cg_dtlb_03_04_inst;

    // 03.05 Sector匹配限制与多命中
    covergroup cg_dtlb_03_05 @(posedge clock);
        option.per_instance = 1;
        // 03.05.002 SECTOR_pteidx门控_匹配
        cp_03_05_002 : coverpoint all_lookup_matches
            iff (sample_enable && (any_pte_hit)) {
            bins b_expected_set = {1'b1};
        }
        // 03.05.003 SECTOR_pteidx门控_不匹配
        cp_03_05_003 : coverpoint all_lookup_matches
            iff (sample_enable && (any_pte_rejection)) {
            bins b_expected_set = {1'b1};
        }
        // 03.05.004 SECTOR_onlyS1忽略pteidx
        cp_03_05_004 : coverpoint all_lookup_matches
            iff (sample_enable && (any_pte_ignored_s1)) {
            bins b_expected_set = {1'b1};
        }
        // 03.05.005 SECTOR_大页忽略pteidx
        cp_03_05_005 : coverpoint all_lookup_matches
            iff (sample_enable && (any_pte_ignored_large)) {
            bins b_expected_set = {1'b1};
        }
        // 03.05.006 SECTOR_onlyS1旁路与缓存粒度
        cp_03_05_006 : coverpoint all_lookup_matches
            iff (sample_enable && (lookup_sample && |lookup_stage_d1 && any_sector_hit)) {
            bins b_expected_set = {1'b1};
        }
        // 03.05.001 SECTOR_扩展valididx多命中
        cp_03_05_001 : coverpoint actual_hit_count
            iff (sample_enable && (lookup_sample && actual_hit_count >= 2) && ((all_lookup_matches) === 1'b1)) {
            bins b_two = {2};
            bins b_multiple = {[3:48]};
        }
    endgroup
    cg_dtlb_03_05 cg_dtlb_03_05_inst;

    // 03.06 64KB NAPOT命中范围
    covergroup cg_dtlb_03_06 @(posedge clock);
        option.per_instance = 1;
        // 03.06.002 NAPOT_回填索引扩展
        cp_03_06_002 : coverpoint (write_fields_match && entry_valididx[write_way_d1] == 255)
            iff (sample_enable && (write_valid_d1 && saved_write_napot)) {
            bins b_expected_result = {1'b1};
        }
        // 03.06.003 NAPOT_单stage透传
        cp_03_06_003 : coverpoint write_fields_match
            iff (sample_enable && (write_valid_d1 && saved_write_stage != 3)) {
            bins b_expected_set = {1'b1};
        }
        // 03.06.001 NAPOT_64KB命中范围
        cp_03_06_001 : coverpoint napot_subpage
            iff (sample_enable && (any_napot_hit) && ((all_lookup_matches) === 1'b1)) {
            bins b_subpage[] = {[0:15]};
        }
    endgroup
    cg_dtlb_03_06 cg_dtlb_03_06_inst;

    // 06.01 回填使能、抑制与字段保存
    covergroup cg_dtlb_06_01 @(posedge clock);
        option.per_instance = 1;
        // 06.01.006 REFILL_异常结果缓存
        cp_06_01_006 : coverpoint all_lookup_matches
            iff (sample_enable && (lookup_sample && any_lookup_hit && |entry_s1_pf)) {
            bins b_expected_set = {1'b1};
        }
        // 06.01.007 REFILL_全字段保存
        cp_06_01_007 : coverpoint write_fields_match
            iff (sample_enable && (write_valid_d1)) {
            bins b_expected_set = {1'b1};
        }
        // 06.01.010 FCOV_回填范围与索引
        cp_06_01_010 : coverpoint write_way
            iff (sample_enable && (write_valid) && ((write_way < 48) === 1'b1)) {
            bins b_way[] = {[0:47]};
        }
    endgroup
    cg_dtlb_06_01 cg_dtlb_06_01_inst;

    // 06.01 回填使能、抑制与字段保存
    covergroup cg_dtlb_diag_06_01 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        // 06.01.008：结果偏离或未知单列，正常命中不进入诊断仓。
        rs_06_01_008 : coverpoint ($isunknown(1'b1) ? 2'd2 : (1'b1) ? 2'd0 : 2'd1)
            iff (sample_enable && (write_valid && overlap_write)) {
            bins b_mismatch = {2'd1};
            bins unknown  = {2'd2};
        }
        // 06.01.008 REFILL_重叠重复检测
        cp_06_01_008 : coverpoint (write_valid && overlap_write)
            iff (sample_enable && (write_valid && overlap_write)) {
            bins b_observed = {1'b1};
        }
    endgroup
    cg_dtlb_diag_06_01 cg_dtlb_diag_06_01_inst;

    // 06.03 PTW旁路与读写冲突
    covergroup cg_dtlb_06_03 @(posedge clock);
        option.per_instance = 1;
        // 06.03.005 RW_读被替换way
        cp_06_03_005 : coverpoint all_lookup_matches
            iff (sample_enable && (lookup_sample && write_valid_d1 && any_way_hit[write_way_d1])) {
            bins b_expected_set = {1'b1};
        }
        // 06.03.006 RW_读其他way
        cp_06_03_006 : coverpoint all_lookup_matches
            iff (sample_enable && (lookup_sample && write_valid_d1 && any_lookup_hit && !any_way_hit[write_way_d1])) {
            bins b_expected_set = {1'b1};
        }
    endgroup
    cg_dtlb_06_03 cg_dtlb_06_03_inst;

    // 06.04 PLRU更新与容量替换
    covergroup cg_dtlb_06_04 @(posedge clock);
        option.per_instance = 1;
        // 06.04.002 REPLACE_有效与无效victim
        cp_06_04_002 : coverpoint invalid_victim
            iff (sample_enable && (write_valid)) {
            bins b_valid_victim = {1'b0};
            bins b_invalid_victim = {1'b1};
        }
        // 06.04.005 REPLACE_回填touch优先
        cp_06_04_005 : coverpoint (touch_way[0] == write_way_d1)
            iff (sample_enable && (write_valid_d1 && |touch_valid)) {
            bins b_expected_result = {1'b1};
        }
        // 06.04.001 REPLACE_各way被选
        cp_06_04_001 : coverpoint write_way
            iff (sample_enable && (write_valid) && ((write_way < 48) === 1'b1)) {
            bins b_way[] = {[0:47]};
        }
    endgroup
    cg_dtlb_06_04 cg_dtlb_06_04_inst;

    // 08.01 SFENCE和HFENCE范围选择
    covergroup cg_dtlb_08_01 @(posedge clock);
        option.per_instance = 1;
        // 08.01.001 SFENCE_host_11
        cp_08_01_001 : coverpoint all_fence_transitions_match
            iff (sample_enable && (fence_valid_d1 && (!fence_hv_d1 && !fence_hg_d1 && !csr_virt_d1 && fence_rs1_d1
 && fence_rs2_d1))) {
            bins b_expected_set = {1'b1};
        }
        // 08.01.002 SFENCE_host_10
        cp_08_01_002 : coverpoint all_fence_transitions_match
            iff (sample_enable && (fence_valid_d1 && (!fence_hv_d1 && !fence_hg_d1 && !csr_virt_d1 && fence_rs1_d1
 && !fence_rs2_d1))) {
            bins b_expected_set = {1'b1};
        }
        // 08.01.003 SFENCE_host_01
        cp_08_01_003 : coverpoint all_fence_transitions_match
            iff (sample_enable && (fence_valid_d1 && (!fence_hv_d1 && !fence_hg_d1 && !csr_virt_d1 && !fence_rs1_d1
 && fence_rs2_d1))) {
            bins b_expected_set = {1'b1};
        }
        // 08.01.004 SFENCE_host_00
        cp_08_01_004 : coverpoint all_fence_transitions_match
            iff (sample_enable && (fence_valid_d1 && (!fence_hv_d1 && !fence_hg_d1 && !csr_virt_d1 && !fence_rs1_d1
 && !fence_rs2_d1))) {
            bins b_expected_set = {1'b1};
        }
        // 08.01.005 SFENCE_guest_rs20
        cp_08_01_005 : coverpoint all_fence_transitions_match
            iff (sample_enable && (fence_valid_d1 && (!fence_hv_d1 && !fence_hg_d1 && csr_virt_d1 && !fence_rs2_d1))) {
            bins b_expected_set = {1'b1};
        }
        // 08.01.006 SFENCE_guest_rs21
        cp_08_01_006 : coverpoint all_fence_transitions_match
            iff (sample_enable && (fence_valid_d1 && (!fence_hv_d1 && !fence_hg_d1 && csr_virt_d1 && fence_rs2_d1))) {
            bins b_expected_set = {1'b1};
        }
        // 08.01.007 HFENCE_VVMA_rs20
        cp_08_01_007 : coverpoint all_fence_transitions_match
            iff (sample_enable && (fence_valid_d1 && (fence_hv_d1 && !fence_hg_d1 && !fence_rs2_d1))) {
            bins b_expected_set = {1'b1};
        }
        // 08.01.008 HFENCE_VVMA_rs21
        cp_08_01_008 : coverpoint all_fence_transitions_match
            iff (sample_enable && (fence_valid_d1 && (fence_hv_d1 && !fence_hg_d1 && fence_rs2_d1))) {
            bins b_expected_set = {1'b1};
        }
        // 08.01.009 HFENCE_GVMA_rs20
        cp_08_01_009 : coverpoint all_fence_transitions_match
            iff (sample_enable && (fence_valid_d1 && (fence_hg_d1 && !fence_hv_d1 && !fence_rs2_d1))) {
            bins b_expected_set = {1'b1};
        }
        // 08.01.010 HFENCE_GVMA_rs21
        cp_08_01_010 : coverpoint all_fence_transitions_match
            iff (sample_enable && (fence_valid_d1 && (fence_hg_d1 && !fence_hv_d1 && fence_rs2_d1))) {
            bins b_expected_set = {1'b1};
        }
        // 08.01.011 FENCE_BME扩大失效
        cp_08_01_011 : coverpoint all_fence_transitions_match
            iff (sample_enable && (fence_valid_d1 && (!fence_hv_d1 && !fence_hg_d1 && !csr_virt_d1 && !fence_rs1_d1
 && csr_bme_d1 && !csr_cmode_d1))) {
            bins b_expected_set = {1'b1};
        }
        // 08.01.013 SFENCE_host指定地址误扩guest
        cp_08_01_013 : coverpoint all_fence_transitions_match
            iff (sample_enable && (fence_valid_d1 && (!fence_hv_d1 && !fence_hg_d1 && !csr_virt_d1 && !fence_rs1_d1
 && |entry_stage_d1))) {
            bins b_expected_set = {1'b1};
        }
    endgroup
    cg_dtlb_08_01 cg_dtlb_08_01_inst;

    // 08.02 页型相关失效与保留
    covergroup cg_dtlb_08_02 @(posedge clock);
        option.per_instance = 1;
        // 08.02.001 FENCE_sector整项清除
        cp_08_02_001 : coverpoint all_fence_transitions_match
            iff (sample_enable && (fence_valid_d1 && |(entry_valid_d1 & fence_selected_d1))) {
            bins b_expected_set = {1'b1};
        }
        // 08.02.002 FENCE_sector无效子页不匹配
        cp_08_02_002 : coverpoint all_fence_transitions_match
            iff (sample_enable && (fence_valid_d1 && retained_valid)) {
            bins b_expected_set = {1'b1};
        }
        // 08.02.003 FENCE_页内任意地址
        cp_08_02_003 : coverpoint all_fence_transitions_match
            iff (sample_enable && (fence_valid_d1 && |entry_n_d1)) {
            bins b_expected_set = {1'b1};
        }
        // 08.02.004 FENCE_异常页尺寸
        cp_08_02_004 : coverpoint all_fence_transitions_match
            iff (sample_enable && (fence_valid_d1 && |entry_s1_pf)) {
            bins b_expected_set = {1'b1};
        }
        // 08.02.005 FCOV_失效与保留转换
        cp_08_02_005 : coverpoint selected_invalidated ? 1'b0 : 1'b1
            iff (sample_enable && (fence_valid_d1 && (selected_invalidated || retained_valid)) &&
                ((all_fence_transitions_match) === 1'b1)) {
            bins b_selected_invalidated = {1'b0};
            bins b_retained_valid = {1'b1};
        }
    endgroup
    cg_dtlb_08_02 cg_dtlb_08_02_inst;

    // 08.05 在途刷新与新旧上下文恢复
    covergroup cg_dtlb_08_05 @(posedge clock);
        option.per_instance = 1;
        // 08.05.002 FENCE_查表同拍
        cp_08_05_002 : coverpoint (fence_valid && |lookup_valid)
            iff (sample_enable && (fence_valid && |lookup_valid)) {
            bins b_observed = {1'b1};
        }
    endgroup
    cg_dtlb_08_05 cg_dtlb_08_05_inst;

    // 09.01 跨流程并发与恢复组合
    covergroup cg_dtlb_09_01 @(posedge clock);
        option.per_instance = 1;
        // 09.01.001 CONCURRENT_回填_查表_fence
        cp_09_01_001 : coverpoint (fence_valid && write_valid && |lookup_valid)
            iff (sample_enable && (fence_valid && write_valid && |lookup_valid)) {
            bins b_observed = {1'b1};
        }
    endgroup
    cg_dtlb_09_01 cg_dtlb_09_01_inst;

    initial begin
        cg_dtlb_03_01_inst = new();
        cg_dtlb_03_02_inst = new();
        cg_dtlb_diag_03_02_inst = new();
        cg_dtlb_03_03_inst = new();
        cg_dtlb_diag_03_03_inst = new();
        cg_dtlb_03_04_inst = new();
        cg_dtlb_03_05_inst = new();
        cg_dtlb_03_06_inst = new();
        cg_dtlb_06_01_inst = new();
        cg_dtlb_diag_06_01_inst = new();
        cg_dtlb_06_03_inst = new();
        cg_dtlb_06_04_inst = new();
        cg_dtlb_08_01_inst = new();
        cg_dtlb_08_02_inst = new();
        cg_dtlb_08_05_inst = new();
        cg_dtlb_09_01_inst = new();
    end

endmodule: memblock_dtlb_storage_fcov
`endif

`endif
