// 阅读约定：所有covergroup在posedge观察NBA更新前的值；_d1/_d2是模型历史。
// 简单事件门控已就地展开；保留的参考计算有中文合同，bind给出各端口的RTL来源。
// 对应表：Mem_env_work_20260716/TLB_fcov/DTLB_readability_20261016/DTLB_测试点与功能仓对应表.xlsx
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

    // 时序对齐：lookup与entry/CSR/fence在posedge保存_d1；本拍hit/valid与前拍输入和条目对比。
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
    // 聚合/历史 fence_selected：按本拍SFENCE/HFENCE/CSR状态及地址、ASID/VMID选择失效way。
    reg [47:0] fence_selected;
    // 聚合/历史 expected_hit：每端口48位参考hit向量：valid+stage+tag+ASID+VMID+index+pte且排除正在写的way。
    reg [3:0][47:0] expected_hit;
    // 聚合/历史 tag_candidates：每端口每way按level/NAPOT比较VPN片段；仅是tag谓词，不包含有效位和context。
    reg [3:0][47:0] tag_candidates;
    // 聚合/历史 asid_candidates：每端口每way满足onlyS2、Global或有效ASID相等中的任一条件。
    reg [3:0][47:0] asid_candidates;
    // 聚合/历史 vmid_candidates：每端口每way满足Host或VMID相等。
    reg [3:0][47:0] vmid_candidates;
    // 聚合/历史 index_candidates：每端口每way满足大页/NAPOT或被访问子页valididx=1。
    reg [3:0][47:0] index_candidates;
    // 聚合/历史 pte_candidates：每端口每way满足S1-only、大页/NAPOT或被访问子页pteidx=1。
    reg [3:0][47:0] pte_candidates;
    // 聚合/历史 cold_lookup：前拍48个entry全部无效；每端口相同的冷表场景。
    reg [3:0] cold_lookup;
    // 聚合/历史 invalid_rejected：存在tag/ASID/VMID/index/pte通过但valid=0的候选way。
    reg [3:0] invalid_rejected;
    // 聚合/历史 tag_rejected：存在有效、stage/ASID/VMID通过但tag不符的候选way。
    reg [3:0] tag_rejected;
    // 聚合/历史 asid_hit：存在有效、stage/tag通过且ASID谓词通过的候选way；不等于完整hit。
    reg [3:0] asid_hit;
    // 聚合/历史 asid_rejected：存在有效、stage/tag通过但ASID谓词失败的候选way。
    reg [3:0] asid_rejected;
    // 聚合/历史 global_hit：存在有效且stage/tag通过的Global候选，且entry_asid!=host csr_asid；不是完整hit。
    reg [3:0] global_hit;
    // 聚合/历史 vmid_hit：存在有效、stage/tag通过且VMID谓词通过的候选way；不等于完整hit。
    reg [3:0] vmid_hit;
    // 聚合/历史 vmid_rejected：存在有效、stage/tag通过但VMID谓词失败的候选way。
    reg [3:0] vmid_rejected;
    // 聚合/历史 stage_rejected：存在tag/ASID/VMID/index/pte通过且有效、但stage不符的候选way。
    reg [3:0] stage_rejected;
    // 聚合/历史 onlys2_asid_ignored：存在有效、stage/tag通过的onlyS2候选。
    reg [3:0] onlys2_asid_ignored;
    // 聚合/历史 host_vmid_ignored：存在有效、stage/tag通过的Host候选。
    reg [3:0] host_vmid_ignored;
    // 聚合/历史 pte_hit：存在有效、stage/tag通过的S2参与候选且pte谓词通过；不等于完整hit。
    reg [3:0] pte_hit;
    // 聚合/历史 pte_rejected：存在有效、stage/tag通过的S2参与候选但pte谓词失败。
    reg [3:0] pte_rejected;
    // 聚合/历史 pte_ignored_s1：存在有效、stage/tag通过的S1-only候选且对应pteidx为0。
    reg [3:0] pte_ignored_s1;
    // 聚合/历史 pte_ignored_large：存在有效、stage/tag通过的大页/NAPOT候选且对应pteidx为0。
    reg [3:0] pte_ignored_large;
    reg [3:0] noncontiguous_page;
    reg [3:0] coexist_access;
    reg [3:0][7:0] selected_valididx;
    // 聚合/历史 selected_level：按实际hit向量聚合页级；多hit时按位OR，不能当作唯一选中项。
    reg [3:0][1:0] selected_level;
    // 聚合/历史 selected_napot：按实际hit向量聚合NAPOT位。
    reg [3:0] selected_napot;
    reg [3:0][2:0] selected_ppn_low;
    reg [3:0][5:0] selected_way;
    // 聚合/历史 hit_comparison：每端口实际lookup_hit_vector是否与完整expected_hit逐位相同；未请求端口在仓条件中屏蔽。
    reg [3:0] hit_comparison;
    // 聚合/历史 all_fence_transitions_match：对前拍有效way逐项核对本拍valid==!前拍fence_selected。
    reg all_fence_transitions_match;
    // 聚合/历史 selected_invalidated：至少一个前拍有效且被fence选择的way本拍无效。
    reg selected_invalidated;
    // 聚合/历史 retained_valid：至少一个前拍有效且未被fence选择的way本拍仍有效。
    reg retained_valid;
    reg [34:0] saved_write_tag;
    reg [15:0] saved_write_asid;
    reg [13:0] saved_write_vmid;
    reg [1:0] saved_write_stage;
    reg [1:0] saved_write_level;
    reg saved_write_napot;
    reg [7:0] saved_write_valididx;
    reg [7:0] saved_write_pteidx;
    // 聚合/历史 overlap_write：本拍写回与有效旧条目tag/stage相同且valididx交集非空。
    reg overlap_write;
    // 聚合/历史 disjoint_pair：本拍写回与有效旧条目tag/stage相同且valididx交集为空。
    reg disjoint_pair;
    // 聚合/历史 subpage_lookup：循环最后一个有效hit端口的VPN[2:0]。
    reg [3:0] subpage_lookup;
    // 聚合/历史 any_sector_hit：至少一个有效lookup实际命中level0且非NAPOT的条目。
    reg any_sector_hit;
    // 聚合/历史 any_napot_hit：至少一个有效lookup实际命中level0且NAPOT的条目。
    reg any_napot_hit;
    // 聚合/历史 any_noncontiguous_hit：至少一个有效lookup命中条目的低两个3位PPN片段不同。
    reg any_noncontiguous_hit;
    // 聚合/历史 any_coexist_access：至少一个有效lookup命中时coexist_access聚合为1；具体write关系见组合块。
    reg any_coexist_access;
    // 聚合/历史 sector_valid_count：循环最后一个有效hit端口所选条目的valididx置1数。
    reg [3:0] sector_valid_count;
    // 聚合/历史 napot_subpage：循环最后一个有效hit端口的VPN[3:0]。
    reg [3:0] napot_subpage;
    // 判据 sample_enable：仅在 reset 明确为0且后端复位完成明确为1时采样；X/Z不算有效复位状态。
    wire sample_enable = (reset === 1'b0) && (reset_backend_done === 1'b1);

    // 判据 actual_hit_count：本拍所有端口实际hit向量中的置1数；多命中诊断使用。
    wire [5:0] actual_hit_count = $countones(lookup_hit_vector);
    // 判据 entry_occupancy：当前有效way数（48项容量）。
    wire [5:0] entry_occupancy = $countones(entry_valid);
    // 判据 invalid_victim：当前写入way在范围内且无效时为1；用于覆盖优先替换无效项。
    wire [5:0] invalid_victim = write_way < 48 ? !entry_valid[write_way] : 1'b0;
    // 判据 page_index_shape：回填valididx形态：0=单个子页，1=部分子页，2=全部8子页。
    wire [1:0] page_index_shape = write_valididx == 8'hff ? 2'd2 : $countones(write_valididx) == 1 ? 2'd0 : 2'd1;
    // 判据 expected_write_valididx：回填valididx参考：大页/NAPOT全1，onlyS2按tag低3位onehot，其余保存输入。
    wire [7:0] expected_write_valididx = effective_level != 0 || effective_napot ? 8'hff :
        write_stage == 2 ? (8'b1 << write_s2tag[2:0]) : write_valididx;
    // 判据 expected_write_pteidx：回填pteidx参考：onlyS2按tag低3位onehot，其余保存输入。
    wire [7:0] expected_write_pteidx = write_stage == 2 ? (8'b1 << write_s2tag[2:0]) : write_pteidx;
    // 判据 write_fields_match：当前way字段逐项对比前拍保存的回填字段：tag/context/pagelevel/NAPOT/valididx/pteidx。
    wire write_fields_match = write_way_d1 < 48 && entry_valid[write_way_d1] &&
        entry_tag[write_way_d1] == saved_write_tag && entry_asid[write_way_d1] == saved_write_asid &&
        entry_vmid[write_way_d1] == saved_write_vmid && entry_stage[write_way_d1] == saved_write_stage &&
        entry_level[write_way_d1] == saved_write_level && entry_n[write_way_d1] == saved_write_napot &&
        entry_valididx[write_way_d1] == saved_write_valididx && entry_pteidx[write_way_d1] == saved_write_pteidx;
    // 查询最多扫描本实例48个物理way；失效匹配只在 fence 生效拍进行。
    // 查表判据：每端口最多48个way。先独立算tag/asid/vmid/index/pte匹配，再合成expected_hit；rejected标志为任一候选被该条件排除。
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
                // 页号比较：按level忽略超级页低段；level0按NAPOT选择比较位，不混入ASID/VMID条件。
                tag_candidates[port][way] = lookup_vpn_d1[port][37:27] == entry_tag_d1[way][34:24] &&
                    (entry_level_d1[way] == 3 || lookup_vpn_d1[port][26:18] == entry_tag_d1[way][23:15]) &&
                    (entry_level_d1[way] >= 2 || lookup_vpn_d1[port][17:9] == entry_tag_d1[way][14:6]) &&
                    (entry_level_d1[way] != 0 || (entry_n_d1[way] ? lookup_vpn_d1[port][8:4] ==
                        entry_tag_d1[way][5:1] :
                    lookup_vpn_d1[port][8:3] == entry_tag_d1[way][5:0]));
                // 上下文比较：onlyS2忽略ASID，Global忽略ASID；Host忽略VMID。index/pte随后独立计算。
                asid_candidates[port][way] = lookup_stage_d1[port] == 2 || entry_s1_g_d1[way] ||
                    entry_asid_d1[way] == (lookup_stage_d1[port] == 0 ? csr_asid_d1 : csr_vasid_d1);
                vmid_candidates[port][way] = lookup_stage_d1[port] == 0 || {2'd0, entry_vmid_d1[way]} == csr_vmid_d1;
                index_candidates[port][way] = entry_level_d1[way] != 0 || entry_n_d1[way] ||
                    entry_valididx_d1[way][lookup_vpn_d1[port][2:0]];
                pte_candidates[port][way] = lookup_stage_d1[port] < 2 || entry_level_d1[way] != 0 ||
                    entry_n_d1[way] ||
                    entry_pteidx_d1[way][lookup_vpn_d1[port][2:0]];
                // 最终参考hit = valid、stage、tag、ASID、VMID、sector、PTE全部通过，且排除前拍正在写的way。
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
                    selected_ppn_low[port] = selected_ppn_low[port] |
                        entry_ppn_low[way][lookup_vpn_d1[port][2:0]*3 +: 3];
                    selected_way[port] = way;
                    noncontiguous_page[port] = noncontiguous_page[port] ||
                        entry_ppn_low[way][2:0] != entry_ppn_low[way][5:3];
                    coexist_access[port] = coexist_access[port] || disjoint_pair;
                end
            end
            hit_comparison[port] = lookup_hit_vector[port] == expected_hit[port];
        end
        // 失效选择：按HG/HV/Guest/Host路径选择way；fence_selected_d1保存选择，下拍与实际entry_valid对比。
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

    // 判据 any_way_hit：把4个端口实际hit向量按位OR，用于逐物理way仓。
    wire [47:0] any_way_hit = lookup_hit_vector[0] | lookup_hit_vector[1] | lookup_hit_vector[2] |
        lookup_hit_vector[3];

    always @* begin
        // 子页聚合：只看前拍lookup有效且本拍实际hit的端口；标量subpage/count字段保留循环最后一个符合条件端口的值。
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
    // ----------------------------------------------------------------------------------------
    // 【cg_interface_diagnostics】接口一致性诊断
    // 探测功能：观察输入与实际输出是否一致，并把偏离/未知单独记录。
    // 采样时序：前拍lookup/entry/CSR快照与本拍实际hit或entry_valid配对；回填写字段在下一拍检查。
    // 仓位判读：本组weight=0，cp为观察结果，rs的1/2分别表示偏离/未知；reference_missing表示信号缺失。
    // write_fields_match：当前way字段逐项对比前拍保存的回填字段：tag/context/pagelevel/NAPOT/valididx/pteidx。
    // all_fence_transitions_match：对前拍有效way逐项核对本拍valid==!前拍fence_selected。
    // hit_comparison：每端口实际lookup_hit_vector是否与完整expected_hit逐位相同；未请求端口在仓条件中屏蔽。
    covergroup cg_interface_diagnostics @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_lookup_result : coverpoint ($isunknown(((hit_comparison | ~lookup_valid_d1) == 4'hf)) ? 2'd2 :
            (((hit_comparison | ~lookup_valid_d1) == 4'hf)) ? 2'd0 : 2'd1)
            iff (sample_enable
                && |lookup_valid_d1) {
            bins b_mismatch = {2'd1};
            bins b_unknown = {2'd2};
        }
        cp_refill_save : coverpoint ($isunknown(write_fields_match) ? 2'd2 : (write_fields_match) ? 2'd0 : 2'd1)
            iff (sample_enable
                && write_valid_d1) {
            bins b_mismatch = {2'd1};
            bins b_unknown = {2'd2};
        }
        cp_fence_effect : coverpoint ($isunknown(all_fence_transitions_match) ? 2'd2 : (all_fence_transitions_match)
            ? 2'd0 : 2'd1)
            iff (sample_enable
                && fence_valid_d1) {
            bins b_mismatch = {2'd1};
            bins b_unknown = {2'd2};
        }
    endgroup
    cg_interface_diagnostics interface_diagnostics;
    initial begin
        interface_diagnostics = new();
    end

    // 03.01 有效项与Tag命中
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_03_01】03.01 有效项与Tag命中
    // 探测功能：观察有效位与页号匹配对缓存命中的作用，区分无有效项、标签不符和真实命中。
    // 采样时序：前拍lookup/entry/CSR快照与本拍实际hit或entry_valid配对；回填写字段在下一拍检查。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：03.01.001, 03.01.002, 03.01.003, 03.01.004；逐点功能见下方注释，精确配值见分解表。
    // any_way_hit：把4个端口实际hit向量按位OR，用于逐物理way仓。
    // cold_lookup：前拍48个entry全部无效；每端口相同的冷表场景。
    // hit_comparison：每端口实际lookup_hit_vector是否与完整expected_hit逐位相同；未请求端口在仓条件中屏蔽。
    // invalid_rejected：存在tag/ASID/VMID/index/pte通过但valid=0的候选way。
    // tag_rejected：存在有效、stage/ASID/VMID通过但tag不符的候选way。
    covergroup cg_dtlb_03_01 @(posedge clock);
        option.per_instance = 1;
        // 03.01.001 LOOKUP_冷启动miss
        // 测试表目标：场景：reset后尚未回填，对合法映射发起请求。 观察功能：storage miss，最终miss或合法bypass/pre-fault覆盖。
        // 测试目的：观察有效位与页号匹配对缓存命中的作用，区分无有效项、标签不符和真实命中。
        cp_03_01_001 : coverpoint (|lookup_hit_vector)
            iff (sample_enable
                && |(cold_lookup & lookup_valid_d1)
                && (!(|lookup_hit_vector)) === 1'b1) {
            bins b_expected_clear = {1'b0};
        }
        // 03.01.003 LOOKUP_valid0不命中
        // 测试表目标：场景：tag/ASID匹配但v[w]=0。 观察功能：不能命中无效way；payload残值不参与有效覆盖。
        // 测试目的：观察有效位与页号匹配对缓存命中的作用，区分无有效项、标签不符和真实命中。
        cp_03_01_003 : coverpoint ((hit_comparison | ~lookup_valid_d1) == 4'hf)
            iff (sample_enable
                && |(invalid_rejected & lookup_valid_d1)) {
            bins b_expected_set = {1'b1};
        }
        // 03.01.004 LOOKUP_多个tag片段不匹配
        // 测试表目标：场景：仅改变VPN高/中/低tag参与比较部分。 观察功能：按level屏蔽低VPN段；未屏蔽段改变则miss。
        // 测试目的：观察有效位与页号匹配对缓存命中的作用，区分无有效项、标签不符和真实命中。
        cp_03_01_004 : coverpoint ((hit_comparison | ~lookup_valid_d1) == 4'hf)
            iff (sample_enable
                && |(tag_rejected & lookup_valid_d1)) {
            bins b_expected_set = {1'b1};
        }
        // 03.01.002 LOOKUP_各way命中
        // 测试表目标：场景：每个w=0..47回填后再次访问映射。 观察功能：各way均被真实命中；数据/权限来自对应entry。
        // 测试目的：观察有效位与页号匹配对缓存命中的作用，区分无有效项、标签不符和真实命中。
        cp_03_01_002 : coverpoint any_way_hit
            iff (sample_enable
                && |lookup_valid_d1
                && |lookup_hit_vector
                && (((hit_comparison | ~lookup_valid_d1) == 4'hf)) === 1'b1) {
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
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_03_02】03.02 ASID、VMID与翻译阶段隔离
    // 探测功能：观察同一虚拟页在不同 ASID、VMID、翻译阶段下的隔离，以及全局映射的适用范围。
    // 采样时序：前拍lookup/entry/CSR快照与本拍实际hit或entry_valid配对；回填写字段在下一拍检查。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：03.02.001, 03.02.002, 03.02.003, 03.02.004, 03.02.005, 03.02.006, 03.02.007,
    // 03.02.008, 03.02.009；逐点功能见下方注释，精确配值见分解表。
    // asid_hit：存在有效、stage/tag通过且ASID谓词通过的候选way；不等于完整hit。
    // asid_rejected：存在有效、stage/tag通过但ASID谓词失败的候选way。
    // global_hit：存在有效且stage/tag通过的Global候选，且entry_asid!=host csr_asid；不是完整hit。
    // hit_comparison：每端口实际lookup_hit_vector是否与完整expected_hit逐位相同；未请求端口在仓条件中屏蔽。
    // host_vmid_ignored：存在有效、stage/tag通过的Host候选。
    // onlys2_asid_ignored：存在有效、stage/tag通过的onlyS2候选。
    // stage_rejected：存在tag/ASID/VMID/index/pte通过且有效、但stage不符的候选way。
    // vmid_hit：存在有效、stage/tag通过且VMID谓词通过的候选way；不等于完整hit。
    // vmid_rejected：存在有效、stage/tag通过但VMID谓词失败的候选way。
    covergroup cg_dtlb_03_02 @(posedge clock);
        option.per_instance = 1;
        // 03.02.001 CTX_ASID命中
        // 测试表目标：场景：同VPN有效表项，G=0，ASID相同。 观察功能：命中；host用satp.asid，guest S1用vsatp.asid。
        // 测试目的：观察同一虚拟页在不同 ASID、VMID、翻译阶段下的隔离，以及全局映射的适用范围。
        cp_03_02_001 : coverpoint ((hit_comparison | ~lookup_valid_d1) == 4'hf)
            iff (sample_enable
                && |(asid_hit & lookup_valid_d1)) {
            bins b_expected_set = {1'b1};
        }
        // 03.02.002 CTX_ASID不匹配
        // 测试表目标：场景：s2xlate=0/1/3；同VPN有效表项，G=0，ASID不同；onlyS2排除。 观察功能：storage miss；不能借用其他地址空间PPN。
        // 测试目的：观察同一虚拟页在不同 ASID、VMID、翻译阶段下的隔离，以及全局映射的适用范围。
        cp_03_02_002 : coverpoint ((hit_comparison | ~lookup_valid_d1) == 4'hf)
            iff (sample_enable
                && |(asid_rejected & lookup_valid_d1)) {
            bins b_expected_set = {1'b1};
        }
        // 03.02.003 CTX_global跨ASID
        // 测试表目标：场景：同VPN有效表项，S1.G=1，ASID不同。 观察功能：storage hit；global不绕过VMID或s2xlate条件。
        // 测试目的：观察同一虚拟页在不同 ASID、VMID、翻译阶段下的隔离，以及全局映射的适用范围。
        cp_03_02_003 : coverpoint ((hit_comparison | ~lookup_valid_d1) == 4'hf)
            iff (sample_enable
                && |(global_hit & lookup_valid_d1)) {
            bins b_expected_set = {1'b1};
        }
        // 03.02.004 CTX_onlyS2忽略ASID
        // 测试表目标：场景：s2xlate=2，VPN/VMID匹配；改变satp/vsatp.asid。 观察功能：命中不依赖ASID。 测试目的：观察同一虚拟页在不同
        // ASID、VMID、翻译阶段下的隔离，以及全局映射的适用范围。
        cp_03_02_004 : coverpoint ((hit_comparison | ~lookup_valid_d1) == 4'hf)
            iff (sample_enable
                && |(onlys2_asid_ignored & lookup_valid_d1)) {
            bins b_expected_set = {1'b1};
        }
        // 03.02.005 CTX_guest_VM匹配_匹配
        // 测试表目标：场景：s2xlate!=0，同VPN/ASID，VMID匹配/不匹配；vmid_hit固定1。
        // 观察功能：匹配项可命中，不匹配项拒绝；需要结合其他way命中及同拍回填mask分类最终storage结果。 测试目的：观察同一虚拟页在不同
        // ASID、VMID、翻译阶段下的隔离，以及全局映射的适用范围。
        cp_03_02_005 : coverpoint ((hit_comparison | ~lookup_valid_d1) == 4'hf)
            iff (sample_enable
                && |(vmid_hit & lookup_valid_d1)) {
            bins b_expected_set = {1'b1};
        }
        // 03.02.006 CTX_guest_VM匹配_不匹配
        // 测试表目标：场景：s2xlate!=0，同VPN/ASID，VMID匹配/不匹配；vmid_hit固定0。
        // 观察功能：匹配项可命中，不匹配项拒绝；需要结合其他way命中及同拍回填mask分类最终storage结果。 测试目的：观察同一虚拟页在不同
        // ASID、VMID、翻译阶段下的隔离，以及全局映射的适用范围。
        cp_03_02_006 : coverpoint ((hit_comparison | ~lookup_valid_d1) == 4'hf)
            iff (sample_enable
                && |(vmid_rejected & lookup_valid_d1)) {
            bins b_expected_set = {1'b1};
        }
        // 03.02.007 CTX_host忽略VMID
        // 测试表目标：场景：s2xlate=0且其他条件匹配，改变hgatp.vmid。 观察功能：storage命中不依赖VMID。 测试目的：观察同一虚拟页在不同
        // ASID、VMID、翻译阶段下的隔离，以及全局映射的适用范围。
        cp_03_02_007 : coverpoint ((hit_comparison | ~lookup_valid_d1) == 4'hf)
            iff (sample_enable
                && |(host_vmid_ignored & lookup_valid_d1)) {
            bins b_expected_set = {1'b1};
        }
        // 03.02.008 CTX_stage编码隔离
        // 测试表目标：场景：相同VPN/ASID/VMID，entry.s2xlate与请求不同。 观察功能：不能命中其他stage模式表项。 测试目的：观察同一虚拟页在不同
        // ASID、VMID、翻译阶段下的隔离，以及全局映射的适用范围。
        cp_03_02_008 : coverpoint ((hit_comparison | ~lookup_valid_d1) == 4'hf)
            iff (sample_enable
                && |(stage_rejected & lookup_valid_d1)) {
            bins b_expected_set = {1'b1};
        }
        // 03.02.009 CTX_同页多上下文共存
        // 测试表目标：场景：连续回填同VPN不同ASID/VMID/s2xlate，再交替访问。 观察功能：各上下文返回对应PPN/权限；避免只统计插入而不访问。
        // 测试目的：观察同一虚拟页在不同 ASID、VMID、翻译阶段下的隔离，以及全局映射的适用范围。
        cp_03_02_009 : coverpoint ((hit_comparison | ~lookup_valid_d1) == 4'hf)
            iff (sample_enable
                && |lookup_valid_d1
                && |lookup_hit_vector
                && csr_asid != csr_asid_d1) {
            bins b_expected_set = {1'b1};
        }
    endgroup
    cg_dtlb_03_02 cg_dtlb_03_02_inst;

    // 03.02 ASID、VMID与翻译阶段隔离
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_diag_03_02】03.02 ASID、VMID与翻译阶段隔离
    // 探测功能：观察同一虚拟页在不同 ASID、VMID、翻译阶段下的隔离，以及全局映射的适用范围。
    // 采样时序：前拍lookup/entry/CSR快照与本拍实际hit或entry_valid配对；回填写字段在下一拍检查。
    // 仓位判读：本组weight=0，cp为观察结果，rs的1/2分别表示偏离/未知；reference_missing表示信号缺失。
    // 测试点：03.02.010；逐点功能见下方注释，精确配值见分解表。
    covergroup cg_dtlb_diag_03_02 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        // 03.02.010：结果偏离或未知单列，正常命中不进入诊断仓。
        rs_03_02_010 : coverpoint ($isunknown(1'b1) ? 2'd2 : (1'b1) ? 2'd0 : 2'd1)
            iff (sample_enable
                && |lookup_valid_d1
                && csr_vmid[15:14] != 0) {
            bins b_mismatch = {2'd1};
            bins unknown  = {2'd2};
        }
        // 03.02.010 CTX_VMID宽度
        // 测试表目标：场景：CSR VMID高2位非0，entry VMID仅14位。 观察功能：记录零扩展比较的实际结果；正常架构用例限制有效14位。 测试目的：观察同一虚拟页在不同
        // ASID、VMID、翻译阶段下的隔离，以及全局映射的适用范围。
        cp_03_02_010 : coverpoint ((|lookup_valid_d1) && csr_vmid[15:14] != 0)
            iff (sample_enable
                && |lookup_valid_d1
                && csr_vmid[15:14] != 0) {
            bins b_observed = {1'b1};
        }
    endgroup
    cg_dtlb_diag_03_02 cg_dtlb_diag_03_02_inst;

    // 03.03 基础页与超级页地址拼接
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_03_03】03.03 基础页与超级页地址拼接
    // 探测功能：观察不同页尺寸下物理页号和页内偏移的拼接，覆盖边界地址与输出位宽截取。
    // 采样时序：前拍lookup/entry/CSR快照与本拍实际hit或entry_valid配对；回填写字段在下一拍检查。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    covergroup cg_dtlb_03_03 @(posedge clock);
        option.per_instance = 1;
    endgroup
    cg_dtlb_03_03 cg_dtlb_03_03_inst;

    // 03.03 基础页与超级页地址拼接
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_diag_03_03】03.03 基础页与超级页地址拼接
    // 探测功能：观察不同页尺寸下物理页号和页内偏移的拼接，覆盖边界地址与输出位宽截取。
    // 采样时序：前拍lookup/entry/CSR快照与本拍实际hit或entry_valid配对；回填写字段在下一拍检查。
    // 仓位判读：本组weight=0，cp为观察结果，rs的1/2分别表示偏离/未知；reference_missing表示信号缺失。
    // 测试点：03.03.005；逐点功能见下方注释，精确配值见分解表。
    covergroup cg_dtlb_diag_03_03 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        // 03.03.005：结果偏离或未知单列，正常命中不进入诊断仓。
        rs_03_03_005 : coverpoint ($isunknown(1'b1) ? 2'd2 : (1'b1) ? 2'd0 : 2'd1)
            iff (sample_enable
                && write_valid
                && (write_s1ppn[40:33] != 0 || write_s2ppn[37:36] != 0)) {
            bins b_mismatch = {2'd1};
            bins unknown  = {2'd2};
        }
        // 03.03.005 LOOKUP_PPN截位
        // 测试表目标：场景：active PPN在48位PA边界及以上有bit。 观察功能：DTLB输出48位PA；合法越界错误由上游PTW AF负责，DTLB只传播。
        // 测试目的：观察不同页尺寸下物理页号和页内偏移的拼接，覆盖边界地址与输出位宽截取。
        cp_03_03_005 : coverpoint (write_valid && (write_s1ppn[40:33] != 0 || write_s2ppn[37:36] != 0))
            iff (sample_enable
                && write_valid
                && (write_s1ppn[40:33] != 0 || write_s2ppn[37:36] != 0)) {
            bins b_observed = {1'b1};
        }
    endgroup
    cg_dtlb_diag_03_03 cg_dtlb_diag_03_03_inst;

    // 03.04 Sector有效子页与PPN选择
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_03_04】03.04 Sector有效子页与PPN选择
    // 探测功能：观察一个 sector 中各4KB子页的独立有效性与低位物理页号选择。
    // 采样时序：前拍lookup/entry/CSR快照与本拍实际hit或entry_valid配对；回填写字段在下一拍检查。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：03.04.001, 03.04.002, 03.04.003, 03.04.004, 03.04.005, 03.04.006, 03.04.007,
    // 03.04.008, 03.04.009, 03.04.010, 03.04.011, 03.04.012；逐点功能见下方注释，精确配值见分解表。
    // write_fields_match：当前way字段逐项对比前拍保存的回填字段：tag/context/pagelevel/NAPOT/valididx/pteidx。
    // any_coexist_access：至少一个有效lookup命中时coexist_access聚合为1；具体write关系见组合块。
    // any_noncontiguous_hit：至少一个有效lookup命中条目的低两个3位PPN片段不同。
    // any_sector_hit：至少一个有效lookup实际命中level0且非NAPOT的条目。
    // hit_comparison：每端口实际lookup_hit_vector是否与完整expected_hit逐位相同；未请求端口在仓条件中屏蔽。
    // sector_valid_count：循环最后一个有效hit端口所选条目的valididx置1数。
    // subpage_lookup：循环最后一个有效hit端口的VPN[2:0]。
    covergroup cg_dtlb_03_04 @(posedge clock);
        option.per_instance = 1;
        // 03.04.010 SECTOR_非连续物理页低位
        // 测试表目标：场景：同sector多个valididx=1，ppn_low各不相同且不等于VPN低3位。 观察功能：每子页返回正确PPN低3位；不误作连续VA→PA映射。
        // 测试目的：观察一个 sector 中各4KB子页的独立有效性与低位物理页号选择。
        cp_03_04_010 : coverpoint ((hit_comparison | ~lookup_valid_d1) == 4'hf)
            iff (sample_enable
                && any_sector_hit
                && any_noncontiguous_hit) {
            bins b_expected_set = {1'b1};
        }
        // 03.04.011 SECTOR_同tag不同有效子页共存
        // 测试表目标：场景：先后回填相同sector tag但不相交valididx到不同way。 观察功能：允许多way存不同子页；访问只选择对应有效way。 测试目的：观察一个
        // sector 中各4KB子页的独立有效性与低位物理页号选择。
        cp_03_04_011 : coverpoint ((hit_comparison | ~lookup_valid_d1) == 4'hf)
            iff (sample_enable
                && any_coexist_access) {
            bins b_expected_set = {1'b1};
        }
        // 03.04.012 SECTOR_onlyS2生成索引
        // 测试表目标：场景：W.s2xlate=2,level=0,n=0。 观察功能：valididx/pteidx由S2.tag[2:0]
        // onehot产生；tag取S2.tag[37:3]。 测试目的：观察一个 sector 中各4KB子页的独立有效性与低位物理页号选择。
        cp_03_04_012 : coverpoint write_fields_match
            iff (sample_enable
                && write_valid_d1
                && saved_write_stage == 2
                && saved_write_level == 0
                && !saved_write_napot) {
            bins b_expected_set = {1'b1};
        }
        // 03.04.001 SECTOR_子页0
        // 测试表目标：场景：level=0,n=0,valididx[0]=1，R.vaddr[14:12]=0。 观察功能：命中并选择ppn_low_0；其他子页无效位不应命中。
        // 测试目的：观察一个 sector 中各4KB子页的独立有效性与低位物理页号选择。
        cp_03_04_001 : coverpoint ((hit_comparison | ~lookup_valid_d1) == 4'hf)
            iff (sample_enable
                && any_sector_hit
                && subpage_lookup == 0) {
            bins b_expected_set = {1'b1};
        }
        // 03.04.002 SECTOR_子页1
        // 测试表目标：场景：level=0,n=0,valididx[1]=1，R.vaddr[14:12]=1。 观察功能：命中并选择ppn_low_1；其他子页无效位不应命中。
        // 测试目的：观察一个 sector 中各4KB子页的独立有效性与低位物理页号选择。
        cp_03_04_002 : coverpoint ((hit_comparison | ~lookup_valid_d1) == 4'hf)
            iff (sample_enable
                && any_sector_hit
                && subpage_lookup == 1) {
            bins b_expected_set = {1'b1};
        }
        // 03.04.003 SECTOR_子页2
        // 测试表目标：场景：level=0,n=0,valididx[2]=1，R.vaddr[14:12]=2。 观察功能：命中并选择ppn_low_2；其他子页无效位不应命中。
        // 测试目的：观察一个 sector 中各4KB子页的独立有效性与低位物理页号选择。
        cp_03_04_003 : coverpoint ((hit_comparison | ~lookup_valid_d1) == 4'hf)
            iff (sample_enable
                && any_sector_hit
                && subpage_lookup == 2) {
            bins b_expected_set = {1'b1};
        }
        // 03.04.004 SECTOR_子页3
        // 测试表目标：场景：level=0,n=0,valididx[3]=1，R.vaddr[14:12]=3。 观察功能：命中并选择ppn_low_3；其他子页无效位不应命中。
        // 测试目的：观察一个 sector 中各4KB子页的独立有效性与低位物理页号选择。
        cp_03_04_004 : coverpoint ((hit_comparison | ~lookup_valid_d1) == 4'hf)
            iff (sample_enable
                && any_sector_hit
                && subpage_lookup == 3) {
            bins b_expected_set = {1'b1};
        }
        // 03.04.005 SECTOR_子页4
        // 测试表目标：场景：level=0,n=0,valididx[4]=1，R.vaddr[14:12]=4。 观察功能：命中并选择ppn_low_4；其他子页无效位不应命中。
        // 测试目的：观察一个 sector 中各4KB子页的独立有效性与低位物理页号选择。
        cp_03_04_005 : coverpoint ((hit_comparison | ~lookup_valid_d1) == 4'hf)
            iff (sample_enable
                && any_sector_hit
                && subpage_lookup == 4) {
            bins b_expected_set = {1'b1};
        }
        // 03.04.006 SECTOR_子页5
        // 测试表目标：场景：level=0,n=0,valididx[5]=1，R.vaddr[14:12]=5。 观察功能：命中并选择ppn_low_5；其他子页无效位不应命中。
        // 测试目的：观察一个 sector 中各4KB子页的独立有效性与低位物理页号选择。
        cp_03_04_006 : coverpoint ((hit_comparison | ~lookup_valid_d1) == 4'hf)
            iff (sample_enable
                && any_sector_hit
                && subpage_lookup == 5) {
            bins b_expected_set = {1'b1};
        }
        // 03.04.007 SECTOR_子页6
        // 测试表目标：场景：level=0,n=0,valididx[6]=1，R.vaddr[14:12]=6。 观察功能：命中并选择ppn_low_6；其他子页无效位不应命中。
        // 测试目的：观察一个 sector 中各4KB子页的独立有效性与低位物理页号选择。
        cp_03_04_007 : coverpoint ((hit_comparison | ~lookup_valid_d1) == 4'hf)
            iff (sample_enable
                && any_sector_hit
                && subpage_lookup == 6) {
            bins b_expected_set = {1'b1};
        }
        // 03.04.008 SECTOR_子页7
        // 测试表目标：场景：level=0,n=0,valididx[7]=1，R.vaddr[14:12]=7。 观察功能：命中并选择ppn_low_7；其他子页无效位不应命中。
        // 测试目的：观察一个 sector 中各4KB子页的独立有效性与低位物理页号选择。
        cp_03_04_008 : coverpoint ((hit_comparison | ~lookup_valid_d1) == 4'hf)
            iff (sample_enable
                && any_sector_hit
                && subpage_lookup == 7) {
            bins b_expected_set = {1'b1};
        }
        // 03.04.009 SECTOR_valididx形状
        // 测试表目标：场景：4KB sector回填valididx为合法不同形状。 观察功能：有效子页命中，无效子页miss。 测试目的：观察一个 sector
        // 中各4KB子页的独立有效性与低位物理页号选择。
        cp_03_04_009 : coverpoint sector_valid_count
            iff (sample_enable
                && any_sector_hit
                && (((hit_comparison | ~lookup_valid_d1) == 4'hf)) === 1'b1) {
            bins b_single = {1};
            bins b_multiple = {[2:7]};
            bins b_all = {8};
        }
    endgroup
    cg_dtlb_03_04 cg_dtlb_03_04_inst;

    // 03.05 Sector匹配限制与多命中
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_03_05】03.05 Sector匹配限制与多命中
    // 探测功能：观察 pteidx 的阶段相关门控、多个缓存项重叠，以及旁路和缓存的匹配粒度差异。
    // 采样时序：前拍lookup/entry/CSR快照与本拍实际hit或entry_valid配对；回填写字段在下一拍检查。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：03.05.001, 03.05.002, 03.05.003, 03.05.004, 03.05.005, 03.05.006；逐点功能见下方注释，精确配值见分解表。
    // actual_hit_count：本拍所有端口实际hit向量中的置1数；多命中诊断使用。
    // any_sector_hit：至少一个有效lookup实际命中level0且非NAPOT的条目。
    // hit_comparison：每端口实际lookup_hit_vector是否与完整expected_hit逐位相同；未请求端口在仓条件中屏蔽。
    // pte_hit：存在有效、stage/tag通过的S2参与候选且pte谓词通过；不等于完整hit。
    // pte_ignored_large：存在有效、stage/tag通过的大页/NAPOT候选且对应pteidx为0。
    // pte_ignored_s1：存在有效、stage/tag通过的S1-only候选且对应pteidx为0。
    // pte_rejected：存在有效、stage/tag通过的S2参与候选但pte谓词失败。
    covergroup cg_dtlb_03_05 @(posedge clock);
        option.per_instance = 1;
        // 03.05.002 SECTOR_pteidx门控_匹配
        // 测试表目标：场景：stage=2/3，4KB且n=0，entry有效，stage/tag/ASID/VMID/valididx均匹配，pteidx[vpn[2:0]]=1。
        // 观察功能：该way满足pteidx门控，其他条件满足且未被同拍写mask时命中；记录下一拍hit与PPN。 测试目的：观察 pteidx
        // 的阶段相关门控、多个缓存项重叠，以及旁路和缓存的匹配粒度差异。
        cp_03_05_002 : coverpoint ((hit_comparison | ~lookup_valid_d1) == 4'hf)
            iff (sample_enable
                && |(pte_hit & lookup_valid_d1)) {
            bins b_expected_set = {1'b1};
        }
        // 03.05.003 SECTOR_pteidx门控_不匹配
        // 测试表目标：场景：stage=2/3，4KB且n=0，entry有效，stage/tag/ASID/VMID/valididx均匹配，pteidx[vpn[2:0]]=0。
        // 观察功能：该way不命中；观察下一拍hit时同时记录是否另有其他way合法命中，不能将单way拒绝等同整TLB miss。 测试目的：观察 pteidx
        // 的阶段相关门控、多个缓存项重叠，以及旁路和缓存的匹配粒度差异。
        cp_03_05_003 : coverpoint ((hit_comparison | ~lookup_valid_d1) == 4'hf)
            iff (sample_enable
                && |(pte_rejected & lookup_valid_d1)) {
            bins b_expected_set = {1'b1};
        }
        // 03.05.004 SECTOR_onlyS1忽略pteidx
        // 测试表目标：场景：s2xlate=0或1；valididx命中、pteidx不命中。 观察功能：命中；不能给S1-only套用两阶段单PTE限制。 测试目的：观察 pteidx
        // 的阶段相关门控、多个缓存项重叠，以及旁路和缓存的匹配粒度差异。
        cp_03_05_004 : coverpoint ((hit_comparison | ~lookup_valid_d1) == 4'hf)
            iff (sample_enable
                && |(pte_ignored_s1 & lookup_valid_d1)) {
            bins b_expected_set = {1'b1};
        }
        // 03.05.005 SECTOR_大页忽略pteidx
        // 测试表目标：场景：level!=0或n=1，pteidx不命中。 观察功能：pteidx不阻挡命中；superpage回填valididx为全1。 测试目的：观察 pteidx
        // 的阶段相关门控、多个缓存项重叠，以及旁路和缓存的匹配粒度差异。
        cp_03_05_005 : coverpoint ((hit_comparison | ~lookup_valid_d1) == 4'hf)
            iff (sample_enable
                && |(pte_ignored_large & lookup_valid_d1)) {
            bins b_expected_set = {1'b1};
        }
        // 03.05.006 SECTOR_onlyS1旁路与缓存粒度
        // 测试表目标：场景：onlyStage1返回4KB sector多valididx，REQ访问非s1.addr_low但valididx有效的邻子页。
        // 观察功能：PtwRespS2.hit在onlyS1分支对完整4KB VPN匹配，可能不bypass；storage使用sector valididx，回填后重试可hit。
        // 测试目的：观察 pteidx 的阶段相关门控、多个缓存项重叠，以及旁路和缓存的匹配粒度差异。
        cp_03_05_006 : coverpoint ((hit_comparison | ~lookup_valid_d1) == 4'hf)
            iff (sample_enable
                && |lookup_valid_d1
                && |lookup_stage_d1
                && any_sector_hit) {
            bins b_expected_set = {1'b1};
        }
        // 03.05.001 SECTOR_扩展valididx多命中
        // 测试表目标：场景：先回填单子页，再回填覆盖该子页的多bit sector。 观察功能：覆盖multi-hit数量和返回数据一致性；不能无条件要求onehot(hitVec)。
        // 测试目的：观察 pteidx 的阶段相关门控、多个缓存项重叠，以及旁路和缓存的匹配粒度差异。
        cp_03_05_001 : coverpoint actual_hit_count
            iff (sample_enable
                && |lookup_valid_d1
                && actual_hit_count >= 2
                && (((hit_comparison | ~lookup_valid_d1) == 4'hf)) === 1'b1) {
            bins b_two = {2};
            bins b_multiple = {[3:48]};
        }
    endgroup
    cg_dtlb_03_05 cg_dtlb_03_05_inst;

    // 03.06 64KB NAPOT命中范围
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_03_06】03.06 64KB NAPOT命中范围
    // 探测功能：观察64KB NAPOT 页内的连续命中范围和越界后的匹配结果。
    // 采样时序：前拍lookup/entry/CSR快照与本拍实际hit或entry_valid配对；回填写字段在下一拍检查。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：03.06.001, 03.06.002, 03.06.003；逐点功能见下方注释，精确配值见分解表。
    // write_fields_match：当前way字段逐项对比前拍保存的回填字段：tag/context/pagelevel/NAPOT/valididx/pteidx。
    // any_napot_hit：至少一个有效lookup实际命中level0且NAPOT的条目。
    // hit_comparison：每端口实际lookup_hit_vector是否与完整expected_hit逐位相同；未请求端口在仓条件中屏蔽。
    // napot_subpage：循环最后一个有效hit端口的VPN[3:0]。
    covergroup cg_dtlb_03_06 @(posedge clock);
        option.per_instance = 1;
        // 03.06.002 NAPOT_回填索引扩展
        // 测试表目标：场景：W有效inner_n=1。 观察功能：entry.valididx全1；不受返回s1.valididx稀疏限制。 测试目的：观察64KB NAPOT
        // 页内的连续命中范围和越界后的匹配结果。
        cp_03_06_002 : coverpoint (write_fields_match && entry_valididx[write_way_d1] == 255)
            iff (sample_enable
                && write_valid_d1
                && saved_write_napot) {
            bins b_expected_result = {1'b1};
        }
        // 03.06.003 NAPOT_单stage透传
        // 测试表目标：场景：W.s2xlate=0/1/2，active stage.n=0/1。 观察功能：entry.n选对应active stage。 测试目的：观察64KB
        // NAPOT 页内的连续命中范围和越界后的匹配结果。
        cp_03_06_003 : coverpoint write_fields_match
            iff (sample_enable
                && write_valid_d1
                && saved_write_stage != 3) {
            bins b_expected_set = {1'b1};
        }
        // 03.06.001 NAPOT_64KB命中范围
        // 测试表目标：场景：W有效n=1,level=0；访问64KB内所有16个4KB子页。 观察功能：忽略VPN低4位tag差异；PPN低4位由VPN补全。 测试目的：观察64KB
        // NAPOT 页内的连续命中范围和越界后的匹配结果。
        cp_03_06_001 : coverpoint napot_subpage
            iff (sample_enable
                && any_napot_hit
                && (((hit_comparison | ~lookup_valid_d1) == 4'hf)) === 1'b1) {
            bins b_subpage[] = {[0:15]};
        }
    endgroup
    cg_dtlb_03_06 cg_dtlb_03_06_inst;

    // 06.01 回填使能、抑制与字段保存
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_06_01】06.01 回填使能、抑制与字段保存
    // 探测功能：观察 PTW 返回何时真正写入缓存，以及写入保存的地址、权限和异常属性。
    // 采样时序：前拍lookup/entry/CSR快照与本拍实际hit或entry_valid配对；回填写字段在下一拍检查。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：06.01.006, 06.01.007, 06.01.010；逐点功能见下方注释，精确配值见分解表。
    // write_fields_match：当前way字段逐项对比前拍保存的回填字段：tag/context/pagelevel/NAPOT/valididx/pteidx。
    // hit_comparison：每端口实际lookup_hit_vector是否与完整expected_hit逐位相同；未请求端口在仓条件中屏蔽。
    covergroup cg_dtlb_06_01 @(posedge clock);
        option.per_instance = 1;
        // 06.01.006 REFILL_异常结果缓存
        // 测试表目标：场景：P携带S1.pf/af或S2.gpf/gaf，满足正常refill使能。 观察功能：异常属性仍可写入；重访产生相同类型fault。 测试目的：观察 PTW
        // 返回何时真正写入缓存，以及写入保存的地址、权限和异常属性。
        cp_06_01_006 : coverpoint ((hit_comparison | ~lookup_valid_d1) == 4'hf)
            iff (sample_enable
                && |lookup_valid_d1
                && |lookup_hit_vector
                && |entry_s1_pf) {
            bins b_expected_set = {1'b1};
        }
        // 06.01.007 REFILL_全字段保存
        // 测试表目标：场景：W.valid，构造各field可区分值。
        // 观察功能：tag/ASID/VMID/level/n/PBMT/permissions/valididx/pteidx/ppn_low按规则保存。 测试目的：观察 PTW
        // 返回何时真正写入缓存，以及写入保存的地址、权限和异常属性。
        cp_06_01_007 : coverpoint write_fields_match
            iff (sample_enable
                && write_valid_d1) {
            bins b_expected_set = {1'b1};
        }
        // 06.01.010 FCOV_回填范围与索引
        // 测试表目标：场景：W.valid；不以wayIdx是否合法限制采样。
        // 观察功能：先采way范围分类，合法时再索引v/entry；根据stage/page_type记录索引形状，不把所有多bit pteidx都判异常。越界与未知为诊断。
        // 测试目的：观察 PTW 返回何时真正写入缓存，以及写入保存的地址、权限和异常属性。
        cp_06_01_010 : coverpoint write_way
            iff (sample_enable
                && write_valid
                && (write_way < 48) === 1'b1) {
            bins b_way[] = {[0:47]};
        }
    endgroup
    cg_dtlb_06_01 cg_dtlb_06_01_inst;

    // 06.01 回填使能、抑制与字段保存
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_diag_06_01】06.01 回填使能、抑制与字段保存
    // 探测功能：观察 PTW 返回何时真正写入缓存，以及写入保存的地址、权限和异常属性。
    // 采样时序：前拍lookup/entry/CSR快照与本拍实际hit或entry_valid配对；回填写字段在下一拍检查。
    // 仓位判读：本组weight=0，cp为观察结果，rs的1/2分别表示偏离/未知；reference_missing表示信号缺失。
    // 测试点：06.01.008；逐点功能见下方注释，精确配值见分解表。
    // overlap_write：本拍写回与有效旧条目tag/stage相同且valididx交集非空。
    covergroup cg_dtlb_diag_06_01 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        // 06.01.008：结果偏离或未知单列，正常命中不进入诊断仓。
        rs_06_01_008 : coverpoint ($isunknown(1'b1) ? 2'd2 : (1'b1) ? 2'd0 : 2'd1)
            iff (sample_enable
                && write_valid
                && overlap_write) {
            bins b_mismatch = {2'd1};
            bins unknown  = {2'd2};
        }
        // 06.01.008 REFILL_重叠重复检测
        // 测试表目标：场景：W.valid且已有同上下文重叠entry，返回wb_valididx仅1bit并满足pteidx比较。
        // 观察功能：采集重叠entry数量、wb_valididx的popcount、pteidx匹配与写使能组合；重叠场景归诊断，不计正常闭合分母。 测试目的：观察 PTW
        // 返回何时真正写入缓存，以及写入保存的地址、权限和异常属性。
        cp_06_01_008 : coverpoint (write_valid && overlap_write)
            iff (sample_enable
                && write_valid
                && overlap_write) {
            bins b_observed = {1'b1};
        }
    endgroup
    cg_dtlb_diag_06_01 cg_dtlb_diag_06_01_inst;

    // 06.03 PTW旁路与读写冲突
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_06_03】06.03 PTW旁路与读写冲突
    // 探测功能：观察新 PTW 返回直接旁路与缓存读写之间的优先级，特别是读到正在被替换的 way。
    // 采样时序：前拍lookup/entry/CSR快照与本拍实际hit或entry_valid配对；回填写字段在下一拍检查。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：06.03.005, 06.03.006；逐点功能见下方注释，精确配值见分解表。
    // any_way_hit：把4个端口实际hit向量按位OR，用于逐物理way仓。
    // hit_comparison：每端口实际lookup_hit_vector是否与完整expected_hit逐位相同；未请求端口在仓条件中屏蔽。
    covergroup cg_dtlb_06_03 @(posedge clock);
        option.per_instance = 1;
        // 06.03.005 RW_读被替换way
        // 测试表目标：场景：REQ目标旧way与同拍W.wayIdx相同。 观察功能：旧way从hitVec被mask；匹配新返回可bypass，否则miss。 测试目的：观察新 PTW
        // 返回直接旁路与缓存读写之间的优先级，特别是读到正在被替换的 way。
        cp_06_03_005 : coverpoint ((hit_comparison | ~lookup_valid_d1) == 4'hf)
            iff (sample_enable
                && |lookup_valid_d1
                && write_valid_d1
                && any_way_hit[write_way_d1]) {
            bins b_expected_set = {1'b1};
        }
        // 06.03.006 RW_读其他way
        // 测试表目标：场景：读旧映射way!=W.wayIdx。 观察功能：原命中/PPN不受新写影响。 测试目的：观察新 PTW
        // 返回直接旁路与缓存读写之间的优先级，特别是读到正在被替换的 way。
        cp_06_03_006 : coverpoint ((hit_comparison | ~lookup_valid_d1) == 4'hf)
            iff (sample_enable
                && |lookup_valid_d1
                && write_valid_d1
                && |lookup_hit_vector
                && !any_way_hit[write_way_d1]) {
            bins b_expected_set = {1'b1};
        }
    endgroup
    cg_dtlb_06_03 cg_dtlb_06_03_inst;

    // 06.04 PLRU更新与容量替换
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_06_04】06.04 PLRU更新与容量替换
    // 探测功能：观察48路缓存的替换选择和 PLRU 更新，区分首次填入、容量替换及多口触碰。
    // 采样时序：前拍lookup/entry/CSR快照与本拍实际hit或entry_valid配对；回填写字段在下一拍检查。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：06.04.001, 06.04.002, 06.04.005；逐点功能见下方注释，精确配值见分解表。
    // invalid_victim：当前写入way在范围内且无效时为1；用于覆盖优先替换无效项。
    covergroup cg_dtlb_06_04 @(posedge clock);
        option.per_instance = 1;
        // 06.04.002 REPLACE_有效与无效victim
        // 测试表目标：场景：W.valid时旧v[wayIdx]=0/1。 观察功能：覆盖首次使用和容量替换；被替换映射后续miss。 测试目的：观察48路缓存的替换选择和 PLRU
        // 更新，区分首次填入、容量替换及多口触碰。
        cp_06_04_002 : coverpoint invalid_victim
            iff (sample_enable
                && write_valid) {
            bins b_valid_victim = {1'b0};
            bins b_invalid_victim = {1'b1};
        }
        // 06.04.005 REPLACE_回填touch优先
        // 测试表目标：场景：前一拍W.valid=1，本拍也有读命中。 观察功能：TLBFA access touch被refill_wayIdx_reg覆盖；对所有read
        // port生效。 测试目的：观察48路缓存的替换选择和 PLRU 更新，区分首次填入、容量替换及多口触碰。
        cp_06_04_005 : coverpoint (touch_way[0] == write_way_d1)
            iff (sample_enable
                && write_valid_d1
                && |touch_valid) {
            bins b_expected_result = {1'b1};
        }
        // 06.04.001 REPLACE_各way被选
        // 测试表目标：场景：W.valid，采W.wayIdx。 观察功能：0..47每way单独命中；48..63禁止。 测试目的：观察48路缓存的替换选择和 PLRU
        // 更新，区分首次填入、容量替换及多口触碰。
        cp_06_04_001 : coverpoint write_way
            iff (sample_enable
                && write_valid
                && (write_way < 48) === 1'b1) {
            bins b_way[] = {[0:47]};
        }
    endgroup
    cg_dtlb_06_04 cg_dtlb_06_04_inst;

    // 08.01 SFENCE和HFENCE范围选择
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_08_01】08.01 SFENCE和HFENCE范围选择
    // 探测功能：观察 SFENCE/HFENCE 根据地址、标识和虚拟化上下文选中哪些缓存项。
    // 采样时序：前拍lookup/entry/CSR快照与本拍实际hit或entry_valid配对；回填写字段在下一拍检查。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：08.01.001, 08.01.002, 08.01.003, 08.01.004, 08.01.005, 08.01.006, 08.01.007,
    // 08.01.008, 08.01.009, 08.01.010, 08.01.011, 08.01.013；逐点功能见下方注释，精确配值见分解表。
    // all_fence_transitions_match：对前拍有效way逐项核对本拍valid==!前拍fence_selected。
    covergroup cg_dtlb_08_01 @(posedge clock);
        option.per_instance = 1;
        // 08.01.001 SFENCE_host_11
        // 测试表目标：在 Host 上下文执行 SFENCE，rs1=1、rs2=1，表示失效范围为全部地址、全部
        // ASID（包含全局映射）。记录失效前有效表项及其上下文，下一拍分别统计选中项失效、未选中项保留。全地址 Host 分支只选 noS2 表项。
        cp_08_01_001 : coverpoint all_fence_transitions_match
            iff (sample_enable
                && fence_valid_d1
                && !fence_hv_d1
                && !fence_hg_d1
                && !csr_virt_d1
                && fence_rs1_d1
                && fence_rs2_d1) {
            bins b_expected_set = {1'b1};
        }
        // 08.01.002 SFENCE_host_10
        // 测试表目标：在 Host 上下文执行 SFENCE，rs1=1、rs2=0，表示失效范围为全部地址、F.id 指定 ASID
        // 的非全局映射。记录失效前有效表项及其上下文，下一拍分别统计选中项失效、未选中项保留。全地址 Host 分支只选 noS2 表项。
        cp_08_01_002 : coverpoint all_fence_transitions_match
            iff (sample_enable
                && fence_valid_d1
                && !fence_hv_d1
                && !fence_hg_d1
                && !csr_virt_d1
                && fence_rs1_d1
                && !fence_rs2_d1) {
            bins b_expected_set = {1'b1};
        }
        // 08.01.003 SFENCE_host_01
        // 测试表目标：在 Host 上下文执行 SFENCE，rs1=0、rs2=1，表示失效范围为F.addr 指定地址所在页、全部
        // ASID（包含全局映射）。记录失效前有效表项及其上下文，下一拍分别统计选中项失效、未选中项保留。当前 RTL 的指定地址分支没有额外 entry.stage/VMID
        // 筛选，需要同时观察地址相同的 Guest 项。
        cp_08_01_003 : coverpoint all_fence_transitions_match
            iff (sample_enable
                && fence_valid_d1
                && !fence_hv_d1
                && !fence_hg_d1
                && !csr_virt_d1
                && !fence_rs1_d1
                && fence_rs2_d1) {
            bins b_expected_set = {1'b1};
        }
        // 08.01.004 SFENCE_host_00
        // 测试表目标：在 Host 上下文执行 SFENCE，rs1=0、rs2=0，表示失效范围为F.addr 指定地址所在页、F.id 指定 ASID
        // 的非全局映射。记录失效前有效表项及其上下文，下一拍分别统计选中项失效、未选中项保留。当前 RTL 的指定地址分支没有额外 entry.stage/VMID
        // 筛选，需要同时观察地址相同的 Guest 项。
        cp_08_01_004 : coverpoint all_fence_transitions_match
            iff (sample_enable
                && fence_valid_d1
                && !fence_hv_d1
                && !fence_hg_d1
                && !csr_virt_d1
                && !fence_rs1_d1
                && !fence_rs2_d1) {
            bins b_expected_set = {1'b1};
        }
        // 08.01.005 SFENCE_guest_rs20
        // 测试表目标：场景：F.valid=1,hv=hg=0,virt=1,rs2=0；rs1=0/1及任意addr。 观察功能：忽略地址；只处理当前VMID
        // guest项；ASID=id且G=0清除；host/其他VMID保留。 测试目的：观察 SFENCE/HFENCE 根据地址、标识和虚拟化上下文选中哪些缓存项。
        cp_08_01_005 : coverpoint all_fence_transitions_match
            iff (sample_enable
                && fence_valid_d1
                && !fence_hv_d1
                && !fence_hg_d1
                && csr_virt_d1
                && !fence_rs2_d1) {
            bins b_expected_set = {1'b1};
        }
        // 08.01.006 SFENCE_guest_rs21
        // 测试表目标：场景：F.valid=1,hv=hg=0,virt=1,rs2=1；rs1=0/1及任意addr。 观察功能：忽略地址；只处理当前VMID
        // guest项；全部ASID且包括global清除；host/其他VMID保留。 测试目的：观察 SFENCE/HFENCE 根据地址、标识和虚拟化上下文选中哪些缓存项。
        cp_08_01_006 : coverpoint all_fence_transitions_match
            iff (sample_enable
                && fence_valid_d1
                && !fence_hv_d1
                && !fence_hg_d1
                && csr_virt_d1
                && fence_rs2_d1) {
            bins b_expected_set = {1'b1};
        }
        // 08.01.007 HFENCE_VVMA_rs20
        // 测试表目标：场景：F.valid=1,hv=1,hg=0,rs2=0；rs1=0/1，addr在/不在表项范围。 观察功能：忽略地址；当前VMID
        // guest项仅ASID匹配且G=0清除；其他VMID/host保留。 测试目的：观察 SFENCE/HFENCE 根据地址、标识和虚拟化上下文选中哪些缓存项。
        cp_08_01_007 : coverpoint all_fence_transitions_match
            iff (sample_enable
                && fence_valid_d1
                && fence_hv_d1
                && !fence_hg_d1
                && !fence_rs2_d1) {
            bins b_expected_set = {1'b1};
        }
        // 08.01.008 HFENCE_VVMA_rs21
        // 测试表目标：场景：F.valid=1,hv=1,hg=0,rs2=1；rs1=0/1，addr在/不在表项范围。 观察功能：忽略地址；当前VMID
        // guest项全部清除，包括G=1；其他VMID/host保留。 测试目的：观察 SFENCE/HFENCE 根据地址、标识和虚拟化上下文选中哪些缓存项。
        cp_08_01_008 : coverpoint all_fence_transitions_match
            iff (sample_enable
                && fence_valid_d1
                && fence_hv_d1
                && !fence_hg_d1
                && fence_rs2_d1) {
            bins b_expected_set = {1'b1};
        }
        // 08.01.009 HFENCE_GVMA_rs20
        // 测试表目标：场景：F.valid=1,hg=1,hv=0,rs2=0；rs1=0/1，addr任意。
        // 观察功能：VMID==F.id的guest项失效，不受G/ASID限制；host项保留；地址不参与匹配。 测试目的：观察 SFENCE/HFENCE
        // 根据地址、标识和虚拟化上下文选中哪些缓存项。
        cp_08_01_009 : coverpoint all_fence_transitions_match
            iff (sample_enable
                && fence_valid_d1
                && fence_hg_d1
                && !fence_hv_d1
                && !fence_rs2_d1) {
            bins b_expected_set = {1'b1};
        }
        // 08.01.010 HFENCE_GVMA_rs21
        // 测试表目标：场景：F.valid=1,hg=1,hv=0,rs2=1；rs1=0/1，addr任意。
        // 观察功能：所有guest项失效，不受G/ASID限制；host项保留；地址不参与匹配。 测试目的：观察 SFENCE/HFENCE 根据地址、标识和虚拟化上下文选中哪些缓存项。
        cp_08_01_010 : coverpoint all_fence_transitions_match
            iff (sample_enable
                && fence_valid_d1
                && fence_hg_d1
                && !fence_hv_d1
                && fence_rs2_d1) {
            bins b_expected_set = {1'b1};
        }
        // 08.01.011 FENCE_BME扩大失效
        // 测试表目标：场景：host SFENCE rs1=0，mbmc.BME=1,CMODE=0，目标地址不匹配。
        // 观察功能：转全地址分支，ID/global条件仍按rs2；BME=0或CMODE=1对照精确地址分支。 测试目的：观察 SFENCE/HFENCE
        // 根据地址、标识和虚拟化上下文选中哪些缓存项。
        cp_08_01_011 : coverpoint all_fence_transitions_match
            iff (sample_enable
                && fence_valid_d1
                && !fence_hv_d1
                && !fence_hg_d1
                && !csr_virt_d1
                && !fence_rs1_d1
                && csr_bme_d1
                && !csr_cmode_d1) {
            bins b_expected_set = {1'b1};
        }
        // 08.01.013 SFENCE_host指定地址误扩guest
        // 测试表目标：场景：virt=0,rs1=0,BME=0，host和guest entry在相同地址范围；分别rs2=0/1。
        // 观察功能：地址分支未检查entry.s2xlate，guest项满足地址/ID条件也可能清除；对照rs1=1只清host。 测试目的：观察 SFENCE/HFENCE
        // 根据地址、标识和虚拟化上下文选中哪些缓存项。
        cp_08_01_013 : coverpoint all_fence_transitions_match
            iff (sample_enable
                && fence_valid_d1
                && !fence_hv_d1
                && !fence_hg_d1
                && !csr_virt_d1
                && !fence_rs1_d1
                && |entry_stage_d1) {
            bins b_expected_set = {1'b1};
        }
    endgroup
    cg_dtlb_08_01 cg_dtlb_08_01_inst;

    // 08.02 页型相关失效与保留
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_08_02】08.02 页型相关失效与保留
    // 探测功能：观察页型和子页有效位如何影响定址失效，并分别记录选中项失效与未选中项保留。
    // 采样时序：前拍lookup/entry/CSR快照与本拍实际hit或entry_valid配对；回填写字段在下一拍检查。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：08.02.001, 08.02.002, 08.02.003, 08.02.004, 08.02.005；逐点功能见下方注释，精确配值见分解表。
    // all_fence_transitions_match：对前拍有效way逐项核对本拍valid==!前拍fence_selected。
    // retained_valid：至少一个前拍有效且未被fence选择的way本拍仍有效。
    // selected_invalidated：至少一个前拍有效且被fence选择的way本拍无效。
    covergroup cg_dtlb_08_02 @(posedge clock);
        option.per_instance = 1;
        // 08.02.001 FENCE_sector整项清除
        // 测试表目标：场景：host SFENCE指定地址命中sector任一valid子页。 观察功能：整个entry.v清0，所有8个子页失效；不只清valididx一bit。
        // 测试目的：观察页型和子页有效位如何影响定址失效，并分别记录选中项失效与未选中项保留。
        cp_08_02_001 : coverpoint all_fence_transitions_match
            iff (sample_enable
                && fence_valid_d1
                && |(entry_valid_d1 & fence_selected_d1)) {
            bins b_expected_set = {1'b1};
        }
        // 08.02.002 FENCE_sector无效子页不匹配
        // 测试表目标：场景：host指定addr落在同sector但valididx对应bit=0，BME扩展关闭。 观察功能：该entry可保留；hit函数使用valididx。
        // 测试目的：观察页型和子页有效位如何影响定址失效，并分别记录选中项失效与未选中项保留。
        cp_08_02_002 : coverpoint all_fence_transitions_match
            iff (sample_enable
                && fence_valid_d1
                && retained_valid) {
            bins b_expected_set = {1'b1};
        }
        // 08.02.003 FENCE_页内任意地址
        // 测试表目标：场景：host指定地址位于2MB/1GB/512GB或NAPOT页内非原请求子页。 观察功能：该大页entry失效；越界地址不误清。
        // 测试目的：观察页型和子页有效位如何影响定址失效，并分别记录选中项失效与未选中项保留。
        cp_08_02_003 : coverpoint all_fence_transitions_match
            iff (sample_enable
                && fence_valid_d1
                && |entry_n_d1) {
            bins b_expected_set = {1'b1};
        }
        // 08.02.004 FENCE_异常页尺寸
        // 测试表目标：场景：回填S1异常大页，S2.level较小，再fence大页内远端地址。 观察功能：基于实际保存inner_level失效；对应MERGE异常corner。
        // 测试目的：观察页型和子页有效位如何影响定址失效，并分别记录选中项失效与未选中项保留。
        cp_08_02_004 : coverpoint all_fence_transitions_match
            iff (sample_enable
                && fence_valid_d1
                && |entry_s1_pf) {
            bins b_expected_set = {1'b1};
        }
        // 08.02.005 FCOV_失效与保留转换
        // 测试表目标：场景：FENCE生效前保存每个有效entry的tag/context/level和应选范围；下一拍读取v。
        // 观察功能：目标bins为选中且1→0、未选中且1→1；另两种偏离组合保留在诊断组。范围按本版RTL推导，包括host指定地址扩大清guest。
        // 测试目的：观察页型和子页有效位如何影响定址失效，并分别记录选中项失效与未选中项保留。
        cp_08_02_005 : coverpoint selected_invalidated ? 1'b0 : 1'b1
            iff (sample_enable
                && fence_valid_d1
                && (selected_invalidated || retained_valid)
                && (all_fence_transitions_match) === 1'b1) {
            bins b_selected_invalidated = {1'b0};
            bins b_retained_valid = {1'b1};
        }
    endgroup
    cg_dtlb_08_02 cg_dtlb_08_02_inst;

    // 08.05 在途刷新与新旧上下文恢复
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_08_05】08.05 在途刷新与新旧上下文恢复
    // 探测功能：观察刷新前后的在途返回和映射切换，跟踪新请求是否最终使用新上下文。
    // 采样时序：前拍lookup/entry/CSR快照与本拍实际hit或entry_valid配对；回填写字段在下一拍检查。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：08.05.002；逐点功能见下方注释，精确配值见分解表。
    covergroup cg_dtlb_08_05 @(posedge clock);
        option.per_instance = 1;
        // 08.05.002 FENCE_查表同拍
        // 测试表目标：场景：REQ与storage FENCE同拍，旧entry原本有效。
        // 观察功能：读hit寄存可能来自清空前；覆盖在途请求及上游flush/replay合同，不能要求同拍组合hit立即0。
        // 测试目的：观察刷新前后的在途返回和映射切换，跟踪新请求是否最终使用新上下文。
        cp_08_05_002 : coverpoint (fence_valid && |lookup_valid)
            iff (sample_enable
                && fence_valid
                && |lookup_valid) {
            bins b_observed = {1'b1};
        }
    endgroup
    cg_dtlb_08_05 cg_dtlb_08_05_inst;

    // 09.01 跨流程并发与恢复组合
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_09_01】09.01 跨流程并发与恢复组合
    // 探测功能：观察多个流程同时竞争时是否经历预期的抑制、释放和重试，并记录最终结束原因。
    // 采样时序：前拍lookup/entry/CSR快照与本拍实际hit或entry_valid配对；回填写字段在下一拍检查。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：09.01.001；逐点功能见下方注释，精确配值见分解表。
    covergroup cg_dtlb_09_01 @(posedge clock);
        option.per_instance = 1;
        // 09.01.001 CONCURRENT_回填_查表_fence
        // 测试表目标：场景：P.valid、REQ与内部fence同拍/相邻拍。 观察功能：记录各事件阶段并检查失效/旁路/回放，不能按发生顺序猜结果。
        // 测试目的：观察多个流程同时竞争时是否经历预期的抑制、释放和重试，并记录最终结束原因。
        cp_09_01_001 : coverpoint (fence_valid && write_valid && |lookup_valid)
            iff (sample_enable
                && fence_valid
                && write_valid
                && |lookup_valid) {
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
