// 阅读约定：所有covergroup在posedge观察NBA更新前的值；_d1/_d2是模型历史。
// 简单事件门控已就地展开；保留的参考计算有中文合同，bind给出各端口的RTL来源。
// 对应表：Mem_env_work_20260716/TLB_fcov/DTLB_readability_20261016/DTLB_测试点与功能仓对应表.xlsx
//=========================================================
// memblock_dtlb_filter_fcov：DTLB V2 只读功能覆盖率。
// 普通信号端口、时钟事件采样、逐特性直接建仓；没有结构体库、function 或采样分发器。
// 时延与长度采用范围仓；物理 way/slot/子页采用逐项仓。
//=========================================================
`ifndef MEMBLOCK_DTLB_FILTER_FCOV__SV
`define MEMBLOCK_DTLB_FILTER_FCOV__SV

`ifdef MEMBLOCK_UT_FCOV
module memblock_dtlb_filter_fcov #(
    parameter integer GROUP_ID = 0,
    parameter integer PORT_NUM = 4,
    parameter integer SLOT_NUM = 16
) (
    input wire clock,
    input wire reset,
    input wire reset_backend_done,
    input wire [15:0] slot_valid,
    input wire [15:0] slot_sent,
    input wire [15:0][37:0] slot_vpn,
    input wire [15:0][1:0] slot_stage,
    input wire [15:0] slot_getgpa,
    input wire [15:0] slot_rmatch,
    input wire [3:0] request_valid,
    input wire [3:0][37:0] request_vpn,
    input wire [3:0][1:0] request_stage,
    input wire [3:0] request_getgpa,
    input wire [3:0] request_enq,
    input wire [3:0][3:0] request_idx,
    input wire [3:0] request_can,
    input wire [3:0][15:0] request_oldmatch,
    input wire [3:0] request_rmatch,
    input wire filter_flush,
    input wire filter_resp,
    input wire filter_issue,
    input wire filter_ready,
    input wire [3:0] filter_index,
    input wire filter_refill,
    input wire filter_getgpa
);

    // 背压/历史：_d1保存前拍slot与请求；stall_length在issue且!ready时累加，握手结束后分档。
    reg [31:0] stall_length;
    // 聚合/历史 completed_stall_length：背压长度快照，用于握手恢复时按连续背压周期数分档。
    reg [31:0] completed_stall_length;
    reg issue_stalled_d1;
    reg [15:0] slot_valid_d1;
    reg [15:0] slot_sent_d1;
    reg [15:0] slot_rmatch_d1;
    reg filter_flush_d1;
    reg filter_resp_d1;
    reg issue_fire_d1;
    reg [3:0] issue_index_d1;
    reg [3:0] request_enq_d1;
    reg [3:0][3:0] request_idx_d1;
    reg [3:0][37:0] request_vpn_d1;
    reg [3:0][1:0] request_stage_d1;
    // 聚合/历史 highest_unsent：扫描有效且未sent的slot，保留最高编号；未找到时保持0。
    reg [3:0] highest_unsent;
    // 聚合/历史 highest_match：扫描有效且匹配返回的slot，保留最高编号；未找到时保持0。
    reg [3:0] highest_match;
    // 聚合/历史 matched_gpa_zero：返回匹配的有效slot中至少一项getGpa=0。
    reg matched_gpa_zero;
    // 聚合/历史 matched_gpa_one：返回匹配的有效slot中至少一项getGpa=1。
    reg matched_gpa_one;
    // 聚合/历史 allocation_matches：全称关系：本拍同key新请求的后口不重复enq且idx等于前口；它本身不核对旧slot合并的idx/内容。
    reg allocation_matches;
    // 聚合/历史 same_key_result_matches：RTL对同VPN/stage后口复用前口enqidx且不再次enq的结果。
    reg same_key_result_matches;
    // 聚合/历史 same_key_new_result_matches：无旧match的同key新请求合并结果。
    reg same_key_new_result_matches;
    // 聚合/历史 old_merge_result_matches：旧match请求不新分配，并返回RTL选出的最高旧match index。
    reg old_merge_result_matches;
    // 聚合/历史 different_stage_result_matches：同VPN不同stage不共享旧match或新分配index。
    reg different_stage_result_matches;
    // 聚合/历史 parallel_distinct_result_matches：不同key并行请求不会复用同一个新分配index。
    reg parallel_distinct_result_matches;
    // 聚合/历史 partition_result_matches：拍前分区满时不产生enqvalid，response释放不改变本拍canenq。
    reg partition_result_matches;
    // 聚合/历史 request_return_result_matches：PTW返回已匹配请求时不再次enq。
    reg request_return_result_matches;
    reg [3:0][3:0] expected_old_idx;
    // 聚合/历史 enqueue_state_matches：前拍每个实际enq请求在本拍slot中具有有效位及相同VPN/stage；前拍flush时跳过。
    reg enqueue_state_matches;
    // 聚合/历史 response_release_matches：前拍返回匹配的有效slot本拍均清valid；前拍flush时所有slot均清valid。
    reg response_release_matches;
    // 聚合/历史 empty_allocation：存在有效请求，所在分区可分配，无旧match且不命中本拍返回。
    reg empty_allocation;
    // 聚合/历史 partition_full：存在有效请求，所在分区不可分配且没有旧match；不同端口的存在性不能当作同一请求。
    reg partition_full;
    // 聚合/历史 partition_release：存在有效请求，其分区不可分配且本拍返回命中该分区slot。
    reg partition_release;
    // 聚合/历史 old_merge：存在有效请求命中旧slot的request_oldmatch；场景标志不等于已验证实际分配结果。
    reg old_merge;
    // 聚合/历史 different_stage：存在有效请求与有效旧slot的VPN相同而stage不同。
    reg different_stage;
    // 聚合/历史 new_merge：两个本拍有效请求的VPN和stage相同，后口与较早端口形成新请求合并场景。
    reg new_merge;
    // 聚合/历史 low_full_high_free：同key新请求中存在较低口不可分配、较高口可分配的组合。
    reg low_full_high_free;
    // 聚合/历史 low_free_high_full：同key新请求中存在较低口可分配、较高口不可分配的组合。
    reg low_free_high_full;
    // 聚合/历史 mixed_sent_return：本拍匹配多个有效slot，且同时包含sent=0和sent=1项。
    reg mixed_sent_return;
    // 聚合/历史 ignored_invalid_sent：存在valid=0而sent=1的slot，用于验证无效项不参与发送。
    reg ignored_invalid_sent;
    // 聚合/历史 request_return_match：存在有效请求的request_rmatch为1，即本拍请求命中PTW返回。
    reg request_return_match;
    // 聚合/历史 return_without_old_entry：有效请求命中本拍返回，且没有有效旧slot命中该返回。
    reg return_without_old_entry;
    // 聚合/历史 gpa_merge_without_upgrade：getGpa=1的新请求命中getGpa=0的旧slot；只描述这个触发场景。
    reg gpa_merge_without_upgrade;
    // 聚合/历史 sent_issue_repeat：本拍issue&&ready发射的index指向有效且已经sent的slot。
    reg sent_issue_repeat;
    // 聚合/历史 full_seen：复位后曾观察到slot满的历史标志。
    reg full_seen;
    // 聚合/历史 release_seen：复位后曾观察到返回释放的历史标志。
    reg release_seen;
    // 聚合/历史 allocated_slot：本拍request_enq为1的最后一个端口给出的idx；用于slot编号仓。
    reg [3:0] allocated_slot;
    // 判据 sample_enable：仅在 reset 明确为0且后端复位完成明确为1时采样；X/Z不算有效复位状态。
    wire sample_enable = (reset === 1'b0) && (reset_backend_done === 1'b1);
    // 判据 occupancy：当前有效slot数量，使用slot_valid计数；不等于已发出数量。
    wire [4:0] occupancy = $countones(slot_valid);
    // 判据 sent_occupancy：slot有效且已发送的数量，使用slot_valid & slot_sent计数。
    wire [4:0] sent_occupancy = $countones(slot_valid & slot_sent);
    // 判据 matching_slots：本拍PTW返回匹配的有效slot数量。
    wire [4:0] matching_slots = $countones(slot_rmatch & slot_valid);
    // 判据 pending_to_issue：当前有效且尚未发送的slot数量。
    wire [4:0] pending_to_issue = $countones(slot_valid & ~slot_sent);

    // 判据 capacity_class：占用分类：0=空，1=单项，2=中间，3=满。
    wire [1:0] capacity_class = occupancy == 0 ? 2'd0 : occupancy == 1 ? 2'd1 : occupancy == SLOT_NUM ? 2'd3 : 2'd2;
    // 当前生成 RTL 未暴露 debugHead/rob_head_miss_in_tlb，也没有可观测的累计 inflight_counter；
    // PTWFilterEntry 内部仅有 _inflight_counter_T 单拍发射条件。两个点保留 weight=0 的 reference_missing 证据，不伪造功能命中。
    // 判据 rob_head_miss_observable：常量0：生成RTL未暴露ROB头miss指示；reference_missing只记录参考缺失。
    wire rob_head_miss_observable = 1'b0;
    // 判据 inflight_counter_observable：常量0：生成RTL没有累计计数器端口，不能观测真实回绕。
    wire inflight_counter_observable = 1'b0;
    // 分配和内容匹配只遍历本实例的16/8个物理slot，状态数组不随事务总数增长。
    // 固定窗口观测：遍历SLOT_NUM个slot与PORT_NUM个请求。每个聚合标志在块首清零/置真，只表示本实例当前拍的存在性或全称检查。
    always @* begin
        highest_unsent = 0;
        highest_match = 0;
        matched_gpa_zero = 0;
        matched_gpa_one = 0;
        allocation_matches = 1;
        same_key_result_matches = 1;
        same_key_new_result_matches = 1;
        old_merge_result_matches = 1;
        different_stage_result_matches = 1;
        parallel_distinct_result_matches = 1;
        partition_result_matches = 1;
        request_return_result_matches = 1;
        expected_old_idx = '0;
        enqueue_state_matches = 1;
        response_release_matches = 1;
        empty_allocation = 0;
        partition_full = 0;
        partition_release = 0;
        old_merge = 0;
        different_stage = 0;
        new_merge = 0;
        low_full_high_free = 0;
        low_free_high_full = 0;
        mixed_sent_return = 0;
        ignored_invalid_sent = 0;
        request_return_match = 0;
        return_without_old_entry = 0;
        gpa_merge_without_upgrade = 0;
        sent_issue_repeat = 0;
        // slot判读：highest_*保留最高编号匹配项；response_release_matches对前拍返回匹配项逐项检查本拍释放。
        for (integer slot = 0; slot < SLOT_NUM; slot = slot + 1) begin
            if (slot_valid[slot] && !slot_sent[slot]) highest_unsent = slot;
            if (slot_valid[slot] && slot_rmatch[slot]) begin
                highest_match = slot;
                matched_gpa_zero = matched_gpa_zero || !slot_getgpa[slot];
                matched_gpa_one = matched_gpa_one || slot_getgpa[slot];
            end
            if (!slot_valid[slot] && slot_sent[slot]) ignored_invalid_sent = 1;
            if ((filter_issue && filter_ready) && filter_index == slot && slot_valid[slot] &&
                slot_sent[slot]) sent_issue_repeat = 1;
            if (filter_resp_d1 && slot_valid_d1[slot] && slot_rmatch_d1[slot])
                response_release_matches = response_release_matches && !slot_valid[slot];
            if (filter_flush_d1) response_release_matches = response_release_matches && !slot_valid[slot];
        end
        mixed_sent_return = matching_slots > 1 && $countones(slot_valid & slot_rmatch & slot_sent) > 0 &&
            $countones(slot_valid & slot_rmatch & ~slot_sent) > 0;
        // 请求判读：先从RTL导出的16位oldmatch向量恢复最高旧entry index，再分别核对旧合并、同key合并、不同stage隔离和分区容量。
        for (integer port = 0; port < PORT_NUM; port = port + 1) begin
            if (request_valid[port]) begin
                empty_allocation = empty_allocation || (request_can[port] && request_oldmatch[port] == 0 &&
                    !request_rmatch[port]);
                partition_full = partition_full || (!request_can[port] && request_oldmatch[port] == 0);
                partition_release = partition_release || (!request_can[port] && |slot_rmatch[port*4 +: 4]);
                old_merge = old_merge || |request_oldmatch[port];
                for (integer old_slot = 0; old_slot < SLOT_NUM; old_slot = old_slot + 1) begin
                    if (request_oldmatch[port][old_slot]) begin
                        expected_old_idx[port] = old_slot;
                    end
                end
                if (|request_oldmatch[port])
                    old_merge_result_matches = old_merge_result_matches &&
                        !request_enq[port] && request_idx[port] == expected_old_idx[port];
                if ((request_oldmatch[port] == 0) && !request_can[port])
                    partition_result_matches = partition_result_matches && !request_enq[port];
                if (request_rmatch[port])
                    request_return_result_matches = request_return_result_matches && !request_enq[port];
                request_return_match = request_return_match || request_rmatch[port];
                return_without_old_entry = return_without_old_entry || (request_rmatch[port] && matching_slots == 0);
                for (integer slot = 0; slot < SLOT_NUM; slot = slot + 1) begin
                    different_stage = different_stage || (slot_valid[slot] && slot_vpn[slot] == request_vpn[port] &&
                        slot_stage[slot] != request_stage[port]);
                    gpa_merge_without_upgrade = gpa_merge_without_upgrade ||
                        (request_getgpa[port] && slot_valid[slot] && !slot_getgpa[slot] &&
                            request_oldmatch[port][slot]);
                end
                for (integer earlier = 0; earlier < port; earlier = earlier + 1) begin
                    if (request_valid[earlier] && request_vpn[earlier] == request_vpn[port] &&
                        request_stage[earlier] == request_stage[port]) begin
                        new_merge = 1;
                        same_key_result_matches = same_key_result_matches && !request_enq[port] &&
                            request_idx[port] == request_idx[earlier];
                        if (request_oldmatch[earlier] == 0 && request_oldmatch[port] == 0)
                            same_key_new_result_matches = same_key_new_result_matches &&
                                !request_enq[port] && request_idx[port] == request_idx[earlier];
                        allocation_matches = allocation_matches && !request_enq[port] &&
                            request_idx[port] == request_idx[earlier];
                        low_full_high_free = low_full_high_free || (!request_can[earlier] && request_can[port]);
                        low_free_high_full = low_free_high_full || (request_can[earlier] && !request_can[port]);
                    end else if (request_valid[earlier] &&
                        (request_vpn[earlier] != request_vpn[port] ||
                         request_stage[earlier] != request_stage[port])) begin
                        parallel_distinct_result_matches = parallel_distinct_result_matches &&
                            !(request_enq[earlier] && request_enq[port] &&
                              request_idx[earlier] == request_idx[port]);
                        if (request_vpn[earlier] == request_vpn[port] &&
                            request_stage[earlier] != request_stage[port])
                            different_stage_result_matches = different_stage_result_matches &&
                                ((request_oldmatch[earlier] & request_oldmatch[port]) == 0) &&
                                !(request_enq[earlier] && request_enq[port] &&
                                  request_idx[earlier] == request_idx[port]);
                    end
                end
            end
            if (request_enq_d1[port] && !filter_flush_d1 && request_idx_d1[port] < SLOT_NUM)
                enqueue_state_matches = enqueue_state_matches && slot_valid[request_idx_d1[port]] &&
                    slot_vpn[request_idx_d1[port]] == request_vpn_d1[port] &&
                    slot_stage[request_idx_d1[port]] == request_stage_d1[port];
        end
    end
    always @(posedge clock) begin
        if (reset) begin
            stall_length <= 0;
            completed_stall_length <= 0;
            issue_stalled_d1 <= 0;
            slot_valid_d1 <= 0;
            slot_sent_d1 <= 0;
            slot_rmatch_d1 <= 0;
            filter_flush_d1 <= 0;
            filter_resp_d1 <= 0;
            issue_fire_d1 <= 0;
            issue_index_d1 <= 0;
            request_enq_d1 <= 0;
            request_idx_d1 <= 0;
            request_vpn_d1 <= 0;
            request_stage_d1 <= 0;
        end else begin
            issue_stalled_d1 <= filter_issue && !filter_ready;
            if (filter_issue && !filter_ready) begin
                if (stall_length != 32'hffffffff) stall_length <= stall_length + 1;
            end else begin
                completed_stall_length <= stall_length;
                stall_length <= 0;
            end
            slot_valid_d1 <= slot_valid;
            slot_sent_d1 <= slot_sent;
            slot_rmatch_d1 <= slot_rmatch;
            filter_flush_d1 <= filter_flush;
            filter_resp_d1 <= filter_resp;
            issue_fire_d1 <= (filter_issue && filter_ready);
            issue_index_d1 <= filter_index;
            request_enq_d1 <= request_enq;
            request_idx_d1 <= request_idx;
            request_vpn_d1 <= request_vpn;
            request_stage_d1 <= request_stage;
        end
    end
    always @(posedge clock) begin
        if (reset || filter_flush) begin full_seen <= 0;
        release_seen <= 0;
        end else begin
            if (partition_full) full_seen <= 1;
            if (filter_resp && matching_slots != 0 && full_seen) release_seen <= 1;
        end
    end

    always @* begin
        allocated_slot = 0;
        for (integer port = 0; port < PORT_NUM; port = port + 1)
            if (request_enq[port]) allocated_slot = request_idx[port];
    end
    // 诊断独立统计，不计入功能分母，不调用UVM错误报告。
    // ----------------------------------------------------------------------------------------
    // 【cg_interface_diagnostics】接口一致性诊断
    // 探测功能：观察输入与实际输出是否一致，并把偏离/未知单独记录。
    // 采样时序：本拍请求/slot/返回用于场景判断；_d1用于下一拍入队、sent、释放或清空检查。
    // 仓位判读：本组weight=0，cp为观察结果，rs的1/2分别表示偏离/未知；reference_missing表示信号缺失。
    // allocation_matches：全称关系：本拍同key新请求的后口不重复enq且idx等于前口；它本身不核对旧slot合并的idx/内容。
    // same_key_result_matches：对任意同key后口检查RTL enqvalid=0且enqidx复用前口；不把“没有同key场景”误判为通过。
    // same_key_new_result_matches：仅在两口均无旧match的同key新请求场景下检查上述复用关系。
    // old_merge_result_matches：对每个oldmatch恢复最高匹配slot，检查RTL不新分配且enqidx指向该slot。
    // different_stage_result_matches：同VPN不同stage只允许各自旧match/分配，不能共享一个旧slot或新idx。
    // parallel_distinct_result_matches：不同VPN或stage的并行请求不能通过同一个新分配idx合并。
    // partition_result_matches：分区拍前已满时，即使本拍有response释放也不产生本拍enqvalid。
    // request_return_result_matches：PTW返回已命中请求时，entry匹配优先于新分配。
    // response_release_matches：前拍返回匹配的有效slot本拍均清valid；前拍flush时所有slot均清valid。
    covergroup cg_interface_diagnostics @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_allocation : coverpoint ($isunknown(allocation_matches) ? 2'd2 : (allocation_matches) ? 2'd0 : 2'd1)
            iff (sample_enable
                && |request_valid) {
            bins b_mismatch = {2'd1};
            bins b_unknown = {2'd2};
        }
        cp_response_release : coverpoint ($isunknown(response_release_matches) ? 2'd2 : (response_release_matches)
            ? 2'd0 : 2'd1)
            iff (sample_enable
                && filter_resp_d1) {
            bins b_mismatch = {2'd1};
            bins b_unknown = {2'd2};
        }
    endgroup
    cg_interface_diagnostics interface_diagnostics;
    initial begin
        interface_diagnostics = new();
    end

    // 05.03 Filter分配与固定分区容量
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_05_03】05.03 Filter分配与固定分区容量
    // 探测功能：观察按端口固定分区的 Filter 槽位分配与满状态，区分接口有效和真正入队。
    // 采样时序：本拍请求/slot/返回用于场景判断；_d1用于下一拍入队、sent、释放或清空检查。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：05.03.001, 05.03.002, 05.03.003, 05.03.004, 05.03.005, 05.03.006；逐点功能见下方注释，精确配值见分解表。
    // occupancy：当前有效slot数量，使用slot_valid计数；不等于已发出数量。
    // allocated_slot：本拍request_enq为1的最后一个端口给出的idx；用于slot编号仓。
    // allocation_matches：全称关系：本拍同key新请求的后口不重复enq且idx等于前口；它本身不核对旧slot合并的idx/内容。
    // enqueue_state_matches：前拍每个实际enq请求在本拍slot中具有有效位及相同VPN/stage；前拍flush时跳过。
    // partition_full：存在有效请求，所在分区不可分配且没有旧match；不同端口的存在性不能当作同一请求。
    // partition_release：存在有效请求，其分区不可分配且本拍返回命中该分区slot。
    covergroup cg_dtlb_05_03 @(posedge clock);
        option.per_instance = 1;
        // 05.03.001 FILTER_空表分配
        // 测试表目标：场景：某口新VPN/stage，既无旧match也无返回match，分区有空位。
        // 观察功能：enqvalid=1；在该口4项分区中选择最高编号空位，v置1、sent清0。 测试目的：观察按端口固定分区的 Filter
        // 槽位分配与满状态，区分接口有效和真正入队。
        cp_05_03_001 : coverpoint enqueue_state_matches
            iff (sample_enable
                && |request_enq_d1) {
            bins b_expected_set = {1'b1};
        }
        // 05.03.003 FILTER_并行异key
        // 测试表目标：场景：多口同拍不同VPN或stage且各分区有空位。 观察功能：各自分配；同拍最多LD4/ST2/PF2条。 测试目的：观察按端口固定分区的 Filter
        // 槽位分配与满状态，区分接口有效和真正入队。
        cp_05_03_003 : coverpoint parallel_distinct_result_matches
            iff (sample_enable
                && $countones(request_valid) >= 2
                && $countones(request_enq) >= 2) {
            bins b_expected_set = {1'b1};
        }
        // 05.03.004 FILTER_本分区满全局未满
        // 测试表目标：场景：目标口4项占满，其他口分区有空闲。 观察功能：canenq=0，不借用其他分区；req.ready恒1不代表成功入队。 测试目的：观察按端口固定分区的
        // Filter 槽位分配与满状态，区分接口有效和真正入队。
        cp_05_03_004 : coverpoint partition_result_matches
            iff (sample_enable
                && partition_full
                && occupancy < SLOT_NUM) {
            bins b_expected_set = {1'b1};
        }
        // 05.03.005 FILTER_整组满
        // 测试表目标：场景：所有v=1且新key请求到达。 观察功能：不入队；load hint.full有效，其他consumer通过miss重试恢复。 测试目的：观察按端口固定分区的
        // Filter 槽位分配与满状态，区分接口有效和真正入队。
        cp_05_03_005 : coverpoint (request_enq == 0)
            iff (sample_enable
                && occupancy == SLOT_NUM
                && |request_valid) {
            bins b_expected_result = {1'b1};
        }
        // 05.03.006 FILTER_清槽与分配同拍
        // 测试表目标：场景：分区满且本拍response清一个slot，同时新key到达。 观察功能：canenq用拍前v，通常本拍不能用刚释放槽，下一拍可分配。
        // 测试目的：观察按端口固定分区的 Filter 槽位分配与满状态，区分接口有效和真正入队。
        cp_05_03_006 : coverpoint partition_result_matches
            iff (sample_enable
                && partition_release
                && filter_resp) {
            bins b_expected_set = {1'b1};
        }
        // 05.03.002 FILTER_分区每slot覆盖
        // 测试表目标：场景：各口反复插入不同key直至分区占满。 观察功能：LD分区0..3/4..7/8..11/12..15；ST/PF分区0..3/4..7。
        // 测试目的：观察按端口固定分区的 Filter 槽位分配与满状态，区分接口有效和真正入队。
        cp_05_03_002 : coverpoint allocated_slot
            iff (sample_enable
                && |request_enq
                && (allocated_slot < SLOT_NUM) === 1'b1) {
            bins b_slot[] = {[0:SLOT_NUM-1]};
        }
    endgroup
    cg_dtlb_05_03 cg_dtlb_05_03_inst;

    // 05.04 Filter重复合并与getGpa元信息
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_05_04】05.04 Filter重复合并与getGpa元信息
    // 探测功能：观察相同 VPN/阶段请求的合并规则，以及合并时 getGpa 元信息是否保留或更新。
    // 采样时序：本拍请求/slot/返回用于场景判断；_d1用于下一拍入队、sent、释放或清空检查。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：05.04.001, 05.04.002, 05.04.003, 05.04.004, 05.04.005, 05.04.006；逐点功能见下方注释，精确配值见分解表。
    // allocation_matches：历史兼容判据；功能仓改用下列按RTL分支拆开的结果信号。
    // different_stage：存在有效请求与有效旧slot的VPN相同而stage不同。
    // low_free_high_full：同key新请求中存在较低口可分配、较高口不可分配的组合。
    // low_full_high_free：同key新请求中存在较低口不可分配、较高口可分配的组合。
    // new_merge：两个本拍有效请求的VPN和stage相同，后口与较早端口形成新请求合并场景。
    // old_merge：存在有效请求命中旧slot的request_oldmatch；场景标志不等于已验证实际分配结果。
    // partition_full：存在有效请求，所在分区不可分配且没有旧match；不同端口的存在性不能当作同一请求。
    covergroup cg_dtlb_05_04 @(posedge clock);
        option.per_instance = 1;
        // 05.04.001 FILTER_旧entry合并
        // 测试表目标：场景：新req VPN/stage==任一有效旧entry，分别sent=0/1。 观察功能：不分配新slot；enqidx指向旧match；匹配可跨端口分区。
        // 测试目的：观察相同 VPN/阶段请求的合并规则，以及合并时 getGpa 元信息是否保留或更新。
        cp_05_04_001 : coverpoint old_merge_result_matches
            iff (sample_enable
                && old_merge
                && |request_valid) {
            bins b_expected_set = {1'b1};
        }
        // 05.04.002 FILTER_同VPN不同stage
        // 测试表目标：场景：同VPN但s2xlate不同。 观察功能：不合并；各自分配和发送。 测试目的：观察相同 VPN/阶段请求的合并规则，以及合并时 getGpa
        // 元信息是否保留或更新。
        cp_05_04_002 : coverpoint different_stage_result_matches
            iff (sample_enable
                && different_stage
                && |request_valid) {
            bins b_expected_set = {1'b1};
        }
        // 05.04.003 FILTER_同拍新请求合并
        // 测试表目标：场景：i>j，两口同拍valid且VPN/stage相同，无旧match。
        // 观察功能：高编号端口enqvalid=0，enqidx/canenq继承较早匹配端口；只建一个entry。 测试目的：观察相同 VPN/阶段请求的合并规则，以及合并时
        // getGpa 元信息是否保留或更新。
        cp_05_04_003 : coverpoint same_key_new_result_matches
            iff (sample_enable
                && new_merge) {
            bins b_expected_set = {1'b1};
        }
        // 05.04.004 FILTER_满但旧match
        // 测试表目标：场景：本分区满，新req命中已有entry。 观察功能：不需分配；load hint.full仍可能因!canenq为1，记录实际行为。 测试目的：观察相同
        // VPN/阶段请求的合并规则，以及合并时 getGpa 元信息是否保留或更新。
        cp_05_04_004 : coverpoint old_merge_result_matches
            iff (sample_enable
                && old_merge
                && partition_full) {
            bins b_expected_set = {1'b1};
        }
        // 05.04.005 FILTER_低口满高口空同key
        // 测试表目标：场景：低口新key但分区满，高口同key且其分区空。 观察功能：高口继承低口canenq=0，不独立分配；后续重试。 测试目的：观察相同
        // VPN/阶段请求的合并规则，以及合并时 getGpa 元信息是否保留或更新。
        cp_05_04_005 : coverpoint same_key_result_matches
            iff (sample_enable
                && low_full_high_free) {
            bins b_expected_set = {1'b1};
        }
        // 05.04.006 FILTER_低口空高口满同key
        // 测试表目标：场景：低口有空位，高口同key但分区满。 观察功能：共享低口slot；高口继承可分配状态。 测试目的：观察相同 VPN/阶段请求的合并规则，以及合并时 getGpa
        // 元信息是否保留或更新。
        cp_05_04_006 : coverpoint same_key_result_matches
            iff (sample_enable
                && low_free_high_full) {
            bins b_expected_set = {1'b1};
        }
    endgroup
    cg_dtlb_05_04 cg_dtlb_05_04_inst;

    // 05.04 Filter重复合并与getGpa元信息
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_diag_05_04】05.04 Filter重复合并与getGpa元信息
    // 探测功能：观察相同 VPN/阶段请求的合并规则，以及合并时 getGpa 元信息是否保留或更新。
    // 采样时序：本拍请求/slot/返回用于场景判断；_d1用于下一拍入队、sent、释放或清空检查。
    // 仓位判读：本组weight=0，cp为观察结果，rs的1/2分别表示偏离/未知；reference_missing表示信号缺失。
    // 测试点：05.04.007, 05.04.008；逐点功能见下方注释，精确配值见分解表。
    // matching_slots：本拍PTW返回匹配的有效slot数量。
    // allocation_matches：全称关系：本拍同key新请求的后口不重复enq且idx等于前口；它本身不核对旧slot合并的idx/内容。
    // gpa_merge_without_upgrade：getGpa=1的新请求命中getGpa=0的旧slot；只描述这个触发场景。
    // highest_match：扫描有效且匹配返回的slot，保留最高编号；未找到时保持0。
    // matched_gpa_one：返回匹配的有效slot中至少一项getGpa=1。
    // matched_gpa_zero：返回匹配的有效slot中至少一项getGpa=0。
    covergroup cg_dtlb_diag_05_04 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        // 05.04.007：结果偏离或未知单列，正常命中不进入诊断仓。
        rs_05_04_007 : coverpoint ($isunknown(filter_getgpa == slot_getgpa[highest_match]) ? 2'd2 : (filter_getgpa
            == slot_getgpa[highest_match]) ? 2'd0 : 2'd1)
            iff (sample_enable
                && filter_resp
                && matching_slots > 1
                && matched_gpa_zero
                && matched_gpa_one) {
            bins b_mismatch = {2'd1};
            bins unknown  = {2'd2};
        }
        // 05.04.007 FILTER_getGpa不是OR
        // 测试表目标：场景：同一返回覆盖多个slot，getGpa混合0/1。 观察功能：E.io_getGpa取最高match slot，不是对全部slot OR；确认混合语义可接受。
        // 测试目的：观察相同 VPN/阶段请求的合并规则，以及合并时 getGpa 元信息是否保留或更新。
        cp_05_04_007 : coverpoint (filter_getgpa == slot_getgpa[highest_match])
            iff (sample_enable
                && filter_resp
                && matching_slots > 1
                && matched_gpa_zero
                && matched_gpa_one) {
            bins b_expected_result = {1'b1};
        }
        // 05.04.008：结果偏离或未知单列，正常命中不进入诊断仓。
        rs_05_04_008 : coverpoint ($isunknown(allocation_matches) ? 2'd2 : (allocation_matches) ? 2'd0 : 2'd1)
            iff (sample_enable
                && gpa_merge_without_upgrade) {
            bins b_mismatch = {2'd1};
            bins unknown  = {2'd2};
        }
        // 05.04.008 FILTER_合并不升级getGpa
        // 测试表目标：场景：有效同key旧entry.getGpa=0，新req.getGpa=1。 观察功能：按当前RTL只合并key，不更新旧getGpa；观察GPA重取能否闭环。
        // 测试目的：观察相同 VPN/阶段请求的合并规则，以及合并时 getGpa 元信息是否保留或更新。
        cp_05_04_008 : coverpoint old_merge_result_matches
            iff (sample_enable
                && gpa_merge_without_upgrade) {
            bins b_expected_set = {1'b1};
        }
    endgroup
    cg_dtlb_diag_05_04 cg_dtlb_diag_05_04_inst;

    // 05.05 Filter发射与RR仲裁
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_05_05】05.05 Filter发射与RR仲裁
    // 探测功能：观察组内待发项选择、真实握手和三组轮询仲裁，覆盖背压及恢复。
    // 采样时序：本拍请求/slot/返回用于场景判断；_d1用于下一拍入队、sent、释放或清空检查。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：05.05.001, 05.05.002, 05.05.003, 05.05.004；逐点功能见下方注释，精确配值见分解表。
    // pending_to_issue：当前有效且尚未发送的slot数量。
    // completed_stall_length：背压长度快照，用于握手恢复时按连续背压周期数分档。
    // highest_unsent：扫描有效且未sent的slot，保留最高编号；未找到时保持0。
    // sent_issue_repeat：本拍issue&&ready发射的index指向有效且已经sent的slot。
    covergroup cg_dtlb_05_05 @(posedge clock);
        option.per_instance = 1;
        // 05.05.001 FILTER_发射最高有效未发
        // 测试表目标：场景：多个v=1,sent=0且ready=1。 观察功能：issueindex选择最高编号v&&!sent；fire后仅该sent置1。
        // 测试目的：观察组内待发项选择、真实握手和三组轮询仲裁，覆盖背压及恢复。
        cp_05_05_001 : coverpoint (filter_index == highest_unsent)
            iff (sample_enable
                && filter_issue
                && filter_ready
                && pending_to_issue >= 2) {
            bins b_expected_result = {1'b1};
        }
        // 05.05.002 FILTER_已发送不重复发
        // 测试表目标：场景：entry v=1,sent=1，未返回。 观察功能：不再因该slot发请求；其他未发slot可继续发。
        // 测试目的：观察组内待发项选择、真实握手和三组轮询仲裁，覆盖背压及恢复。
        cp_05_05_002 : coverpoint sent_issue_repeat
            iff (sample_enable
                && filter_issue
                && filter_ready
                && |(slot_valid & slot_sent)
                && (!sent_issue_repeat) === 1'b1) {
            bins b_expected_clear = {1'b0};
        }
        // 05.05.003 FILTER_背压
        // 测试表目标：场景：E或Filter PTW req.valid=1,ready=0，随后ready=1。
        // 观察功能：未fire不置sent；最终按fire发送；payload不要求在整个stall期间保持同一slot。
        // 测试目的：观察组内待发项选择、真实握手和三组轮询仲裁，覆盖背压及恢复。
        cp_05_05_003 : coverpoint completed_stall_length
            iff (sample_enable
                && filter_issue
                && filter_ready
                && issue_stalled_d1) {
            bins b_one = {1};
            bins b_short = {[2:4]};
            bins b_medium = {[5:16]};
            bins b_long = {[17:$]};
        }
        // 05.05.004 FILTER_发送时返回同slot
        // 测试表目标：场景：已选择issueindex，response同拍命中该slot。 观察功能：sent可被置1同时v清0；后续不再有效；不要求无效sent清零。
        // 测试目的：观察组内待发项选择、真实握手和三组轮询仲裁，覆盖背压及恢复。
        cp_05_05_004 : coverpoint (!slot_valid[issue_index_d1])
            iff (sample_enable
                && issue_fire_d1
                && filter_resp_d1
                && slot_rmatch_d1[issue_index_d1]) {
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_05_05 cg_dtlb_05_05_inst;

    // 05.06 返回匹配、乱序与多项释放
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_05_06】05.06 返回匹配、乱序与多项释放
    // 探测功能：观察返回按内容匹配释放槽位的行为，包括乱序、多槽命中及无匹配返回。
    // 采样时序：本拍请求/slot/返回用于场景判断；_d1用于下一拍入队、sent、释放或清空检查。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：05.06.001, 05.06.002, 05.06.004, 05.06.005, 05.06.006, 05.06.007,
    // 05.06.008；逐点功能见下方注释，精确配值见分解表。
    // matching_slots：本拍PTW返回匹配的有效slot数量。
    // allocation_matches：全称关系：本拍同key新请求的后口不重复enq且idx等于前口；它本身不核对旧slot合并的idx/内容。
    // highest_match：扫描有效且匹配返回的slot，保留最高编号；未找到时保持0。
    // ignored_invalid_sent：存在valid=0而sent=1的slot，用于验证无效项不参与发送。
    // mixed_sent_return：本拍匹配多个有效slot，且同时包含sent=0和sent=1项。
    // request_return_match：存在有效请求的request_rmatch为1，即本拍请求命中PTW返回。
    // response_release_matches：前拍返回匹配的有效slot本拍均清valid；前拍flush时所有slot均清valid。
    // return_without_old_entry：有效请求命中本拍返回，且没有有效旧slot命中该返回。
    // sent_issue_repeat：本拍issue&&ready发射的index指向有效且已经sent的slot。
    covergroup cg_dtlb_05_06 @(posedge clock);
        option.per_instance = 1;
        // 05.06.001 FILTER_返回命中新req
        // 测试表目标：场景：E.io_ptw_resp_valid=1且命中本拍req。 观察功能：req不入队；load
        // hint.full=1；返回若未命中任何旧entry则io_refill=0。 测试目的：观察返回按内容匹配释放槽位的行为，包括乱序、多槽命中及无匹配返回。
        cp_05_06_001 : coverpoint request_return_result_matches
            iff (sample_enable
                && request_return_match
                && filter_resp) {
            bins b_expected_set = {1'b1};
        }
        // 05.06.002 FILTER_只返回匹配无旧entry
        // 测试表目标：场景：返回匹配新请求但没有旧v match。 观察功能：io_refill=0；新请求被返回窗口过滤后需要retry，不伪造storage refill。
        // 测试目的：观察返回按内容匹配释放槽位的行为，包括乱序、多槽命中及无匹配返回。
        cp_05_06_002 : coverpoint filter_refill
            iff (sample_enable
                && return_without_old_entry
                && filter_resp
                && (!filter_refill) === 1'b1) {
            bins b_expected_clear = {1'b0};
        }
        // 05.06.004 FILTER_返回命中多slot
        // 测试表目标：场景：多个旧entry VPN处于同一个返回sector/大页/NAPOT范围。 观察功能：一次返回清所有match；refill=1；load
        // hint.replay_all=1。 测试目的：观察返回按内容匹配释放槽位的行为，包括乱序、多槽命中及无匹配返回。
        cp_05_06_004 : coverpoint response_release_matches
            iff (sample_enable
                && filter_resp_d1
                && $countones(slot_valid_d1 & slot_rmatch_d1) > 1) {
            bins b_expected_set = {1'b1};
        }
        // 05.06.005 FILTER_返回选中index
        // 测试表目标：场景：response命中多个旧entry。 观察功能：ptwResp_EntryMatchFirst实际取最高编号；getGpa/hint.id选该entry。
        // 测试目的：观察返回按内容匹配释放槽位的行为，包括乱序、多槽命中及无匹配返回。
        cp_05_06_005 : coverpoint (filter_getgpa == slot_getgpa[highest_match])
            iff (sample_enable
                && filter_resp
                && matching_slots > 1) {
            bins b_expected_result = {1'b1};
        }
        // 05.06.006 FILTER_不匹配返回
        // 测试表目标：场景：P.valid但无VPN/stage/ASID/VMID合法match。 观察功能：refill=0，不清v；无效返回不能被当作该组完成。
        // 测试目的：观察返回按内容匹配释放槽位的行为，包括乱序、多槽命中及无匹配返回。
        cp_05_06_006 : coverpoint filter_refill
            iff (sample_enable
                && filter_resp
                && matching_slots == 0
                && (!filter_refill) === 1'b1) {
            bins b_expected_clear = {1'b0};
        }
        // 05.06.007 FILTER_无效sent残留
        // 测试表目标：场景：response/flush后v=0,sent=1。 观察功能：不发射该slot、不计有效occupancy；避免用OR(sent)作功能忙状态。
        // 测试目的：观察返回按内容匹配释放槽位的行为，包括乱序、多槽命中及无匹配返回。
        cp_05_06_007 : coverpoint sent_issue_repeat
            iff (sample_enable
                && ignored_invalid_sent
                && (!sent_issue_repeat) === 1'b1) {
            bins b_expected_clear = {1'b0};
        }
        // 05.06.008 FILTER_旧返回可清未发slot
        // 测试表目标：场景：返回大页/sector覆盖一个sent=1 entry和一个sent=0 entry。
        // 观察功能：两个v都可清，匹配不要求sent=1；未发slot不应再单独发射，已接受外部token仍独立记账。
        // 测试目的：观察返回按内容匹配释放槽位的行为，包括乱序、多槽命中及无匹配返回。
        cp_05_06_008 : coverpoint filter_refill
            iff (sample_enable
                && filter_resp
                && mixed_sent_return) {
            bins b_expected_set = {1'b1};
        }
    endgroup
    cg_dtlb_05_06 cg_dtlb_05_06_inst;

    // 05.07 Hint与Filter状态观测
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_05_07】05.07 Hint与Filter状态观测
    // 探测功能：观察 Filter 提示信号与真实有效槽状态的关系，避免用调试计数替代功能状态。
    // 采样时序：本拍请求/slot/返回用于场景判断；_d1用于下一拍入队、sent、释放或清空检查。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：05.07.005；逐点功能见下方注释，精确配值见分解表。
    // capacity_class：占用分类：0=空，1=单项，2=中间，3=满。
    covergroup cg_dtlb_05_07 @(posedge clock);
        option.per_instance = 1;
        // 05.07.005 FILTER_occupancy与outstanding分离
        // 测试表目标：场景：已fire多个token，一次response匹配清多个entry。 观察功能：entry
        // occupancy、sent数量、外部未返回token数分别记录，不能等同。 测试目的：观察 Filter 提示信号与真实有效槽状态的关系，避免用调试计数替代功能状态。
        cp_05_07_005 : coverpoint capacity_class
            iff (sample_enable
                && ((filter_issue && filter_ready) || filter_resp || filter_flush)) {
            bins b_empty = {0};
            bins b_single = {1};
            bins b_partial = {2};
            bins b_full = {3};
        }
        // 05.07.004 FILTER_ROB头miss指示
        // 05.07.004：debugHead/rob_head_miss_in_tlb 已裁剪；仅范围说明，不建立动态仓。
    endgroup
    cg_dtlb_05_07 cg_dtlb_05_07_inst;
    initial begin
        cg_dtlb_05_07_inst = new();
    end

    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_05_07_unobservable】05.07 Hint与Filter状态观测
    // 探测功能：观察 Filter 提示信号与真实有效槽状态的关系，避免用调试计数替代功能状态。
    // 采样时序：本拍请求/slot/返回用于场景判断；_d1用于下一拍入队、sent、释放或清空检查。
    // 仓位判读：本组weight=0，cp为观察结果，rs的1/2分别表示偏离/未知；reference_missing表示信号缺失。
    // 测试点：05.07.004, 05.07.006；逐点功能见下方注释，精确配值见分解表。
    // inflight_counter_observable：常量0：生成RTL裁剪inflight_counter；不能观测真实回绕。
    // rob_head_miss_observable：常量0：生成RTL未暴露ROB头miss指示；reference_missing只记录参考缺失。
    covergroup cg_dtlb_05_07_unobservable @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;

        // 05.07.004 FILTER_ROB头miss指示：RTL 没有 debugHead 观测接口。
        // 测试表目标：场景：debugTopDown.robHeadVaddr.valid且VPN匹配/不匹配任意v entry。
        // 观察功能：OR三组rob_head_miss_in_tlb；按VPN匹配，不是完整ASID/VMID翻译命中。 测试目的：观察 Filter
        // 提示信号与真实有效槽状态的关系，避免用调试计数替代功能状态。 当前RTL不可观测，此处仅为reference_missing占位。
        cp_05_07_004 : coverpoint rob_head_miss_observable
            iff (sample_enable) {
            bins reference_missing = {1'b0};
        }
        rs_05_07_004 : coverpoint rob_head_miss_observable
            iff (sample_enable) {
            bins reference_missing = {1'b0};
        }

        // 05.07.006 FILTER_计数器回绕诊断：RTL 没有可用 inflight_counter。
        // 测试表目标：场景：大量fire/返回及跨组广播未匹配返回，观察inflight_counter。
        // 观察功能：当前计数器位宽LD4位/STPF3位，Size值不可表示，且按所有返回增减；仅诊断，不作为入队容量权威。 测试目的：观察 Filter
        // 提示信号与真实有效槽状态的关系，避免用调试计数替代功能状态。 当前RTL不可观测，此处仅为reference_missing占位。
        cp_05_07_006 : coverpoint inflight_counter_observable
            iff (sample_enable) {
            bins reference_missing = {1'b0};
        }
        rs_05_07_006 : coverpoint inflight_counter_observable
            iff (sample_enable) {
            bins reference_missing = {1'b0};
        }
    endgroup
    cg_dtlb_05_07_unobservable cg_dtlb_05_07_unobservable_inst;
    initial begin
        cg_dtlb_05_07_unobservable_inst = new();
    end

    // 05.07 Hint与Filter状态观测

    // 08.04 Filter清空与同拍竞争
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_08_04】08.04 Filter清空与同拍竞争
    // 探测功能：观察 Filter 清空与入队、发射、返回同拍发生时的最终有效状态。
    // 采样时序：本拍请求/slot/返回用于场景判断；_d1用于下一拍入队、sent、释放或清空检查。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：08.04.001, 08.04.002, 08.04.003, 08.04.004, 08.04.005；逐点功能见下方注释，精确配值见分解表。
    covergroup cg_dtlb_08_04 @(posedge clock);
        option.per_instance = 1;
        // 08.04.001 FENCE_reset优先
        // 测试表目标：场景：reset与fence/回填并发。 观察功能：所有v复位清0；后续第一笔重新miss。 测试目的：观察 Filter
        // 清空与入队、发射、返回同拍发生时的最终有效状态。
        cp_08_04_001 : coverpoint (slot_valid == 0)
            iff (sample_enable
                && filter_flush_d1
                && reset) {
            bins b_expected_result = {1'b1};
        }
        // 08.04.002 FILTER_flush清空
        // 测试表目标：场景：E.io_flush=1且已有未发/已发entry。 观察功能：所有v清0；sent不要求清零；重新分配时sent=0。 测试目的：观察 Filter
        // 清空与入队、发射、返回同拍发生时的最终有效状态。
        cp_08_04_002 : coverpoint (slot_valid == 0)
            iff (sample_enable
                && filter_flush_d1
                && |slot_valid_d1) {
            bins b_expected_result = {1'b1};
        }
        // 08.04.003 FILTER_flush与入队
        // 测试表目标：场景：io_flush=1且enqvalid=1。 观察功能：v清0优先；payload变化不代表有效入队。 测试目的：观察 Filter
        // 清空与入队、发射、返回同拍发生时的最终有效状态。
        cp_08_04_003 : coverpoint (slot_valid == 0)
            iff (sample_enable
                && filter_flush_d1
                && |request_enq_d1) {
            bins b_expected_result = {1'b1};
        }
        // 08.04.004 FILTER_flush与发射
        // 测试表目标：场景：io_flush=1且Q.valid/ready都1。 观察功能：组合端口仍可能fire；下拍v清0，环境epoch协议应处理这笔token。 测试目的：观察
        // Filter 清空与入队、发射、返回同拍发生时的最终有效状态。
        cp_08_04_004 : coverpoint (slot_valid == 0)
            iff (sample_enable
                && filter_flush_d1
                && issue_fire_d1) {
            bins b_expected_result = {1'b1};
        }
        // 08.04.005 FILTER_flush与返回
        // 测试表目标：场景：io_flush=1且response match旧entry。 观察功能：io_refill组合可能仍高，后级MemBlock/DTLB
        // flush门控必须阻止旧回填。 测试目的：观察 Filter 清空与入队、发射、返回同拍发生时的最终有效状态。
        cp_08_04_005 : coverpoint (slot_valid == 0)
            iff (sample_enable
                && filter_flush_d1
                && filter_resp_d1
                && |(slot_valid_d1 & slot_rmatch_d1)) {
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_08_04 cg_dtlb_08_04_inst;

    // 09.01 跨流程并发与恢复组合
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_09_01】09.01 跨流程并发与恢复组合
    // 探测功能：观察多个流程同时竞争时是否经历预期的抑制、释放和重试，并记录最终结束原因。
    // 采样时序：本拍请求/slot/返回用于场景判断；_d1用于下一拍入队、sent、释放或清空检查。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：09.01.002；逐点功能见下方注释，精确配值见分解表。
    // enqueue_state_matches：前拍每个实际enq请求在本拍slot中具有有效位及相同VPN/stage；前拍flush时跳过。
    // full_seen：复位后曾观察到slot满的历史标志。
    // release_seen：复位后曾观察到返回释放的历史标志。
    covergroup cg_dtlb_09_01 @(posedge clock);
        option.per_instance = 1;
        // 09.01.002 CONCURRENT_满分区_返回_重试
        // 测试表目标：场景：分区满→返回释放→新请求重试→回填→命中。 观察功能：确保full不会成为永久丢请求；覆盖各需求端口。
        // 测试目的：观察多个流程同时竞争时是否经历预期的抑制、释放和重试，并记录最终结束原因。
        cp_09_01_002 : coverpoint enqueue_state_matches
            iff (sample_enable
                && full_seen
                && release_seen
                && |request_enq) {
            bins b_expected_set = {1'b1};
        }
    endgroup
    cg_dtlb_09_01 cg_dtlb_09_01_inst;

    initial begin
        cg_dtlb_05_03_inst = new();
        cg_dtlb_05_04_inst = new();
        cg_dtlb_diag_05_04_inst = new();
        cg_dtlb_05_05_inst = new();
        cg_dtlb_05_06_inst = new();
        cg_dtlb_08_04_inst = new();
        cg_dtlb_09_01_inst = new();
    end

endmodule: memblock_dtlb_filter_fcov
`endif

`endif
