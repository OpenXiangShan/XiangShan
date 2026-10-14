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

    reg [31:0] stall_length;
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
    reg [3:0] highest_unsent;
    reg [3:0] highest_match;
    reg matched_gpa_zero;
    reg matched_gpa_one;
    reg allocation_matches;
    reg enqueue_state_matches;
    reg response_release_matches;
    reg empty_allocation;
    reg partition_full;
    reg partition_release;
    reg old_merge;
    reg different_stage;
    reg new_merge;
    reg low_full_high_free;
    reg low_free_high_full;
    reg mixed_sent_return;
    reg ignored_invalid_sent;
    reg request_return_match;
    reg return_without_old_entry;
    reg gpa_merge_without_upgrade;
    reg sent_issue_repeat;
    reg full_seen;
    reg release_seen;
    reg [3:0] allocated_slot;
    wire sample_enable = (reset === 1'b0) && (reset_backend_done === 1'b1);
    wire [4:0] occupancy = $countones(slot_valid);
    wire [4:0] sent_occupancy = $countones(slot_valid & slot_sent);
    wire [4:0] matching_slots = $countones(slot_rmatch & slot_valid);
    wire [4:0] pending_to_issue = $countones(slot_valid & ~slot_sent);
    wire issue_fire = filter_issue && filter_ready;
    wire response_sample = filter_resp;
    wire [1:0] capacity_class = occupancy == 0 ? 2'd0 : occupancy == 1 ? 2'd1 : occupancy == SLOT_NUM ? 2'd3 : 2'd2;
    // 当前生成 RTL 已裁剪 debugHead/rob_head_miss_in_tlb 与 inflight_counter。
    // 这两个点保留 weight=0 的 reference_missing 证据，不伪造功能命中。
    wire rob_head_miss_observable = 1'b0;
    wire inflight_counter_observable = 1'b0;
    // 分配和内容匹配只遍历本实例的16/8个物理slot，状态数组不随事务总数增长。
    always @* begin
        highest_unsent = 0;
        highest_match = 0;
        matched_gpa_zero = 0;
        matched_gpa_one = 0;
        allocation_matches = 1;
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
        for (integer slot = 0; slot < SLOT_NUM; slot = slot + 1) begin
            if (slot_valid[slot] && !slot_sent[slot]) highest_unsent = slot;
            if (slot_valid[slot] && slot_rmatch[slot]) begin
                highest_match = slot;
                matched_gpa_zero = matched_gpa_zero || !slot_getgpa[slot];
                matched_gpa_one = matched_gpa_one || slot_getgpa[slot];
            end
            if (!slot_valid[slot] && slot_sent[slot]) ignored_invalid_sent = 1;
            if (issue_fire && filter_index == slot && slot_valid[slot] && slot_sent[slot]) sent_issue_repeat = 1;
            if (filter_resp_d1 && slot_valid_d1[slot] && slot_rmatch_d1[slot])
                response_release_matches = response_release_matches && !slot_valid[slot];
            if (filter_flush_d1) response_release_matches = response_release_matches && !slot_valid[slot];
        end
        mixed_sent_return = matching_slots > 1 && $countones(slot_valid & slot_rmatch & slot_sent) > 0 &&
            $countones(slot_valid & slot_rmatch & ~slot_sent) > 0;
        for (integer port = 0; port < PORT_NUM; port = port + 1) begin
            if (request_valid[port]) begin
                empty_allocation = empty_allocation || (request_can[port] && request_oldmatch[port] == 0 &&
                    !request_rmatch[port]);
                partition_full = partition_full || (!request_can[port] && request_oldmatch[port] == 0);
                partition_release = partition_release || (!request_can[port] && |slot_rmatch[port*4 +: 4]);
                old_merge = old_merge || |request_oldmatch[port];
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
                        allocation_matches = allocation_matches && !request_enq[port] &&
                            request_idx[port] == request_idx[earlier];
                        low_full_high_free = low_full_high_free || (!request_can[earlier] && request_can[port]);
                        low_free_high_full = low_free_high_full || (request_can[earlier] && !request_can[port]);
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
            issue_fire_d1 <= issue_fire;
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
    wire allocation_sample = |request_enq;
    always @* begin
        allocated_slot = 0;
        for (integer port = 0; port < PORT_NUM; port = port + 1)
            if (request_enq[port]) allocated_slot = request_idx[port];
    end
    // 诊断独立统计，不计入功能分母，不调用UVM错误报告。
    covergroup cg_interface_diagnostics @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_allocation : coverpoint ($isunknown(allocation_matches) ? 2'd2 : (allocation_matches) ? 2'd0 : 2'd1)
            iff (sample_enable && (|request_valid)) {
            bins b_mismatch = {2'd1};
            bins b_unknown = {2'd2};
        }
        cp_response_release : coverpoint ($isunknown(response_release_matches) ? 2'd2 : (response_release_matches)
            ? 2'd0 : 2'd1)
            iff (sample_enable && (filter_resp_d1)) {
            bins b_mismatch = {2'd1};
            bins b_unknown = {2'd2};
        }
    endgroup
    cg_interface_diagnostics interface_diagnostics;
    initial begin
        interface_diagnostics = new();
    end

    // 05.03 Filter分配与固定分区容量
    covergroup cg_dtlb_05_03 @(posedge clock);
        option.per_instance = 1;
        // 05.03.001 FILTER_空表分配
        cp_05_03_001 : coverpoint enqueue_state_matches
            iff (sample_enable && (|request_enq_d1)) {
            bins b_expected_set = {1'b1};
        }
        // 05.03.003 FILTER_并行异key
        cp_05_03_003 : coverpoint allocation_matches
            iff (sample_enable && ($countones(request_valid) >= 2 && $countones(request_enq) >= 2)) {
            bins b_expected_set = {1'b1};
        }
        // 05.03.004 FILTER_本分区满全局未满
        cp_05_03_004 : coverpoint allocation_matches
            iff (sample_enable && (partition_full && occupancy < SLOT_NUM)) {
            bins b_expected_set = {1'b1};
        }
        // 05.03.005 FILTER_整组满
        cp_05_03_005 : coverpoint (request_enq == 0)
            iff (sample_enable && (occupancy == SLOT_NUM && |request_valid)) {
            bins b_expected_result = {1'b1};
        }
        // 05.03.006 FILTER_清槽与分配同拍
        cp_05_03_006 : coverpoint allocation_matches
            iff (sample_enable && (partition_release && filter_resp)) {
            bins b_expected_set = {1'b1};
        }
        // 05.03.002 FILTER_分区每slot覆盖
        cp_05_03_002 : coverpoint allocated_slot
            iff (sample_enable && (allocation_sample) && ((allocated_slot < SLOT_NUM) === 1'b1)) {
            bins b_slot[] = {[0:SLOT_NUM-1]};
        }
    endgroup
    cg_dtlb_05_03 cg_dtlb_05_03_inst;

    // 05.04 Filter重复合并与getGpa元信息
    covergroup cg_dtlb_05_04 @(posedge clock);
        option.per_instance = 1;
        // 05.04.001 FILTER_旧entry合并
        cp_05_04_001 : coverpoint allocation_matches
            iff (sample_enable && (old_merge && |request_valid)) {
            bins b_expected_set = {1'b1};
        }
        // 05.04.002 FILTER_同VPN不同stage
        cp_05_04_002 : coverpoint allocation_matches
            iff (sample_enable && (different_stage && |request_valid)) {
            bins b_expected_set = {1'b1};
        }
        // 05.04.003 FILTER_同拍新请求合并
        cp_05_04_003 : coverpoint allocation_matches
            iff (sample_enable && (new_merge)) {
            bins b_expected_set = {1'b1};
        }
        // 05.04.004 FILTER_满但旧match
        cp_05_04_004 : coverpoint allocation_matches
            iff (sample_enable && (old_merge && partition_full)) {
            bins b_expected_set = {1'b1};
        }
        // 05.04.005 FILTER_低口满高口空同key
        cp_05_04_005 : coverpoint allocation_matches
            iff (sample_enable && (low_full_high_free)) {
            bins b_expected_set = {1'b1};
        }
        // 05.04.006 FILTER_低口空高口满同key
        cp_05_04_006 : coverpoint allocation_matches
            iff (sample_enable && (low_free_high_full)) {
            bins b_expected_set = {1'b1};
        }
    endgroup
    cg_dtlb_05_04 cg_dtlb_05_04_inst;

    // 05.04 Filter重复合并与getGpa元信息
    covergroup cg_dtlb_diag_05_04 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        // 05.04.007：结果偏离或未知单列，正常命中不进入诊断仓。
        rs_05_04_007 : coverpoint ($isunknown(filter_getgpa == slot_getgpa[highest_match]) ? 2'd2 : (filter_getgpa
            == slot_getgpa[highest_match]) ? 2'd0 : 2'd1)
            iff (sample_enable && (filter_resp && matching_slots > 1 && matched_gpa_zero && matched_gpa_one)) {
            bins b_mismatch = {2'd1};
            bins unknown  = {2'd2};
        }
        // 05.04.007 FILTER_getGpa不是OR
        cp_05_04_007 : coverpoint (filter_getgpa == slot_getgpa[highest_match])
            iff (sample_enable && (filter_resp && matching_slots > 1 && matched_gpa_zero && matched_gpa_one)) {
            bins b_expected_result = {1'b1};
        }
        // 05.04.008：结果偏离或未知单列，正常命中不进入诊断仓。
        rs_05_04_008 : coverpoint ($isunknown(allocation_matches) ? 2'd2 : (allocation_matches) ? 2'd0 : 2'd1)
            iff (sample_enable && (gpa_merge_without_upgrade)) {
            bins b_mismatch = {2'd1};
            bins unknown  = {2'd2};
        }
        // 05.04.008 FILTER_合并不升级getGpa
        cp_05_04_008 : coverpoint allocation_matches
            iff (sample_enable && (gpa_merge_without_upgrade)) {
            bins b_expected_set = {1'b1};
        }
    endgroup
    cg_dtlb_diag_05_04 cg_dtlb_diag_05_04_inst;

    // 05.05 Filter发射与RR仲裁
    covergroup cg_dtlb_05_05 @(posedge clock);
        option.per_instance = 1;
        // 05.05.001 FILTER_发射最高有效未发
        cp_05_05_001 : coverpoint (filter_index == highest_unsent)
            iff (sample_enable && (issue_fire && pending_to_issue >= 2)) {
            bins b_expected_result = {1'b1};
        }
        // 05.05.002 FILTER_已发送不重复发
        cp_05_05_002 : coverpoint sent_issue_repeat
            iff (sample_enable && (issue_fire && |(slot_valid & slot_sent)) && ((!sent_issue_repeat) === 1'b1)) {
            bins b_expected_clear = {1'b0};
        }
        // 05.05.003 FILTER_背压
        cp_05_05_003 : coverpoint completed_stall_length
            iff (sample_enable && (issue_fire && issue_stalled_d1)) {
            bins b_one = {1};
            bins b_short = {[2:4]};
            bins b_medium = {[5:16]};
            bins b_long = {[17:$]};
        }
        // 05.05.004 FILTER_发送时返回同slot
        cp_05_05_004 : coverpoint (!slot_valid[issue_index_d1])
            iff (sample_enable && (issue_fire_d1 && filter_resp_d1 && slot_rmatch_d1[issue_index_d1])) {
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_05_05 cg_dtlb_05_05_inst;

    // 05.06 返回匹配、乱序与多项释放
    covergroup cg_dtlb_05_06 @(posedge clock);
        option.per_instance = 1;
        // 05.06.001 FILTER_返回命中新req
        cp_05_06_001 : coverpoint allocation_matches
            iff (sample_enable && (request_return_match && filter_resp)) {
            bins b_expected_set = {1'b1};
        }
        // 05.06.002 FILTER_只返回匹配无旧entry
        cp_05_06_002 : coverpoint filter_refill
            iff (sample_enable && (return_without_old_entry && filter_resp) && ((!filter_refill) === 1'b1)) {
            bins b_expected_clear = {1'b0};
        }
        // 05.06.004 FILTER_返回命中多slot
        cp_05_06_004 : coverpoint response_release_matches
            iff (sample_enable && (filter_resp_d1 && $countones(slot_valid_d1 & slot_rmatch_d1) > 1)) {
            bins b_expected_set = {1'b1};
        }
        // 05.06.005 FILTER_返回选中index
        cp_05_06_005 : coverpoint (filter_getgpa == slot_getgpa[highest_match])
            iff (sample_enable && (filter_resp && matching_slots > 1)) {
            bins b_expected_result = {1'b1};
        }
        // 05.06.006 FILTER_不匹配返回
        cp_05_06_006 : coverpoint filter_refill
            iff (sample_enable && (filter_resp && matching_slots == 0) && ((!filter_refill) === 1'b1)) {
            bins b_expected_clear = {1'b0};
        }
        // 05.06.007 FILTER_无效sent残留
        cp_05_06_007 : coverpoint sent_issue_repeat
            iff (sample_enable && (ignored_invalid_sent) && ((!sent_issue_repeat) === 1'b1)) {
            bins b_expected_clear = {1'b0};
        }
        // 05.06.008 FILTER_旧返回可清未发slot
        cp_05_06_008 : coverpoint filter_refill
            iff (sample_enable && (filter_resp && mixed_sent_return)) {
            bins b_expected_set = {1'b1};
        }
    endgroup
    cg_dtlb_05_06 cg_dtlb_05_06_inst;

    // 05.07 Hint与Filter状态观测
    covergroup cg_dtlb_05_07 @(posedge clock);
        option.per_instance = 1;
        // 05.07.005 FILTER_occupancy与outstanding分离
        cp_05_07_005 : coverpoint capacity_class
            iff (sample_enable && (issue_fire || filter_resp || filter_flush)) {
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

    covergroup cg_dtlb_05_07_unobservable @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;

        // 05.07.004 FILTER_ROB头miss指示：RTL 没有 debugHead 观测接口。
        cp_05_07_004 : coverpoint rob_head_miss_observable iff (sample_enable) {
            bins reference_missing = {1'b0};
        }
        rs_05_07_004 : coverpoint rob_head_miss_observable iff (sample_enable) {
            bins reference_missing = {1'b0};
        }

        // 05.07.006 FILTER_计数器回绕诊断：RTL 没有可用 inflight_counter。
        cp_05_07_006 : coverpoint inflight_counter_observable iff (sample_enable) {
            bins reference_missing = {1'b0};
        }
        rs_05_07_006 : coverpoint inflight_counter_observable iff (sample_enable) {
            bins reference_missing = {1'b0};
        }
    endgroup
    cg_dtlb_05_07_unobservable cg_dtlb_05_07_unobservable_inst;
    initial begin
        cg_dtlb_05_07_unobservable_inst = new();
    end

    // 05.07 Hint与Filter状态观测

    // 08.04 Filter清空与同拍竞争
    covergroup cg_dtlb_08_04 @(posedge clock);
        option.per_instance = 1;
        // 08.04.001 FENCE_reset优先
        cp_08_04_001 : coverpoint (slot_valid == 0)
            iff (sample_enable && (filter_flush_d1 && reset)) {
            bins b_expected_result = {1'b1};
        }
        // 08.04.002 FILTER_flush清空
        cp_08_04_002 : coverpoint (slot_valid == 0)
            iff (sample_enable && (filter_flush_d1 && |slot_valid_d1)) {
            bins b_expected_result = {1'b1};
        }
        // 08.04.003 FILTER_flush与入队
        cp_08_04_003 : coverpoint (slot_valid == 0)
            iff (sample_enable && (filter_flush_d1 && |request_enq_d1)) {
            bins b_expected_result = {1'b1};
        }
        // 08.04.004 FILTER_flush与发射
        cp_08_04_004 : coverpoint (slot_valid == 0)
            iff (sample_enable && (filter_flush_d1 && issue_fire_d1)) {
            bins b_expected_result = {1'b1};
        }
        // 08.04.005 FILTER_flush与返回
        cp_08_04_005 : coverpoint (slot_valid == 0)
            iff (sample_enable && (filter_flush_d1 && filter_resp_d1 && |(slot_valid_d1 & slot_rmatch_d1))) {
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_08_04 cg_dtlb_08_04_inst;

    // 09.01 跨流程并发与恢复组合
    covergroup cg_dtlb_09_01 @(posedge clock);
        option.per_instance = 1;
        // 09.01.002 CONCURRENT_满分区_返回_重试
        cp_09_01_002 : coverpoint enqueue_state_matches
            iff (sample_enable && (full_seen && release_seen && |request_enq)) {
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
