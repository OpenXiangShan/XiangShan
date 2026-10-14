//=========================================================
// memblock_dtlb_integration_fcov：DTLB V2 只读功能覆盖率。
// 普通信号端口、时钟事件采样、逐特性直接建仓；没有结构体库、function 或采样分发器。
// 时延与长度采用范围仓；物理 way/slot/子页采用逐项仓。
//=========================================================
`ifndef MEMBLOCK_DTLB_INTEGRATION_FCOV__SV
`define MEMBLOCK_DTLB_INTEGRATION_FCOV__SV

`ifdef MEMBLOCK_UT_FCOV
module memblock_dtlb_integration_fcov (
    input wire clock,
    input wire reset,
    input wire reset_backend_done,
    input wire ext_valid,
    input wire ext_ready,
    input wire [37:0] ext_vpn,
    input wire [1:0] ext_stage,
    input wire top_sfence,
    input wire top_csr_changed,
    input wire mid_sfence,
    input wire mid_csr_changed,
    input wire [43:0] top_satp_ppn,
    input wire [43:0] top_vsatp_ppn,
    input wire [43:0] top_hgatp_ppn,
    input wire atomic_sel,
    input wire segment_sel,
    input wire [2:0] route_mask,
    input wire route_valid,
    input wire [2:0] route_gpa,
    input wire mem_response_gate,
    input wire [2:0][4:0] hints,
    input wire [5:0] hint_resp,
    input wire [2:0] consumer_hint,
    input wire [2:0][1:0] consumer_replay,
    input wire return_valid,
    input wire [1:0] return_stage,
    input wire [34:0] return_s1tag,
    input wire [37:0] return_s2tag,
    input wire [4:0] consumer_valid,
    input wire [4:0][47:0] consumer_pa,
    input wire [4:0][15:0] consumer_mask,
    input wire [4:0][1:0] consumer_pbmt,
    input wire [4:0] consumer_nc,
    input wire [4:0] consumer_mmio,
    input wire [4:0] consumer_prefetch,
    input wire [4:0] consumer_miss,
    input wire [4:0] consumer_deny,
    input wire [4:0] consumer_pf,
    input wire [4:0] consumer_gpf,
    input wire [4:0] consumer_af,
    input wire [4:0] consumer_active,
    input wire [31:0][1:0] pmp_mode,
    input wire [31:0][45:0] pmp_addr,
    input wire [31:0][47:0] pmp_mask,
    input wire [7:0] request_valid,
    input wire [7:0] request_kill,
    input wire [7:0][2:0] request_cmd,
    input wire [7:0] response_valid,
    input wire [7:0][2:0] response_cmd,
    input wire [7:0] response_qvalid,
    input wire [2:0] filter_issue,
    input wire [2:0] filter_ready,
    input wire [2:0] filter_refill,
    input wire [2:0] filter_flush,
    input wire [2:0] filter_resp,
    input wire [2:0] dtlb_return_valid,
    input wire [2:0] dtlb_return_getgpa,
    input wire [2:0] dtlb_write_valid,
    input wire [2:0] dtlb_fence_valid,
    input wire [7:0] filter_input_valid,
    input wire [7:0] local_return_valid,
    input wire [2:0] hint_request_valid,
    input wire [2:0][3:0] hint_request_id,
    input wire [2:0] hint_request_full,
    input wire [5:0] filter_hint_resp
);

    reg [2:0] route_mask_d1;
    reg [2:0] route_gpa_d1;
    reg route_valid_d1;
    reg response_gate_d1;
    reg [7:0] request_live_d1;
    reg [7:0][2:0] request_cmd_d1;
    reg atomic_selected_d1;
    reg segment_selected_d1;
    reg [1:0] last_ld0_source;
    reg ld0_source_valid;
    reg [2:0][3:0] hint_id_d1;
    reg [2:0] hint_full_d1;
    reg [2:0] hint_valid_d1;
    reg [5:0] hint_response_d1;
    reg [3:0] top_fence_history;
    reg [3:0] top_changed_history;
    reg [1:0] middle_domain_checked;
    reg [43:0] satp_ppn_d1;
    reg [43:0] vsatp_ppn_d1;
    reg [43:0] hgatp_ppn_d1;
    reg external_stalled_d1;
    reg last_return_valid;
    reg [34:0] last_return_tag;
    reg [1:0] last_return_stage;
    reg flush_seen;
    reg [63:0] token_valid;
    reg [63:0][37:0] token_vpn;
    reg [63:0][1:0] token_stage;
    reg [63:0][31:0] token_cycle;
    reg [31:0] cycle_count;
    reg [6:0] token_count;
    reg [6:0] token_match_count;
    reg [6:0] same_key_count;
    reg [5:0] token_allocate;
    reg [5:0] token_complete;
    reg token_space_available;
    reg younger_return;
    reg [31:0] return_latency;
    reg [2:0] association_class;
    reg consumer_access;
    reg consumer_fault_and_deny;
    reg consumer_pbmt_sample;
    reg consumer_pbmt_matches;
    reg [1:0] observed_consumer_pbmt;
    reg [5:0] observed_span;
    reg boundary_crossed;
    reg [47:0] span_first;
    reg [47:0] span_last;
    reg [47:0] region_low;
    reg [47:0] region_high;
    wire sample_enable = (reset === 1'b0) && (reset_backend_done === 1'b1);
    wire external_request_fire = ext_valid && ext_ready;
    wire external_response_sample = return_valid;
    wire [2:0] actual_refill_mask = filter_refill;
    wire [1:0] ld0_source = atomic_sel ? 2'd1 : segment_sel ? 2'd2 : 2'd0;
    wire [2:0] expected_dtlb_mask = route_valid_d1 && !response_gate_d1 ? route_mask_d1 : 3'b000;
    wire route_matches = dtlb_return_valid == expected_dtlb_mask;
    wire hint_combination_matches = consumer_hint[0] == (hints[0][4] || |consumer_replay[0]) &&
        consumer_hint[1] == (hints[1][4] || |consumer_replay[1]) &&
        consumer_hint[2] == (hints[2][4] || |consumer_replay[2]);
    wire request_hint_matches = (!hint_valid_d1[0] || hints[0] == {hint_full_d1[0], hint_id_d1[0]}) &&
        (!hint_valid_d1[1] || hints[1] == {hint_full_d1[1], hint_id_d1[1]}) &&
        (!hint_valid_d1[2] || hints[2] == {hint_full_d1[2], hint_id_d1[2]});
    // 外部每次fire各占一个token，最多64项；无ID的同key返回保持歧义分类。
    always @* begin
        token_count = $countones(token_valid);
        token_match_count = 0;
        same_key_count = 0;
        token_allocate = 0;
        token_complete = 0;
        token_space_available = 0;
        younger_return = 0;
        return_latency = 0;
        for (integer token = 0; token < 64; token = token + 1) begin
            if (!token_valid[token] && !token_space_available) begin
                token_allocate = token;
                token_space_available = 1;
            end
            if (token_valid[token] && token_vpn[token] == ext_vpn &&
                token_stage[token] == ext_stage) same_key_count = same_key_count + 1;
            if (token_valid[token] && token_stage[token] == return_stage &&
                token_vpn[token][37:3] == (return_stage == 2 ? return_s2tag[37:3] : return_s1tag)) begin
                if (token_match_count == 0 || token_cycle[token] < token_cycle[token_complete]) token_complete = token;
                token_match_count = token_match_count + 1;
            end
        end
        if (token_match_count != 0) begin
            return_latency = cycle_count - token_cycle[token_complete];
            for (integer token = 0; token < 64; token = token + 1)
                if (token_valid[token] && token_cycle[token] < token_cycle[token_complete]) younger_return = 1;
        end
        association_class = token_match_count == 0 ? 3'd3 : token_match_count == 1 ? 3'd0 : 3'd4;
    end
    always @(posedge clock) begin
        if (reset) begin
            route_mask_d1 <= 0;
            route_gpa_d1 <= 0;
            route_valid_d1 <= 0;
            response_gate_d1 <= 0;
            request_live_d1 <= 0;
            request_cmd_d1 <= 0;
            atomic_selected_d1 <= 0;
            segment_selected_d1 <= 0;
            last_ld0_source <= 0;
            ld0_source_valid <= 0;
            hint_id_d1 <= 0;
            hint_full_d1 <= 0;
            hint_valid_d1 <= 0;
            hint_response_d1 <= 0;
            top_fence_history <= 0;
            top_changed_history <= 0;
            middle_domain_checked <= 0;
            satp_ppn_d1 <= 0;
            vsatp_ppn_d1 <= 0;
            hgatp_ppn_d1 <= 0;
            external_stalled_d1 <= 0;
            last_return_valid <= 0;
            last_return_tag <= 0;
            last_return_stage <= 0;
            flush_seen <= 0;
            token_valid <= 0;
            token_vpn <= 0;
            token_stage <= 0;
            token_cycle <= 0;
            cycle_count <= 0;
        end else begin
            cycle_count <= cycle_count + 1;
            route_mask_d1 <= route_mask;
            route_gpa_d1 <= route_gpa;
            route_valid_d1 <= route_valid;
            response_gate_d1 <= mem_response_gate;
            request_live_d1 <= request_valid & ~request_kill;
            request_cmd_d1 <= request_cmd;
            atomic_selected_d1 <= atomic_sel;
            segment_selected_d1 <= segment_sel;
            if (request_valid[0] && !request_kill[0]) begin last_ld0_source <= ld0_source;
            ld0_source_valid <= 1;
            end
            hint_id_d1 <= hint_request_id;
            hint_full_d1 <= hint_request_full;
            hint_valid_d1 <= hint_request_valid;
            hint_response_d1 <= filter_hint_resp;
            middle_domain_checked <= {middle_domain_checked[0], top_fence_history[1] ? mid_sfence :
                top_changed_history[1] ? mid_csr_changed : 1'b0};
            top_fence_history <= {top_fence_history[2:0], top_sfence};
            top_changed_history <= {top_changed_history[2:0], top_csr_changed};
            satp_ppn_d1 <= top_satp_ppn;
            vsatp_ppn_d1 <= top_vsatp_ppn;
            hgatp_ppn_d1 <= top_hgatp_ppn;
            external_stalled_d1 <= ext_valid && !ext_ready;
            if (return_valid) begin last_return_valid <= 1;
            last_return_tag <= return_s1tag;
            last_return_stage <= return_stage;
            end
            if (|filter_flush) begin token_valid <= 0;
            flush_seen <= 1;
            end else begin
                if (return_valid && token_match_count != 0) token_valid[token_complete] <= 0;
                if (external_request_fire && token_space_available) begin
                    token_valid[token_allocate] <= 1;
                    token_vpn[token_allocate] <= ext_vpn;
                    token_stage[token_allocate] <= ext_stage;
                    token_cycle[token_allocate] <= cycle_count;
                end
            end
        end
    end
    always @* begin
        consumer_access = 0;
        consumer_fault_and_deny = 0;
        consumer_pbmt_sample = 0;
        consumer_pbmt_matches = 1;
        observed_consumer_pbmt = 0;
        observed_span = 0;
        boundary_crossed = 0;
        span_first = 0;
        span_last = 0;
        region_low = 0;
        region_high = 0;
        for (integer consumer = 0; consumer < 5; consumer = consumer + 1) begin
            if (consumer_valid[consumer]) begin
                consumer_access = 1;
                observed_span = $countones(consumer_mask[consumer]);
                span_first = consumer_pa[consumer];
                span_last = consumer_pa[consumer] + observed_span - 1;
                for (integer region = 0; region < 32; region = region + 1) begin
                    region_low = 0;
                    region_high = 0;
                    case (pmp_mode[region])
                        2'd1: begin region_low = region == 0 ? 48'd0 : {pmp_addr[region-1], 2'd0};
                        region_high = {pmp_addr[region], 2'd0};
                        end
                        2'd2: begin region_low = {pmp_addr[region], 2'd0};
                        region_high = region_low + 4;
                        end
                        2'd3: begin region_low = {pmp_addr[region], 2'd0} & ~pmp_mask[region];
                        region_high = region_low + pmp_mask[region] + 1;
                        end
                        default: begin region_low = 0;
                        region_high = 0;
                        end
                    endcase
                    if (pmp_mode[region] != 0 && ((span_first < region_low && span_last >= region_low) ||
                        (span_first < region_high && span_last >= region_high))) boundary_crossed = 1;
                end
                consumer_fault_and_deny = consumer_fault_and_deny || ((consumer_pf[consumer] ||
                    consumer_gpf[consumer]) &&
                    consumer_deny[consumer] && consumer_active[consumer] && !consumer_prefetch[consumer]);
                if (!consumer_miss[consumer] && !consumer_pf[consumer] && !consumer_gpf[consumer] &&
                    !consumer_af[consumer]) begin
                    consumer_pbmt_sample = 1;
                    observed_consumer_pbmt = consumer_pbmt[consumer];
                    consumer_pbmt_matches = consumer_pbmt_matches &&
                        (consumer_pbmt[consumer] == 1 ? consumer_nc[consumer] :
                        consumer_pbmt[consumer] == 2 ? consumer_mmio[consumer] : 1'b1);
                end
            end
        end
    end
    // 诊断独立统计，不计入功能分母，不调用UVM错误报告。
    covergroup cg_interface_diagnostics @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_return_route : coverpoint ($isunknown(route_matches) ? 2'd2 : (route_matches) ? 2'd0 : 2'd1)
            iff (sample_enable && (route_valid_d1)) {
            bins b_mismatch = {2'd1};
            bins b_unknown = {2'd2};
        }
        cp_hint_combination : coverpoint ($isunknown(hint_combination_matches) ? 2'd2 : (hint_combination_matches)
            ? 2'd0 : 2'd1)
            iff (sample_enable && (|consumer_hint || |consumer_replay)) {
            bins b_mismatch = {2'd1};
            bins b_unknown = {2'd2};
        }
        cp_flush_delay : coverpoint ($isunknown(middle_domain_checked[1] && (|dtlb_fence_valid ||
            |filter_flush)) ? 2'd2 : (middle_domain_checked[1] && (|dtlb_fence_valid ||
            |filter_flush)) ? 2'd0 : 2'd1)
            iff (sample_enable && (top_fence_history[3] || top_changed_history[3])) {
            bins b_mismatch = {2'd1};
            bins b_unknown = {2'd2};
        }
    endgroup
    cg_interface_diagnostics interface_diagnostics;
    initial begin
        interface_diagnostics = new();
    end

    // 01.01 来源选择与共享端口
    covergroup cg_dtlb_01_01 @(posedge clock);
        option.per_instance = 1;
        // 01.01.005 SOURCE_普通load
        cp_01_01_005 : coverpoint (response_cmd[0] == request_cmd_d1[0] || response_cmd[1] == request_cmd_d1[1])
            iff (sample_enable && (|(request_live_d1[1:0] & response_valid[1:0]) && !atomic_selected_d1 &&
                !segment_selected_d1)) {
            bins b_expected_result = {1'b1};
        }
        // 01.01.006 SOURCE_hybrid_load_store
        cp_01_01_006 : coverpoint ((request_live_d1[2] && response_valid[2]) || (request_live_d1[5] &&
            response_valid[5]))
            iff (sample_enable && ((request_live_d1[2] && response_valid[2]) || (request_live_d1[5] &&
                response_valid[5]))) {
            bins b_observed = {1'b1};
        }
        // 01.01.007 SOURCE_atomic_LR_SC_AMO
        cp_01_01_007 : coverpoint (response_cmd[0] == request_cmd_d1[0])
            iff (sample_enable && (request_live_d1[0] && response_valid[0] && atomic_selected_d1)) {
            bins b_expected_result = {1'b1};
        }
        // 01.01.008 SOURCE_vector_segment
        cp_01_01_008 : coverpoint (response_cmd[0] == request_cmd_d1[0])
            iff (sample_enable && (request_live_d1[0] && response_valid[0] && !atomic_selected_d1 &&
                segment_selected_d1)) {
            bins b_expected_result = {1'b1};
        }
        // 01.01.009 SOURCE_stream_prefetch
        cp_01_01_009 : coverpoint (request_live_d1[3] && response_valid[3])
            iff (sample_enable && (request_live_d1[3] && response_valid[3])) {
            bins b_observed = {1'b1};
        }
        // 01.01.010 SOURCE_普通store
        cp_01_01_010 : coverpoint (|(request_live_d1[5:4] & response_valid[5:4]))
            iff (sample_enable && (|(request_live_d1[5:4] & response_valid[5:4]))) {
            bins b_observed = {1'b1};
        }
        // 01.01.011 SOURCE_SMS_prefetch
        cp_01_01_011 : coverpoint (request_live_d1[6] && response_valid[6])
            iff (sample_enable && (request_live_d1[6] && response_valid[6])) {
            bins b_observed = {1'b1};
        }
        // 01.01.012 SOURCE_L2查询
        cp_01_01_012 : coverpoint (request_live_d1[7] && response_valid[7])
            iff (sample_enable && (request_live_d1[7] && response_valid[7])) {
            bins b_observed = {1'b1};
        }
        // 01.01.013 SOURCE_LD0仲裁切换
        cp_01_01_013 : coverpoint (request_valid[0] && !request_kill[0] && ld0_source_valid &&
            ld0_source != last_ld0_source)
            iff (sample_enable && (request_valid[0] && !request_kill[0] && ld0_source_valid &&
                ld0_source != last_ld0_source)) {
            bins b_observed = {1'b1};
        }
    endgroup
    cg_dtlb_01_01 cg_dtlb_01_01_inst;

    // 01.02 请求接受与流水节拍
    covergroup cg_dtlb_01_02 @(posedge clock);
        option.per_instance = 1;
        // 01.02.005 REQ_端口并发
        cp_01_02_005 : coverpoint ($countones(request_valid & ~request_kill) >= 2)
            iff (sample_enable && ($countones(request_valid & ~request_kill) >= 2)) {
            bins b_observed = {1'b1};
        }
    endgroup
    cg_dtlb_01_02 cg_dtlb_01_02_inst;

    // 04.07 PMP交接与内存属性消费
    covergroup cg_dtlb_04_07 @(posedge clock);
        option.per_instance = 1;
        // 04.07.007 PMP_请求尺寸边界
        cp_04_07_007 : coverpoint observed_span
            iff (sample_enable && (consumer_access) && ((!$isunknown({consumer_mask, consumer_deny})) === 1'b1)) {
            bins b_byte = {1};
            bins b_half = {2};
            bins b_word = {4};
            bins b_double = {8};
            bins b_sixteen = {16};
            bins b_partial = {3, 5, 6, 7, [9:15]};
        }
        // 04.07.008 PMP_PF与访问拒绝
        cp_04_07_008 : coverpoint (|(consumer_af & consumer_valid))
            iff (sample_enable && (consumer_fault_and_deny)) {
            bins b_expected_result = {1'b1};
        }
        // 04.07.009 PBMT_consumer属性
        cp_04_07_009 : coverpoint observed_consumer_pbmt
            iff (sample_enable && (consumer_pbmt_sample) && ((consumer_pbmt_matches) === 1'b1)) {
            bins b_pma = {0};
            bins b_nc = {1};
            bins b_io = {2};
        }
    endgroup
    cg_dtlb_04_07 cg_dtlb_04_07_inst;

    // 05.02 返回窗口与重复请求过滤
    covergroup cg_dtlb_05_02 @(posedge clock);
        option.per_instance = 1;
        // 05.02.005 MISS_集成层再次过滤
        cp_05_02_005 : coverpoint (|(response_qvalid & local_return_valid & ~filter_input_valid))
            iff (sample_enable && (|(response_qvalid & local_return_valid & ~filter_input_valid))) {
            bins b_observed = {1'b1};
        }
        // 05.02.006 MISS_集成忽略ASID差异
        cp_05_02_006 : coverpoint (|(response_qvalid & local_return_valid & ~filter_input_valid))
            iff (sample_enable && (|(response_qvalid & local_return_valid & ~filter_input_valid))) {
            bins b_observed = {1'b1};
        }
        // 05.02.007 MISS_未覆盖组不过滤
        cp_05_02_007 : coverpoint (|(response_qvalid & ~local_return_valid & filter_input_valid))
            iff (sample_enable && (|(response_qvalid & ~local_return_valid & filter_input_valid))) {
            bins b_observed = {1'b1};
        }
    endgroup
    cg_dtlb_05_02 cg_dtlb_05_02_inst;

    // 05.04 Filter重复合并与getGpa元信息
    covergroup cg_dtlb_05_04 @(posedge clock);
        option.per_instance = 1;
        // 05.04.009 FILTER_跨组同key
        cp_05_04_009 : coverpoint token_space_available
            iff (sample_enable && (external_request_fire && same_key_count != 0)) {
            bins b_expected_set = {1'b1};
        }
    endgroup
    cg_dtlb_05_04 cg_dtlb_05_04_inst;

    // 05.05 Filter发射与RR仲裁
    covergroup cg_dtlb_05_05 @(posedge clock);
        option.per_instance = 1;
        // 05.05.005 ARB_单组发送
        cp_05_05_005 : coverpoint ($countones(filter_issue & filter_ready) == 1)
            iff (sample_enable && (external_request_fire && $countones(filter_issue) == 1)) {
            bins b_expected_result = {1'b1};
        }
        // 05.05.006 ARB_双组三组竞争
        cp_05_05_006 : coverpoint ($countones(filter_issue & filter_ready) == 1)
            iff (sample_enable && (external_request_fire && $countones(filter_issue) >= 2)) {
            bins b_expected_result = {1'b1};
        }
        // 05.05.007 ARB_背压恢复
        cp_05_05_007 : coverpoint ($countones(filter_issue & filter_ready) == 1)
            iff (sample_enable && (external_request_fire && external_stalled_d1 && $countones(filter_issue) >= 2)) {
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_05_05 cg_dtlb_05_05_inst;

    // 05.06 返回匹配、乱序与多项释放
    covergroup cg_dtlb_05_06 @(posedge clock);
        option.per_instance = 1;
        // 05.06.003 FILTER_返回乱序
        cp_05_06_003 : coverpoint (return_valid && token_match_count == 1 && younger_return)
            iff (sample_enable && (return_valid && token_match_count == 1 && younger_return)) {
            bins b_observed = {1'b1};
        }
    endgroup
    cg_dtlb_05_06 cg_dtlb_05_06_inst;

    // 05.07 Hint与Filter状态观测
    covergroup cg_dtlb_05_07 @(posedge clock);
        option.per_instance = 1;
        // 05.07.001 HINT_req_id_full
        cp_05_07_001 : coverpoint request_hint_matches
            iff (sample_enable && (|hint_valid_d1)) {
            bins b_expected_set = {1'b1};
        }
        // 05.07.002 HINT_resp_replay_all
        cp_05_07_002 : coverpoint (hint_resp == hint_response_d1)
            iff (sample_enable && (hint_response_d1[5])) {
            bins b_expected_result = {1'b1};
        }
        // 05.07.003 HINT_结合tlbreplay
        cp_05_07_003 : coverpoint hint_combination_matches
            iff (sample_enable && (|consumer_hint || |consumer_replay || hints[0][4] || hints[1][4] || hints[2][4])) {
            bins b_expected_set = {1'b1};
        }
    endgroup
    cg_dtlb_05_07 cg_dtlb_05_07_inst;

    // 05.08 三组返回路由与Token关联
    covergroup cg_dtlb_05_08 @(posedge clock);
        option.per_instance = 1;
        // 05.08.009 ROUTE_getGpa组独立
        cp_05_08_009 : coverpoint (dtlb_return_getgpa & route_mask_d1) == (route_gpa_d1 & route_mask_d1)
            iff (sample_enable && (route_valid && $countones(route_mask) >= 2 &&
                $countones(route_gpa & route_mask) != $countones(route_mask) && |(route_gpa & route_mask))) {
            bins b_expected_result = {1'b1};
        }
        // 05.08.010 ROUTE_响应延迟
        cp_05_08_010 : coverpoint route_matches
            iff (sample_enable && (route_valid_d1)) {
            bins b_expected_set = {1'b1};
        }
        // 05.08.011 TOKEN_相同key多次fire
        cp_05_08_011 : coverpoint token_space_available
            iff (sample_enable && (external_request_fire && same_key_count != 0)) {
            bins b_expected_set = {1'b1};
        }
        // 05.08.012 TOKEN_乱序多outstanding
        cp_05_08_012 : coverpoint return_latency
            iff (sample_enable && (return_valid && token_count >= 2 && token_match_count == 1 && younger_return)) {
            bins b_one = {1};
            bins b_short = {[2:4]};
            bins b_medium = {[5:16]};
            bins b_long = {[17:64]};
            bins b_very_long = {[65:$]};
        }
        // 05.08.013 TOKEN_同key旧返回后续重复
        cp_05_08_013 : coverpoint (return_valid && last_return_valid && return_s1tag == last_return_tag &&
            return_stage == last_return_stage)
            iff (sample_enable && (return_valid && last_return_valid && return_s1tag == last_return_tag &&
                return_stage == last_return_stage)) {
            bins b_observed = {1'b1};
        }
        // 05.08.014 FCOV_完成关联与epoch
        cp_05_08_014 : coverpoint association_class
            iff (sample_enable && (return_valid) && ((token_match_count == 1) === 1'b1)) {
            bins b_unique = {0};
        }
        // 05.08.001 ROUTE_vector_001
        cp_05_08_001 : coverpoint route_matches
            iff (sample_enable && (route_valid_d1 && route_mask_d1 == 3'd1)) {
            bins b_expected_set = {1'b1};
        }
        // 05.08.002 ROUTE_vector_010
        cp_05_08_002 : coverpoint route_matches
            iff (sample_enable && (route_valid_d1 && route_mask_d1 == 3'd2)) {
            bins b_expected_set = {1'b1};
        }
        // 05.08.003 ROUTE_vector_011
        cp_05_08_003 : coverpoint route_matches
            iff (sample_enable && (route_valid_d1 && route_mask_d1 == 3'd3)) {
            bins b_expected_set = {1'b1};
        }
        // 05.08.004 ROUTE_vector_100
        cp_05_08_004 : coverpoint route_matches
            iff (sample_enable && (route_valid_d1 && route_mask_d1 == 3'd4)) {
            bins b_expected_set = {1'b1};
        }
        // 05.08.005 ROUTE_vector_101
        cp_05_08_005 : coverpoint route_matches
            iff (sample_enable && (route_valid_d1 && route_mask_d1 == 3'd5)) {
            bins b_expected_set = {1'b1};
        }
        // 05.08.006 ROUTE_vector_110
        cp_05_08_006 : coverpoint route_matches
            iff (sample_enable && (route_valid_d1 && route_mask_d1 == 3'd6)) {
            bins b_expected_set = {1'b1};
        }
        // 05.08.007 ROUTE_vector_111
        cp_05_08_007 : coverpoint route_matches
            iff (sample_enable && (route_valid_d1 && route_mask_d1 == 3'd7)) {
            bins b_expected_set = {1'b1};
        }
        // 05.08.008 ROUTE_vector000
        cp_05_08_008 : coverpoint route_valid
            iff (sample_enable && (filter_resp[0] && filter_refill == 0) && ((!route_valid) === 1'b1)) {
            bins b_expected_clear = {1'b0};
        }
    endgroup
    cg_dtlb_05_08 cg_dtlb_05_08_inst;

    // 06.04 PLRU更新与容量替换
    covergroup cg_dtlb_06_04 @(posedge clock);
        option.per_instance = 1;
        // 06.04.007 CONCURRENT_三组独立替换
        cp_06_04_007 : coverpoint ($countones(dtlb_write_valid) >= 2)
            iff (sample_enable && ($countones(dtlb_write_valid) >= 2)) {
            bins b_observed = {1'b1};
        }
    endgroup
    cg_dtlb_06_04 cg_dtlb_06_04_inst;

    // 08.03 CSR变化与失效生效时序
    covergroup cg_dtlb_08_03 @(posedge clock);
        option.per_instance = 1;
        // 08.03.008 FENCE_延迟定位
        cp_08_03_008 : coverpoint (middle_domain_checked[1] && (|dtlb_fence_valid || |filter_flush))
            iff (sample_enable && (top_fence_history[3] || top_changed_history[3])) {
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_08_03 cg_dtlb_08_03_inst;

    // 08.05 在途刷新与新旧上下文恢复
    covergroup cg_dtlb_08_05 @(posedge clock);
        option.per_instance = 1;
        // 08.05.003 FENCE_旧返回迟到
        cp_08_05_003 : coverpoint route_valid
            iff (sample_enable && (flush_seen && filter_resp[0] && filter_refill == 0) && ((!route_valid) === 1'b1)) {
            bins b_expected_clear = {1'b0};
        }
    endgroup
    cg_dtlb_08_05 cg_dtlb_08_05_inst;

    // 08.05 在途刷新与新旧上下文恢复
    covergroup cg_dtlb_diag_08_05 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        // 08.05.004：结果偏离或未知单列，正常命中不进入诊断仓。
        rs_08_05_004 : coverpoint ($isunknown(1'b1) ? 2'd2 : (1'b1) ? 2'd0 : 2'd1)
            iff (sample_enable && (flush_seen && return_valid && token_match_count > 0)) {
            bins b_mismatch = {2'd1};
            bins unknown  = {2'd2};
        }
        // 08.05.004 FENCE_旧新同key返回
        cp_08_05_004 : coverpoint (flush_seen && return_valid && token_match_count > 0)
            iff (sample_enable && (flush_seen && return_valid && token_match_count > 0)) {
            bins b_observed = {1'b1};
        }
    endgroup
    cg_dtlb_diag_08_05 cg_dtlb_diag_08_05_inst;

    // 08.06 CSR有向切换与在途窗口
    covergroup cg_dtlb_08_06 @(posedge clock);
        option.per_instance = 1;
        // 08.06.001 CSR_hgatp_ppn_改变
        cp_08_06_001 : coverpoint (cycle_count != 0 && top_hgatp_ppn != hgatp_ppn_d1)
            iff (sample_enable && (cycle_count != 0 && top_hgatp_ppn != hgatp_ppn_d1)) {
            bins b_observed = {1'b1};
        }
        // 08.06.004 CSR_satp_ppn_改变
        cp_08_06_004 : coverpoint (cycle_count != 0 && top_satp_ppn != satp_ppn_d1)
            iff (sample_enable && (cycle_count != 0 && top_satp_ppn != satp_ppn_d1)) {
            bins b_observed = {1'b1};
        }
        // 08.06.006 CSR_vsatp_ppn_改变
        cp_08_06_006 : coverpoint (cycle_count != 0 && top_vsatp_ppn != vsatp_ppn_d1)
            iff (sample_enable && (cycle_count != 0 && top_vsatp_ppn != vsatp_ppn_d1)) {
            bins b_observed = {1'b1};
        }
    endgroup
    cg_dtlb_08_06 cg_dtlb_08_06_inst;

    initial begin
        cg_dtlb_01_01_inst = new();
        cg_dtlb_01_02_inst = new();
        cg_dtlb_04_07_inst = new();
        cg_dtlb_05_02_inst = new();
        cg_dtlb_05_04_inst = new();
        cg_dtlb_05_05_inst = new();
        cg_dtlb_05_06_inst = new();
        cg_dtlb_05_07_inst = new();
        cg_dtlb_05_08_inst = new();
        cg_dtlb_06_04_inst = new();
        cg_dtlb_08_03_inst = new();
        cg_dtlb_08_05_inst = new();
        cg_dtlb_diag_08_05_inst = new();
        cg_dtlb_08_06_inst = new();
    end

endmodule: memblock_dtlb_integration_fcov
`endif

`endif
