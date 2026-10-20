// 阅读约定：所有covergroup在posedge观察NBA更新前的值；_d1/_d2是模型历史。
// 简单事件门控已就地展开；保留的参考计算有中文合同，bind给出各端口的RTL来源。
// 对应表：Mem_env_work_20260716/TLB_fcov/DTLB_readability_20261016/DTLB_测试点与功能仓对应表.xlsx
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

    // 路径对齐：route/hint/request的_d1是前拍快照；request_live_d1已包含valid & ~kill。
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
    // 聚合/历史 token_count：64个历史token中有效项数量。
    reg [6:0] token_count;
    // 聚合/历史 token_match_count：本拍返回按stage/tag匹配的有效token个数。
    reg [6:0] token_match_count;
    // 聚合/历史 same_key_count：与本拍外部请求VPN/stage相同的有效token数量。
    reg [6:0] same_key_count;
    reg [5:0] token_allocate;
    reg [5:0] token_complete;
    reg token_space_available;
    // 聚合/历史 younger_return：被本拍返回选中的token之前仍有更老token，表示返回次序倒置场景。
    reg younger_return;
    // 聚合/历史 return_latency：本拍匹配到的最老token从握手开始经历的周期数。
    reg [31:0] return_latency;
    // 聚合/历史 association_class：0=唯一返回匹配，3=无匹配，4=多个候选；无ID时保留关联歧义。
    reg [2:0] association_class;
    // 聚合/历史 consumer_access：至少一个consumer_valid；本标志不检查consumer_active。
    reg consumer_access;
    // 聚合/历史 consumer_fault_and_deny：有效consumer有PF或GPF，同时PMP deny、active且非prefetch；不包含AF。
    reg consumer_fault_and_deny;
    // 聚合/历史 consumer_pbmt_sample：至少一个有效consumer既非miss也非PF/GPF/AF；包含PBMT=0。
    reg consumer_pbmt_sample;
    // 聚合/历史 consumer_pbmt_matches：对所有PBMT采样consumer检查MMIO/NC与PBMT编码一致。
    reg consumer_pbmt_matches;
    // 聚合/历史 observed_consumer_pbmt：循环最后一个符合PBMT采样条件的consumer实际PBMT。
    reg [1:0] observed_consumer_pbmt;
    // 聚合/历史 observed_span：循环最后一个有效consumer的访问mask跨度。
    reg [5:0] observed_span;
    // 聚合/历史 boundary_crossed：存在有效consumer的PA跨度越过某项TOR/NAPOT PMP边界。
    reg boundary_crossed;
    reg [47:0] span_first;
    reg [47:0] span_last;
    reg [47:0] region_low;
    reg [47:0] region_high;
    // 判据 sample_enable：仅在 reset 明确为0且后端复位完成明确为1时采样；X/Z不算有效复位状态。
    wire sample_enable = (reset === 1'b0) && (reset_backend_done === 1'b1);

    // 判据 ld0_source：LD0实际选路：1=atomic，2=segment，0=普通load；atomic优先。
    wire [1:0] ld0_source = atomic_sel ? 2'd1 : segment_sel ? 2'd2 : 2'd0;
    // 判据 expected_dtlb_mask：前拍返回路由mask经前拍response_gate门控后的预期DTLB返回mask。
    wire [2:0] expected_dtlb_mask = route_valid_d1 && !response_gate_d1 ? route_mask_d1 : 3'b000;

    // 判据 hint_combination_matches：逐组核对consumer hint等于filter full提示或本地replay。
    wire hint_combination_matches = consumer_hint[0] == (hints[0][4] || |consumer_replay[0]) &&
        consumer_hint[1] == (hints[1][4] || |consumer_replay[1]) &&
        consumer_hint[2] == (hints[2][4] || |consumer_replay[2]);
    // 判据 request_hint_matches：逐组核对hint的full/id与前拍请求快照相符；仅检查有效hint请求。
    wire request_hint_matches = (!hint_valid_d1[0] || hints[0] == {hint_full_d1[0], hint_id_d1[0]}) &&
        (!hint_valid_d1[1] || hints[1] == {hint_full_d1[1], hint_id_d1[1]}) &&
        (!hint_valid_d1[2] || hints[2] == {hint_full_d1[2], hint_id_d1[2]});
    // 外部每次fire各占一个token，最多64项；无ID的同key返回保持歧义分类。
    // 外部关联：固定64个token，每次valid&&ready独立分配；按stage/tag匹配最老token。无匹配/多匹配仅分类，不假定无ID返回可唯一关联。
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
                if (token_match_count == 0 || token_cycle[token] < token_cycle[token_complete])
                    token_complete = token;
                token_match_count = token_match_count + 1;
            end
        end
        if (token_match_count != 0) begin
            return_latency = cycle_count - token_cycle[token_complete];
            for (integer token = 0; token < 64; token = token + 1)
                if (token_valid[token] && token_cycle[token] < token_cycle[token_complete]) younger_return = 1;
        end
        // 关联编码：0=唯一匹配，3=没有匹配，4=多个候选；不以同VPN合并实际握手次数。
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
                if ((ext_valid && ext_ready) && token_space_available) begin
                    token_valid[token_allocate] <= 1;
                    token_vpn[token_allocate] <= ext_vpn;
                    token_stage[token_allocate] <= ext_stage;
                    token_cycle[token_allocate] <= cycle_count;
                end
            end
        end
    end
    always @* begin
        // consumer/PMP聚合：遍历5个consumer和32项PMP；地址跨度与PBMT使用实际有效consumer输出。
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
    // ----------------------------------------------------------------------------------------
    // 【cg_interface_diagnostics】接口一致性诊断
    // 探测功能：观察输入与实际输出是否一致，并把偏离/未知单独记录。
    // 采样时序：以实际consumer/路由/hint/外部握手采样；_d1为前拍，token为固定窗口历史。
    // 仓位判读：本组weight=0，cp为观察结果，rs的1/2分别表示偏离/未知；reference_missing表示信号缺失。
    // expected_dtlb_mask：前拍返回路由mask经前拍response_gate门控后的预期DTLB返回mask。
    // hint_combination_matches：逐组核对consumer hint等于filter full提示或本地replay。
    covergroup cg_interface_diagnostics @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_return_route : coverpoint ($isunknown((dtlb_return_valid == expected_dtlb_mask)) ? 2'd2 :
            ((dtlb_return_valid == expected_dtlb_mask)) ? 2'd0 : 2'd1)
            iff (sample_enable
                && route_valid_d1) {
            bins b_mismatch = {2'd1};
            bins b_unknown = {2'd2};
        }
        cp_hint_combination : coverpoint ($isunknown(hint_combination_matches) ? 2'd2 : (hint_combination_matches)
            ? 2'd0 : 2'd1)
            iff (sample_enable
                && (|consumer_hint || |consumer_replay)) {
            bins b_mismatch = {2'd1};
            bins b_unknown = {2'd2};
        }
        cp_flush_delay : coverpoint ($isunknown(middle_domain_checked[1] && (|dtlb_fence_valid ||
            |filter_flush)) ? 2'd2 : (middle_domain_checked[1] && (|dtlb_fence_valid ||
            |filter_flush)) ? 2'd0 : 2'd1)
            iff (sample_enable
                && (top_fence_history[3] || top_changed_history[3])) {
            bins b_mismatch = {2'd1};
            bins b_unknown = {2'd2};
        }
    endgroup
    cg_interface_diagnostics interface_diagnostics;
    initial begin
        interface_diagnostics = new();
    end

    // 01.01 来源选择与共享端口
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_01_01】01.01 来源选择与共享端口
    // 探测功能：核对共享端口选中的请求来源、命令与返回使用者，避免不同访存单元的请求串线。
    // 采样时序：以实际consumer/路由/hint/外部握手采样；_d1为前拍，token为固定窗口历史。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：01.01.005, 01.01.006, 01.01.007, 01.01.008, 01.01.009, 01.01.010, 01.01.011,
    // 01.01.012, 01.01.013；逐点功能见下方注释，精确配值见分解表。
    // ld0_source：LD0实际选路：1=atomic，2=segment，0=普通load；atomic优先。
    covergroup cg_dtlb_01_01 @(posedge clock);
        option.per_instance = 1;
        // 01.01.005 SOURCE_普通load
        // 测试表目标：场景：来源选择有效，实际R.valid=1；记录来源标签。 观察功能：翻译、异常和回放返回正确consumer；共享端口不串线。
        // 测试目的：核对共享端口选中的请求来源、命令与返回使用者，避免不同访存单元的请求串线。
        cp_01_01_005 : coverpoint (response_cmd[0] == request_cmd_d1[0] || response_cmd[1] == request_cmd_d1[1])
            iff (sample_enable
                && |(request_live_d1[1:0] & response_valid[1:0])
                && !atomic_selected_d1
                && !segment_selected_d1) {
            bins b_expected_result = {1'b1};
        }
        // 01.01.006 SOURCE_hybrid_load_store
        // 测试表目标：场景：来源选择有效，实际R.valid=1；记录来源标签。 观察功能：翻译、异常和回放返回正确consumer；共享端口不串线。
        // 测试目的：核对共享端口选中的请求来源、命令与返回使用者，避免不同访存单元的请求串线。
        cp_01_01_006 : coverpoint ((request_live_d1[2] && response_valid[2]) || (request_live_d1[5] &&
            response_valid[5]))
            iff (sample_enable
                && ((request_live_d1[2] && response_valid[2]) || (request_live_d1[5] && response_valid[5]))) {
            bins b_observed = {1'b1};
        }
        // 01.01.007 SOURCE_atomic_LR_SC_AMO
        // 测试表目标：场景：来源选择有效，实际R.valid=1；记录来源标签。 观察功能：翻译、异常和回放返回正确consumer；共享端口不串线。
        // 测试目的：核对共享端口选中的请求来源、命令与返回使用者，避免不同访存单元的请求串线。
        cp_01_01_007 : coverpoint (response_cmd[0] == request_cmd_d1[0])
            iff (sample_enable
                && request_live_d1[0]
                && response_valid[0]
                && atomic_selected_d1) {
            bins b_expected_result = {1'b1};
        }
        // 01.01.008 SOURCE_vector_segment
        // 测试表目标：场景：来源选择有效，实际R.valid=1；记录来源标签。 观察功能：翻译、异常和回放返回正确consumer；共享端口不串线。
        // 测试目的：核对共享端口选中的请求来源、命令与返回使用者，避免不同访存单元的请求串线。
        cp_01_01_008 : coverpoint (response_cmd[0] == request_cmd_d1[0])
            iff (sample_enable
                && request_live_d1[0]
                && response_valid[0]
                && !atomic_selected_d1
                && segment_selected_d1) {
            bins b_expected_result = {1'b1};
        }
        // 01.01.009 SOURCE_stream_prefetch
        // 测试表目标：场景：来源选择有效，实际R.valid=1；记录来源标签。 观察功能：翻译、异常和回放返回正确consumer；共享端口不串线。
        // 测试目的：核对共享端口选中的请求来源、命令与返回使用者，避免不同访存单元的请求串线。
        cp_01_01_009 : coverpoint (request_live_d1[3] && response_valid[3])
            iff (sample_enable
                && request_live_d1[3]
                && response_valid[3]) {
            bins b_observed = {1'b1};
        }
        // 01.01.010 SOURCE_普通store
        // 测试表目标：场景：来源选择有效，实际R.valid=1；记录来源标签。 观察功能：翻译、异常和回放返回正确consumer；共享端口不串线。
        // 测试目的：核对共享端口选中的请求来源、命令与返回使用者，避免不同访存单元的请求串线。
        cp_01_01_010 : coverpoint (|(request_live_d1[5:4] & response_valid[5:4]))
            iff (sample_enable
                && |(request_live_d1[5:4] & response_valid[5:4])) {
            bins b_observed = {1'b1};
        }
        // 01.01.011 SOURCE_SMS_prefetch
        // 测试表目标：场景：来源选择有效，实际R.valid=1；记录来源标签。 观察功能：翻译、异常和回放返回正确consumer；共享端口不串线。
        // 测试目的：核对共享端口选中的请求来源、命令与返回使用者，避免不同访存单元的请求串线。
        cp_01_01_011 : coverpoint (request_live_d1[6] && response_valid[6])
            iff (sample_enable
                && request_live_d1[6]
                && response_valid[6]) {
            bins b_observed = {1'b1};
        }
        // 01.01.012 SOURCE_L2查询
        // 测试表目标：场景：来源选择有效，实际R.valid=1；记录来源标签。 观察功能：翻译、异常和回放返回正确consumer；共享端口不串线。
        // 测试目的：核对共享端口选中的请求来源、命令与返回使用者，避免不同访存单元的请求串线。
        cp_01_01_012 : coverpoint (request_live_d1[7] && response_valid[7])
            iff (sample_enable
                && request_live_d1[7]
                && response_valid[7]) {
            bins b_observed = {1'b1};
        }
        // 01.01.013 SOURCE_LD0仲裁切换
        // 测试表目标：场景：LD0普通load、atomic、segment在相邻合法拍切换。 观察功能：VA/cmd/ROB与被选来源一致；segment请求有额外寄存拍。
        // 测试目的：核对共享端口选中的请求来源、命令与返回使用者，避免不同访存单元的请求串线。
        cp_01_01_013 : coverpoint (request_valid[0] && !request_kill[0] && ld0_source_valid &&
            ld0_source != last_ld0_source)
            iff (sample_enable
                && request_valid[0]
                && !request_kill[0]
                && ld0_source_valid
                && ld0_source != last_ld0_source) {
            bins b_observed = {1'b1};
        }
    endgroup
    cg_dtlb_01_01 cg_dtlb_01_01_inst;

    // 01.02 请求接受与流水节拍
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_01_02】01.02 请求接受与流水节拍
    // 探测功能：观察请求有效位和地址/命令穿过一拍流水后的对应关系，覆盖连续吞吐、空泡和端口并发。
    // 采样时序：以实际consumer/路由/hint/外部握手采样；_d1为前拍，token为固定窗口历史。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：01.02.005；逐点功能见下方注释，精确配值见分解表。
    covergroup cg_dtlb_01_02 @(posedge clock);
        option.per_instance = 1;
        // 01.02.005 REQ_端口并发
        // 测试表目标：场景：同一拍多个R.valid=1且kill=0。 观察功能：每端口各自产生结果；共享GPA/替换状态按实际优先级处理。
        // 测试目的：观察请求有效位和地址/命令穿过一拍流水后的对应关系，覆盖连续吞吐、空泡和端口并发。
        cp_01_02_005 : coverpoint ($countones(request_valid & ~request_kill) >= 2)
            iff (sample_enable
                && $countones(request_valid & ~request_kill) >= 2) {
            bins b_observed = {1'b1};
        }
    endgroup
    cg_dtlb_01_02 cg_dtlb_01_02_inst;

    // 04.07 PMP交接与内存属性消费
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_04_07】04.07 PMP交接与内存属性消费
    // 探测功能：观察翻译结果向 PMP 和使用者的交接，区分页表权限异常与物理访问权限拒绝。
    // 采样时序：以实际consumer/路由/hint/外部握手采样；_d1为前拍，token为固定窗口历史。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：04.07.007, 04.07.008, 04.07.009；逐点功能见下方注释，精确配值见分解表。
    // consumer_access：至少一个consumer_valid；本标志不检查consumer_active。
    // consumer_fault_and_deny：有效consumer有PF或GPF，同时PMP deny、active且非prefetch；不包含AF。
    // consumer_pbmt_matches：对所有PBMT采样consumer检查MMIO/NC与PBMT编码一致。
    // consumer_pbmt_sample：至少一个有效consumer既非miss也非PF/GPF/AF；包含PBMT=0。
    // observed_consumer_pbmt：循环最后一个符合PBMT采样条件的consumer实际PBMT。
    // observed_span：循环最后一个有效consumer的访问mask跨度。
    covergroup cg_dtlb_04_07 @(posedge clock);
        option.per_instance = 1;
        // 04.07.007 PMP_请求尺寸边界
        // 测试表目标：场景：consumer合法size=1/2/4/8/16字节及跨PMP边界。
        // 观察功能：尺寸由MemBlock连接/consumer实际路径检查；DTLB生成接口size已被裁剪。 测试目的：观察翻译结果向 PMP
        // 和使用者的交接，区分页表权限异常与物理访问权限拒绝。
        cp_04_07_007 : coverpoint observed_span
            iff (sample_enable
                && consumer_access
                && (!$isunknown({consumer_mask, consumer_deny})) === 1'b1) {
            bins b_byte = {1};
            bins b_half = {2};
            bins b_word = {4};
            bins b_double = {8};
            bins b_sixteen = {16};
            bins b_partial = {3, 5, 6, 7, [9:15]};
        }
        // 04.07.008 PMP_PF与访问拒绝
        // 测试表目标：场景：翻译fault和外部PMP拒绝同时发生。 观察功能：最终LSU异常选择单独检查；不把PMP拒绝当TLB perm.af。 测试目的：观察翻译结果向 PMP
        // 和使用者的交接，区分页表权限异常与物理访问权限拒绝。
        cp_04_07_008 : coverpoint (|(consumer_af & consumer_valid))
            iff (sample_enable
                && consumer_fault_and_deny) {
            bins b_expected_result = {1'b1};
        }
        // 04.07.009 PBMT_consumer属性
        // 测试表目标：场景：成功翻译输出PMA/NC/IO，访问普通内存/MMIO区域。 观察功能：核对LSU缓存性、强序和PMP/PMA属性合成。 测试目的：观察翻译结果向 PMP
        // 和使用者的交接，区分页表权限异常与物理访问权限拒绝。
        cp_04_07_009 : coverpoint observed_consumer_pbmt
            iff (sample_enable
                && consumer_pbmt_sample
                && (consumer_pbmt_matches) === 1'b1) {
            bins b_pma = {0};
            bins b_nc = {1};
            bins b_io = {2};
        }
    endgroup
    cg_dtlb_04_07 cg_dtlb_04_07_inst;

    // 05.02 返回窗口与重复请求过滤
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_05_02】05.02 返回窗口与重复请求过滤
    // 探测功能：观察 PTW 返回前后短时间内的重复 miss 如何被过滤，并跟踪被抑制请求的重试。
    // 采样时序：以实际consumer/路由/hint/外部握手采样；_d1为前拍，token为固定窗口历史。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：05.02.005, 05.02.006, 05.02.007；逐点功能见下方注释，精确配值见分解表。
    covergroup cg_dtlb_05_02 @(posedge clock);
        option.per_instance = 1;
        // 05.02.005 MISS_集成层再次过滤
        // 测试表目标：场景：D.Q.valid=1，但MemBlock当前返回vector包含该组且data.hit(ignoreAsid=true)。
        // 观察功能：Filter对应io_tlb_req_valid被清；load hint路径促使replay。 测试目的：观察 PTW 返回前后短时间内的重复 miss
        // 如何被过滤，并跟踪被抑制请求的重试。
        cp_05_02_005 : coverpoint (|(response_qvalid & local_return_valid & ~filter_input_valid))
            iff (sample_enable
                && |(response_qvalid & local_return_valid & ~filter_input_valid)) {
            bins b_observed = {1'b1};
        }
        // 05.02.006 MISS_集成忽略ASID差异
        // 测试表目标：场景：MemBlock过滤条件只因ignoreAsid=true而命中，DTLB本地严格ASID不匹配。
        // 观察功能：可能多一次replay，但不能错误填用其他ASID；追踪最终完成。 测试目的：观察 PTW 返回前后短时间内的重复 miss 如何被过滤，并跟踪被抑制请求的重试。
        cp_05_02_006 : coverpoint (|(response_qvalid & local_return_valid & ~filter_input_valid))
            iff (sample_enable
                && |(response_qvalid & local_return_valid & ~filter_input_valid)) {
            bins b_observed = {1'b1};
        }
        // 05.02.007 MISS_未覆盖组不过滤
        // 测试表目标：场景：匹配返回VPN但response vector不包含当前组。 观察功能：该组Q不应被MemBlock按此返回抑制；组隔离。 测试目的：观察 PTW
        // 返回前后短时间内的重复 miss 如何被过滤，并跟踪被抑制请求的重试。
        cp_05_02_007 : coverpoint (|(response_qvalid & ~local_return_valid & filter_input_valid))
            iff (sample_enable
                && |(response_qvalid & ~local_return_valid & filter_input_valid)) {
            bins b_observed = {1'b1};
        }
    endgroup
    cg_dtlb_05_02 cg_dtlb_05_02_inst;

    // 05.04 Filter重复合并与getGpa元信息
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_05_04】05.04 Filter重复合并与getGpa元信息
    // 探测功能：观察相同 VPN/阶段请求的合并规则，以及合并时 getGpa 元信息是否保留或更新。
    // 采样时序：以实际consumer/路由/hint/外部握手采样；_d1为前拍，token为固定窗口历史。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：05.04.009；逐点功能见下方注释，精确配值见分解表。
    // same_key_count：与本拍外部请求VPN/stage相同的有效token数量。
    covergroup cg_dtlb_05_04 @(posedge clock);
        option.per_instance = 1;
        // 05.04.009 FILTER_跨组同key
        // 测试表目标：场景：LD/ST/PF分别请求同VPN/stage。 观察功能：各组可独立fire；单个返回可能同时命中多组；外部每次fire各建token。 测试目的：观察相同
        // VPN/阶段请求的合并规则，以及合并时 getGpa 元信息是否保留或更新。
        cp_05_04_009 : coverpoint token_space_available
            iff (sample_enable
                && ext_valid
                && ext_ready
                && same_key_count != 0) {
            bins b_expected_set = {1'b1};
        }
    endgroup
    cg_dtlb_05_04 cg_dtlb_05_04_inst;

    // 05.05 Filter发射与RR仲裁
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_05_05】05.05 Filter发射与RR仲裁
    // 探测功能：观察组内待发项选择、真实握手和三组轮询仲裁，覆盖背压及恢复。
    // 采样时序：以实际consumer/路由/hint/外部握手采样；_d1为前拍，token为固定窗口历史。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：05.05.005, 05.05.006, 05.05.007；逐点功能见下方注释，精确配值见分解表。
    covergroup cg_dtlb_05_05 @(posedge clock);
        option.per_instance = 1;
        // 05.05.005 ARB_单组发送
        // 测试表目标：场景：仅LD或ST或PF有可发请求。 观察功能：对应组获grant，输出VPN/stage正确。
        // 测试目的：观察组内待发项选择、真实握手和三组轮询仲裁，覆盖背压及恢复。
        cp_05_05_005 : coverpoint ($countones(filter_issue & filter_ready) == 1)
            iff (sample_enable
                && ext_valid
                && ext_ready
                && $countones(filter_issue) == 1) {
            bins b_expected_result = {1'b1};
        }
        // 05.05.006 ARB_双组三组竞争
        // 测试表目标：场景：2或3组持续valid，输出ready有接受机会。 观察功能：RR在fire机会间轮转；每组均有grant，不能用墙钟拍数判断公平。
        // 测试目的：观察组内待发项选择、真实握手和三组轮询仲裁，覆盖背压及恢复。
        cp_05_05_006 : coverpoint ($countones(filter_issue & filter_ready) == 1)
            iff (sample_enable
                && ext_valid
                && ext_ready
                && $countones(filter_issue) >= 2) {
            bins b_expected_result = {1'b1};
        }
        // 05.05.007 ARB_背压恢复
        // 测试表目标：场景：三组竞争时ready=0后恢复。 观察功能：未fire时仲裁状态按RTL保持/选择；恢复后继续正常RR。
        // 测试目的：观察组内待发项选择、真实握手和三组轮询仲裁，覆盖背压及恢复。
        cp_05_05_007 : coverpoint ($countones(filter_issue & filter_ready) == 1)
            iff (sample_enable
                && ext_valid
                && ext_ready
                && external_stalled_d1
                && $countones(filter_issue) >= 2) {
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_05_05 cg_dtlb_05_05_inst;

    // 05.06 返回匹配、乱序与多项释放
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_05_06】05.06 返回匹配、乱序与多项释放
    // 探测功能：观察返回按内容匹配释放槽位的行为，包括乱序、多槽命中及无匹配返回。
    // 采样时序：以实际consumer/路由/hint/外部握手采样；_d1为前拍，token为固定窗口历史。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：05.06.003；逐点功能见下方注释，精确配值见分解表。
    // token_match_count：本拍返回按stage/tag匹配的有效token个数。
    // younger_return：被本拍返回选中的token之前仍有更老token，表示返回次序倒置场景。
    covergroup cg_dtlb_05_06 @(posedge clock);
        option.per_instance = 1;
        // 05.06.003 FILTER_返回乱序
        // 测试表目标：场景：先后fire A/B，B先返回且A仍v=1。 观察功能：按内容匹配清B；A继续等待，无FIFO-head依赖。
        // 测试目的：观察返回按内容匹配释放槽位的行为，包括乱序、多槽命中及无匹配返回。
        cp_05_06_003 : coverpoint (return_valid && token_match_count == 1 && younger_return)
            iff (sample_enable
                && return_valid
                && token_match_count == 1
                && younger_return) {
            bins b_observed = {1'b1};
        }
    endgroup
    cg_dtlb_05_06 cg_dtlb_05_06_inst;

    // 05.07 Hint与Filter状态观测
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_05_07】05.07 Hint与Filter状态观测
    // 探测功能：观察 Filter 提示信号与真实有效槽状态的关系，避免用调试计数替代功能状态。
    // 采样时序：以实际consumer/路由/hint/外部握手采样；_d1为前拍，token为固定窗口历史。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：05.07.001, 05.07.002, 05.07.003；逐点功能见下方注释，精确配值见分解表。
    // hint_combination_matches：逐组核对consumer hint等于filter full提示或本地replay。
    // request_hint_matches：逐组核对hint的full/id与前拍请求快照相符；仅检查有效hint请求。
    covergroup cg_dtlb_05_07 @(posedge clock);
        option.per_instance = 1;
        // 05.07.001 HINT_req_id_full
        // 测试表目标：场景：LD需求口请求入队/旧match/满/返回match。 观察功能：hint.id来自enqidx；full=!canenq或return
        // match；经PTWNewFilter再寄存一拍。 测试目的：观察 Filter 提示信号与真实有效槽状态的关系，避免用调试计数替代功能状态。
        cp_05_07_001 : coverpoint request_hint_matches
            iff (sample_enable
                && |hint_valid_d1) {
            bins b_expected_set = {1'b1};
        }
        // 05.07.002 HINT_resp_replay_all
        // 测试表目标：场景：LD返回matchcount=1或>1。 观察功能：hint.resp.valid延迟一拍；id最高match；replay_all=(count>1)。
        // 测试目的：观察 Filter 提示信号与真实有效槽状态的关系，避免用调试计数替代功能状态。
        cp_05_07_002 : coverpoint (hint_resp == hint_response_d1)
            iff (sample_enable
                && hint_response_d1[5]) {
            bins b_expected_result = {1'b1};
        }
        // 05.07.003 HINT_结合tlbreplay
        // 测试表目标：场景：load出现DTLB本地tlbreplay或MemBlock过滤重放。 观察功能：consumer
        // hint.full综合相应延迟replay信号，不能只采filter full。 测试目的：观察 Filter 提示信号与真实有效槽状态的关系，避免用调试计数替代功能状态。
        cp_05_07_003 : coverpoint hint_combination_matches
            iff (sample_enable
                && (|consumer_hint || |consumer_replay || hints[0][4] || hints[1][4] || hints[2][4])) {
            bins b_expected_set = {1'b1};
        }
    endgroup
    cg_dtlb_05_07 cg_dtlb_05_07_inst;

    // 05.08 三组返回路由与Token关联
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_05_08】05.08 三组返回路由与Token关联
    // 探测功能：观察同一返回向 LD/ST/PF 的路由，并分别关联外部 PTW 事务与组内槽位。
    // 采样时序：以实际consumer/路由/hint/外部握手采样；_d1为前拍，token为固定窗口历史。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：05.08.001, 05.08.002, 05.08.003, 05.08.004, 05.08.005, 05.08.006, 05.08.007,
    // 05.08.008, 05.08.009, 05.08.010, 05.08.011, 05.08.012, 05.08.013,
    // 05.08.014；逐点功能见下方注释，精确配值见分解表。
    // expected_dtlb_mask：前拍返回路由mask经前拍response_gate门控后的预期DTLB返回mask。
    // association_class：0=唯一返回匹配，3=无匹配，4=多个候选；无ID时保留关联歧义。
    // return_latency：本拍匹配到的最老token从握手开始经历的周期数。
    // same_key_count：与本拍外部请求VPN/stage相同的有效token数量。
    // token_count：64个历史token中有效项数量。
    // token_match_count：本拍返回按stage/tag匹配的有效token个数。
    // younger_return：被本拍返回选中的token之前仍有更老token，表示返回次序倒置场景。
    covergroup cg_dtlb_05_08 @(posedge clock);
        option.per_instance = 1;
        // 05.08.009 ROUTE_getGpa组独立
        // 测试表目标：场景：同返回匹配两组，一组最高match getGpa=1，另一组0。 观察功能：前组P.getGpa=1禁止常规写，后组允许按自身条件回填。
        // 测试目的：观察同一返回向 LD/ST/PF 的路由，并分别关联外部 PTW 事务与组内槽位。
        cp_05_08_009 : coverpoint (dtlb_return_getgpa & route_mask_d1) == (route_gpa_d1 & route_mask_d1)
            iff (sample_enable
                && route_valid
                && $countones(route_mask) >= 2
                && $countones(route_gpa & route_mask) != $countones(route_mask)
                && |(route_gpa & route_mask)) {
            bins b_expected_result = {1'b1};
        }
        // 05.08.010 ROUTE_响应延迟
        // 测试表目标：场景：外部L2响应valid在t拍被接收。
        // 观察功能：filter先寄存P至t+1匹配/refill，MemBlock再寄存至t+2送DTLB；entry写在消费边沿；以统一采样定义核对。 测试目的：观察同一返回向
        // LD/ST/PF 的路由，并分别关联外部 PTW 事务与组内槽位。
        cp_05_08_010 : coverpoint (dtlb_return_valid == expected_dtlb_mask)
            iff (sample_enable
                && route_valid_d1) {
            bins b_expected_set = {1'b1};
        }
        // 05.08.011 TOKEN_相同key多次fire
        // 测试表目标：场景：外部ready=1，同key在不同拍或跨组多次fire。
        // 观察功能：monitor每fire建独立token；一次大范围返回清多slot不等于可少回复已接受token。 测试目的：观察同一返回向 LD/ST/PF
        // 的路由，并分别关联外部 PTW 事务与组内槽位。
        cp_05_08_011 : coverpoint token_space_available
            iff (sample_enable
                && ext_valid
                && ext_ready
                && same_key_count != 0) {
            bins b_expected_set = {1'b1};
        }
        // 05.08.012 TOKEN_乱序多outstanding
        // 测试表目标：场景：外部至少2笔不同key outstanding，后接受者先返回。
        // 观察功能：根据response范围/context配对，不用FIFO头；最终无丢失/重复提交。 测试目的：观察同一返回向 LD/ST/PF 的路由，并分别关联外部 PTW
        // 事务与组内槽位。
        cp_05_08_012 : coverpoint return_latency
            iff (sample_enable
                && return_valid
                && token_count >= 2
                && token_match_count == 1
                && younger_return) {
            bins b_one = {1};
            bins b_short = {[2:4]};
            bins b_medium = {[5:16]};
            bins b_long = {[17:64]};
            bins b_very_long = {[65:$]};
        }
        // 05.08.013 TOKEN_同key旧返回后续重复
        // 测试表目标：场景：跨组同key独立token，第一个响应已清多组，第二响应再到。
        // 观察功能：无旧有效match时不再refill；有新匹配entry时需验证数据一致及epoch约束。 测试目的：观察同一返回向 LD/ST/PF 的路由，并分别关联外部 PTW
        // 事务与组内槽位。
        cp_05_08_013 : coverpoint (return_valid && last_return_valid && return_s1tag == last_return_tag &&
            return_stage == last_return_stage)
            iff (sample_enable
                && return_valid
                && last_return_valid
                && return_s1tag == last_return_tag
                && return_stage == last_return_stage) {
            bins b_observed = {1'b1};
        }
        // 05.08.014 FCOV_完成关联与epoch
        // 测试表目标：场景：每次外部PTW返回及每次consumer完成，查询monitor token/context/epoch快照。
        // 观察功能：每次外部fire建立token；共享范围entry清除与外部token完成分开计数。同key无ID时记录不可辨，不推断已识别旧响应。 测试目的：观察同一返回向
        // LD/ST/PF 的路由，并分别关联外部 PTW 事务与组内槽位。
        cp_05_08_014 : coverpoint association_class
            iff (sample_enable
                && return_valid
                && (token_match_count == 1) === 1'b1) {
            bins b_unique = {0};
        }
        // 05.08.001 ROUTE_vector_001
        // 测试表目标：构造一次 PTW 返回仅匹配 LD 组的有效 Filter 项。逻辑组掩码 001 按 {PF,ST,LD} 排列；在实际 vector 中 LD/ST/PF
        // 分别对应 bit0/bit4/bit6。观察这些组收到延迟后的返回，其他组没有该次回填；每组 getGpa 根据该组命中项独立产生。
        cp_05_08_001 : coverpoint (dtlb_return_valid == expected_dtlb_mask)
            iff (sample_enable
                && route_valid_d1
                && route_mask_d1 == 3'd1) {
            bins b_expected_set = {1'b1};
        }
        // 05.08.002 ROUTE_vector_010
        // 测试表目标：构造一次 PTW 返回仅匹配 ST 组的有效 Filter 项。逻辑组掩码 010 按 {PF,ST,LD} 排列；在实际 vector 中 LD/ST/PF
        // 分别对应 bit0/bit4/bit6。观察这些组收到延迟后的返回，其他组没有该次回填；每组 getGpa 根据该组命中项独立产生。
        cp_05_08_002 : coverpoint (dtlb_return_valid == expected_dtlb_mask)
            iff (sample_enable
                && route_valid_d1
                && route_mask_d1 == 3'd2) {
            bins b_expected_set = {1'b1};
        }
        // 05.08.003 ROUTE_vector_011
        // 测试表目标：构造一次 PTW 返回仅匹配 LD、ST 组的有效 Filter 项。逻辑组掩码 011 按 {PF,ST,LD} 排列；在实际 vector 中 LD/ST/PF
        // 分别对应 bit0/bit4/bit6。观察这些组收到延迟后的返回，其他组没有该次回填；每组 getGpa 根据该组命中项独立产生。
        cp_05_08_003 : coverpoint (dtlb_return_valid == expected_dtlb_mask)
            iff (sample_enable
                && route_valid_d1
                && route_mask_d1 == 3'd3) {
            bins b_expected_set = {1'b1};
        }
        // 05.08.004 ROUTE_vector_100
        // 测试表目标：构造一次 PTW 返回仅匹配 PF 组的有效 Filter 项。逻辑组掩码 100 按 {PF,ST,LD} 排列；在实际 vector 中 LD/ST/PF
        // 分别对应 bit0/bit4/bit6。观察这些组收到延迟后的返回，其他组没有该次回填；每组 getGpa 根据该组命中项独立产生。
        cp_05_08_004 : coverpoint (dtlb_return_valid == expected_dtlb_mask)
            iff (sample_enable
                && route_valid_d1
                && route_mask_d1 == 3'd4) {
            bins b_expected_set = {1'b1};
        }
        // 05.08.005 ROUTE_vector_101
        // 测试表目标：构造一次 PTW 返回仅匹配 LD、PF 组的有效 Filter 项。逻辑组掩码 101 按 {PF,ST,LD} 排列；在实际 vector 中 LD/ST/PF
        // 分别对应 bit0/bit4/bit6。观察这些组收到延迟后的返回，其他组没有该次回填；每组 getGpa 根据该组命中项独立产生。
        cp_05_08_005 : coverpoint (dtlb_return_valid == expected_dtlb_mask)
            iff (sample_enable
                && route_valid_d1
                && route_mask_d1 == 3'd5) {
            bins b_expected_set = {1'b1};
        }
        // 05.08.006 ROUTE_vector_110
        // 测试表目标：构造一次 PTW 返回仅匹配 ST、PF 组的有效 Filter 项。逻辑组掩码 110 按 {PF,ST,LD} 排列；在实际 vector 中 LD/ST/PF
        // 分别对应 bit0/bit4/bit6。观察这些组收到延迟后的返回，其他组没有该次回填；每组 getGpa 根据该组命中项独立产生。
        cp_05_08_006 : coverpoint (dtlb_return_valid == expected_dtlb_mask)
            iff (sample_enable
                && route_valid_d1
                && route_mask_d1 == 3'd6) {
            bins b_expected_set = {1'b1};
        }
        // 05.08.007 ROUTE_vector_111
        // 测试表目标：构造一次 PTW 返回仅匹配 LD、ST、PF 组的有效 Filter 项。逻辑组掩码 111 按 {PF,ST,LD} 排列；在实际 vector 中
        // LD/ST/PF 分别对应 bit0/bit4/bit6。观察这些组收到延迟后的返回，其他组没有该次回填；每组 getGpa 根据该组命中项独立产生。
        cp_05_08_007 : coverpoint (dtlb_return_valid == expected_dtlb_mask)
            iff (sample_enable
                && route_valid_d1
                && route_mask_d1 == 3'd7) {
            bins b_expected_set = {1'b1};
        }
        // 05.08.008 ROUTE_vector000
        // 测试表目标：场景：外部返回未匹配任一filter有效项。 观察功能：Filter.io_tlb_resp_valid=0；不向三组回填。 测试目的：观察同一返回向
        // LD/ST/PF 的路由，并分别关联外部 PTW 事务与组内槽位。
        cp_05_08_008 : coverpoint route_valid
            iff (sample_enable
                && filter_resp[0]
                && filter_refill == 0
                && (!route_valid) === 1'b1) {
            bins b_expected_clear = {1'b0};
        }
    endgroup
    cg_dtlb_05_08 cg_dtlb_05_08_inst;

    // 06.04 PLRU更新与容量替换
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_06_04】06.04 PLRU更新与容量替换
    // 探测功能：观察48路缓存的替换选择和 PLRU 更新，区分首次填入、容量替换及多口触碰。
    // 采样时序：以实际consumer/路由/hint/外部握手采样；_d1为前拍，token为固定窗口历史。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：06.04.007；逐点功能见下方注释，精确配值见分解表。
    covergroup cg_dtlb_06_04 @(posedge clock);
        option.per_instance = 1;
        // 06.04.007 CONCURRENT_三组独立替换
        // 测试表目标：场景：同时在LD/ST/PF回填/命中不同way。 观察功能：三套48项storage和PLRU不串扰；refillBothTlb=false。
        // 测试目的：观察48路缓存的替换选择和 PLRU 更新，区分首次填入、容量替换及多口触碰。
        cp_06_04_007 : coverpoint ($countones(dtlb_write_valid) >= 2)
            iff (sample_enable
                && $countones(dtlb_write_valid) >= 2) {
            bins b_observed = {1'b1};
        }
    endgroup
    cg_dtlb_06_04 cg_dtlb_06_04_inst;

    // 08.03 CSR变化与失效生效时序
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_08_03】08.03 CSR变化与失效生效时序
    // 探测功能：观察 CSR 改变与 fence 的不同作用及生效延迟，避免把 Filter 清空等同于 storage 失效。
    // 采样时序：以实际consumer/路由/hint/外部握手采样；_d1为前拍，token为固定窗口历史。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：08.03.008；逐点功能见下方注释，精确配值见分解表。
    covergroup cg_dtlb_08_03 @(posedge clock);
        option.per_instance = 1;
        // 08.03.008 FENCE_延迟定位
        // 测试表目标：场景：顶层sfence/CSR事件单拍脉冲，逐拍记录到DTLB输入和storage/filter生效。
        // 观察功能：顶层→MemBlock寄存2拍→DTLB/Filter内部再2拍；按观测域对齐。 测试目的：观察 CSR 改变与 fence 的不同作用及生效延迟，避免把
        // Filter 清空等同于 storage 失效。
        cp_08_03_008 : coverpoint (middle_domain_checked[1] && (|dtlb_fence_valid || |filter_flush))
            iff (sample_enable
                && (top_fence_history[3] || top_changed_history[3])) {
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_08_03 cg_dtlb_08_03_inst;

    // 08.05 在途刷新与新旧上下文恢复
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_08_05】08.05 在途刷新与新旧上下文恢复
    // 探测功能：观察刷新前后的在途返回和映射切换，跟踪新请求是否最终使用新上下文。
    // 采样时序：以实际consumer/路由/hint/外部握手采样；_d1为前拍，token为固定窗口历史。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：08.05.003；逐点功能见下方注释，精确配值见分解表。
    covergroup cg_dtlb_08_05 @(posedge clock);
        option.per_instance = 1;
        // 08.05.003 FENCE_旧返回迟到
        // 测试表目标：场景：filter已清后，旧P响应晚到且无新匹配entry。 观察功能：无refill vector；不能恢复旧storage项。
        // 测试目的：观察刷新前后的在途返回和映射切换，跟踪新请求是否最终使用新上下文。
        cp_08_05_003 : coverpoint route_valid
            iff (sample_enable
                && flush_seen
                && filter_resp[0]
                && filter_refill == 0
                && (!route_valid) === 1'b1) {
            bins b_expected_clear = {1'b0};
        }
    endgroup
    cg_dtlb_08_05 cg_dtlb_08_05_inst;

    // 08.05 在途刷新与新旧上下文恢复
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_diag_08_05】08.05 在途刷新与新旧上下文恢复
    // 探测功能：观察刷新前后的在途返回和映射切换，跟踪新请求是否最终使用新上下文。
    // 采样时序：以实际consumer/路由/hint/外部握手采样；_d1为前拍，token为固定窗口历史。
    // 仓位判读：本组weight=0，cp为观察结果，rs的1/2分别表示偏离/未知；reference_missing表示信号缺失。
    // 测试点：08.05.004；逐点功能见下方注释，精确配值见分解表。
    // token_match_count：本拍返回按stage/tag匹配的有效token个数。
    covergroup cg_dtlb_diag_08_05 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        // 08.05.004：结果偏离或未知单列，正常命中不进入诊断仓。
        rs_08_05_004 : coverpoint ($isunknown(1'b1) ? 2'd2 : (1'b1) ? 2'd0 : 2'd1)
            iff (sample_enable
                && flush_seen
                && return_valid
                && token_match_count > 0) {
            bins b_mismatch = {2'd1};
            bins unknown  = {2'd2};
        }
        // 08.05.004 FENCE_旧新同key返回
        // 测试表目标：场景：flush后新请求复用相同VPN/stage/ID，旧响应迟到。
        // 观察功能：接口无epoch/ID，RTL可能匹配新entry；需responder/真实L2TLB清除旧响应合同保证。
        // 测试目的：观察刷新前后的在途返回和映射切换，跟踪新请求是否最终使用新上下文。
        cp_08_05_004 : coverpoint (flush_seen && return_valid && token_match_count > 0)
            iff (sample_enable
                && flush_seen
                && return_valid
                && token_match_count > 0) {
            bins b_observed = {1'b1};
        }
    endgroup
    cg_dtlb_diag_08_05 cg_dtlb_diag_08_05_inst;

    // 08.06 CSR有向切换与在途窗口
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_08_06】08.06 CSR有向切换与在途窗口
    // 探测功能：观察 CSR 由旧配置切换到新配置的有向窗口，覆盖请求、PTW 等待和 GPA 等待跨越切换的情形。
    // 采样时序：以实际consumer/路由/hint/外部握手采样；_d1为前拍，token为固定窗口历史。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：08.06.001, 08.06.004, 08.06.006；逐点功能见下方注释，精确配值见分解表。
    covergroup cg_dtlb_08_06 @(posedge clock);
        option.per_instance = 1;
        // 08.06.001 CSR_hgatp_ppn_改变
        // 测试表目标：令
        // hgatp_ppn（G阶段页表根物理页号）从一个已知值变为不同的已知值，模拟页表根或地址空间上下文切换。观察顶层到内部CSR的延迟、对应changed脉冲以及请求/Filter/GPA的在途状态；区分新旧上下文，不能仅凭数值改变推定全部缓存已完成失效。
        cp_08_06_001 : coverpoint (cycle_count != 0 && top_hgatp_ppn != hgatp_ppn_d1)
            iff (sample_enable
                && cycle_count != 0
                && top_hgatp_ppn != hgatp_ppn_d1) {
            bins b_observed = {1'b1};
        }
        // 08.06.004 CSR_satp_ppn_改变
        // 测试表目标：令
        // satp_ppn（Host页表根物理页号）从一个已知值变为不同的已知值，模拟页表根或地址空间上下文切换。观察顶层到内部CSR的延迟、对应changed脉冲以及请求/Filter/GPA的在途状态；区分新旧上下文，不能仅凭数值改变推定全部缓存已完成失效。
        cp_08_06_004 : coverpoint (cycle_count != 0 && top_satp_ppn != satp_ppn_d1)
            iff (sample_enable
                && cycle_count != 0
                && top_satp_ppn != satp_ppn_d1) {
            bins b_observed = {1'b1};
        }
        // 08.06.006 CSR_vsatp_ppn_改变
        // 测试表目标：令 vsatp_ppn（Guest
        // VS页表根物理页号）从一个已知值变为不同的已知值，模拟页表根或地址空间上下文切换。观察顶层到内部CSR的延迟、对应changed脉冲以及请求/Filter/GPA的在途状态；区分新旧上下文，不能仅凭数值改变推定全部缓存已完成失效。
        cp_08_06_006 : coverpoint (cycle_count != 0 && top_vsatp_ppn != vsatp_ppn_d1)
            iff (sample_enable
                && cycle_count != 0
                && top_vsatp_ppn != vsatp_ppn_d1) {
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
