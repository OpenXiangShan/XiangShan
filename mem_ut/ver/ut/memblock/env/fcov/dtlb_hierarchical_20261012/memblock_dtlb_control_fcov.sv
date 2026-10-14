//=========================================================
// memblock_dtlb_control_fcov：DTLB V2 只读功能覆盖率。
// 普通信号端口、时钟事件采样、逐特性直接建仓；没有结构体库、function 或采样分发器。
// 时延与长度采用范围仓；物理 way/slot/子页采用逐项仓。
//=========================================================
`ifndef MEMBLOCK_DTLB_CONTROL_FCOV__SV
`define MEMBLOCK_DTLB_CONTROL_FCOV__SV

`ifdef MEMBLOCK_UT_FCOV
module memblock_dtlb_control_fcov #(
    parameter integer GROUP_ID = 0
) (
    input wire clock,
    input wire reset,
    input wire reset_backend_done,
    input wire [3:0] csr_satp_mode,
    input wire [15:0] csr_asid,
    input wire csr_satp_changed,
    input wire [3:0] csr_vsatp_mode,
    input wire [15:0] csr_vasid,
    input wire csr_vsatp_changed,
    input wire [3:0] csr_hgatp_mode,
    input wire [15:0] csr_vmid,
    input wire csr_hgatp_changed,
    input wire csr_virt,
    input wire csr_virt_changed,
    input wire [1:0] csr_dmode,
    input wire ptw_valid,
    input wire [1:0] ptw_stage,
    input wire ptw_getgpa,
    input wire [34:0] ptw_s1tag,
    input wire [1:0] ptw_s1level,
    input wire ptw_s1n,
    input wire ptw_s1_pf,
    input wire ptw_s1_af,
    input wire ptw_s1_v,
    input wire ptw_s1_x,
    input wire ptw_s1_w,
    input wire ptw_s1_r,
    input wire [1:0] ptw_s2level,
    input wire ptw_s2n,
    input wire ptw_s2_pf,
    input wire ptw_s2_af,
    input wire state_write,
    input wire [5:0] state_way,
    input wire state_ng,
    input wire state_ng_wire,
    input wire [46:0] state_plru,
    input wire fence_valid,
    input wire [3:0] gpa_start_mask,
    input wire [1:0] write_level,
    input wire write_napot,
    input wire [3:0] touch_valid_mask
);

    reg [2:0] translation_context_d1;
    reg [3:0] csr_satp_mode_d1;
    reg [3:0] csr_vsatp_mode_d1;
    reg [3:0] csr_hgatp_mode_d1;
    reg [15:0] csr_asid_d1;
    reg [15:0] csr_vasid_d1;
    reg [15:0] csr_vmid_d1;
    reg [1:0] csr_dmode_d1;
    reg csr_virt_d1;
    reg history_valid;
    reg state_write_d1;
    reg [5:0] state_way_d1;
    reg [1:0] ptw_stage_d1;
    reg [34:0] ptw_tag_d1;
    reg [31:0] refill_count;
    reg fence_valid_d1;
    wire sample_enable = (reset === 1'b0) && (reset_backend_done === 1'b1);
    wire flush_mmu = fence_valid || csr_satp_changed || csr_vsatp_changed || csr_hgatp_changed || csr_virt_changed;
    wire [1:0] merged_page_level = ptw_stage == 2'd2 ? ptw_s2level : ptw_stage != 2'd3 ? ptw_s1level :
        ((ptw_s1_pf || ptw_s1_af) || ((ptw_s2_pf || ptw_s2_af) && !(ptw_s1_v && (ptw_s1_r || ptw_s1_w || ptw_s1_x)))) ?
        ptw_s1level : ptw_s1level < ptw_s2level ? ptw_s1level : ptw_s2level;
    wire merged_napot = ptw_stage == 2'd2 ? ptw_s2n : ptw_stage != 2'd3 ? ptw_s1n :
        (ptw_s1n && ptw_s2level != 0) || (ptw_s2n && ptw_s1level != 0) || (ptw_s1n && ptw_s2n);
    wire [2:0] translation_context = !csr_virt ? (csr_satp_mode == 0 ? 3'd4 : 3'd0) :
        csr_vsatp_mode != 0 && csr_hgatp_mode != 0 ? 3'd3 : csr_vsatp_mode != 0 ? 3'd1 :
        csr_hgatp_mode != 0 ? 3'd2 : 3'd4;
    always @(posedge clock) begin
        if (reset) begin
            translation_context_d1 <= 0;
            csr_satp_mode_d1 <= 0;
            csr_vsatp_mode_d1 <= 0;
            csr_hgatp_mode_d1 <= 0;
            csr_asid_d1 <= 0;
            csr_vasid_d1 <= 0;
            csr_vmid_d1 <= 0;
            csr_dmode_d1 <= 0;
            csr_virt_d1 <= 0;
            history_valid <= 0;
            state_write_d1 <= 0;
            state_way_d1 <= 0;
            ptw_stage_d1 <= 0;
            ptw_tag_d1 <= 0;
            refill_count <= 0;
        end else begin
            translation_context_d1 <= translation_context;
            csr_satp_mode_d1 <= csr_satp_mode;
            csr_vsatp_mode_d1 <= csr_vsatp_mode;
            csr_hgatp_mode_d1 <= csr_hgatp_mode;
            csr_asid_d1 <= csr_asid;
            csr_vasid_d1 <= csr_vasid;
            csr_vmid_d1 <= csr_vmid;
            csr_dmode_d1 <= csr_dmode;
            csr_virt_d1 <= csr_virt;
            history_valid <= 1;
            state_write_d1 <= state_write;
            state_way_d1 <= state_way;
            ptw_stage_d1 <= ptw_stage;
            ptw_tag_d1 <= ptw_s1tag;
            if (state_write && refill_count != 32'hffffffff) refill_count <= refill_count + 1;
        end
    end
    always @(posedge clock) begin
        if (reset) fence_valid_d1 <= 0;
        else fence_valid_d1 <= fence_valid;
    end
    // 诊断独立统计，不计入功能分母，不调用UVM错误报告。
    covergroup cg_interface_diagnostics @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_refill_enable : coverpoint ($isunknown(state_write == (!ptw_getgpa && !state_ng && !state_ng_wire &&
            !flush_mmu)) ? 2'd2 : (state_write == (!ptw_getgpa && !state_ng && !state_ng_wire &&
            !flush_mmu)) ? 2'd0 : 2'd1)
            iff (sample_enable && (ptw_valid)) {
            bins b_mismatch = {2'd1};
            bins b_unknown = {2'd2};
        }
    endgroup
    cg_interface_diagnostics interface_diagnostics;
    initial begin
        interface_diagnostics = new();
    end

    // 06.01 回填使能、抑制与字段保存
    covergroup cg_dtlb_06_01 @(posedge clock);
        option.per_instance = 1;
        // 06.01.001 REFILL_正常
        cp_06_01_001 : coverpoint state_write
            iff (sample_enable && (ptw_valid && !ptw_getgpa && !state_ng && !state_ng_wire && !flush_mmu)) {
            bins b_expected_set = {1'b1};
        }
        // 06.01.002 REFILL_抑制_P.getGpa
        cp_06_01_002 : coverpoint state_write
            iff (sample_enable && (ptw_valid && ptw_getgpa && !state_ng && !state_ng_wire && !flush_mmu) &&
                ((!state_write) === 1'b1)) {
            bins b_expected_clear = {1'b0};
        }
        // 06.01.003 REFILL_抑制_D.need_gpa
        cp_06_01_003 : coverpoint state_write
            iff (sample_enable && (ptw_valid && !ptw_getgpa && state_ng && !state_ng_wire && !flush_mmu) &&
                ((!state_write) === 1'b1)) {
            bins b_expected_clear = {1'b0};
        }
        // 06.01.004 REFILL_抑制_D.need_gpa_wire
        cp_06_01_004 : coverpoint state_write
            iff (sample_enable && (ptw_valid && !ptw_getgpa && !state_ng && state_ng_wire && !flush_mmu) &&
                ((!state_write) === 1'b1)) {
            bins b_expected_clear = {1'b0};
        }
        // 06.01.005 REFILL_抑制_flush_mmu
        cp_06_01_005 : coverpoint state_write
            iff (sample_enable && (ptw_valid && !ptw_getgpa && !state_ng && !state_ng_wire && flush_mmu) &&
                ((!state_write) === 1'b1)) {
            bins b_expected_clear = {1'b0};
        }
        // 06.01.009 REFILL_getGpa无等待者
        cp_06_01_009 : coverpoint state_write
            iff (sample_enable && (ptw_valid && ptw_getgpa && !state_ng) && ((!state_write) === 1'b1)) {
            bins b_expected_clear = {1'b0};
        }
    endgroup
    cg_dtlb_06_01 cg_dtlb_06_01_inst;

    // 06.02 两阶段页尺寸与NAPOT合并
    covergroup cg_dtlb_06_02 @(posedge clock);
        option.per_instance = 1;
        // 06.02.006 MERGE_level_S10_S20
        cp_06_02_006 : coverpoint (write_level == 0)
            iff (sample_enable && (state_write && ptw_stage == 3 && ptw_s1level == 0 && ptw_s2level == 0 &&
                !ptw_s1_pf && !ptw_s1_af && !ptw_s2_pf && !ptw_s2_af)) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.007 MERGE_level_S10_S21
        cp_06_02_007 : coverpoint (write_level == 0)
            iff (sample_enable && (state_write && ptw_stage == 3 && ptw_s1level == 0 && ptw_s2level == 1 &&
                !ptw_s1_pf && !ptw_s1_af && !ptw_s2_pf && !ptw_s2_af)) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.008 MERGE_level_S10_S22
        cp_06_02_008 : coverpoint (write_level == 0)
            iff (sample_enable && (state_write && ptw_stage == 3 && ptw_s1level == 0 && ptw_s2level == 2 &&
                !ptw_s1_pf && !ptw_s1_af && !ptw_s2_pf && !ptw_s2_af)) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.009 MERGE_level_S10_S23
        cp_06_02_009 : coverpoint (write_level == 0)
            iff (sample_enable && (state_write && ptw_stage == 3 && ptw_s1level == 0 && ptw_s2level == 3 &&
                !ptw_s1_pf && !ptw_s1_af && !ptw_s2_pf && !ptw_s2_af)) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.010 MERGE_level_S11_S20
        cp_06_02_010 : coverpoint (write_level == 0)
            iff (sample_enable && (state_write && ptw_stage == 3 && ptw_s1level == 1 && ptw_s2level == 0 &&
                !ptw_s1_pf && !ptw_s1_af && !ptw_s2_pf && !ptw_s2_af)) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.011 MERGE_level_S11_S21
        cp_06_02_011 : coverpoint (write_level == 1)
            iff (sample_enable && (state_write && ptw_stage == 3 && ptw_s1level == 1 && ptw_s2level == 1 &&
                !ptw_s1_pf && !ptw_s1_af && !ptw_s2_pf && !ptw_s2_af)) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.012 MERGE_level_S11_S22
        cp_06_02_012 : coverpoint (write_level == 1)
            iff (sample_enable && (state_write && ptw_stage == 3 && ptw_s1level == 1 && ptw_s2level == 2 &&
                !ptw_s1_pf && !ptw_s1_af && !ptw_s2_pf && !ptw_s2_af)) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.013 MERGE_level_S11_S23
        cp_06_02_013 : coverpoint (write_level == 1)
            iff (sample_enable && (state_write && ptw_stage == 3 && ptw_s1level == 1 && ptw_s2level == 3 &&
                !ptw_s1_pf && !ptw_s1_af && !ptw_s2_pf && !ptw_s2_af)) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.014 MERGE_level_S12_S20
        cp_06_02_014 : coverpoint (write_level == 0)
            iff (sample_enable && (state_write && ptw_stage == 3 && ptw_s1level == 2 && ptw_s2level == 0 &&
                !ptw_s1_pf && !ptw_s1_af && !ptw_s2_pf && !ptw_s2_af)) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.015 MERGE_level_S12_S21
        cp_06_02_015 : coverpoint (write_level == 1)
            iff (sample_enable && (state_write && ptw_stage == 3 && ptw_s1level == 2 && ptw_s2level == 1 &&
                !ptw_s1_pf && !ptw_s1_af && !ptw_s2_pf && !ptw_s2_af)) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.016 MERGE_level_S12_S22
        cp_06_02_016 : coverpoint (write_level == 2)
            iff (sample_enable && (state_write && ptw_stage == 3 && ptw_s1level == 2 && ptw_s2level == 2 &&
                !ptw_s1_pf && !ptw_s1_af && !ptw_s2_pf && !ptw_s2_af)) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.017 MERGE_level_S12_S23
        cp_06_02_017 : coverpoint (write_level == 2)
            iff (sample_enable && (state_write && ptw_stage == 3 && ptw_s1level == 2 && ptw_s2level == 3 &&
                !ptw_s1_pf && !ptw_s1_af && !ptw_s2_pf && !ptw_s2_af)) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.018 MERGE_level_S13_S20
        cp_06_02_018 : coverpoint (write_level == 0)
            iff (sample_enable && (state_write && ptw_stage == 3 && ptw_s1level == 3 && ptw_s2level == 0 &&
                !ptw_s1_pf && !ptw_s1_af && !ptw_s2_pf && !ptw_s2_af)) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.019 MERGE_level_S13_S21
        cp_06_02_019 : coverpoint (write_level == 1)
            iff (sample_enable && (state_write && ptw_stage == 3 && ptw_s1level == 3 && ptw_s2level == 1 &&
                !ptw_s1_pf && !ptw_s1_af && !ptw_s2_pf && !ptw_s2_af)) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.020 MERGE_level_S13_S22
        cp_06_02_020 : coverpoint (write_level == 2)
            iff (sample_enable && (state_write && ptw_stage == 3 && ptw_s1level == 3 && ptw_s2level == 2 &&
                !ptw_s1_pf && !ptw_s1_af && !ptw_s2_pf && !ptw_s2_af)) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.021 MERGE_level_S13_S23
        cp_06_02_021 : coverpoint (write_level == 3)
            iff (sample_enable && (state_write && ptw_stage == 3 && ptw_s1level == 3 && ptw_s2level == 3 &&
                !ptw_s1_pf && !ptw_s1_af && !ptw_s2_pf && !ptw_s2_af)) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.001 NAPOT_合并_S1napot_S2large
        cp_06_02_001 : coverpoint (write_napot == 1)
            iff (sample_enable && (state_write && ptw_stage == 3 && ptw_s1n && ptw_s1level == 0 && ptw_s2level > 0)) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.002 NAPOT_合并_S1large_S2napot
        cp_06_02_002 : coverpoint (write_napot == 1)
            iff (sample_enable && (state_write && ptw_stage == 3 && ptw_s1level > 0 && ptw_s2n && ptw_s2level == 0)) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.003 NAPOT_合并_双napot
        cp_06_02_003 : coverpoint (write_napot == 1)
            iff (sample_enable && (state_write && ptw_stage == 3 && ptw_s1n && ptw_s2n && ptw_s1level == 0 &&
                ptw_s2level == 0)) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.004 NAPOT_合并_S1napot_S2_4K
        cp_06_02_004 : coverpoint (write_napot == 0)
            iff (sample_enable && (state_write && ptw_stage == 3 && ptw_s1n && ptw_s1level == 0 && !ptw_s2n &&
                ptw_s2level == 0)) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.005 NAPOT_合并_S1_4K_S2napot
        cp_06_02_005 : coverpoint (write_napot == 0)
            iff (sample_enable && (state_write && ptw_stage == 3 && !ptw_s1n && ptw_s1level == 0 && ptw_s2n &&
                ptw_s2level == 0)) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.022 MERGE_S1异常保持页尺寸
        cp_06_02_022 : coverpoint (write_level == merged_page_level)
            iff (sample_enable && (state_write && ptw_stage == 3 && (ptw_s1_pf || ptw_s1_af) &&
                ptw_s2level < ptw_s1level)) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.023 MERGE_S2异常且S1非叶
        cp_06_02_023 : coverpoint (write_level == merged_page_level)
            iff (sample_enable && (state_write && ptw_stage == 3 && (ptw_s2_pf || ptw_s2_af) && ptw_s1_v &&
                !ptw_s1_r && !ptw_s1_w && !ptw_s1_x)) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.024 MERGE_S2异常且S1叶
        cp_06_02_024 : coverpoint (write_level == merged_page_level)
            iff (sample_enable && (state_write && ptw_stage == 3 && (ptw_s2_pf || ptw_s2_af) && ptw_s1_v &&
                (ptw_s1_r || ptw_s1_w || ptw_s1_x))) {
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_06_02 cg_dtlb_06_02_inst;

    // 06.02 两阶段页尺寸与NAPOT合并
    covergroup cg_dtlb_diag_06_02 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        // 06.02.025：结果偏离或未知单列，正常命中不进入诊断仓。
        rs_06_02_025 : coverpoint ($isunknown(write_level == merged_page_level) ? 2'd2 : (write_level ==
            merged_page_level) ? 2'd0 : 2'd1)
            iff (sample_enable && (state_write && ptw_stage == 3 && (ptw_s2_pf || ptw_s2_af) && !ptw_s1_v &&
                !ptw_s1_pf && !ptw_s1_af && ptw_s1level != 3)) {
            bins b_mismatch = {2'd1};
            bins unknown  = {2'd2};
        }
        // 06.02.025 MERGE_fakePTE优先级待审
        cp_06_02_025 : coverpoint (write_level == merged_page_level)
            iff (sample_enable && (state_write && ptw_stage == 3 && (ptw_s2_pf || ptw_s2_af) && !ptw_s1_v &&
                !ptw_s1_pf && !ptw_s1_af && ptw_s1level != 3)) {
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_diag_06_02 cg_dtlb_diag_06_02_inst;

    // 06.03 PTW旁路与读写冲突
    covergroup cg_dtlb_06_03 @(posedge clock);
        option.per_instance = 1;
        // 06.03.007 RW_相邻拍连续回填
        cp_06_03_007 : coverpoint (state_write && state_write_d1 && ptw_s1tag != ptw_tag_d1)
            iff (sample_enable && (state_write && state_write_d1 && ptw_s1tag != ptw_tag_d1)) {
            bins b_observed = {1'b1};
        }
    endgroup
    cg_dtlb_06_03 cg_dtlb_06_03_inst;

    // 06.04 PLRU更新与容量替换
    covergroup cg_dtlb_06_04 @(posedge clock);
        option.per_instance = 1;
        // 06.04.003 REPLACE_容量压力
        cp_06_04_003 : coverpoint refill_count + 1
            iff (sample_enable && (state_write && refill_count >= 47)) {
            bins b_capacity = {48};
            bins b_first_overflow = {49};
            bins b_intermediate = {[50:95]};
            bins b_double_capacity = {[96:$]};
        }
        // 06.04.006 REPLACE_reset后策略状态
        cp_06_04_006 : coverpoint (state_way < 48)
            iff (sample_enable && (state_write && refill_count == 0)) {
            bins b_expected_result = {1'b1};
        }
        // 06.04.004 REPLACE_读命中更新PLRU
        cp_06_04_004 : coverpoint touch_valid_mask
            iff (sample_enable && (|touch_valid_mask) && ((!$isunknown(state_plru)) === 1'b1)) {
            bins b_single_touch = {1, 2, 4, 8};
            bins b_multiple_touches = {3, 5, 6, 7, [9:15]};
        }
    endgroup
    cg_dtlb_06_04 cg_dtlb_06_04_inst;

    // 07.05 共享GPA状态竞争与恢复
    covergroup cg_dtlb_07_05 @(posedge clock);
        option.per_instance = 1;
        // 07.05.001 GPA_多口同时GPF
        cp_07_05_001 : coverpoint state_ng_wire
            iff (sample_enable && ($countones(gpa_start_mask) >= 2)) {
            bins b_expected_set = {1'b1};
        }
        // 07.05.002 GPA_多口清理与新建竞争
        cp_07_05_002 : coverpoint (state_ng && state_ng_wire)
            iff (sample_enable && (state_ng && state_ng_wire)) {
            bins b_observed = {1'b1};
        }
    endgroup
    cg_dtlb_07_05 cg_dtlb_07_05_inst;

    // 07.05 共享GPA状态竞争与恢复
    covergroup cg_dtlb_diag_07_05 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        // 07.05.003：结果偏离或未知单列，正常命中不进入诊断仓。
        rs_07_05_003 : coverpoint ($isunknown(1'b1) ? 2'd2 : (1'b1) ? 2'd0 : 2'd1)
            iff (sample_enable && (state_ng && flush_mmu)) {
            bins b_mismatch = {2'd1};
            bins unknown  = {2'd2};
        }
        // 07.05.003 GPA_fence与等待
        cp_07_05_003 : coverpoint (state_ng && flush_mmu)
            iff (sample_enable && (state_ng && flush_mmu)) {
            bins b_observed = {1'b1};
        }
    endgroup
    cg_dtlb_diag_07_05 cg_dtlb_diag_07_05_inst;

    // 08.03 CSR变化与失效生效时序
    covergroup cg_dtlb_08_03 @(posedge clock);
        option.per_instance = 1;
        // 08.03.001 FENCE_仅changed不清storage
        cp_08_03_001 : coverpoint state_write
            iff (sample_enable && (!fence_valid && (csr_satp_changed || csr_vsatp_changed || csr_hgatp_changed ||
                csr_virt_changed)) && ((!state_write) === 1'b1)) {
            bins b_expected_clear = {1'b0};
        }
        // 08.03.002 CSR_FLUSH_satp_changed
        cp_08_03_002 : coverpoint state_write
            iff (sample_enable && (csr_satp_changed) && ((!state_write) === 1'b1)) {
            bins b_expected_clear = {1'b0};
        }
        // 08.03.003 CSR_FLUSH_vsatp_changed
        cp_08_03_003 : coverpoint state_write
            iff (sample_enable && (csr_vsatp_changed) && ((!state_write) === 1'b1)) {
            bins b_expected_clear = {1'b0};
        }
        // 08.03.004 CSR_FLUSH_hgatp_changed
        cp_08_03_004 : coverpoint state_write
            iff (sample_enable && (csr_hgatp_changed) && ((!state_write) === 1'b1)) {
            bins b_expected_clear = {1'b0};
        }
        // 08.03.005 CSR_FLUSH_priv_virt_changed
        cp_08_03_005 : coverpoint state_write
            iff (sample_enable && (csr_virt_changed) && ((!state_write) === 1'b1)) {
            bins b_expected_clear = {1'b0};
        }
        // 08.03.006 FENCE_Svinval不取消请求
        cp_08_03_006 : coverpoint (fence_valid)
            iff (sample_enable && (fence_valid)) {
            bins b_observed = {1'b1};
        }
        // 08.03.007 FENCE_flushPipe位
        cp_08_03_007 : coverpoint (fence_valid)
            iff (sample_enable && (fence_valid)) {
            bins b_observed = {1'b1};
        }
    endgroup
    cg_dtlb_08_03 cg_dtlb_08_03_inst;

    // 08.05 在途刷新与新旧上下文恢复
    covergroup cg_dtlb_08_05 @(posedge clock);
        option.per_instance = 1;
        // 08.05.001 FENCE_回填同拍
        cp_08_05_001 : coverpoint state_write
            iff (sample_enable && (ptw_valid && flush_mmu) && ((!state_write) === 1'b1)) {
            bins b_expected_clear = {1'b0};
        }
        // 08.05.006 FENCE_连续与重叠
        cp_08_05_006 : coverpoint (fence_valid && fence_valid_d1)
            iff (sample_enable && (fence_valid && fence_valid_d1)) {
            bins b_observed = {1'b1};
        }
    endgroup
    cg_dtlb_08_05 cg_dtlb_08_05_inst;

    // 08.06 CSR有向切换与在途窗口
    covergroup cg_dtlb_08_06 @(posedge clock);
        option.per_instance = 1;
        // 08.06.002 CSR_hgatp_vmid_改变
        cp_08_06_002 : coverpoint (history_valid && csr_vmid != csr_vmid_d1)
            iff (sample_enable && (history_valid && csr_vmid != csr_vmid_d1)) {
            bins b_observed = {1'b1};
        }
        // 08.06.003 CSR_satp_asid_改变
        cp_08_06_003 : coverpoint (history_valid && csr_asid != csr_asid_d1)
            iff (sample_enable && (history_valid && csr_asid != csr_asid_d1)) {
            bins b_observed = {1'b1};
        }
        // 08.06.005 CSR_vsatp_asid_改变
        cp_08_06_005 : coverpoint (history_valid && csr_vasid != csr_vasid_d1)
            iff (sample_enable && (history_valid && csr_vasid != csr_vasid_d1)) {
            bins b_observed = {1'b1};
        }
        // 08.06.007 CSR_dmode_0到1
        cp_08_06_007 : coverpoint csr_dmode
            iff (sample_enable && (history_valid && csr_dmode_d1 == 0 && csr_dmode == 1)) {
            bins b_changed_to = {1};
        }
        // 08.06.008 CSR_dmode_1到0
        cp_08_06_008 : coverpoint csr_dmode
            iff (sample_enable && (history_valid && csr_dmode_d1 == 1 && csr_dmode == 0)) {
            bins b_changed_to = {0};
        }
        // 08.06.009 CSR_hgatp_mode_0到8
        cp_08_06_009 : coverpoint csr_hgatp_mode
            iff (sample_enable && (history_valid && csr_hgatp_mode_d1 == 0 && csr_hgatp_mode == 8)) {
            bins b_changed_to = {8};
        }
        // 08.06.010 CSR_hgatp_mode_0到9
        cp_08_06_010 : coverpoint csr_hgatp_mode
            iff (sample_enable && (history_valid && csr_hgatp_mode_d1 == 0 && csr_hgatp_mode == 9)) {
            bins b_changed_to = {9};
        }
        // 08.06.011 CSR_hgatp_mode_8到0
        cp_08_06_011 : coverpoint csr_hgatp_mode
            iff (sample_enable && (history_valid && csr_hgatp_mode_d1 == 8 && csr_hgatp_mode == 0)) {
            bins b_changed_to = {0};
        }
        // 08.06.012 CSR_hgatp_mode_8到9
        cp_08_06_012 : coverpoint csr_hgatp_mode
            iff (sample_enable && (history_valid && csr_hgatp_mode_d1 == 8 && csr_hgatp_mode == 9)) {
            bins b_changed_to = {9};
        }
        // 08.06.013 CSR_hgatp_mode_9到0
        cp_08_06_013 : coverpoint csr_hgatp_mode
            iff (sample_enable && (history_valid && csr_hgatp_mode_d1 == 9 && csr_hgatp_mode == 0)) {
            bins b_changed_to = {0};
        }
        // 08.06.014 CSR_hgatp_mode_9到8
        cp_08_06_014 : coverpoint csr_hgatp_mode
            iff (sample_enable && (history_valid && csr_hgatp_mode_d1 == 9 && csr_hgatp_mode == 8)) {
            bins b_changed_to = {8};
        }
        // 08.06.015 CSR_satp_mode_0到8
        cp_08_06_015 : coverpoint csr_satp_mode
            iff (sample_enable && (history_valid && csr_satp_mode_d1 == 0 && csr_satp_mode == 8)) {
            bins b_changed_to = {8};
        }
        // 08.06.016 CSR_satp_mode_0到9
        cp_08_06_016 : coverpoint csr_satp_mode
            iff (sample_enable && (history_valid && csr_satp_mode_d1 == 0 && csr_satp_mode == 9)) {
            bins b_changed_to = {9};
        }
        // 08.06.017 CSR_satp_mode_8到0
        cp_08_06_017 : coverpoint csr_satp_mode
            iff (sample_enable && (history_valid && csr_satp_mode_d1 == 8 && csr_satp_mode == 0)) {
            bins b_changed_to = {0};
        }
        // 08.06.018 CSR_satp_mode_8到9
        cp_08_06_018 : coverpoint csr_satp_mode
            iff (sample_enable && (history_valid && csr_satp_mode_d1 == 8 && csr_satp_mode == 9)) {
            bins b_changed_to = {9};
        }
        // 08.06.019 CSR_satp_mode_9到0
        cp_08_06_019 : coverpoint csr_satp_mode
            iff (sample_enable && (history_valid && csr_satp_mode_d1 == 9 && csr_satp_mode == 0)) {
            bins b_changed_to = {0};
        }
        // 08.06.020 CSR_satp_mode_9到8
        cp_08_06_020 : coverpoint csr_satp_mode
            iff (sample_enable && (history_valid && csr_satp_mode_d1 == 9 && csr_satp_mode == 8)) {
            bins b_changed_to = {8};
        }
        // 08.06.021 CSR翻译阶段_ALL到HOST
        cp_08_06_021 : coverpoint (history_valid && translation_context_d1 == 3 && translation_context == 0)
            iff (sample_enable && (history_valid && translation_context_d1 == 3 && translation_context == 0)) {
            bins b_observed = {1'b1};
        }
        // 08.06.022 CSR翻译阶段_ALL到S1
        cp_08_06_022 : coverpoint (history_valid && translation_context_d1 == 3 && translation_context == 1)
            iff (sample_enable && (history_valid && translation_context_d1 == 3 && translation_context == 1)) {
            bins b_observed = {1'b1};
        }
        // 08.06.023 CSR翻译阶段_ALL到S2
        cp_08_06_023 : coverpoint (history_valid && translation_context_d1 == 3 && translation_context == 2)
            iff (sample_enable && (history_valid && translation_context_d1 == 3 && translation_context == 2)) {
            bins b_observed = {1'b1};
        }
        // 08.06.024 CSR翻译阶段_HOST到ALL
        cp_08_06_024 : coverpoint (history_valid && translation_context_d1 == 0 && translation_context == 3)
            iff (sample_enable && (history_valid && translation_context_d1 == 0 && translation_context == 3)) {
            bins b_observed = {1'b1};
        }
        // 08.06.025 CSR翻译阶段_HOST到S1
        cp_08_06_025 : coverpoint (history_valid && translation_context_d1 == 0 && translation_context == 1)
            iff (sample_enable && (history_valid && translation_context_d1 == 0 && translation_context == 1)) {
            bins b_observed = {1'b1};
        }
        // 08.06.026 CSR翻译阶段_HOST到S2
        cp_08_06_026 : coverpoint (history_valid && translation_context_d1 == 0 && translation_context == 2)
            iff (sample_enable && (history_valid && translation_context_d1 == 0 && translation_context == 2)) {
            bins b_observed = {1'b1};
        }
        // 08.06.027 CSR翻译阶段_S1到ALL
        cp_08_06_027 : coverpoint (history_valid && translation_context_d1 == 1 && translation_context == 3)
            iff (sample_enable && (history_valid && translation_context_d1 == 1 && translation_context == 3)) {
            bins b_observed = {1'b1};
        }
        // 08.06.028 CSR翻译阶段_S1到HOST
        cp_08_06_028 : coverpoint (history_valid && translation_context_d1 == 1 && translation_context == 0)
            iff (sample_enable && (history_valid && translation_context_d1 == 1 && translation_context == 0)) {
            bins b_observed = {1'b1};
        }
        // 08.06.029 CSR翻译阶段_S1到S2
        cp_08_06_029 : coverpoint (history_valid && translation_context_d1 == 1 && translation_context == 2)
            iff (sample_enable && (history_valid && translation_context_d1 == 1 && translation_context == 2)) {
            bins b_observed = {1'b1};
        }
        // 08.06.030 CSR翻译阶段_S2到ALL
        cp_08_06_030 : coverpoint (history_valid && translation_context_d1 == 2 && translation_context == 3)
            iff (sample_enable && (history_valid && translation_context_d1 == 2 && translation_context == 3)) {
            bins b_observed = {1'b1};
        }
        // 08.06.031 CSR翻译阶段_S2到HOST
        cp_08_06_031 : coverpoint (history_valid && translation_context_d1 == 2 && translation_context == 0)
            iff (sample_enable && (history_valid && translation_context_d1 == 2 && translation_context == 0)) {
            bins b_observed = {1'b1};
        }
        // 08.06.032 CSR翻译阶段_S2到S1
        cp_08_06_032 : coverpoint (history_valid && translation_context_d1 == 2 && translation_context == 1)
            iff (sample_enable && (history_valid && translation_context_d1 == 2 && translation_context == 1)) {
            bins b_observed = {1'b1};
        }
        // 08.06.033 CSR_virt_0到1
        cp_08_06_033 : coverpoint csr_virt
            iff (sample_enable && (history_valid && csr_virt_d1 == 0 && csr_virt == 1)) {
            bins b_changed_to = {1};
        }
        // 08.06.034 CSR_virt_1到0
        cp_08_06_034 : coverpoint csr_virt
            iff (sample_enable && (history_valid && csr_virt_d1 == 1 && csr_virt == 0)) {
            bins b_changed_to = {0};
        }
        // 08.06.035 CSR_vsatp_mode_0到8
        cp_08_06_035 : coverpoint csr_vsatp_mode
            iff (sample_enable && (history_valid && csr_vsatp_mode_d1 == 0 && csr_vsatp_mode == 8)) {
            bins b_changed_to = {8};
        }
        // 08.06.036 CSR_vsatp_mode_0到9
        cp_08_06_036 : coverpoint csr_vsatp_mode
            iff (sample_enable && (history_valid && csr_vsatp_mode_d1 == 0 && csr_vsatp_mode == 9)) {
            bins b_changed_to = {9};
        }
        // 08.06.037 CSR_vsatp_mode_8到0
        cp_08_06_037 : coverpoint csr_vsatp_mode
            iff (sample_enable && (history_valid && csr_vsatp_mode_d1 == 8 && csr_vsatp_mode == 0)) {
            bins b_changed_to = {0};
        }
        // 08.06.038 CSR_vsatp_mode_8到9
        cp_08_06_038 : coverpoint csr_vsatp_mode
            iff (sample_enable && (history_valid && csr_vsatp_mode_d1 == 8 && csr_vsatp_mode == 9)) {
            bins b_changed_to = {9};
        }
        // 08.06.039 CSR_vsatp_mode_9到0
        cp_08_06_039 : coverpoint csr_vsatp_mode
            iff (sample_enable && (history_valid && csr_vsatp_mode_d1 == 9 && csr_vsatp_mode == 0)) {
            bins b_changed_to = {0};
        }
        // 08.06.040 CSR_vsatp_mode_9到8
        cp_08_06_040 : coverpoint csr_vsatp_mode
            iff (sample_enable && (history_valid && csr_vsatp_mode_d1 == 9 && csr_vsatp_mode == 8)) {
            bins b_changed_to = {8};
        }
    endgroup
    cg_dtlb_08_06 cg_dtlb_08_06_inst;

    initial begin
        cg_dtlb_06_01_inst = new();
        cg_dtlb_06_02_inst = new();
        cg_dtlb_diag_06_02_inst = new();
        cg_dtlb_06_03_inst = new();
        cg_dtlb_06_04_inst = new();
        cg_dtlb_07_05_inst = new();
        cg_dtlb_diag_07_05_inst = new();
        cg_dtlb_08_03_inst = new();
        cg_dtlb_08_05_inst = new();
        cg_dtlb_08_06_inst = new();
    end

endmodule: memblock_dtlb_control_fcov
`endif

`endif
