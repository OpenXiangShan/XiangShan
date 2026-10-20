// 阅读约定：所有covergroup在posedge观察NBA更新前的值；_d1/_d2是模型历史。
// 简单事件门控已就地展开；保留的参考计算有中文合同，bind给出各端口的RTL来源。
// 对应表：Mem_env_work_20260716/TLB_fcov/DTLB_readability_20261016/DTLB_测试点与功能仓对应表.xlsx
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

    // 上下文/回填历史：_d1表示前拍；refill_count为饱和累计回填次数，仅用于容量覆盖场景。
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
    // 判据 sample_enable：仅在 reset 明确为0且后端复位完成明确为1时采样；X/Z不算有效复位状态。
    wire sample_enable = (reset === 1'b0) && (reset_backend_done === 1'b1);

    // 判据 merged_page_level：参考回填页级：onlyS2取S2；双阶段按故障/非叶规则或较小页级选择。
    wire [1:0] merged_page_level = ptw_stage == 2'd2 ? ptw_s2level : ptw_stage != 2'd3 ? ptw_s1level :
        ((ptw_s1_pf || ptw_s1_af) || ((ptw_s2_pf || ptw_s2_af) && !(ptw_s1_v && (ptw_s1_r || ptw_s1_w ||
            ptw_s1_x)))) ?
        ptw_s1level : ptw_s1level < ptw_s2level ? ptw_s1level : ptw_s2level;
    // 判据 merged_napot：参考回填NAPOT：按翻译阶段及两阶段页级组合选择。
    wire merged_napot = ptw_stage == 2'd2 ? ptw_s2n : ptw_stage != 2'd3 ? ptw_s1n :
        (ptw_s1n && ptw_s2level != 0) || (ptw_s2n && ptw_s1level != 0) || (ptw_s1n && ptw_s2n);
    // 判据 translation_context：上下文编码：0=Host S1，1=Guest S1，2=onlyS2，3=双阶段，4=Bare。
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
    // ----------------------------------------------------------------------------------------
    // 【cg_interface_diagnostics】接口一致性诊断
    // 探测功能：观察输入与实际输出是否一致，并把偏离/未知单独记录。
    // 采样时序：回填/flush直接观察本拍；_d1用于上下文切换、连续回填或下一拍效果。
    // 仓位判读：本组weight=0，cp为观察结果，rs的1/2分别表示偏离/未知；reference_missing表示信号缺失。
    covergroup cg_interface_diagnostics @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_refill_enable : coverpoint ($isunknown(state_write == (!ptw_getgpa && !state_ng && !state_ng_wire &&
            !(fence_valid || csr_satp_changed || csr_vsatp_changed || csr_hgatp_changed ||
                csr_virt_changed))) ? 2'd2 : (state_write == (!ptw_getgpa && !state_ng && !state_ng_wire
                &&
            !(fence_valid || csr_satp_changed || csr_vsatp_changed || csr_hgatp_changed ||
                csr_virt_changed))) ? 2'd0 : 2'd1)
            iff (sample_enable
                && ptw_valid) {
            bins b_mismatch = {2'd1};
            bins b_unknown = {2'd2};
        }
    endgroup
    cg_interface_diagnostics interface_diagnostics;
    initial begin
        interface_diagnostics = new();
    end

    // 06.01 回填使能、抑制与字段保存
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_06_01】06.01 回填使能、抑制与字段保存
    // 探测功能：观察 PTW 返回何时真正写入缓存，以及写入保存的地址、权限和异常属性。
    // 采样时序：回填/flush直接观察本拍；_d1用于上下文切换、连续回填或下一拍效果。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：06.01.001, 06.01.002, 06.01.003, 06.01.004, 06.01.005, 06.01.009；逐点功能见下方注释，精确配值见分解表。
    covergroup cg_dtlb_06_01 @(posedge clock);
        option.per_instance = 1;
        // 06.01.001 REFILL_正常
        // 测试表目标：场景：P.valid=1,getGpa=0,need_gpa=0,need_gpa_wire=0,flush_mmu=0。
        // 观察功能：W.valid=1；下拍对应way置valid并保存payload。 测试目的：观察 PTW 返回何时真正写入缓存，以及写入保存的地址、权限和异常属性。
        cp_06_01_001 : coverpoint state_write
            iff (sample_enable
                && ptw_valid
                && !ptw_getgpa
                && !state_ng
                && !state_ng_wire
                && (!(fence_valid || csr_satp_changed || csr_vsatp_changed || csr_hgatp_changed ||
                    csr_virt_changed))) {
            bins b_expected_set = {1'b1};
        }
        // 06.01.002 REFILL_抑制_P.getGpa
        // 测试表目标：场景：P.valid=1且P.getGpa=1；其余抑制条件0。 观察功能：W.valid=0；不能覆盖旧表项。 测试目的：观察 PTW
        // 返回何时真正写入缓存，以及写入保存的地址、权限和异常属性。
        cp_06_01_002 : coverpoint state_write
            iff (sample_enable
                && ptw_valid
                && ptw_getgpa
                && !state_ng
                && !state_ng_wire
                && (!(fence_valid || csr_satp_changed || csr_vsatp_changed || csr_hgatp_changed ||
                    csr_virt_changed))
                && (!state_write) === 1'b1) {
            bins b_expected_clear = {1'b0};
        }
        // 06.01.003 REFILL_抑制_D.need_gpa
        // 测试表目标：场景：P.valid=1且D.need_gpa=1；其余抑制条件0。 观察功能：W.valid=0；不能覆盖旧表项。 测试目的：观察 PTW
        // 返回何时真正写入缓存，以及写入保存的地址、权限和异常属性。
        cp_06_01_003 : coverpoint state_write
            iff (sample_enable
                && ptw_valid
                && !ptw_getgpa
                && state_ng
                && !state_ng_wire
                && (!(fence_valid || csr_satp_changed || csr_vsatp_changed || csr_hgatp_changed ||
                    csr_virt_changed))
                && (!state_write) === 1'b1) {
            bins b_expected_clear = {1'b0};
        }
        // 06.01.004 REFILL_抑制_D.need_gpa_wire
        // 测试表目标：场景：P.valid=1且D.need_gpa_wire=1；其余抑制条件0。 观察功能：W.valid=0；不能覆盖旧表项。 测试目的：观察 PTW
        // 返回何时真正写入缓存，以及写入保存的地址、权限和异常属性。
        cp_06_01_004 : coverpoint state_write
            iff (sample_enable
                && ptw_valid
                && !ptw_getgpa
                && !state_ng
                && state_ng_wire
                && (!(fence_valid || csr_satp_changed || csr_vsatp_changed || csr_hgatp_changed ||
                    csr_virt_changed))
                && (!state_write) === 1'b1) {
            bins b_expected_clear = {1'b0};
        }
        // 06.01.005 REFILL_抑制_flush_mmu
        // 测试表目标：场景：P.valid=1且flush_mmu=1；其余抑制条件0。 观察功能：W.valid=0；不能覆盖旧表项。 测试目的：观察 PTW
        // 返回何时真正写入缓存，以及写入保存的地址、权限和异常属性。
        cp_06_01_005 : coverpoint state_write
            iff (sample_enable
                && ptw_valid
                && !ptw_getgpa
                && !state_ng
                && !state_ng_wire
                && (fence_valid || csr_satp_changed || csr_vsatp_changed || csr_hgatp_changed || csr_virt_changed)
                && (!state_write) === 1'b1) {
            bins b_expected_clear = {1'b0};
        }
        // 06.01.009 REFILL_getGpa无等待者
        // 测试表目标：场景：P.valid=1,getGpa=1,need_gpa=0。 观察功能：常规W.valid=0；无匹配请求时不虚构GPA完成，有匹配REQ时仍可bypass。
        // 测试目的：观察 PTW 返回何时真正写入缓存，以及写入保存的地址、权限和异常属性。
        cp_06_01_009 : coverpoint state_write
            iff (sample_enable
                && ptw_valid
                && ptw_getgpa
                && !state_ng
                && (!state_write) === 1'b1) {
            bins b_expected_clear = {1'b0};
        }
    endgroup
    cg_dtlb_06_01 cg_dtlb_06_01_inst;

    // 06.02 两阶段页尺寸与NAPOT合并
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_06_02】06.02 两阶段页尺寸与NAPOT合并
    // 探测功能：观察两阶段页尺寸及 NAPOT 的合并结果，决定后续缓存命中和定址失效的范围。
    // 采样时序：回填/flush直接观察本拍；_d1用于上下文切换、连续回填或下一拍效果。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：06.02.001, 06.02.002, 06.02.003, 06.02.004, 06.02.005, 06.02.006, 06.02.007,
    // 06.02.008, 06.02.009, 06.02.010, 06.02.011, 06.02.012, 06.02.013, 06.02.014, 06.02.015,
    // 06.02.016, 06.02.017, 06.02.018, 06.02.019, 06.02.020, 06.02.021, 06.02.022, 06.02.023,
    // 06.02.024；逐点功能见下方注释，精确配值见分解表。
    // merged_page_level：参考回填页级：onlyS2取S2；双阶段按故障/非叶规则或较小页级选择。
    covergroup cg_dtlb_06_02 @(posedge clock);
        option.per_instance = 1;
        // 06.02.006 MERGE_level_S10_S20
        // 测试表目标：在 VS+G 两阶段翻译均无异常的回填中，S1 level=0 表示4KB 基础页，S2 level=0 表示4KB 基础页。观察缓存合并后采用较小页尺寸，即
        // level=0（4KB 基础页），并按合并后的范围重访页内和越界地址。该组合覆盖两阶段页大小相等或第二阶段更小的情况。
        cp_06_02_006 : coverpoint (write_level == 0)
            iff (sample_enable
                && state_write
                && ptw_stage == 3
                && ptw_s1level == 0
                && ptw_s2level == 0
                && !ptw_s1_pf
                && !ptw_s1_af
                && !ptw_s2_pf
                && !ptw_s2_af) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.007 MERGE_level_S10_S21
        // 测试表目标：在 VS+G 两阶段翻译均无异常的回填中，S1 level=0 表示4KB 基础页，S2 level=1 表示2MB 超级页。观察缓存合并后采用较小页尺寸，即
        // level=0（4KB 基础页），并按合并后的范围重访页内和越界地址。S2 页大于 S1 页时还要观察用 S2.tag 补齐合并 PPN 低位的分支。
        cp_06_02_007 : coverpoint (write_level == 0)
            iff (sample_enable
                && state_write
                && ptw_stage == 3
                && ptw_s1level == 0
                && ptw_s2level == 1
                && !ptw_s1_pf
                && !ptw_s1_af
                && !ptw_s2_pf
                && !ptw_s2_af) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.008 MERGE_level_S10_S22
        // 测试表目标：在 VS+G 两阶段翻译均无异常的回填中，S1 level=0 表示4KB 基础页，S2 level=2 表示1GB 超级页。观察缓存合并后采用较小页尺寸，即
        // level=0（4KB 基础页），并按合并后的范围重访页内和越界地址。S2 页大于 S1 页时还要观察用 S2.tag 补齐合并 PPN 低位的分支。
        cp_06_02_008 : coverpoint (write_level == 0)
            iff (sample_enable
                && state_write
                && ptw_stage == 3
                && ptw_s1level == 0
                && ptw_s2level == 2
                && !ptw_s1_pf
                && !ptw_s1_af
                && !ptw_s2_pf
                && !ptw_s2_af) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.009 MERGE_level_S10_S23
        // 测试表目标：在 VS+G 两阶段翻译均无异常的回填中，S1 level=0 表示4KB 基础页，S2 level=3 表示512GB 超级页（合法 Sv48
        // 场景）。观察缓存合并后采用较小页尺寸，即 level=0（4KB 基础页），并按合并后的范围重访页内和越界地址。S2 页大于 S1 页时还要观察用 S2.tag 补齐合并
        // PPN 低位的分支。
        cp_06_02_009 : coverpoint (write_level == 0)
            iff (sample_enable
                && state_write
                && ptw_stage == 3
                && ptw_s1level == 0
                && ptw_s2level == 3
                && !ptw_s1_pf
                && !ptw_s1_af
                && !ptw_s2_pf
                && !ptw_s2_af) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.010 MERGE_level_S11_S20
        // 测试表目标：在 VS+G 两阶段翻译均无异常的回填中，S1 level=1 表示2MB 超级页，S2 level=0 表示4KB 基础页。观察缓存合并后采用较小页尺寸，即
        // level=0（4KB 基础页），并按合并后的范围重访页内和越界地址。该组合覆盖两阶段页大小相等或第二阶段更小的情况。
        cp_06_02_010 : coverpoint (write_level == 0)
            iff (sample_enable
                && state_write
                && ptw_stage == 3
                && ptw_s1level == 1
                && ptw_s2level == 0
                && !ptw_s1_pf
                && !ptw_s1_af
                && !ptw_s2_pf
                && !ptw_s2_af) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.011 MERGE_level_S11_S21
        // 测试表目标：在 VS+G 两阶段翻译均无异常的回填中，S1 level=1 表示2MB 超级页，S2 level=1 表示2MB 超级页。观察缓存合并后采用较小页尺寸，即
        // level=1（2MB 超级页），并按合并后的范围重访页内和越界地址。该组合覆盖两阶段页大小相等或第二阶段更小的情况。
        cp_06_02_011 : coverpoint (write_level == 1)
            iff (sample_enable
                && state_write
                && ptw_stage == 3
                && ptw_s1level == 1
                && ptw_s2level == 1
                && !ptw_s1_pf
                && !ptw_s1_af
                && !ptw_s2_pf
                && !ptw_s2_af) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.012 MERGE_level_S11_S22
        // 测试表目标：在 VS+G 两阶段翻译均无异常的回填中，S1 level=1 表示2MB 超级页，S2 level=2 表示1GB 超级页。观察缓存合并后采用较小页尺寸，即
        // level=1（2MB 超级页），并按合并后的范围重访页内和越界地址。S2 页大于 S1 页时还要观察用 S2.tag 补齐合并 PPN 低位的分支。
        cp_06_02_012 : coverpoint (write_level == 1)
            iff (sample_enable
                && state_write
                && ptw_stage == 3
                && ptw_s1level == 1
                && ptw_s2level == 2
                && !ptw_s1_pf
                && !ptw_s1_af
                && !ptw_s2_pf
                && !ptw_s2_af) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.013 MERGE_level_S11_S23
        // 测试表目标：在 VS+G 两阶段翻译均无异常的回填中，S1 level=1 表示2MB 超级页，S2 level=3 表示512GB 超级页（合法 Sv48
        // 场景）。观察缓存合并后采用较小页尺寸，即 level=1（2MB 超级页），并按合并后的范围重访页内和越界地址。S2 页大于 S1 页时还要观察用 S2.tag 补齐合并
        // PPN 低位的分支。
        cp_06_02_013 : coverpoint (write_level == 1)
            iff (sample_enable
                && state_write
                && ptw_stage == 3
                && ptw_s1level == 1
                && ptw_s2level == 3
                && !ptw_s1_pf
                && !ptw_s1_af
                && !ptw_s2_pf
                && !ptw_s2_af) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.014 MERGE_level_S12_S20
        // 测试表目标：在 VS+G 两阶段翻译均无异常的回填中，S1 level=2 表示1GB 超级页，S2 level=0 表示4KB 基础页。观察缓存合并后采用较小页尺寸，即
        // level=0（4KB 基础页），并按合并后的范围重访页内和越界地址。该组合覆盖两阶段页大小相等或第二阶段更小的情况。
        cp_06_02_014 : coverpoint (write_level == 0)
            iff (sample_enable
                && state_write
                && ptw_stage == 3
                && ptw_s1level == 2
                && ptw_s2level == 0
                && !ptw_s1_pf
                && !ptw_s1_af
                && !ptw_s2_pf
                && !ptw_s2_af) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.015 MERGE_level_S12_S21
        // 测试表目标：在 VS+G 两阶段翻译均无异常的回填中，S1 level=2 表示1GB 超级页，S2 level=1 表示2MB 超级页。观察缓存合并后采用较小页尺寸，即
        // level=1（2MB 超级页），并按合并后的范围重访页内和越界地址。该组合覆盖两阶段页大小相等或第二阶段更小的情况。
        cp_06_02_015 : coverpoint (write_level == 1)
            iff (sample_enable
                && state_write
                && ptw_stage == 3
                && ptw_s1level == 2
                && ptw_s2level == 1
                && !ptw_s1_pf
                && !ptw_s1_af
                && !ptw_s2_pf
                && !ptw_s2_af) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.016 MERGE_level_S12_S22
        // 测试表目标：在 VS+G 两阶段翻译均无异常的回填中，S1 level=2 表示1GB 超级页，S2 level=2 表示1GB 超级页。观察缓存合并后采用较小页尺寸，即
        // level=2（1GB 超级页），并按合并后的范围重访页内和越界地址。该组合覆盖两阶段页大小相等或第二阶段更小的情况。
        cp_06_02_016 : coverpoint (write_level == 2)
            iff (sample_enable
                && state_write
                && ptw_stage == 3
                && ptw_s1level == 2
                && ptw_s2level == 2
                && !ptw_s1_pf
                && !ptw_s1_af
                && !ptw_s2_pf
                && !ptw_s2_af) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.017 MERGE_level_S12_S23
        // 测试表目标：在 VS+G 两阶段翻译均无异常的回填中，S1 level=2 表示1GB 超级页，S2 level=3 表示512GB 超级页（合法 Sv48
        // 场景）。观察缓存合并后采用较小页尺寸，即 level=2（1GB 超级页），并按合并后的范围重访页内和越界地址。S2 页大于 S1 页时还要观察用 S2.tag 补齐合并
        // PPN 低位的分支。
        cp_06_02_017 : coverpoint (write_level == 2)
            iff (sample_enable
                && state_write
                && ptw_stage == 3
                && ptw_s1level == 2
                && ptw_s2level == 3
                && !ptw_s1_pf
                && !ptw_s1_af
                && !ptw_s2_pf
                && !ptw_s2_af) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.018 MERGE_level_S13_S20
        // 测试表目标：在 VS+G 两阶段翻译均无异常的回填中，S1 level=3 表示512GB 超级页（合法 Sv48 场景），S2 level=0 表示4KB
        // 基础页。观察缓存合并后采用较小页尺寸，即 level=0（4KB 基础页），并按合并后的范围重访页内和越界地址。该组合覆盖两阶段页大小相等或第二阶段更小的情况。
        cp_06_02_018 : coverpoint (write_level == 0)
            iff (sample_enable
                && state_write
                && ptw_stage == 3
                && ptw_s1level == 3
                && ptw_s2level == 0
                && !ptw_s1_pf
                && !ptw_s1_af
                && !ptw_s2_pf
                && !ptw_s2_af) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.019 MERGE_level_S13_S21
        // 测试表目标：在 VS+G 两阶段翻译均无异常的回填中，S1 level=3 表示512GB 超级页（合法 Sv48 场景），S2 level=1 表示2MB
        // 超级页。观察缓存合并后采用较小页尺寸，即 level=1（2MB 超级页），并按合并后的范围重访页内和越界地址。该组合覆盖两阶段页大小相等或第二阶段更小的情况。
        cp_06_02_019 : coverpoint (write_level == 1)
            iff (sample_enable
                && state_write
                && ptw_stage == 3
                && ptw_s1level == 3
                && ptw_s2level == 1
                && !ptw_s1_pf
                && !ptw_s1_af
                && !ptw_s2_pf
                && !ptw_s2_af) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.020 MERGE_level_S13_S22
        // 测试表目标：在 VS+G 两阶段翻译均无异常的回填中，S1 level=3 表示512GB 超级页（合法 Sv48 场景），S2 level=2 表示1GB
        // 超级页。观察缓存合并后采用较小页尺寸，即 level=2（1GB 超级页），并按合并后的范围重访页内和越界地址。该组合覆盖两阶段页大小相等或第二阶段更小的情况。
        cp_06_02_020 : coverpoint (write_level == 2)
            iff (sample_enable
                && state_write
                && ptw_stage == 3
                && ptw_s1level == 3
                && ptw_s2level == 2
                && !ptw_s1_pf
                && !ptw_s1_af
                && !ptw_s2_pf
                && !ptw_s2_af) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.021 MERGE_level_S13_S23
        // 测试表目标：在 VS+G 两阶段翻译均无异常的回填中，S1 level=3 表示512GB 超级页（合法 Sv48 场景），S2 level=3 表示512GB 超级页（合法
        // Sv48 场景）。观察缓存合并后采用较小页尺寸，即 level=3（512GB 超级页（合法 Sv48
        // 场景）），并按合并后的范围重访页内和越界地址。该组合覆盖两阶段页大小相等或第二阶段更小的情况。
        cp_06_02_021 : coverpoint (write_level == 3)
            iff (sample_enable
                && state_write
                && ptw_stage == 3
                && ptw_s1level == 3
                && ptw_s2level == 3
                && !ptw_s1_pf
                && !ptw_s1_af
                && !ptw_s2_pf
                && !ptw_s2_af) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.001 NAPOT_合并_S1napot_S2large
        // 测试表目标：场景：W.s2xlate=3；s1.n=1,s1.level=0,s2.level>0。 观察功能：entry.n=1；按合并后范围访问/越界验证。
        // 测试目的：观察两阶段页尺寸及 NAPOT 的合并结果，决定后续缓存命中和定址失效的范围。
        cp_06_02_001 : coverpoint (write_napot == 1)
            iff (sample_enable
                && state_write
                && ptw_stage == 3
                && ptw_s1n
                && ptw_s1level == 0
                && ptw_s2level > 0) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.002 NAPOT_合并_S1large_S2napot
        // 测试表目标：场景：W.s2xlate=3；s1.level>0,s2.n=1,s2.level=0。 观察功能：entry.n=1；按合并后范围访问/越界验证。
        // 测试目的：观察两阶段页尺寸及 NAPOT 的合并结果，决定后续缓存命中和定址失效的范围。
        cp_06_02_002 : coverpoint (write_napot == 1)
            iff (sample_enable
                && state_write
                && ptw_stage == 3
                && ptw_s1level > 0
                && ptw_s2n
                && ptw_s2level == 0) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.003 NAPOT_合并_双napot
        // 测试表目标：场景：W.s2xlate=3；s1.n=1,s2.n=1,levels=0。 观察功能：entry.n=1；按合并后范围访问/越界验证。
        // 测试目的：观察两阶段页尺寸及 NAPOT 的合并结果，决定后续缓存命中和定址失效的范围。
        cp_06_02_003 : coverpoint (write_napot == 1)
            iff (sample_enable
                && state_write
                && ptw_stage == 3
                && ptw_s1n
                && ptw_s2n
                && ptw_s1level == 0
                && ptw_s2level == 0) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.004 NAPOT_合并_S1napot_S2_4K
        // 测试表目标：场景：W.s2xlate=3；s1.n=1,s2.n=0,s2.level=0。 观察功能：entry.n=0；按合并后范围访问/越界验证。
        // 测试目的：观察两阶段页尺寸及 NAPOT 的合并结果，决定后续缓存命中和定址失效的范围。
        cp_06_02_004 : coverpoint (write_napot == 0)
            iff (sample_enable
                && state_write
                && ptw_stage == 3
                && ptw_s1n
                && ptw_s1level == 0
                && !ptw_s2n
                && ptw_s2level == 0) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.005 NAPOT_合并_S1_4K_S2napot
        // 测试表目标：场景：W.s2xlate=3；s1.n=0,s1.level=0,s2.n=1。 观察功能：entry.n=0；按合并后范围访问/越界验证。
        // 测试目的：观察两阶段页尺寸及 NAPOT 的合并结果，决定后续缓存命中和定址失效的范围。
        cp_06_02_005 : coverpoint (write_napot == 0)
            iff (sample_enable
                && state_write
                && ptw_stage == 3
                && !ptw_s1n
                && ptw_s1level == 0
                && ptw_s2n
                && ptw_s2level == 0) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.022 MERGE_S1异常保持页尺寸
        // 测试表目标：场景：allStage且s1.pf或s1.af=1，s1非fake；令s2.level<s1.level。
        // 观察功能：inner_level=s1.level；回填后同大页内地址与fence范围正确。 测试目的：观察两阶段页尺寸及 NAPOT
        // 的合并结果，决定后续缓存命中和定址失效的范围。
        cp_06_02_022 : coverpoint (write_level == merged_page_level)
            iff (sample_enable
                && state_write
                && ptw_stage == 3
                && (ptw_s1_pf || ptw_s1_af)
                && ptw_s2level < ptw_s1level) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.023 MERGE_S2异常且S1非叶
        // 测试表目标：场景：allStage，s2.gpf/gaf=1，S1有效非叶、s1.pf/af=0。 观察功能：inner_level=s1.level；不错误缩成min。
        // 测试目的：观察两阶段页尺寸及 NAPOT 的合并结果，决定后续缓存命中和定址失效的范围。
        cp_06_02_023 : coverpoint (write_level == merged_page_level)
            iff (sample_enable
                && state_write
                && ptw_stage == 3
                && (ptw_s2_pf || ptw_s2_af)
                && ptw_s1_v
                && !ptw_s1_r
                && !ptw_s1_w
                && !ptw_s1_x) {
            bins b_expected_result = {1'b1};
        }
        // 06.02.024 MERGE_S2异常且S1叶
        // 测试表目标：场景：allStage，s2.gpf/gaf=1，S1叶、s1.pf/af=0。 观察功能：inner_level=min(s1.level,s2.level)。
        // 测试目的：观察两阶段页尺寸及 NAPOT 的合并结果，决定后续缓存命中和定址失效的范围。
        cp_06_02_024 : coverpoint (write_level == merged_page_level)
            iff (sample_enable
                && state_write
                && ptw_stage == 3
                && (ptw_s2_pf || ptw_s2_af)
                && ptw_s1_v
                && (ptw_s1_r || ptw_s1_w || ptw_s1_x)) {
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_06_02 cg_dtlb_06_02_inst;

    // 06.02 两阶段页尺寸与NAPOT合并
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_diag_06_02】06.02 两阶段页尺寸与NAPOT合并
    // 探测功能：观察两阶段页尺寸及 NAPOT 的合并结果，决定后续缓存命中和定址失效的范围。
    // 采样时序：回填/flush直接观察本拍；_d1用于上下文切换、连续回填或下一拍效果。
    // 仓位判读：本组weight=0，cp为观察结果，rs的1/2分别表示偏离/未知；reference_missing表示信号缺失。
    // 测试点：06.02.025；逐点功能见下方注释，精确配值见分解表。
    // merged_page_level：参考回填页级：onlyS2取S2；双阶段按故障/非叶规则或较小页级选择。
    covergroup cg_dtlb_diag_06_02 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        // 06.02.025：结果偏离或未知单列，正常命中不进入诊断仓。
        rs_06_02_025 : coverpoint ($isunknown(write_level == merged_page_level) ? 2'd2 : (write_level ==
            merged_page_level) ? 2'd0 : 2'd1)
            iff (sample_enable
                && state_write
                && ptw_stage == 3
                && (ptw_s2_pf || ptw_s2_af)
                && !ptw_s1_v
                && !ptw_s1_pf
                && !ptw_s1_af
                && ptw_s1level != 3) {
            bins b_mismatch = {2'd1};
            bins unknown  = {2'd2};
        }
        // 06.02.025 MERGE_fakePTE优先级待审
        // 测试表目标：场景：allStage，s2.gpf/gaf=1，s1.v=0,s1.pf=0,s1.af=0，s1.level!=3。
        // 观察功能：当前RTL先匹配s2_exception&&!s1_leaf，结果是s1.level；注释所述强制3分支被遮蔽。 测试目的：观察两阶段页尺寸及 NAPOT
        // 的合并结果，决定后续缓存命中和定址失效的范围。
        cp_06_02_025 : coverpoint (write_level == merged_page_level)
            iff (sample_enable
                && state_write
                && ptw_stage == 3
                && (ptw_s2_pf || ptw_s2_af)
                && !ptw_s1_v
                && !ptw_s1_pf
                && !ptw_s1_af
                && ptw_s1level != 3) {
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_diag_06_02 cg_dtlb_diag_06_02_inst;

    // 06.03 PTW旁路与读写冲突
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_06_03】06.03 PTW旁路与读写冲突
    // 探测功能：观察新 PTW 返回直接旁路与缓存读写之间的优先级，特别是读到正在被替换的 way。
    // 采样时序：回填/flush直接观察本拍；_d1用于上下文切换、连续回填或下一拍效果。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：06.03.007；逐点功能见下方注释，精确配值见分解表。
    covergroup cg_dtlb_06_03 @(posedge clock);
        option.per_instance = 1;
        // 06.03.007 RW_相邻拍连续回填
        // 测试表目标：场景：P.valid连续且满足refill条件，两个不同映射。 观察功能：每次独立选way/写入；两条都可重访。 测试目的：观察新 PTW
        // 返回直接旁路与缓存读写之间的优先级，特别是读到正在被替换的 way。
        cp_06_03_007 : coverpoint (state_write && state_write_d1 && ptw_s1tag != ptw_tag_d1)
            iff (sample_enable
                && state_write
                && state_write_d1
                && ptw_s1tag != ptw_tag_d1) {
            bins b_observed = {1'b1};
        }
    endgroup
    cg_dtlb_06_03 cg_dtlb_06_03_inst;

    // 06.04 PLRU更新与容量替换
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_06_04】06.04 PLRU更新与容量替换
    // 探测功能：观察48路缓存的替换选择和 PLRU 更新，区分首次填入、容量替换及多口触碰。
    // 采样时序：回填/flush直接观察本拍；_d1用于上下文切换、连续回填或下一拍效果。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：06.04.003, 06.04.004, 06.04.006；逐点功能见下方注释，精确配值见分解表。
    covergroup cg_dtlb_06_04 @(posedge clock);
        option.per_instance = 1;
        // 06.04.003 REPLACE_容量压力
        // 测试表目标：场景：插入48/49/≥96个不相交映射并穿插命中。 观察功能：覆盖容量溢出、重访淘汰与保留项；不要假设LRU或优先invalid。
        // 测试目的：观察48路缓存的替换选择和 PLRU 更新，区分首次填入、容量替换及多口触碰。
        cp_06_04_003 : coverpoint refill_count + 1
            iff (sample_enable
                && state_write
                && refill_count >= 47) {
            bins b_capacity = {48};
            bins b_first_overflow = {49};
            bins b_intermediate = {[50:95]};
            bins b_double_capacity = {[96:$]};
        }
        // 06.04.006 REPLACE_reset后策略状态
        // 测试表目标：场景：reset后连续回填，记录refill_idx_state_reg与wayIdx。
        // 观察功能：初态可预测且way在0..47；reset与写同拍valid应清零。 测试目的：观察48路缓存的替换选择和 PLRU 更新，区分首次填入、容量替换及多口触碰。
        cp_06_04_006 : coverpoint (state_way < 48)
            iff (sample_enable
                && state_write
                && refill_count == 0) {
            bins b_expected_result = {1'b1};
        }
        // 06.04.004 REPLACE_读命中更新PLRU
        // 测试表目标：场景：storage hit触发touch_ways。 观察功能：PLRU状态按命中way更新；多端口同时touch按RTL折叠次序。
        // 测试目的：观察48路缓存的替换选择和 PLRU 更新，区分首次填入、容量替换及多口触碰。
        cp_06_04_004 : coverpoint touch_valid_mask
            iff (sample_enable
                && |touch_valid_mask
                && (!$isunknown(state_plru)) === 1'b1) {
            bins b_single_touch = {1, 2, 4, 8};
            bins b_multiple_touches = {3, 5, 6, 7, [9:15]};
        }
    endgroup
    cg_dtlb_06_04 cg_dtlb_06_04_inst;

    // 07.05 共享GPA状态竞争与恢复
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_07_05】07.05 共享GPA状态竞争与恢复
    // 探测功能：观察多端口共享 GPA 状态的竞争优先级，以及刷新或并发业务后的恢复。
    // 采样时序：回填/flush直接观察本拍；_d1用于上下文切换、连续回填或下一拍效果。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：07.05.001, 07.05.002；逐点功能见下方注释，精确配值见分解表。
    covergroup cg_dtlb_07_05 @(posedge clock);
        option.per_instance = 1;
        // 07.05.001 GPA_多口同时GPF
        // 测试表目标：场景：同一实例两个或更多端口同拍满足新建need_gpa条件。 观察功能：采owner VPN/ROB优先级；当前生成RTL高索引后赋值可覆盖低索引，其他口需重试。
        // 测试目的：观察多端口共享 GPA 状态的竞争优先级，以及刷新或并发业务后的恢复。
        cp_07_05_001 : coverpoint state_ng_wire
            iff (sample_enable
                && $countones(gpa_start_mask) >= 2) {
            bins b_expected_set = {1'b1};
        }
        // 07.05.002 GPA_多口清理与新建竞争
        // 测试表目标：场景：一口消费/redirect清除，另一口新建或更新GPA。 观察功能：按实际生成RTL组合优先级检查，记录old/new owner；不能假设每口独立状态。
        // 测试目的：观察多端口共享 GPA 状态的竞争优先级，以及刷新或并发业务后的恢复。
        cp_07_05_002 : coverpoint (state_ng && state_ng_wire)
            iff (sample_enable
                && state_ng
                && state_ng_wire) {
            bins b_observed = {1'b1};
        }
    endgroup
    cg_dtlb_07_05 cg_dtlb_07_05_inst;

    // 07.05 共享GPA状态竞争与恢复
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_diag_07_05】07.05 共享GPA状态竞争与恢复
    // 探测功能：观察多端口共享 GPA 状态的竞争优先级，以及刷新或并发业务后的恢复。
    // 采样时序：回填/flush直接观察本拍；_d1用于上下文切换、连续回填或下一拍效果。
    // 仓位判读：本组weight=0，cp为观察结果，rs的1/2分别表示偏离/未知；reference_missing表示信号缺失。
    // 测试点：07.05.003；逐点功能见下方注释，精确配值见分解表。
    covergroup cg_dtlb_diag_07_05 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        // 07.05.003：结果偏离或未知单列，正常命中不进入诊断仓。
        rs_07_05_003 : coverpoint ($isunknown(1'b1) ? 2'd2 : (1'b1) ? 2'd0 : 2'd1)
            iff (sample_enable
                && state_ng
                && (fence_valid || csr_satp_changed || csr_vsatp_changed || csr_hgatp_changed || csr_virt_changed)) {
            bins b_mismatch = {2'd1};
            bins unknown  = {2'd2};
        }
        // 07.05.003 GPA_fence与等待
        // 测试表目标：场景：need_gpa=1时发生sfence或CSR changed。
        // 观察功能：filter/回填被清理或阻止，但need_gpa没有直接flush_mmu清除分支；核对redirect/重试恢复。 测试目的：观察多端口共享 GPA
        // 状态的竞争优先级，以及刷新或并发业务后的恢复。
        cp_07_05_003 : coverpoint (state_ng && (fence_valid || csr_satp_changed || csr_vsatp_changed ||
            csr_hgatp_changed || csr_virt_changed))
            iff (sample_enable
                && state_ng
                && (fence_valid || csr_satp_changed || csr_vsatp_changed || csr_hgatp_changed || csr_virt_changed)) {
            bins b_observed = {1'b1};
        }
    endgroup
    cg_dtlb_diag_07_05 cg_dtlb_diag_07_05_inst;

    // 08.03 CSR变化与失效生效时序
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_08_03】08.03 CSR变化与失效生效时序
    // 探测功能：观察 CSR 改变与 fence 的不同作用及生效延迟，避免把 Filter 清空等同于 storage 失效。
    // 采样时序：回填/flush直接观察本拍；_d1用于上下文切换、连续回填或下一拍效果。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：08.03.001, 08.03.002, 08.03.003, 08.03.004, 08.03.005, 08.03.006,
    // 08.03.007；逐点功能见下方注释，精确配值见分解表。
    covergroup cg_dtlb_08_03 @(posedge clock);
        option.per_instance = 1;
        // 08.03.001 FENCE_仅changed不清storage
        // 测试表目标：场景：satp/vsatp/hgatp.changed或virt_changed，F.valid=0。 观察功能：阻止refill并清filter；storage
        // v没有这些changed清除入口，ASID/VMID改变影响匹配。 测试目的：观察 CSR 改变与 fence 的不同作用及生效延迟，避免把 Filter 清空等同于
        // storage 失效。
        cp_08_03_001 : coverpoint state_write
            iff (sample_enable
                && !fence_valid
                && (csr_satp_changed || csr_vsatp_changed || csr_hgatp_changed || csr_virt_changed)
                && (!state_write) === 1'b1) {
            bins b_expected_clear = {1'b0};
        }
        // 08.03.002 CSR_FLUSH_satp_changed
        // 测试表目标：场景：对应C.satp_changed=1，filter有未发/已发entry，P.valid可能同时到达。 观察功能：生效拍filter.v清0；DTLB
        // W.valid=0；MemBlock过滤该epoch返回。 测试目的：观察 CSR 改变与 fence 的不同作用及生效延迟，避免把 Filter 清空等同于 storage
        // 失效。
        cp_08_03_002 : coverpoint state_write
            iff (sample_enable
                && csr_satp_changed
                && (!state_write) === 1'b1) {
            bins b_expected_clear = {1'b0};
        }
        // 08.03.003 CSR_FLUSH_vsatp_changed
        // 测试表目标：场景：对应C.vsatp_changed=1，filter有未发/已发entry，P.valid可能同时到达。 观察功能：生效拍filter.v清0；DTLB
        // W.valid=0；MemBlock过滤该epoch返回。 测试目的：观察 CSR 改变与 fence 的不同作用及生效延迟，避免把 Filter 清空等同于 storage
        // 失效。
        cp_08_03_003 : coverpoint state_write
            iff (sample_enable
                && csr_vsatp_changed
                && (!state_write) === 1'b1) {
            bins b_expected_clear = {1'b0};
        }
        // 08.03.004 CSR_FLUSH_hgatp_changed
        // 测试表目标：场景：对应C.hgatp_changed=1，filter有未发/已发entry，P.valid可能同时到达。 观察功能：生效拍filter.v清0；DTLB
        // W.valid=0；MemBlock过滤该epoch返回。 测试目的：观察 CSR 改变与 fence 的不同作用及生效延迟，避免把 Filter 清空等同于 storage
        // 失效。
        cp_08_03_004 : coverpoint state_write
            iff (sample_enable
                && csr_hgatp_changed
                && (!state_write) === 1'b1) {
            bins b_expected_clear = {1'b0};
        }
        // 08.03.005 CSR_FLUSH_priv_virt_changed
        // 测试表目标：场景：对应C.priv_virt_changed=1，filter有未发/已发entry，P.valid可能同时到达。
        // 观察功能：生效拍filter.v清0；DTLB W.valid=0；MemBlock过滤该epoch返回。 测试目的：观察 CSR 改变与 fence
        // 的不同作用及生效延迟，避免把 Filter 清空等同于 storage 失效。
        cp_08_03_005 : coverpoint state_write
            iff (sample_enable
                && csr_virt_changed
                && (!state_write) === 1'b1) {
            bins b_expected_clear = {1'b0};
        }
        // 08.03.006 FENCE_Svinval不取消请求
        // 测试表目标：场景：sfence.valid=1且flushPipe=0，流水仍有合法请求。
        // 观察功能：storage/filter失效；非阻塞DTLB仍按自身valid响应，后续请求可重试。 测试目的：观察 CSR 改变与 fence 的不同作用及生效延迟，避免把
        // Filter 清空等同于 storage 失效。
        cp_08_03_006 : coverpoint (fence_valid)
            iff (sample_enable
                && fence_valid) {
            bins b_observed = {1'b1};
        }
        // 08.03.007 FENCE_flushPipe位
        // 测试表目标：场景：sfence.valid=1,flushPipe=0/1；保持其他相同。 观察功能：本MemBlock DTLB
        // io.flushPipe恒0；不能要求该位直接清req_out_v，流水取消由上游实现。 测试目的：观察 CSR 改变与 fence 的不同作用及生效延迟，避免把 Filter
        // 清空等同于 storage 失效。
        cp_08_03_007 : coverpoint (fence_valid)
            iff (sample_enable
                && fence_valid) {
            bins b_observed = {1'b1};
        }
    endgroup
    cg_dtlb_08_03 cg_dtlb_08_03_inst;

    // 08.05 在途刷新与新旧上下文恢复
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_08_05】08.05 在途刷新与新旧上下文恢复
    // 探测功能：观察刷新前后的在途返回和映射切换，跟踪新请求是否最终使用新上下文。
    // 采样时序：回填/flush直接观察本拍；_d1用于上下文切换、连续回填或下一拍效果。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：08.05.001, 08.05.006；逐点功能见下方注释，精确配值见分解表。
    covergroup cg_dtlb_08_05 @(posedge clock);
        option.per_instance = 1;
        // 08.05.001 FENCE_回填同拍
        // 测试表目标：场景：P.valid与内部flush_mmu同拍。 观察功能：W.valid=0；fence优先清目标v，不能复活旧epoch项。
        // 测试目的：观察刷新前后的在途返回和映射切换，跟踪新请求是否最终使用新上下文。
        cp_08_05_001 : coverpoint state_write
            iff (sample_enable
                && ptw_valid
                && (fence_valid || csr_satp_changed || csr_vsatp_changed || csr_hgatp_changed || csr_virt_changed)
                && (!state_write) === 1'b1) {
            bins b_expected_clear = {1'b0};
        }
        // 08.05.006 FENCE_连续与重叠
        // 测试表目标：场景：两次fence相隔0/1/2/4拍，类型不同。 观察功能：各生效拍均正确执行；mask/ID使用对应延迟payload。
        // 测试目的：观察刷新前后的在途返回和映射切换，跟踪新请求是否最终使用新上下文。
        cp_08_05_006 : coverpoint (fence_valid && fence_valid_d1)
            iff (sample_enable
                && fence_valid
                && fence_valid_d1) {
            bins b_observed = {1'b1};
        }
    endgroup
    cg_dtlb_08_05 cg_dtlb_08_05_inst;

    // 08.06 CSR有向切换与在途窗口
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_08_06】08.06 CSR有向切换与在途窗口
    // 探测功能：观察 CSR 由旧配置切换到新配置的有向窗口，覆盖请求、PTW 等待和 GPA 等待跨越切换的情形。
    // 采样时序：回填/flush直接观察本拍；_d1用于上下文切换、连续回填或下一拍效果。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：08.06.002, 08.06.003, 08.06.005, 08.06.007, 08.06.008, 08.06.009, 08.06.010,
    // 08.06.011, 08.06.012, 08.06.013, 08.06.014, 08.06.015, 08.06.016, 08.06.017, 08.06.018,
    // 08.06.019, 08.06.020, 08.06.021, 08.06.022, 08.06.023, 08.06.024, 08.06.025, 08.06.026,
    // 08.06.027, 08.06.028, 08.06.029, 08.06.030, 08.06.031, 08.06.032, 08.06.033, 08.06.034,
    // 08.06.035, 08.06.036, 08.06.037, 08.06.038, 08.06.039, 08.06.040；逐点功能见下方注释，精确配值见分解表。
    // translation_context：上下文编码：0=Host S1，1=Guest S1，2=onlyS2，3=双阶段，4=Bare。
    covergroup cg_dtlb_08_06 @(posedge clock);
        option.per_instance = 1;
        // 08.06.002 CSR_hgatp_vmid_改变
        // 测试表目标：令
        // hgatp_vmid（Guest虚拟机标识）从一个已知值变为不同的已知值，模拟页表根或地址空间上下文切换。观察顶层到内部CSR的延迟、对应changed脉冲以及请求/Filter/GPA的在途状态；区分新旧上下文，不能仅凭数值改变推定全部缓存已完成失效。
        cp_08_06_002 : coverpoint (history_valid && csr_vmid != csr_vmid_d1)
            iff (sample_enable
                && history_valid
                && csr_vmid != csr_vmid_d1) {
            bins b_observed = {1'b1};
        }
        // 08.06.003 CSR_satp_asid_改变
        // 测试表目标：令
        // satp_asid（Host地址空间标识）从一个已知值变为不同的已知值，模拟页表根或地址空间上下文切换。观察顶层到内部CSR的延迟、对应changed脉冲以及请求/Filter/GPA的在途状态；区分新旧上下文，不能仅凭数值改变推定全部缓存已完成失效。
        cp_08_06_003 : coverpoint (history_valid && csr_asid != csr_asid_d1)
            iff (sample_enable
                && history_valid
                && csr_asid != csr_asid_d1) {
            bins b_observed = {1'b1};
        }
        // 08.06.005 CSR_vsatp_asid_改变
        // 测试表目标：令 vsatp_asid（Guest
        // VS地址空间标识）从一个已知值变为不同的已知值，模拟页表根或地址空间上下文切换。观察顶层到内部CSR的延迟、对应changed脉冲以及请求/Filter/GPA的在途状态；区分新旧上下文，不能仅凭数值改变推定全部缓存已完成失效。
        cp_08_06_005 : coverpoint (history_valid && csr_vasid != csr_vasid_d1)
            iff (sample_enable
                && history_valid
                && csr_vasid != csr_vasid_d1) {
            bins b_observed = {1'b1};
        }
        // 08.06.007 CSR_dmode_0到1
        // 测试表目标：将 dmode 从0（U 有效访存特权）切换到1（S 有效访存特权）。在切换两侧保存请求 token 和实际延迟
        // CSR，观察请求所使用的特权/虚拟化上下文，区分请求拍锁存字段与响应拍读取的控制字段。
        cp_08_06_007 : coverpoint csr_dmode
            iff (sample_enable
                && history_valid
                && csr_dmode_d1 == 0
                && csr_dmode == 1) {
            bins b_changed_to = {1};
        }
        // 08.06.008 CSR_dmode_1到0
        // 测试表目标：将 dmode 从1（S 有效访存特权）切换到0（U 有效访存特权）。在切换两侧保存请求 token 和实际延迟
        // CSR，观察请求所使用的特权/虚拟化上下文，区分请求拍锁存字段与响应拍读取的控制字段。
        cp_08_06_008 : coverpoint csr_dmode
            iff (sample_enable
                && history_valid
                && csr_dmode_d1 == 1
                && csr_dmode == 0) {
            bins b_changed_to = {0};
        }
        // 08.06.009 CSR_hgatp_mode_0到8
        // 测试表目标：将 hgatp.mode 从0（Bare，关闭本阶段页表翻译）切换到8（Sv39x4，41位 GPA）。保存切换前后的请求及在途状态，观察 changed
        // 信号、CSR 延迟、Filter 清空和回填抑制，再观察新请求采用的新地址检查与翻译规则。仅 changed 不能作为 storage 已失效的证明。
        cp_08_06_009 : coverpoint csr_hgatp_mode
            iff (sample_enable
                && history_valid
                && csr_hgatp_mode_d1 == 0
                && csr_hgatp_mode == 8) {
            bins b_changed_to = {8};
        }
        // 08.06.010 CSR_hgatp_mode_0到9
        // 测试表目标：将 hgatp.mode 从0（Bare，关闭本阶段页表翻译）切换到9（Sv48x4，50位 GPA）。保存切换前后的请求及在途状态，观察 changed
        // 信号、CSR 延迟、Filter 清空和回填抑制，再观察新请求采用的新地址检查与翻译规则。仅 changed 不能作为 storage 已失效的证明。
        cp_08_06_010 : coverpoint csr_hgatp_mode
            iff (sample_enable
                && history_valid
                && csr_hgatp_mode_d1 == 0
                && csr_hgatp_mode == 9) {
            bins b_changed_to = {9};
        }
        // 08.06.011 CSR_hgatp_mode_8到0
        // 测试表目标：将 hgatp.mode 从8（Sv39x4，41位 GPA）切换到0（Bare，关闭本阶段页表翻译）。保存切换前后的请求及在途状态，观察 changed
        // 信号、CSR 延迟、Filter 清空和回填抑制，再观察新请求采用的新地址检查与翻译规则。仅 changed 不能作为 storage 已失效的证明。
        cp_08_06_011 : coverpoint csr_hgatp_mode
            iff (sample_enable
                && history_valid
                && csr_hgatp_mode_d1 == 8
                && csr_hgatp_mode == 0) {
            bins b_changed_to = {0};
        }
        // 08.06.012 CSR_hgatp_mode_8到9
        // 测试表目标：将 hgatp.mode 从8（Sv39x4，41位 GPA）切换到9（Sv48x4，50位 GPA）。保存切换前后的请求及在途状态，观察 changed
        // 信号、CSR 延迟、Filter 清空和回填抑制，再观察新请求采用的新地址检查与翻译规则。仅 changed 不能作为 storage 已失效的证明。
        cp_08_06_012 : coverpoint csr_hgatp_mode
            iff (sample_enable
                && history_valid
                && csr_hgatp_mode_d1 == 8
                && csr_hgatp_mode == 9) {
            bins b_changed_to = {9};
        }
        // 08.06.013 CSR_hgatp_mode_9到0
        // 测试表目标：将 hgatp.mode 从9（Sv48x4，50位 GPA）切换到0（Bare，关闭本阶段页表翻译）。保存切换前后的请求及在途状态，观察 changed
        // 信号、CSR 延迟、Filter 清空和回填抑制，再观察新请求采用的新地址检查与翻译规则。仅 changed 不能作为 storage 已失效的证明。
        cp_08_06_013 : coverpoint csr_hgatp_mode
            iff (sample_enable
                && history_valid
                && csr_hgatp_mode_d1 == 9
                && csr_hgatp_mode == 0) {
            bins b_changed_to = {0};
        }
        // 08.06.014 CSR_hgatp_mode_9到8
        // 测试表目标：将 hgatp.mode 从9（Sv48x4，50位 GPA）切换到8（Sv39x4，41位 GPA）。保存切换前后的请求及在途状态，观察 changed
        // 信号、CSR 延迟、Filter 清空和回填抑制，再观察新请求采用的新地址检查与翻译规则。仅 changed 不能作为 storage 已失效的证明。
        cp_08_06_014 : coverpoint csr_hgatp_mode
            iff (sample_enable
                && history_valid
                && csr_hgatp_mode_d1 == 9
                && csr_hgatp_mode == 8) {
            bins b_changed_to = {8};
        }
        // 08.06.015 CSR_satp_mode_0到8
        // 测试表目标：将 satp.mode 从0（Bare，关闭本阶段页表翻译）切换到8（Sv39，39位规范虚拟地址）。保存切换前后的请求及在途状态，观察 changed
        // 信号、CSR 延迟、Filter 清空和回填抑制，再观察新请求采用的新地址检查与翻译规则。仅 changed 不能作为 storage 已失效的证明。
        cp_08_06_015 : coverpoint csr_satp_mode
            iff (sample_enable
                && history_valid
                && csr_satp_mode_d1 == 0
                && csr_satp_mode == 8) {
            bins b_changed_to = {8};
        }
        // 08.06.016 CSR_satp_mode_0到9
        // 测试表目标：将 satp.mode 从0（Bare，关闭本阶段页表翻译）切换到9（Sv48，48位规范虚拟地址）。保存切换前后的请求及在途状态，观察 changed
        // 信号、CSR 延迟、Filter 清空和回填抑制，再观察新请求采用的新地址检查与翻译规则。仅 changed 不能作为 storage 已失效的证明。
        cp_08_06_016 : coverpoint csr_satp_mode
            iff (sample_enable
                && history_valid
                && csr_satp_mode_d1 == 0
                && csr_satp_mode == 9) {
            bins b_changed_to = {9};
        }
        // 08.06.017 CSR_satp_mode_8到0
        // 测试表目标：将 satp.mode 从8（Sv39，39位规范虚拟地址）切换到0（Bare，关闭本阶段页表翻译）。保存切换前后的请求及在途状态，观察 changed
        // 信号、CSR 延迟、Filter 清空和回填抑制，再观察新请求采用的新地址检查与翻译规则。仅 changed 不能作为 storage 已失效的证明。
        cp_08_06_017 : coverpoint csr_satp_mode
            iff (sample_enable
                && history_valid
                && csr_satp_mode_d1 == 8
                && csr_satp_mode == 0) {
            bins b_changed_to = {0};
        }
        // 08.06.018 CSR_satp_mode_8到9
        // 测试表目标：将 satp.mode 从8（Sv39，39位规范虚拟地址）切换到9（Sv48，48位规范虚拟地址）。保存切换前后的请求及在途状态，观察 changed
        // 信号、CSR 延迟、Filter 清空和回填抑制，再观察新请求采用的新地址检查与翻译规则。仅 changed 不能作为 storage 已失效的证明。
        cp_08_06_018 : coverpoint csr_satp_mode
            iff (sample_enable
                && history_valid
                && csr_satp_mode_d1 == 8
                && csr_satp_mode == 9) {
            bins b_changed_to = {9};
        }
        // 08.06.019 CSR_satp_mode_9到0
        // 测试表目标：将 satp.mode 从9（Sv48，48位规范虚拟地址）切换到0（Bare，关闭本阶段页表翻译）。保存切换前后的请求及在途状态，观察 changed
        // 信号、CSR 延迟、Filter 清空和回填抑制，再观察新请求采用的新地址检查与翻译规则。仅 changed 不能作为 storage 已失效的证明。
        cp_08_06_019 : coverpoint csr_satp_mode
            iff (sample_enable
                && history_valid
                && csr_satp_mode_d1 == 9
                && csr_satp_mode == 0) {
            bins b_changed_to = {0};
        }
        // 08.06.020 CSR_satp_mode_9到8
        // 测试表目标：将 satp.mode 从9（Sv48，48位规范虚拟地址）切换到8（Sv39，39位规范虚拟地址）。保存切换前后的请求及在途状态，观察 changed
        // 信号、CSR 延迟、Filter 清空和回填抑制，再观察新请求采用的新地址检查与翻译规则。仅 changed 不能作为 storage 已失效的证明。
        cp_08_06_020 : coverpoint csr_satp_mode
            iff (sample_enable
                && history_valid
                && csr_satp_mode_d1 == 9
                && csr_satp_mode == 8) {
            bins b_changed_to = {8};
        }
        // 08.06.021 CSR翻译阶段_ALL到HOST
        // 测试表目标：将请求上下文从Guest VS+G 两阶段翻译（stage=3）切换为Host S 阶段翻译（stage=0）。对切换前后相同虚拟地址的请求保存独立
        // epoch，观察 changed/fence 生效窗口及新请求的阶段、页号和权限来源，覆盖旧请求或旧返回仍在途时的上下文隔离。
        cp_08_06_021 : coverpoint (history_valid && translation_context_d1 == 3 && translation_context == 0)
            iff (sample_enable
                && history_valid
                && translation_context_d1 == 3
                && translation_context == 0) {
            bins b_observed = {1'b1};
        }
        // 08.06.022 CSR翻译阶段_ALL到S1
        // 测试表目标：将请求上下文从Guest VS+G 两阶段翻译（stage=3）切换为Guest 仅 VS 翻译（stage=1）。对切换前后相同虚拟地址的请求保存独立
        // epoch，观察 changed/fence 生效窗口及新请求的阶段、页号和权限来源，覆盖旧请求或旧返回仍在途时的上下文隔离。
        cp_08_06_022 : coverpoint (history_valid && translation_context_d1 == 3 && translation_context == 1)
            iff (sample_enable
                && history_valid
                && translation_context_d1 == 3
                && translation_context == 1) {
            bins b_observed = {1'b1};
        }
        // 08.06.023 CSR翻译阶段_ALL到S2
        // 测试表目标：将请求上下文从Guest VS+G 两阶段翻译（stage=3）切换为Guest 仅 G 翻译（stage=2）。对切换前后相同虚拟地址的请求保存独立
        // epoch，观察 changed/fence 生效窗口及新请求的阶段、页号和权限来源，覆盖旧请求或旧返回仍在途时的上下文隔离。
        cp_08_06_023 : coverpoint (history_valid && translation_context_d1 == 3 && translation_context == 2)
            iff (sample_enable
                && history_valid
                && translation_context_d1 == 3
                && translation_context == 2) {
            bins b_observed = {1'b1};
        }
        // 08.06.024 CSR翻译阶段_HOST到ALL
        // 测试表目标：将请求上下文从Host S 阶段翻译（stage=0）切换为Guest VS+G 两阶段翻译（stage=3）。对切换前后相同虚拟地址的请求保存独立
        // epoch，观察 changed/fence 生效窗口及新请求的阶段、页号和权限来源，覆盖旧请求或旧返回仍在途时的上下文隔离。
        cp_08_06_024 : coverpoint (history_valid && translation_context_d1 == 0 && translation_context == 3)
            iff (sample_enable
                && history_valid
                && translation_context_d1 == 0
                && translation_context == 3) {
            bins b_observed = {1'b1};
        }
        // 08.06.025 CSR翻译阶段_HOST到S1
        // 测试表目标：将请求上下文从Host S 阶段翻译（stage=0）切换为Guest 仅 VS 翻译（stage=1）。对切换前后相同虚拟地址的请求保存独立 epoch，观察
        // changed/fence 生效窗口及新请求的阶段、页号和权限来源，覆盖旧请求或旧返回仍在途时的上下文隔离。
        cp_08_06_025 : coverpoint (history_valid && translation_context_d1 == 0 && translation_context == 1)
            iff (sample_enable
                && history_valid
                && translation_context_d1 == 0
                && translation_context == 1) {
            bins b_observed = {1'b1};
        }
        // 08.06.026 CSR翻译阶段_HOST到S2
        // 测试表目标：将请求上下文从Host S 阶段翻译（stage=0）切换为Guest 仅 G 翻译（stage=2）。对切换前后相同虚拟地址的请求保存独立 epoch，观察
        // changed/fence 生效窗口及新请求的阶段、页号和权限来源，覆盖旧请求或旧返回仍在途时的上下文隔离。
        cp_08_06_026 : coverpoint (history_valid && translation_context_d1 == 0 && translation_context == 2)
            iff (sample_enable
                && history_valid
                && translation_context_d1 == 0
                && translation_context == 2) {
            bins b_observed = {1'b1};
        }
        // 08.06.027 CSR翻译阶段_S1到ALL
        // 测试表目标：将请求上下文从Guest 仅 VS 翻译（stage=1）切换为Guest VS+G 两阶段翻译（stage=3）。对切换前后相同虚拟地址的请求保存独立
        // epoch，观察 changed/fence 生效窗口及新请求的阶段、页号和权限来源，覆盖旧请求或旧返回仍在途时的上下文隔离。
        cp_08_06_027 : coverpoint (history_valid && translation_context_d1 == 1 && translation_context == 3)
            iff (sample_enable
                && history_valid
                && translation_context_d1 == 1
                && translation_context == 3) {
            bins b_observed = {1'b1};
        }
        // 08.06.028 CSR翻译阶段_S1到HOST
        // 测试表目标：将请求上下文从Guest 仅 VS 翻译（stage=1）切换为Host S 阶段翻译（stage=0）。对切换前后相同虚拟地址的请求保存独立 epoch，观察
        // changed/fence 生效窗口及新请求的阶段、页号和权限来源，覆盖旧请求或旧返回仍在途时的上下文隔离。
        cp_08_06_028 : coverpoint (history_valid && translation_context_d1 == 1 && translation_context == 0)
            iff (sample_enable
                && history_valid
                && translation_context_d1 == 1
                && translation_context == 0) {
            bins b_observed = {1'b1};
        }
        // 08.06.029 CSR翻译阶段_S1到S2
        // 测试表目标：将请求上下文从Guest 仅 VS 翻译（stage=1）切换为Guest 仅 G 翻译（stage=2）。对切换前后相同虚拟地址的请求保存独立 epoch，观察
        // changed/fence 生效窗口及新请求的阶段、页号和权限来源，覆盖旧请求或旧返回仍在途时的上下文隔离。
        cp_08_06_029 : coverpoint (history_valid && translation_context_d1 == 1 && translation_context == 2)
            iff (sample_enable
                && history_valid
                && translation_context_d1 == 1
                && translation_context == 2) {
            bins b_observed = {1'b1};
        }
        // 08.06.030 CSR翻译阶段_S2到ALL
        // 测试表目标：将请求上下文从Guest 仅 G 翻译（stage=2）切换为Guest VS+G 两阶段翻译（stage=3）。对切换前后相同虚拟地址的请求保存独立
        // epoch，观察 changed/fence 生效窗口及新请求的阶段、页号和权限来源，覆盖旧请求或旧返回仍在途时的上下文隔离。
        cp_08_06_030 : coverpoint (history_valid && translation_context_d1 == 2 && translation_context == 3)
            iff (sample_enable
                && history_valid
                && translation_context_d1 == 2
                && translation_context == 3) {
            bins b_observed = {1'b1};
        }
        // 08.06.031 CSR翻译阶段_S2到HOST
        // 测试表目标：将请求上下文从Guest 仅 G 翻译（stage=2）切换为Host S 阶段翻译（stage=0）。对切换前后相同虚拟地址的请求保存独立 epoch，观察
        // changed/fence 生效窗口及新请求的阶段、页号和权限来源，覆盖旧请求或旧返回仍在途时的上下文隔离。
        cp_08_06_031 : coverpoint (history_valid && translation_context_d1 == 2 && translation_context == 0)
            iff (sample_enable
                && history_valid
                && translation_context_d1 == 2
                && translation_context == 0) {
            bins b_observed = {1'b1};
        }
        // 08.06.032 CSR翻译阶段_S2到S1
        // 测试表目标：将请求上下文从Guest 仅 G 翻译（stage=2）切换为Guest 仅 VS 翻译（stage=1）。对切换前后相同虚拟地址的请求保存独立 epoch，观察
        // changed/fence 生效窗口及新请求的阶段、页号和权限来源，覆盖旧请求或旧返回仍在途时的上下文隔离。
        cp_08_06_032 : coverpoint (history_valid && translation_context_d1 == 2 && translation_context == 1)
            iff (sample_enable
                && history_valid
                && translation_context_d1 == 2
                && translation_context == 1) {
            bins b_observed = {1'b1};
        }
        // 08.06.033 CSR_virt_0到1
        // 测试表目标：将 virt 从0（非虚拟化 Host）切换到1（Guest 虚拟化状态）。在切换两侧保存请求 token 和实际延迟
        // CSR，观察请求所使用的特权/虚拟化上下文，区分请求拍锁存字段与响应拍读取的控制字段。
        cp_08_06_033 : coverpoint csr_virt
            iff (sample_enable
                && history_valid
                && csr_virt_d1 == 0
                && csr_virt == 1) {
            bins b_changed_to = {1};
        }
        // 08.06.034 CSR_virt_1到0
        // 测试表目标：将 virt 从1（Guest 虚拟化状态）切换到0（非虚拟化 Host）。在切换两侧保存请求 token 和实际延迟
        // CSR，观察请求所使用的特权/虚拟化上下文，区分请求拍锁存字段与响应拍读取的控制字段。
        cp_08_06_034 : coverpoint csr_virt
            iff (sample_enable
                && history_valid
                && csr_virt_d1 == 1
                && csr_virt == 0) {
            bins b_changed_to = {0};
        }
        // 08.06.035 CSR_vsatp_mode_0到8
        // 测试表目标：将 vsatp.mode 从0（Bare，关闭本阶段页表翻译）切换到8（Sv39，39位规范虚拟地址）。保存切换前后的请求及在途状态，观察 changed
        // 信号、CSR 延迟、Filter 清空和回填抑制，再观察新请求采用的新地址检查与翻译规则。仅 changed 不能作为 storage 已失效的证明。
        cp_08_06_035 : coverpoint csr_vsatp_mode
            iff (sample_enable
                && history_valid
                && csr_vsatp_mode_d1 == 0
                && csr_vsatp_mode == 8) {
            bins b_changed_to = {8};
        }
        // 08.06.036 CSR_vsatp_mode_0到9
        // 测试表目标：将 vsatp.mode 从0（Bare，关闭本阶段页表翻译）切换到9（Sv48，48位规范虚拟地址）。保存切换前后的请求及在途状态，观察 changed
        // 信号、CSR 延迟、Filter 清空和回填抑制，再观察新请求采用的新地址检查与翻译规则。仅 changed 不能作为 storage 已失效的证明。
        cp_08_06_036 : coverpoint csr_vsatp_mode
            iff (sample_enable
                && history_valid
                && csr_vsatp_mode_d1 == 0
                && csr_vsatp_mode == 9) {
            bins b_changed_to = {9};
        }
        // 08.06.037 CSR_vsatp_mode_8到0
        // 测试表目标：将 vsatp.mode 从8（Sv39，39位规范虚拟地址）切换到0（Bare，关闭本阶段页表翻译）。保存切换前后的请求及在途状态，观察 changed
        // 信号、CSR 延迟、Filter 清空和回填抑制，再观察新请求采用的新地址检查与翻译规则。仅 changed 不能作为 storage 已失效的证明。
        cp_08_06_037 : coverpoint csr_vsatp_mode
            iff (sample_enable
                && history_valid
                && csr_vsatp_mode_d1 == 8
                && csr_vsatp_mode == 0) {
            bins b_changed_to = {0};
        }
        // 08.06.038 CSR_vsatp_mode_8到9
        // 测试表目标：将 vsatp.mode 从8（Sv39，39位规范虚拟地址）切换到9（Sv48，48位规范虚拟地址）。保存切换前后的请求及在途状态，观察 changed
        // 信号、CSR 延迟、Filter 清空和回填抑制，再观察新请求采用的新地址检查与翻译规则。仅 changed 不能作为 storage 已失效的证明。
        cp_08_06_038 : coverpoint csr_vsatp_mode
            iff (sample_enable
                && history_valid
                && csr_vsatp_mode_d1 == 8
                && csr_vsatp_mode == 9) {
            bins b_changed_to = {9};
        }
        // 08.06.039 CSR_vsatp_mode_9到0
        // 测试表目标：将 vsatp.mode 从9（Sv48，48位规范虚拟地址）切换到0（Bare，关闭本阶段页表翻译）。保存切换前后的请求及在途状态，观察 changed
        // 信号、CSR 延迟、Filter 清空和回填抑制，再观察新请求采用的新地址检查与翻译规则。仅 changed 不能作为 storage 已失效的证明。
        cp_08_06_039 : coverpoint csr_vsatp_mode
            iff (sample_enable
                && history_valid
                && csr_vsatp_mode_d1 == 9
                && csr_vsatp_mode == 0) {
            bins b_changed_to = {0};
        }
        // 08.06.040 CSR_vsatp_mode_9到8
        // 测试表目标：将 vsatp.mode 从9（Sv48，48位规范虚拟地址）切换到8（Sv39，39位规范虚拟地址）。保存切换前后的请求及在途状态，观察 changed
        // 信号、CSR 延迟、Filter 清空和回填抑制，再观察新请求采用的新地址检查与翻译规则。仅 changed 不能作为 storage 已失效的证明。
        cp_08_06_040 : coverpoint csr_vsatp_mode
            iff (sample_enable
                && history_valid
                && csr_vsatp_mode_d1 == 9
                && csr_vsatp_mode == 8) {
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
