// 阅读约定：所有covergroup在posedge观察NBA更新前的值；_d1/_d2是模型历史。
// 简单事件门控已就地展开；保留的参考计算有中文合同，bind给出各端口的RTL来源。
// 对应表：Mem_env_work_20260716/TLB_fcov/DTLB_readability_20261016/DTLB_测试点与功能仓对应表.xlsx
//=========================================================
// memblock_dtlb_core_fcov：DTLB V2 只读功能覆盖率。
// 普通信号端口、时钟事件采样、逐特性直接建仓；没有结构体库、function 或采样分发器。
// 时延与长度采用范围仓；物理 way/slot/子页采用逐项仓。
//=========================================================
`ifndef MEMBLOCK_DTLB_CORE_FCOV__SV
`define MEMBLOCK_DTLB_CORE_FCOV__SV

`ifdef MEMBLOCK_UT_FCOV
module memblock_dtlb_core_fcov #(
    parameter integer GROUP_ID   = 0,
    parameter integer PORT_INDEX = 0,
    parameter bit HAS_FULLVA     = 1'b1,
    parameter bit HAS_GPA        = 1'b1,
    parameter bit HAS_PADDR1     = 1'b1,
    parameter bit HAS_LATE_KILL  = 1'b1,
    parameter bit HAS_HLVX       = 1'b1
) (
    input wire clock,
    input wire reset,
    input wire reset_backend_done,
    input wire req_valid,
    input wire req_kill,
    input wire req_late_kill,
    input wire [49:0] req_vaddr,
    input wire [63:0] req_fullva,
    input wire req_checkfullva,
    input wire [2:0] req_cmd,
    input wire req_hyperinst,
    input wire req_hlvx,
    input wire req_prefetch,
    input wire req_no_translate,
    input wire [47:0] req_pmpaddr,
    input wire req_mabuf,
    input wire [8:0] req_rob,
    input wire [63:0] req_effective_va,
    input wire req_pre_s1,
    input wire req_pre_s2,
    input wire [1:0] req_stage,
    input wire [1:0] req_pmm,
    input wire req_prepf,
    input wire req_pregpf,
    input wire req_preaf,
    input wire req_virt,
    input wire req_lookup,
    input wire resp_valid,
    input wire resp_miss,
    input wire [47:0] resp_paddr_0,
    input wire [47:0] resp_paddr_1,
    input wire [63:0] resp_gpaddr,
    input wire [63:0] resp_fullva,
    input wire [1:0] resp_pbmt,
    input wire [5:0] resp_excp,
    input wire resp_ext,
    input wire resp_hyper,
    input wire resp_nonleaf,
    input wire [49:0] req_out_vaddr,
    input wire [2:0] req_out_cmd,
    input wire [1:0] req_out_s2xlate,
    input wire [1:0] effective_mode,
    input wire resp_virt,
    input wire translate_enable,
    input wire storage_hit,
    input wire ptw_bypass_hit,
    input wire ptw_bypass_hit_fast,
    input wire resp_miss_read,
    input wire resp_need_replay,
    input wire resp_can_gpa,
    input wire resp_has_gpf,
    input wire resp_last_red,
    input wire resp_qvalid,
    input wire [37:0] resp_qvpn,
    input wire [1:0] resp_qstage,
    input wire resp_qgpa,
    input wire resp_replay,
    input wire resp_pmpvalid,
    input wire [47:0] resp_pmpaddr,
    input wire [2:0] resp_pmpcmd,
    input wire [1:0] resp_pmpmode,
    input wire [35:0] resp_ppn,
    input wire [1:0] resp_s1pbmt,
    input wire [1:0] resp_s2pbmt,
    input wire resp_s1_pf,
    input wire resp_s1_af,
    input wire resp_s1_v,
    input wire resp_s1_d,
    input wire resp_s1_a,
    input wire resp_s1_u,
    input wire resp_s1_x,
    input wire resp_s1_w,
    input wire resp_s1_r,
    input wire resp_s2_pf,
    input wire resp_s2_af,
    input wire resp_s2_d,
    input wire resp_s2_a,
    input wire resp_s2_x,
    input wire resp_s2_w,
    input wire resp_s2_r,
    input wire [3:0] csr_satp_mode,
    input wire [15:0] csr_asid,
    input wire csr_satp_changed,
    input wire [3:0] csr_vsatp_mode,
    input wire [15:0] csr_vasid,
    input wire csr_vsatp_changed,
    input wire [3:0] csr_hgatp_mode,
    input wire [15:0] csr_vmid,
    input wire csr_hgatp_changed,
    input wire csr_mxr,
    input wire csr_sum,
    input wire csr_vmxr,
    input wire csr_vsum,
    input wire csr_virt,
    input wire csr_virt_changed,
    input wire csr_spvp,
    input wire [1:0] csr_imode,
    input wire [1:0] csr_dmode,
    input wire csr_bme,
    input wire csr_cmode,
    input wire [1:0] csr_mseccfg,
    input wire [1:0] csr_menvcfg,
    input wire [1:0] csr_henvcfg,
    input wire [1:0] csr_hstatus,
    input wire [1:0] csr_senvcfg,
    input wire ptw_valid,
    input wire [1:0] ptw_stage,
    input wire ptw_getgpa,
    input wire [34:0] ptw_s1tag,
    input wire [15:0] ptw_s1asid,
    input wire [13:0] ptw_s1vmid,
    input wire [1:0] ptw_s1level,
    input wire ptw_s1n,
    input wire ptw_s1_g,
    input wire [2:0] ptw_s1low,
    input wire [7:0] ptw_valididx,
    input wire [37:0] ptw_s2tag,
    input wire [13:0] ptw_s2vmid,
    input wire [1:0] ptw_s2level,
    input wire ptw_s2n,
    input wire ptw_s2_g,
    input wire ptw_s2_u,
    input wire gpa_write,
    input wire gpa_ng,
    input wire gpa_ng_wire,
    input wire [8:0] gpa_count,
    input wire gpa_clear,
    input wire gpa_owner_valid,
    input wire [8:0] gpa_owner,
    input wire [37:0] gpa_vpn,
    input wire [1:0] gpa_stage,
    input wire gpa_response_ready,
    input wire [1:0] gpa_s1level,
    input wire gpa_s1fake,
    input wire redirect_valid,
    input wire redirect_level,
    input wire [8:0] redirect_rob,
    input wire [8:0] rob_head,
    input wire [47:0] storage_hit_vector,
    input wire [47:0][1:0] storage_levels,
    input wire [47:0] storage_napot
);

    // 请求/CSR快照：每个posedge以NBA保存；同一posedge的覆盖点看到旧值，_d1对应前拍请求。
    reg req_valid_d1;
    reg req_kill_d1;
    reg req_late_kill_d1;
    reg [49:0] req_vaddr_d1;
    reg [63:0] req_fullva_d1;
    reg req_checkfullva_d1;
    reg [2:0] req_cmd_d1;
    reg req_hyperinst_d1;
    reg req_hlvx_d1;
    reg req_prefetch_d1;
    reg req_no_translate_d1;
    reg [47:0] req_pmpaddr_d1;
    reg req_mabuf_d1;
    reg [8:0] req_rob_d1;
    reg [63:0] req_effective_va_d1;
    reg req_pre_s1_d1;
    reg req_pre_s2_d1;
    reg [1:0] req_stage_d1;
    reg [1:0] req_pmm_d1;
    reg req_prepf_d1;
    reg req_pregpf_d1;
    reg req_preaf_d1;
    reg req_virt_d1;
    reg req_lookup_d1;
    reg [3:0] csr_satp_mode_d1;
    reg [15:0] csr_asid_d1;
    reg csr_satp_changed_d1;
    reg [3:0] csr_vsatp_mode_d1;
    reg [15:0] csr_vasid_d1;
    reg csr_vsatp_changed_d1;
    reg [3:0] csr_hgatp_mode_d1;
    reg [15:0] csr_vmid_d1;
    reg csr_hgatp_changed_d1;
    reg csr_mxr_d1;
    reg csr_sum_d1;
    reg csr_vmxr_d1;
    reg csr_vsum_d1;
    reg csr_virt_d1;
    reg csr_virt_changed_d1;
    reg csr_spvp_d1;
    reg [1:0] csr_imode_d1;
    reg [1:0] csr_dmode_d1;
    reg csr_bme_d1;
    reg csr_cmode_d1;
    reg [1:0] csr_mseccfg_d1;
    reg [1:0] csr_menvcfg_d1;
    reg [1:0] csr_henvcfg_d1;
    reg [1:0] csr_hstatus_d1;
    reg [1:0] csr_senvcfg_d1;
    reg req_valid_d2;
    reg [31:0] accepted_run_length;
    reg [31:0] accepted_run_length_d1;
    reg gpa_ng_d1;
    reg gpa_ready_d1;
    reg [8:0] gpa_count_d1;
    reg [37:0] gpa_vpn_d1;
    reg [1:0] gpa_stage_d1;
    reg ptw_valid_d1;
    reg [1:0] ptw_stage_d1;
    reg [1:0] ptw_s1level_d1;
    reg [1:0] ptw_s2level_d1;
    reg ptw_s1n_d1;
    reg ptw_s2n_d1;
    reg [34:0] ptw_s1tag_d1;
    reg [37:0] ptw_s2tag_d1;
    reg [2:0] ptw_s1low_d1;
    reg [7:0] ptw_valididx_d1;
    // 跨拍历史：只记录固定容量的最近miss/返回/响应；miss_pending按同VA+ROB完成或256拍超时退出。
    reg [31:0] miss_age;
    reg miss_pending;
    reg [49:0] miss_vaddr;
    reg [8:0] miss_rob;
    reg [31:0] retry_count;
    reg return_seen;
    reg history_valid;
    reg [49:0] req_vaddr_d2;
    reg previous_return_matches;
    reg [49:0] last_vaddr;
    reg [63:0] last_fullva;
    reg [47:0] last_paddr;
    reg [8:0] last_rob;
    reg [2:0] last_fault;
    reg [1:0] last_pbmt;
    reg last_response_valid;
    reg last_bypass;
    reg [3:0] permission_controls_d1;
    reg [47:0] context_ids_d1;
    reg [1:0] s2_ug_d1;
    reg return_older;
    reg bypass_fast_d1;
    reg ptw_getgpa_d1;
    reg owner_redirected_d1;
    reg redirect_valid_d1;
    reg fast_gpa_start_d1;
    reg gpa_start_d1;
    reg ptw_getgpa_event_d1;
    reg flushed_epoch;
    reg refill_seen;
    // 聚合/历史 selected_level：按实际hit向量聚合页级；多hit时按位OR，不能当作唯一选中项。
    reg [1:0] selected_level;
    // 聚合/历史 selected_napot：按实际hit向量聚合NAPOT位。
    reg selected_napot;
    // 判据 sample_enable：仅在 reset 明确为0且后端复位完成明确为1时采样；X/Z不算有效复位状态。
    wire sample_enable = (reset === 1'b0) && (reset_backend_done === 1'b1);

    // 判据 pre_fault：请求拍 checkfullva 有效，且预检查 PF/GPF/AF 中至少一种成立；均使用_d1快照。
    wire pre_fault = req_checkfullva_d1 && (req_prepf_d1 || req_pregpf_d1 || req_preaf_d1);

    // 判据 access_fault：仅检查本次启用的翻译阶段：S1.af 或 S2.af；不启用的阶段不产生AF。
    wire access_fault = ((translate_enable && req_out_s2xlate != 2'd2) && resp_s1_af) || ((translate_enable &&
        req_out_s2xlate >= 2'd2) && resp_s2_af);
    // 判据 fake_pte：S1的V/PF/AF均为0且不是onlyS2；用于区分假PTE分支。
    wire fake_pte = !resp_s1_v && !resp_s1_pf && !resp_s1_af && req_out_s2xlate != 2'd2;
    // 判据 nonleaf_pte：S1的V为1、R/W/X/PF/AF均为0；表示非叶PTE。
    wire nonleaf_pte = resp_s1_v && !resp_s1_r && !resp_s1_w && !resp_s1_x && !resp_s1_pf && !resp_s1_af;

    // 判据 mode_permission：按本拍mode、S1.U与有效SUM判断U/S访问许可；Guest/Hyper选择vsum。
    wire mode_permission = !(effective_mode == 2'd0 && !resp_s1_u) && !(effective_mode == 2'd1 && resp_s1_u &&
        !((resp_virt || req_hyperinst_d1) ? csr_vsum : csr_sum));
    // 判据 s1_read_permission：HLVX检查S1.X；普通读检查S1.R或MXR允许的S1.X。
    wire s1_read_permission = req_hlvx_d1 ? resp_s1_x : (resp_s1_r || ((csr_mxr || ((resp_virt ||
        req_hyperinst_d1) && csr_vmxr)) && resp_s1_x));
    // 判据 s2_read_permission：HLVX检查S2.X；普通读检查S2.R或host MXR允许的S2.X。
    wire s2_read_permission = req_hlvx_d1 ? resp_s2_x : (resp_s2_r || (csr_mxr && resp_s2_x));
    // 判据 s1_page_fault：S1页故障参考值：启用S1、排除fake/nonleaf后，检查命令、特权、R/W/X/A/D和上游PF。
    wire s1_page_fault = (translate_enable && req_out_s2xlate != 2'd2) && !(!resp_s1_v && !resp_s1_pf &&
        !resp_s1_af && req_out_s2xlate != 2'd2) && !(resp_s1_v && !resp_s1_r && !resp_s1_w && !resp_s1_x &&
        !resp_s1_pf && !resp_s1_af) && (((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) && (!(!(effective_mode ==
        2'd0 && !resp_s1_u) && !(effective_mode == 2'd1 && resp_s1_u && !((resp_virt || req_hyperinst_d1) ?
        csr_vsum : csr_sum))) || !(req_hlvx_d1 ? resp_s1_x : (resp_s1_r || ((csr_mxr || ((resp_virt ||
        req_hyperinst_d1) && csr_vmxr)) && resp_s1_x))) || !resp_s1_a || resp_s1_pf)) || ((req_out_cmd == 3'd1 ||
        req_out_cmd == 3'd5) && (!(!(effective_mode == 2'd0 && !resp_s1_u) && !(effective_mode == 2'd1 &&
        resp_s1_u && !((resp_virt || req_hyperinst_d1) ? csr_vsum : csr_sum))) || !resp_s1_w || !resp_s1_a ||
        !resp_s1_d || resp_s1_pf)));
    // 判据 s2_page_fault：S2页故障参考值：启用S2后检查命令、R/W/X/A/D和上游PF；优先级由expected_fault处理。
    wire s2_page_fault = (translate_enable && req_out_s2xlate >= 2'd2) && (((req_out_cmd == 3'd0 || req_out_cmd ==
        3'd4) && (!(req_hlvx_d1 ? resp_s2_x : (resp_s2_r || (csr_mxr && resp_s2_x))) || !resp_s2_a || resp_s2_pf))
        || ((req_out_cmd == 3'd1 || req_out_cmd == 3'd5) && (!resp_s2_w || !resp_s2_a || !resp_s2_d ||
        resp_s2_pf)));
    // 判据 expected_fault：参考异常向量[2:0]={AF,GPF,PF}：预异常优先；无hit为0；有hit时AF优先于PF、PF优先于GPF。
    wire [2:0] expected_fault = pre_fault ? {req_preaf_d1, req_pregpf_d1, req_prepf_d1} :
        !(storage_hit || ptw_bypass_hit) ? 3'b000 : {access_fault, s2_page_fault && !access_fault && !s1_page_fault,
        s1_page_fault && !access_fault};
    // 判据 actual_fault：从实际输出resp_excp聚合[2:0]={AF,GPF,PF}，各类分别对load/store位做OR。
    wire [2:0] actual_fault = {(|resp_excp[5:4]), (|resp_excp[3:2]), (|resp_excp[1:0])};

    // 判据 expected_pbmt：参考PBMT：直通为0；onlyS2取S2；双阶段且S2非0时取S2；其余取S1。
    wire [1:0] expected_pbmt = !translate_enable ? 2'd0 : req_out_s2xlate == 2'd2 ? resp_s2pbmt :
        req_out_s2xlate == 2'd3 && resp_s2pbmt != 0 ? resp_s2pbmt : resp_s1pbmt;
    // 判据 hit_source：命中来源编码：0=storage，1=PTW旁路，2=两者均未命中；旁路优先。
    wire [1:0] hit_source = ptw_bypass_hit ? 2'd1 : storage_hit ? 2'd0 : 2'd2;

    // 判据 gpa_clear_event：GPA清除事件或计数到511，表示等待流程退出。
    wire gpa_clear_event = gpa_clear || gpa_count == 9'd511;
    // 判据 gpa_start：GPA启动参考条件：有效响应有GPF，非旁路/非onlyS2，无匹配GPA结果，无kill/prefetch/redirect抑制。
    wire gpa_start = ((req_valid_d1 && !req_kill_d1) && resp_valid) && resp_has_gpf && !ptw_bypass_hit && !gpa_ng
        && req_out_s2xlate != 2'd2 &&
        !(gpa_response_ready && (gpa_vpn == req_out_vaddr[49:12] && gpa_stage == req_out_s2xlate)) &&
            !req_late_kill && !req_prefetch_d1 && !(redirect_valid && (((req_rob_d1[8] ^ redirect_rob[8]) ^
            (req_rob_d1[7:0] > redirect_rob[7:0])) || (redirect_level && req_rob_d1 == redirect_rob))) &&
            !resp_last_red;

    // 判据 miss_cause：[4:0]={后拍kill,GPA占用且不匹配,replay,前拍PTW返回,同拍PTW返回}。
    wire [4:0] miss_cause = {req_late_kill, (gpa_ng && !gpa_response_ready && !(gpa_vpn == req_out_vaddr[49:12] &&
        gpa_stage == req_out_s2xlate)), resp_need_replay, ptw_valid_d1, ptw_valid};
    // 判据 raw_pmm：按请求拍有效特权及Host/Guest/Hyper上下文选择mseccfg/menvcfg/henvcfg/hstatus/senvcfg。
    wire [1:0] raw_pmm = (req_hyperinst_d1 ? {1'b0, csr_spvp_d1} : csr_dmode_d1) == 2'd3 ? csr_mseccfg_d1 :
        (req_hyperinst_d1 ? {1'b0, csr_spvp_d1} : csr_dmode_d1) == 2'd1 ?
        ((req_hyperinst_d1 || csr_virt_d1) ? csr_henvcfg_d1 : csr_menvcfg_d1) :
        req_hyperinst_d1 ? (csr_imode_d1 == 0 ? csr_hstatus_d1 : csr_senvcfg_d1) :
        csr_virt_d1 ? csr_henvcfg_d1 : csr_senvcfg_d1;
    // 判据 pmm_disabled：HLVX或非M模式有效MXR开启时禁止PMM。
    wire pmm_disabled = req_hlvx_d1 ||
        ((req_hyperinst_d1 ? {1'b0, csr_spvp_d1} : csr_dmode_d1) != 2'd3 &&
         (csr_mxr_d1 || ((csr_virt_d1 || req_hyperinst_d1) && csr_vmxr_d1)));
    // 判据 expected_effective_va：PMM参考地址：禁用时保持fullva；PMM=2保留57位，=3保留48位；S1使能时符号扩展，否则补0。
    wire [63:0] expected_effective_va = pmm_disabled ? req_fullva_d1 : raw_pmm == 2'd2 ?
        (req_pre_s1_d1 ? {{7{req_fullva_d1[56]}}, req_fullva_d1[56:0]} : {7'd0, req_fullva_d1[56:0]}) :
        raw_pmm == 2'd3 ? (req_pre_s1_d1 ? {{16{req_fullva_d1[47]}}, req_fullva_d1[47:0]} :
        {16'd0, req_fullva_d1[47:0]}) : req_fullva_d1;
    // 判据 masked_address_bits：提取PMM丢弃的高位：PMM=2取[63:57]，其余取[63:48]。
    wire [15:0] masked_address_bits = req_pmm_d1 == 2'd2 ? {9'd0, req_fullva_d1[63:57]} : req_fullva_d1[63:48];
    // 判据 pmm_address_class：被屏蔽位分类：0=全0，1=全1，2=混合。
    wire [1:0] pmm_address_class = masked_address_bits == 0 ? 2'd0 :
        ((req_pmm_d1 == 2'd2 && masked_address_bits == 127) || masked_address_bits == 65535) ? 2'd1 : 2'd2;
    // 判据 active_va_mode：按请求拍stage选择satp、hgatp或vsatp的mode快照。
    wire [3:0] active_va_mode = req_stage_d1 == 0 ? csr_satp_mode_d1 :
        req_stage_d1 == 2 ? csr_hgatp_mode_d1 : csr_vsatp_mode_d1;
    // 判据 high_address_bad：地址合法性参考：按直通/onlyS2/S1及Sv39/Sv48检查高位补0或符号扩展。
    wire high_address_bad = (!req_pre_s1_d1 && !req_pre_s2_d1) ? req_effective_va_d1[63:48] != 0 :
        req_stage_d1 == 2 ? (active_va_mode == 8 ? req_effective_va_d1[63:41] != 0 :
            req_effective_va_d1[63:50] != 0) :
        active_va_mode == 8 ? req_effective_va_d1 != {{25{req_effective_va_d1[38]}}, req_effective_va_d1[38:0]} :
        req_effective_va_d1 != {{16{req_effective_va_d1[47]}}, req_effective_va_d1[47:0]};
    // 判据 expected_paddr：参考PA：翻译时拼接resp_ppn与请求页内偏移；直通按no_translate选择PMP地址或请求VA。
    wire [47:0] expected_paddr = !translate_enable ? (req_no_translate_d1 ? req_pmpaddr : req_vaddr_d1[47:0]) :
        {resp_ppn, req_vaddr_d1[11:0]};

    // 判据 va_boundary_39：有效地址边界分类（39位）：0=零，1=正向/无符号上界，2=负向下界，3=邻近边界，4=其余；具体常量见本定义。
    wire [2:0] va_boundary_39 = req_effective_va_d1 == 0 ? 3'd0 : req_effective_va_d1 == 64'h0000003fffffffff ? 3'd1
        : req_effective_va_d1 == 64'hffffffc000000000 ? 3'd2 : req_effective_va_d1 == 64'h0000004000000000
 || req_effective_va_d1 == 64'h0000003ffffffffe ? 3'd3 : 3'd4;
    // 判据 va_boundary_48：有效地址边界分类（48位）：0=零，1=正向/无符号上界，2=负向下界，3=邻近边界，4=其余；具体常量见本定义。
    wire [2:0] va_boundary_48 = req_effective_va_d1 == 0 ? 3'd0 : req_effective_va_d1 == 64'h00007fffffffffff ? 3'd1
        : req_effective_va_d1 == 64'hffff800000000000 ? 3'd2 : req_effective_va_d1 == 64'h0000800000000000
 || req_effective_va_d1 == 64'h00007ffffffffffe ? 3'd3 : 3'd4;
    // 判据
    // va_boundary_41_unsigned：有效地址边界分类（41_unsigned位）：0=零，1=正向/无符号上界，2=负向下界，3=邻近边界，4=其余；具体常量见本定义。
    wire [2:0] va_boundary_41_unsigned = req_effective_va_d1 == 0 ? 3'd0 : req_effective_va_d1 ==
        64'h000001ffffffffff ? 3'd1 : req_effective_va_d1 == 64'h0000020000000000
 || req_effective_va_d1 == 64'h000001fffffffffe ? 3'd3 : 3'd4;
    // 判据
    // va_boundary_50_unsigned：有效地址边界分类（50_unsigned位）：0=零，1=正向/无符号上界，2=负向下界，3=邻近边界，4=其余；具体常量见本定义。
    wire [2:0] va_boundary_50_unsigned = req_effective_va_d1 == 0 ? 3'd0 : req_effective_va_d1 ==
        64'h0003ffffffffffff ? 3'd1 : req_effective_va_d1 == 64'h0004000000000000
 || req_effective_va_d1 == 64'h0003fffffffffffe ? 3'd3 : 3'd4;
    // 判据
    // va_boundary_48_unsigned：有效地址边界分类（48_unsigned位）：0=零，1=正向/无符号上界，2=负向下界，3=邻近边界，4=其余；具体常量见本定义。
    wire [2:0] va_boundary_48_unsigned = req_effective_va_d1 == 0 ? 3'd0 : req_effective_va_d1 ==
        64'h0000ffffffffffff ? 3'd1 : req_effective_va_d1 == 64'h0001000000000000
 || req_effective_va_d1 == 64'h0000fffffffffffe ? 3'd3 : 3'd4;

    // 判据 return_tag：本拍PTW返回页号：onlyS2使用s2tag，其余使用{s1tag,s1low}。
    wire [37:0] return_tag = ptw_stage == 2'd2 ? ptw_s2tag : {ptw_s1tag, ptw_s1low};
    // 判据 return_level：本拍PTW返回的有效页级；双阶段选择较小页级。
    wire [1:0] return_level = ptw_stage == 2'd2 ? ptw_s2level : ptw_stage != 2'd3 ? ptw_s1level :
        ptw_s1level < ptw_s2level ? ptw_s1level : ptw_s2level;
    // 判据 return_napot：本拍PTW返回NAPOT合并结果；双阶段按另一阶段页级决定是否保留NAPOT。
    wire return_napot = ptw_stage == 2'd2 ? ptw_s2n : ptw_stage != 2'd3 ? ptw_s1n :
        (ptw_s1n && ptw_s2level != 0) || (ptw_s2n && ptw_s1level != 0) || (ptw_s1n && ptw_s2n);
    // 判据 return_context_matches：PTW返回与本次响应的stage、ASID/Global、VMID相符。
    wire return_context_matches = ptw_stage == req_out_s2xlate &&
        (req_out_s2xlate == 2 || ptw_s1_g || ptw_s1asid == (req_out_s2xlate == 0 ? csr_asid : csr_vasid)) &&
        (req_out_s2xlate == 0 || {2'd0, (req_out_s2xlate == 2 ? ptw_s2vmid : ptw_s1vmid)} == csr_vmid);
    // 判据 return_address_matches：PTW返回按level/NAPOT比较VPN分段；4KB还检查sector valididx或低位tag。
    wire return_address_matches = req_out_vaddr[49:39] == return_tag[37:27] &&
        (return_level == 3 || req_out_vaddr[38:30] == return_tag[26:18]) &&
        (return_level >= 2 || req_out_vaddr[29:21] == return_tag[17:9]) &&
        (return_level != 0 || (return_napot ? req_out_vaddr[20:16] == return_tag[8:4] :
        req_out_vaddr[20:15] == return_tag[8:3])) && (return_level != 0 || return_napot ||
        (req_out_s2xlate < 2 ? ptw_valididx[req_out_vaddr[14:12]] : return_tag[2:0] == req_out_vaddr[14:12]));

    // 判据 response_end_reason：结束原因编码：0=命中无异常，1=异常，2=miss；miss优先分类。
    wire [1:0] response_end_reason = resp_miss ? 2'd2 : actual_fault != 0 ? 2'd1 : 2'd0;
    // 判据 actual_miss_destination：实际miss去向[1:0]={PTW请求valid,replay}。
    wire [1:0] actual_miss_destination = {resp_qvalid, resp_replay};
    // 判据 response_knownness：响应valid/miss/异常/PA/PBMT中存在X/Z时为1，否则为0。
    wire [1:0] response_knownness = $isunknown({resp_valid, resp_miss, resp_excp, resp_paddr_0, resp_pbmt}) ? 2'd1
        : 2'd0;
    // 判据 owner_redirected：有效redirect命中GPA owner：按ROB环形年龄和redirect level判断。
    wire owner_redirected = redirect_valid && gpa_owner_valid &&
        ((gpa_owner[8] ^ redirect_rob[8] ^ (gpa_owner[7:0] > redirect_rob[7:0])) ||
        (redirect_level && gpa_owner == redirect_rob));
    // 判据 redirect_relation：请求ROB相对redirect：0=相等，1=更老，2=更年轻；含环形flag。
    wire [1:0] redirect_relation = req_rob_d1 == redirect_rob ? 2'd0 : ((req_rob_d1[8] ^ redirect_rob[8]) ^
        (req_rob_d1[7:0] > redirect_rob[7:0])) ? 2'd2 : 2'd1;
    // 判据 expected_gpa_offset：GPA参考偏移：跨页位12相同时用原fullva低12位，否则用请求输出低12位。
    wire [11:0] expected_gpa_offset = req_fullva_d1[12] == req_out_vaddr[12] ? req_fullva_d1[11:0] :
        req_out_vaddr[11:0];
    // 判据 kill_hit_class：0/1/2=请求拍kill且storage/旁路/miss；3/4/5=后拍kill且storage/旁路/miss。
    wire [2:0] kill_hit_class = (req_late_kill && !req_kill_d1 ? 3'd3 : 3'd0) +
        (ptw_bypass_hit ? 3'd1 : storage_hit ? 3'd0 : 3'd2);
    // 仅保留时序点所需的固定容量历史；不扫描 testcase 历史，也不驱动 DUT。
    // 历史更新：本块仅更新覆盖率状态。_d2为前两拍；last_*为最近一次有效响应，不能当作当前RTL值。
    always @(posedge clock) begin

        if (reset) begin
            req_valid_d1 <= '0;
            req_kill_d1 <= '0;
            req_late_kill_d1 <= '0;
            req_vaddr_d1 <= '0;
            req_fullva_d1 <= '0;
            req_checkfullva_d1 <= '0;
            req_cmd_d1 <= '0;
            req_hyperinst_d1 <= '0;
            req_hlvx_d1 <= '0;
            req_prefetch_d1 <= '0;
            req_no_translate_d1 <= '0;
            req_pmpaddr_d1 <= '0;
            req_mabuf_d1 <= '0;
            req_rob_d1 <= '0;
            req_effective_va_d1 <= '0;
            req_pre_s1_d1 <= '0;
            req_pre_s2_d1 <= '0;
            req_stage_d1 <= '0;
            req_pmm_d1 <= '0;
            req_prepf_d1 <= '0;
            req_pregpf_d1 <= '0;
            req_preaf_d1 <= '0;
            req_virt_d1 <= '0;
            req_lookup_d1 <= '0;
            csr_satp_mode_d1 <= '0;
            csr_asid_d1 <= '0;
            csr_satp_changed_d1 <= '0;
            csr_vsatp_mode_d1 <= '0;
            csr_vasid_d1 <= '0;
            csr_vsatp_changed_d1 <= '0;
            csr_hgatp_mode_d1 <= '0;
            csr_vmid_d1 <= '0;
            csr_hgatp_changed_d1 <= '0;
            csr_mxr_d1 <= '0;
            csr_sum_d1 <= '0;
            csr_vmxr_d1 <= '0;
            csr_vsum_d1 <= '0;
            csr_virt_d1 <= '0;
            csr_virt_changed_d1 <= '0;
            csr_spvp_d1 <= '0;
            csr_imode_d1 <= '0;
            csr_dmode_d1 <= '0;
            csr_bme_d1 <= '0;
            csr_cmode_d1 <= '0;
            csr_mseccfg_d1 <= '0;
            csr_menvcfg_d1 <= '0;
            csr_henvcfg_d1 <= '0;
            csr_hstatus_d1 <= '0;
            csr_senvcfg_d1 <= '0;
            history_valid <= '0;
            req_valid_d2 <= '0;
            req_vaddr_d2 <= '0;
            accepted_run_length <= '0;
            accepted_run_length_d1 <= '0;
            gpa_ng_d1 <= '0;
            gpa_ready_d1 <= '0;
            gpa_count_d1 <= '0;
            gpa_vpn_d1 <= '0;
            gpa_stage_d1 <= '0;
            ptw_valid_d1 <= '0;
            ptw_stage_d1 <= '0;
            ptw_s1level_d1 <= '0;
            ptw_s2level_d1 <= '0;
            ptw_s1n_d1 <= '0;
            ptw_s2n_d1 <= '0;
            ptw_s1tag_d1 <= '0;
            ptw_s2tag_d1 <= '0;
            ptw_s1low_d1 <= '0;
            ptw_valididx_d1 <= '0;
            miss_age <= '0;
            miss_pending <= '0;
            miss_vaddr <= '0;
            miss_rob <= '0;
            retry_count <= '0;
            return_seen <= '0;
            last_vaddr <= '0;
            last_fullva <= '0;
            last_paddr <= '0;
            last_rob <= '0;
            last_fault <= '0;
            last_pbmt <= '0;
            last_response_valid <= '0;
            last_bypass <= '0;
            permission_controls_d1 <= '0;
            context_ids_d1 <= '0;
            s2_ug_d1 <= '0;
            return_older <= '0;
            bypass_fast_d1 <= '0;
            ptw_getgpa_d1 <= '0;
            owner_redirected_d1 <= '0;
            redirect_valid_d1 <= '0;
            fast_gpa_start_d1 <= '0;
            gpa_start_d1 <= '0;
            ptw_getgpa_event_d1 <= '0;
            flushed_epoch <= '0;
            refill_seen <= '0;
        end else begin
            req_valid_d1 <= req_valid;
            req_kill_d1 <= req_kill;
            req_late_kill_d1 <= req_late_kill;
            req_vaddr_d1 <= req_vaddr;
            req_fullva_d1 <= req_fullva;
            req_checkfullva_d1 <= req_checkfullva;
            req_cmd_d1 <= req_cmd;
            req_hyperinst_d1 <= req_hyperinst;
            req_hlvx_d1 <= req_hlvx;
            req_prefetch_d1 <= req_prefetch;
            req_no_translate_d1 <= req_no_translate;
            req_pmpaddr_d1 <= req_pmpaddr;
            req_mabuf_d1 <= req_mabuf;
            req_rob_d1 <= req_rob;
            req_effective_va_d1 <= req_effective_va;
            req_pre_s1_d1 <= req_pre_s1;
            req_pre_s2_d1 <= req_pre_s2;
            req_stage_d1 <= req_stage;
            req_pmm_d1 <= req_pmm;
            req_prepf_d1 <= req_prepf;
            req_pregpf_d1 <= req_pregpf;
            req_preaf_d1 <= req_preaf;
            req_virt_d1 <= req_virt;
            req_lookup_d1 <= req_lookup;
            csr_satp_mode_d1 <= csr_satp_mode;
            csr_asid_d1 <= csr_asid;
            csr_satp_changed_d1 <= csr_satp_changed;
            csr_vsatp_mode_d1 <= csr_vsatp_mode;
            csr_vasid_d1 <= csr_vasid;
            csr_vsatp_changed_d1 <= csr_vsatp_changed;
            csr_hgatp_mode_d1 <= csr_hgatp_mode;
            csr_vmid_d1 <= csr_vmid;
            csr_hgatp_changed_d1 <= csr_hgatp_changed;
            csr_mxr_d1 <= csr_mxr;
            csr_sum_d1 <= csr_sum;
            csr_vmxr_d1 <= csr_vmxr;
            csr_vsum_d1 <= csr_vsum;
            csr_virt_d1 <= csr_virt;
            csr_virt_changed_d1 <= csr_virt_changed;
            csr_spvp_d1 <= csr_spvp;
            csr_imode_d1 <= csr_imode;
            csr_dmode_d1 <= csr_dmode;
            csr_bme_d1 <= csr_bme;
            csr_cmode_d1 <= csr_cmode;
            csr_mseccfg_d1 <= csr_mseccfg;
            csr_menvcfg_d1 <= csr_menvcfg;
            csr_henvcfg_d1 <= csr_henvcfg;
            csr_hstatus_d1 <= csr_hstatus;
            csr_senvcfg_d1 <= csr_senvcfg;
            history_valid <= 1'b1;
            req_valid_d2 <= req_valid_d1;
            req_vaddr_d2 <= req_vaddr_d1;
            accepted_run_length_d1 <= accepted_run_length;
            if (req_valid && !req_kill)
                accepted_run_length <= accepted_run_length == 32'hffffffff ? accepted_run_length :
                    accepted_run_length + 1;
            else
                accepted_run_length <= 0;
            gpa_ng_d1 <= gpa_ng;
            gpa_ready_d1 <= gpa_response_ready;
            gpa_count_d1 <= gpa_count;
            gpa_vpn_d1 <= gpa_vpn;
            gpa_stage_d1 <= gpa_stage;
            ptw_valid_d1 <= ptw_valid;
            ptw_stage_d1 <= ptw_stage;
            ptw_s1level_d1 <= ptw_s1level;
            ptw_s2level_d1 <= ptw_s2level;
            ptw_s1n_d1 <= ptw_s1n;
            ptw_s2n_d1 <= ptw_s2n;
            ptw_s1tag_d1 <= ptw_s1tag;
            ptw_s2tag_d1 <= ptw_s2tag;
            ptw_s1low_d1 <= ptw_s1low;
            ptw_valididx_d1 <= ptw_valididx;
            previous_return_matches <= (ptw_valid && return_context_matches && return_address_matches);
            return_older <= previous_return_matches;
            bypass_fast_d1 <= ptw_bypass_hit_fast;
            ptw_getgpa_d1 <= ptw_getgpa;
            owner_redirected_d1 <= owner_redirected;
            redirect_valid_d1 <= redirect_valid;
            fast_gpa_start_d1 <= gpa_start && resp_can_gpa && ptw_bypass_hit_fast;
            gpa_start_d1 <= gpa_start;
            ptw_getgpa_event_d1 <= ptw_valid && ptw_getgpa;
            permission_controls_d1 <= {csr_sum, csr_mxr, csr_vsum, csr_vmxr};
            context_ids_d1 <= {csr_asid, csr_vasid, csr_vmid};
            s2_ug_d1 <= {ptw_s2_u, ptw_s2_g};
            if (csr_satp_changed || csr_vsatp_changed || csr_hgatp_changed || csr_virt_changed)
                flushed_epoch <= 1'b1;
            if (gpa_write)
                refill_seen <= 1'b1;
            if (((req_valid_d1 && !req_kill_d1) && resp_valid)) begin
                last_vaddr <= req_vaddr_d1;
                last_fullva <= req_fullva_d1;
                last_paddr <= resp_paddr_0;
                last_rob <= req_rob_d1;
                last_fault <= actual_fault;
                last_pbmt <= resp_pbmt;
                last_bypass <= ptw_bypass_hit;
                last_response_valid <= 1'b1;
                if (resp_miss) begin
                    miss_pending <= 1'b1;
                    miss_vaddr <= req_vaddr_d1;
                    miss_rob <= req_rob_d1;
                    retry_count <= miss_pending ? retry_count + 1 : 1;
                end else if (miss_pending && miss_vaddr == req_vaddr_d1 && miss_rob == req_rob_d1) begin
                    miss_pending <= 1'b0;
                end
            end
            if (miss_pending) begin
                if (miss_age < 32'd256)
                    miss_age <= miss_age + 1;
                else
                    miss_pending <= 1'b0;
            end else begin
                miss_age <= 0;
            end
            if ((ptw_valid && return_context_matches && return_address_matches))
                return_seen <= 1'b1;
        end
    end

    // 页型选择：旁路取前拍PTW页型；storage路径在固定48个way内按实际hit向量聚合level/NAPOT。
    always @* begin
        selected_level = ptw_bypass_hit ? ptw_s1level_d1 : 2'd0;
        selected_napot = ptw_bypass_hit ? ptw_s1n_d1 : 1'b0;
        if (!ptw_bypass_hit) begin
            for (integer way = 0; way < 48; way = way + 1) begin
                if (storage_hit_vector[way]) begin
                    selected_level = selected_level | storage_levels[way];
                    selected_napot = selected_napot | storage_napot[way];
                end
            end
        end
    end
    // 诊断独立统计，不计入功能分母，不调用UVM错误报告。
    // ----------------------------------------------------------------------------------------
    // 【cg_interface_diagnostics】接口一致性诊断
    // 探测功能：观察输入与实际输出是否一致，并把偏离/未知单独记录。
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：本组weight=0，cp为观察结果，rs的1/2分别表示偏离/未知；reference_missing表示信号缺失。
    // 异常位序：resp_excp[5:0]={af_st,af_ld,gpf_st,gpf_ld,pf_st,pf_ld}；actual_fault==expected_fault核对类别，读写位掩码额外检查命令方向。
    // 来源仓：b_storage=ptw_bypass_hit为0，b_bypass=1；具体仓是否要求storage_hit由各自iff显式限定。
    // actual_fault：从实际输出resp_excp聚合[2:0]={AF,GPF,PF}，各类分别对load/store位做OR。
    // expected_fault：参考异常向量[2:0]={AF,GPF,PF}：预异常优先；无hit为0；有hit时AF优先于PF、PF优先于GPF。
    // expected_paddr：参考PA：翻译时拼接resp_ppn与请求页内偏移；直通按no_translate选择PMP地址或请求VA。
    // expected_pbmt：参考PBMT：直通为0；onlyS2取S2；双阶段且S2非0时取S2；其余取S1。
    covergroup cg_interface_diagnostics @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_request_response : coverpoint ($isunknown(resp_valid && req_out_cmd == req_cmd_d1 &&
            req_out_vaddr == req_vaddr_d1) ? 2'd2 : (resp_valid && req_out_cmd == req_cmd_d1 &&
            req_out_vaddr == req_vaddr_d1) ? 2'd0 : 2'd1)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1) {
            bins b_mismatch = {2'd1};
            bins b_unknown = {2'd2};
        }
        cp_translation_fault : coverpoint ($isunknown((actual_fault == expected_fault && ((req_out_cmd == 3'd0
            || req_out_cmd == 3'd4) ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd ==
            3'd5) ? (resp_excp & 6'b010101) == 0 : 1'b1))) ? 2'd2 : ((actual_fault == expected_fault &&
            ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp & 6'b101010) == 0 : (req_out_cmd ==
            3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0 : 1'b1))) ? 2'd0 : 2'd1)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid) {
            bins b_mismatch = {2'd1};
            bins b_unknown = {2'd2};
        }
        cp_address_mapping : coverpoint ($isunknown((resp_paddr_0 == expected_paddr)) ? 2'd2 : ((resp_paddr_0
            == expected_paddr)) ? 2'd0 : 2'd1)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && !resp_miss
                && actual_fault == 0) {
            bins b_mismatch = {2'd1};
            bins b_unknown = {2'd2};
        }
        cp_pbmt_selection : coverpoint ($isunknown(resp_pbmt == expected_pbmt) ? 2'd2 : (resp_pbmt == expected_pbmt)
            ? 2'd0 : 2'd1)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)) {
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
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：01.01.001, 01.01.002, 01.01.003, 01.01.004；逐点功能见下方注释，精确配值见分解表。
    covergroup cg_dtlb_01_01 @(posedge clock);
        option.per_instance = 1;
        // 01.01.001 REQ_命令类别_cmd0
        // 测试表目标：向具备该命令能力的 DTLB
        // 端口送入普通数据读取（load）请求，记录请求接受、下一拍锁存命令和响应类别。该点用于覆盖命令解码及读写异常分类，并核对共享端口上的真实请求来源。
        cp_01_01_001 : coverpoint req_cmd_d1
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_cmd_d1 == 0
                && (((resp_valid && req_out_cmd == req_cmd_d1)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_command = {3'd0};
        }
        // 01.01.002 REQ_命令类别_cmd1
        // 测试表目标：向具备该命令能力的 DTLB
        // 端口送入普通数据写入（store）请求，记录请求接受、下一拍锁存命令和响应类别。该点用于覆盖命令解码及读写异常分类，并核对共享端口上的真实请求来源。
        cp_01_01_002 : coverpoint req_cmd_d1
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_cmd_d1 == 1
                && (((resp_valid && req_out_cmd == req_cmd_d1)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_command = {3'd1};
        }
        // 01.01.003 REQ_命令类别_cmd4
        // 测试表目标：向具备该命令能力的 DTLB 端口送入LR
        // 原子保留加载（读类）请求，记录请求接受、下一拍锁存命令和响应类别。该点用于覆盖命令解码及读写异常分类，并核对共享端口上的真实请求来源。
        cp_01_01_003 : coverpoint req_cmd_d1
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_cmd_d1 == 4
                && (((resp_valid && req_out_cmd == req_cmd_d1)) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_command = {3'd4};
        }
        // 01.01.004 REQ_命令类别_cmd5
        // 测试表目标：向具备该命令能力的 DTLB 端口送入SC/AMO
        // 原子写类访问（写类）请求，记录请求接受、下一拍锁存命令和响应类别。该点用于覆盖命令解码及读写异常分类，并核对共享端口上的真实请求来源。
        cp_01_01_004 : coverpoint req_cmd_d1
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_cmd_d1 == 5
                && (((resp_valid && req_out_cmd == req_cmd_d1)) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_command = {3'd5};
        }
    endgroup
    cg_dtlb_01_01 cg_dtlb_01_01_inst;

    // 01.02 请求接受与流水节拍
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_01_02】01.02 请求接受与流水节拍
    // 探测功能：观察请求有效位和地址/命令穿过一拍流水后的对应关系，覆盖连续吞吐、空泡和端口并发。
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：01.02.001, 01.02.002, 01.02.003, 01.02.004, 01.02.006；逐点功能见下方注释，精确配值见分解表。
    covergroup cg_dtlb_01_02 @(posedge clock);
        option.per_instance = 1;
        // 01.02.002 REQ_单拍请求
        // 测试表目标：场景：R.valid=1且kill=0；上一拍无请求。 观察功能：下一拍O.valid=1，req_out字段对应本次请求。
        // 测试目的：观察请求有效位和地址/命令穿过一拍流水后的对应关系，覆盖连续吞吐、空泡和端口并发。
        cp_01_02_002 : coverpoint (resp_valid && req_out_vaddr == req_vaddr_d1 && req_out_cmd == req_cmd_d1)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && !req_valid_d2) {
            bins b_expected_result = {1'b1};
        }
        // 01.02.004 REQ_气泡传播
        // 测试表目标：场景：请求序列valid=1,0,1。 观察功能：O.valid按一拍延迟传播；气泡不采集功能结果。
        // 测试目的：观察请求有效位和地址/命令穿过一拍流水后的对应关系，覆盖连续吞吐、空泡和端口并发。
        cp_01_02_004 : coverpoint (!resp_valid)
            iff (sample_enable
                && req_valid_d2
                && !req_valid_d1
                && req_valid) {
            bins b_expected_result = {1'b1};
        }
        // 01.02.006 REQ_无效拍payload变化
        // 测试表目标：场景：R.valid=0时VA/cmd任意变化。 观察功能：不得产生有效事务覆盖；注意no_translate等无使能寄存信号另行观察。
        // 测试目的：观察请求有效位和地址/命令穿过一拍流水后的对应关系，覆盖连续吞吐、空泡和端口并发。
        cp_01_02_006 : coverpoint (!resp_valid)
            iff (sample_enable
                && history_valid
                && !req_valid_d1
                && req_vaddr_d1 != req_vaddr_d2) {
            bins b_expected_result = {1'b1};
        }
        // 01.02.001 REQ_复位空闲
        // 测试表目标：场景：reset=1，之后解除且所有R.valid=0。 观察功能：req_out_v清零；O.valid保持0；不消费未初始化payload。
        // 测试目的：观察请求有效位和地址/命令穿过一拍流水后的对应关系，覆盖连续吞吐、空泡和端口并发。
        cp_01_02_001 : coverpoint resp_valid
            iff (sample_enable
                && history_valid
                && !req_valid
                && !resp_valid
                && (!resp_valid) === 1'b1) {
            bins b_expected_clear = {1'b0};
        }
        // 01.02.003 REQ_连续吞吐
        // 测试表目标：场景：同端口连续2/3/8拍R.valid=1且kill=0。 观察功能：连续返回；不能混用前后拍VA、cmd、ROB。
        // 测试目的：观察请求有效位和地址/命令穿过一拍流水后的对应关系，覆盖连续吞吐、空泡和端口并发。
        cp_01_02_003 : coverpoint accepted_run_length_d1
            iff (sample_enable
                && accepted_run_length_d1 >= 2
                && !req_valid
                && ((!(req_valid_d1 && !req_kill_d1) || resp_valid) === 1'b1)) {
            bins b_two = {2};
            bins b_three = {3};
            bins b_four_to_seven = {[4:7]};
            bins b_eight_or_more = {[8:$]};
        }
    endgroup
    cg_dtlb_01_02 cg_dtlb_01_02_inst;

    // 01.03 取消请求与后拍抑制
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_01_03】01.03 取消请求与后拍抑制
    // 探测功能：区分请求拍取消与响应拍取消：二者对响应有效、PTW 发射和重放的作用不同。
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：01.03.001, 01.03.002, 01.03.003, 01.03.004；逐点功能见下方注释，精确配值见分解表。
    // kill_hit_class：0/1/2=请求拍kill且storage/旁路/miss；3/4/5=后拍kill且storage/旁路/miss。
    covergroup cg_dtlb_01_03 @(posedge clock);
        option.per_instance = 1;
        // 01.03.001 REQ_请求拍kill
        // 测试表目标：场景：R.valid=1且R.kill=1。 观察功能：下一拍不建立该请求的O.valid；相邻有效请求不受影响。
        // 测试目的：区分请求拍取消与响应拍取消：二者对响应有效、PTW 发射和重放的作用不同。
        cp_01_03_001 : coverpoint (!resp_valid)
            iff (sample_enable
                && req_valid_d1
                && req_kill_d1) {
            bins b_expected_result = {1'b1};
        }
        // 01.03.002 REQ_后拍req_kill
        // 测试表目标：场景：前拍R.valid=1；本拍R.req_kill=1。
        // 观察功能：本拍禁止该端口PTW.req.valid，tlbreplay置1；不假设O.valid被清掉。 测试目的：区分请求拍取消与响应拍取消：二者对响应有效、PTW
        // 发射和重放的作用不同。
        cp_01_03_002 : coverpoint (!resp_qvalid && resp_replay)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_late_kill) {
            bins b_expected_result = {1'b1};
        }
        // 01.03.003 REQ_kill与hit_miss交叉
        // 测试表目标：场景：REQ或RSP对应kill路径，分别构造storage hit、miss、bypass hit。 观察功能：区分请求取消与PTW抑制；上游被杀结果不提交。
        // 测试目的：区分请求拍取消与响应拍取消：二者对响应有效、PTW 发射和重放的作用不同。
        cp_01_03_003 : coverpoint kill_hit_class
            iff (sample_enable
                && req_valid_d1
                && (req_kill_d1 || req_late_kill)
                && ((req_kill_d1 ? !resp_valid : !resp_qvalid) === 1'b1)) {
            bins b_kill_storage = {0};
            bins b_kill_bypass = {1};
            bins b_kill_miss = {2};
            bins b_late_storage = {3};
            bins b_late_bypass = {4};
            bins b_late_miss = {5};
        }
        // 01.03.004 REQ_kill仍可触发storage读
        // 测试表目标：场景：R.valid=1,kill=1且storage原本匹配。 观察功能：O.valid不建立，但storage读与PLRU
        // touch可发生；不能把被kill请求定义成内部完全无活动。 测试目的：区分请求拍取消与响应拍取消：二者对响应有效、PTW 发射和重放的作用不同。
        cp_01_03_004 : coverpoint resp_valid
            iff (sample_enable
                && req_valid_d1
                && req_kill_d1
                && storage_hit
                && (!resp_valid) === 1'b1) {
            bins b_expected_clear = {1'b0};
        }
    endgroup
    cg_dtlb_01_03 cg_dtlb_01_03_inst;

    // 01.04 响应关联与副本
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_01_04】01.04 响应关联与副本
    // 探测功能：观察响应字段副本和事务关联，区分正常返回、缺失返回及无法归属的返回。
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：01.04.001, 01.04.002；逐点功能见下方注释，精确配值见分解表。
    covergroup cg_dtlb_01_04 @(posedge clock);
        option.per_instance = 1;
        // 01.04.001 REQ_响应副本
        // 测试表目标：场景：O.valid且无miss，存在两个paddr输出。 观察功能：paddr_0与paddr_1一致；仅对实际存在副本比较。
        // 测试目的：观察响应字段副本和事务关联，区分正常返回、缺失返回及无法归属的返回。
        cp_01_04_001 : coverpoint (resp_paddr_0 == resp_paddr_1)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && !resp_miss
                && GROUP_ID == 0
                && PORT_INDEX<3) {
            bins b_expected_result = {1'b1};
        }
        // 01.04.002 FCOV_请求响应关联
        // 测试表目标：场景：每个非kill REQ建立token；下一拍采是否响应；每个O.valid也采能否匹配token。
        // 观察功能：用逐端口一拍历史及未结束token表分类；缺响应在到期拍就sample，不能仅在O.valid时采。非标准延迟和不匹配单独统计。
        // 测试目的：观察响应字段副本和事务关联，区分正常返回、缺失返回及无法归属的返回。
        cp_01_04_002 : coverpoint (resp_valid && req_out_vaddr == req_vaddr_d1 && req_out_cmd == req_cmd_d1)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1) {
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_01_04 cg_dtlb_01_04_inst;

    // 01.05 请求控制与ROB关系
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_01_05】01.05 请求控制与ROB关系
    // 探测功能：观察命令合法性和完整 ROB 指针年龄关系如何影响请求或 GPA 等待者的取消。
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：01.05.002, 01.05.003, 01.05.004, 01.05.005, 01.05.006, 01.05.007, 01.05.008,
    // 01.05.009；逐点功能见下方注释，精确配值见分解表。
    // redirect_relation：请求ROB相对redirect：0=相等，1=更老，2=更年轻；含环形flag。
    covergroup cg_dtlb_01_05 @(posedge clock);
        option.per_instance = 1;
        // 01.05.002 ROB_redirect_valid0
        // 测试表目标：场景：有效请求或GPA owner在途，redirect.valid=0。 观察功能：观察cur_red、owner清除和Q/replay；valid=0时ROB
        // payload不构成冲刷事件。 测试目的：观察命令合法性和完整 ROB 指针年龄关系如何影响请求或 GPA 等待者的取消。
        cp_01_05_002 : coverpoint (((req_valid_d1 && !req_kill_d1) && resp_valid) && !redirect_valid)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && !redirect_valid) {
            bins b_observed = {1'b1};
        }
        // 01.05.003 ROB_redirect_valid1
        // 测试表目标：场景：有效请求或GPA owner在途，redirect.valid=1。 观察功能：观察cur_red、owner清除和Q/replay；valid=0时ROB
        // payload不构成冲刷事件。 测试目的：观察命令合法性和完整 ROB 指针年龄关系如何影响请求或 GPA 等待者的取消。
        cp_01_05_003 : coverpoint (((req_valid_d1 && !req_kill_d1) && resp_valid) && redirect_valid)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && redirect_valid) {
            bins b_observed = {1'b1};
        }
        // 01.05.004 ROB_redirect_equal_level0
        // 测试表目标：构造当前请求或 GPA 等待者与重定向指令为同一 ROB 项，并使 redirect.valid=1、level=0。level=0
        // 只取消更年轻指令，level=1 同时取消重定向指令自身，因此本点应分类为不被该重定向选中取消。观察 PTW/重放及下一拍 GPA
        // 状态，其他退出原因仍分别记录。比较年龄必须同时使用 ROB flag 与 value。
        cp_01_05_004 : coverpoint (((req_valid_d1 && !req_kill_d1) && resp_valid) && redirect_valid &&
            redirect_relation == 0 && redirect_level == 0)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && redirect_valid
                && redirect_relation == 0
                && redirect_level == 0) {
            bins b_observed = {1'b1};
        }
        // 01.05.005 ROB_redirect_equal_level1
        // 测试表目标：构造当前请求或 GPA 等待者与重定向指令为同一 ROB 项，并使 redirect.valid=1、level=1。level=0
        // 只取消更年轻指令，level=1 同时取消重定向指令自身，因此本点应分类为被该重定向选中取消。观察 PTW/重放及下一拍 GPA
        // 状态，其他退出原因仍分别记录。比较年龄必须同时使用 ROB flag 与 value。
        cp_01_05_005 : coverpoint (((req_valid_d1 && !req_kill_d1) && resp_valid) && redirect_valid &&
            redirect_relation == 0 && redirect_level == 1)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && redirect_valid
                && redirect_relation == 0
                && redirect_level == 1) {
            bins b_observed = {1'b1};
        }
        // 01.05.006 ROB_redirect_older_level0
        // 测试表目标：构造当前请求或 GPA 等待者比重定向指令更老，并使 redirect.valid=1、level=0。level=0 只取消更年轻指令，level=1
        // 同时取消重定向指令自身，因此本点应分类为不被该重定向选中取消。观察 PTW/重放及下一拍 GPA 状态，其他退出原因仍分别记录。比较年龄必须同时使用 ROB flag 与
        // value。
        cp_01_05_006 : coverpoint (((req_valid_d1 && !req_kill_d1) && resp_valid) && redirect_valid &&
            redirect_relation == 1 && redirect_level == 0)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && redirect_valid
                && redirect_relation == 1
                && redirect_level == 0) {
            bins b_observed = {1'b1};
        }
        // 01.05.007 ROB_redirect_older_level1
        // 测试表目标：构造当前请求或 GPA 等待者比重定向指令更老，并使 redirect.valid=1、level=1。level=0 只取消更年轻指令，level=1
        // 同时取消重定向指令自身，因此本点应分类为不被该重定向选中取消。观察 PTW/重放及下一拍 GPA 状态，其他退出原因仍分别记录。比较年龄必须同时使用 ROB flag 与
        // value。
        cp_01_05_007 : coverpoint (((req_valid_d1 && !req_kill_d1) && resp_valid) && redirect_valid &&
            redirect_relation == 1 && redirect_level == 1)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && redirect_valid
                && redirect_relation == 1
                && redirect_level == 1) {
            bins b_observed = {1'b1};
        }
        // 01.05.008 ROB_redirect_younger_level0
        // 测试表目标：构造当前请求或 GPA 等待者比重定向指令更年轻，并使 redirect.valid=1、level=0。level=0 只取消更年轻指令，level=1
        // 同时取消重定向指令自身，因此本点应分类为被该重定向选中取消。观察 PTW/重放及下一拍 GPA 状态，其他退出原因仍分别记录。比较年龄必须同时使用 ROB flag 与
        // value。
        cp_01_05_008 : coverpoint (((req_valid_d1 && !req_kill_d1) && resp_valid) && redirect_valid &&
            redirect_relation == 2 && redirect_level == 0)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && redirect_valid
                && redirect_relation == 2
                && redirect_level == 0) {
            bins b_observed = {1'b1};
        }
        // 01.05.009 ROB_redirect_younger_level1
        // 测试表目标：构造当前请求或 GPA 等待者比重定向指令更年轻，并使 redirect.valid=1、level=1。level=0 只取消更年轻指令，level=1
        // 同时取消重定向指令自身，因此本点应分类为被该重定向选中取消。观察 PTW/重放及下一拍 GPA 状态，其他退出原因仍分别记录。比较年龄必须同时使用 ROB flag 与
        // value。
        cp_01_05_009 : coverpoint (((req_valid_d1 && !req_kill_d1) && resp_valid) && redirect_valid &&
            redirect_relation == 2 && redirect_level == 1)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && redirect_valid
                && redirect_relation == 2
                && redirect_level == 1) {
            bins b_observed = {1'b1};
        }
    endgroup
    cg_dtlb_01_05 cg_dtlb_01_05_inst;

    // 01.05 请求控制与ROB关系
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_diag_01_05】01.05 请求控制与ROB关系
    // 探测功能：观察命令合法性和完整 ROB 指针年龄关系如何影响请求或 GPA 等待者的取消。
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：本组weight=0，cp为观察结果，rs的1/2分别表示偏离/未知；reference_missing表示信号缺失。
    covergroup cg_dtlb_diag_01_05 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        // 01.05.001 请求命令_保留或非数据命令：
        // 测试点表明确要求“不生成运行期 coverpoint”，这里只保留编号说明。
    endgroup
    cg_dtlb_diag_01_05 cg_dtlb_diag_01_05_inst;

    // 02.01 Host翻译与直通
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_02_01】02.01 Host翻译与直通
    // 探测功能：核对 Host 请求是否启用地址翻译，以及关闭翻译时物理地址的来源。
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：02.01.001, 02.01.002, 02.01.003, 02.01.004, 02.01.005, 02.01.006；逐点功能见下方注释，精确配值见分解表。
    // pre_fault：请求拍 checkfullva 有效，且预检查 PF/GPF/AF 中至少一种成立；均使用_d1快照。
    covergroup cg_dtlb_02_01 @(posedge clock);
        option.per_instance = 1;
        // 02.01.001 MODE_host_bare
        // 测试表目标：场景：virt=0,hyperinst=0,satp.mode=0。 观察功能：portTranslateEnable=0；地址走直通。 测试目的：核对 Host
        // 请求是否启用地址翻译，以及关闭翻译时物理地址的来源。
        cp_02_01_001 : coverpoint (!translate_enable)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && !resp_virt
                && !req_hyperinst_d1
                && csr_satp_mode == 0
                && !req_no_translate_d1) {
            bins b_expected_result = {1'b1};
        }
        // 02.01.002 MODE_host_sv39
        // 测试表目标：场景：virt=0,hyperinst=0,satp.mode=8,dmode∈{U,S}。
        // 观察功能：portTranslateEnable=1；s2xlate=0。 测试目的：核对 Host 请求是否启用地址翻译，以及关闭翻译时物理地址的来源。
        cp_02_01_002 : coverpoint csr_satp_mode
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && !resp_virt
                && !req_hyperinst_d1
                && csr_satp_mode == 8
                && effective_mode != 3
                && !req_no_translate_d1
                && (((translate_enable && req_out_s2xlate == 0)) === 1'b1)) {
            bins b_sv39 = {4'd8};
        }
        // 02.01.003 MODE_host_sv48
        // 测试表目标：场景：virt=0,hyperinst=0,satp.mode=9,dmode∈{U,S}。
        // 观察功能：portTranslateEnable=1；s2xlate=0。 测试目的：核对 Host 请求是否启用地址翻译，以及关闭翻译时物理地址的来源。
        cp_02_01_003 : coverpoint csr_satp_mode
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && !resp_virt
                && !req_hyperinst_d1
                && csr_satp_mode == 9
                && effective_mode != 3
                && !req_no_translate_d1
                && (((translate_enable && req_out_s2xlate == 0)) === 1'b1)) {
            bins b_sv48 = {4'd9};
        }
        // 02.01.004 MODE_M模式旁路
        // 测试表目标：场景：virt=0,hyperinst=0,dmode=M，satp.mode=8或9。 观察功能：portTranslateEnable=0。 测试目的：核对
        // Host 请求是否启用地址翻译，以及关闭翻译时物理地址的来源。
        cp_02_01_004 : coverpoint (!translate_enable)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && !resp_virt
                && !req_hyperinst_d1
                && effective_mode == 3
                && csr_satp_mode inside {8, 9}) {
            bins b_expected_result = {1'b1};
        }
        // 02.01.005 MODE_no_translate
        // 测试表目标：场景：privNeedTranslate=1且上一拍R.no_translate=1。
        // 观察功能：portTranslateEnable=0，使用响应拍输入R.pmp_addr；PBMT=0。 测试目的：核对 Host
        // 请求是否启用地址翻译，以及关闭翻译时物理地址的来源。
        cp_02_01_005 : coverpoint (!translate_enable && resp_paddr_0 == req_pmpaddr && resp_pbmt == 0)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && req_no_translate_d1
                && (req_pre_s1_d1 || req_pre_s2_d1)) {
            bins b_expected_result = {1'b1};
        }
        // 02.01.006 MODE_直通地址截位
        // 测试表目标：场景：translateEnable=0且无pre-fault。 观察功能：paddr取req_out.vaddr低48位；地址合法性来自fullva预检查。
        // 测试目的：核对 Host 请求是否启用地址翻译，以及关闭翻译时物理地址的来源。
        cp_02_01_006 : coverpoint (resp_paddr_0 == req_vaddr_d1[47:0])
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && !translate_enable
                && !pre_fault
                && !req_no_translate_d1) {
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_02_01 cg_dtlb_02_01_inst;

    // 02.02 Guest与Hyper翻译选择
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_02_02】02.02 Guest与Hyper翻译选择
    // 探测功能：核对虚拟化或 Hyper 指令选择的翻译阶段和有效特权，防止使用错误的页表上下文。
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：02.02.001, 02.02.002, 02.02.003, 02.02.004, 02.02.005, 02.02.006；逐点功能见下方注释，精确配值见分解表。
    covergroup cg_dtlb_02_02 @(posedge clock);
        option.per_instance = 1;
        // 02.02.001 MODE_onlyS1
        // 测试表目标：场景：(virt||hyperinst)=1,vsatp.mode!=0,hgatp.mode=0,mode<M。
        // 观察功能：s2xlate=1；只使用S1权限/地址。 测试目的：核对虚拟化或 Hyper 指令选择的翻译阶段和有效特权，防止使用错误的页表上下文。
        cp_02_02_001 : coverpoint req_out_s2xlate
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (resp_virt || req_hyperinst_d1)
                && csr_vsatp_mode inside {8, 9}
                && csr_hgatp_mode == 0
                && effective_mode != 3
                && !req_no_translate_d1
                && (((req_out_s2xlate == 1 && translate_enable)) === 1'b1)) {
            bins b_translation_stage = {2'd1};
        }
        // 02.02.002 MODE_onlyS2
        // 测试表目标：场景：(virt||hyperinst)=1,vsatp.mode=0,hgatp.mode∈{8,9},mode<M。
        // 观察功能：s2xlate=2；输入当作GPA。 测试目的：核对虚拟化或 Hyper 指令选择的翻译阶段和有效特权，防止使用错误的页表上下文。
        cp_02_02_002 : coverpoint req_out_s2xlate
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (resp_virt || req_hyperinst_d1)
                && csr_vsatp_mode == 0
                && csr_hgatp_mode inside {8, 9}
                && effective_mode != 3
                && !req_no_translate_d1
                && (((req_out_s2xlate == 2 && translate_enable)) === 1'b1)) {
            bins b_translation_stage = {2'd2};
        }
        // 02.02.003 MODE_allStage
        // 测试表目标：场景：(virt||hyperinst)=1,vsatp.mode∈{8,9},hgatp.mode∈{8,9},mode<M。
        // 观察功能：s2xlate=3；S1+S2。 测试目的：核对虚拟化或 Hyper 指令选择的翻译阶段和有效特权，防止使用错误的页表上下文。
        cp_02_02_003 : coverpoint req_out_s2xlate
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (resp_virt || req_hyperinst_d1)
                && csr_vsatp_mode inside {8, 9}
                && csr_hgatp_mode inside {8, 9}
                && effective_mode != 3
                && !req_no_translate_d1
                && (((req_out_s2xlate == 3 && translate_enable)) === 1'b1)) {
            bins b_translation_stage = {2'd3};
        }
        // 02.02.004 MODE_guest双Bare
        // 测试表目标：场景：(virt||hyperinst)=1,vsatp.mode=0,hgatp.mode=0。
        // 观察功能：s2xlate编码仍为2，但translateEnable=0；不要把编码2直接当翻译开启。 测试目的：核对虚拟化或 Hyper
        // 指令选择的翻译阶段和有效特权，防止使用错误的页表上下文。
        cp_02_02_004 : coverpoint req_out_s2xlate
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (resp_virt || req_hyperinst_d1)
                && csr_vsatp_mode == 0
                && csr_hgatp_mode == 0
                && (((req_out_s2xlate == 2 && !translate_enable)) === 1'b1)) {
            bins b_translation_stage = {2'd2};
        }
        // 02.02.005 MODE_hyperinst有效模式
        // 测试表目标：场景：hyperinst=1且O.valid，spvp=0/1。 观察功能：mode选spvp(U/S)；异常isHyper=1。 测试目的：核对虚拟化或
        // Hyper 指令选择的翻译阶段和有效特权，防止使用错误的页表上下文。
        cp_02_02_005 : coverpoint ((effective_mode == {1'b0, csr_spvp} && (GROUP_ID == 2 || PORT_INDEX == 3 ||
            resp_hyper)))
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && req_hyperinst_d1) {
            bins b_expected_result = {1'b1};
        }
        // 02.02.006 MODE_virt锁存
        // 测试表目标：场景：REQ后外部virt发生变化且遵循flush协议。 观察功能：virt_out使用请求拍virt，CSR值按延迟域取；变化窗口单列竞争覆盖。
        // 测试目的：核对虚拟化或 Hyper 指令选择的翻译阶段和有效特权，防止使用错误的页表上下文。
        cp_02_02_006 : coverpoint (resp_virt == req_virt_d1)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_virt_d1 != req_virt) {
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_02_02 cg_dtlb_02_02_inst;

    // 02.03 地址合法性与预异常
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_02_03】02.03 地址合法性与预异常
    // 探测功能：观察完整地址在查表前的合法性检查，以及预异常如何影响最终异常类型和地址报告。
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：02.03.001, 02.03.002, 02.03.003, 02.03.004, 02.03.005, 02.03.006, 02.03.007,
    // 02.03.008, 02.03.009, 02.03.010, 02.03.011, 02.03.012, 02.03.013, 02.03.014, 02.03.015,
    // 02.03.016, 02.03.017, 02.03.018, 02.03.019, 02.03.020, 02.03.021, 02.03.022, 02.03.023,
    // 02.03.024, 02.03.025, 02.03.026, 02.03.027, 02.03.028, 02.03.029, 02.03.030, 02.03.031,
    // 02.03.032, 02.03.033, 02.03.034, 02.03.035, 02.03.036, 02.03.037, 02.03.038, 02.03.039,
    // 02.03.040, 02.03.041, 02.03.042, 02.03.043, 02.03.044, 02.03.045, 02.03.046, 02.03.047,
    // 02.03.048, 02.03.049, 02.03.050, 02.03.051；逐点功能见下方注释，精确配值见分解表。
    // 异常位序：resp_excp[5:0]={af_st,af_ld,gpf_st,gpf_ld,pf_st,pf_ld}；actual_fault==expected_fault核对类别，读写位掩码额外检查命令方向。
    // actual_fault：从实际输出resp_excp聚合[2:0]={AF,GPF,PF}，各类分别对load/store位做OR。
    // expected_fault：参考异常向量[2:0]={AF,GPF,PF}：预异常优先；无hit为0；有hit时AF优先于PF、PF优先于GPF。
    // high_address_bad：地址合法性参考：按直通/onlyS2/S1及Sv39/Sv48检查高位补0或符号扩展。
    // pre_fault：请求拍 checkfullva 有效，且预检查 PF/GPF/AF 中至少一种成立；均使用_d1快照。
    // va_boundary_39：有效地址边界分类（39位）：0=零，1=正向/无符号上界，2=负向下界，3=邻近边界，4=其余；具体常量见本定义。
    // va_boundary_41_unsigned：有效地址边界分类（41_unsigned位）：0=零，1=正向/无符号上界，2=负向下界，3=邻近边界，4=其余；具体常量见本定义。
    // va_boundary_48：有效地址边界分类（48位）：0=零，1=正向/无符号上界，2=负向下界，3=邻近边界，4=其余；具体常量见本定义。
    // va_boundary_48_unsigned：有效地址边界分类（48_unsigned位）：0=零，1=正向/无符号上界，2=负向下界，3=邻近边界，4=其余；具体常量见本定义。
    // va_boundary_50_unsigned：有效地址边界分类（50_unsigned位）：0=零，1=正向/无符号上界，2=负向下界，3=邻近边界，4=其余；具体常量见本定义。
    covergroup cg_dtlb_02_03 @(posedge clock);
        option.per_instance = 1;
        // 02.03.001 VA_Sv39_合法边界_stage0_cmd0
        // 测试表目标：在noS2：非虚拟化 Host 路径，翻译启用时使用 satp 的 S
        // 阶段场景下发起普通数据读取（load），并开启完整地址检查。构造Sv39地址满足“EVA[63:39] 全部等于
        // bit38（39位符号扩展）”的边界取值。观察该预检查原因不产生异常；后续页表权限或 PMP 仍可能拒绝访问。
        cp_02_03_001 : coverpoint va_boundary_39
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 0
                && req_stage_d1 == 0
                && (req_pre_s1_d1 || req_pre_s2_d1)
                && csr_satp_mode_d1 == 8
                && req_effective_va_d1 == {{25{req_effective_va_d1[38]}}, req_effective_va_d1[38:0]}
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_negative_limit = {2};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.002 VA_Sv39_合法边界_stage0_cmd1
        // 测试表目标：在noS2：非虚拟化 Host 路径，翻译启用时使用 satp 的 S
        // 阶段场景下发起普通数据写入（store），并开启完整地址检查。构造Sv39地址满足“EVA[63:39] 全部等于
        // bit38（39位符号扩展）”的边界取值。观察该预检查原因不产生异常；后续页表权限或 PMP 仍可能拒绝访问。
        cp_02_03_002 : coverpoint va_boundary_39
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 1
                && req_stage_d1 == 0
                && (req_pre_s1_d1 || req_pre_s2_d1)
                && csr_satp_mode_d1 == 8
                && req_effective_va_d1 == {{25{req_effective_va_d1[38]}}, req_effective_va_d1[38:0]}
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_negative_limit = {2};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.003 VA_Sv39_合法边界_stage1_cmd0
        // 测试表目标：在onlyStage1：只执行 Guest 的 VS 阶段，G 阶段
        // Bare场景下发起普通数据读取（load），并开启完整地址检查。构造Sv39地址满足“EVA[63:39] 全部等于
        // bit38（39位符号扩展）”的边界取值。观察该预检查原因不产生异常；后续页表权限或 PMP 仍可能拒绝访问。
        cp_02_03_003 : coverpoint va_boundary_39
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 0
                && req_stage_d1 == 1
                && (req_pre_s1_d1 || req_pre_s2_d1)
                && csr_vsatp_mode_d1 == 8
                && req_effective_va_d1 == {{25{req_effective_va_d1[38]}}, req_effective_va_d1[38:0]}
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_negative_limit = {2};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.004 VA_Sv39_合法边界_stage1_cmd1
        // 测试表目标：在onlyStage1：只执行 Guest 的 VS 阶段，G 阶段
        // Bare场景下发起普通数据写入（store），并开启完整地址检查。构造Sv39地址满足“EVA[63:39] 全部等于
        // bit38（39位符号扩展）”的边界取值。观察该预检查原因不产生异常；后续页表权限或 PMP 仍可能拒绝访问。
        cp_02_03_004 : coverpoint va_boundary_39
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 1
                && req_stage_d1 == 1
                && (req_pre_s1_d1 || req_pre_s2_d1)
                && csr_vsatp_mode_d1 == 8
                && req_effective_va_d1 == {{25{req_effective_va_d1[38]}}, req_effective_va_d1[38:0]}
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_negative_limit = {2};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.005 VA_Sv39_合法边界_stage3_cmd0
        // 测试表目标：在allStage：VS 与 G 两阶段组合翻译场景下发起普通数据读取（load），并开启完整地址检查。构造Sv39地址满足“EVA[63:39] 全部等于
        // bit38（39位符号扩展）”的边界取值。观察该预检查原因不产生异常；后续页表权限或 PMP 仍可能拒绝访问。
        cp_02_03_005 : coverpoint va_boundary_39
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 0
                && req_stage_d1 == 3
                && (req_pre_s1_d1 || req_pre_s2_d1)
                && csr_vsatp_mode_d1 == 8
                && req_effective_va_d1 == {{25{req_effective_va_d1[38]}}, req_effective_va_d1[38:0]}
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_negative_limit = {2};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.006 VA_Sv39_合法边界_stage3_cmd1
        // 测试表目标：在allStage：VS 与 G 两阶段组合翻译场景下发起普通数据写入（store），并开启完整地址检查。构造Sv39地址满足“EVA[63:39] 全部等于
        // bit38（39位符号扩展）”的边界取值。观察该预检查原因不产生异常；后续页表权限或 PMP 仍可能拒绝访问。
        cp_02_03_006 : coverpoint va_boundary_39
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 1
                && req_stage_d1 == 3
                && (req_pre_s1_d1 || req_pre_s2_d1)
                && csr_vsatp_mode_d1 == 8
                && req_effective_va_d1 == {{25{req_effective_va_d1[38]}}, req_effective_va_d1[38:0]}
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_negative_limit = {2};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.007 VA_Sv39_高位非法_stage0_cmd0
        // 测试表目标：在noS2：非虚拟化 Host 路径，翻译启用时使用 satp 的 S
        // 阶段场景下发起普通数据读取（load），并开启完整地址检查。构造Sv39地址违反“EVA[63:39] 全部等于 bit38（39位符号扩展）”的边界取值。观察预检查产生PF
        // 页异常的读类输出，并将最终 miss 置0、gpaddr 置为锁存有效地址、vaNeedExt 置0。
        cp_02_03_007 : coverpoint va_boundary_39
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 0
                && req_stage_d1 == 0
                && (req_pre_s1_d1 || req_pre_s2_d1)
                && csr_satp_mode_d1 == 8
                && !(req_effective_va_d1 == {{25{req_effective_va_d1[38]}}, req_effective_va_d1[38:0]})
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)) && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.008 VA_Sv39_高位非法_stage0_cmd1
        // 测试表目标：在noS2：非虚拟化 Host 路径，翻译启用时使用 satp 的 S
        // 阶段场景下发起普通数据写入（store），并开启完整地址检查。构造Sv39地址违反“EVA[63:39] 全部等于 bit38（39位符号扩展）”的边界取值。观察预检查产生PF
        // 页异常的写类输出，并将最终 miss 置0、gpaddr 置为锁存有效地址、vaNeedExt 置0。
        cp_02_03_008 : coverpoint va_boundary_39
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 1
                && req_stage_d1 == 0
                && (req_pre_s1_d1 || req_pre_s2_d1)
                && csr_satp_mode_d1 == 8
                && !(req_effective_va_d1 == {{25{req_effective_va_d1[38]}}, req_effective_va_d1[38:0]})
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)) && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.009 VA_Sv39_高位非法_stage1_cmd0
        // 测试表目标：在onlyStage1：只执行 Guest 的 VS 阶段，G 阶段
        // Bare场景下发起普通数据读取（load），并开启完整地址检查。构造Sv39地址违反“EVA[63:39] 全部等于
        // bit38（39位符号扩展）”的边界取值。观察预检查产生PF 页异常的读类输出，并将最终 miss 置0、gpaddr 置为锁存有效地址、vaNeedExt 置0。
        cp_02_03_009 : coverpoint va_boundary_39
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 0
                && req_stage_d1 == 1
                && (req_pre_s1_d1 || req_pre_s2_d1)
                && csr_vsatp_mode_d1 == 8
                && !(req_effective_va_d1 == {{25{req_effective_va_d1[38]}}, req_effective_va_d1[38:0]})
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)) && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.010 VA_Sv39_高位非法_stage1_cmd1
        // 测试表目标：在onlyStage1：只执行 Guest 的 VS 阶段，G 阶段
        // Bare场景下发起普通数据写入（store），并开启完整地址检查。构造Sv39地址违反“EVA[63:39] 全部等于
        // bit38（39位符号扩展）”的边界取值。观察预检查产生PF 页异常的写类输出，并将最终 miss 置0、gpaddr 置为锁存有效地址、vaNeedExt 置0。
        cp_02_03_010 : coverpoint va_boundary_39
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 1
                && req_stage_d1 == 1
                && (req_pre_s1_d1 || req_pre_s2_d1)
                && csr_vsatp_mode_d1 == 8
                && !(req_effective_va_d1 == {{25{req_effective_va_d1[38]}}, req_effective_va_d1[38:0]})
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)) && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.011 VA_Sv39_高位非法_stage3_cmd0
        // 测试表目标：在allStage：VS 与 G 两阶段组合翻译场景下发起普通数据读取（load），并开启完整地址检查。构造Sv39地址违反“EVA[63:39] 全部等于
        // bit38（39位符号扩展）”的边界取值。观察预检查产生PF 页异常的读类输出，并将最终 miss 置0、gpaddr 置为锁存有效地址、vaNeedExt 置0。
        cp_02_03_011 : coverpoint va_boundary_39
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 0
                && req_stage_d1 == 3
                && (req_pre_s1_d1 || req_pre_s2_d1)
                && csr_vsatp_mode_d1 == 8
                && !(req_effective_va_d1 == {{25{req_effective_va_d1[38]}}, req_effective_va_d1[38:0]})
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)) && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.012 VA_Sv39_高位非法_stage3_cmd1
        // 测试表目标：在allStage：VS 与 G 两阶段组合翻译场景下发起普通数据写入（store），并开启完整地址检查。构造Sv39地址违反“EVA[63:39] 全部等于
        // bit38（39位符号扩展）”的边界取值。观察预检查产生PF 页异常的写类输出，并将最终 miss 置0、gpaddr 置为锁存有效地址、vaNeedExt 置0。
        cp_02_03_012 : coverpoint va_boundary_39
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 1
                && req_stage_d1 == 3
                && (req_pre_s1_d1 || req_pre_s2_d1)
                && csr_vsatp_mode_d1 == 8
                && !(req_effective_va_d1 == {{25{req_effective_va_d1[38]}}, req_effective_va_d1[38:0]})
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)) && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.013 VA_Sv48_合法边界_stage0_cmd0
        // 测试表目标：在noS2：非虚拟化 Host 路径，翻译启用时使用 satp 的 S
        // 阶段场景下发起普通数据读取（load），并开启完整地址检查。构造Sv48地址满足“EVA[63:48] 全部等于
        // bit47（48位符号扩展）”的边界取值。观察该预检查原因不产生异常；后续页表权限或 PMP 仍可能拒绝访问。
        cp_02_03_013 : coverpoint va_boundary_48
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 0
                && req_stage_d1 == 0
                && (req_pre_s1_d1 || req_pre_s2_d1)
                && csr_satp_mode_d1 == 9
                && req_effective_va_d1 == {{16{req_effective_va_d1[47]}}, req_effective_va_d1[47:0]}
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_negative_limit = {2};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.014 VA_Sv48_合法边界_stage0_cmd1
        // 测试表目标：在noS2：非虚拟化 Host 路径，翻译启用时使用 satp 的 S
        // 阶段场景下发起普通数据写入（store），并开启完整地址检查。构造Sv48地址满足“EVA[63:48] 全部等于
        // bit47（48位符号扩展）”的边界取值。观察该预检查原因不产生异常；后续页表权限或 PMP 仍可能拒绝访问。
        cp_02_03_014 : coverpoint va_boundary_48
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 1
                && req_stage_d1 == 0
                && (req_pre_s1_d1 || req_pre_s2_d1)
                && csr_satp_mode_d1 == 9
                && req_effective_va_d1 == {{16{req_effective_va_d1[47]}}, req_effective_va_d1[47:0]}
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_negative_limit = {2};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.015 VA_Sv48_合法边界_stage1_cmd0
        // 测试表目标：在onlyStage1：只执行 Guest 的 VS 阶段，G 阶段
        // Bare场景下发起普通数据读取（load），并开启完整地址检查。构造Sv48地址满足“EVA[63:48] 全部等于
        // bit47（48位符号扩展）”的边界取值。观察该预检查原因不产生异常；后续页表权限或 PMP 仍可能拒绝访问。
        cp_02_03_015 : coverpoint va_boundary_48
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 0
                && req_stage_d1 == 1
                && (req_pre_s1_d1 || req_pre_s2_d1)
                && csr_vsatp_mode_d1 == 9
                && req_effective_va_d1 == {{16{req_effective_va_d1[47]}}, req_effective_va_d1[47:0]}
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_negative_limit = {2};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.016 VA_Sv48_合法边界_stage1_cmd1
        // 测试表目标：在onlyStage1：只执行 Guest 的 VS 阶段，G 阶段
        // Bare场景下发起普通数据写入（store），并开启完整地址检查。构造Sv48地址满足“EVA[63:48] 全部等于
        // bit47（48位符号扩展）”的边界取值。观察该预检查原因不产生异常；后续页表权限或 PMP 仍可能拒绝访问。
        cp_02_03_016 : coverpoint va_boundary_48
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 1
                && req_stage_d1 == 1
                && (req_pre_s1_d1 || req_pre_s2_d1)
                && csr_vsatp_mode_d1 == 9
                && req_effective_va_d1 == {{16{req_effective_va_d1[47]}}, req_effective_va_d1[47:0]}
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_negative_limit = {2};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.017 VA_Sv48_合法边界_stage3_cmd0
        // 测试表目标：在allStage：VS 与 G 两阶段组合翻译场景下发起普通数据读取（load），并开启完整地址检查。构造Sv48地址满足“EVA[63:48] 全部等于
        // bit47（48位符号扩展）”的边界取值。观察该预检查原因不产生异常；后续页表权限或 PMP 仍可能拒绝访问。
        cp_02_03_017 : coverpoint va_boundary_48
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 0
                && req_stage_d1 == 3
                && (req_pre_s1_d1 || req_pre_s2_d1)
                && csr_vsatp_mode_d1 == 9
                && req_effective_va_d1 == {{16{req_effective_va_d1[47]}}, req_effective_va_d1[47:0]}
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_negative_limit = {2};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.018 VA_Sv48_合法边界_stage3_cmd1
        // 测试表目标：在allStage：VS 与 G 两阶段组合翻译场景下发起普通数据写入（store），并开启完整地址检查。构造Sv48地址满足“EVA[63:48] 全部等于
        // bit47（48位符号扩展）”的边界取值。观察该预检查原因不产生异常；后续页表权限或 PMP 仍可能拒绝访问。
        cp_02_03_018 : coverpoint va_boundary_48
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 1
                && req_stage_d1 == 3
                && (req_pre_s1_d1 || req_pre_s2_d1)
                && csr_vsatp_mode_d1 == 9
                && req_effective_va_d1 == {{16{req_effective_va_d1[47]}}, req_effective_va_d1[47:0]}
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_negative_limit = {2};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.019 VA_Sv48_高位非法_stage0_cmd0
        // 测试表目标：在noS2：非虚拟化 Host 路径，翻译启用时使用 satp 的 S
        // 阶段场景下发起普通数据读取（load），并开启完整地址检查。构造Sv48地址违反“EVA[63:48] 全部等于 bit47（48位符号扩展）”的边界取值。观察预检查产生PF
        // 页异常的读类输出，并将最终 miss 置0、gpaddr 置为锁存有效地址、vaNeedExt 置0。
        cp_02_03_019 : coverpoint va_boundary_48
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 0
                && req_stage_d1 == 0
                && (req_pre_s1_d1 || req_pre_s2_d1)
                && csr_satp_mode_d1 == 9
                && !(req_effective_va_d1 == {{16{req_effective_va_d1[47]}}, req_effective_va_d1[47:0]})
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)) && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.020 VA_Sv48_高位非法_stage0_cmd1
        // 测试表目标：在noS2：非虚拟化 Host 路径，翻译启用时使用 satp 的 S
        // 阶段场景下发起普通数据写入（store），并开启完整地址检查。构造Sv48地址违反“EVA[63:48] 全部等于 bit47（48位符号扩展）”的边界取值。观察预检查产生PF
        // 页异常的写类输出，并将最终 miss 置0、gpaddr 置为锁存有效地址、vaNeedExt 置0。
        cp_02_03_020 : coverpoint va_boundary_48
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 1
                && req_stage_d1 == 0
                && (req_pre_s1_d1 || req_pre_s2_d1)
                && csr_satp_mode_d1 == 9
                && !(req_effective_va_d1 == {{16{req_effective_va_d1[47]}}, req_effective_va_d1[47:0]})
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)) && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.021 VA_Sv48_高位非法_stage1_cmd0
        // 测试表目标：在onlyStage1：只执行 Guest 的 VS 阶段，G 阶段
        // Bare场景下发起普通数据读取（load），并开启完整地址检查。构造Sv48地址违反“EVA[63:48] 全部等于
        // bit47（48位符号扩展）”的边界取值。观察预检查产生PF 页异常的读类输出，并将最终 miss 置0、gpaddr 置为锁存有效地址、vaNeedExt 置0。
        cp_02_03_021 : coverpoint va_boundary_48
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 0
                && req_stage_d1 == 1
                && (req_pre_s1_d1 || req_pre_s2_d1)
                && csr_vsatp_mode_d1 == 9
                && !(req_effective_va_d1 == {{16{req_effective_va_d1[47]}}, req_effective_va_d1[47:0]})
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)) && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.022 VA_Sv48_高位非法_stage1_cmd1
        // 测试表目标：在onlyStage1：只执行 Guest 的 VS 阶段，G 阶段
        // Bare场景下发起普通数据写入（store），并开启完整地址检查。构造Sv48地址违反“EVA[63:48] 全部等于
        // bit47（48位符号扩展）”的边界取值。观察预检查产生PF 页异常的写类输出，并将最终 miss 置0、gpaddr 置为锁存有效地址、vaNeedExt 置0。
        cp_02_03_022 : coverpoint va_boundary_48
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 1
                && req_stage_d1 == 1
                && (req_pre_s1_d1 || req_pre_s2_d1)
                && csr_vsatp_mode_d1 == 9
                && !(req_effective_va_d1 == {{16{req_effective_va_d1[47]}}, req_effective_va_d1[47:0]})
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)) && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.023 VA_Sv48_高位非法_stage3_cmd0
        // 测试表目标：在allStage：VS 与 G 两阶段组合翻译场景下发起普通数据读取（load），并开启完整地址检查。构造Sv48地址违反“EVA[63:48] 全部等于
        // bit47（48位符号扩展）”的边界取值。观察预检查产生PF 页异常的读类输出，并将最终 miss 置0、gpaddr 置为锁存有效地址、vaNeedExt 置0。
        cp_02_03_023 : coverpoint va_boundary_48
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 0
                && req_stage_d1 == 3
                && (req_pre_s1_d1 || req_pre_s2_d1)
                && csr_vsatp_mode_d1 == 9
                && !(req_effective_va_d1 == {{16{req_effective_va_d1[47]}}, req_effective_va_d1[47:0]})
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)) && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.024 VA_Sv48_高位非法_stage3_cmd1
        // 测试表目标：在allStage：VS 与 G 两阶段组合翻译场景下发起普通数据写入（store），并开启完整地址检查。构造Sv48地址违反“EVA[63:48] 全部等于
        // bit47（48位符号扩展）”的边界取值。观察预检查产生PF 页异常的写类输出，并将最终 miss 置0、gpaddr 置为锁存有效地址、vaNeedExt 置0。
        cp_02_03_024 : coverpoint va_boundary_48
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 1
                && req_stage_d1 == 3
                && (req_pre_s1_d1 || req_pre_s2_d1)
                && csr_vsatp_mode_d1 == 9
                && !(req_effective_va_d1 == {{16{req_effective_va_d1[47]}}, req_effective_va_d1[47:0]})
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)) && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.025 VA_Sv39x4_合法边界_stage2_cmd0
        // 测试表目标：在onlyStage2：VS 阶段 Bare，只执行 G 阶段；是否实际翻译还需看
        // TE场景下发起普通数据读取（load），并开启完整地址检查。构造Sv39x4地址满足“EVA[63:41] 全为0，GPA
        // 限于41位”的边界取值。观察该预检查原因不产生异常；后续页表权限或 PMP 仍可能拒绝访问。
        cp_02_03_025 : coverpoint va_boundary_41_unsigned
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 0
                && req_stage_d1 == 2
                && req_pre_s2_d1
                && csr_hgatp_mode_d1 == 8
                && req_effective_va_d1[63:41] == 0
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.026 VA_Sv39x4_合法边界_stage2_cmd1
        // 测试表目标：在onlyStage2：VS 阶段 Bare，只执行 G 阶段；是否实际翻译还需看
        // TE场景下发起普通数据写入（store），并开启完整地址检查。构造Sv39x4地址满足“EVA[63:41] 全为0，GPA
        // 限于41位”的边界取值。观察该预检查原因不产生异常；后续页表权限或 PMP 仍可能拒绝访问。
        cp_02_03_026 : coverpoint va_boundary_41_unsigned
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 1
                && req_stage_d1 == 2
                && req_pre_s2_d1
                && csr_hgatp_mode_d1 == 8
                && req_effective_va_d1[63:41] == 0
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.027 VA_Sv39x4_高位非法_stage2_cmd0
        // 测试表目标：在onlyStage2：VS 阶段 Bare，只执行 G 阶段；是否实际翻译还需看
        // TE场景下发起普通数据读取（load），并开启完整地址检查。构造Sv39x4地址违反“EVA[63:41] 全为0，GPA 限于41位”的边界取值。观察预检查产生GPF
        // Guest 页异常的读类输出，并将最终 miss 置0、gpaddr 置为锁存有效地址、vaNeedExt 置0。
        cp_02_03_027 : coverpoint va_boundary_41_unsigned
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 0
                && req_stage_d1 == 2
                && req_pre_s2_d1
                && csr_hgatp_mode_d1 == 8
                && !(req_effective_va_d1[63:41] == 0)
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)) && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.028 VA_Sv39x4_高位非法_stage2_cmd1
        // 测试表目标：在onlyStage2：VS 阶段 Bare，只执行 G 阶段；是否实际翻译还需看
        // TE场景下发起普通数据写入（store），并开启完整地址检查。构造Sv39x4地址违反“EVA[63:41] 全为0，GPA 限于41位”的边界取值。观察预检查产生GPF
        // Guest 页异常的写类输出，并将最终 miss 置0、gpaddr 置为锁存有效地址、vaNeedExt 置0。
        cp_02_03_028 : coverpoint va_boundary_41_unsigned
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 1
                && req_stage_d1 == 2
                && req_pre_s2_d1
                && csr_hgatp_mode_d1 == 8
                && !(req_effective_va_d1[63:41] == 0)
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)) && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.029 VA_Sv48x4_合法边界_stage2_cmd0
        // 测试表目标：在onlyStage2：VS 阶段 Bare，只执行 G 阶段；是否实际翻译还需看
        // TE场景下发起普通数据读取（load），并开启完整地址检查。构造Sv48x4地址满足“EVA[63:50] 全为0，GPA
        // 限于50位”的边界取值。观察该预检查原因不产生异常；后续页表权限或 PMP 仍可能拒绝访问。
        cp_02_03_029 : coverpoint va_boundary_50_unsigned
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 0
                && req_stage_d1 == 2
                && req_pre_s2_d1
                && csr_hgatp_mode_d1 == 9
                && req_effective_va_d1[63:50] == 0
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.030 VA_Sv48x4_合法边界_stage2_cmd1
        // 测试表目标：在onlyStage2：VS 阶段 Bare，只执行 G 阶段；是否实际翻译还需看
        // TE场景下发起普通数据写入（store），并开启完整地址检查。构造Sv48x4地址满足“EVA[63:50] 全为0，GPA
        // 限于50位”的边界取值。观察该预检查原因不产生异常；后续页表权限或 PMP 仍可能拒绝访问。
        cp_02_03_030 : coverpoint va_boundary_50_unsigned
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 1
                && req_stage_d1 == 2
                && req_pre_s2_d1
                && csr_hgatp_mode_d1 == 9
                && req_effective_va_d1[63:50] == 0
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.031 VA_Sv48x4_高位非法_stage2_cmd0
        // 测试表目标：在onlyStage2：VS 阶段 Bare，只执行 G 阶段；是否实际翻译还需看
        // TE场景下发起普通数据读取（load），并开启完整地址检查。构造Sv48x4地址违反“EVA[63:50] 全为0，GPA 限于50位”的边界取值。观察预检查产生GPF
        // Guest 页异常的读类输出，并将最终 miss 置0、gpaddr 置为锁存有效地址、vaNeedExt 置0。
        cp_02_03_031 : coverpoint va_boundary_50_unsigned
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 0
                && req_stage_d1 == 2
                && req_pre_s2_d1
                && csr_hgatp_mode_d1 == 9
                && !(req_effective_va_d1[63:50] == 0)
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)) && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.032 VA_Sv48x4_高位非法_stage2_cmd1
        // 测试表目标：在onlyStage2：VS 阶段 Bare，只执行 G 阶段；是否实际翻译还需看
        // TE场景下发起普通数据写入（store），并开启完整地址检查。构造Sv48x4地址违反“EVA[63:50] 全为0，GPA 限于50位”的边界取值。观察预检查产生GPF
        // Guest 页异常的写类输出，并将最终 miss 置0、gpaddr 置为锁存有效地址、vaNeedExt 置0。
        cp_02_03_032 : coverpoint va_boundary_50_unsigned
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 1
                && req_stage_d1 == 2
                && req_pre_s2_d1
                && csr_hgatp_mode_d1 == 9
                && !(req_effective_va_d1[63:50] == 0)
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)) && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.033 VA_BarePA_合法边界_stage0_cmd0
        // 测试表目标：在noS2：非虚拟化 Host 路径，翻译启用时使用 satp 的 S
        // 阶段场景下发起普通数据读取（load），并开启完整地址检查。本点显式关闭翻译使能，即使阶段编码非0也按直通地址检查。构造BarePA地址满足“EVA[63:48]
        // 全为0，直通物理地址限于48位”的边界取值。观察该预检查原因不产生异常；后续页表权限或 PMP 仍可能拒绝访问。
        cp_02_03_033 : coverpoint va_boundary_48_unsigned
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 0
                && req_stage_d1 == 0
                && !req_pre_s1_d1
                && !req_pre_s2_d1
                && req_effective_va_d1[63:48] == 0
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.034 VA_BarePA_合法边界_stage0_cmd1
        // 测试表目标：在noS2：非虚拟化 Host 路径，翻译启用时使用 satp 的 S
        // 阶段场景下发起普通数据写入（store），并开启完整地址检查。本点显式关闭翻译使能，即使阶段编码非0也按直通地址检查。构造BarePA地址满足“EVA[63:48]
        // 全为0，直通物理地址限于48位”的边界取值。观察该预检查原因不产生异常；后续页表权限或 PMP 仍可能拒绝访问。
        cp_02_03_034 : coverpoint va_boundary_48_unsigned
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 1
                && req_stage_d1 == 0
                && !req_pre_s1_d1
                && !req_pre_s2_d1
                && req_effective_va_d1[63:48] == 0
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.035 VA_BarePA_合法边界_stage1_cmd0
        // 测试表目标：在onlyStage1：只执行 Guest 的 VS 阶段，G 阶段
        // Bare场景下发起普通数据读取（load），并开启完整地址检查。本点显式关闭翻译使能，即使阶段编码非0也按直通地址检查。构造BarePA地址满足“EVA[63:48]
        // 全为0，直通物理地址限于48位”的边界取值。观察该预检查原因不产生异常；后续页表权限或 PMP 仍可能拒绝访问。
        cp_02_03_035 : coverpoint va_boundary_48_unsigned
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 0
                && req_stage_d1 == 1
                && !req_pre_s1_d1
                && !req_pre_s2_d1
                && req_effective_va_d1[63:48] == 0
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.036 VA_BarePA_合法边界_stage1_cmd1
        // 测试表目标：在onlyStage1：只执行 Guest 的 VS 阶段，G 阶段
        // Bare场景下发起普通数据写入（store），并开启完整地址检查。本点显式关闭翻译使能，即使阶段编码非0也按直通地址检查。构造BarePA地址满足“EVA[63:48]
        // 全为0，直通物理地址限于48位”的边界取值。观察该预检查原因不产生异常；后续页表权限或 PMP 仍可能拒绝访问。
        cp_02_03_036 : coverpoint va_boundary_48_unsigned
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 1
                && req_stage_d1 == 1
                && !req_pre_s1_d1
                && !req_pre_s2_d1
                && req_effective_va_d1[63:48] == 0
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.037 VA_BarePA_合法边界_stage2_cmd0
        // 测试表目标：在onlyStage2：VS 阶段 Bare，只执行 G 阶段；是否实际翻译还需看
        // TE场景下发起普通数据读取（load），并开启完整地址检查。本点显式关闭翻译使能，即使阶段编码非0也按直通地址检查。构造BarePA地址满足“EVA[63:48]
        // 全为0，直通物理地址限于48位”的边界取值。观察该预检查原因不产生异常；后续页表权限或 PMP 仍可能拒绝访问。
        cp_02_03_037 : coverpoint va_boundary_48_unsigned
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 0
                && req_stage_d1 == 2
                && !req_pre_s1_d1
                && !req_pre_s2_d1
                && req_effective_va_d1[63:48] == 0
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.038 VA_BarePA_合法边界_stage2_cmd1
        // 测试表目标：在onlyStage2：VS 阶段 Bare，只执行 G 阶段；是否实际翻译还需看
        // TE场景下发起普通数据写入（store），并开启完整地址检查。本点显式关闭翻译使能，即使阶段编码非0也按直通地址检查。构造BarePA地址满足“EVA[63:48]
        // 全为0，直通物理地址限于48位”的边界取值。观察该预检查原因不产生异常；后续页表权限或 PMP 仍可能拒绝访问。
        cp_02_03_038 : coverpoint va_boundary_48_unsigned
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 1
                && req_stage_d1 == 2
                && !req_pre_s1_d1
                && !req_pre_s2_d1
                && req_effective_va_d1[63:48] == 0
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.039 VA_BarePA_合法边界_stage3_cmd0
        // 测试表目标：在allStage：VS 与 G
        // 两阶段组合翻译场景下发起普通数据读取（load），并开启完整地址检查。本点显式关闭翻译使能，即使阶段编码非0也按直通地址检查。构造BarePA地址满足“EVA[63:48]
        // 全为0，直通物理地址限于48位”的边界取值。观察该预检查原因不产生异常；后续页表权限或 PMP 仍可能拒绝访问。
        cp_02_03_039 : coverpoint va_boundary_48_unsigned
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 0
                && req_stage_d1 == 3
                && !req_pre_s1_d1
                && !req_pre_s2_d1
                && req_effective_va_d1[63:48] == 0
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.040 VA_BarePA_合法边界_stage3_cmd1
        // 测试表目标：在allStage：VS 与 G
        // 两阶段组合翻译场景下发起普通数据写入（store），并开启完整地址检查。本点显式关闭翻译使能，即使阶段编码非0也按直通地址检查。构造BarePA地址满足“EVA[63:48]
        // 全为0，直通物理地址限于48位”的边界取值。观察该预检查原因不产生异常；后续页表权限或 PMP 仍可能拒绝访问。
        cp_02_03_040 : coverpoint va_boundary_48_unsigned
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 1
                && req_stage_d1 == 3
                && !req_pre_s1_d1
                && !req_pre_s2_d1
                && req_effective_va_d1[63:48] == 0
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.041 VA_BarePA_高位非法_stage0_cmd0
        // 测试表目标：在noS2：非虚拟化 Host 路径，翻译启用时使用 satp 的 S
        // 阶段场景下发起普通数据读取（load），并开启完整地址检查。本点显式关闭翻译使能，即使阶段编码非0也按直通地址检查。构造BarePA地址违反“EVA[63:48]
        // 全为0，直通物理地址限于48位”的边界取值。观察预检查产生AF 访问异常的读类输出，并将最终 miss 置0、gpaddr 置为锁存有效地址、vaNeedExt 置0。
        cp_02_03_041 : coverpoint va_boundary_48_unsigned
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 0
                && req_stage_d1 == 0
                && !req_pre_s1_d1
                && !req_pre_s2_d1
                && !(req_effective_va_d1[63:48] == 0)
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)) && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.042 VA_BarePA_高位非法_stage0_cmd1
        // 测试表目标：在noS2：非虚拟化 Host 路径，翻译启用时使用 satp 的 S
        // 阶段场景下发起普通数据写入（store），并开启完整地址检查。本点显式关闭翻译使能，即使阶段编码非0也按直通地址检查。构造BarePA地址违反“EVA[63:48]
        // 全为0，直通物理地址限于48位”的边界取值。观察预检查产生AF 访问异常的写类输出，并将最终 miss 置0、gpaddr 置为锁存有效地址、vaNeedExt 置0。
        cp_02_03_042 : coverpoint va_boundary_48_unsigned
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 1
                && req_stage_d1 == 0
                && !req_pre_s1_d1
                && !req_pre_s2_d1
                && !(req_effective_va_d1[63:48] == 0)
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)) && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.043 VA_BarePA_高位非法_stage1_cmd0
        // 测试表目标：在onlyStage1：只执行 Guest 的 VS 阶段，G 阶段
        // Bare场景下发起普通数据读取（load），并开启完整地址检查。本点显式关闭翻译使能，即使阶段编码非0也按直通地址检查。构造BarePA地址违反“EVA[63:48]
        // 全为0，直通物理地址限于48位”的边界取值。观察预检查产生AF 访问异常的读类输出，并将最终 miss 置0、gpaddr 置为锁存有效地址、vaNeedExt 置0。
        cp_02_03_043 : coverpoint va_boundary_48_unsigned
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 0
                && req_stage_d1 == 1
                && !req_pre_s1_d1
                && !req_pre_s2_d1
                && !(req_effective_va_d1[63:48] == 0)
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)) && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.044 VA_BarePA_高位非法_stage1_cmd1
        // 测试表目标：在onlyStage1：只执行 Guest 的 VS 阶段，G 阶段
        // Bare场景下发起普通数据写入（store），并开启完整地址检查。本点显式关闭翻译使能，即使阶段编码非0也按直通地址检查。构造BarePA地址违反“EVA[63:48]
        // 全为0，直通物理地址限于48位”的边界取值。观察预检查产生AF 访问异常的写类输出，并将最终 miss 置0、gpaddr 置为锁存有效地址、vaNeedExt 置0。
        cp_02_03_044 : coverpoint va_boundary_48_unsigned
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 1
                && req_stage_d1 == 1
                && !req_pre_s1_d1
                && !req_pre_s2_d1
                && !(req_effective_va_d1[63:48] == 0)
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)) && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.045 VA_BarePA_高位非法_stage2_cmd0
        // 测试表目标：在onlyStage2：VS 阶段 Bare，只执行 G 阶段；是否实际翻译还需看
        // TE场景下发起普通数据读取（load），并开启完整地址检查。本点显式关闭翻译使能，即使阶段编码非0也按直通地址检查。构造BarePA地址违反“EVA[63:48]
        // 全为0，直通物理地址限于48位”的边界取值。观察预检查产生AF 访问异常的读类输出，并将最终 miss 置0、gpaddr 置为锁存有效地址、vaNeedExt 置0。
        cp_02_03_045 : coverpoint va_boundary_48_unsigned
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 0
                && req_stage_d1 == 2
                && !req_pre_s1_d1
                && !req_pre_s2_d1
                && !(req_effective_va_d1[63:48] == 0)
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)) && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.046 VA_BarePA_高位非法_stage2_cmd1
        // 测试表目标：在onlyStage2：VS 阶段 Bare，只执行 G 阶段；是否实际翻译还需看
        // TE场景下发起普通数据写入（store），并开启完整地址检查。本点显式关闭翻译使能，即使阶段编码非0也按直通地址检查。构造BarePA地址违反“EVA[63:48]
        // 全为0，直通物理地址限于48位”的边界取值。观察预检查产生AF 访问异常的写类输出，并将最终 miss 置0、gpaddr 置为锁存有效地址、vaNeedExt 置0。
        cp_02_03_046 : coverpoint va_boundary_48_unsigned
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 1
                && req_stage_d1 == 2
                && !req_pre_s1_d1
                && !req_pre_s2_d1
                && !(req_effective_va_d1[63:48] == 0)
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)) && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.047 VA_BarePA_高位非法_stage3_cmd0
        // 测试表目标：在allStage：VS 与 G
        // 两阶段组合翻译场景下发起普通数据读取（load），并开启完整地址检查。本点显式关闭翻译使能，即使阶段编码非0也按直通地址检查。构造BarePA地址违反“EVA[63:48]
        // 全为0，直通物理地址限于48位”的边界取值。观察预检查产生AF 访问异常的读类输出，并将最终 miss 置0、gpaddr 置为锁存有效地址、vaNeedExt 置0。
        cp_02_03_047 : coverpoint va_boundary_48_unsigned
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 0
                && req_stage_d1 == 3
                && !req_pre_s1_d1
                && !req_pre_s2_d1
                && !(req_effective_va_d1[63:48] == 0)
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)) && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.048 VA_BarePA_高位非法_stage3_cmd1
        // 测试表目标：在allStage：VS 与 G
        // 两阶段组合翻译场景下发起普通数据写入（store），并开启完整地址检查。本点显式关闭翻译使能，即使阶段编码非0也按直通地址检查。构造BarePA地址违反“EVA[63:48]
        // 全为0，直通物理地址限于48位”的边界取值。观察预检查产生AF 访问异常的写类输出，并将最终 miss 置0、gpaddr 置为锁存有效地址、vaNeedExt 置0。
        cp_02_03_048 : coverpoint va_boundary_48_unsigned
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_checkfullva_d1
                && req_cmd_d1 == 1
                && req_stage_d1 == 3
                && !req_pre_s1_d1
                && !req_pre_s2_d1
                && !(req_effective_va_d1[63:48] == 0)
                && (((resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                    ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
                    6'b010101) == 0 : 1'b1)) && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.049 VA_关闭检查
        // 测试表目标：场景：R.checkfullva=0，fullva高位非法。 观察功能：不触发pre-fault；仍可产生页表权限fault。
        // 测试目的：观察完整地址在查表前的合法性检查，以及预异常如何影响最终异常类型和地址报告。
        cp_02_03_049 : coverpoint (!pre_fault)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && HAS_FULLVA
                && !req_checkfullva_d1
                && high_address_bad) {
            bins b_expected_result = {1'b1};
        }
        // 02.03.050 VA_prefault优先级
        // 测试表目标：场景：checkfullva=1产生pre-fault，同时entry permission/PBMT/PTW fault任意。
        // 观察功能：输出走pre-fault分支；isForVSnonLeafPTE=0，miss强制0；不得用miss_read内部值代替最终miss。
        // 测试目的：观察完整地址在查表前的合法性检查，以及预异常如何影响最终异常类型和地址报告。
        cp_02_03_050 : coverpoint (resp_valid && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 ||
            req_out_cmd == 3'd4) ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5)
            ? (resp_excp & 6'b010101) == 0 : 1'b1)) && !resp_miss)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && pre_fault) {
            bins b_expected_result = {1'b1};
        }
        // 02.03.051 VA_prefault与PTW请求
        // 测试表目标：场景：pre-fault=1且内部miss_read=1。 观察功能：分别统计最终miss=0与Q.valid/tlbreplay；预检查不一定抑制内部PTW请求。
        // 测试目的：观察完整地址在查表前的合法性检查，以及预异常如何影响最终异常类型和地址报告。
        cp_02_03_051 : coverpoint (!resp_miss)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && pre_fault
                && resp_miss_read) {
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_02_03 cg_dtlb_02_03_inst;

    // 02.04 PMM来源与屏蔽长度
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_02_04】02.04 PMM来源与屏蔽长度
    // 探测功能：观察当前特权和 Hyper/虚拟化状态选中哪一个 PMM 控制字段，以及有效地址如何重建。
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：02.04.001, 02.04.002, 02.04.003, 02.04.004, 02.04.005, 02.04.006, 02.04.007,
    // 02.04.008, 02.04.009, 02.04.010, 02.04.011, 02.04.012, 02.04.013, 02.04.014, 02.04.015,
    // 02.04.016, 02.04.017, 02.04.018, 02.04.019, 02.04.020, 02.04.021, 02.04.022, 02.04.023,
    // 02.04.024, 02.04.025, 02.04.026, 02.04.027, 02.04.028, 02.04.029, 02.04.030, 02.04.031,
    // 02.04.032, 02.04.033, 02.04.034, 02.04.035, 02.04.036, 02.04.037, 02.04.038,
    // 02.04.039；逐点功能见下方注释，精确配值见分解表。
    // expected_effective_va：PMM参考地址：禁用时保持fullva；PMM=2保留57位，=3保留48位；S1使能时符号扩展，否则补0。
    // pmm_address_class：被屏蔽位分类：0=全0，1=全1，2=混合。
    covergroup cg_dtlb_02_04 @(posedge clock);
        option.per_instance = 1;
        // 02.04.001 PMM_HS_PMLEN0
        // 测试表目标：构造Host S/HS 普通访存，由 menvcfg.PMM=0 选择不屏蔽指针高位（PMLEN=0）。观察 fullva
        // 不做指针屏蔽。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求 vaddr，需由上游保证两者符合接口约定。
        cp_02_04_001 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 1
                && csr_virt_d1 == 0
                && req_hyperinst_d1 == 0
                && csr_dmode_d1 == 1
                && csr_menvcfg_d1 == 0
                && csr_mxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.002 PMM_HS_PMLEN7
        // 测试表目标：构造Host S/HS 普通访存，由 menvcfg.PMM=2 选择屏蔽高7位，保留低57位（PMLEN=7）。观察有 S1
        // 翻译时按保留位的最高位符号扩展到64位，直通或仅 G 阶段时零扩展到64位。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求
        // vaddr，需由上游保证两者符合接口约定。
        cp_02_04_002 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 1
                && csr_virt_d1 == 0
                && req_hyperinst_d1 == 0
                && csr_dmode_d1 == 1
                && csr_menvcfg_d1 == 2
                && csr_mxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.003 PMM_HS_PMLEN16
        // 测试表目标：构造Host S/HS 普通访存，由 menvcfg.PMM=3 选择屏蔽高16位，保留低48位（PMLEN=16）。观察有 S1
        // 翻译时按保留位的最高位符号扩展到64位，直通或仅 G 阶段时零扩展到64位。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求
        // vaddr，需由上游保证两者符合接口约定。
        cp_02_04_003 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 1
                && csr_virt_d1 == 0
                && req_hyperinst_d1 == 0
                && csr_dmode_d1 == 1
                && csr_menvcfg_d1 == 3
                && csr_mxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.004 PMM_HS_HYP_S_PMLEN0
        // 测试表目标：构造HS 发起 Hyper 访存，SPVP=S，由 henvcfg.PMM=0 选择不屏蔽指针高位（PMLEN=0）。观察 fullva
        // 不做指针屏蔽。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求 vaddr，需由上游保证两者符合接口约定。
        cp_02_04_004 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 1
                && csr_virt_d1 == 0
                && req_hyperinst_d1 == 1
                && csr_spvp_d1 == 1
                && csr_henvcfg_d1 == 0
                && csr_mxr_d1 == 0
                && csr_vmxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.005 PMM_HS_HYP_S_PMLEN7
        // 测试表目标：构造HS 发起 Hyper 访存，SPVP=S，由 henvcfg.PMM=2 选择屏蔽高7位，保留低57位（PMLEN=7）。观察有 S1
        // 翻译时按保留位的最高位符号扩展到64位，直通或仅 G 阶段时零扩展到64位。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求
        // vaddr，需由上游保证两者符合接口约定。
        cp_02_04_005 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 1
                && csr_virt_d1 == 0
                && req_hyperinst_d1 == 1
                && csr_spvp_d1 == 1
                && csr_henvcfg_d1 == 2
                && csr_mxr_d1 == 0
                && csr_vmxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.006 PMM_HS_HYP_S_PMLEN16
        // 测试表目标：构造HS 发起 Hyper 访存，SPVP=S，由 henvcfg.PMM=3 选择屏蔽高16位，保留低48位（PMLEN=16）。观察有 S1
        // 翻译时按保留位的最高位符号扩展到64位，直通或仅 G 阶段时零扩展到64位。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求
        // vaddr，需由上游保证两者符合接口约定。
        cp_02_04_006 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 1
                && csr_virt_d1 == 0
                && req_hyperinst_d1 == 1
                && csr_spvp_d1 == 1
                && csr_henvcfg_d1 == 3
                && csr_mxr_d1 == 0
                && csr_vmxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.007 PMM_HS_HYP_U_PMLEN0
        // 测试表目标：构造HS 发起 Hyper 访存，SPVP=U，由 senvcfg.PMM=0 选择不屏蔽指针高位（PMLEN=0）。观察 fullva
        // 不做指针屏蔽。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求 vaddr，需由上游保证两者符合接口约定。
        cp_02_04_007 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 1
                && csr_virt_d1 == 0
                && req_hyperinst_d1 == 1
                && csr_spvp_d1 == 0
                && csr_senvcfg_d1 == 0
                && csr_mxr_d1 == 0
                && csr_vmxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.008 PMM_HS_HYP_U_PMLEN7
        // 测试表目标：构造HS 发起 Hyper 访存，SPVP=U，由 senvcfg.PMM=2 选择屏蔽高7位，保留低57位（PMLEN=7）。观察有 S1
        // 翻译时按保留位的最高位符号扩展到64位，直通或仅 G 阶段时零扩展到64位。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求
        // vaddr，需由上游保证两者符合接口约定。
        cp_02_04_008 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 1
                && csr_virt_d1 == 0
                && req_hyperinst_d1 == 1
                && csr_spvp_d1 == 0
                && csr_senvcfg_d1 == 2
                && csr_mxr_d1 == 0
                && csr_vmxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.009 PMM_HS_HYP_U_PMLEN16
        // 测试表目标：构造HS 发起 Hyper 访存，SPVP=U，由 senvcfg.PMM=3 选择屏蔽高16位，保留低48位（PMLEN=16）。观察有 S1
        // 翻译时按保留位的最高位符号扩展到64位，直通或仅 G 阶段时零扩展到64位。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求
        // vaddr，需由上游保证两者符合接口约定。
        cp_02_04_009 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 1
                && csr_virt_d1 == 0
                && req_hyperinst_d1 == 1
                && csr_spvp_d1 == 0
                && csr_senvcfg_d1 == 3
                && csr_mxr_d1 == 0
                && csr_vmxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.010 PMM_HU_HYP_S_PMLEN0
        // 测试表目标：构造HU 发起 Hyper 访存，SPVP=S，由 henvcfg.PMM=0 选择不屏蔽指针高位（PMLEN=0）。观察 fullva
        // 不做指针屏蔽。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求 vaddr，需由上游保证两者符合接口约定。
        cp_02_04_010 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 0
                && csr_virt_d1 == 0
                && req_hyperinst_d1 == 1
                && csr_spvp_d1 == 1
                && csr_henvcfg_d1 == 0
                && csr_mxr_d1 == 0
                && csr_vmxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.011 PMM_HU_HYP_S_PMLEN7
        // 测试表目标：构造HU 发起 Hyper 访存，SPVP=S，由 henvcfg.PMM=2 选择屏蔽高7位，保留低57位（PMLEN=7）。观察有 S1
        // 翻译时按保留位的最高位符号扩展到64位，直通或仅 G 阶段时零扩展到64位。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求
        // vaddr，需由上游保证两者符合接口约定。
        cp_02_04_011 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 0
                && csr_virt_d1 == 0
                && req_hyperinst_d1 == 1
                && csr_spvp_d1 == 1
                && csr_henvcfg_d1 == 2
                && csr_mxr_d1 == 0
                && csr_vmxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.012 PMM_HU_HYP_S_PMLEN16
        // 测试表目标：构造HU 发起 Hyper 访存，SPVP=S，由 henvcfg.PMM=3 选择屏蔽高16位，保留低48位（PMLEN=16）。观察有 S1
        // 翻译时按保留位的最高位符号扩展到64位，直通或仅 G 阶段时零扩展到64位。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求
        // vaddr，需由上游保证两者符合接口约定。
        cp_02_04_012 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 0
                && csr_virt_d1 == 0
                && req_hyperinst_d1 == 1
                && csr_spvp_d1 == 1
                && csr_henvcfg_d1 == 3
                && csr_mxr_d1 == 0
                && csr_vmxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.013 PMM_HU_HYP_U_PMLEN0
        // 测试表目标：构造HU 发起 Hyper 访存，SPVP=U，由 hstatus.PMM=0 选择不屏蔽指针高位（PMLEN=0）。观察 fullva
        // 不做指针屏蔽。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求 vaddr，需由上游保证两者符合接口约定。
        cp_02_04_013 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 0
                && csr_virt_d1 == 0
                && req_hyperinst_d1 == 1
                && csr_spvp_d1 == 0
                && csr_hstatus_d1 == 0
                && csr_mxr_d1 == 0
                && csr_vmxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.014 PMM_HU_HYP_U_PMLEN7
        // 测试表目标：构造HU 发起 Hyper 访存，SPVP=U，由 hstatus.PMM=2 选择屏蔽高7位，保留低57位（PMLEN=7）。观察有 S1
        // 翻译时按保留位的最高位符号扩展到64位，直通或仅 G 阶段时零扩展到64位。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求
        // vaddr，需由上游保证两者符合接口约定。
        cp_02_04_014 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 0
                && csr_virt_d1 == 0
                && req_hyperinst_d1 == 1
                && csr_spvp_d1 == 0
                && csr_hstatus_d1 == 2
                && csr_mxr_d1 == 0
                && csr_vmxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.015 PMM_HU_HYP_U_PMLEN16
        // 测试表目标：构造HU 发起 Hyper 访存，SPVP=U，由 hstatus.PMM=3 选择屏蔽高16位，保留低48位（PMLEN=16）。观察有 S1
        // 翻译时按保留位的最高位符号扩展到64位，直通或仅 G 阶段时零扩展到64位。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求
        // vaddr，需由上游保证两者符合接口约定。
        cp_02_04_015 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 0
                && csr_virt_d1 == 0
                && req_hyperinst_d1 == 1
                && csr_spvp_d1 == 0
                && csr_hstatus_d1 == 3
                && csr_mxr_d1 == 0
                && csr_vmxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.016 PMM_M_PMLEN0
        // 测试表目标：构造有效访存特权为 M 的普通访存，由 mseccfg.PMM=0 选择不屏蔽指针高位（PMLEN=0）。观察 fullva
        // 不做指针屏蔽。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求 vaddr，需由上游保证两者符合接口约定。
        cp_02_04_016 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 3
                && csr_virt_d1 == 0
                && req_hyperinst_d1 == 0
                && csr_dmode_d1 == 3
                && csr_mseccfg_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.017 PMM_M_PMLEN7
        // 测试表目标：构造有效访存特权为 M 的普通访存，由 mseccfg.PMM=2 选择屏蔽高7位，保留低57位（PMLEN=7）。观察有 S1
        // 翻译时按保留位的最高位符号扩展到64位，直通或仅 G 阶段时零扩展到64位。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求
        // vaddr，需由上游保证两者符合接口约定。
        cp_02_04_017 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 3
                && csr_virt_d1 == 0
                && req_hyperinst_d1 == 0
                && csr_dmode_d1 == 3
                && csr_mseccfg_d1 == 2
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.018 PMM_M_PMLEN16
        // 测试表目标：构造有效访存特权为 M 的普通访存，由 mseccfg.PMM=3 选择屏蔽高16位，保留低48位（PMLEN=16）。观察有 S1
        // 翻译时按保留位的最高位符号扩展到64位，直通或仅 G 阶段时零扩展到64位。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求
        // vaddr，需由上游保证两者符合接口约定。
        cp_02_04_018 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 3
                && csr_virt_d1 == 0
                && req_hyperinst_d1 == 0
                && csr_dmode_d1 == 3
                && csr_mseccfg_d1 == 3
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.019 PMM_M_HYP_S_PMLEN0
        // 测试表目标：构造M 发起 Hyper 访存，SPVP=S，由 henvcfg.PMM=0 选择不屏蔽指针高位（PMLEN=0）。观察 fullva
        // 不做指针屏蔽。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求 vaddr，需由上游保证两者符合接口约定。
        cp_02_04_019 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 3
                && csr_virt_d1 == 0
                && req_hyperinst_d1 == 1
                && csr_spvp_d1 == 1
                && csr_henvcfg_d1 == 0
                && csr_mxr_d1 == 0
                && csr_vmxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.020 PMM_M_HYP_S_PMLEN7
        // 测试表目标：构造M 发起 Hyper 访存，SPVP=S，由 henvcfg.PMM=2 选择屏蔽高7位，保留低57位（PMLEN=7）。观察有 S1
        // 翻译时按保留位的最高位符号扩展到64位，直通或仅 G 阶段时零扩展到64位。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求
        // vaddr，需由上游保证两者符合接口约定。
        cp_02_04_020 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 3
                && csr_virt_d1 == 0
                && req_hyperinst_d1 == 1
                && csr_spvp_d1 == 1
                && csr_henvcfg_d1 == 2
                && csr_mxr_d1 == 0
                && csr_vmxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.021 PMM_M_HYP_S_PMLEN16
        // 测试表目标：构造M 发起 Hyper 访存，SPVP=S，由 henvcfg.PMM=3 选择屏蔽高16位，保留低48位（PMLEN=16）。观察有 S1
        // 翻译时按保留位的最高位符号扩展到64位，直通或仅 G 阶段时零扩展到64位。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求
        // vaddr，需由上游保证两者符合接口约定。
        cp_02_04_021 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 3
                && csr_virt_d1 == 0
                && req_hyperinst_d1 == 1
                && csr_spvp_d1 == 1
                && csr_henvcfg_d1 == 3
                && csr_mxr_d1 == 0
                && csr_vmxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.022 PMM_M_HYP_U_PMLEN0
        // 测试表目标：构造M 发起 Hyper 访存，SPVP=U，由 senvcfg.PMM=0 选择不屏蔽指针高位（PMLEN=0）。观察 fullva
        // 不做指针屏蔽。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求 vaddr，需由上游保证两者符合接口约定。
        cp_02_04_022 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 3
                && csr_virt_d1 == 0
                && req_hyperinst_d1 == 1
                && csr_spvp_d1 == 0
                && csr_senvcfg_d1 == 0
                && csr_mxr_d1 == 0
                && csr_vmxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.023 PMM_M_HYP_U_PMLEN7
        // 测试表目标：构造M 发起 Hyper 访存，SPVP=U，由 senvcfg.PMM=2 选择屏蔽高7位，保留低57位（PMLEN=7）。观察有 S1
        // 翻译时按保留位的最高位符号扩展到64位，直通或仅 G 阶段时零扩展到64位。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求
        // vaddr，需由上游保证两者符合接口约定。
        cp_02_04_023 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 3
                && csr_virt_d1 == 0
                && req_hyperinst_d1 == 1
                && csr_spvp_d1 == 0
                && csr_senvcfg_d1 == 2
                && csr_mxr_d1 == 0
                && csr_vmxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.024 PMM_M_HYP_U_PMLEN16
        // 测试表目标：构造M 发起 Hyper 访存，SPVP=U，由 senvcfg.PMM=3 选择屏蔽高16位，保留低48位（PMLEN=16）。观察有 S1
        // 翻译时按保留位的最高位符号扩展到64位，直通或仅 G 阶段时零扩展到64位。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求
        // vaddr，需由上游保证两者符合接口约定。
        cp_02_04_024 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 3
                && csr_virt_d1 == 0
                && req_hyperinst_d1 == 1
                && csr_spvp_d1 == 0
                && csr_senvcfg_d1 == 3
                && csr_mxr_d1 == 0
                && csr_vmxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.025 PMM_M_MPRV_S_PMLEN0
        // 测试表目标：构造指令运行在 M，但 dmode=S 的有效 S 特权访存，由 menvcfg.PMM=0 选择不屏蔽指针高位（PMLEN=0）。观察 fullva
        // 不做指针屏蔽。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求 vaddr，需由上游保证两者符合接口约定。
        cp_02_04_025 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 3
                && csr_virt_d1 == 0
                && req_hyperinst_d1 == 0
                && csr_dmode_d1 == 1
                && csr_menvcfg_d1 == 0
                && csr_mxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.026 PMM_M_MPRV_S_PMLEN7
        // 测试表目标：构造指令运行在 M，但 dmode=S 的有效 S 特权访存，由 menvcfg.PMM=2 选择屏蔽高7位，保留低57位（PMLEN=7）。观察有 S1
        // 翻译时按保留位的最高位符号扩展到64位，直通或仅 G 阶段时零扩展到64位。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求
        // vaddr，需由上游保证两者符合接口约定。
        cp_02_04_026 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 3
                && csr_virt_d1 == 0
                && req_hyperinst_d1 == 0
                && csr_dmode_d1 == 1
                && csr_menvcfg_d1 == 2
                && csr_mxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.027 PMM_M_MPRV_S_PMLEN16
        // 测试表目标：构造指令运行在 M，但 dmode=S 的有效 S 特权访存，由 menvcfg.PMM=3 选择屏蔽高16位，保留低48位（PMLEN=16）。观察有 S1
        // 翻译时按保留位的最高位符号扩展到64位，直通或仅 G 阶段时零扩展到64位。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求
        // vaddr，需由上游保证两者符合接口约定。
        cp_02_04_027 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 3
                && csr_virt_d1 == 0
                && req_hyperinst_d1 == 0
                && csr_dmode_d1 == 1
                && csr_menvcfg_d1 == 3
                && csr_mxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.028 PMM_M_MPRV_U_PMLEN0
        // 测试表目标：构造指令运行在 M，但 dmode=U 的有效 U 特权访存，由 senvcfg.PMM=0 选择不屏蔽指针高位（PMLEN=0）。观察 fullva
        // 不做指针屏蔽。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求 vaddr，需由上游保证两者符合接口约定。
        cp_02_04_028 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 3
                && csr_virt_d1 == 0
                && req_hyperinst_d1 == 0
                && csr_dmode_d1 == 0
                && csr_senvcfg_d1 == 0
                && csr_mxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.029 PMM_M_MPRV_U_PMLEN7
        // 测试表目标：构造指令运行在 M，但 dmode=U 的有效 U 特权访存，由 senvcfg.PMM=2 选择屏蔽高7位，保留低57位（PMLEN=7）。观察有 S1
        // 翻译时按保留位的最高位符号扩展到64位，直通或仅 G 阶段时零扩展到64位。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求
        // vaddr，需由上游保证两者符合接口约定。
        cp_02_04_029 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 3
                && csr_virt_d1 == 0
                && req_hyperinst_d1 == 0
                && csr_dmode_d1 == 0
                && csr_senvcfg_d1 == 2
                && csr_mxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.030 PMM_M_MPRV_U_PMLEN16
        // 测试表目标：构造指令运行在 M，但 dmode=U 的有效 U 特权访存，由 senvcfg.PMM=3 选择屏蔽高16位，保留低48位（PMLEN=16）。观察有 S1
        // 翻译时按保留位的最高位符号扩展到64位，直通或仅 G 阶段时零扩展到64位。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求
        // vaddr，需由上游保证两者符合接口约定。
        cp_02_04_030 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 3
                && csr_virt_d1 == 0
                && req_hyperinst_d1 == 0
                && csr_dmode_d1 == 0
                && csr_senvcfg_d1 == 3
                && csr_mxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.031 PMM_U_PMLEN0
        // 测试表目标：构造Host U 普通访存，由 senvcfg.PMM=0 选择不屏蔽指针高位（PMLEN=0）。观察 fullva
        // 不做指针屏蔽。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求 vaddr，需由上游保证两者符合接口约定。
        cp_02_04_031 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 0
                && csr_virt_d1 == 0
                && req_hyperinst_d1 == 0
                && csr_dmode_d1 == 0
                && csr_senvcfg_d1 == 0
                && csr_mxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.032 PMM_U_PMLEN7
        // 测试表目标：构造Host U 普通访存，由 senvcfg.PMM=2 选择屏蔽高7位，保留低57位（PMLEN=7）。观察有 S1
        // 翻译时按保留位的最高位符号扩展到64位，直通或仅 G 阶段时零扩展到64位。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求
        // vaddr，需由上游保证两者符合接口约定。
        cp_02_04_032 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 0
                && csr_virt_d1 == 0
                && req_hyperinst_d1 == 0
                && csr_dmode_d1 == 0
                && csr_senvcfg_d1 == 2
                && csr_mxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.033 PMM_U_PMLEN16
        // 测试表目标：构造Host U 普通访存，由 senvcfg.PMM=3 选择屏蔽高16位，保留低48位（PMLEN=16）。观察有 S1
        // 翻译时按保留位的最高位符号扩展到64位，直通或仅 G 阶段时零扩展到64位。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求
        // vaddr，需由上游保证两者符合接口约定。
        cp_02_04_033 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 0
                && csr_virt_d1 == 0
                && req_hyperinst_d1 == 0
                && csr_dmode_d1 == 0
                && csr_senvcfg_d1 == 3
                && csr_mxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.034 PMM_VS_PMLEN0
        // 测试表目标：构造Guest VS 访存，由 henvcfg.PMM=0 选择不屏蔽指针高位（PMLEN=0）。观察 fullva
        // 不做指针屏蔽。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求 vaddr，需由上游保证两者符合接口约定。
        cp_02_04_034 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 1
                && csr_virt_d1 == 1
                && req_hyperinst_d1 == 0
                && csr_dmode_d1 == 1
                && csr_henvcfg_d1 == 0
                && csr_mxr_d1 == 0
                && csr_vmxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.035 PMM_VS_PMLEN7
        // 测试表目标：构造Guest VS 访存，由 henvcfg.PMM=2 选择屏蔽高7位，保留低57位（PMLEN=7）。观察有 S1
        // 翻译时按保留位的最高位符号扩展到64位，直通或仅 G 阶段时零扩展到64位。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求
        // vaddr，需由上游保证两者符合接口约定。
        cp_02_04_035 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 1
                && csr_virt_d1 == 1
                && req_hyperinst_d1 == 0
                && csr_dmode_d1 == 1
                && csr_henvcfg_d1 == 2
                && csr_mxr_d1 == 0
                && csr_vmxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.036 PMM_VS_PMLEN16
        // 测试表目标：构造Guest VS 访存，由 henvcfg.PMM=3 选择屏蔽高16位，保留低48位（PMLEN=16）。观察有 S1
        // 翻译时按保留位的最高位符号扩展到64位，直通或仅 G 阶段时零扩展到64位。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求
        // vaddr，需由上游保证两者符合接口约定。
        cp_02_04_036 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 1
                && csr_virt_d1 == 1
                && req_hyperinst_d1 == 0
                && csr_dmode_d1 == 1
                && csr_henvcfg_d1 == 3
                && csr_mxr_d1 == 0
                && csr_vmxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.037 PMM_VU_PMLEN0
        // 测试表目标：构造Guest VU 访存，由 senvcfg.PMM=0 选择不屏蔽指针高位（PMLEN=0）。观察 fullva
        // 不做指针屏蔽。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求 vaddr，需由上游保证两者符合接口约定。
        cp_02_04_037 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 0
                && csr_virt_d1 == 1
                && req_hyperinst_d1 == 0
                && csr_dmode_d1 == 0
                && csr_senvcfg_d1 == 0
                && csr_mxr_d1 == 0
                && csr_vmxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.038 PMM_VU_PMLEN7
        // 测试表目标：构造Guest VU 访存，由 senvcfg.PMM=2 选择屏蔽高7位，保留低57位（PMLEN=7）。观察有 S1
        // 翻译时按保留位的最高位符号扩展到64位，直通或仅 G 阶段时零扩展到64位。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求
        // vaddr，需由上游保证两者符合接口约定。
        cp_02_04_038 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 0
                && csr_virt_d1 == 1
                && req_hyperinst_d1 == 0
                && csr_dmode_d1 == 0
                && csr_senvcfg_d1 == 2
                && csr_mxr_d1 == 0
                && csr_vmxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.039 PMM_VU_PMLEN16
        // 测试表目标：构造Guest VU 访存，由 senvcfg.PMM=3 选择屏蔽高16位，保留低48位（PMLEN=16）。观察有 S1
        // 翻译时按保留位的最高位符号扩展到64位，直通或仅 G 阶段时零扩展到64位。该有效地址供完整地址合法性预检查使用；lookup 仍使用请求
        // vaddr，需由上游保证两者符合接口约定。
        cp_02_04_039 : coverpoint pmm_address_class
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1 == 0
                && csr_imode_d1 == 0
                && csr_virt_d1 == 1
                && req_hyperinst_d1 == 0
                && csr_dmode_d1 == 0
                && csr_senvcfg_d1 == 3
                && csr_mxr_d1 == 0
                && csr_vmxr_d1 == 0
                && HAS_FULLVA
                && (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
    endgroup
    cg_dtlb_02_04 cg_dtlb_02_04_inst;

    // 02.05 PMM优先级与地址协同
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_02_05】02.05 PMM优先级与地址协同
    // 探测功能：观察 HLVX、MXR 和 M 模式之间的 PMM 优先级，并核对预检查地址与查表地址的关系。
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：02.05.001, 02.05.002, 02.05.003, 02.05.005；逐点功能见下方注释，精确配值见分解表。
    // raw_pmm：按请求拍有效特权及Host/Guest/Hyper上下文选择mseccfg/menvcfg/henvcfg/hstatus/senvcfg。
    covergroup cg_dtlb_02_05 @(posedge clock);
        option.per_instance = 1;
        // 02.05.001 PMM_HLVX关闭屏蔽
        // 测试表目标：场景：hlvx=1；所选PMM=2或3。 观察功能：EffectiveVa等于原fullva；仍执行合法性检查。 测试目的：观察 HLVX、MXR 和 M
        // 模式之间的 PMM 优先级，并核对预检查地址与查表地址的关系。
        cp_02_05_001 : coverpoint (req_effective_va_d1 == req_fullva_d1)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_hlvx_d1
                && raw_pmm inside {2, 3}) {
            bins b_expected_result = {1'b1};
        }
        // 02.05.002 PMM_MXR关闭屏蔽
        // 测试表目标：场景：premode!=M，有效MXR=1；所选PMM=2或3。 观察功能：屏蔽关闭；guest有效MXR=mxr|vmxr。 测试目的：观察 HLVX、MXR 和
        // M 模式之间的 PMM 优先级，并核对预检查地址与查表地址的关系。
        cp_02_05_002 : coverpoint (req_effective_va_d1 == req_fullva_d1)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && csr_dmode_d1 != 3
                && (csr_mxr_d1 || ((csr_virt_d1 || req_hyperinst_d1) && csr_vmxr_d1))
                && raw_pmm inside {2, 3}) {
            bins b_expected_result = {1'b1};
        }
        // 02.05.003 PMM_M优先于MXR
        // 测试表目标：场景：premode=M,mxr=1,mseccfg.PMM=2/3。 观察功能：M分支先于MXR，仍按mseccfg屏蔽。 测试目的：观察 HLVX、MXR 和
        // M 模式之间的 PMM 优先级，并核对预检查地址与查表地址的关系。
        cp_02_05_003 : coverpoint ((!HAS_FULLVA || resp_fullva == req_effective_va_d1))
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && !req_hlvx_d1
                && csr_dmode_d1 == 3
                && csr_mxr_d1
                && csr_mseccfg_d1 inside {2, 3}) {
            bins b_expected_result = {1'b1};
        }
        // 02.05.005 PMM_vaddr一致性
        // 测试表目标：场景：fullva经PMM改变且REQ valid。 观察功能：EffectiveVa用于fullva/预检查；查TLB仍使用R.vaddr；核对上游地址生成合同。
        // 测试目的：观察 HLVX、MXR 和 M 模式之间的 PMM 优先级，并核对预检查地址与查表地址的关系。
        cp_02_05_005 : coverpoint ((req_out_vaddr == req_vaddr_d1 && (!HAS_FULLVA || resp_fullva ==
            req_effective_va_d1)))
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_effective_va_d1 != req_fullva_d1) {
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_02_05 cg_dtlb_02_05_inst;

    // 02.05 PMM优先级与地址协同
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_diag_02_05】02.05 PMM优先级与地址协同
    // 探测功能：观察 HLVX、MXR 和 M 模式之间的 PMM 优先级，并核对预检查地址与查表地址的关系。
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：本组weight=0，cp为观察结果，rs的1/2分别表示偏离/未知；reference_missing表示信号缺失。
    // 测试点：02.05.004；逐点功能见下方注释，精确配值见分解表。
    covergroup cg_dtlb_diag_02_05 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        // 02.05.004：结果偏离或未知单列，正常命中不进入诊断仓。
        rs_02_05_004 : coverpoint ($isunknown((req_effective_va_d1 == req_fullva_d1)) ? 2'd2 : ((req_effective_va_d1
            == req_fullva_d1)) ? 2'd0 : 2'd1)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_pmm_d1 == 1) {
            bins b_mismatch = {2'd1};
            bins unknown  = {2'd2};
        }
        // 02.05.004 PMM_reserved01
        // 测试表目标：场景：所选pmm=1。 观察功能：RTL走未屏蔽分支；架构合法CSR激励通常不可达。 测试目的：观察 HLVX、MXR 和 M 模式之间的 PMM
        // 优先级，并核对预检查地址与查表地址的关系。
        cp_02_05_004 : coverpoint (req_effective_va_d1 == req_fullva_d1)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && req_pmm_d1 == 1) {
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_diag_02_05 cg_dtlb_diag_02_05_inst;

    // 03.01 有效项与Tag命中
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_03_01】03.01 有效项与Tag命中
    // 探测功能：观察有效位与页号匹配对缓存命中的作用，区分无有效项、标签不符和真实命中。
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：03.01.005；逐点功能见下方注释，精确配值见分解表。
    // expected_paddr：参考PA：翻译时拼接resp_ppn与请求页内偏移；直通按no_translate选择PMP地址或请求VA。
    covergroup cg_dtlb_03_01 @(posedge clock);
        option.per_instance = 1;
        // 03.01.005 LOOKUP_VA别名同PA
        // 测试表目标：场景：不同VPN映射同PPN，权限/上下文可相同或不同。 观察功能：分别按VA查表并保留各自permission；不因物理PPN相同合并有效entry。
        // 测试目的：观察有效位与页号匹配对缓存命中的作用，区分无有效项、标签不符和真实命中。
        cp_03_01_005 : coverpoint (resp_paddr_0 == expected_paddr)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && last_response_valid
                && last_vaddr[49:12] != req_vaddr_d1[49:12]
                && last_paddr[47:12] == resp_paddr_0[47:12]
                && !resp_miss) {
            bins b_expected_set = {1'b1};
        }
    endgroup
    cg_dtlb_03_01 cg_dtlb_03_01_inst;

    // 03.03 基础页与超级页地址拼接
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_03_03】03.03 基础页与超级页地址拼接
    // 探测功能：观察不同页尺寸下物理页号和页内偏移的拼接，覆盖边界地址与输出位宽截取。
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：03.03.001, 03.03.002, 03.03.003, 03.03.004；逐点功能见下方注释，精确配值见分解表。
    // 来源仓：b_storage=ptw_bypass_hit为0，b_bypass=1；具体仓是否要求storage_hit由各自iff显式限定。
    // expected_paddr：参考PA：翻译时拼接resp_ppn与请求页内偏移；直通按no_translate选择PMP地址或请求VA。
    // selected_level：按实际hit向量聚合页级；多hit时按位OR，不能当作唯一选中项。
    // selected_napot：按实际hit向量聚合NAPOT位。
    covergroup cg_dtlb_03_03 @(posedge clock);
        option.per_instance = 1;
        // 03.03.001 PAGE_4KB
        // 测试表目标：场景：W.valid，返回level=0,n=0；随后访问该页。 观察功能：entry.level=0；paddr高位来自PPN，低12位按页映射保留VA对应部分。
        // 测试目的：观察不同页尺寸下物理页号和页内偏移的拼接，覆盖边界地址与输出位宽截取。
        cp_03_03_001 : coverpoint (req_vaddr_d1[11:0])
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && !resp_miss
                && selected_level == 0
                && !selected_napot
                && ((resp_paddr_0 == expected_paddr)) === 1'b1) {
            bins b_head = {0};
            bins b_tail = {12'hfff};
            bins b_middle = {[12'd1:12'hffe]};
        }
        // 03.03.002 PAGE_2MB
        // 测试表目标：场景：W.valid，返回level=1,n=0；随后访问该页。 观察功能：entry.level=1；paddr高位来自PPN，低21位按页映射保留VA对应部分。
        // 测试目的：观察不同页尺寸下物理页号和页内偏移的拼接，覆盖边界地址与输出位宽截取。
        cp_03_03_002 : coverpoint (req_vaddr_d1[20:0])
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && !resp_miss
                && selected_level == 1
                && !selected_napot
                && ((resp_paddr_0 == expected_paddr)) === 1'b1) {
            bins b_head = {0};
            bins b_tail = {21'h1fffff};
            bins b_middle = {[21'd1:21'h1ffffe]};
        }
        // 03.03.003 PAGE_1GB
        // 测试表目标：场景：W.valid，返回level=2,n=0；随后访问该页。 观察功能：entry.level=2；paddr高位来自PPN，低30位按页映射保留VA对应部分。
        // 测试目的：观察不同页尺寸下物理页号和页内偏移的拼接，覆盖边界地址与输出位宽截取。
        cp_03_03_003 : coverpoint (req_vaddr_d1[29:0])
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && !resp_miss
                && selected_level == 2
                && !selected_napot
                && ((resp_paddr_0 == expected_paddr)) === 1'b1) {
            bins b_head = {0};
            bins b_tail = {30'h3fffffff};
            bins b_middle = {[30'd1:30'h3ffffffe]};
        }
        // 03.03.004 PAGE_512GB
        // 测试表目标：场景：W.valid，返回level=3,n=0；随后访问该页。 观察功能：entry.level=3；paddr高位来自PPN，低39位按页映射保留VA对应部分。
        // 测试目的：观察不同页尺寸下物理页号和页内偏移的拼接，覆盖边界地址与输出位宽截取。
        cp_03_03_004 : coverpoint (req_vaddr_d1[38:0])
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && !resp_miss
                && selected_level == 3
                && !selected_napot
                && ((resp_paddr_0 == expected_paddr)) === 1'b1) {
            bins b_head = {0};
            bins b_tail = {39'h7fffffffff};
            bins b_middle = {[39'd1:39'h7ffffffffe]};
        }
    endgroup
    cg_dtlb_03_03 cg_dtlb_03_03_inst;

    // 03.07 跨页访问与结果分类
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_03_07】03.07 跨页访问与结果分类
    // 探测功能：将跨页访问拆分后的两部分分别关联到翻译结果，覆盖页边界两侧映射或权限不同的情况。
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：03.07.001, 03.07.002；逐点功能见下方注释，精确配值见分解表。
    // 异常位序：resp_excp[5:0]={af_st,af_ld,gpf_st,gpf_ld,pf_st,pf_ld}；actual_fault==expected_fault核对类别，读写位掩码额外检查命令方向。
    // actual_fault：从实际输出resp_excp聚合[2:0]={AF,GPF,PF}，各类分别对load/store位做OR。
    // expected_fault：参考异常向量[2:0]={AF,GPF,PF}：预异常优先；无hit为0；有hit时AF优先于PF、PF优先于GPF。
    // expected_paddr：参考PA：翻译时拼接resp_ppn与请求页内偏移；直通按no_translate选择PMP地址或请求VA。
    covergroup cg_dtlb_03_07 @(posedge clock);
        option.per_instance = 1;
        // 03.07.001 CROSS_相邻页权限不同
        // 测试表目标：场景：跨页拆分请求，两页权限/PPN/PBMT不同。 观察功能：分别翻译；故障地址和上游合并行为正确。
        // 测试目的：将跨页访问拆分后的两部分分别关联到翻译结果，覆盖页边界两侧映射或权限不同的情况。
        cp_03_07_001 : coverpoint (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd ==
            3'd4) ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
            6'b010101) == 0 : 1'b1))
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && last_response_valid
                && last_rob == req_rob_d1
                && last_fullva == req_fullva_d1
                && last_vaddr[49:12] + 1 == req_vaddr_d1[49:12]) {
            bins b_expected_set = {1'b1};
        }
        // 03.07.002 FCOV_地址映射结果
        // 测试表目标：场景：O.valid&&!O.miss且无fault；存在完整参考映射快照。
        // 观察功能：monitor纯函数按快照重构期望PA并分类；未匹配/参考缺失样本也sample。仅统计类别，不调用报错接口或增加独立校验组件。
        // 测试目的：将跨页访问拆分后的两部分分别关联到翻译结果，覆盖页边界两侧映射或权限不同的情况。
        cp_03_07_002 : coverpoint (resp_paddr_0 == expected_paddr)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && !resp_miss
                && actual_fault == 0) {
            bins b_expected_set = {1'b1};
        }
    endgroup
    cg_dtlb_03_07 cg_dtlb_03_07_inst;

    // 04.01 读写权限与A/D
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_04_01】04.01 读写权限与A/D
    // 探测功能：在其他权限原因隔离后观察目标 R/W/A/D 位的作用，并分别覆盖普通和原子访存命令。
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：04.01.001, 04.01.002, 04.01.003, 04.01.004, 04.01.005, 04.01.006, 04.01.007,
    // 04.01.008, 04.01.009, 04.01.010, 04.01.011, 04.01.012, 04.01.013, 04.01.014, 04.01.015,
    // 04.01.016, 04.01.017, 04.01.018, 04.01.019, 04.01.020, 04.01.021, 04.01.022, 04.01.023,
    // 04.01.024, 04.01.025, 04.01.026, 04.01.027, 04.01.028, 04.01.029, 04.01.030, 04.01.031,
    // 04.01.032, 04.01.033, 04.01.034, 04.01.035, 04.01.036, 04.01.037, 04.01.038, 04.01.039,
    // 04.01.040, 04.01.041, 04.01.042, 04.01.043, 04.01.044；逐点功能见下方注释，精确配值见分解表。
    // 异常位序：resp_excp[5:0]={af_st,af_ld,gpf_st,gpf_ld,pf_st,pf_ld}；actual_fault==expected_fault核对类别，读写位掩码额外检查命令方向。
    // 来源仓：b_storage=ptw_bypass_hit为0，b_bypass=1；具体仓是否要求storage_hit由各自iff显式限定。
    // access_fault：仅检查本次启用的翻译阶段：S1.af 或 S2.af；不启用的阶段不产生AF。
    // actual_fault：从实际输出resp_excp聚合[2:0]={AF,GPF,PF}，各类分别对load/store位做OR。
    // expected_fault：参考异常向量[2:0]={AF,GPF,PF}：预异常优先；无hit为0；有hit时AF优先于PF、PF优先于GPF。
    // fake_pte：S1的V/PF/AF均为0且不是onlyS2；用于区分假PTE分支。
    // mode_permission：按本拍mode、S1.U与有效SUM判断U/S访问许可；Guest/Hyper选择vsum。
    // nonleaf_pte：S1的V为1、R/W/X/PF/AF均为0；表示非叶PTE。
    // pre_fault：请求拍 checkfullva 有效，且预检查 PF/GPF/AF 中至少一种成立；均使用_d1快照。
    // s1_page_fault：S1页故障参考值：启用S1、排除fake/nonleaf后，检查命令、特权、R/W/X/A/D和上游PF。
    covergroup cg_dtlb_04_01 @(posedge clock);
        option.per_instance = 1;
        // 04.01.001 S1_read_allow_cmd0
        // 测试表目标：以普通数据读取（load）命中已有表项或 PTW 旁路结果，在S/VS 第一阶段参与翻译且其他优先异常被隔离的条件下，页可读且
        // A=1，观察本阶段不因读权限产生页异常。本点只判定该阶段的权限作用；未被隔离的另一阶段仍按其规则检查。
        cp_04_01_001 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate != 2'd2
                && !fake_pte
                && !nonleaf_pte
                && mode_permission
                && req_out_cmd == 0
                && !req_hlvx_d1
                && resp_s1_r
                && resp_s1_a
                && !resp_s1_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.002 S1_read_allow_cmd4
        // 测试表目标：以LR 原子保留加载（读类）命中已有表项或 PTW 旁路结果，在S/VS 第一阶段参与翻译且其他优先异常被隔离的条件下，页可读且
        // A=1，观察本阶段不因读权限产生页异常。本点只判定该阶段的权限作用；未被隔离的另一阶段仍按其规则检查。
        cp_04_01_002 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate != 2'd2
                && !fake_pte
                && !nonleaf_pte
                && mode_permission
                && req_out_cmd == 4
                && !req_hlvx_d1
                && resp_s1_r
                && resp_s1_a
                && !resp_s1_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.003 S1_read_deny_cmd0
        // 测试表目标：以普通数据读取（load）命中已有表项或 PTW 旁路结果，在S/VS 第一阶段参与翻译且其他优先异常被隔离的条件下，页仅可执行（R=0、X=1），且本阶段有效
        // MXR=0，观察读权限拒绝产生 PF。
        cp_04_01_003 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate != 2'd2
                && !fake_pte
                && !nonleaf_pte
                && mode_permission
                && req_out_cmd == 0
                && !req_hlvx_d1
                && !resp_s1_r
                && resp_s1_x
                && resp_s1_a
                && (!(csr_mxr || ((resp_virt || req_hyperinst_d1) && csr_vmxr)))
                && !resp_s1_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.004 S1_read_deny_cmd4
        // 测试表目标：以LR 原子保留加载（读类）命中已有表项或 PTW 旁路结果，在S/VS 第一阶段参与翻译且其他优先异常被隔离的条件下，页仅可执行（R=0、X=1），且本阶段有效
        // MXR=0，观察读权限拒绝产生 PF。
        cp_04_01_004 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate != 2'd2
                && !fake_pte
                && !nonleaf_pte
                && mode_permission
                && req_out_cmd == 4
                && !req_hlvx_d1
                && !resp_s1_r
                && resp_s1_x
                && resp_s1_a
                && (!(csr_mxr || ((resp_virt || req_hyperinst_d1) && csr_vmxr)))
                && !resp_s1_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.005 S1_write_allow_cmd1
        // 测试表目标：以普通数据写入（store）命中已有表项或 PTW 旁路结果，在S/VS 第一阶段参与翻译且其他优先异常被隔离的条件下，页可写且 A=D=1，观察本阶段不因写权限或
        // A/D 产生页异常。本点只判定该阶段的权限作用；未被隔离的另一阶段仍按其规则检查。
        cp_04_01_005 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate != 2'd2
                && !fake_pte
                && !nonleaf_pte
                && mode_permission
                && req_out_cmd == 1
                && !req_hlvx_d1
                && resp_s1_w
                && resp_s1_a
                && resp_s1_d
                && !resp_s1_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.006 S1_write_allow_cmd5
        // 测试表目标：以SC/AMO 原子写类访问（写类）命中已有表项或 PTW 旁路结果，在S/VS 第一阶段参与翻译且其他优先异常被隔离的条件下，页可写且
        // A=D=1，观察本阶段不因写权限或 A/D 产生页异常。本点只判定该阶段的权限作用；未被隔离的另一阶段仍按其规则检查。
        cp_04_01_006 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate != 2'd2
                && !fake_pte
                && !nonleaf_pte
                && mode_permission
                && req_out_cmd == 5
                && !req_hlvx_d1
                && resp_s1_w
                && resp_s1_a
                && resp_s1_d
                && !resp_s1_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.007 S1_write_deny_cmd1
        // 测试表目标：以普通数据写入（store）命中已有表项或 PTW 旁路结果，在S/VS 第一阶段参与翻译且其他优先异常被隔离的条件下，页 W=0 而
        // A=D=1，隔离出“不可写”这一原因，观察 PF。
        cp_04_01_007 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate != 2'd2
                && !fake_pte
                && !nonleaf_pte
                && mode_permission
                && req_out_cmd == 1
                && !req_hlvx_d1
                && !resp_s1_w
                && resp_s1_a
                && resp_s1_d
                && !resp_s1_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.008 S1_write_deny_cmd5
        // 测试表目标：以SC/AMO 原子写类访问（写类）命中已有表项或 PTW 旁路结果，在S/VS 第一阶段参与翻译且其他优先异常被隔离的条件下，页 W=0 而
        // A=D=1，隔离出“不可写”这一原因，观察 PF。
        cp_04_01_008 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate != 2'd2
                && !fake_pte
                && !nonleaf_pte
                && mode_permission
                && req_out_cmd == 5
                && !req_hlvx_d1
                && !resp_s1_w
                && resp_s1_a
                && resp_s1_d
                && !resp_s1_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.009 S1_A0_read_cmd0
        // 测试表目标：以普通数据读取（load）命中已有表项或 PTW 旁路结果，在S/VS 第一阶段参与翻译且其他优先异常被隔离的条件下，页可读但 A=0，观察未访问位引起 PF。当前
        // DTLB 对 A/D 缺失走异常路径，不执行本地页表 A/D 更新。
        cp_04_01_009 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate != 2'd2
                && !fake_pte
                && !nonleaf_pte
                && mode_permission
                && req_out_cmd == 0
                && !req_hlvx_d1
                && resp_s1_r
                && !resp_s1_a
                && !resp_s1_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.010 S1_A0_read_cmd4
        // 测试表目标：以LR 原子保留加载（读类）命中已有表项或 PTW 旁路结果，在S/VS 第一阶段参与翻译且其他优先异常被隔离的条件下，页可读但 A=0，观察未访问位引起
        // PF。当前 DTLB 对 A/D 缺失走异常路径，不执行本地页表 A/D 更新。
        cp_04_01_010 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate != 2'd2
                && !fake_pte
                && !nonleaf_pte
                && mode_permission
                && req_out_cmd == 4
                && !req_hlvx_d1
                && resp_s1_r
                && !resp_s1_a
                && !resp_s1_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.011 S1_A0_write_cmd1
        // 测试表目标：以普通数据写入（store）命中已有表项或 PTW 旁路结果，在S/VS 第一阶段参与翻译且其他优先异常被隔离的条件下，页可写、D=1 但 A=0，观察未访问位引起
        // PF。当前 DTLB 对 A/D 缺失走异常路径，不执行本地页表 A/D 更新。
        cp_04_01_011 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate != 2'd2
                && !fake_pte
                && !nonleaf_pte
                && mode_permission
                && req_out_cmd == 1
                && !req_hlvx_d1
                && resp_s1_w
                && !resp_s1_a
                && resp_s1_d
                && !resp_s1_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.012 S1_A0_write_cmd5
        // 测试表目标：以SC/AMO 原子写类访问（写类）命中已有表项或 PTW 旁路结果，在S/VS 第一阶段参与翻译且其他优先异常被隔离的条件下，页可写、D=1 但
        // A=0，观察未访问位引起 PF。当前 DTLB 对 A/D 缺失走异常路径，不执行本地页表 A/D 更新。
        cp_04_01_012 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate != 2'd2
                && !fake_pte
                && !nonleaf_pte
                && mode_permission
                && req_out_cmd == 5
                && !req_hlvx_d1
                && resp_s1_w
                && !resp_s1_a
                && resp_s1_d
                && !resp_s1_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.013 S1_D0_write_cmd1
        // 测试表目标：以普通数据写入（store）命中已有表项或 PTW 旁路结果，在S/VS 第一阶段参与翻译且其他优先异常被隔离的条件下，页可写、A=1 但 D=0，观察未置脏位引起
        // PF。当前 DTLB 对 A/D 缺失走异常路径，不执行本地页表 A/D 更新。
        cp_04_01_013 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate != 2'd2
                && !fake_pte
                && !nonleaf_pte
                && mode_permission
                && req_out_cmd == 1
                && !req_hlvx_d1
                && resp_s1_w
                && resp_s1_a
                && !resp_s1_d
                && !resp_s1_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.014 S1_D0_write_cmd5
        // 测试表目标：以SC/AMO 原子写类访问（写类）命中已有表项或 PTW 旁路结果，在S/VS 第一阶段参与翻译且其他优先异常被隔离的条件下，页可写、A=1 但
        // D=0，观察未置脏位引起 PF。当前 DTLB 对 A/D 缺失走异常路径，不执行本地页表 A/D 更新。
        cp_04_01_014 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate != 2'd2
                && !fake_pte
                && !nonleaf_pte
                && mode_permission
                && req_out_cmd == 5
                && !req_hlvx_d1
                && resp_s1_w
                && resp_s1_a
                && !resp_s1_d
                && !resp_s1_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.015 S1_A0D0_write_cmd1
        // 测试表目标：以普通数据写入（store）命中已有表项或 PTW 旁路结果，在S/VS 第一阶段参与翻译且其他优先异常被隔离的条件下，页可写但 A=D=0，观察两种 A/D
        // 缺失同时存在时的 PF。当前 DTLB 对 A/D 缺失走异常路径，不执行本地页表 A/D 更新。
        cp_04_01_015 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate != 2'd2
                && !fake_pte
                && !nonleaf_pte
                && mode_permission
                && req_out_cmd == 1
                && !req_hlvx_d1
                && resp_s1_w
                && !resp_s1_a
                && !resp_s1_d
                && !resp_s1_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.016 S1_A0D0_write_cmd5
        // 测试表目标：以SC/AMO 原子写类访问（写类）命中已有表项或 PTW 旁路结果，在S/VS 第一阶段参与翻译且其他优先异常被隔离的条件下，页可写但 A=D=0，观察两种
        // A/D 缺失同时存在时的 PF。当前 DTLB 对 A/D 缺失走异常路径，不执行本地页表 A/D 更新。
        cp_04_01_016 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate != 2'd2
                && !fake_pte
                && !nonleaf_pte
                && mode_permission
                && req_out_cmd == 5
                && !req_hlvx_d1
                && resp_s1_w
                && !resp_s1_a
                && !resp_s1_d
                && !resp_s1_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.017 S1_D0_read无影响_cmd0
        // 测试表目标：以普通数据读取（load）命中已有表项或 PTW 旁路结果，在S/VS 第一阶段参与翻译且其他优先异常被隔离的条件下，页可读且
        // A=1、D=0，观察读访问无需置脏位，不因 D=0 产生页异常。本点只判定该阶段的权限作用；未被隔离的另一阶段仍按其规则检查。当前 DTLB 对 A/D
        // 缺失走异常路径，不执行本地页表 A/D 更新。
        cp_04_01_017 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate != 2'd2
                && !fake_pte
                && !nonleaf_pte
                && mode_permission
                && req_out_cmd == 0
                && !req_hlvx_d1
                && resp_s1_r
                && resp_s1_a
                && !resp_s1_d
                && !resp_s1_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.018 S1_D0_read无影响_cmd4
        // 测试表目标：以LR 原子保留加载（读类）命中已有表项或 PTW 旁路结果，在S/VS 第一阶段参与翻译且其他优先异常被隔离的条件下，页可读且
        // A=1、D=0，观察读访问无需置脏位，不因 D=0 产生页异常。本点只判定该阶段的权限作用；未被隔离的另一阶段仍按其规则检查。当前 DTLB 对 A/D
        // 缺失走异常路径，不执行本地页表 A/D 更新。
        cp_04_01_018 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate != 2'd2
                && !fake_pte
                && !nonleaf_pte
                && mode_permission
                && req_out_cmd == 4
                && !req_hlvx_d1
                && resp_s1_r
                && resp_s1_a
                && !resp_s1_d
                && !resp_s1_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.019 S1_上游fault标记_cmd0
        // 测试表目标：以普通数据读取（load）命中已有表项或 PTW 旁路结果，在S/VS 第一阶段参与翻译且其他优先异常被隔离的条件下，选中权限中的 pf=1 表示 PTW
        // 已报告该阶段页异常，观察 DTLB 将其传到 PF 输出。
        cp_04_01_019 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate != 2'd2
                && !fake_pte
                && !nonleaf_pte
                && mode_permission
                && req_out_cmd == 0
                && !req_hlvx_d1
                && resp_s1_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.020 S1_上游fault标记_cmd1
        // 测试表目标：以普通数据写入（store）命中已有表项或 PTW 旁路结果，在S/VS 第一阶段参与翻译且其他优先异常被隔离的条件下，选中权限中的 pf=1 表示 PTW
        // 已报告该阶段页异常，观察 DTLB 将其传到 PF 输出。
        cp_04_01_020 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate != 2'd2
                && !fake_pte
                && !nonleaf_pte
                && mode_permission
                && req_out_cmd == 1
                && !req_hlvx_d1
                && resp_s1_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.021 S1_上游fault标记_cmd4
        // 测试表目标：以LR 原子保留加载（读类）命中已有表项或 PTW 旁路结果，在S/VS 第一阶段参与翻译且其他优先异常被隔离的条件下，选中权限中的 pf=1 表示 PTW
        // 已报告该阶段页异常，观察 DTLB 将其传到 PF 输出。
        cp_04_01_021 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate != 2'd2
                && !fake_pte
                && !nonleaf_pte
                && mode_permission
                && req_out_cmd == 4
                && !req_hlvx_d1
                && resp_s1_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.022 S1_上游fault标记_cmd5
        // 测试表目标：以SC/AMO 原子写类访问（写类）命中已有表项或 PTW 旁路结果，在S/VS 第一阶段参与翻译且其他优先异常被隔离的条件下，选中权限中的 pf=1 表示 PTW
        // 已报告该阶段页异常，观察 DTLB 将其传到 PF 输出。
        cp_04_01_022 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate != 2'd2
                && !fake_pte
                && !nonleaf_pte
                && mode_permission
                && req_out_cmd == 5
                && !req_hlvx_d1
                && resp_s1_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.023 S2_read_allow_cmd0
        // 测试表目标：以普通数据读取（load）命中已有表项或 PTW 旁路结果，在G 第二阶段参与翻译且其他优先异常被隔离的条件下，页可读且
        // A=1，观察本阶段不因读权限产生页异常。本点只判定该阶段的权限作用；未被隔离的另一阶段仍按其规则检查。
        cp_04_01_023 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate >= 2'd2
                && !s1_page_fault
                && req_out_cmd == 0
                && !req_hlvx_d1
                && resp_s2_r
                && resp_s2_a
                && !resp_s2_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.024 S2_read_allow_cmd4
        // 测试表目标：以LR 原子保留加载（读类）命中已有表项或 PTW 旁路结果，在G 第二阶段参与翻译且其他优先异常被隔离的条件下，页可读且
        // A=1，观察本阶段不因读权限产生页异常。本点只判定该阶段的权限作用；未被隔离的另一阶段仍按其规则检查。
        cp_04_01_024 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate >= 2'd2
                && !s1_page_fault
                && req_out_cmd == 4
                && !req_hlvx_d1
                && resp_s2_r
                && resp_s2_a
                && !resp_s2_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.025 S2_read_deny_cmd0
        // 测试表目标：以普通数据读取（load）命中已有表项或 PTW 旁路结果，在G 第二阶段参与翻译且其他优先异常被隔离的条件下，页仅可执行（R=0、X=1），且本阶段有效
        // MXR=0，观察读权限拒绝产生 GPF。
        cp_04_01_025 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate >= 2'd2
                && !s1_page_fault
                && req_out_cmd == 0
                && !req_hlvx_d1
                && !resp_s2_r
                && resp_s2_x
                && resp_s2_a
                && !csr_mxr
                && !resp_s2_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.026 S2_read_deny_cmd4
        // 测试表目标：以LR 原子保留加载（读类）命中已有表项或 PTW 旁路结果，在G 第二阶段参与翻译且其他优先异常被隔离的条件下，页仅可执行（R=0、X=1），且本阶段有效
        // MXR=0，观察读权限拒绝产生 GPF。
        cp_04_01_026 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate >= 2'd2
                && !s1_page_fault
                && req_out_cmd == 4
                && !req_hlvx_d1
                && !resp_s2_r
                && resp_s2_x
                && resp_s2_a
                && !csr_mxr
                && !resp_s2_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.027 S2_write_allow_cmd1
        // 测试表目标：以普通数据写入（store）命中已有表项或 PTW 旁路结果，在G 第二阶段参与翻译且其他优先异常被隔离的条件下，页可写且 A=D=1，观察本阶段不因写权限或
        // A/D 产生页异常。本点只判定该阶段的权限作用；未被隔离的另一阶段仍按其规则检查。
        cp_04_01_027 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate >= 2'd2
                && !s1_page_fault
                && req_out_cmd == 1
                && !req_hlvx_d1
                && resp_s2_w
                && resp_s2_a
                && resp_s2_d
                && !resp_s2_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.028 S2_write_allow_cmd5
        // 测试表目标：以SC/AMO 原子写类访问（写类）命中已有表项或 PTW 旁路结果，在G 第二阶段参与翻译且其他优先异常被隔离的条件下，页可写且
        // A=D=1，观察本阶段不因写权限或 A/D 产生页异常。本点只判定该阶段的权限作用；未被隔离的另一阶段仍按其规则检查。
        cp_04_01_028 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate >= 2'd2
                && !s1_page_fault
                && req_out_cmd == 5
                && !req_hlvx_d1
                && resp_s2_w
                && resp_s2_a
                && resp_s2_d
                && !resp_s2_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.029 S2_write_deny_cmd1
        // 测试表目标：以普通数据写入（store）命中已有表项或 PTW 旁路结果，在G 第二阶段参与翻译且其他优先异常被隔离的条件下，页 W=0 而
        // A=D=1，隔离出“不可写”这一原因，观察 GPF。
        cp_04_01_029 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate >= 2'd2
                && !s1_page_fault
                && req_out_cmd == 1
                && !req_hlvx_d1
                && !resp_s2_w
                && resp_s2_a
                && resp_s2_d
                && !resp_s2_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.030 S2_write_deny_cmd5
        // 测试表目标：以SC/AMO 原子写类访问（写类）命中已有表项或 PTW 旁路结果，在G 第二阶段参与翻译且其他优先异常被隔离的条件下，页 W=0 而
        // A=D=1，隔离出“不可写”这一原因，观察 GPF。
        cp_04_01_030 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate >= 2'd2
                && !s1_page_fault
                && req_out_cmd == 5
                && !req_hlvx_d1
                && !resp_s2_w
                && resp_s2_a
                && resp_s2_d
                && !resp_s2_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.031 S2_A0_read_cmd0
        // 测试表目标：以普通数据读取（load）命中已有表项或 PTW 旁路结果，在G 第二阶段参与翻译且其他优先异常被隔离的条件下，页可读但 A=0，观察未访问位引起 GPF。当前
        // DTLB 对 A/D 缺失走异常路径，不执行本地页表 A/D 更新。
        cp_04_01_031 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate >= 2'd2
                && !s1_page_fault
                && req_out_cmd == 0
                && !req_hlvx_d1
                && resp_s2_r
                && !resp_s2_a
                && !resp_s2_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.032 S2_A0_read_cmd4
        // 测试表目标：以LR 原子保留加载（读类）命中已有表项或 PTW 旁路结果，在G 第二阶段参与翻译且其他优先异常被隔离的条件下，页可读但 A=0，观察未访问位引起 GPF。当前
        // DTLB 对 A/D 缺失走异常路径，不执行本地页表 A/D 更新。
        cp_04_01_032 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate >= 2'd2
                && !s1_page_fault
                && req_out_cmd == 4
                && !req_hlvx_d1
                && resp_s2_r
                && !resp_s2_a
                && !resp_s2_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.033 S2_A0_write_cmd1
        // 测试表目标：以普通数据写入（store）命中已有表项或 PTW 旁路结果，在G 第二阶段参与翻译且其他优先异常被隔离的条件下，页可写、D=1 但 A=0，观察未访问位引起
        // GPF。当前 DTLB 对 A/D 缺失走异常路径，不执行本地页表 A/D 更新。
        cp_04_01_033 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate >= 2'd2
                && !s1_page_fault
                && req_out_cmd == 1
                && !req_hlvx_d1
                && resp_s2_w
                && !resp_s2_a
                && resp_s2_d
                && !resp_s2_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.034 S2_A0_write_cmd5
        // 测试表目标：以SC/AMO 原子写类访问（写类）命中已有表项或 PTW 旁路结果，在G 第二阶段参与翻译且其他优先异常被隔离的条件下，页可写、D=1 但
        // A=0，观察未访问位引起 GPF。当前 DTLB 对 A/D 缺失走异常路径，不执行本地页表 A/D 更新。
        cp_04_01_034 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate >= 2'd2
                && !s1_page_fault
                && req_out_cmd == 5
                && !req_hlvx_d1
                && resp_s2_w
                && !resp_s2_a
                && resp_s2_d
                && !resp_s2_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.035 S2_D0_write_cmd1
        // 测试表目标：以普通数据写入（store）命中已有表项或 PTW 旁路结果，在G 第二阶段参与翻译且其他优先异常被隔离的条件下，页可写、A=1 但 D=0，观察未置脏位引起
        // GPF。当前 DTLB 对 A/D 缺失走异常路径，不执行本地页表 A/D 更新。
        cp_04_01_035 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate >= 2'd2
                && !s1_page_fault
                && req_out_cmd == 1
                && !req_hlvx_d1
                && resp_s2_w
                && resp_s2_a
                && !resp_s2_d
                && !resp_s2_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.036 S2_D0_write_cmd5
        // 测试表目标：以SC/AMO 原子写类访问（写类）命中已有表项或 PTW 旁路结果，在G 第二阶段参与翻译且其他优先异常被隔离的条件下，页可写、A=1 但
        // D=0，观察未置脏位引起 GPF。当前 DTLB 对 A/D 缺失走异常路径，不执行本地页表 A/D 更新。
        cp_04_01_036 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate >= 2'd2
                && !s1_page_fault
                && req_out_cmd == 5
                && !req_hlvx_d1
                && resp_s2_w
                && resp_s2_a
                && !resp_s2_d
                && !resp_s2_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.037 S2_A0D0_write_cmd1
        // 测试表目标：以普通数据写入（store）命中已有表项或 PTW 旁路结果，在G 第二阶段参与翻译且其他优先异常被隔离的条件下，页可写但 A=D=0，观察两种 A/D
        // 缺失同时存在时的 GPF。当前 DTLB 对 A/D 缺失走异常路径，不执行本地页表 A/D 更新。
        cp_04_01_037 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate >= 2'd2
                && !s1_page_fault
                && req_out_cmd == 1
                && !req_hlvx_d1
                && resp_s2_w
                && !resp_s2_a
                && !resp_s2_d
                && !resp_s2_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.038 S2_A0D0_write_cmd5
        // 测试表目标：以SC/AMO 原子写类访问（写类）命中已有表项或 PTW 旁路结果，在G 第二阶段参与翻译且其他优先异常被隔离的条件下，页可写但 A=D=0，观察两种 A/D
        // 缺失同时存在时的 GPF。当前 DTLB 对 A/D 缺失走异常路径，不执行本地页表 A/D 更新。
        cp_04_01_038 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate >= 2'd2
                && !s1_page_fault
                && req_out_cmd == 5
                && !req_hlvx_d1
                && resp_s2_w
                && !resp_s2_a
                && !resp_s2_d
                && !resp_s2_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.039 S2_D0_read无影响_cmd0
        // 测试表目标：以普通数据读取（load）命中已有表项或 PTW 旁路结果，在G 第二阶段参与翻译且其他优先异常被隔离的条件下，页可读且 A=1、D=0，观察读访问无需置脏位，不因
        // D=0 产生页异常。本点只判定该阶段的权限作用；未被隔离的另一阶段仍按其规则检查。当前 DTLB 对 A/D 缺失走异常路径，不执行本地页表 A/D 更新。
        cp_04_01_039 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate >= 2'd2
                && !s1_page_fault
                && req_out_cmd == 0
                && !req_hlvx_d1
                && resp_s2_r
                && resp_s2_a
                && !resp_s2_d
                && !resp_s2_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.040 S2_D0_read无影响_cmd4
        // 测试表目标：以LR 原子保留加载（读类）命中已有表项或 PTW 旁路结果，在G 第二阶段参与翻译且其他优先异常被隔离的条件下，页可读且
        // A=1、D=0，观察读访问无需置脏位，不因 D=0 产生页异常。本点只判定该阶段的权限作用；未被隔离的另一阶段仍按其规则检查。当前 DTLB 对 A/D
        // 缺失走异常路径，不执行本地页表 A/D 更新。
        cp_04_01_040 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate >= 2'd2
                && !s1_page_fault
                && req_out_cmd == 4
                && !req_hlvx_d1
                && resp_s2_r
                && resp_s2_a
                && !resp_s2_d
                && !resp_s2_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.041 S2_上游fault标记_cmd0
        // 测试表目标：以普通数据读取（load）命中已有表项或 PTW 旁路结果，在G 第二阶段参与翻译且其他优先异常被隔离的条件下，选中权限中的 pf=1 表示 PTW
        // 已报告该阶段页异常，观察 DTLB 将其传到 GPF 输出。
        cp_04_01_041 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate >= 2'd2
                && !s1_page_fault
                && req_out_cmd == 0
                && !req_hlvx_d1
                && resp_s2_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.042 S2_上游fault标记_cmd1
        // 测试表目标：以普通数据写入（store）命中已有表项或 PTW 旁路结果，在G 第二阶段参与翻译且其他优先异常被隔离的条件下，选中权限中的 pf=1 表示 PTW
        // 已报告该阶段页异常，观察 DTLB 将其传到 GPF 输出。
        cp_04_01_042 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate >= 2'd2
                && !s1_page_fault
                && req_out_cmd == 1
                && !req_hlvx_d1
                && resp_s2_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.043 S2_上游fault标记_cmd4
        // 测试表目标：以LR 原子保留加载（读类）命中已有表项或 PTW 旁路结果，在G 第二阶段参与翻译且其他优先异常被隔离的条件下，选中权限中的 pf=1 表示 PTW
        // 已报告该阶段页异常，观察 DTLB 将其传到 GPF 输出。
        cp_04_01_043 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate >= 2'd2
                && !s1_page_fault
                && req_out_cmd == 4
                && !req_hlvx_d1
                && resp_s2_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.044 S2_上游fault标记_cmd5
        // 测试表目标：以SC/AMO 原子写类访问（写类）命中已有表项或 PTW 旁路结果，在G 第二阶段参与翻译且其他优先异常被隔离的条件下，选中权限中的 pf=1 表示 PTW
        // 已报告该阶段页异常，观察 DTLB 将其传到 GPF 输出。
        cp_04_01_044 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && !access_fault
                && translate_enable
                && req_out_s2xlate >= 2'd2
                && !s1_page_fault
                && req_out_cmd == 5
                && !req_hlvx_d1
                && resp_s2_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
    endgroup
    cg_dtlb_04_01 cg_dtlb_04_01_inst;

    // 04.02 特权与动态权限控制
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_04_02】04.02 特权与动态权限控制
    // 探测功能：观察 U/S 页面权限、SUM、MXR 与 HLVX 执行权限检查之间的关系。
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：04.02.001, 04.02.002, 04.02.003, 04.02.004, 04.02.005, 04.02.006, 04.02.007,
    // 04.02.008, 04.02.009, 04.02.010, 04.02.011, 04.02.012, 04.02.013, 04.02.014, 04.02.015,
    // 04.02.016, 04.02.017, 04.02.018；逐点功能见下方注释，精确配值见分解表。
    // 异常位序：resp_excp[5:0]={af_st,af_ld,gpf_st,gpf_ld,pf_st,pf_ld}；actual_fault==expected_fault核对类别，读写位掩码额外检查命令方向。
    // 来源仓：b_storage=ptw_bypass_hit为0，b_bypass=1；具体仓是否要求storage_hit由各自iff显式限定。
    // access_fault：仅检查本次启用的翻译阶段：S1.af 或 S2.af；不启用的阶段不产生AF。
    // actual_fault：从实际输出resp_excp聚合[2:0]={AF,GPF,PF}，各类分别对load/store位做OR。
    // expected_fault：参考异常向量[2:0]={AF,GPF,PF}：预异常优先；无hit为0；有hit时AF优先于PF、PF优先于GPF。
    // fake_pte：S1的V/PF/AF均为0且不是onlyS2；用于区分假PTE分支。
    // mode_permission：按本拍mode、S1.U与有效SUM判断U/S访问许可；Guest/Hyper选择vsum。
    // nonleaf_pte：S1的V为1、R/W/X/PF/AF均为0；表示非叶PTE。
    // pre_fault：请求拍 checkfullva 有效，且预检查 PF/GPF/AF 中至少一种成立；均使用_d1快照。
    covergroup cg_dtlb_04_02 @(posedge clock);
        option.per_instance = 1;
        // 04.02.002 S1_PRIV_U访U_cmd0
        // 测试表目标：以普通数据读取（load）命中有效叶表项，构造U态访问 U=1 的用户页，允许通过特权检查。保持本命令所需 R/W/A/D
        // 位允许访问，并排除预异常和访问异常，从而单独观察特权检查对读/写页异常输出的影响。
        cp_04_02_002 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && translate_enable
                && req_out_s2xlate != 2'd2
                && !access_fault
                && !fake_pte
                && !nonleaf_pte
                && effective_mode == 0
                && resp_s1_u
                && req_out_cmd == 0
                && resp_s1_a
                && !resp_s1_pf
                && resp_s1_r
                && !req_hlvx_d1
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.02.003 S1_PRIV_U访U_cmd1
        // 测试表目标：以普通数据写入（store）命中有效叶表项，构造U态访问 U=1 的用户页，允许通过特权检查。保持本命令所需 R/W/A/D
        // 位允许访问，并排除预异常和访问异常，从而单独观察特权检查对读/写页异常输出的影响。
        cp_04_02_003 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && translate_enable
                && req_out_s2xlate != 2'd2
                && !access_fault
                && !fake_pte
                && !nonleaf_pte
                && effective_mode == 0
                && resp_s1_u
                && req_out_cmd == 1
                && resp_s1_a
                && !resp_s1_pf
                && resp_s1_w
                && resp_s1_d
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.02.004 S1_PRIV_U访S_cmd0
        // 测试表目标：以普通数据读取（load）命中有效叶表项，构造U态访问 U=0 的监管页，特权检查拒绝并产生 S1 PF。保持本命令所需 R/W/A/D
        // 位允许访问，并排除预异常和访问异常，从而单独观察特权检查对读/写页异常输出的影响。
        cp_04_02_004 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && translate_enable
                && req_out_s2xlate != 2'd2
                && !access_fault
                && !fake_pte
                && !nonleaf_pte
                && effective_mode == 0
                && !resp_s1_u
                && req_out_cmd == 0
                && resp_s1_a
                && !resp_s1_pf
                && resp_s1_r
                && !req_hlvx_d1
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.02.005 S1_PRIV_U访S_cmd1
        // 测试表目标：以普通数据写入（store）命中有效叶表项，构造U态访问 U=0 的监管页，特权检查拒绝并产生 S1 PF。保持本命令所需 R/W/A/D
        // 位允许访问，并排除预异常和访问异常，从而单独观察特权检查对读/写页异常输出的影响。
        cp_04_02_005 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && translate_enable
                && req_out_s2xlate != 2'd2
                && !access_fault
                && !fake_pte
                && !nonleaf_pte
                && effective_mode == 0
                && !resp_s1_u
                && req_out_cmd == 1
                && resp_s1_a
                && !resp_s1_pf
                && resp_s1_w
                && resp_s1_d
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.02.006 S1_PRIV_S访S_cmd0
        // 测试表目标：以普通数据读取（load）命中有效叶表项，构造S态访问 U=0 的监管页，允许通过特权检查。保持本命令所需 R/W/A/D
        // 位允许访问，并排除预异常和访问异常，从而单独观察特权检查对读/写页异常输出的影响。
        cp_04_02_006 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && translate_enable
                && req_out_s2xlate != 2'd2
                && !access_fault
                && !fake_pte
                && !nonleaf_pte
                && effective_mode == 1
                && !resp_s1_u
                && req_out_cmd == 0
                && resp_s1_a
                && !resp_s1_pf
                && resp_s1_r
                && !req_hlvx_d1
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.02.007 S1_PRIV_S访S_cmd1
        // 测试表目标：以普通数据写入（store）命中有效叶表项，构造S态访问 U=0 的监管页，允许通过特权检查。保持本命令所需 R/W/A/D
        // 位允许访问，并排除预异常和访问异常，从而单独观察特权检查对读/写页异常输出的影响。
        cp_04_02_007 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && translate_enable
                && req_out_s2xlate != 2'd2
                && !access_fault
                && !fake_pte
                && !nonleaf_pte
                && effective_mode == 1
                && !resp_s1_u
                && req_out_cmd == 1
                && resp_s1_a
                && !resp_s1_pf
                && resp_s1_w
                && resp_s1_d
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.02.008 S1_PRIV_S访U_SUM0_cmd0
        // 测试表目标：以普通数据读取（load）命中有效叶表项，构造S态访问 U=1 的用户页，SUM=0 禁止此访问并产生 S1 PF。保持本命令所需 R/W/A/D
        // 位允许访问，并排除预异常和访问异常，从而单独观察特权检查对读/写页异常输出的影响。
        cp_04_02_008 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && translate_enable
                && req_out_s2xlate != 2'd2
                && !access_fault
                && !fake_pte
                && !nonleaf_pte
                && effective_mode == 1
                && resp_s1_u
                && (!((resp_virt || req_hyperinst_d1) ? csr_vsum : csr_sum))
                && req_out_cmd == 0
                && resp_s1_a
                && !resp_s1_pf
                && resp_s1_r
                && !req_hlvx_d1
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.02.009 S1_PRIV_S访U_SUM0_cmd1
        // 测试表目标：以普通数据写入（store）命中有效叶表项，构造S态访问 U=1 的用户页，SUM=0 禁止此访问并产生 S1 PF。保持本命令所需 R/W/A/D
        // 位允许访问，并排除预异常和访问异常，从而单独观察特权检查对读/写页异常输出的影响。
        cp_04_02_009 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && translate_enable
                && req_out_s2xlate != 2'd2
                && !access_fault
                && !fake_pte
                && !nonleaf_pte
                && effective_mode == 1
                && resp_s1_u
                && (!((resp_virt || req_hyperinst_d1) ? csr_vsum : csr_sum))
                && req_out_cmd == 1
                && resp_s1_a
                && !resp_s1_pf
                && resp_s1_w
                && resp_s1_d
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.02.010 S1_PRIV_S访U_SUM1_cmd0
        // 测试表目标：以普通数据读取（load）命中有效叶表项，构造S态访问 U=1 的用户页，SUM=1 允许此数据访问通过特权检查。保持本命令所需 R/W/A/D
        // 位允许访问，并排除预异常和访问异常，从而单独观察特权检查对读/写页异常输出的影响。
        cp_04_02_010 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && translate_enable
                && req_out_s2xlate != 2'd2
                && !access_fault
                && !fake_pte
                && !nonleaf_pte
                && effective_mode == 1
                && resp_s1_u
                && ((resp_virt || req_hyperinst_d1) ? csr_vsum : csr_sum)
                && req_out_cmd == 0
                && resp_s1_a
                && !resp_s1_pf
                && resp_s1_r
                && !req_hlvx_d1
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.02.011 S1_PRIV_S访U_SUM1_cmd1
        // 测试表目标：以普通数据写入（store）命中有效叶表项，构造S态访问 U=1 的用户页，SUM=1 允许此数据访问通过特权检查。保持本命令所需 R/W/A/D
        // 位允许访问，并排除预异常和访问异常，从而单独观察特权检查对读/写页异常输出的影响。
        cp_04_02_011 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && translate_enable
                && req_out_s2xlate != 2'd2
                && !access_fault
                && !fake_pte
                && !nonleaf_pte
                && effective_mode == 1
                && resp_s1_u
                && ((resp_virt || req_hyperinst_d1) ? csr_vsum : csr_sum)
                && req_out_cmd == 1
                && resp_s1_a
                && !resp_s1_pf
                && resp_s1_w
                && resp_s1_d
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.02.012 MXR_guest_00
        // 测试表目标：构造 Guest/Hyper 的普通读类请求，S1、S2 页均为仅执行页（R=0、X=1），A=1且无已有页异常。设置 Host mxr=0、Guest
        // vmxr=0：S1有效MXR=0，G阶段有效MXR=0。S1拒绝从仅执行页读取。G阶段拒绝该读取。按实际stage只启用对应阶段；allStage下若S1先产生PF，最终GPF被其抑制。
        cp_04_02_012 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && (resp_virt || req_hyperinst_d1)
                && (req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                && !req_hlvx_d1
                && translate_enable
                && mode_permission
                && !access_fault
                && !fake_pte
                && !nonleaf_pte
                && csr_mxr == 0
                && csr_vmxr == 0
                && !resp_s1_r
                && resp_s1_x
                && resp_s1_a
                && !resp_s1_pf
                && !resp_s2_r
                && resp_s2_x
                && resp_s2_a
                && !resp_s2_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.02.013 MXR_guest_01
        // 测试表目标：构造 Guest/Hyper 的普通读类请求，S1、S2 页均为仅执行页（R=0、X=1），A=1且无已有页异常。设置 Host mxr=0、Guest
        // vmxr=1：S1有效MXR=1，G阶段有效MXR=0。S1允许将执行页作为可读页。G阶段拒绝该读取。按实际stage只启用对应阶段；allStage下若S1先产生PF，最终GPF被其抑制。
        cp_04_02_013 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && (resp_virt || req_hyperinst_d1)
                && (req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                && !req_hlvx_d1
                && translate_enable
                && mode_permission
                && !access_fault
                && !fake_pte
                && !nonleaf_pte
                && csr_mxr == 0
                && csr_vmxr == 1
                && !resp_s1_r
                && resp_s1_x
                && resp_s1_a
                && !resp_s1_pf
                && !resp_s2_r
                && resp_s2_x
                && resp_s2_a
                && !resp_s2_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.02.014 MXR_guest_10
        // 测试表目标：构造 Guest/Hyper 的普通读类请求，S1、S2 页均为仅执行页（R=0、X=1），A=1且无已有页异常。设置 Host mxr=1、Guest
        // vmxr=0：S1有效MXR=1，G阶段有效MXR=1。S1允许将执行页作为可读页。G阶段允许该读取。按实际stage只启用对应阶段；allStage下若S1先产生PF，最终GPF被其抑制。
        cp_04_02_014 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && (resp_virt || req_hyperinst_d1)
                && (req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                && !req_hlvx_d1
                && translate_enable
                && mode_permission
                && !access_fault
                && !fake_pte
                && !nonleaf_pte
                && csr_mxr == 1
                && csr_vmxr == 0
                && !resp_s1_r
                && resp_s1_x
                && resp_s1_a
                && !resp_s1_pf
                && !resp_s2_r
                && resp_s2_x
                && resp_s2_a
                && !resp_s2_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.02.015 MXR_guest_11
        // 测试表目标：构造 Guest/Hyper 的普通读类请求，S1、S2 页均为仅执行页（R=0、X=1），A=1且无已有页异常。设置 Host mxr=1、Guest
        // vmxr=1：S1有效MXR=1，G阶段有效MXR=1。S1允许将执行页作为可读页。G阶段允许该读取。按实际stage只启用对应阶段；allStage下若S1先产生PF，最终GPF被其抑制。
        cp_04_02_015 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && (resp_virt || req_hyperinst_d1)
                && (req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                && !req_hlvx_d1
                && translate_enable
                && mode_permission
                && !access_fault
                && !fake_pte
                && !nonleaf_pte
                && csr_mxr == 1
                && csr_vmxr == 1
                && !resp_s1_r
                && resp_s1_x
                && resp_s1_a
                && !resp_s1_pf
                && !resp_s2_r
                && resp_s2_x
                && resp_s2_a
                && !resp_s2_pf
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.02.016 MXR_host
        // 测试表目标：场景：host read,S1.R=0,X=1,A=1，vmxr=1，mxr=0/1。 观察功能：仅mxr决定是否借X执行读权限，vmxr不生效。 测试目的：观察
        // U/S 页面权限、SUM、MXR 与 HLVX 执行权限检查之间的关系。
        cp_04_02_016 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && req_out_s2xlate == 0
                && translate_enable
                && req_out_s2xlate != 2'd2
                && (req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                && !req_hlvx_d1
                && mode_permission
                && !resp_s1_r
                && resp_s1_x
                && resp_s1_a
                && !resp_s1_pf
                && !access_fault
                && csr_vmxr
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.02.017 HLVX_执行权限读取
        // 测试表目标：场景：hyperinst=1,hlvx=1,read命令；R=0,X=1。 观察功能：S1和S2读权限改检查X，输出仍是ld fault类别；PMP cmd=7。
        // 测试目的：观察 U/S 页面权限、SUM、MXR 与 HLVX 执行权限检查之间的关系。
        cp_04_02_017 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && req_hyperinst_d1
                && req_hlvx_d1
                && (req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                && translate_enable
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.02.018 HLVX_MXR不可替代X
        // 测试表目标：场景：hlvx=1，X=0,R=1,mxr=1。 观察功能：仍权限失败；普通读在相同权限下可通过。 测试目的：观察 U/S 页面权限、SUM、MXR 与 HLVX
        // 执行权限检查之间的关系。
        cp_04_02_018 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && req_hlvx_d1
                && (req_out_cmd == 3'd0 || req_out_cmd == 3'd4)
                && translate_enable
                && resp_s1_r
                && !resp_s1_x
                && (csr_mxr || ((resp_virt || req_hyperinst_d1) && csr_vmxr))
                && mode_permission
                && resp_s1_a
                && !access_fault
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.02.001 MODE_CSR权限实时更新
        // 测试表目标：场景：有效entry保留，合法切换SUM/MXR/vSUM/vMXR/SPVP后再次请求。
        // 观察功能：不需重填permission位即可改变动态许可结果；请求拍/响应拍CSR按实际延迟域判定。 测试目的：观察 U/S 页面权限、SUM、MXR 与 HLVX
        // 执行权限检查之间的关系。
        cp_04_02_001 : coverpoint (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd ==
            3'd4) ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
            6'b010101) == 0 : 1'b1))
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && last_response_valid
                && last_vaddr == req_vaddr_d1
                && {csr_sum, csr_mxr, csr_vsum, csr_vmxr} != permission_controls_d1) {
            bins b_expected_set = {1'b1};
        }
    endgroup
    cg_dtlb_04_02 cg_dtlb_04_02_inst;

    // 04.03 阶段使能与无效字段隔离
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_04_03】04.03 阶段使能与无效字段隔离
    // 探测功能：观察真正参与本次翻译的阶段，避免未启用阶段的残留权限触发异常。
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：04.03.001, 04.03.002, 04.03.003, 04.03.004, 04.03.005, 04.03.006, 04.03.007,
    // 04.03.008, 04.03.009, 04.03.010, 04.03.011, 04.03.012；逐点功能见下方注释，精确配值见分解表。
    // 异常位序：resp_excp[5:0]={af_st,af_ld,gpf_st,gpf_ld,pf_st,pf_ld}；actual_fault==expected_fault核对类别，读写位掩码额外检查命令方向。
    // actual_fault：从实际输出resp_excp聚合[2:0]={AF,GPF,PF}，各类分别对load/store位做OR。
    // expected_fault：参考异常向量[2:0]={AF,GPF,PF}：预异常优先；无hit为0；有hit时AF优先于PF、PF优先于GPF。
    // pre_fault：请求拍 checkfullva 有效，且预检查 PF/GPF/AF 中至少一种成立；均使用_d1快照。
    covergroup cg_dtlb_04_03 @(posedge clock);
        option.per_instance = 1;
        // 04.03.005 权限阶段使能_stage0_TE0
        // 测试表目标：阶段编码 stage=0 表示noS2：非虚拟化 Host 路径，翻译启用时使用 satp 的 S 阶段，TE=0 表示本请求关闭翻译。观察
        // s1_valid/s2_valid 的组合应对应“两个阶段均不参与本地页表权限检查”，再分类实际异常输出；地址预检查异常按其独立条件处理。
        cp_04_03_005 : coverpoint ((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd ==
            3'd4) ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
            6'b010101) == 0 : 1'b1)))
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && req_out_s2xlate == 0
                && translate_enable == 0) {
            bins b_expected_result = {1'b1};
        }
        // 04.03.006 权限阶段使能_stage0_TE1
        // 测试表目标：阶段编码 stage=0 表示noS2：非虚拟化 Host 路径，翻译启用时使用 satp 的 S 阶段，TE=1 表示本请求启用翻译。观察
        // s1_valid/s2_valid 的组合应对应“仅 S/VS 第一阶段参与权限检查”，再分类实际异常输出；地址预检查异常按其独立条件处理。
        cp_04_03_006 : coverpoint ((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd ==
            3'd4) ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
            6'b010101) == 0 : 1'b1)))
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && req_out_s2xlate == 0
                && translate_enable == 1) {
            bins b_expected_result = {1'b1};
        }
        // 04.03.007 权限阶段使能_stage1_TE0
        // 测试表目标：阶段编码 stage=1 表示onlyStage1：只执行 Guest 的 VS 阶段，G 阶段 Bare，TE=0 表示本请求关闭翻译。观察
        // s1_valid/s2_valid 的组合应对应“两个阶段均不参与本地页表权限检查”，再分类实际异常输出；地址预检查异常按其独立条件处理。
        cp_04_03_007 : coverpoint ((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd ==
            3'd4) ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
            6'b010101) == 0 : 1'b1)))
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && req_out_s2xlate == 1
                && translate_enable == 0) {
            bins b_expected_result = {1'b1};
        }
        // 04.03.008 权限阶段使能_stage1_TE1
        // 测试表目标：阶段编码 stage=1 表示onlyStage1：只执行 Guest 的 VS 阶段，G 阶段 Bare，TE=1 表示本请求启用翻译。观察
        // s1_valid/s2_valid 的组合应对应“仅 S/VS 第一阶段参与权限检查”，再分类实际异常输出；地址预检查异常按其独立条件处理。
        cp_04_03_008 : coverpoint ((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd ==
            3'd4) ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
            6'b010101) == 0 : 1'b1)))
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && req_out_s2xlate == 1
                && translate_enable == 1) {
            bins b_expected_result = {1'b1};
        }
        // 04.03.009 权限阶段使能_stage2_TE0
        // 测试表目标：阶段编码 stage=2 表示onlyStage2：VS 阶段 Bare，只执行 G 阶段；是否实际翻译还需看 TE，TE=0 表示本请求关闭翻译。观察
        // s1_valid/s2_valid 的组合应对应“两个阶段均不参与本地页表权限检查”，再分类实际异常输出；地址预检查异常按其独立条件处理。
        cp_04_03_009 : coverpoint ((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd ==
            3'd4) ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
            6'b010101) == 0 : 1'b1)))
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && req_out_s2xlate == 2
                && translate_enable == 0) {
            bins b_expected_result = {1'b1};
        }
        // 04.03.010 权限阶段使能_stage2_TE1
        // 测试表目标：阶段编码 stage=2 表示onlyStage2：VS 阶段 Bare，只执行 G 阶段；是否实际翻译还需看 TE，TE=1 表示本请求启用翻译。观察
        // s1_valid/s2_valid 的组合应对应“仅 G 第二阶段参与权限检查”，再分类实际异常输出；地址预检查异常按其独立条件处理。
        cp_04_03_010 : coverpoint ((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd ==
            3'd4) ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
            6'b010101) == 0 : 1'b1)))
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && req_out_s2xlate == 2
                && translate_enable == 1) {
            bins b_expected_result = {1'b1};
        }
        // 04.03.011 权限阶段使能_stage3_TE0
        // 测试表目标：阶段编码 stage=3 表示allStage：VS 与 G 两阶段组合翻译，TE=0 表示本请求关闭翻译。观察 s1_valid/s2_valid
        // 的组合应对应“两个阶段均不参与本地页表权限检查”，再分类实际异常输出；地址预检查异常按其独立条件处理。
        cp_04_03_011 : coverpoint ((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd ==
            3'd4) ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
            6'b010101) == 0 : 1'b1)))
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && req_out_s2xlate == 3
                && translate_enable == 0) {
            bins b_expected_result = {1'b1};
        }
        // 04.03.012 权限阶段使能_stage3_TE1
        // 测试表目标：阶段编码 stage=3 表示allStage：VS 与 G 两阶段组合翻译，TE=1 表示本请求启用翻译。观察 s1_valid/s2_valid
        // 的组合应对应“VS、G 两阶段均参与权限检查”，再分类实际异常输出；地址预检查异常按其独立条件处理。
        cp_04_03_012 : coverpoint ((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd ==
            3'd4) ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
            6'b010101) == 0 : 1'b1)))
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && req_out_s2xlate == 3
                && translate_enable == 1) {
            bins b_expected_result = {1'b1};
        }
        // 04.03.004 PERM_翻译关闭
        // 测试表目标：场景：translateEnable=0且无pre-fault，表项权限任意。 观察功能：不产生本地页表权限PF/GPF/AF；PMP仍可产生外部访问错误。
        // 测试目的：观察真正参与本次翻译的阶段，避免未启用阶段的残留权限触发异常。
        cp_04_03_004 : coverpoint (actual_fault == 0)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && !pre_fault
                && !translate_enable) {
            bins b_expected_result = {1'b1};
        }
        // 04.03.001 S2_U_G不参与本地权限
        // 测试表目标：场景：仅改变返回S2.U/G，S2 R/W/X/A/D不变。 观察功能：当前DTLB不以S2.U/G决定permission；S2.U是否合法由PTW负责。
        // 测试目的：观察真正参与本次翻译的阶段，避免未启用阶段的残留权限触发异常。
        cp_04_03_001 : coverpoint (actual_fault == last_fault)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && last_response_valid
                && req_out_s2xlate >= 2
                && ptw_valid
                && {ptw_s2_u, ptw_s2_g} != s2_ug_d1
                && req_out_vaddr == last_vaddr) {
            bins b_expected_result = {1'b1};
        }
        // 04.03.002 PERM_S1无效stage不影响
        // 测试表目标：场景：onlyS2且S1权限拒绝/PF/AF任意。 观察功能：仅S2决定翻译异常；S1.AF也不应生效。
        // 测试目的：观察真正参与本次翻译的阶段，避免未启用阶段的残留权限触发异常。
        cp_04_03_002 : coverpoint (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd ==
            3'd4) ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
            6'b010101) == 0 : 1'b1))
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && translate_enable
                && req_out_s2xlate == 2) {
            bins b_expected_set = {1'b1};
        }
        // 04.03.003 PERM_S2无效stage不影响
        // 测试表目标：场景：noS2/onlyS1且S2权限拒绝/GPF/GAF任意。 观察功能：仅S1决定翻译异常。
        // 测试目的：观察真正参与本次翻译的阶段，避免未启用阶段的残留权限触发异常。
        cp_04_03_003 : coverpoint (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd ==
            3'd4) ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
            6'b010101) == 0 : 1'b1))
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && translate_enable
                && req_out_s2xlate < 2) {
            bins b_expected_set = {1'b1};
        }
    endgroup
    cg_dtlb_04_03 cg_dtlb_04_03_inst;

    // 04.04 异常优先级与PTE类型
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_04_04】04.04 异常优先级与PTE类型
    // 探测功能：观察多个异常原因同时存在时的输出优先级，以及 fake/非叶 PTE 对本地权限判断的影响。
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：04.04.001, 04.04.002, 04.04.003, 04.04.004, 04.04.005, 04.04.006, 04.04.007,
    // 04.04.008, 04.04.009, 04.04.010, 04.04.011, 04.04.012, 04.04.013,
    // 04.04.014；逐点功能见下方注释，精确配值见分解表。
    // 异常位序：resp_excp[5:0]={af_st,af_ld,gpf_st,gpf_ld,pf_st,pf_ld}；actual_fault==expected_fault核对类别，读写位掩码额外检查命令方向。
    // 来源仓：b_storage=ptw_bypass_hit为0，b_bypass=1；具体仓是否要求storage_hit由各自iff显式限定。
    // access_fault：仅检查本次启用的翻译阶段：S1.af 或 S2.af；不启用的阶段不产生AF。
    // actual_fault：从实际输出resp_excp聚合[2:0]={AF,GPF,PF}，各类分别对load/store位做OR。
    // expected_fault：参考异常向量[2:0]={AF,GPF,PF}：预异常优先；无hit为0；有hit时AF优先于PF、PF优先于GPF。
    // fake_pte：S1的V/PF/AF均为0且不是onlyS2；用于区分假PTE分支。
    // nonleaf_pte：S1的V为1、R/W/X/PF/AF均为0；表示非叶PTE。
    // pre_fault：请求拍 checkfullva 有效，且预检查 PF/GPF/AF 中至少一种成立；均使用_d1快照。
    // s1_page_fault：S1页故障参考值：启用S1、排除fake/nonleaf后，检查命令、特权、R/W/X/A/D和上游PF。
    // s2_page_fault：S2页故障参考值：启用S2后检查命令、R/W/X/A/D和上游PF；优先级由expected_fault处理。
    covergroup cg_dtlb_04_04 @(posedge clock);
        option.per_instance = 1;
        // 04.04.013 FAULT_V0并非本地invalidPTE判定
        // 测试表目标：场景：S1.v=0但s1.pf=1；或v=0,pf=af=0。 观察功能：前者传播PF；后者fakePTE路径；不要把v=0一律解释为PF。
        // 测试目的：观察多个异常原因同时存在时的输出优先级，以及 fake/非叶 PTE 对本地权限判断的影响。
        cp_04_04_013 : coverpoint ((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd ==
            3'd4) ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
            6'b010101) == 0 : 1'b1)))
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && translate_enable
                && req_out_s2xlate != 2'd2
                && !resp_s1_v
                && !resp_s1_af) {
            bins b_expected_result = {1'b1};
        }
        // 04.04.014 FCOV_异常输出组合
        // 测试表目标：场景：O.valid且来源可判定，记录pre-fault/命中来源和潜在fault原因。
        // 观察功能：从实际excp输出采掩码；用参考优先级派生expected_mask，枚举relation={相符,偏离,不可判定}；不可判定不冒充相符。
        // 测试目的：观察多个异常原因同时存在时的输出优先级，以及 fake/非叶 PTE 对本地权限判断的影响。
        cp_04_04_014 : coverpoint ((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd ==
            3'd4) ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
            6'b010101) == 0 : 1'b1)))
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && ((storage_hit || ptw_bypass_hit) || pre_fault || !translate_enable)) {
            bins b_expected_result = {1'b1};
        }
        // 04.04.001 FAULT_S1_AF_cmd0
        // 测试表目标：场景：S1参与且s1.af=1；cmd=0。 观察功能：输出AF并屏蔽PF/GPF；读/写各自分类。 测试目的：观察多个异常原因同时存在时的输出优先级，以及
        // fake/非叶 PTE 对本地权限判断的影响。
        cp_04_04_001 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && req_out_cmd == 0
                && translate_enable
                && req_out_s2xlate != 2'd2
                && resp_s1_af
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.04.002 FAULT_S1_AF_cmd1
        // 测试表目标：场景：S1参与且s1.af=1；cmd=1。 观察功能：输出AF并屏蔽PF/GPF；读/写各自分类。 测试目的：观察多个异常原因同时存在时的输出优先级，以及
        // fake/非叶 PTE 对本地权限判断的影响。
        cp_04_04_002 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && req_out_cmd == 1
                && translate_enable
                && req_out_s2xlate != 2'd2
                && resp_s1_af
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.04.003 FAULT_S2_AF_cmd0
        // 测试表目标：场景：S2参与且s2.gaf=1；cmd=0。 观察功能：输出AF并屏蔽PF/GPF。 测试目的：观察多个异常原因同时存在时的输出优先级，以及 fake/非叶
        // PTE 对本地权限判断的影响。
        cp_04_04_003 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && req_out_cmd == 0
                && translate_enable
                && req_out_s2xlate >= 2'd2
                && resp_s2_af
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.04.004 FAULT_S2_AF_cmd1
        // 测试表目标：场景：S2参与且s2.gaf=1；cmd=1。 观察功能：输出AF并屏蔽PF/GPF。 测试目的：观察多个异常原因同时存在时的输出优先级，以及 fake/非叶
        // PTE 对本地权限判断的影响。
        cp_04_04_004 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && req_out_cmd == 1
                && translate_enable
                && req_out_s2xlate >= 2'd2
                && resp_s2_af
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.04.005 FAULT_PF优先GPF_cmd0
        // 测试表目标：场景：allStage，S1产生PF且S2同时产生GPF，无AF；cmd=0。 观察功能：PF有效、GPF被hasPf抑制；不同时上报。
        // 测试目的：观察多个异常原因同时存在时的输出优先级，以及 fake/非叶 PTE 对本地权限判断的影响。
        cp_04_04_005 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && req_out_cmd == 0
                && req_out_s2xlate == 3
                && translate_enable
                && !access_fault
                && s1_page_fault
                && s2_page_fault
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.04.006 FAULT_PF优先GPF_cmd1
        // 测试表目标：场景：allStage，S1产生PF且S2同时产生GPF，无AF；cmd=1。 观察功能：PF有效、GPF被hasPf抑制；不同时上报。
        // 测试目的：观察多个异常原因同时存在时的输出优先级，以及 fake/非叶 PTE 对本地权限判断的影响。
        cp_04_04_006 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && req_out_cmd == 1
                && req_out_s2xlate == 3
                && translate_enable
                && !access_fault
                && s1_page_fault
                && s2_page_fault
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.04.007 FAULT_AF优先PF_GPF_cmd0
        // 测试表目标：场景：allStage，AF/PF/GPF潜在原因同时存在；cmd=0。 观察功能：PTW类AF最高；不套用外部PMP与PF的不同优先规则。
        // 测试目的：观察多个异常原因同时存在时的输出优先级，以及 fake/非叶 PTE 对本地权限判断的影响。
        cp_04_04_007 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && req_out_cmd == 0
                && req_out_s2xlate == 3
                && translate_enable
                && access_fault
                && s1_page_fault
                && s2_page_fault
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.04.008 FAULT_AF优先PF_GPF_cmd1
        // 测试表目标：场景：allStage，AF/PF/GPF潜在原因同时存在；cmd=1。 观察功能：PTW类AF最高；不套用外部PMP与PF的不同优先规则。
        // 测试目的：观察多个异常原因同时存在时的输出优先级，以及 fake/非叶 PTE 对本地权限判断的影响。
        cp_04_04_008 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && req_out_cmd == 1
                && req_out_s2xlate == 3
                && translate_enable
                && access_fault
                && s1_page_fault
                && s2_page_fault
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.04.009 FAULT_fakePTE_cmd0
        // 测试表目标：场景：S1.v=0,pf=0,af=0；allStage且S2故障；cmd=0。 观察功能：S1
        // PF抑制，isForVSnonLeafPTE=1；GPF按S2条件产生。 测试目的：观察多个异常原因同时存在时的输出优先级，以及 fake/非叶 PTE 对本地权限判断的影响。
        cp_04_04_009 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && req_out_cmd == 0
                && req_out_s2xlate == 3
                && translate_enable
                && fake_pte
                && s2_page_fault
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.04.010 FAULT_fakePTE_cmd1
        // 测试表目标：场景：S1.v=0,pf=0,af=0；allStage且S2故障；cmd=1。 观察功能：S1
        // PF抑制，isForVSnonLeafPTE=1；GPF按S2条件产生。 测试目的：观察多个异常原因同时存在时的输出优先级，以及 fake/非叶 PTE 对本地权限判断的影响。
        cp_04_04_010 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && req_out_cmd == 1
                && req_out_s2xlate == 3
                && translate_enable
                && fake_pte
                && s2_page_fault
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.04.011 FAULT_nonLeaf_cmd0
        // 测试表目标：场景：S1.v=1,R=W=X=0,pf=af=0；allStage且S2故障；cmd=0。 观察功能：S1
        // PF抑制，isForVSnonLeafPTE=1；GPA是页表访问地址。 测试目的：观察多个异常原因同时存在时的输出优先级，以及 fake/非叶 PTE 对本地权限判断的影响。
        cp_04_04_011 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && req_out_cmd == 0
                && req_out_s2xlate == 3
                && translate_enable
                && nonleaf_pte
                && s2_page_fault
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.04.012 FAULT_nonLeaf_cmd1
        // 测试表目标：场景：S1.v=1,R=W=X=0,pf=af=0；allStage且S2故障；cmd=1。 观察功能：S1
        // PF抑制，isForVSnonLeafPTE=1；GPA是页表访问地址。 测试目的：观察多个异常原因同时存在时的输出优先级，以及 fake/非叶 PTE 对本地权限判断的影响。
        cp_04_04_012 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !pre_fault
                && req_out_cmd == 1
                && req_out_s2xlate == 3
                && translate_enable
                && nonleaf_pte
                && s2_page_fault
                && ((((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp
                    & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0
                    : 1'b1)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
    endgroup
    cg_dtlb_04_04 cg_dtlb_04_04_inst;

    // 04.05 异常分类与Consumer行为
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_04_05】04.05 异常分类与Consumer行为
    // 探测功能：观察异常按读/写命令分类、相关输出字段含义，以及缓存结果和旁路结果的对应关系。
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：04.05.001, 04.05.002, 04.05.003；逐点功能见下方注释，精确配值见分解表。
    // 异常位序：resp_excp[5:0]={af_st,af_ld,gpf_st,gpf_ld,pf_st,pf_ld}；actual_fault==expected_fault核对类别，读写位掩码额外检查命令方向。
    // 来源仓：b_storage=ptw_bypass_hit为0，b_bypass=1；具体仓是否要求storage_hit由各自iff显式限定。
    // actual_fault：从实际输出resp_excp聚合[2:0]={AF,GPF,PF}，各类分别对load/store位做OR。
    // expected_fault：参考异常向量[2:0]={AF,GPF,PF}：预异常优先；无hit为0；有hit时AF优先于PF、PF优先于GPF。
    // pre_fault：请求拍 checkfullva 有效，且预检查 PF/GPF/AF 中至少一种成立；均使用_d1快照。
    covergroup cg_dtlb_04_05 @(posedge clock);
        option.per_instance = 1;
        // 04.05.001 FAULT_SC_AMO分类
        // 测试表目标：场景：cmd=5且触发PF/GPF/AF。 观察功能：走store故障类别；LR cmd=4走load类别。
        // 测试目的：观察异常按读/写命令分类、相关输出字段含义，以及缓存结果和旁路结果的对应关系。
        cp_04_05_001 : coverpoint ((actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd ==
            3'd4) ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
            6'b010101) == 0 : 1'b1)))
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && req_out_cmd inside {4, 5}
                && ((storage_hit || ptw_bypass_hit) || pre_fault)) {
            bins b_expected_result = {1'b1};
        }
        // 04.05.002 FAULT_正常vaNeedExt
        // 测试表目标：场景：没有pre-fault分支，O.valid。 观察功能：vaNeedExt=1；pre-fault为0，分别覆盖。
        // 测试目的：观察异常按读/写命令分类、相关输出字段含义，以及缓存结果和旁路结果的对应关系。
        cp_04_05_002 : coverpoint (resp_ext)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && !pre_fault
                && GROUP_ID<2
                && (!(GROUP_ID == 0 && PORT_INDEX == 3))) {
            bins b_expected_result = {1'b1};
        }
        // 04.05.003 FAULT_缓存与旁路等价
        // 测试表目标：场景：同映射先PTW bypass，再storage命中。 观察功能：权限/PBMT/fault类别相同；GPF可能先经历need_gpa重取。
        // 测试目的：观察异常按读/写命令分类、相关输出字段含义，以及缓存结果和旁路结果的对应关系。
        cp_04_05_003 : coverpoint (actual_fault == last_fault && resp_pbmt == last_pbmt)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && last_response_valid
                && last_bypass
                && storage_hit
                && !ptw_bypass_hit
                && last_vaddr == req_vaddr_d1) {
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_04_05 cg_dtlb_04_05_inst;

    // 04.06 PBMT选取与覆盖
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_04_06】04.06 PBMT选取与覆盖
    // 探测功能：观察两阶段 PBMT 的选择规则及翻译关闭时的属性处理。
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：04.06.001, 04.06.002, 04.06.003, 04.06.004, 04.06.005, 04.06.006, 04.06.007,
    // 04.06.008, 04.06.009, 04.06.010, 04.06.011, 04.06.012, 04.06.013, 04.06.014, 04.06.015,
    // 04.06.016, 04.06.017, 04.06.018, 04.06.019, 04.06.020, 04.06.021, 04.06.022, 04.06.023,
    // 04.06.024, 04.06.025, 04.06.026, 04.06.027, 04.06.028, 04.06.029, 04.06.030, 04.06.031,
    // 04.06.032, 04.06.033, 04.06.034, 04.06.035, 04.06.036, 04.06.037；逐点功能见下方注释，精确配值见分解表。
    // 来源仓：b_storage=ptw_bypass_hit为0，b_bypass=1；具体仓是否要求storage_hit由各自iff显式限定。
    // expected_pbmt：参考PBMT：直通为0；onlyS2取S2；双阶段且S2非0时取S2；其余取S1。
    covergroup cg_dtlb_04_06 @(posedge clock);
        option.per_instance = 1;
        // 04.06.001 PBMT_s0_00
        // 测试表目标：在noS2：非虚拟化 Host 路径，翻译启用时使用 satp 的 S 阶段且翻译使能的命中响应中，将 S1 PBMT 设为0（PMA（使用物理内存属性）），S2
        // PBMT 设为0（PMA（使用物理内存属性））。观察选择 S1 PBMT，S2 属性不参与本次输出，输出 PBMT 应分类为0（PMA（使用物理内存属性））；分别记录缓存命中与
        // PTW 旁路的来源。
        cp_04_06_001 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 0
                && resp_s1pbmt == 0
                && resp_s2pbmt == 0
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.002 PBMT_s0_01
        // 测试表目标：在noS2：非虚拟化 Host 路径，翻译启用时使用 satp 的 S 阶段且翻译使能的命中响应中，将 S1 PBMT 设为0（PMA（使用物理内存属性）），S2
        // PBMT 设为1（NC（不可缓存内存））。观察选择 S1 PBMT，S2 属性不参与本次输出，输出 PBMT 应分类为0（PMA（使用物理内存属性））；分别记录缓存命中与
        // PTW 旁路的来源。
        cp_04_06_002 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 0
                && resp_s1pbmt == 0
                && resp_s2pbmt == 1
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.003 PBMT_s0_02
        // 测试表目标：在noS2：非虚拟化 Host 路径，翻译启用时使用 satp 的 S 阶段且翻译使能的命中响应中，将 S1 PBMT 设为0（PMA（使用物理内存属性）），S2
        // PBMT 设为2（IO（I/O 内存））。观察选择 S1 PBMT，S2 属性不参与本次输出，输出 PBMT 应分类为0（PMA（使用物理内存属性））；分别记录缓存命中与
        // PTW 旁路的来源。
        cp_04_06_003 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 0
                && resp_s1pbmt == 0
                && resp_s2pbmt == 2
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.004 PBMT_s0_10
        // 测试表目标：在noS2：非虚拟化 Host 路径，翻译启用时使用 satp 的 S 阶段且翻译使能的命中响应中，将 S1 PBMT 设为1（NC（不可缓存内存）），S2
        // PBMT 设为0（PMA（使用物理内存属性））。观察选择 S1 PBMT，S2 属性不参与本次输出，输出 PBMT 应分类为1（NC（不可缓存内存））；分别记录缓存命中与
        // PTW 旁路的来源。
        cp_04_06_004 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 0
                && resp_s1pbmt == 1
                && resp_s2pbmt == 0
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.005 PBMT_s0_11
        // 测试表目标：在noS2：非虚拟化 Host 路径，翻译启用时使用 satp 的 S 阶段且翻译使能的命中响应中，将 S1 PBMT 设为1（NC（不可缓存内存）），S2
        // PBMT 设为1（NC（不可缓存内存））。观察选择 S1 PBMT，S2 属性不参与本次输出，输出 PBMT 应分类为1（NC（不可缓存内存））；分别记录缓存命中与 PTW
        // 旁路的来源。
        cp_04_06_005 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 0
                && resp_s1pbmt == 1
                && resp_s2pbmt == 1
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.006 PBMT_s0_12
        // 测试表目标：在noS2：非虚拟化 Host 路径，翻译启用时使用 satp 的 S 阶段且翻译使能的命中响应中，将 S1 PBMT 设为1（NC（不可缓存内存）），S2
        // PBMT 设为2（IO（I/O 内存））。观察选择 S1 PBMT，S2 属性不参与本次输出，输出 PBMT 应分类为1（NC（不可缓存内存））；分别记录缓存命中与 PTW
        // 旁路的来源。
        cp_04_06_006 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 0
                && resp_s1pbmt == 1
                && resp_s2pbmt == 2
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.007 PBMT_s0_20
        // 测试表目标：在noS2：非虚拟化 Host 路径，翻译启用时使用 satp 的 S 阶段且翻译使能的命中响应中，将 S1 PBMT 设为2（IO（I/O 内存）），S2
        // PBMT 设为0（PMA（使用物理内存属性））。观察选择 S1 PBMT，S2 属性不参与本次输出，输出 PBMT 应分类为2（IO（I/O 内存））；分别记录缓存命中与
        // PTW 旁路的来源。
        cp_04_06_007 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 0
                && resp_s1pbmt == 2
                && resp_s2pbmt == 0
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.008 PBMT_s0_21
        // 测试表目标：在noS2：非虚拟化 Host 路径，翻译启用时使用 satp 的 S 阶段且翻译使能的命中响应中，将 S1 PBMT 设为2（IO（I/O 内存）），S2
        // PBMT 设为1（NC（不可缓存内存））。观察选择 S1 PBMT，S2 属性不参与本次输出，输出 PBMT 应分类为2（IO（I/O 内存））；分别记录缓存命中与 PTW
        // 旁路的来源。
        cp_04_06_008 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 0
                && resp_s1pbmt == 2
                && resp_s2pbmt == 1
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.009 PBMT_s0_22
        // 测试表目标：在noS2：非虚拟化 Host 路径，翻译启用时使用 satp 的 S 阶段且翻译使能的命中响应中，将 S1 PBMT 设为2（IO（I/O 内存）），S2
        // PBMT 设为2（IO（I/O 内存））。观察选择 S1 PBMT，S2 属性不参与本次输出，输出 PBMT 应分类为2（IO（I/O 内存））；分别记录缓存命中与 PTW
        // 旁路的来源。
        cp_04_06_009 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 0
                && resp_s1pbmt == 2
                && resp_s2pbmt == 2
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.010 PBMT_s1_00
        // 测试表目标：在onlyStage1：只执行 Guest 的 VS 阶段，G 阶段 Bare且翻译使能的命中响应中，将 S1 PBMT 设为0（PMA（使用物理内存属性）），S2
        // PBMT 设为0（PMA（使用物理内存属性））。观察选择 S1 PBMT，S2 属性不参与本次输出，输出 PBMT 应分类为0（PMA（使用物理内存属性））；分别记录缓存命中与
        // PTW 旁路的来源。
        cp_04_06_010 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 1
                && resp_s1pbmt == 0
                && resp_s2pbmt == 0
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.011 PBMT_s1_01
        // 测试表目标：在onlyStage1：只执行 Guest 的 VS 阶段，G 阶段 Bare且翻译使能的命中响应中，将 S1 PBMT 设为0（PMA（使用物理内存属性）），S2
        // PBMT 设为1（NC（不可缓存内存））。观察选择 S1 PBMT，S2 属性不参与本次输出，输出 PBMT 应分类为0（PMA（使用物理内存属性））；分别记录缓存命中与
        // PTW 旁路的来源。
        cp_04_06_011 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 1
                && resp_s1pbmt == 0
                && resp_s2pbmt == 1
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.012 PBMT_s1_02
        // 测试表目标：在onlyStage1：只执行 Guest 的 VS 阶段，G 阶段 Bare且翻译使能的命中响应中，将 S1 PBMT 设为0（PMA（使用物理内存属性）），S2
        // PBMT 设为2（IO（I/O 内存））。观察选择 S1 PBMT，S2 属性不参与本次输出，输出 PBMT 应分类为0（PMA（使用物理内存属性））；分别记录缓存命中与
        // PTW 旁路的来源。
        cp_04_06_012 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 1
                && resp_s1pbmt == 0
                && resp_s2pbmt == 2
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.013 PBMT_s1_10
        // 测试表目标：在onlyStage1：只执行 Guest 的 VS 阶段，G 阶段 Bare且翻译使能的命中响应中，将 S1 PBMT 设为1（NC（不可缓存内存）），S2
        // PBMT 设为0（PMA（使用物理内存属性））。观察选择 S1 PBMT，S2 属性不参与本次输出，输出 PBMT 应分类为1（NC（不可缓存内存））；分别记录缓存命中与
        // PTW 旁路的来源。
        cp_04_06_013 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 1
                && resp_s1pbmt == 1
                && resp_s2pbmt == 0
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.014 PBMT_s1_11
        // 测试表目标：在onlyStage1：只执行 Guest 的 VS 阶段，G 阶段 Bare且翻译使能的命中响应中，将 S1 PBMT 设为1（NC（不可缓存内存）），S2
        // PBMT 设为1（NC（不可缓存内存））。观察选择 S1 PBMT，S2 属性不参与本次输出，输出 PBMT 应分类为1（NC（不可缓存内存））；分别记录缓存命中与 PTW
        // 旁路的来源。
        cp_04_06_014 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 1
                && resp_s1pbmt == 1
                && resp_s2pbmt == 1
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.015 PBMT_s1_12
        // 测试表目标：在onlyStage1：只执行 Guest 的 VS 阶段，G 阶段 Bare且翻译使能的命中响应中，将 S1 PBMT 设为1（NC（不可缓存内存）），S2
        // PBMT 设为2（IO（I/O 内存））。观察选择 S1 PBMT，S2 属性不参与本次输出，输出 PBMT 应分类为1（NC（不可缓存内存））；分别记录缓存命中与 PTW
        // 旁路的来源。
        cp_04_06_015 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 1
                && resp_s1pbmt == 1
                && resp_s2pbmt == 2
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.016 PBMT_s1_20
        // 测试表目标：在onlyStage1：只执行 Guest 的 VS 阶段，G 阶段 Bare且翻译使能的命中响应中，将 S1 PBMT 设为2（IO（I/O 内存）），S2
        // PBMT 设为0（PMA（使用物理内存属性））。观察选择 S1 PBMT，S2 属性不参与本次输出，输出 PBMT 应分类为2（IO（I/O 内存））；分别记录缓存命中与
        // PTW 旁路的来源。
        cp_04_06_016 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 1
                && resp_s1pbmt == 2
                && resp_s2pbmt == 0
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.017 PBMT_s1_21
        // 测试表目标：在onlyStage1：只执行 Guest 的 VS 阶段，G 阶段 Bare且翻译使能的命中响应中，将 S1 PBMT 设为2（IO（I/O 内存）），S2
        // PBMT 设为1（NC（不可缓存内存））。观察选择 S1 PBMT，S2 属性不参与本次输出，输出 PBMT 应分类为2（IO（I/O 内存））；分别记录缓存命中与 PTW
        // 旁路的来源。
        cp_04_06_017 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 1
                && resp_s1pbmt == 2
                && resp_s2pbmt == 1
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.018 PBMT_s1_22
        // 测试表目标：在onlyStage1：只执行 Guest 的 VS 阶段，G 阶段 Bare且翻译使能的命中响应中，将 S1 PBMT 设为2（IO（I/O 内存）），S2
        // PBMT 设为2（IO（I/O 内存））。观察选择 S1 PBMT，S2 属性不参与本次输出，输出 PBMT 应分类为2（IO（I/O 内存））；分别记录缓存命中与 PTW
        // 旁路的来源。
        cp_04_06_018 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 1
                && resp_s1pbmt == 2
                && resp_s2pbmt == 2
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.019 PBMT_s2_00
        // 测试表目标：在onlyStage2：VS 阶段 Bare，只执行 G 阶段；是否实际翻译还需看 TE且翻译使能的命中响应中，将 S1 PBMT
        // 设为0（PMA（使用物理内存属性）），S2 PBMT 设为0（PMA（使用物理内存属性））。观察选择 S2 PBMT，S1 属性不参与本次输出，输出 PBMT
        // 应分类为0（PMA（使用物理内存属性））；分别记录缓存命中与 PTW 旁路的来源。
        cp_04_06_019 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 2
                && resp_s1pbmt == 0
                && resp_s2pbmt == 0
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.020 PBMT_s2_01
        // 测试表目标：在onlyStage2：VS 阶段 Bare，只执行 G 阶段；是否实际翻译还需看 TE且翻译使能的命中响应中，将 S1 PBMT
        // 设为0（PMA（使用物理内存属性）），S2 PBMT 设为1（NC（不可缓存内存））。观察选择 S2 PBMT，S1 属性不参与本次输出，输出 PBMT
        // 应分类为1（NC（不可缓存内存））；分别记录缓存命中与 PTW 旁路的来源。
        cp_04_06_020 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 2
                && resp_s1pbmt == 0
                && resp_s2pbmt == 1
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.021 PBMT_s2_02
        // 测试表目标：在onlyStage2：VS 阶段 Bare，只执行 G 阶段；是否实际翻译还需看 TE且翻译使能的命中响应中，将 S1 PBMT
        // 设为0（PMA（使用物理内存属性）），S2 PBMT 设为2（IO（I/O 内存））。观察选择 S2 PBMT，S1 属性不参与本次输出，输出 PBMT
        // 应分类为2（IO（I/O 内存））；分别记录缓存命中与 PTW 旁路的来源。
        cp_04_06_021 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 2
                && resp_s1pbmt == 0
                && resp_s2pbmt == 2
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.022 PBMT_s2_10
        // 测试表目标：在onlyStage2：VS 阶段 Bare，只执行 G 阶段；是否实际翻译还需看 TE且翻译使能的命中响应中，将 S1 PBMT
        // 设为1（NC（不可缓存内存）），S2 PBMT 设为0（PMA（使用物理内存属性））。观察选择 S2 PBMT，S1 属性不参与本次输出，输出 PBMT
        // 应分类为0（PMA（使用物理内存属性））；分别记录缓存命中与 PTW 旁路的来源。
        cp_04_06_022 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 2
                && resp_s1pbmt == 1
                && resp_s2pbmt == 0
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.023 PBMT_s2_11
        // 测试表目标：在onlyStage2：VS 阶段 Bare，只执行 G 阶段；是否实际翻译还需看 TE且翻译使能的命中响应中，将 S1 PBMT
        // 设为1（NC（不可缓存内存）），S2 PBMT 设为1（NC（不可缓存内存））。观察选择 S2 PBMT，S1 属性不参与本次输出，输出 PBMT
        // 应分类为1（NC（不可缓存内存））；分别记录缓存命中与 PTW 旁路的来源。
        cp_04_06_023 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 2
                && resp_s1pbmt == 1
                && resp_s2pbmt == 1
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.024 PBMT_s2_12
        // 测试表目标：在onlyStage2：VS 阶段 Bare，只执行 G 阶段；是否实际翻译还需看 TE且翻译使能的命中响应中，将 S1 PBMT
        // 设为1（NC（不可缓存内存）），S2 PBMT 设为2（IO（I/O 内存））。观察选择 S2 PBMT，S1 属性不参与本次输出，输出 PBMT 应分类为2（IO（I/O
        // 内存））；分别记录缓存命中与 PTW 旁路的来源。
        cp_04_06_024 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 2
                && resp_s1pbmt == 1
                && resp_s2pbmt == 2
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.025 PBMT_s2_20
        // 测试表目标：在onlyStage2：VS 阶段 Bare，只执行 G 阶段；是否实际翻译还需看 TE且翻译使能的命中响应中，将 S1 PBMT 设为2（IO（I/O
        // 内存）），S2 PBMT 设为0（PMA（使用物理内存属性））。观察选择 S2 PBMT，S1 属性不参与本次输出，输出 PBMT
        // 应分类为0（PMA（使用物理内存属性））；分别记录缓存命中与 PTW 旁路的来源。
        cp_04_06_025 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 2
                && resp_s1pbmt == 2
                && resp_s2pbmt == 0
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.026 PBMT_s2_21
        // 测试表目标：在onlyStage2：VS 阶段 Bare，只执行 G 阶段；是否实际翻译还需看 TE且翻译使能的命中响应中，将 S1 PBMT 设为2（IO（I/O
        // 内存）），S2 PBMT 设为1（NC（不可缓存内存））。观察选择 S2 PBMT，S1 属性不参与本次输出，输出 PBMT
        // 应分类为1（NC（不可缓存内存））；分别记录缓存命中与 PTW 旁路的来源。
        cp_04_06_026 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 2
                && resp_s1pbmt == 2
                && resp_s2pbmt == 1
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.027 PBMT_s2_22
        // 测试表目标：在onlyStage2：VS 阶段 Bare，只执行 G 阶段；是否实际翻译还需看 TE且翻译使能的命中响应中，将 S1 PBMT 设为2（IO（I/O
        // 内存）），S2 PBMT 设为2（IO（I/O 内存））。观察选择 S2 PBMT，S1 属性不参与本次输出，输出 PBMT 应分类为2（IO（I/O
        // 内存））；分别记录缓存命中与 PTW 旁路的来源。
        cp_04_06_027 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 2
                && resp_s1pbmt == 2
                && resp_s2pbmt == 2
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.028 PBMT_s3_00
        // 测试表目标：在allStage：VS 与 G 两阶段组合翻译且翻译使能的命中响应中，将 S1 PBMT 设为0（PMA（使用物理内存属性）），S2 PBMT
        // 设为0（PMA（使用物理内存属性））。观察S1 PBMT 为0，继续采用 S2 属性，输出 PBMT 应分类为0（PMA（使用物理内存属性））；分别记录缓存命中与 PTW
        // 旁路的来源。
        cp_04_06_028 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 3
                && resp_s1pbmt == 0
                && resp_s2pbmt == 0
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.029 PBMT_s3_01
        // 测试表目标：在allStage：VS 与 G 两阶段组合翻译且翻译使能的命中响应中，将 S1 PBMT 设为0（PMA（使用物理内存属性）），S2 PBMT
        // 设为1（NC（不可缓存内存））。观察S1 PBMT 为0，继续采用 S2 属性，输出 PBMT 应分类为1（NC（不可缓存内存））；分别记录缓存命中与 PTW 旁路的来源。
        cp_04_06_029 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 3
                && resp_s1pbmt == 0
                && resp_s2pbmt == 1
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.030 PBMT_s3_02
        // 测试表目标：在allStage：VS 与 G 两阶段组合翻译且翻译使能的命中响应中，将 S1 PBMT 设为0（PMA（使用物理内存属性）），S2 PBMT
        // 设为2（IO（I/O 内存））。观察S1 PBMT 为0，继续采用 S2 属性，输出 PBMT 应分类为2（IO（I/O 内存））；分别记录缓存命中与 PTW 旁路的来源。
        cp_04_06_030 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 3
                && resp_s1pbmt == 0
                && resp_s2pbmt == 2
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.031 PBMT_s3_10
        // 测试表目标：在allStage：VS 与 G 两阶段组合翻译且翻译使能的命中响应中，将 S1 PBMT 设为1（NC（不可缓存内存）），S2 PBMT
        // 设为0（PMA（使用物理内存属性））。观察S1 PBMT 非0，由 S1 覆盖 S2 属性，输出 PBMT 应分类为1（NC（不可缓存内存））；分别记录缓存命中与 PTW
        // 旁路的来源。
        cp_04_06_031 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 3
                && resp_s1pbmt == 1
                && resp_s2pbmt == 0
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.032 PBMT_s3_11
        // 测试表目标：在allStage：VS 与 G 两阶段组合翻译且翻译使能的命中响应中，将 S1 PBMT 设为1（NC（不可缓存内存）），S2 PBMT
        // 设为1（NC（不可缓存内存））。观察S1 PBMT 非0，由 S1 覆盖 S2 属性，输出 PBMT 应分类为1（NC（不可缓存内存））；分别记录缓存命中与 PTW
        // 旁路的来源。
        cp_04_06_032 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 3
                && resp_s1pbmt == 1
                && resp_s2pbmt == 1
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.033 PBMT_s3_12
        // 测试表目标：在allStage：VS 与 G 两阶段组合翻译且翻译使能的命中响应中，将 S1 PBMT 设为1（NC（不可缓存内存）），S2 PBMT 设为2（IO（I/O
        // 内存））。观察S1 PBMT 非0，由 S1 覆盖 S2 属性，输出 PBMT 应分类为1（NC（不可缓存内存））；分别记录缓存命中与 PTW 旁路的来源。
        cp_04_06_033 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 3
                && resp_s1pbmt == 1
                && resp_s2pbmt == 2
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.034 PBMT_s3_20
        // 测试表目标：在allStage：VS 与 G 两阶段组合翻译且翻译使能的命中响应中，将 S1 PBMT 设为2（IO（I/O 内存）），S2 PBMT
        // 设为0（PMA（使用物理内存属性））。观察S1 PBMT 非0，由 S1 覆盖 S2 属性，输出 PBMT 应分类为2（IO（I/O 内存））；分别记录缓存命中与 PTW
        // 旁路的来源。
        cp_04_06_034 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 3
                && resp_s1pbmt == 2
                && resp_s2pbmt == 0
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.035 PBMT_s3_21
        // 测试表目标：在allStage：VS 与 G 两阶段组合翻译且翻译使能的命中响应中，将 S1 PBMT 设为2（IO（I/O 内存）），S2 PBMT
        // 设为1（NC（不可缓存内存））。观察S1 PBMT 非0，由 S1 覆盖 S2 属性，输出 PBMT 应分类为2（IO（I/O 内存））；分别记录缓存命中与 PTW
        // 旁路的来源。
        cp_04_06_035 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 3
                && resp_s1pbmt == 2
                && resp_s2pbmt == 1
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.036 PBMT_s3_22
        // 测试表目标：在allStage：VS 与 G 两阶段组合翻译且翻译使能的命中响应中，将 S1 PBMT 设为2（IO（I/O 内存）），S2 PBMT 设为2（IO（I/O
        // 内存））。观察S1 PBMT 非0，由 S1 覆盖 S2 属性，输出 PBMT 应分类为2（IO（I/O 内存））；分别记录缓存命中与 PTW 旁路的来源。
        cp_04_06_036 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && req_out_s2xlate == 3
                && resp_s1pbmt == 2
                && resp_s2pbmt == 2
                && ((resp_pbmt == expected_pbmt)) === 1'b1) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.037 PBMT_直通强制PMA
        // 测试表目标：场景：translateEnable=0且返回PBMT非0。 观察功能：O.pbmt=0。 测试目的：观察两阶段 PBMT 的选择规则及翻译关闭时的属性处理。
        cp_04_06_037 : coverpoint (resp_pbmt == 0)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && !translate_enable
                && (resp_s1pbmt != 0 || resp_s2pbmt != 0)) {
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_04_06 cg_dtlb_04_06_inst;

    // 04.06 PBMT选取与覆盖
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_diag_04_06】04.06 PBMT选取与覆盖
    // 探测功能：观察两阶段 PBMT 的选择规则及翻译关闭时的属性处理。
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：本组weight=0，cp为观察结果，rs的1/2分别表示偏离/未知；reference_missing表示信号缺失。
    // 测试点：04.06.038；逐点功能见下方注释，精确配值见分解表。
    // 来源仓：b_storage=ptw_bypass_hit为0，b_bypass=1；具体仓是否要求storage_hit由各自iff显式限定。
    // expected_pbmt：参考PBMT：直通为0；onlyS2取S2；双阶段且S2非0时取S2；其余取S1。
    covergroup cg_dtlb_diag_04_06 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        // 04.06.038：结果偏离或未知单列，正常命中不进入诊断仓。
        rs_04_06_038 : coverpoint ($isunknown((resp_pbmt == 3)) ? 2'd2 : ((resp_pbmt == 3)) ? 2'd0 : 2'd1)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && expected_pbmt == 3) {
            bins b_mismatch = {2'd1};
            bins unknown  = {2'd2};
        }
        // 04.06.038 PBMT_reserved
        // 测试表目标：场景：active PBMT=3的响应注入。 观察功能：DTLB选择逻辑会传播3，不在本地自行生成PF；真实PTW应处理保留编码。 测试目的：观察两阶段 PBMT
        // 的选择规则及翻译关闭时的属性处理。
        cp_04_06_038 : coverpoint (resp_pbmt == 3)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && translate_enable
                && expected_pbmt == 3) {
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_diag_04_06 cg_dtlb_diag_04_06_inst;

    // 04.07 PMP交接与内存属性消费
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_04_07】04.07 PMP交接与内存属性消费
    // 探测功能：观察翻译结果向 PMP 和使用者的交接，区分页表权限异常与物理访问权限拒绝。
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：04.07.001, 04.07.002, 04.07.003, 04.07.004, 04.07.005, 04.07.006；逐点功能见下方注释，精确配值见分解表。
    // 异常位序：resp_excp[5:0]={af_st,af_ld,gpf_st,gpf_ld,pf_st,pf_ld}；actual_fault==expected_fault核对类别，读写位掩码额外检查命令方向。
    // actual_fault：从实际输出resp_excp聚合[2:0]={AF,GPF,PF}，各类分别对load/store位做OR。
    covergroup cg_dtlb_04_07 @(posedge clock);
        option.per_instance = 1;
        // 04.07.001 MODE_prefetch_PMP特权
        // 测试表目标：场景：isPrefetch=1并令hyperinst模式与dmode不同。 观察功能：PMP mode取dmode；普通hyper请求PMP mode取spvp。
        // 测试目的：观察翻译结果向 PMP 和使用者的交接，区分页表权限异常与物理访问权限拒绝。
        cp_04_07_001 : coverpoint (resp_pmpmode == csr_dmode)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && req_prefetch_d1
                && req_hyperinst_d1
                && csr_dmode != {1'b0, csr_spvp}) {
            bins b_expected_result = {1'b1};
        }
        // 04.07.002 PMP_翻译地址
        // 测试表目标：场景：RSP成功且translateEnable=1。 观察功能：PMP.addr与paddr副本0一致；cmd通常等于req_out.cmd。
        // 测试目的：观察翻译结果向 PMP 和使用者的交接，区分页表权限异常与物理访问权限拒绝。
        cp_04_07_002 : coverpoint (resp_pmpaddr == resp_paddr_0 && resp_pmpcmd == (req_hlvx_d1?7:req_out_cmd))
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && translate_enable
                && !resp_miss
                && actual_fault == 0) {
            bins b_expected_result = {1'b1};
        }
        // 04.07.003 PMP_直通地址
        // 测试表目标：场景：RSP且translateEnable=0。 观察功能：PMP.addr走直通；no_translate时取响应拍输入pmp_addr。
        // 测试目的：观察翻译结果向 PMP 和使用者的交接，区分页表权限异常与物理访问权限拒绝。
        cp_04_07_003 : coverpoint (resp_pmpaddr == (req_no_translate_d1?req_pmpaddr:req_vaddr_d1[47:0]))
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && !translate_enable) {
            bins b_expected_result = {1'b1};
        }
        // 04.07.004 PMP_noTranslate无resp
        // 测试表目标：场景：上一拍no_translate=1，本拍O.valid=0。 观察功能：LD/PF
        // PMP.valid仍可为1；不能假设PMP.valid恒等于O.valid。 测试目的：观察翻译结果向 PMP 和使用者的交接，区分页表权限异常与物理访问权限拒绝。
        cp_04_07_004 : coverpoint (resp_pmpvalid)
            iff (sample_enable
                && req_no_translate_d1
                && !resp_valid
                && GROUP_ID != 1) {
            bins b_expected_result = {1'b1};
        }
        // 04.07.005 PMP_HLVX命令
        // 测试表目标：场景：RSP对应hlvx=1。 观察功能：PMP.cmd=3b111；其他读写cmd保持原值。 测试目的：观察翻译结果向 PMP
        // 和使用者的交接，区分页表权限异常与物理访问权限拒绝。
        cp_04_07_005 : coverpoint (resp_pmpcmd == 7)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && req_hlvx_d1) {
            bins b_expected_result = {1'b1};
        }
        // 04.07.006 PMP_模式分派
        // 测试表目标：场景：普通请求/Hyper请求/预取请求。 观察功能：分别mode=dmode/spvp/dmode；与实际PMP port对应。 测试目的：观察翻译结果向 PMP
        // 和使用者的交接，区分页表权限异常与物理访问权限拒绝。
        cp_04_07_006 : coverpoint (resp_pmpmode == (req_prefetch_d1?csr_dmode:effective_mode))
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid) {
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_04_07 cg_dtlb_04_07_inst;

    // 05.01 Miss产生与本地处理去向
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_05_01】05.01 Miss产生与本地处理去向
    // 探测功能：跟踪一次本地 miss 最终进入 PTW 请求、重放还是其他分支，不能只依据响应 miss 位推断去向。
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：05.01.001, 05.01.002, 05.01.003, 05.01.004, 05.01.005, 05.01.006；逐点功能见下方注释，精确配值见分解表。
    // 异常位序：resp_excp[5:0]={af_st,af_ld,gpf_st,gpf_ld,pf_st,pf_ld}；actual_fault==expected_fault核对类别，读写位掩码额外检查命令方向。
    // 来源仓：b_storage=ptw_bypass_hit为0，b_bypass=1；具体仓是否要求storage_hit由各自iff显式限定。
    // actual_fault：从实际输出resp_excp聚合[2:0]={AF,GPF,PF}，各类分别对load/store位做OR。
    // expected_fault：参考异常向量[2:0]={AF,GPF,PF}：预异常优先；无hit为0；有hit时AF优先于PF、PF优先于GPF。
    // miss_cause：[4:0]={后拍kill,GPA占用且不匹配,replay,前拍PTW返回,同拍PTW返回}。
    // pre_fault：请求拍 checkfullva 有效，且预检查 PF/GPF/AF 中至少一种成立；均使用_d1快照。
    // return_address_matches：PTW返回按level/NAPOT比较VPN分段；4KB还检查sector valididx或低位tag。
    // return_context_matches：PTW返回与本次响应的stage、ASID/Global、VMID相符。
    covergroup cg_dtlb_05_01 @(posedge clock);
        option.per_instance = 1;
        // 05.01.001 MISS_普通发PTW
        // 测试表目标：场景：RSP有效且miss_read=1，无返回窗口/need_replay/异页need_gpa/req_kill抑制。
        // 观察功能：Q.valid=1，VPN=req_out.vaddr[49:12]，stage与当前请求匹配。 测试目的：跟踪一次本地 miss 最终进入 PTW
        // 请求、重放还是其他分支，不能只依据响应 miss 位推断去向。
        cp_05_01_001 : coverpoint (resp_qvalid && resp_qvpn == req_out_vaddr[49:12] && resp_qstage == req_out_s2xlate)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && resp_miss_read
                && (!(ptw_valid && return_context_matches && return_address_matches))
                && !previous_return_matches
                && !resp_need_replay
                && (!(gpa_ng && !gpa_response_ready && !(gpa_vpn == req_out_vaddr[49:12] && gpa_stage ==
                    req_out_s2xlate)))
                && !req_late_kill) {
            bins b_expected_result = {1'b1};
        }
        // 05.01.002 MISS_多抑制同时发生
        // 测试表目标：场景：req_kill、need_replay、返回窗口两项同时有效。 观察功能：Q仍被抑制、tlbreplay=1；记录优先原因及完整cause mask。
        // 测试目的：跟踪一次本地 miss 最终进入 PTW 请求、重放还是其他分支，不能只依据响应 miss 位推断去向。
        cp_05_01_002 : coverpoint (!resp_qvalid && (GROUP_ID != 0 || PORT_INDEX == 3 || resp_replay))
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && resp_miss_read
                && $countones(miss_cause) >= 2) {
            bins b_expected_result = {1'b1};
        }
        // 05.01.003 MISS_最终miss与fastMiss差异
        // 测试表目标：场景：storage命中GPF而等待GPA。 观察功能：miss可为1，纯无hit意义的fastMiss为0；当前fastMiss端口裁掉则monitor派生。
        // 测试目的：跟踪一次本地 miss 最终进入 PTW 请求、重放还是其他分支，不能只依据响应 miss 位推断去向。
        cp_05_01_003 : coverpoint (resp_miss_read && resp_miss)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && storage_hit
                && resp_has_gpf
                && !ptw_bypass_hit
                && (!(gpa_vpn == req_out_vaddr[49:12] && gpa_stage == req_out_s2xlate))
                && !req_prefetch_d1
                && req_out_s2xlate != 2
                && !resp_last_red) {
            bins b_expected_result = {1'b1};
        }
        // 05.01.005 MISS_命中不发请求
        // 测试表目标：场景：RSP正常storage或bypass hit，无GPA需求、无req_kill。 观察功能：Q.valid=0；tlbreplay正常为0。
        // 测试目的：跟踪一次本地 miss 最终进入 PTW 请求、重放还是其他分支，不能只依据响应 miss 位推断去向。
        cp_05_01_005 : coverpoint (!resp_qvalid)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && !resp_miss_read
                && !pre_fault
                && !req_late_kill) {
            bins b_expected_result = {1'b1};
        }
        // 05.01.006 FCOV_Miss处理去向
        // 测试表目标：场景：req_out_v&&miss_read，记录kill/GPA/返回窗口等cause mask。
        // 观察功能：采本地Q与tlbreplay组合，再采MemBlock过滤后的valid；按场景标注目标去向，00/11保留为观察结果，不以目标组合为iff。 测试目的：跟踪一次本地
        // miss 最终进入 PTW 请求、重放还是其他分支，不能只依据响应 miss 位推断去向。
        cp_05_01_006 : coverpoint (((req_valid_d1 && !req_kill_d1) && resp_valid) && resp_miss_read)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && resp_miss_read) {
            bins b_observed = {1'b1};
        }
        // 05.01.004 MISS_重试生命周期
        // 测试表目标：场景：首次miss→请求接受→返回→同事务重新issue→成功或最终fault。
        // 观察功能：每次attempt统计一次；transaction级记录最终结果/重试次数。 测试目的：跟踪一次本地 miss 最终进入 PTW
        // 请求、重放还是其他分支，不能只依据响应 miss 位推断去向。
        cp_05_01_004 : coverpoint (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd ==
            3'd4) ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
            6'b010101) == 0 : 1'b1))
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && miss_pending
                && req_vaddr_d1 == miss_vaddr
                && req_rob_d1 == miss_rob
                && return_seen
                && !resp_miss) {
            bins b_expected_set = {1'b1};
        }
    endgroup
    cg_dtlb_05_01 cg_dtlb_05_01_inst;

    // 05.02 返回窗口与重复请求过滤
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_05_02】05.02 返回窗口与重复请求过滤
    // 探测功能：观察 PTW 返回前后短时间内的重复 miss 如何被过滤，并跟踪被抑制请求的重试。
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：05.02.001, 05.02.002, 05.02.003, 05.02.004；逐点功能见下方注释，精确配值见分解表。
    // return_address_matches：PTW返回按level/NAPOT比较VPN分段；4KB还检查sector valididx或低位tag。
    // return_context_matches：PTW返回与本次响应的stage、ASID/Global、VMID相符。
    covergroup cg_dtlb_05_02 @(posedge clock);
        option.per_instance = 1;
        // 05.02.001 MISS_当拍返回重试
        // 测试表目标：场景：RSP miss且P本拍匹配该请求的VPN/stage/ASID/VMID。 观察功能：tlbreplay=1，Q.valid=0；下一次访问可命中。
        // 测试目的：观察 PTW 返回前后短时间内的重复 miss 如何被过滤，并跟踪被抑制请求的重试。
        cp_05_02_001 : coverpoint (!resp_qvalid && (GROUP_ID != 0 || PORT_INDEX == 3 || resp_replay))
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && resp_miss_read
                && ptw_valid
                && return_context_matches
                && return_address_matches
                && !req_late_kill) {
            bins b_expected_result = {1'b1};
        }
        // 05.02.002 MISS_前拍返回重试
        // 测试表目标：场景：RSP miss且前拍P匹配，当前无匹配P。 观察功能：tlbreplay=1，Q.valid=0；与同拍bypass时间窗口区分。 测试目的：观察 PTW
        // 返回前后短时间内的重复 miss 如何被过滤，并跟踪被抑制请求的重试。
        cp_05_02_002 : coverpoint (!resp_qvalid && (GROUP_ID != 0 || PORT_INDEX == 3 || resp_replay))
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && resp_miss_read
                && (!(ptw_valid && return_context_matches && return_address_matches))
                && previous_return_matches
                && !req_late_kill) {
            bins b_expected_result = {1'b1};
        }
        // 05.02.003 MISS_返回两拍后
        // 测试表目标：场景：匹配P发生在本次RSP前2拍且无当前/前拍匹配。 观察功能：不因过期ptw_already_back抑制Q；按storage结果决定。 测试目的：观察 PTW
        // 返回前后短时间内的重复 miss 如何被过滤，并跟踪被抑制请求的重试。
        cp_05_02_003 : coverpoint (resp_qvalid || resp_need_replay || (gpa_ng && !gpa_response_ready &&
            !(gpa_vpn == req_out_vaddr[49:12] && gpa_stage == req_out_s2xlate)) || req_late_kill)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && resp_miss_read
                && return_older
                && (!(ptw_valid && return_context_matches && return_address_matches))
                && !previous_return_matches) {
            bins b_expected_result = {1'b1};
        }
        // 05.02.004 MISS_返回上下文不匹配
        // 测试表目标：场景：当拍/前拍P的VPN或ASID或VMID或stage不匹配。 观察功能：不触发本地返回窗口replay，miss应走Q或其他合法抑制。 测试目的：观察 PTW
        // 返回前后短时间内的重复 miss 如何被过滤，并跟踪被抑制请求的重试。
        cp_05_02_004 : coverpoint (resp_qvalid || resp_need_replay || (gpa_ng && !gpa_response_ready &&
            !(gpa_vpn == req_out_vaddr[49:12] && gpa_stage == req_out_s2xlate)) || req_late_kill)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && resp_miss_read
                && (ptw_valid || ptw_valid_d1)
                && (!(ptw_valid && return_context_matches && return_address_matches))
                && !previous_return_matches) {
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_05_02 cg_dtlb_05_02_inst;

    // 06.03 PTW旁路与读写冲突
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_06_03】06.03 PTW旁路与读写冲突
    // 探测功能：观察新 PTW 返回直接旁路与缓存读写之间的优先级，特别是读到正在被替换的 way。
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：06.03.002, 06.03.003, 06.03.004, 06.03.008；逐点功能见下方注释，精确配值见分解表。
    // 来源仓：b_storage=ptw_bypass_hit为0，b_bypass=1；具体仓是否要求storage_hit由各自iff显式限定。
    // expected_paddr：参考PA：翻译时拼接resp_ppn与请求页内偏移；直通按no_translate选择PMP地址或请求VA。
    covergroup cg_dtlb_06_03 @(posedge clock);
        option.per_instance = 1;
        // 06.03.002 BYPASS_覆盖旧storage
        // 测试表目标：场景：storage旧entry命中且匹配新P返回，payload设为可辨识。
        // 观察功能：p_hit优先于e_hit，选新返回属性；变更映射需合法fence或诊断注入。 测试目的：观察新 PTW 返回直接旁路与缓存读写之间的优先级，特别是读到正在被替换的
        // way。
        cp_06_03_002 : coverpoint (resp_paddr_0 == expected_paddr)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && storage_hit
                && ptw_bypass_hit) {
            bins b_expected_set = {1'b1};
        }
        // 06.03.003 BYPASS_不匹配返回
        // 测试表目标：场景：P.valid但VPN/ASID/VMID/s2xlate任一不匹配。 观察功能：不得产生p_hit；storage命中则保持storage结果。
        // 测试目的：观察新 PTW 返回直接旁路与缓存读写之间的优先级，特别是读到正在被替换的 way。
        cp_06_03_003 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && ptw_valid_d1
                && !bypass_fast_d1
                && (!ptw_bypass_hit) === 1'b1) {
            bins b_expected_clear = {1'b0};
        }
        // 06.03.004 BYPASS_refill受抑仍旁路
        // 测试表目标：场景：匹配P与REQ同拍，getGpa或need_gpa使W.valid=0。 观察功能：按p_hit_fast逻辑可旁路；不把W.valid当旁路使能。
        // 测试目的：观察新 PTW 返回直接旁路与缓存读写之间的优先级，特别是读到正在被替换的 way。
        cp_06_03_004 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && bypass_fast_d1
                && (ptw_getgpa_d1 || gpa_ng_d1)) {
            bins b_expected_set = {1'b1};
        }
        // 06.03.008 PTW回复无效_不形成下一拍旁路
        // 测试表目标：场景：有效请求拍P.valid=0，不要求storage一定命中或miss。 观察功能：该请求不因本拍无效P
        // payload建立ph；下一拍ph=0，storage命中/普通miss及pre-fault分别分类。 测试目的：观察新 PTW
        // 返回直接旁路与缓存读写之间的优先级，特别是读到正在被替换的 way。
        cp_06_03_008 : coverpoint ptw_bypass_hit
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && !ptw_valid_d1
                && (!ptw_bypass_hit) === 1'b1) {
            bins b_expected_clear = {1'b0};
        }
    endgroup
    cg_dtlb_06_03 cg_dtlb_06_03_inst;

    // 06.05 旁路PPN与GVPN拼接分支
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_06_05】06.05 旁路PPN与GVPN拼接分支
    // 探测功能：观察 PTW 旁路直接生成物理页号/GPA 页号的过程，覆盖基础页、超级页和 NAPOT 拼接分支。
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：06.05.001, 06.05.002, 06.05.003, 06.05.004, 06.05.005, 06.05.006, 06.05.007,
    // 06.05.008, 06.05.009, 06.05.010, 06.05.011, 06.05.012, 06.05.013, 06.05.014, 06.05.015,
    // 06.05.016, 06.05.017, 06.05.018, 06.05.019, 06.05.020；逐点功能见下方注释，精确配值见分解表。
    // 来源仓：b_storage=ptw_bypass_hit为0，b_bypass=1；具体仓是否要求storage_hit由各自iff显式限定。
    // expected_paddr：参考PA：翻译时拼接resp_ppn与请求页内偏移；直通按no_translate选择PMP地址或请求VA。
    covergroup cg_dtlb_06_05 @(posedge clock);
        option.per_instance = 1;
        // 06.05.001 PTW旁路PPN_stage0_level0_n0
        // 测试表目标：在noS2：非虚拟化 Host 路径，翻译启用时使用 satp 的 S 阶段下，让当前请求与同拍 PTW
        // 返回匹配，下一拍直接使用返回的S1页号生成物理地址。返回配置 level=0、n=0，表示4KB 基础页。4KB 分支采用返回的物理页号；S1 sector 还要按子页号选择
        // ppn_low。最终记录48位物理地址和原请求的12位页内偏移，避免误用响应当拍的新请求地址。
        cp_06_05_001 : coverpoint (resp_paddr_0 == expected_paddr)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && ptw_bypass_hit
                && req_stage_d1 == 0
                && ptw_stage_d1 == 0
                && ptw_s1level_d1 == 0
                && ptw_s1n_d1 == 0) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.002 PTW旁路PPN_stage0_level0_n1
        // 测试表目标：在noS2：非虚拟化 Host 路径，翻译启用时使用 satp 的 S 阶段下，让当前请求与同拍 PTW
        // 返回匹配，下一拍直接使用返回的S1页号生成物理地址。返回配置 level=0、n=1，表示64KB NAPOT 页。NAPOT 分支取翻译输入页号的低4位拼接到
        // PPN。最终记录48位物理地址和原请求的12位页内偏移，避免误用响应当拍的新请求地址。
        cp_06_05_002 : coverpoint (resp_paddr_0 == expected_paddr)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && ptw_bypass_hit
                && req_stage_d1 == 0
                && ptw_stage_d1 == 0
                && ptw_s1level_d1 == 0
                && ptw_s1n_d1 == 1) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.003 PTW旁路PPN_stage0_level1_n0
        // 测试表目标：在noS2：非虚拟化 Host 路径，翻译启用时使用 satp 的 S 阶段下，让当前请求与同拍 PTW
        // 返回匹配，下一拍直接使用返回的S1页号生成物理地址。返回配置 level=1、n=0，表示2MB
        // 超级页。超级页分支保留翻译输入页号的低9位。最终记录48位物理地址和原请求的12位页内偏移，避免误用响应当拍的新请求地址。
        cp_06_05_003 : coverpoint (resp_paddr_0 == expected_paddr)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && ptw_bypass_hit
                && req_stage_d1 == 0
                && ptw_stage_d1 == 0
                && ptw_s1level_d1 == 1
                && ptw_s1n_d1 == 0) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.004 PTW旁路PPN_stage0_level2_n0
        // 测试表目标：在noS2：非虚拟化 Host 路径，翻译启用时使用 satp 的 S 阶段下，让当前请求与同拍 PTW
        // 返回匹配，下一拍直接使用返回的S1页号生成物理地址。返回配置 level=2、n=0，表示1GB
        // 超级页。超级页分支保留翻译输入页号的低18位。最终记录48位物理地址和原请求的12位页内偏移，避免误用响应当拍的新请求地址。
        cp_06_05_004 : coverpoint (resp_paddr_0 == expected_paddr)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && ptw_bypass_hit
                && req_stage_d1 == 0
                && ptw_stage_d1 == 0
                && ptw_s1level_d1 == 2
                && ptw_s1n_d1 == 0) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.005 PTW旁路PPN_stage0_level3_n0
        // 测试表目标：在noS2：非虚拟化 Host 路径，翻译启用时使用 satp 的 S 阶段下，让当前请求与同拍 PTW
        // 返回匹配，下一拍直接使用返回的S1页号生成物理地址。返回配置 level=3、n=0，表示512GB 超级页（合法 Sv48
        // 场景）。超级页分支保留翻译输入页号的低27位。最终记录48位物理地址和原请求的12位页内偏移，避免误用响应当拍的新请求地址。
        cp_06_05_005 : coverpoint (resp_paddr_0 == expected_paddr)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && ptw_bypass_hit
                && req_stage_d1 == 0
                && ptw_stage_d1 == 0
                && ptw_s1level_d1 == 3
                && ptw_s1n_d1 == 0) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.006 PTW旁路PPN_stage1_level0_n0
        // 测试表目标：在onlyStage1：只执行 Guest 的 VS 阶段，G 阶段 Bare下，让当前请求与同拍 PTW
        // 返回匹配，下一拍直接使用返回的S1页号生成物理地址。返回配置 level=0、n=0，表示4KB 基础页。4KB 分支采用返回的物理页号；S1 sector 还要按子页号选择
        // ppn_low。最终记录48位物理地址和原请求的12位页内偏移，避免误用响应当拍的新请求地址。
        cp_06_05_006 : coverpoint (resp_paddr_0 == expected_paddr)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && ptw_bypass_hit
                && req_stage_d1 == 1
                && ptw_stage_d1 == 1
                && ptw_s1level_d1 == 0
                && ptw_s1n_d1 == 0) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.007 PTW旁路PPN_stage1_level0_n1
        // 测试表目标：在onlyStage1：只执行 Guest 的 VS 阶段，G 阶段 Bare下，让当前请求与同拍 PTW
        // 返回匹配，下一拍直接使用返回的S1页号生成物理地址。返回配置 level=0、n=1，表示64KB NAPOT 页。NAPOT 分支取翻译输入页号的低4位拼接到
        // PPN。最终记录48位物理地址和原请求的12位页内偏移，避免误用响应当拍的新请求地址。
        cp_06_05_007 : coverpoint (resp_paddr_0 == expected_paddr)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && ptw_bypass_hit
                && req_stage_d1 == 1
                && ptw_stage_d1 == 1
                && ptw_s1level_d1 == 0
                && ptw_s1n_d1 == 1) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.008 PTW旁路PPN_stage1_level1_n0
        // 测试表目标：在onlyStage1：只执行 Guest 的 VS 阶段，G 阶段 Bare下，让当前请求与同拍 PTW
        // 返回匹配，下一拍直接使用返回的S1页号生成物理地址。返回配置 level=1、n=0，表示2MB
        // 超级页。超级页分支保留翻译输入页号的低9位。最终记录48位物理地址和原请求的12位页内偏移，避免误用响应当拍的新请求地址。
        cp_06_05_008 : coverpoint (resp_paddr_0 == expected_paddr)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && ptw_bypass_hit
                && req_stage_d1 == 1
                && ptw_stage_d1 == 1
                && ptw_s1level_d1 == 1
                && ptw_s1n_d1 == 0) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.009 PTW旁路PPN_stage1_level2_n0
        // 测试表目标：在onlyStage1：只执行 Guest 的 VS 阶段，G 阶段 Bare下，让当前请求与同拍 PTW
        // 返回匹配，下一拍直接使用返回的S1页号生成物理地址。返回配置 level=2、n=0，表示1GB
        // 超级页。超级页分支保留翻译输入页号的低18位。最终记录48位物理地址和原请求的12位页内偏移，避免误用响应当拍的新请求地址。
        cp_06_05_009 : coverpoint (resp_paddr_0 == expected_paddr)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && ptw_bypass_hit
                && req_stage_d1 == 1
                && ptw_stage_d1 == 1
                && ptw_s1level_d1 == 2
                && ptw_s1n_d1 == 0) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.010 PTW旁路PPN_stage1_level3_n0
        // 测试表目标：在onlyStage1：只执行 Guest 的 VS 阶段，G 阶段 Bare下，让当前请求与同拍 PTW
        // 返回匹配，下一拍直接使用返回的S1页号生成物理地址。返回配置 level=3、n=0，表示512GB 超级页（合法 Sv48
        // 场景）。超级页分支保留翻译输入页号的低27位。最终记录48位物理地址和原请求的12位页内偏移，避免误用响应当拍的新请求地址。
        cp_06_05_010 : coverpoint (resp_paddr_0 == expected_paddr)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && ptw_bypass_hit
                && req_stage_d1 == 1
                && ptw_stage_d1 == 1
                && ptw_s1level_d1 == 3
                && ptw_s1n_d1 == 0) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.011 PTW旁路PPN_stage2_level0_n0
        // 测试表目标：在onlyStage2：VS 阶段 Bare，只执行 G 阶段；是否实际翻译还需看 TE下，让当前请求与同拍 PTW
        // 返回匹配，下一拍直接使用返回的S2页号生成物理地址。返回配置 level=0、n=0，表示4KB 基础页。4KB 分支采用返回的物理页号；S1 sector 还要按子页号选择
        // ppn_low。最终记录48位物理地址和原请求的12位页内偏移，避免误用响应当拍的新请求地址。
        cp_06_05_011 : coverpoint (resp_paddr_0 == expected_paddr)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && ptw_bypass_hit
                && req_stage_d1 == 2
                && ptw_stage_d1 == 2
                && ptw_s2level_d1 == 0
                && ptw_s2n_d1 == 0) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.012 PTW旁路PPN_stage2_level0_n1
        // 测试表目标：在onlyStage2：VS 阶段 Bare，只执行 G 阶段；是否实际翻译还需看 TE下，让当前请求与同拍 PTW
        // 返回匹配，下一拍直接使用返回的S2页号生成物理地址。返回配置 level=0、n=1，表示64KB NAPOT 页。NAPOT 分支取翻译输入页号的低4位拼接到
        // PPN。最终记录48位物理地址和原请求的12位页内偏移，避免误用响应当拍的新请求地址。
        cp_06_05_012 : coverpoint (resp_paddr_0 == expected_paddr)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && ptw_bypass_hit
                && req_stage_d1 == 2
                && ptw_stage_d1 == 2
                && ptw_s2level_d1 == 0
                && ptw_s2n_d1 == 1) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.013 PTW旁路PPN_stage2_level1_n0
        // 测试表目标：在onlyStage2：VS 阶段 Bare，只执行 G 阶段；是否实际翻译还需看 TE下，让当前请求与同拍 PTW
        // 返回匹配，下一拍直接使用返回的S2页号生成物理地址。返回配置 level=1、n=0，表示2MB
        // 超级页。超级页分支保留翻译输入页号的低9位。最终记录48位物理地址和原请求的12位页内偏移，避免误用响应当拍的新请求地址。
        cp_06_05_013 : coverpoint (resp_paddr_0 == expected_paddr)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && ptw_bypass_hit
                && req_stage_d1 == 2
                && ptw_stage_d1 == 2
                && ptw_s2level_d1 == 1
                && ptw_s2n_d1 == 0) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.014 PTW旁路PPN_stage2_level2_n0
        // 测试表目标：在onlyStage2：VS 阶段 Bare，只执行 G 阶段；是否实际翻译还需看 TE下，让当前请求与同拍 PTW
        // 返回匹配，下一拍直接使用返回的S2页号生成物理地址。返回配置 level=2、n=0，表示1GB
        // 超级页。超级页分支保留翻译输入页号的低18位。最终记录48位物理地址和原请求的12位页内偏移，避免误用响应当拍的新请求地址。
        cp_06_05_014 : coverpoint (resp_paddr_0 == expected_paddr)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && ptw_bypass_hit
                && req_stage_d1 == 2
                && ptw_stage_d1 == 2
                && ptw_s2level_d1 == 2
                && ptw_s2n_d1 == 0) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.015 PTW旁路PPN_stage2_level3_n0
        // 测试表目标：在onlyStage2：VS 阶段 Bare，只执行 G 阶段；是否实际翻译还需看 TE下，让当前请求与同拍 PTW
        // 返回匹配，下一拍直接使用返回的S2页号生成物理地址。返回配置 level=3、n=0，表示512GB 超级页（合法 Sv48
        // 场景）。超级页分支保留翻译输入页号的低27位。最终记录48位物理地址和原请求的12位页内偏移，避免误用响应当拍的新请求地址。
        cp_06_05_015 : coverpoint (resp_paddr_0 == expected_paddr)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && ptw_bypass_hit
                && req_stage_d1 == 2
                && ptw_stage_d1 == 2
                && ptw_s2level_d1 == 3
                && ptw_s2n_d1 == 0) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.016 PTW旁路PPN_stage3_level0_n0
        // 测试表目标：在allStage：VS 与 G 两阶段组合翻译下，让当前请求与同拍 PTW 返回匹配，下一拍直接使用返回的S2页号生成物理地址。返回配置
        // level=0、n=0，表示4KB 基础页。allStage 先由 S1 生成中间 GPA 页号，再取其低36位作为 S2 拼接输入。4KB 分支采用返回的物理页号；S1
        // sector 还要按子页号选择 ppn_low。最终记录48位物理地址和原请求的12位页内偏移，避免误用响应当拍的新请求地址。
        cp_06_05_016 : coverpoint (resp_paddr_0 == expected_paddr)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && ptw_bypass_hit
                && req_stage_d1 == 3
                && ptw_stage_d1 == 3
                && ptw_s2level_d1 == 0
                && ptw_s2n_d1 == 0) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.017 PTW旁路PPN_stage3_level0_n1
        // 测试表目标：在allStage：VS 与 G 两阶段组合翻译下，让当前请求与同拍 PTW 返回匹配，下一拍直接使用返回的S2页号生成物理地址。返回配置
        // level=0、n=1，表示64KB NAPOT 页。allStage 先由 S1 生成中间 GPA 页号，再取其低36位作为 S2 拼接输入。NAPOT
        // 分支取翻译输入页号的低4位拼接到 PPN。最终记录48位物理地址和原请求的12位页内偏移，避免误用响应当拍的新请求地址。
        cp_06_05_017 : coverpoint (resp_paddr_0 == expected_paddr)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && ptw_bypass_hit
                && req_stage_d1 == 3
                && ptw_stage_d1 == 3
                && ptw_s2level_d1 == 0
                && ptw_s2n_d1 == 1) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.018 PTW旁路PPN_stage3_level1_n0
        // 测试表目标：在allStage：VS 与 G 两阶段组合翻译下，让当前请求与同拍 PTW 返回匹配，下一拍直接使用返回的S2页号生成物理地址。返回配置
        // level=1、n=0，表示2MB 超级页。allStage 先由 S1 生成中间 GPA 页号，再取其低36位作为 S2
        // 拼接输入。超级页分支保留翻译输入页号的低9位。最终记录48位物理地址和原请求的12位页内偏移，避免误用响应当拍的新请求地址。
        cp_06_05_018 : coverpoint (resp_paddr_0 == expected_paddr)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && ptw_bypass_hit
                && req_stage_d1 == 3
                && ptw_stage_d1 == 3
                && ptw_s2level_d1 == 1
                && ptw_s2n_d1 == 0) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.019 PTW旁路PPN_stage3_level2_n0
        // 测试表目标：在allStage：VS 与 G 两阶段组合翻译下，让当前请求与同拍 PTW 返回匹配，下一拍直接使用返回的S2页号生成物理地址。返回配置
        // level=2、n=0，表示1GB 超级页。allStage 先由 S1 生成中间 GPA 页号，再取其低36位作为 S2
        // 拼接输入。超级页分支保留翻译输入页号的低18位。最终记录48位物理地址和原请求的12位页内偏移，避免误用响应当拍的新请求地址。
        cp_06_05_019 : coverpoint (resp_paddr_0 == expected_paddr)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && ptw_bypass_hit
                && req_stage_d1 == 3
                && ptw_stage_d1 == 3
                && ptw_s2level_d1 == 2
                && ptw_s2n_d1 == 0) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.020 PTW旁路PPN_stage3_level3_n0
        // 测试表目标：在allStage：VS 与 G 两阶段组合翻译下，让当前请求与同拍 PTW 返回匹配，下一拍直接使用返回的S2页号生成物理地址。返回配置
        // level=3、n=0，表示512GB 超级页（合法 Sv48 场景）。allStage 先由 S1 生成中间 GPA 页号，再取其低36位作为 S2
        // 拼接输入。超级页分支保留翻译输入页号的低27位。最终记录48位物理地址和原请求的12位页内偏移，避免误用响应当拍的新请求地址。
        cp_06_05_020 : coverpoint (resp_paddr_0 == expected_paddr)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && ptw_bypass_hit
                && req_stage_d1 == 3
                && ptw_stage_d1 == 3
                && ptw_s2level_d1 == 3
                && ptw_s2n_d1 == 0) {
            bins b_expected_set = {1'b1};
        }
    endgroup
    cg_dtlb_06_05 cg_dtlb_06_05_inst;

    // 07.01 GPA启动资格与启动抑制
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_07_01】07.01 GPA启动资格与启动抑制
    // 探测功能：观察 GPF 出现后能否启动补取 GPA，区分 ROB 资格、取消、预取和已有等待者的限制。
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：07.01.001, 07.01.002, 07.01.003, 07.01.005, 07.01.006, 07.01.007, 07.01.008,
    // 07.01.009, 07.01.010, 07.01.011；逐点功能见下方注释，精确配值见分解表。
    // 来源仓：b_storage=ptw_bypass_hit为0，b_bypass=1；具体仓是否要求storage_hit由各自iff显式限定。
    // gpa_clear_event：GPA清除事件或计数到511，表示等待流程退出。
    // gpa_start：GPA启动参考条件：有效响应有GPF，非旁路/非onlyS2，无匹配GPA结果，无kill/prefetch/redirect抑制。
    // pre_fault：请求拍 checkfullva 有效，且预检查 PF/GPF/AF 中至少一种成立；均使用_d1快照。
    covergroup cg_dtlb_07_01 @(posedge clock);
        option.per_instance = 1;
        // 07.01.003 GPA_非ROB头重放
        // 测试表目标：场景：同GPF条件，ROB!=pendingPtr，frm_mabuf=0，非exec。
        // 观察功能：need_replay=1，tlbreplay=1，不发本次PTW；等待可取GPA时重试。 测试目的：观察 GPF 出现后能否启动补取 GPA，区分 ROB
        // 资格、取消、预取和已有等待者的限制。
        cp_07_01_003 : coverpoint ((resp_need_replay && !resp_qvalid && (GROUP_ID != 0 || PORT_INDEX == 3 ||
            resp_replay)))
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && gpa_start
                && !resp_can_gpa
                && !req_mabuf_d1
                && !gpa_clear_event) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.01.005 GPA_不新建_prefetch
        // 测试表目标：场景：need_gpa=0，GPF潜在触发，isPrefetch=1。
        // 观察功能：不为该请求建立新的need_gpa；分别观察miss/tlbreplay/最终fault；预取异常输出及consumer丢弃同时分类，不能强制gpaddr有效。
        // 测试目的：观察 GPF 出现后能否启动补取 GPA，区分 ROB 资格、取消、预取和已有等待者的限制。
        cp_07_01_005 : coverpoint (!gpa_ng_wire)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (storage_hit || ptw_bypass_hit)
                && resp_has_gpf
                && req_prefetch_d1
                && !gpa_ng) {
            bins b_expected_result = {1'b1};
        }
        // 07.01.006 GPA_不新建_bypass
        // 测试表目标：场景：need_gpa=0，GPF潜在触发，p_hit=1。 观察功能：不为该请求建立新的need_gpa；分别观察miss/tlbreplay/最终fault。
        // 测试目的：观察 GPF 出现后能否启动补取 GPA，区分 ROB 资格、取消、预取和已有等待者的限制。
        cp_07_01_006 : coverpoint (!gpa_ng_wire)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && resp_has_gpf
                && ptw_bypass_hit
                && !gpa_ng) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.01.007 GPA_不新建_已有匹配GPA
        // 测试表目标：场景：已有GPA快照且need_gpa=0，匹配VPN/stage的后续GPF重算；首次need_gpa=1消费归H-019。
        // 观察功能：不为该请求建立新的need_gpa；分别观察miss/tlbreplay/最终fault。 测试目的：观察 GPF 出现后能否启动补取 GPA，区分 ROB
        // 资格、取消、预取和已有等待者的限制。
        cp_07_01_007 : coverpoint (!gpa_ng_wire)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && resp_has_gpf
                && gpa_response_ready
                && gpa_vpn == req_out_vaddr[49:12]
                && gpa_stage == req_out_s2xlate
                && !gpa_ng) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.01.008 GPA_不新建_当前redirect
        // 测试表目标：场景：need_gpa=0，GPF潜在触发，当前redirect使请求ROB需要flush。
        // 观察功能：不为该请求建立新的need_gpa；分别观察miss/tlbreplay/最终fault。 测试目的：观察 GPF 出现后能否启动补取 GPA，区分 ROB
        // 资格、取消、预取和已有等待者的限制。
        cp_07_01_008 : coverpoint (!gpa_ng_wire)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && resp_has_gpf
                && redirect_valid
                && (((req_rob_d1[8] ^ redirect_rob[8]) ^ (req_rob_d1[7:0] > redirect_rob[7:0])) || (redirect_level
                    && req_rob_d1 == redirect_rob))
                && !gpa_ng) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.01.009 GPA_不新建_前拍redirect
        // 测试表目标：场景：need_gpa=0，GPF潜在触发，前拍redirect使请求ROB需要flush。
        // 观察功能：不为该请求建立新的need_gpa；分别观察miss/tlbreplay/最终fault。 测试目的：观察 GPF 出现后能否启动补取 GPA，区分 ROB
        // 资格、取消、预取和已有等待者的限制。
        cp_07_01_009 : coverpoint (!gpa_ng_wire)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && resp_has_gpf
                && resp_last_red
                && !gpa_ng) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.01.010 GPA_不新建_req_kill
        // 测试表目标：场景：need_gpa=0，GPF潜在触发，req_kill=1。
        // 观察功能：不为该请求建立新的need_gpa；分别观察miss/tlbreplay/最终fault。 测试目的：观察 GPF 出现后能否启动补取 GPA，区分 ROB
        // 资格、取消、预取和已有等待者的限制。
        cp_07_01_010 : coverpoint (!gpa_ng_wire && !resp_qvalid)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && resp_has_gpf
                && req_late_kill
                && !gpa_ng) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.01.011 GPA_正常miss非getGpa
        // 测试表目标：场景：storage和bypass都miss，无可确定GPF命中。 观察功能：Q.getGpa=0；不能把所有虚拟化miss都当补取GPA。 测试目的：观察 GPF
        // 出现后能否启动补取 GPA，区分 ROB 资格、取消、预取和已有等待者的限制。
        cp_07_01_011 : coverpoint (!resp_qgpa)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (!(storage_hit || ptw_bypass_hit))
                && translate_enable
                && !pre_fault) {
            bins b_expected_result = {1'b1};
        }
        // 07.01.001 GPA_进入ROB头
        // 测试表目标：场景：RSP有效，storage命中GPF，非onlyS2，非预取，p_hit=0，无匹配resp_gpa_refill，无kill/当前或前拍redirect；ROB==robPendingPtr，need_gpa=0。
        // 观察功能：need_gpa置1并保存VPN/stage/ROB；need_gpa_wire阻止同拍普通refill；Q.getGpa=1。 测试目的：观察 GPF
        // 出现后能否启动补取 GPA，区分 ROB 资格、取消、预取和已有等待者的限制。
        cp_07_01_001 : coverpoint (gpa_ng_wire && resp_qgpa)
            iff (sample_enable
                && gpa_start
                && resp_can_gpa
                && req_rob_d1 == rob_head) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.01.002 GPA_进入MABUF
        // 测试表目标：场景：同进入条件，但ROB!=pendingPtr且frm_mabuf=1。 观察功能：允许建立need_gpa状态。 测试目的：观察 GPF 出现后能否启动补取
        // GPA，区分 ROB 资格、取消、预取和已有等待者的限制。
        cp_07_01_002 : coverpoint (gpa_ng_wire && resp_qgpa)
            iff (sample_enable
                && gpa_start
                && resp_can_gpa
                && req_rob_d1 != rob_head
                && req_mabuf_d1) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_07_01 cg_dtlb_07_01_inst;

    // 07.02 等待请求与返回匹配
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_07_02】07.02 等待请求与返回匹配
    // 探测功能：观察共享 GPA 等待状态下请求与返回的 VPN/阶段匹配，以及不匹配事件的处理。
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：07.02.001, 07.02.002, 07.02.003, 07.02.004, 07.02.005, 07.02.006, 07.02.007,
    // 07.02.008, 07.02.009；逐点功能见下方注释，精确配值见分解表。
    // gpa_clear_event：GPA清除事件或计数到511，表示等待流程退出。
    // pre_fault：请求拍 checkfullva 有效，且预检查 PF/GPF/AF 中至少一种成立；均使用_d1快照。
    // return_address_matches：PTW返回按level/NAPOT比较VPN分段；4KB还检查sector valididx或低位tag。
    // return_context_matches：PTW返回与本次响应的stage、ASID/Global、VMID相符。
    covergroup cg_dtlb_07_02 @(posedge clock);
        option.per_instance = 1;
        // 07.02.001 GPA_等待期间同页
        // 测试表目标：场景：need_gpa=1,resp_gpa_refill=0；请求VPN/stage匹配。
        // 观察功能：允许匹配的getGpa请求继续送filter；重复可由filter合并。 测试目的：观察共享 GPA 等待状态下请求与返回的 VPN/阶段匹配，以及不匹配事件的处理。
        cp_07_02_001 : coverpoint ((resp_qvalid || (ptw_valid && return_context_matches &&
            return_address_matches) || previous_return_matches ||
            resp_need_replay || req_late_kill))
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && resp_miss_read
                && gpa_ng
                && !gpa_response_ready
                && gpa_vpn == req_out_vaddr[49:12]
                && gpa_stage == req_out_s2xlate) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.02.002 GPA_等待期间异页
        // 测试表目标：场景：need_gpa=1,resp_gpa_refill=0；请求VPN或stage不匹配且miss。 观察功能：tlbreplay=1，Q.valid=0。
        // 测试目的：观察共享 GPA 等待状态下请求与返回的 VPN/阶段匹配，以及不匹配事件的处理。
        cp_07_02_002 : coverpoint (!resp_qvalid && (GROUP_ID != 0 || PORT_INDEX == 3 || resp_replay))
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && resp_miss_read
                && gpa_ng
                && !gpa_response_ready
                && (!(gpa_vpn == req_out_vaddr[49:12] && gpa_stage == req_out_s2xlate))) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.02.003 GPA_等待期间普通hit
        // 测试表目标：场景：need_gpa=1；另一个请求正常storage hit无GPF。 观察功能：正常请求仍可响应；不把状态机误建成整个TLB阻塞。 测试目的：观察共享 GPA
        // 等待状态下请求与返回的 VPN/阶段匹配，以及不匹配事件的处理。
        cp_07_02_003 : coverpoint (!resp_miss && !resp_qvalid)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && gpa_ng
                && storage_hit
                && !resp_has_gpf
                && !pre_fault) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.02.007 GPA_已取回时异页请求
        // 测试表目标：场景：need_gpa=1,resp_gpa_refill=1；新请求VPN/stage不匹配且miss。
        // 观察功能：不再被!resp_gpa_refill条件阻挡；观察可发PTW行为。 测试目的：观察共享 GPA 等待状态下请求与返回的 VPN/阶段匹配，以及不匹配事件的处理。
        cp_07_02_007 : coverpoint (resp_qvalid)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && resp_miss_read
                && gpa_ng
                && gpa_response_ready
                && (!(gpa_vpn == req_out_vaddr[49:12] && gpa_stage == req_out_s2xlate))
                && (!(ptw_valid && return_context_matches && return_address_matches))
                && !previous_return_matches
                && !resp_need_replay) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.02.008 GPA_重取后同VPN换stage
        // 测试表目标：场景：GPA已缓存，下一请求VPN相同但stage不同。 观察功能：不得消费旧stage GPA；match需VPN与s2xlate同时一致。 测试目的：观察共享
        // GPA 等待状态下请求与返回的 VPN/阶段匹配，以及不匹配事件的处理。
        cp_07_02_008 : coverpoint (!(gpa_vpn == req_out_vaddr[49:12] && gpa_stage == req_out_s2xlate))
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && gpa_response_ready
                && gpa_vpn == req_out_vaddr[49:12]
                && gpa_stage != req_out_s2xlate) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.02.004 GPA_匹配返回
        // 测试表目标：场景：need_gpa=1,P.valid=1，P.s2xlate相等且P.getVpn(need_gpa_vpn)==need_gpa_vpn；没有同拍更高优先clear/start。
        // 观察功能：resp_gpa_refill=1，保存GPA GVPN、S1 level/leaf/fake；普通entry不写入。 测试目的：观察共享 GPA
        // 等待状态下请求与返回的 VPN/阶段匹配，以及不匹配事件的处理。
        cp_07_02_004 : coverpoint gpa_response_ready
            iff (sample_enable
                && ptw_valid_d1
                && gpa_ng_d1
                && ptw_stage_d1 == gpa_stage_d1
                && !gpa_clear_event) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_set = {1'b1};
        }
        // 07.02.005 GPA_匹配返回getGpa位非门控
        // 测试表目标：场景：need_gpa=1，返回VPN/stage匹配，分别P.getGpa=0/1。
        // 观察功能：两种均可更新resp_gpa_refill；只有getGpa=1推动need_gpa_count。 测试目的：观察共享 GPA 等待状态下请求与返回的
        // VPN/阶段匹配，以及不匹配事件的处理。
        cp_07_02_005 : coverpoint ptw_getgpa_d1
            iff (sample_enable
                && ptw_valid_d1
                && gpa_ng_d1
                && ptw_stage_d1 == gpa_stage_d1
                && !gpa_clear_event
                && (gpa_response_ready) === 1'b1) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_ordinary_return = {1'b0};
            bins b_gpa_return = {1'b1};
        }
        // 07.02.006 GPA_不匹配返回
        // 测试表目标：场景：need_gpa=1，P.valid且VPN或stage不匹配。
        // 观察功能：没有更高优先事件时，resp_gpa_refill及已保存GPA保持，不会被本次不匹配返回更新；不能强制原本为1的resp_gpa_refill变0。getGpa返回仍可推动计数。
        // 测试目的：观察共享 GPA 等待状态下请求与返回的 VPN/阶段匹配，以及不匹配事件的处理。
        cp_07_02_006 : coverpoint (gpa_response_ready == gpa_ready_d1)
            iff (sample_enable
                && ptw_valid_d1
                && gpa_ng_d1
                && ptw_stage_d1 != gpa_stage_d1
                && !gpa_clear_event) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.02.009 GPA_getVpn范围匹配
        // 测试表目标：场景：need_gpa等待VPN处于返回大页或NAPOT中、与原返回tag低位不同。
        // 观察功能：本地用getVpn重构后比较，合法范围内可匹配；范围外不得更新resp_gpa_refill。 测试目的：观察共享 GPA 等待状态下请求与返回的
        // VPN/阶段匹配，以及不匹配事件的处理。
        cp_07_02_009 : coverpoint gpa_response_ready
            iff (sample_enable
                && ptw_valid_d1
                && gpa_ng_d1
                && ptw_stage_d1 == gpa_stage_d1
                && gpa_vpn_d1 != {ptw_s1tag_d1, ptw_s1low_d1}) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_set = {1'b1};
        }
    endgroup
    cg_dtlb_07_02 cg_dtlb_07_02_inst;

    // 07.03 GPA消费、重定向与退出计数
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_07_03】07.03 GPA消费、重定向与退出计数
    // 探测功能：观察 GPA 等待者何时完成或退出，以及计数边界、重定向和标识回绕的作用。
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：07.03.001, 07.03.002, 07.03.003, 07.03.004, 07.03.005, 07.03.006, 07.03.007,
    // 07.03.008；逐点功能见下方注释，精确配值见分解表。
    // 异常位序：resp_excp[5:0]={af_st,af_ld,gpf_st,gpf_ld,pf_st,pf_ld}；actual_fault==expected_fault核对类别，读写位掩码额外检查命令方向。
    // actual_fault：从实际输出resp_excp聚合[2:0]={AF,GPF,PF}，各类分别对load/store位做OR。
    // expected_fault：参考异常向量[2:0]={AF,GPF,PF}：预异常优先；无hit为0；有hit时AF优先于PF、PF优先于GPF。
    // gpa_clear_event：GPA清除事件或计数到511，表示等待流程退出。
    covergroup cg_dtlb_07_03 @(posedge clock);
        option.per_instance = 1;
        // 07.03.008 GPA_owner标识复用
        // 测试表目标：场景：need_gpa保存ROB flag/value，ROB指针环回且新请求数值相同flag不同。
        // 观察功能：redirect和owner资格比较必须含flag；VPN/stage匹配不代表ROB归属相同。 测试目的：观察 GPA
        // 等待者何时完成或退出，以及计数边界、重定向和标识回绕的作用。
        cp_07_03_008 : coverpoint (req_rob_d1 != gpa_owner)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && gpa_owner_valid
                && req_rob_d1[7:0] == gpa_owner[7:0]
                && req_rob_d1[8] != gpa_owner[8]) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.03.001 GPA_消费并退出
        // 测试表目标：场景：resp_gpa_refill=1，RSP有效且hasGpf，请求VPN/stage匹配。
        // 观察功能：need_gpa清0；GPF结果携带完整gpaddr；不必再发PTW。 测试目的：观察 GPA 等待者何时完成或退出，以及计数边界、重定向和标识回绕的作用。
        cp_07_03_001 : coverpoint (!resp_miss && (actual_fault == expected_fault && ((req_out_cmd == 3'd0 ||
            req_out_cmd == 3'd4) ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5)
            ? (resp_excp & 6'b010101) == 0 : 1'b1)))
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && resp_has_gpf
                && gpa_ng
                && gpa_response_ready
                && gpa_vpn == req_out_vaddr[49:12]
                && gpa_stage == req_out_s2xlate) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.03.002 GPA_启动同拍fast返回
        // 测试表目标：场景：启动GPF请求满足canGetGpa，p_hit_fast同拍又匹配新返回。
        // 观察功能：gpa_start&&canGpa&&p_hit_fast时安排need_clear_need_gpa；观察后续清理拍，不能由任意p_hit_fast单独推导必须清GPA。
        // 测试目的：观察 GPA 等待者何时完成或退出，以及计数边界、重定向和标识回绕的作用。
        cp_07_03_002 : coverpoint gpa_clear
            iff (sample_enable
                && fast_gpa_start_d1) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_set = {1'b1};
        }
        // 07.03.003 GPA_redirect退出
        // 测试表目标：场景：need_gpa=1,need_gpa_robidx_valid=1，redirect使owner ROB需要flush。
        // 观察功能：need_gpa/resp_gpa_refill/robidx_valid清0，VPN清0。 测试目的：观察 GPA
        // 等待者何时完成或退出，以及计数边界、重定向和标识回绕的作用。
        cp_07_03_003 : coverpoint (!gpa_ng && !gpa_response_ready && !gpa_owner_valid)
            iff (sample_enable
                && owner_redirected_d1) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.03.004 GPA_redirect不影响older
        // 测试表目标：场景：need_gpa=1且redirect不覆盖owner ROB。 观察功能：状态不因该redirect清除。 测试目的：观察 GPA
        // 等待者何时完成或退出，以及计数边界、重定向和标识回绕的作用。
        cp_07_03_004 : coverpoint gpa_ng
            iff (sample_enable
                && gpa_ng_d1
                && redirect_valid_d1
                && !owner_redirected_d1
                && !gpa_clear_event) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_set = {1'b1};
        }
        // 07.03.005 GPA_计数边界
        // 测试表目标：场景：need_gpa=1，连续返回getGpa=1而未消费匹配结果。 观察功能：count按事件递增；到511全1触发下一拍清理和归零。 测试目的：观察 GPA
        // 等待者何时完成或退出，以及计数边界、重定向和标识回绕的作用。
        cp_07_03_005 : coverpoint gpa_count
            iff (sample_enable
                && gpa_ng
                && gpa_count inside {9'd0, 9'd1, 9'd510, 9'd511}) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_zero = {0};
            bins b_one = {1};
            bins b_near_full = {510};
            bins b_full = {511};
        }
        // 07.03.006 GPA_计数保持
        // 测试表目标：场景：need_gpa=1；无返回或P.getGpa=0。 观察功能：无计数事件且无启动、消费、延迟清理或满清理时保持；同时退出事件另行分类。 测试目的：观察
        // GPA 等待者何时完成或退出，以及计数边界、重定向和标识回绕的作用。
        cp_07_03_006 : coverpoint (gpa_count == gpa_count_d1)
            iff (sample_enable
                && gpa_ng_d1
                && !ptw_getgpa_event_d1
                && !gpa_start_d1
                && !owner_redirected_d1
                && gpa_count_d1 != 511) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.03.007 GPA_计数清零优先
        // 测试表目标：场景：need_gpa_wire或state_clear或count_full，与getGpa返回同拍。 观察功能：清零优先于加1。 测试目的：观察 GPA
        // 等待者何时完成或退出，以及计数边界、重定向和标识回绕的作用。
        cp_07_03_007 : coverpoint (gpa_count == 0)
            iff (sample_enable
                && ptw_getgpa_event_d1
                && (gpa_start_d1 || owner_redirected_d1)) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_07_03 cg_dtlb_07_03_inst;

    // 07.04 GPA地址与跨页故障定位
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_07_04】07.04 GPA地址与跨页故障定位
    // 探测功能：观察 GPF 报告的 GPA 是数据地址还是 VS 页表项地址，并覆盖跨页访问的故障定位。
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：07.04.001, 07.04.002, 07.04.003, 07.04.004, 07.04.005, 07.04.006, 07.04.007,
    // 07.04.008, 07.04.009；逐点功能见下方注释，精确配值见分解表。
    // pre_fault：请求拍 checkfullva 有效，且预检查 PF/GPF/AF 中至少一种成立；均使用_d1快照。
    covergroup cg_dtlb_07_04 @(posedge clock);
        option.per_instance = 1;
        // 07.04.001 CROSS_跨页首半GPF
        // 测试表目标：场景：S1 leaf GPF，fullva[12]==vaddr[12]且fullva[11:0]!=vaddr[11:0]，覆盖非对齐首半使用原始偏移。
        // 观察功能：gpaddr页内偏移取原fullva[11:0]。 测试目的：观察 GPF 报告的 GPA 是数据地址还是 VS 页表项地址，并覆盖跨页访问的故障定位。
        cp_07_04_001 : coverpoint (resp_gpaddr[11:0] == req_fullva_d1[11:0])
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && resp_has_gpf
                && !resp_nonleaf
                && req_fullva_d1[12] == req_out_vaddr[12]
                && req_fullva_d1[11:0] != req_out_vaddr[11:0]
                && GROUP_ID<2) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.04.002 CROSS_跨页第二半GPF
        // 测试表目标：场景：leaf GPF，req_out.fullva[12]!=req_out.vaddr[12]。
        // 观察功能：gpaddr页内偏移取新页vaddr[11:0]，通常为0。 测试目的：观察 GPF 报告的 GPA 是数据地址还是 VS 页表项地址，并覆盖跨页访问的故障定位。
        cp_07_04_002 : coverpoint (resp_gpaddr[11:0] == req_out_vaddr[11:0])
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && resp_has_gpf
                && !resp_nonleaf
                && req_fullva_d1[12] != req_out_vaddr[12]
                && GROUP_ID<2) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.04.003 CROSS_onlyS2地址
        // 测试表目标：场景：s2xlate=2，分别构造首半/第二半GPF。
        // 观察功能：gpaddr整体取crossPageVaddr，首半fullva、第二半vaddr；无预取及特殊抑制时，onlyS2
        // GPF可直接给出地址，不启动补GPA。canGpa独立记录。 测试目的：观察 GPF 报告的 GPA 是数据地址还是 VS 页表项地址，并覆盖跨页访问的故障定位。
        cp_07_04_003 : coverpoint ((resp_gpaddr == (req_fullva_d1[12] ==
            req_out_vaddr[12]?req_effective_va_d1:{14'd0, req_out_vaddr})))
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && resp_has_gpf
                && req_out_s2xlate == 2
                && GROUP_ID<2) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.04.009 GPA_leaf_offset
        // 测试表目标：场景：S1 leaf GPF，非跨页第二半；输入fullva与vaddr页内偏移相同。非对齐首半偏移不同归C-034。
        // 观察功能：gpaddr={s1.genGVPN,fullva[11:0]}；大页/NAPOT按genGVPN重构。 测试目的：观察 GPF 报告的 GPA 是数据地址还是 VS
        // 页表项地址，并覆盖跨页访问的故障定位。
        cp_07_04_009 : coverpoint (resp_gpaddr[11:0] == req_fullva_d1[11:0])
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && resp_has_gpf
                && !resp_nonleaf
                && !pre_fault
                && req_fullva_d1[12] == req_out_vaddr[12]
                && req_fullva_d1[11:0] == req_out_vaddr[11:0]
                && GROUP_ID<2) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.04.004 GPA_nonLeaf_level1
        // 测试表目标：场景：GPF，S1有效nonleaf，level=1，非pre-fault。 观察功能：gpaddr
        // offset=VPN第0段9bit<<3；高位来自返回S1页表PPN。 测试目的：观察 GPF 报告的 GPA 是数据地址还是 VS 页表项地址，并覆盖跨页访问的故障定位。
        cp_07_04_004 : coverpoint (resp_gpaddr[11:0] == {req_vaddr_d1[20:12], 3'd0})
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && resp_has_gpf
                && resp_nonleaf
                && gpa_s1level == 1
                && HAS_GPA) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.04.005 GPA_nonLeaf_level2
        // 测试表目标：场景：GPF，S1有效nonleaf，level=2，非pre-fault。 观察功能：gpaddr
        // offset=VPN第1段9bit<<3；高位来自返回S1页表PPN。 测试目的：观察 GPF 报告的 GPA 是数据地址还是 VS 页表项地址，并覆盖跨页访问的故障定位。
        cp_07_04_005 : coverpoint (resp_gpaddr[11:0] == {req_vaddr_d1[29:21], 3'd0})
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && resp_has_gpf
                && resp_nonleaf
                && gpa_s1level == 2
                && HAS_GPA) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.04.006 GPA_nonLeaf_level3
        // 测试表目标：场景：GPF，S1有效nonleaf，level=3，非pre-fault。 观察功能：gpaddr
        // offset=VPN第2段9bit<<3；高位来自返回S1页表PPN。 测试目的：观察 GPF 报告的 GPA 是数据地址还是 VS 页表项地址，并覆盖跨页访问的故障定位。
        cp_07_04_006 : coverpoint (resp_gpaddr[11:0] == {req_vaddr_d1[38:30], 3'd0})
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && resp_has_gpf
                && resp_nonleaf
                && gpa_s1level == 3
                && HAS_GPA) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.04.007 GPA_fakePTE_mode8
        // 测试表目标：场景：GPF，S1 fakePTE，vsatp.mode=8。 观察功能：gpaddr offset=VPN[2]<<3，与S1返回level无关。 测试目的：观察
        // GPF 报告的 GPA 是数据地址还是 VS 页表项地址，并覆盖跨页访问的故障定位。
        cp_07_04_007 : coverpoint (resp_gpaddr[11:0] == {req_vaddr_d1[38:30], 3'd0})
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && resp_has_gpf
                && gpa_s1fake
                && csr_vsatp_mode == 8
                && HAS_GPA) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.04.008 GPA_fakePTE_mode9
        // 测试表目标：场景：GPF，S1 fakePTE，vsatp.mode=9。 观察功能：gpaddr offset=VPN[3]<<3，与S1返回level无关。 测试目的：观察
        // GPF 报告的 GPA 是数据地址还是 VS 页表项地址，并覆盖跨页访问的故障定位。
        cp_07_04_008 : coverpoint (resp_gpaddr[11:0] == {req_vaddr_d1[47:39], 3'd0})
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && resp_has_gpf
                && gpa_s1fake
                && csr_vsatp_mode == 9
                && HAS_GPA) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_07_04 cg_dtlb_07_04_inst;

    // 07.05 共享GPA状态竞争与恢复
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_07_05】07.05 共享GPA状态竞争与恢复
    // 探测功能：观察多端口共享 GPA 状态的竞争优先级，以及刷新或并发业务后的恢复。
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：07.05.004；逐点功能见下方注释，精确配值见分解表。
    covergroup cg_dtlb_07_05 @(posedge clock);
        option.per_instance = 1;
        // 07.05.004 CONCURRENT_GPA_替换_预取
        // 测试表目标：场景：需求GPF等待GPA同时预取/普通请求触发miss与返回。 观察功能：共享need_gpa会抑制该实例refill；其他实例可独立前进。
        // 测试目的：观察多端口共享 GPA 状态的竞争优先级，以及刷新或并发业务后的恢复。
        cp_07_05_004 : coverpoint (!resp_miss || resp_qvalid || resp_replay)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && gpa_ng
                && !resp_has_gpf) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_07_05 cg_dtlb_07_05_inst;

    // 08.05 在途刷新与新旧上下文恢复
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_08_05】08.05 在途刷新与新旧上下文恢复
    // 探测功能：观察刷新前后的在途返回和映射切换，跟踪新请求是否最终使用新上下文。
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：08.05.005, 08.05.007, 08.05.008, 08.05.009；逐点功能见下方注释，精确配值见分解表。
    // 异常位序：resp_excp[5:0]={af_st,af_ld,gpf_st,gpf_ld,pf_st,pf_ld}；actual_fault==expected_fault核对类别，读写位掩码额外检查命令方向。
    // actual_fault：从实际输出resp_excp聚合[2:0]={AF,GPF,PF}，各类分别对load/store位做OR。
    // expected_fault：参考异常向量[2:0]={AF,GPF,PF}：预异常优先；无hit为0；有hit时AF优先于PF、PF优先于GPF。
    // expected_paddr：参考PA：翻译时拼接resp_ppn与请求页内偏移；直通按no_translate选择PMP地址或请求VA。
    covergroup cg_dtlb_08_05 @(posedge clock);
        option.per_instance = 1;
        // 08.05.005 FENCE_新epoch重新填入
        // 测试表目标：场景：flush生效后发新请求并正常返回。 观察功能：允许重新回填并命中；连续flush不能导致永久丢请求。
        // 测试目的：观察刷新前后的在途返回和映射切换，跟踪新请求是否最终使用新上下文。
        cp_08_05_005 : coverpoint ((resp_paddr_0 == expected_paddr) && (actual_fault == expected_fault &&
            ((req_out_cmd == 3'd0 || req_out_cmd == 3'd4) ? (resp_excp & 6'b101010) == 0 : (req_out_cmd ==
            3'd1 || req_out_cmd == 3'd5) ? (resp_excp & 6'b010101) == 0 : 1'b1)))
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && flushed_epoch
                && refill_seen
                && storage_hit
                && !resp_miss) {
            bins b_expected_result = {1'b1};
        }
        // 08.05.007 CONCURRENT_ASID_VMID快速切换
        // 测试表目标：场景：合法CSR/fence序列切换上下文后复用相同VA。 观察功能：token快照/延迟CSR/entry ID一致；无旧上下文错误命中。
        // 测试目的：观察刷新前后的在途返回和映射切换，跟踪新请求是否最终使用新上下文。
        cp_08_05_007 : coverpoint (actual_fault == expected_fault && ((req_out_cmd == 3'd0 || req_out_cmd ==
            3'd4) ? (resp_excp & 6'b101010) == 0 : (req_out_cmd == 3'd1 || req_out_cmd == 3'd5) ? (resp_excp &
            6'b010101) == 0 : 1'b1))
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && {csr_asid, csr_vasid, csr_vmid} != context_ids_d1) {
            bins b_expected_set = {1'b1};
        }
        // 08.05.008 E2E_映射变更
        // 测试表目标：场景：同VA先映射PA1→正确fence→映射PA2，再执行load/store。 观察功能：最终只使用PA2，权限/PBMT更新也生效。
        // 测试目的：观察刷新前后的在途返回和映射切换，跟踪新请求是否最终使用新上下文。
        cp_08_05_008 : coverpoint (resp_paddr_0 == expected_paddr)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && last_response_valid
                && flushed_epoch
                && last_vaddr == req_vaddr_d1
                && last_paddr != resp_paddr_0
                && !resp_miss) {
            bins b_expected_set = {1'b1};
        }
        // 08.05.009 E2E_故障修复
        // 测试表目标：场景：先缓存PF/GPF/AF→修改映射/权限→fence→重试。 观察功能：新请求恢复成功；旧故障缓存已失效。
        // 测试目的：观察刷新前后的在途返回和映射切换，跟踪新请求是否最终使用新上下文。
        cp_08_05_009 : coverpoint (resp_paddr_0 == expected_paddr)
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && last_response_valid
                && flushed_epoch
                && last_vaddr == req_vaddr_d1
                && last_fault != 0
                && actual_fault == 0
                && !resp_miss) {
            bins b_expected_set = {1'b1};
        }
    endgroup
    cg_dtlb_08_05 cg_dtlb_08_05_inst;

    // 09.03 结果已知性与结束原因
    // ----------------------------------------------------------------------------------------
    // 【cg_dtlb_09_03】09.03 结果已知性与结束原因
    // 探测功能：统计输出已知性、事务结束原因与观察窗口，保留未完成和未知结果作为独立类别。
    // 采样时序：本拍resp/req_out与前拍req_*_d1配对；CSR无后缀是响应拍值，带_d1的是请求拍快照。
    // 仓位判读：iff列出触发场景，coverpoint是实际输出或关系；值为1的结果仓表示该关系成立。
    // 测试点：09.03.001, 09.03.002；逐点功能见下方注释，精确配值见分解表。
    // 异常位序：resp_excp[5:0]={af_st,af_ld,gpf_st,gpf_ld,pf_st,pf_ld}；actual_fault==expected_fault核对类别，读写位掩码额外检查命令方向。
    // response_end_reason：结束原因编码：0=命中无异常，1=异常，2=miss；miss优先分类。
    // response_knownness：响应valid/miss/异常/PA/PBMT中存在X/Z时为1，否则为0。
    covergroup cg_dtlb_09_03 @(posedge clock);
        option.per_instance = 1;
        // 09.03.001 FCOV_输出已知性
        // 测试表目标：场景：O.valid且字段有定义；或W.valid/外部PTW fire。
        // 观察功能：采四态logic快照，monitor用$isunknown及逐位===分类后传枚举；不将X/Z转换成bit后再统计。全已知为目标，其余为诊断。
        // 测试目的：统计输出已知性、事务结束原因与观察窗口，保留未完成和未知结果作为独立类别。
        cp_09_03_001 : coverpoint response_knownness
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && resp_valid
                && (!$isunknown({resp_valid, resp_miss, resp_excp, resp_paddr_0, resp_pbmt})) === 1'b1) {
            bins b_all_known = {0};
        }
        // 09.03.002 FCOV_完成延迟与窗口结束
        // 测试表目标：场景：每个合法REQ建跟踪项；在成功/fault/replay/cancel、观察窗W到期或测试结束时采样。
        // 观察功能：每个attempt只归档一次，replay后的新attempt另计；W是可配置观测阈值，记录其值，不视为RTL时限。到期与结束仍未完成归诊断，不等同功能通过。
        // 测试目的：统计输出已知性、事务结束原因与观察窗口，保留未完成和未知结果作为独立类别。
        cp_09_03_002 : coverpoint response_end_reason
            iff (sample_enable
                && req_valid_d1
                && !req_kill_d1
                && (resp_valid) === 1'b1) {
            bins b_success = {0};
            bins b_fault = {1};
            bins b_replay = {2};
        }
    endgroup
    cg_dtlb_09_03 cg_dtlb_09_03_inst;

    initial begin
        cg_dtlb_01_01_inst = new();
        cg_dtlb_01_02_inst = new();
        cg_dtlb_01_03_inst = new();
        cg_dtlb_01_04_inst = new();
        cg_dtlb_01_05_inst = new();
        cg_dtlb_diag_01_05_inst = new();
        cg_dtlb_02_01_inst = new();
        cg_dtlb_02_02_inst = new();
        cg_dtlb_02_03_inst = new();
        cg_dtlb_02_04_inst = new();
        cg_dtlb_02_05_inst = new();
        cg_dtlb_diag_02_05_inst = new();
        cg_dtlb_03_01_inst = new();
        cg_dtlb_03_03_inst = new();
        cg_dtlb_03_07_inst = new();
        cg_dtlb_04_01_inst = new();
        cg_dtlb_04_02_inst = new();
        cg_dtlb_04_03_inst = new();
        cg_dtlb_04_04_inst = new();
        cg_dtlb_04_05_inst = new();
        cg_dtlb_04_06_inst = new();
        cg_dtlb_diag_04_06_inst = new();
        cg_dtlb_04_07_inst = new();
        cg_dtlb_05_01_inst = new();
        cg_dtlb_05_02_inst = new();
        cg_dtlb_06_03_inst = new();
        cg_dtlb_06_05_inst = new();
        cg_dtlb_07_01_inst = new();
        cg_dtlb_07_02_inst = new();
        cg_dtlb_07_03_inst = new();
        cg_dtlb_07_04_inst = new();
        cg_dtlb_07_05_inst = new();
        cg_dtlb_08_05_inst = new();
        cg_dtlb_09_03_inst = new();
    end

endmodule: memblock_dtlb_core_fcov
`endif

`endif
