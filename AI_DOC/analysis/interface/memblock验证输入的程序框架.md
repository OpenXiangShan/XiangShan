# memblock 验证输入的程序框架

## 范围收敛

本文档只面向 `MemBlock` 顶层 Verilog IO 接口，用于定义 memblock UT 输入生成程序需要驱动、监测和联动的信号集合。

权威接口来源：

```text
build_memblock/rtl/MemBlock.sv
```

Scala 源码仅用于理解语义和合法性约束，不作为端口名、方向、位宽依据。

本轮明确不纳入：

- `DCache.sv`、`Sbuffer.sv` 等 MemBlock 内部 module 的端口
- backend/top bypass、top/backend bypass、trace bypass 类信号
- performance counter、topdown、debug/perf event 信号
- DFT、MBIST、SRAM/bore/scan 相关信号

L2TLB 是唯一特殊情况：memblock 环境中不单独包含 L2TLB module，但需要按 MemBlock 顶层 Verilog 端口分析 L2TLB 与 DTLB/ITLB 的交互输入输出信号。

## 顶层输入生成原则

输入程序只直接驱动 Verilog `input` 方向的 MemBlock 顶层端口。Verilog `output` 方向端口不驱动，只作为握手、响应、scoreboard 和约束状态来源。

对 valid/ready 接口：

- MemBlock input `valid/bits`：由验证环境按事务生成。
- MemBlock output `ready`：由 DUT 给出，输入程序必须读取后决定是否提交事务。
- MemBlock output `valid/bits`：由 monitor 采集，用于更新公共状态、reference model 和 scoreboard。
- MemBlock input `ready`：由验证环境产生 backpressure。

输入框架不应使用内部派生信号替代顶层端口。例如 DCache load/store、Sbuffer 入队、Sbuffer forward 等内部端口都不直接作为 memblock 环境输入；这些行为应通过 OOO issue、LSQ、TL、L2TLB、CSR/redirect 等顶层接口间接产生。

## 必要公共状态变量

建议输入生成器维护以下公共状态，供各 agent/sequence 共用：

| 状态变量 | 作用 |
| --- | --- |
| `cycle` | 全局周期计数，支持延迟、超时、burst 间隔 |
| `reset_done` | reset 释放后才允许发事务 |
| `rob_alloc` | 分配和回收 `robIdx_flag/value`，约束 redirect、writeback、feedback |
| `lq_alloc` | 分配和回收 `lqIdx_flag/value`，约束 load/vec load/LSQ resp |
| `sq_alloc` | 分配和回收 `sqIdx_flag/value`，约束 store/vec store/sbuffer/feedback |
| `uop_alloc` | 生成 `uopIdx`、`vuopIdx`、`lastUop` |
| `ftq_alloc` | 生成 `ftqIdx/ftqOffset/isRVC/pc` |
| `issue_queue_state` | 记录 int/vec issue 已发未完成事务 |
| `lsq_state` | 记录 enqLsq 请求、canAccept、resp 分配结果 |
| `tl_outstanding` | 记录 TileLink A/C/E 与 B/D 响应的 source/sink/data 对应关系 |
| `tlb_state` | 记录 ITLB/DTLB 请求与 L2TLB 响应对应关系 |
| `redirect_state` | redirect 有效时取消或标记 younger robIdx 事务 |
| `csr_state` | satp/vsatp/hgatp/priv/pf/trigger 等控制状态 |

## MemBlock 顶层输入接口分组

### 全局

必须驱动：

- `clock`
- `reset`

### Redirect

输入端口：

- `io_redirect_valid`
- `io_redirect_bits_level`
- `io_redirect_bits_robIdx_flag`
- `io_redirect_bits_robIdx_value[8:0]`

规则：

- redirect 生效时，以 `robIdx` 为界清理或标记 younger 事务。
- issue、LSQ、TLB、writeback 相关输入不能继续提交已被 redirect 杀掉的事务。

### OOO -> Mem intIssue

通道：

- `io_ooo_to_mem_intIssue_0_0_*`
- `io_ooo_to_mem_intIssue_1_0_*`
- `io_ooo_to_mem_intIssue_2_0_*`
- `io_ooo_to_mem_intIssue_3_0_*`
- `io_ooo_to_mem_intIssue_4_0_*`
- `io_ooo_to_mem_intIssue_5_0_*`
- `io_ooo_to_mem_intIssue_6_0_*`

握手：

- DUT output：`*_ready`
- 环境 input：`*_valid` 和 `*_bits_*`

Load 类通道 `0/1/2` 主要输入字段：

- `bits_fuOpType[8:0]`
- `bits_src_0[63:0]`
- `bits_imm[63:0]`
- `bits_robIdx_flag/value[8:0]`
- `bits_pdest[7:0]`
- `bits_rfWen`
- `bits_fpWen`
- `bits_pc[49:0]`
- `bits_isRVC`
- `bits_ftqIdx_flag/value[5:0]`
- `bits_ftqOffset[4:0]`
- `bits_loadWaitBit`
- `bits_waitForRobIdx_flag/value[8:0]`
- `bits_storeSetHit`
- `bits_loadWaitStrict`
- `bits_lqIdx_flag/value[6:0]`
- `bits_sqIdx_flag/value[5:0]`

STA 类通道 `3/4` 主要输入字段：

- `bits_fuType[35:0]`
- `bits_fuOpType[8:0]`
- `bits_src_0[63:0]`
- `bits_imm[63:0]`
- `bits_robIdx_flag/value[8:0]`
- `bits_isFirstIssue`
- `bits_pdest[7:0]`
- `bits_isRVC`
- `bits_ftqIdx_flag/value[5:0]`
- `bits_ftqOffset[4:0]`
- `bits_storeSetHit`
- `bits_ssid[4:0]`
- `bits_sqIdx_flag/value[5:0]`

STD 类通道 `5/6` 主要输入字段：

- `bits_fuType[35:0]`
- `bits_fuOpType[8:0]`
- `bits_src_0[63:0]`
- `bits_robIdx_flag/value[8:0]`
- `bits_sqIdx_flag/value[5:0]`

规则：

- `valid` 只能在 reset 结束后拉高。
- 事务提交条件是 `valid && ready`。
- `robIdx/lqIdx/sqIdx` 必须来自公共分配器，不能各通道独立随机。
- load 通道必须携带合法 `lqIdx`；store address/data 通道必须携带同源 `sqIdx`。
- `fuType/fuOpType` 必须和通道类别一致。

### OOO -> Mem vecIssue

通道：

- `io_ooo_to_mem_vecIssue_0_0_*`
- `io_ooo_to_mem_vecIssue_1_0_*`

握手：

- DUT output：`*_ready`
- 环境 input：`*_valid` 和 `*_bits_*`

两个通道公共输入字段：

- `bits_fuOpType[8:0]`
- `bits_src_0[127:0]`
- `bits_src_1[127:0]`
- `bits_src_2[127:0]`
- `bits_src_3[127:0]`
- `bits_vl[7:0]`
- `bits_robIdx_flag/value[8:0]`
- `bits_pdest[6:0]`
- `bits_pdestVl[4:0]`
- `bits_vecWen`
- `bits_v0Wen`
- `bits_vlWen`
- `bits_vpu_vill`
- `bits_vpu_vma`
- `bits_vpu_vta`
- `bits_vpu_vsew[1:0]`
- `bits_vpu_vlmul[2:0]`
- `bits_vpu_specVill`
- `bits_vpu_specVma`
- `bits_vpu_specVta`
- `bits_vpu_specVsew[1:0]`
- `bits_vpu_specVlmul[2:0]`
- `bits_vpu_vm`
- `bits_vpu_vstart[7:0]`
- `bits_vpu_frm[2:0]`
- `bits_vpu_fpu_isFpToVecInst`
- `bits_vpu_fpu_isFP32Instr`
- `bits_vpu_fpu_isFP64Instr`
- `bits_vpu_fpu_isReduction`
- `bits_vpu_fpu_isFoldTo1_2`
- `bits_vpu_fpu_isFoldTo1_4`
- `bits_vpu_fpu_isFoldTo1_8`
- `bits_vpu_vxrm[1:0]`
- `bits_vpu_vuopIdx[6:0]`
- `bits_vpu_lastUop`
- `bits_vpu_vmask[127:0]`
- `bits_vpu_nf[2:0]`
- `bits_vpu_veew[1:0]`
- `bits_vpu_isReverse`
- `bits_vpu_isExt`
- `bits_vpu_isNarrow`
- `bits_vpu_isDstMask`
- `bits_vpu_isOpMask`
- `bits_vpu_isMove`
- `bits_vpu_isDependOldVd`
- `bits_vpu_isWritePartVd`
- `bits_vpu_isVleff`
- `bits_vpu_maskVecGen[15:0]`
- `bits_vpu_sew8/sew16/sew32/sew64`
- `bits_ftqIdx_flag/value[5:0]`
- `bits_ftqOffset[4:0]`
- `bits_numLsElem[4:0]`
- `bits_lqIdx_flag/value[6:0]`
- `bits_sqIdx_flag/value[5:0]`

通道 `0_0` 额外输入：

- `bits_fuType[35:0]`

规则：

- `vl/vstart/numLsElem/vuopIdx/lastUop` 必须成组合法。
- `fuType/fuOpType` 必须能区分 vector load、vector store、segment、FOF 等路径。
- vector load 使用 `lqIdx`，vector store 使用 `sqIdx`，segment 场景可能同时依赖 `lqIdx/sqIdx/numLsElem`。
- `src_0` 是基地址来源；`src_1/src_2/src_3` 按 indexed/strided/store-data/old-vd 场景约束。

### OOO -> Mem enqLsq

输入端口：

- `io_ooo_to_mem_enqLsq_needAlloc_0..7[1:0]`
- `io_ooo_to_mem_enqLsq_req_0..7_valid`
- `io_ooo_to_mem_enqLsq_req_0..7_bits_fuType[35:0]`
- `io_ooo_to_mem_enqLsq_req_0..7_bits_uopIdx[6:0]`
- `io_ooo_to_mem_enqLsq_req_0..7_bits_robIdx_flag/value[8:0]`
- `io_ooo_to_mem_enqLsq_req_0..7_bits_lqIdx_flag/value[6:0]`
- `io_ooo_to_mem_enqLsq_req_0..7_bits_sqIdx_flag/value[5:0]`
- `io_ooo_to_mem_enqLsq_req_0..7_bits_numLsElem[4:0]`

必须监测的 DUT output：

- `io_ooo_to_mem_enqLsq_canAccept`
- `io_ooo_to_mem_enqLsq_resp_0..7_lqIdx_flag/value[6:0]`
- `io_ooo_to_mem_enqLsq_resp_0..7_sqIdx_flag/value[5:0]`

规则：

- `needAlloc` 要和 `fuType` 指示的 load/store/vector/segment 资源需求一致。
- 只有在 `canAccept` 允许时提交 LSQ enqueue。
- `req_N` 与对应 issue 事务必须共享 `robIdx/uopIdx/numLsElem`。
- resp 返回的 `lqIdx/sqIdx` 必须写回公共 `lsq_state`，后续 issue、feedback、writeback 使用同一索引。

### OOO -> Mem tlbCsr

输入端口：

- `io_ooo_to_mem_tlbCsr_satp_mode/asid/ppn/changed`
- `io_ooo_to_mem_tlbCsr_vsatp_mode/asid/ppn/changed`
- `io_ooo_to_mem_tlbCsr_hgatp_mode/vmid/ppn/changed`
- `io_ooo_to_mem_tlbCsr_mbmc_BME/CMODE/BCLEAR/BMA`
- `io_ooo_to_mem_tlbCsr_priv_mxr/sum/vmxr/vsum/virt/virt_changed/spvp/imode/dmode`
- `io_ooo_to_mem_tlbCsr_mPBMTE`
- `io_ooo_to_mem_tlbCsr_hPBMTE`
- `io_ooo_to_mem_tlbCsr_pmm_mseccfg/menvcfg/henvcfg/hstatus/senvcfg`

规则：

- `*_changed` 拉高时，需要更新 `tlb_state`，并约束后续 ITLB/DTLB 请求的翻译结果。
- `virt/s2xlate/hgatp/vsatp` 必须和 L2TLB 响应合法性一致。

### OOO -> Mem csrCtrl

输入端口分组：

- prefetch control：`pf_ctrl_*`
- store buffer/control：`sbuffer_timeout[21:0]`
- memory check/control：`ldld_vio_check_enable`、`cache_error_enable`、`uncache_write_outstanding_enable`
- power/flush：`power_down_enable`、`flush_l2_enable`
- distributed CSR write：`distribute_csr_w_valid/addr/data`
- frontend/mem trigger：`frontend_trigger_*`、`mem_trigger_*`
- `fsIsOff`

规则：

- 默认随机输入框架可以固定大多数控制位为安全值，只在对应专项用例中打开。
- `sbuffer_timeout`、`flush_l2_enable`、trigger 控制会影响 LSQ/Sbuffer/异常路径，需要和 issue/redirect 状态联动。

### OOO -> Mem sfence

输入端口：

- `io_ooo_to_mem_sfence_valid`
- `io_ooo_to_mem_sfence_bits_rs1`
- `io_ooo_to_mem_sfence_bits_rs2`
- `io_ooo_to_mem_sfence_bits_addr[49:0]`
- `io_ooo_to_mem_sfence_bits_id[15:0]`
- `io_ooo_to_mem_sfence_bits_hv`
- `io_ooo_to_mem_sfence_bits_hg`

规则：

- sfence 发出后需要清理或刷新 `tlb_state`。
- `rs1/rs2/hv/hg/id/addr` 必须和当前 satp/vsatp/hgatp 模式匹配。

### OOO -> Mem LSQ commit/control

输入端口：

- `io_ooo_to_mem_lsqio_pendingPtr_flag`
- `io_ooo_to_mem_lsqio_pendingPtr_value[8:0]`
- `io_ooo_to_mem_flushSb`

规则：

- `pendingPtr` 应来自 ROB 提交边界或当前未提交窗口。
- `flushSb` 触发时，应停止新增 store 类事务，等待相关状态收敛。

### Interrupt / top control

保留为 MemBlock 顶层输入，但默认可固定为安全值：

- interrupt sink：`auto_inner_beu_local_int_sink_in_0`、`auto_inner_nmi_int_sink_in_*`、`auto_inner_plic_int_sink_in_*`、`auto_inner_debug_int_sink_in_0`、`auto_inner_clint_int_sink_in_*`
- `io_hartId[5:0]`
- `io_l2_hint_valid`
- `io_l2_hint_bits_sourceId[3:0]`
- `io_l2_hint_bits_isKeyword`
- `io_l2_flush_done`
- `io_wfi_wfiReq`
- BEU error input：`io_inner_beu_errors_icache_ecc_error_*`

暂不纳入主随机输入的 bypass/top passthrough：

- `io_ooo_to_mem_backendToTopBypass_*`
- `io_mem_to_ooo_topToBackendBypass_*`
- `io_fromTopToBackend_*`
- `io_inner_*` / `io_outer_*` 中只做顶层透传的 bypass/control 信号

## L2TLB 特例：ITLB/DTLB 与 L2TLB 交互

这部分虽然语义来自 L2TLB，但在 memblock 环境中按 MemBlock 顶层 Verilog 端口处理。环境不实例化 L2TLB module 时，需要用该接口模型化 L2TLB 交互。

### ITLB -> L2TLB

输入端口：

- `io_fetch_to_mem_itlb_req_0_valid`
- `io_fetch_to_mem_itlb_req_0_bits_vpn[37:0]`
- `io_fetch_to_mem_itlb_req_0_bits_s2xlate[1:0]`
- `io_fetch_to_mem_itlb_resp_ready`

必须监测的输出端口：

- `io_fetch_to_mem_itlb_req_0_ready`
- `io_fetch_to_mem_itlb_resp_valid`
- `io_fetch_to_mem_itlb_resp_bits_s2xlate[1:0]`
- `io_fetch_to_mem_itlb_resp_bits_s1_entry_*`
- `io_fetch_to_mem_itlb_resp_bits_s1_addr_low`
- `io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_0..7`
- `io_fetch_to_mem_itlb_resp_bits_s1_valididx_0..7`
- `io_fetch_to_mem_itlb_resp_bits_s1_pteidx_0..7`
- `io_fetch_to_mem_itlb_resp_bits_s1_pf`
- `io_fetch_to_mem_itlb_resp_bits_s1_af`
- `io_fetch_to_mem_itlb_resp_bits_s2_entry_*`
- `io_fetch_to_mem_itlb_resp_bits_s2_gpf`
- `io_fetch_to_mem_itlb_resp_bits_s2_gaf`

规则：

- 请求提交条件是 `req_valid && req_ready`。
- 响应消费条件是 `resp_valid && resp_ready`。
- `vpn/s2xlate` 必须和 tlbCsr 中 satp/vsatp/hgatp 状态一致。
- resp 中 stage1/stage2 entry、fault、access fault、PBMT、Napot 字段必须来自同一个 TLB 模型状态。

### DTLB -> L2TLB

输入端口：

- `io_l2_tlb_req_req_valid`
- `io_l2_tlb_req_req_bits_vaddr[49:0]`
- `io_l2_tlb_req_req_bits_fullva[63:0]`
- `io_l2_tlb_req_req_bits_checkfullva`
- `io_l2_tlb_req_req_bits_cmd[2:0]`
- `io_l2_tlb_req_req_bits_hyperinst`
- `io_l2_tlb_req_req_bits_hlvx`
- `io_l2_tlb_req_req_bits_kill`
- `io_l2_tlb_req_req_bits_memidx_is_ld`
- `io_l2_tlb_req_req_bits_memidx_is_st`
- `io_l2_tlb_req_req_bits_memidx_idx[6:0]`
- `io_l2_tlb_req_req_bits_isPrefetch`
- `io_l2_tlb_req_req_bits_no_translate`
- `io_l2_tlb_req_req_bits_pmp_addr[47:0]`
- `io_l2_tlb_req_req_bits_debug_robIdx_flag`
- `io_l2_tlb_req_req_bits_debug_robIdx_value[8:0]`
- `io_l2_tlb_req_req_bits_debug_isFirstIssue`
- `io_l2_tlb_req_req_kill`

必须监测的输出端口：

- `io_l2_tlb_req_resp_valid`
- `io_l2_tlb_req_resp_bits_paddr_0/1[47:0]`
- `io_l2_tlb_req_resp_bits_gpaddr_0/1[63:0]`
- `io_l2_tlb_req_resp_bits_fullva[63:0]`
- `io_l2_tlb_req_resp_bits_pbmt_0/1[1:0]`
- `io_l2_tlb_req_resp_bits_miss`
- `io_l2_tlb_req_resp_bits_fastMiss`
- `io_l2_tlb_req_resp_bits_isForVSnonLeafPTE`
- `io_l2_tlb_req_resp_bits_excp_0/1_*`
- `io_l2_tlb_req_resp_bits_ptwBack`
- `io_l2_tlb_req_resp_bits_memidx_is_ld/is_st/idx`
- `io_l2_tlb_req_resp_bits_debug_robIdx_flag/value`
- `io_l2_tlb_req_resp_bits_debug_isFirstIssue`

规则：

- `memidx_is_ld/st/idx` 必须和公共 `lq_alloc/sq_alloc` 一致。
- `cmd`、`no_translate`、`hyperinst`、`hlvx` 决定异常/fault 合法性。
- `kill` 或 redirect 命中该 `robIdx` 时，不应再将该翻译结果用于后续提交。
- `paddr/gpaddr/pbmt/excp` 必须和 TLB/PMP 模型一致。

## TileLink 顶层接口

TileLink 端口都是 MemBlock 顶层 Verilog IO，但输入生成程序只驱动 MemBlock `input` 方向。

需要建模的 TL 端口家族：

- `auto_inner_buffers_out_*`
- `auto_inner_dcache_client_out_*`
- `auto_inner_frontendBridge_instr_uncache_in/out_*`
- `auto_inner_frontendBridge_icachectrl_in/out_*`
- `auto_inner_frontendBridge_icache_in/out_*`
- `auto_inner_ptw_to_l2_buffer_out_*`

常见驱动规则：

- MemBlock 输出 A/C/E channel 时，环境驱动对应 `*_ready`。
- MemBlock 输入 B/D channel 时，环境驱动对应 `*_valid` 和 `*_bits_*`。
- source/sink 必须和 outstanding table 匹配。
- D channel data/denied/corrupt 必须和之前 A/C 请求语义一致。
- TL 响应延迟、backpressure、乱序能力由 `tl_outstanding` 控制。

暂不作为主输入专项展开：

- `auto_inner_l2_pf_sender_out_*`
- `auto_inner_l3_pf_sender_out_*`

它们是 MemBlock 输出 prefetch request，默认只监测，不驱动。

## Mem -> OOO 必须监测的顶层输出

这些不是输入驱动字段，但输入生成必须监测它们以维持合法状态：

- `io_mem_to_ooo_intWriteback_*`
- `io_mem_to_ooo_vecWriteback_*`，其中 `*_ready` 是环境 input，`valid/bits` 是 DUT output
- `io_mem_to_ooo_wakeup_*`
- `io_mem_to_ooo_*IqFeedback_*`
- `io_mem_to_ooo_lqCancelCnt`
- `io_mem_to_ooo_sqCancelCnt`
- `io_mem_to_ooo_lqDeq/sqDeq`
- `io_mem_to_ooo_lqDeqPtr/sqDeqPtr`
- `io_mem_to_ooo_updateLFST_*`
- `io_mem_to_ooo_memoryViolation_*`
- `io_mem_to_ooo_sbIsEmpty`
- `io_mem_to_ooo_mdpTrain_*`
- `io_mem_to_ooo_lsqio_*`
- `io_mem_to_ooo_ldCancel_*`

规则：

- writeback/feedback 中的 `robIdx/lqIdx/sqIdx` 必须能匹配之前输入事务。
- `memoryViolation`、`ldCancel`、redirect 会改变后续可提交事务集合。
- `sbIsEmpty`、`lq/sqDeq`、`lq/sqCancelCnt` 更新公共 LSQ 状态。

## 排除项清单

以下内容从本文档删除，不再作为输入程序框架字段：

- `DCache` 内部 `io_lsu_load_*`、`io_lsu_store_*`、`io_lsu_forward_*`
- `Sbuffer` 内部 `io_in_req_*`、`io_forward_*`、`io_dcache_*`
- SRAM/MBIST/bore/hold/bypass module 内部控制
- performance counter、topdown、debug/perf event
- trace/bypass/top passthrough

如果后续需要验证 DCache 或 Sbuffer 内部 module，需要另建内部 subagent 专项文档，不应混入 MemBlock 顶层输入框架。

## 输入生成优先级

1. 建立 `rob/lq/sq/uop/ftq` 公共分配器。
2. 先驱动 `enqLsq`，拿到或确认 LSQ 分配结果。
3. 再驱动 `intIssue/vecIssue`，保证 `robIdx/lqIdx/sqIdx/numLsElem/fuType/fuOpType` 与 LSQ 状态一致。
4. 同步驱动 `tlbCsr/sfence/redirect`，维护 TLB 与 flush 状态。
5. 对 L2TLB 特例，按 ITLB/DTLB 请求生成 TLB 响应模型状态。
6. 对 TL 外设/缓存侧输入，根据 MemBlock 输出请求和 outstanding table 返回合法 B/D 响应和 ready。
7. 监测 Mem->OOO 输出，回收 ROB/LSQ、更新 violation/cancel/writeback 状态。
