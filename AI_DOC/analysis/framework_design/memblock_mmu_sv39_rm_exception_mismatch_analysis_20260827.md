# memblock Sv39 U 态 RM 异常失配分析（2026-08-27）

## 1. 结论

本次失配归类为 **RM 的异常比较语义与建模范围不匹配**，不是已证实的 RTL 功能错误。DTLB 先给出单一 Page Fault，随后 LoadUnit/StoreUnit 按现有实现把物理 PMP/PMA checker 的 Access Fault 候选 OR 入 raw `exceptionVec`。DUT 的写回和 commit status 对同一 raw 向量一致；两者均为 `PF + AF`。

复现结果为：前 10 笔已完成的 Load/Store 中，RM 只从翻译结果预测单一 Page Fault，而 DUT 的 raw `exceptionVec` 还包含 U 态默认 PMP deny 导出的 Access Fault。UVM `quit count=10` 使仿真在 `756.3ns` 结束，`MEMBLOCK_MAIN_TRANS_NUM=10000` 尚未完成。

V2 完整核的 CSR 不会把这两个 bit 作为两个架构 trap：`ExceptionNO.priorities` 会选择 Page Fault，且 Page Fault 的优先级高于同类型 Access Fault。因此不能把 raw 向量双 bit 直接判定为 RTL 产生两个架构异常。

## 2. 测试上下文

| 项目 | 值 |
| --- | --- |
| 分支 | `mem_ut_uvm_v2`（V2 profile） |
| testcase | `basicTest` |
| VSEQ | `memblock_dispatch_real_smoke_vseq` |
| CSR sequence | `memblock_mmu_sv39_csr_sequence` |
| preset cfg | `mem_ut/ver/ut/memblock/seq/plus_cfg/tc_dispatch_real_mmu_sv39_smoke.cfg` |
| 随机种子 | `666666` |
| 主表规模 | `MEMBLOCK_MAIN_TRANS_NUM=10000` |
| CSR changed | `MEMBLOCK_CSR_CONTROL_ENABLE=0`，静态 sequence 驱动 |
| SFence 动态控制 | `MEMBLOCK_SFENCE_CONTROL_ENABLE=0` |
| 当前特权级 | `priv_imode=U`、`priv_dmode=U`（编码 `2'd0`） |
| 地址翻译 | `satp.MODE=Sv39`（编码 `4'd8`） |
| satp root | `MEMBLOCK_PADDR_BASE=0x80000000`，`satp.PPN=0x80000` |

复现使用的远端 flow（从 `mem_ut/ver/ut/memblock/sim` 执行）为：

~~~bash
make eda_run tc=basicTest ts=memblock_dispatch_real_smoke_vseq \
  mode=rm_lda_missing_final_20260827 cfg=tc_dispatch_real_mmu_sv39_smoke \
  seed=666666 wave=on timing=rtl pl=UVM_HIGH \
  plus_arg=+MEMBLOCK_BOUNDARY_PROFILE_GEN_EN=0
~~~

上面最后一个 `plus_arg` 是本次隔离复现时的边界场景关闭覆盖；cfg 文件自身的边界权重仍为非零。它不影响下述 PF/PMP 失配。

编译成功日志：

~~~text
mem_ut/ver/ut/memblock/sim/rm_lda_missing_final_20260827/log/vcs_compile_rtl.log
~~~

运行日志和 FSDB：

~~~text
mem_ut/ver/ut/memblock/sim/rm_lda_missing_final_20260827/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_dispatch_real_mmu_sv39_smoke_seed=666666_rtl.log
mem_ut/ver/ut/memblock/sim/rm_lda_missing_final_20260827/wave/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_dispatch_real_mmu_sv39_smoke_seed=666666_rtl.fsdb
~~~

## 3. RM 失配摘要

异常向量位定义来自 `src/main/scala/xiangshan/package.scala:831-841`：`loadAccessFault` 为 bit 5，即 `0x0020`；`storeAccessFault` 为 bit 7，即 `0x0080`；`loadPageFault` 为 bit 13，即 `0x2000`；`storePageFault` 为 bit 15，即 `0x8000`。

| 时间 | UID | 类型 | ROB | VA | first PA | 大小 | RM 期望 | DUT 实际 |
| --- | ---: | --- | --- | --- | --- | ---: | --- | --- |
| `596.3ns` | 0 | Load | `0/113` | `0x84d1ae80` | `0x4d1ae80` | 8 | `0x002000` | `0x002020` |
| `611.3ns` | 1 | Store | `0/114` | `0x84d1ae80` | `0x4d1ae80` | 1 | `0x008000` | `0x008080` |
| `651.3ns` | 2 | Store | `0/115` | `0x84d1ae80` | `0x4d1ae80` | 8 | `0x008000` | `0x008080` |
| `691.3ns` | 3 | Load | `0/116` | `0x8c671600` | `0xc671600` | 2 | `0x002000` | `0x002020` |
| `696.3ns` | 4 | Load | `0/117` | `0x84d1ae80` | `0x4d1ae80` | 4 | `0x002000` | `0x002020` |
| `701.3ns` | 5 | Store | `0/118` | `0x8c671600` | `0xc671600` | 2 | `0x008000` | `0x008080` |
| `741.3ns` | 6 | Load | `0/119` | `0x8b37da40` | `0xb37da40` | 4 | `0x002000` | `0x002020` |
| `746.3ns` | 7 | Load | `0/120` | `0x825e2d40` | `0x25e2d40` | 4 | `0x002000` | `0x002020` |
| `751.3ns` | 8 | Load | `0/121` | `0x80555480` | `0x555480` | 1 | `0x002000` | `0x002020` |
| `756.3ns` | 9 | Store | `0/122` | `0x8c671600` | `0xc671600` | 4 | `0x008000` | `0x008080` |

日志原文示例：

~~~text
uid 0 Load exception expected=0x2000 status=0x2020 actual=0x2020
uid 1 Store exception expected=0x8000 status=0x8080
~~~

RM 的 translation trace 对每个 UID 都记录 `translation_path=PAGED_ENTRY satp=8 vsatp=0 hgatp=0 s2xlate=0`，并且根据 L2TLB 返回的页表异常只产生 PF 期望值。

## 4. FSDB 波形证据

以下信号均从上述 FSDB 在 `eda01` 的 Verdi `fsdbreport` 读取。

### 4.1 CSR 已按要求生效

~~~text
top_tb/U_MEMBLOCK/io_ooo_to_mem_tlbCsr_satp_mode
  250ns: 0
  270.3ns: 8
top_tb/U_MEMBLOCK/io_ooo_to_mem_tlbCsr_satp_ppn
  250ns: 0x00000000000
  270.3ns: 0x00000080000
top_tb/U_MEMBLOCK/io_ooo_to_mem_tlbCsr_priv_imode = 0
top_tb/U_MEMBLOCK/io_ooo_to_mem_tlbCsr_priv_dmode = 0
top_tb/U_MEMBLOCK/io_ooo_to_mem_tlbCsr_satp_changed = 0
~~~

因此失配不是静态 CSR sequence 没有切换到 Sv39/U 态造成的。

### 4.2 Load：L2TLB 只有 PF，PMP 另给 deny

在 UID 0 对应的 `590.3ns` Load 窗口，FSDB 观察到：

~~~text
top_tb/U_MEMBLOCK/_inner_dtlb_ld_tlb_ld_io_requestor_0_resp_bits_excp_0_pf_ld = 1
top_tb/U_MEMBLOCK/_inner_dtlb_ld_tlb_ld_io_requestor_0_resp_bits_excp_0_af_ld = 0
top_tb/U_MEMBLOCK/_inner_pmp_checkers_0_io_resp_ld = 1
top_tb/U_MEMBLOCK/_inner_LoadUnit_0_io_ldout_bits_uop_exceptionVec_13 = 1
top_tb/U_MEMBLOCK/_inner_LoadUnit_0_io_ldout_bits_uop_exceptionVec_5 = 1
top_tb/U_MEMBLOCK/auto_inner_dcache_client_out_d_bits_denied = 0
top_tb/U_MEMBLOCK/auto_inner_dcache_client_out_d_bits_corrupt = 0
~~~

`exceptionVec_13=1` 是 PF，`exceptionVec_5=1` 是额外的 LAF。DCache 没有 `denied/corrupt`，所以额外 LAF 不是 DCache 返回错误。

### 4.3 Store：L2TLB 只有 PF，PMP 另给 deny

在 UID 1 对应的 `600ns` Store 窗口，FSDB 观察到：

~~~text
top_tb/U_MEMBLOCK/_inner_dtlb_st_tlb_st_io_requestor_0_resp_bits_excp_0_pf_st = 1
top_tb/U_MEMBLOCK/_inner_dtlb_st_tlb_st_io_requestor_0_resp_bits_excp_0_af_st = 0
top_tb/U_MEMBLOCK/_inner_pmp_checkers_4_io_resp_st = 1
top_tb/U_MEMBLOCK/_inner_pmp_checkers_4_io_resp_ld = 0
top_tb/U_MEMBLOCK/_inner_StoreUnit_0_io_stout_valid = 1
top_tb/U_MEMBLOCK/_inner_StoreUnit_0_io_stout_bits_uop_exceptionVec_15 = 1
top_tb/U_MEMBLOCK/_inner_StoreUnit_0_io_stout_bits_uop_exceptionVec_7 = 1
top_tb/U_MEMBLOCK/auto_inner_dcache_client_out_d_bits_denied = 0
top_tb/U_MEMBLOCK/auto_inner_dcache_client_out_d_bits_corrupt = 0
~~~

`exceptionVec_15=1` 是 PF，`exceptionVec_7=1` 是额外的 SAF。V2 的 `LduCnt=2`、`HyuCnt=1`，`MemBlock.scala:1259` 将普通 StoreUnit 0 连接到 `pmp_check(LduCnt + HyuCnt + 1 + 0)`，即波形中的 `pmp_checkers_4`。

## 5. RTL 路径分析

### 5.1 PMP 默认权限

`src/main/scala/xiangshan/backend/fu/PMP.scala:426-430` 的默认配置为：

~~~scala
val passThrough = if (pmpEntries.isEmpty) true.B else (mode > 1.U)
pmpDefault.cfg.r := passThrough
pmpDefault.cfg.w := passThrough
pmpDefault.cfg.x := passThrough
~~~

当前 PMP CSR entries 保持 reset 全零，但硬件的 `pmpEntries` 向量本身非空；因此 `pmpEntries.isEmpty` 为假。U 态编码为 0 时 `mode > 1.U` 为假，默认 PMP 结果就是读写权限不通过。波形中的 `resp_ld=1` 和 `resp_st=1` 与该实现一致。

### 5.2 LoadUnit 的 raw 异常合并

`src/main/scala/xiangshan/mem/pipeline/LoadUnit.scala:1206-1212` 已明确说明：地址翻译产生 PF/AF 后，PMP/PMA response 不可信，并计算了 `s2_un_access_exception`。但 `:1222-1228` 仍无条件把 `s2_pmp.ld` 和 `s2_pmp.instr` 合入 `loadAccessFault`：

~~~scala
s2_exception_vec(loadAccessFault) := s2_vecActive && (
  s2_in.uop.exceptionVec(loadAccessFault) ||
  s2_pmp.ld ||
  s2_pmp.instr && LSUOpType.isHlvx(s2_in.uop.fuOpType) ||
  ...
)
~~~

该路径说明 raw vector 可同时保留 `loadPageFault=1` 和 `loadAccessFault=1`。源码注释确实指出翻译异常后的 PMP/PMA response 对物理访问判定“不可信”，但当前实现没有把 `s2_un_access_exception` 用作 AF OR 的 gating 条件；它只用于后续 MMIO/uncache 路径。这个边界应保留为 RTL owner 可评估的实现语义问题，但仅凭 raw 双 bit 不能定性为架构 RTL bug。

### 5.3 StoreUnit 的异常合并

`src/main/scala/xiangshan/mem/pipeline/StoreUnit.scala:471-476` 有同样的“不可信 PMP/PMA response”说明和 `s2_un_access_exception`。但 `:494-500` 仍无条件把 `s2_pmp.st`、以及 CMO 使用的 `s2_pmp.ld` 合入 `storeAccessFault`：

~~~scala
s2_out.uop.exceptionVec(storeAccessFault) := (s2_in.uop.exceptionVec(storeAccessFault) ||
                                              s2_pmp.st ||
                                              s2_pmp.ld && s2_isCbo_noZero ||
                                              ...
                                              ) && s2_vecActive
~~~

该路径同样允许 raw `storePageFault=1` 和 `storeAccessFault=1` 同时存在，形成 `0x8080`。`src/main/scala/xiangshan/mem/MemBlock.scala:793-804` 创建并驱动 PMP checker，StoreUnit 连接到对应 checker；该 AF 是后处理候选，不是 DTLB response 同时返回的 AF。

### 5.4 U 态 PTE 权重会强烈偏向 Page Fault

cfg 中 `MEMBLOCK_L2TLB_S1_PTE_U_1_WT=1` 不是“U 位和非 U 位等权”。`tlb_map_builder::choose_bit()` 的分布是 `1 := one_wt, 0 := 100-one_wt`，所以该设置使 PTE.U 为 1 的概率只有 1%。对 U 态 S1 访问，RM 与 DUT 权限语义都要求 `pte_u=1`；并且 LEGAL PTE 的 fixup 只补 R/A/D/V，不会把 U 强行置 1。

因此该 cfg 会强烈偏向 Page Fault；十笔连续出现 PF 并不表明 `S1_PF_1_WT=1` 被错误解读为 100%。若 10000 笔的目标是覆盖正常数据路径，应把“正常 U PTE”与“故障 PTE”拆成不同权重或不同 preset；这与本次默认 PMP U 态拒绝是两个独立维度。

## 6. RM 问题与触发配置

1. DTLB response 的 PF、AF 位在波形中直接可见，Load 和 Store 都是 `PF=1、AF=0`，RM 的翻译期望正确反映了这一级。
2. 静态 CSR sequence 把 `csrCtrl.distribute_csr.w.valid` 固定为 0，PMP CSR 因而保持 V2 Scala reset 全零。`PMP.scala` 中有非空 PMP entry，U 态 (`mode=0`) 的 `pmpDefault.cfg.r/w/x` 都为 0，故 `PMPResp.ld/st=1` 是该配置的必然结果。
3. `memblock_rm::observer_build_commit_item()` 只合并 `tlb_af`、L2TLB `pma_af`、DCache `denied/corrupt`，没有 post-TLB PMP checker 的 snapshot 或模型；所以它得到 `0x2000/0x8000`，而 raw DUT 得到 `0x2020/0x8080`。
4. `rm_ls_model_t::compare_load()` 与 `commit_store()` 用完整 24-bit 向量做精确相等比较。对于 MemBlock 单元边界，这把低优先级、被 CSR trap 优先级遮蔽的 AF 也升级成 fatal mismatch。
5. DCache `denied=0、corrupt=0`，排除了 DCache responder 注入；写回 raw vector 和 status raw vector 一致，排除了 LDA 关联或 status 采集不一致。

另有一个独立的后续风险：本 cfg 设置 `MEMBLOCK_MAIN_MEM_RANGES_EN=0`，`tlb_map_builder::make_canonical_ppn()` 会进入 sparse PPN 分支，正常翻译不保证落入 `MEMBLOCK_PADDR_BASE/PADDR_RANGE`。这不是本次 AF 的直接证据，但若目标是大量正常 DCache 数据比较，必须单独使有效翻译物理地址落在可服务内存窗口。

## 7. 修改方案（未实施，当前选定：无运行期清除）

完整方案已分别独立整理到 [RM/L2 DCache sticky 错误状态账本修改方案](../../plan/rm_plan/undo/memblock_rm_l2_dcache_sticky_error_ledger_plan_20260828.md)
和 [RM PMA/PMP 独立参考模型修改方案](../../plan/rm_plan/undo/memblock_rm_pma_pmp_model_plan_20260828.md)。本节保留决策摘要和与本次失配直接相关的约束。

### 7.1 方案决策与适用边界

本次采用测试用例级的 sticky error 模式：DCache 物理 64B 行一旦观察到合法
`GrantData.denied/corrupt`，运行期间永久保持该行错误状态；只有 reset 或 testcase 的 shared
state 初始化可以清空。`CBO.clean`、`CBO.flush`、`CBO.inval`、`Probe(toB/toN)` 和 cache
eviction 都不清除该表。

这样可以去掉可清除方案所需的 `clear_sample`、完整 history queue 以及 CBO/Probe 清除回调，
使 RM 直接查询 live sticky map。代价是它不再模拟真实 cache line replacement 后错误 meta
被重建的生命周期，因此必须作为独立、显式的回归模式，不能默认为所有 DCache 回归语义。

### 7.2 RM 异常真源

RM 不得从 `main_view` 读取任何异常字段，也不得把主表中的 `tlbAF`、`tlbPF`、`tlbGPF`、
`pmaAF`、`denied` 或 `corrupt` 作为期望异常来源。

| 异常类别 | 唯一来源 |
| --- | --- |
| S1/S2 PF、GPF、TLB AF/guest AF、TLB response PMA AF | 冻结 CSR request context 与 `tlb_entry_by_key` canonical entry |
| DCache/L2 `denied/corrupt` | sticky L2 D-response 行状态账本 |
| post-TLB PMA/PMP AF | 独立 [PMA/PMP 模型方案](../../plan/rm_plan/undo/memblock_rm_pma_pmp_model_plan_20260828.md)；当前分析只记录现象，未实施 |

### 7.3 Sticky 行状态与并发

账本位于 `mem_access_base_sequence` 共享 runtime state，以 `paddr[47:6]` 为 key；必须与
1 KiB `main_mem` backing line 以及现有 `dcache_corrupt_byte_mask_by_line` 分离。每条记录保存
`state`（`NONE/PENDING/CORRUPT/DENIED`）、`generation`、各错误状态的可见 sample 以及
`source/sink` 诊断字段。

`NONE -> CORRUPT -> DENIED` 只允许单调升级；`DENIED` 始终同时表示 `corrupt`。即使不支持
清除，仍保留 `PENDING` reservation：同一行存在未完成 GrantData 时，后续 outstanding 请求
必须复用同一份 response snapshot，不能各自重新随机。RM 查询按访问 sample 判断错误是否已
在该访问之前生效，避免“后发请求先污染 live map”被错误追溯到早发请求。

### 7.4 DCache 和 RM 的最小流程

1. `AcquireBlock A.fire` 先查 sticky 行状态；已有状态时复用，未命中时只随机一次并建立
   pending response record。
2. 最后一个 `GrantData D.fire` 后发布状态和可见 sample；D ready hold 或多 beat 期间不得重采样。
3. `CBOAck`、`Grant`、`ReleaseAck`、B/C channel 错误和 Uncache response 不写入该账本。
4. RM 翻译成功得到 PA 后，按 LDA writeback 或 Store writeback/fault sample 查询；跨 64B 行
   时逐行查询并 OR。若 sample 早于状态激活 sample，不应用该错误；事实未就绪时等待。
5. 保留两个原始 bit。`denied` 与 `corrupt-only` 到异常向量的映射必须遵循 DUT 实际路径，
   不能把所有 `corrupt` 一律改成 Access Fault；当前 V2 load 路径中 `denied` 对应
   `loadAccessFault`，`corrupt && !denied` 对应 `hardwareError`。

### 7.5 接口、验证与风险

实现落点仍为 `mem_access_base_sequence`、`dcache_mem__access_base_sequence`、
`memblock_rm_readonly_api` 和 `memblock_rm::observer_build_commit_item()`；不修改 RTL。
验证至少覆盖首次 denied/corrupt、同线并发、跨行访问、访问 sample 先后关系、CBO/Probe 不清除、
reset 清空及 TLB 异常与 DCache 错误组合。sticky 模式的物理地址永久污染、地址重用和真实
replacement 差异是已知限制，独立 plan 中给出显式开关和回归隔离要求。

当前请求只完成分析和文档修订，未修改 RM、RTL、sequence 或 cfg。

## 8. 当前状态

- 测试状态：`10` 个 RM error，仿真时间 `756.3ns`，未达到 10000 笔。
- 当前结论：错误来自 raw-vector 比较范围与 U 态默认 PMP 行为的不匹配；没有证据证明 DUT 会产生两个架构 trap，也不满足“已确认 RTL 功能错误即可结束”的条件。
- 波形：

~~~text
mem_ut/ver/ut/memblock/sim/rm_lda_missing_final_20260827/wave/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_dispatch_real_mmu_sv39_smoke_seed=666666_rtl.fsdb
~~~

- 关键日志：

~~~text
mem_ut/ver/ut/memblock/sim/rm_lda_missing_final_20260827/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_dispatch_real_mmu_sv39_smoke_seed=666666_rtl.log:2277
mem_ut/ver/ut/memblock/sim/rm_lda_missing_final_20260827/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_dispatch_real_mmu_sv39_smoke_seed=666666_rtl.log:2304
~~~
