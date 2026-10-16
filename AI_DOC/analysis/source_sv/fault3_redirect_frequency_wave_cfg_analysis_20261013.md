# 问题3 redirect 高频场景波形与测试框架分析报告（第四版）

**分析日期**：2026-10-13
**分析范围**：原始 FSDB、原始/修复后仿真日志、当前 cfg、测试框架实现和 ROB admission window 方案。
**修改边界**：本次只修订报告，没有修改 RTL、sequence 或 cfg；问题3 worktree 中已有的代码改动属于独立实施任务。

## 1. 输入、版本与状态

### 1.1 历史失败输入

- 原始 FSDB：
  `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/fault_2pct_no_trigger_20261009/rerun_wave/wave/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=676087581_rtl.fsdb`
- 原始日志：
  `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/fault_2pct_no_trigger_20261009/rerun_wave/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=676087581_rtl_full.log`
- 原始日志 SHA256：
  `9e53b8eac19b02a17ca07e7f06a7fa6d8ccf7f6aaaa9c11231a2613bef4a3586`
- FSDB 时间范围：`0ps` 到 `977983700ps`。

历史运行的权威配置是原始日志第 1 行展开的完整 `Command:`。关键参数为：

```text
+ntb_random_seed=676087581
+MEMBLOCK_MAIN_TRANS_NUM=10000
+MEMBLOCK_L2TLB_LEVEL_WEIGHT_EN=1
+MEMBLOCK_L2TLB_S1_PF_1_WT=1
+MEMBLOCK_L2TLB_S1_AF_1_WT=1
+MEMBLOCK_L2TLB_S1_LEVEL_0/1/2/3_WT=1/1/1/1
+MEMBLOCK_L2TLB_S1_PTE_R/W/X/U/G/A/D/N/V_1_WT=1/1/1/1/1/1/1/1/1
+MEMBLOCK_MAIN_MEM_RANGES_EN=0
+MEMBLOCK_PMA_PMP_MODEL_EN=0
```

### 1.2 当前真实 seed 自然通过状态

ROB admission window 修复后的完整自然运行日志：

`mem_ut/ver/ut/memblock/sim/rob_admission_window_fault3_verify/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=676087581_rtl_problem3_original_seed_full_natural_20261013.log`

日志 SHA256：

`bd1d9f3a3fa1d543c947773758ebee392982e9fe5d3214d32af20c5ddcb8c23a`

**已验证事实：**

```text
MEMBLOCK_MAIN_TRANS_NUM = 10000
MEMBLOCK_PMA_PMP_MODEL_EN = 1
2440933.700ns  TEST_PASS
UVM_ERROR = 0
UVM_FATAL = 0
```

该运行自然完成全部 10,000 笔；日志中没有 `LSQ_CANCEL_RECONCILE` 或 `stranded_owner`，也没有停在此前的后续 LQ owner 冲突。

版本边界必须保留：历史失败波形使用 `PMA_PMP_MODEL_EN=0`，当前自然通过使用当前 cfg 的 `PMA_PMP_MODEL_EN=1`。因此当前结果证明 ROB admission window 修复在当前 cfg 下完整通过，但不是历史命令行的逐位复现。PMA 差异不能解释历史已观测到的 Page Fault 高频，因为历史 feedback 只有 `0x2000/0x8000`，五次 PTW payload 的显式 `s1_pf/s1_af` 也为 0；但 PMA 仍可能影响其他访问结果，历史复核和 clean fault-rate 统计必须分别固定配置。

## 2. 结论摘要

1. **问题3的直接根因是测试框架缺少 ROB admission window。** 框架只限制 LQ/SQ 容量，没有限制未提交 ROB 窗口，导致 `uid=4116` 在最老 fault head `uid=3955` 尚未释放时以 UID distance 161 进入 active/DUT-visible；ROB 容量只有 160。
2. **prefix 外 owner 是上游 ROB 超窗后的下游症状。** 第一轮 redirect 在非法跨圈 ROB 输入上无法唯一判断年龄，回退 prefix 后留下 `uid=4116`；第二轮 redirect 才出现原 prefix scan 看不到它的 cancel mismatch。prefix 外 owner 不应成为受支持的生命周期。
3. **旧的 prefix 外 owner 补扫必须从最终修复中删除。** 最终方案是在 dispatch/admission 前阻止 distance >= 160 的 UID，并在 activation 和 redirect 前后做 fail-fast/containment 检查。任何 prefix 外 DUT-visible owner 都应报不变量错误，而不是自动补扫。
4. **历史用例的 redirect 确实异常频繁，但它是压力放大条件，不是问题3直接根因。** UID prefix 的 redirect 覆盖率是 `3896/3957=98.4584%`；在该 prefix 内，所有产生 scalar Load/Store fault feedback 的 UID 都最终成为 redirect owner，即 `3896/3896=100%`。
5. **高频主因是错误权限加大页复用。** U-mode 下唯一 canonical 1GB PTE 为 `U=0/W=0/R=1`，整个 256MB 测试区间反复命中同一权限，产生 Load/Store Page Fault；不是每个请求独立抽中了 2% fault。
6. **现有 cfg 可以消除主要噪声。** 正常 PTE 权限固定合法；4KB 专项应使用 `LEVEL_WEIGHT_EN=0 + MAIN_MEM_RANGES_EN=1`；clean cfg 还要关闭 PMA/PMP 和 denied/corrupt 注入。
7. **严格按 UID 控制 fault 需要 `UID_PLAN`。** 新框架策略应由 `FAULT_POLICY_MODE=LEGACY|ENTRY_PPM|ENTRY_PERIODIC|UID_PLAN` 统一选择，并保证默认 `LEGACY` 完全保留旧 plusarg 语义和旧 seed 随机流。

## 3. 已验证事实、合理推断与待验证项

### 3.1 已验证事实

- 原始日志共有 122075 次 fault feedback，涉及 3994 个不同 UID。
- feedback 只有 Load Page Fault `0x2000` 和 Store Page Fault `0x8000`。
- 原始 fatal 前共有 3896 个不同 redirect owner、3895 个 fault retire。
- 3895 个 retire 由 2289 个 Load Page Fault 和 1606 个 Store Page Fault 组成。
- UID `0..3956` 共 3957 个 UID；其中 3896 个有 scalar fault feedback 且全部成为 redirect owner，另 61 个在该 prefix 中没有 scalar fault feedback。
- 还有 98 个 `uid>3956` 已产生 fault feedback，但在 fatal 前尚未轮到它们成为 redirect owner。
- FSDB 中五次有效 PTW response 都复用同一 canonical entry，payload 均为 Stage-1 level 2、`U=0/W=0/R=1/A=1/D=1/V=1`、`s1_pf=0/s1_af=0`。
- ROB admission window 完整自然复跑最终 `TEST_PASS`、`UVM_ERROR=0`、`UVM_FATAL=0`。

### 3.2 合理推断

- 持续的 fault redirect、reissue、ROB wrap 和 LSQ cancel 显著增加了触发非法 ROB 超窗及其下游症状的机会，是问题3的强压力放大条件。
- 单个 1GB `U=0/W=0` mapping 足以解释 prefix 内 scalar Load/Store fault redirect 率为 100%，不需要假设显式 PF/AF 权重失效。
- 合法权限、4KB mapping 和 clean fault source 会大幅降低 redirect 密度，但不能代替 ROB admission window 修复。

### 3.3 待验证项

- 历史 `PMA_PMP_MODEL_EN=0` 命令行在 ROB admission window 修复后是否同样自然完成。
- clean 4KB cfg 下跨 seed 的 canonical-entry fault 比例是否符合目标统计区间。
- 产品目标是“fault entry 比例”还是“scalar UID 比例”；后者需要 `UID_PLAN`，不能用 entry 权重近似替代。
- 新策略模式的随机流兼容性、预算边界和 range-hit 冲突仍需 directed test 与 seed 对照验证。

## 4. redirect 与异常计数口径

### 4.1 四套不能混用的计数

| 指标 | 数量 | 含义 |
| --- | ---: | --- |
| fault feedback event | 122075 | writeback/reissue 产生的重复事件；同一 UID 可出现很多次 |
| unique feedback UID | 3994 | 至少产生过一次 Load/Store Page Fault feedback 的 UID |
| unique redirect owner | 3896 | fatal 前实际被选为 fault redirect owner 的 UID |
| fault retire | 3895 | 已完成异常 retire 的 UID；最后一个 redirect owner 在 fatal 前未 retire |
| retired Load Page Fault | 2289 | `exceptionVec[13] / 0x2000` |
| retired Store Page Fault | 1606 | `exceptionVec[15] / 0x8000` |

事件分类：

```text
fault feedback total          = 122075
feedback unique uid           = 3994
load page fault feedback      = 88733
store page fault feedback     = 33342

redirect unique uid           = 3896
fault retire                  = 3895
retired load page fault       = 2289
retired store page fault      = 1606
```

`2289 + 1606 = 3895`。`122075` 不能作为 fault request 数，因为 redirect/reissue 会让同一 UID 重复 feedback；`3994` 也不能直接与 `3896` 相除解释为 redirect 丢失，因为其中 98 个更年轻 UID 在 fatal 前还没有走到 redirect owner。

### 4.2 两个 redirect 率

**UID prefix redirect 覆盖率：**

```text
3896 / 3957 = 98.4584%
```

分母是 UID `0..3956` 的整个已观察 prefix，包括 61 个没有 scalar fault feedback 的 UID。它描述 prefix 覆盖，不是 fault 请求概率。

**scalar Load/Store fault redirect 率：**

```text
3896 / 3896 = 100%
```

分母只取同一 UID prefix 内产生过 scalar Load/Store fault feedback 的 UID；这些 UID 在 fatal 前全部成为 redirect owner。该 100% 结论不能扩展到 3994 个全部 feedback UID，因为其中 98 个位于 `uid>3956`，仿真在它们成为 owner 前已经结束。

### 4.3 时间分布

每 100us 的 redirect 数：

```text
0-100us      390
100-200us    387
200-300us    399
300-400us    391
400-500us    378
500-600us    409
600-700us    395
700-800us    403
800-900us    430
900-977us    314
```

redirect 从开始到 fatal 持续发生，不是失败点附近的短时 burst。

## 5. FSDB 与日志时序证据

### 5.1 `585.700ns` 与 `590.700ns` 的区别

这两个时间点不能都称为“第一个 PTW response”：

```text
585.700ns  accept token=2, vpn=0x8c11a, range-hit anchor=0x8ea7c；驱动 io_ptw_resp_valid/payload
590.700ns  complete token=2；io_ptw_resp_valid deassert
590.700ns  同一采样还 accept token=3, vpn=0x87f5a
```

因此：

- `585.700ns` 是 token 2 的 request accept，也是 FSDB 中第一次 `io_ptw_resp_valid` 上升并驱动该 token 的 payload；
- `590.700ns` 是 token 2 的 sequence completion 和 `io_ptw_resp_valid` deassert，不是第一次 valid；
- `590.700ns` 同时接受 token 3，不表示 token 3 当拍返回。

### 5.2 五次 request/response 与 payload

request accept：

| token | accept | VPN | lookup | canonical anchor | created |
| ---: | ---: | ---: | ---: | ---: | ---: |
| 0 | 575.700ns | `0x8ea7c` | 2 | `0x8ea7c` | 1 |
| 1 | 580.700ns | `0x81129` | 1 | `0x8ea7c` | 0 |
| 2 | 585.700ns | `0x8c11a` | 1 | `0x8ea7c` | 0 |
| 3 | 590.700ns | `0x87f5a` | 1 | `0x8ea7c` | 0 |
| 4 | 595.700ns | `0x82821` | 1 | `0x8ea7c` | 0 |

FSDB `io_ptw_resp_valid` 上升与 sequence completion/deassert：

| token | `io_ptw_resp_valid` 上升/驱动 payload | sequence completion/valid deassert | payload 结论 |
| ---: | ---: | ---: | --- |
| 2 | 585.700ns | 590.700ns | 与 canonical entry 一致 |
| 1 | 595.700ns | 600.700ns | 与 canonical entry 一致 |
| 3 | 605.700ns | 610.700ns | 与 canonical entry 一致 |
| 0 | 650.700ns | 655.700ns | 与 canonical entry 一致 |
| 4 | 670.700ns | 675.700ns | 与 canonical entry 一致 |

五次 FSDB payload 均为：

```text
s2xlate = 0
S1 tag  = 相同 canonical tag
S1 level = 2                    # Sv39 1GB
S1.U/W/R = 0/0/1
S1.A/D/V = 1/1/1
S1.PF/AF = 0/0                  # 非显式 PF/AF 注入
```

后四个 token 的 `lookup=1` 是 range hit，直接复用 token 0 创建的 frozen entry。不同 VPN 的跨度超过 2MB，但仍命中同一 anchor；结合 level matcher，只能由 Sv39 level 2 的 1GB coverage 解释。

### 5.3 首尾 fault redirect

```text
623.700ns  最早 feedback：uid=75, Load Page Fault
628.700ns  uid=0, Store Page Fault feedback
635.700ns  首个 redirect owner：uid=0
668.700ns  uid=0 fault retire

977938.700ns  uid=3956, Store Page Fault feedback
977943.700ns  uid=3962, Load Page Fault feedback
977945.700ns  redirect owner uid=3956, ROB=1/76
977983.700ns  LSQ_CANCEL_RECONCILE ordinary=8, dut=10
```

`uid=0` 是首个 redirect owner，但不是首个 feedback UID；尾部 `uid=3962` 已 feedback，但年龄更老的 `uid=3956` 先成为 owner。

## 6. 高频 redirect 的形成原因

### 6.1 PTE 权重被错误理解

`choose_bit()` 的实际语义是：

```systemverilog
chosen dist {
  1'b1 := one_wt,
  1'b0 := 100 - one_wt
};
```

所以：

- `S1_PTE_U_1_WT=1`：U 只有 1% 为 1，99% 为 0；
- `S1_PTE_W_1_WT=1`：W 只有 1% 为 1，99% 为 0；
- `PTE_MODE=LEGAL` 只修正叶子形状及 A/D/V，不会把 U/W 修正为 1。

测试运行在 U-mode。Load 即使 `R=1`，也会因 `U=0` 产生 Load Page Fault；Store 同时违反 `U=1` 和 `W=1`，产生 Store Page Fault。

### 6.2 1GB mapping 放大权限错误

测试 VA 范围是 `0x80000000..0x8fffffff`，共 256MB，完全落在一个 1GB mapping 内。唯一 canonical entry 的 `U=0/W=0` 被所有相关访问复用，所以 scalar fault redirect 率达到 100%。这里的随机粒度是 canonical entry，不是 UID，也不是 feedback event。

## 7. 问题3根因与最终修复边界

### 7.1 直接根因：缺少 ROB admission window

失败窗口中：

```text
fault/redirect head = uid3955, ROB=1/75
异常年轻 owner     = uid4116, ROB=0/76
UID distance        = 4116 - 3955 = 161
ROB_SIZE            = 160
```

主表 UID/ROB 静态分配是连续的；错误发生在运行时 admission：

- 框架只检查 LQ/SQ free count 和完整 ROB key 是否重复；
- 年轻请求可先释放 LSQ entry，但最老 fault head 仍阻塞 ROB commit；
- 释放的 LSQ slot 被继续复用，admission tail 越过一个完整 ROB 环；
- 一位 wrap flag 只能区分合法的单窗口年龄，无法解释 distance 160/161 的非法跨圈输入。

### 7.2 prefix 外 owner 只是下游症状

第一轮 redirect `ROB=1/75` 面对非法的 `uid4116/ROB=0/76`，环形年龄比较返回“不覆盖”；其他 UID 被 flush 后 prefix 回退，`uid4116` 才落到 prefix 外。第二轮 redirect `ROB=1/76` 又覆盖该物理 SQ owner，DUT 正确 cancel，但框架 prefix scan 已访问不到它。

因此原来的 `ordinary=8, dut=10` 表面上是少扫描一个 owner，真正上游错误却是这个 owner根本不应越过 ROB admission window进入 DUT-visible 状态。

### 7.3 最终修复要求

权威 frontier 使用“下一条尚未 ROB commit 的 UID”。每个 candidate 在 DUT admission/activation 前必须满足：

```text
distance = candidate_uid - rob_commit_frontier_uid
0 <= distance < MEMBLOCK_ROB_SIZE
```

- distance 159：合法，使用最后一个 ROB slot；
- distance 160：正常 stall，等待 commit frontier 推进；
- distance >160：activation 防线 fail-fast；
- batch 跨越边界时逐 UID 截断，不能只检查 batch 首项；
- redirect 后必须断言不存在 prefix 外 active/enq/LQ/SQ/DUT-visible owner。

### 7.4 删除 prefix 外补扫

早期 `active_sq_snapshots + stranded_owner matcher` 补扫曾证明 DUT 第 10 个 cancel 可由 `uid4116` 解释，具有诊断价值，但不是最终修复：

- 它接受了本应被 admission 阻止的非法生命周期；
- 它会掩盖 ROB gate 的遗漏；
- 原正向“构造 prefix 外 owner并补扫成功”场景在新不变量下应改成 admission boundary/containment 负向检查。

最终实现应恢复 redirect 只扫描合法 admission prefix；prefix 外 owner一律作为不变量失败，不再自动归因。

## 8. cfg 降频与 clean 场景

### 8.1 正常权限固定合法

```text
+MEMBLOCK_L2TLB_S1_PTE_R_1_WT=100
+MEMBLOCK_L2TLB_S1_PTE_W_1_WT=100
+MEMBLOCK_L2TLB_S1_PTE_X_1_WT=100
+MEMBLOCK_L2TLB_S1_PTE_U_1_WT=100
+MEMBLOCK_L2TLB_S1_PTE_G_1_WT=0
+MEMBLOCK_L2TLB_S1_PTE_A_1_WT=100
+MEMBLOCK_L2TLB_S1_PTE_D_1_WT=100
+MEMBLOCK_L2TLB_S1_PTE_N_1_WT=0
+MEMBLOCK_L2TLB_S1_PTE_V_1_WT=100

+MEMBLOCK_L2TLB_S2_PTE_R_1_WT=100
+MEMBLOCK_L2TLB_S2_PTE_W_1_WT=100
+MEMBLOCK_L2TLB_S2_PTE_X_1_WT=100
+MEMBLOCK_L2TLB_S2_PTE_U_1_WT=100
+MEMBLOCK_L2TLB_S2_PTE_G_1_WT=0
+MEMBLOCK_L2TLB_S2_PTE_A_1_WT=100
+MEMBLOCK_L2TLB_S2_PTE_D_1_WT=100
+MEMBLOCK_L2TLB_S2_PTE_N_1_WT=0
```

S1-only 统计场景应把 S2 GPF/GAF 置 0；S2 专项则分别打开 GPF/GAF，不能把四种 fault source 混成一个模糊的“2%”。

### 8.2 固定 4KB 的正确配置

`tlb_map_builder::choose_level()` 在 `LEVEL_WEIGHT_EN=0` 时直接返回 level 0。因此 clean 4KB 场景应使用：

```text
+MEMBLOCK_L2TLB_LEVEL_WEIGHT_EN=0
+MEMBLOCK_MAIN_MEM_RANGES_EN=1
+MEMBLOCK_L2TLB_S1_PTE_N_1_WT=0
+MEMBLOCK_L2TLB_S2_PTE_N_1_WT=0
```

level 权重字段在该模式下不参与随机化。`MAIN_MEM_RANGES_EN=1` 保留严格主存物理范围与 ownership 语义，避免旧建议 `LEVEL_WEIGHT_EN=1 + MAIN_MEM_RANGES_EN=0` 引入 sparse PPN 和额外地址语义变化。

4KB 只降低大页相关性并增加独立 canonical entry 数，仍不能保证 UID 级独立 fault；相同 VPN 的 exact/range reuse 仍共享 immutable entry。

### 8.3 clean cfg 隔离其他异常源

用于统计 S1/S2 TLB fault 的 clean cfg 应显式关闭：

```text
+MEMBLOCK_PMA_PMP_MODEL_EN=0
+MEMBLOCK_PMA_PMP_RAW_COMPARE_EN=0
+MEMBLOCK_L2_D_ERROR_STICKY_EN=0
+MEMBLOCK_L2_GRANTDATA_DENIED_WT=0
+MEMBLOCK_L2_GRANTDATA_CORRUPT_WT=0
+MEMBLOCK_L2_CBO_ACK_DENIED_WT=0
+MEMBLOCK_L2_CBO_ACK_CORRUPT_WT=0
+MEMBLOCK_UNCACHE_DENIED_WT=0
+MEMBLOCK_UNCACHE_CORRUPT_WT=0
+MEMBLOCK_L2TLB_S1_PBMT_0_WT=100
+MEMBLOCK_L2TLB_S1_PBMT_1_WT=0
+MEMBLOCK_L2TLB_S1_PBMT_2_WT=0
+MEMBLOCK_L2TLB_S2_PBMT_0_WT=100
+MEMBLOCK_L2TLB_S2_PBMT_1_WT=0
+MEMBLOCK_L2TLB_S2_PBMT_2_WT=0
```

同时固定 PBMT 为合法预期值，并把未测试 stage 的 PF/AF/GPF/GAF 权重全部置 0。PMA/PMP、DCache denied/corrupt、Uncache denied/corrupt 和 PBMT 权限应由独立 testcase 覆盖，不能混入 fault-rate 分母。

## 9. 测试框架参数化方案

### 9.1 策略总开关

新增：

```text
MEMBLOCK_L2TLB_FAULT_POLICY_MODE=LEGACY|ENTRY_PPM|ENTRY_PERIODIC|UID_PLAN
```

四种模式：

| 模式 | 发生粒度 | 用途 |
| --- | --- | --- |
| `LEGACY` | canonical entry，沿用当前四个独立 bit 随机化 | 兼容旧 cfg 和旧 seed |
| `ENTRY_PPM` | 新建 canonical entry | 支持低于 1% 的 ppm 发生率 |
| `ENTRY_PERIODIC` | 每 N 个新建 canonical entry | 确定性 entry 级压力 |
| `UID_PLAN` | 预先冻结的 UID fault plan | 严格控制哪些 scalar UID 产生哪种 fault |

### 9.2 保留四类 fault 语义与旧随机流

`LEGACY` 必须保持：

- `S1_PF_1_WT`、`S1_AF_1_WT`、`S2_GPF_1_WT`、`S2_GAF_1_WT` 的当前独立百分比语义；
- 当前 randomize 调用次数、调用顺序和 field build 顺序；
- 默认 mode 为 `LEGACY`；未配置新参数时不增加任何随机调用；
- 相同旧 cfg + seed 产生相同 fault bit、level、PTE 和 PPN 随机流。

新模式不能重载旧 `*_1_WT` 的语义。建议使用独立 kind 权重：

```text
MEMBLOCK_L2TLB_FAULT_KIND_S1_PF_WT=<N>
MEMBLOCK_L2TLB_FAULT_KIND_S1_AF_WT=<N>
MEMBLOCK_L2TLB_FAULT_KIND_S2_GPF_WT=<N>
MEMBLOCK_L2TLB_FAULT_KIND_S2_GAF_WT=<N>
```

`ENTRY_PPM/ENTRY_PERIODIC` 选中 fault entry 后，再从当前 active stage允许的 kind 中选择；不兼容的 kind 必须 fail-fast，不能静默改成另一类。所有新模式的 **effective fault mask 必须 one-hot**：normal entry 为全 0，fault entry 在 S1 PF、S1 AF、S2 GPF、S2 GAF 中恰好一位为 1；`UID_PLAN` 文件也只能为每个 planned UID 指定一个有效 fault kind。`LEGACY` 保持现有 raw/effective fault 行为、randomize 次数和随机流，不施加会改变旧结果的新 one-hot 检查。

### 9.3 其他参数

```text
MEMBLOCK_L2TLB_FAULT_RATE_PPM=<0..1000000>
MEMBLOCK_L2TLB_FAULT_PERIOD_NEW_ENTRY=<N>
MEMBLOCK_L2TLB_FAULT_MIN_NORMAL_ENTRY_GAP=<N>
MEMBLOCK_L2TLB_FAULT_MAX_ENTRY_COUNT=<N>
MEMBLOCK_L2TLB_FAULT_MAX_LEVEL=<0..3>
MEMBLOCK_L2TLB_FAULT_UID_PLAN_FILE=<path>
```

`ENTRY_PPM` 使用整数均匀随机数 `draw in [0, 999999]`，且仅当 `draw < RATE_PPM` 时把本次新建 canonical entry 标记为 fault。这样 `RATE_PPM=0` 永不命中，`RATE_PPM=1000000` 必然命中，并能直接检查 1 和 999999 两个边界。

`MAX_ENTRY_COUNT` 和 `MIN_NORMAL_ENTRY_GAP` 只适用于 `ENTRY_PPM/ENTRY_PERIODIC`，并且只控制 **fault-bearing canonical entry**：

- 不限制 dispatch UID、DUT request、normal entry、fault feedback、redirect 或 retire；
- exact/range hit 不消耗 count，也不推进 new-entry gap；
- gap 只统计两个 fault entry 之间成功创建的 normal canonical entry；
- `ENTRY_PPM/ENTRY_PERIODIC` 预算耗尽后仍正常创建合法无 fault entry；
- `UID_PLAN` 不读取也不消费 `MAX_ENTRY_COUNT/MIN_NORMAL_ENTRY_GAP`；在该模式显式配置这两个 entry-policy 参数属于配置冲突，启动时 fail-fast；
- `UID_PLAN` 自身的 planned-page/plan-table 容量在加载计划时静态校验，计划超过该预算时 fail-fast，不能截断计划或把 planned UID 静默改成 normal；
- 已发布 entry immutable，预算变化不得改写 exact/range hit 的 fault 属性。

### 9.4 五层实现边界

**1. dispatch 层**

- 在主表/UID 首次构造时生成或导入稳定的 `fault_plan_id`、四位 fault mask、期望 stage 和 mapping coverage；
- 对每个 planned UID 预先分配一个独占 4KB VPN/page，并在 dispatch 时冻结 UID 到 page 的一一映射；
- planned UID 的地址生成禁止地址复用、禁止跨越 4KB 页边界，全部访问必须落在其独占 page 内；
- unplanned UID 禁止使用任何 planned page；地址分配冲突必须在 request 发出前 fail-fast；
- `UID_PLAN` 决策绑定 UID dynamic identity，redirect/reissue 不重新抽样；
- 不在 writeback 或 redirect handler 中限流真实异常。

**2. common_data 层**

- 持有 policy mode、entry counter、fault-entry budget、gap、UID plan table和冲突诊断；
- 在 canonical miss 创建前解析 UID intent；
- 只接收 dispatch 已冻结的 UID/page/mask 关系；planned UID 若命中不兼容的既有 mapping 必须 fail-fast，不能替换 mapping 或修改 live entry；
- exact/range hit 始终复用已发布 entry。

**3. L2TLB sequence 层**

- PTW request fire 时审计与该 request/page 匹配的全部 `WAITING` UID，不能只取第一个或第一个未标记 match；
- 同一 request/page 同时匹配 planned 和 unplanned UID，或匹配多个 planned UID 时立即 fail-fast；
- request accept 时只消费 dispatch/common_data 已冻结的 UID plan provenance，并把 canonical create 决策传给 builder；
- response 只驱动 frozen entry，不按当前 UID 临时翻转 PF/AF/GPF/GAF；
- L2TLB response path 只消费 dispatch/common_data 已冻结的 mapping，不承担任何 mapping 决策；
- 输出 token、UID、anchor、level、policy mode、plan id、fault mask 和 created/hit telemetry。

**4. builder 层**

- `LEGACY` 走当前代码路径，保持旧随机调用顺序；
- 新模式只消费 common_data 已给出的 occurrence/kind 决策，负责构造合法 PTE payload；
- 不持有全局 budget，不在 range hit 时二次随机。

**5. entry 层**

- 保存 immutable 的 S1 PF/AF、S2 GPF/GAF、level、anchor、coverage 和 policy provenance；
- entry 发布后禁止任何 UID、预算或 redirect 修改这些字段；
- debug/统计以 entry provenance 为权威，不从重复 feedback 反推 occurrence。

### 9.5 参数冲突与优先级

- `FAULT_POLICY_MODE` 是唯一 occurrence source，禁止同时启用多个 mode；
- `LEGACY` 忽略新 occurrence 参数，但若显式配置非默认值应 warning 或 strict-mode fatal；
- `ENTRY_PPM` 只接受 `RATE_PPM`；`ENTRY_PERIODIC` 只接受 `PERIOD_NEW_ENTRY`；
- `UID_PLAN` 只接受 plan file/内建 plan，不读取 PPM/period 做二次抽样；
- `MAX_ENTRY_COUNT/MIN_NORMAL_ENTRY_GAP` 仅是 `ENTRY_PPM/ENTRY_PERIODIC` 的约束器，不是新的 occurrence source；`UID_PLAN` 下显式启用即为配置错误；
- `MAX_LEVEL` 可用于 entry 模式；`UID_PLAN` 固定使用 dispatch 预分配的独占 4KB page，不由 `MAX_LEVEL` 改写；
- 所有冲突在 sequence 启动前由 `SEQ_CSR_CFG` fail-fast。

### 9.6 Runtime reset 语义

fault policy 和 UID plan 是 **testcase 全局状态**，runtime reset 不是新 testcase，也不是 fault occurrence 统计的新 epoch。reset 前后必须连续保留：

- canonical entry ordinal 和 periodic ordinal；
- 已消耗的 fault-entry budget；
- 当前 normal-entry gap 计数；
- UID immutable plan、fault kind、独占 page 归属及其已消费 decision；
- 用于判断某个 planned UID 是否已经消耗 occurrence/budget 的稳定 decision identity。

runtime reset 只清理本轮传输态：in-flight request、response token、pending queue，以及这些 in-flight 对象到 mapping/entry 的临时绑定。reset 不得重新抽样 fault、不把 ordinal 归零、不返还 budget，也不得清除 planned UID 的已消费 decision。

如果 reset 发生在 entry create 后、response 前，reset 后同一 planned UID 需要重建 mapping 时，必须复用原 immutable plan 和原 effective fault decision；该重建不产生新的 occurrence draw、不推进 periodic ordinal、不重复消耗 budget。这样可避免通过插入 reset 人为增加 fault entry 数或 fault 比例。

## 10. 验证矩阵

| 类别 | 配置/场景 | 关键验收 |
| --- | --- | --- |
| 历史失败复核 | 历史命令行、PMA=0、seed `676087581` | 原统计和原 fatal 可复现；仅用于证据冻结 |
| 当前真实 seed | 当前 cfg、PMA=1、ROB window 修复 | **已完成**：10,000 笔自然 `TEST_PASS`，error/fatal=0 |
| ROB boundary | distance 159/160/161 | 159 可 admission；160 stall；绕过 gate 的 >160 activation fatal |
| Batch boundary | batch 跨 distance 159->160 | 只发送窗口内 UID，后续 slot 保持 invalid |
| Prefix containment | redirect + wrap + fault head | redirect 后无 prefix 外 active/enq/LQ/SQ/DUT-visible owner；无补扫日志 |
| 旧补扫删除 | 源码和 factory 静态检查 | 无 `active_sq_snapshots/stranded_owner matcher` 最终路径；旧测试改为边界/断言测试 |
| clean no-fault | 合法 PTE、4KB、四类 TLB fault=0、PMA/denied/corrupt=0 | 10,000 笔完成，无架构 fault，error/fatal=0 |
| S1 PF directed | S1 PF=100，其余 fault source=0 | 只出现预期 Page Fault，cause/writeback/redirect/retire 完整 |
| S1 AF directed | S1 AF=100，其余=0 | 只出现预期 Access Fault |
| S2 GPF directed | S2 GPF=100，启用对应 S2 path | 只出现预期 Guest Page Fault |
| S2 GAF directed | S2 GAF=100，启用对应 S2 path | 只出现预期 Guest Access Fault |
| `s2xlate` mode | 依次覆盖 0/1/2/3 | 0=`noS2xlate`、1=`onlyStage1`、2=`onlyStage2`、3=`allStage`；每种模式只接受兼容的 S1 PF/AF 或 S2 GPF/GAF，非法 kind 配置启动时 fail-fast |
| 4KB cfg | `LEVEL_WEIGHT_EN=0 + MAIN_MEM_RANGES_EN=1 + N=0` | response level 全为 0；PPN 保持主存范围；无 sparse-mode 副作用 |
| LEGACY 随机流 | 旧 cfg/seed，修改前后对照 | entry fault bits、level、PTE/PPN 和 request序列一致 |
| 新模式 one-hot | `ENTRY_PPM/ENTRY_PERIODIC/UID_PLAN` 分别覆盖四种 fault kind 和非法多 bit mask | normal mask 全 0；fault mask 恰好一位有效；多 bit、无兼容 kind 或 stage 不匹配均 fail-fast；`LEGACY` golden seed 不变 |
| ENTRY_PPM=0 | `RATE_PPM=0` | 对每个 new entry 取 `draw in [0,999999]`，`draw < 0` 永假，无 fault entry |
| ENTRY_PPM=1 | `RATE_PPM=1`，directed draw 覆盖 0/1 | draw=0 fault，draw=1 normal；验证下边界无 off-by-one |
| ENTRY_PPM=999999 | `RATE_PPM=999999`，directed draw 覆盖 999998/999999 | draw=999998 fault，draw=999999 normal；验证上边界无 off-by-one |
| ENTRY_PPM=1000000 | `RATE_PPM=1000000` | 所有 new entry 都 fault；exact/range hit 不二次抽样或消耗预算 |
| ENTRY_PERIODIC N=0 | `PERIOD_NEW_ENTRY=0` | 配置阶段 fail-fast，不运行含糊的关闭/除零语义 |
| ENTRY_PERIODIC N=1 | `PERIOD_NEW_ENTRY=1` | 第一个及之后每个 new entry 都 fault；hit/reissue 不推进周期 |
| ENTRY_PERIODIC off-by-one | `PERIOD_NEW_ENTRY=2` | 第一个 new entry normal、第二个 fault，此后每两个 new entry 重复 |
| UID_PLAN 基本功能 | 固定 UID/mask/独占 4KB page | 仅 planned UID fault；reissue 不重抽；planned UID 不复用/跨页，unplanned UID 不进入 planned page |
| UID_PLAN 超预算 | plan 数量超过 planned-page/plan-table 容量 | plan 加载阶段 fail-fast；不得截断、回退为 normal 或运行到请求阶段才失败 |
| UID_PLAN request 冲突 | 同一 PTW request/page 匹配 planned+unplanned，或匹配多个 planned `WAITING` UID | 审计全部 match 并一律 fail-fast；不得只取第一个或未标记 match |
| MAX_COUNT=0 | `ENTRY_PPM/ENTRY_PERIODIC`，`MAX_ENTRY_COUNT=0` | 不创建 fault entry，但继续创建 normal entry并完成请求 |
| MAX_COUNT=1 | `ENTRY_PPM/ENTRY_PERIODIC`，`MAX_ENTRY_COUNT=1` | 仅首个满足策略和 gap 的 new entry 可 fault，hit 不重复消耗 count |
| MAX_COUNT 耗尽 | `ENTRY_PPM/ENTRY_PERIODIC` 达到 count 后继续产生 new entry | 后续 entry 全部 normal，不阻塞请求，不重置或透支预算 |
| MIN_GAP=0 | `ENTRY_PPM/ENTRY_PERIODIC`，`MIN_NORMAL_ENTRY_GAP=0` | 允许相邻两个 new entry 都成为 fault entry |
| MIN_GAP=1 | `ENTRY_PPM/ENTRY_PERIODIC`，`MIN_NORMAL_ENTRY_GAP=1` | 两个 fault entry 之间必须成功创建一个 normal new entry；hit 不计 gap |
| UID_PLAN 参数隔离 | `UID_PLAN` 下显式配置 `MAX_ENTRY_COUNT` 或 `MIN_NORMAL_ENTRY_GAP` | 配置阶段 fail-fast；不得把 entry-policy budget/gap 应用于 UID plan |
| Range-hit immutable | fault/normal entry反复 hit | payload 与 provenance不变，不因 UID plan或预算改写 |
| SFENCE 重建 | canonical entry 创建后执行匹配 SFENCE，再访问同一地址 | 旧 entry 失效；按当前冻结策略重建唯一 canonical entry，无 stale hit/重复 owner |
| Runtime reset 全局策略连续 | policy 已推进 entry/periodic ordinal、budget、gap 和 UID decision 后 reset | **PASS**：上述 testcase 全局状态逐项保持，reset 后下一 new entry 延续原 ordinal/gap/budget；**FAIL**：任一计数归零、budget返还、period重启或重新抽样 |
| Create 后 runtime reset | entry create 后、response 前 reset，之后重放同一 planned UID | **PASS**：清理 in-flight request/token/mapping绑定，无 stale response；重建复用原 plan/decision且不重复消耗 budget或推进 ordinal；**FAIL**：生成第二个 decision、再次扣 budget或提高 fault 比例 |
| Create 后 flush | entry create 后、response 前触发非 reset flush | telemetry 完整记录 create/cancel；不得返回 stale response；同一已消费 decision 不得因重放而二次消耗 budget |
| PMA 隔离 A/B | PMA=0 与 PMA=1，其他 clean | 单独记录 PMA 引入的 fault，不混入 TLB fault-rate 主指标 |
| denied/corrupt 隔离 | 全 0 与 directed 100 | clean 场景无注入；directed 场景独立覆盖对应 error path |
| 高频 stress | 保留历史异常权限或专用 stress cfg | ROB window始终合法，cancel精确对账，最终完整通过 |

### 10.1 统计主指标

entry 模式主指标：

```text
fault-bearing canonical entry / 新建 canonical entry
```

建议至少 50 seeds、累计新建 entry >= 10000；累计比例使用预先定义的容差和 99% exact-binomial 区间，同时检查逐 seed prediction interval。辅助指标单独记录：exact/range hit、各 level、fault kind、unique feedback UID、重复 feedback event、redirect owner、retire 和 transaction completion。

UID 模式主指标：

```text
首次产生计划 fault 的 scalar UID / UID_PLAN 中的目标 UID
非计划 fault scalar UID / 非计划 scalar UID
```

第一项必须 100%，第二项必须 0；重复 feedback、redirect 和 retire 只作为生命周期闭环指标，不作为 occurrence 分母。

### 10.2 建议 telemetry

```text
L2TLB_ENTRY_CREATE seed=<seed> anchor=<vpn> level=<n> s2xlate=<n>
  policy=<LEGACY|ENTRY_PPM|ENTRY_PERIODIC|UID_PLAN>
  plan_id=<id> uid=<uid|none> fault_mask=<s1pf,s1af,s2gpf,s2gaf>
  decision_id=<id> entry_ordinal=<n> periodic_ordinal=<n>
  created=<0/1> lookup=<exact|range|miss> budget=<used/max> gap=<n>
  r/w/x/u/a/d/v=<bits>
```

该日志只提供观测，不改变 stimulus。runtime reset 前后同一已消费 decision 必须保持相同 `decision_id`，用它证明重建没有重新抽样或重复扣 budget。统计必须以 entry create/provenance 为分子分母，不能从 122075 次重复 feedback 反推 fault 发生率。

## 11. 风险与未决点

1. 固定合法权限会移除随机权限负向覆盖，必须拆成独立 permission-fault testcase。
2. 固定 4KB 会减少 superpage/range-hit 覆盖，需保留独立大页回归。
3. PMA=0 的历史复核与 PMA=1 的当前通过不能合并成一个“同配置通过”结论。
4. `UID_PLAN` 的独占 4KB page 必须在 dispatch 前完成规划并冻结；若地址生成仍允许 planned page 被复用、跨页或被 unplanned UID 使用，PTW request fire 的全量 `WAITING` 审计必须 fail-fast，L2TLB response path 仍只消费原冻结 mapping。
5. ROB backpressure 会改变绝对 cycle、record id 和后续随机消费；问题3验收应看 admission/containment 不变量和最终结果，不绑定旧时间戳。
6. `LEGACY` 随机流兼容需要实际 golden seed 对照，静态设计约束不能替代回归。

## 12. 关键源码与证据位置

- ROB admission window 根因/方案：`AI_DOC/plan/test_framework/plan/undo/memblock_v2_rob_admission_window_plan_20261013.md`
- 问题3历史状态：`AI_DOC/analysis/source_sv/fault_2pct_status_20261010.md`
- level 关闭时固定 4KB：`mem_ut/ver/ut/memblock/seq/base_seq_help/tlb_map_builder.sv:108-140`
- PTE bit 百分比采样：`mem_ut/ver/ut/memblock/seq/base_seq_help/tlb_map_builder.sv:20-33`
- LEGAL fixup：`mem_ut/ver/ut/memblock/seq/base_seq_help/tlb_map_builder.sv:252-269`
- fault raw 采样与 effective one-hot 折叠：`mem_ut/ver/ut/memblock/seq/base_seq_help/tlb_map_builder.sv:335-395`
- U-mode 权限判定：`mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv:235-279`
- L2TLB request accept 日志：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv:1178-1184`
- L2TLB response payload与 fault 字段：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv:1566-1628`（S1 PF/AF 约 1611/1612，S2 GPF/GAF 约 1626/1627）
- range hit matcher：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv:5354-5474`
- Page Fault cause 映射：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_ls_core.sv:466-485`

## 13. 第四版最终判断

问题3已经不能再描述为“redirect 扫描范围少扫一个 owner”。准确表述是：测试框架缺少 ROB admission window，允许 UID 跨越完整 ROB 容量进入 DUT-visible；prefix 外 owner和 cancel mismatch只是该非法状态经过两轮 redirect 后的表现。最终修复必须在上游限制 ROB admission，并删除 prefix 外自动补扫。

历史 cfg 又通过错误 U/W 权限和 1GB mapping 把 scalar fault redirect率放大到 100%，因此非常容易持续施压 redirect/ROB/LSQ 生命周期。降频方案用于恢复 testcase 的 2% fault 意图；ROB admission window用于修复功能根因，两者必须独立验证，不能互相替代。
