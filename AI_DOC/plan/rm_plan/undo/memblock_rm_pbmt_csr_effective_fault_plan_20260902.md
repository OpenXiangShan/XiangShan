# memblock RM PBMT CSR 有效页故障推导方案（2026-09-02）

| 项目 | 内容 |
| --- | --- |
| 状态 | 实现完成。RM PBMT overlay 与 software-only 检查已通过；与 L2TLB dynamic smoke 的 real-DUT 联合验证被已确认的 V2 RTL X 传播缺陷阻塞，plan 保持在 `undo`。 |
| 方案类型 | RM plan |
| 目标版本 | V2 |
| 当前分支 | `codex/pbmt-rm-l2tlb-20260902`（V2 基线：`mem_ut_uvm_v2`） |
| 适用范围 | `memblock_rm` 的 scalar Load/Store 翻译异常推导、TLB readonly API、共享 MMU CSR helper 与定向验证 |
| 关联测试框架方案 | [`mem_ut_v2_l2tlb_pbmt_csr_gate_plan_20260902.md`](../../test_framework/plan/undo/mem_ut_v2_l2tlb_pbmt_csr_gate_plan_20260902.md) |
| 关联分析 | [`memblock_mmu_sv39_rm_exception_mismatch_analysis_20260827.md`](../../../analysis/framework_design/memblock_mmu_sv39_rm_exception_mismatch_analysis_20260827.md) |
| 关联 RM 方案 | [`memblock_rm_pma_pmp_model_plan_20260828.md`](./memblock_rm_pma_pmp_model_plan_20260828.md)、[`memblock_rm_l2_dcache_sticky_error_ledger_plan_20260828.md`](./memblock_rm_l2_dcache_sticky_error_ledger_plan_20260828.md) |
| 代码修改授权 | 已按本轮用户授权执行本 plan 的 RM、sequence、testcase 与必要接线；未修改 RTL。 |
| 创建日期 | 2026-09-02 |

## 1. 术语与职责

| 术语 | 中文含义 | 本方案中的职责 |
| --- | --- | --- |
| PBMT | 页表叶子项中的 Physical Memory Attribute 编码；当前框架合法编码为 `00`、`01`、`10`。 | raw live entry 保留该值，RM 据此判断 PBMTE 关闭时是否需要附加页故障。 |
| PBMTE | `mPBMTE` 或 `hPBMTE` CSR 使能位。 | 不是 TLB key，也不触发本方案中的表项删除；只决定当前翻译结果中的非零 PBMT 是否合法可见。 |
| raw live entry | `common_data_transaction::tlb_entry_by_key` 中可被后续请求复用的原始 TLB 表项。 | 提供 PTE、PBMT、PPN、raw/effective PTE fault 和几何信息；本方案绝不写回它。 |
| effective RM fault | RM 对一个 UID、一个 byte/page 命中项按冻结 CSR 计算的最终 S1 PF/S2 GPF。 | 由既有 PTE/PMA/permission fault 与本方案 PBMT overlay 做 OR，驱动 `expected_exception`。 |
| S1 PF | 第一阶段 page fault。 | 对应 Load 的 exception bit 13、Store 的 exception bit 15。 |
| S2 GPF | 第二阶段 guest-page fault。 | 对应 Load 的 exception bit 21、Store 的 exception bit 23。 |
| `tlb_context` | `read_tlb_request_context_for_rm()` 从 UID record 返回的冻结翻译上下文。 | 提供 `s2xlate`、`m_pbmt_en`、`h_pbmt_en` 及 ASID/VMID/translation mode；RM 禁止读取提交时的 runtime current CSR。 |
| response overlay | L2TLB responder 在 response select 时，对 token 私有 payload 叠加的 PBMT PF/GPF。 | 与 RM 的本地推导必须同义，但不是本方案 RM 的唯一输入，也不要求 RM 消费 token payload。 |

本方案的所有逻辑属于 `memblock_rm::observer_build_commit_item()` 的每次 commit 构建路径。
该路径已有最多 8 个 byte 的翻译循环；新增判断只能在当前 byte 已命中的 entry 上做常数时间计算，
不得扫描 UID 表、TLB 表、pending queue 或历史 response 表。

## 2. 背景与当前缺口

当前 RM 的翻译路径如下：

```text
UID record.csr_snapshot
    -> read_tlb_request_context_for_rm()
    -> resolve_tlb_entry_key_for_rm()
    -> read_tlb_entry_for_rm()
    -> raw entry.fault_effective_s1_pf / fault_effective_s2_gpf
    -> expected_exception
```

`read_tlb_request_context_for_rm()` 已把 `m_pbmt_en/h_pbmt_en` 复制到 `tlb_context`，
但 `observer_build_commit_item()` 当前没有读取它们来检查
`s1_entry_pbmt/s2_entry_pbmt`。因此，若一个 raw live entry 在 PBMTE=1 时生成非零 PBMT，
随后 PBMTE 在前一条指令完成后关闭，后续 UID 命中同一 entry 时会发生以下不一致：

```text
raw live entry: S1 PBMT=01, raw/effective S1 PF=0
后续 UID 的冻结 CSR: mPBMTE=0
L2TLB response: PBMT=01, S1 PF=1
当前 RM: 只看 raw entry 的 S1 PF=0，期望正常访问
```

这会导致 DUT 已上报 PF/GPF，而 RM 仍期待正常 Load data 或 Store 正常完成，最终形成
`RM_LS_ERR_EXCEPTION_MISMATCH`。S2 和 `allStage` 场景同理。

## 3. 目标、边界与时序合同

### 3.1 目标

1. RM 对每个已解析的 page entry，按 raw PBMT 和该 UID 的冻结 PBMTE 独立推导 S1 PF/S2 GPF。
2. PBMT 推导结果与 L2TLB responder 的 response overlay 使用完全一致的 stage-to-PBMTE 映射。
3. PBMT overlay 与既有 raw PF/AF/GPF/GAF、permission fault 使用 OR，不能覆盖或清除任何既有异常。
4. 有 PBMT overlay 的翻译被视为翻译失败：RM 不再为该 byte 建立正常 PA，也不继续进入 PMA/PMP 或正常 memory-data 期望路径。
5. raw live entry、TLB key、entry generation、UID payload 访问接口和 responder 生命周期保持不变。

### 3.2 明确不做的事项

- 不修改 Scala/Chisel RTL。
- 不把 PBMT overlay 写回 `tlb_entry_by_key`，也不改写 `entry.fault_effective_*`。
- 不让 RM 读取当前 runtime CSR；不使用 commit 时的 CSR 值覆盖 UID 的冻结上下文。
- 不新增 PBMT runtime plusarg、第二份 TLB 表、UID-to-token 映射或全表扫描。
- 不在本方案中实现 PBMT=`11` 的架构语义；当前生成器和 responder 都将 active stage 的 `11` 视为框架输入不一致。
- 不把 responder 的 `response_pbmt_forced_s1_pf/s2_gpf` 作为 RM 正常功能的真源。它们可以用于 debug 交叉核验，但 RM 必须能仅凭 raw entry 与冻结 CSR 重建期望。

### 3.3 CSR 时序合同

本方案依赖当前已约定的 CSR 串行化：PBMTE 写入只能在其目标指令、该指令的 L2TLB token
以及终态记账完成后发生。因此，对同一 UID：

```text
UID request-time frozen PBMTE == 该 UID response-visible PBMTE
```

RM 使用 `tlb_context.m_pbmt_en/h_pbmt_en` 是正确的，不需要新建 RM 侧 response CSR snapshot。
若未来允许 PBMTE 在一个 outstanding UID/token 期间改变，本方案不得继续直接使用 UID request
snapshot；必须改为由 responder 在 response sample 冻结 PBMTE 后，把同一份可见上下文提供给 RM。
该未来扩展不属于本方案。

## 4. 选定架构

RM 保留 raw live entry 作为 PPN、PTE、权限和几何的来源，但在每次 byte/page lookup 后计算一份
局部 `pbmt_force_s1_pf/pbmt_force_s2_gpf`。该局部结果只影响当前
`rm_ls_program_item_t` 的 expected fault 与异常位：

```text
raw live entry + UID frozen tlb_context
                  |
                  v
       RM PBMT stage-enable evaluation
                  |
                  v
  local S1 PF / S2 GPF overlay for this byte
                  |
                  v
raw PTE/PMA/permission fault OR local PBMT overlay
                  |
                  v
       expected_exception / normal-path gate
```

该架构选择的原因：

- A/B/C 三次复用同一 raw live entry 时，每个 UID 可以按自己的冻结 CSR 独立判断；PBMTE
  重新开启后不会继承 B 的 fault。
- RM 不依赖 responder 内部 token 的生命周期、payload handle 或 UID completion 顺序。
- 现有 `tlb_entry_by_key` lookup 和 range-hit 路径不变，跨页访问仍按当前 byte 循环分别查询。

## 5. stage-to-PBMTE 共同语义

必须只有一套映射实现。L2TLB PBMT 方案中的 instance helper
`mmu_csr_runtime_state::get_stage_pbmt_enable()` 应建立在一个可供 RM 调用的纯值型 static helper 之上：

```systemverilog
static function bit get_stage_pbmt_enable_from_bits(
    input bit       is_s1,
    input bit [1:0] s2xlate,
    input bit       m_pbmt_en,
    input bit       h_pbmt_en
);
```

instance helper 只把对象内的 `m_pbmt_en/h_pbmt_en` 传给该 static helper；RM 则把
`tlb_context` 中的同名字段传入。映射必须固定如下：

| `s2xlate` | active S1 的 PBMTE | active S2 的 PBMTE |
| --- | --- | --- |
| `2'd0` | `m_pbmt_en` | 不存在 |
| `2'd1` | `h_pbmt_en` | 不存在 |
| `2'd2` | 不存在 | `m_pbmt_en` |
| `2'd3` | `h_pbmt_en` | `m_pbmt_en` |

`is_s1` 与 active stage 形状不匹配时，调用点必须先以已有 translation path/entry 一致性检查
拒绝该 entry，而不是把不存在 stage 当作 disabled PBMTE 产生虚假的 PF/GPF。

共享 helper 是两个专项的共同前置：若先执行 RM plan，必须同时落地该 static helper 与 instance
wrapper；若先执行 L2TLB PBMT plan，RM 只复用其已落地的 static helper，禁止在 RM 再写一份
`case (s2xlate)`。

## 6. RM 实现方案

### 6.1 `mmu_csr_runtime_state::get_stage_pbmt_enable_from_bits()`

**抽象功能描述：** 此 helper 接收一个已冻结的 `s2xlate` 和两个 PBMTE bit，返回指定翻译 stage
在该 CSR 上下文下是否允许 nonzero PBMT。它不读取 runtime state、不修改 CSR 对象，也不访问 TLB
entry 或 UID 表。

**函数目的：** 让 L2TLB responder 的 response overlay 和 RM 的 independent expectation 使用同一
stage 映射，避免 M/HS/VS/allStage 的 PBMTE 选择出现双重实现漂移。

**输入：** `is_s1`、`s2xlate`、`m_pbmt_en`、`h_pbmt_en`。

**输出/副作用：** 返回 enable bit；无状态副作用。

**源码级伪代码：**

```text
get_stage_pbmt_enable_from_bits(is_s1, s2xlate, m_en, h_en):
  case s2xlate:
    00: return is_s1 ? m_en : 0
    01: return is_s1 ? h_en : 0
    10: return is_s1 ? 0    : m_en
    11: return is_s1 ? h_en : m_en
    default: uvm_fatal or caller-visible invalid-context failure
```

**中文文字伪代码：** 函数只负责选择 enable bit，不判断 PBMT 是否非零，也不决定 PF/GPF。
调用方已经通过 `rm_ls_decode_translation_path()` 确定 active stage；因此返回 0 的不存在 stage
不能被解释为“该 stage 因 PBMTE 关闭而 fault”。instance wrapper 与 RM 都调用这一个 pure backend。

### 6.2 `memblock_rm::observer_eval_pbmt_fault_overlay()`

**抽象功能描述：** 此新增 helper 在 RM 已经为一个访问 byte 找到 canonical/range-hit TLB entry
且验证 active stage 形状后，基于 `tlb_context` 为该 entry 计算本次访问新增的 S1 PF/S2 GPF。
它只消费当前 entry 和当前 UID 的冻结上下文，返回局部 force bit，不修改任何公共表或 entry。

**函数目的：** 将 PBMTE/PBMT 合法性从 raw entry 的持久状态中剥离，使同一 live entry 可被不同
PBMTE 上下文的后续 UID 安全复用。

**输入：**

- 当前 byte 对应的 `tlb_request_context_view_t tlb_context`；
- `tlb_entry_view_t entry`；
- `s1_active/s2_active`；
- 输出用的 `force_s1_pf/force_s2_gpf` 与 `rm_ls_error_e`。

**输出/副作用：** 对有效输入返回 1，输出本 entry 的 PBMT force bit；对 active PBMT=`11`、
inactive stage 带非零 PBMT 或不一致 stage 形状，返回 0 并由调用者使用既有
`RM_LS_ERR_TLB_ENTRY_INCONSISTENT` 终止当前 RM item 构造。正常 PBMT-induced fault 不是 RM error。

**源码级伪代码：**

```text
observer_eval_pbmt_fault_overlay(context, entry, s1_active, s2_active):
  force_s1_pf  = 0
  force_s2_gpf = 0

  validate inactive-stage PBMT is 00
  validate each active-stage PBMT is not 11

  if s1_active:
    s1_enabled = get_stage_pbmt_enable_from_bits(
      1, context.s2xlate, context.m_pbmt_en, context.h_pbmt_en)
    force_s1_pf = (entry.s1_entry_pbmt != 00) && !s1_enabled

  if s2_active:
    s2_enabled = get_stage_pbmt_enable_from_bits(
      0, context.s2xlate, context.m_pbmt_en, context.h_pbmt_en)
    force_s2_gpf = (entry.s2_entry_pbmt != 00) && !s2_enabled

  return success
```

**中文文字伪代码：** helper 先将两个 force bit 清零，随后只校验当前 entry 的 PBMT 编码和
active shape。若 S1 active，使用 S1 规则取得 enable bit；仅当 S1 PBMT 非零且该 enable bit 为 0
时置 S1 PF。S2 同理，但置的是 GPF。两个分支相互独立，`s2xlate=3` 下可以同时置位。
helper 不写 `entry.fault_effective_*`，不调用 TLB lookup，也不遍历 UID；每次调用是 O(1)。

### 6.3 修改 `memblock_rm::observer_build_commit_item()`

**抽象功能描述：** 此函数在一个已提交的 scalar Load/Store 上建立 RM 的翻译、异常和可比较数据
期望。改动只插入当前按 byte 查询 TLB entry 的循环：先产生本 byte 的 PBMT overlay，再合并进
既有 stage fault，最终由原有 exception-vector 代码编码为架构异常。

**函数目的：** 使 RM 的异常期望与 L2TLB 对同一 raw PBMT entry 的 response-visible PBMTE
行为一致，同时不改变现有 lookup、PMA/PMP 和 memory model 的所有权。

**输入：** 既有 `main_view`、`status_view`、UID `tlb_context`、当前 byte 的 `entry`。

**输出/副作用：** 更新当前 `rm_ls_program_item_t` 的 expected exception、PBMT 诊断字段和必要的
PA valid mask；不修改 `tlb_entry_by_key`、UID record 或 CSR runtime state。

**源码级伪代码：**

```text
for each byte in scalar access:
  resolve current byte VPN to tlb_context.entry_key
  read raw entry
  run existing entry/context shape checks

  if !observer_eval_pbmt_fault_overlay(...):
    ls_model.set_error(RM_LS_ERR_TLB_ENTRY_INCONSISTENT, diagnostic)
    return 0

  item.expected_pbmt_forced_s1_pf |= force_s1_pf
  item.expected_pbmt_forced_s2_gpf |= force_s2_gpf

  entry_stage_one_fault = entry.fault_effective_s1_pf ||
                          existing_s1_permission_fault || force_s1_pf
  entry_stage_two_fault = entry.fault_effective_s2_gpf ||
                          existing_s2_permission_fault || force_s2_gpf
  stage_one_fault |= entry_stage_one_fault
  stage_two_fault |= entry_stage_two_fault

  if !entry.fault && !entry.pma_af && !force_s1_pf && !force_s2_gpf &&
     tlb_context.request_translation_valid:
    preserve existing PA derivation

  entry_translation_fault = existing raw/PMA/permission fault OR
                            force_s1_pf OR force_s2_gpf
  if entry_translation_fault:
    break

preserve existing PMA/PMP eligibility gate and exception-bit encoding
```

**中文文字伪代码：** 循环的每一轮仍先按当前 byte 的 VPN 解析 entry，因而跨页 Load/Store 会让
两个 page entry 分别接受 PBMT 检查。helper 成功后，S1/S2 force bit 分别 OR 到既有阶段 fault；
不能使用赋值覆盖，因为 raw PF/GPF、permission fault 和 PBMT fault 可以同时成立。

PBMT force bit 在 PA 填充前进入该 byte 的翻译失败 gate。这一点只针对新增 PBMT fault：现有
permission-fault 的 PA 保留行为不在本方案中改变。之后既有 `entry_translation_fault` 会立即
停止跨页后续 lookup，避免 RM 在已经确定 page/guest-page fault 后伪造下一页 normal translation。
循环外原有 `!access_fault && !stage_one_fault && !stage_two_fault` PMA/PMP gate 因 force bit 已 OR
而自然关闭；现有 Load data compare 也会因 `expected_exception != 0` 自然跳过。

### 6.4 `rm_ls_program_item_t` 诊断字段与 trace

新增以下只读诊断字段，初始化为 0：

```systemverilog
bit expected_pbmt_forced_s1_pf;
bit expected_pbmt_forced_s2_gpf;
```

它们只记录当前 scalar access 的任一 byte 是否由 PBMT/PBMTE 规则额外产生对应 fault，不改变
`expected_exception` 的已有位定义。`RM_LS_TRACE_TRANSLATION` 日志需增加：`s2xlate`、
`m_pbmt_en`、`h_pbmt_en`、两个 force bit，以及发生 force 的 byte index/entry key。
这类字段不进入跨 transaction 公共 map，不增加 commit 路径扫描或生命周期状态。

## 7. 异常优先级与合法性

| 条件 | RM 处理 | 原因 |
| --- | --- | --- |
| active S1、非零 `s1_entry_pbmt`、对应 PBMTE=0 | OR `force_s1_pf` | 合法的 PBMT-induced S1 page fault。 |
| active S2、非零 `s2_entry_pbmt`、对应 PBMTE=0 | OR `force_s2_gpf` | 合法的 PBMT-induced S2 guest-page fault。 |
| `allStage` 下 S1 和 S2 都满足条件 | 两个 force bit 都置位 | PF 与 GPF 可以同时出现在 exception vector。 |
| raw PF/GPF/AF/GAF 已为 1 | 继续 OR，不丢失 raw fault | PBMT overlay 不得改变原有异常来源。 |
| PBMT=`00` | 不新增 fault | enable/disable 都不能让默认 PBMT 产生异常。 |
| active stage PBMT=`11` | `RM_LS_ERR_TLB_ENTRY_INCONSISTENT` | 当前框架生成/response 都不支持保留编码，不能猜测其架构含义。 |
| inactive stage PBMT 非零 | `RM_LS_ERR_TLB_ENTRY_INCONSISTENT` | entry shape 已损坏，不能将不存在 stage 解释为 PBMTE fault。 |
| PBMTE=0 时创建的新 entry | builder 应已把 PBMT 收敛为 `00`；RM 不额外造 fault | 创建期 gate 是正常 payload 约束，不是 response-time invalid entry。 |

对 Load，S1/S2 fault 继续复用当前 bit 13/21；对 Store，继续复用 bit 15/23。
PBMT overlay 是合法 DUT 异常，不应作为 RM 的内部错误或非法激励过滤条件。

## 8. 与 L2TLB PBMT 方案的接口边界

本 RM plan 与关联 L2TLB plan 的职责分界如下：

| 对象 | L2TLB PBMT 方案 | 本 RM 方案 |
| --- | --- | --- |
| raw live entry | 创建期按 request PBMTE gate 选择 PBMT，之后不改写。 | 只读查询，绝不写回。 |
| response token | response select 时冻结 effective PF/GPF payload 并驱动 DUT。 | 不读取 token handle 或 response payload 作为正常真源。 |
| CSR | response C-2 用于 driver payload 的时间精确性。 | UID frozen `tlb_context` 用于 expectation；依赖 CSR 串行化合同。 |
| PBMTE 映射 | 调用共享 instance helper。 | 调用同一 static pure helper。 |
| PBMT fault | 写入 token 私有 effective payload。 | 写入当前 RM item 的局部 expected fault。 |

real-DUT 定向测试必须在两项方案都落地后执行。若 responder 已返回 PBMT-induced PF/GPF，
但 RM 仍未生成相同 expected exception，测试应报告 RM mismatch；不得通过关闭 RM 或忽略异常
使场景通过。

## 9. 验证方案

### 9.1 software-only RM helper 检查

已新增 `soft_test_rm_pbmt_effective_fault_sequence` 与对应 testcase。该测试只构造
`tlb_context`、entry view 和 RM helper 输入，不驱动真实 DTLB/L2TLB wire，不建立 lifecycle owner。

必须覆盖：

1. `s2xlate=0`：S1 只受 `m_pbmt_en` 控制；`PBMT=01/10` 在 disabled 时 force S1 PF。
2. `s2xlate=1`：S1 只受 `h_pbmt_en` 控制，`m_pbmt_en` 变化不能影响结果。
3. `s2xlate=2`：S2 只受 `m_pbmt_en` 控制，disabled 时 force S2 GPF。
4. `s2xlate=3`：`h=0,m=1`、`h=1,m=0`、`h=0,m=0` 三组独立结果；最后一组允许 PF/GPF 同时为 1。
5. `PBMT=00` 在任意 enable 组合下不产生 force bit。
6. raw S1 PF、S2 GPF、AF/GAF、permission fault 与 PBMT force 使用 OR，原始值不丢失。
7. active PBMT=`11` 与 inactive stage 非零 PBMT 返回 entry inconsistency，而不是伪造架构异常。
8. Load/Store 分别确认最终 exception bit 为 13/15 和 21/23。

### 9.2 跨页和 PA gate 检查

software-only 测试还必须构造最多 8B 的跨页访问：第一页 PBMT=`00`，第二页为 nonzero PBMT 且
PBMTE disabled。确认 RM 仅在第二页命中后产生 force fault，随后停止进一步 lookup，并且第二页
不设置 normal PA valid bit。反向组合也必须确认第一页已 fault 时不查询下一页。

### 9.3 real-DUT directed smoke

复用关联 L2TLB plan 中已实现的
`memblock_l2tlb_pbmt_response_fault_vseq`：

```text
1. 令目标 PBMTE=1，执行指令 A，建立 PBMT=01 或 10 的 raw live entry，等待 A terminal complete。
2. 使用同一 CSR producer 关闭目标 PBMTE，不产生 satp/vsatp/hgatp/priv_virt flush。
3. 发射同上下文指令 B，使其复用 A 的 live entry。
4. 检查 responder 返回的 PBMT 保持非零且 PF/GPF 已置位。
5. 检查 RM 的两个 PBMT diagnostic force bit、expected_exception 和 DUT status exception 一致。
6. 检查不存在 `RM_LS_ERR_EXCEPTION_MISMATCH`，且 B 可以按 fault 路径正常达到 terminal done。
7. 重新开启 PBMTE 后发射 C；C 不得继承 B 的 PBMT force fault，raw live entry 逐字段不变。
```

最小真实 smoke 覆盖 S1 `mPBMTE:1->0`。后续变体必须覆盖 VS-S1 `hPBMTE:1->0`、
S2 `mPBMTE:1->0` 与 allStage 双 stage 场景。

### 9.4 静态与回归检查

实施后至少执行：

```text
1. 编译包含 RM、L2TLB responder 和新增 soft test 的 V2 mem_ut 配置。
2. 运行 software-only RM PBMT helper testcase。
3. 运行 L2TLB PBMT dynamic response fault directed smoke。
4. 运行现有 `tc_sanity/base_fun`，确认 PBMT 默认关闭时不引入正常访问回归。
```

通过标准：新增 RM helper 不引入全表扫描；PBMT disabled 的旧 nonzero entry 可稳定产生同 DUT
一致的 PF/GPF；默认 PBMT=`00` 与 PBMTE=1 的正常翻译路径保持现有结果。

## 10. 文件落点与实施顺序

| 文件 | 计划修改或核验 |
| --- | --- |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/mmu_csr_runtime_state.sv` | 将 L2TLB plan 的 instance stage helper 建立在 shared static `get_stage_pbmt_enable_from_bits()` 上。 |
| `mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv` | 新增 PBMT overlay helper，在逐 byte TLB entry 路径合并 fault、控制 PA derivation，并扩展 trace。 |
| `mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_ls_core.sv` | 为 `rm_ls_program_item_t` 增加两个 PBMT force 诊断字段及默认初始化；不改 exception 编码。 |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_rm_readonly_api.sv` | 核验既有 `tlb_request_context_view_t` 的 `m_pbmt_en/h_pbmt_en` 与 `tlb_entry_view_t` 的 PBMT/fault 字段已足够；默认不新增 UID payload API。 |
| `mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_rm_pbmt_effective_fault_sequence.sv` | 已新增：纯 RM helper、异常 bit 和跨页定向检查。 |
| `mem_ut/ver/ut/memblock/tc/src/soft_test/soft_test_tc_rm_pbmt_effective_fault.sv` | 已新增：software-only testcase 入口。 |
| `mem_ut/ver/ut/memblock/seq/seq_pkg.sv`、`seq/seq.f`、`mem_ut/ver/ut/memblock/tc/tc_pkg.sv`、`tc/tc.f` | 已按依赖顺序接入 software-only 测试。 |
| `AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_l2tlb_pbmt_csr_gate_plan_20260902.md` | coding 时将其 RM 协同支持章节链接到本专项，并核验共享 helper 的单一实现。 |

推荐实施顺序：

1. 先落地 shared static PBMTE helper，并让 L2TLB instance wrapper 复用它。
2. 在 RM 增加 O(1) PBMT overlay helper、item 诊断字段和 trace。
3. 将 helper 接入 `observer_build_commit_item()` 的每 byte entry 路径，保持既有 lookup 与 PMA/PMP gate。
4. 完成 software-only matrix，再执行 L2TLB real-DUT dynamic PBMTE smoke。
5. 通过后将本 plan 从 `undo` 移到 `do`，并同步 implementation review/关联方案链接。

## 11. 验收不变量

1. RM 不读取当前 CSR，不在 commit 时按可变 CSR 重算 PBMTE。
2. 同一 raw live entry 在 disabled PBMTE 的 B UID 上产生 RM local PF/GPF 后，重新 enable 的 C UID 不继承该 fault。
3. raw `tlb_entry_by_key` 在 B 前后逐字段不变；PBMT、raw/effective PTE fault 和 entry generation 均不被 RM 修改。
4. `s2xlate=3` 下 S1 使用 hPBMTE、S2 使用 mPBMTE，两个 force bit 可同时为 1。
5. 跨页路径按每个实际命中 entry 判断 PBMT，不以首字节 entry 代替后续页。
6. 有 PBMT force fault 时，RM 不建立该 fault byte 的 normal PA，也不查询 PMA/PMP 或期待 Load data/正常 Store 结果。
7. 新增逻辑只做常数时间判断，不新增每拍/每 commit 全表扫描、全局队列或第二份 TLB 表。
8. 默认 PBMT=`00` 和 PBMTE enabled 的既有正常场景不产生新增 fault 或回归。

## 执行中补充/修正（IMPLEMENTATION_DELTA）

### [IMPLEMENTATION_DELTA] software-only 的纯值 RM API

来源：software-only sequence 不能创建完整 `memblock_rm` component hierarchy，无法直接调用 instance method
`memblock_rm::observer_eval_pbmt_fault_overlay()`。

原 plan：RM helper 由 `memblock_rm` 的逐 byte commit 路径调用，使用共享 PBMTE 映射并输出两个 force bit。

实现调整：在既有 `memblock_rm_readonly_api` 增加只读纯值包装
`eval_pbmt_fault_overlay()`，把 entry view、冻结 `tlb_context` 和 active-stage 形状转换为同一 O(1)
PBMT force 结果；`memblock_rm::observer_eval_pbmt_fault_overlay()` 与 software-only test 都复用该入口。

原因：避免 software test 重写 `s2xlate` 映射或搭建伪 RM hierarchy，同时保持 RM 主路径与测试使用同一实现。

影响范围：只读 API、RM instance wrapper 与 software-only test；不新增 UID payload API、不读当前 CSR、不修改 raw live entry，也不增加查表或扫描。

## 12. 实施结果与 real-DUT 阻塞

### 12.1 已落地内容

| 功能 | 实现提交 | 验证结果 |
|---|---|---|
| RM 的 PBMT effective fault overlay、逐 byte PA/PMA/PMP gate | `444ee43ed` | 逻辑已接入 `observer_build_commit_item()` 的 commit 构建路径。 |
| 共享只读 API 与 software-only 矩阵 | `1322b5fc4` | `tc_rm_pbmt_effective_fault` 通过，`UVM_ERROR=0`、`UVM_FATAL=0`。 |

### 12.2 联合验证判断

真实 DUT dynamic smoke 在到达 RM commit compare 前，于 `825.3ns` 被 `INT_WB_MON: LDA1 valid is X/Z` 截断。波形显示 DTLB 的 `no_translate` 请求使 PMP valid 有效，但同一请求的 `cmd` 从未初始化的 `req_out_1_cmd` 读取并传播为 X。独立 RTL 复核已确认这是 V2 RTL 缺陷，而不是 RM expected exception、TLB readonly API、L2TLB response payload 或 CSR snapshot 取样问题。

因此本轮不新增 RM 问题分析/修复方案，也不为规避该 X 修改 RM。根因、出错点和波形路径记录在：
`AI_DOC/analysis/rtl/v2/flows/memory_pmp_pma_permission_flow.md` 的“DTLB `no_translate` 的 PMP payload 生命周期缺陷”章节。

按任务终止条件，保持本 plan 在 `undo`，停止后续 10,000 笔真实 DUT 联合回归；待 RTL owner 修复并完成 RTL 再生成后，再从此 real-DUT 场景恢复验证。
