# 问题2：RM Uncache D-error provenance ledger 方案

日期：2026-10-15

## 1. 术语

| 术语 | 本文含义 |
| --- | --- |
| provenance | 一笔 Uncache 响应从 TL A 请求、D 响应到 RM 查询的稳定来源信息，包含 source、generation、地址、采样序号和动态 owner。 |
| owner | 实际消费该 Uncache Load 响应的动态 Load，身份为 `UID + dynamic_epoch + ROB + LQ`，不使用地址替代身份。 |
| generation | 同一个 4-bit TL source 每次合法 A.fire 时递增的代次。source 可复用，但不同 generation 不得互相覆盖。 |
| pending | A.fire 已经发生、owner 已冻结，但对应 D.fire 尚未发生。RM 查询命中 pending 必须返回 not-ready，不能解释为 clean。 |
| stable history | D.fire 后保存的不可变历史。即使 owner 后续被 redirect、ROB/LQ/source 被复用，历史仍归属于 A.fire 时冻结的旧动态实例。 |
| path classification | RM 根据已确认的翻译和 PMA/PMP 结果选择数据源：PMA-MMIO/适用 NC Load 查询 Uncache ledger，normal cacheable Load 查询 L2 ledger。 |

## 2. 根因与责任归属

现有 RM 对所有正常 Load 都调用 `read_l2_d_error_for_rm()`。PMA-MMIO 和 NC Load 实际走 Uncache，TL D 的 `denied/corrupt` 不进入 L2 sticky ledger，因此 RM 会把真实 Uncache denied 错误解释为 clean，产生：

```text
RM expected exception = 0x0
DUT/status exception  = 0x20
```

DUT 映射与波形一致：

```text
denied=1                 -> exception bit5  = 0x20
denied=0 && corrupt=1    -> exception bit19 = 0x80000
denied=1 && corrupt=1    -> bit5，denied 优先
```

本问题属于 RM/测试环境数据源缺失，不修改 DUT，也禁止从 DUT actual exception 反推 expected。

## 3. 数据模型

### 3.1 owner 快照

在真实 Uncache TL `A.fire` 的 responder 采样边界：

1. 读取 `uncache_lq_req_valid/slave_valid/slave_id/rob` 的同拍只读镜像。
2. 选择所有 `req_valid && slave_valid && slave_id == TL source` 的 LoadQueueUncache slot。
3. 用完整 ROB key 查询 `common_data_transaction` 的 active ROB map，取得 UID。
4. 从 UID status 冻结 `dynamic_epoch` 和逻辑 LQ key，并校验 ROB/LQ/active 身份一致。
5. 允许 NC 合并导致一个 source 对应多个 owner，最多保存 16 个；MMIO 正常应为一个 owner。

同地址并发不能按地址选 owner。地址只作为 owner 已确定后的交叉校验字段。

### 3.2 response record

每条 A.fire response record 至少保存：

```text
kind, source, generation, size, address,
A.fire global sample, responder cycle,
denied, corrupt,
owner_count, owner[0:15]
```

`generation` 按 source 独立单调递增。source 在前一 generation 尚未 D.fire 时再次 A.fire 属于协议/环境生命周期错误，立即 fatal。

### 3.3 pending 与 stable history

公共静态 ledger 由 shared-memory testcase lifecycle 初始化和清空：

- A.fire：为每个 owner 建立 pending reference，键为 `UID + dynamic_epoch`。
- D.fire：按 `source + generation` 精确消费 pending，并为每个 owner追加不可变完成历史。
- clean、denied-only、corrupt-only、both 都必须保存；clean 不能靠“查不到记录”表达。
- 历史保存 `A/D global sample`，不得使用 `$time` 或 responder 私有 cycle 与 RM `sample_seq` 比较。
- owner 历史按 owner key 建索引，查询只扫描该动态实例的小队列，不扫描完整 UID/历史表。

## 4. 时序和边界

### 4.1 A.fire

抽象功能：冻结协议请求和动态 owner，并发布 pending；不等待 D，也不读取 DUT exception。

```text
sample = peek_current_dut_global_sample()
generation = next_generation[source]++
owners = capture owners by slave_id == source
validate source has no outstanding generation
publish pending for every owner
enqueue immutable response record
```

Load response若没有唯一且合法的 owner 集合必须 fatal；Store ACK 不进入 Load RM 查询，可保留零 owner provenance。

### 4.2 D.fire

抽象功能：把 current D hold 的固定 payload提交为历史，并结束该 source generation 的 pending 生命周期。

```text
d_sample = peek_current_dut_global_sample()
validate source/generation is outstanding
for each frozen owner:
    remove exact pending(source, generation)
    append completed history
release source generation
```

redirect 后迟到 D response 仍写入旧 `UID + dynamic_epoch`。它不能查询当前 active owner，也不能改绑到同 ROB/LQ/source 的新实例。

### 4.3 RM 查询

查询输入至少包含 `UID、dynamic_epoch、ROB、LQ、PA、access_sample`：

1. 只接受 owner 身份完全一致、地址覆盖请求 PA、`D.fire sample <= access_sample` 的完成记录。
2. 命中完成记录时聚合同一动态 Load 的 denied/corrupt；denied 优先。
3. 只有 pending 或完成记录在查询 sample 之后时返回 not-ready。
4. 完全没有 pending/完成记录时返回 missing。
5. not-ready/missing 都由 RM 设置 `RM_LS_ERR_D_ERROR_NOT_READY` 并打印身份、source/generation/sample，不屏蔽 compare error。

## 5. RM 路径选择

`rm_ls_program_item_t`保存以下稳定分类：

```text
lq identity
all_pma_mmio
all_normal_cacheable
uncache_path
expected_uncache_denied/corrupt
```

选择规则：

| 场景 | 数据源 |
| --- | --- |
| 翻译/PMA基础异常已成立 | 不追加 D-error |
| 已确认 PMA-MMIO Load | Uncache ledger |
| 已确认走 Uncache 的 NC Load | Uncache ledger |
| normal cacheable Load | L2 D-error ledger |
| 分类不完整或互相矛盾 | `RM_LS_ERR_D_ERROR_NOT_READY` |

Uncache 映射：

```text
denied                -> expected_exception[5]
corrupt && !denied    -> expected_exception[19]
```

现有 `expected_dcache_*` 只描述 L2/DCache；新增 `expected_uncache_*`，避免诊断含义混用。

## 6. 源码落点

| 文件 | 修改 |
| --- | --- |
| `mem_base_sequence.sv` | 静态 Uncache ledger 类型/API；shared lifecycle reset；A.fire owner/generation/pending；D.fire stable history。 |
| `memblock_rm_readonly_api.sv` | `uncache_d_error_view_t` 和 `read_uncache_d_error_for_rm()`。 |
| `rm_ls_core.sv` | item 中保存 LQ、路径分类和 Uncache expected 字段。 |
| `memblock_rm.sv` | 构建分类；按路径查询 L2 或 Uncache；异常映射和清晰日志。 |
| 独立 soft test/vseq | 直接验证 ledger/query 语义，不复用主问题 directed test。 |

禁止修改 `status_transaction.sv`、`issue_queue_scheduler.sv`、`dispatch_monitor_batch_handler.sv`、`common_data_transaction.sv`、`memblock_dispatch_types.sv` 和主问题 agent 的 directed test。

## 7. 自Review场景矩阵

| 场景 | 期望 |
| --- | --- |
| MMIO Load clean | Uncache history found，expected exception保持0。 |
| MMIO Load denied-only | bit5=1。 |
| MMIO Load corrupt-only | bit19=1。 |
| MMIO Load both | 只置bit5。 |
| NC Load | 支持一个source多owner，每个owner均查询到同一D结果。 |
| cacheable Load | 继续查询L2 ledger，不查询Uncache。 |
| 同地址并发 | 按UID/epoch/ROB/LQ区分，不串记录。 |
| source复用 | 新generation不覆盖旧generation。 |
| redirect旧响应 | 迟到D写旧epoch；新epoch查询不得命中。 |
| 查询早于D.fire | not-ready，不得返回clean。 |
| 缺记录 | missing并报RM错误，不得返回clean。 |
| ROB/LQ不一致 | 查询拒绝并打印完整身份。 |

自Review结论：方案覆盖用户指定的MMIO、NC、cacheable、四种错误组合、同地址并发、source复用、redirect旧响应和查询时序。实现不得引入全历史扫描，也不得从actual exception建立expected。

实现后复查发现，PMA的 `normal_cacheable` 只描述底层物理区域，不能直接作为最终访问路径：当有效PBMT为
NC/IO时，底层PMA区域仍可能为normal-cacheable，但DUT实际走Uncache。实现已将
`item.all_normal_cacheable` 收紧为 `all_bytes_pma_pbmt && all_bytes_normal_cacheable`；因此PBMT-NC/IO
只走Uncache ledger，PBMT=PMA且逐byte PMA均为normal-cacheable时才走L2 ledger，不再把合法NC Load误报为
“Uncache/cacheable classification contradictory”。

## 8. 验收与测试计划

### 8.1 静态与编译

```text
git diff --check
唯一 mode 执行 VCS 远端编译
```

### 8.2 独立 directed 测试

至少覆盖：

```text
clean / denied / corrupt / both
同source跨generation复用
旧generation/旧epoch迟到D
同地址不同owner
查询早于D.fire和缺记录
cacheable路径仍选择L2
```

### 8.3 真实 seed

使用原始 cfg 和 `seed=37617171`，唯一 mode/output 目录完整自然运行结束。验收：

- 原 UID584/939/1018/1547 的 `expected=0x0 actual=0x20` 消失；
- 无新增 owner/generation/ledger fatal；
- 记录自然结束证据、`UVM_ERROR/UVM_FATAL` 总数；
- 不手动终止，不运行 clean，不删除其他 agent 输出。

## 9. 阻塞记录

### 9.1 2026-10-15 16:39，RM专项directed验证完成

使用唯一 mode `fault2_rm_uncache_ledger_20261015_a` 完成编译和独立 directed 仿真。测试自然结束于
`265.300ns`，日志出现 `RM Uncache D-error ledger directed checks passed` 和 `TEST_PASS`，最终
`UVM_ERROR=0`、`UVM_FATAL=0`。该测试覆盖 clean、denied-only、corrupt-only、both、pending/future
sample、同 source 跨 generation 复用、同地址不同 owner、redirect 旧 epoch 迟到响应，以及 L2/Uncache
ledger 隔离。

### 9.2 2026-10-15 16:44，真实 seed 编译被并行主问题改动阻塞

真实 seed 使用绝对 `plus_file` 加载原始
`tc_l2tlb_fault_2pct_no_trigger.cfg`，避免 worktree 内同名 cfg 缺失时回落到默认配置。启动命令为：

```bash
make eda_run tc=basicTest \
  ts=memblock_dispatch_real_smoke_vseq \
  mode=fault2_rm_uncache_ledger_real_seed37617171_20261015_a \
  cfg=tc_l2tlb_fault_2pct_no_trigger \
  plus_file=/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/seq/plus_cfg \
  seed=37617171 wave=off \
  note=rm_uncache_ledger_real_seed37617171_20261015_a
```

VCS 在并行主问题 agent 独占的禁写文件
`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv:3422` 报错：

```text
Error-[SV-FNYI] Feature not yet implemented
Referencing member of a structure returned by a function call is not supported yet.
```

对应表达式是 `status.get_rob_key() != wb_event.rob_key`，同一日志格式化代码还直接读取
`status.get_rob_key().flag/value`。该文件在本RM专项中明确禁止编辑，而且时间戳显示它在本次启动编译时由并行
主问题任务更新，因此RM专项不越界修复，也不回退该改动。影响是新mode无法生成可执行文件，真实 seed 尚未进入
仿真；这不是RM ledger源码编译错误。待主问题 agent 完成该禁写文件的VCS兼容修正后，使用新唯一 mode 重新执行
上述完整随机测试，并检查 UID584/939/1018/1547。

### 9.3 2026-10-15 16:51，首轮真实流发现owner门槛过严

复用本专项16:39已成功编译的可执行文件，并显式加载原始cfg运行真实seed。该次仿真自然结束于
`3790.900ns`，`UVM_ERROR=0`、`UVM_FATAL=1`。fatal发生在UID14的Uncache A.fire：DUT slot 7的
ROB=`0/85`通过active ROB map唯一解析到UID14，主表ROB、status/main LQ=`0/37`也完全一致，但
`status.active_lq_mapped=0`。

根因是 `active_lq_mapped` 只表示测试框架当前LQ反查map是否仍保留该owner，不表示DUT中的
LoadQueueUncache slot是否已完成。A.fire可以晚于该map释放；此时用完整active ROB仍能唯一关联动态UID，
且main/status保存的LQ身份仍稳定。实现已取消把 `active_lq_mapped=1` 作为A.fire owner合法性的必要条件，
仍严格要求：active UID、完整ROB唯一映射、main ROB一致、status/main LQ一致，并冻结
`UID + dynamic_epoch + ROB + LQ + slot`。因此没有改为按地址匹配，也没有放宽redirect/source复用隔离。

### 9.4 2026-10-15，review补齐NC多owner专项覆盖

review发现原directed已经覆盖错误矩阵、source generation复用、同地址不同owner、redirect旧epoch、
sample边界和L2/Uncache隔离，但没有显式构造“一条NC Uncache response绑定多个Load owner”。已在RM专项
sequence中补充两个不同 `UID + dynamic_epoch + ROB + LQ` owner共享同一source/generation的检查，并验证：

- 两个owner均命中同一冻结A/D sample、source和generation；
- denied/corrupt payload对两个owner一致可见；
- 使用错误LQ身份查询会返回identity mismatch，不会按地址误命中。

使用唯一mode `fault2_rm_uncache_ledger_20261015_d` 重新编译并运行directed，仿真自然结束于
`265.300ns`，出现 `RM Uncache D-error ledger directed checks passed` 和 `TEST_PASS`，最终
`UVM_ERROR=0`、`UVM_FATAL=0`。

### 9.5 2026-10-15，最终共享mode专项与真实seed验收通过

最终唯一mode为`fault2_rm_fp_nanbox_uid98_20261015_c`。Ledger/NaN-box directed日志：

```text
mem_ut/ver/ut/memblock/sim/fault2_rm_fp_nanbox_uid98_20261015_c/log/
tc=basicTest_ts=memblock_rm_uncache_d_error_ledger_directed_vseq_cfg=tc_dispatch_real_smoke_seed=888888_rtl_rm_ledger_nanbox_directed_20261015_c.log
```

该测试自然结束于`200.100ns`，覆盖clean、denied-only、corrupt-only、both、source
generation复用、多owner、错误owner身份、redirect旧epoch迟到D、sample边界和L2/Uncache
隔离，结果为`TEST_PASS`、`UVM_ERROR=0`、`UVM_FATAL=0`。

真实seed日志：

```text
mem_ut/ver/ut/memblock/sim/fault2_rm_fp_nanbox_uid98_20261015_c/log/
tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=37617171_rtl_rm_uid98_overlap_real_seed37617171_20261015_c.log
```

完整日志中共有1281笔Uncache ledger查询：1251笔clean、15笔`denied=1`、15笔
`denied=0/corrupt=1`。例如UID376的`denied=1/corrupt=1`按denied优先映射为`0x20`；
UID14的corrupt-only映射为`0x80000`。两者均满足`A.sample <= D.sample < WB sample`。

原关注UID584/939/1018/1547在本次共享worktree时序下均为clean response，但分别以完整
`UID + dynamic_epoch + ROB + LQ + source generation`命中ledger，A/D sample早于WB sample，
最终data/exception均PASS。完整真实seed自然结束于`3282013.400ns`，
`RM_LS_TRACE_COMPARE=10000`、`Quit count=0/10`、`UVM_ERROR=0`、`UVM_FATAL=0`。

最终自review确认：A.fire冻结owner和generation，D.fire只更新对应source generation；查询
同时检查UID、epoch、ROB、LQ、PA范围和sample边界。redirect迟到D不能污染新epoch，PBMT-NC/IO
选择Uncache ledger，PMA normal-cacheable选择L2 ledger；denied优先于corrupt，缺失或未到D.fire
不会被解释为clean。未修改主阻塞禁写文件，也未放宽RM compare。
