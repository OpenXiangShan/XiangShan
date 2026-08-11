# mem_ut RM 只读 API 与 DCache 驻留计数计划风险评审

状态：最终复审通过（唯一只读 API class、主体逻辑不变）

日期：2026-08-10

评审对象：[mem_ut RM 只读 API class 封装计划](/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/AI_DOC/plan/test_framework/plan/do/mem_ut_rm_readonly_api_encapsulation_plan_20260809.md)

## 1. 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 对应代码对象或落点 | 使用场景/示例 |
|---|---|---|---|
| `resident line` | 测试框架模型认为 DCache 仍持有的 cache line；本文以 record 的 `alias_valid` 表达协议驻留，不等同于数据未损坏。 | `cached_line_by_addr` 的 record。 | GrantAck 后建立 line；`Probe(toN)` 完成后删除 line。 |
| `dcache_valid_line_count` | 当前驻留 line 的聚合计数。它用于推导“当前 DCache 是否为空”，不保存 line 地址或数据。 | 拟新增至 `common_data_transaction` 的全局 DCache 聚合状态。 | `count == 0` 表示模型当前无驻留 line。 |
| `publisher` | 唯一有资格把 DCache 私有 lifecycle 变化投影到公共状态表的 DCache responder 实例。 | `dcache_mem__access_base_sequence` 的运行实例。 | default sequence 与显式 virtual sequence 不能同时成为 publisher。 |
| `generation` | 用于区分不同 testcase 或 reset 生命周期的版本号，避免旧 DCache 状态被 RM 当作当前状态读取。 | 拟新增的 DCache 聚合状态记录。 | 主表重建后，旧 generation 的计数不得继续作为当前快照返回。 |
| `Probe(toB)` | L2 要求 DCache 降为 Branch 权限但保留 line 的 Probe。 | `complete_probe_record()`。 | 驻留计数不变。 |
| `Probe(toN)` | L2 要求 DCache 放弃该 line 的 Probe。 | `complete_probe_record()`。 | C response 收敛并删除 line 后，驻留计数减少。 |
| `overlay batch` | DCache 或 Uncache 已观察到的写先入批次，下一采样边界才提交到 `write_overlay_mem`。 | `dcache_write_batch`、`commit_shared_mem_write_batch()`。 | dirty `ProbeAckData` 后，DCache 已不驻留该 line 不代表 overlay 已立即可读。 |
| `line mutation version` | 同一 64 B line 的事件版本；新 corrupt C-data 或已提交 Uncache 局部写会递增它，旧 ticket 版本不匹配时不能清除新状态。 | `dcache_line_mutation_version_by_line`。 | ticket A 尚未完成时，line 收到新的 corrupt response，A 完成后只能回收自身，不能清除新 mask。 |
| `aggregate snapshot` | owner 完整计算后一次发布的 DCache 聚合值型副本，RM 只读该副本。 | `common_data_transaction` 中的 `dcache_aggregate_snapshot`。 | 避免 RM 看到 resident 已更新但 pending 尚未更新的中间组合。 |
| `废弃的 final commit barrier` | 第一轮曾提出的强制提交当前 sample 方案，后续确认会破坏既有可见性边界，已从最终方案删除。 | 不实现 `finalize_shared_mem_sample_before_drain()`。 | 不得由 owner/API 提前提交当前 sample 的 DCache/Uncache batch。 |
| `下一采样提交确认` | 既有 `begin_shared_mem_sample()` 在下一正常采样边界提交上一 sample 的 batch；不允许强制提交当前 sample。 | DCache/Uncache responder 主循环顶部。 | 最后一笔 C-data 入队后，下一 `drv_cb` 自然提交写回。 |
| `full-line preflight` | 在创建 64 B C-data ticket 前，对两个 32 B fragment 的全部 byte 一次做无分配校验。 | `submit_dcache_cdata_overlay()`。 | 高 32 B 被拒绝时，低 32 B 也不得先入 batch。 |
| `有效性域` | DCache aggregate 与 shared-memory overlay/backing 分别判断有效性，不能把 owner 生命周期套到所有查询。 | aggregate snapshot 与 shared-memory lifecycle。 | owner 退出后 drain 不可读，但已提交 overlay 仍可读取。 |
| `drain transition sample/time` | DCache drain 从未完成变为完成的时刻，不代表最近的 overlay write 提交。 | aggregate snapshot。 | clean `toN` 也可能让 drain 变为完成。 |

本次特性的抽象功能是：保留 `cached_line_by_addr` 的 DCache 私有归属，由唯一 DCache owner 在真实生命周期事件中维护一个常数时间聚合计数；RM 通过 `memblock_rm_readonly_api` 只读该计数的值型快照。API 不扫描私有 map、不参与 DCache 协议推进，也不负责 RM 的读取时机。

## 2. 评审范围与结论

本 review 只审查 plan 与现有测试框架源码之间的设计风险。当前没有实现 `dcache_valid_line_count`、公共状态记录或 RM API 调用，因此本文不对未产生的 coding 给出通过结论。

结论：事件驱动计数替代每拍扫描的性能方向正确，`DCACHE_L2_FLUSH_DONE` 不再直接代表当前为空的语义也正确。但下列前两项会造成计数来源不唯一或 generation 错位，属于 coding 前必须收敛的阻塞风险。

说明：本节及后续 R1～R25 记录的是此前“扩展主体状态/行为”的评审过程；在第 16、17 节的范围重新收敛后，涉及主体逻辑改动的临时解法均不再属于最终 plan。最终判断以第 17 节为准。

| 风险编号 | 严重性 | 结论 |
|---|---|---|
| R1 | 高 | DCache 发布与 `reset_all_tables()` 的先后关系未定义，可能长期得到未发布或过期状态。 |
| R2 | 高 | 全局计数缺少唯一 DCache publisher 约束，可能被多个 sequence 实例写坏。 |
| R3 | 中高 | `alias_valid` 表示驻留而非数据完整性，字段名称和 RM 语义需要固定。 |
| R4 | 中高 | 驻留计数归零早于 dirty writeback overlay 提交，RM 不能把两者视作同一可见边界。 |
| R5 | 中 | 私有 map 的写入口分散，缺少完整迁移清单和专项回归矩阵。 |

## 3. R1：状态发布与主表重建的生命周期竞态

### 3.1 问题如何理解

plan 要求 testcase 重建时先让 DCache 聚合状态失效，再由 DCache owner 发布零计数基线。这个顺序本身合理，但当前 real-smoke 虚拟 sequence 会并发启动 background responder 与核心 dispatch flow；核心 flow 内部会调用 `reset_all_tables()`。因此可能出现 DCache 已发布零计数、随后主表重建又使公共记录失效、而 DCache 暂时没有新的 lifecycle 事件可重新发布的窗口。

此时真实 DCache map 可能已经为空，但 RM 查询只能得到 `UVM_ERROR`。这不是 RM 自身“错误时机读取”的问题，而是测试框架没有定义当前 generation 的状态何时正式可用。

源码依据：background responder 以 `join_none` 启动后，核心 flow 立即继续执行，[memblock_dispatch_real_smoke_vseq.sv](/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_vseq.sv:46)；核心建表调用 `reset_all_tables()`，[memblock_dispatch_base_sequence.sv](/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_base_sequence.sv:241)。DCache responder 自身会在构造、启动和 runtime reset 中清空私有 map，[mem_base_sequence.sv](/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:796)。

### 3.2 方案如何修改

plan 应新增一条明确的 DCache residency 生命周期合同：公共状态记录的 generation 只能由一个 testcase lifecycle coordinator 开启；DCache owner 只能向已开启的当前 generation 发布计数。`reset_all_tables()` 不得在没有同步 DCache owner 的情况下单独把当前 residency 状态改成无效。

建议在 plan 中二选一并固定：

- 在主表重建完成后，由协调者显式通知 DCache owner 清空并发布当前 generation 的零计数；在该通知前 API 读取统一报 `UVM_ERROR`。
- 让 DCache residency generation 独立于 `reset_all_tables()`，只由 DCache startup/runtime reset 变更；公共 dispatch 主表 reset 不修改已由当前 DCache owner 发布的 residency 记录。

无论选择哪一种，状态记录必须一次性发布 `published`、generation 和 count，不能逐字段异步写入。验收应覆盖“responder 先启动”和“主表先重建”两种调度顺序。

## 4. R2：全局计数缺少唯一 publisher 约束

### 4.1 问题如何理解

`cached_line_by_addr` 是 sequence 实例字段，而 `dcache_valid_line_count` 计划成为公共全局状态。若两个 DCache sequence 同时运行，它们各自有独立 map，却会共同写同一份 count；任一实例 reset 都可能把另一个实例仍持有的 line 错误归零。

当前源码同时具备 agent default sequence 启动入口和 virtual sequence 显式启动入口。源码并未在本计划中说明哪一个拥有公共 residency 状态，不能仅依赖“正常 testcase 应当只启动一个”的隐含假设。

源码依据：`tc_base` 为 DCache 配置 default sequence，[tc_base.sv](/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/tc/src/tc_base.sv:75)；real-smoke virtual sequence 也可显式在同一 DCache sequencer 上启动 responder，[memblock_dispatch_real_smoke_vseq.sv](/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_vseq.sv:92)。

### 4.2 方案如何修改

plan 应增加 DCache residency publisher 的唯一 owner 规则。DCache responder 在开始发布前 claim 当前 generation；同 generation 的第二个 publisher 必须立即报错并停止发布。default topology 与显式 virtual topology 必须在 testcase setup 阶段明确选择其中之一，不能让两个路径共享同一份公共计数。

owner 结束或 runtime reset 后的释放规则也要写明：只有当前 owner 能清零、发布和释放；旧实例不得在新 owner 已 claim 后写入公共状态。RM API 不参与 claim，只验证快照 owner/generation 是否有效。

## 5. R3：`alias_valid` 与“有效数据”的语义不完全相同

### 5.1 问题如何理解

plan 以 `alias_valid` 作为唯一计数条件，适合表达“DCache 协议上仍驻留该 line”。但它不等同于“该 line 的 payload 一定可作为无损数据使用”。当前代码在 `ProbeAckData` 数据被标记 corrupt 时会把 `data_valid` 置低；若目标是 `toB`，line 仍会恢复为 `alias_valid=1` 和 `ACTIVE`。

如果 RM 将 `dcache_valid_line_count` 理解成“当前仍有可读取的完整数据”，就会把这条异常路径误判为存在有效 payload。反过来，若它只表示“DCache 仍有协议驻留副本”，则以 `alias_valid` 计数是合理的。

源码依据：Probe 收敛会单独更新 `data_valid`，同时 `toB` 把 line 保留为 `alias_valid=1`，[mem_base_sequence.sv](/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:1701)；corrupt `ProbeAckData` 会跳过写回但仍关闭 Probe，[mem_base_sequence.sv](/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:2327)。

### 5.2 方案如何修改

plan 应将对外语义固定为“DCache 当前驻留 line 数”，建议命名为 `dcache_resident_line_count`；如果保留 `dcache_valid_line_count` 名称，文档必须明确“valid”只表示协议驻留有效，不表示 payload 无损或 clean/dirty 状态。

若后续 RM 还需要数据完整性判断，应另行设计专用只读状态或直接使用 C-channel 观测和 overlay 数据，不应改变本计数的判定条件。验收中应加入 corrupt `ProbeAckData + toB` 场景，确认 count 仍代表驻留而不是数据完整性。

## 6. R4：DCache 为空与 overlay 已提交不是同一时刻

### 6.1 问题如何理解

dirty `ProbeAckData` 的处理顺序是先把数据写入 DCache write batch，再完成 Probe 并删除 `cached_line_by_addr`。batch 要到后续 shared-memory 采样边界才进入 `write_overlay_mem`。因此计数已经归零时，checker 专用 overlay API 仍可能查不到这笔写回。

plan 已说明 RM 负责读取时机，但未把“驻留状态快照”与“overlay 已提交视图”之间没有即时一致性保证写成明确接口合同。后续 RM 若把 `count == 0` 当成可立即读取 overlay 的条件，会产生错误的 `UVM_ERROR` 或旧值比较。

源码依据：`ProbeAckData` 先调用 DCache store path、再收敛 Probe，[mem_base_sequence.sv](/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:2313)；shared memory store 只是把事件压入 `dcache_write_batch`，[mem_base_sequence.sv](/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:404)。

### 6.2 方案如何修改

plan 应明确声明：`dcache_resident_line_count == 0` 只描述 DCache 驻留，不承诺 writeback 已在 `write_overlay_mem` 提交。RM/checker 必须以自身 monitor/event 和已提交 sample 边界决定何时读取 overlay；API 不提供等待、强制提交或隐式重试。

建议让 residency 快照携带现有 testcase sample/cycle 上下文，或要求 RM 在调用前单独读取同一上下文，以便日志和 checker 能关联“line 删除样本”和“overlay 可见样本”。这只是可观测性要求，不改变 API 的只读边界。

## 7. R5：map 写入口迁移缺少完整清单和回归矩阵

### 7.1 问题如何理解

当前 `cached_line_by_addr` 的覆写和删除不只存在于 `record_cached_line()` 与 `remove_cached_line()`；Grant wait、Probe pending、alias conflict、`toB`、alias `toN` 直接删除和 reset 都会直接写 map。只改常见的 GrantAck 和普通 `toN` 路径，计数会在稀有分支上静默漂移。

这类漂移不会自动改变 DCache 协议波形，可能只在很晚的 RM 查询中表现为“错误地为空”或“永久非空”，定位成本很高。

源码依据：常规建表和删除位于 [mem_base_sequence.sv](/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:1102)；Probe pending 会直接覆写 map，[mem_base_sequence.sv](/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:1525)；alias conflict 有覆写、回滚和直接删除路径，[mem_base_sequence.sv](/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:1607)；runtime reset 会整体删除 map，[mem_base_sequence.sv](/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:840)。

### 7.2 方案如何修改

plan 应在实施阶段列出完整 mutation inventory，并规定所有覆写走一个“旧 record 到新 record”的内部更新 helper，所有删除走一个内部删除 helper，整体清表走一个内部清表 helper。计数变化只能发生在这三个 helper 内部；其余调用点不能直接赋值或 `.delete()`。

验收必须覆盖：GrantAck 建表、Grant wait、`Probe(toB)`、普通和 dirty `Probe(toN)`、Release、ReleaseData、CBO Clean/Flush/Inval、alias conflict 的回滚与替换、runtime reset、全局 L2 Flush。可在 reset、全局 Flush `DONE` 和测试结束等低频边界增加 debug-only 全表复核；不得把复核放进每拍主循环。

## 8. 建议的 plan 修改顺序

1. 先解决 R1，定义 generation 开启、失效和 DCache 零基线发布的唯一顺序。
2. 再解决 R2，选择 default 或 explicit topology，并建立唯一 publisher claim。
3. 固定 R3 的对外字段语义为“协议驻留”，避免把计数误用于 payload 完整性。
4. 将 R4 的 overlay 可见性边界写入 RM/checker 接入约束。
5. 最后按 R5 的完整 mutation inventory 实现 helper 和回归场景。

完成以上五项后，`dcache_valid_line_count` 才能作为 RM 的当前驻留快照安全使用。此时 `DCACHE_L2_FLUSH_DONE` 保留为全局 Flush 收敛的旁路诊断状态，不作为当前为空的真源。

## 9. Plan 对齐检查

对应 plan 为：[mem_ut_rm_readonly_api_encapsulation_plan_20260809.md](/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/AI_DOC/plan/test_framework/plan/do/mem_ut_rm_readonly_api_encapsulation_plan_20260809.md)。

本 review 发生在 coding 前；当前仓库没有本特性对应的 `dcache_valid_line_count`、公共聚合状态记录或 `memblock_rm_readonly_api` 实现。因此本章节只确认 plan 风险，不宣称已有实现与 plan 一致。

### 9.1 实现与 Plan 不一致项

尚未开始本特性 coding，不存在可比较的实现与 Plan 不一致项。

### 9.2 Plan 未说明但 Coding 落实的细节

尚未开始本特性 coding，不存在 Plan 未说明但 Coding 已落实的细节。

## 10. 非本次修改的逻辑分析

### 10.1 git status 对比结论

本 review 主题覆盖的文件：

- `AI_DOC/plan/test_framework/plan/do/mem_ut_rm_readonly_api_encapsulation_plan_20260809.md`；
- 本 review 文档。

本次没有修改 `mem_ut` 源码，也没有实现本计划中的 API 或计数器。

`git status --short` 中的其他修改不纳入本次功能正确性结论：

| 类别 | 文件或目录 | 判断 | 原因 |
|---|---|---|---|
| L2TLB 逻辑与文档 | `AI_DOC/analysis/rtl/v2/index.md`、L2TLB plan/review 文档、`L2tlb_agent_agent_driver.sv`、`L2tlb_agent_agent_monitor.sv`、fence monitor、主表 sequence | 另行 review | 属于 L2TLB、fence、dispatch 现有功能修改，与 DCache residency 计数无直接实现关系。 |
| testcase/filelist 修改 | `seq.f`、`seq_pkg.sv`、`tc.f`、`tc_pkg.sv`、soft test testcase/sequence | 另行 review | 当前状态无法证明这些修改是为本 plan 准备，不能把它们计入本次设计结论。 |
| 文档与工具产物 | `.humanize/skill/**`、`mem_ut/ver/ut/memblock/sim/.eda_remote/` | 非本次逻辑 | 属于工具运行记录或远端仿真产物。 |
| 其他未跟踪文档 | `AI_DOC/analysis/rtl/v2/flows/store_tlb_hit_replay_and_retry_flow.md` | 另行 review | 属于 RTL flow 文档，不是本次 DCache API 方案内容。 |

## 11. 最终结论

本 plan 的性能方向和只读 API 边界可保留，但不得直接进入 coding。必须先在 plan 中补齐 R1 的生命周期顺序和 R2 的唯一 publisher 规则；随后明确 R3 的字段语义，并把 R4、R5 的可见性和完整性要求纳入验收。完成这些修改后，再进行 API 与 DCache owner 状态投影的实现。

## 12. 第一轮复审新增风险及最小修改方案

本节记录在既有 R1～R5 之后重新核对 plan 与当前 shared-memory 实现时发现的风险。对应修改已同步写入 plan；本节保留“问题如何理解、影响和最小解法”，供下一轮 subagent 复审逐项确认。

### R6：shared bookkeeping 的实际 owner 落点不够具体（高风险）

问题理解：plan 同时把 ticket/corrupt 状态归 shared-memory owner、把 assembly 归 DCache owner、把聚合快照归 `common_data_transaction`，但没有明确这些可变字段的物理存储位置。若实现者按 sequence 实例新增字段，DCache 与 Uncache 可能重新产生多份计数；若把字段放进公共快照，Uncache 又可能绕过唯一 publisher 直接改写 RM 可见状态。

影响：同一 line 的 pending、corrupt count 和 assembly blocker 可能来自不同副本，drain 判定无法证明一致。

最小解法：plan 固定所有可变 shared bookkeeping 为 `mem_access_base_sequence` static lifecycle state；DCache 实例只保留私有 map 和 owner 身份；`common_data_transaction` 只保存值型 aggregate snapshot。所有跨 owner 更新必须经过 owner token 校验 helper。

### R7：旧 ticket 可能清除后续 corrupt 事件（高风险）

问题理解：仅校验 ticket 的 line、generation 和 fragment，不足以区分“ticket 创建后同一 line 又发生的新 corrupt response”。旧 ticket 的第二个 fragment 完成时，可能把新事件刚置上的整行 mask 清掉。

影响：RM 看到 `corrupt=0`，却读取到不可信或不存在的 overlay 数据，属于静默误比较风险。

最小解法：增加每 line 单调 `dcache_line_mutation_version_by_line`；新 C-data 决策、ticket 登记和 Uncache 实际 commit 递增版本，ticket 保存创建版本。完整提交时版本不匹配只回收 ticket，不清除 mask。

### R8：runtime reset 的 overlay/ticket 清理策略不唯一（高风险）

问题理解：原 plan 允许 reset 选择保留或清除 overlay，但没有唯一顺序，可能出现 overlay、mask、batch、ticket 只清一部分，或者迟到 event 被新 generation 接收。

影响：reset 后新 generation 继承不完整状态，drain 可能错误完成或永久阻塞。

最小解法：固定为“保留已提交 overlay/mask、丢弃未提交 batch/ticket”。reset 进入边沿依次使旧 snapshot 失效、停止 assembly、丢弃未提交 event/ticket、清空 resident/pending、递增 generation，再基于保留 mask 发布基线；旧 event 在 `apply_shared_mem_write()` 前拒绝。reset 保持期间不重复处理。

### R9：原子发布只停留在文字层面（高风险）

问题理解：若分别写公共 resident、pending、corrupt 和 drain 字段，API 可能在 delta-cycle 间读取到混合状态。

影响：RM 可能短暂读到 `count==0 && pending==0`，但实际 ticket 尚未登记的假 drain。

最小解法：新增值型 `dcache_aggregate_snapshot`；publisher 在局部变量中完成全部计算后一次 struct assignment，API 只复制该 snapshot，禁止读取 live counter。

### R10：最后一笔 batch 缺少提交触发点（高风险）

问题理解：现有 `commit_shared_mem_write_batch()` 只在下一次 `begin_shared_mem_sample()` 时触发。如果最后一笔 C response 入队后没有下一次 memory-facing 访问，overlay 和 ticket 可能一直未提交。

影响：resident 已归零但 drain 永远为假，RM 查询 overlay 得到错误 miss。

第一轮临时方案（已废弃）：曾建议新增 `finalize_shared_mem_sample_before_drain()` 提交当前 batch。第二轮确认该方案会破坏既有 sample 可见性，最终替换为 R13 的“下一正常采样边界自然提交”规则；该 helper 不实现。

### R11：overlay 查询的 corrupt/valid 返回组合未固定（中高风险）

问题理解：文档只定义 `corrupt==1`，没有规定该结果是否有效、数据字段是否可用，调用方可能把未定义数据继续比较。

影响：checker 对 corrupt line 进行普通数据比较，或者把 overlay miss 当成合法零数据。

最小解法：固定返回组合：普通命中 `valid=1, corrupt=0, data_valid=1`；corrupt 命中 `valid=1, corrupt=1, data_valid=0`；非 corrupt 的 overlay miss/owner miss `valid=0 + UVM_ERROR`。

### R12：final commit barrier 不能由只读查询隐式触发（高风险）

问题理解：如果把“RM 查询最终 drain 前执行 barrier”理解成 API 内部动作，读 API 会提交 batch、改变 overlay 和 drain，直接违反本计划的只读边界。

影响：同一 API 调用既可能读到旧状态，也可能改变状态后读到新状态；RM 时机错误会被隐藏，且仿真行为会因是否调用 API 而改变。

第一轮临时方案（已废弃）：曾将 barrier 限制为 producer 调用。第二轮已删除 barrier 本身，最终规则见 R13：producer 不调用强制提交，只经过下一正常采样边界；API/RM/checker 不得调用、等待或触发提交。

## 13. 第一轮修改后的对齐结论

以上 R6～R12 已分别落入 plan 的术语、owner、ticket、reset、snapshot、commit、API 返回和验收章节，其中 R10/R12 的强制 barrier 临时方案已由 R13 替换。下一轮 review 重点是检查：line version 的递增边界、reset 保留 overlay 与 testcase 初始化边界，以及“下一正常采样边界”规则是否完整覆盖退出路径。

## 14. 第二轮 subagent 复审问题与收敛方案

第二轮独立复审确认 R6～R11 的大部分修改方向正确，但指出第一轮新增的强制 finalizer 会破坏已有 sample 可见性。以下问题和最终收敛方案均已写回 plan。

### R13：强制提交当前 sample 会改变既有 overlay 可见性（高风险）

问题理解：现有 `commit_shared_mem_write_batch()` 由下一次 `begin_shared_mem_sample()` 调用，保证同拍 DCache 写先于 Uncache 写统一可见。若 DCache owner 在 Flush、退出或 testcase 结束时直接提交当前 batch，会让当前 sample 的数据提前可见，并可能替仍在运行的 Uncache responder 提交它本拍刚入队的 event。

影响：是否发生 owner 退出或 API 相关流程会改变同拍内存模型，违反“不改变既有 batch 提交顺序和可见性”的 plan 边界。

最终方案：删除 `finalize_shared_mem_sample_before_drain()`，不新增强制 commit helper。DCache/Uncache responder 每个正常 `drv_cb` 顶部已有 `begin_shared_mem_sample($time)`；owner 退出、Flush 收敛和 testcase 结束前只需继续经过下一正常采样边界，由既有路径提交上一 sample batch。API/RM/checker 不得调用、等待或触发提交。

### R14：owner 退出后的 aggregate 与 overlay 有效性混淆（高风险）

问题理解：DCache owner 退出时必须将 aggregate `published` 置零；因此 owner 退出后无法再读取 drain。但已提交 overlay、backing 和 corrupt mask 属于 shared-memory lifecycle，按 reset 策略可以继续存在。如果所有查询都依赖 owner，就会把仍可读取的数据错误变成 API miss。

影响：退出后的 checker 无法读取已提交 overlay，或者实现者为了保留 drain 又引入额外 terminal snapshot，扩大状态和 API 复杂度。

最终方案：固定两个有效性域。DCache aggregate/drain API 必须检查当前 owner、`published` 和 generation，owner 退出后返回 invalid；backing/overlay/corrupt API 只检查 shared-memory lifecycle，不检查 DCache owner，因此 owner 退出后仍可读取已提交数据。若 RM 需要 drain，必须在 owner 仍 published 且下一正常采样边界完成提交后读取。

### R15：完整 64 B C-data 缺少全行原子失败边界（高风险）

问题理解：当前源码低/高 32 B 分两次调用 DCache store helper。若直接沿用，高半条的地址校验失败时，低半条可能已经入 batch，导致半条 overlay、半个 ticket 或 pending 泄漏。

影响：drain 永久不收敛，或 RM 将不完整 64 B 数据当成完整 writeback。

最终方案：正常完整 `ReleaseData`/`ProbeAckData` 在创建 ticket 前先做 full-line preflight，一次校验全部 64 B。只有全部成功时，才在同一个零时间 helper 动作中插入 ticket、追加两个 event、`pending += 2`。任一 byte 失败时两个 fragment 都不入 batch，按既有 C-data 后端错误策略处理。

### R16：初始化 backing API 只返回存在性，无法给 RM 提供初始数据（高风险）

问题理解：原 plan 只规定查询 `main_mem` line、地址和 key，没有明确复制已经懒初始化的 payload。RM 仅知道映射存在并不能预测未被 overlay 覆盖的读数据。

影响：后续 RM 仍不得不直接访问主表，或误把 overlay 数据当初始化数据。

最终方案：在唯一 `memblock_rm_readonly_api` class 内增加 `read_initialized_backing_for_rm(addr, byte_mask)`。它只检查已有 `main_mem` 并复制请求字节；任一 backing miss 返回 `valid=0 + UVM_ERROR`，不读取 overlay、不调用 `ensure_main_line()`、不懒分配。

### R17：drain 时间戳不能冒充 overlay 提交时间（中高风险）

问题理解：clean `toN` 可以没有 overlay 写入却让 `dcache_drain_complete` 从假变真。若 snapshot 字段叫“最近 overlay committed sample/time”，却在 drain 变真时更新，就会把非写回时刻错误标成 overlay 提交时刻。

影响：RM 使用该字段关联 monitor event 时会发生错误归因。

最终方案：删除“最近 overlay 提交 sample/time”语义，统一保留 `drain_transition_sample/time`，只表示当前 generation 的 drain 从未完成变为完成的采样边界。该字段允许在 clean `toN` 时更新，不再承担 overlay 提交时间含义。

### R18：Flush `DONE` 保持期可能重复触发全表 debug 扫描（中高风险）

问题理解：`DCACHE_L2_FLUSH_DONE` 是可保持多拍的状态；若按 level 做 map/count 复核，就会退化成每拍扫描。

影响：大 map 场景会出现无必要的性能回退。

最终方案：全表 debug 复核只在 `previous_l2_flush_state != DCACHE_L2_FLUSH_DONE && current_l2_flush_state == DCACHE_L2_FLUSH_DONE` 的进入边沿执行一次；DONE 保持期间禁止重复扫描。进入 DONE 边沿只检查 resident map/count 一致性，dirty 写回的 drain 是否完成只在下一正常采样边界 batch 已提交后检查。

### R19：runtime reset 与既有 batch commit 的调用顺序冲突（高风险）

问题理解：现有 DCache/Uncache 主循环都在采样 `reset_active` 前调用 `begin_shared_mem_sample($time)`。若 plan 写成“reset 先丢弃上一 sample 的未提交 batch”，实现者要么需要改动两个 responder 的主循环顺序，要么会与真实代码行为不一致。

影响：若不明确，reset 边界上的 event 可能被实现成有时提交、有时丢弃；ticket、overlay 和 generation 的归属无法稳定复现。

最终方案：保持现有主循环顺序，不调整协议或 sample 时序。reset 进入采样边界先由既有 `begin_shared_mem_sample()` 用旧 generation 提交上一正常 sample 已接受的 batch；这批写视为 reset 前已提交。随后 reset cleanup 只丢弃仍未提交的残留 batch/ticket，保留已提交 overlay/mask，并创建新 generation。reset cleanup 后出现的旧 event 才必须在 `apply_shared_mem_write()` 前拒绝。

### R20：Uncache event 缺少独立 shared-memory 代次校验（高风险）

问题理解：Uncache event 不属于 DCache generation。若只给 DCache ticket 加 generation，而 reset 后仍残留一个 Uncache batch event，单靠 DCache owner token 无法判断该 event 是否属于旧 shared-memory 状态。

影响：旧 Uncache store 可能在新 generation 中写入 overlay 或清除新 corrupt mask，形成跨 reset 的静默状态污染。

最终方案：为所有 DCache/Uncache write event 增加 shared-memory lifecycle epoch；初始化和 reset cleanup 递增该 epoch。commit 处理每个 event 前先校验 epoch，旧 event 只报 `UVM_ERROR` 并丢弃，不调用 `apply_shared_mem_write()`、不结清新 pending、不改 mask/version。DCache event 仍额外校验 owner token/generation，两个代次职责不混用。

### R21：shared-memory 查询 helper 不能成为第二套 RM 对外接口（中高风险）

问题理解：backing/overlay 数据必须从 shared-memory static state 读取，因此实现需要底层 peek helper；若直接把这些 helper 命名为 RM API 并放在 `mem_access_base_sequence`，会让 RM 看到第二组入口，违反“只封装一个 API class”的边界。

影响：调用方可能绕过 `memblock_rm_readonly_api` 的统一 `UVM_ERROR`、值型 view 和非创建检查，重新暴露静态 owner 内部状态。

最终方案：`memblock_rm_readonly_api::read_initialized_backing_for_rm()` 与 `memblock_rm_readonly_api::read_committed_overlay_for_rm()` 是唯一对 RM 公开的方法。两者所需的 `try_peek_initialized_backing()`/`try_peek_committed_overlay()` 是 API class 自身的 private helper，直接只读已有 static map；helper 不报告为对外 API、不返回 live handle、不允许 RM 直接取得。

### R22：full-line preflight 失败后的 resident 收敛边界不够明确（中高风险）

问题理解：全行预检失败时不能创建 ticket 或写 overlay，但 DUT 已经完成 C response，line 的协议驻留仍需按原 Release/Probe 收敛；如果只报错退出而不说明 line 删除和 corrupt 标记，resident count 可能与真实协议状态分离。

影响：可能出现 line 已被 DUT 放弃但模型仍计为 resident，或模型删除 line 却没有留下不可比较的 corrupt 标记。

最终方案：已知 line 的 full-line preflight 失败必须置全 64 B corrupt mask，并沿原 C response 路径完成 line 删除/Probe 收敛；不得创建 ticket、pending 或 overlay，随后 owner 终止或保持不可 drain。未知 line 不发布 drain。这样 resident 语义和数据不可信语义分别收敛。

### R23：TLB/UID 查询的“快照”可能只是对象浅拷贝（中高风险）

问题理解：plan 原文把 TLB 查询写成“返回 live entry 快照”，但 `memblock_tlb_entry`/`uid_tlb_record` 可能含 queue、payload、CSR snapshot 或 object handle。直接返回对象或浅拷贝会让 RM 间接修改 owner 状态。

影响：只读 API 的值型边界被绕过，且 queue/object 在 owner 更新后可能发生别名变化。

最终方案：TLB/UID 查询只返回 API class 内定义的纯标量 value view；queue 只做独立值拷贝或不暴露，payload/CSR snapshot 递归复制到独立 view，禁止返回原始 `memblock_tlb_entry`、`uid_tlb_record` 或其 handle。

### R24：DCache owner claim 可能早于 shared-memory 初始化（高风险）

问题理解：basicTest vseq 以 fork 启动 responder；若 responder 在 lifecycle 初始化前 claim 并清理 static 状态，会重新引入启动竞态。

影响：DCache 可能发布错误零基线，或者两个 responder/sequence 先后清空同一 shared-memory state。

最终方案：受支持 basicTest vseq 必须先完成唯一 `initialize_shared_memory_state()` 再启动 responder。claim 只验证已有 lifecycle 和 owner 状态；未初始化时 `UVM_ERROR` 后退出，不在 responder 内兜底初始化。该约束不扩展到其他 testcase。

### R25：ticket version 失配时 pending 可能永久残留（高风险）

问题理解：正常 ticket 登记时已经令 pending 增加 2。若同一 line 后续 mutation 改变 version，旧 ticket 的 fragment 不能再写 overlay；若只拒绝这些 event，而 pending 只在“实际写入”时递减，旧 ticket 会永久阻塞 drain。

影响：`dcache_drain_complete` 永远为 0，且状态表看不到是哪一个 ticket 泄漏。

最终方案：新增 `cancel_dcache_writeback_ticket_due_to_version()`。当前 generation ticket version 失配时，helper 一次扣除所有未提交 fragment 的 pending、标记 ticket canceled；当前和剩余 event 只丢弃，不写 overlay、不清新 mask、不二次扣 pending。已提交半条数据保留，但由后续 corrupt mask/new ticket 管理可比较性。旧 epoch/generation ticket 不得触碰当前 pending。回归覆盖 version 失配后 pending 回零、mask 仍保持和 drain 不假完成。

## 15. 第二轮修改后的结论

R13～R25 已在 plan 中收敛为不改变现有 sample/batch 时序、两个有效性域、64 B 全行预检及失败收敛、纯值型 TLB/UID 快照、backing 数据只读接口、单义 drain 时间戳、单次 Flush DONE debug 复核、与现有 reset 顺序一致的提交边界、独立 shared-memory epoch 校验、唯一对外 API class、初始化先行合同和 version 失配 ticket 的 pending 结算。下一轮 review 只需检查这些最终约束之间是否仍存在遗漏或新增高风险；若无必须修改项，即可给出最终 plan 结论。

## 16. 范围重新收敛：主体逻辑不变的最终方案

用户进一步明确，本 plan 的目的只能是从测试框架侧封装一个供 RM 调用的只读 API class；可按 RM 的数据需求设计 view，但不得引入 RM 实现，也不得为获得数据而改变测试框架主体逻辑。

因此，上述 R6～R25 中涉及下列主体行为改动的临时方案全部从最终 plan 删除，保留在本文仅作为风险分析历史：

- 改造或包装 DUT memory-facing read/store、主内存懒分配、overlay 写入；
- 改造 C-data assembly、增加 ticket/epoch/preflight/cancel 机制；
- 改造 batch 入队、提交顺序、commit 边界、reset 顺序或 owner 退出条件；
- 为 RM 读取而引入新的 responder 启动、等待或调度控制。

这些方案本身并非都错误，但会扩大为测试框架主体逻辑修复，违反当前 plan 的最小改动边界；若未来确有需要，必须单独建立主体逻辑 plan 和回归，不得搭载在 RM API 封装中。

### 16.1 最终最小方案

最终 plan 只保留以下内容：

1. 一个 `memblock_rm_readonly_api` singleton class，作为唯一 RM 对外入口；
2. 对已有 dispatch、TLB、UID-TLB、`main_mem`、已提交 overlay 的非创建、非 fatal 值型读取；
3. 对 DCache 驻留、pending、corrupt 和 drain 的被动 observer：仅在现有动作已经完成后记录结果，绝不影响原动作；
4. 统一 `UVM_ERROR + valid=0` miss 语义，以及 corrupt 命中的保护性成功结果；
5. API 未调用与调用两种情况下，原有接口时序、batch、main memory、reset、退出和 testcase 结果必须一致。

### 16.2 重新理解此前高风险问题

| 历史风险 | 最终处理方式 |
|---|---|
| ticket、line version、epoch、preflight、cancel 的一致性问题 | 不在本 plan 引入这些主体机制；不需要解决其新增状态机风险。 |
| batch 最终提交、Flush DONE、reset 先后关系 | API 不提交、不等待、不改变既有边界；observer 只读取既有完成结果。 |
| owner/overlay 生命周期 | API 分别按既有 owner 与 shared-memory lifecycle 检查，不新增 lifecycle 控制。 |
| TLB/UID-TLB live handle 风险 | 保留为最终 plan 要求：只返回 API class 定义的纯值型 view。 |
| DCache count/drain 的性能问题 | 使用已有动作完成点后的被动 observer，禁止每拍扫描和 map 重构。 |

### 16.3 最终 review 结论标准

最终复审只检查三个问题：

1. 是否仍然只有一个对 RM 公开的 API class，且没有 RM 实现；
2. 是否所有读取和 observer 都不改变既有测试框架主体行为；
3. 是否所有无法安全读取的数据都以 invalid/`UVM_ERROR` 表示，而不是通过改变主体逻辑来制造数据。

## 17. Scope-constrained 最终复审记录

按用户最新边界对 plan 重新检查后，确认最终正文只保留一个 `memblock_rm_readonly_api` class、非创建只读 peek、值型 view 和既有完成点后的被动 observer；没有 RM 实现、RM 接入、checker 算法或主体协议改动。

本轮检查未发现新的高风险问题。此前 R1～R25 中涉及 ticket/epoch/preflight、batch/reset 重排或 DUT memory-facing 主流程的内容已明确标记为历史讨论并从最终 plan 排除；剩余 observer 若无法安全获得输入则返回 unavailable/`UVM_ERROR`，不会扩大主体逻辑范围。

最终结论：该 plan 可作为“测试框架侧单一 RM 只读 API class 最小封装”方案进入后续 coding review；coding 仍必须保持 API 未调用与调用两种情况下既有测试框架行为一致。

## 18. 实施后状态更新（2026-08-11）

本文件第 2、9、10 节中“尚未开始 coding”的表述记录的是 2026-08-10 的评审时点，不再描述当前仓库状态。该 plan 已按最小只读边界完成实施并归档，相关实现 commit 为：

- `3f03801d4e`：唯一 API class、非创建 value view 和 backing/overlay 查询；
- `0b8d0a541b`：DCache/Uncache 被动 observer、corrupt mask 与 fragment 提交事实；
- `54e718d53b`：lifecycle 门控、64 B corrupt 计数、reset generation 和局部 fragment 扫描。

最终实现评审见：[mem_ut_rm_readonly_api_encapsulation_implementation_review_20260811.md](/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/AI_DOC/plan/test_framework/review_doc/undo/mem_ut_rm_readonly_api_encapsulation_implementation_review_20260811.md)。该评审记录了计划与实现差异、验证结果和当前范围边界；本文件继续只保留 coding 前风险收敛过程，不能替代最终实现结论。
