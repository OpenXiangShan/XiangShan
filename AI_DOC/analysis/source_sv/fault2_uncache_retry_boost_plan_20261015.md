# Fault2 Uncache Retry Boost 实现方案

## 根因

问题根因是：最终 selected 的 `MEMORY_VIOLATION` subtype 为 `UNCACHE_NACK`，且 `flush_itself=1` 的请求触发 redirect 后，框架继续保留固定的 `send_pri`。年轻的高 priority 请求反复抢占 16 个 Uncache entry，老请求 UID862 长期得不到重新发射机会，形成饥饿。已确认 DUT 的 full rollback、flush、free 流程正常，当前缺陷属于测试框架请求重发调度公平性问题。

## 术语

- **最终 selected**：本轮 memory violation 候选中经身份和优先级裁决后真正被框架选中的事件。
- **dynamic identity**：用于确认事件仍属于同一逻辑请求实例的动态身份，至少包含 UID、ROB 和 `dynamic_epoch`（当字段有效时）。
- **Uncache retry boost**：针对符合条件的 `UNCACHE_NACK` redirect 请求设置的独立动态调度优先级，不修改 `main_tr.send_pri`。
- **实例 E / E+1**：同一 UID/ROB 请求在 redirect 前后的连续动态实例；只有紧邻的一次 redirect reissue 可以继承 boost。

## 设计

仅当最终 selected 事件同时满足以下条件时置位 boost：

1. 事件类型为 `MEMORY_VIOLATION`；
2. subtype 为 `UNCACHE_NACK`；
3. `flush_itself=1`；
4. UID、ROB 及 dynamic identity 有效。

boost 使用独立的动态 `uncache_retry_boost` 和 `retry_count` 状态，不修改 `main_tr.send_pri`。事件在转换为内部事件时冻结已有 `memblock_wb_event_t.dynamic_epoch/has_dynamic_epoch`；最终 selected 阶段再次进行 UID、ROB、dynamic identity 校验。若现有字段足够，复用现有 raw event 结构，不无必要扩展 raw monitor 结构。

issue item 在创建时从 status 冻结 boost、retry count 及身份 snapshot。调度比较器只读取 issue item snapshot，不动态读取 status，避免队列中 item 在比较过程中发生身份漂移。

调度顺序为：

1. boosted item 优先于普通 item；
2. 多个 boosted item 按 ROB 年龄选择；
3. 普通 item 保持既有 `send_pri` 优先、ROB 年龄次优先。

global-priority 预筛选必须感知 boost：存在 boosted item 时，只允许 boosted item 进入候选，并在候选内按 ROB 年龄选择，不能被更高 `global_pri` 的普通 item 挡住。

## 身份校验

boost 状态记录足够的身份信息，至少包括 UID、ROB、dynamic epoch 及 epoch 有效性，并记录其所属实例和允许继承的边界。旧实例 E 只允许同 UID、同 ROB 的 redirect reissue 继承到紧邻的 E+1；UID、ROB、dynamic epoch 不匹配，或跨越非连续生命周期的事件一律视为 stale，不得置位或继承 boost。

事件必须在最终 selected 阶段校验身份后再置位，不能因非最终 winner、候选阶段事件或迟到事件提前污染状态。

## 生命周期

- 最终 selected 且满足条件：置位 `uncache_retry_boost`，递增或初始化 `retry_count`，冻结当前 dynamic identity。
- `clear_uid_dispatch_result`：保留同一请求生命周期所需的 boost 状态。
- `prepare_uid_for_redirect_reissue`：保留 boost，并只允许紧邻 E+1 按同 UID、同 ROB 和 dynamic identity 继承。
- E -> E+1：仅在明确 redirect reissue 且身份连续时继承；完成继承后更新实例边界，防止重复继承。
- terminal/retire：清零 boost、retry count 和身份记录。
- reset：完整清零所有 boost、计数和身份状态。
- end-test check：检查是否存在遗留 boost、retry count 或无效身份，发现泄漏必须打印可定位信息。

日志只覆盖置位、继承、issue item snapshot 和 terminal 清理等低频状态变化，避免高频全表扫描和每拍全量打印。

## 边界

本方案明确不处理以下行为：`flushAfter`、异常 redirect、RAW nuke、普通 replay、非最终 winner、stale identity。它们不应置位或继承本 boost。非 subtype 5、无 `flush_itself`、UID/ROB/dynamic identity 无效的事件同样不 boost。

状态查询和清理应按 UID/ROB 或现有索引完成，不新增高频全表扫描。所有 issue item 字段必须有确定初始化值；缺失 snapshot 时按普通 item 处理并保留既有调度语义。

## 自审

- 未修改 `main_tr.send_pri`，boost 是独立动态状态。
- 只接受最终 selected 的 `MEMORY_VIOLATION + UNCACHE_NACK + flush_itself=1`。
- 复用已有 dynamic identity 字段，优先不扩展 raw monitor 结构。
- stale event 不能凭 UID 单字段重新锚定；继承要求同 UID、同 ROB、连续 E->E+1。
- 比较器读取冻结 snapshot，不动态读取 status。
- global-priority 预筛选不会吞掉 boosted item。
- terminal、retire、reset 和 end-test check 均覆盖清理与泄漏检查。
- 明确不处理 `flushAfter`、异常 redirect、RAW nuke、普通 replay、非最终 winner、stale identity。
- 不修改 RM agent 负责的 `mem_base_sequence.sv`、`memblock_rm_readonly_api.sv`、`memblock_rm.sv`、`rm_ls_core.sv` 及 RM 专项新文件。

## 验收标准

1. 仅目标事件可以置位 boost，其他边界事件均不置位。
2. boost 能跨 `clear_uid_dispatch_result`、`prepare_uid_for_redirect_reissue` 保留，并只在合法紧邻 E+1 继承。
3. 低 `send_pri` boosted item 可以胜过高 `send_pri` 普通 item。
4. 多个 boosted item 按 ROB 年龄选择；普通 item 仍按 `send_pri` 后 ROB 年龄选择。
5. issue item snapshot 在创建后不受 status 后续变化影响。
6. terminal、retire、reset 清理状态，end-test check 无泄漏。
7. directed test 覆盖置位、跨 redirect 保留、低 pri boost 胜高 pri、多个 boost 按年龄、非 subtype5/flushAfter/异常/RAW/replay 不 boost、旧 item snapshot、terminal/reset 清理。
8. 真实测试 `tc=basicTest ts=memblock_dispatch_real_smoke_vseq cfg=tc_l2tlb_fault_2pct_no_trigger seed=37617171` 自然结束，记录 UID862 subtype5 前后、Uncache-full 长阻塞，以及 `UVM_ERROR/UVM_FATAL`。

## 回滚方案

如实现导致编译、仿真或既有调度行为回归，回滚本方案涉及的状态字段、身份传递、issue item snapshot、scheduler boost 分支和 directed test 改动；保留本方案文档中的失败现象、`IMPLEMENTATION_DELTA` 和验证结果，不回退工作区中其他既有修改，不修改 RM 写集合。回滚后重新确认原有普通 `send_pri` + ROB 年龄调度和 `tc_sanity/base_fun` 基线。

## 测试计划

测试输出使用独立 mode/output 后缀：`fault2_uncache_retry_boost_20261015_*`，不清理或终止其他任务。

1. 先运行相关 directed test，覆盖验收标准中的状态、身份、调度和边界场景。
2. 再执行远端 compile，确认 VCS 编译和 filelist 完整。
3. 最后运行真实 seed：`tc=basicTest ts=memblock_dispatch_real_smoke_vseq cfg=tc_l2tlb_fault_2pct_no_trigger seed=37617171`，等待自然结束，禁止 `kill`。
4. 检查日志中的 UID862 subtype5 前后变化、Uncache-full 长阻塞、`TEST CASE PASSED`、`UVM_ERROR` 和 `UVM_FATAL`。
5. 失败时将 `IMPLEMENTATION_DELTA`、失败阶段、日志路径、根因分析和下一轮修正写回本文档，并进行 review 迭代。

## 执行中补充/修正（IMPLEMENTATION_DELTA）

- 来源：2026-10-15 原始 cfg 真实 seed 首版在线结果。
- 失败证据：UID853 的 subtype5 retry_count 超过 397 后最终完成；UID862 在约 889.838us 已累计到 614 次。首版长期同时保留 853/867/892/901/912/964 等多个 boosted UID，说明“所有历史 retry owner 一直 boost 到 terminal”的集合会累积，不能提供确定性前进保证。
- local/global filter 结论：首版 `boost_only` 只是把所有 eligible boosted item 放入各 target 的本地选择器；LOAD/STA/STD 各自仍可输出候选，因此并不存在一个跨 target 的唯一 owner。global `send_pri` 虽已在 boost 周期关闭，但多个 boost 之间仅按各本地队列 ROB 年龄排序，无法保证全局只服务最老 owner。
- 方案迭代：每拍扫描三个 issue queue，仅选择一个 live、eligible、ROB 最老的 boosted issue item 作为全局 winner token。三个 target 的筛选都只接受该 item 的 UID/target/ROB/replay_seq/dynamic_epoch 精确身份，本拍候选总数必须为 1；普通 global `send_pri` 在 token 有效时关闭。
- 有限生命周期：winner 真正通过 `mark_issue_fire*()` 成功 fire 后立即消费 boost token并清除身份，累计 `retry_count` 保留作诊断；若 DUT 再次返回最终 selected subtype5，则 `apply_uncache_retry_boost()` 显式重新挂起。未 fire、身份不匹配或 fire 被拒绝时不得消费。
- directed 验收补充：两个 boosted UID 且 pipeline width 大于 1 时总候选必须恰好为 1且是 ROB 最老者；未 fire token 保留；成功 fire 后 token 清除但 count 保留；下一最老者接管；新 subtype5 重新挂起并递增累计 count；日志打印 global winner 和三个 local 输出数量。
- 来源：2026-10-15 focused directed 单一 winner 验收（mode `fault2_uncache_retry_boost_20261015_directed_r10`）。
- 编译结果：VCS 完成 compile/elaboration/link，本轮新增 live-token、全局 winner 和 consume 接口无编译错误；编译日志为 `mem_ut/ver/ut/memblock/sim/fault2_uncache_retry_boost_20261015_directed_r10/log/vcs_compile_rtl.log`。
- 仿真结果：强制 `+MEMBLOCK_GLOBAL_SEND_PRI_EN_WT=100`。日志显示 UID0 为 global winner 且 local 输出 `load=1 sta=0 std=0`；UID0 成功 fire 后打印 `consume after issue`，随后 UID1 成为唯一 global winner；UID0 新 subtype5 将累计 count 从 1 增至 2并重新挂起。`TEST_PASS`，自然 `$finish` 于 `265.300ns`，`UVM_ERROR=0`、`UVM_FATAL=0`。
- 仿真日志：`mem_ut/ver/ut/memblock/sim/fault2_uncache_retry_boost_20261015_directed_r10/log/tc=basicTest_ts=memblock_uncache_retry_boost_directed_vseq_cfg=default_seed=666666_rtl_uncache_retry_single_winner_token_r10.log`。
- 来源：2026-10-15 原始 cfg 真实 seed r2 在线 review。
- 观察：UID685 在约 169.5159us 成功作为唯一 global winner fire并消费 token，随后仍出现有限次数 subtype5；一度考虑增加 `in-flight issue guard`，在 boosted fire 到反馈之间暂停全部 issue。
- 结论：该 guard 未被 r2 编译，也未进入任何通过的 directed/真实 seed 二进制；后续 r2 在线证据显示单一最老 winner + boosted reissue `ready_cycle=0` 已能持续推进。UID853 仅 3 次 subtype5 后于 253.6159us terminal，UID862 仅 2 次 subtype5 后于 256.2159us terminal，frontier 随后继续越过 UID3000。因此不能把单个 owner 的有限 retry burst 判定为新饥饿。
- 处理：不采用 `in-flight issue guard`。已撤销其全局停发、guard 状态/释放路径和 directed 断言，保留 r2 实际验证的单 winner、精确动态身份、global priority 绕过、fire 消费 token、后续 subtype5 显式重新挂起及 terminal 清理。这样避免把局部 Uncache retry 扩大为所有 LOAD/STA/STD 的全局反馈串行化。

- 来源：2026-10-15 首次 VCS 编译。
- 原计划：运行 pending-MMIO directed 场景并验证 boost 行为。
- 实现调整：将 directed task 中局部变量 `event` 重命名为 `wb_evt`。
- 原因：`event` 是 SystemVerilog 保留字，VCS 在解析该测试源码时于第 752 行报语法错误；该问题不改变 boost 设计语义。
- 影响范围：仅影响 `soft_test_memblock_pending_mmio_directed_sequence.sv` 的测试局部变量命名；首次编译结果为 `COMPILE FAIL`，需复编译确认。
- 来源：2026-10-15 第二次 VCS 编译。
- 原计划：复编译确认局部变量修正后进入 elaboration。
- 实现调整：在 `apply_uncache_retry_boost()` 中使用局部 `memblock_rob_key_t status_rob_key`，替代 `status.get_rob_key().flag/value` 的直接成员访问。
- 原因：VCS Q-2020.03-SP2 对函数返回 packed struct 的成员选择报告 `SV-FNYI`；局部快照写法保持同一 UID/ROB 身份校验语义。
- 影响范围：仅影响 `common_data_transaction.sv` 的编译兼容表达；第二次编译结果为 elaboration 期间 `SV-FNYI`，需再次复编译。
- 来源：2026-10-15 实现自审。
- 原计划：directed test 同时构造两个 boosted item 和一个普通 item，并假设本拍固定返回两个 load candidate。
- 实现调整：测试改为只构造两个 load，分别验证“低 `send_pri` boost 胜高 `send_pri` 普通项”和“两个 boost 第一项按 ROB 年龄”；不再依赖 `sample_load_pip_num()` 的随机返回数量。补充非最终 winner、RAW nuke、VLS exception redirect、普通 replay、flushAfter-class source、stale dynamic identity、terminal/reset 清理和 end-test 泄漏检查。
- 原因：原测试调用 `build_load_table(3)` 超过 helper 的二项上限，并把 `load_items.size()==2` 当作固定条件，既会运行时 fatal，也受 pipeline 配置影响而不稳定。
- 影响范围：仅增强 boost directed 覆盖和一致性审计；主体调度语义不变。生产路径额外拒绝 `is_vls_exception=1`，issue snapshot 身份要求 `has_dynamic_epoch=1`。
- 来源：2026-10-15 独立测试入口 review。
- 实现调整：新增 `soft_test_memblock_uncache_retry_boost_sequence` 和 `memblock_uncache_retry_boost_directed_vseq`，覆盖父类 `body()` 后只运行 boost 专项场景。
- 原因：完整 `memblock_pending_mmio_directed_vseq` 还包含 owner、rollback provenance 和 cancel 等无关场景，不能作为 boost 修改的独立验收结论。
- 验收命令：`tc=tc_dispatch_fault_smoke ts=memblock_uncache_retry_boost_directed_vseq`，使用唯一 mode 编译和仿真。
- 来源：2026-10-15 focused directed 第一次编译（mode `fault2_uncache_retry_boost_20261015_directed_r4`）。
- 结果：VCS parsing 通过，partition elaboration 在测试第 871/872 行报告 `make_issue_item` 参数不足；没有主体实现错误。
- 修正：为 uid0/uid1 显式派生 `memblock_op_behavior_t`，按正式三参数接口构造比较器测试 item。下一轮使用新 mode 复编。
- 来源：2026-10-15 focused directed 入口修正。
- 结果：mode `fault2_uncache_retry_boost_20261015_directed_r6` 使用 `tc_dispatch_fault_smoke` 时，testcase 忽略 `VSEQ_MAIN`，实际未执行 focused vseq；随后尝试在同一 mode 改用 `basicTest` 触发增量重编，旧 partition 数据库 `tdc.sdb` 损坏，VCS 报 `VFS_SDB_ERROR`。该失败属于 mode/testcase 复用问题，不是 boost 代码或测试断言失败。
- 修正：不清理或复用旧 mode，改用唯一 mode `fault2_uncache_retry_boost_20261015_directed_r7`，以 `tc=basicTest ts=memblock_uncache_retry_boost_directed_vseq` 从零编译和运行。
- 来源：2026-10-15 focused directed 最终验收（mode `fault2_uncache_retry_boost_20261015_directed_r7`）。
- 编译结果：VCS/Verdi elaboration 完成，`0 error(s), 0 warning(s)`；编译日志为 `mem_ut/ver/ut/memblock/sim/fault2_uncache_retry_boost_20261015_directed_r7/log/vcs_compile_rtl.log`。
- 仿真结果：确认 `basicTest` 解析并启动 `memblock_uncache_retry_boost_directed_vseq`；日志出现 `uncache retry boost directed checks completed` 和 `uncache retry boost focused directed checks completed`，覆盖最终 selected subtype5、跨 redirect E->E+1、低 priority boost、多个 boost ROB 年龄、非目标事件、snapshot、terminal/reset 清理。仿真自然 `$finish` 于 `265.300ns`，`UVM_ERROR=0`、`UVM_FATAL=0`。
- 仿真日志：`mem_ut/ver/ut/memblock/sim/fault2_uncache_retry_boost_20261015_directed_r7/log/tc=basicTest_ts=memblock_uncache_retry_boost_directed_vseq_cfg=default_seed=666666_rtl.log`。
- 来源：2026-10-15 接管review，重点复核global-priority预筛选。
- 发现：存在eligible boosted item时，原实现跳过`find_global_max_send_pri()`，但没有清除已采样的`use_global_pri`；当本拍global priority开启时，未初始化/无关的`global_pri`仍可能在target筛选中丢掉全部boosted item。
- 修正：初始化`global_pri=0`，并在`boost_only=1`时显式关闭普通`use_global_pri`。此时retry boost本身就是跨LOAD/STA/STD的全局预筛选，boosted item内部继续按ROB年龄选择。
- 验证要求：focused directed运行强制`+MEMBLOCK_GLOBAL_SEND_PRI_EN_WT=100`，确保低`send_pri` boosted item胜过高`send_pri`普通item的检查确定性覆盖global-priority开启路径。
- 来源：2026-10-15 原始cfg真实seed首轮运行（mode `fault2_uncache_retry_boost_real_seed37617171_20261015_r1`）在线review。
- 发现：UID853在每次subtype5 redirect后约32.5ns重建boosted issue item，但约480ns后才真正issue；`make_issue_item()`把主事务的固定随机`main_tr.delay`在每次reissue重新装入`ready_cycle`。boost只参与eligible候选排序，等待固定delay期间年轻请求仍可重新填满16个Uncache entry，UID853连续重复subtype5，说明仅提升排序优先级不完整。
- 修正：合法boosted issue item冻结后将该次`ready_cycle`置0。原始`main_tr.delay`不修改；普通item继续使用原始delay，只有已经通过UID/ROB/dynamic-epoch校验的Uncache-full紧邻reissue绕过重复delay。
- directed补充：普通uid0/uid1均设置`delay=50`，确认普通item保持50；uid0置boost后确认其queue snapshot为0，而普通uid1仍为50。

## 真实 seed r2 最终结果与方案冻结

- 运行：`mode=fault2_uncache_retry_boost_real_seed37617171_20261015_r2`，`seed=37617171`，原始 `cfg=tc_l2tlb_fault_2pct_no_trigger`。
- 日志：`mem_ut/ver/ut/memblock/sim/fault2_uncache_retry_boost_real_seed37617171_20261015_r2/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=37617171_rtl_uncache_retry_single_winner_real_seed37617171_r2.log`。
- 结束方式：仿真自然结束，非手动停止；`$finish @ 1125011.900ns`，`UVM_FATAL=0`，`UVM_ERROR=10`。第 10 条 error 触发 UVM quit count `10/10`，因此在 RM commit 到 UID3570、dispatch/reissue 日志到 UID3705 时结束，未完成 `main_trans_num=10000`。
- 原主阻塞验收：UID853 仅产生 3 次 selected subtype5，`253608.400ns` normal pass、`253615.900ns` terminal clear；UID862 仅产生 2 次 selected subtype5，`256208.400ns` normal pass、`256215.900ns` terminal clear。之后事务持续推进超过 UID3700，未再次卡在 UID853/862。
- 对比：原 baseline 中 UID862 subtype5 约 10074 次，首版多 boost owner 方案也曾长期累积。r2 的有限 retry 后 terminal 和持续 frontier 前进证明“单一最老 winner + 精确动态身份 + global priority 绕过 + boosted reissue ready=0 + fire 消费/新 subtype5 重挂”已解决本主问题的 retry starvation。
- 结论：主方案验收通过并冻结，不再增加 `in-flight issue guard` 或其他全局停发逻辑。当前源码已撤销未进入 r2 二进制的 speculative guard，保持与 r2 已验证实现一致；不重启或复用相同 r2 mode。
- 完整随机用例结论必须分开表述：**主阻塞通过，但 testcase 总体未通过**。10 条 error 是 1 条 UID98 exception mismatch 和 9 条 RM Load data/data_valid mismatch；它们均为独立 RM expectation/model 问题，不属于 retry scheduler。
- RM 协同：9 条 data mismatch 已定位为 FP Load NaN-boxing 期望缺失，详见 `AI_DOC/analysis/source_sv/fault2_rm_fp_load_nan_box_mismatch_20261015.md`。UID98 exception mismatch继续沿用独立 RM translation/PMA 分析，不并入主方案。
