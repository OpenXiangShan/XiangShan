# memblock 并发回归退出身份竞态修复 Plan

状态：实现、验证和 review closure 完成，已归档
创建日期：2026-10-25
责任类别：测试框架
关联脚本：`mem_ut/ver/ut/memblock/scripts/run_eda_regression.sh`

## 1. Plan 定位、专有名词与抽象功能说明

本 plan 修复远端并发回归 controller 在 worker 退出、RC 文件可见性和远端 probe 交错时产生的错误
quarantine。修改范围只包含 Bash 测试框架脚本、可重复 shell 测试和对应文档，不修改 RTL、Scala、
SystemVerilog 激励、RM 或 cfg。

### 1.1 专有名词

| 术语 | 当前语境中的中文含义 | 代码落点 | 场景示例 |
|---|---|---|---|
| `controller` | 当前节点上的回归总控进程 | `run_eda_regression.sh` 普通入口 | 调度 compile 和多个 seed |
| `managed job` | 由 controller 持久记录身份并负责探测、终止、收尾的远端任务 | `compile`、`seed:<seed>` | `seed:101` |
| `run token` | 一次 managed job 实例的唯一身份 | ACK、RC、owner manifest、RPC | 防止 PGID 复用误判 |
| `matching RC` | protocol、job、token、PGID 和结束标记全部匹配当前 owner 的返回码文件 | `status/<job>.rc` | 旧批次或错误 token 的 RC 不可消费 |
| `stability window` | 在终态 probe 后等待共享/远端 RC 可见的有限重试窗口 | 新增 RC recovery helper | 最多按 `SSH_RETRIES` 次尝试 |
| `remote phase` | 与仿真 PASS/FAIL 独立的远端生命周期 | `REMOTE_PHASE[$job]` | `OWNED -> EXIT_CONFIRMED` |
| `IDENTITY_MISMATCH` | PGID 仍有 live member，但没有进程携带当前 token | `remote_rpc()` | 可能是退出发布竞态，也可能是真身份冲突 |
| `REMOTE_UNKNOWN` | 无法证明当前 token 对应 job 已安全退出 | `mark_remote_unknown()` | 必须 quarantine mode |
| `quarantine` | 禁止该 mode 继续复用的安全清单 | `<mode>.quarantine.tsv` | 真 token mismatch 必须保留 |
| `INFRA` | 远端生命周期已确认，但 RC、日志或基础设施证据不完整 | `RESULT_STATE[$job]` | `EXITED` 后 RC 永不到 |

### 1.2 关键函数抽象功能描述

`validate_matching_rc_file()`：校验一个候选 RC 文件是否完整属于当前 managed job；它只做身份和 schema
检查，不推进 remote phase。

`consume_matching_rc_file()`：在候选文件通过严格校验后，把 RC 和结束时间写入 controller 内存，并把
`OWNED` 推进为 `RC_SEEN`；它不判断进程组是否已退出。

`recover_matching_rc_with_retry()`：在终态 probe 后按有限次数同时检查共享路径与远端路径，只发布和消费
匹配 RC；它不因错误 token、截断文件或 SSH 失败放松身份规则。

`settle_terminal_rpc_status()`：统一处理 probe/signal 返回的 `EXITED` 和 `IDENTITY_MISMATCH`，在 RC
稳定窗口后决定 `EXIT_CONFIRMED + finalization`、`INFRA` 或 `REMOTE_UNKNOWN`；compile 与 seed 共用。

## 2. 目标功能 Flow 总览

```text
controller 本轮 observe_rc
  -> framed RPC 返回 EXITED 或 IDENTITY_MISMATCH
  -> 内存已有 matching RC？
       -> 是：EXIT_CONFIRMED -> finalize
       -> 否：进入有限 RC stability window
            -> 每次先检查共享 RC
            -> 未命中则 SSH 读取远端 RC 到临时文件
            -> 只有完整匹配 job/token/PGID 的 RC 才原子发布和消费
            -> 窗口内命中：EXIT_CONFIRMED -> finalize
            -> 窗口结束仍未命中：
                 EXITED -> EXIT_CONFIRMED -> result INFRA，不 quarantine
                 IDENTITY_MISMATCH -> REMOTE_UNKNOWN + quarantine
```

该 flow 同时由 `probe_active_jobs()` 和 `signal_active_jobs()` 调用。compile 主循环与 seed 主循环继续调用
同一 probe 入口，不新增两套状态机。

## 3. RC 身份与状态语义

匹配 RC 必须同时满足：

1. `protocol=EDA_REGRESSION_JOB_V1`。
2. `job_id` 等于当前 `compile` 或 `seed:<seed>`。
3. `token` 等于当前 `TOKEN[$job]`。
4. `pgid` 等于当前 `PGID[$job]`。
5. `rc` 是非负十进制整数。
6. `ended_epoch_ns` 是十进制时间戳。
7. `end=RC_END`。

候选文件不满足任一条件时不得覆盖本地 RC、不得推进 `RC_SEEN`、不得作为退出证明。

状态决策固定如下：

| RPC 状态 | 稳定窗口结果 | remote phase | 业务结果/安全动作 |
|---|---|---|---|
| `EXITED` | matching RC 到达 | `EXIT_CONFIRMED` | 正常 finalization |
| `EXITED` | matching RC 永不到 | `EXIT_CONFIRMED` | compile/seed 均记 `INFRA`，不 quarantine |
| `IDENTITY_MISMATCH` | matching RC 到达 | `EXIT_CONFIRMED` | 原 worker 已由 RC 证明结束，不 quarantine |
| `IDENTITY_MISMATCH` | matching RC 永不到 | `REMOTE_UNKNOWN` | 写 quarantine，停止批次 |

## 4. 主流程实现 Flow

### 4.1 严格 RC 校验和消费

抽象功能描述：校验 helper 只判断候选文件是否属于当前 owner；消费 helper 只在校验成功后更新 RC 观测
状态，不替代远端退出确认。

文字伪代码：

1. 从候选文件读取 protocol、job、token、PGID、RC、结束时间和结束标记。
2. 逐项与当前 job 的内存身份比较；任何字段缺失、截断或不匹配立即返回失败。
3. 校验成功后，消费 helper 保存 RC 与结束时间。
4. 当前 phase 为 `OWNED` 时推进到 `RC_SEEN`；已经是 `RC_SEEN` 或 `EXIT_CONFIRMED` 时保持原 phase。
5. RC 消费本身不进入 `EXIT_CONFIRMED`，仍由 probe/signal 的终态证据决定。

### 4.2 有限 RC 稳定窗口

抽象功能描述：终态 RPC 已出现但本轮未见 RC 时，优先恢复严格匹配的 RC，吸收共享文件系统可见性
延迟；它不无限等待，也不把任意同路径文件当成当前 job 的 RC。

文字伪代码：

1. 重试次数复用现有 `SSH_RETRIES`，每次间隔一秒；不新增公开 CLI 参数。
2. 每次先校验共享路径中的 RC；命中后消费并结束。
3. 共享路径未命中时，通过单次 SSH `cat` 读取远端同一路径到本地临时文件。
4. 远端读取使用命令级 `timeout`，单次 SSH 连接成功但远端 `cat` 阻塞时必须在固定上限内返回；
   整个稳定窗口的最坏耗时为 `SSH_RETRIES × (单次读取超时 + 一秒间隔)`。
5. 临时文件通过完整 matching RC 校验后才使用原子 rename 发布到正式路径，再消费 RC；原子发布失败时
   删除临时文件并返回失败，不更新 RC、结束时间或 remote phase。
6. 所有 RC 恢复入口统一调用本 helper。现有 `recover_file_over_ssh()` 只继续用于 ACK 等非 RC 文件；
   `finalize_exited_job()` 不得再用该通用函数直接恢复 RC，避免未校验候选覆盖本地正式 RC。
7. 错误 token、错误 job、错误 PGID、截断文件、远端文件不存在或单次 SSH 失败都只算本次未命中，
   但不得发布到正式 RC 路径；后续重试出现有效 RC 时仍可恢复。
8. 达到有限次数后返回失败，由 RPC 状态决定 INFRA 或 REMOTE_UNKNOWN。

### 4.3 统一终态收敛

抽象功能描述：该 helper 是 probe 和 signal 对 `EXITED`/`IDENTITY_MISMATCH` 的唯一决策入口，保证
compile 与 seed 使用相同安全规则。

文字伪代码：

1. 若内存尚无 RC，调用有限恢复 helper。
2. 若已得到 matching RC，把 phase 设为 `EXIT_CONFIRMED`，更新 owner manifest，并调用现有
   `finalize_exited_job()`。
3. 若没有 RC 且 RPC 为 `EXITED`，仍把 phase 设为 `EXIT_CONFIRMED`，因为远端 framed probe 已证明
   PGID 无 live member；随后 finalization 把缺失 RC 归类为 `INFRA`。
4. 若没有 RC 且 RPC 为 `IDENTITY_MISMATCH`，调用 `mark_remote_unknown()`，保留 owner 并写 quarantine。
5. signal 路径仅对 `SIGNALED` 更新 disposition；它返回的 `EXITED`/`IDENTITY_MISMATCH` 复用同一 helper。

### 4.4 Finalization 的 compile/seed 一致性

`classify_seed_job()` 调整优先级如下：已由 monitor 提交的根因 FAIL 保持不变；基础设施日志错误和
UVM error/fatal 仍优先于 RC；随后先检查 RC 是否缺失，缺失时无条件记为 `INFRA`，不得被
`SIGNAL_DISPOSITION` 后的完整 PASS 日志或 ABORTED 分支掩盖；只有 RC 存在时才按 signal disposition、
完整 PASS 标记和 RC 值继续分类。这样 `EXITED + RC 永不到` 在 seed 路径稳定为 INFRA。

compile finalization 同步调整为：缺失 RC 时 `RESULT_STATE[compile]=INFRA`；RC 为 0 时 PASS；非零时
FAIL。compile gate 仍然失败并返回退出码 5，不启动 seed、不提交成功 fingerprint，且不把已经明确
`EXITED` 的任务写入 quarantine。

`finalize_exited_job()` 的 RC 恢复必须复用严格 matching RC helper；不得调用通用
`recover_file_over_ssh()` 直接覆盖正式 RC。finalization 再次进入时可以复用已经消费的 `RC[$job]`，
否则只允许严格恢复一次后继续日志收集和分类。

## 5. Fake SSH/worker 可重复测试

新增 `mem_ut/ver/ut/memblock/scripts/tests/` shell 测试资产，使用 PATH 中的 fake `ssh` 在本机执行远端
命令，使用 fake remote entry 生成最小 compile artifact、fingerprint metadata 和完整 PASS 日志。测试专用
`mv` wrapper 只在 fake SSH 子环境中生效，可延迟或丢弃 `*.rc` 的原子发布；生产路径不读取这些测试变量。

至少覆盖：

1. **RC 晚到后 PASS**：compile 与 seed 的 worker 已退出，RC 延迟发布，RPC 短暂返回
   `IDENTITY_MISMATCH`；稳定窗口恢复 matching RC，整批 PASS，无 quarantine。测试必须记录并断言
   controller 日志确实进入“内存尚无 RC 的 `IDENTITY_MISMATCH` 恢复”分支，不能只检查最终 PASS。
2. **RC 永不到转 INFRA**：RPC 返回 `EXITED`，RC 被测试 wrapper 丢弃；seed 结果为 INFRA，
   `remote_unknown_jobs=0`，无 quarantine。
3. **真 token mismatch**：PGID 保留无 token 的 live member且 RC 永不到；结果进入
   `REMOTE_UNKNOWN`，生成 quarantine，controller 非零退出。
4. **jobs=4 近同时退出**：四个 seed 的 RC 同时延迟，四个 PGID 在 probe 时短暂无 token；全部恢复为
   PASS，`peak_active=4`，不得误 quarantine。fake worker 使用同步屏障，四个任务全部到达退出/RC
   延迟点后才统一释放，测试逐 job 断言均命中过目标竞态，不能仅用 `peak_active=4` 代替证明。

补充边界矩阵：

5. **compile EXITED + RC 永不到**：compile 结果为 INFRA，controller 退出码 5，不启动任何 seed，
   不写成功 fingerprint，`remote_unknown_jobs=0` 且无 quarantine。
6. **probe 与 signal 入口一致性**：分别让 PROBE、SIGNAL_TERM 和 SIGNAL_KILL 返回
   `EXITED`/`IDENTITY_MISMATCH`，确认三者复用同一终态收敛；只有 `SIGNALED` 能修改
   `SIGNAL_DISPOSITION`。
7. **非法 RC 不发布**：依次注入错误 token、错误 job、错误 PGID、截断 RC 和单次 SSH 失败，断言正式
   RC 路径未被覆盖、内存 RC 未消费、remote phase 未推进；随后注入有效 RC，必须仍能恢复并通过。

测试结束必须清理 fake live member、独立 mode 的 owner/quarantine 和临时目录，不访问用户正在运行的
`functional_cfg_cov_closure_clean_20261025`。

## 6. 静态与真实回归验证

1. `bash -n` 检查生产脚本和全部新增 shell 测试资产。
2. `shellcheck` 可用时执行；不可用时记录环境缺口，并使用 `bash -n`、`git diff --check` 和定向 `rg`
   作为静态门禁。
3. 执行完整 fake 测试矩阵，逐项检查 exit code、summary、results、quarantine 和 owner。
4. 真实短回归使用本 worktree 的独立新 mode，优先 `default` cfg、4 个显式 seed、`--jobs 4`、
   `--no-coverage --no-wave`。若可安全复用本 worktree 已有 simv，则 `--no-compile`；否则在独立 mode
   compile。不得访问、停止、复用或修改 `functional_cfg_cov_closure_clean_20261025`。
5. 验收要求：4/4 PASS、`infra=0`、`remote_unknown_jobs=0`、无 quarantine，结果目录完整保留。

性能与响应边界：稳定窗口只在 RPC 已返回终态且本轮未见 RC 时触发，不进入普通 ALIVE 高频路径。
同一 framed RPC 中的多个 job 逐个恢复，单 job 和整批最坏延迟必须在 fake 测试中记录；恢复循环每次
SSH 前后调用 `handle_monitor_triggers()`，避免多 job 串行等待期间延迟提交已经发布的根因 trigger。

## 7. 文档与提交

1. 更新当前周唯一 `AI_DOC/buglist/rm/v2/rm_buglist_2026-W43_20261019.md` 原章节，不创建第二个周文件。
2. plan review 通过后执行 coding；实现验证和独立实现 review 通过后把本 plan 从 `undo` 移到 `do`。
3. implementation review 放在 `AI_DOC/plan/test_framework/review_doc/undo/`，并覆盖每个脚本修改点和测试。
4. 同步 `mem_ut_parallel_eda_regression_usage_rule.md` 中的终态恢复与 quarantine 语义。
5. 一个 issue 一个 commit，commit 不包含 RTL/Scala 或其它 worktree 修改。

## 8. 与既有方案差异说明

既有并发回归加固方案把 `IDENTITY_MISMATCH` 视为必须立即 quarantine 的身份风险；该规则对 stale owner
审计仍然正确。本 plan 只修正**当前 controller 正在管理、且拥有唯一 RC 路径与完整 job/token/PGID
上下文**的退出竞态：先给 matching RC 一个有限恢复窗口。没有 matching RC 时，
`IDENTITY_MISMATCH` 仍按原规则 quarantine，不修改 stale owner 的保守审计策略。

## 8.1 执行中补充/修正（IMPLEMENTATION_DELTA）

### [IMPLEMENTATION_DELTA] fake worker bootstrap 注入

**来源**：coding 中完整 controller fake 测试仍会执行项目默认 module bootstrap，而当前本地测试节点没有
`module` shell function，fake worker 会在进入 fake remote entry 前以 rc=127 退出。

**原 plan**：要求 fake SSH/worker 从生产 `--remote-worker` 入口执行，但未指定如何隔离 EDA module
bootstrap。

**实现调整**：增加内部环境变量 `EDA_REGRESSION_REMOTE_BOOTSTRAP`；未设置时仍读取 Makefile 的原
bootstrap，fake 测试显式设为 `:`。该变量不进入 CLI、config、usage 参数表，也不改变生产默认值。

**原因**：必须保持生产 worker/ACK/RC/RPC 路径完整执行，同时避免测试节点环境缺失掩盖被测竞态。

**影响范围**：仅 `run_eda_regression.sh` bootstrap 赋值和 fake 测试 harness；无 RTL/Scala/cfg 影响。

### [IMPLEMENTATION_DELTA] interval peak 批量重算

**来源**：首轮 independent implementation review 发现把 peak 重算放入单 job RC consume 后，
`observe_rc()` 会对同一批 RC 重复排序全表。

**原 plan**：要求 RC helper 分层和有限恢复，没有规定 `peak_live` 的重算粒度。

**实现调整**：`consume_matching_rc_file()` 只更新单 job 状态；`observe_rc()` 汇总本轮是否出现新的
`END_EPOCH_NS`，整批最多重算一次。terminal recovery 只在单 job 成功恢复时重算一次。

**原因**：保持旧实现每轮 observation 一次排序的性能边界，避免大批次调度和 monitor 处理延迟。

**影响范围**：仅统计重算时机，不改变 RC 身份、remote phase 或结果分类。

### [IMPLEMENTATION_DELTA] timeout 返回后的 monitor 提交点

**来源**：首轮 independent implementation review 要求测试能证明远端 RC 读取阻塞时 monitor trigger
仍在 signal 前提交。

**原 plan**：要求每次 SSH 前后调用 `handle_monitor_triggers()`。

**实现调整**：单次 bounded SSH 返回后、候选校验和成功 return 前显式再次处理 trigger；fake 测试注入
30 秒远端 stall，由 5 秒命令 timeout 截断，并检查 commit 时间早于 TERM 时间。

**原因**：避免 SSH 在窗口内阻塞期间出现的根因 trigger 到 RC 成功 return 后仍未提交。

**影响范围**：monitor 提交时机更严格，不改变正常无 trigger 路径。

## 9. RM 协同支持

本 plan 不实现 RM/checker/scoreboard；无新增协同字段。

## 10. 功能覆盖率协同支持

本 plan 不实现 coveragent/covergroup；无新增功能覆盖率采样点。
