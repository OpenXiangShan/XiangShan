# memblock 远端并发仿真脚本加固与 plusargs 实现 Review

日期：2026-10-20
适用版本：V2 `mem_ut_uvm_v2`
实现文件：`mem_ut/ver/ut/memblock/sim/run_eda_regression.sh`
关联 plan：`AI_DOC/plan/test_framework/plan/do/memblock_parallel_eda_regression_hardening_plusargs_plan_20261019.md`

## 1. 术语与抽象功能说明

| 英文术语 | 当前中文含义 | 代码落点 | 示例 |
|---|---|---|---|
| `controller` | 当前节点维护整批回归的总控 shell 进程 | `run_eda_regression.sh` 主流程 | 调度 seed、写 summary |
| `managed job` | 可通过 token、PGID、ACK、RC 持久识别和回收的远端任务 | compile 与 `seed:<n>` | 编译和仿真使用同一生命周期 |
| `remote phase` | 远端进程生命周期，不等同于业务结果 | `REMOTE_PHASE[]` | `OWNED -> EXIT_CONFIRMED` |
| `result state` | seed 的业务结论 | `RESULT_STATE[]` | `PASS`、`FAIL`、`INFRA` |
| `local finalization failure` | 远端终态已确定，但本地记录或 owner 删除提交失败 | `FINALIZATION_FAILURE[]`、`RECORD_INFRA_FAILURE` | 保持 `EXIT_CONFIRMED` 并使批次 FAIL |
| `monitor commit` | 根因错误已完成持久记录的提交证明 | `monitor_failure.committed` | 提交后才允许联停 |
| `stable snapshot` | 远端进程退出后通过前后 stat 和字节数校验的完整日志 | `fetch_stable_remote_log()` | 防止 NFS 尾部延迟误判 |
| `framed RPC` | 带 request id、数量、hash 和 END 的 probe/signal 协议 | `run_remote_rpc()` | 截断响应拒绝执行 signal |
| `quarantine` | 无法证明远端退出时阻止 mode 复用的持久隔离记录 | `*.quarantine.tsv` | stale owner 仍存活 |

抽象功能说明：本 review 检查脚本是否实现用户要求的参数透传、编译一次、多 seed 并发、并发监测、先记录后联停、PASS/FAIL 汇总和异常恢复；不判断 DUT 或 testcase 本身的功能正确性。

## 2. Review 范围和结论

检查范围包括脚本参数解析、cfg/plusargs、Makefile 参数合成、coverage 开关、远端生命周期、monitor 提交顺序、结果分类、owner 恢复、mock 故障注入和 20 路并发证据。

当前实现已修复复审发现的 monitor、cleanup、cfg 冻结、stale owner、summary、PGID 复用和本地 finalization 状态混用问题。cleanup 开始和首次 probe/finalize 后都会处理已发布的 monitor trigger；cleanup 期间再次收到 INT/TERM 只记录附加信号，不跳过后续清理。三路独立 subagent 最终分别覆盖参数/cfg/coverage、生命周期/RPC/owner、monitor/summary/tests/docs，结论均为“无必须修改项，可通过”。

## 3. 关键函数职责和控制流

### 3.1 `validate_plusarg()` 与 Makefile effective plusargs

抽象功能描述：`validate_plusarg()` 在本地 CLI 和远端 worker 两侧验证 runtime plusarg 的格式、字符集、保留 key 和重复 key；Makefile 的 `EFFECTIVE_PLUSARGS` 负责按 cfg 覆盖规则生成唯一有效参数列表。脚本不在 compile job 中传递 runtime plusargs。

源码位置：`run_eda_regression.sh` 的 `validate_plusarg()`、`remote_worker()`；`sim/Makefile` 的 `EFFECTIVE_PLUSARGS`。

```bash
--plusarg +KEY=VALUE
```

中文伪代码：先要求 token 只包含一个等号，再拆出 key/value；key 必须符合标识符规则，value 必须符合安全字符白名单；保留 key 或重复 key 立即返回 2。正式执行通过 `make eda_print_effective_plusargs` 得到 cfg 过滤后的最终列表，所有 seed 使用同一列表，compile 不添加这些参数。

正确性检查：`+MEMBLOCK_MAIN_TRANS_NUM=1000` 可透传；空值、额外等号、shell 元字符、重复 key 和 `UVM_VERBOSITY` 等保留 key 会 fail-fast。输出目录同时保存 `user_plusargs.txt` 和 `effective_plusargs.txt`。

### 3.2 `create_managed_job()` 与 owner manifest

抽象功能描述：`create_managed_job()` 为 compile 或 seed 创建唯一 token，在发起 SSH 前先持久化 `LAUNCHING + PGID=UNKNOWN` owner，收到 ACK 后更新为 `OWNED`。它负责建立可恢复身份，不负责判定最终 PASS/FAIL。

源码位置：`run_eda_regression.sh` 的 `create_managed_job()`、`write_owner_manifest()`。

```bash
TOKEN[$job]="$token"; PGID[$job]="UNKNOWN"; REMOTE_PHASE[$job]="LAUNCHING"
OWNER_ACTIVE[$job]=1
write_owner_manifest || ...
```

中文伪代码：先登记 job、token、host、run id 和日志路径；原子写 owner 失败时不启动 SSH，并把该任务转为基础设施失败；owner 成功后通过 `setsid` 启动远端 worker；ACK 校验 protocol、job id、token、pid/pgid/sid 和 END 标志，成功才进入 OWNED。ACK 不可见时执行 token discovery；仍不能证明身份则写入 quarantine。

### 3.3 `handle_monitor_triggers()` 和先记录后联停

抽象功能描述：`handle_monitor_triggers()` 把 watcher 发布的 UVM_ERROR/UVM_FATAL 阈值事件转换成根因 seed 的权威 FAIL，并在所有必要记录成功后才设置全局停止请求。

源码位置：`run_eda_regression.sh` 的 `handle_monitor_triggers()`、`cleanup_controller()`。

```bash
set_seed_state "$job" FAIL ...
write_monitor_events
write_progress_records 0
atomic_write "$OUTPUT_DIR/monitor_failure.committed"
STOP_REQUESTED=1
```

中文伪代码：校验 trigger 中 seed、token、count 和错误行；先更新 root seed state 为 FAIL，再写 monitor event；record 开启时写 progress/results；全部成功后原子写 commit marker，最后打印错误并设置 STOP_REQUESTED。cleanup 入口先调用该函数，首次 probe/finalize 后再次调用，覆盖 controller 恰好收到用户信号的竞态。任一步写失败则写 uncommitted 标记、设置基础设施失败并保持最终 FAIL。

### 3.4 `finalize_exited_job()` 与 `compute_batch_result()`

抽象功能描述：`finalize_exited_job()` 只处理已经被远端 probe 确认退出的 job，恢复 RC、获取 stable snapshot、分类结果并在持久记录完成后移除 owner；`compute_batch_result()` 只计算最终批次结论，不负责写 summary。

源码位置：`run_eda_regression.sh` 的 `finalize_exited_job()`、`fetch_stable_remote_log()`、`compute_batch_result()`。

中文伪代码：RC 文件出现只推进 RC_SEEN，不直接清理；probe 返回 EXITED 后获取远端日志前后 stat，字节数一致才发布 final log；日志包含 UVM 错误时记 FAIL，完整 PASS 标记和 rc=0 才记 PASS，signal 成功且无完整 PASS 时记 ABORTED。记录和 finalized marker 全部成功后才移除 owner。本地记录失败时设置独立 finalization failure 并保留 `EXIT_CONFIRMED`，不得改写为 `REMOTE_UNKNOWN`。下一批 stale audit 遇到持久化的 `EXIT_CONFIRMED` owner 时直接清理，不再 probe 可能已经复用的 PGID。最终计算要求所有 seed 为 PASS、没有 active/unknown owner、没有 quarantine、没有记录故障且 simv identity 未变化，否则返回 FAIL。

## 4. 参数和用户需求覆盖

| 需求 | 实现和验证 |
|---|---|
| testcase、cfg、count、seed/seeds | `--tc`、`--cfg`、`--count`、重复 `--seed`、`--seeds`；显式 seed 去重并使 count 失效，但非法 count 仍校验 |
| 并发数量 | `--jobs`，默认 20；`peak_active` 统计 controller 调度峰值，`peak_live` 只按 seed ACK/RC 纳秒区间最大重叠计算 |
| compile once | `--compile` 建立 managed compile；完成 fingerprint 后 seed 使用 no-compile 仿真 |
| wave、coverage、verbosity、debug | 默认 wave/coverage 关闭、UVM_LOW；debug 派生 FSDB 和 UVM_DEBUG，显式选项可覆盖 |
| runtime plusargs | 可重复 `--plusarg +KEY=VALUE`，每个 seed 透传，cfg 同名项只保留用户值 |
| 并发监测 | 默认开启；error-limit 达阈值时先提交 root FAIL，再联停全部任务 |
| PASS/FAIL | 全部 seed PASS 且无异常时 summary 写 `result=PASS` 并退出 0，否则 `result=FAIL` 并退出非零 |
| no-record | 关闭 progress/results/success/failed 动态文件，但保留 seed state、monitor event/commit 和 summary |

## 5. 测试证据

### 5.1 静态和协议测试

- `bash -n`：通过。
- `git diff --check`：通过。
- 缺值、非法数字、前导零、cfg 缺失、plusarg 重复/保留 key：按预期返回 2。
- framed RPC：合法 `EXITED` 响应返回 0；尾部追加数据返回 2；非法 hash 和 signal 请求不会执行信号。
- `mv -fT` 故障注入：summary、monitor commit、owner manifest 目标为目录时写入失败并最终非零，不会误判 PASS。

### 5.2 mock regression

| 场景 | 结果 |
|---|---|
| 40 seed，jobs=20，全 PASS | 初始压力测试 `pass=40`、`peak_active=20`、`peak_live=20`、`result=PASS` |
| 20 个零延时 seed，jobs=20 | `pass=20`、`peak_active=20`、`peak_live=1`，证明不会按累计 owner 高报 |
| 20 个长任务 seed，jobs=20 | `pass=20`、`peak_active=20`、`peak_live=20`、`remote_unknown_jobs=0` |
| compile 跨 probe 周期后失败、零 seed 启动 | `result=FAIL`、`peak_active=0`、`peak_live=0`，compile 不污染 seed 存活峰值 |
| monitor early stop | root seed state 先为 FAIL，随后生成 `monitor_failure.committed`，最终 result=FAIL |
| `--no-record` early stop | 不生成 progress/results，保留 monitor event、commit 和 summary |
| `--no-monitor` | 失败 seed 最终 FAIL，其它 seed 继续完成，无提前联停 |
| managed compile/no-compile | compile ACK/RC/fingerprint 完成，后续 seed 复用 simv |
| SIGKILL stale owner | 下一批发现远端仍 ALIVE，写 quarantine 并拒绝复用；watcher 随 controller 退出 |
| `EXIT_CONFIRMED` 后 finalized marker 写失败 | 首批 FAIL、`remote_unknown_jobs=0`、owner 保持 `EXIT_CONFIRMED`、无 quarantine；下一批同 mode PASS |
| stale `EXIT_CONFIRMED` owner 的 PGID 被无关活进程复用 | stale audit 不再 probe；新批次 PASS，无关进程在 audit 前后均为 ALIVE，owner 被清空且无 quarantine；证据为 `sim/.eda_regression/mock_exit_confirmed_retry_20261020_v1_reused_pgid_evidence/reused_pgid_evidence.txt` |
| compile TERM、monitor record 写失败、summary 写失败 | 均以非零结果结束；summary 目标不可写时生成 `summary.failure` |

### 5.3 真实 eda01/VCS

- managed compile：独立 mode 编译、ACK/RC、stable log、fingerprint 和 seed 仿真均 PASS。
- compile-once：同一 fingerprint simv 使用 `--no-compile` 运行 seed `666670`、`666671`，`pass=2`。
- plusarg：上述真实运行均使用 `+MEMBLOCK_MAIN_TRANS_NUM=2`，输出目录记录 effective plusargs。
- wave/coverage：seed `666668` PASS，FSDB 为 1217922 bytes，生成 VCS coverage 数据库。
- coverage off：coverage-enabled simv 运行 seed `666669`、`666672` PASS，coverage manifest 前后差异为 0。持久证据为 `sim/.eda_regression/evidence_cov_off_20261020/coverage_manifest.{before,after}.tsv` 和 `coverage_manifest.diff`。
- 最终生命周期修订后冒烟：复用 `base_fun_regression_20261020` fingerprint，以 seed `666673`、`+MEMBLOCK_MAIN_TRANS_NUM=2` 执行真实 `--no-compile`，summary 为 `result=PASS`；stable final log 为 `UVM_ERROR=0`、`UVM_FATAL=0` 并正常 `$finish`。证据为 `sim/.eda_regression/real_post_lifecycle_fix_20261020/`。
- 真实 20 VCS 并发：未满足量化准入条件。`nproc=256`、load 约 8、`MemAvailable` 约 944 GiB、磁盘可用约 410 GiB，但 `lmstat` 不可用且缺少 `RSS_ONE` 基线，因此记录为容量未验证。证据为 `sim/.eda_regression/real20_capacity_precheck_20261020.txt`。

## 6. 文档和实现偏差

实现细节已写入关联 plan 的 `IMPLEMENTATION_DELTA`，包括 cfg 冻结 hash、eda01 权威 simv identity/hash、只基于 seed ACK/RC 纳秒区间重叠的 `peak_live`、`mv -fT`、严格 stale owner 校验、`EXIT_CONFIRMED` 与本地 finalization failure 分离、summary fallback 和 cleanup monitor trigger 竞态修复。真实 VCS 的 compile-once、plusarg、wave、coverage on/off 已完成；真实 20 路因量化准入条件不满足而保持容量未验证，未用 mock 结果替代真实容量结论。

## 7. 最终结论

三路独立 subagent 最终复审均明确回复“无必须修改项，可通过”。代码、mock、真实 EDA、文档和持久证据已闭环，plan 可以从 `undo` 移动到 `do` 并提交。剩余未验证项仅为真实 20 路 VCS 容量，原因和风险已按准入规则记录，不构成功能 blocker。
