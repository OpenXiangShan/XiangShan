# memblock 远端并发仿真脚本加固与 plusargs Plan Review

关联 plan：`AI_DOC/plan/test_framework/plan/do/memblock_parallel_eda_regression_hardening_plusargs_plan_20261019.md`
Review 日期：2026-10-19
最终结论：通过，可进入 coding

## 1. 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 对应落点 | 使用场景/示例 |
|---|---|---|---|
| `managed job` | 可持久恢复、探测和终止的远端编译或仿真任务 | token、ACK、RC、owner manifest | compile 与 seed 使用同一生命周期 |
| `remote phase` | 与仿真结果独立的远端生命周期 | LAUNCHING、OWNED、RC_SEEN、EXIT_CONFIRMED、REMOTE_UNKNOWN | 根因 FAIL 可同时 cleanup unknown |
| `framed RPC` | 带版本、请求 ID、数量、哈希和 END 的完整控制协议 | probe/signal SSH stdin/stdout | 截断请求不得发送 signal |
| `monitor commit` | 根因失败记录已完成持久提交的证明 | seed state、monitor_events、commit marker | 提交后才能打印错误并联停 |
| `stale owner` | controller 异常退出后遗留的远端任务身份 | `<mode>.owners.tsv` | 下一批取得锁后先审计 |
| `stable snapshot` | 通过远端前后 stat 和字节数校验的最终日志 | `status/<job>.final.log` | 防止 NFS 尾部延迟假 PASS |

抽象功能说明：本 review 检查正式 plan 能否可靠实现参数透传、编译一次、多 seed 并发、监控先提交后联停、远端进程恢复、coverage 开关和最终 PASS/FAIL；不评审 DUT 或 UVM testcase 功能正确性。

## 2. Review 范围

- 用户提出的全部脚本参数、默认值和新 plusarg 需求。
- `run_eda_regression.sh` 当前参数、monitor、probe/signal、分类和清理逻辑。
- Makefile、`project_cfg_vcs.mk` 的 cfg/plusargs、wave、coverage 和 simv 命令契约。
- 编译与仿真远端进程组的 ACK、RC、token、PGID 和异常恢复。
- 全 PASS、early-stop、`--no-record`、`--no-monitor` 和最终 summary 语义。
- 静态、mock、NFS/SSH/RPC 故障注入和真实 eda01 验证计划。

## 3. Review-Modify 过程

### 3.1 参数、plusargs 和 coverage

第一轮发现缺值选项可能死循环、前导零触发 Bash 八进制、cfg 缺失被静默忽略、用户 plusarg 无入口和 coverage off 仍收集。最终 plan 固定字符串十进制规范化、cfg fail-fast、`+KEY=VALUE` 白名单/保留 key/去重规则，并由 Makefile 单一 `EFFECTIVE_PLUSARGS` 生成最终参数。

coverage 编译期和运行期的所有 `-cm*` 均受 `ccov=on` 控制；off 验证使用数据库 manifest 前后差异，而非只观察目录是否存在。

### 3.2 结果状态和远端生命周期

第一轮使用单一 seed 状态，根因 FAIL 会在 cleanup 中被 ABORTED/REMOTE_UNKNOWN 覆盖。最终 plan 把 `RESULT_STATE`、`REMOTE_PHASE` 和 `SIGNAL_DISPOSITION` 分离；RC_SEEN 是可选观测，LAUNCHING/OWNED/RC_SEEN 均可直接进入 EXIT_CONFIRMED。

退出确认后必须无条件获取稳定远端日志，再按错误证据、完整 PASS/结束标记、RC 和 signal disposition 分类。monitor 根因 FAIL 始终保留。

### 3.3 ACK 前 SIGKILL 与 owner 恢复

第二轮发现远端任务可能在 ACK 前启动而未被 owner manifest 记录。最终 plan 要求 SSH launch 前先持久化 `LAUNCHING + PGID=UNKNOWN`。上一批未知 PGID owner 即使 token discovery 暂时为零，也不能证明旧 SSH 不会延迟创建进程，必须 quarantine 并拒绝 mode 复用。

owner 只有在 RC/log 恢复、结果分类和持久记录全部成功后才删除。compile 与 simulation 都遵守同一 owner 规则。

### 3.4 Framed RPC 和统一 cleanup

原实现通过共享 request 文件逐行执行远端 signal，响应缺项可能被部分消费。最终 plan 要求 remote helper 先完整缓存并验证 version、request ID、operation、count、hash、ITEM 集合和 END，全部通过后才允许 signal；controller 只消费完整匹配的 response。

最终 `cleanup_controller()` 使用单一幂等 trap：停止 watcher、接管 LAUNCHING job、probe、TERM、probe、KILL、probe、finalize、quarantine、owner 更新、summary，最后释放 mode lock。

### 3.5 Monitor 提交和 `--no-record`

根因事件固定按以下顺序：

```text
RESULT_STATE=FAIL
  -> seed state
  -> monitor_events.tsv
  -> record=on 时 progress/results
  -> monitor_failure.committed
  -> 打印错误
  -> STOP_REQUESTED
  -> 远端联停
```

`--no-record` 只关闭用户动态 progress/results 和成功/失败 seed 清单；monitor 安全审计记录和最终 summary 不可关闭。记录写失败进入 uncommitted + emergency cleanup，最终 result=FAIL。

### 3.6 验证闭环

最终验证矩阵补齐默认 UVM_LOW、显式 verbosity、debug 派生及覆盖、`--no-monitor`、timeout、非空 FSDB、plusarg 每 seed 一致性、coverage off manifest、ACK/RC/log NFS 延迟、RPC 截断、controller SIGKILL 和 compile INT/TERM。

真实 20 并发先测单 seed RSS/输出基线，并使用内存、磁盘、load、license token或 `2 -> 4 -> 8 -> 20` ramp、ACK 120 秒、总时长 30 分钟和 cleanup 60 秒作为固定门槛。资源不足只能记录容量未验证。

## 4. 用户需求覆盖检查

| 需求 | Review 结论 |
|---|---|
| testcase、cfg、次数、单/多 seed | 已覆盖，且非法 count 不因显式 seed 被跳过 |
| 默认并发 20 | 已覆盖，含 mock 峰值和真实资源门槛 |
| 首次编译一次、后续直接仿真 | 已覆盖，compile 也是 managed job |
| wave、coverage、verbosity、debug | 已覆盖默认值、派生、显式覆盖和真实产物 |
| runtime plusargs | 已覆盖安全格式、cfg override 和每 seed 透传 |
| monitor 默认开启和错误阈值 | 已覆盖，根因先 FAIL并提交记录后联停 |
| 全 PASS 输出 PASS | 已覆盖，summary 明确 `result=PASS` 且退出 0 |
| 中途失败先更新记录再报错 | 已覆盖，commit marker 成功后才打印错误和 stop |
| 记录开关 | 已覆盖，动态记录可关，安全审计和 summary 保留 |
| 异常退出和远端失联 | 已覆盖 owner、discovery、quarantine 和统一 cleanup |

## 5. 最终结论

四路独立 review 分别覆盖生命周期/恢复、参数/coverage、monitor/最终状态和验证/文档结构。最后一轮结论一致：

```text
无必须修改项，可 coding。
```

当前正式 plan 已闭合用户功能、远端身份、状态机、记录顺序、故障恢复和验证标准，可以作为本轮 coding 的执行依据。
