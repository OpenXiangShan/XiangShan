# memblock 远端并发仿真脚本 Plan Review

关联 plan：`AI_DOC/plan/test_framework/plan/do/memblock_parallel_eda_regression_script_plan_20261019.md`
Review 日期：2026-10-19
最终结论：通过，可进入 coding

## 1. 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 对应落点 | 示例 |
|---|---|---|---|
| `early stop` | 单个 seed 的 UVM error/fatal 达到阈值后停止整个批次 | controller、monitor watcher | 阈值为 1 时首个错误联停 |
| `run token` | 标识单个远端 worker 及其进程组的随机身份串 | ACK、进程环境、probe/kill | 防止 PID 重用导致误杀 |
| `compile fingerprint` | simv 编译配置和文件身份记录 | `${mode}/exec/.eda_compile_fingerprint` | 验证 coverage/wave 能力 |
| `mode quarantine` | 远端进程无法确认清理时对 mode 的持久禁用标记 | `.eda_regression/locks/<mode>.quarantine` | 人工确认前禁止复用 mode |

抽象功能说明：本 review 检查 plan 是否能在不修改 UVM/RTL 行为的前提下，可靠完成远端编译一次、多 seed 并发仿真、错误联停、状态记录和资源清理；不评审 DUT 功能正确性。

## 2. Review 范围

本次 review 对照以下内容：

- 用户提出的全部脚本参数和默认行为。
- `mem_ut/ver/ut/memblock/sim/Makefile`。
- `mem_ut/ver/ut/memblock/sim/remote_eda_make.sh`。
- `mem_ut/ver/ut/memblock/sim/eda01_entry.sh`。
- `project_cfg.mk` 和 `project_cfg_vcs.mk` 的 compile/batch_run、wave、coverage、seed、verbosity 和日志命名契约。
- 测试框架 plan、逻辑构建和中文文档规则。

## 3. Review 过程与修正结论

### 3.1 远端环境和编译契约

第一轮发现 remote worker 直接调用 `eda01_entry.sh` 会绕过 EDA bootstrap。最终方案新增 Makefile 只读目标 `eda_print_bootstrap`，脚本从现有 Makefile 获取唯一 bootstrap，不复制 module load 字符串。

coverage 和 wave 依赖编译期能力。最终方案使用 compile fingerprint 记录配置、simv SHA-256 和文件身份；`--no-compile` 请求 coverage/wave 时必须匹配，否则 fail-fast。

### 3.2 并发隔离和进程组清理

初版只保存 PID，无法防止 PID 重用或组长异常退出。最终方案使用 `setsid`、ACK、PID/PGID/SID 和 run token；token 在 `setsid` 前进入初始环境并由 make/simv 继承。probe 和 kill 通过 PGID 成员环境中的 token 确认身份。

同一 mode 使用 `flock -n` 非阻塞排他锁。远端状态无法确认时创建持久 quarantine，禁止新批次覆盖可能仍在使用的 simv 或 coverage 数据。

### 3.3 Early-stop 竞态

最终主循环固定按以下顺序执行：

1. 先检查已有 trigger。
2. 回收已完成 worker；monitor 开启时同步复核最终日志错误数。
3. 批量 probe 远端 worker。
4. 再次检查 stop 条件并联停。
5. 仅在没有 stop 请求时逐个补充任务，每次 launch 前再次检查 trigger。

该顺序避免快速失败尚未写 trigger、批量补任务期间到达错误、或 early stop 后因 PENDING 项无法退出的问题。`--no-monitor` 时失败 seed 只分类为失败，不触发全局联停。

### 3.4 Launch 不确定状态

Launch SSH 不自动重发。SSH 返回失败时仍等待同 token ACK；ACK 超时后按 token 远端发现：唯一 PGID则接管或终止，确认无 PGID后记 `INFRA`，无法确认时记 `REMOTE_UNKNOWN` 并 quarantine。

### 3.5 进度和记录语义

最终方案区分：

```text
completed = PASS + FAIL
settled   = PASS + FAIL + INFRA + INCOMPLETE + ABORTED + REMOTE_UNKNOWN
remaining = planned - completed
```

`--no-record` 只关闭面向用户的动态进度文件，内部 ACK、PID、seed、状态和最小 summary 始终保留，以保证清理和问题定位。

## 4. 用户需求覆盖检查

| 需求 | Plan 结论 |
|---|---|
| testcase、cfg、次数、单/多 seed | 已覆盖，显式 seed 模式忽略 count |
| 默认并发 20 | 已覆盖 |
| 可选首次编译，后续不编译运行 | 已覆盖，使用 `eda_compile` 后只执行 `batch_run` |
| FSDB、代码覆盖率、verbosity | 已覆盖，默认关闭波形/覆盖率、`UVM_LOW` |
| monitor 默认开启和错误阈值 | 已覆盖，默认阈值 1 |
| 动态完成/剩余/成功/失败记录 | 已覆盖，默认开启 |
| debug 默认 FSDB + UVM_DEBUG | 已覆盖，显式参数可覆盖 |
| 任一 seed 达阈值后全部结束 | 已覆盖，按本批次 token/PGID 精确联停 |
| 参数说明和用法注释 | 已列入脚本验收标准 |

## 5. 性能与风险检查

- 日志 watcher 使用 `tail -n +1 --retry -F` 增量消费，不周期性全量扫描大日志。
- active worker 通过一次批量 SSH probe，避免每 seed 高频独立 SSH。
- 状态单向推进，所有用户记录使用临时文件加 `mv` 原子发布。
- 剩余风险是远端完全不可达时无法保证进程已结束；方案不掩盖该风险，而是使用 `REMOTE_UNKNOWN + quarantine` 阻止危险复用。

## 6. 最终结论

经过多轮 review-modify 循环，最后一轮独立 review 结论为：

```text
无必须修改项，可 coding。
```

当前 plan 已覆盖功能、并发、错误联停、远端身份校验、编译契约、记录语义和验证闭环，可以作为本轮 coding 的执行依据。
