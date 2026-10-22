# mem_ut 远端并发仿真脚本使用说明

本文说明 `mem_ut` 的远端编译一次、多 seed 并发仿真脚本如何使用，以及发生
UVM 错误联停后如何区分根因失败 seed、被联停 seed 和未启动 seed。

主 agent 执行完整的回归监控、根因分析、Buglist、专项复测和恢复闭环时，还必须阅读：

```text
AI_DOC/project_management/mem_ut_parallel_regression_execution_rule.md
```

适用脚本：

```text
mem_ut/ver/ut/memblock/scripts/run_eda_regression.sh
```

适用场景：

- 在当前节点触发 `eda01` 上的 VCS 编译和仿真。
- 编译一次后复用同一 `simv` 执行多个 seed。
- 限制同时运行的 seed 数量。
- 临时传递 testcase runtime plusargs。
- 监控所有活动仿真日志，在 UVM 错误达到阈值后联停本批次。
- 汇总成功、根因失败、联停退出、基础设施失败和未启动 seed。

## 1. 执行位置与基本约束

从主仿真目录执行：

```bash
cd mem_ut/ver/ut/memblock/sim
```

查看脚本当前支持的参数：

```bash
../scripts/run_eda_regression.sh --help
```

执行前应满足：

1. 当前节点可以通过 `ssh eda01` 连接远端 EDA 节点；默认 host 不通时，脚本会尝试 `172.28.10.101`。
2. `../seq/plus_cfg/<cfg>.cfg` 必须存在且可读。
3. `--output-dir` 指定的目录必须为空；不指定时，脚本在 `.eda_regression/` 下自动创建带 run id 的目录。
4. 同一个 `mode` 同时只能有一个 controller 使用，避免不同批次覆盖同一 `simv` 或日志目录。
5. 不要在回归运行期间重新编译或替换同一 `mode` 的 `simv`。

## 2. 快速使用

### 2.1 编译一次并执行随机 seed

```bash
../scripts/run_eda_regression.sh \
  --tc tc_sanity \
  --cfg default \
  --mode base_fun_regression \
  --count 100 \
  --jobs 20 \
  --compile
```

该命令先执行一次编译，再使用 100 个自动生成且互不重复的 seed 仿真，最多同时运行 20 个。

### 2.2 复用已有 simv 执行多个指定 seed

```bash
../scripts/run_eda_regression.sh \
  --tc tc_sanity \
  --cfg default \
  --mode base_fun_regression \
  --seeds 666670,666671,666672 \
  --jobs 3 \
  --no-compile
```

显式 seed 的数量决定本批次仿真次数。此时即使同时传入 `--count`，`--count` 也不参与调度，但其参数值仍必须合法。

也可以重复使用 `--seed`：

```bash
../scripts/run_eda_regression.sh \
  --tc tc_sanity \
  --seed 666670 \
  --seed 666671 \
  --no-compile
```

### 2.3 临时传递 plusargs

```bash
../scripts/run_eda_regression.sh \
  --tc tc_sanity \
  --cfg default \
  --seed 666673 \
  --no-compile \
  --plusarg +MEMBLOCK_MAIN_TRANS_NUM=100 \
  --plusarg +MEMBLOCK_LOAD_TRANS_NUM=50
```

`--plusarg` 可以重复传入。用户 plusarg 与 cfg 中存在同名 key 时，用户传入值覆盖 cfg 值，最终只向仿真命令传递一次该 key。

### 2.4 Debug、波形和覆盖率

```bash
../scripts/run_eda_regression.sh \
  --tc tc_sanity \
  --cfg default \
  --seed 666674 \
  --mode base_fun_debug \
  --compile \
  --debug
```

`--debug` 在未显式指定相关选项时默认派生：

- `--wave`
- `--verbosity UVM_DEBUG`

显式传入 `--no-wave` 或 `--verbosity <level>` 时，以显式参数为准。

需要代码覆盖率时使用：

```bash
../scripts/run_eda_regression.sh \
  --tc tc_sanity \
  --mode base_fun_cov \
  --compile \
  --coverage
```

波形和 coverage 能力属于编译能力。`--no-compile --wave` 或
`--no-compile --coverage` 要求已有 fingerprint 中的 `simv` 具备对应能力，否则脚本会在仿真前失败。

### 2.5 只检查参数和配置

```bash
../scripts/run_eda_regression.sh \
  --tc tc_sanity \
  --cfg default \
  --count 20 \
  --jobs 4 \
  --plusarg +MEMBLOCK_MAIN_TRANS_NUM=10 \
  --dry-run
```

`--dry-run` 会完成本地参数、cfg 和 effective plusargs 检查并生成配置记录，不连接远端执行编译或仿真。

## 3. 参数说明

### 3.1 Testcase 与 seed

| 参数 | 默认值 | 说明 |
|---|---:|---|
| `--tc <name>` | 脚本无默认值 | testcase 名称，脚本命令行必填；主 agent 按回归执行规则在提示词未指定时传入 `basicTest`。 |
| `--cfg <name>` | `default` | `seq/plus_cfg/<name>.cfg` 的名称，不带 `.cfg`。 |
| `--ts <name>` | `virtual_base_sequence` | virtual sequence 名称。 |
| `--mode <name>` | `base_fun` | 编译和仿真产物使用的 mode。 |
| `--count <n>` | `1` | 未指定 seed 时自动生成的 seed 数量，范围 `1..100000`。 |
| `--seed <n>` | 无 | 指定一个 seed，可重复传入。 |
| `--seeds <list>` | 无 | 逗号或空格分隔的多个 seed。 |
| `--plusarg +KEY=VALUE` | 无 | runtime plusarg，可重复传入。 |

seed 范围为 `1..2147483646`。脚本会先将十进制 seed 规范化再去重，因此 `1` 和 `01` 只执行一次。

### 3.2 编译与仿真

| 参数 | 默认值 | 说明 |
|---|---:|---|
| `--jobs <n>` | `20` | 最大并发 seed 数，范围 `1..1000`。 |
| `--compile` | 关闭 | 先执行一次 managed compile，再执行 seed 仿真。 |
| `--no-compile` | 开启 | 不编译，直接复用当前 mode 的 `simv`。 |
| `--wave` / `--no-wave` | 关闭 | 开启或关闭 FSDB 波形。 |
| `--coverage` / `--no-coverage` | 关闭 | 开启或关闭 VCS 代码覆盖率。 |
| `--verbosity <level>` | `UVM_LOW` | 支持 `UVM_NONE/LOW/MEDIUM/HIGH/FULL/DEBUG`。 |
| `--debug` | 关闭 | 默认派生 wave on 和 `UVM_DEBUG`。 |
| `--timeout-ns <n>` | `100000000` | UVM timeout，单位 ns。 |

### 3.3 监控与记录

| 参数 | 默认值 | 说明 |
|---|---:|---|
| `--monitor` / `--no-monitor` | 开启 | 是否监控活动仿真日志并执行错误联停。 |
| `--error-limit <n>` | `1` | 单个 seed 的 `UVM_ERROR + UVM_FATAL` 触发阈值。 |
| `--record` / `--no-record` | 开启 | 是否输出动态进度和每 seed 汇总记录。 |

`--no-monitor` 只关闭提前联停。seed 结束后仍会根据最终日志中的 UVM 错误判定为 `FAIL`。

`--no-record` 只关闭 `progress.log`、`results.tsv`、成功/失败/未启动 seed 清单等动态记录；安全审计文件、每 seed state 和最终 `summary.log` 仍保留。

### 3.4 远端控制

| 参数 | 默认值 | 说明 |
|---|---:|---|
| `--remote-host <host>` | `eda01` | 远端 EDA 节点。 |
| `--output-dir <path>` | 自动生成 | 结果目录，必须为空且本地与远端共享可见。 |
| `--poll-interval <sec>` | `1` | controller 主循环轮询间隔。 |
| `--launch-timeout <sec>` | `30` | 等待 worker ACK 的超时。 |
| `--probe-interval <sec>` | `5` | 远端进程组存活探测间隔。 |
| `--ssh-retries <n>` | `3` | 远端操作失败后的重试次数。 |
| `--dry-run` | 关闭 | 只做本地检查，不执行远端操作。 |

## 4. Plusargs 规则

每个 plusarg 必须使用以下形式：

```text
+KEY=VALUE
```

规则如下：

1. key 必须以字母或下划线开头，只能包含字母、数字和下划线。
2. value 不能为空，只允许字母、数字、`_ . / , : @ % + -`。
3. 同一次命令不能重复传入相同 key。
4. 以下 key 由正式脚本参数控制，不能通过 `--plusarg` 传递：
   - `ntb_random_seed`
   - `UVM_TESTNAME`
   - `UVM_VERBOSITY`
   - `UVM_TIMEOUT`
   - `TEST_MODE`
   - `VSEQ_MAIN`
   - `wave_type`
   - `gen_wave`
5. runtime plusargs 只传给 seed 仿真，不传给 compile job。
6. 实际生效列表记录在结果目录的 `effective_plusargs.txt`；用户直接输入列表记录在 `user_plusargs.txt`。

## 5. 编译一次和 simv 复用

### 5.1 首次执行

首次为某个 mode 建立 `simv` 时使用 `--compile`：

```bash
../scripts/run_eda_regression.sh \
  --tc tc_sanity \
  --mode base_fun_regression \
  --seed 1001 \
  --compile
```

compile 作为 managed job 执行，只运行一次。编译成功后脚本记录：

```text
<mode>/exec/.eda_compile_fingerprint
```

fingerprint 包含 wave、coverage、`simv` hash 和文件身份。

### 5.2 后续复用

后续使用同一 mode 时传入 `--no-compile`：

```bash
../scripts/run_eda_regression.sh \
  --tc tc_sanity \
  --mode base_fun_regression \
  --count 100 \
  --jobs 20 \
  --no-compile
```

脚本在启动 seed 前验证 `simv` 是否存在，以及 fingerprint 是否满足本次 wave/coverage 请求。运行期间如果同一 mode 的 `simv` 身份变化，批次结果为 `FAIL`。

## 6. UVM 错误监控与全部联停

监控默认开启。每个已启动 seed 都有独立 watcher 增量读取仿真日志，匹配：

```text
UVM_ERROR @ ...
UVM_FATAL @ ...
```

当任一 seed 的累计数量达到 `--error-limit` 后，流程为：

```text
根因 seed -> RESULT_STATE=FAIL
          -> 写 status/<seed>.state
          -> 写 monitor_events.tsv
          -> record 开启时刷新 progress/results
          -> 写 monitor_failure.committed
          -> 打印根因错误
          -> 停止继续启动新 seed
          -> TERM/KILL 本批次仍活动的远端进程组
          -> 获取各 seed 稳定最终日志并分类
```

脚本保证正常联停前先提交根因记录。可通过以下时间戳审计顺序：

- `monitor_failure.committed` 中的 `committed_epoch_ns`
- `signal_term.sent` 或 `signal_kill.sent` 中的 `sent_epoch_ns`

正常情况下应满足：

```text
committed_epoch_ns < sent_epoch_ns
```

## 7. 如何判断哪个 seed 失败、哪些 seed 被联停

### 7.1 根因失败 seed

查看：

```bash
cat <result_dir>/monitor_events.tsv
```

格式：

```text
seed    count    triggered_at    line
1021    1        ...             UVM_ERROR @ 100: ...
```

该文件记录达到阈值并触发联停的根因 seed、错误数量、触发时间和首条错误行。testcase、cfg 和 mode 可从同目录的 `summary.log` 或 `config.txt` 获取。

### 7.2 被联停 seed

默认 `--record` 开启时查看：

```bash
column -t -s $'\t' <result_dir>/results.tsv
```

主要字段：

| 字段 | 含义 |
|---|---|
| `seed` | seed。 |
| `result_state` | `PASS/FAIL/INFRA/INCOMPLETE/ABORTED/PENDING`。 |
| `remote_phase` | 远端生命周期。正常收敛后通常为 `EXIT_CONFIRMED`。 |
| `rc` | 远端 worker 返回码。 |
| `uvm_count` | 最终稳定日志中的 UVM error/fatal 数量。 |
| `log` | controller 保存的稳定最终日志路径。 |
| `reason` | 最终分类原因。 |

联停时常见结果：

| 状态 | 含义 |
|---|---|
| `FAIL` | 根因 seed，或最终日志自身包含 UVM 错误/worker 非零退出。 |
| `ABORTED` | 因其它 seed 触发联停而收到 TERM/KILL，且没有形成完整 PASS 日志。 |
| `PASS` | 虽然收到联停信号，但信号到达前已经形成完整 PASS 日志。 |
| `PENDING` | 尚未启动。最终会进入 `not_run_seeds.txt`。 |
| `INFRA` | 日志、license、磁盘、simv 或远端控制等基础设施错误。 |
| `INCOMPLETE` | 进程结束但日志既没有完整 PASS，也没有明确错误证据。 |

因此不能仅凭“收到 TERM”认定某个 seed 为 `ABORTED`，应以 `results.tsv` 的最终分类为准。

### 7.3 信号发送记录

查看哪些 job 收到联停信号：

```bash
cat <result_dir>/signal_term.sent
cat <result_dir>/signal_kill.sent
```

示例：

```text
operation=SIGNAL_TERM
job=seed:1021 status=SIGNALED
job=seed:1022 status=SIGNALED
```

信号文件记录远端控制动作，不代表最终业务分类。最终仍以 `results.tsv` 和 `status/<seed>.state` 为准。

### 7.4 失败和未启动清单

默认 `--record` 开启时：

- `failed_seeds.txt`：包含 `FAIL`、`INFRA`、`INCOMPLETE`、`ABORTED`，不区分根因与被联停。
- `success_seeds.txt`：最终为 `PASS` 的 seed。
- `not_run_seeds.txt`：联停或异常结束时仍未启动的 seed。
- `status/<seed>.state`：单个 seed 的最终 state、reason 和更新时间。

要区分根因失败和联停退出，组合读取：

```text
monitor_events.tsv -> 根因 seed
results.tsv        -> 全部 seed 最终分类
signal_*.sent      -> 实际发送信号的 job
not_run_seeds.txt  -> 尚未启动的 seed
```

### 7.5 `--no-record` 模式

使用 `--no-record` 时不生成 `results.tsv` 和成功/失败/未启动清单，但仍保留：

- `monitor_events.tsv`，仅发生 monitor trigger 时生成。
- `monitor_failure.committed` 或 `monitor_failure.uncommitted`，仅发生 monitor trigger 时生成。
- `signal_term.sent` / `signal_kill.sent`，仅执行对应联停动作时生成。
- `status/<seed>.state`
- `status/seed_<seed>.final.log`
- `summary.log`

此模式下可以通过 `monitor_events.tsv` 找根因，通过各 `status/<seed>.state` 判断其余 seed 是 `PASS`、`ABORTED`、`FAIL` 或其它状态。

## 8. 结果目录说明

默认结果目录：

```text
mem_ut/ver/ut/memblock/sim/.eda_regression/<run_id>/
```

常用文件：

| 文件 | 作用 | `--no-record` 时保留 |
|---|---|---|
| `config.txt` | testcase、cfg、mode、开关和路径配置。 | 是 |
| `seeds.txt` | 本批次计划 seed。 | 是 |
| `user_plusargs.txt` | 用户传入的 plusargs。 | 是 |
| `effective_plusargs.txt` | cfg 合并后实际生效的 plusargs。 | 是 |
| `summary.log` | 本批次最终 PASS/FAIL 和计数汇总。 | 是 |
| `monitor_events.tsv` | 达到阈值的根因 seed 事件；仅触发 monitor 时生成。 | 是，按需生成 |
| `monitor_failure.committed` | 根因记录已在联停前提交；仅触发 monitor 时生成。 | 是，按需生成 |
| `signal_term.sent` | TERM 请求和每 job 响应；仅发送 TERM 时生成。 | 是，按需生成 |
| `signal_kill.sent` | KILL 请求和每 job 响应；仅发送 KILL 时生成。 | 是，按需生成 |
| `status/<seed>.state` | 每 seed 状态和原因。 | 是 |
| `status/seed_<seed>.final.log` | 稳定最终仿真日志快照。 | 是 |
| `progress.log` | 动态和最终进度。 | 否 |
| `results.tsv` | 每 seed 最终结果表。 | 否 |
| `success_seeds.txt` | PASS seed 清单。 | 否 |
| `failed_seeds.txt` | 非 PASS 已结算 seed 清单。 | 否 |
| `not_run_seeds.txt` | 未启动 seed 清单。 | 否 |

`summary.log` 主要字段：

| 字段 | 含义 |
|---|---|
| `result` | 整批结果，`PASS` 或 `FAIL`。 |
| `planned` | 计划 seed 数。 |
| `completed` | `PASS + FAIL` 数量。 |
| `settled` | 已归类为 `PASS/FAIL/INFRA/INCOMPLETE/ABORTED` 的数量。 |
| `remaining` | `planned - completed`，包含非 PASS 的其它终态，因此不等于未运行数。 |
| `not_run` | 最终仍为 `PENDING/PENDING` 的未启动 seed 数。 |
| `pass/fail/infra/incomplete/aborted/pending` | 各结果分类数量。 |
| `remote_unknown_jobs` | 无法确认远端生命周期的 job 数。 |
| `peak_active` | controller 调度到的最大活动 seed 数。 |
| `peak_live` | 根据 seed ACK/RC 纳秒区间计算的真实最大远端存活重叠数。 |
| `monitor_commit` | `NONE/COMMITTED/FAILED`。 |
| `stop_reason` | 批次停止原因。 |

只有全部 seed 最终为 `PASS`，且没有 active/unknown owner、quarantine、记录故障或 `simv` 变化时，整批 `result` 才为 `PASS`。

## 9. 常用结果检查命令

查看整批结论：

```bash
grep -E '^(result|planned|pass|fail|infra|incomplete|aborted|pending|not_run|stop_reason)=' \
  <result_dir>/summary.log
```

查看根因 seed：

```bash
column -t -s $'\t' <result_dir>/monitor_events.tsv
```

查看全部 seed 分类：

```bash
column -t -s $'\t' <result_dir>/results.tsv
```

查看被联停 seed：

```bash
awk -F '\t' 'NR == 1 || $2 == "ABORTED"' <result_dir>/results.tsv
```

查看最终失败或基础设施异常：

```bash
awk -F '\t' 'NR == 1 || $2 ~ /^(FAIL|INFRA|INCOMPLETE)$/' <result_dir>/results.tsv
```

查看单个 seed：

```bash
cat <result_dir>/status/<seed>.state
less <result_dir>/status/seed_<seed>.final.log
```

## 10. 回归恢复与专项波形复现

### 10.1 根据上一轮状态恢复

脚本没有 `--resume` 参数，且 `--output-dir` 必须为空，因此不能在原结果目录内原地续跑。恢复时必须
等待上一轮脚本完全退出、状态文件提交且 mode lock 释放，然后创建新的结果目录和显式 seed 批次。

恢复 seed 按以下规则处理：

- `ABORTED` 必须使用原 seed 重新执行。
- `not_run_seeds.txt` 中的原 seed 尚未执行，可以废弃，并重新构造相同数量的新随机 seed。
- 新 seed 不得与历史已执行 seed、保留的 `ABORTED` seed 或其它新 seed 重复。
- `FAIL` 根因 seed 必须先使用原 seed 完成修复专项复测。
- `INFRA/INCOMPLETE` 在问题排除后原则上使用原 seed 重跑。
- testcase、cfg、virtual sequence、用户 plusargs、timeout 和计划执行总数必须保持一致。

不得直接使用 `failed_seeds.txt` 作为恢复列表，因为该文件同时包含 `FAIL`、`INFRA`、`INCOMPLETE`
和 `ABORTED`。恢复前应分别从 `results.tsv` 和 `not_run_seeds.txt` 取得分类，并在外部构造最终 seed
列表，再通过 `--seeds` 显式传入：

```bash
../scripts/run_eda_regression.sh \
  --tc <原tc> \
  --cfg <原cfg> \
  --ts <原ts> \
  --mode <原mode> \
  --seeds <原ABORTED和新构造seed列表> \
  --no-compile \
  <原用户plusargs>
```

### 10.2 使用原 seed 收集波形

原回归没有波形时，可以在上一轮完全退出后复用原 mode，使用首错候选原 seed 重新编译并开启 debug：

```bash
../scripts/run_eda_regression.sh \
  --tc <原tc> \
  --cfg <原cfg> \
  --ts <原ts> \
  --mode <原mode> \
  --seed <首错候选seed> \
  --jobs 1 \
  --compile \
  --debug \
  <原用户plusargs>
```

该命令会覆盖原 mode 的编译产物。执行前必须保留原结果目录和 fingerprint 信息，并确认原 controller
退出、mode lock 释放且 `remote_unknown_jobs=0`。需要保留原 `simv` 时可改用独立 debug mode。

波形复现没有出现原始错误时，只能保留为待确认分析，不得依据不同现象直接修改代码。

## 11. 退出码

| 退出码 | 含义 |
|---:|---|
| `0` | dry-run 成功，或整批仿真最终 `PASS`。 |
| `1` | 仿真批次最终 `FAIL`，包括 testcase 失败、联停、基础设施结果或记录故障。 |
| `2` | 命令行、cfg、seed、plusarg、输出目录等本地输入错误。 |
| `3` | 远端连接、共享目录证明、bootstrap 或初始记录基础设施失败。 |
| `4` | mode lock 被占用，或 stale owner/quarantine 审计失败。 |
| `5` | compile、simv 存在性或 fingerprint 能力检查失败。 |

## 12. 常见问题

### 12.1 提示 mode 正在使用

说明同一 mode 已有 controller，或上一批留下 owner。先检查：

```bash
ls -l .eda_regression/locks/<mode>.*
```

不要直接删除 active owner 或 quarantine。应先确认远端 job 已退出并分析 owner/quarantine 内容，避免误放行仍在运行的仿真。

### 12.2 `simv` 缺失或 fingerprint 不匹配

使用 `--compile` 重新建立该 mode，或改用与已有 `simv` wave/coverage 能力一致的参数。不要在不同 wave/coverage 配置之间无条件复用同一个 mode。

### 12.3 cfg 不存在

确认文件存在：

```bash
ls ../seq/plus_cfg/<cfg>.cfg
```

### 12.4 根因 seed 是 FAIL，但其它 seed 没有全部显示 ABORTED

这是允许的。信号到达前已经完整结束的 seed 可为 `PASS`；尚未启动的 seed 为 `PENDING` 并进入 `not_run_seeds.txt`；只有收到联停且没有完整 PASS 日志的 seed 才为 `ABORTED`。

### 12.5 只想看失败，不想提前联停

使用：

```bash
--no-monitor
```

各 seed 会继续完成，最终根据各自稳定日志分类。

## 13. 维护要求

后续修改 `run_eda_regression.sh` 的公开参数、默认值、结果字段、监控联停顺序、结果分类或输出文件时，必须同步更新本文档，并至少执行：

```bash
bash -n mem_ut/ver/ut/memblock/scripts/run_eda_regression.sh
mem_ut/ver/ut/memblock/scripts/run_eda_regression.sh --help
git diff --check -- AI_DOC/project_management/mem_ut_parallel_eda_regression_usage_rule.md AGENTS.md
```
