# memblock 远端并发仿真脚本加固与 plusargs 支持正式 Plan

状态：已完成；coding、mock/真实 EDA 验证和最终 implementation review 均通过
创建日期：2026-10-19
适用版本：V2 `mem_ut_uvm_v2`
关联实现：`mem_ut/ver/ut/memblock/sim/run_eda_regression.sh`
关联旧 plan：`AI_DOC/plan/test_framework/plan/do/memblock_parallel_eda_regression_script_plan_20261019.md`

## 1. Plan 定位、专有名词与抽象功能说明

本 plan 修复远端并发仿真脚本在参数校验、plusargs、监控提交顺序、远端进程生命周期、NFS 最终日志、coverage 开关和异常恢复方面的问题。修改范围仅包含仿真控制脚本、Makefile 的有效 plusargs 查询入口、VCS coverage 条件和对应文档；不修改 DUT、UVM sequence、driver、monitor、RM 或 scoreboard 行为。

### 1.1 专有名词

| 英文术语 | 当前文档中的中文含义 | 对应代码对象或落点 | 使用场景/示例 |
|---|---|---|---|
| `controller` | 当前节点运行的回归总控进程 | `run_eda_regression.sh` 普通入口 | 维护状态、并发和最终结果 |
| `managed job` | controller 必须持久记录并能探测、终止和恢复的远端任务 | compile job 和每个 seed job | 编译也不能作为普通同步命令处理 |
| `job id` | managed job 的稳定名称 | `compile` 或 `seed:<seed>` | `seed:666668` |
| `run token` | 一次远端任务实例的唯一身份 | owner manifest、ACK、RC、RPC | 防止 PID/PGID 复用误杀 |
| `PGID` | 远端 worker 及其子进程所属进程组 | ACK、owner manifest、probe/signal | TERM/KILL 作用于完整进程组 |
| `result state` | seed 的仿真业务结果 | `RESULT_STATE[$job]` | PASS、FAIL、INFRA、INCOMPLETE、ABORTED |
| `remote phase` | managed job 的远端生命周期 | `REMOTE_PHASE[$job]` | OWNED、RC_SEEN、EXIT_CONFIRMED、REMOTE_UNKNOWN |
| `local finalization failure` | 远端已确认退出后，本地结果记录、finalized marker、fingerprint 或 owner 删除提交失败 | `FINALIZATION_FAILURE[$job]`、`RECORD_INFRA_FAILURE` | 保留 `EXIT_CONFIRMED`，批次返回 FAIL |
| `signal disposition` | 该 job 是否真正收到成功的终止信号 | `SIGNAL_DISPOSITION[$job]` | NEVER、TERM_SENT、KILL_SENT |
| `monitor trigger` | watcher 达到 UVM 错误阈值后发布的原子事件 | `monitor/<seed>.trigger` | 保存 count、错误行和时间 |
| `monitor commit` | controller 已把根因 FAIL 及相关记录持久化完成的证明 | `monitor_failure.committed` | 只有提交成功后才正常联停 |
| `framed RPC` | 带版本、请求 ID、条目数、哈希和结束标记的远端控制协议 | probe/signal 的 SSH stdin/stdout | 截断或重复响应必须被拒绝 |
| `owner manifest` | 当前 mode 全部 active managed job 的持久身份清单 | `.eda_regression/locks/<mode>.owners.tsv` | controller 被 SIGKILL 后供下一批审计 |
| `quarantine` | 无法证明远端 job 已退出时禁止 mode 复用的全量记录 | `.eda_regression/locks/<mode>.quarantine.tsv` | 保留 compile 和全部未知 seed |
| `stable snapshot` | 远端退出确认后，经过前后 stat 一致性校验的完整日志副本 | `status/<job>.final.log` | 防止 NFS 尾部延迟造成假 PASS |
| `effective plusargs` | cfg 参数与用户 plusargs 按实际 Make 规则合成后的最终参数 | `eda_print_effective_plusargs` | dry-run 和执行使用同一权威结果 |
| `compile fingerprint` | simv 能力和文件身份记录 | `${mode}/exec/.eda_compile_fingerprint` | no-compile 前校验 wave/coverage 能力 |

### 1.2 关键函数抽象功能描述

`require_option_value()`：参数解析读取 `$2` 前确认值存在、非空且不是另一个长选项；它不负责值域校验。

`normalize_bounded_decimal()`：以字符串方式去除前导零并检查十进制位数、下界和上界；它不对未校验原始输入执行 Bash 算术。

`validate_plusarg()`：验证单个用户 `+KEY=VALUE` 的格式、字符白名单、保留 key 和重复 key；它不解析 SystemVerilog 参数的业务语义。

`write_owner_manifest()`：从 controller 内存中的 active managed job 集合原子重写完整 owner manifest；它不根据 RC 文件自行删除 owner。

`run_remote_rpc()`：构造并发送完整 framed RPC，验证响应 request ID、条目集合、数量、哈希和结束标记；它不在响应不完整时更新远端状态。

`handle_monitor_triggers()`：把 trigger 转换为根因 seed 的权威 FAIL，无条件写入 seed state、monitor event 和 commit marker，并在 record 开启时写 progress/results，全部必需记录成功后才请求正常联停；记录失败时进入显式基础设施故障紧急清理分支。

`fetch_stable_remote_log()`：在远端退出确认后获取稳定的完整日志快照并原子发布；它不使用现有本地 NFS 日志作为完整性证明。

`finalize_exited_job()`：只在 `REMOTE_PHASE=EXIT_CONFIRMED` 后恢复 RC、取得稳定日志并收敛最终结果；本地记录失败只设置独立 finalization failure 并保留 `EXIT_CONFIRMED`，它不把本地落盘故障伪装成远端生命周期未知。

`compute_batch_result()`：在全部 job 完成 finalization、无 active/unknown owner 且 simv 身份未变化后计算 PASS/FAIL；它不负责写 summary。

`cleanup_controller()`：在正常退出、INT、TERM、记录故障或其它异常路径中，以同一幂等流程停止 launch、接管未完成 job、执行 probe/TERM/KILL/finalize 并最后释放 mode lock；它不把无法确认的远端 job 当作已清理。

## 2. 目标功能 Flow 总览

```text
解析全部 CLI
  -> 带值参数完整性检查
  -> 所有显式数值参数统一十进制规范化和值域检查
  -> cfg、plusarg、output-dir 本地校验
  -> 通过 Makefile 获取 effective plusargs
  -> 生成 seeds/config/user_plusargs/effective_plusargs
  -> dry-run：记录 shared_path_validation=DEFERRED 后退出
  -> 连接 eda01
  -> 本地/远端双向 nonce 证明 output-dir 为同一共享目录
  -> 获取 mode lock
  -> 审计上一批 stale owner manifest
       -> EXIT_CONFIRMED：不再 probe PGID，直接清理 stale owner
       -> 仍 active 或 SSH 不可达：写 quarantine 并拒绝复用
       -> 全部已退出：清理 stale owner 后继续
  -> 可选 compile managed job
       -> ACK/owner/probe/RC/exit-confirmation/fingerprint
  -> 调度最多 jobs 个 seed managed job
  -> watcher 增量监控 UVM_ERROR/UVM_FATAL
       -> 达阈值：发布 trigger
  -> controller 提交 trigger
       -> 根因 RESULT_STATE=FAIL
       -> 强制写 seed state、monitor event、commit marker；record=on 时写 progress/results
       -> 设置 STOP_REQUESTED
  -> 对仍 active job：先 probe 自然完成，再 TERM/probe/KILL/probe
  -> EXIT_CONFIRMED job 无条件取得 stable remote log
  -> 按 result state、signal disposition、RC、日志完成 finalization
  -> 无 active/unknown owner且全部 seed PASS：summary result=PASS，退出 0
  -> 任一非 PASS、remote unknown、simv 改变或记录故障：summary result=FAIL，退出非零
```

## 3. 参数与配置 Flow

### 3.1 带值选项和十进制参数

所有带值选项先调用 `require_option_value <option> "${2-}"`。以下情况返回 2，且不得进入 SSH、编译或仿真：

1. `$2` 不存在。
2. `$2` 为空字符串。
3. `$2` 匹配 `--*`，表示用户漏传当前选项值。

所有显式提供的数值都必须先校验，即使显式 seed 最终使 `--count` 不参与调度，也不能接受非法 count。规范化前只执行字符串正则和长度检查，禁止对未受限原始字符串执行 `(( raw ))`。

| 参数 | 范围 |
|---|---|
| count | 1..100000 |
| jobs | 1..1000 |
| error-limit | 1..1000000 |
| timeout-ns | 1..999999999999999999 |
| poll-interval | 1..3600 |
| launch-timeout | 1..3600 |
| probe-interval | 1..3600 |
| ssh-retries | 1..100 |
| seed | 1..2147483646 |

`08` 规范化为 `8`。seed 规范化后再去重，因此 `1` 与 `01` 只执行一次。显式 seed 模式仅在全部参数合法后打印 count ignored warning，并以 seed 数量作为 planned 数量。

### 3.2 cfg 存在性

`--cfg <name>` 必须映射到可读文件：

```text
mem_ut/ver/ut/memblock/seq/plus_cfg/<name>.cfg
```

不存在、不可读或传入 `.cfg` 后缀导致映射失败时返回 2。脚本不得继续使用空 cfg。

### 3.3 plusargs

新增可重复参数：

```bash
--plusarg +MEMBLOCK_MAIN_TRANS_NUM=1000
--plusarg +MEMBLOCK_ENQ_PER_CYCLE=2
```

规则固定如下：

1. token 必须精确匹配 `+KEY=VALUE`，只允许一个 `=`。
2. KEY 匹配 `[A-Za-z_][A-Za-z0-9_]*`。
3. VALUE 匹配 `[A-Za-z0-9_./,:@%+-]+`，不得为空。
4. 因白名单约束，空白、引号、反引号、`$`、括号、分号、管道、重定向符、反斜杠和换行全部拒绝。
5. 重复 KEY 返回 2。
6. 保留 KEY 返回 2：`ntb_random_seed`、`UVM_TESTNAME`、`UVM_VERBOSITY`、`UVM_TIMEOUT`、`TEST_MODE`、`VSEQ_MAIN`、`wave_type`、`gen_wave`。
7. 合法 token 以独立 argv 传给 remote worker；remote worker 重新执行同样校验后再组装 Make assignment。
8. plusargs 只影响 `batch_run`，不进入 compile。

Makefile 定义唯一 `EFFECTIVE_PLUSARGS`：先读取 cfg，过滤被用户 plusarg 覆盖的同名 key，再追加用户 plusarg。新增只读目标 `eda_print_effective_plusargs`，脚本不得复制 awk 合并规则。dry-run 和正式执行均写：

```text
user_plusargs.txt
effective_plusargs.txt
```

### 3.4 coverage 开关

`project_cfg_vcs.mk` 将 coverage 分为编译期和运行期条件：

```text
ccov=on：
  CMP_OPTIONS 加 COVER_OPTIONS/COVER_TYPE/COVER_DIR
  SIMV_OPTIONS 加 COVER_OPTIONS/COVER_DIR/COV_CM_NAME

ccov=off：
  CMP_OPTIONS 和 SIMV_OPTIONS 均不含任何 -cm、-cm_dir、-cm_name
```

coverage-enabled simv 允许 runtime `ccov=off`；此时不得生成新的 coverage testdata。请求 `--coverage` 时 fingerprint 必须证明 simv 以 `ccov=on` 编译。

### 3.5 output-dir 和共享路径证明

本地先检查：已存在但不是目录、目录非空、创建失败、不可写均返回 2。非 dry-run 连接远端后执行双向 nonce：

1. local 在 output-dir 原子写随机 nonce A。
2. remote 从同一绝对路径读取 A，内容不匹配则失败。
3. remote 原子写包含 A 和随机 nonce B 的 response。
4. local 从同一路径读取并验证 A、B。
5. local 原子写 B 的确认文件，remote 再读取验证。
6. 任一步失败返回 3，不启动 compile 或 seed job。

dry-run 不建立 SSH 连接，`config.txt` 明确记录 `shared_path_validation=DEFERRED`。

## 4. 状态模型与持久恢复 Flow

### 4.1 两套正交状态

业务结果和远端生命周期不得复用同一变量。

`RESULT_STATE[job]`：

```text
PENDING -> PASS | FAIL | INFRA | INCOMPLETE | ABORTED
```

monitor 根因一旦进入 FAIL，后续 cleanup 失败不能覆盖该结果。cleanup 不确定性由 `REMOTE_PHASE` 和 quarantine 单独表达。

`REMOTE_PHASE[job]`：

```text
PENDING -> LAUNCHING -> OWNED -> RC_SEEN
             |            |          |
             +------------+----------+-> EXIT_CONFIRMED
             |            |          |
             +------------+----------+-> REMOTE_UNKNOWN
```

probe 可以在 ACK 或 RC 经 NFS 可见前先确认进程组退出，因此 `LAUNCHING -> EXIT_CONFIRMED`、`OWNED -> EXIT_CONFIRMED` 和 `RC_SEEN -> EXIT_CONFIRMED` 都是合法转换。`RC_SEEN` 是可选观测状态，不是退出确认的前置条件。进入 `EXIT_CONFIRMED` 后再恢复和校验 RC；自然退出但 RC 最终不可恢复时结果为 INFRA。

`SIGNAL_DISPOSITION[job]`：

```text
NEVER -> TERM_SENT -> KILL_SENT
```

只有远端 signal RPC 对该 job 返回经过完整帧验证的 `SIGNALED`，才推进 signal disposition。signal RPC 返回 `EXITED` 且此前为 NEVER，表示自然完成，不得记 ABORTED。

### 4.2 ACK 和 RC 稳定 schema

ACK 至少包含：

```text
protocol=EDA_REGRESSION_JOB_V1
job_id=<compile|seed:n>
token=<token>
pid=<pid>
pgid=<pgid>
sid=<sid>
started_at=<time>
end=ACK_END
```

RC 至少包含：

```text
protocol=EDA_REGRESSION_JOB_V1
job_id=<job id>
token=<token>
pgid=<pgid>
rc=<decimal>
ended_at=<time>
end=RC_END
```

controller 校验全部字段、身份和 END 标记后才能使用。看到 RC 只推进 `RC_SEEN`，不能证明进程组退出。

### 4.3 owner manifest 与 SIGKILL 恢复

每个 managed job 必须先生成 job_id/token，并以 `REMOTE_PHASE=LAUNCHING`、`PGID=UNKNOWN` 原子写入 owner manifest；只有 manifest 写成功后才允许执行 SSH launch。ACK 验证成功后原子更新 PGID 并转为 OWNED。`EXIT_CONFIRMED` 后仍保留 owner，直到 RC/log 恢复、结果分类和持久记录全部成功；只有 finalization 成功后才删除。本地 finalization 写失败时必须保留 `EXIT_CONFIRMED` owner，并通过独立记录故障使当前批次 FAIL；只有远端身份或退出状态无法确认时才允许进入 `REMOTE_UNKNOWN` 和 quarantine。

watcher、tail、ssh launcher、compile launcher 和所有其它子进程在执行前关闭继承的 mode lock FD。

下一批取得 mode lock 后必须先审计 stale owner manifest：

1. 合法的 stale `EXIT_CONFIRMED` owner 已经证明远端终态，直接从 owner 集合清除，不再 probe 其 PGID。
2. 对 `LAUNCHING`、`OWNED`、`RC_SEEN` 且带 PGID 的 stale owner 使用 framed probe；对 `LAUNCHING + PGID=UNKNOWN` 的 stale owner 按 token 重复 discovery。找到唯一 PGID 后更新 owner 再 probe；零进程不能证明旧 SSH 不会延迟启动，必须保守写入 quarantine并拒绝复用；多 PGID 或 SSH 失败同样 quarantine。
3. 其余被 probe 的 owner 全部 `EXITED`：原子清空 stale owner，允许继续。
4. 任一 `ALIVE`、`IDENTITY_MISMATCH`、响应不完整或 SSH 不可达：把全部未确认 job 写入 quarantine，拒绝复用 mode。
5. stale owner 包含 compile job 和 simulation job，不能只恢复 seed。

### 4.4 quarantine

controller 在内存中聚合全部 unknown job，再原子重写完整 quarantine manifest。条目包含 job_id、token、PGID、result_state、remote_phase、reason 和时间。不得覆盖先前未知项；不得因把 result_state 保持为 FAIL 而丢失 cleanup unknown 记录。

## 5. 编译 managed job Flow

### 5.1 `run_compile_job()`

抽象功能描述：把一次远端编译作为与 seed 相同身份和退出确认规则的 managed job 执行，成功后生成 fingerprint；它不把本地 `make` 返回当作远端进程组退出证明。

详细逻辑：

1. 创建 `job_id=compile`、唯一 token、ACK、RC、launcher log 和 compile log 路径。
2. 先原子写入 `LAUNCHING + PGID=UNKNOWN` owner；写入失败不得发起 SSH。
3. 远端通过 `setsid` 启动 compile worker；worker 先写完整 ACK，再执行 `eda01_entry.sh compile`，EXIT trap 写完整 RC。
4. controller 验证 ACK 后更新 owner manifest 的 PGID，进入 OWNED。
5. controller 周期 probe；RC 出现只进入 RC_SEEN，probe 返回 EXITED 后进入 EXIT_CONFIRMED。
6. INT/TERM/EXIT cleanup 对 compile 执行 probe、TERM、probe、KILL、probe；不能只终止本地 ssh/make launcher。
7. 无法确认退出时写 compile quarantine，保留 token/PGID，返回非零。
8. EXIT_CONFIRMED 后获取稳定 compile log和 RC；仅 RC=0、simv 存在且身份稳定时写 fingerprint。
9. fingerprint 写入失败或 simv identity 不稳定时返回 compile failure，不进入 seed 调度。

## 6. 监控、记录与最终结果 Flow

### 6.1 `start_watcher()`

抽象功能描述：为单个 OWNED seed 增量消费仿真日志，达到阈值时只发布原子 trigger；它不修改 controller 内存状态，不发送远端信号。

详细逻辑：

1. 子进程先关闭 lock FD。
2. 使用 `tail -n +1 --retry -F` 增量消费日志。
3. 只统计真实 `UVM_ERROR/FATAL @` report 行。
4. 达到阈值时写 seed、count、完整错误行、时间和 token，再原子 rename 为 trigger。
5. watcher 退出时回收 tail 和 FIFO。

### 6.2 `handle_monitor_triggers()`

抽象功能描述：验证并提交所有未处理 trigger，使根因 FAIL 和用户可见记录在正常联停前持久化；它不直接发送 TERM/KILL。

正常提交顺序固定为：

```text
校验 trigger seed/token/count
  -> RESULT_STATE=FAIL，保存 count/line/time/reason
  -> 原子写 monitor_events.tsv
  -> record=on 时原子写 progress.log 和 results.tsv
  -> 原子写 monitor_failure.committed
  -> 打印阈值错误
  -> STOP_REQUESTED=1
```

多个 seed 同轮触发时，先全部设为 FAIL，再一次性写全量记录和 commit marker。`--no-record` 关闭用户侧周期性和事件时 `progress.log/results.tsv/success_seeds/failed_seeds`，但安全相关的 seed state、`monitor_events.tsv`、`monitor_failure.committed` 和最终 summary 始终生成。这里的 monitor 事件日志是联停安全审计，不属于可关闭的动态进度记录。

任一状态文件或记录原子写失败时：

1. 设置 `RECORD_INFRA_FAILURE=1` 和 STOP_REASON。
2. 尽最大努力写 `monitor_failure.uncommitted`，记录已完成步骤和失败路径。
3. 进入“记录基础设施故障 + 紧急清理”分支，执行远端 probe/TERM/KILL。
4. 最终 result 必须 FAIL，summary 明确 `monitor_commit=FAILED`，不得宣称满足正常的先记录后联停契约。

### 6.3 `write_progress_records()`

抽象功能描述：从内存状态生成 progress、seed 列表和 results 的完整临时文件，再分别原子发布；它不计算最终 batch result。

统计语义：

```text
completed = PASS + FAIL
settled   = PASS + FAIL + INFRA + INCOMPLETE + ABORTED
remaining = planned - completed
```

REMOTE_UNKNOWN 是远端生命周期维度，单独统计 `remote_unknown_jobs`，不能覆盖 result state。周期性 progress/results 写入只在 `--record` 时执行；seed state、monitor event/commit 和 final summary 不受该开关影响。

### 6.4 最终 PASS/FAIL 顺序

固定顺序：

```text
finalize 所有 EXIT_CONFIRMED job
  -> 确认 owner manifest 无 active job
  -> 确认无 REMOTE_UNKNOWN/quarantine
  -> 检查 simv identity
  -> compute_batch_result()
  -> write_summary(result)
  -> result=PASS 返回 0，否则返回 1
```

PASS 条件同时要求：planned seed 全部 PASS、无 active/unknown job、无记录基础设施故障、simv identity 未变化。summary 必须包含 `result=PASS|FAIL`、planned/pass/fail/infra/incomplete/aborted、remote_unknown_jobs 和 monitor_commit。

## 7. Framed RPC、退出确认与最终日志 Flow

### 7.1 framed request/response

request 格式：

```text
PROTO EDA_REGRESSION_RPC_V1
REQUEST_ID <random-id>
OP <PROBE|SIGNAL_TERM|SIGNAL_KILL>
COUNT <n>
HASH <sha256 of canonical ITEM lines>
ITEM <job_id> <token> <pgid>
...
END <request-id>
```

remote helper 必须把 stdin 全部读入临时文件，验证 protocol、ID、operation、count、hash、END、job_id/token/PGID 唯一性和合法性，全部通过后才允许发送任何 signal。截断、重复、未知 operation 或 hash 不匹配时非零退出且不发信号。

response 格式使用相同 protocol/request ID/count/hash/END，并逐 job 返回 `ALIVE`、`EXITED`、`SIGNALED`、`IDENTITY_MISMATCH` 或 `SIGNAL_FAILED`。controller 只有在响应 request ID、条目集合、数量、哈希和 END 全部匹配时才消费状态。任何缺项、多项、重复或未知 job 都使本次 RPC 整体失败，请求内未确认 job 进入 REMOTE_UNKNOWN 和 quarantine。

### 7.2 `fetch_stable_remote_log()`

抽象功能描述：从 eda01 获取退出后的权威日志并证明读取期间文件未变化；它不依赖本地共享目录文件的当前内容。

每次尝试执行：

1. remote `stat` 取得 size、mtime、inode。
2. remote `cat` 输出日志到 controller 临时 snapshot，同时记录接收字节数。
3. remote 再次 `stat`。
4. 前后 size/mtime/inode 一致，且接收字节数等于 size，才原子发布 snapshot。
5. 不一致时等待后重试，最多 `SSH_RETRIES` 轮。

退出确认后的每个 seed都无条件执行此流程，即使本地 NFS 日志看起来已经包含 PASS。稳定 snapshot 失败时不得 PASS：SSH 可达但日志无法稳定读取记 INFRA；SSH 状态本身无法确认则保留 result state 并设置 REMOTE_UNKNOWN/quarantine。

### 7.3 `finalize_exited_job()`

抽象功能描述：在退出确认后把 RC、signal disposition 和稳定日志收敛为最终结果，并在完成持久写入后移除 owner；它不负责发信号。

分类规则：

1. monitor 根因已经 FAIL：始终保持 FAIL，只补 RC、日志和真实 UVM count。
2. signal disposition=NEVER：按稳定日志和 RC 正常分类；RC 恢复失败为 INFRA。
3. signal disposition=TERM_SENT/KILL_SENT：稳定日志包含真实错误则 FAIL；稳定日志同时包含完整 PASS 标记、仿真结束标记且 UVM_ERROR/UVM_FATAL 为 0 则 PASS；其它情况为 ABORTED。该判据不推断 signal 与日志事件的墙钟先后关系。
4. probe/signal 返回 EXITED 且从未成功 signal：按自然完成处理，不能因全局 STOPPING 而 ABORTED。
5. 只有 result state 和所有记录写入完成后，才从 owner manifest 删除该 job。

## 8. 调度与 early-stop Flow

### 8.1 launch ACK 等待

`launch_seed()` 先把 token 以 `LAUNCHING + PGID=UNKNOWN` 写入 owner manifest，成功后才发起 SSH。ACK 等待的每个轮询周期调用 `handle_monitor_triggers()`。已有 seed 提交 monitor failure 后立即停止等待完整 launch timeout，并按 token discovery 接管当前 LAUNCHING job；发现唯一 PGID则更新 owner，零个或多个 PGID、SSH 失败都视为无法确认并进入 REMOTE_UNKNOWN/quarantine，不能把零进程当作安全未启动。此后不再启动新 seed。

### 8.2 `cleanup_controller()`

抽象功能描述：在 controller 任何退出路径中统一收敛 watcher、LAUNCHING/OWNED job、最终结果、owner 和 quarantine，并以释放 mode lock 作为最后一步；它可重复调用但只执行一次清理主体。

取得 mode lock 后立即安装一次 EXIT/INT/TERM trap，后续 compile 和 simulation 阶段不替换 trap。全局 `PHASE` 只用于记录当前阶段和设置 STOP_REASON；统一 cleanup 按实际 owner/remote phase 工作。详细顺序：

1. 检查幂等 guard；已执行则直接返回，首次执行立即设置 guard 和 `STOP_REQUESTED=1`，禁止继续 launch。
2. 调用 `handle_monitor_triggers()`，先提交 watcher 已发布的根因 FAIL、monitor event 和 commit marker。
3. 停止并 wait 所有 watcher/tail，避免后续产生未消费的新 trigger。
4. 对 `LAUNCHING + PGID=UNKNOWN` job 按 token discovery；唯一 PGID则更新 owner，零个、多组或 SSH 失败均为 REMOTE_UNKNOWN，因为旧 SSH 可能延迟创建远端进程。
5. 对全部带 PGID 的 active job执行一次 framed probe；probe 发现 `EXITED` 时可立即 finalize，并由最终日志发布新的 monitor trigger。
6. 再次调用 `handle_monitor_triggers()`，提交首次 probe/finalize 期间刚发布的 trigger，确保后续 TERM/KILL 前完成根因记录。
7. 重试仍保留在 owner 集合中的 `EXIT_CONFIRMED` finalization。
8. 对仍 ALIVE 的 job执行 TERM；只有完整响应中的 SIGNALED 才记录 TERM_SENT。
9. 等待后再次 framed probe并 finalize 已退出 job。
10. 对仍 ALIVE 的 job执行 KILL；只有完整响应中的 SIGNALED 才记录 KILL_SENT。
11. 最后一次 framed probe并 finalize 全部 EXIT_CONFIRMED job。
12. 聚合 REMOTE_UNKNOWN，原子重写全量 quarantine；unresolved owner 保留 token/PGID/phase。
13. 删除已经 finalization 成功的 owner，原子重写剩余 owner manifest。
14. 原子写最终 summary；若清理期间仍有 unknown，batch result 必须 FAIL。
15. 删除 controller owner 信息并最后释放 mode lock。

任一步被第二次 trap 或显式 cleanup 调用打断时，幂等 guard 防止重复发送信号；已经持久化的 unresolved owner 保证下一批可以继续 stale-owner 审计。

### 8.3 正常主循环顺序

```text
处理 trigger 并提交记录
  -> 批量 framed probe
  -> 对 EXITED job finalize
  -> 检查 remote unknown/记录故障/simv 变化
  -> 若 STOP_REQUESTED：先 probe 自然退出，再 TERM/probe/KILL/probe
  -> 否则按 jobs 上限逐个 launch，每次 launch 前再处理 trigger
  -> 周期性记录进度
```

并发数按 `REMOTE_PHASE=LAUNCHING|OWNED|RC_SEEN` 的 job 统计，不能只按 result state。20 并发测试必须证明峰值不超过 20。

### 8.4 关键 helper 源码级伪代码

#### 8.4.1 `require_option_value()`

抽象功能描述：供 CLI 和 remote worker 参数解析器在读取带值选项时使用，只证明下一个 argv 可作为值。

源码级伪代码：

```text
require_option_value(option, argc_remaining, next)
  if argc_remaining < 2 or next is empty or next starts with "--": print error; return 2
  return 0
```

中文详细伪代码：解析器在任何 `shift 2` 前传入当前选项、剩余参数数量和候选值；函数先检查参数是否存在，再拒绝空字符串和另一个长选项。失败时打印具体选项并返回 2，调用者立即退出且不创建执行目录；成功时才允许读取 `$2`。

#### 8.4.2 `normalize_bounded_decimal()`

抽象功能描述：把外部十进制字符串转为受限规范值，避免前导零、八进制和超长整数问题。

源码级伪代码：

```text
normalize_bounded_decimal(name, raw, min, max)
  require raw matches digits and length <= digits(max)
  strip leading zeroes; empty becomes 0
  compare normalized length/value against min/max without raw arithmetic
  output normalized decimal
```

中文详细伪代码：先用正则确认只含数字并用最大值位数限制输入长度；再以字符串方式移除前导零。只有规范值长度受限后才进行十进制比较，超界返回 2；`08` 输出 `8`，不会进入 Bash 八进制解释。

#### 8.4.3 `validate_plusarg()`

抽象功能描述：验证一个用户 runtime plusarg 并登记 key 唯一性，不判断该参数在 UVM 中的业务含义。

源码级伪代码：

```text
validate_plusarg(token)
  split exactly once into +KEY and VALUE
  validate KEY regex and VALUE allowlist
  reject reserved or already-seen KEY
  append original token to USER_PLUSARGS; mark KEY seen
```

中文详细伪代码：先确认只有一个等号且 VALUE 非空，再分别应用 KEY 和 VALUE 白名单；随后检查保留 key 与重复 key。任何失败都返回 2；成功后保持用户输入顺序保存完整 token，后续作为独立 argv 传递。

#### 8.4.4 `write_owner_manifest()`

抽象功能描述：把当前内存 active job 集合变成下一批可恢复的唯一持久 owner 真源。

源码级伪代码：

```text
write_owner_manifest()
  render every LAUNCHING/OWNED/RC_SEEN/EXIT_CONFIRMED-unfinalized job to temp
  fsync temp when available; atomic rename to mode owners file
  return failure without launching/removing any job if publish fails
```

中文详细伪代码：遍历 managed job，仅输出尚未成功 finalization 的 owner，包含 token、PGID 或 UNKNOWN、phase 和 host；完整临时文件写完后原子替换。launch 前发布失败则禁止 launch；finalization 后发布失败则保留旧 owner并把 batch 判为基础设施失败。

#### 8.4.5 `run_remote_rpc()`

抽象功能描述：一次性执行远端 probe 或 signal，并只在完整响应严格匹配时返回可消费结果。

源码级伪代码：

```text
run_remote_rpc(op, jobs)
  canonicalize ITEM lines; compute count/hash/request_id; write framed request
  ssh remote helper with request on stdin, capture response and ssh rc
  validate protocol/id/op/count/hash/items/end and allowed statuses
  on any mismatch return failure without partial state update
  otherwise expose complete status map
```

中文详细伪代码：controller 先按 job ID 排序生成规范 ITEM，再计算哈希和帧头尾；SSH 只发送这一帧。remote helper 完整校验后才允许 signal。controller 捕获全部响应并再次校验集合完全相等，任一缺失、重复或未知项都使整批失败，调用者把请求集合统一转入 remote unknown。

#### 8.4.6 `run_compile_job()`

抽象功能描述：执行一次受管编译并在完整退出、RC=0 和 simv 稳定后发布 fingerprint。

源码级伪代码：

```text
create compile token and files
persist LAUNCHING owner before ssh launch
launch setsid compile worker; validate ACK; update OWNED owner
poll RC and framed probe until EXIT_CONFIRMED or stop
recover RC/stable log; require rc=0 and stable executable simv
write fingerprint; finalize owner
```

中文详细伪代码：先落盘无 PGID owner，再启动远端 worker，ACK 通过后补 PGID。RC 只作为可选观测，probe 确认退出后恢复 RC 和日志；编译失败、simv 缺失或 fingerprint 写失败均返回非零。INT/TERM 由统一 cleanup 接管该 compile owner。

#### 8.4.7 `start_watcher()`

抽象功能描述：增量读取一个 seed 日志并在阈值达到时发布单个不可变 trigger。

源码级伪代码：

```text
child closes lock fd; start tail -F into fifo
for each line matching real UVM_ERROR/FATAL report: increment count
if count reaches limit: atomically publish seed/token/count/line/time trigger; exit
trap kills tail, waits, removes fifo
```

中文详细伪代码：watcher 不写结果状态，只维护本 seed 的错误计数；达到阈值后一次性发布带 token 的 trigger并退出。清理 trap 回收 tail，避免 watcher 继承锁或成为残留进程。

#### 8.4.8 `handle_monitor_triggers()`

抽象功能描述：把 watcher 事件提交成根因 FAIL 和可审计安全记录，再允许主循环联停。

源码级伪代码：

```text
collect every unprocessed valid trigger
set all root RESULT_STATE=FAIL and error evidence
write seed state and monitor_events
if record enabled: write progress/results
write committed marker; then set STOP_REQUESTED
on write failure: mark uncommitted infra failure and request emergency cleanup
```

中文详细伪代码：先验证 seed、token 和 count，再一次性更新所有根因内存状态；随后按固定顺序持久化安全记录。只有 commit marker 成功后才打印阈值错误和设置正常 stop；写失败则记录 uncommitted 状态，batch 必定 FAIL并交给统一 cleanup。

#### 8.4.9 `fetch_stable_remote_log()`

抽象功能描述：取得远端退出 job 的完整、读取期间未变化的权威日志快照。

源码级伪代码：

```text
repeat up to SSH_RETRIES:
  remote stat-before; stream file; count received bytes; remote stat-after
  if inode/size/mtime equal and bytes == size: atomic publish snapshot; return success
  sleep before retry
return failure
```

中文详细伪代码：每轮先取得元数据，再流式读取到临时文件，最后重新取元数据；四项一致才发布。任何 NFS 本地内容都不作为完整证明；重试耗尽后调用者不得把 seed 判为 PASS。

#### 8.4.10 `finalize_exited_job()`

抽象功能描述：在远端退出确认后完成 RC、日志、结果和 owner 的最终一致提交。

源码级伪代码：

```text
require phase EXIT_CONFIRMED
recover and validate RC; fetch stable log
preserve monitor-root FAIL, otherwise classify by errors/pass markers/signal disposition/rc
persist result records
remove job from owner set and atomically rewrite manifest
```

中文详细伪代码：函数先恢复 RC 和稳定日志；monitor 根因只补充证据不改 FAIL。其它 job 按确定性规则分类；结果记录成功后才从 owner 集合删除并重写 manifest。本地 finalization 写失败时保留 `EXIT_CONFIRMED` owner并设置独立记录故障，当前批次返回 FAIL；不得因此进入 quarantine。只有远端身份或退出状态无法确认时才使用 `REMOTE_UNKNOWN` 和 quarantine。

#### 8.4.11 `compute_batch_result()`

抽象功能描述：在所有 finalization 结束后计算唯一批次退出结果，不执行清理动作。

源码级伪代码：

```text
if any seed result != PASS: FAIL
if any active/unknown owner or quarantine: FAIL
if record infra failure or simv identity changed: FAIL
otherwise PASS
```

中文详细伪代码：依次检查所有 seed 结果、远端生命周期、持久 owner/quarantine、记录系统和 simv identity；任一条件不满足即返回 FAIL。只有全部条件满足才返回 PASS，调用者随后写 summary 和进程退出码。

#### 8.4.12 `cleanup_controller()`

抽象功能描述：统一处理所有退出路径，确保能确认的 job 被 finalize，不能确认的 job 被持久隔离，并最后释放锁。

源码级伪代码：

```text
if cleanup guard set: return; set guard and disable launch
handle monitor triggers; stop/wait watchers
discover unknown-PGID launching jobs; probe/finalize exited
handle monitor triggers; retry residual EXIT_CONFIRMED finalization
TERM alive; probe/finalize; KILL alive; probe/finalize
aggregate unknown and atomically write quarantine/unresolved owners
write final summary; remove controller owner; unlock last
```

中文详细伪代码：幂等 guard 防止 trap 重入。函数先提交 watcher 已发布的 monitor trigger，再停止并等待本地 watcher；随后接管 ACK 前任务。首次 probe 发现自然退出项时可立即 finalize，并从最终日志发布新的 trigger；probe 返回后再次调用 `handle_monitor_triggers()`，在任何 TERM/KILL 前提交这些根因记录，再重试仍残留的 `EXIT_CONFIRMED` finalization。之后按 TERM、probe/finalize、KILL、probe/finalize 顺序处理仍存活的远端 job。最后保留所有不确定身份，写 quarantine 和 summary，mode lock 必须是最后释放的资源。

## 9. 代码修改范围

| 文件 | 修改内容 |
|---|---|
| `mem_ut/ver/ut/memblock/sim/run_eda_regression.sh` | 参数、plusargs、共享路径证明、双状态机、managed compile、framed RPC、owner/quarantine、监控提交、stable log、final result |
| `mem_ut/ver/ut/memblock/sim/Makefile` | 定义 `EFFECTIVE_PLUSARGS`，新增 `eda_print_effective_plusargs`，compile/batch_run 复用同一变量 |
| `mem_ut/ver/ut/memblock/cfg/verif/project_cfg_vcs.mk` | cfg override 合并改用 Make 权威变量；coverage 编译期和运行期参数均受 `ccov=on` 控制 |
| 本 plan | coding 中必要调整只允许写入 `IMPLEMENTATION_DELTA` |
| `AI_DOC/plan/test_framework/review_doc/undo/*` | plan review 和 implementation review |

本轮不新增 `plus.sv` 参数；`--plusarg` 只是已有 runtime plusarg 的安全透传入口。

## 10. 验证计划

### 10.1 静态和 CLI

1. `bash -n`、`git diff --check`。
2. 如安装 `shellcheck`，运行脚本检查；如不可用，在 review 中记录并逐项人工检查 quoting、数组、trap、subshell 和未定义变量。
3. 所有带值选项缺值、空值、后接另一个 `--option` 必须在 2 秒内返回 2。
4. 对所有显式数值测试 `0/-1/abc/08/超长数字/超过上界`；`08` 结果为 8。
5. 显式 seed 覆盖合法 count；非法 count 即使被覆盖也必须返回 2；`1/01` 去重。
6. cfg 不存在、output-dir 为文件、非空、不可写均 fail-fast。
7. 单/多 plusarg 接受；空 VALUE、额外 `=`、重复 key、保留 key、空白和 shell 元字符拒绝。
8. dry-run 生成 user/effective plusargs，记录 `shared_path_validation=DEFERRED`。
9. Make 只读目标输出与实际 run command 的 effective plusargs 一致；cfg 同名 key被过滤，用户覆盖值只出现一次，多个不同 key各出现一次，compile 命令不含 runtime plusargs，每个 seed 的 batch_run 收到相同 plusargs。
10. `make -n` 证明 `ccov=off` 编译和运行命令均无 `-cm*`，`ccov=on` 均包含要求的 coverage 参数。
11. 默认 verbosity 为 UVM_LOW；显式 verbosity 进入 effective simv 命令；debug 默认派生 wave=on 和 UVM_DEBUG，显式 `--no-wave/--verbosity` 可覆盖派生值。
12. timeout-ns 必须进入 effective simv 命令；mock timeout 后 job 按稳定日志、RC 和 signal disposition 分类并完成清理。

### 10.2 mock 生命周期、协议和并发

1. 全 PASS：10 seed、jobs=4，result=PASS、pass=10、fail=0、退出 0。
2. 40 个短 job、jobs=20，峰值严格为 20。
3. monitor 根因先写 FAIL 和 monitor_events；record=on 时再写 progress/results；最后写 commit marker，全部完成后才发生首个 signal。
4. `--no-record` 不写用户 progress/results，但 trigger 时强制写 seed state、monitor_events 和 commit marker，最终 summary 始终存在。
5. 写 monitor 记录失败进入 uncommitted + emergency cleanup，最终 FAIL。
6. error-limit=2 时第一条不停止、第二条提交失败事件。
7. root `RESULT_STATE=FAIL` 同时 cleanup `REMOTE_PHASE=REMOTE_UNKNOWN`，FAIL 不被覆盖且 quarantine 存在。
8. signal 前自然退出、signal RPC 返回 EXITED：正常恢复 RC/log 分类，不记 ABORTED。
9. 成功 TERM/KILL 后退出且无真实错误：ABORTED。
10. RC 先出现但组仍活着：保持 RC_SEEN 和 owner，不提前 finalize。
11. ACK、RC 和最终日志分别模拟 NFS 延迟；只能在远端恢复并通过完整 schema/stable snapshot 后消费。
12. framed RPC 测试空输入、截断、错误 hash/count/END、重复/未知 job、重复 response、缺项 response；signal 请求验证失败时零信号副作用。
13. SSH probe/signal 失败时聚合全部 unknown job，quarantine 不覆盖前项。
14. controller SIGKILL 后下一批 stale-owner 审计：active job 阻止复用；全部 EXITED 才清理并继续。
15. compile INT/TERM：执行 TERM/probe/KILL/probe，不能留下 owner；无法确认时 compile quarantine。
16. watcher、ssh、compile 子进程关闭 lock FD，controller 死亡后 lock 可由下一批获取并进入 stale-owner 审计。
17. 双向 nonce 在普通文件、非共享目录、远端不可写和响应篡改场景均失败。
18. 远端 worker 已启动但 ACK 尚未发布时 SIGKILL controller；下一批必须从 `LAUNCHING + PGID=UNKNOWN` owner 按 token discovery 找到或 quarantine 该 job。
19. `--no-monitor` 场景中错误 seed最终为 FAIL，但其它 seed不被提前联停并正常结算。
20. SSH 已接受请求但远端 worker 延迟创建，期间 SIGKILL controller；下一批 discovery 即使暂时为零也必须 quarantine，不能清除 owner 或复用 mode。

### 10.3 真实 eda01/VCS

1. 使用独立 mode 执行一次 managed `--compile --wave --coverage`，确认 compile ACK/RC/exit/fingerprint 闭环。
2. 同一 simv `--no-compile` 执行至少两个 seed，证明 compile once。
3. 使用 `--plusarg +MEMBLOCK_MAIN_TRANS_NUM=<小值>`，从 `effective_plusargs.txt`、最终 simv 命令和 UVM 行为确认消费。
4. coverage on 两 seed 生成各自 testdata。
5. coverage-enabled simv 配合 `--no-coverage` 不产生新 testdata。
6. coverage off 前后分别记录 coverage 目录相对路径、size、mtime、文件数和 seed testdata manifest，差异必须为空。
7. wave=on 真实运行必须生成非空 FSDB；no-wave/no-coverage 单 seed PASS，且无新 FSDB/coverage。
8. 默认 UVM_LOW、显式 verbosity、debug 派生和显式覆盖分别从最终 simv 命令确认。
9. timeout-ns 从最终命令确认，并执行一个短 timeout 场景验证分类和清理。
10. early-stop 专项用例确认根因 FAIL 记录时间早于 TERM/KILL 时间。
11. 真实 20 并发按 10.4 的量化预检执行；资源不足时记录“环境容量未验证”，不得宣称真实 20 并发通过。

### 10.4 真实 20 并发量化资源门槛

先用同一短 testcase 单 seed运行一次基线，1 秒采样该 job 进程组总 RSS，记录峰值 `RSS_ONE`；记录该 seed 新增日志、FSDB 和 coverage 的总字节数 `OUT_ONE`。预检命令至少包含远端 `nproc`、`uptime`、`/proc/meminfo` 的 MemAvailable、`df -Pk` 和可用时的 license `lmstat`。

只有同时满足以下条件才启动真实 jobs=20：

1. `MemAvailable >= 20 * RSS_ONE * 1.25 + 8 GiB`。
2. output/sim 工作盘可用空间 `>= 20 * OUT_ONE * 1.5 + 20 GiB`。
3. 启动前 1 分钟 load average `<= 0.70 * nproc`。
4. `lmstat` 可解析时，目标 VCS runtime feature 的可用 token 必须不少于 20；不可解析时按 `2 -> 4 -> 8 -> 20` 分级 ramp，每一级所有 job 都必须在 120 秒内越过 license checkout，任一级失败即停止、清理并标记容量未验证。
5. 每个 worker ACK 超时 120 秒，批次总超时 30 分钟，TERM/KILL/probe 清理总超时 60 秒。
6. 任一资源指标低于门槛、出现持续 license wait、磁盘增长超预算或清理不能确认时立即停止并记录证据。

### 10.5 通过标准

静态、CLI、mock 状态机/RPC/monitor/恢复测试必须全部通过。真实测试至少完成 managed compile、compile-once 双 seed、plusarg、coverage on/off 和全 PASS summary；20 并发在资源预检通过后执行。最终独立 implementation review 必须明确无 blocker，否则继续修订、测试和 review。

## 11. 与初步 plan 差异说明

本章仅服务 review，不作为 coding 实现依据。

### 11.1 `handle_monitor_triggers()`

修改前文字伪代码：发现 trigger 后只设置全局 stop reason；根因 seed 仍是 RUNNING，联停后通常被归类为 ABORTED；`--no-record` 时没有失败持久提交证据。

修改后文字伪代码：controller 校验全部 trigger，先把每个根因结果设为 FAIL并保存错误证据；随后无条件原子写 seed state、monitor_events 和 commit marker，仅 record=on 时写 progress/results；全部必需记录成功才设置 STOP_REQUESTED。写失败进入显式 emergency cleanup，并在 summary 标记 monitor commit 失败。

### 11.2 `finalize_exited_job()`

修改前文字伪代码：RC 文件出现即可读取本地日志分类并清除 PGID；STOPPING 中 RC 不可见时直接 ABORTED。

修改后文字伪代码：RC 只推进 RC_SEEN；远端 probe 确认 EXITED 后无条件取得稳定远端日志，再根据根因 FAIL、是否成功发送 signal、RC 和日志分类。自然退出但 RC 不可恢复记 INFRA；结果和记录完成后才删除 owner。

### 11.3 `write_progress_records()` 与 summary

修改前文字伪代码：周期性/最终写记录由同一函数混合处理，summary 在 all_pass 计算前写出且没有显式 result。

修改后文字伪代码：进度记录只从内存生成原子文件；最终先完成全部 finalization、owner/unknown/simv 检查，再由 `compute_batch_result()` 计算结果，最后写包含 `result=PASS|FAIL` 的 summary。

### 11.4 probe/signal helper

修改前文字伪代码：controller 把共享 request 文件路径传给远端，remote 逐行处理并立即发 signal；响应不完整仍可能被部分消费。

修改后文字伪代码：controller 通过 SSH stdin 发送带版本、请求 ID、数量、哈希和 END 的完整帧；remote 先缓存并完整验证全部请求，之后才 probe 或 signal；controller 仅在完整响应集合严格匹配时一次性消费，否则整批进入 remote unknown。

### 11.5 compile 和 owner 恢复

修改前文字伪代码：compile 是普通同步 make；controller 被 SIGKILL 后没有持久 active owner，下一批只依赖 lock/quarantine。

修改后文字伪代码：compile 和 seed 都是 managed job，统一 ACK/RC/token/PGID/probe/signal；SSH 前先落盘 LAUNCHING owner，每次 owner 变化原子更新 manifest。下一批取得锁后先远端审计 stale owner；未知 PGID 即使 discovery 为零也不能证明不会延迟启动，必须 quarantine 并阻止 mode 复用。

### 11.6 参数解析、cfg 和十进制

修改前文字伪代码：解析器直接读取 `$2` 并 `shift 2`，缺值可能循环；正整数和 seed 直接进入 Bash 算术，前导零可能按八进制解释；cfg 不存在时 Make 静默忽略。

修改后文字伪代码：每个带值选项先验证 argv 完整性；全部显式数值先用字符串规则规范化和限界，再进入业务逻辑；cfg 文件在 SSH 前必须存在且可读，任何失败返回 2。

### 11.7 plusargs 与 effective plusargs

修改前文字伪代码：脚本没有用户 runtime plusarg 入口，无法临时覆盖 testcase 请求数量等参数。

修改后文字伪代码：可重复 `--plusarg` 逐 token 执行格式、白名单、保留 key 和重复 key 校验；Makefile 的单一 `EFFECTIVE_PLUSARGS` 负责过滤 cfg 同名值并追加用户值，dry-run 和每个 seed复用相同最终结果，compile 不接收 runtime plusargs。

### 11.8 共享目录双向证明

修改前文字伪代码：只检查本地目录或远端路径存在，普通文件、不同挂载和缓存视图可能被误判。

修改后文字伪代码：local 和 remote 通过 nonce A、nonce B 和最终确认文件完成双向读写校验；任一步内容不一致、不可写或不可见都在 launch 前失败。dry-run 明确记录验证延期。

### 11.9 coverage on/off

修改前文字伪代码：运行期 coverage 参数无条件加入 SIMV_OPTIONS，coverage-enabled simv 即使请求 off 仍会收集。

修改后文字伪代码：编译期和运行期所有 `-cm*` 都受 `ccov=on` 控制；coverage off 验证使用运行前后数据库 manifest 差异证明没有新增 testdata。

### 11.10 双状态模型和 `--no-record`

修改前文字伪代码：一个 seed 状态同时表示仿真结果和远端生命周期，cleanup 失败会覆盖根因；关闭 record 后监控安全提交语义不明确。

修改后文字伪代码：RESULT_STATE、REMOTE_PHASE 和 SIGNAL_DISPOSITION 分离维护；根因 FAIL 不被 cleanup unknown 覆盖。`--no-record` 只关闭用户动态 progress/results，seed state、monitor event/commit 和 summary 作为安全审计始终保留。

### 11.11 20 并发资源门槛

修改前文字伪代码：直接要求执行 jobs=20，没有量化资源、license、总超时和清理门槛。

修改后文字伪代码：先测单 seed RSS/输出基线，再按内存、磁盘、load 和 license smoke 的固定阈值决定是否执行；ACK、批次和 cleanup 都有明确超时。资源不足只能记录容量未验证，不能宣称真实 20 并发通过。

## 12. IMPLEMENTATION_DELTA

本章记录 coding 过程中经 review 和实测确认的实现细节，不改变第 10 章验收标准。

### 12.1 cfg 快照与 hash

正式执行前将输入 cfg 复制到输出目录 `frozen_plus_cfg/<cfg>.cfg`，权限收紧为 `0444`，并记录 `cfg_sha256`。所有 seed worker 使用该快照，避免回归期间原始 cfg 被修改导致同一批 seed 读取不同内容。`effective_plusargs.txt` 由 Makefile 的 `eda_print_effective_plusargs` 生成，用户覆盖项只保留一次。

### 12.2 simv 权威身份

由于本地 NFS 可能缓存新建 `simv` 不存在的结果，`simv_exists()`、`simv_identity()` 和 `simv_hash()` 均以 `eda01` 的远端 `test/stat/sha256sum` 为准。compile 完成后等待 fingerprint 和 `compile.finalized`，再移除 compile owner；no-compile 模式校验 fingerprint 的 wave、coverage、identity 和 hash。

### 12.3 监控提交与信号竞态

cleanup 开始时以及首次 probe/finalize 后调用 `handle_monitor_triggers()`。因此 watcher 已发布 trigger 而 controller 恰好收到 INT/TERM 时，root seed 的 FAIL、monitor event 和 commit marker 会先于远端 TERM/KILL 写入。cleanup 期间再次收到信号只记录附加原因并返回，不会跳过 KILL、稳定日志恢复、summary 和解锁流程。

### 12.4 原子发布与 watcher 生命周期

本地原子发布统一使用 GNU `mv -fT`，目标路径为目录时明确失败，避免 summary、owner、quarantine 或 commit 文件被错误移动到目录中。watcher 的 `tail` 使用 `--pid=<controller pid>`；controller 被 SIGKILL 后，watcher 会随 controller 消失而退出，避免本地 tail/FIFO 孤儿长期占用资源。

### 12.5 并发和测试辅助

新增 `peak_live` 汇总字段。worker 在 ACK/RC 中记录远端纳秒开始和结束时间，controller 只对全部 seed 存活区间做 sweep，计算真实最大重叠数；compile job 和运行期 probe 均不更新该字段。mock helper 支持 compile 延时、compile 失败文件和 `MOCK_FAIL_DELAY`，用于编译 TERM、compile-only 峰值、监控提交顺序和故障写入测试。20 个零延时 seed 得到 `peak_live=1`，20 个长任务得到 `peak_live=20`，两组均 PASS；compile 跨 probe 周期后失败且零 seed 启动时得到 `peak_live=0`。

### 12.6 最终 review 修订

最终 implementation review 期间继续补充以下实现：

1. Makefile 在 cfg 快照不可读时 fail-fast，且 cfg 路径含空格时仍能完整解析；remote worker 在 ACK/RC 生命周期建立后校验 `cfg_sha256`。
2. summary 增加 `completed`、`settled`、`remaining`、`not_run`；`summary.log` 无法发布时写独立 `summary.failure`。
3. monitor commit 和 TERM/KILL 事件记录纳秒时间戳，mock 已直接证明 commit 早于首个 TERM。
4. stale owner 对 header、字段数、job 唯一性、PGID、phase、run id、host 和时间字段执行严格校验，畸形 manifest 直接 quarantine。
5. RPC 返回 `IDENTITY_MISMATCH` 时，只有 controller 已恢复同 token/PGID 的有效 RC 才把它解释为“原进程已退出且 PGID 被复用”；无匹配 RC 仍进入 quarantine。
6. 最新实现分别执行短任务和长任务 20 seed、jobs=20 mock；短任务 `peak_live=1`，长任务 `peak_live=20`，两组均 `pass=20`、`remote_unknown_jobs=0`。
7. `EXIT_CONFIRMED` 后的本地记录或 owner 删除提交失败通过 `FINALIZATION_FAILURE` 和 `RECORD_INFRA_FAILURE` 表达，不再把 `REMOTE_PHASE` 降级为 `REMOTE_UNKNOWN`；owner 保留供当前 cleanup 重试或下一批 stale audit 清理。
8. stale audit 对持久化的 `EXIT_CONFIRMED` owner 直接视为远端终态，不再 probe 可能已经复用的 PGID；使用无关活进程 PGID 的故障注入证明新批次 PASS 且该进程未被 probe 或终止。

### 12.7 真实 EDA 验证结果

2026-10-20 在 `eda01` 完成：

- 真实 managed compile 和单 seed PASS。
- 同一 fingerprint simv 的两 seed `--no-compile` PASS，均使用 `+MEMBLOCK_MAIN_TRANS_NUM=2`。
- `--wave --coverage` 编译和运行 PASS，生成非空 FSDB（1217922 bytes）和 VCS coverage 数据库。
- coverage-enabled simv 配合 `--no-coverage` 运行 PASS，coverage manifest 前后差异为 0。
- 最终生命周期修订后复用 `base_fun_regression_20261020` fingerprint，以 seed `666673`、`+MEMBLOCK_MAIN_TRANS_NUM=2` 执行真实 `--no-compile` 冒烟；summary 为 PASS，stable final log 中 `UVM_ERROR=0`、`UVM_FATAL=0` 且正常 `$finish`。

真实 20 VCS 并发未执行：eda01 的内存、磁盘和 load 基础指标满足，但 `lmstat` 不可用且本轮没有取得单 seed 进程组 `RSS_ONE`，不满足第 10.4 节的量化准入条件，因此按规则记录为“真实 20 路容量未验证”。

持久证据位于：

- `mem_ut/ver/ut/memblock/sim/.eda_regression/evidence_cov_off_20261020/`
- `mem_ut/ver/ut/memblock/sim/.eda_regression/real20_capacity_precheck_20261020.txt`
- `mem_ut/ver/ut/memblock/sim/.eda_regression/real_post_lifecycle_fix_20261020/`
- `mem_ut/ver/ut/memblock/sim/.eda_regression/mock_exit_confirmed_retry_20261020_v1_reused_pgid_evidence/reused_pgid_evidence.txt`
