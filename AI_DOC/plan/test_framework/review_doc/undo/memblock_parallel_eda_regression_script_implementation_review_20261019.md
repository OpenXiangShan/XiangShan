# memblock 远端并发仿真脚本 Implementation Review

关联 plan：`AI_DOC/plan/test_framework/plan/do/memblock_parallel_eda_regression_script_plan_20261019.md`
关联 plan review：`AI_DOC/plan/test_framework/review_doc/undo/memblock_parallel_eda_regression_script_plan_review_20261019.md`
Review 日期：2026-10-19
Review 结论：实现通过，无已知 blocker

## 1. 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 对应代码对象或落点 | 使用场景/示例 |
|---|---|---|---|
| `controller` | 当前节点上的批次总控进程 | `run_eda_regression.sh` 普通模式 | 维护最多 20 个 active seed |
| `remote worker` | eda01 上单 seed 的 session/process-group leader | `remote_worker()` | 发布 ACK 后执行 bootstrap 和 `batch_run` |
| `run token` | worker 及其 make/simv 子进程共同继承的身份串 | `EDA_REGRESSION_RUN_TOKEN` | probe/kill 前确认 PGID 所有权 |
| `compile fingerprint` | simv 编译能力与文件身份快照 | `.eda_compile_fingerprint` | no-compile 模式验证 wave/coverage |
| `quarantine` | 远端状态无法确认时对 mode 的持久隔离 | `.eda_regression/locks/<mode>.quarantine` | 防止未知进程仍运行时覆盖 simv |
| `NFS recovery` | 本地暂时看不到共享新文件时，通过 SSH 只读恢复 | `recover_ack_over_ssh()`、`recover_rc_over_ssh()`、`prepare_classification_log()` | 避免 ACK/rc/log 可见性延迟误判 |

抽象功能描述：脚本负责远端编译一次、多 seed 并发运行、错误联停、结果记录和本批次进程清理；不修改或判断 DUT 功能逻辑，不替代 UVM testcase、sequence、RM 或 scoreboard。

## 2. 修改范围

本次实现文件：

- `mem_ut/ver/ut/memblock/sim/run_eda_regression.sh`
- `mem_ut/ver/ut/memblock/sim/Makefile`
- 需求/方案 plan、plan review 和本 implementation review 文档。

未修改现有 `remote_eda_make.sh`、`eda01_entry.sh`、UVM/SystemVerilog、RTL、cfg 或 testcase。

## 3. Makefile Bootstrap 单一来源

### 3.1 抽象功能描述

`eda_print_bootstrap` 只向 controller 输出 Makefile 当前维护的 `REMOTE_BOOTSTRAP`，不执行编译或仿真，也不改变现有远端目标行为。

源码位置：`mem_ut/ver/ut/memblock/sim/Makefile`，目标：`eda_print_bootstrap`

```makefile
eda_print_bootstrap:
	@printf '%s\n' '$(REMOTE_BOOTSTRAP)'
```

中文伪代码：controller 启动后调用该只读目标，取得当前 module 初始化和 VCS/Verdi 加载命令；新脚本不复制工具版本字符串，因此原 Makefile 更新工具版本后脚本自动复用最新值。

正确性检查：`make -s -C mem_ut/ver/ut/memblock/sim eda_print_bootstrap` 已输出当前已验证的 VCS/Verdi bootstrap。

## 4. 参数、seed 与编译产物契约

### 4.1 参数解析

抽象功能描述：主入口在任何 SSH/编译动作前完成参数默认值、显式 seed、debug 派生和非法输入检查。

实现行为：

- `--tc` 必填，`cfg=default`、`count=1`、`jobs=20`。
- 默认 no-compile、no-wave、no-coverage、`UVM_LOW`。
- monitor/record 默认开启，error limit 默认 1。
- `--seed/--seeds` 存在时忽略 count，重复 seed 去重。
- debug 只补默认值；显式 `--no-wave` 或 `--verbosity` 优先。
- 非空 output directory 直接失败，避免覆盖历史批次。

验证：help、generated seed dry-run、显式 seed 去重、debug 覆盖、缺 tc、非法 jobs/seed/verbosity 均已测试。

### 4.2 Compile fingerprint

抽象功能描述：`write_fingerprint()` 在 compile 成功后绑定 simv 配置和文件身份；`verify_fingerprint()` 在 no-compile 请求 wave/coverage 时证明现有 simv 具备对应能力。

源码位置：`run_eda_regression.sh`，函数：`write_fingerprint()`、`verify_fingerprint()`

```bash
printf 'wave=%s\n' "$WAVE_VALUE"
printf 'ccov=%s\n' "$CCOV_VALUE"
printf 'sha256=%s\n' "$hash"
printf 'identity=%s\n' "$identity"
```

中文伪代码：compile 前删除旧 fingerprint；compile 成功后计算 simv SHA-256 和 device/inode/size/mtime，原子写入配置。no-compile 时重新计算并比较；coverage/wave 配置或文件身份不匹配即失败，不启动任何 seed。

正确性检查：base_fun 历史 simv 在没有 fingerprint 时请求 `--wave --no-compile` 已按预期返回非零；真实独立 mode 生成的 fingerprint 记录 `wave=on`、`ccov=on`，后续 no-compile 双 seed通过。

## 5. Remote Worker 与进程身份

### 5.1 `remote_worker()`

抽象功能描述：worker 作为单 seed 的 session/process-group leader，先发布可管理身份，再初始化 EDA 环境并执行一次 `batch_run`；它不执行 compile。

源码位置：`run_eda_regression.sh`，函数：`remote_worker()`

```bash
pgid="$(ps -o pgid= -p "$$" | tr -d ' ')"
sid="$(ps -o sid= -p "$$" | tr -d ' ')"
if [[ "$pid" != "$pgid" || "$pid" != "$sid" ]]; then
    exit 2
fi
```

中文伪代码：worker 确认自身同时是 PID、PGID 和 SID leader，原子发布 ACK；随后执行 Makefile 提供的 bootstrap，再调用 `eda01_entry.sh batch_run`。退出 trap 原子写 rc，controller 由 rc 和 simulation log 判定终态。

### 5.2 `launch_seed()`

抽象功能描述：controller 非阻塞启动带唯一 token 的远端 worker，处理 SSH 返回不确定、ACK NFS 延迟和 ACK 超时发现，不重复启动同一 seed。

源码位置：`run_eda_regression.sh`，函数：`launch_seed()`

```bash
remote_cmd="exec nohup env $env_cmd setsid $worker_cmd > ... 2>&1 < /dev/null"
ssh "${SSH_OPTS[@]}" -f -n "$FINAL_REMOTE_HOST" "$remote_cmd"
```

中文伪代码：run token 在 `setsid` 前进入初始环境，由 worker、make 和 simv 继承。controller 不重发 launch；本地 ACK 不可见时先 SSH 读取 ACK，再按 token 发现远端 PGID。唯一 PGID可接管，没有 PGID记 INFRA，无法确认则 `REMOTE_UNKNOWN + quarantine`。

正确性检查：真实 SSH mock 中两个 worker 并发启动；结束后按 token 发现均无残留进程。

## 6. Early-stop 与清理

### 6.1 `start_watcher()`

抽象功能描述：每个 active seed 使用独立增量 watcher，只统计实际 UVM error/fatal report，并向 controller 写 trigger；它不直接 kill 仿真。

源码位置：`run_eda_regression.sh`，函数：`start_watcher()`

```bash
if [[ "$line" =~ ^[[:space:]]*UVM_(ERROR|FATAL)[[:space:]]+@ ]]; then
    count=$((count + 1))
fi
```

中文伪代码：`tail -n +1 --retry -F` 等待唯一 simulation log 并逐行消费；summary、demoted/caught 统计不匹配该表达式。达到阈值后原子写 trigger。watcher 使用 FIFO 和 cleanup trap，controller 回收 seed 时同时终止 tail 子进程。

### 6.2 `stop_all_workers()`

抽象功能描述：联停只处理当前批次已验证 token 的 PGID，先 TERM、后 KILL，并确认非 zombie 成员消失；无法确认时 quarantine。

源码位置：`run_eda_regression.sh`，函数：`signal_active_groups()`、`stop_all_workers()`、`process_group_has_token()`

中文伪代码：controller 把 active seed/token/PGID 写入请求文件，远端 helper 枚举当前 UID 的 PGID 非 zombie 成员，并在成员环境中确认 run token。身份匹配后发送信号；只剩 zombie 按已退出处理；token 不匹配或 SSH 重试耗尽时不盲目 kill，改记 `REMOTE_UNKNOWN`。

正确性检查：mock seed 101 写入一个 `UVM_ERROR @` 后，seed 101 被联停为 `ABORTED`，seed 103 未启动；两个已启动 seed 的 token 在 eda01 均无残留，本地 watcher 无残留。

## 7. 状态记录与 NFS Recovery

### 7.1 `write_progress()`

抽象功能描述：按 controller 内存状态原子刷新 testcase 的计划数、完成数、剩余数、运行数和 seed 分类，不扫描完整 simulation log 推导调度状态。

固定公式：

```text
completed = PASS + FAIL
settled   = PASS + FAIL + INFRA + INCOMPLETE + ABORTED + REMOTE_UNKNOWN
remaining = planned - completed
```

`--no-record` 已验证只保留 `config.txt`、`seeds.txt`、内部状态和最终 `summary.log`，不生成动态 progress/results 分类文件。

### 7.2 NFS Recovery

抽象功能描述：共享新文件本地不可见时，通过 SSH 只读同一路径，避免把缓存延迟误判为远端失败。

源码位置：`run_eda_regression.sh`，函数：`recover_ack_over_ssh()`、`recover_rc_over_ssh()`、`prepare_classification_log()`

中文伪代码：ACK/rc 读取成功后原子回填同一控制文件；simulation log 不可见时把远端内容保存为 status 下的 snapshot用于分类，但 results 仍记录原始日志路径。远端读取也失败后才进入 INFRA 或 token discovery。

该逻辑由真实 VCS 运行验证：首轮两个 seed 的 simulation log 均 PASS，但旧实例因 rc 本地不可见误记 INFRA；加入 rc recovery 后 no-compile 双 seed均正确分类 PASS。

## 8. 验证结果

### 8.1 Mock 验证

| 场景 | 结果 |
|---|---|
| 两并发 + UVM error early-stop | 通过；1 ABORTED、1 PASS、1 not-run |
| `--no-monitor` | 通过；1 FAIL、1 PASS，无全局联停 |
| `--no-record` | 通过；最终 PASS，仅保留最小记录 |
| 非空 output directory | 通过；返回 2 |
| mode lock 被占用 | 通过；返回 4 |
| wave/coverage fingerprint 不匹配 | 通过；返回 5 |
| 用户中断清理 | 通过；远端任务进入 ABORTED，无 token 残留 |

### 8.2 真实 eda01/VCS 验证

编译 mode：`eda_regression_acceptance_20261019_v1`

1. 使用 `--compile --wave --coverage` 完成一次 VCS compile，生成 fingerprint。
2. 编译后的 seed 666666、666667 实际日志均包含 `TEST CASE PASSED`、`UVM_ERROR : 0`、`UVM_FATAL : 0` 和 `$finish`；该轮用于发现并修复 rc 的 NFS 可见性问题。
3. 修复后使用同一 mode 和 `--no-compile --wave --coverage` 并发运行 seed 666668、666669。
4. 最终 summary：`planned=2 completed=2 remaining=0 pass=2`。
5. 两个 FSDB 大小分别约 1.22 MB，均非空。
6. coverage 数据库分别存在 `seed=666668` 和 `seed=666669` 的 testdata 目录。
7. 运行结束后两个 run token 在 eda01 均无进程残留，无 mode quarantine。

结果目录：

```text
mem_ut/ver/ut/memblock/sim/.eda_regression/acceptance_nocompile_wave_cov_20261019_v2
```

## 9. Plan 对齐检查

### 9.1 实现与 Plan 不一致项

未发现未记录的实现与 Plan 不一致项。bootstrap/ACK 顺序、NFS recovery、旧 procps/zombie 兼容和结果目录保护均已回写 plan 的 `IMPLEMENTATION_DELTA` 章节。

### 9.2 Plan 未说明但 Coding 落实的细节

未发现未回写的额外逻辑；coding 中新增的必要工程细节均已写入 `IMPLEMENTATION_DELTA`。

## 10. 非本次修改的逻辑分析

当前工作区在本任务开始前已存在多类无关修改，包括：

- `AGENTS.md`、RTL 知识库规则和其他 AI 文档修改。
- L2TLB、MMIO、replay 等其他 plan/review 文档。
- 多个 memblock sequence/common helper 的功能修改。
- Verdi/novas 生成文件删除和其他仿真产物。

这些内容不属于本脚本需求，未被回滚、暂存或纳入本次功能正确性结论。本 review 只覆盖第 2 章列出的文件。

## 11. 最终结论

实现满足用户参数、compile-once、并发、FSDB、代码覆盖率、verbosity、early-stop、动态记录和 debug 要求。静态检查、mock 控制链和真实 eda01/VCS 双 seed 验收均已完成，无已知 blocker。
