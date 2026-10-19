# memblock 远端并发仿真脚本需求与实现方案

状态：`do`，plan review、coding、implementation review 和真实 eda01/VCS 双 seed 验收已完成
创建日期：2026-10-19
适用版本：V2 `mem_ut_uvm_v2`
目标脚本：`mem_ut/ver/ut/memblock/sim/run_eda_regression.sh`

## 1. Plan 定位、专有名词与抽象功能说明

本 plan 属于 memblock 仿真任务编排脚本方案，不修改 DUT、UVM transaction、sequence、driver、monitor、RM 或 scoreboard 行为。脚本复用现有远端仿真链路：本地控制器通过 SSH 调用共享目录中的 `eda01_entry.sh`，远端最终执行 `make compile` 或 `make batch_run`。

### 1.1 专有名词

| 英文术语 | 当前文档中的中文含义 | 代码对象或落点 | 使用场景/示例 |
|---|---|---|---|
| `controller` | 在当前节点运行的总控进程，负责解析参数、生成 seed、调度并发、监控和汇总 | `run_eda_regression.sh` 默认执行模式 | `--jobs 20` 时最多维护 20 个远端任务 |
| `remote worker` | 在 `eda01` 上执行单个 seed 仿真的进程 | 同一脚本的内部 `--remote-worker` 模式 | 一个 worker 只执行 seed `12345` |
| `process group` | 单个远端 worker 及其 `make/simv` 子进程组成的进程组 | `setsid` 创建，PID 文件记录组长 | 联停时按负 PGID 终止整组进程 |
| `early stop` | 某个运行中 seed 的 UVM 错误数达到阈值后，停止本批次全部任务 | monitor watcher 和总控 stop 文件 | `--error-limit 1` 时首个 UVM error 触发联停 |
| `explicit seed mode` | 用户显式给出一个或多个 seed，仿真数量由 seed 数量决定 | `--seed`、`--seeds` | `--seeds 11,22,33` 固定运行 3 次 |
| `generated seed mode` | 用户未指定 seed，脚本按 `--count` 生成互不重复的随机 seed | seed 生成函数 | `--count 100` 生成 100 个 seed |
| `result directory` | 保存本批次清单、状态、PID、launcher log 和汇总记录的目录 | 默认 `.eda_regression/<run_id>` | 用于恢复分析和定位失败 seed |
| `simulation log` | VCS 通过 `-l` 写入的单 seed UVM 日志 | `${mode}/log/tc=...seed=...log` | monitor 以增量方式观察该文件 |
| `run token` | 本批次和单 seed 绑定的随机身份串，用于确认 PID 仍属于当前 worker | ACK 文件和远端 worker 命令行 | PID 重用后 token 不匹配，controller 禁止 kill |
| `compile fingerprint` | 描述某个 mode 下 simv 编译能力的配置快照 | `${mode}/exec/.eda_compile_fingerprint` | 校验 coverage/wave 是否与已有 simv 匹配 |
| `mode lock` | 覆盖一个 mode 整个批次生命周期的排他锁 | `.eda_regression/locks/<mode>.lock` | 防止并发批次覆盖 simv 或 coverage 数据 |
| `REMOTE_UNKNOWN` | SSH 连续失败后无法确认远端进程是否仍存活的终态 | seed 状态和最终 summary | 不宣称已清理，要求人工登录远端确认 |

### 1.2 关键函数抽象功能

`parse_args()`：在 controller 启动阶段解析用户参数、应用默认值和 debug 派生默认值，并拒绝互相冲突或非法的输入；它不负责启动 SSH 或仿真。

`build_seed_list()`：根据显式 seed 或仿真次数生成最终唯一 seed 列表，并写入结果目录；显式 seed 模式不读取 `--count`。

`compile_once()`：在首次编译开关开启时，通过现有 `make eda_compile` 远端链路完成一次编译；它不启动任何 seed 仿真。

`launch_seed()`：为一个 seed 建立独立远端 process group、PID/返回码文件、launcher log 和本地 monitor watcher；它不阻塞等待该 seed 完成。

`monitor_seed_log()`：增量读取单个运行中 seed 的 simulation log，只统计实际 `UVM_ERROR/UVM_FATAL` report 行；达到阈值时写入全局 early-stop 触发文件。

`classify_seed()`：在 worker 结束后结合返回码、测试通过标记和 UVM summary，把 seed 分类为 `PASS`、`FAIL`、`INFRA`、`INCOMPLETE` 或 `ABORTED`。

`stop_all_workers()`：只针对本批次记录的远端 process group 发送 `TERM`，等待后再发送 `KILL`；它不得使用宽泛 `pkill simv` 影响其他用户或其他批次。

`write_progress()`：根据内存中的计数和每个 seed 的终态，原子刷新专用进度日志、成功 seed 文件、失败 seed 文件和 TSV 结果表。

`controller_loop()`：在 mode lock 持有期间维持并发上限，统一处理 worker ACK、完成、失联、early stop、watcher 回收和新任务补充；它是唯一允许推进 seed 状态的主循环。

`probe_active_workers()`：通过一次批量 SSH 探测所有 active worker 的 PID、PGID、SID、run token 和存活状态；它只返回事实，不直接修改状态或终止进程。

`cleanup_controller()`：由 `INT/TERM/EXIT` trap 调用，幂等停止 watcher、终止已确认属于本批次的远端进程组并释放 mode lock；重复调用不得误杀或重复改写终态。

## 2. 用户需求与参数定义

### 2.1 必须支持的参数

| 参数 | 默认值 | 语义 |
|---|---:|---|
| `--tc <name>` | 无，必填 | 测试用例名，对应 make 参数 `tc` |
| `--cfg <name>` | `default` | cfg 名，不带 `.cfg` 后缀 |
| `--count <n>` | `1` | generated seed mode 的仿真次数 |
| `--seed <n>` | 无 | 可重复指定单个 seed |
| `--seeds <list>` | 无 | 逗号或空白分隔的多个 seed |
| `--jobs <n>` | `20` | 最大并发仿真数 |
| `--compile` / `--no-compile` | `--no-compile` | 是否先远端编译一次 |
| `--wave` / `--no-wave` | `--no-wave` | 是否开启 FSDB 波形 |
| `--coverage` / `--no-coverage` | `--no-coverage` | 是否开启 VCS 代码覆盖率 `ccov=on` |
| `--verbosity <level>` | `UVM_LOW` | UVM 日志严重度/详细度 |
| `--monitor` / `--no-monitor` | `--monitor` | 是否开启运行中 UVM error/fatal 联停监控 |
| `--error-limit <n>` | `1` | 单个 seed 的 error+fatal 数量达到该值时联停 |
| `--record` / `--no-record` | `--record` | 是否动态刷新专用记录日志 |
| `--debug` | 关闭 | 未显式覆盖时自动设置 `wave=on`、`verbosity=UVM_DEBUG` |

### 2.2 工程兼容参数

脚本同时支持以下可选参数，以便适配现有 make flow，但不改变用户要求的默认行为：

| 参数 | 默认值 | 语义 |
|---|---:|---|
| `--ts <name>` | `virtual_base_sequence` | make 参数 `ts` |
| `--mode <name>` | `base_fun` | 编译产物和日志目录 |
| `--timeout-ns <n>` | `100000000` | UVM timeout |
| `--remote-host <host>` | `eda01` | 远端 EDA 节点；默认主机不可达时回退 `172.28.10.101` |
| `--output-dir <path>` | 自动生成 | 本批次 result directory |
| `--poll-interval <sec>` | `1` | controller 状态轮询周期 |
| `--launch-timeout <sec>` | `30` | 等待 remote worker ACK 的最长时间 |
| `--probe-interval <sec>` | `5` | 批量探测 active worker 存活状态的周期 |
| `--ssh-retries <n>` | `3` | probe/kill SSH 操作的最大尝试次数；launch 不自动重发 |
| `--dry-run` | 关闭 | 只校验参数、生成并显示 seed 和命令，不连接远端 |

### 2.3 参数优先级和约束

1. 只要出现 `--seed` 或 `--seeds`，立即进入 explicit seed mode，`--count` 不参与仿真数量计算。
2. 所有 seed 必须是 `1..2147483646` 的十进制整数；重复 seed 去重并保持首次出现顺序。
3. `--count`、`--jobs`、`--error-limit`、`--timeout-ns` 和 `--poll-interval` 必须为正整数。
4. `--verbosity` 只接受 `UVM_NONE/UVM_LOW/UVM_MEDIUM/UVM_HIGH/UVM_FULL/UVM_DEBUG`。
5. `--debug` 只提供默认派生：用户未显式指定 wave 或 verbosity 时，分别设为开启和 `UVM_DEBUG`；显式参数优先。
6. `--coverage --no-compile` 必须校验 compile fingerprint 中 `ccov=on`，不匹配时 fail-fast，不能只打印提示后继续。
7. `--wave --no-compile` 必须校验 compile fingerprint 中 `wave=on`；无 fingerprint 的历史 simv 只允许 `wave=off/coverage=off` 的普通仿真。
8. `tc/ts/cfg/mode` 只接受字母、数字、下划线、点和连字符；output path 使用绝对路径和逐参数 shell quoting，不拼接未转义命令。
9. 显式 seed 与 `--count` 同时出现时打印 `--count ignored because explicit seeds were provided`。
10. `--count`、`--jobs`、`--error-limit`、`--timeout-ns`、`--poll-interval`、`--launch-timeout`、`--probe-interval` 和 `--ssh-retries` 必须为正整数。
11. 脚本帮助信息和文件头注释必须列出所有参数、默认值和常见用法。

## 3. 目标功能 Flow 总览

```text
解析并校验参数
  -> 解析一次最终 remote host，后续编译和 worker 全部复用该 host
  -> 建立 result directory
  -> 生成/整理最终 seed 列表
  -> 非阻塞获取当前 mode 的排他锁；已被占用则打印持有者并失败退出
  -> 远端连通性、bootstrap、共享脚本和 simv/fingerprint 检查
  -> 如果 compile=on：调用 make eda_compile 一次
  -> 初始化进度记录
  -> 按 jobs 上限启动 remote worker
  -> controller 循环处理：
       worker 完成 -> 分类并回收 -> 启动下一个 seed
       monitor 达到 error-limit -> 设置 early stop -> 停止全部 worker
       用户 SIGINT/SIGTERM -> 停止全部 worker并记录 ABORTED
  -> 刷新最终 summary、成功 seed、失败 seed和未运行 seed
  -> 全部 PASS 返回 0，否则返回非 0
```

编译和仿真命令关系：

```text
首次编译：
  local make eda_compile
    -> remote_eda_make.sh
    -> ssh eda01
    -> eda01_entry.sh compile
    -> make compile

每个 seed 仿真：
  local controller ssh eda01
    -> REMOTE_BOOTSTRAP
    -> setsid run_eda_regression.sh --remote-worker
    -> worker 发布 PID/PGID/SID/run token ACK
    -> eda01_entry.sh batch_run <make assignments>
    -> make batch_run
    -> 已编译 simv
```

## 4. 文件和输出设计

### 4.1 源码文件

新增：

```text
mem_ut/ver/ut/memblock/sim/run_eda_regression.sh
```

脚本使用同文件内部模式承载 remote worker，不新增临时远端 helper 源码。

修改：

```text
mem_ut/ver/ut/memblock/sim/Makefile
```

Makefile 只新增只读目标 `eda_print_bootstrap`，输出当前唯一的 `REMOTE_BOOTSTRAP` 值。controller 必须通过该目标读取 bootstrap，禁止在新脚本中复制 module load 字符串。

### 4.2 结果目录

默认目录：

```text
mem_ut/ver/ut/memblock/sim/.eda_regression/<tc>_<cfg>_<timestamp>_<pid>/
```

固定内容：

| 文件/目录 | 内容 |
|---|---|
| `config.txt` | 本次解析后的全部配置 |
| `seeds.txt` | 最终 seed 顺序 |
| `status/<seed>.state` | 单 seed 当前/最终状态 |
| `status/<seed>.rc` | 远端 worker 返回码，完成时原子写入 |
| `status/<seed>.ack` | worker 发布的 PID、PGID、SID 和 run token |
| `pids/<seed>.pid` | controller 校验 ACK 后保存的远端 process group leader PID |
| `launcher/<seed>.log` | SSH/worker 启动与 make 标准输出 |
| `monitor/<seed>.trigger` | 单 seed 达到 UVM error/fatal 阈值的证据 |
| `results.tsv` | seed、状态、返回码、UVM 计数和 simulation log 路径 |
| `progress.log` | 动态覆盖更新的总数、完成数、剩余数、运行数、成功/失败 seed |
| `success_seeds.txt` | 已通过 seed |
| `failed_seeds.txt` | 失败、基础设施错误或被联停 seed |
| `not_run_seeds.txt` | early stop 后尚未启动的 seed |
| `summary.log` | 最终汇总和联停原因 |

内部控制文件不受 `--record` 控制：`config.txt`、`seeds.txt`、`status/`、`pids/`、`launcher/` 和 `monitor/` 始终生成，因为它们承担身份校验、状态推进和清理职责。`--no-record` 只关闭面向用户的动态产物：不持续生成 `progress.log`、`success_seeds.txt`、`failed_seeds.txt`、`not_run_seeds.txt` 和 `results.tsv`；结束时仍生成一份最小 `summary.log` 并输出终端摘要。

计数公式固定如下：

```text
planned   = seeds.txt 中唯一 seed 数量
running   = 当前状态为 RUNNING 的数量
completed = PASS + FAIL
settled   = PASS + FAIL + INFRA + INCOMPLETE + ABORTED + REMOTE_UNKNOWN
not_run   = 状态仍为 PENDING 且调度已经结束的数量
remaining = planned - completed
```

`completed` 只表示已经得到正常仿真通过或失败结论的次数，不把联停、基础设施错误或远端未知状态计入“仿真完成次数”。early stop 后 `remaining` 包含 `ABORTED`、`INFRA`、`INCOMPLETE`、`REMOTE_UNKNOWN` 和 `not_run`，因此最终可能非零；`summary.log` 同时列出 `settled/not_run` 和所有异常类别。

## 5. 主流程实现方案

### 5.1 `parse_args()`

抽象功能描述：在任何文件创建和 SSH 操作前得到唯一、可执行的配置快照，保证后续主循环不再处理参数歧义。

源码级伪代码：

```text
初始化默认值和“用户是否显式设置”标志
逐项解析长参数
遇到 --seed 时追加一个原始 seed
遇到 --seeds 时按逗号/空白拆分后追加
应用 debug 派生默认值
校验 tc、数值范围、verbosity 和 cfg 名
规范化布尔值和路径
```

中文文字伪代码：先设置 `jobs=20`、`count=1`、关闭编译/波形/覆盖率、开启 monitor/record、`verbosity=UVM_LOW` 等默认值。随后按命令行顺序读取参数，把 seed 参数暂存到列表，并记录 wave、verbosity 是否由用户显式设置。解析完成后，若 debug 开启且用户没有显式覆盖，则打开波形并切换到 `UVM_DEBUG`。最后统一校验所有必填项和数值范围，任一非法输入都在启动远端任务前失败。

### 5.2 `build_seed_list()`

抽象功能描述：把用户意图转换为最终仿真任务清单，保证每个 seed 最多运行一次且顺序稳定。

源码级伪代码：

```text
如果原始 seed 列表非空：
  逐个校验并按首次出现顺序去重
否则：
  从 /dev/urandom 生成 count 个合法随机整数
  碰到重复值则重新生成
把最终列表写入 seeds.txt
```

中文文字伪代码：显式 seed 模式只消费用户给出的 seed，不读取仿真次数；每个 seed 通过关联数组去重。generated seed mode 从系统随机源读取 32 位数，映射到合法正整数范围，并在关联数组中确认未出现后加入任务列表，直到数量达到 `count`。

### 5.3 `compile_once()`

抽象功能描述：在仿真调度开始前建立一份与 wave/coverage 选项匹配的共享 simv，失败时阻止所有 seed 启动。

源码级伪代码：

```text
如果 compile=off：返回
执行 make -C <sim_dir> eda_compile \
  REMOTE_HOST=... tc=... ts=... cfg=... mode=... \
  seed=<首个seed> wave=<on/off> ccov=<on/off> pl=...
检查返回码
失败则记录 COMPILE_FAIL 并退出
成功后原子写 compile fingerprint：mode/timing/wave/ccov/simv path/commit
fingerprint 绑定 simv SHA-256、device/inode/size/mtime
```

中文文字伪代码：首次编译开关关闭时先检查目标 simv 是否存在；普通无波形、无覆盖率模式可使用无 fingerprint 的历史 simv，要求波形或覆盖率时必须由 fingerprint 证明编译能力匹配。开关开启时先删除旧 fingerprint，再只调用一次现有 `eda_compile`。编译成功后计算 simv SHA-256 和稳定文件身份并原子写入 fingerprint，失败时旧 fingerprint 不会继续被信任。`--no-compile` 读取 fingerprint 后重新计算 hash 和文件身份，任何不一致都 fail-fast。

### 5.4 `launch_seed()` 与 remote worker

抽象功能描述：非阻塞启动一个可独立追踪、可精确终止的远端 seed 仿真，并建立 controller 所需的状态文件。

源码级伪代码：

```text
controller:
  状态写为 RUNNING
  生成批次唯一 note 和单 seed run token
  计算本 seed 的 simulation log、ack、pid、rc、launcher 路径
  ssh remote_host：
    先执行与 remote_eda_make.sh 相同的 REMOTE_BOOTSTRAP
    nohup env EDA_REGRESSION_RUN_TOKEN=<run token> \
      setsid <same-script> --remote-worker <quoted worker args> \
      > launcher.log 2>&1 < /dev/null &
  launch SSH 不自动重发；即使 SSH 返回失败也先等待同 token ACK
  在 launch-timeout 内等待 ACK
  ACK 超时后通过 SSH 按 run token 枚举远端进程组：
    找到唯一合法 PGID：补建/校验 ACK 后接管；无法接管则终止并确认消失
    确认没有 token PGID：记 INFRA
    SSH 不可达或结果不唯一：记 REMOTE_UNKNOWN 并 quarantine mode
  校验 ACK token、PID=PGID、SID=PGID 且远端 cmdline 含当前 token
  校验失败则记 INFRA，不进入 active 集合
  如果 monitor=on：启动本地 monitor watcher

remote worker:
  获取自身 PID、PGID、SID
  校验 PID=PGID 且 SID=PGID
  校验初始环境中的 EDA_REGRESSION_RUN_TOKEN 与参数 token 一致
  原子发布 token/PID/PGID/SID ACK
  调用 eda01_entry.sh batch_run 和完整 make assignments
  note=<batch_unique_note>，保证 simulation log 唯一
  EXIT trap 原子写入 rc 文件
  以同一返回码退出
```

中文文字伪代码：controller 先为 seed 生成不可复用的 run token，再通过 SSH 执行从 Makefile `eda_print_bootstrap` 读取的 EDA bootstrap。token 在 `setsid` 执行前通过 `env` 放入 worker 初始环境，确保 worker 自身及后续 make/simv 都能从 `/proc/*/environ` 被识别。launch 命令不自动重发。无论 SSH 返回值如何，controller 都先在 timeout 内等待同 token ACK；ACK 超时后必须再按 token 远端发现。发现唯一合法 PGID 时补建 ACK 并接管，或在无法接管时安全终止；确认不存在 token PGID 后才记 `INFRA`。SSH 不可达或发现多个不一致 PGID 时记 `REMOTE_UNKNOWN` 并 quarantine mode，禁止留下未受管理任务后继续复用该 mode。

### 5.5 `monitor_seed_log()`

抽象功能描述：在仿真仍运行时增量观察 UVM report，达到阈值后通知 controller；它不负责直接杀进程或判定最终 PASS。

源码级伪代码：

```text
确认 simulation log 使用批次唯一 note，旧文件不存在
tail -n +1 --retry -F simulation_log，从首行开始读取
对每一新行：
  如果匹配 ^[[:space:]]*UVM_(ERROR|FATAL)[[:space:]]+@：count++
  如果 count >= error_limit：
    原子写 monitor/<seed>.trigger
    退出 watcher
worker 完成或联停时 controller kill 并 wait watcher
```

中文文字伪代码：每个运行中 seed 对应一个轻量 watcher。批次唯一 note 保证目标日志没有上一轮残留；`tail -n +1 --retry -F` 从新日志首行开始并等待文件创建。watcher 只匹配实际 `UVM_ERROR @` 或 `UVM_FATAL @` report，summary、demoted/caught 统计不会计数。达到阈值后写 trigger 并退出；seed 正常完成时 controller 必须显式 kill/wait watcher，避免后台进程泄漏。

### 5.6 `classify_seed()`

抽象功能描述：在 seed 结束后给出稳定终态，供最终返回码和成功/失败 seed 清单使用。

源码级伪代码：

```text
如果由全局 stop 终止且没有正常完成：ABORTED
如果 simulation log 不存在：INFRA
如果日志包含 license、simv 缺失、磁盘或 VCS 内部错误：INFRA
提取最终 UVM_ERROR/UVM_FATAL summary
如果任一 summary 非零或存在实际 error/fatal report：FAIL
如果日志包含 TEST CASE PASSED/TEST_PASS 且 $finish 且 rc=0：PASS
如果 rc!=0：FAIL
否则：INCOMPLETE
```

中文文字伪代码：先识别是否被本批次联停，避免把主动终止误记成普通失败。simulation log 缺失固定归类为 `INFRA`；日志存在但没有正常结束标记且未发现明确工具错误时归类为 `INCOMPLETE`。正常结束时同时检查 UVM 错误、测试通过标记、`$finish` 和远端返回码；只有通过标记完整、UVM 错误为零且返回码为零时才记为 `PASS`。

### 5.7 `stop_all_workers()`

抽象功能描述：在 early stop、用户中断或 controller 异常退出时收敛本批次所有远端进程，不触碰其他批次。

源码级伪代码：

```text
遍历 active seed：
  读取并核对 ACK token/PID/PGID/SID
  ssh remote_host：
    枚举 PGID 的全部成员
    检查至少一个成员的 /proc/<member>/environ 含当前 EDA_REGRESSION_RUN_TOKEN
    检查 kill -0 -- -<pgid> 成功
    仅 token 成员校验通过后 kill -TERM -- -<pgid>
等待有限时间
再次检查仍存活的 pgid
再次核对 token 后，对仍存活组执行 kill -KILL -- -<pgid>
停止本地 watcher
把未正常完成的 active seed 标记 ABORTED
```

中文文字伪代码：controller 只遍历当前 active seed 的 ACK。每次发送信号前都枚举目标 PGID 的成员，并检查至少一个成员继承了当前 run token。即使 worker 组长异常消失，只要 make/simv 成员仍保留同一 PGID 和 token，就可以安全清理整组；Linux 在组内仍有成员时不会把该 PGID 分配给新组。若组为空则视为已退出；若组存在但没有 token 成员，禁止 kill 并记为 `REMOTE_UNKNOWN`。身份仍匹配时先发送 `TERM`，有限等待后再次核对并发送 `KILL`，最终必须确认整个 PGID 不存在。

### 5.8 `write_progress()`

抽象功能描述：向用户提供指定 testcase 的计划次数、完成次数、剩余次数、运行数以及成功/失败 seed 快照，不参与调度决策。

源码级伪代码：

```text
从内存状态计数 planned/running/completed/remaining/pass/fail/aborted/not_run
写 progress.log.tmp
写入 testcase/cfg/mode、更新时间、成功和失败 seed
mv 为 progress.log
同步刷新 success_seeds.txt、failed_seeds.txt、results.tsv
```

中文文字伪代码：每次 seed 状态变化和固定 poll 周期结束时，根据 controller 已维护的状态数组直接计数，不通过反复扫描 simulation log 推导进度。所有面向用户的记录先写临时文件再原子替换，避免读取者看到半行或不一致的汇总。

### 5.9 `probe_active_workers()`

抽象功能描述：周期性确认 active worker 仍属于本批次且远端进程组存在，使“无 rc 异常退出”能在有限时间内收敛。

源码级伪代码：

```text
每 probe-interval 秒构造 active seed 的 pid/pgid/token 清单
通过一次 SSH 批量执行：
  对每项枚举 PGID 成员并检查继承的环境 token
返回每个 seed 的 ALIVE/EXITED/IDENTITY_MISMATCH
如果 EXITED 且 rc 不存在：终止 watcher并记 INFRA
如果 IDENTITY_MISMATCH：禁止 kill，终止 watcher并记 REMOTE_UNKNOWN
SSH 失败按 ssh-retries 重试；全部失败则设置全局 remote_unknown
```

中文文字伪代码：controller 不为每个 seed 单独建立高频 SSH，而是按固定周期把所有 active worker 合并成一次远端探测。远端通过 PGID 成员继承的环境 token 确认身份；进程组已经消失但没有返回码时归类为 `INFRA`。组存在但 token 不匹配时归类为 `REMOTE_UNKNOWN`。批量 SSH 最多重试 `ssh_retries` 次；全部失败后不再宣称远端任务已经清理，而是设置全局远端未知状态并进入受控退出。

### 5.10 `controller_loop()`

抽象功能描述：在持有 mode lock 的整个仿真阶段内，作为唯一状态推进者维持并发、完成回收、失联检测和 early stop。

源码级伪代码：

```text
while 存在 RUNNING，或 stop_requested=0 且存在 PENDING：
  首先检查已有 monitor trigger；命中则立刻 stop_requested=1
  回收已有 rc 的 worker：
    如果 monitor=on，在停止 watcher前同步扫描最终日志 error/fatal 数量
    monitor=on 且达到 error-limit 时 stop_requested=1
    停止并 wait watcher，classify 并写终态
  到 probe 周期时调用 probe_active_workers
  再次检查 trigger 或 remote_unknown：
    设置 stop_requested=1
    stop_all_workers
    保留未启动 seed 为 PENDING/not_run
  如果未 stop_requested：每次 launch 前再次检查 trigger，然后补充一个任务
  write_progress（record=on 时）
  sleep poll_interval
结束后写 summary 并按是否全 PASS 返回
```

中文文字伪代码：主循环只在仍有运行任务，或尚未联停且仍有待启动任务时继续。每轮最先检查 trigger，避免上一轮末尾已经失败时继续补任务。回收完成 worker 时，仅在 monitor 开启时同步统计最终日志；这样快速失败仍能触发联停，而 `--no-monitor` 只把该 seed 分类为失败，不停止其他 seed。stop 请求置位后不再因 PENDING 项保持循环，未启动项直接作为 not_run 留给最终 summary。

### 5.11 `cleanup_controller()`

抽象功能描述：统一处理正常退出、用户中断和异常退出的资源释放，保证多次调用结果一致。

源码级伪代码：

```text
如果 cleanup_done=1：return
cleanup_done=1
停止并 wait 全部 watcher
对仍 active worker 调用 stop_all_workers
关闭 mode lock fd
```

中文文字伪代码：cleanup 首先用布尔标志防止重复执行。随后停止所有本地 watcher，并对仍处于 active 集合且身份校验通过的远端进程组执行联停。最后释放 mode lock。正常主循环完成前先清空 active 集合，因此正常 EXIT trap 不会重复终止已结束任务。

## 6. 并发、性能与生命周期约束

1. controller 只维护不超过 `jobs` 个 active seed；完成一个才补充一个。
2. simulation log 监控使用每 seed 一个增量 watcher，不在高频 poll 中全量 grep 大日志。
3. seed 状态只允许按 `PENDING -> RUNNING -> 终态` 单向推进；终态不回退。
4. early stop 触发后禁止启动新 seed；未启动项记入 `not_run_seeds.txt`。
5. 所有 PID、返回码和 progress 文件使用临时文件加 `mv` 原子发布。
6. `trap` 覆盖 `INT/TERM/EXIT`，异常退出时仍执行本批次进程组清理。
7. 同一 `mode` 从编译前到最后一个 worker 回收期间必须使用 `flock -n` 非阻塞持有排他 mode lock。锁已占用时立即失败，并打印 sidecar 中的 host、PID、tc、cfg、run_id 和启动时间，不无限等待。不同 mode 的批次可并存。启动时如果存在 `<mode>.quarantine`，无条件拒绝进入该 mode。
8. 批次 note 使用 `eda_reg_<timestamp>_<pid>_<random>`，传入每个 seed，使 simulation log 唯一。
9. 最终 remote host 只在启动阶段解析一次：先检查 `eda01`，仅默认 host 不可达时回退 `172.28.10.101`；compile、launch、probe 和 kill 全部复用该值。
10. simv 基线必须在本批次 compile 成功并写完 fingerprint之后，或 no-compile fingerprint/历史 simv 校验完成之后建立；不得在 compile 前建立。每次 launch 前重新比较 device/inode/size/mtime，发现外部直接 `make compile` 替换产物时立即停止启动并归类为基础设施错误。最终再比较一次并在 summary 记录是否发生外部修改。
11. probe/kill SSH 操作最多重试 `ssh_retries` 次。kill 失败后不得把 seed 标为 `ABORTED`；无法确认远端 PGID 消失时必须标为 `REMOTE_UNKNOWN`，并原子创建 `<mode>.quarantine`，记录 host、run_id、seed、token、PGID 和时间。quarantine 不随 controller 退出删除；人工确认远端进程和产物安全后才能手工删除，删除前新批次不得进入该 mode。

## 7. 帮助信息与用法示例

脚本文件头和 `--help` 至少包含以下示例：

```bash
# 使用已有 simv，随机运行 100 次，最大并发 20
./run_eda_regression.sh --tc basicTest --cfg default --count 100

# 先编译一次，再运行 50 次并收集代码覆盖率
./run_eda_regression.sh --tc basicTest --cfg default --count 50 \
  --compile --coverage --jobs 10

# 指定一个 seed；count 被忽略
./run_eda_regression.sh --tc basicTest --cfg default --seed 123456

# 指定多个 seed
./run_eda_regression.sh --tc basicTest --cfg default --seeds 11,22,33

# debug 模式默认打开 FSDB 和 UVM_DEBUG
./run_eda_regression.sh --tc basicTest --cfg default --seed 123456 --debug

# 关闭 early-stop monitor 或动态记录
./run_eda_regression.sh --tc basicTest --cfg default --count 20 \
  --no-monitor --no-record
```

## 8. 验证方案与验收标准

### 8.1 静态和参数测试

1. `bash -n run_eda_regression.sh` 通过。
2. `--help` 返回 0，列出全部参数和默认值。
3. `--dry-run --count 3` 生成 3 个合法且唯一 seed。
4. `--dry-run --count 99 --seeds 1,2,2,3` 最终只运行 `1,2,3`，证明 count 失效且 seed 去重。
5. 非法 jobs、count、seed、verbosity 和缺少 tc 均返回非 0，且不连接 SSH。
6. debug 模式 dry-run 显示 `wave=on`、`verbosity=UVM_DEBUG`；显式 `--no-wave --verbosity UVM_LOW` 可覆盖。

### 8.2 mock 并发测试

通过临时 fake `ssh`/worker 环境验证：

1. 最大 active 数不超过 `--jobs`。
2. 多个 seed 正常完成时进度计数正确。
3. 某个 mock log 写入达到阈值的 UVM error 后，其他 worker 收到联停并标为 `ABORTED`，未启动 seed 标为未运行。
4. `--no-monitor` 时相同错误不触发提前联停，只在任务结束后分类失败。
5. `--no-record` 时不持续生成 progress/results 分类文件。
6. fake SSH/entry 模拟 ACK 超时、进程退出无 rc、simulation log 缺失，controller 均在有限时间内归类 `INFRA`；token 不匹配归类 `REMOTE_UNKNOWN`。
7. 重复执行 cleanup，不重复 kill、不改变已有终态。
8. fake SSH 连续失败达到重试上限后进入 `REMOTE_UNKNOWN`，summary 不宣称远端进程已清理。
9. `--no-monitor` 下已完成失败 seed 不触发其他任务联停。
10. launch SSH 模拟“远端已启动但本地返回失败”，确认同一 token 只产生一个 PGID。
11. ACK 超时后分别模拟：发现唯一 token PGID并接管、确认无 PGID归类 INFRA、SSH 不可达归类 REMOTE_UNKNOWN并 quarantine。

### 8.3 远端实际验证

第一组验证编译一次、两个 batch_run、FSDB 和覆盖率。使用独立 mode，避免覆盖现有 `base_fun`：

```bash
./run_eda_regression.sh \
  --tc tc_sanity \
  --ts virtual_base_sequence \
  --cfg default \
  --seeds 666666,666667 \
  --jobs 2 \
  --mode eda_regression_acceptance_20261019 \
  --compile \
  --wave \
  --coverage
```

验收标准：

1. launcher/compile log 证明只执行一次 compile，随后两个 seed 均执行 `batch_run`，不重复编译。
2. 同时运行数不超过 2。
3. `progress.log` 记录计划 2、完成 2、剩余 0，以及成功/失败 seed。
4. 对通过 seed，simulation log 含通过标记，`UVM_ERROR=0`、`UVM_FATAL=0`。
5. 两个 seed 均生成非空 FSDB；coverage 数据库中存在两个不同 seed 的测试实例。
6. compile fingerprint 记录 `wave=on` 和 `ccov=on`；同 mode 的 `--no-compile --wave --coverage` 可通过校验，改为不匹配配置时 fail-fast。

第二组使用仅供测试的环境变量 `EDA_REGRESSION_REMOTE_ENTRY` 指向共享目录中的临时 fake entry，并真实通过 SSH/setsid 在 `eda01` 运行，用于验证进程管理，不改变正常用户接口：

1. fake entry 为第一个 seed 写入一个 `UVM_ERROR @` 后保持运行，其他 seed 保持运行。
2. controller 触发 early stop 后，远端检查所有本批次 PGID 均不存在，未启动 seed 进入 not_run。
3. fake entry 模拟进程退出但不写 rc，controller 在下一个 probe 周期内归类 `INFRA`。
4. fake entry 不创建 simulation log，controller 不永久等待，worker 完成后固定归类 `INFRA`。
5. 使用 `--no-record` 运行 mock，确认只保留内部控制文件和最终最小 summary。
6. fake SSH 连续失败达到重试上限时，seed 标为 `REMOTE_UNKNOWN`，summary 不宣称 PGID 已清理。
7. `REMOTE_UNKNOWN` 创建 mode quarantine；新批次被拒绝，人工删除 quarantine 后才允许继续。

## 9. Review 检查点

1. 是否完整覆盖用户要求的 testcase、cfg、次数、seed、并发、编译、波形、覆盖率、verbosity、monitor、阈值、动态记录和 debug 参数。
2. 是否真正实现“编译一次，后续只运行 batch_run”。
3. early stop 是否只杀本批次 process group，不影响其他仿真。
4. monitor 是否增量读取日志，避免周期性全量扫描。
5. explicit seed mode 是否完全忽略 count。
6. 代码覆盖率是否同时传入编译和仿真阶段。
7. trap、SSH 失败、编译失败、worker 启动失败和日志缺失是否都有终态。
8. 记录功能关闭后是否仍保留控制所必需的最小状态。
9. 实际验证是否覆盖至少两个并发 seed。
10. bootstrap、ACK/token、mode lock、fingerprint、worker probe 和 watcher 回收是否全部有验证证据。

## 10. 执行中补充/修正（IMPLEMENTATION_DELTA）

### 10.1 Worker ACK 与 bootstrap 顺序

[IMPLEMENTATION_DELTA]
来源：真实 mock 运行发现 eda01 module bootstrap 约需十余秒，如果在 `setsid` 和 ACK 前执行，controller 会逐个等待 bootstrap，降低并发启动和 early-stop 响应速度。
原 plan：远端先执行 bootstrap，再启动 worker 并发布 ACK。
实现调整：controller 先通过 `setsid` 启动带 run token 的 worker；worker 立即发布 PID/PGID/SID ACK，随后在同一进程组内执行从 Makefile 读取的 bootstrap，bootstrap 成功后再调用 `eda01_entry.sh batch_run`。
原因：进程组必须先进入可管理状态，EDA 环境仍必须在 VCS/simv 调用前完成初始化。
影响范围：只改变 worker 启动顺序，不改变 bootstrap 内容、make 参数或 compile-once/batch_run 契约。

### 10.2 共享 NFS 可见性延迟恢复

[IMPLEMENTATION_DELTA]
来源：eda01 已原子写出 ACK、rc 或 simulation log 时，当前节点可能在数秒内仍通过 `-f` 看不到新文件。
原 plan：controller 直接读取共享目录中的 ACK、rc 和 simulation log。
实现调整：本地文件不可见时，通过 SSH 在 eda01 只读 `cat` 同一路径，并原子回填 ACK/rc 或生成本批次 status 目录中的 log snapshot；只有远端读取也失败后才进入 token 发现或 `INFRA`。
原因：避免把 NFS 属性缓存延迟误判为 worker 启动失败、无 rc 退出或日志缺失。
影响范围：不修改原始仿真日志和产物；results 仍记录原始 simulation log 路径。

### 10.3 eda01 旧 procps 与 zombie 进程组处理

[IMPLEMENTATION_DELTA]
来源：eda01 的旧版 `ps -u <numeric_uid>` 返回结果不稳定；进程组只剩 zombie 时，`kill -0` 仍可能返回存在，但 zombie 的环境已不可读取。
原 plan：使用 `ps -u` 枚举当前用户进程，并以 `kill -0` 和环境 token 判断存活。
实现调整：统一使用 `ps -e -o uid,pid,pgid,sid,stat` 后在脚本内显式比较 UID；probe/signal 先排除只剩 zombie 的进程组，再检查非 zombie 成员的 run token。
原因：兼容 eda01 工具版本，并避免成功联停后的 zombie 窗口被误判为 token mismatch。
影响范围：只影响远端身份探测和清理判断，不改变被仿真进程。

### 10.4 结果目录和最终 simv 身份保护

[IMPLEMENTATION_DELTA]
来源：coding 自查需要防止用户显式复用非空 output directory，以及防止最后一个 seed 运行期间外部 compile 替换 simv 后仍返回成功。
原 plan：result directory 唯一，launch 前检查 simv 身份。
实现调整：显式 output directory 非空时 fail-fast；批次结束时再次比较 simv identity，发生变化时最终返回非零。
原因：避免覆盖历史记录，并补齐整个批次生命周期的产物一致性检查。
影响范围：仅新增保护性失败条件。

## 11. 与初步 plan 差异说明

本任务没有独立的初步 plan。第一轮正式 plan review 后补充了以下实现约束：

1. remote worker 必须复用现有远端 EDA bootstrap，并在启动阶段固定最终 remote host。
2. 远端进程管理由单纯 PID 文件升级为 ACK、PID/PGID/SID 和 run token 身份协议，增加周期性存活探测。
3. simulation log 使用批次唯一 note，watcher 固定采用 `tail -n +1 --retry -F` 并在 seed 结束时回收。
4. 新增 compile fingerprint；coverage/wave 的 no-compile 模式不匹配时 fail-fast。
5. 同一 mode 的完整批次使用排他锁，避免 simv 和 coverage 并发覆盖。
6. 明确 `--no-record` 只关闭用户记录产物，不关闭内部控制状态，并定义 completed/remaining/not_run 公式。
7. 验证方案增加真实 compile-once、FSDB、coverage 和远端 fake worker 联停/失联测试。
8. controller loop 改为先检查 trigger、回收时同步复核日志、每次 launch 前再次检查，关闭 early-stop 竞态。
9. run token 通过环境继承到 make/simv；组长消失后仍可依据 PGID 成员 token 安全清理。
10. bootstrap 改为从 Makefile 新增的 `eda_print_bootstrap` 只读目标获取，不复制硬编码。
11. fingerprint 绑定 simv SHA-256 和文件身份；mode lock 固定采用非阻塞 fail-fast 策略，并检测外部编译替换。
12. 完成次数只统计 PASS/FAIL；SSH 无法确认清理时使用 `REMOTE_UNKNOWN` 终态。
13. early-stop 循环条件改为“仍有 RUNNING，或未 stop 且仍有 PENDING”；`--no-monitor` 不做全局联停复核。
14. token 在 `setsid` 前进入初始环境，launch 不自动重发；simv 基线改在编译或校验完成后建立。
15. `REMOTE_UNKNOWN` 会持久 quarantine 当前 mode，人工确认前禁止新批次复用。
16. launch ACK 超时后新增 token 发现流程，必须确认无孤儿 PGID、接管/终止已发现 PGID，或进入 `REMOTE_UNKNOWN + quarantine`。
