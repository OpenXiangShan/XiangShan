# mem_ut 并发回归仿真执行规则

## 1. 适用范围

本文规定主 agent 使用以下脚本执行编译一次、多 seed 并发回归、失败联停、根因分析、缺陷修复和
恢复回归时的完整闭环：

```text
mem_ut/ver/ut/memblock/sim/run_eda_regression.sh
```

执行前必须同时阅读：

- `AI_DOC/project_management/mem_ut_parallel_eda_regression_usage_rule.md`
- `AI_DOC/project_management/buglist_bug_record_rule.md`
- `AI_DOC/project_management/code_modification_authorization_rule.md`

脚本参数和结果文件以脚本使用说明为准；缺陷责任分类、Buglist 登记和 coding 授权以 Buglist 规则
及本文的优先级说明为准。

## 2. 参数解析和默认值

主 agent 根据用户提示词设置脚本参数。用户没有明确指定的可选参数使用脚本默认值，但 testcase
按本规则使用项目默认值：

```text
--tc basicTest
```

除非用户明确指定其它值，回归执行至少遵守：

- `--cfg default`
- `--count 1`
- `--jobs 20`
- `--no-compile`
- `--no-wave`
- `--no-coverage`
- `--verbosity UVM_LOW`
- `--monitor`
- `--record`
- `--error-limit 1`

用户要求首次编译、重新编译、源码修改后复测，或现有 `simv`/fingerprint 不满足请求时，必须显式
使用 `--compile`。用户指定 seed 后，以显式 seed 数量作为仿真数量；不得再用 `--count` 改变调度数量。

自动恢复依赖 `results.tsv` 和 seed 清单，因此本文规定的批量回归不得使用 `--no-record`。除非用户
明确要求关闭提前联停，否则不得使用 `--no-monitor`。

## 3. 启动和持续监控

主 agent 必须从以下目录启动脚本：

```bash
cd mem_ut/ver/ut/memblock/sim
```

回归启动后不得只做后台触发并立即结束任务。主 agent 必须持续保留 controller 会话并监控脚本，直到
脚本退出、完成远端进程收敛并发布最终结果。

主 agent 判断整批成功时必须同时满足：

1. 脚本退出码为 `0`。
2. `summary.log` 中 `result=PASS`。
3. `results.tsv` 中全部计划 seed 为 `PASS`。
4. `summary.log` 中 `remote_unknown_jobs=0`，且不存在 `INFRA`、`INCOMPLETE`、`ABORTED` 或 `PENDING`。

仅看到终端输出中的 PASS 字样不足以认定整批成功。

## 4. 失败后的状态提交和分类

脚本退出非零或监控触发联停后，主 agent 必须先等待脚本完成以下动作，再开始波形复现或下一轮仿真：

1. 提交根因候选 seed 的状态记录。
2. 停止并确认本批次远端活动进程。
3. 生成稳定最终日志。
4. 发布 `summary.log` 和 `results.tsv`。
5. 释放对应 mode lock。

结果检查顺序为：

1. `summary.log`：检查整批结果、`stop_reason`、各状态数量和远端未知进程数。
2. `results.tsv`：检查所有 seed 的最终分类。
3. `monitor_events.tsv`：仅在 monitor trigger 时检查触发联停的首错候选 seed。
4. `monitor_failure.committed`：仅在 monitor trigger 时检查根因记录是否先于联停信号提交。
5. `signal_term.sent` 和 `signal_kill.sent`：确认实际执行过的远端联停动作。

`monitor_events.tsv` 中的 seed 只表示达到阈值并触发联停的首错候选，不等于已经证明是 DUT/RTL 根因。
责任类别必须经过日志、波形、源码分析和独立 review 后确定。

## 5. 无波形失败的专项复现

原回归未开启波形时，使用首错候选的原 testcase、cfg、virtual sequence、seed、用户 plusargs 和
timeout 进行单 seed debug 复现。默认允许复用原 mode 并重新编译，从而覆盖原 mode 的编译产物：

```bash
./run_eda_regression.sh \
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

覆盖前必须确认：

- 原 controller 已完全退出。
- mode lock 已释放。
- `remote_unknown_jobs=0`。
- 原结果目录、`config.txt`、`effective_plusargs.txt` 和原 fingerprint 信息仍保留。

如果需要保留原 `simv` 基线，可以改用独立 `<原mode>_debug` mode，但必须在分析记录中说明。波形复现
没有出现原始错误时，只能进入 `PAUSED_ANALYSIS`，不得依据不同现象直接登记确认缺陷或修改代码。

## 6. 根因分析、subagent 和责任分类

获取根因候选日志和可用波形后，必须启动 subagent 对测试环境和 DUT 进行独立根因分析或 review；
主 agent 仍须对日志、波形、源码位置和责任类别进行独立复核，不得只转述 subagent 结论。

在正式登记 Buglist 前必须先完成责任分类 review：

- RM/测试框架问题至少完成一轮独立责任分类 review。
- RTL 问题必须满足 Buglist 规则要求；要以确认 RTL 缺陷为理由停止回归时，至少完成两轮独立 review。
- 证据不足或 reviewer 结论不一致时进入 `PAUSED_ANALYSIS`，不得提前登记为确认缺陷。

## 7. Buglist 登记、方案和 coding 授权

责任分类 review 确认后，按版本、类别和 ISO 周扫描现有 Buglist，再登记或更新对应章节。正式登记后
编写最小修复方案，并对修复方案完成独立 review。

在实际批量回归已经触发 Buglist 缺陷闭环时，coding 授权以 Buglist 规则为主，通用代码修改授权规则
为辅：

- RM：责任分类和方案 review 通过后，自动获得当前批量回归内的 RM coding 授权。
- 测试框架：必须有当前任务明确的测试框架 coding 授权。
- RTL/Scala/Chisel/DUT：必须有当前任务明确的 RTL coding 授权，不得自动修改。

授权仅覆盖当前批量回归、已确认责任类别和对应缺陷，不得跨类别、跨任务或扩展到无关源码。
未进入 Buglist 回归闭环的单次仿真、静态分析、普通开发和其它代码修改，仍以通用代码修改授权规则为主。

subagent 没有对应 coding 授权时只能执行分析或 review，不得修改代码。

## 8. 修复后的专项复测

修改任何编译进 `simv` 的 RTL、Scala/Chisel 生成输入、SystemVerilog、UVM 或 RM 代码后，必须重新
执行 `--compile`，不得复用修改前的 `simv`。

修复后首先使用原 testcase、cfg、virtual sequence、根因 seed、用户 plusargs 和 timeout 进行专项复测。
debug 波形复现通过不能自动替代原回归参数复测；需要按原回归参数确认该 seed 最终为 `PASS`。

专项复测失败时继续分析当前错误，不得恢复剩余回归。专项复测通过后，根据 Buglist 规则和当前授权
状态决定是否可以恢复后续批量回归，并同步更新 Buglist 的修复和验证状态。

## 9. 被联停任务的恢复规则

脚本没有 `--resume` 参数，也不允许复用非空 `--output-dir`。恢复必须创建新的空结果目录，并通过
显式 seed 启动新批次。

恢复 seed 集合按以下规则构造：

```text
恢复 seed 集合 = 原 ABORTED seed + 为原 not_run 数量重新构造的新 seed
```

- `ABORTED`：必须使用原 seed 重新执行。
- `not_run`：原 seed 尚未执行，可以废弃，并重新构造相同数量的新随机 seed。
- `FAIL`：根因 seed 必须先完成原 seed 专项复测，不得混入未修复的恢复批次。
- `INFRA/INCOMPLETE`：排除基础设施或日志问题后，原则上使用原 seed 重跑。
- 新构造 seed 不得与历史 `PASS/FAIL/ABORTED/INFRA/INCOMPLETE`、当前恢复 seed 或其它新 seed 重复。
- 恢复前后 testcase、cfg、virtual sequence、用户 plusargs、timeout 和计划执行总数必须保持一致。
- 不得直接使用 `failed_seeds.txt` 作为恢复列表，因为其中混有不同失败类别。

主 agent 必须记录首轮和每轮恢复结果目录、原计划数量、已通过 seed、原 seed 重跑项、新 seed 替换项
和剩余数量，直到整个回归任务收敛。该跨轮状态由主 agent 维护在首轮结果目录的：

```text
campaign_state.tsv
```

该文件不是脚本自动生成文件，至少包含 `round`、`result_dir`、`planned_total`、`pass_total`、
`retry_original_seeds`、`replacement_new_seeds`、`remaining`、`campaign_status` 和 `updated_at` 字段。
每轮恢复启动前和结束后都必须更新，且不能覆盖各轮脚本原始结果文件。

## 10. 回归状态和终止条件

回归任务状态统一使用：

| 状态 | 含义 |
|---|---|
| `COMPLETE` | 整个回归任务计划数量均已完成，最终结果全部通过。 |
| `PAUSED_AUTH` | 已确认问题和方案，但等待对应类别 coding 授权。 |
| `PAUSED_INFRA` | 远端连接、license、磁盘、编译环境或远端进程状态无法确认。 |
| `PAUSED_ANALYSIS` | 波形未复现、证据不足或责任类别尚未确认。 |
| `PAUSED_BY_USER` | 用户明确要求暂停，并保留恢复条件。 |
| `STOPPED_RTL_CONFIRMED` | 至少两轮独立 review 确认 RTL 缺陷并按规则停止后续回归。 |
| `STOPPED_BY_USER` | 用户明确要求终止回归任务。 |

只有 `COMPLETE` 表示回归成功完成。所有 `PAUSED_*` 状态必须保留结果目录、待执行 seed、恢复参数和
暂停原因，不得报告为完成。

## 11. 最终检查清单

主 agent 结束或暂停回归前至少确认：

1. 所有已启动脚本和远端进程均已收敛，或已明确进入 `PAUSED_INFRA`。
2. 每轮 `summary.log`、`results.tsv` 和结果目录均已记录。
3. 首错候选、根因确认结论、`ABORTED` 和 `not_run` 没有混淆。
4. 新构造 seed 没有与历史执行 seed 重复，计划执行总数没有变化。
5. Buglist 登记、方案 review、coding 授权和专项复测顺序符合本文规则。
6. 最终状态是本文定义的状态之一，并向用户说明继续执行所需条件。

## 12. 维护要求

后续修改回归默认参数、监控和成功判定、波形复现方式、责任分类、coding 授权优先级、seed 恢复策略、
跨轮状态字段或终止状态时，必须同步检查：

- `AI_DOC/project_management/mem_ut_parallel_eda_regression_usage_rule.md`
- `AI_DOC/project_management/buglist_bug_record_rule.md`
- `AI_DOC/project_management/code_modification_authorization_rule.md`
- `AGENTS.md`

修改后至少执行：

```bash
git diff --check -- AI_DOC/project_management AGENTS.md
rg -n "mem_ut_parallel_regression_execution_rule|basicTest|PAUSED_ANALYSIS|Buglist 例外" \
  AGENTS.md AI_DOC/project_management
```
