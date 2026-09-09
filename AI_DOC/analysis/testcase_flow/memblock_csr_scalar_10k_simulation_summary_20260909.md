# memblock CSR scalar 10K 随机用例仿真总结

## 1. 结论摘要

本轮在 `mem_ut_uvm_v2` 分支、V2 profile 和 commit
`c909ccb1b mem_ut: add staged CSR configuration and stress sequences` 上，完成了四套
CSR 动静态随机配置场景的远端 VCS 编译和 10,000 笔业务激励仿真。四个用例均打印
`TEST_PASS`，未出现 `UVM_ERROR` 或 `UVM_FATAL`，也没有超时、死锁或未完成 transaction。

逐项结论如下：

1. 当前 commit 的 CSR initial/dynamic sequence 能适配现有 V2 UVM 环境。初始化配置、
   动态配置、main-table worker、L2TLB responder、issue、writeback 和 RM 能共同完成完整生命周期。
2. CSR 配置没有引入功能性仿真失败。四个场景均能跑完 10,000 笔业务；动态切换会形成预期的
   drain/barrier 流量间隔，并会暴露现有 `default_sequence` 与 L2TLB no-progress 诊断 warning，
   但这些 warning 未升级为 error，也未阻止仿真结束。
3. 激励已正常打入。详细日志用例中观测到精确的 10,000 次 load issue、10,000 次 DUT load
   writeback、10,011 次 RM compare PASS，以及 7,310 次需要 L2TLB responder 的翻译完成。
4. CSR 配置已实际生效。详细日志中 initial CSR 完整配置提交一次，10 个 dynamic marker 均完成
   随机化并发送；`satp` mode/ASID、permission 和 privilege/virtualization context 在不同 epoch
   中出现不同取值，RM 看到的 `csr_seq` 也随配置推进。

因此，本轮单 seed 功能验证结论为通过。该结论证明当前四个确定 seed 下的环境兼容性和端到端
功能正确性，不等价于多 seed 随机分布收敛或完整覆盖率签核。

根据后续要求，本报告还记录了第二轮带全层级 FSDB 的重新编译和仿真。第二轮使用四个新 seed，
四个用例再次全部 `TEST_PASS`，并各自产生了可由 `fsdbreport` 正常读取的独立 FSDB 文件。

## 2. 验证对象与环境

| 项目 | 内容 |
| --- | --- |
| 分支 | `mem_ut_uvm_v2` |
| 版本 profile | V2 |
| 被测 commit | `c909ccb1b` |
| 仿真节点 | `172.28.10.101`，远端 hostname 为 `eda01` |
| VCS | `synopsys/vcs/Q-2020.03-SP2` |
| Verdi | `synopsys/verdi/R-2020.12-SP1` |
| 仿真入口 | `mem_ut/ver/ut/memblock/sim` |
| 首轮独立产物目录 | `mem_ut/ver/ut/memblock/sim/csr_sequence_10k_validation_20260909` |
| FSDB 重跑产物目录 | `mem_ut/ver/ut/memblock/sim/csr_sequence_10k_fsdb_20260909_rerun` |
| 波形 | 首轮关闭；第二轮对四个用例打开全层级 FSDB 与 MDA dump |
| 代码覆盖率/功能覆盖率 | 本轮未作为验收项 |

主机名 `eda01` 在当前节点无法解析，SSH 返回
`Could not resolve hostname eda01`。依据项目规则改用 `ssh 172.28.10.101` 后连接成功，
远端确认 hostname 仍为 `eda01`，共享工作目录可见。

当前 commit 已经包含与本轮目标完全对应的四套 main sequence、virtual sequence 和 cfg，
所以本轮直接复用并验证这些用例，没有重复增加等价类：

- `seq/csr_scalar_stress/main_sequence`
- `seq/csr_scalar_stress/virtual_sequence`
- `seq/csr_scalar_stress/cfg`

## 3. 用例设计与结构审计

四个用例均继承
`memblock_csr_scalar_stress_main_sequence_base`。该基类在 main table 构建后和仿真结束前分别审计：

- 业务 transaction 必须精确为 10,000 笔；
- CSR marker 数必须与用例期望一致；
- 必须且只能有一个末尾 `check_store`；
- final audit 时所有 transaction 必须进入 terminal 状态；
- 任一数量或状态不符都会执行 `uvm_fatal`。

因此，即使低 verbosity 日志没有打印 `UVM_INFO` audit 明细，`TEST_PASS` 仍表示相应的强制
audit 已通过。

| 用例 | 业务类型 | initial CSR 重点 | dynamic CSR 组 | marker 间隔/数量 | final table 审计 |
| --- | --- | --- | --- | --- | --- |
| `memblock_csr_scalar_load_10k_vseq` | 10,000 笔 scalar integer load | `satp=Sv39`；支持的静态 capability 打开 | `satp`、permission、privilege context | 1000/10 | 10000 + 10 + 1 = 10011 |
| `memblock_csr_scalar_store_10k_vseq` | 10,000 笔 scalar store | `vsatp=Sv39`、`hgatp=Sv39x4`；store 向静态 capability | `vsatp`、`hgatp`、permission、privilege context | 834/12 | 10000 + 12 + 1 = 10013 |
| `memblock_csr_scalar_mixed_10k_vseq` | 10,000 笔 load/store 混合流量 | `satp/vsatp/hgatp=Bare`；支持的静态开关等权随机 | 六组全开：`satp/vsatp/hgatp`、permission、privilege context、PMP/PMA | 625/16 | 10000 + 16 + 1 = 10017 |
| `memblock_csr_scalar_store_sta_std_10k_vseq` | 10,000 笔严格 STA/STD store 流量 | `satp=Bare`；store/writeback 向静态 capability | `satp`、privilege context、PMP/PMA | 400/25 | 10000 + 25 + 1 = 10026 |

严格 STA/STD 场景额外设置 `MEMBLOCK_STA_REAL_WB_PASS_EN=1`，用于覆盖真实 STA writeback
通过路径；V2 环境中的 STD 仍走真实 writeback 路径。

mixed 场景同时打开六组 dynamic CSR 和 PMP/PMA exception region 随机属性，是本轮对动态
配置组合最宽的环境兼容性检查。各场景都把无关的 L2 response fault、probe 和 reorder 关闭，
从而使失败更容易归因到 CSR 配置而不是独立随机故障。

## 4. 编译问题与修正

第一轮 VCS 编译能解析新增 CSR sequence，但在 elaboration 阶段找不到
`array_7_ext`～`array_12_ext` memory cell。原因是 V2 的 `build/rtl/filelist.f` 包含
`array_*.sv` wrapper，却没有包含 `build/rtl/*_ext.v` 生成 memory model。

本轮在 `mem_ut/ver/ut/memblock/cfg/rtl.f` 中显式加入当前生成的 31 个 memory model：

- `array_ext.v`；
- `array_0_ext.v`、`array_0_0_ext.v`、`array_0_1_ext.v`、`array_0_2_ext.v`；
- `array_1_ext.v`～`array_26_ext.v`。

不能在 VCS filelist 中直接使用 `$MEMBLOCK_XS_HOME/build/rtl/*_ext.v`，因为本次工具调用没有
展开该 wildcard。改为逐文件列出后编译通过，`vcs_compile_rtl.log` 明确记录了上述 model 的
解析过程，最终生成：

```text
mem_ut/ver/ut/memblock/sim/csr_sequence_10k_validation_20260909/exec/simv
```

该修正属于既有 V2 RTL filelist 的完整性问题，不是 CSR sequence 本身的语法、factory 注册或
接口适配问题。

## 5. 仿真结果

| 场景 | seed | 结果 | 仿真时间 | CPU 时间 | warning | error/fatal |
| --- | ---: | --- | ---: | ---: | ---: | ---: |
| load 10K | 710105 | `TEST_PASS` | 719080.100 ns | 463.750 s | 17 | 0/0 |
| store 10K | 710106 | `TEST_PASS` | 692537.700 ns | 412.720 s | 19 | 0/0 |
| mixed 10K | 710107 | `TEST_PASS` | 652612.600 ns | 586.360 s | 18 | 0/0 |
| strict STA/STD 10K | 710108 | `TEST_PASS` | 650807.900 ns | 400.250 s | 15 | 0/0 |

load 用例使用 `UVM_LOW` 保留详细证据，UVM report summary 直接给出
`UVM_ERROR : 0`、`UVM_FATAL : 0`。其余三个 10K 用例使用 `UVM_NONE` 控制日志体积；
`basicTest::report_phase()` 只有在全局 `UVM_FATAL + UVM_ERROR == 0` 时才打印
`TEST_PASS`，所以表中三个低 verbosity 用例的 0/0 由该判定保证。

### 5.1 load 10K 详细证据

load 用例 final audit 为：

```text
phase=final profile=SCALAR_LOAD_10K business_count=10000
csr_marker_count=10 check_store_count=1 table_count=10011 nonterminal_count=0
```

日志计数结果为：

| 证据 | 计数 | 判断 |
| --- | ---: | --- |
| initial CSR profile solve | 1 | initial CSR 随机化启动正常 |
| `initial complete CSR configuration committed` | 1 | 初始完整配置成功提交 |
| dynamic CSR profile solve | 10 | 与 10 个 CSR marker 精确对应 |
| `dynamic CSR candidate staged and sent` | 10 | 10 次动态配置均完成发送 |
| `dispatch issue fire lda_port=` | 10000 | 10,000 笔 load 激励均实际进入 issue |
| `RM_LS_TRACE_DUT_LOAD` 的 `DUT_LOAD_WB` | 10000 | 10,000 笔 load 均观测到 DUT writeback |
| `RM_LS_COMPARE ... compare PASS` | 10011 | 整张 main table 的 RM compare 均通过 |
| `complete L2TLB token=` | 7310 | 需要地址翻译的流量正常进入并完成 L2TLB responder |

动态配置分别锚定在业务进度 UID 1000、2000、……、10000。日志中的随机值实际发生变化，
例如 `satp` 在 Bare/Sv39/Sv48 之间变化，ASID、`mxr/sum/vmxr/vsum`、`virt`、`imode`、
`dmode` 也出现不同组合。RM 的 translation 日志观测到推进后的 `csr_seq`，说明运行期 snapshot
被消费，而不是只完成 sequence 本地随机化。

### 5.2 其余三个用例的证据边界

store、mixed 和 strict STA/STD 用例均完成 `TEST_PASS`，且没有任何
`UVM_ERROR @` 或 `UVM_FATAL @`。这些用例的 main sequence 在 final audit 中会对业务数、marker
数、末尾 `check_store`、table 总数和 terminal 状态执行 fatal 级强制检查，所以可确认各自
10,000 笔业务与 12/16/25 次动态 CSR action 都完成。

由于这三个用例使用 `UVM_NONE`，日志没有保留每个 action 的 `UVM_INFO` 逐条明细。本报告没有
把“final fatal audit 通过”扩大解释为逐拍波形检查。

### 5.3 带 FSDB 的重新编译和仿真结果

第二轮新建独立目录 `csr_sequence_10k_fsdb_20260909_rerun`，使用 `wave=on`、KDB 和
`debug_access+all` 重新编译。编译耗时为 156.367 秒 compile、1.538 秒 elaboration 和
1.533 秒 link；Verdi KDB 报告 0 error、0 warning。

远端 Verdi module 只设置了 `PATH`，没有设置 FSDB UCLI 所需的 `VERDI_HOME`。第一次波形启动
因此报 `UCLI-FSDB-LOAD-FAIL`，尚未进入 10K 主仿真。本轮将
`VERDI_HOME=/nfs/tools/synopsys/verdi/R-2020.12-SP1` 通过本轮命令的 `REMOTE_BOOTSTRAP` 显式导出，
随后从 load 场景重新运行。最终四份有效日志中均不存在该报错。本轮没有据此修改公共
`sim/Makefile` 默认 bootstrap。

| 场景 | seed | 结果 | 仿真时间 | CPU 时间 | warning | FSDB 大小 |
| --- | ---: | --- | ---: | ---: | ---: | ---: |
| load 10K | 720105 | `TEST_PASS` | 626417.600 ns | 517.590 s | 18 | 343150574 B，约 327 MiB |
| store 10K | 720106 | `TEST_PASS` | 655132.600 ns | 416.080 s | 15 | 185109311 B，约 177 MiB |
| mixed 10K | 720107 | `TEST_PASS` | 647698.100 ns | 538.660 s | 23 | 385915118 B，约 368 MiB |
| strict STA/STD 10K | 720108 | `TEST_PASS` | 684863.300 ns | 409.240 s | 22 | 211231484 B，约 201 MiB |

每份日志均满足以下条件：

- 包含一次 `Create FSDB file` 和一次 `TEST_PASS`；
- 不包含 `UVM_ERROR @`、`UVM_FATAL @` 或 `UCLI-FSDB-LOAD-FAIL`；
- 对应 FSDB 文件存在且非空。

另外在 `eda01` 上使用 Verdi `fsdbreport` 分别读取四份 FSDB 的 `/top_tb/clk`，四次返回码均为 0
并能输出 0～2 ns 的信号记录，确认文件不是仅创建文件名的空壳或损坏文件。

## 6. 对验证问题的逐一分析

### 6.1 CSR 动静态配置 sequence 是否适配当前环境

结论：适配。

- 四个 VSEQ 均通过 factory/白名单选择并启动；
- CSR sequencer 的 owner 顺序为 main-table/bootstrap、initial CSR、dynamic CSR worker，未发生
  多 owner 冲突；
- 业务 producer 在 initial config 完成后启动，dynamic marker 与业务进度关联；
- runtime CSR snapshot 能被 L2TLB 与 RM 消费；
- 四类流量都到达 final terminal audit。

编译阶段发现并修正的是 V2 memory model filelist 缺项，不属于 CSR sequence 接口不适配。

### 6.2 是否会影响仿真

结论：没有观察到功能性负面影响，但存在可解释的运行期开销和 warning 噪声。

- 四个用例没有 error/fatal、timeout、deadlock、遗留 transaction 或 RM mismatch；
- CSR action 前后的 drain/barrier 会暂停新业务并等待旧业务收敛，这是动态配置保持 epoch 一致性
  所需的预期行为，会增加仿真墙钟/CPU 时间；
- 每个用例有 12 条 “no default_sequence” warning。当前拓扑由显式 VSEQ 统一启动各 agent
  sequence，这类 warning 不表示 agent 未工作；
- 其余 warning 是 L2TLB responder 在业务间隙输出的 no-progress diagnostic：load/store/mixed/
  STA-STD 分别为 5/7/6/3 条。message 自身说明 owner 保持 active 直到 global stop，最终所有场景
  正常退出。

### 6.3 激励是否正常打入

结论：正常。

load 详细日志给出精确 10,000 次 issue fire、10,000 次 DUT load writeback 和 10,011 次 RM
compare PASS；同时能看到 L2TLB request/response completion。其余三个场景通过 fatal 级结构和
terminal audit，证明各自精确 10,000 笔业务完成，而不是只生成了 cfg 或停留在 main table 中。

### 6.4 静态配置和动态配置是否都被覆盖

结论：都被覆盖。

- 静态配置：每个 VSEQ 都先运行一次 `memblock_csr_initial_config_sequence`，不同 cfg 固定或随机
  选择 startup ATP mode、功能 enable 和 privilege context；
- 动态配置：四个 cfg 分别覆盖 SATP、VSATP、HGATP、permission、privilege context、PMP/PMA
  的不同组合；mixed 用例覆盖全部六组；
- load 详细日志证明 1 次 initial commit 和 10 次 dynamic stage/send；其余场景由 marker/action
  数量的 final fatal audit 保证完整结束。

## 7. Warning 与剩余限制

本轮 warning 均为现有诊断，不影响 pass 判定，但建议后续回归报表把以下两类 warning 单独归类，
避免掩盖新的异常：

1. 显式 VSEQ owner 拓扑下的 agent `no default_sequence`；
2. CSR barrier 或普通业务空窗期间的 L2TLB responder no-progress diagnostic。

本轮仍有以下边界：

- 每个场景只跑一个固定 seed，尚未证明多 seed 下的随机稳定性和分布收敛；
- 第二轮已保留四份可读 FSDB，但本报告只验证了文件完整性与基础信号可读性，尚未对
  `changed` pulse 和全部 CSR 字段逐拍执行自动波形比对；
- 本轮未把 coverage 数字作为验收项；
- 三个 `UVM_NONE` 用例没有逐条 INFO 证据，精确计数由 sequence 内 fatal audit 和最终
  `TEST_PASS` 共同保证。

## 8. 复现命令

在主仿真目录执行，使用 IP 可绕过当前节点的 `eda01` DNS 问题：

```bash
cd mem_ut/ver/ut/memblock/sim

make eda_compile \
  REMOTE_HOST=172.28.10.101 \
  REMOTE_BOOTSTRAP='source /usr/share/Modules/init/bash >/dev/null 2>&1 || true; module load synopsys/vcs/Q-2020.03-SP2 license; module load synopsys/verdi/R-2020.12-SP1 license; export VERDI_HOME=/nfs/tools/synopsys/verdi/R-2020.12-SP1' \
  mode=csr_sequence_10k_fsdb_20260909_rerun \
  tc=basicTest \
  ts=memblock_csr_scalar_load_10k_vseq \
  plus_file=../seq/csr_scalar_stress/cfg \
  cfg=tc_csr_scalar_load_10k \
  timing=rtl wave=on ccov=off fcov=off partcmp_op=off
```

编译一次后可用 `eda_batch_run` 复用 `simv`。以下是 load 场景示例，其他场景替换 `ts`、`cfg`、
`seed` 和 `note`：

```bash
make eda_batch_run \
  REMOTE_HOST=172.28.10.101 \
  REMOTE_BOOTSTRAP='source /usr/share/Modules/init/bash >/dev/null 2>&1 || true; module load synopsys/vcs/Q-2020.03-SP2 license; module load synopsys/verdi/R-2020.12-SP1 license; export VERDI_HOME=/nfs/tools/synopsys/verdi/R-2020.12-SP1' \
  mode=csr_sequence_10k_fsdb_20260909_rerun \
  tc=basicTest \
  ts=memblock_csr_scalar_load_10k_vseq \
  plus_file=../seq/csr_scalar_stress/cfg \
  cfg=tc_csr_scalar_load_10k \
  seed=720105 pl=UVM_NONE \
  note=load_10k_fsdb \
  timing=rtl wave=on ccov=off fcov=off
```

## 9. 产物索引

独立产物根目录：

```text
mem_ut/ver/ut/memblock/sim/csr_sequence_10k_validation_20260909
```

关键日志：

```text
log/vcs_compile_rtl.log
log/tc=basicTest_ts=memblock_csr_scalar_load_10k_vseq_cfg=tc_csr_scalar_load_10k_seed=710105_rtl_load_10k.log
log/tc=basicTest_ts=memblock_csr_scalar_store_10k_vseq_cfg=tc_csr_scalar_store_10k_seed=710106_rtl_store_10k.log
log/tc=basicTest_ts=memblock_csr_scalar_mixed_10k_vseq_cfg=tc_csr_scalar_mixed_10k_seed=710107_rtl_mixed_10k_clean.log
log/tc=basicTest_ts=memblock_csr_scalar_store_sta_std_10k_vseq_cfg=tc_csr_scalar_store_sta_std_10k_seed=710108_rtl_store_sta_std_10k_clean.log
```

FSDB 重跑产物根目录：

```text
mem_ut/ver/ut/memblock/sim/csr_sequence_10k_fsdb_20260909_rerun
```

该目录下的 `log` 保存重新编译日志和四份仿真日志，`wave` 保存一一对应的四份 `.fsdb` 文件，
`exec` 保存本轮带 KDB/FSDB 支持重新生成的 `simv`。
