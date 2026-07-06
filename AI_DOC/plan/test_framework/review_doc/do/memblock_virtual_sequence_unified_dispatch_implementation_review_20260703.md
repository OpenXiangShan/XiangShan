# memblock virtual sequence 统一调度实现 Review

关联 plan：

- `AI_DOC/plan/test_framework/plan/do/memblock_virtual_sequence_unified_dispatch_plan_20260703.md`

验证结果：

- `make eda_compile tc=basicTest ts=virtual_base_sequence mode=base_fun wave=off`：通过。
- `make eda_batch_run tc=basicTest ts=virtual_base_sequence mode=base_fun wave=off`：通过，`TEST_PASS`，`UVM_WARNING=14`，`UVM_ERROR=0`，`UVM_FATAL=0`。
- `make eda_compile tc=basicTest ts=memblock_dispatch_real_smoke_vseq mode=base_fun cfg=tc_dispatch_real_smoke wave=off`：通过。
- `make eda_batch_run tc=basicTest ts=memblock_dispatch_real_smoke_vseq mode=base_fun cfg=tc_dispatch_real_smoke wave=off`：通过，日志显示 `usr_test_vseq_name:memblock_dispatch_real_smoke_vseq`，`memblock_dispatch_real_smoke_vseq` 启动，`TEST_PASS`，`UVM_WARNING=14`，`UVM_ERROR=0`，`UVM_FATAL=0`。

说明：14 条 `UVM_WARNING` 均来自 agent sequencer 报告未配置自身 `main_phase.default_sequence`。这是本轮按 plan 移除 testcase 侧 agent default_sequence 配置后的预期现象；真实功能 sequence 由 vseq 显式启动。

## 1. 总体结论

本轮实现已把新场景入口收敛为 `basicTest + ts=<virtual_sequence>`。`basicTest` 负责构建 env 和设置 `env.vsqr.main_phase.default_sequence`，`memblock_env` 创建并连接 `vsqr`，真实场景由 `memblock_dispatch_real_smoke_vseq` 在 `p_sequencer` 上统一调度各 agent base/responder sequence。

未发现 blocker。存在若干与原 plan 表述不同但经验证可工作的实现差异，主要是使用显式 `start()` 代替 `uvm_do_on`、`basicTest` 直接创建 env、远端 make 补充 `ts` 转发。这些差异已在本文末尾“实现与 Plan 不一致项”中说明。

## 2. 新增 `memblock_virtual_sequencer`

### 修改前逻辑

旧流程没有 env 级 virtual sequencer。各 testcase 或 agent sequencer 的 phase default sequence 分散配置，具体场景需要从 testcase 继承链里判断哪些 sequence 会启动。

### 修改后逻辑

新增 `memblock_virtual_sequencer`，保存所有可由顶层 vseq 调度的 agent sequencer handle。它只做 handle 聚合，不创建 agent，不驱动 transaction。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_virtual_sequencer.sv`，字段定义

```systemverilog
class memblock_virtual_sequencer extends uvm_sequencer;

    backendToTopBypass_agent_agent_sequencer       backendToTopBypass_sqr;
    fence_agent_agent_sequencer                    fence_sqr;
    csr_ctrl_agent_agent_sequencer                 csr_ctrl_sqr;
    lsqcommit_agent_agent_sequencer                lsqcommit_sqr;
    lsqenq_agent_agent_sequencer                   lsqenq_sqr;
    lintsissue_agent_agent_sequencer               lintsissue_sqr;
    vecissue_agent_agent_sequencer                 vecissue_sqr;
    redirect_agent_agent_sequencer                 redirect_sqr;
```

中文伪代码：

```text
这段字段定义在当前 feature 中承担 agent sequencer 聚合表的功能。
类继承 uvm_sequencer，使它可以作为 env.vsqr.main_phase.default_sequence 的启动 sequencer。
每个字段保存一个真实 agent 的 sequencer handle，例如 lsqenq_sqr 保存 LSQ 入队 agent 的 sequencer。
这些字段不在类内部创建，也不在类内部驱动；它们由 memblock_env::connect_phase 从已创建 agent 中赋值。
后续场景 vseq 通过 p_sequencer.<agent>_sqr 启动 child sequence，避免硬编码层级路径。
```

正确性检查：

- 字段覆盖 plan 要求的所有主要 agent，包括 LSQ、issue、redirect、sbuffer、dcache、L2TLB、prefetch 和 OOO 反馈类 agent。
- `memblock_virtual_sequencer` 不拥有 agent 生命周期，避免引入重复创建或重复连接风险。

## 3. 新增 `virtual_base_sequence`

### 修改前逻辑

旧 `basicTest` 曾尝试用 `"tc_sanity"` 作为 factory override 锚点，但 `tc_sanity` 是 testcase component，不是 sequence，不能作为 `env.vsqr.main_phase.default_sequence` 的语义锚点。

### 修改后逻辑

新增 `virtual_base_sequence` 作为所有顶层 vseq 的公共基类。默认 body 为空，`basicTest` 将它设置为 `env.vsqr.main_phase.default_sequence`，用户指定的 vseq 通过 factory override 替换它。

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/virtual_base_sequence.sv`，类定义和检查 helper

```systemverilog
class virtual_base_sequence extends uvm_sequence;

    `uvm_object_utils(virtual_base_sequence)
    `uvm_declare_p_sequencer(memblock_virtual_sequencer)

function void virtual_base_sequence::require_agent_sqr(input string agent_name,
                                                       input uvm_sequencer_base sqr);
    require_virtual_sqr();
    if (sqr == null) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("required agent sequencer is null: %0s", agent_name))
    end
endfunction
```

中文伪代码：

```text
这段逻辑在当前 feature 中提供 vseq 基类和 agent sequencer 必需性检查。
类通过 uvm_declare_p_sequencer 把 p_sequencer 声明为 memblock_virtual_sequencer，保证派生 vseq 能用 p_sequencer.<agent>_sqr。
require_agent_sqr 先调用 require_virtual_sqr，确认当前 sequence 是启动在 env.vsqr 上；如果不是，说明启动点错误，直接 fatal。
随后检查传入的 agent sequencer handle 是否为空；如果为空，说明该场景需要的 agent 未连接或未打开，打印带 agent 名称的 fatal。
该 helper 不启动 sequence，只负责在启动 child sequence 前把配置问题尽早暴露。
```

正确性检查：

- 默认 `virtual_base_sequence` 可作为空场景运行，已通过 `ts=virtual_base_sequence` smoke。
- `require_agent_sqr()` 把场景所需 agent 的缺失变成明确 fatal，避免 child sequence start 到 null sequencer 后出现难定位错误。

## 4. `memblock_env` 接入 `vsqr`

### 修改前逻辑

`memblock_env` 只创建各 agent、RM、scoreboard 连接，没有统一的 virtual sequencer 汇总 agent sequencer handle。

### 修改后逻辑

`memblock_env::build_phase()` 创建 `vsqr`，`connect_phase()` 把已创建 agent 的 `.sqr` 赋给 `vsqr` 对应字段。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_env.sv`，`build_phase`

```systemverilog
this.cfg.apply_user_cfg();
this.vsqr = memblock_virtual_sequencer::type_id::create("vsqr", this);
```

中文伪代码：

```text
这段逻辑在 env build 阶段创建 virtual sequencer。
函数先应用用户 cfg，使 agent 的开关和模式在创建 agent 前生效。
随后创建名为 vsqr 的 memblock_virtual_sequencer；它作为 env 的子 component 存在。
创建后的 vsqr 暂时没有 agent handle，后续 connect_phase 会在 agent 都创建完成后填充字段。
```

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_env.sv`，`connect_phase`

```systemverilog
if (this.vsqr == null) begin
    `uvm_fatal(get_type_name(), "vsqr is null")
end

if (this.u_lsqenq_agent_agent != null) begin
    this.vsqr.lsqenq_sqr = this.u_lsqenq_agent_agent.sqr;
end
if (this.u_L2tlb_agent_agent != null) begin
    this.vsqr.L2tlb_sqr = this.u_L2tlb_agent_agent.sqr;
end
```

中文伪代码：

```text
这段逻辑在 env connect 阶段负责把真实 agent sequencer 连接到 vsqr。
函数先确认 vsqr 已经在 build_phase 创建；如果为空，说明 env 构建失败，直接 fatal。
随后按 agent 逐项判断 component 是否存在；存在时将该 agent 的 sqr 赋值给 vsqr 对应字段。
示例中的 lsqenq_sqr 和 L2tlb_sqr 分别供 real smoke vseq 启动 LSQ 入队 sequence 和 L2TLB responder sequence。
如果某个 agent 不存在，env 不立即 fatal；是否必须存在由具体 vseq 的 require_agent_sqr 决定。
```

正确性检查：

- `connect_phase()` 保留原有 RM/scoreboard 连接后再接 `vsqr`，不改变 monitor/RM 数据通路。
- real smoke 必需 sequencer 在 vseq 中再次检查，能在场景入口给出明确报错。

## 5. `basicTest` 入口改造

### 修改前逻辑

`basicTest` 继承 `tc_base`，默认 `usr_test_vseq` 为 `tc_sanity`，并使用 `"tc_sanity"` 做 factory override 锚点。该方式会和 legacy testcase 继承链耦合，也不能把场景逻辑清晰收敛到 vseq。

### 修改后逻辑

`basicTest` 继承 `tcnt_test_base`，自行创建 `memblock_env`，默认 vseq 为 `virtual_base_sequence`。读取 `+VSEQ_MAIN` 后，用 factory override 把 `virtual_base_sequence` 替换为用户指定 vseq，并把 `env.vsqr.main_phase.default_sequence` 设置到 `virtual_base_sequence`。

源码位置：`mem_ut/ver/ut/memblock/tc/src/basicTest.sv`，`build_phase`

```systemverilog
string usr_test_vseq="virtual_base_sequence";
...
this.env = memblock_env::type_id::create("env", this);
...
if (!$value$plusargs("VSEQ_MAIN=%s", usr_test_vseq)) begin
    void'(uvm_cmdline_proc.get_arg_value("+VSEQ_MAIN=", usr_test_vseq));
end
if (usr_test_vseq != "virtual_base_sequence") begin
    uvm_factory::get().set_type_override_by_name("virtual_base_sequence",usr_test_vseq);
end
uvm_config_db#(uvm_object_wrapper)::set(this, "env.vsqr.main_phase",
    "default_sequence",virtual_base_sequence::type_id::get());
```

中文伪代码：

```text
这段逻辑在当前 feature 中负责 testcase 固定入口和用户 vseq 选择。
函数把默认 vseq 设置为 virtual_base_sequence；如果用户不传 ts，main_phase 会运行空 vseq。
函数创建 env，使 basicTest 不再依赖 tc_base 的 legacy default_sequence 配置。
函数优先通过 $value$plusargs 读取 +VSEQ_MAIN；若没有读到，再用 uvm_cmdline_processor 兼容读取。
如果用户指定的 vseq 不是默认基类，函数通过 UVM factory 把 virtual_base_sequence 替换成用户指定类型。
最后函数把 env.vsqr.main_phase.default_sequence 设置为 virtual_base_sequence；UVM main_phase 创建该 sequence 时会得到 factory override 后的真实 vseq。
```

正确性检查：

- `ts=virtual_base_sequence` smoke 证明默认空 vseq 可运行。
- `ts=memblock_dispatch_real_smoke_vseq` smoke 日志证明 `+VSEQ_MAIN` 被读到并发生 factory override。
- `basicTest` 不继承 `tc_base`，避免 legacy testcase 默认 sequence 配置覆盖新 vseq 调度。

## 6. `memblock_dispatch_real_smoke_vseq` 调度

### 修改前逻辑

real smoke 由 `tc_dispatch_real_smoke` 在 testcase `main_phase` 中置位 `dispatch_real_smoke_active` 并直接启动 real smoke sequence。场景调度和 testcase 耦合。

### 修改后逻辑

real smoke 场景迁移到 `memblock_dispatch_real_smoke_vseq`。该 vseq 检查必需 sequencer，置位 active 标志，启动后台 responder 和核心流程，核心流程结束后清理 active 标志并返回 body；退出收束由 UVM objection 和 1us drain_time 完成，不再显式 disable 后台 fork。

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_vseq.sv`，`body`

```systemverilog
require_real_smoke_sqr();
seq_csr_common::init();
memblock_sync_pkg::dispatch_real_smoke_active = 1'b1;
fork : background_responder_fork
    start_background_responders();
join_none
start_core_dispatch_flow();
memblock_sync_pkg::dispatch_real_smoke_active = 1'b0;
```

中文伪代码：

```text
这段逻辑在当前 feature 中负责 real smoke vseq 的生命周期控制。
函数先检查 real smoke 必须使用的 agent sequencer 是否连接，避免后续 sequence 启动到空 handle。
函数初始化 seq_csr_common，使当前 sequence 使用最新 plus/cfg。
函数置位 dispatch_real_smoke_active；CSR driver 等同步逻辑用该标志进入 real smoke 驱动模式。
函数用后台 fork 启动 responder 组，不等待它们自然结束，因为 responder 可能是长期循环。
函数启动核心 dispatch flow 并等待核心 flow 完成。
核心 flow 完成后，函数清零 dispatch_real_smoke_active，然后返回 body。
该 vseq 的 pre_body 设置 main_phase drain_time 为 1us，并 raise objection；继承的 post_body drop objection 后，UVM 在 phase 结束前保留 1us 给后台 responder 处理尾部请求。
当前实现不再显式 disable 后台 fork，避免在主流程完成点直接截断 responder 线程。
```

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_vseq.sv`，`start_core_dispatch_flow`

```systemverilog
fork
    lsqenq_seq.start(p_sequencer.lsqenq_sqr);
    issue_seq.start(p_sequencer.lintsissue_sqr);
    lsqcommit_seq.start(p_sequencer.lsqcommit_sqr);
    l2tlb_seq.start(p_sequencer.L2tlb_sqr);
    main_seq.start(p_sequencer);
join
```

中文伪代码：

```text
这段逻辑在当前 feature 中负责启动 real smoke 的核心有限流程。
函数并行启动 LSQ 入队、issue、LSQ commit、L2TLB 和 main table 主流程 sequence。
每个 agent sequence 启动到对应真实 agent sequencer，main table 主流程启动到 virtual sequencer。
函数使用 join 等待所有核心 sequence 返回；只有核心流程都达到自身终止条件后，vseq 才会清理 active 标志并关闭后台 responder。
```

正确性检查：

- `require_real_smoke_sqr()` 覆盖 real smoke 必需的 LSQ、issue、commit、L2TLB、dcache、sbuffer、redirect sequencer。
- batch run 日志显示 `memblock_dispatch_real_smoke_vseq`、`memblock_l2tlb_base_sequence`、`memblock_main_dispatch_auto_build_main_table_base_sequence` 均启动，最终 `TEST_PASS`。

## 7. Makefile 与远端运行接入

### 修改前逻辑

默认 testcase 是 `tc_sanity`，且远端 eda make 没有转发 `ts`。即使命令行指定 `ts=memblock_dispatch_real_smoke_vseq`，远端运行仍可能退回默认值。

### 修改后逻辑

默认配置切为 `tc=basicTest ts=virtual_base_sequence`，VCS/xrun 都加入 `+VSEQ_MAIN=${ts}`。远端 make wrapper 转发 `ts`。

源码位置：`mem_ut/scr/verif/project_cfg_vcs.mk`，仿真参数

```makefile
SIMV_OPTIONS += +UVM_TIMEOUT=${timeout_ns}
SIMV_OPTIONS += +VSEQ_MAIN=${ts}
```

中文伪代码：

```text
这段 makefile 逻辑把用户可见的 ts 参数转换为 basicTest 可读取的 +VSEQ_MAIN。
仿真命令先加入 UVM timeout 等原有参数。
随后加入 +VSEQ_MAIN=${ts}，使 SystemVerilog 端 $value$plusargs 可以读取用户指定 vseq 名称。
该参数只作为 makefile 与 basicTest 之间的内部传递接口，用户仍通过 ts 指定场景。
```

源码位置：`mem_ut/ver/ut/memblock/sim/remote_eda_make.sh`，远端变量转发

```bash
FORWARD_VARS=(
    seed
    tc
    ts
    pl
)
```

中文伪代码：

```text
这段 shell 逻辑负责远端 eda make 的参数透传。
FORWARD_VARS 列出需要从本地 make 环境传到远端 make 的变量。
新增 ts 后，远端 make 能得到用户指定的 virtual sequence 名称。
如果不转发 ts，远端工程会使用默认 ts，导致 basicTest 读取到 virtual_base_sequence 而不是用户指定 vseq。
```

正确性检查：

- real smoke batch run 命令行包含 `+VSEQ_MAIN=memblock_dispatch_real_smoke_vseq`。
- basicTest 日志打印 `usr_test_vseq_name:memblock_dispatch_real_smoke_vseq`。

## 8. 编译组织接入

### 修改前逻辑

`memblock_env_pkg` 不依赖 `seq_pkg`，`tb.f` 编译顺序是 env 在 seq 前。新增 `vsqr` 后，env 需要看到 `memblock_virtual_sequencer` 类型。

### 修改后逻辑

`memblock_virtual_sequencer.sv` 物理移动到 `env/src`，但仍由 `seq_pkg.sv` include。`seq/seq.f` 增加 `../env/src` incdir，使 `seq_pkg` 能先看到 `memblock_virtual_sequencer`，随后 `memblock_env_pkg.sv` import `seq_pkg::*`，`tb.f` 保持先编译 seq 再编译 env。

源码位置：`mem_ut/ver/ut/memblock/cfg/tb.f`，filelist 顺序

```text
-F ../common/memblock_common/memblock_common.f
-F ../seq/seq.f
-F ../env/memblock_env.f
-F ../tc/tc.f
```

中文伪代码：

```text
这段 filelist 顺序保证类型定义先于使用点编译。
工程先编译 memblock common，再编译 seq_pkg；seq_pkg 通过 seq.f 的 ../env/src incdir include memblock_virtual_sequencer，再 include virtual_base_sequence。
随后编译 env 包；env 包 import seq_pkg 后，可以合法声明 memblock_virtual_sequencer vsqr。
最后编译 testcase 包，basicTest 可以引用 env 和 seq 类型。
```

正确性检查：

- 调整后远端 `eda_compile` 通过，说明 package 顺序满足类型依赖。

## 9. 性能与结构风险检查

- 运行期主路径没有新增全表扫描；新增逻辑主要发生在 build/connect 和 sequence 启动阶段。
- `vsqr` 只保存 handle，不维护重复状态，不引入状态一致性风险。
- real smoke vseq 的后台 responder 使用外层 named fork 管理，避免 responder forever loop 阻塞 vseq 退出。
- `basicTest` 不再配置各 agent phase default sequence，真实功能 base/responder sequence 均由具体 vseq 显式启动。

## 10. 实现与 Plan 不一致项

### 10.1 子 sequence 启动方式使用 `start()` 而不是 `uvm_do_on`

Plan 原有逻辑：

```text
具体 vseq 在 body 中通过 uvm_do_on 系列宏显式启动 child sequence。
```

当前源码逻辑：

`memblock_dispatch_real_smoke_vseq` 使用 `type_id::create()` 后显式 `seq.start(target_sqr)`。

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_vseq.sv`

```systemverilog
dcache_seq = dcache_mem__access_base_sequence::type_id::create("dcache_seq");
sbuffer_seq = sbuffer_mem_access_base_sequence::type_id::create("sbuffer_seq");
redirect_seq = memblock_redirect_dispatch_base_sequence::type_id::create("redirect_seq");

fork
    dcache_seq.start(p_sequencer.dcache_sqr);
    sbuffer_seq.start(p_sequencer.sbuffer_sqr);
    redirect_seq.start(p_sequencer.redirect_sqr);
join
```

中文伪代码：

```text
这段逻辑先通过 factory 创建三个 responder sequence 对象，保留 UVM factory override 能力。
随后并行调用每个 sequence 的 start 方法，并把目标 agent sequencer 作为参数传入。
start 调用和 uvm_do_on 的核心效果一致，都是把 sequence 启动到指定 sequencer；当前写法让后台 forever responder 的对象创建和 fork 生命周期更直观。
join 表示该 responder 组自身等待三个 responder 返回；实际退出由外层 named fork 在主流程结束后 disable。
```

不一致原因：

显式 `start()` 更容易表达“先 create，再在 named fork 中管理 responder 生命周期”的结构，并且仿真已验证可运行。处理结论：保持当前实现，后续规则中可把 `uvm_do_on` 理解为推荐风格而非必须宏形式；要求仍是必须启动到 `p_sequencer.<agent>_sqr`。

### 10.2 `basicTest` 直接创建 env，不再继承 `tc_base`

Plan 原有逻辑：

```text
basicTest 作为唯一 testcase 入口；
basicTest 设置 env.vsqr.main_phase.default_sequence。
```

当前源码逻辑：

`basicTest extends tcnt_test_base`，自行创建 `memblock_env env`，并设置 cfg 与 default sequence。

源码位置：`mem_ut/ver/ut/memblock/tc/src/basicTest.sv`

```systemverilog
class basicTest extends tcnt_test_base ;
      memblock_env env;
      memblock_env_cfg real_smoke_cfg;
...
real_smoke_cfg = memblock_env_cfg::type_id::create("real_smoke_cfg");
void'(real_smoke_cfg.randomize());
configure_real_env_cfg(real_smoke_cfg);
uvm_config_db#(memblock_env_cfg)::set(this, "env", "cfg", real_smoke_cfg);
this.env = memblock_env::type_id::create("env", this);
```

中文伪代码：

```text
这段逻辑让 basicTest 成为独立的新入口，而不是复用 tc_base 的旧场景配置。
类继承 tcnt_test_base，只保留基础 UVM testcase 能力。
build_phase 创建 real_smoke_cfg 并随机化，随后调用 configure_real_env_cfg 设置安全默认 driver 模式。
函数把 cfg 写入 env 路径的 config_db，再创建 memblock_env。
这样 basicTest 可以避开 tc_base 中分散设置 agent default_sequence 的 legacy 逻辑。
```

不一致原因：

这是为了避免 `tc_base` 的旧 default_sequence 配置覆盖新 vseq 调度。处理结论：保持当前实现，并在 flow 文档中明确 basicTest 自己创建 env。

### 10.3 远端 make 增加 `ts` 转发

Plan 原有逻辑：

```text
makefile 将 ts 转成内部 plusarg +VSEQ_MAIN=<virtual sequence class name>。
```

当前源码逻辑：

除本地 project cfg 加 `+VSEQ_MAIN=${ts}` 外，还修改 `sim/Makefile` 和 `remote_eda_make.sh` 透传 `ts`。

源码位置：`mem_ut/ver/ut/memblock/sim/Makefile`

```makefile
seed='$(seed)' tc='$(tc)' ts='$(ts)' pl='$(pl)' mode='$(mode)' wave='$(wave)' timing='$(timing)' \
```

中文伪代码：

```text
这段逻辑在调用远端 eda make 时把 ts 变量放入环境传参。
本地 make 接收到用户指定的 ts 后，将它和 seed、tc、mode 等一起传给 remote_eda_make.sh。
远端 make 因此能用同一个 ts 值生成 +VSEQ_MAIN。
如果缺少该传参，远端运行会退回默认 ts，导致用户指定的 vseq 不生效。
```

不一致原因：

coding 验证时发现仅修改 project cfg 不足以覆盖远端 eda flow，必须同时转发 `ts`。处理结论：保持当前实现。

## 11. Plan 未说明但 Coding 落实的细节

### 11.1 `+VSEQ_MAIN` 优先使用 `$value$plusargs` 读取

源码位置：`mem_ut/ver/ut/memblock/tc/src/basicTest.sv`

```systemverilog
if (!$value$plusargs("VSEQ_MAIN=%s", usr_test_vseq)) begin
    void'(uvm_cmdline_proc.get_arg_value("+VSEQ_MAIN=", usr_test_vseq));
end
```

中文伪代码：

```text
这段逻辑负责稳定读取远端仿真命令行中的 +VSEQ_MAIN。
函数先用 SystemVerilog 原生 $value$plusargs 读取 VSEQ_MAIN 字符串；如果读取成功，usr_test_vseq 立即变成用户指定 vseq。
如果原生读取失败，函数再调用 uvm_cmdline_processor 的 get_arg_value 作为兼容路径。
该顺序是因为远端 batch run 中曾出现命令行包含 +VSEQ_MAIN 但 UVM cmdline processor 未按预期读到的情况。
```

处理结论：

保留该实现；它不改变用户接口，只增强 plusarg 读取稳定性。

### 11.2 日志和波形文件名包含 `ts`

源码位置：`mem_ut/scr/verif/project_cfg.mk`

```makefile
SIMV_LOG = -l ./${mode}/log/tc=${tc}_ts=${ts}_cfg=${cfg}_seed=$(strip $(seed_tmp))_${timing}.log
EXPORT_OPTS += export WAVE_FILE=./${mode}/wave/tc=${tc}_ts=${ts}_cfg=${cfg}_seed=$(strip $(seed_tmp))_${timing}
```

中文伪代码：

```text
这段逻辑让同一个 basicTest 下的不同 virtual sequence 生成不同日志和波形路径。
日志文件名不再只包含 tc 和 seed，而是加入 ts 和 cfg。
这样 basicTest + virtual_base_sequence 与 basicTest + memblock_dispatch_real_smoke_vseq 不会互相覆盖日志。
该修改只影响输出文件命名，不影响仿真激励行为。
```

处理结论：

保留该实现；它提升 debug 可定位性。

### 11.3 远端状态文件名包含 `ts`

源码位置：`mem_ut/ver/ut/memblock/sim/Makefile`

```makefile
REMOTE_PID_FILE ?= $(REMOTE_STATE_DIR)/$(tc)_$(ts)_$(mode).pid
REMOTE_LAUNCH_LOG ?= $(REMOTE_STATE_DIR)/$(tc)_$(ts)_$(mode).launch.log
```

中文伪代码：

```text
这段逻辑让远端 eda run 的状态文件按 testcase、virtual sequence 和 mode 区分。
REMOTE_PID_FILE 保存远端后台任务 pid，REMOTE_LAUNCH_LOG 保存远端启动日志。
文件名加入 ts 后，同一 tc=basicTest 下并行启动不同 vseq 时，不会共用同一个 pid 或 launch log 文件。
该修改不影响仿真命令内容，只影响远端任务管理文件路径。
```

处理结论：

保留该实现；它修复 subagent review 提出的并发状态文件覆盖风险。

## 12. 剩余风险与后续建议

1. 当前后台 responder 通过 UVM objection drain 收束：`pre_body()` 设置 1us drain_time，body 返回后由 inherited `post_body()` drop objection，UVM phase 结束前保留 1us 尾部处理窗口。后续若要做长压力，仍建议给 responder sequence 增加统一 stop 条件，使 responder 能在 pending 清空后自然返回。
2. legacy `tc_dispatch_real_smoke` 仍保留，当前没有删除旧 testcase 入口。默认入口已切到 `basicTest + ts`，后续可按回归迁移情况再清理 legacy testcase。

## 13. Plan 对齐检查

已逐项检查本次源码差异与关联 plan。结论：

- 目标入口 `basicTest + ts=<vseq>` 已实现。
- `virtual_base_sequence`、`memblock_virtual_sequencer`、`memblock_dispatch_real_smoke_vseq` 已实现并接入 package/filelist。
- `env.vsqr` 已创建并连接 agent sequencer。
- makefile 已把 `ts` 转成 `+VSEQ_MAIN`，并补齐远端 eda flow 的 `ts` 转发。
- real smoke vseq 已启动 dcache/sbuffer/redirect responder 和 LSQ enq/issue/commit/L2TLB/main flow。
- 与 plan 不一致项均已在第 10 节说明，当前均为有意实现调整且通过验证。

## 14. 2026-07-06 路径移动补充 Review

### 14.1 `memblock_virtual_sequencer.sv` 移动到 env/src

修改前逻辑：

```text
memblock_virtual_sequencer.sv 物理位于 mem_ut/ver/ut/memblock/seq/virtual_sequence。
seq_pkg.sv 通过 virtual_sequence incdir include 该文件。
```

修改后逻辑：

```text
memblock_virtual_sequencer.sv 物理位于 mem_ut/ver/ut/memblock/env/src。
seq_pkg.sv 仍 include "memblock_virtual_sequencer.sv"。
seq.f 新增 +incdir+../env/src，让 seq_pkg 能从 env/src 找到该文件。
```

源码位置：`mem_ut/ver/ut/memblock/seq/seq.f`

```text
+incdir+./virtual_sequence
+incdir+../env/src
seq_pkg.sv
...
// ../env/src/memblock_virtual_sequencer.sv
// ./virtual_sequence/virtual_base_sequence.sv
```

中文伪代码：

```text
这段 filelist 逻辑负责给 seq_pkg 提供 include 搜索路径。
先保留 virtual_sequence 路径，使 seq_pkg 能继续 include virtual_base_sequence 和场景 vseq。
再新增 ../env/src 路径，使 seq_pkg 能从 env/src include memblock_virtual_sequencer。
注释列表同步更新实际物理路径，避免 review 时误以为文件仍在 seq/virtual_sequence 目录。
该修改只改变文件物理位置和 include 搜索路径，不改变 memblock_virtual_sequencer 的 package 归属。
```

正确性检查：

```text
virtual_base_sequence 仍在 seq_pkg 中声明 p_sequencer 类型为 memblock_virtual_sequencer。
因此 memblock_virtual_sequencer 仍必须先由 seq_pkg include，不能改为只在 memblock_env_pkg 中 include。
否则 seq_pkg 与 memblock_env_pkg 会形成类型依赖循环，virtual_base_sequence 编译时也看不到 memblock_virtual_sequencer。
```
