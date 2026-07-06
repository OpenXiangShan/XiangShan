# Virtual Sequence 统一调度 Flow

## 1. Flow 定位

本文描述 memblock 当前通过 `basicTest + ts=<virtual sequence>` 启动测试场景的真实源码 flow。该 flow 的目标是把 testcase 固定为 `basicTest`，把场景选择收敛到 `env.vsqr.main_phase.default_sequence` 上运行的 `virtual_base_sequence` 派生类。

核心对象：

- `basicTest`：固定 testcase 入口，负责创建 `memblock_env`、读取 `+VSEQ_MAIN`、配置 `env.vsqr.main_phase.default_sequence`。
- `memblock_virtual_sequencer`：env 里的 virtual sequencer，只保存各 agent 真实 sequencer handle。
- `virtual_base_sequence`：所有顶层 vseq 的基类，默认 body 为空。
- `memblock_dispatch_real_smoke_vseq`：real dispatch smoke 场景 vseq，负责启动 real smoke 需要的 base/responder sequence。

## 2. 函数调用 Flow 图

```mermaid
flowchart TD
    A[make tc=basicTest ts=<vseq>] --> B[project_cfg_vcs/xrun.mk adds +VSEQ_MAIN=${ts}]
    B --> C[basicTest::build_phase]
    C --> D[configure_real_env_cfg]
    C --> E[memblock_env::build_phase creates env.vsqr]
    E --> F[memblock_env::connect_phase connects agent sqr handles to vsqr]
    C --> G[read +VSEQ_MAIN]
    G --> H{usr_test_vseq == virtual_base_sequence}
    H -->|yes| I[keep default empty virtual_base_sequence]
    H -->|no| J[factory override virtual_base_sequence to user vseq]
    I --> K[env.vsqr.main_phase starts selected vseq]
    J --> K
    K --> L[virtual_base_sequence::pre_body raises objection]
    L --> M{selected vseq}
    M -->|empty| N[virtual_base_sequence::body returns]
    M -->|real smoke| O[memblock_dispatch_real_smoke_vseq::body]
    O --> P[require_real_smoke_sqr checks needed agent sqr]
    O --> Q[set dispatch_real_smoke_active=1]
    Q --> R[start_background_responders]
    Q --> S[start_core_dispatch_flow]
    R --> T[dcache/sbuffer/redirect responders run in background]
    S --> U[lsqenq/issue/commit/L2TLB/main flow run]
    U --> V[clear dispatch_real_smoke_active and disable background fork]
```

### 函数调用 Flow 图整体文字伪代码

```text
Virtual sequence 统一调度主流程：

1. makefile 参数转换阶段：
   用户通过 make 参数 tc 和 ts 选择 testcase 与 virtual sequence。
   project_cfg_vcs.mk 或 project_cfg_xrun.mk 把 ts 转成 +VSEQ_MAIN=<ts>。
   远端 eda make 包装脚本把 ts 一并转发到远端 make，避免远端仿真退回默认 vseq。

2. basicTest 构建阶段：
   basicTest::build_phase 创建 memblock_env_cfg，设置 agent driver 进入 DRV_0 且关闭 xz 驱动。
   basicTest 创建 env，并读取 +VSEQ_MAIN。
   如果 +VSEQ_MAIN 不是 virtual_base_sequence，则把 virtual_base_sequence factory override 为用户指定 vseq。
   basicTest 把 env.vsqr.main_phase.default_sequence 设置成 virtual_base_sequence；UVM factory 会在 main_phase 启动实际 vseq 类型。

3. env virtual sequencer 接线阶段：
   memblock_env::build_phase 创建 memblock_virtual_sequencer 类型的 vsqr。
   memblock_env::connect_phase 把每个已创建 agent 的 sqr 赋值到 vsqr 对应字段。
   后续 vseq 只通过 p_sequencer.<agent>_sqr 访问 agent sequencer，不再查找层级路径。

4. vseq 运行阶段：
   virtual_base_sequence::pre_body 在 main_phase 启动时 raise objection，保证 vseq 生命周期覆盖 main_phase。
   如果用户没有指定真实场景，virtual_base_sequence::body 只打印空 body 日志后返回。
   如果用户指定 memblock_dispatch_real_smoke_vseq，则该 vseq 先检查 real smoke 必需的 sequencer handle，再置位 dispatch_real_smoke_active。
   vseq 启动后台 dcache、sbuffer、redirect responder，随后启动 LSQ enq、issue、commit、L2TLB 和 main table 主流程。
   主流程结束后清零 dispatch_real_smoke_active，body 返回后由 inherited post_body drop objection。
   memblock_dispatch_real_smoke_vseq::pre_body 设置 main_phase drain_time 为 1us，使 UVM 在 objection drop 后保留 1us 尾部处理窗口，不再显式 disable 后台 responder fork。
```

## 3. `basicTest::build_phase()`

源码位置：`mem_ut/ver/ut/memblock/tc/src/basicTest.sv`

真实逻辑摘要：

```systemverilog
string usr_test_vseq="virtual_base_sequence";
seq_csr_common::reload_from_plus();
real_smoke_cfg = memblock_env_cfg::type_id::create("real_smoke_cfg");
void'(real_smoke_cfg.randomize());
configure_real_env_cfg(real_smoke_cfg);
uvm_config_db#(memblock_env_cfg)::set(this, "env", "cfg", real_smoke_cfg);
this.env = memblock_env::type_id::create("env", this);
if (!$value$plusargs("VSEQ_MAIN=%s", usr_test_vseq)) begin
    void'(uvm_cmdline_proc.get_arg_value("+VSEQ_MAIN=", usr_test_vseq));
end
if (usr_test_vseq != "virtual_base_sequence") begin
    uvm_factory::get().set_type_override_by_name("virtual_base_sequence",usr_test_vseq);
end
uvm_config_db#(uvm_object_wrapper)::set(this, "env.vsqr.main_phase",
    "default_sequence",virtual_base_sequence::type_id::get());
```

功能解释：

`basicTest::build_phase()` 是新入口的唯一 testcase 构建点。它不直接启动 real smoke sequence，而是把 `env.vsqr.main_phase.default_sequence` 固定设置为 `virtual_base_sequence`，再通过 factory override 把该基类替换成 `ts` 指定的真实 vseq。

输入/输出：

- 输入：makefile 转出的 `+VSEQ_MAIN=<vseq>`，以及普通 plus/cfg 参数。
- 输出：创建 `env`，设置 `env` cfg，设置 `env.vsqr.main_phase.default_sequence`。
- 副作用：如果指定用户 vseq，则注册 `virtual_base_sequence -> usr_test_vseq` 的 factory override。

文字伪代码：

```text
basicTest::build_phase 在当前 flow 中负责固定 testcase 入口并选择顶层 vseq。
函数先重新加载 plus 参数，创建并随机化 memblock_env_cfg，然后把所有 agent 的 driver mode 和 xz 设置调整为 real DUT flow 需要的安全默认配置。
函数将 cfg 写入 config_db，并创建 env，使 env 后续 build_phase 能拿到 cfg。
函数读取 +VSEQ_MAIN：优先使用 $value$plusargs 直接解析命令行，若没有读到，再尝试 uvm_cmdline_processor。
如果读到的 vseq 不是 virtual_base_sequence，函数通过 UVM factory 把 virtual_base_sequence 替换为用户指定 vseq；这样 main_phase 仍只配置一个稳定锚点。
最后函数把 env.vsqr.main_phase.default_sequence 设置为 virtual_base_sequence，使 UVM main_phase 在 vsqr 上启动被 factory override 后的真实 vseq。
```

## 4. `memblock_env::build_phase()` 和 `connect_phase()`

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_env.sv`

真实逻辑摘要：

```systemverilog
this.cfg.apply_user_cfg();
this.vsqr = memblock_virtual_sequencer::type_id::create("vsqr", this);
...
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

功能解释：

`memblock_env` 创建 `vsqr` 并在 connect phase 把各 agent sequencer handle 汇总到 `vsqr`。`vsqr` 不创建 agent、不驱动 transaction，只作为 vseq 调度各 agent sequence 的 handle 聚合点。

输入/输出：

- 输入：env 中已创建的各 agent component。
- 输出：`vsqr.<agent>_sqr` 字段。
- 失败策略：`vsqr` 未创建时 `UVM_FATAL`；具体 agent sequencer 是否必须存在由场景 vseq 调用 `require_agent_sqr()` 决定。

文字伪代码：

```text
memblock_env::build_phase 在当前 flow 中负责创建 virtual sequencer。
函数先读取或创建 env cfg，并调用 apply_user_cfg 把用户配置应用到 agent cfg。
随后创建 memblock_virtual_sequencer 类型的 vsqr；vsqr 与各 agent 同属于 env，不拥有 agent 生命周期。

memblock_env::connect_phase 在当前 flow 中负责把 agent sequencer 接到 vsqr。
函数先完成原有 RM/scoreboard/monitor 连接，保持旧数据通路不变。
然后检查 vsqr 是否为空；如果为空说明 build 阶段没有创建成功，直接 fatal。
对每个 agent，函数判断 agent component 是否存在；若存在，则把 agent.sqr 赋给 vsqr 中对应字段。
没有创建或未打开的 agent 不在 env 层 fatal，后续由具体 vseq 在启动该 agent sequence 前判断该 sequencer 是否为必需资源。
```

## 5. `virtual_base_sequence`

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/virtual_base_sequence.sv`

真实逻辑摘要：

```systemverilog
class virtual_base_sequence extends uvm_sequence;
    `uvm_object_utils(virtual_base_sequence)
    `uvm_declare_p_sequencer(memblock_virtual_sequencer)
...
function void virtual_base_sequence::require_agent_sqr(input string agent_name,
                                                       input uvm_sequencer_base sqr);
    require_virtual_sqr();
    if (sqr == null) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("required agent sequencer is null: %0s", agent_name))
    end
endfunction
```

功能解释：

`virtual_base_sequence` 是所有顶层 memblock vseq 的共同基类。它用 `uvm_declare_p_sequencer` 把 `p_sequencer` 固定为 `memblock_virtual_sequencer`，并提供启动 child sequence 前的 sequencer handle 检查。

输入/输出：

- 输入：UVM 启动该 sequence 时绑定的 sequencer。
- 输出：无 transaction 输出；默认 body 为空。
- 副作用：`pre_body/post_body` 在 phase sequence 场景下 raise/drop objection。

文字伪代码：

```text
virtual_base_sequence 在当前 flow 中提供统一 vseq 基类和 sequencer 检查能力。
pre_body 检查 starting_phase 是否存在；若存在，raise objection，保证 vseq body 执行期间 main_phase 不提前结束。
body 默认只打印空 body 日志，不启动任何 child sequence，作为 basicTest 默认空场景。
post_body 在 starting_phase 存在时 drop objection，释放 main_phase。
require_virtual_sqr 检查当前 sequence 是否运行在 memblock_virtual_sequencer 上；如果 p_sequencer 为空，说明启动点错误，直接 fatal。
require_agent_sqr 先复用 require_virtual_sqr 检查顶层 sequencer，再检查目标 agent sequencer 是否连接；如果为空，说明该场景需要的 agent sequencer 未创建或未打开，直接 fatal。
```

## 6. `memblock_dispatch_real_smoke_vseq::body()`

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_vseq.sv`

真实逻辑摘要：

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

功能解释：

`memblock_dispatch_real_smoke_vseq::body()` 是 real dispatch smoke 的顶层调度入口。它统一启动后台 responder 和核心有限流程 sequence，替代旧 testcase 在 main_phase 中直接 start real smoke sequence 的方式。

输入/输出：

- 输入：`p_sequencer` 中连接好的 agent sequencer handle。
- 输出：向各 agent sequencer 启动 child sequence。
- 副作用：置位/清零 `memblock_sync_pkg::dispatch_real_smoke_active`。

文字伪代码：

```text
body 在当前 flow 中负责 real smoke 场景的完整生命周期。
函数首先调用 require_real_smoke_sqr，逐个检查 lsqenq、issue、commit、L2TLB、dcache、sbuffer、redirect 这些 real smoke 必需 sequencer 已经连接。
函数初始化 seq_csr_common，使当前 sequence 能读取最新 CSR/plus 公共配置。
函数将 dispatch_real_smoke_active 置 1；CSR driver 等同步逻辑据此切换到 real smoke 需要的驱动模式。
函数用后台 fork 启动 start_background_responders，该子流程会启动 dcache、sbuffer、redirect responder，这些 responder 可能持续运行。
函数随后启动 start_core_dispatch_flow，该子流程启动 LSQ enq、issue、commit、L2TLB 和主表主流程，并等待它们结束。
核心流程结束后，函数清零 dispatch_real_smoke_active 并返回 body。
继承自 virtual_base_sequence 的 post_body 会 drop main_phase objection；本 vseq 的 pre_body 已设置 1us drain_time，UVM 在 phase 结束前保留 1us 给后台 responder 处理尾部握手。
该流程不再显式 disable 后台 responder fork，避免在主流程结束点直接截断 responder 线程。
```

## 7. `start_background_responders()`

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_vseq.sv`

真实逻辑摘要：

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

功能解释：

该 task 启动 real smoke 必需的被动 responder。dcache/sbuffer responder 响应 DUT memory request；redirect responder 消费框架已生成的 pending redirect payload，没有 payload 时保持 idle。

输入/输出：

- 输入：`p_sequencer.dcache_sqr`、`sbuffer_sqr`、`redirect_sqr`。
- 输出：在对应 agent sequencer 上启动 responder sequence。
- 生命周期：被外层 `background_responder_fork` 以 join_none 启动，主流程结束后不显式 disable；由 UVM phase drain/结束机制收束。

文字伪代码：

```text
start_background_responders 在当前 flow 中负责启动可能长期运行的后台 responder。
函数创建 dcache、sbuffer 和 redirect 三个 responder sequence 对象。
函数并行 start 三个 sequence：dcache sequence 处理 DUT dcache memory request，sbuffer sequence 处理 sbuffer memory request，redirect sequence 处理框架 pending redirect payload。
该 task 自身使用 join 等待三个后台 sequence，因此通常不会自然返回；外层 body 使用 join_none 启动它。
核心主流程结束后，body 不再 disable 外层 fork，而是清零 active 标志后返回；post_body drop objection 后，UVM 使用 pre_body 设置的 1us drain_time 给后台 responder 留出尾部处理时间。
```

## 8. `start_core_dispatch_flow()`

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_vseq.sv`

真实逻辑摘要：

```systemverilog
fork
    lsqenq_seq.start(p_sequencer.lsqenq_sqr);
    issue_seq.start(p_sequencer.lintsissue_sqr);
    lsqcommit_seq.start(p_sequencer.lsqcommit_sqr);
    l2tlb_seq.start(p_sequencer.L2tlb_sqr);
    main_seq.start(p_sequencer);
join
```

功能解释：

该 task 启动 real smoke 的核心有限流程。主表 sequence 负责 build main table、service real dispatch flow 和 end check；LSQ enq、issue、commit、L2TLB sequence 负责对应 agent 的真实驱动或响应。

输入/输出：

- 输入：`p_sequencer` 及其 agent sequencer handle。
- 输出：在对应 sequencer 上启动核心 child sequence。
- 退出条件：所有并行 child sequence 返回后，该 task 返回给 vseq body。

文字伪代码：

```text
start_core_dispatch_flow 在当前 flow 中负责启动 real smoke 的核心有限阶段。
函数创建 LSQ enq、issue、commit、L2TLB 和 main table sequence 对象。
函数并行 start 这些 sequence：LSQ enq 负责向 LSQ admission 接口发交易，issue 负责发射接口交易，commit 负责 LSQ commit/deq 相关驱动，L2TLB 负责 TLB response，main sequence 负责生成主表并推进 real dispatch 主流程。
函数使用 join 等待所有核心 sequence 结束；只有这些 sequence 都按自身终止条件返回后，body 才能清理 active 标志并进入 UVM drain-time 退出流程。
```

## 9. 编译与仿真参数 Flow

源码位置：

- `mem_ut/scr/verif/project_cfg.mk`
- `mem_ut/scr/verif/project_cfg_vcs.mk`
- `mem_ut/scr/verif/project_cfg_xrun.mk`
- `mem_ut/ver/ut/memblock/sim/Makefile`
- `mem_ut/ver/ut/memblock/sim/remote_eda_make.sh`

真实逻辑摘要：

```makefile
tc       := basicTest
ts       := virtual_base_sequence
SIMV_OPTIONS += +VSEQ_MAIN=${ts}
...
seed='$(seed)' tc='$(tc)' ts='$(ts)' pl='$(pl)' mode='$(mode)'
...
FORWARD_VARS=(
    seed
    tc
    ts
)
```

功能解释：

makefile 默认入口切到 `tc=basicTest ts=virtual_base_sequence`。用户通过 `ts=<vseq>` 指定具体场景，VCS/xrun 配置把 `ts` 转成 `+VSEQ_MAIN`，远端运行脚本把 `ts` 变量透传到远端 make。

文字伪代码：

```text
makefile 参数 flow 负责把用户可见的 ts 转成 basicTest 可读取的 +VSEQ_MAIN。
默认配置把 tc 设为 basicTest，把 ts 设为 virtual_base_sequence，使默认运行只启动空 vseq。
VCS 和 xrun 配置都把 +VSEQ_MAIN=${ts} 加入仿真命令行。
远端 eda make 入口在调用 remote_eda_make.sh 时传入 ts，remote_eda_make.sh 又把 ts 放入 FORWARD_VARS；这样远端 make 不会丢失用户指定的 vseq。
日志和波形文件名包含 tc、ts、cfg、seed，便于同一 testcase 下区分不同 virtual sequence 场景。
```
