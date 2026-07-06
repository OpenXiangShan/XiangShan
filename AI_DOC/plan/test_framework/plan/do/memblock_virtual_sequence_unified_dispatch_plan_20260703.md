# memblock virtual sequence 统一调度方案

## 1. 目标

将 memblock 测试入口收敛为：

```text
启动 testcase:
  basicTest

选择场景:
  makefile 参数 ts=<virtual sequence class name>

运行方式:
  makefile 将 ts 转成内部 plusarg +VSEQ_MAIN=<virtual sequence class name>。
  basicTest 读取 +VSEQ_MAIN，并在 env.vsqr.main_phase 上启动一个 virtual sequence。
  env.vsqr 是 memblock_virtual_sequencer，内部保存各 agent 真正 sequencer 的 handle。
  具体 virtual sequence 通过 p_sequencer 调度所有需要运行的 agent default_sequence 和 base_sequence。
  由 vseq 启动的 agent default_sequence/base_sequence 均通过 uvm_do_on 系列宏运行到对应 sequencer。
```

该方案用于替代当前多个 testcase 自己配置 agent default_sequence、自己显式 start real smoke sequence 的分散方式。后续新增真实 DUT 场景时，只新增 virtual sequence，不再新增独立 testcase。

## 2. 当前问题

### 2.1 tc_base 与 tc_dispatch_real_smoke 分散配置

`tc_base::build_phase()` 会给部分 agent 设置 base sequence：

```text
lsqcommit  -> memblock_lsqcommit_dispatch_base_sequence
lsqenq     -> memblock_lsqenq_dispatch_base_sequence
lintsissue -> memblock_issue_dispatch_base_sequence
redirect   -> memblock_redirect_dispatch_base_sequence
L2tlb      -> memblock_l2tlb_base_sequence
```

`tc_dispatch_real_smoke::configure_real_smoke_default_sequences()` 又会覆盖部分 agent 为 `tcnt_default_sequence_base#(...)`。这会导致同一个场景的 sequence 组织分散在 testcase 继承链里，后续维护时不容易看出最终哪些 sequence 会启动。

### 2.2 basicTest 当前 default_sequence 锚点不干净

当前 `basicTest` 已有如下逻辑：

```systemverilog
factory.set_type_override_by_name("tc_sanity", usr_test_vseq);
uvm_config_db#(uvm_object_wrapper)::set(this,
    "env.vsqr.main_phase",
    "default_sequence",
    tc_sanity::type_id::get());
```

但当前源码里的 `tc_sanity` 是 `tc_base` 派生的 testcase component，不是 `uvm_sequence`。如果继续把 `tc_sanity` 作为 `env.vsqr.main_phase.default_sequence` 的锚点，语义不正确。

实现需要去掉 `tc_sanity` 锚点依赖：

```text
basicTest 作为唯一 testcase 入口；
virtual_base_sequence 作为 env.vsqr.main_phase.default_sequence 的默认锚点；
factory override 的原始类型使用 virtual_base_sequence；
ts 默认值使用 virtual_base_sequence；
用户指定的 vseq class 必须继承 virtual_base_sequence。
```

本方案不再新增 `tc_sanity` vseq：

```text
新增 virtual_base_sequence。
virtual_base_sequence 自身 body 为空，可作为默认空壳 vseq。
basicTest 使用 factory.set_type_override_by_name("virtual_base_sequence", usr_test_vseq)。
usr_test_vseq 指定的 class 必须继承 virtual_base_sequence。
```

因此旧 `tc_sanity` testcase 不再和新 vseq 机制产生 class 重名冲突。旧 sanity testcase 可保留给 legacy 入口使用，但默认回归入口切到 `tc=basicTest ts=virtual_base_sequence`。

## 3. 目录与文件规划

### 3.1 新增 virtual sequence 文件

新增目录已存在：

```text
mem_ut/ver/ut/memblock/seq/virtual_sequence
```

新增文件：

```text
mem_ut/ver/ut/memblock/env/src/memblock_virtual_sequencer.sv
mem_ut/ver/ut/memblock/seq/virtual_sequence/virtual_base_sequence.sv
mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_vseq.sv
```

其中：

```text
memblock_virtual_sequencer:
  env.vsqr 的实际类型。
  保存各 agent 真正 sequencer 的 handle。
  供 virtual sequence 通过 p_sequencer.<agent>_sqr 调度 child sequence。

virtual_base_sequence:
  所有 memblock 顶层 virtual sequence 的公共基类。
  声明 p_sequencer 类型为 memblock_virtual_sequencer。
  body 默认为空。
  不直接查找 agent sequencer，不保存 agent sequencer handle。

memblock_dispatch_real_smoke_vseq:
  继承 virtual_base_sequence。
  通过 p_sequencer.<agent>_sqr 统一调度当前 real dispatch smoke flow。
```

### 3.2 env virtual_sequencer 接入

新增：

```text
mem_ut/ver/ut/memblock/env/src/memblock_virtual_sequencer.sv
```

`memblock_virtual_sequencer` 定义在 `seq_pkg` 中，避免 `seq_pkg` 反向依赖 `memblock_env_pkg`。它是 env 层统一调度 sequencer，职责是保存各 agent sequencer handle：

```text
backendToTopBypass_sqr
fence_sqr
csr_ctrl_sqr
lsqcommit_sqr
lsqenq_sqr
lintsissue_sqr
vecissue_sqr
redirect_sqr
sbuffer_sqr
dcache_sqr
int_sink_sqr
L2tlb_sqr
itlb_sqr
prefetch_sqr
io_mem_to_ooo_ctrl_sqr
io_mem_to_ooo_int_wb_sqr
io_mem_to_ooo_vec_wb_sqr
io_mem_to_ooo_wakeup_sqr
io_mem_to_ooo_iq_feedback_sqr
other_ctrl_sqr
```

env 接入要求：

```text
memblock_env_pkg import seq_pkg。
memblock_env 声明 memblock_virtual_sequencer vsqr。
memblock_env::build_phase 创建 vsqr。
memblock_env::connect_phase 将每个已创建 agent 的 .sqr 赋给 vsqr 对应字段。
如果某 agent sqr_sw 为 OFF 或 agent 未创建，对应字段保持 null。
vseq 启动某 agent sequence 前必须检查对应 p_sequencer.<agent>_sqr 非 null。
```

文字伪代码：

```text
memblock_env::build_phase:
  创建所有 agent。
  创建 vsqr = memblock_virtual_sequencer::type_id::create("vsqr", this)。

memblock_env::connect_phase:
  对每个 agent:
    如果 agent 存在且 agent.sqr 存在:
      vsqr.<agent>_sqr = agent.sqr。
    否则:
      vsqr.<agent>_sqr = null。
```

### 3.3 编译接入

更新：

```text
mem_ut/ver/ut/memblock/env/memblock_env_pkg.sv
mem_ut/ver/ut/memblock/seq/seq_pkg.sv
```

include 顺序：

```text
seq:
  base_seq_help/*.sv
  base_seq/*.sv
  ../env/src/memblock_virtual_sequencer.sv
  virtual_sequence/virtual_base_sequence.sv
  virtual_sequence/memblock_dispatch_real_smoke_vseq.sv

env:
  import seq_pkg
  memblock_env_cfg.sv
  memblock_rm.sv
  memblock_env.sv
```

`seq/seq.f` 当前已有：

```text
+incdir+./virtual_sequence
+incdir+../env/src
```

如果实际实现发现 include 注释列表缺失，应同步补充注释列表，保持可 review。

### 3.4 testcase 接入

更新：

```text
mem_ut/ver/ut/memblock/tc/src/basicTest.sv
mem_ut/ver/ut/memblock/tc/tc_pkg.sv
mem_ut/ver/ut/memblock/tc/tc.f
mem_ut/ver/ut/memblock/cfg/verif/project_cfg.mk
```

目标：

```text
默认 testcase:
  basicTest

默认 vseq:
  virtual_base_sequence

用户指定 vseq:
  make ... tc=basicTest ts=memblock_dispatch_real_smoke_vseq
```

旧 `tc_sanity.sv` testcase 处理：

```text
不需要为了 virtual sequence 机制改名。
旧 tc_sanity 是 testcase component，可作为 legacy testcase 保留。
新流程默认 testcase 使用 basicTest，不再通过 tc_sanity testcase 启动。
```

## 4. virtual_base_sequence 设计

### 4.1 类职责

`virtual_base_sequence` 只做公共调度能力，不包含具体场景：

```text
1. 继承 uvm_sequence。
2. 使用 `uvm_declare_p_sequencer(memblock_virtual_sequencer)` 声明 p_sequencer 类型。
3. body 默认为空。
4. 提供 require_agent_sqr() 类检查逻辑，启动 agent sequence 前确认 p_sequencer 对应字段非 null。
5. 不通过 uvm_top.find 查找 agent sequencer。
6. 不直接保存 agent sequencer handle，统一使用 p_sequencer.<agent>_sqr。
```

### 4.2 为什么不继续依赖 agent phase default_sequence

不推荐由 virtual sequence 在 main_phase 里再去改各 agent 的 `default_sequence`：

```text
UVM phase default_sequence 是各 sequencer 进入 main_phase 时读取。
env.vsqr.main_phase 的 virtual sequence 和各 agent sequencer.main_phase 同属 main_phase。
如果 virtual sequence 在 body 中再写 config_db，可能晚于 agent sequencer 读取 default_sequence。
```

因此本方案采用：

```text
basicTest 只设置 env.vsqr.main_phase.default_sequence。
各 agent 不再通过 tc_base/testcase 分散设置场景 default_sequence。
具体 vseq 在 body 中通过 uvm_do_on 系列宏显式启动 child sequence。
agent sequence 统一启动到 p_sequencer.<agent>_sqr。
顶层 orchestration sequence 启动到 p_sequencer。
```

这样调度顺序由一个 vseq 控制，避免 phase default_sequence 配置时序不确定。

### 4.3 文字伪代码

`virtual_base_sequence::body()`：

```text
进入 body。
不启动任何 child sequence。
直接返回。
```

`virtual_base_sequence::require_agent_sqr(agent_name, sqr)`：

```text
检查 p_sequencer 是否为 null。
如果 p_sequencer 为 null:
  打印 UVM_FATAL，说明当前 vseq 没有运行在 memblock_virtual_sequencer 上。

检查目标 agent sqr 是否为 null。
如果 sqr 为 null:
  打印 UVM_FATAL，说明 agent_name 对应 sequencer 未连接或该 agent sqr_sw 未打开。

如果 p_sequencer 和 sqr 均有效:
  返回，允许后续 uvm_do_on 启动 child sequence。
```

启动 agent default_sequence 的文字伪代码：

```text
先调用 require_agent_sqr(agent_name, p_sequencer.<agent>_sqr)。
如果检查通过:
  使用 uvm_do_on(default_seq, p_sequencer.<agent>_sqr) 启动该 agent 在当前 vseq 选择的 default_sequence。
如果当前场景只需要 idle:
  default_seq 使用 tcnt_default_sequence_base#(<agent_xaction>)。
如果专项场景需要 agent 自动生成 transaction:
  default_seq 可以使用该 agent 自带的 <agent>_default_sequence。
```

启动真实 agent base_sequence 的文字伪代码：

```text
先调用 require_agent_sqr(agent_name, p_sequencer.<agent>_sqr)。
如果检查通过:
  使用 uvm_do_on(base_seq, p_sequencer.<agent>_sqr) 启动目标 base_sequence。
base_sequence 在对应 agent sequencer 上产生或响应 transaction。
```

启动顶层 orchestration sequence 的文字伪代码：

```text
检查 p_sequencer 非 null。
使用 uvm_do_on(main_seq, p_sequencer) 启动顶层 orchestration sequence。
该 sequence 不直接驱动某个单一 agent 接口，而是组织 main table、dispatch service 和 end_test_check。
如果现有 sequence 以前使用 start(null)，需要保证它运行在 p_sequencer 上时不依赖错误类型的 p_sequencer。
```

## 5. memblock_dispatch_real_smoke_vseq 设计

### 5.1 调度目标

`memblock_dispatch_real_smoke_vseq` 统一调度当前 real dispatch smoke 需要的 sequence。

核心真实 DUT base sequence：

```text
memblock_lsqenq_dispatch_base_sequence
memblock_issue_dispatch_base_sequence
memblock_lsqcommit_dispatch_base_sequence
memblock_l2tlb_base_sequence
dcache_mem__access_base_sequence
sbuffer_mem_access_base_sequence
memblock_redirect_dispatch_base_sequence
memblock_main_dispatch_auto_build_main_table_base_sequence
```

可选真实 DUT base sequence：

```text
memblock_flushsb_base_sequence
```

说明：

```text
dcache_mem__access_base_sequence:
  real smoke 必须启动。
  它响应 DUT dcache/TileLink A 通道请求并驱动 D 通道 response。
  通过 p_sequencer.dcache_sqr 启动。

sbuffer_mem_access_base_sequence:
  real smoke 必须启动。
  它响应 DUT sbuffer A 通道请求并驱动 D 通道 response。
  通过 p_sequencer.sbuffer_sqr 启动。

memblock_redirect_dispatch_base_sequence:
  real smoke 必须启动。
  它不主动随机产生 redirect。
  它只从 common_data_transaction.pending_redirect_drive_q 消费已经由框架产生的 redirect payload。
  如果没有 pending redirect payload，它只驱动 idle。
  当 exception_redirect_replay_handler 识别到 redirect event 并 push_redirect_drive() 后，
  它负责把对应 redirect payload 驱动到 DUT redirect input。
  通过 p_sequencer.redirect_sqr 启动。

memblock_flushsb_base_sequence 当前由 memblock_main_dispatch_auto_build_main_table_base_sequence 内部 fork 启动。
第一版不在 vseq 外层重复启动，避免两个 flushSb producer 并行。
```

agent default sequence 启动原则：

```text
功能 base/responder sequence:
  本场景需要真实驱动或响应 DUT 接口，必须启动对应 base/responder sequence。
  例如 lsqenq/issue/commit/L2TLB/dcache/sbuffer/redirect。

安全 idle default sequence:
  只用于“该 agent 不参与本场景功能，但 driver 需要 sequence item 才能持续驱动安全 idle 值”的接口。
  使用 tcnt_default_sequence_base#(<xaction>) 或等价空/idle sequence。

不启动:
  如果该 agent 与本场景无关，并且 driver/env 已能保持安全默认值，则不启动任何 default_sequence。
```

因此，real dispatch smoke 第一版不应把所有 agent 都启动 default_sequence。每个 agent 只能落入以下三类之一：

```text
1. 启动功能 base/responder sequence。
2. 启动安全 idle default sequence。
3. 不启动。
```

不允许因为“统一调度”而无条件启动所有 `<agent>_default_sequence`。agent 自带 default sequence 可能会 randomize 并发送 transaction，只有该随机激励本身就是专项场景目标时才允许启动。

dcache_agent_agent 和 sbuffer_agent_agent 不使用安全 idle default 代替 memory responder；它们必须分别启动 `dcache_mem__access_base_sequence` 和 `sbuffer_mem_access_base_sequence`，否则 DUT 的 dcache/sbuffer memory request 没有 responder，real smoke 可能无法推进。

redirect_agent_agent 不使用安全 idle default 代替 redirect responder；它必须启动 `memblock_redirect_dispatch_base_sequence`。该 sequence 没有 pending redirect payload 时只 drive idle，不会凭空制造 redirect；如果框架已经识别到 redirect 事件，不启动它会导致 redirect payload 无法驱动，redirect/recovery 状态无法闭环。

如果后续专项 vseq 明确需要启动某个 agent 自带 `<agent>_default_sequence`，也必须通过 `uvm_do_on(default_seq, p_sequencer.<agent>_sqr)` 启动，不允许再通过 testcase phase default_sequence 分散配置。

### 5.2 调度顺序

`memblock_dispatch_real_smoke_vseq::body()`：

```text
进入 body。
检查 p_sequencer 非 null。
读取 plus/cfg，初始化 seq_csr_common。
检查本场景需要启动的 agent sequencer 是否已连接到 p_sequencer。
置位 memblock_sync_pkg::dispatch_real_smoke_active = 1。

fork 后台 responder/default sequence:
  对需要 idle 驱动的 agent:
    real smoke 第一版选择 tcnt_default_sequence_base#(<agent_xaction>) 或等价空/idle sequence 作为安全 idle default。
    使用 uvm_do_on(default_seq, p_sequencer.<agent>_sqr) 启动 default_seq。
  对无需驱动且 env/driver 已能保持安全默认值的 agent:
    不启动 default_sequence。
  使用 uvm_do_on(dcache_mem_seq, p_sequencer.dcache_sqr) 启动 dcache_mem__access_base_sequence。
  使用 uvm_do_on(sbuffer_mem_seq, p_sequencer.sbuffer_sqr) 启动 sbuffer_mem_access_base_sequence。
  使用 uvm_do_on(redirect_seq, p_sequencer.redirect_sqr) 启动 memblock_redirect_dispatch_base_sequence。
join_none

fork 核心有限流程 sequence:
  使用 uvm_do_on(lsqenq_seq, p_sequencer.lsqenq_sqr) 启动 memblock_lsqenq_dispatch_base_sequence。
  使用 uvm_do_on(issue_seq, p_sequencer.lintsissue_sqr) 启动 memblock_issue_dispatch_base_sequence。
  使用 uvm_do_on(commit_seq, p_sequencer.lsqcommit_sqr) 启动 memblock_lsqcommit_dispatch_base_sequence。
  使用 uvm_do_on(l2tlb_seq, p_sequencer.L2tlb_sqr) 启动 memblock_l2tlb_base_sequence。
  使用 uvm_do_on(main_seq, p_sequencer) 启动 memblock_main_dispatch_auto_build_main_table_base_sequence。
join

清零 memblock_sync_pkg::dispatch_real_smoke_active = 0。
返回 body；由 UVM objection drop 后的 1us drain_time 收束后台 responder/default sequence 尾部处理。
退出 body。
```

异常处理要求：

```text
如果任意 child sequence UVM_FATAL，仿真结束。
如果 main sequence 正常结束，应确保 dispatch_real_smoke_active 被清零。
如果出现 early return，需要先清零 dispatch_real_smoke_active。
后台 responder/default sequence 可能是 forever loop，不能作为普通 join 等待对象。
vseq 必须定义后台 sequence 的退出收束策略；当前 real smoke 使用 pre_body 设置 1us UVM drain_time，后续可把这些 responder sequence 改造为支持统一 stop 条件。
```

### 5.3 运行终止策略

第一版以 `memblock_main_dispatch_auto_build_main_table_base_sequence` 作为主流程结束条件。

实现逻辑：

```text
启动后台 responder/default sequence 后，不等待它们自然退出。
后台 responder/default sequence 包括 dcache_mem__access_base_sequence、sbuffer_mem_access_base_sequence、memblock_redirect_dispatch_base_sequence，以及少量确实需要 idle 驱动的安全 default sequence。
通过 uvm_do_on(main_seq, p_sequencer) 启动 main sequence 后，main sequence 负责 build main table、service monitor、end_test_check。
当 main sequence 和核心有限流程 sequence 结束后，vseq 清理 dispatch_real_smoke_active。
然后返回 body；继承的 post_body drop objection 后，UVM 使用 pre_body 设置的 1us drain_time 保留尾部处理窗口。
如果后续希望后台 responder 自然退出，需要先给对应 sequence 增加统一 stop 条件或 idle-stop 条件。
```

后台 responder/default sequence 退出收束时必须保证不在半拍 transaction 中间破坏握手。当前实现不再使用显式 disable fork，而是在主流程完成后通过 UVM drain_time 给 responder 1us 尾部窗口；后续更优方案是给 responder sequence 增加公共 stop 条件。

## 6. basicTest 改造方案

### 6.1 build_phase 行为

`basicTest::build_phase()`：

```text
进入 build_phase。
调用 super.build_phase。
重新读取 plus 参数。
构造 memblock_env_cfg。
设置所有 agent drv_mode = DRV_0。
设置所有 agent xz_sw = OFF。
将 cfg 写入 uvm_config_db，路径为 env。
设置 timeout。

读取 +VSEQ_MAIN。
该 plusarg 由 makefile 根据 ts 自动生成，用户正常通过 ts 指定 virtual sequence。
如果用户未指定:
  usr_test_vseq = "virtual_base_sequence"。
如果用户指定:
  usr_test_vseq = 用户传入字符串。

如果 usr_test_vseq 不是 "virtual_base_sequence":
  调用 factory.set_type_override_by_name("virtual_base_sequence", usr_test_vseq)。

将 env.vsqr.main_phase.default_sequence 设置为 virtual_base_sequence::type_id::get()。
获取 rst_vif。
退出 build_phase。
```

### 6.2 main_phase 行为

`basicTest` 不直接启动 real smoke sequence。

```text
basicTest main_phase 不实现具体 stimulus。
具体 stimulus 全部在 ts 指定的 virtual sequence 中实现。
basicTest 只读取 makefile 转出的内部 +VSEQ_MAIN。
```

### 6.3 与 tc_base/tc_dispatch_real_smoke 的关系

`basicTest` 不继承 `tc_base`。

实现后：

```text
tc_base:
  可保留给旧 testcase 使用。
  不再作为新 memblock real DUT flow 的默认入口。

tc_dispatch_real_smoke 及其派生 testcase:
  可保留一段时间作为 legacy smoke。
  默认回归入口切到 basicTest + ts。
  后续稳定后可迁移为 vseq 或删除。
```

## 7. 默认 testcase 与参数

更新默认配置：

```text
mem_ut/ver/ut/memblock/cfg/verif/project_cfg.mk
```

目标：

```makefile
tc := basicTest
ts := virtual_base_sequence
```

典型运行：

```text
默认空壳 vseq:
  make eda_run tc=basicTest mode=base_fun

真实 dispatch smoke:
  make eda_run tc=basicTest ts=memblock_dispatch_real_smoke_vseq mode=base_fun
```

makefile 规则：

```text
VCS:
  project_cfg_vcs.mk 已有 SIMV_OPTIONS += +VSEQ_MAIN=${ts}。

xrun:
  project_cfg_xrun.mk 需要补充 SIMV_OPTIONS += +VSEQ_MAIN=${ts}。

用户接口:
  用户、回归脚本和 make 命令统一使用 ts 指定 virtual sequence。
  不推荐用户直接通过 udr 或 plus_arg 手写 +VSEQ_MAIN。
  +VSEQ_MAIN 只是 basicTest 与 makefile 之间的内部传递参数。
```

如果回归脚本已有 `tc=tc_dispatch_real_smoke`、`tc=tc_dispatch_real_mixed_wb_smoke`，第一版不强行删除；新增等价 `tc=basicTest ts=<vseq>` 配置，验证通过后再迁移回归入口。

## 8. 与 memblock_main_dispatch_auto_build_main_table_base_sequence 的兼容

`memblock_main_dispatch_auto_build_main_table_base_sequence` 当前曾以如下方式启动：

```systemverilog
real_smoke_seq.start(null);
```

新流程不再使用 `start(null)` 作为推荐方式。

要求改为：

```text
memblock_dispatch_real_smoke_vseq 通过 uvm_do_on(main_seq, p_sequencer) 启动 main sequence。
main sequence 运行在 memblock_virtual_sequencer 上。
main sequence 如果不需要 p_sequencer，可以继续不访问 p_sequencer。
main sequence 不允许假设 m_sequencer 为 null。
```

兼容要求：

```text
该 sequence 不再假设自己由 tc_dispatch_real_smoke 启动。
如果需要 real smoke active 标志，由外层 vseq 负责置位/清零。
该 sequence 内部保留 build_main_table、service_real_dispatch_flow、end_test_check 逻辑。
```

## 9. 实现步骤

### 9.1 新增 memblock_virtual_sequencer

```text
创建 mem_ut/ver/ut/memblock/env/src/memblock_virtual_sequencer.sv。
定义 memblock_virtual_sequencer extends uvm_sequencer。
添加 uvm_component_utils。
为所有需要调度的 agent sequencer 声明类型化 handle。
不在 memblock_virtual_sequencer 内创建 agent sequencer。
不在 memblock_virtual_sequencer 内启动 sequence。
它只保存 env connect_phase 赋值进来的 agent sequencer handle。
```

### 9.2 新增 virtual_base_sequence

```text
创建 mem_ut/ver/ut/memblock/seq/virtual_sequence/virtual_base_sequence.sv。
定义 virtual_base_sequence。
添加 uvm_object_utils。
添加 uvm_declare_p_sequencer(memblock_virtual_sequencer)。
实现空 body。
实现 require_agent_sqr() 检查逻辑。
```

### 9.3 新增 memblock_dispatch_real_smoke_vseq

```text
创建 mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_vseq.sv。
继承 virtual_base_sequence。
实现 body。
body 中按第 5 节通过 uvm_do_on 系列宏调度 real dispatch smoke 所需 child sequence。
```

### 9.4 更新 seq_pkg

```text
在 seq_pkg.sv 中 include memblock_virtual_sequencer.sv。
在 memblock_virtual_sequencer.sv 后 include virtual_base_sequence.sv。
在 virtual_base_sequence.sv 后 include memblock_dispatch_real_smoke_vseq.sv。
确保 include 顺序满足继承关系。
```

### 9.5 更新 memblock_env

```text
在 memblock_env_pkg.sv 中 import seq_pkg，使 env 能看到 memblock_virtual_sequencer 类型。
在 memblock_env 中声明 memblock_virtual_sequencer vsqr。
在 build_phase 中创建 vsqr。
在 connect_phase 中将各 agent.sqr 赋给 vsqr 对应字段。
赋值前检查 agent 是否存在。
如果某 agent sqr_sw 为 OFF 或 agent.sqr 不存在，对应 vsqr 字段保持 null。
```

文字伪代码：

```text
memblock_env::build_phase:
  创建 cfg。
  创建所有 agent。
  创建 vsqr。

memblock_env::connect_phase:
  连接 monitor/RM FIFO。
  对每个 agent:
    如果 agent handle 非 null 且 agent.sqr 非 null:
      vsqr.<agent>_sqr = agent.sqr。
    否则:
      vsqr.<agent>_sqr = null。
```

### 9.6 更新 basicTest

```text
去除 basicTest 中对旧 tc_sanity testcase component 的依赖。
保留 +VSEQ_MAIN 解析，但其来源是 makefile 的 ts。
改为 factory.set_type_override_by_name("virtual_base_sequence", usr_test_vseq)。
将 env.vsqr.main_phase.default_sequence 设置为 virtual_base_sequence::type_id::get()。
不在 basicTest 中直接启动任何 base_sequence。
```

### 9.7 处理旧 tc_sanity testcase

```text
旧 tc_sanity testcase 不再作为新流程默认入口。
由于不再新增 tc_sanity vseq，不存在 class 重名冲突。
旧 tc_sanity.sv 可保留给 legacy testcase 使用。
如果项目决定清理旧 testcase，再单独做 legacy testcase 清理方案，不属于本方案必需项。
```

### 9.8 更新默认 testcase

```text
project_cfg.mk 默认 tc 切到 basicTest。
project_cfg.mk 默认 ts 切到 virtual_base_sequence。
project_cfg_vcs.mk 保持 +VSEQ_MAIN=${ts}。
project_cfg_xrun.mk 补充 +VSEQ_MAIN=${ts}。
保留用户显式 tc 覆盖能力。
保留用户显式 ts 覆盖能力。
```

## 10. 验证方案

### 10.1 编译验证

```text
make eda_compile tc=basicTest mode=base_fun
```

检查点：

```text
memblock_virtual_sequencer 编译通过。
virtual_base_sequence 编译通过。
virtual_base_sequence 的 p_sequencer 类型为 memblock_virtual_sequencer。
不新增 tc_sanity vseq，因此不会和旧 tc_sanity testcase 产生 class 重名。
memblock_dispatch_real_smoke_vseq 可被 factory 查到。
basicTest 可设置 env.vsqr.main_phase.default_sequence = virtual_base_sequence。
memblock_env 可创建 vsqr，并在 connect_phase 将各 agent.sqr 连接到 vsqr 对应字段。
```

### 10.2 默认空壳 vseq

```text
make eda_batch_run tc=basicTest mode=base_fun cfg=<basic smoke cfg>
```

预期：

```text
启动 basicTest。
未传 ts 时，makefile 默认 ts=virtual_base_sequence，basicTest 使用 virtual_base_sequence 空壳 vseq。
无随机 agent default_sequence 污染。
仿真能正常进入/退出或按现有 timeout 策略结束。
```

### 10.3 real dispatch vseq

```text
make eda_batch_run tc=basicTest ts=memblock_dispatch_real_smoke_vseq mode=base_fun cfg=<real dispatch cfg>
```

预期日志：

```text
basicTest 打印 usr_test_vseq_name=memblock_dispatch_real_smoke_vseq。
factory override virtual_base_sequence -> memblock_dispatch_real_smoke_vseq。
memblock_dispatch_real_smoke_vseq 启动。
memblock_lsqenq_dispatch_base_sequence 通过 p_sequencer.lsqenq_sqr 启动。
memblock_issue_dispatch_base_sequence 通过 p_sequencer.lintsissue_sqr 启动。
memblock_lsqcommit_dispatch_base_sequence 通过 p_sequencer.lsqcommit_sqr 启动。
memblock_l2tlb_base_sequence 通过 p_sequencer.L2tlb_sqr 启动。
dcache_mem__access_base_sequence 通过 p_sequencer.dcache_sqr 启动。
sbuffer_mem_access_base_sequence 通过 p_sequencer.sbuffer_sqr 启动。
memblock_main_dispatch_auto_build_main_table_base_sequence 通过 p_sequencer 启动。
main table ready。
end_test_check 执行。
dispatch_real_smoke_active 结束后清零。
```

## 11. 风险与约束

### 11.1 virtual_base_sequence 锚点约束

当前 `tc_sanity` 是 testcase component。新流程不再新增 `tc_sanity` vseq，也不再把 `tc_sanity` 作为 default_sequence 锚点。

实现约束：

```text
basicTest 必须使用 virtual_base_sequence 作为 factory override 原始类型。
basicTest 必须设置 env.vsqr.main_phase.default_sequence = virtual_base_sequence::type_id::get()。
project_cfg.mk 默认 ts 必须是 virtual_base_sequence。
不允许在 virtual_base_sequence.sv 内再定义 tc_sanity vseq。
virtual_base_sequence 必须声明 p_sequencer 类型为 memblock_virtual_sequencer。
```

目标实现：

```systemverilog
factory.set_type_override_by_name("virtual_base_sequence", usr_test_vseq);
uvm_config_db#(uvm_object_wrapper)::set(this,
    "env.vsqr.main_phase",
    "default_sequence",
    virtual_base_sequence::type_id::get());
```

### 11.2 agent 自带 default_sequence 不能默认全启动

很多 `<agent>_default_sequence` 会产生随机 transaction。real DUT flow 中默认启动它们会污染主表调度。

因此方案中的“所有 default_sequence”分为两类：

```text
安全 idle default:
  tcnt_default_sequence_base#(<xaction>)，body 为空，可用于保持 driver idle。

agent 自带 default_sequence:
  只在专门 sanity/random 场景 vseq 中按需启动。
  不在 memblock_dispatch_real_smoke_vseq 中默认全启动。
  一旦由 vseq 启动，必须通过 uvm_do_on(default_seq, p_sequencer.<agent>_sqr) 启动。
```

### 11.3 phase objection

child sequence 如果设置 `starting_phase`，会自己 raise/drop objection。

vseq 也运行在 `env.vsqr.main_phase.default_sequence` 下，应避免重复永久 objection：

```text
vseq 只在需要保护整个调度窗口时使用 starting_phase objection。
不允许 child sequence 永久不退出。
不允许外层 testcase 再手动长期 raise objection 等待未知 child sequence。
```

## 12. 最终 flow

```text
用户运行:
  make ... tc=basicTest ts=memblock_dispatch_real_smoke_vseq ...

makefile:
  将 ts 转成 +VSEQ_MAIN=${ts}。

basicTest build_phase:
  构造 env cfg。
  设置 env cfg。
  读取 makefile 转出的 +VSEQ_MAIN。
  factory override virtual_base_sequence 为用户指定 vseq。
  设置 env.vsqr.main_phase.default_sequence = virtual_base_sequence。

UVM main_phase:
  env.vsqr 创建 virtual_base_sequence。
  factory 返回 memblock_dispatch_real_smoke_vseq。
  memblock_dispatch_real_smoke_vseq.body 运行。

memblock_dispatch_real_smoke_vseq.body:
  检查 p_sequencer 非 null。
  检查本场景需要的 p_sequencer.<agent>_sqr 非 null。
  置 dispatch_real_smoke_active = 1。
  fork 对需要 idle 驱动的 agent 使用 uvm_do_on 启动安全 idle default_sequence。
  对无需驱动且已有安全默认值的 agent 不启动 default_sequence。
  fork 使用 uvm_do_on 在 p_sequencer.<agent>_sqr 上启动真实 DUT base_sequence。
  fork 使用 uvm_do_on 在 p_sequencer.dcache_sqr 上启动 dcache_mem__access_base_sequence。
  fork 使用 uvm_do_on 在 p_sequencer.sbuffer_sqr 上启动 sbuffer_mem_access_base_sequence。
  使用 uvm_do_on 在 p_sequencer 上启动 main table orchestration sequence。
  等待主流程结束。
  清 dispatch_real_smoke_active = 0。

main table sequence:
  build_main_table。
  service_real_dispatch_flow。
  end_test_check。
```

## 13. 执行中补充/修正（IMPLEMENTATION_DELTA）

### 13.1 basicTest 不配置各 agent phase default_sequence

[IMPLEMENTATION_DELTA]

来源：coding review 中确认 `basicTest` 配置所有 agent `main_phase.default_sequence` 会违反统一 vseq 调度规则。

原 plan：

```text
real smoke vseq 可按需启动安全 idle default_sequence。
basicTest 只设置 env.vsqr.main_phase.default_sequence。
```

实现调整：

```text
basicTest 不配置任何 agent phase default_sequence。
basicTest 只设置 env.vsqr.main_phase.default_sequence = virtual_base_sequence。
memblock_dispatch_real_smoke_vseq 只显式启动本场景需要的真实 base/responder sequence。
```

原因：

```text
如果 basicTest 配置 agent phase default_sequence，调度所有权会回到 testcase。
这会和“场景完全由 vseq 调度”的规则冲突。
移除后仿真会出现 agent sequencer no-default warning，但不影响 error/fatal 和 real smoke 功能流程。
```

影响范围：

```text
mem_ut/ver/ut/memblock/tc/src/basicTest.sv
AI_DOC/mem_ut_flow_doc/virtual_sequence_unified_dispatch_flow.md
AI_DOC/plan/test_framework/review_doc/undo/memblock_virtual_sequence_unified_dispatch_implementation_review_20260703.md
```

### 13.2 child sequence 使用显式 start() 而不是 uvm_do_on 宏

[IMPLEMENTATION_DELTA]

来源：coding 中为清晰管理后台 responder 对象和 fork 生命周期，采用显式 create + start。

原 plan：

```text
由 vseq 启动的 child sequence 通过 uvm_do_on 系列宏运行到对应 sequencer。
```

实现调整：

```text
先通过 type_id::create 创建 child sequence。
再调用 child_seq.start(p_sequencer.<agent>_sqr) 或 main_seq.start(p_sequencer)。
```

原因：

```text
显式 start() 保留 factory 创建能力，也能清楚表达后台 responder 的对象生命周期。
该方式仍满足“必须启动到 p_sequencer.<agent>_sqr 或 p_sequencer”的核心约束。
```

影响范围：

```text
mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_vseq.sv
```

### 13.3 basicTest 继承 tcnt_test_base 并自建 env

[IMPLEMENTATION_DELTA]

来源：coding 中确认继续继承 `tc_base` 会继承旧 testcase 的 agent default_sequence 配置，不利于统一 vseq 调度。

原 plan：

```text
basicTest 作为唯一 testcase 入口。
basicTest 设置 env.vsqr.main_phase.default_sequence。
```

实现调整：

```text
basicTest extends tcnt_test_base。
basicTest 自行创建 memblock_env_cfg。
basicTest 将 cfg 写入 env 路径。
basicTest 创建 memblock_env env。
basicTest 设置 env.vsqr.main_phase.default_sequence。
```

原因：

```text
绕开 tc_base 中 legacy agent default_sequence 设置。
避免旧 testcase 继承链污染新 vseq 调度入口。
```

影响范围：

```text
mem_ut/ver/ut/memblock/tc/src/basicTest.sv
```

### 13.4 远端 eda flow 需要转发 ts

[IMPLEMENTATION_DELTA]

来源：coding 验证中发现只在 project_cfg 中加入 `+VSEQ_MAIN=${ts}` 不足以覆盖远端 make 包装流程。

原 plan：

```text
makefile 将 ts 转成 +VSEQ_MAIN=${ts}。
```

实现调整：

```text
mem_ut/scr/verif/project_cfg_vcs.mk 和 project_cfg_xrun.mk 加入 +VSEQ_MAIN=${ts}。
mem_ut/ver/ut/memblock/sim/Makefile 将 ts 传给 remote_eda_make.sh。
remote_eda_make.sh 将 ts 加入 FORWARD_VARS。
远端 pid/launch log 文件名加入 ts，避免同一 tc 下不同 vseq 并发运行互相覆盖状态文件。
```

原因：

```text
实际 sim/Makefile include 的是 mem_ut/scr/verif/project_cfg.mk。
eda01 远端运行需要显式转发 ts，否则远端 make 会使用默认 virtual_base_sequence。
```

影响范围：

```text
mem_ut/scr/verif/project_cfg.mk
mem_ut/scr/verif/project_cfg_vcs.mk
mem_ut/scr/verif/project_cfg_xrun.mk
mem_ut/ver/ut/memblock/sim/Makefile
mem_ut/ver/ut/memblock/sim/remote_eda_make.sh
```
