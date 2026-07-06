# mem_ut Virtual Sequence 构建与仿真运行规则

本文约束 mem_ut/memblock 后续测试场景的构建入口和仿真运行入口。后续新增不同测试用例场景时，原则上都通过构建 `virtual_sequence` 实现；运行仿真时通过 makefile 的 `tc/ts` 参数选择 testcase 和 virtual sequence。

本文分为两套规则：

1. `virtual_sequence` 场景构建规则：指导如何新增不同测试用例场景。
2. 基于 `virtual_sequence` 的仿真运行规则：指导如何用指定 vseq 跑仿真。

---

## 1. Virtual Sequence 场景构建规则

### 1.1 基本原则

后续新增测试用例场景时，不应优先新增独立 testcase。默认采用：

```text
固定 testcase:
  basicTest

场景入口:
  一个继承 virtual_base_sequence 的 virtual sequence
```

规则：

1. 新测试场景必须优先实现为 `<scenario>_vseq extends virtual_base_sequence`。
2. testcase 只负责搭建 env、配置 cfg、设置 `env.vsqr.main_phase.default_sequence`。
3. 场景刺激、agent sequence 调度、base sequence 调度、场景内全局标志置位/清零，都放在对应 vseq 中。
4. 不允许通过新增 testcase 分散配置各 agent 的 phase `default_sequence` 来表达新场景。
5. 只有当场景需要完全不同的 env 拓扑、agent build 配置或 UVM phase 行为时，才允许提出新增 testcase，并必须在 plan 中说明为什么 vseq 无法表达。

### 1.2 统一调度结构

所有 memblock 顶层 vseq 使用统一结构：

```text
env.vsqr:
  类型为 memblock_virtual_sequencer。
  由 memblock_env 创建。
  在 memblock_env::connect_phase 中连接各 agent 的真实 sequencer。

virtual_base_sequence:
  所有顶层 vseq 的公共基类。
  使用 uvm_declare_p_sequencer(memblock_virtual_sequencer)。
  body 默认为空。

具体场景 vseq:
  继承 virtual_base_sequence。
  通过 p_sequencer.<agent>_sqr 调度各 agent sequence。
```

`memblock_virtual_sequencer` 只保存 sequencer handle：

```text
不创建 agent。
不启动 sequence。
不直接驱动 transaction。
不保存场景状态。
```

### 1.3 Sequencer 访问规则

场景 vseq 不允许使用以下方式查找 agent sequencer：

```text
uvm_top.find()
硬编码层级路径查找 sequencer
config_db 临时获取 agent sequencer
在 vseq 中缓存一套重复的 agent sequencer handle
```

必须使用：

```text
p_sequencer.<agent>_sqr
```

启动任何 agent sequence 前必须检查：

```text
p_sequencer != null
p_sequencer.<agent>_sqr != null
```

检查失败时使用 `uvm_fatal`，并说明哪个 agent sequencer 未连接或未打开。

### 1.4 Sequence 启动规则

由 vseq 启动的 agent `default_sequence` 和 `base_sequence` 均必须通过 UVM 标准宏启动到对应 agent sequencer：

```text
uvm_do_on(seq, p_sequencer.<agent>_sqr)
```

顶层 orchestration sequence 运行在 virtual sequencer 上：

```text
uvm_do_on(main_seq, p_sequencer)
```

不推荐在新流程中使用：

```text
seq.start(null)
seq.start(<硬编码查找出的 sequencer>)
```

如果旧 sequence 曾用 `start(null)`，迁移到 vseq 体系时必须确认：

```text
该 sequence 不假设 m_sequencer 为 null。
该 sequence 如果使用 p_sequencer，类型必须与 memblock_virtual_sequencer 匹配。
```

### 1.5 Agent Default Sequence 规则

本文中的 agent `default_sequence` 分两类：

```text
安全 idle default:
  tcnt_default_sequence_base#(<agent_xaction>)
  body 为空或只保持 idle，不主动产生随机 transaction。

agent 自带 default_sequence:
  可能会主动 randomize 并发送 transaction。
```

real DUT 主流程类场景不能无条件启动所有 agent default_sequence。每个 agent 必须按场景职责落入以下三类之一：

```text
功能 base/responder sequence:
  本场景需要真实驱动或响应 DUT 接口，启动对应 base/responder sequence。

安全 idle default sequence:
  该 agent 不参与本场景功能，但 driver 需要 sequence item 才能持续驱动安全 idle 值。
  启动 tcnt_default_sequence_base#(<agent_xaction>) 或等价空/idle sequence。

不启动:
  该 agent 与本场景无关，并且 driver/env 已能保持安全默认值。
```

因此，对无关 agent 不是“默认都启动 default_sequence”，而是先判断是否真的需要 idle 驱动。无关随机 transaction 会污染主表调度，agent 自带 default sequence 只有在专项场景明确需要时才允许启动。

memory responder 类 sequence 不是无关随机 default。若 DUT 主流程需要 dcache/sbuffer memory response，场景 vseq 必须显式启动对应 responder sequence，例如：

```text
dcache_mem__access_base_sequence:
  通过 p_sequencer.dcache_sqr 启动。
  响应 DUT dcache/TileLink A 通道请求并驱动 D 通道 response。

sbuffer_mem_access_base_sequence:
  通过 p_sequencer.sbuffer_sqr 启动。
  响应 DUT sbuffer A 通道请求并驱动 D 通道 response。
```

这类 responder sequence 仍必须由 vseq 通过 `uvm_do_on(responder_seq, p_sequencer.<agent>_sqr)` 启动，不允许回到 testcase phase default_sequence 分散配置。

如果专项场景确实要启动某个 agent 自带 default sequence，必须在该 vseq 中显式写清：

```text
为什么该 agent default_sequence 是本场景激励目标的一部分。
它启动在哪个 p_sequencer.<agent>_sqr 上。
它与主表、RM、monitor 或 end condition 的关系。
它的退出条件。
```

无论启动哪类 default sequence，都必须由 vseq 通过 `uvm_do_on(default_seq, p_sequencer.<agent>_sqr)` 启动，不允许再通过 testcase phase default_sequence 分散配置。

### 1.6 场景 Vseq 的文字伪代码要求

新增场景 plan 必须给出 vseq 的文字伪代码。伪代码必须描述实际行为，不能只写“调用某函数”。

推荐格式：

```text
<scenario>_vseq::body:
  检查 p_sequencer 非 null。
  检查本场景需要的 p_sequencer.<agent>_sqr 非 null。
  读取 plus/cfg 或公共参数快照。
  初始化本场景需要的公共状态。
  置位本场景需要的 active 标志。
  fork 启动本场景需要的 agent default_sequence。
  fork 启动本场景需要的 agent base_sequence。
  启动本场景 main orchestration sequence。
  等待主流程结束或各 child sequence 达到定义的退出条件。
  清理 active 标志和本场景临时状态。
  退出 body。
```

### 1.7 退出和清理规则

vseq 必须定义清晰的退出条件：

```text
main orchestration sequence 完成。
核心 agent base_sequence 达到自身 terminal condition。
responder 类 sequence 达到 idle-stop、terminal_done 或显式 stop 条件。
```

不允许依赖以下方式作为正常退出：

```text
UVM timeout。
强行 kill 正在驱动接口的 sequence。
无限 fork 后不 join 或不定义退出。
```

如果 vseq 设置全局 active 标志，例如 `dispatch_real_smoke_active`，必须保证：

```text
启动 child sequence 前置位。
正常退出前清零。
遇到 early return、disable fork 或 fatal 前尽量先清理。
```

### 1.8 Package/Filelist 接入规则

新增 vseq 或 virtual sequencer 相关文件时必须检查：

```text
seq/seq_pkg.sv include 顺序。
seq/seq.f incdir。
env/memblock_env_pkg.sv 是否需要 import seq_pkg。
memblock_env 是否创建并连接 vsqr。
basicTest 是否仍只设置 env.vsqr.main_phase.default_sequence。
```

推荐 include 顺序：

```text
seq:
  base_seq_help/*.sv
  base_seq/*.sv
  ../env/src/memblock_virtual_sequencer.sv
  virtual_sequence/virtual_base_sequence.sv
  virtual_sequence/<scenario>_vseq.sv

env:
  import seq_pkg
  memblock_env_cfg.sv
  memblock_rm.sv
  memblock_env.sv
```

---

## 2. 基于 Virtual Sequence 的仿真运行规则

### 2.1 用户运行入口

memblock 新场景默认使用：

```text
tc=basicTest
ts=<virtual_sequence_class_name>
```

典型命令：

```text
make eda_run tc=basicTest ts=<scenario_vseq> mode=<mode> cfg=<cfg>
make eda_batch_run tc=basicTest ts=<scenario_vseq> mode=<mode> cfg=<cfg>
```

`tc` 表示 UVM testcase，`ts` 表示 main virtual sequence。

### 2.2 Makefile 与 Plusarg 映射

用户和回归脚本统一使用 `ts` 指定场景。`+VSEQ_MAIN` 只作为 makefile 到 `basicTest` 的内部传递参数。

规则：

```text
用户接口:
  ts=<virtual_sequence_class_name>

内部 plusarg:
  +VSEQ_MAIN=${ts}
```

不推荐用户直接通过以下方式指定 vseq：

```text
udr="+VSEQ_MAIN=<scenario_vseq>"
plus_arg="+VSEQ_MAIN=<scenario_vseq>"
```

VCS 和 xrun 路径都必须保证传递：

```text
SIMV_OPTIONS += +VSEQ_MAIN=${ts}
```

### 2.3 默认配置

默认入口应保持为空壳 vseq：

```makefile
tc := basicTest
ts := virtual_base_sequence
```

未显式传 `ts` 时：

```text
basicTest 启动 virtual_base_sequence。
virtual_base_sequence body 为空。
不产生场景激励。
```

### 2.4 basicTest 行为规则

`basicTest` 只负责选择并挂载 vseq，不直接实现具体场景激励。

文字伪代码：

```text
basicTest::build_phase:
  构造 env cfg。
  将 env cfg 写入 uvm_config_db。
  读取 +VSEQ_MAIN。
  如果 +VSEQ_MAIN 未传入:
    使用 virtual_base_sequence。
  如果 +VSEQ_MAIN 传入具体 vseq:
    使用 factory override 将 virtual_base_sequence 替换成该 vseq。
  设置 env.vsqr.main_phase.default_sequence = virtual_base_sequence::type_id::get()。
```

`basicTest` 不应：

```text
直接 start 场景 base_sequence。
直接配置各 agent phase default_sequence。
直接置位/清零具体场景 active 标志。
```

这些行为必须放到具体 vseq 中。

### 2.5 仿真前检查

基于 vseq 跑仿真前必须检查：

```text
ts 指定的 class 已 include 到 seq_pkg。
ts 指定的 class 继承 virtual_base_sequence。
virtual_base_sequence 已声明 p_sequencer 类型为 memblock_virtual_sequencer。
env.vsqr 存在且类型为 memblock_virtual_sequencer。
该 vseq 需要的 agent sqr_sw 已打开。
该 vseq 需要的 p_sequencer.<agent>_sqr 会在 env connect_phase 被连接。
VCS/xrun makefile 都会传 +VSEQ_MAIN=${ts}。
```

如果检查失败，应在编译或仿真早期 `uvm_fatal`，不要在运行中静默跳过核心 child sequence。

### 2.6 仿真日志检查点

运行新 vseq 时，日志至少应能确认：

```text
UVM_TESTNAME = basicTest。
ts 被 makefile 转成 +VSEQ_MAIN。
basicTest 读取到目标 usr_test_vseq。
factory override virtual_base_sequence -> <scenario_vseq>。
env.vsqr 已创建。
<scenario_vseq>.body 已启动。
本场景需要的 p_sequencer.<agent>_sqr 非 null。
关键 child sequence 已通过 p_sequencer.<agent>_sqr 或 p_sequencer 启动。
场景结束时 active 标志清零。
```

### 2.7 回归配置规则

新增回归场景时，回归配置不应新增 testcase 名字表达场景差异。推荐写法：

```text
tc=basicTest
ts=<scenario_vseq>
cfg=<scenario_cfg>
mode=<scenario_mode>
```

不同场景差异通过以下方式表达：

```text
ts:
  选择不同 virtual sequence。

cfg/plus:
  选择该 vseq 内部的参数、权重、directed 模式和规模。

mode:
  选择仿真目录、日志、波形、覆盖率等运行模式。
```

### 2.8 失败定位规则

如果基于 vseq 的仿真失败，优先按以下顺序定位：

```text
1. make 命令是否传了正确 tc=basicTest ts=<scenario_vseq>。
2. makefile 是否把 ts 转成 +VSEQ_MAIN=${ts}。
3. basicTest 是否读取到正确 +VSEQ_MAIN。
4. factory override 是否成功。
5. env.vsqr 是否创建并连接 agent sqr。
6. p_sequencer 是否为 memblock_virtual_sequencer。
7. 目标 p_sequencer.<agent>_sqr 是否非 null。
8. child sequence 是否通过 uvm_do_on 启动到正确 sequencer。
9. child sequence 自身是否达到 terminal condition。
10. main orchestration sequence 是否完成并触发 end check。
```

不得直接把失败归因于 DUT 行为，除非已经确认 vseq 选择、sequencer 连接、child sequence 启动和测试框架状态推进均正确。
