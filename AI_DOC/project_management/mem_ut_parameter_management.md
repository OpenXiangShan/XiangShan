# mem_ut 参数分类管理说明

## 1. 目标

本文用于记录 `mem_ut` 测试环境中参数的归属边界，避免环境组件配置、测试框架公共参数、测试用例个性化参数、编译期宏参数混用。

核心原则是：参数必须先按作用对象分类，再决定放置位置和读取方式。

## 2. 参数分类准则

### 2.1 环境组件控制参数

环境组件控制参数统一由 `memblock_env_cfg` 管理。

这类参数的作用对象是 UVM 环境组件本身，例如 agent 是否开启、driver/monitor/sequencer 是否工作、agent channel 编号、agent driver/monitor 模式等运行期组件行为。

典型字段包括：

- `sqr_sw`
- `drv_sw`
- `mon_sw`
- `xz_sw`
- `channel_id`
- agent 级别的 driver mode / monitor mode / sequencer mode
- agent runtime mode / active mode 等组件运行期控制

约束规则：

- 公共默认值应在 `memblock_env_cfg` 中可见。
- 个人本地覆盖继续通过 `user_cfg.local.sv` 和 `<field>_valid` 机制完成。
- 不应通过测试框架参数直接修改 agent cfg，除非有明确桥接逻辑并写清优先级。

### 2.2 测试框架公共参数

测试框架公共参数统一由 `seq_csr_common` 管理。

这类参数的作用对象是公共 dispatch 测试框架、公共 sequence、公共 helper 或公共数据生成流程。它们不是某一个 testcase 的私有参数，而是多个 sequence/helper 会共享读取的参数快照。

典型字段包括：

- 主表生成规模和生成模式，例如 `main_trans_num`、`use_manual_main_table`
- op 类型权重，例如 load/store/atomic/prefetch 权重
- runtime发射和入队行为上限，例如`enq_per_cycle`、`load_pip_num_limit`、`sta_pip_num_limit`、`std_pip_num_limit`
- 自动主表虚拟地址窗口、TLB 物理地址映射窗口、PTE 权重和 MDP 字段权重
- send priority 相关公共参数
- replay / redirect / flushSb / L2TLB responder 等公共 sequence 行为参数
- 公共 timeout / idle stop / max cycle 参数

约束规则：

- 参数权重或赋值可以通过 `plus.sv` 指定。
- `plus.sv` 是输入解析层，`seq_csr_common` 是公共框架参数的最终读取入口。
- 公共 helper、公共 sequence、公共 transaction 约束不应长期直接读取 `plus::MEMBLOCK_*`，而应读取 `seq_csr_common::get_*()`。
- `seq_csr_common` 只保存测试框架参数，不保存 DUT CSR 实时状态、不保存运行期队列状态、不保存 monitor 采样结果。

主表地址窗口与 TLB 物理映射窗口规则：

- `MEMBLOCK_MAIN_VADDR_BASE/RANGE` 只控制自动主表 normal transaction 的 `src_0/imm/vaddr` 生成范围。
- `MEMBLOCK_PADDR_BASE/RANGE` 只控制 `tlb_map_builder` 为 TLB entry 选择的物理 PPN 范围。
- 两组参数默认数值可以相同以保持 Bare smoke 兼容，但参数语义和 consumer 必须独立；translated testcase 可以配置不同 VA/PA 窗口。
- manual directed 和 boundary profile 地址不由 `MEMBLOCK_MAIN_VADDR_BASE/RANGE` 全局拦截，避免破坏异常地址和边界地址构造。

### 2.2.1 DUT物理结构、runtime行为限制和集中收敛规则

参数必须按以下三层建立单一权威：

```text
编译期物理结构：
  memblock_compile_params.svh中的MEMBLOCK_DUT_*宏
  -> memblock_dispatch_types.sv中的同名typed localparam
  -> interface/driver/scheduler/物理循环直接读取

runtime行为参数：
  plus.sv -> seq_csr_common快照/getter
  -> 只表达本testcase使用量、权重、随机模式或时序策略

runtime资源收敛：
  seq_csr_common::validate_and_clamp()
  -> apply_runtime_resource_limits()
  -> 统一按compile localparam执行fatal或warning+clamp
```

必须遵守：

1. DUT interface数组维度、packed字段宽度、物理slot/pipe/port数量、connect-time capability只能由`memblock_compile_params.svh`配置，不得在`plus.sv`或testcase cfg中建立同义镜像。
2. `memblock_dispatch_types.sv`可以为`seq_pkg`暴露同名typed localparam，但不得再次写数值；agent package因编译顺序不能依赖`seq_pkg`时直接消费同一宏。
3. 物理循环、slot越界、fired-mask布局和scheduler物理扫描上限直接读取compile宏/localparam，不经过runtime getter。
4. runtime plus只保留行为限制。例如`MEMBLOCK_ENQ_PER_CYCLE`控制固定模式每拍候选数，三类`MEMBLOCK_*_PIP_NUM_LIMIT`控制本testcase最多使用多少issue pipe；这些值不能扩大物理资源。
5. 所有直接受物理资源约束的runtime参数必须集中在`seq_csr_common::apply_runtime_resource_limits()`处理。其它sequence/helper不得再次写固定上限或复制clamp。
6. `check_*`函数只做纯校验，不修改runtime字段；需要fatal/clamp并写回参数的函数使用`apply_`、`normalize_`或现有`validate_and_clamp()`入口。
7. 删除硬件结构plus参数时必须同时删除字段定义、加载、default cfg、runtime快照、getter和全部consumer；除非用户明确要求兼容迁移，不为已删除名称保留扫描、warning、fatal或wrapper。
8. 后续物理参数变化时应修改当前版本profile提供的compile宏，但必须先确认全部显式interface、xaction、
   driver、monitor、connect和setter consumer已经参数化；当前V2 LSQ字段链尚未满足该条件，因此由compile
   consistency拒绝非6/6/4 tuple，不能只改宏值假装完成版本切换。

当前LSQ enqueue和scalar issue规则：

- `MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM`是LSQ enqueue物理slot数，当前V2固定为6。
- `MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH/MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH`是V2每拍load/store element物理上限，
  当前固定为6/4；它们不是必须保留的LQ/SQ空项数。
- 当前interface/xaction/driver/setter显式展开6个slot，`check_compile_param_consistency()`必须在激励前要求
  slot/load/store tuple精确为6/6/4。其它tuple只有在对应profile参数化全部显式consumer后才能开放。
- `MEMBLOCK_DUT_LOAD_PIPE_NUM/MEMBLOCK_DUT_STA_PIPE_NUM/MEMBLOCK_DUT_STD_PIPE_NUM`是scalar issue物理pipe数，V2默认3/2/2。
- `MEMBLOCK_ENQ_PER_CYCLE`必须位于`[1:MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM]`，越界直接fatal。
- `MEMBLOCK_ENQ_PER_CYCLE_RAND_EN=1`时，`get_enq_per_cycle()`按`ZERO/MIDDLE/MAX`三类总权重在`[0:MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM]`内采样；MIDDLE的`-1`表示AUTO，派生为物理slot数减1。
- 三类enqueue权重属于runtime行为概率，不镜像物理结构。`ZERO_WEIGHT`允许成为唯一非零权重，
  `ZERO/MIDDLE/MAX=1/0/0`表示idle-only；只有三类全0非法。总权重使用`longint unsigned`逐项累加，
  避免32-bit表达式先溢出。zero-only不会消费uid或产生terminal，只能用于存在外部结束条件的场景；
  非空主表的主动flow若没有外部结束条件，将按既有no-progress/UVM timeout策略保持等待。
- `collect_lsq_candidates()`每拍只采样一次总slot目标，再分别按编译期load/store 6/4和实际LQ/SQ free count截断；不得新增同义per-type runtime limit，也不得复制RTL registered credit公式要求额外预留6/4空项。
- 三类`*_PIP_NUM_LIMIT`按对应compile pipe数执行warning+clamp；三类随机开关只决定固定返回limit还是在`[1:limit]`采样。
- `MEMBLOCK_REAL_LSQ_ENQ_MAX`、`MEMBLOCK_REAL_ENQ_WIDTH`和三个`MEMBLOCK_REAL_*_PIPE_NUM`已经退出配置系统，不得重新引入兼容字段或wrapper。

### 2.3 测试用例个性化参数

测试用例个性化参数通过 Makefile `cfg=<cfg_name>` 和必要的用户 `plus_arg` 控制。

这类参数只服务于某一个 testcase 或少量 directed testcase，用于选择该 testcase 的场景、开关、debug 模式或本地 preset。它们不是公共框架必须统一消费的参数。

允许场景：

- testcase 本地 directed 开关
- testcase 专用 debug knob
- testcase 内部用于选择某种 smoke/preset 的参数
- 用户命令行覆盖某个 testcase 默认值

约束规则：

- testcase 个性化参数不直接散落写在 testcase 源码中；应采用“指定 testcase + 指定 Makefile cfg”的方式管理。
- 每个需要 preset 的 testcase 应新建或绑定一个专用 cfg 文件，在 cfg 文件中集中写
  `+MEMBLOCK_*=...` 形式的 cfg 项；cfg 文件不是 SystemVerilog 源码，不写
  `plus::MEMBLOCK_* = ...` 赋值语句。
- testcase 源码不负责选择、加载或解析 cfg，不新增 `memblock_tc_plus_cfg_loader.sv`
  这类 SV cfg loader。
- Makefile 负责通过 `cfg=<cfg_name>` 选择 `seq/plus_cfg/<cfg_name>.cfg`，并把 cfg 中有效的
  `+KEY=VALUE` 行展开成 runtime plusargs 传给仿真器；不依赖 SV testcase 解析 cfg。
- 展开 cfg 时，Makefile 会过滤掉被用户 `plus_arg` 覆盖的同名 key，再追加用户
  `plus_arg`；这样 `plus.sv` 用 `$value$plusargs` 读取首个匹配值时，仍能保证用户显式
  plusarg 优先级高于 preset cfg。
- 公共 helper 仍应只读 `seq_csr_common`，不要因为 testcase preset cfg 设置 plus 而绕过公共入口。
- 如果某个 testcase 参数开始被多个公共 sequence/helper 共同依赖，应升级为测试框架公共参数，迁入 `seq_csr_common` 管理。
- `plus_arg` 追加在 Makefile 展开的 cfg plusargs 之后，且 cfg 展开阶段会过滤同名 key，因此用户命令行 plusarg 优先级高于 testcase cfg preset。

当前 lintsissue 非阻塞发射参数：

- `MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN=0`：默认保持阻塞等待行为，driver 等当前 xaction 中所有 valid port fire 后返回。
- `MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN=1`：driver 每个 xaction 只采样一次 ready，只把真实 `valid&&ready` port 置入 `memblock_dispatch_fired_mask`；未 ready item 不从 issue queue 删除，下轮继续仲裁。
- 该参数属于公共测试框架参数，读取路径必须是 `plus.sv -> seq_csr_common::get_dispatch_issue_nonblocking_en() -> xaction`，agent driver 只读 xaction 字段，不直接访问 `plus` 或 `seq_csr_common`。

测试用例添加规则：

- 新增 testcase 时，如果需要定制 `MEMBLOCK_*` 参数，必须同时新增或指定该 testcase 对应的 cfg 文件，并在运行命令或回归 ini 中显式写 `cfg=<cfg_name>`。
- cfg 文件命名应能反查 testcase，例如 `tc_dispatch_real_store_smoke.cfg` 对应 `tc_dispatch_real_store_smoke`。
- cfg 文件中直接写该 testcase 需要覆盖的 `+MEMBLOCK_*=...` cfg 项。
- testcase 中不新增散落的 `plus::MEMBLOCK_*` 赋值；已有 testcase 若继续演进，应逐步迁移到 cfg 文件。
- 用户命令行 plusarg 的优先级需要高于 testcase cfg 默认值；cfg 只提供 testcase preset，不应无条件覆盖用户显式指定的值。

推荐运行方式：

```bash
make eda_run tc=tc_dispatch_real_store_wb_smoke mode=base_fun cfg=tc_dispatch_real_store_wb_smoke
make eda_run tc=tc_dispatch_real_store_wb_smoke mode=base_fun cfg=tc_dispatch_real_store_wb_smoke
```

当前 dispatch 的完成来源约束：

- `MEMBLOCK_STA_REAL_WB_PASS_EN=1`：STA IQ feedback hit 不作为 normal pass，等待真实 STA writeback monitor。
- STD 没有 `MEMBLOCK_STD_REAL_WB_PASS_EN` runtime 参数；V2 下 issueStd accept 只记录 dispatched，
  必须等待真实 `writebackStd_0/1` monitor event 才能设置 STD writeback/pass。

STA 的早期 bring-up 兼容路径仍可按既有参数显式控制；STD 不提供 synthetic pass 兼容开关，
缺失或无法唯一归一化的真实 writeback 会在 adapter 中 `uvm_fatal`，避免主动 flow 静默卡住。

### 2.4 编译期宏参数

编译期宏参数通过统一宏文件管理。

这类参数不是 runtime plusarg，通常用于编译期宽度、feature 开关、条件编译、connect 连接选择和默认宏常量。

适合放入宏文件的内容包括：

- 编译期 `define`
- 固定宽度宏
- feature 编译开关
- 条件编译控制
- connect/interface 连接阶段必须使用的静态开关
- 不应由 runtime plusarg 修改的静态常量

当前统一宏文件为：

```text
mem_ut/ver/ut/memblock/cfg/memblock_compile_params.svh
```

约束规则：

- 需要指定宏参数时，应优先通过统一宏文件设置。
- 宏文件中每个宏参数都必须提供默认值，保证未显式覆盖时编译行为确定。
- interface/connect-time 静态参数统一归入宏参数控制，不再单独散落为 plus、testcase 或 env cfg 临时入口。
- 不应把编译期宏散落在 sequence、agent、driver、monitor 或 testcase 内部。
- 宏文件只管理编译期参数，不承担 runtime 配置职责。

### 2.5 校验函数命名规则

后续新增合法性检查、模式一致性检查或上下文一致性检查函数时，函数名必须带
`check_` 前缀。

适用场景包括：

- CSR/runtime 状态与接口字段一致性检查
- opcode/fuOpType 推导结果与 DUT monitor 字段一致性检查
- L2TLB req `s2xlate` 与 runtime CSR、`hyperinst` 语义一致性检查
- 参数取值范围、权重组合、模式组合的合法性检查

`check_` 函数只表达校验或诊断，不承担构造 key、插入表项、修改 runtime 状态或驱动
interface 的职责。构造、填充、插入、驱动类函数继续使用 `make_`、`build_`、
`fill_`、`insert_`、`drive_` 等动词，不使用 `check_`。
