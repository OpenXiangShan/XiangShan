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
- 发射和入队并发上限，例如 `enq_per_cycle`、`load_pip_num`、`sta_pip_num`、`std_pip_num`
- DUT 真实接口宽度和扫描窗口镜像，例如 `real_enq_width`、`real_lsq_enq_max`、`real_load_pipe_num`
- 地址范围、PTE 权重、MDP 字段权重
- send priority 相关公共参数
- replay / redirect / flushSb / L2TLB responder 等公共 sequence 行为参数
- 公共 timeout / idle stop / max cycle 参数

约束规则：

- 参数权重或赋值可以通过 `plus.sv` 指定。
- `plus.sv` 是输入解析层，`seq_csr_common` 是公共框架参数的最终读取入口。
- 公共 helper、公共 sequence、公共 transaction 约束不应长期直接读取 `plus::MEMBLOCK_*`，而应读取 `seq_csr_common::get_*()`。
- `seq_csr_common` 只保存测试框架参数，不保存 DUT CSR 实时状态、不保存运行期队列状态、不保存 monitor 采样结果。

LSQ enqueue 宽度与每拍入队数量规则：

- `MEMBLOCK_REAL_LSQ_ENQ_MAX` 是测试框架统一的 LSQ enqueue slot 宽度镜像，默认按当前 8-wide DUT 接口配置。
- `MEMBLOCK_REAL_ENQ_WIDTH` 作为历史兼容字段保留，但必须与 `MEMBLOCK_REAL_LSQ_ENQ_MAX` 相等；不相等时 `seq_csr_common::validate_and_clamp()` 必须 fatal。
- 所有 LSQ enqueue candidate、xaction slot 清理、slot 填充和 response 读取逻辑必须通过 `seq_csr_common::get_real_enq_width()` 或同等公共 getter 获取宽度，不允许在 sequence/helper 中写死 4 或 8。
- `MEMBLOCK_ENQ_PER_CYCLE` 是固定模式下每拍最多尝试 admission 的数量，必须在 `[1:MEMBLOCK_REAL_ENQ_WIDTH]` 内；超过范围直接 fatal，不做 clamp。
- `MEMBLOCK_ENQ_PER_CYCLE_RAND_EN=1` 时，`seq_csr_common::get_enq_per_cycle()` 每次在 `[1:MEMBLOCK_REAL_ENQ_WIDTH]` 内均匀随机；关闭时返回固定 `MEMBLOCK_ENQ_PER_CYCLE`。
- 如果后续 DUT 配置改变 LSQ enqueue 宽度，应先更新 `MEMBLOCK_REAL_LSQ_ENQ_MAX`，并同步保持 `MEMBLOCK_REAL_ENQ_WIDTH` 一致，相关队列和随机范围自动跟随公共 getter。

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
make eda_run tc=tc_dispatch_real_store_wb_smoke mode=base_fun cfg=tc_dispatch_real_store_wb_smoke plus_arg="+MEMBLOCK_STD_REAL_WB_PASS_EN=0"
```

当前 dispatch 默认要求真实 STA/STD writeback/pass 路径：

- `MEMBLOCK_STA_REAL_WB_PASS_EN=1`：STA IQ feedback hit 不作为 normal pass，等待真实 STA writeback monitor。
- `MEMBLOCK_STD_REAL_WB_PASS_EN=1`：STD issue accept 不注入 synthetic pass，等待真实 STD writeback monitor。

需要早期 bring-up 兼容路径时，必须在 testcase cfg 或用户 `plus_arg` 中显式设置对应参数为 0。

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
