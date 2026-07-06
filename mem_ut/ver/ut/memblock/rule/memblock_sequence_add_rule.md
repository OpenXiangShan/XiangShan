# memblock sequence 添加规则

## 触发条件

当新增、迁移、重构或修改任意 memblock UVM sequence 时，必须使用本规则。范围包括：

- base sequence
- virtual sequence
- responder sequence
- scenario sequence
- 继承 base sequence 的 agent 专用 sequence
- sequence 使用的公共参数、transaction 默认值和随机约束入口

agent 目录下自动生成的 `<agent_name>_default_sequence.sv` 只保留为 agent 的安全默认/空闲驱动入口。真正的测试场景、公共 base、responder、memory access 和多 agent 联动 sequence 不再放入 agent 目录，也不继续新增到 `tc/src` 下。

## 通用中文注释规则

新增或修改 sequence、transaction、公共状态表、helper、reference model 相关核心字段时，必须同时遵循：

```text
mem_ut/ver/ut/memblock/rule/memblock_code_comment_rule.md
```

核心字段含义、置位/清零时机、状态转移影响必须在源码中添加中文注释。

## 目录归属

新增 sequence 必须放在：

```text
mem_ut/ver/ut/memblock/seq
```

目录划分规则：

| 类型 | 目录 | 说明 |
| --- | --- | --- |
| base sequence | `mem_ut/ver/ut/memblock/seq/base_seq` | 公共基类、公共状态、内存模型、通用 builder helper |
| virtual/scenario sequence | `mem_ut/ver/ut/memblock/seq/virtual_sequence` | 顶层 vseq、场景 seq、responder seq、agent 联动 seq |
| soft test sequence | `mem_ut/ver/ut/memblock/seq/base_seq_help/soft_test` | software-only smoke 或纯软件闭环 sequence |
| plus cfg | `mem_ut/ver/ut/memblock/seq/plus_cfg` | runtime plusarg cfg 文件，不放 SV sequence |

## soft_test 函数边界规则

后续在 `mem_ut` 中新增、迁移或重命名 sequence 函数/task 时，必须先判断该函数是否会被真实 DUT 驱动 flow 使用。

判断标准：

1. 如果该函数会在真实 DUT flow 中被调用，才允许放在 `base_seq`、公共 helper、真实 agent sequence 或公共测试框架中。
2. 如果该函数只服务于 software-only smoke、soft replay、人工构造 event、人工推进状态、直接调用 handler 或直接模拟 issue/commit/writeback，而真实 DUT 驱动 flow 不需要调用，则必须放在对应 `soft_test` sequence 内部实现。
3. `mem_ut` 公共测试框架中实现的函数，应当是正式 DUT flow 驱动过程中能使用到的公共函数，或者是公共数据结构/状态表必须提供的基础 API。
4. 不允许为了 soft test 方便，把只给 soft test 使用的 helper 放到 `memblock_dispatch_base_sequence` 或其它公共 framework class 中。

这里的“真实 DUT flow 使用”包括：

- 真实 agent sequence 驱动 DUT interface 时调用，例如 LSQ enqueue、lintsissue、LSQ commit、L2TLB responder、redirect sequence。
- 真实 DUT output monitor 采集事件后，service loop、monitor adapter、writeback/replay/redirect/commit handler 调用。
- 真实 DUT flow orchestration sequence 为服务真实接口和 monitor 而调用。

不属于“真实 DUT flow 使用”的典型场景：

- soft test 人工构造 pass/replay/fault writeback event。
- soft test 人工调用 `mark_issue_fire()` 模拟 issue 已发射。
- soft test 人工调用 LSQ software model 推进 admission/commit/deq。
- soft test 为了验证公共状态机闭环而直接调用 handler。

实现要求：

1. 新增函数前先用 `rg` 检查调用点，确认调用者属于真实 DUT flow 还是 soft_test flow。
2. 如果函数只被 `seq/base_seq_help/soft_test` 下的 sequence 调用，则函数声明和实现也必须放在对应 soft_test sequence 中。
3. 如果真实 DUT flow 和 soft_test flow 需要共享同一段状态更新逻辑，应把真正公共的逻辑放到核心 helper 中，例如 `issue_queue_scheduler`、`issue_field_assigner`、`writeback_status_handler`、`lsq_commit_handler`、`common_data_transaction`；soft_test sequence 只保留自己的 wrapper。
4. 文档中描述函数时必须写完整 class scope，例如 `soft_test_memblock_dispatch_smoke_sequence::submit_writeback_event()` 或 `issue_queue_scheduler::prepare_issue_route_for_uid()`，不要只写裸函数名。
5. 如果发现已有公共 framework 函数实际只给 soft_test 使用，应迁移到对应 soft_test sequence，并同步更新源码分析文档。

命名建议：

- base sequence 使用 `memblock_*_base_sequence.sv` 或 `<domain>_base_sequence.sv`。
- virtual sequence 使用 `memblock_*_vseq.sv`。
- responder sequence 使用 `memblock_*_rsp_sequence.sv` 或 `<agent>_mem_access_sequence.sv`。
- 文件名、class 名、`uvm_object_utils` 注册名必须一致可追踪。

## 编译接入

新增 sequence 后必须同步检查：

```text
mem_ut/ver/ut/memblock/seq/seq.f
mem_ut/ver/ut/memblock/seq/seq_pkg.sv
mem_ut/ver/ut/memblock/cfg/tb.f
```

要求：

1. `seq/seq.f` 必须包含所需 include path，例如：
   ```text
   +incdir+./base_seq
   +incdir+./virtual_sequence
   +incdir+./base_seq_help/soft_test
   ```
2. `seq_pkg.sv` 中按依赖顺序 include：
   - base sequence 先于继承它的 virtual/responder sequence
   - 公共 transaction/helper 先于使用它的 scenario sequence
3. `cfg/tb.f` 必须在 `-F ../tc/tc.f` 之前引入 `-F ../seq/seq.f`，确保 `seq_pkg` 先于 `tc_pkg` 编译。
4. 不允许只新增 SV 文件而不更新 package/filelist/include path。
5. testcase 文件不放在 `seq` 下；testcase 仍由 `tc/tc_pkg.sv` 和 `tc/tc.f` 管理。若是 soft test testcase，放在 `mem_ut/ver/ut/memblock/tc/src/soft_test`。

## 参数控制规则

sequence 中可调的运行参数优先通过 `plus.sv` 暴露，不直接散落在 testcase 或 transaction 随机块中。适合放入 `plus.sv` 的参数包括：

- base sequence 的默认深度、默认 outstanding 数、默认 gap、默认地址范围选择
- transaction 生成时的默认 size、mask、burst/stride、scenario mix 开关
- responder sequence 的默认 `pre_pkt_gap/post_pkt_gap`、backpressure 比例、错误注入使能
- memory model 的默认初始化策略、默认容量或 range 选择

不适合放入 `plus.sv` 的参数包括：

- agent/env 组件开关，例如 `sqr_sw/drv_sw/mon_sw/xz_sw/drv_mode`
- `memblock_env_cfg` 字段
- `user_cfg.local.sv` 管理的本地私有环境组件配置

新增 plus 参数时必须同步：

```text
mem_ut/ver/ut/memblock/env/plus.sv
mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg
mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md
```

规则：

1. `plus.sv` 中定义字段、默认值和读取格式。
2. `seq/plus_cfg/default.cfg` 中添加同名 plusarg 默认项。
3. 默认 cfg 中新增使能类参数必须默认为 0。
4. 只修改 cfg 文件或 runtime `plus_arg` 时，参数在仿真运行阶段生效；新增或删除 `plus.sv` 字段需要重新编译。
5. sequence 内读取时使用 `plus::<field>`，不要重复写 `$value$plusargs`。

## transaction 字段提取规则

当从 transaction/xaction 中提取公共默认值或约束参数时：

1. 协议相关字段仍以 agent xaction 和 RTL interface 为准，不允许用 plus 改变端口语义。
2. 场景可调的默认值可以提升到 base sequence 字段，并由 `plus.sv` 初始化。
3. base sequence 负责把 plus 参数转换成稳定的控制变量，scenario sequence 只消费这些控制变量。
4. transaction 随机约束只保留协议合法性和局部一致性；跨 agent 的合法性放在公共 state/base sequence 中。

## plan 同步规则

所有 sequence 修改都必须同步检查已有 plan 或方案文档。执行修改前至少搜索：

```bash
rg -n "sequence|base_seq|virtual_sequence|vseq|plus\\.sv|mem_base_sequence" AI_DOC mem_ut/ver/ut/memblock/rule
```

如果已有方案描述了被修改 sequence 的目录、类名、接口、参数或时序规则，必须在同一个任务中同步更新对应 plan。已知需要优先同步的文档包括：

```text
AI_DOC/plan/test_framework/plan/do/mem_design_plan_20260614.md
AI_DOC/plan/test_framework/plan/undo/memblock_IO接口公共驱动逻辑方案_20260614.md
AI_DOC/plan/test_framework/plan
mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md
```

不得出现代码已迁移到 `mem_ut/ver/ut/memblock/seq`，但 plan 仍写 `tc/src/seq` 或 `tc/src/mem_base_sequence.sv` 的情况。

## 验证要求

完成 sequence 新增或迁移后，至少执行：

```bash
rg -n "<new_sequence_class>|<new_plus_field>" mem_ut/ver/ut/memblock
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
```

如果 sequence 会在默认 testcase 中启动，还需要执行：

```bash
make eda_run tc=tc_sanity mode=base_fun
```

长时间远端编译或仿真按项目 mem_ut 远端编译仿真流程执行。
