# memblock 参数管理规则

## 触发条件

当任务涉及 `mem_ut` 参数新增、迁移、重命名或默认值调整时，必须先阅读本规则。范围包括：

- 新增或修改 `memblock_env_cfg`、`user_cfg.local.sv`、agent cfg/user ctrl 字段。
- 新增或修改 `env/plus.sv`、`seq_csr_common.sv`、`seq/plus_cfg/*.cfg`。
- 新增或修改 testcase preset cfg、testcase 个性化参数入口。
- 新增或修改编译期宏参数、connect/interface takeover 开关。
- 修改运行期状态表、monitor raw queue 或 CSR runtime snapshot 的参数归属说明。

如果任务同时涉及 cfg、sequence 或 L2TLB，还必须继续阅读：

- `mem_ut/ver/ut/memblock/rule/memblock_cfg_add_rule.md`
- `mem_ut/ver/ut/memblock/rule/memblock_sequence_add_rule.md`
- `mem_ut/ver/ut/memblock/rule/memblock_l2tlb_agent_rule.md`
- `mem_ut/ver/ut/memblock/rule/memblock_code_comment_rule.md`

## 通用中文注释规则

新增或修改参数、状态字段、plus 控制字段、cursor/counter/epoch 字段时，必须遵循：

```text
mem_ut/ver/ut/memblock/rule/memblock_code_comment_rule.md
```

参数或字段的作用对象、默认语义、约束关系和状态影响必须在源码中添加中文注释，并同步更新相关设计文档。

## 参数分类

新增参数前必须先判断作用对象，再决定落点。

| 分类 | 作用对象 | 落点 | 说明 |
|---|---|---|---|
| 环境组件控制参数 | agent/env 组件是否开启、driver/monitor/sequencer 行为 | `memblock_env_cfg`，个人覆盖走 `user_cfg.local.sv` | 不放 `plus.sv`，不由 testcase 直接改 agent cfg |
| 测试框架公共参数 | dispatch 公共 sequence/helper/transaction 约束 | `env/plus.sv -> seq_csr_common.sv -> getter` | plus 负责输入解析，`seq_csr_common` 是正式读取入口 |
| 测试用例个性化参数 | 单个 testcase preset/debug knob | `seq/plus_cfg/<tc>.cfg`，由 Makefile/regress 命令显式选择 cfg | testcase 源码不散落写 `plus::MEMBLOCK_*` |
| 编译期宏参数 | connect-time 静态选择、条件编译、固定宏默认值 | `cfg/memblock_compile_params.svh` | runtime plusarg 不能改变编译期连接结构 |
| 运行期状态 | monitor raw queue、状态表、CSR runtime snapshot | 对应 runtime package/table | 不是配置参数，不放 `seq_csr_common` 或 env cfg |

## 公共测试框架参数规则

公共框架参数必须同时维护：

```text
mem_ut/ver/ut/memblock/env/plus.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv
mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg
```

要求：

- `plus.sv` 定义字段、默认值和命令行解析格式。
- `default.cfg` 提供同名默认项，使 Makefile runtime cfg 路径可见。
- `seq_csr_common.sv` 读取 plus 最终值，执行 clamp/fatal/warning 等合法性处理，并提供 getter。
- 公共 sequence/helper 不长期直接读 `plus::MEMBLOCK_*`，应读 `seq_csr_common::get_*()`。
- 新增使能类参数默认值必须保守，避免无意启动 directed 行为。

## testcase preset 规则

testcase 需要定制 `MEMBLOCK_*` 参数时，使用“指定 testcase + 指定 Makefile cfg”：

```text
make eda_run tc=<tc_name> mode=<mode_name> cfg=<cfg_name>
```

要求：

- testcase 专属 cfg 文件放在 `mem_ut/ver/ut/memblock/seq/plus_cfg/<cfg_name>.cfg`。
- Makefile 通过 `cfg=<cfg_name>` 和 `plus_file=../seq/plus_cfg` 选择 cfg 文件，并在运行命令中
  将 cfg 文件里的有效 `+KEY=VALUE` 行展开成真实 runtime plusargs。
- 展开 cfg 时必须过滤掉被用户 `plus_arg` 覆盖的同名 key，再追加用户 `plus_arg`，保证
  `plus.sv` 使用 `$value$plusargs` 读取首个匹配值时仍满足用户命令行优先。
- 不在 testcase build/main body 中散落写 `plus::MEMBLOCK_* = ...`。
- testcase 源码不新增 cfg loader、preset helper 或按 testcase 名称自动读 cfg 的逻辑。
- 用户命令行 `plus_arg` 追加在 cfg 展开出的 plusargs 之后，且 cfg 展开阶段会过滤同名 key，优先级高于 testcase cfg preset。
- 新增 testcase preset 时，新增 `seq/plus_cfg/<cfg_name>.cfg`，并在回归命令或回归 ini 中显式写
  `cfg=<cfg_name>`。
- `cfg` 名称建议与 testcase 同名，例如
  `tc=tc_dispatch_real_store_wb_smoke cfg=tc_dispatch_real_store_wb_smoke`。
- 不再维护 `memblock_tc_plus_cfg_loader.sv` 这类 SV cfg loader；出现类似需求时优先改
  Makefile/regress 命令的 `cfg=` 选择。

示例：

```bash
make eda_run tc=tc_dispatch_real_store_wb_smoke mode=base_fun cfg=tc_dispatch_real_store_wb_smoke
make eda_run tc=tc_dispatch_real_store_wb_smoke mode=base_fun cfg=tc_dispatch_real_store_wb_smoke plus_arg="+MEMBLOCK_STD_REAL_WB_PASS_EN=0"
```

推荐检查 testcase cfg key 是否已在 `plus.sv` 中声明：

```bash
for f in mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg mem_ut/ver/ut/memblock/seq/plus_cfg/tc_dispatch_real*.cfg; do
  sed 's/[#].*$//; s,//.*$,,; s/^+//; /^[[:space:]]*$/d; s/=.*//' "$f"
done | rg '^MEMBLOCK_' | sort -u > /tmp/memblock_cfg_keys.txt

rg -n '`MEMBLOCK_PLUS_ARGS_DEFINE\(MEMBLOCK_|load_(bit|hex64|int|string)\("MEMBLOCK_' \
  mem_ut/ver/ut/memblock/env/plus.sv \
  | sed -n 's/.*MEMBLOCK_/MEMBLOCK_/p' \
  | sed 's/[^A-Za-z0-9_].*$//' | sort -u > /tmp/memblock_plus_keys.txt

comm -23 /tmp/memblock_cfg_keys.txt /tmp/memblock_plus_keys.txt
```

输出必须为空。

## 环境组件参数规则

agent/env 组件控制参数统一进入 `memblock_env_cfg` 和 user cfg 机制。

要求：

- 公共默认值必须在 `memblock_env_cfg` 可见。
- 个人本地覆盖只能写在 ignored 的 `cfg/user_cfg.local.sv`。
- 新增个人覆盖字段必须带 `<field>_valid`，默认 `1'b0`。
- 不用 `plus.sv` 直接控制 `sqr_sw/drv_sw/mon_sw/xz_sw/channel_id/driver mode`。

## 编译期宏参数规则

connect/interface 连接阶段必须确定的静态参数统一放在：

```text
mem_ut/ver/ut/memblock/cfg/memblock_compile_params.svh
```

要求：

- 每个宏必须有默认值。
- connect 文件只消费宏，不解析 runtime plusarg 或 testcase 名称。
- runtime plusarg 只控制 runtime sequence/helper 行为，不能假装改变编译期连接结构。
- 修改宏默认值必须同步说明默认行为和覆盖方式。

当前 L2TLB 特例：

- `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN` 默认值为 1，表示 mem_ut 默认由 `L2TLB_agent` 接管 DTLB/L2TLB response 通路。
- `MEMBLOCK_L2TLB_SEQ_EN` 是 runtime sequence enable，默认值为 1；默认主动响应 request。
- 如果需要只观察 DUT 原始 PTW/L2TLB response，编译期覆盖 `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN=0`。
- 如果编译期关闭 takeover 又 runtime 打开 `MEMBLOCK_L2TLB_SEQ_EN=1`，sequence 应 fatal。

## 运行期状态规则

以下内容不是参数配置：

- `memblock_sync_pkg` 中的同步标志和 raw monitor queue。
- `common_data_transaction` 中的主表、状态表、TLB 表、issue queue。
- `mmu_csr_runtime_state` 中的 CSR runtime snapshot。
- monitor 采样结果、replay/redirect/flush 运行期状态。

这些对象由运行期状态表或 monitor adapter 维护，不迁入 `seq_csr_common`、`memblock_env_cfg` 或 plus cfg。

## 校验函数命名规则

后续新增合法性检查、模式一致性检查或上下文一致性检查函数时，函数名必须带
`check_` 前缀。

适用场景包括：

- CSR/runtime 状态与接口字段一致性检查。
- opcode/fuOpType 推导结果与 DUT monitor 字段一致性检查。
- TLB/L2TLB req `s2xlate` 与 runtime CSR、`hyperinst` 语义一致性检查。
- 参数取值范围、权重组合、模式组合的合法性检查。

`check_` 函数只表达校验或诊断，不承担构造 key、插入表项、修改 runtime 状态或驱动
interface 的职责。构造、填充、插入、驱动类函数继续使用 `make_`、`build_`、
`fill_`、`insert_`、`drive_` 等动词，不使用 `check_`。

## 文档与验证

修改参数管理后必须同步检查：

```text
AI_DOC/project_management/mem_ut_parameter_management.md
mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md
mem_ut/ver/ut/memblock/rule/memblock_parameter_management_rule.md
```

涉及 L2TLB 参数时，还必须同步检查：

```text
mem_ut/ver/ut/memblock/rule/memblock_l2tlb_agent_rule.md
AI_DOC/analysis/source_sv/dispatch_framework_sv/memblock_l2tlb_base_sequence.md
AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_framework_design_20260614.md
AI_DOC/plan/test_framework/plan/do/l2tlb_base_seq_plan_20260614.md
AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md
AI_DOC/analysis/framework_design/dispatch_backend_interface_closure_code_changes.md
```

基础静态检查：

```bash
git diff --check -- mem_ut/ver/ut/memblock AI_DOC/project_management AI_DOC/analysis AI_DOC/plan
```

如修改影响编译路径、package include、class 定义、connect 宏或 default sequence，必须从 `mem_ut/ver/ut/memblock/sim` 执行远端编译：

```bash
make eda_compile tc=tc_sanity mode=base_fun
```

如修改会影响运行期默认行为，还应执行：

```bash
make eda_run tc=tc_sanity mode=base_fun
```
