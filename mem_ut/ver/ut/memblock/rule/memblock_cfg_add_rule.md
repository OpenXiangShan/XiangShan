# memblock cfg 添加规则

## 触发条件

当用户要求新增、扩展或调整 `memblock` UVM 环境配置字段时，必须使用本规则。

典型触发包括：

- 新增 `memblock_env_cfg` 字段
- 新增 agent 配置开关
- 新增 `user_ctrl` 个性化配置字段
- 新增个人本地配置入口
- 修改 cfg 默认值、优先级或 cfg 应用位置

当前 `user_ctrl` 不再支持通过仿真命令行配置。个人配置统一写入本地文件：

```text
mem_ut/ver/ut/memblock/cfg/user_cfg.local.sv
```

## 通用中文注释规则

新增或修改 cfg 字段、环境控制字段、agent 配置开关、user ctrl 字段时，必须同时遵循：

```text
mem_ut/ver/ut/memblock/rule/memblock_code_comment_rule.md
```

核心配置字段的用途、默认值语义、覆盖关系和生效范围必须在源码中添加中文注释。

## 基本原则

新增 cfg 字段时必须满足：

- 字段名和含义在公共仓库中可见
- 字段必须有明确默认值
- 默认行为不能改变现有 testcase 结果，除非用户明确要求
- 个人专属配置内容不能提交到仓库
- 个人固定配置必须放入本地 ignored 文件
- `user_cfg.local.sv` 中所有新增配置使能位默认必须为 `1'b0`

## user_ctrl 字段添加规则

如果新增字段属于个人可覆盖配置，优先添加到：

```text
mem_ut/ver/ut/memblock/cfg/user_cfg.sv
```

字段添加要求：

- 在 `memblock_user_agent_ctrl` 中新增 `<field>_valid`
- 在 `memblock_user_agent_ctrl` 中新增 `<field>`
- 在 `set_default()` 中设置默认值
- 在 `apply_to()` 中仅当 `<field>_valid=1` 时覆盖目标 cfg
- 如果字段类型是 enum，local 文件必须使用明确 enum 字面量赋值

禁止直接新增一个无 valid 位的个人覆盖字段。

## 本地个人配置规则

个人固定配置文件使用：

```text
mem_ut/ver/ut/memblock/cfg/user_cfg.local.sv
```

默认模板文件使用：

```text
mem_ut/ver/ut/memblock/cfg/user_cfg.local.default.sv
```

要求：

- `user_cfg.local.sv` 不得提交
- `user_cfg.local.sv` 必须由 `mem_ut/ver/ut/memblock/cfg/.gitignore` 忽略
- `user_cfg.local.default.sv` 必须提交，用于自动生成本地文件
- `user_cfg.local.sv` 只能写个人覆盖赋值，不应定义公共字段
- 该文件默认会被 `user_ctrl` include，不需要额外宏定义打开
- 修改 `user_cfg.local.sv` 后必须重新编译才会生效
- 所有可覆盖字段必须同时出现在 `user_cfg.local.default.sv` 和当前环境的 `user_cfg.local.sv`
- 所有 `<field>_valid` 在默认模板和自动创建的 local 文件中都必须为 `1'b0`

示例：

```systemverilog
this.u_lsqenq_agent_agent_ctrl.drv_mode_valid = 1'b1;
this.u_lsqenq_agent_agent_ctrl.drv_mode = tcnt_dec_base::DRV_0;
```

## 自动创建 local 文件规则

`mem_ut/ver/ut/memblock/sim/Makefile` 必须维护 `ensure_user_cfg_local` 规则。

当环境中没有：

```text
mem_ut/ver/ut/memblock/cfg/user_cfg.local.sv
```

时，规则必须从：

```text
mem_ut/ver/ut/memblock/cfg/user_cfg.local.default.sv
```

自动创建一份 local 文件。

自动创建的文件必须满足：

- 包含所有当前 `memblock_user_cfg` control 对象
- 每个 control 对象包含所有当前 `memblock_user_agent_ctrl` 可覆盖字段
- 所有 `<field>_valid` 默认都是 `1'b0`
- 字段值本身使用合法的 SystemVerilog 字面量或 enum 字面量

`compile`、`rtl` 和 `batch_run` 都应依赖该规则，保证正常编译和运行路径会自动补齐 local 文件。

## 新增环境参数同步规则

每当环境中新增一个 cfg 参数、agent 控制参数或需要个人可覆盖的参数时，必须同步更新 `user_ctrl` 的所有相关位置：

1. `memblock_user_agent_ctrl` 字段声明
2. `memblock_user_agent_ctrl::set_default()`
3. `memblock_user_agent_ctrl::apply_to()`
4. `user_cfg.local.default.sv` 中所有 control 对象的参数字段
5. 当前环境 `user_cfg.local.sv` 中所有 control 对象的参数字段
6. `memblock_user_ctrl_usage.md` 中的字段表、合法值和示例
7. 本规则文件中如有新增约束，也要同步更新

新增到 `user_cfg.local.default.sv` 和 `user_cfg.local.sv` 的 `<field>_valid` 必须默认写成：

```systemverilog
this.<ctrl_obj>.<field>_valid = 1'b0;
```

这样新字段进入公共环境后不会自动改变既有 testcase 行为。

## 优先级规则

cfg 应用优先级必须保持：

1. env 默认配置
2. testcase 配置
3. 本地个人配置

因此，`apply_user_cfg()` 应在 `memblock_env.build_phase()` 获取或创建 cfg 后、向 agent 下发 cfg 前执行。

不要在 `memblock_env_cfg.post_randomize()` 内直接应用 user_ctrl，否则 testcase 后续手工设置可能覆盖个人配置。

## env 接入规则

新增 cfg class 或 user_ctrl class 后，应检查并更新：

```text
mem_ut/ver/ut/memblock/env/memblock_env.f
mem_ut/ver/ut/memblock/env/memblock_env_pkg.sv
mem_ut/ver/ut/memblock/env/src/memblock_env_cfg.sv
mem_ut/ver/ut/memblock/env/src/memblock_env.sv
mem_ut/ver/ut/memblock/sim/Makefile
```

要求：

- filelist 必须包含新增 cfg 文件所在 include 路径
- package 必须在使用前 include 新增 cfg class
- `memblock_env_cfg` 必须例化 cfg 对象
- `memblock_env` 必须在 agent cfg 下发前应用最终配置
- `sim/Makefile` 必须能在 local 文件缺失时自动创建默认 local 文件

## 文档同步规则

新增或修改 cfg 字段后，必须同步更新：

```text
mem_ut/ver/ut/memblock/rule/memblock_user_ctrl_usage.md
mem_ut/ver/ut/memblock/rule/memblock_cfg_add_rule.md
```

至少补充：

- 字段名
- 字段类型
- 默认值
- 合法取值
- local 文件默认写法
- 本地配置示例

## 验证规则

完成 cfg 修改后至少执行：

```bash
git diff --check -- mem_ut/ver/ut/memblock/cfg mem_ut/ver/ut/memblock/env mem_ut/ver/ut/memblock/sim/Makefile
```

如果修改影响编译路径、package include、class 定义或 env 接入，必须执行远端编译：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
```

如果修改会改变运行期行为，还应执行：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_run tc=tc_sanity mode=base_fun
```

远端仿真流程按项目远端编译仿真规则执行。
