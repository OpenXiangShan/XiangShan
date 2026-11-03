# mem_ut 功能覆盖率模型文件管理规则

## 1. 适用范围

后续新增、修改、迁移、整理或复查 `mem_ut` 功能覆盖率模型、对应 bind 文件及配套辅助文件时，必须优先阅读本规则。

本规则管理模块目录、文件归属及编译期宏隔离。参数归属同时遵循 [memblock 参数管理规则](../../mem_ut/ver/ut/memblock/rule/memblock_parameter_management_rule.md) 和 [参数分类管理说明](mem_ut_parameter_management.md)；本地配置入口遵循 [cfg 添加规则](../../mem_ut/ver/ut/memblock/rule/memblock_cfg_add_rule.md)。涉及源码修改时仍须满足 [代码修改授权规则](code_modification_authorization_rule.md)。

本文仓库内完整路径以 XiangShan 仓库根目录为基准；下文的 `env/`、`cfg/`、`tb/` 和 `sim/` 以 `mem_ut/ver/ut/memblock` 为基准。

## 2. 每个模块使用独立目录

功能覆盖率统一放在：

```text
mem_ut/ver/ut/memblock/env/fcov/<模块名>/
```

必须遵守：

1. 每个模块的功能覆盖率使用独立目录管理，不将不同模块的模型文件混放。
2. 首次为某个模块添加功能覆盖率时，必须先在 `env/fcov/` 下新建该模块的独立目录，再添加模型及配套文件。
3. 后续扩展已有模块时复用其目录，不在 `env/fcov/` 根目录散放模型或 bind 文件。
4. 同一模块的覆盖模型、bind、配套 SVA 及辅助文件放在同一模块目录中；全局配置、公共 filelist 和顶层 bind 引入入口仍放在原有公共位置。
5. 模块内部存在多个覆盖模型时，仍在该模块目录内统一管理。当前 DTLB 的 core、control、storage、filter、integration 属于同一组，统一放在 `dtlb/`。

当前目录划分为：

```text
env/fcov/
  vector_unit_stride/
  load_unit/
  store_unit/
  dtlb/
```

详细文件清单和本地使用方法见 [功能覆盖率目录与配置说明](../../mem_ut/ver/ut/memblock/env/fcov/README.md)。

## 3. 每个模块使用一个编译期宏隔离

首次添加模块目录时，必须同时添加该模块对应的功能覆盖率控制宏；不能只添加文件而缺少独立开关。

宏名称采用 `MEMBLOCK_FCOV_<模块标识>_EN`。已有模块沿用下列名称：

| 模块目录 | 控制宏 | 默认状态 |
|---|---|---|
| `vector_unit_stride/` | `MEMBLOCK_FCOV_VECTOR_US_EN` | 未定义，关闭 |
| `load_unit/` | `MEMBLOCK_FCOV_LOAD_UNIT_EN` | 未定义，关闭 |
| `store_unit/` | `MEMBLOCK_FCOV_STORE_UNIT_EN` | 未定义，关闭 |
| `dtlb/` | `MEMBLOCK_FCOV_DTLB_EN` | 未定义，关闭 |

隔离要求：

- 每个模块使用一个宏，统一控制该目录内的覆盖模型、对应 bind 和配套辅助逻辑；DTLB 不再按内部模型增加独立开关。
- 模型定义与 bind 必须使用一致的门控条件，避免出现只编译模型或只保留 bind 的不完整配置。
- 生效条件为全局宏 `MEMBLOCK_UT_FCOV` 与对应模块宏同时定义。仅开启全局覆盖率时，未选择的模块仍保持关闭；全局关闭时，即使定义模块宏也不编译对应模型和 bind。
- 所有模块宏默认未定义，以保证默认关闭。存在性宏定义为 `0` 仍会满足 `ifdef`；关闭时必须删除或注释定义行。
- 各源文件必须在判断模块宏前通过统一头 `cfg/memblock_compile_params.svh` 获取配置，并保留独立文件防重复包含保护。
- 模块宏属于编译期参数，不增加 runtime plusarg、`memblock_env_cfg` 或 `user_ctrl` 镜像来控制模型与 bind 的存在性。

## 4. 参数与本地配置统一管理

模块宏的用途、默认行为及依赖关系统一登记在 `cfg/memblock_compile_params.svh`，由该头引入本地编译配置。

个人配置与公共模板分别为：

```text
cfg/user_compile_cfg.local.svh
cfg/user_compile_cfg.local.default.svh
```

新增模块宏时必须同步：

1. 在统一宏头中补充宏的用途、控制范围及默认关闭说明。
2. 在公共模板中添加该宏的注释示例，保持默认不定义。
3. 当前个人文件已存在时，仅补充新宏的注释示例，保留其他个人配置；文件缺少时通过 `make ensure_user_compile_cfg_local` 从公共模板创建。
4. 保持个人文件受 `cfg/.gitignore` 忽略，不提交个人启用设置。
5. 更新参数管理说明及 `env/fcov/README.md` 的目录、宏和使用示例。

本地启用时，在个人编译头中定义所需模块宏，再用 Makefile 的 `fcov=on` 或回归脚本的 `--compile --fcov` 重新编译。修改模块宏后必须重新编译；不能仅修改个人文件就复用旧 `simv`。

## 5. 首次添加及目录调整流程

首次添加模块功能覆盖率时，按以下顺序处理：

1. 确定观测模块和目录名，检查 `env/fcov/` 中是否已有对应目录；没有则先创建独立目录。
2. 在目录内添加模型、bind 及必要辅助文件，并添加该模块唯一控制宏的双重门控。
3. 按第 4 节同步统一参数入口、公共模板和本地配置示例，保持新模块默认关闭。
4. 在 `env/memblock_env.f` 添加模型及配套编译文件，在 `tb/top_tb.sv` 更新对应 bind 引入路径；需要时同步 include 搜索路径。
5. 更新目录和参数说明，检查文件归属、宏覆盖范围及引用完整性。
6. 完成第 6 节的静态检查和必要编译验证。

迁移或重命名已有目录时，必须同步 filelist、bind 引用及相关文档路径，并检索旧路径残留。仅做目录整理和宏隔离时，应保持原有覆盖点、仓定义、采样条件及信号连接不变。

## 6. 检查与验证要求

涉及覆盖率源码、filelist 或宏接入变更时，至少检查以下配置：

| 配置 | 预期结果 |
|---|---|
| 所有模块宏保持默认未定义 | 无功能覆盖率模型及对应 bind |
| 全局关闭，模块宏已定义 | 对应模型及 bind 均不参与编译 |
| 全局开启，只定义一个模块宏 | 仅该模块的模型及 bind 参与编译 |
| 全局开启，定义全部模块宏 | 全部模型及 bind 完整接入，无重复定义或悬空绑定 |

新增模块必须验证其单独开启配置，并检查不会意外带入其他模块。结合预处理结果核对模型与 bind 的隔离范围；涉及编译接入时按项目远端 EDA 流程完成默认关闭、目标模块开启及全部开启的必要编译检查。

规则及入口文档检查：

```bash
git diff --check -- AI_DOC/project_management AGENTS.md mem_ut/ver/ut/memblock
rg -n "mem_ut_fcov_file_management_rule|功能覆盖率模型文件管理规则" AGENTS.md AI_DOC/project_management
```

新增或调整本规则时，必须同步 `AGENTS.md` 中的规则链接、触发条件和优先阅读要求。纯文档变更只执行文档格式、路径和规则一致性检查，不要求重新编译仿真。
