# XiangShan Codex 规则

## 相对路径基准

本文档中的仓库内路径均以当前 `AGENTS.md` 所在的 XiangShan 仓库根目录为基准。

例如：

- `AI_DOC` 表示当前仓库根目录下的 `AI_DOC`
- `mem_ut/ver/ut/memblock/sim` 表示当前仓库根目录下的 memblock 主仿真目录

在命令中进入目录时，也优先使用相对路径：

```bash
cd mem_ut/ver/ut/memblock/sim
```

## AI 文档路径

本项目与 `mem_ut` 相关的 AI 说明文档统一放在：

- [AI_DOC](AI_DOC)

当前 `mem_ut` 远端编译仿真方案与流程文档为：

- [mem_ut远端编译仿真方案与流程.md](AI_DOC/mem_ut远端编译仿真方案与流程.md)

当前 `memblock` RTL 生成规则文档为：

- [memblock_rtl生成规则.md](AI_DOC/memblock_rtl生成规则.md)

当前 `memblock` agent 添加规则文档为：

- [memblock_agent_add_rule.md](mem_ut/ver/ut/memblock/rule/memblock_agent_add_rule.md)

当前 `memblock` 更新最新代码规则文档为：

- [memblock_update_code_rule.md](mem_ut/ver/ut/memblock/rule/memblock_update_code_rule.md)

当前 `memblock` 最新 DUT 适配规则文档为：

- [memblock_latest_dut_adapt_rule.md](mem_ut/ver/ut/memblock/rule/memblock_latest_dut_adapt_rule.md)

当前 `memblock` 版本 profile 目录为：

- [version](mem_ut/ver/ut/memblock/rule/version)

当前 V3 profile 为：

- [v3 branch_policy.md](mem_ut/ver/ut/memblock/rule/version/v3/branch_policy.md)
- [v3 memblock_rtl_profile.md](mem_ut/ver/ut/memblock/rule/version/v3/memblock_rtl_profile.md)
- [v3 dut_interface_baseline.md](mem_ut/ver/ut/memblock/rule/version/v3/dut_interface_baseline.md)
- [v3 l2tlb_interface_profile.md](mem_ut/ver/ut/memblock/rule/version/v3/l2tlb_interface_profile.md)
- [v3 verified_status.md](mem_ut/ver/ut/memblock/rule/version/v3/verified_status.md)

当前 V2 profile 为：

- [v2 branch_policy.md](mem_ut/ver/ut/memblock/rule/version/v2/branch_policy.md)
- [v2 memblock_rtl_profile.md](mem_ut/ver/ut/memblock/rule/version/v2/memblock_rtl_profile.md)
- [v2 dut_interface_baseline.md](mem_ut/ver/ut/memblock/rule/version/v2/dut_interface_baseline.md)
- [v2 l2tlb_interface_profile.md](mem_ut/ver/ut/memblock/rule/version/v2/l2tlb_interface_profile.md)
- [v2 verified_status.md](mem_ut/ver/ut/memblock/rule/version/v2/verified_status.md)

当前 `memblock` cfg 添加规则文档为：

- [memblock_cfg_add_rule.md](mem_ut/ver/ut/memblock/rule/memblock_cfg_add_rule.md)

当前 `memblock` sequence 添加规则文档为：

- [memblock_sequence_add_rule.md](mem_ut/ver/ut/memblock/rule/memblock_sequence_add_rule.md)

当前 `memblock` L2TLB agent 专项规则文档为：

- [memblock_l2tlb_agent_rule.md](mem_ut/ver/ut/memblock/rule/memblock_l2tlb_agent_rule.md)

当前 `memblock` 参数分类管理规则文档为：

- [memblock_parameter_management_rule.md](mem_ut/ver/ut/memblock/rule/memblock_parameter_management_rule.md)

当前 `AI_DOC` 文件分类管理规则文档为：

- [ai_doc_file_management_rule.md](AI_DOC/project_management/ai_doc_file_management_rule.md)

当前 `mem_ut` 测试环境逻辑构建规则为：

- [mem_ut_test_framework_logic_build_rule.md](AI_DOC/project_management/mem_ut_test_framework_logic_build_rule.md)

当前 `mem_ut` 测试框架 plan 生成与评审规则为：

- [mem_ut_test_framework_plan_review_rule.md](AI_DOC/project_management/mem_ut_test_framework_plan_review_rule.md)

当前 `mem_ut` 测试框架 plan 执行通用规则为：

- [mem_ut_test_framework_plan_execution_rule.md](AI_DOC/project_management/mem_ut_test_framework_plan_execution_rule.md)

当前 `mem_ut` 源码 review 文档编写规则为：

- [mem_ut_code_review_document_rule.md](AI_DOC/project_management/mem_ut_code_review_document_rule.md)

后续处理 `mem_ut` 环境时，应优先阅读该文档。

后续生成、刷新或检查 `memblock` RTL 时，应优先阅读 `memblock_rtl生成规则.md`。
同时必须按当前分支或用户指定版本读取对应
`mem_ut/ver/ut/memblock/rule/version/<v2|v3>/memblock_rtl_profile.md`。

后续新增、重新添加或刷新 `memblock` agent，或添加其 `monitor`、`driver`、
`sequencer`、`interface`、`transaction`/`xaction` 等组件时，应优先阅读
`mem_ut/ver/ut/memblock/rule/memblock_agent_add_rule.md`。

后续更新最新代码、同步最新代码、拉取最新代码、更新到最新
`kunminghu-v2` 或 `kunminghu-v3` 时，应优先阅读
`mem_ut/ver/ut/memblock/rule/memblock_update_code_rule.md`。
同时必须按当前分支或用户指定版本读取对应
`mem_ut/ver/ut/memblock/rule/version/<v2|v3>/branch_policy.md`。

后续适配最新 DUT、同步最新 RTL 接口、检查 DUT 顶层端口或内部模块接口变化时，
应优先阅读
`mem_ut/ver/ut/memblock/rule/memblock_latest_dut_adapt_rule.md`。
同时必须按当前分支或用户指定版本读取对应
`mem_ut/ver/ut/memblock/rule/version/<v2|v3>/dut_interface_baseline.md`。

后续新增、扩展或调整 `memblock` UVM 环境配置字段、`user_ctrl`
个性化配置字段或本地个人配置入口时，应优先阅读
`mem_ut/ver/ut/memblock/rule/memblock_cfg_add_rule.md`。

后续新增、迁移、重构或修改 `memblock` sequence，包括 base sequence、
virtual sequence、scenario sequence、responder sequence 或 sequence 参数化入口时，
应优先阅读
`mem_ut/ver/ut/memblock/rule/memblock_sequence_add_rule.md`。

后续修改 `mem_ut` 中任何 L2TLB 相关内容，包括 L2TLB interface/connect、
L2TLB agent 内部 driver/monitor/sequencer/transaction/default sequence、
`memblock_l2tlb_base_sequence.sv` 或 DTLB/L2TLB request/response 查表逻辑时，
必须先阅读
`mem_ut/ver/ut/memblock/rule/memblock_l2tlb_agent_rule.md`。
同时必须按当前分支或用户指定版本读取对应
`mem_ut/ver/ut/memblock/rule/version/<v2|v3>/l2tlb_interface_profile.md`。

后续新增、迁移、重命名或调整 `mem_ut` 参数，包括 `memblock_env_cfg` 字段、
`env/plus.sv` 参数、`seq_csr_common.sv` getter、`seq/plus_cfg/*.cfg` preset、
testcase preset 的 Makefile `cfg=` 入口、编译期宏参数或 connect-time 参数时，必须先阅读
`mem_ut/ver/ut/memblock/rule/memblock_parameter_management_rule.md`。

后续新增、移动、整理、归档或 review `AI_DOC` 下的设计文档、plan、review、flow、
网页文档、分析文档或项目管理规则时，必须先阅读
`AI_DOC/project_management/ai_doc_file_management_rule.md`。

后续新增、修改或 review `mem_ut` 测试框架运行期逻辑、测试用例构建逻辑、
sequence 主循环、monitor service loop、handler、adapter、scheduler 或公共状态维护逻辑时，
必须先阅读
`AI_DOC/project_management/mem_ut_test_framework_logic_build_rule.md`。

后续生成、修改、重构或评审 `AI_DOC/plan/test_framework/plan` 下的 mem_ut 测试框架 plan，
包括只生成 plan、不 coding 的任务，必须先阅读
`AI_DOC/project_management/mem_ut_test_framework_plan_review_rule.md`。

后续按 `AI_DOC/plan/test_framework/plan/undo` 下的测试框架 plan 执行 coding、执行 plan、
根据 plan 修改代码、同步文档、生成 implementation review、移动 plan 到 do 或提交 plan 实现时，
必须先阅读
`AI_DOC/project_management/mem_ut_test_framework_plan_execution_rule.md`。

后续生成、修改或复查 `mem_ut` 测试框架源码 review 文档、implementation review、
code review 或专项源码修改 review 时，必须先阅读
`AI_DOC/project_management/mem_ut_code_review_document_rule.md`。

后续凡新增或调整 `mem_ut/ver/ut/memblock/rule` 下的 mem_ut 规则文件，必须同步在
本 `AGENTS.md` 中添加或更新对应入口，包含规则文档链接、触发条件和优先阅读要求。

后续凡新增或调整 `AI_DOC/project_management` 下的项目规则文件，必须同步在
本 `AGENTS.md` 中添加或更新对应入口，包含规则文档链接、触发条件和优先阅读要求。

## 版本 Profile 路由规则

处理 `mem_ut` 或 `memblock` 相关任务时，必须先确定版本 profile：

- 当前分支为 `mem_ut_uvm` 时，默认使用 V3 profile。
- 当前分支为 `mem_ut_uvm_v2` 时，默认使用 V2 profile。
- 用户显式指定 V2 或 V3 时，以用户指定版本为准。
- 当前分支与用户显式指定版本冲突时，停止并向用户确认，不得按猜测继续。

Profile 只保存版本差异，例如上游设计分支、RTL 生成入口、DUT/interface
基线、L2TLB 连接点和已验证状态。通用规则仍以本文件和
`AI_DOC/project_management`、`mem_ut/ver/ut/memblock/rule` 下的公共规则为准。

## mem_ut UVM 路径规则

`mem_ut` 当前位于：

- `mem_ut`

memblock UVM 环境位于：

- `mem_ut/ver/ut/memblock`

主仿真目录位于：

- `mem_ut/ver/ut/memblock/sim`

处理 memblock UVM 环境时，优先从以下目录运行命令：

```bash
cd mem_ut/ver/ut/memblock/sim
```

## 远端编译仿真规则

本项目采用双节点 flow：

- 当前节点：编辑代码、分析日志、触发命令
- `eda01`：真实执行 `vcs` / `verdi` 编译仿真，对应节点 IP 为 `172.28.10.101`

如果 `ssh eda01` 不通，允许改用 `ssh 172.28.10.101` 连接同一远端节点。

不要默认假设本地能直接运行 `vcs`。

应优先使用 `sim/Makefile` 中的远端目标：

- `make eda_compile tc=tc_sanity mode=base_fun`
- `make eda_run tc=tc_sanity mode=base_fun`
- `make eda_run_bg tc=tc_sanity mode=base_fun`
- `make eda_status tc=tc_sanity mode=base_fun`
- `make eda_tail tc=tc_sanity mode=base_fun`

这些目标会调用：

- `mem_ut/ver/ut/memblock/sim/remote_eda_make.sh`
- `mem_ut/ver/ut/memblock/sim/eda01_entry.sh`

## 远端环境初始化规则

当前默认远端 bootstrap 由 `mem_ut/ver/ut/memblock/sim/Makefile` 维护。
该 bootstrap 负责初始化远端 module 环境，并加载已验证的
`synopsys/vcs/Q-2020.03-SP2` 与 `synopsys/verdi/R-2020.12-SP1`。

如果再次出现 `vcs: command not found`，优先检查远端 `module` 初始化与
工具版本，不要直接回退到本地 `make compile`。

## MEMBLOCK_PROJECT 规则

必须保证：

```text
$MEMBLOCK_PROJECT/XiangShan/build_memblock/rtl/filelist.f
```

路径有效。

因此 `MEMBLOCK_PROJECT` 必须指向 `XiangShan` 的上一级目录，而不是
`XiangShan` 本身。

## memblock RTL 生成规则

当用户要求生成、刷新、重新生成或检查 `memblock` RTL 时：

- 必须先阅读 `AI_DOC/memblock_rtl生成规则.md`
- 必须读取当前版本 profile 的 `memblock_rtl_profile.md`
- 优先使用 `scripts/generate_memblock_rtl.sh`
- 不要跨版本套用 firtool、main class、config 或构建系统假设
- 生成成功标准以当前版本 profile 的产物清单为准

## memblock agent 添加规则

触发条件：

- 用户要求新增、重新添加或刷新 `memblock` agent
- 用户要求添加或更新 `monitor`、`driver`、`sequencer`、`interface`、`transaction`/`xaction`、`cfg`、`default_sequence` 等 agent 组件

处理以上任务时，必须先阅读：

- `mem_ut/ver/ut/memblock/rule/memblock_agent_add_rule.md`

核心要求：

- 先判断接口属于 `memblock` 内部模块还是 `memblock` 顶层端口
- 内部模块接口创建到 `mem_ut/ver/ut/memblock/subagent`
- 顶层端口接口创建到 `mem_ut/ver/ut/memblock/agent`
- 即使只添加单个组件，也要补齐完整 agent 顶层结构
- 同步更新 `mem_ut/ver/ut/memblock/cfg/tb.f`
- 同步完成 `mem_ut/ver/ut/memblock/env/src/memblock_env.sv` 中的声明、例化和必要连接

## 更新最新代码规则

触发条件：

- 用户要求更新最新代码
- 用户要求同步最新代码、拉取最新代码
- 用户要求更新到最新 `kunminghu-v2` 或 `kunminghu-v3`

处理以上任务时，必须先阅读：

- `mem_ut/ver/ut/memblock/rule/memblock_update_code_rule.md`
- 当前版本 profile 的 `branch_policy.md`

核心要求：

- 先运行 `git status`
- 如果工作区存在任何修改、暂存或未跟踪文件，停止并反馈用户先选择本地提交所有修改或执行 `git stash`
- 工作区干净后才按当前版本 profile 选择上游分支并依次运行 `git fetch`、`git rebase FETCH_HEAD`
- rebase 成功后参考 `AI_DOC/memblock_rtl生成规则.md` 重新生成 memblock RTL
- RTL 刷新成功后参考 `memblock_latest_dut_adapt_rule.md` 检查并同步 DUT 交接 interface 与对应 agent 字段

## 最新 DUT 适配规则

触发条件：

- 用户要求适配最新 DUT
- 用户要求同步最新 RTL 接口
- 用户要求检查 DUT 顶层端口或内部模块接口变化
- 更新最新代码并重新生成 memblock RTL 后

处理以上任务时，必须先阅读：

- `mem_ut/ver/ut/memblock/rule/memblock_latest_dut_adapt_rule.md`

核心要求：

- 从 `mem_ut/ver/ut/memblock/tb/top_tb.sv` 展开的所有 RTL 交接 interface 入手
- 检查 `build_memblock/rtl` 中 Verilog RTL 顶层端口和内部模块接口是否变化
- 如有变化，同步更新对应 `tb/*_agent_connect.sv`、agent interface、transaction/xaction、driver 驱动字段、monitor 采集字段
- 如影响 env、RM、sequence、cfg，也必须同步修改

## memblock cfg 添加规则

触发条件：

- 用户要求新增、扩展或调整 `memblock_env_cfg` 字段
- 用户要求新增 agent 配置开关、`user_ctrl` 个性化配置字段或本地个人配置入口
- 用户要求修改 cfg 默认值、配置优先级或 cfg 应用位置

处理以上任务时，必须先阅读：

- `mem_ut/ver/ut/memblock/rule/memblock_cfg_add_rule.md`

核心要求：

- 公共 cfg 字段必须可见且有默认值
- 个人覆盖字段必须使用 `<field>_valid` 控制是否覆盖现有 env/testcase 配置
- 个人本地配置只放在 ignored 的 `mem_ut/ver/ut/memblock/cfg/user_cfg.local.sv`
- `user_ctrl` 不再支持命令行配置，个人配置统一通过 `user_cfg.local.sv` 生效
- 新增个人覆盖字段时必须同步更新 `user_cfg.local.default.sv` 和当前 `user_cfg.local.sv`，并保持新增 `<field>_valid` 默认为 `1'b0`
- 修改 cfg 后同步更新使用说明与规则文档，并按规则执行格式检查和必要的远端编译/仿真

## memblock sequence 添加规则

触发条件：

- 用户要求新增、迁移、重构或修改 `memblock` sequence
- 用户要求添加 base sequence、virtual sequence、scenario sequence、responder sequence
- 用户要求调整 sequence 使用的公共参数、transaction 默认值或随机约束入口

处理以上任务时，必须先阅读：

- `mem_ut/ver/ut/memblock/rule/memblock_sequence_add_rule.md`

核心要求：

- sequence 统一放在 `mem_ut/ver/ut/memblock/seq`
- base sequence 放在 `mem_ut/ver/ut/memblock/seq/base_seq`
- virtual/scenario/responder sequence 放在 `mem_ut/ver/ut/memblock/seq/virtual_sequence`
- software-only/soft_test sequence 放在 `mem_ut/ver/ut/memblock/seq/virtual_sequence/soft_test`
- `mem_ut/ver/ut/memblock/seq/plus_cfg` 只保存 plusarg cfg，不放 SV sequence
- sequence 由 `mem_ut/ver/ut/memblock/seq/seq_pkg.sv` 和 `seq/seq.f` 管理；新增或迁移 sequence 后同步更新这两个文件，并确认 `cfg/tb.f` 中 `-F ../seq/seq.f` 位于 `-F ../tc/tc.f` 之前
- testcase 由 `mem_ut/ver/ut/memblock/tc/tc_pkg.sv` 和 `tc/tc.f` 管理；soft_test testcase 放在 `mem_ut/ver/ut/memblock/tc/src/soft_test`
- base sequence 和 transaction 提取出的场景可调参数优先通过 `env/plus.sv` 和 `seq/plus_cfg/*.cfg` 控制
- 如果已有 plan 描述了被修改 sequence 的目录、类名、接口、参数或时序规则，必须同步更新对应 plan

## memblock L2TLB agent 专项规则

触发条件：

- 用户要求修改 L2TLB/L2tlb agent、interface、connect、driver、monitor、sequencer、transaction/xaction、default sequence 或 cfg
- 用户要求新增或修改 `memblock_l2tlb_base_sequence.sv`、L2TLB responder sequence、DTLB request 采集或 L2TLB response 回填逻辑
- 用户要求修改 L2TLB 相关 plus 参数、TLB lookup API、DTLB/L2TLB 文档或 plan

处理以上任务时，必须先阅读：

- `mem_ut/ver/ut/memblock/rule/memblock_l2tlb_agent_rule.md`

核心要求：

- `L2TLB_agent` 代替的是 L2TLB 对上游 DTLB 的 responder 功能，连接点是 DTLB 与 L2TLB 的 request/response 交互处
- request 方向是 DTLB -> L2TLB_agent，response 方向是 L2TLB_agent -> DTLB
- 不得把 `L2TLB_agent` 写成或接成 L2TLB 与 L2Cache/PTW/memory 的下游交互模型
- L2TLB lookup 优先使用 DTLB request 中的 `vpn/s2xlate` 和 runtime CSR snapshot 中的 `asid/vmid/csr_update_seq`
- 不应假设 DTLB -> L2TLB request 携带 `paddr`；出现 paddr 查表说法时必须重新确认语义
- 所有 L2TLB 相关修改必须同步检查相关 plan 和规则文档，避免文档仍描述为 L2TLB/L2Cache 下游模型

## memblock 参数分类管理规则

触发条件：

- 用户要求新增、迁移、重命名或调整 `mem_ut` 参数
- 用户要求修改 `memblock_env_cfg` 字段、agent cfg/user ctrl、本地个人配置字段
- 用户要求新增或修改 `env/plus.sv`、`seq_csr_common.sv`、`seq/plus_cfg/*.cfg`
- 用户要求新增或修改 testcase preset cfg 或 Makefile `cfg=` 使用规则
- 用户要求新增或修改编译期宏参数、connect/interface takeover 参数

处理以上任务时，必须先阅读：

- `mem_ut/ver/ut/memblock/rule/memblock_parameter_management_rule.md`

核心要求：

- 新增参数前先判断作用对象：环境组件、公共测试框架、testcase 个性化、编译期宏或运行期状态
- 环境组件控制参数归 `memblock_env_cfg` 和 `user_cfg.local.sv`
- 公共测试框架参数走 `env/plus.sv -> seq_csr_common.sv -> getter`
- testcase preset 使用 `seq/plus_cfg/<cfg>.cfg`，通过 Makefile `cfg=<cfg>` 指定，testcase 源码不散落写 `plus::MEMBLOCK_*`
- connect-time 静态参数归 `cfg/memblock_compile_params.svh`
- 运行期状态、表项、monitor raw queue、CSR runtime snapshot 不归入参数配置系统
- 新增 cfg key 后必须保持 `env/plus.sv`、`seq_csr_common.sv`、`default.cfg` 和 testcase preset cfg 同步

## 当前已验证状态

当前已经验证：

- 在 `mem_ut` 位置下远端编译链可用
- `eda01` 上可以成功拉起 `vcs`
- `tc_sanity/base_fun` 可通过远端编译与仿真
- 最新通过日志位于 `mem_ut/ver/ut/memblock/sim/base_fun/log/tc_sanity_666666_rtl_.log`

当前环境的正确验证标准：

- 从仓库根目录执行 `cd mem_ut/ver/ut/memblock/sim`
- 使用 `make eda_compile tc=tc_sanity mode=base_fun`
- 使用 `make eda_run tc=tc_sanity mode=base_fun`
- 结果应看到 `TEST CASE PASSED`，且 `UVM_ERROR`、`UVM_FATAL` 均为 0

## Git 规则

处理该 flow 时：

- 不要把 `mem_ut` 改动和 XiangShan 其他无关脏改动混在一起提交
- 优先只 stage 目标 `mem_ut/**`
- 远端编译基础设施变更应与 `mem_ut` 环境一起维护
- 默认只做本地 `commit`
- 不要自动 `push` 到远端，除非用户明确要求上传远端
