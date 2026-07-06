# memblock sequence rename and directory relayout implementation review

## 1. 关联 Plan

关联 plan：

```text
AI_DOC/plan/test_framework/plan/do/memblock_sequence_rename_and_directory_relayout_plan_20260703.md
```

本次实现性质是纯命名和目录重排：

1. 不改变主表生成算法。
2. 不改变 issue、LSQ enqueue、LSQ commit、redirect、L2TLB、flushSb 的驱动时序。
3. 不新增旧类名兼容 typedef。
4. 不保留旧文件名兼容 include。

## 2. 术语说明

| 术语 | 通俗解释 | 代码落点 |
|---|---|---|
| base sequence | 可被 testcase 或 agent default sequence 直接启动的基础 sequence。 | `mem_ut/ver/ut/memblock/seq/base_seq` |
| helper sequence 文件 | transaction、handler、scheduler、builder、公共 helper 等支撑文件，通常不直接作为 agent default sequence 启动。 | `mem_ut/ver/ut/memblock/seq/base_seq_help` |
| top-level virtual sequence 目录 | 后续用于跨多个 base sequence 编排的预留目录，本轮只保留空目录。 | `mem_ut/ver/ut/memblock/seq/virtual_sequence/.gitkeep` |
| UVM factory 名称 | `type_id::create()` 和 default sequence 配置使用的 UVM 类型注册名。 | `uvm_object_utils(<class>)` |

## 3. 修改点 Review

### 3.1 sequence 类名和文件名同步改名

功能目的：

原有 `real_smoke`、`mixed_smoke` 和若干 `*_dispatch_sequence` 命名容易把“基础可复用 sequence”和“具体 smoke testcase”混在一起。本轮把文件名、类名、include guard、UVM factory 注册名和 constructor 默认名同步改成 plan 指定的新名字，避免同一对象出现旧名和新名并存。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_issue_dispatch_base_sequence.sv`

函数功能简析：该文件仍然实现 lintsissue agent 的 issue dispatch 基础驱动 sequence；本轮只修改命名，不修改成员、task/function 或驱动逻辑。

修改后源码：

```systemverilog
`ifndef MEMBLOCK_ISSUE_DISPATCH_BASE_SEQUENCE__SV
`define MEMBLOCK_ISSUE_DISPATCH_BASE_SEQUENCE__SV

class memblock_issue_dispatch_base_sequence extends lintsissue_agent_agent_default_sequence;

    `uvm_object_utils(memblock_issue_dispatch_base_sequence)

    extern function new(string name = "memblock_issue_dispatch_base_sequence");
```

中文伪代码：

本段逻辑在本 feature 中承担“新类名成为唯一有效入口”的功能。源码先用新的 include guard 保护文件，防止重复 include；然后声明新的 `memblock_issue_dispatch_base_sequence` 类，但继承关系仍然指向原 lintsissue agent 默认 sequence 基类，所以驱动接口类型没有变化；接着用新的类名注册 UVM factory，并把 constructor 默认 name 改成新类名，保证日志和 `type_id` 创建路径都不再出现旧名。这里没有调用子函数，也没有改变任何 runtime 状态。

关键检查：

- 旧类名 `memblock_lintsissue_dispatch_sequence` 在 `mem_ut/ver/ut/memblock` 中无残留。
- 未添加 `typedef memblock_issue_dispatch_base_sequence memblock_lintsissue_dispatch_sequence`。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_main_dispatch_auto_build_main_table_base_sequence.sv`

函数功能简析：该 sequence 仍然负责自动构建 main table，然后进入真实 dispatch service flow；本轮只把原 `real_smoke` 命名改成“auto build main table base”语义。

修改后源码：

```systemverilog
class memblock_main_dispatch_auto_build_main_table_base_sequence extends memblock_dispatch_base_sequence;

    `uvm_object_utils(memblock_main_dispatch_auto_build_main_table_base_sequence)

    extern function new(string name = "memblock_main_dispatch_auto_build_main_table_base_sequence");
    extern virtual task body();
```

中文伪代码：

本段逻辑在本 feature 中承担“自动构建主表基础 sequence 改名”的功能。源码声明新的类名，并保持继承 `memblock_dispatch_base_sequence` 不变，因此主表生成、地址生成、状态表初始化等逻辑仍来自原基类；UVM factory 注册名和 constructor 默认字符串同步改成新类名，确保 testcase 创建和日志显示都使用新名字；`body()` 只是继续作为原流程入口声明，本段没有改变 `body()` 内部执行顺序。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_main_dispatch_manual_main_table_sequence.sv`

函数功能简析：该 sequence 仍然手工构造 directed main table，再复用真实 dispatch service flow；本轮只把原 `mixed_smoke` 命名改成“manual main table”语义。

修改后源码：

```systemverilog
class memblock_main_dispatch_manual_main_table_sequence extends memblock_main_dispatch_auto_build_main_table_base_sequence;

    `uvm_object_utils(memblock_main_dispatch_manual_main_table_sequence)

    extern function new(string name = "memblock_main_dispatch_manual_main_table_sequence");
    extern virtual task body();
```

中文伪代码：

本段逻辑在本 feature 中承担“手工主表 sequence 改名并继续复用自动主表基础流程”的功能。源码把类名改为 `memblock_main_dispatch_manual_main_table_sequence`；继承对象改为新的 auto-build base sequence 名称，保持原来“manual sequence 继承 real smoke sequence”的结构关系；UVM factory 注册和 constructor 默认名也改成新类名。该继承关系保证 manual sequence 仍然能复用 `service_real_dispatch_flow()`，本轮没有新增或删除任何主表 transaction 生成逻辑。

### 3.2 目录重排和编译入口

功能目的：

原目录 `base_seq` 同时放 helper 文件，`virtual_sequence` 同时放可直接启动的基础 sequence，语义不清。本轮按 plan 重排为：

```text
seq/base_seq_help      保存 helper、transaction、handler、scheduler、builder
seq/base_seq           保存可启动/可复用的基础 sequence
seq/virtual_sequence   预留后续 top-level virtual sequence
```

源码位置：`mem_ut/ver/ut/memblock/seq/seq.f`

函数功能简析：`seq.f` 为 VCS 提供 `seq_pkg.sv` 的 include 搜索目录；本轮更新 incdir，使裸 include 文件名能在新目录结构下解析。

修改后源码：

```systemverilog
+incdir+./base_seq_help
+incdir+./base_seq
+incdir+./base_seq/soft_test
+incdir+./virtual_sequence
seq_pkg.sv
```

中文伪代码：

本段逻辑在本 feature 中承担“让编译器按新目录查找 sequence 文件”的功能。编译时先搜索 `base_seq_help`，这样 transaction、handler、scheduler 等 helper 类型先可见；再搜索 `base_seq`，让基础 sequence 文件可见；接着搜索 `base_seq/soft_test`，让 soft-test sequence 可见；最后保留空的 `virtual_sequence` 搜索路径，为后续顶层 virtual sequence 文件预留。`seq_pkg.sv` 仍然是唯一 package 编译入口，文件内部裸 include 不需要改成带路径 include。

源码位置：`mem_ut/ver/ut/memblock/seq/seq_pkg.sv`

函数功能简析：`seq_pkg.sv` 负责按依赖顺序 include helper 和 sequence 文件；本轮更新 include 文件名和顺序。

修改后源码：

```systemverilog
    `include "memblock_dispatch_base_sequence.sv"
    `include "soft_test_memblock_dispatch_smoke_sequence.sv"
    `include "soft_test_memblock_dispatch_fault_smoke_sequence.sv"
    `include "soft_test_memblock_dispatch_replay_smoke_sequence.sv"
    `include "memblock_lsqenq_dispatch_base_sequence.sv"
    `include "memblock_issue_dispatch_base_sequence.sv"
    `include "memblock_lsqcommit_dispatch_base_sequence.sv"
    `include "memblock_flushsb_base_sequence.sv"
    `include "memblock_redirect_dispatch_base_sequence.sv"
    `include "memblock_l2tlb_base_sequence.sv"
    `include "memblock_main_dispatch_auto_build_main_table_base_sequence.sv"
    `include "memblock_main_dispatch_manual_main_table_sequence.sv"
    `include "mem_base_sequence.sv"
```

中文伪代码：

本段逻辑在本 feature 中承担“按新文件名组织 package include”的功能。源码先 include dispatch base sequence，让后续 auto-build main sequence 和 soft-test sequence 能继承基础能力；然后连续 include 三个 soft-test sequence，保持 soft-test 组内顺序清晰；再 include LSQ enqueue、issue、LSQ commit、flushSb、redirect、L2TLB 等基础驱动 sequence；最后先 include auto-build main table sequence，再 include 继承它的 manual main table sequence，保证父类在子类之前被解析。`mem_base_sequence.sv` 仍保留在最后，维持原 package 组织方式。

### 3.3 testcase 和 default sequence 引用更新

功能目的：

UVM default sequence 和 testcase 手动 `type_id::create()` 必须使用新类名，否则编译或 factory 创建会失败。本轮只替换类型引用和 fatal 文本，不改变 testcase run flow。

源码位置：`mem_ut/ver/ut/memblock/tc/src/tc_base.sv`

函数功能简析：`tc_base::configure_real_smoke_default_sequences()` 为真实 dispatch smoke 配置各 agent 的 main_phase default sequence。

修改后源码：

```systemverilog
    uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_lsqcommit_agent_agent.sqr.main_phase"  , "default_sequence", memblock_lsqcommit_dispatch_base_sequence::type_id::get());
    uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_lsqenq_agent_agent.sqr.main_phase"  , "default_sequence", memblock_lsqenq_dispatch_base_sequence::type_id::get());
    uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_lintsissue_agent_agent.sqr.main_phase"  , "default_sequence", memblock_issue_dispatch_base_sequence::type_id::get());
    uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_redirect_agent_agent.sqr.main_phase"  , "default_sequence", memblock_redirect_dispatch_base_sequence::type_id::get());
```

中文伪代码：

本段逻辑在本 feature 中承担“agent default sequence 使用新 UVM 类型”的功能。源码依次给 LSQ commit、LSQ enqueue、lintsissue 和 redirect agent 的 sequencer 配置 default sequence；每个配置仍写入相同的 config_db 路径和 `main_phase` 字段，只是 `type_id::get()` 的类名换成新类名。这样 agent 启动时仍按原时序启动对应 sequence，但 UVM factory 只看到新类名，不再依赖旧类名。

源码位置：`mem_ut/ver/ut/memblock/tc/src/tc_dispatch_real_smoke.sv`

函数功能简析：`run_real_smoke_sequence()` 创建自动构建 main table 的真实 dispatch sequence。

修改后源码：

```systemverilog
        memblock_main_dispatch_auto_build_main_table_base_sequence real_smoke_seq;

        real_smoke_seq = memblock_main_dispatch_auto_build_main_table_base_sequence::type_id::create("real_smoke_seq");
        if (real_smoke_seq == null) begin
            `uvm_fatal(get_type_name(), "failed to create memblock_main_dispatch_auto_build_main_table_base_sequence")
        end
```

中文伪代码：

本段逻辑在本 feature 中承担“真实 dispatch auto-build testcase 创建新 sequence 类型”的功能。源码先声明新类型变量；再通过新类名调用 UVM factory 创建对象，实例名仍保留 `real_smoke_seq`，因此日志层面的实例语义不变；如果 factory 返回空对象，则 fatal 文本也打印新类名，便于定位新类型注册问题；创建成功后后续仍调用 `start(null)`，运行时行为不变。

源码位置：`mem_ut/ver/ut/memblock/tc/src/tc_dispatch_real_mixed_wb_smoke.sv`

函数功能简析：`run_real_smoke_sequence()` 创建手工 main table 的真实 dispatch sequence。

修改后源码：

```systemverilog
        memblock_main_dispatch_manual_main_table_sequence mixed_smoke_seq;

        mixed_smoke_seq = memblock_main_dispatch_manual_main_table_sequence::type_id::create("mixed_smoke_seq");
        if (mixed_smoke_seq == null) begin
            `uvm_fatal(get_type_name(), "failed to create memblock_main_dispatch_manual_main_table_sequence")
        end
```

中文伪代码：

本段逻辑在本 feature 中承担“manual main table testcase 创建新 sequence 类型”的功能。源码声明 manual main table sequence 类型变量；通过新类名创建 UVM 对象，实例名仍保留 `mixed_smoke_seq`；创建失败时 fatal 文本打印新类名；创建成功后的启动方式不变，因此 directed main table 构造和后续 service flow 没有行为变化。

### 3.4 空 virtual_sequence 目录保留

功能目的：

Git 不跟踪空目录。plan 要求新建 `seq/virtual_sequence` 作为后续 top-level virtual sequence 编排层，本轮通过 `.gitkeep` 保留该目录。

文件位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/.gitkeep`

内容：

```text
This directory is reserved for top-level virtual sequence orchestration.
```

中文伪代码：

本段文本在本 feature 中承担“保留空目录语义”的功能。仓库提交该文件后，`virtual_sequence` 目录在其他环境 checkout 时会存在；当前目录不参与编译，也不被任何 include 引用；后续如果新增真正 top-level virtual sequence，可以直接放入该目录并按需更新 `seq_pkg.sv` 和 `seq.f`。

## 4. 文档同步

已同步当前有效文档中的旧类名和旧路径：

1. `AI_DOC/mem_ut_flow_doc/*.md`
2. `AI_DOC/web/**/*.md`
3. `AI_DOC/web/**/*.js`
4. `mem_ut/ver/ut/memblock/rule/*.md`
5. `AI_DOC/project_management/mem_ut_code_review_document_rule.md`

旧名扫描范围：

```text
mem_ut/ver/ut/memblock
AI_DOC/mem_ut_flow_doc
AI_DOC/web
mem_ut/ver/ut/memblock/rule
AI_DOC/project_management
```

扫描结果：未命中 6 个旧 sequence 类名。

## 5. 与 Plan 不一致的实现

本轮 rename/re-layout 实现与 plan 功能目标一致。

需要说明的非本轮差异：

```text
plus.sv
seq_csr_common.sv
memblock_dispatch_base_sequence.sv
exception_redirect_replay_handler.sv
```

这些文件在本轮开始前已经存在未提交修改；本轮目录重排后，它们随 `seq/base_seq -> seq/base_seq_help` 移动，在 `git status` 中显示为 `RM`。这些修改不是本 rename plan 新增的行为逻辑，本 review 不把它们作为 rename plan 的功能变化结论。

## 6. Plan 未说明但实现补充的细节

1. `seq_pkg.sv` 中把三个 soft-test sequence include 连续放置。
   原因：subagent review 指出 plan 目标结构中三个 soft-test sequence 连续列出；最终实现按该顺序整理，降低后续审查噪声。

2. `AI_DOC/project_management/mem_ut_code_review_document_rule.md` 中示例旧类名同步更新。
   原因：该文件属于当前规则文档，不是历史 plan/review；如果保留旧类名，会让旧名扫描继续命中当前有效文档。

## 7. 验证结果

静态检查：

```bash
git diff --check -- mem_ut/ver/ut/memblock/seq mem_ut/ver/ut/memblock/tc AI_DOC
rg -n "memblock_lintsissue_dispatch_sequence|memblock_lsqcommit_dispatch_sequence|memblock_lsqenq_dispatch_sequence|memblock_redirect_dispatch_sequence|memblock_dispatch_real_smoke_sequence|memblock_dispatch_real_mixed_smoke_sequence" mem_ut/ver/ut/memblock AI_DOC/mem_ut_flow_doc AI_DOC/web mem_ut/ver/ut/memblock/rule AI_DOC/project_management
```

结果：

```text
git diff --check 无输出。
旧类名 rg 扫描无输出。
```

编译：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
```

结果：

```text
通过。最终日志显示 Verdi KDB elaboration is skipped because design is not changed。
```

Smoke：

```bash
make eda_batch_run tc=tc_dispatch_real_smoke mode=base_fun cfg=tc_dispatch_real_smoke
make eda_batch_run tc=tc_dispatch_real_mixed_wb_smoke mode=base_fun cfg=tc_dispatch_real_mixed_wb_smoke
```

结果：

```text
tc_dispatch_real_smoke: TEST CASE PASSED，日志出现 memblock_main_dispatch_auto_build_main_table_base_sequence。
tc_dispatch_real_mixed_wb_smoke: TEST CASE PASSED，日志出现 memblock_main_dispatch_manual_main_table_sequence。
```

## 8. Subagent Review 结论

subagent 只读审查结果：

1. 确认 `mem_ut/ver/ut/memblock` 源码中 6 个旧 sequence 类名、旧文件名、旧 include guard 未再命中。
2. 确认 testcase default sequence/create 已更新到新类名。
3. 指出 project_management rule 中仍有一个旧名示例，已修复。
4. 指出 `seq_pkg.sv` soft-test include 顺序与 plan 列表不完全一致，已修复。
5. 指出若提交时漏掉 `.gitkeep` 会丢失空目录，本轮已保留该文件。
6. 指出若把前序 dirty 修改混入本 plan commit 会造成行为变更误判，本 review 已将其标为非本轮行为变更。

## 9. Plan 对齐检查

| Plan 要求 | 实现状态 |
|---|---|
| 6 个 sequence 类名和文件名同步改名 | 已完成 |
| include guard、constructor 默认 name、UVM factory 注册名改为新名 | 已完成 |
| `seq/base_seq -> seq/base_seq_help` | 已完成 |
| `seq/virtual_sequence -> seq/base_seq` | 已完成 |
| 新建 `seq/virtual_sequence` | 已完成，通过 `.gitkeep` 保留 |
| `seq.f` incdir 更新 | 已完成 |
| `seq_pkg.sv` include 文件名和顺序更新 | 已完成 |
| testcase 引用更新 | 已完成 |
| 当前 flow/web/rule 文档引用更新 | 已完成 |
| 不新增旧名 typedef/wrapper | 已确认 |
| 编译和 smoke 验证 | 已通过 |

结论：本 plan 的 rename/re-layout 目标已完成，无已知 blocker。
