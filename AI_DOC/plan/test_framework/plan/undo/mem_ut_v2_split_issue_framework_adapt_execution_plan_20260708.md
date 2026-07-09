# mem_ut V2 split issue 测试框架适配执行 Plan

## 1. Plan 定位

本文是 V2 `issueLda/issueSta/issueStd` split issue 的测试框架适配执行 plan。当前 issue 路径已经使用 V2 split 字段，本 plan 只做细节收敛：版本化 pipe 数、字段宽度、target 到 port 的显式映射、vector issue 禁用边界和字段默认策略。

不改变：

- `issue_queue_scheduler` 的队列模型。
- send priority 仲裁。
- replay/redirect 后重新 route 的状态生命周期。

## 2. 范围边界

涉及文件：

```text
mem_ut/ver/ut/memblock/seq/base_seq_help/issue_field_assigner.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/issue_queue_scheduler.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_ctrl_model.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_types.sv
mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent/src/lintsissue_agent_agent_xaction.sv
mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent/src/lintsissue_agent_agent_driver.sv
mem_ut/ver/ut/memblock/agent/vecissue_agent_agent/src/vecissue_agent_agent_driver.sv
mem_ut/ver/ut/memblock/tb/lintsissue_agent_connect.sv
mem_ut/ver/ut/memblock/tb/vecissue_agent_connect.sv
```

依赖：

- 编译期参数/宽度 plan 提供真实 pipe 数和字段宽度。

不属于本 plan：

- 不实现 `issueVldu` 或 vector LS 主流程。
- 不实现 RM/checker/coverage。

### 2.1 执行前 RTL 基线确认

执行本 plan 前必须从仓库根目录确认当前 V2 RTL 权威输入真实存在：

```bash
test -e build/rtl/MemBlock.sv
test -e build_memblock/rtl/MemBlock.sv
test -e build_memblock/rtl/filelist.f
```

若任一文件不存在，必须先确认当前 worktree 的 RTL 生成状态和 V2 profile，不得继续沿用不存在的 `build_memblock/rtl/MemBlockTop.sv` 或同级旧 worktree 作为接口事实来源。本 plan 主要修改 issue helper/字段边界，也必须先确认 `issueLda/issueSta/issueStd/issueVldu` 的真实 RTL 口径；该检查不代表本 plan 会直接修改 RTL。

## 3. 问题依据

V2 接口事实：

```text
issueLda[0..2]
issueSta[0..1]
issueStd[0..1]
issueVldu[0..1]
```

当前源码事实：

- `issue_field_assigner::is_valid_pipe_idx()` 硬编码 LOAD 3、STA 2、STD 2。
- `assign_load_main_fields()`、`assign_sta_main_fields()`、`assign_std_main_fields()` 已按 V2 split port 赋值。
- `lsq_ctrl_model::derive_op_behavior()` 遇到 vector LS 会 fatal。
- `dispatch_monitor_event_adapter` 遇到 vector feedback 会 drop，`writeback_status_handler` 遇到 `vector_ls` 会 fatal。

## 4. 修改原因

V2 split issue 不是 V3 `intIssue` 的简单改名。测试框架必须明确：

- `MEMBLOCK_ISSUE_TARGET_LOAD` 只能驱动 `issueLda`。
- `MEMBLOCK_ISSUE_TARGET_STA` 只能驱动 `issueSta`。
- `MEMBLOCK_ISSUE_TARGET_STD` 只能驱动 `issueStd`。
- `issueVldu` 当前不在 scalar 主流程中支持，不能被 vector op 静默驱动。

同时 pipe 数和字段宽度必须从版本参数读取，避免 V3/V2 互相污染。

内部公共 `fuType` 仍保持 compile param plan 定义的 36-bit 规范宽度。V2 STA/STD DUT 端口为 `[34:0]` 时，只在 issue xaction/driver/connect 赋值边界通过 `fit_dut_futype()` 或 issue 专用等价 helper 做检查和适配，不能把内部 `fuType` 全局缩成 35 位。

## 5. 修改后方案

### 5.1 target 到 split port 权威映射

在 `issue_field_assigner` 增加注释或 helper：

```text
LOAD -> issueLda
STA  -> issueSta
STD  -> issueStd
VECTOR -> unsupported in this plan
```

### 5.2 pipe 数版本化

把 `is_valid_pipe_idx()` 从硬编码改为读取编译期真实 pipe 数：

```text
LOAD: pipe_idx < MEMBLOCK_LOAD_PIPE_NUM_CFG
STA : pipe_idx < MEMBLOCK_STA_PIPE_NUM_CFG
STD : pipe_idx < MEMBLOCK_STD_PIPE_NUM_CFG
```

`issue_queue_scheduler::select_issue_candidates()` 仍使用 `seq_csr_common::sample_*_pip_num()`，但这些 sample 函数已由参数 plan clamp 到真实上限。

### 5.3 V2 字段默认策略

`issue_field_assigner` 对以下字段必须说明来源：

| 字段 | 来源 |
|---|---|
| `fuOpType` | `main_tr.fuOpType` |
| `fuType` | STA/STD 从 `main_tr.fuType` 经 `fit_dut_futype()` 写入 DUT 端口；LOAD 如 V2 port 无该字段不补 |
| `robIdx` | `main_tr` 的 ROB key |
| `lqIdx/sqIdx` | `issue_queue_scheduler::make_issue_item()` 从 status active map 获取 |
| `ftqPtr/ftqOffset/pc/isRVC/pdest` | `assign_backend_meta_fields()` 计算 |
| `loadWait/storeSetHit` | `assign_issue_dep_fields()` 计算 |

对 `ftqOffset`，V2 端口是 4 bit 时继续写 `ftq_offset[3:0]`，但宽度必须与编译期参数或端口事实一致。

### 5.4 vector issue 边界

保留现有 `lsq_ctrl_model` vector fatal，并新增静态检查：

- 主表生成参数默认不生成 vector LS。
- 若 `MEMBLOCK_FUTYPE_VLDU/VSTU/VSEGLDU/VSEGSTU` 出现在 main table，`derive_op_behavior()` fatal。
- 不改变 `vecissue_agent` 结构，不删除其 interface/driver/connect；本 plan 只是禁止 scalar issue flow 静默消费 vector。`vecissue_agent` 的完整 V2 vector 支持另建专项 plan。

## 6. 函数/任务级伪代码

### 6.1 `get_target_pipe_limit()`

函数目的：提供 target 到真实 pipe 上限的唯一映射，替代 `is_valid_pipe_idx()` 内硬编码。

输入：`memblock_issue_target_e target`。

输出/副作用：返回 pipe 上限；非法 target fatal；无状态副作用。

源码级伪代码：

```text
function int unsigned get_target_pipe_limit(target);
    case (target)
      LOAD: return MEMBLOCK_LOAD_PIPE_NUM_CFG;
      STA : return MEMBLOCK_STA_PIPE_NUM_CFG;
      STD : return MEMBLOCK_STD_PIPE_NUM_CFG;
      default: fatal;
    endcase
endfunction
```

中文文字伪代码：

该函数把公共 target 枚举转换成 V2 真实 split issue port 数。LOAD 读取 load pipe 上限，STA 读取 store address pipe 上限，STD 读取 store data pipe 上限。非法 target 直接 fatal，因为当前 scalar issue flow 不存在其他 target。后续所有 pipe index 检查都先调用该函数，避免多个函数各自写 3/2/2。

### 6.2 `check_pipe_idx()`

函数目的：保留现有检查语义，但使用版本化上限。

源码级伪代码：

```text
function void check_pipe_idx(target, pipe_idx, caller);
    limit = get_target_pipe_limit(target);
    if (pipe_idx >= limit) fatal("%s got target pipe out of range");
endfunction
```

中文文字伪代码：

该函数在每个字段赋值 helper 入口执行。它先拿到 target 对应的真实 pipe 上限，再判断本次选择的 pipe index 是否越界。如果越界，说明 scheduler 采样数量、field assigner case 分支或编译期宏不一致，必须 fatal 暴露，而不是让 driver 写到不存在的 port。

### 6.3 `assign_issue_item_fields()` 新增 wrapper

函数目的：当前源码已有 `assign_main_issue_fields()`、`assign_issue_dep_fields()`、`assign_backend_meta_fields()` 三个函数。本文建议新增 `assign_issue_item_fields()` wrapper，作为三段式赋值的唯一调用入口，并在该 wrapper 中增加 V2 split port 语义检查。

源码落点和调用点：新增在 `issue_field_assigner.sv`；调用点应替换当前 issue sequence/driver 构造 xaction 时对三段函数的直接调用。执行前用 `rg -n "assign_main_issue_fields|assign_issue_dep_fields|assign_backend_meta_fields|assign_issue_item_fields" mem_ut/ver/ut/memblock/seq` 确认实际调用点。

源码级伪代码：

```text
function void assign_issue_item_fields(tr, item, pipe_idx);
    check_unsupported_vector_issue(item);
    check_pipe_idx(item.target, pipe_idx, "assign_issue_item_fields");
    assign_main_issue_fields(tr, item, pipe_idx);
    assign_issue_dep_fields(tr, item, pipe_idx);
    assign_backend_meta_fields(tr, item, pipe_idx);
endfunction
```

中文文字伪代码：

该函数仍是 issue xaction 字段填充的总入口。它先确认 item 不是 vector issue，再确认 pipe index 合法。随后按现有顺序写主字段、依赖字段和 backend 元信息。主字段负责 valid、fuOpType、源操作数和 ROB/LQ/SQ key；依赖字段只在 load 上写 loadWait/storeSetHit；backend meta 写 pc、ftq、pdest、rfWen/fpWen 等辅助字段。该函数不改变 issue queue 出队和状态表推进，真正 fire 仍由 scheduler 的 mark 函数完成。

### 6.4 `fit_issue_dut_futype()` 或复用 `fit_dut_futype()`

函数目的：在 STA/STD 内部 36-bit `fuType` 写入 V2 issue DUT 端口前做边界检查，防止隐式截断。

源码落点：优先复用 compile param plan 的公共 `fit_dut_futype()`；若执行者选择在 `issue_field_assigner.sv` 内新增 `fit_issue_dut_futype()`，必须调用同一套检查规则，并在 review 中说明它与公共 helper 等价。

输入：`main_tr.fuType`、caller 字符串。

输出/副作用：返回 V2 DUT 端口宽度 `fuType`；vector 或高位超出时 fatal；不修改公共状态。

源码级伪代码：

```text
function bit [MEMBLOCK_DUT_FUTYPE_W-1:0] fit_issue_dut_futype(internal_fuType, caller);
    return fit_dut_futype(internal_fuType, caller);
endfunction
```

中文文字伪代码：

该 helper 是 issue 赋值边界的防截断检查。STA/STD 写 `io_ooo_to_mem_issueSta/Std_*_bits_uop_fuType` 前先调用它。helper 会拒绝 vector LS 和高于 V2 DUT 端口宽度的 bit，只有可安全表达的 scalar FU type 才返回给 xaction 字段。

### 6.5 `check_unsupported_vector_issue()`

函数目的：防止 V2 `issueVldu` 被错误地按 scalar split issue 处理。

源码级伪代码：

```text
function void check_unsupported_vector_issue(item);
    main_tr = data.get_main_transaction(item.uid);
    if (lsq_ctrl_model::is_vector_ls_futype(main_tr.fuType)) begin
        fatal("vector LS issue is unsupported by scalar split issue plan");
    end
endfunction
```

中文文字伪代码：

该函数根据 item uid 读取主表 transaction，并复用 `lsq_ctrl_model` 的 vector FU type 判断。如果发现 vector LS，立即 fatal，说明当前测试框架还没有 vector issue/writeback 闭环。这样做是为了避免 vector request 被误写到 LOAD/STA/STD scalar port 后产生错误状态。

## 7. 验收标准

1. `issue_field_assigner::is_valid_pipe_idx()` 不再硬编码 3/2/2。
2. `select_issue_candidates()` 采样数量不会超过编译期真实 pipe 数。
3. LOAD/STA/STD target 到 issueLda/Sta/Std 的映射在注释或 helper 中明确。
4. vector LS 仍显式 fatal 或通过主表参数禁止生成，不进入 scalar split issue。
5. 不新增高频全表扫描；scheduler 仍只扫描 issue queue 或 active window。
6. `ftqOffset`、ROB/LQ/SQ key、FU type 宽度适配与参数 plan 对齐。
7. `assign_issue_item_fields()` 若为新增 wrapper，所有实际调用点已迁移到该 wrapper；若当前源码已有等价函数，plan 执行 review 必须列出真实函数名。
8. STA/STD `fuType` 写入 V2 DUT 端口前调用 `fit_dut_futype()` 或等价 helper，不隐式截断。

## 8. 验证命令或静态检查

```bash
git diff --check -- mem_ut/ver/ut/memblock/seq mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent mem_ut/ver/ut/memblock/agent/vecissue_agent_agent AI_DOC
rg -n "pipe_idx < 3|pipe_idx < 2|issueVldu|VECTOR|VLDU|VSTU|VSEGLDU|VSEGSTU" mem_ut/ver/ut/memblock/seq/base_seq_help mem_ut/ver/ut/memblock/agent
rg -n "assign_main_issue_fields|assign_issue_dep_fields|assign_backend_meta_fields|assign_issue_item_fields|fit_dut_futype|fit_issue_dut_futype" mem_ut/ver/ut/memblock/seq/base_seq_help mem_ut/ver/ut/memblock/seq/base_seq
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
make eda_run tc=tc_sanity mode=base_fun
```

真实 dispatch 主流程修改后增加：

```bash
make eda_run tc=basicTest ts=memblock_dispatch_real_smoke_vseq mode=base_fun cfg=tc_dispatch_real_smoke
```

## 9. 与原始/初步 plan 差异说明

初步 plan 只要求确认 V2 split issue 语义。本文将确认项转为 coding 步骤：建立 target 到 port 的 helper、版本化 pipe 上限、保留三段式字段填充并加入 vector unsupported 检查。

## 10. 风险与非目标

风险：

- 若 V2 Scala 中 STA/STD pipe 顺序与当前 port 顺序不同，需要在 coding 前重新确认并更新 helper 映射。
- 若 testcase 已经生成 vector LS，本 plan 会让它 fatal；这是显式风险暴露，不是回归。

非目标：

- 不实现 `issueVldu`。
- 不改写 issue queue scheduler 的仲裁算法。
- 不实现 vector writeback/replay。

## 11. 与原测试框架逻辑对比和修改类型总结

修改类型结论：`局部逻辑适配 + 仅字段/参数适配`。局部逻辑适配是 pipe 上限 helper、target 到 V2 split port 的显式检查和 vector unsupported fatal；字段/参数适配是 pipe 数、`fuType` 宽度和 V2 issue 字段来源收敛。issue queue 主调度算法不改变。

原测试框架逻辑：

- `issue_queue_scheduler::prepare_issue_route_for_uid()` 在 LSQ admission 后把 uid 标记为 issue ready，再由 `route_uid()` 按 `lsq_ctrl_model::derive_op_behavior()` 将 LOAD/STA/STD item 放入对应 issue queue。
- `route_all_ready_uids()` 在 active window 内有限扫描，不全表扫描；`select_target_candidates()` 在目标 queue 内选最老或最高优先级候选。
- `issue_field_assigner::clear_lintsissue_xaction()` 清空 V2 split issue xaction；`assign_load_main_fields()`、`assign_sta_main_fields()`、`assign_std_main_fields()` 分别写 issueLda/Sta/Std 字段。
- `issue_field_assigner::is_valid_pipe_idx()` 当前硬编码 LOAD 3、STA 2、STD 2；STA/STD `fuType` 当前直接写 `main_tr.fuType`。

本 plan 修改后逻辑：

- issue queue 的入队、候选选择、出队、mark fire 和 replay/redirect 恢复策略保持不变。
- pipe 数从编译期宏和 `seq_csr_common` clamp 后的 runtime 限制读取，不再散落硬编码。
- 字段赋值入口增加 `check_pipe_idx()`、`fit_issue_dut_futype()` 或复用 `fit_dut_futype()`，确保写入 V2 issueLda/Sta/Std 的字段合法。
- vector LS 不进入 scalar LOAD/STA/STD issue flow，遇到 `VLDU/VSTU/VSEGLDU/VSEGSTU` 时 fatal 或由主表参数禁止生成。

逻辑改变项：

1. `is_valid_pipe_idx()` 从硬编码改为读取 target pipe limit。原因是 V2/V3 pipe 数差异应参数化。该改变只修正边界判断，不改变仲裁算法。
2. 新增 `assign_issue_item_fields()` wrapper 或在现有赋值函数入口增加统一检查。原因是需要防止 target/pipe/fuType 不匹配。该 wrapper 不推进 status，只调用原字段赋值 helper。
3. `check_unsupported_vector_issue()` 在 scalar issue plan 中 fatal。原因是当前没有 vector issue/writeback 生命周期闭环。该改变只约束未支持场景。
4. STA/STD `fuType` 写入前增加 DUT 宽度 fit 检查。原因是 V2 DUT 字段宽度不能隐式截断。

字段/参数改变项：

- LOAD/STA/STD pipe 上限由 `MEMBLOCK_LOAD_PIPE_NUM_CFG`、`MEMBLOCK_STA_PIPE_NUM_CFG`、`MEMBLOCK_STD_PIPE_NUM_CFG` 和 runtime `MEMBLOCK_*_PIP_NUM_LIMIT` 共同决定。
- STA/STD `uop_fuType` 写入使用 DUT 边界宽度；LOAD 若 V2 port 无 `fuType` 字段则不新增伪字段。
- `ftqOffset`、ROB/LQ/SQ key、`fuOpType`、`rfWen/fpWen` 等字段保持原来源，但需要在 review 中确认宽度与 V2 端口一致。

性能/生命周期影响：

- RTL 基线路径确认只发生在执行前准备阶段，用于防止误读不存在的 `MemBlockTop.sv` 或错误 worktree，不属于测试框架 runtime 逻辑改变。
- 不新增每拍全表扫描；`route_all_ready_uids()` 仍按 active window 和 `real_lsq_enq_max` 限制扫描。
- `select_target_candidates()` 仍只扫描目标 issue queue；本 plan 不改变其优先级和最老选择规则。
- 不新增 active map、queue、prefix 或 cursor，不改变 issue item 的 queued/dispatched 状态生命周期。
- 不改变 terminal/pass/fail；只改变发射字段合法性和未支持 vector 的失败策略。

覆盖性结论：

本 plan 覆盖 V2 split issue 适配，包含 LOAD/STA/STD scalar issue port 映射、pipe 数版本化和 vector 边界。LSQ enqueue 进入 issue queue 的前置字段由 LSQ plan 覆盖；writeback 回收由 int writeback plan 覆盖。结论是：本 plan 只做 V2 split issue 细节和合法性适配，不改写测试框架主体 issue 调度逻辑。
