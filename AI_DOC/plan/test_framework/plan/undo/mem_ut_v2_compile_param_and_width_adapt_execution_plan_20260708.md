# mem_ut V2 编译期参数与宽度适配执行 Plan

## 1. Plan 定位

本文是 V2/V3 静态结构差异的测试框架执行 plan。目标是在不改变测试框架主体调度、状态表和 handler 架构的前提下，把 ROB/LQ/SQ index 宽度、DUT 端口 `fuType` 宽度、LSQ enqueue slot 数、issue pipe 数等静态结构收敛到单一权威入口。

本 plan 只处理编译期结构参数和 runtime 参数 clamp，不实现 LSQ enqueue、issue、writeback、L2TLB 或 monitor 的业务逻辑修改；这些内容由后续专项 plan 执行。

## 2. 范围边界

涉及文件：

```text
mem_ut/ver/ut/memblock/cfg/memblock_compile_params.svh
mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_types.sv
mem_ut/ver/ut/memblock/env/plus.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv
mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg
```

只允许做：

- 新增 V2/V3 静态结构宏默认值。
- 将 `MEMBLOCK_ROB_VALUE_W`、`MEMBLOCK_LQ_VALUE_W`、`MEMBLOCK_SQ_VALUE_W`、DUT 端口 `fuType` 宽度、`MEMBLOCK_REAL_LSQ_ENQ_MAX`、LOAD/STA/STD pipe 数上限改为消费编译期宏。
- 在 `seq_csr_common::validate_and_clamp()` 中用编译期真实上限 clamp runtime plus 参数。
- 保留 runtime plus 参数作为“本 testcase 本次运行使用多少 slot/pipe”的行为控制。

不允许做：

- 用 runtime plusarg 改变 packed typedef 宽度、interface 数组维度或 connect-time 端口数。
- 为 V2/V3 宽度差异引入两套状态表。
- 通过截断赋值掩盖位宽不一致。
- 把测试框架内部公共 `fuType` 规范宽度缩成 V2 DUT 端口宽度。内部常量和 `lsq_ctrl_model::is_vector_ls_futype()` 仍保留 36-bit 语义；V2 DUT 端口 `[34:0]` 只在 agent/xaction/driver/connect 赋值边界适配。

### 2.1 执行前 RTL 基线确认

执行本 plan 前必须从仓库根目录确认当前 V2 RTL 权威输入真实存在：

```bash
test -e build/rtl/MemBlock.sv
test -e build_memblock/rtl/MemBlock.sv
test -e build_memblock/rtl/filelist.f
```

若任一文件不存在，必须先确认当前 worktree 的 RTL 生成状态和 V2 profile，不得继续沿用不存在的 `build_memblock/rtl/MemBlockTop.sv` 或同级旧 worktree 作为接口事实来源。本 plan 主要修改参数/typedef，仍需要该检查来避免宽度和 slot 数来自错误 RTL 基线；该检查不代表本 plan 会直接修改 RTL。

## 3. 问题依据

V2 接口分析记录：

- V2 `robIdx_value` 多处为 8 bit，V3 多处为 9 bit。
- V2 `enqLsq_req_*_bits_fuType` 为 `[34:0]`，V3 为 `[35:0]`。
- V2 `ftqOffset` 常见 4 bit，V3 常见 5 bit。
- V2 issue 端口为 `issueLda[0..2]`、`issueSta[0..1]`、`issueStd[0..1]`，LSQ enqueue 当前为 6 slot。

当前源码问题：

- `memblock_dispatch_types.sv` 中 `MEMBLOCK_ROB_VALUE_W=9`、`MEMBLOCK_FUTYPE_*` 为 `bit [35:0]`。内部 `fuType` 36-bit 语义本身不能删除，但 DUT 端口宽度需要版本化边界检查。
- `memblock_compile_params.svh` 只有 `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN`。
- `plus.sv/default.cfg/seq_csr_common.sv` 中存在 runtime `MEMBLOCK_REAL_LSQ_ENQ_MAX`、`MEMBLOCK_REAL_ENQ_WIDTH`、pipe limit，但它们不应承担静态端口宽度职责。
- V2 当前实际 LSQ enqueue 口径必须按当前 RTL 和 agent 确认。已知当前 connect/xaction 使用 slot 0..5 共 6 个，执行前必须用 `rg` 确认，不得被旧分析中的 req_6/7 误导。

## 4. 修改原因

V2/V3 静态结构差异决定编译后 SystemVerilog 类型、interface 字段宽度和 case 分支范围。若继续把这些结构写成 runtime plus 或散落硬编码，后续 LSQ enqueue、issue、writeback 适配会出现三类问题：

1. V2 下字段宽度和 typedef 不一致，编译或连接阶段可能出现截断/扩展风险。
2. V3 下回归时被 V2 临时默认值影响。
3. 各 flow 自己各改一处，会让公共状态表 key 宽度、raw queue 宽度和 agent 字段宽度语义分裂。

## 5. 修改后方案

### 5.1 编译期宏

在 `memblock_compile_params.svh` 新增带默认值的宏：

```text
MEMBLOCK_VERSION_V2_EN
MEMBLOCK_ROB_VALUE_W_CFG
MEMBLOCK_LQ_VALUE_W_CFG
MEMBLOCK_SQ_VALUE_W_CFG
MEMBLOCK_INTERNAL_FUTYPE_W_CFG
MEMBLOCK_DUT_FUTYPE_W_CFG
MEMBLOCK_FTQ_OFFSET_W_CFG
MEMBLOCK_LSQ_ENQ_SLOT_NUM_CFG
MEMBLOCK_LOAD_PIPE_NUM_CFG
MEMBLOCK_STA_PIPE_NUM_CFG
MEMBLOCK_STD_PIPE_NUM_CFG
```

V2 默认建议：

```text
MEMBLOCK_VERSION_V2_EN = 1
MEMBLOCK_ROB_VALUE_W_CFG = 8
MEMBLOCK_LQ_VALUE_W_CFG = 7
MEMBLOCK_SQ_VALUE_W_CFG = 6
MEMBLOCK_INTERNAL_FUTYPE_W_CFG = 36
MEMBLOCK_DUT_FUTYPE_W_CFG = 35
MEMBLOCK_FTQ_OFFSET_W_CFG = 4
MEMBLOCK_LSQ_ENQ_SLOT_NUM_CFG = 6
MEMBLOCK_LOAD_PIPE_NUM_CFG = 3
MEMBLOCK_STA_PIPE_NUM_CFG = 2
MEMBLOCK_STD_PIPE_NUM_CFG = 2
```

若同一文件要保留 V3 兼容默认，必须通过显式 `ifndef` 默认值和分支注释说明 V3 值，不得依赖当前分支外部隐式宏。

### 5.2 类型统一

在 `memblock_dispatch_types.sv` 中：

- `MEMBLOCK_ROB_VALUE_W` 等 localparam 从宏派生。
- 新增或保留 `MEMBLOCK_INTERNAL_FUTYPE_W=36`，`MEMBLOCK_FUTYPE_*` 继续使用内部规范宽度，保证现有 `lsq_ctrl_model::is_vector_ls_futype()` 和 vector unsupported 检查还能识别 36-bit vector 常量。
- 新增 `MEMBLOCK_DUT_FUTYPE_W` 或等价 localparam，从 `MEMBLOCK_DUT_FUTYPE_W_CFG` 派生，仅用于 agent/xaction/driver/connect 赋值边界。
- V2 DUT 端口为 `[34:0]` 时，所有写入 DUT `fuType` 字段的位置必须调用 `fit_dut_futype()` 或等价 helper。该 helper 不允许无说明截断；scalar LDU/STU/MOU 可通过，vector 或高位超出必须 fatal/drop，由 vector unsupported 策略处理。
- 所有 raw key typedef 继续从 `MEMBLOCK_*_VALUE_W` 派生。

### 5.3 runtime 参数 clamp

`seq_csr_common::validate_and_clamp()` 增加检查：

- `real_lsq_enq_max`、`real_enq_width` clamp 到 `MEMBLOCK_LSQ_ENQ_SLOT_NUM_CFG`。
- `load_pip_num_limit` clamp 到 `MEMBLOCK_LOAD_PIPE_NUM_CFG`。
- `sta_pip_num_limit` clamp 到 `MEMBLOCK_STA_PIPE_NUM_CFG`。
- `std_pip_num_limit` clamp 到 `MEMBLOCK_STD_PIPE_NUM_CFG`。
- 若用户 plusarg 超过编译期结构上限，打印 `uvm_warning` 并 clamp；若小于 1 则用 1 或 fatal，按现有函数风格保持一致。

### 5.4 执行前 slot 口径确认

执行本 plan 前必须确认当前实际 slot 最大编号：

```bash
rg -n "enqLsq_req_[0-9]+|enqLsq_needAlloc_[0-9]+" \
  build_memblock/rtl/MemBlock.sv \
  build/rtl/MemBlock.sv \
  mem_ut/ver/ut/memblock/tb/lsqenq_agent_connect.sv \
  mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src
```

验收口径以当前存在且实际连接的最大 slot 为准。当前已知 V2 环境按 slot 0..5 共 6 个执行；如果重新生成 RTL 后出现差异，先更新 profile/接口适配 plan，再执行本 plan。

## 6. 函数/任务级伪代码

### 6.1 `check_compile_param_consistency()`

函数目的：在 `seq_csr_common::validate_and_clamp()` 中集中检查编译期宏与 runtime 参数的关系，避免每个 flow 自己判断真实端口数。

输入：

- 编译期宏：`MEMBLOCK_LSQ_ENQ_SLOT_NUM_CFG`、`MEMBLOCK_LOAD_PIPE_NUM_CFG`、`MEMBLOCK_STA_PIPE_NUM_CFG`、`MEMBLOCK_STD_PIPE_NUM_CFG`。
- runtime 字段：`real_lsq_enq_max`、`real_enq_width`、`load_pip_num_limit`、`sta_pip_num_limit`、`std_pip_num_limit`。

输出/副作用：

- 对超出编译期上限的 runtime 字段执行 clamp。
- 对非法 0 值执行 fatal 或修正为 1。
- 不修改 packed typedef 和 interface 结构。

源码级伪代码：

```text
function void check_compile_param_consistency();
    if (MEMBLOCK_LSQ_ENQ_SLOT_NUM_CFG == 0) fatal;
    if (MEMBLOCK_LOAD_PIPE_NUM_CFG == 0) fatal;
    if (MEMBLOCK_STA_PIPE_NUM_CFG == 0) fatal;
    if (MEMBLOCK_STD_PIPE_NUM_CFG == 0) fatal;

    real_lsq_enq_max = clamp_with_warning("MEMBLOCK_REAL_LSQ_ENQ_MAX",
                                          real_lsq_enq_max,
                                          1,
                                          MEMBLOCK_LSQ_ENQ_SLOT_NUM_CFG);
    real_enq_width = clamp_with_warning("MEMBLOCK_REAL_ENQ_WIDTH",
                                        real_enq_width,
                                        1,
                                        MEMBLOCK_LSQ_ENQ_SLOT_NUM_CFG);
    if (real_enq_width != real_lsq_enq_max) begin
        warning;
        real_enq_width = real_lsq_enq_max;
    end

    load_pip_num_limit = clamp_with_warning("MEMBLOCK_LOAD_PIP_NUM_LIMIT",
                                            load_pip_num_limit,
                                            1,
                                            MEMBLOCK_LOAD_PIPE_NUM_CFG);
    sta_pip_num_limit = clamp_with_warning("MEMBLOCK_STA_PIP_NUM_LIMIT",
                                           sta_pip_num_limit,
                                           1,
                                           MEMBLOCK_STA_PIPE_NUM_CFG);
    std_pip_num_limit = clamp_with_warning("MEMBLOCK_STD_PIP_NUM_LIMIT",
                                           std_pip_num_limit,
                                           1,
                                           MEMBLOCK_STD_PIPE_NUM_CFG);

    real_load_pipe_num = load_pip_num_limit;
    real_sta_pipe_num = sta_pip_num_limit;
    real_std_pipe_num = std_pip_num_limit;
endfunction
```

中文文字伪代码：

该函数在参数初始化阶段运行一次。它先确认编译期真实结构上限不是 0，因为端口数或 slot 数为 0 表示编译配置本身不可用。随后对 LSQ enqueue runtime 宽度做范围限制：用户可以把本次运行的入队宽度调小，但不能调大到超过编译期真实 slot 数。若历史兼容字段 `MEMBLOCK_REAL_ENQ_WIDTH` 与新字段 `MEMBLOCK_REAL_LSQ_ENQ_MAX` 不一致，函数以新字段为准并打印 warning，避免两个字段继续表达同一语义。最后对 LOAD/STA/STD pipe runtime 限制做同样 clamp，并把历史兼容 `real_*_pipe_num` 同步成 clamp 后结果。这个函数只处理 runtime 行为上限，不改变 interface 类型宽度。

### 6.2 `get_compile_*()` getter

函数目的：给后续 LSQ/issue plan 提供统一真实结构上限读取入口。

源码落点：建议落在 `seq_csr_common.sv`，作为 runtime class 可读的编译期上限 getter，例如 `seq_csr_common::get_compile_lsq_enq_slot_num()`、`seq_csr_common::get_compile_load_pipe_num()`。如果执行者选择放在 `memblock_dispatch_types.sv`，则必须改名为 localparam，并同步修改调用点，不得在 plan 中保留无法搜索到的伪 getter。

输入：无运行期输入，只返回编译期 localparam。

输出/副作用：返回真实 slot/pipe/FUType 宽度；无状态副作用。

源码级伪代码：

```text
function int unsigned get_compile_lsq_enq_slot_num();
    return MEMBLOCK_LSQ_ENQ_SLOT_NUM_CFG;
endfunction

function int unsigned get_compile_load_pipe_num();
    return MEMBLOCK_LOAD_PIPE_NUM_CFG;
endfunction
```

中文文字伪代码：

这些 getter 只把编译期宏变成测试框架可读的稳定入口。后续 `issue_field_assigner` 检查 pipe index、`memblock_lsqenq_dispatch_base_sequence` 清空 slot、`seq_csr_common` 采样本拍 pipe 数时都读取这些 getter 或对应 localparam，不再把 `3/2/2/6` 直接写在多个文件里。

### 6.3 `fit_dut_futype()`

函数目的：在内部 36-bit `fuType` 写入 V2 DUT 35-bit 端口前做边界检查和裁剪，禁止无说明截断。

源码落点：建议放在 `memblock_dispatch_types.sv` 或新增公共 helper 文件中，由 `memblock_lsqenq_dispatch_base_sequence.sv` 和 `issue_field_assigner.sv` 调用。若放在 class 内，必须分别在 LSQ enqueue 和 issue assigner 中实现同名语义，并在 review 说明没有分叉。

输入：内部规范宽度 `bit [MEMBLOCK_INTERNAL_FUTYPE_W-1:0] internal_fuType`。

输出/副作用：返回 `bit [MEMBLOCK_DUT_FUTYPE_W-1:0]`；若高于 DUT 宽度的 bit 非 0 或属于当前不支持 vector LS，fatal 或返回失败，不修改公共状态。

源码级伪代码：

```text
function bit [MEMBLOCK_DUT_FUTYPE_W-1:0] fit_dut_futype(internal_fuType, caller);
    if (lsq_ctrl_model::is_vector_ls_futype(internal_fuType)) begin
        fatal("%s got unsupported vector fuType", caller);
    end
    if (internal_fuType[MEMBLOCK_INTERNAL_FUTYPE_W-1:MEMBLOCK_DUT_FUTYPE_W] != '0) begin
        fatal("%s fuType has bits above DUT width", caller);
    end
    return internal_fuType[MEMBLOCK_DUT_FUTYPE_W-1:0];
endfunction
```

中文文字伪代码：

该 helper 是内部规范宽度和 DUT 端口宽度之间的唯一裁剪边界。它先检查当前 `fuType` 是否属于不支持的 vector LS；如果是，立即 fatal，交给 vector unsupported 策略处理。随后检查 V2 DUT 端口宽度以上的 bit 是否为 0；若不为 0，说明该值不能安全写入 V2 `[34:0]` 端口，也必须 fatal。只有 scalar LDU/STU/MOU 这类能放入 V2 DUT 宽度的值，才返回低位给 agent/xaction/driver/connect 使用。

## 7. 验收标准

1. `memblock_compile_params.svh` 中每个新增宏都有默认值和中文注释。
2. `memblock_dispatch_types.sv` 中 ROB/LQ/SQ 宽度从编译期宏派生，内部 `fuType` 保留 36-bit 规范宽度，DUT `fuType` 端口宽度单独从 `MEMBLOCK_DUT_FUTYPE_W_CFG` 派生。
3. `seq_csr_common::validate_and_clamp()` 明确区分编译期结构上限和 runtime 行为限制。
4. `MEMBLOCK_REAL_LSQ_ENQ_MAX`、`MEMBLOCK_REAL_ENQ_WIDTH`、LOAD/STA/STD pipe runtime 参数不能超过编译期上限。
5. 所有写入 V2 DUT `fuType` 端口的位置调用 `fit_dut_futype()` 或等价 helper，不允许隐式截断。
6. 执行前用 `rg` 确认当前 LSQ enqueue slot 仍为 0..5 共 6 个；如不是，先更新接口 profile 和本 plan。
7. `rg -n "pipe_idx < 3|pipe_idx < 2|slot < 6" mem_ut/ver/ut/memblock/seq` 不再出现需要版本化的新硬编码；历史无法迁移项必须在 review 中说明。
8. 不新增 RM、coverage、checker 逻辑。

## 8. 验证命令或静态检查

```bash
git diff --check -- mem_ut/ver/ut/memblock/cfg mem_ut/ver/ut/memblock/env mem_ut/ver/ut/memblock/seq AI_DOC
rg -n "MEMBLOCK_ROB_VALUE_W|MEMBLOCK_LQ_VALUE_W|MEMBLOCK_SQ_VALUE_W|MEMBLOCK_INTERNAL_FUTYPE_W|MEMBLOCK_DUT_FUTYPE_W|MEMBLOCK_LSQ_ENQ_SLOT_NUM|MEMBLOCK_LOAD_PIPE_NUM_CFG|fit_dut_futype" mem_ut/ver/ut/memblock
rg -n "enqLsq_req_[0-9]+|enqLsq_needAlloc_[0-9]+" build_memblock/rtl/MemBlock.sv build/rtl/MemBlock.sv mem_ut/ver/ut/memblock/tb/lsqenq_agent_connect.sv mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src
rg -n "pipe_idx < 3|pipe_idx < 2|slot < 6" mem_ut/ver/ut/memblock/seq
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
```

若后续专项 plan 同时修改运行期 flow，按专项 plan 再执行 `make eda_run tc=tc_sanity mode=base_fun`。

## 9. 与原始/初步 plan 差异说明

初步总控 plan 只指出“参数和宏单一权威来源必须先落地”。本文将其整理成可 coding 步骤：先新增编译期宏，再改 typedef/localparam，最后只在 `seq_csr_common` 中 clamp runtime plus 参数。本文明确禁止把静态端口宽度做成 runtime plusarg，也明确保留 runtime plus 作为小于等于真实结构的 testcase 行为限制。

## 10. 风险与非目标

风险：

- 如果当前 agent interface/xaction 已经由脚本生成固定宽度字段，typedef 宽度改动可能暴露出接口字段仍硬编码的问题；这属于本 plan 应暴露的编译风险，不应通过截断掩盖。
- V3 兼容值必须由执行者确认当前分支是否仍需要同时支持 V3 编译。

非目标：

- 不修改 LSQ enqueue 字段填充策略。
- 不修改 issue scheduler 主调度算法。
- 不修改 writeback event 归一化。
- 不处理 L2TLB response 字段链路。

## 11. 与原测试框架逻辑对比和修改类型总结

修改类型结论：`仅字段/参数适配 + 局部逻辑适配`。编译期宏、typedef/localparam 和 runtime clamp 属于字段/参数边界适配；`fit_dut_futype()` 的 fatal 检查属于写 DUT 边界前的局部合法性检查，不改变测试框架主体 flow。

原测试框架逻辑：

- 编译期参数当前主要落在 `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_types.sv` 的 localparam，例如 `MEMBLOCK_ROB_VALUE_W`、`MEMBLOCK_LQ_VALUE_W`、`MEMBLOCK_SQ_VALUE_W` 和 36-bit `MEMBLOCK_FUTYPE_*` 常量。
- runtime 参数由 `mem_ut/ver/ut/memblock/env/plus.sv` 解析，再由 `seq_csr_common::init()`、`load_from_plus()`、`validate_and_clamp()` 固化为 getter。LSQ enqueue flow 通过 `seq_csr_common::get_enq_per_cycle()`、`get_real_enq_width()` 控制本拍入队数量，issue flow 通过 `sample_*_pip_num()` 控制本拍 issue pipe 数。
- 原有状态生命周期不在参数文件中推进。admission、issue、writeback、L2TLB 都分别在各自 sequence/handler 中更新状态表和 queue。

本 plan 修改后逻辑：

- 静态结构差异由 `memblock_compile_params.svh` 中的宏统一描述，再派生到 typedef/localparam、agent 边界和 helper。
- runtime plusarg 仍只表达“本 testcase 本次运行使用多少 slot/pipe”，通过 `seq_csr_common::validate_and_clamp()` 被编译期真实上限 clamp。
- `fit_dut_futype()` 只在写 V2 DUT `fuType` 端口前检查内部 36-bit 规范值是否能安全表达为 V2 DUT 宽度；通过时返回裁剪后的 DUT 边界字段，失败时 fatal。
- 不改变 LSQ admission 高水位、issue queue 仲裁、raw monitor queue 消费、handler 终态推进或 L2TLB responder loop。

逻辑改变项：

1. `seq_csr_common::validate_and_clamp()` 增加编译期上限检查和 runtime 参数 clamp。原因是 V2/V3 端口数和宽度不同，runtime 不能越过编译期结构。该改变只发生在初始化/参数刷新路径，不改变主体架构。
2. `fit_dut_futype()` 增加 vector/high-bit fatal。原因是 V2 DUT `fuType` 端口比内部规范宽度窄，不能静默截断。该改变只影响非法或未支持激励的失败策略，不改变正常 scalar flow。

字段/参数改变项：

- 新增或统一 `MEMBLOCK_ROB_VALUE_W_CFG`、`MEMBLOCK_LQ_VALUE_W_CFG`、`MEMBLOCK_SQ_VALUE_W_CFG`、`MEMBLOCK_INTERNAL_FUTYPE_W_CFG`、`MEMBLOCK_DUT_FUTYPE_W_CFG`。
- 新增或统一 `MEMBLOCK_LSQ_ENQ_SLOT_NUM_CFG`、`MEMBLOCK_LOAD_PIPE_NUM_CFG`、`MEMBLOCK_STA_PIPE_NUM_CFG`、`MEMBLOCK_STD_PIPE_NUM_CFG`。
- 继续保留 `MEMBLOCK_REAL_LSQ_ENQ_MAX`、`MEMBLOCK_REAL_ENQ_WIDTH`、`MEMBLOCK_LOAD_PIP_NUM_LIMIT`、`MEMBLOCK_STA_PIP_NUM_LIMIT`、`MEMBLOCK_STD_PIP_NUM_LIMIT` 作为 runtime 行为限制。
- 内部 `MEMBLOCK_FUTYPE_*` 常量继续保持 36-bit，DUT 端口写入使用独立 DUT 宽度。

性能/生命周期影响：

- RTL 基线路径确认只发生在执行前准备阶段，用于防止误读不存在的 `MemBlockTop.sv` 或错误 worktree，不属于测试框架 runtime 逻辑改变。
- 不新增每拍或每 event 扫描。
- 不新增 active map、raw queue、prefix、cursor 或 issue queue 生命周期字段。
- 不改变 `terminal_done_uid`、pass/fail、redirect/replay 恢复和 L2TLB idle-stop 规则。
- 初始化阶段的 clamp 和合法性检查是低频路径，对仿真主循环无性能影响。

覆盖性结论：

本 plan 覆盖 V2 适配中的 compile/static width 基础 flow，作为 LSQ enqueue、split issue、int writeback 和 L2TLB 权限字段适配的前置参数来源。它不是业务 flow plan，不覆盖字段填充、raw event 归一化或 monitor 分类；这些已由其它 7 个执行 plan 覆盖。结论是：该 plan 只是 V2/V3 静态结构和参数细节适配，不影响测试框架主体逻辑。
