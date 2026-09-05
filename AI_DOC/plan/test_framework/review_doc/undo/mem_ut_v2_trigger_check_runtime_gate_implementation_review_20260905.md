# V2 输出 Trigger 检查运行期开关 Implementation Review

## 1. 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 对应代码对象或落点 | 使用场景/示例 |
| --- | --- | --- | --- |
| output monitor | 对 DUT 已经驱出的 writeback 接口进行采样和语义检查的 monitor 路径。 | int writeback monitor 到 `dispatch_monitor_event_adapter`。 | STA0 已输出 `trigger=0`、`exceptionVec[3]=0` 时检查来源是否合理。 |
| raw event | output monitor 采样到、尚未完成 UID/key 归一化的 writeback 记录。 | `dispatch_raw_int_wb_t raw`。 | LDA/STA/STD 的 `trigger_valid`、`trigger`、`exception_vec` 都来自该记录。 |
| capability check | 检查一个 output source/lane 是否带有当前 V2 profile 要求的字段能力。 | `check_raw_int_wb_capability()`。 | LDA/STA 应提供 trigger metadata，STD 应为 TriggerAction.None。 |
| trigger action | writeback uop 的 4-bit trigger 动作编码。 | `raw.trigger`。 | `4'hf` 是 None，`4'h0` 是 breakpoint exception 动作。 |
| provenance | 根据已绑定的主表 transaction、MMIO/NCIO/CBO 上下文判断输出编码的来源是否合理。 | `INT_WB_STA0_TRIGGER_PROVENANCE`。 | 普通 STA0 不应无 breakpoint exception 地输出 `trigger=0`。 |
| runtime snapshot | testcase build 期冻结的公共 plus 值，运行期 consumer 只读它而不重复解析命令行。 | `seq_csr_common::trigger_check_en`。 | output adapter 每个 raw event 读取同一个 testcase 配置。 |
| input scalar contract | 测试框架发往 DUT 的 LSQ enqueue item 必须满足的输入协议。 | `lsqenq_agent_agent_driver::validate_v2_scalar_item()`。 | active/inactive slot 的 `trigger` 均必须满足原有零值合同。 |

本 feature 的最终语义是：`+MEMBLOCK_CHECK_TRIGGER_EN=<0|1>` 只控制
**DUT 输出 monitor** 中的 trigger 语义检查，默认 `1`。为 `0` 时，不再从输出
`trigger_valid`、trigger action 或 STA0 provenance 报错；输出的 source/lane、key、
exception、replay、flush 和 CBO 检查仍生效。LSQ enqueue 输入 item 的 `trigger` 合同不读取
该参数，始终严格检查。

关联 plan：
`AI_DOC/plan/test_framework/plan/do/mem_ut_v2_trigger_check_runtime_gate_formal_plan_20260905.md`。

## 2. Review 范围与结论

### 2.1 覆盖范围

本 review 覆盖下列最终 feature 文件：

1. `mem_ut/ver/ut/memblock/env/plus.sv`：plus 字段、默认值和解析。
2. `mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv`：build 期 snapshot 与 getter。
3. `mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv`：output monitor 的 capability、metadata 和 provenance 门控。
4. `mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg`：默认开启。
5. 参数说明、正式 plan 和本 implementation review。

不属于最终 feature diff，但本 review 额外检查了
`lsqenq_agent_agent_driver::validate_v2_scalar_item()`：它仍无条件检查输入
`trigger != 0`，证明开关没有放宽 stimulus 侧合同。

### 2.2 最终结论

最终代码只有一条配置链：

```text
+MEMBLOCK_CHECK_TRIGGER_EN
  -> plus::MEMBLOCK_CHECK_TRIGGER_EN
  -> seq_csr_common::trigger_check_en
  -> dispatch_monitor_event_adapter
```

不存在 agent mirror，也不存在 LSQ input driver consumer。默认值为 `1` 时，原有输出
trigger 检查保持严格；值为 `0` 时，只跳过输出 monitor 的五类 trigger 专属 report。
最终远端编译和独立 coding review 结果在第 7、9 节记录。

## 3. 参数解析与冻结

### 3.1 `plus::reload_from_cmdline()`

抽象功能描述：该函数把 cfg/命令行的文本 plusarg 转换为公共静态字段；它只解析输入，不直接
驱动 DUT、LSQ input driver 或 output monitor。

修改前逻辑：没有 `MEMBLOCK_CHECK_TRIGGER_EN` 字段，因此输出 adapter 的 trigger 检查没有
运行期策略输入。

修改后逻辑：新增默认值为 `1'b1` 的 bit 字段，并随现有公共参数调用 `load_bit()`。

正确性检查：没有 plusarg 时保持严格默认值；cfg 或命令行给出 `0` 时，后续只能影响 snapshot
读取者，不能改变 DUT 输入 payload。

源码位置：`mem_ut/ver/ut/memblock/env/plus.sv`，字段定义与
`plus::reload_from_cmdline()`。

```systemverilog
// 中文注释：DUT scalar writeback 输出的 trigger metadata 诊断开关。默认开启以保持既有严格检查；
// 关闭时只跳过输出 trigger capability/action/provenance 报错，不改变输入 item 或 DUT trigger 行为。
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_CHECK_TRIGGER_EN, bit, 1'b1)

load_bit("MEMBLOCK_CHECK_TRIGGER_EN", MEMBLOCK_CHECK_TRIGGER_EN);
```
中文伪代码：该逻辑先为参数建立严格默认值 1。testcase 初始化时，`load_bit()` 从 runtime
plusarg 查找同名字段；若存在则覆盖默认值，若不存在则保留 1。它只写 `plus` 层输入字段，
不会调用 driver、不会改写 transaction，也不会直接触发或抑制 monitor report。

### 3.2 `seq_csr_common::load_from_plus()` 与 `get_trigger_check_en()`

抽象功能描述：`load_from_plus()` 在 testcase build 期冻结已解析的公共参数；
`get_trigger_check_en()` 向 output adapter 返回该冻结值。两者都不创建 agent mirror，
也不参与 DUT 输入 item 的校验。

修改前逻辑：没有 trigger snapshot，adapter 无条件执行输出 trigger 诊断。

修改后逻辑：新增静态 `trigger_check_en`，在 `load_from_plus()` 赋值；getter 先沿用
`check_initialized()` 确认 snapshot 已建立，再返回该 bit。

正确性检查：配置只在初始化期写入一次；高频 output monitor 路径只读取 getter，不重复解析
命令行，也不会与 input driver 产生配置竞争。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv`，
字段、`load_from_plus()` 与 `get_trigger_check_en()`。

```systemverilog
// 中文注释：DUT scalar writeback 输出的 trigger metadata 诊断快照。默认开启以保持既有严格行为；
// writeback adapter 通过 getter 读取，不影响 LSQ enqueue 输入 item 的结构合同。
static bit trigger_check_en = 1'b1;

trigger_check_en = plus::MEMBLOCK_CHECK_TRIGGER_EN;

static function bit get_trigger_check_en();
    check_initialized("get_trigger_check_en");
    return trigger_check_en;
endfunction:get_trigger_check_en
```
中文伪代码：build 期先把 plus 层的最终值复制到 `trigger_check_en`。output adapter 请求该值时，
getter 先调用 `check_initialized()`，其职责是拒绝在 testcase 参数尚未冻结前使用配置；
检查成功后返回 bit。代码没有调用 `memblock_sync_pkg`，因此不会向 agent 发布另一个配置副本。

调用关系：

| 顺序 | 调用者/对象 | 当前流程职责 |
| --- | --- | --- |
| 1 | testcase build phase | 调用 `seq_csr_common::reload_from_plus()`。 |
| 2 | `plus::reload_from_cmdline()` | 解析 `+MEMBLOCK_CHECK_TRIGGER_EN`。 |
| 3 | `seq_csr_common::load_from_plus()` | 冻结为 `trigger_check_en`。 |
| 4 | output adapter | 通过 getter 决定是否执行输出 trigger 专属检查。 |

## 4. 输出 Monitor 门控

### 4.1 `check_raw_int_wb_capability()`

抽象功能描述：该函数在 raw writeback event 绑定 UID 前验证 output source/lane、key、replay、
flush、trigger capability 和 exception mask。它决定 event 是否以 `INT_WB_CAP` fail-fast，
但不负责 metadata action 解码或 UID 生命周期推进。

修改前逻辑：LDA/STA 缺失 `trigger_valid`，或 STD 有 `trigger_valid` / 非 None action，
与其它 capability 错误一起无条件触发 `INT_WB_CAP`。

修改后逻辑：只把这些 trigger 子条件包进 `trigger_check_en`；source/lane、ROB/LQ/SQ、
state lookup、replay、flush、exception mask 仍在 guard 外。

正确性检查：关闭开关不会接受错误 source/lane 或不支持的 exception。例如 STD 关闭后仍要求
value-only ROB、无 LQ/SQ、无 replay/flush 且 `exception_vec==0`。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv`，
`check_raw_int_wb_capability()`。

```systemverilog
trigger_check_en = seq_csr_common::get_trigger_check_en();
case (raw.source_kind)
    memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_SCALAR_LDA: begin
        if (raw.port_id > 2 || !raw.rob_valid || raw.rob_value_only_without_flag ||
            raw.lq_valid || raw.sq_valid || !raw.key_needs_state_lookup ||
            !raw.replay_inst_valid || !raw.flush_pipe_valid ||
            (trigger_check_en && !raw.trigger_valid)) begin
            `uvm_fatal("INT_WB_CAP", "invalid SCALAR_LDA raw capability")
        end
    end
    memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_STA: begin
        if (raw.port_id > 1 || !raw.rob_valid || raw.rob_value_only_without_flag ||
            raw.lq_valid || raw.sq_valid || !raw.key_needs_state_lookup ||
            raw.replay_inst_valid || (trigger_check_en && !raw.trigger_valid) ||
            raw.flush_pipe_valid != (raw.port_id == 0) ||
            raw.replay_inst || (!raw.flush_pipe_valid && raw.flush_pipe)) begin
            `uvm_fatal("INT_WB_CAP", "invalid STA raw capability")
        end
    end
    memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_STD: begin
        if (raw.port_id > 1 || raw.rob_valid || !raw.rob_value_only_without_flag ||
            raw.lq_valid || raw.sq_valid || !raw.key_needs_state_lookup ||
            raw.replay_inst_valid || raw.flush_pipe_valid ||
            (trigger_check_en && (raw.trigger_valid || raw.trigger != 4'hf)) ||
            raw.replay_inst || raw.flush_pipe || raw.exception_vec != 24'b0) begin
            `uvm_fatal("INT_WB_CAP", "invalid STD value-only raw capability")
        end
    end
endcase
if ((raw.exception_vec & ~allowed_exception_mask) != 24'b0) begin
    `uvm_fatal("INT_WB_CAP", "raw exceptionVec contains unsupported bits")
end
```
中文伪代码：函数先从冻结 snapshot 取得开关。LDA、STA、STD 分支仍逐项检查所有非 trigger
能力；只有开关为 1 时才把各自的 trigger-valid/action 条件纳入 fatal。case 分支结束后，
无论开关值如何，都根据 `allowed_exception_mask` 检查 exception 位图。这意味着 `0` 仅
跳过输出 trigger capability，不会让错误 key、replay、flush 或 exception 静默通过。

### 4.2 `check_raw_int_wb_metadata()`

抽象功能描述：该函数解释 output raw event 的 trigger metadata 编码。它始终先验证 LDA 的
replay/flush invariant；随后才检查 None、breakpoint、unsupported 和 unknown action。

修改前逻辑：所有上述 action 编码检查无条件执行。

修改后逻辑：LDA 的非 trigger invariant 完成后，开关为 `0` 时早退；为 `1` 时保留原有
action 检查及 report ID。

正确性检查：早退位于 LDA replay/flush 检查之后，因此关闭 trigger 诊断不会屏蔽
`INT_WB_SCALAR_LDA_REPLAY_INST_INVARIANT` 或
`INT_WB_SCALAR_LDA_FLUSH_PIPE_INVARIANT`。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv`，
`check_raw_int_wb_metadata()`。

```systemverilog
if (raw.source_kind == memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_SCALAR_LDA) begin
    if (raw.replay_inst) begin
        `uvm_fatal("INT_WB_SCALAR_LDA_REPLAY_INST_INVARIANT",
                   "SCALAR_LDA replayInst must be zero in current V2 profile")
    end
    if (raw.flush_pipe) begin
        `uvm_fatal("INT_WB_SCALAR_LDA_FLUSH_PIPE_INVARIANT",
                   "SCALAR_LDA flushPipe must be zero in current V2 profile")
    end
end
if (!seq_csr_common::get_trigger_check_en()) begin
    return;
end
if (!raw.trigger_valid) begin
    if (raw.trigger != 4'hf) begin
        `uvm_fatal("INT_WB_METADATA", "absent trigger metadata must keep TriggerAction.None")
    end
    return;
end
case (raw.trigger)
    4'hf: ;
    4'h0: begin
        if (!raw.exception_vec[3] &&
            !(raw.source_kind == memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_STA &&
              raw.port_id == 0)) begin
            `uvm_fatal("INT_WB_TRIGGER", "BreakpointExp trigger requires exceptionVec[breakPoint]")
        end
    end
    4'h1, 4'h2, 4'h3, 4'h4:
        `uvm_fatal("INT_WB_TRIGGER_UNSUPPORTED",
                   $sformatf("unsupported trigger action=0x%0h", raw.trigger))
    default:
        `uvm_fatal("INT_WB_TRIGGER", $sformatf("unknown trigger action=0x%0h", raw.trigger))
endcase
```
中文伪代码：LDA output 先执行 replay 和 flush 的既有 profile 不变量检查，触发时立即报错。
随后读取开关；为 0 则只结束本 metadata helper，调用者仍继续 UID attach、key normalization
和 batch handler。为 1 时，函数继续检查 metadata 缺失时是否为 None、breakpoint action
是否带 exceptionVec[3]，以及不支持/未知 action 是否报原有 ID。

### 4.3 `check_attached_int_wb_metadata()`

抽象功能描述：该函数在 output writeback 已绑定 UID 后，结合 main transaction 验证 STA0
`flush_pipe` 和 trigger provenance。它不改变 raw event、也不比较 RM。

修改前逻辑：普通 STA0 的 `trigger=0 && !exceptionVec[3]` 无条件做 CBO/MMIO/NCIO
来源判断。

修改后逻辑：`flush_pipe` 的 CBO 检查保持无条件，只给 provenance 判定增加开关。

正确性检查：关闭开关时，STA0 event 仍进入既有 key normalization 和 batch handler；若
`flush_pipe=1`，仍会报告非 CBO 或 CBO consumer 未实现。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv`，
`check_attached_int_wb_metadata()`。

```systemverilog
if (raw.source_kind == memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_STA && raw.port_id == 0) begin
    main_tr = data.get_main_transaction(wb_event.uid);
    if (raw.flush_pipe) begin
        if (main_tr.op_class != MEMBLOCK_OP_CLASS_CBO) begin
            `uvm_fatal("INT_WB_STA0_FLUSH_PIPE",
                       "STA0 flushPipe is only legal for CBO/CMO producer")
        end
        `uvm_fatal("INT_WB_STA0_CBO_UNSUPPORTED",
                   "STA0 CBO flushAfter has no current adapter consumer")
    end
    if (seq_csr_common::get_trigger_check_en() &&
        raw.trigger == 4'h0 && !raw.exception_vec[3] &&
        main_tr.op_class != MEMBLOCK_OP_CLASS_CBO &&
        !raw.debug_is_mmio && !raw.debug_is_ncio) begin
        `uvm_fatal("INT_WB_STA0_TRIGGER_PROVENANCE",
                   "STA0 trigger=0 without breakpoint needs uncache/CBO provenance")
    end
end
```
中文伪代码：函数只处理 STA0 output，并首先通过 UID 取得 main transaction。若有
`flush_pipe`，无论开关值都执行 CBO producer 和 CBO consumer 的既有 fatal。接着只有开关为
1 时，才把普通来源的 `trigger=0`、无 breakpoint exception 组合报告为 provenance 错误；
开关为 0 时跳过该 report，但不修改 transaction 或后续生命周期。

## 5. 输入 Contract 不受开关影响

抽象功能描述：`validate_v2_scalar_item()` 是 testbench 向 DUT 驱动 LSQ enqueue item 前的
输入合法性检查。它不是 output monitor，不读取 `MEMBLOCK_CHECK_TRIGGER_EN`。

范围纠正前的风险：若输入 driver 跟随此开关，关闭一个输出诊断会允许非法 stimulus 中的
nonzero trigger 通过，混淆 DUT 输出缺陷与 testbench 输入错误。

最终逻辑：driver 保持原有无条件 `trigger != '0` 检查；不新增 mirror、setter 或 getter。

正确性检查：静态搜索确认 driver 中没有 `trigger_check_en`、`is_trigger_check_en()` 或
`MEMBLOCK_CHECK_TRIGGER_EN` consumer。

源码位置：`mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_driver.sv`，
`validate_v2_scalar_item()`。

```systemverilog
if (!valid) begin
    if (need_alloc != 2'b00 || fu_type != '0 || uop_idx != '0 ||
        rob_idx_flag || rob_idx_value != '0 ||
        lq_idx_flag || lq_idx_value != '0 ||
        sq_idx_flag || sq_idx_value != '0 || num_ls_elem != '0 ||
        exception_vec != '0 || trigger != '0 || fu_op_type != '0 ||
        flush_pipe || last_uop) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("inactive slot=%0d must have zero qualifier and payload", slot))
    end
end
if (uop_idx != 0 || num_ls_elem != 1 || !last_uop ||
    exception_vec != '0 || trigger != '0 || flush_pipe) begin
    `uvm_fatal(get_type_name(),
               $sformatf("slot=%0d violates scalar LSQ contract: trigger=0x%0h", slot, trigger))
end
```
中文伪代码：inactive input slot 中任何非零 trigger 都与其它非零 payload 一样直接 fatal；
active scalar input slot 中非零 trigger 也无条件 fatal。这里没有读取 runtime snapshot，因此
`MEMBLOCK_CHECK_TRIGGER_EN=0` 不会改变驱动进 DUT 的 stimulus 合法性。

## 6. CFG 与参数文档同步

`seq/plus_cfg/default.cfg` 明确写入：

```text
+MEMBLOCK_CHECK_TRIGGER_EN=1
```

它与 `plus.sv` 的默认值一致。参数说明同步到：

1. `AI_DOC/project_management/mem_ut_parameter_management.md`：明确参数仅控制 output monitor。
2. `mem_ut/ver/ut/memblock/rule/memblock_parameter_management_rule.md`：保留公共诊断参数的通用边界。
3. `mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md`：将索引名称明确为 scalar writeback 输出 trigger metadata 诊断。

## 7. 验证记录

### 7.1 静态验证

1. `git diff --check` 覆盖本 feature 代码与文档。
2. `rg` 确认参数链仅存在于 `plus.sv`、`seq_csr_common.sv`、default cfg 和
   `dispatch_monitor_event_adapter.sv`。
3. `rg` 确认最终代码不存在 `set_trigger_check_en`、`is_trigger_check_en`、
   `memblock_sync_pkg::trigger_check_en` 或 LSQ driver consumer。
4. 人工复查 exception mask、LDA replay/flush、STA0 flush/CBO 仍不受开关影响。

### 7.2 远端验证

最终 output-only 代码使用两个独立 VCS mode 编译，均完成 compile、elaboration、link 和
Verdi KDB 生成。KDB 生成日志报告 `0 error(s), 0 warning(s)`；VCS 的既有 warning 不归因为本 feature。

| mode | 运行参数 | 结果 | 结论 |
| --- | --- | --- | --- |
| `trigger_gate_output_only_20260905` | `seed=666666`、`MEMBLOCK_MAIN_TRANS_NUM=1000`、`MEMBLOCK_CHECK_TRIGGER_EN=1` | `727.8ns` 报 `UVM_FATAL [INT_WB_STA0_TRIGGER_PROVENANCE]`。 | 默认值仍严格检出已知 STA0 metadata 缺陷。 |
| `trigger_gate_output_off_20260905` | 同一 seed/场景，仅 `MEMBLOCK_CHECK_TRIGGER_EN=0`。 | 无 `INT_WB_STA0_TRIGGER_PROVENANCE`、`INT_WB_TRIGGER`、`INT_WB_METADATA` 或 trigger 专属 `INT_WB_CAP`；运行到 `2606.3ns`。 | gate 已允许该输出事件继续沿既有生命周期处理。 |

关闭态最终因十个 `RM_LS_COMPARE` 到达 UVM quit count 而结束。这些错误均为 PMA/PMP
期望异常与 DUT status exception 不一致，首个发生于 `736.3ns`，属于本 feature 范围之外；
它们没有被归因为 trigger gate，也不影响“关闭后不再报 trigger 专属诊断”的验收。

日志位置：

```text
mem_ut/ver/ut/memblock/sim/trigger_gate_output_only_20260905/log/
mem_ut/ver/ut/memblock/sim/trigger_gate_output_off_20260905/log/
```

## 8. Plan 对齐检查

### 8.1 与最终 Plan 的对照

| 最终 Plan 项 | 当前实现 | 结论 |
| --- | --- | --- |
| plus 定义、默认值和解析 | `plus.sv` 已实现。 | 一致。 |
| build 期 snapshot 与 getter | `seq_csr_common.sv` 已实现。 | 一致。 |
| output adapter capability/metadata/provenance 门控 | 三个 helper 已实现。 | 一致。 |
| input LSQ contract 不受开关影响 | driver 保持无条件 `trigger != 0` 检查。 | 一致。 |
| default cfg 与参数说明 | 已同步。 | 一致。 |

### 8.2 实现与执行前原始 Plan 的差异

执行前原始 plan 曾把 agent mirror 和 LSQ input driver 的两个 trigger 条件纳入同一开关。用户
最终明确限定为 output monitor，因此该部分在 plan 的
`IMPLEMENTATION_DELTA: 最终 scope 收敛为输出 monitor` 中被删除。当前代码严格遵从该
最终 scope，不构成遗漏。

### 8.3 Plan 未说明但 Coding 落实的细节

未新增未记录的运行期行为。参数索引同步、远端单 mode 验证组织和 output-only scope 收敛均已
记录在 plan 的 `IMPLEMENTATION_DELTA`。

## 9. 独立 Coding Review

独立 subagent 已对最终 diff 和两组定向日志完成只读复审，结论为通过、没有 blocking finding。
复审确认：

1. `plus.sv` 默认值和 `default.cfg` 都为 `1`，并经 `seq_csr_common` 只读快照传入 adapter。
2. `dispatch_monitor_event_adapter` 是唯一 runtime consumer；没有 agent mirror、LSQ enqueue
   driver、CSR trigger 配置或 DUT 连线 consumer。
3. 开关为 `0` 时，仅排除 `trigger_valid`、trigger action 和 STA0 provenance 的 report 条件；
   source/key/exception mask、LDA replay/flush、STA0 flush/CBO、UID attach 和 key normalization
   都仍在 guard 外。
4. `lsqenq_agent_agent_driver::validate_v2_scalar_item()` 的输入 `trigger != 0` 检查保持无条件。
5. `git diff --check` 对本 feature 源码和参数文件无格式错误。

剩余非阻塞覆盖缺口：真实 DUT 定向场景覆盖了 STA0 provenance 分支；
`INT_WB_CAP` 的 trigger-valid、`INT_WB_METADATA` 和 unsupported action 分支当前由静态控制流
review 覆盖，未另行伪造 raw output event。新增 adapter unit/directed raw-event testcase 可作为
后续 coverage 增强，不阻塞本次提交。

## 10. 非本次修改的逻辑分析

当前 worktree 含有用户已有的大量修改，本 review 不把它们纳入 feature 正确性或提交范围：

| 类别 | 文件/目录示例 | 判断 | 原因 |
| --- | --- | --- | --- |
| RTL/接口知识文档 | `AI_DOC/analysis/rtl/v2/**`、`AI_DOC/analysis/interface/v2/**` | 非本次逻辑 | 属于既有 V2 知识库维护。 |
| 其他 dispatch/RM 改动 | `memblock_dispatch_base_sequence.sv`、`seq_pkg.sv`、`tc_pkg.sv` | 另行 review | 属于 boundary、PBMT/RM、发射或 testcase 工作。 |
| 混合参数文档 hunk | `mem_ut_parameter_management.md`、`plus_demo_migration_plan.md` | 局部纳入 | 仅本 review 标明的 trigger 行属于本 feature。 |
| 仿真产物 | `sim/.eda_remote/`、`fsdb*Log/`、`verdiLog/`、`xcelium.d` | 非源码 review | 运行生成物，不进入 commit。 |

## 11. 当前交付状态

代码、正式 plan 和参数文档已收敛到 output-only 语义；当前源码已完成独立编译、定向正反向
验证和 subagent coding review。plan 将归档到 `do`，本地提交只纳入本 feature 关联 hunk。
