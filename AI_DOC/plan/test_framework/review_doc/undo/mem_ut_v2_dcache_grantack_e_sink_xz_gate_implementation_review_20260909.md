# V2 DCache GrantAck E.sink X/Z 门控实现 Review

## 1. 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 对应代码对象或落点 | 使用场景/示例 |
| --- | --- | --- | --- |
| `E.fire` | DUT 在 E 通道提交 GrantAck，且 responder 已给出 ready 的握手边界 | `e_fire`、`process_e_fire()` | E.valid 与 E.ready 同时为 1 时消费一个 Grant owner |
| `sink` | Grant 与后续 GrantAck 的关联编号 | `grant_ack_wait_q[i].sink`、DUT E.bits.sink | D 的 sink=0 应由后续 E 的已知 0 确认 |
| `GrantAck owner` | 已完成最后一个 D Grant beat、等待 E 确认的 line/sink 记录 | `grant_ack_wait_q` | 未知 E.sink 到来时 owner 必须继续保留 |
| 四态值 | 可表达 0/1/X/Z 的 SystemVerilog `logic` 值 | `raw_observed_sink` | `00x` 在检查前不能被当作 000 |
| 二态折叠 | 四态值赋给 `bit` 时 X/Z 被转换为 0 | `observed_sink` | hard 检查关闭时沿用旧兼容行为 |
| hard X/Z 检查 | 由既有 plusarg 控制的严格输出诊断 | `MEMBLOCK_HARD_XZ_CHECK_EN`、`get_hard_xz_check_en()` | 开启时未知 sink 报 `UVM_ERROR` 并停止本次清账 |
| `record_cached_line()` | 将完成 GrantAck 的 line 投影为 ACTIVE resident | `mem_base_sequence.sv` | 仅已知且匹配的 sink 才能调用 |

本 review 只覆盖 DCache responder 对 DUT E.sink 的观察和 GrantAck 账本保护；不覆盖 RTL、RM、
DCache interface 或其它 X/Z 检查点。

## 2. Review 范围与目标

关联 plan：
`AI_DOC/plan/test_framework/plan/do/mem_ut_v2_dcache_grantack_e_sink_xz_gate_plan_20260909.md`。

目标是修复四态 E.sink 被二态局部变量提前折叠后误删除 `grant_ack_wait_q` owner 的框架缺口，且保留
`MEMBLOCK_HARD_XZ_CHECK_EN=0` 时的既有行为。源码功能范围只有
`dcache_mem__access_base_sequence::process_e_fire()`。

## 3. 修改前后行为

### 3.1 修改前

E.fire 后直接把 DUT 的四态 E.sink 赋给 `bit [9:0] observed_sink`，因此含 X/Z 的值可能变成 0，
随后按 0 匹配并删除合法 owner。若 DUT 继续发送 E.valid，框架会在下一拍以空队列 fatal 收尾，掩盖
最初的非法 payload。

### 3.2 修改后

先以 `logic [9:0] raw_observed_sink` 保存原始值。只有 hard X/Z 开关开启且
`$isunknown(raw_observed_sink)` 为真时，报告 `UVM_ERROR` 并从函数返回；此路径不调用
`record_cached_line()`，也不删除 queue。开关关闭或值已知时，再执行原有 `logic -> bit` 转换、匹配和
owner 删除。

### 3.3 修改后的完整关键函数

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，
函数 `dcache_mem__access_base_sequence::process_e_fire()`（当前约 3410 行）。

抽象功能描述：该函数在已确认的 E.fire 边界消费一个 GrantAck payload，按已分配的 sink 完成唯一
Grant owner；它不负责生成 D response 或修复 DUT 输出。严格模式下，未知 sink 不得推进任何 owner
生命周期。

源码功能简析：输入是 `grant_ack_wait_q` 和 DCache interface 的 E.sink；输出是匹配成功时的 line
ACTIVE 投影及 owner 删除，或未知值时的错误报告与无状态变化。

```systemverilog
function void dcache_mem__access_base_sequence::process_e_fire();
    logic [9:0] raw_observed_sink;
    bit [9:0] observed_sink;

    if (grant_ack_wait_q.size() == 0) begin
        `uvm_fatal(get_type_name(), "unexpected E.valid when no GrantAck is pending")
    end
    // 中文注释：严格 X/Z 检查开启时，先保留 DUT E.sink 的四态值；未知 payload
    // 只能报告错误，不能被折叠成 0 后误消费 GrantAck owner。
    raw_observed_sink = dcache_vif.drv_cb.auto_inner_dcache_client_out_e_bits_sink;
    if (seq_csr_common::get_hard_xz_check_en() &&
        $isunknown(raw_observed_sink)) begin
        `uvm_error(get_type_name(), "GrantAck E.bits.sink sampled as X/Z on E.fire")
        return;
    end
    // hard X/Z 检查关闭时保留原有 logic 到 bit 的转换和匹配行为。
    observed_sink = raw_observed_sink;
    foreach (grant_ack_wait_q[i]) begin
        if (grant_ack_wait_q[i].sink == observed_sink) begin
            record_cached_line(grant_ack_wait_q[i].line_addr, grant_ack_wait_q[i].line_alias);
            grant_ack_wait_q.delete(i);
            return;
        end
    end
    `uvm_fatal(get_type_name(),
               $sformatf("GrantAck sink=%0d does not match any pending Grant owner", observed_sink))
endfunction:process_e_fire
```

中文伪代码：该逻辑先检查 GrantAck owner 是否存在；空队列仍立即报告协议 fatal。然后从 DUT 接口
读取四态 E.sink。若严格开关为 1 且值含 X/Z，就报告 `UVM_ERROR` 并返回，所以不改变 line 或
`grant_ack_wait_q`。其余情况把值转换成原有二态 `observed_sink`，逐项寻找相同 sink；找到时调用
`record_cached_line()` 将 line 标成 ACTIVE，再删除且只删除该 owner；遍历结束仍未命中则保持原有
sink mismatch fatal。`get_hard_xz_check_en()` 只读取既有运行期开关，`record_cached_line()` 只负责
完成已确认 line 的 resident 投影，二者都不改变 D/E 调度。

## 4. 调用关系与状态副作用

| 调用顺序 | 函数/对象 | 本流程中的职责 | 状态副作用 |
| --- | --- | --- | --- |
| 1 | `dcache_mem__access_base_sequence::body()` | 采样上一拍 item 并计算 E.fire | 仅在 E.fire 时调用本函数 |
| 2 | `process_d_fire()` | 在最后 Grant D.fire 建立 owner | 向 `grant_ack_wait_q` 入队 |
| 3 | `process_e_fire()` | 检查 payload、匹配 sink、结算 owner | 严格未知路径无副作用；已知匹配路径更新 line 并删除一项 |
| 4 | `record_cached_line()` | 建立已确认的 ACTIVE line 记录 | 更新 `cached_line_by_addr` |
| 5 | `build_current_d_xaction()` | 根据 owner 是否为空生成下一拍 E.ready | queue 保留时继续允许后续 E 处理 |

D.fire 与 E.fire 同一采样边界时，`body()` 先调用 `process_d_fire()`，因此新建立的 owner 可被随后
的 `process_e_fire()` 看到；本次修改没有改变这一顺序。严格未知分支返回后 owner 仍在队列中，后续
drain/audit 可以观察未闭环状态，而不是被静默清除。

## 5. 正确性检查

| hard X/Z 开关 | E.sink | 预期行为 | owner 结果 |
| --- | --- | --- | --- |
| 0 | 已知且匹配 | 原有二态匹配 | 删除匹配 owner |
| 0 | 含 X/Z | 原有二态折叠和匹配 | 与修改前一致 |
| 1 | 已知且匹配 | 通过 guard 后原有匹配 | 删除匹配 owner |
| 1 | 已知但不匹配 | 原有 fatal | 不删除 |
| 1 | 含 X/Z | `UVM_ERROR` 后返回 | 不调用 line 记账，不删除任何 owner |

关键边界检查：空队列 fatal 仍在 sink 采样前；未知分支的 `return` 位于
`record_cached_line()` 和 `grant_ack_wait_q.delete(i)` 之前；没有新增 queue 扫描，原有最多 16 项
的 `foreach` 复杂度不变。

## 6. 验证结果

### 6.1 静态和编译

- `git diff --check`：通过。
- V2 基础 compile：通过且无编译 error；日志包含既有 `Warning-[KUAI]` 和 LCA 使用提示，
  未发现由本 feature 新增的 warning。
- 关键 guard 顺序和开关条件由源码 review 独立核对通过。

### 6.2 基础仿真

`tc=basicTest, ts=virtual_base_sequence, cfg=default, seed=666666` 通过，`TEST_PASS`，
`UVM_ERROR=0`、`UVM_FATAL=0`。

日志：
`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/grantack_e_sink_base_20260909/log/tc=basicTest_ts=virtual_base_sequence_cfg=default_seed=666666_rtl.log`

### 6.3 关闭 hard X/Z 的历史复现

`seed=710006`、`+MEMBLOCK_HARD_XZ_CHECK_EN=0` 保持原有结果：
`745.6ns` 报 `E.valid observed without a pending GrantAck owner`，未出现新的
`GrantAck E.bits.sink sampled as X/Z on E.fire`。这是兼容性验证，不是通过判据。

日志：
`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/grantack_e_sink_xzoff_20260909/log/tc=basicTest_ts=memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq_cfg=tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_seed=710006_rtl.log`

### 6.4 严格模式未知 sink 复现限制

同一 seed 以 `+MEMBLOCK_HARD_XZ_CHECK_EN=1` 运行时，先在 `690.6ns`、`705.6ns` 和
`720.6ns` 出现其它既有 LDA2/ready X/Z 错误，并因 `tcnt_test_base::build_phase()` 固定的
10 个 quit count 在 `720.6ns` 结束，早于历史 E.sink X/Z 点。因此本轮没有动态捕获到新增的
`GrantAck E.bits.sink sampled as X/Z on E.fire` 日志；未知值不误删 owner 的结论来自完整源码路径、
静态检查和独立 review，不能把该 run 误写成未知 sink 的动态通过。

日志：
`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/grantack_e_sink_xzon_20260909/log/tc=basicTest_ts=memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq_cfg=tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_seed=710006_rtl.log`

### 6.5 严格模式已知 sink 正常路径

`tc=basicTest, ts=memblock_dispatch_real_smoke_vseq, cfg=tc_dispatch_real_l2cache_model,
seed=710001, partcmp_op=off, +MEMBLOCK_HARD_XZ_CHECK_EN=1` 通过，`TEST_PASS`，
`UVM_ERROR=0`、`UVM_FATAL=0`，DCache responder 发布 terminal idle。

日志：
`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/grantack_e_sink_known_20260909/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_dispatch_real_l2cache_model_seed=710001_rtl.log`

## 7. 与 Plan 对齐检查

### 7.1 实现与 Plan 不一致项

未发现实现与执行前正式 plan 不一致的源码行为。plan 第 2.2 节、第 4.2 节已经明确要求始终以四态
临时值采样，并在 `MEMBLOCK_HARD_XZ_CHECK_EN=1` 时才用 `$isunknown()` 拦截；当前实现逐项满足。

plan 中的 `IMPLEMENTATION_DELTA` 只记录 coding 期间补充的文档同步和验证限制，不引入额外源码语义。
下列源码片段用于确认该结论。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，函数
`dcache_mem__access_base_sequence::process_e_fire()`。

抽象功能描述：该函数消费已经发生的 E.fire，并只允许已知且匹配的 GrantAck payload 结算一个
Grant owner；严格模式下未知 sink 仅形成可见错误，不得推动 owner 或 cache-line 状态。

```systemverilog
raw_observed_sink = dcache_vif.drv_cb.auto_inner_dcache_client_out_e_bits_sink;
if (seq_csr_common::get_hard_xz_check_en() &&
    $isunknown(raw_observed_sink)) begin
    `uvm_error(get_type_name(), "GrantAck E.bits.sink sampled as X/Z on E.fire")
    return;
end
observed_sink = raw_observed_sink;
```

中文伪代码：本段先从 DUT 接口取得尚未二态化的 E.sink。既有 getter 读取严格 X/Z 开关；只有该开关
开启且采样值含 X/Z 时，函数报告 `UVM_ERROR` 后立即返回，因此不会继续匹配或删除 owner。其余情况才
把值赋给二态 `observed_sink`，从而继续原有的匹配流程。此片段本身不调用 cache-line 记账函数，也不
修改 D/E 调度。

### 7.2 Plan 未说明但 Coding 落实的细节

未发现其它 Plan 未说明但 Coding 额外落实的细节；局部四态变量、条件 guard、错误等级和提前返回
均已由 plan 主流程或 `IMPLEMENTATION_DELTA` 覆盖。上表所述采样边界不一致已单独列入
“实现与 Plan 不一致项”，不重复归入本节。

## 8. 文档同步与非本次修改的逻辑分析

本次同步了以下有效文档中的 E.sink 语义：

- `AI_DOC/mem_ut_flow_doc/dcache_l2_response_hint_probe_model_flow.md`
- `AI_DOC/mem_ut_flow_doc/dcache_sbuffer_memory_responder_flow.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/mem_base_sequence.md`
- `AI_DOC/analysis/framework_design/memblock_pbmt0_non_nc_10k_dcache_e_xprop_rtl_failure_20260907.md`

`git status --short` 中其它已有修改不属于本 review：`AGENTS.md`、RTL flow/index 文档、buglist
规则及 buglist 内容、旧 `sim/verdiLog` 配置，以及此前生成的 `sim/base_fun` 和本次仿真 mode
目录均为其它任务或生成产物。本 review 不改变、不暂存这些内容；它们需要各自的功能 review。

## 9. Review 结论

代码修改满足 plan：严格模式下未知 E.sink 不再参与 owner 清账，关闭模式保持旧语义；未修改 RTL、RM、
接口或参数。基础编译、基础仿真和严格已知 sink 路径通过；严格未知 sink 动态路径受既有其它 X/Z
错误及固定 quit count 限制，已在 plan 和本 review 中明确记录。未发现本次实现的已知 blocker。
