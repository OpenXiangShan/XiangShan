# V2 DCache GrantAck E.sink X/Z 严格检查门控方案

**状态**：已归档

**版本**：V2，`mem_ut_uvm_v2`

**方案范围**：仅修改 DCache responder 对 DUT 输出 E 通道 `sink` 的严格 X/Z 检查与
`grant_ack_wait_q` 记账保护；不修改 DUT RTL、RM、DCache agent interface、plus 参数定义或默认值。

## 1. 专有名词与抽象功能说明

### 1.1 专有名词

| 名词 | 当前含义 | 代码落点 | 本方案中的例子 |
| --- | --- | --- | --- |
| `D.fire` | DCache TileLink D 通道的 `valid && ready` 握手。对 Grant/GrantData 而言，最后一个 beat 的握手表示 responder 已经完成 Grant 输出。 | `dcache_mem__access_base_sequence::process_d_fire()` | 最后一个 `GrantData D.fire` 将其 `sink=0` 的 owner 放入等待队列。 |
| `E.fire` | DUT 在 E 通道提交 GrantAck 的 `valid && ready` 握手。 | `dcache_mem__access_base_sequence::process_e_fire()` | DUT 输出 `E.valid=1` 且 responder 上一拍已给出 `E.ready=1`。 |
| `sink` | Grant 与其 GrantAck 的关联标识。框架分配的 D Grant `sink` 是已知二态值；DUT 返回的 E `sink` 是四态 DUT 输出。 | `grant_ack_wait_q[i].sink`、`dcache_vif.drv_cb.auto_inner_dcache_client_out_e_bits_sink` | 已知 owner 的 `sink=0` 不能被 DUT 输出 `00x` 当作合法确认。 |
| `grant_ack_wait_q` | 保存“已经完成最后一个 D Grant beat、尚未收到合法 E GrantAck”的 owner 队列。每个记录包含 line、alias 与 sink。 | `mem_base_sequence.sv` 中的 `dcache_grant_ack_record_t` | 第五笔 Grant 的 `sink=0` 在 E 返回 `00x` 前仍应保留在队列中。 |
| 四态值 | `logic` 可表达 `0/1/X/Z`；`bit` 只能表达 `0/1`。四态赋给二态时，`X/Z` 会转换为 `0`。 | DCache interface 输入与 `process_e_fire()` 局部变量 | `10'b00x` 赋给 `bit [9:0]` 后会变成 `10'b000`。 |
| `MEMBLOCK_HARD_XZ_CHECK_EN` | 已有的运行期硬 X/Z 检查 plus 参数。通过 `seq_csr_common::get_hard_xz_check_en()` 读取。 | `seq_csr_common`、`process_e_fire()` | 值为 1 时，将 E.sink 的 X/Z 视为不可参与 owner 匹配的输出违规；值为 0 时保留当前行为。 |

### 1.2 关键函数的抽象功能描述

`dcache_mem__access_base_sequence::process_e_fire()` 在 DCache responder 已判定 E 通道发生握手后，
以 DUT 返回的 `E.bits.sink` 查找唯一的 Grant owner，并将对应 cache line 从 Grant-wait 状态转为
已确认状态。它只处理 E GrantAck 的 owner 结算，不负责生成 D Grant、分配 sink 或修复 DUT 输出。

`dcache_mem__access_base_sequence::process_d_fire()` 在最后一个 Grant/GrantData D beat 被 DUT 接收后，
建立一个待 E 确认的 owner。该函数保持现状，不属于本方案的修改范围。

## 2. 问题、目标与明确边界

### 2.1 已观察到的问题

在严格 PBMT=00/non-NC 的 V2 用例中，D 通道已经依次完成五笔 GrantData，其 sink 为
`0、1、0、1、0`；E 通道随后返回 `0、1、0、1、00x`。现有
`process_e_fire()` 将四态 E 输入直接赋给二态局部变量：

```systemverilog
bit [9:0] observed_sink;
observed_sink = dcache_vif.drv_cb.auto_inner_dcache_client_out_e_bits_sink;
```

因此最后一笔 `00x` 在比较前被折叠为 `000`，错误匹配并删除第五笔 `sink=0` 的 owner。下一拍
DUT 仍保持 `E.valid=1`，而 queue 已空，环境才报出次生的
`E.valid observed without a pending GrantAck owner`。

`E.bits.sink=00x` 本身仍是 DUT 输出的 RTL X 传播问题；本方案不改变该责任归属，只避免严格 X/Z
检查开启时测试框架把未知值伪装成一次合法 owner 完成。

### 2.2 最终目标

1. 复用现有 `MEMBLOCK_HARD_XZ_CHECK_EN`，不新增任何参数、cfg key、mirror 或 runtime snapshot。
2. 始终先把 E.sink 采样到四态临时值；仅当 `get_hard_xz_check_en()==1` 时，在 owner 匹配前用
   `$isunknown()` 检查该值。
3. 严格检查开启且 E.sink 含 X/Z 时，报现有语义的 `UVM_ERROR`，立即结束本次
   `process_e_fire()`；不得调用 `record_cached_line()`，不得删除 `grant_ack_wait_q` 中的任何 owner。
4. 严格检查关闭时，保留当前二态赋值、匹配、cache-line 记账和删除行为，不增加新的诊断或状态变化。
5. 已知且不匹配的 E.sink 仍走现有 `UVM_FATAL`；已知且匹配的 E.sink 仍只删除匹配的一个 owner。

### 2.3 开关行为矩阵

| `MEMBLOCK_HARD_XZ_CHECK_EN` | E.sink | 本次处理 | `grant_ack_wait_q` 结果 |
| --- | --- | --- | --- |
| 0 | 已知值 | 保持当前二态匹配和 owner 完成路径。 | 与当前实现一致。 |
| 0 | 含 X/Z | 保持当前路径：四态输入赋给 `bit` 后再比较，不添加 X/Z 保护或新报错。 | 与当前实现一致，包括现有的二态折叠行为。 |
| 1 | 已知且匹配 | 通过四态临时值后转换为二态 sink，按当前路径结算。 | 仅删除匹配 owner。 |
| 1 | 已知但不匹配 | 保持现有 `GrantAck sink=... does not match any pending Grant owner` fatal。 | 不删除任何 owner。 |
| 1 | 含 X/Z | `UVM_ERROR` 后从 `process_e_fire()` 返回。 | 不调用 cache-line 完成，也不删除任何 owner。 |

严格路径中，E 握手已在 DUT 接口层发生，测试框架不能事后撤销该握手；保留 owner 的目的是不把非法
payload 伪装成正常确认。后续若 DUT 撤销 E.valid 而 owner 仍未闭环，现有 stop-drain/audit 的残留检查
应如实反映这一未完成状态，不允许为了让测试结束而清空 queue。

## 3. 目标功能 Flow

```text
E.fire
  -> 检查 grant_ack_wait_q 非空（保持现有 fatal）
  -> 从 E 接口采样 logic[9:0] raw_observed_sink
  -> hard X/Z 检查是否开启？
       -> 否：按当前方式转换为 bit[9:0] 并匹配
       -> 是且 raw_observed_sink 含 X/Z：UVM_ERROR，返回，不改 queue/cache-line
       -> 是且为已知值：转换为 bit[9:0] 并匹配
  -> 匹配 owner：record_cached_line()，delete 对应 owner
  -> 未匹配：保持现有 UVM_FATAL
```

该 flow 没有扫描主表、没有增加 queue/map、没有改变 D 通道入队顺序；保留的 `foreach` 仍只扫描当前
最多 16 个 outstanding Grant owner 的既有队列。

## 4. `process_e_fire()` 修改 Flow

### 4.1 修改位置与职责

**源码位置**：
`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，
`dcache_mem__access_base_sequence::process_e_fire()`（当前约 3410 行）。

抽象功能描述：该函数在一个已确认的 E.fire 边界消费 DUT 的 GrantAck payload；新逻辑只在严格 X/Z
检查开启时拒绝未知 sink 参与 owner 生命周期推进，已知 sink 和开关关闭的原有路径不变。

函数目的：避免四态 DUT 输出在赋给二态局部变量后失去 X/Z 信息，从而错误完成另一个合法 Grant owner。

输入：已有 `grant_ack_wait_q`、`dcache_vif.drv_cb.auto_inner_dcache_client_out_e_bits_sink` 和
`seq_csr_common::get_hard_xz_check_en()`。

输出/副作用：已知匹配路径仍更新 cached-line 生命周期并删除一个 owner；严格 X/Z 路径只报告
`UVM_ERROR`，不修改 cached-line 或 `grant_ack_wait_q`。

### 4.2 源码级伪代码

```systemverilog
function void process_e_fire();
    logic [9:0] raw_observed_sink;
    bit   [9:0] observed_sink;

    if (grant_ack_wait_q.size() == 0)
        UVM_FATAL("unexpected E.valid when no GrantAck is pending");

    raw_observed_sink = dcache_vif.drv_cb.auto_inner_dcache_client_out_e_bits_sink;

    if (seq_csr_common::get_hard_xz_check_en() &&
        $isunknown(raw_observed_sink)) begin
        UVM_ERROR("GrantAck E.bits.sink sampled as X/Z on E.fire");
        return;
    end

    // hard X/Z 检查关闭时，此赋值保留当前 X/Z 到 bit 的既有折叠语义。
    observed_sink = raw_observed_sink;
    foreach (grant_ack_wait_q[i]) begin
        if (grant_ack_wait_q[i].sink == observed_sink) begin
            record_cached_line(grant_ack_wait_q[i].line_addr,
                               grant_ack_wait_q[i].line_alias);
            grant_ack_wait_q.delete(i);
            return;
        end
    end
    UVM_FATAL("GrantAck sink does not match any pending Grant owner");
endfunction
```

### 4.3 中文文字伪代码

函数先保留现有的空队列 fatal：如果没有任何 Grant owner 却收到 E.fire，不能因为 payload 是否 X/Z
改变该协议错误的优先级。随后把接口中的 E.sink 读入 `logic [9:0] raw_observed_sink`；这个临时值是
四态的，因此尚未丢失 X/Z。

接着只在已有 hard-X/Z 开关为 1 时检查该四态值。若任一 bit 为 X 或 Z，则报告 `UVM_ERROR` 并立即
返回。返回前不调用 `record_cached_line()`，所以对应 line 不会被提前标记为 GrantAck 完成；也不执行
`grant_ack_wait_q.delete(i)`，所以 pending owner 保持可审计。这里不新增“补偿性删除”或“按零匹配”的
分支。

若开关为 0，函数不会执行 `$isunknown()` 的保护分支，随后仍将该值赋给原有 `bit [9:0] observed_sink`。
因此已知值和含 X/Z 值的处理都与修改前相同，满足关闭 hard-X/Z 检查时保持当前检查方式的要求。

若严格检查开启但 raw sink 已知，赋给 `bit` 不会改变数值；函数继续复用现有 `foreach`。匹配时只结算
找到的一个 owner 并立即返回；没有匹配时仍走当前 fatal。D Grant 的分配、入队、sink 复用判断、E.ready
生成和 global-stop drain 条件均不在此函数中改变。

## 5. 不修改的范围与状态约束

1. 不修改 `MEMBLOCK_HARD_XZ_CHECK_EN` 的定义、默认值、解析路径或 `seq_csr_common` getter。
2. 不新增独立的 `GrantAck` X/Z plusarg；本行为必须完全绑定既有 hard-X/Z 开关。
3. 不把 `dcache_grant_ack_record_t.sink` 从 `bit` 改为 `logic`。该字段由 responder 分配并应始终是
   已知值；需要保留四态的是 DUT 返回的 E sink 观察值。
4. 不修改 `process_d_fire()` 的最后 beat 入队语义，不改变 GrantData 的两拍处理，也不改变
   `allocate_grant_sink()` 的复用规则。
5. 不修改 DCache agent monitor、driver、interface、TileLink 接线、DUT RTL 或 RM。
6. 不将 strict-path `UVM_ERROR` 升级为 `UVM_FATAL`，并且不因该错误清空 queue、伪造 E 完成或
   绕过现有 drain/audit 检查。

## 6. 修改文件、实施顺序与验证

### 6.1 修改文件

源码功能范围仅修改：

```text
mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv
```

实施时只在 `process_e_fire()` 内增加四态临时变量和 hard-X/Z guard；不新增 filelist、cfg、参数、
transaction 字段或外部接口。

执行 plan 时还必须按测试框架执行规则同步当前有效的 flow/analysis 文档，并生成 implementation
review；这些文档同步不改变源码功能范围。

### 6.2 静态验收

1. `rg` 确认 E.sink 的 `$isunknown()` guard 位于 `observed_sink = raw_observed_sink` 之前。
2. 确认 guard 的两个条件同时存在：`get_hard_xz_check_en()` 与 `$isunknown(raw_observed_sink)`。
3. 确认 guard 分支中的 `return` 在 `record_cached_line()` 和 `grant_ack_wait_q.delete(i)` 之前。
4. 确认已知 mismatch 的原有 `UVM_FATAL`、空 queue fatal 和 D.fire 入队路径没有被开关包住。
5. 执行 `git diff --check`，并由独立 review 逐项核对本 plan 的开关矩阵。

### 6.3 定向仿真验收

使用已有复现输入：

```text
tc=basicTest
ts=memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq
cfg=tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k
seed=710006
```

关联的既有波形与日志：

```text
/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/pbmt0_non_nc_10k_multiseed_20260906/wave/tc=basicTest_ts=memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq_cfg=tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_seed=710006_rtl_xzoff_e_diag_20260907.fsdb
/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/pbmt0_non_nc_10k_multiseed_20260906/log/tc=basicTest_ts=memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq_cfg=tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_seed=710006_rtl_xzoff_e_diag_20260907.log
```

验收分为三项：

1. **关闭态回归**：使用 `+MEMBLOCK_HARD_XZ_CHECK_EN=0`。日志中不得新增
   `GrantAck E.bits.sink sampled as X/Z on E.fire`；在已知复现点，行为必须保持当前二态折叠和后续
   报错形态，不把本方案作为关闭 X/Z 检查后的修复。
2. **开启态未知 sink**：使用 `+MEMBLOCK_HARD_XZ_CHECK_EN=1`。在原 `E.sink=00x` 的 E.fire 点，
   首先出现 `UVM_ERROR`；波形和代码审查须确认该 E.fire 后 responder 没有执行 owner 删除，下一轮
   输出仍按“queue 非空”路径保留 `E.ready`。本轮使用 `seed=710006` 时，仿真在 `720.6ns` 被
   其它既有 hard-X/Z 检查达到 `tcnt_test_base` 固定的 10 个 quit count，早于预期的 E.fire，
   因而该命令只能作为“未到达 E.sink 点”的限制性结果，不能替代未知 sink 的动态验收；不能把该
   早停误判为 gate 失败。
3. **开启态正常路径**：使用以下可复现的单笔真实 DCache GrantAck 场景：

   ```bash
   make eda_compile tc=basicTest \
     ts=memblock_dispatch_real_smoke_vseq \
     cfg=tc_dispatch_real_l2cache_model \
     mode=grantack_e_sink_known_20260909 partcmp_op=off \
     seed=710001 wave=null ccov=off fcov=off \
     plus_arg='+MEMBLOCK_HARD_XZ_CHECK_EN=1 +MEMBLOCK_CHECK_TRIGGER_EN=0'
   make eda_run tc=basicTest \
     ts=memblock_dispatch_real_smoke_vseq \
     cfg=tc_dispatch_real_l2cache_model \
     mode=grantack_e_sink_known_20260909 partcmp_op=off \
     seed=710001 wave=null ccov=off fcov=off \
     plus_arg='+MEMBLOCK_HARD_XZ_CHECK_EN=1 +MEMBLOCK_CHECK_TRIGGER_EN=0'
   ```

   该 run 完成 `TEST_PASS`，`UVM_ERROR=0`、`UVM_FATAL=0`，且 DCache responder 正常发布 terminal
   idle；已知 E.sink 的匹配和 owner 释放路径保持正常。

本方案的完成条件是：关闭态没有语义变化；开启态在未知 E.sink 时不再删除任何 owner，并以
`UVM_ERROR` 暴露原始 DUT 输出错误。它不以“掩盖 RTL X 后测试通过”为验收目标。

## 7. 执行中补充/修正（IMPLEMENTATION_DELTA）

### 7.1 四态采样与开关边界澄清

- 原 plan 表述容易被理解为“开关开启时才采样四态值”。实际实现始终先采样
  `logic [9:0] raw_observed_sink`，只有开关开启时才调用 `$isunknown()` 并提前返回。
- 关闭开关时仍执行 `logic -> bit` 转换，保持原有二态折叠；该澄清不增加状态、参数或接口。

### 7.2 执行规则要求的文档同步

- 同步 `AI_DOC/mem_ut_flow_doc/dcache_l2_response_hint_probe_model_flow.md`、
  `AI_DOC/mem_ut_flow_doc/dcache_sbuffer_memory_responder_flow.md` 和
  `AI_DOC/analysis/source_sv/dispatch_framework_sv/mem_base_sequence.md` 中的 E.sink 语义，
  避免继续描述为无条件 fatal。
- 更新历史诊断文档 `AI_DOC/analysis/framework_design/memblock_pbmt0_non_nc_10k_dcache_e_xprop_rtl_failure_20260907.md`，
  标明本 plan 已实现门控修复，但不改变 RTL X 传播责任归属。

### 7.3 验证限制记录

- 基础 `virtual_base_sequence` compile/run 通过。
- `seed=710006` 的关闭态复现保持原有 `745.6ns` owner-empty fatal；严格态在其它 hard-X/Z
  错误累计到固定 quit count 后于 `720.6ns` 结束，未到达 E.sink X/Z 点。
- 独立 known-sink real-smoke `seed=710001` 通过，作为严格模式正常路径验收；未知 sink 的
  不误删行为由源码、静态条件和独立 review 共同确认。该限制不改变完成条件，也不引入新的
  `UVM_MAX_QUIT_COUNT` 或其它参数。

## 与初步 plan 差异说明

**函数名**：`dcache_mem__access_base_sequence::process_e_fire()`。

**修改原因**：初步分析提出“未知 E.sink 无论 X/Z 检查开关如何设置都不得参与匹配”。根据最终需求，
该约束必须绑定既有 `MEMBLOCK_HARD_XZ_CHECK_EN`：诊断开启时阻止未知值清账，诊断关闭时保留历史
二态折叠行为。因此不能把未知 sink 的拒绝做成无条件分支。

**抽象功能描述**：该函数在一个已经发生的 E.fire 边界，根据 E.sink 结算唯一的 GrantAck owner。
修改后它在严格诊断模式中先验证 DUT 输出是否仍是已知值；只有 payload 可用时才推进 cache-line 和
owner 生命周期。函数不负责撤销已经发生的握手，不负责改变 D Grant 生成，也不负责修复 DUT 的 X 传播。

**修改前文字伪代码**：E.fire 后把接口 E.sink 直接赋给二态 `bit`，随后按折叠后的值匹配并可能删除
owner；未知值没有独立的生命周期保护。即使配置关闭 X/Z 诊断，或配置开启却只报告错误后继续执行，
未知 bit 仍可能在二态化后误匹配为 sink 0。

**修改后文字伪代码**：E.fire 先把 E.sink 保存到四态 `logic`。如果 hard X/Z 开关开启且值含 X/Z，
报告 `UVM_ERROR` 并立即返回，不更新 line 或 queue；否则转换为 `bit`，沿用原匹配、ACTIVE 投影和
单项删除路径；已知 mismatch 继续 fatal。开关关闭时不执行 `$isunknown()` 拦截，保留既有二态转换的
兼容语义。

**差异影响**：影响范围仅限 `process_e_fire()` 的局部采样和分支控制，以及执行规则要求的
flow/analysis 文档同步；不改变 RTL、RM、参数、接口、D.fire 入队或其它 responder 生命周期。DUT 在
有效 E GrantAck 上输出 `00x` 仍是需要单独修复的 RTL X 传播问题。
