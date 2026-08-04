# DCache/Uncache `main_mem` 范围开关与共享覆盖层实现评审

关联执行 Plan：
`AI_DOC/plan/test_framework/plan/do/mem_ut_dcache_main_mem_range_switch_plan_20260730.md`

评审范围：本次只检查该 Plan 对 DCache/Uncache memory responder、公共 runtime 参数、
real-smoke virtual sequence 及其直接关联文档的实现。并发修改的
`mem_ut_v2_l2tlb_response_random_payload_plan_20260729.md` 不属于本专项，未纳入评审和提交。

## 1. 术语与抽象功能说明

| 英文术语 | 本文中的中文含义 | 代码对象或落点 | 典型场景 |
|---|---|---|---|
| `backing memory` | 只保存确定性懒初始化原始字节的基础稀疏内存，不被 DUT 写覆盖 | `mem_access_base_sequence::main_mem` | overlay 未覆盖的 load byte 回退到它 |
| `write overlay` | 按 byte-valid 保存已确认 DUT 写的覆盖层 | `write_overlay_mem`、`write_overlay_byte_valid` | 一个 8B Uncache partial store 只覆盖其中有效 byte |
| `write batch` | 某个采样边界内已真实握手、但尚未对下一边界读可见的写事件集合 | `dcache_write_batch`、`uncache_write_batch` | 同拍 DCache C writeback 与 Uncache store 的提交顺序固定 |
| `sample boundary` | responder 在 `drv_cb` 观察上一拍 handshake 的时钟边界 | `begin_shared_mem_sample($time)` | 当前边界提交上一边界 write batch，再接收本拍新写 |
| `lifecycle owner` | 每个 testcase 唯一负责清空和配置 shared memory store 的场景入口 | `initialize_shared_memory_store()` | 在 fork DCache/Uncache responder 前完成初始化 |
| `armed snapshot` | 看到 valid 后保存、下一采样边界确认 fire 的完整请求副本 | `armed_a_req_xact` | 避免连续 Uncache A 请求把下一笔 payload 误当成已 fire 请求 |
| `fired snapshot` | 当前 `drv_cb` 已确认 valid/ready 同时为 1 后重新复制的请求副本 | `fired_a_req_xact`、`fired_c_req_xact` | C data 写回只使用已检查的 fire payload |
| `merged read view` | overlay 有效 byte 优先、其余 byte 从 backing 获得的读视图 | `shared_mem_access_task()` | GrantData/AccessAckData 在 A.fire 时固化该数据 |

## 2. 评审结论

本专项已将 DCache coherent port 和历史名为 `sbuffer_agent` 的 Uncache TL-UL port 迁移到同一份
testcase 级 shared memory store。默认严格范围模式和关闭后的 48-bit 稀疏地址模式均通过
`MEMBLOCK_MAIN_MEM_RANGES_EN` 统一控制。DUT 的 memory-facing 写不再污染 backing；DCache C data
和 Uncache store 只在真实 fire 后进入 overlay，并在下一采样边界以确定性顺序可见。

独立终审结论将在本文第 10 节回填；在回填前，本 agent 已完成逐项源码、参数、文档和日志复核。

## 3. 参数链路与范围语义

### 3.1 `MEMBLOCK_MAIN_MEM_RANGES_EN`

修改前：`MEMBLOCK_PADDR_BASE/RANGE` 只有 TLB PPN 构造的明确语义，DCache 和 Uncache 的 sparse
memory 范围边界没有统一 runtime 开关，且 Uncache 可能绕过 DCache 的 range 状态。

修改后：新增公共行为参数，默认值为 `1`。它只决定 shared memory store 是否把
`PADDR_BASE/RANGE` 注册为访问窗口，不改变 TLB PPN 构造、主表虚拟地址生成或任何 DUT 结构参数。

源码位置：`mem_ut/ver/ut/memblock/env/plus.sv`、
`mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv`。

抽象功能描述：`plus` 解析 raw plus 值，`seq_csr_common` 保存 testcase runtime snapshot 并对
memory lifecycle owner 提供只读 getter；两者不直接参与 DCache/Uncache 逐拍读写。

```systemverilog
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_MAIN_MEM_RANGES_EN, bit, 1'b1)

main_mem_ranges_en = plus::MEMBLOCK_MAIN_MEM_RANGES_EN;

static function bit get_main_mem_ranges_en();
    check_initialized("get_main_mem_ranges_en");
    return main_mem_ranges_en;
endfunction:get_main_mem_ranges_en
```

中文伪代码：`plus` 先以 `1` 作为默认值读取 `MEMBLOCK_MAIN_MEM_RANGES_EN`；
`seq_csr_common::load_from_plus()` 将最终值写入 runtime snapshot；memory lifecycle owner 调用
getter 时先确认公共参数已初始化，再取得该开关。getter 只返回配置，不修改 range、backing、overlay
或 responder 状态。

同步项：`seq/plus_cfg/default.cfg`、
`AI_DOC/project_management/mem_ut_parameter_management.md` 和
`mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md` 已说明该参数的 consumer 边界。

## 4. 共享 memory store 与确定性提交

### 4.1 初始化、范围配置和生命周期

修改前：`main_mem`、`prog_mem`、byte valid 和 range 状态是每个 sequence 实例私有对象；两个
memory-facing responder 不能可靠地观察彼此写入。

修改后：backing、overlay、range、write batch 和 lifecycle flag 全部为
`mem_access_base_sequence` 的 static test-level 状态。只有场景 owner 在 responder fork 前清空；
legacy default-sequence topology 仅在尚未初始化时做一次兜底，不能再次清空已有 store。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，
`initialize_shared_memory_state()`、`begin_shared_mem_sample()`。

抽象功能描述：初始化 helper 建立 testcase 起始内存视图；sample helper 在每个 responder 时钟边界
推进 committed write view。二者不处理 TileLink request、response delay 或 DCache alias 状态。

```systemverilog
function void mem_access_base_sequence::initialize_shared_memory_state(
    input bit ranges_en, input mem_addr_t base, input longint unsigned capacity
);
    clear_shared_memory_state();
    if (ranges_en) begin
        init_main_mem_range(base, capacity);
    end
    shared_mem_lifecycle_initialized = 1'b1;
endfunction

function void mem_access_base_sequence::begin_shared_mem_sample(input longint unsigned sample_time);
    if (!shared_mem_sample_valid) begin
        shared_mem_sample_valid = 1'b1;
        shared_mem_sample_time  = sample_time;
        return;
    end
    if (sample_time != shared_mem_sample_time) begin
        commit_shared_mem_write_batch();
        shared_mem_sample_time = sample_time;
    end
endfunction
```

中文伪代码：初始化时先删除旧 testcase 的 backing、overlay、有效掩码、两类写 batch 和 range；
开关为 `1` 时注册 `base..base+capacity-1`，为 `0` 时保持 range 未配置；随后置 lifecycle flag。
每个 responder 的 `drv_cb` 顶部调用 sample helper；首个边界只建立基准时间，后续时间前进时先提交上一拍
所有 DCache 写，再提交所有 Uncache 写，最后记录当前边界。相同 `$time` 的第二个调用不重复提交，因此
两个 sequence 的 delta 执行先后不会改变读写可见性。

正确性检查：write batch 在 DCache 和 Uncache 两个主循环顶部均推进，最后一笔写即使后续没有新的 memory
request，下一时钟边界也会提交，不会永久滞留。

### 4.2 backing 读取、overlay 合并和真实写入边界

修改前：`main_mem_access_task()` 可直接写 backing；`prog_mem` 是另一份私有 write overlay，无法跨
responder 合并。

修改后：`main_mem_access_task()` 只做 backing 读、错误/范围预检和懒初始化；
`shared_mem_access_task()` 统一提供 merged read，并把真正 memory-facing store 排入来源 batch。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，
`main_mem_access_task()`、`shared_mem_access_task()`、`commit_shared_mem_write_batch()`。

抽象功能描述：公共 helper 为 DCache/Uncache wrapper 提供同一份数据视图。它负责 shared-store 数据层，
不决定请求何时 fire，也不决定 D channel response 的 opcode 或 delay。

```systemverilog
begin_shared_mem_sample($time);
main_mem_access_task(addr, 1'b0, byte_mask, '0, main_corrupt, main_denied, main_load_data);

if (is_store) begin
    case (write_owner)
        SHARED_MEM_WRITE_DCACHE:  dcache_write_batch.push_back(write_event);
        SHARED_MEM_WRITE_UNCACHE: uncache_write_batch.push_back(write_event);
    endcase
end
else if (write_overlay_byte_valid.exists(line_addr) &&
         write_overlay_byte_valid[line_addr][byte_offset]) begin
    load_data[(i * 8) +: 8] = write_overlay_mem[line_addr][(byte_offset * 8) +: 8];
end
else begin
    load_data[(i * 8) +: 8] = main_load_data[(i * 8) +: 8];
end
```

中文伪代码：先固定当前 sample 的 committed view，再逐有效 byte 完成 backing 的范围和错误检查；若是读，
overlay valid byte 返回 overlay 数据，否则返回 backing 懒初始化数据；若是写，先保证预检成功，再按写来源
加入 DCache 或 Uncache batch，绝不直接改写 backing。未知写来源立即 fatal，避免没有归属的 DUT 写被静默接受。

写入来源和时机：完整 `ReleaseData` 与完整 `ProbeAckData` 均只有所有 data beat `corrupt=0` 时才由
DCache C assembly 收齐后调用 DCache owner wrapper；任一 beat corrupt 时不写 overlay、但仍结束对应协议
assembly。Uncache 只有 A.fire 的 fired snapshot 调用 Uncache owner wrapper。store issue、dirty 标志、
C.valid 未握手、无 data 的 ProbeAck 均不会写 overlay。

## 5. DCache 与 Uncache responder 接入

### 5.1 DCache C data 和 GrantData

修改前：DCache wrapper 使用实例私有 `prog_mem`，C data 会直接修改该私有层。

修改后：`dcache_mem_access_task()` 按 32B beat 访问 shared helper。GrantData 在 `AcquireBlock A.fire`
时读取 merged view 并保存在 pending D record；C data 完整收齐后才进入 DCache batch。响应延迟期间不再
重新读取 live overlay。

抽象功能描述：DCache wrapper 只把 coherent 32B beat 翻译为 shared memory 的 1KB line/byte mask 表示；
它不拥有跨通道提交顺序。

```systemverilog
shared_mem_access_task(beat_addr, is_store, line_mask, line_store_data,
                       corrupt, denied, line_load_data,
                       is_store ? SHARED_MEM_WRITE_DCACHE : SHARED_MEM_WRITE_NONE);
```

中文伪代码：DCache wrapper 先把 32B mask/data 放入公共 line 宽度容器；读时指定无写 owner，返回 merged
32B 数据给 GrantData snapshot；写时指定 DCache owner，只创建本 sample 的 DCache batch event。实际 overlay
更新延后到下一 sample，因而不会因 C/D sequence 的执行顺序产生不同结果。

### 5.2 Uncache A.fire snapshot 和单笔 D response

原计划未展开 Uncache A payload 的采样域。执行中发现旧实现用 `drv_cb` 判定 valid，却从裸 interface
读取 payload；连续请求在 clock edge 后更新时可能把下一笔 request 当成已 fire request。因此这部分为
`IMPLEMENTATION_DELTA`，保留原来的单笔串行控制，不引入 response queue/outstanding 机制。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，
`capture_sbuffer_a_xaction()`、`sbuffer_mem_access_base_sequence::body()`。

抽象功能描述：Uncache responder 在上一拍驱动的 A.ready/D.valid 与当前 `drv_cb` snapshot 的 valid/ready
组成真实 handshake，fire 后才建立 response 和 shared-store 写。它不负责 DCache coherence owner 或 alias。

```systemverilog
if ($isunknown({sbuffer_vif.drv_cb.auto_inner_buffers_out_a_bits_opcode,
                sbuffer_vif.drv_cb.auto_inner_buffers_out_a_bits_param,
                sbuffer_vif.drv_cb.auto_inner_buffers_out_a_bits_size,
                sbuffer_vif.drv_cb.auto_inner_buffers_out_a_bits_source,
                sbuffer_vif.drv_cb.auto_inner_buffers_out_a_bits_address,
                sbuffer_vif.drv_cb.auto_inner_buffers_out_a_bits_mask,
                sbuffer_vif.drv_cb.auto_inner_buffers_out_a_bits_data,
                sbuffer_vif.drv_cb.auto_inner_buffers_out_a_bits_corrupt})) begin
    `uvm_fatal(get_type_name(), "Uncache A payload sampled as X/Z outside reset")
end
req_xact.auto_inner_buffers_out_a_bits_address =
    sbuffer_vif.drv_cb.auto_inner_buffers_out_a_bits_address;

if (a_accept_armed && sampled_a_valid) begin
    capture_sbuffer_a_xaction(fired_a_req_xact);
    check_sbuffer_a_payload_stable(armed_a_req_xact, fired_a_req_xact);
    sbuffer_mem_access_xaction(fired_a_req_xact, rsp_xact);
    pending_d_xact  = rsp_xact;
    pending_d_valid = 1'b1;
end
```

中文伪代码：复制到二态 xaction 前，先检查 `drv_cb` 的 opcode、param、size、source、address、mask、data
和 corrupt 没有 X/Z；任一未知值立即 fatal，不能被静默转换为 0。armed 阶段保存的 A snapshot 和确认 fire
时的 snapshot 都从同一 `drv_cb` 域获得；若 valid 保持期间 payload 变化则 fatal。确认 A.fire 后才用 fired
snapshot 构造 AccessAck/AccessAckData，并由 store opcode 将数据排入 Uncache batch。非 reset 时 A.valid 或
D.ready 为 X/Z 也直接 fatal。pending D 完成 D.fire 前保持 A.ready 为 0，维持本专项既有单笔串行语义。

### 5.3 Uncache global stop 的 fail-fast 边界

第三轮独立 review 发现，旧 stop 路径只在 `A.valid=0` 时退出，但在 stop 后禁止重新 arm 新 A。若 DUT
此时保持一笔尚未 fire 的新 A.valid，responder 会一直输出 A.ready=0，双方永久等待。

修改后在已 armed A 与 pending D 的上一拍 fire 已处理完毕后执行：

```systemverilog
if (!reset_active && data.is_global_stop_requested() && sampled_a_valid && !a_fire) begin
    `uvm_fatal(get_type_name(),
               "new Uncache A.valid observed after global stop without a sampled fire")
end
```

中文伪代码：stop 后，前拍已经给过 A.ready 且本拍形成 A.fire 的请求仍可建立 D response、进入正常
drain；没有 A.fire 的当前 A.valid 是新请求，测试框架没有为它建立 owner，因此立即 fatal。这样不会阻断
合法 inflight drain，也不会以永久 A.ready=0 掩盖 terminal 卡死。

### 5.4 Uncache driver 的 lockstep 输出合同

第二轮独立 review 发现：旧 `sbuffer_agent_agent_driver` 在 `try_next_item()` 后额外等待一次 `drv_cb`
才写 output，而 sequence 在下一 `drv_cb` 已根据 `last_cycle_xact` 计算 fire。这会把尚未对 DUT 生效的
A.ready/D.valid 提前当作真实握手，造成 store 提前写 overlay 或 D response 提前撤销。

修改前：driver 在无 item 时每拍 drive idle，获得 item 后先等一个 clocking 边界再 `send_pkt()`；这与
Uncache responder 的 next-sample fire 模型不一致。

修改后：driver 采用 DCache 已验证的 lockstep 模式，阻塞等待一条 item 后立即驱动；本专项 responder
不支持 packet gap，因此把非零 gap 明确 fail-fast。

源码位置：`mem_ut/ver/ut/memblock/agent/sbuffer_agent_agent/src/sbuffer_agent_agent_driver.sv`，
`main_phase()`。

抽象功能描述：该 driver 只把 sequence item 放到 clocking output，保证 item 的可见周期；它不拥有
Uncache A/D handshake、memory write 或 response 生命周期。

```systemverilog
req = null;
seq_item_port.get_next_item(req);
if (req == null) begin
    `uvm_fatal(get_type_name(), "get_next_item returned a null Uncache item")
end
if (req.pre_pkt_gap != 0 || req.post_pkt_gap != 0) begin
    `uvm_fatal(get_type_name(), "Uncache responder item must use pre_pkt_gap=0 and post_pkt_gap=0")
end
this.send_pkt(req);
seq_item_port.item_done();
```

中文伪代码：driver 每轮先清空旧句柄，阻塞取得一条新 item；空 item 或 gap 非零均代表 responder
lockstep 合同不成立，立即 fatal。合法 item 不额外等待 `drv_cb`，直接写入 output 并完成 item；下一
sequence sample 因而能以已在 DUT 侧保持一拍的 A.ready/D.valid 计算 fire。该变更不增加 outstanding、
response delay 或其他协议状态。

### 5.5 DCache C payload 的四态保护与 confirmed snapshot

修改前：DCache C interface 为四态 `logic`，但 C xaction 字段为二态 `bit`。若 data 或 corrupt 含
X/Z，直接复制会折叠为 0；尤其 `corrupt=X/Z` 可能被后续 `c_assembly_corrupt_seen` 误认为没有 corrupt，
从而错误写入 overlay。C.fire 分支还会建立 fired snapshot，却将 assembly 传入 armed snapshot。

修改后：`check_dcache_c_payload_known()` 在每次 `capture_dcache_c_xaction()` 前运行。它对所有 C opcode
检查 opcode/param/size/source/address；只对 `ProbeAckData` 和 `ReleaseData` 检查 data/corrupt。无数据
`ProbeAck`/`Release` 的 data/corrupt 不属于当前 responder 的消费语义，因此允许 don't-care。C.fire 后先
对 armed/fired 做稳定性比较，再把 fired snapshot 传给 `start_c_assembly()` 或 `consume_c_beat()`。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，
`check_dcache_c_payload_known()`、`capture_dcache_c_xaction()`、
`dcache_mem__access_base_sequence::body()`。

抽象功能描述：该保护只保证 C data 写回使用真实且已知的 payload；它不改变 C-channel ready 时序、
两拍 assembly、corrupt=1 的“只收敛不写 overlay”行为，或 Probe/Release owner。

```text
准备接受 C request：
  检查 header 已知；data opcode 再检查 data/corrupt 已知；
  复制 armed snapshot，并在下一 item 驱动 C.ready；

确认 C.fire：
  再复制并检查 fired snapshot；
  armed 与 fired 不一致 -> fatal；
  fired snapshot 进入 assembly；
  两个 beat 全部 corrupt=0 -> 写 overlay batch；
  其余情况 -> 只收敛当前协议。
```

## 6. real-smoke 场景生命周期

修改前：DCache/SBuffer 自己在 `body()` 开始时初始化私有 memory；派生 cancel-reconcile vseq 覆盖父类
`body()` 后没有统一 shared-store 清空点。

修改后：普通 real-smoke 和 cancel-reconcile 都在 background responder fork 前调用同一个父类 helper。
responder 自身仅在 legacy topology 尚未初始化时做一次兜底，不在 reset、Probe、CBO、stop 或退出时清空。

源码位置：
`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_vseq.sv`，
`initialize_shared_memory_store()`；
`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_cancel_reconcile_vseq.sv`，`body()`。

抽象功能描述：vseq helper 是 real-smoke topology 的唯一正常 lifecycle owner。它只在 testcase 开始
配置 static store，不参与 core dispatch、redirect 或 responder 的逐拍握手。

```systemverilog
function void memblock_dispatch_real_smoke_vseq::initialize_shared_memory_store();
    mem_access_base_sequence::initialize_shared_memory_state(
        seq_csr_common::get_main_mem_ranges_en(),
        mem_access_base_sequence::mem_addr_t'(seq_csr_common::get_paddr_base()),
        seq_csr_common::get_paddr_range()
    );
endfunction

memblock_sync_pkg::dispatch_real_smoke_active = 1'b1;
initialize_shared_memory_store();
fork
    start_background_responders();
join_none
```

中文伪代码：vseq 先初始化公共参数并置 active；读取 range 开关、PADDR base/range，清空前一 testcase
留下的 static memory state，再配置当前 testcase 的 shared store；初始化完成后才 fork DCache/Uncache
responder。cancel-reconcile 因为覆盖了父类 `body()`，同样在自己的 fork 前调用该 helper，避免复用旧 static
内容。background responder 的 global-stop 自然退出和 core dispatch 的 terminal 行为均未改动。

## 7. 文档同步检查

已同步以下当前有效文档：

| 文档 | 本轮同步内容 |
|---|---|
| `AI_DOC/mem_ut_flow_doc/dcache_sbuffer_memory_responder_flow.md` | shared backing/overlay、batch 提交、DCache C fired snapshot、Uncache A.fire snapshot、范围开关和自然退出边界 |
| `AI_DOC/mem_ut_flow_doc/dcache_l2_response_hint_probe_model_flow.md` | DCache C data 改写 overlay batch、C data X/Z 边界、shared range lifecycle 与 range 开关参数语义 |
| `AI_DOC/analysis/source_sv/dispatch_framework_sv/mem_base_sequence.md` | static store、DCache C 四态保护、DCache/Uncache wrapper、legacy fallback 和数据可见性 |
| `AI_DOC/mem_ut_flow_doc/virtual_sequence_unified_dispatch_flow.md` | normal/cancel vseq 的 owner 初始化位置与后台 responder 关系 |
| `AI_DOC/project_management/mem_ut_parameter_management.md` | 新参数与 MAIN_VADDR/PADDR 的职责分离 |
| `mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md` | 参数分组、默认语义和正式 getter 路径 |

历史 review/plan 中仍保留旧 `prog_mem` 或 responder 私有范围描述的内容，属于历史实现记录；后续行为以本
专项 Plan、本文及上述当前 flow/analysis 文档为准。

## 8. 验证与静态检查

已完成：

```bash
git diff --check

make eda_compile tc=basicTest ts=memblock_dispatch_real_smoke_vseq \
  mode=base_fun cfg=tc_dispatch_real_smoke

make eda_batch_run tc=basicTest ts=memblock_dispatch_real_smoke_vseq \
  mode=base_fun cfg=tc_dispatch_real_smoke \
  plus_arg='+MEMBLOCK_MAIN_MEM_RANGES_EN=0' note=main_mem_ranges_off

make eda_compile tc=basicTest ts=memblock_dispatch_real_cancel_reconcile_vseq \
  mode=base_fun cfg=tc_dispatch_real_cancel_reconcile_smoke

make eda_batch_run tc=basicTest ts=memblock_dispatch_real_cancel_reconcile_vseq \
  mode=base_fun cfg=tc_dispatch_real_cancel_reconcile_smoke \
  note=main_mem_lifecycle_cancel

make eda_compile tc=basicTest ts=memblock_dispatch_real_smoke_vseq \
  mode=base_fun cfg=tc_dispatch_real_smoke

make eda_batch_run tc=basicTest ts=memblock_dispatch_real_smoke_vseq \
  mode=base_fun cfg=tc_dispatch_real_smoke \
  plus_arg='+MEMBLOCK_MAIN_MEM_RANGES_EN=0' note=main_mem_ranges_off_xz_guard

make eda_compile tc=basicTest ts=memblock_dispatch_real_smoke_vseq \
  mode=base_fun cfg=tc_dispatch_real_smoke

make eda_batch_run tc=basicTest ts=memblock_dispatch_real_smoke_vseq \
  mode=base_fun cfg=tc_dispatch_real_smoke \
  plus_arg='+MEMBLOCK_MAIN_MEM_RANGES_EN=0' note=main_mem_ranges_off_stop_guard

make eda_compile tc=basicTest ts=memblock_dispatch_real_smoke_vseq \
  mode=base_fun cfg=tc_dispatch_real_smoke

make eda_batch_run tc=basicTest ts=memblock_dispatch_real_smoke_vseq \
  mode=base_fun cfg=tc_dispatch_real_smoke \
  plus_arg='+MEMBLOCK_MAIN_MEM_RANGES_EN=0' note=main_mem_ranges_off_dcache_c_xz_guard

make eda_batch_run tc=basicTest ts=memblock_dispatch_real_smoke_vseq \
  mode=base_fun cfg=tc_dispatch_real_smoke \
  note=main_mem_ranges_on_dcache_c_xz_guard
```

中文伪代码：先检查本专项 diff 没有空白错误；随后编译 normal real-smoke 和 cancel-reconcile 场景；
normal 场景显式关闭严格 range，验证 48-bit sparse 路径；cancel 场景验证派生 vseq 在 fork 前重新初始化
shared store。首轮两条 run、Uncache payload X/Z fail-fast 复跑和 stop fail-fast 修正后的复跑均出现
`TEST_PASS`，并报告 `UVM_ERROR=0`、`UVM_FATAL=0`。本轮 DCache C payload guard/fired snapshot 修正后
重新编译并分别复跑关闭和默认开启 strict range 的 normal real-smoke，结果同样通过：

- `mem_ut/ver/ut/memblock/sim/base_fun/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_dispatch_real_smoke_seed=666666_rtl_main_mem_ranges_off.log`
- `mem_ut/ver/ut/memblock/sim/base_fun/log/tc=basicTest_ts=memblock_dispatch_real_cancel_reconcile_vseq_cfg=tc_dispatch_real_cancel_reconcile_smoke_seed=666666_rtl_main_mem_lifecycle_cancel.log`
- `mem_ut/ver/ut/memblock/sim/base_fun/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_dispatch_real_smoke_seed=666666_rtl_main_mem_ranges_off_xz_guard.log`
- `mem_ut/ver/ut/memblock/sim/base_fun/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_dispatch_real_smoke_seed=666666_rtl_main_mem_ranges_off_stop_guard.log`
- `mem_ut/ver/ut/memblock/sim/base_fun/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_dispatch_real_smoke_seed=666666_rtl_main_mem_ranges_off_dcache_c_xz_guard.log`
- `mem_ut/ver/ut/memblock/sim/base_fun/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_dispatch_real_smoke_seed=666666_rtl_main_mem_ranges_on_dcache_c_xz_guard.log`

还执行了旧对象扫描：源码中没有 `prog_mem`、`prog_mem_byte_valid` 或 DUT store 直接写入
`main_mem` 的残留 consumer；`main_mem` 唯一写入点是懒初始化 helper。

## 9. Plan 对齐检查

### 9.1 与执行前 Plan 一致的实现

| Plan 要求 | 当前实现 |
|---|---|
| 默认严格 range、关闭后完整物理地址懒分配 | `MEMBLOCK_MAIN_MEM_RANGES_EN=1` 默认，未配置 range 时 `is_main_mem_access_in_range()` 只保留地址位宽溢出保护 |
| DCache/Uncache 使用同一 backing/overlay | 两个 wrapper 都调用 `shared_mem_access_task()` |
| DUT 写不污染 backing | `main_mem_access_task()` 对 store fail-fast；实际写只排入 overlay batch |
| DCache C data 和 Uncache store 按真实 fire 更新 | ReleaseData/ProbeAckData assembly 只消费 C.fire 的 fired snapshot；收齐且所有 data beat非 corrupt才调用 DCache store；Uncache 仅 A.fire 后调用 store wrapper |
| 同拍读写确定性 | 下一 sample 按 DCache 后 Uncache 的顺序提交；读只取上一 committed view |
| vseq 在 fork 前清空 shared store | normal 和 cancel-reconcile 均调用 `initialize_shared_memory_store()` |
| Uncache A payload 不静默折叠 X/Z | `capture_sbuffer_a_xaction()` 在复制到二态 xaction 前对全部 payload `$isunknown()` fail-fast |
| Uncache fire 对应真实已驱动 item | SBuffer driver 改为 `get_next_item -> send_pkt -> item_done`，拒绝 gap，和 sequence `last_cycle_xact` 时序一致 |
| global stop 后的 Uncache terminal 边界 | 已 fire A 正常 drain；`global_stop_requested && sampled_a_valid && !a_fire` 立即 fatal，避免新请求被永久 backpressure |
| DCache C payload 不静默折叠 X/Z | C header 总是 fail-fast；data C opcode 的 data/corrupt 也 fail-fast；C assembly 只消费 fired snapshot |

### 9.2 实现与 Plan 不一致项

未发现需要回改的实现与 Plan 不一致项。

### 9.3 Plan 未说明但 Coding 落实的细节

| 补充项 | 原因与当前行为 |
|---|---|
| 每个 responder 的 `drv_cb` 顶部推进 batch | 避免最后一笔已 fire 写在后续无 request 时滞留；已写入 Plan 的 `IMPLEMENTATION_DELTA` 第 1 项 |
| legacy default-sequence 初始化兜底 | 维持非 real-smoke 旧拓扑可运行，同时以 static flag 防止两个 responder 重复清空；已写入 `IMPLEMENTATION_DELTA` 第 2 项 |
| Uncache `drv_cb` payload snapshot 与四态 fatal | 防止连续 A request 错配和 X/Z 被静默忽略；已写入 `IMPLEMENTATION_DELTA` 第 4 项 |
| cancel-reconcile 派生 vseq 初始化 | 派生类覆盖父类 `body()`，必须显式调用相同 owner helper；已写入 `IMPLEMENTATION_DELTA` 第 5 项 |
| Uncache driver lockstep 输出 | 修正 `last_cycle_xact` 提前确认 fire 的时序错误；已写入 `IMPLEMENTATION_DELTA` 第 6 项 |
| global stop 后新 Uncache A fail-fast | 已 fire 请求仍正常 drain；未 fire 新 A.valid 立即报错，避免 terminal 卡死；已写入 `IMPLEMENTATION_DELTA` 第 7 项 |
| DCache C 的 X/Z 防折叠和 fired snapshot 消费 | C.fire 重新采样并完成 X/Z/stability 检查后，assembly 只消费 `fired_c_req_xact`；避免 `corrupt=X/Z` 被二态 xaction 变成 0；无数据 C opcode 保留 data/corrupt don't-care；已写入 `IMPLEMENTATION_DELTA` 第 8 项 |
| 移除无 consumer 的 `prog_mem` 和 batch timestamp | `prog_mem` 没有有效 consumer；write event 时间由统一 sample state 管理，逐 event timestamp 不参与任何判断，删除可避免第二份无效真源 |

## 10. 独立终审与剩余验证缺口

独立终审：第 3 轮发现 Uncache global-stop 死等、corrupt/data_valid 文档矛盾和 backing line 宽度错误，均已修正；
第 4 轮发现 DCache C xaction 的四态 data/corrupt 可能被二态字段静默折叠，本次已补齐 C header/data 的
fail-fast 与 fired snapshot 消费。第 5 轮独立只读 review 已对 `check_dcache_c_payload_known()`、C.fire
assembly、plan 和 review 做最终检查，结论为 `FINAL PASS`：没有尚存 blocker；C.fire 使用 confirmed
`fired_c_req_xact`，无数据 C opcode 不误查 don't-care data/corrupt，编译、默认 strict range smoke 和
range-off smoke 均通过。

当前没有已知功能 blocker。仍未建立专用 directed 验证的边界如下，不影响本专项完成：

- 连续 direct Uncache A request 的 payload 稳定性/串行 response 专项用例；
- global stop 后新 Uncache A.valid 的 expected-fatal 定向用例；
- 同一 sample 内 DCache C writeback 与 Uncache store 写同一 byte 的定向可见性用例；
- `corrupt=1` 的两拍 `ReleaseData`/`ProbeAckData` 不写 overlay、但协议状态仍收敛的 directed 用例；
- data C opcode 的 header/data/corrupt X/Z expected-fatal directed 用例；
- 后续 response delay/outstanding、D error injection、alias、多 Probe、L2 flush 和 CBO Probe 专项。

这些属于后续已拆分 DCache/Uncache Plan 的验证或功能范围，本专项不扩展 response queue、dynamic sink、
Probe directory 或 reference model。
