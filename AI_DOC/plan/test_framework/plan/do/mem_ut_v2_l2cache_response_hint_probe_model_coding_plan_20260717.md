# mem_ut V2 轻量 L2Cache Response、Hint 与 Probe 建模专项 Coding Plan

| 项目 | 内容 |
|---|---|
| 状态 | `do`，coding、文档同步、compile/smoke 和最终独立 review 已完成并归档 |
| 目标版本 | V2 |
| 当前分支 | `mem_ut_uvm_v2` |
| DUT 接口权威 | `build_memblock/rtl/MemBlock.sv` |
| 测试框架入口 | `dcache_mem__access_base_sequence::body()` |
| 核心原则 | 在现有 DCache responder 内建立单 A 回复、单 Probe 在途的轻量 L2 模型，不复刻完整 L2 directory、slice、MSHR 或 CHI 流水线 |
| 创建日期 | 2026-07-17 |

> 历史实现说明（2026-08-04）：本文保留当时 DCache responder 的 `main_mem`/写回描述。当前有效实现
> 已由 `AI_DOC/plan/test_framework/plan/do/mem_ut_dcache_main_mem_range_switch_plan_20260730.md` 改为 DCache/Uncache 共用 static backing 与
> byte-valid write overlay；DCache C data 不再覆盖 backing，而是在下一 shared sample 提交 overlay。当前
> 行为以最新专项 plan、implementation review 和 flow 文档为准。

## 1. 目标、范围与替代关系

### 1.1 术语与抽象功能说明

| 英文术语 | 当前 plan 中的中文含义 | 代码对象或状态落点 | 典型场景 |
|---|---|---|---|
| `owner` | 某个在途协议生命周期的唯一状态维护者，同一资源不能被第二条路径覆盖 | `pending_d_valid`、`waiting_grant_ack`、`c_assembly_owner`、`waiting_probe_c` | `GrantData` 最后一拍完成后只由 GrantAck owner 等待 E |
| `pending D` | 已接受、已经分类但尚未完成全部 D beat 的唯一回复 | `pending_d_*` | `AcquireBlock` 建立两拍 `GrantData` |
| `armed snapshot` | 已保存 DUT valid/payload，并将在下一采样边界用上一拍 ready 确认 fire 的请求快照 | `a_accept_armed`、`c_accept_armed` | C 首拍 valid 被保存，下一拍 C.fire 后才进入 assembly |
| `C assembly` | 对 `ProbeAckData` 或 `ReleaseData` 两个 C beat 的唯一收集状态 | `c_assembly_*` | 首拍建立 owner，第二拍完成主存写回和状态清理 |
| `drain` | global stop 后只完成已握手或已在 DUT channel 上的事务，不再由环境新建 Probe | `dcache_mem__access_base_sequence::body()` | pending D、GrantAck、Probe C 全部归零后发送 terminal idle |
| `sideband` | 不属于 TileLink A/B/C/D/E 主握手的 Hint/flush 输入 | `io_l2_hint_*`、`io_l2_flush_done` | Hint 只与一笔 `AcquireBlock` 生命周期绑定 |
| `cached line table` | 已完成 GrantAck、可供 Probe 选择的 64B line 候选表，不是完整目录或参考模型 | `cached_alias_by_line` | E.fire 后插入，Probe/Release/CBO 失效时删除 |
| `lockstep driver` | sequence 每个采样边界提交一个 item，driver 立即写 clocking output，不重复上一 item | `dcache_agent_agent_driver::main_phase()` | D.ready=1 时一个 D beat 只计一次 fire |

本 plan 解决当前 `dcache_mem__access_base_sequence` 只做简化 A 到 D 回复、无法按 coherent
TileLink-C 场景返回、没有 L2 response delay、没有有效 hint、也没有主动 Probe 和 DCache 缓存地址
影子表的问题。

本轮实现以下能力：

1. 按 DCache 端口真实请求类型区分 `GrantData`、`Grant`、`CBOAck` 和 `ReleaseAck`。
2. 用 3 个相对权重参数选择 small、medium、large 三类 L2 response delay。
3. 只对 `AcquireBlock -> GrantData` 按权重产生一次 `io_l2_hint_valid`，并正确返回
   `sourceId/isKeyword`。
4. 在 `dcache_mem__access_base_sequence` 内维护一个 64-byte line 对齐的 DCache 缓存地址影子表。
5. 按 Probe 权重决定是否尝试 Probe；命中后从地址表中等概率随机选择一项，发送单个
   `Probe(toN)`，等待 `ProbeAck/ProbeAckData` 后删除该项。
6. 消费 DCache C-channel `Release/ReleaseData`，把它作为替换或写回事件，从地址表删除 line，
   必要时更新现有主内存并返回 `ReleaseAck`。
7. 跟踪 `Grant/GrantData` 的固定 sink 和 E-channel `GrantAck`，在 grant 生命周期完整结束后才把
   line 加入地址表。

替代关系：

- 本 plan 完整替代
  `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_dcache_l2_sideband_responder_adapt_execution_plan_20260712.md`。
  原 plan 的 interface known-zero、idle known-zero 和 `io_l2_flush_done=0` 要求并入本 plan；
  原 plan 的 hint 永久 zero-only 要求不再执行。
- 本 plan 取代
  `AI_DOC/plan/test_framework/plan/do/dcache_l2_tilelink_interaction_plan_20260614.md`
  作为当前 V2 的可执行轻量方案。旧 plan 中多 source、多 sink、完整权限目录、Probe cooldown、
  error injection 和完整并发模型不在本轮实现。

RTL 语义依据：

- `AI_DOC/analysis/rtl/v2/flows/l2_inner_tilelink_request_response_flow.md`
- `AI_DOC/analysis/rtl/v2/flows/dcache_l2_refill_hint_and_flush_done_flow.md`

## 2. 明确不实现的范围

本轮不实现：

- 完整 CoupledL2 directory、slice、MSHR、replacement policy、CHI transaction 或多 client source remap。
- ICache、PTW、Uncache responder，以及这些端口的 `Get -> AccessAckData`。
- 多个 A request、多 sink 或多个 Probe 并发。
- Probe `toB/toT`、随机 `needData`、Probe cooldown、权限升级冲突和 snoop directory 查找。
- `denied/corrupt` 错误注入。
- `io_l2_flush_done` 功能模型。该信号仍从 time zero 到测试结束保持 0。
- RM、scoreboard、checker 和功能覆盖率实现。

dispatch redirect/replay 不直接清除已经 A.fire 的 L2 pending reply、hint 排期、GrantAck 或 Probe
生命周期。当前 RTL 依据没有把这些 coherent transaction 绑定到 dispatch flush epoch；本轮只有 reset
或对应 TileLink transaction 真实完成可以清理这些状态。

当前 MemBlock DUT 暴露的是分离的 DCache coherent 端口。这个端口上的 local source 不能代表
ICache/PTW 等非 DCache client。因此本 plan 对“非 DCache source”和 `AccessAckData` 的处理是明确
拒绝在当前 responder 中伪造，而不是在 DCache 端口增加错误回复类型。

## 3. 修改文件和职责

| 文件 | 修改职责 |
|---|---|
| `mem_ut/ver/ut/memblock/env/plus.sv` | 定义并加载 5 个 L2 responder 权重参数 |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv` | 保存参数快照、检查范围、提供只读 getter |
| `mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg` | 增加保守默认值 |
| `mem_ut/ver/ut/memblock/seq/plus_cfg/tc_dispatch_real_l2cache_model.cfg` | 独立可执行的 real-smoke 基础 preset；覆盖三档 delay，hint/Probe 默认关闭 |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv` | 在 `dcache_mem__access_base_sequence` 内实现逐拍轻量 L2 responder、地址表和 Probe owner |
| `mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv` | 保存 DCache responder terminal-done 兼容握手，不参与主表语义 |
| `mem_ut/ver/ut/memblock/tc/src/tc_dispatch_real_smoke.sv` | legacy direct testcase 等待 DCache responder 自然完成后再 drop objection |
| `mem_ut/ver/ut/memblock/agent/dcache_agent_agent/src/dcache_agent_agent_interface.sv` | 给 4 个 L2 sideband DUT input 增加明确 time-zero 初值 |
| `mem_ut/ver/ut/memblock/agent/dcache_agent_agent/src/dcache_agent_agent_xaction.sv` | 让 generic random path 保持 sideband 为 0，专用 responder 通过手工 builder 产生合法 hint |
| `mem_ut/ver/ut/memblock/agent/dcache_agent_agent/src/dcache_agent_agent_driver.sv` | idle 隔离 sideband，发送前检查 hint payload 和 flush zero-only 合同 |
| `mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md` | 同步登记新增公共参数及使用者 |
| `AI_DOC/mem_ut_flow_doc/dcache_l2_response_hint_probe_model_flow.md` | 新增本专项逐拍协议 flow |
| `AI_DOC/mem_ut_flow_doc/dcache_sbuffer_memory_responder_flow.md` | 同步共享 memory responder 文档中的 DCache 分支，保留 SBuffer 分支 |
| `AI_DOC/mem_ut_flow_doc/virtual_sequence_unified_dispatch_flow.md` | 同步 background DCache responder 的完整 in-flight drain 条件 |
| `AI_DOC/analysis/source_sv/dispatch_framework_sv/mem_base_sequence.md` | 同步共享源码分析、函数清单和 DCache/SBuffer 边界 |
| `AI_DOC/analysis/interface/memblock验证输入的程序框架.md` | 把 Hint 从永久安全常量更新为 request-bound responder 输入，flush_done 仍为 0 |
| `AI_DOC/analysis/interface/v2/mem_ut_v2_agent_interface_signal_matrix_20260709.md` | 在既有字段矩阵后补当前 producer/owner 语义，不改变字段统计 |
| 两个被替代的旧 plan | 标记为已被本 plan 替代，不再单独执行 |

不新增 agent、interface 端口、testcase class 或 virtual sequence；现有
`memblock_dispatch_real_smoke_vseq` 已经在 `p_sequencer.dcache_sqr` 上启动
`dcache_mem__access_base_sequence`，因此 `seq_pkg.sv`、`seq.f`、`tb.f` 和 vseq 调度不变。
当前 DCache monitor 的 transaction 创建和 analysis-port 发布仍处于注释状态，本 plan 不恢复 monitor
producer，也不依赖 monitor event。A/B/C/D/E fire 全部由同一 sequence 直接组合“上一拍实际发送的
`last_cycle_xact`”与当前 VIF 对端 ready/valid 计算。

`tc_dispatch_real_l2cache_model.cfg` 不会继承 `tc_dispatch_real_smoke.cfg`；因此该文件显式包含
real-smoke 所需的主表、LSQ enqueue/issue/commit、L2TLB 和 TLB 合法性开关，再覆盖本专项的 L2
权重。这样直接以 `cfg=tc_dispatch_real_l2cache_model` 运行时不会因缺少基础 preset 而静默关闭主流程。

本专项修改了共享 `dcache_agent_agent_driver` 和 `mem_base_sequence.sv`，因此不能只新增一份 L2Cache
专项 flow。执行完成前必须扫描这些 class/function 的全部当前有效文档引用，并同步 DCache/SBuffer
共享 responder flow、virtual-sequence background drain flow、源码分析和接口输入分类文档。

## 4. 问题一：L2 回复类型没有按 DCache coherent 场景分层

### 功能抽象与实现

L2 responder 的第一项职责是把已握手请求分类，并生成当前 DCache 端口能够消费的回复。分类必须先看
端口身份和请求 opcode，再确定 D opcode、权限 param、beat 数、是否分配 sink、是否等待 E ack 和是否
允许产生 hint。

本轮使用单 D reply 在途模型：同一时刻只保存一个 A-driven reply 或 C-driven `ReleaseAck`。当该回复尚未完成，
`a_ready` 对新 A request 保持 0。`Grant/GrantData` 完成 D-channel 发送后继续等待固定 sink 0 的
`GrantAck`，等待期间不接受新的 Acquire。串行化降低吞吐，但保持 TileLink backpressure 合法，并避免
引入 sink pool、source map 和多队列仲裁。一个已经 B.fire、正在等待 C ack 的 Probe 可以与顺序到达的
一笔普通 C Release 共存：先完成 Release/ReleaseAck，再继续等待原 ProbeAck；仍不允许第二个 Probe 或
第二个 D reply 并发。

场景合同：

| 当前端口输入 | 合法回复 | 权限/生命周期 | Hint |
|---|---|---|---|
| A `AcquireBlock(NtoB)` | 两拍 `GrantData` | `param=toB`，`sink=0`，等待 E | 可按权重产生 |
| A `AcquireBlock(NtoT/BtoT)` | 两拍 `GrantData` | `param=toT`，`sink=0`，等待 E | 可按权重产生 |
| A `AcquirePerm(NtoT/BtoT)` | 单拍 `Grant` | `param=toT`，`sink=0`，等待 E | 不产生 |
| A `CBOClean/CBOFlush/CBOInval` | 单拍 `CBOAck` | 原 source，`param=0`，不等待 E | 不产生 |
| C `Release/ReleaseData` | 单拍 `ReleaseAck` | 原 C source/size，不等待 E | 不产生 |
| DCache 端口 A `Get/Put/Arithmetic/Logical/Hint` | 不回复 `AccessAckData` | 当前 coherent 端口不支持，发送前 fail-fast | 不产生 |
| ICache/PTW/Uncache 或非 DCache source | 本 sequence 不可见 | 留给对应分离端口 responder | 不产生 |

class 内只定义当前轻量模型实际使用的命名常量：

```text
A：AcquireBlock=6，AcquirePerm=7，CBOClean=12，CBOFlush=13，CBOInval=14；
B：Probe=6；
C：ProbeAck=4，ProbeAckData=5，Release=6，ReleaseData=7；
D：Grant=4，GrantData=5，ReleaseAck=6，CBOAck=8；
Grow：NtoB=0，NtoT=1，BtoT=2；
Cap：toT=0，toB=1，toN=2；
Report/Shrink：TtoB=0，TtoN=1，BtoN=2，TtoT=3，BtoB=4，NtoN=5。
```

这些值集中在 `dcache_mem__access_base_sequence` class scope，不修改空的
`dcache_agent_agent_dec.sv`，也不新增全局 package 常量。

`AcquireBlock` 的两拍 data 来自现有 `mem_access_base_sequence` 主内存，line 固定 64 byte，D beat 固定
32 byte。`echo_isKeyword=0` 时发送低半行再高半行；`echo_isKeyword=1` 时发送高半行再低半行，两个
beat 的 `source/sink/size/param/echo/denied/corrupt` 保持不变。`denied/corrupt` 本轮固定为 0。
完整 line 不建立第二份 data store，使用现有 `dcache_mem_access_task()` 分别读取
`line_addr` 和 `line_addr+32` 两个 32-byte beat，再保存到当前 pending reply 快照。

### 修改了什么逻辑

1. 在 class 内集中定义 A/B/C/D opcode 和 Grow/Cap/Report param 的命名 localparam/enum，后续 helper
   不散落 magic number；用 `classify_dcache_a_request()` 取代当前 `dcache_d_opcode()` 的默认 fallback，
   不再把未知 opcode
   默认回复为 `AccessAckData`。
2. `dcache_mem_access_xaction()` 拆成“保存请求快照、构造 pending reply、逐拍发送”三个阶段；不再在
   接受 A 后立即用 for-loop 阻塞发送全部 D beat。
3. 对 request 的 source、size、param 和 line alignment 做 `check_` 前缀的 fail-fast 检查。
   本轮不建模 denied/corrupt，因此完整 64-byte line 不在现有主内存合法 range 时也在建立 reply 前
   fail-fast，不把 range error 静默改成成功 Grant。
4. CBO 保存原 opcode、source 和 address，直到对应 `CBOAck` D.fire，避免只看固定 source 17 丢失
   clean/flush/inval 语义。

### 文字伪代码

```text
classify_dcache_a_request(req)：
  检查 req.size == 6，地址低 6 bit 为 0；
  用 64-byte 全 mask 调用现有 range helper，检查完整 line 位于主内存合法范围；
  如果 opcode == AcquireBlock：
    检查 source 在 0..15；
    如果 param == NtoB，返回 GRANT_DATA_TO_B；
    如果 param == NtoT 或 BtoT，返回 GRANT_DATA_TO_T；
    其它 param 触发 uvm_fatal；
  如果 opcode == AcquirePerm：
    检查 source 在 0..15；
    只接受 NtoT 或 BtoT，返回 GRANT_TO_T；
    NtoB 或其它 param 触发 uvm_fatal；
  如果 opcode 是 CBOClean/CBOFlush/CBOInval：
    检查 source == 17；
    返回 CBO_ACK，并保存具体 CBO opcode；
  其它 opcode 触发 uvm_fatal；
  不生成 AccessAck、AccessAckData 或 HintAck fallback；

accept_dcache_a_request(req, a_fire_cycle)：
  该 helper 只在上一拍 a_ready 与当前 a_valid 共同确认 A.fire 后调用；
  调用 classify_dcache_a_request；
  复制本次 req 的 opcode、param、size、source、address、alias、isKeyword；
  对 GrantData 调用现有 memory helper，读取 line_addr 和 line_addr+32 两个 32-byte beat；
  把两个 beat 合并为当前 pending reply 的 64-byte data 快照；
  根据请求 param 保存最终 grant param；
  对 Grant/GrantData 固定分配 sink=0；
  调用 sample_l2_response_delay；
  first_d_due_cycle = a_fire_cycle + sampled_delay；
  只对 GrantData 调用 hint 采样和排期；
  标记 pending_d_valid=1；
  在 pending reply 完成前不再接受新 A；

service_pending_d_response(current_cycle, cycle_xact)：
  如果 current_cycle 小于 first_d_due_cycle，不置 d_valid；
  到期后按 pending kind 填 D opcode、param、source、size、sink；
  如果是 GrantData：
    beat_index=0 且 isKeyword=0 时选择低 32 byte；
    beat_index=0 且 isKeyword=1 时选择高 32 byte；
    beat_index=1 时选择另一半；
  d_ready=0 时保持全部 D payload 和 beat_index 不变；
  d_ready=1 时完成当前 beat；
  GrantData 第一拍完成后推进到第二拍；
  Grant/GrantData 最后一拍完成后清 pending_d，置 waiting_grant_ack=1；
  CBOAck 完成后清 pending_d，不等待 E；
  CBOFlush/CBOInval 完成时删除地址表中的同 line，CBOClean 保留；
```

## 5. 问题二：L2 response 没有独立、可控的 delay 分布

### 功能抽象与实现

L2 response delay 表示 A.fire 或完整 C Release 接收完成，到首拍 D.valid 启动之间的 service cycle 数。
它不包括 D-channel `ready=0` 导致的额外等待；D.valid 启动后由协议 hold 逻辑负责保持 payload。

新增 3 个相对权重参数，只控制类别选择，不新增 6 个区间边界参数。固定区间为：

| 参数 | 固定 delay 区间 | 默认值 | 含义 |
|---|---:|---:|---|
| `MEMBLOCK_L2_RSP_DELAY_SMALL_WT` | 3..5 cycle | 1 | 默认短延迟 |
| `MEMBLOCK_L2_RSP_DELAY_MEDIUM_WT` | 6..15 cycle | 0 | 中等延迟 |
| `MEMBLOCK_L2_RSP_DELAY_LARGE_WT` | 16..50 cycle | 0 | 长延迟 |

三个值都是非负相对权重，允许其中一类或两类为 0，但不允许全部为 0。区间最小值从 3 开始，使 responder
先完成 A.fire，再在后续周期产生 hint，同时仍具备合法的 2/3 拍提前窗口；hint 命中后不需要再次改写
已经抽中的 delay。

随机实现只使用 SystemVerilog `std::randomize()` 和 `dist`：第一次按三个权重抽类别，第二次在固定
闭区间内均匀抽 exact delay。不复用 `MEMBLOCK_DELAY_0_WT/1_20_WT/21_50_WT`，因为现有参数属于
dispatch issue delay，不属于 L2 responder。

### 修改了什么逻辑

1. `plus.sv -> seq_csr_common -> getter -> default.cfg` 增加独立 L2 delay 参数链。
2. `seq_csr_common::validate_and_clamp()` 对负值和全零组合 fail-fast，不静默 clamp。
3. responder 在 A.fire 或 C transaction 完成时只采样一次；后续拍不重新随机，避免 response due cycle
   被不断推迟。
4. countdown 使用 sequence 自己每发送一个 xaction 就递增一次的 `service_cycle`，不使用
   `pre_pkt_gap/post_pkt_gap` 阻塞 driver。

### 文字伪代码

```text
seq_csr_common::init()：
  从 plus 读取三个 L2 response delay 权重；
  任一权重为负时 uvm_fatal；
  三个权重全为 0 时 uvm_fatal；
  保存为本次仿真的只读快照；

sample_l2_response_delay()：
  调用 std::randomize(delay_class)，使用 dist：
    SMALL  := get_l2_rsp_delay_small_wt()；
    MEDIUM := get_l2_rsp_delay_medium_wt()；
    LARGE  := get_l2_rsp_delay_large_wt()；
  randomize 失败时 uvm_fatal，不 fallback；
  如果选中 SMALL，在 3..5 中均匀 randomize exact_delay；
  如果选中 MEDIUM，在 6..15 中均匀 randomize exact_delay；
  如果选中 LARGE，在 16..50 中均匀 randomize exact_delay；
  返回 exact_delay；

建立 pending reply：
  读取一次 current_service_cycle；
  sampled_delay = sample_l2_response_delay()；
  first_d_due_cycle = current_service_cycle + sampled_delay；
  后续拍只比较 due cycle，不重新采样，不覆盖 due cycle；
```

## 6. 问题三：`io_l2_hint_*` 没有与 GrantData 请求和返回周期绑定

### 功能抽象与实现

Hint 是 DCache `AcquireBlock -> GrantData` 的可选提前通知，不是任意 D response 的伴随 valid。当前
V2 正常 L2 实现倾向于每个 DCache GrantData 都发 hint，但 DCache 支持无 hint fallback。因此本 plan
提供一个 `[0:100]` 高电平权重参数，在准确 hint 和无 hint fallback 之间可控采样：

| 参数 | 范围 | 默认值 | 语义 |
|---|---:|---:|---|
| `MEMBLOCK_L2_HINT_VALID_WT` | 0..100 | 0 | 每个已接受 AcquireBlock 产生 hint 的百分比权重 |

权重 0 表示关闭 hint，100 表示每个 `AcquireBlock` 都产生。隐含的 no-hint 权重为
`100 - MEMBLOCK_L2_HINT_VALID_WT`。只在 A request 已分类为 GrantData 后采样一次。

Hint payload 直接来自已接受请求：

- `io_l2_hint_bits_sourceId = accepted_a_source[3:0]`，且 source 必须已验证在 0..15。
- `io_l2_hint_bits_isKeyword = accepted_a_echo_isKeyword`。
- `Grant`、`CBOAck`、`ReleaseAck`、`AccessAckData` 和非 DCache client 不产生 hint。

Hint 与首拍 D.valid 的目标间隔为 2 或 3 个 service cycle。delay 为 3 时 hint 安排在 A.fire 后一拍，
首拍 D.valid 位于 hint 后 2 拍；delay 大于等于 4 时 hint 安排在首拍 D.valid 前 3 拍。D backpressure
只会让实际 D.fire 更晚，不会重复发送 hint。

### 修改了什么逻辑

1. 原 sideband zero-only owner 改为“generic/idle 保持 0，专用 responder 唯一允许产生非零 hint”。
2. pending A state 保存 `hint_selected`、`hint_due_cycle` 和 `hint_sent`，而不是在 D response builder
   每拍重新判断。
3. 每个 selected AcquireBlock 最多发送一个单拍 valid pulse；下一拍 builder 自动恢复 valid/payload 为 0。
4. `io_l2_flush_done` 不随 hint 能力放开，仍固定为 0。

### 文字伪代码

```text
sample_hint_enable()：
  valid_wt = get_l2_hint_valid_wt()；
  调用 std::randomize(enable)，使用 dist：
    1 := valid_wt；
    0 := 100 - valid_wt；
  randomize 失败时 uvm_fatal；
  返回 enable；

schedule_hint_for_grant_data(req, d_due_cycle, current_cycle)：
  只调用一次 sample_hint_enable；
  如果未选中：hint_selected=0，hint_sent=0；
  如果选中：
    hint_ahead = (d_due_cycle-current_cycle == 3) ? 2 : 3；
    hint_due_cycle = d_due_cycle - hint_ahead；
    保存 sourceId=req.source[3:0]；
    保存 isKeyword=req.echo_isKeyword；
    hint_selected=1，hint_sent=0；

service_hint(current_cycle, cycle_xact)：
  cycle_xact 先由 idle builder 把 valid/sourceId/isKeyword 清 0；
  如果 hint_selected && !hint_sent && current_cycle == hint_due_cycle：
    写 io_l2_hint_valid=1；
    写 sourceId 和 isKeyword；
    hint_sent=1；
  后续周期不再置 valid，不重复覆盖 payload；
```

## 7. 问题四：没有简单、单一 owner 的 DCache 缓存地址影子表

### 功能抽象与实现

Probe 不能从整个物理地址范围盲选，否则大多数请求只会 Probe 一个 DCache 从未缓存的 line。base
sequence 需要维护一个轻量地址表，记录当前模型已确认授予并由 DCache 接受的 cache line。

使用 SystemVerilog 关联数组，不新增 class、数据库或第二份 data memory：

```text
key：48-bit、64-byte 对齐物理 line address；
value：该 line 最近一次 Acquire 保存的 2-bit alias；
data：仍由现有 mem_access_base_sequence 主内存唯一维护，不复制到地址表。
```

地址表只表达“可作为 Probe 候选”，不表达精确 MESI 状态、dirty、owner 或 replacement way。重复插入
同一 line 直接覆盖 alias，属于幂等更新；删除不存在的 line 只打印 `UVM_DEBUG`，不 fatal。

生命周期：

| 事件 | 地址表动作 | 原因 |
|---|---|---|
| `Grant/GrantData` 对应 E `GrantAck` fire | 插入或更新 line/alias | grant 生命周期完成，line 可成为 Probe 候选 |
| pending Probe 收到完整 `ProbeAck/ProbeAckData` | 删除 line | 本轮 Probe 固定 `toN` |
| C `Release/ReleaseData` 完整接收 | 删除 line | 代表替换、主动释放或写回 |
| `CBOFlush/CBOInval` 的 `CBOAck` fire | 删除同 line | operation 完成后 line 不再作为有效候选 |
| `CBOClean` 的 `CBOAck` fire | 保留 | clean 不要求失效 |
| reset 重新有效 | 清空全部表项 | DCache reset 后不能假定旧 line 仍缓存 |

### 修改了什么逻辑

1. 在 `dcache_mem__access_base_sequence` 增加唯一关联数组 owner；driver、monitor、xaction 和全局 package
   不保存镜像副本。
2. 插入动作从 D 最后一拍推迟到 E GrantAck，避免尚未确认 grant 的地址提前被 Probe。
3. 删除动作由真实 C/Probe/CBO 完成事件触发，不由随机 Probe launch 提前删除。
4. alias 随地址保存，用于后续 B Probe 的 `data[2:1]`，避免只保存 paddr 后无法构造当前 DCache 的
   alias payload。

### 文字伪代码

```text
line_addr(addr)：
  返回 {addr[47:6], 6'b0}；

record_cached_line(addr, alias)：
  key = line_addr(addr)；
  cached_alias_by_line[key] = alias；
  不读取或复制主内存 data；

remove_cached_line(addr, reason)：
  key = line_addr(addr)；
  如果 key 存在则 delete(key)；
  如果不存在只打印 UVM_DEBUG 和 reason；

handle_grant_ack(e_sink)：
  检查 waiting_grant_ack=1 且 e_sink==0；
  record_cached_line(pending_grant_line, pending_grant_alias)；
  清 waiting_grant_ack 及保存的 line/alias；

handle_reset()：
  清 pending A/D、pending C、pending Probe、waiting GrantAck；
  cached_alias_by_line.delete()；
  下一个可驱动周期继续从全零 idle 开始；
```

## 8. 问题五：Probe 没有按频率开启，也没有从有效缓存地址中随机选择

### 功能抽象与实现

Probe 只做轻量压力激励，不复刻 L2 directory。每个“Probe 调度机会”先按一个 `[0:100]` 权重随机决定
是否尝试；决定开启后，再从地址表当前全部项中等概率随机选择一项。

| 参数 | 范围 | 默认值 | 语义 |
|---|---:|---:|---|
| `MEMBLOCK_L2_PROBE_ENABLE_WT` | 0..100 | 0 | 每个合格空闲 service cycle 启动一个 Probe 的百分比权重 |

合格调度机会必须同时满足：

- reset 已解除且 backend reset done。
- 地址表非空。
- 没有 pending A/D reply。
- 没有等待 E GrantAck。
- 没有 pending C Release/ReleaseAck。
- 没有已发送或正在握手的 Probe。
- 当前没有待优先接受的 C request 或 A request。

Probe 固定使用最简单的合法模板：`opcode=Probe`、`param=toN`、`size=6`、地址为选中的 line、
`source=0`、mask 全 1、`data[0]=0`、`data[2:1]=cached alias`、其余 data 为 0、`corrupt=0`。
本轮不随机 `needData`；即使 `needData=0`，dirty line 仍可能由 DUT 返回 `ProbeAckData`，responder 必须
能够接收两种 C reply。

为保持逻辑简单，同一时刻只允许一个 pending Probe。B.valid 一旦启动，就保持全部 B payload 到
B.ready fire；不能因为下一拍随机结果为 0 而撤销。

### 修改了什么逻辑

1. 新增 Probe 百分比权重参数和只读 getter；0 保持现有默认无 Probe 行为。
2. 不建立候选 queue 镜像。只有随机结果为开启且 map 非空时，使用关联数组 `first()/next()` 按随机
   ordinal 遍历一次。
3. Probe 地址只来自当前地址表，不从任意 paddr range 生成。
4. B-channel handshake 和后续 C response 由同一个 pending Probe state owner 管理。

### 文字伪代码

```text
sample_probe_enable()：
  probe_wt = get_l2_probe_enable_wt()；
  调用 std::randomize(enable)，使用 dist：
    1 := probe_wt；
    0 := 100 - probe_wt；
  返回 enable；

select_random_cached_line(line, alias)：
  entry_count = cached_alias_by_line.num()；
  如果 entry_count == 0，返回失败；
  用 std::randomize(ordinal) 生成 0..entry_count-1；
  调用 first(key)，再调用 ordinal 次 next(key)；
  返回 line=key、alias=cached_alias_by_line[key]；
  不建立永久 queue，不在每个 idle cycle 扫描 map；

try_start_probe(current_cycle)：
  如果任一 pending/hazard 条件存在，直接返回；
  如果当前 A.valid 或 C.valid，先服务 DUT request，直接返回；
  如果地址表为空，直接返回；
  如果 sample_probe_enable()==0，直接返回；
  调用 select_random_cached_line；
  保存 pending_probe_line 和 pending_probe_alias；
  构造固定 Probe(toN) payload；
  置 pending_probe_b_valid=1；

service_probe_b(cycle_xact)：
  如果 pending_probe_b_valid：
    每拍驱相同 b_valid/opcode/param/size/source/address/mask/data/corrupt；
    b_ready=0 时保持 state 和 payload 不变；
    b_ready=1 时完成 B fire，清 b_valid，置 waiting_probe_c=1；
  地址表此时仍不删除；
```

## 9. 问题六：C-channel 没有区分 Probe 回复、替换和写回

### 功能抽象与实现

C channel 同时承载两类不同生命周期：

1. `ProbeAck/ProbeAckData` 是当前 pending B Probe 的回复，不需要 D ack。
2. `Release/ReleaseData` 是 DCache 主动替换、释放或写回，需要 L2 返回 `ReleaseAck`。

本轮只允许一个 C transaction 正在收集。带 data 的 C transaction 按 64-byte line、32-byte beat 收集
两拍；无 data transaction 单拍完成。`ProbeAckData/ReleaseData` 且任一 beat `corrupt=1` 时不更新主
内存，但仍完成协议生命周期，避免 DUT 卡死。

`ReleaseData` 和 `ProbeAckData` 的完整 64-byte 数据继续写入现有 `mem_access_base_sequence` 主内存，
使用两个 32-byte 全 mask write，不新建 DCache data mirror。

### 修改了什么逻辑

1. idle xaction 的 `c_ready` 不再无条件跟随 generic driver mode，由 responder 根据唯一 C owner 明确驱动。
2. pending Probe 时，`ProbeAck/ProbeAckData` 必须与 pending line 地址一致，但不强制 C source 等于
   B source；若 C 仲裁先送来普通 `Release/ReleaseData`，按独立 Release 生命周期处理，不清 pending Probe。
3. `Release/ReleaseData` 完成后先删除地址表，再按同一 delay sampler 建立 `ReleaseAck` pending D reply。
4. C unknown opcode、beat 地址变化、source/size 在多 beat 中变化均 fail-fast。

### 文字伪代码

```text
accept_dcache_c_beat(c_vif_fields)：
  先按 C opcode 分类，不因为 waiting_probe_c 就把所有 C transaction 当成 ProbeAck；
  如果 opcode 是 ProbeAck 或 ProbeAckData：
    检查 waiting_probe_c=1；
    检查 line_addr(c.address)==pending_probe_line；
    检查 size==6，param 只允许 TtoN/BtoN/NtoN；
    ProbeAck 单拍完成；
    ProbeAckData 保存当前 32-byte beat，检查固定 source/size/address；
    第0拍写入 line byte 0..31，第1拍写入 byte 32..63，收满两拍；
    数据完整且没有 corrupt 时，用两次全 mask store 更新现有主内存；
    完成后 remove_cached_line(pending_probe_line, "probe_toN")；
    清 pending Probe 和 C assembly；
    不建立 D reply；

  如果 opcode 是 Release 或 ReleaseData：
    检查 size==6、地址 64-byte 对齐；
    检查完整 line 位于现有主内存合法范围；
    保存 release source、size、line address；
    Release 单拍完成；
    ReleaseData 保存两拍 data，检查多拍字段稳定；
    第0拍写入 line byte 0..31，第1拍写入 byte 32..63，收满两拍；
    数据完整且没有 corrupt 时，用两次全 mask store 更新现有主内存；
    无论是否 corrupt，都 remove_cached_line(release_line, "release_or_writeback")；
    调用 sample_l2_response_delay；
    建立单拍 ReleaseAck pending D reply；
    ReleaseAck 使用原 C source/size，param/sink/denied/corrupt 全 0；
    如果当前还在 waiting_probe_c，不清 pending Probe；
    ReleaseAck D.fire 后重新开放 c_ready，继续等待原 ProbeAck；

  其它 C opcode 触发 uvm_fatal；
```

## 10. 问题七：Grant、E ack、地址表插入和 Probe hazard 没有闭环

### 功能抽象与实现

`Grant/GrantData` 不是 D.fire 后立即结束。L2 必须保留 sink，直到 DCache 用 E `GrantAck` 归还。
本轮固定只使用 sink 0，并在 E ack 前阻止新 Acquire 和 Probe，因此不需要 sink allocator。

地址表也不能在 D.valid 或第一拍 D.fire 时插入；只有最后一拍 D.fire 且同 sink 的 E.fire 完成后，
才把 line 和 alias 插入候选表。这样随机 Probe 不会打到尚未完成 grant 的 line。

### 修改了什么逻辑

1. 新增 `waiting_grant_ack`、`pending_grant_line`、`pending_grant_alias` 三个 sequence-local 字段。
2. `e_ready` 默认保持 0；只有 `waiting_grant_ack=1` 的 owner 分支才打开为 1，避免 DUT 在
   responder 尚未建立 GrantAck owner 时提前消费 E。E.fire 时检查 sink 和 owner。
3. 未等待 E 时看到 E.valid 视为协议/模型不一致并 `uvm_fatal`，不静默消费。
4. 等待 E 时允许处理 E，A 和 Probe 保持 blocked；不设置固定 timeout，以免把合法 DUT latency误判为错，
   但使用现有 no-progress 机制和 UVM debug 日志暴露长期等待。

### 文字伪代码

```text
Grant 或 GrantData 最后一拍 D.fire：
  保存本次 line 和 alias；
  保存 expected_sink=0；
  waiting_grant_ack=1；
  清 pending D reply；
  不插入地址表；

service_e_channel(cycle_xact)：
  默认 cycle_xact.e_ready=0；
  只有 waiting_grant_ack=1 时把 cycle_xact.e_ready 置 1；
  如果 vif.e_valid==0，返回；
  如果 waiting_grant_ack==0，uvm_fatal；
  如果 vif.e_sink!=0，uvm_fatal；
  本拍形成 E.fire 后：
    record_cached_line(pending_grant_line, pending_grant_alias)；
    清 waiting_grant_ack；
    清保存的 line、alias 和 sink；
  下一拍才重新开放 A 或 Probe 调度；
```

## 11. 问题八：sideband generic random、idle 和专用 responder 的 owner 冲突

### 功能抽象与实现

`dcache_agent_agent_xaction` 同时包含 TileLink channel 和 L2 sideband。generic default randomization
不能随机产生与 pending Acquire 无关的 hint；driver idle 也不能按 `DRV_1/DRV_X/DRV_RAND` 随机改变 hint
或 `io_l2_flush_done`。唯一合法的非零 hint producer 必须是本 plan 的 responder builder。

因此采用两层合同：

1. generic xaction random path 和 driver idle path 永远生成 sideband 0。
2. `dcache_mem__access_base_sequence` 使用 `type_id::create()` 后手工填字段，不调用该 response item 的
   randomize，因此可以按 pending Acquire 构造非零 hint。

driver 只检查 transaction 自洽，不重复维护 pending request map：

- `io_l2_flush_done` 必须为 0。
- `io_l2_hint_valid=0` 时 `sourceId/isKeyword` 必须为 0。
- `io_l2_hint_valid=1` 时透传 responder 已构造的 payload。

### 修改了什么逻辑

1. interface 4 个 sideband 声明增加显式 0 初值，消除 time-zero X。
2. xaction 默认 constraint 和 `new()` 把 4 个字段置 0，阻止无 owner 的 random hint/flush。
3. `build_dcache_idle_xaction()` 每拍先清 4 个字段，所有功能 helper 只覆盖当拍需要的 hint。
4. driver `drive_idle()` 在 mode 分支外无条件驱 4 个 0，不让 generic `drv_mode` 影响 sideband。
5. driver `send_pkt()` 在第一次 vif 赋值前调用 `check_l2_sideband_item()`；合法 hint 继续透传，flush
   永远驱 0。

### 文字伪代码

```text
xaction generic randomize/new：
  约束并初始化 io_l2_hint_valid=0；
  约束并初始化 sourceId=0、isKeyword=0；
  约束并初始化 io_l2_flush_done=0；
  专用 responder create item 后手工覆盖 hint，不对该 item 调 randomize；

driver::check_l2_sideband_item(tr)：
  如果 tr.io_l2_flush_done!=0，uvm_fatal；
  如果 tr.io_l2_hint_valid==0 且 sourceId/isKeyword 任一非零，uvm_fatal；
  不检查 pending request，不建立第二份 hint owner；

driver::drive_idle(mode)：
  保留原 TileLink A/B/C/D/E mode 行为；
  离开 mode 分支后无条件驱 hint valid/payload 和 flush_done 为 0；

driver::send_pkt(tr)：
  第一条 vif 赋值前执行 check_l2_sideband_item；
  检查通过后透传 A/B/C/D/E 和 hint 字段；
  io_l2_flush_done 明确驱 0；
```

## 12. 问题九：现有 body 是阻塞式 A-to-D for-loop，无法统一服务 delay、B/C/E 和 hint

### 功能抽象与实现

所有功能由 `dcache_mem__access_base_sequence::body()` 的单一逐拍 service loop 调度。每轮只构造和发送
一个 `cycle_xact`。sequence 在每轮入口等待 `@(dcache_vif.drv_cb)`，该边界先采样上一轮已经写入
clocking output 的值；随后 driver 通过阻塞 `get_next_item()` 取到本轮 item 后立即 `send_pkt()`，不再
额外插入 hold 边界或复用上一 item。这样每个状态只由 sequence 本身更新一次，不创建并行队列线程，
也不让 `pre_pkt_gap/post_pkt_gap` 代替协议状态机。

fire 采用两阶段确认：当前轮看到 DUT valid 时只捕获稳定 payload 并在 `cycle_xact` 中 arm 对应 ready；
`finish_item()` 返回后保留该 item 作为 `last_cycle_xact`。下一轮先用
`last_cycle_xact.<env_valid_or_ready> && vif.<dut_ready_or_valid>` 判断上一驱动边沿是否真实 fire，只有 fire
成立才创建/推进 pending state。不得在 ready item 尚未送到 driver 时提前宣称 request 已接受。
这里的 DUT ready/valid/payload 使用 `dcache_vif.drv_cb` 在 clocking event 采样的 input，不使用可能已经
进入下一组合状态的 raw interface 值；coding 时必须先对照现有 driver clocking 时序确认
`last_cycle_xact` 与 sampled input 属于同一握手边沿。

每拍优先级：

1. reset 检查和本地状态清理。
2. 用 `last_cycle_xact` 和当前 VIF 采集上一驱动边沿的 A/B/C/D/E handshake 结果并推进 pending state。
3. 继续正在 hold 的 D.valid 或 B.valid，保证 payload 稳定。
4. 继续正在收集的多拍 C transaction；每个 beat 只在确认 C.fire 后入 assembly。
5. pending D reply 未到期时只计时；到期时发送 D，期间不再接受新的 A/C/B transaction。
6. 等待 GrantAck 时只服务 E，A/C/B 保持 backpressure。
7. 等待 Probe C 时开放 C dispatcher；ProbeAck 走 Probe 生命周期，先到的 Release 走 Release 生命周期。
8. 完全空闲时优先接受普通 C `Release/ReleaseData`，其次接受 A request，最后按权重尝试新 Probe。
9. 根据已保存排期叠加当拍单次 hint。
10. 发送 `cycle_xact`，`service_cycle++`。

同一 item 可以同时携带独立的 E.ready、C.ready 和一个 sideband hint，但本轮不在同拍同时接受 A 和 C，
也不同时启动新的 B 和 D transaction。这个限制是测试框架的简化吞吐策略，不改变 DUT 接口合法性。

### 修改了什么逻辑

1. 删除 body 内“看到 A.valid后立即 for-loop 生成 D beat”的阻塞组织方式。
2. `seq_csr_common::init()` 在 body 开头调用；它是 semaphore 保护的幂等 task，兼容 vseq 已提前初始化和
   `tc_base` 直接启动 responder 两种入口。
3. 所有 builder 从 `build_dcache_idle_xaction()` 开始；该 idle 模板固定
   `a_ready=0/b_valid=0/c_ready=0/d_valid=0/e_ready=0`，4 个 sideband 为 0，
   `pre_pkt_gap/post_pkt_gap` 固定 0。只有 `waiting_grant_ack` owner 可以把 e_ready 覆盖为 1，
   其它 owner helper 只能覆盖自己负责的 ready/valid。
4. 只在一个地方更新 `service_cycle`、pending state、地址表和 Probe state；保存
   `last_cycle_xact/last_drive_cycle` 作为 handshake 事实，不读取未启用的 monitor analysis port。

### 文字伪代码

```text
dcache_mem__access_base_sequence::body()：
  获取 dcache_vif，失败则 uvm_fatal；
  调用 seq_csr_common::init()；
  check_l2_model_cfg()；
  清全部 pending state、service_cycle、last_cycle_xact 和缓存地址表；

  forever：
    先等待一个 dcache_vif.drv_cb 边界；
    采样 DUT 当前 valid/ready，作为上一轮 cycle_xact 的握手对端值；
    build_dcache_idle_xaction(cycle_xact)；
    默认 a_ready=0、b_valid=0、c_ready=0、d_valid=0、e_ready=0；
    cycle_xact.pre_pkt_gap=0，post_pkt_gap=0；

    如果 reset 未解除或 backend reset 未完成：
      清 pending state 和地址表；
      发送 a/b/c/d/e/sideband 全 0 的 safe idle；
      last_cycle_xact = cycle_xact；
      last_drive_cycle = service_cycle；
      service_cycle++；
      continue；

    如果 last_cycle_xact 非 null：
      a_fire = last_cycle_xact.a_ready && 当前vif.a_valid；
      b_fire = last_cycle_xact.b_valid && 当前vif.b_ready；
      c_fire = last_cycle_xact.c_ready && 当前vif.c_valid；
      d_fire = last_cycle_xact.d_valid && 当前vif.d_ready；
      e_fire = last_cycle_xact.e_ready && 当前vif.e_valid；
      先按 D完成建立GrantAck owner、E消费、C beat、B完成、A接受的定义顺序推进状态；
      A.fire 时调用 accept_dcache_a_request(a_snapshot, last_drive_cycle) 建立 pending reply；
      C.fire 时才把已捕获的当前 beat snapshot 加入 assembly或完成无data C transaction，
        ReleaseAck due cycle 也以 last_drive_cycle 为起点；

    如果最后一拍 D.fire 与合法 E.fire 在同一观察周期出现，先建立 waiting_grant_ack，再消费 E；
    只有 waiting_grant_ack 分支把 e_ready=1，否则保持 e_ready=0；

    如果 D 正在 hold：继续驱同一 D payload，c_ready和a_ready保持0；
    否则如果 B 正在 hold：继续驱同一 B payload；
    否则如果 C multi-beat assembly 已开始：只开放 c_ready，继续收集同一 C transaction；
    否则如果 pending D 已到期：启动 D reply；
    否则如果 pending D 仍在计时：保持 A/C/B blocked，只等待 due cycle；
    否则如果 waiting_grant_ack：只保持 e_ready=1，A/C/B blocked；
    否则如果 waiting_probe_c：
      先检查当前 C opcode 是否为 ProbeAck/Data 或 Release/Data，再开放 c_ready；
      ProbeAck/Data 必须匹配 pending Probe；
      Release/Data 建立 ReleaseAck，但不清 pending Probe；
    否则如果当前 c_valid：
      检查并捕获稳定 C header，置 c_ready=1；
      下一轮只在确认 C.fire 后采集 Release/Data；
    否则如果没有 pending D/grant/probe/release hazard 且当前 a_valid：
      先捕获稳定 A payload并完成基础合法性检查；
      当拍只置 a_ready=1并记录 a_accept_armed；
      下一轮确认 A.fire 后才建立 pending reply、delay和hint排期；
    否则：
      尝试按权重启动一个 Probe；

    对已经在本轮确认 fire 的 A/C，不再用同一拍 sampled valid 重新 arm；
    也就是 C 分支和 A 分支分别要求 !c_fire / !a_fire，避免同一 fire 被重复分类；
    service_hint(service_cycle, cycle_xact)；
    cycle_xact.io_l2_flush_done=0；
    send_dcache_xaction(cycle_xact)；
    last_cycle_xact = cycle_xact；
    last_drive_cycle = service_cycle；
    service_cycle++；
```

## 13. 参数链和专项 preset

新增参数只属于公共测试框架，不进入 `memblock_env_cfg`、`user_cfg.local.sv` 或 compile macro：

```text
env/plus.sv
  -> seq_csr_common::load_from_plus()
  -> seq_csr_common::validate_and_clamp()
  -> seq_csr_common getter
  -> dcache_mem__access_base_sequence 启动时读取
```

完整参数表：

| 参数 | 默认值 | 合法范围/合同 |
|---|---:|---|
| `MEMBLOCK_L2_RSP_DELAY_SMALL_WT` | 1 | 非负，三类不能全 0 |
| `MEMBLOCK_L2_RSP_DELAY_MEDIUM_WT` | 0 | 非负，三类不能全 0 |
| `MEMBLOCK_L2_RSP_DELAY_LARGE_WT` | 0 | 非负，三类不能全 0 |
| `MEMBLOCK_L2_HINT_VALID_WT` | 0 | 0..100，0 关闭，100 每个 AcquireBlock 都发 |
| `MEMBLOCK_L2_PROBE_ENABLE_WT` | 0 | 0..100，0 关闭，100 每个合格机会都尝试 |

`tc_dispatch_real_l2cache_model.cfg` 是独立可执行 preset，显式包含 real-smoke 的主流程开关，
并在其后覆盖专项值。当前默认配置为：

```text
+MEMBLOCK_L2_RSP_DELAY_SMALL_WT=6
+MEMBLOCK_L2_RSP_DELAY_MEDIUM_WT=3
+MEMBLOCK_L2_RSP_DELAY_LARGE_WT=1
+MEMBLOCK_L2_HINT_VALID_WT=0
+MEMBLOCK_L2_PROBE_ENABLE_WT=0
```

需要观察 hint 或 Probe 时，使用一次只覆盖一个参数的 plusarg，例如
`+MEMBLOCK_L2_HINT_VALID_WT=100` 或 `+MEMBLOCK_L2_PROBE_ENABLE_WT=100`；当前 Makefile/plus
解析路径不把多个空格分隔的 plusarg 稳定合并为一个 directed preset。

### 文字伪代码

```text
seq_csr_common::validate_and_clamp()：
  对三个 delay 权重调用 get_non_negative_int；
  三者全 0 时调用 fatal_if_all_zero3；
  hint_valid_wt 或 probe_enable_wt 不在 0..100 时 uvm_fatal；
  不 clamp，不把非法值静默改成边界值；

各 getter：
  先调用 check_initialized；
  返回对应静态快照；
  不在 getter 中 randomize，不修改 sequence state；
```

## 14. 修改方案总结

修改前：

```text
body 只观察 A.valid，接受后立即阻塞发送 D；
未知 A opcode 默认回复 AccessAckData；
Acquire/CBO 权限、sink、E ack 和 C Release 生命周期不完整；
response 没有独立 delay；
hint/flush sideband 可能由 generic random 或 drv_mode 产生；
B Probe 永远不发，C 只拉 ready 不解析；
没有可供 Probe 选择的 DCache 缓存地址表。
```

修改后：

```text
一个逐拍 service loop 唯一管理 A/C/D/E、hint、Probe 和地址表；
AcquireBlock/AcquirePerm/CBO/Release 分别生成 GrantData/Grant/CBOAck/ReleaseAck；
AccessAckData 和非 DCache client 明确留在其它分离端口，不在 DCache responder 伪造；
每个 reply 只采样一次 small/medium/large delay；
AcquireBlock 可按权重产生一次匹配 sourceId/isKeyword 的 2/3 拍提前 hint；
Grant 等待 E ack 后插入地址表；Probe、Release、写回和失效类 CBO 完成后删除；
Probe 按权重从地址表等概率随机选一项，固定发送 Probe(toN)，等待 C ack 后闭环；
io_l2_flush_done 继续保持 zero-only；
默认 hint/probe 权重为 0，专项 preset 才主动打开。
```

## 15. 验证与验收

### 15.1 静态检查

```bash
rg -n "MEMBLOCK_L2_RSP_DELAY_|MEMBLOCK_L2_HINT_VALID_WT|MEMBLOCK_L2_PROBE_ENABLE_WT" \
  mem_ut/ver/ut/memblock/env/plus.sv \
  mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv \
  mem_ut/ver/ut/memblock/seq/plus_cfg

rg -n "GrantData|GrantAck|CBOAck|ReleaseAck|ProbeAck|ProbeAckData|cached_alias_by_line|io_l2_hint" \
  mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv \
  mem_ut/ver/ut/memblock/agent/dcache_agent_agent/src

git diff --check -- AI_DOC AGENTS.md mem_ut/ver/ut/memblock
```

### 15.2 参数和负向检查

- 三个 delay 权重全 0 时，激励开始前 `uvm_fatal`。
- hint 或 Probe 权重小于 0 或大于 100 时，激励开始前 `uvm_fatal`。
- DCache coherent 端口出现 `Get/Put/Arithmetic/Logical/Hint` 时，不返回 `AccessAckData`，而是在建立
  pending reply 前 fail-fast。
- `io_l2_hint_valid=0` 但 payload 非零，或任意 item 令 `io_l2_flush_done=1` 时，driver 在首次 vif
  赋值前 fail-fast。

### 15.3 功能检查

1. 只打开 SMALL 权重：所有首拍 D.valid 延迟位于 3..5 cycle。
2. 分别只打开 MEDIUM、LARGE：延迟分别位于 6..15、16..50 cycle。
3. hint 权重 100、Probe 权重 0：每个 `AcquireBlock` 恰好一个 hint；`sourceId/isKeyword` 匹配 A；
   首拍 D.valid 在 hint 后 2 或 3 cycle；`AcquirePerm/CBO/ReleaseAck` 无 hint。
4. hint 权重 0：GrantData 正常完成，证明无 hint fallback 不死锁。
5. `AcquireBlock` 返回两拍 `GrantData`，keyword=0/1 时 data half 顺序正确，D backpressure 时 payload
   稳定，最后收到 sink 0 的 E ack 后地址表插入。
6. `AcquirePerm` 返回单拍 `Grant(toT)` 并等待 E ack。
7. 三类 CBO 都返回同 source 的 `CBOAck`；clean 保留 map，flush/inval 删除 map。
8. Probe 权重 100 且 map 非空：B.valid 保持到 B.ready，地址来自 map，收到完整 C ack 后删除；map 为空
   时只跳过，不 fatal。
9. `ReleaseData` 两拍能更新主内存、删除 map 并在随机 delay 后返回 `ReleaseAck`；corrupt data 不写内存
   但仍 ack。
10. 全流程 `io_l2_flush_done` 始终为 0，hint 无效拍 payload 为 0。

当前 scalar real smoke 默认不生成 CBO A request。CBOAck helper、source 17 关联和三类 map 动作的静态
审计属于本 plan 必须完成项；CBO runtime directed 命中依赖后续上游 CBO producer，不为了本 plan 在
DCache DUT-output 端口伪造 A request，也不把暂时不可达误判为 responder coding blocker。

### 15.4 远端编译和仿真

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=v2_l2cache_lockstep_20260723
make eda_batch_run seed=666666 tc=basicTest ts=memblock_dispatch_real_smoke_vseq \
  mode=v2_l2cache_lockstep_20260723 cfg=tc_dispatch_real_l2cache_model wave=off
make eda_batch_run seed=666669 tc=basicTest ts=memblock_dispatch_real_smoke_vseq \
  mode=v2_l2cache_lockstep_20260723 cfg=tc_dispatch_real_l2cache_model wave=on \
  plus_arg=+MEMBLOCK_L2_HINT_VALID_WT=100
make eda_batch_run seed=666668 tc=basicTest ts=memblock_dispatch_real_smoke_vseq \
  mode=v2_l2cache_lockstep_20260723 cfg=tc_dispatch_real_l2cache_model wave=on \
  plus_arg=+MEMBLOCK_L2_PROBE_ENABLE_WT=100
```

验收标准：

- 编译通过。
- 编译结果为 0 error；日志仍包含 VCS `LCA_FEATURES_ENABLED` 环境 warning。普通 real smoke、
  hint=100 定向 smoke 和 Probe=100 定向 smoke
  均为 `TEST_PASS`，`UVM_ERROR=0`、`UVM_FATAL=0`。
- 波形能对应证明 A/C/D/E、hint 和 Probe 的 handshake、delay、hold、map 生命周期。
- 默认 cfg 不主动产生 hint/Probe；使用单独 plusarg 的定向 smoke 已观察到单拍 hint 和 map-backed
  Probe。有限 transaction 数导致随机未命中 Probe 时，应提高 Probe 权重或 transaction 数后重跑，
  不把未命中当成 DUT 错误。

## 16. 剩余风险与后续边界

- 单在途和固定 sink 0 会降低 DCache MSHR 并发压力，但不会产生非法协议行为；多 outstanding 另建专项。
- 地址表只记录 line 和 alias，不是精确 coherence directory。它适合生成已知缓存地址 Probe，不适合
  判断 DUT coherence 正确性。
- 当前 Probe 固定 `toN/needData=0`。`toB/toT`、随机 needData 和权限状态组合另建专项。
- 当前 DCache 分离端口不能覆盖 ICache/PTW `AccessAckData` 和合并 L1 xbar 的非 DCache source remap；
  后续完整 L2 model 必须在对应端口或合并接口上单独实现。
- `io_l2_flush_done` 仍为 0。完整 L2 flush request/done level handshake 和全 cache Probe sweep 不属于本 plan。

## 17. RM 协同支持

本 plan 不实现 RM/checker/scoreboard。

后续正确性检查可观察 A/C/D/E handshake、pending reply kind、sampled delay、hint source/keyword、Probe line、
Release/Probe data writeback 事件和 map insert/delete reason。本 plan 不根据 DUT observed result 决定激励是否
通过。

## 18. 功能覆盖率协同支持

本 plan 不实现 coveragent/covergroup。

后续覆盖率可使用 response kind、delay class、hint selected、keyword、Probe selected、C reply opcode、
ReleaseData corrupt 和 map size 作为采样维度；这些字段只作为后续观测入口，不在本 plan 中定义 coverage
bin 或达标规则。

## 19. 执行中补充/修正（IMPLEMENTATION_DELTA）

### 19.1 专项 cfg 的 hint/probe 默认值

原 plan 建议 `tc_dispatch_real_l2cache_model.cfg` 采用非零 hint/probe 权重，作为专项压力配置。
本次 coding 按用户当前要求，把新 cfg 的 `MEMBLOCK_L2_HINT_VALID_WT` 和
`MEMBLOCK_L2_PROBE_ENABLE_WT` 默认保持为 `0`，仅保留三档 response delay 权重。

原因：

- 用户要求“新增 `tc_dispatch_real_l2cache_model.cfg`，默认参数保持 hint/probe 关闭”。
- 当前 DCache responder 的 hint/probe 能力已经在公共参数链和主循环内补齐，但专项 cfg 默认不主动打开，
  方便先用同一套 responder 验证基础可编译/可运行闭环，再由额外 plusarg 或后续 cfg 单独放开压力项。

影响范围：

- `mem_ut/ver/ut/memblock/seq/plus_cfg/tc_dispatch_real_l2cache_model.cfg`
- 对应 review 文档中的默认 cfg 说明
- 远端仿真命令需要通过额外 plusarg 或后续 cfg 才能观察非零 hint/probe

### 19.2 [IMPLEMENTATION_DELTA] e_ready owner 收紧

原 plan 的早期文字曾把 idle `e_ready` 描述为 1，并只强调在 E.fire 时检查 sink。实际 coding
发现这样会在最后一拍 D.fire 的状态切换前提前让 DUT 消费 E，sequence 还没有建立
`waiting_grant_ack` owner，GrantAck 可能被丢失。

实现调整：`build_dcache_idle_xaction()` 默认 `e_ready=0`；只有上一拍最后一拍 Grant/GrantData
已经转入 `waiting_grant_ack` 时，当前 item 才把 `e_ready` 置 1。E.fire 后校验 sink=0、插入
cached line table 并清 owner。

文字伪代码：

```text
默认构造 item 时 e_ready=0；
若 waiting_grant_ack=1，则本拍 e_ready=1；
若上一拍 e_ready=1 且 DUT e_valid=1，校验 sink 后完成 GrantAck；
其它状态不开放 E，防止无 owner 的 E 被提前消费。
```

### 19.3 [IMPLEMENTATION_DELTA] DCache driver 锁步发送

原 plan 只规定 sequence 的 `last_cycle_xact` 采样，没有明确 driver 是否会在 item 之间额外等待
clocking event。旧 driver 使用 `try_next_item()`，无 item 时会 hold 上一 item，可能在 ready=1
时重复握手同一个 D beat。

实现调整：`dcache_agent_agent_driver::main_phase()` 在 responder 模式下阻塞
`get_next_item()`，收到 item 后立即调用 `send_pkt()` 并 `item_done()`；不再保存或重复驱动上一
item，也不使用 `pre_pkt_gap/post_pkt_gap` 插入协议等待。sequence 每轮入口的 `drv_cb` 只负责
采样上一轮已经提交的 clocking output。

文字伪代码：

```text
driver 主循环：
  清空 req 句柄；
  阻塞取下一 item；
  item 非空则立即写入 clocking output；
  调用 item_done；
sequence 主循环：
  在下一 drv_cb 边界采样上一 item 的对端 ready/valid；
  用 last_cycle_xact 与采样值确认 fire，再构造下一 item。
```

### 19.4 [IMPLEMENTATION_DELTA] 同一采样拍 fire 防重复分类

原 plan 描述了 fire 后推进状态，但没有明确“本轮已经消费的 sampled valid 不得再次作为新请求
arm”。Probe C 完成时，若继续执行 idle C 分支，会把同一拍 ProbeAck 再次分类并触发错误。

实现调整：body 每轮先清零 `a_fire/b_fire/c_fire/d_fire/e_fire`；C assembly、waiting Probe C 和
idle C 分支都要求 `!c_fire`，A 分支要求 `!a_fire`。因此同一采样拍只会完成一次旧 transaction，
不会重复建立新的 A/C snapshot。

文字伪代码：

```text
先根据上一 item 和当前 sampled valid/ready 计算 fire；
若 c_fire，完成当前 C owner 并清 armed C，不再进入任何 sampled C arm 分支；
若 a_fire，接受当前 A 并清 armed A，不再把同一 A 当成新请求；
只有 !c_fire 或 !a_fire 时，才允许对当前 valid 建立下一拍 snapshot。
```

### 19.5 [IMPLEMENTATION_DELTA] 主存物理范围初始化

原 plan 要求对完整 64B line 做范围检查，但 `mem_access_base_sequence` 的 range helper 在没有
调用 `init_main_mem_range()` 时默认允许所有地址，单看 `check_line_range()` 不能保证该合同。

实现调整：DCache responder 在 `seq_csr_common::init()` 后清理本地 range，并用
`get_paddr_base()/get_paddr_range()` 初始化 `main_mem_ranges`。该检查只约束 DCache responder
看到的物理地址和主存访问，不改变主表的虚拟地址窗口或 TLB 映射策略。

文字伪代码：

```text
初始化公共参数；
清除本 sequence 旧的主存 range；
读取共享 PADDR base/range；
把 PADDR 窗口注册到本 sequence；
后续完整 line 检查逐字节要求落在该窗口内，越界直接 fatal。
```

### 19.6 [IMPLEMENTATION_DELTA] 真实 smoke cfg 独立可执行

原 plan 把专项 cfg 描述成只覆盖 L2 参数，但 Makefile 每次只读取一个 cfg 文件，不提供 cfg 继承。
实现已把 real-smoke 的主表、LSQ、issue、commit、L2TLB 和 TLB 合法性开关复制到
`tc_dispatch_real_l2cache_model.cfg`，末尾再覆盖 L2 delay/hint/probe 值；hint/probe 默认仍为 0。

### 19.7 [IMPLEMENTATION_DELTA] 清理 DCache 旧 gap/delay 残留

coding 复查发现 DCache class 中的 `default_pre_pkt_gap`、`default_post_pkt_gap` 和
`accept_dcache_a_request()` 局部 `sampled_delay` 已不再被读取；当前锁步 responder 的 gap 固定由
`build_dcache_idle_xaction()` 写 0，response 到期时间唯一由
`accept_cycle + sample_l2_response_delay()` 计算。

实现调整：删除 DCache 专属的无效字段声明和初始化，以及未使用局部变量；保留 SBuffer class 自己的
gap 字段和单拍响应逻辑。该调整不改变任何 channel、owner、delay 区间或退出条件，只避免形成第二个
时间/gap 状态来源。

### 19.8 [IMPLEMENTATION_DELTA] 最新波形验证记录

为满足本 plan 的波形验收标准，使用独立专项 cfg 重新运行：

- seed 666669、`wave=on`、`+MEMBLOCK_L2_HINT_VALID_WT=100`：A.fire=245.2ns，Hint
  `valid` 在 280.2ns 到 285.2ns 为 1，D 两个 GrantData beat 连续覆盖 295.2ns 到 305.2ns，
  E.ready 在 305.2ns 开放并于 310.2ns 完成 E.fire。
- seed 666668、`wave=on`、`+MEMBLOCK_L2_PROBE_ENABLE_WT=100`：B Probe 在 290.4ns
  完成，地址为 `0x08b64e280`；C ProbeAck 在 1470.4ns 出现同地址，1475.4ns 完成 C.fire，
  responder 退出日志为 `cached_lines=0`。

对应波形文件位于 sim mode 的 `wave/` 目录；该验证只观察接口握手和 sequence 日志，不把波形
观测误写成 RM/scoreboard 结论。

### 19.9 [IMPLEMENTATION_DELTA] 通用 `drive_idle` 模式边界

静态扫描仍可看到 `dcache_agent_agent_driver::drive_idle()` 在显式 `DRV_1/DRV_RAND` 配置下保持
既有 TileLink channel 随机/全 1 行为，其中包括 `e_ready`。这不是 real-smoke responder item 路径：
real-smoke 的 DCache agent 使用 `sqr_sw=ON`，由 `send_pkt()` 透传
`build_dcache_idle_xaction()` 的 `e_ready=0`，再由 `waiting_grant_ack` owner 单独打开 E。

实现保持通用 `drive_idle` 的原有 channel 行为，避免把本专项的 owner 语义扩散为全环境 driver 模式
改造；四个 V2 L2 sideband 在所有 generic idle mode 末尾仍强制 known-zero。后续若要用 `DRV_1` 或
`DRV_RAND` 压测 DCache coherent responder，必须另建兼容专项，不能把该模式和 real-smoke owner
合同混用。

### 19.10 [IMPLEMENTATION_DELTA] 最终 review 收紧 sideband 四态和无主 E.valid 边界

最终源码 review 发现，driver 早期实现用普通 `!=/==` 检查 Hint/flush 字段，四态值可能无法按
fail-fast 合同明确归类；同时 `process_e_fire()` 只检查已经握手的 E，尚未把原 plan 中“没有
GrantAck owner 时出现 E.valid 直接 fatal”落实到主循环。

实现调整：`check_l2_sideband_item()` 使用 case equality/inequality 检查 null item、
`io_l2_flush_done`、Hint valid 和有效 payload 的 known-value 合同；主循环在处理当拍 D/E fire 后，
若看到 `sampled_e_valid=1`、本拍没有 E.fire 且仍没有 `waiting_grant_ack` owner，则立即 fatal。
`process_e_fire()` 的 sink 比较也改为 case inequality，避免未知 sink 被当成匹配。

文字伪代码：

```text
发送 item 前：
  item 为空则 fatal；
  flush_done 不是已知 0 则 fatal；
  Hint valid 不是已知 0/1 则 fatal；
  valid=0 时 payload 不是全 0 则 fatal；
  valid=1 时 payload 含未知值则 fatal；
每个 service 边界：
  先处理最后一拍 D.fire，允许它建立 GrantAck owner；
  再处理合法 E.fire；
  若 E.valid=1 但既没有 E.fire 也没有 owner，则 fatal；
  E.fire 的 sink 不是完全匹配的保存值时 fatal。
```

### 19.11 [IMPLEMENTATION_DELTA] C assembly 完成拍独占仲裁

最终 review 发现，首个 `C.fire` 建立两拍 C assembly 后，同一 service 拍仍可能继续进入 A arm 或
Probe 创建分支，使 A 的 pending D 与 C assembly 后续 `ReleaseAck` 争用唯一 D owner。

实现调整：处理完 `C.fire` 后，本轮仲裁增加独立空分支，不再 arm A 或启动 Probe；下一拍由
`c_assembly_owner` 最高优先级继续接受第二个 C beat。`try_start_probe()` 同时增加 pending D、
GrantAck、Probe、C assembly、A/C armed 和 stop gate，即使未来调用点变化也不能绕过 owner hazard。

文字伪代码：

```text
若本拍发生 C.fire：
  先推进当前 C owner；
  本拍剩余时间不再开放 A.ready，也不创建 B Probe；
  下一拍优先根据 c_assembly_owner 接受第二个 C beat。
尝试 Probe 前：
  任一 pending/owner/armed 状态存在或 global stop 已请求时直接返回；
  只有完全空闲时才采样 Probe 权重并选择 cached line。
```

### 19.12 [IMPLEMENTATION_DELTA] 四态采样链改为真正可达

早期实现虽然在 driver 使用 case equality，但 xaction 的四个 sideband 仍声明为二态 `bit`，并且
service loop 把 E.valid 直接折叠成二态，X/Z 会在检查前丢失。

实现调整：Hint/flush xaction 字段改为 `logic`，compare 使用 case inequality；service loop 先保存
A.valid、B.ready、C.valid、D.ready、E.valid 的四态 raw 值，非 reset 时任一 X/Z 直接 fatal，之后
才生成二态 fire 条件。E sink 继续在真实 E.fire 时用 case inequality 与保存 sink 对比。

文字伪代码：

```text
采样五个 channel 对端握手位的四态 raw 值；
若不在 reset 且任一 raw 不是已知 0/1，则 fatal；
通过 raw===1 生成二态 sampled 值；
只有 sampled valid/ready 与上一 item 同时满足时才形成 fire。
```

### 19.13 [IMPLEMENTATION_DELTA] generic E.ready 统一保持安全 0

原 delta 仅把 `DRV_1/DRV_RAND` 描述为 real-smoke 之外的边界，但这会让同一个 driver 仍存在无
GrantAck owner 的 E.ready producer。

实现调整：`drive_idle()` 的 generic 分支不再给 `e_ready` 写入 1、X 或随机值，并在所有模式末尾
统一写 `e_ready=0`；xaction 的默认随机约束也固定 `e_ready=0`。只有
`dcache_mem__access_base_sequence` 手工构造、且
`waiting_grant_ack=1` 的 item 可以把 E.ready 置 1。

### 19.14 [IMPLEMENTATION_DELTA] global stop 与 legacy testcase 完成握手

global stop 后禁止新 Probe；本拍已经由上一 item 的 A.ready 形成的 A.fire 仍按已握手事务完成。
stop 后新出现但没有 sampled fire 的 A.valid 直接 fatal，不能重新打开 A.ready。C-channel
Release/Probe reply 属于 DUT 已发出的 drain 输入，继续按 owner 接收。

为避免 legacy `tc_dispatch_real_smoke` 在 DCache responder 发送 terminal idle 前 drop phase
objection，公共同步包新增 `dcache_responder_done`。DCache body 启动时清零，完成全部 in-flight、发送
最后 safe idle 后置一；legacy testcase 等待该标志后才清 `dispatch_real_smoke_active` 并退出。
canonical `basicTest + memblock_dispatch_real_smoke_vseq` 仍使用既有 `wait fork` 作为完整 join。

### 19.15 [IMPLEMENTATION_DELTA] 验证 warning 口径修正

VCS compile 的功能结果为 0 error，但日志包含工具自身的 `LCA_FEATURES_ENABLED` warning；UVM 运行
日志也保留既有 resource/default-sequence warning。验收只记录 `TEST_PASS`、`UVM_ERROR=0` 和
`UVM_FATAL=0`，不再把结果写成“0 warning”。
