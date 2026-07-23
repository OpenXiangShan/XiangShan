# mem_base_sequence.sv 源码分析

本文对应当前源码：

- mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv

## 1. 文件定位与职责边界

### 1.1 术语与抽象功能说明

| 术语 | 当前含义 | 代码落点 | 示例 |
|---|---|---|---|
| responder | 由 sequence 驱动、响应 DUT memory channel 的测试框架模型 | dcache_mem__access_base_sequence、sbuffer_mem_access_base_sequence | 不拥有主表或 pass/fail |
| service cycle | DCache responder 自己的逐拍计数 | service_cycle | 用于 response delay 和 Hint 排期 |
| armed snapshot | 看到 DUT valid 后保存、等待下一拍确认 fire 的请求快照 | armed_a_req_xact、armed_c_req_xact | 下一边界才确认 A/C fire |
| fire | valid 和 ready 在同一采样边界同时为 1 | a_fire、b_fire、c_fire、d_fire、e_fire | 只有 fire 后才能建立 pending 状态 |
| pending D | 已真实接受但尚未完成的 D response | pending_d_* | GrantData 两个 beat 共用一个 owner |
| GrantAck owner | Grant/GrantData 完成后等待 E.fire 的状态 | waiting_grant_ack、pending_grant_* | 只有 owner 存在时 e_ready=1 |
| cached line table | 已完成 GrantAck、可作为 Probe 候选的 64B line 影子表 | cached_alias_by_line | key 是 line 对齐地址，value 是 alias |
| Probe owner | 当前唯一的 B Probe 或其 C 回复生命周期 | pending_probe_b_valid、waiting_probe_c | B.fire 后等待 ProbeAck |
| C assembly | 收集 ProbeAckData/ReleaseData 两个 32B beat 的状态 | c_assembly_* | 收满后再写主存 |
| in-flight | 尚未完成握手或生命周期的状态 | pending/armed/owner 字段 | global stop 必须等待其归零 |

本文件提供三层对象：

1. mem_access_base_sequence：公共 sparse memory 后端。它按 1024-bit 地址片段保存 main_mem，
   并提供 lazy line、byte mask、corrupt/denied 和物理范围检查。
2. dcache_mem__access_base_sequence：挂在 DCache agent 上的 V2 coherent responder。当前实现是
   单 A/C response、单 Probe、固定 sink 0 的轻量模型。
3. sbuffer_mem_access_base_sequence：挂在 SBuffer agent 上的旧式单拍 responder。它复用公共
   memory 后端，但不复用 DCache 的 GrantAck/Probe 状态机。

这些 sequence 不生成 dispatch 主表，不更新 uid、ROB、LQ/SQ、pass/fail 或 terminal。

## 2. 调度关系和公共参数

tc_base.sv 把 DCache 和 SBuffer sequence 分别挂到对应 agent sequencer 的 main phase；real smoke
virtual sequence 也会在 real-smoke active 窗口启动它们。DCache responder 读取 DCache VIF、
common_data_transaction 的 global stop、reset_backend_done、L2 runtime 权重和 PADDR base/range。

参数数据流：

```text
env/plus.sv
  -> seq_csr_common::load_from_plus()
  -> seq_csr_common::validate_and_clamp()
  -> seq_csr_common getter
  -> dcache_mem__access_base_sequence
```

中文伪代码：公共 plus 定义并读取五个 L2 权重；seq_csr_common 保存并校验快照；DCache responder 只通过 getter 读取，不直接访问 plus 原始值。

DCache sequence 启动时把 get_paddr_base()/get_paddr_range() 注册到自身 main_mem_ranges。这只
约束 DCache 看到的物理地址，不限制主表的虚拟地址窗口。

## 3. DCache responder 调用链

### 3.1 body() 主循环

源码位置：mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:1305-1585。

抽象功能描述：body 是 DCache responder 的唯一调度入口，在每个 dcache_vif.drv_cb 边界采样
上一 item 的握手结果，推进 owner 状态，再提交下一拍 response item。它不依赖 DCache monitor
analysis port。

旧逻辑是看到 A.valid 后立即发送 A.ready，再用阻塞 for-loop 连续发送 D response。当前逻辑改为
fire 驱动的 service loop，同时处理 A/B/C/D/E、Hint、GrantAck、Probe 和 global stop drain。

```systemverilog
@(dcache_vif.drv_cb);
sampled_a_valid_raw = dcache_vif.drv_cb.auto_inner_dcache_client_out_a_valid;
sampled_c_valid_raw = dcache_vif.drv_cb.auto_inner_dcache_client_out_c_valid;
sampled_e_valid_raw = dcache_vif.drv_cb.auto_inner_dcache_client_out_e_valid;
if (!reset_active &&
    ((sampled_a_valid_raw !== 1'b0 && sampled_a_valid_raw !== 1'b1) ||
     (sampled_c_valid_raw !== 1'b0 && sampled_c_valid_raw !== 1'b1) ||
     (sampled_e_valid_raw !== 1'b0 && sampled_e_valid_raw !== 1'b1))) begin
    `uvm_fatal(get_type_name(), "DCache channel valid/ready sampled as X/Z outside reset")
end
sampled_a_valid = (sampled_a_valid_raw === 1'b1);
sampled_c_valid = (sampled_c_valid_raw === 1'b1);
a_fire = 1'b0;
b_fire = 1'b0;
c_fire = 1'b0;
d_fire = 1'b0;
e_fire = 1'b0;
build_dcache_idle_xaction(cycle_xact);
```

中文伪代码：等待 clocking sample 边界并保留 DUT 对端四态 raw 值；非 reset 时任一握手位为 X/Z
直接 fatal；随后转换成二态 sampled 值；清空本轮 fire 标志并构造全零基线 item；后续只用上一 item
output 与当前采样值确认真实 fire。

```systemverilog
if (last_cycle_valid && (last_cycle_xact != null)) begin
    a_fire = last_cycle_xact.auto_inner_dcache_client_out_a_ready && sampled_a_valid;
    b_fire = last_cycle_xact.auto_inner_dcache_client_out_b_valid && sampled_b_ready;
    c_fire = last_cycle_xact.auto_inner_dcache_client_out_c_ready && sampled_c_valid;
    d_fire = last_cycle_xact.auto_inner_dcache_client_out_d_valid && sampled_d_ready;
    e_fire = last_cycle_xact.auto_inner_dcache_client_out_e_ready && sampled_e_valid;
    if (d_fire) process_d_fire();
    if (e_fire) process_e_fire();
end
```

中文伪代码：将上一 item 的 valid/ready 与当前对端值相与；只有 fire 才推进 D/E 生命周期；valid 单独出现不会被当成完成。

body 的优先级为 reset、pending D、GrantAck、Probe B、C assembly、waiting Probe C、普通 C、
普通 A、空闲 Probe、Hint 和发送。若本轮已 c_fire，显式跳过后续 A/Probe 仲裁；若已 a_fire，
A arm 分支要求 !a_fire。这样首个 C data beat 建立 assembly 后不会被 A pending D 抢占。

### 3.2 reset、range 和 safe idle

源码位置：mem_base_sequence.sv:1339-1350、533-565。

抽象功能描述：初始化公共参数、物理 memory range 和 responder 状态；每拍用 known-zero item
作为默认输出，GrantAck owner 之外不开放 E。

```systemverilog
seq_csr_common::init();
clear_main_mem_ranges();
init_main_mem_range(mem_addr_t'(seq_csr_common::get_paddr_base()),
                    seq_csr_common::get_paddr_range());
check_l2_model_cfg();
clear_runtime_state(1'b1);
```

中文伪代码：初始化 runtime snapshot；清除旧 range；注册共享 PADDR 窗口；校验 L2 权重；清除 pending、owner、assembly、Hint 和 cached line map。

```systemverilog
rsp_xact.auto_inner_dcache_client_out_a_ready = 1'b0;
rsp_xact.auto_inner_dcache_client_out_b_valid = 1'b0;
rsp_xact.auto_inner_dcache_client_out_c_ready = 1'b0;
rsp_xact.auto_inner_dcache_client_out_d_valid = 1'b0;
rsp_xact.auto_inner_dcache_client_out_e_ready = 1'b0;
rsp_xact.io_l2_hint_valid = 1'b0;
rsp_xact.io_l2_flush_done = 1'b0;
rsp_xact.pre_pkt_gap = 0;
rsp_xact.post_pkt_gap = 0;
```

中文伪代码：清零所有 channel valid/ready、payload、Hint、flush 和 gap；只有 waiting_grant_ack 分支能把 e_ready 覆盖为 1。

reset 或 backend reset 未完成时，body 清除所有 in-flight state 和 cached line map，发送 safe idle 后
进入下一拍，不复用 reset 前 snapshot。

## 4. A request snapshot、分类和 pending D

### 4.1 capture 和稳定性检查

源码位置：mem_base_sequence.sv:567-649。

抽象功能描述：从 VIF 复制 A/C channel 完整字段，供下一边界的 fire 和稳定性检查使用；不建立
response owner。

中文伪代码：看到 valid 时复制 payload 并 arm ready；下一 drv_cb 边界确认 fire 后比较实际 payload；valid 消失则清 arm，不创建 pending。

### 4.2 accept_dcache_a_request()

源码位置：mem_base_sequence.sv:858-968。

抽象功能描述：只消费真实 A.fire 的快照，完成 size、line alignment、source、param、PADDR range
和 opcode 检查，并建立唯一 pending D。

```text
AcquireBlock:
  source 0..15；
  NtoB -> GrantData(toB)，NtoT/BtoT -> GrantData(toT)；
  读取 line_addr 和 line_addr+32 两个 32B beat；
  建立两拍 pending D，可按权重排一次 Hint。

AcquirePerm:
  source 0..15；
  只接受 NtoT/BtoT；
  建立单拍 Grant(toT)，固定 sink 0，不发 Hint。

CBOClean/CBOFlush/CBOInval:
  source 必须是 17；
  建立单拍 CBOAck；
  clean 保留 map，flush/inval 在 D.fire 后删除 map。

其它 coherent opcode:
  在 pending 建立前 fatal，不 fallback 为 AccessAckData。
```

中文伪代码：A.fire 后检查完整 64B line；按 opcode 建立 pending kind；只在本次 A.fire 采样 delay；未知或不支持 opcode 直接 fatal，不伪造其它 client response。

### 4.3 build_pending_d_xaction() 和 process_d_fire()

源码位置：mem_base_sequence.sv:970-1050。

抽象功能描述：build_pending_d_xaction 把 pending 状态转换为当前拍 D item，process_d_fire 只消费真实 D.fire 并推进 beat/owner。

中文伪代码：pending 到期时打开 D.valid，按 kind 填 Grant、GrantData、CBOAck 或 ReleaseAck；D.ready=0 时 payload 和 beat index 不变；GrantData 最后一拍或 Grant 完成后建立 GrantAck owner；CBOAck/ReleaseAck 完成后清 pending。

### 4.4 process_e_fire()

源码位置：mem_base_sequence.sv:1052-1067。

抽象功能描述：消费 GrantAck E.fire，确认 E 属于当前 owner，并在生命周期真正完成后插入 cached line table。

中文伪代码：主循环先处理最后一个 D.fire，使合法 Grant 建立 owner；若采样到 E.valid、没有形成
E.fire 且处理 D 后仍无 waiting_grant_ack，则 fatal；合法 E.fire 再用四态完全匹配检查 sink，未知或
不匹配都 fatal；匹配后记录 line/alias、清 owner；下一拍才重新开放 A/Probe。

## 5. delay、Hint 和 cached line table

### 5.1 sample_l2_response_delay()

源码位置：mem_base_sequence.sv:744-785。

抽象功能描述：按三个 runtime 权重选择类别，再在固定区间内选择具体 service cycle；一次 transaction
只采样一次。

中文伪代码：读取 getter；用 std::randomize/dist 抽类别；small 在 3..5、medium 在 6..15、
large 在 16..50 内抽 exact delay；返回值固定到 pending due cycle。

### 5.2 sample_hint_enable() 和 service_hint()

源码位置：mem_base_sequence.sv:787-807、1299-1309。

抽象功能描述：只对 AcquireBlock 的 GrantData 选择一次 Hint，并在 due cycle 输出单拍 valid。

中文伪代码：权重 0 关闭、100 每次命中、中间值用 dist；命中后保存 source[3:0]、isKeyword、
due；到期拍拉高 Hint 一拍，后续由 idle 基线清零；Grant、CBOAck、ReleaseAck 不产生 Hint。
io_l2_flush_done 没有功能模型，始终为 0。

### 5.3 record/remove/select cached line

源码位置：mem_base_sequence.sv:727-742、831-856。

抽象功能描述：维护 line 对齐地址到 alias 的轻量候选表，不复制主存 data，也不表达完整 coherence
状态。

中文伪代码：GrantAck.fire 插入；ProbeAck/Data、Release/Data、CBOFlush/Inval 完成时删除；CBOClean
保留；Probe 只从表中随机选择，表为空就跳过。

## 6. Probe 与 C-channel

### 6.1 try_start_probe()

源码位置：mem_base_sequence.sv:1284-1297。

抽象功能描述：只在未 global stop、没有 pending D、GrantAck、C assembly、B Probe、等待 C 或
A/C armed 的完全空闲窗口启动固定 Probe(toN)。helper 自身重复执行 owner gate，不依赖调用点位置。

中文伪代码：按 Probe 权重抽是否尝试；从 map 选 line/alias；置 B pending；B.fire 后进入
waiting_probe_c。

### 6.2 start_c_assembly() 和 consume_c_beat()

源码位置：mem_base_sequence.sv:1126-1282。

抽象功能描述：区分 ProbeAck/ProbeAckData 与 Release/ReleaseData，检查 header 稳定并收集多拍 data。

中文伪代码：ProbeAck 要求 owner 和 line 匹配并单拍删 map；ProbeAckData 建 Probe assembly；
Release 建 ReleaseAck pending；ReleaseData 建 Release assembly；opcode、line、source、size 或
param 在多拍中改变时 fatal。

### 6.3 complete_probe_c_assembly() 和 complete_release_c_assembly()

源码位置：mem_base_sequence.sv:1069-1124。

抽象功能描述：完整两拍 data 到达后执行主存写回、map 删除和 owner 清理；Release completion 还
建立 ReleaseAck pending D。

中文伪代码：无 corrupt 时两次全 mask store 更新 main_mem；有 corrupt 时跳过写回但仍结束协议；
Probe 清 waiting_probe_c；Release 清 assembly 后按 delay 建 ReleaseAck。

## 7. global stop 与 DCache driver

### 7.1 DCache body 退出

源码位置：mem_base_sequence.sv:1427-1469。

抽象功能描述：global stop 只请求 responder 排空，不允许立即退出。

退出必须同时满足 pending D、GrantAck、Probe B/C、C assembly、A/C armed 和当前 A/C valid 全部
为空；cached line map 非空不阻止退出。stop 后不再创建 Probe；新出现且未形成上一 item sampled
fire 的 A.valid 直接 fatal。满足条件后发送 safe idle、置 `dcache_responder_done=1` 并 break，否则
每拍继续 drain 并周期性 warning。

### 7.2 dcache_agent_agent_driver

源码位置：mem_ut/ver/ut/memblock/agent/dcache_agent_agent/src/dcache_agent_agent_driver.sv:51-255。

抽象功能描述：driver 是 sequence item 到 VIF 的唯一搬运者，不判断 fire、不维护 response owner。

```systemverilog
req = null;
seq_item_port.get_next_item(req);
if (req == null) begin
    `uvm_fatal(get_type_name(), "get_next_item returned a null DCache item")
end
send_pkt(req);
seq_item_port.item_done();
```

中文伪代码：清旧句柄；阻塞获取新 item；空 item fatal；立即写 clocking output；item_done 后等待下一 item，不 hold 或重复上一 item。

send_pkt 要求 pre/post gap 为 0，并在第一次 VIF 赋值前检查 Hint/flush 合同：null item、非已知 0 的
flush、未知 Hint valid、valid=0 时非零 payload、valid=1 时未知 payload均 fatal。sideband xaction
字段使用四态 `logic`；generic `drive_idle` 不论 drv_mode 为何，最后都把四个 sideband 和 E.ready
写为 0。只有专用 responder item 能提供合法 Hint，且 E.ready 只由 GrantAck owner 打开。

## 8. SBuffer responder 当前边界

### 8.1 sbuffer_mem_access_base_sequence::body()

源码位置：mem_base_sequence.sv:1724-1768。

抽象功能描述：SBuffer 仍使用单拍、阻塞式 A-to-D 模型；global stop 后，
仅当没有尚未接受的 A request 才发送 safe idle 并退出。

中文伪代码：检查 stop/reset；发送 idle；看到 A.valid 后采样并发送 A.ready；调用
sbuffer_mem_access_xaction 构造单拍 D；持续发送到 D.ready；完成后回到循环。

本专项没有把 DCache 的 GrantAck、Probe、Hint 或 lockstep 状态复制到 SBuffer。

### 8.2 sbuffer_mem_access_xaction() 和 memory task

源码位置：mem_base_sequence.sv:1649-1722。

抽象功能描述：按 8B 对齐地址和 8-bit mask 访问公共 main_mem，store 返回 ack，load 返回 64-bit
data，不修改 DCache cached line table。

中文伪代码：判断 store；映射到 8B beat；调用 main_mem_access_task；构造 D valid、opcode、
source、size、denied/corrupt/data；等待 D.ready。

## 9. 与其它 flow 的边界

DCache/SBuffer responder 只负责 memory channel response 和自身自然退出：

- 不生成主表或地址；
- 不分配/释放 LQ/SQ；
- 不写 status_by_uid；
- 不处理 redirect/replay/ROB commit/deq；
- 不把 DCache monitor analysis port 当作状态 owner；
- 不把 cached line table 当作 RM/scoreboard。

virtual_sequence_unified_dispatch_flow.md 负责 background responder 启动/join；本文件负责
DCache/SBuffer 内部 response 生命周期。修改 service loop、driver 时序或退出条件时，必须同步
对应 flow、接口分析和总控 plan。

## 10. 修改类型总结

相对旧实现：

- interface/xaction/driver 的四个 V2 L2 sideband 增加 known-zero 和发送前 fail-fast；
- plus/seq_csr_common/cfg 增加五个 L2 runtime 权重；
- DCache A-to-D 阻塞 loop 改为 fire 驱动的 service loop；
- 新增 Grant/GrantData/CBOAck/ReleaseAck、delay、GrantAck/E、cached line、Probe、C assembly；
- driver 改为 get_next_item 后立即发送，消除旧 hold 的重复 beat 风险；
- DCache 不再保留未使用的 `default_pre_pkt_gap`、`default_post_pkt_gap` 和 `sampled_delay` 残留，
  response 时间唯一由 `service_cycle + sample_l2_response_delay()` 推导；SBuffer 的单拍 gap 逻辑不变；
- sideband transaction 和 channel 采样保留四态并 fail-fast；无 GrantAck owner 的 E.valid 和未知
  E sink 直接 fatal；generic E.ready 恒为 0；
- C assembly fire 后独占本拍，Probe helper 自带 owner/stop gate；
- global stop 改为等待 DCache 自身 in-flight 收敛并发布 `dcache_responder_done`；
- SBuffer 保留独立单拍响应主体，不受 DCache coherent 状态机影响。
