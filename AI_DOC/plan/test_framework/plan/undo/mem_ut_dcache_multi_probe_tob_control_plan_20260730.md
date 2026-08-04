# DCache 多 Probe、`Probe(toB)` 与轻量 L2 flush 专项 Plan

> **Alias 前置依赖**：随机 `Probe(toB/toN)` 的 target_cap、旧 alias B payload、alias conflict
> 与 line 删除/保留由
> `mem_ut_dcache_multi_probe_alias_state_plan_20260803.md` 统一定义。本 plan 只负责 Probe batch
> 随机选择、flush 调度和多 Probe 生命周期，不另建 alias 状态。

## 功能目标

扩展 DCache 轻量 L2 responder，使其能够按 runtime 配置随机发起多笔互不重复的 Probe，并支持
`Probe(toN)` 与 `Probe(toB)`；同时支持由 `io_outer_l2_flush_en` 触发的轻量 L2 flush。
本专项只扩展 responder 的 Probe 激励和 B/C 生命周期管理，不建立完整 L2 directory、权限参考模型
或多 client 一致性模型。

`Probe(toN)` 表示目标 DCache line 被失效；`Probe(toB)` 表示目标 line 降为共享但仍保留为有效
Probe 候选。`probe batch` 表示由一次随机 start 产生的一组 Probe 请求。`Probe service` 是所有
Probe policy 共用的请求提交、B-channel 发送和 C-channel 收敛功能。

## 参数功能

| 参数 | 默认值 | 功能 |
|---|---:|---|
| `MEMBLOCK_L2_PROBE_EN` | `0` | 总开关。仅为 `1` 时允许产生 Probe。 |
| `MEMBLOCK_L2_PROBE_PRE_START_WT` | `0` | 每个可启动周期随机启动 batch 的权重，范围 `0..10000`；不启动权重自动为 `10000 - 此值`。 |
| `MEMBLOCK_L2_PROBE_COUNT_ONE_WT` | `1` | 本 batch 发送 1 笔 Probe 的权重。 |
| `MEMBLOCK_L2_PROBE_COUNT_MID_WT` | `0` | 本 batch 从 `2..6` 随机选择次数的权重。 |
| `MEMBLOCK_L2_PROBE_COUNT_LARGE_WT` | `0` | 本 batch 从 `7..15` 随机选择次数的权重。 |
| `MEMBLOCK_L2_PROBE_TO_B_WT` | `0` | `Probe(toB)` 权重，范围 `0..10000`；`Probe(toN)` 权重自动为 `10000 - 此值`。 |

Probe record queue 容量固定为 16 笔，属于测试框架 compile-time 状态容量，不新增 plus 参数，也不允许
由 testcase runtime 配置覆盖。

三类 count weight 必须至少一项非零。按本专项已确认的新参数合同，旧
`MEMBLOCK_L2_PROBE_ENABLE_WT` 有意退出本 Probe 激励路径，并从 plus 配置入口删除；新的
`MEMBLOCK_L2_PROBE_EN` 负责总开关，`MEMBLOCK_L2_PROBE_PRE_START_WT` 负责每拍 batch 启动概率，
不保留两个概率门并存的兼容路径。这是参数迁移，不是遗漏旧参数。
所有参数沿用 `plus.sv -> seq_csr_common -> getter -> DCache responder` 的公共配置链路。

## 修改后行为

```text
Probe_EN 关闭：
  不产生新的 Probe。

Probe_EN 开启且当前可启动：
  每拍按 pre_start 权重决定是否开始新的 probe batch；
  start 命中后，按 count weight 选择 1、2..6 或 7..15 的 batch 目标数；
  从当前有效 Probe 记录表随机选择未被本 batch 选择过的 line；
  对每个选中 line，按 toB/toN 权重选择 Probe 参数；
  按 B-channel 可接受节奏逐笔发送，直至达到 batch 目标数、没有可选记录或 queue 达到 16 笔；
  queue 达到 16 笔时停止当前 batch 的后续选择，不报 fatal；已建立 record 继续等待 C response 收敛。

C-channel 回复：
  用 Probe 记录匹配对应 ProbeAck/ProbeAckData；
  完成匹配后清除 pending Probe 记录；
  Probe(toN) 删除该 line 的全局 Probe 候选记录；
  Probe(toB) 保留该 line 的全局 Probe 候选记录；
  同一 batch 内已选择的 line 不重复选择，toB line 只可在后续 batch 再次被选择。
```

## 共用 Probe Service 与 L2 flush

随机 Probe 和 L2 flush 均通过同一个 Probe service 提交请求。该 service 接收 line、`toN/toB`
和请求来源，负责建立 pending record、按 B-channel 可接受节奏发送，并在 C response 完成后按既有
匹配规则收敛。随机 policy 只负责选择候选和随机参数；flush policy 不参与随机权重选择。

新 Probe 的创建优先级统一为 `ALIAS_CONFLICT > CBO > FLUSH > RANDOM`，但不能抢占已有 C assembly、
pending D、已建立 B hold 等不可打断协议 owner。alias-resolution pending 不能被 flush 清除或覆盖；flush
snapshot 建立前必须等待该 pending 收敛。同一 physical line 已有未完成 Probe 时，CBO、flush 或随机 policy
只能等待，不能重复发 Probe。

三类 plan 只共享这一张“新建 Probe”仲裁表；已经进入 B hold、等待 C、C assembly、pending D 或 E
GrantAck 的 record 不重新参与该表。CBO 只在其 deferred context 的 token 与共享 record 匹配时负责
CBOAck 后处理，ALIAS_CONFLICT/FLUSH/RANDOM 由共享 Probe service 继续处理。

Probe queue 已达到 16 笔时，所有新 Probe policy 均停止创建；CBO/Alias/Flush 不得覆盖已有 record。
只有已有 record 收到合法 C response 并释放容量后，后续 policy 才能重新创建 Probe。

```text
IDLE：
  沿用现有 DCache responder 的 A/B/C/D/E 仲裁；允许随机 probe batch。

观察到 io_outer_l2_flush_en = 1：
  先处理上一拍已经形成的 A.fire；该请求已经被 responder 接受，必须正常 drain，不能撤销；
  从本拍输出起进入 DRAIN，停止随机 probe batch，并驱动 A.ready=0；
  sampled A.valid 若此前没有 A.fire，只保持等待，不建立新的 Grant/GrantData owner；
  保持既有 pending D、GrantAck、已发 B Probe 和 C response 生命周期的收敛。

DRAIN 完成：
  pending D、GrantAck、已建立但尚未 B.fire 的 Probe hold、已发 Probe、C response 和 C data assembly 均为空；
  一次性复制当前有效 cached_line_record 为本轮 flush snapshot；
  snapshot 只包含建立时已经存在的 ACTIVE record；flush 开始后新 GrantAck 建立的 alias 不加入本轮；
  进入 PROBE，A.ready 继续为 0。

PROBE：
  对 snapshot 中每条 line 以 FLUSH 来源调用 Probe service(line, toN)；
  B Probe 按 valid/ready 发送，等待并接收唯一匹配的 C ProbeAck/ProbeAckData；
  C.ready 只要对应 C owner 或多拍 data assembly 仍需收敛就继续按既有规则打开，不能随 A.ready 一同关闭；
  全部 snapshot record 完成后进入 DONE。

flush DRAIN 不撤销已建立的 alias/CBO/普通 Probe record；如果 B valid 已建立但尚未 B.fire，继续保持
B payload 稳定并等待 B.fire，再等待对应 C response 收敛。只有所有 Probe record 和
`c_assembly_probe_token` 均清空后才允许建立 flush snapshot。

DONE 且 io_outer_l2_flush_en 保持为 1：
  io_l2_flush_done=1；恢复既有正常 A.ready 仲裁和 DCache 请求接收；
  新 A.fire/new Grant 允许发生，但不回溯加入已经完成的 flush snapshot；
  随机 probe batch 仍暂停，直到本轮 flush 请求撤销。

DONE 状态采样到 io_outer_l2_flush_en=0：
  下一驱动拍 io_l2_flush_done 清零；
  只删除本轮 flush snapshot、flush Probe 选择记录和 flush 状态；
  不调用 clear_runtime_state()，不取消或清除任何正常 A/D/E、普通 Probe、C response 或 C data assembly owner；
  回到 IDLE，重新允许随机 probe batch；正常通道继续沿用已有仲裁和在途收敛。

DONE 前采样到 io_outer_l2_flush_en=0：
  报 uvm_fatal，不得把尚未完成的 flush 当作已取消而删除 snapshot 或停止等待 C reply；
  完整 L2 的 level 请求合同要求请求端在观察到 done 前保持 FLUSH_L2_ENABLE=1。
```

`io_outer_l2_flush_en` 由现有 `other_ctrl_agent` monitor 观察，`io_l2_flush_done` 继续由
DCache responder sideband 驱动。flush 中所有 Probe 固定为 `toN`；仅在对应 C response 完成后
删除全局候选记录，不能在 B.fire 时提前删除。

本专项通过 `cycle_xact.auto_inner_dcache_client_out_a_ready=0` 冻结新 Grant，而不是只在软件表中
忽略新请求；A 请求只有 A.fire 后才调用既有 `accept_dcache_a_request()` 并建立 D reply owner。对
`io_outer_l2_flush_en` 的监测是只读输入：它只推进 flush 状态，不直接改写正常通道 owner 或在途记录。
driver 的 `io_l2_flush_done` 校验相应改为“必须为已知值，且只有 flush owner 的 DONE 状态可驱动 1”，不得
继续把非零值一律 fatal。本专项的 flush 是面向独立 MemBlock 的轻量、保守 responder 行为，不模拟完整
CHI L2 在 `WAITMSHR` 阶段可能出现的 A-channel 资源仲裁窗口；C-channel Probe 回复始终必须收敛。

## 记录与收敛

- 每笔已发送 B Probe 都建立独立 pending record，复用 alias plan 定义的 `probe_token`、`probe_owner`、
  line、旧 alias、target_cap、batch 归属和 data reply 状态；不同 line 可以同时在途，同一 line 不能重复；
  总数不得超过固定容量 16。
- C `ProbeAck` 或完整 `ProbeAckData` 必须唯一匹配一个 pending record；未知、重复或歧义回复直接
  报错，不能静默删除其他记录。C payload 不携带内部 token 时，使用 line/source 等可观察字段筛选唯一
  record，再由该 record 的 token/owner 完成状态更新。
- `ProbeAckData` 的数据处理继续沿用 DCache responder 既有写回路径；本专项只改变其 owner 的匹配和
  Probe record 的清除规则。第一拍 C.fire 时保存 `c_assembly_probe_token`，第二拍 C.fire 必须匹配同一
  token；两拍之间不允许其他 C response 覆盖 assembly。两拍完整后若 `corrupt=1`，不写 shared
  overlay，并调用 alias plan 的公共状态更新将对应 `cached_line_record.data_valid=0`、报 `uvm_error`；
  随后仍按 toN/toB、owner 和 flush snapshot 规则收敛，不得因数据写回失败把 batch 或 flush 卡住。
- global stop 或 reset 必须停止新 batch，并等待已建立 Probe（包括尚未 B.fire 的 hold）的 response record
  收敛后退出。
- 从观察到 flush 到 `DONE` 之前，A.ready 必须为 0，因此不会出现新的 A.fire 或 Grant owner；
  已进入 `DONE` 后的新正常事务属于后续状态，不回溯加入已完成 flush。任一 line 的 flush Probe
  未完成前不得重复提交。
- `flush_en=0` 的清理只消费 flush-local 状态，不能重置 DCache responder 全局运行期状态；若 DONE
  状态已经允许正常 A.fire 后又收到请求撤销，该正常请求及其后续 D/E 生命周期仍必须按既有 owner 收敛。

## 保持不变的边界

- DCache `GrantData`、`GrantAck`、CBO、Hint、shared memory overlay 和 Uncache 逻辑不因本专项改变。
- B-channel 仍遵守 `valid/ready`；每个 Probe 在未握手时保持 payload 稳定。
- 同一 line 在前一笔 Probe 未完成时不得再次发 Probe；不同 line 的 C response 可按实际到达顺序匹配。
- 不增加动态 sink、完整 directory、`needData` 随机策略、Probe 错误注入或其他 client 的 Probe 模型。
- 本专项的 flush 只保证当前 `cached_line_record` 记录全部经 `toN` 收敛；不模拟完整 CoupledL2
  的 set/way 扫描、MSHR/snoop 全局仲裁、下游 CHI writeback 或其他 L2 client 的 flush 行为。
