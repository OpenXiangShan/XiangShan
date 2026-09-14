# PrefetchBuffer 冲突与一致性规范

本文规定 PrefetchBuffer（PB）与 MainPipe、LoadPipe、MissQueue、WritebackQueue（WBQueue）和 ProbeQueue 之间的冲突检查、所有权交接及同拍事件处理。PB 只负责自身 entry 的物理匹配和生命周期；原有模块继续负责各自流水线、MSHR、Probe 和写回事务的冲突。

## 1. 冲突责任分层

| 冲突类别 | PB 内部处理 | 原有模块继续负责 |
| --- | --- | --- |
| 物理块重复 | `matchBlock` 匹配 Resident、Reserved、锁定和 Poison，并保证结果最多 one-hot；Released、Invalid 不参与查询 | DCache tag/meta 与 MSHR 的物理块唯一性 |
| Entry 分配与回填 | 只有 Invalid 可分配；Reserved 回填必须匹配 entryId 和 missEntryId；不可覆盖 Resident、锁定或 Released | MSHR 决定 PB 分配、合并或改回 DCache；Grant/GrantAck 顺序 |
| PB Load 读授权 | S1 只授权 Resident、非坏数据、干净 B/T；Reserved 不授权，锁定或 Poison 返回 retry；S2 用已寄存 entryId 读数据 | LoadPipe 的并行查询、replay/nack、kill、flush 和 LSU 响应 |
| PB Probe/Store/Move 接管 | Probe 在 S0、Store 在 S1、Move 在 MainPipe S0 实际 fire 后建立锁定；锁定项禁止新 Load 和其他接管 | MainPipe 单入口仲裁及 `s1_s0_set_conflict` |
| PB Release 交接 | 只有 `releaseReq.fire` 才进入 Released；未握手前可被 Probe/Store 撤销，数据和权限保留 | WBQueue pending buffer 和 WritebackEntry 的同块排序 |
| 在途 PB Release | WBQueue 的两个 pending 地址参与 `block_conflict` 与 `block_miss_req` | MainPipe S0 不因 pending 阻塞；Probe 可进入但 S3 写回等待 WBQueue；新 miss 在入口阻挡 |
| MSHR refill 与 Probe | Reserved 期间不接受 PB Probe 接管 | MissQueue `probe.block` 从 Grant 窗口持续阻挡同块 Probe，直到 refill 完成 |
| victim replacement | PB 不持有 DCache way；Move 交给 MainPipe 选择 way/victim | MissQueue `replace.block` 与 MainPipe S3 写端口/WBQueue 等待 |

## 2. PB 本地匹配与生命周期

PB 的物理匹配由统一的 `matchBlock` 逻辑产生 one-hot 结果，并断言至多一项；后续用 one-hot 掩码和 `.orR` 判断，避免对 entryId 额外优先译码。匹配条件包含块对齐物理地址，并排除 Released、Invalid；Reserved、Resident、Probe/Move 锁定和 Poison 仍表示 PB 持有该物理块。

状态规则如下：

1. 分配只选择 Invalid entry，建立 Reserved。
2. 回填只接受 entryId、missEntryId 均匹配的 Reserved entry。
3. `releaseReq.fire` 是交给 WBQueue 的唯一交接点，随后进入一拍 Released。
4. Released 不可被查询授权、重新分配或 refill 覆盖，下一沿才转 Invalid。
5. Probe/Move 完成或 abort 只能作用于自己锁定的 entry，旧 entryId 不能影响复用后的分配。

组合逻辑使用沿前状态，寄存器在时钟沿统一更新。`nextMeta` 的防御性顺序为：分配、取消、回填、Release、Released 窗口结束、Probe/Move 完成、Move abort、Probe 锁定、Store/Move 锁定。`entryUsedNow` 和 Store alias mismatch 只更新元数据，不取得所有权。

## 3. Load 冲突

LoadPipe 在 S1 以物理地址查询 PB。Resident、干净 B/T 且数据可用才 hit；Reserved 返回 miss；Probe/Move 锁定或 Poison 返回 retry。S1 授权后，S2 使用已寄存 entryId 和块内偏移读 16B，不重新检查 live 状态。

授权截止点是 S1。因此 S1 命中后同拍发生 Probe、Store、Move 或 Release，已授权 Load 仍可在 S2 完成；接管和失效逻辑不能清空或覆盖该数据。Released 期间迟到的 `s2_use` 只消费接口，不恢复状态或更新统计。

多个 Load lane 同拍命中同一 Resident 时，各 lane 独立返回同一数据，`entryUsedNow` 按 entry OR 合并。对锁定项的新查询必须等待对应操作完成。

## 4. MainPipe Probe、Store、Move 冲突

MainPipe S0 只接受一个请求，并用 `set_conflict` 检查 S1/S2/S3 的同 set 冲突；同一物理块的有效请求也必须阻挡。PB 只接收实际进入流水线的通知：Probe 在 S0、Store 在 S1、Move 在 MainPipe S0 `fire` 后建立锁定。

Move 在等待 S0 仲裁期间只是候选，不阻止 Probe。Probe 已占用同一 entry 时 Move 候选被排除；Move 已 fire 后，Probe/Store 无法再锁定。三者同块同拍不能同时进入 MainPipe，由单入口仲裁和同 set/物理块冲突保证。

Store 命中 PB 时先完成 PB→DCache 搬运，再 replay；PB 不接收 Store 数据。S1 锁定后，S2/S3 因 way、B→T、写端口或 victim WBQ 不可用可等待或 abort。提交沿发送 `moveDone` 后 PB 才 Invalid；abort 恢复 Resident 或转 Poison。

## 5. Probe 冲突与响应

MissQueue `probe.block` 从第一拍 Grant 握手起阻挡同块 Probe，覆盖 PB 未分配、Reserved、数据收齐尚未 fill 的窗口。refill 握手沿仍阻挡，下一沿 PB Resident 且 MSHR 完成记录后解除；该保护不依赖 PB entry 或 alias。

Probe 进入 MainPipe 后，PB 在 S0 注册 `s1_probeResp`：`locked` 表示取得一致性责任，`entryId` 仅 locked 有效，`coh` 返回原 B/T。反压时保持结果，S3 不重新查询 live entry。PB 为干净 B/T 且 `needData=0`，Probe 不读取整行数据。

Probe S0 与 Release 的边界：

| 情况 | PB 结果 | Probe 结果 |
| --- | --- | --- |
| Release 已 fire | 块已离开 PB | `locked=false`，不发送 `probeDone` |
| Release fire 与 Probe 同拍 | Release 胜出，进入 Released | `locked=false` |
| Release 尚未 fire，Probe 被接受 | 撤销待释放意图并锁给 Probe | `locked=true`，返回原 B/T |
| 块不存在 | 无 entry 操作 | `locked=false`，`coh=Nothing` |

Probe 锁定后禁止同块 Release。只有 WBQueue 接收 ProbeAck 的 S3 事件才能发送一次 `probeDone(entryId)`；WBQueue 未 ready 就保持锁定。`locked=false` 时严禁 probeDone，即使 ProbeAck 已生成也不能按旧 entryId 清除新分配。

## 6. Release 与 WBQueue 冲突

PB Release 通过 WBQueue 独立入口进入两个 entry 的 registered pending buffer，不再与 MainPipe WB 在 Wrapper 外部仲裁。pending 槽保存完整 WritebackReq 和物理块地址，直到真正分配 WritebackEntry。

两个 pending 地址必须同时参与 `block_conflict` 和 `block_miss_req`：同块 WB 不得重复进入 WritebackEntry，同块新 MSHR miss 必须等待 pending/active 写回事务结束；MainPipe S0 不因 pending 阻塞，Probe 可进入但其 S3 写回等待 WBQueue。

同拍 PB Release 入 pending 与 MainPipe 同块 WB 时，WBQueue 只允许一个请求进入 WritebackEntry，输入保护禁止双重交接。pending 头被 active entry 阻塞时，不得垄断无关 MainPipe WB；调度器应在 pending 可发送时提供公平机会。PB entry 在 `pbRelease.fire` 前仍可按协议被 Probe/Store 撤销。

## 7. 同拍事件表

| 同拍事件 | 处理结果 | 正确性依据 |
| --- | --- | --- |
| 多个 Load 读取同一 Resident | 各 lane 返回同一数据，used 合并 | one-hot 匹配与固定 S2 数据访问 |
| Load S1 命中，同时 Probe/Store 锁定 | Load S2 继续使用已授权数据 | S1 授权截止；锁定不覆盖 data |
| Load S2 use 与 Release fire | entry 进入 Released，use 保留旧事务语义 | `entryUsedNow` 先记录；Released 保留一拍 |
| Probe 与 Release 同块 | Release fire 优先；Probe 不锁定 | 唯一 Release 交接点 |
| Store 与 Release 同块 | Store 不锁定并 replay；Release 保留写回责任 | release-fire 屏蔽 Store lock |
| Probe 与 Store/Move 同块 | 仅一个进入 MainPipe；其余重试/取消 | 单入口仲裁与 set/块冲突 |
| Move 与 Release 不同 entry | 可以同拍分别处理 | 独立候选寄存器与调度 |
| Move 与 Probe/Store 同 entry | 先 fire 者取得锁定 | MainPipe S0/S1 冲突与 PB lock |
| Fill 与 Probe 同块 | Fill 前被 MQ 阻挡；fill 后下一拍可查 | `probe.block` 与 Reserved |
| Cancel 与 Fill 同 entry | 合法协议不允许同拍；非法时 cancel 防御性优先 | MSHR 单一完成路径 |
| pending Release 与 Probe | Probe 可进 S0，S3 写回等待 WBQ | pending 地址进入 WBQ 冲突表 |
| pending Release 与 MainPipe WB | WBQ 内部只接收一个 | 单一 WritebackEntry 分配点 |

发生冲突时，优先保证一个物理块只有一个可追踪的一致性所有者。任何模块都不能因本地目录暂时 miss、entry 尚未分配或 pending 尚未出队而提前放行同块 miss、Probe 或第二次写回。
