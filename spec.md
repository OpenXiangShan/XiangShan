# PrefetchBuffer 设计规格

本文定义 PrefetchBuffer（PB）的实现规范，包括预取分配与回填、MainPipe 请求处理、LoadPipe 接口、冲突处理和 PB 核心模块。跨模块冲突的完整规则见 [conflict.md](conflict.md)。

PB 属于 L1 DCache 内部，可持有干净的 B（Branch）或 T（Trunk）权限。数据修改必须在搬入 DCache 后进行，PB 不接收 Store 数据写入。本版面向标量 Load 与现有 MainPipe Store 路径，不扩展 AMO、向量和独立 StorePipe。

除非另有说明，S0/S1/S2/S3 指对应模块的本地流水级；时序表中的 C0/C1 等才表示全局时钟拍。握手指 valid 与 ready 同时成立，状态在该拍结束的时钟沿更新。

PB 容量为 16 个 CacheLine。

上述 16 项是当前 DefaultConfig 和本轮验证使用的容量；容量仍由
`DCacheParameters.nPBEntries` 参数化。

接口迁移状态：PB、MainPipe、LoadPipe、MissQueue、Wrapper 已适配本规格接口，删除旧 claim/readReq/finish/publish 等调用，不保留兼容包装。生产代码编译与分批定向测试已通过；各测试批次、孤立性能计数器测试、统计打印核对和已知多顶层 BoringUtils 测试限制记录见 `.planning/pb-refactor/progress.md` 与 `findings.md`。本轮按用户要求不运行 CI/CR；以上结果不代表工作负载验证或时序收敛。

## 冲突与一致性

PB 与 MainPipe、LoadPipe、MissQueue、WBQueue、ProbeQueue 的冲突责任、同拍优先级和边界时序统一记录在 [conflict.md](conflict.md)。以下仅保留关键摘要，详细规则以 `conflict.md` 为准。

| MSHR refill 与 Probe | PB Reserved 期间不接受 Probe 接管；对应 MSHR 的 `probe.block` 从规定的 Grant 窗口持续阻塞同块 Probe | ProbeQueue 在 TileLink B 边界阻塞，不分配 ProbeEntry；回填完成后才允许 MainPipe 查询 PB |
| victim replacement | PB 不持有 DCache way；Move 交给 MainPipe 选择 way 和 victim | MissQueue `replace.block` 处理 MSHR 与 victim 冲突，MainPipe S3 等待写端口/WBQueue |

### 同拍事件与优先关系

PB 组合逻辑使用沿前状态，寄存器在时钟沿统一更新。对同一个 entry，设计上禁止多个所有权事件同时成立；`nextMeta` 的状态转换顺序仍作为防御性优先级：分配、取消、回填、Release、Released 窗口结束、Probe/Move 完成、Move abort、Probe 锁定、Store/Move 锁定。`entryUsedNow` 和 Store alias mismatch 只更新元数据，不取得所有权；状态转换不能覆盖这些标记的语义。

下表列出允许或必须依赖原有机制解决的边界情况：

| 同拍情况 | 处理结果 | 正确性依据 |
| --- | --- | --- |
| 多个 Load lane 读取同一 Resident entry | 各 lane 独立返回同一数据；`entryUsedNow` 按 entry OR 合并 | PB one-hot 物理匹配；数据只在 Move/Probe/Release 完成沿改变或失效 |
| Load 已在 S1 命中，随后 Probe/Store 在同拍锁定 | Load 的 S2 仍按寄存 entryId 读取；锁定不覆盖数据 | Load 授权截止在 S1；PB lock 不修改 data |
| Load S2 use 与 Release fire 同拍 | 状态转 Released，同时保留该次 use 的 used/统计标记；下一拍允许沿前授权 Load 完成 | `entryUsedNow` 先记录使用，Released 保留一拍；WBQ 接管后才继续释放 |
| Probe S0 与 Release fire 命中同一 entry | Release 胜出，Probe 返回 `locked=false` 和 Nothing，不发送 `probeDone` | `probeS0Lock` 显式屏蔽同拍 `relFire`；Release 交接点唯一 |
| Store S1 与 Release fire 命中同一 entry | Store 不锁定，返回失败/retry；Release 保留该块的写回所有权 | `storeS1Lock` 和 alias-mismatch 路径显式屏蔽同拍 `relFire` |
| Probe 与 Store/Move 同块同拍请求 | 不在 PB 内组合互相反馈；Probe 优先，其他请求不在 MainPipe S0 fire | MainPipe 单入口仲裁和 set/物理块冲突先行阻挡，PB 只接收实际 fire |
| Move 与 Release 作用于不同 entry | 可以同拍分别 fire，各自进入独立状态 | Move/Release 独立候选寄存器和 RR 仲裁；entryId 不同 |
| Move 与 Probe/Store 作用于同一 entry | Move 只有在 MainPipe S0 fire 后才锁定；Probe/Store 已先占用时 move 候选撤销或不再入选 | `entryCanSelect` 排除锁定项；MainPipe S0 冲突避免同拍 fire |
| Fill 与 Probe 查询同一块 | Fill 前 entry 为 Reserved，Probe 不取得 PB 锁；Fill fire 后下一拍才可按 Resident 查询 | PB `probeS0MatchOH` 排除 Reserved；MissQueue `probe.block` 覆盖 refill 窗口 |
| Cancel 与 Fill 作用于同一 Reserved entry | MissQueue 协议禁止同拍发生；若出现非法组合，取消优先，不能把数据发布成 Resident | MSHR 只发送一次 cancel 或 refill；PB 状态优先级提供防御性结果 |
| PB Release 已在 pending buffer，Probe 到达 MainPipe S0 | Probe 允许进入 S0，不读取 PB；到 S3 生成 WB 请求时等待同块 pending/active WB | pending 地址参与 WBQueue `block_conflict`；不增加 S0 组合阻塞 |
| PB Release 入 pending buffer 与 MainPipe 同块 WB 请求同拍 | WBQueue 内部只允许一个请求进入 WritebackEntry；同块入队由输入保护避免双重交接 | PB release 不再经过 Wrapper 仲裁；WBQueue 是唯一 WB Entry 分配点 |

PB 的 `entryLocked` 只表示已经取得 Probe 或 Move 所有权，不等同于“有数据可读”；`entryReadable` 只表示普通 Load 可以取数。Released 是一次 Release 交接后的退休窗口，不能重新授权新查询、分配或接管。这样 PB 的状态检查与原有五类机制形成分层保护：MainPipe 管流水冲突，MissQueue 管 MSHR/refill/probe 窗口，WBQueue 管写回地址唯一性，PB 管自身 entry 的物理匹配和生命周期。

## prefetchReq

prefetchReq 经过 MainPipe，同时查询 DCache 和 PB。需要发起 miss 时，进入 MissQueue 进行分配、合并或拒绝判断。

只有被判定为独立预取、准备回填 PB 的新 MSHR 才申请 PB entry。MissQueue 决定申请资格，PB 决定可分配的 entry；成功分配后，该 entry 进入 Reserved，记录 MSHR id、物理块地址、vaddr/alias 和预取来源。

### MSHR 先启动、PB 后分配

MSHR 的分配和 L2 请求发起不以取得 PB entry 为前提。满足原有 MSHR、数据接收资源及总线条件后，即可接纳预取并启动访存；同时尽早申请 PB entry，不必等 L2 返回后才申请。

MSHR 分别记录“回填目的地为 PB”和“已取得 PB 预留”两个条件。未取得预留时，entryId 无效，不能向 PB 发送 refillReq 或 cancelReq。申请可跨拍保持，直到分配成功，或该事务合并转为 DCache 回填等原因取消申请；同一 MSHR 最多持有一个 PB 预留。

| 数据收齐 | 已取得 PB 预留 | 行为 |
| --- | --- | --- |
| 否 | 否 | 接收 L2 返回，同时继续申请 PB entry |
| 否 | 是 | PB 保持 Reserved，等待整行数据 |
| 是 | 否 | 整行数据和权限/错误信息保留在 MQ，继续申请，不发起任何 refill 请求 |
| 是 | 是 | 可以参与 PB fill 仲裁 |

PB 分配成功的时钟沿建立 Reserved，最早下一拍参与 fill，不在同项上合并分配与回填两个状态转换。PB 满时通过正常维护回收，不能直接覆盖 Resident、Reserved 或 锁定状态，也不自动改为回填 DCache。

未取得 PB entry 不得单独阻止已发出事务的 Grant 接收或正常 GrantAck。MQ 必须保留接收数据所需资源；等待 PB 的 MSHR 不占用 MainPipe，不阻止 PB fill 仲裁、维护、Probe 完成及 WBQueue 推进。多个等待申请采用公平仲裁，重复选中同一 MSHR 不能产生重复预留。MSHR 仅在预留、数据和通知责任均已正确处理后才能释放或复用。

## prefetchRefill

### 合并与回填目的地

已建立的、准备回填 PB 的预取 MSHR 不接受后续 prefetchReq 或 Store 合并。同物理块且 alias 匹配的 demand Load，只能在第一拍 Grant 握手之前合并；第一拍 Grant 握手当拍不允许合并。

Load 合并成功后，MSHR 取消回填 PB 的标记。若尚未取得 PB entry，只取消申请，不发送 cancelReq；若已取得，则由所属 MSHR 的 cancelReq 端口发送 entryId，将对应 Reserved entry 释放。合并转 DCache 与新的 PB 分配确认同拍竞争时，合并优先，禁止该拍建立新的 PB 预留。

原 MSHR 和已发出的 L2 事务保留，后续走正常 DCache 回填，不再向 PB 发起 fill。同拍请求压缩与 MSHR 建立前的合并仍按 MissQueue 原有规则处理，不能与这里的后续合并规则混淆。上述合并窗口与是否取得 PB entry 无关；即使 PB 仍未分配，第一拍 Grant 握手当拍及之后也不接受该 demand Load 合并。

### 直接回填 PB

仍以 PB 为目的地的 MSHR，必须同时满足“整条缓存块已收齐”和“已取得 PB Reserved entry”，才能通过仲裁直接回填 PB，不再发送 MainPipe refill 请求。仲裁选中的 refillReq 在背压期间保持 entryId、MSHR id、完整数据、权限和错误信息稳定；一次回填只能被接受一次。本版对 denied/corrupt 结果也沿用取得预留后握手的完成路径。地址、vaddr 和预取来源已在分配时保存在 PB，不重复传递。

PB 验证目标项仍为对应的 Reserved entry，检查 entryId 和 MSHR id；未 denied 的回填还必须具有干净 B/T 权限。正常回填在该沿写入原始数据并执行 Reserved → Resident；denied 执行 Reserved → Invalid；L2 返回 corrupt 且未 denied 执行 Reserved → Poison。PB SRAM 和 metadata 假定永远可靠，不执行本地 ECC/parity 检查。

refillReq.fire 是唯一的 PB 回填提交事件，握手沿完成数据和状态更新，不另设 publish 或 fillDone。MissQueue 根据实际 PB 接口的 fire 与 missEntryId，在该沿寄存回填已完成；入回填队列不算完成。PB 直填 MSHR 仅在 GrantAck 已发送且回填已提交后释放，不能等待不存在的 MainPipe 完成响应。转为 DCache 回填的 MSHR 保留原有完成条件。

### Probe 保护窗口

| 时段 | 数据与状态 | Probe 处理 |
| --- | --- | --- |
| 第一拍 Grant 握手起，回填前 | 数据由 MQ 接收；PB 可能尚未分配，也可能为 Reserved | MQ 阻挡同物理块 Probe |
| refillReq.fire 当拍 | 沿前 Reserved，沿上写入并更新状态 | MQ 仍保持阻挡，禁止组合使用 fire 提前放行 |
| 正常回填下一拍 | PB 为 Resident，MQ 已寄存回填完成 | MQ 解除该 PB 事务的阻挡，MainPipe 可查询并接管 PB |

MQ 的 PB owner 使用 MSHR 自身的物理块地址产生，不依赖是否已取得 PB 预留。即使数据已经返回、PB 仍无空位，也必须持续保护该块；不能因 PB 目录未命中而回复不存在该所有权。

错误结果也通过同一 refillReq.fire 提交并由 MQ 记录完成：denied 后无 PB 数据所有权，Poison 由 PB 的错误清理路径接管。第一拍 Grant 之前不因等待 PB 或 Reserved 单独施加本窗口的阻挡，继续遵循原有 L2/MQ 事务排序约定。

## storeReq

Store 首次命中 PB 后，先把 PB 原始数据和干净 B/T 权限搬入 DCache，随后 replay，实际写入由后续正常 Store 流程完成。PB 不接收 Store 字节数据或 mask，即使已有 T 权限也不在本次搬运中修改数据。

Store 直接从 S1 用 `s1_paddr` 查询，并提供 `s1_alias = get_alias(s1_req.vaddr)`，不向 PB 发送 S0 虚拟地址。`s1_hit` 表示本次普通查询至少可读，不表示仅有物理块存在。MainPipe 确认本次是可执行的 Store、DCache 未命中且无 tag 错误后，置同拍 Bool `s1_storeReq` 申请锁定；地址和 alias 来自同次查询，Store replay 标识仅由 MainPipe 保存，不传给 PB，也不往返传输查询 token。PB 内部检查物理身份、状态、alias 和冲突。

`s1_paddr.fire && s1_storeReq` 接收一次尝试。成功在该沿 Resident → moveLocked，并为 S2 保存 entryId、权限和其他元数据；整行 data 在 S2 通过该 entryId 直接读取。失败不取得所有权，但仍返回 `s2_storeResp.bits=false`。同拍 `s2_dataResp` 给出有效数据或 retry/错误状态，两者统一由 `s2_dataResp.fire` 消费，MainPipe 不能将“暂不可读”直接当普通 miss。Reserved 仍遵守 MQ 合并与保护窗口，不额外建立第二份所有权。

S2 锁定成功后需要选择 DCache 目标 way、处理替换。替换冲突、目标 way 的 B→T 冲突、数据/标签错误等导致取消时，发送一次 `s2_moveAbort(entryId, dataBad)`。正常取消恢复 Resident 并保留搬运意图，数据错误转 Poison。alias 不匹配时不锁定、不安装到错误 set；PB 根据本次 Store 查询记录按其保存 vaddr 搬运的意图，Store replay 等待后续重试。

进入 S3 后等待所有必要写端口 ready；存在 victim 时还须等待 WBQueue 接收 victim 的释放请求，不等待 ReleaseAck。实际安装提交时发送 `s3_moveDone(entryId)`，同沿完成 DCache 发布和 PB invalid，不能提前完成或释放。

Store 响应/replay 的责任由 MainPipe 持有：PB 所有权导致重试、S2 取消或成功搬运后都须准确返回一次响应，不能把待回复 Store credit 隐藏到已解锁 PB entry 中等待 Probe 或未来 move 返回。普通 PB miss（success=false 且 retry=false）继续走原有 MQ 流程。新接口无 PB→MainPipe 的异步 storeReplay 端口，该责任已迁回 MainPipe。

### Scheme B: S3-serialized Store completion

`StoreRespQueue` is removed. S3 is the only completion outlet for a Store transaction:

- A normal S3 Store hit or a successful PB-to-DCache move produces its completion through the S3 response slot.
- If an S2 Store must terminate through MissQueue admission, MissQueue replay, B→T failure, PB retry, or PB move abort, MainPipe does not emit the external response directly from S2. It waits until the S3 response slot is available, advances the request into an `s3_response_only` cycle, and emits the response from the registered S3-side response path.
- `s3_response_only` performs no tag/meta/data write, victim writeback, access-bit update, replacement update, PB completion, or other cache-side effect. It carries the original Store ID and the replay/miss result captured at S2.
- A normal S3 Store completion has priority over an S2 terminal response. When S3 is completing a normal Store, the S2 Store remains stalled; this prevents two Store responses from being produced in one cycle.
- Store MissQueue valid is also gated by the S3 response-slot availability. Thus MissQueue admission and the corresponding terminal Store response cannot be separated by an untracked queue entry.

The registered response slot emits at most one Store response per cycle. This adds at least one cycle of response latency to an S2-terminal Store, but removes the PB-capacity-coupled credit queue and keeps response identity entirely in MainPipe.

## probeReq

MQ 的保护窗口不变：准备回填 PB 的 MSHR 从第一拍 Grant 握手当拍起，到 PB fill 握手当拍为止阻挡同物理块 Probe，不要求 alias 匹配或已取得 PB entry。ProbeQueue 入口及 MainPipe 的 MQ owner 检查必须保留，覆盖已排队 Probe。普通 DCache 回填沿用原规则。

### S0 单向通知，S1 返回结果

MainPipe 在 Probe 实际进入流水线时发出 `s0_probeReq: Valid[paddr]`，valid 恰好等于该 Probe 的 `s0_fire`；不是候选 valid，也不是只被仲裁选中。接口没有独立 ready，PB 不再向 MainPipe S0 返回 matchOH、takeableOH、busy 或完整目录以决定本拍接收。物理匹配/one-hot 选择留在 PB 内部，不能因此宣称没有组合逻辑或已满足 STA。

PB 在 S0 接收通知的时钟沿，按物理块匹配和本拍事件锁定 entry。S1 返回注册的 `s1_probeResp`：

| 字段 | 定义 |
| --- | --- |
| locked | 本次 Probe 是否取得该 PB entry 的一致性处理责任；不是可读命中 |
| coh | locked 时为原干净 B/T 权限，否则 Nothing |
| entryId | 仅 locked 时有效；保持锁定到本次 probeDone，不能复用 |

`s1_probeResp.valid` 区分“有响应但未锁定”和“没有本次 Probe”。S1 阻塞时保持整份结果；MainPipe 必须保存这次结果传至 S3，不能在 S3 重新查询 live entry。Poison 虽不能普通读取，但持有权限时仍需要被 Probe 接管。

### 与 MainPipe 在途请求互斥

MainPipe S0 检查 S1/S2/S3：同 set 且在途请求不是普通预取，或同物理块的有效请求，会阻挡新请求。S0 本拍只接受一个请求。S1 Store 与 S0 Probe 操作同块时已由此互斥，不增加依赖 Store 锁定结果的组合链。

主动 `s0_moveReq` 在等待 S0 仲裁期间仅为“待搬运”，不禁止 Probe。真正 `s0_moveReq.fire` 才将 entry 锁为搬运中；该 fire 必须对应 MainPipe 接收，不能是中间队列入队。move 与 Probe 同拍不能都进入 MainPipe；同块 Probe 被接受且未发生直接 release 时，取消待搬运请求、改为 Probe 处理中。被取消的旧请求不能引用复用后的槽位。已进入流水线的 move/Store 由既有冲突检查保护，不需 S0 PB busy 反馈。

### 与 PB 直接 Release 的交错

PB 主动驱逐经独立 `releaseReq` 直达 WBQueue，不经过 MainPipe。允许同块 Release 与 Probe S0 接收同拍发生；PB 的注册 Probe 结果必须反映已交接的状态：

| S0 边界情况 | PB 操作 | S1 Probe 结果 |
| --- | --- | --- |
| 同块 release 已交接、PB 不再持有块 | 不锁定 | locked=false，coh=Nothing |
| 同块 releaseReq.fire 与 Probe 接收同拍 | Release 交给 WBQueue，PB invalid；不再锁给 Probe | locked=false，coh=Nothing |
| 同块 release 尚未 fire，Probe 被接受 | 取消待释放/待搬运意图，锁给 Probe | locked=true，原 B/T 权限 |
| 块不存在 | 无 entry 操作 | locked=false，coh=Nothing |

Probe 锁定后不允许同块 Release 再握手；但仅有 `releaseReq.valid` 不代表已交接。未握手请求允许按此协议撤回，仲裁/缓冲不能偷偷保存已取消请求。WBQueue 接收握手必须是唯一交接点。

MainPipe 根据 DCache 查询及保存的 PB 响应生成 ProbeAck。若两处都不持有块，回复 NtoN；不能因 PB locked=false 就忽略 DCache 命中。若旧 Release 仍在 WBQueue，同地址 ProbeAck 会在 MainPipe S3 等待 WBQueue 允许接收。现有 L2 SinkC 独立处理 Release、MainPipe 生成 ReleaseAck，MSHRCtl 不因等待 ProbeAck 阻挡 C-Release，并通过 nested writeback 维护匹配在途事务；不能把 Release 自动当成 ProbeAck 或漏回 ProbeAck。

### S2 保持与 S3 完成

本版 PB 为干净 B/T 且 Probe needData=0，Probe 不读取 PB 整行数据；不满足该约束需扩展并验证数据响应，不能静默丢数据。S2 不走搬运的 moveAbort，不因替换冲突、B→T 或流水线 flush 取消已接受 Probe；只保存响应并等待 S3。

S3 应答被 WBQueue 接收时，仅当保存的 `locked=true`，MainPipe 发一次 `s3_probeDone(entryId)`，PB 在该沿 probeLocked → Invalid。接口无 ready，PB 必须接收。WBQueue 不 ready 就保持锁定；不等待消息实际上总线，也不等待 ReleaseAck。

`locked=false` 时必须禁止 probeDone，即使 ProbeAck 已发送也不能按旧 entryId 清项。成功锁定后 entry 禁止复用或被其他事务释放，保证 entryId 在 done 前始终指向本次分配。仅检查“entry 当前仍锁定”不能替代事务对应。

ProbeAck 与主动 Release 被 WBQueue 接收的事件都沿用 DCache→LSU 的 release 通知，PB Load 继续参加 LoadQueueRAR 跟踪与恢复。PB 的新 Load 授权截止点仍是锁定沿；不能只依靠 RAR 修复锁定后的新读取。

## loadpipe

### 设计原则

PB 是 L1 DCache 的内部组成部分。LSU 发起一次缓存访问，由 LoadPipe 并行查询 DCache array 和 PB，并返回统一的缓存响应。LSU 不维护 PB 状态，不接收 PB token，也不区分数据来自哪个内部存储结构。

PB 命中且获得读授权即可返回数据，不需要等待 PB→DCache 搬运完成。搬运属于缓存内部的后续维护操作。PB 数据仍经过 LSU 原有的 Store forwarding、异常、kill、flush、replay 和 LoadQueue/RAR 处理。

| 内部访问结果 | 对 LSU 的响应语义 |
| --- | --- |
| DCache 命中且数据可用 | 正常返回数据 |
| PB 命中且获得读授权 | 正常返回数据 |
| PB 持有该块但暂时不能提供数据，例如 锁定状态 | 通过统一缓存重试语义要求 replay，不作为新的普通 miss |
| DCache 未命中，PB 既不提供数据，也不要求重试 | 进入正常 MissQueue 流程，包括与已有 MSHR 合并或被拒绝 |

针对“有效使用缓存数据”的反馈，LSU 只提供 dcacheUse，不反馈 PB 地址或条目编号。PB 的 hit/retry 是 DCache 内部信息，由 LoadPipe 转换为统一接口的响应；LoadPipe 向 PB 只反馈本路 S2 的 s2_use 布尔信号。

### S0：LoadPipe 本地处理

PB 是由寄存器搭建的全相联结构，不需要 LoadPipe 的 S0 请求通知或虚拟地址。LoadPipe 自身仍按原流水线处理 S0；进入 S1 后，以 `s1_paddr.valid` 向 PB 发起查询。PB 不保存 S0 有效性或各 entry 的数据/状态快照。物理块匹配和读授权在 S1 完成，16B 数据窗口选择在 S2 完成。

### S1：物理地址查询与读授权

| LoadPipe → PB | 含义 |
| --- | --- |
| valid | 本拍有有效 S1 查询，不依赖 PB 侧的前一拍通知 |
| paddr | 查询同物理块，并确认所需块内位置 |
| kill | 取消本次查询；包括原流水线要求取消的翻译失败等情形 |

PB 使用 S1 paddr 并行比较当前条目的物理块地址，结合当前可读状态和 kill 决定读授权。S1→S2 仅寄存响应 valid、hit/retry、entryId 和 paddr 的块内偏移，不在 S1 选择或寄存数据，也不保存整份元数据。数据位置完全由本次 S1 paddr 确定，不再需要 S0/S1 窗口修正路径。PB 数据阵列假定可靠，不执行 ECC。

若回填在 S0 结束时已完成，S1 可以命中新 Resident 条目。若回填直到 S1 结束沿才完成，沿前仍为 Reserved，本次查询保持未命中，不组合旁路回填授权。

PB 在 S1 向 LoadPipe 提供组合查询结果，用于及早决定是否产生 miss 请求：

| PB → LoadPipe | 含义 |
| --- | --- |
| hit | 本次请求获得 PB 读授权，后续可以使用对应数据 |
| retry | PB 持有该块，但本次不能获得授权，要求重试 |

hit 与 retry 互斥。两者均为 0 表示 PB 本次不提供数据，也不要求因 PB 而重试；仍需结合请求有效性、kill、DCache 结果决定后续操作。Reserved 项不是可读命中，也不单独要求 PB retry，交由正常 MissQueue 流程处理。

### S2：数据返回

PB 在 S2 使用已寄存的 entryId 从数据寄存器中取出对应行，再按已寄存的块内偏移组合选择 16B，直接返回给对应 LoadPipe；不再增加一级数据寄存器：

| PB → LoadPipe | 含义 |
| --- | --- |
| valid | 本拍有对应请求的响应，不等同于数据命中有效 |
| hit / retry | 与本条 Load 对齐的寄存查询结果 |
| data | 获得授权的数据窗口，不需要返回整条 cache line |
| prefetchSource | 该分配的原始预取来源 |

data 仅在 valid 且 hit 时作为 PB 数据使用。LoadPipe 选择 DCache 数据或 PB 数据，向 LSU 返回统一响应。PB 已提供有效数据时，不能仅因未选中的 DCache 数据阵列发生 bank conflict 或 way prediction 失败而要求 replay；LSU 自身的异常和其他真实重试原因仍然生效。

S1 已授权后，S2 不重新检查 Resident/valid 来撤销授权。安全性由当前写入规则保证：接管、失效和分配不覆盖 entryData；只有已为 Reserved 的条目可以接受回填。若 S1 授权与 Release 同沿，S2 时条目虽已 Invalid，但数据、地址和来源仍保留；最早在 S2 结束沿重新分配，再下一拍才能接受新回填。固定 S2 响应及同拍 s2_use 因而可用 PB 内部保存的 entryId 对应原分配，无需额外锁定或 token 往返。禁止将失效旁路成同沿分配/回填，或在失效时清空数据/来源；未来改变这些规则或增加 S2 停顿时，必须重新处理读保护。

### dcacheUse 使用反馈

每条 LoadPipe 对应一路由 LSU 返回的 Bool 信号 dcacheUse，表示本次 Load 有效使用了缓存返回的数据，不表示指令已经提交。置位需要同时满足：

- 本次 Load 有效接受缓存响应，未被 kill、异常、flush 或 replay 取消。
- 至少一个所需字节来自缓存数据；全部由 Store forwarding 提供时不置位。
- 属于本次正常 Load 的数据使用，不是预取或不可缓存访问的使用通知。

反馈与对应 Load 的 S2 响应同拍对齐。LoadPipe 确认该响应实际选用了 PB 数据后，生成本路内部使用通知：

```
s2_use = dcacheUse && responseValid && responseFromPB
```

普通 DCache 命中的 dcacheUse 不产生 PB 使用通知。PB 命中但 LSU 未使用数据，也不产生 PB 使用通知。该反馈不参与本次 S1 授权或响应有效性的组合计算，避免形成反馈环路。

PB 使用本路 `loadS2Valid && loadS2Hit && s2_use` 判断有效使用，以已寄存的 `loadS2EntryId` 定位条目；不重新查询当前 S1 地址，不要求 S2 时仍为 Resident。只有当前仍可见且非 Reserved 的条目才能更新 used/movePending。Release 一旦与 WBQueue 握手，目标条目一律转入一拍 `Released` 状态，保留旧条目的 data、source 和 used 元数据；沿前已经授权的 Load 仍可在下一拍完成数据返回，但 Released 期间的 `s2_use` 不再更新该条目的状态或统计，也不能把状态恢复为 Resident。

`Released` 状态对新的 S1 查询不可见、不能被 allocator 重新分配或接受新的 refill，但保留沿前已授权 Load 的 S2 数据访问。状态在下一沿转为 `Invalid`，之后才允许新的 allocation。Released 项的普通新请求不能复用旧数据或影响同沿重新分配的内容；Released 期间的 `s2_use` 只完成接口消费，不参与 PB entry 的 used/movePending 更新。

仅使用一个 Bool 的前提是每路反馈与响应固定对应，不能任意延后反馈。若 LSU 未在对应拍接受响应，则本次不产生使用通知，按原流水线规则处理；如果未来增加响应保持或延后反馈，必须重新设计事务身份保存，不能把上一条 Load 的反馈关联到下一条的 S2 上下文。无有效响应或 hit=false 时，即使输入 s2_use 为 true，PB 也不记录使用。

### Load 读授权的截止点

以成功 claim 的时钟沿为截止点：**同拍 Load S1 可以获得 PB 读授权，下一拍起由寄存的 锁定状态 状态阻止新的授权。** 同拍成功的 Load 在逻辑上排在接管之前。

Load S1 必须检查时钟沿前的 Resident/readable、物理块匹配和 kill 条件。PB 独立完成读授权，LoadPipe 再结合本地 DCache 命中和错误结果选择响应。PB 数据假定可靠，不增加 ECC 检查。

Load 授权不依赖当拍 claim.fire，也不因为 Probe 在 ProbeQueue 中等待就提前屏蔽。成功授权的 hit、entryId 和偏移在该沿锁存到 S2，与 entry 进入锁定状态同时发生；数据和预取来源在 S2 从原分配仍保留的存储中取得。已获授权的 S2 响应不因随后锁定或失效而追溯取消，但仍服从 LSU 原有异常、flush、资源冲突等 replay 规则。

| 全局拍 | Probe 的阶段 | Load 的阶段及结果 | PB 状态 |
| --- | --- | --- | --- |
| C0 | S0 接管成功 | Load A 在 S1，可按沿前状态授权 | 沿前 Resident，沿后 锁定状态 |
| C1 | S1 | Load A 在 S2，按已锁存授权选择原数据；新的 Load B 在 S1 不能获授权 | 锁定状态 |
| C2 | S2 | 后续新 Load 仍不能获授权 | 锁定状态 |
| C3 或更晚 | S3，等待 WBQueue 接收 | 后续新 Load 仍不能获授权 | wb.fire 的沿后 Invalid |

即使 Load B 在 C0 的 S0 已被接收，C1 的 S1 也必须因当前状态为锁定状态而拒绝授权。S3 等待多久，都不改变这个截止点。该规则同样适用于 S1 Store 锁定和 S0 move 接收；直接 release 的沿前授权及迟到使用按“搬运与直接驱逐”节处理。真正的 Store 写入仍在搬运完成并 replay 之后。

## PB 核心模块

### 容量、数据与身份

本版保持 16 项容量与现有替换策略，避免将接口和一致性改动与容量调优混在一起。

| 项目 | 本版约定 |
| --- | --- |
| entry 数量 | 默认 16 项，参数化配置；Reserved、锁定状态和 Released 均占用容量 |
| 每项数据 | 一条缓存块，当前为 64 B；默认数据容量为 512 B，不含 metadata |
| Load 读端口 | 与标量 LoadPipe 数量一致，当前为三路独立 S1 查询、S2 响应接口 |
| Load 返回窗口 | 与现有 LoadPipe 数据窗口一致，当前为 128 bit；不等于增加向量支持 |
| 物理目录 | 全相联，按物理块地址查询 |
| 数据实现 | 当前使用寄存器阵列；本规格不据此承诺 SRAM 宏实现或目标频率 |
| 权限 | 干净 B 或 T；不持有 Dirty，不接收 Store 字节写入 |

PBMeta 仅保存 state、paddr、missEntryId、vaddr、coh、prefetchSource、used、movePending、dataBad。entryId 来自数组下标，alias 从 vaddr 提取，物理地址保存为 paddr；不向 LoadPipe 返回条目身份。PB 不保存 metadata parity，也不执行本地 ECC；dataBad 只记录上游 L2 corrupt 或接口传入的错误清理状态。

删除 PBMeta.storeReplayId，Store replay credit 仅由 MainPipe 持有。promotePending 和 aliasPromote 合并为 movePending，表示 Resident 的有效 Load 使用、Store alias 不匹配或搬运 abort 产生的搬运意图；单纯 pending 不锁定 entry。若统计只需请求原因，在事件发生时计数；若统计要求最终搬运原因，须在实现前明确独立的归因记录，不能因合并调度位而丢失已要求的统计语义。

删除 PBMeta.op 与 PBOp，用 probeLocked/moveLocked 区分锁定归属。dataBad 仍保留：错误可在锁定期间出现，不能只靠 Poison 状态表达。删除 PBToken 后，PBMeta.gen 及其递增逻辑已无其他功能消费者，一并删除。

MainPipe 在锁定后只传 entryId；MSHR 从预留到取消/回填也只传 entryId，并校验所属 MSHR。两者均依赖持有期间禁止复用、终结事件恰好一次。Load 则依赖固定 S2 响应/反馈及已授权读取结束前不能覆盖分配的约束，由 PB 自行保存各路 entryId；删除 PBToken、entryToken 构造与 sameToken 比较。该协议不支持任意迟到的使用通知，不能依赖已删除的 generation 检测此类违约。

同物理块在 PB 中至多对应一个非 Invalid entry。PB 作为独立可用数据所有者时，不得与 DCache 中同块的另一份独立有效所有权并存。PB→DCache 搬运通过原子发布完成所有权转移。

### 状态与转换

| 状态 | 含义 | 新 Load 读授权 | 接管与退出 |
| --- | --- | --- | --- |
| Invalid | 无有效块，可在未被内部事务引用时分配 | 不授权 | 成功分配后进入 Reserved |
| Reserved | 已为一个 MSHR 预留，数据尚未发布 | 不授权；使用正常 MQ 流程 | 正常 fill 后 Resident；释放或 denied 后 Invalid；corrupt 后 Poison |
| Resident | 有完整数据和干净 B/T 权限 | 满足身份和快照条件时授权 | Probe 接管后 probeLocked；Store/move 接管后 moveLocked；直接 release.fire 后 Invalid；L2 corrupt 后 Poison |
| Poison | 数据不可使用，但仍有权限清理责任 | 不授权，走重试/错误处理 | Probe 锁定后 probeLocked；错误清理 release.fire 后 Invalid |
| Released | Release 已交给 WBQueue，保留一个统一的旧 Load 退休窗口 | 新查询不授权；仅允许沿前已授权 Load 完成 | 保留 data/metadata 一个周期，随后 Invalid；不能重新分配、fill 或接管 |
| probeLocked | Probe 已取得一致性处理责任 | 不产生新授权，保留沿前已授权响应 | 仅 probeDone 后 Invalid，不接受 moveAbort/moveDone |
| moveLocked | Store 或主动 move 已取得搬运责任 | 不产生新授权，保留沿前已授权响应 | moveDone 后 Invalid；moveAbort 恢复 Resident 或 Poison，不接受 probeDone |

Resident 不等于无条件可读：上游标记的坏数据、不满足干净 B/T 权限等条件仍会阻止普通读授权。单纯的后台搬运 pending 不立即禁止读取；Store 成功锁定、move 实际 S0 接收和 Probe 实际 S0 接收分别构成其读截止沿。

pending 表示待搬运/待释放意图，不代表已经锁定。成功锁定时记录归属：Store、move 或 Probe；moveDone/moveAbort/probeDone 必须对应本次归属，不能对未锁定项或复用后的项执行。直接 release.fire 则将未被 MainPipe 锁定的块交给 WBQueue。

“MSHR 等待 PB 分配”仅是 MQ 内部事务状态，不在 PB 中占用 entry，也不是新的 PB 状态。分配申请仍须触发必要的容量压力回收，不能因尚未有 Reserved 项而忽略该请求。

### 与 MissQueue 的接口

PBMSHRIO 的分配与回填均为 MissQueue 内仲裁后的单口，不再按上游请求宽度展开，也不保留 nReq 参数。

| 字段 | PB 侧类型/方向 | 语义 |
| --- | --- | --- |
| allocReq | 输入 Decoupled[PBAlloc] | MSHR id、物理块地址、vaddr、预取来源；fire 沿建立 Reserved，不限制提前发 Acquire |
| allocEntryId | 输出 UInt(PBIdBits) | 仅在 allocReq.fire 时由获选 MSHR 保存；未握手不代表占用 |
| cancelReq | 输入 Vec[nMissEntries, Valid[UInt(PBIdBits)]] | 每个 MSHR 一路，端口编号标识所属 MSHR，bits 为取消的 entryId；无 ready，一次通知即释放预留 |
| refillReq | 输入 Decoupled[PBRefillReq] | entryId、missEntryId、整行 data、coh、denied、corrupt；握手沿提交数据和状态 |
| status | 输出 Vec[PBEntries, Valid[UInt(PAddrBits)]] | 每项只提供有效物理块地址；Resident/锁定状态/Poison 有效，Invalid/Reserved 无效，用于 MQ 阻止同物理块重复分配 |
| preAcquire | 输入 Vec[nMissEntries, Bool] | 暂保留现有 WFI 区分未发 A 与必须排空事务的接口，后续单独梳理 |

Reserved 项在取消或回填前禁止替换、复用，故 MSHR 只需保存 entryId，不需要完整 token。PB 仍记录所属 MSHR，取消时检查端口编号，回填时检查 missEntryId。非法身份或非 Reserved 状态必须报断言，不能改写其他分配。

已接受的 demand 合并优先于新分配：MSHR 在进入分配仲裁前屏蔽 allocReq.valid。未取得预留只撤销请求；已取得预留才发送 cancelReq，并在同沿清除 MSHR 的预留记录。取消无背压，每个 MSHR 只通知一次，不同 MSHR 可同拍取消不同项。取消与回填不得作用于同一项；分配最早下一拍回填，取消释放的项本版最早下一拍重新分配。

多个 MSHR 的申请与回填各自公平仲裁。refillReq 在背压时保持 valid 和所有字段，不能由下一个请求覆盖。若设回填队列，只有队列出口与 PB 的实际握手算完成，MSHR 不能因入队而解除 Probe 阻挡或复用。

不保留独立的 take、publish 或 fillDone：allocReq.fire 提交预留；refillReq.fire 提交回填。MQ 根据后者与 missEntryId 寄存完成状态，握手当拍仍阻挡匹配 Probe，下一拍才由 PB 状态和 MainPipe 接管。正常回填成为 Resident；denied 释放为 Invalid；未 denied 的 corrupt 成为 Poison。MSHR 释放还须满足正常 GrantAck 条件。

若数据写端口未能在当前沿接收整行，PB 必须令 refillReq.ready=false，不能先握手后排队写入而提前解除保护。MainPipe 不产生 PB 预取回填，也不因等待 PB 回填占用流水级。

### 与 MainPipe 的接口

PB 侧 `PBPipeIO` 恰好包含下表十二个扁平字段，MainPipe 使用 `Flipped(new PBPipeIO)`。PB 实例路径是 `io.pipe.s0_moveReq` 等，`moveReq` 不再位于 PB 顶层，也不承担主动驱逐。Load 查询可复用地址/数据载荷与物理匹配逻辑，但独立保持各 lane；不能因复用合并成仲裁式单读口。

| 字段 | PB 侧类型/方向 | 语义与生效点 |
| --- | --- | --- |
| s0_probeReq | 输入 Valid[UInt(PAddrBits)] | 实际 S0 Probe 接收的物理块；PB 内部锁定并记录结果，无 ready 反馈 |
| s0_moveReq | 输出 Decoupled[PBMoveReq] | entryId、paddr、vaddr；只搬入 DCache，fire 对应 MainPipe S0 接收并锁定 |
| s1_hit | 输出 Bool | 有效普通查询匹配且至少可读；不是“存在该物理块”，不是 Store 锁定成功 |
| s1_paddr | 输入 Decoupled[UInt(PAddrBits)] | valid 使能物理查询，fire 对应 MainPipe S1→S2；ready 只表示 PB 能保存该事务后续结果 |
| s1_alias | 输入 UInt(PBAliasBits) | 同次请求的 get_alias(vaddr)，仅用于 Store 接管检查；反压时随查询保持，无 alias 配置时忽略 |
| s1_storeReq | 输入 Bool | Store-only 锁定意图；在同次 s1_paddr.fire 时采样，不带地址、token 或 replay 标识 |
| s1_probeResp | 输出 Valid[PBProbeResp] | S0 的 locked/coh/entryId 注册快照，保持至该 Probe 离开 S1 |
| s2_storeResp | 输出 Valid[Bool] | bits 表示锁定成功；作为 dataResp 的 Store sideband，失败仍 valid，由 s2_dataResp.fire 统一消费 |
| s2_dataResp | 输出 Decoupled[PBPipeDataResp] | Store/move 共享整行数据、权限、使用信息及错误/重试结果；普通查询也可返回无数据结果 |
| s2_moveAbort | 输入 Valid[PBMoveAbort] | entryId、dataBad；取消一个已锁定的 Store/move，不用于 Probe |
| s3_probeDone | 输入 Valid[UInt(PBIdBits)] | locked Probe 的应答被 WBQueue 接收后释放；无 ready、一次脉冲 |
| s3_moveDone | 输入 Valid[UInt(PBIdBits)] | Store/move 安装提交后释放；所需写端口和 victim 释放已接受，无 ready、一次脉冲 |

`PBMoveReq` 不带 op：release 已分离。待搬运期间 PB 内部保存/校验分配身份，不能将槽位重新分配后继续发送旧请求。`s0_moveReq` 在握手前可因同块 Probe/Store 接管而撤回，此处不是 Irrevocable 协议；取消资格必须在下次接收前生效，任何中间缓冲都不能掩盖实际 S0 接收。PB 仅用 `s0_moveReq.fire` 保存 move 来源和 entryId，S1 不再发 storeReq 或通用 claim。每个已接受 move 必须以 moveAbort 或 moveDone 结束，禁止 S1 静默 drop。

#### 查询、阶段推进与响应保持

PB 不再接收通用 `s0_vaddr`。普通请求仅在 S1 提供 paddr、alias 和 Store sideband，不在 PB 重复保存 S0 valid 或完整请求 vaddr。Probe 用 `s0_probeReq.valid` 锁存来源及应答结果；move 用 `s0_moveReq.fire` 锁存来源及 entryId。两类上下文均保持到对应 `s1_paddr.fire`，即使 MainPipe 尚不能推进、暂未给出查询 valid，也不能提前清除。旧 S1 消费与新 Probe/move S0 接收同拍时，先清旧上下文再保存新上下文，支持连续请求。

s1_paddr.valid 表示当前请求有效且满足 MainPipe 自身推进条件；请求取消/错误等资格由 MainPipe 按原逻辑处理。s1_hit 的组合计算不得依赖 s1_paddr.ready、s1_storeReq 或锁定是否成功，避免 MainPipe 根据 hit 产生 storeReq 后形成环路。s1_paddr.fire 与 MainPipe S1→S2 对齐；未 fire 时保持地址、alias、Store sideband 和已有的 Probe/move 上下文，PB 不重复生成响应或锁定。MainPipe 的 S0 接收必须保证当前 S1 上下文有空间，不能将 Probe Valid 输入当作可无限吸收队列。

s1_paddr.ready 只反映响应缓冲容量和已声明的全局停止条件，不能把“锁定失败”编码为永久 ready=false。s1_storeReq 是同次查询的 Bool sideband，不是第二个独立握手口；仅 `s1_paddr.fire && s1_storeReq` 表示接受 Store 尝试。Probe 同样推进 S1 上下文，但没有 s2_storeResp/dataResp，携带已有 Probe 结果进入 MainPipe S2。

Store 在 S1 结束的接受沿执行锁定，S2 返回 success，不能等 S2 才阻止新授权。主动 move 早在 S0 接收沿锁定；PB 依保存的来源和 entryId 允许该 move 读取自己的块，不按普通锁定项拒绝。预取只查询、不锁定、不启动 DCache victim 整行读取，也不消费 PB 整行数据。

MainPipe 的 Store victim 读取资格由请求类型和 DCache miss/tag 状态产生，不反向依赖 PB hit；否则在当前 S1 阶段推进协议下，会形成查询 valid、PB hit、读端口 ready 的组合环。因此当前 Store DCache miss 即使最后 PB miss，也可能读取 victim；这是本版需要在后续性能分析中单独观察的开销。

Store S2 同时有 storeResp 与 dataResp；只有 dataResp 带 ready，`s2_dataResp.fire` 原子消费整次响应。storeResp.valid 表示配对事务是 Store，不能独立消费；它必须与该 Store 的 dataResp.valid 同时置位，并随其保持到消费。storeResp.bits 表示锁定成功，成功时 entryId 仅从 dataResp 取得；失败时 storeResp.bits=false，忽略整行字段，但 dataResp.valid 仍为 true，保证失败也能正常完成握手。若有同块不可用所有权，retry=true，禁止走普通新分配。主动 move 和普通查询令 storeResp.valid=false。MainPipe 的 S1 只完成物理查询、命中判断、entryId 记录和 Store/move 锁定，不读取 PB 整行数据；需要搬运的请求在 S2 通过已锁定的 entryId 读取 `entryData(entryId)`。move/store 条目在进入 S2 前已经是 moveLocked，因此 S2 反压期间整行 data 不会被 release、其他 move 或 refill 改写，data 可持续直接读取，不需要额外的数据锁存；used 仅用于统计和安装标记，不参与数据、权限或完成正确性，S2 每拍直接读取当前值，反压期间允许变化。S2 moveAbort 必须与本事务的数据响应消费对应；S3 阻塞不能阻止一个合法 S2 abort 终结本次尝试。

接口实现必须检查：storeResp.valid 必须蕴含 dataResp.valid，且与 MainPipe 保存的本次 Store 尝试对应；成功 Store 或已接收 move 的 retry=false；失败 Store 与普通查询不得发送 moveAbort/moveDone。MainPipe 根据流水中保存的 move 来源或配对的 storeResp.bits 判断是否持有待搬运行，不增加重复的所有权有效位。外层 dataResp.valid 仅表示响应已准备好，普通查询和失败 Store 也必须完成握手。Probe 的 locked 表示取得一致性处理责任，不是数据读授权。

PBPipeDataResp 包含 retry、entryId、整行 data、coh、prefetchSource、used、dataBad。对于已接收 move 或成功锁定的 Store，entryId、权限和整行相关字段有效；dataBad 时禁止安装，并通过 moveAbort 归还错误清理责任。普通查询和失败 Store 使用 retry；对于保存了 S1 可读命中的普通预取查询，prefetchSource 还用于重复预取命中的来源统计。没有取得所有权时，忽略 entryId、data、coh、used、dataBad，不据其内容触发安装、释放或错误取消。是否持有待搬运行由 MainPipe 保存的事务类型及 Store 锁定结果推导，不另设 dataValid。物理存在与可读的区别由 PB 内部所有权检查及 MQ/WBQ 冲突路径保证，不能仅以 s1_hit=false 发新 miss。Reserved 的 retry 行为遵循 MQ 原有规则。

#### 完成身份与统计交接

同一 entry 的成功 Store/move 只能 moveAbort 或 moveDone 二选一，成功 Probe 只能 probeDone；失败 Store/未锁定 Probe 不得产生 entry 完成通知。锁定到终结通知期间禁止复用，故这些接口只携带 entryId。锁定前的候选在条目可复用前撤销，MSHR 预留校验归属；Load 使用通知必须与固定 S2 响应同拍，由 PB 保存的本路上下文关联。

Store 的 replay credit 由 MainPipe 随流水保存，发生锁定失败、abort 或 done 后各返回一次重试；不能依赖 PB 发异步 replay 通知。调度与 MainPipe 响应只使用已寄存的 used，不组合旁路 entryUsedNow，也不为了最后一次 Load 使用标记增加等待。

pipeS1Resp.used 只读取 S1 的 entryMeta.used；move/store 条目的整行 data 在 S2 始终直接来自已锁定的 `entryData(entryId)`，由 moveLocked 保证稳定，不需要数据锁存。used 仅用于统计和安装标记，S2 每拍直接读取当前值，反压期间允许变化，MainPipe 在握手时采样，不为它增加等待或锁存寄存器。主动 move 在 S0 锁定，Store 在 S1 锁定；随后 Load 使用通知可以在 S2 反压期间更新 used，最终搬运使用握手时的当前值。该取舍不改变数据、权限、锁定或完成协议；后续重发的 Store 按正常路径设置 DCache access。独立 PrefetchUseTracker 的 used 仍只记录 Load 使用，不随 Store access 更新，因此该边界可能重复计首次使用或在 DCache 退出时误计未使用；本版接受该统计限制，不增加等待或 S3 修补接口。

PB 不产生本地目录 ECC/parity 错误；DCache 自身的 tag/data 错误仍由 DCache 原有路径处理，不能因 PB 命中而掩盖。

### Load 接口与使用记录

Load 接口参考 PBPipeIO 使用扁平的流水级前缀。PB 侧声明 `load = Vec(LoadPipelineWidth, new PBLoadIO)`，每条 LoadPipe 使用 `Flipped(new PBLoadIO)`；Wrapper 连接为 `pb.io.load(lane) <> pipe.io.pb`。已删除 PBLoadsIO、PBLoadS1IO、PBLoadS2IO 和 PBLoadReq，并适配调用方；不为 Vec 或单个流水级再建包装类。

| 字段 | PB 侧类型/方向 | 含义 |
| --- | --- | --- |
| s1_paddr | 输入 Valid[UInt(PAddrBits)] | 本拍 S1 物理字节地址查询，无 ready，不依赖 S0 通知 |
| s1_kill | 输入 Bool | 本次请求取消或流水 nack，禁止授权、PB retry 及对应 S2 valid |
| s1_hit | 输出 Bool | 本次查询获得读授权，语义与 MainPipe 普通查询一致 |
| s1_retry | 输出 Bool | PB 所有权使本次读取不可用，禁止因此产生新 miss |
| s2_dataResp | 输出 Valid[PBLoadResp] | 仅含 data、prefetchSource、hit、retry；S2 根据寄存 entryId/偏移取得数据和来源 |
| s2_use | 输入 Bool | 本路 S2 的 PB 数据被实际使用；PB 按本路保存的 valid/hit/entryId 关联，不能延后反馈 |

S1 只对外返回 hit/retry，内部保存 entryId、偏移和查询结果，S2 组合选择数据并取得同一分配的预取来源。两级 hit/retry 对应同一事务，不是两次查询。Load 使用固定延迟 Valid，不能照搬 MainPipe 的 Decoupled 背压：S1 查询对应的响应在下一拍出现一次，不等待 ready，也不能等待 s2_use 后才结束。

PBLoadResp 不再返回 used：原调用方仅将它传至没有读取端的 meta_access，当前 PB 响应将该外层字段置零，DCache 阵列路径保持原有 access 信息。PBMeta.used、功能使用记录及 MainPipe 搬运所需的 used 交接继续保留；主动搬运调度统一读取 movePending。

s1_paddr.valid=false 或 s1_kill=true 时，hit/retry 均为 false，下一拍不产生本次响应。PB 独立执行物理查询和状态检查，不接收 DCache tag 命中或 tag 错误反馈，也不能通过 kill 或查询 valid 变相引入这些组合依赖。LoadPipe 汇总两边结果，保留原 DCache tag/data 错误处理，不能因 PB 命中而掩盖错误；因错误未有效使用数据时，不发送 s2_use。

保留 s1_retry，用于区分 PB 未持有该块与持有但暂不可读：DCache 也未提供有效数据时，后者阻止新 miss 并由 LoadPipe 生成统一重试，前者正常进入 MQ；Reserved 不单独要求 PB retry。PB 的 retry 不是直接向 LSU 发 replay，最终行为仍结合有效性、取消、DCache 命中及错误处理决定。

重复所有权检查放在 DCache 集成处，结合 PB 的物理所有权和 DCache 元数据检查；仅比较两边可读 hit 不足以覆盖 锁定状态 等不可读状态。不能靠屏蔽 PB hit 掩盖重复所有权，也不能为此将 DCache tag 结果重新送入 PB 读授权路径。PB 读授权不转移权限、不释放 entry，最终数据选择和有效使用记录由 LoadPipe 本地控制。

与 MainPipe 复用阶段命名和物理块匹配原则，不强行共用所有响应字段：Load 返回数据窗口、来源和命中/重试结果，MainPipe 返回整行及 B/T 权限。Load 不增加 storeReq、moveDone 等所有权接口，s2_use 也不改变本次读授权。

三路 Load 接口分别执行 loadpipe 节约定的 S1 物理查询和 S2 数据选择，不需要三路 MainPipe 整行响应端口。每路独立保存查询结果、entryId 和窗口偏移，不保存 S0 请求或全体 entry 的数据快照。

PB 接受 DCache 转发的每路 s2_use 布尔通知；LSU 边界只有 dcacheUse。PB 根据已保存的 S2 授权和 entryId 形成各条目的使用事件，同拍多路命中同一项只更新一次。正常 Resident 项首次被有效使用后记录 used，并设置后台搬运意图。仅有目录命中或读授权而没有有效使用，不应直接视为首次有效使用。

使用通知只更新匹配分配的使用信息或维护意图，不直接改写数据、权限，也不能恢复已经退出或正在接管的 entry。数据响应的正确性不依赖使用统计是否更新。被接管项仍由原操作完成，不能因使用通知重复发起同项搬运。

### 搬运与直接驱逐

搬运和释放各自维护一个候选寄存器，并各用独立 RR 仲裁器选择下一项。本次不调容量或预取参数。正常 Resident 项首次有效 Load 使用后设置待搬运意图并请求 `io.pipe.s0_moveReq`；只因容量不足选中的未使用块，或需要错误清理的 Poison，发独立 PB→WBQueue `releaseReq`。释放不再经 MainPipe，也不将未使用块因 PB 满而强制装入 DCache。

两路候选独立保持：MainPipe 反压不阻止另一条目的 Release，WBQueue 反压不阻止另一条目的 move。两路可在同拍分别握手，但 entryId 必须不同。entryNeedMove 定义为 `entryReadable && movePending`，只读取寄存状态，不旁路当前 entryUsedNow；正常可读且有 movePending 的条目只参与搬运，Poison 或容量压力下无搬运需求的 Resident 只参与释放。Resident 的有效 Load 使用在时钟沿同时设置 used 和 movePending，下一拍调度才据此撤销尚未握手的同项释放，并转入搬运候选集合。如果 Release 与首次使用通知同拍握手，允许释放并按原分配记录已使用退出，不能复活条目或再发起搬运。

调度不再额外检查 used：对合法可达的 Resident 条目，used 为真时 movePending 必为真。锁定期间收到此前已授权 Load 的使用通知，可能只更新 used，但锁定项不可入选；若 moveAbort 恢复 Resident，同沿必设置 movePending。分配时同时清零 used 和 movePending，当前分配内不单独清除 movePending。该状态约束保证移除 `|| used` 不改变搬运或释放候选；used 仍独立记录有效使用，随 MainPipe 数据响应交接。

当前候选握手或因接管/资格变化撤销时，该候选寄存器可在同一边沿装入下一项。下一项的仲裁排除两路已经保存的候选以及沿前已被 Probe/Store 接管的条目；候选集使用沿前有效状态，Reserved、锁定条目以及本拍才回填/取消搬运的条目不旁路入选。仲裁结果只送候选寄存器，不能组合反馈到当前 moveReq.valid，避免经过 MainPipe S0 接收条件形成组合环。Store 接管导致的候选失效在下一拍依据 entry 状态体现，允许一个标准 Decoupled 气泡。

多条不同 entry 的 move 可同时在 MainPipe 流水线中执行。调度候选在 moveReq.fire 时即可补入下一项，不等待 s3_moveDone；已发出条目分别保留 moveLocked、数据和权限，直到对应 entryId 的 moveDone 或 moveAbort。各条目的 S1 上下文、S2 数据响应和 S3 完成必须按原流水级对齐，允许同拍完成 A、取消 B、接收 C（entryId 互异）。PB 调度器在候选充足且 MainPipe 连续接收时支持每拍一条；这不保证 MainPipe 在同 set/物理块冲突、端口或 victim 反压时仍能每拍接收。

容量压力仅由当拍的 allocBlocked 表达，即分配请求有效且没有 Invalid 条目，不再寄存历史压力。分配请求撤回或已有空位时，撤销尚未握手的容量释放候选；Poison 清理不依赖容量压力。正在搬运的槽位不视为空位，也不提前分配；当前没有按在途搬运数量抵扣容量需求，可能继续安排其他候选退出。WFI 下暂停无容量压力的主动请求；有受阻分配时允许继续腾空间，safe 必须同时检查两路候选和每个锁定条目。

PB 的 WFI safe 还检查当拍 Probe/move S0 接收、持有的 Probe/move S1 上下文、MainPipe 当前 S1 查询及 S2 响应、有效且未 kill 的 Load S1 查询及 Load S2 响应，以及原有回填/已发 Acquire 预留条件。普通请求在进入 PB 的 S1 之前由所属流水线负责，PB 不再通过 S0 地址端口记录这些请求；PB safe 只代表本模块可暂停，DCache 仍结合 MissQueue 和系统既有 WFI 条件。

待搬运仅是 pending 意图，仍可供合法 Load 读取、被 Store/Probe 接管。moveReq.valid 或仲裁选中不锁定，真正 moveReq.fire 才 Resident → moveLocked。Probe 同拍竞争由 MainPipe 单入口决定；此前已进入 S1/S2/S3 的同块操作由既有流水线冲突保护。move 接受后直接按已拥有的块读取，不能再依赖 S1 Store 请求二次锁定。

Move 调度器不得让 `moveReq.valid` 组合依赖 Probe 或 Store 的请求 valid、地址或目录匹配结果。MainPipe 已规定 Probe S0 优先，并通过 `s1_s0_set_conflict` 阻止同一物理块的 Store S1 与 Probe/Move S0 同时进入流水线；因此 PB 不需要 Store→Move 的组合抑制信号。Probe 或 Store 接管后，PB 只在时钟沿后依据锁定状态刷新或撤销 Move 候选。Move 的实际交接仍以 `moveReq.fire` 为准。

move/Store 搬运均由 MainPipe 选 way、处理 victim，并在 moveDone 沿原子安装 PB 原数据和 B/T 权限。PB 预留不代表已预留 DCache way。S2 abort 保留数据及必要搬运意图；重试重新参与仲裁，不保留失效的 MainPipe 流水线引用。

直接 release 是可取消的待释放事务，直到 PB 的 `releaseReq` 与 WBQueue 的 pending buffer 握手才交接并使 PB invalid；此前必须保留数据/权限。PB 内部不得为已由 Probe/Store/move 锁定的 entry 再发送 release。同块 Probe 与 release.fire 同拍允许，按 probeReq 节将 Probe 结果设为未锁定；Probe 先锁定而 release 未 fire 时撤销 release。若 Store 尝试与同块 release.fire 同拍，Release 已交接，Store 返回 success=false，不再锁定，并按 WBQueue 所有权要求重试；若 release 未 fire 而 Store 成功锁定，则撤销待释放事务。PB 调度不允许同一 entry 同时作为有效 move 和 release 候选；转换意图必须先撤销原未接受请求，禁止两条路径同时 fire。上述互斥由 PB 内部保证，不能指望 MainPipe 单入口覆盖独立 WBQueue 入口。

PB release 不再与 MainPipe `wb` 在 DCacheWrapper 中仲裁。WBQueue 提供独立的 PB release 输入，并在内部维护两项 non-cut-through pending buffer；PB 只观察该 buffer 的入队 ready，pending 项再由 WBQueue 内部调度器搬入普通 WritebackEntry。pending buffer 中两个物理块地址都参与 `block_conflict` 和 `block_miss_req`，因此同块 MainPipe 写回和 MissQueue miss 会被阻挡；这不会阻挡 MainPipe S0 接收 Probe。PB release 使用 `voluntary=true`，按 B/T 生成 BtoN/TtoN；alwaysReleaseData 时提供完整行和 corrupt，否则干净块不带数据。PB 驱逐至 WBQueue 的数据采集在 PB 内部完成，不占 MainPipe 整行读事务。

WBQueue 以 `accepted` 脉冲报告请求真正进入 WritebackEntry 的事件，事件携带完整 `WritebackReq`。DCacheWrapper 的 LSU release 通知和 LR/SC reservation 清除使用该事件；PB 进入 pending buffer 的握手不提前触发这些通知。

Release 沿前已授权的 Load 必须保留正确响应/身份；Released 只保证该 Load 的下一拍数据访问，不允许新查询或复用旧槽位。Released 期间的 `s2_use` 不更新已退出 entry，退出统计以 `Released -> Invalid` 为准，不在 `release.fire` 沿提前复用容量。WBQueue 交接后的同地址请求/ProbeAck 排序沿用既有机制，并以实际 ReleaseAck/ProbeAck 交错用例验证。

### 并行操作与错误处理

不同 entry 可在资源允许时并行执行 fill、接管和 finish，不能为了共享一个状态更新入口而丢失其中任一事件。同一 entry 的分配、fill、free、接管、abort 和 finish 必须按状态表形成唯一合法转换；不允许靠任意赋值优先级掩盖非法并发。

同拍 Load S1 授权与锁定或直接 release.fire 可以按沿前状态排序共存；S2 通过寄存 entryId 读取原分配仍保留的数据和身份，使用通知必须同拍返回。任何直接 `release.fire` 都进入一拍 `Released` 退休状态：下一拍阻止该槽位的新查询、allocation/refill 和接管，但允许沿前已经授权的 Load 完成对应的 S2 数据访问；窗口结束后转为 Invalid。Released 期间的 `s2_use` 不更新已退出 entry。fill 只修改 Reserved，done 只释放已锁定项；不旁路同沿分配和回填，不得在已授权 Load 的 S2 读取结束前覆盖其数据或分配身份。

PB 假定数据和 metadata 永远可靠，不产生本地 ECC 错误或 fatal 目录错误。L2 的 denied/corrupt 仍沿事务结果路径处理；进入低功耗等流程时，除 PB 的 Reserved、锁定状态、维护请求、整行响应和在途 Load/fill 通知外，还须计入 MQ 中尚无 PB entry 的有效事务，不能仅凭 PB 状态判断系统已空闲。

## 预取统计修复与性能计数器

统计修复和新增观测是本次重构的必需交付，不能只实现 PB 数据路径后继续沿用失真的 L1 预取结果。统计应支持分析容量压力、预取前瞻距离、有效使用、搬运等待和额外 replay，并能用于控制变量的性能研究。

### 现有 L1 预取统计审计

逐项检查 LoadPipe、MainPipe、MissQueue、PrefetcherMonitor/FDP、DCache metadata 和 PB 中的事件生产、来源传递、去重及消费逻辑。至少覆盖：

- 预取请求数、实际 MSHR 分配数、L2 请求数、拒绝/重复/合并和实际回填数。
- l1prefetchHitInCache、l1prefetchHitInMSHR、l1prefetchLate、l1prefetchUseless 及各预取来源子项。
- dcache_read_from_prefetched_line、dcache_first_read_from_prefetched_line、demand_miss、pollution 和相关命中率分母。
- PB 的 query_hit、hit、use、lane_use、first_use、promote、evict 等已有事件的含义和计数时点。

不能把所有旧计数器简单加上 PB.hit：有些统计按访问计数，有些按缓存块生命周期计数，有些按周期计数。实现时提供统计字典，列出最终层次名称、事件定义、计数单位、有效条件、去重规则、来源归属、适用范围，以及与旧计数器的对应关系。

区分 DCache array 的局部 miss 与 DCache+PB 的整体访问结果。PB 返回有效数据不能计为整体 L1 demand miss；PB busy/retry 不能计为已完成数据命中，也不能当作新的普通 miss 分配。分别保留阵列局部事件和整体 L1 事件，避免修正整体统计时丢失观察 DCache 覆盖变化的能力。

旧指标在可以保持定义时修复其 PB 覆盖；若旧名含义不足以表达新指标，新增清晰名称并在字典中说明，不静默改变旧指标单位。PB 路径的预取来源必须来自对应 PB 响应/事务，不能使用未命中 DCache way 的来源字段。

### 使用、来源与跨位置去重

区分以下事件：目录匹配、Load S1 授权、S2 响应、LSU dcacheUse、预取块首次有效使用、搬运成功。各自独立计数，不能互相代替。Load 使用类事件只有在对应 lane 的固定 S2 响应同时 valid 且 hit、并收到 dcacheUse 时才成立；kill、replay、无响应 use、全部 Store forwarding 等情况不产生有效缓存使用。

首次有效 Load 使用按 PB→DCache 生命周期去重，已寄存的使用标记及原始来源随搬运传递。按“完成身份与统计交接”的最新取舍，Store 搬运遗漏最后一拍使用是明确的例外：这类块可能在 PB 与 DCache 各计一次首次使用，不再宣称整个生命周期严格只计一次。其余已使用块仍按传递的 used 去重；尚未使用的块因 Store/维护搬入 DCache 后，后续 Load 仍归原预取来源。Store 命中/完成单独保留事件，不能把 Store 触发搬运或普通 Load 授权冒充有效 Load 使用。

同拍多路 Load 使用同一分配的条目，访问数按实际 lane 数计，首次使用按块只计一次；多路使用不同块则不能用简单 OR 漏计。统计按各路固定 S2 的授权、entryId 和来源关联；Probe/Release 完成沿只对当拍仍有效的使用事件分类，不保留退出后的身份快照。预取在 MSHR 被 demand 合并、转入 DCache 后，保留来源与“在途合并”事件，不能把合并既记作 PB 首次使用，又在 DCache 重复记为同一种首次受益事件。

当前实现将 `dcacheUse` 固定反馈到 PB Load 的 S2，并由 PB 保存该路已授权的 entryId；因此有效使用在对应 S2 沿确认，不允许任意迟到的使用通知。首次使用和退出统计使用该沿的旧 metadata 快照，不复活已失效或已复用的 entry。与 Probe/驱逐/搬运边界相遇时，先按功能状态机完成所有权转换，再按同一拍的有效使用事件进行观测归属。

PB→DCache 搬运不是一次新的 L2 预取回填，也不是未使用预取的最终丢弃。区分“PB 未使用就搬走”和“预取块最终未使用就退出整个 L1”。容量驱逐、Probe 退出和错误清理分别计数；错误块不得混入正常预取无用率。

### 必需的 PB 观测事件

以下是必须覆盖的事件族；名称可以按模块风格细化，但不能缺少其观测能力。计数器采用项目现有 XSPerfAccumulate/Histogram 等机制，必须出现在 CI/CR 使用配置的实际日志中。

| 事件族 | 必需内容 | 单位 |
| --- | --- | --- |
| 分配与等待 | PB 目的地 MSHR 建立、预留成功、无预留取消、有预留释放 | 事务/entry 次数 |
| 分配背压 | 有申请但无槽位的周期；无预留等待 MSHR 数的逐拍累加；数据已齐但无槽位的等待 | 周期、MSHR·周期，分别命名 |
| 回填 | 正常/denied/corrupt fill 接受、fill 仲裁等待和出口背压 | 事务次数、周期 |
| 查询与使用 | 有效查询、物理目录匹配、读授权、有效使用 lane 数、首次有效使用块数 | 访问次数或唯一块次数 |
| Load retry | 锁定状态、权限不足、上游错误等不可读原因 | 响应次数；原因是否互斥需注明 |
| MSHR 合并 | demand 早期合并、第一拍 Grant 同拍或之后拒绝、prefetch/Store 不可合并 | 被接受事务或被拒尝试，分别定义 |
| 搬运 | Load/Store/alias 等触发来源、尝试、成功、S2 abort、重试 | 次数；多原因需确定归因方式 |
| 主动调度带宽 | move_request 记录主动 S0 握手，move_request_back_to_back 记录相邻两拍均握手，move_release_parallel 记录两路同拍握手 | 发射次数；连续 N 拍贡献 N-1 次相邻事件，不代表完成次数 |
| 搬运等待 | 等待 MainPipe 入口、整行读、S3 写端口、WBQueue | 周期；允许重叠项不得直接求和当总延迟 |
| 退出 | 容量驱逐、Probe 退出、错误清理，分别区分已使用/未使用 | 唯一退出块次数 |
| Probe 与释放交错 | MQ owner 阻挡、同 set/物理块流水冲突、同拍 release 胜出、Probe 取消待释放、S3 WBQueue 等待 | 周期/事务分别计数；无新增 PB busy→S0 阻挡项 |
| 状态占用 | Reserved、Resident、锁定状态、Poison 数量的逐拍累加与分布，PB 全满周期 | entry·周期、周期直方图 |
| 新授权边界 | Load 授权与 Probe/Store/维护接管同拍的事件 | 访问次数，用于确认新规则实际覆盖 |

### 延迟与预取前瞻观测

至少记录以下周期延迟的样本数、总和及有溢出桶的直方图；起止事件必须在代码注释和统计字典中明确：

- MSHR 建立 → PB 预留成功。
- 整行 Grant 收齐 → PB fill 接受。
- PB 正常 fill 接受 → 首次有效 Load 使用。
- PB 正常 fill 接受 → 未使用容量驱逐，以及 → 未使用 Probe 退出，两类分别记录。
- 首次有效使用 → 搬运接管，以及接管 → 搬运成功；发生取消时另记尝试延迟和重试次数，不混成一次成功延迟。
- PB 预留 → 该预留释放或对应项退出，用于观察 Reserved 占用与总槽位驻留。

发生 Store/alias 先搬运、Probe 先退出等情况时，不制造不存在的“首次 Load 使用”时间戳。记录未使用就搬入 DCache 的数量，避免只看留在 PB 中被使用的样本。只凭 fill-to-use 平均值不能断定前瞻过远；分析必须结合未使用退出、容量和等待分布。

计数和时间戳不应驱动功能 ready、claim 或读授权路径，也不得为统计在关键路径增加大规模 CAM。重复 lane 去重、直方图和归因逻辑可在采样后寄存处理，但要保留正确事务身份。

### 统计窗口、反馈与验收

每个有效事件只在规定的握手或响应接受点计数；valid 保持导致的等待次数和事务完成次数严格区分。使用足够宽的计数与时间戳，明确直方图边界、计数溢出和时间戳回绕处理。

warmup 后清零统计不得清除功能上的 used、来源或 pending。延迟时间戳使用不会因性能计数清零而跳变的时间基准，或者显式跟踪统计 epoch；定义跨窗口样本如何归属。窗口内 first-use/fill 比值可能包含窗口前回填的块，不能强行解释为同批次预取准确率；需要输出边界占用或跨窗口样本数辅助解释。

逐项审查 PrefetcherMonitor/FDP 事件是否用于自适应调节。PB 扩展后的语义需在事件生产与消费两端一致，不能只修打印而留下错误反馈；不得顺便改变调节算法、阈值或预取距离。若正确事件反馈使调节轨迹变化，报告这一影响，不能将性能差异全部归因于 PB 容量或搬运。

必须用可精确预期事件的定向场景核对统计：PB 首次/重复使用、同项多 lane 使用、全部前递/kill/replay、同拍授权与 claim、合并转 DCache、搬运后再次使用、未使用驱逐、Probe 退出和 fill 错误。核对端到端来源、首次计数和实际日志输出，不能仅检查计数器存在或不为零。

首次预取使用率、PB 使用率、无用率等派生指标必须写出分子、分母和统计窗口；禁止将按访问统计的 hit 除以按块统计的 fill 后直接称为准确率。pollution 的 Bloom-filter 估计必须保留“估计”标识，不能作为已证明的因果结论。

交付计数器字典和 CI/CR 日志解析结果，至少能回答：填了多少块、多少实际被用到、多少未用就退出、PB 占用在哪里、等待槽位与等待搬运各占多少、预取回填后多久使用，以及 PB 引入多少 Load replay。

## 代码风格与可读性

代码应让读者直接看出请求来自哪里、处于哪一拍、何时生效以及为何需要该条件。不能以减少字符数代替清晰表达，也不能仅靠增加注释弥补含糊命名和混杂的逻辑组织。

当前 PB 实现中，mid、src、pending、assist、origin、bad 以及大范围使用的 m/n 等名称，脱离上下文后难以辨认具体含义；分配、授权、维护、错误处理和下一状态更新混杂，使得同拍事件及优先关系不易检查。后续实现须同时改进命名、分区和关键注释，而不只是机械重命名。

### 现有模块遵循本地风格

MainPipe、MissQueue 和 LoadPipe 中的修改必须跟随各自文件的命名与实现习惯，包括流水级前缀、组合条件拆分、RegEnable/RegNext 使用方式、握手判断、状态更新和注释格式。不能在这些文件中直接套用 PB 的另一套风格。

- MainPipe、LoadPipe 延续附近代码的 s0_/s1_/s2_/s3_ 等命名方式，新增 PB 条件放在相应阶段的现有逻辑旁边。
- MissQueue 延续其请求来源、分配/合并条件和事务完成标志的组织方式。新增“等待 PB 分配”“已有预留”“PB 回填已完成”必须是语义明确的独立条件，不能混成一个含糊的 pending。
- LSU 中新增 dcacheUse 跟随 NewLoadUnit 的命名、响应接受和 replay 判断风格，保持通用缓存语义。
- 不为统一 PB 风格而重命名周围无关代码，也不把所有 PB 改动集中追加到文件末尾。

### PB 的命名

PB 顶层按对端组织为 io.load、io.pipe、io.mshr、io.dcache，并保留独立的 PB→WBQueue releaseReq。MainPipe 的 PBPipeIO 使用“与 MainPipe 的接口”节列出的十二个扁平字段，s0_/s1_/s2_/s3_ 前缀直接表明阶段，不再额外套 s0/s1/s2/s3 Bundle。主动搬运使用 io.pipe.s0_moveReq，禁止另建顶层 moveReq 或与 release 共用一个 op 请求。

同一协议两端只维护一份 Bundle 定义：MainPipe 使用 Flipped(new PBPipeIO)，MissQueue 使用 Flipped(new PBMSHRIO)，Wrapper 直接连接。Load 的共同地址/数据定义可复用，但窗口宽度、读授权和流控须各自明确；不为接口对称增加重复 claim/readReq/status/dispatch。MainPipe 响应和 MSHR 预留/回填使用 entryId；Load 使用固定 S2 布尔反馈，由 PB 本地保存授权和条目编号，不增加身份往返字段。

PBLookupIO 与旧 PBProbeIO 删除；Probe 不返回 S0 busy/目录，S1 返回 locked/coh/entryId。不要用相同 hit 名称同时指“可读”和“Probe 锁定成功”。s2_storeResp.bits 表示 Store 是否锁定；是否携带已拥有的整行由 MainPipe 的 move 来源或 Store 锁定成功推导，dataBad 另指该行的数据错误。Store/move 共用 s2_moveAbort 和 s3_moveDone，Probe 独用 s3_probeDone。

s1_paddr 的 ready 表示阶段推进所需响应容量，不能解释为 hit 或 Store success。s1_storeReq 是同拍 sideband，由 s1_paddr.fire 接受。s0_moveReq/fire、s0_probeReq.valid、成功 Store 锁定沿分别定义实际拥有 entry 的时刻；等待仲裁不是已锁定。

s1_storeReq 表达普通 Store 在 DCache 未命中且 tag 无错误时的锁定尝试，不由 PB 返回 hit 反向生成。在 S1 等待时，PB 内容可能被直接 Release；Store 意图仍随原请求保持，是否成功由 PB 在实际 fire 沿判断。PB miss 的 Store 可以得到 success=false 且 retry=false，此时 MainPipe 按普通 miss 进入 MQ；存在 PB 所有权但不可搬运时才走 PB replay。

Bundle 字段注释须说明对端、阶段、有效条件、保持/取消规则和生效沿。此次包含功能协议变更，调用方已随之适配；验证结果与尚未覆盖的整机运行、STA 分别记录，不能把纯命名调整的验证范围套用于整个改动。

PrefetchBuffer 参考 NewLoadUnit、NewStoreUnit 的 lowerCamelCase 命名和功能分区方式。名称以“对象 + 动作/状态”为主，准确、适中，不使用过度缩写，也不把整个判断表达式拼成长名称。

以下是命名方向示例，不要求脱离实际语义逐字替换：

| 含义 | 建议命名 |
| --- | --- |
| 对应 MSHR 的编号 | missEntryId |
| 预取来源 | prefetchSource |
| 搬运意图尚待处理 | movePending |
| 因 alias 不匹配请求搬运 | 设置 movePending，不另存一份调度位 |
| MainPipe 持有的 Store 回复标识 | storeReplayId，随原事务保存 |
| entry 数据不可正常使用 | dataBad；若实际表示 ECC 检测事件则用 dataEccError |
| 本拍目录物理命中 one-hot | probeMatchOH |
| Probe 在 S1 返回的锁定结果 | s1_probeResp.locked |
| Probe 实际被 MainPipe 接收 | probeAccept |
| 当前/下一状态 metadata | entryMeta / nextMeta |
| 某路 Load 的 S1 读授权 | s1ReadAllowed |

保留 paddr、vaddr、MSHR、OH 等已有且含义清晰的术语；循环下标 i/j 可以保留在短作用域内。单字母别名不能跨越较长状态更新段，也不能让同一个 n 一处表示项数、另一处表示下一状态。

valid 表示请求或响应有效，ready/canAccept 表示许可，fire/accepted 表示本拍实际事件，pending 表示跨拍保存的待处理状态，不能混用。计数、entry id、one-hot 和按项位图必须能从名称或紧邻定义看出区别。

PB 是单个模块，跨阶段信号需要明确 s0/s1/s2 等前缀；已在独立、短小的局部作用域内表达清楚的量不必重复长前缀。这里的阶段前缀必须说明是 Load 查询阶段还是 MainPipe 操作阶段，不能将两者混为一条流水线。

当前内部使用 loadS1*/loadS2* 与 pipeS1*/pipeS2* 分别表示两条路径；Store/Probe/move 的接受和完成事件明确所属阶段。moveSelValid/Id 与 relSelValid/Id 分别保存两路主动请求的已选条目，Sel 表示选择，不代表锁定；被动接管会在该槽位可重新分配前撤销选择，因此只保存 entryId。moveMask/relMask 表示可选条目集合，moveSelReady/relSelReady 表示可更新选择寄存器，不能与对外请求的 ready 混用。两路使用独立 RR 仲裁并支持发出当拍补入下一项，不再使用共享的 selectedValid/selectedId/selectedIsMove。perf* 寄存器只用于观测，不能流回读授权、分配或 ready。

Move/Release 代码按“共用选择状态与条目资格 → Move → Release”组织。两路选择寄存器需要互相排除，集中在共用部分声明；每条路径按“当前请求 → 下一项选择 → 选择寄存器更新”独立排列，使用对称命名，避免交错书写。SelValid/Id 表示已选请求，Mask 表示仲裁输入集合，不用冗长的 Candidate 前缀，也不为缩短名字引入额外状态或接口。

PB 的内部结构按“存储阵列、访问路径、所有权控制、观测”分层。存储阵列只保存 entryMeta 和原始 entryData；Load、MainPipe、MSHR 和主动维护分别计算自己的查询与握手事件。各路径不直接修改 entryMeta，而是先产生按 entry 编号的一拍事件，例如 entryProbeLock、entryStoreLock、entryMoveLock、entryRefill 和 entryRel。统一状态机只消费这些事件并生成 nextMeta，再由单独的提交区更新 metadata/data；统计观察使用独立寄存器，不能参与功能决策。

MainPipe 相关逻辑按流水级分区组织：S0 集中处理 Probe 接收和 Move 发出，S1 集中处理物理块查询、Store 接管尝试、entryId 记录及 S1→S2 推进，S2 集中处理整行读取、响应输出和 Move abort，S3 集中处理 Probe/Move 完成通知。寄存器声明、组合判断和阶段更新应放在对应区域内；跨区域依赖可以先声明 Wire，再在所属阶段计算，不能为了文本顺序复制同一事件或改变原有状态更新优先级。当前 MainPipe 接口没有 S4 阶段，文档和实现统一使用 S3 完成沿。

同一 entry 的事件必须在状态机前集中解决。事件来源包括 MSHR 分配/取消/回填、Probe S0 锁定、Store S1 锁定、主动 move S0 握手、S2 abort、S3 done 和 WBQueue Release 握手。实现使用统一的每项互斥断言约束其同拍至多一个所有权事件；Probe/Store/move 的锁定事件仍分别保留，不能只用一个无来源的 claimed 位替代。状态转换的优先级和异常处理写在同一段中，不能依赖多个流程块中寄存器赋值的文本先后顺序。

### Bundle 文件组织与清理

PrefetchBufferBundles.scala 按以下顺序组织：枚举、公共身份和内部状态，接口请求/响应载荷，最后是 IO Bundle。PBMeta 必须在 PBLoadResp 等接口载荷之前。任意 A 使用 B 时，B 的定义必须在 A 之前，包括载荷依赖、继承和 IO 嵌套；分组不能破坏依赖顺序。

以当前对外 IO 为根检查文件内部引用，未被这些接口使用的旧接口载荷删除，不因其他文件中的旧实现尚有引用而保留。公共身份与必要内部状态单独保留。已删除 PBRes、PBFill、PBFillResp、PBResult、PBDir、PBMaint、PBClaim、PBLine、PBAbort、PBDispatch；删除 PBReadData 抽象层，Load 载荷直接定义在 PBLoadResp 中。已删除 PBLoadReq、PBLoadS1IO、PBLoadS2IO、PBLoadsIO、PBStoreReq、PBStoreResp 等冗余包装，单个 Bool 直接使用 Bool 或 Valid[Bool]。

PBEntryEvents 不再作为公共 Bundle：分配、取消、回填、Probe/Store/move 接受、完成、Load 使用与错误事件在 PB 实现内部以明确的局部 val 组织，组合事件只计算一次供状态更新、IO 和统计复用。删除包装不等于删除同项互斥检查；Load 使用与接管可以同拍发生，不能把所有事件一概当作互斥。

### PB 的代码组织

学习 NewLoadUnit、NewStoreUnit 的重点是声明、计算、更新、连接的组织顺序，不止是命名和注释。PB 保持一个核心模块，不要求每级单独 Module。除类参数及必要常量外，模块主体必须按以下顺序编排：

1. **IO 声明**。
2. **全部寄存器声明**，紧接 IO：entry 数据、metadata、流水上下文、响应保持和调度寄存器按用途集中列出。
3. **组合逻辑**：按读 tag、读 data、分配/取消/回填、Probe/Store 接管、搬运/驱逐、错误和低功耗等流程分块，先定义身份匹配、许可、选择结果与握手事件等有明确含义的 val。
4. **寄存器赋值与状态机转换**：使用前面定义好的组合条件，集中说明保持、更新、取消及同拍事件优先级；状态更新必须在 IO 赋值之前。
5. **IO 集中赋值**：位于性能计数器之前，按对端与流水级分组，尽可能只连接前面已经计算好的 val。
6. **性能计数器**：放在模块最后，复用已定义事件；本模块显式声明的统计辅助寄存器也遵循前面的声明/更新顺序。

不要在流程块里边声明 Reg 边更新、边连 IO。RegNext/RegEnable 等隐式创建寄存器的表达式也不能散落在后面的组合逻辑或 IO 赋值中；必要时在寄存器区声明 Reg，并在更新区显式赋值。辅助函数不得隐藏握手、副作用或寄存器更新。

IO 输出不直接堆叠长 Bool 表达式。复杂条件先拆成有业务含义的中间 val，计算最终结果，再连到 IO。内部消费者直接复用这些 val，不绕回读取本模块 IO 输出。示例：

```scala
// 组合逻辑
val refillReady = refillOwnerMatch && dataWriteReady
val refillFire = io.mshr.refillReq.valid && refillReady

// 寄存器赋值与状态转换
when (refillFire) {
  // 提交数据、权限与下一状态
}

// IO 集中赋值
io.mshr.refillReq.ready := refillReady

// 性能计数器
XSPerfAccumulate("refill", refillFire)
```

组合逻辑仍须按真实流程分块并标明阶段，不能为了 IO 集中赋值而把组合计算也挤到末尾。entry 下一状态在统一位置提交，按分配、回填、接管、取消、完成等事件清楚分组；合法并发及优先级必须显式表达，不依赖散落赋值的先后顺序。纯组合地址比较和窗口选择可用少量辅助函数，PB 不包含 ECC 编解码辅助函数。

共享物理块比较由同一个纯组合 matchBlock 定义；Load 的 S1 授权/S2 数据选择、MainPipe 的整行读取、Probe/Store 接管和主动请求选择分别组织。nextMeta 计算与 entryMeta/entryData 的寄存器提交分开；退出统计另有独立观测计算和寄存器更新段，不混入 entry 功能状态循环。

`matchBlock` 内断言 `PopCount(matchOH) <= 1.U`：第 i 位表示物理块匹配有效条目 i，允许未命中时全零，包括 Reserved 在内不能同时匹配多个条目。按条目生成事件直接使用 `matchOH(i)`。与已寄存的候选 ID 比较时使用 `matchOH(relSelId)`；比较两路当拍匹配结果时使用 `(matchOH1.asUInt & matchOH2.asUInt).orR`，避免先编码再比较或索引。保留各路径的请求有效、命中和握手条件，确保全零向量不会被当作条目 0 命中。需要保存或返回 entryId 时使用 `OHToUInt`；空闲项分配允许多位有效，仍使用优先选择。

### 注释要求

参考 NewLoadUnit、NewStoreUnit 的块注释和行内解释方式。PB 及本次重构在调用方新增的说明性注释统一使用中文，保留信号名、状态名、协议术语和许可证原文。每个主要功能区说明职责，关键边界说明原因，而非逐行翻译赋值语句。

必须在对应逻辑附近讲清楚：

- 接口的生产者、消费者、阶段对齐，以及 valid 是否意味着数据可用。
- 寄存器何时更新、何时保持，响应背压时由谁保存数据。
- 为什么同拍 Load 授权可以排在 claim 之前，下一拍如何阻挡新授权。
- 为什么同物理块检查足以排除两个接管来源的同项竞争。
- 为什么 MQ owner 不依赖是否已取得 PB 预留，以及 fill 当拍到下一拍的保护交接。
- 分配取消、错误、abort 和 finish 的状态变化及不能合并处理的情形。

注释必须反映目标实现，删除失效的“claim[0] 优先”或“当拍 claim 一律禁止 Load”等旧说明。采用 one-hot 时解释其位与 entry 的对应关系，不仅写“优化时序”。命名重构与代码分区不能改变寄存边界、握手行为或同拍优先关系；必要的功能变化应能与纯可读性调整分别审阅。

## 配套修改

### 本版需要落实的修改

| 修改 | 主要涉及模块 |
| --- | --- |
| MSHR/L2 请求与 PB 分配解耦，持续申请并处理无预留合并取消 | MissQueue、DCacheWrapper、PB |
| 预取绕过 MainPipe 直填 PB，增加 fill 仲裁和直接完成条件 | MissQueue、DCacheWrapper、MainPipe |
| MQ owner 不依赖是否取得 PB 预留，保持 fill 发布与 Probe 无空档交接 | MissQueue、PB、MainPipe |
| 十二字段 PBPipeIO：S0 Probe 单向通知、S1 注册结果、共享搬运数据与终结通知 | MainPipe、DCacheWrapper、PB |
| move 在实际 S0 接收时锁定，复用同物理块流水互斥 | MainPipe、PB |
| PB 直接 release 接入 WBQueue，修正仲裁 ready_dup、同拍 Probe 结果及 release 通知 | PB、DCacheWrapper、WBQueue |
| 允许同拍 Load 授权与接管，下一拍按 锁定状态 阻挡 | PB |
| LSU 改为统一缓存响应及 dcacheUse，PB 身份留在缓存内部 | LoadPipe、DCache 接口、NewLoadUnit |
| 遵循各文件本地风格，重整 PB 命名、功能分区和关键时序注释 | MainPipe、MissQueue、LoadPipe、PB |
| 修复 L1 预取统计与反馈语义，增加可解释的 PB 事件、占用和延迟观测 | PB、LoadPipe、MainPipe、MissQueue、PrefetcherMonitor/FDP |

原有 S0/S1/S2 Load 数据流程、Store 搬运/replay、S2 取消和 S3 背压、Probe 的 BtoN/TtoN 响应及 release 通知继续保留，并随接口变化核对。

目标行为是否正确以及时序是否满足要求，须由对应 RTL 验证与综合/STA 结果确认；现有仿真通过记录不能替代修改后结果。
