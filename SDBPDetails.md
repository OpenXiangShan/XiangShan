# L2 SDBP 详细实现计划
TODO: Sampler Cache 暂时用寄存器实现，以避免读写冲突的问题
## 基准与规则

- RTL 基准：XiangShan `59ef4ea7d4`，XSCache `857c2c2a576de4c016a2b10e6ad241123901291e`。
- 算法基准：[Actions run 40859791862](https://github.com/OpenXiangShan/GEM5/actions/runs/40859791862)，commit `39a2db6f92f0b09a470f8027260777bcc7c68a30`，worktree `~/GEM5-sdbp-sampler-young2-ci`。
- L2 1 MiB，4 个 256 KiB、8-way slice，64 B 行，每 slice 512 set；关闭所有预取及 bypass。L3 保持本项目 RTL 的 exclusive 方案，整机性能不直接视作该 GEM5 run 的复现。
- 每 slice 独立 sampler/predictor。SamplerCache 的 set/way 由 `samplerSets/samplerWays` 独立配置，默认 `samplerSets=128`、`samplerWays=max(1, l2Ways*3/4)`（8-way L2 对应 6-way sampler）；victim 为 **invalid → 按 way 序首个 dead → 全 set 真 LRU**；guard0，不保护 MRU/次 MRU，不使用 L1-copy mask。
- SCArray 为 3 表 × 4096 项 × 2 bit，初值 0，三表之和 ≥ 8 判 dead。signature 为 `xor_fold(PC >> 1, 15)`，表索引先对齐 GEM5 的 `mixed` hash/seed。
- sampler demand hit：旧 signature 训练 live；sampler miss 替换有效 entry：旧 signature 训练 dead；均要求旧 `signatureValid=1`。dead：三表饱和加一；live：表 0/2 饱和减一，表 1 右移一位。
- L2 victim 为 **可用 invalid → 按 way 序首个 dead 且无本核 L1I/L1D 副本的行 → 可用 way 中真 LRU**；copy 保护不清 dead，也不限制 LRU 回退。

## 总体结构：与 L2 同步的三拍流水线

SamplerCache 不设 EventQ、不缓存写请求、不使用跨多拍事务的 set 锁。它与 L2 Tag/Meta 同步读写；只有单端口 SRAM 实现的 SCArray 需要 UpdateQ，保存旧 signature 的训练操作。

| 阶段 | L2 | SamplerCache | SCArray 预测通路 |
| --- | --- | --- | --- |
| s1 | Directory 发起 tag/meta 读 | 抽样请求读对应 set 的 `samplerWays` 路 entry，同时取独立 LRU | 以当前请求 signature 读三表；无有效 PC 时不查 |
| s2 | 锁存 tag/meta 响应 | 锁存 `samplerWays` 路结果和 LRU，比较 partial tag、形成目标 way/旧 signature | 锁存三表读值及前递快照，形成当前预测 |
| s3 | 判定 hit、refill 是否真正安装，完成原有写操作 | 有效访问直接 masked write 新 signature/dead，更新 LRU；产生旧 signature 训练 | 寄存器版直接训练；SRAM 版把训练送入 UpdateQ |

```text
                  s1                     s2                       s3
L2 请求 ──→ Tag/Meta read ───────→ 锁存响应 ───────────────→ hit/refill 判定及写入
       ├──→ SamplerCache read ──→ entry/LRU/tag compare ──→ 单路写新 sig/dead
       └──→ SCArray[currentSig] → 锁存计数/预测 ──────────→ 写 sampler/L2 dead
                                                           │ oldSig + live/dead
                           寄存器版：三表组合读旧值并直接更新 ←─┤
                           SRAM 版：UpdateQ → 训练读 → 训练写 ←─┘
```

s1 的预测查询与后续旧 sig 训练是两种独立操作，不能拿当前 sig 的三表读值去更新旧 sig。当前 signature 和三个索引尽量在入 s1 前算好，旧 signature 的 hash 及寄存器读改写路径需单独检查时序。

### 预测的观察时点

首版两种 SCArray 实现采用同一个定义：**预测使用 s1 发读时的逻辑计数状态**；s1 时已入 UpdateQ、尚未落 SRAM 的更早训练通过前递计入。s1 当拍 s3 新产生的训练按时钟边沿在随后生效，不计入本次 s1 快照；寄存器版也采用相同的边沿顺序。

预测与训练的对象不同，二者不建立等待关系：

- 当前请求携带的新 sig 用于预测这次访问之后该行是否还会复用。
- 当前访问若命中 sampler，训练该 entry 中保存的旧 sig 为 live；若替换有效 sampler victim，训练 victim 的旧 sig 为 dead。标签评价的是旧 sig 所代表的上一次访问。
- 新 sig 写入 sampler 时只保存 signature 和预测，不给它生成 live/dead 训练标签；以后发生复用或替换时，才得到相应标签。新旧 sig 可能相同或 hash 碰撞，但不因此要求当前预测等待旧 sig 更新。

因此保持 s1 查询、s3 更新 sampler，旧 sig 训练独立提交或排队；不增加等待训练的重读/重放。GEM5 同一次函数调用中的“先更新旧 sig、再查询新 sig”只作为软件执行顺序记录；发生索引碰撞时的可见性差异在时序对照中检查，不作为新增硬件依赖。

## SamplerCache：单端口、按路写，无写队列

### 参数与派生量

`L2Param.scala` 定义参数，`Configs.scala` 配置；均在 elaboration 时确定，每 slice 使用同一配置。记 `S=samplerSets`、`W=samplerWays`、`L=每 slice 的 L2 set 数`。

| 参数/派生量 | 定义与约束 |
| --- | --- |
| `samplerSets` | 默认 128；2 的幂，`2 ≤ S ≤ L`，与 GEM5 的 `sampler_num` 对齐；`L` 也须为 2 的幂。 |
| `samplerWays` | 默认 `max(1, l2Ways*3/4)`，整数除法向下取整；可显式覆盖为任意正整数，与 GEM5 的 `sampler_assoc` 对齐，不要求为 2 的幂。 |
| `samplerSetBits` | `h=log2Ceil(S)`；用于 SRAM 地址、流水寄存器及 reset 计数。 |
| `samplerWayBits` | `max(1, log2Ceil(W))`；way 编码范围为 `[0, W)`，所有 waymask/命中向量宽度为 `W`。 |
| `samplerLruBits` | 每 set 为 `W*(W-1)/2` bit；`W=1` 时省去 LRU 存储和计算，唯一候选为 way 0。 |

命中比较、invalid/dead 优先选择和断言均只遍历 `0 until W`，不补齐成 2 的幂路数。`W>1` 使用 `TrueLRU(W)`；可选最近两项保护若开启，保护 `min(2,W)` 项，仅限制 dead 扫描，LRU 回退仍可选择全部 way。

### 存储组织

- 复用 L2 已有的按路写接口：`Directory.scala` 的 Tag 使用 `SplittedSRAM`，Meta 直接使用 `SRAMTemplate`；两者均将 `wayOH` 传入底层 SRAM 的 `waymask`。
- 实例化 `SplittedSRAM(new SamplerEntry, set=S, way=W, setSplit=1, waySplit=1, singlePort=true, readMCP2=false)`；每路为 `valid/partialTag(15)/signature(15)/signatureValid/dead`，共 33 bit，entry 存储量为 `S*W*33` bit。
- 每 set 的 `samplerLruBits` 位真 LRU 独立存寄存器；正常写 `wayOH=UIntToOH(selectedWay, W)`，只更新目标 entry，其余 `W-1` 路不变。`W=1` 时 way 固定为 0、写 mask 为 `1.U`。
- 参照 `Directory.scala` 中 DRRIP 的高、低 set 位比较：令 `setBits=log2Ceil(L)`、`samplerBits=log2Ceil(S)`、`compareBits=setBits-samplerBits`。`compareBits>0` 时，`sampled = localSet(setBits-1, samplerBits) === localSet(compareBits-1, 0)`；索引为 `localSet(samplerBits-1, 0)`。只取 DRRIP 的相等分支，不取互补分支；两段位选按参数生成，位域重叠时仍按原式比较。`S=L` 时直接令 `sampled=true.B`，避免零宽位选。默认 `L=512/S=128` 时比较 `localSet(8,7) === localSet(1,0)`；partial tag 仍取 GEM5 tag 的低 15 bit。
- reset 遍历 `S` 个 set，用 `W` 位全 1 mask 写零清 valid，同时清 LRU；正常更新的 mask 必须 one-hot。sampler dead 保存访问时的预测，不随之后 SCArray 训练自动刷新。

### 读写冲突与流水线 RAW

**复用 L2 现有的 Directory 写阻塞和 MCP2 入口间隔，不另设 sampler 仲裁器、写队列或 set 锁。**SamplerCache 与 Directory 使用同一次 s1 fire，s2 锁存、s3 写回。

1. **同拍读写：复用 `Directory.io.read.ready`。**当前条件为 `!metaWReq.valid && !tagWReq.valid && !replacerWen`，写任意 set 都阻止 s1 发读。sampler 与这些写同拍提交时，读写冲突已经被覆盖，无须再产生 `samplerReadReady` 或重复阻塞。
2. **补齐写覆盖关系。**Acquire hit、成功 refill 已有替换状态写。当前 Get hit 不在 `updateHit` 中；按本计划的 L2 recency 规则，将 Get hit、预取 hit 纳入替换状态更新，使所有 sampler 写满足 `samplerWen -> (metaWReq.valid || tagWReq.valid || replacerWen)`，并用断言检查。若实现中保留不写上述状态的 sampler 更新，再仅为这类情况将 `samplerWen` 合入同一个 Directory ready 条件。
3. **相邻拍 RAW：复用 `RequestArb.ds_mcp2_stall`。**当前非 A-Hint 请求 s1 fire 后，下一拍 `s2_ready=0`，后继不能在前一请求处于 s2 时读 sampler；再下一拍前一请求进入 s3，其写由上述 Directory ready 阻塞。当前关闭预取的配置无需额外的 sampler set 比较器或 s3→s2 前递。抽样映射在被采样的 L2 set 与 sampler index 之间一一对应，不会额外引入不同 L2 set 的索引别名。
4. **Hint 例外。**当前 A-Hint 不触发 MCP2 间隔；以后开启预取且允许其更新 sampler 时，让需要访问 sampler 的 sampled Hint 也触发现有入口间隔。sampler prefetch hit 虽不更新，s1 尚不能判定，按候选访问保守处理。非抽样 Hint 保留原行为。
5. **提交与初始化。**s2/s3 已进入任务照常完成，s3 使用锁存结果写回，不等待新请求。sampler reset 与 L2 初始化统一禁止入口，完成后再放行。L2 dead 首版独立存寄存器，s3 同步写；victim 使用最新 dead，普通 tag/meta hazard 继续复用 L2 原有机制。

### s3 写入与训练条件

`sdbpAccess = 有效 demand/prefetch tag hit 或成功安装的新 tag refill`，由 MainPipe 的实际接受条件生成一次性脉冲：

- 初始 L2 miss：s1 可读 sampler/SCArray，s3 结果作废，不写 sampler、不训练；将来成功 refill 才更新。
- tag hit 即使还需 MSHR 处理权限，也只在这次 hit 更新；后续仅完成权限、未安装新 tag 的响应不再训练。
- refill retry/denied/取消/无实际安装不更新；普通及合并 refill 使用 MSHR 最终 demand/PC 元数据。
- sampler demand hit：取旧有效 signature 训练 live；sampler miss：取有效 victim 的旧有效 signature 训练 dead。新 entry 保存当前 signature、当前预测，置 MRU。
- 无 PC demand 仍可训练旧 signature，但新 `signatureValid=0/dead=0`；无效 sampler victim 不训练。预取 sampler hit 不训练、不写 entry/LRU；预取 miss 可训练 victim，但新 signature 无效、dead=0。
- 非抽样 set 不读写 sampler、不产生训练，带 PC demand 仍查询 SCArray 并更新 L2 dead。Release/Probe/Snoop/CMO 不训练 sampler；失效清 L2 dead，普通一致性 meta 更新保留 dead。

## SCArray 方案一：寄存器

每 slice 用三张 `Reg(Vec(4096, UInt(2.W)))`，共 **24576 bit，即 3 KiB 原始状态**；四 slice 为 12 KiB。此处仅计存储位，实际面积/功耗还包括触发器、译码、多路选择与布线，不能按同容量 SRAM 估算。

- s1 以当前 sig 的三表 index 组合读取，s2 锁存结果。
- s3 得到 sampler 旧 sig 后，**另外组合读取旧 sig 的三表计数值**，计算饱和加减/右移，于边沿更新各表目标 entry。每 slice 每拍至多一个 s3 训练。
- 每表逻辑上需要当前 sig 的预测读、旧 sig 的训练读和训练写；寄存器允许构建这些访问，无单端口 SRAM 的预测/训练端口冲突，不需要 UpdateQ。
- 训练读取当前寄存器值，不能复用本请求 s1 的预测快照。连续训练同一 counter，后一次须读取前一次边沿更新后的值；不得丢失增减。
- 同拍预测读与训练写采用前述边沿顺序；若增加 write-through，必须同时修改 SRAM 版前递定义和对照模型。
- 风险在 s3 的“sampler 旧 sig → hash → 三表组合读 → 更新”路径和寄存器规模；先综合评估。必要时把旧 sig 的 hash 提前到 s2，但仍不改变 s3 的 sampler 写时点。

## SCArray 方案二：单端口 SRAM＋UpdateQ

### 队列内容与更新状态机

三块独立 4096×2 bit 的 1RW SRAM，三表作为一组同步操作。**预测读、旧 sig 训练读、旧 sig 训练写共用同一组端口。**三表共同 ready 后才能原子发起一组操作。

UpdateQ 首版深度 8，FIFO 保存 `{seq, oldSig 或三表 index, live/dead}`。这里训练尚未读出计数值，因此不能只排队一个已经算好的 `newValue`；也不能误用当前 sig 的读值。每次 sampled 访问最多入一项。

```text
PENDING → READ_OLD → WAIT_RESP/COMPUTE → WRITE_NEW → 出队
```

- 只处理队头，一个 RMW 在途；读出三表旧值，按操作计算，放入队头的 `newValue[3]` 寄存器，再等待写端口。
- 三表写都接受后才出队。队头包括 READ/WRITE 在途阶段始终占一项，不因发出读就提前释放容量。
- 物理更新严格 FIFO；队头训练读基于已落 SRAM 的更早训练，不应用队头操作两次，也不能把队头之后的年轻操作折回本次物理写。
- 不合并 live/dead 为净增量；饱和更新与右移的执行顺序会影响结果。连续同 index 的训练必须按原顺序逐项执行。
- 暂停 s1 预测请求不影响队头读改写，s3 已进入任务仍可入队。SRAM 未接受读/写时保持请求和状态，不丢操作、不重复出队。

### 容量预留与背压

s3 不能临时停住等待队列，故在 **s1 sampled 候选请求 fire 前**预留一个最坏情况的训练名额，深度约束为：

```text
qCount + reserved ≤ UpdateQDepth
qCount 包括正在读改写、尚未完成写入的队头
reserved 包括 s1/s2 在途、尚未到 s3 判定是否训练的请求
```

- s3 产生训练：reserved 转成一项队列占用；无训练、初始 miss、retry 或取消：归还 reserved。每项训练只入队一次。
- 队列满时不接新 sampled 候选，继续处理队头以腾空间；非抽样预测仍受 SCArray 端口仲裁，但不预留训练名额。
- 同拍 s3 入队、写完成出队、s1 新预留按净变化更新计数；可首版保守地下一拍再利用释放名额。不能只检查当前 `full` 而漏算在途请求，也不能使用覆盖旧项的 `OverwriteQueue`。
- 这是 **SCArray 训练队列的容量预留**，不缓存 SamplerCache 请求，也不延迟其 s3 写入。

### 预测前递：待执行操作也必须计入

若预测只读物理 SRAM，队列积压越久，预测看到的训练就越旧。为对齐寄存器版的 s1 逻辑快照，查询表 t/index a 时，将当时尚未物理完成的匹配操作按 FIFO 顺序叠加：

```text
value = SRAM[t][a] 的本次读响应
for op in s1 时刻 UpdateQ 的快照，按旧到新：
    if op.index[t] == a:
        value = train_t(value, op.live/dead)
```

- 三表分别匹配 index；不同 signature 可能只在一表碰撞。不能只匹配完整 signature。
- **不能仅取最新一项的 live/dead，也不能只前递加减次数。**例如初值 2，先 dead 再 live，偶数表为 2、奇数表为 1；倒序执行则两者均为 2。
- s1 发预测读时锁存匹配操作的有序快照，s2 对读响应执行 fold；队头在随后出队也不能丢掉这份前递信息。快照不含 s1 当拍 s3 新生成的训练，与寄存器版边沿定义一致。
- 首版始终以本次 SRAM 预测读响应为基值，队头若尚未写入也按原操作 fold 一次；即使已算出队头 `newValue`，也不在原 fold 上再叠加该绝对值，以免重复更新。
- 预测读与训练写不能同拍，所以其 SRAM 基值和队列快照有明确对应关系：查询前已经写完的操作不在队列，尚未写完的操作仍在队列。三表原子写和出队必须同步。
- 队头 WRITE_NEW 的 `newValue` 可作为后续优化的绝对值前递来源，但使用它后只能继续 fold 其后的操作，不能再包含队头。操作型队列不能照搬“所有命中则完全省掉 SRAM 读”的绝对值写缓冲规则。
- 深度 8 的逐项匹配/fold 会增加 s2 逻辑，应综合检查；优化可用等价的 2-bit 状态转换表压缩组合链，但需保持次序，不为省逻辑而漏掉 pending 更新。

### 单端口仲裁与进度

三表每拍最多一组命令；端口仲裁在 s1 接受前决定，不能接受 L2 请求后才撤回其预测读。

| 优先级 | SCArray 操作 |
| --- | --- |
| 1 | reset/维护，期间禁止相关 L2 请求进入 |
| 2 | 已读到结果的队头训练写，尽快完成 RMW 并释放队头 |
| 3 | 强制训练读：占用达到高水位 6 或队头等候达到 8 拍时，暂停新预测读，让队头前进 |
| 4 | 当前请求预测读 |
| 5 | 空闲拍发队头训练读 |

- grant 给训练读/写的拍，阻止需要 SCArray 预测读的 s1 候选；原流水线 s2/s3 照常完成，SamplerCache 的 s3 写不受影响。不需要 SCArray 读的任务可按其他端口条件推进。
- 读响应返回、计算更新值或 s3 入队是寄存器操作，可与其他合法 SRAM 命令并行；只有实际 SRAM 发读/写占端口。
- 单个 RMW 的训练读完成后保证能拿到写槽，不能被持续预测读饿死；高水位和年龄阈值需参数化，队列空闲也应主动排空。
- 若 UpdateQ 深度小于 8，调整高水位使其不超过深度；reset 清三表及 UpdateQ/预留，全部 initDone 后再接请求。
- 每个需要训练且预测不能省略的访问平均占一组预测读、一组训练读和一组训练写，端口带宽上限约为每 3 拍一次；队列仅吸收突发，持续超载必须背压。寄存器版没有这项单端口限制，但仍受 sampler/L2 的单端口写拍约束。

## 两种实现的取舍

| 项目 | 寄存器 SCArray | 单端口 SRAM SCArray |
| --- | --- | --- |
| 原始计数状态 | 每 slice 3 KiB 寄存器 | 每 slice 3 KiB SRAM |
| 旧 sig 训练 | s3 独立组合读改写 | UpdateQ 按 FIFO 读改写 |
| 预测/训练资源冲突 | 无 SRAM 端口冲突；需构建多读通路 | 共用端口，需入口仲裁 |
| UpdateQ 与容量预留 | 不需要 | 需要，包含在途 RMW 与 s1/s2 预留 |
| 待训练前递 | 已提交训练自然可见 | s1 队列快照，按序 fold |
| 主要代价 | 触发器面积、组合读及旧 sig 路径时序 | 训练占端口、前递逻辑及队满背压 |
| SamplerCache 提交 | s3 单路写，无 EventQ | 同左，不等待队头训练完成 |

先用寄存器版验证算法和三拍接入，再以同一 s1 快照定义实现 SRAM 版；两者使用相同查询/训练序列对拍，并分别统计入口停顿，不能只比较最终计数而忽略预测可见性。

## 改动模块

L2 路径为 `XSCache/src/main/scala/coupledL2/`，核内路径为 `src/main/scala/xiangshan/`。

| 模块 | 修改 |
| --- | --- |
| 新增 `SDBP/SamplerCache/SCArray` | s1/s2/s3 控制；sampler 容量、地址/way 位宽、waymask、比较器、LRU 和 reset 由 `samplerSets/samplerWays` 推导；SCArray 可选寄存器/SRAM 后端，SRAM 后端增加 UpdateQ/FSM/快照前递。 |
| `src/main/scala/top/Configs.scala` | 1 MiB/4 slice/8 way、关闭预取、开启 PC；配置 `samplerSets/samplerWays`（默认 128 / `max(1, l2Ways*3/4)`）、guard0；选择 SCArray 后端及 UpdateQ 深度。 |
| 核内 `cache/dcache/DCacheWrapper.scala`、`mainpipe/MissQueue.scala` | 复用 PCKey，补逐请求 pcValid，核对 Acquire 的 PC 传递；无单条 PC 的 Store Buffer 请求标无效。 |
| L2 `L2Param.scala/Common.scala/SinkA.scala` | 定义 `samplerSets/samplerWays`、派生位宽与合法性检查，传给 SDBP 及 bundle；准备当前 signature/index，透传 PC 有效位。 |
| L2 `RequestArb.scala/Directory.scala` | 复用 Directory 写阻塞与 MCP2 入口间隔，补齐 Get/Hint 的覆盖条件；合入 SRAM SCArray 的端口 grant 及训练队列预留，统一实际 s1 fire。 |
| L2 `MainPipe.scala/Slice.scala` | s3 判断接受 hit/成功安装；直接写 sampler/L2 dead，直接训练或入 UpdateQ，取消无效预留；不引入异步 sampler 事务。 |
| L2 `MSHR.scala/RequestBuffer.scala/MSHRCtl.scala` | 最终 demand signature/pcValid 及合并规则，补普通 mp_grant.pc，避免权限完成重复训练。 |
| L2 `Directory.scala/utils/Replacer.scala` | 在有效、可替换 mask 内做 invalid/dead/真 LRU 选择；读最新 dead，失效清零。 |
| 新增 `L1ICopyTracker`，核内 ICache 与 tile 连接 | 导出真实安装/替换/失效/fence.i；D-copy 暂复用 clients，核对副本记录及保守误差。 |

## 验证

1. sampler：覆盖 `S∈{2,64,128,L}`、`W∈{1,3,6,8,12}` 的组合；检查 `W` 路读、单路 mask 写及其他路保持、LRU 同步提交和 invalid/dead/LRU 优先级；way 编码不越界、`W=1` 无 LRU、reset 覆盖全部 entry；检查 sampler 写被 Directory ready 覆盖，非 Hint 的相邻拍 RAW 被 MCP2 间隔覆盖；开启预取时覆盖 sampled Hint 的间隔。抽样遍历全部 `L` 个 set，恰好采样 `S` 个且索引一一覆盖；非 2 次幂/越界 set 数及非正 way 数在 elaboration 拒绝。
2. 接入：初始 miss 不改 sampler、hit 只更新一次、成功 refill 才更新；retry/denied/取消不训练、权限完成不重复；普通/合并 refill 的有效 PC 正确。
3. 寄存器版：预测 sig 与训练旧 sig 不同、连续同 counter 更新、同拍读写边沿顺序、部分表 hash 碰撞；检查 s1 与 s3 组合时序及面积。
4. SRAM 版：`qCount+reserved≤depth`，每个预留恰好入队或归还，队头写成功才出队；预测/训练读/训练写每块 SRAM 不重叠；持续预测下队头仍可完成。
5. 前递：相同 index 多项 live/dead、饱和/右移交错、只有一表碰撞、读在途时队头出队、同拍入队出队、环形回绕；比较 s1 逻辑快照，而非返回时队列现状。
6. 两后端用相同带时序的查询/训练序列对拍；再对照 GEM5 分清流水延迟与实现错误。统计 UpdateQ 峰值、满/预留不足停顿、训练抢端口、Directory/MCP2 入口停顿、SCArray 前递命中及 L2 性能。
