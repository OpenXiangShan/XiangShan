# L1 Demand Block Predictor 设计与实现方案

## 1. 目标

L1DBP 用于预测一个 cache block 重填到 L1 DCache 后，在离开 L1 前是否会发生二次访问。

本设计采用以下定义：

- 预测对象是一次 L1 residency，而不是该物理块的整个生命周期。
- demand load miss 使用 `Hash(PC)` 进行预测。
- L1 Stream/Stride prefetch 使用原始预取来源进行预测，不查询 PC Predictor。
- MSHR 中合并进来的 demand 请求算作该 block 已被访问。
- refill 后的 demand load/store/AMO hit 算作访问，prefetch hit 不算。
- 只使用 sampled L1 sets 训练预测器；所有 L1 sets 都产生预测并统计全局准确率。
- 第一版只统计预测效果，不改变 replacement、insertion、bypass 或其他缓存管理策略。

## 2. 参数

新增参数类，建议放在 `L1DBP.scala`：

```scala
case class L1DBPParams(
  sampleBits: Int = 2,
  pcPredictorEntries: Int = 1024,
  pcHash: (UInt, Int) => UInt = (pc, width) => XORFold(pc >> 1, width)
)
```

参数语义：

- `sampleBits = n` 表示采样比例为 `1 / 2^n`。
- `pcPredictorEntries` 必须是不小于2的2的幂。
- PC Predictor index 宽度为 `log2Ceil(pcPredictorEntries)`。
- `pcHash` 在 elaboration 时传入任意 lambda，调用形式为 `pcHash(pc, indexWidth)`。
- hash 返回值宽度必须等于 PC Predictor index 宽度。
- 2-bit counter宽度、初值和更新算法在第一版固定，不参数化。

必要约束：

```scala
require(sampleBits >= 1 && sampleBits < idxBits)
require(pcPredictorEntries >= 2 && isPow2(pcPredictorEntries))
// 在有实际PC UInt的Module中检查hash结果宽度。
require(pcHash(pc, log2Ceil(pcPredictorEntries)).getWidth == log2Ceil(pcPredictorEntries))
```

在 `DCacheParameters` 中增加：

```scala
l1DBPParams: Option[L1DBPParams] = None
```

`None` 时不生成相关硬件；实验配置显式设为 `Some(L1DBPParams(...))`。

不再单独设置 `enable`，避免 `Option` 和 `enable` 出现互相矛盾的两层开关。

## 3. Sampled set选择

设 L1 set index 宽度为 `W = idxBits`，`n = sampleBits`。sampled set 判定为：

```text
set[W-1 : W-n] == set[n-1 : 0]
```

两个区间允许重叠。例如 `W=8, n=7` 时：

```text
set[7:1] == set[6:0]
```

该条件形成7个相邻位相等约束，只有全0和全1两个 set 命中，因此采样比例仍为 `1/128`。重叠合法；唯一不允许的是 `n == W`，此时字段与自身比较，所有 set 都会命中。

命中采样条件后，高 `n` 位可由低位递归确定，因此紧凑 Sample SRAM index 为：

```text
sampleIndex = set[W-n-1 : 0]
numSampleSets = nSets / 2^n
```

建议在 `L1DBPParams` 或 `L1DBPHelper` 中统一提供：

```scala
def isSampleSet(set: UInt): Bool
def getSampleIndex(set: UInt): UInt
```

MainPipe、Sample Array和Monitor必须使用同一组函数，不能各自复制采样公式。

## 4. 数据结构

### 4.1 Counter Predictor

Counter Predictor 使用寄存器阵列，不使用 SRAM，也不需要初始化状态机。

```scala
val pcCounters = RegInit(VecInit(Seq.fill(pcPredictorEntries)(2.U(2.W))))
val pfCounters = RegInit(VecInit(Seq.fill(2)(2.U(2.W))))
```

Prefetch Predictor映射：

```text
0 -> Stream
1 -> Stride
```

计数器语义：

```text
counter == 0 -> predict dead
counter != 0 -> predict reused
initial       -> 2

actual accessed -> counter := 3
actual dead     -> counter := max(counter - 1, 0)
```

寄存器阵列允许同周期读取新 block 的预测项、读取旧 block 的训练项并写回训练结果，不引入端口阻塞。

同周期查询和训练命中同一 entry 时，查询使用训练后的值：

```scala
queryCounter := Mux(
  trainValid && queryTable === trainTable && queryIndex === trainIndex,
  trainedCounter,
  rawQueryCounter
)
```

PC hash碰撞有意共享同一个 counter，不保存 PC tag。

### 4.2 Sample Entry SRAM

Sample SRAM只覆盖 sampled sets，且只由 MainPipe访问。

```scala
class L1DBPSampleEntry(indexWidth: Int) extends Bundle {
  val valid = Bool()
  val payload = UInt(indexWidth.W)
}
```

`payload` 的解释依靠该 cache line 当前 meta 中的 `pf_source`：

```text
meta.pf_source == L1_HW_PREFETCH_NULL:
    payload = Hash(PC)

meta.pf_source == L1_HW_PREFETCH_CLEAR/STREAM/STRIDE:
    payload = refill时保存的原始预取来源
```

即使当前 meta 已经变为 `CLEAR`，仍然可以判断该 line 原本由预取带入；Sample Entry 中保存的来源必须是未被清除的原始 `STREAM` 或 `STRIDE`，不能保存 `CLEAR`。

建议将原始来源压缩为：

```text
Stream -> 0
Stride -> 1
```

并零扩展到 `payload` 宽度。Demand hash恰好等于0或1不会产生歧义，因为 payload类型由 meta `pf_source` 决定。

Sample Array组织为：

```text
depth = nSets / 2^sampleBits
ways  = nWays
data  = L1DBPSampleEntry
port  = single-port SRAM
```

每次读取一个 sampled set 的全部 ways，s1 根据最终 victim/tag-match way选择 entry。每次确实开始新residency的sampled refill都写目标 way：

- Demand load或Stream/Stride refill：`valid := true`。
- Store/AMO miss及其他不支持来源：`valid := false`，防止使用旧payload。

Sample SRAM不要求复位清零。只有 coherence-valid victim 才允许训练，而每个变为valid的 sampled line都会在 refill commit 时覆盖对应 Sample Entry。增加断言保证该不变量。

### 4.3 Dead Table

Dead Table覆盖全部 L1 sets，只用于准确率统计，使用寄存器阵列，不考虑面积：

```scala
class L1DBPDeadEntry extends Bundle {
  val valid = Bool()
  val origin = UInt(2.W) // Demand、Stream、Stride
  val predictedDead = Bool()
}

val deadTable = RegInit(VecInit(Seq.fill(nSets)(
  VecInit(Seq.fill(nWays)(0.U.asTypeOf(new L1DBPDeadEntry)))
)))
```

`valid` 表示该 residency属于Demand load、Stream或Stride预测范围。`origin` 用于让非sampled set也能参与按来源划分的全局准确率统计；它不保存PC Predictor index。其他来源写 `valid := false`。

Dead Table可以同周期任意读写，不产生反压。旧dead信息在s0/s1读取并沿流水保存，新dead信息在s3写回，因此同一 set/way 被替换时不会丢失旧预测。

residency结束时必须处理Dead Entry：refill替换旧line时直接用新prediction覆盖；Probe toN或显式replace只结束旧line而没有新refill时，将该way的 `valid` 清零。即使后续逻辑还会用coherence-valid作为终止门控，也不能依赖陈旧Dead Entry避免重复统计。

## 5. 原始PC和pf_source通路

当前 `MissReqWoStoreData` 已经携带 PC，但 `MainPipeReq` 没有 PC；当前 MSHR还会在预取被demand合并时将 `req.pf_source` 改为 `CLEAR`。因此不能直接使用现有字段。

在每个 MissQueue entry 中增加不可变寄存器：

```text
dbp_origin_pc
dbp_origin_pf_source
dbp_origin_valid
dbp_origin_is_prefetch
```

MSHR allocation时：

```text
Demand load:
    origin_pc := primary request PC
    origin_pf_source := NULL
    origin_valid := true
    origin_is_prefetch := false

Stream/Stride prefetch:
    origin_pc := DontCare
    origin_pf_source := original STREAM/STRIDE
    origin_valid := true
    origin_is_prefetch := true

其他请求:
    origin_valid := false
```

这些字段在整个MSHR生命周期内不再修改。特别是：

- demand合并到prefetch MSHR时，保留原始预取来源。
- 多个demand load合并时，保留primary miss PC。
- 现有 `req.pf_source := CLEAR` 可继续服务原来的预取统计，不影响DBP来源。

扩展 `MainPipeReq`：

```text
dbp_origin_pc
dbp_origin_pf_source
dbp_origin_valid
dbp_origin_is_prefetch
```

MissQueue生成 `io.main_pipe_req` 时赋值，使MainPipe s0可以获得完整PC和未清除的pf_source。Probe、Store、AMO等非refill请求将这些字段置为无效。

## 6. MainPipe流水时序

### 6.0 模块边界

`L1DBP` 在 `DCacheWrapper` 中按 `l1DBPParams` 条件实例化，内部拥有Counter Predictor、Sample Array、Dead Table和Monitor。MainPipe不直接持有这些表，只提供以下时序接口：

```text
dbp.read       : s0_fire时请求读取当前set的Sample/Dead row
dbp.query      : refill在s0查询新block预测
dbp.sampleResp : s1返回全部ways的Sample Entry
dbp.deadResp   : s1返回全部ways的Dead Entry
dbp.refill     : s3 refill commit时安装新Sample/Dead Entry
dbp.terminate  : s3旧residency真正结束时训练并统计
mainPipe.dbpAccess : 各LoadPipe已经确认的demand access事件；MainPipe本地事件直接内部使用
```

采用这一边界的原因是MainPipe掌握victim、tag match、coherence转移和commit时刻，而Wrapper已经汇集所有LoadPipe的 `access_stat` 事件。这样DBP既不侵入LoadPipe，也能看到MainPipe读取旧access之后发生的并发demand hit。

### 6.1 s0：读取旧信息并预测新block

对 refill 请求：

```text
Demand load:
    pcIndex = pcHash(dbp_origin_pc)
    predictedDead = pcCounters(pcIndex) == 0

Stream/Stride:
    pfIndex = originalPfSource映射到0/1
    predictedDead = pfCounters(pfIndex) == 0

其他来源:
    predictionValid = false
    predictedDead = false
```

对 refill、probe和其他可能结束residency的Tag lookup：

- Tag SRAM按原逻辑读取。
- sampled set同时读取Sample SRAM。
- Dead Table同步读取完整set行。
- AccessArray和PrefetchSourceArray继续通过现有extra-meta读口提供旧meta。

DBP的逻辑读请求以 `s0_fire` 为准；Sample SRAM的物理read valid为 `s0_fire && isSampleSet(s0_idx)`。不能直接使用 `tag_read.fire`，因为MainPipe的显式 `replace` 请求不读Tag SRAM，但仍需读取指定way的Sample Entry。refill和probe请求则继续与原Tag lookup同步。

Sample SRAM是single-port SRAM。它的读请求只在MainPipe `s0_fire` 时发出，写请求只在 `io.tag_write.valid && s3_need_replacement` 时发出。Tag SRAM本身也是single-port，`tag_write_intend` 会令MainPipe的tag read不ready，因此同周期Sample读写冲突已经被现有Tag握手覆盖，不增加新的ready或阻塞条件。实现中仍需断言 `!(sampleRead && sampleWrite)` 验证这一假设。

### 6.2 s1：确定旧line

refill miss使用最终 `s1_repl_way_en`，probe使用tag-match way，显式replace使用 `replace_way_en`，分别选择：

```text
oldSample
oldDead
oldAccess
oldPfSource
```

同时寄存新block的：

```text
newPredictionValid
newPredictedDead
newPayload = pcIndex或原始pfSource编码
```

还需寄存 `oldCohValid`、选中的set/way以及终止类型所需的信息。不能仅依靠 `oldSample.valid` 判断旧line存在，因为Sample SRAM不复位且unsupported refill会写invalid Sample；旧residency是否存在必须由选中way的coherence metadata判断。

### 6.3 s1之后的access forwarding

`oldAccess` 是s1时刻的快照，不一定是最终标签。一个LoadPipe请求可能已经在MainPipe读取后进入流水，并在旧line被s3淘汰的同周期完成demand hit。若直接使用 `!oldAccess`，这类line会被误判为dead。

Wrapper将现有 `ldu.map(_.io.access_stat)` 同时旁路到MainPipe的DBP forwarding输入；`mainPipe.io.access_stat.access` 对应的本地事件直接在MainPipe内部参与OR。对从s1保留到s3的旧line，逐拍执行：

```text
oldAccessFinal = oldAccessSnapshot |
                 any(demandAccess.valid &&
                     demandAccess.set == oldSet &&
                     (demandAccess.way_en & oldWayEn).orR)
```

该forwarding只组合/寄存已有event，不读取或写入新的SRAM端口，也不改变流水ready。MSHR merge对应的是新line的 `s3_req.access`，不能错误地OR到旧victim的 `oldAccessFinal`。

### 6.4 s2/s3：训练旧block并提交新block

旧residency终止事件包括：

```text
refill替换一个coherence-valid victim
Probe导致coherence变为Nothing（toN）
MainPipe显式replace导致coherence-valid line离开L1
```

若后续接入新的flush/invalidate/CMO本地失效路径，只要它使line的coherence变为Nothing，也必须复用同一个termination接口；不能因为请求名称叫flush就无条件训练。当前代码中的 `CMOReq`/CBO事务不等同于已经经过MainPipe清除某一条L1 line，需按实际coherence清除路径判断。

以下事件不训练：

```text
选择invalid victim
Probe toB，line仍留在L1
非sampled set
oldSample.valid == false
refill tag已存在而没有替换另一段residency
```

其中“非sampled set”和“`oldSample.valid == false`”仅禁止Counter训练，不禁止全局准确率统计。只要 `oldDead.valid` 且residency真实结束，所有set都应产生Monitor result。

训练时根据旧meta解释 `oldSample.payload`：

- `pf_source == NULL`：更新PC Predictor。
- `pf_source == CLEAR/STREAM/STRIDE`：使用payload中保存的原始来源更新Prefetch Predictor。

实际标签为：

```text
actualDead = !oldAccessFinal
```

在s3 refill commit时：

```text
newResidencyCommit = io.tag_write.valid && s3_need_replacement

newResidencyCommit:
    写新Tag
    sampled set时写新Sample Entry
    写Dead Table(set)(way) = {newPredictionValid, newOrigin, newPredictedDead}
```

`s3_need_replacement` 在这里既覆盖选择invalid way，也覆盖替换valid victim；它表示本次refill确实开始了一段新residency。若refill命中已经存在的valid tag，则不训练旧line，也不覆盖旧Sample/Dead，因为这只是同一residency内的权限变化或竞态处理。

预测结果可以与replacement update沿同一条MainPipe流水到s3，但Dead/Sample的提交条件必须使用 `newResidencyCommit`，termination必须使用 `s3_fire` 和真实的coherence终止条件。不能仅使用 `replace_access.valid`：当前该信号由进入s3的脉冲产生，在s3因writeback或写口stall时可能早于真正commit。预测查询可以与替换算法更新并行，预测状态的架构提交则必须与Tag提交原子对齐。

Probe toN和显式replace没有新Tag写入：在其 `s3_fire` 时先用旧Sample/Dead产生training/result，再清除旧Dead Entry；Sample Entry可保留陈旧值，因为未来只有coherence-valid victim才允许训练，且下一次sampled refill会覆盖它。

## 7. AccessArray语义

本设计接受MSHR merge算作一次访问，因此保留当前MissQueue行为：secondary non-prefetch merge将MSHR的 `access` 置1，refill时用该值初始化AccessArray。

AccessArray更新规则为：

```text
refill且无demand merge -> false
refill且发生demand merge -> true
refill命中已有valid tag -> 保留旧access，并OR本次MSHR的access
refill后demand load/store/AMO hit -> true
prefetch hit -> 不置位
```

LoadPipe已经排除prefetch访问。MainPipe需要做两处修正：非miss写条件排除prefetch hit；新residency refill使用MSHR携带的 `s3_req.access` 初始化，而tag-hit refill写 `oldAccessFinal || s3_req.access`，不能把同一residency此前已经置1的access清零。

当前AccessArray确实会生成硬件：`DCacheWrapper` 无条件实例化 `L1FlagMetaArray`，而该模块由 `RegInit(Vec(nSets)(Vec(nWays)))` 实现，不是被综合删除的纯统计对象。它现有用途包括prefetcher monitor对“是否访问过”的判断；本设计直接复用该bit作为最终训练标签的基础状态。`L1AccessStatArray` 是另一套按访问次数做性能统计的寄存器阵列，两者不能混为同一个模块。

## 8. Monitor与性能计数

新增 `L1DBPMonitor`，结构和 `L1AccessStatArray` 类似，但不修改现有 `L1AccessStatArray`。

Monitor输入事件：

```scala
class L1DBPResult extends Bundle {
  val sampled = Bool()
  val origin = UInt(2.W) // Demand、Stream、Stride
  val predictorIndex = UInt(pcIndexWidth.W)
  val predictedDead = Bool()
  val actualDead = Bool()
  val fromProbe = Bool()
}

val result = ValidIO(new L1DBPResult)
```

当任意受支持的residency结束时产生一个result事件。全局统计覆盖全部sets：

```text
prediction_total
prediction_correct
pred_dead_actual_dead
pred_dead_actual_reused
pred_reused_actual_dead
pred_reused_actual_reused
```

按Demand、Stream、Stride分别生成同类统计。

每个 Predictor entry 的 `predictionCount` 和 `correctCount` 仅统计 sampled sets，因为非sampled line退出时没有保存PC hash。性能计数代码只用于仿真和分析，不以综合面积为设计约束。

Monitor还应统计：

```text
sample_alloc
sample_train
sample_invalid_termination
non_sample_termination
unsupported_refill
probe_to_n_train
replacement_train
late_access_forwarded
```

## 9. 新增和修改文件

下面的清单是实现边界，不只是推荐的代码组织。DBP表项只放在新增模块中；现有DCache模块负责提供来源、流水时序和访问标签。这样关闭 `l1DBPParams` 后，不会实例化Counter、Sample SRAM、Dead Table和Monitor。

| 文件 | 模块/对象 | 必须完成的工作 |
| --- | --- | --- |
| `dcache/L1DBP.scala` | `L1DBPParams`、`L1DBP`、`L1DBPMonitor`及相关Bundle | 参数检查、采样映射、Counter/Dead Table、预测、训练、统计 |
| `dcache/meta/L1DBPSampleArray.scala` | `L1DBPSampleArray` | sampled-set signature的单口SRAM读写 |
| `dcache/DCacheWrapper.scala` | `DCacheParameters`、`DCacheImp` | 可选实例化、MainPipe连接、LoadPipe late-access扇出 |
| `dcache/mainpipe/MissQueue.scala` | `MissEntry` | 分配时锁存不可变PC/原始pf_source，refill时送MainPipe |
| `dcache/mainpipe/MainPipe.scala` | `MainPipeReq`、`MainPipe` | s0读/预测，s1选way，s1-s3保留旧状态，s3终止/安装 |
| `dcache/mainpipe/Probe.scala` | `ProbeQueue`请求构造 | 明确DBP origin无效；Probe仍直接进入MainPipe |
| `dcache/mainpipe/AtomicsReplayUnit.scala` | AMO请求构造 | 明确DBP origin无效 |
| `top/Configs.scala` | `WithL1DBP`及实验Config | 将 `l1DBPParams` 设为 `Some(...)` |
| `dcache/L1DBPTest.scala` | `L1DBPTest` | Counter、bypass、Sample SRAM和采样映射的模块级测试 |

### 9.1 新增文件

`src/main/scala/xiangshan/cache/dcache/L1DBP.scala`

- `L1DBPParams`
- sampled-set helper
- `L1DBPSampleEntry`、`L1DBPDeadEntry`、query/refill/terminate/result bundles
- `L1DBP`：PC/PF counter寄存器阵列、Dead Table、query、training和同entry bypass
- `L1DBPMonitor`

`src/main/scala/xiangshan/cache/dcache/meta/L1DBPSampleArray.scala`

- 紧凑single-port Sample SRAM
- sampled set到sampleIndex的映射
- 每次读出全部ways，按way mask写单个way
- MBIST和SRAM control参数跟随Tag SRAM配置

`src/test/scala/xiangshan/cache/dcache/L1DBPTest.scala`

- Predictor counter、采样映射、query/train bypass和Monitor的模块级测试

### 9.2 修改文件

`src/main/scala/xiangshan/cache/dcache/DCacheWrapper.scala`

- `DCacheParameters`增加可选 `l1DBPParams`
- 条件实例化 `L1DBP`；`None` 时不生成相关表项
- 连接MainPipe的read/query/refill/terminate及Sample/Dead响应
- 将现有各LoadPipe的demand `access_stat` 事件同时扇出到MainPipe的DBP forwarding输入；MainPipe本地event内部复用
- 如Monitor需要清理统计窗口，连接现有 `perfClean`

`src/main/scala/xiangshan/cache/dcache/mainpipe/MissQueue.scala`

- 每个MSHR entry保存不可变origin PC和origin pf_source
- allocation时识别Demand/Stream/Stride
- merge时保留origin，同时继续维护现有access语义
- 将DBP origin字段送入 `main_pipe_req`
- 注意merge store当前可能整体覆盖 `req`，所以origin必须是独立寄存器，不能继续放在可变 `req` 内

`src/main/scala/xiangshan/cache/dcache/mainpipe/MainPipe.scala`

- 扩展 `MainPipeReq` 和MainPipe IO
- s0发起Sample读取并查询Predictor
- s1按victim、probe tag-match或replace way选择Sample、Dead、Access和pf_source
- s1到s3累计并发demand access forwarding
- s2/s3产生terminate/refill commit事件
- 仅 `tag_write.valid && s3_need_replacement` 的新residency commit写Sample和Dead
- Probe真正转为Nothing、coherence-valid victim replacement和显式replace产生termination事件
- 排除prefetch hit对AccessArray的置位

`src/main/scala/xiangshan/cache/dcache/mainpipe/Probe.scala`

- 构造Probe的 `MainPipeReq` 时显式设置 `dbp_origin_valid := false`
- Probe仍经ProbeQueue直接进入MainPipe，不经过MSHR

`src/main/scala/xiangshan/cache/dcache/mainpipe/AtomicsReplayUnit.scala`

- 构造AMO的 `MainPipeReq` 时显式设置 `dbp_origin_valid := false`

`MainPipe.scala` 中的 `convertStoreReq`、`convertPrefetchReq` 也必须显式设置DBP origin缺省值。即使整个bundle先赋 `DontCare`，也不能让无关请求携带不确定的 `origin_valid` 进入预测逻辑。

`src/main/scala/top/Configs.scala`

- 增加 `WithL1DBP(params)`，为目标配置设置 `l1DBPParams = Some(params)`。
- `DCacheParameters`字段的默认值保持 `None`；具体用于实验或RTL验证的Config可以显式叠加 `WithL1DBP`。不要把“字段默认关闭”和“某个实验Config是否启用”混为一件事。

不需要修改Tag SRAM、replacement policy和现有 `L1AccessStatArray` 的内部实现。

### 9.3 推荐实现顺序

1. 增加参数、bundle和独立 `L1DBP`/Sample Array，先完成sample mapping和Counter单元测试。
2. 在MissEntry增加不可变origin寄存器，并把字段送到 `MainPipeReq`。
3. 接MainPipe的s0/s1/s3时序，先实现refill replacement，再补Probe toN和显式replace。
4. 在Wrapper实例化并接入所有demand access事件，验证late-access forwarding。
5. 最后在目标实验Config中叠加 `WithL1DBP`；同时保留一份不叠加该配置的对照，比较关闭前后的生成RTL接口和行为。

### 9.4 跨模块接口和所有权

实现时各状态只能有一个所有者：

```text
MissEntry
  owns: immutable origin PC / original pf_source
  emits: MainPipeReq.dbp_origin_*

MainPipe
  owns: s0-s3 transaction state and final victim/termination decision
  emits: read, query, refill, terminate
  consumes: sampleResp, deadResp, lateAccess

L1DBP
  owns: PC/PF counters, Dead Table, Sample Array, Monitor
  consumes: read, query, refill, terminate
  emits: queryResp, sampleResp, deadResp

L1FlagMetaArray
  owns: per-residency access bit
  remains the source of the base reuse label
```

不能让MissEntry直接训练Counter，也不能让 `L1DBP` 自己重新推断victim或coherence终止；这两种做法都会复制MainPipe已有的时序状态，并在stall、Probe或tag-hit refill时产生不一致。

### 9.5 必须保持原行为的模块

- Tag Array和replacement policy不增加DBP专用端口，也不改变ready/valid。
- `L1FlagMetaArray`继续保存access和pf_source；DBP只读取并修正其access更新语义。
- `L1AccessStatArray`不承担DBP状态，只作为新增Monitor的编码风格参考。
- LoadPipe不新增表访问，只复用其已经确认命中的demand `access_stat` 事件。
- WritebackQueue和ProbeAck生成路径不需要修改；termination在MainPipe确认Probe toN时完成。

## 10. 断言与验证

必须增加以下断言：

```text
sampleBits满足1 <= n < idxBits
Sample SRAM读写冲突不会发生，且冲突条件被Tag SRAM阻塞覆盖
非sampled set不能访问Sample SRAM
sample valid的coherence-valid line必定由一次refill写入
unsupported refill不能写valid Sample/Dead entry
Probe toB不能训练或清除Sample
一次residency最多训练一次
Dead/Sample新项只在tag_write.valid且s3_need_replacement时写入，并与Tag同set、同way
Probe/replace终止后Dead valid被清除
同周期query/train同entry时使用训练后counter
原始prefetch来源只能为Stream或Stride
origin_valid为false的MainPipe请求不能查询或安装有效预测
oldAccessFinal包含s1读取后、终止提交前命中同set/way的所有demand access
```

验证场景：

1. `sampleBits=1/2/3`及重叠区间的采样数量和sampleIndex唯一性。
2. 自定义PC hash lambda及不同Predictor大小。
3. Demand block有/无merge、有/无后续hit的预测和训练。
4. Stream/Stride block在meta为原source及CLEAR时均训练正确的source counter。
5. PC hash等于0/1时仍能依靠meta正确区分Demand和Prefetch payload。
6. replacement、Probe toN、Probe toB、显式replace、invalid victim和refill tag已存在。
7. 同周期Counter query/train命中相同及不同entry。
8. s3 stall时Dead/Sample不会早于Tag提交。
9. refill命中已有valid tag时不重装Sample/Dead，并保留旧access状态。
10. 全局准确率覆盖全部sets，每entry准确率只覆盖sampled sets。
11. LoadPipe hit发生在MainPipe s1读access之后、s3终止之前时，最终标签仍为reused。
12. Stream/Stride以外的Berti/Store prefetch写invalid Sample/Dead且不训练。
13. DBP关闭时不生成Counter、Sample SRAM、Dead Table和Monitor，DCache原有可见行为不变。
14. 分别用 `l1DBPParams=None` 和 `Some(...)` 做DCache/Top elaboration；启用时检查生成RTL中存在Sample SRAM宏实例，关闭时检查其不存在。

## 11. 讨论中保留的争议与结论

### 11.1 MainPipe是否已有PC和原始pf_source

没有。PC当前保存在MissQueue，`MainPipeReq`没有PC；现有pf_source会被merge/hit清为CLEAR。结论是新增不可变origin字段，并保证MainPipe s0可见。

### 11.2 Counter Predictor使用SRAM还是寄存器阵列

最初考虑SRAM，但连续流水会产生新block query和旧block training的端口讨论。最终决定使用寄存器阵列，初值直接 `RegInit(2)`，允许多读单写，不需要初始化状态机。

### 11.3 MSHR merge是否算二次访问

存在“严格refill后访问”和“miss生命周期内额外需求”两种定义。最终选择后者：secondary demand merge算访问，保留MSHR `access := true` 的现有行为。

### 11.4 如何保留预取来源

当前pf_source会变成CLEAR，但CLEAR仍能说明该line原本是prefetch。Sample payload保存未清除的原始Stream/Stride来源；训练时用当前meta判断payload类型，再用payload选择具体source counter。

### 11.5 是否只处理replacement

不能。Probe toN和显式replace同样结束L1 residency，必须训练；Probe toB不结束residency，因此不训练。对flush/invalidate/CMO不能只按请求名称判断，只有实际导致某条L1 line coherence变为Nothing的本地路径才产生termination。

### 11.6 PC/Dead表冲突是否需要新增阻塞

Sample SRAM只在MainPipe随Tag读写，其单口冲突由Tag SRAM现有阻塞覆盖，不增加反压。Dead Table最终改为寄存器阵列，允许同时读写，也不增加反压。

### 11.7 sampled-set比较区间能否重叠

可以。`set[7:1] == set[6:0]`不是非法表达式，并且产生1/128采样率。仅禁止 `sampleBits == idxBits` 的自比较退化情况。

### 11.8 是否复用L1AccessStatArray

不直接修改或复用其接口。新增 `L1DBPMonitor`，实现风格和性能计数方式仿照 `L1AccessStatArray`。

### 11.9 性能计数器面积

每entry及全局准确率计数仅用于仿真分析，本设计不以其综合面积为约束。

### 11.10 Counter初始化

Counter Predictor使用寄存器阵列，所有PC、Stream和Stride counter直接初始化为2，不生成逐项初始化状态机。

### 11.11 Sample Entry是否需要valid

需要。coherence/client metadata只能说明cache line当前是否存在，不能说明该line是否属于Demand、Stream或Stride预测范围，也不能防止unsupported refill使用旧payload。每个Sample Entry保留独立 `valid`；Counter训练同时要求coherence-valid victim、sampled set和 `sample.valid`。

### 11.12 ProbeAck是否经过MSHR

不经过。TileLink B通道Probe先进入 `ProbeQueue`，再以 `MainPipeReq(probe = true)` 直接走MainPipe；MainPipe确定coherence变化和是否需要数据后送WritebackQueue生成ProbeAck。DBP应在MainPipe看到Probe的旧meta，并仅在 `probe_new_coh == Nothing` 且请求真正 `s3_fire` 时终止residency。

### 11.13 L1预取来源是否真的只有Stream和Stride

当前源码的 `L1PrefetchInterface` 还定义了 `L1_HW_PREFETCH_BERTI` 和 `L1_HW_PREFETCH_STORE`，并且相应模块可能产生这些值。这里的结论不是删除这些来源，而是本实验第一版的Prefetch Predictor只支持Stream和Stride；Berti、Store及其他来源按unsupported refill处理，写 `Sample.valid := false`、`Dead.valid := false`，不查询也不训练两个PF counter。若实验配置明确关闭Berti/Store，这些分支不会命中，但实现仍需防御。

### 11.14 预测能否与replacement update同步

可以处于同一MainPipe阶段，但不能把“同阶段”误写成“同一个valid”。新block预测在s0产生并沿流水携带；Counter训练和Dead/Sample提交在s3完成。`replace_access.valid`可能在s3 stall期间早于Tag commit，所以DBP架构状态必须分别以 `s3_fire` 和 `tag_write.valid && s3_need_replacement` 为准。

### 11.15 Sample SRAM能否完全复用Tag SRAM冲突

可以，前提是Sample read严格由MainPipe `s0_fire` 驱动，Sample write严格由refill的 `tag_write.valid && s3_need_replacement` 驱动，不允许出现额外后台访问。Tag SRAM的single-port写意图已经阻塞同周期MainPipe读取；Sample SRAM只需复用该调度并加冲突断言，不需要把ready接回MainPipe形成第二套阻塞。

### 11.16 s1读出的access是否就是最终标签

不一定。更早进入LoadPipe的请求可能在MainPipe s1之后、旧line s3终止之前完成命中。最终方案对已有demand access事件做set/way forwarding，并与s1快照OR，得到 `oldAccessFinal`；这不改变“Sample SRAM与Tag同步且不新增阻塞”的结论。
