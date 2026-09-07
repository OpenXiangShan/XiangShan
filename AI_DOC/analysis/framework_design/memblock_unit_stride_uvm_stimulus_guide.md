# MemBlock BT：普通 Unit-Stride VLE/VSE 端口赋值参考

本文只说明向 MemBlock BT 顶层端口施加怎样的信号值，覆盖普通、非 fault-only-first、非 mask、非 segment、非 indexed/strided 的 unit-stride 向量加载和存储。

范围内指令为：

| 访问 | 指令形式 | MemBlock 接收端口 | LSQ 分配 | 结果 |
| --- | --- | --- | --- | --- |
| VLE | `vle{8,16,32,64}.v vd, (rs1)` | `ooo_to_mem.issueVldu` | LQ | 写向量目的寄存器 |
| VSE | `vse{8,16,32,64}.v vs3, (rs1)` | `ooo_to_mem.issueVldu` | SQ | 从 `vs3` 取写数据 |

不在本文范围内的情况：`vlseg*、vlsseg*、vsuxei*、vloxei*、vleff*、AMO、非对齐异常、页表/TLB 异常、向量 replay、redirect/flush 与 CSR 状态切换。本文只定义这些普通访问的端口赋值。

## 1. 总体驱动顺序

一次普通访问建议按以下时序推进：

1. 在 `enqLsq` 为该 uop 申请 LQ 或 SQ，等待 `canAccept=1` 且相应 `iqAccept=1`。
2. 等待申请响应 `resp(i)`，将得到的 `lqIdx` 或 `sqIdx` 回填到后续 `issueVldu.bits.uop`。
3. 在 `issueVldu.valid=1` 时保持全部 `bits` 稳定，直到 `issueVldu.ready=1` 的同拍完成握手。
4. VLE 完成后保持提交相关输入为正常非提交状态。VSE 到达提交点时仅驱动向量 store 对应的 `pendingst/pendingVst/commit`，见第 8 节。

每个空闲周期的确定性基线：

```scala
ooo_to_mem.issueVldu.valid := false.B
ooo_to_mem.issueVldu.bits  := 0.U.asTypeOf(new MemExuInput(true))

ooo_to_mem.enqLsq.needAlloc := VecInit(Seq.fill(LoadPipelineWidth)(0.U(2.W)))
ooo_to_mem.enqLsq.req.foreach { r =>
  r.valid := false.B
  r.bits  := 0.U.asTypeOf(new DynInst)
}
```

不要在 `valid=0` 时依赖未初始化的 `bits`。虽然 Decoupled/Valid 协议不会采样它们，置零可以避免 X 扩散和波形歧义。

## 2. 指令身份与访问属性

以下值写入 `issueVldu.bits.uop`。`fuType`、`fuOpType`、`vlsInstr` 决定 VLE/VSE 在 MemBlock 内部被送到 vector load/store 路径。

| 字段 | 普通 VLE | 普通 VSE | 说明 |
| --- | --- | --- | --- |
| `fuType` | `FuType.vldu` | `FuType.vstu` | VLE/VSE 共用 `issueVldu`，但必须分别标识为 vector load/store 功能单元。 |
| `fuOpType` | `VlduType.vle = 9'b01_00_00000` | `VstuType.vse = 9'b10_00_00000` | 单元步长、非 whole、非 mask、非 FOF 的完整固定编码。 |
| `fuOpType(6,5)` | `00` | `00` | MOP。`00` 就是 unit stride；不是独立 Bundle 字段。 |
| `fuOpType(4,0)` | `00000` | `00000` | LUMOP/SUMOP。排除 whole、mask 与 FOF 的普通形式。 |
| `vlsInstr` | `1` | `1` | 单一 Bool，标识 vector load/store 指令。它没有 `isUnitStride/isStrided` 等子字段。 |
| `vecWen` | `1` | `0` | VLE 写向量目的寄存器；VSE 不写。 |
| `v0Wen` / `vlWen` | `0` / `0` | `0` / `0` | 普通访存不写 v0 或 vl。 |
| `rfWen` / `fpWen` | `0` / `0` | `0` / `0` | 非标量/浮点寄存器写回。 |
| `commitType` | `CommitType.LOAD = 3'b010` | `CommitType.STORE = 3'b011` | 普通 cacheable 访存的精确编码。 |
| `exceptionVec` / `hasException` | `0` / `0` | `0` / `0` | 正常访问不预置异常。 |

元素宽度由 `uop.vpu.veew` 指定：

| 指令 | `veew` | 每元素字节数 |
| --- | --- | --- |
| `vle8.v` / `vse8.v` | `00` | 1 |
| `vle16.v` / `vse16.v` | `01` | 2 |
| `vle32.v` / `vse32.v` | `10` | 4 |
| `vle64.v` / `vse64.v` | `11` | 8 |

## 3. VTYPE、VL 与所有合法 EEW/SEW/LMUL 组合

EEW 不要求等于 SEW。普通 unit-stride VLE/VSE 可使用不同的 EEW 与 SEW；约束是：

```text
EMUL  = LMUL * EEW / SEW
1/8 <= EMUL <= 8
VLMAX = VLEN * LMUL / SEW
VL    = 0 .. VLMAX
```

其中 `VLEN` 是本配置的向量寄存器位宽。`VL` 应同时写入 `src(4)` 的 VConfig 和 `uop.vpu.vl`，两者取同一个数；MemBlock 的 VSplit 从 `src(4).VConfig.vl` 读取实际 VL。

编码：SEW `e8/e16/e32/e64 = 00/01/10/11`；LMUL `mf8/mf4/mf2/m1/m2/m4/m8 = 101/110/111/000/001/010/011`。`100` 保留，不能使用。

下表给出每一个合法 SEW/EEW 组合允许的 LMUL 集合。表中任取一个 LMUL 都是合法组合；空白不存在。

| SEW \ EEW | 8 | 16 | 32 | 64 |
| --- | --- | --- | --- | --- |
| 8 | mf8, mf4, mf2, m1, m2, m4, m8 | mf8, mf4, mf2, m1, m2, m4 | mf8, mf4, mf2, m1, m2 | mf8, mf4, mf2, m1 |
| 16 | mf4, mf2, m1, m2, m4, m8 | mf8, mf4, mf2, m1, m2, m4, m8 | mf8, mf4, mf2, m1, m2, m4 | mf8, mf4, mf2, m1, m2 |
| 32 | mf2, m1, m2, m4, m8 | mf4, mf2, m1, m2, m4, m8 | mf8, mf4, mf2, m1, m2, m4, m8 | mf8, mf4, mf2, m1, m2, m4 |
| 64 | m1, m2, m4, m8 | mf2, m1, m2, m4, m8 | mf4, mf2, m1, m2, m4, m8 | mf8, mf4, mf2, m1, m2, m4, m8 |

一次普通非 segment 访问的向量子 uop 数为：

```text
numUops = max(1, EMUL)
vuopIdx = 0, 1, ..., numUops - 1
```

因此 EMUL 小于或等于 1 时仅有一个 uop；EMUL 为 2、4、8 时必须按 `vuopIdx` 依次送入 2、4、8 个 uop，且各 uop 的 `robIdx`、LSQ index、`src(0..4)`、VTYPE/VL 和指令属性保持同一条向量指令的语义。

## 4. `ooo_to_mem.issueVldu`

该端口是 `Decoupled[MemExuInput(isVector=true)]`。只有 `valid && ready` 同时为 1 时，MemBlock 接收一个向量访存 uop。

| 信号 | VLE | VSE | 可赋值范围与要求 |
| --- | --- | --- | --- |
| `issueVldu.valid` | `1` | `1` | 有待发 uop 时为 1；握手前不得撤销或修改 bits。 |
| `issueVldu.ready` | 输入 | 输入 | 由 MemBlock 给出；为 0 时保持 valid/bits。 |
| `bits.src(0)` | 基地址 rs1 | 基地址 rs1 | 任意正常可访问物理/虚拟地址；按 EEW 字节数对齐，建议选 cacheable 地址。 |
| `bits.src(1)` | `0` | `0` | unit stride 不使用 stride。即使 RTL 当前会带入该源，也固定为 0。 |
| `bits.src(2)` | 旧 vd 数据 | vs3 写数据 | 全 VLEN 位均可取 0/1。VLE 在部分写回时作为旧目的值；VSE 的每一个元素数据均从此向量取。 |
| `bits.src(3)` | 掩码向量 | 掩码向量 | `vm=1` 时设为全 1；本文固定非 mask，不能用它屏蔽元素。 |
| `bits.src(4)` | VConfig 原始值 | VConfig 原始值 | 必须编码 VL 和 VTYPE，见下一表。 |
| `bits.iqIdx` | 任意有效 IQ 编号 | 任意有效 IQ 编号 | 与使用的 issue queue 编号一致；单独 BT 可固定为 0。 |
| `bits.isFirstIssue` | `1` | `1` | 首次发射；本文不覆盖 replay 重发。 |
| `bits.flowNum` | `2` | `2` | 普通 unit stride 的 LSQ 分配元素数。不是 DCache 的逐 flow 编号。 |
| `bits.isVecPartReplay` | `0` | `0` | 非部分 replay。 |
| `bits.vecReplayMask` | 全 0 | 全 0 | 非 replay。 |
| `bits.vecReplayMbIdx` | `0` | `0` | 非 replay。 |

`src(4)` 的低位打包如下，其余高位全部置 0：

| 位段 | 内容 | 允许值 |
| --- | --- | --- |
| `[VL_W-1:0]` | `vl` | `0 .. VLMAX` |
| `[VL_W+2:VL_W]` | `vlmul` | `101,110,111,000,001,010,011`，且须在第 3 节矩阵内合法 |
| `[VL_W+4:VL_W+3]` | `vsew` | `00,01,10,11` |
| `[VL_W+5]` | `vta` | 本文固定 `0` |
| `[VL_W+6]` | `vma` | 本文固定 `0` |
| `[VL_W+7]` | `illegal/vill` | 本文固定 `0` |
| 更高位 | 保留 | `0` |

对应 Chisel 赋值可写为：

```scala
val cfg = Wire(new VConfig)
cfg.vl := vl
cfg.vtype.vlmul := vlmul
cfg.vtype.vsew := vsew
cfg.vtype.vta := false.B
cfg.vtype.vma := false.B
cfg.vtype.illegal := false.B
issue.bits.src(4) := cfg.asUInt
issue.bits.uop.vpu.vl := vl
issue.bits.uop.vpu.vlmul := vlmul
issue.bits.uop.vpu.vsew := vsew
```

## 5. `issueVldu.bits.uop`：每个字段的普通赋值

### 5.1 ROB、LSQ、目的寄存器与 uop 顺序

| 字段 | VLE | VSE | 允许值与要求 |
| --- | --- | --- | --- |
| `robIdx` | 当前有效 ROB index | 当前有效 ROB index | 必须唯一标识这条指令；所有拆分 uop 相同。 |
| `lqIdx` | `enqLsq.resp(i).lqIdx` | `0` | VLE 只能使用本指令申请返回的 LQ index。 |
| `sqIdx` | `0` | `enqLsq.resp(i).sqIdx` | VSE 只能使用本指令申请返回的 SQ index。 |
| `numLsElem` | `2` | `2` | 普通 unit stride 的固定 LSQ 元素需求。 |
| `uopIdx` | `0..numUops-1` | `0..numUops-1` | 按递增顺序发送。 |
| `firstUop` | 仅 `uopIdx=0` 为 1 | 同左 | 其他为 0。 |
| `lastUop` | 仅最后一个为 1 | 同左 | 其他为 0。 |
| `numUops` | `max(1, EMUL)` | `max(1, EMUL)` | 与第 3 节一致。 |
| `lsrc(0..4)` | 全 0 | 全 0 | 仅逻辑寄存器元数据；MemBlock 使用 `src(0..4)` 中的实际数据。 |
| `ldest` / `pdest` | 实际 vd 编号 / 已分配物理向量目的 | `0` / `0` | VLE 可写入实际目的；只验证访存行为时可置 0。VSE 无目的。 |
| `psrc(0)` | rs1 的物理编号或 `0` | 同左 | 仅为元数据；地址数据以 `src(0)` 为准。 |
| `psrc(1..2)` | `0` | `0` | unit stride 不需要这些标量源编号。 |
| `srcType` | 按既有 decode 填写或全 0 | 同左 | MemBlock 的正常 unit-stride 地址计算不以它决定源数据。 |

### 5.2 向量控制 `uop.vpu`

下表给出普通、非 mask、非 segment 访问的所有 `VPUCtrlSignals` 字段。未特别说明的字段一律为 0。

| 字段 | VLE | VSE | 值 |
| --- | --- | --- | --- |
| `vill` | 0 | 0 | 合法 VTYPE。 |
| `vma`, `vta` | 0, 0 | 0, 0 | 确定性非 agnostic 模式。 |
| `vsew` | 指令 SEW | 指令 SEW | 00/01/10/11。 |
| `vlmul` | 合法 LMUL | 合法 LMUL | 见第 3 节矩阵。 |
| `specVill/specVma/specVta/specVsew/specVlmul` | 全 0 | 全 0 | 本文不使用 speculative VTYPE。 |
| `vm` | 1 | 1 | 固定非 mask。 |
| `vstart` | 0 | 0 | 从元素 0 开始。 |
| `vl` | `src(4).vl` | `src(4).vl` | 必须完全相等。 |
| `veew` | 指令 EEW | 指令 EEW | 00/01/10/11。 |
| `nf` | 0 | 0 | 非 segment；硬件内部按 `nf+1` 解释字段数。 |
| `vuopIdx` | `uopIdx` | `uopIdx` | 和通用 uop 序号一致。 |
| `lastUop` | `uop.lastUop` | `uop.lastUop` | 和通用字段一致。 |
| `vmask` | 0 | 0 | 不使用单独 vmask 控制。 |
| `frm`, `fpu`, `vxrm` | 0 | 0 | 访存不需要浮点/定点舍入控制。 |
| `isReverse/isExt/isNarrow/isDstMask/isOpMask/isMove` | 全 0 | 全 0 | 非向量算术变形/搬运。 |
| `isDependOldVd` | 0 | 0 | 普通访问不显式声明依赖旧 vd。 |
| `isWritePartVd` | 0 | 0 | 由 VSplit 的实际拆分处理，不预置。 |
| `isVleff` | 0 | 0 | 非 fault-only-first。 |

### 5.3 其余 `DynInst` 字段

普通无异常、无 redirect 的端口激励中，下列字段全部置 0：

```text
instr, pc, foldpc, exceptionVec, isFetchMalAddr, hasException, trigger,
preDecodeInfo, pred_taken, crossPageIPFFix, ftqPtr, ftqOffset, satpFlushFirstFetchFault,
rfWen, fpWen, v0Wen, vlWen, isXSTrap, waitForward, blockBackward, flushPipe,
canRobCompress, selImm, imm, fpu, wfflags, isMove, isDropAmocasSta, uopSplitType, isVset,
numWB, needFrm, debug_fuType, debug_sim_trig,
useRegCache, regCacheIdx, dirtyFs, dirtyVs, traceBlockInPipe, eliminatedMove, snapshot,
debugInfo, debug_seqNum, storeSetHit, waitForRobIdx, loadWaitBit, loadWaitStrict, ssid,
singleStep, replayInst, debug, options
```

`instr`、`pc` 与 FTQ 字段只有在环境依赖其断言、追踪或性能统计时才需要填入真实值；对本文的正常访问路径可固定为 0。`srcLoadDependency`、`srcState`、`lqIdx`、`sqIdx` 等 Bundle 子字段中未在上表指定者置 0。

## 6. `ooo_to_mem.enqLsq`：LQ/SQ 分配

本节以当前 V2 `MemBlock.sv` 的顶层 `io_ooo_to_mem_enqLsq_<0..5>` 为准。该顶层只有
`needAlloc`、`req.valid` 和 `req.bits`，没有向 standalone driver 返回的 `ready`、`canAccept`、
`iqAccept` 或 `resp.lqIdx/sqIdx`。后者是完整后端中 Rename/Dispatch/`LsqEnqCtrl` 的内部信号；
独立测试框架必须自己维护与 DUT 相同的 LQ/SQ 环形分配账本，并把算出的起始指针直接写入
`req.bits.lqIdx/sqIdx` 和后续 `issueVldu`。

### 6.1 `numLsElem` 的来源和普通计算规则

`numLsElem` 不是元素个数，也不是 `PopCount(flowMask)`。它是 Dispatch 为一个 vector data-uop
保守预留的连续 LSQ entry 数；普通 non-segment、非 FOF-tail uop 必须满足：

```text
enqLsq.req.bits.numLsElem == issueVldu.flowNum
```

对普通非 whole、非 mask-register 访问，先由 `vsew`、`vlmul`、`veew` 得到 `EMUL`：

```text
s       = vsew
l       = signed(vlmul)
w       = veew
EMUL    = 2^(l + w - s)
EEW_B   = 2^veew
SEW_B   = 2^vsew
```

随后按地址模式计算：

| 地址模式 | `numLsElem` / `flowNum` | 含义 |
| --- | --- | --- |
| 普通 unit-stride `vle/vse` | `2` | Dispatch 无法提前知道 base address 是否跨 16B 边界，因此固定保守预留两个候选 flow。 |
| strided `vlse/vsse` | `MulDataSize(EMUL) / EEW_B` | 一个 flow 按访问元素宽度 `EEW` 划分。 |
| indexed `vluxei/vloxei/vsuxei/vsoxei`，`EMUL > LMUL` | `MulDataSize(EMUL) / EEW_B` | index 侧容量成为限制。 |
| indexed，`EMUL <= LMUL` | `MulDataSize(LMUL) / SEW_B` | data 侧容量成为限制。 |

`MulDataSize(mf8/mf4/mf2)` 分别为 `2/4/8 B`；`MulDataSize(m1/m2/m4/m8)` 均为
`16 B`。例如 `SEW=e32`、`EEW=e32`、`LMUL=m1` 时，普通 unit-stride 为 `2`，
strided/indexed 均为 `16B / 4B = 4`。

### 6.2 边界和禁止直接套用公式的场景

| 场景 | `numLsElem` 行为 | 测试框架规则 |
| --- | --- | --- |
| `VL=0`、`vstart>=VL` 或全部元素 mask-off | 普通 data-uop 仍采用上述保守值。 | 不得因实际无访问而改成 `0` 或回退已预留指针。 |
| 普通 FOF data-uop | 与普通 load 相同。 | 仍建立 LQ range。 |
| FOF `fix-VL` tail | `0`。 | `req.valid=0`、`needAlloc=00`，不建普通 LQ/SQ range。 |
| segment load/store | 不走普通 `enqLsq` range。 | `req.valid=0`、`needAlloc=00`；由 `VSegmentUnit` 专用路径处理。 |
| whole-register、mask-register | 使用专用 Decode/模板计算。 | 本草稿的普通公式不覆盖，初版随机测试不生成。 |
| partial replay | 保留原始 `numLsElem/flowNum`。 | 不重新 `enqLsq`，不得改为 `PopCount(vecReplayMask)`。 |

因此，`flowNum=2` 只代表 unit-stride 的资源上限。VSplit 根据 `VL`、`vstart`、`vm/v0`
和地址对齐计算实际 `activeNum`，其值可能为 `0`、`1` 或 `2`；`activeNum` 不反向修改
`numLsElem`。

### 6.3 顶层入队 pulse 和 payload

设一个普通 vector load/store data-uop 使用 slot `j`。其余 slot 采用 standalone canonical
idle：`needAlloc=00`、`req.valid=0`、payload 全 0。

| 信号 | vector load | vector store | 约束 |
| --- | --- | --- | --- |
| `needAlloc(j)` | `2'b01` | `2'b10` | `2'b11` 不是当前 V2 的合法生成值。 |
| `req.valid(j)` | `1`，仅一个时钟周期 | `1`，仅一个时钟周期 | 顶层无 ready；多拍保持会被视为重复入队。 |
| `fuType/fuOpType` | `vldu` 和对应 vector load opcode | `vstu` 和对应 vector store opcode | 必须与后续 issue payload 完全一致。 |
| `robIdx/uopIdx/lastUop` | 当前 data-uop 的身份 | 当前 data-uop 的身份 | 分别等于 `issueVldu.robIdx/vuopIdx/lastUop`。 |
| `lqIdx/sqIdx` | 写 LQ 起始指针；SQ canonical 为 0 | 写 SQ 起始指针；LQ canonical 为 0 | 起始指针由测试框架分配器产生。 |
| `numLsElem` | 上述公式结果 | 上述公式结果 | 必须等于 `issueVldu.flowNum`。 |

在完整核中该接口由 `LsqEnqCtrl` 寄存一拍后才到达 MemBlock；独立 driver 无须复现
`needAlloc` 和 payload 的残留值。一个 vector pulse 之后下一拍回到 canonical idle，再在
账本确认该 range 已可见后发对应 `issueVldu`。

### 6.4 测试框架 `lsq_ctrl` 的连续 range 管理

现有 scalar `lsq_ctrl` 的“一个 UID 对应一个 LQ/SQ entry”模型不能直接用于 vector uop。
vector 模型至少应增加 `vec_uop_ctx`，其稳定身份为：

```text
vec_uop_key = {dynamic_epoch, robIdx, vuopIdx}
```

它记录方向、`numLsElem`、LQ/SQ 起始指针、尚未释放的 entry 数、原始 issue payload 和
replay 元数据。每个物理 entry 都要映射到同一个 `vec_uop_key`：

```text
sq_owner[S + 0] = vec_uop_key
sq_owner[S + 1] = vec_uop_key       // unit-stride store, numLsElem=2
```

入队成功后的抽象伪代码如下；`advance_*()` 必须按 LQ=72、SQ=56 环形回绕并翻转 flag：

```text
allocate_vec_range(ctx):
  count = ctx.numLsElem
  require(count > 0)

  if ctx.is_load:
    require(lq_free_count >= count)
    ctx.lq_start = lq_enq_ptr
    for k in 0 .. count-1: lq_owner[advance_lq(ctx.lq_start, k)] = ctx.key
    lq_enq_ptr = advance_lq(lq_enq_ptr, count)
    lq_free_count -= count
  else:
    require(sq_free_count >= count)
    ctx.sq_start = sq_enq_ptr
    for k in 0 .. count-1: sq_owner[advance_sq(ctx.sq_start, k)] = ctx.key
    sq_enq_ptr = advance_sq(sq_enq_ptr, count)
    sq_free_count -= count

  emit_one_cycle_enqLsq(ctx)
  mark_ctx_dut_visible_at_next_sample(ctx)
```

同拍多个 slot 时，后续请求的起始指针必须累计前序有效请求的 `numLsElem`。例如 slot 0 是
`numLsElem=2` 的 vector store，slot 1 是 `numLsElem=1` 的 scalar store，则 slot 1 的
起始 SQ pointer 是 `slot0.sqIdx + 2`。初版 vector sequence 应每拍只发一个 vector slot，
避免混合 batch 的 Dispatch 限制和账本复杂度。

### 6.5 实际 flow 少于预留 range 时的完成、deq 和 cancel

预留两个 entry 而实际只执行一个 flow 是正常情况。DUT 的 merge buffer 按实际 `activeNum`
完成后，以 `{robIdx,uopIdx}` 对整段 vector range 发出完成/flush 标记；未执行的候选 flow
不会额外访存，但其预留 entry 仍随同一 vector uop 成为可提交状态。

测试框架不得在 `activeNum=1` 时把 `free_count` 加回 1，也不得只保留起始 entry。正确的
资源回收以实际观测到的 `lqDeq/sqDeq` 物理 entry 数为准：

```text
on_vector_merge_done(ctx):
  ctx.range_complete = 1             // 仅表示整段 entry 已具备 deq 条件

on_lq_or_sq_deq(entry_key):
  ctx = owner_map[entry_key]
  delete owner_map[entry_key]
  ctx.remaining_entries--
  corresponding_free_count++

  if ctx.remaining_entries == 0:
    remove ctx from active vec_uop map
```

一个 `numLsElem=2` 的 range 可以同拍 deq 两项，也可以分两拍各 deq 一项；两种情况下均在
第二个物理 entry 释放后才删除 `vec_uop_ctx`。redirect/flush 在 range 尚未完成时也按完整
预留量取消：`cancel_lq_count` 或 `cancel_sq_count += numLsElem`，不能按 `activeNum` 取消。

## 7. 其他 `ooo_to_mem` 输入

下表列出普通 VLE/VSE 激励需要固定的其余输入。未列出的复杂接口维持环境既有的正常、无阻塞状态；不要为了本访问额外构造 exception、TLB miss 或 redirect。

| 端口/字段 | 正常值 |
| --- | --- |
| `issueLda/issueSta/issueStd/issueHya` | 每个 lane 均为 `valid=0, bits=0`。本访问只从 `issueVldu` 发射。 |
| `redirect` | `valid=0, bits=0`。 |
| `isStoreException` | `0`。 |
| `isVlsException` | `0`。 |
| `sfence` | `valid=0, bits=0`。 |
| `flushSb` | `0`。 |
| `loadFastMatch`、`loadFastFuOpType`、`loadFastImm` | 每个 lane 全 0。 |
| `storePc`、`hybridPc` | 全 0。 |
| `backendToTopBypass` | 维持环境已有的正常稳定状态；不要在普通访存期间制造 bypass 事件。 |
| `csrCtrl`、TLB/PTW 相关输入 | 维持环境既有的正常翻译/权限状态；本文不定义页表异常。 |

## 8. `ooo_to_mem.lsqio`：提交侧必须区分 VLE 与 VSE

所有 `lsqio` 信号均由外部驱动到 MemBlock。正常未提交周期先使用：

```scala
lsqio.lcommit      := false.B
lsqio.scommit      := false.B
lsqio.pendingMMIOld := false.B
lsqio.pendingld    := false.B
lsqio.pendingst    := false.B
lsqio.pendingVst   := false.B
lsqio.commit       := false.B
lsqio.pendingPtr   := validRobPtr
lsqio.pendingPtrNext := nextValidRobPtr
```

VLE 没有 store 提交动作，始终保持以上非提交值。VSE 到达 ROB 提交拍时必须使用以下组合：

| 字段 | VLE | VSE 的提交拍 | 说明 |
| --- | --- | --- | --- |
| `lcommit` | 0 | 0 | 向量访存不通过该标量 load 提交口。 |
| `scommit` | 0 | **0** | 向量 store 被 RTL 显式从 `scommit` 排除。不得置 1。 |
| `pendingMMIOld` | 0 | 0 | 本文为普通非 MMIO。 |
| `pendingld` | 0 | 0 | VLE 不通过该 pending 提交路径。 |
| `pendingst` | 0 | 1 | 表示已提交的 store。 |
| `pendingVst` | 0 | 1 | 明确表示 vector store。 |
| `commit` | 0 | 1 | VSE 提交有效。 |
| `pendingPtr` | 有效稳定 ROB ptr | VSE 的 `robIdx` | 提交指针必须指向同一条 VSE。 |
| `pendingPtrNext` | 下一有效 ROB ptr | 下一有效 ROB ptr | 由环境的 ROB 指针模型给出。 |

在 VSE 提交拍之后，若没有下一条提交，`pendingst`、`pendingVst` 与 `commit` 均恢复为 0。

## 9. 可直接使用的最小单 uop 模板

选择 `SEW=32, EEW=32, LMUL=m1, VL=1` 时，`EMUL=1`，只需要一个向量 uop。下表是 VLE/VSE 所有关键端口字段的可运行基线。

| 字段 | VLE 模板 | VSE 模板 |
| --- | --- | --- |
| 指令属性 | `fuType=vldu, fuOp=VLE, unit=1, strided=0, indexed=0` | `fuType=vstu, fuOp=VSE, unit=1, strided=0, indexed=0` |
| `src(0)` | `base`，4 字节对齐 | `base`，4 字节对齐 |
| `src(1)` | `0` | `0` |
| `src(2)` | 全 0 的旧 vd | 待写数据，例如低 32 位为 `32`h11223344` |
| `src(3)` | 全 1 | 全 1 |
| `src(4)` | `vl=1, vsew=10, vlmul=000, vta=0, vma=0, vill=0` | 同左 |
| `vpu` | `vm=1, vstart=0, vl=1, veew=10, nf=0, vuopIdx=0, lastUop=1` | 同左 |
| 通用 uop 顺序 | `uopIdx=0, firstUop=1, lastUop=1, numUops=1, numLsElem=2` | 同左 |
| LSQ 申请 | `needAlloc=01`，回填 `lqIdx` | `needAlloc=10`，回填 `sqIdx` |
| 发射 | `valid=1` 等待 `ready=1` | `valid=1` 等待 `ready=1` |
| 提交输入 | 全部非提交 | `scommit=0, pendingst=1, pendingVst=1, commit=1`，指针指向该 VSE |

## 10. 常见错误

| 错误 | 正确处理 |
| --- | --- |
| 将 EEW 固定等于 SEW | 使用第 3 节的 EMUL 约束；EEW 与 SEW 可以不同。 |
| VSE 用 `lqIdx` 或 VLE 用 `sqIdx` | VLE 只分配/回填 LQ；VSE 只分配/回填 SQ。 |
| 将 `flowNum` 设为 1 | 普通 unit stride 使用 `flowNum=2` 和 `numLsElem=2`。 |
| 为 VSE 将 `scommit` 置 1 | 保持 `scommit=0`；vector store 用 `pendingst=1,pendingVst=1,commit=1`。 |
| 只更新 `uop.vpu.vl` | 同时更新 `src(4).VConfig.vl`；VSplit 从后者读取 VL。 |
| `valid=1, ready=0` 时修改 bits | 保持整个 `MemExuInput` 不变，直到握手完成。 |
| mask 未启用却给 `vm=0` | 本文固定 `vm=1` 且 `src(3)=全1`。 |

## 11. RTL 对照

| 信息 | RTL 位置 |
| --- | --- |
| `issueVldu` 和 `lsqio` 顶层定义 | `XiangShan/src/main/scala/xiangshan/mem/MemBlock.scala` |
| VLE/VSE 经 `issueVldu` 路由到 VLSU | `XiangShan/src/main/scala/xiangshan/mem/MemBlock.scala` 的 VLSU 路由逻辑 |
| `MemExuInput`、`DynInst`、`VPUCtrlSignals` 字段 | `XiangShan/src/main/scala/xiangshan/backend/Bundles.scala` |
| `src(0..4)` 的 VLSU 源含义和 `src(4).vl` | `XiangShan/src/main/scala/xiangshan/mem/vector/VSplit.scala` |
| LQ/SQ allocation bit 定义与 unit stride 的 `numLsElem` | `XiangShan/src/main/scala/xiangshan/backend/rename/Rename.scala`、`mem/lsqueue/LSQWrapper.scala` |
| 向量 store 提交时 `scommit=0`、`pendingVst=1` | `XiangShan/src/main/scala/xiangshan/backend/rob/Rob.scala` |
| `VlduType.vle` / `VstuType.vse` 编码 | `XiangShan/src/main/scala/xiangshan/package.scala` |

本文所有未明确需要变化的位均保持 0 或环境已有的正常稳定值。对普通 unit-stride VLE/VSE 来说，真正需要随访问变化的最小集合是：`base、vs3/old-vd、VL、SEW、EEW、LMUL、robIdx、LQ/SQ index、uopIdx`，以及 VSE 提交拍的 `pendingPtr/pendingPtrNext`。
