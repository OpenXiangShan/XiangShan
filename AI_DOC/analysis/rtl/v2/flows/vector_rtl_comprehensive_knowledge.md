# V2 RVV 向量接口与执行流综合知识

## 版本元数据

| 项目 | 内容 |
|---|---|
| RTL 版本 | V2 |
| 分支 | `mem_ut_uvm_v2` |
| 核验 commit | `4ce3563a01254700ed5828797f288bafcc2b491c` |
| 设计基线 | `mem_ut/ver/ut/memblock/rule/version/v2/branch_policy.md` 中的 `2acbf327cf7fb514593acc00d4c41117ec499e08` |
| 权威源码 | 当前 worktree 的 V2 Scala/Chisel 源码和 `build/rtl/MemBlock.sv` |
| 参考输入 | `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan-test/AI_DOC/analysis/rtl/v2/vec_interface.md` |
| 最后核验日期 | `2026-08-25` |

参考输入文档用于汇总接口视角和学习顺序；如果参考文档与当前 V2 源码或生成 RTL
不一致，以当前 commit 的 Scala/Chisel 和 `build/rtl/MemBlock.sv` 为准。

## 文档范围

本文只覆盖本 agent 讨论过的向量相关知识：

- RVV 向量寄存器、`vtype`、`vl`、`vstart`、`v0` mask 和其他向量状态。
- `issueVldu_0/1`、`src_0~src_4`、`uop.vpu`、`flowNum` 和向量写回接口。
- `SEW`、`EEW`、`LMUL`、`EMUL`、`NFIELDS`、uop、flow、`flowMask` 和 merge buffer。
- Rename/Dispatch/VecMem IQ/VLSplit/VSSplit/VSegmentUnit 的向量执行链。
- 普通向量访存与 LQ/SQ 的绑定、segment 路径、FOF、部分 replay 和反馈。

标量 LSQ、通用标量 DCache/TLB flow、非向量指令的完整执行路径不在本文展开；只有
它们与向量接口的边界会被说明。

## 一、先建立整体模型

一条 RVV 向量访存指令不是一个“一次完成的宽访问”，而是经过多个层次的拆分：

```text
RVV 宏指令
  -> Decode/Rename：读取 vtype、veew，计算 EMUL 和候选 flow 数
  -> NewDispatch：给普通向量 LS 预留连续 LQ/SQ entry
  -> VecMem Issue Queue：等待源操作数，保存 numLsElem
  -> issueVldu_0/1：发射一个 VLSU uop
  -> VLSplit/VSSplit：按 uop、地址模式和 mask 生成 flow
  -> LoadUnit/StoreUnit：每个 flow 做地址翻译、权限检查和 cache/LSQ 访问
  -> VLMergeBuffer/VSMergeBuffer：等待活跃 flow 完成
  -> mem_to_ooo：一次 uop 粒度的 writeback 或 replay/exception feedback
```

三个粒度必须分开：

| 粒度 | 含义 | 例子 |
|---|---|---|
| 宏指令 | ISA 看到的一条向量指令 | `vle32.v`、`vlse32.v`、`vluxei16.v` |
| uop | V2 后端为执行宏指令拆出的片段 | `vuopIdx=0/1` 的两个片段 |
| flow | VLSU 对一个 uop 进一步调度的访存工作单元 | 一个 strided 元素，或一个 16B unit-stride 访问块 |

`issueVldu` 传递的是 uop，不是单个 flow；MemBlock 内部的 LoadUnit/StoreUnit 才实际
处理 flow。

## 二、RVV 架构状态基础

### 2.1 向量数据寄存器和 `VLEN`

RVV 有 32 个架构向量寄存器 `v0` 到 `v31`。每个物理向量寄存器的位宽是实现固定的
`VLEN`。当前 V2 配置为：

```text
VLEN = 128 bit = 16 byte
VLENB = VLEN / 8 = 16 byte
```

寄存器本身没有固定的“字段名”。同一寄存器的解释由 `SEW` 和 `LMUL` 决定：

- `SEW=e32` 时，一个 128-bit 寄存器包含 4 个 32-bit 元素。
- `LMUL=m2` 时，连续两个物理寄存器组成一个逻辑寄存器组。
- `LMUL=mf2` 时，一个逻辑操作数只使用半个物理寄存器容量。

`v0` 仍然是普通向量寄存器，只是在 mask 操作数位置按每元素 1 bit 解释。V2 的
`src_3` 携带读出的 `v0` 内容；它不是一个额外的 mask 寄存器文件。

### 2.2 `vtype` 的字段

`vtype` 是决定向量寄存器解释方式的核心状态。主要字段如下：

| 字段 | 含义 | 是否直接决定元素是否访问 |
|---|---|---:|
| `vlmul` | LMUL 编码，决定逻辑寄存器组倍率 | 间接，决定容量和 uop 分片 |
| `vsew` | 当前元素宽度 SEW | 间接，决定元素大小和 VLMAX |
| `vta` | tail agnostic/undisturbed | 否，决定 tail 写回策略 |
| `vma` | mask agnostic/undisturbed | 否，决定 inactive 写回策略 |
| `vill` | 当前 vtype 是否非法 | 是，非法配置不能正常执行 |

V2 `VLmul` 编码为带符号三位指数：

| `vlmul` | 有符号指数 | `LMUL` |
|---|---:|---|
| `000` | 0 | `m1` |
| `001` | 1 | `m2` |
| `010` | 2 | `m4` |
| `011` | 3 | `m8` |
| `101` | -3 | `mf8` |
| `110` | -2 | `mf4` |
| `111` | -1 | `mf2` |
| `100` | 保留 | 不合法 |

`vlmul=3'b111` 不能按无符号数理解成 7，而应理解为 -1，即 `LMUL=2^-1=mf2`。

`vsew/veew` 的 V2 常用编码为：

| 编码 | 宽度 |
|---|---:|
| `00` | e8，8 bit |
| `01` | e16，16 bit |
| `10` | e32，32 bit |
| `11` | e64，64 bit |

### 2.3 `vl`、`VLMAX`、`vstart` 和 mask

当前配置的理论最大元素数为：

```text
VLMAX = LMUL * VLEN / SEW
```

`vl` 是本轮真正处理的元素上界，不一定等于 `VLMAX`。对元素索引 `i`，普通向量
访存的活动判定可以理解为：

```text
prestart : i < vstart
body     : vstart <= i < vl
active   : body && (vm == 1 || src_3[i] == 1)
tail     : i >= vl
```

- `vstart` 用于异常或中断恢复，已经完成的 prestart 元素不能重复访问。
- `vm=1` 表示不使用 `v0.t`，`src_3` 不参与活动判定。
- `vm=0` 表示使用 `src_3[i]` 作为 `v0.mask[i]`。
- `vta`/`vma` 只规定无效位置的目的寄存器写回策略，不负责开启或关闭 mask。

### 2.4 其他向量状态

| 状态 | 含义 | 设计目的 |
|---|---|---|
| `vl` | 当前有效元素数 | 让同一程序适配不同 VLEN，并表达循环剩余量 |
| `vstart` | 恢复起始元素 | 避免异常后重复执行已有副作用的元素 |
| `vxrm` | 定点舍入模式 | 统一 narrowing/右移等定点结果 |
| `vxsat` | 定点累计饱和标志 | 用一个状态位汇总一批元素是否发生饱和 |
| `vcsr` | `vxrm` 和 `vxsat` 的合并访问视图 | 便于保存和恢复 |
| `vlenb` | `VLEN/8` | 软件获得单寄存器字节数，用于上下文保存 |
| `mstatus.VS` | 向量上下文状态 | 只有 Dirty 任务需要保存大型向量状态 |

## 三、V2 接口中的 `VConfig` 与 `src_4`

### 3.1 为什么 `src_4` 是 128 bit

Scala 中的逻辑对象 `VConfig` 定义为：

```scala
class VConfig extends Bundle {
  val vtype = new VType
  val vl    = Vl()
}
```

但 VLDu/VSTu 的第五个源实际配置为 `VlData()`，`VlData` 当前只有 8 bit。为了让
五个向量 source 统一，`MemExuInput(isVector=true)` 将所有 `src` 都定义为 128 bit：

```text
src_0..src_4 : UInt(128.W)
src_4        : 物理上 128 bit，语义上主要是 8-bit VL 数据
```

VSplit 的：

```scala
val vvl = io.in.bits.src_vl.asTypeOf(VConfig()).vl
```

只是借助 Bundle 类型转换取出 `vl`。当前生成 RTL 已明确使用 `src_4[7:0]`，没有从
`src_4[127:8]` 解码 `vtype`。

因此 standalone 接口配置应采用：

```text
src_4[7:0]   = 当前动态 vl
src_4[127:8] = 普通 VLSU 中置 0
uop.vpu      = vsew/vlmul/veew/vm/vstart 等控制快照
```

完整 Core 中，`vset*` 负责更新架构 `vl/vtype`，Decode 将 vtype 快照放入 `uop.vpu`，
而逻辑 `vl` 寄存器 `Vl_IDX` 作为第五源在 issue 时读到 `src_4`。因此 `src_4` 是动态
VL 数据依赖，`uop.vpu` 是控制快照，二者必须表示同一条向量配置语义。

### 3.2 `src_0~src_4` 的含义

| 信号 | 物理宽度 | 普通 load | 普通 store | strided/indexed 用法 |
|---|---:|---|---|---|
| `src_0` | 128 | `rs1` 基地址 | `rs1` 基地址 | 地址基点，实际地址使用低 XLEN 位 |
| `src_1` | 128 | strided stride 或 index vector | stride 或 index vector | strided 是步长；indexed 是索引数据 |
| `src_2` | 128 | old `vd`，用于合并 | `vs3` store data | 数据/旧目的寄存器 |
| `src_3` | 128 | `v0` mask 内容 | `v0` mask 内容 | 仅 `vm=0` 时参与活动判定 |
| `src_4` | 128 | 当前 `vl` | 当前 `vl` | 普通 VLSU 只读取低 8 bit |

`src_3` 和 `uop_vpu_vmask` 不是同一回事：普通 VLSU 的输入活动 mask 来自 `src_3`，
而 `uop_vpu_vmask` 是通用 uop/写回合并元数据，不能用来替代 `src_3`。

whole-register 和 mask-register 有额外的有效长度换算：

- whole-register 访存由 field 数和 EEW 计算有效长度，不能简单把 `src_4` 当普通 VL。
- mask-register 访存将普通 `vl` 换算成 `ceil(vl/8)` 个 mask 字节。

### 3.3 `VConfig` 的配置示例

以普通 `vle32.v` 为例，假设 `VLEN=128`、`SEW=e32`、`LMUL=m1`、`VL=3`、无掩码：

```text
src_4[127:0]    = 128'h0000...0003
uop_vpu_vsew    = 2'b10       // e32
uop_vpu_vlmul   = 3'b000      // m1
uop_vpu_veew    = 2'b10       // e32
uop_vpu_vm      = 1'b1        // 不使用 v0.t
uop_vpu_vstart  = 0
uop_vpu_vuopIdx = 0
```

此时 `VLMAX=128/32=4`，`VL=3` 表示元素 0、1、2 是 body，元素 3 是 tail。

## 四、`fuType`、`fuOpType` 与 issue lane

### 4.1 `fuType`：谁处理

`fuType` 是 one-hot 功能单元类别。V2 向量访存重点类型为：

| `FuType` | 含义 | 普通 LSQ |
|---|---|---:|
| `vldu` | 普通非 segment vector load/store 路径中的 load 类 | 分配 LQ |
| `vstu` | 普通非 segment vector store | 分配 SQ |
| `vsegldu` | segment vector load | 不分配普通 LQ |
| `vsegstu` | segment vector store | 不分配普通 SQ |
| `vecArith` | 向量整数、浮点、置换、归约等 | 不分配 |
| `vset*` | 向量配置指令 | 不分配 |

当前 one-hot 编号中，常见 bit 为 `vldu=31`、`vstu=32`、`vsegldu=33`、`vsegstu=34`。
生成顶层是否保留某个字段还要以 `MemBlock.sv` 为准，因为 FIRRTL 会删除未被某条路径
消费的字段。

### 4.2 `fuOpType`：怎么访存

`fuOpType[8:7]` 区分 load/store，`[6:5]` 区分地址模式：

```text
[8:7] = 01：vector load
[8:7] = 10：vector store
[6:5] = 00：unit-stride
[6:5] = 01：unordered indexed
[6:5] = 10：strided
[6:5] = 11：ordered indexed
```

unit-stride 的低位还有特殊操作：

| 编码示例 | 含义 |
|---|---|
| `load 01_00_00000` | 普通 unit-stride load |
| `load 01_00_01000` | whole-register load |
| `load 01_00_01011` | mask load |
| `load 01_00_10000` | fault-only-first load |
| `store 10_00_00000` | 普通 unit-stride store |
| `store 10_00_01000` | whole-register store |
| `store 10_00_01011` | mask store |

`fuType` 和 `fuOpType` 必须一致。例如 `fuType=vsegstu` 却把 load/store 位编码成 load，
属于非法接口组合；局部 RTL 不一定在每个位置都主动报错，因此 testbench 不能依赖
模块内部自动纠正。

### 4.3 `issueVldu_0/1` 的握手

```text
fire = valid && ready
```

- `valid=1`：OOO 侧有一个准备好的向量访存 uop。
- `ready=1`：MemBlock 当前能接收该 payload。
- 只有 `fire` 的时钟沿才采样 payload。
- `valid=1 && ready=0` 时，所有 payload 字段必须保持不变。

两个 lane 的共同点是都能接收普通向量 load/store；差异是 segment 只允许通过 lane 0，
因为 `VSegmentUnit` 连接在 `issueVldu.head`。因此：

```text
port0.ready = 普通 load split 可接收
           || 普通 store split 可接收
           || 当前是 segment

port1.ready = 普通 load split 可接收
            || 普通 store split 可接收
```

`ready=1` 只表示资源能接收，不表示任意 `fuType/fuOpType` 组合都合法。

## 五、SEW、EEW、LMUL、EMUL、NFIELDS

### 5.1 为什么需要 EMUL

`SEW/LMUL` 是当前向量状态的默认数据布局；`EEW/EMUL` 描述某个具体操作数实际需要
的元素宽度和寄存器容量：

```text
EMUL = (EEW / SEW) * LMUL
```

设计推导如下：

```text
当前最大元素数 = LMUL * VLEN / SEW
该操作数所需 bit 数 = 当前最大元素数 * EEW
                 = LMUL * VLEN / SEW * EEW
                 = EMUL * VLEN
```

所以 EMUL 不是 uop 数，也不是 flow 数；它表达“该操作数需要多少个 VLEN 容量”。

V2 的指数实现为：

```text
emul = EewLog2(veew) - vsew + vlmul
```

普通合法值为 `mf8/mf4/mf2/m1/m2/m4/m8`。whole-register、mask-register 和 segment
路径有专门覆盖，不应机械套用普通公式。

### 5.2 `nf` 和 field

`nf` 表示 `NFIELDS-1`：

```text
nf=0 -> 1 field
nf=1 -> 2 fields
...
nf=7 -> 8 fields
```

segment 指令按 `fieldIdx` 逐字段访问；普通非 segment 通常 `nf=0`。whole-register 指令
只允许 `nf=0/1/3/7`，对应 1/2/4/8 个寄存器。

segment 还需要满足 field 数和 EMUL 的实现限制，通常可理解为：

```text
EMUL * NFIELDS <= 8
```

### 5.3 uop 数不是 flow 数

对普通非 indexed、单 field 指令，V2 常见分片可理解为：

```text
numUops = max(1, EMUL)
```

indexed 指令需要同时覆盖 index 和 data 两侧，V2 按较大的 `LMUL/EMUL` 分片：

```text
numUops = max(1, LMUL, EMUL)
```

segment 还要乘以 field 数。以上只是 uop 组织层的容量规则，不能把结果直接当成 flow 数。

例：`SEW=e8, LMUL=m1, index EEW=e16`：

```text
EMUL = (16/8) * m1 = m2
numUops = 2
```

每个 uop 的 flow 数还要按 index 侧公式单独计算。

## 六、`flowNum` 的完整计算

### 6.1 `flowNum` 的来源链

`flowNum` 在 issue 时不是重新计算的值，而是早期 `numLsElem` 的复制：

```text
Rename:
  uop.numLsElem = unit-stride ? 2 : GenRealFlowNum(...)

NewDispatch:
  enqLsq.req.bits.numLsElem = isVls ? uop.numLsElem : 1

VecMem IQ:
  entry.status.vecMem.numLsElem = s0_enqBits.numLsElem

Backend:
  issueVldu.flowNum = IQ_deq.numLsElem
```

因此，对同一个普通、非 segment、非 FOF-fix-VL uop：

```text
issueVldu.flowNum == enqLsq.req.numLsElem == uop.numLsElem
```

LSQ enqueue 早于 issue 很多周期，实际追踪必须使用 `(robIdx, vuopIdx)`，不能要求两个
接口在同一拍出现。

### 6.2 strided

strided 使用：

```text
flowNum = MulDataSize(EMUL) / EEW_bytes
```

`MulDataSize` 表示单个 VLSU uop 的数据容量：

| EMUL | 单 uop 容量 |
|---|---:|
| `mf8/mf4/mf2` | 2/4/8 byte |
| `m1/m2/m4/m8` | 16 byte |

例：`vlse32.v`，`SEW=e32, LMUL=m1, EEW=e32`：

```text
EMUL      = (32/32) * m1 = m1
EMUL bytes= 16
EEW bytes = 4
flowNum   = 16 / 4 = 4
```

若 `VL=3`，入口 `flowNum` 仍是 4；后续 `flowMask=4'b0111`，实际只发出元素 0、1、2
对应的三个 active flow。

### 6.3 indexed

indexed 同时有 index stream 和 data stream。先分别计算：

```text
indexCapacity = MulDataSize(EMUL) / EEW_bytes
dataCapacity  = MulDataSize(LMUL) / SEW_bytes
```

V2 的选择条件是：

```text
EMUL > LMUL  -> flowNum = indexCapacity
EMUL <= LMUL -> flowNum = dataCapacity
```

这里比较的是带符号 LMUL 指数，不是原始编码的无符号数。

例一：`vluxei16.v`，数据 `SEW=e8, LMUL=m1`，index `EEW=e16`：

```text
EMUL = (16/8) * m1 = m2
indexCapacity = 16 / 2 = 8
dataCapacity  = 16 / 1 = 16
EMUL > LMUL，因此 flowNum = 8
```

`EMUL=m2` 使宏指令拆成 2 个 uop；若 `VL=9`，两个 uop 的入口 `flowNum` 都是 8，但
实际 active 数分别可能是 8 和 1。

例二：数据 `SEW=e32, LMUL=m2`，index `EEW=e16`：

```text
EMUL = (16/32) * m2 = m1
indexCapacity = 16 / 2 = 8
dataCapacity  = 16 / 4 = 4
EMUL <= LMUL，因此 flowNum = 4
```

此时数据侧 `m2` 需要两个 uop，每个 uop 覆盖 4 个 e32 数据元素；index 侧一个 m1
寄存器可以提供 8 个 e16 index，两个数据 uop 分别消费它们。

### 6.4 unit-stride

unit-stride 在 Rename 时还不知道基地址低位和 byte mask 是否跨 16B 边界，因此不用
`GenRealFlowNum`，统一保守：

```text
issueVldu.flowNum = numLsElem = 2
```

VSplit 得到地址后才计算真正的 `activeNum`：

```text
有效字节都落在低 16B 块 -> 1 个 flow
有效字节跨低/高两个块 -> 2 个 flow
没有有效字节           -> 0 个 flow
```

例如 `vle32.v`、`VL=3`：

- base=`0x1000`：12 个有效字节位于同一 16B 块，实际 1 flow。
- base=`0x1008`：12 个有效字节跨两个 16B 块，实际 2 flow。

入口 `flowNum` 两种地址都保持 2。

### 6.5 `flowNum=0` 是否存在

`flowNum` 是 5-bit 无符号字段，因此小于 1 只有 0，没有负数。

| 位置 | `flowNum=0` 的语义 |
|---|---|
| 普通 `issueVldu` data-uop | 不合法；正常候选数至少为 1，unit-stride 为 2 |
| FOF `fix-VL` 尾 uop | 可以为 0；它被排除在普通 VLSplit 外 |
| merge buffer 实际 `flowNum` | 可以为 0；例如 VL=0、mask 全关或 vstart>=VL |

因此 `VL=0` 不会把入口 `issueVldu.flowNum` 改为 0，只会让后续实际 active count 为 0。

普通配置可编码到 5 bit 的 `0..31`，但 V2 普通 VLSU 合法候选范围通常是 `1..16`，
`17..31` 不是当前参数下的合法 flow 配置。

## 七、`flowMask`、byte mask 和 active flow

### 7.1 `flowMask` 的生成

VSplit 先生成整条宏指令的元素活动集合：

```text
elementEnable[i] = (i >= vstart) && (i < effectiveVL) &&
                   (vm == 1 || src_3[i] == 1)
```

然后按 `vuopIdx`、`EMUL/LMUL`、`EEW/SEW` 选择当前 uop 的局部窗口。概念上可以写成：

```text
F = 当前 uop 的候选窗口跨度
P = 当前 uop 之前的 flow 数
Q = 当前 uop 结束位置
D = 当前目的寄存器片段之前的 flow 数

windowMask = elementEnable & LowMask(Q) & ~LowMask(P)
shift      = indexed && (EMUL > LMUL) ? P : D
flowMask   = windowMask >> shift
```

部分 replay 时，正常计算被覆盖：

```text
flowMask = vecReplayMask
```

`flowMask` 每一位先描述一个局部逻辑元素/flow 位置；之后 `GenUopByteMask` 按 EEW/SEW
把一位扩展为 1、2、4 或 8 个 byte mask。unit-stride 再用这些 byte mask 判断访问块。

### 7.2 四个计数不要混淆

| 字段 | 含义 | 是否受 VL/mask 影响 |
|---|---|---:|
| `issueVldu.flowNum` | Dispatch 预留和 split 遍历的候选槽位数 | 否 |
| `flowMask` | 当前 uop 中打开的逻辑位置集合 | 是 |
| `toMergeBuffer.req.flowNum` | 实际需要等待的 flow 数，即 `PopCount(flowMask)` 或 unit-stride 的 0/1/2 | 是 |
| merge entry `flowNum` | 收到每个 flow writeback 后递减的剩余数 | 入 merge 时确定 |

普通 strided/indexed 中，一个 active flow 通常近似一个元素访问；unit-stride 中，一个
flow 是带 byte mask 的 128-bit 对齐块，可能覆盖多个元素。

## 八、flow 与 LSQ entry 的绑定

### 8.1 `needAlloc` 和 `req.valid`

普通向量 load/store 的 Dispatch 预留编码为：

```text
needAlloc = 2'b00：不分配
needAlloc = 2'b01：load / vector load
needAlloc = 2'b10：store / vector store
needAlloc = 2'b11：当前 Dispatch 不生成
```

但 `needAlloc` 不能单独证明 entry 已创建。LsqWrapper 实际 gate 为：

```scala
loadQueue.io.enq.req(i).valid  := needAlloc(i)(0) && enq.req(i).valid
storeQueue.io.enq.req(i).valid := needAlloc(i)(1) && enq.req(i).valid
```

随后还要通过 `canAccept` 和 `!robIdx.needFlush(redirect)` 才会写入 queue 的
`allocated` entry。

从 Dispatch 到 MemBlock 顶层还隔着 `LsqEnqCtrl` 的一拍寄存：

```text
top.needAlloc(t) = RegNext(dispatch.needAlloc)(t)
top.req.valid(t) = RegNext(dispatch.req.valid && !redirect && canAccept)(t)
top.req.bits(t)  = RegEnable(dispatch.req.bits, do_enq)(t)
```

所以完整 core 的周期精确模型中，`top.req.valid=0` 时 `top.needAlloc` 可以仍保留上一拍的
非零类别位，`req.bits` 也可保持旧值；不能把 `req.valid=0 -> needAlloc=0` 写成全核硬约束。
不过独立驱动顶层 MemBlock 时可采用 `valid=0, needAlloc=0, payload=0` 的 canonical idle，
因为真正分配的条件仍是两个 bit 的与。一个向量 request 没有 ready，应只 pulse 一个周期；
随后由 `(robIdx,uopIdx)` 账本把它的 `numLsElem`、LQ/SQ 首指针关联到未来的 issue，而不是
按 enqueue slot 或 issue lane 关联。字段级约束见
[V2 Vector Issue Agent 接口知识](../../../interface/v2/agents/vecissue_agent.md)。

### 8.2 普通向量 LS 的连续范围

对于普通向量 load：

```text
LQ 范围 = [lqIdx, lqIdx + flowNum - 1]
```

对于普通向量 store：

```text
SQ 范围 = [sqIdx, sqIdx + flowNum - 1]
```

VLSplit/VSSplit 对第 `splitIdx` 个候选 flow 使用：

```text
load  : uop.lqIdx + splitIdx
store : uop.sqIdx + splitIdx
```

所以同一普通 uop 必须满足：

```text
enqLsq.req.numLsElem == issueVldu.flowNum
issueVldu.lqIdx/sqIdx == enqLsq 返回的首 entry
```

不一致会导致少分配、资源泄漏、覆盖其他指令的 entry，或使 replay/异常无法匹配正确
的 `(robIdx, vuopIdx)`。

### 8.3 哪些向量 uop 不占普通 LQ/SQ

| 向量类别 | 普通 `enqLsq.req.valid` | 普通 LQ/SQ entry | 实际路径 |
|---|---:|---:|---|
| 普通 `vldu/vstu` | 1 | 有 | VLSplit/VSSplit + LQ/SQ |
| 单 field FOF data-uop，`isVleff=1,lastUop=0` | 1 | 有 LQ | 普通 VLSplit + VfofBuffer 汇总 |
| FOF `fix-VL` 尾 uop，`isVleff=1,lastUop=1` | 0 | 无 | VfofBuffer 最终 VL writeback |
| segment `vsegldu/vsegstu` | 0 | 无普通 entry | VSegmentUnit 专用 FSM |
| vector arithmetic/FP/permutation/reduction | 可能随共享 bundle valid | 无，`needAlloc=0` | 各自向量执行单元 |
| `vset*` | 可能经过共享 bundle | 无 | VConfig/VL 更新 |

## 九、FOF、`isVleff` 和 `lastUop`

### 9.1 `isVleff` 的来源

Decode 的语义是：

```scala
isVload = FuType.isVLoad(decodedInst.fuType)
isFof   = isVload && (decodedInst.fuOpType === VlduType.vleff)
isVleff = isFof && (NF == 0)
```

因此 `isVleff=1` 表示“普通单 field FOF 类型”，不是表示本次已经发生 fault。segment
FOF 因 `NF != 0`，`isVleff` 为 0，但 `VSegmentUnit` 仍会根据 `fuOpType==vleff` 和
`lastUop` 进入自身的 FOF 收尾状态。

### 9.2 `lastUop` 的层次

- Decode/DecodeUnitComp 的通用 `lastUop`：宏指令拆分后的 backend uop 序列末尾。
- VecMem IQ 出队时：复制为 `uop.vpu.lastUop`，形成 `uop_vpu_lastUop` 接口字段。
- VSplit 内部的 `x.uop.lastUop`：VLSU data-uop 序列末尾，是另一层标记。

`lastUop=1` 不等于最后一个元素、最后一个 flow，也不等于 `flowMask` 最高位。

### 9.3 FOF 三种组合

| `isVleff` | `lastUop` | 含义 | LSQ/VLSU 行为 |
|---:|---:|---|---|
| 0 | 0/1 | 普通 vector LS | 正常 VLSplit/VSSplit |
| 1 | 0 | FOF data-uop | 仍进 LQ、VLSplit 和 merge |
| 1 | 1 | FOF `fix-VL` 尾 uop | `numLsElem=0`，不进普通 LSQ/VLSplit |

FOF data-uop 的非首元素 fault 会缩短记录的 `vl`；元素 0 fault 仍保留精确异常。最后
`fix-VL` uop 由 `VfofBuffer` 汇总并通过 `vlWen` 写回新 VL，不能把它理解成最后一个
真实 load flow。

### 9.4 FOF 的发射顺序与可穿插范围

完整 V2 后端不允许同一 FOF 的 data-uop 与 `fix-VL` tail 任意乱序发射。虽然 tail 不申请普通 LQ entry，`LsqEnqCtrl` 仍会给它形成位于所有 FOF data-uop LQ range 之后的 prefix `lqIdx`；VecMem IQ 对全部 `isVleff=1` uop 用 `lqIdx == lqDeqPtr` 解除 blocked。因此 data-uop 按 LQ range 完成顺序发射，tail 只能在最后一个 data-uop range 完成后到达 `VfofBuffer`。

该顺序只串行化当前 FOF 链，不是所有向量执行的全局屏障。普通 `isVleff=0` 且独立的向量 uop 可以在实际 issue 时间线上穿插；但需要 FOF 更新后 VL 的后续向量 uop 必须等待 tail 的 `vlWen`。`VfofBuffer` 只支持一个活动 FOF，不能同拍接收两个 FOF，也不能在 tail writeback/redirect 前接收另一条 FOF。直接驱动 MemBlock 顶层时已绕过 IQ 门控，测试框架必须自行按 data-uop 完成顺序发送，并在所有 data-uop 完成后再发送 tail。完整推导见 [V2 正常向量访存 uop 与 flow 拆分](vector_memory_uop_flow_decomposition.md#fof-的发射顺序乱序边界与可穿插范围)。

### 9.5 非 FOF vector store 的异常收尾

当前 V2 MemBlock 中，除 FOF 外，普通 vector store 的 StoreQueue 也直接消费宏指令的
`lastUop`：在历史 `enqLsq` 进入时，只有宏指令末 uop 的连续 SQ 预留范围尾 entry 会被
标为 `vecLastFlow`。若该宏指令的 vector store 在此边界前出现异常，
`vecExceptionFlag` 会抑制同一 `robIdx` 的后续 entry 写入 SBuffer；尾 entry 经过 data
buffer 时清除 flag。

这是一条 store 异常写入抑制边界，不参与普通 vector load，也不决定 `flowMask`、
`activeNum` 或 VSplit 的实际 flow 生成。`lastUop=1` 提前或漏置会分别提前或延后该边界。
完整赋值时序见 [V2 正常向量访存 uop 与 flow 拆分](vector_memory_uop_flow_decomposition.md) 的 6.2.3 节。

## 十、segment 向量访存

segment 指令使用 `vsegldu/vsegstu`，只能由 `issueVldu_0` 进入 `VSegmentUnit`。它不把
每个 field/segment 当作普通 LQ/SQ flow，而由本地状态管理：

```text
segmentIdx：当前逻辑 segment/元素位置
fieldIdx  ：当前 field
uopFlowNum：segment unit 自己的候选范围
FSM       ：TLB、PMP、cache、merge、SBuffer、finish/FOF 收尾
```

`VSegmentUnit` 维护自己的 buffer、`fieldIdx`、`segmentIdx` 和 `s_fof_fix_vl` 状态，
因此不能把 segment 的 `flowNum`、LSQ entry 或 writeback 直接套用普通 VLSU 公式。

## 十一、向量 load/store 的完成、写回和反馈

### 11.1 向量 load

```text
issueVldu
  -> VLSplit
  -> 多个 LoadUnit flow
  -> VLMergeBuffer
  -> writebackVldu
```

VLMergeBuffer 会：

1. 建立一个 uop 级 merge entry。
2. 记录实际 active flow 数。
3. 按元素位置、byte mask 和旧 `vd` 合并 load data。
4. 保存最早异常及对应 `vstart/vl`。
5. 所有 active flow 完成后只产生一次 uop writeback 和 LSQ feedback。

因此 `writebackVldu.data` 是合并结果，不是某一个 flow 的简单直通数据。

### 11.1.1 向量写使能与目的寄存器类别

`issueVldu.uop.vecWen/v0Wen/vlWen` 描述 writeback 应回到哪一个物理寄存器文件，不是
`v_mode`、`vm` 或 `vma/vta` 的独立属性。V2 的派生关系为：

```text
isFofTail = uop.vpu.isVleff && uop.vpu.lastUop

isFofTail：
  vecWen=0, v0Wen=0, vlWen=1

非 FOF tail 的 vector store：
  vecWen=0, v0Wen=0, vlWen=0

非 FOF tail 的 vector load：
  ldest==0：  vecWen=0, v0Wen=1, vlWen=0
  ldest!=0：  vecWen=1, v0Wen=0, vlWen=0
```

这里的 `ldest` 是 Decode/后台 uop 模板中的架构目的寄存器字段，不是 MemBlock 单独重新
计算的值。`VecDecoder.VLD` 对 `vlm.v` 也先给普通 vector write；所以 mask load 只有在
目的寄存器确实为 `v0` 时才使用 `v0Wen`。`vm=0` 的 mask 读取与 `v0Wen` 的目的写入是
两个独立方向。LMUL 跨越 `v0` 时，不同 data-uop 可以分别出现 `v0Wen=1` 和 `vecWen=1`。
Decode 还断言三个写使能至多一个为 1；`pdest` 必须匹配对应 physical free-list。

### 11.2 向量 store

```text
issueVldu
  -> VSSplit
  -> 多个 StoreUnit flow
  -> VSMergeBuffer
  -> writebackVldu + vstuIqFeedback
```

store 没有向量目的数据，但仍需报告 uop 完成、异常和 replay 状态。一般情况下：

```text
vecWen = 0
v0Wen  = 0
vlWen  = 0
```

`robIdx`、异常字段、`uopIdx` 和 store feedback 仍然有效。

### 11.3 writeback 仲裁

当前 V2 的主要向量写回优先级为：

```text
writebackVldu_0:
  VSegmentUnit > VLMergeBuffer0 > VSMergeBuffer0

writebackVldu_1:
  VfofBuffer > VLMergeBuffer1 > VSMergeBuffer1
```

因此不能简单理解成“issue lane 0 永远对应 writeback 0”：

- segment 只能 lane0 进入，也从 WB0 返回。
- FOF 最终 VL 修正固定优先使用 WB1。
- 普通 load/store 通常按对应 lane 的 merge buffer 返回。

生成顶层当前没有把所有 Scala 内部反馈都暴露成端口；例如 `vlduIqFeedback` 可能在
生成后被裁剪，而 `vstuIqFeedback_0/1.feedbackSlow` 仍可观察。接口分析必须以当前
`build/rtl/MemBlock.sv` 为准，不能只从 Bundle 名称推断端口存在。

## 十二、部分 replay

假设一条 indexed store 的原始 `flowNum=8`，flow 2 和 flow 5 未完成。VSMergeBuffer
可以反馈：

```text
isVecPartReplay = 1
vecReplayMask   = 8'b0010_0100
vecReplayMbIdx  = 原 merge buffer entry
flowNum         = 仍为 8
sqIdx           = 原 SQ 首指针
robIdx/vuopIdx  = 原 uop 身份
```

replay 时：

```text
正常执行：flowMask = vm/src_3/vstart/VL 计算结果
部分 replay：flowMask = vecReplayMask
```

它复用原 merge entry 和 LSQ entry，不重新 Dispatch，也不重新分配 LSQ。`flowNum` 不能
改成 `PopCount(vecReplayMask)`，因为 split buffer 仍需遍历原始候选槽位并保持 `splitIdx`
到 `lqIdx/sqIdx` 的映射。

`vecReplayMask` 是 VLSU flow 掩码，不是 `v0` 元素 mask；unit-stride 中一位 flow 还可能
代表一整段 16B 访问块。

## 十三、接口配置与检查清单

### 13.1 普通 strided load

以 `vlse32.v`、`SEW=e32, LMUL=m1, EEW=e32, VL=3` 为例：

```text
src_0        = base address
src_1        = scalar stride
src_2        = old vd
src_3        = v0 mask (vm=1 时可置 0)
src_4[7:0]   = 3
src_4[127:8] = 0

EMUL         = m1
flowNum      = 16B / 4B = 4
flowMask     = 4'b0111  (无掩码、vstart=0 时)
activeNum    = 3
```

`enqLsq.req.numLsElem` 和 `issueVldu.flowNum` 都应为 4，LQ 首指针加上
`splitIdx=0/1/2/3` 才能覆盖同一预留范围。

### 13.2 普通 unit-stride load

以 `vle32.v`、`VL=3` 为例：

```text
flowNum = 2              // dispatch 保守预留
base=0x1000 -> activeNum=1
base=0x1008 -> activeNum=2
```

不要看到 `flowMask` 有 3 个元素 bit，就把入口 `flowNum` 填成 3；unit-stride 的 flow
是对齐访问块，不是一元素一 flow。

### 13.3 indexed load

以数据 `e8,m1`、index `e16` 的 `vluxei16.v` 为例：

```text
EMUL = m2
numUops = 2
每个 uop flowNum = 16B / 2B = 8
```

`VL=9` 时，第一个 uop 的实际 active flow 可为 8，第二个可为 1，但两者入口
`flowNum` 都是 8。

### 13.4 观察波形的推荐顺序

```text
1. 先确认 issueVldu.valid && ready，得到 fire。
2. 查看 fuType/fuOpType 是否属于同一地址模式和同一 load/store 方向。
3. 查看 robIdx、vuopIdx、lqIdx/sqIdx 和历史 enqLsq 请求是否匹配。
4. 查看 src_4[7:0]、vstart、vm、src_3，重算 effectiveVL 和元素活动范围。
5. 查看 vsew/vlmul/veew/nf，重算 EMUL、numUops 和 flowNum。
6. 查看 VSplit.flowMask，再看 unit-stride byte mask 或 indexed/strided 地址。
7. 查看 merge buffer 的 activeNum 和剩余 flowNum。
8. 最后判断 writeback、vstuIqFeedback、replay 或 exception。
```

### 13.5 约束随机化的总原则

将 `issueVldu` 当作后端已经生成的 uop ABI 时，不能把其全部 bit 做平铺随机。应先随机
宏指令 descriptor，再按下面顺序派生顶层输入：

```text
类别/地址模式/SEW/LMUL/EEW/NFIELDS/vm/vma/vta
  -> EMUL、VLMAX、VL/vstart、VLSU uop 模板与 flowNum
  -> source、fuType/fuOpType、vuopIdx/lastUop/isVleff、写使能
  -> ROB/FTQ/pdest 与 enqLsq 产生的 lqIdx/sqIdx
  -> valid/ready fire
  -> writeback 或 feedback 产生完成/partial replay 状态
```

其中 `vsew/vlmul/veew` 的普通约束是：

```text
emulExp = signed(vlmul) + veew - vsew
-3 <= emulExp <= 3
VLMAX = 2^(4 + signed(vlmul) - vsew)
0 <= VL <= VLMAX
0 <= vstart <= VLMAX
```

`flowNum`、`vuopIdx`、`lastUop`、`isVleff`、`lqIdx/sqIdx` 和 replay 三字段均是依赖值：

- `flowNum` 从地址模式和 EMUL/LMUL 计算，普通 uop 必须匹配历史
  `enqLsq.req.numLsElem`，而不是等于 `VL` 或 `PopCount(flowMask)`。
- `vuopIdx/lastUop` 从真实 Decode uop 模板取得；FOF 的 `fix-VL` tail 是
  `isVleff=1 && lastUop=1 && flowNum=0` 的专用 uop。
- `lqIdx/sqIdx` 由历史连续 LSQ 分配得到；`splitIdx=k` 使用起始指针的环形偏移。
- 首次 issue 的 replay 三字段全部为 0；只有 `vstuIqFeedback.feedbackSlow` 请求重放时，
  才能复用原 uop/LSQ 资源并填入反馈给出的非零 mask 和 merge-buffer index。

对于 FOF，以上“按模板派生”还包括跨周期次序：在完整后端由 VecMem IQ 的 LQ pointer 维持；在 standalone 顶层 driver 中必须显式维持。不得仅因 tail `flowNum=0` 就跳过对 data-uop 完成状态的等待。

`ready`、writeback 和 feedback 都是 DUT 输出，不应由随机 driver 填值。`valid=1 &&
ready=0` 时则必须保持整个 payload 不变。字段逐项合法域、例外和推荐 checker 公式见
[V2 向量 issue agent 接口知识](../../../interface/v2/agents/vecissue_agent.md#约束随机化生成与字段依赖)。

## 十四、常见误区

| 误区 | 正确理解 |
|---|---|
| `src_4` 是完整 128-bit VConfig | 逻辑对象是 `VConfig`，普通 VLSU 实际只读 `src_4[7:0]` 的 VL |
| `vlmul` 的编码是普通无符号倍率 | 它是带符号指数，`111` 是 `mf2` |
| `EMUL` 就是 uop 数 | EMUL 是操作数容量倍率，uop 数还受 field 和 indexed 两侧影响 |
| `flowNum=VL` | flowNum 是候选预留数，VL 只影响后续 flowMask/activeNum |
| 一个 flow 永远等于一个元素 | strided/indexed 常近似如此，unit-stride 一个 flow 是 16B 访问块 |
| `flowNum=0` 表示普通指令没有活动元素 | 普通入口 flowNum 不因 VL/mask 变 0；0 主要是 FOF 尾 uop 或 merge 实际计数 |
| `uop_vpu_vmask` 就是输入 v0 mask | 普通输入活动 mask 来自 `src_3`，vmask 是 uop/写回合并元数据 |
| `lastUop=1` 是最后一个 flow | lastUop 是 uop 序列标记，不是 flow 序号 |
| `isVleff=1` 表示已经发生 fault | 它是静态 FOF 类型标签，动态 fault 由 flow/merge 结果产生 |
| segment 可以套普通 LQ/SQ 规则 | segment 走 `VSegmentUnit` 专用 FSM，不占普通 LQ/SQ |
| replay 要重新分配 LSQ | 部分 replay 复用原 entry 和 merge buffer，保留原 flowNum |
| 只看 `needAlloc` 就能判断 LSQ 分配 | 还要结合 `req.valid`、controller、redirect 和 queue 侧 `allocated` |

## 十五、关联文档

- [V2 向量 issue agent 接口知识](../../../interface/v2/agents/vecissue_agent.md)：顶层字段、位宽、UVM 映射和当前 driver 边界。
- [V2 正常向量访存 uop 与 flow 拆分](vector_memory_uop_flow_decomposition.md)：普通 VLSU flow、EMUL、LSQ、FOF 和 segment 的源码细节。
- [V2 LSQ 入队与 Redirect 恢复 flow](lsq_enqueue_redirect_flow.md)：本文只引用其中的向量 LSQ admission 边界，不展开标量 redirect flow。
- [RISC-V 向量寄存器与控制状态分析](../../riscv_vector_register_state_analysis_20260814.md)：RVV 架构 CSR 和寄存器语义。
- 参考输入：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan-test/AI_DOC/analysis/rtl/v2/vec_interface.md`。

## V2/V3 差异

本文只核验 V2。V3 的 `vecIssue` 聚合形态、issue lane 数量、VLSU 宽度、`src_4` 物理
端口和生成后端口裁剪必须在 V3 分支单独核验，不能把本文的 `issueVldu_0/1`、128-bit
source 和 16-byte per-uop 假设直接套用到 V3。

## 源码证据

- `src/main/scala/xiangshan/backend/fu/vector/Bundles.scala:123-130,167-213`：`VConfig`、`VLmul`、`Vl`、`VEew` 和字段编码。
- `src/main/scala/xiangshan/backend/Bundles.scala:420-465,963-978`：`VPUCtrlSignals`、`MemExuInput`、五个向量 source 和 `flowNum`。
- `src/main/scala/xiangshan/backend/datapath/DataConfig.scala:16-22`、`src/main/scala/xiangshan/backend/fu/FuConfig.scala:757-796`：`VlData` 的 8-bit 宽度和 VLDu/VSTu 源配置。
- `src/main/scala/xiangshan/backend/decode/DecodeUnit.scala:857-863,1043-1082`：`Vl_IDX`、v0、vtype/vpu 控制快照和 `isVleff`。
- `src/main/scala/xiangshan/backend/decode/VecDecoder.scala:165-184`、`src/main/scala/xiangshan/backend/decode/DecodeStage.scala:234-255`：`VLD/VST` 的默认目的写使能、`ldest==0` 到 `v0Wen` 的转换，以及写使能互斥断言。
- `src/main/scala/xiangshan/backend/rename/Rename.scala:319-321,402-408`、`src/main/scala/xiangshan/backend/dispatch/NewDispatch.scala:239-250`：`vecWen/v0Wen/vlWen` 对应三类 free-list 和 `pdest` 分配。
- `src/main/scala/xiangshan/backend/rename/Rename.scala:211-250`：EMUL 与 `numLsElem` 计算，FOF 尾 uop 令 `numLsElem=0`。
- `src/main/scala/xiangshan/backend/dispatch/NewDispatch.scala:538-598,688-706`：segment/AMO/FOF gate、`needAlloc` 和普通 enqueue valid。
- `src/main/scala/xiangshan/backend/issue/IssueQueue.scala:1189-1245`、`src/main/scala/xiangshan/backend/issue/EntryBundles.scala:491-502`、`src/main/scala/xiangshan/backend/Backend.scala:793-832`：VecMem IQ 保存元数据，Backend 将 `numLsElem` 复制为 `issueVldu.flowNum`；FOF entry 以 `lqIdx == lqDeqPtr` 解除发射阻塞。
- `src/main/scala/xiangshan/mem/vector/VecCommon.scala:325-335,530-621,641-668,707-718`：`MulDataSize`、`GenRealFlowNum`、`GenRealFlowLog2`、whole/mask VL 和 `GenFlowMask`。
- `src/main/scala/xiangshan/mem/vector/VSplit.scala:55-180,225-254,315-476`：uop 窗口、flowMask、unit-stride 1/2 flow、splitIdx 和 flow 发射。
- `src/main/scala/xiangshan/mem/vector/VMergeBuffer.scala:78-129,309-419`：实际 active flow 计数、完成递减、数据合并和 LSQ feedback。
- `src/main/scala/xiangshan/mem/lsqueue/LSQWrapper.scala:154-182,361-429`、`src/main/scala/xiangshan/mem/lsqueue/VirtualLoadQueue.scala:92-183`、`src/main/scala/xiangshan/mem/lsqueue/StoreQueue.scala:355-423`：needAlloc/req.valid gate、连续 entry 和 queue allocated；FOF tail 即使不分配 entry，仍取得 prefix `lqIdx` 作为 IQ 排序端点。
- `src/main/scala/xiangshan/mem/lsqueue/StoreQueue.scala:365-405,1202-1249,1351-1405`：普通 vector store 的 `lastUop` 在 SQ 连续范围尾 entry 形成 `vecLastFlow`，并控制异常后的 `vecExceptionFlag` 和 SBuffer 写入抑制。
- `src/main/scala/xiangshan/mem/MemBlock.scala:1565-1757,2059-2070`：issue lane ready、VLSplit/VSSplit、VfofBuffer、writeback 仲裁和 segment 连接。
- `src/main/scala/xiangshan/mem/vector/VfofBuffer.scala:41-145`、`src/main/scala/xiangshan/mem/vector/VSegmentUnit.scala:180-235,360-410,875-965`：普通和 segment FOF 收尾；普通 `VfofBuffer` 只接受一个活动 FOF，并对跨 ROB/尾后 FOF 发出断言。
- `build/rtl/MemBlock.sv:719-793`、`build/rtl/VLSplitPipelineImp.sv:350-404,572-626`：生成顶层端口宽度、`src_4[7:0]`、实际 flowMask 和 merge flowNum。
- `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan-test/AI_DOC/analysis/rtl/v2/vec_interface.md`：向量接口学习顺序、接口字段和边界的参考输入，已与当前 V2 源码交叉核对。

## 知识修订记录

| 日期 | commit | 旧结论 | 新结论 | 修订原因 | 影响范围 |
|---|---|---|---|---|---|
| 2026-08-18 | `4ce3563a01254700ed5828797f288bafcc2b491c` | 向量寄存器、接口、flow、LSQ、FOF 等知识分散在多个文档，`src_4`/`VConfig` 和 `flowNum` 的关系需要跨文档拼接。 | 新建本综合文档，统一描述 RVV 状态、V2 issue interface、EMUL/uop/flow、LSQ 绑定、FOF/segment、replay 和写回，并吸收 `XiangShan-test/vec_interface.md` 的接口视角。 | 用户要求将本 agent 的向量分析整合成一份详细知识文档。 | V2 RVV、VecMem IQ、MemBlock、VLSU、LSQ、VSegmentUnit、VfofBuffer。 |
| 2026-08-19 | `4ce3563a01254700ed5828797f288bafcc2b491c` | 接口配置清单只说明静态字段含义和观察顺序，缺少可用于约束随机环境的依赖生成模型。 | 增加 descriptor -> 派生 uop -> 历史 LSQ 分配 -> feedback/replay 的随机化顺序，明确随机输入、派生字段和 DUT 输出的边界。 | 用户要求梳理向量输入信号的合法约束及其依赖关系。 | V2 `issueVldu_0/1`、Rename、Dispatch、VecMem IQ、VSplit、VMergeBuffer、LSQ、writeback/feedback。 |
| 2026-08-19 | `4ce3563a01254700ed5828797f288bafcc2b491c` | `needAlloc` 与 `req.valid` 的功能门控已说明，但缺少其到 MemBlock 顶层的寄存时序，容易把 standalone idle 简化误当作全核时序约束。 | 增加 LsqEnqCtrl 的 `RegNext/RegEnable` 边界、同拍 slot 与未来 issue 的账本关联，并链接到逐字段随机约束。 | 用户要求区分被依赖信号与派生信号的约束行为。 | V2 LsqEnqCtrl、LSQWrapper、top `enqLsq`、issueVldu。 |
| 2026-08-21 | `4ce3563a01254700ed5828797f288bafcc2b491c` | `lastUop` 章节仅说明 FOF 收尾，普通 vector store 的异常写入边界未记录。 | 增加 `lastUop -> vecLastFlow -> vecExceptionFlag` 摘要，明确其只影响 StoreQueue 异常后的 SBuffer 写入抑制，不是普通 load 或 VSplit flow 计数条件。 | 用户追问 FOF 之外的 `lastUop` 影响。 | V2 LsqEnq、StoreQueue、SBuffer、普通 vector store。 |
| 2026-08-25 | `4ce3563a01254700ed5828797f288bafcc2b491c` | FOF 小节没有明确 data-uop、tail 和其他向量 uop 的实际 issue 次序。 | 补充由 LQ pointer 驱动的 FOF 串行化、`VfofBuffer` 单活动限制、独立非 FOF 的可穿插范围，以及 standalone 顶层 driver 需自行实现排序的边界。 | 用户追问 FOF data-uop 与 fix-VL tail 是否能乱序发送。 | V2 LsqEnqCtrl、VecMem IQ、VfofBuffer、MemBlock vector issue。 |
| 2026-08-25 | `4ce3563a01254700ed5828797f288bafcc2b491c` | 写回章节未明确 `vecWen/v0Wen/vlWen` 与 `ldest`、mask load 和 FOF tail 的关系。 | 增加三类目的寄存器写使能的派生规则，明确 `vlm.v` 不自动写 `v0`，以及 LMUL 跨 `v0` 时不同 data-uop 可使用不同写使能。 | 用户要求分析 `issueVldu` 三个写使能与向量类型的关联。 | V2 Decode、Rename、VecMem IQ、issueVldu、向量 writeback。 |

## 待确认项

- 无。本文已将参考文档中与当前 V2 源码一致的向量结论合并；V3 差异和任何未来生成 RTL 的端口变化需要重新核验。
