# V2 StoreQueue 功能特性

## 1. 文档定位

本文总结 V2 `StoreQueue` 的主要功能特性、状态组织、上下游协作关系和当前源码限制，作为后续
RTL 信号分析、功能场景建模和覆盖率规划的功能语义参考。

本文以 Scala/Chisel 源码
`src/main/scala/xiangshan/mem/lsqueue/StoreQueue.scala` 为主要依据。具体覆盖率采样信号、
接口端口名称和生成后的层次关系，仍需以当前 `build/rtl` 中的 Verilog RTL 为准。

| 项目 | 内容 |
|---|---|
| 目标版本 | V2 |
| 当前分支 | `mem_ut_uvm_v2` |
| 主要源码 | `src/main/scala/xiangshan/mem/lsqueue/StoreQueue.scala` |
| 关联模块 | `SQAddrModule`、`SQDataModule`、`DatamoduleResultBuffer`、`StoreMisalignBuffer`、`SBuffer`、`Uncache`、ROB |
| 文档日期 | 2026-10-14 |

## 2. 总体职责

`StoreQueue` 是后端 store 指令的有序暂存和下发中心，负责连接 dispatch、StoreUnit、LoadUnit、
ROB、SBuffer、Uncache、CMO 处理逻辑、StoreMisalignBuffer 以及向量反馈路径。

它的核心职责是：

1. 为投机执行的 store 分配和维护 SQ entry。
2. 分别保存 store 的 uop、虚拟地址、物理地址、数据和 byte mask。
3. 为年轻 load 提供 store-to-load forwarding 和依赖阻塞信息。
4. 跟踪 store 的地址就绪、数据就绪、ROB 提交和最终完成状态。
5. 根据 cacheable、NC、MMIO、CBO、非对齐和异常类型选择不同的下发路径。
6. 在 redirect 或异常恢复时取消未提交的年轻 store，并恢复队列指针。
7. 处理标量 store、向量 store、向量异常反馈和跨页非对齐 store。

因此，StoreQueue 不是单纯的数据 FIFO，而是同时承担了 store 生命周期管理、内存顺序维护、
forwarding 查询、异常协调和下游请求仲裁功能。

## 3. 队列结构与 entry 状态

### 3.1 环形队列指针

`SqPtr` 由 `flag` 和 `value` 两部分组成，用于区分环形队列中相同 slot 的不同代次。源码维护
多组指针，各自代表不同阶段的边界：

| 指针 | 功能 |
|---|---|
| `enqPtrExt` | dispatch 分配新 SQ entry 的尾指针 |
| `cmtPtrExt` | ROB 提交边界，表示可以继续提交的 store 前沿 |
| `rdataPtrExt` | 读取 SQ 数据并向 DataBuffer/Uncache 下发的前沿 |
| `deqPtrExt` | 已完成并可以从 SQ 释放的头指针 |
| `addrReadyPtrExt` | 连续地址已经准备好的前沿 |
| `dataReadyPtrExt` | 连续地址和数据均准备好的前沿 |

相关定义位于 `StoreQueue.scala:39`、`StoreQueue.scala:279`。多组指针使地址就绪、数据就绪、
提交、下发和回收可以分别推进，而不是强制使用一个统一的 FIFO 指针。

### 3.2 Entry 状态分类

每个 SQ entry 维护多类状态信息：

| 状态字段 | 功能含义 |
|---|---|
| `allocated` | entry 已被分配并属于当前 SQ 内容 |
| `addrvalid` | 地址信息已经有效 |
| `datavalid` | store 数据已经写入 |
| `completed` | 下游写入或特殊完成动作已经结束 |
| `committed` | ROB 已提交该 store |
| `pending` | 等待 MMIO/uncache 处理的状态 |
| `nc` | non-cacheable store 标记 |
| `mmio` | memory-mapped I/O store 标记 |
| `memBackTypeMM` | memory-back 类型信息 |
| `unaligned` | 非对齐 store 标记 |
| `cross16Byte` | 非对齐 store 是否跨 16B 边界 |
| `prefetch` | 提交到 SBuffer 时是否携带 prefetch 行为 |
| `isVec` | 是否为向量 store |
| `vecLastFlow` | 是否为该向量 store 的最后 flow |
| `vecMbCommit` | 向量 merge buffer 已经完成提交 |
| `hasException` | store 有异常但仍需按队列规则排空 |
| `waitStoreS2` | 等待 Store S2 阶段补充 MMIO/异常信息 |

这些状态在 `StoreQueue.scala:255` 附近定义，共同决定一个 entry 是否能够 forwarding、commit、
进入 DataBuffer、发送 Uncache 或从队列释放。

## 4. 入队与资源管理

### 4.1 Dispatch 入队

源码通过 `SqEnqIO` 接收多路 enqueue 请求，并通过 `canAccept` 告知上游当前是否有足够空间。
入队能力由 `validCount` 和 `StoreQueueSize - LSQStEnqWidth` 共同决定，保留必要的安全余量，
避免在同一周期出现无法容纳完整入队宽度的情况。

### 4.2 向量 store 的动态 entry 分配

向量 store 的 `numLsElem` 决定需要占用的物理 SQ entry 数量。源码根据每个请求的
`sqIdx`、`numLsElem`、上下界和环回标志，判断每一个 SQ slot 是否属于该请求。

因此具备以下特性：

- 标量 store 通常占用一个 entry。
- 向量 store 可以连续占用多个 entry。
- 多个 enqueue 请求可以在同一周期分配不同的 entry 区间。
- 当分配区间跨越队列末尾时，支持环回匹配。
- 最后一个向量 flow 通过 `vecLastFlow` 记录。

相关逻辑位于 `StoreQueue.scala:363` 至 `StoreQueue.scala:423`。

### 4.3 入队时的状态初始化

新 entry 入队时，源码会清除 `completed`、`datavalid`、`addrvalid`、`committed`、`pending`、
`nc`、`mmio`、`unaligned`、`cross16Byte`、`vecMbCommit`、`hasException` 等当前生命周期控制位。
但这不表示 slot 中所有 payload 和元数据都会在 allocation 时清零：例如 `memBackTypeMM`、地址/data/mask
RAM 内容和部分无 valid 限定的 sideband 可能保留旧值。后续逻辑必须依靠当前 generation、有效位和
合法 owner 事件判断其语义，测试也不能在 valid 条件缺失时检查这些旧 payload。

### 4.4 容量状态与 force write

StoreQueue 对外提供：

- `sqFull`：当前是否不允许继续入队；
- `sqEmpty`：队列是否为空，输出带有一拍延迟；
- `force_write`：根据 entry 数量高低水位产生的滞回式加速写出提示；
- `sqCancelCnt`：redirect 后取消的 entry 数量。

`force_write` 使用 `ForceWriteUpper` 和 `ForceWriteLower` 两个阈值，避免队列占用在阈值附近
频繁抖动，相关逻辑位于 `StoreQueue.scala:1540`。

## 5. 地址、数据和 mask 管理

### 5.1 地址写入

Store 地址通过 `storeAddrIn` 写入 `SQAddrModule`，分别保存虚拟地址和物理地址。地址写回时，
StoreQueue 同时更新：

- `addrvalid`；
- `nc`；
- `unaligned`；
- `cross16Byte`；
- 物理地址和虚拟地址 CAM 内容；
- `wlineflag` 等下游写入属性。

当 PMA/PMP 结果需要延迟一拍返回时，`storeAddrInRe` 会补充 `mmio`、`memBackTypeMM`、
`hasException` 和 `prefetch` 等信息，并清除 `waitStoreS2`。

相关逻辑位于 `StoreQueue.scala:511` 至 `StoreQueue.scala:597`。

### 5.2 数据写入

store 数据通过 `storeDataIn` 写入 `SQDataModule`。数据路径具有以下处理：

- 标量数据根据 byte、halfword、word、doubleword 等访问宽度进行格式化；
- 向量 store 保留向量数据宽度和布局；
- `cbo_zero` 的数据被强制设置为零；
- 数据写入请求和 `datavalid` 设置分成不同周期，形成两级流水；
- 调试路径保存物理地址、虚拟地址、数据和向量非对齐信息。

相关逻辑位于 `StoreQueue.scala:598` 至 `StoreQueue.scala:637`。

### 5.3 地址/数据 ready 前沿

StoreQueue 分别维护地址 ready 和数据 ready 前沿：

- 地址 ready：`mmio`、`addrvalid` 或已提交的向量 merge buffer flow 满足条件；
- 数据 ready：地址有效且数据有效，或者向量 merge buffer flow 已提交，同时排除非对齐 entry；
- ready 前沿每拍只扫描限定数量的 entry；
- redirect 后，ready pointer 根据提交和 dequeue 边界恢复。

对外输出 `stAddrReadySqPtr`、`stAddrReadyVec`、`stDataReadySqPtr` 和 `stDataReadyVec`，
供上游调度或相关控制逻辑使用。相关逻辑位于 `StoreQueue.scala:426` 至 `StoreQueue.scala:496`。

## 6. Store-to-Load forwarding

### 6.1 年龄窗口筛选

Load 发起 forwarding 查询时，StoreQueue 根据 dequeue pointer 和 Load 携带的 SQ pointer flag
划分年龄窗口，只检查比该 Load 更老的 store。对于环形队列，支持同 flag 和跨 flag 两种范围：

- 同 flag：检查 dequeue 到 Load SQ index 之间的区间；
- 跨 flag：检查队列尾部到末端以及队列起始到 Load SQ index 的两个区间。

### 6.2 地址 CAM 匹配与数据返回

VAddr CAM 和 PAddr CAM 同时参与匹配，`SQDataModule` 根据匹配结果产生：

- 快速 forwarding mask；
- 一拍后的完整 forwarding mask；
- 一拍后的 forwarding data。

多个 store 命中时，按 byte 选择有效的年轻 store 数据，因此不同 byte 可以来自不同的 SQ entry。

### 6.3 依赖阻塞与 replay 信息

StoreQueue 为 Load 返回三类重要状态：

- `dataInvalid`：地址匹配但 store data 尚未准备好，或存在不支持直接 forwarding 的非对齐 store；
- `addrInvalid`：更老 store 的地址尚未准备好；
- `matchInvalid`：虚拟地址 CAM 与物理地址 CAM 的命中结果不一致，需要 replay 或进一步处理。

同时返回 `addrInvalidSqIdx` 和 `dataInvalidSqIdx`，指示造成阻塞的 SQ entry。地址阻塞可根据
`loadWaitStrict`、LFST/store-set 信息和 `waitForRobIdx` 选择不同等待策略。

相关逻辑位于 `StoreQueue.scala:649` 至 `StoreQueue.scala:828`。

## 7. ROB 提交与正常 store 下发

### 7.1 ROB 顺序提交

StoreQueue 接收 ROB 提交信息，按 `cmtPtrExt` 顺序推进 `committed` 状态。只有满足队列顺序、
ROB 关系、异常状态和 S2 补充信息要求的 entry 才能提交。

标量 store 和已由 vector merge buffer 提交的向量 store采用不同的提交条件。提交后，store 才能
进入正常 SBuffer、NC 或 MMIO 路径。

### 7.2 Cacheable store 写入 SBuffer

正常 cacheable store 的主要路径为：

```text
SQ entry committed
    -> DataBuffer
    -> SBuffer
    -> completed
    -> deqPtrExt 推进
    -> entry 释放
```

`DatamoduleResultBuffer` 对下游提供数据、地址、mask、`wline`、`prefetch`、`vecValid` 和
`sqNeedDeq` 等信息。只有对应的 SBuffer handshake 完成后，StoreQueue 才会根据
`sqNeedDeq` 更新 `completed` 或推进 rdata/dequeue 相关状态。

源码还刻意延迟 entry 回收，使 SBuffer 写入完成前，StoreQueue 仍能为 Load forwarding 提供
正确的旧 store 信息。相关逻辑位于 `StoreQueue.scala:1186` 和 `StoreQueue.scala:1341`。

## 8. 非对齐、跨 16B 和跨页 store

StoreQueue 能够处理多种非对齐情况：

1. 16B 范围内的非对齐 store：对地址、数据和 mask 进行移位和对齐处理。
2. 跨 16B 边界的 store：拆分成低半部分和高半部分两个 DataBuffer 请求。
3. 跨 4KB 页面：低页请求使用当前物理地址，高页请求通过 `StoreMisalignBuffer` 获取高页物理地址。
4. 拆分 store：通过 `sqNeedDeq` 标识最后一部分，确保同一个逻辑 store 不会在第一部分发送后过早
   释放 SQ entry。
5. 跨页场景：使用 `crossPageWithHit`、`crossPageCanDeq` 和 `doDeq` 协调 StoreQueue 与
   StoreMisalignBuffer。

只有满足 DataBuffer 双 lane 接收条件、跨页地址已经返回且异常状态允许时，非对齐 store 才能继续
下发。相关逻辑位于 `StoreQueue.scala:1198` 至 `StoreQueue.scala:1337`。

## 9. MMIO、NC 与 CMO/CBO 特性

### 9.1 MMIO store 状态机

MMIO store 使用独立状态机：

```text
s_idle -> s_req -> s_resp -> s_wb -> s_wait -> s_idle
```

主要特性：

- MMIO store 必须到达 ROB head 才能发起请求；
- 请求通过 Uncache 接口发送，响应返回后经 `mmioStout` 写回；
- `denied` 映射为 `storeAccessFault`；
- `corrupt` 且未被 `denied` 覆盖时映射为 `hardwareError`；
- MMIO 请求未发出前，相关 store 不能正常提交完成；
- 当前向量 MMIO 写回接口被固定为无效，源码明确标记为未完成。

相关逻辑位于 `StoreQueue.scala:828` 至 `StoreQueue.scala:918`。

### 9.2 NC store 状态机

NC store 使用另一套状态机：

```text
nc_idle -> nc_req -> nc_req_ack -> nc_resp -> nc_idle
```

主要特性：

- 仅允许满足已分配、已提交、地址/数据有效、非向量、非 MMIO 和无异常条件的标量 NC store 发起；
- 通过 `nc_req` 发送 Uncache 请求；
- 通过 `idResp` 确认请求已经被下游接收；
- 支持 outstanding 和 non-outstanding 两种模式；
- non-outstanding 模式等待最终 response 后完成；
- outstanding 模式在下游确认后即可释放相应 NC 生命周期；
- 当前源码明确标记向量 NC 暂不支持。

相关逻辑位于 `StoreQueue.scala:919` 至 `StoreQueue.scala:991`。

### 9.3 CMO 和 CBO.zero

StoreQueue 对 CMO/CBO 进行特殊排序和下发：

- CMO 操作开始前可以要求 SBuffer 清空；
- SBuffer 清空完成后，通过 `cmoOpReq/cmoOpResp` 发起和接收 CMO 操作；
- CMO response 的 `denied` 和 `corrupt` 会转换为 store 异常；
- CMO 写回通过 `mmioStout`，并使用 `flushPipe` 保持流水顺序；
- memory-backed 的 `CBO.zero` 可以通过 SBuffer 的 line write 路径处理；
- I/O-backed 的 `CBO.zero` 通过多个 8-byte Uncache 写请求处理；
- CBO zero 的 SBuffer flush、写回和 SQ 完成由 `cboZeroValid`、`cboZeroWaitFlushSb` 等状态控制。

相关逻辑位于 `StoreQueue.scala:992` 至 `StoreQueue.scala:1132`。

## 10. 异常处理

### 10.1 StoreExceptionBuffer

`StoreExceptionBuffer` 汇聚多个来源的 store 异常：

- Store S1 阶段产生的异常；
- Store S2 阶段补充的 access fault；
- 向量 store feedback 中的 flush/异常信息；
- MMIO 或 Uncache 写回阶段产生的异常；
- SoC 非数据错误。

异常请求通过一拍寄存后参与选择。多个请求同时有效时，使用 `robIdx` 和 `uopIdx` 选择最老异常，
并过滤已经被 exception redirect 取消的请求。

### 10.2 异常 store 的排空策略

有异常的 store 不一定直接从 SQ 删除，而是根据 store 类型执行排空：

- 标量异常 store 可以经过 DataBuffer drain，但不能形成真实 SBuffer 写入；
- 向量异常通过 `vecExceptionFlag` 阻止同一向量指令后续 flow 继续写 SBuffer；
- NC 异常可以直接标记完成，不进入正常 NC 下发路径；
- MMIO/CMO 异常在写回阶段携带异常信息返回 ROB；
- exception 地址和虚拟化上下文由 `exceptionAddr` 输出。

相关逻辑位于 `StoreQueue.scala:68`、`StoreQueue.scala:1363` 和 `StoreQueue.scala:1462`。

## 11. 向量 store 特性

StoreQueue 对向量 store 提供独立的 flow 管理：

- 一条向量 store 可以对应多个 SQ entry；
- 每个 flow 保留相同或相关的 ROB/uop 身份信息；
- `vecLastFlow` 标识该指令的最后 flow；
- `vecMbCommit` 表示 vector merge buffer 已提交该 flow；
- 接收 vector feedback 的 commit/flush 结果；
- 根据 `vecValid` 控制向量异常 flow 是否可以写入 SBuffer；
- 当异常出现在向量指令中间 flow 时，通过 `vecExceptionFlag` 阻止后续 flow 产生实际写入；
- 向量跨页 store 可以通过 StoreMisalignBuffer 补充高页地址。

向量异常标志在同一 ROB 指令的 `vecLastFlow` 经 DataBuffer
`fire && sqNeedDeq` 完成排空时清除；该最后 flow 自身不要求再次携带异常。源码同时通过
`TimeOutAssert` 防止异常状态长期滞留。
相关逻辑位于 `StoreQueue.scala:1363` 至 `StoreQueue.scala:1417`。

## 12. Redirect 与恢复

当发生 branch redirect 或异常 redirect 时，StoreQueue 会根据 ROB index 判断哪些 entry 需要取消：

- 只取消尚未提交的年轻 entry；
- 已提交 entry 不被普通 redirect 清除；
- 被取消 entry 的 `allocated` 和 `completed` 状态失效；
- 统计 redirect 当拍取消的已分配 entry 和 enqueue 请求；
- 两拍后根据取消数量回退 `enqPtrExt`；
- 通过 `sqCancelCnt` 向外提供取消数量。

这种延迟恢复方式将取消统计和指针修复分开，以降低恢复路径的时序压力。相关逻辑位于
`StoreQueue.scala:1493` 至 `StoreQueue.scala:1542`。

## 13. WFI、性能统计与 Difftest

### 13.1 WFI 安全状态

`noPending` 记录是否存在未完成 MMIO/CMO 等下游操作。`wfiSafe` 在没有 pending 操作且 WFI 请求
成立时输出延迟后的安全许可，不能简单等同于“所有 SQ entry 都为空”。

### 13.2 性能统计

源码统计以下状态和事件：

- MMIO 活跃周期和请求数量；
- MMIO 写回成功和写回阻塞；
- SQ 当前有效 entry 数量；
- 已提交和未提交 entry 数量；
- 队列占用低于 1/4、1/2、3/4 以及更高水位的区间；
- 地址匹配失败和 forwarding 相关 replay 情况。

相关逻辑位于 `StoreQueue.scala:1562` 至 `StoreQueue.scala:1580`。

### 13.3 Difftest

当 `EnableDifftest` 有效时，StoreQueue 会输出：

- CMO invalidate 事件；
- DataBuffer 中的普通 store 事件；
- NC/MMIO store 事件；
- 向量非对齐调试信息。

## 14. 当前源码限制与覆盖边界

当前 `StoreQueue.scala` 中明确存在以下限制，不能直接作为已支持正向功能宣称：

1. 向量 MMIO 处理尚未实现，`vecmmioStout.valid` 被固定为 `false`。
2. 向量 NC 路径被源码标记为暂不支持。
3. 非对齐 store-to-load forwarding 尚未完整实现，相关旧 store 可能导致 Load replay。
4. 向量 store 异常处理要求 `EnsbufferWidth == 2`。
5. CBO zero 同时依赖 SBuffer flush、Uncache response 和特殊写回状态，不能按普通 store 路径建模。
6. `sqEmpty`、`force_write`、`sqDeq` 等部分状态带有寄存器延迟，覆盖率采样不能默认与内部同拍事件对齐。

## 15. 功能特性总结

StoreQueue 的功能可以归纳为以下闭环：

```text
dispatch 分配 SQ entry
    -> 地址/数据/mask 分阶段回填
    -> 为年轻 load 提供 forwarding 或阻塞/replay 信息
    -> ROB 顺序提交
    -> 按 cacheable/NC/MMIO/CMO/非对齐/异常类型选择下游
    -> DataBuffer、SBuffer 或 Uncache 完成写入
    -> completed 和 dequeue pointer 推进
    -> 释放 SQ entry
```

其中，`allocated`、`addrvalid`、`datavalid`、`committed`、`completed`、各类 pointer、两套
uncache 状态机、`vecExceptionFlag` 和 redirect cancel 逻辑，是后续功能覆盖率场景建模时最重要的
功能状态依据。具体建模时应从本文确认功能语义，再回到生成 Verilog 中核验真实信号名称、流水延迟
和可观测边界。
