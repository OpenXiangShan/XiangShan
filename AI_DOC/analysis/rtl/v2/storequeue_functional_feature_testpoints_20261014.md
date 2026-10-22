# V2 StoreQueue 功能特性测试点追踪矩阵

## 1. 文档定位

本文把 [V2 StoreQueue 功能特性](storequeue_functional_features_20261014.md) 逐项拆解为可执行、
可观察和可判定的测试点，并记录当前版本的静态 review 结论。

本文与旧版
[V2 StoreQueue 测试点与功能覆盖模型](storequeue_testpoints_and_functional_coverage.md)
的区别是：本文以 2026-10-14 当前 HEAD 为基线，吸收当前已经合入的 CMO error、misalign replay、
`rdataPtr` 去重和 VLS exception redirect 修复；旧文档只作为历史专项分析参考。

| 项目 | 内容 |
|---|---|
| RTL 版本 | V2 |
| 分支 | `mem_ut_uvm_v2` |
| 核验 commit | `e3916a0cd92346930e4566373f4db2bb68a060a8` |
| Scala 权威 | `src/main/scala/xiangshan/mem/lsqueue/StoreQueue.scala` |
| 相关源码 | `LSQWrapper.scala`、`StoreUnit.scala`、`StoreQueueData.scala`、`StoreMisalignBuffer.scala`、`DatamoduleResultBuffer.scala`、`MemBlock.scala` |
| Verilog 依据 | `build/rtl/StoreQueue.sv` |
| Verilog SHA-256 | `6f0d67c9422a70d268260b79ce3bc5ff40dc5b0748c7570e1f25d37b60cd1490` |
| 文档日期 | 2026-10-14 |

## 2. 测试分类与观察层级

### 2.1 测试分类

| 分类 | 含义 |
|---|---|
| 正向 | 当前 V2 支持且满足上游协议的功能路径，应计入功能 closure |
| 正向异常 | 合法事务触发异常并按当前设计完成清理、写回或 trap |
| 修复回归 | 已合入修复的历史问题，验证当前行为不会回退 |
| 协议非法 | 电气上可驱动，但 owner、generation、握手、前缀或身份关系非法 |
| 未支持 | 源码明确没有正常成功路径，不建立正向成功 bin |
| 源码观察边界 | 静态源码可以观察，但当前缺少架构签署或完整动态证据，不计正向 closure |

### 2.2 观察层级

| 层级 | 含义 |
|---|---|
| `PIN` | 当前 `StoreQueue.sv` 顶层可直接采样 |
| `BIND` | 需要 bind 或层次引用 StoreQueue 内部状态 |
| `RM` | 由 monitor 保存 transaction token 后推断 |
| `FULL` | 必须在完整 MemBlock/LSQ/ROB/SBuffer/Uncache 环境观察 |
| `SOURCE` | 只在 Scala/source 层可确认，当前顶层端口无法直接观察 |

任何标为 `BIND`、`RM`、`FULL` 或 `SOURCE` 的测试点都不能被描述为 standalone StoreQueue
pin-level 已直接覆盖。

## 3. 统一采样与时序规则

1. Valid-only 输入在 `valid=1` 的时钟沿建立事务；Decoupled 接口只在 `valid && ready` 时建立事务。
2. `storeAddrInRe` 没有独立 owner ID，必须绑定前一拍同 lane、合法且 non-miss 的 S1 token。
3. 地址、数据、mask 和 vector feedback 都必须关联 live `{sqPtr.flag, sqPtr.value}` 或等价 generation；
   slot reuse 后的旧事务属于协议非法。
4. forwarding 的 slow `forwardMask/forwardData/dataInvalid/addrInvalid/matchInvalid` 对应前一拍 query；
   Scala fast 输出不等同于当前 standalone 顶层可见 slow 输出。
5. `stAddrReadyVec/stDataReadyVec` 对应源 bitmap 的 T+1；ready pointer 是当前 pointer 寄存器输出，
   不能无条件按同拍组合比较。
6. `sqDeq`、`sqEmpty`、`force_write` 均对应各自源条件的 T+1；当前 emitted `perf[0..7]`
   对应源事件的 T+2。
7. ExceptionBuffer source 先经过 source register，再进入可保持的 oldest request；`exceptionAddr` 本身没有
   valid，必须依赖已知异常 token 和后端异常生命周期采样。
8. 双 lane 前缀统一写为 `(lane0, lane1)`：合法 ready/valid 组合是 `(0,0)`、`(1,0)`、`(1,1)`；
   `(0,1)` 为协议非法。
9. reset 后只能检查有定义复位值的状态；不得检查无 valid 限定的 uop、RAM payload、
   `exceptionAddr` 或 commit identity sideband。

## 4. 功能特性追踪

| 功能特性 | 对应测试点 |
|---|---|
| 环形队列、entry 生命周期和容量 | `SQ-RST-*`、`SQ-ENQ-*`、`SQ-PTR-*` |
| 地址、数据、mask 和 ready 前沿 | `SQ-WB-*`、`SQ-RDY-*` |
| Store-to-load forwarding | `SQ-FWD-*` |
| ROB commit、SBuffer、完成和 dequeue | `SQ-CMT-*`、`SQ-SB-*`、`SQ-PTR-*` |
| 非对齐、跨 16B、跨页和 replay | `SQ-MA-*` |
| MMIO、NC、CMO/CBO | `SQ-MMIO-*`、`SQ-NC-*`、`SQ-CMO-*` |
| Store exception、向量和 redirect | `SQ-EXC-*`、`SQ-VEC-*`、`SQ-REDIR-*` |
| WFI、状态、性能和 Difftest | `SQ-OBS-*` |
| 当前不支持或只允许异常转换的路径 | `SQ-LIMIT-*` |

## 5. Reset、入队与容量测试点

源码依据：`StoreQueue.scala:255-423,1540-1562`，`LSQWrapper.scala:172-185`。

| ID | 前置条件与激励 | 预期结果 | 观察层级 | 分类 |
|---|---|---|---|---|
| SQ-RST-01 | 复位后不注入任何请求 | entry 有复位定义的控制位为空；六类 pointer 回到初值；MMIO/NC FSM 回 idle；`noPending=1`、`vecExceptionFlag.valid=0`、CBO 控制状态无活动 owner；`sqEmpty` 在 T+1 为 1 | `PIN+BIND` | 正向 |
| SQ-RST-02 | 复位后读取未分配 slot 的 uop、地址、data、mask 和 `exceptionAddr` | 不对这些无 valid 限定 payload 建立值比较；monitor 只确认其未被当作有效事务消费 | `RM+BIND` | 源码观察边界 |
| SQ-ENQ-01 | 单路 scalar store，`numLsElem=1` | 分配一个 live entry，生命周期控制位按新 entry 规则初始化 | `FULL+BIND` | 正向 |
| SQ-ENQ-02 | 同拍多路 scalar/vector 混合入队 | 各请求按实际 flow 数占用互不重叠的连续区间；响应 SQ pointer 与前序分配量一致 | `FULL+BIND` | 正向 |
| SQ-ENQ-03 | vector store 的 `numLsElem>1` | 分配多个 entry，仅物理最后 flow 按 `lastUop` 形成 `vecLastFlow` | `BIND` | 正向 |
| SQ-ENQ-04 | 分配区间跨队列末端 | 低地址段与高地址段均分配，pointer value 环回且 flag 翻转 | `FULL+BIND` | 正向 |
| SQ-ENQ-05 | full LSQ 中构造 `needAlloc` 稀疏组合 | `io.enq.resp` 按需要分配 store 的 lane 及其 flow 数计算，不被不分配 lane 扰动 | `FULL` | 正向 |
| SQ-ENQ-06 | 占用量取 `StoreQueueSize-LSQStEnqWidth` 及其下一值 | 等于阈值时 `allowEnqueue=1/sqFull=0`；严格大于阈值时 `allowEnqueue=0/sqFull=1`。`sqFull` 只通知上游禁止发送，不是 entry 写入的内部硬门控 | `PIN+FULL+SOURCE` | 正向 |
| SQ-ENQ-07 | 占用跨越 force-write 上下阈值后回落 | `force_write` 按一拍延迟和高低水位滞回，不与 `sqFull` 强行等价 | `PIN+BIND` | 正向 |
| SQ-ENQ-08 | 两个请求声明重叠区间、错误 `sqIdx`，或 full 时仍要求接受 | 不计正向覆盖；driver/RM 或 `XSError` 报告 allocation 合同错误 | `FULL+RM` | 协议非法 |
| SQ-ENQ-09 | slot reuse 前后注入上一代迟到地址、数据、mask 或 feedback | 迟到事件不得作为新 generation 的正常回填；monitor 必须用 generation 报错 | `RM` | 协议非法 |
| SQ-ENQ-10 | 复用 slot 前保留旧 `memBackTypeMM` 或 RAM payload | 只有本代合法 S2/写入事件和有效位建立后才能消费；旧 payload 不能决定新事务行为 | `BIND+RM` | 正向 |

## 6. 指针、生命周期和 ready 前沿测试点

源码依据：`StoreQueue.scala:279-360,426-496,1133-1184,1493-1557`。

| ID | 前置条件与激励 | 预期结果 | 观察层级 | 分类 |
|---|---|---|---|---|
| SQ-PTR-01 | 分别驱动 enqueue、commit、read-complete 和 dequeue | `enqPtrExt/cmtPtrExt/rdataPtrExt/deqPtrExt` 只由各自事件推进，允许彼此暂时分离 | `BIND+PIN` | 正向 |
| SQ-PTR-02 | 各 pointer 分别穿越最后 slot 到 slot 0 | value 正确环回，flag 翻转，entry generation 不混淆 | `BIND+PIN` | 正向 |
| SQ-PTR-03 | head 连续有 0、1、2 个 `completed` entry | `deqPtrExt` 只按连续前缀推进 0/1/2；`sqDeq` 下一拍反映数量 | `PIN+BIND` | 正向 |
| SQ-PTR-04 | lane1 entry completed、lane0 未完成 | 不允许越过 lane0 释放 lane1 | `BIND` | 正向 |
| SQ-PTR-05 | 同一完整 `sqPtr` 同拍同时满足 DataBuffer completion 与 NC completion | `readyReadGoVec` 对该 entry 只命中一次，`rdataPtrExt` 只前进 1 | `BIND` | 修复回归 |
| SQ-PTR-06 | 两个不同且连续的 `sqPtr` 同拍完成 | lane0/lane1 前缀合法时 `rdataPtrExt` 允许前进 2 | `BIND` | 正向 |
| SQ-PTR-07 | MMIO writeback、NC completion、普通 DataBuffer completion 分别发生 | `rdataPtrExt` 的推进原因和目标 owner 正确，不与 `deqPtrExt` 混为同一指针 | `BIND+RM` | 正向 |
| SQ-PTR-08 | redirect T0 取消已分配 entry 和当拍 enqueue | T1 保持恢复窗口，T2 输出 `sqCancelCnt` 并回退 `enqPtrExt`；T1 不允许新 enqueue 数推进 tail | `PIN+BIND` | 正向 |
| SQ-RDY-01 | 地址 ready 连续前缀长度取 0 至 4 及大于 4 | `addrReadyPtrExt` 每拍最多前进 4，只跨连续前缀 | `PIN+BIND` | 正向 |
| SQ-RDY-02 | 数据 ready 连续前缀包含普通、MMIO、vector commit、unaligned 和 gap | data frontier 仅按源码公式推进，遇到 unaligned/gap 停止，MMIO 只在 `addrvalid` 成立后替代 `datavalid` | `PIN+BIND` | 正向 |
| SQ-RDY-03 | ready frontier 跨队列末端 | pointer 环回和 Vec bitmap 对应正确 generation | `PIN+BIND` | 正向 |
| SQ-RDY-04 | redirect 与 ready frontier 同拍或相邻拍 | ready pointer 恢复到 commit/dequeue 合法边界，寄存 Vec 输出不与 pointer 错拍比较 | `PIN+BIND` | 正向 |
| SQ-RDY-05 | unaligned entry 位于本拍 dequeue group | data-ready pointer 按 `sqDeqCnt` 和 group 位置执行补跳，不重复或越过未释放 entry | `BIND` | 正向 |

## 7. 地址、数据和 mask 回填测试点

源码依据：`StoreQueue.scala:497-648`，`StoreUnit.scala:418,513-550`。

| ID | 前置条件与激励 | 预期结果 | 观察层级 | 分类 |
|---|---|---|---|---|
| SQ-WB-01 | 普通 S1，`valid && !miss && updateAddrValid` | 写 VAddr/PAddr CAM，更新 `addrvalid/nc/unaligned/cross16Byte` 和 uop | `BIND` | 正向 |
| SQ-WB-02 | 普通 S1，`updateAddrValid=0` | 可以按源码写 CAM/非对齐信息，但不能把 entry 标成 address-ready | `BIND` | 正向 |
| SQ-WB-03 | MAB 回流 S1，交叉 final/non-final、exception 和 `updateAddrValid` | 回流不覆盖普通 CAM/非对齐状态；final 或 exception 可形成合法回填，non-final 且无 exception 保持 `updateAddrValid=0` | `FULL+BIND` | 正向 |
| SQ-WB-04 | S1 `miss=1` | CAM write enable、`addrvalid/nc` 和正常 S2 owner 被抑制；但 fire 仍可更新 uop，普通非 MAB 分支还会采样 `unaligned/cross16Byte`，不得误判为完整 no-op | `BIND` | 正向 |
| SQ-WB-05 | 合法 S1 token 后一拍 S2 `updateAddrValid=1` | 更新 `pending/mmio/memBackTypeMM/hasException/prefetch` 并清 `waitStoreS2` | `BIND+RM` | 正向 |
| SQ-WB-06 | S2 `updateAddrValid=0` 的合法 replay/inert 周期 | 不清 `waitStoreS2`，不更新 pending/MMIO/exception；payload 视为 don't-care | `BIND+RM` | 正向 |
| SQ-WB-07 | misalign buffer backpressure 或 TLB replay 后重发 | replay 周期不产生普通 stout/S2 副作用；重发成功后只执行一次合法 S2 更新 | `FULL+BIND` | 修复回归 |
| SQ-WB-08 | 无前一拍 S1 owner却拉高 S2 update，或 owner generation/identity 错误 | adapter/RM 报错，不计正向回填 | `RM` | 协议非法 |
| SQ-WB-09 | data 与 mask 分别按 data-first、mask-first、same-cycle 到达 | 两者独立写入；`datavalid` 只跟随 data 写入流水，消费前最终 data/mask 必须收敛 | `BIND+RM` | 正向 |
| SQ-WB-10 | SB/SH/SW/SD、vector 和 `cbo_zero` 数据 | 标量按宽度复制格式化，vector 保持原布局，`cbo_zero` 保存全零 | `BIND+FULL` | 正向 |
| SQ-WB-11 | data fire 到下一拍期间目标 slot 保持同一 live owner且未 cancel/reuse | 下一拍 `allocated(lastStWbIndex)` 为 1 时置 `datavalid` | `BIND+RM` | 正向 |
| SQ-WB-12 | 同拍多个 writer 写同一 live entry | 不计正向；driver/assertion 报 writer 冲突 | `RM+BIND` | 协议非法 |
| SQ-WB-13 | data fire 后目标 slot 被 cancel/reuse，再观察延迟 `datavalid` | RTL 没有 generation 比较；该输入序列由 driver/RM 禁止并报错，不能期待 DUT 自动隔离污染 | `RM+BIND` | 协议非法 |

## 8. Store-to-Load Forwarding 测试点

源码依据：`StoreQueue.scala:649-828`，`StoreQueueData.scala:140-268`。

| ID | 前置条件与激励 | 预期结果 | 观察层级 | 分类 |
|---|---|---|---|---|
| SQ-FWD-01 | query 年龄窗口无有效匹配，且无未就绪 blocker | 下一拍无 forwarding byte，三类 invalid 均为 0 | `PIN+RM` | 正向 |
| SQ-FWD-02 | 同 flag 与跨 flag 两类年龄窗口 | 只搜索比 Load 更老的连续 SQ 区间，不把 `sqIdxMask` 当任意稀疏候选集 | `PIN+RM` | 正向 |
| SQ-FWD-03 | 单个 ready store 与 Load 有 1/部分/全部 byte overlap | 下一拍 mask/data 对应重叠 byte | `PIN+RM` | 正向 |
| SQ-FWD-04 | 多个 store 覆盖不同 byte 或同一 byte | 每个 byte 选择正确的年轻匹配 store；不同 byte 可来自不同 entry | `PIN+RM` | 正向 |
| SQ-FWD-05 | 地址命中但 data 未 ready | `dataInvalid=1`；blocker 先选跨环 mask2，所选 mask 段内按 reverse-priority 取物理 index 最大置位项 | `PIN+BIND` | 正向 |
| SQ-FWD-06 | 年龄窗口含 allocated unaligned store | `dataInvalid=1`，不伪造非对齐 forwarding；blocker 选择仍遵守 mask2/reverse-priority 规则 | `PIN+BIND` | 正向 |
| SQ-FWD-07 | non-strict LFST wait 指向地址未就绪 store | `addrInvalid=1`；有 invalid mask 时按 mask2/reverse-priority 返回 blocker，否则回退 query SQ pointer | `PIN+RM` | 正向 |
| SQ-FWD-08 | strict wait，年龄窗口含任意地址未就绪 store | `addrInvalid=1`，返回 query `sqIdx-1`，即使实际未就绪项不是 LFST 指定 store | `PIN+BIND` | 正向 |
| SQ-FWD-09 | VAddr/PAddr CAM 候选不一致 | 下一拍 `matchInvalid=1`，普通 forwarding 结果不作为架构数据使用 | `PIN+BIND` | 正向 |
| SQ-FWD-10 | 同一 port 连续两拍不同 query | T+1/T+2 分别匹配对应 token，不串线 | `PIN+RM` | 正向 |
| SQ-FWD-11 | 三个 forwarding port 同拍输入不同地址、年龄和 blocker | 三路结果独立，不能共享错误 owner 或历史 token | `PIN+RM` | 正向 |
| SQ-FWD-12 | 尝试在 standalone 顶层覆盖 Scala fast 输出 | 当前顶层未暴露的 fast 信号只能用 full-MemBlock proxy/BIND，不能声明 pin-level 覆盖 | `FULL+SOURCE` | 源码观察边界 |

## 9. Commit、SBuffer、完成和 Dequeue 测试点

源码依据：`StoreQueue.scala:1133-1184,1186-1361`，`DatamoduleResultBuffer.scala:48-85`。

| ID | 前置条件与激励 | 预期结果 | 观察层级 | 分类 |
|---|---|---|---|---|
| SQ-CMT-01 | scalar head 满足 pending pointer、S2 完成且未 cancel | `committed=1`，commit pointer 按连续 prefix 推进 | `PIN+BIND` | 正向 |
| SQ-CMT-02 | vector entry 尚无 `vecMbCommit`，随后 feedback 到达 | feedback 前阻塞 commit，匹配 feedback 后允许提交 | `BIND+FULL` | 正向 |
| SQ-CMT-03 | commit prefix 中间存在不满足条件的 entry | 后续 entry 不越过该 gap 提交 | `BIND` | 正向 |
| SQ-CMT-04 | MMIO FSM 处于非 idle/wait 可释放阶段 | 禁止普通 commit 穿过未完成 MMIO；断言不触发 | `BIND` | 正向 |
| SQ-SB-01 | 对齐 cacheable scalar 已提交且 data/address ready | 进入 DataBuffer，再从 SBuffer lane0/1 发出，最终 completed/dequeue | `PIN+BIND+FULL` | 正向 |
| SQ-SB-02 | 双 lane ready/valid 分别取 `(0,0)`、`(1,0)`、`(1,1)` | 只允许前缀组合；lane1 fire 必然伴随 lane0 fire | `PIN+BIND` | 正向 |
| SQ-SB-03 | 驱动 `(lane0,lane1)=(0,1)` | 命中前缀 assertion/driver error，不计正向 | `PIN` | 协议非法 |
| SQ-SB-04 | SBuffer backpressure 多拍 | DataBuffer payload 和 owner 保持稳定，解除 backpressure 后只 fire 一次 | `PIN+RM` | 正向 |
| SQ-SB-05 | scalar cacheable `hasException=1` | 可以通过 `vecValid=0` 的 DataBuffer/SBuffer handshake 排空，但 SBuffer 真实 writeReq 必须为 0 | `FULL+BIND` | 正向异常 |
| SQ-SB-06 | vector 已提交且 mask 全零 | 可以排空，`vecValid=0`，不得形成真实 SBuffer 写 | `FULL+BIND` | 正向 |
| SQ-SB-07 | cross16 low/high fragment | low `sqNeedDeq=0` 不完成原 entry；high `sqNeedDeq=1` 后才完成 | `BIND+FULL` | 正向 |
| SQ-SB-08 | 普通 SBuffer `fire && sqNeedDeq && !wline` | 只置 payload `sqPtr` 对应 entry 的 `completed` | `BIND+RM` | 正向 |
| SQ-SB-09 | NC ack/response completion | 只按当前模式选择的 NC owner 置 `completed` | `BIND+RM` | 正向 |
| SQ-SB-10 | `mmioStout.fire` | MMIO owner completed，rdata 与物理 dequeue 后续独立推进 | `BIND+RM` | 正向 |
| SQ-SB-11 | `cboZeroStout.fire` | 保存的 `cboZeroSqIdx` completed，清 `cboZeroValid` | `BIND+RM` | 正向 |
| SQ-SB-12 | scalar/vector exception no-write drain | 释放目标 entry，但不形成真实 SBuffer write | `FULL+BIND` | 正向异常 |
| SQ-SB-13 | commit 时 `nc && hasException` | entry direct completed，不产生 NC request | `BIND+PIN` | 正向异常 |
| SQ-SB-14 | S2 采样 DCache miss 并随后正常下发 | `prefetch` 从 entry 传播到 DataBuffer/SBuffer payload，仅在合法 owner/valid 下检查 | `BIND+FULL` | 正向 |
| SQ-SB-15 | 普通 `wlineflag/rlineflag` 与 CBO.zero `wline` | 普通 line flag 按地址模块传播；`wline=1` 的 CBO.zero 不使用普通 completed 条件 | `BIND+FULL` | 正向 |

## 10. 非对齐、跨页和 Replay 测试点

源码依据：`StoreQueue.scala:1198-1337`，`StoreUnit.scala:498-550`，`StoreMisalignBuffer.scala`。

| ID | 前置条件与激励 | 预期结果 | 观察层级 | 分类 |
|---|---|---|---|---|
| SQ-MA-01 | `unaligned=1 && cross16Byte=0` | 生成一个 16B 对齐 DataBuffer record，数据按地址低位移位，`sqNeedDeq=1` | `BIND+FULL` | 正向 |
| SQ-MA-02 | `unaligned=1 && cross16Byte=1` 且双 lane 可接收 | 形成 low/high 两个 record，mask/data 拆分正确 | `BIND+FULL` | 正向 |
| SQ-MA-03 | cross16 但只有 lane0 可接收 | 不允许只提交半个逻辑 store；等待双 lane 条件 | `BIND` | 正向 |
| SQ-MA-04 | 跨 4KB parent 命中，`crossPageCanDeq=0` | 正常 pair 保持等待，不消费未就绪高页 PAddr，`doDeq=0` | `PIN+BIND+FULL` | 正向 |
| SQ-MA-05 | 跨页 parent 命中且 `crossPageCanDeq=1` | high lane 使用 MAB 提供的完整 PAddr，双 lane fire 后 `doDeq=1` | `PIN+BIND+FULL` | 正向 |
| SQ-MA-06 | MAB ready 但当前 SQ head 不是对应 parent | 不得错误使用 MAB 高页地址或释放 MAB | `PIN+RM` | 正向 |
| SQ-MA-07 | misalign request 因 MAB not-ready/TLB replay | 本次 `updateAddrValid=0`，SQ 不产生 S2 副作用；重发后只更新一次 | `FULL+BIND` | 修复回归 |
| SQ-MA-08 | 同一 split entry 同拍出现两个 completion 原因 | 完整 `sqPtr` 匹配保证 `rdataPtr` 只推进一次 | `BIND` | 修复回归 |
| SQ-MA-09 | scalar MAB child 实际为 MMIO 或 NC | 不进入普通成功 MMIO/NC sink；按 StoreUnit/MAB 当前异常路径收敛 | `FULL` | 源码观察边界 |
| SQ-MA-10 | MAB cancel/revoke 发生于 req/resp/wb/block | 下一周期回收内部 owner，不遗留 stale `crossPageCanDeq/doDeq` 或终点 writeback | `FULL+BIND` | 正向 |

## 11. MMIO 测试点

源码依据：`StoreQueue.scala:828-918,1070-1088`。

| ID | 前置条件与激励 | 预期结果 | 观察层级 | 分类 |
|---|---|---|---|---|
| SQ-MMIO-01 | scalar MMIO 到 ROB head，地址/数据 ready、无异常 | `idle -> req -> resp -> wb -> wait -> idle`，成功 writeback 后等待 `scommit` | `PIN+BIND+FULL` | 正向 |
| SQ-MMIO-02 | request 或 writeback backpressure | FSM 保持当前 owner/payload，不重复 request/writeback | `PIN+RM` | 正向 |
| SQ-MMIO-03 | response `denied=1` | `uncacheUop` 置 SAF，writeback 后 `s_wb -> s_idle`，不等待 `scommit` | `PIN+BIND+FULL` | 正向异常 |
| SQ-MMIO-04 | response `corrupt=1 && denied=0` | `uncacheUop` 置 HWE，writeback 后直接回 idle | `PIN+BIND+FULL` | 正向异常 |
| SQ-MMIO-05 | response `denied=1 && corrupt=1` | denied 优先，只按 SAF 终点检查 | `PIN+BIND+FULL` | 正向异常 |
| SQ-MMIO-06 | FSM 已进入 `s_req` 但 request 尚未 fire 时拉高 `wfiReq` | `mmioReq.valid` 被抑制且 FSM 保持 `s_req`；请求已 fire、进入 response/wb 的合法事务继续排空 | `PIN+BIND` | 正向 |
| SQ-MMIO-07 | vector 地址实际分类为 MMIO，随后 matching vector feedback 到达 | StoreUnit 转成 SAF，`vecmmioStout.valid=0`；matching feedback 后经 `vecValid=0` 的 DataBuffer/SBuffer no-write drain 清理 | `FULL+PIN+BIND` | 正向异常 |

## 12. NC 测试点

源码依据：`StoreQueue.scala:919-990`，Uncache/MemBlock error 路径。

| ID | 前置条件与激励 | 预期结果 | 观察层级 | 分类 |
|---|---|---|---|---|
| SQ-NC-01 | 标量 NC 已提交、allvalid、无异常，non-outstanding | `idle -> req -> req_ack -> resp -> idle`，最终 response 使用锁存 owner 完成 | `PIN+BIND+FULL` | 正向 |
| SQ-NC-02 | request ready 拉低多拍 | `ncReq` owner/id/payload 保持，request 只 fire 一次 | `PIN+RM` | 正向 |
| SQ-NC-03 | `uncacheOutstanding=1` | 只有 `idResp.valid && idResp.nc` 触发 completion，owner 取 `mid`，FSM 回 idle | `PIN+BIND+RM` | 正向 |
| SQ-NC-04 | outstanding 连续发出至少两笔 NC | RM 维护多 owner/mid/generation，ack 不能串 owner | `PIN+RM` | 正向 |
| SQ-NC-05 | outstanding ack 后旧 final response 迟到 | 不得再次完成新 owner；由 RM 根据旧 token 消化，错误响应另走 BEU | `FULL+RM` | 正向 |
| SQ-NC-06 | `idResp.valid && !idResp.nc` | 不推进 NC FSM或 NC completion；该 ack 可属于 MMIO/CBO.zero | `PIN+BIND` | 正向 |
| SQ-NC-07 | final response 携带 clean/denied/corrupt/both，交叉 outstanding 模式和 `cache_error_enable` | SQ 不形成 ROB store exception；Uncache error 经 MemBlock T+2 BEU 路径，并受 gate 控制 | `FULL+RM` | 源码观察边界 |
| SQ-NC-08 | 错 mid、无 owner、旧 generation 或模式中途切换 | RM 报 response 合同错误；不能把 RTL 静默 slot 更新当成功 | `RM+BIND` | 协议非法 |
| SQ-NC-09 | `nc && hasException` 在 commit 时成立 | direct completed，不发普通 NC request | `PIN+BIND` | 正向异常 |
| SQ-NC-10 | vector 地址实际分类为 NC，随后 matching feedback/commit | NC FSM 因 `!isVec` 不启动；commit 时 `nc && hasException` direct-complete，不进入 DataBuffer/SBuffer，也不要求 `vecExceptionFlag` | `FULL+BIND` | 正向异常 |

## 13. CMO 与 CBO.zero 测试点

源码依据：`StoreQueue.scala:992-1132`，当前修复 commit `7aa145db8f`。

| ID | 前置条件与激励 | 预期结果 | 观察层级 | 分类 |
|---|---|---|---|---|
| SQ-CMO-01 | non-zero CBO 到达 head，SBuffer 非空后变空 | 先请求 flush，`empty` 后才允许 CMO request | `PIN+BIND+FULL` | 正向 |
| SQ-CMO-02 | CMO response 正常 | `cmoOpResp.fire -> s_wb -> mmioStout.fire -> s_wait -> scommit -> idle` | `PIN+BIND+FULL` | 正向 |
| SQ-CMO-03 | CMO response denied | SAF 写入 `uncacheUop`，writeback 后直接回 idle | `PIN+BIND+FULL` | 正向异常 |
| SQ-CMO-04 | CMO response corrupt 且未 denied | HWE 写入 `uncacheUop`，writeback 后直接回 idle | `PIN+BIND+FULL` | 正向异常 |
| SQ-CMO-05 | CMO response denied 与 corrupt 同时为 1 | denied 优先，只检查 SAF | `PIN+BIND+FULL` | 正向异常 |
| SQ-CMO-06 | 前一 CMO error 后执行无错 MMIO/CMO | 新 owner 的 exceptionVec 被重新初始化，不得泄漏旧 CMO error | `BIND+FULL` | 修复回归 |
| SQ-CMO-07 | main-memory-backed `CBO.zero` | 经 SBuffer line write，等待 SBuffer flush 后由 `cboZeroStout` 完成 | `PIN+BIND+FULL` | 正向 |
| SQ-CMO-08 | actual NC 或 actual MMIO 的 `CBO.zero`，每个 beat response 正常 | 两类都复用 SQ MMIO FSM；覆盖 first/middle/last beat，普通 NC FSM 不启动，最后进入 writeback | `PIN+BIND+RM` | 正向 |
| SQ-CMO-09 | actual NC/MMIO `CBO.zero` 的 first/middle/last beat 分别返回 denied、corrupt、both | 任一错误提前进入 `s_wb`；denied 优先 SAF，仅 corrupt 为 HWE | `PIN+BIND+RM` | 正向异常 |
| SQ-CMO-10 | I/O-backed `CBO.zero` 全部正常完成后观察 `noPending/wfiSafe` | 当前源码不会在正常 CBO.zero response 分支恢复 `noPending`；即使 FSM 经 writeback/scommit 回 idle，`wfiSafe` 仍可保持阻塞，记录为当前静态行为 | `BIND+FULL` | 源码观察边界 |
| SQ-CMO-11 | actual NC/MMIO `CBO.zero` 在 first/middle/last beat 发生 denied、corrupt 或 both，随后请求 WFI | error beat 提前进入 `s_wb`，但同样不会恢复 `noPending`；覆盖 `actual_{NC,MMIO} x error_beat x error_kind x noPending=0 x wfiSafe_blocked` | `BIND+FULL` | 源码观察边界 |

## 14. 异常、向量和 Redirect 测试点

源码依据：`StoreQueue.scala:68-145,230-246,1363-1417,1471-1533`，
`LSQWrapper.scala:187-190`，`MemBlock.scala:401-410`。

| ID | 前置条件与激励 | 预期结果 | 观察层级 | 分类 |
|---|---|---|---|---|
| SQ-EXC-01 | 两路 S1、两路 S2、两路 vector FLUSH 和一条 special writeback，共七个 source port 分别单发合法 StaCfg exception | 每个端口只有在 selected StaCfg exception 非零时才成为 candidate；正常 MMIO/CMO success writeback 不计异常 source | `BIND+FULL` | 正向异常 |
| SQ-EXC-02 | 多 source 同拍、相邻拍、held request 与新请求竞争 | 按 `{robIdx,uopIdx}` 选最老；完全同身份按静态 source 顺序确定 | `BIND+RM` | 正向异常 |
| SQ-EXC-03 | S1 非 vector `StaCfg` exception，分别取 `updateAddrValid=0/1` | 两种都可形成 S1 exception candidate；`updateAddrValid=0` 不把 entry 标成普通 address-ready | `BIND+FULL` | 正向异常 |
| SQ-EXC-04 | 合法 S1 owner 后 S2 顶层可见非 SAF 位 `{3,6,15,19,23}` 任一非零，或 `af=1` | S2 source 正确；SAF 由独立 `af` 写入，当前顶层不存在 Re `exceptionVec[7]` | `BIND+FULL` | 正向异常 |
| SQ-EXC-05 | S1 trigger 为 DebugMode，S2 `hasException=1` 但 exceptionVec/af 为 0 | SQ no-write drain，ExceptionBuffer 不强制更新；最终在 ROB/CSR 检查 debug trap | `FULL` | 正向异常 |
| SQ-EXC-06 | scalar cacheable exception 已提交 | entry 能排空且无真实 memory write，最终资源释放 | `FULL+BIND` | 正向异常 |
| SQ-VEC-01 | vector COMMIT feedback 的 `{robIdx,uopIdx}` 匹配 live entry | 对应 `vecMbCommit` 置位，不形成 ExceptionBuffer source | `BIND+FULL` | 正向 |
| SQ-VEC-02 | vector FLUSH feedback 携带合法 StaCfg exception | matching entry 可置 `vecMbCommit`，feedback payload 独立进入 ExceptionBuffer source | `BIND+FULL` | 正向异常 |
| SQ-VEC-03 | feedback ROB/uop 不匹配 live entry | 不置 `vecMbCommit`；但 FLUSH payload 若自身异常有效仍可能进入 ExceptionBuffer，不能把两种 receiver 合并判定 | `RM+BIND` | 协议非法 |
| SQ-VEC-04 | feedback `sqIdx`/generation 错误但 ROB/uop 碰撞当前 live entry | RTL 不校验 `sqIdx` generation；RM 必须报协议错误，不能期待硬件自动隔离 | `RM+BIND` | 协议非法 |
| SQ-VEC-05 | 非最后 vector exception flow 排空，随后同 ROB `vecLastFlow` 排空 | 前者设置 `vecExceptionFlag`，后者以 `fire && sqNeedDeq` 清除，不要求最后 flow 再带异常 | `BIND+FULL` | 正向异常 |
| SQ-VEC-06 | 两 lane 同拍属于同 ROB/不同 ROB，以及只有 lane0 fire | `vecCommitLastFlow` 判定与 lane/ROB/last-flow 组合一致 | `BIND` | 正向 |
| SQ-REDIR-01 | 普通 redirect 命中未提交年轻 entry | 取消 entry，已提交 entry 保留，T2 恢复 tail | `PIN+BIND` | 正向 |
| SQ-REDIR-02 | `vecExceptionFlag` 有效时发生 redirect | cancel 使用严格年龄边界，保留 faulting vector 指令所需排空 flow | `BIND+FULL` | 正向 |
| SQ-REDIR-03 | VLS exception redirect | SQ/MAB 接收 `flushAfterRedirect`，ExceptionBuffer 接收原始 `brqRedirect/flush` | `FULL+BIND` | 正向 |
| SQ-REDIR-04 | VLS exception 下同 ROB 的旧 ExceptionBuffer request 与剩余 SQ flow 并存 | ExceptionBuffer 清除旧地址记录，同时 SQ 保留必须排空的 faulting flow | `FULL+BIND` | 修复回归 |
| SQ-REDIR-05 | redirect 后 adapter 继续注入旧 S2/data/mask/vector token | 这些 receiver 不具备统一 generation 保护；RM 必须报协议非法并禁止其进入正常 coverage | `RM+BIND` | 协议非法 |
| SQ-REDIR-06 | redirect 时已有 MMIO/CMO/NC request fire | 已建立的外部事务按各自 FSM/owner 规则排空，不重新归属新 SQ entry | `FULL+RM` | 正向 |
| SQ-REDIR-07 | redirect 与 MAB req/resp/wb/block 交错 | 按 MAB 自身 redirect/revoke 规则回收，不能套用 StoreQueue generation 结论 | `FULL+BIND` | 正向 |

## 15. WFI、状态、性能和 Difftest 测试点

源码依据：`StoreQueue.scala:1045-1068,1419-1460,1540-1580` 和当前 emitted RTL。

| ID | 前置条件与激励 | 预期结果 | 观察层级 | 分类 |
|---|---|---|---|---|
| SQ-OBS-01 | `wfiReq=1`，分别处于无 pending、MMIO/CMO pending 和 NC outstanding | `wfiSafe` 只表示 `noPending` 的 T+1 许可；NC 生命周期不修改 `noPending`，因此 NC outstanding 时仍可能置位，不能解释为 LSQ 全空 | `PIN+BIND` | 正向 |
| SQ-OBS-02 | `sqDeqCnt`、empty 条件和 occupancy 在相邻周期变化 | `sqDeq/sqEmpty/force_write` 均按源条件 T+1 采样，不用同拍 head sideband错误分类 | `PIN+BIND` | 正向 |
| SQ-OBS-03 | occupancy 落在各正常 bucket | `perf` bucket 与源码比较式一致，输出在源条件 T+2 | `PIN+BIND` | 正向 |
| SQ-OBS-04 | StoreQueueSize=56 且 occupancy=14 | 四个 occupancy bucket 均不命中，记录当前比较式空洞 | `PIN+BIND` | 源码观察边界 |
| SQ-OBS-05 | 触发 MMIO active、request、WB success 和 WB blocked | 当前 emitted `perf[0..3]` 在源事件 T+2 对应 | `PIN+RM` | 正向 |
| SQ-OBS-06 | 使用 `enablePerfPrint` 的 perf-debug RTL，改变 valid/committed/non-committed entry 数并触发 vaddr match fail | `validEntryCnt/cmtEntryCnt/nCmtEntryCnt` 和 forwarding/vaddr-match 统计与内部源事件一致；当前标准 `StoreQueue.sv` 未生成这些计数器，不计入当前 RTL closure | `BIND+SOURCE` | 源码观察边界 |
| SQ-OBS-07 | 使用 `EnableDifftest=1` 的 full-top 构建，普通/向量 store 进入 DataBuffer | Difftest `pmaStore` 以及 `diffInfo.uop/start/offset` 与 DataBuffer owner 一致；当前 standalone `StoreQueue.sv` 不直接暴露这些信号 | `FULL+SOURCE` | 源码观察边界 |
| SQ-OBS-08 | 使用 `EnableDifftest=1` 的 full-top 构建，NC/MMIO main-memory-backed request、CBO.inval 完成 | `ncStore` 和 CMO inval Difftest 事件只在各自合法 fire/写回事件产生；不计入当前 standalone RTL closure | `FULL+SOURCE` | 源码观察边界 |

## 16. 未支持路径与排除项

| ID | 场景 | 处理要求 | 观察层级 | 分类 |
|---|---|---|---|---|
| SQ-LIMIT-01 | 正常 vector MMIO 成功下发 | `vecmmioStout.valid` 固定为 0；正常成功 sink exclude，实际地址分类只覆盖异常转换路径 | `PIN+FULL` | 未支持 |
| SQ-LIMIT-02 | 正常 vector NC 成功下发 | NC FSM 要求 `!isVec`；正常成功 sink exclude，实际地址分类覆盖异常/direct-complete | `BIND+FULL` | 未支持 |
| SQ-LIMIT-03 | 非对齐 store-to-load 直接 forwarding | 当前以 replay/dataInvalid 保证正确性，不建立成功 forwarding bin | `PIN+BIND` | 未支持 |
| SQ-LIMIT-04 | `EnsbufferWidth != 2` 的 vector exception 处理 | 当前源码 `require(EnsbufferWidth == 2)`，不作为参数化支持范围 | `SOURCE` | 未支持 |
| SQ-LIMIT-05 | 无 owner/generation 的迟到 response 或 writeback | 一律归协议非法，不得通过放宽 monitor 条件填充正向 bin | `RM` | 协议非法 |

## 17. 覆盖完整性检查表

以下项目全部具有至少一个正向或明确排除的测试点：

- reset、entry allocation、slot reuse 和 generation；
- scalar/vector/multi-lane/wrap enqueue；
- enq/commit/read/dequeue/ready 六类 pointer；
- 地址 S1、S2 replenish、replay、data 和 mask；
- ready frontier、forwarding、LFST/strict blocker；
- ROB commit、DataBuffer、SBuffer、completion 和 physical dequeue；
- within-16B、cross-16B、cross-page MAB；
- MMIO、NC outstanding/non-outstanding、CMO、CBO.zero；
- 七类 exception source、向量反馈、vector exception flag；
- 普通 redirect、VLS exception 双 redirect 和 T0/T1/T2 pointer 恢复；
- WFI、状态延迟、perf、Difftest；
- 当前未支持 vector MMIO/NC 和非对齐直接 forwarding。

## 18. Review 记录

| 日期 | 轮次 | Reviewer | 结论 |
|---|---:|---|---|
| 2026-10-14 | 预审 | subagent A/B | 发现旧测试点基线过期；要求按当前 HEAD 重写 CMO error、NC cross16 修复回归、VLS 双 redirect、misalign replay、双 lane 位序、观察层级和时序规则。 |
| 2026-10-14 | 1 | subagent A/B | 不通过。发现容量边界、data/feedback generation、redirect receiver、CBO.zero `noPending` 四项阻塞问题，并要求补 NC outstanding、vector 终点、七端口资格、prefetch/wline、perf 和 Difftest。本文已按意见修订。 |
| 2026-10-14 | 2 | subagent A/B | 不通过。剩余问题为 CBO.zero error beat 的 `noPending/wfiSafe`、fast forwarding 分类名称、perf-debug 构建条件和 Difftest 构建条件。本文已按意见修订。 |
| 2026-10-14 | 3 | subagent A/B | 通过。两位 reviewer 独立确认第一、二轮阻断项均已关闭；当前 HEAD 静态 Scala 与当前生成 `StoreQueue.sv` 范围内，无已知测试点遗漏，测试场景描述无已知事实错误。动态仿真不在本轮范围。 |

## 19. 当前结论边界

本文的“完整”指当前静态源码和当前生成 RTL 范围内，所有已识别 StoreQueue 功能特性均已映射到
正向、异常、修复回归、协议非法、未支持或源码观察边界测试点。本文已经过三轮正式 review，
其中前两轮发现并修订问题，第三轮由两位 reviewer 独立通过，因此当前静态范围内可以作为测试点
closure 基线。该结论不替代 VCS 仿真、coverage 实现和 FSDB 波形验证；RTL 重新生成、commit 变化或
参数配置变化后必须重新 review。
