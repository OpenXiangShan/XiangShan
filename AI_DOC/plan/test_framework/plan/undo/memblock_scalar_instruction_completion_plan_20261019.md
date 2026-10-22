# MemBlock 标量指令测试框架完善计划

## 计划信息

- 创建日期：2026-10-19
- 适用目录：`XiangShan-test/mem_ut/ver/ut/memblock`
- 计划状态：执行中
- 总体目标：完善 MemBlock 测试框架中的标量内存指令生成、调度、响应、写回、提交、恢复和回归测试。
- 状态约定：`[ ] 待完成`，`[x] 已完成`。
- 更新规则：每完成一个阶段，更新本文件对应状态，并记录修改文件、验证命令和验证结果。

## 专有名词与抽象功能说明

### 专有名词

- `main table`：测试序列预先生成的主事务表，保存 UID、指令类别、FuType、FuOpType、LSQ flow、地址和 ROB 信息。
- `op class`：主事务的指令类别，例如整数 load、FP load、store、prefetch、AMO 和 CBO。
- `FuType/FuOpType`：DUT MemBlock 使用的功能单元类型和具体操作类型。
- `LQ/SQ`：Load Queue 和 Store Queue，分别承载 load 和 store 类事务的生命周期。
- `MOU`：当前框架中用于标识原子内存操作的 FuType。
- `CBO context`：DCache responder 中从 CBO 请求到 CBOAck 完成期间保存的上下文。
- `issue`：测试框架向 DUT 发出 STA、STD、LOAD 或其他执行请求的阶段。
- `writeback`：DUT 返回访存结果、异常或完成信息后，测试框架更新事务状态的阶段。
- `replay/redirect/flush`：事务发生重试、控制流重定向或清除时的恢复流程。
- `directed testcase`：固定指令类型、地址或 response 条件，用于验证单一功能路径的测试用例。

### 抽象功能说明

- 主表生成函数负责根据配置生成合法的标量内存 transaction，不承担 DUT 结果正确性判断。
- 行为派生函数负责把 FuType/FuOpType 转换成 LQ/SQ allocation、issue route、commit 类型和恢复属性。
- issue 字段分配函数负责根据事务行为生成 DUT issue 接口字段，并拒绝非法字段组合。
- responder 负责根据 DUT 请求产生可收敛的内存、DCache、MMIO 或原子响应。
- monitor/adapter 负责把 DUT 观察到的事件归一化并驱动事务状态转移。
- testcase 和 virtual sequence 负责构造 directed、随机和混合场景，不承担 checker 或 RM 的实现。

## 当前基线

### 已具备基础闭环

- 整数 load：`LB/LH/LW/LD/LBU/LHU/LWU`。
- 整数 store：`SB/SH/SW/SD`。
- 基本流程已经覆盖 admission、issue、writeback、commit/dequeue、异常和 replay。
- prefetch 已有分类、FuOpType 和普通 load issue 路径。
- FP load 已有独立 `op class` 和 `fpWen` 分类。
- MMIO tag、pending 状态和 stale response 处理机制已经存在。
- `SFENCE.VMA`、`HFENCE.VVMA`、`HFENCE.GVMA` 的 CSR runtime snapshot 和失效队列机制已经存在。

### 当前主要缺口

- AMO 配置权重非零时会直接触发 fatal。
- 主表校验和 scalar issue 字段分配会拒绝 AMO/MOU。
- CBO responder 协议模型已有，但主动 CBO 不能从主表进入完整生命周期。
- AMOCAS 的 FuOpType 和多 uop 数量已有部分预留，但尚未形成可执行闭环。
- prefetch、FP load、MMIO 和 Fence 主要缺少真实 DUT 混合场景覆盖。

## P0：完成现状基线整理

状态：[x] 已完成

完成内容：

- 确认当前测试框架的代码入口和主要 transaction flow。
- 区分“显式禁止”“已有基础模型但未闭环”和“已有闭环但覆盖不足”三类问题。
- 确认后续计划优先级为 AMO、AMOCAS、CBO、FP load、prefetch、MMIO、Fence、普通 load/store 边界和回归收敛。

说明：本阶段只完成现状分析，不代表后续代码功能已经实现。

## P1：完善 Scalar AMO 基础路径

状态：[ ] 待完成

### 覆盖指令

- `LR.W/LR.D`
- `SC.W/SC.D`
- `AMOSWAP.W/D`
- `AMOADD.W/D`
- `AMOXOR.W/D`
- `AMOAND.W/D`
- `AMOOR.W/D`
- `AMOMIN.W/D`
- `AMOMAX.W/D`
- `AMOMINU.W/D`
- `AMOMAXU.W/D`

### 实现任务

- 去除 `MEMBLOCK_OP_CLASS_AMO_WT` 非零时的强制 fatal。
- 完善 AMO 主表生成、权重选择和 transaction 字段校验。
- 完善 MOU admission、行为派生和 issue 字段生成。
- 建立 AMO memory RMW response 模型。
- 支持 old value writeback。
- 支持 `SC` 成功和失败返回值。
- 完善 ROB commit、replay、redirect、flush 和异常处理。
- 校验 AMO 事务顺序性、完成条件和资源释放。

### 验证任务

- 每个 AMO opcode 至少增加一个 directed testcase。
- 覆盖 W/D 两种宽度、cache hit/miss、replay 和 redirect。
- 覆盖对齐异常、访问异常和页故障。
- 检查内存新值、目的寄存器旧值、commit 状态和最终回收状态。

### 完成标准

- AMO 权重打开后不再触发配置、主表或 issue 层 fatal。
- 所有基础 AMO 指令均能完成 admission、issue、response、writeback、commit 和 dequeue。
- `LR/SC` 保留集建立、失效和成功/失败结果符合预期。

## P2：完善 AMOCAS 多 uop 路径

状态：[ ] 待完成

### 覆盖指令

- `AMOCAS.W`
- `AMOCAS.D`
- `AMOCAS.Q`

### 实现任务

- 完善 AMOCAS 主表生成和 FuOpType 校验。
- 建模比较值、写入值和返回值。
- 支持 AMOCAS 多 STA/STD uop 生命周期。
- 完善多 uop 的 issue、response、writeback 和完成计数。
- 处理比较成功和比较失败两条路径。
- 处理异常、replay、flush 和 redirect 中断。

### 验证任务

- 覆盖 W/D/Q 三种数据宽度。
- 覆盖比较成功和比较失败。
- 检查比较失败时内存不被修改。
- 检查比较成功时内存、返回值和提交状态正确。

### 完成标准

- 三种 AMOCAS 均能完成完整生命周期。
- 多 uop 不会重复提交、丢失完成事件或残留状态。

## P3：完善 Scalar CBO 主动流程

状态：[ ] 待完成

### 覆盖指令

- `CBO.ZERO`
- `CBO.CLEAN`
- `CBO.FLUSH`
- `CBO.INVAL`

### 实现任务

- 去除 CBO op class 非零时的强制 fatal。
- 完善 CBO admission、SQ/STA/STD 资源管理。
- 增加 CBO 专用 issue 字段检查。
- 建立主表 UID 与 DCache CBO context 的绑定。
- 完善 Probe、CBOAck、denied 和 corrupt 的完成判定。
- 支持 ROB commit、异常、replay、redirect 和 flush。
- 区分 `CBO.ZERO` 与 `CLEAN/FLUSH/INVAL` 的行为差异。

### 验证任务

- 四种 CBO 各增加独立 directed testcase。
- 覆盖 cache hit、cache miss、Probe 和直接 CBOAck。
- 覆盖 denied/corrupt response。
- 检查 CBO 不会被误判为普通 store。
- 检查 CBO context、主表 UID 和最终回收状态一致。

### 完成标准

- 四种 CBO 均能从主表进入主动流程并正常结束。
- CBO 请求、Probe、CBOAck 和 commit 事件能够正确关联。
- 不出现无法收敛的 CBO context 或残留 SQ owner。

## P4：确认并完善 FP Load

状态：[ ] 待完成

### 实现任务

- 确认 `FLW/FLD` 与当前 FP load 类别的 FuOpType 映射。
- 校验 `fpWen`、目标寄存器类型和 writeback 路径。
- 校验 32-bit、64-bit 数据宽度。
- 确认符号扩展、零扩展和必要的高位填充行为。
- 增加 FP load 与整数 load/store 的混合场景。
- 确认 FP store 是否可以直接复用普通 store 路径。

### 完成标准

- `FLW/FLD` 均有真实 DUT testcase。
- 能区分整数 load 和 FP load。
- writeback 目标、数据宽度和 `fpWen` 均检查正确。
- 不会因为复用普通 load FuOpType 产生错误分类。

## P5：完善 Prefetch 真实闭环测试

状态：[ ] 待完成

### 覆盖指令

- `PREFETCH.I`
- `PREFETCH.R`
- `PREFETCH.W`

### 实现和验证任务

- 增加真实 DUT directed testcase。
- 验证 prefetch 不产生错误的架构寄存器写回。
- 覆盖 cache hit、cache miss、TLB miss 和 replay。
- 覆盖 prefetch 与普通 load/store 混合场景。
- 检查 prefetch 的 commit 和资源回收行为。
- 增加不同 prefetch 权重配置的回归测试。

### 完成标准

- 三种 prefetch 均能通过真实 issue 和完成路径。
- 不会被错误当作普通 load 处理。
- 不产生多余的 LQ/SQ 残留或错误 writeback。

## P6：完善 MMIO Load/Store 真实测试

状态：[ ] 待完成

### 实现和验证任务

- 增加真实 DUT MMIO load testcase。
- 增加真实 DUT MMIO store testcase。
- 覆盖 pending MMIO load/store。
- 覆盖 redirect 后旧响应丢弃。
- 覆盖 stale ownership、flush 和 reissue。
- 检查 monitor 生成的 MMIO tag 与当前动态实例一致。
- 增加 MMIO 与普通 cacheable load/store 混合测试。

### 完成标准

- MMIO load/store 均能完成真实 DUT 闭环。
- 旧实例响应不会污染新实例。
- MMIO tag 不会绑定到错误 UID。
- pending、commit 和释放状态最终一致。

## P7：完善 Fence 与 TLB 控制指令组合测试

状态：[ ] 待完成

### 覆盖指令

- `SFENCE.VMA`
- `HFENCE.VVMA`
- `HFENCE.GVMA`

### 实现和验证任务

- 覆盖 Fence 前后的 load/store 行为。
- 覆盖 S1/S2 TLB entry 失效。
- 覆盖 TLB miss、replay 和重新访问。
- 覆盖 Fence 与 MMIO、AMO、CBO 的组合场景。
- 覆盖 Fence 前后 redirect 和 flush。
- 检查 raw fence queue、CSR snapshot 和延迟失效逻辑。

### 完成标准

- 三类 Fence 均有定向测试。
- 失效范围和地址匹配规则正确。
- 不会错误清除无关 TLB entry。
- 旧请求和新请求能够在 Fence 前后正确区分。

## P8：补齐普通 Load/Store 边界覆盖

状态：[ ] 待完成

### 覆盖指令

- `LB/LH/LW/LD`
- `LBU/LHU/LWU`
- `SB/SH/SW/SD`

### 验证任务

- 补充不同地址对齐情况。
- 补充跨 cache line、跨 page 场景。
- 补充 page fault、access fault 和 misaligned exception。
- 补充 cache miss、replay 和 redirect。
- 补充多 load、多 store 和 load/store 交错场景。
- 补充不同数据宽度下的符号扩展和 byte mask 检查。
- 补充普通内存与 MMIO 混合场景。

### 完成标准

- 每种宽度和符号属性都有测试。
- 异常、replay、commit 和 dequeue 状态一致。
- 不出现 LQ/SQ 泄漏、重复完成或错误回收。

## P9：回归和覆盖率协同收敛

状态：[ ] 待完成

### 测试框架工作

- 建立基础 smoke、定向、随机和压力测试分层。
- 为 AMO、AMOCAS、CBO、FP load、prefetch、MMIO 和 Fence 分别建立配置。
- 增加 opcode、数据宽度、异常类型和 response 类型的激励统计。
- 增加 issue、writeback、commit、replay、flush 和 dequeue 状态统计。
- 对所有 fatal gate 做反向检查。
- 整理失败日志分类和回归结果统计。

### 完成标准

- 所有已实现指令类别都能独立回归。
- 关键 opcode 和关键异常路径达到预设覆盖目标。
- 配置打开后不出现与功能无关的框架 fatal。
- 能区分 DUT 问题、测试模型问题和 testcase 配置问题。

## 推荐执行顺序

```text
P0 -> P1 -> P2 -> P3 -> P4/P5/P6/P7 -> P8 -> P9
```

- P1 依赖主表、行为派生和 issue 框架先具备可扩展的 MOU 入口。
- P2 依赖 P1 的原子 response、writeback 和恢复状态模型。
- P3 可复用已有 DCache CBO responder，但必须独立完成主动指令生命周期。
- P4、P5、P6、P7 可以在 P3 完成后并行推进。
- P8 和 P9 在前面功能路径稳定后进行收敛。

## 关联源码入口

- `XiangShan-test/mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_types.sv`
- `XiangShan-test/mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_base_sequence.sv`
- `XiangShan-test/mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_op_behavior_util.sv`
- `XiangShan-test/mem_ut/ver/ut/memblock/seq/base_seq_help/issue_field_assigner.sv`
- `XiangShan-test/mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv`
- `XiangShan-test/mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`
- `XiangShan-test/mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv`
- `XiangShan-test/mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`

## RM 协同支持

本计划不实现 RM、checker 或 scoreboard。

后续协同组件可使用以下字段和事件：

- `uid`、`op_class`、`fuType`、`fuOpType`。
- 访问地址、数据宽度、byte mask 和异常类型。
- AMO old value、new value、`SC` 成功/失败结果。
- CBO opcode、cache line 地址、Probe 和 CBOAck 状态。
- MMIO kind、pending 状态、flush epoch 和 stale response 标记。
- Fence 类型、目标 stage、地址匹配属性和失效结果。

## 功能覆盖率协同支持

本计划不实现 covergroup 或 coveragent。

后续覆盖率组件可使用以下维度：

- 指令类别、FuOpType、数据宽度和地址对齐属性。
- cache hit/miss、TLB hit/miss、Probe、CBOAck 和 MMIO response 类型。
- issue、writeback、commit、replay、redirect、flush 和 dequeue 状态。
- AMO 成功/失败、AMOCAS 比较成功/失败和 `SC` 成功/失败。
- Fence 类型与 load/store、AMO、CBO、MMIO 的交叉场景。

## 变更记录

### 2026-10-19

- 新建本计划文档。
- 完成现状基线整理，P0 标记为已完成。
- 建立 P1-P9 的执行顺序、完成标准和后续状态更新格式。
