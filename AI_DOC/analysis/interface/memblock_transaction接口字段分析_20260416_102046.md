# MemBlock transaction 接口字段分析

## 文档说明
本文档为当前唯一的 `MemBlock` 接口分析主文档。

已合并原先单独整理的：
- `fuType / fuOpType / uopIdx` 编码与合法值分析
- 各 transaction 的方向、来源和字段语义分析

后续统一维护本文件，不再拆成多份并行文档。

## 文档目的
本文档用于汇总 `mem_ut/ver/ut/memblock/agent/*_xaction.sv` 中各个 transaction 的字段含义、合法值来源、以及与 DUT 端口的映射关系。

分析原则：
- 合法值优先以 Scala 设计源码为真值来源
- 接口方向优先以 `tb/*_connect.sv` 中 `force` 方向为准
- 不能从 Scala 直接给出枚举真值的字段，按“位宽/握手/索引/状态”语义约束
- 对 DUT 输出类 transaction，不应在 UVM 中主观施加“输入合法值约束”；它们的合法性由 DUT 子模块决定，UVM 主要负责监控和必要的 ready/backpressure

关键真值来源：
- `src/main/scala/xiangshan/mem/MemBlock.scala`
- `src/main/scala/xiangshan/backend/Backend.scala`
- `src/main/scala/xiangshan/backend/Bundles.scala`
- `src/main/scala/xiangshan/backend/fu/FuType.scala`
- `src/main/scala/xiangshan/package.scala`
- `src/main/scala/xiangshan/mem/lsqueue/LSQWrapper.scala`
- `src/main/scala/xiangshan/Parameters.scala`
- `mem_ut/ver/ut/memblock/tb/*_connect.sv`

---

## 表格化速查

### 1. fuType 速查表

| 语义 | Scala 常量 | 实际编码 |
|---|---|---|
| 标量 load | `FuType.ldu` | `36'h0001_0000` |
| 标量 store | `FuType.stu` | `36'h0002_0000` |
| 标量 AMO | `FuType.mou` | `36'h0004_0000` |
| 向量 load | `FuType.vldu` | `36'h1000_00000` |
| 向量 store | `FuType.vstu` | `36'h2000_00000` |
| 向量 segment load | `FuType.vsegldu` | `36'h4000_00000` |
| 向量 segment store | `FuType.vsegstu` | `36'h8000_00000` |

### 2. 标量 LSU fuOpType 速查表

| 路径 | Scala 常量 | 实际编码 |
|---|---|---|
| load | `LSUOpType.lb` | `0` |
| load | `LSUOpType.lh` | `1` |
| load | `LSUOpType.lw` | `2` |
| load | `LSUOpType.ld` | `3` |
| load | `LSUOpType.lbu` | `4` |
| load | `LSUOpType.lhu` | `5` |
| load | `LSUOpType.lwu` | `6` |
| store | `LSUOpType.sb` | `0` |
| store | `LSUOpType.sh` | `1` |
| store | `LSUOpType.sw` | `2` |
| store | `LSUOpType.sd` | `3` |

### 3. 原子操作 fuOpType 速查表

| Scala 常量 | 实际编码 | Scala 常量 | 实际编码 |
|---|---:|---|---:|
| `LSUOpType.lr_w` | 2 | `LSUOpType.lr_d` | 3 |
| `LSUOpType.sc_w` | 6 | `LSUOpType.sc_d` | 7 |
| `LSUOpType.amoswap_w` | 10 | `LSUOpType.amoswap_d` | 11 |
| `LSUOpType.amoadd_w` | 14 | `LSUOpType.amoadd_d` | 15 |
| `LSUOpType.amoxor_w` | 18 | `LSUOpType.amoxor_d` | 19 |
| `LSUOpType.amoand_w` | 22 | `LSUOpType.amoand_d` | 23 |
| `LSUOpType.amoor_w` | 26 | `LSUOpType.amoor_d` | 27 |
| `LSUOpType.amomin_w` | 30 | `LSUOpType.amomin_d` | 31 |
| `LSUOpType.amomax_w` | 34 | `LSUOpType.amomax_d` | 35 |
| `LSUOpType.amominu_w` | 38 | `LSUOpType.amominu_d` | 39 |
| `LSUOpType.amomaxu_w` | 42 | `LSUOpType.amomaxu_d` | 43 |
| `LSUOpType.amocas_q` | 44 | `LSUOpType.amocas_d` | 47 |
| `LSUOpType.amocas_w` | 46 |  |  |

### 4. uopIdx / 索引容量速查表

| 项 | 当前值/范围 | 说明 |
|---|---|---|
| `MaxUopSize` | `65` | 来自 `Parameters.scala` |
| `uopIdx` 位宽 | `7 bit` | `log2Up(MaxUopSize + 1)` |
| `uopIdx` 语义范围 | `0..64` | 标量默认固定 `0` |
| `RobSize` | `352` | `robIdx_value` 约束 `0..351` |
| `VirtualLoadQueueSize` | `72` | `lqIdx_value` 约束 `0..71` |
| `StoreQueueSize` | `56` | `sqIdx_value` 约束 `0..55` |

---

## 总体分类

### A. 后端/控制侧输入 MemBlock 的 transaction
这些 transaction 由 UVM driver 主动驱动到 DUT：

- `backendToTopBypass_agent_agent_xaction`
- `csr_ctrl_agent_agent_xaction`
- `fence_agent_agent_xaction`
- `lsqcommit_agent_agent_xaction`
- `lsqenq_agent_agent_xaction`
- `lintsissue_agent_agent_xaction`
- `vecissue_agent_agent_xaction`
- `redirect_agent_agent_xaction`
- `other_ctrl_agent_agent_xaction`
- `int_sink_agent_agent_xaction`
- `itlb_agent_agent_xaction`
- `L2tlb_agent_agent_xaction`

### B. 环境/存储系统响应 MemBlock 的 transaction
这些 transaction 代表 MemBlock 对 cache/TLB/buffer 外围接口的交互，通常是：
- 一部分字段由 DUT 输出、UVM monitor 观察
- 少数 `ready` / response 字段由 UVM driver 反馈给 DUT

- `dcache_agent_agent_xaction`
- `sbuffer_agent_agent_xaction`
- `prefetch_agent_agent_xaction`
- `io_mem_to_ooo_int_wb_agent_agent_xaction`
- `io_mem_to_ooo_vec_wb_agent_agent_xaction`

### C. DUT 输出到后端/前端的 transaction
这些 transaction 主要用于监控 DUT 输出，不应把它们当成“任意可驱动输入”：

- `io_mem_to_ooo_ctrl_agent_agent_xaction`
- `io_mem_to_ooo_wakeup_agent_agent_xaction`
- `io_mem_to_ooo_iq_feedback_agent_agent_xaction`

---

## 索引容量约束总表

以下范围约束应作为所有后续 UVM sequence / transaction 的统一索引真值，而不是只按位宽放开。

### ROB
来源：
- [Parameters.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/Parameters.scala)

当前配置：
- `RobSize = 352`

接口编码形态：
- `robIdx_flag`
- `robIdx_value[8:0]`

建议约束：
- `robIdx_flag inside {0,1}`
- `robIdx_value inside {[0:351]}`

说明：
- 9bit 位宽能表示 `0..511`
- 但当前 ROB 实际容量只有 `352`
- 因此不能把 `robIdx_value` 放宽到整个 9bit 空间

### LQ
来源：
- [Parameters.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/Parameters.scala)

当前配置：
- `VirtualLoadQueueSize = 72`

接口编码形态：
- `lqIdx_flag`
- `lqIdx_value[6:0]`

建议约束：
- `lqIdx_flag inside {0,1}`
- `lqIdx_value inside {[0:71]}`

说明：
- 7bit 位宽能表示 `0..127`
- 但当前有效 LQ 槽位只有 `72`

### SQ
来源：
- [Parameters.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/Parameters.scala)

当前配置：
- `StoreQueueSize = 56`

接口编码形态：
- `sqIdx_flag`
- `sqIdx_value[5:0]`

建议约束：
- `sqIdx_flag inside {0,1}`
- `sqIdx_value inside {[0:55]}`

说明：
- 6bit 位宽能表示 `0..63`
- 但当前有效 SQ 槽位只有 `56`

### 使用原则
- 位宽只表示“可编码范围”，不等于“当前实现容量”
- 所有 transaction 中出现的 `robIdx/lqIdx/sqIdx` 都应受上面容量约束
- sequence/resource manager 必须同时维护 `flag + value`，不能只维护 value

---

## 1. backendToTopBypass_agent_agent_xaction

### DUT 连接方向
见：
- [backendToTopBypass_agent_connect.sv](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/mem_ut/ver/ut/memblock/tb/backendToTopBypass_agent_connect.sv)

`MEMBLOCK_UT` 下由 UVM 驱动到 DUT：
- `io_ooo_to_mem_backendToTopBypass_cpuWfi`
- `io_ooo_to_mem_backendToTopBypass_cpuCriticalError`
- `io_ooo_to_mem_backendToTopBypass_msiAck`

### Scala 来源
这些信号进入 `MemBlock.scala` 的：
- `io.ooo_to_mem.backendToTopBypass`

并最终影响：
- `io.outer_cpu_wfi`
- `io.outer_cpu_critical_error`
- `io.outer_msi_ack`

### 字段合法值
- 三个字段都是 `bit`
- 合法值：`0/1`

### 建议约束
- 默认空闲：全 `0`
- 冒烟阶段不要随机置 `cpuCriticalError`
- `msiAck` 只有在你构造外部 MSI 场景时才有必要拉高

---

## 2. csr_ctrl_agent_agent_xaction

### DUT 连接方向
见：
- [csr_ctrl_agent_connect.sv](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/mem_ut/ver/ut/memblock/tb/csr_ctrl_agent_connect.sv)

`MEMBLOCK_UT` 下该 transaction 全部由 UVM 驱动到 DUT。

### Scala 来源
进入 `MemBlock.scala`：
- `io.ooo_to_mem.tlbCsr`
- `io.ooo_to_mem.csrCtrl`

这些字段影响：
- TLB/PTW 配置
- 预取器控制
- cache error 使能
- uncache outstanding
- store misalign 支持
- power down / flush_l2
- trigger 配置

### 字段分组与合法值

#### 2.1 TLB CSR 组
字段：
- `satp_mode/asid/ppn/changed`
- `vsatp_*`
- `hgatp_*`

合法值来源：
- 受 RISC-V SATP / HGATP 编码约束

建议：
- `mode` 不要任意随机，至少限制在设计支持模式集合
- `changed` 只在配置更新拍拉高
- `asid/vmid/ppn` 受位宽约束即可

#### 2.2 Memory / PMP / PBMT 组
字段：
- `mPBMTE`
- `hPBMTE`
- `pmm_*`
- `mbmc_*`

建议：
- 这些字段首版以稳定保守配置为主
- 不要在没有目标场景时随机开启复杂内存属性

#### 2.3 Prefetch 控制组
字段：
- `pf_ctrl_l1I_pf_enable`
- `pf_ctrl_l2_pf_enable`
- `pf_ctrl_l1D_pf_enable`
- `pf_ctrl_l2_pf_store_only`
- `pf_ctrl_l2_pf_recv_enable`
- `pf_ctrl_l2_pf_pbop_enable`
- `pf_ctrl_l2_pf_vbop_enable`
- `pf_ctrl_l2_pf_tp_enable`
- `pf_ctrl_l2_pf_delay_latency`

建议：
- 首版功能验证时默认固定到已知稳定配置
- `delay_latency` 按位宽给范围即可

#### 2.4 Backend 控制组
字段：
- `ldld_vio_check_enable`
- `cache_error_enable`
- `uncache_write_outstanding_enable`
- `hd_misalign_st_enable`
- `power_down_enable`
- `flush_l2_enable`

建议：
- 按场景显式配置，不做完全随机

#### 2.5 Trigger / distribute CSR 组
字段：
- `distribute_csr_w_*`
- `frontend_trigger_*`
- `mem_trigger_*`

合法值来源：
- 对应 Backend trigger / CSR 结构

建议：
- 没有专门 trigger testcase 时，全部保持 idle/默认值

### 文档结论
这是一个“强配置接口”，不适合做大范围随机激励。推荐采用：
- 固定稳定默认配置
- 按 testcase 场景只改必要字段

---

## 3. fence_agent_agent_xaction

### DUT 连接方向
见：
- [fence_agent_connect.sv](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/mem_ut/ver/ut/memblock/tb/fence_agent_connect.sv)

UVM 驱动到 DUT：
- `io_ooo_to_mem_sfence_valid`
- `io_ooo_to_mem_sfence_bits_rs1`
- `io_ooo_to_mem_sfence_bits_rs2`
- `io_ooo_to_mem_sfence_bits_addr`
- `io_ooo_to_mem_sfence_bits_id`
- `io_ooo_to_mem_sfence_bits_hv`
- `io_ooo_to_mem_sfence_bits_hg`

### Scala 来源
进入：
- `MemBlock.scala` 中 `io.ooo_to_mem.sfence`

### 字段合法值
- `valid`：`0/1`
- `rs1/rs2/hv/hg`：布尔控制位
- `addr/id`：按位宽合法

### 建议
- 无专门 fence 场景时，默认 idle
- `valid=1` 时其他字段必须成组有效

---

## 4. lsqcommit_agent_agent_xaction

### DUT 连接方向
见：
- [lsqcommit_agent_connect.sv](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/mem_ut/ver/ut/memblock/tb/lsqcommit_agent_connect.sv)

UVM 驱动到 DUT：
- `io_ooo_to_mem_lsqio_pendingPtr_flag`
- `io_ooo_to_mem_lsqio_pendingPtr_value`
- `io_ooo_to_mem_flushSb`

### Scala 来源
对应 `MemBlock.scala` 中：
- `io.ooo_to_mem.lsqio.pendingPtr`
- `io.ooo_to_mem.flushSb`

注意：
- Scala 侧完整 LSQ 提交接口还包括 `lcommit/scommit/commit/pendingPtrNext`
- 当前这个 agent 只覆盖了很少一部分

### 字段合法值
- `pendingPtr_flag/value`
  - 由 ROB 提交窗口推进决定
  - 不能独立随机
- `flushSb`
  - `0/1`

当前容量约束：
- `pendingPtr_flag inside {0,1}`
- `pendingPtr_value inside {[0:351]}`

### 建议
- `pendingPtr` 必须跟 `robIdx` 生命周期一致
- 没有构造 flush 场景时，`flushSb=0`

---

## 5. lsqenq_agent_agent_xaction

### DUT 连接方向
见：
- [lsqenq_agent_connect.sv](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/mem_ut/ver/ut/memblock/tb/lsqenq_agent_connect.sv)

UVM 驱动到 DUT：
- `needAlloc_[0..7]`
- `req_[0..7]_*`

UVM 观察 DUT 输出：
- `canAccept`
- `resp_[0..7].lqIdx/sqIdx`

### Scala 来源
关键源码：
- [Dispatch.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/backend/dispatch/Dispatch.scala)
- [LSQWrapper.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/mem/lsqueue/LSQWrapper.scala)

### 字段分组与合法值

#### 5.1 `needAlloc`
合法值：
- `00`：不分配
- `01`：load / vload
- `10`：store / vstore

#### 5.2 `req_i_valid`
合法值：
- `0/1`

#### 5.3 `req_i_bits_fuType`
实际合法子集：
- `ldu`
- `stu`
- `vldu`
- `vstu`

对标量框架建议：
- load: `36'h0001_0000`
- store: `36'h0002_0000`

补充：
- `mou = 36'h0004_0000` 是合法 `FuType`，但通常不走普通标量 `lsqenq` 主路径
- `Dispatch.scala` 中 `enqLsqIO.req.valid` 会过滤掉 `isAMOVec(i)`
- 因此对普通 `lsqenq` 有效请求来说，不应把 `mou` 当作常规合法值来随机发

#### 5.4 `req_i_bits_uopIdx`
来源：
- decode split 结果

合法值：
- 标量：固定 `0`
- 向量：`0..numUops-1`
- 语义上不应超过 `64`

#### 5.5 `req_i_bits_robIdx`
合法值：
- 来自 ROB/rename 分配
- 必须和 issue/commit 统一使用

当前容量约束：
- `robIdx_flag inside {0,1}`
- `robIdx_value inside {[0:351]}`

#### 5.6 `req_i_bits_lqIdx/sqIdx`
说明：
- 输入侧是 uop 字段镜像
- 真正有效的分配结果以 `resp` 为准

建议：
- 首版激励输入可固定 0
- 后续跟踪分配使用 `resp`

当前容量约束：
- `lqIdx_flag inside {0,1}`
- `lqIdx_value inside {[0:71]}`
- `sqIdx_flag inside {0,1}`
- `sqIdx_value inside {[0:55]}`

#### 5.7 `req_i_bits_numLsElem`
合法值：
- 标量固定 `1`
- 向量按元素数分配

### 结论
这是 `1.1` 最关键的 transaction，字段之间必须保持强一致性：
- `needAlloc`
- `fuType`
- `uopIdx`
- `robIdx`
- `numLsElem`

---

## 6. lintsissue_agent_agent_xaction

### DUT 连接方向
见：
- [lintsissue_agent_connect.sv](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/mem_ut/ver/ut/memblock/tb/lintsissue_agent_connect.sv)

UVM 观察 `ready`
UVM 驱动 `valid/bits`

### Scala 来源
见 `MemBlock.scala`：
- `issueLda`
- `issueSta`
- `issueStd`

### 端口语义

#### Load 相关端口
- `intIssue_0/1/2`

#### Store address 相关端口
- `intIssue_3/4`

#### Store data 相关端口
- `intIssue_5/6`

### 字段合法值

#### `ready`
- 由 DUT 输出
- 不应人为约束其“值域行为”，只监控

#### `valid`
- `0/1`

#### `fuType`
- load 路径：`ldu`
- store address / store data 路径：`stu`
- AMO 路径可能为 `mou`

实际映射：
- `ldu = 36'h0001_0000`
- `stu = 36'h0002_0000`
- `mou = 36'h0004_0000`

#### `fuOpType`
- 如果 `fuType=ldu`
  - 用 `LSUOpType.lb/lh/lw/ld/lbu/lhu/lwu`
- 如果 `fuType=stu`
  - 用 `LSUOpType.sb/sh/sw/sd`
- 如果 `fuType=mou`
  - 用 AMO 相关 `LSUOpType`

当 `fuType = mou` 时，可直接映射的原子类 `fuOpType` 包括：
- `lr_w      = 2`
- `sc_w      = 6`
- `amoswap_w = 10`
- `amoadd_w  = 14`
- `amoxor_w  = 18`
- `amoand_w  = 22`
- `amoor_w   = 26`
- `amomin_w  = 30`
- `amomax_w  = 34`
- `amominu_w = 38`
- `amomaxu_w = 42`
- `amocas_w  = 46`
- `lr_d      = 3`
- `sc_d      = 7`
- `amoswap_d = 11`
- `amoadd_d  = 15`
- `amoxor_d  = 19`
- `amoand_d  = 23`
- `amoor_d   = 27`
- `amomin_d  = 31`
- `amomax_d  = 35`
- `amominu_d = 39`
- `amomaxu_d = 43`
- `amocas_d  = 47`
- `amocas_q  = 44`

#### `src_0/src_1/...`
- 由具体 issue 类型决定
- 对标量 LSU 首版：
  - LDA 的 `src_0` 作为地址
  - STA 的 `src_0` 作为地址
  - STD 的 `src_0` 作为数据

#### `robIdx/lqIdx/sqIdx`
- 必须和 `lsqenq` 分配结果一致
- 不能重新随机

当前容量约束：
- `robIdx_flag inside {0,1}`
- `robIdx_value inside {[0:351]}`
- `lqIdx_flag inside {0,1}`
- `lqIdx_value inside {[0:71]}`
- `sqIdx_flag inside {0,1}`
- `sqIdx_value inside {[0:55]}`

#### `pdest/rfWen/fpWen`
- load 路径要和后端寄存器写回语义一致

### 指定字段影响性 note
针对以下 `intIssue_0_0` 字段：
- `pdest`
- `rfWen`
- `fpWen`
- `pc`
- `isRVC`
- `ftqIdx_flag/value`
- `ftqOffset`
- `loadWaitBit`
- `waitForRobIdx_flag/value`
- `storeSetHit`
- `loadWaitStrict`

结合 [Bundles.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/backend/Bundles.scala)、[Backend.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/backend/Backend.scala)、[Dispatch.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/backend/dispatch/Dispatch.scala) 和 `mem/*` 内部使用点，可以分成三类。

#### 1. 对 MemBlock 行为有直接功能影响
- `loadWaitBit`
- `waitForRobIdx_flag/value`
- `storeSetHit`
- `loadWaitStrict`

原因：
- 这些字段不是普通附带 metadata，而是后端 MDP / store-set 预测结果。
- 在 [Dispatch.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/backend/dispatch/Dispatch.scala) 中，`loadWaitBit / waitForRobIdx / loadWaitStrict` 会被 LFST/store-set 查询结果覆盖或更新。
- 在 [Backend.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/backend/Backend.scala) 中，这些字段被明确送入 `toMem`，且受 `EnableMdp` 控制。
- 在 [NewLoadUnit.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/mem/pipeline/NewLoadUnit.scala) 中，`storeForwardReq.loadWaitBit/loadWaitStrict/storeSetHit/waitForRobIdx` 直接参与 load 等待、前递和冲突处理。
- 在 [NewStoreQueue.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/mem/lsqueue/NewStoreQueue.scala) 中，`loadWaitBit + waitForRobIdx` 或 `storeSetHit + ssid` 直接生成 store-set mask，影响 load 看见哪些老 store。
- 在 [LoadQueueReplay.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/mem/lsqueue/LoadQueueReplay.scala) 中，`loadWaitStrict` 被写入 replay/strict 等待相关状态。

结论：
- 这 4 组字段属于真正的功能控制输入，乱随机会直接改变 MemBlock 的等待、前递、重放和依赖判定行为。
- UVM 不应把它们当成“无语义 bit”；必须与后端 dispatch/store-set 语义一致。

#### 2. 对 MemBlock 有间接功能影响，不是纯透传
- `pc`
- `isRVC`
- `ftqIdx_flag/value`
- `ftqOffset`

原因：
- 这些字段主要描述“这条访存指令是谁、来自哪里”，不直接决定地址计算结果，但会参与若干 MemBlock 子功能。
- `pc` 在 [MemBlock.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/mem/MemBlock.scala) 中送给 prefetcher 训练；在 [NewLoadUnit.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/mem/pipeline/NewLoadUnit.scala)、[StoreUnit.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/mem/pipeline/NewStoreUnit.scala)、[AtomicsUnit.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/mem/pipeline/AtomicsUnit.scala) 中进入 TLB debug、rollback、nuke query、prefetch/debug 相关路径。
- `ftqIdx/ftqOffset/isRVC` 在 [Bundles.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/backend/Bundles.scala) 中被写进 `DynInst/uop`；随后在 [LoadQueueRAW.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/mem/lsqueue/LoadQueueRAW.scala)、[LoadQueueUncache.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/mem/lsqueue/LoadQueueUncache.scala)、[NewLoadUnit.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/mem/pipeline/NewLoadUnit.scala)、[StoreUnit.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/mem/pipeline/NewStoreUnit.scala) 中参与 redirect/rollback/nuke/异常定位。
- `pc` 在输入侧本身就是通过 `pc + (ftqOffset << instOffsetBits)` 组出来的，所以 `ftqOffset` 也会影响最终记录的指令 PC 语义。

结论：
- 这组字段不是 load/store 主数据通路控制位，但也不是“纯透传可忽略”。
- 它们主要影响 prefetch 训练、redirect/rollback 定位、异常/调试/trace 语义。
- 冒烟时可以给相对稳定、可解释的值；做精确场景时必须与真实指令流对应。

#### 3. 主要是透传到写回/ROB/结果侧的元数据
- `pdest`
- `rfWen`
- `fpWen`

原因：
- 在 [Bundles.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/backend/Bundles.scala) 中，这些字段被装入 `DynInst.uop`。
- 在 [mem/Bundles.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/mem/Bundles.scala) 的 `toExuOutput()` 中，它们主要被重新带到 `ExuOutput`/写回口。
- 在 [NewLoadUnit.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/mem/pipeline/NewLoadUnit.scala)、[StoreUnit.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/mem/pipeline/NewStoreUnit.scala)、[AtomicsUnit.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/mem/pipeline/AtomicsUnit.scala) 中，`pdest/rfWen/fpWen` 主要用于决定结果回写到哪个物理寄存器、是否向整型/浮点写回，以及 ROB/写回 sideband 携带什么信息。
- 它们通常不决定 load/store 是否命中、是否前递、是否等待某个 store，也不决定地址翻译或 store-set 判定。

结论：
- 从“MemBlock 内部访存功能”角度看，`pdest/rfWen/fpWen` 更接近透传元数据。
- 但它们对系统级正确性仍然重要，因为会影响写回通路和完成语义。
- 因此不能乱填；只是它们不是本 transaction 中最核心的 mem 功能激励点。

#### 建议
- 如果目标是测 MemBlock 核心访存行为，优先保证：
  - `loadWaitBit`
  - `waitForRobIdx_flag/value`
  - `storeSetHit`
  - `loadWaitStrict`
  - `pc/isRVC/ftqIdx/ftqOffset`
  这两组字段语义自洽。
- 如果目标是只做最小冒烟，`pdest/rfWen/fpWen` 可以采用保守固定模板，但不要与 `fuType/fuOpType` 的真实写回属性矛盾。

### 指定字段测试赋值策略 note
针对：
- `pc`
- `isRVC`
- `ftqIdx_flag/value`
- `ftqOffset`
- `pdest`
- `rfWen`
- `fpWen`

建议不要统一按“全随机”处理，而应按“是否在 MemBlock 内部产生真实状态影响”分层。

#### A. 在 MemBlock 内部没有实质状态作用，主要是记录/透传/写回 sideband
字段：
- `pdest`
- `rfWen`
- `fpWen`

源码依据：
- [mem/Bundles.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/mem/Bundles.scala#L134)
- [NewLoadUnit.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/mem/pipeline/NewLoadUnit.scala#L1417)
- [StoreUnit.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/mem/pipeline/NewStoreUnit.scala#L633)
- [AtomicsUnit.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/mem/pipeline/AtomicsUnit.scala#L154)

分析：
- 这组字段主要被带到 `DynInst/uop`，再经 `toExuOutput()` 带到写回口和 ROB sideband。
- 它们通常不参与 load/store 冲突检测、前递 mask、store-set 等 MemBlock 内部核心状态机。
- 它们真正的系统级效果主要落在后端写回语义，而不是 MemBlock 内部访存功能控制。

测试建议：
- 不需要大范围随机。
- 应给合法固定初值，再做很弱的、可解释的递增。

合法初值建议：
- 普通标量 load：
  - `pdest = 8'd1`
  - `rfWen = 1'b1`
  - `fpWen = 1'b0`
- store / STA / STD：
  - `pdest = 8'd0`
  - `rfWen = 1'b0`
  - `fpWen = 1'b0`

合法激励增加逻辑：
- `pdest` 只在 `rfWen || fpWen` 为 `1` 时递增。
- `pdest` 建议在小范围内滚动，如 `1..63` 或 `1..127`，不需要覆盖全 8bit。
- `rfWen` 和 `fpWen` 不允许同时为 `1`。
- 除非明确测试浮点 load/store 相关路径，否则 `fpWen` 建议固定 `0`。

最小模板：
```systemverilog
if (is_load_like) begin
  tr.pdest = next_pdest;
  tr.rfWen = 1'b1;
  tr.fpWen = 1'b0;
  next_pdest++;
end else begin
  tr.pdest = '0;
  tr.rfWen = 1'b0;
  tr.fpWen = 1'b0;
end
```

#### B. 在 MemBlock 内部有间接作用，但不是核心状态控制
字段：
- `pc`
- `isRVC`
- `ftqIdx_flag/value`
- `ftqOffset`

源码依据：
- [Bundles.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/backend/Bundles.scala#L1189)
- [MemBlock.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/mem/MemBlock.scala#L733)
- [NewLoadUnit.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/mem/pipeline/NewLoadUnit.scala#L1050)
- [LoadQueueRAW.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/mem/lsqueue/LoadQueueRAW.scala#L369)
- [LoadQueueUncache.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/mem/lsqueue/LoadQueueUncache.scala#L608)

分析：
- 这组字段不直接决定 load/store 的等待、前递和依赖判定。
- 但会影响：
  - prefetch train
  - rollback / redirect 定位
  - 异常、nuke、debug、trace 语义
- 所以它们不能全随机噪声化，否则虽然未必立刻出功能错误，但波形和行为会很难解释。

测试建议：
- 采用“固定合法初值 + 缓慢递增”的方式。
- 不建议全宽随机。

合法初值建议：
- `pc = 50'h1000`
- `isRVC = 1'b0`
- `ftqIdx_flag = 1'b0`
- `ftqIdx_value = 6'd0`
- `ftqOffset = 5'd0`

合法激励增加逻辑：
- `pc` 每发一条事务 `+4`。
- 如果未来专门覆盖压缩指令场景，再允许 `isRVC = 1`，并令对应步长为 `+2`。
- `ftqOffset` 在最简单冒烟中建议固定 `0`。
- `ftqIdx_value` 不需要每拍变化；可选策略是每 `8` 条事务 `+1`。
- `ftqIdx_flag` 只有在 `ftqIdx_value` 回卷时再翻转；最简单场景下可一直固定 `0`。

最小模板：
```systemverilog
tr.pc           = next_pc;        // init 50'h1000
tr.isRVC        = 1'b0;
tr.ftqIdx_flag  = 1'b0;
tr.ftqIdx_value = 6'd0;
tr.ftqOffset    = 5'd0;

next_pc += 4;
```

稍真实但仍安全的模板：
```systemverilog
tr.pc           = next_pc;
tr.isRVC        = 1'b0;
tr.ftqIdx_flag  = ftq_wrap;
tr.ftqIdx_value = ftq_idx;
tr.ftqOffset    = 5'd0;

next_pc += 4;
txn_cnt++;
if (txn_cnt % 8 == 0) begin
  ftq_idx++;
  if (ftq_idx == 6'd63) begin
    ftq_idx  = 0;
    ftq_wrap = ~ftq_wrap;
  end
end
```

#### C. 与真正影响 MemBlock 内部状态的字段的关系
对比：
- `loadWaitBit`
- `waitForRobIdx_flag/value`
- `storeSetHit`
- `loadWaitStrict`

这组字段在 [Dispatch.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/backend/dispatch/Dispatch.scala#L798)、[Backend.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/backend/Backend.scala#L486)、[NewLoadUnit.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/mem/pipeline/NewLoadUnit.scala#L358)、[NewStoreQueue.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/mem/lsqueue/NewStoreQueue.scala#L366) 中会直接改变 load wait、store-set mask、前递和 replay 判定，属于真正的功能控制位。

因此：
- `pc/isRVC/ftq*` 和 `pdest/rfWen/fpWen` 可以采用“合法固定值 + 轻量递增”策略。
- `loadWait* / waitForRobIdx* / storeSetHit` 不能套用同样思路，默认应全 `0`，只有在专门的 MDP/store-set 场景下才按真实语义驱动。

#### 推荐最小完整给值方案
适合当前 memblock UT 基础冒烟：

```systemverilog
tr.pc                    = next_pc;   // init 50'h1000
tr.isRVC                 = 1'b0;
tr.ftqIdx_flag           = 1'b0;
tr.ftqIdx_value          = 6'd0;
tr.ftqOffset             = 5'd0;

if (is_load_like) begin
  tr.pdest               = next_pdest; // init 8'd1
  tr.rfWen               = 1'b1;
  tr.fpWen               = 1'b0;
  next_pdest++;
end else begin
  tr.pdest               = '0;
  tr.rfWen               = 1'b0;
  tr.fpWen               = 1'b0;
end

tr.loadWaitBit           = 1'b0;
tr.waitForRobIdx_flag    = 1'b0;
tr.waitForRobIdx_value   = '0;
tr.storeSetHit           = 1'b0;
tr.loadWaitStrict        = 1'b0;

next_pc += 4;
```

### 结论
`lintsissue` 是最容易因为“字段看起来都能随机”而造出非法激励的 transaction。
最重要的是：
- store 不能只发一类 issue
- `robIdx/lqIdx/sqIdx` 必须引用 enqueue 结果

### AtomicsUnit note
`AtomicsUnit` 的入口复用 `intIssue`，不是单独一套专用外部接口。

在 [MemBlock.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/mem/MemBlock.scala) 中：
- `issueSta = intIssue.filter(_.bits.params.hasStoreAddrFu)`
- `st_atomics = issueSta.map(x => x.valid && FuType.storeIsAMO(x.bits.fuType))`
- `issueSta(i).ready := atomicsUnit.io.in.ready`
- `atomicsUnit.io.in.bits := Mux1H(st_atomics, issueSta.map(_.bits))`

这说明：
- `AtomicsUnit` 入口来自 `STA` 路径
- 进入原子单元的关键判定是 `fuType = mou`
- 对应 `fuOpType` 必须使用上面的 AMO/LR/SC 编码
- 完整原子事务还会关联数据侧输入，因此不能只打一笔孤立的地址 issue 就视为完整 AMO 测试

---

## 7. vecissue_agent_agent_xaction

### DUT 连接方向
见：
- [vecissue_agent_connect.sv](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/mem_ut/ver/ut/memblock/tb/vecissue_agent_connect.sv)

UVM 观察：
- `ready`

UVM 驱动：
- `valid`
- `fuType/fuOpType`
- `vpu.*`
- `vuopIdx`
- `numLsElem`
- `lqIdx/sqIdx`

### Scala 来源
见：
- `MemBlock.scala`
- `backend/decode/DecodeUnitComp.scala`
- `backend/decode/UopInfoGen.scala`
- `backend/fu/vector/Bundles.scala`
- `package.scala`

### 字段合法值

#### `fuType`
合法值子集：
- `vldu`
- `vstu`
- `vsegldu`
- `vsegstu`

实际映射：
- `vldu    = 36'h1000_00000`
- `vstu    = 36'h2000_00000`
- `vsegldu = 36'h4000_00000`
- `vsegstu = 36'h8000_00000`

#### `fuOpType`
合法值来源：
- `VlduType`
- `VstuType`

说明：
- `fuType = vldu / vsegldu` 时，`fuOpType` 应来自 `VlduType`
- `fuType = vstu / vsegstu` 时，`fuOpType` 应来自 `VstuType`
- 不能把标量 `LSUOpType.lb/lh/lw/ld/sb/sh/sw/sd` 直接用于 vector LSU

结合 `DecodeUnit.scala` 和 `package.scala`，vector LSU 最常见的合法子类包括：
- `VlduType.vle`
- `VlduType.vleff`
- `VlduType.vlse`
- `VlduType.vloxe`
- `VlduType.vluxe`
- `VlduType.vlm`
- `VlduType.vlr`
- `VstuType.vse`
- `VstuType.vsse`
- `VstuType.vsoxe`
- `VstuType.vsuxe`
- `VstuType.vsm`
- `VstuType.vsr`

约束建议：
- 首版不要把 `fuType` 和 `fuOpType` 分开独立随机
- 应按 `load类 / store类 / segment类 / unit-stride/strided/indexed` 模板成组生成

#### `vpu.vl`
含义：
- 当前 vector length

来源：
- 后端 decode / vset 结果

建议：
- 非零
- 首版建议用小而稳定的值，例如 `1 / 2 / 4 / 8 / 16`

#### `vpu.vsew`
来源：
- `backend/fu/vector/Bundles.scala`

合法值：
- `VSew.e8  = 3'b000`

---

- `VSew.e16 = 3'b001`
- `VSew.e32 = 3'b010`
- `VSew.e64 = 3'b011`

保留值：
- `3'b1??` 为 reserved

#### `vpu.vlmul`
来源：
- `backend/fu/vector/Bundles.scala`

合法值：
- `VLmul.m1  = 3'b000`
- `VLmul.m2  = 3'b001`
- `VLmul.m4  = 3'b010`
- `VLmul.m8  = 3'b011`
- `VLmul.mf2 = 3'b111`
- `VLmul.mf4 = 3'b110`
- `VLmul.mf8 = 3'b101`

保留值：
- `3'b100` 为 reserved

#### `vpu.vma / vpu.vta`
含义：
- 当前 vtype 的 agnostic 控制位

合法值：
- 都是 `0/1`

#### `vpu.vill`
含义：
- 当前向量配置非法标志

建议：
- 正常功能路径固定 `0`
- 非法配置测试时才拉高

#### `vpu.veew`
含义：
- vector memory 元素宽度编码

约束建议：
- 应与 `fuOpType`、`vsew` 协同
- 不能脱离 opcode 独立随机

#### `vpu.nf`
含义：
- segment field 数

建议：
- 非 segment 访存固定 `0`
- segment 场景按 `0..7` 合法生成，对应 field 数 `1..8`

#### `vpu.vuopIdx`
合法值：
- `0..numUops-1`
- 语义上不超过 `64`

#### `vpu.lastUop`
含义：
- 当前拆分 uop 是否为最后一个

合法值：
- `0/1`
- 必须与 `vuopIdx/numUops` 一致

#### `numUops` 说明
`vecissue_agent_agent_xaction` 自身没有单独的 `numUops` 字段，但真实 `numUops` 由后端 decode 的 `UopInfoGen.scala` 决定。

与 vector memory 最直接相关的 split 类型是：
- `UopSplitType.VEC_US_LDST`
- `UopSplitType.VEC_US_FF_LD`
- `UopSplitType.VEC_S_LDST`
- `UopSplitType.VEC_I_LDST`

`numOfWB` / `numUops` 由以下因素共同决定：
- `vsew`
- `veew`
- `vlmul`
- `nf`
- `isVlsr`
- `isVlsm`
- 单位步长 / 跨步 / 索引 / segment 类型

因此约束原则是：
- `vuopIdx` 必须落在 decode 预计的 `0..numUops-1`
- 非 segment、非复杂 split 的首版场景建议固定：
  - `vuopIdx = 0`
  - `lastUop = 1`
  - `nf = 0`
  - `vlmul = m1`
  - `vsew = e32 或 e64`

#### `robIdx/lqIdx/sqIdx`
这些索引在 vector LSU 上同样受当前容量约束：
- `robIdx_flag inside {0,1}`
- `robIdx_value inside {[0:351]}`
- `lqIdx_flag inside {0,1}`
- `lqIdx_value inside {[0:71]}`
- `sqIdx_flag inside {0,1}`
- `sqIdx_value inside {[0:55]}`

#### `vpu.vl/vsew/vlmul/vma/vta/vill/...`
合法值来源：
- 后端 decode 产生的 VPU 控制字段

建议：
- 不要完全随机
- 应按一个合法 `vtype` 和具体 vector LSU opcode 成组生成

#### `numLsElem/lqIdx/sqIdx`
- 必须和 vector lsq enqueue 规则一致
- `numLsElem` 应与 `vl / nf / 访存类型` 协同
- 首版建议取小值，如 `1 / 2 / 4`

### 结论
如果没有先把 `VlduType/VstuType` 和 `vuopIdx` 的 decode 规则吃透，不建议直接随机这个 transaction。

首版推荐收敛策略：
- `fuType`
  - load：`vldu`
  - store：`vstu`
- `fuOpType`
  - 先只用 unit-stride 类
- `vpu.vill = 0`
- `vpu.vma/vta` 固定稳定组合
- `vpu.vsew` 先只用 `e32/e64`
- `vpu.vlmul` 先只用 `m1`
- `vpu.nf = 0`
- `vuopIdx = 0`
- `lastUop = 1`

---

## 8. redirect_agent_agent_xaction

### DUT 连接方向
见：
- [redirect_agent_connect.sv](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/mem_ut/ver/ut/memblock/tb/redirect_agent_connect.sv)

UVM 驱动到 DUT：
- `io_redirect_valid`
- `io_redirect_bits_level`
- `io_redirect_bits_robIdx_flag/value`

### Scala 来源
对应：
- `MemBlock.scala` 中 `io.redirect`

### 字段合法值
- `valid`：`0/1`
- `level`：redirect level 编码，见下方说明
- `robIdx`：必须是当前 ROB 域中的合法指针

#### `level`
真值来源：
- [package.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/package.scala) 中 `RedirectLevel`
- [Bundle.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/Bundle.scala) 中 `Redirect.level`

当前实现不是多位枚举，而是 `1 bit`：
- `RedirectLevel.flushAfter = 1'b0`
- `RedirectLevel.flush      = 1'b1`

语义：
- `flushAfter`
  - 重定向从当前指令之后开始生效
  - 常见于 branch/jump/CSR 正常重定向
- `flush`
  - 当前指令自身也被冲刷
  - 常见于 replay / exception / 某些 ROB flush 场景

源码使用点：
- `JumpUnit` 默认设置 `redirect.level := RedirectLevel.flushAfter`
- `BranchUnit` 默认设置 `redirect.bits.level := RedirectLevel.flushAfter`
- `Rob` 会在 `flush` 和 `flushAfter` 间选择：
  - `deqHasReplayInst`
  - `intrEnable`
  - `deqHasException`
  - `needModifyFtqIdxOffset`

约束建议：
- 普通分支/跳转测试优先使用 `flushAfter (0)`
- replay/异常/精确 flush 测试再使用 `flush (1)`

### 建议
- 没有专门 rollback/flush testcase 时，默认 idle

---

## 9. other_ctrl_agent_agent_xaction

### DUT 连接方向
该接口是混合接口：
- 一部分字段 UVM 驱动到 DUT
- 一部分字段是 DUT 输出供监控

从 driver 可见当前主动驱动的是：
- `io_hartId`
- `io_outer_reset_vector`
- `io_inner_beu_errors_icache_ecc_error_valid`
- `io_inner_beu_errors_icache_ecc_error_bits`

### Scala 来源
这些对应 `MemBlock.scala` 中：
- hart/reset vector
- 部分 error / control sideband

### 字段合法值
- `hartId/reset_vector`：按 SoC/系统配置合法
- `ecc_error_valid/bits`：只有在异常注入场景才主动驱动
- 其余状态类字段主要由 DUT 输出，属于 monitor 观测对象

### 建议
- 这类接口要区分“输入控制字段”和“输出状态字段”，不要一股脑当作随机 driver transaction

---

## 10. int_sink_agent_agent_xaction

### DUT 连接方向
UVM 驱动到 DUT：
- 各类 interrupt sink 输入

### Scala 来源
对应 `MemBlock` 及外围中断输入路径。

### 字段合法值
- 全部是 `bit`
- 合法值都是 `0/1`

### 建议
- 默认全 0
- 单独测试中断场景时，再按源分类拉高

---

## 11. itlb_agent_agent_xaction

### DUT 连接方向
UVM 驱动：
- `io_fetch_to_mem_itlb_req_0_valid`
- `vpn`
- `s2xlate`
- `resp_ready`

UVM 观察：
- `resp_valid/resp_bits`

### Scala 来源
对应：
- `MemBlock.scala` 中 `io.fetch_to_mem.itlb`

### 合法值
- `vpn`：虚页号位宽内合法
- `s2xlate`：按 stage-2 模式编码
- `resp_ready`：`0/1`

### 建议
- ITLB 场景要和 `satp/vsatp/hgatp` 配置一致

---

## 12. L2tlb_agent_agent_xaction

### DUT 连接方向
UVM 驱动：
- `io_ptw_req_0_ready`
- `io_ptw_resp_valid`
- `io_ptw_resp_bits_*`

UVM 观察：
- `io_ptw_req_0_valid`
- `io_ptw_req_0_bits_*`

### Scala 来源
对应：
- `MemBlock.scala` 中 `PTWNewFilter/PTW`
- `L2TLB` 相关请求/响应

### 字段合法值

#### 请求侧
- `vpn`
- `s2xlate`
由 DUT 产生，UVM 只观察

#### 响应侧
- `entry_tag/asid/vmid/ppn/perm/level/v/pf/af/gpf/gaf`
必须来自合法页表项语义

建议：
- 不要纯随机每个 bit
- 应围绕“页表命中 / page fault / access fault / stage2 fault”模板生成

---

## 13. dcache_agent_agent_xaction

### DUT 连接方向
见：
- [dcache_agent_connect.sv](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/mem_ut/ver/ut/memblock/tb/dcache_agent_connect.sv)

这是典型的双向接口：

UVM 驱动：
- `A.ready`
- `B.*`
- `C.ready`
- `D.*`
- `E.ready`

UVM 观察：
- `A.*` request
- `C.*` request
- `E.valid/sink`

### Scala 来源
对应：
- `MemBlock.scala` 中 `dcache.io.lsu.load/sta/atomics`
- `DCacheWrapper.scala`

### 字段分组

#### A 通道
- DUT 发请求
- UVM 主要观察
- `a_ready` 由 UVM 环境反馈

#### B 通道
- Probe/外部请求类输入
- 首版常固定 idle，除非专门做 coherency/probe 场景

#### C 通道
- DUT 发写回/释放类请求
- `c_ready` 由 UVM 反馈

#### D 通道
- UVM 对 DUT 的响应通道
- 合法值取决于 opcode/param/size/source/sink/data/denied/corrupt 组合

#### E 通道
- DUT 对 grant/ack 的后续处理
- `e_ready` 由 UVM 反馈

### 建议
- 这类 transaction 不适合“字段逐 bit 随机”
- 应按 TileLink 通道模板构造合法组合

---

## 14. sbuffer_agent_agent_xaction

### DUT 连接方向
见：
- [sbuffer_agent_connect.sv](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/mem_ut/ver/ut/memblock/tb/sbuffer_agent_connect.sv)

与 dcache 类似，是双向接口：
- `A` 请求由 DUT 输出、UVM 观察
- `D` 响应由 UVM 驱动回 DUT

### Scala 来源
对应：
- `MemBlock.scala` 中 `sbuffer.io.*`
- `src/main/scala/xiangshan/mem/sbuffer/*`

### 字段合法值

#### A 通道
- `a_ready`：UVM 反馈
- 其他 `a_*` 字段由 DUT 输出，应监控

#### D 通道
- `valid/opcode/param/size/source/sink/denied/data/corrupt`
必须按合法响应模板生成

### 建议
- 首版按“正常 ack / denied / corrupt”三类模板建模

---

## 15. prefetch_agent_agent_xaction

### DUT 连接方向
主要是 DUT 输出的预取请求/通知，UVM 监控为主。

字段包括：
- `auto_inner_l3_pf_sender_out_*`
- `auto_inner_l2_pf_sender_out_*`
- `io_ifetchPrefetch_*`

### Scala 来源
对应：
- `MemBlock.scala` 中 `PrefetcherWrapper`
- 各级预取发送路径

### 字段合法值
- 地址字段按位宽合法
- `addr_valid/l2_pf_en` 为布尔
- `pf_source` 应来自预取源编码

### 建议
- 这类接口通常不做 driver 约束，主要做 monitor 观察

---

## 16. io_mem_to_ooo_ctrl_agent_agent_xaction

### DUT 连接方向
这是 `mem_to_ooo` 的 DUT 输出接口，UVM 主要监控。

### Scala 来源
对应：
- `MemBlock.scala` 中 `io.mem_to_ooo.*`
- 最终反馈到 backend/top

### 字段分组
- topToBackendBypass / interrupt sideband
- `lqCancelCnt/sqCancelCnt/lqDeq/sqDeq`
- `lsqio_vaddr/vstart/vl/gpaddr/isForVSnonLeafPTE/mmioBusy/lqCanAccept/sqCanAccept`
- `updateLFST`
- `stIssuePtr`
- `memoryViolation`
- `mdpTrain`
- `lsTopdownInfo`

### `mem_to_ooo.lsqio` 返回字段说明

当前 RTL 顶层暴露以下 `mem_to_ooo.lsqio` 输出：

- `io_mem_to_ooo_lsqio_vaddr`
- `io_mem_to_ooo_lsqio_vstart`
- `io_mem_to_ooo_lsqio_vl`
- `io_mem_to_ooo_lsqio_gpaddr`
- `io_mem_to_ooo_lsqio_isForVSnonLeafPTE`
- `io_mem_to_ooo_lsqio_mmioBusy`
- `io_mem_to_ooo_lsqio_lqCanAccept`
- `io_mem_to_ooo_lsqio_sqCanAccept`

这些字段定义在 [MemBlock.scala](/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/mem/MemBlock.scala) 的 `mem_to_ooo.lsqio` bundle 中，属于 DUT 返回后端的状态和异常信息，不是 UVM 输入激励字段。

#### 异常地址类字段

`vaddr/vstart/vl/gpaddr/isForVSnonLeafPTE` 来自 `ExceptionInfoGen`，在 `MemBlock.scala` 中由 `exceptionInfoGen.io.exceptionInfo` 打一拍后输出。

- `vaddr`：内存异常对应的虚拟地址。`XSCore.scala` 将其接到 `backend.io.mem.exceptionAddr.vaddr`，后端 CSR 逻辑用于更新 trap 相关异常地址。
- `gpaddr`：guest physical address。用于虚拟化两阶段地址转换相关异常，后端 CSR trap entry 会用它生成 guest physical address 相关 trap 信息。
- `isForVSnonLeafPTE`：表示异常是否属于 VS-stage non-leaf PTE 访问语义。后端 CSR trap entry 会用它决定 `mtinst/htinst` 等 trap 信息。
- `vstart`：向量 LS 异常时对应的向量起始元素索引。
- `vl`：向量 LS 异常时对应的 vector length。

当前 `XSCore.scala` 只把 `vaddr/gpaddr/isForVSnonLeafPTE` 接给 backend 的 `exceptionAddr`；`vstart/vl` 虽然从 MemBlock 顶层输出，但当前后端连接路径中没有看到实际消费。对当前 mem_ut 标量 LS 测试，`vstart/vl` 建议只作为 monitor 观测或覆盖字段。

#### `mmioBusy`

`mmioBusy` 在 `MemBlock.scala` 中来自 `lsq.io.rob.mmioBusy`，更底层由 LSQ 汇总 store queue 或 load queue 的 MMIO/uncache busy 状态后输出。

在完整 Core 中，`XSCore.scala` 将它接到 `backend.io.mem.robLsqIO.mmioBusy`。ROB 中用它屏蔽 commit-stuck 判断：如果 LSQ 正在处理 MMIO/uncache，ROB 长时间不提交不应被误判为普通 commit stuck。

对当前 mem_ut MemBlock-only 基础功能测试，如果测试框架不建模完整 ROB commit-stuck 检测，也不构造 MMIO/uncache 长等待场景，则 `mmioBusy` 不需要参与 issue/commit gating。它可保持为 monitor 观测、X-check 或覆盖字段。

#### `lqCanAccept/sqCanAccept`

`lqCanAccept/sqCanAccept` 来自 `LSQWrapper.scala`：

- `lqCanAccept = loadQueue.io.enq.canAccept`
- `sqCanAccept = storeQueue.io.enq.canAccept`

语义上它们分别表示 LoadQueue / StoreQueue 当前是否还能接收新项。`LSQWrapper` 内部还会用两者互相约束 load/store 入队合法性。

Scala 后端 dispatch 主阻塞条件使用的是统一的 `enqLsqIO.canAccept`。当前 mem_ut LSQ 入队已经等待 DUT `io_ooo_to_mem_enqLsq_canAccept`，并在 canAccept 后采样 `enqLsq_resp` 分配到的 `lqIdx/sqIdx`，所以 `lqCanAccept/sqCanAccept` 不应替代当前入队成功条件。

建议：

- 普通标量 LS flow 仍以 `io_ooo_to_mem_enqLsq_canAccept + resp` 作为入队闭环依据。
- `lqCanAccept/sqCanAccept` 作为更细粒度的 LQ/SQ 接收能力观测信号，可用于 X-check、coverage，或后续 directed case 分析 LQ 满/SQ 满导致的 admission stall。
- 如果后续要精确建模 backend dispatch stall 原因，可以把这两个信号加入 runtime snapshot，但不要绕过全局 canAccept/resp 闭环。

### 字段合法值
- 这些值由 DUT 内部状态机和后端协议产生
- 不应从 UVM 端反向施加“输入约束”

### 建议
- 文档化每组字段的语义和来源
- 用于 checker/scoreboard，不用于随意驱动

---

## 17. io_mem_to_ooo_int_wb_agent_agent_xaction

### DUT 连接方向
这个接口是 memblock 向后端的 int writeback 输出。

当前 UVM 里唯一 driver 反馈的是：
- `io_mem_to_ooo_intWriteback_3_0_ready`

其他大多数字段是 DUT 输出。

### Scala 来源
对应：
- `MemBlock.scala` 中 `intWriteback`

### 合法值
- `ready`：由后端/环境反馈给 DUT
- `valid/toRob/pdest/toIntRf/toFpRf/debug_*`：由 DUT 输出，不能作为任意输入

### 建议
- 对这类接口，把 `ready` 和 `payload` 分开建模

---

## 18. io_mem_to_ooo_vec_wb_agent_agent_xaction

### DUT 连接方向
类似 int wb：
- UVM driver 反馈：
  - `vecWriteback_1_0_ready`
  - `vecWriteback_0_0_ready`
- 其他字段大多为 DUT 输出

### Scala 来源
对应：
- `MemBlock.scala` 中 `vecWriteback`

### 字段合法值
- `ready`：`0/1`
- `valid/data/pdest/robIdx/vecWen/v0Wen/vlWen/vls/vpu/debug_*`：由 DUT 输出

### 建议
- 不要把整份 transaction 当成“随机输入包”

---

## 19. io_mem_to_ooo_wakeup_agent_agent_xaction

### DUT 连接方向
这是 DUT 输出到后端 issue queue/wakeup 相关接口。

### Scala 来源
对应：
- `MemBlock.scala` 中 `io.mem_to_ooo.wakeup`

### 字段合法值
- 由 DUT writeback/wakeup 逻辑决定
- UVM 主要监控

### 建议
- 这类 transaction 应用于 checker，不应用于 driver 随机化

---

## 20. io_mem_to_ooo_iq_feedback_agent_agent_xaction

### DUT 连接方向
这是 DUT 输出到 IQ 的 feedback 接口。

### Scala 来源
对应：
- `MemBlock.scala` 中：
  - `staIqFeedback`
  - `vstuIqFeedback`
  - `vlduIqFeedback`

### 字段合法值
- `valid/hit/robIdx/sqIdx/lqIdx/flushState/sourceType`
由 DUT 内部反馈链路决定

### 建议
- 这类 transaction 只做 monitor 侧语义分析

---

## 总结建议

### 1. 真正需要重点做“合法值约束”的 transaction
优先级最高：
- `lsqenq_agent_agent_xaction`
- `lintsissue_agent_agent_xaction`
- `vecissue_agent_agent_xaction`
- `lsqcommit_agent_agent_xaction`
- `L2tlb_agent_agent_xaction`
- `dcache_agent_agent_xaction`
- `sbuffer_agent_agent_xaction`

### 2. 主要应监控、不应任意驱动的 transaction
- `io_mem_to_ooo_ctrl_agent_agent_xaction`
- `io_mem_to_ooo_wakeup_agent_agent_xaction`
- `io_mem_to_ooo_iq_feedback_agent_agent_xaction`
- 以及各类 writeback payload 字段

### 3. 对后续 UVM 建模的直接建议
- 对输入 transaction，约束必须追溯到 Scala 源码真值
- 对输出 transaction，不要误建成“全字段随机驱动”
- 对双向握手接口，必须拆清：
  - 哪些字段是 DUT 输出
  - 哪些字段是环境返回的 `ready/resp`
