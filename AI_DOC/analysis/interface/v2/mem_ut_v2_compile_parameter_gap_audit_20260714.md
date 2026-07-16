# mem_ut V2 编译期参数遗漏审计

状态：审计通过（第十一轮无新增项且无必须修复项）

日期：2026-07-14

## 1. 审计目标

本文检查 `mem_ut/ver/ut/memblock` 测试框架和环境中仍由固定数值表达、但实际由
XiangShan 硬件配置或 elaboration 结果决定的字段宽度、端口数量和能力开关。

本轮只形成参数缺口和 consumer 修改清单，不修改 SystemVerilog 源码。后续 coding
必须保持以下单一权威链：

```text
V2 profile / compile override
  -> memblock_compile_params.svh 默认宏
  -> package localparam / typedef（仅在需要时派生）
  -> interface / xaction / driver / monitor / XZ check
  -> raw struct / transaction / sequence helper
```

## 2. 版本和权威输入

- 分支：`mem_ut_uvm_v2`。
- 审计基线 commit：`6e721ccb42bec882b3254062bff003294a507854`。
- V2 权威 RTL：`build_memblock/rtl/MemBlock.sv`。
- compile 宏入口：`mem_ut/ver/ut/memblock/cfg/memblock_compile_params.svh`。
- V2 DUT profile：`mem_ut/ver/ut/memblock/rule/version/v2/dut_interface_baseline.md`。
- V2 L2TLB profile：`mem_ut/ver/ut/memblock/rule/version/v2/l2tlb_interface_profile.md`。
- 参数归属规则：`mem_ut/ver/ut/memblock/rule/memblock_parameter_management_rule.md`。

`tb/dut_inst.sv` 是当前生成 RTL 的逐端口展开边界。该文件可以保留与当前 V2 RTL
完全一致的固定声明，不作为公共测试框架的第二参数权威；agent、公共 package、sequence
和 helper 不能据此继续复制固定宽度。

## 3. 判定规则

### 3.1 必须进入编译期参数链

满足以下任一条件时，应由 `memblock_compile_params.svh` 或其同源派生类型表达：

1. 字段宽度由 core 参数、queue size、VLEN、地址模式或物理寄存器数量计算。
2. 宽度由 TileLink edge、source/sink range、beat size 或 user bundle elaboration 决定。
3. 端口数量或端口布局决定 interface 数组、循环边界、mask 索引或 driver/monitor 扫描范围。
4. capability 决定某一 DUT 字段是否存在，公共逻辑不能在字段不存在时读取默认零值。

### 3.2 不建立第二数值宏

若某宽度可以从已存在的主参数无歧义派生，则只建立 package localparam 或 typedef，
不增加可独立覆盖的第二数值权威。例如：

```systemverilog
localparam int MEMBLOCK_COMMIT_COUNT_W =
    $clog2(`MEMBLOCK_DUT_COMMIT_WIDTH + 1);
localparam int MEMBLOCK_LQ_CANCEL_COUNT_W =
    $clog2(`MEMBLOCK_DUT_LQ_SIZE + 1);
localparam int MEMBLOCK_SQ_CANCEL_COUNT_W =
    $clog2(`MEMBLOCK_DUT_SQ_SIZE + 1);
```

### 3.3 不应宏化的内容

固定 ISA 编码、固定协议字段和仅存在于展开边界且无公共 consumer 的字段不新增死宏。
运行期 queue、关联数组、状态表条目数和 testcase 行为限制也不是硬件结构参数。

## 4. 确认需要新增的编译期参数项

### 4.1 Scalar 和公共跨 flow 字段

| 建议宏 | V2 默认值 | 配置来源 | 主要固定值 consumer | 参数化原因 |
|---|---:|---|---|---|
| `MEMBLOCK_DUT_INT/FP/VF/V0/VL_PREG_NUM` | `224/192/128/22/32` | 五类 `PregParams.numEntries` | pdest 合法范围和派生 `PDEST_W` | 只有8-bit容器无法表达各 register file 的合法上限。当前 scalar flow 按 `rfWen/fpWen` 分别使用 INT/FP 数量；vector LS unsupported gate 保留，后续 vector专项使用 VF/V0/VL数量。 |
| `MEMBLOCK_DUT_MAX_UOP_SIZE` | 65 | V2 `MaxUopSize` | LSQ enqueue `uopIdx` 合法范围、由其派生的 `UOP_IDX_W`，以及 vector `vuopIdx` 同类型字段 | DUT packed width严格复现 `UopIdx=log2Up(MaxUopSize+1)`；测试框架合法激励范围保持 `0..MaxUopSize-1`，两者不能互相替代。 |
| `MEMBLOCK_DUT_XLEN` | 64 | V2 `XLEN` | scalar issue LDA/STA/STD `src_0`、int-WB data、`lsqio_vaddr/gpaddr`、distributed CSR write data | 这些 64-bit 字段不是 VAddr/PAddr shape。用独立 XLEN 主宏可以防止误套地址宏，并闭合 RV64 数据宽度 consumer。 |
| `MEMBLOCK_DUT_VADDR_W` | 50 | V2 `VAddrBits` 由 Sv48/H 扩展配置决定 | fence/prefetch DUT-facing vaddr、scalar issue `uop_pc`、`issue_field_assigner::compute_pc()` | 50 bit 是当前 MMU 配置结果，不是固定协议宽度。主表和软件模型中有意保留的 64-bit 地址容器不应机械缩窄。 |
| `MEMBLOCK_DUT_PADDR_W` | 48 | V2 core physical address 配置 | reset vector、BEU/ECC address 及其它确有公共 UVM consumer且明确使用 `PAddrBits` 的字段 | core PAddr 与 TL edge addressBits 是独立来源；当前同为48不能共用权威。顶层 L2 TLB paddr 当前只在 `dut_inst.sv` 展开，不作为公共 consumer；`lsqio_vaddr/gpaddr` 是 XLEN，L2/L3 sender address 是固定 64-bit bridge 字段。 |
| `MEMBLOCK_DUT_SFENCE_ID_W` | 16 | `AsidLength` | fence interface/xaction/monitor、raw sfence payload 和匹配 helper | sfence ID 有独立配置来源，不能与 PTW entry ASID 或 SATP CSR ASID 仅因当前同宽而合并。 |
| `MEMBLOCK_DUT_TLB_ASID_W` | 16 | `MMUAsidLen` | ITLB/L2TLB response/entry interface、xaction、driver/monitor、sequence | `PtwReq` request只有 `vpn/s2xlate`，不携带 ASID；lookup context来自 runtime CSR/software容器，在 entry fit边界转换。 |
| `MEMBLOCK_DUT_HART_ID_W` | 6 | `MaxHartIdBits = max(log2Up(n), 6)` | `other_ctrl_agent_agent.io_hartId`、ctrl agent `topToBackendBypass_hartId` 及其字段链 | Hart ID 宽度由系统 tile/hart 配置决定，不是固定 CSR 编码。 |
| `MEMBLOCK_DUT_MSI_INFO_W` | 13 | `imsicIntSrcWidth + log2Ceil(2 + geilen)` | 普通 ctrl `msiInfo`；`HAS_TEE_IMSIC=1` 时的 `teemsiInfo.bits` | 普通和 TEE MSI payload使用同一 `MSI_INFO_WIDTH`；固定 `[12:0]` 会形成独立系统配置权威。 |
| `MEMBLOCK_DUT_ENSBUFFER_WIDTH` | 2 | V2 `EnsbufferWidth` | `sqDeq` count 的派生上限 | `sqDeq` 每拍数量由 enqueue-to-sbuffer width 决定，与 SQ size、SQ pointer width 和 commit width均不同。 |

Scala/RTL 代表依据：

- `src/main/scala/xiangshan/Parameters.scala:154`：`MaxUopSize`。
- `src/main/scala/xiangshan/Parameters.scala:624`：VAddr 配置选择。
- `src/main/scala/xiangshan/Parameters.scala:748`：`PhyRegIdxWidth`。
- `src/main/scala/xiangshan/backend/Bundles.scala:445`：`vuopIdx = UopIdx()`。
- `src/main/scala/xiangshan/backend/Bundles.scala:897`：`UopIdx` 定义。
- `src/main/scala/xiangshan/backend/fu/vector/Bundles.scala:209`：`NumLsElem` 定义。
- `src/main/scala/xiangshan/mem/MemBlock.scala:133`：`sqDeq` count 宽度来源。
- `src/main/scala/xiangshan/mem/MemBlock.scala:307`：Hart ID 接口。
- `src/main/scala/system/SoC.scala:112`：MSI info 配置来源。
- `build_memblock/rtl/MemBlock.sv:266`：V2 sfence address 宽度。
- `build_memblock/rtl/MemBlock.sv:405`：V2 LSQ enqueue `uopIdx` 宽度。
- `build_memblock/rtl/MemBlock.sv:413`：V2 `numLsElem` 宽度。

### 4.2 Vector 配置字段

本轮 scalar load/store 主流程不支持 vector LS / `issueVldu`，但现有 vector agent 已经是
编译环境的一部分，其 DUT-facing packed 字段仍不能保留独立 VLEN 常量。建议新增：

| 建议宏 | V2 默认值 | 主要 consumer | 参数化原因 |
|---|---:|---|---|
| `MEMBLOCK_DUT_VLEN` | 128 | vector issue source、vmask；vector WB data、vmask | `[127:0]` 直接由 VLEN 派生。 |
| `MEMBLOCK_DUT_VLENB` | 16 | vector replay byte mask 及同语义 mask | VLENB 由 VLEN/8 派生；可在 package 中由 `MEMBLOCK_DUT_VLEN` 计算，避免独立 override。 |
| `MEMBLOCK_DUT_VL_W` | 8 | RTL 中使用 `Vl()` 的 vector issue/WB `vl` 或 `vstart` 字段 | `vlWidth = log2Up(VLEN) + 1`；仅替换权威 RTL 中确认为 `Vl()` 的字段，不能把独立 `Vstart` 类型机械套用此宏。 |
| `MEMBLOCK_DUT_VSMB_SIZE` | 16 | vector replay merge-buffer 合法条目范围及派生 index width | 非二次幂配置下，只有 index width 无法表达合法范围，必须以 `VsMergeBufferSize` 为主数量。 |
| `MEMBLOCK_DUT_MAX_FLOW_NUM` | 16 | vector issue `flowNum` 合法语义范围 | 顶层 `MemExuInput.flowNum` 的 packed类型仍是 `NumLsElem()`，宽度消费 `NUM_LS_ELEM_W`；独立 `maxFlowNum` 只约束 flow语义上限。 |
| `MEMBLOCK_DUT_VLDU_PORT_NUM` | 2 | vector issue、vector WB 的 `issueVldu/writebackVldu` 端口族 | 由 `backendParams.VlduCnt` 展开；固定端口 0/1 是第二数量权威。 |
| `MEMBLOCK_DUT_VSTU_PORT_NUM` | 2 | vector store IQ-feedback `vstuIqFeedback_0/1` 端口族 | 由独立 `VstuCnt` 展开，不能复用 VLDU port 数。 |
| `MEMBLOCK_DUT_LD_EXU_PORT_NUM` | 3 | wakeup 端口族 | 由 `LdExuCnt = LduCnt + HyuCnt` 派生，语义上不能复用只表示 `LduCnt` 的 LOAD pipe 数。 |
| `MEMBLOCK_DUT_VLDU_FUTYPE_PORT_MASK` | `2'b01` | vector issue 各端口 `fuType` presence | V2 只有 vector issue port 0 具有 35-bit `fuType`，port 1 没有该命名端口；数组化不能假设两端口同构。 |

`MEMBLOCK_DUT_VLENB` 和 `MEMBLOCK_DUT_VL_W` 优先作为
`MEMBLOCK_DUT_VLEN` 的只读派生 localparam。只有编译器/include 层次不能消费 package
localparam 时，才允许增加同名预处理宏，并必须在 `check_compile_param_consistency()` 中检查
派生关系，不允许三个值独立漂移。

本轮仍不支持 vector LS / `issueVldu` 行为闭环。上述宏只描述已经参与编译的接口 shape；
unsupported gate 不得因字段参数化而被移除。固定命名端口未生成化前，`VLDU_PORT_NUM=2`、
`LD_EXU_PORT_NUM=3` 和 `VLDU_FUTYPE_PORT_MASK=2'b01` 是精确 V2 tuple，其它 override 必须
fail-fast。

### 4.3 公共 PTW/TLB elaboration shape

L2TLB agent 仍保持 DTLB -> L2TLB request、L2TLB -> DTLB response 的 responder 语义。
下列 shape 来自公共 `PtwReq/PtwRespS2`，同时被顶层 `itlb_agent` 和内部 DTLB/L2TLB
responder 消费，因此不能命名成 L2TLB 私有参数。参数化只改变 packed shape，不改变
L2TLB agent 连接方向、lookup key 或 response 行为：

| 建议宏 | V2 默认值 | 主要 consumer | 参数化原因 |
|---|---:|---|---|
| `MEMBLOCK_DUT_TLB_VPN_W` | 38 | ITLB/L2TLB request interface/xaction/monitor、lookup helper | 由 VAddr/MMU 配置计算。 |
| `MEMBLOCK_DUT_TLB_S1_TAG_W` | 35 | S1 entry tag、TLB entry 和 response chain | `vpnLen - sectorIdxW = 38 - 3`。 |
| `MEMBLOCK_DUT_TLB_ASID_W` | 16 | PTW/TLB response/entry ASID | 来自 `MMUAsidLen`；request不携带 ASID，runtime CSR/software容器只在显式 fit/slice边界转换；不得与 sfence ID 或 SATP/VSATP CSR容器合并。 |
| `MEMBLOCK_DUT_TLB_VMID_W` | 14 | PTW/TLB entry/response 中的 VMID | entry VMID 与 HGATP CSR 的 16-bit 容器不是同一字段，禁止互换。 |
| `MEMBLOCK_DUT_HAS_H_EXTENSION` | 1 | `PtwEntry.vmid` presence、VAddr 配置选择 | Scala 中真正条件存在的是 `PtwEntry.vmid`。不得用该 capability 包住无条件存在的 `vsatp/hgatp`、sfence `hv/hg` 或整个 `PtwRespS2.s2`。 |
| `MEMBLOCK_DUT_TLB_S1_PPN_W` | 41 | S1 PPN response 字段 | `ptePPNLen - sectorIdxW = 44 - 3`，不是从 `PADDR_W` 推导。 |
| `MEMBLOCK_DUT_TLB_GVPN_W` | 38 | S2 tag、S2 PPN/GVPN response 字段 | S2 tag 和 S2 PPN 都使用 `gvpnLen`。 |
| `MEMBLOCK_DUT_TLB_SECTOR_NUM` | 8 | `ppn_low_0..7`、`valididx_0..7`、`pteidx_0..7` 三组数组及 sequence fill/clear 循环 | 固定数组项数目前形成 interface、transaction、driver/monitor/connect、sequence多份权威。 |
| `MEMBLOCK_DUT_TLB_SECTOR_IDX_W` | 3 | `addr_low`、八个 `ppn_low` 和 sector index/slice | 应由 sector count 派生，不允许独立覆盖。 |

代表位置：

- `src/main/scala/xiangshan/cache/mmu/MMUConst.scala:102`。
- `src/main/scala/xiangshan/cache/mmu/MMUBundle.scala:1218`。
- `mem_ut/ver/ut/memblock/agent/itlb_agent_agent/src/itlb_agent_agent_interface.sv:22`。
- `mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent/src/L2tlb_agent_agent_interface.sv:22`。
- `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_tlb_entry.sv:60`。
- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv:299`。

### 4.4 DCache 和 uncache/MMIO TL edge 协商宽度

TileLink 的 `opcode/param/denied/corrupt` 是固定协议字段，不宏化；`size` 的编码语义固定，
但其容器宽度由 edge `maxTransfer` elaboration 计算，必须参数化。data、mask、address、
source、sink 和 user bundle 的实际宽度也由当前 edge shape 决定。

| 建议宏 | V2 默认值 | 主要 consumer | 参数化原因 |
|---|---:|---|---|
| `MEMBLOCK_DUT_DCACHE_TL_DATA_W` | 256 | DCache interface/xaction/driver/monitor data | 当前 DCache beat shape。 |
| `MEMBLOCK_DUT_DCACHE_TL_ADDRESS_W` | 48 | DCache TL address | 来自 TL manager address range，不等同 core `PAddrBits`。 |
| `MEMBLOCK_DUT_DCACHE_TL_SIZE_W` | 3 | DCache TL A/B/C/D size | `sizeBits` 由 edge `maxTransfer` 计算，不是固定3-bit协议容器。 |
| `MEMBLOCK_DUT_DCACHE_TL_SOURCE_W` | 6 | DCache source | 由当前 edge source range 协商。 |
| `MEMBLOCK_DUT_DCACHE_TL_SINK_W` | 10 | DCache sink | 由当前 edge sink range 协商。 |
| `MEMBLOCK_DUT_DCACHE_USER_VADDR_W` | 44 | DCache user vaddr | user bundle elaboration shape，不等同完整 VAddr。 |
| `MEMBLOCK_DUT_DCACHE_ALIAS_W` | 2 | DCache alias | DCache 配置派生。 |
| `MEMBLOCK_DUT_DCACHE_HAS_ALIAS` | 1 | DCache alias 字段的 interface/connect presence | `aliasBitsOpt` 是 Option；只有 width 不能表达字段不存在的 profile。 |
| `MEMBLOCK_DUT_DCACHE_HAS_BCE` | 1 | DCache B/C/E channel presence | coherent channel 是否存在由 edge `hasBCE` 决定；只有 A/D shape 不足以描述接口。 |
| `MEMBLOCK_DUT_DCACHE_HAS_USER_VADDR` | 1 | DCache A/C `user_vaddr` presence | BundleField presence 由 edge 协商；只有44-bit width不能表达字段不存在。 |
| `MEMBLOCK_DUT_DCACHE_HAS_USER_REQ_SOURCE` | 1 | DCache A/C `user_reqSource` presence | 与公共5-bit reqSource width分开表达 presence。 |
| `MEMBLOCK_DUT_DCACHE_HAS_USER_NEED_HINT` | 1 | DCache A/C `user_needHint` presence | 1-bit字段仍可能在其它 profile 完全不存在。 |
| `MEMBLOCK_DUT_DCACHE_HAS_ECHO_IS_KEYWORD` | 1 | DCache A/C/D `echo_isKeyword` presence | Echo BundleField 由 edge协商，不能因其为1 bit而忽略 presence。 |
| `MEMBLOCK_DUT_REQ_SOURCE_W` | 5 | DCache user reqSource、L2 prefetch source | 来自公共 `MemReqSource.reqSourceBits`，与 TL source 不是同一字段。 |
| `MEMBLOCK_DUT_DCACHE_L2_HINT_SOURCE_W` | 4 | DCache L2 hint source ID | 由 `log2Up(nMissEntries)` 派生，不能复用 6-bit TL source。 |
| `MEMBLOCK_DUT_UNCACHE_TL_DATA_W` | 64 | 当前 `sbuffer_agent` 承接的 uncache/MMIO TL interface/xaction/driver/monitor data | uncache edge beat shape与DCache不同。 |
| `MEMBLOCK_DUT_UNCACHE_TL_ADDRESS_W` | 48 | 当前 `sbuffer_agent` TL address | 来自 uncache manager address range；当前与 core PAddr同值不代表同一语义权威。 |
| `MEMBLOCK_DUT_UNCACHE_TL_SIZE_W` | 3 | 当前 `sbuffer_agent` TL size | 由该 edge `maxTransfer` elaboration 计算。 |
| `MEMBLOCK_DUT_UNCACHE_TL_SOURCE_W` | 4 | 当前 `sbuffer_agent` source | uncache edge source range。 |
| `MEMBLOCK_DUT_UNCACHE_TL_SINK_W` | 1 | 当前 `sbuffer_agent` sink | uncache edge sink range；1-bit 字段仍应由同源 shape 表达。 |
| `MEMBLOCK_DUT_UNCACHE_HAS_BCE` | 0 | 当前 `sbuffer_agent` channel presence | V2 uncache/MMIO edge 只有 A/D，B/C/E 完全不存在。 |

现有 `sbuffer_agent` 连接的是 `uncache_port` 的 `auto_inner_buffers_out_*` TLBuffer 链，
不是 StoreBuffer 对外边界。保留 agent 类名时，参数名和文档语义仍必须使用 uncache/MMIO
edge，不能把这组 data/source/sink 误写成 StoreBuffer 内部结构参数。

V2 channel-width consumer 矩阵：

| Edge | address | data | mask | size/source | sink | user/echo |
|---|---|---|---|---|---|---|
| DCache | A/B/C | A/B/C/D | A/B | A/B/C/D | D/E | A/C：alias、vaddr、reqSource、needHint、isKeyword；D：isKeyword |
| uncache/MMIO | A | A/D | A | A/D | D | 无 |

DCache B 无 user/echo，C 无 mask，D 无 address/mask，E 只有 sink；后续参数化不得为了统一
数组形状补造 RTL 不存在的字段。所有 presence capability 必须同时保护 interface/clocking、
xaction field/constraint/automation/print/compare、driver、monitor/XZ、connect 和 default sequence
经 xaction randomization 形成的传递依赖。

`seq/base_seq_help/mem_base_sequence.sv` 也是两条 edge shape的直接 consumer：transaction
构造参数、address slice、line/beat byte literal、data/mask sized literal和稀疏存储访问必须
消费对应 `*_TL_ADDRESS/DATA/MASK/SIZE_W`。通用稀疏存储容器可以保留内部 geometry，但
不能把 `[47:5]`、`[47:3]`、32 bytes、`[255:0]`、`[63:0]` 当作 DUT shape第二权威。

代表位置：

- `src/main/scala/xiangshan/cache/dcache/DCacheWrapper.scala:39`。
- `src/main/scala/xiangshan/mem/MemBlock.scala:286`。
- `mem_ut/ver/ut/memblock/agent/dcache_agent_agent/src/dcache_agent_agent_interface.sv:25`。
- `mem_ut/ver/ut/memblock/agent/sbuffer_agent_agent/src/sbuffer_agent_agent_interface.sv:25`。
- `mem_ut/ver/ut/memblock/tb/sbuffer_agent_connect.sv:18`。
- `build_memblock/rtl/MemBlock.sv:61`。

`MEMBLOCK_DUT_DCACHE_USER_VADDR_W=44` 可在后续 coding 中由
`VADDR_W - DCACHE_BLOCK_OFFSET_W` 派生；若本轮不新增 block-offset 主参数，则允许把44作为
V2 profile 叶子 shape，但必须在一致性检查中核对 RTL，不得从 PAddr 或 TL address 猜测。

### 4.5 Control、trigger 和可选端口 capability

| 建议宏 | V2 默认值 | 主要 consumer | 参数化原因和边界 |
|---|---:|---|---|
| `MEMBLOCK_DUT_TRIGGER_NUM` | 4 | CSR frontend/mem `tEnableVec_0..3`、`tUpdate.bits.addr` | trigger 条目数由 `TriggerNum` 配置。`tUpdate.bits.addr` 宽度由数量派生；`tdata.action[3:0]` 仍是固定 action 编码，不使用本宏。 |
| `MEMBLOCK_DUT_HAS_TEE_IMSIC` | 0 | 条件存在的 `teemsiInfo` 输入和 `outer_teemsi_ack` 输出链 | 由 `HasTEEIMSIC` elaboration；V2 当前端口被删除。capability=0 时 interface/connect不得引用不存在端口。 |
| `MEMBLOCK_DUT_HAS_L2_PREFETCH_SENDER` | 1 | `auto_inner_l2_pf_sender_out_*` interface/clocking、xaction/automation/compare/print、monitor/XZ、connect | L2 sender 取决于 core prefetcher配置，不能假设所有 profile都存在。 |
| `MEMBLOCK_DUT_HAS_L3_PREFETCH_SENDER` | 1 | `auto_inner_l3_pf_sender_out_*` interface/clocking、xaction/automation/compare/print、monitor/XZ、connect | L3 sender还依赖 `L3CacheParamsOpt`；当前 V2 RTL存在该端口族。 |
| `MEMBLOCK_DUT_CLINT_INT_PORT/SINK_NUM` | `1/2` | int-sink interface/xaction/driver/monitor/connect 固定端口族 | `IntSinkPortSimple` 参数语义是 `(ports,sinks)`。 |
| `MEMBLOCK_DUT_PLIC_INT_PORT/SINK_NUM` | `2/1` | int-sink固定端口族 | 来自 `IntSinkPortSimple(2,1)`。 |
| `MEMBLOCK_DUT_NMI_INT_PORT/SINK_NUM` | `1/2` | int-sink固定端口族 | sink数来自 `NonmaskableInterruptIO.elements.size`。 |
| `MEMBLOCK_DUT_BEU_INT_PORT/SINK_NUM` | `1/1` | int-sink固定端口族 | 来自 `IntSinkPortSimple(1,1)`。 |

`io_ifetchPrefetch_0..2` 是 `MEMBLOCK_DUT_LOAD_PIPE_NUM` 的固定命名 consumer；ctrl
`ldCancel_0..2` 来自 `LdExuCnt`，必须消费 `MEMBLOCK_DUT_LD_EXU_PORT_NUM`。L2/L3 prefetch sender
的 address 是 bridge 定义的固定 64-bit `PrefetchRecv.addr`，不使用
`MEMBLOCK_DUT_PADDR_W`；L2 `pf_source` 使用公共 `MEMBLOCK_DUT_REQ_SOURCE_W`。

Presence capability 只隔离真正条件存在的字段：

- `MEMBLOCK_DUT_HAS_H_EXTENSION` 只控制 `PtwEntry.vmid` presence，并参与 VAddr profile选择。
- `vsatp/hgatp`、sfence `hv/hg` 和 `PtwRespS2.s2` 在当前 Scala bundle中无条件存在。
- `MEMBLOCK_DUT_HAS_TEE_IMSIC` 控制 TEE MSI端口，不影响普通 `msiInfo`。
- L2/L3 prefetch sender capability 不控制无关的 `io_ifetchPrefetch_*` 端口族。
- debug interrupt sink当前没有 `int_sink_agent` consumer，不为其新增死宏；NMI cause编码也不与
  NMI sink拓扑参数混用。

## 5. 应从既有主宏派生的字段宽度

| 派生 localparam/typedef | 主参数 | V2 结果 | 当前 consumer | 修改理由 |
|---|---|---:|---|---|
| `MEMBLOCK_COMMIT_COUNT_W` | `MEMBLOCK_DUT_COMMIT_WIDTH=8` | 4 | `lsqcommit_agent_agent` 的 `scommit`、ctrl agent 的 `lqDeq` 及其 xaction/monitor/XZ | 计数需表示 0..CommitWidth，公式是 `$clog2(CommitWidth+1)`；不能长期固定 `[3:0]`。 |
| `MEMBLOCK_LQ_CANCEL_COUNT_W` | `MEMBLOCK_DUT_LQ_SIZE=72` | 7 | ctrl agent `lqCancelCnt` 字段链 | cancel count 表示 0..LQ size；当前与 LQ pointer width 同值只是巧合。 |
| `MEMBLOCK_SQ_CANCEL_COUNT_W` | `MEMBLOCK_DUT_SQ_SIZE=56` | 6 | ctrl agent `sqCancelCnt` 字段链 | cancel count 表示 0..SQ size；不能复用 pointer 宏作为语义权威。 |
| `MEMBLOCK_SQ_DEQ_COUNT_W` | `MEMBLOCK_DUT_ENSBUFFER_WIDTH=2` | 2 | ctrl agent/raw ctrl `sqDeq` 字段链 | 表示每拍进入 SBuffer 的 store 数量，公式是 `$clog2(EnsbufferWidth+1)`，不能固定 `[1:0]` 或复用 commit count。 |
| `MEMBLOCK_DUT_PDEST_W` | 五类 `MEMBLOCK_DUT_*_PREG_NUM` 的最大值224 | 8 | issue/WB/wakeup字段、helper、constraint、XZ | 由 `$clog2(max(PREG_NUM...))` 派生；合法 pdest范围仍按实际 register file数量检查，不能使用整个8-bit容器范围。 |
| `MEMBLOCK_DUT_UOP_IDX_W` | `MEMBLOCK_DUT_MAX_UOP_SIZE=65` | 7 | LSQ enqueue `uopIdx`、vector `vuopIdx`、临时变量和 X/Z | 按权威 Scala `UopIdx` 原式由 `$clog2(MAX_UOP_SIZE+1)` 派生；合法 stimulus constraint 独立保持 `0..MAX_UOP_SIZE-1`，不得为简化公式而偏离 DUT packed width。 |
| `MEMBLOCK_DUT_MAX_LS_ELEM` | `MEMBLOCK_DUT_VLEN=128` | 16 | LSQ `numLsElem` 合法上限 | 由 `VLEN/8` 派生，不建立可独立 override 的第二主参数。 |
| `MEMBLOCK_DUT_NUM_LS_ELEM_W` | `MEMBLOCK_DUT_MAX_LS_ELEM=16` | 5 | LSQ `numLsElem`、vector issue `flowNum` packed字段、raw/main transaction、sequence/helper和 X/Z | 由 `$clog2(MAX_LS_ELEM)+1` 派生；`numLsElem`合法值是 `0..MAX_LS_ELEM`，`flowNum` 则按独立 `MAX_FLOW_NUM`限制。 |
| `MEMBLOCK_DUT_VLENB` | `MEMBLOCK_DUT_VLEN=128` | 16 | vector byte mask | 由 VLEN/8 派生。 |
| `MEMBLOCK_DUT_VL_W` | `MEMBLOCK_DUT_VLEN=128` | 8 | 使用 `Vl()` 类型的字段 | 由 `$clog2(VLEN)+1` 派生。 |
| `MEMBLOCK_DUT_VSMB_IDX_W` | `MEMBLOCK_DUT_VSMB_SIZE=16` | 4 | vector replay merge-buffer index | 由 `$clog2(VSMB_SIZE)` 派生；constraint仍按真实 size限制。 |
| `MEMBLOCK_DUT_TLB_SECTOR_IDX_W` | `MEMBLOCK_DUT_TLB_SECTOR_NUM=8` | 3 | `addr_low`、`ppn_low` 和 sector index/slice | 由 `$clog2(SECTOR_NUM)` 派生；sector count同时控制 `ppn_low/valididx/pteidx` 三组数组项数。 |
| `MEMBLOCK_DUT_DCACHE_TL_MASK_W` | `MEMBLOCK_DUT_DCACHE_TL_DATA_W=256` | 32 | DCache mask | 由 data width/8 派生，不允许独立 override。 |
| `MEMBLOCK_DUT_UNCACHE_TL_MASK_W` | `MEMBLOCK_DUT_UNCACHE_TL_DATA_W=64` | 8 | 当前 `sbuffer_agent` mask | 由 data width/8 派生，不允许独立 override。 |
| `MEMBLOCK_TRIGGER_IDX_W` | `MEMBLOCK_DUT_TRIGGER_NUM=4` | 2 | CSR frontend/mem trigger `tUpdate.bits.addr` | 由 `$clog2(TRIGGER_NUM)` 派生；trigger action 4-bit 编码不使用本宽度。 |

这些值可以作为 package localparam/typedef 使用；若 interface package 的编译顺序要求预处理
宏，则宏表达式也必须直接引用主宏，不能复制默认数值。

## 6. 已有宏但 consumer 仍遗漏

### 6.1 机械字段、约束和 slice

| 已有宏 | 遗漏位置 | 当前问题 | 后续修改要求 |
|---|---|---|---|
| `MEMBLOCK_DUT_FTQ_OFFSET_W` | `issue_field_assigner.sv:513,523,533` | 仍使用 `ftq_offset[3:0]` | 源和目标同宽时直接赋值；确需转换时使用同源 sized cast，禁止固定 slice。 |
| `MEMBLOCK_DUT_FUTYPE_W` 和 `MEMBLOCK_DUT_FUTYPE_*_BIT` | `lsqenq_agent_agent_xaction.sv:607` | constraint 保留 V3 `36'h...` 编码，V2 LDU bit15 不在合法集合 | 用当前 scalar FuType 常量/bit 宏生成合法集合；本轮不支持 vector LS 时，vector FuType 必须 constraint 排除或 fail-fast。 |
| `MEMBLOCK_DUT_LQ_SIZE` / `MEMBLOCK_DUT_SQ_SIZE` | ctrl、IQ feedback、LSQ enqueue xaction 中的 `72/56/71/55` range constraint | packed width 已宏化，但合法值范围仍是第二权威 | 约束统一写成 `[0:SIZE-1]` 或明确 count range；不能把 value width 当 queue size。 |
| PREG数量和派生 `MEMBLOCK_DUT_PDEST_W` | wakeup xaction 固定 `8'd255`；`seq_csr_common` 把 pdest base/range clamp 到 `255/256` | 容器可表示范围大于 INT=224、FP=192 的合法条目范围 | 声明/XZ使用派生 `PDEST_W`；激励合法性和 runtime range按 `rfWen/fpWen` 选择 INT/FP PREG数量。不得把 `2**PDEST_W` 当作合法pdest数量。 |
| `MEMBLOCK_DUT_MAX_UOP_SIZE` / `UOP_IDX_W` | LSQ enqueue constraint 固定 `0..64` | 只改字段宽度会丢失真实合法范围 | constraint 上限读 `MAX_UOP_SIZE-1`，声明/XZ读派生 width。 |
| `MEMBLOCK_DUT_MAX_LS_ELEM` / `NUM_LS_ELEM_W` | LSQ constraint、dispatch/manual/soft-test 的 `5'd0/1` | 固定 literal和 `0..31`范围形成第二权威 | sized literal、range、raw/main/sequence临时变量全部读同源派生值；合法上限保持16。 |
| `MEMBLOCK_DUT_VADDR_W` | `fence_agent_agent_xaction.sv:65` | 50-bit字段只允许到 `(1<<49)-1`，最高位被固定为0且无文档依据 | 若没有规范语义依据，constraint 按 `VADDR_W` 完整可表示范围重写；如需 canonical address限制，应显式建独立语义检查。 |

代表位置：

- `mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_ctrl_agent_agent/src/io_mem_to_ooo_ctrl_agent_agent_xaction.sv:204`。
- `mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_iq_feedback_agent_agent/src/io_mem_to_ooo_iq_feedback_agent_agent_xaction.sv:160`。
- `mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_xaction.sv:607`。

### 6.2 端口数量和物理布局

| 已有宏 | 遗漏 consumer | 风险 | 修改边界 |
|---|---|---|---|
| LOAD/STA/STD `PIPE_NUM`、`PORT_BASE`、`SCALAR_ISSUE_PORT_NUM`、`ISSUE_PORT_STYLE_SPLIT` | `issue_field_assigner.sv` 固定 3/2/2 case；`lintsissue_agent_agent_driver.sv` 固定七个 valid、mask bit 和 `2/4/-3/-5` 分界 | compile override 后 target 路由、mask、ready/fire 映射仍按固定 V2 布局；port-style 宏成为死参数 | 公共循环、mask 和 target offset 必须消费宏。编号化 DUT 端口的实际 connect 仍由版本 adapter 显式映射；宏不能凭空生成不存在的 RTL 端口。 |
| LOAD/STA/STD `PIPE_NUM` 和派生 `PORT_BASE` | int-WB interface/xaction/monitor/connect 固定七组字段；monitor 固定 `port_id=0..6`；`dispatch_monitor_event_adapter.sv` 固定 `case 0..2/3..4/5..6` | issue 链参数化后，writeback raw/event 仍可能按旧端口范围分类 | int-WB port id、raw event kind 和 adapter range必须从同一 base/count 派生；精确 tuple fatal不能替代 consumer修改。 |
| `MEMBLOCK_DUT_STA_PIPE_NUM` | IQ feedback `staIqFeedback_0/1` 和 monitor `port_id=0/1` | STA issue数量已宏化，feedback端口仍复制2 | interface/xaction/monitor/connect和 raw producer纳入同一 STA count。 |
| `MEMBLOCK_DUT_VSTU_PORT_NUM` | IQ feedback `vstuIqFeedback_0/1` | vector store feedback数量来自独立 `VstuCnt` | 全字段链、unsupported gate和 fixed tuple消费独立宏；不能复用 VLDU count。 |
| `MEMBLOCK_DUT_LOAD_PIPE_NUM` | prefetch `ifetchPrefetch_0..2` | 固定命名端口族仍复制 LduCnt=3 | 纳入固定命名端口 tuple和同源 consumer清单，不新建同义数量宏。 |
| `MEMBLOCK_DUT_LD_EXU_PORT_NUM` | ctrl `ldCancel_0..2` 和 wakeup端口族 | `ldCancel` 来自 `LdExuCnt`，不能归入 LduCnt | ctrl interface/xaction/monitor/XZ/connect与wakeup全链同源消费 LD_EXU count。 |
| `MEMBLOCK_DUT_MMIO_LOAD_PORT_NUM` | ctrl agent interface/xaction/monitor/connect 固定展开 loadMmio 0/1/2 | 宏当前只被一致性检查消费，端口变化会漏采或引用不存在端口 | transaction/raw 状态和循环改为宏控制的 packed array；编号化 connect 按 profile 显式连接并做数量一致性检查。 |
| `MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM` | `memblock_lsqenq_dispatch_base_sequence.sv` 只映射 slot 0..5；LSQ enqueue driver 固定六组字段 | 宏 override 后大于6会进入 default fatal，小于6仍访问额外字段 | 公共数组/循环/clear/mask 使用宏；编号化物理端口仍保留 V2 adapter 映射并在编译期检查支持数量。 |

在编号化 interface/connect 完成生成化或条件编译前，当前代码只可接受精确 V2 tuple：

```text
LSQ_ENQ_SLOT_NUM=6
LOAD/STA/STD_PIPE_NUM=3/2/2
MMIO_LOAD_PORT_NUM=3
LOAD_PORT_BASE=0
ISSUE_PORT_STYLE_SPLIT=1
TLB_SECTOR_NUM=8
VLDU_PORT_NUM=2
VSTU_PORT_NUM=2
LD_EXU_PORT_NUM=3
VLDU_FUTYPE_PORT_MASK=2'b01
TRIGGER_NUM=4
HAS_H_EXTENSION=1
DCACHE_HAS_ALIAS=1
DCACHE_HAS_BCE=1
DCACHE_HAS_USER_VADDR=1
DCACHE_HAS_USER_REQ_SOURCE=1
DCACHE_HAS_USER_NEED_HINT=1
DCACHE_HAS_ECHO_IS_KEYWORD=1
UNCACHE_HAS_BCE=0
HAS_TEE_IMSIC=0
HAS_L2_PREFETCH_SENDER=1
HAS_L3_PREFETCH_SENDER=1
LSQ_ENQ_HAS_ACCEPT_RESP=0
HAS_SQ_DEQ_PTR=0
CLINT_INT_PORT/SINK_NUM=1/2
PLIC_INT_PORT/SINK_NUM=2/1
NMI_INT_PORT/SINK_NUM=1/2
BEU_INT_PORT/SINK_NUM=1/1
```

任何其它 override 必须由 `check_compile_param_consistency()` 或编译期 assertion fatal；
不能在固定命名端口仍存在时宣称单独改宏即可适配。

代表位置：

- `mem_ut/ver/ut/memblock/seq/base_seq_help/issue_field_assigner.sv:281`。
- `mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent/src/lintsissue_agent_agent_driver.sv:277`。
- `mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_ctrl_agent_agent/src/io_mem_to_ooo_ctrl_agent_agent_interface.sv:57`。
- `mem_ut/ver/ut/memblock/tb/io_mem_to_ooo_ctrl_agent_connect.sv:52`。
- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_lsqenq_dispatch_base_sequence.sv:399`。
- `mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_driver.sv:91`。

### 6.3 capability 宏存在但运行期逻辑未隔离

| capability 宏 | 当前遗漏 | V2 风险 | 后续方案 |
|---|---|---|---|
| `MEMBLOCK_DUT_LSQ_ENQ_HAS_ACCEPT_RESP=0` | enqueue sequence 仍固定等待 accept，driver response 采样路径为空 | 无 response 的 V2 flow 可能只能等待 timeout | capability=0 时 admission completion 必须走 V2 已定义的 request fire/驱动完成语义；capability=1 时才等待并解析 accept response。不得伪造 response 字段。 |
| `MEMBLOCK_DUT_HAS_SQ_DEQ_PTR=0` | raw ctrl 状态默认清零后，adapter 无条件把零值当真实 SQ deq pointer | `sqDeq!=0` 时可能从错误 key 释放 mapping 或报 mismatch | capability=0 时禁止消费该字段，SQ deq 起点按 V2 commit/deq 专项定义的 cursor/head 状态推导；capability=1 时才读取 monitor raw pointer。 |

代表位置：

- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_lsqenq_dispatch_base_sequence.sv:171`。
- `mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv:66`。
- `mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv:203`。

这两项不是单纯字段声明替换，而是既有 capability 宏未进入运行期分支。后续 coding 必须归入
LSQ admission 和 commit/deq 对应专项，不能在 compile 参数文件中实现业务 fallback。

### 6.4 Raw/common 和生成代码 consumer

新增或派生宏不能只改 interface 声明。后续 coding 必须逐链检查：

- 所有 count字段：interface/clocking、xaction field/constraint/automation/print/compare、monitor
  X/Z、connect、raw payload和 handler临时变量；其中 `COMMIT_COUNT_W` 覆盖 raw `lq_deq`，
  `SQ_DEQ_COUNT_W` 覆盖 raw `sq_deq`。
- `MEMBLOCK_DUT_SFENCE_ID_W` / `MEMBLOCK_DUT_VADDR_W`：fence interface/clocking、xaction
  field/constraint/automation/print/compare、driver、monitor/XZ、connect、raw sfence payload、
  runtime snapshot和 `common_data_transaction` 匹配 helper。
- `MEMBLOCK_DUT_XLEN`：scalar issue LDA/STA/STD `src_0`、int-WB data、
  `lsqio_vaddr/gpaddr`、distributed CSR write data 的
  interface/clocking、xaction automation/constraint/print/compare、driver各模式、monitor/XZ和
  connect，以及 `issue_field_assigner` 的显式 fit边界；trigger `tdata2`、clintTime和prefetch
  sender固定64-bit字段不自动归入 XLEN。
- PTW/TLB shape：ITLB/L2TLB interface/clocking、xaction automation/constraint/print/compare、
  L2TLB driver、monitor/XZ、connect、`memblock_l2tlb_base_sequence` request临时变量和 response
  fill固定 slice、`memblock_tlb_entry`，以及 `ppn_low/valididx/pteidx` 三组 sector数组。
- lookup key和 CSR runtime snapshot中的 ASID/VMID使用固定16-bit软件/CSR容器，不是
  `TLB_VMID_W=14` 的声明 consumer；`TLB_ASID_W/TLB_VMID_W` 只用于 PTW/TLB entry字段及
  明确的 fit/slice转换边界。
- LOAD/STA/STD base/count：int-WB monitor `port_id`、raw int-WB event 和
  `dispatch_monitor_event_adapter` 分类 case。
- `PDEST/UOP/VLEN/VL/VSMB`：xaction pack/unpack/compare/print、monitor X/Z width、
  driver 临时变量和 connect，不得只改 interface/xaction 主声明。
- vector `flowNum` 的 interface/xaction/driver/monitor/connect packed width消费
  `NUM_LS_ELEM_W`，constraint合法范围消费 `MAX_FLOW_NUM`；不建立独立 DUT-facing
  width宏。

Vector monitor 当前边界必须显式记录：

- vector issue 和 vector WB monitor 的 X/Z block 当前为空，后续必须始终检查
  `issueVldu/writebackVldu` valid，并只在 valid时检查 payload；不能把“接口宽度已宏化”
  记为 X/Z 链闭合。
- vector IQ feedback replay mask/index 已进入 interface/xaction/connect，但 monitor X/Z未检查；
  当前 scalar-only范围无需把 replay payload塞入 scalar raw struct，但 `issueVldu`、
  `writebackVldu`、VSTU feedback 三类 valid 均必须在各自 monitor内立即
  `uvm_error`/`uvm_fatal`。VSTU当前在 IQ monitor内直接忽略、没有进入 raw queue，不应误写成
  adapter drop。

`dispatch_raw_ctrl_t.memory_violation_target[49:0]` 当前没有对应 RTL 端口，也未由 monitor
赋值，属于死字段清理候选，不作为 `VADDR_W` 参数需求依据。是否删除应由 raw ctrl/monitor
专项决定，不在参数文件中保留虚假硬件字段。

## 7. 明确不新增宏的项目

| 项目 | 结论和理由 |
|---|---|
| `exceptionVec[23:0]` | `ExceptionVecSize=24` 是异常 cause 集合的固定架构定义，不是 V2 core 配置数量。 |
| `fuOpType[8:0]` | LSU operation encoding 容器为固定语义编码，不随当前 profile 资源数量变化。 |
| trigger `tdata.action[3:0]` | `TriggerAction` 是固定 4-bit 编码，不由 trigger 数量派生；trigger 条目数和 `tUpdate.bits.addr` 仍按第4.5节参数化。 |
| `nf/veew/vsew/vlmul`、`vdIdx` 等 vector 编码字段 | 属于 RISC-V Vector 或固定组织编码；除非 Scala 明确由可变容量派生，否则不宏化。 |
| TileLink `opcode/param/denied/corrupt` | 固定协议编码或 1-bit 状态。TL `size` 容器宽度、data/mask/address/source/sink 仍按第4.4节参数化。 |
| CSR address、SATP/HGATP PPN 容器、PBMT、permission、`s2xlate`、level 编码 | ISA/CSR/MMU 协议布局；distributed CSR write data 明确使用 XLEN，不在本排除项。 |
| SATP/VSATP ASID `[15:0]` CSR 容器 | 固定 CSR 布局，不与 `SFENCE_ID_W` 或 `TLB_ASID_W` 合并。 |
| CSR prefetch control 多 bit 字段 | `active_threshold[3:0]`、`active_stride[5:0]`、`delay_latency[9:0]`、`sbuffer_timeout[21:0]` 是显式 CSR 布局，不是资源数量派生。 |
| L2/L3 sender address `[63:0]` | coupledL2/HuanCun `PrefetchRecv.addr` 明确定义为64-bit bridge字段，不使用 core `PADDR_W`。 |
| trigger `tdata2[63:0]`、`clintTime[63:0]` | 权威 bundle显式固定64 bit，不因当前等于 XLEN而合并。 |
| trace `iretire[6:0]` | 虽由 rename/commit 参数派生，但当前只存在于 `dut_inst.sv` 的展开连接，没有 agent/common/RM consumer；不新增无 consumer 的宏。后续新增 trace agent 时必须补宏。 |
| wakeup/vector/LSQ 编号化物理端口名称 | 端口数量确属配置事实，但宏只能控制公共数组、循环和一致性检查，不能生成或删除 RTL 的命名端口。端口族变化仍需版本 connect adapter。 |
| StoreBuffer/MSHR/release 内部容量 | 当前没有作为 agent/RM 索引表容量使用，不新增死宏。 |
| 主表 64-bit virtual/physical address 容器 | 属于测试框架内部通用容器；只有写入 DUT-facing 字段时按 VAddr/PAddr 宏做显式 fit。 |
| `tb/dut_inst.sv` 展开声明 | 当前 RTL 边界事实，不作为公共测试框架第二权威。 |

## 8. Coding 分组和验收要求

本文不是执行 plan。后续更新对应 flow 执行 plan 时，至少拆为以下 coding 边界：

1. 公共 compile 参数和派生 typedef：新增主宏、派生关系与一致性检查。
2. Scalar 公共字段链：`pdest/uopIdx/numLsElem/VAddr/PAddr` 的声明、临时变量、slice、constraint 和 X/Z width。
3. 端口/能力 consumer：issue、LSQ enqueue、MMIO load、accept response、SQ deq pointer。
4. Vector 字段链：在 vector LS unsupported gate 不变的前提下完成 packed shape 参数化。
5. 公共 PTW/TLB shape：覆盖 ITLB 和 L2TLB agent，遵循 L2TLB responder 规则，不改变
   request/response 方向和 G/U 权限建模。
6. DCache/uncache edge shape：参数化 elaboration width，并覆盖第4.4节全部 BCE、alias、
   user/echo presence capability；不在本项新增 corrupt/denied response 注入行为。

每组 coding 至少检查：

```text
声明宽度
  -> xaction/transaction字段
  -> driver/monitor局部变量
  -> X/Z检查width
  -> constraint/sized literal/slice
  -> raw struct/status/helper
  -> connect映射和compile consistency check
```

验收时不得只搜索 `[7:0]` 等文本并全部替换。必须按字段语义确认固定值已消失，协议固定字段
仍保持原定义，并执行远端 V2 编译；涉及默认运行期行为的 capability 修改还必须执行对应 flow
smoke。

## 9. 多轮 subagent 审计记录

### 9.1 第一轮并行审计

第一轮由三名独立 subagent 分别检查缺失主宏、已有宏 consumer 和边界字段分类。

新增发现：

- 缺失主宏：`PDEST_W`、`UOP_IDX_W`、`NUM_LS_ELEM_W`、VAddr/PAddr、vector shape、
  PTW/TLB shape、DCache/uncache edge shape。
- 应派生宽度：commit count、LQ/SQ cancel count、VLENB、VL width、L2TLB sector index。
- 已有宏 consumer：FTQ fixed slice、旧 FuType constraint、fixed issue/MMIO/LSQ 端口、
  LQ/SQ range constraint、两个 capability 未隔离。

分歧处理：

- `pdest/uopIdx` 不是现有 ROB/LQ/SQ 宏的 consumer，已作为独立语义主宏记录。
- DCache/uncache edge 的 opcode/param 等协议字段排除，但协商得到的 data/mask/source/sink
  作为 elaboration shape 纳入。
- 编号化物理端口不能仅靠宏生成，公共数组/循环参数化与版本 connect 显式映射同时保留。
- cancel/commit count 使用已有 size/count 主宏派生，不新增可独立漂移的第二默认值。

### 9.2 第二轮独立复核

第二轮发现本文仍有新增遗漏和错误分类，因此未通过，并已按 review 结果修订本文。

新增发现：

- `MEMBLOCK_DUT_ENSBUFFER_WIDTH` 及其派生 `MEMBLOCK_SQ_DEQ_COUNT_W`。
- `MEMBLOCK_DUT_DCACHE_L2_HINT_SOURCE_W`、`MEMBLOCK_DUT_HART_ID_W`、
  `MEMBLOCK_DUT_MSI_INFO_W`。
- `MEMBLOCK_DUT_DCACHE_HAS_ALIAS` presence capability。
- `sfence.id` 的固定16-bit consumer被发现；第二轮曾建议公共 ASID 宏，第三轮已进一步
  修正为独立 `MEMBLOCK_DUT_SFENCE_ID_W` 和 `MEMBLOCK_DUT_TLB_ASID_W`。

必须修正：

- 原 L2TLB 私有命名改为公共 PTW/TLB shape，并补 ITLB consumer、S1/S2 公式和
  `addr_low/ppn_low`。
- DCache/uncache mask 改由 data width 派生，`reqSource` 改为公共宏。
- 原“SBuffer edge”表述改为当前 `sbuffer_agent` 实际承接的 uncache/MMIO TL edge。
- 补 VAddr/PAddr consumer并识别 `lsqio_vaddr/gpaddr` 是 XLEN语义；第三轮进一步把 XLEN
  建为独立主宏。
- 固定命名端口未生成化前，所有数量 override 只允许 V2 精确 tuple，其它值必须 fatal。

第二轮因限时未穷举的范围：

- vector issue/writeback、wakeup/int-WB 的全部编号化端口族。
- 可选 L3 prefetch、TEE IMSIC、H-extension presence capability。
- ctrl/CSR agent 中其它配置派生字段。

上述未覆盖范围是第三轮强制 review 输入，不能视为无遗漏。

### 9.3 第三轮分域复核

第三轮由三名全新 subagent 分别检查 vector/WB/wakeup、control/CSR/presence 和
PTW/TLB/DCache/uncache。三路均发现新增项，本轮未通过。

新增主参数或 capability：

- `MEMBLOCK_DUT_XLEN`、`MEMBLOCK_DUT_VLDU_PORT_NUM`、
  `MEMBLOCK_DUT_LD_EXU_PORT_NUM`、`MEMBLOCK_DUT_VLDU_FUTYPE_PORT_MASK`。
- DCache/uncache 各自的 TL address/size width，`MEMBLOCK_DUT_DCACHE_HAS_BCE`。
- `MEMBLOCK_DUT_HAS_H_EXTENSION`、`MEMBLOCK_DUT_HAS_TEE_IMSIC`、L2/L3 prefetch sender
  presence capability。
- `MEMBLOCK_DUT_TRIGGER_NUM` 及 trigger index 派生宽度。

新增 consumer：

- LOAD/STA/STD base/count 还覆盖 int-WB interface/monitor/raw/adapter 固定端口编号。
- 第三轮发现 `ifetchPrefetch_0..2` 和 `ldCancel_0..2` 的固定端口consumer；第五轮进一步
  修正归属为前者消费 `LOAD_PIPE_NUM`、后者消费 `LD_EXU_PORT_NUM`。
- Hart ID 还覆盖 ctrl `topToBackendBypass_hartId`。
- commit count、sfence ID/VAddr 还覆盖 raw/common helper。

必须修正：

- sfence ID、PTW/TLB entry ASID、SATP/VSATP CSR ASID 拆为三种语义，不能共用一个
  `ASID_W`。
- H capability 只控制真正可选的 `PtwEntry.vmid`，不能机械包住无条件存在的 H 相关字段。
- TL `size` 不是固定3-bit容器；TL addressBits 也不能直接复用 core `PADDR_W`。
- L2/L3 sender address 是固定64-bit bridge字段，不使用 `PADDR_W`。
- vector issue port 0/1 字段不完全同构，必须保留显式 adapter/presence mask。

第三轮因限时仍需第四轮闭合的范围：

- DCache `user/echo` BundleField 的 presence 和宽度。
- 各 agent xaction pack/unpack/compare/print、constraint、driver/monitor/XZ 的逐链 consumer。
- vector IQ feedback 和本轮新增 presence capability 的文档内部一致性。
- ITLB/L2TLB sequence、lookup helper 和 transitive raw/common consumer 的最终核对。

### 9.4 第四轮全链复核

第四轮三名全新 subagent 分别闭合 TL user/echo、control/PTW/raw 和
vector/IQ-feedback/xaction 辅助链，仍发现新增项，本轮未通过。

新增参数或 capability：

- DCache `user_vaddr/user_reqSource/user_needHint/echo_isKeyword` presence capability，
  `MEMBLOCK_DUT_UNCACHE_HAS_BCE=0`。
- `MEMBLOCK_DUT_VSTU_PORT_NUM=2`。
- `MAX_UOP_SIZE`、`VSMB_SIZE` 改为主数量；`UOP_IDX_W`、`VSMB_IDX_W` 改为派生宽度；
  `MAX_LS_ELEM/NUM_LS_ELEM_W` 均由 VLEN派生。

新增 consumer或运行期边界：

- XLEN 还覆盖 distributed CSR write data；PDEST 还覆盖 wakeup constraint和 runtime
  base/range clamp；NUM_LS_ELEM 还覆盖多个 sized literal。
- STA count 还覆盖 scalar IQ feedback；VSTU feedback使用独立 port count。
- PTW/TLB sequence、TLB entry和 lookup helper仍有固定 slice；L2/L3 sender capability必须
  保护 xaction automation/compare/print和 monitor X/Z。
- fence address constraint当前无依据地屏蔽 VAddr最高位。
- vector issue/WB X/Z block为空；VSTU feedback当前被 IQ monitor直接忽略，scalar-only范围
  必须在 monitor push前 fail-fast。

必须修正：

- DCache/uncache channel-width consumer矩阵逐 channel记录，不能为不存在的channel补字段。
- DCache user/echo、alias、BCE和 uncache A/D-only capability必须保护完整 agent链。
- CSR address等固定布局可排除，但 distributed CSR data必须消费 XLEN。
- `PADDR_W` 不再把只存在于 `dut_inst.sv` 的 L2 TLB paddr列为公共 consumer。
- 历史 review记录中的单一 `ASID_W` 建议改为已修正的双宏结论。

第四轮 reviewer B 因限时未逐项完成所有 control/PTW xaction辅助函数。第五轮必须定点复核：

- control/PTW/fence/prefetch xaction 的 field automation、pack/unpack/compare/print、constraint。
- 新增 DCache presence capability 与 channel矩阵的文档一致性。
- MAX_UOP/MAX_LS_ELEM/VSMB 主数量和 vector scalar-only fail-fast边界。

### 9.5 第五轮定点复核

第五轮三名全新 subagent 定点检查第四轮未闭合范围，仍发现新增 consumer和分类错误，
本轮未通过。

新增参数依据：

- `pdest` 合法范围不能由 `PDEST_W` 推导；新增 INT/FP/VF/V0/VL 五类 PREG数量主参数，
  `PDEST_W` 改为由最大数量派生，scalar激励按 `rfWen/fpWen` 使用 INT/FP真实范围。

新增 consumer：

- `ldCancel_*` 实际属于 `LD_EXU_PORT_NUM`，不是 `LOAD_PIPE_NUM`。
- TLB sector count还控制 `valididx_0..7` 和 `pteidx_0..7`。
- `teemsiInfo.bits` 与普通 MSI payload共同消费 `MSI_INFO_W`。
- fence、XLEN、distributed CSR data、trigger、PTW/TLB、count字段的 automation/constraint/
  print/compare/driver/monitor/XZ/connect需要逐链列明。

必须修正：

- lookup key/runtime snapshot中的16-bit软件/CSR ASID/VMID容器不作为14-bit TLB entry VMID
  的声明 consumer，只在 entry fit/slice边界转换。
- vector scalar-only边界覆盖 `issueVldu/writebackVldu/VSTU feedback` 三类 valid；VSTU当前
  由 monitor直接忽略，不是 adapter drop。
- DCache coding分组必须覆盖全部 BCE/alias/user/echo和 uncache BCE capability，不能只写
  alias presence。

第五轮三个定点范围均已声明完整覆盖。第六轮应作为全篇独立一致性复核，重点确认上述修订
没有引入新矛盾；若无新增和必须修复项，才可通过。

### 9.6 第六轮全篇与反向扫描复核

第六轮由一名 reviewer做全篇一致性检查，另一名 reviewer反向扫描 238 个 SV/SVH和
22 个 connect文件。两者均完整覆盖指定范围，但仍发现新增项，本轮未通过。

新增参数或拓扑：

- vector `flowNum` 具有独立 `maxFlowNum=16` 语义上限；第六轮曾误认为其 packed width也
  独立派生，第七轮已修正为顶层字段宽度继续消费 `NUM_LS_ELEM_W`。
- int-sink CLINT/PLIC/NMI/BEU 的 port/sink elaboration tuple进入 compile参数和精确
  V2 tuple；无 agent consumer的 debug sink不新增死宏。

新增 consumer和必须修正：

- `mem_base_sequence.sv` 的 TL address/data/mask/size、slice和 byte literal纳入
  DCache/uncache shape consumer。
- `TLB_ASID_W` 只覆盖 response/entry，`PtwReq` request不携带 ASID。
- exact tuple补 `LSQ_ENQ_HAS_ACCEPT_RESP=0` 和 `HAS_SQ_DEQ_PTR=0`。

第六轮反向扫描范围已经覆盖全部现存 agent/common/seq SV/SVH和 connect文件，排除
`dut_inst.sv`；仓库当前不存在 `mem_ut/ver/ut/memblock/subagent` 目录。第七轮只需独立复核
上述修订和全篇内部一致性，不再扩展扫描范围。

### 9.7 第七轮修订复核

第七轮两名全新 subagent 复核第六轮修订和全文一致性，发现两项语义归类错误及一个遗漏
consumer，本轮未通过。

必须修正：

- 顶层 `MemExuInput.flowNum` 的 packed类型是 `NumLsElem()`，因此字段宽度消费
  `NUM_LS_ELEM_W`；独立 `MAX_FLOW_NUM` 只约束合法 flow语义范围，不新增 DUT-facing
  width宏。
- `IntSinkPortSimple` tuple第一维语义是 port，不是 source；宏和精确 tuple统一改为
  `INT_PORT/SINK_NUM`。
- scalar issue LDA/STA/STD `src_0` 是 XLEN consumer，补 interface/xaction/driver/monitor/
  XZ/connect和 `issue_field_assigner` fit边界。

两名 reviewer均确认 `mem_base_sequence` TL consumer、TLB ASID response-only语义、LSQ两个
capability exact tuple和其余 V2值没有新增问题。第八轮只需复核上述三项修订及全文一致性。

### 9.8 第八轮一致性复核

第八轮两名全新 subagent 只检查第七轮三项修订和全文一致性，发现一项真实 consumer总表
残留和一项需由主 agent结合权威源码裁决的公式意见，本轮未通过。

已修正：

- consumer总表不再把独立 flow width字段当作 packed width权威；明确 `flowNum` 宽度消费
  `NUM_LS_ELEM_W`，合法范围消费 `MAX_FLOW_NUM`，不建立独立 DUT-facing width宏。

主 agent源码裁决：

- reviewer建议把 `UOP_IDX_W` 改为 `$clog2(MAX_UOP_SIZE)`，但权威
  `backend/Bundles.scala::UopIdx` 明确使用 `log2Up(MaxUopSize+1)`。文档保留 DUT原式，
  同时把测试框架合法 stimulus范围独立写为 `0..MaxUopSize-1`。不能为了当前结果同为7 bit
  而改写权威 packed width公式。

interrupt PORT/SINK tuple和 scalar issue `src_0` XLEN链均通过第八轮复核。第九轮必须由
全新 subagent确认 flow consumer修订和 UopIdx源码裁决后，方可结束。

### 9.9 第九轮最终签署复核

第九轮两名全新 subagent 定点复核 UopIdx源码裁决、flowNum主从关系和全文一致性。

- 一名 reviewer确认无新增项且无必须修复项，技术 review通过。
- 另一名 reviewer确认 UopIdx和flowNum技术结论正确，但要求历史说明也不得保留被否定的
  旧独立 width宏字面名称。

本轮已删除该旧名称的三处否定性历史引用；技术参数集合、默认值、派生关系和 consumer均未
变化。第十轮由全新 subagent确认全文无该旧名称且无其它必须修复项后，方可最终结束。

### 9.10 第十轮零残留签署

第十轮全新 subagent确认 flowNum和UopIdx技术结论均正确，仅发现第八轮历史说明仍保留一处
被否定的独立 flow width简称。本轮已删除该简称，技术参数集合和 consumer未变化。

第十一轮只检查全文零残留、状态和最终结论；无新增且无必须修复项后才可结束。

### 9.11 第十一轮最终签署

第十一轮由全新 subagent做最终零残留只读签署，结论原文为：

```text
本轮无新增项且无必须修复项，review通过
```

该轮确认被否定的旧独立 flow width名称全文零残留，flowNum和UopIdx最终技术结论正确，
没有其它技术矛盾。

### 9.12 结束条件

每当独立复核发现本文遗漏或错误分类时，必须先更新本文，再启动一名全新 subagent 复核。
只有最后一轮同时满足以下条件才可结束：

1. 没有新增必须参数化字段或已有宏 consumer。
2. 已列字段的配置来源、V2 默认值和 consumer 分类正确。
3. 协议固定字段、运行期状态和 `dut_inst.sv` 边界没有被误列为主宏。
4. 最后一轮 review 明确给出“无必须修复项”。

当前结论：以上四项全部满足，第十一轮 review通过，审计结束。
