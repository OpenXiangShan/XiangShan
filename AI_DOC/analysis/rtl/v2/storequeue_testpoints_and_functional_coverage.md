# V2 StoreQueue 测试点与功能覆盖模型

## 版本元数据

| 项目 | 内容 |
|---|---|
| RTL 版本 | V2 |
| 分支 | `mem_ut_uvm_v2` |
| 核验 commit | `1567628320ef77e1de1a3ae7a7c7057423e842b4` |
| 接口权威 | 工作区现存 `build/rtl/StoreQueue.sv`。该生成产物未单独记录生成 commit，因此端口存在性、方向和位宽以它为准。 |
| 行为权威 | `StoreQueue.scala`、`StoreQueueData.scala`、`StoreMisalignBuffer.scala`、`LSQWrapper.scala`、`MemBlock.scala`、DCache `CMOUnit/MainPipe` 与 CoupledL2 `SinkC/MSHR/MainPipe`。 |
| 最后核验日期 | 2026-09-15 |

## 范围和使用方法

本文把 StoreQueue 的验证需求分成两类：

- **接口组合触发**：从真实 `StoreQueue.sv` 的输入组合、握手和可见输出构造场景，适合 standalone black-box 或接口 agent。
- **内部逻辑触发**：从 Scala 的 entry 状态、指针、FSM、优先级和跨模块条件构造场景，适合 full MemBlock 仿真、bind assertion 或层次化 functional coverage。

本文不实现 SV `covergroup`、sequence、driver、monitor 或 RM。每个“功能覆盖检测点”定义未来 coverage model 应采样的事件、coverpoint/bin/cross 和判定边界。它不能被误读为“只要 signal 翻转过就覆盖”；必须同时满足该行的前置条件和可观察结果。

### 测试分类

| 分类 | 含义 | 覆盖处理 |
|---|---|---|
| 正向功能 | 当前 V2 RTL 支持、且上游协议允许的事务。 | 计入功能覆盖 closure。 |
| protocol-negative | 接口电气上可驱动、但违反 owner、generation、时序或握手约束的组合。 | 用 assertion/error bin 监测；不计为正向功能覆盖。 |
| 已知 RTL 缺陷签名 | 已由专项分析确认的当前实现行为，和期望架构行为不同。 | 独立 bug-regression bin；不得拿它填充正向功能 bin。 |
| 源码观察边界 | 当前源码可确定的行为，但尚未由架构规格或 full-core 波形确认其应当被视为设计缺陷。 | 单列 watch/fail 检测点；不计为正向功能覆盖，也不能被成功路径覆盖掩盖。 |
| 未支持边界 | 当前源码明确 TODO/未实现的路径。 | 明确 exclude 或 fail-fast；不得伪造正向通过。 |

### 统一采样规则

1. Valid-only 输入在 `valid=1` 的时钟沿采样；Decoupled 输出只在 `valid && ready`（`fire`）采样。
2. `forward` 的结果固定对应前一拍同 port 的 query；没有 query `valid` 时，输出没有新事务含义。
3. `storeAddrInRe` 没有 `valid` 或 `sqIdx`，只能作为前一拍同 lane、`storeAddrIn.valid && !miss` 的 S2 owner；coverage model 必须保存该一拍 token。
4. `sqPtr/uop` 等无 valid sideband 只在其所属事件或已知状态下采样，不能逐拍当作独立请求。
5. SQ entry 的地址、数据、mask 输入没有 generation flag；模型必须维护 live `{sqPtr.flag,value}`，迟到回写或重用 slot 覆盖都归 protocol-negative。

## 1. 接口组合触发的测试点与功能覆盖模型

### 1.1 分配、复位与 Redirect

| ID | 接口组合与前置条件 | 期望功能结果 | 功能覆盖检测点 |
|---|---|---|---|
| IF-01 | reset 释放后无有效输入。 | 主 entry/pointer 状态回到空队列；延迟观测输出在规定寄存级数后稳定。主状态使用高有效异步复位，但 `sqEmpty/sqDeq/perf` 等部分观测寄存器仍需等待时钟边界。 | `cp_reset_release`；cross `reset_release_cycle x sqEmpty`，禁止 reset 当拍比较一般 sideband。 |
| IF-02 | 一路 `enq_req.valid`，`numLsElem=1`，合法 live `sqIdx`。 | 一个 entry 进入 allocated，地址/数据/提交状态重新初始化。 | `cp_enq_width={1}`、`cp_scalar_vector={scalar}`、`cp_sq_wrap={no_wrap}`。 |
| IF-03 | 多个合法 enqueue slot 同拍，包含相邻 scalar 与 vector `numLsElem>1`。 | 每个请求按已分配连续 SQ 范围初始化，vector 的最后物理 flow 才得到 `vecLastFlow`。 | `cp_enq_slots`、`cp_numLsElem`、`cp_multi_req_same_cycle`；cross `numLsElem x isVector x wrap`。 |
| IF-04 | `sqIdx + numLsElem` 跨 56-entry 环回。 | 低/高两段物理槽均被正确初始化，flag 表示新 generation。 | `cp_sq_wrap={wrap}`；检查 allocated bitmap、entry identity 和后续 forward 年龄。 |
| IF-05 | 队列接近容量上限，分别在 `sqFull=0/1`、`force_write` 上下阈值附近 enqueue。 | `sqFull` 反映容量门限，`force_write` 为一拍延迟滞回提示；二者不可强行等同。 | `cp_occupancy_bucket`、`cp_sqFull`、`cp_force_write_transition`；cross `occupancy x force_write`。 |
| IF-06 | T0 `brqRedirect.valid=1`，同时存在年轻未提交 entry；观察 T1/T2。 | 命中的未提交 entry 被取消，`sqCancelCnt` 在 T2 反映取消数，tail 在恢复阶段回退。 | `cp_redirect={none,with_cancel}`、`cp_cancel_count`、`cp_redirect_phase={T0,T1,T2}`。 |
| IF-07 | redirect 与 enqueue 同拍、以及 redirect 后 T1 有 enqueue。 | 同拍被 flush 的请求不形成 live entry；T1 不能依赖新的 enqueue 量推进 tail。 | protocol-negative assertion：`lastCycleRedirect && enqNumber!=0`；正向 bin 仅采合法暂停序列。 |

### 1.2 地址 S1/S2、数据和 mask 到达次序

| ID | 接口组合与前置条件 | 期望功能结果 | 功能覆盖检测点 |
|---|---|---|---|
| IF-08 | 四类合法 non-miss S1：普通/MAB 回流各自分别取 `updateAddrValid=1` 和 `0`。普通 S1 还要求 `!isFrmMisAlignBuf`，MAB 回流取 `isFrmMisAlignBuf`。 | `updateAddrValid=1` 时，普通 S1 写 VAddr/PAddr CAM、非对齐状态、`addrvalid/nc`；MAB 回流仍可更新 uop、`addrvalid/nc`，但抑制普通 CAM 和 `unaligned/cross16Byte` 覆盖。`updateAddrValid=0` 也是合法 S1：普通项仍可写 CAM/非对齐状态，却不得置/更新 `addrvalid/nc`；MAB 回流仍抑制普通 CAM，也不得置/更新 `addrvalid/nc`。 | `cp_addr_s1={normal_ready,mab_ready,normal_not_ready,mab_not_ready,miss}`；cross `s1_kind x CAM_write x addrvalid_write x nc_write x isMisalign`。 |
| IF-08a | 非 vector、`storeAddrIn.valid && !miss` 的 S1 携带非零 `StaCfg.exceptionVec`，并分别取 `updateAddrValid=0/1`。 | S1 的 ExceptionBuffer source 只要求 `valid && !miss && !isvec`，不要求 `updateAddrValid=1`；因此两种取值都应能形成 S1 异常候选。`updateAddrValid=0` 时 entry 仍不应被标成普通 address-ready，S2 仍需由独立合法 Re token 驱动分类。 | `cp_s1_exception_update_addr={0,1}`；cross `exception_bit x updateAddrValid x exceptionAddr_source x addrvalid_state`。 |
| IF-09 | S1 `valid && !miss` 后，T+1 同 lane `storeAddrInRe.updateAddrValid=1`，分别为普通、`mmio=1`、`hasException=1`。 | 用前拍 owner 更新 `pending/mmio/memBackTypeMM/hasException/waitStoreS2`；exception 可使地址完成。 | `cp_s2_class={normal,mmio,exception}`、`cp_s1_s2_pair`；cross `s2_class x data_arrival_order`。 |
| IF-10 | S1 `miss=1` 或 S2 无合法 owner 却拉高 `updateAddrValid`。 | 不得作为正常 S1->S2 事务覆盖；后者由 adapter/assertion 报 protocol-negative。 | `cp_s1_miss`；`illegal_bins no_s1_owner_for_re`。 |
| IF-11 | data 先到、mask 后到；再反向 mask 先到、data 后到。 | data 和 byte mask 独立写入，`datavalid` 只等待 data 写入流水，并**不会**等待最终 mask 收敛。正向 driver/RM 必须在 entry 被 forwarding 或 SBuffer 观察前，让最终 mask 与每个有效 byte 的 data 都已收敛；这是一条测试输入约束，不是 RTL 自动阻塞条件。若特意施加 pin-level 压力使二者失步，checker 必须按当拍可见的旧 data/mask 预测，不得假定 DUT 会自动给出 blocker。 | 正向 `cp_data_mask_order={data_first,mask_first,same_cycle}` 只在观察前已收敛时计入；另设 `cp_data_mask_skew_observation` 压力 bin，cross `order x forward_or_sbuffer_observation`，不填正向 completion bin。 |
| IF-12 | scalar SB/SH/SW/SD 与 vector data，分别驱动 `storeDataIn`。 | scalar 数据按 access size 复制规范化，vector 保留 128-bit 原样。 | `cp_data_format={b,h,w,d,vector}`；每种格式采样 data pattern 和 byte mask。 |
| IF-13 | `fuOpType=cbo_zero` 的 data 输入。 | SQ 保存全零数据，后续由 CBO.zero 路径决定 SBuffer 或 uncache 处理。 | `cp_cbo_type={zero}`；cross `cbo_zero x memBackTypeMM`。 |
| IF-14 | 同类双 data port 或双 mask port 同拍写同一 live entry。 | 不是合法正向场景；应触发 driver 约束/RTL 冲突 assertion。 | `illegal_bins same_entry_two_data_writers`、`same_entry_two_mask_writers`。 |

### 1.3 Store-to-load forwarding 查询

| ID | 接口组合与前置条件 | 期望功能结果 | 功能覆盖检测点 |
|---|---|---|---|
| IF-15 | `forward[q].valid=1`，且查询年龄集合中没有会触发 blocker 的 entry：strict 模式下所有 `needForward` entry 均已 address-ready；同时没有地址匹配但 data 未 ready、allocated unaligned 或 V/P CAM 不一致候选。 | T+1 无有效 forward byte，三个 invalid 标志均按合法无 blocker 解释。仅“无更老重叠 store”不足以保证 strict `addrInvalid=0`，因为 strict 会检查整个 `needForward` 窗口。 | `cp_forward_result={no_match}`；按 `q=0..2` 全覆盖。 |
| IF-15a | 合法 forward query：令 `sqIdxMask=UIntToMask(uop.sqIdx.value,56)`，即 `[0,uop.sqIdx.value)` 为连续 1；并令独立 `forward.sqIdx.flag == uop.sqIdx.flag`。分别覆盖不跨环与跨环年龄窗口。 | SQ 以该 prefix mask 与 `deqPtr.flag` 形成连续的“更老 store”窗口；它不是可任意稀疏挑选的 56-bit candidate set。当前顶层的独立 query SQ anchor 没有 value，边界 value 由 `uop.sqIdx.value` 提供。 | `cp_forward_age_mask_shape={prefix_same_flag,prefix_wrap_flag}`；cross `mask_shape x deq_flag_relation x q`。另设 `illegal_bins sparse_sqIdxMask`、`sqIdx_flag_uop_flag_mismatch`，不计正向 forwarding 覆盖。 |
| IF-16 | 一个地址/数据均 ready 的更老 store 与 query 有 byte overlap。 | T+1 对重叠 query byte 返回正确 `forwardMask/forwardData`。 | `cp_forward_byte_count={1,partial,all}`；cross `store_size x load_mask_shape x q`。 |
| IF-17 | 多个更老 store 覆盖不同 byte 或同一 byte。 | 每个 byte 选择正确的最年轻匹配 store；不同 byte 可来自不同 entry。 | `cp_forward_source_count`、`cp_per_byte_source_mixed`；cross `overlap_kind x sq_wrap`。 |
| IF-18 | 地址已匹配但 data 尚未 ready，或候选中有 allocated unaligned entry。 | `dataInvalid=1`，相应 `dataInvalidSqIdx` 只在 invalid 时作为 blocker。 | `cp_data_invalid_cause={data_late,unaligned}`；cross `cause x strict_mode`。 |
| IF-19 | `loadWaitStrict=0`，`loadWaitBit=1` 且 `waitForRobIdx` 指向地址未完成 store。 | `addrInvalid=1`，blocker 反映 LFST 等待关系。 | `cp_addr_invalid_cause={lfst}`；cross `loadWaitBit x wait_store_ready`。 |
| IF-20 | `loadWaitStrict=1`，候选年龄窗口含任一地址未完成更老 store。 | `addrInvalid=1`，即使其不等于 `waitForRobIdx`。 | `cp_addr_invalid_cause={strict_window}`；cross `strict x candidate_count`。 |
| IF-21 | VAddr/PAddr CAM 相关候选集合不一致。 | T+1 `matchInvalid=1`；普通 forwarding 数据不作为可信架构结果。 | `cp_match_invalid`；cross `match_invalid x addr_invalid x data_invalid`，只纳入源码允许的组合。 |
| IF-22 | 连续两拍向同一 forward port 发不同 query。 | T+1/T+2 分别对应前一拍 query，不串线。 | `cp_forward_pipeline_back_to_back`；scoreboard token match。 |

### 1.4 可见下发、uncache、CMO 和 MAB 协作接口

| ID | 接口组合与前置条件 | 期望功能结果 | 功能覆盖检测点 |
|---|---|---|---|
| IF-23 | 对齐、cacheable、scalar entry 地址/数据 ready，ROB 允许提交，SBuffer ready。 | SQ 经 DataBuffer 向 `sbuffer` 交付，随后完成并释放 entry。`prefetch` 等 Scala 内部属性不在当前 `build/rtl/StoreQueue.sv` 顶层可见；若需覆盖只能作为 source/full-MemBlock proxy，不能作为 standalone 接口 cross。 | `cp_sink={sbuffer}`；cross `sbuffer_lane_count x store_size`；另设 source-level `prefetch` watcher（不计 pin-level 正向 bin）。 |
| IF-24 | 两条可连续下发的正常 store；覆盖 SBuffer/DataBuffer 的前缀接收和双 lane fire。 | `DatamoduleResultBuffer` 同时保证 `valid(1) => valid(0)` 与 `ready(1) => ready(0)`；正向 responder 只驱动 `00/10/11`，分别表示不接收、仅接收 lane0、同时接收两 lane。`ready=01` 违反已生成的 DataBuffer dequeue 前缀合同，不能作为普通正向组合。 | 正向 `cp_sbuffer_ready_prefix={00,10,11}`、`cp_sbuffer_fire_pattern={none,lane0_only,both}`；`illegal_bins ready1_without_ready0`、`valid1_without_valid0`、`fire1_without_fire0`。 |
| IF-25 | scalar NC store 已提交、地址/数据 ready，分别使用 non-outstanding 与 outstanding uncache 配置；outstanding 额外连续发出至少两笔 NC。 | 进入 NC request/ack/response 生命周期；non-outstanding 在最终 `resp` 完成，outstanding 在与各 request fire 正确关联的 `idResp.nc`/`mid` 到达时完成。当前 `idResp` 按 request 接纳顺序产生；最终无 ID 的 `resp` 由 Uncache buffer 仲裁，可能晚于或不同于请求顺序返回，顺序差异本身不判错，但错误 owner/mid/generation/phase 必须报错。两种模式的 denied/corrupt 都不转换为 SQ/ROB store exception。只有 Uncache 收到并 **实际 fire 的 grant 携带 `denied` 或 `corrupt`** 时才产生 `busError.ecc_error`，再经 MemBlock 两拍延迟和 `cache_error_enable` 门控形成 BEU `uncacheError`；clean grant 不产生该 bus-error，且 gate=0 时必须覆盖 `BEU_seen=0`。outstanding 模式中第一笔收到 ack 后 SQ 可继续发第二笔，多个 `mid` 必须分别关联各自 entry；第一笔较晚到达的最终 error response 不应被错误送回 SQ 或完成第二笔。 | `cp_sink={nc_uncache}`、`cp_nc_mode={wait_resp,outstanding}`；cross `mode x completion_trigger={resp,ack} x grant_error={clean,denied,corrupt,both} x BEU_seen`，并覆盖 `owner_set_depth={1,2+}`、多个 `mid` 的正确关联/环回及最终 response 的允许顺序；错误 `mid`、错误 owner/generation/phase 归 IF-38 protocol-negative。 |
| IF-26 | scalar MMIO store 到 ROB pending owner，且当前 `cmoOpResp.bits.denied/corrupt=0`；uncache request/response/`mmioStout` 依次 fire。 | 严格串行的 MMIO FSM 完成一次 request、response、writeback、`scommit` 释放。此正向 row 只覆盖本次 `io_uncache.resp.fire && !nc` 自己的 error bits：own `denied` 覆盖 own `corrupt`；若 CMO sideband 残留 error，则转入 stale-error watcher，不能填该正向 bin。 | `cp_sink={mmio}`、`cp_mmio_fsm_phase`；cross `own_uncache_error={ok,denied_only,corrupt_only,both} x cmo_sideband_clean x writeback_ready`。 |
| IF-27 | non-zero CBO，`memBackTypeMM=1`，SBuffer 已清空，且 `cmoOpResp.fire` 的 `denied=0/corrupt=0`。 | 走 `cmoOpReq.fire -> cmoOpResp.fire -> mmioState.s_wb -> mmioStout.fire -> s_wait`，而不是普通 data sink。带 error 的 CMO response 不填入本正向场景，改由内部异常边界检测。 | `cp_cbo_type={clean,flush,inval}`、`cp_cmo_phase`；cross `cbo_opcode x cmo_success_response x mmioStout_to_swait`。 |
| IF-28 | CBO.zero 分别为 main-memory-backed 与 I/O-backed。 | 前者走 wline/SBuffer flush 与 `cboZeroStout`；后者以多个 uncache 8-byte beat 处理并经 MMIO writeback。 | `cp_cbo_zero_path={sbuffer,uncache}`；cross `path x flush_sbuffer_needed`。 |
| IF-29 | MAB 正常跨页项使 `crossPageWithHit=1`，先令 `crossPageCanDeq=0`，再置 1；另覆盖 MAB 已/未 ready 但当前 `rdataPtrExt(0)` 未命中 parent 的组合。 | 命中且未 ready 时只阻止该 parent 的正常 pair，低 lane 不能先入 DataBuffer，`doDeq=0`；命中且 ready 后 high lane 使用 MAB `paddr`，双 lane 接收后 `doDeq=1`。未命中时当前 entry 仍按自身规则处理，不能取 MAB `paddr`。`hasException` 旁路另归 no-write drain，不填正常 pair bin。 | `cp_mab_phase={unmatched,matched_wait,can_deq,do_deq}`；cross `crossPageWithHit x crossPageCanDeq x dataBuffer_ready x hasException`；断言 `matched_wait -> !normal_pair_valid && !doDeq`，并只在两个 MAB 信号均为 1 时采样 high PAddr。 |
| IF-30 | vector 跨页 MAB 项，`withSamePtr=1`。 | SQ 将当前 rdata head 的 `vecMbCommit` 置位，随后按 vector 条件 drain；该信号不是高地址选择或最终 deq。 | `cp_mab_vector_bridge`；cross `isVector x withSamePtr x vecMbCommit`。 |
| IF-31 | `wfiReq=1`，分别处于无 pending、MMIO/CMO pending、NC outstanding。 | `wfiSafe` 只覆盖 SQ 的 `noPending` 一拍延迟许可；不能被当作全 SQ 或所有 NC 已空。 | `cp_wfi_safe={grant,blocked}`；cross `wfi_state x mmio_state`。 |
| IF-32 | 在 full-MemBlock 路径中，vector store 的 S2 地址分类实际落到 NC 或 MMIO 地址。 | 两者先由 StoreUnit 转成 `storeAccessFault`，而不是 SQ 正常 vector NC/MMIO 下发：`vecmmioStout` 必须无效、normal `ncState` 不得请求。实际 MMIO 的 S2 `mmio` 会被 exception 抑制，matching vector feedback 后可走 `vecValid=0` 的 no-write drain；实际 NC 的 `nc` 已在 S1 写入且不被 S2 exception 覆盖，matching feedback/commit 后走 `nc && hasException` direct-complete，**不**经 DataBuffer/SBuffer，也不要求 `vecExceptionFlag` 置位。 | `cp_vec_uncache_handling={mmio_to_SAF_drain,nc_to_SAF_direct_complete}`；assertion `!normal_nc_req && !vecmmioStout_valid`；cross `actual_mem_class x terminal_path x vector_feedback_kind`。 |
| IF-33 | 已提交的 scalar cacheable entry 已有 `hasException=1`，DataBuffer/SBuffer 接收端 ready。 | `sbuffer.fire` 可以发生以完成 SQ drain，但 payload `vecValid=0`；full-MemBlock 中 SBuffer 的实际 `writeReq.valid` 必须为 0，随后 SQ 才 completed/deq。不能把这次 handshake 计成真实 store 写。 | `cp_scalar_exception_drain={sbuffer_fire_vecinvalid}`；cross `exception_origin x drain_fire x no_real_sbuffer_write`。 |
| IF-34 | 构造地址已到/数据未到、`mmio=1`、vector `vecMbCommit=1`、unaligned 及连续超过四项 ready 的 entry 区间；分别在 redirect 前后观察 `stAddrReadyVec/SqPtr` 与 `stDataReadyVec/SqPtr`。 | 两组 Vec 与 pointer 要遵守各自条件：`mmio` 只在数据前沿的 `addrvalid` 分支替代 `datavalid`；vector 是独立旁路；data 前沿不跨 unaligned。pointer 每拍最多扫描四项且 redirect 可回退，不能把任一 pointer 当实际 memory completion。 | `cp_ready_frontier_visible={addr,data}`；cross `entry_kind x ready_reason x ptr_step={0,1,2,3,4} x redirect`。 |
| IF-35 | 已知 S1/S2、vector FLUSH 或 `mmioStout.fire` 的 exception source，分别观察 `io_exceptionAddr_*`；另覆盖 Dmode-only。 | `exceptionAddr` 没有 valid，只能通过 source token/后端异常生命周期采样。七个 eligible source 选择最老 `{robIdx,uopIdx}`；Dmode-only 不得要求新地址；MMIO source 只检查明确构造的 VA、`vaNeedExt` 与 uop identity。 | `cp_exceptionAddr_source`；cross `source x selected_oldest x redirect_phase x metadata_shape`；illegal bin `Dmode_only_requires_exceptionAddr`。 |
| IF-36 | non-zero CBO 等待 SBuffer 变空，及 main-memory-backed CBO.zero 经 SBuffer fire 后等待变空。 | `flushSbuffer.valid` 是状态请求而非 payload；只有相应 empty 确认后才允许 CMO request 或 `cboZeroStout` 进入后续阶段。I/O-backed CBO.zero 不得错误等待 `cboZeroStout`。 | `cp_flush_sbuffer_owner={cmo,cbo_zero_MM}`；cross `owner x empty_delay x next_sink`；illegal bin `cbo_zero_IO_waits_cboZeroStout`。 |
| IF-37 | 依次触发 MMIO active、non-NC uncache request fire、writeback fire/stall 与 4 个 occupancy 区间。 | `perf[0..7]` 是寄存后的单周期事件/状态观察，非累计计数器；`sqEmpty`、`sqDeq`、`force_write` 也各有寄存边界，不能与同拍内部 fire 或 head type 强配对。 | `cp_perf_index`；cross `perf_event x delayed_observation`；`cp_sq_status_delay={empty,deq,force_write}`。 |
| IF-38 | 在无相应 active token、MMIO/NC phase 错配、错误 `idResp.mid`、已回收代次和 `uncacheOutstanding` 中途切换时注入 `idResp/resp`。outstanding 模式还覆盖“SQ FSM 已回 idle、但旧 NC token 仍在下游”的迟到 final response。 | 这些输入在电气上可驱动，但不满足 Uncache response 合同；不过 MMIO/I/O-backed CBO.zero 的 `idResp.nc=0` 是合法接收确认，SQ 应忽略且不推进 NC 状态。SQ 顶层没有完整 owner/generation 校验，不能把静默状态变化当作正常完成。adapter/RM 应先按 `activeNcSet` 判断：有旧 token 的迟到 NC response 可被下游消化但不再驱动 SQ completion；找不到 token 才报 protocol-negative。正向覆盖只接受 request fire 建立的合法 owner，且 outstanding 模式按 `mid` 维护可并存 NC owner。 | `cp_response_phase`、`cp_error_owner`；illegal bins `resp_without_token`、`mmio_nc_phase_mismatch`、`wrong_mid`、`stale_generation`、`outstanding_mode_toggle`；另设 `cp_mmio_idresp_nc0_ignored` 正向旁路 bin；cross `sq_fsm_state x token_set_depth x response_phase`。 |

### 1.5 接口组合 coverage model 的收敛要求

功能覆盖模型至少包含下列采样对象；其中所有 cross 都先加合法性约束，禁止把不支持/违反协议的
组合计入 hit：

```text
cp_instruction_kind = scalar / vector
cp_alignment_kind   = aligned / unaligned-within-16B / unaligned-cross-16B
cp_cross_page       = no / yes                 // yes 合法时必须同时是 cross-16B
cp_mem_class        = cacheable / NC / MMIO / CBO-nonzero / CBO-zero-MM / CBO-zero-IO
cp_addr_data_state  = addr-only / data-only / both-ready / S2-exception
cp_completion_sink  = SBuffer-real-write / NC-uncache / MMIO-writeback / CMO /
                      CBO-zero-WB / exception-drain-no-write / NC-direct-complete /
                      MAB-scalar-direct-ROB / MAB-vector-feedback
cp_forward_outcome  = no-match / byte-forward / dataInvalid / addrInvalid-LFST /
                      addrInvalid-strict / matchInvalid
cp_pointer_event    = enqueue / commit / rdata-advance / deq / redirect-cancel / wrap
cp_exception_origin = S1-STA / S2-STA / S2-Dmode-only / uncache-denied /
                      uncache-corrupt / vector-feedback / vector-uncache-conversion /
                      MAB-scalar-child / MAB-vector-child
cp_exception_shape  = none / one-eligible-bit / source-reachable-multi-bit / Dmode-only
cp_s2_exception_encoding = Re-nonSAF-bit / af-SAF / Dmode-no-ExBuf /
                           raw-bit7-overridden-source-watch
cp_exception_end    = exceptionAddr / mmioStout / scalar-drain-no-write /
                      NC-direct-complete / MAB-scalar-direct-ROB /
                      MAB-vector-feedback / uncache-BEU / ROB-debug-trap
cp_exbuf_source     = STA-S1-0 / STA-S1-1 / STA-S2-0 / STA-S2-1 /
                      vec-FLUSH-0 / vec-FLUSH-1 / mmioStout
cp_exbuf_arbitration= single / same-cycle / adjacent-cycle / same-ROB-different-uopIdx /
                      held-vs-new-older / held-vs-new-younger /
                      held-vs-new-same-identity
cp_exbuf_redirect   = producer-cycle / S2-buffered-cycle / held-request-cycle
cp_error_boundary   = cmo-error-not-propagated / stale-cmo-error-leak /
                      nc-error-not-to-ROB-exception / MAB-MMIO-to-SAF /
                      MAB-NC-to-SAM / MAB-trigger-metadata-defect /
                      MAB-scalar-Dmode-observation /
                      MAB-vector-Dmode-observation / vector-Dmode-to-commit-watch /
                      SQ-special-writeback-trigger-not-pin-visible /
                      CMO-address-nderr-not-pin-visible / uncache-response-owner-phase /
                      NC-BEU-latency-gate
cp_response_phase    = no-owner / MMIO-owner / MMIO-idResp-nc0-ignored /
                        NC-owner / CMO-owner / stale-generation
cp_error_owner       = own-response / stale-CMO / mixed-own-stale / none
```

必需正向 cross 为：

```text
instruction_kind x alignment_kind x cross_page x mem_class x completion_sink
instruction_kind x addr_data_state x pointer_event
forward_outcome x loadWaitStrict x sq-wrap-condition
exception_origin x exception_shape x exception_end x redirect-or-commit-resolution
exbuf_source x exbuf_arbitration x exbuf_redirect x selected-oldest-token
cp_s2_exception_encoding x hasException x exceptionAddr-or-no-address
MAB phase x instruction_kind(vector/scalar) x DataBuffer dual-lane acceptance
response_phase x error_owner x completion_or_protocol_negative
```

coverage model 必须把下列组合显式排除在**正向完成** cross 之外：

```text
ignore_bins normal_vector_mmio_or_nc =
  instruction_kind=vector && mem_class in {NC, MMIO} &&
  completion_sink in {NC-uncache, MMIO-writeback}

ignore_bins normal_nc_cross16 =
  instruction_kind=scalar && mem_class=NC && alignment_kind=unaligned-cross-16B &&
  completion_sink=NC-uncache

illegal_bins cross_page_without_cross16 =
  cross_page=yes && alignment_kind != unaligned-cross-16B
```

第一条不排除 **vector uncache 被 StoreUnit 转换为 `storeAccessFault`** 的异常场景；实际 MMIO 应以
`exception_origin=vector-uncache-conversion`、`completion_sink=exception-drain-no-write` 计入正向异常功能，
实际 NC 则以 `completion_sink=NC-direct-complete` 计入正向异常功能。第二条只从**标量正常 NC-uncache
完成** coverage 排除已知 `NC && unaligned && cross16Byte` 缺陷；vector NC direct-complete、MAB 异常终点
和其他异常 sink 不被该 ignore bin 误删，相同缺陷组合另设独立 bug-regression coverpoint。
`MAB-vector-child`、MAB child 身份、高页 child 地址、SBuffer 的实际 `writeReq` 与 Uncache 的 BEU 不是
standalone StoreQueue 顶层的直接可见信号，必须标为 full-MemBlock 的层次采样或 writeback proxy，不能伪装成
纯 black-box 的直接 bin。

`cp_error_boundary` 与其相关 cross 是 watcher/bug-regression，不属于上述正向 cross。特别是 CMO
response 的 error bits、NC response 的 error bits、MAB actual-MMIO/NC 转换和两类 trigger metadata 都不能
用来增加正常 CMO、NC 或 MAB completion 的覆盖率；它们只验证当前源码的实际边界签名。

`SQ-special-writeback-trigger-not-pin-visible` 只允许在 Scala/层次化 source 观察中采样，不能从当前
`build/rtl/StoreQueue.sv` 的 `mmioStout` 顶层端口建立 pin-level coverpoint。`CMO-address-nderr-not-pin-visible`
同理只能在 CMOUnit/full-MemBlock 层采样；StoreQueue 顶层只看到 `denied/corrupt`。`NC-BEU-latency-gate`
必须同时检查 Uncache grant、MemBlock 两拍延迟和 `cache_error_enable`，不能用 SQ `completed` 代替。

## 2. 内部逻辑触发的测试点与功能覆盖模型

### 2.1 Entry 状态、指针和提交边界

| ID | 内部触发条件 | 需要观察的状态变化和外部证据 | 功能覆盖检测点 |
|---|---|---|---|
| IN-01 | `entryCanEnq` 命中一个或多个物理 slot。 | `allocated=1`，完成/地址/数据/特殊分类状态清零，`waitStoreS2=1`。但 allocation 分支不清 `memBackTypeMM`，复用槽在合法 S2 Re 到达前可能保留上一代 backing 值；RM 必须把它视为暂态旧值，而不是新 entry 的有效分类。 | 层次化 `cp_entry_lifecycle=allocated`；cross `enq_width x wrap x vector_flow_count x memBackTypeMM_preS2`，并单列 stale-metadata watcher。 |
| IN-02 | S1 分别取 `updateAddrValid=1/0`，再由合法 S2 owner 的 `updateAddrValid=1` 完成分类。 | S1 ready 才能写 `addrvalid/nc`；S1 non-ready 仍可能更新 CAM/非对齐状态，但 entry 保持非 address-ready。对于非 vector S1 的非零 `StaCfg.exceptionVec`，ExceptionBuffer source 不要求 `updateAddrValid=1`，所以 `s1_nonready` 还要覆盖“异常可见、地址未 ready”的组合。随后只有合法 S1->S2 token 才清 `waitStoreS2` 并写 MMIO/exception 分类。 | `cp_entry_ready_transition={s1_ready,s1_nonready_CAM_only,s1_nonready_exception,s2_class}`。 |
| IN-03 | data 写入后的一拍 `allocated(lastStWbIndex)`。 | `datavalid=1`；mask 独立于此状态。 | `cp_data_ready_delay=one_cycle`；cross `data_first_mask_first x allocated`。 |
| IN-04 | `addrReadyPtrExt` / `dataReadyPtrExt` 连续扫描 ready entry。 | 每拍最多跨四个连续 entry；data ready 遇到非对齐实际 dequeue group 时有额外补跳。 | `cp_ready_frontier_step={0,1,2,3,4,unaligned_adjust}`。 |
| IN-05 | `cmtPtrExt` 前沿 entry 满足 ROB `pendingPtr`、无 cancel、S2 已完成。 | scalar 直接 commit；vector 还要求 `vecMbCommit`；MMIO FSM busy 时限制新的 scalar commit。 | `cp_commit_gate={scalar,vector_wait,vector_commit,mmio_block}`。 |
| IN-06 | entry `completed=1` 并位于 `deqPtrExt` 连续前沿。 | 最多两项同拍释放，`allocated/completed` 清零，`sqDeq` 一拍后反映数量。 | `cp_deq_count={0,1,2}`；cross `completion_source x deq_count`。 |
| IN-07 | `readyReadGoVec` 因 DataBuffer high fragment、NC complete 或 MMIO writeback而有效。 | `rdataPtrExt` 按 popcount 前移；不得把它和 `deqPtrExt` 混为同一指针。 | `cp_rdata_advance_cause={dataBuffer,nc,mmio}`；cross `cause x deq_count`。 |

### 2.2 Forwarding、DataBuffer 与 SBuffer 内部路径

| ID | 内部触发条件 | 需要观察的状态变化和外部证据 | 功能覆盖检测点 |
|---|---|---|---|
| IN-08 | VAddr/PAddr CAM 同时命中一个 ready 的 older entry。 | `SQData8Module` 逐 byte 从候选中选数据；数据和 mask 的 producer entry 可逐 byte 不同。 | `cp_cam_match={v_and_p}`、`cp_byte_source_cardinality`。 |
| IN-09 | CAM 候选地址 ready、data 未 ready，或 allocated unaligned 候选在年龄窗口内。 | 生成 `dataInvalidMask`，随后在 mask2 非空时优先 mask2，否则按 `Reverse(PriorityEncoderOH(Reverse(mask)))` 选择对应 mask 段中物理 index 最大的置位项作为 `dataInvalidSqIdx`。这不是 ExceptionBuffer 的异常年龄仲裁，不能把该 pointer 笼统解释为按程序年龄排序的最老项。 | `cp_data_invalid_mask_nonzero`；cross `cause x selected_blocker_position x selected_mask_segment`。 |
| IN-10 | strict/non-strict 地址等待路径。 | strict 对相关窗口检查；non-strict 仅用 LFST `loadWaitBit + waitForRobIdx` 关系。 | `cp_addr_invalid_path={strict,lfst}`；cross `path x blocker_ready`。 |
| IN-11 | 对齐 cacheable 已提交 store 进入 DataBuffer。 | `vecValid` 仅在无异常且 vector completion/数据条件满足时为 1；DataBuffer backpressure 不改变 payload。 | `cp_databuffer_enq={lane0,lane1,both}`；cross `vecValid x hasException x sbuffer_ready`。 |
| IN-12 | SBuffer fire 且 `sqNeedDeq=1 && !wline`。 | 对应 SQ entry `completed=1`；低跨 16B fragment 的 `sqNeedDeq=0` 不得单独完成原 entry。 | `cp_completion_source={sbuffer_high,sbuffer_normal}`；assertion bin `low_fragment_no_complete`。 |
| IN-12a | scalar cacheable entry 的 `hasException=1` 走 DataBuffer/SBuffer drain。 | SQ 可以看到 `sbuffer.fire` 并据 `sqNeedDeq` 置 completed，但 `DataBufferEntry.vecValid=0`，SBuffer 内部 `writeReq.valid` 必须为 0；这是“释放无真实写”，不是成功 SBuffer store。 | `cp_exception_drain_no_write`；cross `exception_origin x sbuffer_fire x vecValid=0 x completed`。 |

### 2.3 NC、MMIO、CMO/CBO 状态机

| ID | 内部触发条件 | 需要观察的状态变化和外部证据 | 功能覆盖检测点 |
|---|---|---|---|
| IN-13 | `ncState: idle -> req -> req_ack -> resp/idle`。 | normal scalar NC 在 commit、allvalid、无 exception、非 MMIO/非 CBO 条件下发 request；non-outstanding 由 response 完成，outstanding 由 `idResp.nc` 的 `mid` 完成。最终 response 的 error bits 不改变 SQ completion，但要由 Uncache/BEU 单独观测。outstanding 模式允许 SQ 在 ack 后继续发新 NC，因此模型必须维护按 `mid`/generation 索引的 owner 集合。`ncDeqTrigger` 没有 `ncState`、owner、generation 或 `allocated` 门控，错误 response 可能静默修改任意槽；该副作用必须单列风险 watcher。 | `cp_nc_fsm_transition`；transition bins 覆盖每条合法边；cross `outstanding_mode x completion_trigger x response_error x BEU_seen`，并覆盖 `owner_set_depth`。另设 `cp_nc_unmatched_response_side_effect`，不计正向覆盖。 |
| IN-13a | vector store 的 StoreUnit S2 实际解析为 NC 或 MMIO address。 | `StoreUnit` 将其编码为 `storeAccessFault`；normal vector NC/MMIO FSM 不得启动。实际 MMIO 由 exception 抑制 `mmio` 后，可经 vector feedback 进入 `vecValid=0` no-write drain；实际 NC 的 `nc` 已由 S1 写入且不被 S2 exception 覆盖，在 matching feedback/commit 后 direct-complete，不进入 DataBuffer/SBuffer。 | `cp_vec_uncache_convert_to_exception`；cross `actual_{nc,mmio} x terminal_{direct_complete,no_write_drain} x vecExceptionFlag_{absent,eligible}`。 |
| IN-14 | `mmioState: idle -> req -> resp -> wb -> wait -> idle`。 | 启动谓词经 `RegNext`，要求 `pendingst/pendingPtr`、pending、地址/数据 ready、无 exception；`scommit` 释放 wait。 | `cp_mmio_fsm_transition`；cross `response_kind x terminal_state`。 |
| IN-15 | 非 NC 的 `io.uncache.resp.fire`，分别为 `denied`、仅 `corrupt`、两者同时为 1，且 `cmoOpResp.bits.denied/corrupt=0`。 | 这是当前 SQ 真正执行 error injection 的路径：own `denied` 置 store access fault；仅 own `corrupt` 置 hardware error；同一 uncache response 的两 bit 同时为 1 时 own denied 覆盖 own corrupt。普通 MMIO 和 I/O-backed CBO.zero 的每一个 beat 都应覆盖。 | `cp_uncache_error={ok,denied_only,corrupt_only,own_both}`；cross `own_error x {mmio,cbo_zero_io} x cbo_zero_beat={first,middle,last} x writeback_ready`。 |
| IN-15a | non-zero CBO 的 `io.cmoOpResp.fire` 带 `denied=1` 或 `corrupt=1`，且没有伪造 `uncache.resp.fire`。 | 当前源码只将 `mmioState` 从 `s_resp` 转到 `s_wb`，**不会**在合法 CMO response fire 上把 error 写入 `uncacheUop.exceptionVec`。真实当前序列是 `cmoOpResp.fire -> s_wb -> 无异常 mmioStout.fire -> s_wait`，不能算作“CMO error 已被正确上报”。若错误源是 L1/L2 本地 ECC，可能另有对应 cache BEU；这不能替代当前 CBO 的精确 exception。CHI `NDERR` 也不进入 L2 本地 ECC BEU。 | `cp_error_boundary=cmo_error_not_propagated`；watch `cmo_error_source={l1_local,l2_local,chi_downstream,external_injection} x cmo_error x cache_beu x mmioStout_exceptionVec x s_wait_after_fire`，不进入正向异常 bin。 |
| IN-15b | 先完成一次带 error 的 non-zero CMO，再在没有新的 CMO request 清除 DCache CMOUnit error 寄存器时，让任意 `!nc` uncache response 到达：普通 MMIO 或 I/O-backed CBO.zero 的 first/middle/last beat。 | `StoreQueue` 在 uncache response 分支未以 `cmoOpResp.fire` gate `cmoOpResp.bits.denied/corrupt`；stale CMO bit 可污染无关响应。own denied/corrupt 的“优先级”不跨来源：例如 own denied 与 stale CMO corrupt 可同时置 store access fault 和 hardware error。该序列是 source-confirmed defect signature。 | `cp_error_boundary=stale_cmo_error_leak`；bug-regression cross `response_owner x own_uncache_error x stale_cmo_{none,denied,corrupt,both} x injected_exceptionVec`。 |
| IN-15c | normal NC 的最终 response 带 `denied/corrupt`，分别覆盖 `uncacheOutstanding=0/1`。 | non-outstanding：当前 `resp.fire` 才使 SQ completed，同时 Uncache 可上报 bus error；outstanding：合法 `idResp.nc/mid` 已先使 SQ completed，之后最终 error response 没有 SQ/ROB store-exception receiver，只能走 Uncache `busError.ecc_error -> MemBlock.uncacheError`（受 cache-error enable gate）。`idResp.nc=0` 不属于 NC error 场景；它是 MMIO/I/O-backed CBO.zero 的合法 ack，SQ 忽略且不推进 NC。 | `cp_error_boundary=nc_error_not_to_ROB_exception`；watch `mode x completion_trigger={resp,idResp} x error_arrival_phase={current,after_ack} x uncache_BEU_seen x no_sq_exception`，不计正向异常。 |
| IN-15d | MMIO、non-zero CMO、I/O-backed CBO.zero 的 SQ special writeback。 | Scala 在 MMIO FSM 入口把内部 `uncacheUop.trigger` 置为编码 0，而全局 `TriggerAction.None` 是 15；但当前生成 `StoreQueue.sv` 的 `mmioStout` 顶层没有 `trigger` 字段，只有 source/层次化模型能观察这一差异。不得把它写成 pin-level output 或用它构造 breakpoint 覆盖。 | `cp_error_boundary=SQ-special-writeback-trigger-not-pin-visible`；source-level watch `op_class x uncacheUop.trigger`。 |
| IN-15e | CMOUnit 返回带 `address`、`nderr`、`denied` 或 `corrupt` 的 CMO response。 | CMOUnit 会锁存 block-aligned `address`、`nderr` 和错误状态，但 StoreQueue 顶层只接收 `valid/denied/corrupt`，没有 `address/nderr`；因此地址对齐、`nderr` 与 denied/corrupt 一致性必须在 CMOUnit/full-MemBlock 层检查，不能声称 SQ pin-level 已覆盖。 | `cp_error_boundary=CMO-address-nderr-not-pin-visible`；cross `cmo_phase x address_alignment x nderr x denied_corrupt`。 |
| IN-15f | Uncache grant 携带 store error，且 CSR `cache_error_enable` 取 0/1。 | Uncache 在 `mem_grant.fire` 产生 `busError.ecc_error`；MemBlock 延迟两拍后输出 `uncacheError`，并由 `cache_error_enable=0` 抑制。SQ 的 NC completion 与 BEU 事件必须分开计数。 | `cp_error_boundary=NC-BEU-latency-gate`；cross `grant_phase x beu_phase={+2} x block_alignment x cache_error_enable`。 |
| IN-16 | `deqCanDoCbo`，且 SBuffer 尚未或已经 flush。 | 非 zero CBO 等待 `cboFlushedSb` 后启动 CMO；CBO.zero main-memory 路径启动独立 `cboZeroValid/cboZeroWaitFlushSb`。 | `cp_cbo_control={wait_flush,cmo_req,cmo_resp,zero_wb}`。 |
| IN-17 | CBO.zero I/O-backed 的 offset 递增。 | 每个 response 决定下一 beat 或 `s_wb`；覆盖所有 line beat，不把一拍 completion 当成整行完成。 | `cp_cbo_zero_offset={first,middle,last}`；cross `offset x response_kind`。 |

### 2.4 非对齐、跨页 MAB 和 vector 特性

| ID | 内部触发条件 | 需要观察的状态变化和外部证据 | 功能覆盖检测点 |
|---|---|---|---|
| IN-18 | `unaligned=1 && cross16Byte=0`。 | SQ 组织一个 16-byte 对齐的 DataBuffer record，并保持正常 `sqNeedDeq=1`。 | `cp_misalign_kind={within_16B}`；cross `misalign_kind x store_size`。 |
| IN-19 | `unaligned=1 && cross16Byte=1`，两 DataBuffer lane ready。 | 用同一原 SQ entry 形成 low/high 两个 record；low `sqNeedDeq=0`，high `sqNeedDeq=1`。 | `cp_misalign_kind={cross16}`；cross `dual_lane_fire x low_high_mask_shape`。 |
| IN-20 | 无 `hasException` 的 MAB 正常跨页项，`crossPageWithHit=1 && crossPageCanDeq=0`。 | `misalignToDataBufferValid` 的正常跨页路径停住，避免使用未就绪高页 PAddr。异常 drain 是独立例外，不填入此 bin。 | `cp_crosspage_wait`；normal-path assertion `no_high_paddr_consume_while_not_can_deq`。 |
| IN-21 | MAB `crossPageWithHit=1 && crossPageCanDeq=1`，双 lane fire。 | lane 1 的地址等于 MAB 高 child PAddr，`doDeq=1` 释放 MAB；这早于 SQ completed/deq。 | `cp_crosspage_release`；cross `mab_state x dataBuffer_pair_fire x sq_completion_later`。 |
| IN-22 | `vecFeedback.isCommit` 或 `isFlush` 与 live entry 的 ROB/uop identity 匹配。 | `vecMbCommit=1`，vector entry 才可跨 commit/DataBuffer gate。 | `cp_vec_commit_source={feedback,maControl_bridge}`；cross `source x vec_last_flow`。 |
| IN-23 | 非 NC 的 vector exception flow 在 DataBuffer `enq.fire && sqNeedDeq=1`，且当前 flow 不是该 ROB 指令的最后 flow；随后同 ROB 的最后 flow 也以 DataBuffer `enq.fire && sqNeedDeq=1` 到达；另覆盖实际 MMIO/实际 NC 的转换路径。 | 非最后 flow 的 MMIO 异常 drain 才置 `vecExceptionFlag` 并抑制同 ROB 后续 `vecValid`；同 ROB 最后 flow 的 DataBuffer 释放才清 flag。实际 NC 的 `nc && hasException` direct-complete 不进入 DataBuffer，因此 `vecExceptionFlag` 不置位；不能把它与 MMIO no-write drain 合并。 | `cp_vec_exception_flag={set,hold,clear,not_set_for_nc_direct_complete}`；cross `actual_mem_class x flow_position={non_last,last} x terminal_path={MMIO_no_write_drain,NC_direct_complete} x vecExceptionFlag`。 |

### 2.5 异常、redirect 与优先级

| ID | 内部触发条件 | 需要观察的状态变化和外部证据 | 功能覆盖检测点 |
|---|---|---|---|
| IN-24 | 单一 scalar S1 `StaCfg` exception 进入 `StoreExceptionBuffer`。 | SQ source port enable 精确为 `storeAddrIn.fire && !miss && !isvec`；ExceptionBuffer 随后还要求 `ExceptionNO.selectByFu(uop.exceptionVec, StaCfg).asUInt.orR=1` 才接纳候选，并按 oldest 选择 exception 地址上下文。被 redirect flush 的请求不能继续作为有效 exception。vector S1 即使有 exceptionVec 也不从此 source 直接产生 `exceptionAddr`，应改走 vector `FLUSH` source。 | `cp_exception_source={s1}`；cross `source x sta_cfg_nonzero x isvec_gate x redirect_before_select`；illegal/watch `vector_S1_direct_exbuf`。 |
| IN-24a | `StoreExceptionBuffer` 的七个真实 source 逐一单发、同拍/相邻拍竞争、同 ROB 不同 `uopIdx` 竞争，以及已有 `req` 与新候选竞争。 | source 固定为 S1×2、S2×2、vector FLUSH×2、MMIO error writeback×1。按 `{robIdx,uopIdx}` 选最老；同 ROB 时较小 `uopIdx` 更老。若身份完全相同，`selectOldest` 按静态输入 source 顺序取先者；当拍 source 排在已保持 `req` 前，因此可替换 held request，不能只把“更老的新候选”视为覆盖条件。`exceptionAddr` 无 valid，必须结合已知 source token 和后端异常生命周期采样。 | `cp_exbuf_source`、`cp_exbuf_arbitration`；cross `source_set x selected_oldest_token x same_rob_uop_order`，包含 `held-vs-new-same-identity` bin。 |
| IN-24b | `vecFeedback.valid && FLUSH`，其 `StaCfg` 选择后的 exceptionVec 非零，并与 live SQ entry 的 `{robIdx,uopIdx}` 匹配。 | vector FLUSH 同时令 matching entry `vecMbCommit=1`，并作为 ExceptionBuffer source 经 oldest/redirect filter 后提供 `exceptionAddr`。若 FLUSH 而所有 StaCfg bit 为 0，`vecMbCommit` 可置位但不得期待 exceptionAddr；把它列为 source-consistency/protocol-negative 边界。 | `cp_exception_source=vector_FLUSH`；cross `FLUSH_exceptionVec_nonzero x vecMbCommit x exceptionAddr_seen`；illegal bin `FLUSH_zero_exceptionVec_as_exbuf_source`。 |
| IN-25 | 前一拍同 lane 有 `storeAddrIn.valid && !miss` 的 S1 owner，且本拍 `storeAddrInRe.updateAddrValid=1`；Re 的**顶层可见**非 SAF 位 `{3,6,15,19,23}` 至少一个为 1，或 `af=1`，并且真实 producer 同时令 Re `hasException=1`。 | 只有这个 S1→S2 token 才更新 entry `hasException`。送入 StoreExceptionBuffer 的 S2 payload 会把 bit 7 **写成** `af`：`af=1` 才形成 S2 SAF source；它不是对原 Re bit 7 的 OR。S2 exceptionVec 不回写 `uop(sqIdx).exceptionVec`，最终应查 exception buffer/StoreUnit writeback，而不是 SQ entry uop。无 owner、身份/代次错误，或真实 producer 明确要求 S2 更新却给出 `updateAddrValid=0`，均为该正向异常场景的 protocol-negative；合法的无更新周期由 IN-25g 单独覆盖。 | `cp_s2_exception_encoding`；cross `valid_owner_token x updateAddrValid x hasException x exceptionAddr_seen x entry_completion_path`；illegal bins `Re_without_owner`、`Re_identity_mismatch`、`expected_exception_with_no_update`（仅限 producer 明确要求更新）。 |
| IN-25g | 前一拍同 lane 有合法 `storeAddrIn.valid && !miss` 的 S1 owner，本拍 `storeAddrInRe.updateAddrValid=0`，覆盖非最终 split/replay 等可产生无更新 sideband 的路径。 | 这是合法 inert 周期：SQ 不清 `waitStoreS2`，不更新 `pending/mmio/hasException`，也不形成 S2 ExceptionBuffer source；不能把它当作 S2 exception completion。由于写使能为 0，Re 的 tag/异常 payload 对 SQ 为 don't-care，不应把不匹配本身报为协议错误。只有无前驱却拉高 `updateAddrValid`，或在 `updateAddrValid=1` 的真实回填周期身份/代次不一致，才报协议错误。 | `cp_s2_no_update={paired_inert}`；cross `producer_path x updateAddrValid=0 x waitStoreS2_held x no_exception_source`；另设 payload-dont-care bin。 |
| IN-25a | 前一拍 S1 uop 的 `trigger=DebugMode`；T+1 Re 只能用 `hasException=1`、`StaCfg.exceptionVec=0`、`af=0` 表示 S2 结果（当前生成 Re **没有** trigger 端口）。 | SQ 会抑制真实 SBuffer 写并允许异常 drain；StoreExceptionBuffer 仍按 exceptionVec 过滤，不能期待 `exceptionAddr` 新值。最终 debug trap 要通过前拍保留的 S1 trigger 在 ROB/CSR 链检查。 | `cp_exception_source=s2_Dmode_only`；cross `S1_trigger_Dmode x Re_exceptionVec_zero x exceptionAddr_not_required x ROB_debug_trap x no_real_sbuffer_write`。 |
| IN-25b | source-reachable 的多个 store exception bit 同时为 1，或 error 注入与已有 bit 同时存在。 | 不同 producer 的状态落点不同：非 vector S1 exceptionVec 存入 SQ uop 并可进入 S1 ExceptionBuffer source；vector S1 exceptionVec 不直接进入该 source，需由 vector FLUSH 提供；S2/Re exceptionVec 仅走 StoreExceptionBuffer；MMIO response 写 `uncacheUop.exceptionVec`；MAB 保存自身 `exceptionVec`。SQ 不负责压缩成单一 cause；full-core 必须按 `ExceptionNO.priorities`/CSR 验证最终 cause。 | `cp_exception_shape={one_bit,legal_multi_bit}`；cross `shape x origin x producer_storage x final_selected_cause x redirect_phase`。 |
| IN-25c | S1 发出后紧随 redirect/cancel、slot reuse 或 Re 携带错误 ROB/uop tag；随后仍收到 `storeAddrInRe.updateAddrValid=1`。 | Re 的硬件关联只用前拍同 lane、non-miss S1 锁存的 slot，不检查 Re identity、generation、allocated 或 redirect。因此这类 late/wrong Re 不能当作正常 S2 exception，adapter/RM 必须报 protocol-negative 并禁止作为正向输入。 | assertion/illegal bins `late_Re_after_redirect_or_reuse`、`Re_identity_mismatch`、`Re_without_live_generation`。 |
| IN-25d | `mmioStout.fire` 携带 SAF/HWE 的 exceptionVec，作为 ExceptionBuffer 的第七 source。 | 只比较源码明确赋值的 `fullva=vaddrModule.rdata.head`、`vaNeedExt=1`、`uncacheUop` 的 ROB/uop identity 与 SAF/HWE 路径；不得要求 `gpaddr/isHyper/isForVSnonLeafPTE` 保留原 S1/S2 的翻译上下文。 | `cp_exbuf_source=mmioStout`；cross `uncache_error_kind x exceptionAddr_vaddr_token x exceptionAddr_vaNeedExt`。 |
| IN-25e | 对 S2 SAF 编码分别观察 `af=1`、顶层可见非 SAF bit 非零、以及 source-level 的 raw Re `exceptionVec[7]=1 && af=0`。 | 当前 build 顶层根本没有 Re bit 7；若在 Scala/层次化模型观察到 raw bit 7，它在送入 ExceptionBuffer 前仍被 `af` 覆盖。因此 `af=1` 必须形成 SAF source；仅 raw bit 7 不能单独要求 `exceptionAddr`，除非另有可见非 SAF bit。后者是 source-level 观察边界，不是 standalone pin-level stimulus。 | `cp_s2_exception_encoding={Re_nonSAF_bit,af_SAF,raw_bit7_overridden_source_watch}`；cross `encoding x hasException x exceptionAddr_seen`。 |
| IN-25f | 普通 `VMergeBuffer` entry 的 `TriggerAction.isDmode=1` 且 `exceptionVec=0`，随后观察 `toLsq` feedback 和 SQ vector state。 | `VMergeBuffer.entryHasException` 会把 Dmode 纳入异常选择，但 `ToLsqConnect` 只按 `exceptionVec` 设置 FLUSH/COMMIT；该组合可能被送成 COMMIT，trigger 元数据不会出现在 SQ `vecFeedback` 顶层。作为 full-MemBlock debug/metadata watcher，不计入 vector FLUSH 正向覆盖；需同时确认并行 vector writeback/ROB 是否拦截。 | `cp_error_boundary=vector-Dmode-to-commit-watch`；cross `dmode_only x toLsq_feedback={commit,flush} x ROB_debug_trap`。 |
| IN-26 | 已 commit 的 `nc && hasException`。 | 直接置 `completed`，不启动 normal NC request。 | `cp_nc_exception_direct_complete`；cross `nc x hasException x commit`。 |
| IN-27a | scalar MAB child 带真实 `StaCfg.exceptionVec` 且 `isUncache=0`。 | child request/response 路与 MAB parent final writeback 是**并行分叉**：`MAB.splitStoreReq -> StoreUnit.misalign_stin -> misalign_stout -> MAB`；child 的 StoreUnit S1/S2 可独立向 SQ StoreExceptionBuffer 提供地址上下文；MAB `writeBack` 则直接 `-> MemBlock.stOut(0) -> ROB`，不会回灌 StoreUnit。MAB final 不进 normal `s_block/paddr/doDeq`。 | `cp_mab_scalar_terminal=child_exception`；cross `child_index x MAB_direct_ROB_WB x child_exceptionAddr_proxy x ROB_identity`。 |
| IN-27b | scalar MAB child 实际分类为 MMIO。 | StoreUnit 对 `isFrmMisAlignBuf && actually_mmio` 先生成 store access fault，并因 S2 exception 抑制 `mmio`；MAB 看到 `hasException=1,isUncache=0`，走 `globalException`，最终 MAB direct writeback 为 SAF。它不是 generic `globalUncache + storeAddrMisaligned`。未有架构规格签署时列为源码观察边界。 | `cp_error_boundary=MAB_MMIO_to_SAF`；watch `child_index x globalException=1 x globalUncache=0 x MAB_WB_SAF`。 |
| IN-27c | scalar MAB child 实际分类为 NC。 | StoreUnit 生成 store-address-misaligned，NC 属性仍在；MAB 同时可见 `hasException=1,isUncache=1`，记录的 `globalException/globalUncache` 均可为 1，而 `isUncache` 的 exceptionVec 分支优先，最终 MAB direct writeback 为 SAM。它不产生普通 uncache request。未有架构规格签署时列为源码观察边界。 | `cp_error_boundary=MAB_NC_to_SAM`；watch `child_index x globalException=1 x globalUncache=1 x MAB_WB_SAM`。 |
| IN-27d | vector MAB child 带 exceptionVec 或 uncache 分类。 | exceptionVec 类的链应为 `MAB.vecWriteBack -> VMergeBuffer -> toLsq`；`ToLsqConnect` 仅依 exceptionVec 决定 FLUSH/COMMIT，再影响 SQ `vecMbCommit`/StoreExceptionBuffer。MAB vector writeback 固定 `trigger=None`；Dmode-only metadata 会丢失、也不能保证 FLUSH，故 Dmode-only 与 MAB uncache 分类均单列源码观察边界，不能算已支持 debug/uncache 正向功能。 | `cp_mab_vector_terminal={exceptionVec_FLUSH,normal_COMMIT}`；cross `child_index x exceptionVec x VMerge_feedback x SQ_vector_state`；watch `MAB_vector_Dmode_observation`。 |
| IN-28 | redirect 命中未提交 entry，分别在有/无 `vecExceptionFlag` 下。 | 普通按 `robIdx.needFlush` cancel；vector exception flag 存在时使用其更严格的年龄边界。 | `cp_cancel_policy={normal,vec_exception}`；cross `policy x committed_bit`。 |
| IN-28a | redirect 与异常来源交错：S1、延迟 S2、ExceptionBuffer 已驻留、MMIO error writeback、MAB `s_resp/s_wb/s_block` 和 `vecExceptionFlag`。 | SQ 与 MAB 是独立 redirect receiver：SQ 在 `vecExceptionFlag` 有效时有特殊严格年龄谓词；MAB 仍只用自身 `req.uop.robIdx.needFlush` 或 `s2_needRevoke`。下游 ROB/ExceptionGen/StoreExceptionBuffer/VMerge 需过滤被杀项；不能错误断言 MAB `writeBack.valid` 同拍必为 0。 | `cp_exception_redirect_phase`；cross `origin x redirect_phase x SQ_or_MAB_receiver x downstream_filter_result`。 |
| IN-28b | MAB 位于 `s_req/s_resp/s_wb/s_block` 时命中 `req.uop.robIdx.needFlush(redirect)`，以及在可入队后命中 `s2_needRevoke`。 | 不能要求 redirect 当拍 `writeBack.valid=0`；但寄存器更新后必须回到 `s_idle`，清 `req_valid/curPtr/unSentStores/unWriteStores/isCrossPage/globalException/globalUncache`，且不得遗留 `crossPageCanDeq/doDeq` 或迟到 terminal writeback。 | `cp_mab_cancel_state={req,resp,wb,block,revoke}`；cross `cancel_state x next_cycle_idle x no_stale_sideband`。 |
| IN-29 | redirect 命中已 committed entry。 | 不走 `needCancel`；其完成/dequeue 仍由已获得的合法路径推进。 | `cp_redirect_committed_survives`；cross `committed x completion_sink`。 |
| IN-30 | 普通 cacheable scalar 跨 16B MAB store，无 memory trigger 命中。 | 当前 MAB admission 复制 StoreUnit 的早期 `s1_in.uop.trigger`，MAB final STA0 writeback 可表现为 `trigger=BreakpointExp(0)` 且 `exceptionVec[breakPoint]=0`，而正确无 trigger 编码应为 `None(15)`。这是已确认的 metadata defect，不得作为真实 breakpoint exception 或正向成功覆盖。 | `cp_error_boundary=MAB_trigger_metadata_defect`；bug-regression bin `trigger_0_and_breakpoint_0`，并检查 MAB 是 STA0 producer。 |
| IN-30a | scalar 跨 16B MAB store 携带 `DebugMode` trigger，观察 MAB admission、child StoreUnit Dmode 结果和 parent final writeback。 | `StoreUnit.s1_toMisalignBufferValid` 的 admission 条件没有像 `s1_misalign` 那样排除 Dmode；MAB parent 仍从早期 `s1_in` 保存 trigger，而 child 回流会重新计算 Dmode。`globalException`、MAB scalar `writeBack` metadata 以及是否被 revoke/迟到送到 ROB 可能因此分叉；这是 full-MemBlock/source-level 观察边界，不要求 SQ `exceptionAddr` 出现，也不计入 breakpoint/DebugMode 正向完成覆盖，需用 ROB/CSR 终点和 MAB producer 身份确认。 | `cp_error_boundary=MAB-scalar-Dmode-observation`；cross `cross16_scalar x Dmode x mab_admission x child_dmode x globalException x parent_wb_metadata x ROB_debug_endpoint`。 |
| IN-31 | ready frontier 的四项 lookahead 依次遇到普通 addr/data ready、MMIO、vector commit、unaligned、tail 与 redirect。 | `addrReadyPtrExt/dataReadyPtrExt` 仅跨连续前缀，每拍至多前进 4；data pointer 的 unaligned dequeue 补跳另行发生。`stAddrReadyVec` 与 scan 公式的 vector gate 不完全相同，必须各自采样。 | `cp_ready_pointer_reason`；cross `frontier_kind x scan_stop_reason x step x vector_formula_variant`。 |
| IN-32 | `sqDeqCnt`、`sqEmpty`、`force_write` 与 `perf` 的驱动条件在相邻周期变化。 | `sqDeq` 是 `sqDeqCnt` 的寄存输出，`sqEmpty` 和 `force_write` 同样有寄存边界；`sqDeqIsVec` 直接读当前 head，禁止拿来给同拍 `sqDeq` 分类。 | `cp_status_register_delay`；assertion `no_same_cycle_sqDeqIsVec_typing`；cross `internal_event x output_cycle_offset`。 |
| IN-33 | redirect T0 后，分别观察 `needCancel`、`lastEnqCancel`、`lastCycleCancelCount`、`redirectCancelCount` 与 `enqPtrExt`。 | T0 计算 kill，T1 持有恢复窗口且不能有 enqueue number，T2 输出 `sqCancelCnt` 并回退 tail；已 committed 项不在 `needCancel` 中。 | `cp_redirect_pipeline={T0,T1,T2}`；cross `cancel_source={allocated,enq} x count x committed_bit`。 |
| IN-34 | `noPending` 在 MMIO/CMO request 与 response 时变化，并和 `wfiReq` 相交。 | `wfiSafe=GatedValidRegNext(noPending && wfiReq)`；它是 SQ 许可，不证明 writeback/ROB 或 NC 生命周期已清空。I/O-backed `CBO.zero` 的 non-NC response 走 `mmioIsCboZero` 分支时不执行 `noPending := true.B`，可能使 `noPending/wfiSafe` 持续为假；该现象列源码风险 watcher，不当作架构要求。 | `cp_wfi_noPending_transition`；cross `special_fsm_phase x wfiReq x wfiSafe_delay`；watch `cbo_zero_io_noPending_stuck`。 |

### 2.6 内部 coverage model 的层次化观察点

若验证环境允许 bind 或层次引用，最小内部采样集合为：

```text
entry state: allocated, addrvalid, datavalid, committed, completed, pending,
             nc, mmio, memBackTypeMM, unaligned, cross16Byte, hasException,
             waitStoreS2, isVec, vecLastFlow, vecMbCommit,
             allocation_memBackTypeMM_stale
pointers:    enqPtrExt, cmtPtrExt, rdataPtrExt, deqPtrExt,
             addrReadyPtrExt, dataReadyPtrExt
FSMs:        ncState, mmioState, cboZeroValid, cboZeroWaitFlushSb,
             cboFlushedSb, StoreMisalignBuffer.bufferState
buffers:     DataBuffer enq/deq valid/fire/payload.sqNeedDeq/vecValid,
             StoreExceptionBuffer.req_valid, SBuffer in.fire/vecValid/writeReq.valid,
             StoreMisalignBuffer.writeBack/vecWriteBack, VMergeBuffer-to-LSQ feedback
```

如果只能 black-box 采样，应以对应的 `sbuffer`、`uncache`、`cmoOp`、writeback、`sqDeq`、
ready pointer、forward response 以及 `maControl` 事件建立 proxy bins，并在 coverage report 中将它们标记为
“外部可推断”，不可声称已经直接覆盖每个内部状态。

## 3. 异常测试点审查边界

异常测试至少要覆盖以下互斥/优先级关系，而不是只随机 exception bit：

| 异常族 | 最小场景 | 必检结果 | 覆盖归类 |
|---|---|---|---|
| 地址 S1 exception | S1 携带非 vector 的 `StaCfg` exception；分别取 breakpoint、store-address-misaligned、store-access-fault、store-page-fault、hardware-error、store-guest-page-fault，并交叉 `updateAddrValid=0/1`。 | ExceptionBuffer 的 S1 source 使能是 `valid && !miss && !isvec`，不要求 `updateAddrValid=1`；因此 `updateAddrValid=0` 仍应获得正确地址/uop，但 entry 不应因此变成普通 address-ready。并在同 ROB 不同 `uopIdx`、多源并发时选择最老候选；普通 store 写路径不被当作成功完成。 | 正向异常功能；cross `exception_bit x updateAddrValid x addrvalid_state`。 |
| S2 Re exception | 前一拍合法 S1 owner，T+1 `updateAddrValid=1`，真实 producer 同步 `hasException=1`。正向 ExceptionBuffer source 只能是：顶层可见的 Re 非 SAF 位 `{3,6,15,19,23}` 至少一位为 1，或 `af=1`。分别覆盖 `af` 与 Re 暴露的 breakpoint、store-address-misaligned、store-page-fault、hardware-error、store-guest-page-fault。 | entry `hasException`、exceptionAddr 和最终完成路径一致；S2 payload 的 bit 7 由 `af` 直接覆盖/写入，故 `af=1` 才映射为 StoreExceptionBuffer 的 store access fault。当前生成 Re 没有 bit 7 引脚；若 source-level raw bit 7 为 1 且 `af=0`，不得单独期待 exceptionAddr。Re exceptionVec 也不应被误查为写回 SQ entry uop。 | 正向异常功能；raw bit 7 覆盖只作 source-level观察。 |
| S2 Dmode-only trigger | 前一拍 S1 uop 的 `trigger=DebugMode`；T+1 Re 仅以 `hasException=1`、`exceptionVec=0`、`af=0` 表示结果。当前生成 Re 没有 `trigger` 端口。 | SQ 有 exception-like drain/no-real-write 行为；StoreExceptionBuffer 不应被要求产生地址，ROB/CSR debug-trap 才是终点。 | 正向 scalar debug 功能；exceptionAddr 不计该 bin。 |
| non-NC uncache response error | 普通 MMIO 或 I/O-backed CBO.zero 的 valid response，分别为 denied-only、corrupt-only、both，且 CMO sideband error 为 0。 | 同一 uncache response 内 denied 优先为 access fault；仅 corrupt 为 hardware error；带 exception 的 `mmioStout` 不进入正常 `s_wait`。 | 正向异常功能。 |
| non-zero CMO response error | CMO `valid && ready` 时带 denied/corrupt，且不伪造同拍非 NC `uncache.resp.fire`。 | 当前源码中虽有 `denied -> storeAccessFault`、`corrupt(且非 denied) -> hardwareError` 的映射代码，但它被放在非 NC `uncache.resp.fire` 外层，不在 `cmoOpResp.fire` 分支中执行；因此实际走 `cmoOpResp.fire -> s_wb -> 无异常 mmioStout.fire -> s_wait`。`Uncache.busError` 不覆盖 CMOUnit 的 CBOAck，所以 CMO/SQ 路径不会产生本条 ROB exception 或专用 Uncache BEU。L1/L2 本地 ECC 可能由对应 cache 另报 BEU；CHI transaction error 没有 CoupledL2 本地 ECC BEU 补偿。 | 已知 RTL 缺陷签名/源码观察边界；后续 V2 上游 `7aa145db8f` 已修复精确 exception 传播。 |
| stale CMO error leakage | 前一 CMO error 后无新 CMO request，后续任何 `!nc` response 到达（普通 MMIO 或 I/O-backed CBO.zero 的任一 beat）。 | stale `cmoOpResp.bits` 可污染无关 uncache exception；跨来源时 own denied 与 stale corrupt 可同时置 SAF/HWE，不能沿用单来源 denied 优先的结论。 | 已知 RTL 缺陷签名。 |
| NC final-response error | normal NC final response 带 denied/corrupt，分别覆盖 outstanding/non-outstanding。 | SQ 不转换为 ROB store exception；non-outstanding 在 resp 完成，outstanding 在更早的 idResp 完成。Uncache 仍把 store response error 上报 BEU/`uncacheError`。 | 源码观察边界。 |
| NC + exception | `nc=1 && hasException=1` 已 commit。 | direct completed，无 normal NC request。 | 正向异常功能。 |
| vector 实际 MMIO | vector StoreUnit S2 的实际地址分类落到 MMIO。 | StoreUnit 转成 `storeAccessFault` 且 exception 抑制 normal mmio；不能进入 `vecmmioStout`。matching vector feedback 后可走 no-real-write DataBuffer/SBuffer drain。 | 正向异常功能；正常 vector MMIO sink exclude。 |
| vector 实际 NC | vector StoreUnit S2 的实际地址分类落到 NC。 | StoreUnit 转成 `storeAccessFault`；NC 属性在 S1 写入，normal NC FSM 不得启动；matching feedback/commit 后 `nc && hasException` direct-complete，DataBuffer/SBuffer 和 `vecExceptionFlag` 均不应被强制期待。 | 正向异常功能；正常 vector NC sink exclude。 |
| MAB scalar child 的普通 exceptionVec | low/high child 产生非 uncache 的 `StaCfg` exception。 | child request/response路径和 MAB terminal scalar writeback 并行：child 可经 StoreUnit S1/S2 给 SQ exceptionAddr，MAB parent 则 direct `writeBack -> stOut(0) -> ROB`。不使用 normal `maControl.paddr/doDeq`；固定无效的 `overwriteExpBuf.valid` 不是终点。 | 正向异常功能；full-MemBlock proxy。 |
| MAB scalar actual MMIO/NC child | low/high child 实际分类为 MMIO 或 NC。 | MMIO 产生 SAF/globalException；NC 产生 SAM，`globalException/globalUncache` 可同为 1 且 uncache 分支优先。MAB terminal 都直达 ROB，不经普通 uncache；当前保留为源码观察边界。 | 源码观察边界。 |
| MAB vector child | low/high child 有 exceptionVec 或 uncache 分类。 | exceptionVec 通过 `MAB vecWriteBack -> VMergeBuffer -> toLsq` 形成 FLUSH/COMMIT feedback；MAB uncache 与 Dmode-only metadata 丢失不作为已支持正向功能。 | exceptionVec 类为正向全核功能；uncache/Dmode-only 为源码观察边界。 |
| scalar exception drain | 已 committed 的 scalar entry 带 `hasException=1`，不需要 vector feedback，随后 DataBuffer/SBuffer `fire`。 | 可发生 `vecValid=0` 的 handshake 以释放 SQ entry，但 SBuffer 真实 `writeReq.valid` 必须为 0；该路径不能计为实际 cacheable store 写。 | 正向异常功能；cross `scalar_exception x sbuffer_fire x no_real_write`。 |
| vector exception | 非 NC 的 vector exception 在 DataBuffer `enq.fire && sqNeedDeq` 发生于非最后 flow，随后最后 flow 同样 fire。 | `vecExceptionFlag` 仅在前者 set、后者 clear；它不是泛化的 ROB commit/SBuffer fire 信号，且需要 vector feedback/flow identity 作为前置条件。 | 正向异常功能。 |
| 多 bit 与最终 cause 优先级 | source-reachable 的两个及以上 store exception bit 同时存在。 | 每个 producer 保存于自己的 receiver（S1 SQ uop、S2 buffer、MMIO uncacheUop、MAB exceptionVec）；最终 CSR cause 必须遵循 `ExceptionNO.priorities`，SQ 自身不负责压缩 cause。 | full-core 正向异常功能，按 `shape x origin x storage x final_cause x redirect_phase` 覆盖。 |
| MAB trigger metadata | 普通 cacheable scalar 跨16B、无 trigger 命中。 | 当前 MAB final STA0 `trigger=0 && exceptionVec[3]=0` 的已确认 defect signature 只能作为回归检测，不能伪造 breakpoint 覆盖。 | 已知 RTL 缺陷签名。 |
| MAB scalar Dmode | scalar 跨16B MAB 携带 `DebugMode=1`，从 admission 到 child response、parent writeback 和 ROB/CSR 终点全链路观察。 | admission 使用的 parent `s1_in` 与 child StoreUnit 重新计算的 Dmode 可能不一致；同时 `hasException/globalException`、revoke 和迟到 writeback 的组合尚未由 SQ 单独证明。必须确认实际 MAB producer、是否被 redirect/revoke、最终 ROB debug 处理；不要求 `exceptionAddr`，不计入正向 breakpoint/DebugMode coverage。 | 源码观察边界；watch `MAB-scalar-Dmode-observation`。 |
| redirect cancel | 未提交/已提交、普通/vector-exception policy，并与 S1/S2、ExceptionBuffer、MMIO writeback、MAB `s_resp/s_wb/s_block` 交错。 | SQ 与 MAB 分别用自己的 redirect receiver；下游 receiver 过滤被杀异常，MAB writeback 同拍电平不强制为 0，指针恢复延迟正确。 | 正向恢复功能。 |
| 已知 NC 跨 16B 缺陷 | `nc && unaligned && cross16Byte`。 | 当前 RTL 的 `rdataPtr` 双步推进签名仅用于 bug regression。 | 已知 RTL 缺陷签名，不计正向。 |

本章节最终关闭前，必须由至少两名独立 reviewer 分别检查：

1. 每个异常来源是否有真实 producer、entry/FSM 接收者和可观察终点；
2. 是否把 protocol-negative、未支持 vector MMIO/NC 或已知缺陷误当作正向功能 bin；
3. denied/corrupt 的 valid owner、CMO/NC error 的当前边界、MAB low/high child exception、vector exception/redirect 和 NC exception 是否有遗漏的交叉条件；
4. 已知 MAB trigger metadata defect、scalar MAB Dmode 观察边界与 `nc && unaligned && cross16Byte` 缺陷是否被隔离在 bug-regression/source watcher，而未进入正向 bins。

## 关联文档

- [V2 StoreQueue 内部接口知识](../../interface/v2/agents/storequeue_agent.md)：端口方向、valid/ready、裁剪字段和 black-box 时序。
- [LSQ 入队与 Redirect 恢复 flow](flows/lsq_enqueue_redirect_flow.md)：LSQ 分配和 redirect 的更大范围关系。
- [Store TLB 命中后的 Replay 与 Retry flow](flows/store_tlb_hit_replay_and_retry_flow.md)：MAB admission/retry 与 SBuffer-DCache retry 的边界。
- [V2 StoreQueue NC 跨 16B 异常、读指针与 Redirect 恢复设计确认](flows/storequeue_nc_cross16_exception_rdataptr_redirect_design_confirmation.md)：已确认缺陷与正常覆盖的隔离原则。

## 源码证据

- `build/rtl/StoreQueue.sv:58-857`：所有 standalone 可见端口及方向/宽度。
- `build/rtl/StoreQueue.sv:59266-59279`：S2 ExceptionBuffer source 将 bit 7 直接接到 `storeAddrInRe.af`，当前 Re 的顶层可见非 SAF exception 位集合。
- `src/main/scala/xiangshan/mem/lsqueue/StoreQueue.scala:255-488`：entry 状态、read/deq/ready pointer、分配和 ready frontier。
- `src/main/scala/xiangshan/mem/lsqueue/StoreQueue.scala:506-821`：地址 S1/S2、data/mask、forwarding、invalid blocker。
- `src/main/scala/xiangshan/mem/lsqueue/StoreQueue.scala:73-144`：StoreExceptionBuffer 的 exception source 过滤、最老选择和 redirect 过滤。
- `src/main/scala/xiangshan/mem/lsqueue/StoreQueue.scala:823-1169`：MMIO/NC/CMO/CBO FSM、CMO error 观察边界与 commit。
- `src/main/scala/xiangshan/mem/sbuffer/DatamoduleResultBuffer.scala:57-78`：两 lane prefix FIFO 的 valid/ready assertion。
- `src/main/scala/xiangshan/mem/lsqueue/StoreQueue.scala:1186-1529`：DataBuffer/SBuffer、non-aligned/cross-page、vector exception、redirect 和 pointer 恢复。
- `src/main/scala/xiangshan/mem/lsqueue/StoreMisalignBuffer.scala:162-231,233-324,338-346,527-673`：MAB cross-page 条件、high child PAddr、异常/uncache writeback 和已禁用的 overwrite 输出。
- `src/main/scala/xiangshan/mem/pipeline/StoreUnit.scala:272,430-543`：MAB 回流的 `isFrmMisAlignBuf`、child 分类和 MAB admission。
- `src/main/scala/xiangshan/cache/dcache/mainpipe/MissQueue.scala:311-367`：DCache CMOUnit error 寄存器保持与 CMO response producer 行为。
- `src/main/scala/xiangshan/cache/dcache/Uncache.scala:477-479`、`src/main/scala/xiangshan/mem/MemBlock.scala:414-418`：uncache store error 经两拍延迟及 `cache_error_enable` gate 后导出 BEU/`uncacheError`。
- `src/main/scala/xiangshan/mem/MemBlock.scala:1388-1396,1673-1703`、`src/main/scala/xiangshan/mem/vector/VMergeBuffer.scala:112-129,361-390`：MAB scalar terminal writeback 直达 `stOut(0)`，vector writeback 经 merge 回到 LSQ。
- `src/main/scala/xiangshan/mem/lsqueue/StoreQueueData.scala:140-268`：数据/mask 写入、16-byte byte-level forwarding 选择。

## 审查记录

| 日期 | 审查轮次 | 结论 | 范围 |
|---|---:|---|---|
| 2026-09-10 | 0 | 初稿建立，待接口组合、内部状态和异常专项独立复核。 | 全文。 |
| 2026-09-10 | 1 | 异常 coverage/dataflow 独立审查发现：必须区分 Dmode-only 与 exceptionAddr、正常 vector uncache 下发与 StoreUnit 异常转换、scalar/vector MAB 终点、SBuffer no-write drain、多 bit 最终 cause、redirect 交错以及 NC 跨 16B defect 排除。上述内容已补入 IF-32/33/35、IN-23 至 IN-30、异常边界表和正向/缺陷分类。 | 异常 producer、receiver、endpoint、redirect、MAB/vector/NC/CMO。 |
| 2026-09-11 | 2 | 异常路径复审发现 S2 ExceptionBuffer payload 的 `exceptionVec[7]` 由 `af` 直接覆盖，Re raw bit 7 不可作为独立 SAF source；同时要求明确 S1 `updateAddrValid=0` 是合法事件。已增加 `cp_s2_exception_encoding`、IN-25e、IF-08/IN-02 的 non-ready bin。 | S1/S2 owner、SAF 编码、ExceptionBuffer、地址 CAM/ready。 |
| 2026-09-11 | 3 | 最终异常事实复审发现 held request 与当拍 source 的 `{robIdx,uopIdx}` 完全相同时，静态输入顺序使当拍 source 优先；已增加 `held-vs-new-same-identity` bin。修订后复读确认 S2 `af`、同身份仲裁和 non-ready S1 无新增事实错误。 | 七个 ExceptionBuffer source、仲裁平手、S1/S2/redirect 生命周期。 |
| 2026-09-11 | 4 | 可读性与黑盒语义复审发现 flow 中混写 Scala `canAccept/resp` 与裁剪 RTL、将 NC 错写成 S2 回填、将 lane1 ready 错写成组合 prefix，以及遗漏 data frontier 的 MMIO 特例。已在 flow 和测试点同步修正；后续静态检索确认没有保留这些旧表述。 | standalone RTL interface、S1/S2 属性归属、DataBuffer/SBuffer 双 lane、ready frontier。 |
| 2026-09-11 | 5 | 第三轮异常专项复审发现 `mmioStout.trigger`、CMO `address/nderr` 在生成 StoreQueue 顶层不可观测；outstanding NC 在 ack 后可继续发新请求，RM 不能只保留单一 owner；普通 VMergeBuffer 的 Dmode-only 结果可能在 `ToLsqConnect` 中被编码为 COMMIT。已将这些场景改为 source/full-MemBlock watcher 或 protocol-negative，并补 BEU 两拍延迟与 `cache_error_enable` 覆盖。 | 生成顶层端口裁剪、VMergeBuffer、LSQWrapper、NC response owner、CMOUnit/Uncache/BEU。 |
| 2026-09-11 | 7 | 接口/forwarding 独立复审发现：`idResp.nc=0` 是 MMIO/I/O CBO.zero 的合法 ack；Re 在 `updateAddrValid=0` 时 payload 为 don't-care；forward 年龄 mask 不能被当成任意 56-bit 集合；S2 Re 异常位不回写 entry uop。 | 增加合法 NC/MMIO ack、Re inert payload、S1→redirect/reuse→Re 负向与 `UIntToMask` prefix 年龄窗口的覆盖边界；S2 exception 保持只检查 ExceptionBuffer/StoreUnit writeback。范围：Uncache ack、S1/S2 owner、forward query、异常 RM。 |
| 2026-09-11 | 8 | 最终接口、RTL、异常和可读性复读未发现新的事实矛盾；仍需把 ready frontier、forward blocker 选择和异常终点的限制写成可执行 coverage 约束。 | 确认每拍最多四项 frontier 扫描、MMIO 在数据前沿仅替代 `datavalid`、invalid index 不是“最老异常项”，并将 vector MMIO/NC 正常 sink、NC×cross16 缺陷、CMO/NC error 与 protocol-negative 明确排除或单列 watcher。异常章节的七源仲裁、多 bit cause、Dmode-only、MAB scalar/vector 分叉及 outstanding NC 多 owner 均有独立检测点。范围：收尾独立静态复审，交叉检查接口文档、flow 文档、Scala 条件和 emitted RTL 端口；全部接口组合 coverage、内部逻辑 coverage、异常与缺陷边界。 |
| 2026-09-11 | 9 | 异常专项末轮仍需确认 S1 ExceptionBuffer 的端口使能是否被误当成最终异常候选，以及 Uncache `idResp` 与最终 `resp` 的顺序约束是否过度收紧。 | 明确 S1 source port enable 与 `StaCfg` 非零过滤是两层条件；`idResp` 只按 request fire 接纳顺序关联，最终无 ID `resp` 的返回顺序由 Uncache 仲裁决定，顺序差异不单独判错。同步收紧 IF-24/IF-25 与 IN-24 的可执行检查条件。范围：最终接口/异常交叉复核，逐项对照 `StoreExceptionBuffer.scala` 与 `Uncache.scala` 的 source/response 生成逻辑。 |
| 2026-09-11 | 10 | 异常测试点在前九轮已覆盖主体来源、接收者、终点、优先级、redirect 和缺陷隔离，但需确认没有残留“待复核”或矛盾的正向 bin。 | 对 IF/IN 两章、覆盖模型、ignore/illegal bins、源码证据和关联文档做最终独立复读；未发现新增事实错误或遗漏。异常专项 review 正式关闭：正向功能、protocol-negative、已知缺陷和源码观察边界均已分栏，未运行仿真。范围：末轮独立 closure review；全部异常 producer/receiver/endpoint、向量与 MAB 分叉、NC/CMO/BEU、redirect 交错。 |
| 2026-09-11 | 11 | MAB 跨页测试点只覆盖 `crossPageWithHit=1` 的状态变化，且 DataBuffer ready 组合曾允许 `ready=01`。 | 增加“未命中但 MAB ready/未 ready”对照，明确命中未 ready 只阻断当前 parent 的正常 pair；同步将 DataBuffer ready 前缀纳入 protocol-negative，异常旁路单列为 no-write drain。 | 用户追问 `crossPageCanDeq=0` 与正常标量跨页流程；独立源码/生成 RTL 复核。 | MAB/SQ sideband、DataBuffer pair、跨页及异常覆盖。 |
| 2026-09-15 | 12 | CMO error 测试点曾把 SQ/Uncache 没有 error 输出过度推广成任何错误源都没有 BEU，且仍把 CMO 精确异常需求列为待确认。复核后已区分 L1/L2 本地 ECC 的独立 cache BEU、下游 CHI transaction error 和当前 CBO 精确 exception；保留当前 DUT 的 missing-propagation/stale-error defect watcher，并以 V2 上游 `7aa145db8f` 及其前序动态 reproducer 确认 CMO 精确异常是修复目标。 | CMO error source coverage、BEU 观察、SQ writeback exception 和缺陷回归。 |

异常专项审查结论：截至第 10 轮，未发现新增阻塞问题；上述正向、protocol-negative、已知缺陷和源码观察边界的分类均已固定。该结论基于当前 V2 Scala 与工作区 `build/rtl/StoreQueue.sv` 的静态核验，尚不等同于 VCS/全核波形验证。

## 待确认项

- 当前 V2 `vector MMIO`、`vector NC` 的**正常成功下发**在 `StoreQueue.scala` 中仍标为未实现；在设计 owner 明确支持前保持 exclude/fail-fast。StoreUnit 将实际落入这些地址的 vector store 转为 `storeAccessFault` 的异常转换路径不在 exclude 范围内，且分别检查 MMIO no-write drain 与 NC direct-complete。
- 本文不把 `nc && unaligned && cross16Byte` 当前 RTL 缺陷包装为功能要求；其 bug-regression 仅用于防止现象被误判或消失后无记录。
- CMO response error 在当前 SQ 中没有正常的 error 注入 receiver，但后续 V2 上游修复及其动态 reproducer
  已明确要求按当前 CBO owner 形成 `storeAccessFault/hardwareError`；它应继续作为当前 DUT 的已知缺陷
  回归点。NC response error 仍不会形成 SQ exception writeback，其架构处理意图继续保留为待确认边界。
