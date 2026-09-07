# V2 顶层 Interface-Agent 知识索引

## 版本范围

- RTL 版本：V2。
- 版本 profile：`mem_ut/ver/ut/memblock/rule/version/v2`。
- 长期知识目录：`AI_DOC/analysis/interface/v2/agents`。

本索引只收录经 V2 权威源码核验的 MemBlock 顶层接口与 agent 映射。历史专项分析仍可保留在本目录，但新增长期知识统一进入 `agents/`。

## Agent 文档

| Agent 文档 | 关键词 | 覆盖模块/端口 | 入口信号 | 关联 Flow |
|---|---|---|---|---|
| [Vector issue agent](agents/vecissue_agent.md) | `issueVldu`、`enqLsq`、`needAlloc`、`numLsElem`、`LsqEnqCtrl`、随机约束、descriptor、`src_4`、`VConfig`、`vl`、`flowNum`、`flowMask`、`src_3`、`vm`、`vstart`、`vuopIdx`、`vsew`、`vlmul`、`veew`、`pdest`、`lqIdx/sqIdx`、`uop_vpu_lastUop`、`uop_vpu_isVleff`、`lastUop`、`vecLastFlow`、`vecExceptionFlag`、`isVleff`、`vleff.v`、FOF、`fix-VL`、FOF 发射顺序、`lqDeqPtr`、`VfofBuffer`、`vecReplayMask`、`writebackVldu`、`vstuIqFeedback` | MemBlock `io_ooo_to_mem_enqLsq_*`、`io_ooo_to_mem_issueVldu_0/1`、VLSplit/VSSplit、LsqEnqCtrl、VfofBuffer、VSegmentUnit、StoreQueue | `enqLsq.req.valid`、`needAlloc`、`numLsElem`、`issueVldu.valid && ready`、`src_4[7:0]`、`uop.vpu.*`、`flowNum`、`isVleff && lastUop`、`isVleff && (lqIdx == lqDeqPtr)`、`lastUop -> vecLastFlow`、`vstuIqFeedback.feedbackSlow` | V2 正常向量访存 uop 与 flow 拆分、LSQ 入队与 Redirect 恢复 flow |
| [DCache agent](agents/dcache_agent.md) | `auto_inner_dcache_client_out_a/b/c/d/e`、`user_alias`、`user_vaddr`、`user_needHint`、`echo_isKeyword`、FENCE、HFENCE.GVMA、HFENCE.VVMA、`s1_kill/s2_kill`、CBO、`b_data[2:1]`、`b_data[0]`、Probe、ProbeAckData、GrantAck、source、sink、`corrupt`、`TLError`、`L1ErrorMetaArray` | MemBlock DCache TileLink client、DCacheWrapper、LoadPipe、StorePipe、MissQueue、WritebackQueue、CoupledL2 SourceB/SinkA/MainPipe | A/C/E DUT request、B/D responder response；Fence 仅经 LSU kill 间接影响 | Memory flushPipe、DCache-L2 refill hint 与 L2 flush done、L2 内侧 TileLink 请求、权限与回复 flow |
| [Int writeback agent](agents/int_writeback_agent.md) | `writebackLda`、`writebackSta`、`writebackStd`、`replayInst`、`trigger`、`flushPipe`、split lane | MemBlock、LoadUnit、StoreUnit、StoreQueue、Backend int writeback | `io_mem_to_ooo_writebackLda/Sta/Std_*_valid` | memory trigger、memory flushPipe、ROB/ExceptionGen flow |
| [L2TLB agent](agents/l2tlb_agent.md) | `dtlbRepeater`、`inner_ptw`、`vpn`、`s2xlate`、PtwReq、CSR history、`priv_virt_changed`、`dvirt`、MPRV、MPV、C-2、multi-outstanding、raw hit、UID multicast、due response、C4、barrier | MemBlock internal DTLB/L2TLB request-response | `_inner_dtlbRepeater_io_ptw_req_0_*`、`_inner_ptw_io_tlb_1_*`、`ptwResp_valid`、`priv_virt_changed` | DTLB-L2TLB 多请求与 Response 次序 flow、Memory flushPipe flow、PMP/PMA flow |

## 维护规则

Vector issue agent 还覆盖目的写使能关键词：`vecWen`、`v0Wen`、`vlWen`、`ldest`、`pdest`；
这些字段由 Decode/目的寄存器类别派生，不能独立随机。

- 新增 agent 文档后必须补充上表。
- 一个端口只指定一个主要 agent，跨 agent 观察关系通过交叉引用表达。
- V3 差异链接到 `../v3/index.md` 或对应 V3 agent 文档，不在本文混写 V3 事实。
