# V2 顶层 Interface-Agent 知识索引

## 版本范围

- RTL 版本：V2。
- 版本 profile：`mem_ut/ver/ut/memblock/rule/version/v2`。
- 长期知识目录：`AI_DOC/analysis/interface/v2/agents`。

本索引只收录经 V2 权威源码核验的 MemBlock 顶层接口与 agent 映射。历史专项分析仍可保留在本目录，但新增长期知识统一进入 `agents/`。

## Agent 文档

| Agent 文档 | 关键词 | 覆盖模块/端口 | 入口信号 | 关联 Flow |
|---|---|---|---|---|
| [DCache agent](agents/dcache_agent.md) | `auto_inner_dcache_client_out_a/b/c/d/e`、`user_alias`、`user_vaddr`、`user_needHint`、`echo_isKeyword`、FENCE、HFENCE.GVMA、HFENCE.VVMA、`s1_kill/s2_kill`、CBO、`b_data[2:1]`、`b_data[0]`、Probe、ProbeAckData、GrantAck、source、sink、`corrupt`、`TLError`、`L1ErrorMetaArray` | MemBlock DCache TileLink client、DCacheWrapper、LoadPipe、StorePipe、MissQueue、WritebackQueue、CoupledL2 SourceB/SinkA/MainPipe | A/C/E DUT request、B/D responder response；Fence 仅经 LSU kill 间接影响 | Memory flushPipe、DCache-L2 refill hint 与 L2 flush done、L2 内侧 TileLink 请求、权限与回复 flow |
| [Int writeback agent](agents/int_writeback_agent.md) | `writebackLda`、`writebackSta`、`writebackStd`、`replayInst`、`trigger`、`flushPipe`、split lane | MemBlock、LoadUnit、StoreUnit、StoreQueue、Backend int writeback | `io_mem_to_ooo_writebackLda/Sta/Std_*_valid` | memory trigger、memory flushPipe、ROB/ExceptionGen flow |
| [L2TLB agent](agents/l2tlb_agent.md) | `dtlbRepeater`、`inner_ptw`、`vpn`、`s2xlate`、PtwReq、CSR history、C-2、multi-outstanding、raw hit、UID multicast、due response、C4、barrier | MemBlock internal DTLB/L2TLB request-response | `_inner_dtlbRepeater_io_ptw_req_0_*`、`_inner_ptw_io_tlb_1_*`、`ptwResp_valid` | DTLB-L2TLB 多请求与 Response 次序 flow、Memory flushPipe flow、PMP/PMA flow |

## 维护规则

- 新增 agent 文档后必须补充上表。
- 一个端口只指定一个主要 agent，跨 agent 观察关系通过交叉引用表达。
- V3 差异链接到 `../v3/index.md` 或对应 V3 agent 文档，不在本文混写 V3 事实。
