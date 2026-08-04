# V2 RTL 内部 Flow 知识索引

## 版本范围

- RTL 版本：V2。
- 版本 profile：`mem_ut/ver/ut/memblock/rule/version/v2`。
- 长期知识目录：`AI_DOC/analysis/rtl/v2/flows`。

本索引只收录经 V2 权威 RTL/Scala 源码核验的内部功能 flow。历史专项分析仍可保留在 `AI_DOC/analysis/rtl`，但新增长期知识统一进入本版本 `flows/`。

## Flow 文档

| Flow 文档 | 关键词 | 覆盖模块 | 入口信号/函数 | 关联 Agent/Flow |
|---|---|---|---|---|
| [Memory flushPipe flow](flows/memory_flush_pipe_flow.md) | `flushPipe`、`sfence.bits.flushPipe`、SFENCE、Svinval、`VSETVL`、`NewCSR`、`TLBNonBlock`、DTLB hit/miss、CBO、CMO、`flushAfter`、`s3_flushPipe`、`s3_rep_frm_fetch`、STA0/STA1 | Decode、Fence FU、CSR/NewCSR、MemBlock、TLB、TLBStorage、PTW、LoadUnit、HybridUnit、StoreUnit、StoreQueue、ExceptionGen、ROB | Decode `flushPipe`、NewCSR `resetSatp/triggerFrontendChange/status/vstart/frm`、`sfence.valid`、`deqCanDoCbo`、`mmioStout` | mem_ut sfence flow、memory trigger flow、int writeback agent |
| [Memory trigger flow](flows/memory_trigger_flow.md) | `trigger`、`TriggerAction`、`BreakpointExp=0`、`DebugMode=1`、`Trace=2/3/4`、`None=15`、`tdata`、`triggerCanRaiseBpExp`、LDA/STA/STD | NewCSR Debug、MemBlock、LoadUnit、StoreUnit、StoreQueue、ExceptionGen、ROB | CSR `mem_trigger`、Load/Store S1 vaddr、split writeback trigger | memory flushPipe flow、int writeback agent |
| [LSQ 入队与 Redirect 恢复 flow](flows/lsq_enqueue_redirect_flow.md) | LSQ enqueue、`needAlloc`、`canAccept`、`ldCanAccept`、`sqCanAccept`、`LSQLdEnqWidth`、`LSQStEnqWidth`、`numLoadDp`、`numStoreDp`、`IssueBlockParams.numEnq`、`RenameWidth`、registered credit、redirect、cancel、pointer recovery、`flushPipe`、vector LS fault、AMO/MOU | NewDispatch、LsqEnqCtrl、LsqWrapper、VirtualLoadQueue、StoreQueue、VMergeBuffer、VSegmentUnit、AtomicsUnit、Scheduler、IssueQueue、ROB | `fromRename.fire`、`LsqEnqCtrl.do_enq`、`RegNext(canAccept)`、`RobPtr.needFlush`、vector `toLsq` feedback | memory flushPipe flow、memory trigger flow、ROB 压缩与后端指令信息流 |
| [ROB 压缩与后端指令信息流](flows/rob_compress_and_backend_instruction_flow.md) | ROB、RAB、`RobEntryBundle`、`canRobCompress`、`instrSize`、`realDestSize`、`uopNum`、`robIdx`、`replayInst`、`matchInvalid`、`has_exception`、`commitType`、`exceptionHappen`、`RegEnable`、`isStoreException`、fault store、scalar load fault、scalar MMIO fault、early CBO fault、vector load/store fault、FOF、AMO、MOU、AtomicsUnit、`wline`、`flush`、`flushAfter`、`pendingPtr`、`pendingst`、`pendingMMIOld`、`scommit`、`committed`、`completed`、`lqDeq`、`sqDeq`、`sqCancelCnt`、uncache、MMIO fault | Decode、Rename、CompressUnit、NewDispatch、HybridUnit、ROB、RAB、ExceptionGen、IssueQueue、LSQ、VirtualLoadQueue、StoreQueue、VMergeBuffer、VSegmentUnit、AtomicsUnit、SBuffer、Uncache、MemBlock writeback | Decode `canRobCompress`、Rename `robIdx` 分配、HybridUnit `s3_rep_frm_fetch`、vector `toLsq` feedback、atomic writeback、ROB `exceptionHappen/commitType`、`isStoreException`、ROB exception redirect、StoreQueue request/completion/deq/cancel | memory flushPipe flow、memory trigger flow、LSQ 入队与 Redirect 恢复 flow、int writeback agent |
| [Memory PMP/PMA 权限检查 flow](flows/memory_pmp_pma_permission_flow.md) | `tlbCsr_priv_debug`、`priv.debug`、PMP、PMA、`debugStart`、`debugEnd`、access fault、MMIO、atomic | NewCSR、MemBlock、Frontend、L2TLB、PMPChecker、PMA checker | `io.tlb.debug`、`PMPCheckerEnv.debug`、`pmp_match_res`、`pma_match_res` | memory trigger flow、memory flushPipe flow |
| [DCache-L2 refill hint 与 L2 flush done flow](flows/dcache_l2_refill_hint_and_flush_done_flow.md) | `io_l2_hint_valid`、`sourceId`、`isKeyword`、GrantData、MSHR、critical beat、`io_l2_flush_done`、`mflushpwr` | CoupledL2、CustomL1Hint、GrantBuffer、L2Top、DCache MissQueue、LoadQueueReplay、MemBlock、NewCSR | DCache Acquire echo、L2 MainPipe s1/s3、`l2Flush` | LSQ replay flow、低功耗 flow、L2 sideband agent |
| [L2 内侧 TileLink 请求、权限与回复 flow](flows/l2_inner_tilelink_request_response_flow.md) | `AcquireBlock`、`AcquirePerm`、`Grant`、`GrantData`、`AccessAckData`、`CBOAck`、`NtoB`、`NtoT`、`BtoT`、source range、sink、GrantAck、`AliasKey`、`user.alias`、`cache_alias`、`aliasTask`、`B.data[2:1]`、`IsKeywordKey`、`echo.isKeyword`、ICache、PTW、Uncache | DCache、ICache、L2TLB、Uncache、L2Top、CoupledL2 MainPipe/MSHR/GrantBuffer/SourceB | L1 A/C/E channel、`odOpGen()`、`ClientMetadata.onAccess()`、CMOUnit、DCache/L2 alias Probe、keyword A/D/hint 路径 | DCache-L2 refill hint 与 L2 flush done flow、L2 sideband responder |
| [DTLB-L2TLB 多请求与 Response 次序 flow](flows/dtlb_l2tlb_request_response_ordering_flow.md) | `PTWNewFilter`、`PTWFilterEntry`、`PtwReq`、`PtwRespS2`、multi-outstanding、response ordering、`mergeArb`、sfence | MemBlock DTLB、PTWNewFilter、L2TLB、PtwCache、PTW、LLPTW | `dtlbRepeater.io.ptw.req(0).fire`、`io.tlb(1).resp.fire` | L2TLB agent、memory flushPipe flow、PMP/PMA flow |
| [MMU GPF/AF 异常优先级与并发边界 flow](flows/mmu_gpf_af_exception_priority_flow.md) | `gpf`、`gaf`、`af`、`level`、`levelNext`、`getVpnn`、Sv39、Sv48、`PtwCache`、`PtwEntry`、`PTWEntries`、`cacheline`、`sector`、`blockBytes_align`、`sel_pte`、`l3Hit`、`l3Refill`、`noS2xlate`、`allStage`、`onlyStage1`、`onlyStage2`、VS-stage、G-stage、PLRU、`perm_check`、`HPTW`、`PTW`、`LLPTW`、`PtwRespS2`、`hasPf`、异常优先级 | PtwCache、HPTW、PTW、LLPTW、L2TLB、L1 TLB、LoadUnit、StoreUnit、HybridUnit、ROB/CSR | TLB request、64-byte page-table Get、PtwCache request/refill、PTW/HPTW request fire、`HptwResp.apply`、`TLB.perm_check`、`TlbResp.excp` | DTLB-L2TLB 多请求与 Response 次序 flow、PMP/PMA flow、L2TLB agent |

## 维护规则

- 新分析优先合并到已有 flow；新增 flow 后必须补充上表。
- 表中关键词应包含用户可能搜索的字段原名和行为同义词。
- V3 差异链接到 `../v3/index.md` 或对应 V3 flow，不在本文混写 V3 事实。
