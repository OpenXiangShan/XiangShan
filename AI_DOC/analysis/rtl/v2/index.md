# V2 RTL 内部 Flow 知识索引

## 版本范围

- RTL 版本：V2。
- 版本 profile：`mem_ut/ver/ut/memblock/rule/version/v2`。
- 长期知识目录：`AI_DOC/analysis/rtl/v2/flows`。

本索引只收录经 V2 权威 RTL/Scala 源码核验的内部功能 flow。历史专项分析仍可保留在 `AI_DOC/analysis/rtl`，但新增长期知识统一进入本版本 `flows/`。

## Flow 文档

| Flow 文档 | 关键词 | 覆盖模块 | 入口信号/函数 | 关联 Agent/Flow |
|---|---|---|---|---|
| [Memory flushPipe flow](flows/memory_flush_pipe_flow.md) | `flushPipe`、SFENCE、HFENCE、CBO、CMO、`flushAfter`、`s3_flushPipe` | Decode、Fence FU、MemBlock、LoadUnit、StoreUnit、StoreQueue、ExceptionGen、ROB、TLB | Decode `flushPipe`、`deqCanDoCbo`、`mmioStout` | mem_ut sfence flow、memory trigger flow |
| [Memory trigger flow](flows/memory_trigger_flow.md) | `trigger`、`TriggerAction`、breakpoint、Debug Mode、`tdata`、`triggerCanRaiseBpExp` | NewCSR Debug、MemBlock、LoadUnit、StoreUnit、ExceptionGen、ROB | CSR `mem_trigger`、Load/Store S1 vaddr | memory flushPipe flow、V2 interface signal matrix |
| [LSQ 入队与 Redirect 恢复 flow](flows/lsq_enqueue_redirect_flow.md) | LSQ enqueue、`needAlloc`、`canAccept`、`ldCanAccept`、`sqCanAccept`、`LSQLdEnqWidth`、`LSQStEnqWidth`、`numLoadDp`、`numStoreDp`、`IssueBlockParams.numEnq`、`RenameWidth`、registered credit、redirect、cancel、pointer recovery、`flushPipe` | NewDispatch、LsqEnqCtrl、LsqWrapper、VirtualLoadQueue、StoreQueue、Scheduler、IssueQueue、ROB | `fromRename.fire`、`LsqEnqCtrl.do_enq`、`RegNext(canAccept)`、`RobPtr.needFlush` | memory flushPipe flow、memory trigger flow |
| [ROB 压缩与后端指令信息流](flows/rob_compress_and_backend_instruction_flow.md) | ROB、RAB、`RobEntryBundle`、`canRobCompress`、`instrSize`、`realDestSize`、`uopNum`、`robIdx` | Decode、Rename、CompressUnit、NewDispatch、ROB、RAB、ExceptionGen、IssueQueue、LSQ | Decode `canRobCompress`、Rename `robIdx` 分配、ROB enqueue/writeback/commit | memory flushPipe flow、memory trigger flow、LSQ 入队与 Redirect 恢复 flow |

## 维护规则

- 新分析优先合并到已有 flow；新增 flow 后必须补充上表。
- 表中关键词应包含用户可能搜索的字段原名和行为同义词。
- V3 差异链接到 `../v3/index.md` 或对应 V3 flow，不在本文混写 V3 事实。
