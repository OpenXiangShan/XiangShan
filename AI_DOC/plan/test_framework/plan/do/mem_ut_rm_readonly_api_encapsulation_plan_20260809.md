# mem_ut 测试框架 RM 只读 API class 最小封装计划

状态：已执行并归档（2026-08-11）。

实现评审：[mem_ut_rm_readonly_api_encapsulation_implementation_review_20260811.md](../../review_doc/undo/mem_ut_rm_readonly_api_encapsulation_implementation_review_20260811.md)。

关联 flow 已同步：

- [DCache/Uncache Memory Responder Flow](../../../../mem_ut_flow_doc/dcache_sbuffer_memory_responder_flow.md)
- [DCache 轻量 L2 Response、Hint 与 Probe Flow](../../../../mem_ut_flow_doc/dcache_l2_response_hint_probe_model_flow.md)

## 1. 专有名词与计划边界

| 术语 | 当前含义 | 代码落点或示例 |
|---|---|---|
| RM | 后续 reference model 组件。本计划只为它提供读取入口，不实现其建模、时序、比较或接入。 | `memblock_rm.sv` 不修改。 |
| 只读 API class | 唯一向 RM/checker 公开的 `memblock_rm_readonly_api` 单例 class。 | 后续 RM 只通过该 class 取得值型快照。 |
| backing map | 真实 DUT memory-facing read 已经懒建立的 `main_mem` 初始内存映射。 | 只表达初始化数据，不包含 DUT 后续写入。 |
| overlay | 测试框架按既有路径已经提交的 DCache writeback 或 Uncache store 数据。 | `write_overlay_mem`、`write_overlay_byte_valid`。 |
| 被动 observer | 仅在既有测试框架动作完成后记录事实的内部状态维护；它不决定、不修改也不延迟原动作。 | map 已写入后记录驻留数；batch 已提交后记录已提交状态。 |
| DCache 聚合快照 | 对 DCache 私有状态的只读摘要，包括驻留数、已发布状态和 drain 状态。 | 不暴露 `cached_line_by_addr` 或 payload。 |
| DCache overlay 可读标志 | 唯一提供给 RM 判断 DCache 写回 overlay 是否已可安全读取的状态。 | `dcache_overlay_read_ready=1` 表示当前 DCache 已空，且 observer 已确认相关写回完整提交。 |
| corrupt 范围 | 已观察到数据型 C response 的 payload 不可信，因而不允许后续 checker 对该地址范围做普通数据比较。 | API 对命中范围返回 `corrupt=1`。 |
| 值型 view | API 返回的独立标量、struct、数组副本；不含可写 live handle、queue 或 associative array 引用。 | RM 修改副本不能改变测试框架状态。 |
| miss | 调用者请求的 owner、表项、backing byte 或 overlay byte 不存在。 | API 报 `UVM_ERROR` 并返回 `valid=0`。 |

本计划的唯一目标是在测试框架侧新增一个供后续 RM 调用的统一只读 API class。可以按 RM 所需的数据形态设计 API 和 view，但不得新增、接入或实现任何 RM、checker、scoreboard、比较算法或调用流程。

最重要的范围约束如下：

- 不改变测试框架主体逻辑的协议含义和运行结果；
- 不改变 DCache/Uncache 的 A/C/D/E/Probe/Grant/Release 握手、驱动时序、response 调度、主内存懒分配、overlay 数据、batch 入队/提交顺序、reset 行为或 owner 退出条件；
- 不修改 `read_shared_mem_for_dut()`、`main_mem_access_task()`、`apply_shared_mem_write()`、`commit_shared_mem_write_batch()` 的既有读写语义；
- 不通过 API 发起等待、重试、提交、初始化、分配、修复或任何 DUT 接口访问；
- 不为解决 RM 读取问题而引入 C-data ticket、event 代次、预检、取消、重排或新的测试框架控制流；若后续发现既有测试框架主体本身需要修复，必须单独建立主体逻辑 plan，不混入本计划。

本计划只支持 `basicTest` 的既有 virtual-sequence 启动拓扑。该计划不增加 testcase、default sequence 或新的 responder 启动路径；只在现有支持拓扑中绑定已经实际运行的 DCache owner。

## 2. 抽象功能

`memblock_rm_readonly_api` 是测试框架状态与后续 RM 之间的只读 façade。它不拥有第二套主表、内存或 DCache 模型；它只探测既有 owner、复制既有数据或复制被动 observer 已发布的摘要，然后返回独立 value view。

该 class 提供四类能力：

1. 读取 dispatch 主表、status、UID 反查、issue membership、TLB 和 UID-TLB 的既有值型信息；
2. 读取已建立 backing map 的初始化数据；
3. 读取已经提交的 overlay 数据，并在命中已观察到的 corrupt 范围时返回保护性结果；
4. 读取 DCache 当前驻留与 drain 的聚合快照，以及唯一的 overlay 可读标志；两者均不扫描或暴露 DCache 私有 map。

API 只报告当前已存在的事实。RM 必须自行选择正确的读取时机；任何 miss 都是调用侧问题，而不是 API 触发框架状态前进的理由。

## 3. 唯一对外 API class

建议新增：

```text
mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_rm_readonly_api.sv
```

class 采用单例方式返回唯一句柄。只有该 class 的 singleton getter 可以创建 API class 自身；它不得调用 `common_data_transaction::get()`、任何 `ensure_*`、`get_or_create_*` 或会 `uvm_fatal` 的表项 getter。

### 3.1 句柄与上下文查询

抽象功能描述：返回已有 dispatch/shared-memory owner 是否可读，以及已有 sample/cycle、reset/flush/replay 上下文的值型副本；不创建 owner，也不推导 testcase generation。

API 只探测 `common_data_transaction::m_inst` 和已有 shared-memory lifecycle 标志。owner 或上下文不存在时报告 `UVM_ERROR`，返回 `valid=0`。

### 3.2 Dispatch 表查询

抽象功能描述：按 UID、ROB/LQ/SQ key 或 TLB key 复制既有 dispatch 信息，使 RM 能读取已经建立的测试框架事实；不创建 UID、status、TLB entry、issue item 或索引。

包括：

- 主 transaction 和 status 的值型 view；
- ROB/LQ/SQ 到 UID 的反查结果；
- issue queue membership 的轻量值型信息；
- TLB 和 UID-TLB 的纯值型 view。

TLB/UID-TLB view 不得返回 `memblock_tlb_entry`、`uid_tlb_record`、payload、CSR snapshot、queue 或任何 UVM object handle。需要的标量字段复制到 API class 自定义 view；动态数组只能复制为独立值数组，不能返回原 queue 引用。

### 3.3 初始化 backing map 查询

对外函数：`read_initialized_backing_for_rm(addr, byte_mask)`。

抽象功能描述：从已经存在的 `main_mem` backing map 复制请求字节的初始化数据，供后续 RM 使用；不读取 overlay，也不触发主内存懒分配。

API class 内部 private helper 仅检查 `main_mem.exists` 并复制对应 byte。所有请求 byte 均已存在时返回 `valid=1`；任一 backing miss 时统一 `UVM_ERROR + valid=0`。它不得调用 `ensure_main_line()`、`main_mem_access_task()`、`read_shared_mem_for_dut()` 或任何 DUT memory-facing 路径。

### 3.4 已提交 overlay 查询

对外函数：`read_committed_overlay_for_rm(addr, byte_mask)`。

抽象功能描述：读取已经由既有测试框架路径提交的 overlay byte；若请求范围命中 observer 已发布的 corrupt 范围，则返回保护性 corrupt 结果而不提供普通数据。

返回结果固定为值型组合：

- 正常 overlay 命中：`valid=1, corrupt=0, data_valid=1`；
- corrupt 范围命中：`valid=1, corrupt=1, data_valid=0`；
- overlay/byte-valid miss 或 shared-memory lifecycle 无效：`valid=0` 并报告 `UVM_ERROR`。

该 API 不回退到 `main_mem`，不修改 overlay、byte-valid、corrupt 状态或 pending batch。已提交 backing/overlay 的查询只依赖 shared-memory lifecycle，不依赖 DCache owner 是否仍发布。

### 3.5 DCache 聚合查询

抽象功能描述：返回唯一 DCache owner 已发布的驻留/drain 值型摘要，并向 RM 提供单一的 `dcache_overlay_read_ready` 判断结果；RM 不需要自行组合 resident、pending、assembly 或 corrupt 等内部状态，也不扫描 `cached_line_by_addr`、Probe record 或 batch。

对外函数：`get_dcache_overlay_readiness_for_rm()`。

该函数只返回值型 `{valid, ready}` 结果，其中 `ready` 对应已发布快照中的 `dcache_overlay_read_ready`。`valid=0` 表示 owner 未发布、observer 输入不完整或快照已失效；API 必须报告 `UVM_ERROR`。`valid=1, ready=0` 表示 observer 已正常工作但 DCache 当前尚未达到可读条件，不是查询 miss，也不触发 `UVM_ERROR`。RM 只以 `valid=1 && ready=1` 作为读取 `read_committed_overlay_for_rm()` 的统一门槛；它不需要读取并自行判断其他 DCache 聚合字段。

快照至少包含：`published`、owner 标识、DCache lifecycle generation、`dcache_valid_line_count`、由 count 推导的当前为空结果、待提交 DCache writeback 数、已观察 corrupt line 数、`dcache_drain_complete`、`dcache_overlay_read_ready`、`dcache_drain_epoch` 以及 drain 转换 sample/time。除 `get_dcache_overlay_readiness_for_rm()` 返回的 `{valid, ready}` 外，其余字段只作为同一 API class 内的诊断型值型快照，RM 不以它们拼接读取时机。

`dcache_valid_line_count` 只表示协议驻留 line 数，内部以已有 record 的 `alias_valid` 为判据；它不表示 payload 完整性、clean/dirty 或 `data_valid`。

`dcache_drain_complete` 只表示当前已发布 owner 的 observer 已经看到：无驻留 line、无已登记但尚未观察到既有 batch commit 的 DCache writeback、无未完成 C-data assembly、无已知 corrupt 范围。它不是 `DCACHE_L2_FLUSH_DONE` 的替代，也不保证未来不会有新的 Acquire 或 Uncache 覆盖。

`dcache_overlay_read_ready` 是测试框架内部从同一份 aggregate snapshot 一次性推导的统一门槛，不单独维护第二份可漂移的布尔状态。只有以下条件同时满足时它才为 `1`：当前 owner 已发布；observer 已具备所有必需输入；`dcache_valid_line_count==0`；没有未完成 C-data assembly；没有已登记但尚未观察到既有 batch commit 的 DCache writeback；没有已知 corrupt byte 范围；以及每条已登记的 64 B DCache writeback 均已观察到低、高两个既有 32 B fragment 完成 overlay 提交。任一新 Acquire、C-data assembly、待提交 writeback、corrupt 观察结果、owner 失效或 observer 自检不一致都必须使该标志为 `0` 或使快照 `valid=0`。该标志只表示当前已发布快照时刻可读取，不承诺后续不会有新的 Acquire 或 Uncache 覆盖。

只有当前 DCache owner 已发布快照时该 API 才有效。owner 退出后 aggregate 查询返回 invalid；后续 RM 若需要 drain，必须在 owner 仍发布时读取。

## 4. 最小被动 observer 状态

为避免 API 每拍扫描 DCache 私有 map，本计划允许增加最小的内部 observer 状态。observer 只记录既有动作的结果，不参与该动作的选择、执行或失败处理。

### 4.1 DCache 驻留 observer

抽象功能描述：在现有 `cached_line_by_addr` 写入、覆盖、删除和清空已经完成后，依据写前/写后 record 的 `alias_valid` 差异更新驻留计数；它不替换 map 写入口，不改变 map 内容，也不改变调用者控制流。

现有 Grant wait、GrantAck、Probe、Release、ReleaseData、ProbeAckData、alias conflict 和 reset 路径仍按原代码执行。observer 仅在这些既有路径完成 map 变化后记录 `0 -> 1`、`1 -> 0` 或不变的计数差值。低频 debug 边界可比对实际 `alias_valid` 条目数与 observer count；禁止每拍扫描。

实施时必须先完成现有 `cached_line_by_addr` 写入、覆盖、删除和整体清空点的检索清单，并在每个既有动作之后追加 observer 通知；不得借此把 map 写入口重构为新控制路径。若 observer 自检发现遗漏或不一致，只将 aggregate 标为 unavailable，不能回写 map、改变 DUT 响应或在 API 未被调用时额外改变 testcase 结果；后续 API 查询该 unavailable 项时才统一产生 `UVM_ERROR`。

### 4.2 C-data、writeback 与 corrupt observer

抽象功能描述：观察既有 C-data assembly 结果、既有 DCache batch 入队和既有 batch 提交结果，发布 pending、corrupt 范围和 drain 所需的旁路摘要；它不改变 C-data 收集、overlay 写入、batch event 内容或 commit 顺序。

规则如下：

- 首拍/完整 C-data assembly 只记录 observer 的 in-flight 状态，不改变原 assembly 状态机；
- 既有 C response 已被框架判为 corrupt 时，observer 记录对应 64 B 范围为 corrupt，不伪造 overlay 数据；
- 既有正常 DCache writeback 已经完整提交到 overlay 后，observer 才可清除该完整 64 B 范围的 corrupt 标记；对一个拆分为低/高两个 32 B fragment 的 64 B writeback，只有两个 fragment 均已观察到既有 overlay commit 才视为完整提交。既有 Uncache store 已经实际提交后，只可清除它实际覆盖的 byte 范围。observer 不决定该写是否提交，也不修改写入数据；若无法仅凭既有完成事实确认两个 fragment 均已完成，则该 writeback 保持未确认，`dcache_overlay_read_ready` 不得置 `1`，不得改变主体路径补建确认机制；
- DCache pending 只镜像既有 DCache batch 已登记和既有 commit 已完成的事实；observer 不新增 ticket、不重排 event、不取消 event；
- 任一 observer 状态更新后重新计算并发布 DCache aggregate snapshot。发布使用一个值型 snapshot assignment，API 只复制已发布副本。

若既有框架无法在不改变主体逻辑的前提下提供某个 observer 输入，API 必须将该项结果标记为 `valid=0` 并报告 `UVM_ERROR`；不得为了让 RM 获得结果而改变既有协议或内存行为。

### 4.3 Owner、reset 与 Flush

抽象功能描述：把 observer 归属绑定到现有 basicTest 中实际运行的唯一 DCache responder，并在既有启动、reset、退出和 Flush 状态变化后发布或失效摘要；不改变这些流程的原有时序。

`reset_all_tables()` 继续只管理 dispatch 主表，不负责修改 DCache observer。DCache owner 的已有启动、runtime reset 和退出位置负责 observer 的发布、清理或失效；这些仅是旁路记录，不得改变 overlay、batch、主表或 DCache 私有 map 的现有 reset 行为。

observer 在既有 runtime reset/owner 退出后可以将相关 DCache aggregate 或 corrupt 观察结果标为 unavailable；API 必须如实返回 invalid，而不是依据 overlay 是否保留去推断或改变 reset 语义。reset 后何时重新建立可读 observer 快照只跟随既有 owner 的下一次实际生命周期事件。

`DCACHE_L2_FLUSH_DONE` 只可作为低频 observer 自检边界，且只在进入 DONE 的单次边沿检查；DONE 保持期间不得扫描。它不作为“DCache 当前为空”或 drain 的真源。

## 5. 只读性和错误语义

所有 API 返回 API class 定义的 value view。禁止返回 live UVM object、queue、associative array、`ref`、`inout` 或任何可写底层 handle。

每个查询必须先进行非创建、非 fatal 的 owner、边界、`exists` 和 null 检查。未命中时：

1. 产生一次 `UVM_ERROR`；
2. 返回 `valid=0` 的值型结果；
3. 不调用创建型/fatal 型 getter；
4. 不改变主表、TLB、main memory、overlay、DCache 状态、observer、epoch、时间戳、queue 或 batch。

corrupt 命中是成功的保护性结果，不是 miss；它返回 `valid=1, corrupt=1, data_valid=0`，不读取或合成普通数据。

## 6. 明确不在本计划内的内容

- `memblock_rm.sv`、RM 句柄获取、RM API 调用、reference value 推导、checker/scoreboard/coverage 实现；
- 任何 DCache/Uncache 协议修复、主内存模型重构、batch/commit/reset 语义调整或 response 调度调整；
- 修改现有 DUT 读写路径、补建 backing map、强制提交 batch、等待下一个 sample、重排 event、改变 C-data 处理；
- 新增 testcase、default sequence、agent、adapter、registry 或第二个对外 API class；
- 通过 API 或 observer 修复既有测试框架主体逻辑问题。

## 7. 建议文件落点与最小改动

- 新增 `memblock_rm_readonly_api.sv`，实现唯一对外 singleton、private peek helper 和全部 value view；
- 在 `common_data_transaction.sv` 增加非创建 singleton/table peek，以及存放 DCache aggregate value snapshot 的最小字段；`reset_all_tables()` 保持既有主体职责；
- 在 `mem_base_sequence.sv` 的既有 DCache map/C-data/batch 完成点追加 observer 通知或旁路状态更新；不得改写原函数输入、返回值、条件分支、batch event、commit 顺序或 reset/退出控制；
- 在 `seq_pkg.sv`/`seq.f` 收录 API class 文件。

任何 observer 通知无法安全追加到既有完成点时，该 API 项应返回 unavailable，而不是修改主体逻辑来制造通知点。

## 8. 实施阶段

1. 定义 `memblock_rm_readonly_api` 的 singleton、统一结果 view 和私有非创建 peek helper。
2. 实现 dispatch、TLB、UID-TLB、backing map 和 overlay 的直接只读查询；先保证所有返回均为值型副本。
3. 以既有 DCache 生命周期完成点为唯一输入，追加不影响原行为的 resident/C-data/pending/corrupt observer；发布 DCache aggregate snapshot。
4. 接入 API class 编译收录，并对所有 API miss、corrupt 命中、owner 未发布和 owner 退出后的 backing/overlay 查询做只读性检查。
5. 对 basicTest 既有场景运行对比：API class 未被调用与被调用时，原有接口时序、main_mem/overlay 写入、batch 提交、reset/退出及 testcase 结果必须保持一致。

## 9. 验收目标

1. 测试框架只新增一个对 RM 公开的 `memblock_rm_readonly_api` class。
2. 不修改 RM/checker/scoreboard，也不新增任何 RM 调用。
3. API 所有结果均为 value view；修改返回副本不影响主表、TLB、UID-TLB、backing、overlay 或 observer。
4. backing miss、overlay miss、owner 未发布、无效 key 和不存在 singleton 均为 `UVM_ERROR + valid=0`，且不创建或修改框架状态。
5. `read_initialized_backing_for_rm()` 只读已存在 `main_mem`；`read_committed_overlay_for_rm()` 只读已提交 overlay，二者不互相回退。
6. corrupt 范围命中返回保护性成功结果，API 不返回伪造数据。
7. DCache aggregate 来自被动 observer，不扫描或暴露 `cached_line_by_addr`；`DCACHE_L2_FLUSH_DONE` 不作为空/drain 的推导来源。
8. `get_dcache_overlay_readiness_for_rm()` 是 RM 判断 overlay 读取时机的唯一 DCache 门槛：仅当返回 `valid=1, ready=1` 时才允许读取 overlay；RM 不自行组合 resident、pending、assembly 或 corrupt 字段。
9. `dcache_overlay_read_ready` 只能由同一 aggregate snapshot 一次性推导；每条已登记的 64 B DCache writeback 均已观察到低、高两个 32 B fragment 的既有 overlay commit 后才可置 `1`。任一 fragment 未确认、corrupt、observer 不完整或 owner 无效时不得发布 ready。
10. observer 只能在既有动作完成后记录状态；不得改变任何既有协议、读写、batch、reset、owner、response 或退出行为。
11. API 不调用 DUT 接口、不等待 sample、不提交 batch、不初始化 backing、不消费 queue。
12. 在 basicTest 既有 regression 中，对比 API 未调用/调用两种情况，确认原有测试框架主体逻辑和仿真结果一致。

## 10. 与此前扩展方案的差异说明

此前讨论中过度引入了 C-data ticket、event epoch、full-line preflight、batch cancel、强制提交边界和 DUT memory-facing 读写路径调整。这些方案会扩大到测试框架主体逻辑，违背“仅封装 RM 只读 API class、主体行为不变”的目标，因此不纳入本计划。

最终方案仅保留：一个对外 API class、既有状态的值型复制、必要的非创建 peek，以及只在既有动作完成后记录事实的旁路 observer。任何无法通过旁路 observer 安全获取的信息，以 API unavailable/`UVM_ERROR` 表示，留给独立主体逻辑 plan 处理。

## 执行中补充/修正（IMPLEMENTATION_DELTA）

### [IMPLEMENTATION_DELTA] 阶段一：只读 API class 与非创建值型查询

- 新增 `memblock_rm_readonly_api` singleton class，并在 `seq_pkg.sv` 中按依赖顺序收录；该 class 是唯一对外入口。
- dispatch 主表、status、ROB/LQ/SQ 反查、TLB 和 UID-TLB 查询均直接探测已有 static owner/table/map，复制到 API 自定义值型 view；不会调用创建型或 fatal 型 getter。
- 初始化 backing 与已提交 overlay 查询共用非创建的 byte 级探测逻辑；当前 overlay corrupt mask 和 DCache aggregate 的 observer 更新在后续阶段实现，尚未把 `ready` 发布为有效状态。
- 本阶段只完成测试框架 API 封装，不实现 RM/checker 调用，也不改变 DUT memory-facing 主流程。

### [IMPLEMENTATION_DELTA] 阶段二：backing/overlay 查询与写回事实 observer

- `main_mem`、`write_overlay_mem` 和 byte-valid map 继续由既有 memory-facing 流程维护；API 只按请求 byte 探测已有 map，不调用懒分配或 DUT 访问 task。
- 新增 byte-granular `write_overlay_corrupt_byte_mask` 旁路表。数据型 C response 已被既有代码判为 corrupt 时记录整条 64 B；Uncache batch 已实际提交后只清除其覆盖 byte。
- DCache batch 入队/提交处增加旁路 fragment 记录：低、高两个 32B fragment 均被观察到既有 commit 后，才结束该 line 的未完成观察窗口；observer 不新增 ticket、不改变 batch 顺序。
- 该阶段仍未实现 RM 调用或 checker；所有状态只通过后续 API value view 读取。

### [IMPLEMENTATION_DELTA] 阶段三：只读生命周期门控与 corrupt 计数收敛

- 增加 `read_framework_context_for_rm()`，只复制已有 dispatch owner 的主表就绪、UID 进度、reset/flush、replay pending 数和当前 sample 等标量上下文；不返回 queue 本体，不修改任何调度状态。
- `read_initialized_backing_for_rm()` 与 `read_committed_overlay_for_rm()` 在访问 map 前先检查既有 shared-memory lifecycle；生命周期未初始化时统一 `UVM_ERROR + valid=0`，仍不创建 line 或 owner。
- 将 aggregate 的 corrupt 计数改为以 64 B DCache line 为粒度的旁路 byte mask。1KiB `write_overlay_corrupt_byte_mask` 继续是 API 的逐 byte 查询真源；新增的 64 B map 只用于准确维护“仍有多少条 corrupt DCache line”，避免相邻多条 64 B line 落入同一 1KiB backing line 时提前或滞后减计数。
- 正常 DCache 写回仍只在低、高两个既有 32 B fragment 均观察到 commit 后清除整条 64 B corrupt 范围；Uncache commit 只清除自身实际覆盖的 byte。fragment 观察缺少已登记输入或发生计数下溢时，仅使 aggregate unavailable，不回写 DCache 私有 map 或 batch。
- `dcache_drain_epoch` 与 transition sample/time 仅在已发布 aggregate 从未 drain 变为 drain 的边沿记录，表示门槛转换而非 overlay 最近提交时间。DCache aggregate 仍实际存放在 shared-memory owner `mem_access_base_sequence`，而非 `common_data_transaction`，以保持共享 memory/observer 状态只有一个生命周期 owner；该变化不增加第二个对外 API class。
- DCache responder 在 reset 采样期间先使当前 aggregate `published=0`、清除未完成 observer 片段并只在第一次失效时递增 generation；reset 保持期间不重复递增。reset 解除后的第一个正常采样边界重新发布基线。该过程不强制提交当前 sample，仍由既有下一次 `begin_shared_mem_sample()` 提交上一 sample 的 batch。
- fragment commit observer 只检查本次 commit event 实际触及的 64 B line 集合，不遍历全部 fragment map；因此不会把事件驱动观察退化为每次提交的全表扫描，未改变 batch 的入队、提交或 overlay 可见性顺序。
