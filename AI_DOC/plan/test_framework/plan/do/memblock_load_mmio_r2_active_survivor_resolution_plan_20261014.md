# MemBlock Load MMIO R+2 Active Survivor Resolution Plan

状态：R+2 survivor 已实现并通过 directed；真实 1:98:1 后续问题正在迭代修复

日期：2026-10-14

适用范围：V2 `mem_ut_uvm_v2` 测试框架；不修改 RTL、接口、cfg、seed 或 UVM severity。

## 1. 问题与目标

真实 seed `37617171` 在原 R+2 deleted-owner stale-drop、R/R+1 survivor 和 ROB
admission/frontier 修复之后，仍有两处相同失败：

| 配置 | raw/redirect sample | active owner | resolver 分类 |
|---|---:|---|---|
| enqueue `1:98:1` | `1675/1673` | `uid160 ROB=1/71` | `active=1 old=0 new=0 uncovered=1 incompatible=0` |
| enqueue `1:1:1` | `2246/2244` | `uid149 ROB=1/60` | `active=1 old=0 new=0 uncovered=1 incompatible=0` |

两者都是精确 R+2 的唯一 active scalar Load。owner 在 redirect 前激活，但完整 ROB
key 不被该 redirect 覆盖，是合法 survivor。当前 resolver 只允许 R/R+1 survivor 返回
`CURRENT`，R+2 固定落入 fatal。

目标是在严格排除 stale/new/mixed producer 后，把 R..R+2 的唯一 active survivor归为
`CURRENT`；保留 covered-old 和 deleted-owner stale-drop，以及全部歧义 fatal。

## 2. 证据模型

### 2.1 可以作为 producer 证据的状态

1. `uid_by_active_rob` 中按完整 `{flag,value}` 唯一命中的 active owner。
2. 本次 redirect 的单代 `redirect_deleted_lq_owner_by_key`，且 epoch、record、dynamic
   epoch 与 inactive status 全部一致。
3. active owner 的操作类型、dispatch 和 activation epoch provenance。

主表中已经 terminal/inactive 的旧 UID 只是诊断历史，不能单独作为迟到 producer。例如
`uid0 ROB=0/71 terminal_done` 不属于 active map，也不属于 record 32 的 deleted-owner窗口。

### 2.2 R+2 survivor 的排他证明

允许 `CURRENT` 必须同时满足：

1. raw 唯一重叠一个 redirect record/anchor，sample 位于 `R..R+2`。
2. 相同 value 的两个完整 ROB key只命中一个 active owner。
3. owner 是已 dispatch scalar Load。
4. `active_instance_flush_epoch < redirect_epoch`，排除同 UID/same-key redirect 后动态
   reissue；后者继续计入 `new_candidate`。
5. `rob_need_flush(active_key, redirect)==0`，证明 owner 是 survivor。
6. `old_covered/new/incompatible` 全为零，禁止混合候选。
7. ROB admission保持 `candidate_uid - rob_commit_frontier_uid < 160`，所有 activation入口
   再次审计。相同 value、相反 flag 的两个未提交 owner相隔160，不能同时进入 active。
8. redirect scan前执行 active ROB window审计；scan后只删除 covered owner，不新建 owner。
9. deleted-owner窗口是单代的：窗口清除前禁止下一次 redirect；overlap命中多个 record/
   anchor时直接 fatal。因此更早 redirect的迟到证据不能混入当前 R+2。

若本次 redirect 确实删除了相同 value 的另一个 producer，则它在 R 时必须与 survivor同时
active。对于未提交 owner，这违反第7条；对于已提交但仍 LSQ-active 的 owner，其完整 ROB
active map仍存在，会使 `active_candidate_count>1`，不能满足严格 CURRENT 条件。故当前条件
足以排除同 value deleted producer。

## 3. 完整决策矩阵

| 时序/候选 | 结果 | 理由 |
|---|---|---|
| R/R+1/R+2，唯一 covered old active | `STALE_DROP` | 当前 redirect明确覆盖旧实例，优先级最高 |
| R/R+1/R+2，唯一 uncovered old active，其他分类为0 | `CURRENT` | 唯一合法 survivor |
| R/R+1/R+2，active为 redirect epoch内 same-key/new owner | fatal | 可能承接旧脉冲，不能写tag |
| R/R+1/R+2，Store/prefetch/未dispatch Load | fatal | producer类型不兼容 |
| 任意 overlap，多active或 covered/uncovered/new/incompatible混合 | fatal | value-only端口无法唯一归属 |
| 精确 R+2，active=0，本次deleted scalar Load唯一 | `STALE_DROP` | 原19条真实路径保持 |
| 精确 R+2，active=0，deleted为0或多个 | fatal | 无法证明旧owner |
| R+2 active survivor且主表有同value terminal历史 | `CURRENT` | terminal历史不是动态producer证据 |
| R+2 active survivor且存在本次同value deleted owner | 不可由合法admission构造；若状态损坏则 fatal | admission/active候选唯一性保护 |
| R+3及以后 | 普通current解析或无owner fatal | 超出RTL已知最大延迟窗口 |
| 多redirect anchor/record overlap | fatal | redirect provenance不唯一 |
| raw epoch未来、sample为0/未来 | fatal | provenance损坏 |

## 4. 实现方案

### 4.1 Resolver

保持分支顺序：

1. 唯一 covered-old active -> `STALE_DROP`。
2. 唯一 uncovered active survivor，sample位于唯一 overlap 的 `R..R+2` -> `CURRENT`。
3. 精确 R+2、active=0、唯一 redirect-deleted scalar Load -> `STALE_DROP`。
4. 其余 -> `MMIO_RESOLVE` fatal并打印上下文。

实现只把现有 survivor sample条件从 R/R+1 扩展到完整合法 overlap窗口，不改变候选分类、
redirect选择、deleted-owner helper、raw epoch或tag setter。

### 4.2 Directed test

把 `run_r2_active_survivor_fatal_scenario()` 改为正向 CURRENT 场景：

- redirect边界选择uid1，uid0为唯一未覆盖active scalar Load；
- 在R+2注入uid0 ROB value；
- 要求uid0写入monitor MMIO tag，uid1不得写tag；
- 继续保留R/R+1 survivor、R+2 deleted stale、new owner、无历史、非Load、R+3和
  distance=160阻塞测试。

## 5. 自审记录

### Review 1：producer集合与wrap

发现：仅凭“主表相同value记录数”拒绝CURRENT会错误地把terminal历史当作动态producer。

修正：producer集合限定为完整key active map与本次record的deleted-owner窗口。ROB admission
证明的是未提交owner容量，不负责删除长期主表历史；这不影响排他证明。

结论：通过。

### Review 2：reissue、多redirect与分支优先级

发现：若 survivor分支放在 covered-old之前，或只判断`active_count==1`，会误写covered old、
new owner或不兼容owner。

修正：保持 covered-old stale-drop优先；CURRENT同时要求`uncovered=1`且old/new/incompatible
均为0。same-key reissue由activation epoch分类为new；单代owner窗口和多overlap fatal排除
前序redirect残留。

结论：通过，可以进入coding。

## 6. 验证与迭代

1. `git diff --check`和逐分支代码review。
2. ROB admission directed。
3. pending-MMIO directed并核对决策矩阵。
4. 原始完整cfg，seed `37617171`，enqueue `1:98:1`。
5. 同一完整cfg，仅末尾覆盖enqueue为`1:1:1`。
6. 两个真实运行均要求自然结束，检查`TEST_PASS`、`UVM_ERROR`、`UVM_FATAL`、`$finish`、
   `RM_LS_COMPARE`，并确认原sample 1675/2246消失、原R+2 stale-drop保持。
7. 后续失败写入本文迭代记录，区分本修复回归、owner边界和独立RM/RTL问题；补充方案、
   自审、代码和测试后继续运行，不降低checker或删减cfg。

## 7. 迭代记录

### 7.1 R+2 survivor 首轮验证

- ROB admission directed 通过，distance=159 放行、distance=160 阻塞。
- pending-MMIO directed 通过：R+2 deleted-owner 仍为 `STALE_DROP`，R+2 唯一
  active survivor 返回 `CURRENT` 并只向正确 UID 写 MMIO tag。
- 原始 cfg、seed `37617171`、enqueue `1:98:1` 自然运行到
  `159970.900ns`。原 `sample=1675/redirect=1673` fatal 未复现，运行中仍观察到
  多条 R+2 deleted-owner stale-drop，证明原 19 条路径未被 survivor 放宽吞掉。
- 该轮最终为 `UVM_ERROR=2, UVM_FATAL=1`，均是越过原问题点后的独立后续问题。

首轮日志：

```text
/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fault2_mmio_owner_20261012/mem_ut/ver/ut/memblock/sim/fault2_r2_survivor_real_1_98_1_20261014_c/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=37617171_rtl_real_1_98_1_seed37617171_20261014_c.log
```

### 7.2 后续问题 A：commit batch 与 redirect 竞争

#### 现象和根因

`uid464 ROB=1/55` 在 `159963.400ns` 正常写回。LSQ commit sequence 已选中
该 normal batch，并在 `start_item/finish_item` 等待 driver；等待期间另一线程启动
redirect/flush。`finish_item` 返回后，旧实现逐 UID 调用 `mark_rob_commit_uid()`：

1. `mark_rob_commit_uid()` 看到全局 flush，合法返回 0；
2. `mark_rob_commit_batch()` 已进入逐项应用阶段，把返回 0 升级为 fatal；
3. 若 batch 多于一项，前项还可能已修改、后项才失败，违反 batch 原子性。

该场景的 redirect 位于更年轻 ROB，`uid464` 是 ROB-head survivor。单纯“flush 时整批
取消”也不完整：transaction 中的 store `scommit` 已经送到 DUT，无条件重试可能重复
提交 store。因此 post-drive 决策必须区分“整批 survivor”与“批内存在 covered UID”。

#### 方案

把 `mark_rob_commit_batch()` 改为返回应用结果，并在任何 status/cursor/frontier 修改前
完成一次全批 preflight：

1. 校验 batch 从 modeled ROB head 开始、UID 连续、完整 ROB key 单调且每项仍是 normal
   candidate。
2. 若当前没有 flush，按原路径应用。
3. 若当前有 flush，只有 `active_redirect.valid` 且 batch 内每个完整 ROB key 都不被
   `rob_need_flush()` 覆盖时，才证明整批是 older/equal survivor并继续应用。
4. 若任一项被覆盖，或 flush 没有可证明的 active redirect provenance，整批返回 0，
   status、watermark、commit cursor、ROB frontier 全部保持不变。
5. preflight 通过后，在同一个 SystemVerilog function 内完成所有 UID、watermark、cursor
   修改；function 内无 timing control，其他进程不能在批内插入新的 redirect。
6. `mark_rob_commit_uid()` 不再独立重查 global flush；它只允许由已通过全批 preflight
   的 batch 调用，避免把全批原子决策重新拆成逐 UID 决策。

分支矩阵：

| post-drive 状态 | batch 与 active redirect 关系 | 结果 |
|---|---|---|
| 无 flush | 全部 normal candidate | 原子提交 |
| 有 active redirect | 全部不被覆盖 | 原子提交 survivor batch |
| 有 active redirect | 至少一项被覆盖 | 整批取消、零软件副作用 |
| 有 flush但无 active redirect provenance | 无法证明 | 整批取消、零软件副作用 |
| 任一 candidate/ROB/head 校验失败 | 状态损坏 | fatal，且发生在任何修改前 |

directed 增加两个场景：更年轻 redirect 下 normal head survivor整批成功；一个 batch
同时含 survivor和covered项时整批取消，证明不会部分推进 cursor/frontier。

### 7.3 后续问题 B：UID 14 sticky L2 D-error 未进入 RM expectation

#### 现象和根因

`uid14 ROB=0/85` 的 DUT/status exception 为 `0x80000`（bit19 hardware error），RM
预测为 0。RTL `LoadUnit` 对 DCache `corrupt && !denied` 设置 bit19，对 `denied`
设置 load access fault bit5。cfg 已开启 sticky L2 D-error，测试框架也有按 physical line
和 DUT sample 查询的只读 ledger API，但 RM 未调用它；现有 `access_sample`、
`expected_dcache_denied/corrupt` 字段一直未赋值。

#### 方案

1. passive DUT writeback record 增加 `sample_seq`，在真实 LDA 采样点读取
   `peek_current_dut_global_sample()`；不以提交时刻或 `$time` 代替。
2. RM 获得已匹配 ROB 的 LDA record 后，把 `sample_seq` 写入 item 的
   `access_sample`。
3. 仅对完成正常地址翻译、具有完整 PA geometry 的 scalar Load，按访问覆盖的每个
   64-byte line调用 `read_l2_d_error_for_rm(line, access_sample)`。
4. 聚合 denied/corrupt：denied 映射 bit5；corrupt 且非 denied 映射 bit19。期待值只来自
   sticky ledger，不读取 DUT exception反推。
5. ledger 未初始化或 WB sample 为0属于 provenance 缺失，显式 RM error；sticky关闭则
   返回空错误视图，不新增异常。

### 7.4 后续问题 C：UID 98 提交时误读 live canonical TLB entry

#### 现象和根因

`uid98 ROB=1/9` 最终正常写回，DUT/status exception 为0；RM却预测 PF bit13
(`0x2000`)。每个 UID 的 completed TLB record 已保存本次动态 attempt 真正收到的
`payload` 和 request-derived PPN，但 RM 在 commit 时只读取冻结 CSR context，随后重新
查询全局 `tlb_entry_by_key`。相同 key 的后续 UID 可覆盖 live canonical entry，因此
RM把别人的 PF response用于 uid98。

#### 方案

新增一个一次性返回 `{frozen context, entry value view}` 的只读解析接口：

1. 当 `request_vpn == record.vpn` 且 UID record 为 completed、`pte_valid=1`、payload
   有效时，必须复制 `record.payload`，并使用 record 中保存的
   `request_derived_valid/request_*_resolved_ppn/request_gvpn`；禁止回读 live table。
2. 同一页的所有 byte重复使用该 UID payload。
3. 只有跨页 byte 的 VPN 与 UID record.vpn 不同、且没有 per-UID payload时，才保留
   exact/range canonical fallback；该 fallback不能覆盖首个请求页。
4. directed 构造“UID 完成正常 response后，同 key live canonical 被 PF entry覆盖”，
   要求 resolver仍返回 UID payload且 fault 位为0。

### 7.5 后续方案自审

#### Review 1：原子性与 DUT pulse

发现：仅在 sequence 端 `finish_item` 后看到 flush就无条件取消，会忽略已经送出的 store
`scommit`，重试可能产生双重副作用。

修正：batch preflight必须使用完整 ROB key判断 active redirect coverage。全部为更老
survivor时应用一次；混合/covered时整批零软件副作用。所有合法性检查都在首个 status
写之前完成。

结论：通过。

#### Review 2：RM provenance 与遗漏场景

发现一：使用 commit时当前 DUT sample 查询 sticky error会把后续激活的错误错误归给旧
load。修正为在 passive LDA采样点冻结 `sample_seq`。

发现二：无条件使用 UID payload会错误处理跨页访问的第二页。修正为仅在
`request_vpn == record.vpn` 时强制 UID snapshot；其他 VPN保留受控 canonical fallback。

发现三：直接根据实际 exception补 expected会形成自证。修正为实际值只提供 ROB匹配和
sample provenance，异常类型只读 sticky ledger。

结论：通过，可以 coding。后续每轮随机失败继续追加本节，不把首个后续失败当作完成。

### 7.6 后续问题 D：UID 0 的 L1-TLB hit 没有 per-UID completed payload

#### 现象

完整 `1:98:1` 在 `841.900ns` 报两个关联的 `UVM_ERROR`：

```text
RM_READONLY_API uid_tlb_entry query miss:
  uid=0 request VPN=0x8d34a record_valid=1 pte_valid=0 wait_state=0
RM_LS_COMPARE:
  uid 0 first VPN completed UID translation unavailable
```

`wait_state=0` 是 `MEMBLOCK_UID_TLB_WAITING`，不是 completed fault。框架当前约束为：

- `COMPLETED` 表示已冻结一个真实 response payload，正常翻译或 PF/AF/GPF/GAF 均要求
  `pte_valid=1`；这里的 `pte_valid` 是“payload 有效”，不是“页表项无 fault”。
- `WAITING + pte_valid=0` 表示 issue 已登记，但尚未由 L2TLB response multicast 回填。
- `CANCELED + pte_valid=0` 表示该动态 attempt 已失效，不能被 RM 当成翻译真源。

因此该错误不能通过允许 `COMPLETED/pte_valid=0`、根据 DUT exception 反推 expected，或降低
severity 处理。

#### 日志证据和根因

`uid=0` 的地址为 `VPN=0x8d34a`。日志中没有该 VPN 的 L2TLB request accept；此前已有：

```text
token=1 vpn=0x879a6 lookup=MISS_BUILD anchor=0x879a6 complete sample=98
token=2 vpn=0x83d43 lookup=RANGE_HIT anchor=0x879a6
token=3 vpn=0x8fa47 lookup=RANGE_HIT anchor=0x879a6 complete sample=97
```

该宽页 response 已被 DUT 实际采样，后续 `uid=0` issue 命中 L1 TLB，故不再产生新的
DTLB-to-L2TLB request。DUT 在 `833.400ns` 返回 bit5 fault，随后 fault redirect；框架却只在
`complete_waiting_uid_records_by_response()` 中给 UID 保存 payload。`uid=0` 在旧 response
完成后才 issue，错过 multicast，最终保持 `WAITING + first_request_fire_sample_seq=0`。

结论：这是合法 L1-TLB hit 的 per-UID issue 快照缺失，不是合法的“无 PTE completed fault”，
也不是 DUT response 采样丢失。

#### 最小修复

新增两层只读 provenance，不改变 WAITING/COMPLETED/CANCELED 状态机：

1. common data 保存“最近一次被 DUT 实际采样完成的 response”表。key 为 canonical anchor；
   value 是 `driving_req.entry_snapshot` 的值拷贝，并保存 completion sample、token和generation。
   只允许 `complete_driving_response()` 在 `sampled_resp_valid` 的 completion 边界写入。
2. `register_uid_tlb_record_on_issue()` 建立新 WAITING epoch 时，以该 UID 冻结的 CSR 和 VPN
   在 completed-response 表中做 exact/range lookup。若唯一命中，则把 payload和
   request-derived PPN/GVPN复制到独立的 immutable issue-hit snapshot，并记录 issue sample、
   response sample/token/generation。
3. `resolve_tlb_entry_for_rm()` 的首 VPN 优先级为：
   - `COMPLETED + pte_valid=1`：继续使用本 UID 的真实 response payload；
   - `WAITING + first_request_fire_sample_seq=0 + issue_hit_snapshot_valid=1`：使用 issue 时冻结的
     completed response快照；
   - 其余状态保持 query miss。
4. `mark_request_fire()` 一旦观察到该 UID 的真实 L2 request，立即使 issue-hit fallback不可用。
   后续只能等待对应 response 把 record变成 `COMPLETED`，禁止旧 L1 快照遮蔽新 miss。
5. runtime reset、`reset_all_tables()` 清空 completed-response 表；SFENCE/HFENCE C4 删除某个
   canonical anchor时同步删除该 anchor的 completed-response快照。这样软件表只模拟仍可能
   驻留在 DUT L1 TLB 的 payload。
6. completed-response range lookup复用有限 secondary bucket作为候选索引，但候选必须在
   completed-response表中存在，并以其冻结 payload重新执行 raw matcher；不能回读尚未完成或
   已被后续 request建立的 live canonical entry。

不能直接在 RM commit时 fallback到 `tlb_entry_by_key`：live canonical 在 request accept时就已
建立，尚不代表 DUT收到 response；它还可能被后续 key/range状态改变，且 response-time PBMT
overlay只存在于 token snapshot。使用 live表会把 pending/他人 payload错误归给当前 UID。

#### 决策矩阵

| 场景 | UID状态/证据 | RM结果 |
|---|---|---|
| 正常或fault L2 response已匹配本 UID | COMPLETED，payload有效 | 使用 UID response payload |
| L1 hit，无本 UID request fire | WAITING，issue-hit快照有效 | 使用 issue-hit payload |
| issue后真实 L2 miss仍pending | WAITING，request-fire非0 | query miss，等待 response |
| issue时没有已完成命中 | WAITING，issue-hit无效 | query miss |
| redirect取消旧attempt | CANCELED | query miss，不复活旧快照 |
| same UID reissue | 新 wait epoch | 重新冻结；旧 epoch快照不可复用 |
| same key live entry存在但未response | completed表无对应完成证据 | query miss |
| same key后续 live变化 | UID issue-hit已按值冻结 | 不受 live overwrite影响 |
| range/superpage L1 hit | completed候选唯一且raw match | 冻结所选anchor和派生PPN |
| SFENCE/HFENCE已使anchor失效 | completed快照同步删除 | 不允许 issue-hit |
| response与issue同拍，completion先可见 | issue lookup命中 | issue-hit；无request-fire才可使用 |
| response与issue同拍，issue先登记 | response multicast匹配 | 转为COMPLETED，优先使用UID payload |
| 跨页第二VPN | 无per-UID第二页快照 | 暂保留现有受控canonical路径并专项审计 |

#### 实现范围和测试

预计最小修改：

- `memblock_tlb_entry.sv`：增加 issue-hit payload和provenance字段，reset/init显式清理，
  request-fire失效fallback。
- `common_data_transaction.sv`：completed-response表、completion发布、exact/range lookup、
  issue冻结、SFENCE/reset清理。
- `memblock_l2tlb_base_sequence.sv`：response completion调用发布API。
- `memblock_rm_readonly_api.sv`：在严格条件下读取issue-hit snapshot。
- `soft_test_rm_provenance_followup_sequence.sv`：增加 no-request-fire L1 hit、request-fire禁用、
  fault payload、range、same-key live变化和SFENCE/reset场景。

旧 `fault2_followup_real_1_98_1_20261014_h` 使用已编译 binary，源码修改不会改变其行为。
2026-10-15 用户明确要求保持该进程自然运行但不再等待，D/E 已进入并行 coding、compile、
directed 和新诊断版真实 seed 阶段。

## 7.8 问题 E：UID 6889 在 memoryViolation redirect 上不收敛

### 现象和判定

当前 `1:98:1` 完整用例越过原 MMIO 和 commit-batch 失败点后，在主事务约 7000 附近停止
前进。旧 run 继续自然运行；诊断源码和新 binary 独立构建，不再以旧 run 结束作为前置条件。

已确认的关键顺序：

```text
3565225.900ns request scalar fault redirect uid=6888 rob=1/79
3565258.400ns fault retire uid=6888 terminal_done=1
随后 commit frontier 指向 uid=6889 rob=1/80
```

从约 `3.5ms` 到至少 `11.58ms`，UID 6889 持续重复以下闭环：

```text
uid=6889 以新的 LQ key 重新入队/执行
LSQ_COMMIT dut lqDeq ... uid=6889
COMMON_DATA release lq mapping uid=6889
COMMON_DATA cancel WAITING uid_tlb_record uid=6889 reason=redirect_flush
COMMON_DATA retire active uid=6889 success=0 terminal_done=0 rob=1/80
```

尾部相邻闭环间隔稳定约 `545-560ns`，LQ key 持续变化，例如 `0/24 -> 0/45 -> 0/66 ->
1/15`，说明 DUT 和框架都在重新准入、重新执行动态实例，不是同一条软件记录没有清理。
更年轻的 UID 6890 动态 epoch 已增长到上万，也说明每轮都真实发生 flush/reissue。主事务无法越过
6889，因此这是确定的 livelock，不是高 fault 压力下的有限慢收敛。

后续只读观察把该结论延伸到至少 `13.122608ms`。其中一轮的完整边界为：

```text
13122588.400ns  batch 已选择 oldest redirect，并丢弃其覆盖的 younger event
13122593.400ns  DUT lqDeq 一批 8 项，第一项 uid=6889 lq=0/54
13122593.400ns  framework release uid=6889 LQ mapping
13122598.400ns  active redirect 已建立，继续丢弃其覆盖事件
13122608.400ns  redirect flush 从 uid=6889 ROB=1/80 开始
13122608.400ns  uid=6889 retire active, success=0, terminal_done=0
```

随后相同闭环继续以约 `540-570ns` 周期出现，UID 6889 的 LQ key 依次轮转。该顺序进一步
区分了三类可能性：

1. 不是合法高 fault 慢收敛：UID 6889 本身没有 fault completion，且数毫秒内没有一次
   terminal progress；
2. 是同一静态 UID 的无限动态 replay：每轮都有新 LQ owner、真实 dequeue、释放、flush 和重新
   admission，而不是旧软件 owner 卡死；
3. frontier/commit 阻塞是结果而非首因：UID 6889 在 dequeue 后仍被 redirect 自身覆盖并保持
   `terminal_done=0`，所以 commit frontier 合法地不能前进。

同轮中 younger UID 的 fault feedback、dequeue 和 redirect-covered drop 很多，但 flush 覆盖范围的
第一项始终是 UID 6889。它们会放大重建压力，却不能替代 UID 6889 的 redirect producer 证据。
在取得六路 rollback subtype 之前，不能把这些 younger fault 当作根因。

### 已排除的错误假设

曾考虑“UID 6889 自身 Load fault 与同 batch memoryViolation 同时到达，redirect 抢在 fault commit
前反复冲刷”的解释。完整日志不支持该解释：

- 没有 UID 6889 的 `fault feedback`；
- 没有 UID 6889 的 normal pass；
- UID 6889 每轮先被 LQ dequeue/release，随后又被 redirect flush；
- 日志中的 UID 6890 fault 是更年轻事件，每轮被 UID 6889 的 redirect 清除，不能作为 6889 的
  fault provenance。

因此不能把 `store_redirect_waits_fault_commit()` 简单扩展到所有 fault；该修改既没有命中已观察
状态，也可能错误延迟普通 load ordering redirect。

### 统一 memoryViolation 端口和未决证据

`dispatch_monitor_event_adapter::convert_raw_memory_violation()` 把 `mem_to_ooo.memoryViolation` 统一
转为框架 source 8，并直接使用 `memory_violation_level` 同时设置 `redirect.level` 和
`flush_itself`。但 RTL `MemBlock.scala` 的该输出是以下多类 rollback 的 oldest 仲裁结果：

1. 各 LoadUnit 的 `s3_vp_match_fail`、
   `io_lsq_ldld_nuke_query_resp_valid && rep_frm_fetch`、`s3_frm_mis_flush`；
2. LoadQueueUncache enqueue nack rollback；
3. LoadQueueRAW 的 store-load ordering nuke rollback。

因此 source 8 只表示“统一 memory replay redirect”，不能单凭它断言是 RAW 或存在某个冲突
store。当前能严格确认的只有：UID 6889 每个动态实例都收到 `flush_itself=1` 的统一 rollback，
所以自身被取消；重新准入后同类条件再次出现。

当前日志没有打印 DUT oldestRedirect 的输入子源，也没有打印 batch arbiter 最终选择 event，无法
严格区分：

1. 旧 memoryViolation pulse/event 被错误跨动态实例重复消费；
2. 每个动态实例都真实产生 LoadUnit 的vp/misalign/fetch rollback；
3. uncache buffer full/nack 条件持续不消失；
4. 每个动态实例都真实产生 RAW violation；
5. 框架 reissue没有携带对应子类所需的等待/ordering状态，固定重建相同失败条件。

生成 RTL `build/rtl/MemBlock.sv` 已确认该配置没有HybridUnit rollback输入，实际仲裁六路为：

```text
_inner_LoadUnit_[0..2]_io_rollback_*
_inner_lsq_io_nack_rollback_0_*
_inner_lsq_io_nuke_rollback_[0..1]_*
```

对应 output mux one-hot 是 `inner__oldestOneHot_resultOnehot_T_{15,31,46,60,73,85}`。这些信号可在
后续重编译时通过现有 `uvm_hdl_read` 模式加入partcomp XMR referee，不需要增加RTL功能接口。

另一个已排除候选是紧邻的 UID 6888：它是 ROB 1/79 scalar store fault，最终在
`3565258.400ns` 收到真实 `sqDeq`，释放 SQ 1/9并以 `terminal_done=1` 退休。近期日志中的SQ释放
均为UID 6890及以后，没有 UID 6888残留。因此不能把6889长期rollback归因于6888 fault SQ没有
释放；若后续确认是RAW，必须找到另一笔更老store或证明RTL RAW entry/provenance残留。

原始 cfg 的地址复用开启权重为 `1:1`，四类 load/store 依赖关系也是等权，确实会高概率制造
RAW 冲突；但 cfg 压力不能解释同一请求无限不收敛，也不能通过降低权重作为完整用例修复。

### 诊断增强和定向修复入口

当前 run 自然结束后，先加入低 verbosity、仅在选择 redirect 和启动 flush 时打印的 provenance：

```text
selected redirect: uid/source/ROB/level/flush_itself/cycle
owner status: dynamic_epoch/active/pass/fault/replay/rob_commit/terminal_done
flush start: first covered active UID/ROB、commit cursor/frontier、cancel record/redirect epoch
```

memoryViolation raw 还需要带上本次 sample/service cycle，并核对 normalizer 解析出的 UID 与该 UID
当前 dynamic epoch。更重要的是，在 `MemBlock.scala` oldestRedirect 仲裁输入侧采样以下 subtype
provenance：LoadUnit各端口rollback、uncache nack、RAW nuke。输出端
Redirect bundle没有 subtype，现有agent interface也没有 `stFtqIdx/stFtqOffset`，所以只增加框架
source日志仍不够。优先通过testbench bind/XMR只读内部valid和ROB key，避免修改功能RTL接口。

生成 RTL 进一步给出 `level` 的排他信息：LoadUnit 的
`ldld rep_frm_fetch` 路径产生 `rollback_valid=1, level=0`；`s3_vp_match_fail` 和
`s3_frm_mis_flush` 产生 `level=1`。RAW nuke在顶层被固定编码为`level=1`，uncache使用自己的
rollback level。本案 UID 6889 每轮都被 `flush_itself=1` 覆盖，因此可以排除 LoadUnit
`rep_frm_fetch` 作为当前循环的直接获胜子因，但仍不能在vp/misalign、uncache和RAW之间选择。

若确认为RAW且RTL/内部信号可提供违反对端 SQ/ROB key，应一并记录；若接口没有该字段，必须通过
同周期 storeIn 地址和active ROB集合做唯一候选证明，不能按猜测直接改变行为。

诊断打印只在output `memoryViolation_valid` 时执行，并要求：

- 六路输入中至少一路valid，且output ROB必须等于one-hot获胜输入ROB；
- 打印全部valid输入的kind/port/ROB/level以及获胜kind，避免只看oldest output；
- 若获胜为LoadUnit，继续采样对应内部 `s3_vp_match_fail`、
  `io_lsq_ldld_nuke_query_resp_valid && rep_frm_fetch && s3_ldld_rep_inst_last_REG`、
  `s3_frm_mis_flush`，并校验它们与output level一致；
- 若获胜为nack，采样uncache buffer valid/free count和被拒绝load identity；
- 若获胜为nuke，采样对应storeIn valid/ROB/paddr/mask和目标load ROB；
- 任一XMR路径不可读时直接报告诊断不完整，不得把unknown当0后继续给出根因结论。

后续定向决策矩阵：

| 观察结果 | 根因分类 | 允许的修复方向 |
|---|---|---|
| 相同 raw cycle/event 被跨多个 dynamic epoch重复消费 | stale event | event绑定sample+dynamic epoch；旧epoch严格drop并诊断 |
| 每个新epoch都是LoadUnit vp/misalign/fetch rollback | load pipeline持续条件 | 检查对应reason；补足等待/response状态，禁止无条件drop |
| 每个新epoch都是uncache nack/full | uncache资源不收敛 | 检查buffer owner/response；修复资源释放或refetch节流 |
| 每个新epoch都是RAW且更老store仍active | 真实RAW冲突 | 保留redirect；reissue必须建立ordering约束或等待冲突store drain |
| RAW pulse出现但对端store已terminal/不存在 | stale DUT/adapter owner | 以完整ROB/动态epoch证明后丢弃失效pulse，不得value-only猜测 |
| UID 6889 redirect实际来自fault/replay非source-8 | 仲裁/provenance错误 | 按真实source修正对应状态机，禁止泛化memoryViolation策略 |
| redirect合法但framework固定重建相同发射顺序 | framework livelock | 对该UID维护一次性violation replay barrier，直到older frontier推进 |

诊断后的最小行为修复优先级为：先验证 event 是否属于 UID 6889 当前 dynamic epoch；再验证获胜
subtype及其释放条件；只有二者都成立且重新 admission 会立即重建同一未解除条件时，才允许建立
`{uid, dynamic_epoch, subtype, blocking-owner/resource}` 绑定的 recovery barrier。barrier 不能仅以
ROB value、source 8 或固定拍数解除，必须由更老 owner terminal、uncache resource progress、或
对应 replay reason 的完成事件解除。这样既防止无限重建，也不会吞掉新的真实 violation。

任何修复都不得无条件丢弃 memoryViolation、改低 severity、关闭 checker 或调低地址复用压力。
先用诊断闭合 producer/conflicting-owner 证据，再增加最小定向 directed，最后才修改收敛行为。

### 测试要求

- directed 分别覆盖 LoadUnit replay、uncache nack和RAW nuke的 `flush_itself=1` 自重放，并证明
  对应资源/依赖解除后frontier前进；
- stale source-8 旧 dynamic epoch 必须 drop，当前 epoch 的真实事件必须保留；
- 普通非 fault redirect 仍立即生效，不能被 fault commit helper延迟；
- ROB admission、pending-MMIO、RM provenance directed 全量回归；
- 两个完整随机用例使用原 cfg/seed/checker/`MAIN_TRANS_NUM=10000`；后续可用
  `pl=UVM_LOW` 抑制 UVM_HIGH 海量日志，但不改变测试内容和检查强度；
- 必须自然结束，并检查 `TEST_PASS`、`$finish`、`UVM_ERROR/FATAL` 和 `RM_LS_COMPARE`。

### 问题 E 自审

#### Review 1：收敛分类

发现：只看“UID停在6889”可能把合法慢收敛误判为死循环；只看ROB不变也可能是单一记录未清理。

修正：以数千次 LQ key轮转、dynamic epoch持续增加、固定闭环周期、frontier长期不变共同证明真实
re-admission livelock。明确排除“单记录卡住”和“有限高fault慢收敛”。

结论：通过。

#### Review 2：修改边界

发现：当前没有 selected redirect source和冲突owner证据，直接增加 redirect suppression 或把所有
load redirect延迟到commit，都会掩盖真实DUT ordering事件并可能破坏合法rollback。

修正：第一步仅增加事件选择/owner/frontier provenance；行为修改必须由 source-8 当前动态实例和
冲突owner的排他证明驱动。保留所有负向fatal和checker，不接受cfg降压作为修复。

结论：通过，等待当前完整run自然结束后应用诊断和问题D修复。

#### Review 3：dequeue、frontier和自 flush 的因果关系

发现：仅看到 UID 6889 多次 `lqDeq`，可能误认为请求已经功能完成而只是 commit cursor 没推进；
也可能把同批大量 younger fault 当作真正 redirect producer。

修正：逐拍核对显示 `lqDeq/release` 后 active redirect 已存在，最终 flush 从 UID 6889/ROB 1/80
开始，并把它以 `terminal_done=0` 退休。故 dequeue 只表示 LSQ entry 被 DUT 回收，不证明该动态
实例可提交；frontier停住符合状态机约束。younger fault 只能算同轮压力，不能证明 subtype。

结论：通过。收敛分类已经闭合，行为修复仍必须等待 rollback subtype/provenance 诊断。

### 7.7 问题 D 自审

#### Review 1：状态语义与排他条件

发现：若把 `WAITING/CANCELED` 都视作可fallback，会让 redirect取消的旧动态attempt重新参与
RM；若只检查 `first_request_fire==0` 而不要求 completed-response provenance，也会把尚未
response的 live canonical误当L1命中。

修正：仅允许当前 `WAITING` epoch，且同时满足 request-fire为0、issue-hit快照有效、快照来自
真实completion。`CANCELED`、request-fire非0、无完成快照全部query miss。

结论：通过。

#### Review 2：range、覆盖与失效生命周期

发现：直接复用 live canonical payload不能证明 response完成；只按 exact key又会漏掉本次
`uid=0` 的宽页range hit。completed快照若不跟随SFENCE删除，还会在硬件已失效后制造伪L1 hit。

修正：secondary bucket只用于枚举anchor；最终候选必须来自completed-response表并重新raw
match。response completion按值发布，issue按值冻结；SFENCE/HFENCE、runtime reset和table
reset同步失效。same-key pending/live对象不能覆盖已冻结UID快照，也不能在未完成时成为候选。

结论：通过。2026-10-15 已按用户指示在旧 binary 自然运行期间进入独立 coding/compile。

### 7.9 问题 D/E 第一轮实现（2026-10-15）

问题 D 已按 Review 3/4 实现：

1. `memblock_uid_tlb_record` 增加独立 issue-hit payload、response token/sample/generation。
2. `common_data_transaction` 增加只由 sampled response completion 发布的 completed-response
   exact/range cache；live pending entry 不进入该 cache。
3. UID issue以冻结CSR对completed cache做exact/range raw match并按值复制payload，同时派生
   request-specific PPN。
4. request fire、cancel/reissue、reset和SFENCE/HFENCE C4清除对应fallback。
5. RM首VPN优先使用COMPLETED UID payload，其次只接受
   `WAITING + request_fire=0 + valid issue-hit provenance`。
6. directed覆盖completed cache捕获、live overwrite隔离、request-fire失效、cancel/reissue失效和
   cache删除。

问题 E 已加入不改变行为的最小诊断：

1. ctrl monitor在`memoryViolation_valid`同拍冻结3路LoadUnit、uncache nack、2路RAW nuke的
   valid/完整ROB/level以及oldest winner。
2. LoadUnit冻结vp-match、ldld fetch三条件和frm-misalign；uncache冻结free-list
   `canAllocate[0:2]`；RAW冻结两路store input的ROB/paddr/mask。
3. adapter要求winner唯一并校验选中输入的ROB/level等于顶层输出，编码明确subtype。
4. batch在解析到UID后打印UID、dynamic epoch、subtype、ROB、frontier和admission水位。
5. software directed覆盖六种输入/subtype decode。任何XMR不可读或X/Z均显式报错，不按0处理。

第二轮自审：completed cache不跟踪DUT L1容量淘汰，但真实miss随后出现request-fire并在RM使用前
清除issue-hit fallback；只有真实hit保持no-request-fire。completion/issue同拍两种调度顺序均
闭合。rollback诊断只读内部状态，未增加drop、barrier或redirect抑制。`uvm_hdl_data_t`先清零
再读取，避免未使用高位被误判为X；实际信号X/Z仍被检测。

### 7.10 问题 D/E 编译与专项验证（2026-10-15）

包含最终XMR smoke的完整编译通过：

```text
mode=fault2_d_e_diag_compile_20261015_d
note=rm_l1hit_rollback_diag_20261015_d
log=/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fault2_mmio_owner_20261012/mem_ut/ver/ut/memblock/sim/fault2_d_e_diag_compile_20261015_d/log/vcs_compile_rtl.log
```

问题 D RM provenance directed：

```text
mode=fault2_rm_provenance_directed_20261015_a
note=completed_cache_issue_hit_20261015_a
log=/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fault2_mmio_owner_20261012/mem_ut/ver/ut/memblock/sim/fault2_rm_provenance_directed_20261015_a/log/tc=basicTest_ts=memblock_rm_provenance_followup_directed_vseq_cfg=default_seed=37617171_rtl_completed_cache_issue_hit_20261015_a.log
result=TEST_PASS, UVM_ERROR=0, UVM_FATAL=0, natural $finish at 290.900ns
```

该用例覆盖真实completion cache捕获、issue-time不可变快照、live overwrite隔离、request-fire
失效、cancel/reissue失效和cache删除。

pending-MMIO和rollback provenance directed：

```text
mode=fault2_pending_mmio_diag_directed_20261015_a
note=rollback_xmr_subtype_20261015_a
log=/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fault2_mmio_owner_20261012/mem_ut/ver/ut/memblock/sim/fault2_pending_mmio_diag_directed_20261015_a/log/tc=basicTest_ts=memblock_pending_mmio_directed_vseq_cfg=default_seed=37617171_rtl_rollback_xmr_subtype_20261015_a.log
result=TEST_PASS, UVM_ERROR=0, UVM_FATAL=0, natural $finish at 365.900ns
```

XMR smoke和六路winner/subtype校验通过，共观察6条`ROLLBACK_PROVENANCE`：Load VP match
subtype 1、Load FRM misalign subtype 2、Load LDLD fetch level 0 subtype 3、uncache nack
subtype 5、RAW nuke lane 0 subtype 6、RAW nuke lane 1 subtype 7。directed预期捕获的10个负向
fatal均由catcher消费，最终fatal计数为0；所有负向分支均未写入错误tag或改变redirect行为。

ROB admission directed：

```text
mode=fault2_rob_admission_directed_20261015_a
note=distance159_160_frontier_20261015_a
log=/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fault2_mmio_owner_20261012/mem_ut/ver/ut/memblock/sim/fault2_rob_admission_directed_20261015_a/log/tc=basicTest_ts=memblock_rob_admission_window_directed_vseq_cfg=default_seed=37617171_rtl_distance159_160_frontier_20261015_a.log
result=TEST_PASS, UVM_ERROR=0, UVM_FATAL=0, natural $finish at 290.900ns
```

distance 159合法准入，distance 160继续触发预期且被catcher捕获的fatal。以上诊断全部保持
behavior-neutral；问题 E 的producer-specific收敛修改仍必须等待真实seed给出UID 6889同拍获胜
subtype和资源/冲突owner证据，不能由directed decode结果提前推断。

### 7.11 问题 D 第一次真实seed迭代：zero-based token（2026-10-15）

诊断版真实`1:98:1`使用原始cfg、seed 37617171和`UVM_LOW`启动，运行时确认
`MAIN_TRANS_NUM=10000`、dispatch/lsqenq/lsqcommit均为1、enqueue权重为`1/98/1`。该次运行在
735.900ns自然结束：

```text
mode=fault2_d_e_real_1_98_1_20261015_a
note=completed_cache_rollback_diag_seed37617171_20261015_a
log=/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fault2_mmio_owner_20261012/mem_ut/ver/ut/memblock/sim/fault2_d_e_real_1_98_1_20261015_a/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=37617171_rtl_completed_cache_rollback_diag_seed37617171_20261015_a.log
failure=UVM_FATAL completed TLB response cache requires valid provenance
```

精确根因是`next_request_token`从0开始，首个合法L2TLB request token 0在sample 108完成；新增
completed-response代码错误地把`response_token == 0`当作无效。相同错误还存在于UID issue-hit
setter和RM fallback判定。token是zero-based identity，不是valid bit；真正provenance由以下排他
条件建立：completed cache三张表同key存在、response sample非0、entry generation非0；冻结后再由
`issue_hit_snapshot_valid`、sample和generation一致性证明。清除状态即使把token字段重置为0，也不会
与合法token 0混淆，因为valid bit同时清零。

最小修复：删除三处token非0要求，保留sample/generation/cache存在和WAITING/no-request-fire约束；
directed将completed response token改为0，完整覆盖cache发布、issue冻结、RM读取、request-fire失效、
reissue重取、cancel和cache删除。该修复不放宽pending/live entry、CANCELED或已request-fire UID。

zero-token专项重编译和运行已通过：

```text
mode=fault2_rm_token0_directed_20261015_b
note=completed_cache_zero_token_20261015_b
log=/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fault2_mmio_owner_20261012/mem_ut/ver/ut/memblock/sim/fault2_rm_token0_directed_20261015_b/log/tc=basicTest_ts=memblock_rm_provenance_followup_directed_vseq_cfg=default_seed=37617171_rtl_completed_cache_zero_token_20261015_b.log
result=TEST_PASS, UVM_ERROR=0, UVM_FATAL=0, natural $finish at 290.900ns
```

日志明确显示token 0以`response_sample=41, generation=1`完成issue-hit冻结，并在request-fire清除后
由reissue再次合法冻结。pending-MMIO/XMR和ROB admission在同一D/E实现版本的既有专项均通过；本轮
只删除错误token sentinel并增强RM directed，未触及两者逻辑。

### 7.12 问题 D 第二次真实seed迭代：D-error采样时间域（2026-10-15）

`fault2_d_e_real_1_98_1_20261015_b`越过原735.900ns token-0失败点后，在3911.900ns观察到：

```text
uid=14 VA=0x8b93a1c6 PA=0xb93a1c6
DUT load writeback sample_seq=740 exception=0x80000
RM sticky query line=0xb93a1c0 sample=740 corrupt=0 denied=0 generation=0
```

根因不是completed TLB payload，也不是DUT出现了未建模的新错误源。sticky ledger在GrantData最后一拍
`D.fire`时把`$time`写入`corrupt_activation_sample/denied_activation_sample`，而RM以CSR monitor维护的
单调`dut_sample_seq`查询。真实时间约为数千ns时，activation数值必然大于数百量级的sample序号，
因此已经送入DUT的corrupt被账本误判为“尚未生效”。同一字段还在AcquireBlock reservation时混入
`$time`，虽当前只用于诊断，也必须统一，避免后续比较再次跨时间域。

最小修复是增加D-error专用的current-DUT-sample helper：AcquireBlock建reservation和GrantData
最后一个`D.fire`均冻结`memblock_sync_pkg::peek_current_dut_global_sample()`，sample为0继续fatal；
RM仍使用DUT load writeback observer冻结的sample。不得读取DUT actual exception反推expected，也不得
把查询边界放宽为无条件sticky。directed除保留10/20显式边界外，还必须通过production wrapper提交
一次current-sample corrupt，并证明`sample-1`不可见、`sample`可见。

### 7.13 问题 E 第一次真实诊断迭代：非获胜rollback bundle的X/Z（2026-10-15）

同一真实run在8545.900ns首次出现`memoryViolation`时，monitor连续报告9个`ROLLBACK_XMR` error并达到
quit-count。X/Z集中在LoadUnit 0/1的valid与payload和LoadUnit 2 valid。生成RTL确认oldest mux允许
非获胜输入bundle处于don't-care/X；顶层output仍可由某个确定winner产生。原monitor无条件读取六路
valid、ROB、level和全部辅助payload，错误地把非获胜don't-care当成selected provenance缺失，并在
adapter有机会打印winner前耗尽quit-count。

修复采用winner-first分层采样：

1. 先读取六个`inner__oldestOneHot_resultOnehot_T_*`向量，分别记录known-mask和归约AND winner。
2. 只要存在唯一known winner，就以生成RTL的oldest-onehot结构证明该输入获胜；其他winner/candidate
   的unknown必须进入known-mask日志，不能静默编码为0。
3. 获胜输入的valid、完整ROB、level和对应subtype reason是强制provenance，任一不可读/X/Z仍保持
   error/incomplete，adapter不得猜测subtype。
4. 非获胜candidate仅在valid已知为1时读取其ROB/level；valid未知时不读取其don't-care payload。
5. LoadUnit只严格读取获胜lane的vp/ldld/frm reason；uncache读取资源状态；RAW storeIn payload只在
   对应storeIn valid已知为1时读取。所有optional未知均通过known-mask呈现。

这不是降低fatal或忽略X：selected provenance仍严格；变化仅是把“所有物理输入都必须完整”收窄为
“决定本次顶层output的winner必须完整”。directed必须构造唯一selected输入known、其余candidate和
winner标记unknown的场景，仍正确解码subtype并匹配顶层完整ROB；另保留六路全known decode。

### 7.14 迭代状态

`fault2_d_e_real_1_98_1_20261015_b`自然在8545.900ns结束，`UVM_ERROR=10`：1条UID14 RM compare和
9条rollback非获胜XMR；`UVM_FATAL=0`。修复后先重跑RM provenance、pending-MMIO/XMR和ROB admission，
再以新mode、原cfg、seed 37617171、`MAIN_TRANS_NUM=10000`和`UVM_LOW`启动真实run。真实run必须确认：

- 735.900ns token-0 completion继续合法；
- UID14 expected hardware error由sample-740 ledger正确得到；
- 首次memoryViolation不会因非获胜bundle X/Z耗尽quit-count，并输出selected subtype；
- 后续运行继续闭合UID6889的真实producer，不能在获得selected资源/owner证据前加入generic drop。

### 7.15 问题 D provenance 修复后的专项复核（2026-10-15）

735.900ns 的 completion 调用本身合法且 provenance 完整：它来自第一个真实 L2TLB request，
`response_token=0`、`response_sample_seq=108`、`entry_generation!=0`。失败原因是 completed-response
cache、UID issue-hit snapshot 和 RM fallback 三处错误地把数值 0 当成“未分配 token”。当前 token
生成器从 0 开始，故 token 0 是合法 identity，不能承担 valid-bit 语义。

修复删除 token 非零限制，但不降低 fatal：completed cache 仍要求 entry/token/sample 三表同 key
存在、sample 非零且 generation 非零；UID 冻结仍要求 WAITING、尚未出现该 UID 的 request fire、
`issue_hit_snapshot_valid` 和冻结 sample/generation 一致。真正缺 entry、sample、generation 或 UID
snapshot valid 的 completion/fallback 继续 fatal。directed 直接使用 token 0，并覆盖 cache 发布、
UID 冻结、RM 读取、request-fire 失效、reissue、cancel 和 delete；因此不是通过降低 severity 绕过。

D-error sample-domain 修复和 rollback winner-first 诊断一起重编译后，三项专项均自然结束：

```text
mode=fault2_rm_sample_domain_directed_20261015_c
note=d_error_current_sample_boundary_20261015_c
log=/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fault2_mmio_owner_20261012/mem_ut/ver/ut/memblock/sim/fault2_rm_sample_domain_directed_20261015_c/log/tc=basicTest_ts=memblock_rm_provenance_followup_directed_vseq_cfg=default_seed=37617171_rtl_d_error_current_sample_boundary_20261015_c.log
result=TEST_PASS, UVM_ERROR=0, UVM_FATAL=0, natural $finish at 290.900ns

mode=fault2_pending_mmio_winner_first_directed_20261015_d
note=rollback_winner_first_inactive_x_20261015_d
log=/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fault2_mmio_owner_20261012/mem_ut/ver/ut/memblock/sim/fault2_pending_mmio_winner_first_directed_20261015_d/log/tc=basicTest_ts=memblock_pending_mmio_directed_vseq_cfg=default_seed=37617171_rtl_rollback_winner_first_inactive_x_20261015_d.log
result=TEST_PASS, UVM_ERROR=0, UVM_FATAL=0, natural $finish at 365.900ns

mode=fault2_rob_admission_post_provenance_20261015_d
note=distance159_160_post_provenance_20261015_d
log=/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fault2_mmio_owner_20261012/mem_ut/ver/ut/memblock/sim/fault2_rob_admission_post_provenance_20261015_d/log/tc=basicTest_ts=memblock_rob_admission_window_directed_vseq_cfg=default_seed=37617171_rtl_distance159_160_post_provenance_20261015_d.log
result=TEST_PASS, UVM_ERROR=0, UVM_FATAL=0, natural $finish at 290.900ns
```

pending-MMIO directed 中 10 个预期负向 fatal 均由 catcher 定向捕获，最终 severity 计数为 0；
ROB directed 中 distance 160 的预期拒绝同样由 catcher 捕获。两者不是被静默忽略。

### 7.16 第三次真实seed迭代：rollback采样域与D-error line provenance（2026-10-15）

真实run `fault2_d_e_real_1_98_1_20261015_e`自然在11440.900ns结束，最终
`UVM_ERROR=10/UVM_FATAL=0`：UID14 D-error compare 1条，`ROLLBACK_XMR` 9条。原735.900ns
token-0 fatal未再出现。9条rollback错误全部是`known=0x0 winner=0x0`，而顶层output ROB/level有效，
adapter只能记录`diag=1/0 subtype=8`。

生成RTL证明`io_mem_to_ooo_memoryViolation_*`直接组合自六路candidate和oldest-onehot，没有RTL寄存级。
错位来自monitor采样域：顶层output经clocking block input skew在边沿前冻结，旧实现却在边沿后的
procedural region调用`uvm_hdl_read()`读取内部瞬时net，candidate已撤销。因此“output同拍live XMR”
不是同一个物理sample，winner全0不是inactive payload X。

修复分两层：

1. 六路winner、candidate ROB/level和subtype reason由TB静态force到ctrl interface，并进入同一`mon_cb`。
2. monitor每个sample先把唯一winner冻结为短窗口provenance；output出现时按完整
   `ROB flag/value + level`查找最近记录，允许0到4个sample的传输偏移。同key窗口内若出现不同winner/
   subtype证据则报告冲突，窗口外、零匹配或selected X/Z继续`ROLLBACK_XMR` error，不按当前epoch猜测。

directed增加sample 100输入、sample 102输出的跨拍命中，并检查level不匹配和age=5过期均不能命中。
该cache只关联诊断，不改变DUT redirect、event优先级或flush行为。

实现自审补充：cache的“相同provenance”必须包含candidate known/valid集合与winner known集合，不能只
比较ROB payload和reason。否则同一ROB/level在窗口内先后出现不同active candidate集合时，默认零值
可能掩盖冲突。directed因此同时覆盖：完全相同记录重复时选择最新sample；同key但candidate集合变化时
必须返回conflict；清空后age=5仍过期。

UID14方面，sample-domain统一后账本仍对`0xb93a1c0`返回generation 0，说明此前“仅时间域错误”的根因
不完整。下一轮不复制DUT actual到expected，也不放宽bit19；只在corrupt/denied reservation和最后
GrantData D.fire记录`line/source/sample/state/generation`，directed覆盖同line reservation到live ledger
的generation/source保持。真实seed将用该诊断区分DUT response line与RM PA line不一致、commit路径漏走，
或bit19来自其他DCache错误源。

### 7.17 问题 E producer闭合后的Uncache资源不收敛迭代（2026-10-15）

真实run i已把重复redirect稳定闭合到input 3 / subtype 5：UID862、ROB 1/133从dynamic epoch 775
以后反复reissue，`candidate=winner=0x8`、`uncache_known=0x7`、`uncache_free=0`，frontier始终862。
因此后续分析不再怀疑rollback仲裁来源；需要解释的是LoadQueueUncache 16个slot为何没有真实释放。

生成RTL给出的完整资源链为：

1. LoadQueueUncache entry从freeList分配，state 1发`io_uncache_req`；
2. 顶层Uncache接受请求并返回`idResp(mid,sid)`，LQ entry冻结slaveId后进入state 2；
3. 顶层Uncache以sid作为TL A source，框架在A.fire后建立同source response owner；
4. 框架保持D payload直到D.fire，顶层Uncache把对应source转成`io_lsq_resp(id=sid)`；
5. LQ entry按slaveId接收response。未被redirect的entry进入state 3，等待`ncOut/mmioOut`写回；
   已被redirect的state-2 entry只在真实response到达后flush；
6. entry的writeback或flush才进入FreeList_6.io_free，slot才可重新分配。

`uncache_free=0`只观察freeList的三路canAllocate，不能区分state 2等待response、state 3等待写回，
也不能证明顶层Uncache或框架response queue已满。先加入只读、同sample诊断：16个LQ slot的
state/ROB/slaveId/needFlush/输出valid/free事件，顶层16个source的valid/inflight/waitSame/waitReturn，
idResp、LSQ response和A/D source握手。框架同时建立每source严格A.fire->D.fire账本，禁止source在
D完成前复用、禁止无owner D，并支持乱序D完成。诊断只在资源快照变化或每256次NACK输出一次。

第一阶段不改DUT释放条件、不drop NACK、不增加固定重试延迟。诊断真实run必须先把停滞边归为以下之一：

- 框架A未接收或response容量账本卡住；
- A已接收但D未完成；
- D完成但顶层source/LSQ id未闭合；
- 顶层LSQ response完成但LQ slave owner未匹配；
- LQ已到state 3但writeback持续无ready；
- redirect后的state 2 owner等待其真实terminal response。

最终行为修复必须绑定已证明的owner terminal事件，例如修正source/id匹配、保留pending D到真实消费、
或修正写回仲裁/flush释放。禁止无条件忽略Uncache NACK、按当前epoch猜测owner、固定延迟后强制重试，
也禁止直接篡改freeList。

### 7.18 问题 E Uncache lifecycle诊断实现与公平性遗漏（2026-10-15）

run i后续日志已经从UID862推进到更高UID，因此“永久full”必须收窄为“在该时间窗内无可见释放且
frontier极慢”；不能在缺少逐source terminal证据时宣称DUT资源永不释放。新增诊断同时覆盖以下链路：

1. 16个LoadQueueUncache slot的valid/state/ROB/slaveId/redirect flush/writeback valid/freeMask；
2. 顶层Uncache 16个source的valid/inflight/waitSame/waitReturn；
3. `idResp(mid,sid)`、TL A/D source、`io_lsq_resp(id=sid)`及ready；
4. framework以A.fire建立、D.fire释放的严格source ledger，记录地址、accept cycle、完成cycle和age。

这样可以逐项列出outstanding owner，并把停滞边严格分类为：尚未A.fire、framework pending D、D已到但
top source未转LSQ response、LSQ response未命中LQ slave owner、或LQ state 3写回未retire。source提前
复用和无owner D仍fatal；乱序source完成合法。

代码审查还发现一个此前遗漏的有效场景：framework response scheduler每次timer到期后重新随机选择
eligible response，队列容量虽为16，但任一特定source没有确定的服务上界。这个性质可能造成老owner
极慢完成，却不能仅凭代码静态推断为UID862的真实根因，因为任意D完成都可能先释放别的LQ slot。
诊断run必须先观察A/D age、队列full后的release和LQ state变化。若证明确为scheduler starvation，
最小修复应把选择策略改成有界公平（例如oldest eligible/显式round-robin），而不是固定延迟、直接drop
NACK或绕过D/LSQ terminal。

诊断版完整编译通过：

```text
mode=fault2_uncache_lifecycle_compile_20261015_j
note=uncache_lifecycle_diag_ledger_20261015_j
log=/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fault2_mmio_owner_20261012/mem_ut/ver/ut/memblock/sim/fault2_uncache_lifecycle_compile_20261015_j/log/vcs_compile_rtl.log
result=PASS, VCS exit 0
```

### 7.19 问题 E 资源进展诊断闭环准备（2026-10-15）

旧真实run i没有包含7.18新增的Uncache lifecycle镜像和framework source ledger。该run最终自行在
1693406.900ns结束，原因是累计10条独立RM compare error达到quit-count；未执行kill。它只能证明
UID862附近曾长时间由Uncache NACK反复redirect，不能回答16个slot停在response前还是writeback后。

诊断版RM directed首次运行自然结束并通过：

```text
mode=fault2_uncache_lifecycle_rm_directed_20261015_k
note=uncache_source_ledger_directed_20261015_k
log=/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fault2_mmio_owner_20261012/mem_ut/ver/ut/memblock/sim/fault2_uncache_lifecycle_rm_directed_20261015_k/log/tc=basicTest_ts=memblock_rm_provenance_followup_directed_vseq_cfg=default_seed=37617171_rtl_uncache_source_ledger_directed_20261015_k.log
result=TEST_PASS, UVM_ERROR=0, UVM_FATAL=0, natural $finish at 290.900ns
```

自审发现该正向测试只证明双source逆序完成和reset清理，尚未证明ledger会拒绝同source提前复用及
ownerless D。directed现补两条精确message catcher负向场景，生产fatal保持不变。

为使下一次`UVM_LOW`真实run能够在资源持续full时直接列出framework owner，不依赖海量`UVM_HIGH`
A/D逐笔日志，responder新增只读full watchdog：16个source全占用后首拍及每256拍打印每个source的
地址、A.fire cycle和age，同时打印response queue、current D hold、timer及due cycle。任何D.fire会
通过原ledger打印full->非full释放事件；watchdog不选择response、不修改delay、不释放source。

下一真实run按以下证据判定停滞边：

- framework owner age增长且queue/timer无D完成：responder scheduler或D ready停滞；
- framework owner持续释放，但top source `waitReturn`不退：TL D到top source关联问题；
- top `io_lsq_resp`握手而LQ相同slaveId仍state 2：LQ response owner匹配遗漏；
- LQ大量state 3且`ncOut/mmioOut`无retire：写回仲裁/ready阻塞；
- LQ state 2均`needFlushReg=1`且framework owner正常完成：redirect-canceled owner的terminal drain吞吐不足。

只有新run证明具体停滞边后才批准行为修复。若证实reorder scheduler使某source长期无服务，最小修复
必须把delay选择绑定到确定source并提供有界公平仲裁；不得在NACK处drop、按固定延迟直接free或绕过
真实D/LSQ terminal。

### 7.20 问题 E 新真实run的诊断失败与采样边界修复（2026-10-15）

新的原始cfg/seed真实run使用了完整cfg和`UVM_LOW`，命令行确认：
`MAIN_TRANS_NUM=10000`、enqueue权重为`1/98/1`、Uncache response delay权重为`1/1/1/1`、
response reorder开启，且原始cfg的`MEMBLOCK_MAIN_MEM_RANGES_EN=0`。该run自然在
`11440.900ns`结束，退出原因是累计10条`UNCACHE_PROGRESS` error，不是fatal或EDA超时。

10条error全部来自监视器把未占用RTL entry的X/Z payload当成活动字段检查。`uncache_lq_req_valid`
和`uncache_top_valid`中的无效槽位可能没有初始化的state/ROB/slaveId，TL A/D和LSQ response在
valid为0时的payload也不构成有效事务；把这些字段整体拼接进`$isunknown`会在真实压力下错误退出，
因此该run没有产生可用于判断owner释放链的快照，不能据此认定DUT或framework泄漏。

本轮最小诊断修复：

1. 先确认LQ/top valid mask已知，再仅对valid槽位读取state、ROB、slaveId、flush、writeback和
   top source状态；无效槽位的X/Z在镜像中屏蔽为0。
2. 对`idResp`、LSQ response、TL A/D只在valid为1时检查payload；free mask和valid本身仍保持严格
   X/Z检查。该修复不改变任何ready、response、free或NACK行为。
3. framework responder watchdog不再只在16个source全部占用时打印；只要存在outstanding owner，
   首次及每256个service cycle打印source、地址、A.fire cycle、age、response queue、current D、
   timer和source mask。source仍只在真实D.fire释放，提前复用和无owner D仍fatal。

这一步完成后才能区分：LQ freeList占满但framework owner未满、framework A后D未完成、D已完成但
top/LSQ没有闭合，或LQ进入state 3后writeback/flush没有free。行为修复仍不得drop NACK、固定延迟
free或直接改freeList；只有明确的terminal owner边才能进入最小功能修改。

### 7.21 问题 E 采样修复后的闭环判据

下一轮真实run必须自然结束并同时保存：

- `UNCACHE_PROGRESS`中每个活动LQ slot的state/ROB/slaveId/needFlush/nc/mmio/free；
- top source的valid/inflight/waitSame/waitReturn；
- idResp、TL A/D、LSQ response的valid/ready/source/id；
- `UNCACHE_RESPONDER`的A.fire/D.fire owner账本、queue/current-D/timer及owner age。

首条不推进边按以下顺序判定：framework owner没有D、D已到但top source不转LSQ response、LSQ
response握手但LQ slave owner不匹配、LQ state 3没有写回，或redirect state 2等待真实response。
只有证据闭合后才允许提出绑定该terminal事件的行为修复，并为该边补专项directed和真实seed复跑。

### 7.22 问题 E producer闭合后的资源链复核（2026-10-15）

新 producer 已将重复事件固定为 LoadQueueUncache NACK：UID862、ROB=1/133、
`dynamic_epoch`从775继续增加，rollback input=3、subtype=5、`candidate=winner=0x8`、
`uncache_known=0x7`、`uncache_free=0`，frontier停在862。这个事实证明反复事件来自
Uncache buffer allocation failure，但还不能证明是 TL response owner永久占用。

当前 RTL 的真实释放链是：LoadQueueUncache freeList分配entry，entry request被顶层Uncache
接受并通过`idResp(mid,sid)`绑定source；framework在TL A.fire建立response owner，只有D.fire
释放source；顶层再产生LSQ response，LQ按slaveId匹配，最后由nc/mmio writeback或redirect flush
驱动freeList.free。因此`uncache_free=0`不等于framework response queue已满，可能是slot停在
未发出request、state=2等待response、state=3等待写回，或redirect下等待真实response后flush。

本轮诊断保留DUT侧D.valid，并由framework responder的真实D.fire owner ledger补充ready握手信息；snapshot同时报告每个活动LQ slot的state/ROB/slaveId/
flush/writeback/free、top source状态、idResp、LSQ response fire、TL A.fire和TL D.fire。该修改不改变
ready、response调度、freeList、NACK或flush行为。

### 7.23 Review 21 资源full证据门槛

第一轮review覆盖：LQ 16-slot全占但top source为空、top source处于waitSame、state=2且needFlushReg、
state=3但writeback ready为0、LSQ response id与slaveId不匹配，以及freeMask与canAllocate时序不同。
第二轮确认只有`valid && ready`才记为A/D/LSQ response fire；D.valid=1而D.ready=0只是hold，不是释放。
source提前复用、ownerless D和无匹配response仍保留fatal。当前没有充分证据批准功能性free、NACK
drop或固定延迟修改；若后续证明已完成terminal事件但下一层不推进，才按第一条断点做最小修复。

### 7.24 问题 F：uid0 普通 store 被错误套用双 real-WB 门控（2026-10-15）

新真实 run q 尚未推进到 UID862，在 `3.142ms` 左右持续打印：

```text
[LSQ_COMMIT] hold pendingPtr before scalar fault dual real-WB uid=0
  head=0/0 hold=1/159 sta_wb=0 std_wb=0
```

q 的日志显示主表在 `290.900ns` 建立，之后从 `295.900ns` 开始只重复该 hold；在该窗口内没有
redirect、fault terminal 或 store-fault record。进一步确认 q 不是完整原始 cfg：命令引用的
`plus_file` 目录中没有 `tc_l2tlb_fault_2pct_no_trigger.cfg`，运行时回落到默认配置，日志实际为
`main_trans_num=100` 且 `MEMBLOCK_DISPATCH_ISSUE_SEQ_EN=0`。这解释了为什么 q 停在 uid0，但不能掩盖
框架门控本身的逻辑错误。

根因是 `lsq_commit_handler::head_pending_ptr_blocked()` 原先把“所有 scalar store 且 STA/STD
尚未双 WB”当作 fault barrier。普通 store 的 STA/STD WB 依赖正常 pendingPtr/commit sideband
推进；在 issue worker 未启动、尚未入队或普通路径尚未产生 WB 时，先把 pendingPtr 回退到前一 ROB
位置会形成自锁。该 uid 既没有 redirect-deleted owner，也没有 terminal/fault 状态，不能等待
fault 双 WB。

修复原则：普通 store 保持原有 pendingPtr 推进；只有 `pending_store_fault_matches(uid)` 已经
证明某个 STA/STD target fault 已捕获，并且另一个 real-WB 尚未到达时，才启用双 WB barrier。该
条件不改变 fault finalize、fault commit、redirect 或 owner release 语义，也不把未证明的 fault
当作正常提交。

Review 22：检查了四类遗漏场景：普通 store（无 pending fault）、STA-first/STD-first store fault、
redirect 后旧 fault record 已失效、以及 fault 已双 WB 完成但 status 尚未 terminal。结论是只读取
动态 epoch 匹配的 `pending_store_fault`，并继续由既有 `store_real_writebacks_done()` 判断第二个
WB；普通 store 不再被拦截，STA/STD-first fault 仍保持 barrier，epoch 失配会由
`pending_store_fault_matches()` 拒绝。没有放宽 fatal、没有伪造 WB、没有无条件推进 fault cursor。

Review 23：代码路径自审确认 `head_pending_ptr_blocked()` 同时服务 idle xaction、normal head
sideband 和 fault candidate；收窄后 normal store 的 `pendingst/pendingPtr` 可发出，而 fault
candidate 仍不会被选中，直到 pending record 和双 real-WB 条件满足。现有
`uid_is_fault_terminal_candidate()`、`mark_fault_rob_commit_uid()` 和
`sync_modeled_head_after_fault_terminal()` 的双 WB 检查保持不变。

最小 coding 修改：`lsq_commit_handler.sv` 仅将门控条件增加为
`pending_store_fault_matches(uid)`。专项验证使用现有 `tc_dispatch_real_store_wb_smoke`、
`tc_dispatch_real_store_sta_wb_smoke`，并保留 pending-MMIO、ROB admission 和 RM provenance directed
回归；专项日志必须确认 uid0 产生正常 pendingPtr/STA/STD WB，不再无限重复 hold。

### 7.25 问题 F 复核：三个 fault token 门控点与 uid130 动态实例（2026-10-15）

快速代码审查发现 Review 23 仍遗漏了三个 fault-token 生命周期入口：
`uid_is_fault_terminal_candidate()`、`mark_fault_rob_commit_uid()` 和
`sync_modeled_head_after_fault_terminal()` 仍以“所有 scalar store 且 STA/STD 未双 WB”为条件。
`common_data_transaction::try_retire_committed_uid()` 也有同样的终态回收检查。这样即使
`head_pending_ptr_blocked()` 已经识别出普通 store/旧 epoch，fault token 仍会继续等待旧实例
的 `std_writeback`。

uid130 证据把这个边界分成两个不同问题：

1. STA fault 已捕获、STD 仍 queued 但尚未 dispatch：pendingPtr 不能回退，否则 STD 没有机会
   发射，形成框架自锁；但 fault candidate/commit 仍必须等待双 real-WB，不得提前进入 terminal。
2. STA fault 的当前动态实例已被 redirect 覆盖，或 `dynamic_epoch` 已变化：该实例的 pending
   record 必须失效，旧 STD late WB 不能污染新实例；fault token 必须先识别 epoch/flush/killed
   失配，再决定是否等待，不能先读取当前 UID 的 WB bit。

本轮实现引入两个统一 helper：

- `store_fault_real_wb_pending(uid)`：只接受 active、未 redirect/flushed/killed、且
  `pending_store_fault.dynamic_epoch == status.dynamic_epoch` 的 record；它用于 fault
  candidate、fault commit、fault terminal 和 fault retire 的严格双 WB 屏障。
- `store_fault_blocks_pending_ptr(uid)`：在上述严格屏障基础上，只有 pending record 的另一
  target 已经 `dispatched` 才阻塞 pendingPtr。STA-first 且 STD 未 dispatch 时返回 false，
  允许 STD 正常入队/发射；STD-first 对 STA 对称处理。

Review 24：逐项检查了所有 `store_real_writebacks_done()` 调用。普通完成路径的 WB bit
   汇总保持原语义；所有 fault candidate/commit/sync/retire 的“是否继续等待”入口改为
   `store_fault_real_wb_pending()`，避免无 pending record 的普通 store 和 redirect-deleted
   old epoch 误等待。`head_pending_ptr_blocked()` 改用 `store_fault_blocks_pending_ptr()`。

Review 25：检查 redirect/reissue 清理链。`prepare_uid_for_redirect_reissue()`、
`reset_uid_after_older_redirect()` 和 `clear_uid_dispatch_result()` 都会删除 pending
   record、清除目标 dispatch/WB 状态和 target flush epoch；`pending_store_fault_matches()`
   额外拒绝 `redirect_pending/flushed/issue_killed`。迟到 WB 仍必须通过 active、target
   issue epoch、replay sequence 和当前 target dispatch 校验，因此旧 STD 不能设置新实例
   的 `std_writeback`。没有放宽 fatal、伪造 STD WB、无条件 drop 或按 UID 猜测 epoch。

专项验证要求：

- 已有 dispatch fault smoke 继续覆盖 STA fault、双 real-WB finalize、fault token commit 和
  terminal retire；编译后必须重新运行。
- 新增/复用诊断需明确检查 STA-first/STD-not-dispatched 不形成 pendingPtr 自锁、被 redirect
  的旧实例不进入 fault wait，以及同 UID late STD 不改变新 epoch 的 WB 状态。
- 真实 seed 仍使用 `37617171`、`MAIN_TRANS_NUM=10000` 和完整原始 cfg；旧 EDA 进程不参与
  本轮结果，也不被手工终止。

### 7.26 Review 26：helper 语义专项与 UID130 遗漏场景闭环（2026-10-15）

专项序列 `run_store_fault_helper_semantics_scenario()` 对统一 helper 做了四段检查：

1. 普通 scalar store 没有 pending fault record 时，
   `store_fault_real_wb_pending()` 和 `store_fault_blocks_pending_ptr()` 都必须为 0；
2. STA fault 已捕获、STD 仍 queued 但尚未 dispatch 时，必须保持 fault terminal 的双 WB 等待，
   但 pendingPtr 不得阻塞，避免 uid130 的 STA-first 自锁；
3. STD 已 dispatch 但未 real-WB 时，pendingPtr 必须重新进入严格屏障；
4. 双 real-WB 到达后屏障释放；redirect pending 或 dynamic epoch 变化后，旧 pending record
   不得被当前 UID 实例重新观察。

该专项运行于新编译 binary `fault2_store_fault_epoch_helper_compile_20261015_b`，日志为：

`mem_ut/ver/ut/memblock/sim/fault2_store_fault_epoch_helper_compile_20261015_b/log/`
`tc=tc_dispatch_fault_smoke_ts=virtual_base_sequence_cfg=default_seed=37617171_rtl_store_fault_helper_semantics_directed_20261015_c.log`

结果为自然 `$finish`、`TEST CASE PASSED`、`UVM_ERROR=0`、`UVM_FATAL=0`。日志同时确认原有
Load fault、STA fault、双 real-WB finalize、fault token commit/deq 场景继续通过。

第二轮自审结论：helper 只依赖 active/current dynamic epoch/pending fault record，普通 store
不会进入 fault barrier；STA-first 未 dispatch 的另一 target 不会被错误回退；已 redirect 或
epoch 失配的旧实例不会因 late STD WB 再次进入 fault wait。该专项没有降低 checker severity、
伪造 writeback 或无条件 drop；完整随机验证仍需使用原始 cfg 和真实 seed 完成。

### 7.27 Review 27：UID134 同 dynamic epoch 的 STA tombstone/replay livelock（2026-10-15）

#### 7.27.1 真实日志事实

真实 seed `37617171` 的低 verbosity 日志确认 UID134 在同一个
`dynamic_epoch=72` 内发生如下链路：

```text
首次 tombstone: 46508ns, issue_epoch=9953, replay_seq=61
后续每次 STA feedback failed:
  capture helper coalesce retained=(issue_epoch=9953, replay_seq=61)
  current issue_epoch/replay_seq 持续增长
  writeback_status_handler 仍 push_feedback_event()
  exception_redirect_replay_handler 继续 mark_replay_pending()
```

现有 log 还出现了 `current_issue_epoch=230635`、`current_replay_seq=220542`
而 history 仍为 1 的证据。由此确认 livelock 不是 tombstone queue overflow，也不是
redirect 新实例误命中，而是 capture 的返回值只表示“该 identity 可接受”：首次创建和
同 stable scope 的重复 coalesce 对 caller 都返回 1，caller 因而把重复 failed 当成新的
replay 请求。`replay_seq` 持续增加正是该自激闭环的直接结果。

#### 7.27.2 决策矩阵

| 场景 | tombstone capture | 是否 enqueue replay | late fault 行为 |
| --- | --- | --- | --- |
| 首次 `(dynamic_epoch, ROB, SQ)` | 新建一条 record | 是，保留首次 replay | 允许通过该 record 消费 |
| 同 dynamic epoch、同 ROB/SQ、不同 issue/replay | coalesce，保留最早 record | 否，阻止重复 replay | 原 record 仍可被 raw fault reader 读取 |
| 完全相同 issue/replay identity | 严格复核 ROB/SQ/flush，视为重复 | 否 | 不改变原 record |
| redirect 后新 dynamic epoch | 旧 history 按既有 redirect 清理 | 是，新实例首次 failed 允许 replay | 只能消费新实例 record |
| 新 dynamic epoch 但旧 history 未清理 | 不得按旧 record coalesce；应暴露生命周期错误或新建后由清理审计发现 | 不允许借旧 record 静默恢复 | 不能跨 epoch 消费 |
| 首次 tombstone 后 raw late fault | 不新增 replay | 否 | 保留首次 record，允许 fallback 读取并消费 |
| normal STA writeback | 不读取 tombstone | 不适用 | 继续要求 current issue snapshot |

#### 7.27.3 最小修复方案

修改 `common_data_transaction::capture_sta_late_fault_tombstone()`，新增
`output bit created_new` 结果。函数的成功返回值继续表示输入属于合法的当前
STA/tombstone scope；`created_new` 专门区分是否真的 append 了第一条 record：

1. 新 dynamic scope 首次 append 时置 `created_new=1`；
2. 完全重复或同 dynamic epoch/ROB/SQ coalesce 时置 `created_new=0`，保留最早
   `issue_epoch/replay_seq/flush_epoch`，不覆盖历史；
3. `writeback_status_handler::handle_issue_feedback_event()` 只有在
   `created_new=1` 时调用 `push_feedback_event()`；重复 coalesce 直接返回已处理，
   不再触发 `mark_replay_pending()`；
4. redirect/reset/terminal/fault-consume 的既有
   `clear_sta_late_fault_tombstones()` 不变，因此新 dynamic epoch 不会复用旧 record；
5. raw late fault 仍走既有 `read_sta_late_fault_tombstone()`，不因为重复 coalesce
   被删除或降级为 warning。

该方案不按 UID 猜测 epoch，不无条件 drop fault，不修改 DUT feedback 协议，也不降低
fatal/error severity。它只阻止已经被同一 stable scope 的首条 tombstone 覆盖的重复 replay
请求。

#### 7.27.4 遗漏场景自审

第一轮自审检查了：同一 epoch 的重复 failed、不同 issue/replay 的 coalesce、完全相同
identity 的重复、redirect 后的新 epoch、late fault fallback 和 normal STA writeback。
结论是 `created_new` 必须由 capture helper 产生，不能由 caller 根据 history size 推断，
否则空 history、并发清理和 queue overflow 边界会被误判。

第二轮自审检查了：同一 service batch 内两条 failed、redirect 覆盖与 failed 同拍、旧
replay event 已在 `exception_event_q` 中而新 failed 到达、以及 tombstone consume 后再
收到 duplicate failed。处理规则是：已入队的首条 replay仍只消费一次；重复 coalesce 不再
增加 queue；redirect-first 仍由既有 batch handler决定，旧实例清理后新 epoch重新允许
创建；late fault consume 清除 history 后，后续旧 epoch failed 不得重新创建或 enqueue。

#### 7.27.5 专项验证要求

- same epoch duplicate fail：首条 failed 入队并产生一次 replay，第二条 coalesce 后
  `exception_event_q` 不增加、`replay_seq` 不再增长、history 保持 1；
- redirect/new epoch fail：旧 history 清除并改变 dynamic epoch 后，首条新实例 failed
  仍可创建新 tombstone并入队，不能被旧实例静默抑制；
- late fault after tombstone：重复 coalesce 后仍能用首条 record 恢复 late fault，且
  tombstone consume 后不残留；
- 保留原有 pending-MMIO、ROB admission、Store fault helper、Load/STA fault terminal
  directed，以及完整真实 seed 的 checker severity。

#### 7.27.6 首轮专项失败复核与断言修正（2026-10-15）

首次 `fault2_sta_tombstone_coalesce_directed_20261015_c` 已自然结束，日志确认核心
coalesce 行为正确：同一 dynamic epoch 的第二次 STA failed 只产生
`coalesced duplicate STA failed feedback`，没有新增 recovery event 或 replay。
专项随后在 late-fault 检查处失败，原因是测试把 `exception_pending=1` 当成残留状态。

源码链路证明 late tombstone consume 会先建立 pending store-fault record；STA/STD 两个
real-WB 都已完成后，`finalize_pending_store_fault()` 合法设置
`fault=1, exception_pending=1`，并清除 tombstone、pending replay 和 recovery event。
因此该 `exception_pending=1` 是 fault terminal 的有效结果，不是 livelock 残留。

已将 directed 断言修正为：`fault=1`、`exception_pending=1`、无 tombstone、无
`replay_pending`、`exception_event_q` 为空；未放宽任何 checker severity，也未改变
capture/handler 的 coalesce 语义。

#### 7.28 Review 28：专项 fatal 与源码/binary 不一致（2026-10-15）

`fault2_sta_tombstone_coalesce_directed_20261015_c` 自然结束时报告了
`soft_test_memblock_dispatch_fault_smoke_sequence.sv:296`，日志内容仍为旧版本的
`late STA fault left stale tombstone/replay state`。该旧版本条件把
`status.exception_pending` 直接当成失败项，所以在日志已显示
`history=0 fault=1 exc=1 replay=0 events=0` 时必然触发 fatal；它不是生产逻辑错误，
也不是 4-state 条件误判。

复核 `status_transaction.sv` 确认 `fault`、`exception_pending` 和 `replay_pending` 均为
2-state `bit`。同时，源码已经修正为要求合法 fault terminal：
`fault=1`、`exception_pending=1`、`replay_pending=0`、tombstone/history=0、
recovery event=0。旧日志中的旧 fatal 文本证明 `c` mode 运行的是修正前 binary，不能
作为 `d` mode 修正后的测试结果。

为消除 handle 状态在断言和 `$sformatf` 中重复读取带来的诊断歧义，专项现在先把上述
终态字段和 queue size 快照到局部变量，再用快照执行单一断言；这只改测试断言，不放宽
任何生产 fatal/error，也不改变 tombstone capture、coalesce 或 late-fault finalize。

### 7.29 Review 29：UID118 STA-first 后 SQ deq/release 提前释放（2026-10-15）

#### 7.29.1 新失败证据与根因

真实 seed `37617171` 的 `review27_coalesce_real_20261015_g` 在约 `31743.4ns` 观察到 UID118：

```text
SQ deq before STD real-WB uid=118 epoch=69
sta_dispatch/wb=1/1 std_dispatch/queued/wb/pass=0/1/0/0 pending_fault=1
release sq mapping uid=118
```

随后 `terminal_done_uid=118` 永久不再推进，LSQ commit/enqueue 只报告 no-progress。
这不是 Review 27 的同 epoch tombstone coalesce livelock：UID118 的 pending store-fault
record 仍然有效，STA 已完成，STD 还未 dispatch；pendingPtr 侧已正确放行 STD，但
`preflight_dut_sq_deq_from_start()` 仍把该 owner 当普通 live SQ entry，接着
`commit_dut_sq_deq()` 先推进 `lsq_ctrl.sq_deq_ptr/sq_free_count`，再调用
`release_uid_sq_mapping()` 删除 queued STD 和 UID mapping。STD 从此没有 SQ owner，永远
无法产生 STD real-WB，而 fault terminal 又严格等待双 real-WB，形成永久闭环。

#### 7.29.2 决策矩阵

| 当前 UID 状态 | pendingPtr | SQ deq/release | 解释 |
| --- | --- | --- | --- |
| 普通 scalar store，无当前 epoch pending record | 原有规则 | 允许 | 不改变普通 store 吞吐 |
| 当前 epoch STA fault，STD 未 dispatch | 允许 | 禁止 | 让 STD 能发射，但保留 SQ owner |
| 当前 epoch STA/STD fault，缺任一 real-WB | 按已有严格规则 | 禁止 | 不能丢失另一侧 real-WB |
| 双 real-WB 已到并 finalize，或当前 record 已被 epoch/redirect 失效 | 原有规则 | 允许 | 可正常 deq/retire；旧实例不阻塞新实例 |
| redirect 清理旧实例 | 不适用 | 仅显式 cleanup 例外允许 | 由 redirect owner/cancel 机制接管，不能伪装为正常 SQ deq |

#### 7.29.3 最小修复

1. `common_data_transaction.sv` 新增 `store_fault_blocks_sq_deq(uid)`，复用严格的
   `store_fault_real_wb_pending(uid)`，因此只匹配 active/current dynamic epoch、未 flush/
   killed 的 pending record。
2. `lsq_commit_handler.sv::preflight_dut_sq_deq_from_start()` 在每个 live SQ owner
   加入该 helper 检查；任一 owner 被阻塞时清空整个 item list并返回失败，保持 raw batch
   原子性，不推进 SQ pointer/free count，也不释放 UID mapping。
3. `release_uid_sq_mapping()` 增加默认禁止绕过 preflight 的 invariant。只有
   `prepare_uid_for_redirect_reissue()` 的旧 fault redirect cleanup 显式传入 cleanup 例外，
   防止 redirect 清理和正常 deq 共用接口时误判；普通 bypass 直接 fatal，保留问题可见性。
4. pendingPtr 仍使用 `store_fault_blocks_pending_ptr()`：STD 未 dispatch 时返回 false，
   所以修复不会重新引入 STA-first 自锁。

#### 7.29.4 遗漏场景复核

第一轮检查了 pointer-capable SQ deq、V2 count-only SQ deq、full raw 的联合 LQ/SQ
preflight，以及多 entry batch的部分提交风险。统一在 SQ preflight 失败，避免 LQ 或前序
SQ entry 单独释放；`apply_raw_ctrl_deq()` 已有联合预检，因此失败时不会推进另一侧。

第二轮检查了普通 store、epoch mismatch、redirect-pending old instance、STD 已 dispatch
但未 WB、双 WB 完成、以及 redirect reissue cleanup。普通/失配路径不匹配 helper；旧实例
cleanup 使用显式例外并随后清理 pending record；正常 deq不能绕过 guard。没有降低 checker
severity、伪造 STD WB、无条件 drop 或改变 seed/事务数量。

#### 7.29.5 专项验证

`run_store_fault_helper_semantics_scenario()` 新增：

- STA real-WB/fault 已到、STD queued 未 dispatch时，SQ pointer、free count、mapping和
  `lsq_deq` 均保持不变；
- STD dispatch 但未 real-WB时仍保持阻塞；
- STD real-WB 到达并 finalize后，SQ deq 才推进并释放 mapping；
- 既有普通 store、redirect/epoch mismatch、双 WB、fault terminal和 tombstone 专项不删减。

#### 7.29.6 实现后自审与测试记录

实现自审重点是 `commit_dut_sq_deq()` 的先 `release_sq()` 后 mapping release 顺序，确认
guard 必须位于 preflight而非只放在 mapping release；mapping release 的 fatal invariant
用于捕获未来新增绕过入口。下一步使用全新 mode 编译并运行 directed，随后以完整原始 cfg、
seed `37617171`、`MAIN_TRANS_NUM=10000` 启动真实回归；旧诊断仿真保持运行且不纳入本轮结果。

实现验证结果：

- 编译 mode：`fault2_sq_deq_guard_compile_20261015_s`，binary 为
  `mem_ut/ver/ut/memblock/sim/fault2_sq_deq_guard_compile_20261015_s/exec/simv`；
- directed mode 复用上述 binary，note 为
  `review29_sq_deq_guard_directed_20261015_t`，自然 `$finish`；
- directed 日志在 STA-first/STD-not-dispatched 和 STD-dispatched/no-WB 两阶段均出现
  `defer SQ deq until STD real-WB`，双 real-WB finalize 后出现正常 SQ preflight/release；
- 结果为 `TEST CASE PASSED`、`UVM_ERROR=0`、`UVM_FATAL=0`，仅保留既有两个无 default
  sequence 的 warning；`git diff --check` 通过。

### 7.30 Review 30：UID862 Uncache subtype=5 资源不收敛断点（2026-10-15）

#### 7.30.1 当前真实run状态

真实回归 `review29_sq_deq_guard_real_20261015_u` 仍未自然结束；日志仍反复出现
`terminal_done_uid=862`，没有 `$finish`、`TEST CASE PASSED` 或最终 RM 汇总。日志绝对路径为：

```text
/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fault2_mmio_owner_20261012/mem_ut/ver/ut/memblock/sim/review29_sq_deq_guard_real_20261015_u/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=37617171_rtl_review29_sq_deq_guard_real_1_98_1_seed37617171_20261015_u.log
```

#### 7.30.2 首次失败前后的证据链

在 `257253.400ns`，framework 仍能完成前一批 LQ deq；紧接着
`257255.900ns` 第一次观察到 Uncache NACK：

```text
nack=234 sample=51412 rob=1/133
lq_valid=0xb9df lq_free_mask=0x4620
state_count=11/0/1/0 flush_now/reg=0x0000/0x0000
free=0x0000 nc/mmio=0x0000/0x0000
top_valid=0x0001 inflight=0x0001 waitSame=0x0000 waitReturn=0x0000
idResp=0/mid=15/sid=0 lsqResp=0/1/id=15/fire=0
A=0/1/fire=0/src=15 D=0/src=0
slot0=state0 rob1/82 ... slot15=state2 rob1/63 sid=0
```

这说明本次 NACK 的直接条件是 DUT `LoadQueueUncache` free list 已空，而不是
framework 侧已经有 16 个 response owner。16 个 entry 中当前快照有 11 个处于
`LoadQueueUncache.UncacheEntry` 的 `s_idle`，1 个处于 `s_resp`；顶层 Uncache 只有
1 个 inflight，A valid 但没有 fire，D valid 为 0。RTL 中 `s_idle` 的含义是等待
`pendingMMIOld && req.uop.robIdx == pendingPtr`，不是等待 D response。

同一拍之后，framework 先对 UID862 执行 LQ deq/release，再记录
`ROLLBACK_SELECTED uid=862 dynamic_epoch=28 subtype=5 input=3`。此后每次 reissue
都重复该顺序：Uncache free 仍为 0，UID862 生成新的 dynamic epoch，随后普通 LQ
deq/release，再被 `redirect_flush` 清理。日志中后续已出现 `dynamic_epoch=1764..1793`
而 `terminal_done_uid` 仍停在 862。这证明当前 livelock 的循环边是：

```text
旧 Uncache entry 占满 DUT free list
  -> 多数 entry 停在 s_idle，等待 pendingPtr
  -> 新 UID862 无法分配 Uncache entry，产生 subtype=5 NACK
  -> framework 对失败实例做 redirect/reissue
  -> commit frontier 仍停在 UID862
  -> 旧 s_idle entry 没有得到可观察的 pendingPtr/terminal 进展
  -> free list 仍空，下一轮再次 NACK
```

#### 7.30.3 framework owner 与 scheduler 排除项

在相邻 framework 诊断快照中，response owner 数量为 1，典型记录为
`cycle=51383 queue=1 current_d=0 timer=0 mask=0x0001`；后续也出现
`cycle=52626 queue=1 current_d=0 timer=1 due=53310 mask=0x0001`。因此当前证据不能把
问题归因于 16 个 framework source 全部占用，也不能把 source 提前复用当作根因。
scheduler 确实可能产生较长 D 延迟，但首次 NACK 时 DUT 已经是 entry free list 空、
而非 framework response owner 满；是否存在 scheduler starvation 仍需用 A.fire/D.fire
和对应 LQ state 变化闭合，不能直接改成固定延迟或强制释放。

#### 7.30.4 尚未闭合的关键链路

当前诊断还没有直接打印 `pendingPtr/pendingMMIOld`，因此以下两种情况必须区分：

1. framework 的 commit/frontier 建模没有把 pendingPtr 推到最老的 `s_idle` entry，导致
   DUT 合法地长期等待；
2. `pendingPtr` 已经覆盖某个 entry，但 framework 或 DUT 的 entry 状态/ROB 归属没有
   正确匹配，导致该 entry 没有转入 `s_req`。

还需确认 UID862 在“Uncache allocation 被拒绝、尚未获得 DUT entry”的实例中，执行
LQ deq/release 是否属于合法 redirect cleanup。该释放不能直接当作 Uncache owner
terminal；同时也不能简单禁止所有 LQ release，否则会把旧动态实例的 flush 清理和
新实例映射混在一起。

#### 7.30.5 遗漏场景审查

本轮复核了以下场景，当前均保留为必须覆盖的决策矩阵：

- 16 个 entry 中全为 `s_idle`、仅一个 `s_resp`、以及混合 `s_resp/s_wait`；
- `pendingMMIOld=0`、`pendingPtr` 不匹配、恰好匹配最老 entry；
- Uncache top A valid/ready、A.fire 后无 D、D.fire 后无 LSQ response、LSQ response
  无 LQ slaveId 命中；
- redirect 前已分配 owner 与 allocation-fail 的新 UID 两种实例；
- redirect 后 state2 owner 的 delayed flush，以及正常 writeback/flush 释放；
- framework response queue 空、单 owner、16 owner 满、乱序 D 和 timer 长延迟；
- UID862 同 dynamic epoch 重试、新 dynamic epoch 重试，以及 LQ mapping 重新分配；
- 普通 MMIO、NC load、store ack 不得被本问题的诊断或修复误伤。

#### 7.30.6 Review结论与下一步

Review 30 当前结论是：**直接 NACK producer 已闭合，DUT free list/pendingPtr 方向已
锁定，但 pendingPtr 具体值和 allocation-fail 实例的 LQ cleanup 归属尚未闭合，因此
暂不批准 coding。** 下一步仅增加只读诊断和 directed 场景：

1. 同 sample 记录 `pendingMMIOld/pendingPtr`、每个 valid entry 的 state/ROB、state
   转换以及 free 事件；
2. 将每次 A.fire、D.fire、LSQ response、writeback/flush/free 按 entry/slaveId/source
   建立一条账；
3. 单独构造“allocation-fail rollback 不拥有 DUT entry”和“已有 state2 owner 被
   redirect 后延迟 response”两个场景，验证 LQ mapping cleanup 只能匹配对应动态实例；
4. 只有证明具体断点后，才设计绑定 terminal event 的最小修复。禁止 drop NACK、固定
   延迟 free、直接篡改 free list、按当前 epoch 猜测 owner 或降低 checker severity。

本轮没有修改生产逻辑，也没有启动新的编译/仿真；现有真实仿真保持运行。

#### 7.30.7 同 sample pending sideband 诊断实施记录（2026-10-15）

本轮开始补充只读诊断，目标是把 framework 构造的 `pendingPtr/pendingMMIOld/pendingst`
与 Uncache monitor 的同一 DUT sample 对齐，并区分两类情况：

1. UID862 本次 rollback 是 allocation-fail，DUT 没有为该动态实例分配 Uncache entry；
2. UID862 失败时，DUT 已存在属于该 redirect/flush 范围的 entry，实际问题是旧 owner
   没有 cleanup 或 cleanup 与新动态实例错配。

已修改但不改变行为的文件：

- `memblock_sync_pkg.sv`：增加 `memblock_lsq_pending_diag_t` 公共快照，记录
  `sample_seq/service_cycle/pendingPtr/pendingMMIOld/pendingst/scommit/commit-head`，并提供
  publish/peek 接口；
- `lsq_commit_handler.sv`：在构造 LSQ commit transaction 后发布上述 sideband；使用已有
  `io_ooo_to_mem_lsqio_scommit` 字段，不新增 DUT 端口；
- `io_mem_to_ooo_ctrl_agent_agent_monitor.sv`：在 `UNCACHE_PROGRESS` 中增加同 sample
  匹配标志、pending pointer、MMIO/store pending 状态、commit/fault-head 标志，以及每个
  valid Uncache entry 与 rollback anchor ROB key 的 `anchor_match` mask。

首轮 compile mode `fault2_pending_diag_compile_20261015_v` 自然失败，原因是诊断代码误用
不存在的 transaction 字段 `io_ooo_to_mem_lsqio_commit`；同时 VCS 报告 progress format
少一个 `flush` 字段。该失败是诊断补丁接口错误，不是 DUT 行为结论。已修正为已有
`io_ooo_to_mem_lsqio_scommit`，并补齐格式字段，`git diff --check` 通过。

第二轮 compile mode `fault2_pending_diag_compile_20261015_w` 已自然完成并生成可运行 binary：
`mem_ut/ver/ut/memblock/sim/fault2_pending_diag_compile_20261015_w/exec/simv`。旧的
`review29_sq_deq_guard_real_20261015_u` 仍保持自然运行，未被本轮停止。

#### 7.30.8 诊断 review 结论

第一轮 review：诊断必须保留 `pending_diag.sample_seq == DUT sample_seq` 判定；不能把最近一次
framework transaction 当成当前 NACK 的 sideband，否则会把 R/R+1 的 pointer 误归因到当前
rollback。`anchor_match` 只允许在 entry `req_valid=1` 且 payload 已知时计算，空槽的 X
payload 不得当成 owner。

第二轮 review：`pendingMMIOld=0`、pointer 不匹配、pointer 恰好匹配最老 state0 entry、以及
已有 state2 owner 四种状态必须分别记录；`anchor_match=0` 只能证明本次 allocation-fail
没有同 ROB owner，不能单独证明 younger state0 entry 一定能被释放。还必须结合
`pendingMMIOld/pendingPtr` 和 `A.fire/D.fire/LSQ response/free` 的跨拍链路。

当前批准结论：**只批准诊断代码和 directed 诊断，不批准任何 admission、free-list、NACK
drop 或 owner cleanup 行为修改。** 待第二轮 binary 成功后，先跑 directed 诊断并取得实际
`pendingDiag`/`anchor_match` 证据，再决定是否进入行为方案 review。

#### 7.30.9 诊断专项结果与当前证据边界（2026-10-15）

使用上述 binary 的两个独立 mode 均自然结束：

- `fault2_pending_diag_directed_20261015_ab`：
  `memblock_pending_mmio_directed_vseq`，日志为
  `sim/fault2_pending_diag_directed_20261015_ab/log/tc=basicTest_ts=memblock_pending_mmio_directed_vseq_cfg=default_seed=37617171_rtl_review30_pending_diag_directed_20261015_ab.log`；
  在 365.900ns `$finish`，`TEST_PASS`，`UVM_ERROR=0`，`UVM_FATAL=0`。该专项覆盖了
  pending-MMIO owner contract、R/R+1/R+2 survivor/stale/mixed 及既有负向 fatal。
- `fault2_pending_diag_rob_directed_20261015_ac`：
  `memblock_rob_admission_window_directed_vseq`，日志为
  `sim/fault2_pending_diag_rob_directed_20261015_ac/log/tc=basicTest_ts=memblock_rob_admission_window_directed_vseq_cfg=default_seed=37617171_rtl_review30_rob_diag_directed_20261015_ac.log`；
  在 290.900ns `$finish`，`TEST_PASS`，`UVM_ERROR=0`，`UVM_FATAL=0`，并保留
  `distance=160 limit=160` 的阻塞检查。

这两个 directed 没有产生真实 `UNCACHE_PROGRESS` NACK，因此不能从专项日志证明 UID862 的
`pendingMMIOld/pendingPtr` 或 `anchor_match`。它们只证明诊断代码没有破坏既有问题2/3
专项。当前仍未批准行为 coding；需要使用完整 cfg/真实 seed 的新诊断 binary 观察至少一条
同 sample `pendingDiag=1/1` 记录，区分 `anchor_match=0` 的 allocation-fail 与存在匹配
owner 的 cleanup 场景。
