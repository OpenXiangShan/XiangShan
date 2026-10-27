# V2 PMA/PMP raw CSR FIFO 持续消费者修复计划

日期：2026-10-27；分支：`codex/memblock-csr-prefix-fix-20261025`。状态：round2 REVIEW_FAIL blocker 已修订，等待 round3 独立复审。
本版取代此前六轮复杂方案；用户明确授权本问题 RM/测试框架编码、专项验证及独立提交。

## 1. 术语与职责

| 术语 | 当前语境含义与落点 | 示例 |
|---|---|---|
| capture sample | CSR monitor 捕获外部写入的全局采样序号 C | PMPADDR 在 C 入 raw FIFO |
| effective boundary | 外部 CSR 经 RTL 两级延迟后的生效采样 C+2 | service 在 published>=C+2 才应用 |
| published sample | CSR/fence 两 producer 均完成发布的 sample，`lifecycle_event_published_seq` | 不读取仍在发布中的 current sample |
| raw FIFO | `raw_pma_pmp_csr_write_q`，唯一有序审计和延迟缓存 | 连续 PMPADDR、PMPCFG 保序 |
| reset domain / epoch | testcase startup domain 合法使用 epoch 0；第一次 runtime reset 后使用非零单调 epoch，producer、consumer 必须同域同代 | startup 的 epoch 0 事件不得跨入 runtime epoch 1 |
| generation/history | model 每次有效修改的版本和表快照 | history 的 write_sample 改为 C+2 |
| prepare/precheck/commit | 离线准备、只读复核、无等待提交 | 第二笔非法时第一笔也不提交 |
| ticket/revision | 整个成熟前缀的准备结果和状态版本 | 重复或过时 ticket 零副作用失败 |
| barrier | 旧 UID 完成、阻挡后续 UID 的 CSR 控制屏障 | 不存在普通 request 与 PMP 写正常重叠窗口 |
| expected count | 从发送前 producer 累计数加计划 write 数计算的完成门槛 | 最后一笔未捕获或未成熟均不能释放 |

主职责：CSR producer 只记录事实；持续 service 独立维护 model 和 FIFO；barrier/startup 只等待全部预期写成熟且表匹配；issue residency 只读已提交 history；end_test_check 只审计。

## 2. 技术边界与明确删除项

RTL `MemBlock.scala` 对 csrCtrl 使用 DelayN(...,2)，本任务不修改 RTL、Scala、DUT interface 或 driver 激励。
事件 C 仅在 producer-complete W 满足 `W>=C+2` 时成熟，history 记录生效时间 C+2，查询使用 `effective_sample<=issue_sample`。
不再保留“普通同拍 request 必须旧 generation”的设计；删除 prospective generation helper 和 residency/replay composite transaction。
保留已有 UID capture ticket：它属于混合工作树的 stale-TLB 实现，不属于本问题，不撤销也不提交。
保留已有 model batch prepare 和 raw FIFO revision，只实现一份 replay destructive owner。

## 3. Producer、reset 与统计合同

CSR monitor 对每个 generic write 填 `reset_epoch=get_l2tlb_current_reset_epoch()`，先 push 再 producer done。

reset domain 固定为两类，不引入第二个 reset owner：

1. testcase lifecycle 已初始化、`l2tlb_runtime_reset_active=0` 且 current epoch 为 0 时，epoch 0 是合法 startup domain，不表示事件未初始化。其合法性由 `l2tlb_testcase_lifecycle_initialized` 与 reset inactive 共同证明；未初始化 lifecycle 下的 epoch 0 非法。
2. 第一次 `begin_l2tlb_runtime_reset()` 把 epoch 从 0 推到 1；此后每次 runtime reset 单调递增且不得回绕到 0。active reset 只允许非零 current epoch。reset 前的 epoch 0 FIFO/ticket/history不得进入 epoch 1。
3. package 提供唯一 `pma_pmp_csr_epoch_is_current(epoch)` 判定：lifecycle 必须已初始化、epoch 必须等于 current epoch；reset active 时 additionally 要求 epoch 非零，reset inactive 时允许 startup 0 或当前非零 runtime epoch。producer、prepare、commit、completion 共用该判定，禁止各自写不同的 `epoch==0` 特判。
4. `reset_all_tables()` 在 testcase 启动时记录 current epoch（允许 0）并建立空 model/FIFO/count；runtime replay reset helper 仍只在 active、非零且 epoch 变化时执行，并在 adapter ACK 前把旧 domain 状态替换为新 domain baseline。

startup 唯一初始化顺序固定为：testcase `end_of_elaboration_phase` 调用 `initialize_l2tlb_testcase_lifecycle()`；之后 runtime owner 调用 `reset_all_tables()` 建立 epoch 0 replay baseline；write-plan preflight 调用统一 epoch helper 和 checked-count helper；全部成功后才允许 drive 第一笔 write。任一步缺失均在 drive/stage 前 fatal，producer count、FIFO、model/history、candidate、expected 均不变。software directed 复用 testcase 已建立的 lifecycle，不直接改 lifecycle flag、current epoch 或 reset-active。未初始化 lifecycle 负例使用纯函数 `pma_pmp_csr_epoch_state_is_current(lifecycle_initialized, reset_active, current_epoch, event_epoch)`；生产 wrapper `pma_pmp_csr_epoch_is_current(event_epoch)` 只把 package live state传入同一判断体。

sync package 维护每 reset domain 的 producer write count；push 成功递增，testcase clear/runtime replay reset 清零。consumer 维护同 domain 的 replay epoch、last capture watermark、applied count、service revision。

generic write 的语义分类固定如下：

- `accepted_write`：地址属于已实现 PMA/PMP CSR，且至少一个目标 entry 未被 lock/TOR-lock 拒绝。每笔 accepted write 恰好建立一个对应 PMA 或 PMP generation/history，即使所有可写字段最终与旧值相同；该子类记为 `accepted_same_value`，绝不归类为 no-op。
- `ignored_locked`：地址已实现，但本笔涉及的全部目标 entry 均因 lock/TOR-lock 拒绝。消费 FIFO、producer/applied counter 各计一次，不改 table/generation/history。
- `ignored_unknown`：不属于 PMA/PMP CSR address family。消费并计数，不改 table/generation/history。
- `ignored_unsupported`：属于 PMA/PMP address family，但 index/group/hole 在当前 V2 model 未实现。消费并计数，不改 table/generation/history。
- `ignored_noop` 只是上述 ignored 类别的统一 audit outcome，不是独立输入类别；不得把 accepted_same_value 记为 ignored_noop。

PMPCFG/PMACFG mixed-lock 时，只要至少一个 slot accepted，整笔 CSR write 建立一个 generation；锁定 slot 保持不变，解锁 slot按写值更新。model enable 只控制 RM 使用权限结果，不得关闭 producer/consumer/history。

prepare 建立可观测 `pma_pmp_csr_replay_outcome_e`：`ACCEPTED_CHANGED`、`ACCEPTED_SAME_VALUE`、`IGNORED_LOCKED`、`IGNORED_UNSUPPORTED`、`IGNORED_UNKNOWN`。audit row 保存 outcome，不再用单个 `changed` 同时表达“被接受”和“值变化”。`ignored_noop` 仅是后三种 ignored outcome 的日志汇总名，不是 enum。

地址 classifier 固定为：已实现 CFG 为 `{3a0,3a2,3a4,3a6}` 与 `{7c0,7c2,7c4,7c6}`；已实现 ADDR 为 `3b0..3cf` 与 `7c8..7e7`。CFG family 窗口 `3a0..3a7`、`7c0..7c7` 中未实现的 hole，以及 PMPADDR architecture family `3b0..3ef` 中超出当前 32-entry model 的 `3d0..3ef`，归 `IGNORED_UNSUPPORTED`；上述 family 外归 `IGNORED_UNKNOWN`。已实现地址先判断 lock/TOR-lock；全部目标被拒绝归 `IGNORED_LOCKED`。至少一个目标 accepted 后比较所有 accepted slot/entry 的 pre/post 值：任一变化归 `ACCEPTED_CHANGED`，全部同值归 `ACCEPTED_SAME_VALUE`。mixed-lock 仍只产生一个 outcome 和最多一个 generation。
`reset_pma_pmp_csr_replay_for_runtime_epoch()` 抽象职责：在 adapter runtime reset ack 前建立新代际的空 FIFO、默认 model/history 和零 watermark/count；仅 active reset 且 epoch 变化执行一次，不清其它任务状态。
内部流程：验证 active/nonzero runtime epoch；已处理则返回；重建默认 model；清本 FIFO/count；记录新 epoch、清 consumer watermark/count、推进 revision。testcase reset 同步初始化，并允许已初始化 lifecycle 的 startup epoch 0。

## 4. 唯一消费者与原子性

`prepare_pma_pmp_csr_replay(W, ticket)` 抽象职责：为已发布采样 W 的成熟连续前缀准备离线 model post-state，不改变共享状态。
内部流程：校验 model，并用统一 epoch-current helper 验证 startup 0 或 runtime 非零 domain；从 FIFO head 按索引扫描，仅扫描成熟前缀及第一个 future head；验证 valid、sample/time 非零、同 epoch、严格递增、capture watermark、`C<=MAX_U64-2`；冻结 raw revision、service revision、capture 原值。复制事件给 model 前将 sample 改为 C+2；调用已有 model batch prepare。失败返回 0，queue/model/history/watermark/count 不变。空前缀直接成功且不创建 model ticket。

`commit_pma_pmp_csr_replay(ticket)` 抽象职责：作为唯一生产 destructive owner，一次提交完整成熟前缀。
内部流程：先校验非空未消费 ticket、统一 current epoch、service/raw/model revision、完整 FIFO prefix及以下所有上限；任何失败返回 0 且零 mutation。全部通过后，无等待调用 model mutation-only helper、raw mutation-only prefix helper，推进 capture watermark/applied count/service revision，标记 ticket 消费并逐事件输出 capture/effective/generation/audit outcome。提交 helper 不 report、不分配 snapshot、不再执行可失败分支。重复提交失败。

所有计数和 revision 固定使用 `longint unsigned` 64 位语义，并在 mutation 前统一检查：

- expected count 使用 `checked_pma_pmp_csr_write_count_add(base, delta, owner)`；`delta` 先无符号扩展为 64 位，若 `base > MAX_U64-delta` 则在任何 write drive/candidate stage 前 `uvm_fatal`。
- producer push 在 count 为 MAX_U64 时 fatal，不能入队；applied batch 在 `applied > MAX_U64-batch_size` 时 commit precheck 失败并由 service fatal。
- `MAX_U64` 是 raw/service/model revision 与 generation 的永久非法哨兵，不得成为 live 值。所有单步 revision writer（raw push/prefix commit/reset、service commit/runtime reset、model commit/reset）仅在 current `<=MAX_U64-2` 时允许 mutation，结果最大为 `MAX_U64-1`；current=`MAX_U64-1` 时下一次 mutation 在第一处 live write 前失败。generation 在 prepare private post-state 上逐个 accepted write执行同一检查，批内任一失败则整批 live state不变。
- capture sample 必须 `<=MAX_U64-2`；effective sample 只能由已验证的 `C+2` 得到。
- expected/completion 比较均为 64 位无符号：`applied>=expected` 证明本计划累计门槛，且同时要求 `applied==produced` 与 FIFO 空证明当前 domain 所有已捕获写均完成。比较不得截断为 int。

所有 queue/dynamic-array `size()` 必须先无损转换为命名的 `longint unsigned delta64`，再做 `MAX_U64-delta64`、加法和比较；禁止 unsized literal或隐式 int 算术。expected overflow在drive/stage前fatal；producer overflow在入队前fatal；applied/revision/model/generation/C+2 overflow在prepare/precheck阶段失败，持续service把生产失败升级为fatal，且共享state未变。

`service_csr_pmp_pma_write_observations()` 抽象职责：主 monitor 和 dedicated completion 共用的持续入口。
内部流程固定为：reset或FIFO空返回；读取producer-complete W，W=0返回；prepare失败fatal；prepare成功但 `ticket.events.size()==0` 作为正常no-work返回，不调用commit；只有非空ticket才commit，commit失败fatal。空ticket必须非null并冻结reset/service/raw revision，`model_ticket=null`；它只证明当前head未成熟，不可提交。每拍空FIFO O(1)，非空只扫描pending prefix和第一个future head，不扫描UID主表。
`collect_runtime_context_events()` 在 reset gate 后 drain runtime CSR，再调用该 service。adapter reset 分支调用上述 reset helper。startup 自己也复用 service，因此无需后续 request。
删除 `apply_pma_pmp_csr_writes_before_request()`；issue residency 不调用 consumer，也不提交 model/FIFO。

## 5. Startup、dynamic barrier 和收尾

`csr_pmp_pma_writes_observed(expected_count)` 抽象职责：只读判断本 epoch 全部计划写已消费且 raw FIFO 空；调用者再检查 model_matches。
startup 在发送 write plan 前读取 64 位 producer count，把 `beats.size()` 无符号扩展后通过 checked-add helper生成 expected；overflow fatal 必须发生在第一笔 drive 前。等待时持续 service，必须同时满足同 startup epoch 0 domain、expected count、produced/applied 守恒、空 FIFO、profile 匹配才能 publish initial done。
dynamic 在发送计划前用同一 checked-add helper生成 expected，staged candidate 携带 64 位门槛；completion service 在 full/runtime snapshot 对应当前 action、同 runtime epoch、expected count 达成、FIFO 空及 profile 匹配后才 commit candidate/release barrier。无 PMA/PMP 改动也不能绕过当前 pending FIFO。legacy completion同样要求 FIFO drained。
static Sv39 的两笔启动写由持续主 service 在 settle 窗口成熟，原 producer 64 settle 不改。
`end_test_check()` 保持只审计：raw 非空或 produced/applied 不守恒为 error，不补消费、不清队列。

## 6. 文件与混合工作树边界

修改 sync package 本 FIFO 字段/API、CSR monitor epoch、common_data replay 和 completion、dispatch runtime service、adapter reset、initial/dynamic sequence；model 复用 batch 并删除 prospective helper、history 边界改为生效采样。
新增 software directed sequence/testcase、caller 白名单 checker。精准 stage 本问题 hunks；不 stage stale-TLB、control、issue-fire 未提交改动。不得 reset/checkout/revert 混合修改，不提交仿真产物。

新增文件使用显式白名单；共享大文件只能逐 hunk stage，禁止整文件 `git add`。commit 前审计 `git diff --cached --name-only` 和完整 cached diff，排除 stale-TLB/control/issue-fire/ROB/provenance及无关文档。checker、diff-check和目标验证必须对应 cached tree 内容。implementation review记录 cached diff SHA256；只做独立本地commit，不push，不stage sim/coverage。

## 7. 验证与审查门禁

独立 plan review 为 REVIEW_PASS 后才 coding。software directed 覆盖 C/C+1 不应用、C+2 exactly-once、连续 ADDR/CFG 保序、无 request/纯 store、startup epoch 0、runtime 非零 epoch/reset、expected-count overflow 在 drive/stage 前 fatal、ADDR accepted_same_value、CFG accepted_same_value、CFG mixed-lock、ignored_locked、ignored_unknown、ignored_unsupported、model off、重复/stale/失败提交零半更新、end audit 不消费；startup/dynamic 使用真实 DUT 回归补足。
静态 checker 限制唯一 consumer/commit caller、producer epoch、成熟条件、issue residency 无消费；执行 git diff --check。

directed 结果矩阵：

| 场景 | Table | Generation/history | 守恒与 completion | Outcome |
|---|---|---|---|---|
| ADDR accepted_same_value | 不变 | 对应侧 +1，新增一条 C+2 history | produced/applied各+1、FIFO清、完成 | `ACCEPTED_SAME_VALUE` |
| CFG accepted_same_value | unlocked slot均不变 | 对应侧 +1，仅一条C+2 history | 同上 | `ACCEPTED_SAME_VALUE` |
| CFG mixed-lock且unlocked变化 | locked不变、unlocked更新 | 对应侧 +1，仅一条history | 同上 | `ACCEPTED_CHANGED` |
| CFG mixed-lock且unlocked同值 | 全表不变 | 对应侧 +1，仅一条history | 同上 | `ACCEPTED_SAME_VALUE` |
| ignored_locked | 不变 | 不变 | produced/applied各+1、FIFO清、完成 | `IGNORED_LOCKED` |
| ignored_unknown | 不变 | 不变 | 同上 | `IGNORED_UNKNOWN` |
| ignored_unsupported | 不变 | 不变 | 同上 | `IGNORED_UNSUPPORTED` |

overflow directed分别覆盖 expected add、producer push、applied batch、raw/service/model revision、PMP/PMA generation、capture=`MAX_U64-2`可成熟和capture=`MAX_U64-1`拒绝；每项验证FIFO、table、history、counter、revision、candidate零半更新。C/C+1还验证FIFO非空时no-work无fatal且所有revision不变。未初始化lifecycle+epoch0负例验证发送前失败及所有状态不变。

caller白名单固定为：raw producer生产路径仅CSR monitor，software directed为显式测试例外；service production caller仅`collect_runtime_context_events()`、startup wait、control/dynamic completion；raw prefix commit与model batch commit的生产caller仅`commit_pma_pmp_csr_replay()`；`apply_csr_write()`只保留model兼容wrapper及已列software provenance测试；issue residency/end-test禁止service/prepare/commit/pop及裸raw queue mutation。checker扫描function、task、全文件API引用和裸queue mutation。
每次 EDA 命令前 source `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.envrc`；仅 eda01 编译仿真，新 mode，监控/record 开启，源码变更后重编译。
首测原失败：basicTest / memblock_dispatch_real_smoke_vseq / tc_dispatch_real_multi_store_wb_smoke / seed=1。
再测 nonblocking multi-store（runtime plusarg）、static Sv39、CSR random 已知 seed 903730789/1611682665/2124399471/1596235805、PBMT/PMA directed、issue-fire/provenance/ROB 门禁。记录每条实际命令和严格结果；未跑的母任务 original10/known10/fresh30 明确未执行。
PASS 必须 rc=0、summary PASS、全部 planned seed PASS、remote_unknown_jobs=0、日志 TEST CASE PASSED/$finish、UVM_ERROR/FATAL 均为0；ABORTED/INCOMPLETE/INFRA/NOT_RUN/threshold-killed 均不算通过。
新缺陷先分类/记录方案并独立 review，再在授权范围修复、原 seed 重测。implementation review 必须独立 REVIEW_PASS，核对 cached diff 后本问题独立本地 commit，不 push。完成验证后 plan 移 rm_plan/do；review 留 undo。

## 8. Round2 REVIEW_FAIL 修订记录

2026-10-27：关闭 round2 B1。固定 epoch 0 为 testcase lifecycle 已初始化且 runtime reset inactive 时的合法 startup domain；第一次 runtime reset进入 epoch 1，后续非零单调。统一 helper 区分合法 startup 0 与未初始化 0，禁止 software test直接改全局 epoch掩盖生产合同。

2026-10-27：关闭 round2 B2。expected/producer/applied/revision/generation/sample 全部明确为 64 位无符号边界；新增 checked-add 抽象，overflow 在 drive/stage 或 commit mutation 前 fatal/失败，完成比较不允许 int 截断。

2026-10-27：关闭 round2 B3。accepted_same_value 必须建立 generation；ignored_locked/unknown/unsupported消费并计数但不建 generation；ignored_noop 只作为 ignored audit outcome。补齐 ADDR/CFG 同值、mixed-lock和各 ignored 原因专项。

2026-10-27：关闭 round3 B1-B4。补齐startup/direct测试合法初始化顺序和未初始化负例；MAX_U64作为revision/generation非法哨兵并显式转换所有size；新增稳定outcome enum/address classifier和逐场景结果矩阵；固定空成熟前缀为正常no-work。恢复caller与cached-tree独立提交完整门禁。

software-only 测试按现有 API 使用 reset_all_tables(1)，随后在测试内部设置 main_trans_num=0，只审计本 FIFO，不改变生产 reset 的正数约束；不得直接赋值 current reset epoch。
