# MemBlock Load MMIO R+2 Active Survivor Review

日期：2026-10-14

对象：`memblock_load_mmio_r2_active_survivor_resolution_plan_20261014.md`

## Review 1

- 结论：有条件通过。
- 必须把“历史主表记录”和“当前producer证据”分开。terminal uid即使ROB value相同，也不能
  阻止唯一active survivor匹配。
- 必须保留完整ROB key的双flag probe，禁止按value直接选owner。
- ROB admission只排除相隔160的两个未提交owner同时active；若旧owner已提交但仍active，
  active map双flag probe会使候选数大于1，仍然fatal。
- 本次redirect保存的同value deleted producer若与uncovered survivor并存，在R时必须违反
  admission或active唯一性；合法状态下不可达。

## Review 2

- 结论：通过。
- same-key redirect reissue的activation epoch不小于redirect epoch，继续落入new candidate，
  不能因R+2放宽而写tag。
- request入口禁止deleted-owner单代窗口未清时启动新redirect；overlap finder对多个record/
  anchor直接fatal，前序redirect不能提供混合provenance。
- Store、prefetch、未dispatch Load继续计入incompatible。
- covered-old stale-drop必须位于survivor CURRENT之前；active=0 R+2 deleted helper位于其后；
  其余保持fatal。
- 最小实现应只扩展现有严格survivor条件的sample范围，并把directed期望从fatal改为CURRENT。

实现review和测试结果将在coding后追加。

## Follow-up Review 1：commit batch/redirect atomicity

- 结论：原实现有真实竞态。batch 在 driver handshake 前选择，handshake 期间可以启动
  redirect；逐 UID 再检查 flush 会把批次决策拆开。
- 不接受“把 fatal 改 warning”：前项可能已提交并推进内部 owner，后项跳过后无法恢复。
- 不接受“flush 时一律取消并重试”：store `scommit` pulse 已送往 DUT，重复驱动可能
  造成 SQ 重复提交。
- 采用全批 preflight：active redirect 覆盖任一 UID则零副作用取消；全部是完整 ROB key
  survivor则一次性应用。preflight 与 apply 位于无 timing control 的同一 function。
- cursor、frontier、watermark必须只在整批成功后统一推进。

## Follow-up Review 2：RM TLB 与 D-error provenance

- `uid98` 的有效真源是 completed UID record payload，不是提交时的 live canonical entry。
  同 VPN强制快照；跨页且无 per-UID payload时才允许 canonical fallback。
- `uid14` 的 expected hardware error只能来自 sticky L2 D-error ledger。DUT exception不能
  反向写 expected。
- ledger查询 sample必须冻结在 passive LDA采样点；commit时 sample太晚，会看到后续
  GrantData激活的错误。
- denied映射 load access fault bit5；corrupt且非denied映射 hardware error bit19，与
  `LoadUnit.scala` 的 S2/S3规则一致。
- directed必须覆盖：survivor batch成功、mixed covered batch零副作用、UID TLB snapshot
  抵抗 live overwrite、sticky corrupt/denied sample边界。

结论：两轮 follow-up review 通过，可以进入实现。

## Follow-up Review 3：UID 0 no-completed-payload 状态判定

- `wait_state=0` 是 `WAITING`。本框架的 `pte_valid` 表示response payload已冻结，不表示
  PTE语义上“无fault”；所以合法PF/AF/GPF/GAF completion同样必须是
  `COMPLETED + pte_valid=1`。
- 日志中 `VPN=0x8d34a` 没有 L2 request accept；它位于此前已完成的宽页anchor
  `0x879a6`覆盖范围。最合理且与事件顺序一致的解释是 DUT L1 TLB hit，而不是丢失一笔
  L2 response采样。
- 不接受降低severity、允许 `COMPLETED/pte_valid=0`、读取actual exception反推expected，
  或在commit时无条件读取live canonical。
- issue-hit fallback必须同时证明：当前epoch仍为WAITING、没有观察到本UID request fire、
  issue时存在一份真实完成且仍有效的response快照。任一条件缺失都保持query miss。
- `CANCELED`不能fallback。redirect取消的旧attempt即使保存过issue快照，也不能重新成为
  当前翻译真源。

结论：通过。根因是合法L1 hit缺少per-UID issue payload快照。

## Follow-up Review 4：completed-response cache 安全性

- live canonical在request accept时建立，早于response completion；因此不能兼任
  completed-response cache。
- completion cache只由 `complete_driving_response()` 在真实 sampled response边界写入，
  保存token私有、含PBMT overlay的值快照及sample/token/generation。
- range lookup可复用现有有限bucket枚举anchor，但最终payload必须从completion cache取得并
  重新执行raw match。pending live entry不能成为候选，也不能遮蔽已完成的较窄候选。
- UID issue时立即按值冻结payload和request-derived PPN；后续live overwrite、其他token完成
  或commit时刻变化都不能改写该UID快照。
- 一旦 `mark_request_fire()`，issue-hit fallback立即失效，防止旧L1快照遮蔽当前真实miss。
  response随后正常multicast并转为COMPLETED。
- `reset_all_tables()`、runtime reset清空cache；SFENCE/HFENCE C4删除live anchor时同步删除
  cache，防止硬件TLB已失效后仍生成伪hit。
- 同拍顺序闭合：completion先发生则issue可冻结；issue先发生则completion multicast使UID
  completed。两条路径均不依赖process调度猜测。
- directed必须覆盖normal/fault completion、L1 hit、pending miss、cancel/reissue、same-key
  live变化、range、SFENCE/reset及request-fire后禁止fallback。

结论：通过。当前完整run仍在自然运行，按约束尚未应用源码修改或启动重编译。

## Follow-up Review 5：UID 6889 长期 memoryViolation/replay 收敛

- 从约 `3.5ms` 到至少 `11.58ms`，commit frontier固定在 UID 6889/ROB 1/80；UID 6889 每轮
  使用新的 LQ key，被 dequeue/release 后又由 redirect flush，并以 `terminal_done=0` 退休。
- LQ key轮转和更年轻UID dynamic epoch上万证明这是反复重新准入，不是旧映射未释放；固定约
  `545-560ns` 周期且数毫秒无frontier进展，足以判定livelock，不是合法高fault慢收敛。
- 日志没有UID 6889 fault feedback或normal pass。先前“同batch Load fault导致redirect早到”的
  假设被推翻；不得因此把scalar-store fault deferral泛化到所有load/fault。
- source 8只是框架对统一 `mem_to_ooo.memoryViolation` 输出的命名。该配置RTL会把3路LoadUnit、
  uncache nack和2路LoadQueueRAW nuke共同仲裁到该端口，不能据此断言是RAW或存在冲突store。
- 当前只证明统一 memory replay 的 `flush_itself=1` 每轮自取消。日志没有DUT oldestRedirect输入
  subtype、最终event dynamic epoch或RAW冲突owner，尚不能直接改变redirect语义。
- 第一阶段只允许增加低频selected-event、owner状态、first-covered UID和frontier日志，并通过
  bind/XMR采样LoadUnit/uncache/RAW仲裁输入valid+ROB。需先区分stale event、pipeline replay、
  uncache资源、RAW依赖和framework固定重建失败条件。
- 不接受无条件drop memoryViolation、按ROB value猜owner、降低severity/checker、减少事务数、
  修改seed或降低cfg依赖压力。
- 行为修复必须保持：当前dynamic epoch真实source-8事件有效；旧epoch事件严格drop；普通非fault
  redirect立即生效；若需要replay barrier，必须绑定UID+dynamic epoch+older frontier且可证明释放。
- 后续完整重跑可设 `pl=UVM_LOW`，仅减少 UVM_HIGH I/O，不改变checker与测试内容。

结论：两轮自审通过。当前证据已证明收敛失败，但尚不足以选择行为修复；等待现有run自然结束后
先加入最小provenance并用directed/完整run闭合producer和冲突owner。

## Follow-up Review 6：rollback subtype可观测性

- 生成RTL确认该配置的oldest仲裁为3路LoadUnit、1路uncache nack、2路RAW nuke，共6路；当前
  `memoryViolation` agent只采样mux输出，确实丢失根因subtype。
- 后续不修改RTL功能接口，使用已有`uvm_hdl_read`模式读取6路valid/ROB/level和one-hot获胜条件；
  partcomp会把被引用内部net加入XMR referee。
- 仅打印6路valid仍不够：LoadUnit必须细分vp match fail、ldld `rep_frm_fetch`和misalign flush，nack必须打印
  uncache资源，nuke必须打印store输入和目标load。否则行为修复仍然是猜测。
- UID6888/ROB1/79已在`3565258.400ns`真实sqDeq并terminal，排除其fault SQ长期占用导致6889
  rollback；不能因为两者相邻就建立未经证实的producer关系。
- 诊断必须校验output ROB与获胜输入ROB一致；XMR读失败必须显式报出，禁止unknown按0处理。

结论：通过。最小诊断范围已闭合，行为修改继续等待subtype证据。

## Follow-up Review 6A：LoadUnit rollback reason与level排他性修正

- 生成RTL显示LoadUnit `io_rollback_valid` 的直接条件只有：`s3_vp_match_fail`、
  `io_lsq_ldld_nuke_query_resp_valid && rep_frm_fetch && s3_ldld_rep_inst_last_REG`、
  `s3_frm_mis_flush`。此前把`flushPipe`列为直接rollback reason不准确，已从方案删除。
- LoadUnit `io_rollback_bits_level = s3_vp_match_fail | s3_frm_mis_flush`，所以纯
  `rep_frm_fetch`只能输出level 0。顶层对RAW nuke固定编码level 1，uncache透传其level。
- UID 6889 的框架redirect为`flush_itself=1`，等价于本次output level 1。因此当前循环可以排除
  LoadUnit纯`rep_frm_fetch`，剩余候选为LoadUnit vp/misalign、uncache level-1 nack或RAW nuke。
- 该排除只用于本次根因定位；诊断和directed仍需覆盖level-0 fetch rollback，防止后续改动把
  合法非self-flush路径错误归类或屏蔽。

结论：通过。subtype候选已进一步收窄，但仍不足以直接选择行为修复。

## Follow-up Review 7：UID 6889 dequeue、frontier与自 flush 因果关系

- 后续观察已延伸到至少 `13.122608ms`，UID 6889 仍以约 `540-570ns` 周期持续轮转新的
  LQ key，未出现 terminal progress；合法高 fault 慢收敛可排除。
- 单轮严格顺序是：batch选择redirect -> UID 6889随一批load真实`lqDeq`并释放mapping ->
  active redirect生效 -> flush从UID 6889/ROB 1/80开始 -> 该UID以
  `success=0, terminal_done=0`退休。
- 因此`lqDeq`只证明DUT回收本轮LQ entry，不证明请求可以提交。commit frontier停在6889是
  自flush循环的结果，不是另一个独立cursor bug。
- 同轮有大量younger fault/redirect-covered event，但它们都位于flush起点之后，只能证明压力
  被每轮重建；在六路rollback输入可观测前，不能把它们认定为6889的producer。
- recovery barrier若最终需要实现，必须绑定当前UID dynamic epoch和已确认subtype的资源/owner
  释放条件；禁止按source 8、ROB value或固定等待拍数无条件解锁或drop。

结论：通过。收敛问题已分类为当前静态UID的动态实例无限replay；具体行为修复仍等待subtype
诊断。

## Follow-up Review 8：D/E 实现自审

- completed-response cache只在真实sampled response completion写入，live request accept不能写入；
  因此`record_valid=1,pte_valid=0`不会因存在pending canonical entry被误判为L1 hit。
- issue-hit是值快照，不保存live handle；same-key后续覆盖、另一个token完成或RM延迟读取均不能
  改写该UID证据。request fire、cancel/reissue、fence和reset清除fallback。
- DUT L1容量淘汰不要求软件预测：若已淘汰，DUT会产生真实L2 request fire并及时清除fallback；
  若无request fire，才允许RM读取issue-hit。
- 六路诊断的winner校验使用完整ROB和level；LoadUnit level-0 ldld fetch与level-1 vp/frm不会混淆。
  RAW nuke固定level 1，uncache使用其输出level。
- XMR失败不会产生全0伪subtype：monitor显式UVM_ERROR并标记diagnostic incomplete；adapter只在
  complete时执行winner/subtype证明。
- 当前实现不改变redirect仲裁、flush_itself、batch优先级或reissue行为；真实seed只用于闭合
  UID6889 producer，不能把诊断结果直接当作静默drop依据。

结论：源码方案自审通过，进入compile和directed；编译/测试发现将继续回写。

## Follow-up Review 9：D/E 编译与专项测试闭环

- 最终全量编译`fault2_d_e_diag_compile_20261015_d`通过，证明六路XMR路径可由当前partcomp配置
  正确解析；不存在仅在software directed赋值、真实层次不可引用的假通过。
- RM provenance directed自然结束且`TEST_PASS/UVM_ERROR=0/UVM_FATAL=0`。completed cache、issue
  快照、live overwrite隔离以及request-fire/cancel/reissue失效均有独立断言，因此UID 0修复不是
  commit时宽松fallback。
- pending-MMIO directed自然结束且最终错误/致命计数为0。六个输入分别被唯一选中并得到预期
  subtype；output完整ROB和level与winner输入一致，XMR smoke通过。
- ROB admission directed继续证明distance 159可接受、distance 160被拒绝，D/E修改没有放松
  ROB容量边界或破坏problem 3同步来的admission约束。
- rollback诊断目前仍只读并记录。directed只能证明decode和排他校验正确，不能证明真实seed的
  UID 6889来自哪一路；在真实`ROLLBACK_SELECTED`证据产生前不得增加generic drop、固定延迟或
  replay barrier。

结论：Review 9通过。问题 D 可进入完整真实用例验证；问题 E 可进入低verbosity真实seed producer
闭合阶段，行为修复继续以真实winner和释放条件为前置证据。

## Follow-up Review 10：zero-based response token遗漏

- 第一次真实seed在735.900ns完成首个L2TLB token 0时fatal。`next_request_token`初始化为0后递增，
  因此0是合法identity；此前directed只使用非零常量，遗漏了首个token边界。
- 不能把token生成器改为从1开始来迁就cache：token 0已是现有协议和日志中的合法值，改变生成器会
  扩大行为面并掩盖消费者的错误valid假设。
- 删除`token != 0`不会接受无provenance对象。completed cache以entry/token/sample三表同key存在
  为valid；UID冻结以`issue_hit_snapshot_valid`为valid，并继续要求WAITING、no request fire、
  sample非0、generation非0且payload generation一致。
- clear路径把token清零的同时也清除valid bit和payload；因此合法token 0不会与cleared状态混淆。
- directed必须直接使用token 0并走完RM成功查询及所有失效路径，避免仅测试cache API不测试最终
  resolver条件。

结论：Review 10通过。修复严格限定为移除错误的数值sentinel，不改变L1-hit资格或redirect行为。

### Review 10 验证结论

`fault2_rm_token0_directed_20261015_b`自然结束并通过，最终`UVM_ERROR=0/UVM_FATAL=0`。日志直接
观察到token 0的两次合法冻结，覆盖首次issue和request-fire失效后的reissue；RM成功读取冻结payload，
cancel及cache delete继续清除valid。该结果证明修复依赖显式valid和sample/generation，而不是把
cleared token 0误当成有效completion。

结论：zero-based token遗漏已闭环，可以重跑原始真实seed；原provenance fatal保持用于真正缺少
entry/sample/generation的情况。

## Follow-up Review 11：UID14 sticky D-error时间域

- 真实证据排除了TLB payload：UID14已有合法issue-hit translation，PA稳定为`0xb93a1c6`；DUT在
  writeback sample 740报告hardware error bit19，而RM对同一64B line查询到空ledger。
- production代码在GrantData最后一个`D.fire`传入`$time`，RM查询传入`sample_seq`。字段名虽为
  activation_sample，写者实际使用ns，读者使用全局序号，比较结果没有物理意义。
- 不接受把bit19从DUT actual复制到expected；这会让checker失去独立性。也不接受忽略activation
  边界；后到的GrantData错误不能污染更早的load writeback。
- AcquireBlock reservation的accept sample也应使用同一全局序号，避免pending/live诊断继续保留
  混合时间域。sample 0保持fatal，不能在CSR sample owner尚未建立时创建可查询ledger记录。
- production wrapper与directed共用同一current-sample入口，显式验证`sample-1`不可见、`sample`
  可见，防止测试仅覆盖手工数字API而遗漏真实调用点。

结论：通过。最小修改只统一provenance时间域，不改变注错权重、sticky状态机或exception映射。

## Follow-up Review 12：rollback XMR inactive-X与selected严格性

- 生成RTL的六个winner向量把candidate valid放在自身位置，并以两两ROB年龄比较构造oldest onehot。
  顶层output只消费归约AND为1的输入。非获胜LoadUnit bundle可为X，不是本次output的有效payload。
- 原monitor无条件读取所有valid/payload，会把inactive don't-care误报成九个独立error，并在winner
  校验前触发quit-count。这不是DUT功能错误，也不是producer证据。
- 不能简单把`$isunknown`删除或把X转换成0。新增candidate/winner known-mask；唯一known winner的
  selected valid、ROB、level、reason继续强制完整并与顶层output比较。selected缺失仍报告诊断不完整，
  不允许adapter猜测。
- 当某candidate valid已知为1时，即使它未获胜，其ROB/level也属于真实仲裁输入，仍必须读取；仅
  valid未知/0的非选中payload不读取。RAW store payload同样受storeIn valid门控。
- 唯一known winner成立时，其他unknown winner不能推翻该winner：该winner向量已经包含针对所有
  其他输入的年龄排他项；unknown只表示非获胜输入自身bundle未初始化。known-mask仍写入日志供审计。
- directed需覆盖两类：六路分别全known获胜；以及selected-only known、其余输入unknown的合法场景。
  selected candidate unknown、selected payload X/Z仍不得变成通过路径。

结论：通过。该修改修正诊断的有效域，保持selected provenance严格性，不改变redirect功能行为。

## Follow-up Review 13：合法 completion 的 token-0 provenance 与回归结果

- 735.900ns 调用具有真实 response sample 和非零 entry generation；唯一失败条件是 token 数值为 0。
  `next_request_token` 明确从 0 开始，因此把 token 0 当无效是消费者的 sentinel 设计错误，不是 producer
  漏采样。
- 修复没有把 token 字段本身改成隐式 valid。completed cache 继续以三张表同 key 存在作为对象有效性，
  UID snapshot 继续使用独立 `issue_hit_snapshot_valid`，且 sample/generation 必须一致。
- clear 路径同时清 valid/table presence 和 payload；因此 cleared token 0 不会与合法 token 0 混淆。
  真正缺 entry、sample、generation、WAITING 状态或 UID snapshot 的场景仍触发原 fatal。
- directed 用生产 API 走完整链路，而不是直接篡改 RM expected。token 0、D-error sample-1/sample 边界、
  request-fire 后失效、reissue 重建、cancel/delete 均得到断言覆盖。
- `fault2_rm_sample_domain_directed_20261015_c`、
  `fault2_pending_mmio_winner_first_directed_20261015_d` 和
  `fault2_rob_admission_post_provenance_20261015_d` 均自然 `$finish`，最终
  `TEST_PASS/UVM_ERROR=0/UVM_FATAL=0`。预期负向 fatal 仅由 directed catcher 精确捕获。
- rollback selected provenance 仍保持严格，ROB distance 160 边界仍拒绝；本轮修复没有放宽 checker、
  redirect 或 admission 语义。

结论：Review 13 通过。合法 token-0 completion 已闭环，原 fatal 对真正 provenance 缺失继续有效；
可以用原始 cfg 和 seed 37617171 进入完整真实用例复核。

## Follow-up Review 14：rollback output与仲裁输入的采样域

- 真实run e的9条错误全部为winner known mask零，但顶层`memoryViolation`完整ROB/level有效；这排除
  “某个非获胜payload为X”作为主因。
- 生成RTL的output是六路oldest-onehot的纯组合结果，没有隐藏寄存级。旧monitor的跨拍现象由
  clocking block input skew与边沿后`uvm_hdl_read()`混用造成：两者虽在同一日志时间，实际不是同一
  sampling region。
- 仅把procedural read换成output拍live mirror仍对未来RTL pipeline不稳健。最终方案每拍冻结输入winner，
  output按完整ROB flag/value和level在4-sample有界窗口关联；窗口不是按ROB value单独匹配。
- 同ROB/level重复记录只有provenance完全相同才可选择最新项；winner/reason/RAW owner等任一不同均返回
  conflict。无记录、过期、selected X/Z和多winner继续error，adapter不得把incomplete subtype当有效根因。
- cache只服务诊断，不消费DUT事件，也不延迟、drop或重写redirect。reset/L2TLB reset清空，防止旧epoch
  provenance跨生命周期命中。
- 跨拍directed覆盖age=2合法命中、level错误拒绝、age=5过期拒绝；现有六subtype decode和selected严格
  检查继续保留。

结论：Review 14通过。真实run e说明原winner-first方案遗漏了monitor sampling-region边界；有界
provenance cache补齐该场景且没有降低fatal/error语义。

## Follow-up Review 15：UID14 D-error根因边界

- sample-domain修复后真实UID14仍在sample 740查询到generation 0，因此不能继续声称唯一根因是`$time`
  与sample_seq混用。
- 当前证据只证明DUT产生hardware-error bit19、RM翻译line为`0xb93a1c0`；尚未证明注错GrantData的
  response record也是该line，也未证明该bit19一定来自L2 D corrupt。
- 最小下一步是在独立expected路径记录corrupt/denied reservation和最终D.fire的line/source/generation/
  sample。不得从DUT exception反推expected，不得把任意hardware-error全局sticky。
- directed必须经过prepare reservation再commit，验证同line、同generation、同source进入live ledger；
  仅直接调用commit helper不足以覆盖response reservation owner丢失。

结论：Review 15通过。先用严格provenance诊断闭合真实producer，得到证据后再决定是否需要行为修复。

## Follow-up Review 16：跨拍cache完整身份自审

- 生成RTL再次确认`memoryViolation`没有寄存级，当前错位是clocking-block input skew与procedural读取
  混用；静态mirror让output和六路输入进入同一采样域，有界cache负责允许真实或未来实现中的跨拍偏移。
- 初版payload equality遗漏`rollback_candidate_known/valid`和`rollback_winner_known`。同ROB/level下若
  active candidate集合变化而payload默认值相同，可能被错误视为等价，这是严格排他证明的遗漏。
- equality现包含diag完整性、candidate known/valid、完整candidate ROB/level、winner known/value、
  LoadUnit reason、uncache状态和RAW store owner。任一差异都返回conflict，不按最近项猜测。
- directed新增相同记录重复时选择最新sample，以及同key不同candidate集合必须conflict；原age=2命中、
  level不匹配和age=5过期继续保留。

结论：Review 16通过。cache仍只提供诊断provenance；无匹配、过期、多义或不完整证据继续报错，未降低
checker severity，也未改变DUT rollback/redirect行为。

## Follow-up Review 17：Uncache full的owner/terminal边界

- run i已稳定证明selected input 3和subtype 5，故重复redirect producer已闭合；`uncache_free=0`仍只是
  LoadQueueUncache freeList三路分配视图，不能直接归因于外部memory responder。
- LQ entry在state 2收到redirect时不会提前释放，因为已经发出的sid必须等待真实response以避免source
  被新owner复用；未redirect entry收到response后进入state 3，仍需真实nc/mmio writeback握手才free。
  这两类长期占用都合法地表现为canAllocate全0，但根因和修复完全不同。
- 顶层Uncache另有16个source entry。只有把LQ slot ROB/state/slaveId、top source状态、TL A/D和LSQ
  response放在同一采样域，才能判断framework遗漏、source匹配遗漏或writeback阻塞。仅增加A/D打印而
  不观察LQ state仍会遗漏“D已完成但state 3未写回”的有效场景。
- 新增框架source ledger以A.fire为唯一建立点、D.fire为唯一释放点；source提前复用和无owner D保持
  fatal。乱序source完成合法，directed覆盖两个owner逆序完成和reset清理。该账本不改变response delay、
  reorder、denied/corrupt或D hold行为。
- 诊断日志必须限流；快照变化时立即记录，稳定循环每256个NACK记录一次。这样能观察长期无进展，
  又不会用重复UVM_LOW日志显著拖慢真实seed。
- 当前阶段没有足够证据选择功能修复。任何“full时drop rollback”“redirect后固定N拍再试”或直接释放
  LQ slot都可能造成sid复用和迟到D写入新owner，明确拒绝。下一真实诊断run闭合停滞边后再实施绑定
  terminal progress的最小修改。

结论：Review 17通过诊断方案，不通过任何未经owner证据的行为修改。专项编译和真实run需验证静态镜像
无X/Z、source ledger正向链闭合，并给出UID862时16个slot逐项状态后才能进入最终修复。

## Follow-up Review 18：Uncache lifecycle实现、自审与公平性边界

- 生成RTL确认LQ state 2在redirect后只会于匹配`slaveId`的真实response到达时flush；state 3只会于
  `ncOut/mmioOut`真实writeback或redirect flush时释放。任何在NACK点直接释放slot都会破坏迟到response
  的owner隔离。
- LQ slot index通过Uncache `idResp.mid`取得source `sid`；TL A/D和top source state都以同一个sid关联，
  顶层`io_lsq_resp.id`再广播回所有匹配sid的LQ entry。新增镜像覆盖这三个关联点，没有按ROB value猜owner。
- framework ledger只在A.fire建立owner，只在D.fire释放owner。D hold期间owner保持；跨source乱序完成
  合法，同source复用或无owner D保持fatal。directed覆盖双owner逆序完成和reset清理。
- run i后续已越过UID862，所以当前证据是长时间资源饱和而非已证明的永久泄漏。诊断必须量化source age
  和state 2/3驻留时间，避免把合法的大延迟压力误修成静默drop。
- response scheduler在timer到期时重新随机选择eligible record，理论上缺少单source确定公平界。这是
  review新发现的候选根因，但尚未由UID862实测owner链证明；功能修改必须等新run明确停滞边。
- 完整VCS编译`fault2_uncache_lifecycle_compile_20261015_j`通过，证明所有新增静态XMR层次和SystemVerilog
  ledger语法有效。`git diff --check`在编译前通过。

结论：Review 18通过诊断实现；不批准无证据的行为修改。下一步先跑三项directed，再以原cfg/seed的
低verbosity诊断run获得每个outstanding source的A/D/LSQ/writeback terminal证据。

## Follow-up Review 19：Uncache full诊断的负向契约与低verbosity可观测性

- 首轮ledger directed只覆盖合法双source乱序完成，遗漏了两个关键负向契约：同source在旧D完成前
  复用、以及没有A owner的D完成。两者必须继续fatal，不能因为后续公平性修改而退化成warning。
- 新增directed catcher只按精确message匹配单次预期fatal；未匹配fatal继续抛出。每个负向场景后
  clear responder，避免被catch后的测试副作用污染下一场景。
- 真实run使用`UVM_LOW`时，逐笔A/D ledger的`UVM_HIGH`不可见；仅在full建立/释放时打印mask不足以
  判断同一批owner是否长期不动。新增full watchdog在16 owner持续占用时限流打印所有source的
  address/accept/age及queue/current-D/timer状态，且只读这些字段。
- watchdog的触发条件是framework ledger全占用，不等同于DUT LQ freeMask为0。两套快照并行存在，
  正是为了区分framework response阻塞和DUT response后writeback/flush阻塞。
- 原cfg明确开启`MEMBLOCK_UNCACHE_RSP_REORDER_EN=1`并把0/small/medium/large delay权重设为1/1/1/1。
  当前scheduler在timer到期时重新随机选eligible source，静态上没有公平上界，但在真实owner-age证据
  出现前仍不能把它定为根因。
- 允许的最终修复必须同时满足：选择与具体owner绑定、每个持续eligible owner有有限服务上界、D.valid
  在ready前保持、source只在真实D.fire释放、乱序能力仍存在。固定延迟free、NACK drop和直接篡改
  freeList均不满足这些约束。

结论：Review 19通过诊断增强和负向测试设计。下一步重编译并先跑ledger directed，再跑pending-MMIO、
ROB admission及原cfg/seed真实诊断；功能修复取决于新日志给出的第一条不推进边。

## Follow-up Review 20：真实run诊断X/Z误报与owner证据完整性

### Review 20.1 失败事实

- 新真实run `fault2_uncache_lifecycle_real_1_98_1_20261015_o`自然结束于
  `11440.900ns`，累计`UVM_ERROR=10`、`UVM_FATAL=0`。
- 10条错误均为`UNCACHE_PROGRESS Uncache lifecycle snapshot contains X/Z on an active diagnostic path`。
- 运行命令确认加载了原始cfg绝对路径，`MAIN_TRANS_NUM=10000`、enqueue为`1/98/1`、Uncache delay
  为`1/1/1/1`、reorder开启；原始cfg的`MAIN_MEM_RANGES_EN=0`也实际生效。
- 由于监视器在生成有效snapshot前退出，日志没有`UNCACHE_PROGRESS`资源快照，也没有
  `UNCACHE_RESPONDER` full-owner证据。因此不能把该run解释为DUT资源真实泄漏。

### Review 20.2 根因与修复边界

监视器原逻辑把16个LQ entry、16个top source以及valid为0的A/D payload整体送入
`$isunknown`。未占用entry的state/ROB/slaveId和无效通道payload出现X/Z是合法的“无事务”状态，
不是owner链错误。修复后：valid mask本身仍严格检查；只有活动LQ/top entry读取payload；只有
valid握手读取id/source/ready；source ledger的A.fire->D.fire和负向fatal契约不变。

### Review 20.3 第二轮自审

1. **漏掉空槽屏蔽？** 已对LQ和top source分别按`req_valid`/`top_valid`逐项屏蔽，snapshot比较使用
   屏蔽后的数组，避免inactive X导致重复变化或误报。
2. **漏掉活动字段的X/Z？** 活动LQ的state/ROB/slave/flush/writeback、活动top的三类状态、
   free mask以及valid为1时的idResp/LSQ/A/D payload仍严格报错；没有降低severity。
3. **会不会掩盖真实DUT问题？** 不会。活动owner字段仍是严格检查；只忽略没有valid语义的无事务
   payload。source提前复用、ownerless D以及response容量越界仍保留fatal。
4. **是否已证明永久full？** 尚未。旧run只有rollback producer证据；新run因诊断误报退出，必须
   在修复后的真实run获得LQ/top/framework三层同拍链路后再决定功能修改。

结论：Review 20通过诊断采样修复和证据门槛；不批准任何无owner terminal证据的功能性free、
NACK drop、固定延迟重试或scheduler改动。下一步重编译、专项验证，再以原始cfg/seed自然运行闭合
首条资源不推进边。

## Review 21：Uncache full producer与释放链证据（2026-10-15）

producer已稳定复现UID862/ROB=1/133的input=3、subtype=5 Uncache NACK，
`dynamic_epoch=775..814`、`candidate=winner=0x8`、`uncache_known=0x7`、`uncache_free=0`、
frontier=862。该证据只定位到LQ Uncache分配失败，不能直接定位到framework响应owner泄漏。

`uncache_free`来自LoadQueueUncache freeList的canAllocate结果，不是顶层Uncache source outstanding
mask，也不是framework response queue occupancy。合法状态包括LQ entry尚未发出request、top source
等待D、state=2等待真实response、state=3等待写回和redirect后等待response再flush。

本轮代码保留DUT侧`uncache_top_d_valid`，并由framework responder ledger提供真实D.fire；
无效D payload仍屏蔽，只有真实D.fire才清除framework owner，slot仍只能由writeback/flush释放。
两轮自审确认不改变ready、NACK、freeList、response调度或flush语义，也不引入按epoch猜测owner。

当前批准诊断扩展、owner ledger directed和低verbosity真实重跑；尚未批准行为修复。只有日志证明
某条真实terminal边已完成而下一条映射未推进时，才提交绑定该边的最小修改。

## Follow-up Review 26：UID130 helper 语义专项

- 新增 `run_store_fault_helper_semantics_scenario()`，覆盖普通 store、STA-first/STD-not-dispatched、
  STD-dispatched/未 WB、双 real-WB 完成、redirect pending 和 dynamic epoch 失配边界。
- directed 日志：
  `mem_ut/ver/ut/memblock/sim/fault2_store_fault_epoch_helper_compile_20261015_b/log/`
  `tc=tc_dispatch_fault_smoke_ts=virtual_base_sequence_cfg=default_seed=37617171_rtl_store_fault_helper_semantics_directed_20261015_c.log`。
- 结果：`TEST CASE PASSED`、`UVM_ERROR=0`、`UVM_FATAL=0`、自然 `$finish`。
- `store_fault_real_wb_pending()` 只识别 active/current-epoch pending record；
  `store_fault_blocks_pending_ptr()` 只有 fault 的另一 target 已 dispatch 后才阻塞。
  因此 STA fault 先到而 STD 尚未 dispatch 时不会自锁，但 fault candidate 仍不会提前 terminal。
- redirect 标记或 dynamic epoch 变化会使旧 pending record 对当前实例不可见；没有降低 fatal/error
  severity，也没有伪造 WB 或静默丢弃旧事件。

结论：Review 26通过。UID130 相关 helper 语义和既有 fault token 生命周期专项已闭环；完整随机
`1:98:1` 与 `1:1:1` 仍需在包含该 patch 的新 binary 上验证。
