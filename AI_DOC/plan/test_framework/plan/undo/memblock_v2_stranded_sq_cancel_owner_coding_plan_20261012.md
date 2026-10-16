# V2 Redirect 补记未完成 Admission 的 DUT 可见 SQ Owner 方案

## 1. Plan 定位、专有名词与抽象功能说明

### 1.1 Plan 定位

本 plan 修复 V2 `mem_ut` redirect cancel 软件计数漏掉一类 SQ owner 的问题。该 owner 已经由
`lsq_ctrl_model::commit_allocate()` 完成 allocate/enq，`set_status_field(..., MEMBLOCK_STATUS_ENQ, 1)`
也已经通过 `mark_uid_enqueued()` 推进 admission 连续前缀；问题不在 `complete_admission()`。
真实根因是前一轮 redirect 回退了连续前缀，却保留了一个不被该轮 redirect 覆盖的 `DUT_VISIBLE`
active SQ owner，使该 owner 落到回退后 prefix 之外。下一轮 redirect 覆盖它时，原 prefix scan 已经无法访问。

修复只改变 redirect 中频事件的 owner 收集范围，不改变 DUT cancel 接口、不放宽
`LSQ_CANCEL_RECONCILE` 对账、不改变 `max_enqueued_uid` 的 admission 顺序职责，也不在每拍主循环扫描
完整 UID 状态表。

### 1.2 专有名词与抽象功能说明

| 术语 | 当前文档中的中文含义 | 代码对象或落点 | 使用场景/示例 |
| --- | --- | --- | --- |
| `admission prefix` | 从 UID 0 开始连续完成 admission 的软件前缀。 | `dispatch_progress.max_enqueued_uid` | UID 4116 已被 DUT 采样，但前缀仍停在 4115。 |
| `reservation` | 测试框架对一次 LQ/SQ 分配动态实例的记录。 | `lsq_reservation_state`、`lsq_reservation_sample_seq` | launch 后先是 `LAUNCHED_PENDING_SAMPLE`，下一采样边界成为 `DUT_VISIBLE`。 |
| `DUT_VISIBLE` | 该 reservation 已跨过 DUT 采样边界，物理 LSQ owner 已对 DUT 生效。 | `MEMBLOCK_LSQ_RESERVATION_DUT_VISIBLE` | redirect 时 DUT 会把该 SQ slot 纳入 cancel。 |
| `active SQ owner map` | 用完整 SQ flag/value 反查当前动态实例 UID 的物理 owner 表。 | `uid_by_sq` | `{flag=1,value=35} -> uid=4116`。 |
| `stranded owner` | 前一轮 redirect 回退 prefix 后，仍是 `active && enq && active_sq_mapped`、已经 `DUT_VISIBLE`，但落在当前 prefix 外的 SQ owner。 | 本 plan 新增的 redirect 快照候选 | record 3895 保留 UID 4116，record 3896 覆盖它但原 scan 扫不到。 |
| `dynamic epoch` | 同一 UID 每次 redirect reissue 的动态版本号。 | `status.dynamic_epoch` | 快照与处理时 epoch 不同则候选已失效。 |
| `cancel accounted epoch` | 某 UID 最近一次已计入 cancel 的 redirect epoch。 | `status.lsq_cancel_accounted_epoch` | 连续 redirect 中防止同一动态实例重复记账。 |
| `full SQ key` | 同时包含 wrap flag 与 value 的 SQ 物理位置。 | `memblock_sq_key_t` | 不能只比较 value，否则 wrap 后会误认 owner。 |
| `snapshot` | 在任何 redirect 删除动作之前冻结的有限 active owner 集合。 | 新增 `memblock_active_sq_owner_snapshot_t` queue | 后续删除 `uid_by_sq` 时不再迭代原关联数组。 |

抽象功能说明：`apply_redirect_flush_range()` 仍是 redirect 状态清理和 cancel 软件计数的唯一入口。
它先冻结当前 active SQ owner，再执行原 admission prefix scan，最后只补处理 prefix 外、仍可由快照严格
证明属于同一动态实例的 stranded owner。所有 owner 仍通过 `prepare_uid_for_redirect_reissue()` 完成
cancel 记账、map 删除和 reissue 状态转换。

## 2. 问题与目标行为

### 2.1 修改前行为

`lsq_ctrl_model::commit_allocate()` 先调用 `activate_uid()` 建立 `uid_by_sq`，随后调用
`set_status_field(uid, MEMBLOCK_STATUS_ENQ, 1)`；该 setter 立即调用 `mark_uid_enqueued()`，所以 UID 4116
在成为 `DUT_VISIBLE` 前后都已经进入 `max_enqueued_uid` 表示的连续前缀。`complete_admission()` 只负责
开放 issue route，不推进该前缀。

真实失败由连续两轮 redirect 形成：

```text
uid=4116 已完成 commit_allocate/enq，进入 uid_by_sq，reservation=DUT_VISIBLE
  -> record 3895 redirect ROB=1/75 回退 admission prefix
  -> uid4116 ROB=0/76 不被 record 3895 覆盖，因此保持 active/mapped/DUT_VISIBLE
  -> uid4116 落到回退后的 admission prefix 外
  -> record 3896 redirect ROB=1/76 覆盖 uid4116 ROB=0/76
  -> apply_redirect_flush_range() 仍只扫描当前 prefix，访问不到 uid4116
  -> DUT 扫描真实物理 SQ slot 并统计 uid4116，软件漏记 1
```

失败 seed `676087581` 的 record 3896 因而形成：

```text
8 ordinary + 1 fault + 1 stranded ordinary = DUT 10
软件只得到 8 ordinary + 1 fault = 9
```

### 2.2 目标行为

redirect 软件 owner 集合由两部分组成：

1. 原 admission prefix 活跃窗口内的 UID；
2. redirect 开始删除 owner 前，`uid_by_sq` 中 prefix 外的 `DUT_VISIBLE` owner 快照。

第二部分只有同时满足以下条件才进入普通 cancel/reissue：

- 快照 UID 合法，处理时仍 `active && enq && active_sq_mapped`；
- 主 transaction 是 V2 scalar 普通 SQ allocation，`numLsElem == 1`，且不是 control marker；
- `dynamic_epoch` 与快照一致；
- status 中完整 SQ key 与快照一致；
- `uid_by_sq[full_key]` 仍映射到该 UID；
- reservation state 为 `DUT_VISIBLE` 且 sample valid；
- reservation sample 不晚于本 redirect 的 LSQ cutoff；
- 完整 ROB key 与快照一致，并被本 redirect 覆盖；
- `lsq_cancel_accounted_epoch` 不等于当前 redirect epoch；
- UID 不在原 prefix scan 范围内。

若 prefix 外 owner 仍 active/mapped，但带有 control、`rob_commit`、`terminal_done`、fault 或 exception
语义，必须输出完整归因并 `uvm_fatal`，不得作为 ordinary owner 计数。fault/exception 范围包括
`fault`、`load_fault`、`sta_fault`、`std_fault`、`exception_pending`。若它已经记录当前
`lsq_cancel_accounted_epoch` 却仍 active/mapped，也必须 fatal，不能静默跳过。未被本轮 redirect 覆盖、
尚未在 cutoff 前成为 `DUT_VISIBLE`、或 reservation sample 晚于 cutoff 的合法 owner保持 active且不计数。

满足后调用既有 `prepare_uid_for_redirect_reissue()`。该函数继续统一完成
`capture_redirect_deleted_owner()`、`note_lsq_cancel_for_uid()`、active map 删除、状态清理和
`dynamic_epoch` 推进。无法证明的 owner 不增加软件 cancel 数；结构不一致则 fatal，未被 redirect 覆盖则
保持 active。

## 3. 数据结构方案

新增只在 `apply_redirect_flush_range()` 单次调用期间使用的值类型：

```systemverilog
typedef struct {
    bit                valid;
    memblock_uid_t     uid;
    int unsigned       dynamic_epoch;
    memblock_rob_key_t rob_key;
    memblock_sq_key_t  sq_key;
} memblock_active_sq_owner_snapshot_t;
```

该类型不进入 `common_data_transaction` 长期状态，不需要 reset、清理或 end-test 独立审计。完整 SQ key
解决 wrap value 重用；dynamic epoch 与 ROB key 解决同 UID reissue 或 ROB wrap 后的实例混淆。

不采用显式“DUT-visible 但未 complete admission”长期侧表，原因是 `uid_by_sq` 已经是物理 owner 的
权威 active map。新增侧表会重复维护 owner 生命周期，并要求在 deq、redirect、reset、reissue、end-test
增加第二套一致性逻辑。

## 4. 主流程实现 Flow

### 4.1 `snapshot_dut_visible_sq_owners()`

抽象功能描述：该 helper 在 redirect 删除任何 owner 前，从有限的 `uid_by_sq` active map 冻结
active SQ 动态实例身份。它只读公共状态，不修改 cancel count、map、status 或 admission prefix。

函数目的：避免 `apply_redirect_flush_range()` 后续调用 `retire_active_uid()` 删除 `uid_by_sq` 时，一边迭代
同一关联数组一边删除；同时为 prefix 外 owner 保存完整 SQ key、ROB key 和 dynamic epoch 证明。

输入：当前 `uid_by_sq`、`status_by_uid`。

输出/副作用：清空并填充调用者传入的 snapshot queue；无公共状态副作用。

源码级伪代码：

```text
snapshot queue = empty
foreach uid_by_sq[map_key]:
    uid = uid_by_sq[map_key]
    校验 uid/status/main transaction 合法
    校验 status active、enq、active_sq_mapped，任一不成立均 fatal
    从 status 取得 full sq_key，并校验其 map_key 等于当前 map_key
    保存 uid、dynamic_epoch、full rob_key、full sq_key
```

中文文字伪代码：先清空本次局部快照，然后只遍历最大为 `MEMBLOCK_SQ_SIZE` 的 active SQ map。每个 map
元素必须能回到一个 `active && enq && active_sq_mapped` 的 status 和非空 main transaction，status 中完整
SQ key 必须与关联数组 key 一致，否则说明权威 owner 表已经损坏并立即 fatal。快照保存所有 active SQ owner
的 UID、动态版本、ROB 身份和完整 SQ 身份；reservation/cutoff、ordinary/fault 分类和 redirect 覆盖判断
统一留到原 prefix scan 完成后的补充处理阶段，避免在快照阶段静默丢失异常 owner。

### 4.2 `redirect_sq_owner_snapshot_matches()`

抽象功能描述：该 helper 在原 prefix scan 之后验证一条快照是否仍代表同一个 active SQ 动态实例，并判断
它是否属于本次 redirect 的 prefix 外补充 owner。它只返回布尔值，不修改公共状态。

函数目的：把 dynamic epoch、完整 key、map 一致性、DUT 可见性、redirect cutoff 和 already-accounted
条件集中到一个入口，避免主流程分散判断。

输入：snapshot、redirect、redirect epoch、redirect LSQ cutoff、原 prefix `[begin_uid,end_uid)`。

输出/副作用：匹配返回 1；不匹配且属于正常“不覆盖/已由 prefix 处理”返回 0；权威 map/status 自相矛盾
时 fatal。

源码级伪代码：

```text
若 snapshot 无效或 uid 位于原 prefix：return 0
读取 status
若 status 已非 active 或 SQ mapping 已删除：return 0（表示原 prefix 已消费）
若 dynamic_epoch 已变化：fatal
校验 status full SQ key、ROB key 与 snapshot 一致
校验 uid_by_sq 仍存在且指向该 uid
校验 status.enq；否则 fatal
校验 main transaction 为非 control 的 scalar SQ allocation，numLsElem==1；否则 fatal
若 terminal_done、rob_commit、fault 或 exception 任一成立：fatal
若已经按当前 redirect epoch 记账且仍 active/mapped：fatal
若 ROB 不被 redirect 覆盖：return 0
若 reservation 非 DUT_VISIBLE 或 sample 无效：return 0
若 sample 晚于 redirect LSQ cutoff：return 0
return 1
```

中文文字伪代码：先排除原 prefix 范围，保证 fault head、已完成 admission 的普通 owner 和 LQ/SQ 双资源
UID 只由原循环处理一次。原循环可能已经删除某些 map，因此 active 或 mapping 已消失表示快照已被 prefix
消费；除此之外，dynamic epoch、完整 ROB/SQ key、map owner、`enq` 任一不一致都是生命周期损坏并 fatal。
仍 active/mapped 的 prefix 外 owner 必须是 `numLsElem==1` 的普通 scalar SQ allocation；control、commit、
terminal、fault、exception 或“当前 epoch 已 accounted 但 owner 仍存在”全部 fatal。完成结构和分类检查后，
ROB 不被本轮 redirect 覆盖属于合法 survivor并返回 0；reservation 尚未在 cutoff 前 `DUT_VISIBLE` 也不属于
本轮 DUT existing-owner cancel，保持 active并返回 0；只有全部证明成立时返回 1。

### 4.3 修改 `apply_redirect_flush_range()`

抽象功能描述：该函数在已锚定的 redirect LSQ 边界执行唯一一次 active owner scan，生成软件 cancel
计数并把被覆盖动态实例转入 reissue。修改后它合并 admission prefix 与物理 SQ owner 快照，不承担 DUT
snapshot 对账或 fault SQ delta 仲裁。

输入：当前 redirect payload、active cancel record、公共 active map/status。

输出/副作用：更新 cancel record 软件 LQ/SQ count、删除被覆盖 owner、回退 admission prefix、启动
redirect-deleted owner window，并结束 active cancel scan。

源码级伪代码：

```text
校验 redirect record 已锚定且到达 LSQ cutoff
snapshot_dut_visible_sq_owners(stranded_sq_snapshots)
计算 begin_uid/end_uid
按原逻辑扫描 prefix：
    fault head 走 preserve
    terminal/inactive 清 issue queue
    被 redirect 覆盖的普通 uid 走 prepare_uid_for_redirect_reissue
遍历 stranded_sq_snapshots：
    若 redirect_sq_owner_snapshot_matches 返回 0：continue
    更新 oldest_flushed_uid
    打印通用 stranded owner 归因日志
    prepare_uid_for_redirect_reissue(snapshot.uid, redirect)
如有 flush owner，按最老 UID rollback admission prefix
按原逻辑 finalize count/window/record
```

中文文字伪代码：在任何 owner 删除前取得有限 SQ 快照。原 prefix 循环完全保留，因此 completed admission、
fault head、LQ/SQ 双资源和既有 redirect-deleted owner 逻辑不变。原循环结束后，只检查快照中 prefix 外且
仍可证明为同一动态实例的 owner；每个 UID 只调用一次 `prepare_uid_for_redirect_reissue()`，该函数内部的
`note_lsq_cancel_for_uid()` 根据当前 LQ/SQ mapping 分别累计资源数，并通过
`lsq_cancel_accounted_epoch` 防重。最后仍由同一个 `oldest_flushed_uid` 回退 admission 水位；当只有
stranded batch 时，最老 UID 应等于旧 `max_enqueued_uid+1`，回退结果保持原前缀不变。

## 5. 诊断整理

保留现有失败时 `dump_sq_cancel_reconcile_diag()` 和有界 provenance ring，但做以下整理：

- 删除 UID 3800..4000 和 record 3880 的硬编码门槛；
- 不在正常 owner add/remove/launch/sample/count 路径持续记录 seed 专用历史；
- 新增一条仅在真正补处理 stranded owner 时产生的通用 `LSQ_CANCEL` 低等级日志，包含 record、UID、
  dynamic epoch、ROB、SQ、sample/cutoff 和 `numLsElem`；
- `dump_sq_cancel_reconcile_diag()` 只在 reconcile 即将 fatal 时执行；HDL 深层诊断路径无法定位时不得额外
  产生 UVM error。现有会调用 `uvm_hdl_read()` 访问不稳定内部层次的逐 slot dump 从正式路径移除，保留
  框架状态、fault resource、cancel record 和 redirect-deleted owner 的纯软件有界诊断。

## 6. 正确性与边界评审清单

| 检查项 | 方案结论 |
| --- | --- |
| 迭代 active map 时删除安全 | 先复制值类型 queue，再运行原删除流程；不边迭代边删除 `uid_by_sq`。 |
| V2 scalar SQ 边界 | 补扫只接受普通 scalar SQ allocation，要求 `numLsElem==1` 且 SQ mapping 唯一；不把 LQ/SQ 双资源当作本路径合法状态。 |
| `numLsElem` | matcher 先强检查 `numLsElem==1`，后续仍复用 `note_lsq_cancel_for_uid()` 的同一强检查，不放宽。 |
| fault 与 ordinary 边界 | prefix 内 fault head仍由 `preserve_fault_uid_during_redirect()` 处理；prefix 外出现 control/commit/terminal/fault/exception owner立即 fatal，不进入 ordinary count。 |
| 连续 redirect | 每个 active redirect 建局部快照；旧 owner window 未清时既有逻辑禁止新 redirect；accounted epoch 防重复。 |
| ROB/SQ wrap | 快照同时保存 ROB flag/value 和 SQ flag/value，禁止 value-only 匹配。 |
| dynamic epoch 重用 | 快照与处理时 dynamic epoch 必须相同。 |
| pending sample 竞态 | 只有 `DUT_VISIBLE && sample_valid && sample<=LSQ cutoff` 才计数；未到 cutoff 的合法 owner保持 active，不猜测。 |
| 已记账但 owner 仍 active | `lsq_cancel_accounted_epoch==current_epoch` 且仍 active/mapped 是生命周期错误，必须 fatal，不能静默返回。 |
| owner 未被 redirect 覆盖 | 返回 0，不计数、不删除 map。 |
| end-test 残留 | 被覆盖 stranded owner走既有 reissue；不新增长期状态。未覆盖 owner保持 active，由后续正常流程负责。 |
| 性能 | 仅 redirect 中频事件遍历 `uid_by_sq`，最坏 `MEMBLOCK_SQ_SIZE`，不扫描 `main_trans_num`。 |

评审结论必须在 coding 前确认：不得简单接受 `ordinary+2`，不得把 `max_enqueued_uid` 改成 active owner
全表水位，不得把补扫放到每拍 service loop，不得根据 UID 数值或 record 数值特判。

## 7. 测试方案

### 7.1 `basicTest + 专项 vseq` 文件与接入

本 plan 不新增 testcase。专项软件定向场景使用固定 `basicTest` 和新 vseq：

| 文件 | class | 修改内容 |
| --- | --- | --- |
| `mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_stranded_sq_cancel_sequence.sv` | `soft_test_memblock_stranded_sq_cancel_sequence extends soft_test_memblock_dispatch_smoke_sequence` | 新增 software-only 两轮 redirect 场景；只在该 class 内放置人工建表、monitor 边界构造、expected-fatal catcher 和状态断言 helper。 |
| `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_stranded_sq_cancel_directed_vseq.sv` | `memblock_stranded_sq_cancel_directed_vseq extends virtual_base_sequence` | 调用 `require_virtual_sqr()`，再用 `uvm_do_on(directed_seq, p_sequencer)` 启动专项 soft sequence。 |
| `mem_ut/ver/ut/memblock/seq/seq_pkg.sv` | `seq_pkg` | 在父类定义之后 include 新 soft sequence；在 `virtual_base_sequence` 之后 include 新 vseq。 |
| `mem_ut/ver/ut/memblock/seq/seq.f` | filelist | 现有 `+incdir+./base_seq/soft_test` 与 `+incdir+./virtual_sequence` 已覆盖新文件；coding 时复核，不新增重复 filelist entry。 |
| `mem_ut/ver/ut/memblock/cfg/tb.f` | 顶层 filelist | 复核 `-F ../seq/seq.f` 仍位于 testcase package 之前；无顺序变化则不修改。 |
| `mem_ut/ver/ut/memblock/tc/src/basicTest.sv` | `basicTest` | 复用通用 `+VSEQ_MAIN` factory 创建/启动逻辑；专项 vseq 不需要 L2TLB/control topology，不修改 testcase 白名单。 |

运行入口固定为：

```text
make eda_run tc=basicTest ts=memblock_stranded_sq_cancel_directed_vseq mode=base_fun
```

### 7.2 专项 vseq 伪代码

抽象功能描述：`memblock_stranded_sq_cancel_directed_vseq::body()` 是 `basicTest` 选择的场景入口，只负责在
virtual sequencer 上调度 software-only directed sequence；它不直接修改公共 status/map，也不启动无关 agent。

源码级伪代码：

```text
body:
    require_virtual_sqr()
    创建 soft_test_memblock_stranded_sq_cancel_sequence
    uvm_do_on(directed_seq, p_sequencer)
```

中文文字伪代码：先确认 `p_sequencer` 有效，再通过标准 UVM 宏把专项 sequence 启动到同一个 virtual
sequencer。场景的建表、两轮 redirect、断言和 expected-fatal 捕获全部由 soft sequence 自己完成，vseq 不
缓存 agent sequencer、不使用 `uvm_top.find()`，也不新增 testcase/default-sequence 配置。

### 7.3 真实两轮 redirect 主场景

抽象功能描述：`soft_test_memblock_stranded_sq_cancel_sequence::run_two_redirect_stranded_owner_scenario()`
通过公开 allocate/enq/reservation/redirect API 构造与 record 3895/3896 同构的两轮 redirect。第一轮必须自然
回退 admission prefix 并保留未覆盖 owner；第二轮再覆盖该 owner，验证补扫只计一次。

源码级伪代码：

```text
reset data/LSQ runtime owner
建立至少两个 scalar store：
    uid0 ROB=1/76，作为第一轮被覆盖 owner
    uid1 ROB=0/76，作为第一轮 survivor/第二轮 stranded owner
依次调用 lsq_ctrl.commit_allocate(uid0/uid1)：
    证明 max_enqueued_uid 已推进到 uid1
    证明两者 active && enq && active_sq_mapped
为两者建立 reservation launch，并在合法 sample 边界调用 mark_lsq_reservation_sampled()

first_redirect = flushAfter ROB=1/75
通过 request_redirect_flush() 和生产等价的 anchor/cutoff 前置 helper执行第一轮 scan
断言：
    uid0 被覆盖并触发 rollback
    uid1 ROB=0/76 不被覆盖，仍 active/mapped/DUT_VISIBLE
    max_enqueued_uid 已回退到 uid1 之前，uid1 位于 prefix 外
完成第一轮 cancel record apply/window 清理，保留 uid1 active owner

second_redirect = flushAfter ROB=1/76
通过相同生产等价前置 helper执行第二轮 scan
断言：
    原 prefix scan 不包含 uid1
    prefix 外补扫选择 uid1
    第二轮 software_cancel_sq_count 精确增加 1
    uid1 只调用一次 prepare_uid_for_redirect_reissue()
    uid1 active SQ map 被删除，dynamic_epoch 只增加一次
```

中文文字伪代码：场景不能直接写 `max_enqueued_uid`。两个 store 必须先经过 `commit_allocate()`，证明 prefix
确实已经推进；第一轮 redirect `1/75` 覆盖 `uid0 ROB=1/76`，因此由正式 rollback 逻辑把 prefix 回退，
同时 `uid1 ROB=0/76` 按 wrap 顺序不被覆盖而继续持有物理 SQ owner。第一轮 cancel record 和
redirect-deleted owner window 必须正常闭环后才能发起第二轮。第二轮 redirect `1/76` 覆盖 `uid1 ROB=0/76`，
此时原 prefix scan 已为空或止于 uid1 之前，只有新补扫能够找到 uid1；测试直接检查本轮 software SQ count、
map 删除和 dynamic epoch，不能靠放宽 reconcile delta 通过。

### 7.4 负向与边界场景

同一 soft sequence 继续覆盖以下场景，每个场景独立 reset owner/runtime：

1. prefix 外 owner不被第二轮 redirect 覆盖：count不变，active mapping保持。
2. 普通 owner仍在 prefix 内：只由原 scan处理，补扫不重复计数。
3. 相同 SQ value、不同 flag：只有完整 SQ key匹配的 owner可被选择。
4. `dynamic_epoch`、ROB key、SQ key或 `uid_by_sq` owner不一致：捕获指定 ID/message 的 expected fatal。
5. prefix 外 owner分别注入 control、`rob_commit`、`terminal_done`、fault、exception：每种均必须 fatal，不能进入 ordinary count。
6. `lsq_cancel_accounted_epoch==current_epoch` 且 owner仍 active/mapped：必须 fatal，不能静默跳过。
7. reservation尚未 `DUT_VISIBLE` 或 sample晚于 cutoff：不计入本轮 cancel，owner保持 active。
8. fault SQ candidate 与 stranded ordinary owner并存：ordinary owner由补扫计数，fault仍由
   `resolve_fault_sq_candidate_from_snapshot()` 独立归因，保持 `ordinary + fault` 职责分离。
9. 没有 stranded owner：原 cancel 行为和计数不变。

software-only test 只用 production-shaped record/anchor/cutoff 前置条件触发 owner scan，不伪造“更宽松的 DUT
期望值”来让 reconcile 通过；真实 DUT cancel 数仍由失败 seed专项仿真验收。

### 7.5 编译与真实仿真

先执行 `source <worktree>/.envrc`，确认 `VCS_BIN`、license 和项目变量，再通过项目正式 EDA 入口执行：

```text
git diff --check
make eda_compile tc=basicTest ts=virtual_base_sequence mode=base_fun
make eda_run tc=basicTest ts=memblock_stranded_sq_cancel_directed_vseq mode=base_fun
make eda_run tc=basicTest ts=memblock_dispatch_real_smoke_vseq mode=base_fun \
  cfg=tc_l2tlb_fault_2pct_no_trigger seed=676087581 note=fault3_fix
make eda_run tc=basicTest ts=memblock_dispatch_real_smoke_vseq mode=base_fun \
  cfg=tc_dispatch_real_smoke seed=666666 note=fault3_normal
```

真实失败 seed 验收：record 3895 后 UID 4116 保持 active/mapped 并位于回退 prefix 外；record 3896 的通用
stranded owner归因指向 UID 4116，software ordinary SQ count从 8 补为 9，fault candidate仍独立贡献 1，
DUT 10 与 framework 10 对齐，不再出现该 record 的 `LSQ_CANCEL_RECONCILE` fatal。最终要求
`UVM_ERROR=0`、`UVM_FATAL=0`、`TEST CASE PASSED`；若先遇到问题1/2的独立 fatal，必须记录 record 3896
是否已经闭环及完整阻塞日志，不能宣称整条 seed 已通过。

## 8. 文档与归档

coding 和验证后：

- 生成 `AI_DOC/plan/test_framework/review_doc/undo/` 下的中文 implementation review；
- review 覆盖全部源码 diff、plan 对齐、诊断整理和测试结果；
- 更新问题状态分析文档中问题 3 的状态、根因、实现与验证日志；
- coding、文档和必要验证闭环后，把本 plan 从 `undo` 移到 `do`；
- 不提交 git commit，不纳入 `sim/.eda_remote/*.pid`。

## 9. 与初步 plan 差异说明

初步方向提出“从 active LQ/SQ owner map 快照后与 admission prefix 合并”。正式方案把本问题的最小 owner
来源限定为 `uid_by_sq`，因为失败差值来自 DUT 物理 SQ cancel；LQ 仍由原 prefix 逻辑处理，不新增无证据
的 LQ 补扫。独立 review 后纠正根因：`commit_allocate()` 已经通过 ENQ setter 推进 prefix，stranded owner
来自前一轮 redirect 回退 prefix 后保留未覆盖 owner，而不是 `complete_admission()` 被跳过。

正式方案新增两个 helper：`snapshot_dut_visible_sq_owners()` 只负责生成有限值快照；
`redirect_sq_owner_snapshot_matches()` 只负责验证快照与当前动态实例、完整 key、cutoff 和 redirect 覆盖。
主函数继续复用 `prepare_uid_for_redirect_reissue()`，不复制 cancel 计数与 owner 删除逻辑。独立 review 后还
补齐普通/fault 边界：prefix 外只接受 `active && enq && active_sq_mapped` 的 scalar ordinary SQ owner；
control/commit/terminal/fault/exception 和当前 epoch 已 accounted 但仍 active/mapped 都必须 fatal。

正式方案还明确移除 seed 专用 UID/record 日志门槛，并禁止失败诊断通过不稳定 RTL 内部层次产生额外
`UVM_ERROR`；保留纯软件状态的有界失败诊断和成功补记时的一条通用归因日志。定向测试改为固定
`basicTest + memblock_stranded_sq_cancel_directed_vseq`，通过两次正式 redirect scan自然制造 prefix 回退，
不再新增 testcase，也不再手工把 `max_enqueued_uid` 停在目标 UID 之前。
