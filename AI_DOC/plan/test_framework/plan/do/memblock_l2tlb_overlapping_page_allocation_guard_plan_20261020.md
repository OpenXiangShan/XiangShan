# MemBlock L2TLB 大小页重叠分配保护方案

状态：已完成，独立实现review、专项复测和原失败seed 10000笔复验通过

创建日期：2026-10-20

完成日期：2026-10-21

适用版本：V2 `mem_ut_uvm_v2`

## 1. 方案定位、专有名词与抽象功能说明

### 1.1 方案定位

本方案修改memblock测试框架的L2TLB响应表项生成、索引和失效生命周期，保证同一个DUT查询上下文
中任意请求最多命中一个canonical live entry。方案不修改DUT RTL/Scala，不给DUT增加大小页优先级，
也不在RM中模拟不存在的优先选择行为。

本问题属于测试框架激励构造错误：测试框架先创建小页，后续lookup miss又创建覆盖它的大页，导致
DUT的`hitVecReg`出现多个bit同时为1。DUT用`Mux1H`选择PPN、PBMT和权限，多命中时多个payload
会按位OR，输出不再等于任一合法表项。

本方案属于L2TLB miss build运行期路径。正常exact/range hit不访问新增父区域索引；只有真正需要创建
新canonical entry时才执行固定数量的关联数组查询，不允许每次miss扫描完整live表。

### 1.2 专有名词

| 名词 | 当前含义 | 代码或状态落点 | 示例 |
|---|---|---|---|
| canonical live entry | 当前对DUT查询可见的测试框架TLB权威表项 | `tlb_entry_by_key` | 一个2MB响应完成后继续供同范围请求复用 |
| anchor key | canonical entry创建请求对应的`VPN/ASID/VMID/s2xlate`键 | `memblock_tlb_lookup_key_t` | VPN B范围命中VPN A创建的2MB entry时，anchor仍是VPN A |
| range index | 从请求地址查找“已有大页是否覆盖当前请求”的索引 | `tlb_anchor_keys_by_range_key` | 1GB entry按清零VPN低18位后的key注册 |
| parent occupancy index | 从候选大页区域查找“该区域是否已有小页”的反向占用索引 | 本方案新增count和owner集合 | 1GB候选查询其区域中是否已有4KB/64KB/2MB child |
| coverage shape | DUT raw hit使用的实际覆盖范围，包含4KB、64KB NAPOT、2MB、1GB、512GB | 复用`memblock_tlb_range_coverage_rank_e` | 64KB和4KB都是level 0，但coverage shape不同 |
| parent region | 某个entry所属的更大coverage区域 | parent occupancy key | 4KB entry同时属于64KB、2MB、1GB、512GB父区域 |
| context scope | parent occupancy key的ASID/global查询范围 | 本方案新增枚举 | non-global候选查询当前ASID桶和global桶 |
| occupancy count | 指定父区域和上下文中已有child owner数量 | 本方案新增关联数组 | count大于0时对应大页shape不能创建 |
| owner set | parent key与canonical anchor的扁平所有权集合 | 本方案新增关联数组 | 用于精确反注册和失败诊断，不参与正常线性扫描 |
| allowed shape mask | 本次miss build允许生成的coverage shape集合 | 本方案新增5bit mask | 当前1GB区域已有child时屏蔽1GB和可能更大的shape |
| effective lookup shape | allStage中由S1/S2 level和PTE.N共同形成的最终DUT命中范围 | `memblock_tlb_entry::derive_allstage_lookup_shape()` | `min(s1_level,s2_level)`及NAPOT条件共同决定范围 |
| provenance | 历史response与UID issue时刻的token、sample、generation归属 | completed cache和UID snapshot | 历史多候选必须按issue-time provenance处理，不能套用live唯一性规则 |
| atomic publication | 所有key先完成预检查，再无时间间隔地发布canonical和索引状态 | 新增统一插表helper | 不允许canonical已存在但parent index尚未注册的半状态 |
| prepared entry | 已冻结stage/CSR/PTE/fault但尚未选择level和PPN的临时entry | builder prepare/finalize接口 | common data读取其context生成allowed mask |
| shape bucket | 多个raw level组合收敛后的同一最终level/N/shape候选 | builder finalize局部集合 | 无fault NAPOT使不同raw level归并到同一bucket |
| N override | 原候选全被屏蔽时，选择前显式清除一个或多个active stage PTE.N | builder fallback统计 | allStage优先仅清S2 N，再尝试S1 N |
| issue boundary | UID进入WAITING时冻结的sample上界 | `uid_wait_start_sample_seq` | completed response必须不晚于该sample才可被capture |
| test-only injection | soft test为覆盖防御分支而故意绕过生产publish的非法状态注入 | range lookup directed test本地helper | 注入两个重叠live entry并捕获预期fatal |

### 1.3 关键函数抽象职责

`build_tlb_parent_occupancy_keys()`根据entry最终raw-hit coverage和上下文生成其拥有的全部父区域key；
它只计算key，不修改live表和索引。

`derive_allowed_tlb_shape_mask()`在exact/range miss后，通过固定数量的occupancy count查询生成合法shape
集合；它不扫描owner，不创建entry，也不改变已有状态。

`prepare_tlb_payload_context()`在miss build第一阶段创建entry并冻结stage、CSR、PTE profile、PTE字段和
effective fault；它不选择level、不生成PPN，也不访问`common_data_transaction`持有的live索引。

`derive_allowed_tlb_shape_mask()`由`common_data_transaction`在builder两个阶段之间读取parent occupancy，
生成合法shape集合；它不修改entry，也不依赖尚未创建的PPN。

`finalize_tlb_payload_shape_and_payload()`消费allowed mask，按现有builder规则选择并收敛最终level/N，
再生成tag、PPN、sector、权限和PBMT；fault候选保留所选raw level/N，不套用无fault NAPOT fixup。

`memblock_tlb_entry::derive_allstage_lookup_shape()`是移动到builder之前定义的纯shape方法；builder、range
index和raw matcher共同调用它，避免三处维护不同allStage公式。

`publish_live_tlb_entry()`在所有key、容量和重叠条件预检查通过后，统一插入canonical表、range index和
parent occupancy index；它是新entry对其他请求可见的唯一发布入口。

`unregister_tlb_parent_occupancy_index()`根据entry保存的parent key精确递减count并删除owner，不重新
推导shape，不扫描bucket或完整live表。

`validate_no_live_tlb_overlap()`在发布前对最终shape执行固定索引复核，保证没有遗漏global、NAPOT和
allStage组合；它是激励合法性检查，不承担DUT结果正确性判断。

## 2. 问题场景与设计原则

## 执行中补充/修正（IMPLEMENTATION_DELTA）

### [IMPLEMENTATION_DELTA] occupancy key必须区分实际owner shape与登记区域

来源：实现review和索引算术复核发现，entry从自身shape登记到所有更大区域后，候选若直接查询自身到
512GB的区域后缀，会把同一大区域内并不重叠的小页误判为覆盖关系。例如已有4KB entry位于候选2MB的
相邻2MB区域，两者只共享1GB/512GB祖先区域，旧后缀查询仍会错误屏蔽候选2MB；普通4KB候选也可能被
另一个同512GB但不同VPN的4KB owner误阻塞。

原plan及上一轮调整：parent key只记录`parent_region`，上一轮为覆盖跨ASID global同尺寸/更大owner，
让entry登记自身及全部更大shape，并让候选查询shape后缀。该做法无法从聚合count区分bucket中的owner
实际是小页还是覆盖当前地址的大页。

实现调整：`memblock_tlb_parent_occupancy_key_t`新增`owner_shape`。entry仍从自身shape登记到全部更大
`parent_region`，但每个key同时保存实际raw-hit shape。候选shape C的固定查询拆为：

1. 对所有`owner_shape <= C`查询`parent_region=C`，判断候选区域内部是否已有更小或同尺寸owner。
2. 对所有`owner_shape > C`查询`parent_region=owner_shape`，判断当前地址是否被真实更大owner覆盖。

publish前复核使用同一查询helper。新增负向专项验证同一64KB/1GB/512GB祖先区域内的两个4KB，以及相邻
2MB区域不会互相误阻塞；包含已有4KB的1GB候选仍必须被屏蔽。global/non-global/ASID/VMID scope规则不变。

原因：重叠关系必须同时包含“owner实际范围”和“候选区域”，只保存归一化父区域会丢失owner尺寸信息。
新增字段后最多仍查询5个owner shape乘固定1到2个context key，不扫描live表或owner集合。

影响范围：`memblock_dispatch_types.sv`的parent key、`common_data_transaction.sv`的注册/查询/最终复核、
range lookup directed专项及implementation review；需要重新编译、专项复测和10000笔原seed复验。

### [IMPLEMENTATION_DELTA] 跨ASID global随机候选需要context fallback

来源：实现review发现，ASID A已有non-global entry时，ASID B同范围miss所准备的entry若随机到
`s1_pte_g=1`，global候选按`ANY_ASID`正确看见ASID A owner，可能连4KB shape都被屏蔽。原fallback只改
level/N，不改变context scope，因此无法恢复；这不是已有owner非法，而是尚未发布候选的随机G位扩大了
匹配上下文。

原plan：prepared entry冻结PTE字段后直接调用`derive_allowed_tlb_shape_mask()`，不允许occupancy阶段修改entry。

实现调整：新增`derive_allowed_tlb_shape_mask_with_global_fallback()`。先按原G位计算mask；仅当4KB也被屏蔽、
S1 stage active且prepared entry的G为1时，在detached prepared entry上清G并按当前ASID重新计算mask。
若清G后4KB仍被屏蔽，说明存在当前ASID owner或已有global owner，后续仍由原builder/final publish检查fatal，
不得掩盖真实冲突。已发布canonical entry、range index和owner索引均不修改。

原因：G位是本次随机payload的一部分，candidate尚未publish时可做受限合法化；已有owner才是live真源，不能
为了迁就候选删除或改写。该fallback把跨ASID non-global owner场景恢复为non-global候选，同时保留同ASID和
已有global的冲突保护。

影响范围：`common_data_transaction.sv`新增G override统计并在builder两阶段之间调用fallback；range lookup
directed专项增加跨ASID恢复、owner不变和删除后global恢复检查；需要重新编译、专项复测和10000笔原seed复验。

### [IMPLEMENTATION_DELTA] onlyStage1必须使用hgatp VMID且global fallback只接受detached entry

来源：独立实现review发现，builder在onlyStage1响应中冻结`hgatp_vmid`，DUT的`PtwRespS2.hit()`也比较该
VMID，但`mmu_csr_runtime_state::current_vmid(2'd1)`原先返回0。这样request lookup key不会按VMID隔离，
exact hit可能跨VMID复用旧entry，range/raw matcher又会用0与entry的真实VMID比较而失配。review同时发现
global fallback虽然当前生产调用传入prepared entry，但helper自身没有拒绝canonical live handle。

原plan：正文要求onlyStage1和allStage使用冻结S1 VMID，也要求fallback只修改detached prepared entry，
但没有把这两个约束落实为公共helper的入口检查和专项断言。

实现调整：`current_vmid()`对onlyStage1、onlyStage2和allStage统一返回`hgatp_vmid`，noS2xlate仍返回0。
`derive_allowed_tlb_shape_mask_with_global_fallback()`要求输入entry处于显式`PREPARED`构建阶段、generation为0，
且range/parent ownership queue为空，否则立即fatal。专项增加onlyStage1、onlyStage2同/异VMID，allStage
ASID/G/VMID，以及同ASID owner、已有global owner和`owner_shape > candidate_shape`的正反场景；directed
global候选由builder prepare入口生成。

原因：VMID是DUT onlyStage1命中条件的一部分，lookup key和raw matcher必须使用同一上下文。generation为0
只能证明尚未分配canonical代号，不能区分prepared与finalized-detached payload；显式构建阶段配合ownership
queue为空可在固定时间内拒绝live/finalized handle，无需扫描canonical表，避免fallback原地清G后留下旧索引scope。

影响范围：`mmu_csr_runtime_state.sv`、`common_data_transaction.sv`、range lookup directed专项、implementation
review和重新编译/复测；不修改DUT RTL/Scala。

### [IMPLEMENTATION_DELTA] payload构建阶段与builder同源directed payload

来源：第二轮独立实现review发现，generation和ownership queue不能证明fallback输入仍处于prepare/finalize之间；
旧directed helper还手工拼接level/tag/raw PPN，未生成resolved PPN，且onlyStage1/onlyStage2使用了生产流程不可达的
双stage分页CSR组合。

原plan：要求fallback只修改detached prepared entry，directed专项覆盖全部stage/context/shape，但没有定义可机器
检查的payload构建阶段，也没有要求directed entry复用生产builder的完整payload生成路径。

实现调整：`memblock_tlb_entry`新增`RESET/PREPARED/FINALIZED`构建阶段。builder prepare结束置`PREPARED`，
finalize入口只接受`PREPARED`并在tag、PPN、sector、PBMT和resolved PPN全部生成后置`FINALIZED`；fallback只接受
`PREPARED`，publish/range/parent注册只接受`FINALIZED`。directed owner通过builder prepare与同一个finalize核心生成
完整payload，测试专用shape override只绕过随机shape选择，不绕过payload物化。新增finalized-detached fallback拒绝
专项。onlyStage1使用`vsatp=Sv48,hgatp=Bare`，onlyStage2使用`vsatp=Bare,hgatp=Sv48`，并断言
`expected_s2xlate()`与目标一致。既有PPN-reuse superpage directed helper在完成tag、resolved PPN和sector payload
后显式标记`FINALIZED`，继续满足统一publish契约。

原因：构建阶段是prepared语义的直接证明；builder同源payload避免测试空壳entry误过publish检查；真实可达CSR组合
才能证明VMID隔离覆盖生产路径，而不是只验证人为拼接的lookup key。

影响范围：`memblock_tlb_entry.sv`、`tlb_map_builder.sv`、`common_data_transaction.sv`、range lookup与PPN-reuse
directed专项、plan/review文档及重新编译/复测；不修改DUT RTL/Scala。

### [IMPLEMENTATION_DELTA] 非法重叠专项捕获fatal后不得继续miss build

来源：最新实现的range lookup专项中，test-only非法重叠注入按预期触发并捕获了
`illegal overlapping L2TLB live hit` fatal。由于原专项调用完整`get_or_create`入口，fatal被
`uvm_report_catcher`吞掉后函数继续执行到miss build；故意注入的非法live状态随后又屏蔽4KB，产生
第二个无关的`ordinary 4KB fallback is unexpectedly blocked` fatal。

原plan：通过test-only injection捕获一次预期fatal，验证多命中防御分支，但没有明确catch后控制流边界。

实现调整：专项先显式构造request key，只调用只读`find_tlb_range_hit_by_req()`触发重叠检查；不再调用
会继续创建entry的完整`get_or_create`入口。生产路径、索引注册/删除和fallback规则不变。

原因：`uvm_report_catcher`的`CAUGHT`会抑制报告，不会像未捕获`UVM_FATAL`一样终止仿真。专项必须把
预期fatal限制在只读检查边界，避免在故意非法状态下继续执行生产建表逻辑。

影响范围：仅`soft_test_l2tlb_range_lookup_sequence.sv`的负向专项控制流；需重新编译并确认恰好捕获一次
预期fatal、最终`UVM_FATAL=0`且专项正常结束。

### [IMPLEMENTATION_DELTA] global候选必须覆盖同尺寸和更大跨ASID owner

来源：独立实现review发现，原parent occupancy只让entry登记严格大于自身shape的父区域，且miss候选只查询
候选自身的父区域。这样在ASID A已有2MB non-global entry、ASID B切换后准备创建G=1 entry时，新的global
候选可能绕过ASID隔离，未发现ASID A的同尺寸2MB或更大entry，最终在ASID A下形成多个raw hit。

原plan：parent occupancy只描述child对严格更大parent区域的占用，4KB不作为parent region；候选只查询自身
shape对应的parent bucket。

实现调整：parent occupancy key仍使用固定关联数组，但每个canonical entry从自身coverage shape开始登记到
512GB的全部shape bucket，4KB允许作为最小shape key。miss候选从自身shape开始查询到512GB的全部shape；
non-global只查询当前ASID和已有global，global查询`ANY_ASID`，因此同尺寸和更大范围的跨ASID owner均会被
屏蔽。publish前的live overlap validation使用同一shape后缀查询，防止test-only或边界路径绕过allowed mask。

原因：只登记严格父区域只能防止“先有小页、后建大页”，不能防止“先有大页、后切换ASID建global小页”。
把自身shape纳入索引并查询有限的shape后缀，补齐global语义，同时仍是最多5个shape乘2个上下文key的固定
工作量，不需要扫描canonical live表或owner集合。

影响范围：`common_data_transaction.sv` 的parent key归一化、注册、候选mask和publish复核；RM首字节
PMA/PMP gate与该索引调整互相独立。需要增加跨ASID global-overlap专项，覆盖4KB、2MB和更大已有owner，
并复跑原seed与全量回归。

### [IMPLEMENTATION_DELTA] 翻译权限异常时禁止首字节PMA/PMP追加AF

来源：独立实现review发现，首字节已因S1/S2 permission PF/GPF进入`stage_one_fault`或`stage_two_fault`时，
仍可能因已生成PA而查询PMA/PMP，并把额外的MMIO/AF叠加到原翻译异常。

原plan：首字节PMA/PMP用于判断非对齐MMIO和PBMT-NC/IO的停止边界，但未明确排除已有翻译权限异常。

实现调整：首字节查询增加`!stage_one_fault && !stage_two_fault && !access_fault`条件；一旦翻译异常成立，
保留原PF/GPF结果，不再由PMA/PMP推导AF。完整尺寸PMA/PMP查询本来已有同样门控，保持前后语义一致。

原因：权限PF/GPF和PMA/PMP AF是不同阶段的异常，DUT不会在翻译阶段失败后继续用该PA产生第二个访问异常。

影响范围：`memblock_rm.sv` 首字节跨页/非对齐异常建模；增加权限异常加PMA/MMIO的专项负向覆盖，确认只保留
PF/GPF且不误读backing。

### [IMPLEMENTATION_DELTA] RM对地址未对齐跨页访问的终止边界

来源：真实配置 seed `850748471` 复验时，UID=494 的 `LW/LH` 类跨页访问在
`VA=0x866b1fff` 首字节已由 DUT 判定为地址未对齐异常（`exception_vec=0x20`），
但 RM 继续查询第二页 `VPN=0x866b2`。该页没有对应的 DUT L2TLB request，因而
触发 `tlb_entry_exact_or_range query miss`。

原 plan：只规定翻译 fault 后停止后续页查询，没有明确地址未对齐异常对跨页查询
边界的影响。

实现调整：RM在首个字节已确定为 DUT 会产生地址未对齐异常的场景下，完成首个字节
的翻译和 PA 记录后停止跨页查询；对于 `hdMisalign=1` 的配置，额外查询首字节的
PMA/PMP 属性，若首字节属于已确认的 MMIO 区域或已产生 PMA access fault，则停止
后续页查询并把该事实同步到 RM 的 `access_fault/expected_exception`。若硬件配置允许
该类访问继续执行且首字节不是 MMIO/fault，则不提前停止，仍按每个字节解析翻译。
该调整只修正 RM 对 DUT 已停止请求的建模边界，不修改 DUT RTL。

原因：DUT 不会为未对齐异常后的剩余页面发出 L2TLB request，RM不能要求测试框架
为不存在的请求补造翻译表项。

影响范围：`memblock_rm.sv` 的跨页翻译循环；专项测试增加地址未对齐单页/跨页
覆盖，原 overlap guard、range index、parent occupancy 和 provenance 逻辑不变。

当前range lookup只能回答“已有entry是否覆盖本次请求”，不能回答“本次准备创建的大页是否反向
覆盖其他地址对应的已有小页”。典型错误流程为：

```text
VPN A创建2MB entry
  -> VPN B与A同属一个1GB区域，但不在A的2MB范围
  -> VPN B exact/range miss
  -> VPN B随机到1GB
  -> 新1GB entry反向覆盖A的2MB entry
  -> VPN A再次查询时2MB和1GB同时命中DUT
```

已观察实例中，两条entry生成PPN `0x3315e`和`0x0f35e`，DUT输出
`0x3315e | 0x0f35e = 0x3f35e`。因此不能把测试框架当前“多个range候选选择最大页”作为DUT语义。

实现遵循以下原则：

1. 先用range index处理“已有大页覆盖新请求”。
2. 只有range miss才用parent occupancy index处理“新大页覆盖已有小页”。
3. 正常路径只查询count，不遍历owner queue或完整live表。
4. coverage shape是权威概念，不能只用level，因为64KB NAPOT也是level 0。
5. 上下文兼容性直接编码进索引key，避免不同ASID/VMID集中到同一bucket后再线性过滤。
6. canonical live唯一性和历史response provenance分开处理。
7. Fence只通过现有canonical删除入口联动索引，不新建第二套Fence匹配逻辑。

## 3. 目标功能Flow

```text
L2TLB request fire
  -> 使用该请求冻结的CSR snapshot生成request key
  -> exact lookup
     -> 命中：复用entry并结束
  -> live range lookup
     -> 0个候选：进入miss build
     -> 1个候选：复用entry并结束
     -> 多个候选：live状态非法，fatal并打印候选
  -> prepare_tlb_payload_context冻结stage、CSR、PTE profile、PTE字段和fault
  -> common_data_transaction读取context-aware occupancy count生成allowed shape mask
  -> finalize_tlb_payload_shape_and_payload按原始权重选择合法raw level组合
  -> 无fault候选执行现有N=1 level收敛，fault候选保留raw level/N
  -> 聚合收敛到相同最终组合的权重并选择最终shape
  -> 生成最终tag、PPN、sector、权限和PBMT payload
  -> 使用最终shape做固定索引重叠复核
  -> 预生成并检查range keys和parent occupancy keys
  -> 原子发布canonical entry、range index和parent occupancy index
```

本方案不新增plusarg/cfg。已有level和PTE.N权重仍是随机权威来源；父区域约束只在当前上下文中把
会构造非法重叠的shape权重置零。无冲突场景的随机行为保持不变。

## 执行中补充/修正（IMPLEMENTATION_DELTA）

### [IMPLEMENTATION_DELTA] RM提交早于L2TLB response完成时进入pending/retry

来源：`l2tlb_overlap_guard_v5_20261020` 的真实仿真 seed `850748471`。UID=3870
在 issue 阶段先冻结了旧的 issue-hit 证据，但随后真实 L2TLB request 已经 fire，
`mark_request_fire()` 正确清除了旧 snapshot。DUT 的 store fault/writeback/commit
先于该 request 的 response 完成，RM在commit轮询中立即查询UID翻译，旧逻辑把
“WAITING且已fire、尚未pte_valid”的正常中间态当成 query miss。

原 plan未覆盖：commit事件可能早于L2TLB responder的固定延迟完成，RM需要等待
response发布，而不能复用已经失效的issue-hit snapshot。

实现调整：`memblock_rm_readonly_api`新增不产生UVM错误的
`uid_tlb_translation_pending_for_rm()` readiness probe；`observer_build_commit_item()`
在首个VPN及跨页遍历前检查该状态，并通过`translation_pending`返回值把已fire未完成
状态交给`observer_process_current_commit()`。RM保持当前UID不推进、不置全局
`observer_mismatch_reported`，下一轮poll重试；response完成后原有completed payload
解析和比较流程继续执行。未知记录、取消记录、context不一致和真正的range miss仍走
原有错误路径。

该调整只修改测试框架RM，不修改DUT RTL/Scala，不改变旧issue-hit provenance约束，
也不把pending误当成成功翻译。新增逻辑位于commit中频轮询路径，单次只检查当前UID
及其跨页字节，不扫描完整UID表；专项验证需覆盖response完成前后、普通query miss和
跨页首VPN pending三类场景。

### [IMPLEMENTATION_DELTA] 非对齐首字节PBMT类型必须参与跨页停止判断

来源：独立代码 review 发现，前一版RM在进入跨页循环前把
`observer_should_expect_addr_misaligned()`的`all_bytes_pma_pbmt`固定传为1，
导致`hdMisalign=1`时首字节已经是PBMT-NC/IO仍被当成普通PMA路径，可能继续查询
DUT不会发出的后续页翻译。

实现调整：跨页循环处理首字节并更新`all_bytes_pma_pbmt`后，立即用首字节的实际
PBMT分类重新计算`dut_stops_after_first_page`。普通PMA且HD misalign允许硬件处理时
继续逐字节查询；PBMT-NC/IO、首字节PMA fault或MMIO等需要DUT停止后续翻译的场景
在首字节后退出。该逻辑不改变已完成的首字节翻译，也不影响已确认的翻译fault提前
退出分支。

影响范围：仅RM跨页查询边界和异常期望构建；不修改DUT，不改变L2TLB overlap guard
的canonical/parent索引。需增加或复用PBMT-NC跨页非对齐专项，确认首字节异常与
后续页不查询同时成立。

### [IMPLEMENTATION_DELTA] 独立review后的PBMT约束、pending有界性和日志状态修正

独立review发现并要求修正以下三点：

1. 首字节PMA-MMIO判定必须要求`effective_pbmt==0`。PBMT-NC/IO覆盖底层PMA
   memory type时，不能把底层C=0直接改判为Access Fault；同时首字节PBMT-NC/IO
   仍需参与HD misalign停止条件，避免继续查询DUT不会发出的后续页。
2. `WAITING && request_fire != 0 && !pte_valid`只能证明当前是已发出的pending，
   不能证明response最终一定会到达。RM增加以`get_l2tlb_idle_stop_cycle()`为上界
   的poll计数；超过上界报告明确的pending lifecycle error，避免无限静默阻塞。
3. pending诊断、translation path诊断和最终translation结果使用独立的去重状态。
   pending不会再占用最终`RM_LS_TRACE_TRANSLATION`的已输出标记，且同一UID只打印
   一次pending/path诊断，不在每个1ns poll重复刷日志。

这些调整仍只属于测试框架RM，不修改DUT RTL/Scala；真正的普通query miss、cancel后
无记录、上下文不一致和response lifecycle超时均保留可观测错误。专项验证增加PBMT-NC
跨页、pending成功重试、pending超时诊断和最终translation trace完整性检查。

## 4. 数据结构与索引语义

### 4.1 复用coverage rank

父区域类型直接复用已有枚举：

```systemverilog
typedef enum int unsigned {
    MEMBLOCK_TLB_COVERAGE_4K   = 0,
    MEMBLOCK_TLB_COVERAGE_64K  = 1,
    MEMBLOCK_TLB_COVERAGE_2M   = 2,
    MEMBLOCK_TLB_COVERAGE_1G   = 3,
    MEMBLOCK_TLB_COVERAGE_512G = 4
} memblock_tlb_range_coverage_rank_e;
```

4KB既是allowed shape，也是同尺寸owner登记和查询使用的最小region。`owner_shape`额外区分普通4KB与
64KB NAPOT，避免二者虽然同为level 0却被错误合并。

### 4.2 上下文scope

在`memblock_dispatch_types.sv`新增：

```systemverilog
typedef enum bit [1:0] {
    MEMBLOCK_TLB_CONTEXT_EXACT_ASID = 2'd0,
    MEMBLOCK_TLB_CONTEXT_GLOBAL     = 2'd1,
    MEMBLOCK_TLB_CONTEXT_ANY_ASID   = 2'd2,
    MEMBLOCK_TLB_CONTEXT_S2_ONLY    = 2'd3
} memblock_tlb_context_scope_e;
```

### 4.3 Parent key和owner key

```systemverilog
typedef struct packed {
    memblock_tlb_range_kind_e          range_kind;
    bit [1:0]                          s2xlate;
    bit [15:0]                         vmid;
    memblock_tlb_context_scope_e       context_scope;
    bit [15:0]                         asid;
    memblock_tlb_range_coverage_rank_e owner_shape;
    memblock_tlb_range_coverage_rank_e parent_region;
    bit [51:0]                         normalized_vpn;
} memblock_tlb_parent_occupancy_key_t;

typedef struct packed {
    memblock_tlb_parent_occupancy_key_t parent_key;
    memblock_tlb_lookup_key_t           anchor_key;
} memblock_tlb_parent_owner_key_t;
```

归一化规则：64KB清零VPN低4位，2MB清零低9位，1GB清零低18位，512GB清零低27位。

### 4.4 Count、owner set和entry ownership

在`common_data_transaction.sv`新增：

```systemverilog
int unsigned tlb_parent_occupancy_count[memblock_tlb_parent_occupancy_key_t];
bit tlb_parent_owner_set[memblock_tlb_parent_owner_key_t];
```

在`memblock_tlb_entry`新增：

```systemverilog
memblock_tlb_parent_occupancy_key_t parent_occupancy_keys[$];
```

正常分配只读取`tlb_parent_occupancy_count`。扁平`owner_set`用于拒绝重复注册、精确删除和失败后寻找
witness anchor，不形成每次miss或全Fence的bucket线性扫描。

## 5. 父区域注册和上下文查询规则

### 5.1 Coverage父区域关系

| entry最终shape | 注册父区域 |
|---|---|
| 4KB | 4KB、64KB、2MB、1GB、512GB |
| 64KB NAPOT | 64KB、2MB、1GB、512GB |
| 2MB | 2MB、1GB、512GB |
| 1GB | 1GB、512GB |
| 512GB | 512GB |

#### `build_tlb_parent_occupancy_keys()`

抽象功能描述：该新增helper根据一个已完成payload的实际raw-hit覆盖范围和上下文，生成该entry需要
拥有的自身及全部更大区域key；它是纯计算helper，不注册所有权。

函数目的：让反向重叠保护覆盖4KB、64KB、2MB、1GB和512GB，并与现有raw matcher保持同一语义。

输入：canonical `anchor_key`、完整entry和冻结CSR上下文。

输出/副作用：输出去重后的`parent_keys` queue；不读写count、owner、canonical或range索引。

源码级伪代码：

```text
range_keys = build_entry_range_index_keys(entry)
entry_shape = derive actual raw-hit coverage from entry
for each parent region from entry_shape through 512GB:
    normalized_vpn = clear low VPN bits for parent region
    context_keys = expand normalized_vpn by owner_shape/s2xlate/range_kind/context scope
    append context_keys not already present
return parent_keys
```

中文文字伪代码：先复用`build_entry_range_index_keys()`和现有raw shape推导，确保noS2xlate sector多
`valididx`、onlyStage1、onlyStage2和allStage与DUT匹配语义一致。随后从entry自身shape到512GB逐级生成key，
每个key保留真实`owner_shape`并按5.2的scope规则展开。helper不把key写进entry；只有统一publish成功后才
记录ownership。

### 5.2 Context注册规则

上下文scope的唯一权威定义为：

```text
context_global = s1_stage_active && s1_pte_g
```

`s2_pte_g`不参与scope选择。onlyStage2忽略ASID和global，只按S2 VMID隔离；allStage使用S1的ASID、
`s1_pte_g`和S1 VMID语义，与`entry_matches_request_raw()`一致。

S1非global entry对每个父区域注册：

```text
EXACT_ASID(asid=entry.s1_asid)
ANY_ASID(asid=0)
```

S1 global entry注册：

```text
GLOBAL(asid=0)
ANY_ASID(asid=0)
```

S2-only entry注册：

```text
S2_ONLY(asid=0, vmid=entry.s2_vmid)
```

noS2xlate固定`vmid=0`；onlyStage1和allStage使用冻结的S1 VMID；onlyStage2使用S2 VMID。

### 5.3 Context查询规则

新S1非global候选对每个候选父区域查询：

```text
EXACT_ASID(当前asid)
GLOBAL
```

新S1 global候选查询：

```text
ANY_ASID
```

新S2-only候选查询：

```text
S2_ONLY(当前vmid)
```

因此不同非global ASID可以合法复用相同VPN范围；任一方为global时会进入同一冲突查询；不同VMID、
不同`s2xlate`和不同`range_kind`不会互相误限制。

## 6. 两阶段Builder与合法Shape选择Flow

`tlb_map_builder.sv`在package中先于`common_data_transaction.sv`定义，builder不能反向持有或读取
`common_data_transaction`的occupancy状态。因此miss build拆成“builder准备、common data查询、builder
完成”三个顺序步骤，live索引所有权仍只属于`common_data_transaction`。

### 6.1 `memblock_tlb_entry::derive_allstage_lookup_shape()`

抽象功能描述：该共享纯方法在entry的S1/S2最终level/N已经确定后，返回allStage实际raw-hit level/N；
它不读取common data，也不修改entry。

函数目的：解决builder先于common data定义的编译依赖，并让builder候选、range index和raw matcher共用
唯一allStage shape算法。

输入：四个显式纯值参数`s1_level/s1_pte_n/s2_level/s2_pte_n`。调用者在进入该静态helper前负责确认
当前路径确实是合法allStage双stage形状。

输出/副作用：输出`effective_level/effective_n`；不读取entry、stage-active、表或随机状态，也不产生副作用。

源码级伪代码：

```text
effective_level = min(s1_level, s2_level)
effective_n = (s1_pte_n && s2_level != 0) ||
              (s2_pte_n && s1_level != 0) ||
              (s1_pte_n && s2_pte_n)
```

中文文字伪代码：该算法从`common_data_transaction.sv`移动到先定义的`memblock_tlb_entry.sv`，公式保持
不变并实现为四参数静态纯函数。builder在完成无fault level收敛或保留fault raw level后调用；
`build_entry_range_index_keys()`、`get_tlb_range_match_coverage_rank()`和`entry_matches_request_raw()`在各自
已有stage形状检查之后调用该静态方法。原common data实例helper删除，禁止保留第二份公式。

### 6.2 `prepare_tlb_payload_context()`

抽象功能描述：该新增builder helper创建detached entry并冻结本次build不应被shape约束改变的上下文；
它不选择S1/S2 level、不生成PPN、不访问live表。

函数目的：在查询occupancy前稳定stage、CSR、PTE和fault语义，同时解除builder对common data类型的依赖。

输入：`memblock_tlb_lookup_key_t key`和非空`mmu_csr_runtime_state csr_state`。

输出/副作用：返回prepared entry；只修改该detached entry，不修改canonical、range、completed或parent表。

源码级伪代码：

```text
entry = create/reset
entry.lookup_key/s2xlate/csr_context = key/csr_state
freeze_stage_context(key, csr_state, entry)
freeze_pte_profile_modes(entry)
for each active stage:
    fill_pte_fields(stage, active = true, entry)
fill_faults(key.s2xlate, entry)
apply existing out-of-range S2 effective-GPF rule
finalize_pte_fields for active stages, but do not change level/N
return entry
```

中文文字伪代码：先创建并reset detached entry，再写入lookup key和CSR序号。随后复用现有stage和profile
冻结helper，采样权限、G、N等PTE字段并确定effective fault。此阶段不得调用`choose_level()`，也不得执行
“无fault且N=1则level=0”的fixup；因此entry只表示冻结上下文，不是可发布payload。

### 6.3 `derive_allowed_tlb_shape_mask()`

抽象功能描述：该新增`common_data_transaction` helper在prepared entry形成后，以固定次数的count查询返回
4KB、64KB、2MB、1GB、512GB合法集合；它不选择level、不扫描owner、不修改entry。

函数目的：阻止新entry反向覆盖当前上下文中的已有child，同时保持exact/range hit快路径不变。

输入：请求VPN、prepared entry中的stage/mode/ASID/VMID和`context_global`。

输出/副作用：返回5bit `allowed_shape_mask`；更新轻量lookup/block计数，不修改任何所有权表。

源码级伪代码：

```text
allowed = shapes_supported_by_active_translation_modes
allowed[4KB] = 1
for candidate_shape in {4KB, 64KB, 2MB, 1GB, 512GB}:
    query_keys = build_overlap_query_keys(vpn, prepared_entry, candidate_shape)
    for each fixed query_key:
        if occupancy_count[query_key] > 0:
            allowed[candidate_shape] = 0
            break
return allowed
```

中文文字伪代码：先按Sv39/Sv48及active stage建立架构允许集合。对每个候选shape枚举固定五种owner shape：
较小或同尺寸owner按候选区域归一化，更大owner按owner真实区域归一化。S1 non-global查当前ASID与GLOBAL，
S1 global查ANY_ASID，onlyStage2只查当前S2 VMID。任一count非零即屏蔽该shape，包括被真实更大owner覆盖的
4KB候选；owner witness只允许在后续fatal诊断路径查询。

### 6.4 `finalize_tlb_payload_shape_and_payload()`

抽象功能描述：该新增builder helper消费prepared entry和allowed mask，选择合法最终raw-hit shape并生成
完整immutable payload；它不发布entry，也不读取live索引。

函数目的：复用现有level权重与payload生成逻辑，同时保证实际raw matcher shape属于allowed mask。

输入：prepared entry、`key`、`csr_state`和`allowed_shape_mask`。

输出/副作用：返回完整detached entry；更新builder shape约束统计，不修改common data表。

源码级伪代码：

```text
candidate_buckets = empty
for each active stage:
    stage_weights = level0-only when weight enable == 0, else configured weights
    zero stage_weights for raw levels unsupported by that stage translation mode
    if sum(stage_weights) == 0:
        fatal invalid level-weight configuration
for each raw active-stage level combination:
    raw_weight = product of filtered per-stage weights
    if raw_weight == 0:
        continue
    candidate = copy(prepared_entry)
    candidate.stage_level = raw level combination
    if !candidate.has_effective_fault():
        for each active stage with candidate.pte_n == 1:
            candidate.stage_level = 0
    else:
        preserve selected raw stage level and pte_n unchanged
    effective_shape = derive shape from candidate final raw fields
    if mode supports effective_shape && allowed[effective_shape]:
        aggregate positive raw_weight into bucket keyed by final S1/S2 level/N and shape

total_positive_weight = sum(candidate_bucket.weight)
if total_positive_weight == 0:
    apply documented minimum N-override fallback and rebuild buckets
if total_positive_weight still == 0:
    force ordinary 4KB: all active levels=0 and all active N=0
    require allowed[4KB] and mode support, otherwise fatal
    increment forced_4k_fallback_count and emit rate-limited warning

selected_bucket = weighted_select(aggregated bucket weights)
write selected final level/N fields to entry
generate tag/canonical PPN/sector/PBMT/resolved PPN with existing helpers
run inactive-stage, sector and raw-shape consistency checks
return entry
```

中文文字伪代码：单stage枚举4个raw level，allStage枚举16个raw level组合。每个候选先复制prepared
entry，再按当前builder真实规则得到最终stage字段：无effective fault时，active stage只要`PTE.N=1`，该
stage level就收敛为0；有effective fault时不执行该fixup，所选raw level和N原样保留。因此fault只有
`level=0 && N=1`时是64KB，`level=1/2/3`仍分别是2MB/1GB/512GB。allStage候选完成上述转换后，必须
调用entry共享的`derive_allstage_lookup_shape()`得到effective shape，不能直接从冻结N或原始组合猜测。

level权重开关关闭时必须复现现有`choose_level()`行为：所有active stage只建立level 0权重1候选，不读取
配置中可能非零的level 1到3权重。开关打开时才读取现有stage权重。每个active stage先独立按自己的
translation mode过滤raw level：Sv39的level 3权重固定为0，Sv48允许level 0到3；allStage即使最终
effective shape较小，也不能保留任一stage自身不支持的raw level。过滤后才计算stage权重乘积。

多个raw组合可能因无fault NAPOT fixup收敛到同一最终组合。实现按最终`S1 level/N、S2 level/N、shape`
组成bucket并累加原raw正权重，随后对聚合权重随机选择，从而保持原始raw分布经过合法条件过滤后的概率，
不会把同一最终组合重复当成多种独立结果。`raw_weight=0`的候选不创建bucket；每轮以
`total_positive_weight==0`而不是queue size判断是否进入N override或forced 4KB。

若level权重使能且某个active stage在其mode过滤后的原始权重和本身为0，保持现有`choose_level()`配置
错误语义并立即fatal；只有各stage原始权重合法、经过occupancy allowed过滤后总正权重变为0时，才进入
N override和forced 4KB。

最低限度N override只在所有原始候选都被occupancy屏蔽时使用，且不改变effective fault、权限、G、PBMT
或CSR：单stage清除该active stage的N后重新枚举；allStage依次尝试“仅清S2 N”“仅清S1 N”“同时清
S1/S2 N”，选第一个具有合法候选且清除bit数最少的profile。两个单bit profile均合法时固定优先清S2，
以保持S1语义并保证seed确定性。override发生后，fault候选仍保留新profile下所选raw level/N，不执行
无fault level收敛。

若清N后聚合权重仍为0，可能只是配置把全部正权重放在已被屏蔽的大页，不属于索引损坏。此时强制构造
普通4KB：所有active stage level设为0、N设为0，effective fault及其他PTE/PBMT/CSR字段保持不变，增加
`forced_4k_fallback_count`并输出限流warning。该fallback是候选选择结果，fault payload最终raw level/N
即0/0，不再做后置改写。只有`allowed[4KB]`被错误清除或active mode不支持4KB时才fatal。

### 6.5 `build_payload_for_key_with_csr()`兼容封装

抽象功能描述：该修改后的已有API仅保留给不接入live表的纯builder单元测试；live miss路径不得继续用
它绕过occupancy查询。

函数目的：维持已有离线builder调用兼容性，同时强制runtime路径使用两阶段接口。

输入：`key`、`csr_state`；兼容模式使用“当前mode全部shape允许”的mask。

输出/副作用：返回完整detached payload，不修改任何common data状态。

源码级伪代码：

```text
prepared = prepare_tlb_payload_context(key, csr_state)
mask = all shapes supported by prepared active modes
return finalize_tlb_payload_shape_and_payload(prepared, key, csr_state, mask)
```

中文文字伪代码：wrapper先冻结payload上下文，再构造只受翻译mode限制的mask，最后完成payload。所有
`get_or_create_tlb_entry_by_req_with_snapshot()`运行期miss必须显式执行prepare、derive mask、finalize，
不得调用该兼容wrapper后再尝试修补shape。

### 6.6 `build_tlb_entry_for_key_with_csr()`

抽象功能描述：修改后的common data helper负责一次runtime miss的两阶段payload构造和generation分配；
它读取parent occupancy，但不执行PPN reuse，也不发布live entry。

函数目的：把builder准备、common data mask查询、builder完成和generation分配组织为可实现的顺序入口。

输入：request key和该request冻结的CSR snapshot。

输出/副作用：返回带非零generation的完整detached entry；读取count并更新shape统计，递增generation游标，
不修改canonical/range/parent ownership。

源码级伪代码：

```text
require non-null csr_snapshot
builder = create tlb_map_builder
prepared = builder.prepare_tlb_payload_context(key, csr_snapshot)
allowed = derive_allowed_tlb_shape_mask_with_global_fallback(
    key.vpn, prepared, pte_g_overridden)
entry = builder.finalize_tlb_payload_shape_and_payload(
    prepared, key, csr_snapshot, allowed)
require non-null entry and entry raw shape in allowed
entry.lookup_key/s2xlate = key
entry.entry_generation = allocate_tlb_entry_generation()
return entry
```

中文文字伪代码：common data先创建builder并让其冻结payload上下文，再由自身读取occupancy count形成mask。
若随机得到的global候选只能看到其它ASID的non-global owner，global fallback会把候选收敛为non-global并
重新计算mask，避免无冲突的当前ASID 4KB映射被错误阻塞。随后把prepared entry、key、CSR和mask交回builder
完成shape与payload；完整entry通过raw shape检查后才分配generation。该helper不调用兼容wrapper，且不插表，
因此上层仍可在publish前应用只改PPN的reuse策略。

### 6.7 `get_or_create_l2tlb_entry_by_req_with_snapshot()`

抽象功能描述：修改后的runtime lookup入口保持exact/range hit只读；只有miss按固定顺序构造、可选复用
PPN并统一publish。

函数目的：替换原“build后直接insert再register range”的半发布流程，确保generation、reuse和parent
ownership顺序唯一。

输入：VPN、s2xlate、冻结CSR、PPN reuse enable/weight及现有输出参数。

输出/副作用：hit返回已有entry；miss发布一个新canonical entry并返回created=1，可能读取PPN history。

源码级伪代码：

```text
request_key = csr_snapshot.make_lookup_key(vpn, s2xlate)
if exact live hit:
    return existing entry as EXACT_HIT
if unique live range hit:
    return existing entry as RANGE_HIT

entry = build_tlb_entry_for_key_with_csr(request_key, csr_snapshot)
// 内部顺序已完成 prepare -> mask -> finalize -> allocate generation
if ppn_reuse_en:
    try_apply_l2tlb_ppn_reuse_to_new_entry(entry, weight, reused_ppn)
    // 该helper只允许合法4KB target且只改最终PPN，不改shape/context/generation
publish_live_tlb_entry(request_key, entry, csr_snapshot)
return entry as MISS_BUILD, created=1
```

中文文字伪代码：入口先按request CSR构造key，exact和range hit继续只读返回。miss调用上一节helper，严格
完成`prepare -> derive mask -> finalize -> allocate generation`。随后按原策略尝试PPN reuse；现有reuse
资格检查只允许合法4KB目标且不改level/N，所以不会使parent mask失效。最后调用统一publish再次用最终raw
shape复核并原子注册所有表。原`insert_tlb_entry()`失败回滚分支删除，不再暴露半发布状态。

## 7. 最终校验与原子发布

### 7.1 `validate_no_live_tlb_overlap()`

抽象功能描述：该helper在最终shape和payload生成后重新构造实际parent查询key，确认所有相关
occupancy count仍为0；它不扫描owner、不写状态。

函数目的：在首次写状态前，以最终raw字段复核prepare阶段计算的allowed mask没有因shape收敛遗漏冲突。

输入：canonical anchor key、完整final entry和冻结CSR上下文。

输出/副作用：成功返回1；发现冲突时fatal并在debug路径打印一个witness，不修改任何live状态。

源码级伪代码：

```text
final_shape = 从最终entry按现有raw matcher语义推导
query_keys = 为final_shape和最终上下文生成冲突查询key

for each query_key:
    if occupancy count > 0:
        在失败debug路径从owner set寻找一个witness anchor
        fatal并打印新entry、parent key、count和witness entry

return success
```

中文文字伪代码：先从entry最终level/N和stage组合推导真实coverage shape，再构造该shape对应的固定
上下文查询key。逐个读取count，全部为0才返回成功；若非0，仅在fatal诊断路径遍历owner set找一个
anchor打印。exact/range lookup已排除“已有同级或更大entry覆盖本请求”，因此这里只复核反向覆盖child。

### 7.2 `publish_live_tlb_entry()`

抽象功能描述：该helper是新canonical live entry对外可见的唯一入口。它先预生成并验证全部range和
parent key，再连续发布主表和两个索引，避免半注册状态。

函数目的：把canonical、range与parent ownership作为一个零时间事务发布，并统一directed/runtime入口。

输入：anchor key、完整final entry和冻结CSR上下文。

输出/副作用：成功后写入`tlb_entry_by_key`、range index、count、owner set和live entry自身ownership；
失败必须发生在首次写状态前。

源码级伪代码：

```text
确认request key尚无canonical entry
确认entry generation、lookup key和最终payload有效
调用validate_no_live_tlb_overlap完成固定索引复核
预生成range index keys和parent occupancy keys
检查同一entry内部key无重复
检查所有owner key尚未注册、count未溢出

插入tlb_entry_by_key
注册range index，使后续内部地址可以复用该entry
对每个parent key：
    occupancy count++
    owner_set[{parent key, anchor key}] = 1
    把parent key保存到entry.parent_occupancy_keys
```

中文文字伪代码：先检查anchor、generation和payload，再调用`validate_no_live_tlb_overlap()`用最终raw
shape复核count。随后预生成并去重range/parent key，检查owner不存在和count可递增。所有检查通过后，
连续写canonical、range、count和owner，最后把parent key保存到canonical live entry。SystemVerilog
function不消耗时间，其他request不会看到半发布状态。

## 8. 删除、Fence与Reset生命周期

### 8.1 `unregister_tlb_parent_occupancy_index()`

抽象功能描述：该helper在canonical删除前按entry-owned key精确删除扁平owner并递减count；它不扫描
bucket，不重新推导shape，也不处理Fence地址匹配。

函数目的：保证parent索引删除与publish严格对称，避免Fence/reset后残留虚假占用。

输入：anchor key和canonical live entry。

输出/副作用：删除对应owner，递减或删除count，并清空live entry的ownership queue。

源码级伪代码：

```text
遍历entry.parent_occupancy_keys
构造{parent key, anchor key} owner key
owner必须存在，occupancy count必须大于0
删除owner set元素
occupancy count--
count变为0时删除count关联数组元素
清空entry.parent_occupancy_keys
```

中文文字伪代码：逐个读取entry保存的parent key，组合owner key并检查owner/count均有效。删除owner后
将count减一，减到0时删除关联数组元素。全部完成后清空entry queue；任何缺失或下溢均说明生命周期
不对称，立即fatal。

### 8.2 Canonical删除入口

抽象功能描述：修改后的`delete_live_tlb_entry_by_anchor_key()`是Fence和soft test逐条删除canonical live
entry的唯一入口；它不重新判断Fence匹配条件。runtime全量clear一次销毁完整所有权域，不逐entry调用本入口。

函数目的：按依赖顺序撤销range和parent ownership，再删除canonical及对应completed cache。

输入：anchor key和删除原因字符串。

输出/副作用：移除该anchor对应的全部live索引与completed response状态。

源码级伪代码：

```text
确认canonical entry存在
反注册range index
反注册parent occupancy index
删除tlb_entry_by_key
删除对应completed cache entry
```

中文文字伪代码：先确认canonical存在，保存entry引用；range索引先撤销，防止后续请求发现将被删除的
anchor；parent ownership随后精确递减；最后删除canonical和相同anchor的completed cache。SFENCE/HFENCE
C4仍只用`sfence_match_entry()`决定anchor集合，再逐个调用本入口。

现有SFENCE/HFENCE C4继续使用`sfence_match_entry()`决定删除哪些canonical entry，然后统一调用该删除
入口。不能为parent index重新实现Fence匹配。地址Fence删除一个owner时，其他entry对应count仍保留；
最后一个owner删除后count key消失。

### 8.3 `memblock_tlb_entry::reset()`

抽象功能描述：修改后的entry reset清除所有payload字段，同时确保对象不保留任何live parent ownership。

函数目的：防止对象复用或snapshot clear后残留旧parent key。

输入：无。

输出/副作用：重置entry现有字段并清空`parent_occupancy_keys`。

源码级伪代码：执行现有字段reset；最后`parent_occupancy_keys.delete()`。

中文文字伪代码：保留现有payload清零顺序，在reset结束前清空ownership queue。该方法不访问count/owner，
因为只有canonical delete入口有权撤销公共所有权；调用reset前若对象仍是live canonical属于调用错误。

### 8.4 `memblock_tlb_entry::copy_from()`

抽象功能描述：修改后的copy只复制payload与provenance，不把源对象的live ownership复制给snapshot。

函数目的：保证pending、completed和UID payload不会获得canonical parent索引删除权。

输入：非空源entry。

输出/副作用：复制现有payload/provenance字段，目标`parent_occupancy_keys`始终为空。

源码级伪代码：先执行现有逐字段复制；不复制source ownership；最后清空destination ownership queue。

中文文字伪代码：无论源对象是否为live canonical，目标都只得到response所需字段。显式清空目标queue，
使后续snapshot reset/delete不会递减公共count。

### 8.5 `reset_all_tables()`与`clear_dispatch_l2tlb_live_entries()`

抽象功能描述：两个已有reset入口在各自原有清理范围内同步清空新增parent count和owner set。

函数目的：避免testcase reset或runtime reset后留下虚假父区域占用。

输入：`reset_all_tables()`保留现有main transaction数量；runtime clear无新增输入。

输出/副作用：除现有表外，均执行`tlb_parent_occupancy_count.delete()`和
`tlb_parent_owner_set.delete()`。

源码级伪代码：在删除canonical/range表的同一reset分支删除count与owner；不逐entry调用unregister，
因为整个所有权域同时销毁。

中文文字伪代码：全量reset不需要逐owner递减，直接删除canonical、range、count和owner四类live状态。
completed表按原入口语义清理；UID历史是否清理由各入口原有边界决定，不因parent索引改变。

### 8.6 Directed install helper

抽象功能描述：修改`install_s1_range_entry()`和`install_entry()`，使两个soft test安装完整entry时都调用
统一publish，不再手工insert再注册range。

函数目的：directed场景与runtime使用同一重叠检查和ownership生命周期。

输入：anchor key、完整entry及该测试冻结的CSR snapshot。

输出/副作用：成功发布canonical/range/parent状态；失败在首次写状态前fatal。

源码级伪代码：检查entry/key/generation；调用`publish_live_tlb_entry(anchor, entry, csr_snapshot)`；删除
原`insert_tlb_entry()+register_tlb_range_index()`组合。清理统一调用canonical delete入口。

中文文字伪代码：`soft_test_l2tlb_range_lookup_sequence.sv:116`和
`soft_test_l2tlb_ppn_reuse_sequence.sv:312`先保持各自directed payload构造，再把CSR一并传给publish。
测试不再拥有索引注册细节，删除时也不直接改关联数组。

### 8.7 Provenance soft test live安装与旧预期迁移

抽象功能描述：修改`soft_test_rm_provenance_followup_sequence`中的live setup/cleanup和issue-hit断言，
使其遵守统一publish/delete并验证冻结snapshot权威性。

函数目的：移除测试自身对`tlb_entry_by_key`的直接写删，以及旧“RM重新选择最大页”的过期预期。

输入：各task现有CSR、anchor、entry、token和sample。

输出/副作用：directed live entry通过publish/delete维护全部索引；completed overlap case断言RM返回capture
冻结的最新sample entry。

源码级伪代码：

```text
check_uid_tlb_payload_over_live_entry/check_l1_hit_issue_snapshot_lifecycle:
    build a complete legal replacement entry through build_tlb_entry_for_key_with_csr
    apply only the directed PF override and re-run payload consistency checks
    publish directed live replacement through publish_live_tlb_entry
    run existing UID/RM assertion
    cleanup through delete_live_tlb_entry_by_anchor_key

check_l1_hit_overlap_resolution:
    record super response at sample 102
    record exact PF response at sample 108
    capture at sample 120; require exact PF snapshot
    resolve RM; require same exact PF generation/token/sample
    remove old expectation that RM switches to wider superpage
```

中文文字伪代码：前两个task仍验证UID历史payload不受后续live replacement影响。replacement不得继续用
只填写key/generation/fault的空壳entry；必须先调用`build_tlb_entry_for_key_with_csr()`得到stage、mode、
tag、sector和PPN均完整的detached entry，再只覆盖目标PF/effective fault字段并执行现有sector/inactive
stage一致性检查，之后统一publish并统一delete。overlap task继续构造两个completed历史候选，用于验证
capture前按sample选择最新的exact PF；capture后RM必须保持该entry，不能切换到较旧但更宽的superpage。

### 8.8 `inject_live_overlap_for_expected_fatal_test_only()`

抽象功能描述：该新增soft-test本地helper仅用于构造生产publish永远不会产生的非法live multi-hit状态，
随后验证range lookup的expected fatal；生产runtime不调用它。

函数目的：在不削弱统一publish规则的前提下，保留live多候选fatal分支的定向覆盖。

输入：两个已完成且raw范围重叠的anchor/entry，以及测试CSR。

输出/副作用：仅在当前soft test内直接填充canonical和range index形成非法状态；不注册parent ownership。

源码级伪代码：安装专用L2TLB expected-fatal catcher；test-only helper直接注入两条canonical/range记录；
调用`find_tlb_range_hit_by_req()`并要求捕获一次指定fatal；随后调用`reset_all_tables()`整体清理非法状态，
不得调用生产delete逐项解释这个故意不完整的ownership。

中文文字伪代码：该helper名称和注释必须包含`test_only`，调用点只存在于soft test。它绕过publish的唯一
理由是覆盖publish已禁止的防御分支。fatal catcher按report id/message精确匹配且检查捕获次数为1；清理
使用全表reset，保证非法注入不会污染后续case。

completed response cache、pending token和UID snapshot不注册parent occupancy；它们是历史provenance，
不是当前DUT active TLB所有权。

## 9. Live唯一性与历史RM边界

### 9.1 Live查询

抽象功能描述：修改后的`find_tlb_range_hit_by_req()`只解析当前canonical live表的raw range hit；它不
读取completed历史，也不承担RM结果选择。

函数目的：把DUT要求的one-hot live不变量变成环境自身的即时一致性检查。

输入：request key和对应CSR snapshot。

输出/副作用：0候选返回miss，1候选返回anchor/entry，多候选fatal并打印全部anchor；不修改表。

源码级伪代码：

```text
candidates = deduplicated raw matches from live range index
if candidates.size == 0: return miss
if candidates.size == 1: return hit candidate
fatal live one-hot invariant violation with all candidate anchors
```

中文文字伪代码：按现有range query key读取live bucket，去重anchor并用`entry_matches_request_raw()`复核。
没有候选是正常miss，一个候选正常返回；两个及以上说明测试框架已让DUT可能multi-hit，立即fatal，不能
再按最大coverage rank选择一个结果掩盖状态错误。

### 9.2 Completed和UID历史查询

不能把live多候选规则直接扩展到completed response和UID历史snapshot。Fence前后的不同generation可能
在历史记录中同时存在，但不代表DUT当前TLB同时命中。

#### `find_completed_tlb_response_for_issue()`

抽象功能描述：修改后的该helper在UID snapshot尚未建立时，使用completed cache自身的response provenance
和UID wait起点，确定issue边界之前最后一个可见response；它不读取UID尚未生成的token/generation。

函数目的：消除capture前循环依赖，并用时间provenance替代“最大页优先”的历史仲裁。

输入：request key、CSR snapshot和`issue_boundary_sample = uid_wait_start_sample_seq`。

输出/副作用：返回唯一entry及其token/sample；无候选返回0，同一最新sample存在多个raw候选时fatal。

源码级伪代码：

```text
candidates = exact/range completed entries that:
    have complete token/sample provenance
    response_sample <= issue_boundary_sample
    raw-match request under captured CSR
if candidates empty: return miss
latest_sample = max(candidate.response_sample)
latest_candidates = candidates with response_sample == latest_sample
if latest_candidates.size != 1:
    fatal ambiguous same-sample completed visibility
return latest candidate entry/token/sample
```

中文文字伪代码：capture前唯一可用的时间边界是record初始化时冻结的`uid_wait_start_sample_seq`。helper
收集不晚于该边界且raw匹配的completed响应，按response sample选择最后一次可见fill。旧generation即使
仍在历史索引中也不会覆盖较新的fill；若同一最新sample有两个不同raw候选，无法证明DUT one-hot，按
环境状态错误fatal。该算法不使用coverage rank，也不访问尚未创建的UID snapshot字段。

#### `capture_uid_issue_hit_snapshot()`

抽象功能描述：修改后的capture helper在no-request-fire issue被确认时，把上述唯一completed response
完整冻结到UID record，形成后续RM的唯一权威来源。

函数目的：使后续cache替换、Fence或同anchor新generation不会改变该UID的issue-time解释。

输入：WAITING UID record，其中CSR snapshot和`uid_wait_start_sample_seq`已冻结。

输出/副作用：写入`issue_hit_payload/token/sample/generation`及请求专属derived字段；无候选返回0。

源码级伪代码：

```text
winner = find_completed_tlb_response_for_issue(
    record.lookup_key, record.csr_snapshot, record.uid_wait_start_sample_seq)
if no winner: return 0
record.set_issue_hit_snapshot(winner.entry, winner.token, winner.sample)
derive request-specific PPN/GVPN from frozen payload and record VPN
store derived fields
return 1
```

中文文字伪代码：capture只把completed helper返回的唯一winner写入record，并立即基于UID自己的VPN推导
PPN/GVPN。token、sample和generation由winner一次性冻结，不再由后续查询反推。该过程不重扫live表，
也不按页大小替换winner。

#### `resolve_uid_issue_hit_entry_for_rm()`

抽象功能描述：修改后的RM只读helper直接读取UID冻结的issue-hit payload和derived字段；它不重新扫描
completed range index，也不重新执行候选仲裁。

函数目的：保证RM使用的entry与issue时capture的response完全一致，消除cache后续变化造成的漂移。

输入：common data只读句柄、有效WAITING UID record、request VPN和context view。

输出/副作用：填充entry/context view；仅做provenance一致性检查，不修改测试框架状态。

源码级伪代码：

```text
require issue_hit_snapshot_valid and non-null payload
require frozen sample <= uid_wait_start_sample_seq
require frozen generation == payload.entry_generation
require payload raw-matches request/CSR
copy frozen payload into entry_view
copy record.request_* derived fields into context_view
return hit
```

中文文字伪代码：RM先验证snapshot、sample、generation和raw match均一致，随后直接复制冻结payload及
capture时已计算的请求专属字段。删除当前`memblock_rm_readonly_api.sv:1204`开始的completed cache重扫、
补回旧snapshot候选和最大coverage选择逻辑。历史唯一性在capture时已经确定，RM不得二次改变结论。

## 10. 性能与统计要求

新增逻辑的正常运行期开销限定为：

1. exact/range hit不访问parent occupancy。
2. 一次non-global mask计算对每个候选shape最多执行5个owner shape乘2个context scope，即10次count查询；
   五种候选shape最坏共50次。global首次计算最多25次；若触发清G fallback，再做一次non-global计算，因此
   mask阶段最坏75次。
3. publish前按最终shape复核最多额外10次count查询，所以单次miss最坏85次固定关联数组查询，不随live entry
   数量增长。
4. insert/delete按每entry最多10个context-expanded parent key执行关联数组增删。
5. parent mask、publish复核、单entry注册和单entry删除不扫描完整live表或parent owner bucket；Fence按已有
   语义扫描live/completed entry选择失效对象，不属于固定常数上界的parent查询本身。
6. owner set遍历只允许出现在fatal/debug witness查找路径。

新增轻量统计，测试结束统一打印，不逐请求输出`UVM_INFO`：

```text
parent_lookup_count
parent_block_64k_count
parent_block_2m_count
parent_block_1g_count
parent_block_512g_count
pte_n_override_count
pte_g_override_count
forced_4k_fallback_count
parent_owner_peak_count
```

同一seed、相同10000笔配置必须比较修改前后墙钟时间和日志大小；若出现明显退化，优先检查是否误把
owner witness扫描或逐请求日志放入正常路径。

## 11. 修改范围

计划修改：

- `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_types.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_tlb_entry.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/tlb_map_builder.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_rm_readonly_api.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/mmu_csr_runtime_state.sv`
- `mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_l2tlb_range_lookup_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_l2tlb_ppn_reuse_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_rm_provenance_followup_sequence.sv`
- `mem_ut/ver/ut/memblock/tc/src/soft_test/soft_test_tc_rm_pbmt_effective_fault.sv`
- `mem_ut/ver/ut/memblock/tc/src/basicTest.sv`
- 对应plan review、implementation review和必要bug记录

明确不修改：

- DUT RTL/Scala和`Mux1H`。
- DUT replacement、refill或SFENCE实现。
- 主存地址范围和PPN reuse策略。
- 现有level、PTE.N权重参数名和默认值。
- UID历史snapshot的生命周期所有权。

## 12. 验证方案与验收标准

### 12.1 定向功能场景

反向重叠基础矩阵必须逐项覆盖全部10种child到parent组合：

```text
4KB  -> 64KB / 2MB / 1GB / 512GB
64KB -> 2MB / 1GB / 512GB
2MB  -> 1GB / 512GB
1GB  -> 512GB
```

每种基础组合至少与以下维度形成pairwise交叉，关键边界做全交叉：

1. noS2xlate、onlyStage1、onlyStage2、allStage。
2. Sv39与Sv48；Sv39确认512GB候选在mode mask阶段被禁止。
3. non-global同ASID冲突、异ASID允许；existing global到new non-global和反向global冲突。
4. onlyStage2同VMID冲突、异VMID允许；allStage以S1 VMID和S1 global语义判断。
5. 无fault NAPOT收敛，以及fault raw `level=0,N=1`、`level>0,N=1`两类shape。
6. allStage 16个raw level组合先按无fault/fault规则转换，再与entry共享的
   `derive_allstage_lookup_shape()`一致。
7. allStage混合S1=Sv39/S2=Sv48及反向组合，验证Sv39一侧raw level 3在相乘前权重为0，即使另一侧
   level 0会把effective shape缩小也不能放行该组合。

补充生命周期和provenance场景：

8. 已有1GB后请求内部VPN，必须range hit，不进入miss build或parent查询。
9. 地址Fence只递减被删除owner；其他owner保留，最后一个owner删除后count key消失。
10. 全Fence、runtime reset和`reset_all_tables()`后canonical/range/count/owner均为空。
11. entry `copy_from()`到pending/completed/UID snapshot后，目标`parent_occupancy_keys`必须为空。
12. 三个相关directed soft test的生产语义安装只经统一publish，删除只经统一delete。
13. provenance replacement先由builder生成完整合法entry，再覆盖PF并publish；空壳entry必须被测试禁止。
14. 人工注入live重叠候选，live range lookup必须fatal并打印全部anchor。
15. completed历史含多个不同sample raw候选时选择不晚于wait起点的最新sample；同一最新sample双候选fatal。
16. `check_l1_hit_overlap_resolution()`改为断言capture与RM都保持sample 108的exact PF，不再期望切换到
    sample 102的较宽superpage。
17. capture后替换completed cache，RM仍使用冻结token/sample/generation和payload，不重新选择最大页。
18. N导致原候选全被屏蔽时触发最小N override，验证`pte_n_override_count`和最终raw shape合法。
19. level权重使能且全部正权重只指向被屏蔽大页时，零权重候选不得建立bucket，必须强制普通4KB并验证
    `forced_4k_fallback_count`、限流warning、fault字段保持及level/N最终为0/0。

### 12.2 索引一致性检查

1. 每个entry保存的parent key都存在对应owner set和正count。
2. 每个owner set元素都能回查canonical entry。
3. 重复注册同一`{parent, anchor}`必须fatal。
4. 删除不存在owner、count下溢或count为0仍残留owner必须fatal。
5. normal path不得调用owner set全量遍历。

### 12.3 原失败seed复验

使用原失败seed和原始cfg完整执行10000笔请求，验收至少包括：

- `TEST_PASS`。
- `UVM_ERROR=0`、`UVM_FATAL=0`。
- `RM_LS_TRACE_COMPARE=10000`。
- `Quit count=0/10`。
- 正常`$finish`。
- UID106查询波形中`PopCount(hitVecReg) <= 1`。
- DUT PPN不再出现2MB和1GB按位OR形成的`0x3f35e`。
- 统计能够说明对应1GB shape被parent occupancy屏蔽。

### 12.4 回归与性能

原seed通过后恢复问题3配置的100-seed、每seed 10000笔随机回归。对同一失败seed记录修改前后墙钟
时间、日志大小、miss build数量和parent lookup数量。任何新失败都先区分索引生命周期错误、RM历史
provenance问题、配置/基础设施问题和RTL问题，不得恢复“最大页优先”绕过multi-hit。

### 12.5 最终执行与验证结果

1. Range/生命周期专项：
   - mode：`l2tlb_overlap_guard_v16_20261021`
   - `TEST CASE PASSED`
   - 预期 catcher：fatal=2、warning=4
   - 最终 `UVM_ERROR=0`、`UVM_FATAL=0`、Quit `0/10`、正常 `$finish`
2. RM permission/PMA/PBMT 专项最终版：
   - mode：`l2tlb_overlap_guard_v20_exact_exception_20261021`
   - UID5：VA `0x812aa001`、PA `0x20010001`、完整异常向量严格等于 `24'h002000`
   - UID6：VA `0x812bb001`、PA `0x20010001`、完整异常向量严格等于 `24'h200000`
   - `TEST CASE PASSED`、`UVM_ERROR=0`、`UVM_FATAL=0`、Quit `0/10`、正常 `$finish`
3. 原失败seed `850748471`、原cfg、10000笔：
   - mode：`l2tlb_overlap_guard_v17_seed_20261021`
   - `RM_LS_TRACE_COMPARE=10000`、`TEST_PASS`
   - `UVM_ERROR=0`、`UVM_FATAL=0`、Quit `0/10`、正常 `$finish`
   - UID494 未对齐跨页只解析首字节，未再查询不存在的第二页
   - parent统计：`block_64k=961`、`block_2m=2341`、`block_1g=2468`
4. 独立review迭代：首次 review 发现 UID5/6 只检查目标位和Load AF，未严格约束其余异常位；修改为
   完整24-bit case inequality、重新编译和复测后，第二个独立只读 reviewer 返回 `REVIEW_PASS`。
5. implementation review：
   `AI_DOC/plan/test_framework/review_doc/undo/memblock_l2tlb_overlapping_page_allocation_guard_implementation_review_20261021.md`

本次实现关闭条件已满足。100-seed随机回归属于后续扩大验证，不作为本Plan coding关闭的阻塞项。

## 13. 风险与约束

1. coverage shape而不是level是唯一重叠判断单位。
2. parent occupancy只维护canonical live所有权，不复制到completed、pending或UID snapshot。
3. global候选必须查询ANY_ASID；non-global候选必须同时查询EXACT_ASID和GLOBAL。
4. allStage必须先按真实builder规则得到最终stage字段，再调用现有shape helper；不能直接用raw组合猜测。
5. PTE.N冲突只执行文档规定的最小N override，不重新随机effective fault、权限、G、PBMT或CSR。
6. 所有结构检查在发布前完成，删除必须与注册严格对称。
7. owner set只用于O(1)精确所有权和失败诊断，不能成为正常路径扫描集合。
8. fallback warning必须限流，统计计数保留完整频次。
9. live多命中是环境状态错误；历史候选必须在capture前按response sample边界唯一化，两者不能混用。

## 与初步 plan 差异说明

### 修改目的

初步建议只记录2MB所在1GB父区域，不能覆盖4KB、64KB、512GB、global、allStage和fault raw shape，
也没有解决builder定义顺序、历史response归属及reset/copy生命周期。本正式方案把该想法扩展为完整的
live one-hot不变量，同时保持正常路径为固定次数关联数组查询。

### 修改前逻辑行为

```text
创建entry时一次性随机PTE、level、fault并生成PPN
插入canonical表后单独注册range index
range lookup出现多个候选时按最大页选择
completed/RM历史查询再次扫描候选并按最大页选择
Fence删除canonical和range状态

初步建议额外记录2MB对应的1GB parent tag：
  新请求miss时若parent tag命中，则禁止1GB
  Fence时单独清除该记录
```

### 修改后逻辑行为

```text
prepare阶段只冻结stage/CSR/PTE/fault，不选择level或生成PPN
common data用context-aware occupancy count生成allowed shape mask
finalize阶段枚举raw level组合：
  无fault按现有N规则收敛最终level
  fault保留所选raw level/N
  allStage调用提前定义在entry中的共享derive helper计算effective shape
  对相同最终组合聚合原始权重后选择
最终payload通过固定count复核
统一publish原子写canonical/range/parent ownership
统一delete按range、parent、canonical、completed顺序撤销
live多候选fatal
capture前按response sample选择issue边界内最后一个completed response
capture后RM只使用冻结payload，不再重扫或按最大页选择
```

### 新增helper差异

#### `memblock_tlb_entry::derive_allstage_lookup_shape()`

抽象功能描述：在最终stage level/N确定后计算allStage raw-hit shape，供builder与common data共用。

添加原因：原算法位于后定义的common data，先定义的builder无法合法调用。

功能说明：接收四个显式level/N纯值参数并输出effective level/N，不读取entry、表或随机状态。

详细文字伪代码：调用者先检查或通过构造保证S1/S2均active，再传入两stage的level/N；effective level取
两stage较小值，effective N按DUT现有三项布尔公式计算；builder、range index、coverage rank和raw matcher
全部调用该静态方法，删除common data中的重复公式。

#### `prepare_tlb_payload_context()`

抽象功能描述：在miss build第一阶段冻结不受shape约束改变的entry上下文，不访问live索引。

添加原因：builder定义早于common data，原单函数无法在PTE/fault冻结后插入occupancy查询。

功能说明：输出prepared entry，供common data计算mask后继续finalize。

详细文字伪代码：创建并reset entry，写入key和CSR，冻结stage/profile，采样PTE字段和effective fault，
执行已有fault合法化但不选择level、不生成PPN，返回detached prepared entry且不修改任何表。

#### `derive_allowed_tlb_shape_mask()`

抽象功能描述：在两个builder阶段之间读取parent count，生成本次miss允许的coverage shape集合。

添加原因：现有range索引只能检测“已有大页覆盖新请求”，不能检测“新大页覆盖已有child”。

功能说明：固定查询各候选parent region，不扫描owner或完整live表。

详细文字伪代码：按active mode初始化4KB、64KB、2MB、1GB、512GB候选mask。对每个候选shape C固定枚举
五种owner shape：当`owner_shape <= C`时按候选C的归一化区域查询，判断候选区域内部是否已有小于或等于C的
owner；当`owner_shape > C`时按请求地址所在owner真实区域查询，判断是否已有更大owner覆盖该候选。S1
non-global分别查询EXACT_ASID与GLOBAL，S1 global查询ANY_ASID，onlyStage2查询S2_ONLY；count非零则清除
对应候选bit。4KB既是最小候选shape，也是parent occupancy的同尺寸owner bucket，返回mask前不扫描owner集合。

#### `finalize_tlb_payload_shape_and_payload()`

抽象功能描述：消费prepared entry和allowed mask，选择最终raw-hit shape并生成完整payload。

添加原因：shape必须在occupancy查询后选择，且allStage/fault必须与真实builder fixup顺序一致。

功能说明：枚举、转换、聚合并加权选择合法候选，再复用现有tag/PPN/sector/PBMT生成helper。

详细文字伪代码：level权重关闭时只建立level-0候选，开启时枚举单stage 4组或allStage 16组raw level；
每个stage先按自身Sv39/Sv48 mode过滤raw level并拒绝零权重组合，再计算乘积。无fault时把active `N=1`
stage level收敛到0，fault时保持raw level/N；allStage调用entry共享shape helper；仅对正权重合法shape按
最终组合累加。总正权重为0时先做最小N override，仍为0则强制普通4KB并增加fallback统计；选中结果后
生成完整payload并做一致性检查。

#### `build_tlb_parent_occupancy_keys()`

抽象功能描述：从完整entry的raw-hit shape和上下文计算自身shape及全部更大区域的所有权key。

添加原因：所有entry都需要为可能覆盖自己的更大shape建立反向占用，而不只是2MB到1GB。

功能说明：纯计算并去重key，不写count或owner。

详细文字伪代码：复用range key/raw shape语义确定entry coverage；从自身shape枚举到512GB region；按context scope
展开normalized VPN key并去重；返回parent key queue供publish预检查和注册。

#### `validate_no_live_tlb_overlap()`

抽象功能描述：发布前以最终raw字段复核新entry不会覆盖已有child。

添加原因：prepare时mask与finalize后shape之间需要最终一致性防线。

功能说明：只读固定count，失败时才查owner witness。

详细文字伪代码：推导最终shape，构造冲突查询key，逐个检查count；全0返回成功，非0在debug路径找
witness并fatal，不修改live状态。

#### `publish_live_tlb_entry()`

抽象功能描述：作为runtime和directed场景唯一live发布入口，原子注册三类状态。

添加原因：原`insert_tlb_entry()+register_tlb_range_index()`允许出现半状态并绕过parent ownership。

功能说明：首次写状态前完成全部结构和重叠检查，随后连续发布canonical/range/parent。

详细文字伪代码：检查anchor/generation/payload；预生成并去重range与parent key；确认owner不存在且count
可递增；依次写canonical、range、count、owner，并把parent key保存在canonical live entry。

#### `unregister_tlb_parent_occupancy_index()`

抽象功能描述：canonical删除前按entry-owned key精确撤销parent ownership。

添加原因：Fence和soft test的逐条删除必须与publish严格对称；runtime clear则在同一零时间函数内批量销毁
canonical、range、count和owner完整所有权域。

功能说明：O(parent key数)删除owner和递减count，不重新推导shape或扫描bucket。

详细文字伪代码：遍历entry ownership queue，检查owner/count，删除owner并递减count，count为0时删除
元素，最后清空queue；缺失或下溢fatal。

### 修改helper差异

#### `build_tlb_entry_for_key_with_csr()`与`get_or_create_l2tlb_entry_by_req_with_snapshot()`

修改原因：原runtime miss把完整build、generation、reuse、insert和range注册分散执行，无法插入occupancy
查询，也可能暴露半发布状态。

抽象功能描述：前者完成两阶段build及generation分配，后者组织hit快路径、PPN reuse和统一publish。

修改前文字伪代码：build helper调用一次性builder并立即分配generation；runtime入口可选reuse后先
`insert_tlb_entry()`，再注册range，失败时删除canonical回滚。

修改后文字伪代码：build helper执行prepare、common data mask查询、finalize、raw shape检查和generation
分配；runtime入口exact/range miss后调用build helper，可选执行只改4KB PPN的reuse，最后调用统一publish
原子写canonical/range/parent。

差异影响：明确固定顺序为`prepare -> mask -> finalize -> generation -> reuse -> publish`，hit路径不变。

#### `build_payload_for_key_with_csr()`

修改原因：原API一次完成全部payload，runtime无法在shape选择前查询occupancy。

抽象功能描述：修改后仅作为纯builder兼容wrapper，runtime miss改用显式两阶段flow。

修改前文字伪代码：创建entry，连续采样PTE、选择level、采样fault、执行N fixup并生成PPN后返回。

修改后文字伪代码：调用prepare冻结上下文；构造mode允许的全shape mask；调用finalize生成payload。runtime
不调用wrapper，而是在prepare和finalize之间调用common data mask helper。

差异影响：离线builder调用保持可用，runtime新增反向重叠约束。

#### `delete_live_tlb_entry_by_anchor_key()`与reset/copy入口

修改原因：新增parent ownership后，任何绕过统一删除或复制ownership都会产生悬空count。

抽象功能描述：统一删除所有live索引；reset清表；copy只复制payload provenance。

修改前文字伪代码：删除range、canonical和completed；entry copy包含普通payload字段；directed test手工
insert后注册range。

修改后文字伪代码：删除range后调用unregister撤销parent，再删canonical/completed；两类全局reset清空
count/owner；entry reset/copy清空ownership；directed install/delete改走统一publish/delete。

差异影响：Fence语义不变，但所有索引生命周期完整对称。

#### `find_completed_tlb_response_for_issue()`、`capture_uid_issue_hit_snapshot()`和
`resolve_uid_issue_hit_entry_for_rm()`

修改原因：原历史路径依赖最大页优先，且不能用尚未建立的UID snapshot反向指导capture。

抽象功能描述：capture前按completed response时间边界唯一化，capture后以冻结snapshot为权威。

修改前文字伪代码：completed查询按最大coverage选entry；capture冻结该结果；RM再次扫描历史候选、补回
snapshot并再次按最大coverage选择。

修改后文字伪代码：completed查询只使用cache token/sample和wait起点，选择边界内唯一最新sample；
capture冻结payload/token/sample/generation及derived字段；RM只验证和复制冻结结果，不再重扫。

差异影响：历史generation可共存，但不会被页大小启发式错误重解释；同一最新sample双候选明确fatal。

#### Provenance与range soft test helper

修改原因：现有soft test直接写删live表，并保留“RM重新选择最大页”的旧断言。

抽象功能描述：生产语义setup改走publish/delete；非法multi-hit覆盖通过专用test-only注入和expected-fatal
catcher完成。

修改前文字伪代码：测试直接赋值或删除`tlb_entry_by_key`；overlap case期望capture exact PF后RM切换到
更宽superpage；range test用普通install构造两个重叠live entry。

修改后文字伪代码：普通live replacement先通过builder形成stage/sector/PPN完整的合法entry，再仅覆盖
目标PF并执行一致性检查，随后统一publish/delete；overlap case要求RM保持capture冻结的最新sample exact
PF；需要覆盖range多候选fatal时，test-only helper直接注入非法canonical/range状态，catcher精确捕获
一次fatal，再由`reset_all_tables()`整体清理。

差异影响：soft test不再把生产API禁止的状态当成正常行为，历史snapshot与新RM规则一致。

### 参数、行为与性能影响

本方案不新增、删除或重命名plus/cfg参数，也不改变现有level/PTE.N权重默认值。无重叠场景保持原始
权重条件分布；只有会破坏DUT one-hot命中前提的shape被屏蔽。exact/range hit不增加parent查询，miss
只做固定count查询，insert/delete只按每entry有限parent key更新，owner全量遍历仅存在于fatal诊断路径。
