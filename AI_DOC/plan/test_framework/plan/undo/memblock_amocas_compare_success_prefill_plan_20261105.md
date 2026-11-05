# MemBlock AMOCAS Compare 成功前置 Store 适配计划

## 1. 计划定位

### 1.1 计划信息

- 创建日期：2026-11-05
- 当前版本：v1.0
- 适用代码基线：AMO worktree `.codex_memblock_amo_20261103`，分支 `codex/memblock-amo-adaptation-20261103`
- 适用范围：`mem_ut/ver/ut/memblock` real-dispatch 主表生成、issue 字段派生和 software-only 生成检查
- 目标：当随机主表生成 `AMOCAS.W/D/Q` 时，按参数概率在更老 UID 生成匹配的前置 Store，并在未来 UID 生成原 CAS，使 CAS 发起比较时内存旧值具备等于 compare operand 的确定性前置条件。
- 非目标：本计划不增加 RM、scoreboard 或 CAS 结果 checker，不检查最终内存值，也不修改 RTL。
- Review 状态：已完成三轮独立 subagent review；第一轮4个Major和第二轮3个一致性问题均已修正，第三轮确认无剩余blocker。
- Coding 状态：方案可直接进入coding；尚未实施、编译或仿真。
- 设计原则：保持固定主表长度，不移动既有 UID；通过“当前位置替换 + 未来 UID 预约”实现；任一配对无法完整建立时整体回滚，不能留下孤立 Store。
- 状态约定：`[ ]` 待完成，`[x]` 已完成。

### 1.2 与既有 AMO 方案的关系

本计划建立在以下已实现或已规划能力之上：

1. `AMOCAS.W/D/Q` 已能按真实 STA/STD slot 数量进入原子路径。
2. `AMOCAS.W/D` 使用 STD slot `0,1`；`AMOCAS.Q` 使用 STD slot `0,1,2,3`。
3. `AMOCAS.Q` 使用 STA slot `0,2`，并产生两次 final WB。
4. 原子 admission 只在全部更老 UID 已 terminal 时建立唯一 atomic barrier。
5. LR 生成后已经可以使用 `sc_reservation_by_uid` 预约未来 SC，可复用其“预约替换未来随机项、不扩表”的基本思路。

本计划只补充 CAS compare 成功所需的输入构造，不改变既有 AMO issue、writeback、fault、redirect 和 terminal 机制。

## 2. 需求解释

### 2.1 CAS 真正比较的内容

CAS 不是比较地址。地址只决定从哪里读取旧内存数据，真正的成功条件是：

```text
Memory[address] == compare_operand
```

因此测试激励要提高 CAS compare 成功覆盖，必须同时保证：

1. 前置 Store 和 CAS 使用相同目标内存字节范围。
2. 前置 Store 写入值等于 CAS 的 compare operand。
3. 两者之间没有其他写事务覆盖该字节范围。
4. 前置 Store 在 CAS 发起 DCache atomic request 前已经完成并变为可见。
5. CAS 本身不因非对齐、TLB、PMP、PMA、denied 或 corrupt 提前异常结束。

### 2.2 随机生成语义

主表随机到 `AMOCAS.W/D/Q` 后独立抽样一次：

```text
prefill_enable = random(0..99) < MEMBLOCK_AMO_CAS_PREFILL_EN_1_WT
```

未命中：

```text
保留当前随机 CAS，不生成前置 Store。
```

命中且预约成功：

```text
当前 CAS UID 改成第一条前置 Store；
CAS.Q 再预约一条高 64-bit Store；
从最后一条前置 Store 开始随机距离并预约未来 CAS；
未来预约位置不再执行普通随机生成，而是恢复原 CAS。
```

命中但主表尾部没有足够位置：

```text
整体取消本次 prefill 计划，当前 UID 保留原 CAS；
不得只生成 Store、不得生成半个 CAS.Q prefill group。
```

## 3. 参数方案

### 3.1 新增参数

```text
MEMBLOCK_AMO_CAS_PREFILL_EN_1_WT=0
MEMBLOCK_AMO_CAS_PREFILL_DIST_1_5_WT=1
MEMBLOCK_AMO_CAS_PREFILL_DIST_6_10_WT=1
MEMBLOCK_AMO_CAS_PREFILL_DIST_11_30_WT=1
```

| 参数 | 默认值 | 约束 | 含义 |
| --- | ---: | --- | --- |
| `MEMBLOCK_AMO_CAS_PREFILL_EN_1_WT` | 0 | `0..100` | CAS 随机到后启用前置 Store 构造的百分比；关闭概率自动为 `100-value`。默认必须为 0，保持默认回归不变。 |
| `MEMBLOCK_AMO_CAS_PREFILL_DIST_1_5_WT` | 1 | 非负 | 选择距离 `1..5` 的权重，档内等概率。 |
| `MEMBLOCK_AMO_CAS_PREFILL_DIST_6_10_WT` | 1 | 非负 | 选择距离 `6..10` 的权重，档内等概率。 |
| `MEMBLOCK_AMO_CAS_PREFILL_DIST_11_30_WT` | 1 | 非负 | 选择距离 `11..30` 的权重，档内等概率。 |

用户原始区间中的 `[6:10]` 和 `[10:30]` 在 10 处重叠。为避免 10 被重复加权，本计划将第三档固定为 `[11:30]`。

专用 AMO smoke cfg 建议使用：

```text
+MEMBLOCK_AMO_CAS_PREFILL_EN_1_WT=80
+MEMBLOCK_AMO_CAS_PREFILL_DIST_1_5_WT=1
+MEMBLOCK_AMO_CAS_PREFILL_DIST_6_10_WT=1
+MEMBLOCK_AMO_CAS_PREFILL_DIST_11_30_WT=1
```

定向全成功前置场景使用 `PREFILL_EN_1_WT=100`，自然 CAS 场景使用 0。

### 3.2 距离定义

本计划不复用 LR/SC 的 `lr_uid + dist + 1` 定义。CAS 距离严格定义为：

```text
initial_cas_uid = last_prefill_store_uid + selected_distance
```

所以：

- `distance=1`：CAS 紧跟最后一条前置 Store。
- `distance=5`：CAS UID 等于最后一条 Store UID 加 5。
- 发生预约冲突后只增加实际 CAS UID，不重新随机 bucket 和距离。

### 3.3 参数接入位置

需要同步修改：

1. `mem_ut/ver/ut/memblock/env/plus.sv`
2. `mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv`
3. `mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg`
4. `mem_ut/ver/ut/memblock/seq/plus_cfg/tc_dispatch_real_amo_smoke.cfg`
5. `mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md`

`seq_csr_common::init_from_plus()`负责快照；`validate_runtime_config()`负责范围和三档权重非全零检查；主表生成只通过 getter 读取，不直接访问 `plus::*`。

## 4. 指令构造

### 4.1 AMOCAS.W

假设原本 UID `N` 随机到 `AMOCAS.W`：

```text
address = A，A 按 4-byte 对齐
compare = C[31:0]
new     = V[31:0]

uid=N：SW C, 0(A)
uid=M：AMOCAS.W
       STA slot0 address=A
       STD slot0 compare=C
       STD slot1 new=V
```

Store 和 CAS 的高 32 bit 可以保存随机值，但成功关系只检查低 32 bit；为减少歧义，建议 override 中对 W 操作的高 32 bit 清零。

### 4.2 AMOCAS.D

```text
address = A，A 按 8-byte 对齐
compare = C[63:0]
new     = V[63:0]

uid=N：SD C, 0(A)
uid=M：AMOCAS.D
       STA slot0 address=A
       STD slot0 compare=C
       STD slot1 new=V
```

### 4.3 AMOCAS.Q

普通标量 Store 没有 128-bit 指令，必须使用两个 `SD`：

```text
address      = A，A 按 16-byte 对齐
compare_low  = C[63:0]
compare_high = C[127:64]
new_low      = V[63:0]
new_high     = V[127:64]

uid=N0：SD compare_low,  0(A)
uid=N1：SD compare_high, 8(A)
uid=M ：AMOCAS.Q
```

CAS.Q STD 映射固定为：

```text
slot0 = compare_low
slot1 = new_low
slot2 = compare_high
slot3 = new_high
```

STA 映射保持既有规则：

```text
STA slot0 = address A，低 64-bit destination
STA slot2 = address A，高 64-bit destination
```

CAS.Q 的距离以第二条 `SD` 的 UID 为基准。

## 5. 数据模型修改

### 5.1 为什么必须增加显式 STD payload

当前 `main_control_transaction` 没有独立 Store/CAS data 字段：

- 普通 Store STD 默认直接使用 `main_tr.src_0`。
- CAS STD 由 `issue_field_assigner::derive_atomic_std_data()`根据 `src_0 + uid + slot`临时派生。

如果仍使用该规则，前置 Store 和未来 CAS 使用不同 UID，无法证明：

```text
store_data == cas_compare_data
```

因此本功能不能只复制地址，必须显式冻结数据。

### 5.2 主表字段

在 `main_control_transaction` 增加统一的 STD slot override：

```systemverilog
bit        std_data_override_valid[4];
bit [63:0] std_data_override[4];
```

语义：

| 指令 | 使用方式 |
| --- | --- |
| 普通 Store | 使用 slot0 override。 |
| 非 CAS AMO | 可继续使用旧派生逻辑，本计划不强制 override。 |
| AMOCAS.W/D | slot0 compare，slot1 new。 |
| AMOCAS.Q | slot0 compare low，slot1 new low，slot2 compare high，slot3 new high。 |

字段必须在 constructor 中清零，并在手工/随机 transaction 校验中保证：

1. 普通 Store 最多只允许 slot0 valid。
2. CAS.W/D prefill owner 必须 slot0、slot1 valid，slot2、slot3 invalid。
3. CAS.Q prefill owner 必须四个 slot 全 valid。
4. 非 CAS 主表项不得意外继承其他 group 的 override。

不得使用 `uvm_object::copy()`或`clone()`保存原 CAS transaction。当前
`main_control_transaction`没有字段自动注册，也没有覆盖`do_copy()`；依赖默认 copy
会漏掉新增 override 和后续字段。预约结构必须显式保存 opcode、地址、四份数据和
成员 UID，未来 CAS 由这些冻结值重新构造。

### 5.3 issue 字段选择

新增统一 helper：

```text
issue_field_assigner::derive_std_data(main_tr, slot)
```

选择优先级：

```text
if std_data_override_valid[slot]:
    return std_data_override[slot]
else if atomic:
    return derive_atomic_std_data(main_tr, slot)
else:
    return main_tr.src_0
```

`assign_std_main_fields()`只调用该 helper，避免普通 Store 和 CAS 各自维护一套 override 分支。

## 6. 预约数据结构

### 6.1 CAS group 描述符

在 `memblock_dispatch_base_sequence` 增加：

```systemverilog
typedef struct {
    bit              valid;
    memblock_uid_t   origin_uid;
    memblock_uid_t   store_low_uid;
    bit              store_high_valid;
    memblock_uid_t   store_high_uid;
    memblock_uid_t   cas_uid;
    bit [8:0]        cas_fuOpType;
    bit [63:0]       address;
    bit [63:0]       compare_low;
    bit [63:0]       compare_high;
    bit [63:0]       new_low;
    bit [63:0]       new_high;
    int unsigned     selected_distance;
} memblock_cas_prefill_group_t;
```

当前 UID 的 low Store 不属于“未来预约”。`try_plan_cas_prefill()`成功后，它由当前
循环直接作为 replacement transaction 返回并写入主表，不能再放进 pending map。

未来预约表固定为：

```systemverilog
memblock_cas_prefill_group_t cas_prefill_store_high_by_uid[memblock_uid_t];
memblock_cas_prefill_group_t cas_prefill_owner_by_uid[memblock_uid_t];
```

另设只用于建表后静态审计的 ledger：

```systemverilog
memblock_cas_prefill_group_t cas_prefill_group_audit_q[$];
```

生命周期固定为：

1. low Store 当前拍直接生成，不进入 pending map。
2. Q high Store写入`cas_prefill_store_high_by_uid`，生成时精确消费并删除。
3. CAS owner写入`cas_prefill_owner_by_uid`，生成时精确消费并删除。
4. 完整group在正式提交时追加到`cas_prefill_group_audit_q`，不因成员生成而删除。
5. 建表结束要求两张pending map均为空；随后使用audit ledger检查完整group。

`reset_all_tables()`后的主表构造入口必须清空两张pending map和audit ledger，避免不同
testcase或重复建表残留。

### 6.2 统一 UID 占用判断

不要求第一版重构所有预约成一个 class，但必须新增统一只读 helper：

```text
uid_reserved_for_companion(uid,
                           csr_target,
                           sfence_target,
                           check_store_uid)
```

它至少检查：

1. `sc_reservation_by_uid`
2. `cas_prefill_store_high_by_uid`
3. `cas_prefill_owner_by_uid`
4. 当前 CSR target
5. 当前 SFence target
6. AUTO 模式末尾 `check_store`

除CSR/SFence现有的同target例外外，任何两类预约落到同一 UID 都必须在建表阶段
`uvm_fatal`，不能等到该 UID 被生成时再按 if/else 顺序静默覆盖。

CSR与SFence允许初始target相同，保持既有行为：该UID由CSR生成，SFence以相同base
UID重新预约。这个例外只适用于CSR/SFence彼此；任一control target都不得与SC、CAS
high Store或CAS owner重叠。

### 6.3 冲突后连续加一

新增：

```text
find_next_free_companion_uid(start_uid, table_limit, reserved_targets...)
```

算法固定为：

```text
candidate = start_uid
while candidate < table_limit:
    if candidate未被任何预约占用:
        return success, candidate
    candidate++
return failure
```

不能在冲突后重新随机距离。日志必须同时打印：

```text
origin uid
selected distance
initial candidate
final candidate
skipped conflict count
```

## 7. 主表生成流程

### 7.1 每个 UID 的生成优先级

AUTO 控制主表中按以下顺序处理：

```text
1. 当前已预约的 CSR/SFence control marker
2. CAS prefill high Store
3. CAS owner
4. paired SC
5. 普通随机事务
```

但正常设计中这些预约不得重叠；优先级只用于明确生成入口，不能作为冲突解决机制。

low Store只可能在第5步普通随机事务随机到CAS、且prefill规划成功后，当拍直接替换该
CAS生成；它从不作为未来预约进入上述优先级。

普通随机主表没有 control marker，顺序从 CAS prefill reservation 开始。

### 7.2 随机到 CAS 后的原子化规划

新增：

```text
try_plan_cas_prefill(original_cas,
                     table_limit,
                     current_control_targets,
                     output replacement_store)
```

流程：

```text
1. 确认 original_cas 是 AMOCAS.W/D/Q。
2. 抽样 PREFILL_EN_1_WT；未命中直接返回 false。
3. 从 original_cas 生成一份临时 group，不修改任何预约表。
4. 强制生成自然对齐、cacheable、无异常地址。
5. 随机 compare/new 数据并冻结。
6. W/D：store_low_uid = current uid。
7. Q：store_low_uid = current uid；从 current+1 连续扫描 high Store UID。
8. 随机一个距离 bucket 和具体 distance。
9. 从 last_store_uid + distance 连续扫描 CAS UID。
10. 只有 high Store（若需要）和 CAS UID 都找到后，才一次性写入预约表。
11. 创建当前 UID 的 replacement Store 并返回 true。
12. 任一步失败时不写任何预约表，original_cas 保持原样。
```

临时规划和正式提交必须分开，防止 CAS.Q 只预约到一半。

### 7.3 固定主表长度

第一版不动态扩展任何模式的主表：

- 原始 CAS UID 被替换为 Store，所以不增加槽位。
- 未来 Store/CAS 预约覆盖这些 UID 原本会生成的随机事务。
- AUTO 模式 `check_store` 仍固定在最后 UID，不能被扫描或覆盖。
- 扫描到 table limit 仍无空间时整体放弃本次 prefill。

两种模式的容量定义必须分别写死：

```text
普通随机模式：
    MEMBLOCK_MAIN_TRANS_NUM = N
    最终主表长度          = N
    companion table_limit = N
    可预约 UID            = [0, N)

AUTO控制模式：
    MEMBLOCK_MAIN_TRANS_NUM = N
    普通/control区域       = UID [0, N)
    check_store            = UID N
    最终主表长度           = N + 1
    companion table_limit  = N
    可预约 UID             = [0, N)
```

所以AUTO模式下`MEMBLOCK_MAIN_TRANS_NUM`表示普通区域长度，不是最终物理条目数；
所有CAS/SC/control companion扫描均以`table_limit=N`结束，绝不能把UID N当成空位。

### 7.4 与控制预约配合

当前 CSR/SFence 每次命中后会重新计算下一 target。为了避免它们在未来覆盖已经建立的 CAS group：

1. `reschedule_control_reservation()`增加 companion-reservation skip。
2. 它先按原 interval 得到 candidate，再调用相同的连续 `+1` helper。
3. 超出普通表范围时保持 `target_valid=0`，不得占用末尾 `check_store`。
4. 不再通过“control 命中时删除 paired SC”处理新冲突；新预约建立前必须找到真正空位。

第一版保留 CSR 优先于 SFence 的既有同target策略：CSR占用该UID，SFence从相同base
重新预约。两者重预约时都必须跳过 CAS 和 SC companion UID。

若 CSR/SFence 在重新预约时从初始 candidate 连续加一，最终越过
`normal_main_trans_num-1`，则本次 control target 直接失效；不得反向寻找空位，也不得
要求已建立的 CAS group 搬迁。

### 7.5 与 LR/SC 预约配合

`try_reserve_paired_sc()`至少需要：

1. 检查两张 CAS 未来预约表。
2. 初始 SC candidate 冲突时连续 `+1` 查找，而不是重新随机 16 次。
3. 达到 table limit 时放弃 paired SC，不影响 LR 本身。

这样所有“未来插入”使用同一种冲突语义，避免 CAS 与 paired SC 的结果取决于 helper 调用顺序。

## 8. 前置 Store 和 CAS transaction 构造

### 8.1 前置 Store

新增：

```text
make_cas_prefill_store_transaction(group, high_half, rob_key)
```

固定字段：

| CAS 类型 | low Store | high Store |
| --- | --- | --- |
| CAS.W | `SW`，地址 `A`，data=`compare_low[31:0]` | 无 |
| CAS.D | `SD`，地址 `A`，data=`compare_low` | 无 |
| CAS.Q | `SD`，地址 `A`，data=`compare_low` | `SD`，地址 `A+8`，data=`compare_high` |

建议直接使用：

```text
src_0 = final_vaddr
imm   = 0
```

避免重新拆分 base+imm 后引入符号扩展差异。

其他约束：

1. `op_class=STORE`，走普通 LSQ/SQ/SBuffer 生命周期。
2. `std_data_override_valid[0]=1`。
3. 不复制原 CAS 的 `fuType/lsq_flow/numLsElem`。
4. 重新调用 Store template，生成合法 Store 行为字段。
5. `send_pri/send_pri_std`使用普通 Store 策略，不强制继承 CAS 的 100。
6. 不进入普通地址复用随机，避免配对地址被修改。

### 8.2 未来 CAS owner

新增：

```text
make_reserved_cas_transaction(group, rob_key)
```

固定字段：

1. 恢复原 `AMOCAS.W/D/Q` opcode。
2. `op_class=AMO`，使用既有 atomic non-LSQ template。
3. `src_0=A`、`imm=0`、`vaddr=A`。
4. `amo_expect_misaligned=0`。
5. `amo_alignment_bytes`分别为 4、8、16。
6. `send_pri=100`、`send_pri_std=100`。
7. W/D 写入 slot0/1 override；Q 写入四个 slot override。
8. 不再次执行 CAS prefill 抽样，避免预约 CAS 再递归生成新 group。
9. 不进入 `apply_addr_reuse_window()`。

## 9. 地址保护

### 9.1 保护范围

group 正式建立后，到对应 CAS UID 生成为止，保护：

```text
CAS.W: [A, A+3]
CAS.D: [A, A+7]
CAS.Q: [A, A+15]
```

### 9.2 必须规避的写操作

中间随机事务如果可能修改重叠范围，必须重新生成地址：

1. 普通 Store，包括部分重叠。
2. SC。
3. 非 LR 的普通 AMO。
4. 其他 CAS。
5. `CBO.ZERO`覆盖的 cache line。

Load、LR和普通 prefetch不会修改内存，可以访问保护范围。

新随机出的 CAS 如果准备建立另一个 prefill group，也属于潜在写操作。它选择地址时
必须先避开所有尚未到达 owner UID 的已有 group 保护范围，不能先建立重叠 group 再
依赖后续普通事务修正。例如：

```text
group0：Store(A) uid=10，CAS(A) uid=30
group1：在uid=15随机到CAS
```

group1 不能再选择地址 A，否则 group1 的前置 Store 或 CAS 可能在 uid=30 前覆盖
group0 的 compare 数据。只有当已有 group 的 CAS UID 小于当前 UID、其保护窗口已经
结束后，地址才允许再次使用。

### 9.3 检查时机

保护检查必须位于普通事务全部地址修改之后：

```text
randomize_main_transaction()
-> apply_addr_reuse_window()
-> sync_boundary_profile_after_addr_reuse()
-> avoid_pending_cas_prefill_write_overlap()
-> validate_main_table_entry()
```

因为地址复用可能把一个原本不冲突的 Store 改到受保护地址。

`avoid_pending_cas_prefill_write_overlap()`只允许改地址，不允许把 AMO/CBO 改成普通 load/store。它应使用有界重试；达到上限后使用确定性的对齐步长寻找非重叠地址，仍失败才 `uvm_fatal`。

### 9.4 地址别名边界

第一版成功前置保证以主表最终虚拟字节范围和相同 TLB lookup key 为基础。为避免不同虚拟地址映射到同一物理地址破坏数据：

1. paired Store/CAS 必须使用完全相同的低地址 VPN/offset 语义；CAS.Q高位Store固定使用同一VPN内的`A+8`。
2. 地址保护至少对完全相同 VPN/offset 和同一地址复用来源生效。
3. 若启用 `MEMBLOCK_L2TLB_PPN_REUSE_EN` 或显式 routed-PPN alias 场景，CAS prefill 功能首版应 fail-fast 或被专项 cfg 关闭，不能宣称 compare 一定成功。
4. 后续若要与 PPN alias 场景组合，必须在 TLB 构建完成后按最终 PPN+offset 建立物理范围保护并支持主表/TLB 联合重建；不纳入本次最小实现。

## 10. 执行可见性与时序

### 10.1 为什么不需要 driver 等待 SBuffer empty

当前 atomic admission token 只有在：

```text
dispatch_progress.terminal_done_uid == atomic_uid
```

时才能完成。因此 CAS 成为 active atomic owner 前，全部更老 UID，包括所有前置 Store，已经 terminal。

Store 进入 SBuffer 后可能尚未立即写入 DCache array，但 RTL `AtomicsUnit` 在发起 atomic request 前会等待 SBuffer/Uncache内部条件满足。该等待属于 MemBlock 内部行为，不是测试框架 driver 的 issue 门控。

所以本计划不增加：

```text
driver wait sbuffer_empty
driver wait uncache_empty
driver wait dcache idle
```

### 10.2 保证边界

本计划保证测试框架制造“CAS 应成功比较的输入前置条件”，但没有 RM/checker 时不能仅凭 final WB 判断 CAS 实际成功，因为 CAS 成功和失败都属于 `exceptionVec==0`的正常完成。

## 11. 校验和 fail-fast

新增建表后静态检查：

```text
validate_all_cas_prefill_groups()
```

每个 group 必须满足：

1. UID 顺序为 `store_low < store_high（若有） < cas`。
2. W/D只有一条 Store，Q恰好两条 Store。
3. Store opcode与CAS宽度匹配。
4. CAS地址自然对齐，且所有成员地址关系正确。
5. Store data等于CAS compare slot。
6. CAS new data已经冻结，且不依赖发射时重新随机。
7. CAS UID与其他 SC/control/CAS预约不重叠；CSR/SFence彼此同target只允许按既有CSR优先规则消解。
8. AUTO 模式不占用末尾 `check_store`。
9. group区间内不存在其他已生成写事务与保护范围重叠。
10. 两张CAS pending reservation map在主表完成后都被恰好消费一次，不存在残留预约；low Store不进入pending map。
11. audit ledger中的每个group都能在最终主表找到low Store、可选high Store和CAS owner。

任何不满足都在 TLB 构建和真实 driver 启动前 `uvm_fatal`。

预约消费时还必须检查：

```text
实际生成UID == reservation记录UID
实际成员role == 该预约表对应role
实际group内容 == reservation冻结group内容
```

不能只按 UID 命中后就删除预约，否则错误的 map 写入会被静默掩盖。

## 12. 代码修改清单

| 文件 | 修改内容 |
| --- | --- |
| `env/plus.sv` | 增加4个参数定义和load。 |
| `seq/plus_cfg/default.cfg` | 增加默认值；使能默认0。 |
| `seq/plus_cfg/tc_dispatch_real_amo_smoke.cfg` | 设置建议成功前置权重和距离权重。 |
| `rule/plus_demo_migration_plan.md` | 登记新增参数。 |
| `seq/base_seq_help/seq_csr_common.sv` | 参数快照、getter、范围检查、距离权重检查。 |
| `seq/base_seq_help/main_control_transaction.sv` | 增加4槽STD override及初始化/校验。 |
| `seq/base_seq_help/issue_field_assigner.sv` | 增加统一STD data选择helper。 |
| `seq/base_seq_help/memblock_dispatch_base_sequence.sv` | group结构、预约表、距离随机、冲突扫描、Store/CAS构造、地址保护、建表后检查。 |
| `seq/base_seq/soft_test/soft_test_memblock_lsq_amo_admission_sequence.sv` | 增加参数、预约、冲突、slot data和尾部回滚software-only检查。 |
| `AI_DOC/plan/test_framework/plan/undo/memblock_amo_stimulus_only_adaptation_plan_20261022.md` | 增加本计划引用及CAS成功前置扩展说明。 |

不新增独立 real-drive sequence，不修改 agent xaction，不修改 AtomicsUnit RTL。

## 13. Coding顺序

### P0：参数和数据字段

- [ ] 增加4个plus参数及默认cfg。
- [ ] 增加`seq_csr_common`快照、getter和校验。
- [ ] 增加`std_data_override_valid/data[4]`。
- [ ] 让`issue_field_assigner`优先消费override。
- [ ] software-only验证普通Store和CAS slot数据保持稳定。

### P1：预约和transaction构造

- [ ] 增加CAS group、两张未来预约表和一份group audit ledger。
- [ ] 增加三档距离随机函数。
- [ ] 增加统一UID占用查询和连续`+1`扫描。
- [ ] 实现W/D前置Store构造。
- [ ] 实现Q低/高两条`SD`构造。
- [ ] 实现未来CAS恢复构造。
- [ ] 实现临时规划成功后一次性提交。
- [ ] 实现表尾失败整体回滚。
- [ ] 当前low Store直接生成，不进入pending reservation map。

### P2：与现有预约融合

- [ ] 普通随机表优先消费CAS预约。
- [ ] AUTO控制表优先消费CAS预约。
- [ ] CSR/SFence重预约跳过CAS/SC预约。
- [ ] paired SC预约跳过CAS预约并改为连续`+1`。
- [ ] 保持末尾`check_store`不可占用。
- [ ] 建表后检查所有预约被消费且无重叠。

### P3：地址保护

- [ ] 增加pending CAS写范围重叠判断。
- [ ] 在地址复用和boundary同步之后执行保护修正。
- [ ] 覆盖Store、SC、写型AMO、CAS和CBO.ZERO。
- [ ] 新CAS group选址时避开所有尚未结束的已有group保护范围。
- [ ] alias专项组合首版fail-fast或cfg禁用。
- [ ] 建表后验证group区间无破坏性写入。

### P4：测试和回归

- [ ] `PREFILL_EN=0`保持旧CAS生成行为。
- [ ] `PREFILL_EN=100`时每个有空间的CAS都形成完整group。
- [ ] CAS.W验证`SW data == STD slot0[31:0]`。
- [ ] CAS.D验证`SD data == STD slot0`。
- [ ] CAS.Q验证两个`SD`分别匹配slot0/slot2。
- [ ] 验证CAS new数据匹配slot1/slot3且发射重试不变化。
- [ ] 验证三个距离bucket边界1、5、6、10、11、30。
- [ ] 验证SC、CAS、CSR、SFence冲突时连续`+1`。
- [ ] 验证扫描到表尾后整体回滚，无孤立Store和残留reservation。
- [ ] 验证地址复用不能覆盖pending compare数据。
- [ ] 编译AMO smoke。
- [ ] 运行最小CAS.W/D/Q real-dispatch smoke。

## 14. 验收标准

### 14.1 静态生成验收

1. 所有新增参数可由plusarg覆盖，非法值在初始化时fail-fast。
2. 默认配置下新功能关闭，现有随机主表不变化。
3. 每个成功建立的W/D group恰好包含一条Store和一条CAS。
4. 每个成功建立的Q group恰好包含两条`SD`和一条CAS.Q。
5. CAS compare slot数据与前置Store数据逐位一致。
6. 冲突解决只连续增加候选UID，不重新随机距离。
7. 主表长度、UID连续性、ROB key连续性和末尾control位置保持合法。
8. 没有未消费预约、重复UID或半完成group。

### 14.2 真实接口验收

1. 前置Store走普通STA/STD、SQ、commit、SBuffer生命周期。
2. CAS仍走既有atomic non-LSQ admission和pipe0原子issue路径。
3. CAS.Q四个STD slot的override值在任意shuffle顺序下保持正确。
4. atomic barrier建立时前置Store均已terminal。
5. 新功能不增加driver侧SBuffer/Uncache等待。
6. CAS normal final WB可以正常commit/terminal，框架不会把比较失败误判为异常redirect。

### 14.3 非验收项

没有RM/checker时，以下不能作为本计划完成条件：

- 根据writeback数据证明CAS一定成功。
- 检查CAS后的最终内存值。
- 检查其他hart或外部一致性主设备是否修改同一cache line。

## 15. Review记录

### 15.1 第一轮Review结论

Review执行方式：独立subagent只读审查AMO worktree，没有修改代码和文档。

发现并关闭的Major：

- [x] **未来UID预约冲突没有统一入口。** 方案固定增加统一占用查询和连续`+1`扫描；SC、CAS、CSR、SFence和`check_store`共同参与冲突判断。
- [x] **主表没有独立Store/CAS data。** 方案固定增加4槽STD override，并禁止依赖默认`copy/clone`保存原CAS。
- [x] **中间写操作可能覆盖compare数据。** 方案固定增加pending字节范围保护，并补充并发CAS group选址互斥。
- [x] **固定表尾可能产生半完成group。** 方案固定使用临时规划、全部位置成功后一次性提交；任一步失败保留原CAS。

已确认不需要修改的运行期行为：

- [x] atomic admission已经要求全部更老UID terminal。
- [x] atomic barrier已经隔离CAS owner和年轻事务。
- [x] SBuffer/Uncache排空属于AtomicsUnit内部行为，不增加driver等待。
- [x] CAS.Q现有STA slot`0,2`和STD slot`0,1,2,3`映射正确。

第一轮Minor处理：

- [x] 距离区间固定为`[1:5]`、`[6:10]`、`[11:30]`，消除10重叠。
- [x] 按项目参数规则，默认cfg中的新使能保持0；专用AMO cfg建议80。该点优先于review中“全局默认80”的建议。
- [x] prefill group强制自然对齐、cacheable和无主动异常，不服从AMO非对齐权重。
- [x] “compare成功”限定为CAS真正执行比较时具备数据匹配前置条件；没有RM时不把最终成功结果作为本计划验收。

### 15.2 第二轮Review结论

- [x] 第一轮4个Major已转化为coding步骤。
- [x] 发现并修正AUTO模式`N/N+1`定义：AUTO companion limit固定为N，UID N只属于`check_store`。
- [x] 发现并修正low Store生命周期：当前拍直接替换，不进入未来pending map；增加audit ledger保存完整group。
- [x] 发现并修正CSR/SFence规则矛盾：只允许二者彼此同target并按CSR优先重预约，其他重叠一律fatal。

第二轮结论：上述3项修正前不可coding；v0.3已完成修正，进入第三轮最终复审。

### 15.3 第三轮Review目标

- [x] 普通模式固定`N`，AUTO模式固定`N+1`，所有companion的`table_limit=N`，不会占用`check_store UID=N`。
- [x] low Store当前拍直接生成；只有high Store和CAS owner进入pending map；audit ledger独立保留完整group，生命周期闭环。
- [x] CSR/SFence是唯一允许同target的例外，按CSR优先、SFence重新预约处理；其他预约重叠一律禁止。
- [x] 第一、二轮发现的全部Major blocker均已关闭。
- [x] 第三轮结论：**可coding**。

三轮review均为只读审查，subagent没有修改源码或文档；最终文档修改由主任务统一完成。
