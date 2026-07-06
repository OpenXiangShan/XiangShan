# 主表权限字段端到端一致性支持方案

状态：undo

创建日期：2026-06-26

关联源码：

- `mem_ut/ver/ut/memblock/seq/base_seq/main_control_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_tlb_entry.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/tlb_map_builder.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_l2tlb_base_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/mem_base_sequence.sv`
- `mem_ut/ver/ut/memblock/env/plus.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`
- `mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg`

## 1. 背景

当前 `main_control_transaction` 已有以下字段：

```systemverilog
rand bit                 tlbAF;
rand bit                 tlbPF;
rand bit                 tlbGPF;
rand bit [1:0]           PBMT;
rand bit                 pmaAF;
rand bit                 corrupt;
rand bit                 denied;
```

但当前代码中这些字段没有形成端到端一致性控制：

- `tlbAF/tlbPF/tlbGPF/PBMT/pmaAF` 没有接入 L2TLB entry builder；L2TLB response 只从 `memblock_tlb_entry` 取值。
- `corrupt/denied` 没有接入 DCache/SBuffer memory responder；responder 当前根据 memory model 地址范围自行计算。
- 主表生成后没有检查同一 VPN 或同一 VA 的权限字段是否一致。

因此开启这些字段的随机或手工配置后，当前无法保证“主表字段值”和 DUT 实际收到的 responder response 保持一致。

## 2. 目标

第一版目标是让主表权限字段在受控开关下真正生效：

1. 主表总权限开关打开时，启用主表权限一致性检查和必要的主表派生字段回填。
2. 主表 TLB 权限开关打开时，L2TLB responder 建表使用主表 `tlbAF/tlbPF/tlbGPF/PBMT` 控制 entry。
3. 主表 DCache 权限开关打开时，memory responder 使用主表 `corrupt/denied` 控制 DCache/SBuffer response。
4. 默认所有新开关为 0，不改变现有 `tc_sanity` 和默认 smoke 行为。
5. `pmaAF` 第一版保留字段但暂不支持端到端控制；如果配置为 1，需要给出明确 warning 或 fatal 策略。

## 3. 参数方案

### 3.1 新增 3 个正式控制参数

新增公共测试框架 plus 参数：

| 参数 | 默认值 | 含义 |
|---|---:|---|
| `MEMBLOCK_MAIN_PERMISSION_EN` | 0 | 主表权限总开关。为 0 时不做主表权限一致性检查，也不使用主表字段覆盖 responder。 |
| `MEMBLOCK_MAIN_TLB_PERMISSION_EN` | 0 | TLB 子开关。必须同时满足总开关为 1 才允许主表 TLB 字段控制 L2TLB entry。 |
| `MEMBLOCK_MAIN_DCACHE_PERMISSION_EN` | 0 | DCache/SBuffer 子开关。必须同时满足总开关为 1 才允许主表 `corrupt/denied` 控制 memory responder。 |

落点：

- `mem_ut/ver/ut/memblock/env/plus.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`
- `mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg`

### 3.2 Debug hit-check 参数

用户需求中提到“hit 命中已有 TLB 表项时，如果 plus 中 debug 参数开启，需要比对 hit entry 和主表控制项”。当前没有通用 debug 参数适合承载这个语义。

本方案建议新增一个诊断参数：

| 参数 | 默认值 | 含义 |
|---|---:|---|
| `MEMBLOCK_MAIN_PERMISSION_DEBUG_CHECK_EN` | 0 | 只做一致性诊断。TLB hit 已有 entry 时，按当前 req 对应 VPN 遍历 fired active uid 索引查主表控制项并和 hit entry 比对，不一致时报 `uvm_error`。 |

如果严格要求只新增 3 个参数，则该 debug check 可并入 `MEMBLOCK_MAIN_PERMISSION_EN && MEMBLOCK_MAIN_TLB_PERMISSION_EN`，但这样无法独立关闭 hit 诊断。

## 4. 主表字段扩展

### 4.1 新增 `ppn/pa` 字段

在 `main_control_transaction` 中新增：

```systemverilog
bit [63:0] ppn;
bit [63:0] pa;
bit        main_perm_pa_valid;
```

字段含义：

- `ppn`：只在 `MEMBLOCK_MAIN_PERMISSION_EN=1` 且 TLB responder 已为匹配 VPN 生成/命中 entry 后回填。
- `pa`：由 `ppn` 与主表虚拟地址 page offset 组合生成，只服务 DCache/SBuffer permission match。
- `main_perm_pa_valid`：标识 `ppn/pa` 是否已经由 TLB entry 回填，避免 DCache responder 误用默认 0。

### 4.2 `pmaAF` 第一版处理

`pmaAF` 暂不支持。建议：

- 默认继续为 0。
- 当 `MEMBLOCK_MAIN_PERMISSION_EN=1` 且任意主表项 `pmaAF=1` 时，第一版直接 `uvm_fatal`，提示 `pmaAF` 尚未接入 L2TLB response 控制。
- 后续如需支持，可把 `pmaAF` 纳入 TLB entry 覆盖和一致性检查。

## 5. 有效 uid 查询范围

需求描述中的“commit 之后到已经发射的 uid 范围”需要在代码中定义清楚。

第一版改为维护 **已经发射且当前动态实例仍有效的 uid 索引**，不再在每次 TLB/DCache 权限查询时扫描 `[success_prefix_uid, main_trans_num)` 并反复读取 status。

### 5.1 新增 fired active uid 运行期索引

在 `common_data_transaction` 中新增：

```systemverilog
// 已经至少一个 target 成功 fire，且当前动态实例仍允许参与主表权限匹配的 uid 集合。
bit fired_uid_active[memblock_uid_t];
// fired uid 的遍历顺序队列。允许保留旧 uid 残留，查询时必须用 fired_uid_active 过滤。
memblock_uid_t fired_uid_q[$];
```

字段含义：

- `fired_uid_active[uid]=1`：该 uid 当前动态实例已经至少一个 issue target 成功发射，且尚未被当前测试框架认为完成或清理。
- `fired_uid_q`：保存 fired uid 的遍历顺序。为避免频繁从 queue 中间删除，允许保留旧 uid 残留；查询时只信任 `fired_uid_active`。

采用该结构后，主表权限查询范围为：

```text
遍历 fired_uid_q；
只处理 fired_uid_active.exists(uid) && fired_uid_active[uid] 为 1 的 uid；
再按 VPN 或 PA 匹配主表 transaction。
```

原因：

- TLB/DCache 权限匹配只需要关注已经真实发射到 DUT 流水线的 uid。
- `mark_issue_fire()` / `mark_issue_fire_already_accepted()` 是成功 fire 的集中入口，适合维护该索引。
- 查询时遍历 fired uid 队列，比每次从 `success_prefix_uid` 扫到 `main_trans_num` 再读取 status 更直接。
- `fired_uid_active` 作为有效位，可以接受 queue 中存在旧 uid 残留，避免中间删除带来的实现复杂度。

### 5.2 维护函数

在 `common_data_transaction` 中新增：

```systemverilog
extern function void mark_uid_fired_active(input memblock_uid_t uid);
extern function void clear_uid_fired_active(input memblock_uid_t uid);
extern function bit is_uid_fired_active(input memblock_uid_t uid);
```

源码级伪代码：

```text
mark_uid_fired_active(uid):
  check_uid(uid)
  if !fired_uid_active.exists(uid) || !fired_uid_active[uid]:
    fired_uid_q.push_back(uid)
  fired_uid_active[uid] = 1

clear_uid_fired_active(uid):
  check_uid(uid)
  fired_uid_active[uid] = 0

is_uid_fired_active(uid):
  if !is_valid_uid(uid):
    return 0
  return fired_uid_active.exists(uid) && fired_uid_active[uid]
```

中文文字伪代码：

`mark_uid_fired_active` 在 uid 的某个 issue target 成功 fire 后调用。函数先检查 uid 合法性，如果该 uid 之前没有处于 fired active 状态，则把 uid 追加到 `fired_uid_q`，用于后续按发射顺序遍历；随后置 `fired_uid_active[uid]=1`。`clear_uid_fired_active` 只清有效位，不从 `fired_uid_q` 中间删除 uid，允许 queue 保留旧残留。`is_uid_fired_active` 是查询侧唯一有效性判断入口，后续 TLB/DCache 权限匹配必须通过它过滤 `fired_uid_q` 中的旧 uid。

### 5.3 维护点

确定维护点：

- `issue_queue_scheduler::mark_issue_fire()` 成功设置 target dispatched 后调用 `data.mark_uid_fired_active(item.uid)`。
- `issue_queue_scheduler::mark_issue_fire_already_accepted()` 成功设置 target dispatched 后调用 `data.mark_uid_fired_active(item.uid)`。
- `common_data_transaction::reset_all_tables()` 清空 `fired_uid_q` 和 `fired_uid_active`。
- normal pass retire 结束当前 active 实例时调用 `clear_uid_fired_active(uid)`。
- fault/exception retire 结束当前 active 实例时调用 `clear_uid_fired_active(uid)`；即使该 uid 最终是 `success=0`，也不能继续参与主表权限匹配。
- redirect/reissue 清理旧动态实例时调用 `clear_uid_fired_active(uid)`，等待同一 uid 后续重新 admission/issue/fire 时再重新置位。
- replay 后同一 uid 再次 fire 时继续调用 `mark_uid_fired_active(uid)`，但该函数必须保持幂等：如果 `fired_uid_active[uid]` 已经为 1，不重复 push 到 `fired_uid_q`，也不刷新遍历顺序。

实现落点说明：

- redirect/reissue 清理时，`clear_uid_fired_active(uid)` 的具体调用点可在 coding 阶段放在 `clear_uid_dispatch_result()` 或 `prepare_uid_for_redirect_reissue()` 外层，但语义必须满足：旧动态实例被清理后 `is_uid_fired_active(uid)` 返回 0。

本方案把 replay 重复 fire 语义固化为 uid 级幂等记录；redirect/reissue 清理点不再作为未决功能，只保留具体函数落点由 coding 阶段按调用覆盖面选择。

replay 重复 fire 文字伪代码：

```text
mark_uid_fired_active(uid):
  如果 uid 当前没有 fired active：
    将 uid push 到 fired_uid_q。
    设置 fired_uid_active[uid]=1。
  如果 uid 当前已经 fired active：
    只保持 fired_uid_active[uid]=1。
    不重复 push。
    不移动 uid 在 fired_uid_q 中的位置。

replay 后再次 fire：
  调用 mark_uid_fired_active(uid)。
  因为 uid 已经 fired active：
    fired_uid_q 不新增重复项。
    fired_uid_q 遍历顺序不刷新。
```

说明：TLB/DCache 主表权限匹配只需要知道“该 uid 当前动态实例是否已经真实进入 DUT 流水线”，不需要知道 LOAD/STA/STD 哪个 target 最近一次 fire。replay 是同一 uid 的 target 级重发，不代表产生新的主表 owner；刷新队列顺序反而会让 canonical owner 随 replay 时机变化，降低 debug 稳定性。

### 5.4 与 terminal_done/retire/reissue 的交互约束

本方案拥有 `fired_uid_active`、`fired_uid_q` 和对应维护函数的具体实现；terminal_done 完成前缀方案只需要在已存在该能力时调用本方案提供的清理 hook，不应自行新增这些字段或函数。

生命周期约束：

```text
normal pass retire：
  当前 active 实例结束；
  清 fired_uid_active。

fault/exception retire：
  当前 active 实例结束；
  即使 success=0，也清 fired_uid_active；
  fault/exception 结果保留给最终 checker，不再作为 fired active owner 参与权限匹配。

redirect/reissue 清动态实例：
  旧动态实例结束；
  清 fired_uid_active；
  后续同一 uid 重新 fire 时由 mark_uid_fired_active 重新置位。

replay：
  默认不清 fired_uid_active；
  除非该 replay 路径实际清除并重建整个动态实例。
```

如果 terminal_done 重构已经落地，`terminal_done=1` 表示 uid 生命周期已经进入终态；对应 retire 路径在释放 active map、issue queue 残留或执行 reissue 清理时，必须同步触发本方案的 fired active 清理 hook。清理依据是 active 实例生命周期结束，不是 `success` 值，因此 `success=0 && terminal_done=1` 的 fault 终态也必须清理。

## 6. 主表生成后一致性检查

### 6.1 检查规则

当 `MEMBLOCK_MAIN_PERMISSION_EN=1`：

1. 检查相同 VPN 的主表项，其 `tlbAF/tlbPF/tlbGPF/PBMT` 必须一致。
2. 检查相同 VA 的主表项，其 `corrupt/denied` 必须一致。
3. `pmaAF=1` 第一版不支持，直接 fatal 或按本方案最终 coding 策略处理。

手动表模式：

- 如果发现相同 VPN 但 TLB 权限不一致，直接 `uvm_fatal`。
- 如果发现相同 VA 但 DCache 权限不一致，直接 `uvm_fatal`。
- fatal 信息需要指出当前 uid、冲突 uid、VPN/VA 和两边字段值。

非手动随机表模式：

- 如果发现相同 VPN 但 TLB 权限不一致，以 uid 最小的主表项为 canonical，把后续 uid 的 `tlbAF/tlbPF/tlbGPF/PBMT` 改成 canonical。
- 如果发现相同 VA 但 DCache 权限不一致，以 uid 最小的主表项为 canonical，把后续 uid 的 `corrupt/denied` 改成 canonical。
- 修改后打印 `UVM_LOW` 或 `UVM_MEDIUM` 信息，方便 debug 确认自动归一化发生过。

### 6.2 修改点

在 `memblock_dispatch_base_sequence` 中新增：

```systemverilog
extern virtual function void check_main_permission_consistency(input bit manual_mode);
extern virtual function bit same_tlb_permission(input main_control_transaction a,
                                                input main_control_transaction b);
extern virtual function bit same_dcache_permission(input main_control_transaction a,
                                                   input main_control_transaction b);
extern virtual function void copy_tlb_permission(input main_control_transaction dst,
                                                 input main_control_transaction src);
extern virtual function void copy_dcache_permission(input main_control_transaction dst,
                                                    input main_control_transaction src);
```

调用点：

- `build_random_main_table()`：`init_status_for_main_table()` 前调用，`manual_mode=0`。
- `import_manual_main_table()`：`init_status_for_main_table()` 前调用，`manual_mode=1`。

### 6.3 源码级伪代码

```text
check_main_permission_consistency(manual_mode):
  if !seq_csr_common::get_main_permission_en():
    return

  vpn_owner.delete()
  va_owner.delete()

  for uid in [0, data.main_trans_num):
    tr = data.get_main_transaction(uid)

    if tr.pmaAF:
      fatal("pmaAF is not supported in main permission mode")

    vpn = tr.vaddr[63:12]
    va  = tr.vaddr

    if vpn_owner.exists(vpn):
      owner = data.get_main_transaction(vpn_owner[vpn])
      if !same_tlb_permission(owner, tr):
        if manual_mode:
          fatal("same vpn has inconsistent tlb permission")
        else:
          copy_tlb_permission(tr, owner)
    else:
      vpn_owner[vpn] = uid

    if va_owner.exists(va):
      owner = data.get_main_transaction(va_owner[va])
      if !same_dcache_permission(owner, tr):
        if manual_mode:
          fatal("same va has inconsistent dcache permission")
        else:
          copy_dcache_permission(tr, owner)
    else:
      va_owner[va] = uid
```

中文文字伪代码：

该函数负责在主表构建完成后统一检查主表权限字段是否自洽。函数先读取主表权限总开关，如果总开关没有打开，直接返回，不改变主表也不做检查。总开关打开后，函数按 uid 顺序遍历主表，用两个临时表分别记录每个 VPN 和每个 VA 第一次出现的 owner uid。遇到 `pmaAF=1` 时，因为第一版不支持该字段端到端控制，直接报错阻止后续仿真继续。遇到重复 VPN 时，调用 `same_tlb_permission` 比较 owner 和当前 uid 的 `tlbAF/tlbPF/tlbGPF/PBMT`；手动表模式下不一致直接 fatal，随机表模式下调用 `copy_tlb_permission` 把当前 uid 改成 owner 权限。遇到重复 VA 时，调用 `same_dcache_permission` 比较 `corrupt/denied`；手动表模式下不一致 fatal，随机表模式下调用 `copy_dcache_permission` 自动归一化。`same_*` helper 只负责字段比较，不修改状态；`copy_*` helper 只负责把 canonical 权限复制到当前 transaction。

## 7. L2TLB responder 接入主表权限

### 7.1 建表 miss 路径

当 `MEMBLOCK_MAIN_PERMISSION_EN=1 && MEMBLOCK_MAIN_TLB_PERMISSION_EN=1`：

1. L2TLB sequence 收到 req 后仍按 `{vpn, s2xlate, runtime CSR asid/vmid}` 生成 key。
2. 如果 `tlb_entry_by_key` miss，建表后不使用随机 fault/PBMT 值作为最终控制项。
3. 遍历 fired active uid 索引，查找 VPN 匹配的主表项。
4. 提取 canonical uid 的 `tlbAF/tlbPF/tlbGPF/PBMT` 覆盖到 entry。
5. 将 entry 的 `ppn` 回填给 fired active uid 索引中所有 VPN 匹配的主表项，计算 `pa = {ppn, vaddr[11:0]}` 并置 `main_perm_pa_valid=1`。

注意：

- PTE `r/w/x/u/g/a/d/n/v` 仍由现有 TLB PTE 权重控制。本方案只接管 `tlbAF/tlbPF/tlbGPF/PBMT`。
- `pmaAF` 第一版不覆盖到 entry。
- 如果找不到匹配主表 uid，保留现有随机/默认 entry 行为，并报 `uvm_warning` 或 `uvm_error` 需要 coding 阶段按严格度确认。建议第一版在主表权限模式下找不到匹配时 `uvm_error`，但 response 仍发送，避免 responder 卡死。

### 7.2 hit 路径 debug check

当命中已有 entry：

- 默认不修改 entry，保持 TLB 表项稳定。
- 如果 `MEMBLOCK_MAIN_PERMISSION_DEBUG_CHECK_EN=1`，则遍历 fired active uid 索引查 VPN 匹配项，并比对 hit entry 的 `tlbAF/tlbPF/tlbGPF/PBMT` 是否和主表 canonical 值一致。
- 不一致时报 `uvm_error`，打印 key、entry 字段、owner uid 和主表字段。
- 同时可重新回填匹配 uid 的 `ppn/pa`，保证后续 DCache 权限匹配有 PA。

### 7.3 修改点

在 `common_data_transaction` 中新增：

```systemverilog
extern function bit find_main_permission_tlb_owner(input bit [51:0] vpn,
                                                   output memblock_uid_t owner_uid,
                                                   output main_control_transaction owner_tr);
extern function int unsigned apply_main_tlb_permission_to_entry(input memblock_tlb_lookup_key_t key,
                                                                input memblock_tlb_entry entry);
extern function void check_main_tlb_permission_hit(input memblock_tlb_lookup_key_t key,
                                                   input memblock_tlb_entry entry);
extern function int unsigned update_main_pa_by_tlb_entry(input memblock_tlb_lookup_key_t key,
                                                         input memblock_tlb_entry entry);
extern function void mark_uid_fired_active(input memblock_uid_t uid);
extern function void clear_uid_fired_active(input memblock_uid_t uid);
extern function bit is_uid_fired_active(input memblock_uid_t uid);
```

调整 `get_or_create_tlb_entry_by_req()`：

- miss 建表后调用 `apply_main_tlb_permission_to_entry()`。
- hit 时按 debug 开关调用 `check_main_tlb_permission_hit()`。
- hit/miss 均在主表权限总开关打开时调用 `update_main_pa_by_tlb_entry()`。

### 7.4 源码级伪代码

```text
get_or_create_tlb_entry_by_req(vpn, s2xlate, key, entry, created):
  key = make_tlb_key_by_req(vpn, s2xlate)

  if has_tlb_entry(key):
    entry = tlb_entry_by_key[key]
    entry.last_hit_cycle = current_cycle
    created = 0
    if main_permission_en && main_tlb_permission_en && main_permission_debug_check_en:
      check_main_tlb_permission_hit(key, entry)
    if main_permission_en:
      update_main_pa_by_tlb_entry(key, entry)
    return 1

  entry = build_tlb_entry_for_key(key)
  if main_permission_en && main_tlb_permission_en:
    apply_main_tlb_permission_to_entry(key, entry)
  insert_tlb_entry(key, entry)
  if main_permission_en:
    update_main_pa_by_tlb_entry(key, entry)
  created = 1
  return 1
```

中文文字伪代码：

该函数仍然是 L2TLB responder 的统一查表入口。函数先用 request 的 `vpn/s2xlate` 和 runtime CSR 构造 lookup key。命中已有 entry 时，不重建也不覆盖 entry，只更新时间戳并返回。如果主表权限、TLB 子开关和 debug check 都打开，则调用 `check_main_tlb_permission_hit` 对比 hit entry 与主表 canonical 权限，发现不一致时报错；如果主表权限总开关打开，则调用 `update_main_pa_by_tlb_entry` 把 entry 的 PPN 回填到匹配主表项，保证后续 DCache responder 可以按 PA 找到主表权限。miss 时先沿用现有 builder 创建 entry；如果主表权限和 TLB 子开关打开，则调用 `apply_main_tlb_permission_to_entry` 用主表 `tlbAF/tlbPF/tlbGPF/PBMT` 覆盖 entry；随后插入 TLB 表，并按总开关回填主表 `ppn/pa`。`apply_main_tlb_permission_to_entry` 只修改 entry 的主表控制字段，不改变 PTE 随机权限位；`update_main_pa_by_tlb_entry` 只修改主表派生字段 `ppn/pa/main_perm_pa_valid`。

### 7.5 `apply_main_tlb_permission_to_entry()` 伪代码

```text
apply_main_tlb_permission_to_entry(key, entry):
  if entry == null:
    fatal

  if !find_main_permission_tlb_owner(key.vpn, owner_uid, owner_tr):
    error("no main table tlb permission owner")
    return 0

  entry.tlbAF  = owner_tr.tlbAF
  entry.tlbPF  = owner_tr.tlbPF
  entry.tlbGPF = owner_tr.tlbGPF
  entry.pbmt   = owner_tr.PBMT
  return 1
```

中文文字伪代码：

该函数负责把主表 TLB 控制字段应用到新建 TLB entry。函数先检查 entry 非空，避免 responder 后续使用空对象。然后调用 `find_main_permission_tlb_owner` 在 `fired_uid_q` 中寻找 VPN 匹配的 canonical 主表项；该 helper 的职责是按 fired uid 顺序遍历队列，并用 `is_uid_fired_active` 过滤旧 uid 残留，只返回当前仍有效的 fired uid 和 transaction。如果找不到 owner，说明当前 L2TLB request 没有可追溯主表来源，函数报错并返回 0，但不阻塞 responder 继续返回 response。如果找到 owner，函数将 owner 的 `tlbAF/tlbPF/tlbGPF/PBMT` 写入 entry 的 `tlbAF/tlbPF/tlbGPF/pbmt`，使后续 `fill_dtlb_resp_from_entry` 发出的 response 与主表控制字段一致。

## 8. Memory responder 接入主表 `corrupt/denied`

### 8.1 控制规则

当 `MEMBLOCK_MAIN_PERMISSION_EN=1 && MEMBLOCK_MAIN_DCACHE_PERMISSION_EN=1`：

1. DCache/SBuffer responder 收到 DUT memory request 后，仍先按现有 memory model 计算 load data、基础 corrupt/denied。
2. 在填 response 前，根据 request PA 查询主表中已回填 `main_perm_pa_valid=1` 且 `pa` 匹配的 transaction。
3. 如果匹配成功，用主表 `corrupt/denied` 覆盖 response transaction 的 `denied/corrupt`。
4. 如果匹配失败，保留现有 memory model 结果，并报 `uvm_warning` 或 `uvm_error`。建议第一版报 `uvm_warning`，因为 DCache 也可能访问非主表来源地址。

### 8.2 修改点

在 `common_data_transaction` 中新增：

```systemverilog
extern function bit find_main_permission_dcache_owner(input bit [63:0] pa,
                                                      output memblock_uid_t owner_uid,
                                                      output main_control_transaction owner_tr);
extern function bit override_dcache_error_from_main(input bit [63:0] pa,
                                                    output bit corrupt,
                                                    output bit denied);
```

在 `mem_base_sequence.sv` 中调整：

- `dcache_mem_access_xaction()` 填 response 前调用公共 override helper。
- `sbuffer_mem_access_xaction()` 填 response 前调用公共 override helper。

### 8.3 源码级伪代码

```text
override_dcache_error_from_main(pa, corrupt, denied):
  if !(main_permission_en && main_dcache_permission_en):
    return 0

  if !find_main_permission_dcache_owner(pa, owner_uid, owner_tr):
    warning("no main table dcache permission owner")
    return 0

  corrupt = owner_tr.corrupt
  denied  = owner_tr.denied
  return 1
```

中文文字伪代码：

该函数负责在 memory responder 生成 response 前，用主表 `corrupt/denied` 覆盖现有 memory model 错误结果。函数先判断主表权限总开关和 DCache 子开关是否同时打开，未打开时直接返回 0，表示调用方继续使用原有 memory model 结果。开关打开后，函数调用 `find_main_permission_dcache_owner` 用物理地址 PA 查找主表项；该 helper 遍历 `fired_uid_q`，通过 `is_uid_fired_active` 过滤旧 uid 残留，只匹配 `main_perm_pa_valid=1` 且 `pa` 完全一致的 transaction。如果找不到 owner，函数打印 warning 并返回 0，调用方保留原有 `corrupt/denied`。如果找到 owner，函数把 owner 的 `corrupt/denied` 赋给输出参数并返回 1，调用方后续 response xaction 会使用主表控制值。

### 8.4 DCache/SBuffer response 伪代码

```text
dcache_mem_access_xaction(req_xact, rsp_xact):
  call dcache_mem_access_task(...)
  if data.override_dcache_error_from_main(req_pa, main_corrupt, main_denied):
    corrupt = main_corrupt
    denied  = main_denied
    if corrupt || denied:
      load_data = 0
  fill dcache response xaction with denied/corrupt/load_data
```

中文文字伪代码：

该逻辑仍然先执行现有 DCache memory model，得到基础 `load_data/corrupt/denied`。如果主表权限和 DCache 子开关打开，调用 `override_dcache_error_from_main` 根据 request PA 查主表权限；该 helper 返回 1 时说明找到匹配主表项，当前 response 的 `corrupt/denied` 被主表字段覆盖。如果覆盖后存在错误，load data 需要清 0，保持现有 error response 行为。如果 helper 返回 0，说明开关未开或没有匹配主表项，调用方保持原有 memory model 结果。最后按现有流程把 `denied/corrupt/load_data` 写入 D channel response。

## 9. 主表字段随机/手工配置关系

本方案只解决“字段生效和一致性”。字段如何随机出 1 或 PBMT 非 0，由已有或待实现的主表异常属性权重方案负责：

- `AI_DOC/plan/test_framework/plan/undo/main_table_exception_attr_weight_plan_20260625.md`

如果该方案尚未 coding，则本方案 coding 后默认仍可能所有字段为 0，但手工主表可以显式设置字段验证端到端覆盖。

## 10. 关键风险与约束

### 10.1 VPN 匹配与 s2xlate/ASID/VMID

主表一致性检查按 VPN 检查 TLB 权限，但 L2TLB entry key 实际包含 `vpn/asid/vmid/s2xlate`。如果两个 uid VPN 相同但 ASID/VMID 不同，硬件语义上可以对应不同翻译上下文。用户需求指定“相同 VPN 权限一致”，第一版按该要求实现。

后续如果需要区分进程上下文，应把主表一致性 key 扩展为 `{vpn, expected_s2xlate, asid, vmid}`，但这需要主表生成阶段能稳定拿到 CSR snapshot，不建议混入第一版。

### 10.2 fired uid 队列旧残留

`fired_uid_q` 允许保留旧 uid 残留，不要求在 retire、redirect 或 replay 清理时从 queue 中间删除。

采用方式：

```text
遍历 fired_uid_q；
每个 uid 先调用 is_uid_fired_active(uid)；
只有 active=1 的 uid 才能参与 TLB/DCache 主表权限匹配。
```

这样可以避免频繁从 queue 中间删除元素，降低实现复杂度。后续如果发现 `fired_uid_q` 在长时间 replay/redirect 压力下膨胀明显，再补充低频压缩函数；第一版不做压缩。

### 10.3 PA 回填时机与 DCache 请求时序

按真实 DUT 时序，DCache request 需要在 L2TLB response 返回并获得 translated PA 后才能产生。因此“DCache 请求早于 PA 回填”不作为本方案主要风险。

代码实现需要保证：

```text
L2TLB responder 在 fill/drive response 的同一处理路径内调用 update_main_pa_by_tlb_entry；
主表 pa/main_perm_pa_valid 先于或同步于 response drive 更新；
DCache responder 后续按 PA 查询时，应能看到已回填的主表 PA。
```

### 10.4 replay 重复 fire 语义

第一版不引入 target 级 fired 记录，也不在 replay 再次 fire 时刷新 `fired_uid_q` 顺序。`mark_uid_fired_active(uid)` 必须保持 uid 级幂等：同一 uid 当前动态实例已经处于 fired active 时，重复 fire 只保持有效位为 1，不新增 queue 元素。

源码级伪代码：

```text
mark_uid_fired_active(uid):
  check_uid(uid)
  if !fired_uid_active.exists(uid) || !fired_uid_active[uid]:
    fired_uid_q.push_back(uid)
  fired_uid_active[uid] = 1

replay_refire(uid):
  mark_uid_fired_active(uid)
  // 如果 uid 已 fired active，不重复 push，不刷新顺序。
```

中文文字伪代码：

`mark_uid_fired_active` 是 uid 级 fired active 入口。首次 fire 时，该函数把 uid 追加到 `fired_uid_q` 并置有效位；replay 后同一 uid 再次 fire 时，因为有效位已经为 1，只保持有效位，不重复入队，也不把 uid 移动到队尾。TLB/DCache 主表权限匹配只关心该 uid 当前动态实例是否已经进入 DUT 流水线，不关心哪个 target 最近 fire，因此不需要 LOAD/STA/STD target 级 fired bit。

采用该策略的原因：

- replay 是同一 uid 的 target 重发，不代表新的主表权限 owner。
- 刷新 fired 队列顺序会让 canonical owner 随 replay 时机变化，不利于 debug 稳定性。
- target 级 fired bit 第一版没有使用方，会增加状态清理和 redirect/replay 一致性维护成本。
- 后续如果需要做 target 级 debug，可新增只受 debug 开关控制的统计字段，不参与主表权限匹配主路径。

### 10.5 redirect/reissue 清理点实现约束

redirect/reissue 会清除旧动态实例并等待同 uid 重新 admission/issue/fire。`clear_uid_fired_active(uid)` 必须在旧实例被清理时调用。具体函数落点由 coding 阶段按覆盖面选择，但不再作为未决功能：

- 可以放在 `clear_uid_dispatch_result(uid)` 内：覆盖面更大，任何清 dispatch 结果都会清 fired active。
- 也可以放在 `prepare_uid_for_redirect_reissue(uid, redirect)` 外层：语义更窄，只针对 redirect/reissue。

无论最终选择哪个落点，都必须满足：

```text
旧动态实例被 terminal_done retire、fault retire 或 redirect/reissue 清理后：
  is_uid_fired_active(uid) 返回 0。

同一 uid 重新 admission/issue/fire 后：
  mark_uid_fired_active(uid) 重新置位；
  fired_uid_q 可以保留旧残留，但查询必须只信任 fired_uid_active。
```

### 10.6 DCache 非主表来源访问与 PA 匹配粒度

DCache 请求早于 PA 回填不是本方案预期风险。按真实 DUT 时序，DCache request 需要在 L2TLB response 返回并获得 translated PA 后才能产生；测试框架实现必须保证 L2TLB response drive 前或同一处理路径内完成 `update_main_pa_by_tlb_entry()`。

剩余需要关注的是：

1. DCache 请求可能不是当前主表 uid 直接触发，例如 DUT 内部 refill、probe、flush、prefetch 或其他非当前主表 transaction 触发的访问。
2. DCache 请求地址粒度可能是 cache line/beat 地址，而主表 `pa` 是具体访存地址。第一版先按精确 PA 匹配；如果后续发现漏匹配，再扩展为 line base 或 byte mask 覆盖范围匹配。
3. 如果 `MEMBLOCK_MAIN_DCACHE_PERMISSION_EN=1` 且查不到 PA owner，默认不阻塞 response，保留 memory model 结果并打印包含 PA/source/opcode 的 warning，便于确认是否属于非主表访问或匹配粒度问题。

### 10.7 多 uid 相同 PA

如果多个 uid 回填后 PA 相同，且主表生成后已检查相同 VA 的 `corrupt/denied` 一致，通常不会冲突。但不同 VA 可能映射到同一 PA，这种 alias 第一版不额外检查。

建议后续增强：

- 在 `update_main_pa_by_tlb_entry()` 回填后检查相同 PA 的 `corrupt/denied` 是否一致。
- 如果不一致，手动表 fatal，随机表按最小 uid 归一化。

### 10.8 `pmaAF`

`pmaAF` 与 PMA/访问 fault 语义相关，不能简单等价为 TLB entry `af`。第一版暂不支持，避免把 PMA fault 和 TLB fault 混在一起。

## 11. 验证计划

静态检查：

```bash
git diff --check -- mem_ut/ver/ut/memblock AI_DOC
rg -n "MEMBLOCK_MAIN_PERMISSION|main_perm_pa|apply_main_tlb_permission|override_dcache_error_from_main" mem_ut/ver/ut/memblock AI_DOC
```

基础编译：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
```

默认行为验证：

```bash
make eda_run tc=tc_sanity mode=base_fun
```

定向验证建议：

1. 默认参数全部为 0，确认行为和当前一致。
2. 打开 `MEMBLOCK_MAIN_PERMISSION_EN=1 + MEMBLOCK_MAIN_TLB_PERMISSION_EN=1`，手工主表设置某个 VPN 的 `tlbPF=1/PBMT!=0`，确认 L2TLB response 对应字段来自主表。
3. 打开 hit debug check，构造已有 entry 与主表权限不一致，确认报 `uvm_error`。
4. 打开 `MEMBLOCK_MAIN_PERMISSION_EN=1 + MEMBLOCK_MAIN_DCACHE_PERMISSION_EN=1`，手工主表设置 `corrupt/denied`，确认 DCache/SBuffer response 字段被主表覆盖。
5. 手动表构造相同 VPN 不同 TLB 权限，确认 fatal 信息包含冲突 uid 和字段。
6. 随机表构造相同 VPN 不同 TLB 权限，确认自动归一化到最小 uid。

## 12. Coding Checklist

- [ ] 新增 plus 参数、默认 cfg 和 `seq_csr_common` getter。
- [ ] `main_control_transaction` 新增 `ppn/pa/main_perm_pa_valid`，补充中文注释。
- [ ] 主表构建后新增 `check_main_permission_consistency()` 调用。
- [ ] L2TLB miss 建表路径接入主表 TLB 权限覆盖。
- [ ] L2TLB hit debug check 接入主表 TLB 权限比对。
- [ ] L2TLB hit/miss 后回填匹配 uid 的 `ppn/pa`。
- [ ] DCache/SBuffer responder 接入 `override_dcache_error_from_main()`。
- [ ] `pmaAF=1` 在主表权限模式下给出明确不支持诊断。
- [ ] 同步 `AI_DOC/mem_ut_flow_doc/tlb_l2tlb_responder_flow.md`。
- [ ] 同步 `AI_DOC/mem_ut_flow_doc/dcache_sbuffer_memory_responder_flow.md`。
- [ ] 同步参数管理相关说明。
- [ ] 通过 `git diff --check`。
- [ ] 通过 `tc_sanity` 编译和默认仿真。

## 13. 执行前新增风险审查：transaction 级权限字段无法完整表达真实访问片段

本节是 2026-06-28 执行前审查新增内容。当前第 1 到第 12 章仍按“每条 `main_control_transaction` 只有一组 TLB/DCache 权限字段”设计，但结合 Scala 源码后，该抽象存在语义风险。因此本 plan 暂不应直接进入 coding，需要先按本节和第 14 章修正方案。

### 13.1 跨 4K 导致一条主表 transaction 对应两笔 TLB/VPN 请求

Scala 行为依据：

- `src/main/scala/xiangshan/cache/mmu/TLB.scala` 中注释说明，非对齐 load/store 跨页时会被拆成两笔 load，例如页尾一笔和下一页页头一笔。
- `src/main/scala/xiangshan/mem/pipeline/NewLoadUnit.scala` 中 `unalignTail` 注入逻辑会生成第二笔访问，且 `unalignTail.noQuery := false.B`，表示 tail 也需要重新查询 TLB。
- `src/main/scala/xiangshan/mem/pipeline/NewStoreUnit.scala` 也有 `cross4KPage` 与 unalign tail 相关路径。

风险：

```text
一条主表 transaction:
  只有一组 tlbAF/tlbPF/tlbGPF/PBMT。

真实 DUT:
  如果该访问跨 4K：
    head 访问一个 VPN；
    tail 访问另一个 VPN；
    两个 VPN 可以对应两个不同 TLB entry。

结果:
  单个 tlbAF/tlbPF/tlbGPF/PBMT 字段无法分别控制 head VPN 和 tail VPN。
```

中文文字伪代码：

当前 plan 把 TLB 权限绑定在 transaction 上。真实 DUT 在跨 4K 时会把一次访存拆成 head 和 tail 两笔 TLB 查询；head 与 tail 的 VPN 不同，可能需要不同 `tlbAF/tlbPF/tlbGPF/PBMT`。如果仍只使用主表一组字段，就只能让两笔 TLB 查询共用同一组权限，无法表达“第一页正常、第二页 fault”或“两页 PBMT 不同”等场景，也会让一致性检查错误地把跨页两笔请求压成同一语义。

### 13.2 DCache miss 可能由两笔 DCache 操作分别触发，两笔可能访问不同 cacheline

Scala 行为依据：

- `src/main/scala/xiangshan/mem/pipeline/NewLoadUnit.scala` 中注释说明，unalign 跨 16B bank 时需要 split into 2 operations on DCache。
- `src/main/scala/xiangshan/cache/dcache/loadpipe/LoadPipe.scala` 送 miss queue 时使用 `get_block_addr(s2_paddr)`，miss 粒度是 cacheline。
- `src/main/scala/xiangshan/cache/dcache/mainpipe/MissQueue.scala` 发送 L2 `AcquireBlock/AcquirePerm` 时 `lgSize = log2Up(cfg.blockBytes)`，当前默认 `blockBytes=64`，即一次 miss entry 向 L2 获取一条 64B cacheline。
- 如果两笔 DCache 操作落在同一 cacheline，MissQueue 可以按 block merge；如果落在不同 cacheline，则会形成两笔 L2 miss/acquire。

风险：

```text
一条主表 transaction:
  只有一组 corrupt/denied。

真实 DUT:
  如果该访问被拆成两笔 DCache 操作：
    head 可能访问 cacheline A；
    tail 可能访问 cacheline B；
    A 和 B 可能分别命中或 miss；
    miss 时分别从 L2/refill 路径获得 response。

结果:
  单个 corrupt/denied 字段无法分别控制两个 cacheline 或两个 memory response。
```

中文文字伪代码：

当前 plan 把 DCache 错误属性绑定在 transaction 上。真实 DUT 中一次非对齐访问可能先拆成两笔 DCache 操作；如果两笔落到不同 cacheline，每条 cacheline 都可能独立 miss/refill，也可能一条命中一条 miss。此时单个 `corrupt/denied` 只能表达“整条 transaction 都错误或都正常”，无法表达“head cacheline 正常、tail cacheline denied/corrupt”这类真实可发生场景。

### 13.3 TLB/DCache 命中会复用已有状态，后续同地址访问不会重新下发请求

风险：

```text
第一次访问地址 X:
  TLB/DCache miss；
  responder 建立 entry 或 cacheline；
  如果主表字段为无错误，则后续状态被填成正常。

第二次访问同一 VPN 或同一 cacheline:
  可能直接命中；
  不会再向 L2TLB responder 或 L2/memory responder 请求；
  第二条主表 transaction 即使设置 tlbPF/corrupt/denied，也可能没有机会生效。
```

中文文字伪代码：

当前 plan 假设 responder 每次收到请求时都可以按当前主表 transaction 字段覆盖 response。但真实 DUT 存在 TLB hit 和 DCache hit：第一次访问已经建立正常 entry/cacheline 后，后续相同 VPN 或相同 cacheline 可能直接命中，不再走 responder。因此后续 transaction 上标注的错误字段不一定有触发点，测试框架不能承诺“每条主表 transaction 的权限字段都会独立生效”。

### 13.4 当前一致性检查粒度不足

当前第 6 章检查：

- 相同 VPN 的 `tlbAF/tlbPF/tlbGPF/PBMT` 一致。
- 相同 VA 的 `corrupt/denied` 一致。

风险：

```text
TLB:
  跨 4K 时需要检查访问片段对应的 VPN，不只是原始 transaction 起始 VA 的 VPN。

DCache:
  DCache/L2 miss 粒度是 cacheline，不是完整 VA；
  两个不同 VA 可能落到同一 cacheline；
  同一 transaction 也可能覆盖两个 cacheline。

命中复用:
  已有 TLB entry 或 cacheline 的首个 owner 会影响后续访问；
  只按 transaction 自身字段检查，无法保证后续字段可见。
```

中文文字伪代码：

一致性检查应围绕“真实硬件会建立或命中的地址粒度”展开，而不是只围绕 transaction 起始地址。TLB 应至少按每个访问片段的 VPN 建 canonical 权限；DCache 应至少按每个访问片段覆盖的 cacheline 建 canonical 错误属性。否则主表看似字段一致，但实际 DUT 访问的第二个 VPN 或第二条 cacheline 没有被正确建模。

## 14. 推荐修正方案：从 transaction 级权限改为访问片段 / 地址粒度权限

### 14.1 设计原则

本 plan 建议修正为两层模型：

1. `main_control_transaction` 保留原有字段，作为“该 transaction 默认权限意图”。
2. 主表构建后派生出访问片段表，由访问片段表承载真实 TLB/DCache responder 使用的权限。

核心思想：

```text
transaction 字段:
  表示用户想让该 transaction 触发什么权限场景。

访问片段:
  表示 DUT 实际会看到的 TLB VPN 请求和 DCache cacheline 访问。

responder:
  不直接相信 transaction 单字段；
  按 request 的 VPN/cacheline 查访问片段 canonical 权限。
```

中文文字伪代码：

主表 transaction 仍是用户配置入口，但不能直接作为 responder 的唯一 owner。构建主表后，测试框架根据 `src_0 + imm`、访问 size、是否跨页、是否跨 16B/cacheline，把每条 transaction 展开成一个或多个访问片段。每个片段记录自己对应的 VPN、VA 范围、cacheline base 和权限字段。后续 L2TLB responder 按 VPN 查片段权限，DCache/memory responder 按 cacheline/PA 查片段错误属性。

### 14.2 新增派生数据结构建议

建议新增派生结构，字段名可在 coding 前按现有类型风格调整：

```systemverilog
typedef struct {
    memblock_uid_t uid;
    bit            is_tail;
    bit [63:0]     va;
    bit [51:0]     vpn;
    bit            tlbAF;
    bit            tlbPF;
    bit            tlbGPF;
    bit [1:0]      PBMT;
    bit [63:0]     ppn;
    bit [63:0]     pa;
    bit            pa_valid;
} memblock_main_tlb_perm_fragment_t;

typedef struct {
    memblock_uid_t uid;
    bit            is_tail;
    bit [63:0]     va;
    bit [63:0]     line_va;
    bit [63:0]     line_pa;
    bit            line_pa_valid;
    bit            corrupt;
    bit            denied;
} memblock_main_dcache_perm_fragment_t;
```

中文文字伪代码：

TLB fragment 描述一次真实 TLB 查询可能命中的 VPN 权限。它保留 owner uid、是否 tail、片段 VA、VPN 和 TLB 权限字段；TLB entry 建好后再回填 PPN/PA。DCache fragment 描述一次真实 DCache/cacheline 访问的错误属性。它保留 owner uid、是否 tail、片段 VA、cacheline VA/PA 和 `corrupt/denied`。这两个表都是主表派生数据，不改变原始主表字段的用户配置入口。

### 14.3 主表构建后展开 fragment

源码级伪代码：

```text
build_main_permission_fragments():
  if !main_permission_en:
    return

  tlb_fragments.delete()
  dcache_fragments.delete()

  for uid in [0, main_trans_num):
    tr = get_main_transaction(uid)
    access_va = tr.src_0 + tr.imm
    size_bytes = decode_size_bytes(tr)

    tlb_va_list = split_by_4k(access_va, size_bytes)
    for each va_piece in tlb_va_list:
      frag = make_tlb_fragment(uid, va_piece)
      frag.tlbAF/tlbPF/tlbGPF/PBMT = select_tlb_permission_for_piece(tr, va_piece)
      push tlb_fragments

    dcache_line_list = split_by_cacheline(access_va, size_bytes, dcache_block_bytes)
    for each line_piece in dcache_line_list:
      frag = make_dcache_fragment(uid, line_piece)
      frag.corrupt/denied = select_dcache_permission_for_piece(tr, line_piece)
      push dcache_fragments
```

中文文字伪代码：

构建函数在主表完成后运行。函数先判断主表权限总开关，未开启时直接返回。开启后清空旧 fragment 表，然后按 uid 顺序遍历主表。对每条 transaction，先计算真实访问 VA 和访问字节数；再按 4K 页边界拆出 TLB fragment，按 cacheline 边界拆出 DCache fragment。第一版如果暂时没有 per-fragment 用户字段，`select_*_permission_for_piece` 可以先把 transaction 默认字段复制给所有片段，但文档必须明确这只是默认策略，后续可扩展 head/tail 独立字段。

### 14.4 一致性检查改为 fragment canonical

源码级伪代码：

```text
canonicalize_tlb_fragments(manual_mode):
  for each tlb_frag in uid order:
    key = tlb_frag.vpn
    if owner_by_vpn.exists(key):
      owner = owner_by_vpn[key]
      if !same_tlb_permission(owner, tlb_frag):
        if manual_mode:
          fatal
        else:
          copy owner permission to tlb_frag
    else:
      owner_by_vpn[key] = tlb_frag

canonicalize_dcache_fragments(manual_mode):
  for each dcache_frag in uid order:
    key = dcache_frag.line_va
    if owner_by_line_va.exists(key):
      owner = owner_by_line_va[key]
      if !same_dcache_permission(owner, dcache_frag):
        if manual_mode:
          fatal
        else:
          copy owner permission to dcache_frag
    else:
      owner_by_line_va[key] = dcache_frag
```

中文文字伪代码：

TLB canonical 不再直接扫描 transaction 的起始 VPN，而是扫描已经展开的 TLB fragment。相同 VPN 的 fragment 必须权限一致；随机表模式自动归一到最老 fragment，手动表模式直接报错。DCache canonical 不再按完整 VA，而是按 cacheline VA 或后续回填后的 cacheline PA 建 owner；同一 cacheline 的 fragment 必须 `corrupt/denied` 一致。这样可以自然覆盖“不同 VA 落入同一 cacheline”和“一条 transaction 跨两条 cacheline”的情况。

### 14.5 L2TLB responder 查 fragment，而不是直接查 transaction

源码级伪代码：

```text
find_tlb_fragment_owner(vpn):
  for uid in fired_uid_q order:
    if !is_uid_fired_active(uid):
      continue
    for each tlb_frag owned by uid:
      if tlb_frag.vpn == vpn:
        return tlb_frag
  return not_found

on_l2tlb_miss_build_entry(req):
  key = make_tlb_key(req)
  entry = build_default_entry(key)
  if main_permission_en && main_tlb_permission_en:
    if find_tlb_fragment_owner(req.vpn, frag):
      entry.tlbAF/tlbPF/tlbGPF/PBMT = frag fields
  insert entry
  update_frag_pa_by_entry(req.vpn, entry)
```

中文文字伪代码：

L2TLB responder 收到请求后，用 request VPN 查 fragment owner。查询仍可用 `fired_uid_q` 限制在当前已发射 active uid 范围内，但 owner 返回的是 TLB fragment，而不是整条 transaction。miss 建表时，用 fragment 的 TLB 权限覆盖 entry。hit 时如果 debug check 开启，也用 fragment canonical 权限与已有 entry 比对。entry 产生 PPN 后，回填所有同 VPN fragment 的 PA，供后续 DCache fragment 使用。

### 14.6 DCache responder 查 cacheline fragment，并处理命中复用风险

源码级伪代码：

```text
find_dcache_fragment_owner(pa):
  line_pa = cacheline_base(pa)
  for uid in fired_uid_q order:
    if !is_uid_fired_active(uid):
      continue
    for each dcache_frag owned by uid:
      if dcache_frag.line_pa_valid && dcache_frag.line_pa == line_pa:
        return dcache_frag
  return not_found

on_dcache_memory_response(req_pa):
  base_result = memory_model_response(req_pa)
  if main_permission_en && main_dcache_permission_en:
    if find_dcache_fragment_owner(req_pa, frag):
      base_result.corrupt = frag.corrupt
      base_result.denied  = frag.denied
  return base_result
```

中文文字伪代码：

DCache responder 不再按 transaction 的精确 PA 查 owner，而是按 cacheline PA 查 DCache fragment。这样一条访存即使只访问 cacheline 内几个 byte，也能和 DCache/L2 miss 的 64B cacheline 粒度对齐。由于 DCache hit 不会触发 memory responder，后续同 cacheline transaction 的 `corrupt/denied` 不一定能独立生效；因此该方案必须把同一 cacheline 的错误属性 canonical 化，避免“第一条正常建 cacheline，第二条希望错误但没有下游请求”的不可实现语义。

### 14.7 对用户配置能力的第一版取舍

第一版建议：

1. 保留当前 transaction 级字段作为默认权限意图。
2. 自动展开 head/tail fragment。
3. 如果一条 transaction 跨两个 VPN 或两个 cacheline，默认把同一 transaction 的字段复制到所有 fragment。
4. 文档明确：第一版不支持同一 transaction 的 head/tail 配置不同权限。
5. 如果后续需要精确控制 head/tail，不应复用单字段，而应新增数组或 fragment override，例如：

```text
tlb_frag_perm_override_q:
  uid, fragment_index, tlbAF, tlbPF, tlbGPF, PBMT

dcache_frag_perm_override_q:
  uid, fragment_index, corrupt, denied
```

中文文字伪代码：

第一版先解决“测试框架内部建模粒度正确”的问题，不急着暴露复杂用户配置入口。transaction 字段作为默认值复制到所有 fragment，可以保持现有主表接口简单；但一旦跨页或跨 cacheline，框架内部已经知道有两个 fragment，并能按 fragment 做 canonical 和 responder 匹配。后续如果需要构造 head 正常、tail fault 这种更细场景，再新增 fragment override，而不是继续扩展 transaction 单字段语义。

### 14.8 命中复用场景的测试建议

定向用例需要覆盖：

```text
TLB hit 复用:
  uid0 建 VPN entry，权限正常；
  uid1 使用同 VPN，主表字段若试图设置不同权限：
    手动表应 fatal；
    随机表应归一化；
    不应期待 uid1 再次触发 L2TLB responder。

DCache hit 复用:
  uid0 miss/refill cacheline，corrupt/denied=0；
  uid1 同 cacheline，主表字段若试图设置 corrupt/denied=1：
    手动表应 fatal；
    随机表应归一化；
    不应期待 uid1 再次触发 memory responder。

跨 4K:
  一条 transaction 展开两个 TLB fragment；
  两个 VPN 均能被 L2TLB responder 按 fragment 权限处理。

跨 cacheline:
  一条 transaction 展开两个 DCache fragment；
  两个 cacheline miss 时能分别按 fragment 权限处理；
  同 cacheline 时能被 canonical/merge 逻辑稳定处理。
```

中文文字伪代码：

验证不应只看“某条 transaction 字段是否最终出现在 response 上”，而应看“真实 responder 请求是否存在，以及该请求对应的 VPN/cacheline canonical 权限是否正确”。对于 hit 复用场景，后续 transaction 没有下游 request 是正常 DUT 行为，测试框架应通过一致性检查提前禁止不可实现的冲突配置，而不是在 responder 中强行制造错误。

### 14.9 修正后的 Coding Checklist 增量

在执行 coding 前，第 12 章 checklist 需要替换或补充以下内容：

- [ ] 新增 TLB permission fragment 和 DCache permission fragment 派生表，而不是只新增 `ppn/pa` 到 transaction。
- [ ] 主表构建完成后按访问 VA/size 展开跨 4K TLB fragment。
- [ ] 主表构建完成后按 DCache cacheline 大小展开 DCache fragment。
- [ ] 一致性检查改为按 fragment VPN 和 fragment cacheline 做 canonical。
- [ ] L2TLB responder miss/hit debug check 查 TLB fragment owner。
- [ ] TLB entry PPN 回填到 TLB fragment，并派生 DCache fragment 的 line PA。
- [ ] DCache/SBuffer responder 按 cacheline PA 查 DCache fragment owner。
- [ ] 文档明确第一版不支持同一 transaction head/tail 不同权限；如需要，后续新增 fragment override。
- [ ] 定向验证覆盖 TLB hit 复用、DCache hit 复用、跨 4K、跨 cacheline。
