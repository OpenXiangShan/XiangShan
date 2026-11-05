# MemBlock CAS 成功前置 Store 实现审查

## 1. 定位与术语

### 1.1 术语与抽象功能说明

| 英文术语 | 当前中文含义 | 源码对象或落点 | 示例 |
|---|---|---|---|
| CAS / compare / new | 条件原子交换 / 比较旧值 / 待写新值 | `MEMBLOCK_LSUOP_AMOCAS_*`、四槽覆盖值 | Q 比较两份64位值 |
| UID / ROB key | 软件主表编号 / DUT 重排序环形索引 | `main_control_transaction` | 替换 UID 5，保留其连续 ROB key |
| prefill group / owner | Store 与未来 CAS 的完整组 / 最后执行的 CAS | `memblock_cas_prefill_group_t` | Q 的 low SD、high SD、owner 共三条 |
| pending reservation / map | 未生成成员的未来位置预约 / 关联数组 | 两张 `cas_prefill_*_by_uid` | high 消费后删 map，owner 仍待生成 |
| audit ledger | 完整组的持久冻结账本 | `cas_prefill_group_audit_q` | 全部成员消费后仍可比对数据 |
| STD override / slot | 冻结的数据覆盖值 / 指令内部片编号 | `std_data_override_valid/data[4]` | Q 槽0/2比较、槽1/3新值 |
| STA / STD / WB | Store 地址片 / 数据片 / 写回 | `issue_field_assigner`、既有监控链 | Q 的 STA 为槽0/2，STD 为槽0..3 |
| AUTO / control marker | 自动控制表模式 / 控制标记 | `build_control_auto_main_table()` | 普通区间[0,N)，UID N固定 check_store |
| LR/SC reservation | 保留读之后的软件未来条件写预约，不是 DUT 保留状态 | `sc_reservation_by_uid` | 同地址 SC 必须保留 LR 地址 |
| bucket / alias | 距离权重档 / 不同虚拟地址映射同物理范围 | 三档距离参数、PPN配置 | 11..30不与6..10重叠 |
| admission / barrier / terminal | 接纳 / 单原子所有者隔离屏障 / 指令终态 | 既有公共生命周期 | CAS接纳前更老Store已终态 |
| TLB / VPN / PPN | 地址转换表 / 虚拟页号 / 物理页号 | 既有建表与响应链 | Store/CAS同地址使用相同转换键 |
| LSQ / SQ / SBuffer | 访存队列 / Store队列 / 已提交Store缓冲 | 既有普通Store链 | 前置Store仍分配真实SQ |
| CSR / SFence / CBO.ZERO | 控制寄存器操作 / 转换失效操作 / 整行清零 | control预约、写范围判定 | 只有CSR/SFence彼此可同target |
| smoke / software-only / fail-fast | 小规模接口测试 / 不依赖DUT事件的软件检查 / 初始化或建表立即拒绝非法输入 | 本文验证命令与预期fatal捕获 | 非法百分比在建表前拒绝 |
| packed struct / getter | 可按位比较的结构体 / 冻结参数只读入口 | group描述符、`seq_csr_common` | 比较map描述与ledger原始内容 |

### 1.2 范围和基线

- 日期：2026-11-05；版本：V2；分支：`codex/memblock-amo-adaptation-20261103`。
- 开始时工作区干净，HEAD 包含 `e3ae325f8d` 和 `55dc0aedf6`；所有操作限定本 worktree。
- 关联计划：`AI_DOC/plan/test_framework/plan/do/memblock_amocas_compare_success_prefill_plan_20261105.md`；已完成coding与验证，2026-11-05由undo归档到do。
- 用户明确授权按计划修改代码；subagent按分工不提交，由主agent完成独立审查后执行最终提交。
- 本实现只制造输入前置条件，不增加 RM、scoreboard、最终内存值或 CAS 成败 checker，不修改 RTL、driver、agent transaction、atomic barrier 或写回机制。

### 1.3 修改文件与职责

下列源码路径以 `mem_ut/ver/ut/memblock/` 为基准。

| 文件 | 审查范围 |
|---|---|
| `env/plus.sv` | 四参数定义、保守默认和命令行解析 |
| `seq/base_seq_help/seq_csr_common.sv` | 参数快照、getter、非负/范围/权重/别名检查 |
| `seq/plus_cfg/default.cfg`、`tc_dispatch_real_amo_smoke.cfg` | 默认0、专项80、三档权重1 |
| `rule/plus_demo_migration_plan.md` | 参数登记、距离及别名边界 |
| `seq/base_seq_help/main_control_transaction.sv` | 四槽覆盖字段、初始化和掩码校验 |
| `seq/base_seq_help/issue_field_assigner.sv` | 统一STD数据选择与实际端口赋值 |
| `seq/base_seq_help/memblock_dispatch_base_sequence.sv` | 描述符、两张预约表、账本、构造、位置仲裁、保护、审计、两种建表入口、LR/SC和control融合 |
| `seq/base_seq/soft_test/soft_test_memblock_lsq_amo_admission_sequence.sv` | 定向构造、负向捕获、参数/槽/距离/冲突/保护/边界/审计检查 |
| 本次计划、旧AMO总计划、`amo_stimulus_flow.md`、三份源码分析 | 状态、路径及现行数据/控制链同步 |

## 2. 参数链

修改前：CAS 无成功前置构造参数。修改后：使能默认0，三档默认各1；专项 AMO cfg 使能80。公共生成器只读冻结 getter，不读取 `plus::*`。四个输入均经 `get_non_negative_int()`，使能额外限制到100，距离权重总和以64位判断非零。

抽象功能描述：`load_from_plus()`在框架初始化阶段保存最终输入；`check_amo_cas_prefill_config()`拒绝不能支持的组合，不改动运行期状态。

源码位置：`seq/base_seq_help/seq_csr_common.sv`，`check_amo_cas_prefill_config()`。

```systemverilog
if (amo_cas_prefill_en_1_wt > 100)
    `uvm_fatal("SEQ_CSR_CFG", "MEMBLOCK_AMO_CAS_PREFILL_EN_1_WT must be within [0:100]")
if ((64'(amo_cas_prefill_dist_wt[0]) + 64'(amo_cas_prefill_dist_wt[1]) +
     64'(amo_cas_prefill_dist_wt[2])) == 0)
    `uvm_fatal("SEQ_CSR_CFG", "CAS prefill distance weights must not all be zero")
if (amo_cas_prefill_en_1_wt != 0 &&
    (l2tlb_ppn_reuse_en || l2tlb_ppn_region_id >= 0))
    `uvm_fatal("SEQ_CSR_CFG", "CAS prefill does not support PPN reuse or directed routed-PPN alias scenarios")
```

中文伪代码：本段在初始化时检查前置激励配置。先拒绝大于100的使能百分比，再把三档转成64位相加并拒绝全零；启用且打开PPN复用或显式固定物理路由窗口时立即拒绝。负值已经由输入快照的非负检查拒绝；此函数不随机、不改队列。`validate_and_clamp()`调用它，失败时真实激励尚未开始。

正确性检查：soft test覆盖输入快照与三个getter、百分比101、负权重、全零权重、PPN reuse、固定窗口。三档巨大合法权重也被实际抽样验证，避免32位加法溢出。首版保守地拒绝所有显式固定窗口，不宣称其中每个窗口都有物理别名。

## 3. 四槽数据与issue统一入口

### 3.1 主表冻结字段

修改前：Store用地址基数作数据，CAS按UID和slot派生，跨UID无法证明比较值相同。修改后：新增四槽覆盖数组，构造时全部清零；组构造只设置所属角色需要的槽。未覆盖项保留旧数据行为。

抽象功能描述：`validate_main_transaction()`检查主表数据合同，避免手工或随机事务携带不属于本指令的覆盖掩码。

源码位置：`seq/base_seq_help/main_control_transaction.sv`，覆盖掩码分支。

```systemverilog
if (any_override) begin
    if (op_class == MEMBLOCK_OP_CLASS_STORE) begin
        if (!std_data_override_valid[0] || std_data_override_valid[1] ||
            std_data_override_valid[2] || std_data_override_valid[3]) return 1'b0;
    end else if (op_class == MEMBLOCK_OP_CLASS_AMO &&
                 fuOpType inside {MEMBLOCK_LSUOP_AMOCAS_W, MEMBLOCK_LSUOP_AMOCAS_D,
                                  MEMBLOCK_LSUOP_AMOCAS_Q}) begin
        if (!std_data_override_valid[0] || !std_data_override_valid[1]) return 1'b0;
        if (std_data_override_valid[2] != (fuOpType == MEMBLOCK_LSUOP_AMOCAS_Q) ||
            std_data_override_valid[3] != (fuOpType == MEMBLOCK_LSUOP_AMOCAS_Q)) return 1'b0;
    end else return 1'b0;
end
```

中文伪代码：本段维护数据角色。只在任意槽有效时进入新检查；普通Store要求且仅允许槽0；CAS要求槽0/1，Q额外要求槽2/3，W/D则要求槽2/3无效；其它分类携带覆盖值直接返回非法。通过后继续旧的地址关系和优先级范围检查。构造函数清零数组，`post_randomize()`和`post_manual_config()`共用此校验。

正确性检查：软件测试验证Store单槽、CAS.D双槽、CAS.Q四槽合法；Store多槽、Q缺槽、非CAS原子残留覆盖均非法。组账本另外要求owner确实启用覆盖，防止“所有槽无效仍是合法普通CAS”的规则被组成员误用。

### 3.2 实际STD数据

抽象功能描述：`derive_std_data()`为所有STD提供唯一选择入口，冻结值不受UID、重试或shuffle影响。

源码位置：`seq/base_seq_help/issue_field_assigner.sv`，`derive_std_data()`。

```systemverilog
if (main_tr == null || slot >= 4)
    `uvm_fatal("ISSUE_FIELD", "derive_std_data requires a transaction and slot in [0:3]")
if (main_tr.std_data_override_valid[slot]) return main_tr.std_data_override[slot];
behavior = lsq_ctrl_model::derive_op_behavior(main_tr);
if (behavior.is_atomic) return derive_atomic_std_data(main_tr, slot);
return main_tr.src_0;
```

中文伪代码：本段选择本槽的数据。空事务或越界槽先报错；有效覆盖直接返回冻结值；否则行为派生器根据主表分类得到是否原子，原子保留旧数据派生，其它STD返回原`src_0`。该函数不改事务或运行期状态。`assign_std_main_fields()`只调用它，再把结果写入所选物理STD端口。

正确性检查：软件测试逆序重复读取所有compare/new槽并临时改变owner UID，数据不变；另直接调用实际端口赋值，检查Q四槽与普通Store的端口数据。真实CAS.Q smoke在STD shuffle开启下正常收敛。

## 4. 冻结组与位置仲裁

### 4.1 状态生命周期

`memblock_cas_prefill_group_t`显式保存valid、原始/low/high/owner UID、Q标志、opcode、地址、四份数据、选中距离。使用packed struct保证整个描述可按位比较；没有依赖`copy/clone`或UVM字段自动注册。

low Store当拍返回，永不进入未来map；high只进入高Store map；owner只进入CAS map。全部位置与当前Store构造成功后，组整体追加到账本。两个真实建表入口以及manual导入入口在`reset_all_tables()`后清空SC、两张CAS map和账本。

正确性检查：重复建表不会带入旧组；账本按origin递增，成员消费不删账本；建表结束检查两张map和SC map均空。新逻辑全在构建期，没有增加每拍全表扫描。

### 4.2 统一占用判断

修改前：SC、控制预约分别检查，控制命中可删除SC。修改后：所有未来成员共用只读占用判断；只有CSR/SFence彼此同target允许并存。

抽象功能描述：`uid_reserved_for_companion()`判断候选是否可用，同时拒绝非法重复预约，防止按生成优先级吞掉成员。

源码位置：`seq/base_seq_help/memblock_dispatch_base_sequence.sv`，同名函数。

```systemverilog
companion_count = int'(sc_reservation_by_uid.exists(uid)) +
                  int'(cas_prefill_store_high_by_uid.exists(uid)) +
                  int'(cas_prefill_owner_by_uid.exists(uid));
control_hit = (control0_valid && uid == control0) || (control1_valid && uid == control1);
if (companion_count > 1 || (companion_count != 0 && (control_hit || uid >= check_store_uid)))
    `uvm_fatal("CAS_PREFILL", $sformatf("overlapping companion/control reservation uid=%0d", uid))
return companion_count != 0 || control_hit || uid >= check_store_uid;
```

中文伪代码：本段统计三类companion的占用数，并把两个控制target合并为一个是否命中的事实；companion互相重叠、与control或末尾保留位重叠立即fatal。没有非法重叠时，任一占位或候选达到N都返回已占用。控制彼此同target只形成一个control_hit，不触发错误。

抽象功能描述：`find_next_free_companion_uid()`从唯一初始候选向前找到第一个空UID，不重新抽距离，不扩表。

源码位置：同文件，同名函数。

```systemverilog
selected_uid = 0;
for (longint unsigned candidate = start_uid; candidate < table_limit; candidate++) begin
    if (!uid_reserved_for_companion(memblock_uid_t'(candidate), control0_valid, control0,
                                     control1_valid, control1, table_limit)) begin
        selected_uid = memblock_uid_t'(candidate);
        return 1'b1;
    end
end
return 1'b0;
```

中文伪代码：本段进行连续顺延。输出先清零，从输入初始UID开始逐个检查，遇到统一查询返回空位即写输出并成功返回；扫描达到N则失败。统一查询承担冲突诊断和保留位约束，调用方根据失败保持原事务或使control target失效。

正确性检查：软件构造SC=1、CAS=2、high=3、CSR=4、SFence=5，候选1顺延到6；limit=6时失败，不占UID N。另测试SC/control非法同位和CSR/SFence合法同位。

### 4.3 距离抽样

抽象功能描述：`randomize_cas_prefill_distance()`在最后一条Store之后选择一次距离；它不读取预约或修改状态。

源码位置：同文件，距离桶分支。

```systemverilog
case (bucket)
    0: return $urandom_range(5, 1);
    1: return $urandom_range(10, 6);
    2: return $urandom_range(30, 11);
    default: `uvm_fatal("CAS_PREFILL", "invalid CAS prefill distance bucket")
endcase
```

中文伪代码：本段在已选中的三档内均匀抽样，分别返回1..5、6..10或11..30；非法档立即fatal。前置逻辑将三个getter权重用64位累加，生成64位随机数对总和取模，再按累计权重确定桶，权重全零另有fatal。规划仅调用一次，位置冲突只顺延UID。

正确性检查：每档单独启用512次，范围与1、5、6、10、11、30端点全部覆盖；另外用三份`0x7fffffff`测试总和无32位溢出。

## 5. 完整组规划与成员构造

### 5.1 原子化规划

修改前：随机CAS保持原位，无冻结数据及配对Store。修改后：选中后在局部描述符规划全部UID、地址和数据，全部成功才提交；任一容量失败返回false，原CAS、map和账本不变。

抽象功能描述：`try_plan_cas_prefill()`由普通随机分支调用，输出当前UID的替换Store及未来组预约，预约owner不会再经过此入口。

源码位置：`seq/base_seq_help/memblock_dispatch_base_sequence.sv`，high与owner位置规划。

```systemverilog
last_store = group.store_low_uid;
if (group.store_high_valid) begin
    if (!find_next_free_companion_uid(64'(last_store) + 1, table_limit,
        control0_valid, control0, control1_valid, control1, group.store_high_uid)) return 1'b0;
    last_store = group.store_high_uid;
end
group.selected_distance = randomize_cas_prefill_distance();
initial_candidate = 64'(last_store) + group.selected_distance;
if (!find_next_free_companion_uid(initial_candidate, table_limit,
    control0_valid, control0, control1_valid, control1, group.cas_uid)) return 1'b0;
```

中文伪代码：本段找到完整组的未来位置。low是当前UID；Q从当前UID加1找high，失败直接返回且未改全局状态；随后只抽一次距离，从最后Store加距离找CAS owner，失败同样不提交。扫描helper统一跳过全部预约，high在owner前，所以尚未写入map也不会被owner复用。进入本段前先确认AMO分类和CAS编码；使能0立即返回且不抽样，百分比未命中也不改原CAS。

抽象功能描述：规划后半段将安全地址与四份数据固化为可恢复输入，只在替换Store已合法构造后发布组。

源码位置：同函数，正式提交分支。

```systemverilog
replacement_store = make_cas_prefill_store_transaction(group, 0, original_cas.get_rob_key());
if (group.store_high_valid) cas_prefill_store_high_by_uid[group.store_high_uid] = group;
cas_prefill_owner_by_uid[group.cas_uid] = group;
cas_prefill_group_audit_q.push_back(group);
```

中文伪代码：本段发布完整组。此前已用临时CAS模板生成对齐地址、规避pending组和同地址SC，随机冻结compare/new，W清高32位，Q额外冻结高64位；现在以原CAS的ROB身份构造当前Store并完成合法性检查，然后一次写入可选high map、owner map和完整账本。low不进map。日志打印origin、high、选中距离、初始/最终owner候选、顺延数、地址和opcode。原CAS对象没有被拷贝或改写。

正确性检查：W/D用一Store，Q用两SD；六个距离端点全部验证；W/D表长1与Q表长2整体回滚，三张容器均为空，原CAS仍保留原opcode且无覆盖值。地址空间无法满足普通写保护时明确fatal，不静默改成load。

### 5.2 Store成员

抽象功能描述：`make_cas_prefill_store_transaction()`把冻结描述转换成普通LSQ Store，保留原地址/数据关系，独立配置普通Store优先级。

源码位置：同文件，Store模板与数据。

```systemverilog
tr.uid = high_half ? group.store_high_uid : group.store_low_uid;
tr.op_class = MEMBLOCK_OP_CLASS_STORE;
apply_op_class_template(tr, group.cas_fuOpType == MEMBLOCK_LSUOP_AMOCAS_W ?
                           MEMBLOCK_LSUOP_SW : MEMBLOCK_LSUOP_SD);
tr.robIdx_flag = rob_key.flag;
tr.robIdx_value = rob_key.value;
tr.src_0 = group.address + (high_half ? 8 : 0);
tr.imm = 0;
tr.update_vaddr();
tr.std_data_override_valid[0] = 1'b1;
tr.std_data_override[0] = high_half ? group.compare_high : group.compare_low;
```

中文伪代码：本段恢复普通Store。按角色选择UID，W选择SW，其它选择SD；Store模板生成STU、STORE flow和一个LSQ元素；写入当前位置的ROB key，地址为A或A+8、imm为0，更新最终地址；仅槽0有效并写入对应compare。函数前部拒绝无效组或非Q high角色；后部独立抽普通Store的send/std优先级与delay，完成校验再返回。构造器默认异常字段清零，不复制CAS的MOU、原子flow或优先级100。

### 5.3 owner成员

抽象功能描述：`make_reserved_cas_transaction()`在未来UID恢复已冻结CAS，不重新规划、不执行地址复用。

源码位置：同文件，冻结数据恢复。

```systemverilog
for (int slot = 0; slot < (group.store_high_valid ? 4 : 2); slot++)
    tr.std_data_override_valid[slot] = 1'b1;
tr.std_data_override[0] = group.compare_low;
tr.std_data_override[1] = group.new_low;
tr.std_data_override[2] = group.compare_high;
tr.std_data_override[3] = group.new_high;
```

中文伪代码：本段恢复owner数据。W/D有效槽数为2，Q为4，然后按compare low、new low、compare high、new high写回冻结值。函数先检查有效CAS描述，使用AMO模板恢复MOU、ATOMIC flow和零LSQ分配，设置未来UID及当前连续ROB，地址A、imm0、自然对齐4/8/16、非对齐标志0、两优先级100；最后合法性检查并返回。它不抽compare/new，也不再次生成group。

### 5.4 精确消费与账本一致性

抽象功能描述：`consume_cas_prefill_reservation()`校验并恢复指定角色成员，然后删除且只删除该预约。

源码位置：同文件，消费前账本定位。

```systemverilog
while (left <= right) begin
    mid = left + (right - left) / 2;
    if (cas_prefill_group_audit_q[mid].origin_uid == group.origin_uid) begin
        ledger_match = cas_prefill_group_audit_q[mid] == group;
        break;
    end
    if (cas_prefill_group_audit_q[mid].origin_uid < group.origin_uid) left = mid + 1;
    else right = mid - 1;
end
if (!ledger_match) `uvm_fatal("CAS_PREFILL", "reservation frozen descriptor differs from audit ledger")
```

中文伪代码：本段证明map内容等于正式提交的组。先根据角色读取存在的预约，再在按origin单调追加的账本中二分定位；找到相同origin后比较整个packed描述，不同或未找到立即fatal。之后high分支检查Q标志、high UID、owner仍存在且描述相同；owner分支检查其UID、high已消费。分别调用Store或CAS构造器，通过后精确删除对应map项，并检查实际生成UID。账本不删除，供最终审计。

正确性检查：注入high map冻结值损坏时捕获fatal；每组正常high和owner精确消费，完整表后map为空；二分避免每次消费全扫历史组。

## 6. 地址保护

### 6.1 写范围判定

修改前：地址复用可把随机写重新指向compare数据。修改后：复用与边界同步之后，写保护规避所有尚未生成owner的组；恢复的组成员直接使用冻结地址。

抽象功能描述：`cas_prefill_write_overlaps()`按最终虚拟字节范围判断一条激励是否会破坏组数据，`overlaps_pending_cas_prefill()`只遍历尚未到达owner的map。

源码位置：`seq/base_seq_help/memblock_dispatch_base_sequence.sv`，写类型和区间判断。

```systemverilog
if (!(tr.op_class == MEMBLOCK_OP_CLASS_STORE ||
      (tr.op_class == MEMBLOCK_OP_CLASS_AMO && !is_lr_fuoptype(tr.fuOpType)) ||
      (tr.op_class == MEMBLOCK_OP_CLASS_CBO && tr.fuOpType == MEMBLOCK_LSUOP_CBO_ZERO))) return 1'b0;
size_bytes = derive_size_bytes(tr.op_class, tr.fuOpType);
first_byte = tr.vaddr;
if (tr.op_class == MEMBLOCK_OP_CLASS_CBO) begin
    size_bytes = 64;
    first_byte &= ~64'd63;
end
last_byte = first_byte + size_bytes - 1;
protected_last = group.address + derive_size_bytes(MEMBLOCK_OP_CLASS_AMO, group.cas_fuOpType) - 1;
return first_byte <= protected_last && group.address <= last_byte;
```

中文伪代码：本段把激励转成可能写入的闭区间。普通Store、非LR原子、CBO.ZERO继续，其它返回不冲突；普通写按opcode宽度，CBO.ZERO先向下对齐64字节并覆盖整行；组按CAS的4/8/16字节宽度得到末地址，再检查两个区间是否相交。调用方只在事务UID小于pending owner时应用，故已结束组不再阻止复用。Load、LR和prefetch保持可读。

### 6.2 地址修正

抽象功能描述：`avoid_pending_cas_prefill_write_overlap()`只移动冲突写地址，保持分类、opcode、数据与原子非对齐意图。

源码位置：同文件，确定性后备扫描。

```systemverilog
address = ((base + alignment - 1) & ~(64'(alignment - 1))) + offset;
while (address <= upper && size_bytes - 1 <= upper - address) begin
    tr.src_0 = address;
    tr.imm = 0;
    tr.update_vaddr();
    if (!overlaps_pending_cas_prefill(tr)) begin
        if (seq_csr_common::get_boundary_profile_gen_en())
            sync_boundary_profile_after_addr_reuse(tr, "CAS protection deterministic relocation");
        return;
    end
    address += alignment;
end
`uvm_fatal("CAS_PREFILL", $sformatf("no unprotected write address for uid=%0d", tr.uid))
```

中文伪代码：本段是16次随机修正仍冲突时的后备路径。先从窗口起点按访问宽度对齐并保留原子非对齐偏移，循环确认完整跨度未越界后设置最终地址；不冲突则必要时同步边界标签并返回；冲突只增加一个对齐步长；整个窗口均无可用地址则fatal。此前16次重试使用现有合法地址模板，保留相同偏移并确认末字节不越界；全程不把AMO/CBO转换成其它分类，不修改覆盖数据。

正确性检查：软件测试覆盖SW部分重叠、SC、普通AMO、其它CAS、CBO.ZERO整行、允许Load/LR；把窗口限制为单行，使16次64B随机anchor必冲突，从而实际执行确定性步长后备并找到非重叠地址。

### 6.3 新组与未来同地址SC

抽象功能描述：`cas_prefill_address_conflicts()`在组发布前排除已有组和已经承诺同地址的未来SC。

源码位置：同文件，同地址预约分支。

```systemverilog
if (reservation.same_addr && sc_uid > group.origin_uid && sc_uid < group.cas_uid) begin
    probe.fuOpType = reservation.sc_fuOpType;
    probe.vaddr = reservation.lr_src_0 + main_control_transaction::sign_extend_imm12(reservation.lr_imm);
    if (cas_prefill_write_overlaps(probe, group)) return 1'b1;
end
```

中文伪代码：本段保护在先的LR/SC承诺。函数先用临时CAS probe检查pending组；再遍历SC预约，仅对组区间内且承诺同地址的SC恢复其真实地址和宽度，发现与新组字节重叠就返回冲突。规划方先尝试随机安全地址，仍有冲突时按CAS宽度顺序寻找；没有完整跨度则返回规划失败。这样不会到SC生成时改变已承诺地址。反向情况下，新同地址SC若会破坏既有pending组则取消本次配对，LR保持原指令。

## 7. 建表入口和既有预约融合

### 7.1 普通与AUTO构造

抽象功能描述：`build_random_main_table()`和`build_control_auto_main_table()`在固定UID/ROB序列上恢复预约成员或生成随机项，并在TLB阶段之前完成组审计。

源码位置：`seq/base_seq_help/memblock_dispatch_base_sequence.sv`，普通表预约入口。

```systemverilog
void'(uid_reserved_for_companion(uid, 0, 0, 0, 0, main_trans_num_i));
if (cas_prefill_store_high_by_uid.exists(uid)) begin
    tr = consume_cas_prefill_reservation(uid, rob_key, 1'b1);
end else if (cas_prefill_owner_by_uid.exists(uid)) begin
    tr = consume_cas_prefill_reservation(uid, rob_key, 1'b0);
end else if (sc_reservation_by_uid.exists(uid)) begin
    tr = make_paired_sc_transaction(sc_reservation_by_uid[uid], rob_key);
    sc_reservation_by_uid.delete(uid);
    avoid_pending_cas_prefill_write_overlap(tr);
end else begin
```

中文伪代码：本段先检查当前UID预约没有非法重叠，再依次恢复high、owner或SC；SC精确消费后执行保护，普通随机分支随后生成新事务。随机分支先做现有地址复用和边界同步，再保护最终写地址、尝试CAS整组规划，成功才用low Store替换当前对象；最后验证、写主表、维护recent队列和连续ROB key。只有AMO分类的LR才尝试SC配对。

AUTO分支在同样链路前处理CSR/SFence，传入实时两个control target；control与companion非法同位先fatal，CSR/SFence彼此同位只生成CSR并让SFence按相同base重新预约。所有companion的limit固定N，最后额外构造UID N的check_store；普通长度N，AUTO长度N+1。结尾先初始化状态并确认主表完整，再做组审计与既有control结构检查。

正确性检查：定向Q普通N=12得到4完整组；AUTO N=12最终13条且UID12为check_store；control每隔3条时3/6/9均保持CSR优先，Q的high/owner自动顺延；重复关闭prefill建表时账本清空。

### 7.2 SC与control的连续顺延

抽象功能描述：`try_reserve_paired_sc()`维持LR宽度、地址承诺和原`LR+distance+1`定义，改成单次抽样后连续顺延。

源码位置：同文件，配对入口与同地址保护。

```systemverilog
if (lr_tr == null || lr_tr.op_class != MEMBLOCK_OP_CLASS_AMO || !is_lr_fuoptype(lr_tr.fuOpType)) begin
    `uvm_fatal(get_type_name(), "try_reserve_paired_sc requires an LR transaction")
end
```

中文伪代码：本段要求合法AMO分类的LR对象，避免普通SW/SD的2/3编码被误判。两个建表调用点也使用相同分类条件。随后按原概率决定配对、只抽一次距离，从LR加距离加1连续寻找未来空位；成功才冻结SC宽度、同地址策略与LR地址/非对齐元数据。若同地址SC与pending组重叠，返回而不插入预约；失败仅保留LR，不扩表。日志保留初始、最终和跳过数。

抽象功能描述：`reschedule_control_reservation()`在旧间隔语义上避开所有SC/CAS，超界时失效，不反向挤占成员。

源码位置：同文件，control顺延。

```systemverilog
if (!find_next_free_companion_uid(candidate_uid, normal_main_trans_num,
                                  0, 0, 0, 0, selected_uid)) return;
plan.next_uid = selected_uid;
plan.target_valid = 1'b1;
```

中文伪代码：本段在已抽取间隔并检查初始候选合法后，调用统一helper连续跳过companion；失败保持函数开头设置的target_valid=0，成功保存新UID并置有效。两个控制target未互相传入，保留它们既有同target例外；建表命中时不再删除SC。软件检查既覆盖顺延到空位，也覆盖越过N后的失效。

## 8. 建表后审计

抽象功能描述：`check_all_cas_prefill_groups()`只检查激励静态一致性，在TLB构造和真实driver之前拒绝残缺或被覆盖的组，不判断DUT结果。

源码位置：`seq/base_seq_help/memblock_dispatch_base_sequence.sv`，审计入口与区间保护复核。

```systemverilog
if (cas_prefill_store_high_by_uid.num() != 0 || cas_prefill_owner_by_uid.num() != 0 ||
    sc_reservation_by_uid.num() != 0)
    `uvm_fatal("CAS_PREFILL", "unconsumed companion reservations after main table build")
```

中文伪代码：本段先确认所有未来预约已消费，任何high、owner或SC残留都会立即报错；这不删除预约，也不会用清理动作掩盖未生成成员。随后按账本逐组核验单调origin、valid、opcode、low/high/owner顺序、距离1..30、实际距离不小于抽样值、owner严格小于N和地址对齐。

源码位置：同函数，区间内写入复核。

```systemverilog
for (memblock_uid_t uid = group.store_low_uid + 1; uid < group.cas_uid; uid++) begin
    if (group.store_high_valid && uid == group.store_high_uid) continue;
    if (cas_prefill_write_overlaps(data.get_main_transaction(uid), group))
        `uvm_fatal("CAS_PREFILL", $sformatf("intervening write uid=%0d destroys group origin=%0d", uid, group.origin_uid))
end
```

中文伪代码：本段独立检查组保护结果，从low之后逐个读取直到owner之前；只有本组Q高Store允许写保护范围，其余条目通过统一字节重叠判定，命中立即fatal。前面的成员核验逐一读取主表low/owner/可选high，比较角色、opcode、UID、地址关系、有效覆盖及四份compare/new、自然对齐元数据、无主动异常字段，并调用主表合同校验。所有检查只在一次建表末尾执行。

正确性检查：软件测试对每个W/D/Q和距离构造完整主表并审计通过；随后翻转owner new槽一位，审计必须捕获一次冻结数据不一致。预约描述损坏在消费前被账本比较捕获。

## 9. software-only测试组织

抽象功能描述：本测试文件扩充既有LSQ/AMO测试，不向生产基类添加只服务软件测试的API。

| 测试对象 | 实现与检查 |
|---|---|
| `memblock_cas_prefill_expected_fatal_catcher` | 单次调用期间按severity、ID及消息模式捕获；计数必须符合预期；其它fatal继续抛出 |
| `randomize_cas_prefill_distance()`软件覆盖 | 仅定向测试时返回端点，否则调用父类随机逻辑 |
| `randomize_main_transaction()`软件覆盖 | 定向Q建表使用正式template、UID和ROB；关闭定向标志时原样调用父类 |
| `make_prefill_cas()/reset_prefill_fixture()` | 构造软件输入、清空本测试预约；不混入真实driver主循环 |
| `check_cas_prefill_parameters()` | 临时修改输入并快照、精确检查非法组合，最后恢复原plus与快照 |
| `check_cas_prefill_groups()` | 关闭旁路、100%组、W/D/Q、六端点、真实三档随机、大权重、重试数据、尾部回滚、UID冲突、审计数据损坏 |
| `check_cas_prefill_protection_and_builders()` | 写保护、允许读、同地址SC双向约束、后备扫描、损坏预约、SC顺延、N/N+1、control例外、重复建表清理 |
| `check_atomic_issue_fields()`扩展 | 掩码合法/非法、四槽实际端口数据、普通Store旧数据回退与覆盖 |

软件检查临时改动配置快照与拓扑观测值后均恢复；此testcase关闭真实dispatch默认sequence，不把人工测试状态带入真实driver。没有新增sequence类注册文件，因此`seq_pkg.sv/seq.f/cfg/tb.f`保持既有接入顺序。

### 9.1 负向报告捕获

抽象功能描述：`memblock_cas_prefill_expected_fatal_catcher::catch()`只把当前负向测试期待的fatal转为可计数结果，不掩盖真实错误。

源码位置：`seq/base_seq/soft_test/soft_test_memblock_lsq_amo_admission_sequence.sv`。

```systemverilog
if (get_severity() == UVM_FATAL && get_id() == expected_id &&
    uvm_is_match(expected_pattern, get_message())) begin
    caught_count++;
    return CAUGHT;
end
return THROW;
```

中文伪代码：本段先同时检查severity、报告ID和消息模式，全部符合才增加捕获次数并阻止该次预期fatal终止仿真；其它报告继续抛出。构造函数从单次测试传入ID与模式并清零计数；测试仅在目标调用周围安装callback，调用后立即删除并检查精确次数。

### 9.2 软件定向入口与恢复

抽象功能描述：软件覆盖函数只改变本测试的构造输入，真实基类仍使用生产随机逻辑。

源码位置：同文件，距离虚函数覆盖。

```systemverilog
if (directed_cas_distance != 0) return directed_cas_distance;
return super.randomize_cas_prefill_distance();
```

中文伪代码：本段在软件测试明确设置距离时返回指定端点，其余调用父类真实三档抽样；测试退出恢复0，所以后续检查不会意外继续固定距离。主表随机覆盖采用同样旁路原则，开启时用真实opcode模板和连续UID/ROB生成Q，关闭时直接调用父类。`make_prefill_cas()`创建合法分类/地址/对齐的输入；`reset_prefill_fixture()`清三张预约map及账本，均只属于软件文件。

### 9.3 参数、完整组与保护测试入口

抽象功能描述：三个新增检查task临时设置输入并验证公共生产helper，再恢复所有快照，不维护第二套生产算法。

源码位置：同文件，`body()`新增调用。

```systemverilog
check_cas_prefill_parameters();
check_cas_prefill_groups();
check_cas_prefill_protection_and_builders();
```

中文伪代码：本段先验证plus到冻结getter及非法配置，再验证每种宽度/距离的数据和预约，最后验证地址与两种真实建表入口。参数task保存plus、设置四输入、调用真实快照、逐项断言，捕获非法范围/权重/alias后恢复；组task逐宽度逐端点规划、消费全部成员、审计并注入new数据破坏，随后测三档随机与表尾回滚及全部预约碰撞；保护task用真实写范围helper逐类检查、强制确定性后备、验证SC承诺、再通过N/N+1入口和control同位规则建表。任一非预期失败通过`check_expect()`fatal终止，不打印完成信息。

### 9.4 实际STD端口覆盖检查

抽象功能描述：`check_atomic_issue_fields()`扩展验证冻结数据通过正式赋值入口到达真实端口字段。

源码位置：同文件，Q端口循环。

```systemverilog
for (int slot = 3; slot >= 0; slot--) begin
    item.uop_index = slot;
    field_assigner.assign_std_main_fields(issue_tr, amo, item, 0);
    check_expect(issue_tr.io_ooo_to_mem_issueStd_0_bits_src_0 == amo.std_data_override[slot],
        "STD port did not consume selected frozen slot");
end
```

中文伪代码：本段逆序选择Q四槽，给正式issue item设置槽编号，用正式字段赋值器写STD0，然后逐槽比较端口与主表冻结值；不能只证明getter返回正确。之后破坏Q掩码、切换CAS.D和非CAS验证合法/非法边界，再用普通Store测试旧`src_0`回退、槽0覆盖及多槽拒绝。原有STA目的寄存器和未覆盖CAS数据检查仍保留。

## 10. 验证记录

命令均从 `mem_ut/ver/ut/memblock/sim` 执行，真实工具在eda01运行，日志保留在本worktree的`.eda_remote`与`base_fun/log`。`eda_batch_run`复用本次已编译的simv，避免重复编译；每项串行运行。

| 检查 | 命令或日志 | 结果 |
|---|---|---|
| 提交与工作区基线 | `git merge-base --is-ancestor e3ae325f8d HEAD`及`55dc0aedf6`；`git status --short` | 两祖先检查成功；初始工作区干净 |
| 远端完整编译 | `make eda_compile tc=basicTest ts=virtual_base_sequence mode=base_fun`；`.eda_remote/cas_prefill_compile_20261105_v2.launch.log` | VCS/KDB成功；无编译错误 |
| focused首次运行 | `make eda_run tc=tc_lsq_amo_admission_soft mode=base_fun seed=20261105 wave=off`；`cas_prefill_soft_20261105.launch.log` | 失败：SW/SD同编码误入LR配对；已定位修复，保留失败日志 |
| focused修复复测 | 同上；`cas_prefill_soft_20261105_retry.launch.log` | TEST CASE PASSED；UVM_ERROR=0、UVM_FATAL=0；26次预期fatal被捕获 |
| focused最终扩充复测 | 同上；`cas_prefill_soft_20261105_final.launch.log` | TEST CASE PASSED；UVM_ERROR=0、UVM_FATAL=0；包含实际STD端口和掩码检查，26次预期fatal被捕获 |
| tc_sanity | `make eda_run tc=tc_sanity mode=base_fun seed=20261105 wave=off`；`cas_prefill_sanity_20261105.launch.log` | TEST CASE PASSED；UVM_ERROR=0、UVM_FATAL=0；1400000ns结束，保留无主表场景既有等待告警 |
| 基础vseq | `make eda_batch_run tc=basicTest ts=virtual_base_sequence mode=base_fun seed=20261105 wave=off`；`cas_prefill_basic_20261105.launch.log` | TEST_PASS；UVM_ERROR=0、UVM_FATAL=0 |
| 普通real-dispatch | `make eda_batch_run tc=basicTest ts=memblock_dispatch_real_smoke_vseq mode=base_fun cfg=tc_dispatch_real_smoke seed=20261105 wave=off`；`cas_prefill_dispatch_20261105.launch.log` | TEST_PASS；UVM_ERROR=0、UVM_FATAL=0 |
| CAS.W/D/Q真实接口 | 见下方参数矩阵；`cas_prefill_real_W/D/Q_20261105.launch.log` | 三项均TEST_PASS；UVM_ERROR=0、UVM_FATAL=0；均有完整组与最终提交 |
| 静态、链接、review格式 | `git diff --check`；四参数定义/load/快照/cfg检查；cfg key声明与重复项；Markdown与review格式检查；旧路径检索 | 全部通过；23个review源码块均在紧邻位置给出中文伪代码 |

真实CAS smoke公共命令：

```bash
make eda_batch_run tc=basicTest ts=memblock_dispatch_real_smoke_vseq \
  mode=base_fun cfg=tc_dispatch_real_amo_smoke seed=20261105 wave=off \
  note=cas_prefill_<W|D|Q> plus_arg='<下述参数矩阵>'
```

参数矩阵：`MEMBLOCK_MAIN_TRANS_NUM=8`；`MEMBLOCK_AMO_CAS_PREFILL_EN_1_WT=100`；距离三档`1/0/0`；22个非CAS AMO opcode权重全0；W/D/Q三项权重分别选`1/0/0`、`0/1/0`、`0/0/1`。保留cfg中的`AMO_ISSUE_ORDER_MODE=2`、`AMO_STD_SHUFFLE_EN=1`、主存物理范围与合法PTE设置。

W/D各建4组；Q建2组。Q最终UID7 normal terminal、ROB commit与vseq正常结束，证明两个SD前置、四槽shuffle与既有atomic生命周期闭环。没有依据final WB宣称CAS比较实际成功。

最终日志判读同时要求通过标记、UVM_ERROR/UVM_FATAL计数0及正常`$finish`。基础、普通dispatch及W/D/Q使用`TEST_PASS`标记；focused与tc_sanity使用`TEST CASE PASSED`标记。`eda_run`包含重编译，最终focused与tc_sanity均使用最终源码编译结果；此前W/D/Q通过后仅追加了software-only测试与文档，生产实现未再改变。

## 11. 实现与 Plan 不一致项

本节明确区分执行前原方案与`IMPLEMENTATION_DELTA`，后者不是原计划已覆盖的内容。

| 原计划 | 当前实现 | 原因与影响 | 处理 |
|---|---|---|---|
| `init_from_plus()/validate_runtime_config()` | 接入实际`load_from_plus()/validate_and_clamp()` | 当前基线已有正式入口；不新增第二配置真源 | 已回写delta1，参数链见第2节 |
| `validate_all_cas_prefill_groups()` | `check_all_cas_prefill_groups()` | 参数规则要求新增校验以check开头 | 已回写delta1，源码与中文伪代码见第8节 |
| 原计划旧AMO总计划位于undo | 同步实际do位置 | 旧计划已归档 | 已回写delta6 |
| 通用执行规则最后commit | subagent不暂存、不commit，主agent最终提交 | 保持实现与独立审查职责分离 | 已回写delta7 |

其它功能目标保持原方案：固定长度、low直接替换、未来high/owner、四槽冻结、整组回滚、统一UID占用、写保护和静态审计均已落实。源码片段与逐段中文伪代码见第2至8节，对应上述命名与入口差异。

## 12. Plan 未说明但 Coding 落实的细节

| 必要细节 | 原计划未展开部分与作用 | 源码及解释 | 是否回写 |
|---|---|---|---|
| packed描述及二分账本定位 | 原计划要求冻结内容一致，但未选比较实现；按位比较覆盖全部字段，二分控制消费成本 | 第4.1、5.4节源码与中文伪代码 | delta2 |
| LR/SC同地址双向保护 | 原计划写保护若直接搬SC会破坏same_addr；现在新组避开在先SC，新同地址SC破坏旧组时取消本次配对 | 第6.3、7.2节源码与中文伪代码 | delta3 |
| 64位距离权重累计 | 三份合法大权重可能溢出旧32位通用helper | 第4.3节源码与中文伪代码 | delta4 |
| 显式固定路由窗口保守拒绝 | 首版不实现物理范围联合重建，拒绝范围比特定alias子场景更保守 | 第2节源码与中文伪代码 | delta5 |
| 分类与opcode一起识别LR | SW/SD与LR同编码，首次focused用新Store触发错误SC；两个建表入口和配对helper要求AMO分类 | 第7.2节源码与中文伪代码 | delta8 |

## 13. 风险、性能与审查结论

- 新扫描只在建表期间运行；保护扫描pending owner，不扫描已完成历史；消费按账本二分；最终审计只遍历每组low到owner的实际区间。没有每拍全表扫描、driver等待或新运行期状态机。
- 虚拟地址保护以相同TLB lookup key为前提；不同物理别名、跨动态CSR地址空间组合、外部一致性写入和独立故障注入不属于“必然CAS比较成功”的保证。PPN reuse和显式固定路由窗口组合已拒绝。
- 自然对齐、无主动异常字段是本激励保证；不覆盖外部PMP/PMA/TLB故障专项的合法异常行为。没有添加结果checker，不以正常WB区分CAS成功或失败。
- CBO.ZERO保护已通过软件判定验证；当前V2公共参数本来就禁止随机CBO权重，未改变该既有能力边界。
- 用户要求的coding、中文文档、review、基础及专项验证均在本worktree完成。对照git diff自查未发现遗留blocker；计划已归档do，review按规则保留undo。状态为`COMPLETE`，没有未完成的必需测试或环境阻塞；上述结果检查及别名边界仍是明确限制。

## 14. 非本次修改的逻辑分析

初始`git status --short`为空；当前所有源码、参数、配置与文档变更均属于本CAS prefill主题。没有混入其它功能修改。`base_fun`与`.eda_remote`是ignored仿真产物，保留准确证据但不纳入源码审查；没有改RTL、agent或公共规则。subagent未提交，最终提交由主agent完成。
