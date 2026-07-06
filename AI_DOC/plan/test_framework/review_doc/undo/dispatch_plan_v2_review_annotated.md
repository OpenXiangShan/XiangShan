# 可参数化的测试激励框架设计计划

> [审查结论] 原方案整体方向可行：以主控制表、TLB 表、状态表和三个轻量发射队列作为统一数据源，能够支撑可参数化随机和 directed case。暂不建议另起替代优化方案；更合理的做法是在现有方案上补清关键边界，包括 TLB key、状态并发更新、flush/replay 时序、LSQ commit 精确定义、plus 初始化与文件集成。下面的 `REVIEW` 注释为终稿需要保留的实现风险提示。

> [REVIEW 处理规则] 后续处理任一 `REVIEW` 注释时，必须先确认该 review 指出的真实风险，再把修正方案整合进正文；正文修正完成后由 subagent 独立确认风险已解决，确认后才删除对应 `REVIEW` 注释。

## 1. 背景与目标

为 MemBlock UT 环境设计一套可参数化的测试激励框架。该框架用于生成并发送 load、store、AMO、prefetch 等访存请求，所有测试用例均基于该框架生成。

生成的测试用例必须符合 MemBlock DUT 的合法行为，尤其需要保证：

- 各信号字段赋值合法。
- 字段之间的依赖关系满足 DUT 约束。
- 入队、TLB 映射、队列路由、发射、写回、异常处理和提交的时序符合 DUT 行为。
- 所有公共表和公共队列最终统一存入 `common_data_transaction.sv`，并通过单例模式共享给其他环境。

本文只描述激励框架设计方案，不在本文内展开验收表。开发任务开始前需要由独立验收文档定义具体验收场景、通过标准和回归检查项。

## 2. 核心要求

本任务需要实现第一套主控制表生成逻辑，以及一套发射队列相关 task，覆盖 TLB 地址映射、队列路由、发射控制、异常处理和提交处理等完整流程。

公共数据统一存放在 `common_data_transaction.sv` 中，包括：

- 第一套主控制表。
- TLB 相关表。
- 主任务状态表。
- load 发射队列。
- STA 发射队列。
- STD 发射队列。

存储要求：

- 所有公共数据必须存放在 `common_data_transaction.sv` 中；按连续 `uid` 访问的主表、状态表和按条目追溯的 TLB 表优先使用动态数组，按 TLB lookup key、活跃 `robIdx`、`lqIdx`、`sqIdx` 访问的稀疏映射使用关联数组。
- load、STA、STD 不再维护为三张子表，而是维护为三个发射队列；队列同样存放在 `common_data_transaction.sv` 中。
- 队列元素建议保存轻量调度快照，而不是完整 transaction。最小推荐字段为 `uid`、`rob_key`、`target`、`send_pri`、`ready_cycle` 或 `delay_left`、`replay_seq`；可选缓存 `lqIdx/sqIdx/numLsElem` 等高频字段。完整 transaction 内容仍通过 `uid` 回查主控制表、状态表和 TLB 表。
- 三个发射队列只作为待发射缓冲。队列项成功发射到对应流水线后即可从队列中删除，不再依赖队列做历史追溯。
- 发射后的追溯、debug、replay、redirect 和 commit 判断统一通过主控制表和主任务状态表完成。
- 默认不维护 `robIdx -> queue_index/status` 辅助索引。队列删除、replay 重新入队和历史追溯均以主控制表和主任务状态表为准；发射成功后删除当前队列项，replay 时由状态表设置 `replay_pending/replay_target_mask/replay_seq` 后重新路由入队。若后续 profiling 证明队列扫描成为瓶颈，可以额外维护非权威辅助索引，但辅助索引不得作为状态真值来源，且不建议保存易失效的 `queue_index`。

存储方式需要以性能为优先目标，重点评估：

- 读写速度。
- 内存占用。
- 按 `robIdx`、`lqIdx`、`sqIdx`、TLB lookup key 等索引访问的效率。
- 主表、TLB 表、状态表和三个发射队列之间的同步维护成本。
- 队列调度扫描成本，尤其是 `global_send_pri_en=1` 时跨 load/STA/STD 三个队列查找全局最大实际发射优先级的成本。

## 3. 命名统一

文档和实现中统一使用以下命名，避免同一字段出现多种写法。

| 原错误或不统一命名 | 统一命名 | 说明 |
|---|---|---|
| `dispath`、`dipatch` | `dispatch` | 发射状态或发射计划命名 |
| `lsq_crtl` | `lsq_ctrl` | LSQ 分配与回收控制 task |
| `post_rodmoize` | `post_randomize` | SystemVerilog 随机化回调 |
| `VDDR`、`vadvr`、`BADR` | `vaddr` | 虚拟地址字段 |
| `futype` | `fuType` | 功能单元类型 |
| `Robidx`、`robidx` | `robIdx` | ROB 索引 |
| `lqidx` | `lqIdx` | Load Queue 索引 |
| `sqidx` | `sqIdx` | Store Queue 索引 |
| `TLBPF`、`TLB_PF` | `tlbPF` | TLB page fault 控制字段 |
| `TLB GPF`、`TLBgPF`、`TLB_gPF` | `tlbGPF` | TLB guest page fault 控制字段 |
| `tlbaf`、`TLBAF` | `tlbAF` | TLB access fault 控制字段 |
| `PMA_af`、`after forty` | `pmaAF` | PMA access fault 控制字段 |
| `common data transaction DSV`、`common_data transaction.SV` | `common_data_transaction.sv` | 公共数据单例文件 |
| `ent_q` | `enq` | 入队状态 |
| `wb` | `writeback` | 写回状态 |

说明：原文件 `dispath_plan.md` 保留不动，v2 文件使用新命名 `dispatch_plan_v2.md`；文档内容中统一使用 `dispatch`。

## 4. 文件分工与参数管理

### 4.1 公共数据文件：`common_data_transaction.sv`

`common_data_transaction.sv` 是全框架唯一公共数据单例，负责保存所有表、队列和状态。该文件不应只定义一组散落全局变量，而应定义一个统一 owner class，并通过 `common_data_transaction::get()` 提供唯一入口。

必须存放在该文件中的数据包括：

- 主控制表：按连续 `uid` 索引的动态数组，例如 `main_table_by_uid[]`。
- TLB 表：按连续 `uid` 索引的动态数组 `tlb_table_by_uid[]`，以及按 `{vpn, asid, vmid, s2xlate}` 翻译 key 索引到 `uid` 的关联数组 `tlb_uid_by_key[tlb_lookup_key_t]`。
- 状态表：按连续 `uid` 索引的动态数组，例如 `status_by_uid[]`，状态条目中必须包含 `active` 字段。
- load 发射队列：例如 `load_issue_q[$]`。
- STA 发射队列：例如 `sta_issue_q[$]`。
- STD 发射队列：例如 `std_issue_q[$]`。
- 稳定辅助索引：例如 `uid_by_active_rob[rob_key]`、`uid_by_lq[lqIdx]`、`uid_by_sq[sqIdx]`。不维护 `queue_pos_by_rob` 这类易失效队列下标索引。
- 运行时 MMU CSR 镜像：例如 `mmu_csr_runtime_state`，由 `csr_agent monitor` 根据 DUT CSR 更新实时维护。
- 全局控制状态：例如 `flush_in_progress`、`active_redirect`、`global_issue_epoch`、`issue_freeze_ack` 和 `exception_event_q`。

当前按单轮 testcase 设计，不引入 `test_epoch`。公共数据的权威主键采用 TB 内部 `uid/alloc_seq`，而不是裸 `robIdx`。`uid` 在本轮 testcase 内按 `0 .. main_trans_num-1` 连续分配，不发给 DUT，也不参与 DUT 行为判断；`robIdx_flag/value` 仍作为主表字段保存，用于发给 DUT、做 ROB 顺序比较、flush/redirect/commit 判断。`robIdx` 允许按 ROB 最大容量回绕，但只有状态表中 `active=1` 的条目会建立 `robIdx -> uid` 活跃映射，因此 DUT monitor 只会匹配当前 DUT 活跃窗口内的条目。

推荐基础格式如下：

```systemverilog
typedef int unsigned memblock_uid_t;

typedef struct packed {
  bit                  flag;
  logic [ROB_W-1:0]    value;
} rob_key_t;

typedef struct packed {
  logic [VPN_W-1:0]    vpn;
  logic [ASID_W-1:0]   asid;
  logic [VMID_W-1:0]   vmid;
  bit                  s2xlate;
} tlb_lookup_key_t;

typedef struct packed {
  bit                  valid;
  logic [ASID_W-1:0]   asid;
  logic [VMID_W-1:0]   vmid;
  bit                  s2xlate;
  bit                  virt_en;
  priv_mode_e          priv_mode;
  // 实际实现需要继续补齐影响 S1/S2 翻译和权限判断的 CSR 字段。
  longint unsigned     update_seq;
} mmu_csr_runtime_state_t;

typedef struct packed {
  memblock_uid_t       uid;
  rob_key_t            rob_key;
  issue_target_e       target;      // LOAD/STA/STD
  int unsigned         send_pri;
  longint unsigned     ready_cycle;
  int unsigned         replay_seq;
  bit                  has_lqIdx;
  int unsigned         lqIdx;
  bit                  has_sqIdx;
  int unsigned         sqIdx;
  int unsigned         numLsElem;
} issue_q_entry_t;

class common_data_transaction extends uvm_object;
  static common_data_transaction m_inst;

  int unsigned main_trans_num;
  memblock_uid_t next_uid;

  main_control_transaction main_table_by_uid[];
  status_transaction       status_by_uid[];
  tlb_transaction          tlb_table_by_uid[];
  memblock_uid_t           tlb_uid_by_key[tlb_lookup_key_t];

  mmu_csr_runtime_state_t  mmu_csr_runtime_state;

  memblock_uid_t uid_by_active_rob[rob_key_t];
  memblock_uid_t uid_by_lq[lq_idx_t];
  memblock_uid_t uid_by_sq[sq_idx_t];

  issue_q_entry_t load_issue_q[$];
  issue_q_entry_t sta_issue_q[$];
  issue_q_entry_t std_issue_q[$];

  bit flush_in_progress;
  redirect_payload_t active_redirect;
  int unsigned global_issue_epoch;
  bit issue_freeze_ack;
  exception_event_t exception_event_q[$];

  protected function new(string name = "common_data_transaction");
    super.new(name);
  endfunction

  static function common_data_transaction get();
    if (m_inst == null) m_inst = new();
    return m_inst;
  endfunction
endclass
```

生命周期 API 必须在 `common_data_transaction.sv` 中明确：

- `reset_all_tables(main_trans_num)`：主表随机生成或手动导入之前的第一步必须调用。该 API 设置 `next_uid=0`，删除并按本轮 `main_trans_num` 重新分配 `main_table_by_uid/status_by_uid/tlb_table_by_uid`，初始化所有状态条目的 `active=0` 且所有 pending、dispatch、writeback、commit、replay、redirect、exception 状态为 0；同时清空 `tlb_uid_by_key`、三个发射队列、`uid_by_active_rob`、`uid_by_lq`、`uid_by_sq`、`exception_event_q`，并清零 `flush_in_progress`、`active_redirect`、`global_issue_epoch`、`issue_freeze_ack`。`mmu_csr_runtime_state` 只允许用 `csr_csr_common.sv` 的初始值做 reset baseline，reset 后必须由 `csr_agent monitor` 持续更新，后续 task 不得再次把初始值当运行时真值使用。
- `alloc_uid()`：主表导入或随机生成时调用，只返回连续 `uid` 并写入 transaction，不建立活跃映射。该 API 必须检查 `uid < main_trans_num`；主表生成结束后必须检查 `next_uid == main_trans_num`，防止少分配、多分配或跳号破坏连续数组索引假设。
- `rob_key_t`：统一使用 `robIdx_flag/value` 两部分表示 ROB 环形 key，禁止只用裸 `robIdx_value` 作为 active 映射 key。
- `init_status_for_uid(uid)`：主表条目写入后调用，为该 `uid` 创建默认状态对象，并保持 `active=0`。
- `activate_uid(uid, op_behavior)`：LSQ 入队 fire 后或 non-LSQ admission 成功后调用。该 API 必须检查 `status_by_uid[uid].active==0`，并检查 `uid_by_active_rob[rob_key]` 不存在同 key 活跃项；随后置 `active=1`，建立 `uid_by_active_rob[rob_key]=uid`。只有 `op_behavior.needAlloc` 需要 LQ/SQ 时，才按 `lqIdx/sqIdx/numLsElem` 建立 `uid_by_lq/uid_by_sq`，并检查对应 LSQ 范围内不存在其他 active uid；`ATOMIC` 等 `needAlloc=0` 条目不得建立 LQ/SQ 映射。
- `lookup_active_uid_by_rob(rob_key)`：DUT monitor 只有 `robIdx` 时使用。查到 `uid` 后还必须检查 `status_by_uid[uid].active==1`；解析失败时默认报错，不允许静默更新当前状态。只有能明确证明该事件属于已 retire 或已 flush 的迟到反馈，且不会更新任何当前状态时，才允许记录后忽略。
- `retire_active_uid(uid)`：commit/deq 完成或 flush/cancel 确认后调用。该 API 必须校验 `uid_by_active_rob[rob_key] == uid`，删除 active rob 映射，置 `active=0`；若该 `uid` 曾分配 LQ/SQ，再按 `lqIdx/sqIdx/numLsElem` 清理完整 `uid_by_lq/uid_by_sq` 范围，范围跨环形队列尾部时必须按 modulo 或 flag/value 规则清理。该 API 只释放 active 映射和 LSQ 资源，不删除 `main_table_by_uid[uid]`、`status_by_uid[uid]` 或 `tlb_table_by_uid[uid]` 的历史内容。
- `retire_tlb_lookup_index(uid, reason)`：可选 API，只清理或重建 `tlb_uid_by_key` 这类 lookup 索引，不删除 `tlb_table_by_uid[uid]`。调用场景包括 CSR/SFENCE 导致 `csr_update_seq` 失效、redirect/flush 取消未完成请求、testcase reset 或 debug 配置要求重建映射。commit/deq 本身默认不需要清理 `tlb_uid_by_key`；若为了避免 key 复用歧义而清理，也必须保留 per-uid TLB 追溯记录。
- `end_test_check()`：testcase 结束前调用，必须扫描 `status_by_uid[0:main_trans_num-1]`，确认无 `active`、无 `replay_pending/redirect_pending/exception_pending`、无未完成 dispatch/writeback/commit；同时检查三个发射队列为空、active rob/lq/sq 映射为空、`exception_event_q` 为空、全局 flush 控制状态为 idle。

数据结构选择原则：

- `uid` 连续递增，因此主表、状态表和按 `uid` 追溯的 TLB 表优先使用动态数组，读写速度和内存效率优于关联数组。
- TLB lookup key、活跃 `robIdx`、`lqIdx`、`sqIdx` 属于稀疏或环形复用 key，使用关联数组保存当前 active 映射。
- replay、redirect、异常和 commit 时通过 active 映射先定位 `uid`，再访问主表和状态表。
- 队列与状态表之间通过 `uid` 关联，避免复制大 transaction 数据。
- 历史追溯数据和活跃索引必须分层管理：`main_table_by_uid[]`、`status_by_uid[]`、`tlb_table_by_uid[]` 保留到 `end_test_check()` 完成；`uid_by_active_rob`、`uid_by_lq`、`uid_by_sq` 和 `tlb_uid_by_key` 是可释放或可重建索引，不作为唯一历史记录来源。
- 便于多环境共享同一份单例数据。

### 4.2 参数文件：`seq_csr_common.sv`

测试框架中所有可配置参数统一放入 `seq_csr_common.sv`，包括：

- 主表生成数量。
- 入队宽度和每拍入队数量。
- `load_pip_num`、`sta_pip_num`、`std_pip_num`，分别控制每拍进入 load、STA、STD 流水线的最大数量。
- TLB 权重参数。
- 异常注入权重。
- DCache 回复控制权重。
- commit 延迟。
- `global_send_pri_en`。
- `send_pri` 默认值和权重。
- `send_pri_std` 默认值和权重。
- load 地址复用到 store 的权重参数。
- store 地址复用到后续 load 的权重参数。
- 手动主表是否允许地址复用后处理的显式开关。
- 地址范围约束参数。
- TLB/PTE 权限位、N 位、V 位等随机权重参数。

注意：`seq_csr_common.sv` 不保存 CSR/虚拟化实时状态，不作为任何基于 CSR 的 task 的运行时信息来源。`csr_csr_common.sv` 也只作为 testcase reset 或初始化阶段的 MMU CSR baseline，不是运行时真值。所有需要 CSR/虚拟化状态的逻辑，包括 TLB 映射、L2TLB 回复、异常判断和权限修正，都必须读取 `common_data_transaction.sv` 中由 `csr_agent monitor` 实时维护的 `mmu_csr_runtime_state`。

`seq_csr_common.sv` 只保存解析后的最终配置值。plusargs 的解析和默认值管理通过 `plus.sv` 完成。

`seq_csr_common::init()` 的统一调用入口放在 `memblock_base_sequence::pre_body()`。所有主表生成、手动主表导入、TLB 表生成和 transaction `randomize()` 必须发生在 `pre_body()` 调用 `seq_csr_common::init()` 之后。具体要求：

- `memblock_base_sequence::pre_body()` 开始阶段调用 `seq_csr_common::init()`，随后再调用公共数据生命周期初始化和各类生成 task。
- `seq_csr_common::init()` 必须具备幂等性，内部通过 `initialized` guard 保证 plusargs 只解析一次；多个 sequence 重复调用时应直接复用第一次解析后的结果，禁止重复解析导致配置变化。
- 若后续允许多个 sequence 并发启动，`init()` 需要通过 semaphore 或等价机制保护 `initialized` 检查和 plusargs 解析临界区，保证并发下也只会有一次真实解析。
- transaction 的 `pre_randomize()` 或生成 task 在调用 `randomize()` 前必须检查 `seq_csr_common::is_initialized()`；若未初始化，直接 `uvm_fatal`，禁止带默认空配置继续随机。
- `seq_csr_common.sv` 不负责 testcase 生命周期清表；公共表和状态表的 reset 仍由 `common_data_transaction.sv` 的生命周期 API 负责。

`seq_csr_common::init()` 在 plusargs 解析后必须立即调用 `validate_and_clamp()`，集中完成非法值保护。保护策略分为两类：

- `fatal`：配置无法唯一修正，或者自动修正会改变 testcase 语义时直接停止。例如同一 `dist` 组权重全为 0、`main_trans_num=0`、`paddr_range=0`、地址 `base+range` 溢出、非法枚举模式、手动主表模式但条目数为 0。
- `clamp + warning`：配置是上限或建议值，压回合法边界后语义仍明确时打印 `uvm_warning` 并继续。例如 `send_pri_default/send_pri_std_default` 超出 0 到 100 时压到边界，地址复用概率超出 0 到 100 时压到边界，`load_pip_num/sta_pip_num/std_pip_num` 大于真实流水线数量时压到真实流水线数量。

所有被 transaction `dist` 约束使用的权重必须按组检查，禁止出现整组权重全 0。典型组包括 `send_pri_low/mid/high`、`send_pri_std_low/mid/high`、delay 权重、TLB/PTE 每个 bit 的 0/1 权重、异常类型权重、DCache 回复权重。若某个权重来自有符号 plusarg，解析时应先进入 signed 临时变量，再检查是否小于 0；禁止负数权重经 unsigned 转换后变成大正数。

地址类 plusargs 也必须显式防护。`paddr_base/paddr_range` 这类十六进制参数只接受非负 hex 字符串，禁止 `-1` 这类负数文本被读入 unsigned 后变成大正数；实现时可使用 `read_non_negative_longint_plusarg()` 或等价 helper。真实 DUT/agent 上限参数，例如 `real_lsq_enq_max/real_enq_width/real_load_pipe_num/real_sta_pipe_num/real_std_pipe_num`，在用于 clamp 前必须先检查非 0，否则 `min=1/max=0` 会导致 clamp 规则本身非法。`real_lsq_enq_max` 是当前 LSQ enqueue 统一 slot 宽度镜像，默认 8；`real_enq_width` 作为历史兼容字段保留，必须与 `real_lsq_enq_max` 相等。后续新增任何 `dist` 权重组时，必须同步在 `validate_and_clamp()` 中添加“全 0 fatal”检查。

### 4.3 plus 管理模板：`plus.sv`

`plus.sv` 负责集中管理 plusargs。参数可以通过 plusargs 直接指定，未指定时使用默认值。

模板示例：

```systemverilog
package memblock_plus_pkg;

  class memblock_plus_cfg;
    static int unsigned main_trans_num = 100;
    // 0: random generate main table; 1: use manually configured associative array.
    static bit use_manual_main_table = 1'b0;

    static int unsigned enq_per_cycle = 4;
    // 1: each get_enq_per_cycle() randomizes in [1:real_enq_width].
    static bit enq_per_cycle_rand_en = 1'b0;
    static int unsigned load_pip_num = 3;
    static int unsigned sta_pip_num = 2;
    static int unsigned std_pip_num = 2;

    // op_class/fuOpType selection weights. The selected op then fixes rfWen/fpWen.
    static int unsigned op_class_int_load_wt = 8;
    static int unsigned op_class_fp_load_wt = 1;
    static int unsigned op_class_store_wt = 6;
    static int unsigned op_class_prefetch_wt = 1;
    static int unsigned op_class_amo_wt = 1;

    static bit lsq_resync_on_mismatch = 1'b0;

    static bit global_send_pri_en = 1'b0;
    static int unsigned send_pri_default = 50;
    static int unsigned send_pri_low_wt = 1;
    static int unsigned send_pri_mid_wt = 8;
    static int unsigned send_pri_high_wt = 1;
    static int unsigned send_pri_std_default = 50;
    static int unsigned send_pri_std_low_wt = 1;
    static int unsigned send_pri_std_mid_wt = 8;
    static int unsigned send_pri_std_high_wt = 1;

    // 0..100; 0 disables address reuse injection.
    static int unsigned ld_to_st_addr_reuse_wt = 0;
    static int unsigned st_to_ld_addr_reuse_wt = 0;
    // Manual table keeps user-directed addresses by default.
    static bit manual_addr_reuse_en = 1'b0;

    static int unsigned delay_0_wt = 10;
    static int unsigned delay_1_20_wt = 5;
    static int unsigned delay_21_50_wt = 1;

    // TLB/PTE bit weights; each bit is constrained by plus-controlled 0/1 weights.
    static int unsigned tlb_pte_r_1_wt = 8;
    static int unsigned tlb_pte_r_0_wt = 1;
    static int unsigned tlb_pte_w_1_wt = 6;
    static int unsigned tlb_pte_w_0_wt = 1;
    static int unsigned tlb_pte_x_1_wt = 4;
    static int unsigned tlb_pte_x_0_wt = 1;
    static int unsigned tlb_pte_u_1_wt = 1;
    static int unsigned tlb_pte_u_0_wt = 8;
    static int unsigned tlb_pte_g_1_wt = 1;
    static int unsigned tlb_pte_g_0_wt = 8;
    static int unsigned tlb_pte_a_1_wt = 8;
    static int unsigned tlb_pte_a_0_wt = 1;
    static int unsigned tlb_pte_d_1_wt = 8;
    static int unsigned tlb_pte_d_0_wt = 1;
    static int unsigned tlb_pte_n_1_wt = 1;
    static int unsigned tlb_pte_n_0_wt = 8;
    static int unsigned tlb_pte_v_1_wt = 9;
    static int unsigned tlb_pte_v_0_wt = 1;

    static longint unsigned paddr_base = 64'h8000_0000;
    static longint unsigned paddr_range = 64'h1000_0000;

    // Real DUT/agent limits. Bind these to local env parameters during coding.
    static int unsigned real_lsq_enq_max = 8;
    // Compatibility alias. Must equal real_lsq_enq_max.
    static int unsigned real_enq_width = 8;
    static int unsigned real_load_pipe_num = 3;
    static int unsigned real_sta_pipe_num = 2;
    static int unsigned real_std_pipe_num = 2;

    static function void load_from_plusargs();
      read_non_negative_int_plusarg("MEMBLOCK_MAIN_TRANS_NUM", main_trans_num);
      use_manual_main_table = $test$plusargs("MEMBLOCK_USE_MANUAL_MAIN_TABLE");
      read_non_negative_int_plusarg("MEMBLOCK_ENQ_PER_CYCLE", enq_per_cycle);
      read_bit_plusarg("MEMBLOCK_ENQ_PER_CYCLE_RAND_EN", enq_per_cycle_rand_en);
      read_non_negative_int_plusarg("MEMBLOCK_REAL_LSQ_ENQ_MAX", real_lsq_enq_max);
      read_non_negative_int_plusarg("MEMBLOCK_LOAD_PIP_NUM", load_pip_num);
      read_non_negative_int_plusarg("MEMBLOCK_STA_PIP_NUM", sta_pip_num);
      read_non_negative_int_plusarg("MEMBLOCK_STD_PIP_NUM", std_pip_num);

      read_non_negative_int_plusarg("MEMBLOCK_OP_CLASS_INT_LOAD_WT", op_class_int_load_wt);
      read_non_negative_int_plusarg("MEMBLOCK_OP_CLASS_FP_LOAD_WT", op_class_fp_load_wt);
      read_non_negative_int_plusarg("MEMBLOCK_OP_CLASS_STORE_WT", op_class_store_wt);
      read_non_negative_int_plusarg("MEMBLOCK_OP_CLASS_PREFETCH_WT", op_class_prefetch_wt);
      read_non_negative_int_plusarg("MEMBLOCK_OP_CLASS_AMO_WT", op_class_amo_wt);
      lsq_resync_on_mismatch = $test$plusargs("MEMBLOCK_LSQ_RESYNC_ON_MISMATCH");

      global_send_pri_en = $test$plusargs("MEMBLOCK_GLOBAL_SEND_PRI_EN");
      read_non_negative_int_plusarg("MEMBLOCK_SEND_PRI_DEFAULT", send_pri_default);
      read_non_negative_int_plusarg("MEMBLOCK_SEND_PRI_LOW_WT", send_pri_low_wt);
      read_non_negative_int_plusarg("MEMBLOCK_SEND_PRI_MID_WT", send_pri_mid_wt);
      read_non_negative_int_plusarg("MEMBLOCK_SEND_PRI_HIGH_WT", send_pri_high_wt);
      read_non_negative_int_plusarg("MEMBLOCK_SEND_PRI_STD_DEFAULT", send_pri_std_default);
      read_non_negative_int_plusarg("MEMBLOCK_SEND_PRI_STD_LOW_WT", send_pri_std_low_wt);
      read_non_negative_int_plusarg("MEMBLOCK_SEND_PRI_STD_MID_WT", send_pri_std_mid_wt);
      read_non_negative_int_plusarg("MEMBLOCK_SEND_PRI_STD_HIGH_WT", send_pri_std_high_wt);

      read_non_negative_int_plusarg("MEMBLOCK_LD_TO_ST_ADDR_REUSE_WT", ld_to_st_addr_reuse_wt);
      read_non_negative_int_plusarg("MEMBLOCK_ST_TO_LD_ADDR_REUSE_WT", st_to_ld_addr_reuse_wt);
      manual_addr_reuse_en = $test$plusargs("MEMBLOCK_MANUAL_ADDR_REUSE_EN");

      read_non_negative_int_plusarg("MEMBLOCK_DELAY_0_WT", delay_0_wt);
      read_non_negative_int_plusarg("MEMBLOCK_DELAY_1_20_WT", delay_1_20_wt);
      read_non_negative_int_plusarg("MEMBLOCK_DELAY_21_50_WT", delay_21_50_wt);

      read_non_negative_int_plusarg("MEMBLOCK_TLB_PTE_R_1_WT", tlb_pte_r_1_wt);
      read_non_negative_int_plusarg("MEMBLOCK_TLB_PTE_R_0_WT", tlb_pte_r_0_wt);
      read_non_negative_int_plusarg("MEMBLOCK_TLB_PTE_W_1_WT", tlb_pte_w_1_wt);
      read_non_negative_int_plusarg("MEMBLOCK_TLB_PTE_W_0_WT", tlb_pte_w_0_wt);
      read_non_negative_int_plusarg("MEMBLOCK_TLB_PTE_X_1_WT", tlb_pte_x_1_wt);
      read_non_negative_int_plusarg("MEMBLOCK_TLB_PTE_X_0_WT", tlb_pte_x_0_wt);
      read_non_negative_int_plusarg("MEMBLOCK_TLB_PTE_U_1_WT", tlb_pte_u_1_wt);
      read_non_negative_int_plusarg("MEMBLOCK_TLB_PTE_U_0_WT", tlb_pte_u_0_wt);
      read_non_negative_int_plusarg("MEMBLOCK_TLB_PTE_G_1_WT", tlb_pte_g_1_wt);
      read_non_negative_int_plusarg("MEMBLOCK_TLB_PTE_G_0_WT", tlb_pte_g_0_wt);
      read_non_negative_int_plusarg("MEMBLOCK_TLB_PTE_A_1_WT", tlb_pte_a_1_wt);
      read_non_negative_int_plusarg("MEMBLOCK_TLB_PTE_A_0_WT", tlb_pte_a_0_wt);
      read_non_negative_int_plusarg("MEMBLOCK_TLB_PTE_D_1_WT", tlb_pte_d_1_wt);
      read_non_negative_int_plusarg("MEMBLOCK_TLB_PTE_D_0_WT", tlb_pte_d_0_wt);
      read_non_negative_int_plusarg("MEMBLOCK_TLB_PTE_N_1_WT", tlb_pte_n_1_wt);
      read_non_negative_int_plusarg("MEMBLOCK_TLB_PTE_N_0_WT", tlb_pte_n_0_wt);
      read_non_negative_int_plusarg("MEMBLOCK_TLB_PTE_V_1_WT", tlb_pte_v_1_wt);
      read_non_negative_int_plusarg("MEMBLOCK_TLB_PTE_V_0_WT", tlb_pte_v_0_wt);

      read_non_negative_longint_plusarg("MEMBLOCK_PADDR_BASE", paddr_base);
      read_non_negative_longint_plusarg("MEMBLOCK_PADDR_RANGE", paddr_range);
    endfunction

    static function void read_non_negative_int_plusarg(string name, ref int unsigned dst);
      int signed tmp;
      if ($value$plusargs({name, "=%0d"}, tmp)) begin
        if (tmp < 0) begin
          `uvm_fatal("MEMBLOCK_PLUS", $sformatf("%s must not be negative, got %0d", name, tmp))
        end
        dst = int'(tmp);
      end
    endfunction

    static function void read_non_negative_longint_plusarg(string name, ref longint unsigned dst);
      string tmp_str;
      string parse_str;
      longint unsigned tmp;
      if ($value$plusargs({name, "=%s"}, tmp_str)) begin
        if (tmp_str.len() == 0 || tmp_str.substr(0, 0) == "-") begin
          `uvm_fatal("MEMBLOCK_PLUS", $sformatf("%s must be a non-negative hex value, got %s", name, tmp_str))
        end
        if (!is_legal_hex_string(tmp_str)) begin
          `uvm_fatal("MEMBLOCK_PLUS", $sformatf("%s has illegal hex value %s", name, tmp_str))
        end

        parse_str = tmp_str;
        if (tmp_str.len() >= 2 &&
            (tmp_str.substr(0, 1) == "0x" || tmp_str.substr(0, 1) == "0X")) begin
          parse_str = tmp_str.substr(2, tmp_str.len() - 1);
        end

        if ($sscanf(parse_str, "%h", tmp) != 1) begin
          `uvm_fatal("MEMBLOCK_PLUS", $sformatf("%s has illegal hex value %s", name, tmp_str))
        end
        dst = tmp;
      end
    endfunction

    static function bit is_legal_hex_string(string value);
      int unsigned start_idx = 0;
      int unsigned digit_cnt = 0;

      if (value.len() >= 2 &&
          (value.substr(0, 1) == "0x" || value.substr(0, 1) == "0X")) begin
        start_idx = 2;
      end

      for (int unsigned i = start_idx; i < value.len(); i++) begin
        byte c = value[i];
        if (c == "_") begin
          continue;
        end
        if ((c >= "0" && c <= "9") ||
            (c >= "a" && c <= "f") ||
            (c >= "A" && c <= "F")) begin
          digit_cnt++;
          continue;
        end
        return 1'b0;
      end

      return digit_cnt != 0;
    endfunction
  endclass

endpackage
```

`seq_csr_common.sv` 使用模板：

```systemverilog
`include "plus.sv"
import memblock_plus_pkg::*;

class seq_csr_common;
  static bit initialized = 1'b0;
  static semaphore init_sem = new(1);

  static task init();
    init_sem.get();
    if (initialized) begin
      init_sem.put();
      return;
    end

    memblock_plus_cfg::load_from_plusargs();
    validate_and_clamp();
    initialized = 1'b1;
    init_sem.put();
  endtask

  static function bit is_initialized();
    return initialized;
  endfunction

  static function void check_initialized(string caller = "seq_csr_common");
    if (!initialized) begin
      `uvm_fatal("SEQ_CSR_INIT", $sformatf("%s called before seq_csr_common::init()", caller))
    end
  endfunction

  static function void validate_and_clamp();
    fatal_if_zero("main_trans_num", memblock_plus_cfg::main_trans_num);
    fatal_if_zero("paddr_range", memblock_plus_cfg::paddr_range);
    fatal_if_zero("real_lsq_enq_max", memblock_plus_cfg::real_lsq_enq_max);
    fatal_if_zero("real_enq_width", memblock_plus_cfg::real_enq_width);
    fatal_if_zero("real_load_pipe_num", memblock_plus_cfg::real_load_pipe_num);
    fatal_if_zero("real_sta_pipe_num", memblock_plus_cfg::real_sta_pipe_num);
    fatal_if_zero("real_std_pipe_num", memblock_plus_cfg::real_std_pipe_num);

    if (memblock_plus_cfg::paddr_base + memblock_plus_cfg::paddr_range < memblock_plus_cfg::paddr_base) begin
      `uvm_fatal("SEQ_CSR_CFG", "paddr_base + paddr_range overflows")
    end

    clamp_int("send_pri_default", memblock_plus_cfg::send_pri_default, 0, 100);
    clamp_int("send_pri_std_default", memblock_plus_cfg::send_pri_std_default, 0, 100);
    clamp_int("ld_to_st_addr_reuse_wt", memblock_plus_cfg::ld_to_st_addr_reuse_wt, 0, 100);
    clamp_int("st_to_ld_addr_reuse_wt", memblock_plus_cfg::st_to_ld_addr_reuse_wt, 0, 100);

    clamp_int("real_lsq_enq_max", memblock_plus_cfg::real_lsq_enq_max, 1, 8);
    clamp_int("real_enq_width", memblock_plus_cfg::real_enq_width, 1, 8);
    if (memblock_plus_cfg::real_enq_width != memblock_plus_cfg::real_lsq_enq_max) begin
      `uvm_fatal("SEQ_CSR_CFG", "MEMBLOCK_REAL_ENQ_WIDTH must equal MEMBLOCK_REAL_LSQ_ENQ_MAX")
    end
    if (memblock_plus_cfg::enq_per_cycle == 0 ||
        memblock_plus_cfg::enq_per_cycle > memblock_plus_cfg::real_enq_width) begin
      `uvm_fatal("SEQ_CSR_CFG", "MEMBLOCK_ENQ_PER_CYCLE must be in [1:MEMBLOCK_REAL_ENQ_WIDTH]")
    end
    clamp_int("load_pip_num", memblock_plus_cfg::load_pip_num, 1, memblock_plus_cfg::real_load_pipe_num);
    clamp_int("sta_pip_num", memblock_plus_cfg::sta_pip_num, 1, memblock_plus_cfg::real_sta_pipe_num);
    clamp_int("std_pip_num", memblock_plus_cfg::std_pip_num, 1, memblock_plus_cfg::real_std_pipe_num);

    fatal_if_all_zero3("send_pri weights",
                       memblock_plus_cfg::send_pri_low_wt,
                       memblock_plus_cfg::send_pri_mid_wt,
                       memblock_plus_cfg::send_pri_high_wt);
    fatal_if_all_zero3("send_pri_std weights",
                       memblock_plus_cfg::send_pri_std_low_wt,
                       memblock_plus_cfg::send_pri_std_mid_wt,
                       memblock_plus_cfg::send_pri_std_high_wt);
    fatal_if_all_zero3("delay weights",
                       memblock_plus_cfg::delay_0_wt,
                       memblock_plus_cfg::delay_1_20_wt,
                       memblock_plus_cfg::delay_21_50_wt);
    fatal_if_all_zero5("op_class weights",
                       memblock_plus_cfg::op_class_int_load_wt,
                       memblock_plus_cfg::op_class_fp_load_wt,
                       memblock_plus_cfg::op_class_store_wt,
                       memblock_plus_cfg::op_class_prefetch_wt,
                       memblock_plus_cfg::op_class_amo_wt);

    fatal_if_all_zero2("tlb_pte_r weights", memblock_plus_cfg::tlb_pte_r_1_wt, memblock_plus_cfg::tlb_pte_r_0_wt);
    fatal_if_all_zero2("tlb_pte_w weights", memblock_plus_cfg::tlb_pte_w_1_wt, memblock_plus_cfg::tlb_pte_w_0_wt);
    fatal_if_all_zero2("tlb_pte_x weights", memblock_plus_cfg::tlb_pte_x_1_wt, memblock_plus_cfg::tlb_pte_x_0_wt);
    fatal_if_all_zero2("tlb_pte_u weights", memblock_plus_cfg::tlb_pte_u_1_wt, memblock_plus_cfg::tlb_pte_u_0_wt);
    fatal_if_all_zero2("tlb_pte_g weights", memblock_plus_cfg::tlb_pte_g_1_wt, memblock_plus_cfg::tlb_pte_g_0_wt);
    fatal_if_all_zero2("tlb_pte_a weights", memblock_plus_cfg::tlb_pte_a_1_wt, memblock_plus_cfg::tlb_pte_a_0_wt);
    fatal_if_all_zero2("tlb_pte_d weights", memblock_plus_cfg::tlb_pte_d_1_wt, memblock_plus_cfg::tlb_pte_d_0_wt);
    fatal_if_all_zero2("tlb_pte_n weights", memblock_plus_cfg::tlb_pte_n_1_wt, memblock_plus_cfg::tlb_pte_n_0_wt);
    fatal_if_all_zero2("tlb_pte_v weights", memblock_plus_cfg::tlb_pte_v_1_wt, memblock_plus_cfg::tlb_pte_v_0_wt);

    // Any new dist-controlled group, such as exception type or DCache response
    // weights, must add a matching fatal_if_all_zero* check here.
  endfunction

  static function void fatal_if_zero(string name, longint unsigned value);
    if (value == 0) begin
      `uvm_fatal("SEQ_CSR_CFG", $sformatf("%s must be non-zero", name))
    end
  endfunction

  static function void fatal_if_all_zero2(string name, int unsigned w0, int unsigned w1);
    if (w0 == 0 && w1 == 0) begin
      `uvm_fatal("SEQ_CSR_CFG", $sformatf("%s must not be all zero", name))
    end
  endfunction

  static function void fatal_if_all_zero3(string name, int unsigned w0, int unsigned w1, int unsigned w2);
    if (w0 == 0 && w1 == 0 && w2 == 0) begin
      `uvm_fatal("SEQ_CSR_CFG", $sformatf("%s must not be all zero", name))
    end
  endfunction

  static function void fatal_if_all_zero5(string name, int unsigned w0, int unsigned w1, int unsigned w2, int unsigned w3, int unsigned w4);
    if (w0 == 0 && w1 == 0 && w2 == 0 && w3 == 0 && w4 == 0) begin
      `uvm_fatal("SEQ_CSR_CFG", $sformatf("%s must not be all zero", name))
    end
  endfunction

  static function void clamp_int(string name, ref int unsigned value, int unsigned min_v, int unsigned max_v);
    if (value < min_v) begin
      `uvm_warning("SEQ_CSR_CFG", $sformatf("%s=%0d is below min %0d, clamp to %0d", name, value, min_v, min_v))
      value = min_v;
    end else if (value > max_v) begin
      `uvm_warning("SEQ_CSR_CFG", $sformatf("%s=%0d exceeds max %0d, clamp to %0d", name, value, max_v, max_v))
      value = max_v;
    end
  endfunction

  static function bit get_global_send_pri_en();
    check_initialized("get_global_send_pri_en");
    return memblock_plus_cfg::global_send_pri_en;
  endfunction

  static function int unsigned get_send_pri_default();
    check_initialized("get_send_pri_default");
    return memblock_plus_cfg::send_pri_default;
  endfunction

  static function int unsigned get_send_pri_std_default();
    check_initialized("get_send_pri_std_default");
    return memblock_plus_cfg::send_pri_std_default;
  endfunction

  static function int unsigned get_ld_to_st_addr_reuse_wt();
    check_initialized("get_ld_to_st_addr_reuse_wt");
    return memblock_plus_cfg::ld_to_st_addr_reuse_wt;
  endfunction

  static function int unsigned get_st_to_ld_addr_reuse_wt();
    check_initialized("get_st_to_ld_addr_reuse_wt");
    return memblock_plus_cfg::st_to_ld_addr_reuse_wt;
  endfunction

  static function bit get_manual_addr_reuse_en();
    check_initialized("get_manual_addr_reuse_en");
    return memblock_plus_cfg::manual_addr_reuse_en;
  endfunction
endclass
```

实际实现中可根据工程已有 package/class 风格调整封装方式，但参数来源必须统一经过 `plus.sv -> seq_csr_common.sv`。

### 4.4 表生成器文件分工

所有表生成器由两部分组成：

1. 对应表的 transaction 文件。
2. `memblock_base_sequence.sv` 中的生成器 task。

transaction 文件负责：

- 定义表字段。
- 定义字段合法性约束。
- 定义权重约束。
- 在 `post_randomize()` 中填充派生字段。
- 从 `seq_csr_common.sv` 读取 plusargs 解析后的权重或默认配置。

`memblock_base_sequence.sv` 负责：

- 在 `pre_body()` 开始阶段统一调用 `seq_csr_common::init()`，保证所有生成器 task 和 transaction 随机化前 plus 配置已经加载。
- 实现主表生成 task。
- 实现 TLB 表生成 task。
- 实现发射队列路由与维护 task。
- 实现发射控制 task。
- 实现第二类字段赋值 task：`assign_issue_dep_fields()`。
- 实现第三类字段赋值 task：`assign_backend_meta_fields()`。
- 实现异常监测、写回处理和 commit task。
- 调用 transaction randomize。
- 将生成结果写入 `common_data_transaction.sv` 单例。

`memblock_base_sequence.sv` 是主流程 task 的调度入口，不要求把所有算法细节都内联在同一个文件中。复杂且可复用的逻辑应拆到 helper class 或 `common_data_transaction.sv` API 中，base sequence task 负责串联流程、调用 helper、写入公共数据和更新状态。

推荐拆分原则：

- `lsq_ctrl_model`：负责 `lqIdx/sqIdx/numLsElem` 分配、LSQ 空闲资源、cancel/deq 回收和环形范围检查。
- `issue_queue_scheduler`：负责 eligible 过滤、实际发射优先级全局仲裁、流水线数量限制、随机/确定性选择和 `(uid, target, replay_seq)` 去重。
- `tlb_map_builder`：负责根据 `mmu_csr_runtime_state`、主表 `vaddr` 和 plus 权重生成 TLB/PTE/PMA 结果。`mmu_csr_runtime_state` 由 `csr_agent monitor` 实时维护，`csr_csr_common.sv` 只提供 reset baseline。
- `csr_runtime_tracker`：负责接收 `csr_agent monitor` 采样到的 CSR 更新，并更新 `common_data_transaction.sv` 中的 `mmu_csr_runtime_state`。该模块只维护必要 MMU CSR 状态，不引入额外 testcase epoch。
- `rob_order_util`：负责 `robIdx_flag/value` 构造、环形比较、flush boundary、oldest 选择和 `pendingPtr/pendingPtrNext` 计算辅助。
- `common_data_transaction.sv` API：负责表、队列、状态、active 映射和生命周期检查，禁止各 task 绕过 API 直接做易冲突的整表覆盖。

因此，“所有生成器 task 在 `memblock_base_sequence.sv` 中实现”应理解为所有外部可调度 task 的入口在该文件中；task 内部可以调用 helper 完成具体算法。这样既保留统一调度入口，也避免 `memblock_base_sequence.sv` 过度膨胀。

建议文件示例：

- `main_control_transaction.sv`：主控制表字段和约束。
- `tlb_transaction.sv`：TLB 表字段和约束。
- `status_transaction.sv`：状态表字段。
- `common_data_transaction.sv`：所有表、队列和状态的单例容器。
- `seq_csr_common.sv`：解析后的公共配置和 plus 权重，不保存 CSR/虚拟化实时状态。
- `csr_csr_common.sv`：CSR/虚拟化初始状态接口，只用于初始化 `mmu_csr_runtime_state` 的 baseline。
- `csr_runtime_tracker.sv`：CSR 运行时镜像维护 helper，接收 `csr_agent monitor` 的 CSR 更新并维护 `mmu_csr_runtime_state`。
- `plus.sv`：plusargs 模板和默认值。
- `memblock_base_sequence.sv`：所有生成器 task 和主流程 task 的调度入口。
- `lsq_ctrl_model.sv`：LSQ 分配、回收和环形资源检查 helper。
- `issue_queue_scheduler.sv`：发射队列调度和去重 helper。
- `tlb_map_builder.sv`：TLB/PTE/PMA 映射构造 helper。
- `rob_order_util.sv`：ROB 环形比较和边界计算 helper。

## 5. 第一套公共 Transaction 与主控制表

### 5.1 主表定位

第一套公共 Transaction 不是“后端到 MemBlock issue 接口”的逐字段镜像，而是整个 MemBlock UT 测试框架的全局主控制表。

主表负责驱动和关联以下流程：

- 主表随机生成。
- LSQ 入队与 `lqIdx/sqIdx` 分配。
- TLB 地址映射。
- load/STA/STD 队列路由。
- 发射控制。
- 写回监测。
- 异常处理。
- replay 或 redirect。
- commit 与资源回收。

因此，主表只应优先保存全局控制需要共享查询的字段。部分后端 issue 接口字段不需要在主表阶段提前强随机，而应在 transaction 真正发往 load/STA/STD 流水线前由专门函数派生并赋值。

### 5.2 主表字段

主表优先保存第一类全局控制字段：

- `uid` 或 `alloc_seq`，TB 内部唯一流水号，不发送给 DUT。
- `sqIdx_flag`
- `sqIdx_value`
- `lqIdx_flag`
- `lqIdx_value`
- `fuType`
- `fuOpType`
- `src_0`
- `imm`
- `vaddr`
- `robIdx_flag`
- `robIdx_value`
- `tlbAF`
- `tlbPF`
- `tlbGPF`
- `PBMT`
- `pmaAF`
- `delay`
- `send_pri`
- `send_pri_std`
- `corrupt`
- `denied`

`uid/alloc_seq` 由 `common_data_transaction.sv` 的 `alloc_uid()` 分配。`robIdx_flag/value` 仍需按 DUT ROB 环形语义生成和比较，但不作为主表、状态表的唯一权威 key；需要按 DUT 事件回查时，通过 `uid_by_active_rob[rob_to_map_key(rob_key)]` 找到当前活跃 `uid`，再访问主表和状态表。

### 5.2.1 ROB 环形指针和比较规则

`robIdx` 在文档和实现中统一表示为 `rob_key_t`，由 `flag` 和 `value` 两部分组成：

- `value` 是 ROB 表项下标，范围为 `0 .. RobSize-1`。
- `flag` 是 ROB 指针回绕位，`value` 从 `RobSize-1` 回到 0 时翻转。
- 主表、状态表、发射队列轻量项、redirect/replay/exception 事件中凡需要保存 ROB 语义的字段，都必须保存完整 `robIdx_flag/value`，禁止只保存裸 `robIdx_value`。

主表生成和 commit pointer 推进统一使用 `rob_advance(base, step)`。`step=1` 等价于 next，`step>1` 用于一次推进到 `pendingPtrNext` 或生成连续 ROB 条目，避免同时维护 `rob_next/rob_add` 两套接口。

```systemverilog
function automatic rob_key_t rob_advance(rob_key_t base, int unsigned step);
  rob_key_t cur = base;
  repeat (step) begin
    if (cur.value == RobSize - 1) begin
      cur.value = '0;
      cur.flag  = ~cur.flag;
    end else begin
      cur.value = cur.value + 1'b1;
    end
  end
  return cur;
endfunction
```

所有 ROB 顺序判断必须通过公共 helper 完成，建议放在 `rob_order_util.sv` 中。公共接口只保留必要核心函数，避免 helper 膨胀：

```systemverilog
function automatic rob_key_t rob_advance(rob_key_t base, int unsigned step);
function automatic bit       rob_is_after(rob_key_t a, rob_key_t b);
function automatic bit       rob_need_flush(rob_key_t uop_rob, redirect_payload_t redirect);
```

比较规则需要和 DUT 的 `robIdx.needFlush/isAfter/isNotAfter` 保持一致。实现时不得直接比较 `robIdx_value` 的 unsigned 大小；所有 redirect flush 边界、oldest exception/replay 仲裁、实际发射优先级同值截断、LSQ commit `pendingPtr/pendingPtrNext`、队列清理和状态恢复均必须调用上述 helper。

核心函数的必要性和职责如下：

| 函数 | 必要原因 | 支持的功能 |
|---|---|---|
| `rob_advance()` | 集中处理 `value` 回绕和 `flag` 翻转，避免主表生成和 commit pointer 推进各写一套规则 | 主表 `robIdx` 生成、`pendingPtr/pendingPtrNext` 推进、连续 commit candidate 计算 |
| `rob_is_after()` | 唯一核心顺序比较，判断 `a` 是否比 `b` younger，必须同时使用 `flag/value` | oldest exception/replay 仲裁、同优先级候选截断、redirect 边界判断的基础比较 |
| `rob_need_flush()` | 封装 redirect 的 `flushItself` 语义，避免每个 task 自己组合 `equal/is_after` | redirect/flush 队列清理、状态恢复、旧 issue epoch feedback 丢弃 |

以下接口不作为必须公共 API：

- `rob_equal(a, b)`：直接使用 `a == b` 即可，或在 `rob_need_flush()` 内部局部判断。
- `rob_is_not_after(a, b)`：等价于 `!rob_is_after(a, b)`，commit/SQ committed 逻辑可局部写 wrapper 提升可读性，但不强制放入公共 API。
- `select_oldest_uid(candidates)`：属于 exception 或 scheduler 的选择算法，应在调用方基于 `rob_is_after()` 实现；`uid` 只可作为同一 `rob_key` 不应出现时的 debug tie-break。
- `rob_distance(from, to)`：初版不是功能必需，仅在 debug、覆盖率统计或后续性能优化需要时再增加。

状态表中也必须保存对应 `rob_key` 快照。monitor 只有 DUT `robIdx_flag/value` 时，直接组成 `rob_key` 后，通过 `lookup_active_uid_by_rob()` 找到当前 active `uid`；如果 active 映射不存在，不能退化成按 value 扫描主表。

`send_pri` 和 `send_pri_std` 是主控制表字段，取值范围均为 0 到 100，数值越大表示越优先发射。该字段是否生效由 `seq_csr_common.sv` 中的 `global_send_pri_en` 控制：

- `send_pri`：load、STA 以及非 store 单入口发射的默认优先级；store 拆分成 STA/STD 两个队列时，STA 队列项使用该字段。
- `send_pri_std`：store 拆分后 STD 队列项的独立优先级，只在该主表条目会进入 STD 队列时生效；非 store 条目可随机生成但不参与调度，也可在 `post_randomize()` 中默认等于 `send_pri`。
- `global_send_pri_en=1`：发射控制 task 选择候选 transaction 时优先考虑队列项中缓存的实际发射优先级；STA 队列项来自 `send_pri`，STD 队列项来自 `send_pri_std`，load 队列项来自 `send_pri`。
- `global_send_pri_en=0`：忽略 `send_pri/send_pri_std`，按原有乱序或顺序策略选择。

主任务状态不直接混入主表字段，而是单独存放在 `common_data_transaction.sv` 的状态表中。主任务负责状态表生命周期管理，包括初始化、最终清理和一致性检查；具体状态字段由直接造成状态变化的子 task 在真实事件发生点更新。

第二类和第三类字段不作为主表阶段强随机的核心字段：

- 第二类字段：`loadWaitBit`、`waitForRobIdx_flag`、`waitForRobIdx_value`、`storeSetHit`、`loadWaitStrict`、`isFirstIssue`。
- 第三类字段：`pc`、`isRVC`、`ftqIdx_flag`、`ftqIdx_value`、`ftqOffset`、`pdest`、`rfWen`、`fpWen`。

这些字段不新增独立补充字段文件。发射 task 在真正发送前，先根据主表信息生成对应发往 agent 的 transaction，再调用两个赋值 task 补齐第二类和第三类字段，最后驱动 agent 发送。

### 5.3 主表生成器 task

主表生成器通过 `seq_csr_common.sv` 提供的参数控制生成规模、权重和生成模式，例如随机生成 100 笔 Transaction，或直接使用外部手动配置的主表关联数组。

主表生成器需要满足以下要求：

- 支持随机生成模式和手动配置模式。
- 随机生成模式接收生成数量参数，基于公共 Transaction 约束随机生成对应数量的条目。
- 手动配置模式不随机生成主表，而是直接读取用户预先配置好的主表关联数组。
- `robIdx` 按 DUT ROB 环形语义生成，`value` 在 ROB 最大容量内递增并回绕，`flag` 随回绕翻转或按 DUT 定义更新；主表条目整体仍按 `uid` 连续递增。
- `lqIdx` 和 `sqIdx` 在主表阶段保留字段。需要 LSQ entry 的 load/store/CBO 在入队 fire 后由 Task1 调用 `lsq_ctrl` 最终确认并回填；`ATOMIC` 不分配真实 LQ/SQ，只在发射时填接口占位值或边界快照。
- `numLsElem/lsq_flow` 初版固定为标量 `1`；vector LS 在主表生成和手动导入阶段拒绝，不进入 `lsq_ctrl`。
- `vaddr` 由 `src_0 + SignExt(imm[11:0])` 计算得出。
- `fuType` 和 `fuOpType` 必须按合法指令模板成对生成，不能独立随机。
- `fuOpType` 合法取值和权重通过 `plus.sv -> seq_csr_common.sv` 控制；生成 `fuOpType/op_class` 后，再按最小合法模板修正 `rfWen/fpWen/pdest`，禁止 `rfWen/fpWen` 与 `fuOpType/op_class` 完全独立随机。
- 初版只支持标量 LS，不生成 vector load/store。随机生成器的合法 `op_class/fuOpType` 模板中不包含 vector LS；手动主表导入时若发现 vector LS 条目，默认 `uvm_fatal`，等待后续向量专项方案补齐字段、flow 和发射路径后再开放。
- 主表随机生成完成后，需要根据 `fuOpType/op_class` 调用地址复用注入 task，按 plus 权重把部分 load/store 的 `src_0 + imm` 调整为相同地址，提高 load-store 违例、转发、replay 或 MDP 场景出现概率。
- 地址复用注入 task 的两个频率参数必须通过 `plus.sv -> seq_csr_common.sv` 控制，默认 0 表示不注入。手动主表模式默认不执行地址复用注入，除非显式开启 `+MEMBLOCK_MANUAL_ADDR_REUSE_EN`。
- `send_pri/send_pri_std` 取值范围为 0 到 100，权重由 `seq_csr_common.sv` 中的 plus 配置控制。store 拆成 STA/STD 队列时，STA 使用 `send_pri`，STD 使用 `send_pri_std`。
- `tlbAF`、`tlbPF`、`tlbGPF`、`PBMT`、`pmaAF`、`delay`、`corrupt`、`denied` 等控制字段需要支持权重约束；这些异常或返回路径控制字段允许组合给出，不在主表生成阶段强制互斥。
- `pmaAF` 字段必须在主表中保留并贯通到后续表/状态记录；初版可以先做基础功能或占位实现，PMA 精细行为和覆盖率后续再完善。
- `post_randomize()` 中完成派生字段填充，例如 `vaddr`，以及必要的合法性修正。
- 生成后的主表条目按 `uid` 写入 `common_data_transaction.sv` 中的 `main_table_by_uid[]`。

#### 随机生成模式

随机生成模式是默认模式，由 `MEMBLOCK_USE_MANUAL_MAIN_TABLE` 未开启时使用。

流程：

1. 从 `seq_csr_common.sv` 读取 `main_trans_num` 和各字段权重参数，并确保已经调用 `common_data_transaction::get().reset_all_tables(main_trans_num)`。
2. 创建主表 transaction。
3. 调用 `randomize()`。
4. 在 `post_randomize()` 中生成 `vaddr`、修正派生字段、检查字段合法性。
5. 调用 `common_data_transaction::get().alloc_uid()` 分配连续 `uid`，写入 transaction 后按 `uid` 写入 `common_data_transaction.sv` 的 `main_table_by_uid[]`。当前活跃 `robIdx -> uid` 映射只允许在 LSQ 入队 fire 或 non-LSQ admission 成功后通过 `activate_uid(uid, op_behavior)` 建立。
6. 调用 `inject_ls_addr_reuse_by_fuoptype()`，按 plus 权重对已生成主表做 load/store 地址复用后处理。
7. 后处理完成后重新计算并校验被修改条目的 `vaddr = src_0 + SignExt(imm[11:0])`。
8. 主表生成结束后检查 `next_uid == main_trans_num`。

#### 地址复用注入 task

为了提高 load-store 地址相关、违例、转发、replay 和 MDP 场景出现概率，主表生成 task 需要在随机主表初步生成后增加一个小型后处理 task，建议命名为 `inject_ls_addr_reuse_by_fuoptype()`。

默认触发规则：

- 随机生成模式：生成完整主表后，允许按 `MEMBLOCK_LD_TO_ST_ADDR_REUSE_WT` 和 `MEMBLOCK_ST_TO_LD_ADDR_REUSE_WT` 执行地址复用注入。
- 手动配置模式：默认不执行地址复用注入，避免破坏 directed case 或复现场景中用户显式配置的地址关系。
- 手动配置模式如需地址复用后处理，必须显式开启 `+MEMBLOCK_MANUAL_ADDR_REUSE_EN`。开启后仍使用同一套地址复用权重参数，并且必须重新计算 `vaddr`、执行派生字段补齐和合法性校验。

该 task 必须通过 `fuOpType/op_class` 判断 load/store 类型，不能只按 `fuType` 粗略判断。推荐规则如下：

- `ld_to_st_addr_reuse_wt`：范围 0 到 100。对每条 store/store-like transaction，按该权重随机决定是否从已经生成的 load 候选中选取一条，把该 load 的 `src_0/imm` 地址组合复用给当前 store。
- `st_to_ld_addr_reuse_wt`：范围 0 到 100。对每条 load/load-like transaction，按该权重随机决定是否从更早的 store 候选中选取一条，把该 store 的 `src_0/imm` 地址组合复用给当前 load，从而提高“前序 store 与后续 load 同地址”的违例概率。
- 两个参数均从 `seq_csr_common.sv` 读取，最终由 plusargs 控制；默认值为 0，即保持原随机地址，不做地址复用注入。
- 复用时优先直接复制候选 transaction 的 `src_0` 和 `imm`，保证 `src_0 + SignExt(imm[11:0])` 完全相同。
- 如果 load/store 的访问粒度、对齐、异常注入或地址范围约束不兼容，需要重新选择候选；找不到合法候选时跳过该条，不强行生成非法 transaction。
- 每次修改 `src_0/imm` 后必须重新计算 `vaddr`，并调用统一合法性检查，保证 `vaddr == src_0 + SignExt(imm[11:0])`。
- 该 task 是主表级后处理，依赖完整主表候选集合，因此不放在单条 transaction 的 `post_randomize()` 中实现。

伪代码示例：

```systemverilog
task inject_ls_addr_reuse_by_fuoptype(
  ref main_control_transaction main_table_by_uid[]
);
  int unsigned ld_to_st_wt = seq_csr_common::get_ld_to_st_addr_reuse_wt();
  int unsigned st_to_ld_wt = seq_csr_common::get_st_to_ld_addr_reuse_wt();

  foreach (main_table_by_uid[uid]) begin
    main_control_transaction tr = main_table_by_uid[uid];

    if (is_store_fuoptype(tr.fuOpType) && rand_percent_hit(ld_to_st_wt)) begin
      main_control_transaction ld_ref;
      if (select_legal_load_addr_ref(main_table_by_uid, uid, ld_ref)) begin
        tr.src_0 = ld_ref.src_0;
        tr.imm   = ld_ref.imm;
        tr.update_vaddr();
        tr.validate_main_transaction();
      end
    end

    if (is_load_fuoptype(tr.fuOpType) && rand_percent_hit(st_to_ld_wt)) begin
      main_control_transaction st_ref;
      if (select_legal_prior_store_addr_ref(main_table_by_uid, uid, st_ref)) begin
        tr.src_0 = st_ref.src_0;
        tr.imm   = st_ref.imm;
        tr.update_vaddr();
        tr.validate_main_transaction();
      end
    end
  end
endtask
```

其中 `select_legal_prior_store_addr_ref()` 必须只从更早 `uid` 或 ROB 环形顺序更老的 store 候选中选择；`select_legal_load_addr_ref()` 可以按场景选择任意已生成 load，若需要更强时序相关性，也可以限制为更早 `uid` 或 ROB 环形顺序更老的 load 候选。

#### 手动配置关联数组模式

手动配置模式由 plusarg `+MEMBLOCK_USE_MANUAL_MAIN_TABLE` 开启。该模式用于 directed case 或复现指定场景。

手动模式输入：

```systemverilog
main_control_transaction manual_main_table_by_rob[int unsigned];
```

手动模式要求：

- 用户或 testcase 在主表生成 task 执行前完成 `manual_main_table_by_rob` 配置。
- 导入前必须先根据手动配置条目数量确定 `main_trans_num`，并调用 `common_data_transaction::get().reset_all_tables(main_trans_num)`。
- 主表生成 task 不调用 `randomize()` 生成新条目。
- 主表生成 task 直接遍历手动关联数组，校验后通过 `alloc_uid()` 分配 TB 内部 `uid`，再导入到 `common_data_transaction.sv` 的 `main_table_by_uid[]`。
- 手动条目仍必须执行统一合法性校验，例如 `validate_main_transaction()`。
- 如果手动条目没有填写派生字段，例如 `vaddr`，允许通过 `post_manual_config()` 或等价函数补齐。
- 如果手动条目已经填写派生字段，则需要校验其与源字段一致，例如 `vaddr == src_0 + SignExt(imm[11:0])`。
- 手动配置中的 `robIdx` 需要按 ROB 最大容量和 flag/value 规则合法生成；如果配置数量超过 ROB 容量，允许后续条目复用回绕后的 `robIdx`。导入后的权威 key 仍为 `uid`，手动表 key 只作为配置入口；LSQ 入队 fire 或 non-LSQ admission 成功时由 `activate_uid(uid, op_behavior)` 检查同一 `rob_key` 当前没有其他 active 条目。
- `lqIdx/sqIdx` 仍可保留未分配状态，由后续入队 task 在 fire 后调用 `lsq_ctrl` 最终确认并回填。
- 手动模式默认不执行 `inject_ls_addr_reuse_by_fuoptype()`，避免修改用户 directed 配置的地址关系。
- 只有显式开启 `+MEMBLOCK_MANUAL_ADDR_REUSE_EN` 时，手动导入完成后才允许执行地址复用后处理；执行后必须重新计算 `vaddr = src_0 + SignExt(imm[11:0])`，并再次执行统一合法性校验。
- 手动模式导入完成后，除可选地址复用后处理外，后续状态初始化、TLB 映射、队列路由、发射控制、异常和提交流程与随机模式完全复用。

手动模式 task 形式示例：

```systemverilog
task build_main_table(
  input bit use_manual,
  ref main_control_transaction manual_table_by_rob[int unsigned]
);
  if (use_manual) begin
    foreach (manual_table_by_rob[rob]) begin
      main_control_transaction tr = manual_table_by_rob[rob];
      tr.post_manual_config();
      if (!tr.validate_main_transaction()) begin
        `uvm_fatal("MAIN_TABLE", $sformatf("illegal manual main transaction robIdx=%0d", rob))
      end
      tr.uid = common_data_transaction::get().alloc_uid();
      common_data_transaction::get().main_table_by_uid[tr.uid] = tr;
      common_data_transaction::get().init_status_for_uid(tr.uid);
    end
    if (seq_csr_common::get_manual_addr_reuse_en()) begin
      inject_ls_addr_reuse_by_fuoptype(common_data_transaction::get().main_table_by_uid);
    end
  end else begin
    build_random_main_table();
  end
endtask
```

权重约束可以参考以下形式：

```systemverilog
constraint c_trans_delay {
  delay dist {
    0       :/ memblock_plus_cfg::delay_0_wt,
    [1:20]  :/ memblock_plus_cfg::delay_1_20_wt,
    [21:50] :/ memblock_plus_cfg::delay_21_50_wt
  };
}

constraint c_send_pri {
  send_pri inside {[0:100]};
  send_pri_std inside {[0:100]};
  send_pri dist {
    [0:30]   :/ memblock_plus_cfg::send_pri_low_wt,
    [31:70]  :/ memblock_plus_cfg::send_pri_mid_wt,
    [71:100] :/ memblock_plus_cfg::send_pri_high_wt
  };
  send_pri_std dist {
    [0:30]   :/ memblock_plus_cfg::send_pri_std_low_wt,
    [31:70]  :/ memblock_plus_cfg::send_pri_std_mid_wt,
    [71:100] :/ memblock_plus_cfg::send_pri_std_high_wt
  };
}

constraint c_tlb_pte_bits {
  pte_r dist {1 :/ memblock_plus_cfg::tlb_pte_r_1_wt, 0 :/ memblock_plus_cfg::tlb_pte_r_0_wt};
  pte_w dist {1 :/ memblock_plus_cfg::tlb_pte_w_1_wt, 0 :/ memblock_plus_cfg::tlb_pte_w_0_wt};
  pte_x dist {1 :/ memblock_plus_cfg::tlb_pte_x_1_wt, 0 :/ memblock_plus_cfg::tlb_pte_x_0_wt};
  pte_u dist {1 :/ memblock_plus_cfg::tlb_pte_u_1_wt, 0 :/ memblock_plus_cfg::tlb_pte_u_0_wt};
  pte_g dist {1 :/ memblock_plus_cfg::tlb_pte_g_1_wt, 0 :/ memblock_plus_cfg::tlb_pte_g_0_wt};
  pte_a dist {1 :/ memblock_plus_cfg::tlb_pte_a_1_wt, 0 :/ memblock_plus_cfg::tlb_pte_a_0_wt};
  pte_d dist {1 :/ memblock_plus_cfg::tlb_pte_d_1_wt, 0 :/ memblock_plus_cfg::tlb_pte_d_0_wt};
  pte_n dist {1 :/ memblock_plus_cfg::tlb_pte_n_1_wt, 0 :/ memblock_plus_cfg::tlb_pte_n_0_wt};
  pte_v dist {1 :/ memblock_plus_cfg::tlb_pte_v_1_wt, 0 :/ memblock_plus_cfg::tlb_pte_v_0_wt};
}
```

### 5.4 字段分类

#### 第一类：主表全局控制字段

第一类字段必须进入主表，约束性强。它们直接驱动测试框架控制流，影响入队、TLB 映射、队列路由、异常和提交处理。

| 字段 | 作用 | 约束 |
|---|---|---|
| `fuType`、`fuOpType` | 区分 load、store、AMO、prefetch 等操作，是队列路由和发射路径选择依据 | 必须按合法指令模板成对生成 |
| `src_0`、`imm`、`vaddr` | 生成访存虚拟地址 | `vaddr = src_0 + SignExt(imm[11:0])` |
| `robIdx_flag/value` | DUT ROB 语义、异常、写回、flush 边界和提交关联；主表/状态表权威索引仍为 `uid` | 主表生成阶段按 ROB 环形规则递增 |
| `lqIdx_flag/value` | LS transaction 携带的 LQ 位置或 LQ 边界信息；load 消费 LQ 分配，store 仍需要携带该字段用于接口一致性和年龄窗口判断 | 主表保留字段，需要 LSQ 分配的条目在入队 fire 时由 `lsq_ctrl` 最终确认并回填；`ATOMIC` 不分配真实 LQ |
| `sqIdx_flag/value` | LS transaction 携带的 SQ 位置或 SQ 边界信息；store 消费 SQ 分配，load 仍需要携带该字段用于 load-store 依赖窗口判断 | 主表保留字段，需要 LSQ 分配的条目在入队 fire 时由 `lsq_ctrl` 最终确认并回填；`ATOMIC` 不分配真实 SQ |
| `numLsElem/lsq_flow` | 描述一条 transaction 本次需要占用的 LQ/SQ entry 数量 | 初版只支持标量；load/store/CBO 合法值固定为 1，`ATOMIC` 合法值为 0；向量派生规则不在本方案开放 |
| `tlbAF`、`tlbPF`、`tlbGPF`、`PBMT`、`pmaAF` | 控制地址翻译、权限、内存属性和异常场景 | CSR/虚拟化实时状态来自 `mmu_csr_runtime_state`；`csr_csr_common.sv` 只提供初始 baseline；异常和权限位权重来自 plusargs；允许与 DCache/TL 返回路径控制字段组合 |
| `pmaAF` | PMA access fault 控制字段 | 字段保留并贯通；初版功能可先基础实现或占位，后续再完善 PMA 精细行为 |
| `delay` | 控制发射节奏 | 主表保留，发射 task 通过 gap transaction 使用 |
| `send_pri` | 控制 load、STA 和非 store 单入口候选的发射优先级 | 范围 0 到 100，由 `global_send_pri_en` 控制是否生效；store 的 STA 队列项使用该字段 |
| `send_pri_std` | 控制 store 拆分后 STD 队列项的发射优先级 | 范围 0 到 100，由 `global_send_pri_en` 控制是否生效；非 STD 队列项不使用该字段 |
| `corrupt`、`denied` | 控制 DCache/TL 回复和失败路径 | 可与 `tlbPF/tlbAF/tlbGPF/pmaAF` 等翻译或访问异常字段同时给出；测试框架不在主表阶段建立互斥或优先级模型 |

异常控制字段处理原则：

- `tlbPF/tlbAF/tlbGPF/pmaAF` 属于翻译、权限或访问异常控制字段。
- `corrupt/denied` 属于 DCache/TL 返回路径控制字段。
- 测试用例允许这些字段同时给出，用于制造组合异常压力，不要求默认互斥。
- 测试框架不预判组合字段的最终异常优先级，也不把某一类字段自动清零；最终写回、异常、redirect 或 replay 结果以 DUT 观测接口为准。
- monitor 和状态表需要同时记录主表注入字段以及 DUT 实际返回的 `exceptionVec/replay/redirect/pass` 结果，便于后续 debug 和覆盖率分析。

`fuType/fuOpType` 示例约束：

- `fuType=ldu`：匹配 integer load、FP load、prefetch、HLV/HLVX 等 load 类 `fuOpType`。
- `fuType=stu`：匹配 store、FP store、CBO、HSV 等 store 类 `fuOpType`。
- `fuType=mou`：匹配 AMO、LR、SC 类 `fuOpType`。

#### 第二类：流水线发射与相关性字段

第二类字段不在主表中预先强随机，也不新增独立补充字段文件。它们在发射前由专门赋值 task 写入对应发往 agent 的 transaction。它们会影响 load 等待、MDP 相关性、首次发射和 replay 行为。

| 字段 | 作用 | 约束 |
|---|---|---|
| `loadWaitBit` | 控制 load 是否等待预测相关的前序 store | 普通 load 可为 0，MDP 场景按权重打开 |
| `waitForRobIdx_flag/value` | 指定需要等待的前序 store ROB 项 | 应指向合法前序 store |
| `storeSetHit` | Store Set/MDP 命中标识 | 需要与 `ssid`、`waitForRobIdx` 等信息一致 |
| `loadWaitStrict` | 更严格的 load 等待策略 | 打开后需要等待所有相关前序 store 地址计算完成 |
| `isFirstIssue` | 区分首次发射和 replay/redirect 后重发 | 首次发射为有效，重发时由异常或 replay 处理逻辑重新判定 |

推荐新增赋值 task：

```systemverilog
task assign_issue_dep_fields(
  ref memblock_agent_transaction agent_tr,
  input main_control_transaction main_tr
);
  // 发射 task 已经先把 main_tr 中的主表字段赋给 agent_tr。
  // 本 task 再根据 robIdx 状态、前序 store 状态、replay 状态、
  // MDP 专项场景和 seq_csr_common.sv 中的权重填充第二类字段。
endtask
```

#### 第三类：后端写回与前端调试元信息字段

第三类字段不主导主表控制流，主要随流水线携带，用于写回、ROB/前端 redirect、debug 或提交信息。它们不新增独立补充字段文件，而是在发射前由专门赋值 task 写入对应发往 agent 的 transaction。它们不能完全独立随机，需要根据第一类字段和指令模板派生。

| 字段 | 作用 | 约束 |
|---|---|---|
| `pc` | 指令 PC | 普通场景可统一生成，异常/redirect 场景需要可追踪 |
| `isRVC` | 是否压缩指令 | 与 PC 递增和指令长度相关 |
| `ftqIdx_flag/value`、`ftqOffset` | 前端 FTQ 元信息 | 用于异常、redirect、debug、前端训练或回放定位 |
| `pdest` | 物理目的寄存器 | 只有 `rfWen` 或 `fpWen` 为 1 时有意义 |
| `rfWen` | 整数寄存器写使能 | integer load、HLV、AMO/LR/SC 为 1 |
| `fpWen` | 浮点寄存器写使能 | FP load 为 1，并影响 LoadUnit 数据格式 |

`rfWen/fpWen/pdest` 约束：

- `rfWen/fpWen` 不是 RAW 或 memory violation 的触发条件，不参与 load-store 地址相关判断；RAW/memory violation 由 `op_class`、ROB 顺序、地址、mask 和状态共同决定。
- `rfWen/fpWen` 不独立随机，由已生成的 `fuOpType/op_class` 按最小合法模板修正。
- integer load、HLV、AMO、LR、SC：`rfWen=1`，`fpWen=0`，`pdest` 为整数物理寄存器。
- FP load：`rfWen=0`，`fpWen=1`，`pdest` 为浮点物理寄存器。
- store、FP store、prefetch、CBO：`rfWen=0`，`fpWen=0`，`pdest` 无实际写回意义。
- `rfWen` 和 `fpWen` 必须互斥。
- 当前源码中 `FLH/FLW` 与整数 `LH/LW` 共用 `fuOpType=lh/lw` 编码，通过 `fpWen` 区分返回数据格式和写回 FP RF：
  - `LH/LW`：`fuOpType=lh/lw`，`rfWen=1`，`fpWen=0`。
  - `FLH/FLW`：`fuOpType=lh/lw`，`rfWen=0`，`fpWen=1`。
- 不新增 `data_type` 主表字段。`data_type` 仅作为文档中的模板分类概念，必要时可用于 debug，不作为 DUT 输入或主表强制字段。

推荐新增赋值 task：

```systemverilog
task assign_backend_meta_fields(
  ref memblock_agent_transaction agent_tr,
  input main_control_transaction main_tr
);
  // 发射 task 已经先把 main_tr 中的主表字段赋给 agent_tr。
  // 本 task 再根据 op_class 和指令模板填充 pc/isRVC/ftq/pdest/rfWen/fpWen。
endtask
```

### 5.5 推荐生成流程

1. 公共数据 reset 阶段：主表随机生成或手动导入之前，必须先解析得到本轮 `main_trans_num`，并调用 `common_data_transaction::get().reset_all_tables(main_trans_num)`。
2. 主表准备阶段：根据 `MEMBLOCK_USE_MANUAL_MAIN_TABLE` 选择随机生成主表，或导入手动配置的主表关联数组；两种模式最终都按连续 `uid` 写入 `common_data_transaction.sv` 的 `main_table_by_uid[]`。
3. 主表地址复用后处理阶段：随机生成模式下，根据 `MEMBLOCK_LD_TO_ST_ADDR_REUSE_WT` 和 `MEMBLOCK_ST_TO_LD_ADDR_REUSE_WT` 调用 `inject_ls_addr_reuse_by_fuoptype()`，对部分 load/store 的 `src_0/imm/vaddr` 做合法同地址注入；手动配置模式默认跳过该后处理，只有 `+MEMBLOCK_MANUAL_ADDR_REUSE_EN` 显式开启时才执行。
4. 状态初始化阶段：主任务在 `status_by_uid[]` 中为每条 `uid` 创建状态条目，状态条目同时保存对应 `robIdx_flag/value` 供 debug 和 DUT 事件回查，且初始 `active=0`。
5. LSQ 入队资源管理阶段：入队 task 先调用 `derive_op_behavior()`。需要 LSQ 分配的条目结合 DUT `canAccept/lqCanAccept/sqCanAccept`、LSQ 空闲资源和本拍配置上限调用 `lsq_ctrl` 计算候选项的 `lqIdx/sqIdx/numLsElem`，只有入队 fire 后才最终回填主表、调用 `activate_uid(uid, op_behavior)` 建立活跃 `robIdx -> uid` 映射，并推进本地指针；`ATOMIC` 等 `needAlloc=0` 条目走 non-LSQ admission，不推进 LSQ 指针。
6. 入队和 TLB 阶段：基于已成功 LSQ 入队或完成 non-LSQ admission 的主表第一类字段生成 TLB 表，并写入 `common_data_transaction.sv`。
7. 队列路由阶段：将已入队或完成 non-LSQ admission、且完成 TLB 映射的 `uid` 路由到 load、STA、STD 三个发射队列。
8. 发射前字段赋值阶段：发射 task 先用主表信息生成发往 agent 的 transaction，再调用 `assign_issue_dep_fields()` 和 `assign_backend_meta_fields()`，为该 agent transaction 补齐第二类和第三类字段。

## 6. 发射队列相关 task

### 6.1 主任务

主任务负责调用各子 task 控制完整流程，并负责状态表生命周期管理。

状态表不再作为主任务内部局部表维护，而是作为独立状态表存放在 `common_data_transaction.sv` 中。主任务是该状态表的生命周期 owner，负责创建初始状态、最终清理状态和检查状态一致性；各子 task 是对应状态字段的事件 owner，负责在真实状态转变点更新字段。

状态表以 `uid` 作为权威主键，记录主表条目的完整生命周期状态。状态对象内部必须保存对应的 `robIdx_flag/value`，用于 debug、ROB 顺序比较和 DUT monitor 事件回查；monitor 只有 `robIdx` 信息时，先通过 `uid_by_active_rob` 找到当前活跃 `uid`，再更新状态表。

- `active`：该 `uid` 当前已经进入 DUT 可观察窗口。需要 LSQ entry 的 load/store/CBO 在 LSQ 入队 fire 后由 `activate_uid(uid, op_behavior)` 置 1；`ATOMIC` 这类 `needAlloc=0` 的 non-LSQ 条目在 admission 完成、准备进入发射队列前置 1。commit/deq 完成或 flush/cancel 确认后由 `retire_active_uid(uid)` 置 0。
- `enq`：已完成 LSQ 入队或 non-LSQ admission。若需要区分两者，可增加 `lsq_enq` 和 `non_lsq_admitted` 两个派生状态位。
- `dispatch`：已发射到流水线。
- `writeback`：已从流水线写回。
- `fault`：写回后检测到异常。
- `exception_pending`：异常事件已捕获，等待 trap/redirect/recovery 处理。
- `flushed`：该条目已被 redirect/flush 清理。
- `pass`：写回后正常通过。
- `rob_commit`：已按 ROB commit 语义驱动 LSQ commit 输入。
- `lsq_deq`：已观察到 `lqDeq/sqDeq`，对应 LSQ entry 已实际释放。
- `load_issue_epoch/sta_issue_epoch/std_issue_epoch`：该条目各 target 最近一次发射所属的 epoch，用于区分 replay/redirect 前后的旧发射。
- `replay_seq`：该条目 replay/reissue 计数，用于发射队列去重和调试。
- `issue_killed`：该条目已发射但被同拍或后续 redirect/flush 杀掉，后续写回应忽略或转入 replay 恢复。
- `load_dispatch`、`sta_dispatch`、`std_dispatch`：分别记录 load、STA、STD 三类入口是否已经成功发射，store 需要分别判断地址侧和数据侧，不能只用一个总 `dispatch`。
- `load_pass`、`sta_pass`、`std_pass`：分别记录各入口是否已经完成并通过；store 的 STA replay 只清 `sta_pass`，不清 `std_pass`。
- `replay_pending`：已捕获 replay 事件，需要由队列路由 task 按 target mask 重新放入目标发射队列。
- `replay_target_mask`：本次需要重新发射的队列集合，例如 load、STA、STD。store 地址侧 replay 只置 STA，不影响 STD。
- `redirect_pending`：等待 redirect 或 flush 处理。

所有会引起 transaction 状态变化的 task 都必须在真实事件发生点按 `uid` 更新状态表中的对应字段，例如入队 task 在 LSQ 入队 fire 或 non-LSQ admission 成功后更新 `enq`，发射 task 在成功发送到流水线后更新 `dispatch`，写回监测 task 在捕获写回后通过条件更新 API 更新 `writeback/pass/fault`，`exception_redirect_replay_task` 在确认异常、redirect 或 replay 处理动作后更新 `exception_pending/flushed/replay_pending/redirect_pending`，LSQ commit 驱动 task 在驱动 ROB commit 输入后更新 `rob_commit`，在观察到 `lqDeq/sqDeq` 后更新 `lsq_deq`。如果事件只携带 `robIdx`，必须先解析为当前活跃 `uid`；解析失败时默认报错，只有能明确证明该事件属于已 retire 或已 flush 的迟到反馈，且不会更新任何当前状态时，才允许记录后忽略。

主任务不集中代替子 task 更新所有状态字段，否则容易因为事件汇报滞后或局部信息缺失造成状态不准确。主任务主要负责：

- 初始化每条 `uid` 的状态条目。
- 调度和启动各子 task。
- 周期性检查状态合法性，例如禁止重复发射、禁止未写回提交、禁止已 commit 条目重新入队。
- 在 commit/deq、flush/redirect 完成后调用 `retire_active_uid(uid)` 清理活跃映射，并在测试结束时调用 `end_test_check()` 做最终一致性检查。
- 对状态异常进行报错或停止测试。

状态表并发更新策略：

- 推荐仍使用统一状态表管理，便于通过 `uid` 获取完整生命周期状态，并通过活跃 `robIdx -> uid` 映射处理 DUT 事件。
- 不允许多个 task 直接采用“读出整个状态条目 -> 修改局部字段 -> 写回整个状态条目”的方式更新，否则不同 task 在同一仿真时间片更新不同字段时可能互相覆盖。
- `common_data_transaction.sv` 需要提供统一状态更新 API，例如 `set_status_field(uid, field, value)`、`update_status(uid, mask, value)` 或 `set_status_by_active_rob(rob_key, field, value)`，所有 task 只能通过该 API 修改状态。
- 状态更新 API 必须同时提供 target 级条件更新形式，用于来自流水线反馈、写回、异常和 replay feedback 的完成类状态更新。推荐接口为 `conditional_update_status(uid, target, issue_epoch, replay_seq, update_mask, update_value)` 或按用途拆分为 `set_writeback_if_current()`、`set_pass_if_current()`、`set_fault_if_current()`。
- 条件更新必须检查 `status_by_uid[uid].active==1`、`status_by_uid[uid].get_target_issue_epoch(target) == issue_epoch`、`status_by_uid[uid].replay_seq == replay_seq`，并且 `issue_killed==0`。涉及 load/STA/STD 分入口时，`target` 就是入口匹配条件。
- 条件不匹配的反馈视为旧 epoch、旧 replay_seq 或已被 redirect/flush 杀掉的迟到事件，默认记录到 debug 日志或可选统计中并忽略，不允许更新当前 `writeback/pass/fault/load_pass/sta_pass/std_pass/exception_pending/replay_pending` 等状态。
- LSQ 入队 fire、non-LSQ admission、发射成功、commit/deq、flush/cancel 这类由本地 task 在当前控制点直接产生的状态变化，可以使用普通字段级 API；但任何从 DUT pipeline/feedback/monitor 回来的完成或异常结果，都必须使用条件更新 API。
- API 内部建议使用原地字段更新，优先使用 class handle 形式的状态对象；必要时为状态表或单个 `uid` 增加 semaphore/mailbox 保护，保证同一条目的多字段更新具备原子性。
- 如果后续实现发现单一状态对象存在工具兼容性或并发覆盖风险，可以退化为“每个状态字段一张独立 bit 关联数组”的形式，例如 `enq_by_uid[uid]`、`dispatch_by_uid[uid]`、`writeback_by_uid[uid]`。该方案能避免字段互相覆盖，但一致性快照和调试成本更高。
- 当前推荐方案是统一状态表加统一更新 API；只要禁止整条目覆盖式写回，多 task 更新不同字段不会影响正确性。

主任务分为两条并行流程，建议通过 `fork...join` 或等价机制并发运行：

1. 发射流程：
   - 从 `common_data_transaction.sv` 获取主控制表。
   - 调用入队 task 完成 LSQ 入队。
   - 调用 TLB 地址映射 task。
   - 调用队列路由与维护 task，将条目放入 load/STA/STD 发射队列。
   - 调用发射控制 task 将 transaction 送入 load/STA/STD 流水线。
   - 每个子 task 在真实事件发生点更新 `common_data_transaction.sv` 中的状态表。

2. 写回与异常处理流程：
   - 监测 MemBlock 到后端的写回端口。
   - 监测 `lqIdx`、`sqIdx`、`robIdx` 等返回信息。
   - 根据异常类型执行 redirect 或 replay。
   - 写回监测 task 只负责写回、异常、replay/redirect 事实采样，不直接合并实现 LSQ commit 驱动。
   - 调用专门的 LSQ commit 驱动 task，按源码 ROB commit 语义驱动 `lcommit/scommit/commit/pendingPtr/pendingPtrNext`，并监测 `lqDeq/sqDeq` 完成资源释放。
   - 每个子 task 在真实事件发生点更新 `common_data_transaction.sv` 中的状态表。

入队与发射之间的具体时序要求需要继续参考源码和 Verilog 接口确认，不能假设入队后一拍必然可以发射。

### 6.2 Task0：`lsq_ctrl`

`lsq_ctrl` 维护 LSQ 中 `lqIdx` 和 `sqIdx` 的分配与回收。

要求：

- 按 `uid` 顺序处理主表条目，ROB 语义比较仍使用主表中的 `robIdx_flag/value`。
- 根据 `fuType/op_class` 判断本条 transaction 是否为 load、store、AMO/LR/SC 或非 LS 操作。初版不支持 vload/vstore，生成器和手动导入校验均需要拒绝 vector LS。
- 所有通过 LSQ `needAlloc` 分配资源的 load/store/CBO transaction 都必须携带 `lqIdx` 和 `sqIdx` 两个字段。这里的“携带字段”和“消费队列 entry”需要分开理解：
  - load：`lqIdx` 为本条 load 实际占用的 LQ 起始位置，LQ 入队指针增加 1；`sqIdx` 为当前 SQ 边界或 LSQ 返回的 store 依赖窗口信息，不推进 SQ 入队指针。
  - store：`sqIdx` 为本条 store 实际占用的 SQ 起始位置，SQ 入队指针增加 1；`lqIdx` 为当前 LQ 边界或接口需要携带的 load 窗口信息，不推进 LQ 入队指针。
- `ATOMIC` 行为类不通过 LSQ `needAlloc` 分配 LQ/SQ；若 issue 接口字段要求携带 `lqIdx/sqIdx`，只填接口占位值或当前边界快照，不建立 LQ/SQ active 映射，也不参与 LSQ free count。
- 源码口径：`Dispatch.scala` 中 load/vload 对 LSQ `needAlloc` 置 bit0，store/vstore 置 bit1；`LSQWrapper.scala` 最终把 `lqIdx` 和 `sqIdx` 都返回给 dispatch 并更新到 uop 中。因此文档和 transaction 结构不能再写成 load/store 单字段模型。初版虽然不生成 vload/vstore，但字段结构保留未来扩展兼容性。
- `numLsElem/lsq_flow` 是 LSQ 资源消耗粒度：
  - 标量 load/store 的 flow 为 1。
  - 初版不计算向量 flow，所有需要 LSQ 分配的合法生成条目 `numLsElem/lsq_flow=1`。
  - `ATOMIC` 行为类 `lsq_flow=0`，但需要根据 `fuOpType` 另外派生 `atomic_data_uop_count`，供 STD 队列生成 atomic data uop。
  - 后续开放向量 LS 时，再补充 `numLsElem`、`vtype`、`vl`、`mop` 等字段和多 entry flow 计算规则。
- 资源分配、队列路由、写回/pass 和 commit/deq 判断必须调用 `derive_op_behavior(fuType, fuOpType, tlb_attr)` 得到统一行为模板，不能在各 task 中各自用 `fuType` 分散判断。

#### `op_class` 最小行为表

源码依据：

- `FuType.scala` 将标量内存类分为 `ldu`、`stu`、`mou`，其中 `mou` 是 AMO/LR/SC。
- `DecodeUnit.scala` 中 CBO decode 为 `FuType.stu`，software prefetch 会改写为 `FuType.ldu`，AMO/LR/SC decode 为 `FuType.mou`。
- `Dispatch.scala` 中 load/vload 对 LSQ `needAlloc` 置 bit0，store/vstore 置 bit1；`mou` 不置 `needAlloc`，且不通过 LSQ enq req 分配 LQ/SQ entry。
- `LSQWrapper.scala` 将 `needAlloc[0]` 接到 LQ、`needAlloc[1]` 接到 SQ。
- `MemBlock.scala` 中 `FuType.storeIsAMO(fuType)` 命中 `mou` 时，STA issue 被 `AtomicsUnit` 接管，普通 `storeUnit` 输入关闭；`AtomicsUnit` 使用 load0 的 TLB 端口和 load writeback 端口。
- `AtomicsUnit.scala` 中 LR/SC 和普通 AMO 需要 1 个 atomic data uop，AMOCAS.W/D 需要 2 个，AMOCAS.Q 需要 4 个；完成后通过 `out.toRob` 写回。
- `AtomicsUnit.scala` 中 AMOCAS.Q 还需要 2 个 STA uop；LR/SC、普通 AMO 和 AMOCAS.W/D 只需要 1 个 STA uop。
- `NewStoreUnit.scala` 和 `NewStoreQueue.scala` 中 CBO 是 store pipeline 特殊路径，MMIO/NC 来自翻译/PMP/PBMT 后的内存属性，会改变 StoreQueue/Uncache/CBO 完成路径。

初版行为表如下，原则是保守但不制造非法激励：不提前释放 LSQ，不提前判定 pass，不能观察到 DUT 完成事实时宁可等待或通过 plus 关闭该类随机权重。

| 行为类 | 判定规则 | LSQ 分配 | 发射队列 | 写回/pass 条件 | ROB commit 与 LSQ deq |
|---|---|---|---|---|---|
| `LOAD` | `fuType=ldu` 且 `!LSUOpType.isPrefetch(fuOpType)` | `needAlloc=1`，占 LQ；同时携带当前 SQ 边界 | load 队列 | 普通 cacheable load 等待 load writeback；MMIO/NC load 等待 LoadQueueUncache 的 `mmioOut/ncOut` 或等价写回；异常/replay 由异常 task 接管 | `commitType=LOAD`，正常 pass 后才允许 `lcommit`；LQ 资源只在观察到 `lqDeq/cancel` 后释放 |
| `PREFETCH` | `fuType=ldu` 且 `LSUOpType.isPrefetch(fuOpType)` | software prefetch 按源码仍是 `ldu`，`needAlloc=1`，占 LQ；hardware prefetch 不由主表生成 | load 队列 | 不等待 RF 写回；software prefetch 可用 load `toRob/LQ write`、DCache 接收或配置的 prefetch done 延迟判定 `load_pass`，若观察到异常/replay 则交给异常 task | ROB 侧按 load 类处理，pass 后才允许 `lcommit`；LQ 资源仍等 `lqDeq/cancel` |
| `STORE` | `fuType=stu` 且 `!LSUOpType.isCboAll(fuOpType)` | `needAlloc=2`，占 SQ；同时携带当前 LQ 边界 | STA 队列 + STD 队列 | STA 地址侧完成且 STD 数据写入 SQ 后可置 store pass；普通 cacheable store 可用 storeUnit 写回或 SQ `addrValid/dataValid/waitStoreS2=0/noException` 作为 pass 事实，异常/replay 由异常 task 接管 | `commitType=STORE`；ROB `scommit` 需要该 ROB entry 已满足写回/pass 条件，SQ 资源只在 `sqDeq/cancel` 后释放 |
| `STORE_MMIO_NC` 属性 | `STORE` 且 TLB/PMP/PBMT 派生 `mmio/nc` | 不改变 `STORE` 的 `needAlloc=2` | STA 队列 + STD 队列 | MMIO store 不走普通 storeUnit writeback，需等待 StoreQueue uncache response 后 `writeBack`；NC store 走 uncache req/idResp，NC ack 可作为外部完成事实。最简模型不自行判断完成，只记录 StoreQueue uncache/writeBack 或 NC ack 事实 | 区分 StoreQueue 内部 `committed` 和 ROB `scommit`：内部 `committed` 可由 `pendingPtr` 推进以启动 uncache；ROB `scommit` 仍需 ROB entry 写回/pass。NC store 可早于 NC ack/deq 进入 ROB commit，MMIO store 需等 StoreQueue `writeBack` 后才能 ROB commit/`scommit`；资源释放仍等 `sqDeq/cancel` |
| `CBO` | `fuType=stu` 且 `LSUOpType.isCboAll(fuOpType)` | `needAlloc=2`，占 SQ | STA 队列 + STD 队列；STD 对 clean/flush/inval 可填 dummy data，只用于满足 SQ data valid，`cbo_zero` 数据为 0 | 不走普通 storeUnit 立即写回路径；STA/STD 完成后可进入 StoreQueue CBO FSM，等待 CBO `writeBack` 或异常事实记录最终 pass/fault。`denied/corrupt` 由实际 CBO/StoreQueue 写回记录；若 CBO 地址属性为 uncache，`NewStoreUnit` 会转 store access fault，按异常处理 | ROB 侧仍按 store 类处理；StoreQueue 内部 `committed` 可由 `pendingPtr` 推进以启动 CBO FSM，但 ROB commit/`scommit` 需等 CBO `writeBack` 形成 ROB 可提交事实；SQ 资源等 CBO `writeBack` 导致的 `sqDeq/cancel` |
| `ATOMIC` | `fuType=mou`，包含 LR/SC/AMO/AMOCAS | `needAlloc=0`，不占 LQ/SQ，不调用 LSQ enq 分配 `lqIdx/sqIdx` | STA 队列 + STD 队列，但目标是 `AtomicsUnit` 的 `mou/moud` 路径，不写普通 StoreQueue；LR/SC/普通 AMO/AMOCAS.W/D 需要 1 个 STA，AMOCAS.Q 需要 2 个 STA；LR/SC/普通 AMO 需要 1 个 STD data，AMOCAS.W/D 需要 2 个，AMOCAS.Q 需要 4 个 | 等待 `AtomicsUnit.out.toRob` 写回；SC 成功/失败返回值都属于正常 pass，异常由 `AtomicsUnit.exceptionInfo/out.toRob.exceptionVec` 记录 | decode commitType 为 NORMAL，不计入 `lcommit/scommit`，没有 LSQ deq；完成后按普通 ROB 写回/commit 模型 retire，不触发 LQ/SQ 资源回收 |
| `MMIO/NC` 属性 | 不是独立 op_class，由 TLB/PMP/PBMT 或 TLB 表字段派生，可修饰 load/store/CBO/atomic | 不改变基础 op 的 `needAlloc`；atomic 仍不占 LSQ | 不改变基础 op 路由 | load 等待 uncache `mmio/nc` response；store 见 `STORE_MMIO_NC`；CBO-on-uncache 作为 access fault；atomic 源码中 MMIO/NC 会转访问异常或特殊完成路径，按 `AtomicsUnit` 实际写回记录 | load 的 `lcommit` 必须等 response/pass；store 的 ROB `scommit` 是否等待 uncache/writeBack 由写回事实决定：NC store 可早于 NC ack/deq，MMIO store 需等 StoreQueue `writeBack`；最终资源释放仍以 DUT 事实为准 |

实现约束：

- 主表中的 `op_class` 是生成约束和 debug 分类，真实行为以 `fuType/fuOpType` 派生出的 `op_behavior_t` 为准。建议增加 `op_subclass` 或 helper 返回值区分 `LOAD/PREFETCH/STORE/CBO/ATOMIC`，避免把 CBO 混入普通 store、把 AMO 混入普通 LSQ。
- `lsq_ctrl` 只对 `needAlloc!=0` 的条目分配和回收 LQ/SQ；`ATOMIC` 不建立 `uid_by_lq/uid_by_sq`，但仍需要通过 `robIdx -> uid` 活跃映射追踪写回和异常。
- `issue_queue_route_task` 按行为表的发射队列 mask 入队。`ATOMIC` 虽然使用 STA/STD 两类发射队列，但发往 agent 的 transaction 必须保持 `fuType=mou`，由 MemBlock 内部 `AtomicsUnit` 接管，不能改写成普通 store。
- `writeback_monitor_task` 按行为表选择完成来源：load 看 load writeback 或 uncache out，store/CBO 看 storeUnit/StoreQueue，atomic 看 `AtomicsUnit` 写回；prefetch 不等待 RF 写回。
- `lsq_commit_drive_task` 按行为表生成 `commitType` 和等待条件；`ATOMIC` 不参与 `lcommit/scommit`。必须区分 ROB `scommit` 与 StoreQueue 内部 `committed` 位：内部 `committed` 由 `pendingPtr` 推进，可用于启动 uncache/CBO 后续处理；ROB `scommit` 只能在 ROB entry 已满足对应写回/pass 条件后产生，不能把 `scommit` 和 `sqDeq` 合并成同一事件。
- `exception_redirect_replay_task` 按行为表分类异常和 replay：store STA replay 只重发 STA，load 内部 replay 默认只记录，atomic 异常来自 `AtomicsUnit`，CBO/MMIO/NC 异常来自 StoreQueue/Uncache/CBO 写回。
- 本地指针和 free count 只能在真实 LSQ 入队 fire 后推进；如果 DUT `canAccept` 不允许入队，本拍只保持候选项，不消耗 `lqIdx/sqIdx`。`needAlloc=0` 的 `ATOMIC` 不消耗 LQ/SQ、不检查 LQ/SQ free count、不驱动 LSQ enq；若 Task1 模拟 Dispatch admission，仍需要考虑 `Dispatch.scala` 中全局 `lsqCanAccept` 对 `fromRename.ready/fire` 的门控，同时还要受发射队列容量、redirect/flush 屏障和 ROB active 映射检查约束。
- 回收逻辑按源码语义拆开处理：
  - LQ 资源主要随 load commit/deq、redirect/flush cancel 释放，不能仅因 load 发射或写回就提前释放。
  - SQ 资源主要随 store commit 后的 deq/sbuffer 完成路径、redirect/flush cancel 释放，不能仅因 STA/STD 发射就提前释放。
  - 状态表记录每个 `uid` 的入队、发射、写回、提交、flush 状态，`lsq_ctrl` 根据状态事件和 DUT monitor 返回的 deq/cancel 事实更新空闲资源。
- `lsq_ctrl` 的本地 free count 和 head/tail 指针只是软件镜像，释放和取消的最终真源必须是 DUT monitor 采样到的 `lqDeq/sqDeq/lqDeqPtr/sqDeqPtr/lqCancelCnt/sqCancelCnt`。本地模型可以预测下一状态，但不能仅凭预测释放资源。
- 每次采样到 `lqDeq/sqDeq/cancelCnt` 后，`lsq_ctrl` 必须校验本地预期释放范围、数量和 DUT 返回的 pointer/cancel count 是否一致；一致时更新 free count、head/tail 镜像和 `uid_by_lq/uid_by_sq` 活跃映射。
- 如果本地预测与 DUT monitor 不一致，默认 `uvm_fatal`，因为后续 `lqIdx/sqIdx` 分配已经不可信。仅当 `seq_csr_common.sv` 中 `lsq_resync_on_mismatch` 通过 plus 显式开启时，允许按 DUT monitor 的 deq/cancel 结果重同步本地 free count、pointer 镜像和 active LQ/SQ 映射，并打印 `uvm_warning`；该模式只用于 debug，不作为默认回归行为。
- 分配结果在 LSQ 入队 fire 后回填 `main_table_by_uid[uid]`，调用 `activate_uid(uid, op_behavior)` 建立 active 映射，并同步更新 `common_data_transaction.sv` 中的状态表；non-LSQ admission 成功时只建立 ROB active 映射，不回填真实 LQ/SQ 分配。

### 6.3 Task1：发射入队 task

数据来源：

- 从 `common_data_transaction.sv` 的 `main_table_by_uid[]` 中按 `next_enq_uid` 顺序提取待入队 transaction，避免每拍全表扫描；ROB 相关合法性判断仍使用主表保存的 `robIdx_flag/value`。

入队参数：

- 每拍最大尝试入队数量由 `seq_csr_common.sv` 控制，但这只是配置上限，不是实际 fire 数量。
- 每拍入队数量不得超过 LSQ 入队端口宽度。
- 入队端口按从小到大顺序分配。
- 若本拍最终确认可入队数量为 8，则 8 个端口全部启用，并依次对应 8 条入队数据。
- 实际可入队数量需要同时受 DUT 返回的可入队信号限制。至少需要监测全局 `canAccept`；如果接口暴露 `lqCanAccept`、`sqCanAccept`，也需要一并采样并纳入判定。
- 源码中 `LSQWrapper` 的 `canAccept` 等价于 LQ 与 SQ 都可接收；load queue 断言入队时 `sqCanAccept` 也必须满足，store queue 断言入队时 `lqCanAccept` 也必须满足。因此测试框架不能只按本地配置数量盲目推 valid。

核心流程：

1. 根据 `seq_csr_common.sv` 参数确定当前拍最多尝试入队数量。
2. 读取 DUT/LSQ 入队可接受信号，形成本拍 `can_enqueue_now`。
3. 从主表中按 `next_enq_uid` 选择候选项，并调用 `derive_op_behavior()` 计算每个候选项的 `needAlloc/numLsElem/lsq_flow`。
4. 对 `needAlloc!=0` 的候选调用 `lsq_ctrl` 做临时资源检查和临时索引计算，检查 LQ/SQ 空闲资源是否能容纳候选项累计 flow；对 `needAlloc=0` 的 `ATOMIC` 候选不驱动 LSQ enq、不检查 LQ/SQ free count，只执行 non-LSQ admission。
5. 本拍实际入队数量取以下条件的最小可行集合：
   - `seq_csr_common.sv` 配置的每拍最大入队数。
   - LSQ 入队端口宽度。
   - 待入队主表条目数量。
   - 需要 LSQ 分配的候选必须满足 DUT `canAccept/lqCanAccept/sqCanAccept` 允许的入队状态。
   - 需要 LSQ 分配的候选必须满足 LQ/SQ 当前可用资源和 `numLsElem/lsq_flow` 需求。
   - `ATOMIC` 候选不消耗 LSQ 端口和 LQ/SQ free count；若 Task1 模拟 Dispatch admission，仍需受全局 `lsqCanAccept` 对 dispatch fire 的门控，并受主表顺序、发射队列容量和 redirect/flush 屏障约束。
6. 对 `needAlloc!=0` 的候选，根据临时索引和主表字段生成 `LsqEnqTransaction`，驱动入队接口；对 `needAlloc=0` 的 `ATOMIC`，不生成 `LsqEnqTransaction`，只在状态表中记录 `non_lsq_admitted` 或等价标识。
7. 只有观测到 LSQ 入队 fire，才确认 `needAlloc!=0` 的本拍成功项：
   - 回填主表 `lqIdx/sqIdx/numLsElem`。
   - 调用 `common_data_transaction::get().activate_uid(uid, op_behavior)`，建立当前活跃 `robIdx -> uid` 映射。
   - 更新 `uid_by_lq[lqIdx]` 或 `uid_by_sq[sqIdx]` 等稳定 LSQ 索引。
   - 推进 `lsq_ctrl` 的 LQ/SQ 本地入队指针和 free count。
   - 更新 `common_data_transaction.sv` 状态表中的 `enq` 标识。
   - 触发后续 TLB 映射 task。
8. `ATOMIC` non-LSQ admission 成功时：
   - 不回填真实 `lqIdx/sqIdx` 分配，不更新 `uid_by_lq/uid_by_sq`，不推进 LQ/SQ 指针。
   - 调用 `activate_uid(uid, op_behavior)` 建立活跃 `robIdx -> uid` 映射。
   - 设置 `enq/non_lsq_admitted`，并触发后续 TLB 映射 task 和队列路由 task。
9. 如果 DUT 不能接收或 flush/redirect 屏障阻塞，候选项保持待入队或待 admission 状态，下一拍重试，不消耗索引、不推进指针、不更新 `enq`。

向量 LS 初版策略：

- 当前框架初版只支持标量 LS，合法生成条目均按 `numLsElem=1`、`lsq_flow=1` 处理。
- 主表随机生成器不生成 vload/vstore，也不提供 vector LS 权重入口。
- 手动主表导入时若发现 vector LS 条目，直接 `uvm_fatal`，避免字段不完整的向量 transaction 进入后续入队、TLB、发射或 commit 流程。
- 后续需要支持向量时，再单独补充向量字段、flow 计算、多 entry 预留、vector writeback/commit/deq 和对应验收场景。

### 6.4 Task2：TLB 地址映射 task

输出产物：

- `tlb_table_by_uid[uid]`：按主表 `uid` 追溯的 TLB transaction 动态数组。
- `tlb_uid_by_key[tlb_lookup_key_t]`：按 `{vpn, asid, vmid, s2xlate}` 翻译 key 查询 `uid` 的关联数组。
- `tlb_table_by_uid[]` 是追溯表，默认保留到 testcase 结束；`tlb_uid_by_key[]` 是 lookup 索引，可按 CSR/SFENCE、redirect/flush、reset 或显式重建策略清理。两者不能混为一谈。

触发机制：

- 由 Task1 触发。每有 transaction 成功 LSQ 入队或完成 non-LSQ admission，即生成或更新对应的 TLB 映射数据。

输入来源：

- `common_data_transaction.sv` 状态表中已入队或完成 non-LSQ admission、且 `active=1` 的 `uid`。
- 主表中对应条目的 `vaddr/VPN`、`tlbAF`、`tlbPF`、`tlbGPF`、`PBMT`。
- `seq_csr_common.sv` 提供的地址约束参数，例如起始地址和范围。
- `seq_csr_common.sv` 提供的 TLB/PTE 权限位、N 位、V 位等 plus 权重参数。
- `common_data_transaction.sv` 中的 `mmu_csr_runtime_state`，包括当前 ASID、VMID、特权态、虚拟化使能状态以及 S1/S2 翻译相关 CSR。该运行时镜像由 `csr_agent monitor` 实时更新；`csr_csr_common.sv` 只允许在 reset/init 阶段提供初始 baseline。

核心功能：

- 基于 `tlb_lookup_key_t'{vpn, asid, vmid, s2xlate}` 建立 TLB 查找索引，生成虚拟地址到最终物理地址的映射。`stage_mode` 不单独进入 key，当前简化方案用 `s2xlate` 区分翻译模式。
- 将 `PBMT`、`tlbAF`、`tlbPF`、`tlbGPF` 写入对应 TLB 表项。
- 生成每条 TLB transaction 前读取 `mmu_csr_runtime_state`，并用该运行时镜像配置 `ASID`、`VMID`、特权态和虚拟化相关字段。
- `vpn` 来自主表 `vaddr` 派生；L2TLB 回复路径中 `vpn` 和 `s2xlate` 以 `l2tlb monitor` 采集到的请求为准，`asid/vmid/update_seq` 使用请求采样时刻的 `mmu_csr_runtime_state` 快照，避免 request 到 reply 之间 CSR 变化造成 key 漂移。
- 根据 `mmu_csr_runtime_state` 的虚拟化状态决定 S1/S2 映射策略；请求侧已有 `s2xlate` 时，以请求 `s2xlate` 作为 lookup key 字段。
- 禁止从 `seq_csr_common.sv` 获取 CSR/虚拟化实时状态；`seq_csr_common.sv` 中若存在被测试流程修改后的 CSR 配置，只能作为控制/权重配置，不作为 TLB 映射的实时真源。
- 禁止在运行阶段直接从 `csr_csr_common.sv` 重新读取 CSR 作为当前真值；该接口只代表初始配置，运行时真值必须来自 `csr_agent monitor` 维护的 `mmu_csr_runtime_state`。
- 生成的物理地址需要落在 `seq_csr_common.sv` 给定的地址范围内。
- 根据 VPN 推导 PTE index、`ppn_low`、`valid_index`，避免手动随机出不一致结果。

最小查找函数：

```systemverilog
function automatic bit lookup_tlb_uid(
  input  tlb_lookup_key_t key,
  input  longint unsigned csr_update_seq,
  output memblock_uid_t   uid
);
```

该函数只负责 `tlb_uid_by_key.exists(key)` 查询、`uid` 返回和 `tlb_table_by_uid[uid].csr_update_seq == csr_update_seq` 一致性校验，不额外承担状态更新、异常注入或重建 TLB 表。若 key 存在但 `csr_update_seq` 不一致，按 miss 处理，由调用方按配置刷新映射或报错。完整 TLB transaction 内容统一通过 `tlb_table_by_uid[uid]` 回查。

同一个 `{vpn, asid, vmid, s2xlate}` 被多个 active `uid` 使用时，不引入 canonical/ref uid 语义。Task2 可以复用相同映射内容，但必须把 TLB entry 复制或写入到每个使用者自己的 `tlb_table_by_uid[uid]`，保证异常、写回和追溯仍能按 transaction `uid` 独立定位。若同一个 active key 出现冲突映射，默认 `uvm_fatal`，除非 testcase 明确触发 CSR/SFENCE/redirect 后重建对应 TLB 映射。

TLB 表生命周期策略：

- commit/deq 完成后，不删除 `tlb_table_by_uid[uid]`。该表项与主表、状态表一起作为 testcase 内追溯记录保留，用于迟到反馈分析、覆盖率、debug 和 `end_test_check()`。
- `tlb_uid_by_key[]` 只承担运行时 lookup 加速。若 `csr_update_seq` 变化、SFENCE、redirect/flush 取消未完成请求或显式配置要求重建映射，可以删除或重建对应 key 索引；删除索引不影响 `tlb_table_by_uid[uid]` 历史记录。
- 如果多个 `uid` 共享同一 lookup key，索引只能指向当前可用于新请求的 active 或最新合法 entry；旧 `uid` 的 per-uid TLB 记录仍保留。需要查历史时必须通过 `uid` 访问 `tlb_table_by_uid[uid]`，不能反向依赖 `tlb_uid_by_key[]`。
- testcase 结束或下一轮 `reset_all_tables(main_trans_num)` 时，才统一删除并重新分配 `tlb_table_by_uid[]`，同时清空 `tlb_uid_by_key[]`。

S1/S2 配置规则：

- 非虚拟化场景：配置 S1。
- Only S2 场景：仅配置 S2。
- All-stage 场景：S1 和 S2 均配置。

字段作用范围约束：

- Only S2：主表中的 `PBMT`、`tlbAF`、`tlbGPF` 作用于 S2 表项，S1 的 `tlbPF` 置 0。
- All-stage：主表中的 `PBMT`、`tlbAF`、`tlbGPF` 作用于 S2 表项，`tlbPF` 作用于 S1 表项。
- Only S1：`tlbGPF` 置 0，`PBMT`、`tlbAF`、`tlbPF` 作用于 S1 映射结果。

其他权限位、N 位、V 位等字段需要通过约束随机生成，权重必须来自 `plus.sv -> seq_csr_common.sv` 解析后的 plus 参数，并在 `post_randomize()` 中完成合法性修正，不能完全无约束随机。`post_randomize()` 需要处理 R/W/X 非法组合、V=0 时其他权限位的合法化、N 位与 Svnapot 场景约束，以及 S1/S2 不同阶段下 U/G/A/D 等位的合法性修正。

CSR 运行时一致性规则：

- `mmu_csr_runtime_state` 在 testcase reset/init 时可由 `csr_csr_common.sv` 初始化，之后只由 `csr_agent monitor` 根据 DUT CSR 更新维护。
- 所有基于 CSR 的 task 都必须读取 `mmu_csr_runtime_state`。除初始化外，禁止直接读取 `csr_csr_common.sv` 或从 `seq_csr_common.sv` 推断当前 CSR 真值。
- TLB 表项需要记录生成时使用的 `asid/vmid/s2xlate/priv_mode/csr_update_seq`。如果后续 `csr_agent monitor` 观察到影响翻译的 CSR 变化，使当前 `mmu_csr_runtime_state.update_seq` 与表项记录不一致，则旧表项只允许用于已发射且未被 flush 的追溯记录；新请求必须重新通过 Task2 生成或刷新 TLB 映射。
- L2TLB 回复 task 查询表项时，用请求侧 `vpn/s2xlate` 和请求采样时刻保存的 `asid/vmid/csr_update_seq` 构造 key 并调用 `lookup_tlb_uid()`。若查不到 key 或 `csr_update_seq` 不匹配，不允许回退到初始 CSR 值，应按配置选择生成新映射或报错。

### 6.5 Task3：发射队列路由与维护 task

原“子表拆分与实时维护 task”更名为“发射队列路由与维护 task”，建议实现名为 `issue_queue_route_task`。

触发机制：

- 由 TLB 地址映射 task 触发。TLB 映射完成后立即执行，形成“入队 -> TLB 映射 -> 队列路由”的连续流程。

核心逻辑：

- 从 `common_data_transaction.sv` 状态表中读取已经入队或完成 non-LSQ admission、`active=1` 且完成 TLB 映射的 `uid`。
- 根据 `derive_op_behavior()` 从 `main_table_by_uid[uid]` 的 `fuType/fuOpType/op_class` 派生发射目标，将该 `uid` 路由到 load、STA、STD 三个发射队列。
- `ATOMIC` 行为类路由到 STA 和 STD 队列，但必须保留 `fuType=mou`。STA 侧进入 `AtomicsUnit.io.in`，STD 侧通过 `StdExeUnit.io.atomicData` 提供 atomic data；LR/SC/普通 AMO/AMOCAS.W/D 的 `atomic_sta_uop_count=1`，AMOCAS.Q 的 `atomic_sta_uop_count=2`；LR/SC/普通 AMO 的 `atomic_data_uop_count=1`，AMOCAS.W/D 为 2，AMOCAS.Q 为 4。
- 不再生成 load、STA、STD 三张子表。
- 三个队列存放在 `common_data_transaction.sv` 中，由该 task 统一生成和维护。
- 队列元素建议保存轻量调度快照，发射时再根据 `uid` 回查主表、TLB 表和状态表。
- 最小推荐字段为 `uid`、`rob_key`、`target`、`send_pri`、`ready_cycle` 或 `delay_left`、`replay_seq`。队列项中的 `send_pri` 表示该入口的实际发射优先级：load/STA 队列项取自主表 `send_pri`，store 拆分出的 STD 队列项取自主表 `send_pri_std`。该值在主表生成后基本稳定，缓存到队列中可以避免每拍扫描队列时频繁回查主表。
- 可选缓存 `lqIdx`、`sqIdx`、`numLsElem` 等发射高频字段。需要 LSQ 分配的 load/store/CBO 可以缓存真实 `lqIdx/sqIdx`；`ATOMIC` 只缓存占位值或不缓存。是否缓存不再按 load/store 二选一，而应按 driver 访问频率和实现复杂度决定。
- 不建议在队列中复制完整 transaction，也不建议缓存 `src_0/imm/vaddr/tlb*`、`pc/ftq/pdest/rfWen/fpWen`、状态字段等信息。这些字段仍以主表、TLB 表和状态表为准，避免多份副本不一致。

队列元素结构示例：

```systemverilog
typedef struct packed {
  int unsigned     uid;
  rob_key_t        rob_key;
  issue_target_e   target;
  int unsigned     send_pri;
  longint unsigned ready_cycle; // 或 delay_left
  int unsigned     replay_seq;
  bit              has_lqIdx;
  int unsigned     lqIdx;
  bit              has_sqIdx;
  int unsigned     sqIdx;
  int unsigned     numLsElem;
} issue_q_item_t;
```

其中 `lqIdx/sqIdx/numLsElem` 是可选缓存字段，不改变主表和状态表的权威性。

- 队列需要防重复入队；replay 或 redirect 场景下，由状态表控制是否重新入队。队列路由、发射选择、队列删除和 replay 去重都以 `(uid, target, replay_seq)` 为准，`rob_key` 只用于 DUT 语义比较、flush 边界判断和 debug，不作为队列删除的权威 key。
- 队列不承担历史记录职责。条目一旦被对应发射 task 成功送入流水线，就可以立即从对应队列删除；后续状态追踪依赖主表和状态表。

#### replay 重新入队规则

replay 重新进入发射队列必须由 `exception_redirect_replay_task` 先完成事件分类和状态恢复，再由 `issue_queue_route_task` 统一入队，禁止写回 task 或发射 task 直接把条目塞回队列。

源码上需要区分两类事件：

- `replay-only`：例如 store 地址侧 TLB miss 通过 STA feedback 让 IssueQueue 重新发射。这类事件不产生全局 redirect，不需要 flush，不清理 LSQ entry，只清对应入口的发射状态。
- `redirect-flush`：例如 memory violation、nuke、nack rollback 或 trap redirect。这类事件会触发 flush 边界，必须先冻结发射、清理被 flush 的队列项和状态，再决定是否重新入队。

推荐规则：

- 每个 replay 条目使用 `(uid, target, replay_seq)` 作为去重 key。`replay_seq` 每次确认需要重发时加 1，队列中已有同 key 时不得重复插入。
- 正常路径下，队列项在对应流水线实际 fire 成功后立即删除，因此 replay 到来时通常不会有旧队列项残留。但 replay 入队仍必须具备幂等性：每次确认需要 replay 并准备重新入队前，先扫描目标队列并删除同 `(uid, target)` 的旧 pending 项，再插入新的 `(uid, target, replay_seq)`。
- 该清理只扫描 load/STA/STD 三个发射队列中的轻量项，不新增 `robIdx -> queue_index` 或 `uid -> queue_index` 这类易失效辅助索引。历史追溯仍通过主控制表和状态表完成。
- Task4 从队列取候选后、真正 drive 前必须二次检查 `status_by_uid[uid].replay_seq == entry.replay_seq`，并确认当前入口仍 eligible；若不匹配，说明这是旧 replay/旧 epoch 残留项，直接丢弃该队列项，不得发射。
- `replay_pending=1` 表示事件已捕获并需要重发；只有 `replay_target_mask` 指定了目标队列时，队列路由 task 才能重新入队。
- replay-only 不设置 `flush_in_progress`，不递增 `global_issue_epoch`，也不阻塞其他无关 `uid` 的发射；只阻止同一 `uid` 的旧队列项、旧写回或重复 replay 覆盖状态。
- store 地址侧 replay：只重新放入 `sta_issue_q`，清除 `sta_dispatch` 和 STA 侧完成状态，保留 `std_dispatch`、STD data 状态和已分配的 `sqIdx/lqIdx`。
- store 数据侧 replay：若后续存在数据侧 feedback，则只重新放入 `std_issue_q`，不清 STA 地址状态。当前源码中常见 store TLB miss 是 STA feedback，因此初版可只实现 STA replay。
- load 的非 redirect replay 如果需要由 TB 重发，才重新放入 `load_issue_q`；MemBlock 内部 `LoadQueueReplay` 送回 LoadUnit 的事件默认只观察记录，不主动从 TB load 队列再发一次，避免重复激励。
- redirect-flush 覆盖到的条目不能直接 replay 入 load/STA/STD 发射队列。当前简化模型中被 flush 覆盖且已取消的条目标记为 `flushed` 并 retire，不在同一路径内维护重新取指状态位。
- 初版最简化策略：redirect-flush 后只恢复或重发 redirect 边界要求继续执行的条目；younger 条目标记 `flushed` 并从三个队列删除，不主动重发，避免模拟完整后端 refetch/rename。
- redirect flush 恢复未完成前，不允许任何被 flush 边界覆盖的 replay 条目重新入队，避免旧 epoch 发射和新 epoch replay 同拍冲突。

store 路由说明：

- 上游先通过 `fuType=stu` 或 op_class 判断该条是 store，并分配 SQ。
- 队列路由逻辑将该 store 对应 `uid` 同时放入 STA 队列和 STD 队列。
- STA 队列项缓存的 `send_pri` 来自主表 `send_pri`；STD 队列项缓存的 `send_pri` 来自主表 `send_pri_std`。两者独立参与后续全局优先级仲裁。
- STA 侧发射时使用 `src_0 + imm` 生成地址。
- STD 侧发射时使用 store data 源写入 SQ data。
- 到 MemBlock Verilog 端口时，STA 和 STD 已经是不同入口，不是在 MemBlock 内部再靠 `fuType` 拆分。

维护要求：

- 一旦有新入队且完成 TLB 映射的数据，立即更新对应队列。
- 队列与主表、TLB 表和状态表保持一致。
- 发射成功后，队列维护 task 或发射 task 需要按 `(uid, target, replay_seq)` 删除对应队列项，避免同一入口重复发射。
- commit、flush、redirect 或 replay 时，队列维护 task 根据状态表判断是否需要清理残留队列项或重新放入对应 `uid`。

### 6.6 Task4：发射控制 task

发射控制 task 负责从 `common_data_transaction.sv` 中的 load、STA、STD 三个发射队列选择候选 transaction，并驱动对应流水线接口。

发射控制需要先读取 `seq_csr_common.sv` 中的以下参数：

- `global_send_pri_en`：是否启用全局优先级调度。
- `load_pip_num`：本拍最多允许进入 load 流水线的数量，不能超过真实 load 流水线数量。
- `sta_pip_num`：本拍最多允许进入 STA 流水线的数量，不能超过真实 STA 流水线数量。
- `std_pip_num`：本拍最多允许进入 STD 流水线的数量，不能超过真实 STD 流水线数量。

每拍实际可发射数量还需要受队列中 eligible 元素数量限制。eligible 元素至少需要满足：

- 已完成 LSQ 入队或 non-LSQ admission。
- 已完成 TLB 映射或不需要 TLB 映射。
- 当前目标入口尚未完成：load 看 `load_dispatch/writeback/pass`，STA 看 `sta_dispatch/sta_pass`，STD 看 `std_dispatch/std_pass`；store 不能因为另一侧已完成就阻塞当前侧 replay。
- 未被 `redirect_pending` 阻塞。
- `replay_pending=1` 时只有 `replay_target_mask` 包含当前队列才可作为 replay 候选。
- 对应流水线本拍 ready。
- 全局 `flush_in_progress` 为 0。初版保守策略下 redirect 恢复期间不允许继续发射 older 或 younger 候选。
- 候选携带的 target 级 `issue_epoch/replay_seq` 必须与当前状态一致，且没有 `issue_killed`。重复入队由队列项 `(uid, target, replay_seq)` 去重和 `queued_*` 状态保护，不维护独立 replay 入队状态位。

发射控制 task 内部仍可拆成三个专项发送 task：

- load 发射 task。
- STA 发射 task。
- STD 发射 task。

但候选选择需要先经过统一调度仲裁，再分别交给三个专项 task 发送。

#### redirect/flush 与 replay-only 发射握手

发射控制 task 必须和 `exception_redirect_replay_task` 通过 `common_data_transaction.sv` 中的全局控制状态协同，至少包含：

- `flush_in_progress`：只有 redirect/trap/memory violation 等需要全局 flush 屏障的事件才置 1。store RS replay 等 replay-only 事件不得置该位。
- `active_redirect`：当前 redirect payload，包含 flush 边界 `robIdx`、`ftqIdx/ftqOffset`、level 和 target。
- `global_issue_epoch`：每次 redirect/flush 生效后递增，用于区分 redirect 前已经发出的旧 transaction 和恢复后的新 transaction。
- `issue_freeze_ack`：Task4 在某一拍采样到 `flush_in_progress=1` 后，确认本拍不再选择或驱动新候选时拉起的确认信号。异常处理 task 只能在下一拍观察到该 ack 后清队列和改状态，用于避免同一仿真 delta 内边清队列边发射。

默认采用保守屏障策略：

1. `exception_redirect_replay_task` 捕获 replay-only 事件时，只更新对应 `uid` 的 `replay_pending/replay_target_mask/replay_seq`，不冻结三队列，不递增 epoch。
2. `exception_redirect_replay_task` 捕获 redirect-flush 事件时，只设置 `flush_in_progress=1` 和 `active_redirect`，并记录下一轮将使用的 `global_issue_epoch`。本拍不直接清 load/STA/STD 队列，也不覆盖 per-uid 状态。
3. Task4 每拍采用固定阶段：采样全局控制状态和队列状态 -> 仲裁候选 -> 发射前二次检查 -> drive agent -> 更新状态和删除已 fire 队列项。上述阶段在 Task4 内顺序固定，不允许异常处理 task 在同一 delta 中穿插清队列。
4. 如果 Task4 在本拍采样阶段看到 `flush_in_progress=1`，则本拍停止从 load/STA/STD 队列选择新候选，并在本拍末或下一拍可见位置拉起 `issue_freeze_ack=1`。
5. 如果 `flush_in_progress` 是在 Task4 本拍采样之后才被置位，而 Task4 已经完成仲裁但尚未 drive，Task4 必须在发射前二次检查中重新读取 `flush_in_progress/active_redirect`。若候选 `robIdx` 被 redirect 命中，则取消本次 valid，不更新 `dispatch`，该队列项后续由异常处理 task 统一清理。
6. 如果 transaction 已经在同一拍 drive 到 DUT，Task4 必须记录对应 target 的 `issue_epoch`。后续若确认该 `robIdx.needFlush(active_redirect)`，状态表设置 `issue_killed=1`，该条旧 epoch 写回或 feedback 只能被忽略，不能更新新状态。
7. `exception_redirect_replay_task` 只有在下一拍采样到 `issue_freeze_ack=1` 后，才能清理发射队列残留项、更新状态表并处理 `lqCancelCnt/sqCancelCnt`。如果 ack 未到，只能等待，不得提前清队列。
8. flush 恢复完成后，异常处理 task 递增并发布新的 `global_issue_epoch`，清除 `flush_in_progress/issue_freeze_ack`，Task4 才能在新 epoch 下重新选择候选。

是否允许 redirect 期间穿插发射：

- 初版不允许。redirect 生效到 `flush_recover_done` 之间不发射任何新候选，这是最简单且不容易出错的策略。
- replay-only 不属于 redirect 期间穿插发射，不需要 flush 屏障；它只通过目标队列重新发射同一 `uid` 的指定入口。
- 后续如果为了提高压力需要优化，可以允许严格 older-than-redirect 且不受 flush 影响的条目继续发射；但该模式必须增加 `robIdx` 边界判断和 epoch 校验，初版不推荐。

发射 task 的状态更新也需要区分成功发射和被 flush 杀掉：

- 正常发射：按目标入口设置 `load_dispatch`、`sta_dispatch` 或 `std_dispatch`，并把新 epoch 记录到 `load_issue_epoch/sta_issue_epoch/std_issue_epoch` 中对应入口，从对应队列删除。load 完成后可同步设置总 `dispatch`；store 需要 STA 和 STD 都完成发射后再认为总 `dispatch` 完成。
- replay 发射：除上述入口状态外，还需要清除该入口对应的 `replay_target_mask` bit，保留 `replay_seq` 作为追溯；如果 `replay_target_mask` 中所有目标入口都已重新发射，清 `replay_pending`。
- 发射前被 redirect 命中：不设置入口 dispatch，不删除或由异常处理 task 统一清理。
- 发射后被 redirect 命中：设置 `issue_killed=1`，后续写回监测 task 忽略该旧 epoch 写回；如进入局部 replay，则由异常处理 task 设置 replay 状态，不由发射 task 自己入队。

#### `global_send_pri_en=1`：全局优先级调度

当 `global_send_pri_en` 为高时，调度策略以队列项中缓存的实际 `send_pri` 为第一优先级，且比较范围是 STA、STD、load 三个队列的所有 eligible 元素。对于同一 store 的两个入口，STA 队列项使用主表 `send_pri`，STD 队列项使用主表 `send_pri_std`，两者可以被调度成不同优先级。

调度规则：

1. 同一拍先扫描 `sta_issue_q`、`std_issue_q`、`load_issue_q` 三个队列中的 eligible 元素。
2. 在三个队列的全集中找出当前最大的实际发射优先级，即队列项缓存的 `send_pri`。
3. 只从当前最大实际发射优先级的候选集合中选择本拍要发射的 transaction。
4. 如果最大实际发射优先级同时出现在 STA、STD、load 三类队列中，三类队列可以在同一拍并发发送到各自流水线。
5. 实际 driver 调用可以按 STA、STD、load 的固定顺序执行发送 task，但同一拍输出 valid 允许并发拉起。
6. 如果同一类队列中有多个相同最大 `send_pri`：
   - 数量不超过该类流水线可用数量时，全部可以同拍发射；发送顺序可随机。
   - 数量超过该类流水线可用数量时，调用 `rob_order_util` 按 ROB 环形顺序筛选前 N 个发射，其中 N 为该类本拍可用流水线数量；`uid` 只允许作为同一 `rob_key` 不应出现时的 debug tie-break。
7. load 队列最多发射 `min(load_pip_num, load_pipe_count, load_eligible_count)` 条。
8. STA 队列最多发射 `min(sta_pip_num, sta_pipe_count, sta_eligible_count)` 条。
9. STD 队列最多发射 `min(std_pip_num, std_pipe_count, std_eligible_count)` 条。

示例：

- 若当前全局最大实际发射优先级为 90，且 STA、STD、load 中都存在队列项 `send_pri=90` 的 eligible 元素，则 STA、STD、load 可同拍分别发射。
- 若 load 队列中有 3 条 `send_pri=90`，且 `load_pip_num=3`，则 3 条可同时进入 3 条 load 流水线。
- 若 load 队列中有 5 条 `send_pri=90`，但 `load_pip_num=3`，则调用 `rob_order_util` 按 ROB 环形顺序选择 3 条发射，其余保留到后续周期。
- STA/STD 队列同理，只要同优先级候选数量不超过对应流水线数量，就可以同拍发送到不同 STA/STD 流水线。
- 同一 store 的 STA/STD 是否同拍发射由二者各自队列项的实际优先级、eligible 状态和对应流水线 ready 共同决定。若 `send_pri == send_pri_std` 且 STA/STD 均 eligible，可以同拍分别发往 STA/STD；若需要刻意拉开两侧发射时序，可通过主表或 plus 权重让 `send_pri_std` 与 `send_pri` 不同。

#### `global_send_pri_en=0`：三队列完全并行调度

当 `global_send_pri_en` 为低时，`send_pri` 不参与调度，STA、STD、load 三个队列完全并行选择候选项。三类流水线之间不做全局优先级互斥。

调度规则：

- load 每拍最多发射 `min(load_pip_num, load_pipe_count, load_queue_eligible_count)` 条。
- STA 每拍最多发射 `min(sta_pip_num, sta_pipe_count, sta_queue_eligible_count)` 条。
- STD 每拍最多发射 `min(std_pip_num, std_pipe_count, std_queue_eligible_count)` 条。
- 三类队列互不阻塞，可以同拍分别向各自流水线发送。
- 每类队列内部可以随机选择候选项；如果需要确定性回归，可通过参数切换为按 ROB 环形顺序选择，`uid` 只作为 debug tie-break，不能替代 ROB 顺序语义。
- `load_pip_num/sta_pip_num/std_pip_num` 是配置上限，真实可发送数量必须同时受 DUT 流水线数量、ready 信号和队列 eligible 元素数量限制。

#### 发射前字段赋值

在真正驱动 agent 发送前，需要按固定顺序完成字段赋值：

1. 根据队列项中的 `uid` 从 `main_table_by_uid[uid]` 读取 `main_control_transaction`。
2. 将主表中的第一类字段赋值到本次将要发往 agent 的 transaction。
3. 调用 `assign_issue_dep_fields()`，补齐第二类字段；如果这是 replay 发射，需要根据 `replay_seq/replay_cause` 重新计算 `isFirstIssue` 等字段。
4. 调用 `assign_backend_meta_fields()`，补齐第三类字段。
5. 记录本次 transaction 的 TB 侧 `issue_epoch/replay_seq`，并在驱动前执行 redirect、`replay_seq` 和 eligible 状态二次检查。
6. 驱动 load/STA/STD agent 发送 transaction。

这样可以确保主表保持为全局控制表，而实际发往 agent 的 transaction 满足 MemBlock issue 接口约束。

#### 状态表更新

任一 transaction 被发射到对应流水线后，发射控制 task 必须通过 `common_data_transaction.sv` 提供的统一状态更新 API 按 `uid` 更新状态字段：

- 按发射目标设置入口级状态：load 设置 `load_dispatch`，STA 设置 `sta_dispatch`，STD 设置 `std_dispatch`。
- 记录发射目标类型、target 级 `issue_epoch`、`replay_seq` 和 cycle 计数，便于 debug、条件更新和 replay 追溯；同样的 `issue_epoch/replay_seq/target` 快照必须能被对应 monitor 反馈带回或从 driver 侧发送记录中查回。
- 按 `(uid, target, replay_seq)` 从对应发射队列中删除已发射项。队列删除只表示“已离开发射等待队列”，不表示该 transaction 已写回或提交。
- 对于 store 同时进入 STA/STD 队列的情况，STA 和 STD 分别维护 `sta_dispatch`、`std_dispatch`；只有两者都完成后才将总 `dispatch` 视为完成。
- replay 发射成功后，只清当前入口对应的 `replay_target_mask` bit；所有目标 bit 都清空后，才清 `replay_pending`。

状态字段更新不得通过整条状态表项覆盖实现，必须调用统一 API 做字段级更新，避免和写回、异常、commit task 的状态更新互相覆盖。

发射后的追溯规则：

- 队列中只保留尚未发射的待选项。
- 已发射条目的生命周期状态由主任务状态表追踪。
- 已发射条目的原始控制字段由主控制表追踪。
- replay-only 需要重新发射时，由 `exception_redirect_replay_task` 更新 `replay_pending/replay_target_mask/replay_seq`，再由队列维护 task 将对应 `uid` 放入目标队列；不需要 flush 屏障。
- redirect-flush 生效时，必须先完成 flush 屏障、队列清理和 LSQ cancel 处理；被 flush 覆盖且已取消的 entry 不直接塞回发射队列。
- 旧 epoch 的写回、replay 或异常反馈不得覆盖新 epoch 状态；所有 monitor 更新状态前都需要校验 target 级 `issue_epoch/replay_seq`。

#### 发射时序控制

主表中的 `delay` 字段用于控制发射到 driver 的时序。通过 gap transaction 中的 gap 参数实现精确延迟控制，确保 DUT 输入时序合法。

### 6.7 Task5：异常监测与写回处理 task

监测目标：

- MemBlock 到后端的写回端口。
- `lqIdx`、`sqIdx`、`robIdx` 等返回信息。
- 异常、replay、redirect 相关信号。
- 多 load pipe、多 store/StoreQueue 写回、atomic 写回、STA/STD feedback、`memoryViolation` 和 `ExceptionInfoGen` 相关异常信息。初版主表不生成 vector LS；若 monitor 采样到 vector LS 写回或 feedback，必须记录原始事件并默认 `uvm_fatal("UNSUPPORTED_VECTOR_LS_WB")`，后续扩展时再按 `uopIdx/elemIdx` 增加向量分片计数和合并规则。

源码口径：

- `MemBlock.scala` 中 load 写回端口 `writebackLda` 可能来自普通 `LoadUnit`，其中 `AtomicWBPort` 会在 `atomicsUnit.io.out.toRob.valid` 时优先承载 atomic 写回。
- store 普通路径、StoreQueue MMIO/CBO writeback、LSQ exceptionInfo、atomic exceptionInfo 和 load exceptionInfo 都可能进入异常信息选择路径。
- `NewStoreQueue.scala` 的 MMIO/CBO `writeBack.valid` 拉起时，`writeBack.bits.exceptionVec` 可能携带 `hardwareError/storeAccessFault`，并且同拍也会产生 `exceptionInfo.valid`。
- 因此 `writeback valid` 只表示“DUT 对 ROB/后端给出了反馈”，不能直接等价为 normal pass。写回 task 必须先检查该反馈是否携带非零 `exceptionVec`、replay、redirect 或被当前 redirect/epoch 杀掉；只有这些条件都不成立时，才允许置 `pass`。

写回事件标准化：

- 每拍先采样所有写回、feedback、exception 和 redirect/replay 来源，不在采样循环中直接改状态表。
- 每个 valid 端口统一转换成 `wb_event_t` 临时事件，至少包含：
  - `source`：`LOAD_WB`、`STORE_WB`、`STA_FEEDBACK`、`STD_FEEDBACK`、
    `MEMORY_VIOLATION`、`BACKEND_REPLAY`、`REDIRECT` 等来源标签；`ATOMIC_WB`
    reserved 给后续 AMO/LR/SC 专项，`SQ_WB/EXCEPTION_INFO` 不再作为独立 source。
  - `port_id`：真实端口编号，用于 debug 和同拍确定性排序。
  - `target`：`LOAD`、`STA`、`STD`、`ATOMIC`、`STORE_QUEUE` 或 `GLOBAL_REDIRECT`。
  - `rob_key`、`has_rob`、`lqIdx`、`has_lqIdx`、`sqIdx`、`has_sqIdx`、`uopIdx`。
  - `exceptionVec`、`has_exception`、`replay_valid`、`redirect_valid`、`real_wb_valid`。
  - 反查得到的 `uid`，以及从发射记录或状态表读出的 target 级 `issue_epoch/replay_seq`。
  - 原始接口快照和发生 cycle。
- `has_exception` 不能只看独立 exception 端口，也必须检查写回 payload 自身的 `exceptionVec` 是否非零。带异常的 writeback 归类为 fault feedback，而不是 normal pass。
- `source` 不单独驱动行为。normal pass 看 `real_wb_valid`，fault 看
  `has_exception/exceptionVec`，replay 看 `replay_valid`，redirect 看
  canonical `redirect.valid`。`redirect_valid` 作为兼容/显式标志必须与
  `redirect.valid` 一致，不一致 fatal；`REDIRECT/MEMORY_VIOLATION` source
  不能单独触发 redirect。例如 `BACKEND_REPLAY` source 必须同时设置
  `replay_valid=1` 才会进入 replay 处理。

`uid` 反查规则：

1. 事件携带完整 `robIdx_flag/value` 时，直接组成 `rob_key` 后通过 `lookup_active_uid_by_rob()` 反查 `uid`。
2. load 类事件只有 `lqIdx` 或需要交叉校验时，通过 `uid_by_lq[lqIdx]` 反查；若同时带 `robIdx`，必须和 `uid_by_active_rob[rob_key]` 的结果一致，否则默认 `uvm_fatal("WB_UID_MISMATCH")`。
3. store/CBO/StoreQueue 类事件只有 `sqIdx` 或需要交叉校验时，通过 `uid_by_sq[sqIdx]` 反查；若同时带 `robIdx`，同样必须交叉一致。
4. `ATOMIC` 不占 LQ/SQ，只允许通过 `robIdx` 反查 `uid`；如果 atomic 写回没有可用 `robIdx`，默认报错。
5. STA/STD feedback 优先使用反馈 payload 中的 `robIdx` 反查；如果接口只暴露 SQ 相关信息，则退化为 `uid_by_sq[sqIdx]`，但必须确认该 `uid` 当前目标入口与 feedback 类型匹配。
6. 反查失败时默认报错；只有事件能明确证明属于已 retire、已 flush 或旧 `issue_epoch` 的迟到反馈，且不会更新任何当前状态时，才允许记录 debug 后忽略。

同拍多事件处理顺序：

1. 每拍先完成采样和 `uid` 反查，形成 `wb_event_q_this_cycle`。
2. 对每个事件做条件检查：`active=1`、`issue_killed=0`、target 级 `issue_epoch` 和 `replay_seq` 与状态表当前值匹配；不匹配的事件只记录为旧反馈，不更新当前状态。
3. 将同一 `uid`、同一 `target`、同一 `replay_seq` 的事件合并成一个 cycle-local 结果。合并优先级为：`redirect/flush` 高于 `replay`，`replay` 高于 `exception/fault`，`exception/fault` 高于 normal writeback/pass。
4. 同一 `uid` 同拍若既观察到 `real_wb_valid`，又观察到该写回携带非零 `exceptionVec`、replay feedback 或独立异常事件，则该写回不能置 normal pass；只能记录原始 writeback 事实，并按 exception/replay 事件更新状态或写入 `exception_event_q`。
5. 不同 `uid` 的正常写回可以同拍全部逐项更新。处理顺序固定为事件优先级、ROB 环形 oldest、`source`、`port_id`，保证回归可复现；但不同 `uid` 的状态更新不应互相覆盖。
6. 同一 `uid` 的不同目标入口可以同拍分别更新，例如同一 store 的 STA feedback 和 STD data 完成分别更新 `sta_pass/std_pass`；只有行为表要求的所有目标入口均完成且无 fault/replay/redirect 时，才置总 `pass`。
7. 初版标量模式下，同一 `uid + target + replay_seq` 同拍出现多个 normal pass 事件视为重复反馈，默认 `uvm_fatal("DUP_WB_EVENT")`。后续开启 vector LS 时，需要改为按 `uopIdx/elemIdx` 计数，全部分片完成后再置目标 pass。

异常处理：

- 异常、redirect、replay 不在写回 task 中直接完成恢复，而是交给专门的 `exception_redirect_replay_task` 处理。
- 写回 task 只负责采样异常事实，例如异常写回、`memoryViolation`、replay 请求、rollback redirect，并把对应 `uid`、`target`、`issue_epoch`、`replay_seq`、异常类型和原始接口信息写入 `exception_event_q` 或通过条件更新 API 更新状态；如果 DUT 接口只返回 `robIdx/lqIdx/sqIdx`，必须先通过 active 映射反查 `uid`。
- `exception_redirect_replay_task` 统一决定是 replay、redirect/flush 还是 trap 类 fault，并同步发射队列、commit task 和 `lsq_ctrl`，避免同一条 transaction 被错误重复发射或过早提交。

正常写回处理：

- 若写回事件经过同拍合并后确认为 normal pass，写回 task 必须通过条件更新 API 设置状态表中对应条目的 `writeback/pass` 标识。条件必须匹配 `uid + target + issue_epoch + replay_seq`，且该条目仍为 `active`、`issue_killed=0`。
- 写回 task 不直接驱动 LSQ commit 端口，避免把“写回完成”和“ROB 提交/LSQ 释放”混成一个事件。
- 写回 task 需要把可提交事实通过条件更新写入状态表，例如 load 已正常写回、store 地址/数据已完成、异常已清空，供专门的 LSQ commit 驱动 task 按 ROB 顺序消费。
- redirect 或 replay 发生时，写回 task 需要通过条件更新或异常事件队列设置 `replay_pending/redirect_pending`，阻止 commit task 继续推进对应 `robIdx`。如果反馈携带的 `issue_epoch/replay_seq` 与状态表当前值不匹配，则只能记录为迟到反馈，不得更新当前状态。
- 当主表同时注入翻译/访问异常字段和 DCache/TL 返回路径字段时，写回 task 不按 TB 预设优先级合成结果；它只记录主表注入字段和 DUT 实际观测到的 `exceptionVec/replay/redirect/pass`，由后续检查或覆盖率逻辑分析组合场景。

### 6.8 Task6：异常、redirect 与 replay 处理 task

需要新增专门 task，建议命名为 `exception_redirect_replay_task`，在 `memblock_base_sequence.sv` 中实现。该 task 负责处理 MemBlock 写回异常、memory violation rollback、backend/RS replay 和 redirect/flush 后状态恢复，不应只在写回 task 中简单置位。这里的 replay 需要先分类：store RS replay 是局部重发，不需要 flush；memory violation、nuke、nack rollback 和 ROB replayInst 才进入 redirect/flush 流程。

源码依据：

- `MemBlock.scala` 将 `newLoadUnits(i).io.rollback`、`lsq.io.nack_rollback`、`lsq.io.nuke_rollback` 汇总为 `allRedirect`，通过 `Redirect.selectOldestRedirect()` 选择最老 redirect，并输出到 `mem_to_ooo.memoryViolation`。
- `MemBlock.scala` 对 memory replay 生成的 redirect 清掉 `backendIAF/backendIPF/backendIGPF`，说明 memory violation/replay 类 redirect 不应被当作取指异常。
- `ExceptionInfoGen.scala` 对异常请求先过滤 `robIdx.needFlush(io.redirect)`，再选择最老异常；已有 current exception 如果被后续 redirect flush 且没有新异常，会清空。
- `LoadQueueRAW.scala` 在检测到 store-load violation 时生成 `RedirectLevel.flush`，redirect payload 使用被 rollback 的 load `robIdx/ftqIdx/ftqOffset/pc`，并输出 `mdpTrain`。
- `LoadQueueUncache.scala` 在 uncache buffer 满或 nack 类场景生成 rollback，并过滤当前、上一拍、上上拍已经被 flush 的 `robIdx`。
- `LoadQueueReplay.scala` 对 replay queue 中 `robIdx.needFlush(io.redirect)` 的条目直接释放；正常 replay 请求带 `cause`、`replayQueueIdx`、`mshrId`、`uncacheReplay/ncReplay` 等信息重新送回 load pipeline。
- `NewStoreUnit.scala` 中 store 地址侧 TLB miss 通过 `needRSReplay = feedBackValid && !feedBackHit` 产生 STA feedback，`hit=0` 表示 IssueQueue 需要重新发射该 STA。
- `Region.scala` 将 STA feedback 转成 `s2Resp.failed/finalSuccess`，IssueQueue entry 收到 `failed` 后清 `issued`，属于局部 RS replay，不产生 MemBlock redirect。
- `Rob.scala` 中真正的后端 `replayInst` 会通过 `flushOut` 产生 `RedirectLevel.flush`；但 `FuConfig.scala` 里 `StaCfg/LduCfg/VstuCfg` 当前 `replayInst=false`，store TLB replay 不走 ROB flushOut。
- LQ/SQ、replay queue、store queue 均通过 `robIdx.needFlush(io.redirect)` 和 cancel count 完成清理，`lsq_ctrl` 需要用 DUT 返回的 `lqCancelCnt/sqCancelCnt` 修正本地资源。

输入来源：

- 写回监测 task 记录的异常写回事实：`robIdx`、`lqIdx/sqIdx`、exceptionVec、vaddr、gpaddr、是否 store、是否 vector LS。
- MemBlock 输出：`memoryViolation`、`mdpTrain`、`lqCancelCnt/sqCancelCnt`、`lqDeq/sqDeq`。
- replay 相关 monitor：STA/STD feedback `failed/finalSuccess`、load replay valid/fire、replay cause、replayQueueIdx、是否 uncache/nc replay。
- 状态表：按 `uid` 读取 `active/dispatch/writeback/fault/pass/replay_pending/redirect_pending/rob_commit/lsq_deq`。
- 主表和队列：通过 `uid` 找回原始 transaction、发射队列残留项和 TLB 表项；只有原始 DUT 事件只携带 `robIdx/lqIdx/sqIdx` 时，才先通过 `uid_by_active_rob/uid_by_lq/uid_by_sq` 反查当前 active `uid`。

核心流程：

1. 每拍先采样异常和 redirect/replay 来源，统一写入 `exception_event_q`，事件至少包含 `event_type`、`uid`、`target`、`issue_epoch`、`replay_seq`、`robIdx`、`uopIdx`、`lqIdx/sqIdx`、exceptionVec、redirect payload、replay cause 和发生 cycle。
2. 对同拍多个事件按源码策略仲裁：
   - 异常写回事件按 `ExceptionInfoGen` 口径选择最老 `robIdx/uopIdx`。
   - rollback redirect 事件按 `Redirect.selectOldestRedirect()` 口径选择最老 redirect。
   - 已经满足 `robIdx.needFlush(active_redirect)` 的事件丢弃，不再二次处理。
3. 对 trap/fault 类异常：
   - 先通过条件更新校验 `uid + target + issue_epoch + replay_seq` 仍匹配当前状态；匹配后将状态表中的对应 `fault/exception_pending` 置高，保存 `exceptionVec/vaddr/gpaddr`。不匹配的异常反馈只能记录为迟到事件。
   - 阻止该 `robIdx` 及 younger 条目继续 commit。
   - 由 task 按测试框架的后端模型生成或等待 backend redirect；redirect 生效后进入 flush 恢复流程。
4. 对 memory violation / nuke / nack rollback：
   - 使用 `memoryViolation.bits.robIdx` 作为 flush 边界，设置 `redirect_pending=1`。
   - 以下清队列和状态恢复动作必须在 `exception_redirect_replay_task` 发出 freeze request 后、下一拍采样到 `issue_freeze_ack=1` 才能执行；ack 未到时只能等待，不能提前清队列或覆盖状态。
   - 对所有通过 `rob_is_after(uop_rob, redirect.robIdx)` 判断为 younger 的条目，以及通过 `rob_need_flush(uop_rob, redirect)` 覆盖到的 flushItself 条目，清理发射队列残留、取消 `dispatch/writeback/pass/rob_commit` 等不可再沿用状态。
   - 对被 rollback 的 load/store 按场景设置 `replay_pending`：若 LSQ entry 未被 cancel 且只是局部 replay，才由队列路由 task 回到 load/STA/STD 队列；若已被 redirect flush cancel，则按 flush 路径 retire，不在当前简化路径重入队。
   - 捕获并记录 `mdpTrain` 信息，供第二类 MDP 字段或覆盖率检查使用。
5. 对 replay-only 事件：
   - store 地址侧 replay：设置 `replay_pending=1`，清除 `sta_dispatch/sta_pass` 等 STA 侧完成状态；设置 `replay_target_mask.STA=1`、`replay_seq++`，由队列路由 task 重新放入 STA 队列。
   - store 地址侧 replay 不设置 `flush_in_progress`，不清 `std_dispatch/std_pass`，不释放 `sqIdx/lqIdx`，不阻塞其他无关 `robIdx` 发射。
   - load 的非 redirect replay 如果由 TB 管理，则只设置 `replay_target_mask.LOAD=1` 并回 load 队列；MemBlock 内部 `LoadQueueReplay` 默认只记录，不额外从 TB 发射。
   - replay 期间 commit task 必须跳过或阻塞该 `robIdx`，直到 replay 后对应入口重新完成并重新置 `pass`。
6. redirect/flush 生效后，task 需要执行恢复：
   - 从 load/STA/STD 发射队列删除被 flush 的 younger 项。
   - 对状态表中被 flush 的项清除 `dispatch/writeback/pass/rob_commit`，设置 `flushed`。
   - 对仍需重发的项先判断 LSQ entry 是否仍有效：仍有效时设置 `replay_pending/replay_target_mask`，通知队列路由 task 放入对应队列；已被 cancel 时按 flush 路径 retire，不在当前简化路径重入队。
   - 等待并采样 DUT 的 `lqCancelCnt/sqCancelCnt`，调用 `lsq_ctrl` 修正 LQ/SQ 本地 free count 和指针镜像。
   - 对 TLB 表，保留主表仍有效且地址映射未变的项；如果 CSR/虚拟化状态或异常场景要求重新翻译，则清除对应 `{vpn, asid, vmid, s2xlate}` 到 `uid` 的映射并重新触发 Task2。
7. task 与发射/commit 的同步规则：
   - 一旦 `redirect_pending` 或 `replay_pending` 置位，Task4 不得继续发射该 `robIdx` 的旧 transaction。
   - `lsq_commit_drive_task` 不得提交 `fault/replay_pending/redirect_pending` 为 1 的条目。
   - flush 恢复完成后，必须通过统一状态 API 清除 pending 位，避免子 task 之间整条状态覆盖。
8. 结束条件：异常事件被处理、必要 redirect 已被 DUT 接收、`lqCancelCnt/sqCancelCnt` 已反映到 `lsq_ctrl`，且队列/状态表/TLB 表完成一致性检查。

伪代码示例：

```systemverilog
task exception_redirect_replay_task();
  forever begin
    collect_exception_and_redirect_events();
    select_oldest_exception_like_exception_info_gen();
    select_oldest_redirect_like_memblock();

    if (has_fault_event()) begin
      mark_fault_and_block_commit();
      request_or_wait_backend_redirect();
    end

    if (has_replay_only_event()) begin
      mark_replay_pending_with_target_mask();
      clear_only_failed_issue_side();
      notify_issue_queue_route_task();
    end

    if (has_memory_violation_redirect()) begin
      request_issue_freeze(); // set flush_in_progress/active_redirect only
      wait_next_cycle_until_issue_freeze_ack();
      apply_redirect_flush_boundary();
      cleanup_younger_issue_queues();
      mark_replay_pending_or_flush();
    end

    sample_lsq_cancel_count_and_recover_lsq_ctrl();
    clear_pending_after_replay_or_flush_done();
    wait_next_cycle();
  end
endtask
```

该 task 与其他 task 的边界：写回 task 负责发现事件；本 task 负责分类、flush/replay 状态恢复和队列同步；LSQ commit task 只消费恢复后的可提交状态。

### 6.9 Task7：LSQ commit 驱动 task

需要新增专门 task，建议命名为 `lsq_commit_drive_task`，在 `memblock_base_sequence.sv` 中实现。该 task 负责模拟源码中 ROB 到 MemBlock 的 LSQ commit 输入，不应混在写回监测 task 内部。

源码依据：

- `Rob.scala` 中 ROB 对 LSQ 的提交输入为 `lcommit/scommit/commit/pendingPtr/pendingPtrNext`。
- `lcommit = RegNext(PopCount(ldCommitVec))`，其中 `ldCommitVec` 为本拍 ROB commit window 内 `commitType=LOAD` 的条目。
- `scommit = RegNext(PopCount(stCommitVec))`，其中 `stCommitVec` 为本拍 ROB commit window 内 `commitType=STORE && !vls` 的标量 store 条目。
- `commit = RegNext(io.commits.isCommit && io.commits.commitValid(0))`。
- `pendingPtr = RegNext(deqPtr)`，`pendingPtrNext = RegNext(deqPtrVec_next.head)`。
- `MemBlock.scala` 将这些信号接到 `lsq.io.rob.*`；`LSQWrapper.scala` 将 `pendingPtr` 送给 store queue，将 `lcommit/scommit/commit` 送给 load queue 相关逻辑。

注意区分两类信号：

- 输入提交信号：测试框架驱动 `ooo_to_mem.lsqio.lcommit/scommit/commit/pendingPtr/pendingPtrNext`，语义来自 ROB commit。
- 实际释放反馈：MemBlock 输出 `lqDeq/sqDeq/lqDeqPtr/sqDeqPtr`，语义来自 LQ/SQ 内部真实 deq。后续空闲资源、清理和 dispatch 反馈应优先以这些输出为准。

输入数据来源：

- `common_data_transaction.sv` 主表：按 `commit_uid_ptr` 或 ROB 顺序候选 `uid` 读取 `robIdx`、`fuType/fuOpType/op_class`、是否 load/store、是否 vector LS、`lqIdx/sqIdx`。
- 状态表：按 `uid` 读取 `active/enq/dispatch/writeback/pass/fault/exception_pending/flushed/replay_pending/redirect_pending/sta_dispatch/std_dispatch` 等状态。
- `seq_csr_common.sv`：读取 commit 延迟、每拍最大 commit 数等 plus 配置。
- DUT monitor：读取 `lqDeq/sqDeq/lqDeqPtr/sqDeqPtr`、redirect/flush、store exception、MMIO/NC/CBO 完成等真实反馈。

核心流程：

1. task 内部维护一个软件 `commit_uid_ptr`，按 `uid` 顺序扫描主表，最多形成 `CommitWidth` 个连续 commit candidate。`commit_uid_ptr` 只用于 TB 主表扫描，不允许直接作为 DUT commit pointer。
2. task 内部另维护 `rob_key_t commit_rob_ptr` 作为 ROB commit 指针镜像。真正驱动 DUT 的 `pendingPtr/pendingPtrNext` 必须由 `commit_rob_ptr` 和候选主表中的 `robIdx_flag/value` 推导，并遵循 DUT ROB 环形语义。
3. 每个 candidate 必须满足：已入队或完成 non-LSQ admission、未 fault、未 replay/redirect pending、没有被 flush；load/prefetch 需要已经正常写回或被模型判定可提交；普通 cacheable store 需要 STA/STD 完成且已有 store 写回/pass 事实；NC store 可以在 ROB entry 写回/pass 后提交，不必等待 NC ack/deq；MMIO store 和 CBO 需要等待 StoreQueue `writeBack` 形成 ROB 可提交事实后才能提交；`ATOMIC` 需要 `AtomicsUnit` 写回/pass 完成。
4. 对候选集合计算本拍 ROB commit 结果：
   - `ldCommitVec[i] = commitValid[i] && commitType == LOAD`。
   - `stCommitVec[i] = commitValid[i] && commitType == STORE && !isVls`。
   - `ATOMIC` 的 `commitType=NORMAL`，不计入 `ldCommitVec/stCommitVec`，但仍会影响 `commit/pendingPtr` 的 ROB 顺序推进。
   - `lcommit_next = PopCount(ldCommitVec)`。
   - `scommit_next = PopCount(stCommitVec)`。
   - `commit_next = commitValid[0]`。
5. 按源码 `RegNext` 语义驱动端口：本拍计算出的 `lcommit_next/scommit_next/commit_next/pendingPtr_next/pendingPtrNext_next` 在下一拍驱动到 DUT。实现上可以用 task 内部寄存变量保存上一拍计算结果，再在每拍开始驱动 DUT。
6. `pendingPtr` 表示本拍提交边界，store queue 通过 `isNotAfter(uop.robIdx, pendingPtr)` 判断 SQ entry 是否可置 committed；因此该指针必须随 ROB commit pointer 单调推进，不能直接用最后一个写回条目的 `robIdx`。
   注意这里的 StoreQueue 内部 `committed` 位不是 ROB `scommit` 本身。它可以使已到提交边界的 SQ entry 启动 uncache/CBO/deq 路径；ROB `scommit` 仍来自 ROB commit window 中 `commitType=STORE` 的条目。
7. `pendingPtr/pendingPtrNext` 必须使用与 DUT 一致的 ROB 指针宽度、`flag/value` 和 wrap 规则。实现中禁止写 `pending_ptr++` 或直接比较裸 `robIdx_value` 大小；推进 commit pointer 只能调用 `rob_advance(commit_rob_ptr, commit_count)`，顺序和 flush 判断只能调用 `rob_is_after()` 或 `rob_need_flush()`。
8. `pendingPtr_next` 可取本拍 `commit_rob_ptr`，`pendingPtrNext_next` 可由 `rob_advance(commit_rob_ptr, commit_count)` 得到；其中 `commit_count` 是本拍实际 ROB commit window 推进的条目数，不是 `lcommit+scommit`，因为 `ATOMIC/NORMAL` 等非 LSQ 条目也会推进 ROB commit 边界。
9. `rob_key_t` 转成 DUT 端口时必须按接口字段宽度拆成 `flag/value`，宽度由 DUT/agent interface 定义；禁止用 `int unsigned` 临时值直接赋给 `pendingPtr/pendingPtrNext`。
10. 对 scalar store，`scommit` 只统计 `commitType=STORE && !vls` 的 ROB commit 条目；初版生成和手动导入均不得让 vector LS 进入 commit 流程，避免误计入 scalar `scommit`。
11. 端口驱动后，task 需要监测 MemBlock 输出：
   - `lqDeq` 表示 LQ 实际释放的 entry 数量。
   - `sqDeq` 表示 SQ 实际释放到 sbuffer/uncache/deq 路径的 entry 数量。
   - `lqDeqPtr/sqDeqPtr` 用于校验释放顺序和更新本地 `lsq_ctrl` free count。
12. 状态更新需要拆成两层：
   - `rob_commit` 或等价字段：表示该 `robIdx` 已被 commit task 作为 ROB 提交项驱动。
   - `lsq_deq` 或资源释放字段：表示观察到 `lqDeq/sqDeq` 后 LSQ entry 已实际释放。
   如果状态表暂时只有 `commit` 字段，推荐将 `commit` 定义为“ROB commit 已驱动且对应 LSQ deq 已观察完成”，避免过早清理。
13. 只有在对应 `lqDeq/sqDeq` 被观察到后，才能通知 `lsq_ctrl` 回收 LQ/SQ 资源，并调用 `retire_active_uid(uid)` 清理 active rob/lq/sq 映射。主表和状态表历史保留到 `end_test_check()` 完成，不在 commit 时删除。
14. commit/deq 后的清理边界必须分层：
   - 可清理：`uid_by_active_rob`、`uid_by_lq`、`uid_by_sq`、LSQ free count/pointer 镜像、已经完成使命的发射队列项。
   - 默认保留：`main_table_by_uid[uid]`、`status_by_uid[uid]`、`tlb_table_by_uid[uid]`，直到 `end_test_check()` 完成。
   - 可按策略重建：`tlb_uid_by_key[]` lookup 索引。CSR/SFENCE、redirect/flush 或显式重建配置可以清理索引，但不得删除 per-uid TLB 历史表项。
15. redirect/flush 发生时，commit task 必须停止推进受影响 `robIdx`，等待异常处理 task 完成状态恢复；恢复后再从新的 commit pointer 继续。

伪代码示例：

```systemverilog
task lsq_commit_drive_task();
  memblock_uid_t commit_uid_ptr;
  rob_key_t commit_rob_ptr;
  lsq_commit_drive_t drive_q;

  forever begin
    drive_lsq_commit_ports(drive_q); // drive previous-cycle result, matching RegNext in Rob.scala

    collect_commit_candidates(commit_uid_ptr, commit_rob_ptr, CommitWidth);
    calc_lcommit_scommit_by_commit_type();
    calc_pending_ptr_and_next_with_rob_advance();
    drive_q = build_next_cycle_lsq_commit_drive();

    monitor_lsq_deq_outputs();
    update_status_and_lsq_free_count_after_deq();

    wait_next_cycle();
  end
endtask
```

该 task 与写回 task 的边界：写回 task 只负责发现 `writeback/pass/fault/replay`；commit task 只负责按 ROB 顺序驱动 LSQ commit 端口并观察 LSQ deq。两者通过 `common_data_transaction.sv` 状态表解耦。

## 7. 发射队列补充要求

- 所有发射队列相关 task，包括入队、TLB 映射、队列路由、发射控制、异常 redirect/replay 处理、LSQ commit 驱动和提交清理，都必须读取或更新 `common_data_transaction.sv` 中的数据。
- 所有公共数据必须存储在 `common_data_transaction.sv` 中；连续 `uid` 索引的数据使用动态数组，稀疏或环形复用 key 使用关联数组。
- load、STA、STD 不再是子表，而是三个队列，统一存储在 `common_data_transaction.sv` 中。
- 三个发射队列只保存待发射项；条目成功发射后可从队列删除，追溯记录由主控制表和主任务状态表承担。
- commit/deq 或 flush/cancel 后只清理 active 映射、LSQ 资源镜像和发射队列残留；主控制表、状态表和按 `uid` 追溯的 TLB 表保留到 testcase 结束。TLB lookup 索引 `tlb_uid_by_key[]` 可以按 CSR/SFENCE、redirect/flush 或显式重建策略清理，但不能作为删除 `tlb_table_by_uid[]` 历史项的理由。
- 所有 task 共享同一套单例数据源，避免多环境访问时出现数据不一致。
- 所有参数都必须放在 `seq_csr_common.sv` 中，并通过 `plus.sv` 统一管理。
- 所有参数，例如入队宽度、每拍入队数量、`load_pip_num`、`sta_pip_num`、`std_pip_num`、TLB 权重参数、异常注入权重、提交延迟、`global_send_pri_en`、`send_pri/send_pri_std` 权重、`MEMBLOCK_LD_TO_ST_ADDR_REUSE_WT`、`MEMBLOCK_ST_TO_LD_ADDR_REUSE_WT`、`MEMBLOCK_MANUAL_ADDR_REUSE_EN` 等，都需要支持 plusargs 配置。
- `global_send_pri_en=1` 时，发射控制必须在 STA、STD、load 三个队列的 eligible 元素全集中选择当前最大实际发射优先级；store 的 STA 队列项使用主表 `send_pri`，STD 队列项使用主表 `send_pri_std`。最大值分布在不同队列时允许同拍并发发送，同一队列中最大值数量超过流水线数量时调用 `rob_order_util` 按 ROB 环形顺序截取，`uid` 只作为 debug tie-break。
- `global_send_pri_en=0` 时，STA、STD、load 三个队列完全并行调度，分别受 `load_pip_num`、`sta_pip_num`、`std_pip_num`、实际流水线数量、ready 和队列 eligible 数量限制。
- 所有会引起 transaction 状态变化的 task 都必须通过 `common_data_transaction.sv` 提供的状态更新 API 按 `uid` 在真实事件发生点更新对应字段；只有 DUT monitor 事件只携带 `robIdx` 时，才先通过活跃映射解析为 `uid`，禁止主任务集中代更所有状态，也禁止整条状态表项覆盖式写回。
- task 联动关系必须清晰，默认流程为“`memblock_base_sequence.pre_body()` 调用 `seq_csr_common::init()` -> `reset_all_tables(main_trans_num)` -> 主表准备（随机生成或手动导入） -> 状态表初始化 -> 入队 -> TLB 映射 -> 队列路由 -> 发射 -> 写回监测 -> 异常处理/状态恢复 -> LSQ commit 驱动 -> LSQ deq 监测/清理 -> `end_test_check()`”。
- 所有标识字段，例如 `enq`、`dispatch`、`writeback`、`fault`、`exception_pending`、`flushed`、`pass`、`commit`、`rob_commit`、`lsq_deq`、`success`、`access` 等，需要同步存入或映射到 `common_data_transaction.sv` 的状态表，便于调试、监测和追溯。

文件集成、package 归属、filelist 编译顺序和 include/import 细节不在本设计方案中展开，作为实施阶段事项按现有 memblock sequence/filelist 规则补充。

## 8. 整体要求

1. 主表生成与发射队列相关 task 必须协同，发射队列的所有操作都基于主表数据，确保数据流转准确。
2. 主表生成 task 必须支持两种模式：随机生成主表，以及直接导入手动配置的主表关联数组；手动导入模式不调用随机生成，但必须执行统一合法性校验和派生字段补齐。
3. 所有表的生成和更新逻辑需要关联同步，主表约束规则需要同步作用于 TLB 表、状态表和三个发射队列。
4. 所有表均存入 `common_data_transaction.sv`；主表、状态表和按条目追溯的 TLB 表以 TB 内部连续 `uid/alloc_seq` 作为权威 key，并优先使用动态数组组织。
5. load、STA、STD 不再生成子表，改为维护三个发射队列。
6. 三个发射队列只承担待发射缓冲职责，发射成功后删除对应队列项；后续追溯记录通过主控制表和主任务状态表完成。
7. commit/deq 完成后释放 active 映射和 LSQ 资源，但不删除主控制表、状态表和 `tlb_table_by_uid[]` 历史记录；这些追溯数据统一保留到 `end_test_check()` 完成。`tlb_uid_by_key[]` 仅是 lookup 索引，可按 CSR/SFENCE、redirect/flush 或重建策略清理。
8. 状态表不再由主任务内部局部维护，而是作为单独状态表存入 `common_data_transaction.sv`；主任务负责状态生命周期、初始化、清理和一致性检查，具体状态字段由直接造成状态变化的子 task 按 `uid` 更新。
9. 所有参数均放入 `seq_csr_common.sv`，并通过 `plus.sv` 统一解析和管理。
10. 所有表生成器由两部分组成：transaction 文件管理字段和约束，`memblock_base_sequence.sv` 中的 task 作为生成、维护和写入公共数据的调度入口；复杂可复用算法允许调用 helper class 或 `common_data_transaction.sv` API 实现。
11. 主控制表新增 `send_pri` 和 `send_pri_std` 字段，范围均为 0 到 100，并由 `seq_csr_common.sv` 中的 `global_send_pri_en` 控制是否参与发射优先级选择。store 拆分进入队列时，STA 使用 `send_pri`，STD 使用 `send_pri_std`。
12. `global_send_pri_en=1` 时，发射调度以 STA、STD、load 三个队列的全局最大实际发射优先级为准；相同最大优先级可以跨队列并发发送，同队列超出流水线容量时调用 `rob_order_util` 按 ROB 环形顺序截取，`uid` 只作为 debug tie-break。
13. `global_send_pri_en=0` 时，STA、STD、load 三个队列完全并行，分别由 `load_pip_num`、`sta_pip_num`、`std_pip_num` 控制每拍最多进入对应流水线的数量。
13. 状态表统一存放在 `common_data_transaction.sv`，并通过统一状态更新 API 做字段级更新；如后续发现并发写字段存在工具或实现风险，可退化为各状态字段独立数组或关联数组管理。主表生成或手动导入之前必须调用 `reset_all_tables(main_trans_num)`，结束时必须调用 `end_test_check()`，防止旧表项、旧队列或旧状态污染当前激励。
14. 优先通过约束机制和 plus 权重实现字段自动生成、关联和配置，减少手动干预。主表地址复用注入 task 的 load->store 和 store->load 频率必须通过 plus 参数控制；手动主表默认不执行地址复用注入，必须通过 `+MEMBLOCK_MANUAL_ADDR_REUSE_EN` 显式开启。
15. 严格按三类字段组织主表和发往 agent 的 transaction：
    - 第一类全局控制字段进入主表并严格约束。
    - 第二类流水线发射/相关性字段不在主表阶段独立随机，而是在发射前由专门函数派生赋值。
    - 第三类后端写回/前端调试元信息字段不在主表阶段独立随机，而是在发射前由专门函数派生赋值。
16. TLB 地址映射逻辑必须贴合 TLB 接口逻辑，配置规则和字段关联需符合虚拟化场景要求；所有 CSR/虚拟化实时状态必须从 `csr_agent monitor` 维护的 `mmu_csr_runtime_state` 获取，不能从 `seq_csr_common.sv` 的可变配置读取，也不能在运行阶段直接把 `csr_csr_common.sv` 的初始值当作当前真值。
17. PPN 约束入口需要能接收外部地址约束，并受 `seq_csr_common.sv` 的地址范围参数控制。
18. 单例模式需要保证共享安全性和访问效率，避免多环境访问冲突。
19. 队列路由需要严格按 `fuType/op_class` 执行，字段新增和发送 ID 约束需要符合 DUT 接口要求。
