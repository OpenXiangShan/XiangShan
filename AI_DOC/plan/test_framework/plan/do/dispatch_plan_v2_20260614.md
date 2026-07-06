# 可参数化的测试激励框架设计计划

## 1. 背景与目标

为 MemBlock UT 环境设计一套可参数化的测试激励框架。该框架用于生成并发送 load、store、AMO、prefetch 等访存请求，所有测试用例均基于该框架生成。

生成的测试用例必须符合 MemBlock DUT 的合法行为，尤其需要保证：

- 各信号字段赋值合法。
- 字段之间的依赖关系满足 DUT 约束。
- 入队、TLB 映射、队列路由、发射、写回、异常处理和提交的时序符合 DUT 行为。
- 所有公共表和公共队列最终统一存入 `common_data_transaction.sv`，并通过单例模式共享给其他环境。

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

- 所有“表”必须在 `common_data_transaction.sv` 中以关联数组形式存放。
- load、STA、STD 不再维护为三张子表，而是维护为三个发射队列；队列同样存放在 `common_data_transaction.sv` 中。
- 队列元素建议保存轻量调度快照，而不是完整 transaction。最小推荐字段为 `robIdx`、`send_pri`、`ready_cycle` 或 `delay_left`；可选缓存 `lqIdx/sqIdx/numLsElem` 等高频字段。完整 transaction 内容仍通过 `robIdx` 回查主控制表、状态表和 TLB 表。
- 三个发射队列只作为待发射缓冲。队列项成功发射到对应流水线后即可从队列中删除，不再依赖队列做历史追溯。
- 发射后的追溯、debug、replay、redirect 和 commit 判断统一通过主控制表和主任务状态表完成。
- 如需提升队列删除、查重、replay 定位效率，可以额外维护 `robIdx -> queue_index/status` 的关联数组。

存储方式需要以性能为优先目标，重点评估：

- 读写速度。
- 内存占用。
- 按 `robIdx`、`lqIdx`、`sqIdx`、`vaddr/VPN` 等索引访问的效率。
- 主表、TLB 表、状态表和三个发射队列之间的同步维护成本。
- 队列调度扫描成本，尤其是 `global_send_pri_en=1` 时跨 load/STA/STD 三个队列查找全局最大 `send_pri` 的成本。

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

`common_data_transaction.sv` 是全框架唯一公共数据单例，负责保存所有表、队列和状态。

必须存放在该文件中的数据包括：

- 主控制表：关联数组，例如 `main_table_by_rob[robIdx]`。
- TLB 表：关联数组，例如 `tlb_table_by_vpn[vpn]`，必要时增加 `tlb_table_by_rob[robIdx]`。
- 状态表：关联数组，例如 `status_table_by_rob[robIdx]`。
- load 发射队列：例如 `load_issue_q[$]`。
- STA 发射队列：例如 `sta_issue_q[$]`。
- STD 发射队列：例如 `std_issue_q[$]`。
- 可选辅助索引：例如 `lq_to_rob[lqIdx]`、`sq_to_rob[sqIdx]`、`queue_pos_by_rob[robIdx]`。

表使用关联数组的原因：

- 按 `robIdx`、VPN、`lqIdx`、`sqIdx` 查询更直接。
- replay、redirect、异常和 commit 时可以快速定位条目。
- 与状态表和队列之间通过 key 关联，避免复制大 transaction 数据。
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
- load 地址复用到 store 的权重参数。
- store 地址复用到后续 load 的权重参数。
- 地址范围约束参数。
- TLB/PTE 权限位、N 位、V 位等随机权重参数。

注意：`seq_csr_common.sv` 不保存 CSR/虚拟化实时状态，不作为 TLB 映射 task 的 CSR 信息来源。TLB 映射需要的 CSR、ASID、VMID、虚拟化模式和 S1/S2 状态必须从 `csr_csr_common.sv` 接口实时采样。

`seq_csr_common.sv` 只保存解析后的最终配置值。plusargs 的解析和默认值管理通过 `plus.sv` 完成。

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
    static int unsigned real_lsq_enq_max = 8;
    // Compatibility alias. Must equal real_lsq_enq_max.
    static int unsigned real_enq_width = 8;
    static int unsigned load_pip_num = 3;
    static int unsigned sta_pip_num = 2;
    static int unsigned std_pip_num = 2;

    static bit global_send_pri_en = 1'b0;
    static int unsigned send_pri_default = 50;
    static int unsigned send_pri_low_wt = 1;
    static int unsigned send_pri_mid_wt = 8;
    static int unsigned send_pri_high_wt = 1;

    // 0..100; 0 disables address reuse injection.
    static int unsigned ld_to_st_addr_reuse_wt = 0;
    static int unsigned st_to_ld_addr_reuse_wt = 0;

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

    static function void load_from_plusargs();
      void'($value$plusargs("MEMBLOCK_MAIN_TRANS_NUM=%0d", main_trans_num));
      use_manual_main_table = $test$plusargs("MEMBLOCK_USE_MANUAL_MAIN_TABLE");
      void'($value$plusargs("MEMBLOCK_ENQ_PER_CYCLE=%0d", enq_per_cycle));
      void'($value$plusargs("MEMBLOCK_ENQ_PER_CYCLE_RAND_EN=%0d", enq_per_cycle_rand_en));
      void'($value$plusargs("MEMBLOCK_REAL_LSQ_ENQ_MAX=%0d", real_lsq_enq_max));
      void'($value$plusargs("MEMBLOCK_REAL_ENQ_WIDTH=%0d", real_enq_width));
      void'($value$plusargs("MEMBLOCK_LOAD_PIP_NUM=%0d", load_pip_num));
      void'($value$plusargs("MEMBLOCK_STA_PIP_NUM=%0d", sta_pip_num));
      void'($value$plusargs("MEMBLOCK_STD_PIP_NUM=%0d", std_pip_num));

      global_send_pri_en = $test$plusargs("MEMBLOCK_GLOBAL_SEND_PRI_EN");
      void'($value$plusargs("MEMBLOCK_SEND_PRI_DEFAULT=%0d", send_pri_default));
      void'($value$plusargs("MEMBLOCK_SEND_PRI_LOW_WT=%0d", send_pri_low_wt));
      void'($value$plusargs("MEMBLOCK_SEND_PRI_MID_WT=%0d", send_pri_mid_wt));
      void'($value$plusargs("MEMBLOCK_SEND_PRI_HIGH_WT=%0d", send_pri_high_wt));

      void'($value$plusargs("MEMBLOCK_LD_TO_ST_ADDR_REUSE_WT=%0d", ld_to_st_addr_reuse_wt));
      void'($value$plusargs("MEMBLOCK_ST_TO_LD_ADDR_REUSE_WT=%0d", st_to_ld_addr_reuse_wt));

      void'($value$plusargs("MEMBLOCK_DELAY_0_WT=%0d", delay_0_wt));
      void'($value$plusargs("MEMBLOCK_DELAY_1_20_WT=%0d", delay_1_20_wt));
      void'($value$plusargs("MEMBLOCK_DELAY_21_50_WT=%0d", delay_21_50_wt));

      void'($value$plusargs("MEMBLOCK_TLB_PTE_R_1_WT=%0d", tlb_pte_r_1_wt));
      void'($value$plusargs("MEMBLOCK_TLB_PTE_R_0_WT=%0d", tlb_pte_r_0_wt));
      void'($value$plusargs("MEMBLOCK_TLB_PTE_W_1_WT=%0d", tlb_pte_w_1_wt));
      void'($value$plusargs("MEMBLOCK_TLB_PTE_W_0_WT=%0d", tlb_pte_w_0_wt));
      void'($value$plusargs("MEMBLOCK_TLB_PTE_X_1_WT=%0d", tlb_pte_x_1_wt));
      void'($value$plusargs("MEMBLOCK_TLB_PTE_X_0_WT=%0d", tlb_pte_x_0_wt));
      void'($value$plusargs("MEMBLOCK_TLB_PTE_U_1_WT=%0d", tlb_pte_u_1_wt));
      void'($value$plusargs("MEMBLOCK_TLB_PTE_U_0_WT=%0d", tlb_pte_u_0_wt));
      void'($value$plusargs("MEMBLOCK_TLB_PTE_G_1_WT=%0d", tlb_pte_g_1_wt));
      void'($value$plusargs("MEMBLOCK_TLB_PTE_G_0_WT=%0d", tlb_pte_g_0_wt));
      void'($value$plusargs("MEMBLOCK_TLB_PTE_A_1_WT=%0d", tlb_pte_a_1_wt));
      void'($value$plusargs("MEMBLOCK_TLB_PTE_A_0_WT=%0d", tlb_pte_a_0_wt));
      void'($value$plusargs("MEMBLOCK_TLB_PTE_D_1_WT=%0d", tlb_pte_d_1_wt));
      void'($value$plusargs("MEMBLOCK_TLB_PTE_D_0_WT=%0d", tlb_pte_d_0_wt));
      void'($value$plusargs("MEMBLOCK_TLB_PTE_N_1_WT=%0d", tlb_pte_n_1_wt));
      void'($value$plusargs("MEMBLOCK_TLB_PTE_N_0_WT=%0d", tlb_pte_n_0_wt));
      void'($value$plusargs("MEMBLOCK_TLB_PTE_V_1_WT=%0d", tlb_pte_v_1_wt));
      void'($value$plusargs("MEMBLOCK_TLB_PTE_V_0_WT=%0d", tlb_pte_v_0_wt));

      void'($value$plusargs("MEMBLOCK_PADDR_BASE=%h", paddr_base));
      void'($value$plusargs("MEMBLOCK_PADDR_RANGE=%h", paddr_range));
    endfunction
  endclass

endpackage
```

`seq_csr_common.sv` 使用模板：

```systemverilog
`include "plus.sv"
import memblock_plus_pkg::*;

class seq_csr_common;
  static function void init();
    memblock_plus_cfg::load_from_plusargs();
  endfunction

  static function bit get_global_send_pri_en();
    return memblock_plus_cfg::global_send_pri_en;
  endfunction

  static function int unsigned get_send_pri_default();
    return memblock_plus_cfg::send_pri_default;
  endfunction

  static function int unsigned get_ld_to_st_addr_reuse_wt();
    return memblock_plus_cfg::ld_to_st_addr_reuse_wt;
  endfunction

  static function int unsigned get_st_to_ld_addr_reuse_wt();
    return memblock_plus_cfg::st_to_ld_addr_reuse_wt;
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

- 实现主表生成 task。
- 实现 TLB 表生成 task。
- 实现发射队列路由与维护 task。
- 实现发射控制 task。
- 实现第二类字段赋值 task：`assign_issue_dep_fields()`。
- 实现第三类字段赋值 task：`assign_backend_meta_fields()`。
- 实现异常监测、写回处理和 commit task。
- 调用 transaction randomize。
- 将生成结果写入 `common_data_transaction.sv` 单例。

建议文件示例：

- `main_control_transaction.sv`：主控制表字段和约束。
- `tlb_transaction.sv`：TLB 表字段和约束。
- `status_transaction.sv`：状态表字段。
- `common_data_transaction.sv`：所有表、队列和状态的单例容器。
- `seq_csr_common.sv`：解析后的公共配置和 plus 权重，不保存 CSR/虚拟化实时状态。
- `csr_csr_common.sv`：CSR/虚拟化实时状态接口，供 TLB 映射 task 采样 ASID、VMID、S1/S2 和虚拟化状态。
- `plus.sv`：plusargs 模板和默认值。
- `memblock_base_sequence.sv`：所有生成器 task 和主流程 task。

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
- `corrupt`
- `denied`

`send_pri` 是主控制表字段，取值范围为 0 到 100，数值越大表示越优先发射。该字段是否生效由 `seq_csr_common.sv` 中的 `global_send_pri_en` 控制：

- `global_send_pri_en=1`：发射控制 task 选择候选 transaction 时优先考虑 `send_pri`。
- `global_send_pri_en=0`：忽略 `send_pri`，按原有乱序或顺序策略选择。

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
- `robIdx` 按从小到大顺序递增。
- `lqIdx` 和 `sqIdx` 在主表阶段保留字段，入队 fire 后由 Task1 调用 `lsq_ctrl` 最终确认并回填。
- `numLsElem/lsq_flow` 可以在主表阶段按指令模板预先派生，也可以在入队前由 `lsq_ctrl` 根据向量简化策略补齐。
- `vaddr` 由 `src_0 + SignExt(imm[11:0])` 计算得出。
- `fuType` 和 `fuOpType` 必须按合法指令模板成对生成，不能独立随机。
- 主表随机生成完成后，需要根据 `fuOpType/op_class` 调用地址复用注入 task，按 plus 权重把部分 load/store 的 `src_0 + imm` 调整为相同地址，提高 load-store 违例、转发、replay 或 MDP 场景出现概率。
- 地址复用注入 task 的两个频率参数必须通过 `plus.sv -> seq_csr_common.sv` 控制，默认 0 表示不注入。
- `send_pri` 取值范围为 0 到 100，权重由 `seq_csr_common.sv` 中的 plus 配置控制。
- `tlbAF`、`tlbPF`、`tlbGPF`、`PBMT`、`pmaAF`、`delay`、`corrupt`、`denied` 等控制字段需要支持权重约束。
- `post_randomize()` 中完成派生字段填充，例如 `vaddr`，以及必要的合法性修正。
- 生成后的主表条目写入 `common_data_transaction.sv` 中的关联数组。

#### 随机生成模式

随机生成模式是默认模式，由 `MEMBLOCK_USE_MANUAL_MAIN_TABLE` 未开启时使用。

流程：

1. 从 `seq_csr_common.sv` 读取 `main_trans_num` 和各字段权重参数。
2. 创建主表 transaction。
3. 调用 `randomize()`。
4. 在 `post_randomize()` 中生成 `vaddr`、修正派生字段、检查字段合法性。
5. 调用 `inject_ls_addr_reuse_by_fuoptype()`，按 plus 权重对已生成主表做 load/store 地址复用后处理。
6. 后处理完成后重新计算并校验被修改条目的 `vaddr = src_0 + SignExt(imm[11:0])`。
7. 按 `robIdx` 写入 `common_data_transaction.sv` 的主表关联数组。

#### 地址复用注入 task

为了提高 load-store 地址相关、违例、转发、replay 和 MDP 场景出现概率，主表生成 task 需要在随机主表初步生成后增加一个小型后处理 task，建议命名为 `inject_ls_addr_reuse_by_fuoptype()`。

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
  ref main_control_transaction main_table_by_rob[int unsigned]
);
  int unsigned ld_to_st_wt = seq_csr_common::get_ld_to_st_addr_reuse_wt();
  int unsigned st_to_ld_wt = seq_csr_common::get_st_to_ld_addr_reuse_wt();

  foreach (main_table_by_rob[rob]) begin
    main_control_transaction tr = main_table_by_rob[rob];

    if (is_store_fuoptype(tr.fuOpType) && rand_percent_hit(ld_to_st_wt)) begin
      main_control_transaction ld_ref;
      if (select_legal_load_addr_ref(main_table_by_rob, rob, ld_ref)) begin
        tr.src_0 = ld_ref.src_0;
        tr.imm   = ld_ref.imm;
        tr.update_vaddr();
        tr.validate_main_transaction();
      end
    end

    if (is_load_fuoptype(tr.fuOpType) && rand_percent_hit(st_to_ld_wt)) begin
      main_control_transaction st_ref;
      if (select_legal_prior_store_addr_ref(main_table_by_rob, rob, st_ref)) begin
        tr.src_0 = st_ref.src_0;
        tr.imm   = st_ref.imm;
        tr.update_vaddr();
        tr.validate_main_transaction();
      end
    end
  end
endtask
```

其中 `select_legal_prior_store_addr_ref()` 必须只从 `robIdx < 当前 load robIdx` 的 store 候选中选择；`select_legal_load_addr_ref()` 可以按场景选择任意已生成 load，若需要更强时序相关性，也可以限制为 `robIdx < 当前 store robIdx` 的 load 候选。

#### 手动配置关联数组模式

手动配置模式由 plusarg `+MEMBLOCK_USE_MANUAL_MAIN_TABLE` 开启。该模式用于 directed case 或复现指定场景。

手动模式输入：

```systemverilog
main_control_transaction manual_main_table_by_rob[int unsigned];
```

手动模式要求：

- 用户或 testcase 在主表生成 task 执行前完成 `manual_main_table_by_rob` 配置。
- 主表生成 task 不调用 `randomize()` 生成新条目。
- 主表生成 task 直接遍历手动关联数组，并导入到 `common_data_transaction.sv` 的主表关联数组。
- 手动条目仍必须执行统一合法性校验，例如 `validate_main_transaction()`。
- 如果手动条目没有填写派生字段，例如 `vaddr`，允许通过 `post_manual_config()` 或等价函数补齐。
- 如果手动条目已经填写派生字段，则需要校验其与源字段一致，例如 `vaddr == src_0 + SignExt(imm[11:0])`。
- `robIdx` key 必须唯一，且建议按从小到大可遍历；如果关联数组 key 无序，导入后需要建立按 `robIdx` 排序的访问列表。
- `lqIdx/sqIdx` 仍可保留未分配状态，由后续入队 task 在 fire 后调用 `lsq_ctrl` 最终确认并回填。
- 手动模式导入完成后，后续状态初始化、TLB 映射、队列路由、发射控制、异常和提交流程与随机模式完全复用。

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
      common_data_transaction::get().main_table_by_rob[rob] = tr;
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
  send_pri dist {
    [0:30]   :/ memblock_plus_cfg::send_pri_low_wt,
    [31:70]  :/ memblock_plus_cfg::send_pri_mid_wt,
    [71:100] :/ memblock_plus_cfg::send_pri_high_wt
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
| `robIdx_flag/value` | 主表索引、状态表映射、异常、写回和提交关联 | 主表生成阶段递增 |
| `lqIdx_flag/value` | LS transaction 携带的 LQ 位置或 LQ 边界信息；load/vload 消费 LQ 分配，store/vstore 仍需要携带该字段用于接口一致性和年龄窗口判断 | 主表保留字段，入队 fire 时由 `lsq_ctrl` 最终确认并回填 |
| `sqIdx_flag/value` | LS transaction 携带的 SQ 位置或 SQ 边界信息；store/vstore 消费 SQ 分配，load/vload 仍需要携带该字段用于 load-store 依赖窗口判断 | 主表保留字段，入队 fire 时由 `lsq_ctrl` 最终确认并回填 |
| `numLsElem/lsq_flow` | 描述一条 LS transaction 本次入队需要占用的 LQ/SQ entry 数量 | 标量为 1；向量按指令模板、`numLsElem` 和简化 flow 策略派生 |
| `tlbAF`、`tlbPF`、`tlbGPF`、`PBMT`、`pmaAF` | 控制地址翻译、权限、内存属性和异常场景 | CSR/虚拟化实时状态来自 `csr_csr_common.sv`，异常和权限位权重来自 plusargs |
| `delay` | 控制发射节奏 | 主表保留，发射 task 通过 gap transaction 使用 |
| `send_pri` | 控制候选 transaction 的发射优先级 | 范围 0 到 100，由 `global_send_pri_en` 控制是否生效 |
| `corrupt`、`denied` | 控制 DCache 回复和失败路径 | 需要与访问类型、TLB/PMA 场景保持一致 |

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

- integer load、HLV、AMO、LR、SC：`rfWen=1`，`fpWen=0`，`pdest` 为整数物理寄存器。
- FP load：`rfWen=0`，`fpWen=1`，`pdest` 为浮点物理寄存器。
- store、FP store、prefetch、CBO：`rfWen=0`，`fpWen=0`，`pdest` 无实际写回意义。
- `rfWen` 和 `fpWen` 必须互斥。
- `fuOpType=lh/lw` 时，`fpWen` 会影响 LoadUnit 数据格式：
  - `fpWen=0`：整数 `LH/LW`，执行符号扩展。
  - `fpWen=1`：`FLH/FLW`，执行 FP boxing。

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

1. 主表准备阶段：根据 `MEMBLOCK_USE_MANUAL_MAIN_TABLE` 选择随机生成主表，或导入手动配置的主表关联数组；两种模式最终都写入 `common_data_transaction.sv` 的主表关联数组。
2. 主表地址复用后处理阶段：随机生成模式下，根据 `MEMBLOCK_LD_TO_ST_ADDR_REUSE_WT` 和 `MEMBLOCK_ST_TO_LD_ADDR_REUSE_WT` 调用 `inject_ls_addr_reuse_by_fuoptype()`，对部分 load/store 的 `src_0/imm/vaddr` 做合法同地址注入。
3. 状态初始化阶段：主任务在 `common_data_transaction.sv` 的状态表关联数组中为每条 `robIdx` 创建状态条目。
4. LSQ 入队资源管理阶段：入队 task 结合 DUT `canAccept/lqCanAccept/sqCanAccept`、LSQ 空闲资源和本拍配置上限调用 `lsq_ctrl` 计算候选项的 `lqIdx/sqIdx/numLsElem`，只有入队 fire 后才最终回填主表并推进本地指针。
5. 入队和 TLB 阶段：基于已成功入队的主表第一类字段生成 TLB 表，并写入 `common_data_transaction.sv`。
6. 队列路由阶段：将已入队且完成 TLB 映射的条目路由到 load、STA、STD 三个发射队列。
7. 发射前字段赋值阶段：发射 task 先用主表信息生成发往 agent 的 transaction，再调用 `assign_issue_dep_fields()` 和 `assign_backend_meta_fields()`，为该 agent transaction 补齐第二类和第三类字段。

## 6. 发射队列相关 task

### 6.1 主任务

主任务负责调用各子 task 控制完整流程，并负责状态表生命周期管理。

状态表不再作为主任务内部局部表维护，而是作为独立状态表存放在 `common_data_transaction.sv` 中。主任务是该状态表的生命周期 owner，负责创建初始状态、最终清理状态和检查状态一致性；各子 task 是对应状态字段的事件 owner，负责在真实状态转变点更新字段。

状态表以 `robIdx` 作为主键，记录主表条目的状态：

- `enq`：已入队。
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

所有会引起 `robIdx` 状态变化的 task 都必须在真实事件发生点更新状态表中的对应字段，例如入队 task 在入队 fire 后更新 `enq`，发射 task 在成功发送到流水线后更新 `dispatch`，写回监测 task 在捕获写回后更新 `writeback/pass/fault`，`exception_redirect_replay_task` 在确认异常、redirect 或 replay 处理动作后更新 `exception_pending/flushed/replay_pending/redirect_pending`，LSQ commit 驱动 task 在驱动 ROB commit 输入后更新 `rob_commit`，在观察到 `lqDeq/sqDeq` 后更新 `lsq_deq`。

主任务不集中代替子 task 更新所有状态字段，否则容易因为事件汇报滞后或局部信息缺失造成状态不准确。主任务主要负责：

- 初始化每条 `robIdx` 的状态条目。
- 调度和启动各子 task。
- 周期性检查状态合法性，例如禁止重复发射、禁止未写回提交、禁止已 commit 条目重新入队。
- 在测试结束或 flush/redirect 完成后执行最终清理。
- 对状态异常进行报错或停止测试。

状态表并发更新策略：

- 推荐仍使用统一状态表管理，便于通过 `robIdx` 获取完整生命周期状态。
- 不允许多个 task 直接采用“读出整个状态条目 -> 修改局部字段 -> 写回整个状态条目”的方式更新，否则不同 task 在同一仿真时间片更新不同字段时可能互相覆盖。
- `common_data_transaction.sv` 需要提供统一状态更新 API，例如 `set_status_field(robIdx, field, value)` 或 `update_status(robIdx, mask, value)`，所有 task 只能通过该 API 修改状态。
- API 内部建议使用原地字段更新，优先使用 class handle 形式的状态对象；必要时为状态表或单个 `robIdx` 增加 semaphore/mailbox 保护，保证同一条目的多字段更新具备原子性。
- 如果后续实现发现单一状态对象存在工具兼容性或并发覆盖风险，可以退化为“每个状态字段一张独立 bit 关联数组”的形式，例如 `enq_by_rob[robIdx]`、`dispatch_by_rob[robIdx]`、`writeback_by_rob[robIdx]`。该方案能避免字段互相覆盖，但一致性快照和调试成本更高。
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

- 按 `robIdx` 顺序处理主表条目。
- 根据 `fuType/op_class` 判断本条 transaction 是否为 load/vload、store/vstore、AMO/LR/SC 或非 LS 操作。
- 所有 LS transaction 都必须携带 `lqIdx` 和 `sqIdx` 两个字段。这里的“携带字段”和“消费队列 entry”需要分开理解：
  - load/vload：`lqIdx` 为本条 load 实际占用的 LQ 起始位置，LQ 入队指针按 `numLsElem/lsq_flow` 增加；`sqIdx` 为当前 SQ 边界或 LSQ 返回的 store 依赖窗口信息，不推进 SQ 入队指针。
  - store/vstore：`sqIdx` 为本条 store 实际占用的 SQ 起始位置，SQ 入队指针按 `numLsElem/lsq_flow` 增加；`lqIdx` 为当前 LQ 边界或接口需要携带的 load 窗口信息，不推进 LQ 入队指针。
- 源码口径：`Dispatch.scala` 中 load/vload 对 LSQ `needAlloc` 置 bit0，store/vstore 置 bit1；`LSQWrapper.scala` 最终把 `lqIdx` 和 `sqIdx` 都返回给 dispatch 并更新到 uop 中。因此文档和 transaction 结构不能再写成 load/store 单字段模型。
- `numLsElem/lsq_flow` 是资源消耗粒度：
  - 标量 load/store 的 flow 为 1。
  - 向量 unit-stride 初始按保守 flow 处理，优先使用源码参数 `VecMemUnitStrideMaxFlowNum`，没有直接暴露时可先按 2 处理。
  - 其他向量 load/store 初始按保守 flow 16 处理。
  - 后续如果能从 `numLsElem`、`vtype`、`vl`、`mop` 等字段得到更精确 flow，可以在 `lsq_ctrl` 中替换保守值。
- 多元素向量入队采用“整条 transaction 原子预留”的简化策略：一条 transaction 占用 `[baseIdx, baseIdx + numLsElem)` 范围，包含环形队列回绕处理；如果本拍资源不足以容纳完整范围，则该 transaction 本拍不入队，不做部分拆分。
- AMO/LR/SC 等特殊操作根据源码确认 LQ/SQ 资源使用规则。
- 本地指针和 free count 只能在真实入队 fire 后推进；如果 DUT `canAccept` 不允许入队，本拍只保持候选项，不消耗 `lqIdx/sqIdx`。
- 回收逻辑按源码语义拆开处理：
  - LQ 资源主要随 load commit/deq、redirect/flush cancel 释放，不能仅因 load 发射或写回就提前释放。
  - SQ 资源主要随 store commit 后的 deq/sbuffer 完成路径、redirect/flush cancel 释放，不能仅因 STA/STD 发射就提前释放。
  - 状态表记录每个 `robIdx` 的入队、发射、写回、提交、flush 状态，`lsq_ctrl` 根据状态事件和 DUT monitor 返回的 deq/cancel 事实更新空闲资源。
- 分配结果在入队 fire 后回填主表，并同步更新 `common_data_transaction.sv` 中的状态表。

### 6.3 Task1：发射入队 task

数据来源：

- 从 `common_data_transaction.sv` 的主表关联数组中按 `robIdx` 从小到大提取待入队 transaction。

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
3. 从主表中按 `robIdx` 选择候选项，并计算每个候选项的 `numLsElem/lsq_flow`。
4. 调用 `lsq_ctrl` 做临时资源检查和临时索引计算，检查 LQ/SQ 空闲资源是否能容纳候选项累计 flow。
5. 本拍实际入队数量取以下条件的最小可行集合：
   - `seq_csr_common.sv` 配置的每拍最大入队数。
   - LSQ 入队端口宽度。
   - 待入队主表条目数量。
   - DUT `canAccept/lqCanAccept/sqCanAccept` 允许的入队状态。
   - LQ/SQ 当前可用资源和候选项 `numLsElem/lsq_flow` 需求。
6. 根据临时索引和主表字段生成 `LsqEnqTransaction`，驱动入队接口。
7. 只有观测到入队 fire 后，才确认本拍成功项：
   - 回填主表 `lqIdx/sqIdx/numLsElem`。
   - 推进 `lsq_ctrl` 的 LQ/SQ 本地入队指针和 free count。
   - 更新 `common_data_transaction.sv` 状态表中的 `enq` 标识。
   - 触发后续 TLB 映射 task。
8. 如果 DUT 不能接收，候选项保持待入队状态，下一拍重试，不消耗索引、不推进指针、不更新 `enq`。

向量多元素入队简化方案：

- 入队端口按 transaction/uop 粒度使用，LSQ entry 资源按 `numLsElem/lsq_flow` 粒度消耗。
- 标量 load/store 的 `numLsElem=1`。
- 向量 unit-stride 初始按 `VecMemUnitStrideMaxFlowNum` 或 2 做保守检查；其他向量 load/store 初始按 16 做保守检查。
- load/vload 和 store/vstore 都使用同一套 flow 计算，但 load/vload 只推进 LQ 指针，store/vstore 只推进 SQ 指针，另一侧索引字段仅作为边界或依赖窗口信息携带。
- 对于占用范围 `[baseIdx, baseIdx + numLsElem)` 跨越环形队列末尾的情况，`lsq_ctrl` 需要按 flag/value 形式处理回绕。
- UT 初期可以先完整支持标量路径；向量路径至少要做保守资源检查和范围预留，避免生成 DUT 源码断言必然失败的入队组合。

### 6.4 Task2：TLB 地址映射 task

输出产物：

- `tlb_transaction` 表，作为关联数组存储在 `common_data_transaction.sv` 中。

触发机制：

- 由入队 task 触发。每有 transaction 成功入队，即生成或更新对应的 TLB 映射数据。

输入来源：

- `common_data_transaction.sv` 状态表中已入队的 `robIdx`。
- 主表中对应条目的 `vaddr/VPN`、`tlbAF`、`tlbPF`、`tlbGPF`、`PBMT`。
- `seq_csr_common.sv` 提供的地址约束参数，例如起始地址和范围。
- `seq_csr_common.sv` 提供的 TLB/PTE 权限位、N 位、V 位等 plus 权重参数。
- `csr_csr_common.sv` 接口实时提供的 CSR 快照，包括当前 ASID、VMID、特权态、虚拟化使能状态以及 S1/S2 翻译相关 CSR。

核心功能：

- 以 VPN 为索引，生成虚拟地址到最终物理地址的映射。
- 将 `PBMT`、`tlbAF`、`tlbPF`、`tlbGPF` 写入对应 TLB 表项。
- 在生成每条 TLB transaction 前，从 `csr_csr_common.sv` 接口采样当前 CSR 快照，并用该快照配置 `ASID`、`VMID`。
- 根据 `csr_csr_common.sv` 实时采样到的虚拟化状态决定 `s2xlate` 和 S1/S2 映射策略。
- 禁止从 `seq_csr_common.sv` 获取 CSR/虚拟化实时状态；`seq_csr_common.sv` 中若存在被测试流程修改后的 CSR 配置，只能作为控制/权重配置，不作为 TLB 映射的实时真源。
- 生成的物理地址需要落在 `seq_csr_common.sv` 给定的地址范围内。
- 根据 VPN 推导 PTE index、`ppn_low`、`valid_index`，避免手动随机出不一致结果。

S1/S2 配置规则：

- 非虚拟化场景：配置 S1。
- Only S2 场景：仅配置 S2。
- All-stage 场景：S1 和 S2 均配置。

字段作用范围约束：

- Only S2：主表中的 `PBMT`、`tlbAF`、`tlbGPF` 作用于 S2 表项，S1 的 `tlbPF` 置 0。
- All-stage：主表中的 `PBMT`、`tlbAF`、`tlbGPF` 作用于 S2 表项，`tlbPF` 作用于 S1 表项。
- Only S1：`tlbGPF` 置 0，`PBMT`、`tlbAF`、`tlbPF` 作用于 S1 映射结果。

其他权限位、N 位、V 位等字段需要通过约束随机生成，权重必须来自 `plus.sv -> seq_csr_common.sv` 解析后的 plus 参数，并在 `post_randomize()` 中完成合法性修正，不能完全无约束随机。`post_randomize()` 需要处理 R/W/X 非法组合、V=0 时其他权限位的合法化、N 位与 Svnapot 场景约束，以及 S1/S2 不同阶段下 U/G/A/D 等位的合法性修正。

### 6.5 Task3：发射队列路由与维护 task

原“子表拆分与实时维护 task”更名为“发射队列路由与维护 task”，建议实现名为 `issue_queue_route_task`。

触发机制：

- 由 TLB 地址映射 task 触发。TLB 映射完成后立即执行，形成“入队 -> TLB 映射 -> 队列路由”的连续流程。

核心逻辑：

- 从 `common_data_transaction.sv` 状态表中读取已经入队且完成 TLB 映射的 `robIdx`。
- 根据主表中的 `fuType/op_class` 将 `robIdx` 路由到 load、STA、STD 三个发射队列。
- 不再生成 load、STA、STD 三张子表。
- 三个队列存放在 `common_data_transaction.sv` 中，由该 task 统一生成和维护。
- 队列元素建议保存轻量调度快照，发射时再根据 `robIdx` 回查主表、TLB 表和状态表。
- 最小推荐字段为 `robIdx`、`send_pri`、`ready_cycle` 或 `delay_left`。`send_pri` 在主表生成后基本稳定，缓存到队列中可以避免每拍扫描队列时频繁回查主表。
- 可选缓存 `lqIdx`、`sqIdx`、`numLsElem` 等发射高频字段。由于所有 LS transaction 都携带 `lqIdx/sqIdx`，是否缓存不再按 load/store 二选一，而应按 driver 访问频率和实现复杂度决定。
- 不建议在队列中复制完整 transaction，也不建议缓存 `src_0/imm/vaddr/tlb*`、`pc/ftq/pdest/rfWen/fpWen`、状态字段等信息。这些字段仍以主表、TLB 表和状态表为准，避免多份副本不一致。

队列元素结构示例：

```systemverilog
typedef struct packed {
  int unsigned robIdx;
  int unsigned send_pri;
  int unsigned ready_cycle; // 或 delay_left
  bit          has_lqIdx;
  int unsigned lqIdx;
  bit          has_sqIdx;
  int unsigned sqIdx;
  int unsigned numLsElem;
} issue_q_item_t;
```

其中 `lqIdx/sqIdx/numLsElem` 是可选缓存字段，不改变主表和状态表的权威性。

- 队列需要防重复入队；replay 或 redirect 场景下，由状态表控制是否重新入队。
- 队列不承担历史记录职责。条目一旦被对应发射 task 成功送入流水线，就可以立即从对应队列删除；后续状态追踪依赖主表和状态表。

#### replay 重新入队规则

replay 重新进入发射队列必须由 `exception_redirect_replay_task` 先完成事件分类和状态恢复，再由 `issue_queue_route_task` 统一入队，禁止写回 task 或发射 task 直接把条目塞回队列。

源码上需要区分两类事件：

- `replay-only`：例如 store 地址侧 TLB miss 通过 STA feedback 让 IssueQueue 重新发射。这类事件不产生全局 redirect，不需要 flush，不清理 LSQ entry，只清对应入口的发射状态。
- `redirect-flush`：例如 memory violation、nuke、nack rollback 或 trap redirect。这类事件会触发 flush 边界，必须先冻结发射、清理被 flush 的队列项和状态，再决定是否重新入队。

推荐规则：

- 每个 replay 条目使用 `(robIdx, queue_type, replay_seq)` 作为去重 key。`replay_seq` 每次确认需要重发时加 1，队列中已有同 key 时不得重复插入。
- `replay_pending=1` 表示事件已捕获并需要重发；只有 `replay_target_mask` 指定了目标队列时，队列路由 task 才能重新入队。
- replay-only 不设置 `flush_in_progress`，不递增 `global_issue_epoch`，也不阻塞其他无关 `robIdx` 的发射；只阻止同一 `robIdx` 的旧队列项、旧写回或重复 replay 覆盖状态。
- store 地址侧 replay：只重新放入 `sta_issue_q`，清除 `sta_dispatch` 和 STA 侧完成状态，保留 `std_dispatch`、STD data 状态和已分配的 `sqIdx/lqIdx`。
- store 数据侧 replay：若后续存在数据侧 feedback，则只重新放入 `std_issue_q`，不清 STA 地址状态。当前源码中常见 store TLB miss 是 STA feedback，因此初版可只实现 STA replay。
- load 的非 redirect replay 如果需要由 TB 重发，才重新放入 `load_issue_q`；MemBlock 内部 `LoadQueueReplay` 送回 LoadUnit 的事件默认只观察记录，不主动从 TB load 队列再发一次，避免重复激励。
- redirect-flush 覆盖到的条目不能直接 replay 入 load/STA/STD 发射队列。当前简化模型中被 flush 覆盖且已取消的条目标记为 `flushed` 并 retire，不在同一路径内维护重新取指状态位。
- 初版最简化策略：redirect-flush 后只恢复或重发 redirect 边界要求继续执行的条目；younger 条目标记 `flushed` 并从三个队列删除，不主动重发，避免模拟完整后端 refetch/rename。
- redirect flush 恢复未完成前，不允许任何被 flush 边界覆盖的 replay 条目重新入队，避免旧 epoch 发射和新 epoch replay 同拍冲突。

store 路由说明：

- 上游先通过 `fuType=stu` 或 op_class 判断该条是 store，并分配 SQ。
- 队列路由逻辑将该 store 对应 `robIdx` 同时放入 STA 队列和 STD 队列。
- STA 侧发射时使用 `src_0 + imm` 生成地址。
- STD 侧发射时使用 store data 源写入 SQ data。
- 到 MemBlock Verilog 端口时，STA 和 STD 已经是不同入口，不是在 MemBlock 内部再靠 `fuType` 拆分。

维护要求：

- 一旦有新入队且完成 TLB 映射的数据，立即更新对应队列。
- 队列与主表、TLB 表和状态表保持一致。
- 发射成功后，队列维护 task 或发射 task 需要删除对应队列项，避免同一 `robIdx` 重复发射。
- commit、flush、redirect 或 replay 时，队列维护 task 根据状态表判断是否需要清理残留队列项或重新放入对应 `robIdx`。

### 6.6 Task4：发射控制 task

发射控制 task 负责从 `common_data_transaction.sv` 中的 load、STA、STD 三个发射队列选择候选 transaction，并驱动对应流水线接口。

发射控制需要先读取 `seq_csr_common.sv` 中的以下参数：

- `global_send_pri_en`：是否启用全局优先级调度。
- `load_pip_num`：本拍最多允许进入 load 流水线的数量，不能超过真实 load 流水线数量。
- `sta_pip_num`：本拍最多允许进入 STA 流水线的数量，不能超过真实 STA 流水线数量。
- `std_pip_num`：本拍最多允许进入 STD 流水线的数量，不能超过真实 STD 流水线数量。

每拍实际可发射数量还需要受队列中 eligible 元素数量限制。eligible 元素至少需要满足：

- 已入队。
- 已完成 TLB 映射或不需要 TLB 映射。
- 当前目标入口尚未完成：load 看 `load_dispatch/writeback/pass`，STA 看 `sta_dispatch/sta_pass`，STD 看 `std_dispatch/std_pass`；store 不能因为另一侧已完成就阻塞当前侧 replay。
- 未被 `redirect_pending` 阻塞。
- `replay_pending=1` 时只有 `replay_target_mask` 包含当前队列才可作为 replay 候选。
- 对应流水线本拍 ready。
- 全局 `flush_in_progress` 为 0。初版保守策略下 redirect 恢复期间不允许继续发射 older 或 younger 候选。
- 候选携带的 target 级 `issue_epoch/replay_seq` 必须与当前状态一致，且没有 `issue_killed`。重复入队由队列项 `(robIdx, queue_type, replay_seq)` 去重和对应 queued 状态保护，不维护独立 replay 入队状态位。

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
- `issue_freeze_ack`：发射 task 停止选择新候选后给异常处理 task 的确认，用于避免边清队列边发射。

默认采用保守屏障策略：

1. `exception_redirect_replay_task` 捕获 replay-only 事件时，只更新对应 `robIdx` 的 `replay_pending/replay_target_mask/replay_seq`，不冻结三队列，不递增 epoch。
2. `exception_redirect_replay_task` 捕获 redirect-flush 事件时，先设置 `flush_in_progress=1` 和 `active_redirect`，并记录下一轮将使用的 `global_issue_epoch`。
3. Task4 在每拍开始先检查 `flush_in_progress`。如果为 1，停止从 load/STA/STD 队列选择新候选，并拉起 `issue_freeze_ack`。
4. 对同一拍已经完成仲裁但尚未真正驱动 agent 的候选，Task4 必须在驱动前再做一次状态二次检查：如果 `flush_in_progress=1` 且该 `robIdx` 被 `active_redirect` 命中，取消本次 valid，不更新 `dispatch`。
5. 对已经在同一拍驱动到 DUT 的 transaction，Task4 需要记录对应 target 的 `issue_epoch`。如果后续确认该 `robIdx.needFlush(active_redirect)`，设置 `issue_killed=1`，该条旧 epoch 写回或 feedback 只能被忽略，不能更新新状态。
6. `exception_redirect_replay_task` 只有在收到 `issue_freeze_ack` 后，才能清理发射队列残留项、更新状态表并处理 `lqCancelCnt/sqCancelCnt`。
7. flush 恢复完成后，异常处理 task 递增并发布新的 `global_issue_epoch`，清除 `flush_in_progress`，Task4 才能在新 epoch 下重新选择候选。

是否允许 redirect 期间穿插发射：

- 初版不允许。redirect 生效到 `flush_recover_done` 之间不发射任何新候选，这是最简单且不容易出错的策略。
- replay-only 不属于 redirect 期间穿插发射，不需要 flush 屏障；它只通过目标队列重新发射同一 `robIdx` 的指定入口。
- 后续如果为了提高压力需要优化，可以允许严格 older-than-redirect 且不受 flush 影响的条目继续发射；但该模式必须增加 `robIdx` 边界判断和 epoch 校验，初版不推荐。

发射 task 的状态更新也需要区分成功发射和被 flush 杀掉：

- 正常发射：按目标入口设置 `load_dispatch`、`sta_dispatch` 或 `std_dispatch`，并把新 epoch 记录到 `load_issue_epoch/sta_issue_epoch/std_issue_epoch` 中对应入口，从对应队列删除。load 完成后可同步设置总 `dispatch`；store 需要 STA 和 STD 都完成发射后再认为总 `dispatch` 完成。
- replay 发射：除上述入口状态外，还需要清除该入口对应的 `replay_target_mask` bit，保留 `replay_seq` 作为追溯；如果 `replay_target_mask` 中所有目标入口都已重新发射，清 `replay_pending`。
- 发射前被 redirect 命中：不设置入口 dispatch，不删除或由异常处理 task 统一清理。
- 发射后被 redirect 命中：设置 `issue_killed=1`，后续写回监测 task 忽略该旧 epoch 写回；如进入局部 replay，则由异常处理 task 设置 replay 状态，不由发射 task 自己入队。

#### `global_send_pri_en=1`：全局优先级调度

当 `global_send_pri_en` 为高时，调度策略以 `send_pri` 为第一优先级，且比较范围是 STA、STD、load 三个队列的所有 eligible 元素。

调度规则：

1. 同一拍先扫描 `sta_issue_q`、`std_issue_q`、`load_issue_q` 三个队列中的 eligible 元素。
2. 在三个队列的全集中找出当前最大的 `send_pri`。
3. 只从当前最大 `send_pri` 的候选集合中选择本拍要发射的 transaction。
4. 如果最大 `send_pri` 同时出现在 STA、STD、load 三类队列中，三类队列可以在同一拍并发发送到各自流水线。
5. 实际 driver 调用可以按 STA、STD、load 的固定顺序执行发送 task，但同一拍输出 valid 允许并发拉起。
6. 如果同一类队列中有多个相同最大 `send_pri`：
   - 数量不超过该类流水线可用数量时，全部可以同拍发射；发送顺序可随机。
   - 数量超过该类流水线可用数量时，按 `robIdx` 从小到大筛选前 N 个发射，其中 N 为该类本拍可用流水线数量。
7. load 队列最多发射 `min(load_pip_num, load_pipe_count, load_eligible_count)` 条。
8. STA 队列最多发射 `min(sta_pip_num, sta_pipe_count, sta_eligible_count)` 条。
9. STD 队列最多发射 `min(std_pip_num, std_pipe_count, std_eligible_count)` 条。

示例：

- 若当前全局最大 `send_pri=90`，且 STA、STD、load 中都存在 `send_pri=90` 的 eligible 元素，则 STA、STD、load 可同拍分别发射。
- 若 load 队列中有 3 条 `send_pri=90`，且 `load_pip_num=3`，则 3 条可同时进入 3 条 load 流水线。
- 若 load 队列中有 5 条 `send_pri=90`，但 `load_pip_num=3`，则按 `robIdx` 从小到大选择 3 条发射，其余保留到后续周期。
- STA/STD 队列同理，只要同优先级候选数量不超过对应流水线数量，就可以同拍发送到不同 STA/STD 流水线。

#### `global_send_pri_en=0`：三队列完全并行调度

当 `global_send_pri_en` 为低时，`send_pri` 不参与调度，STA、STD、load 三个队列完全并行选择候选项。三类流水线之间不做全局优先级互斥。

调度规则：

- load 每拍最多发射 `min(load_pip_num, load_pipe_count, load_queue_eligible_count)` 条。
- STA 每拍最多发射 `min(sta_pip_num, sta_pipe_count, sta_queue_eligible_count)` 条。
- STD 每拍最多发射 `min(std_pip_num, std_pipe_count, std_queue_eligible_count)` 条。
- 三类队列互不阻塞，可以同拍分别向各自流水线发送。
- 每类队列内部可以随机选择候选项；如果需要确定性回归，可通过参数切换为按 `robIdx` 从小到大选择。
- `load_pip_num/sta_pip_num/std_pip_num` 是配置上限，真实可发送数量必须同时受 DUT 流水线数量、ready 信号和队列 eligible 元素数量限制。

#### 发射前字段赋值

在真正驱动 agent 发送前，需要按固定顺序完成字段赋值：

1. 根据 `robIdx` 从主表读取 `main_control_transaction`。
2. 将主表中的第一类字段赋值到本次将要发往 agent 的 transaction。
3. 调用 `assign_issue_dep_fields()`，补齐第二类字段；如果这是 replay 发射，需要根据 `replay_seq/replay_cause` 重新计算 `isFirstIssue` 等字段。
4. 调用 `assign_backend_meta_fields()`，补齐第三类字段。
5. 记录本次 transaction 的 TB 侧 `issue_epoch/replay_seq`，并在驱动前执行 redirect 二次检查。
6. 驱动 load/STA/STD agent 发送 transaction。

这样可以确保主表保持为全局控制表，而实际发往 agent 的 transaction 满足 MemBlock issue 接口约束。

#### 状态表更新

任一 transaction 被发射到对应流水线后，发射控制 task 必须通过 `common_data_transaction.sv` 提供的统一状态更新 API 更新该 `robIdx` 的状态字段：

- 按发射目标设置入口级状态：load 设置 `load_dispatch`，STA 设置 `sta_dispatch`，STD 设置 `std_dispatch`。
- 记录发射目标类型、target 级 `issue_epoch`、`replay_seq` 和 cycle 计数，便于 debug 和 replay 追溯。
- 从对应发射队列中删除已发射的 `robIdx`。队列删除只表示“已离开发射等待队列”，不表示该 transaction 已写回或提交。
- 对于 store 同时进入 STA/STD 队列的情况，STA 和 STD 分别维护 `sta_dispatch`、`std_dispatch`；只有两者都完成后才将总 `dispatch` 视为完成。
- replay 发射成功后，只清当前入口对应的 `replay_target_mask` bit；所有目标 bit 都清空后，才清 `replay_pending`。

状态字段更新不得通过整条状态表项覆盖实现，必须调用统一 API 做字段级更新，避免和写回、异常、commit task 的状态更新互相覆盖。

发射后的追溯规则：

- 队列中只保留尚未发射的待选项。
- 已发射条目的生命周期状态由主任务状态表追踪。
- 已发射条目的原始控制字段由主控制表追踪。
- replay-only 需要重新发射时，由 `exception_redirect_replay_task` 更新 `replay_pending/replay_target_mask/replay_seq`，再由队列维护 task 将对应 `robIdx` 放入目标队列；不需要 flush 屏障。
- redirect-flush 生效时，必须先完成 flush 屏障、队列清理和 LSQ cancel 处理；被 flush 覆盖且已取消的 entry 不直接塞回发射队列。
- 旧 epoch 的写回、replay 或异常反馈不得覆盖新 epoch 状态；所有 monitor 更新状态前都需要校验 target 级 `issue_epoch/replay_seq`。

#### 发射时序控制

主表中的 `delay` 字段用于控制发射到 driver 的时序。通过 gap transaction 中的 gap 参数实现精确延迟控制，确保 DUT 输入时序合法。

### 6.7 Task5：异常监测与写回处理 task

监测目标：

- MemBlock 到后端的写回端口。
- `lqIdx`、`sqIdx`、`robIdx` 等返回信息。
- 异常、replay、redirect 相关信号。

异常处理：

- 异常、redirect、replay 不在写回 task 中直接完成恢复，而是交给专门的 `exception_redirect_replay_task` 处理。
- 写回 task 只负责采样异常事实，例如异常写回、`memoryViolation`、replay 请求、rollback redirect，并把对应 `robIdx`、异常类型和原始接口信息写入状态表。
- `exception_redirect_replay_task` 统一决定是 replay、redirect/flush 还是 trap 类 fault，并同步发射队列、commit task 和 `lsq_ctrl`，避免同一条 transaction 被错误重复发射或过早提交。

正常写回处理：

- 若写回正常，将状态表中对应条目的 `writeback/pass` 标识置高。
- 写回 task 不直接驱动 LSQ commit 端口，避免把“写回完成”和“ROB 提交/LSQ 释放”混成一个事件。
- 写回 task 需要把可提交事实写入状态表，例如 load 已正常写回、store 地址/数据已完成、异常已清空，供专门的 LSQ commit 驱动 task 按 ROB 顺序消费。
- redirect 或 replay 发生时，写回 task 需要设置 `replay_pending/redirect_pending`，阻止 commit task 继续推进对应 `robIdx`。

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
- 状态表：`dispatch/writeback/fault/pass/replay_pending/redirect_pending/rob_commit/lsq_deq`。
- 主表和队列：通过 `robIdx` 找回原始 transaction、发射队列残留项和 TLB 表项。

核心流程：

1. 每拍先采样异常和 redirect/replay 来源，统一写入 `exception_event_q`，事件至少包含 `event_type`、`robIdx`、`uopIdx`、`lqIdx/sqIdx`、exceptionVec、redirect payload、replay cause 和发生 cycle。
2. 对同拍多个事件按源码策略仲裁：
   - 异常写回事件按 `ExceptionInfoGen` 口径选择最老 `robIdx/uopIdx`。
   - rollback redirect 事件按 `Redirect.selectOldestRedirect()` 口径选择最老 redirect。
   - 已经满足 `robIdx.needFlush(active_redirect)` 的事件丢弃，不再二次处理。
3. 对 trap/fault 类异常：
   - 将主表、TLB 表、状态表中的对应 `fault/exception_pending` 置高，保存 `exceptionVec/vaddr/gpaddr`。
   - 阻止该 `robIdx` 及 younger 条目继续 commit。
   - 由 task 按测试框架的后端模型生成或等待 backend redirect；redirect 生效后进入 flush 恢复流程。
4. 对 memory violation / nuke / nack rollback：
   - 使用 `memoryViolation.bits.robIdx` 作为 flush 边界，设置 `redirect_pending=1`。
   - 对所有 `isAfter(robIdx, redirect.robIdx)` 的 younger 条目，以及 flushItself 语义覆盖到的条目，清理发射队列残留、取消 `dispatch/writeback/pass/rob_commit` 等不可再沿用状态。
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
   - 对 TLB 表，保留主表仍有效且地址映射未变的项；如果 CSR/虚拟化状态或异常场景要求重新翻译，则清除对应 `robIdx/VPN` 映射并重新触发 Task2。
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
      request_issue_freeze();
      wait_issue_freeze_ack();
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

- `common_data_transaction.sv` 主表：读取 `robIdx`、`fuType/fuOpType/op_class`、是否 load/store、是否 vector LS、`lqIdx/sqIdx`。
- 状态表：读取 `enq/dispatch/writeback/pass/fault/exception_pending/flushed/replay_pending/redirect_pending/sta_dispatch/std_dispatch` 等状态。
- `seq_csr_common.sv`：读取 commit 延迟、每拍最大 commit 数等 plus 配置。
- DUT monitor：读取 `lqDeq/sqDeq/lqDeqPtr/sqDeqPtr`、redirect/flush、store exception、MMIO/NC/CBO 完成等真实反馈。

核心流程：

1. task 内部维护一个软件 ROB commit pointer，按 `robIdx` 顺序扫描主表，最多形成 `CommitWidth` 个连续 commit candidate。
2. 每个 candidate 必须满足：已入队、未 fault、未 replay/redirect pending、没有被 flush；load 需要已经正常写回或被模型判定可提交；store 需要 STA 和 STD 均完成，且没有 store exception、`waitStoreS2`、MMIO/NC/CBO 未完成等阻塞条件。
3. 对候选集合计算本拍 ROB commit 结果：
   - `ldCommitVec[i] = commitValid[i] && commitType == LOAD`。
   - `stCommitVec[i] = commitValid[i] && commitType == STORE && !isVls`。
   - `lcommit_next = PopCount(ldCommitVec)`。
   - `scommit_next = PopCount(stCommitVec)`。
   - `commit_next = commitValid[0]`。
4. 按源码 `RegNext` 语义驱动端口：本拍计算出的 `lcommit_next/scommit_next/commit_next/pendingPtr_next/pendingPtrNext_next` 在下一拍驱动到 DUT。实现上可以用 task 内部寄存变量保存上一拍计算结果，再在每拍开始驱动 DUT。
5. `pendingPtr` 表示本拍提交边界，store queue 通过 `isNotAfter(uop.robIdx, pendingPtr)` 判断 SQ entry 是否可置 committed；因此该指针必须随 ROB commit pointer 单调推进，不能直接用最后一个写回条目的 `robIdx`。
6. 对 scalar store，`scommit` 只统计 `commitType=STORE && !vls` 的 ROB commit 条目；vector store 的 commit 还依赖 vector feedback/`vecMbCommit` 等源码路径，初期可先在文档和实现中标记为保守支持，不要错误计入 scalar `scommit`。
7. 端口驱动后，task 需要监测 MemBlock 输出：
   - `lqDeq` 表示 LQ 实际释放的 entry 数量。
   - `sqDeq` 表示 SQ 实际释放到 sbuffer/uncache/deq 路径的 entry 数量。
   - `lqDeqPtr/sqDeqPtr` 用于校验释放顺序和更新本地 `lsq_ctrl` free count。
8. 状态更新需要拆成两层：
   - `rob_commit` 或等价字段：表示该 `robIdx` 已被 commit task 作为 ROB 提交项驱动。
   - `lsq_deq` 或资源释放字段：表示观察到 `lqDeq/sqDeq` 后 LSQ entry 已实际释放。
   如果状态表暂时只有 `commit` 字段，推荐将 `commit` 定义为“ROB commit 已驱动且对应 LSQ deq 已观察完成”，避免过早清理。
9. 只有在对应 `lqDeq/sqDeq` 被观察到后，才能通知 `lsq_ctrl` 回收 LQ/SQ 资源，并清理主表、TLB 表、状态表中的已完成条目。
10. redirect/flush 发生时，commit task 必须停止推进受影响 `robIdx`，等待异常处理 task 完成状态恢复；恢复后再从新的 commit pointer 继续。

伪代码示例：

```systemverilog
task lsq_commit_drive_task();
  rob_ptr_t commit_ptr;
  lsq_commit_drive_t drive_q;

  forever begin
    drive_lsq_commit_ports(drive_q); // drive previous-cycle result, matching RegNext in Rob.scala

    collect_commit_candidates(commit_ptr, CommitWidth);
    calc_lcommit_scommit_by_commit_type();
    calc_pending_ptr_and_next();
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
- 所有表必须以关联数组形式存储在 `common_data_transaction.sv` 中。
- load、STA、STD 不再是子表，而是三个队列，统一存储在 `common_data_transaction.sv` 中。
- 三个发射队列只保存待发射项；条目成功发射后可从队列删除，追溯记录由主控制表和主任务状态表承担。
- 所有 task 共享同一套单例数据源，避免多环境访问时出现数据不一致。
- 所有参数都必须放在 `seq_csr_common.sv` 中，并通过 `plus.sv` 统一管理。
- 所有参数，例如入队宽度、每拍入队数量、`load_pip_num`、`sta_pip_num`、`std_pip_num`、TLB 权重参数、异常注入权重、提交延迟、`global_send_pri_en`、`MEMBLOCK_LD_TO_ST_ADDR_REUSE_WT`、`MEMBLOCK_ST_TO_LD_ADDR_REUSE_WT` 等，都需要支持 plusargs 配置。
- `global_send_pri_en=1` 时，发射控制必须在 STA、STD、load 三个队列的 eligible 元素全集中选择当前最大 `send_pri`；最大值分布在不同队列时允许同拍并发发送，同一队列中最大值数量超过流水线数量时按 `robIdx` 从小到大截取。
- `global_send_pri_en=0` 时，STA、STD、load 三个队列完全并行调度，分别受 `load_pip_num`、`sta_pip_num`、`std_pip_num`、实际流水线数量、ready 和队列 eligible 数量限制。
- 所有会引起 `robIdx` 状态变化的 task 都必须通过 `common_data_transaction.sv` 提供的状态更新 API 在真实事件发生点更新对应字段，禁止主任务集中代更所有状态，也禁止整条状态表项覆盖式写回。
- task 联动关系必须清晰，默认流程为“主表准备（随机生成或手动导入） -> 状态表初始化 -> 入队 -> TLB 映射 -> 队列路由 -> 发射 -> 写回监测 -> 异常处理/状态恢复 -> LSQ commit 驱动 -> LSQ deq 监测/清理”。
- 所有标识字段，例如 `enq`、`dispatch`、`writeback`、`fault`、`exception_pending`、`flushed`、`pass`、`commit`、`rob_commit`、`lsq_deq`、`success`、`access` 等，需要同步存入或映射到 `common_data_transaction.sv` 的状态表，便于调试、监测和追溯。

## 8. 整体要求

1. 主表生成与发射队列相关 task 必须协同，发射队列的所有操作都基于主表数据，确保数据流转准确。
2. 主表生成 task 必须支持两种模式：随机生成主表，以及直接导入手动配置的主表关联数组；手动导入模式不调用随机生成，但必须执行统一合法性校验和派生字段补齐。
3. 所有表的生成和更新逻辑需要关联同步，主表约束规则需要同步作用于 TLB 表、状态表和三个发射队列。
4. 所有表均存入 `common_data_transaction.sv`，并以关联数组形式组织。
5. load、STA、STD 不再生成子表，改为维护三个发射队列。
6. 三个发射队列只承担待发射缓冲职责，发射成功后删除对应队列项；后续追溯记录通过主控制表和主任务状态表完成。
7. 状态表不再由主任务内部局部维护，而是作为单独状态表存入 `common_data_transaction.sv`；主任务负责状态生命周期、初始化、清理和一致性检查，具体状态字段由直接造成状态变化的子 task 更新。
8. 所有参数均放入 `seq_csr_common.sv`，并通过 `plus.sv` 统一解析和管理。
9. 所有表生成器由两部分组成：transaction 文件管理字段和约束，`memblock_base_sequence.sv` 中的 task 实现生成、维护和写入公共数据。
10. 主控制表新增 `send_pri` 字段，范围 0 到 100，并由 `seq_csr_common.sv` 中的 `global_send_pri_en` 控制是否参与发射优先级选择。
11. `global_send_pri_en=1` 时，发射调度以 STA、STD、load 三个队列的全局最大 `send_pri` 为准；相同最大优先级可以跨队列并发发送，同队列超出流水线容量时按 `robIdx` 小者优先。
12. `global_send_pri_en=0` 时，STA、STD、load 三个队列完全并行，分别由 `load_pip_num`、`sta_pip_num`、`std_pip_num` 控制每拍最多进入对应流水线的数量。
13. 状态表统一存放在 `common_data_transaction.sv`，并通过统一状态更新 API 做字段级更新；如后续发现并发写字段存在工具或实现风险，可退化为各状态字段独立关联数组管理。
14. 优先通过约束机制和 plus 权重实现字段自动生成、关联和配置，减少手动干预。主表地址复用注入 task 的 load->store 和 store->load 频率必须通过 plus 参数控制。
15. 严格按三类字段组织主表和发往 agent 的 transaction：
    - 第一类全局控制字段进入主表并严格约束。
    - 第二类流水线发射/相关性字段不在主表阶段独立随机，而是在发射前由专门函数派生赋值。
    - 第三类后端写回/前端调试元信息字段不在主表阶段独立随机，而是在发射前由专门函数派生赋值。
16. TLB 地址映射逻辑必须贴合 TLB 接口逻辑，配置规则和字段关联需符合虚拟化场景要求；CSR/虚拟化实时状态必须从 `csr_csr_common.sv` 接口采样，不能从 `seq_csr_common.sv` 的可变配置读取。
17. PPN 约束入口需要能接收外部地址约束，并受 `seq_csr_common.sv` 的地址范围参数控制。
18. 单例模式需要保证共享安全性和访问效率，避免多环境访问冲突。
19. 队列路由需要严格按 `fuType/op_class` 执行，字段新增和发送 ID 约束需要符合 DUT 接口要求。
