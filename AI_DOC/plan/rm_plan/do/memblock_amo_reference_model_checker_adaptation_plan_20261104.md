# MemBlock AMO 参考模型与架构 Checker 适配方案

| 项目 | 内容 |
| --- | --- |
| 状态 | `DONE`，coding、review 与基础回归已完成 |
| 创建日期 | 2026-11-04 |
| 完成日期 | 2026-11-06 |
| 适用分支 | `mem_ut_uvm_v2` |
| 适用版本 | XiangShan V2 / MemBlock UT |
| 方案类型 | RM 参考模型与架构结果 checker coding plan |
| 主要目标 | 为 AMO、LR/SC、AMOCAS.W/D/Q 增加架构结果计算、写回比较和内存效果检查 |
| 必要前置改动 | 复用现有 Main table STD override，补齐所有原子 owner 的建表冻结和真实 STD input checker |
| 不包含 | 改变 AMO opcode/地址/顺序随机策略；改变 STA/STD 调度、atomic barrier、held piece、feedback 数量等生命周期实现；DCache reservation 周期级/失效原因微架构 checker |
| 代码基线 | AMO stimulus 适配及 CAS prefill 已合入后的接口形态；`main_control_transaction` 已有 `std_data_override_valid/std_data_override` |
| 关联激励方案 | `AI_DOC/plan/test_framework/plan/do/memblock_amo_stimulus_only_adaptation_plan_20261022.md` |
| 关联执行流程 | `AI_DOC/analysis/framework_design/memblock_amo_lrsc_backend_execution_flow_20261019.md` |

## 1. 文档目标

当前测试框架已经能够生成 AMO/LR/SC/CAS 指令，并检查原子 owner、STA/STD slot、feedback、final writeback 数量和 barrier 生命周期，但现有 RM 还没有建立原子指令的架构功能模型。

本方案解决以下问题：

1. 计算普通 AMO 的旧值、运算新值和最终写回值。
2. 检查 LR 返回值，并维护 LR/SC reservation 的最小架构状态。
3. 检查 SC 成功和失败返回码，以及成功或失败对应的内存副作用。
4. 检查 AMOCAS.W/D/Q 的比较结果、返回旧值和条件写入。
5. 将成功的原子写效果加入现有 expected memory，使后续 Load、AMO 和最终 `check_store` 都能看到该结果。
6. 检查原子地址翻译、PMA/PMP、PBMT、非对齐和 DCache 错误产生的异常。
7. 明确 RM 架构 checker 与现有 atomic lifecycle checker 的职责边界，避免重复建模 DUT 内部状态机。

本文只形成方案，不修改 RTL、RM、sequence、interface 或 cfg。后续 coding 除 RM/readonly/checker 外，只允许为“冻结原子操作数唯一真源”做最小 stimulus-side 改动，不重新设计现有 AMO 调度和随机策略。

## 2. 当前 RM 状态与主要缺口

### 2.1 AMO 当前被当成 OTHER

`rm_ls_core.sv` 当前只有：

```systemverilog
RM_LS_KIND_OTHER
RM_LS_KIND_LOAD
RM_LS_KIND_STORE
```

`memblock_rm.sv::observer_build_commit_item_with_observation()` 只把普通 LDU 识别成 Load，把普通 STU 识别成 Store。`MEMBLOCK_FUTYPE_MOU` 会落入 `RM_LS_KIND_OTHER`。

因此 AMO 当前提交时只执行：

```systemverilog
commit_exception == item.expected_exception
```

当前 RM 不会检查：

- AMO 写回的旧内存值；
- AMO ALU 计算的新内存值；
- LR 返回值和 reservation；
- SC 成功/失败返回码；
- SC 是否正确写入或保持内存不变；
- CAS 比较成功或失败；
- CAS.Q 两次写回；
- 原子写入对后续访存的可见性。

### 2.2 AMO 当前跳过地址和异常模型

当前 `observer_scalar_size()` 只支持普通 Load/Store 编码，且 `OTHER` 会直接使用 `path=NOT_REQUIRED` 返回。

这意味着 AMO 当前没有进入：

- 地址一致性检查；
- TLB translation；
- S1/S2 permission；
- PMA/PMP atomic permission；
- PBMT-NC/IO 限制；
- W/D/Q 对齐检查；
- denied/corrupt 错误期望。

另外，部分 AMO `fuOpType` 数值与普通 Load/Store 编码重合，不能直接用普通 `observer_scalar_size()` 推导。必须先根据 `fuType=MOU` 分类，再调用独立 atomic size decoder。

### 2.3 当前数据宽度不足以支持 CAS.Q

当前 RM 主要结构为：

```systemverilog
longint unsigned pa_by_byte[8];
bit [7:0]         pa_valid_mask;
bit [63:0]        store_data;
bit [7:0]         store_byte_mask;
```

这些结构最多支持 8 字节。AMOCAS.Q 是 16 字节访问，因此必须扩展为 16-byte geometry 和 128-bit data，不能只增加 opcode 分支。

当前 PMA/PMP model 的部分查询也限制 `size_bytes <= 8`。CAS.Q 接入时必须同步支持 16B atomic 查询，或增加等价且与 RTL 一致的 16B atomic 查询入口。

### 2.4 当前写回 API 只能读取一条 LDA

当前 `read_dut_load_writeback_for_rm()` 返回同 ROB 最新的一条非 replay、非 flush LDA 记录。

这对普通 Load 足够，但不能完整支持：

- CAS.Q 的两次 final writeback；
- 根据 `pdest` 区分 CAS.Q 低 64 位和高 64 位结果；
- 对同 ROB 多条原子写回做数量、顺序和重复检查。

### 2.5 当前主表只对 CAS prefill 冻结了原子操作数

最新 AMO stimulus 代码已经在 `main_control_transaction` 中增加：

```systemverilog
bit        std_data_override_valid[4];
bit [63:0] std_data_override[4];
```

CAS prefill 的前置 Store 和未来 CAS owner 已使用这些字段冻结 compare/new 数据，`issue_field_assigner::derive_std_data()` 也会优先读取 override。因此原方案中“主表完全没有冻结字段”的描述已经过时，不应再新增一套同义的 `atomic_std_valid_mask/atomic_std_data` 主表字段。

当前剩余缺口是：普通随机 CAS、普通 AMO、LR 和 SC 仍允许所有 override valid 为 0，issue 阶段再回退调用 `issue_field_assigner::derive_atomic_std_data()`。这意味着只有 CAS prefill 路径具备稳定的主表操作数真源，其他原子 owner 仍没有保证在建表时冻结 required STD slot。

尤其 CAS：

```text
slot0 = compare low
slot1 = new low
slot2 = compare high
slot3 = new high
```

RM 当前看不到这些实际期望操作数。若 RM 在提交时重新调用 driver helper，会让 driver 和 RM 共用同一个计算来源，无法发现 helper 自身错误。

本方案因此保留“原子操作数必须建表冻结”的要求，但复用现有 `std_data_override_valid/std_data_override`：

- CAS prefill 保留已经冻结的值；
- 其他原子 owner 在主表最终定型阶段一次性补齐 required slot；
- issue、重发、STD shuffle 和 RM 都只消费冻结值；
- `derive_atomic_std_data()` 只允许作为建表阶段生成任意 stimulus operand 的一次性 helper，不得继续作为 issue/RM 的运行时真源。

## 3. 总体设计原则

### 3.1 复用统一 expected memory

AMO 不建立独立内存镜像，继续复用现有：

```text
backing_valid/backing_byte
expected_store_valid/expected_store_byte
```

程序顺序读值保持：

```text
先读 expected memory overlay
找不到时再读 initialized backing
```

成功的 AMO、SC 和 CAS 写入统一提交到 expected memory。这样：

- 后续普通 Load 能读到 AMO 新值；
- 后续 AMO 能以前一个 AMO 的结果作为旧值；
- `check_store` 能统一检查普通 Store 和原子写效果。

### 3.2 expected 与 actual 来源分离

RM expected 来源：

- Main table 中冻结的 opcode、地址和 atomic operand；
- 独立 TLB/PMA/PMP/backing 模型；
- RM 自己维护的程序顺序 expected memory；
- RM 自己维护的 reservation。

DUT actual 来源：

- LDA0 atomic final writeback；
- status terminal exception；
- 最终 committed memory overlay；
- 可选的 DCache atomic request/response 被动观测。

禁止用 DUT 写回数据反推 expected old value 或 expected ALU 结果。

### 3.3 不复制 AtomicsUnit FSM

RM 不建模以下内部时序：

- SBuffer flush 等待周期；
- Uncache 排空周期；
- TLB polling 次数；
- DCache MainPipe stage；
- MissQueue refill/replay 次数；
- AtomicsUnit FSM state；
- `stdCnt/data_valid` 的逐拍变化。

这些行为只要最终得到唯一合法架构结果即可。必要的接口和生命周期约束继续由现有 checker 检查。

## 4. 数据结构修改方案

### 4.1 增加 RM atomic kind

在 `rm_ls_kind_e` 中增加：

```systemverilog
RM_LS_KIND_ATOMIC
```

再增加原子子类型：

```systemverilog
typedef enum int unsigned {
    RM_ATOMIC_NONE,
    RM_ATOMIC_LR,
    RM_ATOMIC_SC,
    RM_ATOMIC_AMO,
    RM_ATOMIC_CAS
} rm_atomic_kind_e;
```

### 4.2 扩展 `rm_ls_program_item_t`

建议增加：

```systemverilog
rm_atomic_kind_e atomic_kind;
int unsigned     atomic_size_bytes;       // 4、8、16
bit [3:0]        atomic_std_valid_mask;
bit [63:0]       atomic_std_data[4];
bit [127:0]      atomic_operand;
bit [127:0]      atomic_compare_value;
bit [127:0]      atomic_new_value;
bit [127:0]      expected_old_value;
bit [127:0]      expected_memory_value;
bit              expected_memory_write;
bit              sc_success_allowed;
bit              expected_cas_success;
int unsigned     expected_atomic_wb_count;
```

地址结构扩展为：

```systemverilog
longint unsigned pa_by_byte[16];
bit [15:0]       pa_valid_mask;
```

普通 Load/Store 仍只使用低 8 个 byte，不改变已有 1/2/4/8B 语义。

### 4.3 增加 atomic actual

新增值型对象 `rm_atomic_actual_t`：

```systemverilog
bit                  valid;
int unsigned         uid;
rm_ls_rob_key_t      rob;
int unsigned         dynamic_epoch;
int unsigned         wb_count;
rm_atomic_wb_record_t wb[2];
```

每条 WB record 至少保存：

```text
data
exception_vec
pdest
rf_wen
flush_pipe
replay_inst
sample_seq
sample_cycle
```

### 4.4 泛化 memory-effect entry

建议把 `rm_ls_store_cache_entry_t` 泛化为：

```systemverilog
rm_ls_memory_effect_entry_t
```

增加来源类型：

```text
STORE
AMO
SC_SUCCESS
CAS_SUCCESS
```

entry 支持 1 到 16 字节：

```text
uid/ROB
source kind
vaddr
pa_by_byte[16]
size_bytes
byte_mask[15:0]
committed_data[127:0]
```

为了降低改动，也可以保留现有 class 名称，但其语义必须从“普通 Store entry”扩展为“所有 committed memory write effect”。日志中要输出真实 source kind，避免把 AMO 错称为 Store。

### 4.5 reservation 状态

在 `rm_ls_model_t` 增加一个最小、提交级的 reservation 可能性状态：

```systemverilog
typedef enum bit {
    RM_RESV_NONE,
    RM_RESV_POSSIBLE
} rm_reservation_state_e;

rm_reservation_state_e reservation_state;
longint unsigned     reservation_block_addr;
int unsigned         reservation_size_bytes;
int unsigned         reservation_lr_uid;
rm_ls_rob_key_t      reservation_lr_rob;
longint unsigned     reservation_create_sample;
```

语义：

- `RM_RESV_NONE`：根据 RM 已知事实，当前 SC 不可能合法成功；
- `RM_RESV_POSSIBLE`：正常 LR 已经建立过 reservation，但从 LR 到 SC 之间可能发生 RM 不可观察的 RTL 超时或 matching release/writeback，RM 不能断言 reservation 此刻仍然有效；
- `reservation_create_sample` 只用于日志和问题定位，不用于模拟 `lrsc_count` 或决定 SC 成败。
- `reservation_size_bytes` 只用于日志和诊断；当前 RTL 的 SC success 地址条件按 cache block 比较，不额外要求 LR/SC width 相同。

第一版只维护当前 hart 的单个 reservation，不建立 coherence、cache residency 或 DCache 周期模型。

### 4.6 SC failure no-write obligation

新增值型对象：

```systemverilog
typedef enum bit {
    RM_SC_NOWRITE_PENDING,
    RM_SC_NOWRITE_FINAL_CHECK_STORE
} rm_sc_no_write_evidence_e;

class rm_sc_no_write_obligation_t;
    bit                  valid;
    int unsigned         uid;
    rm_ls_rob_key_t      rob;
    int unsigned         dynamic_epoch;
    longint unsigned     pa_by_byte[16];
    bit [15:0]           byte_mask;
    int unsigned         size_bytes;
    bit [127:0]          expected_before_value;
    bit [15:0]           shadowed_mask;
    bit [15:0]           checked_mask;
    bit                  resolved;
    rm_sc_no_write_evidence_e evidence_kind;
endclass
```

`rm_ls_model_t` 保存尚未完成或已完成审计的 obligation queue。该对象只描述“该失败 SC 不应产生写效果”，不充当成功 memory-effect entry。

## 5. 原子操作数冻结方案

### 5.1 复用现有 Main table 冻结字段

不再向 `main_control_transaction` 增加重复的 `atomic_std_valid_mask/atomic_std_data`，统一复用当前已经存在的：

```systemverilog
bit        std_data_override_valid[4];
bit [63:0] std_data_override[4];
```

这两个字段从“可选 override”提升为原子 owner required STD slot 的唯一真源。主表最终定型后：

- issue driver 按 slot 读取冻结值发送 STD；
- redirect 重发和 STD 顺序重随机不改变冻结值；
- readonly API 复制冻结值给 RM；
- RM 只根据冻结值计算 expected；
- 被动 input checker 比较真实 issue STD payload 与对应 slot 的冻结值。

RM 内部 `rm_ls_program_item_t` 可以继续保存归一化的 `atomic_std_valid_mask/atomic_std_data[4]`，但它们只是 readonly Main view 的值型副本，不是第二套主表字段或第二个生成源。

### 5.2 冻结时机和回退规则

增加统一的主表最终化步骤，例如：

```text
freeze_atomic_std_operands(main_tr)
```

执行规则：

1. CAS prefill owner 已有完整 override 时原样保留，禁止重新派生；
2. 其他原子 owner 在主表最终定型时仅生成一次 required slot 数据并写入 override；
3. 冻结完成后，原子 issue 遇到 required slot valid=0 必须报错，不能静默回退；
4. `derive_atomic_std_data()` 只允许被最终化步骤用于生成任意 stimulus value，issue 和 RM 不得调用；
5. 普通 Store 保持现有 slot0 override 语义，不受原子严格规则影响；
6. draft transaction 可以暂时没有 override，但进入真实 Main table/readonly view 前必须完成冻结和完整性校验。

之所以把严格校验放在“主表最终定型”而不是对象刚 randomize 完成的瞬间，是因为当前 generator 会先选择 opcode/地址，再规划 LR/SC 或 CAS prefill companion。过早要求所有 valid 已置位会阻断现有两阶段建表流程。

最小 coding 插入点固定为：随机/预约成员已经确定 opcode、地址和 companion 角色之后，调用 `validate_main_table_entry()` 和 `set_main_transaction()` 之前。若原始 CAS 被 CAS prefill 的 low Store 替换，不冻结已经被替换的临时 CAS；未来 CAS owner 在 reservation 被消费并构造出来时直接携带已有四槽冻结值。

不要直接把当前宽松的 `validate_main_transaction()` 改成“任何 draft atomic 都必须已有 override”，否则会破坏 randomize 和 companion 规划。新增最终边界检查，例如 `validate_frozen_atomic_std_operands()`，只在真实 Main table 接纳前强制 required mask。

### 5.3 slot 含义

```text
普通 AMO/SC：
  slot0 = operand 或写入数据

LR：
  slot0 保留为测试框架要求的数据片段，但架构计算不使用它

AMOCAS.W/D：
  slot0 = compare
  slot1 = new value

AMOCAS.Q：
  slot0 = compare[63:0]
  slot1 = new[63:0]
  slot2 = compare[127:64]
  slot3 = new[127:64]
```

STD 发射顺序可以乱序，但 RM 按 slot 身份组装操作数，不能按到达先后猜测含义。

冻结完成后的 required valid mask 固定为：

```text
普通 AMO/LR/SC：4'b0001
AMOCAS.W/D：    4'b0011
AMOCAS.Q：      4'b1111
```

其中 LR 的 slot0 是当前 MemBlock 原子输入合同要求的数据片段，架构 LR 结果不使用该值，但 input checker 仍检查它在重发和乱序过程中保持一致。

### 5.4 input checker 职责

input checker 不是第二个 RM。它在真实 `issueStd_0/1.fire` 时检查：

```text
框架同拍 expected owner：uid + 完整 ROB + dynamic epoch + expected slot
真实 STD monitor：pipe + robIdx_value + fuOpType中的slot + src_0 + valid/ready
actual issueStd.bits.src_0 == Main table std_data_override[slot]
```

STD 接口本身没有完整 ROB flag 和 dynamic epoch；dynamic epoch 只能来自框架 owner/issue ledger。checker 必须用同一拍的 expected fired piece 把被动 monitor 事实绑定到 owner，同时逐项检查 monitor 上的 `robIdx_value` 和 slot，不能声称这些元数据直接存在于 DUT wire 上。

expected fired piece 只用于说明“本拍应该是哪一个 owner/slot”，实际 payload、pipe、valid/ready 和 wire identity 必须来自 monitor。它负责发现 slot/data 绑定、pipe 选择、held-piece 保持和重发 payload 错误。RM 则负责使用同一份冻结 stimulus specification 计算架构结果，两者不共用 driver 的运行时派生结果。

CAS.Q 即使按 `2,0,3,1` 等顺序发射，也必须分别满足 slot2=compare high、slot0=compare low、slot3=new high、slot1=new low，不能按到达顺序重新解释角色。

## 6. 地址、翻译和异常模型

### 6.1 独立 atomic size decoder

增加：

```systemverilog
observer_atomic_size(fu_op_type)
```

规则：

```text
所有 *_W 和 LR.W/SC.W/CAS.W：4B
所有 *_D 和 LR.D/SC.D/CAS.D：8B
CAS.Q：16B
```

不得根据普通 Load/Store 同值 opcode 推导 atomic size。

### 6.2 atomic effective address

当前 AtomicsUnit 使用 STA 输入中的 `src_rs1` 作为地址，不在 AtomicsUnit 内执行 `rs1 + imm`。

当前 AMO stimulus 应保持：

```text
atomic address = main.src_0
imm = 0
main.vaddr = src_0
```

RM 对 AMO 应检查该契约。如果以后接口改为在进入 AtomicsUnit 前已经完成地址计算，则 readonly API 应明确暴露“实际架构 effective address”，不能继续隐式复用普通 Load/Store 公式。

### 6.3 对齐规则

```text
W：vaddr[1:0] == 0
D：vaddr[2:0] == 0
Q：vaddr[3:0] == 0
```

异常类型：

```text
LR 非对齐：loadAddrMisaligned
SC/普通 AMO/CAS 非对齐：storeAddrMisaligned
```

原子操作不复用普通标量非对齐拆分能力。即使普通 Load/Store 支持硬件非对齐，AMO 仍按自身宽度严格对齐。

### 6.4 TLB permission 命令

```text
LR：ATOM_READ
SC/普通 AMO/CAS：ATOM_WRITE
```

因此：

- LR 使用 load 类 PF/GPF/AF；
- SC、普通 AMO、CAS 使用 store 类 PF/GPF/AF；
- SC 即使最终失败，也仍按 atomic write 权限检查；
- CAS 比较失败也不能跳过 atomic write 权限检查。

### 6.5 PMA/PMP 与 PBMT

PMA/PMP 查询必须使用：

```text
LR：PMA_PMP_CMD_ATOM_READ
其他 atomic：PMA_PMP_CMD_ATOM_WRITE
```

期望规则：

1. PMA atomic 属性不允许时产生 access fault。
2. SC/AMO/CAS 还需要 write permission。
3. LR 需要 read permission和 atomic permission。
4. PBMT-NC、PBMT-IO 和实际 PMA-MMIO/非 cacheable atomic 路径产生 access fault。
5. 原子指令不进入普通 Uncache 正常数据路径。
6. CAS.Q 的 16B 查询必须与 RTL 的 `PMPChecker(lgMaxSize=4)` 语义对齐。

### 6.6 DCache denied/corrupt

独立 D-error ledger 应扩展到 atomic access：

```text
LR + denied：loadAccessFault
SC/AMO/CAS + denied：storeAccessFault
corrupt 且未 denied：hardwareError
```

错误必须绑定当前 atomic owner、地址和 access sample，不能读取同 cache line 的历史 sticky error 后直接套用。

### 6.7 fault 对内存效果的约束

只要 expected 或 actual 终态存在架构异常：

- 不计算正常数据比较；
- 不提交 expected memory effect；
- 不建立 LR reservation；
- 不允许 SC/CAS/AMO 修改 expected memory；
- CAS.Q 的两条 WB 都必须属于同一个 fault owner。

## 7. 原子运算参考函数

### 7.1 普通 AMO helper

新增纯值函数：

```systemverilog
compute_amo_new_value(op, size_bytes, old_value, operand)
```

支持：

```text
AMOSWAP
AMOADD
AMOXOR
AMOAND
AMOOR
AMOMIN
AMOMAX
AMOMINU
AMOMAXU
```

规则：

1. W 操作只对低 32 位运算。
2. D 操作只对低 64 位运算。
3. ADD 按目标宽度自然截断。
4. MIN/MAX 使用目标宽度的有符号比较。
5. MINU/MAXU 使用目标宽度的无符号比较。
6. AMOSWAP 的新值直接取 operand。
7. 内存只更新目标宽度对应的 byte。
8. 普通 AMO writeback 始终返回旧内存值。
9. W writeback 将旧的 32 位值符号扩展到 64 位。

### 7.2 CAS helper

```systemverilog
cas_success = width(old) == width(compare);
```

```text
成功：
  WB = old
  memory = new value

失败：
  WB = old
  memory 不变
```

CAS.W 的 WB 仍按 32 位旧值符号扩展。CAS.D 返回 64 位旧值。CAS.Q 返回完整 128 位旧值，由两条 64-bit WB 拼接。

## 8. LR/SC reservation 模型

### 8.1 LR

正常 LR：

```text
expected WB = old memory
memory effect = none
reservation.state = RM_RESV_POSSIBLE
reservation.block = cacheline block address
reservation.width = LR width
```

这里的 `POSSIBLE` 表示 RM 确认该 LR 具备建立 reservation 的架构条件，但不能从延后的 ROB commit 时刻反推 DCache 中 reservation 当前是否已因超时或 release/writeback 失效。

fault LR 不建立新的 reservation。DCache miss、MissQueue 等待和 replay 不是 LR 建立失败，不能据此清除或拒绝 reservation；它们只是 LR 在 DUT 内部尚未形成最终结果。

### 8.2 SC

DUT SC writeback：

```text
0 = success
1 = failure
```

SC 成功：

```text
reservation.state 必须为 RM_RESV_POSSIBLE
reservation block 必须匹配
WB 必须为 0
memory 写入 slot0 数据
正常 SC 后 reservation.state = RM_RESV_NONE
```

SC 失败：

```text
WB 必须为 1
memory 必须保持不变
正常 SC 后 reservation.state = RM_RESV_NONE
```

### 8.3 合法结果集合，而不是确定预测 SC 成败

基础 RM 计算：

```systemverilog
sc_success_allowed =
    reservation_state == RM_RESV_POSSIBLE &&
    reservation_block_addr == sc_block_addr;
```

然后用 DUT SC final WB 在合法结果集合中选择实际分支：

```systemverilog
if (dut_sc_result == 0) begin
    if (!sc_success_allowed)
        report SC_SUCCESS_WITHOUT_RESERVATION;
    else
        commit expected SC memory effect;
end
else if (dut_sc_result == 1) begin
    create/check SC no-write obligation;
end
else begin
    report illegal XiangShan SC result;
end

reservation_state = RM_RESV_NONE;
```

判定矩阵：

| RM 已知状态 | DUT SC WB | 结果 |
| --- | --- | --- |
| `NONE` | 0 | 非法，报错 |
| block 不匹配 | 0 | 非法，报错 |
| `POSSIBLE` 且 block 匹配 | 0 | 合法，检查并提交 SC 写效果 |
| `NONE` | 1 | 合法失败，不得写内存 |
| block 不匹配 | 1 | 合法失败，不得写内存 |
| `POSSIBLE` 且 block 匹配 | 1 | 合法；可能是偶发失败、超时或不可观察的 release/writeback 失效 |

DUT WB 只负责从多个合法结果中选择实际发生的分支。SC 地址、写数据、旧内存值和成功分支的 memory effect 仍全部来自 Main table 冻结值和 RM expected memory，禁止使用 DUT WB 反推 expected 数据。

### 8.4 提交级 RM 可确定的状态转移

基础 RM 按以下确定表更新状态：

| 提交级事件 | reservation 转移 |
| --- | --- |
| 正常 LR | 覆盖为新的 `RM_RESV_POSSIBLE` |
| fault LR | 不建立新记录，保留进入该 LR 前的状态 |
| 正常 SC，WB=0 或 1 | `RM_RESV_NONE` |
| misaligned、TLB、PMA/PMP、PBMT、trigger 等 DCache 前 fault SC | 保留原状态 |
| DCache response denied/corrupt 的 fault SC | `RM_RESV_NONE` |
| reset | `RM_RESV_NONE` |

DCache 前 fault 来源只使用当前 RM 已有的 expected fault provenance 分类确定：`expected_dcache_denied/expected_dcache_corrupt` 表示请求已经到达 MainPipe，不新增、也不依赖未定义的 actual DCache fault provenance。若 expected exception 与 actual/status exception 来源或cause不一致，先报告 mismatch，并将 reservation 清为 `RM_RESV_NONE`，避免错误路径给后续 SC 留下会放宽 success 的陈旧状态。

当前 XiangShan RTL 中，普通本 hart Store/AMO 命中同一 cache line 不直接清除 `lrsc_count`。因此基础 RM 不得把本 hart 同 block Store/AMO 当作必然失效事件。

### 8.5 不模拟不可观察的 RTL 失效原因

当前 RM 在 ROB commit 后处理指令，不能看到 reservation 在 DCache MainPipe S3 建立的精确周期，也不能看到：

- `lrsc_count` 的逐周期递减；
- matching cache block 的内部 release/writeback；
- `invalid_resv_set`；
- `lrsc_valid` 从 1 变 0 的准确时刻。

因此禁止使用以下近似决定 SC 成败：

```text
LR commit 后轮询 64 次
LR/SC UID 距离超过 64
LR/SC ROB commit cycle 差超过 64
reservation_create_sample 与 SC sample 的差值
```

这些量与 RTL `lrsc_count` 起点不对齐，会制造假失败。超时和 matching release/writeback 只作为“匹配 SC 允许返回 1”的隐藏原因，不在基础 RM 中预测或验证具体原因。

本方案暂不增加 DCache reservation 微架构 checker，不绑定或读取 `update_resv_set`、`lrsc_locked_block`、`invalid_resv_set`、`lrsc_count/lrsc_valid`。因此当前方案明确不覆盖“RTL 是否在精确周期或精确 release/writeback 事件上清除 reservation”这一微架构检查点。

### 8.6 SC failure no-write obligation

`SC WB=1` 只证明 DUT 报告失败，不能单独证明 DUT 没有错误写内存。RM 必须建立 no-write obligation：

```text
uid/ROB/dynamic epoch
PA byte geometry
size
SC 前 expected memory value
actual SC result = failure
```

第一版固定使用最终 `check_store` watch obligation，不依赖 owner 化 DCache write 事实：

1. SC failure commit 时读取并冻结 SC 前 expected memory byte，创建 obligation；
2. RM 继续推进 UID，obligation 延迟到最终 `check_store` 处理，不阻塞 commit cursor；
3. 后续合法 memory effect 写到 obligation 覆盖的 PA byte 时，将对应 bit 记入 `shadowed_mask`；
4. `observer_check_final_store()` 遍历“成功 memory-effect entry + SC failure obligation 地址”的并集；
5. 对未 shadowed byte，读取最终 committed overlay，与 RM 最终 expected byte 比较并记入 `checked_mask`；
6. 若 DUT overlay 对该地址不存在，按“未观察到 DUT 写入”处理，再与 initialized backing/RM expected byte 的存在性和值一致性比较，不能把 entry 不存在直接报成缺失；
7. directed SC-failure case 在 obligation 完成前不得插入同地址年轻写，要求 `shadowed_mask=0` 且全部目标 byte 进入 `checked_mask`；
8. 随机场景中被后续合法写覆盖的 byte 只记为 shadowed coverage limitation，不得宣称该 byte 已验证 SC no-write；未 shadowed byte 仍必须检查。

状态计算：

```systemverilog
required_unshadowed_mask = byte_mask & ~shadowed_mask;
resolved = (checked_mask & required_unshadowed_mask) == required_unshadowed_mask;
fully_verified = resolved && shadowed_mask == '0;
```

`expected_before_value`保存 SC failure 当时的诊断快照；最终比较使用 RM final expected byte。对未 shadowed byte，两者应保持一致，否则说明后续写入的 shadow 标记或 expected memory 更新存在遗漏。shadowed byte 只参与覆盖统计，不使用 `expected_before_value` 与最终 overlay 做归因比较。

无论采用哪种证据，SC failure 都不创建成功 memory-effect entry，也不更新 expected memory。

## 9. atomic final writeback 采样

### 9.1 新增批量读取 API

在 `memblock_rm_dut_writeback_observer` 和 readonly API 增加类似：

```systemverilog
read_dut_atomic_writebacks_for_rm(
    rob_key,
    dynamic_epoch,
    expected_count,
    records
)
```

### 9.2 选择规则

1. 只读取 AtomicWBPort 对应的 LDA0。
2. ROB flag/value 必须精确匹配。
3. 排除 `replayInst=1`。
4. 排除 `flushPipe=1` 的 stale/replay 记录；若某种合法 atomic completion 可能携带该位，需按 RTL 明确语义分类，不能直接复用普通 Load 过滤规则。
5. 排除旧 dynamic epoch 或旧 flush epoch。
6. 普通 AMO/LR/SC/CAS.W/D 期望 1 条。
7. CAS.Q 期望 2 条。
8. 记录按 `sample_seq` 排序，但数据身份优先依据 `pdest`，不能只依赖先后顺序。
9. 多于期望数量视为 duplicate final WB。
10. 少于期望数量时等待事实；commit 已发生且事实仍缺失时报告 missing WB，并消费当前 UID，避免永久阻塞后续比较。

### 9.3 CAS.Q 两次 WB

RM 需要从 atomic STA slot snapshot 获得：

```text
slot0 pdest = low result pdest
slot2 pdest = high result pdest
```

然后匹配：

```text
low WB  -> expected_old[63:0]
high WB -> expected_old[127:64]
```

不能假设第一条永远是 low、第二条永远是 high；RTL 当前按 `s_finish`、`s_finish2` 顺序输出，但 checker 使用 pdest 身份更稳健，也能发现 pdest 绑定错误。

## 10. commit 阶段处理流程

### 10.1 atomic commit 前的事实准备

对当前 committed atomic UID：

1. 从 readonly Main/status 构造 atomic item。
2. 完成地址、翻译、PMA/PMP、PBMT 和 expected exception 计算。
3. 从 Main readonly view 读取冻结的 atomic STD operands。
4. 读取 1 条或 2 条 atomic final WB。
5. 比较 RM expected、WB exception 和 status exception。
6. 无异常时读取 initialized backing，并叠加 RM expected memory，得到 old value。
7. 根据 opcode 计算 expected WB、SC success allowed 集合和 expected memory effect。
8. 比较 actual WB；SC 用实际 0/1 在合法结果集合中选择 success/failure 分支。
9. SC failure 建立 no-write obligation；该 obligation 延迟到最终 `check_store` 完成，SC success 构造写效果。
10. 只有合法结果才提交 memory effect；reservation 生命周期按已消费的 LR/SC 终态独立推进。
11. 推进 RM current UID。

### 10.2 指令行为矩阵

| 指令 | expected WB | expected memory | reservation |
| --- | --- | --- | --- |
| 普通 AMO | old | `AMO_ALU(old, operand)` | 不建立 |
| LR | old | 不变 | 建立 `POSSIBLE` |
| SC success | 0 | 写入 operand | 清除 |
| SC failure | 1 | 不变并创建 no-write obligation | 清除 |
| CAS success | old | 写入 new value | 不建立 |
| CAS failure | old | 不变 | 不建立 |
| 任意 fault | 不比较正常数据 | 不变 | fault LR 保留旧状态；SC 按 8.4 确定表处理 |

### 10.3 原子提交原则

与当前 Store 一样，memory effect 必须先验证全部证据再一次性提交；reservation 生命周期则按已消费终态独立推进：

```text
检查地址和异常
检查 old value 可构造
检查所有 WB
检查 SC 合法结果集合和 CAS outcome
建立 SC failure no-write obligation
检查 memory effect geometry
合法 success 才更新 expected memory
已消费 LR/SC 按 8.4 独立更新 reservation
```

memory effect 与 reservation 生命周期分开提交：

- 只有合法 SC success 才能更新 expected memory；
- SC failure 只创建 no-write obligation，不更新 expected memory；
- 非法 SC success 必须立即报错，不得把非法 DUT 写入吸收到 expected memory，也不得伪装成 `SC failure no-write obligation`；该 case 已经失败，后续可能出现的内存连锁 mismatch 仅作为附加诊断；
- 一旦正常 SC 的 committed terminal 事实被 RM 消费，reservation 必须转为 `RM_RESV_NONE`，即使返回码、地址或数据比较失败；
- 正常 LR 的 committed terminal 事实被消费后建立新的 `RM_RESV_POSSIBLE`，即使其数据比较随后报错，避免后续状态停留在旧 LR；
- DCache 前 fault LR/SC 与 DCache fault SC 按 8.4 状态表推进。

任何中间失败都不能留下部分 byte 更新或半个 CAS.Q effect；但已消费指令的 reservation 生命周期转移也不能因为 compare error 被跳过，否则会让后续 UID 使用陈旧状态。

## 11. checker 分层

### 11.1 继续由现有 lifecycle checker 负责

- atomic barrier 是否建立和释放；
- younger 指令是否被 barrier 阻止；
- required STA/STD slot mask；
- slot 是否重复或缺失；
- held piece 在 ready=0 时是否稳定；
- STD shuffle/STA-STD 发射顺序；
- atomic STA feedback 数量；
- final WB 数量；
- redirect/flush/epoch 生命周期；
- 原子不分配 LQ/SQ owner。

### 11.2 RM 新增的架构 checker

- opcode 和宽度是否合法；
- atomic 地址及对齐异常；
- TLB/PMA/PMP/PBMT 权限；
- old memory value；
- AMO ALU 新值；
- W 型写回符号扩展；
- LR 返回值和 reservation 建立；
- SC 返回码与 memory effect 一致；
- SC success 只能来自 `POSSIBLE + matching block`；
- SC failure no-write obligation；
- CAS 比较结果与条件写入；
- CAS.Q 双 WB 拼接；
- fault 抑制 memory effect；
- 原子 effect 对后续访存可见；
- 最终 committed overlay 与 expected memory 一致。

### 11.3 可选 DCache atomic request checker

后续可增加被动 checker，比较：

```text
cmd
paddr/block/word index
amo_mask
amo_data
amo_cmp
size
```

该 checker 只验证 AtomicsUnit 到 DCache 的请求编码，不参与架构结果真值计算。

允许 miss/replay 导致同一 owner 多次发送 DCache request，但只能形成一个架构终态和一个 committed memory effect。

### 11.4 本方案不增加 reservation 微架构 checker

本次 coding 不增加、也不依赖以下 checker：

```text
lrsc_count 64-cycle 精确计数
lrsc_valid 精确窗口
matching release/writeback -> invalid_resv_set
同 block Probe 阻塞周期
block_lr backoff 周期
```

这些行为不影响基础架构 RM 对合法 SC outcome 和最终 memory effect 的检查，但属于明确保留的微架构覆盖缺口。

## 12. readonly API 修改范围

### 12.1 Main view

增加：

```text
std_data_override_valid[4]
std_data_override[4]
amo_expect_misaligned（仅诊断，不作为 expected 真源）
amo_alignment_bytes（仅静态一致性检查）
```

readonly API 可以在返回 RM view 时把 unpacked valid array 归一化为 `atomic_std_valid_mask`，但源值必须逐槽复制自 Main table 的 `std_data_override_valid/std_data_override`。API 不得调用 `derive_atomic_std_data()` 补洞，也不得根据已经发射的 STD payload 反填 expected operand。

### 12.2 Status view

只增加 RM 必需的值型诊断字段：

```text
atomic_final_wb_expected_count
atomic_final_wb_seen_count
atomic_finalized
atomic_pending_fault
```

RM 不读取或控制 `active_atomic_barrier_uid`、held piece object、IQ queue handle。

### 12.3 issue expected/actual snapshot view

为 CAS.Q pdest 和 STD payload checker 增加两类值型事实：

预期身份和值来自冻结 Main/owner：

```text
uid/ROB
dynamic epoch
target
slot
expected payload
expected pdest
```

实际 STD 输入只能来自真实 MemBlock issue 接口的被动 monitor：

```text
target
slot
pipe
actual payload
actual robIdx_value
actual fuOpType/fuType
actual sqIdx
sample seq
fire
```

STD wire 不携带完整 ROB flag、uid 或 dynamic epoch；checker 通过同拍 expected issue ledger 绑定 owner/epoch，再检查 monitor 可见字段。CAS.Q 的 low/high `pdest` 则来自 STA slot0/slot2 的实际接口 snapshot，不是 STD snapshot。

input checker 使用 expected/actual 两类值型事实比较，但不得用 driver 已冻结的 held piece 或即将发送的 xaction 充当 actual payload 证据，否则只能证明 driver 内部自洽，不能证明真实 DUT 接口收到的数据正确。RM 不读取可变 owner object；RM 只读取 readonly Main 冻结值和已经冻结的 STA pdest 身份。

## 13. `check_store` 与最终内存检查

成功的 AMO、SC 和 CAS 必须加入统一 memory-effect entry。

`observer_check_final_store()` 改为遍历所有 memory-effect entry：

```text
普通 Store
普通 AMO
SC success
CAS success
SC failure watch obligation address
```

对于同一 PA 被多次写入：

- 每个 entry 的 expected data 从最终 expected byte table 读取，保持现有最终 overlay 检查语义；
- 日志额外输出 source kind，说明该地址最后由哪类操作更新；
- SC failure、CAS failure、LR 和 fault atomic 不创建成功 write-effect entry；
- SC failure 另外创建 no-write obligation，不能仅因“没有成功 entry”就判定无写；
- `check_store` 对 obligation 未 shadowed byte 做 committed overlay/backing/expected byte 比较；
- overlay entry 不存在表示“DUT overlay 未记录该地址写入”，需要结合 backing/expected byte 判断，而不是直接报 missing；
- 后续合法写覆盖的 byte 写入 `shadowed_mask`，只作为 coverage limitation，不计入 `checked_mask`；
- directed SC-failure case 在 no-write obligation 完成前避免同地址年轻写覆盖证据，并要求 obligation 全 byte 完成。

CAS.Q 必须一次比较完整 16 个 byte，不能拆成两个彼此独立、可能部分通过的 8-byte Store entry。

## 14. aq/rl 处理边界

当前 MemBlock 测试接口和主表没有把 `aq/rl` 保存为独立可控字段，当前 `fuOpType` 也不携带可供 RM 独立检查的 aq/rl 语义。

因此本方案：

- 不增加 aq/rl 随机控制；
- 不增加 aq/rl ordering checker；
- 不把 aq/rl 当作 AMO 功能 RM 完整性的阻塞项；
- 保留以后扩展 backend ordering 接口时再单独建模。

## 15. 错误分类与日志

建议新增错误类型：

```text
RM_LS_ERR_ATOMIC_OPERAND_NOT_READY
RM_LS_ERR_ATOMIC_WB_NOT_READY
RM_LS_ERR_ATOMIC_WB_COUNT_MISMATCH
RM_LS_ERR_ATOMIC_WB_IDENTITY_MISMATCH
RM_LS_ERR_ATOMIC_DATA_MISMATCH
RM_LS_ERR_ATOMIC_RESULT_MISMATCH
RM_LS_ERR_ATOMIC_MEMORY_EFFECT_MISMATCH
RM_LS_ERR_ATOMIC_SIZE_UNSUPPORTED
RM_LS_ERR_ATOMIC_ALIGNMENT_MISMATCH
RM_LS_ERR_SC_SUCCESS_WITHOUT_RESERVATION
RM_LS_ERR_SC_RESULT_MISMATCH
RM_LS_ERR_SC_FAILURE_MEMORY_EFFECT
RM_LS_ERR_CAS_RESULT_MISMATCH
RM_LS_ERR_CASQ_WB_PAIR_MISMATCH
```

每笔 atomic compare 日志至少包含：

```text
uid/ROB/dynamic epoch
opcode/width
VA/first PA
STD slot values
old value
expected WB / actual WB
SC success allowed / actual success
expected memory write enable
expected new memory value
expected/status/WB exception
result PASS/FAIL
```

## 16. 分阶段 Coding 顺序

### 阶段 1：基础结构和普通 AMO W/D

- [x] 增加 `RM_LS_KIND_ATOMIC` 和 atomic subtype。
- [x] 增加独立 atomic size decoder。
- [x] 扩展 16-byte PA geometry 和 128-bit value。
- [x] 增加主表最终化步骤，为所有原子 owner 的 required slot 补齐现有 `std_data_override_valid/std_data_override`。
- [x] CAS prefill 已冻结值保持不变；非-prefill 原子只在最终化阶段允许调用一次 operand generator。
- [x] 原子 issue 删除 required slot 的运行时 derive fallback，缺少冻结值时直接报错。
- [x] readonly Main view 逐槽复制冻结值，RM 不调用 driver derive helper。
- [x] input checker 在真实 STD fire 时按 owner+epoch+slot 比较 payload 与冻结值。
- [x] 增加单条 atomic WB 读取。
- [x] 实现 SWAP/ADD/XOR/AND/OR/MIN/MAX/MINU/MAXU。
- [x] 成功 AMO 更新 expected memory。
- [x] 后续 Load 能读取 AMO 新值。

### 阶段 2：LR/SC

- [x] 实现 LR old-value WB 检查。
- [x] 实现 `RM_RESV_NONE/RM_RESV_POSSIBLE` 状态和提交级转移。
- [x] 实现独立 SC 必须失败检查。
- [x] 实现 `sc_success_allowed = POSSIBLE && block_match`。
- [x] 实现 SC success 的前提、WB=0 和 memory write。
- [x] 实现 SC failure 的 WB=1，并在 commit 创建、在最终 `check_store` 完成 no-write obligation。
- [x] 实现 obligation `shadowed_mask/checked_mask` 和 overlay 不存在语义。
- [x] 实现正常 SC 即使 compare error 也清 reservation，正常 LR 即使 data compare error 也覆盖旧 reservation。
- [x] 实现 8.4 fault LR/SC 状态转移表。
- [x] 禁止使用 UID 距离、commit cycle 或 RM poll 次数模拟 reservation 超时。
- [x] 不把本 hart 同 block Store/AMO 当作必然 reservation invalidation。
- [x] 不增加 DCache reservation 微架构 checker。

### 阶段 3：CAS.W/D

- [x] 实现 compare/new operand 拼装。
- [x] 实现 CAS success/failure。
- [x] 实现 CAS.W WB 符号扩展。
- [x] 实现 CAS failure no-effect。

### 阶段 4：CAS.Q

- [x] PMA/PMP model 支持 16B atomic query。
- [x] 增加两条 atomic WB 批量读取。
- [x] 使用 pdest 区分 low/high WB。
- [x] 拼接 128-bit old/compare/new。
- [x] 一次性提交或拒绝完整 16-byte memory effect。

### 阶段 5：异常和最终 overlay

- [x] 覆盖 W/D/Q 非对齐异常。
- [x] 覆盖 TLB PF/GPF/AF。
- [x] 覆盖 PMA/PMP atomic permission fault。
- [x] 覆盖 PBMT-NC/IO 和 PMA-MMIO atomic AF。
- [x] 覆盖 denied/corrupt。
- [x] `check_store` 比较所有 atomic memory effects。

### 阶段 6：review 与回归

- [x] review RM 与 lifecycle checker 是否重复检查。
- [x] review expected 是否依赖 DUT actual。
- [x] review fault path 是否产生错误 memory effect。
- [x] review CAS.Q 是否存在部分提交。
- [x] 完成 directed unit test。
- [x] 完成 AMO smoke regression。
- [x] 完成普通 Load/Store regression，确认无回归。

## 17. 最小 Directed Test 集合

### 17.1 操作数冻结与 STD input checker

- 普通 AMO、LR、SC 进入 Main table 前 required mask 为 `0001`；
- 非-prefill CAS.W/D required mask 为 `0011`；
- 非-prefill CAS.Q required mask 为 `1111`；
- CAS prefill owner 的 compare/new 四槽保持规划值，不被最终化步骤覆盖；
- CAS.Q STD 顺序分别覆盖升序和非升序，实际 payload 始终按 slot 匹配冻结值；
- `ready=0` held 后再次发送，slot 和 payload 不改变；
- redirect 后同一 owner 重发仍使用同一冻结值；
- 人工交换 CAS compare/new 或 low/high payload 时，input checker 必须报错；
- required slot valid 缺失时在 Main table 接纳或 issue 前报错，不能运行时回退派生；
- readonly Main view 与 Main table 四槽逐项一致，RM 不调用 `derive_atomic_std_data()`。

### 17.2 普通 AMO

每个 W/D opcode 至少覆盖：

- old 为 0；
- old 为全 1；
- 符号边界；
- ADD 溢出截断；
- MIN/MAX 正负数组合；
- MINU/MAXU 高位数组合；
- AMO 后普通 Load 读新值；
- 连续两个 AMO 读取前一个 AMO 的新值。

### 17.3 LR/SC

- LR.W/LR.D 正常返回旧值；
- 独立 SC 失败且不写内存；
- 同 block LR/SC 返回 success 时严格检查写入；
- 同 block LR/SC 返回 failure 时允许隐藏失效，但必须证明无写；
- 不同 block SC failure；
- LR 后本地同 block Store/AMO 不作为 RM 必然失效条件；SC success/failure 都按合法结果集合检查；
- SC failure 后 reservation 清除；
- `POSSIBLE -> fault LR -> POSSIBLE`；
- `NONE -> fault LR -> NONE`；
- 正常 LR data compare 失败后仍覆盖旧 reservation，后续状态为该 LR 的 `POSSIBLE`；
- `POSSIBLE -> DCache 前 fault SC -> POSSIBLE`；
- `POSSIBLE -> denied/corrupt SC -> NONE`；
- 非法 SC result/非法 SC success 报错后，reservation 仍按已消费正常 SC 清除；
- SC failure obligation 在最终 `check_store` 全 byte 完成；
- SC failure 地址没有 DUT overlay entry 时按未写路径通过；
- 后续同地址合法写产生 `shadowed_mask`，该 byte 不计为已完整验证；
- LR fault 不建立 reservation；
- SC fault 不产生写效果。

### 17.4 CAS

- CAS.W/D compare equal；
- CAS.W/D compare unequal；
- CAS.W 旧值符号扩展；
- CAS.Q low 不同；
- CAS.Q high 不同；
- CAS.Q 完全相等；
- CAS.Q 两条 WB pdest 交换或缺失时 checker 报错；
- CAS.Q 不允许只写低 8B 或只写高 8B。

### 17.5 异常

- W/D/Q 非对齐；
- LR load PF/GPF/AF；
- SC/AMO/CAS store PF/GPF/AF；
- PMA atomic=0；
- PBMT-NC/IO；
- denied；
- corrupt；
- fault 后后续 Load 仍看到原内存值。

## 18. 完成标准

本方案 coding 完成需要同时满足：

1. 所有当前支持的 AMO opcode 都不再落入 `RM_LS_KIND_OTHER`。
2. 普通 AMO/LR/SC/CAS.W/D/Q 都具有独立 expected 计算。
3. CAS.Q 支持完整 16B geometry 和两次 WB。
4. 所有成功 atomic memory effect 都进入统一 expected memory。
5. 所有失败或 fault atomic 都不修改 expected memory。
6. 后续 Load/AMO 能看到前序 atomic 写入。
7. SC success 不允许在无 reservation 时通过。
8. SC success 只允许在 `RM_RESV_POSSIBLE` 且 block 匹配时通过。
9. SC failure 不允许修改内存，并具有可审计的 no-write obligation。
10. CAS failure 不允许修改内存。
11. W 型 WB 符号扩展与 RTL 一致。
12. atomic exception 同时与 WB 和 status 终态一致。
13. 现有 atomic lifecycle checker 职责不被 RM 重复实现。
14. 基础 RM 不使用提交时序近似 `lrsc_count`，也不增加 DCache reservation 微架构 checker。
15. 正常 SC 即使 compare 失败也不会给后续 UID 留下陈旧 reservation。
16. 正常 LR 即使 data compare 失败也会覆盖旧 reservation，不会让后续 UID 继续使用前一笔 LR 状态。
17. directed SC-failure 测试结束时不存在未完成或 shadowed 的 no-write obligation。
18. 随机场景所有未 shadowed obligation byte 都完成检查，shadowed byte 只报告 coverage limitation。
19. 普通 Load/Store RM 回归无行为退化。
20. 所有进入真实 Main table 的原子 owner 都具有与类型匹配的完整冻结 mask：普通 AMO/LR/SC=`0001`、CAS.W/D=`0011`、CAS.Q=`1111`。
21. CAS prefill 与非-prefill 原子都不在 issue、重发或 RM commit 阶段重新调用 `derive_atomic_std_data()`。
22. STD 任意发射顺序下，input checker 都按 slot 检查实际 payload；交换 compare/new 或 low/high 必须报错。
23. RM expected operand 只来自 readonly Main 冻结值，不能由 DUT STD/WB actual 反推。

## 19. 方案结论

AMO RM 适配不能只增加一个 ALU 函数。功能完整的最小闭环是：

```text
atomic 分类
  -> operand 冻结
  -> 地址/翻译/权限/异常
  -> old memory 构造
  -> AMO/LR/SC/CAS expected 计算
  -> 1/2 条 final WB 比较
  -> success/failure 与 memory effect 一致性
  -> expected memory 原子提交
  -> 后续访存和 check_store 统一验证
```

其中，原子架构结果由 RM 负责，STA/STD slot、barrier、feedback、held piece 和 redirect 生命周期继续由现有测试框架 checker 负责。LR/SC 使用 `NONE/POSSIBLE` 合法结果集合，不用延后的 ROB commit 时序模拟 DCache `lrsc_count`。本方案暂不增加 DCache reservation 微架构 checker，因此不验证精确超时窗口和 matching release/writeback 的失效时刻；该边界必须在日志和验收结果中明确保留，不能宣称已覆盖。分层后既能检查 AMO 最终功能，也不会把 AtomicsUnit 或 DCache 内部流水状态复制到抽象参考模型中。

## 20. 方案 Review 结论（2026-11-05）

本轮结合 AMO worktree 当前代码完成两轮 review，结论为：`可 coding`，Blocker=0，Major=0，Minor=0。

已核对的代码事实：

- `main_control_transaction` 已有 `std_data_override_valid/std_data_override`，无需增加重复主表字段；
- CAS prefill Store/owner 已冻结 compare/new，普通随机原子仍存在 issue-time derive fallback；
- random/AUTO/manual 建表都具有 `validate_main_table_entry()`、`set_main_transaction()` 接纳边界，可插入统一最终化和严格冻结校验；
- `lintsissue` monitor 能被动观察 STD0/1 的 valid、ready、payload、fuOpType、ROB value、SQ identity 和 pipe；
- STD wire 不携带完整 ROB flag/dynamic epoch，必须由同拍 owner ledger 绑定，不能从 wire 虚构这些字段；
- 普通 Store 的 `src_0` fallback 可保留，严格禁止 fallback 只作用于 atomic required slot。

Review 中已修正：

1. 将过时的“主表没有冻结字段”改为“只有 CAS prefill 已冻结，其他原子尚未统一冻结”；
2. 取消新增重复 `atomic_std_*` 主表字段，改为复用当前 override；
3. 明确最终化插在 companion 规划完成后、真实 Main table 接纳前，避免破坏 draft randomize；
4. 明确 CAS prefill 已冻结值不得被最终化覆盖；
5. 明确 actual STD payload 必须来自真实接口 monitor，held piece 只能提供 expected owner/slot；
6. 修正 STD wire 身份边界，dynamic epoch 属于框架 ledger，不是 DUT STD 字段；
7. 增加非-prefill CAS、重发、乱序、缺失 valid 和故意 slot 交换的 directed test；
8. 增加验收条件，保证 issue、重发和 RM commit 均不再运行时调用 atomic derive helper。

文档级检查通过：Markdown fence 配对、尾随空白检查和关键术语一致性检查均通过。本轮只修改方案文档，未执行编译或仿真。

## 21. Coding 与验证结果（2026-11-06）

实现已在 AMO worktree 完成，基础 stimulus/CAS prefill 基线提交为：

```text
2df71931d6a04c36fc40cb9f70b6c38193027243
mem_ut: implement AMOCAS compare prefill plan
```

本轮 RM/checker coding 覆盖：

- 普通 AMO、LR/SC、CAS.W/D/Q 的 expected 计算、异常和 memory effect；
- 16-byte PA geometry、128-bit CAS.Q 和两条 LDA0 final WB；
- `RM_RESV_NONE/RM_RESV_POSSIBLE` reservation、SC no-write obligation；
- 原子 STD operand 建表冻结和真实 issue payload 被动检查；
- CAS prefill Store frozen override，同时保留普通 Store `src_0` fallback；
- atomic required STA/STD mask 在 barrier 建立时冻结，并通过 sync snapshot、monitor transaction 传递到 RM，避免延迟采样绑定到新 owner/epoch 或可变 mask；
- SC 缺失 WB 时，normal、denied、corrupt 路径清 reservation，DCache 前 fault 路径保留 reservation。

验证结果：

| 项目 | 结果 |
| --- | --- |
| clean VCS compile | PASS，mode=`amo_rm_finalfix2_20261106` |
| directed software test | PASS，AMO ALU、LR/SC、CAS.W/D/Q、WB identity、fault path，`UVM_ERROR=0`、`UVM_FATAL=0` |
| real AMO smoke | PASS，8 笔 atomic RM compare 全部通过，`UVM_ERROR=0`、`UVM_FATAL=0` |
| CAS-only prefill smoke | PASS，6 笔 CAS compare 通过，其中 2 笔 CAS.Q 为 `wb_count=2`，`UVM_ERROR=0`、`UVM_FATAL=0` |
| ordinary Load smoke | PASS，`UVM_ERROR=0`、`UVM_FATAL=0` |
| ordinary Store smoke | PASS，`UVM_ERROR=0`、`UVM_FATAL=0` |
| final read-only review | Blocker=0，Major=0 |

后续补充的最终 `check_store` 与负向测试同样通过：

| 分类 | 覆盖内容 | 结果 |
| --- | --- | --- |
| 真实 DUT | AMOADD.D、同地址 LR.D/SC.D 成功、独立 SC.D 失败、AMOCAS.Q、非对齐 AMOADD.D | 5/5 PASS；全部 `UVM_ERROR=0`、`UVM_FATAL=0`，最终 `check_store` 通过 |
| soft/framework | missing-WB timeout、STD payload/pipe 负向检查、AMO.W/D 运算、LR/SC reservation、CAS.W/D/Q、异常无写效果 | 23/23 PASS；`UVM_ERROR=0`、`UVM_FATAL=0` |

测试用例清单已按项目规则分别归档：

- `AI_DOC/seq/memblock_amo_real_dut_testcases_20261106.csv`
- `AI_DOC/seq/memblock_amo_soft_framework_testcases_20261106.csv`

保留边界：

- 按方案不增加 DCache reservation 周期级 checker，因此不验证精确 `lrsc_count` 超时和 matching release/writeback 的失效时刻；
- missing-WB 的 directed test 当前直接覆盖 RM helper 语义，尚未增加真实 observer 256-poll timeout 的独立集成用例；
- required-mask 延迟消费竞态已由代码链路 review 确认关闭，后续可增加跨 barrier release 的 monitor FIFO 压力回归作为增强覆盖，不影响本方案完成状态。
