# mem_ut V2 DCache/Uncache D 通道错误权重注入专项 Plan

| 项目 | 内容 |
|---|---|
| 状态 | `undo`，仅完成方案设计，尚未 coding |
| 目标版本 | V2 |
| 当前分支 | `mem_ut_uvm_v2` |
| 测试框架入口 | DCache：`dcache_mem__access_base_sequence::accept_dcache_a_request()`；Uncache：`sbuffer_mem_access_base_sequence::sbuffer_mem_access_xaction()` |
| 主要修改文件 | `env/plus.sv`、`seq/base_seq_help/seq_csr_common.sv`、`seq/base_seq_help/mem_base_sequence.sv` |
| 创建日期 | 2026-08-03 |
| 方案原则 | 只在既有 pending D 回复建立时一次采样并保存错误字段；不建立 L2 directory、下游错误原因模型、RM 或 scoreboard。 |

## 1. 术语与抽象功能说明

| 术语 | 中文含义 | 代码对象或状态落点 | 典型场景 |
|---|---|---|---|
| `D error injection` | 测试框架在合法 D response 中可控地驱动 `denied/corrupt`，用于触发 MemBlock 自身错误消费路径 | DCache 的 `pending_d_denied/pending_d_corrupt` 或 Uncache 的 `rsp_xact` | A `AcquireBlock` 或 Uncache `Get` 已接受后建立带错误回复 |
| `pending D` | 已接受 A/C request、尚未完成 D handshake 的唯一回复快照 | `pending_d_*` | 两拍 `GrantData` 在 D backpressure 时保持相同错误字段 |
| `GrantData denied` | L2 拒绝该次数据获取 | `pending_d_denied=1` | 必须同时使 `pending_d_corrupt=1` |
| `GrantData corrupt` | 返回数据不可用，但请求未被拒绝 | `pending_d_denied=0`、`pending_d_corrupt=1` | 数据完整性错误激励 |
| `CBOAck error` | CBO 完成回复携带的拒绝或完整性错误 | `pending_d_kind=DCACHE_PENDING_D_CBO_ACK` | CMOUnit 把错误回传 LSQ，但仍完成 CBO FSM |
| `Uncache port` | MemBlock 到外部 TL-UL manager 的独立端口；历史 agent 名称为 `sbuffer_agent` | `auto_inner_buffers_out_a_*` / `auto_inner_buffers_out_d_*`、`sbuffer_mem_access_base_sequence` | MMIO/NC load 发 `Get`，收到 `AccessAckData` |
| `AccessAckData error` | Uncache data reply 的拒绝或数据错误 | `auto_inner_buffers_out_d_bits_denied/corrupt` | `Get` 回复前按 Uncache 权重一次采样；denied 命中时 corrupt 必须为 1 |
| `AccessAck error` | Uncache store reply 的拒绝状态 | `auto_inner_buffers_out_d_bits_denied` | `Put*` 回复；无数据 response 的 corrupt 必须为 0 |
| `weight` | 一个 `[0:100]` 的百分比权重；0 永不命中，100 每次均命中 | `plus -> seq_csr_common getter` | `WT=25` 表示每个合格采样机会 25% 命中 |

## 2. 当前问题、目标与边界

当前轻量 DCache responder 的 `pending_d_denied` 和 `pending_d_corrupt` 始终保持 0。因此
MemBlock 的 `GrantData` error 累积路径、CMOUnit 对 `CBOAck` error 的传递路径无法由当前
responder 定向触发。Uncache sequence 虽会从 `main_mem_access_task()` 获得后端错误位，但没有
独立、可控的 runtime error-injection 入口，也不能复用 DCache 的 `pending_d_*` 状态。

本 plan 的目标：

1. 使用六个 plus 权重独立控制 DCache `GrantData/CBOAck` 与 Uncache `AccessAckData/AccessAck`
   的 `denied/corrupt` 命中。
2. 对 `GrantData` 强制保持协议约束：`denied=1 -> corrupt=1`。
3. 错误值在 request 接受时只采样一次，写入 pending D 快照；D backpressure 和两拍
   `GrantData` 不得重新采样或改变字段。
4. 用接口身份而非 `source` 数值或 D opcode 猜测区分处理路径：`auto_inner_dcache_client_out_*`
   始终进入 DCache sequence，`auto_inner_buffers_out_*` 始终进入 Uncache sequence。
5. 保持现有 opcode、source、sink、cap、数据、`isKeyword`、Hint、Probe、GrantAck、CBO 和
   terminal 主体逻辑不变。

本轮不实现：

- `Grant`、`ReleaseAck`、B/C channel 的主动错误注入。
- `ReleaseAck.denied/corrupt` 非零驱动。该组合不合法，继续固定 0。
- 非法 Uncache opcode 的回复；仅既有白名单 `Get/PutFullData/PutPartialData` 可进入本 plan 的
  Uncache 分支。
- 对 payload 位翻转、ECC syndrome、L2 directory、CHI/downstream error cause 的建模。
- RM、scoreboard、coverage 或 error 后端异常结果判定。

## 3. 参数与配置链路

新增六个公共 runtime plus 参数，类型均为 `int`，默认值均为 0：

| 参数 | 合法范围 | 作用时机 | 语义 |
|---|---:|---|---|
| `MEMBLOCK_L2_GRANTDATA_DENIED_WT` | 0..100 | 每个已接受 `AcquireBlock` 建立 `GrantData` pending D 时 | `denied` 命中概率；命中时强制 `corrupt=1` |
| `MEMBLOCK_L2_GRANTDATA_CORRUPT_WT` | 0..100 | `GrantData denied` 未命中后 | 非拒绝 data-corrupt 概率 |
| `MEMBLOCK_L2_CBO_ACK_DENIED_WT` | 0..100 | 每个已接受 CBO A request 建立 `CBOAck` pending D 时 | `CBOAck.denied` 命中概率 |
| `MEMBLOCK_L2_CBO_ACK_CORRUPT_WT` | 0..100 | 每个已接受 CBO A request 建立 `CBOAck` pending D 时 | `CBOAck.corrupt` 命中概率 |
| `MEMBLOCK_UNCACHE_DENIED_WT` | 0..100 | 每个合法 Uncache D response 建立时 | `AccessAckData` 与 `AccessAck` 的 denied 命中概率 |
| `MEMBLOCK_UNCACHE_CORRUPT_WT` | 0..100 | 每个 `Get -> AccessAckData` 建立时 | 非拒绝 data-corrupt 命中概率；不适用于无数据 `AccessAck` |

参数实现沿用既有公共参数链路：

```text
env/plus.sv
  -> seq_csr_common::load_from_plus()
  -> seq_csr_common::validate_and_clamp()
  -> seq_csr_common::get_l2_*_wt()/get_uncache_*_wt()
  -> dcache_mem__access_base_sequence 或 sbuffer_mem_access_base_sequence
```

`validate_and_clamp()` 对每个权重执行 `[0:100]` fail-fast 检查；负数由现有
`get_non_negative_int()` 拒绝。不得直接在 sequence 中读取 `plus::*`。

配置示例：

```text
+MEMBLOCK_L2_GRANTDATA_DENIED_WT=10
+MEMBLOCK_L2_GRANTDATA_CORRUPT_WT=20
+MEMBLOCK_L2_CBO_ACK_DENIED_WT=5
+MEMBLOCK_L2_CBO_ACK_CORRUPT_WT=5
+MEMBLOCK_UNCACHE_DENIED_WT=10
+MEMBLOCK_UNCACHE_CORRUPT_WT=20
```

上述例子中，`GrantData` 的 `corrupt` 总出现率不等于固定 20%，因为 denied 命中时也会
强制 corrupt。`MEMBLOCK_L2_GRANTDATA_CORRUPT_WT` 精确定义为“未 denied transaction 的
独立 data-corrupt 概率”，避免 `denied=1/corrupt=0` 非法组合。
`MEMBLOCK_UNCACHE_CORRUPT_WT` 采用相同定义，但只作用于 `AccessAckData`；`AccessAck` 是无数据
response，协议要求其 corrupt 永远为 0，不能为了消费该权重而生成非法 response。

## 4. 文件修改与逻辑方案

### 4.1 参数定义、快照和 getter

修改文件：

```text
mem_ut/ver/ut/memblock/env/plus.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv
mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg
mem_ut/ver/ut/memblock/seq/plus_cfg/tc_dispatch_real_l2cache_model.cfg
```

修改内容：

1. `plus.sv` 添加六个 `MEMBLOCK_PLUS_ARGS_DEFINE(..., int, 0)` 和对应 `load_int()`。
2. `seq_csr_common` 添加六个 `int unsigned` runtime snapshot、`load_from_plus()` 赋值、
   `[0:100]` 合法性检查和六个只读 getter。
3. `default.cfg` 和 DCache L2 responder 专项 preset 显式配置六项为 0，保持旧回归行为。

### 4.2 单次加权采样 helper

修改文件：

```text
mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv
```

在公共父类 `mem_access_base_sequence` 新增 `sample_d_error_enable(weight, error_name)`，供 DCache
和 Uncache 两个派生 sequence 复用；不得让 `sbuffer_mem_access_base_sequence` 访问 DCache 子类 helper。

抽象功能：在一个已经确定的 D response 生命周期建立点，根据一个合法百分比权重返回一次
布尔选择；不驱动接口、不修改 pending 状态、不承担 opcode 分类。

文字伪代码：

```text
sample_d_error_enable(weight, error_name)：
  如果 weight == 0：返回 0；
  如果 weight == 100：返回 1；

  使用 std::randomize(enable) 和 dist：
    1 := weight；
    0 := 100 - weight；

  randomize 失败：uvm_fatal，并打印 error_name 和 weight；
  返回 enable；
```

该 helper 复用现有 `sample_hint_enable()`/`sample_probe_enable()` 的百分比随机模式；不新建
第二套随机数或 plus 读取机制。调用者只在 response 快照创建时调用一次，D ready hold 期间不调用。

### 4.3 `GrantData` 错误字段建立

修改位置：`dcache_mem__access_base_sequence::accept_dcache_a_request()` 的
`TL_A_OPCODE_ACQUIRE_BLOCK` 分支。

抽象功能：在 A.fire 已确认并准备好两拍 `GrantData` 快照时，确定该 transaction 的唯一
`denied/corrupt` 值；其后所有 D beat 只读取保存结果。

文字伪代码：

```text
建立 AcquireBlock 的 pending GrantData：
  pending_d_denied = sample_d_error_enable(GRANTDATA_DENIED_WT, "GrantData denied")；

  如果 pending_d_denied == 1：
    pending_d_corrupt = 1；
  否则：
    pending_d_corrupt = sample_d_error_enable(
      GRANTDATA_CORRUPT_WT,
      "GrantData corrupt"
    )；

  继续执行既有 data line snapshot、source/sink、cap、isKeyword 和 hint 排期；
  建立 pending_d_valid；

发送两个 GrantData beat：
  两拍均复用 pending_d_denied/pending_d_corrupt；
  D.ready=0 时不调用采样 helper，也不更新错误字段；
```

此逻辑不改变 `GrantData` 的 data payload。`corrupt=1` 仅表示该 payload 对 DUT 不可信，
由 MemBlock 自身 error path 消费；测试框架不额外翻转 data 位。

### 4.4 `CBOAck` 错误字段建立

修改位置：`accept_dcache_a_request()` 的 `CBOClean/CBOFlush/CBOInval` 分支。

抽象功能：在已接受的 CBO request 建立单拍 `CBOAck` pending D 时，独立生成 denied 和 corrupt，
但始终保留同 source 的 Ack，使 DUT CMOUnit 能结束等待。

文字伪代码：

```text
建立 CBOAck pending D：
  pending_d_denied = sample_d_error_enable(CBO_ACK_DENIED_WT, "CBOAck denied")；
  pending_d_corrupt = sample_d_error_enable(CBO_ACK_CORRUPT_WT, "CBOAck corrupt")；

  保存原 CBO source、size、opcode、line；
  发送单拍 CBOAck；
  CBOAck.fire 后继续已有 clean/flush/inval 地址表处理；
```

`CBOAck` 的两个字段不建立强制蕴含关系。DCache CMOUnit 分别保留它们，并以
`denied || corrupt` 形成错误结果，因此允许 denied-only、corrupt-only 或两者同时命中。

### 4.5 保持既有 DCache builder 与生命周期

`build_pending_d_xaction()` 已统一从 `pending_d_denied/pending_d_corrupt` 驱动 D fields，
因此不新增 driver、interface、transaction 字段或 second response builder。需要确保：

```text
clear_pending_d_state()：继续清两字段为 0；
AcquirePerm Grant：继续保持两字段为 0；
ReleaseAck：继续保持两字段为 0；
process_d_fire()：不按错误位提前清 pending，不跳过 GrantAck owner，不改变 CBOAck 完成时机；
```

### 4.6 Uncache D error 分流、采样和格式归一化

修改位置：`sbuffer_mem_access_base_sequence::sbuffer_mem_access_xaction()` 新增对
`apply_uncache_d_error_injection()` 的一次调用；该 helper 与公共 `sample_d_error_enable()` 均声明在
`mem_access_base_sequence`。不修改 `dcache_mem__access_base_sequence` 的 pending D 状态，也不通过
`source` 数值在两个端口之间反查或转发 response。

本 plan 与 `mem_ut_dcache_uncache_response_delay_control_plan_20260730.md` 的职责边界固定为：

```text
Uncache response-delay plan：
  opcode 白名单、A/D 生命周期、response record/ready queue、延迟和 D ready hold；

本 plan：
  MEMBLOCK_UNCACHE_DENIED_WT/MEMBLOCK_UNCACHE_CORRUPT_WT、一次随机采样、
  AccessAckData/AccessAck 的 denied/corrupt 格式归一化；

两项同时实施时：
  decode_uncache_a_opcode() 先给出 STORE_ACK/LOAD_DATA，
  本 plan 再只为该已确定 response kind 建立错误字段。
```

实施顺序约束：在当前直接回复实现中，唯一调用点是 `sbuffer_mem_access_xaction()` 创建 `rsp_xact`
之后、首次 `send_sbuffer_xaction()` 之前；若 response-delay plan 已将该路径重构为 response record，
调用点随之迁移到 `create_uncache_response_record()` 创建 record 时。两种实现形态只能保留一个调用点；
不得在 record 创建时采样后，又在 driver 从 record 取出 D payload 时再次采样。

抽象功能描述：`apply_uncache_d_error_injection()` 在已接受的 Uncache A request 完成既有 memory
access 后，给将要建立的单个 `AccessAckData` 或 `AccessAck` xaction 采样 error bit，并按 D opcode
归一化为协议合法字段。它接收 backend error 和 response kind，返回 D payload 的 denied/corrupt；
不访问主存、不修改 A ready、D opcode、source、data、LSQ 状态或 terminal。调用者把返回值写入当前
`rsp_xact`；该对象随后由既有 `D.ready` 重试循环保持，不会重新随机。

接口归属必须固定为：

```text
dcache_agent / auto_inner_dcache_client_out_*：
  DCache A/D/E responder；只使用 existing pending_d_* 和四个 MEMBLOCK_L2_*_WT；

sbuffer_agent / auto_inner_buffers_out_*：
  Uncache TL-UL responder；只使用 sbuffer_mem_access_xaction() 和两个 MEMBLOCK_UNCACHE_*_WT；

禁止：根据 source、地址、D opcode 或 is_store 把一条已经属于 Uncache 端口的 transaction
      送到 DCache pending D builder，或反向送回 Uncache sequence。
```

文字伪代码：

```text
apply_uncache_d_error_injection(kind, backend_denied, backend_corrupt,
                                 output d_denied, output d_corrupt)：
  inject_denied = sample_d_error_enable(
    get_uncache_denied_wt(), "Uncache denied"
  )；
  raw_denied = backend_denied || inject_denied；

  如果 kind == LOAD_DATA，即 Get -> AccessAckData：
    inject_corrupt = sample_d_error_enable(
      get_uncache_corrupt_wt(), "Uncache corrupt"
    )；
    raw_corrupt = backend_corrupt || inject_corrupt；
    d_denied  = raw_denied；
    d_corrupt = raw_corrupt || raw_denied；
    返回；

  如果 kind == STORE_ACK，即 Put* -> AccessAck：
    若 backend_corrupt == 1：
      uvm_fatal；当前无数据 AccessAck 不能承载 corrupt；
    d_denied  = raw_denied；
    d_corrupt = 0；
    返回；

sbuffer_mem_access_xaction(req_xact, rsp_xact)：
  kind = 既有 Uncache opcode 白名单的结果；
  调用既有 sbuffer_mem_access_task()，得到 backend_denied/backend_corrupt/load_data；
  调用 apply_uncache_d_error_injection(
    kind, backend_denied, backend_corrupt, d_denied, d_corrupt
  )；
  按既有规则建立 rsp_xact，并写入 d_denied/d_corrupt、AccessAckData 或 AccessAck opcode；

  保持既有 source/size/sink/data/pre_pkt_gap/post_pkt_gap 赋值；
  返回同一个 rsp_xact；

D.ready == 0：
  既有 do-while 重发同一个 rsp_xact；
  不再次调用 sbuffer_mem_access_task() 或 sample_d_error_enable()；
```

`MEMBLOCK_UNCACHE_DENIED_WT` 在合法 `AccessAckData` 和 `AccessAck` 上均可生效。前者按
`denied -> corrupt` 实际驱动 `1/1`；后者按无数据 response 规则实际驱动 `1/0`。该 standalone
responder 在 denied 权重大于 0 时承担 `mayDenyGet/mayDenyPut` 外部 manager 契约；若后续引入
TileLink Monitor 或连接模型并固定 `mayDenyGet/mayDenyPut=0`，初始化时必须对非零 denied 权重
`uvm_fatal`，不能产生与该静态 capability 冲突的 stimulus。

`MEMBLOCK_UNCACHE_CORRUPT_WT` 只在 `AccessAckData` 上采样。它与 denied 权重均在同一 response
建立点独立采样；若 denied 命中，最后的 `corrupt` 强制置 1。`AccessAck` 不采样 corrupt 权重，
否则会为无数据 D response 制造协议违规输入。

## 5. 与原有轻量 responder 的对比

| 项目 | 原有逻辑 | 修改后逻辑 | 修改原因 |
|---|---|---|---|
| DCache `GrantData.denied/corrupt` | 始终为 0 | 在 DCache A.fire 时按两个权重采样；denied 命中强制 corrupt=1 | 覆盖 MemBlock refill/forward error 消费路径，同时保持合法错误组合 |
| DCache `CBOAck.denied/corrupt` | 始终为 0 | 在 DCache CBO A.fire 时按两个独立权重采样 | 覆盖 CMOUnit 到 LSQ 的 error 传递路径，但不模拟 L2 错误成因 |
| Uncache `AccessAckData.denied/corrupt` | 只继承 memory backend 的错误位，无 runtime 权重 | `Get` response 建立时按两个 Uncache 权重与 backend error 合并；denied 命中强制 corrupt=1 | 支持 NC/MMIO load 的 access fault 与 hardware error stimulus，同时保持 data response 格式合法 |
| Uncache `AccessAck.denied/corrupt` | 只继承 memory backend 的错误位 | `Put*` response 建立时按 denied 权重与 backend denied 合并；corrupt 固定 0 | 支持 store access-fault stimulus，避免无数据 response 的 corrupt 协议违规 |
| 接口路由 | responder 名称容易把 `sbuffer_agent` 当作 DCache 内部路径 | `auto_inner_dcache_client_out_*` 固定走 DCache；`auto_inner_buffers_out_*` 固定走 Uncache | 防止复用错误的 pending、source/sink 和 D opcode 生命周期 |
| D backpressure/多 beat | pending D 已保存全部字段 | 不变；错误字段同样来自 pending 快照 | 防止 valid 等待期间或第二 beat 出现随机值变化 |
| `Grant`、`ReleaseAck`、B/C | 正常值与既有生命周期 | 不变 | 限制本专项范围，避免将协议保留/非法组合当作错误注入 |
| 主表、TLB、LSQ、RM、scoreboard | 不参与 L2 responder 的 error 位生成 | 不变 | 本专项只扩展 responder 的合法 D stimulus |

## 6. 文档同步

coding 时同步更新：

- `AI_DOC/analysis/interface/v2/agents/dcache_agent.md`：记录六个权重、DCache/Uncache 两个端口的
  route、`GrantData/AccessAckData` 蕴含规则、`AccessAck` 固定 corrupt=0 和 `CBOAck` 独立错误语义。
- `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_l2cache_response_hint_probe_model_coding_plan_20260717.md`：
  在已实现专项后追加本 plan 的增量链接，不改写其原始归档结论。
- `AI_DOC/plan/test_framework/plan/do/mem_ut_dcache_uncache_response_delay_control_plan_20260730.md`：
  改为引用本 plan 的 Uncache error 参数与格式归一化，保留其 opcode、延迟和 hold 所有权，避免两个
  plan 分别要求实现同一个 helper。
- `AI_DOC/project_management/mem_ut_parameter_management.md` 与
  `mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md`：登记新增公共 runtime 参数及 consumer。

本 plan 不修改 `AI_DOC/mem_ut_flow_doc`；只有完成 coding、形成稳定调用链后才按实际代码刷新对应
DCache/Uncache responder flow 文档。
