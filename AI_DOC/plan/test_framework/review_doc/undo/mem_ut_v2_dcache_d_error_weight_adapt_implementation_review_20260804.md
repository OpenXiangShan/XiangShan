# mem_ut V2 DCache/Uncache D-error 权重注入实施 Review

| 项目 | 结论 |
|---|---|
| 关联 plan | `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_dcache_d_error_weight_adapt_plan_20260803.md` |
| Review 范围 | DCache coherent D、Uncache TL-UL D 的 `denied/corrupt` runtime stimulus、参数链路与文档同步 |
| Review 结论 | 通过。未发现阻断问题；实现只扩展 response record 的合法 D payload，不改变主表或 LSQ 主控制。 |
| Review 日期 | 2026-08-04 |

## 1. 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 对应代码对象或落点 | 使用场景/示例 |
|---|---|---|---|
| `response record` | 已经真实接受 request、等待最后一个 D handshake 的回复快照 | `dcache_response_record_t`、`uncache_response_record_t` | `AcquireBlock` 建立两拍 GrantData record，最后一个 D.fire 后释放。 |
| `D-error snapshot` | 一笔 response 固定的 `denied/corrupt` 值 | 两种 response record 的同名字段 | D.ready=0 或 GrantData 第二 beat 到来时仍使用首拍决定的值。 |
| `D hold` | 已被 scheduler 选中但尚未完成 D.fire 的唯一 D payload | `current_d_record/current_d_valid` | DUT 拉低 D.ready 时保持 source、data 和错误位。 |
| `backend error` | shared-memory access 返回的原始 `denied/corrupt` | `sbuffer_mem_access_task()` 输出 | Uncache Get/Put 在 runtime error 注入之前已经得到的后端结果。 |
| `normalization` | 按当前 D opcode 把原始/随机错误收敛为合法字段组合 | `apply_uncache_d_error_injection()` | AccessAckData 的 denied 强制 corrupt；AccessAck 的 corrupt 固定 0。 |

## 2. Review 范围与结论

抽象功能描述：本专项只给已经建立的 DCache/Uncache response record 增加一次性错误快照。它不负责
构造下游 L2 错误原因、不根据 uid 反查主表，也不直接推进 fault、writeback、commit/deq、pass/fail 或
terminal。

本 agent 逐项复查了参数定义/加载/getter、DCache GrantData/CBOAck record 创建、Uncache record
归一化、D hold 字段搬运、默认 cfg、历史文档和 TODO。所有六个参数均有默认值、范围检查、唯一
consumer 和可追踪文档；未发现二次采样或把 Uncache response 路由至 DCache owner 的情况。

## 3. 参数链路 Review

| 参数组 | `plus.sv` 默认 | `seq_csr_common` 处理 | 唯一 consumer |
|---|---:|---|---|
| GrantData denied/corrupt | 0/0 | 非负读取、`[0:100]` fail-fast、只读 getter | `accept_dcache_a_request()` 的 AcquireBlock record。 |
| CBOAck denied/corrupt | 0/0 | 同上 | `accept_dcache_a_request()` 的 CBOAck record。 |
| Uncache denied/corrupt | 0/0 | 同上 | `create_uncache_response_record()` 的归一化 helper。 |

`default.cfg` 与 `tc_dispatch_real_l2cache_model.cfg` 均显式写入六个 0，维持历史 normal
smoke。公共 sequence 不直接读取 `plus::*`，只通过 `seq_csr_common` getter 使用最终快照，符合参数
管理规则。

## 4. 源码逻辑 Review

### 4.1 单次加权选择

抽象功能描述：`sample_d_error_enable()` 由 DCache 和 Uncache responder 在 response record 创建时
调用，输入一个已验证的百分比权重，输出一次布尔选择。它不驱动接口、不修改 queue，也不负责 opcode
分类。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，函数：`sample_d_error_enable()`。

```systemverilog
if (weight == 0) begin
    return 1'b0;
end
if (weight == 100) begin
    return 1'b1;
end
if (!std::randomize(enable) with {
        enable dist {
            1'b1 := weight,
            1'b0 := (100 - weight)
        };
    }) begin
    `uvm_fatal(get_type_name(),
               $sformatf("failed to randomize %s with weight=%0d", error_name, weight))
end
return enable;
```

中文伪代码：该逻辑先把 0 和 100 两个确定场景直接返回，避免无意义的随机化。其余权重按
`weight : 100-weight` 选择真假；随机化失败立即 fatal，不能静默把错误 response 变成正常 response。
调用者只在建立 record 时调用本函数，函数本身不保存状态，因此 D hold 和第二个 GrantData beat 不会
触发重新采样。

### 4.2 DCache GrantData/CBOAck record

抽象功能描述：`accept_dcache_a_request()` 在确认 coherent A.fire 后建立一条语义固定的 DCache
response record。它给 GrantData 保存跨两拍稳定的错误组合，给 CBOAck 保存独立错误组合；scheduler、
sink、Hint 和 GrantAck 生命周期仍由既有 owner 管理。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，task：`accept_dcache_a_request()`。

```systemverilog
response_record.denied = sample_d_error_enable(
    seq_csr_common::get_l2_grantdata_denied_wt(),
    "DCache GrantData denied"
);
response_record.corrupt = response_record.denied ? 1'b1 : sample_d_error_enable(
    seq_csr_common::get_l2_grantdata_corrupt_wt(),
    "DCache GrantData corrupt"
);
```

中文伪代码：在 AcquireBlock 已经取得 line data、分配 sink 并准备入队时，先按 GrantData denied
权重生成一次 denied。若命中，立即把同一 record 的 corrupt 固定为 1；若未命中，才按 corrupt 权重
生成一次 corrupt。record 后续经过 delay、乱序选择、D hold 和两个 D beat 时只读取该保存结果，所以
不会产生 `denied=1/corrupt=0` 或两拍字段不一致。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，task：`accept_dcache_a_request()`。

```systemverilog
response_record.denied = sample_d_error_enable(
    seq_csr_common::get_l2_cbo_ack_denied_wt(),
    "DCache CBOAck denied"
);
response_record.corrupt = sample_d_error_enable(
    seq_csr_common::get_l2_cbo_ack_corrupt_wt(),
    "DCache CBOAck corrupt"
);
```

中文伪代码：在合法 CBO A.fire 建立单拍 CBOAck record 时，分别采样 denied 和 corrupt，不建立
GrantData 的蕴含关系。无论错误位是否命中，CBOAck 都保留原 source 并在真实 D.fire 后执行原有
clean/flush/inval 的 cached-line 动作，因此不会因错误注入让 CMO FSM 丢失 completion。

### 4.3 Uncache opcode 归一化

抽象功能描述：`apply_uncache_d_error_injection()` 接收已经完成 memory access 的 backend error 和
已解码 response kind，输出当前 TL-UL D opcode 可合法承载的字段。它不重做 memory access、不修改
response delay，也不操作 LSQ 状态。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，函数：`apply_uncache_d_error_injection()`。

```systemverilog
d_denied = backend_denied || inject_denied;
case (response_kind)
    UNCACHE_RESPONSE_LOAD_DATA: begin
        inject_corrupt = sample_d_error_enable(
            seq_csr_common::get_uncache_corrupt_wt(), "Uncache corrupt"
        );
        d_corrupt = backend_corrupt || inject_corrupt || d_denied;
    end
    UNCACHE_RESPONSE_STORE_ACK: begin
        if (backend_corrupt) begin
            `uvm_fatal(get_type_name(), "Uncache AccessAck cannot carry backend corrupt=1")
        end
    end
endcase
```

中文伪代码：函数先把 backend denied 与 runtime denied 合并。对于 Get 的 AccessAckData，再采样
runtime corrupt，并把 backend corrupt、runtime corrupt 和 denied 合并为 corrupt，保证 denied 时
固定输出 `1/1`。对于 Put 的无数据 AccessAck，若 backend 试图给出 corrupt 则立即 fatal；否则保留
合并后的 denied、维持 corrupt=0。这样不把 data-only 的 corrupt 错误位错误地驱到 store ack。

### 4.4 记录创建与 D hold 字段搬运

抽象功能描述：`create_uncache_response_record()` 是 Uncache response record 的唯一创建点；它在
真实 A.fire 后调用 backend 和归一化 helper，再把结果存入 queue。`build_current_uncache_d_xaction()`
只把 current record 映射为本拍 D payload，不作随机或重读。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，task：`create_uncache_response_record()`。

```systemverilog
apply_uncache_d_error_injection(
    response_kind,
    denied,
    corrupt,
    response_record.denied,
    response_record.corrupt
);
uncache_rsp_q.push_back(response_record);
```

中文伪代码：真实 A.fire 后，先由现有 backend 完成一次读或写，再把 response kind、source、size、
address、data 和归一化后的错误位写入同一 record 并入队。scheduler 选中后将该 record 移到 current
D hold；因为后续只复制保存字段，D.ready=0 或乱序调度不能改变已经确定的错误结果。

## 5. 与原逻辑的差异

| 对象 | 原有逻辑 | 当前逻辑 | 修改原因 |
|---|---|---|---|
| DCache GrantData | `denied/corrupt` 恒为 0 | response record 创建时一次采样；denied 蕴含 corrupt | 生成合法 refill/forward error stimulus，同时保持多 beat 稳定。 |
| DCache CBOAck | `denied/corrupt` 恒为 0 | 两个权重独立采样，但仍正常返回 Ack | 覆盖 CMO error consumer，不改变 CBO completion owner。 |
| Uncache AccessAckData | 仅保留 backend error | backend 与两项 runtime 权重合并；denied 蕴含 corrupt | 支持 NC/MMIO load error stimulus，避免非法 D 组合。 |
| Uncache AccessAck | 仅保留 backend error | backend/runtime denied 合并；corrupt 固定 0 | 无数据 response 不能承载 corrupt。 |
| 主框架状态 | responder 不写主表/LSQ | 保持不写主表/LSQ | 避免 responder 在没有稳定 uid 所有权时篡改 commit、terminal 或异常流。 |

## 6. 文档与 TODO Review

已同步 V2 DCache 接口说明、DCache/Uncache responder flow、`mem_base_sequence`/`plus` 源码分析、
参数管理规则和 plus 迁移说明。历史 DCache response/sideband plan 仅追加后续实施注记，不改写其
历史结论。已从总 TODO 文档删除完成的 DCache/SBuffer `corrupt/denied` response 注入条目，并把 scalar
fault TODO 改为“只消费 DUT 可观察事件”的当前边界。

## 7. 验证与静态检查

| 检查 | 结果 |
|---|---|
| `git diff --check` | 通过。 |
| 六参数链路检索 | 每项均存在 `plus` 定义/加载、`seq_csr_common` snapshot/getter、两个 cfg 默认项及唯一 record consumer。 |
| default real smoke | `memblock_dispatch_real_smoke_vseq`，`TEST_PASS`、`UVM_ERROR=0`、`UVM_FATAL=0`。 |
| 显式权重启动 | 六个权重均覆盖为 100 的 `virtual_base_sequence`，`TEST_PASS`、`UVM_ERROR=0`、`UVM_FATAL=0`。 |

显式权重启动验证覆盖参数解析、范围检查与完整编译；它不主动生成 DCache/Uncache request，因此不是
“DUT 对 nonzero error 的异常路径已验收”。该后续结果需要独立 directed testcase、预期异常/terminal
策略和必要 checker，仍不属于本 responder 专项。

## 8. Plan 对齐检查

关联 plan 已逐项复查。执行前原 plan 的参数、一次采样、DCache/Uncache opcode 约束、默认关闭和
不接管主框架状态均已落实。

### 8.1 实现与 Plan 不一致项

存在两项有意执行中修正，均已写入 plan 的 `IMPLEMENTATION_DELTA`：

| Plan 原有逻辑 | 当前源码/文档逻辑 | 不一致原因 | 落点 |
|---|---|---|---|
| 使用旧 `pending_d_*`/直接 `rsp_xact` 描述保存位置 | 使用已经落地的 DCache/Uncache response record | response-delay 专项先完成，恢复旧 pending owner 会造成两套生命周期 | `accept_dcache_a_request()`、`create_uncache_response_record()`。 |
| flow 文档待 coding 稳定后再更新 | coding 完成后同步两个 responder flow | 防止有效 flow 继续误写 D-error 为未来 TODO | 两个 `AI_DOC/mem_ut_flow_doc/dcache_*` 文件。 |

第一项的源码和中文伪代码见第 4.2、4.3、4.4 节：DCache/Uncache 都在 response record 创建点写入
错误快照，后续 queue、scheduler 和 D hold 只读取该快照。第二项没有 SV 逻辑调整，文字伪代码为：
“coding 稳定后，用实际 record owner、一次采样时点和 opcode 规则替换 flow 中的 future TODO；不新增
主表 consumer 或 driver 行为”。

### 8.2 Plan 未说明但 Coding 落实的细节

无。公共父类 `sample_d_error_enable()` 已由原 plan 第 4.2 节明确要求；它让 DCache 与 Uncache 使用
同一百分比语义，但不属于计划外补充。

## 9. 最终结论与剩余边界

本轮 implementation review 通过。D-error stimulus 的真源是 response record，不是主表 uid 或 D hold；
默认值关闭，normal smoke 保持兼容。剩余边界为 nonzero D-error 的 DUT 异常功能验收、RM/scoreboard、
覆盖率、L2 directory/downstream cause 和 C/B/ReleaseAck 主动错误注入，均未被本提交隐式实现。

## 后续 CBO Probe closure 注记

2026-08-04 后续 CBO 专项已把命中 CBO 的 response record 创建延后到 Probe C completion。本 review 中
“CBOAck record 创建”均应理解为 CBO A.fire 的一次性 error snapshot：miss 当场转 record，hit 先保存到
`cbo_context`，Probe 完成后再复用同一快照转 record。两项 CBO error 权重、独立采样和正常 CMO Ack
语义不变，不允许在延后建 record 时二次随机。
