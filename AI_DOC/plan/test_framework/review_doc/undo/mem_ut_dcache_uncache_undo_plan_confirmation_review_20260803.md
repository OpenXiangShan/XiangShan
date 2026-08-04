# DCache/Uncache Undo Plan 待确认项与交叉一致性 Review

## 1. 术语与抽象功能说明

| 术语 | 当前含义 | 对应计划对象 | 使用场景 |
|---|---|---|---|
| `response record` | 测试框架保存一笔尚未完成最后一个 D beat 的响应状态 | DCache/Uncache response queue | Grant、CBOAck、ReleaseAck 或 AccessAck 等响应等待 D.fire |
| `Grant sink` | Grant/GrantData 与后续 E GrantAck 关联的独立标识资源 | DCache sink pool、`grant_ack_wait` | 最后一个 Grant D.fire 后仍保留到 E.fire |
| `Probe C param` | DCache 对 B Probe 实际权限转换结果的 TileLink 回报 | `ProbeAck/ProbeAckData.param` | `TtoB`、`BtoB`、`NtoN` 等 |
| `capacity reservation` | 在接收可能产生后续响应的第一拍提前占用 response record | ReleaseData 首拍状态 | 防止第二拍到来时 response 容量被其他请求抢占 |
| `overlay` | 保存 DUT 已确认写入、按 byte valid 管理的共享写覆盖层 | `write_overlay_mem` | merged read 优先读取已提交覆盖字节 |
| `backing` | 保存懒初始化原始数据的基础内存 | `main_mem` | overlay 未命中时提供初始数据 |

抽象功能说明：本 review 对六份 DCache/Uncache `undo` plan 做交叉检查，确认用户需要决定的行为边界、
跨 plan 状态所有权以及可直接采用的默认方案。本文不实现测试框架代码，也不替代各专项 plan 的 coding 细节。

## 2. Review 范围与结论

审查文件：

- `AI_DOC/plan/test_framework/plan/do/mem_ut_dcache_main_mem_range_switch_plan_20260730.md`
- `AI_DOC/plan/test_framework/plan/undo/mem_ut_dcache_multi_probe_alias_state_plan_20260803.md`
- `AI_DOC/plan/test_framework/plan/undo/mem_ut_dcache_multi_probe_tob_control_plan_20260730.md`
- `AI_DOC/plan/test_framework/plan/do/mem_ut_dcache_uncache_response_delay_control_plan_20260730.md`
- `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_dcache_cbo_probe_closure_plan_20260731.md`
- `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_dcache_d_error_weight_adapt_plan_20260803.md`

第一轮 review 结论：发现四项必须修正的计划问题；未发现必须由用户在行为上二选一的新问题。默认值、
Probe 默认关闭、DCache/Uncache 默认 `1..10` cycle、默认顺序返回、各通道 16 笔 response record 已有明确方案。

## 3. 必须修正的问题与建议

### 3.1 `Probe(toB/toN)` 的 `NtoN` 被错误拒绝

涉及：

- `mem_ut_dcache_multi_probe_alias_state_plan_20260803.md` 的 C Probe reply 分支。
- `mem_ut_v2_dcache_cbo_probe_closure_plan_20260731.md` 的 CBO Probe response 分支。

问题：两个 plan 当前只接受目标权限对应的 `Tto*`/`Bto*`。V2 TileLink 的 `NtoN` 是合法 C report，
表示 Probe 真正到达 DCache 时该副本已经不存在。它不能作为随机生成的普通结果，但也不能被当成协议非法
而卡住 Probe、CBO、flush 或 alias deferred Acquire。

推荐修改：

```text
C Probe param 为 NtoN：
  按 TileLink 合法回包接受；
  对应软件 line record 报 uvm_error 并失效/删除；
  释放 Probe record、C assembly 和对应 owner；
  继续解除 CBO、flush 或 alias deferred Acquire 等等待；
  不把 NtoN 加入随机 response 选择，也不提供 plus 参数让用户决定是否接受。
```

该问题是协议回包处理逻辑错误，不是用户配置项。

### 3.2 `ReleaseData` 两拍接收缺少 response capacity 预留

涉及：`mem_ut_dcache_uncache_response_delay_control_plan_20260730.md` 的 DCache C response 准入 flow。

问题：`ReleaseData` 的第一拍 C.fire 后，第二拍仍必须属于同一笔 C transaction；但当前描述只说明
C.fire 时检查 response record 是否有空位。如果第一拍之后其他 A/C request 占满 16 笔，第二拍可能无法
维持完整 assembly 和后续 ReleaseAck，造成半条 C transaction。

推荐修改：

```text
第一拍 ReleaseData C.fire：
  立即申请并锁定一个 ReleaseAck response record；
  保存 C assembly token 和 ReleaseAck owner；

第二拍 ReleaseData C.fire：
  只允许匹配同一 assembly token；不重新申请容量；

ReleaseAck D.fire：
  释放 ReleaseAck response record。
```

如果 16 笔 capacity 已满，第一拍 `ReleaseData` 不应 fire，必须保持 `C.ready=0`。这不会占用 Grant sink。

### 3.3 Grant sink pool 缺少权威容量定义

涉及：`mem_ut_dcache_uncache_response_delay_control_plan_20260730.md` 的 `Dynamic Sink Flow`。

问题：plan 已要求动态 sink，但只写了抽象 `sink pool`，没有定义 sink 数量的 V2 compile-time 权威来源。
如果 coding 直接假定 sink 为 16，可能把 DCache response record 容量错误当成 sink 容量；二者生命周期不同：
response record 在最后一个 D.fire 释放，sink 要到 E.fire 才释放。

推荐修改：

- 从 V2 实际 L2/D response sink 资源确认 sink 数量。
- 用 `MEMBLOCK_DUT_DCACHE_GRANT_SINK_NUM` 作为 compile-time capability 宏。
- Acquire 同时检查共享 response record 和 sink pool；CBOAck/ReleaseAck 只检查 response record。
- sink 耗尽时只阻塞需要 Grant 的 Acquire，不阻塞 CBO/Release response。
- 不新增 runtime plus 镜像，不让 testcase 随意改变硬件 sink 容量。

这是 coding 前必须核对的硬件能力项，不是用户行为选择项。

### 3.4 `ReleaseData.corrupt` 的数据面规则未定义

涉及：`AI_DOC/plan/test_framework/plan/do/mem_ut_dcache_main_mem_range_switch_plan_20260730.md` 的覆盖层写入边界。

问题：plan 已明确 `ProbeAckData.corrupt=1` 不写 overlay，但没有同样规定 `ReleaseData.corrupt=1`。
如果损坏的 ReleaseData 仍写入 shared overlay，后续 merged read 会观察到不可靠数据。

推荐修改：

```text
完整 ReleaseData 且 corrupt=0：写入 overlay；
完整 ReleaseData 且 corrupt=1：不写 overlay，记录 uvm_error；
无论 corrupt：继续完成两拍 C assembly，并建立/发送 ReleaseAck；
ReleaseAck D.fire 后释放共享 response record。
```

`main_mem` 仍不得被 DUT 写覆盖；既有可靠 overlay 数据也不能因一次 corrupt 数据被清除。

## 4. 可按默认方案直接补齐的边界

### 4.1 无数据 C response 的错误字段

`ProbeAck`、无数据 `Release` 的 `corrupt` 应按 V2 TileLink 字段规则检查。若观测到协议允许的错误
表示，记录错误但继续对应 Probe/Release/CBO/flush 生命周期；未知或违反接口协议的 X/Z 仍沿用既有
fail-fast 检查。该项不需要用户增加开关。

### 4.2 `data_valid` 与 overlay 的独立性

`cached_line_record.data_valid=0` 只表示当前 Probe 返回数据不可靠，不代表已提交 overlay 必须被删除，
也不允许把 `main_mem` 改成当前 DUT 写值。后续 merged read 继续按 byte-valid overlay 合并 backing 数据。

## 5. 用户确认项

本轮未发现必须由用户确认的行为分歧。以下内容已按默认方案固定：

- `main_mem_ranges_en=1` 默认限制 DCache/Uncache 地址，关闭后允许物理地址位宽内懒分配。
- DCache/Uncache 默认返回延迟为 `1..10` cycle。
- 默认 `ORDERED`，显式开启才允许 `REORDER`。
- Probe 默认关闭，启用后按既有权重选择 batch、数量和 `toB/toN`。
- DCache 与 Uncache 各自维护 16 笔 response record。
- DCache `Grant/GrantData`、`CBOAck`、`ReleaseAck` 共用 16 笔 response record；Grant sink 单独到 E.fire。

## 6. 后续 TODO 边界

以下不属于本轮用户确认项，也不阻塞当前轻量 responder 适配：

- 完整 CoupledL2 directory、多个 client 的一致性和替换策略。
- 动态 `needData`、真实 dirty owner 和完整 Probe/Release 数据所有权模型。
- 多 CBO context、完整 CHI 下游事务和 L2 全量 set/way flush。

## 7. Review 记录

| 轮次 | 方式 | 结果 |
|---|---|---|
| 第 1 轮 | 独立 subagent 只读交叉审查六份 plan | 发现 4 项必须修正问题，已记录于第 3 章 |
| 第 2 轮 | 基于本文基线的独立 subagent 只读复查 | 除第 3 章已记录问题外无新发现，最终通过 |

## 8. 复查要求

后续 review 必须以本文第 3 章已记录问题为基线，检查是否出现新的遗漏或逻辑冲突。发现的新问题
必须追加到本文，并在下一轮 review 中确认；不能把已记录问题重复报告为新问题。
