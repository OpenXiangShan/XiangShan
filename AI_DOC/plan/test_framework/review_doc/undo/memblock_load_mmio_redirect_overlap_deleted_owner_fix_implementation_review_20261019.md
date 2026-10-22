# Load MMIO Redirect Overlap 已删除 Owner 修复实现 Review（2026-10-19）

| 项目 | 内容 |
| --- | --- |
| Review 状态 | 源码、专项测试和原失败 seed 复验通过；100-seed 回归进行中 |
| 关联 plan | `AI_DOC/plan/test_framework/plan/undo/memblock_load_mmio_redirect_overlap_deleted_owner_fix_plan_20261019.md` |
| 关联 plan review | `AI_DOC/plan/test_framework/review_doc/undo/memblock_load_mmio_redirect_overlap_deleted_owner_fix_plan_review_20261019.md` |
| 关联缺陷 | `AI_DOC/buglist/rm/v2/rm_buglist_2026-W43_20261019.md` |
| 基线 commit | `8e45befc10bc1ab93d465ebc3ac16a91d6b750c6` |
| 修改范围 | Load MMIO owner解析和software-only定向场景 |
| 非目标 | DUT RTL/Scala、monitor采样、redirect生产、Store MMIO和cfg |

## 1. 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 代码落点 | 使用场景 |
| --- | --- | --- | --- |
| `overlap window` | redirect可解释旧Load MMIO的`R..R+2`采样窗口 | `resolve_mmio_uid_by_rob_value()` | redirect与raw同拍或相差1至2拍 |
| `active owner` | 当前完整ROB key对应的有效uid | `uid_by_active_rob` | 正常归属或survivor判定 |
| `deleted owner` | redirect删除owner时保留的单代证据 | `redirect_deleted_lq_owner_by_key` | active owner已消失后的严格证明 |
| `historical proof` | 结合status、record、ROB/LQ和epoch证明旧实例 | `find_unique_redirect_deleted_load_mmio_owner()` | 仅唯一候选成功 |
| `stale-drop` | 丢弃已被redirect覆盖的旧raw，不写tag | `MEMBLOCK_MMIO_RESOLVE_STALE_DROP` | 不影响新动态实例 |

`resolve_mmio_uid_by_rob_value()`负责把value-only raw解析为当前owner或已证明的旧事件；不创建owner、
不改变redirect，也不修改DUT激励。`find_unique_redirect_deleted_load_mmio_owner()`只读现有单代历史，
不修改map、queue、tag或生命周期。

## 2. 修改前问题与实现范围

修改前只有精确`R+2`且active owner为0时调用历史证明。同拍`R`场景中，redirect先删除
`uid=7358 ROB=1/158`，随后raw解析直接fatal，尽管deleted-owner、epoch和record均能唯一证明归属。

实现只修改两个文件：

1. `common_data_transaction.sv`把严格历史证明调用范围扩展到`R..R+2`。
2. `soft_test_memblock_pending_mmio_directed_sequence.sv`把原`R+2`正向场景参数化为offset 0、1、2。

## 3. Resolver 调用窗口修改

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`，函数
`resolve_mmio_uid_by_rob_value()`。

抽象功能描述：当Load raw已命中唯一redirect overlap且不存在active owner时，在完整硬件允许窗口内
调用既有严格历史证明；成功只返回stale-drop，证明失败仍fatal。

```systemverilog
if (raw_sample_seq >= overlap_redirect_sample_seq &&
    raw_sample_seq <= overlap_redirect_sample_seq +
                      MEMBLOCK_LOAD_MMIO_REDIRECT_MAX_SAMPLE_OFFSET &&
    active_candidate_count == 0) begin
    if (find_unique_redirect_deleted_load_mmio_owner(...)) begin
        return MEMBLOCK_MMIO_RESOLVE_STALE_DROP;
    end
    `uvm_fatal("MMIO_RESOLVE", stale_reason)
end
```

中文伪代码：先确认raw sample位于redirect sample到加2的闭区间，再要求两个ROB flag均没有active
候选；随后调用历史helper检查同一redirect record下是否只有一个被删除的旧Load。唯一证明成功时只
丢弃raw；零候选、多候选或身份不一致时继续fatal。active survivor和new owner不会进入该分支。

`find_unique_redirect_deleted_load_mmio_owner()`内部条件未改，仍要求同一epoch/record、record已完成扫描、
scalar Load、inactive/flushed/killed/redirect-pending、完整ROB被覆盖、有效LQ deleted owner、dynamic
epoch连续且最终候选数严格为1。因此实现没有把时间窗口本身当作stale依据。

## 4. 定向场景参数化

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_pending_mmio_directed_sequence.sv`，
task `run_deleted_load_stale_scenario()`。

抽象功能描述：通过正式redirect和LSQ owner API建立已删除Load owner，再按指定offset注入raw，验证
stale-drop且不写MMIO tag；该task不直接伪造内部map。

```systemverilog
run_deleted_load_stale_scenario(0);
run_deleted_load_stale_scenario(1);
run_deleted_load_stale_scenario(2);

apply_load_mmio_raw(owner_key.value, anchor_sample_seq + sample_offset,
                    scenario_name);
if (data.get_status(1).mmio_tag_valid) begin
    `uvm_fatal(get_type_name(), "stale loadMmio incorrectly tagged deleted uid")
end
```

中文伪代码：主测试依次运行同拍、晚1拍和晚2拍场景。每个场景先建立真实active owner并执行redirect
删除，检查旧状态已经inactive、flushed、killed且等待重发；再按offset注入同ROB value的raw。resolver
必须通过历史证明丢弃raw，测试最后确认旧uid没有被写入MMIO tag。

原有survivor、新owner、wrap歧义、无历史、非Load和`R+3`expected-fatal场景全部保留；仅同步更新
fatal匹配文本，未放宽catcher范围。

## 5. Plan 对齐检查

### 5.1 与 plan 不一致的实现

均与计划保持一致。

### 5.2 Plan 未说明但实现补充的细节

均与计划保持一致；只为诊断文本增加实际offset，没有新增状态或行为。

## 6. 验证结果

- `git diff --check`通过。
- 编译mode：`fault3_mmio_overlap_deleted_owner_fix_v2_20261019`，VCS编译成功。
- software-only专项：offset 0、1、2正向路径和既有负向边界全部通过，`TEST_PASS`、
  `UVM_ERROR=0`、`UVM_FATAL=0`、`Quit count=0/10`、正常`$finish @360.300ns`。
- 原失败seed `1864500247`：原cfg完整执行10000请求，`TEST_PASS`、
  `RM_LS_TRACE_COMPARE=10000`、`UVM_ERROR=0`、`UVM_FATAL=0`、`Quit count=0/10`、
  正常`$finish @3430467.800ns`。
- 原错误点：`sample=489999`、redirect sample同为`489999`，`epoch/record=11058`，现唯一匹配
  deleted `uid=7358 ROB=1/158`并以`offset=0` stale-drop；随后新动态实例compare PASS。
- 100-seed回归：待使用同一修复版binary恢复剩余85个seed。

## 7. Review 结论

实现仅补齐既有严格证明的调用时点，没有降低owner证明门槛。专项和真实失败seed证明同拍旧raw可被
正确丢弃，survivor/new owner及负向边界未被吞掉。当前无源码blocker；待100个唯一seed全部终态后
更新最终统计并归档plan。
