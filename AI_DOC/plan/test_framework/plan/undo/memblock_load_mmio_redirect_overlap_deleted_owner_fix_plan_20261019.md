# MemBlock Load MMIO Redirect Overlap 已删除 Owner 修复 Plan

状态：coding、专项和原失败 seed 复验完成；100-seed 回归进行中

创建日期：2026-10-19

适用版本：V2 `mem_ut_uvm_v2`

## 1. Plan 定位、专有名词与抽象功能说明

本 plan 修复测试框架解析 `loadMmio` 时的事件处理顺序缺口。DUT 的 redirect 与
`loadMmio` 可以属于同一采样 `R`，但测试框架可能先应用 redirect flush、删除 active owner，
再处理该拍的 MMIO raw。现有 resolver 仅允许精确 `R+2` 在 active owner 消失后使用历史证明，
因此合法的 `R` 或 `R+1` 旧脉冲会被误报为 `MMIO_RESOLVE` fatal。

### 1.1 专有名词

| 英文术语 | 当前文档中的中文含义 | 代码落点 | 示例 |
|---|---|---|---|
| `overlap window` | 同一 redirect 可解释旧 `loadMmio` 的采样窗口 | `R..R+2` | raw sample 为 redirect sample、加1或加2 |
| `active owner` | 当前仍有效的完整 ROB key 到 uid 映射 | `uid_by_active_rob` | redirect flush 前可直接找到旧 Load |
| `deleted owner` | redirect 删除 active owner 时保留的单代短时历史 | `redirect_deleted_lq_owner_by_key` | flush 后用于证明旧 raw 属于被杀实例 |
| `historical proof` | main/status、redirect record 和 deleted owner 联合形成的唯一归属证明 | `find_unique_redirect_deleted_load_mmio_owner()` | 只有一个满足全部条件的旧 scalar Load |
| `stale-drop` | 证明 raw 属于旧实例后丢弃，不写当前 MMIO tag | `MEMBLOCK_MMIO_RESOLVE_STALE_DROP` | 同拍 redirect 已覆盖该 Load |
| `sample ordering` | 同一采样内测试框架处理 redirect 和 MMIO raw 的先后顺序 | adapter/handler 调度顺序 | redirect 先处理时 active map 已为空 |

### 1.2 关键函数抽象功能

`resolve_mmio_uid_by_rob_value()`：把只有 ROB value 的 MMIO raw解析为当前唯一 owner，或严格证明为
被 redirect 覆盖的旧事件后 stale-drop；它不创建 owner、不改变 redirect，也不修改 DUT 激励。

`find_unique_redirect_deleted_load_mmio_owner()`：在 active owner 已消失时，只读现有状态和单代
deleted-owner窗口，证明是否存在唯一、由同一次 redirect 删除且曾真实 issue 的 scalar Load。

`run_deleted_load_stale_scenario()`：software-only 定向场景在真实公共 API 建立 redirect-deleted owner
后，分别注入 `R`、`R+1`、`R+2` raw，验证处理顺序变化不会造成误 fatal 或误写 tag。

## 2. 根因与责任判定

失败 seed `1864500247` 在 `2450222.800ns` 报错：

```text
UVM_FATAL [MMIO_RESOLVE] cannot prove LOAD MMIO stale ownership
sample=489999 redirect_sample=489999 redirect_record=11058
active=0 old_covered=0 new=0 uncovered=0 incompatible=0
```

日志同时证明 `uid=7358 ROB=1/158` 已被 record `11058` 覆盖，状态为
`active=0 flushed=1 killed=1 redirect_pending=1`，deleted-owner窗口的 epoch/record也均为
`11058`。因此 DUT 已给出可解释的一组同拍 redirect 与旧 `loadMmio`，错误来自测试框架先删除
active owner后，resolver仍把历史证明限制在精确 `R+2`。责任归属为测试框架，不修改 DUT RTL。

## 3. 修改 Flow

### 3.1 Resolver 调用条件

抽象功能描述：在唯一 Load redirect overlap 已建立、两个完整 ROB flag 均没有 active owner时，
对整个 `R..R+2` 窗口调用同一严格历史证明 helper。

文字伪代码：

```text
如果 Load raw 命中唯一 redirect overlap：
  先保留 active old owner、active survivor、new owner和不兼容owner的原判定。
  如果 active owner数量为0，且raw sample位于redirect sample到redirect sample+2：
    调用deleted-owner历史证明。
    唯一证明成功则stale-drop，不写MMIO tag。
    证明失败则fatal并打印offset、record和证明失败原因。
  其它无法唯一归属的情况继续fatal。
```

该修改只扩大既有 helper 的调用时点，不放宽 helper 内任何 owner、epoch、record、ROB、LQ、
issue 或 redirect 覆盖条件。

### 3.2 历史证明保持不变

抽象功能描述：`find_unique_redirect_deleted_load_mmio_owner()` 继续作为只读证明函数，零候选或多候选
均失败，不产生状态副作用。

必须同时满足：

1. overlap redirect、epoch、cancel record和deleted-owner窗口一致。
2. cancel record已完成 active scan，且payload与overlap redirect一致。
3. 候选是相同ROB value的scalar Load，并且旧实例曾真实issue。
4. 当前状态为inactive、flushed、issue-killed、redirect-pending，且尚未重新dispatch。
5. 完整ROB key确实被该redirect覆盖。
6. LQ deleted owner的uid、旧dynamic epoch、旧activation epoch、redirect epoch和record id全部一致。
7. 最终候选数严格等于1。

### 3.3 定向测试

修改 `soft_test_memblock_pending_mmio_directed_sequence.sv`：

1. 将精确 `R+2` deleted-owner正向场景参数化为offset `0/1/2`，逐一验证stale-drop且不写tag。
2. 保留active owner下的 `R/R+1` stale、survivor、new owner、不兼容owner和wrap歧义场景。
3. 保留 `R+2` active survivor、新owner、无历史、非Load和 `R+3` 拒绝边界。
4. expected-fatal catcher仍只捕获指定 `MMIO_RESOLVE` 文本，不能吞其它fatal。

## 4. 性能与生命周期边界

该逻辑位于MMIO monitor事件路径，但全表历史扫描仅在唯一redirect overlap且active owner为0时触发。
正常MMIO raw仍只probe两个完整ROB key；active overlap路径不新增扫描。deleted-owner窗口仍按现有
短生命周期建立和过期，不新增长期历史表。

## 5. 修改范围

计划修改：

1. `mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`
2. `mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_pending_mmio_directed_sequence.sv`
3. 本 plan、plan review、implementation review和W43 buglist记录

明确不修改：DUT RTL/Scala、monitor采样字段、redirect生产逻辑、Store MMIO路径、cfg和参数。

## 6. 验证计划

1. `git diff --check`和关键条件静态复查。
2. 新mode编译。
3. `basicTest + memblock_pending_mmio_directed_vseq + default + seed=666666`完整通过。
4. 原失败 `seed=1864500247`、原cfg、10000请求完整复验，要求10000 compare、0 error/fatal、
   `Quit count=0/10`和正常`$finish`。
5. 原seed通过后，使用新binary恢复剩余100-seed回归；保留此前PASS结果和旧失败现场。
