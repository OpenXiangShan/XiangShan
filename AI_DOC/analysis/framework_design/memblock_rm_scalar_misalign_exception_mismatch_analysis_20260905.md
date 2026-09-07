# V2 标量非对齐访存 RM 异常误判分析（2026-09-05）

| 项目 | 内容 |
| --- | --- |
| 状态 | 已确认是 RM 建模缺口；首地址 PMA gate 的 review 缺口已修正，专项回归进行中。 |
| 适用版本 | V2，分支 `codex/pbmt-rm-l2tlb-20260902`。 |
| 测试场景 | `basicTest` / `memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq` / `tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k` / seed `666666`。本次为 `+MEMBLOCK_MAIN_TRANS_NUM=1000` 的定向复现。 |
| 场景约束 | Sv39/U、PBMT=`00`、无 CSR/SFence 动态切换、标量 Load/Store、PMA DDR 可缓存地址范围、开启普通非对齐和跨 16B 边界生成。 |
| 首次 RM 错误 | `736.3ns`，UID0，`RM_LS_COMPARE`：`Store exception expected=0x40 status=0x0`。 |
| 波形 | `mem_ut/ver/ut/memblock/sim/trigger_gate_output_off_20260905/wave/tc=basicTest_ts=memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq_cfg=tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_seed=666666_rtl.fsdb`。 |
| RTL 修改 | 禁止；本问题不修改 RTL。 |

## 1. 术语

| 术语 | 当前含义 |
| --- | --- |
| 自然非对齐 | 访问地址没有满足其字节数的自然边界，例如 `SH` 的地址 bit0 为 1。它只是地址几何属性。 |
| 跨 16B 非对齐 | 标量访问首尾字节位于两个不同的 16B bank。本例的 2B store 从 `0x...edf` 跨到 `0x...ee0`。 |
| MAB | `StoreMisalignBuffer` 或 `LoadMisalignBuffer`。普通可缓存跨 16B 标量访存可由该硬件缓冲拆成多个对齐子请求。 |
| PBMT | 页表内存属性。`00` 表示交由 PMA 决定，`01` 为 NC，`10` 为 IO。 |
| PMA C 属性 | V2 PMA 的 cacheable 位。PBMT=`00` 且 PMA C=1 才是本分析中的普通可缓存内存。 |
| `HD_MISALIGN_*_ENABLE` | CSR 中控制硬件处理标量非对齐 Load/Store 的开关。RM 必须使用 UID 冻结的 CSR 快照，而非 commit 当拍 CSR。 |

## 2. 失败现象

UID0 为 `SH`，其访问 VA 为 `0x863abedf`，长度 2B。RM 按地址低位得到“非对齐”，随后直接期望
`storeAddrMisaligned`（exception bit6，数值 `0x40`）。DUT 真实 commit status 是 `0x0`，因此在
ROB commit 后报出 `RM_LS_COMPARE`。

同一轮 1000 笔定向运行共出现 10 条同类错误：5 条 Store 期望 bit6，5 条 Load 期望 bit4。它们都
落在该用例故意开启的非对齐 boundary profile；这不是 terminal retire 判断失败。UID0 随后在
`742.8ns` 完成 SQ dequeue 并成为 terminal done，证明 DUT 的正常完成路径闭环。

## 3. 波形证据

UID0 的关键时序如下：

| 时间 | 观察 | 结论 |
| --- | --- | --- |
| `670.3ns` | `top_tb.U_MEMBLOCK.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable=1`，`_inner_StoreUnit_0_io_misalign_enq_req_valid=1`，ROB=0，VA=`0x863abedf`。 | 硬件非对齐处理已使能，StoreUnit 将请求交给 MAB。 |
| `685.3ns` | `_inner_storeMisalignBuffer_io_splitStoreReq_valid=1`，子请求 VA=`0x863abedf`。 | MAB 发出低字节子 Store。 |
| `700.3ns` | 同一 split request 路径发出 VA=`0x863abee0`。 | MAB 发出高字节子 Store；两者共同实现原始 SH。 |
| `720.3ns` | MAB 与 `writebackSta_0` 的 ROB=0，`exceptionVec[6]=0`，`debug.isMMIO=0`，`debug.isNCIO=0`。 | 最终写回是普通 cacheable 完成，不是 NC/MMIO 异常路径。 |
| `735.3ns` / `742.8ns` | ROB commit，随后 SQ dequeue 与 terminal retire。 | UID0 被正常提交并终结。 |

## 4. 根因

RM 的 `memblock_rm::observer_build_commit_item()` 仅用以下几何判断产生地址异常：

```text
misaligned = (computed_vaddr & (size_bytes - 1)) != 0
expected_exception[store ? 6 : 4] = 1
```

该逻辑没有消费已经冻结在 `tlb_context` 中的
`hd_misalign_ld_enable/hd_misalign_st_enable`，也没有区分实际 PBMT/PMA 内存类型。于是它把“地址不自然对齐”错误等价为“体系结构 address-misaligned exception”。

V2 StoreUnit 的行为与波形一致：当硬件非对齐处理使能且请求是普通内存时，StoreUnit 将跨 16B
store 送入 `StoreMisalignBuffer`；MAB 拆分为对齐子 Store。只有 split response 实际进入 uncache
路径时，MAB 才生成 `storeAddrMisaligned`。普通 cacheable 请求不置 bit6。Load 路径同理。

因此，本失败是 RM 期望过严，不是 RTL 漏报异常，也不是本 strict PBMT=00/non-NC cfg 失效。

## 5. 最优修复方案

RM 不应从 DUT 最终异常反推期望；应在已有的翻译和 PMA/PMP 构建路径上补足一个窄的、可复现的事实：

1. 从每个已命中的 TLB entry 以 V2 `TLB.pbmt_check()` 相同规则得到实际可见 PBMT：S1-only 取 S1，S2-only 取 S2，allStage 优先非零 S1、否则取 S2。
2. 在 PMA/PMP readonly view 中暴露“此访问经 PMA 判定为可缓存”的单一事实，不暴露或比较 DUT 的 MMIO/NCIO 输出。
3. 只有以下条件同时成立时，RM 才抑制原有的 bit4/bit6 期望：
   - 访问几何确实非对齐；
   - UID 冻结的 `HD_MISALIGN_LD_ENABLE` 或 `HD_MISALIGN_ST_ENABLE` 为 1；
   - 访问所有已解析 byte 的有效 PBMT 都为 `00`；
   - PMA/PMP 模型确认每个实际 PA byte 都是普通可缓存内存；
   - 不存在已有 PF/GPF/AF。
4. NC、IO、PMA 非缓存或 PMA/PMP 模型未提供确定事实时，保留既有保守的地址异常期望；本轮不把未知路由属性猜成正常完成。
5. 不能只用首地址的 PMA 结果代表整笔访问。RM 保留完整尺寸 query 用于原有 AF 期望，另对
   每个实际 PA byte 做单字节 query；任何 byte 未明确为可缓存、AF 未决或有方向相关 AF，均不放宽
   bit4/bit6。

这样修复只放宽已被 V2 RTL 和 PMA/PBMT 事实证明的普通可缓存路径，且仍使用 UID request-time
CSR snapshot，不增加全表扫描、第二份状态表或对 RTL 输出的依赖。

## 6.1 Subagent review 修正

Subagent review 发现初版实现仅查询 `item.pa_by_byte[0]`，将首字节的
`normal_cacheable` 错当成整笔访问事实。该实现对跨 PMA/PMP 边界的访问不安全，已在 RM 中改为
逐 byte `size_bytes=1` 查询并取合取；完整尺寸查询仍保留用于已有 PMA/PMP AF 字段。修正后，未知
或任一字节非普通可缓存时回到原有 bit4/bit6 期望，不会因首地址合法而误放宽。

对应可执行方案见：
[`memblock_rm_scalar_misalign_semantics_plan_20260905.md`](../../plan/rm_plan/undo/memblock_rm_scalar_misalign_semantics_plan_20260905.md)。

## 7. 预期验证

修复后以相同 testcase、cfg、seed 和 1000 笔覆盖运行。验收条件：

- UID0 的 RM expected exception 变为 `0x0`；
- 原有 10 条 bit4/bit6 假阳性不再出现；
- 非对齐 Load/Store 仍完成数据或 Store table 比较；
- 若出现新错误，按该错误重新分类为 RM 还是 RTL，不修改 RTL。
