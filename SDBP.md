# L2 SDBP 初步实现计划

## 基准与规则

- 基准：[Actions run 40859791862](https://github.com/OpenXiangShan/GEM5/actions/runs/40859791862)，commit `39a2db6f92f0b09a470f8027260777bcc7c68a30`；代码位于 `~/GEM5-sdbp-sampler-young2-ci`。
- 相对 DefaultConfig 的修改 L2 1 MiB，4 个 256 KiB、8-way slice（64 B 行、每 slice 512 set）；关闭所有预取。
- sampler：每 slice **`samplerSets × samplerWays`**，默认 `samplerSets=128`、`samplerWays=max(1, l2Ways*3/4)`（整数除法向下取整；8-way L2 对应 6-way sampler）；15-bit partial tag、15-bit PC signature；**invalid → 按 way 序首个 dead → 全 set 真 LRU**。sampler set/way 均可显式覆盖默认值，约束及派生量见 `SDBPDetails.md`。
- 本 run 为 **guard0**：未传 `--sdbp-sampler-protect-youngest-two`，不保护 sampler MRU/次 MRU。该 commit 的保护功能仅作可选参数，默认关闭；开启时仅在 dead 扫描中跳过全 set 最近 `min(2,samplerWays)` 项，LRU 回退不受保护限制。
- predictor：3 × 4096 项、2-bit 饱和计数器，初值 0，三表之和 ≥ 8 判 dead。signature 为 `xor_fold(PC >> 1, 15)`；表索引先逐位对齐 GEM5 的 `mixed` hash/seed，硬件简化 hash 另行评估。
- 训练旧 signature：sampler demand hit → live；有效 sampler victim 被替换 → dead；仅 `signatureValid` 时训练。dead：三表饱和加一；live：第 0/2 表饱和减一，第 1 表右移一位。RTL 在 s1 以新 sig 预测，s3 根据 sampler 结果更新旧 sig；两者独立，预测不等待该次旧 sig 训练，具体观察时点见 `SDBPDetails.md`。
- L2 victim：**可用 invalid → 按 way 序首个 dead 且无本核 L1I/L1D 副本的行 → 可用 way 中的真 LRU**。LRU 回退允许淘汰有 L1 副本的行；不清除受保护行的 dead。

## 新增模块

以下文件放在 `XSCache/src/main/scala/coupledL2/`，每 slice 实例化一套。

| 模块 | 职责 |
| --- | --- |
| `SDBPSampler.scala` | `samplerSets × samplerWays` 项，保存 `valid/tag/signature/signatureValid/dead`，每 set 独立真 LRU；按配置路数选择 victim、产生旧 signature 训练事件并写入当前预测。支持非 2 的幂 way 数；单路时省去 LRU。 |
| `SCArray.scala` | s1 按当前 sig 读三表预测；寄存器版在 s3 按旧 sig 直接训练，单端口 SRAM 版通过 UpdateQ 排队读改写并前递 pending 操作。 |
| `SDBP.scala` | 与 L2 对齐：s1 读 sampler/当前 sig 的 SCArray，s2 寄存，s3 直接 masked write sampler/L2 dead；无需 EventQ 或异步 set 锁，仅 SRAM SCArray 训练使用队列。 |
| `L1ICopyTracker.scala` | 镜像本核 ICache 的有效物理行；按 ICache set/way 接收安装、替换、失效及 fence.i 事件，提供候选行驻留 mask，补足现有 `clients` 不覆盖 L1I 的缺口。 |

sampler 抽样参照 L2 DRRIP 的高、低 set 位比较：令 `setBits=log2(localSets)`、`samplerBits=log2(samplerSets)`、`compareBits=setBits-samplerBits`；当 `compareBits>0`，采样条件为 `set[setBits-1:samplerBits] == set[compareBits-1:0]`，索引为 set 低 `samplerBits` 位。只用相等分支，不用 DRRIP 的互补分支；两段位域可重叠。`localSets/samplerSets` 均须为 2 的幂，`2 ≤ samplerSets ≤ localSets`；二者相等时全部采样。`samplerWays ≥ 1`，无须为 2 的幂。默认 `localSets=512/samplerSets=128` 时比较 `set[8:7] == set[1:0]`。地址、waymask、比较器及 reset 按参数生成；每 set LRU 为 `samplerWays*(samplerWays-1)/2` bit，单路不存 LRU。

## 需修改模块

| 位置 | 修改 |
| --- | --- |
| `XSCache/.../L2Param.scala`、`Common.scala` | 增加独立的 `samplerSets/samplerWays`（默认 128 / `max(1, l2Ways*3/4)`）、`protectYoungestTwo=false` 及 predictor 参数；检查合法性，派生地址/way 位宽、mask、LRU 和事件/结果 bundle；为 `TaskBundle`、`MergeTaskBundle`、`ReplacerInfo` 补 `pcValid/signature/请求类型`，区分无 PC 与 PC=0。 |
| `src/main/scala/top/Configs.scala` | 增加 SDBP 配置入口，开启现有 `enablePC`，配置容量、slice、`samplerSets/samplerWays` 与 predictor，显式关闭 sampler 最近两项保护；保留 LRU 对照配置。 |
| `src/.../cache/dcache/DCacheWrapper.scala`、`mainpipe/MissQueue.scala` | 复用现有 `PCField/PCKey`，补有效位；核对 Acquire 的 PC 来源。其他客户端无 PC 时显式无效。 |
| `XSCache/.../SinkA.scala`、`RequestArb.scala` | 解析并透传元数据到 Directory；复用 Directory 写阻塞和 MCP2 入口间隔，补齐 Get/Hint 覆盖条件；合入 SCArray 端口仲裁；仅 SRAM 版为旧 sig 训练预留 UpdateQ 名额。 |
| `XSCache/.../RequestBuffer.scala`、`MSHRCtl.scala`、`MSHR.scala` | 保存最终 demand/PC 元数据；prefetch 合并 demand 后按 demand refill，优先首个带有效 PC 的 demand；补齐普通及合并 refill 的 PC 传递。 |
| `XSCache/.../MainPipe.scala`、`Slice.scala` | 实例化并连接 SDBP；在确定接受的 hit、成功安装的 refill 上完成一次 s3 写，并直接训练或入 UpdateQ；retry 不重复训练。 |
| `XSCache/.../Directory.scala`、`utils/Replacer.scala` | 增加独立 per-way dead 存储，复用 `TrueLRU`；结合有效位、L1-copy mask、`wayMask/freeWayMask` 选 victim。失效/新行覆盖同步清理 dead，普通一致性 metadata 写不覆盖 dead。 |
| `src/.../frontend/icache/ICacheImp.scala`、`ICacheMetaArray.scala` 及 `XSTile.scala/L2Top.scala` | 导出 ICache 实际驻留变化，连接 tracker 并向对应 L2 slice 分发；覆盖替换、维护失效和 fence.i。 |

其中 `XSCache/.../` 指 `XSCache/src/main/scala/coupledL2/`，`src/.../` 指 `src/main/scala/xiangshan/`。

## 模块交互

```text
s1：L1/最终 refill 请求 → Directory + SamplerCache + SCArray[currentSig] 读
s2：寄存三者结果，形成 sampler 目标 way 与旧 sig
s3：有效 hit/refill → sampler 单路写 + L2 dead 更新
                   ↘ oldSig 训练：寄存器直接更新 / SRAM UpdateQ → 读改写
L1I tracker + Directory.clients → L2 victim 的 I/D-copy mask
```

- D-copy 复用 `MetaEntry.clients.orR`，核对副本记录；它是协议持有状态，可能比 GEM5 的瞬时 tag 查询保守。I-copy 用 tracker，不能把 `clients==0` 当作 L1I/L1D 均无副本。
- tracker 更新按实际 tag 生效顺序送达；未完成的驻留更新先排空或阻塞相关 victim 查询。MSHR/响应队列不计作驻留副本。
- SamplerCache 在 s3 直接按路写，复用 Directory 写阻塞与非 Hint 请求的 MCP2 入口间隔；补齐 Get hit 的替换状态写，开启预取时让 sampled Hint 也触发现有间隔，无需独立 sampler RAW 检测或前递。SCArray 可选寄存器或单端口 SRAM；后者的 UpdateQ 保存旧 sig 的训练操作，满时背压，不使用 sampler EventQ。

## 整体 L2 请求流程

1. **入站**：L1 Acquire/Get 或预取进入 `SinkA → RequestBuffer → RequestArb`，携带请求类型、signature 和有效位；Release/Probe/Snoop/CMO 不训练 sampler。
2. **查目录**：Directory s1 读 tag/meta/LRU/dead，s2 锁存，s3 判 hit。s1 同时读 sampler 和当前 sig 的 SCArray，s2 锁存；s3 仅实际接受的 hit 使用结果，初始 miss 不改 sampler/不训练。
3. **hit 更新**：抽样 set 执行 sampler 操作。sampler demand hit 训练旧 signature 为 live，同时保存当前 signature/s1 预测 dead、刷新 sampler LRU；sampler miss 按 invalid→首个 dead→LRU 换入并训练旧有效 signature 为 dead。无 PC demand 仍可训练旧 signature，但清当前 `signatureValid/dead`。预取 sampler hit 完全不更新。L2 demand hit 写当前预测，无 PC 写 dead=0；预取 hit 保留原 dead。L2 recency 与 GEM5 `touch` 对齐，预取 hit 也刷新。
4. **miss/合并**：MainPipe 分配 MSHR，经 `TXREQ` 向下游发请求，`RXDAT/RXRSP` 接收响应。合并仅更新 MSHR 最终元数据，不额外训练；纯预取 refill 无有效 signature。
5. **refill 选 victim**：MSHR refill task 经 RequestArb 重读 Directory；先屏蔽占用/不可替换 way，再按 invalid→无 L1-copy 的 dead→LRU 选择。全忙则 retry，SDBP 状态不更新。保留现有 MSHR 的 Probe、脏数据回收及 CHI 写回流程。
6. **成功安装**：refill s1 使用最终请求元数据读 sampler/SCArray，s3 成功安装时执行第 3 步的规则；sampler 预取 hit 仍不更新。有 PC demand 使用 s1 预测写 sampler/L2 dead，旧 sig 的训练在寄存器版直接执行、SRAM 版入 UpdateQ；预取/无 PC 的 sampler 新插入清 `signatureValid/dead`，L2 refill 写 dead=0。L2 新行置 MRU，经 DataStorage/GrantBuffer 返回 L1；失败或重试不重复安装、训练。

## 落地顺序与验证

1. 打通 PC 有效位及 MSHR 最终元数据；实现 sampler/predictor，与该 worktree 导出的事件序列逐项对拍。
2. 接入 Directory 的 dead+TrueLRU，再接入 D/I-copy 保护；仅 D-copy 的中间版本不宣称与该 run 等价。
3. 参数覆盖：`samplerSets∈{2,64,128,localSets}`、`samplerWays∈{1,3,6,8,12}`；检查抽样索引一一覆盖、位宽/mask、单路无 LRU、非 2 次幂 way 不越界及非法配置拒绝。定向覆盖：sampler 的 invalid/dead/LRU 优先级、dead 按 way 序选择（含 MRU/次 MRU）、sampler dead 写回、计数器训练、无 PC、预取合并、全部 L2 dead 有 L1 副本时的 LRU 回退、占用 way、refill retry、连续同 set、ICache 替换/fence.i。可选 guard 单独测试。关闭预取，对照 LRU 和原 8-way 纯 LRU sampler，统计 sampler/dead hit、live/dead 训练、dead victim、copy 保护及 LRU 回退。
