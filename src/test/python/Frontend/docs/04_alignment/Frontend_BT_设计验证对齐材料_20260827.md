# Frontend BT 设计-验证对齐材料（2026-08-27）

> 文档状态：本轮对齐草案
> 对齐日期：`2026-08-27`
> 对齐窗口：`2026-08-13` 至 `2026-08-27`
> 统计时间：`2026-08-27 14:54:20 +0800`
> 负责人：`Frontend BT 验证团队`
> 评审参与人：`Frontend 设计 / Frontend BT 验证`

## 0. 本次对齐摘要

1. 本周期分别于 `2026-08-17`、`2026-08-24` 两次同步 `kunminghu-v3`，当前设计基线为 `6891f912cddf49318ca89d69a37da44dda49c882`，已纳入 IFU checkerRedirect、two-pre/fetch PNR、IBuffer partial-drain、PC 类型重构以及 BPU/TAGE/BTB 训练时序等变更。
2. Frontend BT 主表叶子测试点由上轮 `1134` 个调整为 `1122` 个；coverage registry 由 `405` 项增至 `686` 项，其中完整 active Python bin 由 `247` 个增至 `528` 个，测试点、覆盖率模型和回归用例同步扩展。
3. 三人共同推进测试点、覆盖率模型、回归用例和结果反标，当前主表共有 `1122` 个叶子测试点，coverage registry 共 `686` 项，其中 active Python bin `528` 项；主表累计 `HIT 259` 个，后续继续按统一版本基线收敛整体覆盖率。
4. 当前 Verilator DUT 绑定验证提交 `7afd6f737d9e3ddc881bda5523e854f07ef5e246`；覆盖率模型和测试用例仍在持续补充，当前尚未形成与 `c6cbdac5` 定义完全兼容的全量 aggregate，因此累计反标结果可用于进度对齐，不能直接作为当前 HEAD 的单次签核报告。
5. 本次希望设计侧重点确认 `notCfiTaken/canTrain`、PNR 对 two-pre/fetch 的阻断边界、IBuffer partial-drain 的 valid/bits 原子性，以及 PC 类型重构后的地址宽度与 canonical 语义；确认后由验证侧补齐对应专项回归和覆盖率闭环。

## 1. 版本基线

### 1.1 设计版本 baseline

| 项目 | 内容 |
| --- | --- |
| 设计主线 + 合入 commit | `kunminghu-v3 @ 6891f912cddf49318ca89d69a37da44dda49c882` |
| 本轮最新 frontend 相关 PR | `#6398`，`fix(IFU): ifu redirect canTrain add notCfiTaken signal`，设计提交日期 `2026-08-21` |
| 本轮 latest design sync | `d886682a24d6dfb13770ca72f91ef184943d91d2`，`2026-08-24` 合入 `frontend-bt` |
| 本轮 previous design sync | `14905e6587a6b367d2a37f3a60348636d2148ff5`，`2026-08-17` 合入 `frontend-bt` |
| 上次对齐设计基线 | `kunminghu-v3 @ 00eb9ee7c544caddef192eef81f9490533ffbad3` |

### 1.2 验证仓库 baseline

| 项目 | 内容 |
| --- | --- |
| 验证仓库/分支 + commit | `frontend-bt / frontend-bt @ c6cbdac5da0243d8df5531306e5b182510b68e29` |
| 验证 commit 日期 | `2026-08-27 14:33:08 +0800` |
| 回归绑定 implementation/DUT commit | `7afd6f737d9e3ddc881bda5523e854f07ef5e246`，可从当前 HEAD 到达 |
| 工作树说明 | 本材料统计锁定已提交的 `c6cbdac5`；统计时存在其他在途修改，不纳入本轮已提交口径 |

设计基线使用 8 月 24 日同步时的 `kunminghu-v3` second parent；验证基线使用本材料生成时的已提交 HEAD。DUT manifest 的 `design_baseline_sha` 当前仍默认写成验证提交 `7afd6f737...`，后续构建应显式记录对应的 v3 SHA，避免设计基线与验证实现 SHA 混用。

### 1.3 验证内部追溯信息

| 项目 | 内容 |
| --- | --- |
| DUT 构建 manifest | `build-frontend/frontend_build_manifest.verilator.json` |
| manifest 文件签名 | `sha256:2836ba78f1abe5911131cff7aba753a8c81af0f5f74f045f721f2879fe749c71` |
| 仿真工具 | `Verilator` |
| DUT 构建命令 | `make frontend CONFIG=DefaultConfig ISSUE=E.b NUM_CORES=1 CHISEL_TARGET=systemverilog FRONTEND_WAVEFORM_FORMAT=fst` |
| DUT build hash | `bdf35950c250a319c9b6cb9d097e3ec27510e579788bad01eaf61e5672bdc750` |
| DUT Python extension hash | `5e837dff5154901f459ec0fbb51fc9dbe1ef3cb6277cab03a7b8544308ad5fea` |
| generated RTL hash | `a77587797da5f725a31fe55168c6c5672b9d2dfbafc2746395f37e2f12542354` |
| signal-contract hash | `b9be682e895214e6b671fb1422effe8cac2057f2cda82d91a85cca1186467d0f` |
| manifest 生成时间 | `2026-08-25T03:32:10.842590+00:00` |
| 测试点主表 HEAD 签名 | `sha256:c3d2797a64d53d773ff7bbe21e7139145bab05015120eaedca8a48955d16ac2b` |
| coverage registry HEAD 签名 | `sha256:7404f7e8a64017f1af0441c385f0f9b8eee7fe6fa2bfffc3905b0f260459ba9f` |

## 2. 当前验证状态

### 2.1 与上次对齐相比的进展

| 指标 | 上次材料 | 本次材料 | 变化 | 说明 |
| --- | ---: | ---: | ---: | --- |
| 叶子测试点数 | `1134` | `1122` | `-12` | 测试点重整、去重后的可执行叶子口径 |
| coverage registry 总项数 | `405` | `686` | `+281` | 包含 active 与历史/未激活项 |
| active Python bin 数 | `247` | `528` | `+281` | `Coverage_Group/Coverpoint/Bin_Name` 完整的可执行项 |
| 主表累计 `HIT` | `61` | `259` | `+198` | 主表累计反标状态，不等同于当前版本单次 aggregate |
| `.S` 汇编源用例 | `19` | `26` | `+7` | 汇编逻辑场景数同步由 `19` 增至 `26` |

### 2.2 测试点与覆盖率模型统计

测试点主表：`src/test/python/Frontend/docs/02_testpoint/Frontend_testpoint_0525_coverage_backannotated.csv`；覆盖率模型：`src/test/python/Frontend/docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv`。

| 指标 | 数量 | 统计口径/证据 |
| --- | ---: | --- |
| 测试点文档物理行数 | `1646` | HEAD 文件 `wc -l` |
| CSV 逻辑记录数 | `1645` | `csv.DictReader` |
| 叶子测试点数 | `1122` | `report_frontend_verification_alignment.py` executable leaf 规则 |
| coverage registry 总项数 | `686` | pilot CSV 全部 `Bin_ID` 记录 |
| active Python bin 数 | `528` | coverage group、coverpoint、bin name 三字段完整 |
| Python pytest 可收集节点 | `800` | HEAD 快照 `pytest --collect-only`；含环境单测、契约测试和 DUT 场景 |
| `.S` 汇编源用例 | `26` | `tests/asm_cases/**/*.S` |
| 当前工作区已编译 `.bin` | `26` | `tests/asm_cases/generated/*.bin`；生成产物不纳入 Git HEAD |
| ASM 逻辑场景数 | `26` | 按 `.S` 源文件去重，不与 `.bin` 重复相加 |

当前主表原始状态如下。`BLOCKED` 保留为独立原因状态；若需要强制归入五类主状态，可将其映射到 `MODELED`，但不得计入 `HIT`。

| 状态 | 数量 | 占叶子点比例 | 说明 |
| --- | ---: | ---: | --- |
| `UNMAPPED` | `279` | `24.87%` | 尚无 coverage/testcase/evidence 映射 |
| `MODELED` | `570` | `50.80%` | 已建模但无有效 HIT 状态 |
| `PARTIAL` | `12` | `1.07%` | 部分路径或条件已有证据 |
| `HIT` | `259` | `23.08%` | 主表已累计反标命中 |
| `CLOSED` | `0` | `0.00%` | 尚未执行人工 CLOSED 升级 |
| `BLOCKED` | `2` | `0.18%` | 保留原因标签，单独跟踪 |
| **合计** | **`1122`** | **`100.00%`** |  |

主表当前还有 5 个 WFI/InstrUncache `PARTIAL` 叶子没有 coverage 文本，属于状态与模型字段不一致，需要补充模型或调整状态后再进入统一统计。

### 2.3 回归结果

| 维度 | Verilator | VCS/Verdi |
| --- | --- | --- |
| 工具/基线 | 当前 manifest；DUT `7afd6f737...` | 本轮无可审计 aggregate |
| 回归范围 | 当前版本定向回归及已有历史回归证据复核 | `TBD` |
| 用例总数 | 本轮未形成单一全量 aggregate，按各专项回归分别统计 | `0` |
| PASS | `TBD（待统一汇总）` | `0` |
| FAIL | `TBD（待统一汇总）` | `0` |
| BLOCKED/拒绝 | 各专项回归结果待按统一基线汇总 | `TBD` |
| 结果 | 已完成多组 IFU、MMIO/NC、PredChecker 和 IBuffer 定向回归 | 未执行统一 VCS/SV 导出 |

当前已有多组 targeted run 用于逐点收敛，但未打包为同一 suite；本材料不将分散专项结果重复合并为全量覆盖率。

### 2.4 代码覆盖率

| 覆盖类型 | 当前 Verilator | 当前 VCS/Verdi | 说明 |
| --- | ---: | ---: | --- |
| Line | `TBD` | `TBD` | 当前 DUT/registry 基线没有兼容的全量 code coverage aggregate |
| Branch | `TBD` | `TBD` | 同上 |
| Toggle | `TBD` | `TBD` | 本轮未形成统一结果 |
| FSM state/transition | `TBD` | `TBD` | 本轮未形成统一结果 |

上轮 `asm_all19` 的 `Line 136230/230816 = 59.02%`、`Branch 386560/911590 = 42.41%` 只作为历史参考，不迁移为当前版本签核结果。

### 2.5 功能覆盖率

| 口径 | Modeled 分母 | HIT 分子 | 比例 | 说明 |
| --- | ---: | ---: | ---: | --- |
| Frontend BT 主表累计状态 | `1122` | `259` | `23.08%` | 主表累计状态；本材料不展开专项覆盖率分项结果 |
| 当前 canonical Python active bins | `528` | `TBD` | `TBD` | 当前 registry/sampler 演进后尚无单一兼容 aggregate |
| 当前版本专项回归观察 | `528` | `TBD` | `TBD` | 分散专项结果尚未形成兼容的全量 aggregate |

## 3. 过去两周设计变更及验证影响

本轮设计增量从上次 `kunminghu-v3 @ 00eb9ee7...` 前进到 `6891f912...`，通过 8 月 17 日和 8 月 24 日两次 merge 进入 `frontend-bt`。下表按功能影响归并列出，不把同一 PR 内的机械类型迁移拆成多项。

| 日期 | commit / PR | 设计改动 | 验证影响及动作 | 状态 | 对齐需求 |
| --- | --- | --- | --- | --- | --- |
| `2026-08-06` | `c7b373bdb` / `#6281` | FTQ/ICache meta read port timing 调整 | 已扩展 cacheable pipeline、metadata transfer、two-fetch source/waylookup 相关模型和用例 | `部分完成` | 确认时序调整不改变 meta 与 FTQ entry 的事务配对语义 |
| `2026-08-06` | `b3f72b99b` / `#6263` | PHR s1/s3 更新时序修正 | 当前以 BPU/FTQ 基础回归间接覆盖，缺 PHR 专项 evidence | `计划中` | 请设计给出建议观测点和最小训练序列 |
| `2026-08-07` | `766b9f264` / `#6315` | BPU WriteBuffer 同 entry 读写冲突修正 | 需补同 entry read/write conflict 与恢复场景 | `计划中` | 确认冲突优先级与期望写入可见拍 |
| `2026-08-08` | `97e650416` / `#6301` | 异常场景下 IFU `s1_instrCount` 修正 | 已补异常优先级、预译码、IBuffer 输出数量和写回一致性模型；已有针对性回归 | `完成/待设计确认` | 确认异常槽是否计入 instrCount 的最终规格 |
| `2026-08-13` | `325f8605f` 等 | 引入 `Pc` 类型替换 `PrunedAddr(VAddrBits)`，并补 strict/canonical/宽度相关改动 | 已完成新设计 DUT 构建和信号契约适配；地址边界与 canonical 专项仍需统一确认 | `部分完成` | 确认各接口是否统一保持 `VAddrBits+1` 语义及截断位置 |
| `2026-08-13` | `17e07d632` / `#6350` | MainBtb 使用属性表达 valid | 需要复核 valid 属性对替换、命中及训练覆盖点的影响 | `计划中` | 确认旧 valid 与新属性的等价边界 |
| `2026-08-14` | `ff4720da4` / `#6014` | 修复 satp flush 后首次取指异常 | 已新增/扩展地址翻译、权限、PTW fault、PMP/PMA 及 redirect 回归 | `部分完成` | 确认 satp flush 后首个 fault 的 PC/mtval/异常优先级 |
| `2026-08-17` | `3594ce9da` / `#6354` | 修正 IFU checkerRedirect taken | 已完成 PredChecker V3 模型、branch/JAL/JALR/Call/Ret 场景及优先级回归 | `完成/待设计确认` | 确认 earlier fault 屏蔽 younger taken 的优先级 |
| `2026-08-17` | `bb887b596`、`a026e93e7` / `#6349/#6358` | TAGE random allocate；FTQ 删除 dropTrainCounter | 现有 BPU 训练覆盖可做基础检查，尚缺随机分配与 counter 删除后的专项闭环 | `计划中` | 确认可观测的 allocation/训练验收条件 |
| `2026-08-18` | `6de780379` / `#6348` | BTB replacer 从 prediction touch 改为 training touch | 需新增 replacement touch 来源及 victim 选择场景 | `计划中` | 确认同周期 prediction/train 的 touch 优先级 |
| `2026-08-18` | `15ad523be`、`3dcd0d4d8` | IBuffer partial-drain valid/bits 一致性及 valid 路径优化 | 已补 IBuffer pointer、输出槽位、backpressure hold、reserved slot 相关模型与真实 DUT 用例 | `部分完成` | 确认 stall/partial drain/redirect 同拍时 valid 与 bits 必须原子保持 |
| `2026-08-19` | `0a1cc79f2` / `#6378` | 使用 PNR pointer 判断 two-pre/fetch | 已补 two-fetch eligibility、size/cross-page/exception 阻断、dual source 和 flush 场景 | `部分完成` | 确认 PNR 阻断第二块时第一块是否继续，以及 exception/redirect 优先级 |
| `2026-08-20~21` | `cb9ff1d8d`、`5d5e6cab0`、`a99a09b01` / `#6328/#6356/#6377` | TAGE UseAltOnNa、basePred reread、useful counter flush | 尚未形成 TAGE 专项全闭环 | `计划中` | 请设计明确本阶段必须签核的训练/flush场景 |
| `2026-08-21` | `91053ff25` / `#6375` | 预计算 `t0_firstMispredictMask` | PredChecker/earliest fault 场景可间接覆盖，内部 timing 路径不直接签核 | `部分完成` | 确认对外功能语义是否保持不变 |
| `2026-08-21` | `53a957667` / `#6237` | uBTB 使用 resolve + fast train | 需补 resolve/fast train 交叉及恢复场景 | `计划中` | 确认双训练源同周期优先级 |
| `2026-08-21` | `59086b8a5` / `#6398` | IFU redirect `canTrain` 增加 `notCfiTaken` | 已补 FTQ training、normal writeback 和 PredChecker 相关 contract/模型 | `部分完成` | 确认 `notCfiTaken` 的产生条件及与 redirect/taken 的合法组合 |

## 4. 本周期三人协作及量化进展

| Owner | 提交数 | 涉及文件数 | Python用例节点（上次→本次） | 覆盖率/测试点相关文件 | 主要进展 |
| --- | ---: | ---: | ---: | ---: | --- |
| 加柏文 | `50` | `52` | `151 → 271（+120）` | IFU、two-fetch、PredChecker、MMIO/NC及公共覆盖率基础设施 | 完成多类IFU模型、回归和累计反标，推动团队统一版本证据管理 |
| 芮尔涵 | `26` | `29` | `42 → 143（+101）` | `9` 个覆盖率/环境文件，另更新测试点主表 | 集中完善 ICache MainPipe、MissUnit、PrefetchPipe、WayLookup 和 two-fetch 覆盖率，修订反压、flush、refill、dedup、fence.i 等场景，并清理 `10` 个无效 xfail 用例 |
| 赵欣然 | `44` | `52` | `35 → 180（+145）` | 地址翻译、权限及验证环境相关文件，另更新测试点主表 | 完善 Sv39/Sv48 地址翻译、PMP/PMA 权限、PTW 各阶段时序及 fault response，新增 sector-lane rewalk、SFENCE 丢弃、随机回归和 source-bound redirect 场景 |

补充量化：本周期三人合计提交 `120` 次、涉及 `120` 个唯一文件；coverage registry 总项由 `405` 增至 `686`（`+281`），active Python bin 由 `247` 增至 `528`（`+281`），Python可收集节点达到 `800` 个，ASM源用例达到 `26` 个。三人分别围绕 IFU/ICache、地址翻译和公共环境协同推进测试点、覆盖模型与回归证据。

## 5. 对齐事项与遗留问题

### 5.1 上次遗留问题跟踪

| ID | 上次事项 | 本次状态 | 已采取动作 | 下一步 |
| --- | --- | --- | --- | --- |
| `A-001` | genhtml 不可用，缺 HTML code coverage | `进行中` | 本轮未用旧 raw summary 冒充当前结果 | 补齐工具后在锁定基线上重跑 |
| `A-002` | alignment report provenance 字段不完整 | `部分解决` | manifest/evidence 已增加 source delta、artifact hash 等字段 | 修正当前 registry/sampler 演进后的 aggregate 筛选与归并流程 |
| `A-003` | ABTB/BPU 变更缺专项 directed/funcov | `进行中` | 已补 PredChecker、FTQ training 与部分 two-fetch 场景 | 依据本轮设计确认补 TAGE/BTB/uBTB 专项 |
| `A-004` | baremode case 与 target bin 关系不清 | `部分解决` | 已扩展 canonical registry、累计反标和 testcase 绑定 | 剩余 smoke 明确为非coverage或补齐 target bin |

### 5.2 本次需要设计确认

| ID | 问题/需求 | 优先级 | 验收标准 | Owner |
| --- | --- | --- | --- | --- |
| `A-NEW-001` | 明确 `canTrain.notCfiTaken` 的产生条件、合法组合及 redirect 优先级 | `高` | 形成可直接转成 testcase/checker/bin 的真值关系 | `设计 + 验证团队` |
| `A-NEW-002` | 明确 PNR pointer 对 two-pre/fetch 的阻断边界 | `高` | 确认第二块被阻断时第一块、exception、redirect 的预期行为 | `设计 + 验证团队` |
| `A-NEW-003` | 明确 IBuffer partial-drain 下 valid/bits、backpressure、redirect 同拍语义 | `高` | 给出原子保持与丢弃规则，验证补齐对应断言/用例 | `设计 + 验证团队` |
| `A-NEW-004` | 明确 `Pc` 类型重构后的接口宽度、canonical 与截断约束 | `高` | 形成统一地址语义，更新 signal contract 和边界场景 | `设计 + 验证` |
| `A-NEW-005` | 确认 TAGE/BTB/uBTB 训练与替换变更的本阶段签核范围 | `中` | 设计给出必须覆盖的最小场景清单与可观测点 | `设计 + 验证团队` |
| `A-NEW-006` | MMIO 剩余 4 个测试点尚无覆盖率代码 | `中` | 新增模型并回填测试点；对应模块建模补齐 | `验证团队` |

## 6. 结论与决策记录

| 项目 | 当前建议 | Owner | 评审确认 |
| --- | --- | --- | --- |
| 版本基线是否锁定 | 设计锁定 `kunminghu-v3 6891f912...`；验证材料锁定 `c6cbdac5...`；真实 DUT evidence 锁定 `7afd6f737...` | `验证` | `待评审` |
| 当前回归能否作为签核证据 | 当前专项回归可用于验证对应场景；不能替代当前 registry 的全量兼容 aggregate | `验证` | `待评审` |
| 功能覆盖率阶段目标 | 当前主表累计 `HIT 259/1122`；active Python bin `528` 项，待统一版本基线后生成兼容 aggregate | `验证团队` | `待评审` |
| 本轮设计变更优先验证项 | `#6398`、`#6378`、IBuffer partial-drain、PC/canonical 地址语义 | `设计 + 验证` | `待评审` |
| 下一轮签核前动作 | 锁定新 HEAD 和 v3 SHA、刷新 DUT manifest、运行统一 suite、生成兼容 funcov/code coverage aggregate | `验证` | `待评审` |

## 7. 证据索引

| 证据类型 | 路径 | 对应版本/签名 | 备注 |
| --- | --- | --- | --- |
| 测试点主表 | `src/test/python/Frontend/docs/02_testpoint/Frontend_testpoint_0525_coverage_backannotated.csv` | HEAD `sha256:c3d2797a...` | 叶子 `1122` |
| coverage registry | `src/test/python/Frontend/docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv` | HEAD `sha256:7404f7e8...` | 总项 `686`，active `528` |
| DUT manifest | `build-frontend/frontend_build_manifest.verilator.json` | `sha256:2836ba78...` | DUT source `7afd6f737...` |
| Python 用例 | `src/test/python/Frontend/tests/py/` | HEAD `c6cbdac5...` | pytest collect `800` |
| ASM 用例 | `src/test/python/Frontend/tests/asm_cases/` | HEAD `c6cbdac5...` | `.S 26`，当前工作区 `.bin 26` |
| 上轮代码覆盖率 | `src/test/python/Frontend/data/runs/suites/20260812/162404_asm_all19` | 旧 DUT/registry 签名 | 仅作历史参考，不迁移签核 |

## 附：本轮统计和使用约束

1. 本材料按团队整体口径统计，专项覆盖率结果在各自材料中单独展开。
2. “HIT”仍需要真实回归命中证据；有 coverage 代码不等于已 HIT，PASS 也不自动等于对应目标 bin HIT。
3. 主表累计 HIT 用于项目进度管理；版本签核必须使用同一 DUT manifest、registry、sampler、signal contract 和 verification environment 签名下的兼容 aggregate。
4. 当前 active bin `528` 少于 registry 总项 `686`，差额 `158` 为历史或字段不完整的未激活项，不能直接进入 active 分母。
5. 本材料没有使用当前工作树在途修改提升任何统计数字；后续在途修改合入后，应重新生成签名和报告。
