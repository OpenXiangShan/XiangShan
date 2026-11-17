# V2 三方案实现后的 basicTest 全量回归记录（2026-11-17）

## 1. 术语与记录边界

| 术语 | 本文含义 | 对应产物 |
|---|---|---|
| `DUT` | 实际参与仿真的 MemBlock RTL；请求级 DTLB 场景也属于真实 DUT 测试 | VCS 仿真与接口观测 |
| `soft/framework` | 依靠软件模型、公共 API 或配置运输断言判定的框架自测 | 独立框架 CSV |
| `campaign` | 冻结清单下按固定 index、参数、seed 执行的一整轮测试 | `inventory.csv`、`manifest.json` |
| `controller` | 调用正式远端回归脚本、收敛结果的单组合执行过程 | `summary.log`、`results.tsv` |
| `index` | 本轮稳定组合编号，范围 000～092 | 两份 CSV 的 `index` 列 |
| `retry` | 保留原记录、沿原 seed 使用新目录复测，不额外增加计划数量 | `_retry1` 与恢复记录 |
| `override` | UVM factory 类型替换；本轮负例需要分别替换 CSR 与 Fence driver | 两条 UVM 参数 |
| `snapshot` | 本文与两份 CSV 共同采用的一次结果快照，后续恢复由主代理更新 | `basic_results_doc_snapshot_20261117.json` |

本文记录测试入口、参数、执行证据和验证边界，不展开框架实现算法。运行计数快照截止于本轮 index014 发布最终结果（日志时间 **2026-11-17 22:53:25 CST**），责任分类已结合随后23:01:32发布的精确复现结果与两轮review更新。整轮尚未完成：**93 个组合、50 个入口中，14 个通过、1 个已确认 RTL 缺陷失败、78 个尚未执行；campaign 状态为 `STOPPED_RTL_CONFIRMED`**。尚未关闭的问题和未执行组合不能记为通过。原参数波形复现及两轮独立 review 已确认 RTL 责任。

## 2. 实现版本与已完成专项

工作树为 `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_memblock_virtualization_20261117`，分支为 `codex/memblock-virtualization-20261117`。三个方案已按顺序独立本地提交：

| 顺序 | 提交 | 实现与文档入口 |
|---|---|---|
| 1 | `6f60bb6c34` | 后端周期帧、异常 redirect/LSQ 控制同步与异常地址采样；见[实现 review](../../plan/test_framework/review_doc/undo/memblock_v2_backend_cycle_frame_implementation_review_20261117.md) |
| 2 | `e1767d7e61` | HLV/HLVX/HSV 公共流程、SPVP/CSR 上下文和 HFENCE/HINVAL 控制；见[实现 review](../../plan/test_framework/review_doc/undo/memblock_virtualization_framework_implementation_review_20261117.md) |
| 3 | `4240e4247d` | 冻结访存快照、Guest 权限/结果检查、PMM 及异常地址检查；见[实现 review](../../plan/rm_review_doc/undo/memblock_v2_virtualization_rm_implementation_review_20261117.md) |

三份 plan 已归入 `do`；review 按规则保留在 `undo`。该状态代表实现及专项证据已登记，不表示本轮全量回归已经验收。

已完成专项证据如下，历史失败版本仍保留，最终结果以对应稳定日志为准：

| 专项 | 已确认结果与边界 |
|---|---|
| 后端运输 | 两真实 driver 的 81 项检查通过；正常 Store、Store PF、flushSb 已验证；Store PF 的波形核对实际 DUT 输入同拍及地址采样时序 |
| 虚拟化框架 | 混合 994 项、单权重 1122 项框架检查及 15 个配置运输负例通过；包含动态 CSR、legacy 和 HINVAL 路径 |
| 最终 rm5 软件自测 | seed6111801，95 个场景、184 项检查通过，稳定结果无 UVM_ERROR/FATAL；预期负例的 caught report 不作为未捕获错误 |
| 真实 Guest 矩阵 | seed6111810～6111839，共 30 组通过，包含 13 个操作的 SPVP0/1 及 PF/GPF/AF 场景 |
| 真实 PMM | seed6111900～6111903，共 4 组通过，包含 PMM10、PMM11、关闭 PMM 的预访问错误及 only-S2 别名 |
| 普通基线 | 空 virtual sequence 与普通 dispatch smoke 通过 |
| 旧 ROB fixture | 补齐冻结 CSR 与 WAITING 索引后，重编原 seed6111999 通过，四个 redirect/后页迟到 completion 场景完成 |

专项权威目录根为 `/nfs/home/lixiangrui/.tmp/memblock_virtualization_impl_20261117`：`rm5_selftest`、`virt_rm5_20261117_real_00...29*`、`virt_rm5_20261117_pmm_0...3`、`rm5_baseline_0`、`rm5_baseline_1`、`rm5_rob_retest`。PMM 的 IO+GPF 优先级另有静态复核，本文不将其声称为新增动态覆盖。

## 3. 全量清单与分类

本轮清单来自逐入口核对后的 `~/.tmp/basic_full_inventory_validated.csv`，执行前冻结为 campaign 下的 `inventory.csv`。两文件 SHA256 相同：

```text
5c2175b081f979c4aa7f0efcef830577db7e620ba2d93a7e094426d63ec8705c
```

本轮保留原 89 个组合并加入 4 个真实 PMM 入口，共 **93 组合/50 入口**。仅排除直接运行必定 fatal 的 CSR stress 抽象基类，空 `virtual_base_sequence` 则作为环境启动基线保留。

长期清单按结果依赖分类，逐组合一行：

- [真实 DUT CSV](../../seq/basic_full_real_dut_testcases_20261117.csv)：81 行。
- [框架自测 CSV](../../seq/basic_full_soft_framework_testcases_20261117.csv)：12 行。

每行保存稳定编号、`tc/ts/cfg/plus_file`、原始及实际 `plusargs`、seed、状态、结果目录和分类证据。`testcase_name` 带 index，用于区分同一入口、同一 cfg 下不同错误注入参数。分类已查阅原 feature CSV 和实际 VSEQ/继承链；没有直接将库存中的 `preset`、`registered` 等描述当作分类结论。

特殊分类边界：

- index001 后端运输自测通过真实 driver/monitor 验证 DUT 交接端口，归入真实 DUT。
- index002 通过故障注入检查 CSR/Fence 配置运输断言，不检查 DUT 访存计算结果，归入框架自测。
- index054～070 是真实 DTLB 请求级测试，归入真实 DUT，但不宣称整条指令退休覆盖；两项训练入口也执行真实 DUT 流程。
- index073、075、077～083 分别调用软件模型或公共 API 自测；index084 是空基线，均归入框架自测。
- index085～088 CSR scalar stress 和 index089～092 PMM 均执行真实访存流程。

参数规模保持原样。三个 `100k` preset 均保留 `MEMBLOCK_MAIN_TRANS_NUM=100000`；`tc_sfence_stress_10k` 保留 10000。四个 CSR scalar stress 分别保留 10010、10012、10016、10025 条，不将这些规模缩成 smoke。运行参数中也未追加更小的条目数量覆盖。

## 4. 执行与判定方式

使用已编译的 `virt_rm5_20261117`，从 `mem_ut/ver/ut/memblock/sim` 调用正式 `run_eda_regression.sh`，逐组合运行固定 seed：`seed = 6112000 + index`。各组合保留原 cfg、plus-file 与 plusargs；开启 monitor/record，错误阈值为 1，不开启波形/coverage/fcov，复用同版 simv，不重复编译。

临时顺序 runner 位于 `~/.tmp/run_basic_full_campaign.py`，未修改正式回归脚本或测试源码。每个组合必须同时满足以下条件才记为通过：

1. controller 退出码为 0。
2. `summary.log` 为 `PASS`，计划/完成/收敛/通过均为 1，剩余/未运行/失败/基础设施错误/不完整/取消/pending/远端未知任务均为 0，停止原因为空。
3. `seeds.txt` 恰为本组合原 seed。
4. `results.tsv` 恰有一条对应 seed，状态为 `PASS/EXIT_CONFIRMED`，rc 为 0，未捕获 UVM 错误数为 0。
5. 指向的稳定最终日志存在；summary 的记录基础设施错误与 simv 变化标志均为 0。

恢复仅跳过重新严格核验仍通过的组合；已有非通过或证据缺失目录会停止，需要完成责任分类、原 seed 专项复测并指定新的空目录，不能把存在目录当作通过。恢复不删除旧记录，不换 seed 绕过失败，不将 retry 计为新组合。

## 5. index002 的 UVM 参数规范化及恢复

原库存包含两条必需的 `+uvm_set_type_override`，分别替换 Fence 和 CSR driver。正式回归脚本按原 key 拒绝重复，在远端任务创建前本地退出 2。原 controller 日志和 `results.jsonl` 失败记录保留；该次没有 summary/results，不算 DUT 功能失败，也不能记为通过。

独立核对实际 UVM1.2 源码 `/nfs/tools/synopsys/vcs/Q-2020.03-SP2/etc/uvm-1.2/base/uvm_root.svh`：`m_do_factory_settings()` 使用同一表达式匹配 `UVM_SET_TYPE_OVERRIDE|uvm_set_type_override`，逐条执行同一类型替换处理，两种拼写同义。因此临时 runner 仅将第二条 key 改成大写，两个 value 和顺序保持原样：

```text
+uvm_set_type_override=fence_agent_agent_driver,memblock_csr_mapping_negative_fence_driver
+UVM_SET_TYPE_OVERRIDE=csr_ctrl_agent_agent_driver,memblock_csr_mapping_negative_csr_driver
```

原库存与冻结 SHA 未改，实际参数另存 `002.recovery.json`，CSV 同时保留 `plusargs` 与 `effective_plusargs`。这不是删除重复测试动作，也未放宽正式脚本的参数检查。原 seed6112002 使用新目录 `_retry1` 恢复，稳定日志 79/80 行分别确认两条 factory override，拓扑与 driver 日志确认两个负例 driver 均生效，15 个负例完成；最终 controller/summary/results 严格通过，计入 index002 一次。原失败和恢复结果均保留在历史中。

## 6. 本次快照与待处理结果

campaign 根目录：

```text
/nfs/home/lixiangrui/.tmp/memblock_virtualization_impl_20261117/virt_rm5_20261117_basic_full
```

| 范围 | seed | 本次快照状态 |
|---|---|---|
| index000～013 | 6112000～6112013 | 14 组合通过，其中 index002 使用同 seed retry1 |
| index014 | 6112014 | 失败，远端已 `EXIT_CONFIRMED`，未捕获 UVM 错误为 1；两轮独立 review 确认 RTL |
| index015～092 | 6112015～6112092 | 78 组合尚未执行 |

index014 的入口为 `memblock_dispatch_real_cbo_directed_vseq`，cfg 为 `tc_dispatch_real_cbo_zero_uncache`，附加参数是 `+MEMBLOCK_UNCACHE_DENIED_WT=0 +MEMBLOCK_UNCACHE_CORRUPT_WT=100`。首错为：

```text
RM_EXCEPTION_ADDRESS_MISMATCH
uid=2 epoch=0 frame=6 identity=1
VA actual=xxxxxxxxxxxxxxxx expected=0000000080002008
GPA actual=xxxxxxxxxxxxxxxx expected=0000000000000000 valid=0
VSnonleaf actual=x expected=0
```

已通过原参数波形与两轮独立 review 完成分类：NC 返回未携带冻结异常地址，LoadUnit2 无新 TLB 请求却使用其始终为 X 的响应；同 ROB/uop 新记录在 1045.1ns 覆盖正确异常地址，顶层在 1050.1ns 变 X，早于 1800.1ns redirect。故不是 S+2 采样错误。见[正式 RTL Buglist](../../buglist/rtl/v2/rtl_buglist_2026-W47_20261116.md)。不修改 RTL、不放宽 checker，按项目规则停止剩余回归。

快照文件为 `~/.tmp/basic_results_doc_snapshot_20261117.json`，保留当时 `results.jsonl`、manifest 和状态表内容。两份新增 CSV 使用同一运行计数快照，并已同步最终RTL责任与停止状态；原有各 feature CSV 不在本次文档任务中改动。后续状态由主代理在回归恢复后统一更新，本文未宣称整轮完成。


## 7. 最终停止、复现与恢复条件

第一轮debug误漏原附加corrupt=100，结果通过但不作为原故障反证；`basic14_debug_exact` 恢复原两项plusargs后，在同一1818.100ns精确复现。两次原故障运行均最终FAIL、EXIT_CONFIRMED、remote_unknown_jobs=0，monitor记录已提交。源码、波形及两轮独立review链接和详细修复建议均进入正式Buglist。RTL没有修改。

最终处置为 **STOPPED_RTL_CONFIRMED**，不等于COMPLETE。三项实现提交与专项通过保持有效，整轮尚欠失败原seed修复复测及78组合执行。恢复清单位于campaign目录的 `remaining_original_jobs.json`、`remaining_original_seeds.txt`，保留index015..092、seed6112015..6112092及原cfg/plusargs；失败seed6112014单独先复测。冻结库存、原失败结果和全部历史均保留。

后续RTL修复须另行授权，修复后重新生成/编译，原basicTest/ts/cfg/seed6112014及denied0、corrupt100原参数专项通过，再由新的空结果目录恢复剩余清单，不能直接用当前runner越过index014。未降低100K/10K等原有规模。

最终结果文档由独立reviewer `/root/rm_prepare` 复核通过，报告 `/nfs/home/lixiangrui/.tmp/results_final_review.md`；分类、计数、CSV、专项及两轮RTL责任证据一致。
