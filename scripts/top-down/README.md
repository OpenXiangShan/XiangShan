# top-down 分析工具 ([English](#Top-down-Analysis-Tool))

本目录集成了 top-down 分析所需要的工具。在使用 [env-scripts](https://github.com/OpenXiangShan/env-scripts) 脚本完成 checkpoint 的运行后，可以使用本目录下的工具进行 top-down 分析。

## 使用方法

Top-down 分析工具目录结构如下：

```shell
# tree top-down
top-down
├── configs.py
├── draw.py
├── README.md
├── resources
│   └── spec06_rv64gcb_o2_20m.json
├── top_down.py
└── utils.py

1 directories, 6 files
```

### top_down.py 使用方法

`top_down.py` 是 Top-down 分析的主程序，用法如下：

```shell
# python top_down.py --help
usage: generate top-down results

optional arguments:
  -h, --help            show this help message and exit
  -b BASE_STAT_DIR, --base-stat-dir BASE_STAT_DIR
                        base stat output directory (required)
  -r REF_STAT_DIR, --ref-stat-dir REF_STAT_DIR
                        ref stat output directory (optional)
  -j JSON, --json JSON  specify json file (required)
  --base-issue BASE_ISSUE
                        base issue width (required when both --base-stat-dir and --ref-stat-dir are provided)
  --ref-issue REF_ISSUE
                        ref issue width (required when both --base-stat-dir and --ref-stat-dir are provided)
  --base-label BASE_LABEL
                        label prefix for base (default: BASE)
  --ref-label REF_LABEL
                        label prefix for ref (default: REF)
```

使用示例如下：

```shell
# python top_down.py -b <...>/SPEC06_EmuTasks_1021_0.3_c157cf -j resources/spec06_rv64gcb_o2_20m.json
# python top_down.py -b <...>/base_perf_report -r <...>/ref_perf_report -j <...>/checkpoint.json --base-issue 6 --ref-issue 8 --base-label Base --ref-label Feature1
```

默认配置下，脚本运行结束后会生成 `results` 目录：

```shell
# tree results
results
├── result_backend.png
├── result_custom.png
├── result_frontend.png
├── result_intel_topdown.png
├── result_mem.png
├── result_robhead.png
├── results_base.csv
├── results_ref.csv
├── results-weighted_base.csv
├── results-weighted_ref.csv
└── result_total.png

0 directories, 11 files
```

其中：

- `result_total.png` 为 Top-down 总体粗粒度聚合堆叠条形图，用于展示各 benchmark 在整体视角下的瓶颈构成。
- `result_frontend.png`、`result_backend.png`、`result_mem.png` 和 `result_custom.png` 分别对前端、后端、访存及自定义子维度进行进一步拆分，便于从特定维度开展更细粒度的瓶颈分析。
- `result_intel_topdown.png` 为类似 Intel Top-down 口径的派生分析图，将执行槽位拆分为 `Base`、`FetchLatencyBound`、`FetchBandwidthBound`、`BadSpec`、`CoreBound`、`L1Bound`、`L2Bound`、`L3Bound`、`MemBound` 和 `StoreBound`。
- `result_robhead.png` 为 ROB 头部等待原因分析图，将 `wait*Cycle` 计数器归一化拆分为 `mul`、`ldu`、`stu`、`fma`、`fdivsqrt` 和 `other`。
- `results_base.csv` 与 `results_ref.csv` 保存原始统计结果，按采样点记录各 Top-down 计数器数值。
- `results-weighted_base.csv` 与 `results-weighted_ref.csv` 则基于原始统计结果，结合各采样点对应权重完成加权汇总，可用于人工检查、后续分析或重新绘图。

### configs.py 使用方法

`configs.py` 是 Top-down 分析工具的配置文件，主要可配置项包括：

- **自定义子维度**：用户可在 `configs.py` 中修改 `xs_custom_rename_map`，以将部分阻塞归因重新聚类并生成对应图表。具体格式可参考其他子维度的重命名映射，例如 `xs_frontend_rename_map`。
- **benchmark_list**：用户可通过配置 `benchmark_list` 选择部分 checkpoint 子项进行单独绘图分析；若未额外指定，则默认对全部 checkpoint 作图。`INT_ONLY` 和 `FP_ONLY` 可用于快速筛选仅整数或仅浮点 benchmark。
- **xx_ANALYSE**：用户可通过配置此类选项，关闭部分子维度的绘图分析。
- **INTEL_TOPDOWN_ANALYSE**：打开后额外生成 `result_intel_topdown.png`。该分析依赖 `targets` 中的 `if_fetch_bubble`、`if_fetch_bubble_eq_max`、`inst_spec`、`recovery_bubble`、`mem_stall_anyload`、`mem_stall_store`、`mem_stall_l1miss`、`mem_stall_l2miss`、`mem_stall_l3miss` 和 `total_cycles` 等计数器，并需要正确的 issue width。
- **ROBHEAD_ANALYSE**：打开后额外生成 `result_robhead.png`。该分析依赖 `targets` 中的 `waitAluCycle`、`waitMulCycle`、`waitDivCycle`、`waitBrhCycle`、`waitJmpCycle`、`waitCsrCycle`、`waitFenCycle`、`waitBkuCycle`、`waitLduCycle`、`waitStuCycle`、`waitAtmCycle`、`waitfaluCycle`、`waitfmacCycle`、`waitfcvtCycle`、`waitfDivSqrtCycle` 和 `waitfcmpCycle` 等 ROB 头部等待周期计数器。

### draw.py 使用方法

`draw.py` 是 Top-down 分析工具的绘图脚本，主要用于基于已生成的 `results-weighted.csv` 直接绘图。用法如下：

```shell
# python3 draw.py --help
usage: draw top-down stacked bar chart (base/ref weighted csv)

options:
  -h, --help            show this help message and exit
  -b BASE_WEIGHTED_CSV, --base-weighted-csv BASE_WEIGHTED_CSV
                        base weighted csv path (default: results/results-weighted_base.csv)
  -r REF_WEIGHTED_CSV, --ref-weighted-csv REF_WEIGHTED_CSV
                        ref weighted csv path (default: results/results-weighted_ref.csv)
  --base-label BASE_LABEL
                        base label (default: BASE)
  --ref-label REF_LABEL
                        ref label (default: REF)
  --issue-width ISSUE_WIDTH
                        issue width for intel topdown analyse (default: 8)
```

使用示例如下：

```shell
# python3 draw.py --base-label Base --ref-label Feature1
# python3 draw.py -b <...>/xxxresults-weighted_base.csv -r <...>/xxxresults-weighted_ref.csv --base-label Base --ref-label Feature1
# python3 draw.py -b results/results-weighted_base.csv --base-label Base --issue-width 8
```

`draw.py` 会根据 `configs.py` 中开启的 `TOTAL_ANALYSE`、`BACKEND_ANALYSE`、`FRONTEND_ANALYSE`、`MEM_ANALYSE`、`CUSTOM_ANALYSE`、`INTEL_TOPDOWN_ANALYSE` 和 `ROBHEAD_ANALYSE` 依次生成对应图表。直接运行 `top_down.py` 时，绘图阶段的 issue width 会由 `--base-issue`、`--ref-issue` 推导；单独运行 `draw.py` 重新绘图时，需要通过 `--issue-width` 指定 Intel Top-down 分析使用的 issue width，默认值为 8。

---

# <div id="Top-down-Analysis-Tool">Top-down Analysis Tool</div>

This directory contains analysis tool for top-down. After running checkpoints by using [env-scripts](https://github.com/OpenXiangShan/env-scripts), you may use the tool to analyze top-down counters.

## Usage

The directory structure of the Top-down analysis tool is shown below:

```shell
# tree top-down
top-down
├── configs.py
├── draw.py
├── README.md
├── resources
│   └── spec06_rv64gcb_o2_20m.json
├── top_down.py
└── utils.py

1 directories, 6 files
```

### How to use top_down.py

`top_down.py` is the main entry of the Top-down analysis tool. Its usage is as follows:

```shell
# python top_down.py --help
usage: generate top-down results

optional arguments:
  -h, --help            show this help message and exit
  -b BASE_STAT_DIR, --base-stat-dir BASE_STAT_DIR
                        base stat output directory (required)
  -r REF_STAT_DIR, --ref-stat-dir REF_STAT_DIR
                        ref stat output directory (optional)
  -j JSON, --json JSON  specify json file (required)
  --base-issue BASE_ISSUE
                        base issue width (required when both --base-stat-dir and --ref-stat-dir are provided)
  --ref-issue REF_ISSUE
                        ref issue width (required when both --base-stat-dir and --ref-stat-dir are provided)
  --base-label BASE_LABEL
                        label prefix for base (default: BASE)
  --ref-label REF_LABEL
                        label prefix for ref (default: REF)
```

Example commands:

```shell
# python top_down.py -b <...>/SPEC06_EmuTasks_1021_0.3_c157cf -j resources/spec06_rv64gcb_o2_20m.json
# python top_down.py -b <...>/base_perf_report -r <...>/ref_perf_report -j <...>/checkpoint.json --base-issue 6 --ref-issue 8 --base-label Base --ref-label Feature1
```

With the default configuration, a `results` directory will be generated after execution:

```shell
# tree results
results
├── result_backend.png
├── result_custom.png
├── result_frontend.png
├── result_intel_topdown.png
├── result_mem.png
├── result_robhead.png
├── results_base.csv
├── results_ref.csv
├── results-weighted_base.csv
├── results-weighted_ref.csv
└── result_total.png

0 directories, 11 files
```

Among them:

- `result_total.png` is the coarse-grained stacked bar chart for overall Top-down analysis, showing the bottleneck composition of each benchmark from a global perspective.
- `result_frontend.png`, `result_backend.png`, `result_mem.png`, and `result_custom.png` further break down the frontend, backend, memory, and custom sub-dimensions, respectively, making it easier to perform fine-grained bottleneck analysis from specific viewpoints.
- `result_intel_topdown.png` is a derived Intel-like Top-down view. It partitions execution slots into `Base`, `FetchLatencyBound`, `FetchBandwidthBound`, `BadSpec`, `CoreBound`, `L1Bound`, `L2Bound`, `L3Bound`, `MemBound`, and `StoreBound`.
- `result_robhead.png` is a ROB-head wait reason view. It normalizes `wait*Cycle` counters into `mul`, `ldu`, `stu`, `fma`, `fdivsqrt`, and `other`.
- `results_base.csv` and `results_ref.csv` store the raw statistical results, recording the values of each Top-down counter at each sampling point.
- `results-weighted_base.csv` and `results-weighted_ref.csv` provide weighted summaries based on the raw results and the corresponding weights of sampling points, and can be used for manual inspection, further analysis, or redrawing figures.

### How to use configs.py

`configs.py` is the configuration file of the Top-down analysis tool. The main configurable items include:

- **Custom sub-dimensions**: Users can modify `xs_custom_rename_map` in `configs.py` to regroup selected stall attributions and generate customized plots. The format can refer to the rename maps of other sub-dimensions, such as `xs_frontend_rename_map`.
- **benchmark_list**: Users can configure `benchmark_list` to select a subset of checkpoints for standalone plotting and analysis. If not specified, all checkpoints will be plotted by default. `INT_ONLY` and `FP_ONLY` can be used to quickly generate plots for integer-only or floating-point-only benchmarks.
- **xx_ANALYSE**: Users can disable plotting for selected sub-dimensions through this type of option.
- **INTEL_TOPDOWN_ANALYSE**: When enabled, the tool additionally generates `result_intel_topdown.png`. This analysis depends on counters in `targets`, including `if_fetch_bubble`, `if_fetch_bubble_eq_max`, `inst_spec`, `recovery_bubble`, `mem_stall_anyload`, `mem_stall_store`, `mem_stall_l1miss`, `mem_stall_l2miss`, `mem_stall_l3miss`, and `total_cycles`, and requires the correct issue width.
- **ROBHEAD_ANALYSE**: When enabled, the tool additionally generates `result_robhead.png`. This analysis depends on ROB-head wait cycle counters in `targets`, including `waitAluCycle`, `waitMulCycle`, `waitDivCycle`, `waitBrhCycle`, `waitJmpCycle`, `waitCsrCycle`, `waitFenCycle`, `waitBkuCycle`, `waitLduCycle`, `waitStuCycle`, `waitAtmCycle`, `waitfaluCycle`, `waitfmacCycle`, `waitfcvtCycle`, `waitfDivSqrtCycle`, and `waitfcmpCycle`.

### How to use draw.py

`draw.py` is the plotting script of the Top-down analysis tool. It is mainly used to directly generate figures from the extracted `results-weighted.csv`. Its usage is as follows:

```shell
# python3 draw.py --help
usage: draw top-down stacked bar chart (base/ref weighted csv)

options:
  -h, --help            show this help message and exit
  -b BASE_WEIGHTED_CSV, --base-weighted-csv BASE_WEIGHTED_CSV
                        base weighted csv path (default: results/results-weighted_base.csv)
  -r REF_WEIGHTED_CSV, --ref-weighted-csv REF_WEIGHTED_CSV
                        ref weighted csv path (default: results/results-weighted_ref.csv)
  --base-label BASE_LABEL
                        base label (default: BASE)
  --ref-label REF_LABEL
                        ref label (default: REF)
  --issue-width ISSUE_WIDTH
                        issue width for intel topdown analyse (default: 8)
```

Example commands:

```shell
# python3 draw.py --base-label Base --ref-label Feature1
# python3 draw.py -b <...>/xxxresults-weighted_base.csv -r <...>/xxxresults-weighted_ref.csv --base-label Base --ref-label Feature1
# python3 draw.py -b results/results-weighted_base.csv --base-label Base --issue-width 8
```

`draw.py` generates the enabled figures according to `TOTAL_ANALYSE`, `BACKEND_ANALYSE`, `FRONTEND_ANALYSE`, `MEM_ANALYSE`, `CUSTOM_ANALYSE`, `INTEL_TOPDOWN_ANALYSE`, and `ROBHEAD_ANALYSE` in `configs.py`. When running `top_down.py` directly, the issue width used by the plotting stage is inferred from `--base-issue` and `--ref-issue`. When rerunning `draw.py` standalone, use `--issue-width` to specify the issue width for Intel Top-down analysis. The default value is 8.

### 前端计数器口径 / Frontend counter semantics

前端 bubble 在 IBuffer 展开为缺失的指令槽，并在 Dispatch 累计，因此数值表示损失槽数，不能直接当作周期数。
Dispatch 每个槽接收一个原因编号，按 `PopCount(reason == counterId)` 累加。前端也传单个原因编号，
无需 one-hot 位向量或依赖枚举顺序的优先级编码器。后端重定向覆盖 IFU 重定向，重定向整体替换旧原因；
BPU override 仅标记实际冲掉的阶段，等待原因在相应供给阶段产生。

`UBTBMissBubble`、`ABTBMissBubble`、`UTAGEMissBubble`、`URASMissBubble` **仅统计 override 损失**，
归责于被纠正的早级预测中提供错误字段的组件。预测通过 S3 校验后，无论 S3 是否发出 override，
IFU/precheck/后端纠错都归责 S3 的主 BTB、TAGE、SC、ITTAGE 或 RAS。
主 BTB 负责分支存在性、位置、类型与自身提供的目标；方向错误按该分支实际使用的 SC/TAGE/BTB 计数，
包含 not-taken；目标错误按实际使用的 RAS/ITTAGE/BTB 计数。预测错误归 BadSpec。

`FtqUpdateBubble` 仅在训练让行造成待取指目标供应中断时统计，包含必要恢复空拍，归前端供给损失。
预取超前距离限制、仍有目标可消费的 FTQ 满/训练背压不直接记损失。
`FtqFullStall` 表示容量门槛实际阻止 fetch；`FtqEmptyBubble` 表示排除训练/容量/重定向后的目标队列空缺，
包括复位初始化和正常预测入队流水延迟。`ICacheSupplyBubble` 表示有 FTQ 请求却等待 WayLookup 元数据或数据阵列端口；
cacheable miss/refill 使用 `ICacheMissBubble`，翻译缺失使用 `ITLBMissBubble`。

`UncacheFetchBubble` 包含 uncache/MMIO 等待、单指令交付余槽及重定向恢复；
`IbufferStructure` 表示有效 enqueue 无法与不足宽度的 dequeue 合并；`FetchFragBubble` 表示有效短块 bypass 的余槽，
包括还未组成一条完整指令的有效块。无 enqueue 时沿用上游原因；有效指令槽为 `NoStall`。
`FrontendOtherCoreStall` 保留为未覆盖原因的诊断计数，验收要求为零，不作为正常分类。

Frontend counters count **lost instruction slots**, not wait cycles. Early predictors own override penalties only;
IFU/backend corrections always blame the S3 field provider. Training and ordinary target/cache supply losses belong to
Frontend; prediction errors belong to BadSpec. New logs are required by these mappings.
