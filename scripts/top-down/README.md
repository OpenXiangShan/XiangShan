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
