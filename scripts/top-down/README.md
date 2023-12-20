# top-down 分析工具 ([English](#Top-down-Analysis-Tool))

本目录集成了 top-down 分析所需要的工具。在使用 [env-scripts](https://github.com/OpenXiangShan/env-scripts) 脚本完成 checkpoint 的运行后，可以使用本目录下的工具进行 top-down 分析。

## 使用方法

``` shell
# python top_down.py --help
usage: generate top-down results

optional arguments:
  -h, --help            show this help message and exit
  -s STAT_DIR, --stat-dir STAT_DIR
                        stat output directory
  -j JSON, --json JSON  specify json file
```

举例：

``` shell
# python top_down.py -s <...>/SPEC06_EmuTasks_1021_0.3_c157cf -j resources/spec06_rv64gcb_o2_20m.json
# python top_down.py -s <...>/SPEC06_EmuTasks_1215_allbump -j <...>/spec06_rv64gcb_O3_20m_gcc12.2.0-intFpcOff-jeMalloc/checkpoint-0-0-0/cluster-0-0.json
```

脚本运行结束后，会生成 `results` 目录：

``` shell
# tree results
results
├── result.png
├── results.csv
└── results-weighted.csv

0 directories, 3 files
```

其中，`result.png` 为 top-down 堆叠条形统计图，`results.csv` 为各采样点的 top-down 计数器，`results-weighted.csv` 为各子项的加权 top-down 计数器。

# <div id="Top-down-Analysis-Tool">Top-down Analysis Tool</div>

This directory contains analysis tool for top-down. After running checkpoints by using [env-scripts](https://github.com/OpenXiangShan/env-scripts), you may use the tool to analyze top-down counters.

## Usage

``` shell
# python top_down.py --help
usage: generate top-down results

optional arguments:
  -h, --help            show this help message and exit
  -s STAT_DIR, --stat-dir STAT_DIR
                        stat output directory
  -j JSON, --json JSON  specify json file
```

Some examples:

``` shell
# python top_down.py -s <...>/SPEC06_EmuTasks_1021_0.3_c157cf -j resources/spec06_rv64gcb_o2_20m.json
# python top_down.py -s <...>/SPEC06_EmuTasks_1215_allbump -j <...>/spec06_rv64gcb_O3_20m_gcc12.2.0-intFpcOff-jeMalloc/checkpoint-0-0-0/cluster-0-0.json
```

A `results` directory would be generated then:

``` shell
# tree results
results
├── result.png
├── results.csv
└── results-weighted.csv

0 directories, 3 files
```

The `result.png` is a stacked bar chart of top-down. The `results.csv` contains per-checkpoint top-down counters. And the `results-weighted.csv` contains weighted counters for all sub tests.
