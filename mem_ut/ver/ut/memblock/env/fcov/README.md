# memblock 功能覆盖率目录与本地配置

功能覆盖率按观测模块分为四组。模型、bind 和对应 SVA 文件放在同一目录；SVA 当前仅为预留空壳。

| 目录 | 模块控制宏 | 文件数 | bind 语句数 | 范围 |
|---|---|---:|---:|---|
| `vector_unit_stride/` | `MEMBLOCK_FCOV_VECTOR_US_EN` | 2 | 1 | MemBlock 向量 unit-stride 覆盖率 |
| `load_unit/` | `MEMBLOCK_FCOV_LOAD_UNIT_EN` | 3 | 2 | 标量 LoadUnit 覆盖率及 SVA 空壳 |
| `store_unit/` | `MEMBLOCK_FCOV_STORE_UNIT_EN` | 3 | 2 | 标量 StoreUnit 覆盖率及 SVA 空壳 |
| `dtlb/` | `MEMBLOCK_FCOV_DTLB_EN` | 6 | 18 | core/control/storage/filter/integration 全部模型 |

合计 14 个文件、4 个 bind 文件、23 条 bind 语句。DTLB 的 18 条语句均使用 `bind`，其中 17 条带参数；不能将带参数的语句统计成直接例化。模型定义通过 `env/memblock_env.f` 编译，四个 bind 文件由 `tb/top_tb.sv` 引入。

## 默认行为与参数归属

四个模块宏默认均不定义，因此功能覆盖率默认全部关闭。覆盖模型及其 bind 同时满足以下条件才参与编译：

1. Makefile 使用 `fcov=on`，产生全局宏 `MEMBLOCK_UT_FCOV`。
2. 对应模块宏已经定义。

DTLB 只使用一个模块宏，不对五类模型细分开关。定义为 `0` 仍会被预处理器视作已定义；关闭时应删除或注释定义行。

统一入口为 `cfg/memblock_compile_params.svh`，它引入个人文件 `cfg/user_compile_cfg.local.svh`。个人文件被 Git 忽略，缺少时由 Makefile 从 `cfg/user_compile_cfg.local.default.svh` 创建。已有个人文件不会被覆盖。此配置属于编译期参数，`user_cfg.local.sv`、运行期 env cfg 或 plusargs 无法改变已经编译的模型和 bind。

## 本地启用示例

从仓库根目录进入仿真目录，先生成默认个人文件：

```bash
cd mem_ut/ver/ut/memblock/sim
make ensure_user_compile_cfg_local
```

编辑 `../cfg/user_compile_cfg.local.svh`，例如只开启 LoadUnit 和 DTLB：

```sv
`define MEMBLOCK_FCOV_LOAD_UNIT_EN
`define MEMBLOCK_FCOV_DTLB_EN
```

随后重新编译并运行：

```bash
make eda_run tc=tc_sanity mode=fcov_local fcov=on
```

使用并发回归脚本时，使用 `--fcov` 打开全局开关：

```bash
../scripts/run_eda_regression.sh --tc basicTest --cfg default \
  --mode fcov_local_regression --compile --fcov --seeds 11,22,33 --jobs 3
```

`--coverage` 控制 VCS 代码覆盖率，`--fcov` 控制这里的功能覆盖率，两者独立。修改本地宏后必须重新编译；`--no-compile --fcov` 会检查编译指纹中的功能覆盖率开关及本地配置内容摘要，不匹配时拒绝复用。绕过 sim/Makefile 直接调用工具前，也必须先创建个人编译头。
