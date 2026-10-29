# Memblock CSR Scalar 10k 功能覆盖矩阵集成修复 Plan

后续修订（2026-10-30）：本方案历史“普通26项全override”不再包含PBMT response-fault。
现为普通25项保留统一覆盖、PBMT1项使用fixed_cfg保持两UID、scalar4项保持本方案10k策略。
独立缺陷及实现证据见`memblock_pbmt_matrix_fixed_cfg_profile_plan_20261029.md`与对应review，不重写本方案历史验收记录。

## 1. 专有名词与抽象功能说明

### 1.1 专有名词

- **plus cfg**：由 `+KEY=VALUE` 行组成的运行期配置文件。代码落点是 Makefile 的 `plus_file/cfg`
  组合；例如 `tc_csr_scalar_load_10k.cfg` 定义精确事务数和 CSR stress profile。
- **plus-file directory**：保存 plus cfg 的目录。本 plan 中用户入口为 `--plus-file <dir>`，目录必须
  先规范化和验证，不能把任意文件路径直接当作 cfg 输入。
- **canonical path**：消除相对段和 symlink 后的物理绝对路径。它用于证明所选 cfg 最终仍直接位于
  已验证目录内。
- **frozen cfg**：controller 启动时复制到结果目录的只读 cfg 快照。后续 effective plusargs、compile
  和 simulation 只读取该快照，避免源 cfg 在回归期间变化。
- **effective plusargs**：cfg plusargs 与用户 runtime plusargs 按覆盖规则合并后的实际参数列表；同名
  runtime key 优先。
- **profile**：功能覆盖矩阵中对 cfg 目录和公共 override 策略的命名组合。`default` 指普通 26 项，
  `csr_scalar_10k` 指最后四个精确计数项。
- **dry-run**：runner 完成本地参数、cfg 冻结、effective plusargs 和 `config.txt` 生成后退出，不连接
  EDA 节点。
- **fake runner**：测试脚本注入的本地替身，只记录矩阵传给 runner 的参数并生成最小 PASS summary，
  用于审计 30 行参数路由，不执行编译或仿真。

### 1.2 修改对象的抽象功能

- `normalize_plus_file_dir()`：在正式读取 cfg 前，把用户目录输入收敛为可信、可审计的物理绝对目录；
  不负责选择 cfg 内容或合并 plusargs。
- `resolve_cfg_file()`：使用已经验证的目录和受限 cfg 名选择唯一 `.cfg` 文件，并阻止最终文件目标逃出
  目录；不把用户输入当作任意文件名。
- `resolve_case_profile()`：把矩阵行中的 profile 转为权威 plus cfg 目录和是否注入公共事务数的策略；
  未知 profile 直接失败。
- `run_eda_regression.sh` 主入口：解析 `--plus-file`，冻结选中 cfg，记录源路径和冻结路径，再沿用既有
  effective plusargs 与远端 worker flow。
- `run_function_cfg_cov_20261022.sh` 主循环：逐行解析四字段 CASE，按 profile 组装 runner 参数；普通
  26 项维持原行为，四个 CSR scalar 10k 项保留 cfg 自带精确事务数。

## 2. 问题与责任边界

### 2.1 已确认根因

1. `run_eda_regression.sh` 固定使用 `seq/plus_cfg`，最后四个正式 cfg 位于
   `seq/csr_scalar_stress/cfg`。
2. 覆盖矩阵对全部条目统一注入 `+MEMBLOCK_MAIN_TRANS_NUM=1000`，覆盖四个 cfg 的
   `10010/10012/10016/10025`。
3. CSR scalar sequence 的 10k 业务审计依赖这些精确计数，因此 `1000` 不是合法缩规模值。

### 2.2 范围

本 plan 只修改：

- `mem_ut/ver/ut/memblock/scripts/run_eda_regression.sh`
- `mem_ut/ver/ut/memblock/scripts/run_function_cfg_cov_20261022.sh`
- `mem_ut/ver/ut/memblock/scripts/tests/` 下新增 shell 测试和 fake fixture
- runner 使用规则、当前周测试框架 buglist、plan/review 文档

本 plan 不修改 RTL、Scala、SystemVerilog、cfg 内容、sequence、RM 或 DUT 接口。

## 3. Runner 安全目录入口

### 3.1 命令行接口

新增：

```text
--plus-file <dir>
```

默认值继续指向 `seq/plus_cfg`。显式相对路径按 controller 调用时的当前工作目录解析；绝对路径直接
解析。帮助文本和使用规则必须说明该语义。

### 3.2 `normalize_plus_file_dir()`

**抽象功能描述**：该 helper 在 cfg 选择前验证目录输入并返回物理绝对目录，确保后续代码不会把未验证
字符串传给 Makefile、冻结逻辑或远端 worker。

源码级伪代码：

```text
normalize_plus_file_dir(raw_dir):
  reject empty or CR/LF/TAB-containing input
  if raw_dir is relative:
    candidate = caller_pwd + raw_dir
  else:
    candidate = raw_dir
  require candidate is directory, readable, searchable
  canonical_dir = physical_pwd(candidate)
  require canonical_dir remains directory, readable, searchable
  return canonical_dir
```

中文顺序说明：先拒绝会破坏 `config.txt` 行结构的控制字符；相对路径只在 controller 本地解析一次；
通过进入目录并执行物理 `pwd` 消除 `.`、`..` 和目录 symlink；最后再次确认规范化目标可用。路径始终以
shell 数组参数传递，不使用 `eval` 或字符串拼接执行。

### 3.3 `resolve_cfg_file()`

**抽象功能描述**：该 helper 根据规范化目录和已通过 `valid_name()` 的 cfg 名定位唯一 cfg，并证明其
最终文件目标没有经 symlink 逃出允许目录。

源码级伪代码：

```text
resolve_cfg_file(canonical_dir, cfg_name):
  candidate = canonical_dir + "/" + cfg_name + ".cfg"
  require candidate is readable regular file
  resolved_file = realpath(candidate)
  require dirname(resolved_file) == canonical_dir
  require basename(resolved_file) == cfg_name + ".cfg"
       or resolved_file is an in-directory target selected by that fixed candidate
  return resolved_file
```

中文顺序说明：cfg 名继续使用现有受限 grammar，调用者不能提供 `/`；helper 固定追加 `.cfg`。随后对
候选文件做真实路径解析，并要求最终父目录仍等于规范化目录。允许目录内普通文件或目录内 symlink，拒绝
指向目录外部的文件。冻结目标名只由已验证 cfg 名生成。

### 3.4 冻结与配置记录

**抽象功能描述**：runner 主入口把验证后的源 cfg 复制为固定只读快照，并在 `config.txt` 同时记录源
目录、源 cfg、冻结 cfg 和 hash，后续流程只消费冻结副本。

修改后文字伪代码：

```text
parse --plus-file into PLUS_FILE_INPUT, defaulting to seq/plus_cfg
validate tc/cfg/ts/mode names and numeric options
PLUS_FILE = normalize_plus_file_dir(PLUS_FILE_INPUT)
CFG_FILE = resolve_cfg_file(PLUS_FILE, CFG)
create empty result directory
copy CFG_FILE to frozen_plus_cfg/CFG.cfg and chmod read-only
calculate frozen hash
ask Makefile for effective plusargs using only frozen_plus_cfg
write source_plus_file, source_cfg, frozen_cfg and cfg_sha256 to config.txt
dry-run exits here; compile/seed workers receive only frozen_plus_cfg
```

保持不变：默认目录行为、runtime plusarg 优先级、远端 cfg hash 校验、输出目录空目录约束和 compile/seed
worker 协议。

## 4. 功能覆盖矩阵 Profile 路由

### 4.1 CASE 格式

CASES 从：

```text
cfg|tc|ts
```

扩展为：

```text
cfg|tc|ts|profile
```

普通 26 项填 `default`，四个 CSR scalar 10k 项填 `csr_scalar_10k`。

### 4.2 `resolve_case_profile()`

**抽象功能描述**：该 helper 把受控 profile 名映射为 cfg 目录和公共事务数 override 策略，集中维护
矩阵差异，避免每行携带自由路径或散布 cfg 名特判。

源码级伪代码：

```text
resolve_case_profile(profile):
  case default:
    CASE_PLUS_FILE = canonical seq/plus_cfg directory
    CASE_OVERRIDE_TRANS_NUM = 1
  case csr_scalar_10k:
    CASE_PLUS_FILE = canonical seq/csr_scalar_stress/cfg directory
    CASE_OVERRIDE_TRANS_NUM = 0
  otherwise:
    print error and fail before invoking runner
```

中文顺序说明：矩阵只接受两个静态 profile。目录由脚本自身位置派生，不接受 CASE 行中的自由文件名。
返回值分别控制 `--plus-file` 和是否添加公共 `MEMBLOCK_MAIN_TRANS_NUM`。

### 4.3 主循环参数组装

**抽象功能描述**：矩阵主循环读取一条 CASE，解析 profile 后构造单次 runner 调用；它不修改 cfg 文件，
只决定目录和公共 override。

修改后文字伪代码：

```text
for each cfg|tc|ts|profile:
  require cfg, tc and ts match the runner name grammar and are not "." or ".."
  resolve profile to plus_file and trans_num policy
  build common args including --plus-file and CHECK_TRIGGER_EN=0
  if trans_num policy is enabled:
    append MEMBLOCK_MAIN_TRANS_NUM=TRANS_NUM
  first case appends --compile; later cases append --no-compile
  invoke runner and record summary
```

普通 26 项继续使用 `TRANS_NUM` 环境变量。四个 scalar 项不传同名 runtime plusarg，因此 Makefile 保留
cfg 内 `10010/10012/10016/10025`。四项仍传 `MEMBLOCK_CHECK_TRIGGER_EN=0`。

## 5. 测试方案

### 5.1 Runner dry-run 测试

新增 `test_run_eda_regression_plus_file.sh`：

1. 不传 `--plus-file` 跑普通 `default` cfg，确认 `source_plus_file` 为默认规范化目录，冻结 cfg 存在，
   effective plusargs 正常。
2. 分别对四个 scalar cfg 使用绝对专用目录执行 `--dry-run`，确认 `source_cfg`、`frozen_cfg`、hash 和
   effective plusargs，事务数分别且仅为 `10010/10012/10016/10025`。
3. 使用相对专用目录再跑一项，确认规范化结果与绝对路径相同。
4. 传不存在目录、普通文件、含控制字符目录和指向目录外 cfg 的 symlink，确认 rc=2 且输出明确拒绝原因。
5. 确认源 cfg 在 dry-run 后未被修改。

### 5.2 矩阵 fake runner 测试

新增 fake runner fixture 和 `test_run_function_cfg_cov_matrix.sh`：

1. fake runner 只解析参数、记录一行结构化调用并在 `--output-dir` 写最小 `summary.log`。
2. 通过环境变量把矩阵 runner 与结果根目录替换为临时路径；生产默认值不变。
3. 断言共调用 30 次，首项 `--compile`、其余 `--no-compile`。
4. 断言前 26 项使用默认目录且包含公共 `TRANS_NUM`。
5. 断言最后四项使用专用目录，不包含公共 `TRANS_NUM`，仍包含 `CHECK_TRIGGER_EN=0`。
6. 增加一个未知 profile fixture/测试入口，确认矩阵在调用 runner 前失败。

### 5.3 静态和真实入口检查

- 对生产脚本、测试脚本和 fixture 执行 `bash -n`。
- 执行两个新增 shell 测试和既有 identity-race fake matrix，防止 controller 回归。
- 执行四个 scalar 各自正式 `--dry-run` 并保留结果目录。
- 若 worktree 已有匹配 fingerprint/simv，可选跑一个完整 10k profile；不得通过 runtime plusarg 篡改精确
  计数。没有可复用可执行文件时不强制编译，因为本问题只修改 shell 集成层。
- 执行 `git diff --check`，确认未修改 RTL、Scala、SystemVerilog 或 cfg。

## 6. 文档与归档

1. 本问题属于仿真启动前即可静态确认的 runner/矩阵集成缺口，不满足 simulation buglist 对 testcase、
   日志和波形证据的触发条件；不得登记到当前周 V2 RM buglist，证据由本 plan/review 和单 issue commit
   承载。
2. 更新 `mem_ut_parallel_eda_regression_usage_rule.md` 的 `--plus-file` 参数、相对路径语义、安全校验和
   `config.txt` 字段。
3. 用户已明确授权测试框架 coding；独立 review 的 findings 必须修正并复审通过后合入。实现和验证完成后
   把本 plan 从 `plan/undo` 移到 `plan/do`。
4. implementation review 文档保留在 `review_doc/undo`，记录逐文件 diff、测试证据和 plan 对齐。
5. 全部门禁通过后创建一个 issue commit。

## 7. 风险与保持边界

- 不允许 `--plus-file` 接受具体文件路径；只接受目录，cfg 文件名始终由受限 `--cfg` 派生。
- 不允许源 cfg 直接传给远端 worker；必须继续使用冻结副本和 hash。
- 不改变普通 26 项公共 `TRANS_NUM` 行为。
- 不允许四个 scalar profile 注入任何 `MEMBLOCK_MAIN_TRANS_NUM` runtime override。
- 不复制四个 scalar cfg 到 `seq/plus_cfg`，避免双源维护。
- 不修改 RTL、Scala、SystemVerilog、cfg、sequence 或 RM。

## 8. 验收标准

1. `--plus-file` 默认兼容、绝对路径和相对路径均可用，非法目录及 cfg 目录逃逸被拒绝。
2. `config.txt` 记录规范化源目录、源 cfg、冻结 cfg 与 hash。
3. 30 项矩阵目录/profile 路由正确；普通 26 项保留公共 `TRANS_NUM`。
4. 四个 scalar effective plusargs 分别保留 `10010/10012/10016/10025`，没有 `1000` 覆盖。
5. dry-run、fake matrix、既有 controller 测试、`bash -n` 和 `git diff --check` 全部通过。
6. 独立 implementation review 无必须修改项；工作树只有本 issue 文件；单 commit 完成。

## 9. 与初步 plan 差异说明

用户给出的初步要求已直接整理为正式实现 flow，没有删除功能要求。正式 plan 补充了：

- 使用 profile 而不是在 CASE 中放自由目录字符串，缩小矩阵输入面。
- 对目录做物理路径规范化，并验证 cfg 最终文件目标仍位于该目录。
- `config.txt` 同时记录 source 与 frozen 路径，形成审计链。
- 用两个独立 shell 测试分别覆盖 runner dry-run 和矩阵 fake 参数路由。

这些补充只收紧安全和验证，不改变四个 CSR scalar 10k 用例的测试语义。
