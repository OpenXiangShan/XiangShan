# Memblock CSR Scalar 10k 功能覆盖矩阵集成修复 Implementation Review

关联 plan：`AI_DOC/plan/test_framework/plan/do/memblock_csr_scalar_functional_matrix_integration_fix_plan_20261025.md`

Review 日期：2026-10-25
责任类别：测试框架
基线：`045011bf6bd912410395eeadf01b007a2c11339b`
状态：`REVIEW_PASS`

## 1. 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 对应代码对象或落点 | 使用场景/示例 |
|---|---|---|---|
| `plus-file directory` | runner 允许读取 plus cfg 的源目录，不是任意 cfg 文件入口 | `PLUS_FILE_INPUT`、`PLUS_FILE` | `seq/csr_scalar_stress/cfg` |
| `canonical path` | 消除相对段和目录 symlink 后的物理绝对路径 | `normalize_plus_file_dir()` | 相对目录最终记录为绝对目录 |
| `containment` | cfg 真实目标的父目录必须等于已验证源目录 | `resolve_cfg_file()` | 拒绝指向目录外部的 cfg symlink |
| `frozen cfg` | controller 创建结果目录后复制的只读 cfg 快照 | `FROZEN_PLUS_FILE` | compile 和 seed 都只读取该副本 |
| `effective plusargs` | cfg 与 runtime plusarg 按 Makefile 覆盖规则合并后的唯一参数集 | `EFFECTIVE_PLUSARGS` | scalar 保留 `10010`，普通项可被 `TRANS_NUM` 覆盖 |
| `profile` | 矩阵对 cfg 目录和公共事务数策略的受控映射 | `resolve_case_profile()` | `default`、`csr_scalar_10k` |
| `fake runner` | 只记录矩阵参数并生成最小 PASS summary 的测试替身 | `fake_function_cfg_runner.sh` | 审计 30 次调用，不启动 EDA |
| `dry-run` | 完成本地验证、冻结和审计文件生成后退出 | `--dry-run` | 四个 scalar 精确计数审计 |

本 review 只覆盖 Bash runner、矩阵脚本、Bash 测试和同步 AI_DOC，不覆盖 RTL、Scala、SystemVerilog、cfg、
sequence、RM 或 DUT 行为。

## 2. 功能特性与修改前后行为

### 2.1 修改前逻辑

`run_eda_regression.sh` 把 cfg 源目录固定为 `seq/plus_cfg`。功能覆盖矩阵只有三字段 CASE，并对全部 30 项
无条件注入 `+MEMBLOCK_MAIN_TRANS_NUM=1000`。因此最后四项既无法选择专用目录，又会覆盖 cfg 内精确
`10010/10012/10016/10025`。

### 2.2 修改后逻辑

1. runner 新增公开 `--plus-file <dir>`，默认仍为 `seq/plus_cfg`。
2. 目录先拒绝控制字符，再按 caller cwd 解析相对路径，规范化为物理绝对路径。
3. cfg 名继续复用受限 `valid_name()`，固定追加 `.cfg`；真实文件目标必须仍直接位于允许目录。
4. 源 cfg 复制为只读 frozen cfg；effective plusargs 和远端 worker 协议保持只消费冻结副本。
5. `config.txt` 新增源目录和源 cfg，形成 source/frozen/hash 审计链。
6. 矩阵 CASE 扩展为四字段 profile；普通 26 项保留公共 `TRANS_NUM`，四个 scalar 不注入该 key。

### 2.3 正确性检查

| 场景 | 预期行为 | 验证 |
|---|---|---|
| 不传 `--plus-file` | 使用默认目录，旧调用兼容 | default dry-run PASS |
| 绝对/相对专用目录 | 规范化到相同物理目录 | plus-file test PASS |
| cfg symlink 指向目录外 | rc=2，冻结前拒绝 | symlink escape test PASS |
| 普通 26 项 | 默认目录且含公共 `TRANS_NUM` | fake matrix 26/26 PASS |
| 四个 scalar | 专用目录且无公共 override | fake matrix 4/4 PASS |
| scalar effective count | 各只有一个精确值 | 正式四项 dry-run PASS |

## 3. 调用关系

| 顺序 | 函数/流程 | 在当前功能中的职责 |
|---|---|---|
| 1 | controller 参数解析 | 保存 `--plus-file` 原始目录，不把它当命令或 cfg 文件名 |
| 2 | `normalize_plus_file_dir()` | 验证并规范化允许目录 |
| 3 | `resolve_cfg_file()` | 选择受限 cfg 并执行真实路径 containment 检查 |
| 4 | frozen cfg 主流程 | 复制只读快照、计算 hash、解析 effective plusargs |
| 5 | `write_config()` | 记录 source/frozen/hash 审计字段 |
| 6 | `resolve_case_profile()` | 为每条 CASE 决定目录和事务数 override 策略 |
| 7 | 矩阵主循环 | 组装 runner 数组参数并维持 compile-once 行为 |

## 4. Runner 源码 Review

### 4.1 `normalize_plus_file_dir()`

源码位置：`mem_ut/ver/ut/memblock/scripts/run_eda_regression.sh`，函数：`normalize_plus_file_dir()`。

抽象功能描述：该 helper 在 cfg 选择前把用户目录输入转换为可信物理绝对目录；它不选择 cfg、不读取
plusarg，也不创建结果目录。

- 输入：命令行原始目录，或默认目录。
- 输出：stdout 返回规范化目录；非法输入返回 2。
- caller：controller 初始化流程。
- callee：shell 目录检查、`cd`、`pwd -P`。

```bash
normalize_plus_file_dir() {
    local raw_dir="$1" candidate canonical_dir
    if [[ -z "$raw_dir" || "$raw_dir" =~ [[:cntrl:]] ]]; then
        log_error "--plus-file requires a non-empty directory without control characters"
        return 2
    fi
    if [[ "$raw_dir" == /* ]]; then candidate="$raw_dir"; else candidate="$PWD/$raw_dir"; fi
    [[ -d "$candidate" && -r "$candidate" && -x "$candidate" ]] || {
        log_error "plus-file directory is not readable and searchable: $candidate"
        return 2
    }
    canonical_dir="$(cd -- "$candidate" && pwd -P)" || return 2
    [[ -n "$canonical_dir" && ! "$canonical_dir" =~ [[:cntrl:]] ]] || return 2
    [[ -d "$canonical_dir" && -r "$canonical_dir" && -x "$canonical_dir" ]] || return 2
    printf '%s\n' "$canonical_dir"
}
```

中文伪代码：该逻辑承担公开目录入口的第一层安全检查。先拒绝空输入和任何控制字符，防止审计文件
换行或不可见字符污染；绝对路径直接作为候选，相对路径只按 controller 启动时的 `PWD` 拼接一次。
随后要求候选是可读、可遍历目录，再进入目录并用 `pwd -P` 消除相对段和目录 symlink。规范化结果再次
检查目录权限，成功时只输出物理绝对路径。`log_error()` 只输出错误，目录命令只负责验证和转换，不执行
用户字符串。

关键副作用：无文件写入；调用者只有在成功返回后才进入 cfg 选择。

### 4.2 `resolve_cfg_file()`

源码位置：同脚本，函数：`resolve_cfg_file()`。

抽象功能描述：该 helper 使用已验证目录和受限 cfg 名选择唯一 cfg，并证明真实文件目标未逃出目录；它
不接受用户提供的文件名或后缀。

- 输入：规范化目录、已通过 `valid_name()` 的 cfg 名。
- 输出：真实 cfg 绝对路径；缺失、不可读或逃逸返回 2。
- caller：controller 初始化流程。
- callee：`realpath -e`、`dirname`。

```bash
resolve_cfg_file() {
    local canonical_dir="$1" cfg_name="$2" candidate resolved_file resolved_parent
    candidate="$canonical_dir/$cfg_name.cfg"
    [[ -f "$candidate" && -r "$candidate" ]] || return 2
    resolved_file="$(realpath -e -- "$candidate")" || return 2
    [[ -n "$resolved_file" && ! "$resolved_file" =~ [[:cntrl:]] ]] || return 2
    [[ -f "$resolved_file" && -r "$resolved_file" ]] || return 2
    resolved_parent="$(dirname -- "$resolved_file")"
    [[ "$resolved_parent" == "$canonical_dir" ]] || {
        log_error "cfg file escapes plus-file directory: $candidate -> $resolved_file"
        return 2
    }
    printf '%s\n' "$resolved_file"
}
```

中文伪代码：该逻辑承担 cfg 文件级 containment。先用规范化目录、受限 cfg 名和固定 `.cfg` 后缀构造
唯一候选，不解析用户给出的任意文件名；要求候选是可读普通文件。调用 `realpath -e` 解析文件 symlink，
再确认真实目标仍是可读普通文件。最后取真实目标父目录并与允许目录做完整字符串相等比较；不同则打印
逃逸错误并返回 2，相同才输出真实路径。`realpath` 只做路径解析，`dirname` 只提取父目录。

关键副作用：目录内普通文件和指向目录内目标的 symlink 可用，指向目录外目标的 symlink 被拒绝。

### 4.3 冻结和 `write_config()`

源码位置：同脚本，controller frozen cfg 主流程和 `write_config()`。

抽象功能描述：controller 在结果目录建立后复制所选源 cfg，后续只从冻结目录计算 effective plusargs并
传给 worker；`write_config()` 记录本次运行的源和冻结身份，不修改参数策略。

```bash
FROZEN_PLUS_FILE="$OUTPUT_DIR/frozen_plus_cfg"
mkdir -p "$FROZEN_PLUS_FILE" || exit 2
cp -- "$CFG_FILE" "$FROZEN_PLUS_FILE/$CFG.cfg" || exit 2
chmod 0444 "$FROZEN_PLUS_FILE/$CFG.cfg" || exit 2
validate_cfg_plusargs "$FROZEN_PLUS_FILE/$CFG.cfg" || exit 2
CFG_HASH="$(sha256sum "$FROZEN_PLUS_FILE/$CFG.cfg" | awk '{print $1}')"
EFFECTIVE_PLUSARGS="$(make -s -C "$SIM_DIR" eda_print_effective_plusargs \
    cfg="$CFG" plus_file="$FROZEN_PLUS_FILE" plus_arg="$USER_PLUSARGS_STRING" | paste -sd' ' -)"
```

中文伪代码：该逻辑先创建固定冻结目录，把已验证源 cfg 复制为由受限 cfg 名生成的文件，再改为只读。
随后对冻结副本计算 SHA-256，并调用 Makefile 的 `eda_print_effective_plusargs`，明确把 `plus_file` 指向
冻结目录，使源 cfg 后续变化不会影响本批次。`cp/chmod/sha256sum` 只操作结果目录副本，Makefile 只读取
冻结 cfg 并合并 runtime plusargs。

```bash
printf 'source_plus_file=%s\n' "$PLUS_FILE"
printf 'source_cfg=%s\n' "$CFG_FILE"
printf 'frozen_cfg=%s\n' "$FROZEN_PLUS_FILE/$CFG.cfg"
printf 'cfg_sha256=%s\n' "$CFG_HASH"
```

中文伪代码：`write_config()` 按固定 key-value 行记录规范化源目录、规范化源 cfg、冻结副本路径和冻结
内容 hash。写入通过现有 `atomic_write()` 原子发布；这些字段只用于审计，不改变 compile 或 seed 行为。

关键副作用：远端 worker 仍只收到 `--plus-file "$FROZEN_PLUS_FILE"`，已有 cfg hash 校验保持不变。

## 5. 功能覆盖矩阵源码 Review

### 5.1 `resolve_case_profile()`

源码位置：`mem_ut/ver/ut/memblock/scripts/run_function_cfg_cov_20261022.sh`，函数：`resolve_case_profile()`。

抽象功能描述：该 helper 把受控 profile 名转换为 cfg 目录和公共事务数开关；它不读取 cfg 内容，也不
直接执行 runner。

```bash
resolve_case_profile() {
    local profile="$1"
    case "$profile" in
        default)
            CASE_PLUS_FILE="$SIM_DIR/../seq/plus_cfg"
            CASE_OVERRIDE_TRANS_NUM=1
            ;;
        csr_scalar_10k)
            CASE_PLUS_FILE="$SIM_DIR/../seq/csr_scalar_stress/cfg"
            CASE_OVERRIDE_TRANS_NUM=0
            ;;
        *)
            echo "unknown functional cfg profile: $profile" >&2
            return 2
            ;;
    esac
}
```

中文伪代码：该逻辑是 CASE profile 的唯一权威映射。收到 `default` 时选择原 `seq/plus_cfg` 并允许
注入公共事务数；收到 `csr_scalar_10k` 时选择专用目录并关闭公共事务数 override。任何其他字符串先
打印错误再返回 2，因此主循环不会调用 runner。该函数只设置当前 CASE 的两个 shell 变量。

关键副作用：矩阵行不能携带自由路径，避免 CASE 文本成为任意目录入口。

### 5.2 CASE 主循环

源码位置：同脚本，30 项 CASE 定义和主循环。

抽象功能描述：主循环校验四字段 CASE，解析 profile，构造一次 runner 数组调用，并保持第一项 compile、
后续 no-compile 的原有共享 mode 行为。

```bash
IFS='|' read -r cfg tc ts profile extra <<<"$item"
[[ -n "$cfg" && -n "$tc" && -n "$ts" && -n "$profile" && -z "${extra:-}" ]] || exit 2
for value in "$cfg" "$tc" "$ts"; do
    [[ "$value" =~ ^[A-Za-z0-9_.-]+$ && "$value" != . && "$value" != .. ]] || exit 2
done
resolve_case_profile "$profile" || exit 2
args=(
    --tc "$tc" --cfg "$cfg" --plus-file "$CASE_PLUS_FILE" --ts "$ts"
    --mode "$MODE" --count "$COUNT" --jobs "$JOBS" --coverage --no-wave
    --verbosity UVM_LOW --timeout-ns "$TIMEOUT_NS"
    --plusarg '+MEMBLOCK_CHECK_TRIGGER_EN=0' --output-dir "$out"
)
if (( CASE_OVERRIDE_TRANS_NUM )); then
    args+=(--plusarg "+MEMBLOCK_MAIN_TRANS_NUM=$TRANS_NUM")
fi
```

中文伪代码：每轮把 CASE 拆为 cfg、testcase、virtual sequence、profile 和多余字段；任一必填字段为空或
存在第五字段就退出 2。cfg/tc/ts 必须匹配 runner name grammar，并显式拒绝 `.` 与 `..` 特殊路径组件，
该校验发生在结果目录构造和 `mkdir -p` 前。调用 `resolve_case_profile()` 取得可信目录和事务数策略，然后
使用 shell 数组逐项保存 runner 参数，所有 profile 都保留 `CHECK_TRIGGER_EN=0`。只有 profile 开启公共 override 时才
追加 `MEMBLOCK_MAIN_TRANS_NUM`；四个 scalar 因开关为 0 不追加同名 key，Makefile 因而保留 cfg 精确值。
后续原有 compile 标志只让第一项编译，runner 返回码和 summary 仍按原流程汇总。

关键副作用：普通 26 项行为不变；四项只改变 cfg 目录和事务数 override 策略。

## 6. 测试资产 Review

- `test_run_eda_regression_plus_file.sh`：验证默认兼容、绝对/相对目录、source/frozen/hash、四个精确计数、
  源 cfg 不变、缺失目录、普通文件、控制字符、非法 cfg 名和 cfg symlink 逃逸。
- `fake_function_cfg_runner.sh`：只解析已知参数、记录 cfg/目录/compile/事务数/check trigger，并生成最小
  `result=PASS`；不会调用 EDA。
- `test_run_function_cfg_cov_matrix.sh`：断言 30 次调用、compile-once、前 26 项公共 `TRANS_NUM`、后四项
  无 override、目录路由和未知 profile 在 runner 前失败。
- 既有 `test_run_eda_regression_identity_race.sh` 全量通过，证明 controller 远端生命周期逻辑未回归。

## 7. 验证结果

| 门禁 | 结果 |
|---|---|
| 全部 `memblock/scripts/**/*.sh` 执行 `bash -n` | PASS |
| `test_run_eda_regression_plus_file.sh` | PASS |
| `test_run_function_cfg_cov_matrix.sh` | PASS |
| `test_run_eda_regression_identity_race.sh` | PASS |
| 四个 scalar 正式 `--dry-run` | PASS，`10010/10012/10016/10025` 各唯一出现 |
| `git diff --check` | PASS |
| 可复用 `simv` 检查 | 当前 worktree 无候选，因此未强制真实 10k smoke |
| 禁止文件扩展名检查 | 无 `.sv/.svh/.scala/.cfg` 修改 |
| `shellcheck` | 当前环境未安装，已由 `bash -n`、fake 测试和人工 shell 审查覆盖 |

正式 dry-run 结果目录：
`mem_ut/ver/ut/memblock/sim/csr_scalar_matrix_fix_20261025_dryrun_703788_25991/`。

## 8. Plan 对齐检查

已找到并逐项检查：
`AI_DOC/plan/test_framework/plan/do/memblock_csr_scalar_functional_matrix_integration_fix_plan_20261025.md`。

用户十项要求均有实现或验证证据。真实 smoke 按 plan 定义为“仅在已有匹配可执行文件时可选”，当前没有
可复用 `simv`，因此不属于未完成项。

## 9. 实现与 Plan 不一致项

首版 plan 要求更新 simulation buglist，但独立 review 对照
`AI_DOC/project_management/buglist_bug_record_rule.md` 后确认该要求不合法：问题在矩阵启动前即可静态
确定，没有 testcase 仿真日志或波形，不满足 buglist 触发条件。plan 第 6 节已修正为禁止登记该条目，
原 buglist 章节已删除；问题证据继续由 plan/review 和单 issue commit 承载。

## 10. Plan 未说明但 Coding 落实的细节

矩阵脚本增加 `FUNCTION_CFG_CASES_FILE` 测试注入点，并增加空 fixture 拒绝；它只服务未知 profile 的
确定性 fake 测试，生产默认值为空且不会改变正式 30 项。plan 的 fake 测试章节要求未知 profile fixture，
但未指定具体注入变量名，因此该细节属于测试 hook 的实现落点。该入口仍按生产 CASE name grammar 校验
cfg/tc/ts，禁止在 runner 校验前通过 cfg 构造结果目录穿越。

矩阵脚本从 `set -u` 调整为 `set -uo pipefail`，用于让测试 fixture 读取 pipeline 失败可见；不改变正式
成功路径，也未开启会改变现有 runner 错误收集行为的 `set -e`。

## 11. 非本次修改的逻辑分析

`git status --short` 中全部修改均属于本次 issue：buglist、runner 使用规则、plan/plan review/
implementation review、两个生产 Bash 脚本和三个 Bash 测试资产。没有主 worktree 修改、其他功能源码、
RTL/Scala/SystemVerilog/cfg 修改或需要另行 review 的生成文件。

## 12. 独立 Review 结论与修正

独立只读 reviewer `Zeno` 对 commit `85ff565558` 及其工作树差异给出 `REVIEW_FAIL`。该 verdict 取代本文
此前错误记录的机械 `REVIEW_PASS`。首轮 findings 与修正如下：

1. **High：cfg 内容可进入 Makefile recipe，存在命令注入面。**
   修正：冻结 cfg 后、解析 effective plusargs 前新增 `validate_cfg_plusargs()`。只允许空行、注释、兼容的
   单独 `+` 行和严格 `+KEY=VALUE`；key/value grammar 与 CLI plusarg 收敛一致；拒绝 shell 元字符和重复
   key。cfg 对 `VSEQ_MAIN` 等 key 的既有 override 语义保持不变。
2. **Medium：只检查原始目录控制字符，未检查 `pwd -P` 后的 canonical path。**
   修正：`normalize_plus_file_dir()` 在物理路径解析后再次拒绝空值和控制字符，覆盖指向含换行目录的
   symlink。
3. **Medium：文档使用不存在的机械 `REVIEW_PASS`。**
   修正：plan review 改为 `NO_VERDICT`；本文记录真实的 `Zeno REVIEW_FAIL`，最终状态等待第二轮独立复审。
4. **Medium：预仿真矩阵集成问题不满足正式 simulation buglist 触发条件。**
   修正：从 `AI_DOC/buglist/rm/v2/rm_buglist_2026-W43_20261019.md` 删除该章节；问题、方案、代码和测试证据
   继续由本 plan/review 文档及单 issue commit 承载，不改写其他 bug 历史。

第二轮独立 reviewer `Mendel` 给出 `REVIEW_FAIL`，新增 findings 与修正如下：

1. **Medium：in-directory cfg symlink 可解析到含控制字符的文件名并污染 `config.txt`。**
   修正：`resolve_cfg_file()` 对 `realpath -e` 结果再次拒绝空值和控制字符；测试使用目录内 symlink 指向
   含换行的目标文件，确认 dry-run 在写配置前 rc=2。
2. **Medium：implementation review 摘录与 plan delta 过期。**
   修正：源码摘录补齐 canonical directory/file 控制字符检查和 `validate_cfg_plusargs()`；plan 第 6 节与
   本文第 9 节明确记录 buglist 要求的修订。
3. **Low：测试 CASE hook 可在 runner name 校验前用 cfg 穿越结果目录。**
   修正：矩阵在 `mkdir -p` 前按 runner `valid_name` 等价 grammar 校验 cfg/tc/ts；新增 `../escape` fixture，
   断言 rc=2 且结果根目录外没有创建目录。focused re-review 进一步指出 grammar 仍接受特殊组件 `..`；
   最终实现显式拒绝 `.`/`..`，并新增 `cfg=..` fixture，确认不会在结果根目录父目录写 `summary.log`。

最终独立复审：reviewer `Mendel` 在全部修正和最终门禁完成后给出 `REVIEW_PASS`，确认无剩余 correctness、
security、regression、test 或 documentation findings；Zeno 与 Mendel 的全部 findings 均已关闭。最终门禁
包括两个生产脚本 `bash -n`、30 个现有 cfg 全量 dry-run、plus-file 安全负向矩阵、功能矩阵 profile 与
路径穿越测试、identity-race controller fake matrix，以及相对基线 `045011bf6b` 的 `git diff --check`。

## 13. 2026-10-28 真实 EDA 门禁补充

后续为四个 CSR scalar profile 补做真实 EDA smoke。编译成功，但四项均在 `0.000ns` 报
`SEQ_CSR_CFG: LEVEL_WEIGHT_EN requires MAIN_MEM_RANGES_EN=0`。原 dry-run 只证明 cfg 选择、冻结和
参数路由正确，未执行 sequence 配置合法性检查，因此不能替代真实运行门禁。

根因是四个 profile 使用独立 `--plus-file` 目录，不会与 `seq/plus_cfg/default.cfg` 合并；profile 已显式
设置 `MEMBLOCK_MAIN_MEM_RANGES_EN=1` 和 `MEMBLOCK_PMA_PMP_MODEL_EN=1`，却遗漏显式设置
`MEMBLOCK_L2TLB_LEVEL_WEIGHT_EN=0`。该问题按 W44 测试框架 Bug 单独记录并修复，原矩阵功能提交
`e888e5c43` 不改写。配置 Bug 的独立 gate 是四个 cfg 的 dry-run 三项组合断言全部通过，并由一个代表性
原始 seed 真实运行越过 `0.000ns` 配置检查。四个完整 10k 原始 seed 改为配置 commit 与独立 prefix
Bug 均合入母任务分支后的联合回归门禁。

首个原始 seed `1684208666` 的修复后真实复测已证明配置 gate 修复生效：仿真越过 `0.000ns`，完成
`6930` 次 RM compare PASS。随后在 `553888.000ns` 命中独立 `ROB_ADMISSION_PREFIX` fatal，属于
redirect/admission prefix 生命周期问题，不是 cfg 参数组合回归。该独立 Bug 将按用户要求在母任务
worktree 中顺序修复；其余三个 scalar 原 seed 和 load seed 的完整 10k PASS 在该 Bug 合并后继续执行。

新增负向测试覆盖 canonical path 换行、cfg 命令注入、重复 key 和 malformed 行，并断言注入 marker 未被
创建；另增加正式 key override 兼容测试。全目录兼容扫描发现既有 cfg 合法设置 `VSEQ_MAIN`，因此取消
首版 validator 对正式 key 的禁止，只保留语法安全与重复 key 检查。修正后 `bash -n`、plus-file dry-run
matrix、30 项 profile routing、identity-race fake matrix、全部现有 cfg dry-run 和 `git diff --check` 均重新
执行。最终结论必须由第二轮独立 reviewer 给出；主 agent 不再使用机械 closure 替代 reviewer verdict。
