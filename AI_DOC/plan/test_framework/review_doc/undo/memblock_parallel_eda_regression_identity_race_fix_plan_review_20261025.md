# memblock 并发回归退出身份竞态修复 Plan Review

关联 plan：`AI_DOC/plan/test_framework/plan/do/memblock_parallel_eda_regression_identity_race_fix_plan_20261025.md`
Review 日期：2026-10-25
状态：首轮 `REVIEW_FAIL`，修订后独立复审 `REVIEW_PASS`

## 1. 术语与抽象功能说明

| 术语 | 中文含义 | Review 关注点 |
|---|---|---|
| `matching RC` | 完整匹配当前 job/token/PGID 的 RC 文件 | 是否足以证明原 worker 实例结束 |
| `stability window` | 终态 RPC 后有限等待 RC 可见的窗口 | 是否有限、可测试且不掩盖失联 |
| `IDENTITY_MISMATCH` | PGID live member 中没有当前 token | 真 mismatch 是否仍 quarantine |
| `EXITED` | framed RPC 已证明 PGID 无 live member | RC 缺失是否应归 INFRA 而非 unknown |
| `REMOTE_UNKNOWN` | 当前 token 对应远端生命周期无法证明 | mode 安全约束是否保持 |

抽象 review 目标：独立检查 plan 是否以最小修改吸收 RC 可见性竞态，同时保持 token mismatch 的
quarantine 安全边界，并确认 compile、seed、probe、signal 和 fake 测试闭环。

## 2. 独立 Review 结论

独立只读 reviewer `01a172af-04c4-7510-81c1-9ea6dd35b1d6` 于 2026-10-25 给出
`REVIEW_FAIL`。修复方向正确，但正式 plan 尚未关闭 finalization 未校验 RC 恢复旁路，也没有处理
signal disposition 在 RC 缺失时把 seed 判为 PASS/ABORTED 的优先级问题，因此首轮不可 coding。

## 3. 必须修改项

1. `classify_seed_job()` 必须在 monitor 根因、日志基础设施错误和 UVM 错误之后，先把缺失 RC 固定分类为
   INFRA，再进入 signal disposition 分支，禁止完整 PASS 日志掩盖 RC 缺失。
2. `finalize_exited_job()` 不得继续使用通用 `recover_file_over_ssh()` 直接发布 RC；所有 RC 恢复入口必须
   先校验 job/token/PGID/schema，再原子发布和消费。
3. fake 测试补充 compile 缺 RC、probe/TERM/KILL 一致性、非法/截断 RC 不发布、明确命中竞态和
   jobs=4 同步屏障断言。

## 4. 风险与验证检查

- 远端 `cat` 需要命令级 timeout，不能只依赖 SSH ConnectTimeout。
- 多 job 串行恢复必须给出最坏延迟，并在等待期间继续处理 monitor trigger。
- 首轮只完成静态 review，未运行测试或仿真。

## 5. 修订结果

正式 plan 已补充上述三项 blocker 和两项风险边界。独立 reviewer
`01a172b1-f082-7263-9199-28364fd87eb1` 于 2026-10-25 复审结论为 `REVIEW_PASS`：确认缺失 RC
分类优先级、严格校验后原子发布、compile/probe/TERM/KILL/jobs=4/非法 RC 测试、命令级超时及等待期间
monitor trigger 处理均已进入正式方案，方案允许进入 coding。
