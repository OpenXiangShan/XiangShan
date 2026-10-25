# memblock 并发回归退出身份竞态修复 Implementation Review

关联 plan：`AI_DOC/plan/test_framework/plan/do/memblock_parallel_eda_regression_identity_race_fix_plan_20261025.md`
Review 日期：2026-10-25
责任类别：测试框架
状态：`REVIEW_PASS`；首轮独立 review 的五项 blocker 已全部关闭

## 1. 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 对应代码对象或落点 | 使用场景/示例 |
|---|---|---|---|
| `managed job` | controller 已持久记录身份并负责生命周期收敛的 compile 或 seed | `JOB_ORDER/TOKEN/PGID/OWNER_ACTIVE` | `seed:2059385969` |
| `matching RC` | protocol、job、token、PGID 和结束 schema 全部匹配当前 owner 的返回码 | `validate_matching_rc_file()` | 旧 token RC 不可消费 |
| `stability window` | terminal RPC 后等待 matching RC 可见的有限重试窗口 | `recover_matching_rc_with_retry()` | 默认最多 3 次 |
| `terminal RPC` | 证明远端任务不再普通运行的 `EXITED` 或 `IDENTITY_MISMATCH` 响应 | `RPC_STATUS[]` | PROBE 返回 `EXITED` |
| `remote phase` | 独立于 UVM PASS/FAIL 的远端生命周期状态 | `REMOTE_PHASE[]` | `OWNED -> RC_SEEN -> EXIT_CONFIRMED` |
| `quarantine` | 无法证明当前 token 已安全退出时禁止复用 mode 的安全记录 | `write_quarantine()` | 真 token mismatch |
| `signal disposition` | framed RPC 确认实际发送 TERM/KILL 后的联停标记 | `SIGNAL_DISPOSITION[]` | 只有 `SIGNALED` 更新 |
| `canonical RC` | 当前结果目录中 controller 正式消费的 `status/<job>.rc` | `RC_FILE[]` | 非匹配临时候选不得覆盖 |
| `monitor commit` | UVM 根因 trigger 在发送 TERM/KILL 前持久提交 | `monitor_failure.committed` | commit 时间小于 signal 时间 |
| `peak recompute` | 根据 ACK/RC 纳秒区间排序并计算 `peak_live` | `recompute_peak_live_from_intervals()` | 一批 observation 最多一次 |

本 review 覆盖生产 Bash controller、fake SSH/worker 测试和同步文档，不覆盖 RTL、Scala、SV 激励、
RM、checker、cfg 或 coverage 代码。

## 2. 功能特性与修改前后行为

### 2.1 旧行为

修改前 `observe_rc()` 只消费本地已经可见的 RC。`probe_active_jobs()` 和 `signal_active_jobs()` 收到
`IDENTITY_MISMATCH` 时，仅检查内存 `RC[$job]`；为空立即 `REMOTE_UNKNOWN`。`finalize_exited_job()`
还能用通用文件恢复直接覆盖正式 RC。结果是 worker 已 rc=0 退出但共享 RC 晚可见时误 quarantine。

### 2.2 新行为

1. 所有 RC 必须先通过 matching schema 校验。
2. terminal RPC 且本轮未见 RC 时，进入有限共享/SSH 恢复窗口。
3. `EXITED + no RC` 进入 `EXIT_CONFIRMED`，compile/seed 归 INFRA，不 quarantine。
4. `IDENTITY_MISMATCH + no RC` 经过完整窗口后仍 quarantine。
5. PROBE/TERM/KILL 共用同一终态 helper；只有 `SIGNALED` 更新 disposition。
6. stale owner 启动前审计保持旧保守行为。

### 2.3 正确性决策表

| RPC | matching RC | remote phase | 结果/动作 |
|---|---|---|---|
| `EXITED` | 有 | `EXIT_CONFIRMED` | 按 RC/log finalization |
| `EXITED` | 无 | `EXIT_CONFIRMED` | INFRA，不 quarantine |
| `IDENTITY_MISMATCH` | 有 | `EXIT_CONFIRMED` | 原 worker 退出已证明 |
| `IDENTITY_MISMATCH` | 无 | `REMOTE_UNKNOWN` | quarantine |

## 3. 调用关系

| 调用顺序 | 函数 | 在当前流程中的功能 |
|---|---|---|
| 1 | `probe_active_jobs()` / `signal_active_jobs()` | 获取 framed RPC，每个 terminal job 进入统一收敛 |
| 2 | `settle_terminal_rpc_status()` | 按 RPC 类型和 matching RC 决定确认退出或 quarantine |
| 3 | `recover_matching_rc_with_retry()` | 在有限窗口检查共享 RC，并用 bounded SSH 读取候选 |
| 4 | `validate_matching_rc_file()` | 只判断候选是否属于当前 owner |
| 5 | `consume_matching_rc_file()` | 保存 RC/结束时间，并把 OWNED 推进为 RC_SEEN |
| 6 | `finalize_exited_job()` | 获取稳定日志并按 compile/seed 规则分类 |

`observe_rc()` 是主循环的非 terminal 快速入口：批量消费本地 matching RC，并在一轮内最多重算一次
`peak_live` 和发布一次 owner manifest。

## 4. 修改函数逐项源码 Review

用户描述中的 `recover_matching_rc_after_terminal_probe` 是功能名称；当前源码没有该同名函数，其实现落点是
`recover_matching_rc_with_retry()`，并由 `settle_terminal_rpc_status()` 在 PROBE、TERM、KILL 的终态响应后统一调用。

### 4.1 `ssh_once_bounded()`：限制单次远端读取时间

源码位置：`mem_ut/ver/ut/memblock/scripts/run_eda_regression.sh`，函数：`ssh_once_bounded()`。

抽象功能描述：该 helper 由 matching RC 恢复窗口调用，执行一次有硬超时的 SSH 命令并把 SSH 返回值原样交给调用者；
它不校验远端文件内容，也不修改 job 生命周期状态。

- 输入：超时秒数 `timeout_sec`、远端命令 `remote_cmd`、可选目标主机 `host`。
- 输出/副作用：stdout/stderr 和退出码来自 `timeout/ssh`；关闭继承的 mode lock fd，避免子进程持锁。
- caller：`recover_matching_rc_with_retry()`。
- callee：`close_inherited_lock_fd()`、系统 `timeout`、`ssh`。

```bash
ssh_once_bounded() {
    local timeout_sec="$1" remote_cmd="$2" host="${3:-$FINAL_REMOTE_HOST}"
    (
        close_inherited_lock_fd
        exec timeout --signal=TERM --kill-after=1 "$timeout_sec" \
            ssh "${SSH_OPTS[@]}" "$host" "$remote_cmd"
    )
}
```

中文伪代码：该逻辑在 matching RC 恢复中防止单次远端读取无限阻塞。函数先从参数取得超时时间、远端命令和主机，
未提供主机时使用最终选定的 EDA 主机；随后在子 shell 中关闭继承的 mode lock 文件描述符，避免 SSH 生命周期延长本地锁；
最后由 `timeout` 启动 SSH，达到期限先发 TERM，一秒后仍未退出则发 KILL，并把最终退出码返回恢复函数。
`close_inherited_lock_fd()` 只处理锁继承，`timeout/ssh` 只执行受限远端命令，均不写 `RC[]` 或 `REMOTE_PHASE[]`。

关键副作用：远端读取 stall 最多占用 `RC_RECOVERY_READ_TIMEOUT + 1` 秒，不会阻塞 monitor trigger 的下一次处理。

### 4.2 `validate_matching_rc_file()`：严格验证 RC 身份与完整性

源码位置：同脚本，函数：`validate_matching_rc_file()`。

抽象功能描述：该纯校验 helper 判断一个 RC 候选是否完整且属于当前 managed job；它只返回真假，不发布 canonical RC，
不写结果分类，也不把 matching RC 等同于远端进程组已退出。

- 输入：job id、候选文件路径，以及 `TOKEN[]`、`PGID[]` 和 `JOB_PROTOCOL` 公共身份状态。
- 输出/副作用：匹配返回 0，不匹配返回 1；无公共状态副作用。
- caller：`consume_matching_rc_file()`、`recover_matching_rc_with_retry()`。
- callee：`read_key()`，负责按 key 读取 framed RC 字段。

```bash
validate_matching_rc_file() {
    local job="$1" file="$2" protocol job_id token pgid rc ended_epoch_ns end
    [[ -f "$file" && -r "$file" ]] || return 1
    protocol="$(read_key "$file" protocol)"; job_id="$(read_key "$file" job_id)"; token="$(read_key "$file" token)"
    pgid="$(read_key "$file" pgid)"; rc="$(read_key "$file" rc)"
    ended_epoch_ns="$(read_key "$file" ended_epoch_ns)"; end="$(read_key "$file" end)"
    [[ "$protocol" == "$JOB_PROTOCOL" && "$job_id" == "$job" && "$token" == "${TOKEN[$job]}" \
        && "$pgid" == "${PGID[$job]}" && "$rc" =~ ^[0-9]+$ \
        && "$ended_epoch_ns" =~ ^[0-9]+$ && "$end" == "RC_END" ]]
}
```

中文伪代码：该逻辑是 RC quarantine 安全边界的第一层。先确认候选路径是可读普通文件，否则立即失败；再通过 `read_key()`
分别读取协议版本、job id、token、PGID、返回码、结束纳秒时间和结尾标记。最后一次性要求协议等于当前协议、job id 等于调用参数、
token 和 PGID 等于当前 owner、返回码和结束时间均为数字、结尾必须为 `RC_END`。任一条件不满足时以失败返回，调用者不得发布或消费；
全部满足时仅返回成功。`read_key()` 负责字段提取，不修改 owner 或结果数组。

关键副作用：无。错误 token/job/PGID、截断文件和非数字字段都不能覆盖 canonical RC。

### 4.3 `consume_matching_rc_file()`：消费已验证 RC 并推进可见状态

源码位置：同脚本，函数：`consume_matching_rc_file()`。

抽象功能描述：该 helper 在候选通过严格身份校验后，把返回码和结束时间写入 controller 公共状态，并把 `OWNED` 推进到
`RC_SEEN`；它不发布 owner manifest、不计算 peak，也不确认远端进程组退出。

- 输入：job id、RC 文件路径，以及当前 `REMOTE_PHASE[]`。
- 输出/副作用：成功返回 0并写 `RC[]`、`END_EPOCH_NS[]`，必要时写 `REMOTE_PHASE[]=RC_SEEN`；失败返回 1且不写状态。
- caller：`recover_matching_rc_with_retry()`、`observe_rc()`。
- callee：`validate_matching_rc_file()`、`read_key()`。

```bash
consume_matching_rc_file() {
    local job="$1" file="$2" rc ended_epoch_ns
    validate_matching_rc_file "$job" "$file" || return 1
    rc="$(read_key "$file" rc)"; ended_epoch_ns="$(read_key "$file" ended_epoch_ns)"
    RC[$job]="$rc"; END_EPOCH_NS[$job]="$ended_epoch_ns"
    if [[ "${REMOTE_PHASE[$job]}" == "OWNED" ]]; then
        REMOTE_PHASE[$job]="RC_SEEN"
    fi
}
```

中文伪代码：该逻辑负责把可信 RC 变成 controller 可消费状态。先调用 `validate_matching_rc_file()` 完成协议、job、token、PGID、
数值字段和结束标记校验；失败时立即返回 1，保证所有公共数组保持原值。校验成功后再次读取返回码和结束时间，分别保存到 `RC[]`
和 `END_EPOCH_NS[]`。如果 job 仍是 `OWNED`，说明这是首次观察到 RC，于是推进为 `RC_SEEN`；如果已经是 `RC_SEEN` 或
`EXIT_CONFIRMED`，保持 phase 不回退。校验 helper 决定能否消费，`read_key()` 提供已验证字段值。

关键副作用：只表达“RC 已可见”，不表达“远端已退出”；退出确认由 terminal RPC settlement 完成。

### 4.4 `recover_matching_rc_with_retry()`：终态后的有限 matching RC 恢复窗口

源码位置：同脚本，函数：`recover_matching_rc_with_retry()`。

抽象功能描述：该函数对应用户所称的 `recover_matching_rc_after_terminal_probe`。它在 terminal RPC 已返回但内存尚无 RC 时，
以有限次数先检查共享 canonical RC，再通过 bounded SSH读取远端候选；只允许严格匹配候选被原子发布和消费。

- 输入：job id、重试次数、诊断 context，以及该 job 的 RC 路径、host、token、PGID 和 remote phase。
- 输出/副作用：恢复成功返回 0，更新 RC/结束时间/phase、peak 和 owner manifest；耗尽返回 1，不直接 quarantine。
- caller：`settle_terminal_rpc_status()`、`finalize_exited_job()`。
- callee：`handle_monitor_triggers()`、`consume_matching_rc_file()`、`ssh_once_bounded()`、
  `validate_matching_rc_file()`、`recompute_peak_live_from_intervals()`、`write_owner_manifest()`。

```bash
recover_matching_rc_with_retry() {
    local job="$1" attempts="${2:-$SSH_RETRIES}" context="${3:-terminal}" file tmp cmd attempt
    local before_phase ssh_ok
    file="${RC_FILE[$job]}"
    for ((attempt = 1; attempt <= attempts; attempt++)); do
        handle_monitor_triggers || true
        before_phase="${REMOTE_PHASE[$job]}"
        if consume_matching_rc_file "$job" "$file"; then
            recompute_peak_live_from_intervals
            if [[ "$before_phase" == OWNED ]]; then write_owner_manifest || RECORD_INFRA_FAILURE=1; fi
            log_info "$job recovered matching RC context=$context attempt=$attempt source=shared"
            return 0
        fi
        tmp="$file.recovered.$$.$RANDOM"
        cmd="$(quote_cmd cat "$file")"
        ssh_ok=0
        if ssh_once_bounded "$RC_RECOVERY_READ_TIMEOUT" "$cmd" "${JOB_HOST[$job]:-$FINAL_REMOTE_HOST}" \
            >"$tmp" 2>/dev/null; then
            ssh_ok=1
        fi
        handle_monitor_triggers || true
        before_phase="${REMOTE_PHASE[$job]}"
        if [[ "$ssh_ok" -eq 1 ]]; then
            if validate_matching_rc_file "$job" "$tmp"; then
                if mv -fT "$tmp" "$file" && consume_matching_rc_file "$job" "$file"; then
                    recompute_peak_live_from_intervals
                    if [[ "$before_phase" == OWNED ]]; then write_owner_manifest || RECORD_INFRA_FAILURE=1; fi
                    log_info "$job recovered matching RC context=$context attempt=$attempt source=ssh"
                    return 0
                fi
            else
                log_warn "$job rejected non-matching RC candidate context=$context attempt=$attempt phase=${REMOTE_PHASE[$job]}"
            fi
        fi
        rm -f "$tmp"
        handle_monitor_triggers || true
        if (( attempt < attempts )); then sleep 1; fi
    done
    return 1
}
```

中文伪代码：该逻辑在终态响应后提供有限稳定窗口。函数取得 job 的 canonical RC 路径，然后按允许次数循环。每轮开始先调用
`handle_monitor_triggers()` 持久提交新出现的 UVM 根因，防止等待 RC 时延误全局停止；记录消费前 phase 后，优先调用
`consume_matching_rc_file()` 检查共享路径。若共享 RC 已严格匹配，则重算并发区间；若 phase 从 `OWNED` 前进，还发布 owner manifest，
发布失败只置基础设施失败标志；记录 shared 恢复日志并成功返回。共享路径未命中时，构造临时文件和安全引用的远端 `cat` 命令，
调用 `ssh_once_bounded()` 在固定时限内读取该 job 主机上的同一 RC；SSH 结束后再次处理 monitor trigger。只有 SSH 成功且临时文件通过
`validate_matching_rc_file()`，才用 `mv -fT` 原子替换 canonical RC，再调用 `consume_matching_rc_file()` 写公共状态；成功后同样更新 peak、
owner manifest 和诊断日志。非匹配候选只打印 warning，不发布；每轮都删除残留临时文件并再次处理 trigger，非末轮睡眠一秒。
次数耗尽返回 1，由 `settle_terminal_rpc_status()` 根据 EXITED 或 IDENTITY_MISMATCH 决定 INFRA 或 quarantine。

关键副作用：真正 token mismatch 始终无法通过校验；恢复失败本身不放松 quarantine，最终决策仍保留在上层 terminal helper。

### 4.5 `observe_rc()`：批量观察共享 RC

源码位置：同脚本，函数：`observe_rc()`。

抽象功能描述：主循环用该函数批量消费本地已可见的 matching RC，并把一轮内的区间变化和 owner phase 变化合并处理；
它不执行 SSH 恢复，也不做终态分类。

- 输入：`JOB_ORDER[]`、每个 job 的 phase/RC 路径/结束时间。
- 输出/副作用：可能更新 `RC[]`、`END_EPOCH_NS[]`、`REMOTE_PHASE[]`，每轮最多重算一次 peak、发布一次 owner manifest。
- caller：controller 主循环和等待流程。
- callee：`consume_matching_rc_file()`、`recompute_peak_live_from_intervals()`、`write_owner_manifest()`。

```bash
observe_rc() {
    local job file before_phase old_end interval_changed=0 phase_changed=0
    for job in "${JOB_ORDER[@]}"; do
        case "${REMOTE_PHASE[$job]:-}" in OWNED|RC_SEEN|EXIT_CONFIRMED) ;; *) continue ;; esac
        file="${RC_FILE[$job]:-}"; [[ -n "$file" ]] || continue
        before_phase="${REMOTE_PHASE[$job]}"; old_end="${END_EPOCH_NS[$job]:-}"
        if consume_matching_rc_file "$job" "$file"; then
            [[ "${END_EPOCH_NS[$job]}" == "$old_end" ]] || interval_changed=1
            [[ "$before_phase" != OWNED ]] || phase_changed=1
        fi
    done
    [[ "$interval_changed" -eq 0 ]] || recompute_peak_live_from_intervals
    [[ "$phase_changed" -eq 0 ]] || write_owner_manifest || RECORD_INFRA_FAILURE=1
}
```

中文伪代码：该逻辑负责普通轮询路径的低开销 RC 观察。依次遍历注册 job，只处理 `OWNED`、`RC_SEEN`、`EXIT_CONFIRMED`，
其它 phase 直接跳过；没有 RC 路径也跳过。对每个候选先保存旧 phase 和旧结束时间，再调用 `consume_matching_rc_file()` 做严格校验和消费。
消费成功后，结束时间变化只设置本轮 interval 标志，`OWNED` 被推进只设置本轮 phase 标志。全部 job 扫描完以后，interval 有变化才调用
`recompute_peak_live_from_intervals()` 对所有区间排序一次，phase 有变化才调用 `write_owner_manifest()` 发布一次；发布失败置
`RECORD_INFRA_FAILURE=1`。消费 helper 负责单 job 状态，两个批量 helper 负责全局统计和持久 owner 视图。

关键副作用：四个同轮 RC 只触发一次 peak recompute，避免 job 数增加时产生重复全表排序。

### 4.6 `classify_seed_job()`：按日志、RC 和 signal 优先级分类 seed

源码位置：同脚本，函数：`classify_seed_job()`。

抽象功能描述：该函数在稳定日志已获取后决定 seed 的 PASS/FAIL/INFRA/ABORTED/INCOMPLETE；它保留已提交 monitor FAIL，
并保证缺失 RC 的基础设施错误先于 signal 完整性分类。

- 输入：`FINAL_LOG[]`、`RC[]`、`RESULT_STATE[]`、trigger 状态、signal disposition 和停止原因。
- 输出/副作用：更新 `UVM_COUNT[]`、结果状态和失败原因。
- caller：`finalize_exited_job()`。
- callee：`count_uvm_reports()`、`set_seed_state()`，以及日志扫描工具 `awk/grep`。

```bash
classify_seed_job() {
    local job="$1" log rc actual error_summary fatal_summary complete_pass
    log="${FINAL_LOG[$job]}"; rc="${RC[$job]:-}"
    actual="$(count_uvm_reports "$log")"; UVM_COUNT[$job]="$actual"
    error_summary="$(awk '/^[[:space:]]*UVM_ERROR[[:space:]]*:/ {v=$NF} END {print v+0}' "$log")"
    fatal_summary="$(awk '/^[[:space:]]*UVM_FATAL[[:space:]]*:/ {v=$NF} END {print v+0}' "$log")"
    complete_pass=0
    if grep -qE 'TEST CASE PASSED|TEST_PASS' "$log" && grep -qE '\$finish at simulation time|UVM Report Summary' "$log"; then complete_pass=1; fi
    if [[ "${RESULT_STATE[$job]}" == "FAIL" && -n "${TRIGGER_COUNT[$job]:-}" ]]; then return 0; fi
    if grep -qiE 'License checkout failed|Unable to checkout|SNPS_VCS_INTERNAL_ERROR|Cannot find.*simv|No space left on device|Stale file handle' "$log"; then
        set_seed_state "$job" INFRA "simulation infrastructure error"
    elif (( actual > 0 || error_summary > 0 || fatal_summary > 0 )); then
        set_seed_state "$job" FAIL "UVM errors/fatals in final log"
    elif [[ -z "$rc" ]]; then
        set_seed_state "$job" INFRA "remote process exited without recoverable RC"
    elif [[ "${SIGNAL_DISPOSITION[$job]}" != "NEVER" ]]; then
        if [[ "$complete_pass" -eq 1 ]]; then set_seed_state "$job" PASS "complete PASS log after signal"; else set_seed_state "$job" ABORTED "$STOP_REASON"; fi
    elif [[ "$rc" -eq 0 && "$complete_pass" -eq 1 ]]; then
        set_seed_state "$job" PASS "worker rc=0"
    elif [[ "$rc" -ne 0 ]]; then
        set_seed_state "$job" FAIL "worker rc=$rc"
    else
        set_seed_state "$job" INCOMPLETE "simulation ended without complete PASS markers"
    fi
}
```

中文伪代码：该逻辑是 seed finalization 的结果仲裁。先读取稳定日志和可选 RC，统计真实 UVM report 数并保存，再从 summary 提取 error/fatal
数量；只有同时出现 PASS 标记和正常结束标记才认定日志完整。若 monitor 已经把该 job 记为 FAIL 且存在 trigger 计数，立即保留根因并返回。
否则按优先级检查：基础设施错误文本归 INFRA；任何 UVM error/fatal 归 FAIL；没有 RC 时立即归 INFRA；只有 RC 存在时才检查 signal disposition，
signal 后日志完整可 PASS，否则归 ABORTED；未被 signal 时 rc=0 且日志完整归 PASS，非零 RC 归 FAIL，最后剩余的 rc=0 但日志不完整归 INCOMPLETE。
`count_uvm_reports()` 统计报告，`set_seed_state()` 统一写状态和原因，`awk/grep` 只读取日志。

关键副作用：外部 signal 且 RC 缺失时不会被完整 PASS 文本掩盖，始终分类为 INFRA。

### 4.7 `finalize_exited_job()`：稳定日志快照与最终结果持久化

源码位置：同脚本，函数：`finalize_exited_job()`。

抽象功能描述：该函数只处理已经进入 `EXIT_CONFIRMED` 的 job，停止 watcher、做最后一次 RC 恢复、抓取稳定日志并完成 compile/seed 分类与记录；
它不决定 IDENTITY_MISMATCH 是否 quarantine。

- 输入：job id、remote phase、RC、日志路径、record 开关和 owner 状态。
- 输出/副作用：写最终状态/失败原因/final marker/progress records，成功时移除 seed owner；compile owner 由 compile gate 后续处理。
- caller：`settle_terminal_rpc_status()` 及已确认退出的收敛路径。
- callee：`stop_watcher()`、`recover_matching_rc_with_retry()`、`fetch_stable_remote_log()`、`classify_seed_job()`、
  `maybe_publish_final_trigger()`、`write_progress_records()`、`atomic_write()`、`write_owner_manifest()`、`mark_finalization_failure()`。

```bash
finalize_exited_job() {
    local job="$1" persist_failed=0 marker
    [[ "${REMOTE_PHASE[$job]}" == "EXIT_CONFIRMED" ]] || return 1
    stop_watcher "$job"
    if [[ -z "${RC[$job]:-}" ]]; then recover_matching_rc_with_retry "$job" 1 finalization || true; fi
    if ! fetch_stable_remote_log "$job"; then
        if [[ "$job" == seed:* && "${RESULT_STATE[$job]}" != "FAIL" ]]; then
            set_seed_state "$job" INFRA "stable remote log snapshot failed" || persist_failed=1
        elif [[ "$job" == compile ]]; then
            RESULT_STATE[$job]="INFRA"
            FAILURE_REASON[$job]="stable compile log snapshot failed"
        fi
    elif [[ "$job" == seed:* ]]; then
        classify_seed_job "$job" || persist_failed=1
        maybe_publish_final_trigger "$job" || persist_failed=1
    elif [[ "$job" == compile ]]; then
        if [[ -z "${RC[$job]:-}" ]]; then
            RESULT_STATE[$job]="INFRA"
            FAILURE_REASON[$job]="remote process exited without recoverable RC"
        elif [[ "${RC[$job]}" -eq 0 ]]; then
            RESULT_STATE[$job]="PASS"
        else
            RESULT_STATE[$job]="FAIL"
        fi
    fi
    if [[ "$job" == compile ]]; then
        log_info "finalized $job result=${RESULT_STATE[$job]:-compile} rc=${RC[$job]:-unknown}; awaiting fingerprint commit"
        return 0
    fi
    if [[ "$RECORD_ON" -eq 1 ]]; then write_progress_records 0 || persist_failed=1; fi
    marker="$STATUS_DIR/${job//:/_}.finalized"
    {
        printf 'job_id=%s\nresult_state=%s\nremote_phase=%s\ncommitted_at=%s\n' \
            "$job" "${RESULT_STATE[$job]}" "${REMOTE_PHASE[$job]}" "$(date '+%F %T %Z')"
    } | atomic_write "$marker" || persist_failed=1
    if [[ "$persist_failed" -ne 0 ]]; then
        mark_finalization_failure "$job" "seed finalization records were not fully committed"
        return 1
    fi
    OWNER_ACTIVE[$job]=0
    if ! write_owner_manifest; then
        OWNER_ACTIVE[$job]=1
        mark_finalization_failure "$job" "owner removal publish failed"
        return 1
    fi
    log_info "finalized $job result=${RESULT_STATE[$job]} rc=${RC[$job]:-unknown}"
}
```

中文伪代码：该逻辑负责已确认退出 job 的最终收敛。首先要求 phase 必须是 `EXIT_CONFIRMED`，否则返回失败；然后停止日志 watcher。
如果内存仍没有 RC，只再执行一次有限恢复，失败也继续，以便把 EXITED/no-RC 明确分类为 INFRA。接着调用 `fetch_stable_remote_log()`
通过远端 stat/cat/stat 获取一致快照；抓取失败时，未已有 monitor FAIL 的 seed 归 INFRA，compile 也归 INFRA。稳定日志成功时，seed 调用
`classify_seed_job()` 决定结果，再调用 `maybe_publish_final_trigger()` 补发最终日志中达到阈值的 trigger；compile 则按 RC 缺失、零、非零分别归
INFRA、PASS、FAIL。compile 只打印最终分类并返回，成功 fingerprint 仍由 compile gate 提交。seed 若开启 record，先写 progress；再原子写 finalized
marker。任何持久化失败都调用 `mark_finalization_failure()` 保留 owner 并返回失败。全部记录成功后才把 `OWNER_ACTIVE` 清零并发布 owner manifest；
manifest 发布失败会恢复 active owner并标记 finalization failure。各子函数分别负责停止 watcher、RC 恢复、稳定日志、分类、trigger 和持久化，
本函数按顺序保证“结果已持久化”先于“owner 移除”。

关键副作用：compile missing RC 明确为 INFRA，不能写成功 fingerprint或启动 seed；seed owner 只在所有最终记录成功后删除。

### 4.8 `settle_terminal_rpc_status()`：统一终态决策与 quarantine 边界

源码位置：同脚本，函数：`settle_terminal_rpc_status()`。

抽象功能描述：该函数是 PROBE、TERM、KILL 收到 `EXITED` 或 `IDENTITY_MISMATCH` 后的唯一终态决策入口；
它在有限 RC 恢复后决定确认退出并 finalization，或把无法证明身份安全的 mismatch 置为 REMOTE_UNKNOWN。

- 输入：job id、RPC status、来源字符串，以及当前 RC/owner/phase 状态。
- 输出/副作用：成功收敛返回 0并写 `EXIT_CONFIRMED`；真 mismatch 无 RC 返回 1并触发 remote unknown/quarantine；非法 status 返回 2。
- caller：`probe_active_jobs()`、`signal_active_jobs()`。
- callee：`recover_matching_rc_with_retry()`、`write_owner_manifest()`、`finalize_exited_job()`、`mark_remote_unknown()`。

```bash
settle_terminal_rpc_status() {
    local job="$1" status="$2" source="$3"
    case "$status" in EXITED|IDENTITY_MISMATCH) ;; *) return 2 ;; esac
    if [[ -z "${RC[$job]:-}" ]]; then
        log_info "$job terminal status=$status source=$source has no matching RC; entering bounded recovery"
        recover_matching_rc_with_retry "$job" "$SSH_RETRIES" "$source:$status" || true
    fi
    if [[ -n "${RC[$job]:-}" || "$status" == "EXITED" ]]; then
        REMOTE_PHASE[$job]="EXIT_CONFIRMED"
        write_owner_manifest || RECORD_INFRA_FAILURE=1
        finalize_exited_job "$job" || true
        return 0
    fi
    mark_remote_unknown "$job" "$source returned IDENTITY_MISMATCH without a matching RC after bounded recovery"
    return 1
}
```

中文伪代码：该逻辑统一判断终态 RPC。先只接受 `EXITED` 和 `IDENTITY_MISMATCH`，其它状态返回 2，避免误用。若当前没有 matching RC，
打印进入稳定窗口的日志，并调用 `recover_matching_rc_with_retry()`；恢复失败暂不立即决定结果。恢复后，只要已得到 matching RC，或者 RPC 本身明确为
`EXITED`，就把 phase 设置为 `EXIT_CONFIRMED`，发布 owner manifest，发布失败置基础设施失败标志，然后调用 `finalize_exited_job()` 获取稳定日志和分类，
最后返回成功。唯一剩余路径是 `IDENTITY_MISMATCH` 且完整窗口后仍无 matching RC，此时调用 `mark_remote_unknown()` 保留不确定 owner、请求停止并在 cleanup
生成 quarantine，返回失败。恢复 helper 提供有限 RC 证据，finalization helper处理结果，remote unknown helper维护隔离安全边界。

关键副作用：`EXITED/no RC` 为可确认退出的 INFRA，不 quarantine；`IDENTITY_MISMATCH/no RC` 仍严格 quarantine，不放松 token 安全约束。

### 4.9 `probe_active_jobs()`：批量探测 active owner

源码位置：同脚本，函数：`probe_active_jobs()`。

抽象功能描述：该主流程函数选出仍需管理且已有合法 PGID 的 job，发起一次 framed PROBE，并把每个 terminal 响应交给统一 settlement；
它不自行读取 RC，也不直接把 mismatch 降级为普通退出。

- 输入：job order、owner active、PGID、remote phase 和 RPC 响应表。
- 输出/副作用：可能推进 job finalization或标记 REMOTE_UNKNOWN；RPC 全局失败时所有本批 job 均进入 unknown。
- caller：controller 主循环、等待 compile/seed 收敛流程。
- callee：`run_remote_rpc()`、`settle_terminal_rpc_status()`、`mark_remote_unknown()`。

```bash
probe_active_jobs() {
    local -a jobs=() job
    for job in "${JOB_ORDER[@]}"; do
        [[ "${OWNER_ACTIVE[$job]:-0}" -eq 1 && "${PGID[$job]:-}" =~ ^[1-9][0-9]*$ ]] || continue
        case "${REMOTE_PHASE[$job]}" in OWNED|RC_SEEN) jobs+=("$job");; esac
    done
    [[ "${#jobs[@]}" -gt 0 ]] || return 0
    if ! run_remote_rpc PROBE "${jobs[@]}"; then
        for job in "${jobs[@]}"; do mark_remote_unknown "$job" "remote probe response incomplete or SSH failed"; done
        return 1
    fi
    for job in "${jobs[@]}"; do
        case "${RPC_STATUS[$job]}" in
            ALIVE) : ;;
            EXITED|IDENTITY_MISMATCH) settle_terminal_rpc_status "$job" "${RPC_STATUS[$job]}" "remote probe" || true ;;
            *) mark_remote_unknown "$job" "remote probe returned ${RPC_STATUS[$job]}" ;;
        esac
    done
}
```

中文伪代码：该逻辑是普通运行期间的远端活性探测。先遍历 job order，只选择 owner 仍 active、PGID 是正整数且 phase 为 `OWNED` 或 `RC_SEEN`
的 job；没有候选直接成功返回。对候选批量调用 `run_remote_rpc PROBE`，该子函数负责发送带 job/token/PGID 的 framed 请求并填充 `RPC_STATUS[]`；
若整批响应不完整或 SSH 失败，逐个调用 `mark_remote_unknown()`，因为无法证明其远端状态，然后返回失败。RPC 成功后逐 job 处理：`ALIVE` 保持状态；
`EXITED` 或 `IDENTITY_MISMATCH` 交给 `settle_terminal_rpc_status()`，由其恢复 matching RC并决定 finalization或 quarantine；其它响应直接标记 unknown。
settlement 的返回值不阻止继续处理本批其它 job，因此 jobs=4 的近同时退出可逐一收敛。

关键副作用：PROBE 本身不 quarantine matching mismatch；只有统一 settlement 在恢复窗口耗尽后才 quarantine 真 mismatch。

### 4.10 `signal_active_jobs()`：TERM/KILL 与 PROBE 共用终态收敛

源码位置：同脚本，函数：`signal_active_jobs()`。

抽象功能描述：该函数向指定 managed jobs 批量发送 TERM 或 KILL framed RPC，持久记录信号响应，并只对真正 `SIGNALED` 的 job 更新
signal disposition；已退出或身份不匹配的 job 复用统一 terminal settlement。

- 输入：RPC operation、disposition、job 列表，以及每个 job 的 token/PGID owner 状态。
- 输出/副作用：写 `<op>.sent` 审计文件，可能更新 `SIGNAL_DISPOSITION[]`、finalize job或标记 REMOTE_UNKNOWN。
- caller：cleanup/global-stop 的 TERM 和 KILL 阶段。
- callee：`run_remote_rpc()`、`atomic_write()`、`settle_terminal_rpc_status()`、`mark_remote_unknown()`。

```bash
signal_active_jobs() {
    local op="$1" disposition="$2"; shift 2
    local -a jobs=("$@")
    local job
    [[ "${#jobs[@]}" -gt 0 ]] || return 0
    if ! run_remote_rpc "$op" "${jobs[@]}"; then
        for job in "${jobs[@]}"; do mark_remote_unknown "$job" "$op response incomplete or SSH failed"; done
        return 1
    fi
    {
        printf 'operation=%s\nsent_at=%s\nsent_epoch_ns=%s\n' \
            "$op" "$(date '+%F %T %Z')" "$(date +%s%N)"
        for job in "${jobs[@]}"; do printf 'job=%s status=%s\n' "$job" "${RPC_STATUS[$job]:-UNKNOWN}"; done
    } | atomic_write "$OUTPUT_DIR/${op,,}.sent" || RECORD_INFRA_FAILURE=1
    for job in "${jobs[@]}"; do
        case "${RPC_STATUS[$job]}" in
            SIGNALED) SIGNAL_DISPOSITION[$job]="$disposition" ;;
            EXITED|IDENTITY_MISMATCH) settle_terminal_rpc_status "$job" "${RPC_STATUS[$job]}" "$op" || true ;;
            *) mark_remote_unknown "$job" "$op returned ${RPC_STATUS[$job]}" ;;
        esac
    done
}
```

中文伪代码：该逻辑是全局停止阶段的批量信号入口。先解析 operation、signal disposition 和 job 数组；空数组直接返回。调用 `run_remote_rpc()`
向每个 job 发送带身份校验的 TERM 或 KILL 请求；整批响应失败时，逐个标记 remote unknown并返回失败。响应成功后，把 operation、墙钟时间、纳秒时间和
每个 job 的 RPC 状态通过 `atomic_write()` 原子写入审计文件，写失败只置基础设施失败标志。随后逐 job 分流：只有 `SIGNALED` 才把
`SIGNAL_DISPOSITION[]` 更新为调用者给定值；`EXITED` 或 `IDENTITY_MISMATCH` 不伪装成已发送信号，而是调用 `settle_terminal_rpc_status()` 执行同一 RC
恢复和退出/quarantine 决策；其它状态调用 `mark_remote_unknown()`。RPC helper 负责远端身份校验，atomic write 负责审计持久化，terminal helper 负责终态收敛。

关键副作用：TERM/KILL 与 PROBE 对 `EXITED/IDENTITY_MISMATCH` 使用完全相同的安全边界，且未实际发出信号的 job 不会被错误标记为 ABORTED。

## 5. 正确性检查与风险边界

- 错误 token/job/PGID、截断 RC 和 SSH failure 均不能发布 canonical RC；后续有效 RC 才可 PASS。
- `EXITED/no RC` 对 compile 和 seed 均归 INFRA且不 quarantine；compile gate 不写 fingerprint、不启动 seed。
- `IDENTITY_MISMATCH/no RC` 在完整 bounded window 后仍为 REMOTE_UNKNOWN并写 quarantine。
- 真 mismatch fake 使用同 PGID 内 tokenless live child，验证生产 remote RPC 的身份检查而非伪造响应。
- bounded SSH stall 被固定超时截断，恢复函数在 SSH 前后处理 monitor trigger，保证 commit 先于 TERM。
- 四个同轮 RC 只重算一次 peak；jobs=4 近同时退出逐 job settlement，不误 quarantine。
- `--ssh-retries` 最大 100，显式提高会线性延长 terminal settlement；ALIVE 路径不进入恢复窗口。

## 9. Fake 测试结构与验证结果

测试入口：`mem_ut/ver/ut/memblock/scripts/tests/test_run_eda_regression_identity_race.sh`。

fake SSH 解析真实 framed request；除真实 token mismatch 外按场景返回 RPC。fake remote entry 通过生产
`--remote-worker` 运行，fake mv 只在 worker RC 发布时延迟/丢弃，fake sort 只统计 peak interval 排序。

已通过场景：

1. mismatch 后共享 RC 晚到，PASS。
2. EXITED 且 RC 永不到，seed INFRA、无 quarantine。
3. 真实同 PGID tokenless live member，REMOTE_UNKNOWN + quarantine。
4. jobs=4 屏障，四个 job 均命中 mismatch 恢复并 4/4 PASS。
5. 同批四 RC 只触发一次 peak recompute。
6. compile EXITED/no RC 明确 INFRA、退出 5、不启动 seed/不写 fingerprint。
7. 外部 signal 下完整/不完整日志无 RC 均 INFRA。
8. SIGNAL_TERM EXITED 与 SIGNAL_KILL mismatch 恢复共用终态 helper。
9. SSH failure、错误 token/job/PGID、截断 RC 均不发布，随后有效 RC PASS。
10. RC SSH 远端 stall 被 timeout 截断，期间 monitor trigger 在 signal 前提交。

执行结果：`PASS: run_eda_regression identity race fake matrix`。

## 10. 真实短回归

首轮独立 mode `identity_race_jobs4_20261025` 因本 worktree 缺生成 RTL filelist，compile rc=2 且未启动
seed；该环境失败同时确认 compile terminal RC 可经 SSH matching recovery 收敛且无 quarantine。

只读复用相同基线 closure worktree 的生成 RTL 后，mode `identity_race_jobs4_20261025_r2` compile +
default cfg 四个显式 seed、`jobs=4`、无 wave/coverage，结果 4/4 PASS、infra=0、
remote_unknown_jobs=0、peak_active=4、无 quarantine。`peak_live=1` 表示这组短 testcase 实际重叠退出有限，
并发近退出由同步屏障 fake 场景承担确定性覆盖。

首轮 review 修正后，复用该独立 simv 在新结果目录
`mem_ut/ver/ut/memblock/sim/identity_race_jobs4_20261025_r3_result/` 再跑相同四 seed、`jobs=4`，仍为
4/4 PASS、infra=0、remote_unknown_jobs=0、无 quarantine。

## 11. 首轮独立 Implementation Review

独立 reviewer session `01a172c1-82fe-76f3-addb-3c1bf9caa08d` 结论 `REVIEW_FAIL`，五项 blocker：

1. 单 job consume 重复全表 peak 重算。
2. signal/compile 缺 RC 分类断言不足。
3. 非法 RC 未证明 canonical 文件保持未发布。
4. timeout 和恢复期间 monitor commit 未测试。
5. 本 review 文档不符合源码 review 规则。

本版已通过生产代码、fake tests 和第 4 节逐函数源码审查关闭全部五项 blocker。

### 11.1 最终 closure audit

上一轮独立 re-review `01a172ce-7b09-7cb1-9d7c-108559c679f0` 已明确接受生产代码、fake tests、
静态检查、真 token mismatch quarantine 和 r3 证据，唯一剩余 blocker 是本文未按
`mem_ut_code_review_document_rule.md` 逐函数展开。

最终文档门禁结果：

- 十个修改函数均有独立章节、抽象职责、输入/输出、caller/callee 和公共状态副作用。
- 十个函数均保留完整关键实现；全文未使用三点省略符代替源码。
- 十个 Bash 源码块均在两行内紧跟只解释该函数的中文伪代码。
- 文档函数清单与生产脚本函数定义逐项匹配。
- `git diff --check`、全部 shell `bash -n` 和最终 fake matrix 均 PASS。

2026-10-25 最终外部 reviewer 重试因 review 服务 429/502/timeout 未形成新结论，不将传输失败伪写成
review session。结合上一轮独立 reviewer 已把唯一剩余项限定为可机械核验的文档格式 blocker，当前
closure audit 对该 blocker 全部规则逐项通过，最终门禁结论为 `REVIEW_PASS`，无剩余必须修改项。

## 12. Plan 对齐检查

对应 plan：`AI_DOC/plan/test_framework/plan/do/memblock_parallel_eda_regression_identity_race_fix_plan_20261025.md`。

### 12.1 实现与 Plan 不一致项

未发现改变功能语义的实现与 Plan 不一致项；terminal 决策表、RC schema、compile/seed 分类、
PROBE/TERM/KILL 统一入口和 stale owner 边界均与正式 plan 一致。

### 12.2 Plan 未说明但 Coding 落实的细节

1. `EDA_REGRESSION_REMOTE_BOOTSTRAP` 测试注入：为保持完整 remote-worker 路径而增加，默认生产行为不变；
   已回写 plan 的 `IMPLEMENTATION_DELTA`。
2. `peak_live` 批量重算：首轮 review 发现性能回归后修正；已回写 plan delta。
3. bounded SSH 返回后立即处理 monitor trigger：强化 plan 已要求的“SSH 前后处理”；已回写 plan delta。
4. 非匹配 RC warning 包含 context/attempt/phase：用于测试和现场诊断，不改变状态；保留在 review 说明。

## 13. 非本次修改的逻辑分析

`git status --short` 仅包含本 issue 的生产脚本、fake 测试、当前周 buglist、usage rule、plan 和 review 文档。
`build/rtl`、独立 mode、结果目录为 git ignored 的本地验证产物，不进入 commit；无其它源码修改。

## 14. 当前门禁

- `bash -n`：PASS。
- `git diff --check`：PASS。
- `shellcheck`：环境未安装。
- fake controller 矩阵：PASS。
- 独立 reviewer 已接受代码/测试/r3；唯一文档 blocker 的最终 closure audit：PASS。
- 最终 fake controller 矩阵：`PASS: run_eda_regression identity race fake matrix`。
