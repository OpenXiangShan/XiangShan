# T05 BackendModel 设计

## 1. 目标

`BackendModel` 在黑盒边界上模拟后端反馈行为，保证 Frontend 持续可运行并可注入控制事件：

- 持续消费 FTQ（commit）
- 生成 resolve/update/redirect
- 处理 canAccept 背压
- 基于 `io_frontendInfo_ibufFull` 做看门狗保护

## 2. 核心接口

- `bind(dut) -> None`
- `on_clock_edge(cycle: int) -> None`
- `inject_redirect(target_pc: int, reason: str) -> None`
- `inject_exception(cause: int, tval: int, pc: int) -> None`
- `wait_for_commits(n: int, max_cycles: int) -> int`
- `set_can_accept(value: int) -> None`

## 3. 数据结构

```text
ftq_queue: deque[FrontendPacket]   # 深度默认32
commit_ptr: {flag: int, value: int}
pending_events: deque[BackendEvent]
watchdog: {ibuf_full_streak: int, threshold: int}
```

`FrontendPacket` 来自 monitor 或直接采集 `cfVec`。

## 4. commit 驱动策略

- 每周期若 `ftq_queue` 非空则驱动一次 `io_backend_toFtq_commit_valid`。
- 提交指针按 `(value + 1) % FTQ_SIZE` 前进，回卷时翻转 `flag`。
- 若队列为空，commit 拉低。

目标：避免 FTQ 填满导致前端停取指。

## 5. redirect/resolve 事件

地址语义约束：

- `BackendModel` 内部一律保存 byte address。
- `io_backend_toFtq_resolve_{0..2}_bits_pc_addr` 和 `...target_addr` 对应 `Resolve.pc/target: PrunedAddr`，驱动 DUT 前必须做 `byte_addr >> 1`。
- `io_backend_toFtq_redirect_bits_pc` 和 `...target` 虽然也是 50bit 端口，但语义仍是完整 byte address，驱动 DUT 时不能右移。
- `io_backend_fromFtq_startPc_addr` 从 DUT 读回时已经是 `>> 1` 后的编码值；testbench 若要缓存和参与内部计算，必须先还原成 byte address。

### 5.1 redirect 注入

驱动字段（最小集）：

- `io_backend_toFtq_redirect_valid`
- `io_backend_toFtq_redirect_bits_pc`
- `io_backend_toFtq_redirect_bits_target`
- `io_backend_toFtq_redirect_bits_taken`
- `io_backend_toFtq_redirect_bits_ftqIdx_*`

约束：

- redirect 的 `pc/target` 直接发送 byte address。

### 5.2 resolve/update

按需驱动 `io_backend_toFtq_resolve_{0..2}_*`：

- `mispredict=1` 时配合 redirect 触发恢复路径。

约束：

- resolve 的 `pc_addr/target_addr` 统一由 byte address 右移 1 位得到。
- `pc_addr` 使用 FTQ entry start PC，不使用具体分支指令 PC。

## 6. 背压与看门狗

- `io_backend_canAccept` 可动态切换，模拟后端拥塞。
- 若 `io_frontendInfo_ibufFull` 连续高于阈值（如 32 cycle），触发强制 redirect 到安全 PC。

## 7. 与 GoldenTrace 的闭环

- `GoldenTrace` 提供下一条应执行分支/异常事件。
- `BackendModel` 对比实际输出后决定是否发 update/redirect。

## 8. 验收标准

1. 1000 周期持续运行中 FTQ 不溢出。
2. 人工注入 redirect 后 monitor 观测到 PC 跳转。
3. 背压打开时前端可暂停，背压释放后可恢复。
4. ibufFull 连续异常时看门狗可解除卡死。
