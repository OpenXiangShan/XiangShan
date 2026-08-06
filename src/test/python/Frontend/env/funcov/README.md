# Frontend 功能覆盖率建模说明

本目录存放 frontend Python 验证环境中的功能覆盖率相关代码。

ICache WayLookup 模型写在：

`src/test/python/Frontend/env/funcov/py/icache/icache_waylookup_funcov.py`

对应主测试点表中 `cacheable取指/icache/icache waylookup` 下的 42 个叶子，覆盖队列读写、双 entry 出队、MSHR update、异常 entry、global/BPU flush、容量边界和指针环回。模型只采样 DUT 状态、接口握手和跨周期状态，不用 Checkpoint 结果作为命中条件。

ICache hit/miss 路径模型写在：

`src/test/python/Frontend/env/funcov/py/icache/icache_hitmiss_funcov.py`

对应主测试点表中 `cacheable取指/icache/hit路径` 与 `cacheable取指/icache/miss路径` 下的 10 个叶子，覆盖同 cacheline hit、跨 cacheline hit、双 fetch hit、hit 保护异常、fetch/prefetch 并发、fetch refill 后 prefetch hit、连续 fetch miss merge、PLRU victim 选择和 refill 后同地址再次 hit。模型只使用 MainPipe、MissUnit、MSHR 与 victim 选择相关 DUT 信号；跨周期 refill 关系由 sampler 内部状态保存，Checkpoint 中的正确性仍由 testcase、checker 或 scoreboard 证明。

IFU 的覆盖率按 coverage group 拆分：

- `src/test/python/Frontend/env/funcov/py/ifu/cfvec_funcov.py`：CFVec 的指令尺寸、边界位置、CFI 类型和页边界时序。
- `src/test/python/Frontend/env/funcov/py/ifu/compact_funcov.py`：IFU 到 IBuffer 的 compact layout、RVC 展开、异常和 two-fetch source 补充点。
- `src/test/python/Frontend/env/funcov/py/ifu/sampler.py`：兼容入口和聚合导出，保持旧测试的导入路径不变。

FTQ 的 two-fetch coverage group 也按请求、WayLookup、MainPipe、IFU delivery 和 checker 分文件维护；跨周期状态采样保留在 `two_fetch_funcov.py`，由 `ftq/sampler.py` 统一调度。

地址翻译与权限检查模型写在：

`src/test/python/Frontend/env/funcov/address_translation_permission.py`

对应源测试点：

`src/test/python/Frontend/docs/02_testpoint/Frontend_testpoint_v1.0.csv`

当前只处理 CSV 逻辑行 `382-532`。

## 建模规则

`CoverageBin` 只描述功能覆盖率命中条件，必须是 DUT/SV 信号组合。

`Condition` 用来构造 coverage bin。

`Checkpoint` 不进入 coverage bin。`Checkpoint` 是 checker、scoreboard 或断言要检查的预期行为。

### 测试点与 funcov 的边界

测试点的 `Condition` 和叶子场景描述决定 funcov 的采样条件：只要 DUT 可见信号证明该场景或其必要时序关系已经出现，就可以标为 `MODELED`。`Checkpoint` 只定义该场景出现后 DUT 应有的正确行为；它不得进入 coverpoint/bin，也不得作为 `MODELED`、`PARTIAL` 或 `UNMAPPED` 的判定理由。

`PARTIAL` 仅用于场景本身只能观测到一部分、不同事件不能确认属于同一事务、或一个 aggregate bin 混合了多个待区分场景。若现有 bin 以 `cfVec`、指针恢复、旧路径抑制等 Checkpoint 结果作为额外门槛，应拆成只采样 Condition 的 funcov bin，并将正确性保留给 testcase、checker 或 scoreboard。

单周期组合覆盖写在 Python 索引里。带时序关系的覆盖写在 SV bind 文件里，用 `covergroup` / `coverpoint` 和必要状态寄存器建模。

当前模型识别 113 个五级测试点，其中 110 个已经建立 coverage bin。CSV 526 行是没有 Condition/Checkpoint 的迁移占位项；491 行缺少 locked entry CSR 重写尝试事件，532 行缺少 Frontend DUT 边界上的 PBMTE 配置输入。这两项不会用 Checkpoint 结果伪造成 coverage bin，原因写在 `address_translation_permission.py` 的 `UNMODELED_POINTS` 和 `NON_EXECUTABLE_POINTS`。

## ICache MissUnit 模型

ICache MissUnit 模型写在：

`src/test/python/Frontend/env/funcov/py/icache/icache_missunit_funcov.py`

该模型对应 CSV 中 BIN-686 至 BIN-716 的 31 个叶子，覆盖请求分配、MSHR 容量、查重、Acquire 仲裁、prefetch FIFO、redirect/fence.i、TileLink refill 和 source 路由。模型只使用 DUT signal contract 中注册的内部状态与接口信号，不使用 testcase Checkpoint 结果作为命中条件。

MissUnit 时序判断应与 RTL 保持一致：同拍 fetch/prefetch merge 必须接受 RTL 的 `prefetchHit` 结果；data beat 判断必须匹配 TileLink `edge.hasData`；prefetch FIFO issue 必须校验 FIFO 队首与 Acquire source；refill 写回和 source 路由必须要求有效 response。对应行为测试位于 `tests/test_icache_functional_coverage.py`。

## 每个 coverage bin 在哪里

每个 coverage bin 的索引都显式写在：

`src/test/python/Frontend/env/funcov/address_translation_permission.py`

具体位置：

- `SV_BIN_SPECS`：单周期 SV 组合覆盖。
- `SV_BIND_BIN_SPECS`：第一批 SV bind coverpoint 的索引。
- `SV_REMAINING_BIN_SPECS`：CSR context、PMP/PMA、PBMT 和地址属性时序覆盖索引。

`SV_BIN_SPECS` 和 `SV_BIND_BIN_SPECS` 中每个 `_b(...)` 调用就是一个 coverage bin 索引。字段含义：

- `row`：源 CSV 逻辑行号。
- `bin_id`：coverage bin ID。
- `group`：coverage group。
- `name`：bin name。
- `signals`：该 bin 采集的 SV 信号。
- `expr`：该 bin 命中的 SV 布尔表达式。

时序覆盖的真正 SV 实现写在：

`src/test/python/Frontend/env/funcov/sv/address_translation_permission_funcov.sv`

其中每个 `ATP_xxx:` label 对应一个 VCS functional coverage coverpoint。

构建接入点在 `Makefile`：

- `FRONTEND_FUNCOV_SV_DIR`
- `FRONTEND_FUNCOV_FILELIST`
- `picker export --filelist $(FRONTEND_FUNCOV_FILELIST)`

## 386 行测试点

CSV 386 行：

`非虚拟化，地址翻译处于 Bare 模式且没有额外页表保护模式`

Condition：

`io_tlbCsr_priv_virt=0；io_tlbCsr_satp_mode=4'h0`

对应 coverage bin 写在 `address_translation_permission.py` 的 `SV_BIN_SPECS`：

```python
_b(
    row=386,
    bin_id="ATP-001",
    group="itlb_translation_mode_bare",
    name="nonvirtual_bare",
    signals=(
        "io_tlbCsr_priv_virt",
        "io_tlbCsr_satp_mode",
    ),
    expr=(
        "(io_tlbCsr_priv_virt == 1'b0) && "
        "(io_tlbCsr_satp_mode == 4'h0)"
    ),
)
```

386 行没有采集 `io_ptw_req_0_valid`。原因是“不发起普通 PTW 页表翻译请求”来自 `Checkpoint`，不属于 coverage bin 的命中条件。

## 时序 coverage 示例

CSV 420 行：

`PTW 返回一个只打开部分 lane 的 sector TLB entry，ITLB 访问其中一个有效 lane 覆盖的页内区域`

对应索引写在 `address_translation_permission.py` 的 `SV_BIND_BIN_SPECS`：

```python
_b(
    row=420,
    bin_id="ATP-059",
    group="itlb_sector_entry",
    name="s1_sector_valid_lane",
    signals=(
        "ptw_resp_valid",
        "ptw_resp_s2xlate",
        "ptw_resp_s1_entry_v",
        "ptw_resp_s1_entry_level",
        "atp_ptw_resp_lane",
        "ptw_resp_s1_valididx",
    ),
    expr="atp_s1_4k_sector_resp && atp_resp_lane_valid",
)
```

真正 coverpoint 写在 `address_translation_permission_funcov.sv`：

```systemverilog
covergroup frontend_atp_funcov_cg @(posedge clock);
  ATP_420_s1_sector_valid_lane:
    coverpoint (atp_s1_4k_sector_resp && atp_resp_lane_valid) iff (!reset) {
      bins hit = {1'b1};
    }
endgroup
```

CSV 460 行：

`等待 PTW resp 返回期间，收到相同上下文的 sfence`

对应索引写在 `address_translation_permission.py` 的 `SV_BIND_BIN_SPECS`：

```python
_b(
    row=460,
    bin_id="ATP-049",
    group="itlb_sfence_ptw_interaction",
    name="sfence_during_ptw_wait",
    signals=(
        "atp_seen_sfence",
        "ptw_resp_valid",
    ),
    expr="atp_seen_sfence && ptw_resp_valid",
)
```

真正 coverpoint 写在 `address_translation_permission_funcov.sv`：

```systemverilog
covergroup frontend_atp_funcov_cg @(posedge clock);
  ATP_460_sfence_during_ptw_wait:
    coverpoint (atp_seen_sfence && ptw_resp_valid) iff (!reset) {
      bins hit = {1'b1};
    }
endgroup
```

## SV funcov 文件写作模板

SV funcov 只采集 Frontend DUT 可见的触发条件、状态和时序关系；`Checkpoint` 中的正确性判断仍由 testcase、checker 或 scoreboard 负责。一个 `*_funcov.sv` 文件负责一个连续的前端观察域，例如 IBuffer delivery、IFU exception delivery 或 redirect/flush recovery；跨域接线只放在 `sv/zz_frontend_funcov_bind.sv`。

文件结构固定如下。其中 `<domain>` 是观察域，`<PREFIX>` 是稳定的大写前缀，名称应能直接追溯到测试点或 coverage bin。

```systemverilog
module frontend_<domain>_funcov (
  input logic clock,
  input logic reset,
  // 仅列出该观察域需要的 DUT 信号
);

  wire request_fire = request_valid && request_ready;

  covergroup frontend_<domain>_cg @(posedge clock);
    option.per_instance = 1;

    <PREFIX>_request_accept_cp:
      coverpoint (request_valid && request_ready) iff (!reset) {
        bins observed = {1'b1};
      }
  endgroup

  frontend_<domain>_cg cg = new();
endmodule
```

### Coverpoint 粒度

一个 coverpoint 对应一个可独立解释、可由 testcase 命中的观察项。它可以是一个测试点叶子，也可以是同一采样维度下的互斥叶子集合；不按代码行数、信号个数或“每个 point 至少两个 bins”拆分。

- 单一事件、单一组合或单一时序关系只需要一个 point 和一个 bin。例如“PTW response 与 sfence 同周期出现”是一个观察项：

  ```systemverilog
  ATP_sfence_during_ptw_wait_cp:
    coverpoint (seen_sfence && ptw_resp_valid) iff (!reset) {
      bins observed = {1'b1};
    }
  ```

- 同一维度的互斥分类放在一个 point 的多个 bins 中。功能有效位属于采样值，`iff` 中仍只保留 reset。例如同一次 exception delivery 的 exception type：

  ```systemverilog
  wire [3:0] exception_delivery_kind = {
    ifu_to_ibuffer_fire,
    ifu_exception_type
  };

  IFED_exception_type_cp:
    coverpoint exception_delivery_kind iff (!reset) {
      bins instruction_page_fault = {4'b1001};
      bins instruction_guest_page_fault = {4'b1010};
      bins instruction_access_fault = {4'b1011};
    }
  ```

- 能在同一个周期同时发生的观察不能合并为 `if` / `else if` 编码。每个并发条件各用一个直接谓词的 coverpoint，避免低优先级条件被隐藏：

  ```systemverilog
  RFR_control_redirect_cp:
    coverpoint (backend_redirect_valid && backend_redirect_is_ctrl) iff (!reset) {
      bins observed = {1'b1};
    }
  RFR_memory_violation_redirect_cp:
    coverpoint (backend_redirect_valid && backend_redirect_is_mem_vio) iff (!reset) {
      bins observed = {1'b1};
    }
  ```

- 跨周期关系由最少的历史寄存器表达。历史状态只保存下一次采样所需的事实；不要为了覆盖而复制业务状态机，也不要用 checker 的预期结果作为命中条件。例如前一周期请求、当前周期响应：

  ```systemverilog
  logic request_seen;

  always_ff @(posedge clock) begin
    if (reset) begin
      request_seen <= 1'b0;
    end else begin
      request_seen <= request_fire;
    end
  end

  <PREFIX>_response_after_request_cp:
    coverpoint (request_seen && response_valid) iff (!reset) {
      bins observed = {1'b1};
    }
  ```

### Coverpoint 写法

1. coverpoint 表达式写完整的功能命中条件，`iff` 只做全局采样门控，通常是 `iff (!reset)`。禁止 `coverpoint 1'b1 iff (!reset && condition)`，也不要把功能条件藏进 `iff`。
2. 直接使用可读的布尔谓词。表达式较长时按逻辑子条件换行；不要为了压缩 point 数量而引入 `always_comb` event selector。
3. point 名称采用 `<PREFIX>_<observable>_cp`；单一命中 bin 用 `observed`、`hit` 或描述该结果的名字。多 bin 时，每个 bin 名称表达实际类别，禁止用无意义的 `bin0`、`other` 补位。
4. bins 只描述需要分别计数的可观察类别。没有有意义的补集时，不创建 false/default bin；一个 point 只有一个 bin 是正常写法。
5. 条件、bin 值和 CSV/SV 覆盖索引必须一一可追溯。一个 aggregate OR point 不能作为多个独立测试点叶子的命中证据；应拆成叶子 point，或在映射中明确标为部分覆盖。

### 提交前检查

```bash
# 禁止的条件隐藏写法必须为零。
rg -n -U "coverpoint\\s+1'b1\\s*iff" src/test/python/Frontend/env/funcov/sv

# 确认新文件进入 VCS filelist。
make -nB FRONTEND_SIM=vcs frontend

# 校验 Python 模型与 SV 名称/索引映射。
source /nfs/home/zhaoxinran/.venv/mcpgateway/bin/activate
python -m pytest -p no:rerunfailures src/test/python/Frontend/tests/test_funcov_models.py -q

git diff --check
```

## 查看当前模型

在仓库根目录执行：

```bash
source /nfs/share/unitychip/activate
source /nfs/home/zhaoxinran/.venv/mcpgateway/bin/activate

python - <<'PY'
from src.test.python.Frontend.env.funcov import get_model

model = get_model("address_translation_permission")
print(model.name, len(model.points), len(model.bins))

for bin_def in model.bins:
    print(bin_def.bin_id, "row", bin_def.source_row, bin_def.coverage_group, bin_def.bin_name)
    print("  signals:", bin_def.signals)
    print("  expr:", bin_def.expr)
PY
```

查看未建模原因：

```bash
python - <<'PY'
from src.test.python.Frontend.env.funcov.address_translation_permission import UNMODELED_POINTS

for row, reason in sorted(UNMODELED_POINTS.items()):
    print(row, reason)
PY
```

## 验证

```bash
source /nfs/share/unitychip/activate
source /nfs/home/zhaoxinran/.venv/mcpgateway/bin/activate
python -m pytest -p no:rerunfailures src/test/python/Frontend/tests/test_funcov_models.py -q
```

该测试只验证 Python coverage 索引和文档映射，不会重新编译 frontend。SV bind 是否成功进入仿真，需要手工执行 `make frontend` 后确认。
