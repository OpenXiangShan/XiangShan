# mem_ut V2 DCache L2 Sideband Responder 适配最终 Coding Plan

| 项目 | 内容 |
|---|---|
| 状态 | `undo`，待 coding |
| 目标版本 | V2 |
| 当前分支 | `mem_ut_uvm_v2` |
| V2 接口权威 | `build_memblock/rtl/MemBlock.sv` |
| 测试框架范围 | DCache agent responder 的 `io_l2_hint_*` 与 `io_l2_flush_done` 输入保护 |
| 适配原则 | 当前 main flow 只支持 zero-only sideband，不实现主动 hint 或 L2 flush completion 模型 |
| 创建/修订日期 | 2026-07-15 |

## 1. 范围与边界

本 plan 只整理 V2 MemBlock 下列 4 个 DCache/L2 sideband DUT input 的安全适配方案：

```text
io_l2_hint_valid
io_l2_hint_bits_sourceId[3:0]
io_l2_hint_bits_isKeyword
io_l2_flush_done
```

当前 scalar main flow 不建模主动 L2 hint，也不建模 L2 flush 或低功耗完成。因此这 4 个字段从
time zero 到测试结束都必须保持 known-zero。

本轮修改范围：

| 文件 | 修改内容 |
|---|---|
| `mem_ut/ver/ut/memblock/agent/dcache_agent_agent/src/dcache_agent_agent_interface.sv` | interface 声明显式初始化 0 |
| `mem_ut/ver/ut/memblock/agent/dcache_agent_agent/src/dcache_agent_agent_xaction.sv` | xaction constraint hard-zero，`new()` 显式清 0 |
| `mem_ut/ver/ut/memblock/agent/dcache_agent_agent/src/dcache_agent_agent_driver.sv` | idle 统一驱 0，send 前 fail-fast |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv` | DCache idle/response builder 显式写 0 |

本轮不修改：

- DCache A/B/C/D/E responder 的 opcode、payload、handshake、beat、gap 和 memory access。
- 主表、status/raw、pass/fail/terminal、redirect/replay、commit/deq。
- monitor analysis producer、testcase、plus/cfg、DCache `drv_mode` 配置入口。
- DCache/SBuffer `corrupt/denied` 注入 TODO。

后续如需支持非零 L2 hint，必须另建专项，把 `sourceId` 与 MSHR、GrantData 周期和 keyword beat
绑定；后续如需支持 `l2_flush_done`，也必须另建 L2 flush/低功耗完成模型。本 plan 不允许通过放宽
constraint 或删除 driver fatal 临时启用这些 future 能力。

## 2. 问题一：sideband 默认值和随机值没有 zero-only 合同

### V2 问题

4 个 sideband 字段是 V2 DUT input，并会影响 DCache、LoadUnit、LSQ replay 或 CSR
`L2_FLUSH_DONE`。如果 interface time-zero 为 X，或 generic `uvm_do(req)` 随机出非零值，mem_ut
可能在没有任何 hint/flush 建模来源的情况下改变 DUT 行为。

### 修改原因

当前 main flow 只支持 zero-only sideband。默认值和随机约束必须表达这个 capability，而不能依赖
SystemVerilog 二态隐式初值，也不能让随机 item 构造出未建模的非零 hint/flush。

### 修改方案与修改逻辑

1. 在 `dcache_agent_agent_interface.sv` 中保持字段位置、位宽、clocking block 和 modport 不变，只给
   4 个字段声明增加显式 0 初值。
2. 在 `dcache_agent_agent_xaction.sv` 中保留 4 个 `rand bit` 字段、extern constraint 名称、
   UVM field automation、`psdisplay()` 和 `compare()`，但将 constraint body 改为 hard equality 0。
3. 在 `dcache_agent_agent_xaction::new()` 中，调用父类构造后显式写 4 个字段为 0。
4. 不新增 initial block、reset task、helper、queue、map 或 cfg。

本问题修改后的逻辑是：interface 创建时已经是 known-zero；随机路径不能生成非零；manual create
即使不 randomize 也有显式 0 默认值。manual 后续强行赋非零不在本层静默修正，由 driver 发送边界拒绝。

### 文字伪代码

```text
interface 实例化：
  声明 io_l2_hint_valid = 0；
  声明 io_l2_hint_bits_sourceId = 0；
  声明 io_l2_hint_bits_isKeyword = 0；
  声明 io_l2_flush_done = 0；
  保持 drv_cb output 和 mon_cb input 不变；
  connect 继续从 interface force 到 MemBlock input；

xaction randomize：
  对四个 sideband 字段分别加入 hard constraint == 0；
  如果 inline constraint 要求任一字段非零，randomize 失败；
  不 retry，不 fallback，也不把非零请求改写成 0；

xaction new(name)：
  调用 super.new() 完成原对象构造；
  按 valid、sourceId、isKeyword、flush_done 顺序显式赋 0；
  返回当前对象；
  不读取 cfg、DUT、公共状态或 memory model。
```

## 3. 问题二：builder 和 idle path 可能留下隐式值或跟随 `drv_mode`

### V2 问题

DCache responder builder 当前主要构造 A/B/C/D/E response；sideband 若只依赖构造默认值，不够可审计。
同时 `drive_idle()` 是 generic idle path，若 4 个字段跟随 `DRV_1/DRV_X/DRV_RAND`，idle 拍也可能驱动
非零或 X。

### 修改原因

这 4 个字段不是普通随机填充位，而是有协议语义的 DUT input。当前 zero-only capability 下，所有
idle、gap、reset、response builder 产生的 item 都必须明确写 0，不能受 DCache TL 通道的 generic
`drv_mode` 影响。

### 修改方案与修改逻辑

1. `dcache_mem__access_base_sequence::build_dcache_idle_xaction()` 保留现有 A/B/C/D/E idle 模板，
   在同一函数内显式写 4 个 sideband 字段为 0。
2. `dcache_mem__access_base_sequence::dcache_mem_access_xaction()` 在 `rsp_xact` create 后立即写
   4 个 sideband 字段为 0，然后继续执行原 memory/opcode/data/gap 构造。
3. `dcache_agent_agent_driver::drive_idle()` 保留每个 `drv_mode` 分支内原 A/B/C/D/E 赋值，删除分支中
   对 4 个 sideband 字段的赋值，在分支链结束后无条件驱 4 个 0。
4. 不强制整个 DCache agent 改成 `DRV_0`，不修改 cfg，也不新增未知 mode fatal。

本问题修改后的逻辑是：DCache TL responder 原行为不变；sideband 在 builder、reset、idle、gap、no-item
路径均被隔离为常量 0。

### 文字伪代码

```text
build_dcache_idle_xaction()：
  创建 dcache idle xaction；
  保留原 a_ready/b_valid/c_ready/d_valid/e_ready idle 模板；
  显式写 io_l2_hint_valid = 0；
  显式写 io_l2_hint_bits_sourceId = 0；
  显式写 io_l2_hint_bits_isKeyword = 0；
  显式写 io_l2_flush_done = 0；
  返回 rsp_xact；
  不访问 memory，不写 status/raw，不新增 helper；

dcache_mem_access_xaction(req)：
  创建 response xaction；
  立即清 4 个 sideband 字段；
  调用原 is_store_opcode：判断 A request 是否为 store；
  调用原 dcache_mem_access_task：根据 request 和 memory model 取得 data/denied/corrupt；
  调用原 dcache_d_opcode：生成 D response opcode；
  保持 source、size、sink、gap 和 beat 逻辑不变；
  不根据 request、beat index 或 memory 结果生成 hint/flush；

drive_idle()：
  根据 cfg.drv_mode 执行原 A/B/C/D/E idle 赋值；
  不在各 mode 分支内驱动 sideband；
  离开 if/else-if 分支链后，无条件把 4 个 sideband 字段驱 0；
  正常返回，不更新公共状态，不扫描任何表或 queue。
```

## 4. 问题三：`send_pkt()` 没有发送前 fail-fast 边界

### V2 问题

hard constraint 和 builder 清 0 只能保护正常构造路径。manual transaction、未来误用或其它 sequence
仍可能在发送前把 4 个字段改成非零。如果 `send_pkt()` 直接透传字段，DUT 会先收到未建模 sideband，
再由后续监控或仿真现象暴露问题，定位成本高。

### 修改原因

当前 zero-only sideband 是接口 capability 基础合法性，不是 debug/二次防御检查。非法 item 必须在第一条
vif 赋值前停止，避免半拍或部分字段已经驱入 DUT。

### 修改方案与修改逻辑

1. `dcache_agent_agent_driver::send_pkt()` 的第一段可执行逻辑检查 4 个 sideband 字段是否全部为 0。
2. 任一字段非零时，使用固定 ID `DCACHE_L2_SIDEBAND_UNSUPPORTED` 触发 `uvm_fatal`，fatal 信息打印
   4 个实际值。
3. 检查通过后，原 A/B/C/D/E payload、gap 和发送顺序不变。
4. 发送 sideband 时不再 `<= tr.io_l2_*` 透传，而是明确驱常量 0。
5. 不 drop、不 fallback、不把非法 transaction 静默改写为 0。

本问题修改后的逻辑是：manual 非零 item 不能进入 DUT；合法 item 的 DCache TL 行为完全保持原样；
sideband 始终从 driver 发送边界被重申为 0。

### 文字伪代码

```text
send_pkt(tr)：
  如果 tr.io_l2_hint_valid != 0，或
     tr.io_l2_hint_bits_sourceId != 0，或
     tr.io_l2_hint_bits_isKeyword != 0，或
     tr.io_l2_flush_done != 0：
       在任何 vif 字段赋值前 uvm_fatal；
       fatal ID 使用 DCACHE_L2_SIDEBAND_UNSUPPORTED；
       日志打印 valid/sourceId/isKeyword/flush_done 的实际值；
       不 drop、不 fallback、不调用 drive_idle 伪装成功；

  如果 4 个字段全为 0：
       按原顺序驱动 A/B/C/D/E 字段；
       保持原 pre/post gap、beat、opcode、data、source、sink 和 item_done ownership；
       对 4 个 sideband 字段明确驱常量 0；
       正常返回；
```

## 5. 修改方案总结

本 plan 修改的是 DCache responder 的 sideband 输入保护逻辑，不修改 DCache responder 主行为。

修改前：

```text
interface 可能在 time-zero 暴露隐式初值；
xaction 随机约束允许完整二态范围；
builder 主要定义 A/B/C/D/E response，sideband 依赖默认值；
drive_idle 可按 generic drv_mode 生成 1/X/random；
send_pkt 直接透传 sideband，manual 非零 item 可进入 DUT。
```

修改后：

```text
interface、random item、manual create、idle builder、response builder、idle driver 和 send_pkt
  全部收敛到 zero-only sideband；
send_pkt 在第一条 vif 赋值前拒绝非零 item；
DCache A/B/C/D/E handshake、memory response、testcase cfg、monitor analysis 和公共状态不变；
future 非零 hint/flush completion 只能由后续专项实现。
```

## 6. 验证方案

静态检查：

```bash
rg -n "io_l2_hint_valid|io_l2_hint_bits_sourceId|io_l2_hint_bits_isKeyword|io_l2_flush_done" \
  build_memblock/rtl/MemBlock.sv \
  mem_ut/ver/ut/memblock/agent/dcache_agent_agent/src \
  mem_ut/ver/ut/memblock/tb/dcache_agent_connect.sv \
  mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv

git diff --check -- \
  mem_ut/ver/ut/memblock/agent/dcache_agent_agent/src/dcache_agent_agent_interface.sv \
  mem_ut/ver/ut/memblock/agent/dcache_agent_agent/src/dcache_agent_agent_xaction.sv \
  mem_ut/ver/ut/memblock/agent/dcache_agent_agent/src/dcache_agent_agent_driver.sv \
  mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv
```

coding 完成后的远端验证入口：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
make eda_run tc=tc_sanity mode=base_fun
make eda_run tc=tc_dispatch_smoke mode=base_fun
make eda_run tc=tc_dispatch_real_smoke mode=base_fun cfg=tc_dispatch_real_smoke
```

验收要求：

- 三组 testcase 均 `TEST CASE PASSED`，且 `UVM_ERROR=0`、`UVM_FATAL=0`。
- 波形或日志能证明首个 driver edge 前四字段已为 known-zero。
- normal flow 不命中 `DCACHE_L2_SIDEBAND_UNSUPPORTED`。
- 负向代码审计确认 manual 非零 item 在任何 vif 赋值前 fatal。

## 7. 风险与未解决项

- 当前 plan 不支持非零 L2 hint，也不支持 L2 flush completion；相关能力必须另建专项。
- DCache/SBuffer `corrupt/denied` 注入仍属于既有 TODO。
- 本 plan 只整理执行方案，不在本文档阶段执行编译或仿真。
