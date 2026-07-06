# mem_ut 双层内存访问模型执行计划

## 1. 目标

本计划用于指导后续在 mem_ut UVM 环境中实现 `mem_base_sequence.sv`，其中公共基类命名为 `mem_access_base_sequence`。sequence 目录规则遵循 `mem_ut/ver/ut/memblock/rule/memblock_sequence_add_rule.md`：base sequence 放在 `mem_ut/ver/ut/memblock/seq/base_seq`，其他 virtual/scenario/responder sequence 放在 `mem_ut/ver/ut/memblock/seq/virtual_sequence`。

目标是在 sequence 层提供两套关联内存模型：

1. 主内存：公共服务型内存，按公共地址 transaction 的合法范围管理数据。
2. 程序访问专用小内存：只服务程序 load/store 访问，记录程序 store 写过的 byte，并在 load 时与主内存数据合成。

两个内存分别通过两个 task 访问。小内存 task 内部经过 byte-valid 判断后，会调用主内存 task 获取未命中的数据。

主内存还需要服务 DUT dcache 和 sbuffer 的 TileLink 类访问。后续通过 `dcache_mem__access_base_sequence` 和 `sbuffer_mem_access_base_sequence` 两个派生 sequence 分别对接 dcache/sbuffer agent 的 driver 与 monitor：monitor 采集 DUT 发出的请求，sequence 根据请求调用主内存 task，driver 再把响应数据反馈给 DUT。

## 2. 存储结构选择

后续实现选择 SystemVerilog associative array，不采用链表。

选择依据：

1. memblock 地址空间大且访问稀疏，associative array 可以按 line address 懒分配，避免为未访问地址占用空间。
2. 随机地址查找、读写更新是主要操作，associative array 按 key 查询更直接，通常优于链表逐节点遍历。
3. 链表适合顺序遍历和频繁插入删除节点，但本模型主要是地址索引访问，链表查找复杂度会随已访问 line 数量增长。

因此主内存和小内存均使用 associative array，以 line address 为 key。

## 3. 主内存模型

主内存按 1KB 为一个存储 line。

地址拆分规则：

```systemverilog
line_addr   = addr[47:10];
byte_offset = addr[9:0];
```

其中：

1. 1KB = 1024 byte = 8192 bit。
2. 每个主内存 line 保存 `data[8191:0]`。
3. 地址不要求从 0 开始，合法地址范围来自公共地址 transaction 约束。

初始化规则：

1. 主内存支持显式初始化合法地址范围，也支持 lazy init。
2. 当访问一个未初始化 line 时，主内存 task 负责生成该 line 的随机初值并保存。
3. 随机初值只要求稳定可复现，不增加额外强约束。
4. 后续如果找到真实 `public_env_csr_transaction` 类型，应新增 adapter 将其中的地址范围转换为主内存初始化参数。

访问规则：

1. 支持 byte 粒度读取。
2. 支持 byte mask 写入。
3. 跨 1KB line 的访问需要拆成多个 line 内访问。
4. 主内存 task 不承担时序 delay，访问 task 只处理数据、mask、range、`corrupt/denied`。
5. dcache/sbuffer 响应延迟由对应 sequence 设置 transaction 的 `pre_pkt_gap/post_pkt_gap`，再由 agent driver 消费。

## 4. 程序访问专用小内存模型

小内存只记录程序 store 写过的数据，不直接替代主内存。

默认实现也按 1KB line 与主内存对齐：

1. 每个小内存 line 保存 `data[8191:0]`。
2. 每个小内存 line 保存 `byte_valid[1023:0]`。
3. 只有发生 store 的 line 才懒分配。

该方案可以减少主内存和小内存之间的地址换算、跨粒度 merge 和边界处理复杂度。由于小内存只在 store 后分配，预计不会明显影响仿真速度和内存占用。

后续优化规则：

1. 如果回归中发现小内存占用明显偏高，再将小内存 line 粒度优化为 128B 或 256B。
2. 优化时保持小内存 task 对外接口不变，只调整内部 key 和 offset 计算。
3. 主内存仍保持 1KB line，不随小内存优化改变。

小内存首次访问状态：

1. 初始为空，没有任何有效 byte。
2. load 不更新小内存。
3. store 才更新小内存的 `data` 和 `byte_valid`。

## 5. Task 封装方案

### 5.1 主内存 task

主内存访问封装为 `main_mem_access_task`。

职责：

1. 处理主内存 line lazy init。
2. 根据 byte mask 读写主内存。
3. 处理跨 1KB line 访问。
4. 生成访问结果的 `corrupt` 和 `denied` 输出。
5. 不执行访问前后 delay。

接口计划：

```systemverilog
task main_mem_access_task(
    input  bit [47:0] addr,
    input  bit        is_store,
    input  bit [1023:0] byte_mask,
    input  bit [8191:0] store_data,
    output bit        corrupt,
    output bit        denied,
    output bit [8191:0] load_data
);
```

delay 规则：

1. `main_mem_access_task` 不提供 `pre_delay_cycle/post_delay_cycle` 输入。
2. 不在 `mem_access_base_sequence` 中实现 `wait_cycles()`。
3. dcache/sbuffer 访问延迟统一通过响应 transaction 的 `pre_pkt_gap/post_pkt_gap` 控制。
4. 程序专用小内存路径固定 0 延时。

### 5.2 小内存 task

程序访问专用小内存封装为 `prog_mem_access_task`。

职责：

1. 处理程序 load/store 访问。
2. 管理小内存 `byte_valid`。
3. load 时按 byte 判断小内存命中情况。
4. 对小内存未命中的 byte，调用 `main_mem_access_task` 获取主内存数据。
5. store 时只更新小内存，不更新主内存。
6. 输出 `corrupt` 和 `denied`，其结果来自内部主内存访问判定。

接口计划：

```systemverilog
task prog_mem_access_task(
    input  bit [47:0] addr,
    input  bit        is_store,
    input  bit [1023:0] byte_mask,
    input  bit [8191:0] store_data,
    output bit        corrupt,
    output bit        denied,
    output bit [8191:0] load_data
);
```

小内存 delay 规则：

1. 小内存 task 自身不提供 delay 输入。
2. 小内存内部访问固定为 0 延时。
3. 小内存 task 内部调用 `main_mem_access_task` 时不传任何 delay 参数。
4. 因此程序私有小内存访问不会额外扰动时序。

### 5.3 dcache/sbuffer 访问 task

`dcache_mem__access_base_sequence` 和 `sbuffer_mem_access_base_sequence` 都需要继承 `mem_access_base_sequence`。其中 `mem_access_base_sequence` 放在 `mem_ut/ver/ut/memblock/seq/base_seq/mem_base_sequence.sv` 中，负责持有主内存、小内存、主 mem 初始化函数以及 `main_mem_access_task`、`prog_mem_access_task`。

两个派生 sequence 分别封装和 DUT agent 接口位宽一致的访问 task：

1. `dcache_mem__access_base_sequence` 封装 dcache 访问 task，接口宽度对齐 `dcache_agent_agent_xaction`：
   - 请求地址：`auto_inner_dcache_client_out_a_bits_address[47:0]`
   - 请求 size：`auto_inner_dcache_client_out_a_bits_size[2:0]`
   - 请求 mask：`auto_inner_dcache_client_out_a_bits_mask[31:0]`
   - 请求/响应 data：`auto_inner_dcache_client_out_a_bits_data[255:0]`、`auto_inner_dcache_client_out_d_bits_data[255:0]`
   - 错误控制：`auto_inner_dcache_client_out_a_bits_corrupt`、`auto_inner_dcache_client_out_d_bits_denied`、`auto_inner_dcache_client_out_d_bits_corrupt`
2. `sbuffer_mem_access_base_sequence` 封装 sbuffer 访问 task，接口宽度对齐 `sbuffer_agent_agent_xaction`：
   - 请求地址：`auto_inner_buffers_out_a_bits_address[47:0]`
   - 请求 size：`auto_inner_buffers_out_a_bits_size[2:0]`
   - 请求 mask：`auto_inner_buffers_out_a_bits_mask[7:0]`
   - 请求/响应 data：`auto_inner_buffers_out_a_bits_data[63:0]`、`auto_inner_buffers_out_d_bits_data[63:0]`
   - 错误控制：`auto_inner_buffers_out_a_bits_corrupt`、`auto_inner_buffers_out_d_bits_denied`、`auto_inner_buffers_out_d_bits_corrupt`

派生 task 的职责：

1. 从 agent monitor 获取 DUT 发出的 A channel 请求。
2. 根据 opcode/size/mask 判断读写方向和有效 byte 数。
3. 将 dcache 的 256bit/32B 或 sbuffer 的 64bit/8B 访问转换成主内存 `main_mem_access_task` 使用的 8192bit/1024B line 内访问。
4. 对跨 1KB 主内存 line 的请求拆分成多次主内存 task 调用。
5. load 类请求从主内存取数后，裁剪并对齐成 dcache/sbuffer 响应 data 位宽。
6. store 类请求将 DUT 请求 data/mask 扩展并对齐到主内存 byte mask 后调用主内存 task 写入。
7. 生成 D channel 响应 xaction，由对应 agent driver 反馈给 DUT。

派生 task 调用主内存时不传 delay 参数。dcache/sbuffer sequence 需要在生成响应 xaction 时设置 `pre_pkt_gap/post_pkt_gap`，由 driver 在协议层完成 delay 控制；程序小内存路径仍固定 0 延时。

## 6. 主内存与小内存联动规则

### 6.1 load

程序 load 统一调用 `prog_mem_access_task`。

处理流程：

1. 先将 `corrupt` 和 `denied` 默认置 0。
2. 按访问地址和 byte mask 定位小内存 line。
3. 对每个请求 byte 判断 `byte_valid`。
4. 小内存有效 byte 从小内存 `data` 返回。
5. 小内存无效 byte 调用 `main_mem_access_task` 从主内存返回，并接收主内存输出的 `corrupt/denied`。
6. 最终按 byte 合成 `load_data`。
7. 如果主内存返回 `corrupt || denied`，`load_data` 输出全 0。
8. load 不更新小内存。

### 6.2 store

程序 store 统一调用 `prog_mem_access_task`。

处理流程：

1. 先将 `corrupt` 和 `denied` 默认置 0。
2. 如果访问的小内存 line 不存在，创建该 line，`byte_valid` 初值为 0。
3. 根据 `byte_mask` 将有效 byte 写入小内存 `data`。
4. 将对应 `byte_valid` 置 1。
5. store 不更新主内存。
6. 如后续需要 store 前读取未覆盖 byte 的基础值，由小内存 task 内部以 0 delay 调用 `main_mem_access_task` 完成。
7. 如果内部主内存访问返回 `corrupt || denied`，小内存不更新，并向外输出相同的 `corrupt/denied`。

### 6.3 主内存直接访问

非程序私有路径如果需要直接访问公共内存，可以调用 `main_mem_access_task`。

规则：

1. 主内存直接 load 只读主内存，不看小内存。
2. 主内存直接 store 只更新主内存，不更新小内存。
3. 是否允许测试用例直接调用主内存 task，由后续 `mem_base_sequence.sv` 接入时统一约束。

### 6.4 dcache/sbuffer DUT 访问

dcache/sbuffer DUT 访问不经过程序专用小内存，直接访问主内存。

处理流程：

1. dcache/sbuffer agent monitor 采集 DUT 的 A channel 请求。
2. 对应 memory access sequence 从 monitor 或 agent 内部请求队列取得请求。
3. sequence 调用本接口同宽 task 完成协议位宽、mask 和地址 offset 转换。
4. 同宽 task 调用 `main_mem_access_task` 读写主内存。
5. sequence 根据主内存返回值填充 D channel 响应字段。
6. dcache/sbuffer agent driver 将响应驱动回 DUT。

dcache/sbuffer 的请求和响应必须保持 source、size、denied、corrupt、data 之间的关联。读响应 data 来自主内存；写响应不返回有效 data，只确认访问结果和错误状态。

## 7. corrupt 和 denied 规则

`corrupt` 和 `denied` 是内存访问 task 的输出结果，不再作为 `main_mem_access_task` 或 `prog_mem_access_task` 的输入。task 入口默认将二者置 0，访问过程中由主内存模型或后续预留错误判定逻辑置位。

`corrupt` 或 `denied` 任意一个输出为 1 时，访问视为失败。

写操作：

1. 主内存不更新。
2. 小内存不更新。
3. `byte_valid` 不变化。

读操作：

1. 返回全 0。
2. 不更新主内存以外的任何状态。
3. 不使用随机乱码，保证回归结果稳定可复现。

该规则同时适用于 `main_mem_access_task` 和 `prog_mem_access_task`。`prog_mem_access_task` 的 `corrupt/denied` 输出由其内部调用 `main_mem_access_task` 的结果决定。

## 8. 地址范围和约束接入

主内存合法地址范围需要来自公共地址 transaction 的约束。

执行计划：

1. `mem_access_base_sequence` 中提供单一主 mem 初始化函数，参数为 base 和 capacity，可通过多次调用追加多个 range。
2. 主内存访问时检查地址是否落在合法范围内。
3. 不合法地址默认报 `uvm_error`，并返回全 0。
4. 后续定位到真实 `public_env_csr_transaction` 后，新增 adapter 将 transaction 约束转成初始化 task 参数。

建议初始化接口：

```systemverilog
function void init_main_mem_range(
    input bit [47:0] base_addr,
    input longint unsigned capacity
);
```

`capacity == 0` 表示配置了空 range，此时所有访问都会因为没有命中合法范围而被 deny。若公共地址 transaction 支持多段范围，调用方按每段 base+capacity 多次调用 `init_main_mem_range`，不改变两个访问 task 的接口。

## 9. plus 参数化接入

`mem_access_base_sequence` 和继承它的 dcache/sbuffer responder sequence 可以从 `plus.sv` 读取场景参数，并在 `new()` 或 `pre_body()` 中转换成类内控制变量。

适合纳入 plus 控制的字段：

1. 主内存默认 range/capacity 选择。
2. lazy init seed 或数据模板选择。
3. dcache/sbuffer responder 的默认 `pre_pkt_gap/post_pkt_gap`。
4. 默认 denied/corrupt 注入使能和比例。
5. 默认 outstanding 深度或 backpressure 策略。

新增字段时必须同步：

```text
mem_ut/ver/ut/memblock/env/plus.sv
mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg
mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md
```

默认 cfg 中使能类参数保持 0。新增或删除 `plus.sv` 字段需要重新编译；只修改 `seq/plus_cfg/*.cfg` 或 runtime `plus_arg` 时在仿真运行阶段生效。

## 10. 后续落地步骤

1. 新增 `mem_ut/ver/ut/memblock/seq/base_seq/mem_base_sequence.sv`。
2. 在其中定义 `mem_access_base_sequence`、主内存 associative array、小内存 associative array 和公共 typedef。
3. 实现 `init_main_mem_range(base, capacity)`、`main_mem_access_task`、`prog_mem_access_task`。
4. 新增 `dcache_mem__access_base_sequence`，继承 `mem_access_base_sequence`，放在 `seq/virtual_sequence`，对接 dcache agent monitor/driver，并封装 256bit/32B dcache 同宽访问 task。
5. 新增 `sbuffer_mem_access_base_sequence`，继承 `mem_access_base_sequence`，放在 `seq/virtual_sequence`，对接 sbuffer agent monitor/driver，并封装 64bit/8B sbuffer 同宽访问 task。
6. 将 `seq/base_seq`、`seq/virtual_sequence` 加入 `tc/tc.f` include path，并在 `tc_pkg.sv` 中按依赖顺序 include。
7. 需要程序访问内存的 testcase sequence 继承或持有该 base sequence。
8. 将 dcache、sbuffer、TL response 等需要内存数据的 sequence 逐步改为通过 task 访问内存模型。
9. 如果小内存 1KB line 带来明显内存压力，再按本计划优化为更小 line 粒度。
10. 所有 sequence 目录、参数或接口变化必须同步 `memblock_sequence_add_rule.md` 中列出的 plan 文档。

## 11. 验证场景

后续 SV 实现完成后，至少覆盖以下场景：

1. 主内存首次 load 触发 lazy init，后续同地址 load 返回稳定数据。
2. 主内存 byte mask store 后，对应 byte 更新，未 mask byte 不变。
3. 小内存初始为空，load 从主内存返回。
4. 小内存 partial store 后，再 load 同地址，store byte 来自小内存，其他 byte 来自主内存。
5. 小内存跨 1KB line 的 load/store 能正确拆分和合成。
6. `corrupt=1` store 不更新主内存和小内存。
7. `denied=1` store 不更新主内存和小内存。
8. `corrupt=1` 或 `denied=1` load 返回全 0。
9. 小内存 task 调用主内存 task 时不引入 delay。
10. dcache/sbuffer sequence 设置 `pre_pkt_gap/post_pkt_gap` 后，driver 能按 transaction gap 控制响应时序。
11. dcache 256bit/32B 读请求能从主内存返回正确对齐的数据。
12. dcache 256bit/32B 带 mask 写请求能正确更新主内存对应 byte。
13. sbuffer 64bit/8B 读请求能从主内存返回正确对齐的数据。
14. sbuffer 64bit/8B 带 mask 写请求能正确更新主内存对应 byte。
15. dcache/sbuffer 跨 1KB line 请求能正确拆分主内存访问并合成响应。
16. dcache/sbuffer agent monitor 捕获请求、sequence 处理、driver 返回响应的闭环能在基础 testcase 中跑通。
17. `main_mem_access_task` 和 `prog_mem_access_task` 的 `corrupt/denied` 均作为输出参数生效，默认输出为 0。

## 12. 默认假设

1. 本计划中的 1KB 指 1024 byte，不是 1K-bit。
2. 主内存 line 固定为 1KB。
3. 小内存默认也使用 1KB line，但只懒分配 store 命中的 line。
4. 小内存 task 自身无 delay。
5. 主内存 task 和附属内存 task 都不处理 delay。
6. 错误读固定返回全 0。
7. dcache/sbuffer 访问主内存，不访问程序专用小内存。
8. `dcache_mem__access_base_sequence` 按用户指定名称保留双下划线。
9. `main_mem_access_task` 和 `prog_mem_access_task` 中 `corrupt/denied` 是输出参数。
10. dcache/sbuffer 的响应 delay 通过 transaction 的 `pre_pkt_gap/post_pkt_gap` 实现。
11. sequence 新增和迁移遵循 `memblock_sequence_add_rule.md`。

## 13. DCache-L2 TileLink-C 扩展计划入口

`dcache_mem__access_base_sequence` 后续需要从简单 A 到 D 内存响应扩展为完整的 DCache-L2 TileLink-C responder。详细方案见：

```text
AI_DOC/plan/test_framework/plan/undo/dcache_l2_tilelink_interaction_plan_20260614.md
```

该扩展计划覆盖：

1. DCache A channel `AcquireBlock/AcquirePerm/CBO` 的合法 D 响应。
2. L2 主动发 B `Probe` 的字段和约束。
3. DCache C channel `ProbeAck/ProbeAckData/Release/ReleaseData` 的消费和主内存更新规则。
4. D `Grant/GrantData/ReleaseAck/CBOAck` 与 E `GrantAck` 的 outstanding/sink 管理。
5. 可选 `l2_hint` 顶层侧带信号联动。
