# mem_ut 数据持久化后续扩展方案 TODO

## 1. 背景

当前 mem_ut 中部分激励数据是在 SystemVerilog 运行时生成后直接放入数组、主表或 transaction table 中，并在同一次仿真中立即使用。该方式适合一次性随机激励，但不方便复现、长期保存、跨 testcase 复用或 debug 10 万笔级别的请求序列。

后续可扩展一套“主表数据持久化”能力：生成数据后写入文件；后续 testcase 可直接从文件恢复同一组 transaction 数据，再进入原有 admission、issue、writeback、commit flow。

## 2. 推荐保存格式

### 2.1 transaction 主表推荐 TSV

对于 100000 笔请求，推荐使用 TSV 文本格式，而不是 bin。

原因：

- 10 万笔 transaction 的文本体量可接受，通常约 10MB 到 30MB。
- 便于人工 grep、diff、定位指定 `uid`、`robIdx`、`fuOpType`、地址或 replay/redirect 相关字段。
- 字段增删时兼容性比二进制更好。
- 仿真 debug 时可直接查看，不需要额外解析工具。

建议文件名：

```text
main_table_100k.tsv
```

建议一行一个 transaction，一列一个字段：

```text
uid	rob_flag	rob_value	lq_flag	lq_value	sq_flag	sq_value	op_class	fuType	fuOpType	src_0	imm	vaddr	send_pri	send_pri_std	delay
0	0	0	0	0	0	0	INT_LOAD	0x1	0x10	0x80000000	0x0	0x80000000	50	0	0
1	0	1	0	0	0	0	STORE	0x2	0x21	0x80000040	0x8	0x80000048	30	60	2
```

### 2.2 简单 bit/memory 数组可用 HEX/MEM

如果数据只是 `logic [N-1:0] array[]` 或 memory 初始化数据，可使用 `.hex/.mem`：

```text
0a10ff00
0a10ff08
0a10ff10
```

这类格式适合 `$readmemh/$writememh`，但不适合复杂 transaction、class、关联数组或包含多个字段的主表。

### 2.3 BIN 暂不作为默认方案

bin 文件只建议在以下条件同时满足时使用：

- 数据量达到百万到千万级，文本读写成为瓶颈。
- 字段格式长期稳定。
- 不需要人工查看和 diff。
- 有明确的版本号、字段布局和解析工具。

当前 100000 笔 mem_ut transaction 主表仍优先使用 TSV。

## 3. 后续建议 plus 参数

后续实现时建议增加如下 plus 参数：

```text
+MEMBLOCK_MAIN_TABLE_SAVE_FILE=<path>
+MEMBLOCK_MAIN_TABLE_LOAD_FILE=<path>
+MEMBLOCK_MAIN_TABLE_SAVE_EN=0/1
+MEMBLOCK_MAIN_TABLE_LOAD_EN=0/1
+MEMBLOCK_MAIN_TABLE_FILE_STRICT_EN=1
+MEMBLOCK_MAIN_TABLE_FILE_VERSION=<version>
```

含义：

| 参数 | 含义 |
|---|---|
| `MEMBLOCK_MAIN_TABLE_SAVE_FILE` | 随机或手动生成主表后，把主表 transaction 写入该 TSV 文件 |
| `MEMBLOCK_MAIN_TABLE_LOAD_FILE` | testcase 启动时从该 TSV 文件恢复主表 transaction |
| `MEMBLOCK_MAIN_TABLE_SAVE_EN` | 是否启用保存 |
| `MEMBLOCK_MAIN_TABLE_LOAD_EN` | 是否启用加载；启用后不再随机生成主表 |
| `MEMBLOCK_MAIN_TABLE_FILE_STRICT_EN` | 字段缺失、非法枚举、uid 不连续时是否 fatal |
| `MEMBLOCK_MAIN_TABLE_FILE_VERSION` | 文件格式版本，用于后续字段扩展兼容 |

## 4. 预期使用流程

### 4.1 生成并保存

```text
build_random_main_table()
-> common_data_transaction.main_table_by_uid[uid] 写入 transaction
-> save_main_table_to_tsv(path)
-> 后续 flow 继续使用内存中的主表
```

### 4.2 从文件恢复

```text
load_main_table_from_tsv(path)
-> 逐行解析 TSV
-> 重建 main_control_transaction
-> 写入 common_data_transaction.main_table_by_uid[uid]
-> check_main_table_complete()
-> 后续 admission/issue/commit flow 复用原逻辑
```

## 5. 实现注意点

- 文件中必须保存足够重建主表 transaction 的字段，至少包括 `uid`、ROB 信息、op/fu 信息、地址字段、`send_pri`、`send_pri_std`、`delay`。
- `uid` 建议从 0 连续递增，便于 10 万笔性能优化中的 active window 和 success prefix 逻辑使用。
- `robIdx` 仍需符合当前 ROB wrap 规则；不能只保存整数而丢失 flag/value。
- `delay` 保存的是 issue queue 软件延迟，即 `main_tr.delay -> item.ready_cycle`，不是 driver `pre_pkt_gap`。
- 加载后仍需执行现有合法性检查，例如 `validate_main_table_entry()` 或等价检查。
- 如果后续字段增加，应通过 `version` 或表头字段名兼容旧文件。

## 6. 当前结论

100000 笔 mem_ut transaction 主表持久化，后续优先实现 TSV 文本保存/加载：

```text
保存：main_table -> main_table_100k.tsv
加载：main_table_100k.tsv -> main_table
```

bin 格式暂作为远期优化，不作为第一版实现目标。

## 7. 当前测试框架梳理

### 7.1 主表生成入口

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`

当前入口在 `build_main_table()`，行号约为：

- `151-161`：`build_main_table()`
- `163-193`：`build_random_main_table()`
- `195-231`：`import_manual_main_table()`

当前逻辑：

```text
build_main_table:
  如果 data 为空，获取 common_data_transaction 单例。
  如果 MEMBLOCK_USE_MANUAL_MAIN_TABLE=1：
    调 import_manual_main_table()
  否则：
    调 build_random_main_table(seq_csr_common::get_main_trans_num())
```

随机主表逻辑：

```text
build_random_main_table(main_trans_num_i):
  data.reset_all_tables(main_trans_num_i)
  选择 ROB 起点
  选择地址复用窗口
  for idx in [0, main_trans_num_i):
    uid = data.alloc_uid()
    创建 main_control_transaction
    randomize_main_transaction(tr, uid, rob_key)
    基于 recent load/store 窗口做地址复用修正
    data.set_main_transaction(uid, tr)
    push_recent_uid()
    rob_key = rob_advance(rob_key, 1)
  init_status_for_main_table()
  data.check_main_table_complete()
```

手工主表逻辑：

```text
import_manual_main_table:
  manual_num = manual_main_table_by_rob.num()
  data.reset_all_tables(manual_num)
  按 rob_key 排序 manual_main_table_by_rob
  foreach rob_key:
    uid = data.alloc_uid()
    tr.uid = uid
    tr.post_manual_config()
    validate_main_table_entry()
    data.set_main_transaction(uid, tr)
  init_status_for_main_table()
  data.check_main_table_complete()
```

因此持久化最合适的接入点是 `build_main_table()`：

- `load_en=1` 时，在随机/手工生成前从 TSV 恢复主表。
- `save_en=1` 时，在随机/手工/加载完成并 `check_main_table_complete()` 后保存主表。
- 保存和加载只处理“静态主表 transaction”，不保存运行期状态表、issue queue、active map、TLB 表。

### 7.2 公共数据 owner

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`

关键行号：

- `15-31`：主表、状态表、TLB 表、issue queue 字段。
- `101-159`：`reset_all_tables()`，清空所有运行期状态并重新创建 `main_table_by_uid/status_by_uid`。
- `161-174`：`alloc_uid()`，要求 uid 从 0 连续分配。
- `194-204`：`set_main_transaction()`，把主表项写到 `main_table_by_uid[uid]`。
- `2075-2094`：`check_main_table_complete()`，检查 `next_uid == main_trans_num` 且每个 uid 都有主表和状态表。

持久化加载必须复用 `reset_all_tables()` 和 `alloc_uid()`，原因：

- `reset_all_tables()` 会清空上一轮运行期状态、raw monitor queue、active map、issue queue、TLB 表、redirect/flush/flushSb 状态。
- `alloc_uid()` 保证 `next_uid` 与 `main_trans_num` 一致，后续 `check_main_table_complete()` 和 100k success 前缀逻辑依赖这个连续 uid。
- `set_main_transaction()` 会统一写入 `main_table_by_uid`，后续 admission/issue/writeback 都从这里取 transaction。

### 7.3 参数入口

源码位置：

- `mem_ut/ver/ut/memblock/env/plus.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`
- `mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg`

关键行号：

- `plus.sv:24-147`：当前 dispatch plus 参数声明区。
- `plus.sv:152-270`：`reload_from_cmdline()` 解析 plusarg。
- `plus.sv:272-279`：已有 `load_string()`，可以直接复用文件路径参数。
- `seq_csr_common.sv:16-127`：seq 参数快照字段。
- `seq_csr_common.sv:129-140`：`init()`。
- `seq_csr_common.sv:180-267`：`load_from_plus()`。
- `seq_csr_common.sv:270-290`：`validate_and_clamp()` 基础合法性检查。
- `seq_csr_common.sv:850-900` 附近：现有 getter 区。
- `default.cfg:6-104`：默认 cfg 参数。

## 8. 第一版实现目标

第一版只支持主表 `main_control_transaction` TSV 保存/加载：

- 保存对象：`common_data_transaction.main_table_by_uid[uid]` 中的静态字段。
- 加载对象：从 TSV 重建 `main_control_transaction`，写回 `main_table_by_uid`。
- 不保存运行期状态：`status_by_uid`、TLB 表、raw monitor queue、issue queue、active ROB/LQ/SQ map、CSR runtime mirror。
- 不支持 vector LS 新语义；加载出的 transaction 仍需通过当前 `validate_main_table_entry()` 和 `derive_op_behavior()` 支持边界。

第一版推荐文件格式使用数字/hex 字段，避免字符串枚举解析复杂化。

建议 TSV header：

```text
version	uid	op_class	lsq_flow	fuType	fuOpType	src_0	imm	vaddr	rob_flag	rob_value	lq_flag	lq_value	sq_flag	sq_value	numLsElem	tlbAF	tlbPF	tlbGPF	PBMT	pmaAF	corrupt	denied	delay	send_pri	send_pri_std
1	0	1	1	0x000000001	0x010	0x0000000080000000	0x0000000000000000	0x0000000080000000	0	0	0	0	0	0	1	0	0	0	0	0	0	0	0	50	50
```

字段说明：

| 字段 | 用途 |
|---|---|
| `version` | 文件格式版本，第一版固定为 1。 |
| `uid` | 主表静态 uid，必须从 0 连续递增。 |
| `op_class/lsq_flow` | 恢复 op 分类和抽象 LSQ flow。 |
| `fuType/fuOpType` | 恢复 DUT issue/admission 使用的 FU 编码。 |
| `src_0/imm/vaddr` | 恢复地址表达式；加载时重新计算并检查 `vaddr == src_0 + sign_extend_imm12(imm)`。 |
| `rob_flag/rob_value` | 恢复 ROB key，不能只保存 value。 |
| `lq/sq flag/value` | 第一版保存字段，但加载后通常为 0；真实 LSQ admission 后 DUT resp 会覆盖。 |
| `numLsElem` | 标量 load/store 为 1，AMO 为 0；后续 vector 扩展依赖该字段。 |
| `tlbAF/tlbPF/tlbGPF/PBMT/pmaAF/corrupt/denied` | 恢复主表异常/返回属性。 |
| `delay/send_pri/send_pri_std` | 恢复 issue queue 软件延迟和发射优先级。 |

## 9. 需要修改的文件和 coding 方案

### 9.1 `plus.sv` 增加 plus 参数

文件：`mem_ut/ver/ut/memblock/env/plus.sv`

建议修改点：

- 在 `24-31` 行附近 `MEMBLOCK_MAIN_TRANS_NUM` 后增加主表文件参数。
- 在 `160-162` 行附近解析 `MEMBLOCK_MAIN_TRANS_NUM` 后加载新增参数。
- 复用 `272-279` 行已有 `load_string()`。

新增参数：

```systemverilog
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_MAIN_TABLE_SAVE_EN, bit, 1'b0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_MAIN_TABLE_LOAD_EN, bit, 1'b0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_MAIN_TABLE_SAVE_FILE, string, "")
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_MAIN_TABLE_LOAD_FILE, string, "")
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_MAIN_TABLE_FILE_STRICT_EN, bit, 1'b1)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_MAIN_TABLE_FILE_VERSION, int, 1)
```

解析伪代码：

```text
reload_from_cmdline:
  load_int("MEMBLOCK_MAIN_TRANS_NUM", MEMBLOCK_MAIN_TRANS_NUM)
  load_bit("MEMBLOCK_MAIN_TABLE_SAVE_EN", MEMBLOCK_MAIN_TABLE_SAVE_EN)
  load_bit("MEMBLOCK_MAIN_TABLE_LOAD_EN", MEMBLOCK_MAIN_TABLE_LOAD_EN)
  load_string("MEMBLOCK_MAIN_TABLE_SAVE_FILE", MEMBLOCK_MAIN_TABLE_SAVE_FILE)
  load_string("MEMBLOCK_MAIN_TABLE_LOAD_FILE", MEMBLOCK_MAIN_TABLE_LOAD_FILE)
  load_bit("MEMBLOCK_MAIN_TABLE_FILE_STRICT_EN", MEMBLOCK_MAIN_TABLE_FILE_STRICT_EN)
  load_int("MEMBLOCK_MAIN_TABLE_FILE_VERSION", MEMBLOCK_MAIN_TABLE_FILE_VERSION)
```

### 9.2 `seq_csr_common.sv` 增加参数快照和合法性检查

文件：`mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`

建议修改点：

- 在 `16-18` 行附近增加静态字段。
- 在 `180` 行附近 `load_from_plus()` 增加赋值。
- 在 `270-290` 行 `validate_and_clamp()` 增加约束。
- 在 `850-900` getter 区附近增加 getter。

新增字段：

```systemverilog
static bit          main_table_save_en = 1'b0;
static bit          main_table_load_en = 1'b0;
static string       main_table_save_file = "";
static string       main_table_load_file = "";
static bit          main_table_file_strict_en = 1'b1;
static int unsigned main_table_file_version = 1;
```

加载伪代码：

```text
load_from_plus:
  main_table_save_en = plus::MEMBLOCK_MAIN_TABLE_SAVE_EN
  main_table_load_en = plus::MEMBLOCK_MAIN_TABLE_LOAD_EN
  main_table_save_file = plus::MEMBLOCK_MAIN_TABLE_SAVE_FILE
  main_table_load_file = plus::MEMBLOCK_MAIN_TABLE_LOAD_FILE
  main_table_file_strict_en = plus::MEMBLOCK_MAIN_TABLE_FILE_STRICT_EN
  main_table_file_version = get_non_negative_int("MEMBLOCK_MAIN_TABLE_FILE_VERSION", plus::MEMBLOCK_MAIN_TABLE_FILE_VERSION)
```

合法性检查伪代码：

```text
validate_and_clamp:
  如果 main_table_save_en=1 且 save_file 为空：
    fatal，提示必须指定 MEMBLOCK_MAIN_TABLE_SAVE_FILE。
  如果 main_table_load_en=1 且 load_file 为空：
    fatal，提示必须指定 MEMBLOCK_MAIN_TABLE_LOAD_FILE。
  如果 main_table_load_en=1 且 use_manual_main_table=1：
    fatal 或 warning+忽略 manual。
    第一版建议 fatal，避免两个主表来源同时生效。
  如果 main_table_file_version != 1：
    strict=1 时 fatal；
    strict=0 时 warning，但第一版仍只按 version 1 解析。
```

新增 getter：

```text
get_main_table_save_en()
get_main_table_load_en()
get_main_table_save_file()
get_main_table_load_file()
get_main_table_file_strict_en()
get_main_table_file_version()
```

### 9.3 `main_control_transaction.sv` 可选增加格式化 helper

文件：`mem_ut/ver/ut/memblock/seq/base_seq/main_control_transaction.sv`

当前字段定义在 `11-38` 行，合法性在 `93-111` 行。

第一版可以不新增方法，直接由 base sequence 读写字段。但为了让持久化逻辑更清晰，建议增加两个轻量 helper：

```systemverilog
function string to_main_table_tsv_line(input int unsigned version);
function bit from_main_table_fields(...);
```

如果担心函数参数太长，可以不做 `from_main_table_fields()`，只做 `to_main_table_tsv_line()`；解析逻辑放在 `memblock_dispatch_base_sequence` 中。

推荐第一版最小实现：不改该文件，只在 `memblock_dispatch_base_sequence` 中集中处理保存/加载。这样改动文件少，风险低。

### 9.4 `memblock_dispatch_base_sequence.sv` 增加保存/加载主流程

文件：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`

建议修改点：

- 在类声明 `30-32` 行附近增加 task/function 声明。
- 修改 `151-161` 行 `build_main_table()`。
- 在 `195-231` 行 `import_manual_main_table()` 后或文件末尾增加 TSV helper 实现。

新增声明：

```systemverilog
extern virtual task load_main_table_from_tsv(input string path);
extern virtual task save_main_table_to_tsv(input string path);
extern virtual function bit parse_main_table_tsv_line(input string line,
                                                      output main_control_transaction tr,
                                                      output memblock_uid_t uid,
                                                      output int unsigned version);
extern virtual function string main_table_tsv_header();
extern virtual function string main_table_tsv_line(input main_control_transaction tr,
                                                   input int unsigned version);
```

`build_main_table()` 修改后伪代码：

```text
build_main_table:
  如果 data 为空，获取 common_data_transaction。

  如果 seq_csr_common::get_main_table_load_en():
    load_main_table_from_tsv(seq_csr_common::get_main_table_load_file())
  否则如果 seq_csr_common::get_use_manual_main_table():
    import_manual_main_table()
  否则:
    build_random_main_table(seq_csr_common::get_main_trans_num())

  如果 seq_csr_common::get_main_table_save_en():
    save_main_table_to_tsv(seq_csr_common::get_main_table_save_file())
```

为什么保存放在最后：

- 随机模式下保存的是地址复用、send_pri、delay 等修正后的最终主表。
- 手工模式下保存的是按 uid 重排、合法性检查后的最终主表。
- 加载模式下也允许另存一份，用于格式归一化或 debug。

`save_main_table_to_tsv()` 伪代码：

```text
save_main_table_to_tsv(path):
  fd = $fopen(path, "w")
  如果 fd == 0：fatal
  写 header
  for uid in [0, data.main_trans_num):
    tr = data.get_main_transaction(uid)
    validate_main_table_entry(tr, "save uid")
    写 main_table_tsv_line(tr, version)
  $fclose(fd)
  uvm_info 打印保存路径和条数
```

`main_table_tsv_line()` 伪代码：

```text
main_table_tsv_line(tr, version):
  返回 $sformatf("%0d\t%0d\t%0d\t0x%09h\t0x%03h\t0x%016h\t...",
    version,
    tr.uid,
    tr.op_class,
    tr.lsq_flow,
    tr.fuType,
    tr.fuOpType,
    tr.src_0,
    tr.imm,
    tr.vaddr,
    tr.robIdx_flag,
    tr.robIdx_value,
    tr.lqIdx_flag,
    tr.lqIdx_value,
    tr.sqIdx_flag,
    tr.sqIdx_value,
    tr.numLsElem,
    tr.tlbAF,
    tr.tlbPF,
    tr.tlbGPF,
    tr.PBMT,
    tr.pmaAF,
    tr.corrupt,
    tr.denied,
    tr.delay,
    tr.send_pri,
    tr.send_pri_std)
```

`load_main_table_from_tsv()` 推荐两遍读文件，避免加载前不知道数组大小：

```text
load_main_table_from_tsv(path):
  count = count_valid_tsv_rows(path)
  如果 count == 0：fatal
  data.reset_all_tables(count)

  fd = $fopen(path, "r")
  读取每一行：
    跳过空行
    跳过 header 行：如果 line 开头是 "version" 则 continue
    parse_main_table_tsv_line(line, tr, uid, version)
    如果 version != seq_csr_common::get_main_table_file_version():
      strict=1 fatal，否则 warning
    expected_uid = data.alloc_uid()
    如果 uid != expected_uid：
      strict=1 fatal
      strict=0 时也不建议重排，第一版直接 fatal，保证 uid 连续
    tr.uid = uid
    tr.post_manual_config(1'b1)
    validate_main_table_entry(tr, $sformatf("load uid=%0d", uid))
    data.set_main_transaction(uid, tr)
  $fclose(fd)

  init_status_for_main_table()
  data.check_main_table_complete()
  uvm_info 打印加载路径和条数
```

`parse_main_table_tsv_line()` 伪代码：

```text
parse_main_table_tsv_line(line, tr, uid, version):
  创建 tr
  用 $sscanf 解析固定字段：
    "%0d\t%0d\t%0d\t%0d\t%h\t%h\t%h\t%h\t%h\t%0d\t%0d\t..."
  如果解析字段数 != 26：
    strict=1 fatal
    strict=0 return 0 并 warning
  将 int 转 enum：
    tr.op_class = memblock_op_class_e'(op_class_i)
    tr.lsq_flow = memblock_lsq_flow_e'(lsq_flow_i)
  填充 tr 所有字段
  tr.update_vaddr()
  如果文件 vaddr != tr.vaddr：
    strict=1 fatal
    strict=0 warning 并以重新计算后的 tr.vaddr 为准
  返回 1
```

注意：SystemVerilog 对 enum cast 不会自动检查取值是否是已定义枚举值，所以必须在 `validate_main_table_entry()` 中补充 op_class/lsq_flow/fuType/fuOpType 合法性检查，或者在 parse 后直接调用现有 `derive_op_behavior()` 做边界确认。

### 9.5 `validate_main_table_entry()` 补强文件加载合法性

文件：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`

该函数声明在 `100-101` 行，当前实现位于文件后段。第一版需要确保加载文件不能绕过现有支持边界。

建议补强伪代码：

```text
validate_main_table_entry(tr, caller):
  如果 tr == null：fatal
  tr.post_manual_config 或 tr.update_vaddr 后：
    检查 vaddr == src_0 + sign_extend_imm12(imm)
    检查 send_pri/send_pri_std <= 100
    检查 robIdx_value < MEMBLOCK_ROB_SIZE
    检查 lqIdx_value < MEMBLOCK_LQ_SIZE，如果 lqIdx 有效
    检查 sqIdx_value < MEMBLOCK_SQ_SIZE，如果 sqIdx 有效
    调 derive_op_behavior(tr)
      如果 vector/非法 fuType/fuOpType，会按当前模型 fatal
    检查 tr.lsq_flow 与 behavior 一致：
      load/prefetch -> LOAD
      store/CBO -> STORE
      atomic -> ATOMIC
    检查 numLsElem 与 behavior.num_ls_elem 一致
```

这一步的意义：

- 防止 TSV 人工编辑出当前框架不支持的 vector LS。
- 防止 `op_class` 与 `fuType/fuOpType` 不一致。
- 防止加载文件破坏 ROB/LQ/SQ 指针范围。

### 9.6 `default.cfg` 增加默认参数

文件：`mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg`

建议在 `+MEMBLOCK_MAIN_TRANS_NUM=100` 后增加：

```text
+MEMBLOCK_MAIN_TABLE_SAVE_EN=0
+MEMBLOCK_MAIN_TABLE_LOAD_EN=0
+MEMBLOCK_MAIN_TABLE_SAVE_FILE=main_table.tsv
+MEMBLOCK_MAIN_TABLE_LOAD_FILE=main_table.tsv
+MEMBLOCK_MAIN_TABLE_FILE_STRICT_EN=1
+MEMBLOCK_MAIN_TABLE_FILE_VERSION=1
```

默认关闭 save/load，避免影响现有 testcase。

### 9.7 可选新增 testcase cfg

建议新增两个 cfg，便于 smoke 验证：

```text
mem_ut/ver/ut/memblock/seq/plus_cfg/tc_dispatch_real_save_main_table.cfg
mem_ut/ver/ut/memblock/seq/plus_cfg/tc_dispatch_real_load_main_table.cfg
```

保存 cfg 示例：

```text
+MEMBLOCK_MAIN_TRANS_NUM=100
+MEMBLOCK_MAIN_TABLE_SAVE_EN=1
+MEMBLOCK_MAIN_TABLE_SAVE_FILE=main_table_save.tsv
+MEMBLOCK_MAIN_TABLE_LOAD_EN=0
```

加载 cfg 示例：

```text
+MEMBLOCK_MAIN_TABLE_LOAD_EN=1
+MEMBLOCK_MAIN_TABLE_LOAD_FILE=main_table_save.tsv
+MEMBLOCK_MAIN_TABLE_SAVE_EN=0
```

是否需要把 cfg 加入 filelist：不需要，当前 cfg 通过 make/cfg 参数指定，不进入 SV filelist。

## 10. 推荐编码顺序

### Step 1：参数通路

修改文件：

- `mem_ut/ver/ut/memblock/env/plus.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`
- `mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg`

验收点：

```text
+MEMBLOCK_MAIN_TABLE_SAVE_EN=1
+MEMBLOCK_MAIN_TABLE_SAVE_FILE=xxx.tsv
```

在仿真 log 中能看到 plus 被解析，`seq_csr_common` getter 能返回正确值。

### Step 2：保存路径

修改文件：

- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`

先只实现 `save_main_table_to_tsv()`，不实现 load。

验收点：

```text
随机 build_main_table 完成
-> data.check_main_table_complete()
-> save_main_table_to_tsv()
-> 生成 TSV
-> TSV 行数 = main_trans_num + header
```

### Step 3：加载路径

修改文件：

- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`

实现 `load_main_table_from_tsv()` 和 `parse_main_table_tsv_line()`。

验收点：

```text
load_main_table_from_tsv:
  count TSV rows
  data.reset_all_tables(count)
  按 uid 连续加载
  init_status_for_main_table()
  data.check_main_table_complete()
```

### Step 4：保存-加载一致性验证

推荐手动验证流程：

```text
第一次运行：
  +MEMBLOCK_MAIN_TABLE_SAVE_EN=1
  +MEMBLOCK_MAIN_TABLE_SAVE_FILE=/tmp/mem_ut_main_table.tsv

第二次运行：
  +MEMBLOCK_MAIN_TABLE_LOAD_EN=1
  +MEMBLOCK_MAIN_TABLE_LOAD_FILE=/tmp/mem_ut_main_table.tsv
```

检查点：

- 第二次运行不调用随机主表生成。
- 两次主表 `uid/rob/fuType/fuOpType/src_0/imm/vaddr/send_pri/delay` 一致。
- LSQ admission、issue、writeback、commit flow 仍走原逻辑。

## 11. 风险点和处理规则

### 11.1 文件字段版本兼容

第一版只支持 `version=1`。字段增删时必须升级 `MEMBLOCK_MAIN_TABLE_FILE_VERSION`，并在 parser 中保留旧版本兼容逻辑。

### 11.2 加载后是否重新随机 TLB 表

第一版主表持久化只恢复主表。TLB 表仍由 `tlb_map_builder` 根据主表和实时 CSR runtime 重新生成。这样更符合当前“CSR 运行时值必须来自 monitor/runtime state”的规则，避免把旧仿真的 CSR 上下文固化进主表文件。

### 11.3 lqIdx/sqIdx 文件字段的处理

TSV 保存 `lqIdx/sqIdx/numLsElem`，但第一版加载后进入真实 LSQ admission 时：

- `lqIdx/sqIdx` 初始可为 0。
- DUT admission response 会通过现有 flow 写回最终 LQ/SQ idx。
- 如果文件中保存的是已 admission 后的 idx，第一版仍建议在 load 时清零或只作为 debug 字段，不直接跳过 LSQ admission。

推荐第一版策略：

```text
加载主表时保留文件中的 lq/sq 字段；
但 validate 时不要求其非零；
真实 admission 后由 confirm_lsq_candidates()/set_main_transaction() 覆盖。
```

### 11.4 uid 必须连续

当前 100k 性能优化依赖 `success_prefix_uid`、`max_enqueued_uid` 和连续 uid。第一版必须要求 TSV uid 从 0 连续到 N-1。

### 11.5 当前无关工作树状态

制定或执行本方案前需要注意当前工作区可能存在其它任务残留修改。执行 coding 前建议先：

```text
git status --short
```

只提交本功能涉及文件，避免把无关 review 文档删除或仿真生成目录一起提交。
