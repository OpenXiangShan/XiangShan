# DUT 缺陷分析文档

## 概述

当测试执行过程中发现某些检查点（Check Point）未能通过时，需要在 `{DUT}_bug_analysis.md` 文档中进行详细的缺陷分析。本文档用于记录和分析测试失败的检查点，评估缺陷的严重程度，并提供根因分析。

## 文档结构

缺陷分析文档包含以下部分：
1. **Bug概述** - 按 <FG-*>, <FC-*>, <CK-*>, <BG-*> 的层级结构标记并列出所有Bug描述，在bug描述下用 <TC-*> 列出所有由该bug导致的Fail测试用例
2. **Bug分析** - 基于源代码，对bug根本原因进行深入分析和归类

**注意：**
- 所有Bug都需要有至少一个Fail的测试用例与其对应
- 没检测通过的检查点（Checkpoint）当bug处理，就没有对应Fail的测试用例，也需要添加一个`assert False`的测试用例用于标记
- <FG-*> 等标签结构为树状结构，同一个父节点下的子节点不能出现同名
- 在 <BG-*-xx> 中给bug命名时，应当取简洁、有意义、可读性强、容易理解的名字
- 在 <TC-*>标签中，测试用例如果是基于Class的，也需要带上类名，例如： <TC-test_example.py::TestMyClassName::test_function_name>
- 在 <TC-*>标签中标记的测试用例必须为 Fail ，只有测试用例没有通过才能证明它触发了bug


### 在进行缺陷根因分析时，需要结合源代码进行分析（{DUT}的源文件通常为{DUT}.v、{DUT}.sv、或者{DUT}.scala），并在文档中把bug相关的部分列出来，用注释说明bug原因，例如：

**Verilog代码bug示例：**
```verilog
// Adder.v 第8-12行，位宽错误导致溢出处理异常
8:   input [WIDTH-1:0] a,
9:   input [WIDTH-1:0] b, 
10:  output [WIDTH-2:0] sum,    // BUG: 应该是 [WIDTH-1:0]，少了1位导致高位截断
11:  output cout
12: );
13: 
14: assign {cout, sum} = a + b + cin;  // 由于sum位宽不足，高位丢失
```

然后同样以源代码的方式给出修复建议：

**Verilog修复示例：**
```verilog
// 修复后的Adder.v 第8-16行
8:   input [WIDTH-1:0] a,
9:   input [WIDTH-1:0] b, 
10:  output [WIDTH-1:0] sum,    // 修复: 恢复正确的位宽定义
11:  output cout
12: );
13: 
14: wire [WIDTH:0] full_result = a + b + cin;  // 使用完整位宽进行计算
15: assign sum = full_result[WIDTH-1:0];       // 取低位作为结果
16: assign cout = full_result[WIDTH];          // 取最高位作为进位输出
```

**注意**： 在给出代码时，需要在第一行的注释中说明是哪个文件，每一行的开头为行号。

## Bug分析格式

### 基本语法规则

- 使用功能组标签 `<FG-*>` 对失败检查点进行分组
- 使用功能点标签 `<FC-*>` 标识具体功能
- 使用检查点标签 `<CK-*>` 标识失败的具体检查点
- 使用Bug标签 `<BG-*-xx>` 标识缺陷名称和置信度（xx取值0-100）
- 使用多个测试用例标签 `<TC-*>` 标识测出bug的所有测试用例，这些测试用例必须为Fail（Fail的测试用例意味着bug）

例如：

```
## 未测试通过检测点分析

<FG-ARITHMETIC>

#### 加法功能 <FC-ADD>
- <CK-BOUNDARY> 边界值处理：当操作数为最大值时，结果计算错误，Bug置信度 85% <BG-MAXBOUNDARY-85>
  - 触发bug的测试用例:
    -  <TC-test_example.py::test_case_1> test_example.py::test_case_1 用例说明
    -  <TC-test_example.py::test_case_2> test_example.py::test_case_2 用例说明
    ...
  - Bug根因分析：
  ...
```

### 置信度评估指南

| 置信度范围 | 含义 | 建议处理方式 |
|-----------|------|-------------|
| 90-100% | 确认存在缺陷 | 立即修复 |
| 70-89% | 很可能存在缺陷 | 优先修复 |
| 50-69% | 可能存在缺陷 | 进一步调查 |
| 20-49% | 不确定是否缺陷 | 低优先级调查 |
| 1-19% | 很可能是测试问题 | 检查测试用例 |
| 0% | 已知忽略点 | 需要说明忽略原因 |

### 完整示例

下列示例演示了一个（虚构的）算术逻辑单元（ALU）在一次回归中发现的缺陷记录方式。示例重点演示：标签层级、置信度书写、一条 Bug 下多测试用例的列出方式、以及临时占位测试用例的用法。

## 未测试通过检测点分析

<FG-ARITHMETIC>

#### 加法功能 <FC-ADD>
- <CK-CIN-OVERFLOW> 带进位溢出处理异常：在最大无符号数 + 1 + cin=1 时未正确拉高溢出标志；Bug 置信度 98% <BG-CIN_OVERFLOW-98>
  - 触发 Bug 的测试用例：
    - <TC-tests/test_adder.py::test_add_with_cin_overflow_boundary> 边界 + 进位溢出
    - <TC-tests/test_adder.py::test_add_with_cin_random> 随机激励下复现（多次）
  - 备注：两条测试均稳定 Fail，波形比对一致，已锁定 RTL 逻辑问题

- <CK-BOUNDARY> 最大值 + 1 结果截断：期望得到进位或正确饱和，但结果被截断；Bug 置信度 85% <BG-ADD_BOUNDARY-85>
  - 触发 Bug 的测试用例：
    - <TC-tests/test_adder.py::test_add_unsigned_max_plus_one>
  - 备注：与 <CK-CIN-OVERFLOW> 共享部分根因（位宽+溢出逻辑）

#### 减法功能 <FC-SUB>
- <CK-BORROW> 借位信号错误：当被减数 < 减数时 borrow 未置位；Bug 置信度 92% <BG-SUB_BORROW-92>
  - 触发 Bug 的测试用例：
    - <TC-tests/test_sub.py::test_sub_basic_borrow>
    - <TC-tests/test_sub.py::test_sub_chain_with_borrow>

- <CK-UNDERFLOW> 下溢标志不稳定：同一输入在不同仿真次序下标志位不一致；Bug 置信度 72% <BG-SUB_UNDERFLOW-72>
  - 触发 Bug 的测试用例：
    - <TC-tests/test_sub.py::test_sub_underflow_flag>
  - 备注：疑似组合逻辑竞争 / 采样时序问题

<FG-LOGIC>

#### 位操作功能 <FC-BITOP>
- <CK-SHL> 左移超范围行为未定义：移位数 >= 宽度时出现 X 或旧值残留；Bug 置信度 88% <BG-SHL_RANGE-88>
  - 触发 Bug 的测试用例：
    - <TC-tests/test_shift.py::test_shl_over_width>
    - <TC-tests/test_shift.py::test_shl_boundary>

- <CK-SHR> 算术右移符号扩展错误：负数高位填充值不正确；Bug 置信度 95% <BG-SHR_SIGNEXT-95>
  - 触发 Bug 的测试用例：
    - <TC-tests/test_shift.py::test_shr_sign_extend>

#### 比较功能 <FC-COMPARE>
- <CK-EQUAL> 罕见输入组合下偶发失配：无法稳定复现，疑似测试激励或未初始化寄存器影响；Bug 置信度 18% <BG-CMP_EQUAL-18>
  - 触发（疑似）测试用例：
    - <TC-tests/test_compare.py::test_equal_random_sweep>
  - 后续计划：添加更高可控度的定向激励并捕获波形

<FG-CONTROL>

#### 分支预测 <FC-BRANCH>
- <CK-MISPREDICT> 特定随机模式下预测准确率低：确认是当前版本有意降级策略；Bug 置信度 0% <BG-BR_PRED_POLICY-0>
  - 占位测试用例（设计已知限制）：
    - <TC-tests/test_branch.py::test_branch_random_policy_guard>  // assert False 标记（未来删除）
  - 说明：作为已知策略限制记录，后续版本若策略升级需重新评估


### 标签与字段书写要点（示例总结）

| 层级 | 示例 | 说明 |
|------|------|------|
| 功能组 FG | <FG-ARITHMETIC> | 顶层功能域，全部大写 |
| 功能点 FC | <FC-ADD> | 具体子功能 |
| 检查点 CK | <CK-CIN-OVERFLOW> | 单一可验证点，短横线分隔 |
| 缺陷 BUG | <BG-CIN_OVERFLOW-98> | 后缀数字=置信度（0-100） |
| 测试用例 TC | <TC-tests/test_adder.py::test_add_with_cin_overflow_boundary> | 路径+函数全称 |

补充规范：
1. 一个 <CK-*> 允许关联多个 <BG-*>
2. 若一个 Bug 影响多个检查点，需在`根因分析`部分统一列出受影响集合
3. 临时占位测试需带有清晰注释，避免长期遗留

## 缺陷根因分析

根因分析部分不使用标签，直接使用路径格式（如 `FG-ARITHMETIC/FC-ADD/CK-CIN-OVERFLOW`）来引用失败的检查点和BUG，不能有`<`或者`>`出现。

### 分析框架

每个缺陷分析应包含：
1. **缺陷描述** - 简明扼要描述问题现象
2. **影响范围** - 列出受影响的检查点 / 关联 Bug 标签
3. **根本原因** - 分析问题的根本原因（需要基于源代码）
4. **修复建议** - 提供具体的修复方案（可附代码差异、伪代码）
5. **验证方法** - 说明如何验证修复效果（新增/复用哪些测试、波形关键观察点）

### 根因分析示例

#### 1. 进位处理缺陷

**缺陷描述：** 加法器在处理带进位输入的溢出场景时，未能正确设置溢出标志位。

**影响范围：**
- FG-ARITHMETIC/FC-ADD/CK-CIN-OVERFLOW （BG-CIN_OVERFLOW-98）
- FG-ARITHMETIC/FC-ADD/CK-BOUNDARY （BG-ADD_BOUNDARY-85）

**根本原因：** 
在RTL设计中，溢出检测逻辑只考虑了两个操作数的加法结果，忽略了进位输入对溢出判断的影响。具体来说，当 `(a + b + cin) > MAX_VALUE` 时，应该设置溢出标志，但当前实现只检查了 `(a + b) > MAX_VALUE`。

**具体代码缺陷：**
```verilog
// Adder.v 第25-30行，溢出检测逻辑错误
25: wire [WIDTH-1:0] sum_temp;
26: wire carry_temp;
27: 
28: assign {carry_temp, sum_temp} = a + b;          // BUG: 未考虑cin
29: assign {cout, sum} = {carry_temp, sum_temp} + cin;
30: assign overflow = carry_temp;                   // BUG: 溢出判断错误
```

**修复建议：**
```verilog
// 正确的实现
wire [WIDTH:0] full_sum = a + b + cin;
assign {cout, sum} = full_sum[WIDTH:0];
assign overflow = full_sum[WIDTH];                  // 正确的溢出检测
```

**验证方法：** 重新执行涉及 CK-CIN-OVERFLOW 的两个测试用例，并添加定向向量：`a = MAX`, `b = 1`, `cin = 1`；波形中重点确认：进位链、sum 高位、overflow 标志；修复后应全部 Pass。

#### 2. 移位操作缺陷

**缺陷描述：** 左移和右移操作在移位位数等于或超过数据位宽时行为不符合预期。

**影响范围：**
- FG-LOGIC/FC-BITOP/CK-SHL （BG-SHL_RANGE-88）
- FG-LOGIC/FC-BITOP/CK-SHR （BG-SHR_SIGNEXT-95）

**根本原因：**
设计中未对移位位数进行有效性检查，当移位位数 >= 数据位宽时，应该有明确的行为定义（如清零或保持原值），但当前实现产生了不确定的结果。

**具体代码缺陷：**
```systemverilog
// Shifter.sv 第67-75行，移位范围检查缺失
67: always_comb begin
68:   case (operation)
69:     SHL: result = data << shift_amount;         // BUG: 未检查shift_amount范围
70:     SHR: result = data >> shift_amount;         // BUG: 可能产生不确定结果
71:     ASR: result = $signed(data) >>> shift_amount; // BUG: 同样的问题
72:   endcase
73: end
```

**修复建议：**
```systemverilog
// 添加移位位数检查
localparam int MAX_SHIFT = $clog2(WIDTH);
wire shift_valid = shift_amount < MAX_SHIFT;

always_comb begin
  case (operation)
    SHL: result = shift_valid ? (data << shift_amount) : '0;
    SHR: result = shift_valid ? (data >> shift_amount) : '0;
    ASR: result = shift_valid ? ($signed(data) >>> shift_amount) : {WIDTH{data[WIDTH-1]}};
  endcase
end
```

**验证方法：** 使用边界移位位数（31, 32, 33 对于32位数据）进行测试，确认结果的一致性。

#### 3. 状态机转换错误

**缺陷描述：** 缓存控制器在同时收到读写请求时进入了错误状态，导致后续操作异常。

**影响范围：**
- FG-CONTROL/FC-CACHE/CK-CONFLICT
- FG-CONTROL/FC-CACHE/CK-STATE-TRANS

**根本原因：**
状态机设计时未考虑读写冲突的异常情况处理，当同时收到读写请求时，应该拒绝操作并返回错误状态，但当前实现选择了其中一个操作继续执行。

**具体代码缺陷：**
```systemverilog
// CacheController.sv 第112-125行，状态转换逻辑错误
112: IDLE: begin
113:   if (read_req && !write_req) begin
114:     current_state <= READ_STATE;
115:   end else if (!read_req && write_req) begin
116:     current_state <= WRITE_STATE;
117:   end else if (read_req && write_req) begin    // BUG: 冲突处理错误
118:     current_state <= READ_STATE;              // 应该进入ERROR_STATE
119:     read_ack <= 1'b1;                         // BUG: 错误地确认读操作
120:   end
121: end
```

**修复建议：**
```systemverilog
// 正确的冲突处理
IDLE: begin
  if (read_req && write_req) begin
    current_state <= ERROR_STATE;
    error_code <= ERR_CONFLICT;
  end else if (read_req) begin
    current_state <= READ_STATE;
  end else if (write_req) begin
    current_state <= WRITE_STATE;
  end
end
```

**验证方法：** 构造同时发起读写请求的测试场景，验证错误状态和错误码的正确设置。

#### 4. Chisel 流水线缺陷

**缺陷描述：** ALU流水线在处理数据冒险时出现计算错误，特别是连续相关操作时。

**影响范围：**
- FG-PIPELINE/FC-HAZARD/CK-DATA-HAZARD
- FG-PIPELINE/FC-FORWARD/CK-BYPASS

**根本原因：**
流水线前递逻辑实现不完整，未正确处理写后读（RAW）数据冒险，导致使用了过期的寄存器值。

**具体代码缺陷：**
```scala
// Pipeline.scala 第156-168行，前递逻辑不完整
156: // EX阶段
157: val ex_result = Wire(UInt(32.W))
158: val ex_alu_op = Wire(UInt(4.W))
159: 
160: when(id_ex_reg.valid) {
161:   val operand_a = Mux(forward_a === 0.U, 
162:                       rf.read_data1,           // BUG: 可能是过期数据
163:                       ex_wb_result)            // 只考虑了EX->EX前递
164:   val operand_b = Mux(forward_b === 0.U,
165:                       rf.read_data2,           // BUG: 同样的问题
166:                       ex_wb_result)            // 缺少MEM->EX前递
167:   ex_result := alu.compute(operand_a, operand_b, ex_alu_op)
168: }
```

**修复建议：**
```scala
// 完整的前递逻辑
val operand_a = MuxCase(rf.read_data1, Seq(
  (forward_a === 1.U) -> mem_wb_result,    // MEM->EX前递
  (forward_a === 2.U) -> ex_wb_result      // EX->EX前递
))
val operand_b = MuxCase(rf.read_data2, Seq(
  (forward_b === 1.U) -> mem_wb_result,    // MEM->EX前递  
  (forward_b === 2.U) -> ex_wb_result      // EX->EX前递
))
```

**验证方法：** 编写连续相关指令的测试序列，验证数据前递的正确性和计算结果的准确性。

#### 5. 未知缺陷待调查

**缺陷描述：** 某些检查点失败但暂时无法确定根本原因。

**影响范围：**
- FG-LOGIC/FC-COMPARE/CK-EQUAL （BG-CMP_EQUAL-18）

**当前状态：** 正在调查中，需要更详细的仿真分析和波形查看。

**下一步行动：**
1. 收集更多失败案例的输入数据
2. 进行详细的时序仿真分析
3. 检查相关的组合逻辑实现
4. 与设计团队进行技术讨论

## 质量保证要求

### 强制要求

1. **完整性检查**：每个Bug都必须有对应的标签 `BG-*-xx` 标签
2. **置信度评估**：置信度必须基于客观分析，不能随意设定
3. **根因分析**：高置信度（>70%）的缺陷必须提供详细的根因分析
4. **修复跟踪**：每个缺陷都应有对应的修复计划和验证方法

### 文档维护

- 缺陷修复后及时更新文档状态
- 保留历史记录以供后续分析参考
- 定期回顾分析质量，持续改进分析方法

-----

**重要提示：** 
- 在文本中引用标签时，为防止被解析导致错误，需要去掉尖括号，例如`FG-CONTROL`、`CK-MISPREDICT`、`BG-*-xx`等
- Bug 对应的测试用例应该 Fail，功能正常的检查点对应的测试用例应该 Pass；若出现`全部测试用例 Fail`，应优先排查：测试基线 / 复位时序 / 公共依赖环境。
- 当一个测试用例覆盖多个测试点时，如可行，应拆分为多个细粒度用例，使定位和覆盖统计更清晰。
- 标签`BG-*-xx`中xx为0时，表示该标签用于为占位，其后续的测试用例标签`TC-*`可以为Pass或者Fail，如果不为零，这些测试用例必须为Fail。
- 当Check Point为Fail且并没有对应的Fail测试用例说明其是否有bug时，需要用标签`BG-*-0`进行标记，在后续验证中需要再次对Bug置信度为0的标记进行深入分析。

---

## 静态分析Bug文档规范（`{DUT}_static_bug_analysis.md`）

静态分析阶段通过源码审查（不运行仿真）发现潜在设计缺陷，其结果记录在独立文件 `{OUT}/{DUT}_static_bug_analysis.md` 中，与动态测试结果文件 `{DUT}_bug_analysis.md` 相互补充，共同构成完整的Bug记录体系。

### 标签层级结构

静态分析文档与动态测试文档使用**完全相同的** `FG → FC → CK` 层级组织结构。`<BG-STATIC-*>` 挂靠在 `<CK-*>` 之下，其下一级是**动态Bug关联标签 `<LINK-BUG-*>`**，用于在 `static_bug_validation` 阶段建立静态Bug与动态Bug之间的可追踪链接。每个 `<LINK-BUG-*>` 下还必须包含**源文件位置标签 `<FILE-*>`**，标记该Bug在源代码中的具体位置。

**重要**：一个 `<CK-*>` 检测点下可以挂靠多个 `<BG-STATIC-*>` 标签，每个标签代表在该检测点发现的一个独立Bug。

```
<FG-功能组>                                 ← 与 _functions_and_checks.md 共用或新增 <FG-STATIC>
  <FC-功能点>                               ← 与 _functions_and_checks.md 共用或新增 <FC-STATIC-*>
    <CK-检测点>                             ← 一个CK检测点（可挂靠多个BG-STATIC）
      <BG-STATIC-序号-名称1>                ← 第一个静态Bug
        <LINK-BUG-[BG-TBD]>                 ← 静态分析时默认填写，validation阶段必须替换
          <FILE-filepath:line1-line2>       ← 源文件位置（必填），紧排在 LINK-BUG 行下
      <BG-STATIC-序号-名称2>                ← 第二个静态Bug（同一CK下）
        <LINK-BUG-[BG-TBD]>
          <FILE-filepath:line1-line2>
      <BG-STATIC-000-NULL>                  ← 无Bug声明（未发现Bug时），不需要 <LINK-BUG-*> 子标签
```

`<BG-STATIC-*>` 下的 `<LINK-BUG-*>` 子标签状态转换：

| 子标签 | 状态 | 含义 |
|--------|------|------|
| `<LINK-BUG-[BG-TBD]>` | 待验证 | 静态分析阶段默认填写，表示尚未有对应动态测试结果 |
| `<LINK-BUG-[BG-NAME-xx]>` | 已证实（单个） | 替换 `<LINK-BUG-[BG-TBD]>`，填写 `_bug_analysis.md` 中对应动态Bug的实际标签名 |
| `<LINK-BUG-[BG-NAME1-xx][BG-NAME2-xx]>` | 已证实（多个） | 一个静态Bug对应多个动态Bug时，用多个 `[BG-*]` 方括号组依次拼写；每个标签均须在 `_bug_analysis.md` 中存在 |
| `<LINK-BUG-[BG-NA]>` | 误报 | 替换 `<LINK-BUG-[BG-TBD]>`，表示经动态测试验证该潜在Bug不存在 |

**`static_bug_validation` 阶段的核心任务就是消除所有 `<LINK-BUG-[BG-TBD]>`：**
- 扫描 `_static_bug_analysis.md` 中所有 `<LINK-BUG-[BG-TBD]>` 标签
- 对每一个编写动态测试用例，根据测试结果将 `<LINK-BUG-[BG-TBD]>` 替换为 `<LINK-BUG-[BG-NAME-xx]>`（或多标签形式）或 `<LINK-BUG-[BG-NA]>`
- 阶段完成时 `_static_bug_analysis.md` 中**不允许有任何 `<LINK-BUG-[BG-TBD]>` 残留**

`<LINK-BUG-*>` 标签是标准的层级标签，可由 `parse_nested_keys` 统一解析（层级：`FG → FC → CK → BG-STATIC → LINK-BUG`）。

与动态格式的对比：

| 层级 | 动态测试文档（`_bug_analysis.md`） | 静态分析文档（`_static_bug_analysis.md`） |
|------|----------------------------------|------------------------------------------|
| 功能组 | `<FG-*>` | `<FG-*>`（同，可引用已有或新增 `<FG-STATIC>`） |
| 功能点 | `<FC-*>` | `<FC-*>`（同，可引用已有或新增 `<FC-STATIC-*>`） |
| 检测点 | `<CK-*>` | `<CK-*>`（同，可引用已有或在 `_functions_and_checks.md` 中新增） |
| 静态Bug标签 | 无 | `<BG-STATIC-序号[-名称]>`（挂靠在 CK 之下） |
| 动态Bug关联 | `<BG-功能名-置信度数字>`（直接在 CK 下） | `<LINK-BUG-[BG-TBD]>` / `<LINK-BUG-[BG-NAME-xx]>` / `<LINK-BUG-[BG-NA]>`（挂靠在 BG-STATIC 之下） |
| 源文件位置 | 无 | `<FILE-filepath:line1-line2>`（挂靠在 LINK-BUG 之下，**必填**） |
| 测试用例 | `<TC-*>`（必须 Fail） | **不出现**（写入 `_bug_analysis.md`） |

### 源文件位置标签 `<FILE-filepath:linerange>`

每个 `<LINK-BUG-*>` 标签下必须紧跟至少一个 `<FILE-*>` 子标签，标明该静态Bug在源代码中的具体位置，格式为：

```
  - <FILE-filepath:line1-line2[,line3-line4,...]>
```

| 字段 | 说明 | 示例 |
|------|------|------|
| `filepath` | 源文件**相对于工作区根目录**的相对路径，不含空格 | `UartTx.v`，`src/rtl/fsm.v` |
| `:` | 路径与行号分隔符（固定为英文冒号） | `:` |
| `line1-line2` | 连续行范围，start ≤ end | `50-56`，`100-120` |
| `,line3-line4` | 多个不连续行范围，逗号分隔 | `50-56,100-120` |

**一个 `<LINK-BUG-*>` 可以携带多个 `<FILE-*>` 标签**，对应同一静态Bug涉及多个文件或多处代码段的情形：

```
  - <LINK-BUG-[BG-TBD]>
    - <FILE-UartTx.v:50-56>
    - <FILE-UartTx.v:100-105>
    - <FILE-pkg/uart_pkg.sv:22-24>
```

`<BG-STATIC-000-NULL>` 无需 `<LINK-BUG-*>` 子标签，因此也无需 `<FILE-*>` 子标签。

**Checker 强制验证**：每个非 NULL 的 `<BG-STATIC-*>` 下的 `<LINK-BUG-*>` 子标签必须至少包含一个格式合法的 `<FILE-*>` 子标签，否则报错。同时 Checker 会通过 `self.get_path(filepath)` 验证 `filepath` 指向的源文件在工作区中实际存在；若文件不存在则报错，提示将路径更正为相对于工作区根目录的正确相对路径（例如 `rtl/dut.v:50-56`，而非绝对路径）。

**源代码引用要求**：在 `<FILE-*>` 标签行下方（作为自由文本子内容），必须贴出对应的 RTL 源代码片段（fenced code block），以便审阅者无需打开原始文件即可理解Bug上下文：

```markdown
  - <FILE-UartTx.v:50-56>
    ```verilog
    50: always @(posedge clk) begin
    51:   case (state)
    52:     IDLE: if (start) state <= SEND;   // BUG: 未检查 tx_busy
    53:     SEND: if (bit_cnt == 8) state <= IDLE;
    54:     // default 分支缺失
    55:   endcase
    56: end
    ```
```

### 批次分析进度标记 `<file>`

`static_bug_analysis` 阶段采用**批次推进**方式：`UnityChipBatchCheckerStaticBug` Checker 每次从待分析文件列表中取出若干个文件（由 `batch_size` 控制，默认为 1），由 LLM 对其进行静态审查并将结果写入 `{DUT}_static_bug_analysis.md`。

每完成一批次的分析后，LLM 需要在 `{DUT}_static_bug_analysis.md` **末尾**维护一个 **`## 批次分析进度`** 进度表格，记录每个已分析文件及其发现的疑似Bug数量。

#### 进度表格格式

````markdown
## 批次分析进度

| 源文件 | 发现疑似Bug数 | 状态 |
|--------|-------------|------|
| <file>UartTx/UartTx.v</file> | 1 | ✅ 完成 |
| <file>UartTx/uart_pkg.sv</file> | 0 | ✅ 完成 |
````

**操作规则：**

1. **首次追加**：若文档末尾尚无 `## 批次分析进度` 章节，先创建该章节和表格标题行，再添加文件行；
2. **后续批次**：直接在已有表格中**追加新行**，不重新创建章节；
3. 每个已分析文件**一行**，格式固定为 `| <file>文件路径</file> | N | ✅ 完成 |`。

**列说明：**

| 列 | 要求 | 示例 |
|----|------|------|
| 源文件 | `<file>` 标签包裹路径，与工作区根目录的相对路径**完全一致**（大小写敏感） | `<file>UartTx/UartTx.v</file>` |
| 发现疑似Bug数 | 本批次分析该文件发现的疑似 Bug 数量（整数，可为 0） | `1`、`0` |
| 状态 | 固定写 `✅ 完成` | — |

**Checker 的状态推导机制（无状态）：**

`UnityChipBatchCheckerStaticBug` 是**无状态**的，每次检查时通过正则解析文档中所有 `<file>…</file>` 标记来确定已完成的文件列表：

- 若某文件的路径出现在任意 `<file>` 标记中（无论其位于纯文本还是表格行内），则认为该文件已完成分析；
- 否则该文件将被纳入下一批次；
- 当所有文件均有对应标记后，Checker 自动调用 `UnityChipCheckerStaticBugFormat` 执行完整格式校验；
- `<file>` 标记（进度追踪）与 `<FILE-*>` 源文件位置标签（Bug 层级中的结构化标签）**功能不同，勿混淆**。

> **注意**：`## 批次分析进度` 章节是追加在文档正文之后的**进度元数据**，不属于标签层级结构，`parse_nested_keys` 不会解析它；`<file>` 标签内容中的斜杠、冒号等字符不影响主体文档的标签解析。

### 静态Bug标签 `<BG-STATIC-*>`

静态Bug标签格式为 `<BG-STATIC-序号>` 或 `<BG-STATIC-序号-名称>`，序号从 `001` 开始递增：

- `<BG-STATIC-001>` — 纯序号形式
- `<BG-STATIC-001-FSM-DEAD>` — 推荐格式，附加简洁名称提高可读性
- `<BG-STATIC-000-NULL>` — **无Bug声明**：静态审查完成后**未发现任何潜在缺陷**时使用；序号固定为 `000`，名称固定为 `NULL`；**不需要** `<LINK-BUG-*>` 子标签；**不允许**与其他 `<BG-STATIC-*>` 标签共存。Checker 强制验证：若文档中既无任何 `<BG-STATIC-*>` 标签、又无 `<BG-STATIC-000-NULL>`，则报错。

**`<FG-*>`/`<FC-*>`/`<CK-*>` 的来源规则：**

| 情形 | 做法 |
|------|------|
| 静态bug对应已有功能点 | 直接使用 `_functions_and_checks.md` 中已有的 `<FG-*>`、`<FC-*>`、`<CK-*>` 标签 |
| 静态bug对应已有功能点但缺少检测点 | 在 `_functions_and_checks.md` 中补充新 `<CK-*>` 检测点，再在静态文档中引用 |
| 静态bug对应全新功能域（源码中发现但原规格未覆盖） | 在 `_functions_and_checks.md` 中新增 `<FG-STATIC>`、`<FC-STATIC-*>`、`<CK-STATIC-*>`，再引用 |

高/中置信度潜在Bug新增的 `<CK-*>` 检测点，必须可通过 DUT 输入输出端口观测（不依赖内部信号状态），以确保后续动态测试可验证。

### 静态分析置信度

| 置信度 | 含义 | 后续处理 |
|--------|------|---------|
| 高 | RTL 逻辑明确有误，无需仿真即可判断是Bug | 必须补充 `<CK-*>` 检测点；动态测试阶段优先复现 |
| 中 | 代码逻辑可疑，行为不确定，需测试验证 | 应补充 `<CK-*>` 检测点；动态测试阶段进行验证 |
| 低 | 边界条件存疑或风格问题，可能不影响功能 | 可在资源充裕时进行验证，检测点可选 |

### `{DUT}_static_bug_analysis.md` 文档结构

```
# {DUT} RTL 源码静态分析报告

## 一、架构概述

（简要描述模块层次、数据流、关键设计单元）

## 二、审查范围

- 审查文件列表：...
- 对应功能测试点文档：{OUT}/{DUT}_functions_and_checks.md

## 三、潜在Bug汇总

（按置信度从高到低排列；动态Bug关联列初始全部填写 LINK-BUG-[BG-TBD]，validation 后更新）

| 序号 | Bug标签 | 功能路径 | 描述摘要 | 置信度 | 涉及文件 | 动态Bug关联 |
|------|---------|---------|---------|--------|---------|------------|
| 001 | BG-STATIC-001-NAME | FG-XXX/FC-YYY/CK-ZZZ | 描述... | 高 | Foo.v | LINK-BUG-[BG-TBD] |
| 002 | BG-STATIC-002-NAME | FG-XXX/FC-YYY/CK-ZZZ | 描述... | 中 | Foo.v | LINK-BUG-[BG-TBD] |

## 四、详细分析

（与动态bug文档相同的层级结构，Bug标签改为 <BG-STATIC-*>；每个 <BG-STATIC-*> 下必须紧跟一行动态Bug关联子标签）

<FG-功能组>

#### 功能点 <FC-功能点>
- <CK-检测点> 检测点描述：...；置信度：高 <BG-STATIC-001-NAME>
  - <LINK-BUG-[BG-TBD]>    ← 静态分析阶段默认填写；validation后替换为 <LINK-BUG-[BG-NAME-xx]> 或 <LINK-BUG-[BG-NA]>
    - <FILE-{DUT}.v:xx-yy>   ← 必填；标记Bug所在源文件和行号范围
      ```verilog
      xx: ...
      yy: ...
      ```
  - **触发条件**：（输入/状态组合）
  - **预期行为**：（正确应有的输出）
  - **推断实际行为**：（RTL 推断出的错误输出）
  - **修复建议**：
    ```verilog
    // 修复后
    xx: ...   // 修复说明
    ```
```

> **注意**：`<LINK-BUG-[BG-TBD]>` 是每条静态Bug的**必填子标签**，在 `static_bug_analysis` 阶段写入，紧排在 `<BG-STATIC-*>` 行的下一子条目。`<FILE-filepath:line1-line2>` 是 `<LINK-BUG-*>` 的**必填子标签**，紧排在 `<LINK-BUG-*>` 行的下一子条目，并附带 RTL 源代码片段。`static_bug_validation` 阶段结束后不允许有 `<LINK-BUG-[BG-TBD]>` 残留。

### 动态Bug关联标签规范（`<LINK-BUG-[BG-TBD]>` → `<LINK-BUG-[BG-NAME-xx]>` / `<LINK-BUG-[BG-NA]>`）

每个 `<BG-STATIC-*>` 下方的第一个子项**必须**是动态Bug关联标签，格式固定为：

```
  - <LINK-BUG-[标签内容]>
```

其中 `[标签内容]` 按如下规则确定：

| 标签格式 | 阶段 | 含义 | 要求 |
|----------|------|------|------|
| `<LINK-BUG-[BG-TBD]>` | `static_bug_analysis` 写入 | 待验证，尚未有动态测试结果 | 所有新建静态Bug的默认值，不允许在 `static_bug_validation` 结束时残留 |
| `<LINK-BUG-[BG-NAME-xx]>` | `static_bug_validation` 更新 | 已证实，对应单个动态Bug | 必须在 `{DUT}_bug_analysis.md` 中存在对应完整 `<BG-NAME-xx>` + `<TC-*>` 记录 |
| `<LINK-BUG-[BG-N1-xx][BG-N2-xx]>` | `static_bug_validation` 更新 | 已证实，对应多个动态Bug | 用多个 `[BG-*]` 方括号组依次拼写；每个动态Bug均须在 `{DUT}_bug_analysis.md` 中有完整记录 |
| `<LINK-BUG-[BG-NA]>` | `static_bug_validation` 更新 | 误报，经动态测试验证该潜在Bug不存在 | 在该行下方（可选）添加一行误判说明 |

**`static_bug_validation` 阶段完成标准**：`{DUT}_static_bug_analysis.md` 中不存在任何 `<LINK-BUG-[BG-TBD]>` 标签，Checker 会通过 `parse_nested_keys` 解析该文件强制验证此规则。

### 完整示例（三阶段演进）

以 UartTx 模块为例，展示从静态发现到动态验证的完整标签演进过程。

**阶段一：`static_bug_analysis` 阶段完成时**（所有静态Bug均为 `<LINK-BUG-[BG-TBD]>`）

```markdown
## 三、潜在Bug汇总

| 序号 | Bug标签 | 功能路径 | 描述摘要 | 置信度 | 涉及文件 | 动态Bug关联 |
|------|---------|---------|---------|--------|---------|------------|
| 001 | BG-STATIC-001-FSM-DEAD | FG-CONTROL/FC-FSM/CK-FSM-BUSY-CONFLICT | FSM跳转缺少tx_busy保护 | 高 | UartTx.v | LINK-BUG-[BG-TBD] |
| 002 | BG-STATIC-002-FSM-DEFAULT | FG-CONTROL/FC-FSM/CK-FSM-BUSY-CONFLICT | FSM缺少default分支 | 高 | UartTx.v | LINK-BUG-[BG-TBD] |
| 003 | BG-STATIC-003-WIDTH-MISMATCH | FG-TIMING/FC-BAUD/CK-BAUD-OVERFLOW | baud_cnt位宽可能不足 | 中 | UartTx.v | LINK-BUG-[BG-TBD] |

## 四、详细分析

<FG-CONTROL>

#### 状态机功能 <FC-FSM>
- <CK-FSM-BUSY-CONFLICT> 状态机检测点（该检测点下发现两个Bug）
  - <BG-STATIC-001-FSM-DEAD> IDLE→SEND 跳转缺少 tx_busy 保护，start 在发送中拉高时可能进入未定义状态；置信度：高
    - <LINK-BUG-[BG-TBD]>
      - <FILE-UartTx.v:50-56>
        ```verilog
        50: always @(posedge clk) begin
        51:   case (state)
        52:     IDLE: if (start) state <= SEND;          // BUG: 未检查 tx_busy
        53:     SEND: if (bit_cnt == 8) state <= IDLE;
        54:     // default 分支缺失
        55:   endcase
        56: end
        ```
  - <BG-STATIC-002-FSM-DEFAULT> FSM缺少default分支，可能进入非法状态；置信度：高
    - <LINK-BUG-[BG-TBD]>
      - <FILE-UartTx.v:50-56>
        ```verilog
        50: always @(posedge clk) begin
        51:   case (state)
        52:     IDLE: if (start) state <= SEND;
        53:     SEND: if (bit_cnt == 8) state <= IDLE;
        54:     // BUG: default 分支缺失，可能导致锁死
        55:   endcase
        56: end
        ```

<FG-TIMING>

#### 波特率计数功能 <FC-BAUD>
- <CK-BAUD-OVERFLOW> baud_cnt 位宽为 8 位，当分频系数超过 255 时可能截断；置信度：中 <BG-STATIC-002-WIDTH-MISMATCH>
  - <LINK-BUG-[BG-TBD]>
    - <FILE-UartTx.v:20>
      ```verilog
      20: reg [7:0] baud_cnt;   // BUG: 8位宽不足以确论覆盖所有分频参数
      ```

**阶段二：`static_bug_validation` 阶段完成时**（`<LINK-BUG-[BG-TBD]>` 全部被替换，无残留）

```markdown
## 三、潜在Bug汇总

| 序号 | Bug标签 | 功能路径 | 描述摘要 | 置信度 | 涉及文件 | 动态Bug关联 |
|------|---------|---------|---------|--------|---------|------------|
| 001 | BG-STATIC-001-FSM-DEAD | FG-CONTROL/FC-FSM/CK-FSM-BUSY-CONFLICT | FSM跳转缺少tx_busy保护 | 高 | UartTx.v | LINK-BUG-[BG-FSM-DEAD-92] |
| 002 | BG-STATIC-002-FSM-DEFAULT | FG-CONTROL/FC-FSM/CK-FSM-BUSY-CONFLICT | FSM缺少default分支 | 高 | UartTx.v | LINK-BUG-[BG-FSM-DEFAULT-85] |
| 003 | BG-STATIC-003-WIDTH-MISMATCH | FG-TIMING/FC-BAUD/CK-BAUD-OVERFLOW | baud_cnt位宽可能不足 | 中 | UartTx.v | LINK-BUG-[BG-NA] |

## 四、详细分析

<FG-CONTROL>

#### 状态机功能 <FC-FSM>
- <CK-FSM-BUSY-CONFLICT> 状态机检测点（该检测点下发现两个Bug）
  - <BG-STATIC-001-FSM-DEAD> IDLE→SEND 跳转缺少 tx_busy 保护，start 在发送中拉高时可能进入未定义状态；置信度：高
    - <LINK-BUG-[BG-FSM-DEAD-92]>    ← 已替换，该静态Bug证实对应一个动态Bug
      - <FILE-UartTx.v:50-56>    ← 源文件位置不变
        ```verilog
        50: always @(posedge clk) begin
        51:   case (state)
        52:     IDLE: if (start) state <= SEND;          // BUG: 未检查 tx_busy
        53:     SEND: if (bit_cnt == 8) state <= IDLE;
        54:     // default 分支缺失
        55:   endcase
        56: end
        ```
  - <BG-STATIC-002-FSM-DEFAULT> FSM缺少default分支，可能进入非法状态；置信度：高
    - <LINK-BUG-[BG-FSM-DEFAULT-85]>    ← 已替换，该静态Bug证实对应一个动态Bug
      - <FILE-UartTx.v:50-56>    ← 源文件位置不变
        ```verilog
        50: always @(posedge clk) begin
        51:   case (state)
        52:     IDLE: if (start) state <= SEND;
        53:     SEND: if (bit_cnt == 8) state <= IDLE;
        54:     // BUG: default 分支缺失，可能导致锁死
        55:   endcase
        56: end
        ```

<FG-TIMING>

#### 波特率计数功能 <FC-BAUD>
- <CK-BAUD-OVERFLOW> baud_cnt 位宽为 8 位，当分频系数超过 255 时可能截断；置信度：中 <BG-STATIC-003-WIDTH-MISMATCH>
  - <LINK-BUG-[BG-NA]>             ← 已替换，误报
    - <FILE-UartTx.v:20>    ← 源文件位置不变
      ```verilog
      20: reg [7:0] baud_cnt;   // 实际参数通过 parameter 约束不超过 200，8位宽足够
      ```
  - 误判说明：波特率参数通过 `parameter` 约束不超过 200，8 位宽足够
```

**阶段三：`{DUT}_bug_analysis.md` 中对应的动态Bug记录**（由 `static_bug_validation` 阶段写入）

```markdown
<FG-CONTROL>

#### 状态机功能 <FC-FSM>
- <CK-FSM-BUSY-CONFLICT> 测试 start 在 tx_busy=1 期间重新置位时 FSM 行为 <BG-FSM-DEAD-92>
  - <TC-FSM-REENTRANT-FAIL> start_during_send：验证FSM重入保护
    - 测试结果：FAIL ← 证实静态分析Bug BG-STATIC-001-FSM-DEAD

- <CK-FSM-BUSY-CONFLICT> 测试缺少 default 分支时 FSM 进入非法状态的行为 <BG-FSM-DEFAULT-85>
  - <TC-FSM-ILLEGAL-STATE> illegal_state_enter：验证default分支缺失
    - 测试结果：FAIL ← 证实静态分析Bug BG-STATIC-002-FSM-DEFAULT
```

### 标签书写要点

| 层级 | 格式示例 | 说明 |
|------|---------|------|
| 功能组 FG | `<FG-CONTROL>` | 与 `_functions_and_checks.md` 共用，或新增 `<FG-STATIC>` |
| 功能点 FC | `<FC-FSM>` | 与 `_functions_and_checks.md` 共用，或新增 `<FC-STATIC-*>` |
| 检测点 CK | `<CK-FSM-BUSY-CONFLICT>` | 需同步写入 `_functions_and_checks.md`（高/中置信度必须） |
| 静态Bug | `<BG-STATIC-001-FSM-DEAD>` | 挂靠在 `<CK-*>` 之后，序号+名称格式；**一个 `<CK-*>` 下可以有多个 `<BG-STATIC-*>` 标签** |
| 静态Bug（无Bug声明） | `<BG-STATIC-000-NULL>` | 挂靠在 `<CK-*>` 之后；序号固定 `000`，名称固定 `NULL`；不需要 `<LINK-BUG-*>` 子标签；不可与其他 `<BG-STATIC-*>` 共存 |
| 动态Bug关联（待验证） | `<LINK-BUG-[BG-TBD]>` | 每个 `<BG-STATIC-*>` 的**必填**子标签，`static_bug_analysis` 阶段写入 |
| 动态Bug关联（已证实，单个） | `<LINK-BUG-[BG-FSM-DEAD-92]>` | `static_bug_validation` 后替换，需在 `_bug_analysis.md` 中有对应完整记录 |
| 动态Bug关联（已证实，多个） | `<LINK-BUG-[BG-FSM-DEAD-92][BG-FSM-DEFAULT-85]>` | 一个静态Bug证实存在多个动态Bug时，用多个 `[BG-*]` 方括号组依次拼写；每个标签均须在 `_bug_analysis.md` 中有完整记录 |
| 动态Bug关联（误报） | `<LINK-BUG-[BG-NA]>` | `static_bug_validation` 后替换，可选附加误判说明行 |
| 源文件位置（单个范围） | `<FILE-UartTx.v:50-56>` | 每个 `<LINK-BUG-*>` 的**必填**子标签；路径相对于项目根目录；行号范围 `N-M` |
| 源文件位置（多范围） | `<FILE-UartTx.v:50-56,100-120>` | 同一文件的多个不连续行范围，逗号分隔 |
| 源文件位置（多文件） | `<FILE-pkg/uart_pkg.sv:22-24>` | 同一 `<LINK-BUG-*>` 下可配置多个 `<FILE-*>` 子标签 |

**注意**：
- `<BG-STATIC-*>` 标签仅在 `{DUT}_static_bug_analysis.md` 中使用，不出现在 `{DUT}_bug_analysis.md` 中
- **一个 `<CK-*>` 检测点下可以挂靠多个 `<BG-STATIC-*>` 标签，每个标签代表在该检测点发现的一个独立Bug**
- `<BG-STATIC-000-NULL>` 是必须显式书写的无Bug声明，不可省略——若 `_static_bug_analysis.md` 中既无任何 `<BG-STATIC-*>` 又无 `<BG-STATIC-000-NULL>`，Checker 将报错
- `<LINK-BUG-*>` 标签仅在 `{DUT}_static_bug_analysis.md` 中使用，不出现在 `{DUT}_bug_analysis.md` 中
- `<FILE-*>` 标签是 `<LINK-BUG-*>` 的必填子标签，Checker 强制验证其存在和格式；`<FILE-*>` 行下方必须附带对应 RTL 源代码片段
- `static_bug_validation` 阶段结束后，`{DUT}_static_bug_analysis.md` 中**不允许有任何 `<LINK-BUG-[BG-TBD]>` 残留**，Checker 通过 `parse_nested_keys` 强制验证
- 在文中引用标签时去掉尖括号，例如 `BG-STATIC-001`，防止解析错误
- 静态分析报告的主体文档内容须使用 `{DOC_GEN_LANG}` 指定的语言编写
