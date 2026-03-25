
# 代码行覆盖率指南

## 概述

代码行覆盖率（Line Coverage）是芯片验证中的重要质量指标，用于衡量测试用例对设计代码的执行覆盖程度。它反映了在测试过程中有多少比例的代码行被实际执行，是评估验证完整性和发现测试盲点的关键工具。

### 重要性

- **验证完整性评估**：确保关键代码路径都被测试覆盖
- **测试质量量化**：提供可量化的验证质量指标
- **回归测试保障**：确保代码修改不会破坏已有功能
-## 覆盖率过滤：Ignore文件使用

在实际验证中，DUT模块可能引用其他模块，或者测试时仅关注特定功能，但代码行覆盖率统计了所有代码导致结果与预期不符。为此需要使用ignore文件来让测试报告忽略指定代码行的统计。

### Ignore文件基础语法

在unitytest环境中可通过 `*.ignore` 文件来指定需要忽略的代码行。ignore文件支持以下语法：

```bash
# 注释以井号开头，仅支持单行注释
# 解析ignore文件时，会忽略空行
# 1. 忽略整个文件
Adder.v
SubModule.v

# 2. 忽略文件中的特定行（行号从1开始）
# 格式：文件名:起始行-结束行[,起始行-结束行,...]
example.v:4-4        # 忽略第4行
example.v:10-20      # 忽略第10到20行
example.v:4-4,83-92  # 忽略第4行和第83到92行(多个代码段之间用逗号隔离)

# 3. 使用通配符（glob语法）
Sub*.v               # 忽略所有以Sub开头的.v文件
*_tb.v               # 忽略所有以_tb结尾的.v文件
test/*.v             # 忽略test目录下的所有.v文件
**/*_old.v           # 忽略任意目录下以_old结尾的.v文件
```

**重要**：

`*.ignore` 文件中的匹配语法请参考python中的函数[fnmatch.fnmatch](https://docs.python.org/zh-cn/3/library/fnmatch.html)的匹配规则


### 高级用法示例

**1. 复杂项目的ignore配置**

```bash
# project.ignore
# ================================
# 项目覆盖率忽略配置
# ================================

# 第三方IP核
vendor/xilinx/*.v
vendor/altera/*.v

# 测试台文件
**/*_tb.v
**/*_test.v
**/testbench/*.v

# 自动生成的文件
generated/*.v
build/**/*.v

# 特定模块的特定行
# CPU模块中的调试相关代码
cpu.v:100-120,200-250
cpu.v:500-600

# 内存控制器中的未使用功能
memory_ctrl.v:1000-1500

# Legacy代码
legacy/*.v
deprecated/**/*.v
```

**2. 基于功能模块的ignore配置**

```bash
# functional.ignore
# ================================
# 基于功能的忽略配置
# ================================

# 忽略浮点运算单元（当前测试不涉及）
fpu/*.v

# 忽略调试接口
debug_interface.v
jtag_controller.v

# 忽略电源管理相关代码
power_mgmt.v:*

# 忽略可选功能模块
optional_features/*.v

# 忽略特定配置下不会执行的代码
config_a.v:50-100    # 仅在配置B下使用
config_b.v:150-200   # 仅在配置A下使用
```

**3. 按测试阶段的ignore配置**

```bash
# stage1.ignore - 基础功能测试阶段
# ================================

# 暂不测试的高级功能
advanced_features/*.v
optimization/*.v

# 错误注入和容错相关
fault_injection.v
error_correction.v

# 性能计数器
performance_counters.v:*
```

## Uniytest验证项目中对应的方法

### 开启覆盖率生成

需要在创建dut的`create_dut(request)`函数中，调用DUT的`SetCoverage`方法设置生成覆盖率文件名，例如

```python
...
    dut.SetCoverage("path/to/coverage.dat")
...
```

在test case执行完成后，调用`toffee_test.reporter`包中的`set_line_coverage`方法，进行覆盖率收集，例如在`dut fixture`中的 `yield`之后调用：

```python
@pytest.fixture()
def dut(request):
    ...
    yield dut
    ...
    set_line_coverage(request, "path/to/coverage.dat", ignore=[])
    ...
```

**重要：**
- 需要通过get_coverage_data_path(request, new_path=True)获取代码行覆盖率文件路径，并传递给 dut.SetCoverage
- 需要通过get_coverage_data_path(request, new_path=False)获取已有路径的代码行覆盖率文件，并传递给 set_line_coverage


在ignore参数中指定ignore文件(或者ignore表达式，或者ignore文件所在的文件夹)，支持以list格式传递多个值。


#### 覆盖率分析

覆盖率分析报告基于DUT验证代码行覆盖率的分析：

- 是否达到验证要求
- 未覆盖代码行由于什么原因没被覆盖，是否重要
- 如果ignore了文件或者代码行，需要进行逐一分析为什么要ignore

ignore分析格式为：

<LINE_IGNORE>pattern</LINE_IGNORE>： 具体ignore原因


**注意：**
- `<LINE_IGNORE>pattern</LINE_IGNORE>` 只能在同一行，且一行只能出现一次。
- 检测器通过该标签来分析ignore文件，一定要注意格式


##### 覆盖率分析举例

**示例1：CPU核心模块覆盖率分析**

```
模块名称：CPU_Core.v
总代码行数：1250行
覆盖行数：1125行
覆盖率：90.0%
目标覆盖率：≥92%
分析结论：未达到目标，需要补充测试用例
```

**未覆盖代码分析：**

```verilog
// 文件：CPU_Core.v
// 第45-52行：异常处理代码（未覆盖）
45: if (exception_type == 4'b1111) begin
46:     exception_pc <= pc_reg;
47:     exception_cause <= RESERVED_EXCEPTION;
48:     state <= EXCEPTION_STATE;
49: end else if (exception_type == 4'b1110) begin
50:     // 调试模式异常处理
51:     debug_mode <= 1'b1;
52: end
```

**ignore配置分析：**

<LINE_IGNORE>*/CPU_Core.v:45-48</LINE_IGNORE>：预留异常处理功能，当前设计规范v1.2中未定义异常类型4'b1111的具体行为，计划在v2.0版本中实现。此代码路径在当前版本无法通过正常测试激活。

**行动计划：**
- 第49-52行：需要添加调试模式测试用例，不应ignore
- 第45-48行：可以ignore，但需要在v2.0版本实现时移除ignore配置

**示例2：内存控制器模块分析**

```
模块名称：Memory_Controller.v
总代码行数：800行
覆盖行数：720行
覆盖率：90.0%
目标覆盖率：≥90%
分析结论：达到目标
```

**ignore配置分析：**

<LINE_IGNORE>*/Memory_Controller.v:156-164</LINE_IGNORE>：错误注入测试功能，通过宏定义ERROR_INJECTION控制，仅用于可靠性测试阶段。在功能验证阶段此宏未定义，代码不会被编译。

<LINE_IGNORE>*/debug_interface.v</LINE_IGNORE>：调试接口模块，仅在FPGA验证环境中使用，ASIC实现中会被移除。包含JTAG扫描链和调试寄存器功能。

**未覆盖但不ignore的代码：**
```verilog
// 第234-240行：边界条件处理（重要安全功能）
234: if (burst_length > MAX_BURST_LEN) begin
235:     burst_error <= 1'b1;
236:     burst_length <= MAX_BURST_LEN;
237: end else if (burst_length == 0) begin
238:     burst_error <= 1'b1;
239:     burst_length <= 1;
240: end
```
**分析：** 此代码为关键安全保护功能，必须测试。需要创建测试用例test_illegal_burst_length。

**示例3：ALU模块完整分析报告**

```markdown
## ALU模块覆盖率分析报告

### 基本信息
- 文件：ALU.v
- 总行数：450行
- 覆盖行数：441行  
- 覆盖率：98.0%
- 目标：≥95%
- 结论：✅ 达标

### ignore配置详细分析

<LINE_IGNORE>*/ALU.v:89-95</LINE_IGNORE>：浮点除法器功能。当前ALU配置为定点运算模式(FLOAT_ENABLE=0)，浮点功能在此配置下不可达。根据需求文档，当前阶段仅验证定点运算，浮点功能计划Phase2实现。

<LINE_IGNORE>*/ALU.v:156-160</LINE_IGNORE>：溢出标志位特殊处理。仅在OVERFLOW_MODE=2时执行，当前验证使用OVERFLOW_MODE=0。经设计团队确认，模式2为实验性功能，暂不验证。

<LINE_IGNORE>*/ALU.v:203</LINE_IGNORE>：内部一致性检查断言。正常功能测试中不会触发，仅在内部状态异常时生效。属于保护性代码。

### 风险评估
- **整体风险：** 低
- **关键路径：** 已充分覆盖
- **建议：** 下版本重新评估ALU.v:156-160的必要性

### 未覆盖代码处理
剩余9行未覆盖代码均为边缘情况处理，已通过等价性验证确认功能正确性。
```

**示例4：项目级综合分析**

```markdown
## CPU项目覆盖率综合分析

### 整体概况
| 模块 | 覆盖率 | 目标 | 状态 | ignore行数 |
|------|--------|------|------|------------|
| CPU核心 | 90.0% | ≥92% | ❌ 未达标 | 12 |
| ALU单元 | 98.0% | ≥95% | ✅ 达标 | 9 |
| 缓存控制 | 87.5% | ≥90% | ❌ 未达标 | 45 |
| 调试接口 | 45.2% | N/A | - | 234 |

### ignore合理性统计
- **合理ignore：** 89% (300/337行)
  - 预留功能：35%
  - 调试专用：40% 
  - 配置相关：25%
- **需要重新评估：** 11% (37行)
  - 缺少充分理由：15行
  - 可能过度ignore：22行

### 重点关注项
1. **缓存控制模块**：覆盖率不足，且ignore行数较多，需要详细分析
2. **CPU核心模块**：接近目标但未达标，需要补充测试
3. **ignore配置审查**：11%的ignore需要重新评估理由

### 后续行动计划
- [ ] 立即：补充缓存控制和CPU核心的测试用例
- [ ] 本周：审查37行可疑ignore配置
- [ ] 下周：建立ignore配置的定期审查机制  
- [ ] 下月：制定覆盖率监控自动化方案
```

**示例5：ignore配置标准模板**

每个ignore配置都应该遵循以下分析模板：

```markdown
<LINE_IGNORE>*/文件名:行号范围</LINE_IGNORE>：[ignore类型] ignore的具体原因，包括技术依据和时间计划。

ignore类型包括：
- 预留功能：未来版本实现的功能
- 配置相关：特定配置下不执行的代码  
- 调试专用：仅用于调试的代码
- 测试专用：仅用于测试的代码
- 第三方IP：外部IP核代码
- 安全保护：异常情况下的保护代码
```

**良好的ignore分析示例：**

<LINE_IGNORE>*/FPU.v:*</LINE_IGNORE>：[预留功能] 浮点运算单元，根据项目roadmap计划在v2.0版本实现。当前配置ENABLE_FPU=0，整个模块不会被使用。预计2024年Q3开始实现，届时需要移除此ignore。

<LINE_IGNORE>*/cache_ctrl.v:456-480</LINE_IGNORE>：[配置相关] 4路组相联缓存的替换算法，当前验证配置为直接映射缓存(CACHE_WAYS=1)。此代码段仅在CACHE_WAYS>1时执行。已通过单独的4路缓存配置验证此功能。

**需要改进的ignore分析示例：**

<LINE_IGNORE>*/module.v:100-120</LINE_IGNORE>：暂时不测试 ❌
*问题：原因不明确，没有技术依据*

<LINE_IGNORE>*/relative_path/complex_logic.v:200-300</LINE_IGNORE>：太复杂了，难以测试 ❌  
*问题：不是合理的ignore理由，应该设计测试方法*

