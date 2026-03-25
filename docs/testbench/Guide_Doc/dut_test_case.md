
# DUT 测试用例编写指南

## 概述

测试用例是验证DUT功能正确性的核心组件。本文档介绍如何编写高质量的测试用例，包括测试结构、覆盖率关联、最佳实践等内容。

## 基本结构和导入

### 标准导入模式

```python
from {DUT}_api import * # 重要，必须用 import *， 而不是 import env，不然会出现 dut 没定义错误
import pytest
# 根据需要导入其他必要模块
```

**设计原则：**
- 测试用例应该只依赖DUT提供的API接口
- 避免直接操作DUT的底层实现细节
- 通过API封装保证测试用例的稳定性和可维护性
- 每个测试用例都应该有合理的assert判断：assert output==expected_output, assert_message

### 基本测试函数结构

```python
def test_basic_functionality(env):
    """测试基本功能的描述

    Args:
        env: Env fixture实例，由pytest自动注入
    """    
    # 0. 覆盖率标记（必须）
    env.dut.fc_cover["FG-ARITHMETIC"].mark_function("FC-ADD",test_basic_functionality, ["CK-BASIC1", "CK-BASIC2"])
    # 如果该用例和多个功能点相关，则需要多次调用 mark_function 分别进行功能点-检查点标记
    # Eg:
    # env.dut.fc_cover["FG-XXXXXX"].mark_function("FC-YYYYYY", test_basic_functionality, ["CK-AAA1", "CK-AAA2"])

    # 1. 测试数据准备
    input_a = 10
    input_b = 20
    expected_result = 30
    
    # 2. 调用API执行操作
    actual_result = api_{DUT}_operation(env, input_a, input_b)
    
    # 3. 断言验证结果
    assert actual_result == expected_result, f"预期: {expected_result}, 实际: {actual_result}"
```

## 覆盖率关联机制

### 标记语法

```python
env.dut.fc_cover["{功能分组}"].mark_function("{功能点}", {测试函数}, ["{检查点列表}"])
```

需要在测试函数的最开始就通过mark_function进行覆盖率关联。不建议在函数结束时关联，因为有可能测试不通过导致关联失败。一次调用mark_function只能关联一个功能点中的多个测试点，如果一个测试用例覆盖多个功能点，需要多次调用mark_function分别进行标记


**参数说明：**
- **功能分组**：对应功能描述文档中的 `<FG-*>` 标签
- **功能点**：对应功能描述文档中的 `<FC-*>` 标签
- **测试函数**：当前测试函数的函数对象（不是字符串）
- **检查点列表**：要关联的检查点列表，对应 `<CK-*>` 标签

### 标记示例

```python
def test_addition_overflow(env):
    """测试加法溢出场景"""
    # 标记覆盖的检查点
    env.dut.fc_cover["FG-ARITHMETIC"].mark_function("FC-ADD", test_addition_overflow, ["CK-OVERFLOW"])

    # 使用最大值测试溢出
    max_val = (1 << 64) - 1
    result, carry = api_adder_add(env, max_val, 1, 0)
    
    # 验证溢出标志
    assert carry == 1, "溢出时应该设置进位标志"
    assert result == 0, "溢出时低位应该为0"    

def test_addition_with_carry(env):
    """测试带进位的加法"""

    # 可以标记多个检查点
    env.dut.fc_cover["FG-ARITHMETIC"].mark_function("FC-ADD", test_addition_with_carry, ["CK-BASIC", "CK-CARRY-IN"])

    result, carry = api_adder_add(env, 10, 20, 1)  # 10 + 20 + 1 = 31
    
    assert result == 31, f"预期结果31，实际{result}"
    assert carry == 0, "正常情况下不应有进位"
    
```

## 覆盖率完整性要求

### 双向覆盖原则

1. **测试→检查点**：每个测试函数至少覆盖一个检查点
2. **检查点→测试**：每个检查点至少被一个测试函数覆盖

### 覆盖率检查清单

```python
# ✅ 良好的覆盖率设计
def test_comprehensive_addition(env):
    """综合加法测试，覆盖多个场景"""
    # 标记所有相关检查点
    env.dut.fc_cover["FG-ARITHMETIC"].mark_function("FC-ADD", test_comprehensive_addition, 
                                                ["CK-BASIC", "CK-ZERO", "CK-CARRY-IN"])

    # 基本加法
    result, _ = api_adder_add(env, 5, 3, 0)
    assert result == 8
    
    # 零值测试
    result, _ = api_adder_add(env, 0, 0, 0)
    assert result == 0
    
    # 进位测试
    result, _ = api_adder_add(env, 10, 20, 1)
    assert result == 31


# ❌ 避免：没有覆盖任何检查点
def test_bad_example(env):
    """这个测试没有标记任何检查点"""
    result = api_some_operation(env)
    assert result is not None
    # 缺少覆盖率标记！
```

## 测试用例分类和组织

### 按功能分组组织

```python
# test_arithmetic.py - 算术功能测试
class TestArithmetic:
    def test_basic_addition(self, env):
        """基本加法测试"""
        ...
        
    def test_addition_overflow(self, env):
        """加法溢出测试"""
        ...
        
    def test_subtraction_basic(self, env):
        """基本减法测试"""
        ...

# test_logic.py - 逻辑功能测试  
class TestLogic:
    def test_bitwise_and(self, env):
        """按位与测试"""
        ...
        
    def test_bitwise_or(self, env):
        """按位或测试"""
        ...
```

### 按测试类型组织

```python
# test_basic.py - 基础功能测试
def test_basic_operations(env):
    """基本操作测试"""
    ...

# test_boundary.py - 边界条件测试
def test_max_value_handling(env):
    """最大值处理测试"""
    ...

def test_min_value_handling(env):
    """最小值处理测试"""
    ...

# test_error.py - 错误处理测试
def test_invalid_input_handling(env):
    """无效输入处理测试"""
    ...
```

## 完整测试用例示例

### 基础加法器测试

```python
from adder_api import *  # 重要，必须用 import *， 而不是 import env，不然会出现 dut 没定义错误
import pytest

class TestAdderBasic:
    """加法器基础功能测试"""
    
def test_basic_addition(env):
    """测试基本加法功能"""
    env.dut.fc_cover["FG-ARITHMETIC"].mark_function("FC-ADD", test_basic_addition, 
                                                ["CK-BASIC", "CK-ZERO", "CK-CARRY-IN"])
    test_cases = [
        (1, 2, 0, 3, 0),      # 简单加法
        (0, 0, 0, 0, 0),      # 零值加法
        (10, 15, 1, 26, 0),   # 带进位输入
    ]
    
    for a, b, cin, expected_sum, expected_cout in test_cases:
        sum_result, cout_result = api_adder_add(env, a, b, cin)
        assert sum_result == expected_sum, f"加法错误: {a}+{b}+{cin}={sum_result}, 期望{expected_sum}"
        assert cout_result == expected_cout, f"进位错误: {a}+{b}+{cin}, cout={cout_result}, 期望{expected_cout}"


def test_overflow_scenarios(env):
    """测试溢出场景"""
    env.dut.fc_cover["FG-ARITHMETIC"].mark_function("FC-ADD", test_overflow_scenarios, 
                                                ["CK-OVERFLOW", "CK-CARRY-OVERFLOW"])
    max_64bit = (1 << 64) - 1
    
    # 最大值 + 1 溢出
    sum_result, cout_result = api_adder_add(env, max_64bit, 1, 0)
    assert cout_result == 1, "溢出时应设置进位标志"
    
    # 最大值 + 最大值溢出
    sum_result, cout_result = api_adder_add(env, max_64bit, max_64bit, 0)
    assert cout_result == 1, "大数相加溢出时应设置进位标志"
    
    # 带进位的溢出
    sum_result, cout_result = api_adder_add(env, max_64bit, 0, 1)
    assert cout_result == 1, "最大值加进位应产生溢出"


def test_boundary_conditions(env):
    """测试边界条件"""
    env.dut.fc_cover["FG-ARITHMETIC"].mark_function("FC-ADD", test_boundary_conditions, 
                                                ["CK-BOUNDARY-MAX", "CK-BOUNDARY-ZERO"])
    max_val = (1 << 64) - 1
    # 最大值与0相加
    sum_result, cout_result = api_adder_add(env, max_val, 0, 0)
    assert sum_result == max_val, "最大值加0应该等于最大值"
    assert cout_result == 0, "最大值加0不应产生进位"
    # 0与最大值相加
    sum_result, cout_result = api_adder_add(env, 0, max_val, 0)
    assert sum_result == max_val, "0加最大值应该等于最大值"
    assert cout_result == 0, "0加最大值不应产生进位"


# 也可以把相同类别的测试用例放入同一个Test Class中进行归类

class TestAdderError:
    """加法器错误处理测试"""
    
    def test_concurrent_operations(self, env):
        """测试并发操作（如果适用）"""
        # 这里可以测试时序相关的问题
        ...
        
    def test_reset_behavior(self, env):
        """测试复位行为"""
        env.dut.fc_cover["FG-CONTROL"].mark_function("FC-RESET", self.test_reset_behavior, ["CK-RESET-RECOVERY"])

        # 先执行一个操作
        api_adder_add(env, 100, 200, 1)
        
        # 复位
        api_adder_reset(env)
        
        # 验证复位后状态
        sum_result, cout_result = api_adder_add(env, 1, 1, 0)
        assert sum_result == 2, "复位后应能正常工作"

```

### 参数化测试示例

```python
import pytest

class TestParameterized:
    """参数化测试示例"""
    
    @pytest.mark.parametrize("a,b,cin,expected_sum,expected_cout", [
        (0, 0, 0, 0, 0),
        (1, 1, 0, 2, 0),
        (10, 20, 1, 31, 0),
        ((1<<64)-1, 1, 0, 0, 1),  # 溢出情况
    ])
    def test_addition_cases(self, env, a, b, cin, expected_sum, expected_cout):
        """参数化加法测试"""
        # 根据测试数据选择不同的检查点
        check_points = ["CK-BASIC"]
        if a == 0 and b == 0:
            check_points.append("CK-ZERO")
        if cin == 1:
            check_points.append("CK-CARRY-IN")
        if expected_cout == 1:
            check_points.append("CK-OVERFLOW")

        env.dut.fc_cover["FG-ARITHMETIC"].mark_function("FC-ADD", self.test_addition_cases, check_points)
        sum_result, cout_result = api_adder_add(env, a, b, cin)
        assert sum_result == expected_sum
        assert cout_result == expected_cout
```

## 最佳实践

### 1. 测试数据设计

```python
def test_comprehensive_data_coverage(env):
    """全面的数据覆盖测试"""
    env.dut.fc_cover["FG-TEST"].mark_function("FC-COMPREHENSIVE", test_comprehensive_data_coverage, ["CK-COVERAGE"])

    # 典型值
    typical_values = [1, 10, 100, 1000]
    
    # 边界值
    boundary_values = [0, (1<<32)-1, (1<<63)-1, (1<<64)-1]
    
    # 特殊值
    special_values = [0x5555555555555555, 0xAAAAAAAAAAAAAAAA]  # 交替位模式

    def golden_result(x, y):
        ....

    for a in typical_values + boundary_values + special_values:
        for b in typical_values[:2]:  # 限制组合数量
            result = api_operation(env, a, b)
            correct_result = golden_result(a, b)
            assert result == correct_result, f"操作失败: a={a}, b={b}, expect: {correct_result}, but find: {result}"

```

### 2. 测试辅助函数

```python
def generate_test_vectors(count=100):
    """生成测试向量的辅助函数"""
    import random
    vectors = []
    for _ in range(count):
        a = random.randint(0, (1<<32)-1)
        b = random.randint(0, (1<<32)-1)
        cin = random.choice([0, 1])
        vectors.append((a, b, cin))
    return vectors

def verify_operation_properties(env, a, b):
    """验证运算性质的辅助函数"""
    # 交换律测试
    result1 = api_commutative_op(env, a, b)
    result2 = api_commutative_op(env, b, a)
    assert result1 == result2, f"交换律失败: {a} op {b} != {b} op {a}"
    
    return result1

def test_mathematical_properties(env):
    """测试数学性质"""
    env.dut.fc_cover["FG-MATH"].mark_function("FC-PROPERTIES", test_mathematical_properties, ["CK-COMMUTATIVE"])

    test_pairs = [(1, 2), (10, 20), (100, 200)]
    
    for a, b in test_pairs:
        result = verify_operation_properties(env, a, b)
        assert result == expected_result, ...
```

## 质量保证检查清单

### 测试完整性
- [ ] 每个测试函数都有明确的docstring说明
- [ ] 每个测试函数都进行了覆盖率标记
- [ ] 所有检查点都被至少一个测试覆盖
- [ ] 测试数据覆盖典型值、边界值、异常值

### 测试质量
- [ ] 断言信息清晰，便于定位问题
- [ ] 测试逻辑简单明确，避免复杂嵌套
- [ ] 测试之间相互独立，无依赖关系
- [ ] 适当使用参数化测试减少重复代码

### 可维护性
- [ ] 测试命名规范，体现测试意图
- [ ] 测试文件组织合理，便于查找
- [ ] 测试辅助函数复用性好
- [ ] 测试数据和预期结果易于理解


**注意：**
- 所有测试函数（test case function）都必须有合理assert：
  - 就算调用函数中有合理的assert，最外层test函数也需要有
  - assert 的基本格式为 assert output == expected_output, description
  - 合理的assert示例：
    - `assert output == expected_output, description`
    - `assert output == 0x123, description`
    - `assert output['c'] == 0x456, description`
    - `assert output['sig'] == 2, description`
  - 不合理assert示例：
    - `assert output is not None, description`
    - `assert hasattr(output, "sig"), description`
    - `assert "sig" in output, description`
    - `assert isinstance(output, int), description`
- 一个检测点最好对应一个测试函数（如果可以的话）
- 如果测试用例和多个功能点相关，则需要调用多次 mark_function 分别进行标记
- 功能尽量单一
- 触发bug需要是确定性的，不是随机的
