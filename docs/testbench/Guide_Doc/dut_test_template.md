
# 空测试用例（测试用例模板）

空测试用例，在本测试环境中也称之为"测试用例模板"，其作用是将需要测试的内容先明确下来，统计工作量和测试进度，以及为后续测试用例的"并发生成（填充）"提供基础。

空测试用例的主要作用包括：

1. **工作量评估**：通过预先定义测试用例结构，便于估算测试开发的工作量
2. **进度跟踪**：明确每个测试用例的实现状态，便于跟踪测试开发进度
3. **结构规范**：确保所有测试用例都遵循统一的结构和命名规范
4. **并发开发**：多人协作时可以并行开发不同的测试用例
5. **覆盖率规划**：提前明确测试覆盖的功能点和检查点

## 空测试用例示例

### 基本示例

```python
def test_basic_addition(env):
    """测试基本加法功能"""
    env.dut.fc_cover["FG-ADD"].mark_function("FC-BASIC", test_basic_addition, ["CK-NORM", "CK-ZERO", "CK-CIN"])

    # TASK: 实现基本加法测试逻辑
    # Step:
    # - 测试 1 + 2 = 3, 0 + 0 = 0, 带进位等基础场景

    assert False, "Not implemented"
```

### 复杂示例

```python
def test_overflow_scenarios(env):
    """测试溢出场景"""
    env.dut.fc_cover["FG-ADD"].mark_function("FC-OVERFLOW", test_overflow_scenarios, ["CK-OVERFLOW_NO_CIN", "CK-OVERFLOW_WITH_CIN"])
    # TASK: 实现溢出测试逻辑
    # Step:
    # 1. 测试最大值 + 1 的溢出情况
    # 2. 测试带进位的溢出情况
    # 3. 验证溢出标志位的正确性

    assert False, "Not implemented"
```

## 空测试用例的必要组成部分

一个标准的空测试用例必须包含以下部分：

### 1. 用例名称和参数

- **函数名称**：必须以 `test_` 开头，符合pytest规范
- **命名规范**：使用有意义的名称，清晰表达测试意图
- **参数规范**：固定使用 `env` 作为参数，表示被测设计单元环境

```python
def test_boundary_conditions(env):  # ✓ 良好的命名
def test_func_a(env):              # ✗ 命名不明确
```

### 2. 用例注释

- **docstring**：使用三引号字符串描述测试目的
- **TASK注释**：详细说明需要实现的测试逻辑
- **测试场景**：列出具体需要验证的测试场景

```python
def test_edge_cases(env):
    """测试边界情况和特殊场景
    
    测试内容：
    1. 输入为最大值时的行为
    2. 输入为最小值时的行为  
    3. 输入为零时的特殊处理
    """
    # TASK: 实现边界测试逻辑
```

### 3. 测试点反标

- **语法**：`env.dut.fc_cover["功能组"].mark_function("功能点", 测试函数, ["检查点列表"])`
- **功能组**：对应功能描述文档中的 `<FG-*>` 标签
- **功能点**：对应功能描述文档中的 `<FC-*>` 标签  
- **检查点**：对应功能描述文档中的 `<CK-*>` 标签

```python
# 标记该测试覆盖了FG-ADD组中的FC-BASIC功能点的三个检查点
env.dut.fc_cover["FG-ADD"].mark_function("FC-BASIC", test_basic_addition, ["CK-NORM", "CK-ZERO", "CK-CIN"])
```

### 4. 强制失败断言

- **目的**：确保空模板不会被意外当作已实现的测试执行
- **语法**：`assert False, "Not implemented"`
- **位置**：放在函数的最后

## 从空测试用例到完整实现

### 实现步骤

1. **分析需求**：理解测试用例需要验证的功能点
2. **设计测试数据**：准备测试输入和期望输出
3. **调用API**：使用DUT提供的API函数进行操作
4. **添加断言**：验证实际结果与期望结果一致
5. **移除失败断言**：删除 `assert False` 语句

### 完整实现示例

```python
def test_basic_addition(env):
    """测试基本加法功能"""
    env.dut.fc_cover["FG-ADD"].mark_function("FC-BASIC", test_basic_addition, ["CK-NORM", "CK-ZERO", "CK-CIN"])
    
    # 测试基本加法: 1 + 2 = 3
    sum_val, carry = api_adder_add(env, 1, 2, 0)
    assert sum_val == 3
    assert carry == 0
    
    # 测试零输入: 0 + 0 = 0  
    sum_val, carry = api_adder_add(env, 0, 0, 0)
    assert sum_val == 0
    assert carry == 0
    
    # 测试带进位: 1 + 2 + 1 = 4
    sum_val, carry = api_adder_add(env, 1, 2, 1)  
    assert sum_val == 4
    assert carry == 0
        
```

## 最佳实践

### 1. 确保反标成功

建议在函数的最开始进行反标， 确保覆盖率标记始终执行：

```python
def test_example(env):
    """测试示例"""  
    env.dut.fc_cover["FG-X"].mark_function("FC-Y", test_example, ["CK-Z"])
    # 测试逻辑
    # ...
    assert False, "Not implemented"  # 空模板阶段保留
```

### 2. 模块化导入

遵循项目约定导入必要的API：

```python
from {DUT}_api import *  # 导入DUT相关的API函数, 必须用 import *， 而不是 import env，不然会出现 dut 没定义错误
```

### 3. 覆盖率完整性

- 确保每个测试用例至少覆盖一个检查点
- 确保每个检查点至少被一个测试用例覆盖
- 避免重复标记相同的检查点
