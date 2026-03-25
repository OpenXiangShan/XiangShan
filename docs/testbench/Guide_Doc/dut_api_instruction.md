
# API 设计指南

本文档介绍如何为DUT（Design Under Test）创建API和测试环境接口，确保测试的稳定性和可维护性。

## DUT API 设计

### 设计原则

DUT API的设计应遵循以下原则：

1. **封装性**：隐藏底层实现细节和时序
2. **稳定性**：接口变更不应影响测试用例
3. **通用性**：API应覆盖主要功能，避免过度细化
4. **一致性**：命名和参数传递保持统一风格
5. **可测试性**：便于单元测试和集成测试
6. **参数要求**：第一个参数必须为 env，最后一个参数必须为 max_cycles=default_value （default_value具体值按需要填写）

### 命名规范

API函数命名格式：`api_{DUT}_{function_name}`

- `DUT`：DUT的名称，如adder、alu、cache等
- `function_name`：具体功能名称，使用动词描述操作

```python
# 良好的命名示例
api_adder_add(env, a, b, cin, max_cycles=100)           # 加法器执行加法
api_cache_read(env, address, max_cycles=200)            # 缓存读取
api_uart_send(env, data, max_cycles=100)                # UART发送数据
api_cpu_execute(env, instruction, max_cycles=300)       # CPU执行指令

# 避免的命名
api_test_func(env, max_cycles=10)                      # 命名不明确
api_adder_do_something(env, max_cycles=20)             # 功能描述模糊
```

#### 详尽的注释

在每个API函数中都需要通过函数 DOC 编写对应的注释，遵循Google风格的docstring规范。注释应该详细、准确、可维护。

##### 注释格式要求

```python
from typing import Tuple

def api_demo_op(env, a: int, b: int, mode: str = "add", max_cycles=100) -> Tuple[int, bool]:
    """对API的功能进行详细描述，说明其作用、适用场景和注意事项

    详细描述API的工作原理、时序要求、边界条件等重要信息。
    如果有特殊的使用注意事项或限制条件，也要在这里说明。

    Args:
        env: Env 实例，必须是已初始化的 Env 实例
        a (int): 第一个操作数，取值范围[0, 2^32-1]，表示输入A的数值
        b (int): 第二个操作数，取值范围[0, 2^32-1]，表示输入B的数值
        mode (str): 操作模式，可选值为"add"/"sub"/"mul"，默认为"add"
        max_cycles (int): 最大超时Cycle

    Returns:
        Tuple[int, bool]: 包含两个元素的元组
            - result (int): 运算结果，范围取决于操作模式
            - overflow (bool): 溢出标志，True表示发生溢出

    Raises:
        ValueError: 当参数超出有效范围时抛出
        RuntimeError: 当DUT硬件故障时抛出
        TimeoutError: 当操作超时时抛出（适用于时序电路）

    Example:
        >>> result, overflow = api_demo_op(env, 100, 200, "add")
        >>> print(f"结果: {result}, 溢出: {overflow}")
        结果: 300, 溢出: False

    Note:
        - 该API适用于同步时序电路，会自动处理时钟推进
        - 连续调用时建议间隔至少1个时钟周期
    """
    # 参数验证
    if not (0 <= a <= 0xFFFFFFFF):
        raise ValueError(f"参数a超出范围: {a}")
    if not (0 <= b <= 0xFFFFFFFF):
        raise ValueError(f"参数b超出范围: {b}")
    if mode not in ["add", "sub", "mul"]:
        raise ValueError(f"无效的操作模式: {mode}")
    # 设置输入信号
    env.input_a.value = a
    env.input_b.value = b
    env.operation_mode.value = {"add": 0, "sub": 1, "mul": 2}[mode]

    # 推进电路执行一个时钟周期（组合电路一般立即有效；为流程统一可仍调用 Step(1)）
    env.Step(1)

    # 读取结果
    result = env.output_result.value
    overflow = bool(env.overflow_flag.value)

    return result, overflow
```

##### 注释结构说明

API函数的docstring应包含以下几个部分：

1. **功能描述**：简洁明了地说明API的主要功能
2. **详细说明**：补充重要的实现细节、使用场景、注意事项
3. **Args**：详细描述每个参数的含义、类型、取值范围、默认值
4. **Returns**：描述返回值的类型、含义、可能的取值
5. **Raises**：列出可能抛出的异常及其触发条件
6. **Example**：提供使用示例，帮助理解API用法
7. **Note**：补充重要的使用注意事项

##### 参数描述最佳实践

```python
def api_memory_access(env, address: int, data: Optional[int] = None,
                     read_enable: bool = True, max_cycles: int = 100) -> Union[int, None]:
    """访问DUT内存接口，支持读写操作

    Args:
        env: Env实例，必须是已初始化的memory相关Env实例
        address (int): 内存地址，范围[0x0000, 0xFFFF]，必须4字节对齐
        data (Optional[int]): 写入数据，None表示读操作，其他值表示写操作
                             写入时取值范围[0, 2^32-1]
        read_enable (bool): 读使能信号，True表示读操作，False表示写操作
                           当data不为None时，该参数被忽略
        max_cycles (int): 最大超时Cycle

    Returns:
        Union[int, None]:
            - 读操作时返回int类型的数据值
            - 写操作时返回None
    """
    pass
```

##### 类型提示的使用

```python
from typing import Union, Optional, List, Dict, Tuple, Any

def api_batch_operation(env,
                       operations: List[Dict[str, Any]],
                       config: Optional[Dict[str, Union[int, str]]] = None,
                       max_cycles: int = 100) -> List[Tuple[bool, Any]]:
    """批量执行多个操作

    Args:
        env: Env 实例
        operations (List[Dict[str, Any]]): 操作列表，每个元素为操作字典
                                          字典格式: {"type": str, "params": Dict, "id": int}
        config (Optional[Dict[str, Union[int, str]]]): 可选配置参数
                                                      键为配置名，值为配置值
        max_cycles (int): 最大超时Cycle

    Returns:
        List[Tuple[bool, Any]]: 结果列表，每个元素为(成功标志, 结果数据)的元组
    """
    pass
```

### API 实现模式

#### 1. 基础操作API

```python
def api_adder_add(env, a, b, cin=0, max_cycles=100):
    """执行加法操作
    
    Args:
        env: Env实例
        a: 操作数A 
        b: 操作数B
        cin: 进位输入，默认为0
        max_cycles: 最大超时Cycle
        
    Returns:
        tuple: (sum_result, carry_out) 求和结果和进位输出
    """
    # 设置输入
    env.a.value = a
    env.b.value = b  
    env.cin.value = cin
    
    # 推进电路（为流程统一，组合电路也可调用 Step）
    env.Step(1)
    
    # 读取结果
    return env.sum.value, env.cout.value
```

#### 2. 复杂时序API

```python
def api_cache_read(env, address, max_cycles=100):
    """从缓存读取数据
    
    Args:
        env: Env 实例
        address: 读取地址
        max_cycles: 超时周期数
        
    Returns:
        int: 读取的数据值
        
    Raises:
        TimeoutError: 读取超时
    """
    # 发起读请求
    env.addr.value = address
    env.read_enable.value = 1
    env.Step(1)
    
    # 等待响应
    cycles = 0
    while not env.data_valid.value:
        if cycles >= max_cycles:
            raise TimeoutError(f"缓存读取超时，地址: 0x{address:x}")
        env.Step(1)
        cycles += 1
    
    # 获取数据并清除请求
    data = env.data_out.value
    env.read_enable.value = 0
    env.Step(1)
    
    return data
```

#### API 实现注意

##### 按有符号数读取端口

DUT 的端口可以通过`S()`方法完成按有符号数读取，例如:

```python
    signed_value = env.addr.S() # 这样得到的是**有符号的**结果
    unsigned_value = env.addr.value # 这样得到的是无符号的结果
```

##### 读取/修改端口的指定位

DUT 的端口支持对某位进行读写，例如：

```python
   lsb = env.addr[0] # 读取addr最低位的值
   bit2 = env.addr[1] # 读取addr第1位的值
   env.addr[2] = 0 # 修改addr第2位的值
```

### API 测试

完成API编写后，需要对其进行功能测试，检验其是否满足要求。API测试主要关注单个API函数的功能正确性。

#### 测试文件组织

需要创建`test_{DUT}_api_<category>.py`的测试文件进行测试，其中：
- `{DUT}`: DUT名称，如adder、alu、cache等
- `<category>`: 功能分类，如basic、advanced等

```
tests/
├── test_adder_api_basic.py      # 基础功能测试
├── test_adder_api_advanced.py   # 高级功能测试
└── test_adder_api_edge_cases.py # 边界情况测试
```

#### 测试函数命名规范

测试函数采用`test_<api_name>[_<test_scenario>]`的命名方式：

```python
# 基础功能测试
def test_api_adder_add():
    """测试加法API的基本功能"""
    pass

# 边界条件测试
def test_api_adder_add_overflow():
    """测试加法API的溢出处理"""
    pass

# 错误处理测试
def test_api_adder_add_invalid_input():
    """测试加法API的无效输入处理"""
    pass
```

#### 测试用例编写规范

每个测试用例需要包含详细的docstring，描述测试目标、流程和预期结果：

```python
import pytest
from {DUT}_api import * # 重要，必须用 import *， 而不是 import env，不然会出现 dut 没定义错误

def test_api_adder_add_basic(env):
    """测试加法器API基础功能

    测试目标:
        验证api_adder_add函数能正确执行基本加法运算

    测试流程:
        1. 使用典型正数进行加法运算
        2. 验证结果正确性
        3. 检查进位输出

    预期结果:
        - 计算结果正确
        - 进位标志符合预期
        - 无异常抛出
    """
    # 测试典型情况
    result, carry = api_adder_add(env, 100, 200)
    assert result == 300, f"预期结果300，实际{result}"
    assert carry == 0, f"预期进位0，实际{carry}"

    # 测试带进位情况
    result, carry = api_adder_add(env, 0xFFFFFFFF, 1)
    assert result == 0, f"溢出时预期结果0，实际{result}"
    assert carry == 1, f"溢出时预期进位1，实际{carry}"

def test_api_adder_add_edge_cases(env):
    """测试加法器API边界情况

    测试目标:
        验证API在边界条件下的正确行为

    测试流程:
        1. 测试零值加法
        2. 测试最大值加法
        3. 测试单操作数为最大值的情况

    预期结果:
        - 边界值计算正确
        - 溢出检测准确
        - 特殊情况处理得当
    """
    # 零值测试
    result, carry = api_adder_add(env, 0, 0)
    assert result == 0 and carry == 0

    # 最大值测试
    max_val = 0xFFFFFFFF
    result, carry = api_adder_add(env, max_val, max_val)
    assert carry == 1, "最大值相加应产生进位"

def test_api_adder_add_error_handling(env):
    """测试加法器API错误处理

    测试目标:
        验证API对无效输入的错误处理机制

    测试流程:
        1. 传入超出范围的参数
        2. 传入错误类型的参数
        3. 验证异常类型和错误信息

    预期结果:
        - 正确抛出预期异常
        - 错误信息描述准确
        - 不会导致程序崩溃
    """
    # 测试参数超出范围
    with pytest.raises(ValueError, match="参数.*超出范围"):
        api_adder_add(env, -1, 100)

    with pytest.raises(ValueError, match="参数.*超出范围"):
        api_adder_add(env, 100, 0x100000000)

    # 测试参数类型错误
    with pytest.raises(TypeError):
        api_adder_add(env, "100", 200)
```

#### 测试数据驱动

使用pytest的参数化功能进行数据驱动测试：

```python
@pytest.mark.parametrize("a,b,expected_sum,expected_carry", [
    (0, 0, 0, 0),
    (1, 1, 2, 0),
    (100, 200, 300, 0),
    (0xFFFFFFFF, 1, 0, 1),
    (0x80000000, 0x80000000, 0, 1),
])
def test_api_adder_add_parametrized(env, a, b, expected_sum, expected_carry):
    """参数化测试加法器API

    测试目标:
        使用多组测试数据验证API的正确性

    测试数据:
        覆盖典型值、边界值、特殊值等多种情况
    """
    result, carry = api_adder_add(env, a, b)
    assert result == expected_sum, f"输入({a}, {b}): 预期和{expected_sum}，实际{result}"
    assert carry == expected_carry, f"输入({a}, {b}): 预期进位{expected_carry}，实际{carry}"

@pytest.mark.parametrize("invalid_input", [
    (-1, 100),      # 负数
    (100, -1),      # 负数
    (0x100000000, 0),  # 超出范围
    (0, 0x100000000),  # 超出范围
])
def test_api_adder_add_invalid_inputs(env, invalid_input):
    """参数化测试无效输入处理"""
    a, b = invalid_input
    with pytest.raises(ValueError):
        api_adder_add(env, a, b)
```

#### 测试要求

API测试应该覆盖以下几个方面：

1. **基础功能**：验证API的核心功能是否正确
2. **边界条件**：测试边界值和特殊值的处理
3. **错误处理**：验证异常情况的处理机制
4. **参数验证**：检查输入参数的合法性验证


## 最佳实践

### 1. 模块化组织

将相关的API函数组织在一起：

```python
# {DUT}_api.py
"""DUT API模块 - 提供高级接口函数"""

# 基础操作
def api_{DUT}_reset(env, max_cycles=100):
    """复位DUT"""
    pass

def api_{DUT}_init(env, config=None, max_cycles=100):
    """初始化DUT"""  
    pass

# 数据操作
def api_{DUT}_read(env, addr, max_cycles=100):
    """读取数据"""
    pass

def api_{DUT}_write(env, addr, data, max_cycles=100):
    """写入数据"""
    pass

# 状态查询
def api_{DUT}_status(env, max_cycles=100):
    """获取状态"""
    pass
```

### 2. 文档和类型提示

```python
from typing import Tuple, List, Optional, Dict, Any

def api_processor_execute(
    env: Any, 
    instruction: int, 
    operands: Optional[List[int]] = None,
    max_cycles: int = 1000
) -> Tuple[int, Dict[str, Any]]:
    """执行处理器指令
    
    详细描述指令执行过程和返回值含义...
    
    Args:
        env: 处理器 Env 实例
        instruction: 指令编码
        operands: 操作数列表，可选
        max_cycles: 执行超时时间（周期数）
        
    Returns:
        Tuple包含:
        - result: 执行结果
        - status: 状态信息字典
        
    Raises:
        TimeoutError: 指令执行超时
        ValueError: 指令编码无效
    """
    pass
```

### 3. 代码质量保证

#### 代码审查清单

在提交API代码前，请检查以下项目：

- [ ] **命名规范**：函数名遵循`api_{DUT}_{function}`格式
- [ ] **类型提示**：所有参数和返回值都有正确的类型标注
- [ ] **文档完整**：docstring包含所有必需部分（功能、参数、返回值、异常）
- [ ] **参数验证**：对输入参数进行合理的范围和类型检查
- [ ] **错误处理**：适当的异常处理和有意义的错误信息
- [ ] **测试覆盖**：所有API都有对应的测试用例
- [ ] **性能考虑**：没有明显的性能瓶颈
- [ ] **依赖最小**：避免不必要的外部依赖

#### 代码风格规范

```python
# 良好的API示例
def api_cache_invalidate(env, address_range: Tuple[int, int],
                        invalidate_type: str = "data", max_cycles: int = 100) -> bool:
    """使指定地址范围的缓存失效

    Args:
        env: 缓存Env实例
        address_range: 地址范围元组(start_addr, end_addr)
        invalidate_type: 失效类型，"data"或"instruction"
        max_cycles: 最大超时Cycle

    Returns:
        bool: 操作是否成功

    Raises:
        ValueError: 地址范围无效或类型不支持
    """
    start_addr, end_addr = address_range

    # 参数验证
    if start_addr < 0 or end_addr < start_addr:
        raise ValueError(f"无效的地址范围: [{start_addr:x}, {end_addr:x}]")

    if invalidate_type not in ["data", "instruction"]:
        raise ValueError(f"不支持的失效类型: {invalidate_type}")

    # 执行操作
    try:
        env.cache_invalidate_start.value = start_addr
        env.cache_invalidate_end.value = end_addr
        env.cache_invalidate_type.value = 0 if invalidate_type == "data" else 1
        env.cache_invalidate_enable.value = 1
        env.Step(1)
        env.cache_invalidate_enable.value = 0

        # 等待操作完成
        cycles = 0
        while env.cache_invalidate_busy.value and cycles < max_cycles:
            env.Step(1)
            cycles += 1

        if cycles >= max_cycles:
            raise TimeoutError("缓存失效操作超时")

        return not bool(env.cache_invalidate_error.value)

    except Exception as e:
        raise RuntimeError(f"缓存失效操作失败: {e}")
```


### 其他说明

- 需要在 `{DUT}_api.py` 中同时实现所有fixture 与 API，编写用例时只要 `from {DUT}_api import *` (必须用 import *， 而不是 import env，不然会出现 dut 没定义错误)
- API 注释应当尽可能的详尽
- 在 fixture env 中需要做好 DUT 实例的生命周期管理
