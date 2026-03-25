
# DUT 随机测试用例编写指南

## 概述

随机测试用例是暴露DUT设计缺陷的重要手段

## 基本结构

需要用ucagent库提供的函数`repeat_count`来控制测试用例的循环测试次数，具体示例如下：

```python
from {DUT}_api import * # 重要，必须用 import *， 而不是 import env，不然会出现 dut 没定义错误
import ucagent
# 根据需要导入其他必要模块
import random
# ...

def test_random_basic_functionality(env):
    """测试基本功能的描述

    Args:
        env: Env fixture实例，由pytest自动注入
    """    
    # 0. 覆盖率标记（必须）
    env.dut.fc_cover["FG-ARITHMETIC"].mark_function("FC-ADD",test_basic_functionality, ["CK-BASIC1", "CK-BASIC2"])
    # 如果该用例和多个功能点相关，则需要多次调用 mark_function 分别进行功能点-检查点标记
    # Eg:
    # env.dut.fc_cover["FG-XXXXXX"].mark_function("FC-YYYYYY", test_basic_functionality, ["CK-AAA1", "CK-AAA2"])

    # 1. 获取循环测试次数
    N = ucagent.repeat_count()
    for i in range(N):
        # 2. 测试数据准备
        input_a = random.randint(0, 100)
        input_b = random.randint(0, 100)
        expected_result = input_a + input_b

        # 3. 调用API执行操作
        actual_result = api_{DUT}_operation(env, input_a, input_b)

        # 4. 断言验证结果
        assert actual_result == expected_result, f"预期: {expected_result}, 实际: {actual_result}"

# 其他随机测试用例
```


## 设计原则
- 必须通过`ucagent.repeat_count()`获取循环次数
- 测试用例名称的命名格式为`test_random_<test_name>`
- 测试文件名称的命名格式为`test_{DUT}_random_<group_name>.py`
- 测试用例应该只依赖DUT提供的API接口
- 传递给DUT API的值，必须是符合API要求，合理且随机的
- 根据需要可以增加边界情况输入的概率
- 避免直接操作DUT的底层实现细节
- 通过API封装保证测试用例的稳定性和可维护性
- 每个测试用例都应该有合理的assert判断：assert output==expected_output, assert_message
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
