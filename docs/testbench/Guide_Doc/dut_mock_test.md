# DUT Mock 组件测试

Mock 组件是验证环境的重要组成部分（参见 `dut_mock.md`）。在将其集成到 ENV 中用于驱动 RTL 之前，我们需要对其本身的功能进行独立的单元测试（Self-Test）。

由于 Mock 组件通常依赖 DUT 的信号接口，为了在没有真实 RTL 仿真环境的情况下测试它，我们需要模拟一个“假的 DUT 环境”，即 **Mock DUT**。

## 什么是 Mock DUT

Mock DUT 是一个纯 Python 对象，它自动复制了真实 DUT Python 接口（通常是 `DUT{{DUT}}` 类）中定义的所有信号（Pin）。
- 它不连接真实的仿真器（如 Verilator/VCS）。
- 它的信号是 XPin 对象，读写时应通过 `.value` 访问（不要直接覆盖 pin 对象）。
- 它提供了 `Step(n)` 方法，用于推进虚拟的时间，触发注册的时钟回调函数。

## 创建 Mock DUT

UCAgent 提供了 `get_mock_dut_from(dut_class)` 方法来反射生成 Mock DUT 类。
请在文件 `{DUT}_api.py` 中定义一个 `pytest fixture` 来提供 `mock_dut` 实例。

```python
import pytest
import ucagent
from {{DUT}} import DUT{{DUT}}  # 导入真实的 DUT 接口类

@pytest.fixture(scope="function")
def mock_dut():
    """
    创建一个全新的 Mock DUT 实例。
    scope="function" 确保每个测试用例运行时都拥有一个独立、干净的 DUT 状态。
    """
    return ucagent.get_mock_dut_from(DUT{{DUT}})
```

## 命名与结构规范

为了配合 UCAgent 的自动化回归流程，建议遵循以下命名规范：

- **测试文件名**：`test_{{DUT}}_mock_<ComponentName>.py`
- **测试函数名**：`test_api_{{DUT}}_mock_<FeatureName>`
- **参数要求**：测试函数的第一个参数必须为 `mock_dut` fixture。

## 测试步骤与方法

测试 Mock 组件的核心流程如下：

1. **实例化**：创建待测的 Mock 组件实例，配置参数（如 io prefix）。
2. **绑定**：调用 Mock 组件的 `bind(mock_dut)`，将其信号连接到 Mock DUT。
   - *关键点*：Mock 组件`bind`方法内部应当通过 `dut.StepRis(self.on_clock_edge)` 注册时钟回调。
3. **驱动与激励**：
    - **Scenario A (Mock 作为 Slave)**：测试代码向 `mock_dut` 输出信号赋值（模拟 RTL 发起请求），验证 Mock 是否正确响应（assert `mock_dut` 输入信号）。
    - **Scenario B (Mock 作为 Master)**：调用 Mock 组件的 API（如 `send_req`），验证 Mock 是否正确驱动 `mock_dut` 的信号。
4. **推进时间**：调用 `mock_dut.Step(cycles)` 模拟时钟流逝，触发 Mock 的 `on_clock_edge` 逻辑。
5. **验证**：断言检查信号状态、内部 `stats` 计数器等。

### 信号读写规范

Mock DUT 的 pin 是 XPin 对象，读写必须通过 `.value`：

- 写入：`mock_dut.req_valid.value = 1`
- 读取：`assert mock_dut.req_ready.value == 1`

避免直接赋值 `mock_dut.req_valid = 1`，否则会覆盖 pin 对象，导致回调与绑定失效。

### 完整示例

以下示例演示了如何测试一个简单的内存 Mock 组件（Slave 角色）。

```python
# file: test_ICache_mock_Mem.py
import pytest
from ICache_api import * # 包含 mock_dut 的 pytest fixture 定义
from ICache_mock_Mem import MockMem # 待测组件

def test_api_ICache_mock_write_interaction(mock_dut):
    # 1. Setup & Bind
    # 实例化 Mock，设定延迟为 2 个周期
    mem = MockMem(io_prefix="io_mem_", latency=2)
    mem.bind(mock_dut)
    # 此时 mem 内部应已执行 Bundle 的绑定与上升沿/下降沿回调函数注册
    # 如果没有注册，请在外部进行回调注册: eg: mock_dut.StepRis(mem.on_clock_edge)

    # 2. Stimulate (模拟 RTL 行为)
    # 就像在 Verilog 中驱动信号一样，给 mock_dut 的属性赋值
    mock_dut.mem_req.value = 1
    mock_dut.mem_addr.value = 0x8000
    mock_dut.mem_wdata.value = 0xAA
    mock_dut.mem_we.value = 1

    # 3. Check Behavior over Time
    
    # [Cycle 0 -> 1]
    mock_dut.Step(1)
    # 延迟未到，ack 应该为 0
    assert mock_dut.mem_ack.value == 0

    # [Cycle 1 -> 2]
    mock_dut.Step(1)
    # 延迟满足(latency=2)，Mock 应该产生响应
    assert mock_dut.mem_ack.value == 1
    
    # 4. Check Internal State (Observability)
    # 验证 Mock 内部是否统计了这次事务
    assert mem.stats['write_ops'] == 1
    
    # 5. Clear Stimulus
    mock_dut.mem_req.value = 0
    mock_dut.Step(1)
    assert mock_dut.mem_ack.value == 0
```

### Scenario B 示例（Mock 作为 Master）

```python
# 假设 Mock 组件提供 send_req()，会驱动 req_valid/req_data
def test_api_ICache_mock_master_drive(mock_dut):
    mem = MockMem(io_prefix="io_mem_", latency=1)
    mem.bind(mock_dut)

    mem.send_req(addr=0x100, data=0x55)
    mock_dut.Step(1)

    assert mock_dut.mem_req.value == 1
    assert mock_dut.mem_addr.value == 0x100
    assert mock_dut.mem_wdata.value == 0x55
```

## 常见测试点

结合 `dut_mock.md` 中的设计原则，建议覆盖以下测试点：

1. **基本协议交互**：Req/Ack 握手，数据传输正确性。
2. **时序参数配置**：验证不同的 `latency` 参数设定下，响应周期是否精准。
3. **背压 (Backpressure)**：如果协议支持，验证当 RTL 侧 `ready=0` 时，Mock 能够正确保持数据不丢失。
4. **复位逻辑**：调用 `mock.reset()` 后，检查内部队列和状态是否清零。
5. **错误注入**：如果有错误注入功能（如 `inject_error=True`），验证是否产生了预期的错误响应。

## 调试建议

- Mock DUT 的所有信号都可以打印日志来随时查看状态。
- 如果 Mock 组件内部有 `print` 或日志输出，在执行 pytest 时使用 `-s` 选项可以看到实时输出。

MockDUT 提供的方法：

```python
def Step(self, i:int = 1): # 模拟时钟驱动
def StepRis(self, callback, args=(), kwargs={}): # 设置时钟上升沿回调
def StepFal(self, callback, args=(), kwargs={}): # 设置时钟下降沿回调
```
