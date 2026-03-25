# DUT Mock 组件指南

本文档给出在验证环境中设计与实现 Mock 组件的规范与实例，配合 `dut_fixture.md` 一起使用。

目标：
- 为 DUT 的上下游依赖（存储器、总线、外设、参考模型等）提供可控、可观测、可复现的替代实现
- 便于在 env fixture 中通过 `Step()`方法统一驱动 Mock 组件，简化测试编写

## 何时需要 Mock

- DUT 依赖外部模块（如存储器、总线/通道、外设、协议端点）而无法在测试中直接提供真实实现
- 需要可控的时序、延迟、吞吐、背压、错误注入等行为
- 需要参考模型（Golden Model）用于结果比对


## 设计原则

1) 可配置：
- 通过构造参数或属性设置延迟、带宽、FIFO 深度、随机抖动、错误注入概率等

2) 明确接口：
- 对外暴露清晰的方法：如 `control(data)`, `read(addr)`, `write(addr, data)`, `push(...)`, `pop(...)`，或协议事务接口

3) 时序统一：
- 将所有时序行为集中在 `on_clock_edge(cycles, ...)` 中进行处理，通过 `dut.StepRis(...)` 或 `dut.StepFal(...)` 注册后，由 `dut.Step(...)` 方法进行驱动
- 组合电路场景仍复用上述流程，以保持统一

4) 资源管理：
- 提供 `reset()`/`clear()` 用于状态清空，必要时提供 `close()` 或在 env 清理阶段移除回调

5) 可观测性：
- 提供统计计数器、最近 N 次事务的日志、调试开关；遇到协议违规能输出提示

6) 可复现性：
- 涉及随机行为时接受 `seed`，并保证同一 seed 下行为可复现


## 生命周期与对接点（在 env 内集成，请参考env相关文档）

典型流程：
1. 在 env 的 `__init__` 中实例化 Mock组件，然后通过mock组件的`bind`方法绑定dut
2. 需要在Mock组件的`bind`方法中进行引脚绑定和`on_clock_edge`回调注册
3. 需要时在 env 暴露一些便捷方法，转调到 Mock（如 `mem_read`, `mem_write`）

注意：默认 `StepRis`/`StepFal` 回调按注册顺序执行。一般先注册覆盖率采样，再注册 Mock 驱动，最后注册监控/日志回调，便于保证正确的执行顺序。


## 最小可用 Mock 模板

```python
class MockTemplate:
	"""最小可用 Mock 模板

	Args:
		latency: 周期延迟，控制请求到响应的最小周期
		enable_log: 是否打印关键日志
	"""

	def __init__(self, io_prefix, latency=0, enable_log=False):
		self.latency = latency
		self.enable_log = enable_log
		self._cycles = 0
		self._pending = []  # [(ready_cycle, payload), ...]
		self.stats = {
			'req': 0,
			'rsp': 0,
		}
		self.io = IOBundle.from_prefix(io_prefix)

	def bind(self, dut):
		"""主要功能：1 引脚绑定，2 注册回调"""
		self.dut = dut
		self.io.bind(dut)
		self.dut.StepRis(self.on_clock_edge)

	def reset(self):
		"""清空内部状态。"""
		self._cycles = 0
		self._pending.clear()
		for k in self.stats:
			self.stats[k] = 0

	def request(self, payload):
		"""上层或 env 主动投递一个请求。"""
		self.stats['req'] += 1
		self._pending.append((self._cycles + self.latency, payload))

	def on_clock_edge(self, cycles):
		"""时钟上升沿回调，由 dut.StepRis 驱动。"""
		self._cycles = cycles
		# 到期事务产生响应/驱动信号
		ready = [p for p in self._pending if p[0] <= cycles]
		if ready:
			for _, payload in ready:
				self._do_response(payload)
				self.stats['rsp'] += 1
			# 移除已完成事务
			self._pending = [p for p in self._pending if p[0] > cycles]

	def _do_response(self, payload):
		"""将响应写回到绑定的接口（示例）。"""
		if self.enable_log:
			print(f"[MockTemplate] cycles={self._cycles}, payload={payload}")
		# 示例：如果 io 有 'valid'/'data' 信号可驱动
		if hasattr(self.io, 'valid'):
			self.io.valid.value = 1
		if hasattr(self.io, 'data'):
			self.io.data.value = payload
```


## 示例一：AXI4-Lite 内存模型 Mock（简化）

该示例展示如何用 Bundle 封装 DUT 端口，并实现带可配置延迟的读写存储。

```python
from toffee import Bundle, Signals


class AXI4LiteBundle(Bundle):
	# 极简化信号：valid/ready/data 仅作示范，真实 AXI4-Lite 请按协议完整定义
	aw_valid, aw_ready, aw_addr = Signals(3)
	w_valid, w_ready, w_data = Signals(3)
	b_valid, b_ready, b_resp = Signals(3)
	ar_valid, ar_ready, ar_addr = Signals(3)
	r_valid, r_ready, r_data, r_resp = Signals(4)


class MockAXI4LiteMemory:
	"""简化的 AXI4-Lite 内存模型

	Args:
		read_latency: 读响应延迟（周期）
		write_latency: 写响应延迟（周期）
		size: 内存大小（字）
		init_value: 默认初始化值
	"""

	def __init__(self, io_prefix, read_latency=1,
	write_latency=0, size=1024, init_value=0):
		self.read_latency = read_latency
		self.write_latency = write_latency
		self.mem = [init_value] * size
		self._cycles = 0
		self._pending_w = []  # (ready_cycle, addr, data)
		self._pending_r = []  # (ready_cycle, addr)
		self.stats = {'read': 0, 'write': 0}
		self.axi = AXI4LiteBundle.from_prefix(io_prefix)

	def bind(self, dut):
		# 通过前缀绑定：例如 DUT 端口以 io_axi_ 开头
		self.dut = dut
		self.axi.bind(dut)
		self.dut.StepRis(self.on_clock_edge)

	def reset(self):
		self._cycles = 0
		self._pending_w.clear()
		self._pending_r.clear()
		self.stats = {'read': 0, 'write': 0}

	def on_clock_edge(self, cycles):
		self._cycles = cycles

		# 写地址/数据握手（极简示范）
		if self.axi.aw_valid.value and self.axi.w_valid.value:
			addr = self.axi.aw_addr.value
			data = self.axi.w_data.value
			self._pending_w.append((cycles + self.write_latency, addr, data))
			self.stats['write'] += 1
			# 简化 ready 逻辑
			self.axi.aw_ready.value = 1
			self.axi.w_ready.value = 1

		# 处理到期写响应
		due_w = [x for x in self._pending_w if x[0] <= cycles]
		if due_w:
			for _, addr, data in due_w:
				if 0 <= addr < len(self.mem):
					self.mem[addr] = data
			# 返回写响应
			self.axi.b_valid.value = 1
			self.axi.b_resp.value = 0  # OKAY
			self._pending_w = [x for x in self._pending_w if x[0] > cycles]

		# 读地址握手
		if self.axi.ar_valid.value:
			addr = self.axi.ar_addr.value
			self._pending_r.append((cycles + self.read_latency, addr))
			self.stats['read'] += 1
			self.axi.ar_ready.value = 1

		# 处理到期读响应
		due_r = [x for x in self._pending_r if x[0] <= cycles]
		if due_r:
			for _, addr in due_r:
				data = self.mem[addr] if 0 <= addr < len(self.mem) else 0
				self.axi.r_valid.value = 1
				self.axi.r_data.value = data
				self.axi.r_resp.value = 0  # OKAY
			self._pending_r = [x for x in self._pending_r if x[0] > cycles]
```

在 env 中集成：

```python
class DUTEnv:
	def __init__(self, dut):
		self.dut = dut
		self.mem = MockAXI4LiteMemory(
			io_prefix="io_axi_",
			read_latency=2, write_latency=1)
		self.mem.bind(dut)
		self.mem.reset()

	def Step(self, c=1):
		return self.dut.Step(c)
```


## 常见问题排查

1) 回调不生效：
- 是否调用了 `dut.StepRis`或者`dut.StepFal`进行了回调注册？是否实际推进了 `dut.Step(...)`？
- 组合电路仅 `RefreshComb` 不会触发 StepRis，必须 `Step(1)`

1) 死锁/事务不前进：
- 检查 valid/ready 或 req/resp 的握手机制是否对齐；确认回调里是否正确更新握手信号

1) 随机导致不可复现：
- 保证所有随机路径受相同 `seed` 控制；在日志中记录 `seed`


## 开发检查清单（Checklist）

- [ ] Mock 类名以 `Mock` 开头，并位于 `{OUT}/tests/{DUT}_mock_<MockComponentName>.py`
- [ ] 需要有函数：`on_clock_edge(self, cycles)`和`bind(self, dut)`
- [ ] 提供必要的配置参数（延迟、深度、背压等）与统计信息
- [ ] 关键方法和类提供中文注释，便于协作与审阅
- [ ] 通过一个最小单元测试/示例验证 Mock 行为正确


参考文档：
- `Guide_Doc/dut_fixture.md`
