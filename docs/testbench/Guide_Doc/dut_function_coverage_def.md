
# DUT 功能覆盖率定义指南

## 概述

功能覆盖率（Functional Coverage）是验证过程中的重要指标，用于衡量测试用例对设计功能的覆盖程度。本文档介绍如何使用 `toffee` 和 `toffee_test` 库定义和实现功能覆盖率。

## 核心概念

### 覆盖率层次结构
- **覆盖组（CovGroup）**：对应功能分组 `<FG-*>`，包含相关功能点的集合
- **功能点（Watch Point）**：对应功能点 `<FC-*>`，包含具体的检查点集合  
- **检查点（Bins）**：对应检查点 `<CK-*>`，具体的功能检查条件

### 工作流程
1. **定义覆盖组**：为每个功能分组创建CovGroup
2. **添加检查点**：为每个功能点定义具体的检查条件
3. **关联测试用例**：将测试函数与检查点建立关联
4. **采样统计**：在测试执行过程中进行覆盖率采样
5. **结果报告**：生成覆盖率统计报告

## 必需的接口函数

### 主接口函数

```python
def get_coverage_groups(dut) -> List[CovGroup]:
    """获取所有功能覆盖组
    
    Args:
        dut: DUT实例，可为None（用于获取覆盖组结构）
        
    Returns:
        List[CovGroup]: 功能覆盖组列表
    """
```

**注意**：CovGroup类型为 toffee.funcov.CovGroup

## 常用功能覆盖接口

### 1. 创建覆盖组

```python
import toffee.funcov as fc

# 创建功能覆盖组，名称对应功能描述文档中的<FG-*>标签
funcov_arithmetic = fc.CovGroup("FG-ARITHMETIC")
funcov_logic = fc.CovGroup("FG-LOGIC") 
funcov_control = fc.CovGroup("FG-CONTROL")
```

### 2. 添加检查点

```python
CovGroup.add_watch_point(target, bins: dict, name: str)
```

**参数说明：**
- `target`：检查函数的输入参数
  - ⚠️ **注意**：不要传递值类型参数（如 `dut.a.value`），会导致值固化
  - ✅ **推荐**：传递引用类型参数（如 `dut` 或 `dut.a`）
- `bins`：字典类型，key为检查点名称，value为检查函数 `func(target) -> bool`
- `name`：功能点名称，对应 `<FC-*>` 标签

### 3. 采样和统计

必须要有功能覆盖组采样，即调用其sample()方法, 如何不在StepRis/StepFal的回调函数中采样，则需要在test function中手动调用，否则无法统计覆盖率导致失败。

举例：
```python
# 手动采样
covgroup.sample()

# 在dut.Step执行过程中自动采样（推荐在fixture中设置）
dut.StepRis(lambda _: [g.sample() for g in coverage_groups])
```

### 4. 关联测试用例

```python
CovGroup.mark_function(watch_point_name: str, test_function_or_list, bins=[])
```

**参数说明：**
- `watch_point_name`：功能点名称（如 "FC-ADD"）
- `test_function_or_list`：测试函数或函数列表  
- `bins`：指定的检查点列表（可选）

### 5. 生成测试报告

```python
from toffee_test.reporter import set_func_coverage

# 在fixture的teardown阶段调用
set_func_coverage(request, coverage_groups)
```

## 完整实现示例

### 基础示例

```python
import toffee.funcov as fc

# 1. 创建覆盖组
funcov_arithmetic = fc.CovGroup("FG-ARITHMETIC")
funcov_logic = fc.CovGroup("FG-LOGIC")
funcov_group = [funcov_arithmetic, funcov_logic]

def init_coverage_group_arithmetic(g, dut):
    """初始化算术运算覆盖组"""
    # 加法功能点
    g.add_watch_point(dut,
        {
            "CK-BASIC": lambda x: x.a.value + x.b.value == x.sum.value,
            "CK-OVERFLOW": lambda x: x.cout.value == 1,
            "CK-ZERO": lambda x: (x.a.value == 0) and (x.b.value == 0) and (x.sum.value == 0),
            "CK-CIN": lambda x: x.cin.value == 1 and (x.a.value + x.b.value + 1) & 0xFFFFFFFF == x.sum.value,
        },
        name="FC-ADD")
    
    # 减法功能点
    g.add_watch_point(dut,
        {
            "CK-BASIC": lambda x: (x.a.value - x.b.value) & 0xFFFFFFFF == x.result.value,
            "CK-BORROW": lambda x: x.borrow_out.value == 1,
            "CK-UNDERFLOW": lambda x: x.a.value < x.b.value and x.underflow.value == 1,
        },
        name="FC-SUB")

def init_coverage_group_logic(g, dut):
    """初始化逻辑运算覆盖组"""
    g.add_watch_point(dut,
        {
            "CK-AND": lambda x: (x.a.value & x.b.value) == x.result.value,
            "CK-OR": lambda x: (x.a.value | x.b.value) == x.result.value, 
            "CK-XOR": lambda x: (x.a.value ^ x.b.value) == x.result.value,
            "CK-NOT": lambda x: (~x.a.value & 0xFFFFFFFF) == x.result.value,
        },
        name="FC-BITWISE")

def init_function_coverage(dut, cover_group):
    """初始化所有功能覆盖"""
    coverage_init_map = {
        "FG-ARITHMETIC": init_coverage_group_arithmetic,
        "FG-LOGIC": init_coverage_group_logic,
    }
    
    for g in cover_group:
        init_func = coverage_init_map.get(g.name)
        if init_func:
            init_func(g, dut)
        else:
            print(f"警告：未找到覆盖组 {g.name} 的初始化函数")

def get_coverage_groups(dut):
    """获取功能覆盖组列表"""
    init_function_coverage(dut, funcov_group)
    return funcov_group
```

### 高级示例：复杂检查条件

```python
def init_coverage_group_memory(g, dut):
    """内存操作覆盖组"""
    
    def check_cache_hit(x):
        """检查缓存命中"""
        return x.cache_hit.value == 1 and x.mem_ready.value == 1
        
    def check_cache_miss(x):
        """检查缓存缺失"""
        return x.cache_hit.value == 0 and x.mem_access.value == 1
        
    def check_write_through(x):
        """检查写穿模式"""
        return (x.write_enable.value == 1 and 
                x.cache_write.value == 1 and 
                x.mem_write.value == 1)
    
    g.add_watch_point(dut,
        {
            "CK-READ-HIT": check_cache_hit,
            "CK-READ-MISS": check_cache_miss,
            "CK-WRITE-THROUGH": check_write_through,
            "CK-WRITE-BACK": lambda x: x.dirty.value == 1 and x.evict.value == 1,
        },
        name="FC-CACHE")
```

### 边界条件覆盖示例

```python
def init_coverage_group_boundary(g, dut):
    """边界条件覆盖"""
    
    # 数值边界
    MAX_VAL = (1 << 32) - 1
    MIN_VAL = 0
    
    g.add_watch_point(dut,
        {
            "CK-MAX-A": lambda x: x.a.value == MAX_VAL,
            "CK-MAX-B": lambda x: x.b.value == MAX_VAL,
            "CK-MAX-BOTH": lambda x: x.a.value == MAX_VAL and x.b.value == MAX_VAL,
            "CK-MIN-A": lambda x: x.a.value == MIN_VAL,
            "CK-MIN-B": lambda x: x.b.value == MIN_VAL,
            "CK-MIN-BOTH": lambda x: x.a.value == MIN_VAL and x.b.value == MIN_VAL,
        },
        name="FC-BOUNDARY")
```

## 最佳实践

### 1. 参数传递注意事项

```python
# ❌ 错误：传递值类型，会导致值固化
g.add_watch_point(dut.a.value,  # 这是一个int值，不会更新
    {"CK-TEST": lambda x: x == 5}, 
    name="FC-BAD")

# ✅ 正确：传递引用类型
g.add_watch_point(dut,  # 传递整个dut对象
    {"CK-TEST": lambda x: x.a.value == 5}, 
    name="FC-GOOD")

# ✅ 正确：传递信号引用
g.add_watch_point(dut.a,  # 传递信号对象
    {"CK-TEST": lambda x: x.value == 5}, 
    name="FC-BETTER")
```

### 2. 采样策略

#### 自动采样（推荐）
```python
# 在fixture中设置，适用于大多数情况
dut.StepRis(lambda _: [g.sample() for g in func_coverage_group])
```

#### 手动采样
```python
# 在API函数中采样，适用于特定操作
def api_complex_operation(env, ...):
    # 执行操作
    result = perform_operation(env.dut, ...)
    
    # 手动采样
    for g in get_coverage_groups():
        g.sample()
    
    return result
```

### 3. 覆盖组组织

```python
# 按功能模块组织
funcov_cpu_core = fc.CovGroup("FG-CPU-CORE")
funcov_memory_subsystem = fc.CovGroup("FG-MEMORY") 
funcov_io_controller = fc.CovGroup("FG-IO")

# 按抽象层次组织
funcov_instruction_level = fc.CovGroup("FG-ISA")
funcov_microarch_level = fc.CovGroup("FG-MICROARCH")
funcov_physical_level = fc.CovGroup("FG-PHYSICAL")
```

### 4. 错误处理

```python
def init_function_coverage(dut, cover_group):
    """带错误处理的覆盖率初始化"""
    for g in cover_group:
        try:
            init_func_name = f"init_coverage_group_{g.name.lower().replace('-', '_')}"
            init_func = globals().get(init_func_name)
            if init_func:
                init_func(g, dut)
            else:
                print(f"警告：未找到 {g.name} 的初始化函数")
        except Exception as e:
            print(f"错误：初始化覆盖组 {g.name} 失败 - {e}")
            # 可以选择继续或抛出异常
            continue
```

### 5. 调试和验证

```python
def debug_coverage_groups():
    """调试覆盖组定义"""
    groups = get_coverage_groups()
    
    for g in groups:
        print(f"覆盖组: {g.name}")
        print(f"检查点数量: {len(g.cov_points)}")
        
        for point_name, point in g.cov_points.items():
            print(f"  功能点: {point_name}")
            print(f"    检查数量: {len(point.bins)}")
            for bin_name in point.bins.keys():
                print(f"      检查点: {bin_name}")
```

## 注意事项

### 性能考虑
- 覆盖率采样会影响仿真性能，在性能敏感场景下考虑降低采样频率
- 复杂的检查函数会增加采样开销，适当优化检查逻辑

### 调试建议
- 使用 `print` 或日志记录调试覆盖率采样
- 定期检查覆盖率统计，确保采样正常工作
- 使用简单的检查条件验证覆盖率框架工作正常

### 维护性
- 保持检查点命名的一致性和可读性
- 定期回顾和更新覆盖率定义
- 文档化复杂的检查逻辑和边界条件
