# NOOP-FPU

一个完全符合IEEE754-2008标准的混合精度(Float/Double)RISCV-FPU

FPU除法/开方模块使用了SRT-4算法，采用多周期设计，其余部件均为流水线结构，具体情况如下：

| 功能部件  | 流水级数 | 
| :----:   | :----: |
|FMA       | 5    |
|F32toF64  | 2    |
|F64toF32  | 2    |
|FCMP      | 2    |
|FloatToInt| 2    |
|IntToFloat| 2    |  

不同功能部件之间相互独立，不共享硬件资源；
同一部件内部，双精度/单精度运算共享硬件资源。
 
FPU中所有部件都已通过
berkeley-testfloat和riscv-tests中的rvd/rvf测试，
在axu3cg上运行频率超过200MHz

## 开启/关闭FPU

将`HasNOOPParameter`中的`HasFPU`定义为`true`/`false`即可

## FPU单元测试

### 使用berkeley-testfloat测试FPU中的所有模块：

```
cd deug
make fputest FPU_TEST_ARGS=-Pn
```
`n`为线程数


### 自定义测试：

在`src/test/fpu/FPUSubModuleTester`中修改测试配置

```
配置格式
case class FpuTest
(
  name: String,
  roundingModes: Seq[UInt],
  backend: String = "verilator", 
  writeVcd: Boolean = false,
  pipeline: Boolean = true
)
```

`backend`可选`verilator`/`treadle`/`vcs`，
`verilator`编译较慢但仿真运行速度最快；
`treadle`输出格式较为整齐，适合debug

`pipeline`为`false`时每执行完一个测例才开始输入下一个






