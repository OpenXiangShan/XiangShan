# NOOP-FPU

一个完全符合IEEE754-2008标准的混合精度RISCV-FPU

## 开启/关闭FPU

将`HasNOOPParameter`中的`HasFPU`定义为`true`/`false`即可

## FPU单元测试

```
cd deug
make fputest FPU_TEST_ARGS=-Pn
```
`n`为线程数