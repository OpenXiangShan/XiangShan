# V3 memblock RTL 规则 profile

## 版本事实

| 项目 | V3 取值 |
|---|---|
| 构建系统 | `build.mill` |
| Chisel 版本 | `7.3.0` |
| 已验证 firtool | `/nfs/home/lixiangrui/.cache/llvm-firtool/1.135.0/bin/firtool` |
| main class | `top.MemBlockTopMain` |
| 默认 config | `TLConfig` |
| 默认 issue | `E.b` |
| 默认输出目录 | `build_memblock/rtl` |

## 期望产物

```text
build_memblock/rtl/filelist.f
build_memblock/rtl/ClockGate.sv
build_memblock/rtl/MemBlock.sv
build_memblock/rtl/MemBlockTop.sv
```

必要时，V3 脚本可以修补 `filelist.f`，将 `ClockGate.sv` 插入到
`MbistClockGateCell.sv` 之前。

## 已知失败模式

不要使用共享旧版 firtool：

```text
/nfs/home/share/firtool-1.56.1/bin/firtool
```

该版本对 Chisel 7.3 生成的 FIRRTL 过旧，可能在 verification layer 语法处失败。
