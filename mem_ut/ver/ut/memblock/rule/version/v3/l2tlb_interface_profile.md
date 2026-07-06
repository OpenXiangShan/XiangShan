# V3 L2TLB 接口规则 profile

## 语义边界

V3 mem_ut L2TLB agent 建模的是上游 DTLB 到 L2TLB 的 request 路径，以及
L2TLB 到 DTLB 的 response 路径。它不是 L2Cache、PTW 或 memory 下游模型。

## 已记录基线

已记录的 V3 mem_ut 基线为：

```text
1f96d06acbd75f00d619885ca27155810f72d922
```

如果 V3 DUT interface 发生变化，必须以生成后的 V3 Verilog 和公共 L2TLB
规则为依据刷新本 profile。
