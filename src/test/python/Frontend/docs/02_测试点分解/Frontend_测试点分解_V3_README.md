# Frontend 测试点分解说明

本文件已迁入当前 `src/test/python/Frontend/docs/02_测试点分解/`，作为后续 Frontend BT 测试点继续演进的主说明文件。

- 产物文件：`Frontend_测试点分解_V3.csv`
- 行数：364（不含表头）
- DUT：Kunminghu V3 `Frontend` 顶层模块
- 参考输入：`../01_验证策略及方案/Frontend_BT_验证方案.md`、当前 Frontend Chisel 源码、已有 Frontend BT 闭环文档
- 字段说明：
  - `NO.`：编号
  - `SPEC`：测试点大类
  - `C/D/E`：逐级细化测试点
  - `F`：推荐的测试点分解方法
  - `Condition`：激励和成立条件
  - `Checkpoint`：验证通过判据
  - `Object`：建议重点观测/检查的对象和信号
- 说明：这是一版 300+ 行测试点基线，后续可以继续按模块补细，尤其是 BPU、ICache、ITLB/PTW、性能建模和 funcov 映射。

- 已完成一轮精修：接口、顶层IO、流水线控制、组合场景、性能、维测类测试点的 `Condition/Checkpoint/Object` 已按 `Frontend.scala` 顶层真实连线收紧，减少“接口正确/无协议违例”这类过泛描述。
