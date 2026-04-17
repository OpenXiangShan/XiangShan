# 测试台设计索引

本目录收纳从旧 `frontend_bt/testbench/docs` 迁入的 Frontend BT 测试台设计文档。

这些文档的作用是保留当时对 Python 黑盒验证环境的分层设计与实现思路，方便后续继续在当前 doc worktree 里迭代，不再回头翻旧目录。

## 文件列表

- `T01_memory_model.md`
- `T02_icache_agent.md`
- `T03_uncache_ptw_agent.md`
- `T04_frontend_monitor.md`
- `T05_backend_model.md`
- `T06_golden_trace.md`
- `T07_coverage.md`
- `T08_api_layer.md`
- `T09_test_cases.md`

## 使用建议

如果当前工作是补文档或扩覆盖率，建议按下面顺序阅读：

1. `T04_frontend_monitor.md`
2. `T05_backend_model.md`
3. `T07_coverage.md`
4. `T08_api_layer.md`
5. `T09_test_cases.md`

如果当前工作是补充测试台结构性文档，再继续回看 `T01` 到 `T03` 与 `T06`。
