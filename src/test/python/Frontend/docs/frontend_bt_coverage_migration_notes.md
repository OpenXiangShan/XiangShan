# Frontend BT Coverage Migration Notes

## 1. 背景

当前 Frontend BT 验证工作原先在独立目录 `frontend_bt/` 中推进，已经形成了一套“测试点 -> testcase -> cover bin -> 原始证据 -> 经理验收文档”的闭环试点方法。

随着环境主线迁移到：

- `src/test/python/Frontend/`

后续所有覆盖率闭环工作都应以该目录为主线继续演进。

## 2. 当前迁移策略

本轮迁移先做最小可验证切片，不一次性重构全部覆盖率基础设施：

1. 先迁移 Frontend BT 试点 CSV 到当前主线。
2. 先迁移最成熟、最干净的闭环样例：`BIN-004 ctrl redirect`。
3. 用当前主线环境既有的：
   - `Frontend_api`
   - `env/sequences`
   - `env/request_apis`
   - `env/fixtures`
   来重写 testcase，而不是把旧支线的环境文件整体照搬。

## 3. 本轮迁移产物

### 3.1 文档

- `docs/frontend_bt_functional_coverage_pilot.csv`
- `docs/frontend_bt_coverage_migration_notes.md`

### 3.2 testcase

- `tests/test_functional_coverage_pilot.py`

## 4. 为什么先迁 BIN-004

`BIN-004` 适合作为第一块迁移试点，原因是：

1. 它不依赖 `sv39` 翻译检查。
2. 它不受当前 translation-aware monitor 缺口影响。
3. 它的 stimulus 和 oracle 都比较清晰：
   - 程序镜像可控
   - redirect 接口是显式 DUT-facing 输入
   - monitor 可直接检查 redirect 后的 PC 序列

## 5. 后续迁移顺序建议

1. 迁 `BIN-004`
2. 迁 `BIN-012`
3. 再补：
   - `backend_accept_mode`
   - `fetch_path_type`
   - `frontend_exception_type`
4. 最后再考虑把旧支线中的 raw funcov / summary / unhit recorder 迁到当前主线

## 6. 当前结论

本轮迁移的重点不是把旧环境文件整体 merge 进来，而是确认：

- 新主线环境是否能承接“覆盖率闭环”方法
- 最小 testcase 是否能按新主线风格跑通
- 后续覆盖率扩展应依附于新主线目录结构，而不是继续依赖旧的 `frontend_bt/` 独立工作区

