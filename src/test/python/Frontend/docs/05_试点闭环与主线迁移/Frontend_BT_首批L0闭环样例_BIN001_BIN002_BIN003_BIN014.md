# Frontend BT 首批 L0 闭环样例

本文用于补齐首批可直接写成覆盖率闭环材料的 L0 bin：

- `BIN-001 reset_boot_path::seen`
- `BIN-002 fetch_path_type::icache_seq`
- `BIN-003 fetch_path_type::mmio_uncache`
- `BIN-014 backend_accept_mode::all_block_short`

这里的结论口径与 `BIN-004` / `BIN-012` 不同：

1. 这四个点当前依据的是历史 standalone `frontend_bt` 证据。
2. 因此当前结论写成：
   - 历史闭环已完成
   - 主线回放待升级
3. 只有在环境 agent 给出当前主线的新 evidence 后，才把它们升级成“主线已验收”。

## 1. BIN-001 / BIN-002

### 1.1 证据文件

当前引用的历史 evidence 文件：

- `test_funcov_boot_and_icache_seq.funcov.json`
- `test_funcov_boot_and_icache_seq.funcov.summary.csv`
- `test_funcov_boot_and_icache_seq.fst`

### 1.2 BIN-001 关键证据

主命中项：

- `reset_boot_path::seen`
  - `bin_id = BIN-001`
  - `hits = 1`
  - `first_cycle = 550`
  - `reset_release_cycle = 21`
  - `slot_count = 8`

运行统计：

- `monitor.error_count = 0`
- `backend.commit_count = 8`
- `monitor.redirect_count = 0`

### 1.3 BIN-002 关键证据

主命中项：

- `fetch_path_type::icache_seq`
  - `bin_id = BIN-002`
  - `hits = 139`
  - `first_cycle = 543`
  - `last_cycle = 566`

raw evidence 中已经记录到连续的 `handshake.icache_a` 事件，例如：

- `address = 0x80000000`
- `latency = 1`
- `miss = false`

运行统计：

- `icache.req_count = 11`
- `monitor.error_count = 0`
- `backend.commit_count = 8`

### 1.4 当前结论

对这组 evidence，可给出以下口径：

1. `BIN-001` 已形成历史复位启动闭环。
2. `BIN-002` 已形成历史 ICache 主路径顺序取指闭环。
3. 两个点共用同一条基础顺序取指 testcase 证据链，因此适合作为最小 L0 启动路径样例。
4. 当前仍应标注：
   - 历史闭环已完成
   - 主线回放待升级

## 2. BIN-003

### 2.1 证据文件

当前引用的历史 evidence 文件：

- `test_funcov_mmio_uncache_path.funcov.json`
- `test_funcov_mmio_uncache_path.funcov.summary.csv`
- `test_funcov_mmio_uncache_path.fst`

### 2.2 关键证据

主命中项：

- `fetch_path_type::mmio_uncache`
  - `bin_id = BIN-003`
  - `hits = 64`
  - `first_cycle = 571`
  - `last_cycle = 578`

同一 raw funcov 中还记录到：

- `fetch_path_type::icache_seq`
  - `hits = 7`

这说明该 testcase 不只是“纯 MMIO 固定停留”，而是已经体现了普通 ICache 路径与 MMIO 路径的切换背景。

运行统计：

- `monitor.error_count = 0`
- `backend.commit_count = 4`
- `icache.req_count = 7`
- `uncache.req_count = 0`

### 2.3 当前结论

对这个点，当前可给出以下口径：

1. `BIN-003` 已形成历史 MMIO 取指路径闭环。
2. 当前 raw funcov 已证明 `mmio_uncache` 路径被稳定命中。
3. monitor 没有报错，说明在当前历史环境假设下路径切换行为可接受。
4. 当前仍应标注：
   - 历史闭环已完成
   - 主线回放待升级

## 3. BIN-014

### 3.1 证据文件

当前引用的历史 evidence 文件：

- `test_funcov_short_backpressure.funcov.json`
- `test_funcov_short_backpressure.funcov.summary.csv`
- `test_funcov_short_backpressure.fst`

### 3.2 关键证据

主命中项：

- `backend_accept_mode::all_block_short`
  - `bin_id = BIN-014`
  - `hits = 1`
  - `first_cycle = 58`
  - `blocked_cycles = 12`

运行统计：

- `monitor.error_count = 0`
- `backend.commit_count = 8`
- `monitor.redirect_count = 0`

同一 raw funcov 中还带有：

- `fetch_path_type::icache_seq`
  - `hits = 139`

因此该 testcase 的行为可解释为：

1. 前端主取指路径已建立。
2. backend `canAccept` 出现短时全反压。
3. 反压解除后 testcase 仍能继续 commit，且 monitor 未报错。

### 3.3 当前结论

对这个点，当前可给出以下口径：

1. `BIN-014` 已形成历史短时全反压闭环。
2. `blocked_cycles = 12` 为当前最核心的场景命中证据。
3. 当前仍应标注：
   - 历史闭环已完成
   - 主线回放待升级

## 4. 当前首批 L0 闭环集合

结合当前已知 evidence，当前可组织出的首批 L0 闭环集合为：

- 主线已验收：
  - `BIN-004`
  - `BIN-012`
- 历史闭环已完成、主线回放待升级：
  - `BIN-001`
  - `BIN-002`
  - `BIN-003`
  - `BIN-014`

这样已经能够形成一批对外可讲的基础闭环故事线：

1. 复位启动
2. ICache 主路径顺序取指
3. MMIO 取指路径
4. backend ctrl redirect
5. ITLB miss / PTW / refill
6. backend 短时全反压

## 5. 下一步

后续文档建议继续按下面顺序推进：

1. 如果环境 agent 给出主线新 evidence，优先把：
   - `BIN-001`
   - `BIN-002`
   - `BIN-003`
   - `BIN-014`
   从“历史闭环已完成”升级到“主线已验收”。
2. 再评估：
   - `BIN-007`
   - `BIN-013`
   是否具备下一批闭环材料基础。
