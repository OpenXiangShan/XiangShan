# Frontend BT 覆盖率报告命令速查

## 1. 合并指定 `.dat` 到 `coverage.genhtml`

```bash
source /nfs/share/unitychip/activate && PATH=/nfs/share/unitychip/bin:$PATH \
src/test/python/Frontend/scripts/gen_coverage_html.sh \
src/test/python/Frontend/data/<case>.dat \
src/test/python/Frontend/data/coverage.genhtml
```

## 2. 合并多个 `.dat` 到 `coverage.genhtml`

```bash
source /nfs/share/unitychip/activate && PATH=/nfs/share/unitychip/bin:$PATH \
src/test/python/Frontend/scripts/gen_coverage_html.sh \
src/test/python/Frontend/data/<case1>.dat \
src/test/python/Frontend/data/<case2>.dat \
src/test/python/Frontend/data/coverage.genhtml
```

## 3. 基于 `data/*.dat` 重新生成全新的覆盖率报告

```bash
source /nfs/share/unitychip/activate && PATH=/nfs/share/unitychip/bin:$PATH \
src/test/python/Frontend/scripts/gen_coverage_html.sh \
src/test/python/Frontend/data
```

## 4. 生成到新的输出目录

```bash
source /nfs/share/unitychip/activate && PATH=/nfs/share/unitychip/bin:$PATH \
src/test/python/Frontend/scripts/gen_coverage_html.sh \
src/test/python/Frontend/data \
src/test/python/Frontend/data/coverage_rtl_new.genhtml
```

## 5. 用 Python 本地打开 `coverage.genhtml`

```bash
cd src/test/python/Frontend/data/coverage.genhtml
python -m http.server 8000
```

```text
http://127.0.0.1:8000/
```

## 6. 直接用 Python 打开 `index.html`

```bash
python -m webbrowser file://$PWD/src/test/python/Frontend/data/coverage.genhtml/index.html
```
