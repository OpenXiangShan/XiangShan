# Frontend Top-Level Refactor Design

**Goal**

Refactor `src/test/python/Frontend` to follow the same top-level packaging contract as `src/test/python/MemBlock` without changing the real toffee-based implementation under `src/test/python/Frontend/env`.

**Scope**

- Keep `env/` as the single implementation layer.
- Rework only the Frontend root entry files:
  - `Frontend_api.py`
  - `Frontend_env.py`
  - `conftest.py`
  - `README.md`
- Add tests that lock the root-module export contract.

**Non-Goals**

- No internal `env/` re-layering.
- No compatibility shims beyond the current public root imports.
- No behavior changes in DUT driving, monitor logic, sequences, or model code.

**Approach**

Use the MemBlock layout as the packaging reference, but map it onto Frontend's existing structure:

- `Frontend_api.py` becomes the documented root DUT/API entry, explicitly re-exporting `create_dut`, `dut`, and the public `api_Frontend_*` helpers from `env`.
- `Frontend_env.py` becomes the documented root environment entry, explicitly re-exporting `FrontendEnv`, `env`, and `full_env`.
- `conftest.py` remains thin and only handles import path/data-dir setup.
- `README.md` explains that Frontend differs from MemBlock in one important way: the top level is a stable facade, while the actual toffee env lives under `env/`.

**Testing**

Add a focused test that imports the root modules and verifies they re-export the exact objects from `env.api`, `env.fixtures`, and `env.frontend_env`. This protects the public package contract while keeping the implementation unchanged.

**Risks**

- Over-wrapping the top level would duplicate logic already centralized in `env/`.
- Using wildcard re-exports makes the top-level contract implicit and harder to test.

**Decision**

Take the minimal path: explicit root exports plus documentation and a focused compatibility test.
