# BIN-1095/BIN-1096 Current DUT Signal Visibility Review

## Scope

This review concerns the two InstrUncache TL-A stall leaves:

- `BIN-1095` / `instruncache_leaf_002`: `memBackTypeMM` is stable while a
  valid TL-A request is backpressured.
- `BIN-1096` / `instruncache_leaf_003`: `memPageTypeNC` is stable in the
  same interval.

It records a current-DUT observability result.  It is not evidence of either
leaf being HIT, nor evidence that the RTL behavior is globally unreachable.

## Current-Provenance Attempt

The following exact-target DUT run used the current standalone Verilator
manifest: implementation `1a32a9056d993233fa1bf3a394b16e8a762abf52`, design
baseline `e5c70547f3a966accf20a4b065ec1d8e33443180`, `DefaultConfig`.

```bash
source /nfs/home/zhaoxinran/.venv/mcpgateway/bin/activate
TB_ENABLE_DUT_TESTS=1 \
TB_FUNCOV_TARGET_BINS=BIN-1095,BIN-1096 \
TB_RUN_ID=ctrl_bin1095_1096_head981_20260902_01 \
TB_BACKEND_RANDOM_SEED=1 TB_ICACHE_RANDOM_SEED=1 TB_PTW_RANDOM_SEED=1 \
pytest -q \
  src/test/python/Frontend/tests/py/jiabowen/test_ifu_instr_uncache_attribute_stability_v3_dut.py::test_pbmt_nc_tl_a_backpressure_holds_user_attributes
```

The test was skipped after reaching a real TL-A stall, not passed.  At cycle
578 its recorded observation was:

```text
tl_a_valid=1, tl_a_ready=0, tl_a_addr=0x80001000,
tl_a_mem_back_type_mm=null, tl_a_mem_page_type_nc=null
```

The artifact is
`data/runs/ctrl_bin1095_1096_head981_20260902_01/funcov/test_pbmt_nc_tl_a_backpressure_holds_user_attributes.funcov.json`.
It declares both bins explicitly, has no target hit, and its checker is not
eligible because the test outcome is `skipped`.

## Signal Contract Result

`mmio_nc_owner_funcov._snapshot` resolves the V3 TL user names
`auto_inner_instrUncache_client_out_a_bits_user_memBackType_MM` and
`auto_inner_instrUncache_client_out_a_bits_user_memPageType_NC`.  Neither is
retained in the current generated standalone build inventory.  The fixture
therefore refuses to infer either value from request classification or from a
default value.

The focused producer unit suite passed (`24 passed`).  In particular,
`test_tl_a_stall_missing_user_attributes_do_not_default_to_hits` verifies that
the two leaves remain unhit when either required observation is absent.

## Disposition

Keep `BIN-1095` and `BIN-1096` as `MODELED` and retain the strict denominator
at `343`.  The model expresses the required transaction semantics; the
standalone DUT currently lacks the two direct observables needed to turn that
model into traceable primary HIT evidence.  A future closure requires either a
retained signal-contract/bind path or a higher-level integration environment
that exposes the actual TL user fields.  Do not backannotate this skipped run
and do not reinterpret request type as proof of either user bit.
