PYTHON ?= python3
REPO_ROOT := $(abspath ../..)
RTL := $(REPO_ROOT)/build/rtl/MemBlock.sv
CONFIG := config/memblock.json
PORT_JSON := config/expected_ports.json
PORT_DOC := docs/PORTS.md
SVA := sv/memblock_protocol_sva.sv
VERIFY_SCRIPT := scripts/verify_regression.py
SOURCE_FILELIST := $(REPO_ROOT)/build/rtl/filelist.f
PREPARED_DIR := $(REPO_ROOT)/build/memblock
PREPARED_FILELIST := $(PREPARED_DIR)/rtl.f
PREPARED_METADATA := $(PREPARED_DIR)/rtl.json
REGRESSION_OUTPUT ?= $(PREPARED_DIR)/regression.json
EXTENDED_RESULT ?= $(PREPARED_DIR)/extended-mixed-frozen-6h-8192.json
FINAL_RESULT ?= $(PREPARED_DIR)/final-frozen-8h-16384.json
LONG_RESULT ?= $(PREPARED_DIR)/final-frozen-12h-32768.json
STRESS_RESULT ?= $(PREPARED_DIR)/stress-frozen-1h-16384.json
JOBS ?= 8
PICKER_ROOT := $(PREPARED_DIR)/tools/picker
PICKER ?= $(PICKER_ROOT)/build/bin/picker
PICKER_METADATA := $(PREPARED_DIR)/tools/picker.json
PICKER_OUTPUT := $(PREPARED_DIR)/picker
CPP_DEFAULTS := cpp/generated_port_defaults.hpp
PICKER_HARNESS := $(PICKER_OUTPUT)/UT_MemBlock
PICKER_EXPORT_STAMP := $(PICKER_OUTPUT)/.export.stamp
PICKER_MODEL_STAMP := $(PICKER_OUTPUT)/build/UT_MemBlock/.model.stamp
PICKER_HARNESS_STAMP := $(PICKER_HARNESS)/.prepared.stamp
PICKER_TEST_BINARY := $(PICKER_HARNESS)/build/UTMemBlock_example
XSPCOMM_LIBRARY := $(PICKER_ROOT)/build/dependence/xcomm/lib/libxspcomm.so.0.0.1
RUNTIME_DIR := $(PREPARED_DIR)/runtime
RUNTIME_METADATA := $(RUNTIME_DIR)/runtime.json
FROZEN_RTL_METADATA := $(RUNTIME_DIR)/rtl.json
CPP_MAIN := cpp/memblock_main.cpp
CPP_ENV := cpp/memblock_env.hpp
CONTROLLER_FILES := --controller-file $(CPP_MAIN) --controller-file $(CPP_ENV) --controller-file $(CPP_DEFAULTS) --controller-file $(SVA) --controller-file $(CONFIG) --controller-file $(VERIFY_SCRIPT) --controller-file scripts/freeze_runtime.py --controller-file Makefile

.PHONY: ports check-ports prepare-rtl check-rtl bootstrap-picker export-picker picker-model freeze-runtime smoke pin-space single-load fp-loads trigger-contracts metadata-contracts dcache-errors atomic-contracts atomic-dchannel-errors uncache-errors uncache-widths mmio-contracts cbo-zero-contracts reset-recovery scalar-misaligned misaligned-stores exception-contracts l2-tlb-contracts two-stage-translation translation-matrix translation-fence translation-context translation-bare translation-faults translation-permissions translation-superpages scalar-guest-fault vector-guest-fault vector-guest-fault-split vector-load vector-split-load vector-addressing vector-store-forwarding store-forwarding store-rdata-order store-tlb-miss-preserve dcache-release redirect queue-pressure random-loads random-vector-loads random-forwarding random-vector-forwarding random-mixed random-stress random-mixed-hunt random-boundary-hunt random-boundary-regression known-bug-sentinel regression hunt-regression stress-regression verify-stress-results verify-boundary-hunt extended-regression final-regression long-final-regression verify-extended-results verify-final-results verify-long-results final-acceptance unit

ports: $(PORT_JSON) $(PORT_DOC) $(SVA) $(CPP_DEFAULTS)

$(PORT_JSON) $(PORT_DOC) &: $(RTL) $(CONFIG) scripts/rtl_ports.py
	$(PYTHON) scripts/rtl_ports.py generate --rtl $(RTL) --config $(CONFIG) --json $(PORT_JSON) --markdown $(PORT_DOC)

$(SVA): $(PORT_JSON) scripts/generate_sva.py
	$(PYTHON) scripts/generate_sva.py generate --manifest $(PORT_JSON) --output $(SVA)

$(CPP_DEFAULTS): $(PORT_JSON) scripts/generate_cpp.py
	$(PYTHON) scripts/generate_cpp.py generate --manifest $(PORT_JSON) --output $(CPP_DEFAULTS)

check-ports:
	$(PYTHON) scripts/rtl_ports.py check --rtl $(RTL) --config $(CONFIG) --json $(PORT_JSON) --markdown $(PORT_DOC)
	$(PYTHON) scripts/generate_sva.py check --manifest $(PORT_JSON) --output $(SVA)
	$(PYTHON) scripts/generate_cpp.py check --manifest $(PORT_JSON) --output $(CPP_DEFAULTS)

prepare-rtl: $(PREPARED_FILELIST) $(PREPARED_METADATA)

$(PREPARED_FILELIST) $(PREPARED_METADATA) &: $(SOURCE_FILELIST) $(RTL) $(CONFIG) $(SVA) scripts/prepare_rtl.py
	$(PYTHON) scripts/prepare_rtl.py generate --source $(SOURCE_FILELIST) --bind $(SVA) --config $(CONFIG) --output $(PREPARED_FILELIST) --metadata $(PREPARED_METADATA)

check-rtl: check-ports
	$(PYTHON) scripts/prepare_rtl.py check --source $(SOURCE_FILELIST) --bind $(SVA) --config $(CONFIG) --output $(PREPARED_FILELIST) --metadata $(PREPARED_METADATA)

bootstrap-picker:
	$(PYTHON) scripts/bootstrap_picker.py --root $(PICKER_ROOT) --metadata $(PICKER_METADATA) --jobs $(JOBS)

export-picker: $(PICKER_EXPORT_STAMP)

$(PICKER_EXPORT_STAMP): $(PREPARED_FILELIST) $(PREPARED_METADATA) $(PORT_JSON) $(SVA)
	@test -x $(PICKER) || { echo "Picker is missing: $(PICKER); run make bootstrap-picker or set PICKER" >&2; exit 1; }
	@test "$(abspath $(PICKER_OUTPUT))" = "$(PREPARED_DIR)/picker"
	rm -rf -- $(PICKER_OUTPUT)
	$(PICKER) export --fs $(PREPARED_FILELIST) --lang cpp --sname MemBlock --tdir $(PICKER_OUTPUT) --autobuild false --build-threads $(JOBS) --vflag '--assert -Wno-fatal' --cp_lib true
	@touch $@

picker-model: $(PICKER_MODEL_STAMP)

$(PICKER_MODEL_STAMP): $(PICKER_EXPORT_STAMP)
	$(MAKE) -C $(PICKER_OUTPUT) compile NPROC=$(JOBS)
	@test -f $(PICKER_OUTPUT)/build/UT_MemBlock/libUTMemBlock.so
	@touch $@

$(PICKER_HARNESS_STAMP): $(PICKER_MODEL_STAMP) scripts/prepare_picker_harness.py $(CPP_MAIN) $(CPP_ENV) $(CPP_DEFAULTS)
	$(PYTHON) scripts/prepare_picker_harness.py --picker-output $(PICKER_OUTPUT) --main-source $(CPP_MAIN)
	@touch $@

$(PICKER_TEST_BINARY): $(PICKER_HARNESS_STAMP)
	CXXFLAGS=-I$(CURDIR)/cpp $(MAKE) -C $(PICKER_HARNESS) compile NPROC=$(JOBS)
	@test -x $@

freeze-runtime: $(RUNTIME_METADATA) $(FROZEN_RTL_METADATA)

$(RUNTIME_METADATA) $(FROZEN_RTL_METADATA) &: $(PICKER_TEST_BINARY) $(PICKER_HARNESS)/libUTMemBlock.so $(XSPCOMM_LIBRARY) $(PREPARED_METADATA) scripts/freeze_runtime.py
	$(PYTHON) scripts/freeze_runtime.py --binary $(PICKER_TEST_BINARY) --model $(PICKER_HARNESS)/libUTMemBlock.so --xspcomm $(XSPCOMM_LIBRARY) --rtl-metadata $(PREPARED_METADATA) --output $(RUNTIME_DIR)

smoke: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test smoke

pin-space: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test pin-space

single-load: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test single-load

fp-loads: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test fp-loads

trigger-contracts: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test trigger-contracts

metadata-contracts: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test metadata-contracts

dcache-errors: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test dcache-errors

atomic-contracts: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test atomic-contracts

atomic-dchannel-errors: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test atomic-dchannel-errors

uncache-errors: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test uncache-errors

uncache-widths: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test uncache-widths

mmio-contracts: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test mmio-contracts

cbo-zero-contracts: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test cbo-zero-contracts

reset-recovery: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test reset-recovery

scalar-misaligned: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test scalar-misaligned

misaligned-stores: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test misaligned-stores

exception-contracts: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test exception-contracts

l2-tlb-contracts: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test l2-tlb-contracts

two-stage-translation: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test two-stage-translation

translation-matrix: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test translation-matrix

translation-fence: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test translation-fence

translation-context: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test translation-context

translation-bare: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test translation-bare

translation-faults: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test translation-faults

translation-permissions: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test translation-permissions

translation-superpages: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test translation-superpages

vector-guest-fault: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test vector-guest-fault

vector-guest-fault-split: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test vector-guest-fault-split

known-bug-sentinel: $(PICKER_TEST_BINARY)
	$(MAKE) check-rtl
	$(PYTHON) scripts/check_known_bug.py --binary $(PICKER_TEST_BINARY) --rtl-metadata $(PREPARED_METADATA) --timeout-seconds 300 --expect repaired-pass

random-mixed-hunt: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test random-mixed --seed $(or $(SEED),1) --transactions $(or $(TRANSACTIONS),16384) --hunt-boundaries

random-boundary-hunt: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test random-boundary-hunt --seed $(or $(SEED),1) --transactions $(or $(TRANSACTIONS),512)

hunt-regression: $(PICKER_TEST_BINARY) $(PREPARED_METADATA)
	$(PYTHON) scripts/run_regression.py --binary $(PICKER_TEST_BINARY) --rtl-metadata $(PREPARED_METADATA) --output $(PREPARED_DIR)/boundary-hunt.json --jobs $(or $(REGRESSION_JOBS),$(JOBS)) --seeds $(or $(SEEDS),32) --transactions $(or $(TRANSACTIONS),16384) --mixed-transactions $(or $(MIXED_TRANSACTIONS),16384) --scenarios random-mixed --hunt-boundaries --continue-on-failure

random-boundary-regression: $(PICKER_TEST_BINARY) $(PREPARED_METADATA)
	$(PYTHON) scripts/run_regression.py --binary $(PICKER_TEST_BINARY) --rtl-metadata $(PREPARED_METADATA) --output $(PREPARED_DIR)/random-boundary-hunt.json --jobs $(or $(REGRESSION_JOBS),$(JOBS)) --seeds $(or $(SEEDS),32) --transactions $(or $(TRANSACTIONS),512) --scenarios random-boundary-hunt --hunt-boundaries --continue-on-failure

verify-boundary-hunt:
	$(PYTHON) scripts/verify_boundary_hunt.py --input $(PREPARED_DIR)/random-boundary-hunt.json --min-seeds $(or $(SEEDS),32) --transactions $(or $(TRANSACTIONS),512) --rtl-metadata $(PREPARED_METADATA) --expect repaired-all-pass

scalar-guest-fault: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test scalar-guest-fault

vector-load: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test vector-load

vector-split-load: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test vector-split-load

vector-addressing: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test vector-addressing

vector-store-forwarding: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test vector-store-forwarding

store-forwarding: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test store-forwarding

store-rdata-order: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test store-rdata-order

store-tlb-miss-preserve: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test store-tlb-miss-preserve

dcache-release: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test dcache-release

redirect: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test redirect

queue-pressure: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test queue-pressure

random-loads: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test random-loads --seed $(or $(SEED),1) --transactions $(or $(TRANSACTIONS),16384)

random-vector-loads: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test random-vector-loads --seed $(or $(SEED),1) --transactions $(or $(TRANSACTIONS),16384)

random-forwarding: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test random-forwarding --seed $(or $(SEED),1) --transactions $(or $(TRANSACTIONS),48)

random-vector-forwarding: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test random-vector-forwarding --seed $(or $(SEED),1) --transactions $(or $(TRANSACTIONS),24)

random-mixed: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test random-mixed --seed $(or $(SEED),1) --transactions $(or $(TRANSACTIONS),16384)

random-stress: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test random-stress --seed $(or $(SEED),1) --transactions $(or $(TRANSACTIONS),16384)

regression: $(PICKER_TEST_BINARY)
	$(MAKE) check-rtl
	$(PYTHON) scripts/run_regression.py --binary $(PICKER_TEST_BINARY) --rtl-metadata $(PREPARED_METADATA) --output $(REGRESSION_OUTPUT) --jobs $(or $(REGRESSION_JOBS),$(JOBS)) --seeds $(or $(SEEDS),8) --transactions $(or $(TRANSACTIONS),16384) --forwarding-transactions $(or $(FORWARD_TRANSACTIONS),48) --mixed-transactions $(or $(MIXED_TRANSACTIONS),16384)

stress-regression: $(RUNTIME_METADATA) $(FROZEN_RTL_METADATA)
	$(MAKE) check-rtl
	$(PYTHON) scripts/run_regression.py --runtime-metadata $(RUNTIME_METADATA) --rtl-metadata $(FROZEN_RTL_METADATA) $(CONTROLLER_FILES) --output $(STRESS_RESULT) --jobs $(or $(REGRESSION_JOBS),$(JOBS)) --duration-seconds $(or $(DURATION_SECONDS),3600) --timeout-seconds $(or $(TIMEOUT_SECONDS),1800) --scenarios random-stress --transactions $(or $(STRESS_TRANSACTIONS),16384) --mixed-transactions $(or $(STRESS_TRANSACTIONS),16384) --progress-interval-seconds 60

verify-stress-results:
	$(MAKE) check-rtl
	$(PYTHON) $(VERIFY_SCRIPT) --input $(STRESS_RESULT) --min-duration-seconds $(or $(MIN_DURATION_SECONDS),3600) --min-results $(or $(MIN_RESULTS),128) --scenario random-stress --transactions $(or $(STRESS_TRANSACTIONS),16384) --mixed-transactions $(or $(STRESS_TRANSACTIONS),16384) --expected-jobs $(or $(REGRESSION_JOBS),$(JOBS)) --require-backpressure --require-frozen-runtime --runtime-metadata $(RUNTIME_METADATA) --rtl-metadata $(FROZEN_RTL_METADATA) $(CONTROLLER_FILES) --runner scripts/run_regression.py

extended-regression: $(RUNTIME_METADATA) $(FROZEN_RTL_METADATA)
	$(MAKE) check-rtl
	$(PYTHON) scripts/run_regression.py --runtime-metadata $(RUNTIME_METADATA) --rtl-metadata $(FROZEN_RTL_METADATA) $(CONTROLLER_FILES) --output $(EXTENDED_RESULT) --jobs $(or $(REGRESSION_JOBS),$(JOBS)) --duration-seconds $(or $(DURATION_SECONDS),21600) --timeout-seconds $(or $(TIMEOUT_SECONDS),1800) --scenarios random-mixed --mixed-transactions $(or $(MIXED_TRANSACTIONS),8192) --progress-interval-seconds 60

final-regression: $(RUNTIME_METADATA) $(FROZEN_RTL_METADATA)
	$(MAKE) check-rtl
	$(PYTHON) scripts/run_regression.py --runtime-metadata $(RUNTIME_METADATA) --rtl-metadata $(FROZEN_RTL_METADATA) $(CONTROLLER_FILES) --output $(FINAL_RESULT) --jobs $(or $(REGRESSION_JOBS),$(JOBS)) --duration-seconds $(or $(DURATION_SECONDS),28800) --timeout-seconds $(or $(TIMEOUT_SECONDS),1800) --scenarios random-mixed --mixed-transactions $(or $(MIXED_TRANSACTIONS),16384) --progress-interval-seconds 60

long-final-regression: $(RUNTIME_METADATA) $(FROZEN_RTL_METADATA)
	$(MAKE) check-rtl
	$(PYTHON) scripts/run_regression.py --runtime-metadata $(RUNTIME_METADATA) --rtl-metadata $(FROZEN_RTL_METADATA) $(CONTROLLER_FILES) --output $(LONG_RESULT) --jobs $(or $(REGRESSION_JOBS),$(JOBS)) --duration-seconds $(or $(DURATION_SECONDS),43200) --timeout-seconds $(or $(TIMEOUT_SECONDS),1800) --scenarios random-mixed --mixed-transactions $(or $(MIXED_TRANSACTIONS),32768) --progress-interval-seconds 60

verify-extended-results:
	$(MAKE) check-rtl
	$(PYTHON) $(VERIFY_SCRIPT) --input $(EXTENDED_RESULT) --min-duration-seconds 21600 --min-results 128 --scenario random-mixed --transactions 4096 --mixed-transactions 8192 --expected-jobs 8 --require-backpressure --require-frozen-runtime --runtime-metadata $(RUNTIME_METADATA) --rtl-metadata $(FROZEN_RTL_METADATA) $(CONTROLLER_FILES) --runner scripts/run_regression.py

verify-final-results:
	$(MAKE) check-rtl
	$(MAKE) dcache-release
	$(MAKE) known-bug-sentinel
	$(MAKE) verify-boundary-hunt
	$(PYTHON) $(VERIFY_SCRIPT) --input $(FINAL_RESULT) --min-duration-seconds 28800 --min-results 128 --scenarios random-mixed --transactions 4096 --mixed-transactions 16384 --expected-jobs 8 --require-backpressure --require-frozen-runtime --runtime-metadata $(RUNTIME_METADATA) --rtl-metadata $(FROZEN_RTL_METADATA) $(CONTROLLER_FILES) --runner scripts/run_regression.py

verify-long-results:
	$(MAKE) check-rtl
	$(MAKE) dcache-release
	$(MAKE) known-bug-sentinel
	$(MAKE) verify-boundary-hunt
	$(PYTHON) $(VERIFY_SCRIPT) --input $(LONG_RESULT) --min-duration-seconds 43200 --min-results 128 --scenarios random-mixed --transactions 4096 --mixed-transactions 32768 --expected-jobs 8 --require-backpressure --require-frozen-runtime --runtime-metadata $(RUNTIME_METADATA) --rtl-metadata $(FROZEN_RTL_METADATA) $(CONTROLLER_FILES) --runner scripts/run_regression.py

final-acceptance:
	$(MAKE) unit
	$(MAKE) trigger-contracts
	$(MAKE) metadata-contracts
	$(MAKE) fp-loads
	$(MAKE) dcache-errors
	$(MAKE) uncache-errors
	$(MAKE) uncache-widths
	$(MAKE) mmio-contracts
	$(MAKE) cbo-zero-contracts
	$(MAKE) reset-recovery
	$(MAKE) atomic-contracts
	$(MAKE) atomic-dchannel-errors
	$(MAKE) translation-matrix
	$(MAKE) translation-fence
	$(MAKE) translation-context
	$(MAKE) translation-bare
	$(MAKE) translation-faults
	$(MAKE) translation-permissions
	$(MAKE) translation-superpages
	$(MAKE) l2-tlb-contracts
	$(MAKE) dcache-release
	$(MAKE) known-bug-sentinel
	$(MAKE) random-boundary-regression
	$(MAKE) verify-boundary-hunt
	$(MAKE) final-regression
	$(MAKE) verify-final-results

unit:
	$(PYTHON) -m unittest discover -s unit -p 'test_*.py' -v
