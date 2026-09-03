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
EXTENDED_RESULT ?= $(PREPARED_DIR)/extended-mixed-frozen-4h.json
FINAL_RESULT ?= $(PREPARED_DIR)/final-frozen-6h.json
LONG_RESULT ?= $(PREPARED_DIR)/final-frozen-6h-4096.json
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
CPP_MAIN := cpp/memblock_main.cpp
CPP_ENV := cpp/memblock_env.hpp
CONTROLLER_FILES := --controller-file $(CPP_MAIN) --controller-file $(CPP_ENV) --controller-file $(CPP_DEFAULTS) --controller-file $(SVA) --controller-file $(CONFIG) --controller-file $(VERIFY_SCRIPT)

.PHONY: ports check-ports prepare-rtl check-ports check-rtl bootstrap-picker export-picker picker-model freeze-runtime smoke pin-space single-load scalar-misaligned misaligned-stores exception-contracts two-stage-translation scalar-guest-fault vector-guest-fault vector-guest-fault-split vector-load vector-split-load vector-addressing vector-store-forwarding store-forwarding store-rdata-order store-tlb-miss-preserve dcache-release redirect queue-pressure random-loads random-vector-loads random-forwarding random-vector-forwarding random-mixed random-mixed-hunt random-boundary-hunt random-boundary-regression known-bug-sentinel regression hunt-regression verify-boundary-hunt extended-regression final-regression long-final-regression verify-extended-results verify-final-results verify-long-results unit

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

freeze-runtime: $(RUNTIME_METADATA)

$(RUNTIME_METADATA): $(PICKER_TEST_BINARY) $(PICKER_HARNESS)/libUTMemBlock.so $(XSPCOMM_LIBRARY) scripts/freeze_runtime.py
	$(PYTHON) scripts/freeze_runtime.py --binary $(PICKER_TEST_BINARY) --model $(PICKER_HARNESS)/libUTMemBlock.so --xspcomm $(XSPCOMM_LIBRARY) --output $(RUNTIME_DIR)

smoke: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test smoke

pin-space: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test pin-space

single-load: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test single-load

scalar-misaligned: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test scalar-misaligned

misaligned-stores: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test misaligned-stores

exception-contracts: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test exception-contracts

two-stage-translation: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test two-stage-translation

vector-guest-fault: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test vector-guest-fault

vector-guest-fault-split: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test vector-guest-fault-split

known-bug-sentinel: $(PICKER_TEST_BINARY)
	$(PYTHON) scripts/check_known_bug.py --binary $(PICKER_TEST_BINARY) --expect repaired-pass

random-mixed-hunt: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test random-mixed --seed $${SEED:-1} --transactions $${TRANSACTIONS:-512} --hunt-boundaries

random-boundary-hunt: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test random-boundary-hunt --seed $${SEED:-1} --transactions $${TRANSACTIONS:-64}

hunt-regression: $(PICKER_TEST_BINARY) $(PREPARED_METADATA)
	$(PYTHON) scripts/run_regression.py --binary $(PICKER_TEST_BINARY) --rtl-metadata $(PREPARED_METADATA) --output $(PREPARED_DIR)/boundary-hunt.json --jobs $${REGRESSION_JOBS:-$(JOBS)} --seeds $${SEEDS:-32} --transactions $${TRANSACTIONS:-512} --scenarios random-mixed --hunt-boundaries --continue-on-failure

random-boundary-regression: $(PICKER_TEST_BINARY) $(PREPARED_METADATA)
	$(PYTHON) scripts/run_regression.py --binary $(PICKER_TEST_BINARY) --rtl-metadata $(PREPARED_METADATA) --output $(PREPARED_DIR)/random-boundary-hunt.json --jobs $${REGRESSION_JOBS:-$(JOBS)} --seeds $${SEEDS:-32} --transactions $${TRANSACTIONS:-64} --scenarios random-boundary-hunt --hunt-boundaries --continue-on-failure

verify-boundary-hunt:
	$(PYTHON) scripts/verify_boundary_hunt.py --input $(PREPARED_DIR)/random-boundary-hunt.json --min-seeds $${SEEDS:-32} --transactions $${TRANSACTIONS:-64} --rtl-metadata $(PREPARED_METADATA) --allow-all-pass

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
	$(PICKER_TEST_BINARY) --test random-loads --seed $${SEED:-1} --transactions $${TRANSACTIONS:-200}

random-vector-loads: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test random-vector-loads --seed $${SEED:-1} --transactions $${TRANSACTIONS:-200}

random-forwarding: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test random-forwarding --seed $${SEED:-1} --transactions $${TRANSACTIONS:-48}

random-vector-forwarding: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test random-vector-forwarding --seed $${SEED:-1} --transactions $${TRANSACTIONS:-24}

random-mixed: $(PICKER_TEST_BINARY)
	$(PICKER_TEST_BINARY) --test random-mixed --seed $${SEED:-1} --transactions $${TRANSACTIONS:-512}

regression: $(PICKER_TEST_BINARY)
	$(PYTHON) scripts/run_regression.py --binary $(PICKER_TEST_BINARY) --rtl-metadata $(PREPARED_METADATA) --output $(REGRESSION_OUTPUT) --jobs $${REGRESSION_JOBS:-$(JOBS)} --seeds $${SEEDS:-8} --transactions $${TRANSACTIONS:-1000} --forwarding-transactions $${FORWARD_TRANSACTIONS:-48} --mixed-transactions $${MIXED_TRANSACTIONS:-512}

extended-regression: $(RUNTIME_METADATA)
	$(PYTHON) scripts/run_regression.py --runtime-metadata $(RUNTIME_METADATA) --rtl-metadata $(PREPARED_METADATA) $(CONTROLLER_FILES) --output $(EXTENDED_RESULT) --jobs $${REGRESSION_JOBS:-$(JOBS)} --duration-seconds $${DURATION_SECONDS:-21600} --scenarios random-mixed --mixed-transactions $${MIXED_TRANSACTIONS:-4096} --progress-interval-seconds 60

final-regression: $(RUNTIME_METADATA)
	$(PYTHON) scripts/run_regression.py --runtime-metadata $(RUNTIME_METADATA) --rtl-metadata $(PREPARED_METADATA) $(CONTROLLER_FILES) --output $(FINAL_RESULT) --jobs $${REGRESSION_JOBS:-$(JOBS)} --duration-seconds $${DURATION_SECONDS:-21600} --scenarios random-mixed --mixed-transactions $${MIXED_TRANSACTIONS:-4096} --progress-interval-seconds 60

long-final-regression: $(RUNTIME_METADATA)
	$(PYTHON) scripts/run_regression.py --runtime-metadata $(RUNTIME_METADATA) --rtl-metadata $(PREPARED_METADATA) $(CONTROLLER_FILES) --output $(LONG_RESULT) --jobs $${REGRESSION_JOBS:-$(JOBS)} --duration-seconds $${DURATION_SECONDS:-21600} --scenarios random-mixed --mixed-transactions $${MIXED_TRANSACTIONS:-4096} --progress-interval-seconds 60

verify-extended-results:
	$(PYTHON) $(VERIFY_SCRIPT) --input $(EXTENDED_RESULT) --min-duration-seconds 14400 --min-results 1 --scenario random-mixed --transactions 1000 --mixed-transactions 4096 --require-backpressure --require-frozen-runtime --runtime-metadata $(RUNTIME_METADATA) --rtl-metadata $(PREPARED_METADATA) $(CONTROLLER_FILES) --runner scripts/run_regression.py

verify-final-results:
	$(PYTHON) $(VERIFY_SCRIPT) --input $(FINAL_RESULT) --min-duration-seconds 21600 --min-results 1 --scenarios random-mixed --transactions 1000 --mixed-transactions 4096 --require-backpressure --require-frozen-runtime --runtime-metadata $(RUNTIME_METADATA) --rtl-metadata $(PREPARED_METADATA) $(CONTROLLER_FILES) --runner scripts/run_regression.py

verify-long-results:
	$(PYTHON) $(VERIFY_SCRIPT) --input $(LONG_RESULT) --min-duration-seconds 21600 --min-results 1 --scenarios random-mixed --transactions 1000 --mixed-transactions 4096 --require-backpressure --require-frozen-runtime --runtime-metadata $(RUNTIME_METADATA) --rtl-metadata $(PREPARED_METADATA) $(CONTROLLER_FILES) --runner scripts/run_regression.py

unit:
	$(PYTHON) -m unittest discover -s unit -p 'test_*.py' -v
