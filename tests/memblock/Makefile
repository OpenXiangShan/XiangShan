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
EXTENDED_RESULT ?= $(PREPARED_DIR)/extended-constrained-random-6h.json
FINAL_RESULT ?= $(PREPARED_DIR)/final-constrained-random-48h.json
LONG_RESULT ?= $(PREPARED_DIR)/final-constrained-random-12h.json
ENDURANCE_RESULT ?= $(PREPARED_DIR)/endurance-constrained-random-8x1000000.json
STRICT_COVERAGE_RESULT ?= $(PREPARED_DIR)/strict-coverage-results.json
PROBE_STRICT_COVERAGE_RESULT ?= $(PREPARED_DIR)/probe-strict-coverage-results.json
STRICT_CONSTRAINTS ?= coverage
STRICT_CONSTRAINT_ARGS = $(foreach item,$(STRICT_CONSTRAINT),--constraint $(item))
ACTIONS ?= 16384
ENDURANCE_ACTIONS ?= 1000000
ENDURANCE_SEEDS ?= 8
ENDURANCE_TIMEOUT_SECONDS ?= 720000
BENCHMARK_RESULT ?= $(PREPARED_DIR)/test-scale.json
BENCHMARK_MARKDOWN ?= $(PREPARED_DIR)/test-scale.md
BENCHMARK_TIMEOUT_SECONDS ?= 7200
BENCHMARK_SCENARIOS ?=
SPEC_COUNTER_ROOT ?=
SPEC_COUNTER_JSON ?= $(PREPARED_DIR)/spec-counters.json
JOBS ?= 8
PICKER_ROOT := $(PREPARED_DIR)/tools/picker
PICKER ?= $(PICKER_ROOT)/build/bin/picker
PICKER_REF ?=
XCOMM_REF ?=
PICKER_METADATA := $(PREPARED_DIR)/tools/picker.json
PICKER_OUTPUT := $(PREPARED_DIR)/picker
PICKER_BUILD_LOCK := $(PREPARED_DIR)/.picker-build.lock
PICKER_LOCKED_MAKE = $(PYTHON) scripts/with_build_lock.py --lock $(PICKER_BUILD_LOCK) -- $(MAKE) --no-print-directory MEMBLOCK_PICKER_BUILD_LOCK_HELD=1
PICKER_OFFSET := $(PICKER_OUTPUT)/MemBlock_offset.yaml
CPP_DEFAULTS := cpp/generated_port_defaults.hpp
CPP_TYPES := cpp/generated_interface_types.hpp
CPP_ADAPTERS := cpp/environment/dut_adapter.hpp cpp/environment/dut_adapter.cpp
PICKER_HARNESS := $(PICKER_OUTPUT)/UT_MemBlock
PICKER_EXPORT_STAMP := $(PICKER_OUTPUT)/.export.stamp
PICKER_MODEL_STAMP := $(PICKER_OUTPUT)/build/UT_MemBlock/.model.stamp
PICKER_HARNESS_STAMP := $(PICKER_HARNESS)/.prepared.stamp
PICKER_TEST_BINARY := $(PICKER_HARNESS)/build/UTMemBlock_example
XSPCOMM_LIBRARY := $(PICKER_ROOT)/build/dependence/xcomm/lib/libxspcomm.so
RUNTIME_DIR := $(PREPARED_DIR)/runtime
RUNTIME_METADATA := $(RUNTIME_DIR)/runtime.json
FROZEN_RTL_METADATA := $(RUNTIME_DIR)/rtl.json
CPP_MAIN := cpp/memblock_main.cpp
CPP_ENV := cpp/memblock_env.hpp
CPP_SCENARIOS := $(sort $(wildcard cpp/scenarios/*.inc cpp/scenarios/*.hpp cpp/scenarios/*.json cpp/scenarios/*.cpp cpp/scenarios/mixed/*.hpp))
CPP_HEADERS := $(sort $(wildcard cpp/*.hpp))
SCENARIO_MAKE := cpp/scenarios/generated_targets.mk
CPP_ENV_PARTS := $(sort $(wildcard cpp/environment/*.inc cpp/environment/*.hpp cpp/environment/*.cpp))
CPP_RANDOM_PARTS := $(sort $(wildcard cpp/random/*.inc))
CPP_SUPPORT := $(CPP_SCENARIOS) $(CPP_ENV_PARTS) $(CPP_RANDOM_PARTS) $(CPP_HEADERS)
CONSTRAINT_ARGS = $(if $(CONSTRAINTS),--constraints $(CONSTRAINTS)) $(foreach item,$(CONSTRAINT),--constraint $(item))
LONG_CONSTRAINT_ARGS = --constraints $(or $(CONSTRAINTS),spec) $(foreach item,$(CONSTRAINT),--constraint $(item))

.PHONY: ports check-ports check-docs prepare-rtl check-rtl bootstrap-picker export-picker picker-model picker-harness freeze-runtime translation-fence-all translation-inflight-context-all known-bug-sentinel regression extended-regression final-regression long-final-regression endurance-regression verify-endurance-results benchmark-tests analyze-spec-counters verify-extended-results verify-final-results verify-long-results verify-coverage-results strict-coverage-regression probe-strict-coverage final-acceptance unit

ports: $(PORT_JSON) $(PORT_DOC) $(SVA) $(CPP_DEFAULTS) $(CPP_TYPES) $(CPP_ADAPTERS)

$(PORT_JSON) $(PORT_DOC) &: $(RTL) $(CONFIG) scripts/rtl_ports.py
	$(PYTHON) scripts/rtl_ports.py generate --rtl $(RTL) --config $(CONFIG) --json $(PORT_JSON) --markdown $(PORT_DOC)


include $(SCENARIO_MAKE)

$(SCENARIO_MAKE): cpp/scenarios/catalog.json scripts/scenario_catalog.py $(CPP_SCENARIOS)
	$(PYTHON) scripts/scenario_catalog.py generate-make --output $@

.PHONY: check-scenarios
check-scenarios:
	$(PYTHON) scripts/scenario_catalog.py check --output $(SCENARIO_MAKE)

$(SVA): $(PORT_JSON) scripts/generate_sva.py
	$(PYTHON) scripts/generate_sva.py generate --manifest $(PORT_JSON) --output $(SVA)

$(CPP_DEFAULTS) $(CPP_TYPES) &: $(PORT_JSON) scripts/generate_cpp.py
	$(PYTHON) scripts/generate_cpp.py generate --manifest $(PORT_JSON) --output $(CPP_DEFAULTS) --types-output $(CPP_TYPES)

$(CPP_ADAPTERS) &: config/semantic_ports.json $(PORT_JSON) $(CPP_DEFAULTS) $(CPP_TYPES) scripts/generate_semantic_ports.py
	$(PYTHON) scripts/generate_semantic_ports.py generate

check-ports:
	$(PYTHON) scripts/rtl_ports.py check --rtl $(RTL) --config $(CONFIG) --json $(PORT_JSON) --markdown $(PORT_DOC)
	$(PYTHON) scripts/generate_sva.py check --manifest $(PORT_JSON) --output $(SVA)
	$(PYTHON) scripts/generate_cpp.py check --manifest $(PORT_JSON) --output $(CPP_DEFAULTS) --types-output $(CPP_TYPES)
	$(PYTHON) scripts/generate_semantic_ports.py check

check-docs:
	$(PYTHON) scripts/check_io_coverage.py check --manifest $(PORT_JSON) --doc docs/IO_COVERAGE.md --constraints cpp/random/constraints.inc --related-doc docs/CONSTRAINED_RANDOM.md

prepare-rtl: $(PREPARED_FILELIST) $(PREPARED_METADATA)

$(PREPARED_FILELIST) $(PREPARED_METADATA) &: $(SOURCE_FILELIST) $(RTL) $(CONFIG) $(SVA) scripts/prepare_rtl.py
	$(PYTHON) scripts/prepare_rtl.py generate --source $(SOURCE_FILELIST) --bind $(SVA) --config $(CONFIG) --output $(PREPARED_FILELIST) --metadata $(PREPARED_METADATA)

check-rtl: check-ports check-docs check-scenarios
	$(PYTHON) scripts/prepare_rtl.py check --source $(SOURCE_FILELIST) --bind $(SVA) --config $(CONFIG) --output $(PREPARED_FILELIST) --metadata $(PREPARED_METADATA)

bootstrap-picker:
	$(PYTHON) scripts/bootstrap_picker.py --root $(PICKER_ROOT) --metadata $(PICKER_METADATA) --jobs $(JOBS) $(if $(PICKER_REF),--picker-ref '$(PICKER_REF)') $(if $(XCOMM_REF),--xcomm-ref '$(XCOMM_REF)')

ifeq ($(MEMBLOCK_PICKER_BUILD_LOCK_HELD),1)

.PHONY: _export-picker _picker-model _picker-harness _freeze-runtime

export-picker _export-picker: $(PICKER_EXPORT_STAMP)

$(PICKER_EXPORT_STAMP): $(PREPARED_FILELIST) $(PREPARED_METADATA) $(PORT_JSON) $(SVA) $(PICKER_METADATA) scripts/prepare_picker_export.py
	@test -x $(PICKER) || { echo "Picker is missing: $(PICKER); run make bootstrap-picker or set PICKER" >&2; exit 1; }
	@test "$(abspath $(PICKER_OUTPUT))" = "$(PREPARED_DIR)/picker"
	rm -rf -- $(PICKER_OUTPUT)
	$(PICKER) export --fs $(PREPARED_FILELIST) --lang cpp --sname MemBlock --tdir $(PICKER_OUTPUT) --autobuild false --build-threads $(JOBS) --rw mem_direct --vflag '--assert -Wno-fatal' --cp_lib true
	$(PYTHON) scripts/prepare_picker_export.py --picker-output $(PICKER_OUTPUT)
	@touch $@

picker-model _picker-model: $(PICKER_MODEL_STAMP)

$(PICKER_MODEL_STAMP): $(PICKER_EXPORT_STAMP)
	$(MAKE) -C $(PICKER_OUTPUT) compile NPROC=$(JOBS)
	@test -f $(PICKER_OUTPUT)/build/UT_MemBlock/libUTMemBlock.so
	@test -x $(PICKER_OUTPUT)/mem_direct/export.bin
	$(PICKER_OUTPUT)/mem_direct/export.bin > $(PICKER_OFFSET)
	@test -s $(PICKER_OFFSET)
	@touch $@

$(PICKER_HARNESS_STAMP): $(PICKER_MODEL_STAMP) scripts/prepare_picker_harness.py scripts/scenario_catalog.py $(CPP_MAIN) $(CPP_ENV) $(CPP_SUPPORT) $(CPP_DEFAULTS) $(CPP_TYPES) $(CPP_ADAPTERS)
	$(PYTHON) scripts/prepare_picker_harness.py --picker-output $(PICKER_OUTPUT) --main-source $(CPP_MAIN)
	@touch $@

$(PICKER_TEST_BINARY): $(PICKER_HARNESS_STAMP)
	CXXFLAGS=-I$(CURDIR)/cpp $(MAKE) -C $(PICKER_HARNESS) compile NPROC=$(JOBS)
	@test -x $@
	@touch $@

picker-harness _picker-harness: $(PICKER_TEST_BINARY)

freeze-runtime _freeze-runtime: $(RUNTIME_METADATA) $(FROZEN_RTL_METADATA)

$(RUNTIME_METADATA) $(FROZEN_RTL_METADATA) &: $(PICKER_TEST_BINARY) $(PICKER_HARNESS)/libUTMemBlock.so $(XSPCOMM_LIBRARY) $(PREPARED_METADATA) scripts/freeze_runtime.py
	$(PYTHON) scripts/freeze_runtime.py --binary $(PICKER_TEST_BINARY) --model $(PICKER_HARNESS)/libUTMemBlock.so --xspcomm $(XSPCOMM_LIBRARY) --rtl-metadata $(PREPARED_METADATA) --output $(RUNTIME_DIR)

else

.PHONY: $(PICKER_TEST_BINARY)

export-picker:
	+$(PICKER_LOCKED_MAKE) _export-picker

picker-model:
	+$(PICKER_LOCKED_MAKE) _picker-model

picker-harness $(PICKER_TEST_BINARY):
	+$(PICKER_LOCKED_MAKE) _picker-harness

freeze-runtime:
	+$(PICKER_LOCKED_MAKE) _freeze-runtime

endif

translation-fence-all:
	$(MAKE) translation-fence
	$(MAKE) translation-fence-selective
	$(MAKE) translation-fence-sv48
	$(MAKE) translation-fence-sv48-selective
	$(MAKE) translation-fence-sv39-sv48x4
	$(MAKE) translation-fence-sv39-sv48x4-selective
	$(MAKE) translation-fence-sv48-sv39x4
	$(MAKE) translation-fence-sv48-sv39x4-selective

translation-inflight-context-all:
	$(MAKE) translation-inflight-context
	$(MAKE) translation-inflight-context-sv48
	$(MAKE) translation-inflight-context-sv39-sv48x4
	$(MAKE) translation-inflight-context-sv48-sv39x4

known-bug-sentinel: $(PICKER_TEST_BINARY)
	$(MAKE) check-rtl
	$(PYTHON) scripts/check_known_bug.py --binary $(PICKER_TEST_BINARY) --timeout-seconds 300 --expect repaired-pass

regression: $(PICKER_TEST_BINARY)
	$(MAKE) check-rtl
	$(PYTHON) scripts/run_regression.py --binary $(PICKER_TEST_BINARY) --output $(REGRESSION_OUTPUT) --jobs $(or $(REGRESSION_JOBS),$(JOBS)) --seeds $(or $(SEEDS),8) --actions $(or $(ACTIONS),16384) $(CONSTRAINT_ARGS)

extended-regression: freeze-runtime
	$(MAKE) check-rtl
	$(PYTHON) scripts/run_regression.py --runtime-metadata $(RUNTIME_METADATA) --output $(EXTENDED_RESULT) --jobs $(or $(REGRESSION_JOBS),$(JOBS)) --duration-seconds $(or $(DURATION_SECONDS),21600) --timeout-seconds $(or $(TIMEOUT_SECONDS),12000) --scenarios constrained-random --actions $(or $(ACTIONS),8192) --progress-interval-seconds 60 $(LONG_CONSTRAINT_ARGS)

final-regression: freeze-runtime
	$(MAKE) check-rtl
	$(PYTHON) scripts/run_regression.py --runtime-metadata $(RUNTIME_METADATA) --output $(FINAL_RESULT) --jobs $(or $(REGRESSION_JOBS),$(JOBS)) --duration-seconds $(or $(DURATION_SECONDS),172800) --timeout-seconds $(or $(TIMEOUT_SECONDS),12000) --scenarios constrained-random --actions $(or $(ACTIONS),16384) --progress-interval-seconds 60 $(LONG_CONSTRAINT_ARGS)

long-final-regression: freeze-runtime
	$(MAKE) check-rtl
	$(PYTHON) scripts/run_regression.py --runtime-metadata $(RUNTIME_METADATA) --output $(LONG_RESULT) --jobs $(or $(REGRESSION_JOBS),$(JOBS)) --duration-seconds $(or $(DURATION_SECONDS),43200) --timeout-seconds $(or $(TIMEOUT_SECONDS),12000) --scenarios constrained-random --actions $(or $(ACTIONS),32768) --progress-interval-seconds 60 $(LONG_CONSTRAINT_ARGS)

endurance-regression: freeze-runtime
	$(MAKE) check-rtl
	$(PYTHON) scripts/run_regression.py --runtime-metadata $(RUNTIME_METADATA) --output $(ENDURANCE_RESULT) --jobs $(or $(REGRESSION_JOBS),$(JOBS)) --seeds $(ENDURANCE_SEEDS) --timeout-seconds $(ENDURANCE_TIMEOUT_SECONDS) --scenarios constrained-random --actions $(or $(ENDURANCE_ACTIONS),1000000) --progress-interval-seconds 60 $(LONG_CONSTRAINT_ARGS)

verify-endurance-results:
	$(MAKE) check-rtl
	$(PYTHON) $(VERIFY_SCRIPT) --input $(ENDURANCE_RESULT) --min-duration-seconds 1 --min-results $(ENDURANCE_SEEDS) --min-timeout-seconds 720000 --exact-results --scenario constrained-random --actions $(or $(ENDURANCE_ACTIONS),1000000) --expected-jobs $(or $(REGRESSION_JOBS),$(JOBS)) --require-backpressure --require-frozen-runtime --runtime-metadata $(RUNTIME_METADATA) --allow-finite

benchmark-tests: freeze-runtime
	$(PYTHON) scripts/benchmark_tests.py --runtime-metadata $(RUNTIME_METADATA) --output $(BENCHMARK_RESULT) --markdown $(BENCHMARK_MARKDOWN) $(if $(BENCHMARK_SCENARIOS),--scenarios $(BENCHMARK_SCENARIOS)) --seed $(or $(SEED),1) --actions $(or $(ACTIONS),16384) --timeout-seconds $(BENCHMARK_TIMEOUT_SECONDS) --constraints $(or $(CONSTRAINTS),spec) --jobs $(or $(BENCHMARK_JOBS),$(JOBS)) $(foreach item,$(CONSTRAINT),--constraint $(item))

analyze-spec-counters:
	@test -n "$(SPEC_COUNTER_ROOT)" || (echo "set SPEC_COUNTER_ROOT to a checkpoint directory or simulator_err.txt" >&2; exit 2)
	$(PYTHON) scripts/analyze_spec_counters.py $(SPEC_COUNTER_ROOT) --json $(SPEC_COUNTER_JSON)

verify-coverage-results:
	$(MAKE) check-rtl
	$(PYTHON) $(VERIFY_SCRIPT) --input $(STRICT_COVERAGE_RESULT) --min-duration-seconds 1 --min-results $(or $(SEEDS),2) --scenario constrained-random --actions $(or $(ACTIONS),16384) --expected-jobs $(or $(REGRESSION_JOBS),$(JOBS)) --require-backpressure --require-coverage --allow-finite

strict-coverage-regression:
	$(MAKE) check-rtl
	$(PYTHON) scripts/run_regression.py --binary $(PICKER_TEST_BINARY) --output $(STRICT_COVERAGE_RESULT) --jobs $(or $(REGRESSION_JOBS),$(JOBS)) --seeds $(or $(SEEDS),2) --actions $(or $(ACTIONS),16384) --constraints $(STRICT_CONSTRAINTS) $(STRICT_CONSTRAINT_ARGS)
	$(MAKE) verify-coverage-results

probe-strict-coverage:
	$(MAKE) strict-coverage-regression STRICT_CONSTRAINT='probe=1000' SEEDS=$(or $(SEEDS),1) ACTIONS=$(or $(ACTIONS),4608) STRICT_COVERAGE_RESULT=$(PROBE_STRICT_COVERAGE_RESULT)

verify-extended-results:
	$(MAKE) check-rtl
	$(PYTHON) $(VERIFY_SCRIPT) --input $(EXTENDED_RESULT) --min-duration-seconds 21600 --min-results 128 --scenario constrained-random --actions $(or $(ACTIONS),8192) --expected-jobs 8 --require-backpressure --require-frozen-runtime --runtime-metadata $(RUNTIME_METADATA)

verify-final-results:
	$(MAKE) check-rtl
	$(MAKE) dcache-release
	$(MAKE) known-bug-sentinel
	$(PYTHON) $(VERIFY_SCRIPT) --input $(FINAL_RESULT) --min-duration-seconds 172800 --min-results 128 --min-timeout-seconds 12000 --scenarios constrained-random --actions $(or $(ACTIONS),16384) --expected-jobs 8 --require-backpressure --require-frozen-runtime --runtime-metadata $(RUNTIME_METADATA)

verify-long-results:
	$(MAKE) check-rtl
	$(MAKE) dcache-release
	$(MAKE) known-bug-sentinel
	$(PYTHON) $(VERIFY_SCRIPT) --input $(LONG_RESULT) --min-duration-seconds 43200 --min-results 128 --scenarios constrained-random --actions $(or $(ACTIONS),32768) --expected-jobs 8 --require-backpressure --require-frozen-runtime --runtime-metadata $(RUNTIME_METADATA)

final-acceptance:
	$(MAKE) unit
	$(MAKE) benchmark-tests
	$(MAKE) known-bug-sentinel
	$(MAKE) final-regression
	$(MAKE) verify-final-results
	$(MAKE) endurance-regression
	$(MAKE) verify-endurance-results

unit:
	$(PYTHON) -m unittest discover -s unit -p 'test_*.py' -v
