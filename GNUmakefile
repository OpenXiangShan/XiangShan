include Makefile

BASEBENCHMARK_DIR := $(CURDIR)/test/BaseBenchmark

.PHONY: basebuild baserun base

basebuild:
	$(BASEBENCHMARK_DIR)/build-xs-workloads.sh

baserun:
	$(BASEBENCHMARK_DIR)/run-xs-workloads.sh

base: basebuild
	$(BASEBENCHMARK_DIR)/run-xs-workloads.sh
