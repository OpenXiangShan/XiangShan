#***************************************************************************************
# Copyright (c) 2025 Beijing Institute of Open Source Chip (BOSC)
# Copyright (c) 2025 Institute of Computing Technology, Chinese Academy of Sciences
#
# XiangShan is licensed under Mulan PSL v2.
# You can use this software according to the terms and conditions of the Mulan PSL v2.
# You may obtain a copy of Mulan PSL v2 at:
#          http://license.coscl.org.cn/MulanPSL2
#
# THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
# EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
# MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
#
# See the Mulan PSL v2 for more details.
#***************************************************************************************

PDB_HOME          := $(abspath $(BUILD_DIR)/xspdb)
RTL_HOME          := $(abspath $(RTL_DIR))
DIFFTEST_HOME     := $(abspath $(PDB_HOME)/pydifftest)

DIFFTEST_INCLUDE   = $(abspath difftest/src/test/vsrc/common)
PICKER_INCLUDE     = $(shell picker --show_xcom_lib_location_cpp | grep include | awk '{print $$2}')
PYTHON_VERSION     = $(shell python3 --version | awk '{print $$2}' | cut -d'.' -f1-2)
SIM_LDFLAGS        = $(shell cat $(DIFFTEST_HOME)/sim_ld_flags.txt)

PYTHON_DEPS        = urwid capstone

.PHONY: pdb $(DIFFTEST_HOME)/_difftest.so $(PDB_HOME)/picker.f $(PDB_HOME)/libUTSimTop.so $(PDB_HOME)/xspdb $(PDB_HOME)/xspdb.tar.gz pdb-run check-deps

pdb: $(DIFFTEST_HOME)/_difftest.so $(PDB_HOME)/picker.f $(PDB_HOME)/libUTSimTop.so

$(DIFFTEST_HOME)/_difftest.so: sim-verilog
	$(MAKE) -C ./difftest pydifftest WITH_CONSTANTIN=0 WITH_CHISELDB=0
	ln -s ../pydifftest/_difftest.so $(DIFFTEST_HOME)/libdifftest.so

$(PDB_HOME)/picker.f:
	find $(realpath $(RTL_HOME)) -maxdepth 1 \( -name "*.sv" -o -name "*.v" -o -name "*.cpp" -o -name "*.so" \) -not -name "SimTop.sv" -printf "%p\n" > $(PDB_HOME)/picker.f
	find $(realpath $(DIFFTEST_INCLUDE)) -maxdepth 1 \( -name "*.v" \) -not -name "SimTop.sv" -printf "%p\n" >> $(PDB_HOME)/picker.f
	echo "$(DIFFTEST_HOME)/libdifftest.so" >> $(PDB_HOME)/picker.f

$(PDB_HOME)/libUTSimTop.so:
	time picker export $(RTL_HOME)/SimTop.sv \
		--rw 1 \
		-w $(PDB_HOME)/xs.fst \
		--lang python \
		--tdir $(PDB_HOME)/pyxscore \
		--fs $(PDB_HOME)/picker.f \
		-V "--no-timing;--threads;128;+define+DIFFTEST;-I$(NOOP_HOME)/build/generated-src" \
		-C "-fPIC -lz -I$(PICKER_INCLUDE) -L$(PDB_HOME)/pyxscore -ldifftest -lpython$(PYTHON_VERSION) $(SIM_LDFLAGS)" \
		--autobuild=false
	ln -s ../pydifftest/_difftest.so $(PDB_HOME)/pyxscore/libdifftest.so
	$(MAKE) -C $(PDB_HOME)/pyxscore

$(PDB_HOME)/xspdb:
	rm -rf $(PDB_HOME)/xspdb
	cp -r $(NOOP_HOME)/scripts/xspdb $(PDB_HOME)/xspdb
	cp -r $(PDB_HOME)/pydifftest $(PDB_HOME)/xspdb/src
	cp -r $(PDB_HOME)/pyxscore $(PDB_HOME)/xspdb/src

$(PDB_HOME)/xspdb.tar.gz: $(PDB_HOME)/xspdb
	rm -rf $(PDB_HOME)/xspdb.tar.gz
	tar -C $(PDB_HOME) -czvf $(PDB_HOME)/xspdb.tar.gz xspdb

pdb-run: check-deps
	LD_PRELOAD="$(PDB_HOME)/pyxscore/xspcomm/libxspcomm.so.0.0.1 $(PDB_HOME)/pyxscore/libdifftest.so" PYTHONPATH="$(PDB_HOME):scripts/xspdb/src" python3 scripts/xspdb/pdb-run.py

check-deps:
	@echo "--- Checking Python dependencies: $(PYTHON_DEPS) ---"
	@for dep in $(PYTHON_DEPS); do \
		pip show $$dep >/dev/null 2>&1 || \
			(echo "Error: Dependency '$$dep' is not installed. Please run 'pip install -r scripts/xspdb/requirements.txt'" && exit 1); \
	done
	@echo "--- All dependencies are satisfied ---"

