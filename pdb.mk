PDB_HOME    := $(abspath $(BUILD_DIR)/xspdb)
RTL_HOME    := $(abspath $(RTL_DIR))

PICKER_INCLUDE := $(shell picker --show_xcom_lib_location_cpp | grep include | awk '{print $$2}')
PYTHON_VERSION := $(shell python3 --version | awk '{print $$2}' | cut -d'.' -f1-2)
SIM_LDFLAGS    := $(shell cat $(PDB_HOME)/python/sim_ld_flags.txt)

PYTHON_DEPS = urwid capstone

.PHONY: pdb swig-difftest xspython xspdb xspdb-compress pdb-run check-deps

pdb: clean swig-difftest xspython xspdb

swig-difftest: sim-verilog
	$(MAKE) -C ./difftest difftest_python WITH_CONSTANTIN=0 WITH_CHISELDB=0

xspython:
	cp $(NOOP_HOME)/difftest/src/test/vsrc/common/* $(RTL_HOME)
	cp $(PDB_HOME)/python/_difftest.so $(RTL_DIR)/libdifftest.so
	find $(RTL_HOME) -maxdepth 1 \( -name "*.sv" -o -name "*.v" -o -name "*.cpp" -o -name "*.so" \) -not -name "SimTop.sv" -printf "%f\n" > $(RTL_HOME)/picker.f
	time picker export $(RTL_HOME)/SimTop.sv \
		--rw 1 \
		-w $(PDB_HOME)/xs.fst \
		--lang python \
		--tdir $(PDB_HOME)/XSPython \
		--fs $(RTL_HOME)/picker.f \
		-V "--no-timing;--threads;8;+define+DIFFTEST;-I$(NOOP_HOME)/build/generated-src;" \
		-C "-fPIC -lz -I$(PICKER_INCLUDE) -L$(RTL_HOME) -ldifftest -lpython$(PYTHON_VERSION) $(SIM_LDFLAGS)" 
		--autobuild false 
	cp $(RTL_HOME)/libdifftest.so $(PDB_HOME)/XSPython
	cd $(PDB_HOME)/XSPython/ && make

xspdb:
	cp $(PDB_HOME)/python/_difftest.so $(PDB_HOME)/XSPython
	cp $(PDB_HOME)/python/difftest.py $(PDB_HOME)/XSPython
	ln -s $(PDB_HOME)/XSPython/_difftest.so $(PDB_HOME)/XSPython/libdifftest.so
	cp -r $(NOOP_HOME)/scripts/xspdb $(PDB_HOME)/XSPdb
	mv $(PDB_HOME)/XSPython $(PDB_HOME)/XSPdb

xspdb-compress:
	tar -C $(PDB_HOME) -czvf $(PDB_HOME)/xspdb.tar.gz XSPdb

pdb-run: check-deps
	LD_PRELOAD=$(PDB_HOME)/XSPdb/XSPython/xspcomm/libxspcomm.so.0.0.1 PYTHONPATH=$(PDB_HOME)/XSPdb python3 $(PDB_HOME)/XSPdb/pdb-run.py

check-deps:
	@echo "--- Checking Python dependencies: $(PYTHON_DEPS) ---"
	@for dep in $(PYTHON_DEPS); do \
		pip show $$dep >/dev/null 2>&1 || \
			(echo "Error: Dependency '$$dep' is not installed. Please run 'pip install -r scripts/xspdb/requirements.txt'" && exit 1); \
	done
	@echo "--- All dependencies are satisfied ---"

