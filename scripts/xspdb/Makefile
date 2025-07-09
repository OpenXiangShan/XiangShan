# Makefile 
NOOP_HOME = $(abspath ../../)
#RTL_HOME = $(NOOP)/build/rtl
RTL_HOME = $(abspath ../../build/rtl)
PDB_HOME = $(abspath ../../scripts/xspdb)

PICKER_INCLUDE = $(shell picker --show_xcom_lib_location_cpp|grep include|awk '{print $$2}')
PYTHON_VERSION  = $(shell python3 --version|awk '{print $$2}'|cut -d'.' -f1-2)
#RTL_HOME  = $(abspath ../../build/rtl)
SIM_LDFLAGS    = $(shell cat $(NOOP_HOME)/build/python/sim_ld_flags.txt)

export LD_LIBRARY_PATH := ${lib_diff_path}

all: prepare_env update_env build_xspython update_lib build_xspdb archive_xspdb

prepare_env:
	cd $(NOOP_HOME) && make sim-verilog WITH_CONSTANTIN=0 WITH_CHISELDB=0 JVM_XMX=16g
	cd $(NOOP_HOME) && make -C difftest/ difftest_python WITH_CONSTANTIN=0 WITH_CHISELDB=0

update_env:
	cp $(NOOP_HOME)/difftest/src/test/vsrc/common/* $(RTL_HOME)/
	cp $(NOOP_HOME)/build/xspdb/python/_difftest.so $(NOOP_HOME)/build/rtl/libdifftest.so
	cd $(RTL_HOME) && ls|grep -E "\.sv$$|\.v$$|\.cpp$$|\.so$$"|grep -v "SimTop\.sv" > picker.f

build_xspython:
	picker export $(RTL_HOME)/SimTop.sv --rw 1 -w xs.fst --lang python --tdir XSIT --fs $(RTL_HOME)/picker.f -V "--no-timing;--threads;8;+define+DIFFTEST" -C "-fPIC -lz -I$(PICKER_INCLUDE) -L$(RTL_HOME) -ldifftest -lpython${PYTHON_VERSION} ${SIM_LDFLAGS}"

update_lib:
	cd XSIT && rm -rf _difftest.so libdifftest.so difftest.py
	cd XSIT && cp $(NOOP_HOME)/build/xspdb/python/_difftest.so .
	cd XSIT && ln -s _difftest.so libdifftest.so
	cd XSIT && cp $(NOOP_HOME)/build/xspdb/python/difftest.py .

build_xspdb:
	rm -rf XSPdb
	git clone https://github.com/OpenXiangShan/XSPdb.git 
	cd XSPdb && wget https://github.com/OpenXiangShan/XSPdb/releases/download/v0.1.0-test/ready-to-run.tar.gz 
	cd XSPdb && tar zxvf ready-to-run.tar.gz 

archive_xspdb:
	tar czvf XSPdb.tar.gz XSPdb

clean:
	rm -rf XSIT
	rm -rf __pycache__

