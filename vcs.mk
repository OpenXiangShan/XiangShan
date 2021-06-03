#***************************************************************************************
# Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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

VCS_TARGET = simv

VCS_CSRC_DIR = $(abspath ./src/test/csrc/vcs)
VCS_CXXFILES = $(SIM_CXXFILES) $(DIFFTEST_CXXFILES) $(shell find $(VCS_CSRC_DIR) -name "*.cpp")
VCS_CXXFLAGS += -std=c++11 -static -Wall -I$(VCS_CSRC_DIR) -I$(SIM_CSRC_DIR) -I$(DIFFTEST_CSRC_DIR)
VCS_LDFLAGS  += -lpthread -lSDL2 -ldl -lz

VCS_VSRC_DIR = $(abspath ./src/test/vsrc/vcs)
VCS_VFILES   = $(SIM_VSRC) $(shell find $(VCS_VSRC_DIR) -name "*.v")

VCS_SEARCH_DIR = $(abspath ./build)
VCS_BUILD_DIR  = $(abspath ./build/simv-compile)

VCS_FLAGS += -full64 +v2k -timescale=1ns/1ns -sverilog -debug_access+all +lint=TFIPC-L
# randomize all undefined signals (instead of using X)
VCS_FLAGS += +vcs+initreg+random
# VCS_FLAGS += +define+RANDOMIZE_GARBAGE_ASSIGN +define+RANDOMIZE_INVALID_ASSIGN
# VCS_FLAGS += +define+RANDOMIZE_MEM_INIT +define+RANDOMIZE_DELAY=0 +define+RANDOMIZE_REG_INIT
# SRAM lib defines
# VCS_FLAGS += +define+UNIT_DELAY +define+no_warning
# C++ flags
VCS_FLAGS += -CFLAGS "$(VCS_CXXFLAGS)" -LDFLAGS "$(VCS_LDFLAGS)" -j200
# search build for other missing verilog files
VCS_FLAGS += -y $(VCS_SEARCH_DIR) +libext+.v
# build files put into $(VCS_BUILD_DIR)
VCS_FLAGS += -Mdir=$(VCS_BUILD_DIR)

$(VCS_TARGET): $(SIM_TOP_V) $(VCS_CXXFILES) $(VCS_VFILES)
	vcs $(VCS_FLAGS) $(SIM_TOP_V) $(VCS_CXXFILES) $(VCS_VFILES)

vcs-clean:
	rm -rf simv csrc DVEfiles simv.daidir stack.info.* ucli.key $(VCS_BUILD_DIR)

