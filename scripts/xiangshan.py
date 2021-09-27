#***************************************************************************************
# Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
# Copyright (c) 2020-2021 Peng Cheng Laboratory
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

# Simple version of xiangshan python wrapper

import argparse
import os
import random
import subprocess
import sys


class XSArgs(object):
    script_path = os.path.realpath(__file__)
    # default path to the repositories
    noop_home = os.path.join(os.path.dirname(script_path), "..")
    nemu_home = os.path.join(noop_home, "../NEMU")
    am_home = os.path.join(noop_home, "../nexus-am")
    dramsim3_home = os.path.join(noop_home, "../DRAMsim3")
    rvtest_home = os.path.join(noop_home, "../riscv-tests")

    def __init__(self, args):
        # all path environment variables that should be set
        all_path = [
            # (python argument, environment variable, default, target function)
            (None, "NOOP_HOME", self.noop_home, self.set_noop_home),
            (args.nemu, "NEMU_HOME", self.nemu_home, self.set_nemu_home),
            (args.am, "AM_HOME", self.am_home, self.set_am_home),
            (args.dramsim3, "DRAMSIM3_HOME", self.dramsim3_home, self.set_dramsim3_home),
            (args.rvtest, "RVTEST_HOME", self.rvtest_home, self.set_rvtest_home),
        ]
        for (arg_in, env, default, set_func) in all_path:
            set_func(self.__extract_path(arg_in, env, default))
        # Chisel arguments
        self.disable_log = args.disable_log
        self.dual_core = args.dual_core
        # Makefile arguments
        self.threads = args.threads
        self.with_dramsim3 = 1 if args.with_dramsim3 else None
        self.trace = 1 if args.trace else None
        self.config = args.config
        # emu arguments
        self.max_instr = args.max_instr
        self.seed = random.randint(0, 9999)
        self.numa = args.numa

    def get_env_variables(self):
        all_env = {
            "NOOP_HOME"    : self.noop_home,
            "NEMU_HOME"    : self.nemu_home,
            "AM_HOME"      : self.am_home,
            "DRAMSIM3_HOME": self.dramsim3_home
        }
        return all_env

    def get_chisel_args(self, prefix=None):
        chisel_args = [
            (self.disable_log, "disable-log"),
            (self.dual_core,   "dual-core")
        ]
        args = map(lambda x: x[1], filter(lambda arg: arg[0], chisel_args))
        if prefix is not None:
            args = map(lambda x: prefix + x, args)
        return args

    def get_makefile_args(self):
        makefile_args = [


            (self.threads, "EMU_THREADS"),
            (self.with_dramsim3, "WITH_DRAMSIM3"),
            (self.trace, "EMU_TRACE"),
            (self.config, "CONFIG")
        ]
        args = filter(lambda arg: arg[0] is not None, makefile_args)
        return args

    def get_emu_args(self):
        emu_args = [
            (self.max_instr, "max-instr"),
            (self.seed,      "seed")
        ]
        args = filter(lambda arg: arg[0] is not None, emu_args)
        return args

    def show(self):
        print("Extra environment variables:")
        env = self.get_env_variables()
        for env_name in env:
            print(f"{env_name}: {env[env_name]}")
        print()
        print("Chisel arguments:")
        print(" ".join(self.get_chisel_args()))
        print()
        print("Makefile arguments:")
        for val, name in self.get_makefile_args():
            print(f"{name}={val}")
        print()
        print("emu arguments:")
        for val, name in self.get_emu_args():
            print(f"--{name} {val}")
        print()

    def __extract_path(self, path, env=None, default=None):
        if path is None and env is not None:
            path = os.getenv(env)
        if path is None and default is not None:
            path = default
        path = os.path.realpath(path)
        return path

    def set_noop_home(self, path):
        self.noop_home = path

    def set_nemu_home(self, path):
        self.nemu_home = path

    def set_am_home(self, path):
        self.am_home = path

    def set_dramsim3_home(self, path):
        self.dramsim3_home = path

    def set_rvtest_home(self, path):
        self.rvtest_home = path

# XiangShan environment
class XiangShan(object):
    def __init__(self, args):
        self.args = XSArgs(args)

    def show(self):
        self.args.show()

    def generate_verilog(self):
        print("Generating XiangShan verilog with the following configurations:")
        self.show()
        sim_args = " ".join(self.args.get_chisel_args(prefix="--"))
        make_args = " ".join(map(lambda arg: f"{arg[1]}={arg[0]}", self.args.get_makefile_args()))
        return_code = self.__exec_cmd(f'make -C $NOOP_HOME verilog SIM_ARGS="{sim_args}" {make_args}')
        return return_code

    def build_emu(self):
        print("Building XiangShan emu with the following configurations:")
        self.show()
        sim_args = " ".join(self.args.get_chisel_args(prefix="--"))
        make_args = " ".join(map(lambda arg: f"{arg[1]}={arg[0]}", self.args.get_makefile_args()))
        return_code = self.__exec_cmd(f'make -C $NOOP_HOME emu -j200 SIM_ARGS="{sim_args}" {make_args}')
        return return_code

    def run_emu(self, workload):
        print("Running XiangShan emu with the following configurations:")
        self.show()
        emu_args = " ".join(map(lambda arg: f"--{arg[1]} {arg[0]}", self.args.get_emu_args()))
        print("workload:", workload)
        numa_args = f"numactl -m 1 -C 64-{64+self.args.threads-1}" if self.args.numa else ""
        return_code = self.__exec_cmd(f'{numa_args} $NOOP_HOME/build/emu -i {workload} {emu_args}')
        return return_code

    def run(self, args):
        if args.ci is not None:
            return self.run_ci(args.ci)
        actions = [
            (args.generate, lambda _ : self.generate_verilog()),
            (args.build, lambda _ : self.build_emu()),
            (args.workload, lambda args: self.run_emu(args.workload))
        ]
        valid_actions = map(lambda act: act[1], filter(lambda act: act[0], actions))
        for i, action in enumerate(valid_actions):
            print(f"Action {i}:")
            ret = action(args)
            if ret:
                return ret
        return 0

    def __exec_cmd(self, cmd):
        env = dict(os.environ)
        env.update(self.args.get_env_variables())
        print("subprocess call cmd:", cmd)
        return_code = subprocess.call(cmd, shell=True, env=env)
        return return_code

    def __get_ci_cputest(self, name=None):
        base_dir = os.path.join(self.args.am_home, "tests/cputest/build")
        cputest = os.listdir(base_dir)
        cputest = filter(lambda x: x.endswith(".bin"), cputest)
        cputest = map(lambda x: os.path.join(base_dir, x), cputest)
        return cputest

    def __get_ci_rvtest(self, name=None):
        base_dir = os.path.join(self.args.rvtest_home, "isa/build")
        riscv_tests = os.listdir(base_dir)
        riscv_tests = filter(lambda x: x.endswith(".bin"), riscv_tests)
        all_rv_tests = ["rv64ui", "rv64um", "rv64ua", "rv64uf", "rv64ud"]
        riscv_tests = filter(lambda x: x[:6] in all_rv_tests, riscv_tests)
        riscv_tests = map(lambda x: os.path.join(base_dir, x), riscv_tests)
        return riscv_tests

    def __get_ci_misc(self, name=None):
        base_dir = "/home/ci-runner/xsenv/workloads"
        workloads = [
            "bitmanip/bitMisc.bin",
            "coremark_rv64gc_o2/coremark-riscv64-xs.bin",
            "coremark_rv64gc_o3/coremark-riscv64-xs.bin",
            "coremark_rv64gcb_o3/coremark-riscv64-xs.bin",
            "ext_intr/amtest-riscv64-xs.bin"
        ]
        misc_tests = map(lambda x: os.path.join(base_dir, x), workloads)
        return misc_tests

    def __am_apps_path(self, bench):
        filename = f"{bench}-riscv64-noop.bin"
        return [os.path.join(self.args.am_home, "apps", bench, "build", filename)]

    def __get_ci_workloads(self, name):
        workloads = {
            "linux-hello": "bbl.bin",
            "povray": "_700480000000_.gz",
            "mcf": "_17520000000_.gz",
            "xalancbmk": "_266100000000_.gz",
            "gcc": "_39720000000_.gz",
            "namd": "_434640000000_.gz",
            "milc": "_103620000000_.gz",
            "lbm": "_140840000000_.gz",
            "gromacs": "_275480000000_.gz"
        }
        return [os.path.join("/home/ci-runner/xsenv/workloads", name, workloads[name])]

    def run_ci(self, test):
        all_tests = {
            "cputest": self.__get_ci_cputest,
            "riscv-tests": self.__get_ci_rvtest,
            "misc-tests": self.__get_ci_misc,
            "microbench": self.__am_apps_path,
            "coremark": self.__am_apps_path
        }
        for target in all_tests.get(test, self.__get_ci_workloads)(test):
            print(target)
            ret = self.run_emu(target)
            if ret:
                return ret
        return 0

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Python wrapper for XiangShan')
    parser.add_argument('workload', nargs='?', type=str, default="",
                        help='input workload file in binary format')
    # actions
    parser.add_argument('--build', action='store_true', help='build XS emu')
    parser.add_argument('--generate', action='store_true', help='generate XS verilog')
    parser.add_argument('--ci', nargs='?', type=str, const="", help='run CI tests')
    # environment variables
    parser.add_argument('--nemu', nargs='?', type=str, help='path to nemu')
    parser.add_argument('--am', nargs='?', type=str, help='path to nexus-am')
    parser.add_argument('--dramsim3', nargs='?', type=str, help='path to dramsim3')
    parser.add_argument('--rvtest', nargs='?', type=str, help='path to riscv-tests')
    # chisel arguments
    parser.add_argument('--disable-log', action='store_true', help='disable log')
    parser.add_argument('--dual-core', action='store_true', help='dual core')
    # makefile arguments
    parser.add_argument('--with-dramsim3', action='store_true', help='enable dramsim3')
    parser.add_argument('--threads', nargs='?', type=int, help='number of emu threads')
    parser.add_argument('--trace', action='store_true', help='enable waveform')
    parser.add_argument('--config', nargs='?', type=str, help='config')
    # emu arguments
    parser.add_argument('--numa', action='store_true', help='use numactl')
    parser.add_argument('--max-instr', nargs='?', type=int, help='max instr')

    args = parser.parse_args()

    xs = XiangShan(args)
    ret = xs.run(args)

    sys.exit(ret)
