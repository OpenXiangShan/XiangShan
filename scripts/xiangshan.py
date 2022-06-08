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
import json
import os
import random
import signal
import subprocess
import sys
import time

import psutil


def load_all_gcpt(gcpt_path, json_path):
    all_gcpt = []
    with open(json_path) as f:
        data = json.load(f)
    for benchspec in data:
        for point in data[benchspec]:
            weight = data[benchspec][point]
            gcpt = os.path.join(gcpt_path, "_".join([benchspec, point, weight]))
            bin_dir = os.path.join(gcpt, "0")
            bin_file = list(os.listdir(bin_dir))
            assert(len(bin_file) == 1)
            bin_path = os.path.join(bin_dir, bin_file[0])
            assert(os.path.isfile(bin_path))
            all_gcpt.append(bin_path)
    return all_gcpt


class XSArgs(object):
    script_path = os.path.realpath(__file__)
    # default path to the repositories
    noop_home = os.path.join(os.path.dirname(script_path), "..")
    nemu_home = os.path.join(noop_home, "../NEMU")
    am_home = os.path.join(noop_home, "../nexus-am")
    dramsim3_home = os.path.join(noop_home, "../DRAMsim3")
    rvtest_home = os.path.join(noop_home, "../riscv-tests")
    default_wave_home = os.path.join(noop_home, "build")
    wave_home   = default_wave_home

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
        self.enable_log = args.enable_log
        self.num_cores = args.num_cores
        # Makefile arguments
        self.threads = args.threads
        self.with_dramsim3 = 1 if args.with_dramsim3 else None
        self.is_release = 1 if args.release else None
        self.trace = 1 if args.trace or not args.disable_fork else None
        self.config = args.config
        # emu arguments
        self.max_instr = args.max_instr
        self.seed = random.randint(0, 9999)
        self.numa = args.numa
        self.diff = args.diff
        self.fork = not args.disable_fork
        self.disable_diff = args.no_diff
        # wave dump path
        if args.wave_dump is not None:
            self.set_wave_home(args.wave_dump)
        else:
            self.set_wave_home(self.default_wave_home)

    def get_env_variables(self):
        all_env = {
            "NOOP_HOME"    : self.noop_home,
            "NEMU_HOME"    : self.nemu_home,
            "WAVE_HOME"    : self.wave_home,
            "AM_HOME"      : self.am_home,
            "DRAMSIM3_HOME": self.dramsim3_home,
            "MODULEPATH": "/usr/share/Modules/modulefiles:/etc/modulefiles"
        }
        return all_env

    def get_chisel_args(self, prefix=None):
        chisel_args = [
            (self.enable_log, "enable-log")
        ]
        args = map(lambda x: x[1], filter(lambda arg: arg[0], chisel_args))
        if prefix is not None:
            args = map(lambda x: prefix + x, args)
        return args

    def get_makefile_args(self):
        makefile_args = [
            (self.threads,       "EMU_THREADS"),
            (self.with_dramsim3, "WITH_DRAMSIM3"),
            (self.is_release,    "RELEASE"),
            (self.trace,         "EMU_TRACE"),
            (self.config,        "CONFIG"),
            (self.num_cores,     "NUM_CORES")
        ]
        args = filter(lambda arg: arg[0] is not None, makefile_args)
        return args

    def get_emu_args(self):
        emu_args = [
            (self.max_instr, "max-instr"),
            (self.diff,      "diff"),
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

    def set_wave_home(self, path):
        print(f"set wave home to {path}")
        self.wave_home = path

# XiangShan environment
class XiangShan(object):
    def __init__(self, args):
        self.args = XSArgs(args)
        self.timeout = args.timeout

    def show(self):
        self.args.show()

    def make_clean(self):
        print("Clean up CI workspace")
        self.show()
        return_code = self.__exec_cmd(f'make -C $NOOP_HOME clean')
        return return_code

    def generate_verilog(self):
        print("Generating XiangShan verilog with the following configurations:")
        self.show()
        sim_args = " ".join(self.args.get_chisel_args(prefix="--"))
        make_args = " ".join(map(lambda arg: f"{arg[1]}={arg[0]}", self.args.get_makefile_args()))
        return_code = self.__exec_cmd(f'make -C $NOOP_HOME verilog SIM_ARGS="{sim_args}" {make_args}')
        return return_code

    def generate_sim_verilog(self):
        print("Generating XiangShan sim-verilog with the following configurations:")
        self.show()
        sim_args = " ".join(self.args.get_chisel_args(prefix="--"))
        make_args = " ".join(map(lambda arg: f"{arg[1]}={arg[0]}", self.args.get_makefile_args()))
        return_code = self.__exec_cmd(f'make -C $NOOP_HOME sim-verilog SIM_ARGS="{sim_args}" {make_args}')
        return return_code

    def build_emu(self):
        print("Building XiangShan emu with the following configurations:")
        self.show()
        sim_args = " ".join(self.args.get_chisel_args(prefix="--"))
        make_args = " ".join(map(lambda arg: f"{arg[1]}={arg[0]}", self.args.get_makefile_args()))
        return_code = self.__exec_cmd(f'make -C $NOOP_HOME emu -j200 SIM_ARGS="{sim_args}" {make_args}')
        return return_code

    def build_simv(self):
        print("Building XiangShan simv with the following configurations")
        self.show()
        make_args = " ".join(map(lambda arg: f"{arg[1]}={arg[0]}", self.args.get_makefile_args()))
        # TODO: make the following commands grouped as unseen scripts
        return_code = self.__exec_cmd(f'\
            eval `/usr/bin/modulecmd zsh load license`;\
            eval `/usr/bin/modulecmd zsh load synopsys/vcs/Q-2020.03-SP2`;\
            eval `/usr/bin/modulecmd zsh load synopsys/verdi/S-2021.09-SP1`;\
            VERDI_HOME=/nfs/tools/synopsys/verdi/S-2021.09-SP1 \
            make -C $NOOP_HOME simv {make_args} CONSIDER_FSDB=1')  # set CONSIDER_FSDB for compatibility
        return return_code

    def run_emu(self, workload):
        print("Running XiangShan emu with the following configurations:")
        self.show()
        emu_args = " ".join(map(lambda arg: f"--{arg[1]} {arg[0]}", self.args.get_emu_args()))
        print("workload:", workload)
        numa_args = ""
        if self.args.numa:
            numa_info = get_free_cores(self.args.threads)
            numa_args = f"numactl -m {numa_info[0]} -C {numa_info[1]}-{numa_info[2]}"
        fork_args = "--enable-fork" if self.args.fork else ""
        diff_args = "--no-diff" if self.args.disable_diff else ""
        return_code = self.__exec_cmd(f'{numa_args} $NOOP_HOME/build/emu -i {workload} {emu_args} {fork_args} {diff_args}')
        return return_code

    def run_simv(self, workload):
        print("Running XiangShan simv with the following configurations:")
        self.show()
        diff_args = "$NOOP_HOME/"+ args.diff
        return_code = self.__exec_cmd(f'$NOOP_HOME/difftest/simv +workload={workload} +diff={diff_args}')
        return return_code

    def run(self, args):
        if args.ci is not None:
            return self.run_ci(args.ci)
        if args.ci_vcs is not None:
            return self.run_ci_vcs(args.ci_vcs)
        actions = [
            (args.generate, lambda _ : self.generate_verilog()),
            (args.vcs_gen, lambda _ : self.generate_sim_verilog()),
            (args.build, lambda _ : self.build_emu()),
            (args.vcs_build, lambda _ : self.build_simv()),
            (args.workload, lambda args: self.run_emu(args.workload)),
            (args.clean, lambda _ : self.make_clean())
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
        start = time.time()
        proc = subprocess.Popen(cmd, shell=True, env=env, preexec_fn=os.setsid)
        try:
            return_code = proc.wait(self.timeout)
            end = time.time()
            print(f"Elapsed time: {end - start} seconds")
            return return_code
        except (KeyboardInterrupt, subprocess.TimeoutExpired):
            os.killpg(os.getpgid(proc.pid), signal.SIGINT)
            print(f"KeyboardInterrupt or TimeoutExpired.")
            return 0

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
        base_dir = "/nfs/home/share/ci-workloads"
        workloads = [
            "bitmanip/bitMisc.bin",
            "crypto/crypto-riscv64-noop.bin",
            "coremark_rv64gc_o2/coremark-riscv64-xs.bin",
            "coremark_rv64gc_o3/coremark-riscv64-xs.bin",
            "coremark_rv64gcb_o3/coremark-riscv64-xs.bin",
            "ext_intr/amtest-riscv64-xs.bin",
            "cache-alias/aliastest-riscv64-xs.bin",
            "Svinval/rv64mi-p-svinval.bin",
            "pmp/pmp.riscv.bin",
            "asid/asid.bin",
            "isa_misc/xret_clear_mprv.bin",
            "isa_misc/satp_ppn.bin",
            "cache-management/softprefetchtest-riscv64-xs.bin"
        ]
        misc_tests = map(lambda x: os.path.join(base_dir, x), workloads)
        return misc_tests

    def __get_ci_mc(self, name=None):
        base_dir = "/nfs/home/share/ci-workloads"
        workloads = [
            "dualcoretest/ldvio-riscv64-xs.bin"
        ]
        mc_tests = map(lambda x: os.path.join(base_dir, x), workloads)
        return mc_tests

    def __get_ci_nodiff(self, name=None):
        base_dir = "/nfs/home/share/ci-workloads"
        workloads = [
            "cache-management/cacheoptest-riscv64-xs.bin"
        ]
        tests = map(lambda x: os.path.join(base_dir, x), workloads)
        return tests

    def __am_apps_path(self, bench):
        filename = f"{bench}-riscv64-noop.bin"
        return [os.path.join(self.args.am_home, "apps", bench, "build", filename)]

    def __get_ci_workloads(self, name):
        workloads = {
            "linux-hello": "bbl.bin",
            "linux-hello-smp": "bbl.bin",
            "povray": "_700480000000_.gz",
            "mcf": "_17520000000_.gz",
            "xalancbmk": "_266100000000_.gz",
            "gcc": "_39720000000_.gz",
            "namd": "_434640000000_.gz",
            "milc": "_103620000000_.gz",
            "lbm": "_140840000000_.gz",
            "gromacs": "_275480000000_.gz",
            "wrf": "_1916220000000_.gz",
            "astar": "_122060000000_.gz"
        }
        if name in workloads:
            return [os.path.join("/nfs/home/share/ci-workloads", name, workloads[name])]
        # select a random SPEC checkpoint
        assert(name == "random")
        all_cpt = [
            "/nfs-nvme/home/share/checkpoints_profiles/spec06_rv64gcb_o2_20m/take_cpt",
            "/nfs-nvme/home/share/checkpoints_profiles/spec06_rv64gcb_o3_20m/take_cpt",
            "/nfs-nvme/home/share/checkpoints_profiles/spec06_rv64gc_o2_20m/take_cpt",
            "/nfs-nvme/home/share/checkpoints_profiles/spec06_rv64gc_o2_50m/take_cpt",
            "/nfs-nvme/home/share/checkpoints_profiles/spec17_rv64gcb_o2_20m/take_cpt",
            "/nfs-nvme/home/share/checkpoints_profiles/spec17_rv64gcb_o3_20m/take_cpt",
            "/nfs-nvme/home/share/checkpoints_profiles/spec17_rv64gc_o2_50m/take_cpt"
        ]
        all_json = [
            "/nfs-nvme/home/share/checkpoints_profiles/spec06_rv64gcb_o2_20m/json/simpoint_summary.json",
            "/nfs-nvme/home/share/checkpoints_profiles/spec06_rv64gcb_o3_20m/simpoint_summary.json",
            "/nfs-nvme/home/share/checkpoints_profiles/spec06_rv64gc_o2_20m/simpoint_summary.json",
            "/nfs-nvme/home/share/checkpoints_profiles/spec06_rv64gc_o2_50m/simpoint_summary.json",
            "/nfs-nvme/home/share/checkpoints_profiles/spec17_rv64gcb_o2_20m/simpoint_summary.json",
            "/nfs-nvme/home/share/checkpoints_profiles/spec17_rv64gcb_o3_20m/simpoint_summary.json",
            "/nfs-nvme/home/share/checkpoints_profiles/spec17_rv64gc_o2_50m/simpoint_summary.json"
        ]
        assert(len(all_cpt) == len(all_json))
        cpt_path, json_path = random.choice(list(zip(all_cpt, all_json)))
        all_gcpt = load_all_gcpt(cpt_path, json_path)
        return [random.choice(all_gcpt)]

    def run_ci(self, test):
        all_tests = {
            "cputest": self.__get_ci_cputest,
            "riscv-tests": self.__get_ci_rvtest,
            "misc-tests": self.__get_ci_misc,
            "mc-tests": self.__get_ci_mc,
            "nodiff-tests": self.__get_ci_nodiff,
            "microbench": self.__am_apps_path,
            "coremark": self.__am_apps_path
        }
        for target in all_tests.get(test, self.__get_ci_workloads)(test):
            print(target)
            ret = self.run_emu(target)
            if ret:
                if self.args.default_wave_home != self.args.wave_home:
                    print("copy wave file to " + self.args.wave_home)
                    self.__exec_cmd(f"cp $NOOP_HOME/build/*.vcd $WAVE_HOME")
                    self.__exec_cmd(f"cp $NOOP_HOME/build/emu $WAVE_HOME")
                    self.__exec_cmd(f"cp $NOOP_HOME/build/SimTop.v $WAVE_HOME")
                return ret
        return 0

    def run_ci_vcs(self, test):
        all_tests = {
            "cputest": self.__get_ci_cputest,
            "riscv-tests": self.__get_ci_rvtest,
            "misc-tests": self.__get_ci_misc,
            "mc-tests": self.__get_ci_mc,
            "nodiff-tests": self.__get_ci_nodiff,
            "microbench": self.__am_apps_path,
            "coremark": self.__am_apps_path
        }
        for target in all_tests.get(test, self.__get_ci_workloads)(test):
            print(target)
            ret = self.run_simv(target)
            if ret:
                if self.args.default_wave_home != self.args.wave_home:
                    print("copy wave file to " + self.args.wave_home)
                    self.__exec_cmd(f"cp $NOOP_HOME/build/*.vcd $WAVE_HOME")
                    self.__exec_cmd(f"cp $NOOP_HOME/build/emu $WAVE_HOME")
                    self.__exec_cmd(f"cp $NOOP_HOME/build/SimTop.v $WAVE_HOME")
                return ret
        return 0

def get_free_cores(n):
    while True:
        # To avoid potential conflicts, we allow CI to use SMT.
        num_logical_core = psutil.cpu_count(logical=True)
        core_usage = psutil.cpu_percent(interval=1, percpu=True)
        num_window = num_logical_core // n
        for i in range(num_window):
            window_usage = core_usage[i * n : i * n + n]
            if sum(window_usage) < 0.3 * n and True not in map(lambda x: x > 0.5, window_usage):
                return (((i * n) % 128)// 64, i * n, i * n + n - 1)
        print(f"No free {n} cores found. CPU usage: {core_usage}\n")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Python wrapper for XiangShan')
    parser.add_argument('workload', nargs='?', type=str, default="",
                        help='input workload file in binary format')
    # actions
    parser.add_argument('--build', action='store_true', help='build XS emu')
    parser.add_argument('--generate', action='store_true', help='generate XS verilog')
    parser.add_argument('--vcs-gen', action='store_true', help='generate XS sim verilog for vcs')
    parser.add_argument('--vcs-build', action='store_true', help='build XS simv')
    parser.add_argument('--ci', nargs='?', type=str, const="", help='run CI tests')
    parser.add_argument('--ci-vcs', nargs='?', type=str, const="", help='run CI tests on simv')
    parser.add_argument('--clean', action='store_true', help='clean up XiangShan CI workspace')
    parser.add_argument('--timeout', nargs='?', type=int, default=None, help='timeout (in seconds)')
    # environment variables
    parser.add_argument('--nemu', nargs='?', type=str, help='path to nemu')
    parser.add_argument('--am', nargs='?', type=str, help='path to nexus-am')
    parser.add_argument('--dramsim3', nargs='?', type=str, help='path to dramsim3')
    parser.add_argument('--rvtest', nargs='?', type=str, help='path to riscv-tests')
    parser.add_argument('--wave-dump', nargs='?', type=str , help='path to dump wave')
    # chisel arguments
    parser.add_argument('--enable-log', action='store_true', help='enable log')
    parser.add_argument('--num-cores', type=int, help='number of cores')
    # makefile arguments
    parser.add_argument('--release', action='store_true', help='enable release')
    parser.add_argument('--with-dramsim3', action='store_true', help='enable dramsim3')
    parser.add_argument('--threads', nargs='?', type=int, help='number of emu threads')
    parser.add_argument('--trace', action='store_true', help='enable waveform')
    parser.add_argument('--config', nargs='?', type=str, help='config')
    # emu arguments
    parser.add_argument('--numa', action='store_true', help='use numactl')
    parser.add_argument('--diff', nargs='?', default="./ready-to-run/riscv64-nemu-interpreter-so", type=str, help='nemu so')
    parser.add_argument('--max-instr', nargs='?', type=int, help='max instr')
    parser.add_argument('--disable-fork', action='store_true', help='disable lightSSS')
    parser.add_argument('--no-diff', action='store_true', help='disable difftest')

    args = parser.parse_args()

    xs = XiangShan(args)
    ret = xs.run(args)

    sys.exit(ret)
