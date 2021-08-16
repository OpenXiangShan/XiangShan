# Copyright 2020 zyy
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

import sh
import os
import os.path as osp
from pprint import pprint
from multiprocessing import Lock

class SimulatorTaskGoBack:
    def __init__(
            self, exe: str, top_data_dir: str,
            task_name: str, workload: str, sub_phase: int):
        # options passing to simulator
        self.direct_options = []
        # options passing to python parser
        self.dict_options = dict()
        self.list_options = set()
        self.final_options = []

        self.work_dir = None
        self.log_dir = None

        assert osp.isdir(top_data_dir)
        self.top_data_dir = top_data_dir
        self.task_name = task_name

        self.workload = workload
        self.sub_phase_id = sub_phase

        self.exe = exe
        # assert osp.isfile(exe) # numactl isn't a file

        self.dry_run = False

    def set_workload(self, workload: str):
        self.workload = workload

    def add_direct_options(self, options: list):
        self.direct_options += options

    def insert_direct_options(self, options: list, index: int):
        assert(index < len(self.direct_options))
        insert_index = index
        for op in options:
            self.direct_options.insert(insert_index, op)
            insert_index += 1

    def add_dict_options(self, options: dict, replace=True):
        for k, v in options.items():
            if replace or k not in self.dict_options:
                self.dict_options[k] = v

    def add_list_options(self, options: list):
        for x in options:
            self.list_options.add(x)

    def format_options(self):
        self.final_options = self.direct_options
        self.final_options += list(self.list_options)
        for k, v in self.dict_options.items():
            self.final_options.append(f'{k}={v}')

    def workload_level_path_format(self):
        self.log_dir = f'{self.top_data_dir}/{self.task_name}/{self.workload}/'

    def sub_workload_level_path_format(self):
        self.log_dir = f'{self.top_data_dir}/{self.task_name}/{self.workload}/{self.sub_phase_id}/'

    def set_trivial_workdir(self):
        self.work_dir = self.log_dir

    def check_and_makedir(self, d):
        if not osp.isdir(d):
            assert not osp.isfile(d)
            os.makedirs(d)

    def run(self, is_goback):
        assert self.work_dir is not None
        assert self.log_dir is not None

        self.check_and_makedir(self.log_dir)
        self.check_and_makedir(self.work_dir)
        self.check_and_makedir(osp.join(self.log_dir, 'build'))

        if self.dry_run:
            pprint(self.exe)
            pprint(self.final_options)
            print('log_dir: ', self.log_dir)
            return 0

        os.chdir(self.work_dir)

        cmd = sh.Command(self.exe)

        # sh.rm(['-f', osp.join(self.log_dir, 'aborted')])
        # sh.rm(['-f', osp.join(self.log_dir, 'completed')])

        # sh.rm(['-f', osp.join(self.log_dir, 'aborted_back')])
        # sh.rm(['-f', osp.join(self.log_dir, 'completed_back')])
        sh.touch(osp.join(self.log_dir, 'running'))
        out_path = 'simulator_out.txt' if not is_goback else 'simulator_out_back.txt'
        err_path = 'simulator_err.txt' if not is_goback else 'simulator_err_back.txt'
        aborted_signal = 'aborted' if not is_goback else 'aborted_back'
        completed_signal = 'completed' if not is_goback else 'completed_back'
        print(self.final_options)
        try:
            cmd(
                _out = osp.join(self.log_dir, out_path),
                _err = osp.join(self.log_dir, err_path),
                _env = {"NOOP_HOME": self.log_dir} if is_goback else {"NOOP_HOME": "/home/ccc/XiangShan"},
                *self.final_options
            )
        except sh.ErrorReturnCode_1 as e:
            # TODO
            pass
        except sh.ErrorReturnCode_2 as e:
            print(e)
            sh.rm(osp.join(self.log_dir, 'running'))
            sh.touch(osp.join(self.log_dir, aborted_signal))
            cycle_cnt = check_simulator(osp.join(self.log_dir, out_path))
            assert(cycle_cnt != -1)
            return cycle_cnt
        except sh.ErrorReturnCode_3 as e:
            # TODO
            pass

        sh.rm(osp.join(self.log_dir, 'running'))
        sh.touch(osp.join(self.log_dir, completed_signal))
        return 0

def check_simulator(simulator_out_path: str):
    file = open(simulator_out_path, 'r')
    is_aborted = False
    for line in file.readlines():
        if line.find('cycleCnt') != -1:
            words = line.split(' ')
            cycle_cnt_index = 0
            for word in words:
                if word == 'cycleCnt':
                    assert(len(words) >= cycle_cnt_index + 3)
                    words = words[cycle_cnt_index + 2].split(',')
                    assert(len(words) == 2)
                    assert(words[1] == '')
                    file.close()
                    return int(words[0])
                else:
                    cycle_cnt_index += 1
    file.close()
    return -1
