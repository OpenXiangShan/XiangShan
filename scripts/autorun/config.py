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

from common.simulator_task_goback import SimulatorTaskGoBack

class EmuTasksConfig(SimulatorTaskGoBack):
    def __init__(self, exe: str, top_data_dir: str, task_name: str, workload: str, sub_phase: int, emu: str, max_instr: int):
        super().__init__(exe, top_data_dir, task_name, workload, sub_phase)
        self.window_size = 192

        self.add_direct_options(
            [ emu ],
        )

        self.add_direct_options(
            ['-I', str(max_instr)],
        )

        self.list_conf = [
            # '-i'
        ]

        self.core_dict = {
            # TODO
        }

        self.mem_dict = {
            # TODO
        }

        self.dict_options = {
            **self.dict_options,
            **self.core_dict,
            **self.mem_dict,
        }

        self.add_list_options(self.list_conf)
