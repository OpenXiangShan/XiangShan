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


# example task tree structure:
ExampleTask = {
    'gcc': {
        '0': '/path/to/gcc/0/cpt0.gz',
        '1': '/path/to/gcc/1/cpt0.gz',
    }
}

def task_tree_to_batch_task(
        task,
        batch_task_desc: dict,
        exe: str, top_data_dir: str, batch_task_name: str, emu: str, max_instr: int):
    tasks = []
    for workload, cpts in batch_task_desc.items():
        for cpt_id, cpt_file in cpts.items():
            tasks.append(task(exe, top_data_dir, batch_task_name, workload, cpt_id, emu, max_instr))
    return tasks
