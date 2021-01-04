# Copyright 2020 zyy
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
