# Copyright 2020 zyy
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
