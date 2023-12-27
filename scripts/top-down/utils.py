import os
import os.path as osp
from os.path import expanduser as expu
import re


def to_num(x: str) -> (int, float):
    if '.' in x:
        return float(x)
    return int(x)


def xs_get_stats(stat_file: str, targets: list) -> dict:

    if not os.path.isfile(expu(stat_file)):
        print(stat_file)
    assert os.path.isfile(expu(stat_file))
    with open(stat_file, encoding='utf-8') as f:
        lines = f.read().splitlines()

    if lines is None:
        return None

    patterns = {}
    accumulate_table = {}  # key: pattern, value: (count, [matched values])
    for k, p in targets.items():
        if isinstance(p, str):
            patterns[k] = re.compile(p)
        else:
            patterns[k] = re.compile(p[0])
            accumulate_table[k] = (p[1], [])
    stats = {}

    for _, line in enumerate(lines):
        for k, pattern in patterns.items():
            m = pattern.search(line)
            if m is not None:
                if k in accumulate_table:
                    accumulate_table[k][1].append(to_num(m.group(1)))
                else:
                    stats[k] = to_num(m.group(1))
                break
    for k, accumulate in accumulate_table:
        stats[k] = sum(accumulate[1][-accumulate[0]:])

    desired_keys = set(patterns.keys())
    obtained_keys = set(stats.keys())
    not_found_keys = desired_keys - obtained_keys
    if not_found_keys:
        print(stat_file)
        print(targets)
        print(not_found_keys)
    assert len(not_found_keys) == 0

    stats['ipc'] = stats['commitInstr'] / stats['total_cycles']
    return stats


def workload_point_frompath(path):
    split_path = path.split('/')[0].split('_')
    second_layer = path.split('/')[1]
    level = 1
    if second_layer.isdigit() and len(second_layer) > 1:  # workload/point/ ; work_load/point/
        workload = path.split('/')[0]
        point = second_layer
        level = 2
    elif len(split_path) == 3 and split_path[1].isdigit():  # workload_point_xxx/
        workload = split_path[0]
        point = split_path[1]
    elif len(split_path) == 4 and split_path[2].isdigit():  # work_load_point_xxx/
        workload = split_path[0] + '_' + split_path[1]
        point = split_path[2]
    else:
        workload = path.split('/')[0]
        point = '0'
        level = 1

    return workload, point, level


def strip_path(file_path: str, prefix_path: str):
    x = prefix_path.join(file_path.split(prefix_path)[1:])
    if prefix_path.startswith('.') and x.startswith('/'):
        x = x[1:]
    if prefix_path.startswith('/') and x.startswith('/'):
        x = x[1:]
    return x


def glob_stats(path: str, fname='x'):
    files = []
    # check for checkpoints conflict
    files_map = {}

    probe_stat_path = find_file_in_maze(path, fname)  # use it to probe the directory layout
    workload, point, segments = workload_point_frompath(strip_path(probe_stat_path, path))
    for l2_dir in os.listdir(path):
        l2_path = osp.join(path, l2_dir)
        # workload/point
        if segments == 2:
            # two layer directory
            for l3_dir in os.listdir(l2_path):
                l3_path = osp.join(l2_path, l3_dir)
                if not osp.isdir(l3_path):
                    continue
                stat_path = find_file_in_maze(l3_path, fname)
                if stat_path is not None:
                    workload, point, _ = workload_point_frompath(strip_path(stat_path, path))
                    point_identifier = workload + '_' + point
                    files_map.update({point_identifier: stat_path})
                    files.append((point_identifier, stat_path))
        else:
            # workload_point_xx/
            stat_path = find_file_in_maze(l2_path, fname)
            if stat_path is not None:
                workload, point, _ = workload_point_frompath(strip_path(stat_path, path))
                point_identifier = workload + '_' + point
                files_map.update({point_identifier: stat_path})
                files.append((point_identifier, stat_path))
    return files


def find_file_in_maze(path: str, stat_file='stats.txt'):
    file_path = osp.join(path, stat_file)
    if osp.isfile(file_path) or osp.islink(file_path):
        return file_path
    if not osp.isdir(path):
        return None
    for l2_dir in os.listdir(path):
        l2_path = osp.join(path, l2_dir)
        if not osp.isdir(l2_path):
            continue
        ret = find_file_in_maze(l2_path, stat_file)
        if ret is not None:
            return ret
    return None
