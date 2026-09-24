#!/usr/bin/env python3

class statutils:
    def __init__(self, stat_path, tqdm=False):
        self.stat_path = stat_path
        with open(stat_path, 'r') as f:
            current_pc = None
            current_time = None
            last_counter = dict() # (module, counter, occurrance) -> last_value
            # Note: A counter may occur multiple times in one time stamp, we need to distinguish them by occurrance count
            occurrance_count = dict() # (module, counter) -> occurrance
            pending = dict() # (module, counter) -> delta
            self.stat = dict() # pc -> (module, counter) -> total
            line = f.readlines()
            if tqdm:
                from tqdm import tqdm
                iterator = tqdm(line, desc="Parsing stat file")
            for line in iterator if tqdm else line:
                prefix = "[PERF ][time="
                if not line.startswith(prefix):
                    continue
                after_time_pos = len(prefix) + line[len(prefix):].index(']')
                time = int(line[len(prefix):after_time_pos])
                if time != current_time:
                    if current_pc is not None and current_time is not None:
                        pending[(None, "cycles")] = time - current_time
                        for key in pending:
                            if current_pc not in self.stat:
                                self.stat[current_pc] = dict()
                            if key not in self.stat[current_pc]:
                                self.stat[current_pc][key] = 0
                            self.stat[current_pc][key] += pending[key]
                    current_pc = None
                    pending = dict()
                    occurrance_count = dict()
                    current_time = time
                after_module_pos = line.index(':')
                module = line[after_time_pos + 1:after_module_pos].strip()
                counter, value = line[after_module_pos + 1:].strip().split(',')
                value = int(value)
                if counter == "robHeadPC":
                    current_pc = value
                else:
                    key = (module, counter)
                    if key not in occurrance_count:
                        occurrance_count[key] = 0
                    else:
                        occurrance_count[key] += 1
                    key_with_occurrance = (module, counter, occurrance_count[key])
                    last_value = last_counter.get(key_with_occurrance, 0)
                    delta = value - last_value
                    if delta > 0:
                        pending[key] = delta
                    last_counter[key_with_occurrance] = value
        self.sum_events = dict() # counter -> total
        for pc in self.stat:
            for key in self.stat[pc]:
                if key[1] not in self.sum_events:
                    self.sum_events[key[1]] = 0
                self.sum_events[key[1]] += self.stat[pc][key]
        self.stat2 = None

    def get_stat(self):
        return self.stat

    def get_stat_devide_by_function_and_bb(self, search_func: callable):
        if self.stat2 is not None:
            return self.stat2
        self.stat2 = dict() # (function_name, bb_start, bb_size) -> (module, counter) -> total
        # search_func: (addr: int) -> (function_name: str, bb_start: int, bb_size: int)
        for pc in self.stat:
            func_name, bb_start, bb_size = search_func(pc)
            if func_name is None:
                continue
            key = (func_name, bb_start, bb_size)
            if key not in self.stat2:
                self.stat2[key] = dict()
            for stat_key in self.stat[pc]:
                if stat_key not in self.stat2[key]:
                    self.stat2[key][stat_key] = 0
                self.stat2[key][stat_key] += self.stat[pc][stat_key]
        return self.stat2
    
    def get_top_functions_by_counter(self, counter_key: str, top_limit: float = 0.01):
        func_totals = dict() # function_name -> (total_count, percentage)
        assert self.stat2 is not None, "Please call get_stat_devide_by_function_and_bb first"
        for func, bb_start, bb_size in self.stat2:
            for module, counter in self.stat2[(func, bb_start, bb_size)]:
                if counter == counter_key:
                    if func not in func_totals:
                        func_totals[func] = 0
                    func_totals[func] += self.stat2[(func, bb_start, bb_size)][(module, counter)]
        for func in func_totals:
            func_totals[func] = (func_totals[func], func_totals[func] / self.sum_events.get(counter_key, 1))
        sorted_funcs = sorted(func_totals.items(), key=lambda item: item[1][0], reverse=True)
        return [(func, total, percentage) for func, (total, percentage) in sorted_funcs if percentage >= top_limit]
    
    def print_top_functions_by_counter(self, counter_key: str, top_limit: float = 0.01):
        top_funcs = self.get_top_functions_by_counter(counter_key, top_limit)
        print(f"Top functions by counter '{counter_key}':")
        for func, total, percentage in top_funcs:
            print(f"{percentage*100:04.2f}%, {total:8d} - Function: {func}")

    def get_top_bbs_by_counter(self, counter_key: str, top_limit: float = 0.01):
        bb_totals = dict() # (function_name, bb_start, bb_size) -> (total_count, percentage)
        assert self.stat2 is not None, "Please call get_stat_devide_by_function_and_bb first"
        for func, bb_start, bb_size in self.stat2:
            key = (func, bb_start, bb_size)
            for module, counter in self.stat2[key]:
                if counter == counter_key:
                    if key not in bb_totals:
                        bb_totals[key] = 0
                    bb_totals[key] += self.stat2[key][(module, counter)]
        sorted_bbs = sorted(bb_totals.items(), key=lambda item: item[1], reverse=True)
        return [(func, bb_start, bb_size, total, percentage) for (func, bb_start, bb_size), total in sorted_bbs if (percentage := total / self.sum_events.get(counter_key, 1)) >= top_limit]

    def print_top_bbs_by_counter(self, counter_key: str, top_limit: float = 0.01):
        top_bbs = self.get_top_bbs_by_counter(counter_key, top_limit)
        print(f"Top basic blocks by counter '{counter_key}':")
        for func, bb_start, bb_size, total, percentage in top_bbs:
            print(f"{percentage*100:04.2f}%, {total:8d} - Function: {func} - 0x{bb_start:x}")
