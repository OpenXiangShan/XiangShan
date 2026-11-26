import os
import pathlib
import psutil
import random
import sys
import time

MEMORY_RESERVED_ABS = 20 * 1024 * 1024 * 1024
MEMORY_RESERVED_RATIO = 0.1

def check_memory():
    memory = psutil.virtual_memory()
    reserved = max(memory.total * MEMORY_RESERVED_RATIO, MEMORY_RESERVED_ABS)
    return memory.available > reserved

def get_numa_count():
    node_dir = pathlib.Path("/sys/devices/system/node/")
    return len(list(node_dir.glob("node*")))

def get_cpu_usage():
    return psutil.cpu_percent(interval=0.5, percpu=True)

def get_physical_cores():
    count = psutil.cpu_count(logical=False)
    if count is None:
        raise RuntimeError("Failed to get physical core count")
    return set(range(count))

def get_usable_cores():
    return os.sched_getaffinity(0)

def get_bound_cores():
    valid_list = ["running", "disk-sleep", "waking", "waiting"]
    cores = set()
    for proc in psutil.process_iter(["pid", "name", "cpu_affinity", "status"]):
        try:
            if not proc.info["status"] in valid_list or len(proc.info["cpu_affinity"]) > 16:
                continue
            cores |= set(proc.info["cpu_affinity"])
        except (psutil.NoSuchProcess, psutil.AccessDenied, psutil.ZombieProcess):
            pass
    return cores

def detect(n):
    if not check_memory(): # fail-fast if no enough memory
        return (False, 0, 0, 0, 0)

    numa_count = get_numa_count()
    phy_cores = get_physical_cores() # invariant, get once
    num_core = len(phy_cores)
    num_window = num_core // n
    def check(i):
        window = range(i * n, i * n + n)
        candidates = phy_cores & get_usable_cores()
        if not set(window).issubset(candidates): # fail-fast
            return False
        candidates -= get_bound_cores()
        if not set(window).issubset(candidates): # fail-fast
            return False
        core_usage = get_cpu_usage()
        window_usage = core_usage[i * n : i * n + n]
        return (
            sum(window_usage) < 30 * n and
            sum(map(lambda x: x > 80, window_usage)) < 1
        )
    for i in random.sample(range(num_window), num_window):
        if not check(i):
            continue
        time.sleep(random.uniform(0, 10))
        if not check(i):
            continue
        return (
            True,
            (int)(((i * n) % num_core) // (num_core // numa_count)),
            (int)(i * n),
            (int)(i * n + n - 1),
            num_core,
        )
    return (False, 0, 0, 0, num_core)

# get n from argv
if len(sys.argv) > 1:
    n = int(sys.argv[1])
else:
    n = int("{n}") # for inline mode, server.py will call a .format() to replace {n} with the actual value

print(detect(n))
