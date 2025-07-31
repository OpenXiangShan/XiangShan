#coding=utf-8

import os
import inspect
import importlib.util
import sys
import logging

RESET = "\033[0m"
GREEN = "\033[32m"
RED = "\033[31m"
YELLOW = "\033[33m"

_xspdb_enable_log = False
_xspdb_logger = None

def xspdb_set_log(xspdb_enable_log:bool):
    global _xspdb_enable_log
    global _xspdb_logger

    _xspdb_enable_log = xspdb_enable_log
    if _xspdb_enable_log and _xspdb_logger is None:
        #initialize logger
        _xspdb_logger = logging.getLogger("XSPdb")
        _xspdb_logger.setLevel(logging.DEBUG)
        ch = logging.FileHandler("XSPdb.log")
        ch.setLevel(logging.DEBUG)
        formatter = logging.Formatter('%(message)s')
        ch.setFormatter(formatter)
        _xspdb_logger.addHandler(ch)

def xspdb_set_log_file(log_file:str):
    global _xspdb_logger
    if _xspdb_logger is None:
        _xspdb_logger = logging.getLogger("XSPdb")
        _xspdb_logger.setLevel(logging.DEBUG)
        ch = logging.FileHandler(log_file)
        ch.setLevel(logging.DEBUG)
        formatter = logging.Formatter('%(message)s')
        ch.setFormatter(formatter)
        _xspdb_logger.addHandler(ch)
    else:
        for handler in _xspdb_logger.handlers:
            if isinstance(handler, logging.FileHandler):
                handler.close()
                _xspdb_logger.removeHandler(handler)
        ch = logging.FileHandler(log_file)
        ch.setLevel(logging.DEBUG)
        formatter = logging.Formatter('%(message)s')
        ch.setFormatter(formatter)
        _xspdb_logger.addHandler(ch)

_XSPDB_LOG_LEVEL = logging.DEBUG

def set_xspdb_log_level(level):
    """
    Set the log level for XSPdb.

    Args:
        level (int): Log level to set. Use logging.DEBUG, logging.INFO, etc.
    """
    global _xspdb_logger
    if _xspdb_logger is not None:
        _xspdb_logger.setLevel(level)
        for handler in _xspdb_logger.handlers:
            handler.setLevel(level)

def set_xspdb_debug_level(level):
    global _XSPDB_LOG_LEVEL
    _XSPDB_LOG_LEVEL = level
    set_xspdb_log_level(level)

def log_message(*a, **k):
    """Print a message to log"""
    if _xspdb_enable_log:
        old_level = _xspdb_logger.level
        set_xspdb_log_level(logging.INFO)
        _xspdb_logger.info(*a, **k)
        set_xspdb_log_level(old_level)

def message(*a, **k):
    """Print a message"""
    k["flush"] = True
    print(*a, **k)
    del k["flush"]
    log_message(*a, **k)

def info(msg):
    """Print information"""
    if _XSPDB_LOG_LEVEL <= logging.INFO:
        print(f"{GREEN}[Info] %s{RESET}" % msg, flush=True)
    if _xspdb_enable_log:
        _xspdb_logger.info("[Info] %s" % msg)

def debug(msg):
    """Print debug information"""
    if _XSPDB_LOG_LEVEL <= logging.DEBUG:
        print("[Debug] %s" % msg, flush=True)
    if _xspdb_enable_log:
        _xspdb_logger.debug("[Debug] %s" % msg)

def error(msg):
    """Print error information"""
    if _XSPDB_LOG_LEVEL <= logging.ERROR:
        print(f"{RED}[Error] %s{RESET}" % msg, flush=True)
    if _xspdb_enable_log:
        _xspdb_logger.error("[Error] %s" % msg)

def warn(msg):
    """Print warning information"""
    if _XSPDB_LOG_LEVEL <= logging.WARNING:
        print(f"{YELLOW}[Warn] %s{RESET}" % msg, flush=True)
    if _xspdb_enable_log:
        _xspdb_logger.warning("[Warn] %s" % msg)

def build_prefix_tree(signals):
    tree = {}
    for signal in signals:
        current = tree
        parts = signal.split('.')
        for part in parts:
            if part not in current:
                current[part] = {}
            current = current[part]
    return tree


def get_completions(tree, prefix):
    parts = prefix.split('.')
    current_node = tree
    for i, part in enumerate(parts):
        if i == len(parts) - 1:
            break
        if part not in current_node:
            return []
        current_node = current_node[part]

    if prefix.endswith('.'):
        return [prefix + v for v in current_node.keys()]
    elif part in current_node:
        return [prefix + "." + v for v in current_node[part].keys()]
    else:
        if "." in prefix:
            prefix = prefix.rsplit(".", 1)[0] + "."
        else:
            prefix = ""
        last_part = parts[-1] if parts else ''
        candidates = list(current_node.keys())
        completions = [prefix + c for c in candidates if c.startswith(last_part)]
        return completions


def find_executable_in_dirs(executable_name, search_dirs="./ready-to-run"):
    """
    Search for an executable file in the specified directories. If not found, search in the system path.

    Args:
        executable_name (str): Name of the executable file
        search_dirs (list): List of specified directories

    Returns:
        str: Path to the executable file, or None if not found
    """
    import shutil
    for directory in search_dirs:
        potential_path = os.path.join(directory, executable_name)
        if os.path.isfile(potential_path) and os.access(potential_path, os.X_OK):
            return os.path.abspath(potential_path)
    return shutil.which(executable_name)


spike_dasm_path = find_executable_in_dirs("spike-dasm", search_dirs=["./ready-to-run"])
if not spike_dasm_path:
    info(f"spike-dasm found, use captone to disassemble, this may cannot work for some instructions")
def dasm_bytes(bytes_data, address):
    """Disassemble binary data

    Args:
        bytes_data (bytes): Binary data
        address (int): Starting address

    Returns:
        list: List of disassembled results
    """
    if spike_dasm_path is not None:
        # iterate over bytes_data in chunks of 2 bytes (c.instr. 16 bits)
        instrs_todecode = []
        full_instr = None
        for i in range(0, len(bytes_data), 2):
            c_instr = bytes_data[i:i+2]
            if full_instr is not None:
                full_instr += c_instr
                instrs_todecode.append((int.from_bytes(full_instr, byteorder='little', signed=False),
                                        full_instr[::-1].hex(), i-2 + address))
                full_instr = None
                continue
            # Is full 32 bit instr
            if c_instr[0] & 0x3 == 0x3: # full instr
                full_instr = c_instr
                continue
            # Is compressed 16 instr
            instrs_todecode.append((int.from_bytes(c_instr, byteorder='little', signed=False),
                                    c_instr[::-1].hex(), i + address))
        import subprocess
        result_asm = []
        # For every 1000 instrs, call spike-dasm
        for i in range(0, len(instrs_todecode), 1000):
            instrs = instrs_todecode[i:i+1000]
            ins_dm = "\\n".join(["DASM(%016lx)" % i[0] for i in instrs])
            # Call spike-dasm
            bash_cmd = 'echo "%s"|%s' % (ins_dm, spike_dasm_path)
            result = subprocess.run(bash_cmd,
                                    shell=True,
                                    text=True,
                                    stdout=subprocess.PIPE,
                                    stderr=subprocess.PIPE)
            assert result.returncode == 0, f"Error({bash_cmd}): " + str(result.stderr)
            ins_dm = result.stdout.strip().split("\n")
            assert len(ins_dm) == len(instrs), "Error(%s): %d != %d\n%s vs %s" % (bash_cmd, len(ins_dm), len(instrs), ins_dm, instrs)
            for i, v in enumerate(instrs):
                result_asm.append((v[2], v[1], ins_dm[i] if "unknown" not in ins_dm[i] else "unknown.bytes %s" % v[1], ""))
        return result_asm
    try:
        import capstone
    except ImportError:
        raise ImportError("Please install capstone library: pip install capstone")
    md = capstone.Cs(capstone.CS_ARCH_RISCV, capstone.CS_MODE_RISCV32|capstone.CS_MODE_RISCV64|capstone.CS_MODE_RISCVC)
    md.detail = True
    md.skipdata = True
    md.skipdata_setup = (".byte", None, None)
    asm_data = []
    for instr in md.disasm(bytes_data, address):
        asm_data.append((instr.address, instr.bytes[::-1].hex(), instr.mnemonic, instr.op_str))
    return asm_data


def register_commands(src_class, dest_class, dest_instance):
    """
    Register all commands from src_class to dest_class.
    """
    reg_count = 0
    for cls_name in dir(src_class):
        if not cls_name.startswith("Cmd"):
            continue
        cmd_cls = getattr(src_class, cls_name)
        if not inspect.isclass(cmd_cls):
            continue
        for func in dir(cmd_cls):
            if func.startswith("__"):
                continue
            func_obj = getattr(cmd_cls, func)
            if not callable(func_obj):
                continue
            if not hasattr(dest_class, func):
                setattr(dest_class, func, func_obj)
                dest_instance.register_map[func] = src_class.__name__+"."+cls_name
                reg_count += 1
            elif has_override_tag(func_obj):
                # If the function is overridden, replace it
                old_func = getattr(dest_class, func)
                func_obj.__old_func__ = old_func
                setattr(dest_class, func, func_obj)
                warn(f"Command {func} overridden in {dest_class.__name__} from {src_class.__name__}.{cls_name}")
            else:
                warn(f"Command {func} already exists in {dest_class.__name__}, ignoring")
                continue
        init = getattr(cmd_cls, "__init__", None)
        if init:
            init(dest_instance)
    return reg_count


def load_module_from_file(filepath):
    module_name = os.path.splitext(os.path.basename(filepath))[0]
    spec = importlib.util.spec_from_file_location(module_name, filepath)
    module = importlib.util.module_from_spec(spec)
    sys.modules[module_name] = module
    spec.loader.exec_module(module)
    return module


def load_package_from_dir(dirpath):
    module_name = os.path.basename(dirpath)
    init_path = os.path.join(dirpath, "__init__.py")
    assert os.path.exists(init_path), f"__init__.py not found in {dirpath}"
    spec = importlib.util.spec_from_file_location(module_name, init_path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[module_name] = module
    spec.loader.exec_module(module)
    return module


# decorators
def override(func):
    """
    Decorator to override a function in a class.
    """
    func.__is_xspdb_override__ = True
    return func


def has_override_tag(func):
    """
    Check if a function is overridden.
    """
    return getattr(func, "__is_xspdb_override__", False)
