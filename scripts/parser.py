#! /usr/bin/env python3

import argparse
import os
import re
from datetime import date
from shutil import copy, copytree

import xlsxwriter


class VIO(object):
    def __init__(self, info):
        self.info = info
        assert(self.info[0] in ["input", "output"])
        self.direction = self.info[0]
        self.width = 0 if self.info[1] == "" else int(self.info[1].split(":")[0].replace("[", ""))
        self.width += 1
        self.name = self.info[2]

    def get_direction(self):
        return self.direction

    def get_width(self):
        return self.width

    def get_name(self):
        return self.name

    def startswith(self, prefix):
        return self.info[2].startswith(prefix)

    def __str__(self):
        return " ".join(self.info)

    def __repr__(self):
        return self.__str__()

    def __lt__(self, other):
        return str(self) < str(other)

class VModule(object):
    module_re = re.compile(r'^\s*module\s*(\w+)\s*(#\(?|)\s*(\(.*|)\s*$')
    io_re = re.compile(r'^\s*(input|output)\s*(\[\s*\d+\s*:\s*\d+\s*\]|)\s*(\w+),?\s*$')
    submodule_re = re.compile(r'^\s*(\w+)\s*(#\(.*\)|)\s*(\w+)\s*\(\s*(|//.*)\s*$')

    def __init__(self, name):
        self.name = name
        self.lines = []
        self.io = []
        self.submodule = dict()
        self.instance = set()

    def add_line(self, line):
        debug_dontCare = False
        if "NegedgeDataModule_" in self.name and "@(posedge clock)" in line:
            line = line.replace("posedge", "negedge")
        elif "RenameTable" in self.name:
            if line.strip().startswith("assign io_debug_rdata_"):
                debug_dontCare = True
        elif "SynRegfileSlice" in self.name:
            if line.strip().startswith("assign io_debug_ports_"):
                debug_dontCare = True
        if debug_dontCare:
            self.lines.append("`ifndef SYNTHESIS\n")
        self.lines.append(line)
        if debug_dontCare:
            self.lines.append("`else\n")
            debug_dontCare_name = line.strip().split(" ")[1]
            self.lines.append(f"  assign {debug_dontCare_name} = 0;\n")
            self.lines.append("`endif\n")
        if len(self.lines):
            io_match = self.io_re.match(line)
            if io_match:
                this_io = VIO(tuple(map(lambda i: io_match.group(i), range(1, 4))))
                self.io.append(this_io)
            submodule_match = self.submodule_re.match(line)
            if submodule_match:
                this_submodule = submodule_match.group(1)
                if this_submodule != "module":
                    self.add_submodule(this_submodule)
                    self.add_instance(this_submodule, submodule_match.group(3))

    def add_lines(self, lines):
        for line in lines:
            self.add_line(line)

    def get_name(self):
        return self.name

    def set_name(self, updated_name):
        for i, line in enumerate(self.lines):
            module_match = VModule.module_re.match(line)
            if module_match:
                print(f"Line Previously: {line.strip()}")
                updated_line = line.replace(self.name, updated_name)
                print(f"Line Updated: {updated_line.strip()}")
                self.lines[i] = updated_line
                break
        self.name = updated_name

    def get_lines(self):
        return self.lines + ["\n"]

    def get_io(self, prefix="", match=""):
        if match:
            r = re.compile(match)
            return list(filter(lambda x: r.match(str(x)), self.io))
        else:
            return list(filter(lambda x: x.startswith(prefix), self.io))

    def get_submodule(self):
        return self.submodule

    def get_instance(self):
        return self.instance

    def add_submodule(self, name):
        self.submodule[name] = self.submodule.get(name, 0) + 1

    def add_instance(self, name, instance_name):
        self.instance.add((name, instance_name))

    def add_submodules(self, names):
        for name in names:
            self.add_submodule(name)

    def dump_io(self, prefix="", match=""):
        print("\n".join(map(lambda x: str(x), self.get_io(prefix, match))))

    def get_mbist_type(self):
        r = re.compile(r'input.*mbist_(\w+)_(trim|sleep)_fuse.*')
        mbist_fuse_io = list(filter(lambda x: r.match(str(x)), self.io))
        mbist_types = list(set(map(lambda io: io.get_name().split("_")[1], mbist_fuse_io)))
        assert(len(mbist_types) == 1)
        return mbist_types[0]

    def replace(self, s):
        self.lines = [s]

    def replace_with_macro(self, macro, s):
        replaced_lines = []
        in_io, in_body = False, False
        for line in self.lines:
            if self.io_re.match(line):
                in_io = True
                replaced_lines.append(line)
            elif in_io:
                in_io = False
                in_body = True
                replaced_lines.append(line) # This is ");"
                replaced_lines.append(f"`ifdef {macro}\n")
                replaced_lines.append(s)
                replaced_lines.append(f"`else\n")
            elif in_body:
                if line.strip() == "endmodule":
                    replaced_lines.append(f"`endif // {macro}\n")
                replaced_lines.append(line)
            else:
                replaced_lines.append(line)
        self.lines = replaced_lines

    def __str__(self):
        module_name = "Module {}: \n".format(self.name)
        module_io = "\n".join(map(lambda x: "\t" + str(x), self.io)) + "\n"
        return module_name + module_io

    def __repr__(self):
        return "{}".format(self.name)


class VCollection(object):
    def __init__(self):
        self.modules = []
        self.ancestors = []

    def load_modules(self, vfile):
        in_module = False
        current_module = None
        skipped_lines = []
        with open(vfile) as f:
            print("Loading modules from {}...".format(vfile))
            for i, line in enumerate(f):
                module_match = VModule.module_re.match(line)
                if module_match:
                    module_name = module_match.group(1)
                    if in_module or current_module is not None:
                        print("Line {}: does not find endmodule for {}".format(i, current_module))
                        exit()
                    current_module = VModule(module_name)
                    for skip_line in skipped_lines:
                        print("[WARNING]{}:{} is added to module {}:\n{}".format(vfile, i, module_name, skip_line), end="")
                        current_module.add_line(skip_line)
                    skipped_lines = []
                    in_module = True
                if not in_module or current_module is None:
                    if line.strip() != "":# and not line.strip().startswith("//"):
                        skipped_lines.append(line)
                    continue
                current_module.add_line(line)
                if line.startswith("endmodule"):
                    self.modules.append(current_module)
                    current_module = None
                    in_module = False

    def get_module_names(self):
        return list(map(lambda m: m.get_name(), self.modules))

    def get_all_modules(self, match=""):
        if match:
            r = re.compile(match)
            return list(filter(lambda m: r.match(m.get_name()), self.modules))
        else:
            return self.modules

    def get_module(self, name, negedge_modules=None, negedge_prefix=None, with_submodule=False, try_prefix=None, ignore_modules=None):
        if negedge_modules is None:
            negedge_modules = []
        target = None
        for module in self.modules:
            if module.get_name() == name:
                target = module
        if target is None and try_prefix is not None:
            for module in self.modules:
                name_no_prefix = name[len(try_prefix):]
                if module.get_name() == name_no_prefix:
                    target = module
                    print(f"Replace {name_no_prefix} with modulename {name}. Please DOUBLE CHECK the verilog.")
                    target.set_name(name)
        if target is None or not with_submodule:
            return target
        submodules = set()
        submodules.add(target)
        for submodule, instance in target.get_instance():
            if ignore_modules is not None and submodule in ignore_modules:
                continue
            self.ancestors.append(instance)
            is_negedge_module = False
            if negedge_prefix is not None:
                if submodule.startswith(negedge_prefix):
                    is_negedge_module = True
                elif try_prefix is not None and submodule.startswith(try_prefix + negedge_prefix):
                    is_negedge_module = True
            if is_negedge_module:
                negedge_modules.append("/".join(self.ancestors))
            result = self.get_module(submodule, negedge_modules, negedge_prefix, with_submodule=True, try_prefix=try_prefix, ignore_modules=ignore_modules)
            self.ancestors.pop()
            if result is None:
                print("Error: cannot find submodules of {} or the module itself".format(submodule))
                return None
            submodules.update(result)
        return submodules

    def dump_to_file(self, name, output_dir, with_submodule=True, split=True, try_prefix=None, ignore_modules=None):
        print("Dump module {} to {}...".format(name, output_dir))
        modules = self.get_module(name, with_submodule=with_submodule, try_prefix=try_prefix, ignore_modules=ignore_modules)
        if modules is None:
            print("does not find module", name)
            return False
        # print("All modules:", modules)
        if not with_submodule:
            modules = [modules]
        if not os.path.isdir(output_dir):
            os.makedirs(output_dir, exist_ok=True)
        if split:
            for module in modules:
                output_file = os.path.join(output_dir, module.get_name() + ".v")
                # print("write module", module.get_name(), "to", output_file)
                with open(output_file, "w") as f:
                    f.writelines(module.get_lines())
        else:
            output_file = os.path.join(output_dir, name + ".v")
            with open(output_file, "w") as f:
                for module in modules:
                    f.writelines(module.get_lines())
        return True

    def dump_negedge_modules_to_file(self, name, output_dir, with_submodule=True, try_prefix=None):
        print("Dump negedge module {} to {}...".format(name, output_dir))
        negedge_modules = []
        self.get_module(name, negedge_modules, "NegedgeDataModule_", with_submodule=with_submodule, try_prefix=try_prefix)
        negedge_modules_sort = []
        for negedge in negedge_modules:
            re_degits = re.compile(r".*[0-9]$")
            if re_degits.match(negedge):
                negedge_module, num = negedge.rsplit("_", 1)
            else:
                negedge_module, num = negedge, -1
            negedge_modules_sort.append((negedge_module, int(num)))
        negedge_modules_sort.sort(key = lambda x : (x[0], x[1]))
        output_file = os.path.join(output_dir, "negedge_modules.txt")
        with open(output_file, "w")as f:
            f.write("set sregfile_list [list\n")
            for negedge_module, num in negedge_modules_sort:
                if num == -1:
                    f.write("{}\n".format(negedge_module))
                else:
                    f.write("{}_{}\n".format(negedge_module, num))
            f.write("]")

    def add_module(self, name, line):
        module = VModule(name)
        module.add_line(line)
        self.modules.append(module)
        return module

    def count_instances(self, top_name, name):
        if top_name == name:
            return 1
        count = 0
        top_module = self.get_module(top_name)
        if top_module is not None:
            for submodule in top_module.submodule:
                count += top_module.submodule[submodule] * self.count_instances(submodule, name)
        return count

def check_data_module_template(collection):
    error_modules = []
    field_re = re.compile(r'io_(w|r)data_(\d*)(_.*|)')
    modules = collection.get_all_modules(match="(Sync|Async)DataModuleTemplate.*")
    for module in modules:
        module_name = module.get_name()
        print("Checking", module_name, "...")
        wdata_all = sorted(module.get_io(match="input.*wdata.*"))
        rdata_all = sorted(module.get_io(match="output.*rdata.*"))
        wdata_pattern = set(map(lambda x: " ".join((str(x.get_width()), field_re.match(x.get_name()).group(3))), wdata_all))
        rdata_pattern = set(map(lambda x: " ".join((str(x.get_width()), field_re.match(x.get_name()).group(3))), rdata_all))
        if wdata_pattern != rdata_pattern:
            print("Errors:")
            print("  wdata only:", sorted(wdata_pattern - rdata_pattern, key=lambda x: x.split(" ")[1]))
            print("  rdata only:", sorted(rdata_pattern - wdata_pattern, key=lambda x: x.split(" ")[1]))
            print("In", str(module))
            error_modules.append(module)
    return error_modules

def create_verilog(files, top_module, config, try_prefix=None, ignore_modules=None):
    collection = VCollection()
    for f in files:
        collection.load_modules(f)
    today = date.today()
    directory = f'{top_module}-Release-{config}-{today.strftime("%b-%d-%Y")}'
    success = collection.dump_to_file(top_module, os.path.join(directory, top_module), try_prefix=try_prefix, ignore_modules=ignore_modules)
    collection.dump_negedge_modules_to_file(top_module, directory, try_prefix=try_prefix)
    if not success:
        return None, None
    return collection, os.path.realpath(directory)

def get_files(build_path):
    files = []
    for f in os.listdir(build_path):
        file_path = os.path.join(build_path, f)
        if f.endswith(".v") or f.endswith(".sv"):
            files.append(file_path)
        elif os.path.isdir(file_path):
            files += get_files(file_path)
    return files

def create_filelist(filelist_name, out_dir, file_dirs=None, extra_lines=[]):
    if file_dirs is None:
        file_dirs = [filelist_name]
    filelist_entries = []
    for file_dir in file_dirs:
        for filename in os.listdir(os.path.join(out_dir, file_dir)):
            if filename.endswith(".v") or filename.endswith(".sv"):
                # check whether it exists in previous directories
                # this infers an implicit priority between the file_dirs
                has_found = False
                for entry in filelist_entries:
                    if entry.endswith(filename):
                        has_found = True
                        break
                if has_found:
                    continue
                filelist_entry = os.path.join(file_dir, filename)
                filelist_entries.append(filelist_entry)
    with open(os.path.join(out_dir, f"{filelist_name}.f"), "w") as f:
        for entry in filelist_entries + extra_lines:
            f.write(f"{entry}\n")


class SRAMConfiguration(object):
    ARRAY_NAME = "sram_array_(\d)p(\d+)x(\d+)m(\d+)(_multicycle|)(_repair|)"

    SINGLE_PORT = 0
    SINGLE_PORT_MASK = 1
    DUAL_PORT = 2
    DUAL_PORT_MASK = 3

    def __init__(self):
        self.name = None
        self.depth = None
        self.width = None
        self.ports = None
        self.mask_gran = None
        self.has_multi_cycle = False
        self.has_repair = False

    def size(self):
        return self.depth * self.width

    def is_single_port(self):
        return self.ports == self.SINGLE_PORT or self.ports == self.SINGLE_PORT_MASK

    def mask_width(self):
        return self.width // self.mask_gran

    def match_module_name(self, module_name):
        sram_array_re = re.compile(self.ARRAY_NAME)
        module_name_match = sram_array_re.match(self.name)
        return module_name_match

    def from_module_name(self, module_name):
        self.name = module_name
        module_name_match = self.match_module_name(self.name)
        assert(module_name_match is not None)
        num_ports = int(module_name_match.group(1))
        self.depth = int(module_name_match.group(2))
        self.width = int(module_name_match.group(3))
        self.mask_gran = int(module_name_match.group(4))
        assert(self.width % self.mask_gran == 0)
        if num_ports == 1:
            self.ports = self.SINGLE_PORT if self.mask_width() == 1 else self.SINGLE_PORT_MASK
        else:
            self.ports = self.DUAL_PORT if self.mask_width() == 1 else self.DUAL_PORT_MASK
        self.has_multi_cycle = str(module_name_match.group(5)) != ""
        self.has_repair = str(module_name_match.group(6)) != ""

    def ports_s(self):
        s = {
            self.SINGLE_PORT: "rw",
            self.SINGLE_PORT_MASK: "mrw",
            self.DUAL_PORT: "write,read",
            self.DUAL_PORT_MASK: "mwrite,read"
        }
        return s[self.ports]

    def to_sram_conf_entry(self):
        all_info = ["name", self.name, "depth", self.depth, "width", self.width, "ports", self.ports_s()]
        if self.mask_gran < self.width:
            all_info += ["mask_gran", self.mask_gran]
        return " ".join(map(str, all_info))

    def from_sram_conf_entry(self, line):
        items = line.strip().split(" ")
        self.name = items[1]
        if items[7] == "rw":
            ports = self.SINGLE_PORT
        elif items[7] == "mrw":
            ports = self.SINGLE_PORT_MASK
        elif items[7] == "write,read":
            ports = self.DUAL_PORT
        elif items[7] == "mwrite,read":
            ports = self.DUAL_PORT_MASK
        else:
            assert(0)
        depth = int(items[3])
        width = int(items[5])
        mask_gran = int(items[-1]) if len(items) > 8 else width
        matched_name = self.match_module_name(self.name) is not None
        if matched_name:
            self.from_module_name(self.name)
            assert(self.ports == ports)
            assert(self.depth == depth)
            assert(self.width == width)
            assert(self.mask_gran == mask_gran)
        else:
            self.ports = ports
            self.depth = depth
            self.width = width
            self.mask_gran = mask_gran

    def to_sram_xlsx_entry(self, num_instances):
        if self.is_single_port():
            num_read_port = "shared 1"
            num_write_port = "shared 1"
            read_clk = "RW0_clk"
            write_clk = "RW0_clk"
        else:
            num_read_port = 1
            num_write_port = 1
            read_clk = "R0_clk"
            write_clk = "W0_clk"
        all_info = [self.name, num_instances, "SRAM", num_read_port, num_write_port, 0,
                    self.depth, self.width, self.mask_gran, read_clk, write_clk, "N/A"]
        return all_info

    def get_foundry_sram_wrapper(self, mbist_type):
        wrapper_type = "RAMSP" if self.is_single_port() else "RF2P"
        wrapper_mask = "" if self.mask_width() == 1 else f"_M{self.mask_width()}"
        wrapper_module = f"{wrapper_type}_{self.depth}x{self.width}{wrapper_mask}_WRAP"
        wrapper_instance = "u_mem"
        foundry_ports = {
            "IP_RESET_B"           :  "mbist_IP_RESET_B",
            "PWR_MGMT_IN"          :  "mbist_PWR_MGNT_IN",
            "TRIM_FUSE_IN"         : f"mbist_{mbist_type}_trim_fuse",
            "SLEEP_FUSE_IN"        : f"mbist_{mbist_type}_sleep_fuse",
            "FSCAN_RAM_BYPSEL"     :  "mbist_bypsel",
            "FSCAN_RAM_WDIS_B"     :  "mbist_wdis_b",
            "FSCAN_RAM_RDIS_B"     :  "mbist_rdis_b",
            "FSCAN_RAM_INIT_EN"    :  "mbist_init_en",
            "FSCAN_RAM_INIT_VAL"   :  "mbist_init_val",
            "FSCAN_CLKUNGATE"      :  "mbist_clkungate",
            "OUTPUT_RESET"         :  "mbist_OUTPUT_RESET",
            "PWR_MGMT_OUT"         :  "mbist_PWR_MGNT_OUT"
        }
        if self.is_single_port():
            foundry_ports["WRAPPER_CLK_EN"] = "mbist_WRAPPER_CLK_EN"
        else:
            foundry_ports["WRAPPER_WR_CLK_EN"] = "mbist_WRAPPER_RD_CLK_EN"
            foundry_ports["WRAPPER_RD_CLK_EN"] = "mbist_WRAPPER_WR_CLK_EN"
        if self.has_repair:
            foundry_ports["ROW_REPAIR_IN"] = "repair_rowRepair"
            foundry_ports["COL_REPAIR_IN"] = "repair_colRepair"
            foundry_ports["io_bisr_shift_en"] = "mbist_bisr_shift_en"
            foundry_ports["io_bisr_clock"] = "mbist_bisr_clock"
            foundry_ports["io_bisr_reset"] = "mbist_bisr_reset"
            foundry_ports["u_mem_bisr_inst_SI"] = "mbist_bisr_scan_in"
            foundry_ports["u_mem_bisr_inst_SO"] = "mbist_bisr_scan_out"
        if self.is_single_port():
            func_ports = {
                "CK"  : "RW0_clk",
                "A"   : "RW0_addr",
                "WEN" : "RW0_en & RW0_wmode",
                "D"   : "RW0_wdata",
                "REN" : "RW0_en & ~RW0_wmode",
                "Q"   : "RW0_rdata"
            }
            if self.mask_width() > 1:
                func_ports["WM"] = "RW0_wmask"
        else:
            func_ports = {
                "WCK" : "W0_clk",
                "WA"  : "W0_addr",
                "WEN" : "W0_en",
                "D"   : "W0_data",
                "RCK" : "R0_clk",
                "RA"  : "R0_addr",
                "REN" : "R0_en",
                "Q"   : "R0_data"
            }
            if self.mask_width() > 1:
                func_ports["WM"] = "W0_mask"
        if self.width > 256:
            func_ports["MBIST_SELECTEDOH"] = "mbist_selectedOH"
        verilog_lines = []
        verilog_lines.append(f"  {wrapper_module} {wrapper_instance} (\n")
        connected_pins = []
        for pin_name in func_ports:
            connected_pins.append(f".{pin_name}({func_ports[pin_name]})")
        for pin_name in foundry_ports:
            connected_pins.append(f".{pin_name}({foundry_ports[pin_name]})")
        verilog_lines.append("    " + ",\n    ".join(connected_pins) + "\n")
        verilog_lines.append("  );\n")
        return wrapper_module, "".join(verilog_lines)

def generate_sram_conf(collection, module_prefix, out_dir):
    if module_prefix is None:
        module_prefix = ""
    sram_conf = []
    sram_array_name = module_prefix + SRAMConfiguration.ARRAY_NAME
    modules = collection.get_all_modules(match=sram_array_name)
    for module in modules:
        conf = SRAMConfiguration()
        conf.from_module_name(module.get_name()[len(module_prefix):])
        sram_conf.append(conf)
    conf_path = os.path.join(out_dir, "sram_configuration.txt")
    with open(conf_path, "w") as f:
        for conf in sram_conf:
            f.write(conf.to_sram_conf_entry() + "\n")
    return conf_path

def create_sram_xlsx(out_dir, collection, sram_conf, top_module, try_prefix=None):
    workbook = xlsxwriter.Workbook(os.path.join(out_dir, "sram_list.xlsx"))
    worksheet = workbook.add_worksheet()
    # Header for the list. Starting from row 5.
    row = 5
    columns = ["Array Instance Name", "# Instances", "Memory Type",
               "# Read Ports", "# Write Ports", "# CAM Ports",
               "Depth (Entries)", "Width (Bits)", "# Write Segments",
               "Read Clk Pin Names(s)", "Write Clk Pin Name(s)", "CAM Clk Pin Name"
    ]
    for col, column_name in enumerate(columns):
        worksheet.write(row, col, column_name)
    row += 1
    # Entries for the list.
    total_size = 0
    with open(sram_conf) as f:
        for line in f:
            conf = SRAMConfiguration()
            conf.from_sram_conf_entry(line)
            num_instances = collection.count_instances(top_module, conf.name)
            if num_instances == 0 and try_prefix is not None:
                try_prefix_name = f"{try_prefix}{conf.name}"
                num_instances = collection.count_instances(top_module, try_prefix_name)
                if num_instances != 0:
                    conf.name = try_prefix_name
            all_info = conf.to_sram_xlsx_entry(num_instances)
            for col, info in enumerate(all_info):
                worksheet.write(row, col, info)
            row += 1
            total_size += conf.size() * num_instances
    # Total size of the SRAM in top of the sheet
    worksheet.write(0, 0, f"Total size: {total_size / (8 * 1024)} KiB")
    workbook.close()

def create_extra_files(out_dir, build_path):
    extra_path = os.path.join(out_dir, "extra")
    copytree("/nfs/home/share/southlake/extra", extra_path)
    for f in os.listdir(build_path):
        file_path = os.path.join(build_path, f)
        if f.endswith(".csv"):
            copy(file_path, extra_path)

def replace_sram(out_dir, sram_conf, top_module, module_prefix):
    replace_sram_dir = "memory_array"
    replace_sram_path = os.path.join(out_dir, replace_sram_dir)
    if not os.path.exists(replace_sram_path):
        os.mkdir(replace_sram_path)
    sram_wrapper_dir = "memory_wrapper"
    sram_wrapper_path = os.path.join(out_dir, sram_wrapper_dir)
    if not os.path.exists(sram_wrapper_path):
        os.mkdir(sram_wrapper_path)
    replaced_sram = []
    with open(sram_conf) as f:
        for line in f:
            conf = SRAMConfiguration()
            conf.from_sram_conf_entry(line)
            sim_sram_module = VModule(conf.name)
            sim_sram_path = os.path.join(out_dir, top_module, f"{conf.name}.v")
            if not os.path.exists(sim_sram_path) and module_prefix is not None:
                sim_sram_path = os.path.join(out_dir, top_module, f"{module_prefix}{conf.name}.v")
                sim_sram_module.name = f"{module_prefix}{conf.name}"
            with open(sim_sram_path, "r") as sim_f:
                sim_sram_module.add_lines(sim_f.readlines())
            mbist_type = sim_sram_module.get_mbist_type()
            wrapper, instantiation_v = conf.get_foundry_sram_wrapper(mbist_type)
            sim_sram_module.replace_with_macro("FOUNDRY_MEM", instantiation_v)
            output_file = os.path.join(replace_sram_path, f"{sim_sram_module.name}.v")
            with open(output_file, "w") as f:
                f.writelines(sim_sram_module.get_lines())
            # uncomment the following lines to copy the provided memory wrapper
            # wrapper_dir = "/nfs/home/share/southlake/sram_replace/mem_wrap"
            # wrapper_path = os.path.join(wrapper_dir, f"{wrapper}.v")
            # copy(wrapper_path, os.path.join(sram_wrapper_path, f"{wrapper}.v"))
            replaced_sram.append(sim_sram_module.name)
    with open(os.path.join(out_dir, f"{sram_wrapper_dir}.f"), "w") as wrapper_f:
        wrapper_f.write("// FIXME: include your SRAM wrappers here\n")
    return replace_sram_dir, [f"-F {sram_wrapper_dir}.f"]


def replace_mbist_scan_controller(out_dir):
    target_dir = "scan_mbist_ctrl"
    target_path = os.path.join(out_dir, target_dir)
    if not os.path.exists(target_path):
        os.mkdir(target_path)
    blackbox_src_dir = "/nfs/home/share/southlake/sram_replace/scan_mbist_ctrl_rpl_rtl"
    for filename in os.listdir(blackbox_src_dir):
        if filename.startswith("bosc_") and (filename.endswith(".v") or filename.endswith(".sv")):
            copy(os.path.join(blackbox_src_dir, filename), target_path)
    with open(os.path.join(out_dir, "dfx_blackbox.f"), "w") as wrapper_f:
        wrapper_f.write("// FIXME: include your blackbox mbist/scan controllers here\n")
    return target_dir, [f"-F dfx_blackbox.f"]


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Verilog parser for XS')
    parser.add_argument('top', type=str, help='top-level module')
    parser.add_argument('--xs-home', type=str, help='path to XS')
    parser.add_argument('--config', type=str, default="Unknown", help='XSConfig')
    parser.add_argument('--prefix', type=str, help='module prefix')
    parser.add_argument('--ignore', type=str, default="", help='ignore modules (and their submodules)')
    parser.add_argument('--include', type=str, help='include verilog from more directories')
    parser.add_argument('--no-filelist', action='store_true', help='do not create filelist')
    parser.add_argument('--no-sram-conf', action='store_true', help='do not create sram configuration file')
    parser.add_argument('--no-sram-xlsx', action='store_true', help='do not create sram configuration xlsx')
    parser.add_argument('--no-extra-files', action='store_true', help='do not copy extra files')
    parser.add_argument('--sram-replace', action='store_true', help='replace SRAM libraries')
    parser.add_argument('--mbist-scan-replace', action='store_true', help='replace mbist and scan controllers')

    args = parser.parse_args()

    xs_home = args.xs_home
    if xs_home is None:
        xs_home = os.path.realpath(os.getenv("NOOP_HOME"))
        assert(xs_home is not None)
    build_path = os.path.join(xs_home, "build")
    files = get_files(build_path)
    if args.include is not None:
        for inc_path in args.include.split(","):
            files += get_files(inc_path)

    top_module = args.top
    module_prefix = args.prefix
    config = args.config
    ignore_modules = list(filter(lambda x: x != "", args.ignore.split(",")))
    if module_prefix is not None:
        top_module = f"{module_prefix}{top_module}"
        ignore_modules += list(map(lambda x: module_prefix + x, ignore_modules))

    print(f"Top-level Module: {top_module} with prefix {module_prefix}")
    print(f"Config:           {config}")
    print(f"Ignored modules:  {ignore_modules}")
    collection, out_dir = create_verilog(files, top_module, config, try_prefix=module_prefix, ignore_modules=ignore_modules)
    assert(collection)

    rtl_dirs = [top_module]
    extra_filelist_lines = []
    if args.mbist_scan_replace:
        dfx_ctrl, extra_dfx_lines = replace_mbist_scan_controller(out_dir)
        rtl_dirs = [dfx_ctrl] + rtl_dirs
        extra_filelist_lines += extra_dfx_lines
    if not args.no_filelist:
        create_filelist(top_module, out_dir, rtl_dirs, extra_filelist_lines)
    if not args.no_sram_conf:
        sram_conf = generate_sram_conf(collection, module_prefix, out_dir)
        if not args.no_sram_xlsx:
            create_sram_xlsx(out_dir, collection, sram_conf, top_module, try_prefix=module_prefix)
        if args.sram_replace:
            sram_replace_dir, sram_extra_lines = replace_sram(out_dir, sram_conf, top_module, module_prefix)
            # We create another filelist for foundry-provided SRAMs
            if not args.no_filelist:
                rtl_dirs = [sram_replace_dir] + rtl_dirs
                extra_filelist_lines += sram_extra_lines
                create_filelist(f"{top_module}_with_foundry_sram", out_dir, rtl_dirs, extra_filelist_lines)
    if not args.no_extra_files:
        create_extra_files(out_dir, build_path)
