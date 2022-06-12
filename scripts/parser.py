#! /usr/bin/env python3

import os
import re
import sys
from datetime import date
from shutil import copytree, copy

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

    def add_line(self, line):
        self.lines.append(line)
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

    def add_submodule(self, name):
        self.submodule[name] = self.submodule.get(name, 0) + 1

    def add_submodules(self, names):
        for name in names:
            self.add_submodule(name)

    def dump_io(self, prefix="", match=""):
        print("\n".join(map(lambda x: str(x), self.get_io(prefix, match))))

    def replace(self, s):
        self.lines = [s]

    def __str__(self):
        module_name = "Module {}: \n".format(self.name)
        module_io = "\n".join(map(lambda x: "\t" + str(x), self.io)) + "\n"
        return module_name + module_io

    def __repr__(self):
        return "{}".format(self.name)


class VCollection(object):
    def __init__(self):
        self.modules = []

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

    def get_module(self, name, with_submodule=False, try_prefix=None):
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
        for submodule in target.get_submodule():
            result = self.get_module(submodule, with_submodule=True, try_prefix=try_prefix)
            if result is None:
                print("Error: cannot find submodules of {} or the module itself".format(submodule))
                return None
            submodules.update(result)
        return submodules

    def dump_to_file(self, name, output_dir, with_submodule=True, split=True, try_prefix=None):
        print("Dump module {} to {}...".format(name, output_dir))
        modules = self.get_module(name, with_submodule, try_prefix=try_prefix)
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

def create_verilog(files, top_module, config, try_prefix=None):
    collection = VCollection()
    for f in files:
        collection.load_modules(f)
    today = date.today()
    directory = f'XSTop-Release-{config}-{today.strftime("%b-%d-%Y")}'
    success = collection.dump_to_file(top_module, os.path.join(directory, top_module), try_prefix=try_prefix)
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

def create_filelist(out_dir, top_module):
    filelist_name = f"{top_module}.f"
    with open(os.path.join(out_dir, filelist_name), "w") as f:
        for filename in os.listdir(os.path.join(out_dir, top_module)):
            if filename.endswith(".v"):
                filelist_entry = os.path.join(top_module, filename)
                f.write(f"{filelist_entry}\n")

def generate_sram_conf(collection, module_prefix, out_dir):
    if module_prefix is None:
        module_prefix = ""
    sram_conf = []
    sram_array_name = module_prefix + "sram_array_\d+_(\d)p(\d+)x(\d+)m(\d+)(_multi_cycle|)"
    modules = collection.get_all_modules(match=sram_array_name)
    sram_array_re = re.compile(sram_array_name)
    for module in sorted(modules, key=lambda m: int(m.get_name().split("_")[3])):
        # name
        module_name = module.get_name()
        module_name_match = sram_array_re.match(module_name)
        assert(module_name_match is not None)
        num_ports = int(module_name_match.group(1))
        depth = int(module_name_match.group(2))
        width = int(module_name_match.group(3))
        mask_gran = int(module_name_match.group(4))
        assert(width % mask_gran == 0)
        mask_width = width // mask_gran
        if num_ports == 1:
            ports = "rw" if mask_width == 1 else "mrw"
        else:
            ports = "write,read" if mask_width == 1 else "mwrite,read"
        all_info = ["name", module_name, "depth", depth, "width", width, "ports", ports]
        if mask_gran < width:
            all_info += ["mask_gran", mask_gran]
        sram_conf.append(all_info)
    conf_path = os.path.join(out_dir, "sram_configuration.txt")
    with open(conf_path, "w") as f:
        for conf in sram_conf:
            f.write(" ".join(map(str, conf)) + "\n")
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
            items = line.strip().split(" ")
            sram_module_name = items[1]
            num_instances = collection.count_instances(top_module, sram_module_name)
            if num_instances == 0 and try_prefix is not None:
                try_prefix_name = f"{try_prefix}{sram_module_name}"
                num_instances = collection.count_instances(top_module, try_prefix_name)
                if num_instances != 0:
                    sram_module_name = try_prefix_name
            if items[7] == "mrw" or items[7] == "rw":
                num_read_port = "shared 1"
                num_write_port = "shared 1"
            elif items[7] == "mwrite,read" or items[7] == "write,read":
                num_read_port = 1
                num_write_port = 1
            else:
                num_read_port = 0
                num_write_port = 0
            depth = int(items[3])
            width = int(items[5])
            mask_gran = int(items[-1]) if len(items) > 8 else width
            read_clk = "RW0_clk" if "rw" in items[7] else "R0_clk"
            write_clk = "RW0_clk" if "rw" in items[7] else "W0_clk"
            all_info = [sram_module_name, num_instances, "SRAM", num_read_port, num_write_port, 0,
                        depth, width, mask_gran, read_clk, write_clk, "N/A"]
            for col, info in enumerate(all_info):
                worksheet.write(row, col, info)
            row += 1
            total_size += depth * width * num_instances
    # Total size of the SRAM in top of the sheet
    worksheet.write(0, 0, f"Total size: {total_size / (8 * 1024)} KiB")
    workbook.close()

def create_extra_files(out_dir, build_path):
    extra_path = os.path.join(out_dir, "extra")
    copytree("/nfs/home/share/southlake/extra", extra_path)
    for f in os.listdir(build_path):
        file_path = os.path.join(build_path, f)
        if f.endswith(".xls"):
            copy(file_path, extra_path)

if __name__ == "__main__":
    xs_home = os.path.realpath(os.getenv("NOOP_HOME"))
    build_path = os.path.join(xs_home, "build")
    files = get_files(build_path)

    module_prefix = None
    top_module = "XSTop"
    config = "DefaultConfig"
    if len(sys.argv) > 1:
        config = sys.argv[1]
    if len(sys.argv) > 2:
        module_prefix = sys.argv[2]
        top_module = f"{module_prefix}{top_module}"
    print(f"Top-level Module: {top_module} with prefix {module_prefix}")
    print(f"Config:         : {config}")
    collection, out_dir = create_verilog(files, top_module, config, try_prefix=module_prefix)
    assert(collection)

    create_filelist(out_dir, top_module)
    sram_conf = generate_sram_conf(collection, module_prefix, out_dir)
    create_sram_xlsx(out_dir, collection, sram_conf, top_module, try_prefix=module_prefix)
    create_extra_files(out_dir, build_path)
