#! /usr/bin/env python3

import argparse
import os
import re
import sys


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
    io_re = re.compile(r'^\s*(input|output)\s*(\[\s*\d+\s*:\s*\d+\s*\]|)\s*(\w+),?\s*$')
    submodule_re = re.compile(r'^\s*(\w+)\s*(#\(.*\)|)\s*(\w+)\s*\(\s*(|//.*)\s*$')
    array_ext_line_re = re.compile(r'^  array_(\d*)_ext array_(\d*)_ext.*$')

    def __init__(self, name):
        self.name = name
        self.lines = []
        self.io = []
        self.submodule = set()

    def add_line(self, line):
        debug_dontCare = False
        if self.name.startswith("NegedgeDataModule_") and "@(posedge clock)" in line:
            line = line.replace("posedge", "negedge")
        elif self.name == "RenameTable" or self.name == "RenameTable_1":
            if line.strip().startswith("assign io_debug_rdata_"):
                debug_dontCare = True
        elif self.name.startswith("SynRegfileSlice"):
            if line.strip().startswith("assign io_debug_ports_"):
                debug_dontCare = True
        
        array_ext_match = self.array_ext_line_re.match(line)
        if (array_ext_match):
            print('array_ext match line ', line)
            idx = int(array_ext_match.group(1))
            # this is ugly
            # sram with idx 4 is eliminated, so those with idx >= 4 should use idx + 1
            if idx >= 4:
                new_line = re.sub(r'\d+', str(idx + 1), line)
                print(line, '->', new_line)
                line = new_line
        
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
                    self.add_submodule(this_submodule, submodule_match.group(3))

    def get_name(self):
        return self.name

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

    def add_submodule(self, name, instance_name):
        # print(self.get_name(), "add submodule", name)
        self.submodule.add((name, instance_name))

    def add_submodules(self, names):
        # print(self.get_name(), "add submodules", names)
        self.submodule.update(names)

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
    module_re = re.compile(r'^\s*module\s*(\w+)\s*(#\(?|)\s*(\(.*|)\s*$')

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
                module_match = self.module_re.match(line)
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
                    if line.strip() != "" and not line.strip().startswith("//"):
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

    def get_module(self, name, negedge_modules, prefix, with_submodule=False):
        target = None
        for module in self.modules:
            if module.get_name() == name:
                target = module
        if target is None or not with_submodule:
            return target
        submodules = set()
        submodules.add(target)
        for submodule, instance_name in target.get_submodule():
            self.ancestors.append(instance_name)
            if prefix != None and submodule.startswith(prefix):
                negedge_modules.append("/".join(self.ancestors))
            result = self.get_module(submodule, negedge_modules, prefix, with_submodule=True)
            self.ancestors.pop()
            if result is None:
                print("Error: cannot find submodules of {} or the module itself".format(submodule))
                continue#return None
            submodules.update(result)
        return submodules

    def dump_to_file(self, name, output_dir, with_submodule=True, split=True):
        print("Dump module {} to {}...".format(name, output_dir))
        modules = self.get_module(name, [], None, with_submodule)
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
                    
    def dump_negedge_modules_to_file(self, name, output_dir, with_submodule=True):
        print("Dump negedge module {} to {}...".format(name, output_dir))
        negedge_modules = []
        self.get_module(name, negedge_modules, "NegedgeDataModule_", with_submodule)
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

def get_files(build_path):
    files = []
    for f in os.listdir(build_path):
        file_path = os.path.join(build_path, f)
        if f.endswith(".v"):
            files.append(file_path)
        elif os.path.isdir(file_path):
            files += get_files(file_path)
    return files

def main(files):
    collection = VCollection()
    for f in files:
        collection.load_modules(f)

    directory = "rtl"
    out_modules = ["XSTop", "XSCore", "CtrlBlock", "ExuBlock", "ExuBlock_1", "MemBlock", "Frontend", "HuanCun", "HuanCun_2"]
    out_negedge_modules = ["XSTop"]
    for m in out_modules:
        collection.dump_to_file(m, os.path.join(directory, m))
    for m in out_negedge_modules:
        collection.dump_negedge_modules_to_file(m, "nanhu_release")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Verilog parser for XS')
    parser.add_argument('--xs-home', type=str, help='path to XS')

    args = parser.parse_args()

    xs_home = args.xs_home
    if xs_home is None:
        xs_home = os.path.realpath(os.getenv("NOOP_HOME"))
        assert(xs_home is not None)
    build_path = os.path.join(xs_home, "build")
    files = get_files(build_path)

    main(files)
