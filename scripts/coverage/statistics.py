#/usr/bin/python3
# -*- coding: UTF-8 -*-
import sys
import re
import copy
import pprint

COVERRED = "COVERRED"
NOT_COVERRED = "NOT_COVERRED"
DONTCARE = "DONTCARE"
BEGIN = "BEGIN"
END = "END"
CHILDREN = "CHILDREN"
MODULE = "MODULE"
INSTANCE = "INSTANCE"
TYPE="TYPE"
ROOT="ROOT"
NODE="NODE"
SELFCOVERAGE="SELFCOVERAGE"
TREECOVERAGE="TREECOVERAGE"

def get_lines(input_file):
    lines = []
    with open(input_file) as f:
        for line in f:
            lines.append(line)
    return lines

def get_line_annotation(lines):
    line_annotations = []
    # pattern_1: 040192     if(array_0_MPORT_en & array_0_MPORT_mask) begin
    # pattern_2: 2218110        end else if (_T_30) begin // @[Conditional.scala 40:58]
    # pattern_2: 000417     end else begin
    coverred_pattern_1 = re.compile('^\s*(\d+)\s+if')
    coverred_pattern_2 = re.compile('^\s*(\d+)\s+end else')
    not_coverred_pattern_1 = re.compile('^\s*(%0+)\s+if')
    not_coverred_pattern_2 = re.compile('^\s*(%0+)\s+end else')

    for line in lines:
        coverred_match = coverred_pattern_1.search(line) or coverred_pattern_2.search(line)
        not_coverred_match = not_coverred_pattern_1.search(line) or not_coverred_pattern_2.search(line)

        assert not (coverred_match and not_coverred_match)
        
        if coverred_match:
            line_annotations.append(COVERRED)
        elif not_coverred_match:
            line_annotations.append(NOT_COVERRED)
        else:
            line_annotations.append(DONTCARE)
    return line_annotations

# get the line coverage statistics in line range [start, end)
def get_coverage_statistics(line_annotations, start, end):
    coverred = 0
    not_coverred = 0
    for i in range(start, end):
        if line_annotations[i] == COVERRED:
            coverred += 1

        if line_annotations[i] == NOT_COVERRED:
            not_coverred += 1

    # deal with divide by zero
    coverage = 1.0
    if coverred + not_coverred != 0:
        coverage = float(coverred) / (coverred + not_coverred)
    return (coverred, not_coverred, coverage)

# get modules and all it's submodules
def get_modules(lines):
    modules = {}

    module_pattern = re.compile("module (\w+)\(")
    endmodule_pattern = re.compile("endmodule")
    submodule_pattern = re.compile("(\w+) (\w+) \( // @\[\w+.scala \d+:\d+\]")

    line_count = 0

    name = "ModuleName"

    for line in lines:
        module_match = module_pattern.search(line)
        endmodule_match = endmodule_pattern.search(line)
        submodule_match = submodule_pattern.search(line)

        assert not (module_match and endmodule_match)
        
        if module_match:
            name = module_match.group(1)
            # print("module_match: module: %s" % name)
            assert name not in modules
            # [begin
            modules[name] = {}
            modules[name][BEGIN] = line_count
            # the first time we see a module, we treat as a root node
            modules[name][TYPE] = ROOT

        if endmodule_match:
            # print("endmodule_match: module: %s" % name)
            assert name in modules
            assert END not in modules[name]
            # end)
            modules[name][END] = line_count + 1
            # reset module name to invalid
            name = "ModuleName"

        if submodule_match:
            # submodule must be inside hierarchy
            assert name != "ModuleName"
            submodule_type = submodule_match.group(1)
            submodule_instance = submodule_match.group(2)
            # print("submodule_match: type: %s instance: %s" % (submodule_type, submodule_instance))

            # submodules should be defined first
            # if we can not find it's definition
            # we consider it a black block module
            if submodule_type not in modules:
                print("Module %s is a Blackbox" % submodule_type)
            else:
                # mark submodule as a tree node
                # it's no longer root any more
                modules[submodule_type][TYPE] = NODE

                if CHILDREN not in modules[name]:
                    modules[name][CHILDREN] = []
                submodule = {MODULE: submodule_type, INSTANCE: submodule_instance}
                modules[name][CHILDREN].append(submodule)

        line_count += 1
    return modules

# we define two coverage metrics:
# self coverage: coverage results of this module(excluding submodules)
# tree coverage: coverage results of this module(including submodules)
def get_tree_coverage(modules, coverage):
    def dfs(module):
        if TREECOVERAGE not in modules[module]:
            self_coverage = modules[module][SELFCOVERAGE]
            if CHILDREN not in modules[module]:
                modules[module][TREECOVERAGE] = self_coverage
            else:
                coverred = self_coverage[0]
                not_coverred = self_coverage[1]
                # the dfs part
                for child in modules[module][CHILDREN]:
                    child_coverage = dfs(child[MODULE])
                    coverred += child_coverage[0]
                    not_coverred += child_coverage[1]
                # deal with divide by zero
                coverage = 1.0
                if coverred + not_coverred != 0:
                    coverage = float(coverred) / (coverred + not_coverred)
                modules[module][TREECOVERAGE] = (coverred, not_coverred, coverage)
        return modules[module][TREECOVERAGE]

    for module in modules:
        modules[module][SELFCOVERAGE] = coverage[module]

    for module in modules:
        modules[module][TREECOVERAGE] = dfs(module)
    return modules

# arg1: tree coverage results
# arg2: coverage type
def sort_coverage(coverage, coverage_type):
    l = [(module, coverage[module][coverage_type])for module in coverage]
    l.sort(key=lambda x:x[1][2])
    return l

def print_tree_coverage(tree_coverage):
    def dfs(module, level):
        # print current node
        tree = tree_coverage[module][TREECOVERAGE]
        self = tree_coverage[module][SELFCOVERAGE]
        print("  " * level + "- " + module)
        print("  " * level + "  tree", end="")
        print("(%d, %d, %.2f)" % (tree[0], tree[1], tree[2] * 100.0))
        print("  " * level + "  self", end="")
        print("(%d, %d, %.2f)" % (self[0], self[1], self[2] * 100.0))

        # print children nodes
        if CHILDREN in modules[module]:
                # the dfs part
                for child in modules[module][CHILDREN]:
                    dfs(child[MODULE], level + 1)

    for module in tree_coverage:
        if tree_coverage[module][TYPE] == ROOT:
            dfs(module, 0)

if __name__ == "__main__":
    assert len(sys.argv) == 2, "Expect input_file"
    input_file = sys.argv[1]
    pp = pprint.PrettyPrinter(indent=4)

    lines = get_lines(input_file)
    # print("lines:")
    # pp.pprint(lines)

    annotations = get_line_annotation(lines)
    # print("annotations:")
    # pp.pprint(annotations)

    modules = get_modules(lines)
    # print("modules:")
    # pp.pprint(modules)

    self_coverage = {module: get_coverage_statistics(annotations, modules[module][BEGIN], modules[module][END])
            for module in modules}
    # print("self_coverage:")
    # pp.pprint(self_coverage)

    tree_coverage = get_tree_coverage(modules, self_coverage)
    # print("tree_coverage:")
    # pp.pprint(tree_coverage)

    print("SelfCoverage:")
    pp.pprint(sort_coverage(tree_coverage, SELFCOVERAGE))

    print("TreeCoverage:")
    pp.pprint(sort_coverage(tree_coverage, TREECOVERAGE))

    print("AllCoverage:")
    print_tree_coverage(tree_coverage)
