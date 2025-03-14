#coding=utf-8

import pdb

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


class XSPdb(pdb.Pdb):
    def __init__(self, dut, difftest_stat):
        super().__init__()
        self.dut = dut
        self.dut_tree = build_prefix_tree(dut.GetInternalSignalList())
        self.prompt = "(XiangShan) "
        self.difftest_stat = difftest_stat

    def do_tprint(self, arg):
        sig = self.dut.GetInternalSignal(arg)
        if sig:
            print(f"value: {hex(sig.value)}  width: {sig.W()}")

    def complete_tprint(self, text, line, begidx, endidx):
        cmp = get_completions(self.dut_tree, text)
        return cmp

    def do_tset(self, arg):
        args = arg.strip().split()
        if len(args) < 2:
            print("need args format: name value")
            return
        pin_name, pin_value = args[0], args[1]
        try:
            pin_value = int(pin_value)
        except Exception as e:
            print(f"convert {args[1]} to number fail: {str(e)}")
            return
        pin = self.dut.GetInternalSignal(pin_name)
        if pin:
            pin.AsImmWrite()
            pin.value = pin_value

    def complete_tset(self, text, line, begidx, endidx):
        cmp = get_completions(self.dut_tree, text)
        return cmp

    def do_tpc(self, a):
        index = 0
        while True:
            cmt = self.difftest_stat.get_commit(index)
            if cmt:
                print(f" PC-{index} {hex(cmt.pc)}", end="")
                index += 1
            else:
                print("")
                break

    def do_texpdiffstate(self, var):
        self.curframe.f_locals[var] = self.difftest_stat



