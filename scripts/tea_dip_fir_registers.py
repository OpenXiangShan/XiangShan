"""Inventory FIR register declarations, including regreset and centralized diagnostics."""
from pathlib import Path
import hashlib
import re


def register_inventory(path):
    modules = {}
    current = None
    digest = hashlib.sha256()
    with Path(path).open('rb') as stream:
        for raw in stream:
            digest.update(raw)
            line = raw.decode()
            module = re.match(r'\s*(?:public )?(?:extmodule|module)\s+(\S+)\s*:', line)
            if module:
                current = module[1]
                modules[current] = {}
                continue
            register = re.match(r'\s*(regreset|reg)\s+(\S+)\s*:\s*(.*)', line)
            if register:
                kind, name, rest = register.groups()
                depth = 0
                for at, char in enumerate(rest):
                    if char in '{[(':
                        depth += 1
                    elif char in '}])':
                        depth -= 1
                    elif char == ',' and depth == 0:
                        register_type = rest[:at].strip()
                        break
                else:
                    raise AssertionError('Missing clock expression in register declaration: ' + line)
                assert current is not None and name not in modules[current]
                modules[current][name] = dict(kind=kind, type=register_type)
    return dict(path=str(path), sha256=digest.hexdigest(), modules=modules,
                register_declarations=sum(len(values) for values in modules.values()))
