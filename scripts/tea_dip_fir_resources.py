"""Count FIR state and memories using the reachable hierarchy's instance multiplicity."""
from collections import Counter
import json
import re


def resource_inventory(path, inventory):
 modules = {}
 current = None
 memory = None
 def flush_memory():
  nonlocal memory
  if memory is not None:
   modules[current]['memory'].append(('mem', tuple(memory['lines'])))
   memory = None
 with path.open() as stream:
  for raw in stream:
   clean = raw.split(' @[', 1)[0].rstrip()
   stripped = clean.strip()
   indent = len(clean) - len(clean.lstrip())
   if memory is not None and stripped and indent <= memory['indent']:
    flush_memory()
   match = re.match(r'\s*(?:public )?(extmodule|module)\s+(\S+)\s*:', clean)
   if match:
    flush_memory()
    current = match[2]
    modules[current] = dict(children=Counter(), memory=[], external=match[1]=='extmodule', interface=[])
    continue
   if current is None:
    continue
   record = modules[current]
   instance = re.match(r'\s*inst\s+\S+\s+of\s+(\S+)', clean)
   if instance:
    record['children'][instance[1]] += 1
   if record['external'] and re.match(r'(?:input|output|defname|parameter)\b', stripped):
    record['interface'].append(stripped)
   if re.match(r'mem\s+\S+\s*:', stripped):
    memory = dict(indent=indent, lines=[])
   elif memory is not None and stripped:
    memory['lines'].append(stripped)
   match = re.match(r'(cmem|smem)\s+\S+\s*:\s*(.*)', stripped)
   if match:
    record['memory'].append((match[1], match[2]))
 flush_memory()
 assert modules.keys() == inventory['modules'].keys()
 visited = set()
 order = []
 def visit(module):
  if module in visited:
   return
  visited.add(module)
  for child in modules[module]['children']:
   visit(child)
  order.append(module)
 visit('SimTop')
 multiplicity = Counter({'SimTop': 1})
 for module in reversed(order):
  for child, amount in modules[module]['children'].items():
   multiplicity[child] += multiplicity[module] * amount
 registers = Counter()
 memories = Counter()
 externals = Counter()
 for module, amount in multiplicity.items():
  registers.update({kind: count * amount for kind, count in Counter(v['type'] for v in inventory['modules'][module].values()).items()})
  for item in modules[module]['memory']:
   memories[json.dumps(item)] += amount
  if modules[module]['external']:
   externals[json.dumps(modules[module]['interface'])] += amount
 return dict(registers=registers, memories=memories, externals=externals,
             reachable_modules=len(multiplicity), instantiated_modules=sum(multiplicity.values()))


def register_type_bits(kind):
 if kind in ['Clock', 'Reset', 'AsyncReset']:
  return 1
 match = re.fullmatch(r'(?:UInt|SInt)<(\d+)>((?:\[\d+\])*)', kind)
 assert match, 'Unsupported changed FIR register type: ' + kind
 width = int(match[1])
 for length in re.findall(r'\[(\d+)\]', match[2]):
  width *= int(length)
 return width
