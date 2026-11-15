"""Read PLRU state and pending touches beside the bounded DTLB residency probe."""
from pathlib import Path
import re

from tea_dip_gsim_load_service_probe import PREFIX
from tea_dip_gsim_lsu_s1_probe import digest

COLUMNS = ['state', 'next_state', 'victim'] + [name for i in range(4)
           for name in (f'touch_valid_{i}', f'touch_way_{i}')]


def add_capture(root, header, cpp, ranges):
    root = Path(root)
    tlb = PREFIX + 'dtlb_ld_tlb_ld__DOT__entries__DOT__'
    page = tlb + 'page_ldtlb_storage_fa__DOT__'
    bindings = {}

    def field(name, width):
        assert re.search(r'uint(?:8|16|32|64)_t ' + re.escape(name) +
                         r'; // width = ' + str(width) + ',', header), name
        bindings[name] = width
        return 'model->' + name

    paths = [root / 'src/main/scala/xiangshan/cache/mmu/TLBStorage.scala',
             root / 'rocket-chip/src/main/scala/util/Replacement.scala']
    storage, policy = [re.sub(r'\s+', '', p.read_text()) for p in paths]
    pending = ('re.access(page.access.map(_.touch_ways))valstate=re.state_read.asTypeOf(UInt(re.nBits.W))'
               're.get_replace_way(re.get_next_state(state,page.access.map(_.touch_ways)))') in storage
    assert pending or 're.access(page.access.map(_.touch_ways))re.way' in storage
    assert 'when(RegNext(io.w.valid)){io.access.map{access=>' in storage
    assert 'defway=get_replace_way(state_reg)' in policy
    assert 'state_reg:=get_next_state(state_reg,touch_ways)' in policy
    values = [field(tlb + 'refill_idx_state_reg', 47),
              field(tlb + 'refill_idx_state_reg$NEXT', 47), field(tlb + 'refill_idx', 6)]
    for i in range(4):
        base = page + 'io__DOT__access__DOT__touch_ways__DOT__'
        values += [field(base + f'valid_{i}', 1), field(base + f'bits_{i}', 6)]
    selected = ' || '.join(f'(context.core >= {a}ULL && context.core < {b}ULL)' for a, b in ranges)
    helper = f'''
static FILE *dtlb_replacement_file = nullptr;
static void dtlb_replacement_observe(SSimTop *model) {{
  if (!context.seen || !({selected})) return;
  if (!dtlb_replacement_file) {{
    dtlb_replacement_file = fopen("load-dtlb-replacement-probe.csv", "w"); assert(dtlb_replacement_file);
    fprintf(dtlb_replacement_file, "core_cycle,clean,native_dump,{','.join(COLUMNS)}\\n");
  }}
  const uint64_t row[] = {{{','.join('uint64_t(' + v + ')' for v in values)}}};
  fprintf(dtlb_replacement_file, "%lu,%u,%u", context.core, context.clean, context.dump);
  for (auto value : row) fprintf(dtlb_replacement_file, ",%lu", value);
  fputc('\\n', dtlb_replacement_file);
}}
'''
    marker = 'static bool in_step = false;'
    call = '  dtlb_observe(model);'
    assert cpp.count(marker) == cpp.count(call) == 1
    cpp = cpp.replace(marker, helper + '\n' + marker).replace(call, call + '\n  dtlb_replacement_observe(model);')
    binding = dict(columns=COLUMNS, ways=48, ports=4, core_ranges=list(ranges), bindings=bindings,
                     source_sha256={str(p): digest(p) for p in paths}, generator_sha256=digest(Path(__file__)),
                     new_RTL_state_bits=0, scope='Actual registered PLRU state, computed NEXT and four touches. '
                     'Local next-state victim is a counterfactual selector, not a simulated speedup.')
    if pending:
        binding.update(victim_state='next_state',
                       scope='Actual victim decodes pending NEXT; registered state and four touches are also recorded. '
                             'State selection does not establish a speedup or exclusive CPU loss.')
    return cpp, binding
