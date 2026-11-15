"""Observe load translation provenance and TLB residency in bounded core ranges."""
from pathlib import Path
import re

from tea_dip_gsim_load_service_probe import IDENTITY, PREFIX
from tea_dip_gsim_lsu_s1_probe import digest

COLUMNS = ['kind', 'lane', 'way', 'rob_flag', 'rob_index', 'rob_former', 'rank', 'uop_index',
           'issue_cycle', 'pc', 'virtual_address', 'no_query', 'request_valid', 'translate',
           'storage_hit_mask', 'ptw_bypass_hit', 'combined_hit', 'refill_mask', 'refill_way',
           'sfence', 'satp_changed', 'satp_asid', 'entry_valid', 'entry_tag', 'entry_asid',
           'entry_vmid', 'entry_s2xlate', 'entry_level', 'entry_global', 'valididx', 'pteidx']


def add_capture(root, header, cpp, ranges):
    ranges = [tuple(pair) for pair in ranges]
    assert ranges and all(0 <= begin < end for begin, end in ranges)
    assert all(left[1] <= right[0] for left, right in zip(ranges, ranges[1:]))
    root = Path(root)
    tlb = PREFIX + 'dtlb_ld_tlb_ld__DOT__'
    page = tlb + 'entries__DOT__page_ldtlb_storage_fa__DOT__'
    bindings = {}

    def field(name, width, indices=''):
        declaration = r'uint(?:8|16|32|64)_t ' + re.escape(name) + r'(\[[0-9]+\])*; // width = ' + str(width) + ','
        assert re.search(declaration, header), name
        bindings[name] = width
        return 'model->' + name + indices

    sources = [root / 'src/main/scala/xiangshan/cache/mmu' / name for name in ['TLB.scala', 'TLBStorage.scala']]
    compact = [re.sub(r'\s+', '', p.read_text()) for p in sources]
    for equation in ['valhit=e_hit||p_hit', 'valp_hit=GatedValidRegNext(p_hit_fast)']:
        assert equation in compact[0], equation
    for equation in ['valrefill_mask=Mux(io.w.valid,UIntToOH(io.w.bits.wayIdx),0.U(nWays.W))',
                     's2xlate_hit&&hit&&m._1&&!m._2', 'valhitVecReg=RegEnable(hitVec,req.fire)']:
        assert equation in compact[1], equation
    ways = sorted(int(i) for i in re.findall(re.escape(page) + r'hitVecReg_(\d+);', header))
    assert ways == list(range(len(ways))) and 0 < len(ways) <= 64
    columns = {name: i for i, name in enumerate(COLUMNS)}

    def set_row(values):
        return '\n'.join(f'    row[{columns[name]}] = uint64_t({value});' for name, value in values.items())

    global_values = dict(refill_mask=field(page + 'refill_mask_3', len(ways)),
                         refill_way=field(tlb + 'entries__DOT__refill_idx', 6),
                         sfence=field(tlb + 'sfence_delay__DOT__REG_1__DOT__valid', 1),
                         satp_changed=field(tlb + 'csr_delay__DOT__REG_1__DOT__satp__DOT__changed', 1),
                         satp_asid=field(tlb + 'csr_delay__DOT__REG_1__DOT__satp__DOT__asid', 16))
    cases = []
    for lane in range(3):
        load = PREFIX + f'LoadUnit_{lane}__DOT__s0__DOT__'
        identity = {name: field(load + 'pipeOutBits__DOT__' + suffix, width)
                    for name, (suffix, width) in zip(COLUMNS[3:11], IDENTITY[:8])}
        hits = ' | '.join(f'(uint64_t({field(page + (f"hitVecReg_{lane}_0_{way}" if lane else f"hitVecReg_{way}"), 1)}) << {way})' for way in ways)
        bypass = field(tlb + 'readResult_p_hit_last_REG' + (f'_{lane}' if lane else ''), 1)
        combined = field(tlb + f'hit_read_{lane}', 1)
        values = dict(kind='1', lane=str(lane), **identity,
                      no_query=field(load + 'pipeOutBits__DOT__noQuery', 1),
                      request_valid=field(tlb + f'req_out_v_{lane}', 1),
                      translate=field(tlb + f'portTranslateEnable_{lane}', 1),
                      storage_hit_mask='(' + hits + ')', ptw_bypass_hit=bypass, combined_hit=combined)
        valid = field(load + 'pipeOutValid', 1)
        cases.append(f'''  if ({valid}) {{
    uint64_t row[{len(COLUMNS)}]{{}};
{set_row(values)}
    assert(bool(row[{columns['combined_hit']}]) == bool(row[{columns['storage_hit_mask']}] || row[{columns['ptw_bypass_hit']}]));
    dtlb_write(row);
  }}''')
    entry_values = dict(kind='2', way='way', entry_valid=field(page + 'v', 1, '[way]'))
    for key, suffix, width in [('entry_tag', 'tag', 35), ('entry_asid', 'asid', 16),
                               ('entry_vmid', 'vmid', 14), ('entry_s2xlate', 's2xlate', 2),
                               ('entry_level', 'level', 2), ('entry_global', 'perm__DOT__g', 1)]:
        entry_values[key] = field(page + 'entries__DOT__' + suffix, width, '[way]')
    for key in ['valididx', 'pteidx']:
        array = field(page + 'entries__DOT__' + key, 1, '[way][sector]')
        entry_values[key] = '(' + ' | '.join(f'(uint64_t({array.replace("[sector]", f"[{i}]")}) << {i})' for i in range(8)) + ')'
    selected = ' || '.join(f'(context.core >= {begin}ULL && context.core < {end}ULL)' for begin, end in ranges)
    helper = f'''
static FILE *dtlb_file = nullptr;
static void dtlb_write(const uint64_t *row) {{
  if (!dtlb_file) {{
    dtlb_file = fopen("load-dtlb-probe.csv", "w"); assert(dtlb_file);
    fprintf(dtlb_file, "core_cycle,clean,native_dump,{','.join(COLUMNS)}\\n");
  }}
  fprintf(dtlb_file, "%lu,%u,%u", context.core, context.clean, context.dump);
  for (unsigned i=0; i<{len(COLUMNS)}; ++i) fprintf(dtlb_file, ",%lu", row[i]);
  fputc('\\n', dtlb_file);
}}
static void dtlb_observe(SSimTop *model) {{
  if (!context.seen || !({selected})) return;
  {{
    uint64_t row[{len(COLUMNS)}]{{}};
{set_row(global_values)}
    if (row[{columns['refill_mask']}]) assert(row[{columns['refill_way']}] < {len(ways)} && row[{columns['refill_mask']}] == (1ULL << row[{columns['refill_way']}]));
    dtlb_write(row);
  }}
{chr(10).join(cases)}
  for (unsigned way=0; way<{len(ways)}; ++way) {{
    uint64_t row[{len(COLUMNS)}]{{}};
{set_row(entry_values)}
    dtlb_write(row);
  }}
}}
'''
    marker = 'static bool in_step = false;'
    call = '  if (context.seen && (context.clean || context.dump)) {'
    assert cpp.count(marker) == 1 and cpp.count(call) == 1
    cpp = cpp.replace(marker, helper + '\n' + marker).replace(call, '  dtlb_observe(model);\n' + call)
    return cpp, dict(columns=COLUMNS, core_ranges=ranges, ways=len(ways), bindings=bindings,
                     generator_sha256=digest(Path(__file__)), source_sha256={str(p): digest(p) for p in sources},
                     new_RTL_state_bits=0,
                     scope='Current load S1 carried key, stored-hit mask versus registered PTW bypass, '
                           'and bounded per-cycle resident-way snapshots. Registered lookup masks refer '
                           'to preceding lookup state; refill masks describe the next write. Validate '
                           'each transition and S1/S2 phase on closed real captures before attribution. '
                           'Window-external history is unknown; hit does not prove residency or saved CPU cycles.')
