"""Relink an old GSIM model to observe qualified DTLB filter arbitration."""
import argparse
import json
from pathlib import Path
import re

from tea_dip_gsim_load_service_probe import PREFIX, build
from tea_dip_gsim_lsu_s1_probe import digest

COLUMNS = ['kind', 'valid_mask', 'ready_mask', 'downstream_ready', 'chosen',
           'load_valid_mask', 'load_sent_mask', 'load_issue_index', 'load_request_vpn',
           'head_address_valid', 'head_virtual_page', 'head_match_mask'] + ['entry_vpn' + str(i) for i in range(16)]


def source(root, header, database, symbols):
    fields = {}

    def field(name, width, shape=''):
        assert re.search(r'uint(?:8|16|32|64)_t ' + re.escape(name + shape) +
                         r'; // width = ' + str(width) + ',', header), name
        fields[name] = dict(width=width, shape=shape)
        return 'model->' + name

    repeater = root / 'src/main/scala/xiangshan/cache/mmu/Repeater.scala'
    text = re.sub(r'\s+', '', repeater.read_text())
    for equation in ['valfilter=load_filter++store_filter++prefetch_filter',
                     'valptw_arb=Module(newRRArbiterInit(newPtwReq,3))',
                     'valissuevec=v.zip(sent).map{case(v,s)=>v&&!s}',
                     'valissueindex=firstValidIndex(issuevec,true.B)',
                     'io.ptw.req(0).valid:=canissue',
                     'io.ptw.req(0).bits.vpn:=vpn(issueindex)']:
        assert equation in text, equation
    arb = PREFIX + 'dtlbRepeater__DOT__ptw_arb__DOT__'
    load = PREFIX + 'dtlbRepeater__DOT__load_filter_load_entry__DOT__'
    rob = 'cpu__DOT__l_soc__DOT__core_with_l2__DOT__core__DOT__backend__DOT__inner__DOT__ctrlBlock__DOT__rob__DOT__'
    valid = field(arb + 'io__DOT__in__DOT__valid', 1, '[4]')
    ready = [field(arb + f'_io_in_{i}_ready_T', 1) for i in range(3)]
    downstream = field(PREFIX + 'ptw__DOT__ptw__DOT__arb1__DOT___io_in_1_ready_T', 1)
    chosen = field(arb + 'io_chosen_choice', 2)
    out_valid = field(arb + 'io__DOT__out__DOT__valid', 1)
    request_vpn = field(arb + 'io__DOT__in__DOT__bits__DOT__vpn', 38, '[4]') + '[0]'
    load_valid = field(load + 'v', 1, '[16]')
    load_sent = field(load + 'sent', 1, '[16]')
    load_vpn = field(load + 'vpn', 38, '[16]')
    issue = field(load + 'issueindex', 4)
    head_index = field(rob + 'deqPtrGenModule__DOT__deqPtrVec__DOT__value_0', 8)
    head_valid = field(rob + 'debug_lsTopdownInfo__DOT__s1__DOT__vaddr_valid', 1, '[256]') + '[head_index]'
    head_address = field(rob + 'debug_lsTopdownInfo__DOT__s1__DOT__vaddr_bits', 50, '[256]') + '[head_index]'
    clean = field('difftest__DOT__perfCtrl__DOT__clean', 1)
    dump = field('difftest__DOT__perfCtrl__DOT__dump', 1)
    step = '_ZN7SSimTop4stepEv'
    assert any(line.split()[-1:] == [step] for line in symbols.splitlines())
    writer, = [line.split()[-1] for line in symbols.splitlines() if 'InstTable0Writer' in line]
    signature, = re.findall(r'void InstTable0Writer\((.*?)\) \{', database, re.S)
    args = [item.strip().split()[-1].lstrip('*') for item in signature.split(',')]
    values = ['1', 'valid_mask', 'ready_mask', downstream, chosen, 'entry_mask', 'sent_mask', issue,
              request_vpn, head_valid, 'head_page', 'head_mask'] + [load_vpn + '[' + str(i) + ']' for i in range(16)]
    cpp = f'''#include "SimTop.h"
#include <cassert>
#include <cstdint>
#include <cstdio>
static bool in_step = false, seen = false;
static uint64_t core = 0;
static FILE *output = nullptr;
static void record(unsigned clean, unsigned dump, const uint64_t *row) {{
  if (!output) {{
    output = fopen("ptw-arb-probe.csv", "w"); assert(output);
    fprintf(output, "core_cycle,clean,native_dump,{','.join(COLUMNS)}\\n");
  }}
  fprintf(output, "%lu,%u,%u", core, clean, dump);
  for (unsigned i=0; i<{len(COLUMNS)}; ++i) fprintf(output, ",%lu", row[i]);
  fputc('\\n', output);
}}
extern "C" void __real_{writer}({signature});
extern "C" void __wrap_{writer}({signature}) {{
  if (!reset) {{ assert(in_step); if (seen) assert(core == stamp); seen = true; core = stamp; }}
  __real_{writer}({','.join(args)});
}}
extern "C" void __real_{step}(SSimTop *);
extern "C" void __wrap_{step}(SSimTop *model) {{
  assert(!in_step);
  const unsigned clean = {clean}, dump = {dump};
  seen = false; in_step = true; __real_{step}(model); in_step = false;
  assert(clean == {clean} && dump == {dump});
  if (!seen) return;
  if (clean || dump) {{ const uint64_t row[{len(COLUMNS)}]{{}}; record(clean,dump,row); }}
  unsigned valid_mask = 0, ready_mask = 0;
  const unsigned ready[3] = {{{','.join(ready)}}};
  for (unsigned i=0; i<3; ++i) {{
    valid_mask |= unsigned({valid}[i]) << i;
    ready_mask |= ready[i] << i;
  }}
  assert(bool({out_valid}) == bool(valid_mask));
  if (valid_mask) assert({chosen} < 3 && (valid_mask & (1u << {chosen})));
  const unsigned fires = valid_mask & ready_mask;
  assert(fires == ((valid_mask && {downstream}) ? 1u << {chosen} : 0));
  const unsigned head_index = {head_index};
  const uint64_t head_page = {head_address} >> 12;
  unsigned entry_mask = 0, sent_mask = 0, head_mask = 0, highest = 0;
  for (unsigned i=0; i<16; ++i) {{
    entry_mask |= unsigned({load_valid}[i]) << i;
    sent_mask |= unsigned({load_sent}[i]) << i;
    if ({load_valid}[i] && !{load_sent}[i]) highest = i;
    if ({head_valid} && {load_valid}[i] && {load_vpn}[i] == head_page) head_mask |= 1u << i;
  }}
  assert(bool(valid_mask & 1) == bool(entry_mask & ~sent_mask));
  if (valid_mask & 1) {{ assert({issue} == highest); assert({request_vpn} == {load_vpn}[highest]); }}
  if (valid_mask || entry_mask) {{
    const uint64_t row[] = {{{','.join('uint64_t(' + v + ')' for v in values)}}};
    record(clean,dump,row);
  }}
}}
'''
    return cpp, [step, writer], dict(kind='DTLB_filter_arbitration', columns=COLUMNS, fields=fields,
        RTL_source_sha256={str(repeater): digest(repeater)}, generator_sha256=digest(Path(__file__)),
        new_RTL_state_bits=0,
        scope='Three-group valid/ready handshakes and load-filter entry selection. Load group includes assigned prefetch ports. ROB-head debug VPN matching is a page-level candidate proxy, not a unique dynamic load or proven CPU stall. Native phase margins and execution neutrality require closed-guest calibration.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--compiler', default='clang++')
    parser.add_argument('--dramsim3-library', type=Path, required=True)
    args = parser.parse_args()
    proof = build(args.root, args.output, args.compiler, args.dramsim3_library, source_factory=source)
    print(json.dumps({key: proof[key] for key in ['passed', 'binary', 'binary_sha256', 'original_files_unchanged']}))


if __name__ == '__main__':
    main()
