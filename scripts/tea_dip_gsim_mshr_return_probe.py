"""Add actual L1 MissEntry returns and LRQ MSHR tags to a calibrated LRQ relink."""
import argparse
import json
from pathlib import Path
import re
import subprocess

from tea_dip_gsim_load_service_probe import PREFIX
from tea_dip_gsim_lsu_s1_probe import digest

MSHR_FIELDS = ['valid', 'line', 'source', 'primary_source', 'acquired', 'first', 'last', 'hint', 'has_data']
MSHR_REGS = [('req_valid', 1), ('req__DOT__addr', 48), ('req__DOT__source', 4),
             ('req_primary_fire__DOT__source', 4), ('s_acquire', 1), ('w_grantfirst', 1),
             ('w_grantlast', 1), ('w_l2hint', 1), ('hasData', 1)]
TAG_FIELDS = ['valid', 'mshr_id', 'handled', 'issue_cycle', 'rob_flag', 'rob_index', 'rob_former', 'rank', 'uop_index', 'pc']
TAG_REGS = ['uop__DOT__perfDebugInfo__DOT__issueTime', 'uop__DOT__robIdx__DOT__flag',
            'uop__DOT__robIdx__DOT__value', 'uop__DOT__robIdx__DOT__isFormer',
            'uop__DOT__perfDebugInfo__DOT__teaMemberRank', 'uop__DOT__debug_seqNum__DOT__uopIdx', 'uop__DOT__pc']


def add_capture(root, header, cpp):
    mq = PREFIX + 'dcache__DOT__dcache__DOT__missQueue__DOT__'
    lrq = PREFIX + 'lsq__DOT__loadQueue__DOT__loadQueueReplay__DOT__'
    rtl = root / 'src/main/scala/xiangshan/cache/dcache/mainpipe/MissQueue.scala'
    text = rtl.read_text()
    assert 'io.mem_grant.ready := true.B' in text
    assert 'XSPerfAccumulate("penalty_waiting_for_channel_D", s_acquire && !w_grantlast && !io.mem_grant.valid)' in text

    def field(name, width):
        assert re.search(r'uint(?:8|16|32|64)_t ' + re.escape(name) + r'; // width = ' + str(width) + ',', header), name
        return 'model->' + name

    def entry(i, name, nxt=False):
        width = dict(MSHR_REGS)[name]
        return field(mq + f'entries_{i}__DOT__' + name + ('$NEXT' if nxt else ''), width)

    captures, snapshots, bindings = [], [], []
    for i in range(16):
        current = [entry(i, name) for name, _ in MSHR_REGS]
        nxt = [entry(i, name, True) for name, _ in MSHR_REGS]
        grant = field(mq + f'entries_{i}__DOT__io__DOT__mem_grant__DOT__valid', 1)
        data = field(mq + f'entries_{i}__DOT__io__DOT__mem_grant__DOT__bits__DOT__opcode$0_0', 1)
        snapshots.append('\n'.join(f'  miss_next[{i}][{j}] = ' + (f'(miss_next[{i}][0] ? uint64_t({value}) : 0);' if 1 <= j <= 3 else f'uint64_t({value});') for j, value in enumerate(nxt)))
        captures.append(f'''  {{
    const unsigned slot = {i};
    uint64_t now[9]{{}};
{chr(10).join(f'    now[{j}] = uint64_t({value});' for j,value in enumerate(current)if j==0 or j>=4)}
    if (now[0]) {{
{chr(10).join(f'      now[{j}] = uint64_t({current[j]});' for j in [1,2,3])}
    }}
    if (context.seen) {{
      for (unsigned j = 0; j < 9; ++j) assert(miss_next[slot][j] == now[j]);
      if (now[0]) {{
        assert((now[1] & 63) == 0);
        if (miss_seen && miss_previous[slot][0]) assert(now[1] == miss_previous[slot][1]);
      }} else {{ now[1] = now[2] = now[3] = 0; }}
      if (!miss_seen || std::memcmp(now, miss_previous[slot], sizeof(now)) || context.clean || context.dump)
        miss_write(1, slot, now, 0, 0);
      if ({grant}) {{
        assert(now[0] && now[4] && !now[6]);
        assert({nxt[5]});
        assert(bool({nxt[8]}) == bool({data}));
        miss_write(2, slot, now, unsigned({data}), unsigned({nxt[6]}));
      }}
      std::memcpy(miss_previous[slot], now, sizeof(now));
    }}
  }}''')
        bindings.append(dict(slot=i, current=current, next=nxt, grant_valid=grant, grant_has_data=data))
    for suffix in ['', '$NEXT']:
        assert re.search(r'uint8_t ' + re.escape(lrq + 'missMSHRId' + suffix) + r'\[128\]; // width = 5,', header)
    values = ['1', 'model->' + lrq + 'missMSHRId[i]', 'lrq_handled[i]'] + ['model->' + lrq + name + '[i]' for name in TAG_REGS]
    # These exact generated write guards include needReplay and redirect cancellation.
    model = root / 'build/gsim-compile/model'
    paths = subprocess.check_output(['rg', '-l', '-F', lrq + 'missMSHRId$NEXT[', str(model),
                                     '--glob', '*.cpp'], text=True).splitlines()
    generated = '\n'.join(Path(path).read_text() for path in paths)
    enqueues, enqueue_bindings = [], []
    for lane in range(3):
        guard = field(lrq + '_needEnqueue_T_' + str(2 + 3 * lane), 1)
        index = field(lrq + 'enqIndex' + ('_' + str(lane) if lane else ''), 7)
        unit = PREFIX + f'LoadUnit_{lane}__DOT__s3__DOT__'
        handled, tag = field(unit + 'lqWriteHandledByMSHR', 1), field(unit + 'lqWriteMshrId', 4)
        fragment = (f'if ({guard[7:]}) {{\n'
                    f'        if ({handled[7:]}) {{\n'
                    f'          if (({index[7:]} < 120)) {{\n'
                    f'            {lrq}missMSHRId$NEXT[{index[7:]}] = {tag[7:]};')
        assert fragment in generated, 'Actual qualified LRQ tag-write guard changed'
        enqueues.append(f'''  if ({guard}) {{
    const unsigned slot = {index};
    assert(slot < 120);
    lrq_handled[slot] = bool({handled});
    if (lrq_handled[slot]) assert(lrq_tag_next[slot] == {tag});
  }}''')
        enqueue_bindings.append(dict(lane=lane, guard=guard, index=index, handled=handled, tag=tag))
    helper = f'''
#include <cstring>
static uint64_t miss_next[16][9]{{}}, miss_previous[16][9]{{}}, lrq_tag_next[120]{{}}, lrq_tag_previous[120][10]{{}};
static bool lrq_handled[120]{{}};
static bool miss_seen = false;
static FILE *miss_output = nullptr, *tag_output = nullptr;
static void miss_write(unsigned kind, unsigned slot, const uint64_t *values, unsigned data, unsigned complete) {{
  if (!miss_output) {{
    miss_output = fopen("load-mshr-state.csv", "w");
    if (!miss_output) {{ perror("load-mshr-state.csv"); abort(); }}
    fprintf(miss_output, "core_cycle,clean,native_dump,kind,mshr,{','.join(MSHR_FIELDS)},grant_has_data,grant_complete\\n");
  }}
  fprintf(miss_output, "%lu,%u,%u,%u,%u", context.core,context.clean,context.dump,kind,slot);
  for (unsigned j = 0; j < 9; ++j) fprintf(miss_output, ",%lu", values[j]);
  fprintf(miss_output, ",%u,%u\\n",data,complete);
}}
static void tag_write(unsigned kind, unsigned entry, const uint64_t *values) {{
  if (!tag_output) {{
    tag_output = fopen("load-lrq-mshr-tags.csv", "w");
    if (!tag_output) {{ perror("load-lrq-mshr-tags.csv"); abort(); }}
    fprintf(tag_output, "core_cycle,clean,native_dump,kind,entry,{','.join(TAG_FIELDS)}\\n");
  }}
  fprintf(tag_output, "%lu,%u,%u,%u,%u", context.core,context.clean,context.dump,kind,entry);
  for (unsigned j = 0; j < 10; ++j) fprintf(tag_output, ",%lu", values[j]);
  fprintf(tag_output, "\\n");
}}
static void miss_snapshot(SSimTop *model) {{
{chr(10).join(snapshots)}
  for (unsigned i = 0; i < 120; ++i) {{
    lrq_tag_next[i] = model->{lrq}missMSHRId$NEXT[i];
    if (!model->{lrq}allocated$NEXT[i]) lrq_handled[i] = false;
  }}
{chr(10).join(enqueues)}
}}
static void miss_observe(SSimTop *model) {{
  const uint64_t empty[9]{{}};
  if (context.seen && (context.clean || context.dump)) {{
    const uint64_t tag_empty[10]{{}};
    miss_write(0,16,empty,0,0); tag_write(0,120,tag_empty);
  }}
{chr(10).join(captures)}
  for (unsigned i = 0; i < 120; ++i) {{
    assert(lrq_tag_next[i] == model->{lrq}missMSHRId[i]);
    if (!context.seen) continue;
    uint64_t now[10]{{}};
    if (model->{lrq}allocated[i] && !model->{lrq}vecReplay__DOT__isvec[i] && (model->{lrq}cause[i] & 64)) {{
      const uint64_t active[10] = {{{','.join('uint64_t('+value+')' for value in values)}}};
      std::memcpy(now,active,sizeof(now));
    }}
    if (std::memcmp(now,lrq_tag_previous[i],sizeof(now)) || ((context.clean || context.dump) && now[0])) {{
      tag_write(1,i,now); std::memcpy(lrq_tag_previous[i],now,sizeof(now));
    }}
  }}
  if (context.seen) miss_seen = true;
}}
'''
    for old, new in [('static Context context{};', 'static Context context{};\n' + helper),
                     ('  replay_snapshot(model);', '  miss_snapshot(model);\n  replay_snapshot(model);'),
                     ('  replay_observe(model);', '  replay_observe(model);\n  miss_observe(model);')]:
        assert cpp.count(old) == 1, old
        cpp = cpp.replace(old, new)
    return cpp, dict(entries=16, LRQ_entries=120, state_fields=MSHR_FIELDS, tag_fields=TAG_FIELDS,
                     bindings=bindings, qualified_LRQ_tag_write_bindings=enqueue_bindings,
                     generated_tag_write_source_sha256={path: digest(Path(path)) for path in paths},
                     LRQ_handled_provenance_asserted=True, RTL_source_sha256={str(rtl): digest(rtl)},
                     next_register_commit_asserted=True, grant_qualification_asserted=True,
                     scope='Actual always-ready entry D-valid/data-bit events, next-register commitments during non-reset core callbacks and current LRQ C_DM tag. Acquire is a recorded flag; its pipe-register A handshake can precede the first valid entry sample.')


def build(lrq_build_proof, output):
    lrq_build_proof, output = Path(lrq_build_proof).resolve(), Path(output).resolve()
    previous = json.loads(lrq_build_proof.read_text())
    assert previous['passed'] and previous['original_files_unchanged'] and previous['original_objects'] == 577
    assert previous['replay_state_binding']['entries'] == 120 and not output.exists()
    assert previous['generator_sha256'] == digest(Path(__file__).with_name('tea_dip_gsim_replay_state_probe.py'))
    parent, root = lrq_build_proof.parent, Path(previous['root'])
    assert digest(parent / 'probe.cpp') == previous['probe_cpp_sha256']
    for name, value in previous['original_sha256'].items():
        assert digest(Path(name)) == value, name
    cpp, binding = add_capture(root, (root / 'build/gsim-compile/model/SimTop.h').read_text(),
                               (parent / 'probe.cpp').read_text())
    output.mkdir(parents=True)
    (output / 'probe.cpp').write_text(cpp)
    (output / 'objects.rsp').write_bytes((parent / 'objects.rsp').read_bytes())
    commands = [[arg.replace(str(parent), str(output)) for arg in command] for command in previous['commands']]
    for i, command in enumerate(commands):
        with (output / f'build-{i}.stdout').open('wb') as out, (output / f'build-{i}.stderr').open('wb') as err:
            subprocess.run(command, stdout=out, stderr=err, check=True)
    for name, value in previous['original_sha256'].items():
        assert digest(Path(name)) == value, name
    result = dict(previous, binary=str(output / 'emu'), binary_sha256=digest(output / 'emu'), commands=commands,
                  probe_cpp_sha256=digest(output / 'probe.cpp'), generator_sha256=digest(Path(__file__)),
                  base_LRQ_build_proof_sha256=digest(lrq_build_proof), MSHR_return_binding=binding,
                  scope='One further host-only observer object relinked against the same577 model objects. No modeled resource growth. Closed old-output neutrality, D-wait marginals, generations and joins still require real calibration.')
    (output / 'build-proof.json').write_text(json.dumps(result, indent=2) + '\n')
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--lrq-build-proof', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    result = build(args.lrq_build_proof, args.output)
    print(json.dumps({name: result[name] for name in ['passed', 'binary', 'binary_sha256', 'original_objects']}))


if __name__ == '__main__':
    main()
