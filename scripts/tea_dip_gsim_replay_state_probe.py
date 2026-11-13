"""Add read-only LRQ state changes to a calibrated successful-load-WB host relink."""
import argparse
import json
from pathlib import Path
import re
import subprocess

from tea_dip_gsim_load_service_probe import PREFIX
from tea_dip_gsim_lsu_s1_probe import digest

FIELDS = ['allocated', 'scheduled', 'blocking', 'eligible', 'cause', 'is_vector',
          'issue_cycle', 'rob_flag', 'rob_index', 'rob_former', 'rank', 'uop_index', 'pc']
ARRAYS = [('allocated', 1), ('scheduled', 1), ('blocking', 1), ('cause', 13),
          ('vecReplay__DOT__isvec', 1), ('uop__DOT__perfDebugInfo__DOT__issueTime', 64),
          ('uop__DOT__robIdx__DOT__flag', 1), ('uop__DOT__robIdx__DOT__value', 8),
          ('uop__DOT__robIdx__DOT__isFormer', 1), ('uop__DOT__perfDebugInfo__DOT__teaMemberRank', 3),
          ('uop__DOT__debug_seqNum__DOT__uopIdx', 8), ('uop__DOT__pc', 50)]


def add_capture(header, cpp):
    prefix = PREFIX + 'lsq__DOT__loadQueue__DOT__loadQueueReplay__DOT__'

    def pieces(name):
        found = re.findall(r'uint(?:8|16|32|64)_t (' + re.escape(prefix + name) +
                           r'\$(\d+)_(\d+)); // width = (\d+),', header)
        found = sorted((int(lo), int(hi), field, int(width)) for field, hi, lo, width in found)
        assert found and found[0][0] == 0
        assert all(hi - lo + 1 == width for lo, hi, _, width in found)
        assert all(a[1] + 1 == b[0] for a, b in zip(found, found[1:]))
        return found

    normal, fast = pieces('s0_loadNormalReplaySelMask'), pieces('s0_tlbHintWakeMask')
    size = normal[-1][1] + 1
    assert size == fast[-1][1] + 1 == 120
    for name, width in ARRAYS:
        for suffix in ['', '$NEXT']:
            assert re.search(r'uint(?:8|16|32|64)_t ' + re.escape(prefix + name + suffix) +
                             r'\[128\]; // width = ' + str(width) + ',', header), (name, suffix)

    def mask(name, fields):
        return f'static bool {name}(SSimTop *model, unsigned i) {{\n' + ''.join(
            f'  if (i >= {lo} && i <= {hi}) return (model->{field} >> (i - {lo})) & 1;\n'
            for lo, hi, field, _ in fields) + '  abort();\n}\n'

    def array(name, nxt=False):
        return 'model->' + prefix + name + ('$NEXT' if nxt else '') + '[i]'

    payload = [name for name, _ in ARRAYS[3:]]
    snapshot = '\n'.join(f'      replay_next[i][{j + 3}] = {array(name, True)};'
                         for j, name in enumerate(payload))
    check = '\n'.join(f'      assert(replay_next[i][{j + 3}] == {array(name)});'
                      for j, name in enumerate(payload))
    # Insert eligible after the first three registered controls in the CSV.
    values = [array(name) for name, _ in ARRAYS[:3]] + ['eligible'] + [array(name) for name in payload]
    fill = '\n'.join(f'      now[{j}] = {value};' for j, value in enumerate(values))
    helper = f'''
#include <cstring>
static uint64_t replay_next[{size}][{len(ARRAYS)}]{{}};
static uint64_t replay_previous[{size}][{len(FIELDS)}]{{}};
static FILE *replay_output = nullptr;
{mask('replay_eligible', normal)}
{mask('replay_fast_tlb', fast)}
static void replay_write(unsigned kind, unsigned entry, const uint64_t *values) {{
  if (!replay_output) {{
    replay_output = fopen("load-replay-state.csv", "w");
    if (!replay_output) {{ perror("load-replay-state.csv"); abort(); }}
    fprintf(replay_output, "core_cycle,clean,native_dump,kind,entry,{','.join(FIELDS)}\\n");
  }}
  fprintf(replay_output, "%lu,%u,%u,%u,%u", context.core, context.clean, context.dump, kind, entry);
  for (unsigned j = 0; j < {len(FIELDS)}; ++j) fprintf(replay_output, ",%lu", values[j]);
  fprintf(replay_output, "\\n");
}}
static void replay_snapshot(SSimTop *model) {{
  for (unsigned i = 0; i < {size}; ++i) {{
    replay_next[i][0] = {array('allocated', True)};
    replay_next[i][1] = {array('scheduled', True)};
    replay_next[i][2] = {array('blocking', True)};
    if (replay_next[i][0]) {{
{snapshot}
    }}
  }}
}}
static void replay_observe(SSimTop *model) {{
  const uint64_t empty[{len(FIELDS)}]{{}};
  if (context.seen && (context.clean || context.dump)) replay_write(0, {size}, empty);
  for (unsigned i = 0; i < {size}; ++i) {{
    assert(replay_next[i][0] == {array('allocated')});
    assert(replay_next[i][1] == {array('scheduled')});
    assert(replay_next[i][2] == {array('blocking')});
    if ({array('allocated')}) {{
{check}
    }}
    if (!context.seen) continue;
    bool eligible = replay_eligible(model, i);
    assert(eligible == (({array('allocated')} && !{array('scheduled')} && !{array('blocking')}) || replay_fast_tlb(model, i)));
    uint64_t now[{len(FIELDS)}]{{}};
    if ({array('allocated')}) {{
{fill}
    }}
    if (std::memcmp(now, replay_previous[i], sizeof(now)) || ((context.clean || context.dump) && now[0])) {{
      replay_write(1, i, now);
      std::memcpy(replay_previous[i], now, sizeof(now));
    }}
  }}
}}
'''
    anchors = [('static Context context{};', 'static Context context{};\n' + helper),
               ('  in_step = true; __real_', '  replay_snapshot(model);\n  in_step = true; __real_'),
               ('  assert(context.clean == ', '  replay_observe(model);\n  assert(context.clean == ')]
    for old, new in anchors:
        assert cpp.count(old) == 1, old
        cpp = cpp.replace(old, new)
    return cpp, dict(entries=size, array_padding=128, arrays=ARRAYS, fields=FIELDS,
                     actual_eligible_mask=normal, fast_TLB_mask=fast,
                     next_register_commit_asserted=True, eligible_definition_asserted=True,
                     scope='Current allocated entry state including release/cancel cycles. Ready means actual S0 eligibility, not data arrival. No selected-bank split or unique MSHR return identity.')


def build(wb_build_proof, output):
    wb_build_proof, output = Path(wb_build_proof).resolve(), Path(output).resolve()
    previous = json.loads(wb_build_proof.read_text())
    assert previous['passed'] and previous['original_files_unchanged'] and previous['original_objects'] == 577
    assert previous['binding']['writeback_capture'] and not output.exists()
    assert previous['generator_sha256'] == digest(Path(__file__).with_name('tea_dip_gsim_load_service_probe.py'))
    for path, value in previous['binding']['RTL_source_sha256'].items():
        assert digest(Path(path)) == value, path
    parent, root = wb_build_proof.parent, Path(previous['root'])
    assert digest(parent / 'probe.cpp') == previous['probe_cpp_sha256']
    assert digest(Path(previous['binary'])) == previous['binary_sha256']
    for path, value in previous['original_sha256'].items():
        assert digest(Path(path)) == value, path
    cpp, binding = add_capture((root / 'build/gsim-compile/model/SimTop.h').read_text(),
                               (parent / 'probe.cpp').read_text())
    output.mkdir(parents=True)
    (output / 'probe.cpp').write_text(cpp)
    (output / 'objects.rsp').write_bytes((parent / 'objects.rsp').read_bytes())
    commands = [[arg.replace(str(parent), str(output)) for arg in command] for command in previous['commands']]
    for i, command in enumerate(commands):
        with (output / f'build-{i}.stdout').open('wb') as out, (output / f'build-{i}.stderr').open('wb') as err:
            subprocess.run(command, stdout=out, stderr=err, check=True)
    for path, value in previous['original_sha256'].items():
        assert digest(Path(path)) == value, path
    result = dict(previous, binary=str(output / 'emu'), binary_sha256=digest(output / 'emu'),
                  commands=commands, probe_cpp_sha256=digest(output / 'probe.cpp'),
                  generator_sha256=digest(Path(__file__)),
                  base_WB_build_proof_sha256=digest(wb_build_proof),
                  replay_state_binding=binding,
                  scope='One extended host observer object relinked against the same577 original objects. Old S2/WB/native/SQL neutrality and real LRQ state semantics still require closed-guest calibration. No RTL state or capacity added.')
    (output / 'build-proof.json').write_text(json.dumps(result, indent=2) + '\n')
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--wb-build-proof', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    result = build(args.wb_build_proof, args.output)
    print(json.dumps({name: result[name] for name in ['passed', 'binary', 'binary_sha256', 'original_objects']}))


if __name__ == '__main__':
    main()
