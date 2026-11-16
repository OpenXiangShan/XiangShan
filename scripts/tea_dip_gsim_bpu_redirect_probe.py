"""Add resolve-time backend redirect metadata to the existing read-only FTQ observer."""
from pathlib import Path
import re
import subprocess

from tea_dip_gsim_bpu_probe import PREFIX
from tea_dip_gsim_lsu_s1_probe import digest

COLUMNS = ['kind', 'is_mispredict', 'ftq_flag', 'ftq_index', 'is_rvc', 'fetch_start_pc',
           'ftq_offset', 'cfi_position', 'actual_taken', 'actual_type', 'actual_ras',
           's1_source', 's3_source', 's2_override', 's3_override', 'priority_cause_mask']
CAUSES = ['wrong_taken', 'wrong_position', 'wrong_attribute', 'wrong_target']


def add_capture(root, cpp, value):
    root = Path(root)
    model = root / 'build/gsim-compile/model'
    bindings = {}
    paths = [root / 'src/main/scala/xiangshan/frontend/ftq/Ftq.scala',
             root / 'src/main/scala/xiangshan/frontend/bpu/Helpers.scala']
    assert 'withPriority = true' in paths[0].read_text()
    assert 'ftqOffset +& getAlignedInstOffset(startPc)' in paths[1].read_text()
    predicates, generated = {}, {}
    for cause in CAUSES:
        name = 'logEndpoint__DOT___squash_cycles_bp_wrong_redirect_' + cause + 'Next_T'
        found = subprocess.check_output(['rg', '-l', '-F', '--glob', '*.cpp', name + ' = ', str(model)], text=True).splitlines()
        assignments = []
        for path in found:
            generated[path] = digest(path)
            assignments += re.findall(r'^\s*' + re.escape(name) + r' = (.*);$', Path(path).read_text(), re.M)
        rhs, = set(assignments)
        predicate, = re.findall(r'\+ \(\(uint64_t\)([A-Za-z0-9_$]+)\)', rhs)
        predicates[cause] = dict(assignment=rhs, signal=predicate, expression=value(predicate))

    def redirect(name):
        key = PREFIX + 'backendRedirect__DOT__' + name
        bindings[name] = value(key)
        return bindings[name]

    valid = redirect('valid')
    names = ['isMisPred', 'ftqIdx__DOT__flag', 'ftqIdx__DOT__value', 'isRVC', 'pc',
             'ftqOffset', 'taken', 'attribute__DOT__branchType', 'attribute__DOT__rasAction']
    values = {name: redirect('bits__DOT__' + name) for name in names}
    sources = [value(PREFIX + 'perfQueue__DOT__bpuPerf__DOT__bpSource__DOT__' + name) + '[index]'
               for name in ['s1Source', 's3Source', 's2Override', 's3Override']]
    mask = ' | '.join(f'(uint64_t({predicates[cause]["expression"]}) << {i})' for i, cause in enumerate(CAUSES))
    payload = ['1', values['isMisPred'], values['ftqIdx__DOT__flag'], 'index', values['isRVC'],
               values['pc'], values['ftqOffset'], 'position', values['taken'],
               values['attribute__DOT__branchType'], values['attribute__DOT__rasAction'], *sources, 'mask']
    assert len(payload) == len(COLUMNS)
    helper = f'''
static FILE *bpu_redirect_output = nullptr;
static void bpu_redirect_record(const uint64_t *row) {{
  if (!bpu_redirect_output) {{
    bpu_redirect_output = fopen("bpu-redirect-probe.csv", "w"); assert(bpu_redirect_output);
    fprintf(bpu_redirect_output, "core_cycle,clean,native_dump,{','.join(COLUMNS)}\\n");
  }}
  fprintf(bpu_redirect_output, "%lu,%u,%u", context.core, context.clean, context.dump);
  for (unsigned i = 0; i < {len(COLUMNS)}; ++i) fprintf(bpu_redirect_output, ",%lu", row[i]);
  fputc('\\n', bpu_redirect_output);
}}
static void bpu_redirect_observe(SSimTop *model) {{
  if (context.clean || context.dump) {{ uint64_t row[{len(COLUMNS)}]{{}}; bpu_redirect_record(row); }}
  if (!({valid})) return;
  const unsigned index = {values['ftqIdx__DOT__value']};
  const uint64_t position = ((({values['pc']} >> 1) & 15) + {values['ftqOffset']}) & 31;
  const uint64_t mask = {mask};
  assert(!mask || !(mask & (mask - 1)));
  assert({values['isMisPred']} || !mask);
  const uint64_t row[] = {{{','.join('uint64_t(' + item + ')' for item in payload)}}};
  bpu_redirect_record(row);
}}
'''
    marker, call = 'static bool in_step = false;', '  if (!context.seen) return;'
    assert cpp.count(marker) == cpp.count(call) == 1
    cpp = cpp.replace(marker, helper + '\n' + marker).replace(call, call + '\n  bpu_redirect_observe(model);')
    return cpp, dict(columns=COLUMNS, units='qualified backendRedirect.valid cycles',
                     fields=bindings, native_priority_predicates=predicates,
                     generated_source_sha256=generated, RTL_source_sha256={str(p): digest(p) for p in paths},
                     generator_sha256=digest(Path(__file__)), new_RTL_state_bits=0,
                     scope='All backendRedirect.valid cycles, not committed FTQ entries or unique dynamic branches. '
                           'Cause predicates and selected-stage metadata are captured on the same observed redirect. '
                           'PC reference/offset/isRVC follow the old32-byte half-alignment protocol; '
                           'native margins, ELF mapping, clock phase and execution neutrality require calibration. '
                           'FTQ flag/index alone is not a lifetime identity across reuse, nor a commit/resolve join.')
