"""Add read-only LLPTW state/request observations to the calibrated load observer."""
from pathlib import Path
import re

from tea_dip_gsim_lsu_s1_probe import digest

PREFIX = 'cpu__DOT__l_soc__DOT__core_with_l2__DOT__core__DOT__memBlock__DOT__inner__DOT__ptw__DOT__ptw__DOT__llptw__DOT__'
FIELDS = [('state', 4), ('entries__DOT__req_info__DOT__vpn', 38),
          ('entries__DOT__req_info__DOT__source', 2),
          ('entries__DOT__req_info__DOT__s2xlate', 2), ('entries__DOT__wait_id', 3)]


def add_capture(root, header, cpp):
    path = root / 'src/main/scala/xiangshan/cache/mmu/PageTableWalker.scala'
    text = path.read_text().split('class LLPTW', 1)[1]
    enum, = re.findall(r'val (state_idle\s*::.*?)\s*::\s*Nil\s*=\s*Enum\(14\)', text, re.S)
    states = [name.strip() for name in enum.split('::')]
    assert states.index('state_idle') == 0 and states.index('state_mem_waiting') == 5
    assert 'XSPerfAccumulate("llptw_mem_req_count", io.mem.req.fire)' in text
    assert 'XSPerfAccumulate("llptw_mem_busy_cycle", PopCount(is_waiting) =/= 0.U)' in text
    assert 'XSPerfAccumulate("llptw_mem_wait_entry_cycle", PopCount(is_waiting))' in text
    bindings = {}

    def field(name, width, array=False):
        declaration = name + ('[8]' if array else '')
        assert re.search(r'uint(?:8|16|32|64)_t ' + re.escape(declaration) + r'; // width = ' + str(width) + ',', header), declaration
        bindings[declaration] = width
        return 'model->' + name

    current = [field(PREFIX + name, width, True) for name, width in FIELDS]
    nxt = [field(PREFIX + name + '$NEXT', width, True) for name, width in FIELDS]
    metrics = ['llptw_mem_req_count', 'llptw_mem_req_count_source0', 'llptw_mem_req_count_source1',
               'llptw_mem_req_count_source2', 'llptw_mem_busy_cycle', 'llptw_mem_wait_entry_cycle']
    increments = []
    for name in metrics:
        before = field('logEndpoint__DOT__' + name + 'Counter', 64)
        after = field('logEndpoint__DOT___' + name + 'Next_T', 64)
        increments.append('uint64_t(' + after + ' - ' + before + ')')
    vpn = field(PREFIX + 'mem_arb__DOT__io__DOT__out__DOT__bits__DOT__req_info__DOT__vpn', 38)
    stage = field(PREFIX + 'mem_arb__DOT__io__DOT__out__DOT__bits__DOT__req_info__DOT__s2xlate', 2)
    helper = r'''
static uint64_t llptw_next[8][5]{}, llptw_previous[8][5]{};
static bool llptw_seen = false;
static uint64_t llptw_last_core = 0;
static FILE *llptw_output = nullptr;
static void llptw_write(unsigned kind, unsigned slot, const uint64_t *row) {
  if (!llptw_output) {
    llptw_output = fopen("llptw-probe.csv", "w");
    if (!llptw_output) { perror("llptw-probe.csv"); abort(); }
    fprintf(llptw_output, "core_cycle,clean,native_dump,kind,slot,state,virtual_page,source,translation_stage,wait_id\n");
  }
  fprintf(llptw_output, "%lu,%u,%u,%u,%u,%lu,%lu,%lu,%lu,%lu\n",
    context.core,context.clean,context.dump,kind,slot,row[0],row[1],row[2],row[3],row[4]);
}
static void llptw_snapshot(SSimTop *model) {
  for (unsigned slot=0; slot<8; ++slot) {
SNAPSHOTS
  }
}
static void llptw_observe(SSimTop *model) {
  if (!context.seen) return;
  if (llptw_seen) assert(context.core == llptw_last_core + 1);
  const uint64_t increments[6] = {INCREMENTS};
  assert(increments[0] <= 1);
  unsigned source=3;
  for (unsigned i=0; i<3; ++i) {
    assert(increments[i+1] <= 1);
    if (increments[i+1]) { assert(source==3); source=i; }
  }
  assert(increments[0] == increments[1]+increments[2]+increments[3]);
  const bool boundary = context.clean || context.dump;
  const bool force = !llptw_seen || boundary;
  if (boundary) { const uint64_t row[5]{}; llptw_write(0,8,row); }
  if (increments[0]) {
    const uint64_t row[5] = {255,REQUEST_VPN,source,REQUEST_STAGE,8};
    llptw_write(1,8,row);
  }
  unsigned waiting=0;
  for (unsigned slot=0; slot<8; ++slot) {
    const uint64_t row[5] = {CURRENT};
    assert(row[0] < 14);
    assert(llptw_next[slot][0] == row[0]);
    if (row[0]) for (unsigned j=1; j<5; ++j) assert(llptw_next[slot][j] == row[j]);
    waiting += row[0] == 5;
    bool changed = llptw_previous[slot][0] != row[0];
    if (row[0]) for (unsigned j=1; j<5; ++j) changed |= llptw_previous[slot][j] != row[j];
    if (force || changed) llptw_write(2,slot,row);
    for (unsigned j=0; j<5; ++j) llptw_previous[slot][j] = row[j];
  }
  assert(increments[4] == unsigned(waiting != 0));
  assert(increments[5] == waiting);
  llptw_seen=true; llptw_last_core=context.core;
}
'''
    helper = helper.replace('SNAPSHOTS', '\n'.join(f'    llptw_next[slot][{i}] = {value}[slot];' for i, value in enumerate(nxt)))
    helper = helper.replace('CURRENT', ','.join('uint64_t(' + value + '[slot])' for value in current))
    helper = helper.replace('INCREMENTS', ','.join(increments)).replace('REQUEST_VPN', 'uint64_t(' + vpn + ')').replace('REQUEST_STAGE', 'uint64_t(' + stage + ')')
    for old, new in [('static Context context{};', 'static Context context{};\n' + helper),
                     ('  in_step = true; __real_', '  llptw_snapshot(model);\n  in_step = true; __real_'),
                     ('  assert(context.clean == ', '  llptw_observe(model);\n  assert(context.clean == ')]:
        assert cpp.count(old) == 1, old
        cpp = cpp.replace(old, new)
    return cpp, dict(fields=bindings, state_encoding=dict(zip(states, range(14))),
                     metric_order=metrics, per_cycle_native_request_partition_and_waiting_asserted=True,
                     next_register_commit_asserted=True, source_sha256={str(path): digest(path)},
                     new_RTL_state_bits=0,
                     scope='Current eight-entry LLPTW state/owner changes and native-qualified memory request increments. '
                           'Kinds:0perf boundary,1memory request,2current entry state. Request slot8/state255/wait_id8 '
                           'are unknown sentinels, not a selected-entry match. Source1 is the L1-TLB port aggregate; '
                           'source2 is L2-TLB prefetch. Entries can share memory through wait_id. Full runtime after '
                           'reset; native ROI comes from closed endpoint evidence. A VPN/time match alone is not '
                           'unique request identity, demand-load service latency or CPU critical-cycle attribution.')
