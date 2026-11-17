"""Capture accepted PBOP training, generation gates and actual skid-buffer exits."""
import argparse
import json
from pathlib import Path
import re
import subprocess

from tea_dip_gsim_l2_return_probe import PREFIX
from tea_dip_gsim_lsu_s1_probe import digest

FIELDS = ['base_line', 'target_line', 'issue_offset', 'source_id', 'need_t', 'enable',
          'issue_enable', 'student_selected', 'student_offset', 'teacher_offset',
          'teacher_disabled', 'cross_page', 'degree', 'emitted',
          'train_count', 'enq_count', 'pop_count', 'emit_count', 'phase_count']


def add_capture(root, header, cpp):
    source = Path(root) / 'XSCache/src/main/scala/coupledL2/prefetch/BestOffsetPrefetch.scala'
    assert 'io.selectedOffset := selectedOffset.asUInt' in source.read_text()
    text = source.read_text().split('class PBestOffsetPrefetch', 1)[1]
    for fragment in ['val s0_fire = s0_ready && io.train.valid',
                     's1_req_valid := enable && !s0_crossPage && issueEnable',
                     'reqSkidBuffer.io.enq.valid := s1_req_valid',
                     'io.req.valid := enable && reqSkidBuffer.io.deq.valid && io.fdbkDegree > 0.U',
                     'reqSkidBuffer.io.deq.ready := io.req.ready']:
        assert fragment in text, fragment
    declarations = dict(re.findall(r'uint(?:8|16|32|64)_t (\S+); // width = (\d+),', header))
    parent = PREFIX + 'prefetcher__DOT__'
    prefix = parent + 'pbop__DOT__'

    def field(name, width=1, base=prefix):
        assert declarations.get(base + name) == str(width), base + name
        return 'model->' + base + name

    train = field('s0_fire')
    s1_valid = field('s1_req_valid')
    enqueue = f"({s1_valid} && {field('reqSkidBuffer__DOT___io_enq_ready_T')})"
    ready = field('_pbop_io_req_ready_T_2', base=parent)
    dequeue = f"({field('reqSkidBuffer__DOT__io__DOT__deq__DOT__valid')} && {ready})"
    emitted = field('_io_req_valid_T_2')
    phase = field('scoreTable__DOT__io_phaseEndPulse_REG')
    enable = field('enable')
    degree = field('prefetchController__DOT__io__DOT__l2PfFbCtrl__DOT__pbopDegree', 2, parent)

    def values(**entries):
        row = ['0'] * len(FIELDS)
        for key, value in entries.items():
            row[FIELDS.index(key)] = value
        return row

    def address(part):
        return f"((uint64_t({field(part + 'tag', 32)}) << 16) | (uint64_t({field(part + 'set', 10)}) << 6))"

    training = values(base_line=f"({field('s0_oldAddr', 48)} & ~uint64_t(63))",
                      target_line=f"({field('_s0_newAddr_T_5', 48)} & ~uint64_t(63))",
                      issue_offset=field('issueOffset', 7),
                      source_id=field('pbop_train_buf__DOT__io_deq_bits_MPORT__DOT__source', 8, parent),
                      need_t=field('pbop_train_buf__DOT__io_deq_bits_MPORT__DOT__needT', 1, parent),
                      enable=enable, issue_enable=field('issueEnable'),
                      student_selected=field('student__DOT__selectedEnable'),
                      student_offset=field('student__DOT__selectedOffset', 7),
                      teacher_offset=field('scoreTable__DOT__prefetchOffset', 7),
                      teacher_disabled=field('scoreTable__DOT__prefetchDisable'),
                      cross_page=field('s0_crossPage'), degree=degree)
    queued = values(target_line=address('s1_req__DOT__'), issue_offset=field('s1_issueOffset', 7),
                    source_id=field('s1_req__DOT__source', 8), need_t=field('s1_req__DOT__needT'))
    part = 'reqSkidBuffer__DOT__io__DOT__deq__DOT__bits__DOT__'
    popped = values(target_line=address(part + 'req__DOT__'),
                    issue_offset=field(part + 'issueOffset', 7),
                    source_id=field(part + 'req__DOT__source', 8), need_t=field(part + 'req__DOT__needT'),
                    enable=enable, degree=degree, emitted=emitted)
    committed = values(teacher_offset=field('scoreTable__DOT__phaseBestOffset', 7),
                       issue_enable=field('scoreTable__DOT__phaseIssueEnable'))
    captures = [f'  pbop_counts[0] += bool({train});', f'  pbop_counts[1] += bool({enqueue});',
                f'  pbop_counts[2] += bool({dequeue});',
                f'  pbop_counts[3] += bool({dequeue} && {emitted});',
                f'  pbop_counts[4] += bool({phase});']
    for kind, condition, row in [(1, train, training), (2, enqueue, queued),
                                  (3, dequeue, popped), (4, phase, committed)]:
        entries = ','.join('uint64_t(' + value + ')' for value in row)
        captures.append(f'  if ({condition}) {{ const uint64_t row[{len(FIELDS)}] = {{{entries}}}; pbop_write({kind},row); }}')
    captures.append('  if(context.clean || context.dump) pbop_footer(0);')
    helper = f'''
static FILE *pbop_output=nullptr;
static uint64_t pbop_counts[5]{{}},pbop_last_cycle=0;
static void pbop_finish();
static void pbop_write(unsigned kind,const uint64_t *row) {{
  if(!pbop_output) {{
    pbop_output=fopen("pbop-train-issue.csv","w");
    if(!pbop_output) {{perror("pbop-train-issue.csv");abort();}}
    fprintf(pbop_output,"core_cycle,clean,native_dump,kind,{','.join(FIELDS)}\\n");
    std::atexit(pbop_finish);
  }}
  fprintf(pbop_output,"%lu,%u,%u,%u",pbop_last_cycle,
    kind==5 ? 0 : context.clean,kind==5 ? 0 : context.dump,kind);
  for(unsigned i=0;i<{len(FIELDS)};++i) fprintf(pbop_output,",%lu",row[i]);
  fprintf(pbop_output,"\\n");
}}
static void pbop_footer(unsigned kind) {{
  uint64_t row[{len(FIELDS)}]{{}};
  for(unsigned i=0;i<5;++i) row[14+i]=pbop_counts[i];
  pbop_write(kind,row);
}}
static void pbop_finish() {{
  if(!pbop_output) return;
  pbop_footer(5);fclose(pbop_output);pbop_output=nullptr;
}}
static void pbop_observe(SSimTop *model) {{
  if(!context.seen) return;
  pbop_last_cycle=context.core;
{chr(10).join(captures)}
}}
'''
    assert cpp.count('static void directory_observe(SSimTop *model)') == 1
    assert cpp.count('  directory_observe(model);') == 1
    cpp = cpp.replace('static void directory_observe(SSimTop *model)', helper + '\nstatic void directory_observe(SSimTop *model)')
    cpp = cpp.replace('  directory_observe(model);', '  directory_observe(model);\n  pbop_observe(model);')
    return cpp, dict(fields=FIELDS, offset_bits=7, source_sha256=digest(source),
                     events=dict(train=train, enqueue=enqueue, dequeue=dequeue, emitted=emitted, phase=phase),
                     values=dict(training=training, queued=queued, popped=popped, committed=committed),
                     new_RTL_state_bits=0,
                     scope='Actual accepted training, S1-to-skid enqueue and skid dequeue including gate-discarded exits. '
                           'Logical offsets are seven bits; SInt fields retain signed8-bit GSIM host storage. '
                           'Counts are independent of emitted CSV rows. '
                           'Queue flow-through and censored tails need replay validation; emitted does not imply L2 S3 arrival.')


def build(previous_proof, output):
    previous_proof, output = Path(previous_proof).resolve(), Path(output).resolve()
    previous = json.loads(previous_proof.read_text())
    assert previous['passed'] and previous['original_files_unchanged'] and 'L2_directory_binding' in previous
    assert not output.exists()
    parent, root = previous_proof.parent, Path(previous['root'])
    assert digest(parent / 'probe.cpp') == previous['probe_cpp_sha256']
    for name, expected in previous['original_sha256'].items():
        assert digest(Path(name)) == expected, name
    cpp, binding = add_capture(root, (root / 'build/gsim-compile/model/SimTop.h').read_text(),
                               (parent / 'probe.cpp').read_text())
    output.mkdir(parents=True)
    (output / 'probe.cpp').write_text(cpp)
    (output / 'objects.rsp').write_bytes((parent / 'objects.rsp').read_bytes())
    commands = [[arg.replace(str(parent), str(output)) for arg in cmd] for cmd in previous['commands']]
    for i, command in enumerate(commands):
        with (output / f'build-{i}.stdout').open('wb') as stdout, (output / f'build-{i}.stderr').open('wb') as stderr:
            subprocess.run(command, stdout=stdout, stderr=stderr, check=True)
    for name, expected in previous['original_sha256'].items():
        assert digest(Path(name)) == expected, name
    result = dict(previous, binary=str(output / 'emu'), binary_sha256=digest(output / 'emu'), commands=commands,
                  probe_cpp_sha256=digest(output / 'probe.cpp'), generator_sha256=digest(Path(__file__)),
                  base_directory_build_proof_sha256=digest(previous_proof), PBOP_train_issue_binding=binding)
    (output / 'build-proof.json').write_text(json.dumps(result, indent=2) + '\n')
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--directory-build-proof', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    value = build(args.directory_build_proof, args.output)
    print(json.dumps({name: value[name] for name in ['passed', 'source', 'binary', 'binary_sha256']}))


if __name__ == '__main__':
    main()
