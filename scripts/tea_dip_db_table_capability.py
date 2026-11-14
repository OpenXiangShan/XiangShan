"""Check generated ChiselDB table initializers before starting a capture.

Names and runtime enable flags survive even when a generated initializer is an
empty function. A nonempty initializer is necessary, not sufficient, for data:
writer semantics, runtime gates and closed table populations still need checks.
"""
import argparse
import json
from pathlib import Path
import re

from tea_dip_query_endpoint_evidence import digest


def analyze(database_cpp, tables):
    text = database_cpp.read_text()
    initializers = {}
    for table in tables:
        match = re.search(r'\bvoid init_db_' + re.escape(table) + r'\(\)\s*\{', text)
        status = ('missing_initializer' if match is None else
                  'empty_initializer' if re.match(r'\s*}', text[match.end():]) else 'implemented_initializer')
        initializers[table] = status
    return dict(database_cpp=str(database_cpp), database_cpp_sha256=digest(database_cpp),
                initializers=initializers,
                all_initializers_implemented=all(status == 'implemented_initializer' for status in initializers.values()),
                tool_sha256=digest(Path(__file__)),
                scope='Generated ChiselDB initializer implementation only. Empty/missing initializers '
                      'cannot create requested tables through this path; strings or CST flags are insufficient. '
                      'Nonempty initializers do not prove writer support, enabled runtime gates, nonzero events, '
                      'complete coverage, observation neutrality or performance. Inspect closed captures too.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--generated-db', required=True, type=Path)
    parser.add_argument('--tables', required=True, nargs='+')
    parser.add_argument('--output', required=True, type=Path)
    args = parser.parse_args()
    result = analyze(args.generated_db.resolve(), args.tables)
    args.output.write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(result, indent=2))


if __name__ == '__main__':
    main()
