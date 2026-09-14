#!/usr/bin/env python3
import argparse
import json
import os.path as osp
import re
import sys

SCRIPT_DIR = osp.dirname(osp.abspath(__file__))
REPO_ROOT = osp.abspath(osp.join(SCRIPT_DIR, '..', '..'))
COUNTERS_SCALA = osp.join(REPO_ROOT, 'src', 'main', 'scala', 'xiangshan', 'TopDownCounters.scala')
DEFAULT_METADATA_JSON = osp.join(REPO_ROOT, 'out', 'topdown-metadata.json')
SENTINEL_NAMES = {'NumStallReasons'}
REQUIRED_FIELDS = ('name', 'display_name', 'l1_group', 'l2_group', 'priority')


def parse_topdown_counters(counters_scala_path):
    text = open(counters_scala_path, encoding='utf-8').read()
    start = text.find('object TopDownCounters extends TopDownEnumeration')
    if start < 0:
        raise RuntimeError(f'TopDownCounters not found in {counters_scala_path}')
    names = []
    for line in text[start:].splitlines():
        stripped = line.lstrip()
        if stripped.startswith('//'):
            continue
        match = re.search(r'val\s+(\w+)\s*=\s*Value\("([^"]+)"(?:,\s*[\w.]+)?\)', line)
        if match:
            names.append((match.group(1), match.group(2)))
    mismatched = [decl for decl, literal in names if decl != literal]
    if mismatched:
        raise RuntimeError(f'TopDownCounters declaration/literal mismatch: {mismatched}')
    return [literal for _, literal in names]


def load_metadata(json_path):
    with open(json_path, encoding='utf-8') as f:
        data = json.load(f)
    counters = data.get('counters')
    if not isinstance(counters, list) or not counters:
        raise ValueError(f'TopDown metadata JSON has no counters: {json_path}')
    for item in counters:
        missing = [field for field in REQUIRED_FIELDS if field not in item]
        if missing:
            raise ValueError(f'TopDown metadata item missing {missing}: {item}')
    return data


def main(argv=None):
    parser = argparse.ArgumentParser(usage='check TopDown metadata against TopDownCounters')
    parser.add_argument('--metadata', default=DEFAULT_METADATA_JSON)
    parser.add_argument('--counters-scala', default=COUNTERS_SCALA)
    opt = parser.parse_args(argv)

    errors = []

    if not osp.isfile(opt.metadata):
        errors.append(
            f'metadata JSON not found: {opt.metadata}; '
            'run `mill topdownMeta.exportMetadata`'
        )
        print_report(errors)
        return 1

    try:
        meta = load_metadata(opt.metadata)
    except (OSError, ValueError, json.JSONDecodeError) as exc:
        errors.append(str(exc))
        print_report(errors)
        return 1

    counters = meta['counters']
    json_names = [item['name'] for item in counters]
    json_name_set = set(json_names)
    if len(json_names) != len(json_name_set):
        errors.append('metadata JSON contains duplicate counter names')

    priorities = [item['priority'] for item in counters]
    if priorities != sorted(priorities):
        errors.append('metadata JSON priority is not sorted in enum order')
    if len(priorities) != len(set(priorities)):
        errors.append('metadata JSON priority is not unique')
    if priorities and priorities[0] != 0:
        errors.append('NoStall / first metadata priority must stay 0')

    for item in counters:
        expected = f"{item['l1_group']}_{item['l2_group']}_{item['name']}"
        if item['display_name'] != expected:
            errors.append(f"{item['name']} display_name {item['display_name']!r} != {expected!r}")

    if not osp.isfile(opt.counters_scala):
        errors.append(f'TopDownCounters.scala not found: {opt.counters_scala}')
    else:
        enum_names = parse_topdown_counters(opt.counters_scala)
        expected = [name for name in enum_names if name not in SENTINEL_NAMES]
        missing = [name for name in expected if name not in json_name_set]
        extra = [name for name in json_names if name not in set(expected)]
        if missing:
            errors.append('JSON missing TopDownCounters: ' + ', '.join(missing))
        if extra:
            errors.append('JSON has names not in TopDownCounters: ' + ', '.join(extra))
        if 'NumStallReasons' in json_name_set:
            errors.append('JSON must not export the NumStallReasons sentinel')
        if json_names != expected and not missing and not extra:
            errors.append('JSON counter order does not match TopDownCounters declaration order')
        for idx, name in enumerate(json_names):
            if name in expected and counters[idx]['priority'] != idx:
                errors.append(f'{name} priority {counters[idx]["priority"]} != declaration index {idx}')
                break

    print_report(errors)
    return 1 if errors else 0


def print_report(errors):
    if errors:
        for item in errors:
            print(f'[ERROR] {item}')
        print('[FAIL] TopDown metadata consistency check failed')
    else:
        print('[OK] TopDown metadata is consistent with TopDownCounters')


if __name__ == '__main__':
    sys.exit(main())
