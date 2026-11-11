"""Discover SC capture tables from emitted C++; prediction and training widths differ."""
from pathlib import Path
import re

FAMILIES = ['scCondTrace_', 'scMatchedConditional_', 'scPredictionOpposition_']


def generated_sc_tables(generated_cpp):
    names = re.findall(r'\bvoid init_db_(\w+)\s*\(', Path(generated_cpp).read_text())
    result = []
    for prefix in FAMILIES:
        tables = sorted({name for name in names if re.fullmatch(re.escape(prefix) + r'\d+', name)},
                        key=lambda name: int(name.removeprefix(prefix)))
        assert tables and tables == [prefix + str(i) for i in range(len(tables))], prefix
        result.extend(tables)
    return result
