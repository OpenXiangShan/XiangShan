"""Use actual compiled/selected SC table indices instead of a fixed eight-slot assumption."""
from pathlib import Path
import re

FAMILIES = ['scCondTrace_', 'scMatchedConditional_', 'scPredictionOpposition_']


def selected_tables(names, prefix):
    tables = sorted({name for name in names if re.fullmatch(re.escape(prefix) + r'\d+', name)},
                    key=lambda name: int(name.removeprefix(prefix)))
    assert tables and tables == [prefix + str(i) for i in range(len(tables))], prefix
    return tables


def generated_sc_tables(generated_cpp):
    names = re.findall(r'\bvoid init_db_(\w+)\s*\(', Path(generated_cpp).read_text())
    groups = [selected_tables(names, prefix) for prefix in FAMILIES]
    assert len({len(tables) for tables in groups}) == 1, 'SC table families have different compiled widths'
    return [name for tables in groups for name in tables]
