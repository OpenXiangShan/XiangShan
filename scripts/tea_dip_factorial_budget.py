"""Measure two-policy interaction from four independently validated closed-guest budgets."""
import argparse
import json
import math
from pathlib import Path

CORNERS = ('B', 'C', 'D', 'CD')
FAMILIES = ('sampled_TEA_CPI', 'DIP_CPI', 'exact_TEA_CPI')


def analyze(corners):
    assert set(corners) == set(CORNERS)
    cpi = {}
    for name, data in corners.items():
        cpi[name] = data['native_cycles'] / data['native_retired']
        for family in FAMILIES:
            budget = data[family]
            if budget is not None:
                assert math.isclose(sum(budget.values()), cpi[name], rel_tol=0, abs_tol=1e-12)
    comparisons = {}
    for before, after in [('B', 'C'), ('B', 'D'), ('B', 'CD'), ('D', 'CD')]:
        delta = cpi[after] - cpi[before]
        states = {}
        for family in FAMILIES:
            left, right = corners[before][family], corners[after][family]
            if left is None or right is None:
                states[family] = None
                continue
            assert set(left) == set(right)
            states[family] = {state: right[state] - left[state] for state in left}
            assert math.isclose(sum(states[family].values()), delta, rel_tol=0, abs_tol=1e-12)
        comparisons[after + '_vs_' + before] = dict(
            speedup=cpi[before] / cpi[after] - 1, delta_CPI=delta, state_delta_CPI=states)
    interaction = cpi['CD'] - cpi['D'] - cpi['C'] + cpi['B']
    state_interactions = {}
    for family in FAMILIES:
        budgets = {name: corners[name][family] for name in CORNERS}
        if any(value is None for value in budgets.values()):
            state_interactions[family] = None
            continue
        assert all(set(value) == set(budgets['B']) for value in budgets.values())
        values = {state: budgets['CD'][state] - budgets['D'][state] - budgets['C'][state] + budgets['B'][state]
                  for state in budgets['B']}
        assert math.isclose(sum(values.values()), interaction, rel_tol=0, abs_tol=1e-12)
        state_interactions[family] = values
    return dict(passed=True, native_CPI=cpi, comparisons=comparisons, interaction_CPI=interaction,
                state_interaction_CPI=state_interactions,
                scope='B is baseline, C/D are individual policies, CD is their combination. '
                      'Interaction is CPI_CD-CPI_D-CPI_C+CPI_B; negative means less CPI than the additive-CPI '
                      'prediction, not automatically a gain over B. Each corner retains its own retired denominator. '
                      'Sampled TEA and DIP are separate budgets; exact TEA may be unavailable. Native condition '
                      'counters are not additive losses. Input closure, same workload/window/seed and resource '
                      'contracts must be independently established. No dynamic-prefix identity, unique causality '
                      'or suite score is inferred.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--input', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    assert not args.output.exists()
    result = analyze(json.loads(args.input.read_text()))
    args.output.write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(result, indent=2))


if __name__ == '__main__':
    main()
