"""Compare sampled TEA deltas with full endpoint budgets for a validated closed pair."""
from pathlib import Path
from tea_dip_query_endpoint_evidence import STATES,digest


def concordance(audits,comparison):
    assert len(audits)==2 and audits[0]['point']==audits[1]['point']
    assert [a['cpi'] for a in audits]==[comparison['control_cpi'],comparison['candidate_cpi']]
    sampled={state:audits[1]['metrics']['TEA_'+state]-audits[0]['metrics']['TEA_'+state] for state in STATES}
    exact=comparison['exact_state_delta_cpi']
    coverage=[audit['coverage'] for audit in audits]
    return dict(sampled_state_delta_cpi=sampled,
                sampled_minus_endpoint_delta_cpi={state:sampled[state]-exact[state] for state in STATES} if exact is not None else None,
                opposite_sign_states=[state for state in STATES if sampled[state]*exact[state]<0] if exact is not None else None,
                complete_grid_both_sides=all(c['observed_sample_rows']==c['expected_sample_boundaries'] for c in coverage),
                coverage=coverage,consumer_sha256=digest(Path(__file__)),
                scope='Strict nonzero sign disagreement, not statistical significance. Full grids do not prove representative state or PC attribution. Endpoint comparison requires matching ROI durations; event phase/prefix identity remains unverified.')
