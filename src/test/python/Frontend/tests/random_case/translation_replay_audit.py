"""Read-only translation replay qualification; never grants HIT eligibility."""
from __future__ import annotations

import argparse
from collections import Counter
import hashlib
import json
from pathlib import Path
import re


_FTQ_SIZE = 64
_FTQ_MODULUS = 2 * _FTQ_SIZE
_POST_REDIRECT_RESIDUAL_CYCLES = 1
_PROGRAM_MAP_PATH_METADATA_FIELDS = frozenset({"source_path"})
# The replay contract uses redirect_min/max_delay=5/8.  Allow the sampled
# native output one additional edge after the model event.  Ten is therefore
# a conservative upper bound, not a value learned from this artifact.
_MAX_REDIRECT_CAUSAL_DELAY = 10
_COMPLETION_RE = re.compile(
    r"run until golden complete: cursor=(?P<cursor>\d+) entries=(?P<entries>\d+) "
    r"cycles=(?P<cycles>\d+) completion_pending_work=(?P<completion>\d+) "
    r"backend_pending_work=(?P<backend>\d+)"
)


def _post_replay_audit_only_paths() -> frozenset[str]:
    """Return tools that may change after a replay without changing its execution.

    Keep this list explicit and local to the random-case audit directory.  Files
    that generate inputs, configure a scenario, sample coverage, or drive/check
    the DUT must never be added here: a mismatch in any of those files invalidates
    replay provenance.
    """

    audit_dir = Path(__file__).resolve().parent
    basenames = (
        "translation_replay_audit.py",
        "test_translation_replay_audit.py",
        "target_hit_audit.py",
        "test_target_hit_audit.py",
        "candidate_offline_compare.py",
        "test_candidate_offline_compare.py",
        "pa_response_boundary_audit.py",
        "test_pa_response_boundary_audit.py",
        "timing_replay_compare.py",
        "test_timing_replay_compare.py",
    )
    return frozenset(str((audit_dir / basename).resolve()) for basename in basenames)


def _timeline_events(timeline: dict, event_type: str) -> list[dict]:
    return [
        event
        for event in (timeline.get("timeline") or [])
        if isinstance(event, dict) and event.get("type") == event_type
    ]


def _fold_pc(pc: int, width: int = 10) -> int:
    mask = (1 << int(width)) - 1
    value = int(pc) >> 1
    folded = 0
    while value:
        folded ^= value & mask
        value >>= int(width)
    return int(folded) & mask


def _ftq_pointer(payload: dict) -> int | None:
    if "ftq_flag" not in payload or "ftq_value" not in payload:
        return None
    return int(payload["ftq_flag"]) * _FTQ_SIZE + int(payload["ftq_value"])


def _program_ranges(timeline: dict) -> list[tuple[int, int]]:
    mapping = timeline.get("program_address_mapping") or {}
    return [
        (int(page["va"]), int(page["va"]) + 0x1000)
        for page in (mapping.get("pages") or [])
        if isinstance(page, dict) and "va" in page
    ]


def _exception_causal_review(artifact: dict, timeline: dict) -> dict:
    stats = artifact.get("stats") or {}
    oracle = stats.get("translation_oracle") or {}
    oracle_records = [
        record
        for record in (oracle.get("records") or [])
        if isinstance(record, dict)
        and record.get("kind") == "speculative_cfvec_exception"
    ]
    oracle_events = _timeline_events(
        timeline, "translation.speculative_cfvec_exception"
    )
    native_events = _timeline_events(timeline, "audit.native_cfvec_exception")
    ftq_writes = _timeline_events(timeline, "audit.native_ftq_write")
    redirects = _timeline_events(timeline, "audit.native_redirect")
    commits = _timeline_events(timeline, "backend.commit")
    ranges = _program_ranges(timeline)

    oracle_by_cycle: dict[int, list[dict]] = {}
    for record in oracle_records:
        oracle_by_cycle.setdefault(int(record.get("cycle", -1)), []).append(record)
    timeline_oracle_by_cycle: dict[int, list[dict]] = {}
    for event in oracle_events:
        timeline_oracle_by_cycle.setdefault(int(event.get("cycle", -1)), []).append(
            event.get("payload") or {}
        )

    rows: list[dict] = []
    failures: list[str] = []
    for event in native_events:
        cycle = int(event.get("cycle", -1))
        payload = event.get("payload") or {}
        pointer = _ftq_pointer(payload)
        value = payload.get("ftq_value")
        prior_writes = [
            candidate
            for candidate in ftq_writes
            if int(candidate.get("cycle", -1)) <= cycle
            and int((candidate.get("payload") or {}).get("ftq_idx_raw", -1))
            == int(value if value is not None else -1)
        ]
        write = prior_writes[-1] if prior_writes else None
        later_writes = [
            candidate
            for candidate in ftq_writes
            if int(candidate.get("cycle", -1)) > cycle
            and int((candidate.get("payload") or {}).get("ftq_idx_raw", -1))
            == int(value if value is not None else -1)
        ]
        next_write_cycle = (
            None if not later_writes else int(later_writes[0].get("cycle", -1))
        )
        reconstructed_pc = None
        fold_matches = False
        if write is not None:
            raw_start = int(
                (write.get("payload") or {}).get("start_pc_addr_raw", 0)
            )
            reconstructed_pc = (
                (raw_start << 1)
                + (int(payload.get("ftq_offset", 0)) << 1)
                - (0 if bool(payload.get("is_rvc")) else 2)
            ) & 0xFFFFFFFFFFFFFFFF
            fold_matches = _fold_pc(reconstructed_pc) == int(
                payload.get("folded_pc", -1)
            )

        cycle_oracle = oracle_by_cycle.get(cycle, [])
        cycle_timeline_oracle = timeline_oracle_by_cycle.get(cycle, [])
        oracle_matches = bool(
            len(cycle_oracle) == 1
            and len(cycle_timeline_oracle) == 1
            and reconstructed_pc is not None
            and int(cycle_oracle[0].get("pc", -1)) == reconstructed_pc
            and int(cycle_oracle[0].get("folded_pc", -1))
            == int(payload.get("folded_pc", -2))
            and cycle_oracle[0].get("fault") == "instruction_page_fault"
            and cycle_timeline_oracle[0] == cycle_oracle[0]
        )
        exception_bits = payload.get("exception_bits") or {}
        bit12_only = int(exception_bits.get("12", 0)) == 1 and all(
            int(exception_bits.get(str(bit), exception_bits.get(bit, 0))) == 0
            for bit in (1, 2, 19, 20)
        )
        outside_program = reconstructed_pc is not None and not any(
            start <= reconstructed_pc < end for start, end in ranges
        )

        previous_redirects = [
            candidate
            for candidate in redirects
            if int(candidate.get("cycle", -1)) <= cycle
        ]
        later_redirects = [
            candidate
            for candidate in redirects
            if int(candidate.get("cycle", -1)) > cycle
        ]
        causal_redirect = None
        classification = "unproved"
        delta = None
        distance = None
        if pointer is not None and previous_redirects:
            previous = previous_redirects[-1]
            previous_delta = cycle - int(previous.get("cycle", -1))
            previous_pointer = _ftq_pointer(previous.get("payload") or {})
            previous_distance = (
                None
                if previous_pointer is None
                else (pointer - previous_pointer) % _FTQ_MODULUS
            )
            if (
                previous_delta <= _POST_REDIRECT_RESIDUAL_CYCLES
                and previous_distance is not None
                and 0 < previous_distance < _FTQ_SIZE
            ):
                causal_redirect = previous
                classification = "post_redirect_residual"
                delta = previous_delta
                distance = previous_distance
        if causal_redirect is None and pointer is not None and later_redirects:
            following = later_redirects[0]
            following_delta = int(following.get("cycle", -1)) - cycle
            following_pointer = _ftq_pointer(following.get("payload") or {})
            following_distance = (
                None
                if following_pointer is None
                else (pointer - following_pointer) % _FTQ_MODULUS
            )
            if (
                following_delta <= _MAX_REDIRECT_CAUSAL_DELAY
                and following_distance is not None
                and 0 < following_distance < _FTQ_SIZE
            ):
                causal_redirect = following
                classification = "later_older_redirect_flush"
                delta = following_delta
                distance = following_distance

        generation_start = -1 if write is None else int(write.get("cycle", -1))
        generation_end = next_write_cycle
        matching_commits = [
            commit
            for commit in commits
            if _ftq_pointer(commit.get("payload") or {}) == pointer
            and int(commit.get("cycle", -1)) >= generation_start
            and (
                generation_end is None
                or int(commit.get("cycle", -1)) < generation_end
            )
        ]
        no_commit_in_generation = not matching_commits
        accepted_sample = int(payload.get("can_accept", 0)) == 1
        row_pass = all(
            (
                pointer is not None,
                write is not None,
                fold_matches,
                oracle_matches,
                bit12_only,
                outside_program,
                causal_redirect is not None,
                no_commit_in_generation,
                accepted_sample,
            )
        )
        if not row_pass:
            failures.append(f"cycle={cycle}")
        rows.append(
            {
                "cycle": cycle,
                "ftq": (
                    None
                    if pointer is None
                    else [int(payload["ftq_flag"]), int(payload["ftq_value"])]
                ),
                "slot": payload.get("slot"),
                "reconstructed_pc": reconstructed_pc,
                "folded_pc": payload.get("folded_pc"),
                "fault": "instruction_page_fault" if bit12_only else "other",
                "ftq_write_cycle": (
                    None if write is None else int(write.get("cycle", -1))
                ),
                "classification": classification,
                "redirect_cycle": (
                    None
                    if causal_redirect is None
                    else int(causal_redirect.get("cycle", -1))
                ),
                "redirect_delta_cycles": delta,
                "younger_ftq_distance": distance,
                "no_same_generation_commit": no_commit_in_generation,
                "pass": row_pass,
            }
        )

    record_count_matches = (
        len(native_events) == len(oracle_records) == len(oracle_events)
    )
    if not record_count_matches:
        failures.append("exception_record_count_mismatch")
    classification_counts = Counter(row["classification"] for row in rows)
    monitor_exception_count = int(
        ((stats.get("monitor") or {}).get("exception_mark_count") or 0)
    )
    monitor_count_matches_pre_redirect = monitor_exception_count == int(
        classification_counts.get("later_older_redirect_flush", 0)
    )
    if rows and not monitor_count_matches_pre_redirect:
        failures.append("monitor_exception_count_mismatch")
    return {
        "status": "proved" if not failures else "unproved",
        "policy": (
            "each unexpected exception must have matching native/oracle evidence, an "
            "FTQ write and folded PC, an out-of-program address, no same-generation "
            "commit, and either a younger-than-redirect residual within one cycle or "
            "a following older-FTQ redirect within the bounded replay delay"
        ),
        "record_count": len(rows),
        "oracle_record_count": len(oracle_records),
        "timeline_oracle_record_count": len(oracle_events),
        "record_count_matches": record_count_matches,
        "classification_counts": dict(sorted(classification_counts.items())),
        "monitor_exception_mark_count": monitor_exception_count,
        "monitor_count_matches_pre_redirect": monitor_count_matches_pre_redirect,
        "maximum_following_redirect_delta_cycles": max(
            (
                int(row["redirect_delta_cycles"])
                for row in rows
                if row["classification"] == "later_older_redirect_flush"
            ),
            default=None,
        ),
        "failures": failures,
        "records": rows,
    }


def _completion_review(artifact: dict, timeline: dict, case_log_text: str | None) -> dict:
    details = timeline.get("details") or {}
    matches = [] if case_log_text is None else list(_COMPLETION_RE.finditer(case_log_text))
    match = matches[-1] if matches else None
    log_values = (
        None
        if match is None
        else {key: int(value) for key, value in match.groupdict().items()}
    )
    backend = ((artifact.get("stats") or {}).get("backend") or {})
    complete = bool(
        log_values is not None
        and log_values["completion"] == 0
        and log_values["cursor"] == log_values["entries"]
        and int(details.get("golden_cursor", -1))
        == int(details.get("golden_entries", -2))
        == log_values["entries"]
    )
    return {
        "status": "proved" if complete else "unproved",
        "contract": (
            "architectural matched-path completion_pending_work must be zero; "
            "purely speculative backend residue is reported but is not called full drain"
        ),
        "completion_log": log_values,
        "backend_snapshot": {
            key: backend.get(key)
            for key in ("ftq_entries_pending", "pending_resolves", "pending_events")
        },
        "full_backend_drain_claimed": False,
    }


def _scenario_review(timeline: dict) -> dict:
    actions = timeline.get("actions") or []
    starts = _timeline_events(timeline, "scenario.action_start")
    completes = _timeline_events(timeline, "scenario.action_complete")
    timing_plan = timeline.get("timing_plan") or {}
    expected_backend = timing_plan.get("maximum_backpressure_windows")
    try:
        stall_cycles = int(timing_plan.get("uncache_a_stall_cycles", 0))
    except (TypeError, ValueError):
        stall_cycles = -1
    expected_by_kind = {
        "backend_can_accept_low": expected_backend,
        "uncache_a_ready_low": 1 if stall_cycles > 0 else 0,
    }
    action_kind_counts = {
        kind: sum(
            isinstance(action, dict)
            and str(action.get("kind") or "") == kind
            for action in actions
        )
        for kind in expected_by_kind
    }
    unknown_action_kinds = sorted(
        {
            (
                str(action.get("kind") or "missing")
                if isinstance(action, dict)
                else "malformed"
            )
            for action in actions
            if not isinstance(action, dict)
            or str(action.get("kind") or "") not in expected_by_kind
        }
    )
    action_ids = {
        str(action.get("action_id"))
        for action in actions
        if isinstance(action, dict)
    }
    start_ids = {
        str((event.get("payload") or {}).get("action_id")) for event in starts
    }
    complete_ids = {
        str((event.get("payload") or {}).get("action_id")) for event in completes
    }
    try:
        realized = all(
            isinstance(action, dict)
            and
            action.get("status") == "REALIZED"
            and int(action.get("confirmed_low_cycles", -1))
            == int(action.get("duration_cycles", -2))
            for action in actions
        )
    except (TypeError, ValueError):
        realized = False
    try:
        count_matches = bool(
            stall_cycles >= 0
            and not unknown_action_kinds
            and all(
                expected is None or action_kind_counts[kind] == int(expected)
                for kind, expected in expected_by_kind.items()
            )
        )
    except (TypeError, ValueError):
        count_matches = False
    try:
        expected = (
            None
            if expected_backend is None
            else int(expected_backend) + (1 if stall_cycles > 0 else 0)
        )
    except (TypeError, ValueError):
        expected = "invalid"
    passed = bool(
        timeline.get("execution_status") == "REALIZED"
        and timeline.get("dropped_timeline_events") == 0
        and realized
        and count_matches
        and action_ids == start_ids == complete_ids
    )
    return {
        "status": "proved" if passed else "unproved",
        "execution_status": timeline.get("execution_status"),
        "dropped_timeline_events": timeline.get("dropped_timeline_events"),
        "action_count": len(actions),
        "expected_action_count": expected,
        "action_kind_counts": action_kind_counts,
        "expected_action_kind_counts": expected_by_kind,
        "unknown_action_kinds": unknown_action_kinds,
        "all_actions_realized": realized,
        "start_complete_ids_match": action_ids == start_ids == complete_ids,
        "event_counts": timeline.get("event_counts") or {},
    }


def _sha256_path(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _program_map_path_metadata_equivalence(
    *,
    expected_path: Path,
    observed_path: Path,
    source_sha256: str,
    binary_sha256: str,
) -> dict:
    """Allow a relocated ProgramMap only when its executable meaning is identical.

    The pipeline writes a replay-local ``source_path`` into ProgramMap.  That
    path is location metadata, not program content.  Any other JSON delta, an
    unbound source copy, or a different binary remains a qualification failure.
    """

    evidence = {
        "expected_program_map_path": str(expected_path),
        "observed_program_map_path": str(observed_path),
        "ignored_metadata_fields": sorted(_PROGRAM_MAP_PATH_METADATA_FIELDS),
        "source_sha256": str(source_sha256),
        "binary_sha256": str(binary_sha256),
    }
    if not expected_path.is_file() or not observed_path.is_file():
        evidence.update({"pass": False, "reason": "program_map_file_missing"})
        return evidence
    try:
        expected = json.loads(expected_path.read_text(encoding="utf-8"))
        observed = json.loads(observed_path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as exc:
        evidence.update({"pass": False, "reason": f"program_map_json_invalid:{type(exc).__name__}"})
        return evidence
    if not isinstance(expected, dict) or not isinstance(observed, dict):
        evidence.update({"pass": False, "reason": "program_map_not_object"})
        return evidence

    source_paths = [
        Path(str(payload.get("source_path") or ""))
        for payload in (expected, observed)
    ]
    source_paths_bound = bool(
        source_sha256
        and all(path.is_file() and _sha256_path(path) == source_sha256 for path in source_paths)
    )
    binary_bound = bool(
        binary_sha256
        and all(
            payload.get("binary_text_verified") is True
            and str(payload.get("binary_sha256") or "") == binary_sha256
            for payload in (expected, observed)
        )
    )
    expected_semantic = {
        key: value
        for key, value in expected.items()
        if key not in _PROGRAM_MAP_PATH_METADATA_FIELDS
    }
    observed_semantic = {
        key: value
        for key, value in observed.items()
        if key not in _PROGRAM_MAP_PATH_METADATA_FIELDS
    }
    semantic_equal = expected_semantic == observed_semantic
    passed = source_paths_bound and binary_bound and semantic_equal
    evidence.update(
        {
            "pass": passed,
            "source_paths_bound": source_paths_bound,
            "binary_bound": binary_bound,
            "semantic_json_equal": semantic_equal,
            "reason": "path_metadata_only" if passed else "program_map_semantics_or_binding_changed",
        }
    )
    return evidence


def _input_integrity_review(
    artifact: dict,
    *,
    program_map_path: Path,
    campaign_path: Path,
    case_name: str,
    inherited_input_audit_path: Path | None = None,
) -> dict:
    campaign = json.loads(campaign_path.read_text(encoding="utf-8"))
    matching = [
        record
        for record in (campaign.get("cases") or [])
        if record.get("name") == case_name
    ]
    if len(matching) != 1:
        return {
            "status": "unproved",
            "failures": [f"campaign_case_matches={len(matching)}"],
            "files": {},
        }
    expected = dict(matching[0])
    inherited_input_binding = None
    binding_failures: list[str] = []
    if inherited_input_audit_path is not None:
        binding = json.loads(
            inherited_input_audit_path.read_text(encoding="utf-8")
        )
        qualification = binding.get("qualification") or {}
        candidate_campaign = (binding.get("inputs") or {}).get(
            "candidate_campaign"
        ) or {}
        campaign_sha = _sha256_path(campaign_path)
        binding_matches = [
            record
            for record in (binding.get("cases") or [])
            if record.get("name") == case_name
        ]
        if not (
            qualification.get("pass") is True
            and qualification.get("status") == "qualified"
            and qualification.get("input_qualification") is True
            and qualification.get("nemu_replay_required") is False
            and qualification.get("dut_replay_completed") is False
            and qualification.get("coverage_hit_claimed") is False
        ):
            binding_failures.append("inherited_input_audit_not_qualified")
        recorded_campaign_path = Path(
            str(candidate_campaign.get("path") or "/nonexistent")
        )
        recorded_campaign_sha = (
            _sha256_path(recorded_campaign_path)
            if recorded_campaign_path.is_file()
            else ""
        )
        if not recorded_campaign_sha:
            binding_failures.append("inherited_input_audit_recorded_campaign_missing")
        if not (
            str(candidate_campaign.get("sha256") or "")
            == campaign_sha
            == recorded_campaign_sha
        ):
            binding_failures.append("inherited_input_audit_campaign_hash_mismatch")
        if len(binding_matches) != 1:
            binding_failures.append(
                f"inherited_input_audit_case_matches={len(binding_matches)}"
            )
        else:
            binding_case = binding_matches[0]
            if binding_case.get("pass") is not True or binding_case.get("reasons"):
                binding_failures.append("inherited_input_audit_case_not_qualified")
            inherited = binding_case.get("inherited_nemu_inputs") or {}
            for key, campaign_prefix in (
                ("binary", "bin"),
                ("trace", "trace"),
                ("program_map", "program_map"),
            ):
                evidence = inherited.get(key) or {}
                path = Path(str(evidence.get("path") or "/nonexistent"))
                recorded = str(evidence.get("recorded_sha256") or "")
                actual = _sha256_path(path) if path.is_file() else ""
                if not (
                    evidence.get("pass") is True
                    and recorded
                    and str(evidence.get("actual_sha256") or "") == recorded
                    and actual == recorded
                ):
                    binding_failures.append(f"inherited_{key}_binding_invalid")
                    continue
                expected[f"{campaign_prefix}_path"] = str(path)
                expected[f"{campaign_prefix}_sha256"] = recorded
        inherited_input_binding = {
            "path": str(inherited_input_audit_path.resolve()),
            "sha256": _sha256_path(inherited_input_audit_path),
            "candidate_campaign_recorded_path": str(
                candidate_campaign.get("path") or ""
            ),
            "candidate_campaign_recorded_sha256": str(
                candidate_campaign.get("sha256") or ""
            ),
            "candidate_campaign_recorded_actual_sha256": recorded_campaign_sha,
            "failures": binding_failures,
            "status": "proved" if not binding_failures else "unproved",
        }
    run = artifact.get("run") or {}
    specifications = {
        "source": (
            Path(str(run.get("asm_path") or expected.get("source_path") or "")),
            str(expected.get("source_sha256") or ""),
            str(run.get("asm_sha256") or ""),
        ),
        "binary": (
            Path(str(run.get("bin_path") or expected.get("bin_path") or "")),
            str(expected.get("bin_sha256") or ""),
            str(run.get("bin_sha256") or ""),
        ),
        "trace": (
            Path(str(run.get("trace_path") or expected.get("trace_path") or "")),
            str(expected.get("trace_sha256") or ""),
            str(run.get("trace_sha256") or ""),
        ),
        "scenario_spec": (
            Path(str(expected.get("scenario_spec_path") or "")),
            str(expected.get("scenario_spec_sha256") or ""),
            str(expected.get("scenario_spec_sha256") or ""),
        ),
        "program_map": (
            program_map_path,
            str(expected.get("program_map_sha256") or ""),
            str(expected.get("program_map_sha256") or ""),
        ),
    }
    files: dict[str, dict] = {}
    failures: list[str] = list(binding_failures)
    for label, (path, expected_hash, recorded_hash) in specifications.items():
        exists = path.is_file()
        actual_hash = _sha256_path(path) if exists else ""
        byte_exact = bool(
            exists
            and expected_hash
            and recorded_hash
            and actual_hash == expected_hash == recorded_hash
        )
        path_metadata_equivalence = None
        passed = byte_exact
        if label == "program_map" and not passed and exists and expected_hash:
            expected_map_path = Path(str(expected.get("program_map_path") or ""))
            path_metadata_equivalence = _program_map_path_metadata_equivalence(
                expected_path=expected_map_path,
                observed_path=path,
                source_sha256=str(expected.get("source_sha256") or ""),
                binary_sha256=str(expected.get("bin_sha256") or ""),
            )
            passed = bool(path_metadata_equivalence["pass"])
        if not passed:
            failures.append(label)
        files[label] = {
            "path": str(path.resolve()) if exists else str(path),
            "expected_sha256": expected_hash,
            "recorded_sha256": recorded_hash,
            "actual_sha256": actual_hash,
            "byte_exact": byte_exact,
            "path_metadata_equivalence": path_metadata_equivalence,
            "pass": passed,
        }
    return {
        "status": "proved" if not failures else "unproved",
        "campaign": {
            "path": str(campaign_path.resolve()),
            "sha256": _sha256_path(campaign_path),
            "case_name": case_name,
        },
        "failures": failures,
        "files": files,
        "inherited_input_binding": inherited_input_binding,
    }


def _replay_provenance_review(
    *,
    manifest_before: Path,
    manifest_after: Path,
    environment_before: Path,
    environment_after: Path,
    python_hashes: Path,
) -> dict:
    manifest_match = manifest_before.read_bytes() == manifest_after.read_bytes()
    environment_match = environment_before.read_bytes() == environment_after.read_bytes()
    audit_only_paths = _post_replay_audit_only_paths()
    checked = 0
    missing: list[str] = []
    mismatches: list[str] = []
    for line in python_hashes.read_text(encoding="utf-8").splitlines():
        expected_hash, separator, raw_path = line.partition("  ")
        if not separator:
            continue
        path = Path(raw_path)
        checked += 1
        if not path.is_file():
            missing.append(str(path))
        elif _sha256_path(path) != expected_hash:
            mismatches.append(str(path.resolve()))
    execution_relevant_mismatches = sorted(set(mismatches) - audit_only_paths)
    passed = bool(
        manifest_match
        and environment_match
        and not missing
        and not execution_relevant_mismatches
    )
    return {
        "status": "proved" if passed else "unproved",
        "policy": (
            "build and worktree snapshots must match before/after; Python replay "
            "inputs are rechecked, with only post-replay audit-tool edits allowed"
        ),
        "build_manifest_before_after_identical": manifest_match,
        "environment_diff_before_after_identical": environment_match,
        "python_inputs_checked": checked,
        "python_inputs_missing": missing,
        "python_input_mismatches": sorted(mismatches),
        "post_replay_audit_only_deltas": sorted(set(mismatches) & audit_only_paths),
        "execution_relevant_python_deltas": execution_relevant_mismatches,
        "evidence_files": {
            "manifest_before": {
                "path": str(manifest_before.resolve()),
                "sha256": _sha256_path(manifest_before),
            },
            "manifest_after": {
                "path": str(manifest_after.resolve()),
                "sha256": _sha256_path(manifest_after),
            },
            "environment_before": {
                "path": str(environment_before.resolve()),
                "sha256": _sha256_path(environment_before),
            },
            "environment_after": {
                "path": str(environment_after.resolve()),
                "sha256": _sha256_path(environment_after),
            },
            "python_hashes": {
                "path": str(python_hashes.resolve()),
                "sha256": _sha256_path(python_hashes),
            },
        },
    }


def audit_translation_replay(
    artifact: dict,
    timeline: dict,
    manifest: dict,
    program_map: dict | None = None,
    case_log_text: str | None = None,
    input_integrity_review: dict | None = None,
    replay_provenance_review: dict | None = None,
) -> dict:
    stats = artifact.get("stats") or {}
    oracle = stats.get("translation_oracle") or {}
    records = oracle.get("records") or []
    exceptions = [r for r in records if r.get("kind") == "speculative_cfvec_exception"]
    provenance = artifact.get("provenance") or {}
    expected = {
        "dut_source_sha": manifest.get("dut_source_sha"),
        **{key: (manifest.get("artifacts") or {}).get(key) for key in (
            "dut_build_sha256", "generated_rtl_sha256", "signal_contract_sha256",
        )},
    }
    differences = {
        key: {"recorded": provenance.get(key), "current": value}
        for key, value in expected.items()
        if not value or provenance.get(key) != value
    }
    reasons = []
    if differences:
        reasons.append("current_dut_baseline_not_established")
    if not oracle or not isinstance(oracle.get("records"), list):
        reasons.append("translation_oracle_evidence_missing")
    if oracle.get("error_count") != 0 or oracle.get("errors"):
        reasons.append("translation_oracle_not_error_free")
    exception_review = _exception_causal_review(artifact, timeline)
    if exceptions and exception_review["status"] != "proved":
        reasons.append("speculative_exceptions_require_causal_review")
    if artifact.get("checker", {}).get("status") != "pass":
        reasons.append("checker_not_pass")
    if artifact.get("outcome", {}).get("status") != "passed":
        reasons.append("run_not_passed")
    if timeline.get("execution_status") != "REALIZED":
        reasons.append("scenario_not_realized")
    if timeline.get("dropped_timeline_events") != 0:
        reasons.append("timeline_missing_or_dropped_events")
    details = timeline.get("details") or {}
    if not details.get("golden_entries") or details.get("golden_cursor") != details.get("golden_entries"):
        reasons.append("golden_completion_not_established")
    completion_review = _completion_review(artifact, timeline, case_log_text)
    if case_log_text is not None and completion_review["status"] != "proved":
        reasons.append("architectural_completion_not_proved")
    scenario_review = _scenario_review(timeline)
    if timeline.get("timeline") is not None and scenario_review["status"] != "proved":
        reasons.append("scenario_action_evidence_incomplete")
    mapping = timeline.get("program_address_mapping") or {}
    mapping_reasons: list[str] = []
    if program_map is not None:
        if program_map.get("binary_text_verified") is not True:
            mapping_reasons.append("program_map_binary_text_not_verified")
        run = artifact.get("run") or {}
        binary_hashes = {
            str(program_map.get("binary_sha256") or ""),
            str(timeline.get("program_map_binary_sha256") or ""),
            str(run.get("bin_sha256") or ""),
        }
        if "" in binary_hashes or len(binary_hashes) != 1:
            mapping_reasons.append("program_map_binary_hash_mismatch")
        if mapping.get("valid") is not True or not mapping.get("pages"):
            mapping_reasons.append("timeline_program_mapping_invalid")
        if mapping_reasons:
            reasons.append("program_mapping_not_qualified")
    if input_integrity_review is not None and input_integrity_review.get("status") != "proved":
        reasons.append("retained_input_integrity_not_proved")
    if replay_provenance_review is not None and replay_provenance_review.get("status") != "proved":
        reasons.append("replay_provenance_not_stable")
    backend = stats.get("backend") or {}
    pending = {
        key: backend.get(key)
        for key in ("ftq_entries_pending", "pending_resolves", "pending_events")
    }
    coverage_targets = artifact.get("coverage_targets") or {}
    target_bin_ids = list(coverage_targets.get("bin_ids") or [])
    replay_qualification = not reasons and program_map is not None and case_log_text is not None
    return {
        "schema_version": 2,
        "policy": (
            "Replay qualification, DUT PASS, and coverage HIT eligibility are "
            "separate conclusions; this sidecar never rewrites source artifacts."
        ),
        "testcase_name": artifact.get("testcase_name"),
        "scenario_id": timeline.get("scenario_id"),
        "review_status": "requires_review" if reasons else "no_flags_in_selected_checks",
        "review_reasons": reasons,
        "replay_artifact_qualification": {
            "pass": replay_qualification,
            "status": "qualified" if replay_qualification else "not_qualified",
            "reasons": reasons + (
                []
                if program_map is not None
                else ["program_map_evidence_not_supplied"]
            ) + (
                []
                if case_log_text is not None
                else ["completion_log_evidence_not_supplied"]
            ),
        },
        "dut_pass": {
            "pass": (
                artifact.get("checker", {}).get("status") == "pass"
                and artifact.get("checker", {}).get("error_count") == 0
                and artifact.get("outcome", {}).get("status") == "passed"
                and int(((stats.get("monitor") or {}).get("error_count") or 0)) == 0
                and oracle.get("error_count") == 0
            ),
            "checker": artifact.get("checker") or {},
            "monitor_error_count": (stats.get("monitor") or {}).get("error_count"),
            "translation_oracle_error_count": oracle.get("error_count"),
        },
        "coverage_hit_eligibility": {
            "eligible": False,
            "reason": (
                "diagnostic_run_has_no_declared_target_bins"
                if not target_bin_ids
                else "sidecar_does_not_grant_coverage_hit_eligibility"
            ),
            "declared_target_bin_ids": target_bin_ids,
            "raw_observed_hit_count": len(artifact.get("hits") or {}),
        },
        "baseline_differences": differences,
        "recorded_compatibility_signature": provenance.get("compatibility_signature"),
        "oracle_record_counts": dict(sorted(Counter(r.get("kind", "unknown") for r in records).items())),
        "speculative_exception_causal_review": exception_review,
        "completion_review": completion_review,
        "scenario_review": scenario_review,
        "program_mapping_review": {
            "status": (
                "not_supplied"
                if program_map is None
                else ("proved" if not mapping_reasons else "unproved")
            ),
            "reasons": mapping_reasons,
            "timeline_mapping": mapping,
            "program_map_summary": (
                None if program_map is None else program_map.get("summary")
            ),
        },
        "input_integrity_review": input_integrity_review,
        "replay_provenance_review": replay_provenance_review,
        "backend_pending_snapshot": pending,
        "golden_entries": details.get("golden_entries"),
        "golden_cursor": details.get("golden_cursor"),
        "dropped_timeline_events": timeline.get("dropped_timeline_events"),
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--artifact", required=True, type=Path)
    parser.add_argument("--timeline", required=True, type=Path)
    parser.add_argument("--current-manifest", required=True, type=Path)
    parser.add_argument("--program-map", type=Path)
    parser.add_argument("--case-log", type=Path)
    parser.add_argument("--campaign", type=Path)
    parser.add_argument("--case-name")
    parser.add_argument("--inherited-input-audit", type=Path)
    parser.add_argument("--manifest-before", type=Path)
    parser.add_argument("--environment-before", type=Path)
    parser.add_argument("--environment-after", type=Path)
    parser.add_argument("--python-input-hashes", type=Path)
    parser.add_argument("--output", required=True, type=Path)
    args = parser.parse_args()
    inputs = {}
    payloads = []
    source_paths = [
        ("artifact", args.artifact),
        ("timeline", args.timeline),
        ("current_manifest", args.current_manifest),
    ]
    if args.program_map is not None:
        source_paths.append(("program_map", args.program_map))
    for label, path in source_paths:
        raw = path.read_bytes()
        inputs[label] = {"path": str(path.resolve()), "sha256": hashlib.sha256(raw).hexdigest()}
        payloads.append(json.loads(raw))
    case_log_text = None
    if args.case_log is not None:
        raw_log = args.case_log.read_bytes()
        inputs["case_log"] = {
            "path": str(args.case_log.resolve()),
            "sha256": hashlib.sha256(raw_log).hexdigest(),
        }
        case_log_text = raw_log.decode("utf-8", errors="replace")
    input_review = None
    if args.campaign is not None or args.case_name is not None:
        if args.campaign is None or not args.case_name or args.program_map is None:
            parser.error("--campaign, --case-name, and --program-map must be supplied together")
        input_review = _input_integrity_review(
            payloads[0],
            program_map_path=args.program_map,
            campaign_path=args.campaign,
            case_name=args.case_name,
            inherited_input_audit_path=args.inherited_input_audit,
        )
    elif args.inherited_input_audit is not None:
        parser.error(
            "--inherited-input-audit requires --campaign, --case-name, and --program-map"
        )
    provenance_paths = (
        args.manifest_before,
        args.environment_before,
        args.environment_after,
        args.python_input_hashes,
    )
    provenance_review = None
    if any(path is not None for path in provenance_paths):
        if any(path is None for path in provenance_paths):
            parser.error(
                "--manifest-before, --environment-before, --environment-after, and "
                "--python-input-hashes must be supplied together"
            )
        provenance_review = _replay_provenance_review(
            manifest_before=args.manifest_before,
            manifest_after=args.current_manifest,
            environment_before=args.environment_before,
            environment_after=args.environment_after,
            python_hashes=args.python_input_hashes,
        )
    report = audit_translation_replay(
        *payloads,
        case_log_text=case_log_text,
        input_integrity_review=input_review,
        replay_provenance_review=provenance_review,
    )
    report["inputs"] = inputs
    # Preserve existing reports and all source artifacts, even on repeated invocation.
    with args.output.open("x", encoding="utf-8") as stream:
        json.dump(report, stream, indent=2, sort_keys=True)
        stream.write("\n")
    print(json.dumps({
        "output": str(args.output.resolve()),
        "review_reasons": report["review_reasons"],
        "replay_qualified": report["replay_artifact_qualification"]["pass"],
        "coverage_hit_eligible": report["coverage_hit_eligibility"]["eligible"],
        "speculative_exceptions": report["speculative_exception_causal_review"]["record_count"],
    }))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
