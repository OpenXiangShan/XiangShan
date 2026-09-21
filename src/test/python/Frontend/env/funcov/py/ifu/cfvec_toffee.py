from __future__ import annotations

from .cfvec_funcov import (
    CFVEC_SAMPLER_BIN_KEYS,
    evaluate_cfvec_coverage,
    reset_ifu_coverage_state,
)
from ...native_toffee import NativeDomainToffeeCoverage


CFVEC_COVERPOINTS = {
    "ifu_aligned_slot": "coherence",
    "ifu_boundary_event": "instr_start_boundary",
    "ifu_cacheable_boundary": "sequence_shape",
    "ifu_cacheable_cfi_flow": "next_pc",
    "ifu_cacheable_compact": "output_shape",
    "ifu_cacheable_delivery": "stream_shape",
    "ifu_cacheable_expander": "input_type",
    "ifu_cacheable_main_path": "delivery",
    "ifu_cfi_decode_type": "instr_opcode_class",
    "ifu_data_slice": "source_selection",
    "ifu_fetch_block_position": "pc[5:1]_block_pos",
    "ifu_ibuffer_alignment": "pointer_alignment",
    "ifu_ibuffer_backpressure": "hold_sequence",
    "ifu_ibuffer_output": "field_observation",
    "ifu_instr_boundary_alignment": "output_slot",
    "ifu_instr_boundary_expansion": "width_preservation",
    "ifu_instr_boundary_half": "cross_block_state",
    "ifu_instr_boundary_source": "high_half_entry",
    "ifu_instr_boundary_v3": "cross_block_delivery",
    "ifu_instr_compact": "output_layout",
    "ifu_instr_compact_rank": "rank_mapping",
    "ifu_instr_compact_source": "source_mapping",
    "ifu_instr_end_offset": "instruction_width",
    "ifu_instr_size_type": "rvc_rvi_mix_seen",
    "ifu_invalid_taken_exception": "stimulus_cross",
    "ifu_pc_step_type": "adjacent_cfvec_pc_step",
    "ifu_predchecker_v3_fault": "fault_type",
    "ifu_predchecker_v3_range": "first_fault_range",
    "ifu_predchecker_v3_redirect": "registered_redirect",
    "ifu_predecode": "decode_coherence",
    "ifu_rvc_exception": "exception_priority",
    "ifu_rvc_expander": "instruction_result",
    "ifu_writeback": "ftq_update",
    "uncache_page_boundary": "tail_instruction_kind",
}


class IfuCfvecToffeeCoverage(NativeDomainToffeeCoverage):
    """Native Toffee CovGroup sampling for IFU cfVec/compact."""

    def __init__(self, runtime, *, sink=None, audit_recorder=None) -> None:
        super().__init__(
            runtime,
            keys=CFVEC_SAMPLER_BIN_KEYS,
            coverpoints=CFVEC_COVERPOINTS,
            evaluate=evaluate_cfvec_coverage,
            reset=reset_ifu_coverage_state,
            sink=sink,
            audit_recorder=audit_recorder,
        )


__all__ = ["CFVEC_COVERPOINTS", "IfuCfvecToffeeCoverage"]
