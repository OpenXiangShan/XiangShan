#!/usr/bin/env python3
"""Generate C++ port-policy helpers from the checked MemBlock manifest."""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path
from typing import Any


class CppGenerationError(RuntimeError):
    pass


def quiescent_output(port: dict[str, Any]) -> bool:
    if port["direction"] != "output" or port["role"] != "valid":
        return False
    if port["protocol"] == "tilelink":
        return True
    return port["name"].startswith("io_mem_to_ooo_writeback")


def matching_lanes(port_names: set[str], pattern: str) -> list[int]:
    regex = re.compile(pattern)
    return sorted(
        {
            int(match.group(1))
            for name in port_names
            if (match := regex.fullmatch(name)) is not None
        }
    )


def render_lane_adapters(manifest: dict[str, Any]) -> list[str]:
    """Render typed adapters only when the complete MemBlock interfaces exist."""
    port_names = {port["name"] for port in manifest["ports"]}
    lsq_lanes = matching_lanes(
        port_names, r"io_ooo_to_mem_enqLsq_req_([0-9]+)_valid"
    )
    load_lanes = matching_lanes(
        port_names, r"io_ooo_to_mem_issueLda_([0-9]+)_valid"
    )
    wb_lanes = matching_lanes(
        port_names, r"io_mem_to_ooo_writebackLda_([0-9]+)_valid"
    )
    sta_lanes = matching_lanes(
        port_names, r"io_ooo_to_mem_issueSta_([0-9]+)_valid"
    )
    std_lanes = matching_lanes(
        port_names, r"io_ooo_to_mem_issueStd_([0-9]+)_valid"
    )
    sta_wb_lanes = matching_lanes(
        port_names, r"io_mem_to_ooo_writebackSta_([0-9]+)_valid"
    )
    std_wb_lanes = matching_lanes(
        port_names, r"io_mem_to_ooo_writebackStd_([0-9]+)_valid"
    )
    if not any((lsq_lanes, load_lanes, wb_lanes, sta_lanes, std_lanes)):
        return []

    lines = [
        f"inline constexpr unsigned kLsqEnqueueLanes = {len(lsq_lanes)};",
        "",
        "struct LsqEnqueue {",
        "    std::uint8_t need_alloc = 0;",
        "    std::uint32_t exception_mask = 0;",
        "    std::uint8_t trigger = 15;",
        "    bool flush_pipe = false;",
        "    std::uint64_t fu_type = 0;",
        "    std::uint16_t fu_op_type = 0;",
        "    std::uint8_t uop_idx = 0;",
        "    bool last_uop = true;",
        "    bool rob_flag = false;",
        "    std::uint8_t rob_value = 0;",
        "    bool lq_flag = false;",
        "    std::uint8_t lq_value = 0;",
        "    bool sq_flag = false;",
        "    std::uint8_t sq_value = 0;",
        "    std::uint8_t num_ls_elem = 1;",
        "};",
        "",
        "inline void clear_lsq_enqueue_valids(UTMemBlock &dut)",
        "{",
    ]
    for lane in lsq_lanes:
        lines.append(
            f"    dut.io_ooo_to_mem_enqLsq_req_{lane}_valid.ImmSet(std::uint64_t{{0}});"
        )
        lines.append(
            f"    dut.io_ooo_to_mem_enqLsq_needAlloc_{lane}.ImmSet(std::uint64_t{{0}});"
        )
    lines.extend(["}", "", "inline void drive_lsq_enqueue(",
                  "    UTMemBlock &dut, unsigned lane, const LsqEnqueue &item)", "{",
                  "    switch (lane) {"])
    for lane in lsq_lanes:
        prefix = f"io_ooo_to_mem_enqLsq_req_{lane}"
        lines.append(f"    case {lane}:")
        lines.append(
            f"        dut.io_ooo_to_mem_enqLsq_needAlloc_{lane}.ImmSet(item.need_alloc);"
        )
        for bit in range(24):
            lines.append(
                f"        dut.{prefix}_bits_exceptionVec_{bit}.ImmSet((item.exception_mask >> {bit}) & 1U);"
            )
        assignments = {
            "trigger": "item.trigger",
            "fuType": "item.fu_type",
            "fuOpType": "item.fu_op_type",
            "flushPipe": "item.flush_pipe",
            "uopIdx": "item.uop_idx",
            "lastUop": "item.last_uop",
            "robIdx_flag": "item.rob_flag",
            "robIdx_value": "item.rob_value",
            "lqIdx_flag": "item.lq_flag",
            "lqIdx_value": "item.lq_value",
            "sqIdx_flag": "item.sq_flag",
            "sqIdx_value": "item.sq_value",
            "numLsElem": "item.num_ls_elem",
        }
        for suffix, value in assignments.items():
            lines.append(f"        dut.{prefix}_bits_{suffix}.ImmSet({value});")
        lines.append(f"        dut.{prefix}_valid.ImmSet(std::uint64_t{{1}});")
        lines.extend(["        return;"])
    lines.extend(
        [
            "    default:",
            '        throw std::out_of_range("invalid LSQ enqueue lane");',
            "    }",
            "}",
            "",
            "struct ScalarLoadIssue {",
            "    std::uint64_t pc = 0;",
            "    bool predecode_rvc = false;",
            "    std::uint64_t ftq_ptr = 0;",
            "    std::uint8_t ftq_offset = 0;",
            "    std::uint16_t fu_op_type = 0;",
            "    bool rf_wen = true;",
            "    bool fp_wen = false;",
            "    std::uint32_t imm = 0;",
            "    std::uint8_t pdest = 0;",
            "    bool rob_flag = false;",
            "    std::uint8_t rob_value = 0;",
            "    bool lq_flag = false;",
            "    std::uint8_t lq_value = 0;",
            "    bool sq_flag = false;",
            "    std::uint8_t sq_value = 0;",
            "    bool store_set_hit = false;",
            "    bool wait_for_rob_flag = false;",
            "    std::uint8_t wait_for_rob_value = 0;",
            "    bool load_wait_bit = false;",
            "    bool load_wait_strict = false;",
            "    std::uint64_t src = 0;",
            "};",
            "",
            "inline void clear_scalar_load_issue_valids(UTMemBlock &dut)",
            "{",
        ]
    )
    for lane in load_lanes:
        lines.append(
            f"    dut.io_ooo_to_mem_issueLda_{lane}_valid.ImmSet(std::uint64_t{{0}});"
        )
    lines.extend(["}", "", "inline void clear_scalar_load_issue_valid(UTMemBlock &dut, unsigned lane)",
                  "{", "    switch (lane) {"])
    for lane in load_lanes:
        lines.extend(
            [
                f"    case {lane}:",
                f"        dut.io_ooo_to_mem_issueLda_{lane}_valid.ImmSet(std::uint64_t{{0}});",
                "        return;",
            ]
        )
    lines.extend(["    default:",
                  '        throw std::out_of_range("invalid scalar load issue lane");',
                  "    }", "}", "",
                  "inline bool scalar_load_issue_ready(UTMemBlock &dut, unsigned lane)",
                  "{", "    switch (lane) {"])
    for lane in load_lanes:
        lines.append(
            f"    case {lane}: return dut.io_ooo_to_mem_issueLda_{lane}_ready.B();"
        )
    lines.extend(
        [
            "    default:",
            '        throw std::out_of_range("invalid scalar load issue lane");',
            "    }",
            "}",
            "",
            "inline void drive_scalar_load_issue(",
            "    UTMemBlock &dut, unsigned lane, const ScalarLoadIssue &item)",
            "{",
            "    switch (lane) {",
        ]
    )
    issue_assignments = {
        "uop_pc": "item.pc",
        "uop_preDecodeInfo_isRVC": "item.predecode_rvc",
        "uop_ftqPtr_flag": "std::uint64_t{0}",
        "uop_ftqPtr_value": "item.ftq_ptr",
        "uop_ftqOffset": "item.ftq_offset",
        "uop_fuOpType": "item.fu_op_type",
        "uop_rfWen": "item.rf_wen",
        "uop_fpWen": "item.fp_wen",
        "uop_imm": "item.imm",
        "uop_pdest": "item.pdest",
        "uop_robIdx_flag": "item.rob_flag",
        "uop_robIdx_value": "item.rob_value",
        "uop_storeSetHit": "item.store_set_hit",
        "uop_waitForRobIdx_flag": "item.wait_for_rob_flag",
        "uop_waitForRobIdx_value": "item.wait_for_rob_value",
        "uop_loadWaitBit": "item.load_wait_bit",
        "uop_loadWaitStrict": "item.load_wait_strict",
        "uop_lqIdx_flag": "item.lq_flag",
        "uop_lqIdx_value": "item.lq_value",
        "uop_sqIdx_flag": "item.sq_flag",
        "uop_sqIdx_value": "item.sq_value",
        "src_0": "item.src",
    }
    for lane in load_lanes:
        prefix = f"io_ooo_to_mem_issueLda_{lane}_bits_"
        lines.append(f"    case {lane}:")
        for suffix, value in issue_assignments.items():
            lines.append(f"        dut.{prefix}{suffix}.ImmSet({value});")
        lines.append(
            f"        dut.io_ooo_to_mem_issueLda_{lane}_valid.ImmSet(std::uint64_t{{1}});"
        )
        lines.append("        return;")
    lines.extend(
        [
            "    default:",
            '        throw std::out_of_range("invalid scalar load issue lane");',
            "    }",
            "}",
            "",
            "struct ScalarLoadWriteback {",
            "    bool valid = false;",
            "    std::uint32_t exception_mask = 0;",
            "    bool rf_wen = false;",
            "    bool fp_wen = false;",
            "    bool flush_pipe = false;",
            "    std::uint8_t trigger = 15;",
            "    bool is_from_load_unit = false;",
            "    bool debug_is_mmio = false;",
            "    bool debug_is_ncio = false;",
            "    bool debug_is_perf_cnt = false;",
            "    std::uint8_t pdest = 0;",
            "    bool rob_flag = false;",
            "    std::uint8_t rob_value = 0;",
            "    bool replay = false;",
            "    std::uint64_t data = 0;",
            "};",
            "",
            "inline ScalarLoadWriteback sample_scalar_load_writeback(",
            "    UTMemBlock &dut, unsigned lane)",
            "{",
            "    ScalarLoadWriteback result;",
            "    switch (lane) {",
        ]
    )
    for lane in wb_lanes:
        prefix = f"io_mem_to_ooo_writebackLda_{lane}"
        lines.append(f"    case {lane}:")
        lines.append(f"        result.valid = dut.{prefix}_valid.B();")
        for bit in range(24):
            name = f"{prefix}_bits_uop_exceptionVec_{bit}"
            if name in port_names:
                lines.append(
                    f"        result.exception_mask |= dut.{name}.B() ? (1U << {bit}) : 0U;"
                )
        sample_assignments = {
            "rf_wen": "uop_rfWen",
            "fp_wen": "uop_fpWen",
            "flush_pipe": "uop_flushPipe",
            "trigger": "uop_trigger",
            "pdest": "uop_pdest",
            "rob_flag": "uop_robIdx_flag",
            "rob_value": "uop_robIdx_value",
            "replay": "uop_replayInst",
            "data": "data",
        }
        bool_fields = {"rf_wen", "fp_wen", "flush_pipe", "rob_flag", "replay"}
        for member, suffix in sample_assignments.items():
            name = f"{prefix}_bits_{suffix}"
            if name not in port_names:
                continue
            method = "B()" if member in bool_fields else "U()"
            lines.append(f"        result.{member} = dut.{name}.{method};")
        for member, suffix in (
            ("is_from_load_unit", "isFromLoadUnit"),
            ("debug_is_mmio", "debug_isMMIO"),
            ("debug_is_ncio", "debug_isNCIO"),
            ("debug_is_perf_cnt", "debug_isPerfCnt"),
        ):
            name = f"{prefix}_bits_{suffix}"
            if name in port_names:
                lines.append(f"        result.{member} = dut.{name}.B();")
        lines.append("        return result;")
    lines.extend(
        [
            "    default:",
            '        throw std::out_of_range("invalid scalar load writeback lane");',
            "    }",
            "}",
            "",
        ]
    )

    lines.extend(
        [
            "struct ScalarStoreIssue {",
            "    std::uint64_t fu_type = 0;",
            "    std::uint16_t fu_op_type = 0;",
            "    std::uint32_t imm = 0;",
            "    std::uint8_t pdest = 0;",
            "    bool rf_wen = false;",
            "    bool rob_flag = false;",
            "    std::uint8_t rob_value = 0;",
            "    bool sq_flag = false;",
            "    std::uint8_t sq_value = 0;",
            "    std::uint64_t src = 0;",
            "};",
            "",
            "inline void clear_scalar_store_issue_valids(UTMemBlock &dut)",
            "{",
        ]
    )
    for lane in sta_lanes:
        lines.append(
            f"    dut.io_ooo_to_mem_issueSta_{lane}_valid.ImmSet(std::uint64_t{{0}});"
        )
    for lane in std_lanes:
        lines.append(
            f"    dut.io_ooo_to_mem_issueStd_{lane}_valid.ImmSet(std::uint64_t{{0}});"
        )
    lines.extend(
        [
            "}",
            "",
            "inline bool scalar_store_address_ready(UTMemBlock &dut, unsigned lane)",
            "{",
            "    switch (lane) {",
        ]
    )
    for lane in sta_lanes:
        lines.append(
            f"    case {lane}: return dut.io_ooo_to_mem_issueSta_{lane}_ready.B();"
        )
    lines.extend(
        [
            "    default:",
            '        throw std::out_of_range("invalid scalar store-address lane");',
            "    }",
            "}",
            "",
            "inline bool scalar_store_data_ready(UTMemBlock &dut, unsigned lane)",
            "{",
            "    switch (lane) {",
        ]
    )
    for lane in std_lanes:
        lines.append(
            f"    case {lane}: return dut.io_ooo_to_mem_issueStd_{lane}_ready.B();"
        )
    lines.extend(
        [
            "    default:",
            '        throw std::out_of_range("invalid scalar store-data lane");',
            "    }",
            "}",
            "",
            "inline void drive_scalar_store_address(",
            "    UTMemBlock &dut, unsigned lane, const ScalarStoreIssue &item)",
            "{",
            "    switch (lane) {",
        ]
    )
    sta_assignments = {
        "uop_fuType": "item.fu_type",
        "uop_fuOpType": "item.fu_op_type",
        "uop_rfWen": "item.rf_wen",
        "uop_imm": "item.imm",
        "uop_pdest": "item.pdest",
        "uop_robIdx_flag": "item.rob_flag",
        "uop_robIdx_value": "item.rob_value",
        "uop_sqIdx_flag": "item.sq_flag",
        "uop_sqIdx_value": "item.sq_value",
        "src_0": "item.src",
    }
    for lane in sta_lanes:
        prefix = f"io_ooo_to_mem_issueSta_{lane}_bits_"
        lines.append(f"    case {lane}:")
        for suffix, value in sta_assignments.items():
            lines.append(f"        dut.{prefix}{suffix}.ImmSet({value});")
        lines.append(
            f"        dut.io_ooo_to_mem_issueSta_{lane}_valid.ImmSet(std::uint64_t{{1}});"
        )
        lines.extend(["        return;"])
    lines.extend(
        [
            "    default:",
            '        throw std::out_of_range("invalid scalar store-address lane");',
            "    }",
            "}",
            "",
            "inline void drive_scalar_store_data(",
            "    UTMemBlock &dut, unsigned lane, const ScalarStoreIssue &item)",
            "{",
            "    switch (lane) {",
        ]
    )
    std_assignments = {
        "uop_fuType": "item.fu_type",
        "uop_fuOpType": "item.fu_op_type",
        "uop_robIdx_value": "item.rob_value",
        "uop_sqIdx_flag": "item.sq_flag",
        "uop_sqIdx_value": "item.sq_value",
        "src_0": "item.src",
    }
    for lane in std_lanes:
        prefix = f"io_ooo_to_mem_issueStd_{lane}_bits_"
        lines.append(f"    case {lane}:")
        for suffix, value in std_assignments.items():
            lines.append(f"        dut.{prefix}{suffix}.ImmSet({value});")
        lines.append(
            f"        dut.io_ooo_to_mem_issueStd_{lane}_valid.ImmSet(std::uint64_t{{1}});"
        )
        lines.extend(["        return;"])
    lines.extend(
        [
            "    default:",
            '        throw std::out_of_range("invalid scalar store-data lane");',
            "    }",
            "}",
            "",
            "struct ScalarStoreWriteback {",
            "    bool valid = false;",
            "    std::uint32_t exception_mask = 0;",
            "    std::uint8_t trigger = 15;",
            "    bool flush_pipe = false;",
            "    bool debug_is_mmio = false;",
            "    bool debug_is_ncio = false;",
            "    bool debug_is_perf_cnt = false;",
            "    bool rob_flag = false;",
            "    std::uint8_t rob_value = 0;",
            "};",
            "",
            "inline ScalarStoreWriteback sample_scalar_store_address_writeback(",
            "    UTMemBlock &dut, unsigned lane)",
            "{",
            "    ScalarStoreWriteback result;",
            "    switch (lane) {",
        ]
    )
    for lane in sta_wb_lanes:
        prefix = f"io_mem_to_ooo_writebackSta_{lane}"
        lines.append(f"    case {lane}:")
        lines.append(f"        result.valid = dut.{prefix}_valid.B();")
        for bit in range(24):
            name = f"{prefix}_bits_uop_exceptionVec_{bit}"
            if name in port_names:
                lines.append(
                    f"        result.exception_mask |= dut.{name}.B() ? (1U << {bit}) : 0U;"
                )
        lines.append(
            f"        result.rob_flag = dut.{prefix}_bits_uop_robIdx_flag.B();"
        )
        lines.append(
            f"        result.rob_value = dut.{prefix}_bits_uop_robIdx_value.U();"
        )
        for member, suffix, method in (
            ("trigger", "uop_trigger", "U()"),
            ("flush_pipe", "uop_flushPipe", "B()"),
            ("debug_is_mmio", "debug_isMMIO", "B()"),
            ("debug_is_ncio", "debug_isNCIO", "B()"),
            ("debug_is_perf_cnt", "debug_isPerfCnt", "B()"),
        ):
            name = f"{prefix}_bits_{suffix}"
            if name in port_names:
                lines.append(f"        result.{member} = dut.{name}.{method};")
        lines.extend(["        return result;"])
    lines.extend(
        [
            "    default:",
            '        throw std::out_of_range("invalid scalar store-address writeback lane");',
            "    }",
            "}",
            "",
            "inline ScalarStoreWriteback sample_scalar_store_data_writeback(",
            "    UTMemBlock &dut, unsigned lane)",
            "{",
            "    ScalarStoreWriteback result;",
            "    switch (lane) {",
        ]
    )
    for lane in std_wb_lanes:
        prefix = f"io_mem_to_ooo_writebackStd_{lane}"
        lines.append(f"    case {lane}:")
        lines.append(f"        result.valid = dut.{prefix}_valid.B();")
        lines.append(
            f"        result.rob_value = dut.{prefix}_bits_uop_robIdx_value.U();"
        )
        lines.extend(["        return result;"])
    lines.extend(
        [
            "    default:",
            '        throw std::out_of_range("invalid scalar store-data writeback lane");',
            "    }",
            "}",
            "",
        ]
    )
    return lines


def render_vector_adapters(manifest: dict[str, Any]) -> list[str]:
    """Render the vector-memory issue and writeback adapters."""
    port_names = {port["name"] for port in manifest["ports"]}
    issue_lanes = matching_lanes(
        port_names, r"io_ooo_to_mem_issueVldu_([0-9]+)_valid"
    )
    writeback_lanes = matching_lanes(
        port_names, r"io_mem_to_ooo_writebackVldu_([0-9]+)_valid"
    )
    if not issue_lanes and not writeback_lanes:
        return []

    lines = [
        "struct VectorMemoryIssue {",
        "    std::uint64_t ftq_ptr = 0;",
        "    std::uint8_t ftq_offset = 0;",
        "    std::uint64_t fu_type = 0;",
        "    std::uint16_t fu_op_type = 0;",
        "    bool vec_wen = true;",
        "    bool v0_wen = false;",
        "    bool vl_wen = false;",
        "    bool vma = false;",
        "    bool vta = false;",
        "    std::uint8_t vsew = 0;",
        "    std::uint8_t vlmul = 0;",
        "    bool vm = true;",
        "    std::uint8_t vstart = 0;",
        "    std::uint8_t vuop_idx = 0;",
        "    bool last_uop = true;",
        "    std::array<unsigned char, 16> vmask{};",
        "    std::uint8_t nf = 0;",
        "    std::uint8_t veew = 0;",
        "    bool is_vleff = false;",
        "    std::uint8_t pdest = 0;",
        "    bool rob_flag = false;",
        "    std::uint8_t rob_value = 0;",
        "    bool lq_flag = false;",
        "    std::uint8_t lq_value = 0;",
        "    bool sq_flag = false;",
        "    std::uint8_t sq_value = 0;",
        "    std::array<std::array<unsigned char, 16>, 5> src{};",
        "    std::uint8_t flow_num = 2;",
        "    bool is_part_replay = false;",
        "    std::uint16_t replay_mask = 0;",
        "    std::uint8_t replay_mb_index = 0;",
        "};",
        "",
        "inline void clear_vector_memory_issue_valids(UTMemBlock &dut)",
        "{",
    ]
    for lane in issue_lanes:
        lines.append(
            f"    dut.io_ooo_to_mem_issueVldu_{lane}_valid.ImmSet(std::uint64_t{{0}});"
        )
    lines.extend(
        [
            "}",
            "",
            "inline bool vector_memory_issue_ready(UTMemBlock &dut, unsigned lane)",
            "{",
            "    switch (lane) {",
        ]
    )
    for lane in issue_lanes:
        lines.append(
            f"    case {lane}: return dut.io_ooo_to_mem_issueVldu_{lane}_ready.B();"
        )
    lines.extend(
        [
            "    default:",
            '        throw std::out_of_range("invalid vector memory issue lane");',
            "    }",
            "}",
            "",
            "inline void drive_vector_memory_issue(",
            "    UTMemBlock &dut, unsigned lane, const VectorMemoryIssue &item)",
            "{",
            "    switch (lane) {",
        ]
    )
    assignments = {
        "uop_ftqPtr_flag": "std::uint64_t{0}",
        "uop_ftqPtr_value": "item.ftq_ptr",
        "uop_ftqOffset": "item.ftq_offset",
        "uop_fuType": "item.fu_type",
        "uop_fuOpType": "item.fu_op_type",
        "uop_vecWen": "item.vec_wen",
        "uop_v0Wen": "item.v0_wen",
        "uop_vlWen": "item.vl_wen",
        "uop_vpu_vma": "item.vma",
        "uop_vpu_vta": "item.vta",
        "uop_vpu_vsew": "item.vsew",
        "uop_vpu_vlmul": "item.vlmul",
        "uop_vpu_vm": "item.vm",
        "uop_vpu_vstart": "item.vstart",
        "uop_vpu_vuopIdx": "item.vuop_idx",
        "uop_vpu_lastUop": "item.last_uop",
        "uop_vpu_nf": "item.nf",
        "uop_vpu_veew": "item.veew",
        "uop_vpu_isVleff": "item.is_vleff",
        "uop_pdest": "item.pdest",
        "uop_robIdx_flag": "item.rob_flag",
        "uop_robIdx_value": "item.rob_value",
        "uop_lqIdx_flag": "item.lq_flag",
        "uop_lqIdx_value": "item.lq_value",
        "uop_sqIdx_flag": "item.sq_flag",
        "uop_sqIdx_value": "item.sq_value",
        "flowNum": "item.flow_num",
        "isVecPartReplay": "item.is_part_replay",
        "vecReplayMask": "item.replay_mask",
        "vecReplayMbIdx": "item.replay_mb_index",
    }
    for lane in issue_lanes:
        prefix = f"io_ooo_to_mem_issueVldu_{lane}_bits_"
        lines.append(f"    case {lane}: {{")
        for suffix, value in assignments.items():
            if f"{prefix}{suffix}" in port_names:
                lines.append(f"        dut.{prefix}{suffix}.ImmSet({value});")
        vmask_name = f"{prefix}uop_vpu_vmask"
        if vmask_name in port_names:
            lines.extend(
                [
                    "        std::vector<unsigned char> vmask(",
                    "            item.vmask.begin(), item.vmask.end());",
                    f"        dut.{vmask_name}.ImmSetBytes(vmask);",
                ]
            )
        for source in range(5):
            name = f"{prefix}src_{source}"
            if name not in port_names:
                continue
            lines.extend(
                [
                    f"        std::vector<unsigned char> src_{source}(",
                    f"            item.src[{source}].begin(), item.src[{source}].end());",
                    f"        dut.{name}.ImmSetBytes(src_{source});",
                ]
            )
        lines.extend(
            [
                f"        dut.io_ooo_to_mem_issueVldu_{lane}_valid.ImmSet(std::uint64_t{{1}});",
                "        return;",
                "    }",
            ]
        )
    lines.extend(
        [
            "    default:",
            '        throw std::out_of_range("invalid vector memory issue lane");',
            "    }",
            "}",
            "",
            "struct VectorMemoryWriteback {",
            "    bool valid = false;",
            "    std::uint32_t exception_mask = 0;",
            "    std::uint8_t trigger = 15;",
            "    bool debug_is_mmio = false;",
            "    bool debug_is_ncio = false;",
            "    bool debug_is_perf_cnt = false;",
            "    std::uint16_t fu_op_type = 0;",
            "    bool vec_wen = false;",
            "    bool v0_wen = false;",
            "    bool vl_wen = false;",
            "    bool flush_pipe = false;",
            "    std::uint8_t vsew = 0;",
            "    std::uint8_t vlmul = 0;",
            "    std::uint8_t vstart = 0;",
            "    std::uint8_t vuop_idx = 0;",
            "    std::vector<unsigned char> vmask;",
            "    std::uint8_t vl = 0;",
            "    std::uint8_t nf = 0;",
            "    std::uint8_t veew = 0;",
            "    std::uint8_t pdest = 0;",
            "    bool rob_flag = false;",
            "    std::uint8_t rob_value = 0;",
            "    bool replay = false;",
            "    std::vector<unsigned char> data;",
            "    std::uint8_t vd_index_in_field = 0;",
            "};",
            "",
            "inline VectorMemoryWriteback sample_vector_memory_writeback(",
            "    UTMemBlock &dut, unsigned lane)",
            "{",
            "    VectorMemoryWriteback result;",
            "    switch (lane) {",
        ]
    )
    sample_assignments = {
        "fu_op_type": ("uop_fuOpType", "U()"),
        "trigger": ("uop_trigger", "U()"),
        "vec_wen": ("uop_vecWen", "B()"),
        "v0_wen": ("uop_v0Wen", "B()"),
        "vl_wen": ("uop_vlWen", "B()"),
        "flush_pipe": ("uop_flushPipe", "B()"),
        "vsew": ("uop_vpu_vsew", "U()"),
        "vlmul": ("uop_vpu_vlmul", "U()"),
        "vstart": ("uop_vpu_vstart", "U()"),
        "vuop_idx": ("uop_vpu_vuopIdx", "U()"),
        "vl": ("uop_vpu_vl", "U()"),
        "nf": ("uop_vpu_nf", "U()"),
        "veew": ("uop_vpu_veew", "U()"),
        "pdest": ("uop_pdest", "U()"),
        "rob_flag": ("uop_robIdx_flag", "B()"),
        "rob_value": ("uop_robIdx_value", "U()"),
        "replay": ("uop_replayInst", "B()"),
        "vd_index_in_field": ("vdIdxInField", "U()"),
    }
    for lane in writeback_lanes:
        prefix = f"io_mem_to_ooo_writebackVldu_{lane}"
        lines.append(f"    case {lane}:")
        lines.append(f"        result.valid = dut.{prefix}_valid.B();")
        for bit in range(24):
            name = f"{prefix}_bits_uop_exceptionVec_{bit}"
            if name in port_names:
                lines.append(
                    f"        result.exception_mask |= dut.{name}.B() ? (1U << {bit}) : 0U;"
                )
        for member, (suffix, method) in sample_assignments.items():
            name = f"{prefix}_bits_{suffix}"
            if name in port_names:
                lines.append(f"        result.{member} = dut.{name}.{method};")
        vmask_name = f"{prefix}_bits_uop_vpu_vmask"
        if vmask_name in port_names:
            lines.append(f"        result.vmask = dut.{vmask_name}.GetBytes();")
        data_name = f"{prefix}_bits_data"
        if data_name in port_names:
            lines.append(f"        result.data = dut.{data_name}.GetBytes();")
        for member, suffix in (
            ("debug_is_mmio", "debug_isMMIO"),
            ("debug_is_ncio", "debug_isNCIO"),
            ("debug_is_perf_cnt", "debug_isPerfCnt"),
        ):
            name = f"{prefix}_bits_{suffix}"
            if name in port_names:
                lines.append(f"        result.{member} = dut.{name}.B();")
        lines.append("        return result;")
    lines.extend(
        [
            "    default:",
            '        throw std::out_of_range("invalid vector memory writeback lane");',
            "    }",
            "}",
            "",
        ]
    )
    return lines


def render_pin_space_helpers(manifest: dict[str, Any]) -> list[str]:
    inputs = [
        port
        for port in manifest["ports"]
        if port["direction"] == "input" and port["name"] not in {"clock", "reset"}
    ]
    outputs = [port for port in manifest["ports"] if port["direction"] == "output"]
    input_bits = sum(port.get("width", 1) for port in inputs)
    output_bits = sum(port.get("width", 1) for port in outputs)
    lines = [
        "inline std::vector<unsigned char> pin_pattern_bytes(",
        "    unsigned width, unsigned salt, unsigned pattern)",
        "{",
        "    std::vector<unsigned char> result((width + 7) / 8);",
        "    std::uint64_t state = 0x9e3779b97f4a7c15ULL ^",
        "                          (std::uint64_t{salt} * 0xd1b54a32d192ed03ULL) ^",
        "                          (std::uint64_t{pattern} * 0x94d049bb133111ebULL);",
        "    for (auto &byte : result) {",
        "        state ^= state << 13;",
        "        state ^= state >> 7;",
        "        state ^= state << 17;",
        "        const unsigned char mixed = static_cast<unsigned char>(state);",
        "        byte = pattern == 0 ? 0x00 : pattern == 1 ? 0xff : mixed;",
        "    }",
        "    if ((width & 7U) != 0) {",
        "        result.back() &= static_cast<unsigned char>((1U << (width & 7U)) - 1U);",
        "    }",
        "    return result;",
        "}",
        "",
        "inline std::uint64_t pin_pattern_u64(",
        "    unsigned width, unsigned salt, unsigned pattern)",
        "{",
        "    const auto bytes = pin_pattern_bytes(width, salt, pattern);",
        "    std::uint64_t result = 0;",
        "    for (std::size_t index = 0; index < bytes.size(); ++index) {",
        "        result |= std::uint64_t{bytes[index]} << (index * 8);",
        "    }",
        "    return result;",
        "}",
        "",
        "inline void drive_pin_space_pattern(UTMemBlock &dut, unsigned pattern)",
        "{",
    ]
    for index, port in enumerate(inputs):
        width = port.get("width", 1)
        if width <= 64:
            lines.append(
                f"    dut.{port['name']}.ImmSet(pin_pattern_u64({width}U, {index}U, pattern));"
            )
        else:
            lines.extend(
                [
                    "    {",
                    f"        auto value = pin_pattern_bytes({width}U, {index}U, pattern);",
                    f"        dut.{port['name']}.ImmSetBytes(value);",
                    "    }",
                ]
            )
    lines.extend(
        [
            "}",
            "",
            "inline bool verify_pin_space_pattern(UTMemBlock &dut, unsigned pattern)",
            "{",
        ]
    )
    for index, port in enumerate(inputs):
        width = port.get("width", 1)
        if width <= 64:
            actual = f"dut.{port['name']}.U()"
            expected = f"pin_pattern_u64({width}U, {index}U, pattern)"
        else:
            actual = f"dut.{port['name']}.GetBytes()"
            expected = f"pin_pattern_bytes({width}U, {index}U, pattern)"
        lines.extend(
            [
                f"    if ({actual} != {expected}) {{",
                f"        std::cerr << \"input pattern mismatch: {port['name']}\\n\";",
                "        return false;",
                "    }",
            ]
        )
    lines.extend(
        [
            "    return true;",
            "}",
            "",
            "inline std::uint64_t sample_all_outputs(UTMemBlock &dut)",
            "{",
            "    std::uint64_t digest = 1469598103934665603ULL;",
        ]
    )
    for index, port in enumerate(outputs):
        width = port.get("width", 1)
        lines.extend(["    {", f"        digest ^= {index}U;",
                      "        digest *= 1099511628211ULL;"])
        if width <= 64:
            lines.extend(
                [
                    f"        const auto value = dut.{port['name']}.U();",
                    f"        for (unsigned byte = 0; byte < {(width + 7) // 8}U; ++byte) {{",
                    "            digest ^= static_cast<unsigned char>(value >> (byte * 8));",
                    "            digest *= 1099511628211ULL;",
                    "        }",
                ]
            )
        else:
            lines.extend(
                [
                    f"        const auto value = dut.{port['name']}.GetBytes();",
                    f"        for (unsigned byte = 0; byte < {(width + 7) // 8}U; ++byte) {{",
                    "            digest ^= value[byte];",
                    "            digest *= 1099511628211ULL;",
                    "        }",
                ]
            )
        lines.append("    }")
    lines.extend(
        [
            "    return digest;",
            "}",
            "",
            f"inline constexpr unsigned kSweptInputCount = {len(inputs)}U;",
            f"inline constexpr unsigned kSampledOutputCount = {len(outputs)}U;",
            f"inline constexpr unsigned kSweptInputBitCount = {input_bits}U;",
            f"inline constexpr unsigned kSampledOutputBitCount = {output_bits}U;",
            "inline constexpr unsigned kPinSpacePatternCount = 256U;",
            "",
        ]
    )
    return lines


def render(manifest: dict[str, Any]) -> str:
    inputs = [port for port in manifest["ports"] if port["direction"] == "input"]
    missing_policy = [port["name"] for port in inputs if not port["input_policy"]]
    if missing_policy:
        raise CppGenerationError(
            "input ports without idle policy: " + ", ".join(missing_policy)
        )
    policies = {"zero": 0, "one": 1, "reset_active_high": 1}
    unsupported = sorted(
        {
            port["input_policy"]
            for port in inputs
            if port["input_policy"] not in policies and port["input_policy"] != "clock"
        }
    )
    if unsupported:
        raise CppGenerationError("unsupported input policies: " + ", ".join(unsupported))

    outputs = [port for port in manifest["ports"] if quiescent_output(port)]
    lines = [
        "// Generated by scripts/generate_cpp.py; do not edit manually.",
        f"// MemBlock RTL SHA-256: {manifest.get('complete_rtl_sha256', manifest['rtl_sha256'])}",
        "#pragma once",
        "",
        "#include \"UT_MemBlock.hpp\"",
        "",
        "#include <array>",
        "#include <cstddef>",
        "#include <cstdint>",
        "#include <iostream>",
        "#include <stdexcept>",
        "#include <vector>",
        "",
        "namespace memblock::generated {",
        "",
        "inline void drive_idle_inputs(UTMemBlock &dut)",
        "{",
    ]
    for port in inputs:
        policy = port["input_policy"]
        if policy == "clock":
            continue
        lines.append(
            f"    dut.{port['name']}.ImmSet(std::uint64_t{{{policies[policy]}}});"
        )
    lines.extend(
        [
            "}",
            "",
            "inline bool expect_quiescent_outputs(UTMemBlock &dut)",
            "{",
            "    bool ok = true;",
        ]
    )
    for port in outputs:
        lines.extend(
            [
                f"    if (dut.{port['name']}.B()) {{",
                f"        std::cerr << \"unexpected idle valid: {port['name']}\\n\";",
                "        ok = false;",
                "    }",
            ]
        )
    lines.extend(
        [
            "    return ok;",
            "}",
            "",
        ]
    )
    lines.extend(render_pin_space_helpers(manifest))
    lines.extend(render_lane_adapters(manifest))
    lines.extend(render_vector_adapters(manifest))
    lines.extend(
        [
            f"inline constexpr const char *kRtlSha256 = \"{manifest.get('complete_rtl_sha256', manifest['rtl_sha256'])}\";",
            f"inline constexpr unsigned kDrivenInputCount = {len(inputs) - 1}U;",
            f"inline constexpr unsigned kQuiescentOutputCount = {len(outputs)}U;",
            "",
            "} // namespace memblock::generated",
            "",
        ]
    )
    return "\n".join(lines)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("action", choices=("generate", "check"))
    parser.add_argument("--manifest", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    try:
        manifest = json.loads(args.manifest.read_text(encoding="utf-8"))
        content = render(manifest)
        if args.action == "generate":
            args.output.parent.mkdir(parents=True, exist_ok=True)
            if not args.output.exists() or args.output.read_text(encoding="utf-8") != content:
                args.output.write_text(content, encoding="utf-8")
        elif not args.output.exists() or args.output.read_text(encoding="utf-8") != content:
            raise CppGenerationError(
                f"generated C++ header is stale: {args.output}; regenerate and review the diff"
            )
    except (OSError, json.JSONDecodeError, KeyError, CppGenerationError) as error:
        print(f"generate_cpp.py: error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
