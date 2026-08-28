#!/usr/bin/env python3
from __future__ import annotations

import csv
import io
from pathlib import Path

from refresh_jiabowen_ifu_v3_contract import _mapped_paths, _read_csv, _write_csv


_FRONTEND_ROOT = Path(__file__).resolve().parents[1]
_TESTPOINT_PATH = (
    _FRONTEND_ROOT
    / "docs"
    / "02_testpoint"
    / "Frontend_testpoint_0525_coverage_backannotated.csv"
)
_PILOT_PATH = (
    _FRONTEND_ROOT
    / "docs"
    / "03_funcov_model"
    / "frontend_bt_functional_coverage_pilot.csv"
)
_GROUP = "ifu_instruncache_owner_v3"
_COVERPOINT = "protocol_leaf"
_MODEL_EVIDENCE = "MODEL:sample_instr_uncache_owner_coverage"

_LEAVES = (
    "A通道valid保持期间地址addr不变化",
    "A通道valid保持期间memBackTypeMM不变化",
    "A通道valid保持期间memPageTypeNC不变化",
    "A通道fire后进入等待D通道响应",
    "D通道返回后输出data",
    "D通道返回denied时形成取指异常输入",
    "D通道返回corrupt时形成取指异常输入",
    "D通道返回时同步给出needResend判断",
    "等待A握手期间flush取消旧请求",
    "等待D返回期间flush使旧响应不可见",
    "resend过程中flush清理内部resending状态",
    "RVI起始于8B beat末尾2B且不跨页，第一beat clean时触发内部resend",
    "RVI起始于8B beat内0/2/4偏移时不触发内部resend",
    "RVC位于8B beat末尾2B时不触发resend",
    "第一beat corrupt且denied=0时不触发resend而直接异常返回",
    "第一beat corrupt和denied同时返回时不触发resend而直接异常返回",
    "内部resend第二beat请求地址按下一8B beat生成",
    "第二beat clean时拼接前后两个halfword形成完整RVI",
    "第二beat corrupt且denied=0时异常绑定当前RVI",
    "第二beat corrupt和denied同时返回时异常绑定当前RVI",
    "内部resend完成后清理resending状态并只返回一条指令",
    "RVI起始于4K页尾2B且第一半无异常时返回needResend",
    "跨页RVI不假设物理页连续，不能在InstrUncache内部直接取下一页",
    "跨页RVC页尾2B不返回needResend",
    "跨页第一半corrupt/denied时不返回needResend",
    "needResend由IFU转化为uncacheRedirect.isHalfInstr",
    "IFU保存halfPc/halfData后重新发起下一页取指",
    "下一页返回后由IFU侧完成跨页RVI拼接",
    "跨页补半期间flush必须清理IFU half状态与InstrUncache内部状态",
    "RVI第一页低半PMP execute deny时不应继续按第二页数据误译码",
    "RVI第一页低半PMP execute deny时mtval应为RVI起始PC",
    "RVI第一页低半PMP execute deny且第二页可执行时mcause仍为Instruction Access Fault",
    "第一页低半可取、第二页PMP execute deny时mtval仍为RVI起始PC",
    "第一页低半可取、第二页PF/GPF/AF时不把异常PC改成页基址",
    "异常优先级不应把跨页RVI误报成Illegal Instruction",
    "memBackTypeMM正确表达是否主存backing",
    "memPageTypeNC正确表达PBMT.NC属性",
    "MMIO与NC属性混合时由IFU上层决定顺序语义，InstrUncache只执行单次访问与resend语义",
)

_RTL_CONTRACT_REWRITES = {
    _LEAVES[8]: {
        "五级测试点": "A通道等待ready期间WFI撤回valid，解除后以相同请求上下文重发",
        "Condition": "InstrUncache entry处于RefillReq且TL A valid=1、ready=0；随后wfiReq有效，再解除wfiReq",
        "Checkpoint": "wfiReq期间A valid撤回且不发生fire；解除后同一addr/memBackTypeMM/memPageTypeNC重新出现并可握手",
        "Object": "InstrUncache entry state/reqReg、wfiReq、TL A valid/ready/address/user属性及fire",
        "evidence": "RTL_REVIEW:c0ca46459:InstrUncache.io.flush tied false; WFI is the implemented pre-A cancellation mechanism",
    },
    _LEAVES[9]: {
        "五级测试点": "等待D返回期间backend redirect使旧响应不得交付旧IFU路径",
        "Condition": "InstrUncache entry已发TL A并等待D；随后较老backend redirect清理IFU旧事务",
        "Checkpoint": "底层D和InstrUncache response允许自然完成，但旧ftqIdx/PC事务不得进入IBuffer；恢复路径使用新身份",
        "Object": "backend redirect、IFU uncache state/ftqIdx/PC、InstrUncache entry/D/response、IBuffer交付身份",
        "evidence": "RTL_REVIEW:c0ca46459:InstrUncache.io.flush tied false; cancellation is enforced at IFU delivery identity",
    },
    _LEAVES[10]: {
        "五级测试点": "frontend redirect期间内部resend自然完成但旧结果不得交付",
        "Condition": "InstrUncache已因跨8B RVI进入resending，随后backend/checker redirect清理IFU旧事务",
        "Checkpoint": "InstrUncache不接收frontend flush，可自然完成第二beat并清resending；旧路径response不得进入IBuffer，恢复路径身份独立",
        "Object": "redirect、entry resending/state、第二beat TL A/D、InstrUncache response、IFU/IBuffer ftqIdx/PC",
        "evidence": "RTL_REVIEW:c0ca46459:InstrUncache.io.flush tied false; do not require redirect to clear entry state",
    },
    _LEAVES[28]: {
        "五级测试点": "跨页补半期间backend redirect清理IFU half有效状态且旧半条不得泄漏",
        "Condition": "IFU已保存第一页halfPc/halfData并等待下一页事务；随后backend redirect",
        "Checkpoint": "s0/s1 half存储和s2有效状态被清理，旧half不得拼接或交付到新路径；InstrUncache此时无flush契约",
        "Object": "backend redirect、IFU s0/s1/s2 half valid/data/PC、下一页请求和IBuffer新旧身份",
        "evidence": "RTL_REVIEW:c0ca46459:half state belongs to IFU; InstrUncache.io.flush is tied false",
    },
}


def _append_pilot_rows(fields: list[str], rows: list[dict[str, str]]) -> None:
    buffer = io.StringIO(newline="")
    writer = csv.DictWriter(buffer, fieldnames=fields, lineterminator="\n")
    writer.writerows(rows)
    with _PILOT_PATH.open("a", encoding="utf-8", newline="") as handle:
        handle.write(buffer.getvalue())


def synchronize() -> dict[str, int]:
    testpoint_fields, testpoint_rows = _read_csv(_TESTPOINT_PATH)
    pilot_fields, pilot_rows = _read_csv(_PILOT_PATH)
    pilot_by_id = {row["Bin_ID"].strip(): row for row in pilot_rows}

    row_by_leaf: dict[str, int] = {}
    for index, row in enumerate(testpoint_rows):
        leaf = row["五级测试点"].strip()
        if leaf in _LEAVES:
            if leaf in row_by_leaf:
                raise ValueError(f"duplicate InstrUncache leaf: {leaf}")
            row_by_leaf[leaf] = index
    missing = set(_LEAVES) - set(row_by_leaf)
    if missing:
        # The tool is idempotent after the reviewed flush leaves are renamed.
        rewritten = {
            values["五级测试点"]: original
            for original, values in _RTL_CONTRACT_REWRITES.items()
        }
        for index, row in enumerate(testpoint_rows):
            original = rewritten.get(row["五级测试点"].strip())
            if original is not None:
                row_by_leaf[original] = index
        missing = set(_LEAVES) - set(row_by_leaf)
    if missing:
        raise ValueError(f"missing InstrUncache leaves: {sorted(missing)}")

    new_pilot_rows: list[dict[str, str]] = []
    for ordinal, leaf in enumerate(_LEAVES, start=1):
        row_index = row_by_leaf[leaf]
        row = testpoint_rows[row_index]
        bin_id = f"BIN-{1093 + ordinal}"
        bin_name = f"instruncache_leaf_{ordinal:03d}"
        python_mapping = (
            f"covergroup {_GROUP}, coverpoint {_COVERPOINT}, bins {bin_name} ({bin_id})"
        )
        if bin_id not in row["coverage"]:
            row["coverage"] = "; ".join(
                part for part in (row["coverage"].strip(), python_mapping) if part
            )
        if row["status"] == "UNMAPPED":
            row["status"] = "MODELED"
        if _MODEL_EVIDENCE not in row["evidence"]:
            row["evidence"] = "; ".join(
                part for part in (_MODEL_EVIDENCE, row["evidence"].strip()) if part
            )
        rewrite = _RTL_CONTRACT_REWRITES.get(leaf)
        if rewrite:
            for field, value in rewrite.items():
                if field == "evidence":
                    if value not in row[field]:
                        row[field] = "; ".join(
                            part for part in (value, row[field].strip()) if part
                        )
                else:
                    row[field] = value

        paths = _mapped_paths(testpoint_rows)
        pilot_row = {
            "Bin_ID": bin_id,
            "阶段": "L1",
            "覆盖类型": "协议叶子覆盖",
            "Coverage_Group": _GROUP,
            "Coverpoint": _COVERPOINT,
            "Bin_Name": bin_name,
            "映射测试点路径": paths[row_index],
            "建议采样事件": row["Condition"].strip(),
            "建议观测对象": row["Object"].strip(),
            "命中判据": row["Checkpoint"].strip(),
            "优先级": "P0" if row["status"] == "PARTIAL" else "P1",
            "建议试点用例": "fe_instr_uncache_protocol_v3",
            "Legacy_Bin_ID": "",
        }
        existing = pilot_by_id.get(bin_id)
        if existing is None:
            new_pilot_rows.append(pilot_row)
            pilot_by_id[bin_id] = pilot_row
        elif existing != pilot_row:
            raise ValueError(f"existing pilot row differs: {bin_id}")

    _write_csv(_TESTPOINT_PATH, testpoint_fields, testpoint_rows)
    if new_pilot_rows:
        if len(new_pilot_rows) != len(_LEAVES):
            raise ValueError("InstrUncache pilot registry is partially populated")
        _append_pilot_rows(pilot_fields, new_pilot_rows)
    return {
        "registered_leaves": len(_LEAVES),
        "new_pilot_rows": len(new_pilot_rows),
        "first_bin": 1094,
        "last_bin": 1131,
    }


if __name__ == "__main__":
    print(synchronize())
