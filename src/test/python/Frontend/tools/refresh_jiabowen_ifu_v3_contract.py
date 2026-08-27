#!/usr/bin/env python3
"""Apply the reviewed Jiabowen IFU testpoint contract for V3 c0ca46459."""

from __future__ import annotations

import argparse
import csv
import io
import re
import subprocess
from dataclasses import dataclass
from pathlib import Path


_FRONTEND_ROOT = Path(__file__).resolve().parents[1]
_REPO_ROOT = _FRONTEND_ROOT.parents[3]
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
_BIN_RE = re.compile(r"BIN-\d+")
_LEVEL_FIELDS = ("一级测试点", "二级测试点", "三级测试点", "四级测试点", "五级测试点")


@dataclass(frozen=True)
class ContractUpdate:
    leaf: str
    condition: str
    checkpoint: str
    observation: str
    status: str | None = None
    evidence: str = ""


_CACHEABLE = "PMA可缓存且可执行地址"
_REVIEW = "RTL_REVIEW:c0ca46459"
_SUPERSEDED_EVIDENCE = {
    "BIN-1067": (
        f"{_REVIEW}:PredChecker is a cacheable writeback path; this leaf covers "
        "an older cacheable checker redirect cancelling a younger NC transaction"
    ),
}

_UPDATES = {
    "BIN-899": ContractUpdate(
        "taken预测返回块保留takenCfiOffset、range和size元数据",
        f"{_CACHEABLE}；ICache返回taken预测块",
        "s0/s1保留takenCfiOffset有效位、offset、range和size；target由PreDecode/PredChecker/checkerRedirect规范项校验",
        "ICache→IFU返回valid/ready、takenCfiOffset、range、size；IFU s0/s1对应字段",
        evidence=f"{_REVIEW}:ICache响应不携带target，target校验归入CSV 708/732/754",
    ),
    "BIN-901": ContractUpdate(
        "BPU stage3 flush按block0 ftqIdx命中时整块双fetch窗口共同丢弃",
        f"{_CACHEABLE}；双fetch窗口有效且BPU stage3 flush命中block0 ftqIdx",
        "当前V3以block0 ftqIdx标识整个返回窗口，命中时两块均不进入后续拼接；不支持block1-only选择性删除",
        "双块返回、block0/block1 ftqIdx、BPU stage3 flush、s0_flush/s0_fire、后续IBuffer",
        evidence=f"{_REVIEW}:whole-window flush contract; block1-only deletion is unsupported",
    ),
    "BIN-831": ContractUpdate(
        "ICache返回takenCfiOffset有效时保留valid、offset、range和size",
        f"{_CACHEABLE}；ICache返回takenCfiOffset.valid=1",
        "匹配的s1事务保持takenCfiOffset valid/bits、range和size一致；本接口不校验target",
        "ICache→IFU源事务及s1 fetchBlock的takenCfiOffset、range、size",
        evidence=f"{_REVIEW}:target不是ICacheToIfu接口字段",
    ),
    "BIN-814": ContractUpdate(
        "BPU stage3 flush命中block0 ftqIdx时清理整个返回窗口",
        f"{_CACHEABLE}；BPU stage3 flush命中当前窗口block0 ftqIdx",
        "s0_flushFromBpu和s0_flush有效且s0_fire为0，双块窗口按整体被丢弃",
        "BPU stage3 flush、block0 ftqIdx、s0_flushFromBpu、s0_flush、s0_fire",
        evidence=f"{_REVIEW}:block0 identifies the complete IFU response window",
    ),
    "BIN-815": ContractUpdate(
        "BPU stage3 flush未命中block0 ftqIdx时不误杀整个新窗口",
        f"{_CACHEABLE}；BPU stage3 flush有效但不命中当前窗口block0 ftqIdx",
        "s0_flushFromBpu保持无效，当前单块或双块返回窗口正常fire",
        "BPU stage3 flush、block0 ftqIdx、s0_flushFromBpu、s0_fire、返回窗口",
        evidence=f"{_REVIEW}:nonmatching block0 identity preserves the complete window",
    ),
    "BIN-839": ContractUpdate(
        "ICache响应从s0到s1的ftqIdx、地址和范围身份保持自洽",
        f"{_CACHEABLE}；ICache→IFU响应完成握手",
        "同一已接收响应在s1保持ftqIdx、startVAddr、takenCfiOffset、range和size一致，不比较IFU接口不可见的raw FTQ请求",
        "ICache→IFU响应快照、s0_fire、s1 fetchBlock快照",
        evidence=f"{_REVIEW}:identity scope is ICache response to IFU s1, not raw FTQ request matching",
    ),
    "BIN-904": ContractUpdate(
        "上游检测任一块uncache/MMIO属性时不生成cacheable+uncache混合双块响应",
        "FTQ请求第二块有效，WayLookup任一块具有uncache/MMIO属性",
        "ICache MainPipe的realTwoFetchValid为0且toIfu.req.bits(1).valid为0；IFU不接收非法混合双块窗口",
        "MainPipe s0_hasMmio、s0_realTwoFetchValid、FTQ req(1).valid、toIfu req(1).valid",
        evidence=f"{_REVIEW}:mixed cacheable+uncache dual-block response is not a legal V3 IFU input",
    ),
    "BIN-906": ContractUpdate(
        "第一cacheline late ECC/TL corrupt或denied在s1 stall/flush竞争下归属当前窗口",
        f"{_CACHEABLE}；第一cacheline late ECC或TL错误与s1 stall或flush相邻",
        "未flush时异常合入meta(0)并绑定首个可见槽；flush时旧窗口不可交付，PreDecode不得覆盖异常",
        "ICache late corrupt/denied、IFU s1_valid/ready/flush、meta(0).exception、IBuffer exceptionMask",
        status="MODELED",
        evidence=f"{_REVIEW}:revised late-fault timing contract requires fresh directed evidence",
    ),
    "BIN-907": ContractUpdate(
        "第一块异常将交付截断为首个异常槽并屏蔽后续正常指令",
        f"{_CACHEABLE}；meta(0)包含ITLB/PMP/ECC/TL异常且窗口后续数据正常",
        "s1_instrCount收敛为1，IBuffer仅首个可见槽携带异常，后续正常指令不交付",
        "meta(0).exception、s1_instrCount、IBuffer enqEnable/exceptionMask/exceptionType",
        evidence=f"{_REVIEW}:V3 exception count and mask are based on meta(0)",
    ),
    "BIN-908": ContractUpdate(
        "第二块ITLB异常禁止双fetch；第二块独立PMP异常尚无per-block接口",
        "FTQ请求第二块有效且第二块WayLookup ITLB异常；另行审计第二块PMP表达能力",
        "MainPipe s0_hasItlbException有效、realTwoFetchValid为0且toIfu第二块无效；不得声称第二块PMP lane已独立验证",
        "第二块ITLB exception、s0_hasItlbException、s0_realTwoFetchValid、toIfu req(1).valid、PMP接口粒度",
        status="PARTIAL",
        evidence=f"{_REVIEW}:second ITLB suppression is reachable; second-block PMP is not independently represented",
    ),
    "BIN-909": ContractUpdate(
        "第二cacheline late ECC/TL corrupt或denied的精确lane归属",
        "双cacheline返回中仅第二cacheline产生late ECC/TL corrupt或denied，并覆盖stall与flush竞争",
        "OPEN/FIXME：当前异常归并后经meta(0)计数和mask，RTL尚不能证明第二块指令lane精确归属；不得伪造HIT",
        "ICache per-line corrupt/denied、IFU s1_icacheMeta(0/1)、s1 stall/flush、IBuffer exceptionMask",
        status="BLOCKED",
        evidence=f"{_REVIEW}:OPEN/FIXME current RTL merges the late fault and lacks second-lane attribution",
    ),
    "BIN-933": ContractUpdate(
        "预测正确时正常交付IBuffer且不产生错误的FTQ redirect",
        f"{_CACHEABLE}；PredChecker请求无remask fault且cacheable输出可交付",
        "合法指令正常进入IBuffer，checkerRedirect、uncache redirect和toFtq.wbRedirect均不因普通路径误拉高",
        "toIBuffer fire及payload、PredChecker fixedInstrValid/checkerRedirect、toFtq.wbRedirect.valid",
        status="MODELED",
        evidence=f"{_REVIEW}:ordinary cacheable flow has no toFtq.wbRedirect contract; fresh evidence required",
    ),
    "BIN-934": ContractUpdate(
        "Non-CFI被预测taken时形成notCfiTaken、canTrain并进入FTQ resolve",
        f"{_CACHEABLE}；真实Non-CFI槽被预测taken",
        "PredChecker产生notCfiTaken redirect，IFU置canTrain，FTQ随后产生对应ifuResolve且mispredict为1",
        "checkerRedirect.notCfiTaken、toFtq.wbRedirect.canTrain/ftqIdx、FTQ ifuResolve",
        status="MODELED",
        evidence=f"{_REVIEW}:expanded from PredChecker-only fault to IFU-to-FTQ training chain",
    ),
    "BIN-883": ContractUpdate(
        "IBuffer ready低时s2输出payload保持稳定",
        f"{_CACHEABLE}；toIBuffer.valid=1且ready连续为0",
        "连续反压周期活动槽的s2 payload签名保持不变，ready恢复后交付同一事务",
        "toIBuffer valid/ready和活动槽instr/pc/isRvc/ftqPtr/endOffset payload",
        evidence=f"{_REVIEW}:normalized current IFU output stage name to s2",
    ),
    "BIN-886": ContractUpdate(
        "普通cacheable交付只更新内部wb bookkeeping且不产生toFtq.wbRedirect",
        f"{_CACHEABLE}；正常toIBuffer fire后进入IFU内部wb拍且无预测错误",
        "wbInstrCount与真实入队计数一致，同时checkerRedirect和toFtq.wbRedirect.valid均为0",
        "先前IBuffer入队事务、wbValid/wbInstrCount、checkerRedirect、toFtq.wbRedirect.valid",
        status="MODELED",
        evidence=f"{_REVIEW}:revised bin explicitly excludes ordinary FTQ redirect; fresh evidence required",
    ),
    "BIN-952": ContractUpdate(
        "BPU flush使用block0 ftqIdx作为整个IFU窗口身份",
        f"{_CACHEABLE}；BPU stage3 flush分别命中和未命中当前block0 ftqIdx",
        "命中时入口窗口整体flush，未命中时窗口整体保留；当前不支持block1-only选择性裁剪",
        "block0/block1 ftqIdx、s0_flushFromBpu、s0/s1/s2 valid/fire、IBuffer结果",
        evidence=f"{_REVIEW}:whole-window identity contract replaces selective block1 semantics",
    ),
    "BIN-954": ContractUpdate(
        "异常窗口完整传递backend与跨页元数据字段",
        "分别构造isBackendException、hasSatpFlush、GPF gpAddr/isForVSnonLeafPTE及跨页异常",
        "s2交付保持isBackendException、hasSatpFlush、exceptionCrossPage，并在GPF时写入匹配ftqIdx的gpAddr/isForVSnonLeafPTE",
        "s2 meta(0)、toIBuffer backend/跨页字段、toBackend.gpAddrMem wen/waddr/wdata",
        status="MODELED",
        evidence=f"{_REVIEW}:replaces random-only stimulus with explicit exception metadata coverage",
    ),
    "BIN-955": ContractUpdate(
        "合法路径类型、fetch形态、指令宽度和预测结果显式cross",
        "按V3合法组合覆盖cacheable/NC/MMIO × single/dual/cross-line/cross-page × RVC/RVI/cross-half × NT/taken/invalidTaken/notCfiTaken",
        "每个组合遵守路径能力：NC/MMIO不伪造双块输入，cross-line/page通过resend或连续事务表达；输出PC/half/属性一致",
        "path、fetch shape、isRvc/cross-half、prediction outcome、IBuffer/redirect结果",
        status="MODELED",
        evidence=f"{_REVIEW}:legality-aware explicit cross replaces random-only closure",
    ),
    "BIN-956": ContractUpdate(
        "合法路径与stall/BPU flush/backend redirect/checker redirect显式cross",
        "在合法path×shape×width×prediction样本上分别施加stall、BPU flush、backend redirect和checker redirect",
        "各控制事件按优先级保留或丢弃完整事务，且PC、half、属性及redirect身份不串扰",
        "path/shape/width/prediction、s0/s1/s2 stall/flush、redirect、IBuffer结果",
        status="MODELED",
        evidence=f"{_REVIEW}:explicit control cross replaces random insertion",
    ),
    "BIN-957": ContractUpdate(
        "cacheable到NC/MMIO连续事务切换时PC、half和属性状态不串扰",
        "分别执行cacheable→NC、cacheable→MMIO、NC→cacheable和NC→MMIO连续合法事务",
        "每次切换均通过独立单块事务完成；后继路径PC、half状态、PBMT/PMP属性和顺序门控只来自当前事务",
        "路径接受/交付事件、PC、halfPc/halfData、PBMT/PMP属性、WaitLastCommit、redirect",
        status="MODELED",
        evidence=f"{_REVIEW}:sequential path transitions replace illegal mixed dual-block input",
    ),
    "BIN-958": ContractUpdate(
        "FTQ训练仅保留首个mispredict及其之前分支",
        "同一FTQ resolve entry包含多个有效分支且较早分支mispredict，之后仍有年轻分支",
        "trainFirstMispredictMask使trainCache只保留首个mispredict及其之前分支，所有更年轻训练valid均为0",
        "ResolveQueue bpuTrain branches、cfiPosition/mispredict、trainFirstMispredictMask、trainCache branch valids",
        status="MODELED",
        evidence=f"{_REVIEW}:cross-module FTQ training mask checker replaces random end-to-end claim",
    ),
    "BIN-1016": ContractUpdate(
        "s2有效且无flush、redirect时才接收MMIO属性取指块",
        "构造V3 MMIO IFU顺序取指路径；s2_valid且无s2_flush/backend redirect",
        "MMIO属性按meta(0)识别并进入IfuUncacheUnit；当前IFU输出阶段统一称为s2",
        "s2_valid、s2_icacheMeta(0)、s2_flush、uncacheUnit.req、toUncache、toIBuffer、toFtq.wbRedirect",
        evidence=f"{_REVIEW}:normalized stale f3/s3 wording to current s2",
    ),
    "BIN-1032": ContractUpdate(
        "IfuUncache SendReq反压时实际toUncache.valid保持低（PBMT.NC为合法witness）",
        "IfuUncacheUnit处于SendReq且io.toIBuffer.ready=0；使用PBMT.NC合法流制造反压，MMIO/IO因WaitLastCommit顺序化通常不产生该组合",
        "实际观测uncacheUnit.io.toUncache.req.valid=0；该协议点以CSV 1026–1142为canonical，NC区重复点仅作cross-reference",
        "uncacheState、ifuStall、io.toIBuffer.ready、uncacheUnit.io.toUncache.req.valid、PBMT/PMP属性",
        status="MODELED",
        evidence=f"{_REVIEW}:generic IfuUncache handshake contract uses a legal PBMT.NC witness and observes the real valid signal",
    ),
    "BIN-1062": ContractUpdate(
        "NC路径复用InstrUncache SendReq反压契约（cross-reference BIN-1032）",
        "PBMT.NC进入SendReq且IBuffer反压；作为canonical BIN-1032的合法路径witness",
        "命中BIN-1032的同一实际握手观察；NC区只确认路径适配，不重复定义IfuUncache FSM协议",
        "PBMT.NC属性、uncacheState、ifuStall、io.toIBuffer.ready、uncacheUnit.io.toUncache.req.valid",
        status="MODELED",
        evidence=f"{_REVIEW}:NC policy section cross-references the canonical InstrUncache protocol leaf BIN-1032",
    ),
    "BIN-1067": ContractUpdate(
        "更老cacheable checker redirect阻止年轻NC旧请求并从目标重启NC路径",
        "cacheable块产生PredChecker redirect，顺序后继为独立PBMT.NC事务；当前in-order IFU在NC接受前解析checker redirect",
        "checker redirect触发年轻流水flush且同拍不发出NC请求；随后仅redirect恢复路径的新NC请求可进入SendReq/WaitResp；NC自身不产生PredChecker redirect",
        "cacheable checkerRedirect/wbValid/toFtq.wbRedirect、s2 flush、NC属性和uncache req.fire、redirect周期与恢复NC接受周期",
        status="MODELED",
        evidence=f"{_REVIEW}:PredChecker is cacheable-only; legal V3 ordering resolves the checker redirect before the younger NC request and restarts NC from the redirect target",
    ),
    "BIN-1084": ContractUpdate(
        "NC页尾第一页执行权限异常应归属请求起始PC（OPEN/FIXME）",
        "PBMT.NC请求起始PC位于4K页尾2B，第一页PMP execute=0且第二页可执行",
        "toIBuffer交付Instruction Access Fault且活动槽PC等于请求起始PC；当前V3因s2_useUncacheFetch=0未更新uncachePc，实测活动槽PC为0，不得标HIT",
        "s2_reqIsUncache、s2_useUncacheFetch、s2起始PC、uncacheUnit.req.fire、uncachePc、toIBuffer活动槽PC/exceptionType",
        status="BLOCKED",
        evidence=f"{_REVIEW}:OPEN/FIXME first-page fault bypasses uncache req.fire but the uncache output branch still selects stale uncachePc",
    ),
    "BIN-1086": ContractUpdate(
        "NC页尾第一页不可执行时按Instruction Access Fault交付",
        "PBMT.NC请求起始PC位于4K页尾2B，第一页PMP execute=0且第二页可执行；权限异常发生在uncache请求发出前",
        "s2_reqIsUncache=1、s2_useUncacheFetch=0且s2异常为AF；toIBuffer实际交付AF；PC归属问题由BIN-1084独立跟踪",
        "PBMT.NC/PMP属性、s2起始PC、s2_reqIsUncache、s2_useUncacheFetch、toIBuffer valid/ready/exceptionType",
        status="MODELED",
        evidence=f"{_REVIEW}:separates the reachable IAF type contract from blocked BIN-1084 PC attribution",
    ),
}


def _read_csv(path: Path) -> tuple[list[str], list[dict[str, str]]]:
    with path.open(encoding="utf-8-sig", newline="") as handle:
        reader = csv.DictReader(handle)
        fields = list(reader.fieldnames or ())
        rows = [
            {field: str(row.get(field) or "") for field in fields}
            for row in reader
        ]
        return fields, rows


def _serialize_row(fields: list[str], row: dict[str, str]) -> str:
    buffer = io.StringIO(newline="")
    writer = csv.DictWriter(buffer, fieldnames=fields, lineterminator="\n")
    writer.writerow(row)
    return buffer.getvalue().removesuffix("\n")


def _write_csv(path: Path, fields: list[str], rows: list[dict[str, str]]) -> None:
    """Preserve untouched physical records to keep the review diff narrow."""

    relative = path.relative_to(_REPO_ROOT)
    head_bytes = subprocess.run(
        ["git", "show", f"HEAD:{relative}"],
        cwd=_REPO_ROOT,
        check=True,
        stdout=subprocess.PIPE,
    ).stdout
    head_text = head_bytes.decode("utf-8-sig")
    head_lines = head_text.splitlines()
    head_reader = csv.DictReader(io.StringIO(head_text, newline=""))
    head_rows = [
        {field: str(row.get(field) or "") for field in fields}
        for row in head_reader
    ]
    if list(head_reader.fieldnames or ()) != fields or len(head_rows) != len(rows):
        raise ValueError(f"cannot preserve HEAD layout for structurally changed CSV: {path}")

    output_lines = [head_lines[0]]
    for index, row in enumerate(rows):
        if row == head_rows[index]:
            output_lines.append(head_lines[index + 1])
        else:
            output_lines.append(_serialize_row(fields, row))
    path.write_text("\n".join(output_lines) + "\n", encoding="utf-8-sig", newline="")


def _bin_rows(rows: list[dict[str, str]]) -> dict[str, int]:
    found: dict[str, int] = {}
    for index, row in enumerate(rows):
        for bin_id in _BIN_RE.findall(row.get("coverage", "")):
            if bin_id in _UPDATES:
                if bin_id in found:
                    raise ValueError(f"duplicate testpoint mapping for {bin_id}")
                found[bin_id] = index
    missing = sorted(set(_UPDATES) - set(found))
    if missing:
        raise ValueError(f"missing testpoint mappings: {missing}")
    return found


def _mapped_paths(rows: list[dict[str, str]]) -> list[str]:
    context = [""] * len(_LEVEL_FIELDS)
    paths: list[str] = []
    for row in rows:
        for depth, field in enumerate(_LEVEL_FIELDS):
            value = row[field].strip()
            if value:
                context[depth] = value.replace("@加柏文", "").strip()
                context[depth + 1 :] = [""] * (len(context) - depth - 1)
        paths.append("/".join(value for value in context if value))
    return paths


def refresh(*, check: bool = False) -> dict[str, int]:
    testpoint_fields, testpoint_rows = _read_csv(_TESTPOINT_PATH)
    pilot_fields, pilot_rows = _read_csv(_PILOT_PATH)
    mapped = _bin_rows(testpoint_rows)

    changed_testpoints = 0
    for bin_id, update in _UPDATES.items():
        row = testpoint_rows[mapped[bin_id]]
        superseded = _SUPERSEDED_EVIDENCE.get(bin_id)
        if superseded and superseded in row["evidence"]:
            row["evidence"] = row["evidence"].replace(
                f"{superseded}; ", ""
            ).replace(superseded, "")
            changed_testpoints += 1
        desired = {
            "五级测试点": update.leaf,
            "Condition": update.condition,
            "Checkpoint": update.checkpoint,
            "Object": update.observation,
        }
        if update.status is not None:
            # Runtime backannotation may promote a modeled leaf to HIT.  The
            # contract refresh must not erase that accepted evidence, while
            # design-review dispositions such as PARTIAL/BLOCKED remain
            # authoritative.
            if not (update.status == "MODELED" and row["status"] == "HIT"):
                desired["status"] = update.status
        for field, value in desired.items():
            if row[field] != value:
                row[field] = value
                changed_testpoints += 1
        if update.evidence:
            if update.evidence not in row["evidence"]:
                row["evidence"] = "; ".join(
                    part for part in (update.evidence, row["evidence"].strip()) if part
                )
                changed_testpoints += 1

    paths = _mapped_paths(testpoint_rows)
    pilot_by_id = {row["Bin_ID"].strip(): row for row in pilot_rows}
    changed_pilot = 0
    for bin_id, update in _UPDATES.items():
        pilot = pilot_by_id.get(bin_id)
        if pilot is None:
            raise ValueError(f"pilot is missing {bin_id}")
        desired = {
            "映射测试点路径": paths[mapped[bin_id]],
            "建议采样事件": update.condition,
            "建议观测对象": update.observation,
            "命中判据": update.checkpoint,
        }
        for field, value in desired.items():
            if pilot[field] != value:
                pilot[field] = value
                changed_pilot += 1

    if not check:
        _write_csv(_TESTPOINT_PATH, testpoint_fields, testpoint_rows)
        _write_csv(_PILOT_PATH, pilot_fields, pilot_rows)
    return {
        "reviewed_bins": len(_UPDATES),
        "testpoint_field_changes": changed_testpoints,
        "pilot_field_changes": changed_pilot,
    }


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()
    result = refresh(check=args.check)
    if args.check and (result["testpoint_field_changes"] or result["pilot_field_changes"]):
        raise SystemExit(f"IFU V3 contract files are stale: {result}")
    print(result)


if __name__ == "__main__":
    main()
