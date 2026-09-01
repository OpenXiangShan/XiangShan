#!/usr/bin/env python3
"""Apply the reviewed Jiabowen IFU testpoint contract for current V3."""

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
_CURRENT_REVIEW = "RTL_REVIEW:e5c70547f"
_SUPERSEDED_EVIDENCE = {
    "BIN-1067": (
        f"{_REVIEW}:PredChecker is a cacheable writeback path; this leaf covers "
        "an older cacheable checker redirect cancelling a younger NC transaction",
        f"{_REVIEW}:PredChecker is cacheable-only; legal V3 ordering resolves the "
        "checker redirect before the younger NC request and restarts NC from the redirect target",
    ),
    "BIN-1084": (
        f"{_REVIEW}:OPEN/FIXME first-page fault bypasses uncache req.fire but "
        "the uncache output branch still selects stale uncachePc"
    ),
    "BIN-1086": (
        f"{_REVIEW}:separates the reachable IAF type contract from blocked "
        "BIN-1084 PC attribution"
    ),
}


def _reviewed_group_updates(
    leaves: dict[str, str],
    *,
    checkpoint: str,
    observation: str,
    review_note: str,
    condition_prefix: str = _CACHEABLE,
) -> dict[str, ContractUpdate]:
    return {
        bin_id: ContractUpdate(
            leaf,
            f"{condition_prefix}；{leaf}",
            f"{checkpoint}；确认：{leaf}",
            observation,
            evidence=f"{_CURRENT_REVIEW}:{review_note}",
        )
        for bin_id, leaf in leaves.items()
    }


_UPDATES = {
    **_reviewed_group_updates(
        {
            "BIN-807": "聚合响应仅info(0).valid时建立单块窗口",
            "BIN-808": "聚合响应info(0/1)均有效时建立双块窗口",
            "BIN-828": "跨cacheline聚合响应保留info(1)身份、raw data和offset",
            "BIN-829": "fetch size变化时顶层firstRange/totalRange保持同一坐标",
            "BIN-902": "顶层firstRange边界正确生成raw blockSel",
        },
        checkpoint=(
            "聚合response以顶层firstRange/totalRange/maybeRvcMap建立统一坐标，"
            "info(0/1)身份、raw data和meta保持同事务；raw blockSel仅表示起始halfword"
        ),
        observation=(
            "ICache→IFU req.valid/ready、bits.info(0/1)、顶层firstRange/totalRange/"
            "maybeRvcMap；IFU s0_fire、s1 fetchBlock/range/raw blockSel"
        ),
        review_note=(
            "#6220 aggregate MainPipeToIfuReq replaces per-block range/map and selectBlock"
        ),
    ),
    **_reviewed_group_updates(
        {
            "BIN-844": "全RVI窗口按真实4B边界形成raw instruction起点",
            "BIN-845": "全RVC窗口按真实2B边界形成raw instruction起点",
            "BIN-846": "RVC/RVI混合窗口按maybeRvcMap形成真实指令边界",
            "BIN-847": "RVI高halfword即使编码像RVC也不形成新指令起点",
            "BIN-911": "聚合maybeRvcMap按窗口起点对齐到统一fetch坐标",
            "BIN-912": "firstRange/totalRange仅允许预测窗口内的指令起点",
            "BIN-913": "s0_prevEndIsHalfRvi使窗口首halfword用于补齐上一条RVI",
            "BIN-914": "firstRange限制第一fetch block的末尾有效边界",
            "BIN-915": "totalRange限制taken预测后的整体有效边界",
            "BIN-916": "taken offset落在真实指令起点时映射到压缩后predTakenIdx",
            "BIN-917": "taken offset指向范围末端RVI低半时形成invalidTaken",
            "BIN-918": "第二块taken offset结合第一块size映射到统一坐标",
            "BIN-919": "Not-Taken窗口无CFI时不产生有效predTakenIdx",
        },
        checkpoint=(
            "聚合maybeRvcMap、firstRange和totalRange以同一fetch坐标进入InstrBoundary；"
            "raw instruction边界、end mask、invalidTaken及predTakenIdx保持同一事务"
        ),
        observation=(
            "ICache→IFU聚合info(0/1)/maybeRvcMap/firstRange/totalRange、s0_fire、"
            "InstrBoundary rawInstrVec/instrEndMask/firstEndIsHalfRvi/totalEndIsHalfRvi及s1_invalidTaken/predTakenIdx"
        ),
        review_note="#6220 moves maybeRvc alignment into ICache and uses top-level aggregate ranges",
    ),
    **_reviewed_group_updates(
        {
            "BIN-860": "下一窗口首halfword与s1_prevEndHalfRviInfo.bits.data拼成完整RVI",
            "BIN-861": "恢复RVI的PC使用s1_prevEndHalfRviInfo.bits.pc",
            "BIN-863": "窗口尾部半条RVI原子保存为Valid EndHalfRviInfo",
            "BIN-920": "backend redirect清除半条RVI valid且旧bits不可复用",
            "BIN-921": "checker redirect通过wbRedirect.halfRviInfo恢复半条RVI状态",
            "BIN-922": "uncacheRedirect.halfRviInfo不污染后续cacheable半条状态",
        },
        checkpoint=(
            "Valid[EndHalfRviInfo]的valid、bits.pc和bits.data原子保存、恢复或清除；"
            "invalid valid不得以零PC/data替代"
        ),
        observation=(
            "s0_prevEndIsHalfRvi、s1_prevEndHalfRviInfo、s1_first/totalEndHalfRvi、"
            "wbRedirect.halfRviInfo、uncacheRedirect.halfRviInfo及恢复后的instruction/PC"
        ),
        review_note=(
            "#6220 replaces split half-RVI PC/data and isHalfInstr with Valid EndHalfRviInfo"
        ),
    ),
    **_reviewed_group_updates(
        {
            "BIN-858": "窗口首halfword使用s1_prevEndHalfRviInfo补齐上一窗口RVI",
            "BIN-859": "窗口尾部RVI低半原子形成s1_totalEndHalfRvi",
            "BIN-862": "窗口起点为上一RVI高半时使用s1_prevEndHalfRviInfo而非解码新RVC",
            "BIN-866": "预测块尾部RVI低半原子保存为Valid EndHalfRviInfo",
            "BIN-867": "下一预测块首halfword与保存的EndHalfRviInfo拼成一条RVI",
            "BIN-960": "flush或redirect清除半条RVI valid且旧bits不得复用",
        },
        checkpoint=(
            "半条状态以Valid[EndHalfRviInfo]的valid、bits.pc和bits.data原子传递；"
            "补齐后的instruction/PC来自同一保存状态，flush后旧bits不可见"
        ),
        observation=(
            "s0_prevEndIsHalfRvi、s1_prevEndHalfRviInfo.valid/bits.pc/bits.data、"
            "s1_firstEndHalfRvi/s1_totalEndHalfRvi、s2注册instruction/PC及IBuffer输出"
        ),
        review_note="#6220 makes half-RVI state atomic and moves stitching into s1",
    ),
    **_reviewed_group_updates(
        {
            "BIN-854": "s1_prevIBufEnqPtrDup为0时从IBuffer第0槽开始对齐",
            "BIN-855": "s1_prevIBufEnqPtrDup非0时按指针低位平移输出槽位",
            "BIN-857": "s1_fire后prevIBufEnqPtr按当前事务指令数更新",
            "BIN-864": "上一窗口半条RVI补齐后占用当前align head槽位",
            "BIN-876": "s1对齐的valid、PC、data和predecode同槽注册到s2",
            "BIN-923": "flush或redirect后prevIBufEnqPtr按当前redirect语义恢复或清零",
        },
        checkpoint=(
            "s1按prevIBufEnqPtrDup对raw slots对齐并把valid/PC/data/predecode同事务注册到s2；"
            "fire、stall和redirect不得造成槽位或事务串拍"
        ),
        observation=(
            "s1_prevIBufEnqPtrDup、s1_alignedInstrValid/InstrVec/InstrPcVec/PdInfoVec、"
            "s2对应注册字段、s1_fire/s2_fire和backend/wb/uncache redirect"
        ),
        review_note="#6220 moves alignment/predecode work to s1 and registers the complete transaction into s2",
    ),
    "BIN-824": ContractUpdate(
        "invalidTaken与取指异常同拍时异常优先且仅交付一条",
        "第一块takenCfiOffset指向范围末端RVI低半并形成s1_invalidTaken(0)；同一事务存在PF、AF或parity/ECC异常；无flush抢占",
        "s1同时观测invalidTaken和fetch exception时将s1_instrCount固定为1；toIBuffer fire仅一个enqEnable/exceptionMask槽有效且不覆盖IBuffer旧项",
        "s1_valid/invalidTaken/icacheMeta.exception/instrCount/prevIBufEnqPtrDup、toIBuffer valid/enqEnable/exceptionMask及IBuffer enqPtr/旧有效项",
        evidence=f"{_CURRENT_REVIEW}:#6220 current alignment pointer is s1_prevIBufEnqPtrDup",
    ),
    "BIN-856": ContractUpdate(
        "最大32条指令叠加最大对齐偏移时不截断",
        f"{_CACHEABLE}；s1_instrCount=32且s1_prevIBufEnqPtrDup低2位为3",
        "32个有效指令完整注册并落在toIBuffer槽3到34，末槽保持无效且无指令被截断",
        "s1_prevIBufEnqPtrDup/s1_instrCount/s1_alignedInstrValid、s2_alignShiftNum/s2_instrCount及toIBuffer valid/enqEnable",
        evidence=f"{_CURRENT_REVIEW}:#6220 alignment is computed in s1 and consumed from registered s2 fields",
    ),
    **_reviewed_group_updates(
        {
            "BIN-848": "raw instruction valid仅压缩真实指令起点",
            "BIN-849": "raw blockSel保留起始块且effective owner显式包含跨块RVI",
            "BIN-850": "跨双块压缩后保留原始start/end offset",
            "BIN-851": "有效指令压缩到连续IBuffer槽位且无空洞",
            "BIN-870": "第一块raw data按统一index取出RVC/RVI",
            "BIN-871": "第二块raw data按index取数且身份不串项",
            "BIN-872": "第一块尾部RVI使用两块raw data拼接并归第二块owner",
            "BIN-873": "第一块尾部RVC不消耗第二块halfword",
            "BIN-875": "instrCountBeforeCurrent与压缩后slot一致",
        },
        checkpoint=(
            "s1按index取数/拼接并保持raw blockSel、isCrossBlockInstr、PC、offset和"
            "predecode同slot；IBuffer/FTQ effective owner为两者OR"
        ),
        observation=(
            "s1 raw cache-line data/index、s1/s2 alignedInstrVec.blockSel/"
            "isCrossBlockInstr/data/PC/offset、s2注册predecode、IBuffer ftqPtr/enqEnable"
        ),
        review_note=(
            "#6220 moves raw-data extraction and predecode to s1 and requires explicit effective owner"
        ),
    ),
    **_reviewed_group_updates(
        {
            "BIN-930": "全not-taken且无结束型CFI时不产生checker redirect",
            "BIN-931": "taken预测命中真实Branch起点时不产生remask fault",
            "BIN-932": "taken预测命中真实JAL/JALR/RET起点时不产生remask fault",
            "BIN-935": "taken指向范围末端RVI低半字时产生invalidTaken",
            "BIN-936": "JAL/JALR/CALL/RET未被正确预测时产生对应fault",
            "BIN-937": "多个预测错误按最早指令选择checker redirect",
            "BIN-938": "checker fault之前的合法指令仍可交付",
            "BIN-939": "checker fault之后的指令不得继续交付",
            "BIN-940": "invalidTaken恢复保留所需halfRviInfo",
            "BIN-941": "checkerRedirect同拍携带PC、target、endOffset及双重block身份",
        },
        checkpoint=(
            "PredChecker消费同一s2注册事务；redirect保留raw blockSel和"
            "isCrossBlockInstr，IBuffer/FTQ归属使用effective owner，payload不串拍"
        ),
        observation=(
            "s2 registered instruction/PC/predecode/jumpOffset、fixedInstrValid/fixedTaken、"
            "checkerRedirect.blockSel/isCrossBlockInstr/mispredPc/target/endOffset/attribute"
        ),
        review_note=(
            "#6220/#6354 registered PredChecker payload uses raw blockSel plus isCrossBlockInstr"
        ),
    ),
    "BIN-897": ContractUpdate(
        "checkerRedirect携带raw blockSel、isCrossBlockInstr、mispredPc、endOffset、isRVC和invalidTaken",
        "预测错误覆盖第一块、第二块及raw blockSel=0的跨块RVI位置",
        "所有redirect字段来自同一s2注册事务；FTQ归属按blockSel OR isCrossBlockInstr计算，raw blockSel仍保留起始块语义",
        "checkerRedirect.blockSel/isCrossBlockInstr/mispredPc/endOffset/isRVC/invalidTaken、toFtq.wbRedirect.ftqIdx",
        evidence=f"{_CURRENT_REVIEW}:#6220 effective owner is raw blockSel OR isCrossBlockInstr",
    ),
    "BIN-946": ContractUpdate(
        "checkerRedirect有效归属为第一fetch block时写回第一FTQ entry",
        f"{_CACHEABLE}；checkerRedirect.blockSel=0且isCrossBlockInstr=0",
        "toFtq.wbRedirect选择第一块ftqIdx/start PC；halfRviInfo仍按raw blockSel选择边界状态",
        "checkerRedirect.blockSel/isCrossBlockInstr、两块ftqIdx/startVAddr、toFtq.wbRedirect、wbRedirect.halfRviInfo",
        evidence=f"{_CURRENT_REVIEW}:#6220 separates effective FTQ owner from raw half-state selection",
    ),
    "BIN-947": ContractUpdate(
        "checkerRedirect有效归属为第二fetch block时写回第二FTQ entry",
        f"{_CACHEABLE}；checkerRedirect.blockSel=1或isCrossBlockInstr=1",
        "toFtq.wbRedirect按blockSel OR isCrossBlockInstr选择第二块ftqIdx/start PC；跨块RVI的halfRviInfo选择仍使用raw blockSel",
        "checkerRedirect.blockSel/isCrossBlockInstr、两块ftqIdx/startVAddr、toFtq.wbRedirect、wbRedirect.halfRviInfo",
        evidence=f"{_CURRENT_REVIEW}:#6220 cross-block RVI has raw blockSel=0 and effective owner=1",
    ),
    "BIN-427": ContractUpdate(
        "双fetch输出raw blockSel与effective owner均正确",
        "输入来自两个fetch block/cacheline，包含普通第二块指令及raw blockSel=0的跨块RVI",
        "raw blockSel仅表示起始halfword所在块；effective owner按blockSel OR isCrossBlockInstr计算，toIBuffer ftqPtr与entry-last归属一致",
        "s2_alignedInstrVec.blockSel/isCrossBlockInstr、两块ftqIdx、toIBuffer ftqPtr/isLastInFtqEntry及instruction/PC",
        evidence=f"{_CURRENT_REVIEW}:#6220 separates raw source selection from effective IBuffer/FTQ ownership",
    ),
    **_reviewed_group_updates(
        {
            "BIN-944": "toFtq.wbRedirect.ftqOffset与同一checkerRedirect.endOffset一致",
            "BIN-945": "checkerRedirect写回保持正确ftqIdx、PC、target和CFI属性",
            "BIN-948": "invalidTaken仅在所选边界有效时保存wbRedirect.halfRviInfo",
            "BIN-949": "backend redirect与checkerRedirect竞争时backend清理优先",
            "BIN-950": "wbRedirect产生时清理s0/s1/s2旧事务及旧halfRviInfo",
            "BIN-951": "backend redirect清理s0/s1/s2 valid及旧halfRviInfo",
            "BIN-953": "reset后halfRviInfo、prevIBufEnqPtr和流水valid均为初始值",
        },
        checkpoint=(
            "checkerRedirect、toFtq.wbRedirect和wbRedirect来自同一注册事务；"
            "ftqIdx/PC/target/endOffset/attribute一致，halfRviInfo原子保存且flush后无旧副作用"
        ),
        observation=(
            "wbValid、checkerRedirect.blockSel/isCrossBlockInstr/endOffset/target/attribute、"
            "toFtq.wbRedirect、wbRedirect.halfRviInfo、s0/s1/s2 valid/flush和prevIBufEnqPtr"
        ),
        review_note="#6220 registers redirect payload identity and replaces split wb half-PC/data state",
    ),
    **_reviewed_group_updates(
        {
            "BIN-976": "跨预测块RVI-JAL由注册PC/predecode识别并归属起始PC",
            "BIN-982": "跨预测块RVI-JALR由注册PC/predecode识别并归属起始PC",
        },
        checkpoint=(
            "s2注册instruction/PC/predecode/jumpOffset保持同一跨块RVI身份；"
            "对应JAL/JALR fault和checkerRedirect payload归属RVI起始PC"
        ),
        observation=(
            "s2_alignedInstrVec.blockSel/isCrossBlockInstr、s2_alignedInstrPcVec/"
            "PdInfoVec/JumpOffsetVec、JAL/JALR fault及checkerRedirect"
        ),
        review_note="#6220 moves cross-block stitching/predecode into s1 before registered s2 checking",
    ),
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
        "路径接受/交付事件、PC、halfRviInfo.valid/bits.pc/bits.data、PBMT/PMP属性、WaitLastCommit、redirect",
        status="MODELED",
        evidence=f"{_CURRENT_REVIEW}:sequential path transitions use atomic halfRviInfo and replace illegal mixed dual-block input",
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
    **_reviewed_group_updates(
        {
            "BIN-1037": "页尾RVI needResend触发uncacheRedirect.halfRviInfo.valid",
            "BIN-1038": "uncacheRedirect.halfRviInfo.bits.pc保持RVI起始PC",
            "BIN-1039": "uncacheRedirect.halfRviInfo.bits.data保持第一页低halfword",
            "BIN-1040": "跨页补半请求不等待前一半指令commit而形成顺序死锁",
            "BIN-1041": "下一页补半成功后只拼成并交付一条完整RVI",
            "BIN-1042": "页尾RVC不触发needResend或halfRviInfo.valid",
            "BIN-1043": "页尾RVC不因保守incomplete进入补半等待",
            "BIN-1044": "第一页低半不可执行时异常身份归属RVI起始PC",
            "BIN-1045": "第二页补半PF/AF/GPF仍归属原RVI起始PC",
            "BIN-1046": "MMIO返回RVC时仅交付一条RVC且PC按2B推进",
            "BIN-1047": "MMIO返回RVI时仅交付一条RVI且PC按4B推进",
            "BIN-1048": "跨8B beat的RVI由InstrUncache补齐后再进入IFU注册译码",
        },
        checkpoint=(
            "MMIO跨页/跨beat事务以uncacheRedirect.halfRviInfo原子保存valid/PC/data；"
            "补齐、异常、stall或redirect后输出instruction/PC/exception和事务身份一致且无旧状态泄漏"
        ),
        observation=(
            "s2 MMIO属性及事务身份、IfuUncache/InstrUncache req/resp/needResend、"
            "uncacheRedirect.halfRviInfo、s1_prevEndHalfRviInfo、toIBuffer和toFtq.wbRedirect"
        ),
        review_note="#6220 replaces split uncache half-PC/data with atomic EndHalfRviInfo",
        condition_prefix="合法MMIO取指事务",
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
        "较老cacheable PredChecker redirect清理紧随其后的年轻NC事务并仅允许恢复路径新NC请求",
        "cacheable块A产生checkerRedirect/wbRedirect；独立且更年轻、ftqIdx不同的PBMT.NC后继B在redirect到来时已真实位于IFU s1、s2或IfuUncacheUnit内部请求入口；无backend redirect干扰",
        "A的wbRedirect清理B对应流水valid；若B的uncacheUnit.req.fire与flush同拍则flush获胜；旧B不产生InstrUncache请求、TL A请求、uncache响应或IBuffer交付；恢复后仅身份不同的新NC可请求并正常交付",
        "A的wb ftqIdx/checkerRedirect/toFtq.wbRedirect；B的s1/s2 valid、PBMT/PMP、ftqIdx/PC/物理地址、s2_wbNotFlush、uncacheUnit req.fire/flush；InstrUncache请求、TL A、响应、IBuffer身份；恢复NC身份",
        status="PARTIAL",
        evidence=f"{_REVIEW}:RTL permits a different-ftqIdx younger NC in s1 or s2 when checker redirect arrives; s1 is flushed unconditionally, s2 is flushed unless s2_wbNotFlush, and IfuUncacheUnit flush overrides same-cycle Idle request capture; existing 91b6219d3 evidence only proves post-redirect recovery",
    ),
    "BIN-1083": ContractUpdate(
        "NC跨页补半后通过原子halfRviInfo完成一条RVI交付",
        "合法PBMT.NC页尾RVI事务产生needResend并顺序补取下一页",
        "uncacheRedirect.halfRviInfo的valid/PC/data绑定原事务；下一页返回后只交付一条完整RVI且旧状态不泄漏",
        "PBMT.NC属性、IfuUncache/InstrUncache req/resp、uncacheRedirect.halfRviInfo、s1_prevEndHalfRviInfo及toIBuffer instruction/PC",
        evidence=f"{_CURRENT_REVIEW}:#6220 makes NC cross-page half-RVI state atomic",
    ),
    "BIN-1084": ContractUpdate(
        "NC第一页权限异常不发InstrUncache请求且保留后端异常身份",
        "PBMT.NC事务在ITLB/PMP权限检查产生PF/AF；异常发生在InstrUncache请求发出前，页尾2B场景作为directed witness",
        "s2_reqIsUncache=1、s2_useUncacheFetch=0且不发出IfuUncache/InstrUncache/TL请求；异常通过toIBuffer交付，exceptionType与ftqPtr/ftqOffset保留当前FTQ事务身份；cfVec.pc为debug-only，不要求等于NC VA",
        "PBMT/权限异常、s2_reqIsUncache、s2_useUncacheFetch、IfuUncache/InstrUncache/TL请求、toIBuffer exceptionType/ftqPtr/ftqOffset；后端FTQ→Backend PC memory路径",
        status="MODELED",
        evidence=f"{_REVIEW}:cfVec.pc is debug-only; functional exception identity uses exceptionVec plus ftqPtr/ftqOffset and Backend FTQ PC memory",
    ),
    "BIN-1086": ContractUpdate(
        "NC页尾第一页不可执行时按Instruction Access Fault交付",
        "PBMT.NC请求起始PC位于4K页尾2B，第一页PMP execute=0且第二页可执行；权限异常发生在uncache请求发出前",
        "s2_reqIsUncache=1、s2_useUncacheFetch=0且s2异常为AF；toIBuffer实际交付AF；cfVec.pc debug值不作为功能判据",
        "PBMT.NC/PMP属性、s2起始PC、s2_reqIsUncache、s2_useUncacheFetch、toIBuffer valid/ready/exceptionType",
        status="MODELED",
        evidence=f"{_REVIEW}:first-page IAF is a functional exception contract; cfVec.pc is debug-only",
    ),
    **_reviewed_group_updates(
        {
            "BIN-1119": "needResend触发uncacheRedirect并原子保存halfRviInfo",
            "BIN-1120": "IFU保存halfRviInfo后以新事务补取下一页",
            "BIN-1122": "跨页补半期间backend redirect清除halfRviInfo且旧半条不泄漏",
        },
        checkpoint=(
            "uncacheRedirect以Valid[EndHalfRviInfo]原子携带valid、起始PC和低halfword；"
            "补取或backend redirect后请求、IBuffer输出及旧状态副作用符合当前事务身份"
        ),
        observation=(
            "InstrUncache needResend、uncacheRedirect.halfRviInfo、s0_prevEndIsHalfRvi、"
            "s1_prevEndHalfRviInfo、下一页请求、backend redirect及toIBuffer instruction/PC"
        ),
        review_note="#6220 replaces IFU split half-PC/data redirect state with atomic EndHalfRviInfo",
        condition_prefix="合法MMIO或PBMT.NC跨页RVI事务",
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
        superseded_items = (
            superseded if isinstance(superseded, tuple) else (superseded,)
        )
        for stale_evidence in superseded_items:
            if stale_evidence and stale_evidence in row["evidence"]:
                row["evidence"] = row["evidence"].replace(
                    f"{stale_evidence}; ", ""
                ).replace(stale_evidence, "")
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
            strengthened_bin1067_hit = (
                bin_id == "BIN-1067"
                and row["status"] == "HIT"
                and "DUT:test_cacheable_checker_redirect_flushes_younger_nc_internal_request:hits="
                in row["evidence"]
            )
            if not (
                (update.status == "MODELED" and row["status"] == "HIT")
                or strengthened_bin1067_hit
            ):
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
