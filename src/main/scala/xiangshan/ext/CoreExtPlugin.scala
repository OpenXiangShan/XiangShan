/***************************************************************************************
* Copyright (c) 2024-2026 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2026 Institute of Computing Technology, Chinese Academy of Sciences
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.ext

import chisel3._
import chisel3.util.{BitPat, DecoupledIO, ValidIO}
import org.chipsalliance.cde.config.{Config, Field, Parameters}
import xiangshan.{FuOpType, Redirect, XSBundle, XSCoreBase, XSCoreImp, XSTile, XSTileKey}
import xiangshan.backend.fu.FuType
import xiangshan.backend.rob.RobPtr

/** Request from in-core ExtProxyFu to core-level extension decoupled unit */
class ExtExecReq(implicit p: Parameters) extends XSBundle {
  val fuOpType = FuOpType()
  val rs1Data  = UInt(XLEN.W)
  val rs2Data  = UInt(XLEN.W)
  val raw25    = UInt(25.W)
  val robIdx   = new RobPtr
  val seqId    = UInt(2.W)
}

/** Response from core-level extension decoupled unit to in-core ExtProxyFu */
class ExtExecResp(implicit p: Parameters) extends XSBundle {
  val rdData       = UInt(XLEN.W)
  val seqId        = UInt(2.W)
  val illegalInstr = Bool()
  val virtualInstr = Bool()
  val accessFault  = Bool()
}

/**
 * Execution-stage interface between in-core ExtProxyFu (initiator) and extension decoupled unit (responder).
 * For asynchronous extension ops, `resp` indicates the core-side execution stage can complete
 * (e.g., enqueued into a pre-commit queue), while engine execution occurs after ROB commit.
 */
class ExtExecIO(implicit p: Parameters) extends XSBundle {
  val req   = DecoupledIO(new ExtExecReq)
  val resp  = Flipped(ValidIO(new ExtExecResp))
  val abort = Output(Bool())

  def tieOffResponder(): Unit = {
    req.ready  := false.B
    resp.valid := false.B
    resp.bits  := 0.U.asTypeOf(resp.bits)
  }
}

/** ROB commit observation bundle for core-level extension units */
class ExtRobCommitIO(implicit p: Parameters) extends XSBundle {
  val isCommit    = Bool()
  val commitValid = Vec(CommitWidth, Bool())
  val robIdx      = Vec(CommitWidth, new RobPtr)
}

/**
 * Aggregated core-level extension interface handed to CoreExtPlugin.onCoreElaborated.
 * Signal directions are from the plugin's perspective.
 */
class CoreExtIO(
  hasExec: Boolean,
)(implicit p: Parameters) extends XSBundle {
  val exec      = Option.when(hasExec)(Flipped(new ExtExecIO))
  val robCommit = Input(new ExtRobCommitIO)
  val redirect  = Input(ValidIO(new Redirect))
  val sfence    = Input(Bool())
  val pagingEn  = Input(Bool())
  val sbIsEmpty = Input(Bool())
  val flushSb   = Output(Bool())

  def tieOffPluginOutputs(): Unit = {
    exec.foreach(_.tieOffResponder())
    flushSb := false.B
  }
}

/** Strongly-typed decode row injected by a CoreExtPlugin into DecodeChannels. */
case class ExtDecodeRow(
  pat:       BitPat,
  fuType:    FuType.OHType,
  fuOpType:  Int,
  src1Ren:   Boolean,
  src2Ren:   Boolean,
  rfWen:     Boolean,
  noSpec:    Boolean,
  blockBack: Boolean,
  flushPipe: Boolean,
)

trait CoreExtPlugin {
  def name: String

  /** Whether this plugin uses the in-core ExtProxyFu execution channel */
  def enableExtExec: Boolean = false

  /** Inject instruction decode truth table rows into DecodeChannels. */
  def decodeTable(implicit p: Parameters): Seq[ExtDecodeRow] = Nil

  /** Lifecycle hook 1: called at the end of XSCoreBase LazyModule body (e.g., creating BundleBridge nodes). */
  def onCoreLazyElaborated(core: XSCoreBase)(implicit p: Parameters): Unit = {}

  /** Lifecycle hook 2: called at the end of XSTile LazyModule body (e.g., attaching engines & BundleBridge/TL nodes). */
  def onTileLazyElaborated(tile: XSTile)(implicit p: Parameters): Unit = {}

  /** Lifecycle hook 3: called at the end of XSCoreImp elaboration with CoreExtIO. */
  def onCoreElaborated(core: XSCoreImp, extIO: CoreExtIO)(implicit p: Parameters): Unit = {}

  /** Lifecycle hook 4: called at the end of XSTileImp elaboration (e.g., connecting Tile-level engines). */
  def onTileImpElaborated(tile: XSTile#XSTileImp)(implicit p: Parameters): Unit = {}
}

case object CoreExtPluginsKey extends Field[Seq[CoreExtPlugin]](Nil)

/** Composable CDE Config mixin that registers a CoreExtPlugin and enables HasExtExec when required. */
class WithCoreExtPlugin(plugin: CoreExtPlugin) extends Config((_, _, up) => {
  case CoreExtPluginsKey => up(CoreExtPluginsKey) :+ plugin
  case XSTileKey => up(XSTileKey).map { tp =>
    tp.copy(HasExtExec = tp.HasExtExec || plugin.enableExtExec)
  }
})

object ExtDecodeHelper {
  import chisel3.util._
  import xiangshan.backend.vector.Decoder.DecodeChannelOutput
  import xiangshan.backend.vector.Decoder.Types.{DecodeSelImm, DecodeSrcType}

  def decode(raw: UInt, rows: Seq[ExtDecodeRow]): ValidIO[DecodeChannelOutput] = {
    val out = Wire(ValidIO(new DecodeChannelOutput))
    if (rows.isEmpty) {
      out.valid := false.B
      out.bits  := 0.U.asTypeOf(new DecodeChannelOutput)
    } else {
      val hits = rows.map(r => raw === r.pat)
      val uop  = WireInit(0.U.asTypeOf(new DecodeChannelOutput))
      uop.fuType         := Mux1H(hits.zip(rows.map(_.fuType.U)))
      uop.opcode         := Mux1H(hits.zip(rows.map(_.fuOpType.U(9.W))))
      uop.src1Ren        := Mux1H(hits.zip(rows.map(_.src1Ren.B)))
      uop.src1Type.value := DecodeSrcType.GP
      uop.src2Ren        := Mux1H(hits.zip(rows.map(_.src2Ren.B)))
      uop.src2Type.value := DecodeSrcType.GP
      uop.lsrc1          := raw(19, 15)
      uop.lsrc2          := raw(24, 20)
      uop.ldest          := raw(11, 7)
      uop.gpWen          := Mux1H(hits.zip(rows.map(_.rfWen.B))) && (raw(11, 7) =/= 0.U)
      uop.noSpec         := Mux1H(hits.zip(rows.map(_.noSpec.B)))
      uop.blockBack      := Mux1H(hits.zip(rows.map(_.blockBack.B)))
      uop.flushPipe      := Mux1H(hits.zip(rows.map(_.flushPipe.B)))
      uop.selImm.valid   := true.B
      // Reuse existing 32-bit pass-through Imm_LUI32 to carry raw inst[31:7] (upper 7 bits stay 0)
      uop.selImm.bits    := DecodeSelImm.LUI32
      uop.imm            := raw(31, 7)
      uop.canRobCompress := false.B
      uop.isFirstUop     := true.B
      uop.isLastUop      := true.B
      out.valid          := VecInit(hits).asUInt.orR
      out.bits           := uop
    }
    out
  }
}

/** Encapsulates XSCoreImp-level CoreExtIO aggregation and per-plugin dispatch. */
object ExtCoreHelper {
  def elaborate(core: XSCoreImp, extFlushSbs: Seq[Bool])(implicit p: Parameters): Unit = {
    val extPlugins = p(CoreExtPluginsKey)
    if (extPlugins.nonEmpty) {
      val extSfence = Wire(Bool())
      extSfence := core.backend.io.mem.sfence.valid ||
        core.backend.io.mem.tlbCsr.satp.changed ||
        core.backend.io.mem.tlbCsr.vsatp.changed ||
        core.backend.io.mem.tlbCsr.hgatp.changed
      val extPagingEn = Wire(Bool())
      extPagingEn := core.backend.io.mem.tlbCsr.satp.mode =/= 0.U ||
        core.backend.io.mem.tlbCsr.vsatp.mode =/= 0.U

      require(
        extPlugins.count(_.enableExtExec) <= 1,
        s"At most one CoreExtPlugin may enable enableExtExec, got: ${extPlugins.filter(_.enableExtExec).map(_.name).mkString(", ")}",
      )

      val execPluginIdxOpt = extPlugins.indexWhere(_.enableExtExec) match {
        case -1 => None
        case i  => Some(i)
      }
      if (execPluginIdxOpt.nonEmpty) {
        require(
          core.backend.io.extExec.nonEmpty,
          "CoreExtPlugin enables enableExtExec, but XSCoreParameters.HasExtExec is false (use WithCoreExtPlugin or set HasExtExec = true)",
        )
      }

      extPlugins.zipWithIndex.foreach { case (plugin, idx) =>
        val hasExec = execPluginIdxOpt.contains(idx) && core.backend.io.extExec.nonEmpty

        val extIO = Wire(new CoreExtIO(
          hasExec = hasExec,
        ))
        extIO.tieOffPluginOutputs()

        extIO.exec.foreach(e => core.backend.io.extExec.get <> e)
        extIO.robCommit  := core.backend.io.extRobCommit.get
        extIO.redirect   := core.backend.io.mem.redirect
        extIO.sfence     := extSfence
        extIO.pagingEn   := extPagingEn
        extIO.sbIsEmpty  := core.memBlock.io.mem_to_ooo.sbIsEmpty
        extFlushSbs(idx) := extIO.flushSb

        plugin.onCoreElaborated(core, extIO)
      }
    }
  }
}
