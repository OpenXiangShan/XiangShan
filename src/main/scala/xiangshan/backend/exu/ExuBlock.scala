package xiangshan.backend.exu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xiangshan.backend.fu.{CSRFileIO, FenceIO}
import xiangshan.backend.Bundles._
import xiangshan.backend.issue.SchdBlockParams
import xiangshan.{HasXSParameter, Redirect, XSBundle}

class ExuBlock(params: SchdBlockParams)(implicit p: Parameters) extends LazyModule with HasXSParameter {
  val exus: Seq[ExeUnit] = params.issueBlockParams.flatMap(_.exuBlockParams.map(x => LazyModule(x.genExuModule)))

  lazy val module = new ExuBlockImp(this)(p, params)
}

class ExuBlockImp(
  override val wrapper: ExuBlock
)(implicit
  p: Parameters,
  params: SchdBlockParams
) extends LazyModuleImp(wrapper) {
  val io = IO(new ExuBlockIO)

  private val exus = wrapper.exus.map(_.module)

  private val ins: collection.IndexedSeq[DecoupledIO[ExuInput]] = io.in.flatten
  private val outs: collection.IndexedSeq[DecoupledIO[ExuOutput]] = io.out.flatten

  (ins zip exus zip outs).foreach { case ((input, exu), output) =>
    exu.io.flush <> io.flush
    exu.io.csrio.foreach(exuio => io.csrio.get <> exuio)
    exu.io.fenceio.foreach(exuio => io.fenceio.get <> exuio)
    exu.io.frm.foreach(exuio => io.frm.get <> exuio)
    exu.io.in <> input
    output <> exu.io.out
  }
}

class ExuBlockIO(implicit p: Parameters, params: SchdBlockParams) extends XSBundle {
  val flush = Flipped(ValidIO(new Redirect))
  // in(i)(j): issueblock(i), exu(j)
  val in: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = Flipped(params.genExuInputBundle)
  // out(i)(j): issueblock(i), exu(j).
  val out: MixedVec[MixedVec[DecoupledIO[ExuOutput]]] = params.genExuOutputDecoupledBundle

  val csrio = if (params.hasCSR) Some(new CSRFileIO) else None
  val fenceio = if (params.hasFence) Some(new FenceIO) else None
  val frm = if (params.needSrcFrm) Some(Input(UInt(3.W))) else None
}