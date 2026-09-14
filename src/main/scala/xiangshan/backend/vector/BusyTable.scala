package xiangshan.backend.vector

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.{XSBundle, XSModule}
import xiangshan.backend.regfile.PregParams

class BusyTable(
  numRead: Int,
  numWrite: Int,
  pregParams: PregParams,
  alwaysReadyFunc: Int => Boolean,
)(implicit p: Parameters) extends XSModule {
  private val addrWidth = pregParams.addrWidth
  private val numPregs = pregParams.numEntries

  require(numRead > 0, s"[Vector BusyTable] numRead must be positive, got ${numRead}")
  require(numWrite > 0, s"[Vector BusyTable] numWrite must be positive, got ${numWrite}")
  require(numPregs > 0, s"[Vector BusyTable] pregParams.numEntries must be positive, got ${numPregs}")
  require(RenameWidth > 0, s"[Vector BusyTable] RenameWidth must be positive, got ${RenameWidth}")

  val in = IO(Input(new BusyTable.In(numRead, numWrite, pregParams)))
  val out = IO(Output(new BusyTable.Out(numRead)))

  private val table = RegInit(0.U(numPregs.W)) // 1 = busy, 0 = ready
  private val alwaysReadyMask = VecInit((0 until numPregs).map(i => alwaysReadyFunc(i).B)).asUInt

  private val allocMask = reqVecToMask(in.allocPregs)
  private val wakeUpMask = reqVecToMask(in.wakeUp)

  assert(!(allocMask & wakeUpMask).orR, "[Vector BusyTable] alloc/wakeup hit same preg in one cycle")

  private val nextTable = ((table | allocMask) & (~wakeUpMask).asUInt) & (~alwaysReadyMask).asUInt
  table := nextTable

  out.readResp.zip(in.readReq).foreach { case (resp, req) =>
    val readBypass = in.allocPregs.map(x => x.valid && x.bits === req).reduce(_ || _)
    val reqAlwaysReady = VecInit((0 until numPregs).map(i => (req === i.U) && alwaysReadyFunc(i).B)).asUInt.orR
    resp := !(table(req) || (readBypass && !reqAlwaysReady))
  }

  private def reqVecToMask(reqVec: Seq[ValidIO[UInt]]): UInt = {
    reqVec.map(v => Mux(v.valid, UIntToOH(v.bits, numPregs), 0.U(numPregs.W))).reduce(_ | _)
  }


}

object BusyTable {
  class In(
    numRead: Int,
    numWrite: Int,
    pregParams: PregParams,
  )(implicit p: Parameters) extends XSBundle {
    private val addrWidth = pregParams.addrWidth

    val allocPregs = Vec(RenameWidth, ValidIO(UInt(addrWidth.W)))

    val wakeUp = Vec(numWrite, Flipped(ValidIO(UInt(addrWidth.W))))
    // read preg state
    val readReq = Vec(numRead, UInt(addrWidth.W))
  }

  class Out(
    numRead: Int,
  )(implicit p: Parameters) extends XSBundle {
    val readResp = Vec(numRead, Bool())
  }
}
