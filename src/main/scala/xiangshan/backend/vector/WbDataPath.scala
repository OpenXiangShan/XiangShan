package xiangshan.backend.vector

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.experimental.VecLiterals.AddVecLiteralConstructor
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.{HasXSParameter, XSBundle}
import xiangshan.backend.regfile.PregParams
import xiangshan.backend.vector.VecRegionModule.RfWriteBundle

class WbDataPath(pregParams: PregParams)(implicit val p: Parameters) extends Module with HasXSParameter {
  val in = IO(Input(new WbDataPath.In(pregParams)))
  val out = IO(Output(new WbDataPath.Out(pregParams)))

  private val fromExus: Seq[Exu.ToRf] = in.fromExus.flatten.flatten
  private val wb0, wb1, wb2: Vec[RfWriteBundle] = RegInit(
    VecInit.fill(backendParams.getRfWriteSize(pregParams.dataCfg))(new RfWriteBundle(pregParams).Lit(_.wen -> false.B))
  )

  private val wb0Next = Wire(chiselTypeOf(wb0))

  private val groupedExuToRf: Map[Int, Seq[Exu.ToRf]] = fromExus.groupBy(_.wbCfg.port)

  for ((wbNext, i) <- wb0Next.zipWithIndex) {
    wbNext.wen := VecInit(groupedExuToRf(i).map(x => x.wen)).asUInt.orR
    wbNext.pdest := Mux1H(groupedExuToRf(i).map(x => x.wen -> x.pdest))
    wbNext.data := Mux1H(groupedExuToRf(i).map(x => x.wen -> x.data))
  }

  connectWbWithWbNext(wb0, wb0Next)
  connectWbWithWbNext(wb1, wb0)
  connectWbWithWbNext(wb2, wb1)

  out.wb0 := wb0
  out.wb1 := wb1
  out.wb2 := wb2

  def connectWbWithWbNext(wbVec: Vec[RfWriteBundle], wbNextVec: Vec[RfWriteBundle]): Unit = {
    for ((wb, wbNext) <- wbVec lazyZip wbNextVec) {
      // Todo[gate]: make wen gated
      wb.wen := wbNext.wen
      when (wbNext.wen) {
        wb.pdest := wbNext.pdest
        wb.data := wbNext.data
      }
    }
  }
}

object WbDataPath {
  class In(pregParams: PregParams)(implicit p: Parameters) extends XSBundle {
    val fromExus: MixedVec[MixedVec[MixedVec[Exu.ToRf]]] = backendParams.genExuToRfBundle(pregParams)
  }

  class Out(pregParams: PregParams)(implicit p: Parameters) extends XSBundle {
    val wb0, wb1, wb2 = Vec(backendParams.getRfWriteSize(pregParams.dataCfg), new RfWriteBundle(pregParams))
  }
}
