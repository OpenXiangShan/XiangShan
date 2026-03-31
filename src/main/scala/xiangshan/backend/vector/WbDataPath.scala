package xiangshan.backend.vector

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.XSBundle
import xiangshan.backend.regfile.PregParams
import xiangshan.backend.vector.VecRegionModule.RfWriteBundle

class WbDataPath(pregParams: PregParams)(implicit p: Parameters) extends Module {
  val in = IO(Input(new WbDataPath.In(pregParams)))
  val out = IO(Output(new WbDataPath.Out(pregParams)))

  private val toRfs: Seq[Exu.ToRf] = in.fromExus.flatten.flatten

  private val groupedExuToRf: Map[Int, Seq[Exu.ToRf]] = toRfs.groupBy(_.wbCfg.port)

  for ((toRf, i) <- out.toRf.zipWithIndex) {
    toRf.wen := VecInit(groupedExuToRf(i).map(x => x.wen)).asUInt.orR
    toRf.pdest := Mux1H(groupedExuToRf(i).map(x => x.wen -> x.pdest))
    toRf.data := Mux1H(groupedExuToRf(i).map(x => x.wen -> x.data))
  }
}

object WbDataPath {
  class In(pregParams: PregParams)(implicit p: Parameters) extends XSBundle {
    val fromExus: MixedVec[MixedVec[MixedVec[Exu.ToRf]]] = backendParams.genExuToRfBundle(pregParams)
  }

  class Out(pregParams: PregParams)(implicit p: Parameters) extends XSBundle {
    val toRf = Vec(backendParams.getRfWriteSize(pregParams.dataCfg), new RfWriteBundle(pregParams))
  }
}
