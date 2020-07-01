package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import xiangshan.{ExuOutput, XSModule}

class Wbu(wbIntIdx: Array[Int], wbFpIdx: Array[Int]) extends XSModule{

  val io = IO(new Bundle() {
    val in  = Vec(exuConfig.ExuCnt, Flipped(DecoupledIO(new ExuOutput)))
    val toRoq = Vec(exuConfig.ExuCnt, ValidIO(new ExuOutput))
    val toIntRf, toFpRf = Vec(NRWritePorts, ValidIO(new ExuOutput))
  })

  val intArb = Module(new WriteBackArbMtoN(wbIntIdx.size, NRWritePorts))
  val fpArb = Module(new WriteBackArbMtoN(wbFpIdx.size, NRWritePorts))

  def exuOutToRfReq
  (exuOutVec: Vec[DecoupledIO[ExuOutput]], fp: Boolean): Seq[(DecoupledIO[ExuOutput], Int)] = {
    val wbIdxSet = if(fp) wbFpIdx else wbIntIdx
    exuOutVec.zipWithIndex.filter(x => wbIdxSet.contains(x._2)).map({
      case(exuOut, idx) =>
        val req = Wire(Decoupled(new ExuOutput))
        req.valid := exuOut.valid && {if(fp) exuOut.bits.uop.ctrl.fpWen else exuOut.bits.uop.ctrl.rfWen}
        req.bits := exuOut.bits
        (req, idx)
    })
  }

  val wbIntReq = exuOutToRfReq(io.in, fp = false)

  val wbFpReq = exuOutToRfReq(io.in, fp = true)

  for(i <- io.in.indices){
    val intReqIdx = wbIntReq.map(_._2).indexOf(i)
    val fpReqIdx = wbFpReq.map(_._2).indexOf(i)

    val wbInt = intReqIdx >= 0
    val wbFp = fpReqIdx >= 0

    val iReq = if(wbInt) wbIntReq(intReqIdx)._1 else null
    val fReq = if(wbFp) wbFpReq(fpReqIdx)._1 else null

    if(wbInt && wbFp){
      io.in(i).ready := Mux(iReq.valid,
        iReq.ready,
        Mux(fReq.valid,
          fReq.ready,
          true.B
        )
      )
      assert(!(iReq.valid && fReq.valid), s"Error: iReq and fReq valid at same time, idx=$i")
    } else if(wbInt){
      io.in(i).ready := Mux(iReq.valid, iReq.ready, true.B)
    } else if(wbFp){
      io.in(i).ready := Mux(fReq.valid, fReq.ready, true.B)
    } else {
      assert(cond = false, s"Error: Found a input wb nothing! idx=$i")
    }
  }

  intArb.io.in <> wbIntReq.map(_._1)
  io.toIntRf <> intArb.io.out

  fpArb.io.in <> wbFpReq.map(_._1)
  io.toFpRf <> fpArb.io.out

  io.toRoq.zip(io.in).foreach({
    case(roq, in) =>
      roq.valid := in.fire() && !in.bits.redirectValid
      roq.bits := in.bits
  })

}
