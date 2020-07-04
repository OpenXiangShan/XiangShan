package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.utils._

class DecodeBuffer extends XSModule {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val in  = Vec(DecodeWidth, Flipped(DecoupledIO(new CfCtrl)))
    val out = Vec(RenameWidth, DecoupledIO(new CfCtrl))
  })

  require(DecodeWidth == RenameWidth)

  val validVec = RegInit(VecInit(Seq.fill(DecodeWidth)(false.B)))

  val leftCanIn = ParallelAND(
    validVec.zip(io.out.map(_.fire())).map({
      case (v, fire) =>
        !v || fire
    })
  ).asBool()

  val rightRdyVec = io.out.map(_.ready && leftCanIn)

  for( i <- 0 until RenameWidth){
    when(io.out(i).fire()){
      validVec(i) := false.B
    }
    when(io.in(i).fire()){
      validVec(i) := true.B
    }
    when(io.redirect.valid){
      validVec(i) := false.B
    }

    io.in(i).ready := rightRdyVec(i)
    io.out(i).bits <> RegEnable(io.in(i).bits, io.in(i).fire())
    io.out(i).valid := validVec(i) && !io.redirect.valid
  }

  for(in<- io.in){
    XSInfo(p"in v:${in.valid} r:${in.ready} pc=${Hexadecimal(in.bits.cf.pc)}\n")
  }
  for(out <- io.out){
    XSInfo(p"out v:${out.valid} r:${out.ready} pc=${Hexadecimal(out.bits.cf.pc)}\n")
  }

  XSDebug(p"validVec: ${Binary(validVec.asUInt())}\n")
}
