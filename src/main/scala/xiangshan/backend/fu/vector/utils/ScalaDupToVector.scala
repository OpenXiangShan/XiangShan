package xiangshan.backend.fu.vector.utils

import chisel3._
import chisel3.util._
import xiangshan.backend.fu.vector.Bundles.VSew

class ScalaDupToVectorIO(vlen: Int) extends Bundle {
  val in = Input(new Bundle {
    val scalaData = UInt(64.W) // scala data would no more than 64 bits
    val vsew = VSew()           // 0: 8bits, 1: 16bits, 2: 32bits, 3: 64bits
  })
  val out = Output(new Bundle {
    val vecData = UInt(vlen.W)
  })
}

class ScalaDupToVector(vlen: Int) extends Module {
  val io = IO(new ScalaDupToVectorIO(vlen))

  private val scalaData = io.in.scalaData
  private val vsew = io.in.vsew

  private val vecE8Data  = Wire(Vec(vlen /  8, UInt( 8.W)))
  private val vecE16Data = Wire(Vec(vlen / 16, UInt(16.W)))
  private val vecE32Data = Wire(Vec(vlen / 32, UInt(32.W)))
  private val vecE64Data = Wire(Vec(vlen / 64, UInt(64.W)))

  vecE8Data   := VecInit(Seq.fill(vlen /  8)(scalaData( 7, 0)))
  vecE16Data  := VecInit(Seq.fill(vlen / 16)(scalaData(15, 0)))
  vecE32Data  := VecInit(Seq.fill(vlen / 32)(scalaData(31, 0)))
  vecE64Data  := VecInit(Seq.fill(vlen / 64)(scalaData(63, 0)))

  io.out.vecData := Mux1H(Seq(
    (vsew === VSew.e8)  -> vecE8Data.asUInt,
    (vsew === VSew.e16) -> vecE16Data.asUInt,
    (vsew === VSew.e32) -> vecE32Data.asUInt,
    (vsew === VSew.e64) -> vecE64Data.asUInt,
  ))
}
