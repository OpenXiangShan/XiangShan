package xiangshan.backend.fu.vector.utils

import chisel3._

class VecDataSplitIO(inDataWidth: Int, outDataWidth: Int) extends Bundle {
  val inVecData = Input(UInt(inDataWidth.W))
  val outVec8b  = Output(Vec(inDataWidth /  8, UInt(outDataWidth.W)))
  val outVec16b = Output(Vec(inDataWidth / 16, UInt(outDataWidth.W)))
  val outVec32b = Output(Vec(inDataWidth / 32, UInt(outDataWidth.W)))
  val outVec64b = Output(Vec(inDataWidth / 64, UInt(outDataWidth.W)))
}

class VecDataSplitModule(inDataWidth: Int, outDataWidth: Int) extends Module {
  val io = IO(new VecDataSplitIO(inDataWidth, outDataWidth))

  private val inData = io.inVecData
  private val vec8b  = Wire(Vec(inDataWidth /  8, UInt( 8.W)))
  private val vec16b = Wire(Vec(inDataWidth / 16, UInt(16.W)))
  private val vec32b = Wire(Vec(inDataWidth / 32, UInt(32.W)))
  private val vec64b = Wire(Vec(inDataWidth / 64, UInt(64.W)))

  vec8b  := inData.asTypeOf(vec8b)
  vec16b := inData.asTypeOf(vec16b)
  vec32b := inData.asTypeOf(vec32b)
  vec64b := inData.asTypeOf(vec64b)

  io.outVec64b.zip(vec64b).foreach { case(sink, source) => sink := source }
  io.outVec32b.zip(vec32b).foreach { case(sink, source) => sink := source }
  io.outVec16b.zip(vec16b).foreach { case(sink, source) => sink := source }
  io.outVec8b .zip(vec8b) .foreach { case(sink, source) => sink := source }
}
