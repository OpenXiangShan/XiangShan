package xiangshan.backend.vector.Decoder

import chisel3._
import xiangshan.backend.vector.Decoder.Select.ChannelUopSelectUtil.genUopNumPatterns
import xiangshan.backend.vector.HasVectorSettings

import scala.collection.mutable.ArrayBuffer

class MopSelectDecoder(
  mopWidth: Int,
  uopWidth: Int,
  uopBufferSize: Int,
  numM2M4M8Channel: (Int, Int, Int),
) extends Module with HasVectorSettings {
  val MaxM2UopIdx = numM2M4M8Channel._1
  val MaxM4UopIdx = numM2M4M8Channel._2
  val MaxM8UopIdx = numM2M4M8Channel._3

  val in = IO(Input(new Bundle {
    val uopBufferNum = UInt(3.W)
    val channelUopNum = Vec(mopWidth, NumUopOH())
  }))
  val out = IO(Output(new Bundle {
    val uopBufferNum = UInt(3.W)
    val acceptVec = Vec(mopWidth, Bool())
  }))

  val alluops: ArrayBuffer[ArrayBuffer[ArrayBuffer[Int]]] = genUopNumPatterns(mopWidth, numM2M4M8Channel)


}
