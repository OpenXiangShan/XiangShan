package xiangshan.backend.vector.Decoder.Select

import chisel3._
import chisel3.util._
import xiangshan.backend.vector.Decoder.RVVDecodeUtil
import xiangshan.backend.vector.Decoder.RVVDecodeUtil.UopNumOHsPattern
import xiangshan.backend.vector.Decoder.Select.ChannelUopSelectUtil.{genUopBufferPort2, genUopNumPatterns2}
import xiangshan.backend.vector.Decoder.UopBufferCtrlDecoderUtil.{UopNumPattern, UopNumWithChannelUopNum, UopSelectBundle}
import xiangshan.backend.vector.HasVectorSettings
import xiangshan.backend.vector.util.Verilog

import scala.collection.mutable.ArrayBuffer

class BufferSelectModule[T1 <: Data, T2 <: Data](
  uopBundle      : => T1,
  mopBundle      : => T2,
  mopWidth       : Int,
  uopWidth       : Int,
  uopBufferLength: Int,
  // if isEmpty, use uopBuffer[uopIdx - uopWidth] instead
  filteredPorts  : Seq[Seq[Option[(Int, Int)]]],
) extends Module with HasVectorSettings {

  override def desiredName: String = s"BufferSelectModule_${uopBundle}_${mopBundle}"

  val in = IO(Input(new Bundle {
    val uopFromChannel = Vec(mopWidth, Vec(maxSplitUopNum, uopBundle))
    val uopFromBuffer = Vec(uopBufferLength, uopBundle)
    val mopFromInput = Vec(mopWidth, mopBundle)
    val mopFromBuffer = mopBundle
    val uopSelect = Vec(uopBufferLength, new UopSelectBundle(mopWidth))
  }))
  val out = IO(Output(new Bundle {
    val decodedInfoOut = Vec(uopBufferLength, uopBundle)
    val mopToBuffer = mopBundle
  }))

  val uopSelectBuffer: Seq[Bool] = in.uopSelect.map(_.uopBufferSel)
  val uopSelectChannel: Seq[Vec[Vec[Bool]]] = in.uopSelect.map(_.channelSel)
  val mopSelectBuffer: Seq[Bool] = uopSelectBuffer
  val mopSelect: Seq[Seq[Bool]] = uopSelectChannel.map(_.map(x => Cat(x).orR))

  val filteredUopFromChannel: Seq[Seq[T1]] =
    for (uopIdx <- 0 until uopBufferLength) yield {
      filteredPorts(uopIdx).collect {
        case Some((channelIdx, portIdx)) if portIdx >= 0 => in.uopFromChannel(channelIdx)(portIdx)
      }
    }

  val filteredUopSelectChannel: Seq[Seq[Bool]] = for (uopIdx <- 0 until uopBufferLength) yield {
    filteredPorts(uopIdx).collect {
      case Some((channelIdx, portIdx)) => uopSelectChannel(uopIdx)(channelIdx)(portIdx)
    }
  }

  val filteredMopFromInput: Seq[Seq[T2]] = for (uopIdx <- 0 until uopBufferLength) yield {
    filteredPorts(uopIdx).collect {
      case Some((channelIdx, _)) => in.mopFromInput(channelIdx)
    }
  }

  val filteredMopSelect: Seq[Seq[Bool]] = for (uopIdx <- 0 until uopBufferLength) yield {
    filteredPorts(uopIdx).collect {
      case Some((channelIdx, _)) => mopSelect(uopIdx)(channelIdx)
    }
  }

  for (i <- 0 until uopBufferLength) {
    out.decodedInfoOut(i) :=
      Mux1H(
        filteredUopSelectChannel(i) ++ Option.when(in.uopFromBuffer.isDefinedAt(i + uopWidth))(uopSelectBuffer(i + uopWidth)),
        filteredUopFromChannel(i) ++ Option.when(in.uopFromBuffer.isDefinedAt(i + uopWidth))(in.uopFromBuffer(i + uopWidth)),
      )
    out.mopToBuffer :=
      Mux1H(
        filteredMopSelect(i) ++ Option.when(in.uopFromBuffer.isDefinedAt(i + uopWidth))(mopSelectBuffer(i + uopWidth)),
        filteredMopFromInput(i) ++ Option.when(in.uopFromBuffer.isDefinedAt(i + uopWidth))(in.mopFromBuffer),
      )
  }
}

object BufferSelectModuleMain extends App {
  val mopWidth = 6
  val uopWidth = 6
  val numM2M4M8Channel = (6, 6, 6)
  val uopBufferLength = 7

  val alluops: ArrayBuffer[ArrayBuffer[ArrayBuffer[Int]]] = genUopNumPatterns2(mopWidth, numM2M4M8Channel)

  val patternsForUopSel: Seq[Seq[UopNumWithChannelUopNum]] = alluops.zipWithIndex.map {
    case (uopPatterns: ArrayBuffer[ArrayBuffer[Int]], uopIdx) =>
      uopPatterns.map {
        pattern: ArrayBuffer[Int] =>
          val res: UopNumWithChannelUopNum = UopNumPattern(pattern(0)) ## UopNumOHsPattern(pattern.drop(1).toSeq, mopWidth)
          res
      }.toSeq
  }.toSeq

  val patternsForBufferSel: Seq[UopNumWithChannelUopNum] = patternsForUopSel.last

  // if isEmpty, use uopBuffer[uopIdx - uopWidth] instead
  val bufferUsedPorts: Seq[Seq[Option[(Int, Int)]]] = (0 to uopBufferLength).map {
    uopBufferIdx =>
      patternsForBufferSel.map {
        pattern: UopNumWithChannelUopNum => {
          println(pattern.p1.num, pattern.p2.uopNumSeq, uopBufferIdx)
          val port = genUopBufferPort2(pattern.p1.num, pattern.p2.uopNumSeq, uopWidth, uopBufferIdx)
          port match {
            case Some(value) => println(value)
            case None => println(s"uopBuffer${uopBufferIdx + uopWidth}")
          }
          port
        }
      }.distinct
  }

  bufferUsedPorts.zipWithIndex.map {
    case (ports, uopBufferIdx) =>
      println(s"uopBuffer${uopBufferIdx}")
      ports.map {
        case Some(value) => println(s"from $value")
        case None => println(s"from uopBuffer${uopBufferIdx + uopWidth}")
      }
  }

  Verilog.emitVerilog(
    new BufferSelectModule(
      uopBundle = UInt(1.W),
      mopBundle = UInt(1.W),
      mopWidth = 8,
      uopWidth = 8,
      uopBufferLength = 7,
      filteredPorts = bufferUsedPorts,
    )
  )
}
