package xiangshan.backend.vector.Decoder.Select

import chisel3._
import chisel3.util._
import xiangshan.backend.vector.Decoder.RVVDecodeUtil.UopNumOHsPattern
import xiangshan.backend.vector.Decoder.Select.ChannelUopSelectUtil.{genDecodeOutPort2, genUopNumPatterns2}
import xiangshan.backend.vector.Decoder.UopBufferCtrlDecoderUtil.{UopNumPattern, UopNumWithChannelUopNum, UopSelectBundle}
import xiangshan.backend.vector.HasVectorSettings
import xiangshan.backend.vector.util.ChiselTypeExt._
import xiangshan.backend.vector.util.Verilog

import scala.collection.mutable.ArrayBuffer

class UopSelectModule[T1 <: Data, T2 <: Data](
  uopBundle    : => T1,
  mopBundle    : => T2,
  mopWidth     : Int,
  uopWidth     : Int,
  uopBufferSize: Int,
  // if isEmpty, use uopBuffer[uopIdx] instead
  filteredPorts: Seq[Seq[Option[(Int, Int)]]],
) extends Module with HasVectorSettings {

  override def desiredName: String = s"UopSelectModule_${uopBundle}_${mopBundle}"

  val in = IO(Input(new Bundle {
    val uopFromChannel = Vec(mopWidth, Vec(maxSplitUopNum, uopBundle))
    val uopFromBuffer = Vec(uopBufferSize, uopBundle)
    val mopFromInput = Vec(mopWidth, mopBundle)
    val mopFromBuffer = mopBundle
    val uopSelect = Vec(uopWidth, new UopSelectBundle(mopWidth))
  }))
  val out = IO(Output(new Bundle {
    val decodedInfoOut = Vec(uopWidth, uopBundle)
    val bypassInfoOut  = Vec(uopWidth, mopBundle)
  }))

  val uopSelectBuffer: Seq[Bool] = in.uopSelect.map(_.uopBufferSel)
  val uopSelectChannel: Seq[Vec[Vec[Bool]]] = in.uopSelect.map(_.channelSel)
  val mopSelectBuffer: Seq[Bool] = uopSelectBuffer
  val mopSelect: Seq[Seq[Bool]] = uopSelectChannel.map(_.map(x => Cat(x).orR))

  val filteredUopFromChannel: Seq[Seq[T1]] =
    for (uopIdx <- 0 until uopWidth) yield {
      filteredPorts(uopIdx).collect {
        case Some((channelIdx, portIdx)) => in.uopFromChannel(channelIdx)(portIdx)
      }
    }

  val filteredUopSelectChannel: Seq[Seq[Bool]] = for (uopIdx <- 0 until uopWidth) yield {
    filteredPorts(uopIdx).collect {
      case Some((channelIdx, portIdx)) => uopSelectChannel(uopIdx)(channelIdx)(portIdx)
    }
  }

  val filteredMopFromInput: Seq[Seq[T2]] = for (uopIdx <- 0 until uopWidth) yield {
    filteredPorts(uopIdx).collect {
      case Some((channelIdx, _)) => in.mopFromInput(channelIdx)
    }
  }

  val filteredMopSelect: Seq[Seq[Bool]] = for (uopIdx <- 0 until uopWidth) yield {
    filteredPorts(uopIdx).collect {
      case Some((channelIdx, _)) => mopSelect(uopIdx)(channelIdx)
    }
  }

  for (i <- 0 until uopWidth) {
    out.decodedInfoOut(i) :=
      Mux1H(
        filteredUopSelectChannel(i) ++ Option.when(in.uopFromBuffer.isDefinedAt(i))(uopSelectBuffer(i)),
        filteredUopFromChannel(i) ++ Option.when(in.uopFromBuffer.isDefinedAt(i))(in.uopFromBuffer(i)),
      )
    out.bypassInfoOut(i) :=
      Mux1H(
        filteredMopSelect(i) ++ Option.when(in.uopFromBuffer.isDefinedAt(i))(mopSelectBuffer(i)),
        filteredMopFromInput(i) ++ Option.when(in.uopFromBuffer.isDefinedAt(i))(in.mopFromBuffer),
      )
  }
}

object UopSelectModuleMain extends App {
  val mopWidth = 8
  val uopWidth = 8
  val numM2M4M8Channel = (8, 8, 8)
  val uopBufferLength = 7

  val alluops: ArrayBuffer[ArrayBuffer[ArrayBuffer[Int]]] = genUopNumPatterns2(mopWidth, numM2M4M8Channel)

  val patternsForUopSel: Seq[Seq[UopNumWithChannelUopNum]] = alluops.zipWithIndex.map {
    case (uopPatterns: ArrayBuffer[ArrayBuffer[Int]], uopIdx) =>
      uopPatterns.map {
        pattern: ArrayBuffer[Int] =>
          UopNumPattern(pattern(0)) ## UopNumOHsPattern(pattern.drop(1).toSeq, mopWidth)
      }.toSeq
  }.toSeq

  // if isEmpty, use uopBuffer[uopIdx] instead
  val uopUsedPorts: Seq[Seq[Option[(Int, Int)]]] = patternsForUopSel.zipWithIndex.map {
    case (patterns: Seq[UopNumWithChannelUopNum], i) =>
      patterns.map {
        pattern: UopNumWithChannelUopNum => {
          val port = genDecodeOutPort2(pattern.p1.num, pattern.p2.uopNumSeq, i)
          port
        }
      }.distinct
  }

  Verilog.emitVerilog(
    new UopSelectModule(
      uopBundle = UInt(1.W),
      mopBundle = UInt(1.W),
      mopWidth = 8,
      uopWidth = 8,
      uopBufferSize = 7,
      filteredPorts = uopUsedPorts,
    )
  )
}