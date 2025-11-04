package xiangshan.backend.vector.Decoder

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.{BoolDecodeField, DecodeField, DecodePattern, DecodeTable}
import xiangshan.backend.vector.Decoder.RVVDecodeUtil.{DecodePatternComb2, UopNumOHsPattern}
import xiangshan.backend.vector.Decoder.Select.ChannelUopSelectUtil.{genDecodeOutPort2, genUopBufferPort2, genUopNumPatterns2}
import xiangshan.backend.vector._
import xiangshan.backend.vector.util.ChiselTypeExt._
import xiangshan.backend.vector.util.ScalaTypeExt._
import xiangshan.backend.vector.util.Verilog

import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

class UopBufferCtrlDecoder(
  mopWidth        : Int,
  uopWidth        : Int,
  uopBufferLength : Int,
  numM2M4M8Channel: (Int, Int, Int),
) extends Module with HasVectorSettings {
  val MaxM2UopIdx = numM2M4M8Channel._1
  val MaxM4UopIdx = numM2M4M8Channel._2
  val MaxM8UopIdx = numM2M4M8Channel._3

  import UopBufferCtrlDecoderUtil._

  val in = IO(Input(new Bundle {
    val uopBufferNum = UInt(3.W)
    val channelUopNum = Vec(mopWidth, UopNumOH())
  }))
  val out = IO(Output(new Bundle {
    val uopValids = Vec(uopWidth, Bool())
    val bufferValids = Vec(uopBufferLength, Bool())
    val uopBufferNum = UInt(3.W)
    val acceptVec = Vec(mopWidth, Bool())
    val selForUop = Vec(uopWidth, new UopSelectBundle(mopWidth))
    val selForBufffer = Vec(uopBufferLength, new UopSelectBundle(mopWidth))
  }))

  val alluops: ArrayBuffer[ArrayBuffer[ArrayBuffer[Int]]] = genUopNumPatterns2(mopWidth, numM2M4M8Channel)

  val patternsForUopSel: Seq[Seq[UopNumWithChannelUopNum]] = alluops.zipWithIndex.map {
    case (uopPatterns: ArrayBuffer[ArrayBuffer[Int]], uopIdx) =>
      uopPatterns.map {
        pattern: ArrayBuffer[Int] =>
          UopNumPattern(pattern(0)) ## UopNumOHsPattern(pattern.drop(1).toSeq, mopWidth)
      }.toSeq
  }.toSeq

  val selForUopFields: Seq[UopSelectField] = (0 until uopWidth).map(uopIdx => new UopSelectField(mopWidth, uopIdx, uopBufferLength))
  val selForUopDecodeTables: Seq[DecodeTable[UopNumWithChannelUopNum]] = patternsForUopSel.indices.map(uopIdx =>
    new DecodeTable(patternsForUopSel(uopIdx), Seq(selForUopFields(uopIdx)))
  )
  println(s"lengths of DecodeTable in UopBufferCtrlDecoder: ${patternsForUopSel.map(_.length)}")

  val patternsForBufferSel: Seq[UopNumWithChannelUopNum] = patternsForUopSel.last

  val selForBufferFields: Seq[UopSelectForBufferField] = (0 until uopBufferLength).map(
    uopBufferIdx =>
      new UopSelectForBufferField(
        mopWidth = mopWidth,
        uopWidth = uopWidth,
        uopBufferIdx = uopBufferIdx,
        uopBufferLen = uopBufferLength,
      )
  )

  val selForBufferDecodeTables: Seq[DecodeTable[UopNumWithChannelUopNum]] = selForBufferFields.indices.map(i =>
    new DecodeTable(patternsForBufferSel, Seq(selForBufferFields(i)))
  )

  val uopBufferNumField = new UopBufferNumField(uopWidth)
  val acceptVecField = new AcceptVecField(mopWidth, uopWidth, uopBufferLength)
  val uopValidsField = new UopValidsField(uopWidth)
  val bufferValidsField = new BufferValidsField(uopWidth, uopBufferLength)

  val fields = Seq(
    uopBufferNumField,
    acceptVecField,
    uopValidsField,
    bufferValidsField,
  )

  println(s"length of DecodeTable in UopBufferCtrlDecoder: ${patternsForBufferSel.length}")
  val decodeTable = new DecodeTable(patternsForBufferSel, fields)
  val decodeResult = decodeTable.decode(in.uopBufferNum ## in.channelUopNum.reverse.reduce(_ ## _))

  val acceptVec = decodeResult(acceptVecField)

  out.uopValids := decodeResult(uopValidsField)
  out.bufferValids := decodeResult(bufferValidsField)

  out.uopBufferNum := decodeResult(uopBufferNumField)
  for (i <- out.acceptVec.indices) {
    out.acceptVec(i) := acceptVec(i) &&
      ((i < MaxM2UopIdx).B || (i >= MaxM2UopIdx).B && !in.channelUopNum(i)(1)) &&
      ((i < MaxM4UopIdx).B || (i >= MaxM4UopIdx).B && !in.channelUopNum(i)(2)) &&
      ((i < MaxM8UopIdx).B || (i >= MaxM8UopIdx).B && !in.channelUopNum(i)(3))
  }

  for (i <- 0 until uopWidth) {
    out.selForUop(i) := selForUopDecodeTables(i)
      .decode(in.uopBufferNum ## in.channelUopNum.reverse.reduce(_ ## _))
      .apply(selForUopFields(i))
  }

  for (i <- 0 until uopBufferLength) {
    out.selForBufffer(i) := selForBufferDecodeTables(i)
      .decode(in.uopBufferNum ## in.channelUopNum.reverse.reduce(_ ## _))
      .apply(selForBufferFields(i))
  }

  // if isEmpty, use uopBuffer[uopIdx] instead
  val uopUsedPorts: Seq[Seq[Option[(Int, Int)]]] = patternsForUopSel.zipWithIndex.map {
    case (patterns: Seq[UopNumWithChannelUopNum], i) =>
      patterns.map {
        pattern: UopNumWithChannelUopNum => {
          genDecodeOutPort2(pattern.p1.num, pattern.p2.uopNumSeq, i)
        }
      }.distinct
  }

  // if isEmpty, use uopBuffer[uopIdx - uopWidth] instead
  val bufferUsedPorts: Seq[Seq[Option[(Int, Int)]]] = (0 to uopBufferLength).map {
    uopBufferIdx =>
      patternsForBufferSel.map {
        case (pattern: UopNumWithChannelUopNum) =>
          genUopBufferPort2(pattern.p1.num, pattern.p2.uopNumSeq, uopWidth, uopBufferIdx)
      }.distinct
  }

}

object UopBufferCtrlDecoderMain extends App {
  Verilog.emitVerilog(
    new UopBufferCtrlDecoder(8, 8, 7, (8, 8, 8))
  )
}

object UopBufferCtrlDecoderUtil extends HasVectorSettings {
  case class UopNumPattern(num: Int) extends DecodePattern {
    require(num >= 0 && num < maxSplitUopNum)

    override def bitPat: BitPat = new BitPat(value = num, mask = (1 << 3) - 1, width = 3)
  }

  type UopNumWithChannelUopNum = DecodePatternComb2[
    UopNumPattern,
    UopNumOHsPattern,
  ]

  object UopNumWithChannelUopNum {
    def apply(uopBufferNum: Int, uopNumOHs: Seq[Int], mopWidth: Int): UopNumWithChannelUopNum = {
      UopNumPattern(uopBufferNum) ## UopNumOHsPattern(uopNumOHs, mopWidth)
    }

    def unapply(arg: UopNumWithChannelUopNum): Option[(Int, Seq[Int])] = {
      Some(arg.p1.num, arg.p2.uopNumSeq)
    }

    def simplify(patterns: Seq[UopNumWithChannelUopNum], uopIdx: Int): Seq[UopNumWithChannelUopNum] = {
      println(s"uop$uopIdx before simplify")
      patterns.foreach(x => println(x.p1.num, x.p2.uopNumSeq))

      val res = ArrayBuffer.empty[UopNumWithChannelUopNum]
      val tail0Indices: Seq[Int] = patterns.zipWithIndex.collect { case (x: UopNumWithChannelUopNum, idx: Int) if x.p2.uopNumSeq.last == 0 => idx }
      res ++= patterns.slice(0, tail0Indices.head)
      for ((idx, i) <- tail0Indices.zipWithIndex) {
        // i is the index of tail0Indices
        val needRemove =
          patterns.isDefinedAt(idx + 1) && patterns(idx + 1).p2.uopNumSeq.last == 1 &&
          patterns.isDefinedAt(idx + 2) && patterns(idx + 2).p2.uopNumSeq.last == 2 &&
          patterns.isDefinedAt(idx + 3) && patterns(idx + 3).p2.uopNumSeq.last == 4 &&
          patterns.isDefinedAt(idx + 4) && patterns(idx + 4).p2.uopNumSeq.last == 8
        if (needRemove) {
          res +=  patterns(idx).p1 ## patterns(idx).p2.copy(uopNumSeq = patterns(idx).p2.uopNumSeq.dropRight(1) :+ 0b1111)
          res ++= patterns.slice(
            if (tail0Indices.isDefinedAt(i + 1)) idx + 5 else patterns.length,
            if (tail0Indices.isDefinedAt(i + 1)) tail0Indices(i + 1) else patterns.length
          )
        } else {
          res ++= patterns.slice(idx, if (tail0Indices.isDefinedAt(i + 1)) tail0Indices(i + 1) else patterns.length)
        }
      }
      if (res.last.p2.uopNumSeq.last == 0b1111) {
        res ++= patterns.slice(tail0Indices.last + 5, patterns.length)
      }
      println(s"uop$uopIdx after simplify")
      res.foreach(x => println(x.p1.num, x.p2.uopNumSeq))

      res.toSeq
    }
  }

  class UopBufferNumField(uopWidth: Int) extends DecodeField[UopNumWithChannelUopNum, UInt] {

    override def name: String = "uopBufferNum"

    override def chiselType: UInt = UInt(3.W)

    override def genTable(op: UopNumWithChannelUopNum): BitPat = {
      val UopNumWithChannelUopNum(uopBufferNum, uopNumOHs) = op
      val scanSum = uopNumOHs.scan(uopBufferNum)(_ + _)
      val firstGeUopWidth = scanSum.find(_ >= uopWidth)
      if (firstGeUopWidth.isEmpty)
        BitPat.N(3)
      else
        (firstGeUopWidth.get - uopWidth).U(3.W).toBitPat
    }
  }

  class AcceptVecField(mopWidth: Int, uopWidth: Int, uopBufferSize: Int) extends DecodeField[UopNumWithChannelUopNum, Vec[Bool]] {

    override def name: String = "acceptVec"

    override def chiselType: Vec[Bool] = Vec(mopWidth, Bool())

    override def genTable(op: UopNumWithChannelUopNum): BitPat = {
      val UopNumWithChannelUopNum(uopBufferNum, uopNumOHs) = op
      val scanSum = uopNumOHs.scan(uopBufferNum)(_ + _)
      val acceptLastIdx = scanSum.lastIndexWhere(_ <= uopWidth + uopBufferSize, Int.MaxValue)
      if (acceptLastIdx <= 0) {
        // can only accept uop from uopBuffer
        BitPat.N(mopWidth)
      } else
        BitPat.Y(acceptLastIdx).pad0To(mopWidth)
    }
  }

  class UopSelectBundle(mopWidth: Int) extends Bundle {
    val uopBufferSel = Bool()
    val channelSel = Vec(mopWidth, Vec(maxSplitUopNum, Bool()))
  }

  class UopSelectField(mopWidth: Int, uopIdx: Int, uopBufferSize: Int) extends DecodeField[UopNumWithChannelUopNum, UopSelectBundle] {

    override def name: String = "uopSelect"

    override def chiselType: UopSelectBundle = new UopSelectBundle(mopWidth)

    override def genTable(op: UopNumWithChannelUopNum): BitPat = {
      val channelPort: Option[(Int, Int)] = genDecodeOutPort2(op.p1.num, op.p2.uopNumSeq, uopIdx)

      val uopBufferBP = channelPort.isEmpty.toBitPat
      val channelBP = channelPort match {
        case Some((mopIdx, portIdx)) =>
          Seq.tabulate(mopWidth) { i =>
          if (i == mopIdx)
            (1 << portIdx).U(8.W).toBitPat
          else
            BitPat.N(8)
        }.reverse.reduce(_ ## _)
        case None => BitPat.N(mopWidth * maxSplitUopNum)
      }

      uopBufferBP ## channelBP
    }
  }

  class UopValidsField(uopWidth: Int) extends DecodeField[UopNumWithChannelUopNum, Vec[Bool]] {

    override def name: String = "uopValids"

    override def chiselType: Vec[Bool] = Vec(uopWidth, Bool())

    override def genTable(op: UopNumWithChannelUopNum): BitPat = {
      val uopBufferCount = op.p1.num
      val channelUopNums = op.p2.uopNumSeq
      Seq.tabulate(uopWidth) { case i =>
        i < uopBufferCount + channelUopNums.sum
      }.reverse.map(_.toBitPat).reduce(_ ## _)
    }
  }

  class BufferValidsField(uopWidth: Int, uopBufferLength: Int) extends DecodeField[UopNumWithChannelUopNum, Vec[Bool]] {

    override def name: String = "bufferValids"

    override def chiselType: Vec[Bool] = Vec(uopBufferLength, Bool())

    override def genTable(op: UopNumWithChannelUopNum): BitPat = {
      val uopBufferCount = op.p1.num
      val channelUopNums = op.p2.uopNumSeq
      Seq.tabulate(uopBufferLength) { case i =>
        uopWidth + i < uopBufferCount + channelUopNums.sum
      }.reverse.map(_.toBitPat).reduce(_ ## _)
    }
  }

  class UopSelectForBufferField(mopWidth: Int, uopWidth: Int, uopBufferIdx: Int, uopBufferLen: Int) extends DecodeField[UopNumWithChannelUopNum, UopSelectBundle] {

    override def name: String = "uopSelectForBuffer"

    override def chiselType: UopSelectBundle = new UopSelectBundle(mopWidth)

    override def genTable(op: UopNumWithChannelUopNum): BitPat = {
      val port: Option[(Int, Int)] =
        genUopBufferPort2(op.p1.num, op.p2.uopNumSeq, uopWidth, uopBufferIdx)
      println(op.p1.num, op.p2.uopNumSeq, uopWidth, uopBufferIdx, port)
      val uopBufferBP = (port.isEmpty || port.nonEmpty && port.get._2 < 0).toBitPat
      val channelBP = port match {
        case Some((mopIdx, portIdx)) if portIdx >= 0 =>
          Seq.tabulate(mopWidth) { i =>
            if (i == mopIdx)
              (1 << portIdx).U(8.W).toBitPat
            else
              BitPat.N(8)
          }.reverse.reduce(_ ## _)
        case Some((mopIdx, portIdx)) if portIdx < 0 =>
          BitPat.N(mopWidth * maxSplitUopNum)
        case None =>
          BitPat.N(mopWidth * maxSplitUopNum)
      }
      uopBufferBP ## channelBP
    }
  }
}
