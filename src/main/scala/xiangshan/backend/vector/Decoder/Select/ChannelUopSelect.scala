package xiangshan.backend.vector.Decoder.Select

import chisel3._
import chisel3.util._
import xiangshan.backend.vector.Decoder.RVVDecodeUtil.{UopNumOHsPattern, UopNumOHsPatterns}
import xiangshan.backend.vector.Decoder.NumUopOH
import xiangshan.backend.vector.Decoder.util.{DecodeField, DecodeTable}
import xiangshan.backend.vector.HasVectorSettings
import xiangshan.backend.vector.util.ChiselTypeExt._
import xiangshan.backend.vector.util.Verilog

import scala.collection.mutable.ArrayBuffer

class ChannelUopSelectVectorModule(
  mopWidth: Int,
  uopWidth: Int,
  uopBufferSize: Int,
  numM2M4M8Channel: (Int, Int, Int),
) extends Module with HasVectorSettings {
  import ChannelUopSelectUtil._

  val in = IO(Input(new Bundle {
    val uopNumOHs = Vec(mopWidth, NumUopOH())
  }))
  val out = IO(Output(new Bundle {
    // uopSel(i)(j): uop(i) select decoder(j)'s k-th port
    val uopSel = Vec(uopWidth + uopBufferSize, Vec(mopWidth, Vec(maxSplitUopNum, Bool())))
  }))

  val alluops: ArrayBuffer[ArrayBuffer[ArrayBuffer[Int]]] = genUopNumPatterns(mopWidth, numM2M4M8Channel)

  val numOHsPatterns: ArrayBuffer[ArrayBuffer[UopNumOHsPattern]] = alluops.zipWithIndex.map {
    case (uopPatterns: ArrayBuffer[ArrayBuffer[Int]], uopIdx) =>
      uopPatterns.map {
        pattern: ArrayBuffer[Int] =>
          UopNumOHsPattern(pattern.toSeq, mopWidth)
      }
  }

  val simplifiedPatterns = numOHsPatterns.zipWithIndex.map { case (uopIdx, i) =>
    UopNumOHsPatterns.simplify(numOHsPatterns(i).toSeq)
  }

  val uopUsedPorts: ArrayBuffer[Seq[(Int, Int)]] = simplifiedPatterns.zipWithIndex.map {
    case (patterns: Seq[UopNumOHsPattern], i) => {
      patterns.map {
        case pattern =>
          genDecodeOutPort(pattern.uopNumSeq, i)
      }
    }
  }

  val uopSelectFields: Seq[UopSelectField] = (0 until uopWidth).map(uopIdx => new UopSelectField(mopWidth, uopIdx))

  val decodeTables = numOHsPatterns.indices.map(uopIdx =>
    new DecodeTable(simplifiedPatterns(uopIdx), Seq(uopSelectFields(uopIdx)))
  )
  println(s"The lengths of decodeTables in ChannelUopSelect: ${decodeTables.map(_.table.table.length)}")

  for (i <- 0 until uopWidth) {
    out.uopSel(i) := decodeTables(i).decode(in.uopNumOHs.reverse.reduce(_ ## _))(uopSelectFields(i))
  }

  val uopBufferPatterns: Seq[UopNumOHsPattern] = alluops.last.filter(_.sum > mopWidth).map {
    case x: ArrayBuffer[Int] =>
      UopNumOHsPattern(x.toSeq, mopWidth)
  }.toSeq

  val uopBufferUsedPorts: Seq[Seq[(Int, Int)]] = Seq.tabulate(uopBufferSize) { uopBufferIdx =>
    uopBufferPatterns.map {
      case (pattern: UopNumOHsPattern) =>
        genUopBufferPort(pattern.uopNumSeq, mopWidth, uopBufferIdx)
    }.filter(_.nonEmpty).map(_.get)
  }

  val uopBufferSelectFields: Seq[UopSelectForBufferField] =
    (0 until (8 - 1)).map(uopBufferIdx => new UopSelectForBufferField(mopWidth, uopBufferIdx))

  val uopBufferSelectDecodeTables = uopBufferSelectFields.indices.map(uopBufferIdx =>
    new DecodeTable(uopBufferPatterns, Seq(uopBufferSelectFields(uopBufferIdx)))
  )
  println(s"lengths of uopBufferSelectDecodeTables in ChannelUopSelect: ${uopBufferSelectDecodeTables.map(_.table.table.length)}")

  for (i <- 0 until uopBufferSize) {
    out.uopSel(i + uopWidth) := uopBufferSelectDecodeTables(i).decode(in.uopNumOHs.reverse.reduce(_ ## _))(uopBufferSelectFields(i))
  }

  def showUopInfo(): Unit = {
    alluops.zipWithIndex.map {
      case (uop, i) =>
        println(s"uop$i")
        uop.zipWithIndex.map {
          case (pattern, j) =>
            println(s"pattern$j: $pattern, port: ${uopUsedPorts(i)(j)}")
        }
    }

    alluops.zipWithIndex.map {
      case (uop, i) =>
        val ports = uopUsedPorts(i).distinct
        println(s"uop$i ports(${ports.length}): ${ports}")
    }
  }

  def showAllInPattern(): Unit = {
    numOHsPatterns.indices.foreach { i =>
      println(s"uop$i")
      numOHsPatterns(i).indices.foreach{ j =>
        println(numOHsPatterns(i)(j))
        println(numOHsPatterns(i)(j).bitPat)
        println(uopSelectFields(i).genTable(numOHsPatterns(i)(j)))
      }
    }
  }

  def showSimplifiedPattern(): Unit = {
    println("Simplified")
    simplifiedPatterns.indices.foreach { i =>
      println(s"uop$i")
      simplifiedPatterns(i).indices.foreach{ j =>
        println(f"${simplifiedPatterns(i)(j).uopNumSeq}%-30s port: ${uopUsedPorts(i)(j)}")
      }
    }
  }
}

class ChannelUopSelectModule[T <: Data](
  gen: => T,
  mopWidth: Int,
  uopWidth: Int,
  uopBufferSize: Int,
  numM2M4M8Channel: (Int, Int, Int),
) extends Module with HasVectorSettings {
  import ChannelUopSelectUtil._

  val bundle = gen

  override def desiredName: String = s"UopSelectModule_${bundle}"

  val in =  IO(Input(new Bundle {
    val channelOut = Vec(mopWidth, Vec(8, gen))
    val uopNumOHs = Vec(mopWidth, NumUopOH())
  }))
  val out = IO(Output(new Bundle {
    val channelUopSel = Vec(uopWidth + uopBufferSize, Vec(mopWidth, Vec(maxSplitUopNum, Bool())))
    val decodeUopOut = Vec(uopWidth + uopBufferSize, bundle)
  }))

  val selectVector = Module(new ChannelUopSelectVectorModule(
    mopWidth = mopWidth,
    uopWidth = uopWidth,
    uopBufferSize = uopBufferSize,
    numM2M4M8Channel = numM2M4M8Channel,
  ))

  selectVector.in.uopNumOHs := in.uopNumOHs

  val uopSel: Vec[Vec[Vec[Bool]]] = selectVector.out.uopSel

  out.channelUopSel := uopSel

  for (i <- 0 until uopWidth) {
    val filteredUopSel = filterBundleByPorts(uopSel(i), selectVector.uopUsedPorts(i).toSeq)
    val filteredBundle = filterBundleByPorts(in.channelOut, selectVector.uopUsedPorts(i).toSeq)

    out.decodeUopOut(i) := Mux1H(filteredUopSel, filteredBundle)
  }

  for (i <- 0 until uopBufferSize) {
    val filteredUopSel = filterBundleByPorts(uopSel(i + uopWidth), selectVector.uopBufferUsedPorts(i))
    val filteredBundle = filterBundleByPorts(in.channelOut, selectVector.uopBufferUsedPorts(i))
    if (filteredUopSel.isEmpty)
      out.decodeUopOut(i + uopWidth) := 0.U.asTypeOf(bundle)
    else
      out.decodeUopOut(i + uopWidth) := Mux1H(filteredUopSel, filteredBundle)
  }
}

object ChannelUopSelectUtil extends HasVectorSettings {
  class UopSelectField(mopWidth: Int, uopIdx: Int) extends DecodeField[UopNumOHsPattern, Vec[Vec[Bool]]] {

    override def name: String = "uopSelect"

    override def chiselType: Vec[Vec[Bool]] = Vec(mopWidth, Vec(8, Bool()))

    override def genTable(op: UopNumOHsPattern): BitPat = {
      val (mopIdx, portIdx) = genDecodeOutPort(op.uopNumSeq, uopIdx)
      Seq.tabulate(mopWidth) { i =>
        if (i == mopIdx)
          (1 << portIdx).U(8.W).toBitPat
        else
          BitPat.N(8)
      }.reverse.reduce(_ ## _)
    }
  }

  class UopSelectForBufferField(mopWidth: Int, uopBufferIdx: Int) extends DecodeField[UopNumOHsPattern, Vec[Vec[Bool]]] {

    override def name: String = "uopSelectForBuffer"

    override def chiselType: Vec[Vec[Bool]] = Vec(mopWidth, Vec(8, Bool()))

    override def genTable(op: UopNumOHsPattern): BitPat = {
      val port = genUopBufferPort(op.uopNumSeq, mopWidth, uopBufferIdx)

      if (port.isEmpty) {
        BitPat.N(mopWidth * 8)
      } else {
        val (mopIdx, portIdx) = port.get
        Seq.tabulate(mopWidth) { i =>
          if (i == mopIdx)
            (1 << portIdx).U(8.W).toBitPat
          else
            BitPat.N(8)
        }.reverse.reduce(_ ## _)
      }
    }
  }

  def genUopNumPatterns(numChannel: Int, numM2M4M8Channel: (Int, Int, Int)): ArrayBuffer[ArrayBuffer[ArrayBuffer[Int]]] = {
    val MaxM2UopIdx = numM2M4M8Channel._1
    val MaxM4UopIdx = numM2M4M8Channel._2
    val MaxM8UopIdx = numM2M4M8Channel._3

    val firstUopPatterns = Seq(0, 1, 2, 4, 8).map(x => ArrayBuffer(x))

    val alluops: ArrayBuffer[ArrayBuffer[ArrayBuffer[Int]]] = ArrayBuffer(ArrayBuffer(firstUopPatterns:_*))

    for (i <- 1 until numChannel) {
      val uop: ArrayBuffer[ArrayBuffer[Int]] = alluops(i - 1)
      alluops += ArrayBuffer.empty

      for (pattern: ArrayBuffer[Int] <- uop) {
        val nUop = pattern.sum
        val curMopIdx = pattern.length
        pattern.last match {
          case 0 =>
            alluops(i) += pattern.clone()
          case 1 | 2 | 4 | 8 if nUop > i =>
            alluops(i) += pattern.clone()
          case 1 | 2 | 4 | 8 =>
            alluops(i) += (pattern :+ 0)
            alluops(i) += (pattern :+ 1)
            if (curMopIdx < MaxM2UopIdx)
              alluops(i) += (pattern :+ 2)
            if (curMopIdx < MaxM4UopIdx)
              alluops(i) += (pattern :+ 4)
            if (curMopIdx < MaxM8UopIdx)
              alluops(i) += (pattern :+ 8)
        }
      }
    }

    alluops
  }

  def genUopNumPatterns2(numChannel: Int, numM2M4M8Channel: (Int, Int, Int)): ArrayBuffer[ArrayBuffer[ArrayBuffer[Int]]] = {
    val MaxM2UopIdx = numM2M4M8Channel._1
    val MaxM4UopIdx = numM2M4M8Channel._2
    val MaxM8UopIdx = numM2M4M8Channel._3

    val firstUopPatterns = {
      for {
        uopBufferCount <- 0 until maxSplitUopNum
        uopNum <- Seq(0, 1, 2, 4, 8)
      } yield {
        ArrayBuffer(uopBufferCount, uopNum)
      }
    }

    val alluops: ArrayBuffer[ArrayBuffer[ArrayBuffer[Int]]] = ArrayBuffer(ArrayBuffer(firstUopPatterns:_*))

    for (i <- 1 until numChannel) {
      val uop: ArrayBuffer[ArrayBuffer[Int]] = alluops(i - 1)
      alluops += ArrayBuffer.empty

      for (pattern: ArrayBuffer[Int] <- uop) {
        val nUop = pattern.sum
        val curMopIdx = pattern.length
        pattern.last match {
          case 0 =>
            alluops(i) += pattern.clone()
          case 1 | 2 | 4 | 8 if nUop > i =>
            alluops(i) += pattern.clone()
          case _ =>
            alluops(i) += (pattern :+ 0)
            alluops(i) += (pattern :+ 1)
            if (curMopIdx < MaxM2UopIdx)
              alluops(i) += (pattern :+ 2)
            if (curMopIdx < MaxM4UopIdx)
              alluops(i) += (pattern :+ 4)
            if (curMopIdx < MaxM8UopIdx)
              alluops(i) += (pattern :+ 8)
        }
      }
    }

    alluops
  }

  /**
   *
   * @param uopBufferCount
   * @param pattern
   * @param uopIdx
   * @return (uopFromUopBuffer: Boolean, Some((channelIdx, portIdx)))
   */
  def genDecodeOutPort2(
    uopBufferCount: Int,
    pattern: Seq[Int],
    uopIdx: Int,
  ): Option[(Int, Int)] = {
    if (uopIdx < uopBufferCount)
      None
    else {
      val portNum = uopIdx - (pattern.dropRight(1).sum + uopBufferCount)
      val curMopIdx = pattern.length - 1
      Some((curMopIdx, portNum))
    }
  }

  def genDecodeOutPort(
    pattern: Seq[Int],
    uopIdx: Int,
  ): (Int, Int) = genDecodeOutPortImpl1(
    pattern = pattern, uopIdx = uopIdx
  )

  def genDecodeOutPortImpl1(
    pattern: Seq[Int],
    uopIdx: Int,
  ): (Int, Int) = {
    val portNum = uopIdx - pattern.dropRight(1).sum
    val curMopIdx = pattern.length - 1
    (curMopIdx, portNum)
  }

  def genDecodeOutPorts(
    uopPatterns: ArrayBuffer[ArrayBuffer[ArrayBuffer[Int]]],
  ): ArrayBuffer[ArrayBuffer[(Int, Int)]] = {
    val portRes = ArrayBuffer.empty[ArrayBuffer[Tuple2[Int, Int]]]

    uopPatterns.zipWithIndex.map {
      case (uopPattern: ArrayBuffer[ArrayBuffer[Int]], i) =>
        portRes.append(ArrayBuffer.empty)
        uopPattern.map {
          pattern: ArrayBuffer[Int] =>
            val port = genDecodeOutPort(pattern.toSeq, i)
            portRes(i).append(port)
        }
    }

    portRes
  }

  def genUopBufferPort2(
    uopBufferCount: Int,
    pattern       : Seq[Int],
    uopWidth      : Int,
    uopBufferIdx  : Int,
  ): Option[Tuple2[Int, Int]] = {
    val nUop = pattern.sum + uopBufferCount
    val hasUop = nUop > (uopWidth + uopBufferIdx)
    if (hasUop) {
      val mopIdx = pattern.length - 1
      val portIdx = uopBufferIdx + uopWidth - (nUop - pattern.last)
      // if portIdx is negative number, it means that this entry of uopBuffer will get uop from uopBuffer itself and the
      // uopBufferIdx will be this index plus uopWidth
      if (portIdx >= 0)
        Some((mopIdx, portIdx))
      else
        None
    } else {
      None
    }
  }

  def genUopBufferPort(
    pattern     : Seq[Int],
    uopWidth    : Int,
    uopBufferIdx: Int
  ): Option[Tuple2[Int, Int]] = {
    val nUop = pattern.sum
    val hasUop = nUop > (uopWidth + uopBufferIdx)
    if (hasUop) {
      val mopIdx = pattern.length - 1
      val portIdx = uopBufferIdx + uopWidth - (nUop - pattern.last)
      Some((mopIdx, portIdx))
    } else {
      None
    }
  }

  def filterBundleByPorts[T](
    origin: Seq[Seq[T]],
    ports: Seq[(Int, Int)],
  ): Seq[T] = {
    origin.zipWithIndex.flatMap {
      case (seqt: Seq[T], i) =>
        seqt.zipWithIndex.map {
          case (t, j) =>
            (i, j, t)
        }
    }.filter {
      case (i, j, t) =>
        ports.contains((i, j))
    }.map(_._3)
  }
}

object ChannelUopSelectVectorMain extends App {
  Verilog.emitVerilog(
    {
      val mod = new ChannelUopSelectVectorModule(
        mopWidth = 8, uopWidth = 8, uopBufferSize = 7, numM2M4M8Channel = (8, 4, 2)
      )
      mod.decodeTables.zipWithIndex.foreach { case (table, i) =>
        println(s"uop${i}")
        table.table.table.foreach(println)
      }
      mod.showSimplifiedPattern()
      mod.uopBufferSelectDecodeTables.zipWithIndex.foreach { case (table, i) =>
        println(s"uopBuffer${i}")
        table.table.table.foreach(println)
      }
      mod
    },
  )
}

object ChannelUopSelectModuleMain extends App {
  Verilog.emitVerilog(
    new ChannelUopSelectModule(
      gen = UInt(1.W),
      mopWidth = 6,
      uopWidth = 6,
      uopBufferSize = 7,
      numM2M4M8Channel = (6, 6, 6),
    )
  )
}
