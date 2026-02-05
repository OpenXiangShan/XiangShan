package xiangshan.backend.regfile

import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.DecodeLogic
import utility._

class V0RegFile(readProtCount: Int, WriteProtCount: Int, VLEN: Int, V0RegCount: Int) extends Module {
  private val rowWidth       = VLEN / 8
  private val rows           = VLEN / rowWidth
  private val wIdxBitWidth   = log2Up(rows)
  private val rIdxBitWidth   = log2Up(rows)
  private val RegSelBitWidth = log2Up(V0RegCount)
  require(isPow2(VLEN), "V0reg VLEN is not Pow2")
  require(isPow2(rowWidth), "V0reg rowWidth is not Pow2")
  require(isPow2(rows), "V0reg rows is not Pow2")
  require(isPow2(V0RegCount), "V0reg V0RegNum is not Pow2")

  val io          = IO(V0RegFileBundleIO(readProtCount, WriteProtCount, rowWidth, rows, V0RegCount))
  val V0Regs      = VecInit(Seq.fill(V0RegCount)(Module(new V0Reg(readProtCount, WriteProtCount, VLEN)).io))
  val readRegData = Wire(Vec(V0RegCount, Vec(readProtCount, UInt(rowWidth.W))))
  val wRegSel     = Wire(Vec(WriteProtCount, UInt(RegSelBitWidth.W)))
  val rRegSel     = Wire(Vec(readProtCount, UInt(RegSelBitWidth.W)))
  for (i <- 0 until WriteProtCount) {
    wRegSel(i) := io.writePorts(i).wIdx(RegSelBitWidth + wIdxBitWidth - 1, wIdxBitWidth)
  }
  for (i <- 0 until readProtCount) {
    rRegSel(i)            := io.readPorts(i).rIdx(RegSelBitWidth + rIdxBitWidth - 1, rIdxBitWidth)
    io.readPorts(i).rData := readRegData(rRegSel(i))(i)
  }
  for {
    regId   <- 0 until V0RegCount
    wPortId <- 0 until WriteProtCount
  } yield {
    V0Regs(regId).writePorts(wPortId).wen := io.writePorts(wPortId).wen & regId.U === wRegSel(
      wPortId
    )
    V0Regs(regId).writePorts(wPortId).wIdx       := io.writePorts(wPortId).wIdx(wIdxBitWidth - 1, 0)
    V0Regs(regId).writePorts(wPortId).wData      := io.writePorts(wPortId).wData
    V0Regs(regId).writePorts(wPortId).wDataWidth := io.writePorts(wPortId).wDataWidth
  }

  for {
    regId   <- 0 until V0RegCount
    rPortId <- 0 until readProtCount
  } yield {
    V0Regs(regId).readPorts(rPortId).ren := io.readPorts(rPortId).ren & regId.U === rRegSel(
      rPortId
    )
    V0Regs(regId).readPorts(rPortId).rIdx       := io.readPorts(rPortId).rIdx(rIdxBitWidth - 1, 0)
    V0Regs(regId).readPorts(rPortId).rLineCount := io.readPorts(rPortId).rLineCount
    readRegData(regId)(rPortId)                 := V0Regs(regId).readPorts(rPortId).rData
  }

}

class V0Reg(readProtCount: Int, WriteProtCount: Int, VLEN: Int) extends Module {
  private val rowWidth           = VLEN / 8
  private val rows               = VLEN / rowWidth
  private val rLineCountBitWidth = log2Up(log2Up(rows) + 1)
  private val rIdxBitWidth       = log2Up(rows)

  val io = IO(V0RegBundleIO(readProtCount, WriteProtCount, rowWidth, rows))

  val maskReg = RegInit(VecInit.fill(rows)(0.U(rowWidth.W)))
  // write
  val expandDatas     = Wire(Vec(WriteProtCount, UInt(rowWidth.W)))
  val rowWidthMaxLog2 = log2Up(rowWidth) + 1
  val widthWIdxMappings = (0 until rowWidthMaxLog2).collect {
    case i => i.U -> (1 << i)
  }.toSeq

  for (i <- 0 until WriteProtCount) {
    expandDatas(i) := LookupTree(
      io.writePorts(i).wDataWidth,
      widthWIdxMappings.map { case (wWidth, actualWidth) =>
        wWidth -> FillInterleaved(rowWidth / actualWidth, io.writePorts(i).wData(actualWidth - 1, 0))
      }
    )
  }
  for (i <- 0 until rows) {
    val wenOH = VecInit(io.writePorts.map(w => w.wen && w.wIdx === i.U))
    val wData = Mux1H(wenOH, expandDatas)
    when(wenOH.asUInt.orR) {
      maskReg(i) := wData
    }
  }

  for (i <- 0 until io.writePorts.size - 1) {
    val hasSameWrite = io.writePorts.drop(i + 1).map(w =>
      w.wen && w.wIdx === io.writePorts(i).wIdx && io.writePorts(i).wen
    ).reduce(_ || _)
    assert(!hasSameWrite, "V0Reg two or more writePorts write same addr")
  }

  // read
  val readDate = Wire(Vec(readProtCount, UInt(rowWidth.W)))
  def generateOutData(rIdx: Int, lineNum: Int): UInt = {
    val readyLineNum = 1 << lineNum
    val repeatedBits = Seq.tabulate(readyLineNum) { rrowi =>
      val bits = Seq.tabulate(rowWidth / readyLineNum)(i =>
        this.maskReg(rIdx.U(rIdxBitWidth.W) + rrowi.U(rIdxBitWidth.W))(i * readyLineNum)
      )
      Cat(bits.reverse)
    }
    Cat(repeatedBits.reverse)
  }

  val resultsOutData = for {
    rLineN <- 0 to rIdxBitWidth
    rId    <- 0 to (rows - 1)
    if rId % (1 << rLineN) == 0
  } yield {
    generateOutData(rId, rLineN)
  }

  var Index = 0;
  val decoderTable = for {
    rLineN <- 0 to rIdxBitWidth
    rId    <- 0 to (rows - 1)
    if rLineN == 0 || (rId % (1 << rLineN) == 0)
  } yield {
    val inputWidth  = rIdxBitWidth + rLineCountBitWidth
    val inputVal    = ((rLineN << rIdxBitWidth) | (rId & (~((1 << rLineN) - 1)))) >> rLineN
    var inputBinary = inputVal.toBinaryString
    inputBinary = if (rLineN != 0) inputBinary + "?" * rLineN else inputBinary
    inputBinary = inputBinary.reverse.padTo(inputWidth, '0').reverse
    val inputPat = BitPat("b" + inputBinary)

    val outputWidth  = 2 * rows - 1
    val outputVal    = 1 << Index
    val outputBinary = outputVal.toBinaryString.reverse.padTo(outputWidth, '0').reverse
    val outputPat    = BitPat("b" + outputBinary)
    Index += 1
    (inputPat, outputPat)
  }

  val default = BitPat("b" + "?" * (2 * rows - 1))
  val readDecoders = VecInit(
    (0 until readProtCount).map { i =>
      DecodeLogic(
        Cat(io.readPorts(i).rLineCount, io.readPorts(i).rIdx),
        default,
        decoderTable
      )
    }
  )
  val outReadData = RegInit(VecInit(Seq.fill(readProtCount)(0.U(rowWidth.W))))
  for (i <- 0 until readProtCount) {
    readDate(i) := Mux1H(readDecoders(i), resultsOutData)
    when(io.readPorts(i).ren) {
      outReadData(i) := readDate(i)
    }
    io.readPorts(i).rData := outReadData(i)
  }

}

/*
 rLineCount encoding rules:
  0 -> read row count = 1 (2^0 = 1 row)
  1 -> read row count = 2 (2^1 = 2 rows)
  2 -> read row count = 4 (2^2 = 4 rows)
  ...
  n -> read row count = 2^n rows
 */
class V0RegReadPort(rIdxBitWidth: Int, rLineCountBitWidth: Int, rowWidth: Int) extends Bundle {
  val ren        = Input(Bool())
  val rIdx       = Input(UInt(rIdxBitWidth.W))
  val rLineCount = Input(UInt(rLineCountBitWidth.W))
  val rData      = Output(UInt(rowWidth.W))
}
/*
wDataWidth encoding rules:
  0 -> actual width = 1 (2^0 = 1 bit)
  1 -> actual width = 2 (2^1 = 2 bits)
  2 -> actual width = 4 (2^2 = 4 bits)
  ...
  n -> actual width = 2^n bits
 */
class V0RegWritePort(wIdxBitsWidth: Int, wDataWidthBitWidth: Int, rowWidth: Int) extends Bundle {
  val wen        = Input(Bool())
  val wIdx       = Input(UInt(wIdxBitsWidth.W))
  val wDataWidth = Input(UInt(wDataWidthBitWidth.W))
  val wData      = Input(UInt(rowWidth.W))
}

object V0RegBundleIO {
  def apply(readProtCount: Int, WriteProtCount: Int, rowWidth: Int, rows: Int) = {
    val wDataWidthBitWidth = log2Up(log2Up(rowWidth) + 1)
    val rLineCountBitWidth = log2Up(log2Up(rows) + 1)
    val rIdxBitWidth       = log2Up(rows)
    val wIdxBitWidth       = log2Up(rows)
    new Bundle {
      val readPorts  = Vec(readProtCount, new V0RegReadPort(rIdxBitWidth, rLineCountBitWidth, rowWidth))
      val writePorts = Vec(WriteProtCount, new V0RegWritePort(wIdxBitWidth, wDataWidthBitWidth, rowWidth))
    }
  }
}

object V0RegFileBundleIO {
  def apply(readProtCount: Int, WriteProtCount: Int, rowWidth: Int, rows: Int, V0RegCount: Int) = {
    val wDataWidthBitWidth = log2Up(log2Up(rowWidth) + 1)
    val rLineCountBitWidth = log2Up(log2Up(rows) + 1)
    val rIdxBitWidth       = log2Up(rows) + log2Up(V0RegCount)
    val wIdxBitWidth       = log2Up(rows) + log2Up(V0RegCount)
    new Bundle {
      val readPorts  = Vec(readProtCount, new V0RegReadPort(rIdxBitWidth, rLineCountBitWidth, rowWidth))
      val writePorts = Vec(WriteProtCount, new V0RegWritePort(wIdxBitWidth, wDataWidthBitWidth, rowWidth))
    }
  }
}
