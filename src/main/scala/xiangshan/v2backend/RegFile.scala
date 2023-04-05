package xiangshan.v2backend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.regfile.Regfile
import xiangshan.v2backend.Bundles.IssueQueueWakeUpBundle

class RfReadPortWithConfig(val rfReadDataCfg: DataConfig, addrWidth: Int) extends Bundle {
  val addr: UInt = Input(UInt(addrWidth.W))
  val data: UInt = Output(UInt(rfReadDataCfg.dataWidth.W))

  def readInt: Boolean = rfReadDataCfg.isInstanceOf[IntData]
  def readFp : Boolean = rfReadDataCfg.isInstanceOf[FpData]
  def readVec: Boolean = rfReadDataCfg.isInstanceOf[VecData]
}

class RfWritePortWithConfig(val rfWriteDataCfg: DataConfig, addrWidth: Int) extends Bundle {
  val wen = Input(Bool())
  val addr = Input(UInt(addrWidth.W))
  val data = Input(UInt(rfWriteDataCfg.dataWidth.W))
  val intWen = Input(Bool())
  val fpWen = Input(Bool())
  val vecWen = Input(Bool())
  def writeInt: Boolean = rfWriteDataCfg.isInstanceOf[IntData]
  def writeFp : Boolean = rfWriteDataCfg.isInstanceOf[FpData]
  def writeVec: Boolean = rfWriteDataCfg.isInstanceOf[VecData]

  def toWakeUpBundle: ValidIO[IssueQueueWakeUpBundle] = {
    val wakeup = Wire(ValidIO(new IssueQueueWakeUpBundle(addrWidth)))
    wakeup.bits.pdest := this.addr
    wakeup.bits.rfWen := this.intWen && this.wen
    wakeup.bits.fpWen := this.fpWen && this.wen
    wakeup.bits.vecWen := this.vecWen && this.wen
    wakeup.valid := this.wen
    wakeup
  }
}

object RegFile {
  // non-return version
  def apply(
    name         : String,
    numEntries   : Int,
    raddr        : Seq[UInt],
    rdata        : Vec[UInt],
    wen          : Seq[Bool],
    waddr        : Seq[UInt],
    wdata        : Seq[UInt],
    hasZero      : Boolean,
    withReset    : Boolean = false,
    debugReadAddr: Option[Seq[UInt]],
    debugReadData: Option[Vec[UInt]],
  )(implicit p: Parameters): Unit = {
    val numReadPorts = raddr.length
    val numWritePorts = wen.length
    require(wen.length == waddr.length)
    require(wen.length == wdata.length)
    val dataBits = wdata.map(_.getWidth).min
    require(wdata.map(_.getWidth).min == wdata.map(_.getWidth).max, s"dataBits != $dataBits")
    val addrBits = waddr.map(_.getWidth).min
    require(waddr.map(_.getWidth).min == waddr.map(_.getWidth).max, s"addrBits != $addrBits")

    val regfile = Module(new Regfile(name, numEntries, numReadPorts, numWritePorts, hasZero, dataBits, addrBits))
    rdata := regfile.io.readPorts.zip(raddr).map { case (rport, addr) =>
      rport.addr := addr
      rport.data
    }

    regfile.io.writePorts.zip(wen).zip(waddr).zip(wdata).foreach{ case (((wport, en), addr), data) =>
      wport.wen := en
      wport.addr := addr
      wport.data := data
    }
    if (withReset) {
      val numResetCycles = math.ceil(numEntries / numWritePorts).toInt
      val resetCounter = RegInit(numResetCycles.U)
      val resetWaddr = RegInit(VecInit((0 until numWritePorts).map(_.U(log2Up(numEntries + 1).W))))
      val inReset = resetCounter =/= 0.U
      when (inReset) {
        resetCounter := resetCounter - 1.U
        resetWaddr := VecInit(resetWaddr.map(_ + numWritePorts.U))
      }
      when (!inReset) {
        resetWaddr.map(_ := 0.U)
      }
      for ((wport, i) <- regfile.io.writePorts.zipWithIndex) {
        wport.wen := inReset || wen(i)
        wport.addr := Mux(inReset, resetWaddr(i), waddr(i))
        wport.data := wdata(i)
      }
    }

    require(debugReadAddr.nonEmpty == debugReadData.nonEmpty, "Both debug addr and data bundles should be empty or not")
    regfile.io.debug_rports := DontCare
    if (debugReadAddr.nonEmpty && debugReadData.nonEmpty) {
      debugReadData.get := VecInit(regfile.io.debug_rports.zip(debugReadAddr.get).map { case (rport, addr) =>
        rport.addr := addr
        rport.data
      })
    }
  }
}

object IntRegFile {
  // non-return version
  def apply(
    name         : String,
    numEntries   : Int,
    raddr        : Seq[UInt],
    rdata        : Vec[UInt],
    wen          : Seq[Bool],
    waddr        : Seq[UInt],
    wdata        : Seq[UInt],
    debugReadAddr: Option[Seq[UInt]],
    debugReadData: Option[Vec[UInt]],
    withReset    : Boolean = false,
  )(implicit p: Parameters): Unit = {
    RegFile(
      name, numEntries, raddr, rdata, wen, waddr, wdata,
      hasZero = true, withReset, debugReadAddr, debugReadData)
  }
}

object VfRegFile {
  // non-return version
  def apply(
    name         : String,
    numEntries   : Int,
    splitNum     : Int,
    raddr        : Seq[UInt],
    rdata        : Vec[UInt],
    wen          : Seq[Seq[Bool]],
    waddr        : Seq[UInt],
    wdata        : Seq[UInt],
    debugReadAddr: Option[Seq[UInt]],
    debugReadData: Option[Vec[UInt]],
    withReset    : Boolean = false,
  )(implicit p: Parameters): Unit = {
    require(splitNum >= 1, "splitNum should be no less than 1")
    require(splitNum == wen.length, "splitNum should be equal to length of wen vec")
    if (splitNum == 1) {
      RegFile(
        name, numEntries, raddr, rdata, wen.head, waddr, wdata,
        hasZero = false, withReset, debugReadAddr, debugReadData)
    } else {
      val dataWidth = 64
      val numReadPorts = raddr.length
      require(splitNum > 1 && wdata.head.getWidth == dataWidth * splitNum)
      val wdataVec = Wire(Vec(splitNum, Vec(wdata.length, UInt(dataWidth.W))))
      val rdataVec = Wire(Vec(splitNum, Vec(raddr.length, UInt(dataWidth.W))))
      val debugRDataVec: Option[Vec[Vec[UInt]]] = debugReadData.map(x => Wire(Vec(splitNum, Vec(x.length, UInt(dataWidth.W)))))
      for (i <- 0 until splitNum) {
        wdataVec(i) := wdata.map(_ ((i + 1) * dataWidth - 1, i * dataWidth))
        RegFile(
          name + s"Part${i}", numEntries, raddr, rdataVec(i), wen(i), waddr, wdataVec(i),
          hasZero = false, withReset, debugReadAddr, debugRDataVec.map(_(i))
        )
      }
      for (i <- 0 until rdata.length) {
        rdata(i) := Cat(rdataVec.map(_ (i)).reverse)
      }
      if (debugReadData.nonEmpty) {
        for (i <- 0 until debugReadData.get.length) {
          debugReadData.get(i) := Cat(debugRDataVec.get.map(_ (i)).reverse)
        }
      }
    }
  }
}