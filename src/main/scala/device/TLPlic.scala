package device

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.RegField
import utils.{HasTLDump, XSDebug}
import xiangshan.HasXSLog

object PLICConsts
{
  def maxDevices = 1023
  def maxHarts = 15872
  def priorityBase = 0x0
  def pendingBase = 0x1000
  def enableBase = 0x2000
  def hartBase = 0x200000

  def claimOffset = 4
  def priorityBytes = 4

  def enableOffset(i: Int) = i * ((maxDevices+7)/8)
  def hartOffset(i: Int) = i * 0x1000
  def enableBase(i: Int):Int = enableOffset(i) + enableBase
  def hartBase(i: Int):Int = hartOffset(i) + hartBase

  def size(maxHarts: Int): Int = {
    require(maxHarts > 0 && maxHarts <= maxHarts, s"Must be: maxHarts=$maxHarts > 0 && maxHarts <= PLICConsts.maxHarts=${PLICConsts.maxHarts}")
    1 << log2Ceil(hartBase(maxHarts))
  }

  require(hartBase >= enableBase(maxHarts))
}

class TLPLIC(address: Seq[AddressSet], sim: Boolean)(implicit p: Parameters) extends LazyModule {

  val device = new SimpleDevice("interrupt-controller", Seq("riscv,plic0"))
  val node = TLRegisterNode(address, device, beatBytes = 8/*, concurrency = 1*/)  // TODO: decide concurrency argument
  val NumCores = top.Parameters.get.socParameters.NumCores

  lazy val module = new LazyModuleImp(this) with HasXSLog with xiangshan.HasXSParameter with HasTLDump{
    val io = IO(new Bundle() {
      val intrVec = Input(UInt(NrExtIntr.W))
      val meip = Output(Vec(NumCores, Bool()))
    })
    
    for (i <- 0 until NumCores) {
        io.meip(i) := false.B
    }
  }
}

