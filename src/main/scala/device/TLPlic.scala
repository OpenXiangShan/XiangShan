package device

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.RegField
import utils.{HasTLDump, XSDebug}
import xiangshan.HasXSLog


/*  base + 0x000000: Reserved (interrupt source 0 does not exist)
    base + 0x000004: Interrupt source 1 priority
    base + 0x000008: Interrupt source 2 priority
    ...
    base + 0x000FFC: Interrupt source 1023 priority
    base + 0x001000: Interrupt Pending bit 0-31
    base + 0x00107C: Interrupt Pending bit 992-1023
    ...
    base + 0x002000: Enable bits for sources 0-31 on context 0
    base + 0x002004: Enable bits for sources 32-63 on context 0
    ...
    base + 0x00207F: Enable bits for sources 992-1023 on context 0
    base + 0x002080: Enable bits for sources 0-31 on context 1
    base + 0x002084: Enable bits for sources 32-63 on context 1
    ...
    base + 0x0020FF: Enable bits for sources 992-1023 on context 1
    base + 0x002100: Enable bits for sources 0-31 on context 2
    base + 0x002104: Enable bits for sources 32-63 on context 2
    ...
    base + 0x00217F: Enable bits for sources 992-1023 on context 2
    ...
    base + 0x1F1F80: Enable bits for sources 0-31 on context 15871
    base + 0x1F1F84: Enable bits for sources 32-63 on context 15871
    base + 0x1F1FFF: Enable bits for sources 992-1023 on context 15871
    ...
    base + 0x1FFFFC: Reserved
    base + 0x200000: Priority threshold for context 0
    base + 0x200004: Claim/complete for context 0
    base + 0x200008: Reserved
    ...
    base + 0x200FFC: Reserved
    base + 0x201000: Priority threshold for context 1
    base + 0x201004: Claim/complete for context 1
    ...
    base + 0x3FFE000: Priority threshold for context 15871
    base + 0x3FFE004: Claim/complete for context 15871
    base + 0x3FFE008: Reserved
    ...
    base + 0x3FFFFFC: Reserved  */

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
    
    require(NrExtIntr <= PLICConsts.maxDevices)
    require(NumCores  <= PLICConsts.maxHarts)
    println(s"Interrupt map with ${NumCores} harts ${NrExtIntr} interrupts")

    val addressSpaceSize = 0x4000000
    val addressBits = log2Up(addressSpaceSize)
    def getOffset(addr: UInt) = addr(addressBits-1,0)

    // MMIO regs
    val nrIntrWord = (NrExtIntr + 31) / 32  // roundup
    val priority = List.fill(NrExtIntr)(Reg(UInt(32.W)))
    val pending = List.fill(nrIntrWord)(RegInit(0.U.asTypeOf(Vec(32, Bool()))))
    val enable = List.fill(NumCores)( List.fill(nrIntrWord)(RegInit(0.U(32.W))) )
    val threshold = List.fill(NumCores)(Reg(UInt(32.W)))
    val claimCompletion = List.fill(NumCores)(Reg(UInt(32.W)))
    /* pending bits are updated in the unit of bit by PLIC,
       so define it as vectors of bits, instead of UInt(32.W) */
    
    // Intr generation
    val inHandle = RegInit(0.U.asTypeOf(Vec(NrExtIntr + 1, Bool())))
    def completionFn(wdata: UInt) = {
      inHandle(wdata(31,0)) := false.B
      0.U
    }

    io.intrVec.asBools.zipWithIndex.map { case (intr, i) => {
      val id = i + 1
      when (intr) { pending(id / 32)(id % 32) := true.B }
      when (inHandle(id)) { pending(id / 32)(id % 32) := false.B }  // 如果被读取了，则清除相应的 pending 位
    } }

    val pendingVec = Cat(pending.map(x => Cat(x.reverse)))
    claimCompletion.zipWithIndex.map { case (r, hart) => {
      val takenVec = pendingVec & Cat(enable(hart))
      r := Mux(takenVec === 0.U, 0.U, PriorityEncoder(takenVec))
    } }

  }
}

