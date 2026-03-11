package xiangshan.backend.rename.freelist

import chisel3._
import chisel3.util._
import freechips.rocketchip.formal._
import iopmp._
import org.chipsalliance.cde.config.Parameters
import utility._
import utils._
import xiangshan._
import xiangshan.backend.rename.FreeListSnapshotGenerator
import xiangshan.backend.rename.RegType
import xiangshan.backend.rename.Reg_I
import xiangshan.backend.rename.Reg_F
import xiangshan.backend.rename.Reg_V
import cc.xiangshan.openncb.EnumAXIMasterOrder.Request
import chisel3.{NumIntf => value}

class FreeListBundle(numPhyRegs: Int, RenameWidth: Int, commitWidth: Int)(implicit
    p: Parameters
) extends XSBundle {
  val redirect = Input(Bool())
  val walk     = Input(Bool())

  val walkPhyReg   = Input(Vec(commitWidth,UInt(log2Up(numPhyRegs).W)))
  val walkReq = Input(Vec(commitWidth, Bool()))
  val allocateReq    = Input(Vec(RenameWidth, Bool()))
  val allocatePhyReg = Output(Vec(RenameWidth, UInt(log2Up(numPhyRegs).W)))
  val canAllocate    = Output(Bool())
  val doAllocate     = Input(Bool())

  val freeReq    = Input(Vec(commitWidth, Bool()))
  val freePhyReg = Input(Vec(commitWidth, UInt(log2Up(numPhyRegs).W)))
  val commit = Input(new NewFreeListCommitBundle(commitWidth,numPhyRegs))
  
  val snpt = Input(new SnapshotPort)
  
  val debug_UnusedRegCount = if(backendParams.debugEn) Some(Output(UInt(PhyRegIdxWidth.W))) else None
}

object FreeListBundle {
  def apply(numPhyRegs: Int, RenameWidth: Int, commitWidth: Int)(implicit p: Parameters) =
    new FreeListBundle(numPhyRegs, RenameWidth, commitWidth)
}

class NewFreeList(numPhyRegs: Int, commitWidth: Int, RenameWidth: Int,regType: RegType,numLogicRegs:Int = 32)(implicit p: Parameters)
    extends XSModule with HasXSParameter with HasPerfEvents{
  private val bankCount = RenameWidth / 2
  private val inBankRegCount   = numPhyRegs / bankCount
  val io                = IO(FreeListBundle(numPhyRegs, RenameWidth, commitWidth))
  require(RenameWidth % 2 == 0, "In FreeList RenameWidth must be even (current: %0d)");
  //Init
  def InitFreeList(reg_t: RegType) = {
    reg_t match {
      case Reg_I => RegInit(VecInit(Seq.tabulate(numPhyRegs)( i => if(i==0)false.B else true.B)))
      case Reg_F => RegInit(VecInit(Seq.tabulate(numPhyRegs)( i => if(i%(numPhyRegs/numLogicRegs)==0)false.B else true.B)))
      case Reg_V => RegInit(VecInit(Seq.tabulate(numPhyRegs)( i => if(i%(numPhyRegs/numLogicRegs)==0)false.B else true.B)))
      case _ => RegInit(VecInit(Seq.tabulate(numPhyRegs)( i => if(i<numLogicRegs)false.B else true.B)))
    }
  }

  val specfreeListReg   = InitFreeList(regType)
  val archfreeListReg   = InitFreeList(regType)

  //get free phyreg
  val getAllocatePhyReg   = Wire(Vec(RenameWidth, UInt(log2Up(numPhyRegs).W)))
  val ifCanAllocateReg    = Wire(Vec(RenameWidth, Bool()))
  for (bankIndex <- 0 until RenameWidth/2) {
    val BankBit = specfreeListReg.asUInt(inBankRegCount * (bankIndex + 1) - 1, inBankRegCount * bankIndex)
    val extendedBankBit = Cat(1.U(1.W), BankBit)
    val inBankIndex = ParallelPriorityEncoder(extendedBankBit)
    val reverseBankBit = Reverse(BankBit)
    val reverseExtendedBankBit = Cat(1.U(1.W), reverseBankBit)
    val inreverseBankIndex = ParallelPriorityEncoder(reverseExtendedBankBit)
    getAllocatePhyReg(bankIndex) := inBankIndex + (inBankRegCount * bankIndex).U(log2Up(numPhyRegs).W)
    getAllocatePhyReg(bankIndex+bankCount) :=  (inBankRegCount * (bankIndex+1)-1).U(log2Up(numPhyRegs).W) - inreverseBankIndex

    ifCanAllocateReg(bankIndex)  := inBankIndex < inBankRegCount.U
    ifCanAllocateReg(bankIndex+bankCount)  := (inBankIndex < inBankRegCount.U) && (getAllocatePhyReg(bankIndex) =/= getAllocatePhyReg(bankIndex+bankCount))
  }
  
  //allocate phyreg
  val salt = RegInit(0.U((log2Up(RenameWidth)-1).W))
  when(io.doAllocate){
    salt := salt+1.U
  }
  val ifCanAllocateRegOnSalt = Wire(Vec(RenameWidth, Bool()))
  for(i <- 0 until RenameWidth){
    val saltIndex = (salt + i.U(RenameWidth.W))(RenameWidth-1,0)
    ifCanAllocateRegOnSalt(i) := Mux(io.allocateReq(i),ifCanAllocateReg(saltIndex),true.B)
    io.allocatePhyReg(i) := getAllocatePhyReg(saltIndex)
  }
  io.canAllocate := ifCanAllocateRegOnSalt.asUInt.andR

  val AllocateOHOR = (0 until RenameWidth).map { i =>
    Mux(
      io.allocateReq(i),
      UIntToOH(io.allocatePhyReg(i), numPhyRegs),
      0.U(numPhyRegs.W)
    )
  }.reduce(_ | _)
  val freePhyRegOHOR = (0 until commitWidth).map { i =>
    Mux(
      io.freeReq(i),
      UIntToOH(io.freePhyReg(i), numPhyRegs),
      0.U(numPhyRegs.W)
    )
  }.reduce(_ | _)
  val walkPhyRegOHOR = (0 until commitWidth).map { i =>
    Mux(
      io.walkReq(i),
      UIntToOH(io.walkPhyReg(i), numPhyRegs),
      0.U(numPhyRegs.W)
    )
  }.reduce(_ | _)
  val commitPhyRegOHOR = (0 until commitWidth).map { i =>
    Mux(
      io.commit.archAlloc(i),
      UIntToOH(io.commit.archAllocPhyReg(i), numPhyRegs),
      0.U(numPhyRegs.W)
    )
  }.reduce(_ | _)

  val lastCycleRedirect = RegNext(RegNext(io.redirect))
  val lastCycleSnpt     = RegNext(RegNext(io.snpt, 0.U.asTypeOf(io.snpt)))
  val snapshots = FreeListSnapshotGenerator(specfreeListReg.asUInt|freePhyRegOHOR, io.snpt.snptEnq, io.snpt.snptDeq, io.redirect, io.snpt.flushVec, freePhyRegOHOR,numPhyRegs)

  val redirectedFreeList = Mux(
    lastCycleSnpt.useSnpt,
    snapshots(lastCycleSnpt.snptSelect) & ~walkPhyRegOHOR,
    archfreeListReg.asUInt & ~walkPhyRegOHOR
  )
  
  val isWalkAlloc = io.walk && io.doAllocate
  val isNormalAlloc = io.canAllocate && io.doAllocate
  val isAllocate = isWalkAlloc || isNormalAlloc

  val allocate = Mux(io.walk,walkPhyRegOHOR,AllocateOHOR)
  val freeListRegAllocate = Mux(lastCycleRedirect, redirectedFreeList, specfreeListReg.asUInt & (~allocate))
  // priority: (1) exception and flushPipe; (2) walking; (3) mis-prediction; (4) normal dequeue
  val realDoAllocate = !io.redirect && isAllocate
  specfreeListReg := VecInit((Mux(realDoAllocate, freeListRegAllocate, specfreeListReg.asUInt)|freePhyRegOHOR).asBools)
  
  archfreeListReg := VecInit((Mux(io.commit.doCommit,archfreeListReg.asUInt & ~commitPhyRegOHOR,archfreeListReg.asUInt)|freePhyRegOHOR).asBools)

  //Debug
  XSPerfAccumulate("utilization", PopCount(io.allocateReq))
  XSPerfAccumulate("allocation_blocked_cycle", !io.canAllocate)
  
  val freeListSize = numPhyRegs - numLogicRegs
  val freeRegCnt = PopCount(specfreeListReg.asUInt)
  io.debug_UnusedRegCount.foreach(_ := freeRegCnt)
  val freeRegCntReg = RegNext(freeRegCnt)
  val perfEvents = Seq(
    ("std_freelist_1_4_valid", freeRegCntReg <  (freeListSize / 4).U                                            ),
    ("std_freelist_2_4_valid", freeRegCntReg >= (freeListSize / 4).U && freeRegCntReg < (freeListSize / 2).U    ),
    ("std_freelist_3_4_valid", freeRegCntReg >= (freeListSize / 2).U && freeRegCntReg < (freeListSize * 3 / 4).U),
    ("std_freelist_4_4_valid", freeRegCntReg >= (freeListSize * 3 / 4).U                                        )
  )

  QueuePerf(size = freeListSize, utilization = freeRegCntReg, full = freeRegCntReg === 0.U)

  generatePerfEvent()
}

class NewFreeListCommitBundle(commitWidth: Int,numPhyRegs: Int) extends Bundle {
  val doCommit = Bool()
  val archAlloc = Vec(commitWidth, Bool())
  val archAllocPhyReg = Vec(commitWidth, UInt(log2Up(numPhyRegs).W))
}
