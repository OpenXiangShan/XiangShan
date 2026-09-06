package xiangshan.backend.rename.freelist

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan._
import xiangshan.backend.rename.FreeListSnapshotGenerator
import xiangshan.backend.rename.RegType
import xiangshan.backend.rename.Reg_I
import xiangshan.backend.rename.Reg_F
import xiangshan.backend.rename.Reg_V

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

object NewFreeList {
  val DefaultS1QueueSize = NewFLManager.DefaultS1QueueSize
}

class NewFreeList(
  numPhyRegs: Int,
  commitWidth: Int,
  RenameWidth: Int,
  regType: RegType,
  numLogicRegs: Int = 32,
  s1QueueSize: Int = NewFreeList.DefaultS1QueueSize
)(implicit p: Parameters)
    extends XSModule with HasXSParameter with HasPerfEvents{
  val io                = IO(FreeListBundle(numPhyRegs, RenameWidth, commitWidth))
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

  /** NewFreeList owns the free bitmaps; NewFLManager owns preg allocation. */
  val flManager = Module(new NewFLManager(numPhyRegs, RenameWidth, s1QueueSize))
  flManager.in.freeBitmap := specfreeListReg.asUInt
  flManager.in.allocateReq := io.allocateReq
  flManager.in.doAllocate := io.doAllocate && !io.walk
  flManager.in.flush := io.redirect || io.walk

  io.allocatePhyReg := flManager.out.allocatePhyReg
  io.canAllocate := flManager.out.canAllocate

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

  val allocate = Mux(io.walk,walkPhyRegOHOR,flManager.out.allocateBitmap)
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
