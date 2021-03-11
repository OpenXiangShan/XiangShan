package xiangshan.backend.rename

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.roq.RoqPtr
import xiangshan.backend.dispatch.PreDispatchInfo

class RenameBypassInfo extends XSBundle {
  val lsrc1_bypass = MixedVec(List.tabulate(RenameWidth-1)(i => UInt((i+1).W)))
  val lsrc2_bypass = MixedVec(List.tabulate(RenameWidth-1)(i => UInt((i+1).W)))
  val lsrc3_bypass = MixedVec(List.tabulate(RenameWidth-1)(i => UInt((i+1).W)))
  val ldest_bypass = MixedVec(List.tabulate(RenameWidth-1)(i => UInt((i+1).W)))
  val move_eliminated_src1 = Vec(RenameWidth-1, Bool())
  val move_eliminated_src2 = Vec(RenameWidth-1, Bool())
}

class Rename extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val flush = Input(Bool())
    val roqCommits = Flipped(new RoqCommitIO)
    // from decode buffer
    val in = Vec(RenameWidth, Flipped(DecoupledIO(new CfCtrl)))
    // to dispatch1
    val out = Vec(RenameWidth, DecoupledIO(new MicroOp))
    val renameBypass = Output(new RenameBypassInfo)
    val dispatchInfo = Output(new PreDispatchInfo)
    val csrCtrl = Flipped(new CustomCSRCtrlIO)
  })

  def printRenameInfo(in: DecoupledIO[CfCtrl], out: DecoupledIO[MicroOp]) = {
    XSInfo(
      in.valid && in.ready,
      p"pc:${Hexadecimal(in.bits.cf.pc)} in v:${in.valid} in rdy:${in.ready} " +
        p"lsrc1:${in.bits.ctrl.lsrc1} -> psrc1:${out.bits.psrc1} " +
        p"lsrc2:${in.bits.ctrl.lsrc2} -> psrc2:${out.bits.psrc2} " +
        p"lsrc3:${in.bits.ctrl.lsrc3} -> psrc3:${out.bits.psrc3} " +
        p"ldest:${in.bits.ctrl.ldest} -> pdest:${out.bits.pdest} " +
        p"old_pdest:${out.bits.old_pdest} " +
        p"out v:${out.valid} r:${out.ready}\n"
    )
  }

  for((x,y) <- io.in.zip(io.out)){
    printRenameInfo(x, y)
  }

  val intFreeList, fpFreeList = Module(new FreeList).io
  val intRat = Module(new RenameTable(float = false)).io
  val fpRat = Module(new RenameTable(float = true)).io
  val allPhyResource = Seq((intRat, intFreeList, false), (fpRat, fpFreeList, true))

  allPhyResource.map{ case (rat, freelist, _) =>
    rat.redirect := io.redirect.valid
    rat.flush := io.flush
    rat.walkWen := io.roqCommits.isWalk
    freelist.redirect := io.redirect.valid
    freelist.flush := io.flush
    freelist.walk.valid := io.roqCommits.isWalk
  }
  val canOut = io.out(0).ready && fpFreeList.req.canAlloc && intFreeList.req.canAlloc && !io.roqCommits.isWalk

  def needDestReg[T <: CfCtrl](fp: Boolean, x: T): Bool = {
    {if(fp) x.ctrl.fpWen else x.ctrl.rfWen && (x.ctrl.ldest =/= 0.U)}
  }
  def needDestRegCommit[T <: RoqCommitInfo](fp: Boolean, x: T): Bool = {
    {if(fp) x.fpWen else x.rfWen && (x.ldest =/= 0.U)}
  }
  fpFreeList.walk.bits := PopCount(io.roqCommits.valid.zip(io.roqCommits.info).map{case (v, i) => v && needDestRegCommit(true, i)})
  intFreeList.walk.bits := PopCount(io.roqCommits.valid.zip(io.roqCommits.info).map{case (v, i) => v && needDestRegCommit(false, i)})
  // walk has higher priority than allocation and thus we don't use isWalk here
  fpFreeList.req.doAlloc := intFreeList.req.canAlloc && io.out(0).ready
  intFreeList.req.doAlloc := fpFreeList.req.canAlloc && io.out(0).ready

  // speculatively assign the instruction with an roqIdx
  val validCount = PopCount(io.in.map(_.valid))
  val roqIdxHead = RegInit(0.U.asTypeOf(new RoqPtr))
  val lastCycleMisprediction = RegNext(io.redirect.valid && !io.redirect.bits.flushItself())
  val roqIdxHeadNext = Mux(io.flush,
    0.U.asTypeOf(new RoqPtr),
    Mux(io.redirect.valid,
      io.redirect.bits.roqIdx,
      Mux(lastCycleMisprediction,
        roqIdxHead + 1.U,
        Mux(canOut, roqIdxHead + validCount, roqIdxHead))
    )
  )
  roqIdxHead := roqIdxHeadNext

  /**
    * Rename: allocate free physical register and update rename table
    */
  val uops = Wire(Vec(RenameWidth, new MicroOp))

  uops.foreach( uop => {
//    uop.brMask := DontCare
//    uop.brTag := DontCare
    uop.src1State := DontCare
    uop.src2State := DontCare
    uop.src3State := DontCare
    uop.roqIdx := DontCare
    uop.diffTestDebugLrScValid := DontCare
    uop.debugInfo := DontCare
    uop.lqIdx := DontCare
    uop.sqIdx := DontCare
  })

  val needFpDest = Wire(Vec(RenameWidth, Bool()))
  val needIntDest = Wire(Vec(RenameWidth, Bool()))
  val hasValid = Cat(io.in.map(_.valid)).orR
  for (i <- 0 until RenameWidth) {
    uops(i).cf := io.in(i).bits.cf
    uops(i).ctrl := io.in(i).bits.ctrl

    val inValid = io.in(i).valid

    // alloc a new phy reg
    needFpDest(i) := inValid && needDestReg(fp = true, io.in(i).bits)
    needIntDest(i) := inValid && needDestReg(fp = false, io.in(i).bits)
    fpFreeList.req.allocReqs(i) := needFpDest(i)
    intFreeList.req.allocReqs(i) := needIntDest(i)

    io.in(i).ready := !hasValid || canOut

    // do checkpoints when a branch inst come
    // for(fl <- Seq(fpFreeList, intFreeList)){
    //   fl.cpReqs(i).valid := inValid
    //   fl.cpReqs(i).bits := io.in(i).bits.brTag
    // }

    uops(i).pdest := Mux(needIntDest(i),
      intFreeList.req.pdests(i),
      Mux(
        uops(i).ctrl.ldest===0.U && uops(i).ctrl.rfWen,
        0.U, fpFreeList.req.pdests(i)
      )
    )

    uops(i).roqIdx := roqIdxHead + i.U

    io.out(i).valid := io.in(i).valid && intFreeList.req.canAlloc && fpFreeList.req.canAlloc && !io.roqCommits.isWalk
    io.out(i).bits := uops(i)

    // write speculative rename table
    allPhyResource.map{ case (rat, freelist, _) =>
      val specWen = freelist.req.allocReqs(i) && freelist.req.canAlloc && freelist.req.doAlloc && !io.roqCommits.isWalk

      rat.specWritePorts(i).wen := specWen
      rat.specWritePorts(i).addr := uops(i).ctrl.ldest
      rat.specWritePorts(i).wdata := freelist.req.pdests(i)

      freelist.deallocReqs(i) := specWen
    }

    // read rename table
    def readRat(lsrcList: List[UInt], ldest: UInt, fp: Boolean) = {
      val rat = if(fp) fpRat else intRat
      val srcCnt = lsrcList.size
      val psrcVec = Wire(Vec(srcCnt, UInt(PhyRegIdxWidth.W)))
      val old_pdest = Wire(UInt(PhyRegIdxWidth.W))
      for(k <- 0 until srcCnt+1){
        val rportIdx = i * (srcCnt+1) + k
        if(k != srcCnt){
          rat.readPorts(rportIdx).addr := lsrcList(k)
          psrcVec(k) := rat.readPorts(rportIdx).rdata
        } else {
          rat.readPorts(rportIdx).addr := ldest
          old_pdest := rat.readPorts(rportIdx).rdata
        }
      }
      (psrcVec, old_pdest)
    }
    val lsrcList = List(uops(i).ctrl.lsrc1, uops(i).ctrl.lsrc2, uops(i).ctrl.lsrc3)
    val ldest = uops(i).ctrl.ldest
    val (intPhySrcVec, intOldPdest) = readRat(lsrcList.take(2), ldest, fp = false)
    val (fpPhySrcVec, fpOldPdest) = readRat(lsrcList, ldest, fp = true)
    uops(i).psrc1 := Mux(uops(i).ctrl.src1Type === SrcType.reg, intPhySrcVec(0), fpPhySrcVec(0))
    uops(i).psrc2 := Mux(uops(i).ctrl.src2Type === SrcType.reg, intPhySrcVec(1), fpPhySrcVec(1))
    uops(i).psrc3 := fpPhySrcVec(2)
    uops(i).old_pdest := Mux(uops(i).ctrl.rfWen, intOldPdest, fpOldPdest)
  }

  // We don't bypass the old_pdest from valid instructions with the same ldest currently in rename stage.
  // Instead, we determine whether there're some dependences between the valid instructions.
  for (i <- 1 until RenameWidth) {
    io.renameBypass.lsrc1_bypass(i-1) := Cat((0 until i).map(j => {
      val fpMatch  = needFpDest(j) && io.in(i).bits.ctrl.src1Type === SrcType.fp
      val intMatch = needIntDest(j) && io.in(i).bits.ctrl.src1Type === SrcType.reg
      (fpMatch || intMatch) && io.in(j).bits.ctrl.ldest === io.in(i).bits.ctrl.lsrc1
    }).reverse)
    io.renameBypass.lsrc2_bypass(i-1) := Cat((0 until i).map(j => {
      val fpMatch  = needFpDest(j) && io.in(i).bits.ctrl.src2Type === SrcType.fp
      val intMatch = needIntDest(j) && io.in(i).bits.ctrl.src2Type === SrcType.reg
      (fpMatch || intMatch) && io.in(j).bits.ctrl.ldest === io.in(i).bits.ctrl.lsrc2
    }).reverse)
    io.renameBypass.lsrc3_bypass(i-1) := Cat((0 until i).map(j => {
      val fpMatch  = needFpDest(j) && io.in(i).bits.ctrl.src3Type === SrcType.fp
      val intMatch = needIntDest(j) && io.in(i).bits.ctrl.src3Type === SrcType.reg
      (fpMatch || intMatch) && io.in(j).bits.ctrl.ldest === io.in(i).bits.ctrl.lsrc3
    }).reverse)
    io.renameBypass.ldest_bypass(i-1) := Cat((0 until i).map(j => {
      val fpMatch  = needFpDest(j) && needFpDest(i)
      val intMatch = needIntDest(j) && needIntDest(i)
      (fpMatch || intMatch) && io.in(j).bits.ctrl.ldest === io.in(i).bits.ctrl.ldest
    }).reverse)
    io.renameBypass.move_eliminated_src1(i-1) :=
      // the producer move instruction writes to non-zero register
      io.in(i-1).bits.ctrl.isMove && io.in(i-1).bits.ctrl.ldest =/= 0.U &&
      // the consumer instruction uses the move's destination register
      io.in(i).bits.ctrl.src1Type === SrcType.reg && io.in(i).bits.ctrl.lsrc1 === io.in(i-1).bits.ctrl.ldest &&
      // CSR control (by srnctl)
      io.csrCtrl.move_elim_enable
    io.renameBypass.move_eliminated_src2(i-1) :=
      // the producer move instruction writes to non-zero register
      io.in(i-1).bits.ctrl.isMove && io.in(i-1).bits.ctrl.ldest =/= 0.U &&
      // the consumer instruction uses the move's destination register
      io.in(i).bits.ctrl.src2Type === SrcType.reg && io.in(i).bits.ctrl.lsrc2 === io.in(i-1).bits.ctrl.ldest &&
      // CSR control (by srnctl)
      io.csrCtrl.move_elim_enable
  }

  val isLs    = VecInit(uops.map(uop => FuType.isLoadStore(uop.ctrl.fuType)))
  val isStore = VecInit(uops.map(uop => FuType.isStoreExu(uop.ctrl.fuType)))
  val isAMO   = VecInit(uops.map(uop => FuType.isAMO(uop.ctrl.fuType)))
  io.dispatchInfo.lsqNeedAlloc := VecInit((0 until RenameWidth).map(i =>
    Mux(isLs(i), Mux(isStore(i) && !isAMO(i), 2.U, 1.U), 0.U)))

  /**
    * Instructions commit: update freelist and rename table
    */
  for (i <- 0 until CommitWidth) {
    if (i >= RenameWidth) {
      allPhyResource.map{ case (rat, _, _) =>
        rat.specWritePorts(i).wen   := false.B
        rat.specWritePorts(i).addr  := DontCare
        rat.specWritePorts(i).wdata := DontCare
      }
    }

    allPhyResource.map{ case (rat, freelist, fp) =>
      // walk back write
      val commitDestValid = io.roqCommits.valid(i) && needDestRegCommit(fp, io.roqCommits.info(i))

      when (commitDestValid && io.roqCommits.isWalk) {
        rat.specWritePorts(i).wen := true.B
        rat.specWritePorts(i).addr := io.roqCommits.info(i).ldest
        rat.specWritePorts(i).wdata := io.roqCommits.info(i).old_pdest
        XSInfo({if(fp) p"fp" else p"int "} + p"walk: " +
          p" ldest:${rat.specWritePorts(i).addr} old_pdest:${rat.specWritePorts(i).wdata}\n")
      }

      rat.archWritePorts(i).wen := commitDestValid && !io.roqCommits.isWalk
      rat.archWritePorts(i).addr := io.roqCommits.info(i).ldest
      rat.archWritePorts(i).wdata := io.roqCommits.info(i).pdest

      XSInfo(rat.archWritePorts(i).wen,
        {if(fp) p"fp" else p"int "} + p" rat arch: ldest:${rat.archWritePorts(i).addr}" +
          p" pdest:${rat.archWritePorts(i).wdata}\n"
      )

      freelist.deallocReqs(i) := rat.archWritePorts(i).wen
      freelist.deallocPregs(i) := io.roqCommits.info(i).old_pdest
    }
  }

  XSPerf("in", Mux(RegNext(io.in(0).ready), PopCount(io.in.map(_.valid)), 0.U))
  XSPerf("utilization", PopCount(io.in.map(_.valid)))
  XSPerf("waitInstr", PopCount((0 until RenameWidth).map(i => io.in(i).valid && !io.in(i).ready)))
  XSPerf("stall_cycle_dispatch", hasValid && !io.out(0).ready && fpFreeList.req.canAlloc && intFreeList.req.canAlloc && !io.roqCommits.isWalk)
  XSPerf("stall_cycle_fp", hasValid && io.out(0).ready && !fpFreeList.req.canAlloc && intFreeList.req.canAlloc && !io.roqCommits.isWalk)
  XSPerf("stall_cycle_int", hasValid && io.out(0).ready && fpFreeList.req.canAlloc && !intFreeList.req.canAlloc && !io.roqCommits.isWalk)
  XSPerf("stall_cycle_walk", hasValid && io.out(0).ready && fpFreeList.req.canAlloc && intFreeList.req.canAlloc && io.roqCommits.isWalk)

}
