/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.backend.rename

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.dispatch.PreDispatchInfo

class RenameBypassInfo(implicit p: Parameters) extends XSBundle {
  val lsrc1_bypass = MixedVec(List.tabulate(RenameWidth-1)(i => UInt((i+1).W)))
  val lsrc2_bypass = MixedVec(List.tabulate(RenameWidth-1)(i => UInt((i+1).W)))
  val lsrc3_bypass = MixedVec(List.tabulate(RenameWidth-1)(i => UInt((i+1).W)))
  val ldest_bypass = MixedVec(List.tabulate(RenameWidth-1)(i => UInt((i+1).W)))
}

class Rename(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val flush = Input(Bool())
    val robCommits = Flipped(new RobCommitIO)
    // from decode
    val in = Vec(RenameWidth, Flipped(DecoupledIO(new CfCtrl)))
    // to rename table
    val intReadPorts = Vec(RenameWidth, Vec(3, Input(UInt(PhyRegIdxWidth.W))))
    val fpReadPorts = Vec(RenameWidth, Vec(4, Input(UInt(PhyRegIdxWidth.W))))
    val intRenamePorts = Vec(RenameWidth, Output(new RatWritePort))
    val fpRenamePorts = Vec(RenameWidth, Output(new RatWritePort))
    // to dispatch1
    val out = Vec(RenameWidth, DecoupledIO(new MicroOp))
    val renameBypass = Output(new RenameBypassInfo)
    val dispatchInfo = Output(new PreDispatchInfo)
  })

  // create free list and rat
  val intFreeList = Module(new freelist.MEFreeList)
  val fpFreeList = Module(new freelist.StdFreeList)

  // decide if given instruction needs allocating a new physical register (CfCtrl: from decode; RobCommitInfo: from rob)
  def needDestReg[T <: CfCtrl](fp: Boolean, x: T): Bool = {
    {if(fp) x.ctrl.fpWen else x.ctrl.rfWen && (x.ctrl.ldest =/= 0.U)}
  }
  def needDestRegCommit[T <: RobCommitInfo](fp: Boolean, x: T): Bool = {
    {if(fp) x.fpWen else x.rfWen && (x.ldest =/= 0.U)}
  }

  // connect [flush + redirect + walk] ports for __float point__ & __integer__ free list
  Seq((fpFreeList, true), (intFreeList, false)).foreach{ case (fl, isFp) =>
    fl.flush := io.flush
    fl.redirect := io.redirect.valid
    fl.walk := io.robCommits.isWalk
    // when isWalk, use stepBack to restore head pointer of free list
    // (if ME enabled, stepBack of intFreeList should be useless thus optimized out)
    fl.stepBack := PopCount(io.robCommits.valid.zip(io.robCommits.info).map{case (v, i) => v && needDestRegCommit(isFp, i)})
  }
  // walk has higher priority than allocation and thus we don't use isWalk here
  // only when both fp and int free list and dispatch1 has enough space can we do allocation
  intFreeList.doAllocate := fpFreeList.canAllocate && io.out(0).ready
  fpFreeList.doAllocate := intFreeList.canAllocate && io.out(0).ready

  //           dispatch1 ready ++ float point free list ready ++ int free list ready      ++ not walk
  val canOut = io.out(0).ready && fpFreeList.canAllocate && intFreeList.canAllocate && !io.robCommits.isWalk


  // speculatively assign the instruction with an robIdx
  val validCount = PopCount(io.in.map(_.valid)) // number of instructions waiting to enter rob (from decode)
  val robIdxHead = RegInit(0.U.asTypeOf(new RobPtr))
  val lastCycleMisprediction = RegNext(io.redirect.valid && !io.redirect.bits.flushItself())
  val robIdxHeadNext = Mux(io.flush, 0.U.asTypeOf(new RobPtr), // flush: clear rob
              Mux(io.redirect.valid, io.redirect.bits.robIdx, // redirect: move ptr to given rob index (flush itself)
         Mux(lastCycleMisprediction, robIdxHead + 1.U, // mis-predict: not flush robIdx itself
                         Mux(canOut, robIdxHead + validCount, // instructions successfully entered next stage: increase robIdx
                      /* default */  robIdxHead)))) // no instructions passed by this cycle: stick to old value
  robIdxHead := robIdxHeadNext

  /**
    * Rename: allocate free physical register and update rename table
    */
  val uops = Wire(Vec(RenameWidth, new MicroOp))
  uops.foreach( uop => {
    uop.srcState(0) := DontCare
    uop.srcState(1) := DontCare
    uop.srcState(2) := DontCare
    uop.robIdx := DontCare
    uop.diffTestDebugLrScValid := DontCare
    uop.debugInfo := DontCare
    uop.lqIdx := DontCare
    uop.sqIdx := DontCare
  })

  val needFpDest = Wire(Vec(RenameWidth, Bool()))
  val needIntDest = Wire(Vec(RenameWidth, Bool()))
  val hasValid = Cat(io.in.map(_.valid)).orR

  val isMove = io.in.map(_.bits.ctrl.isMove)
  val isMax = intFreeList.maxVec
  val meEnable = WireInit(VecInit(Seq.fill(RenameWidth)(false.B)))
  val psrc_cmp = Wire(MixedVec(List.tabulate(RenameWidth-1)(i => UInt((i+1).W))))
  val intPsrc = Wire(Vec(RenameWidth, UInt()))

  val intSpecWen = Wire(Vec(RenameWidth, Bool()))
  val fpSpecWen = Wire(Vec(RenameWidth, Bool()))

  // uop calculation
  for (i <- 0 until RenameWidth) {
    uops(i).cf := io.in(i).bits.cf
    uops(i).ctrl := io.in(i).bits.ctrl

    val inValid = io.in(i).valid

    // alloc a new phy reg
    needFpDest(i) := inValid && needDestReg(fp = true, io.in(i).bits)
    needIntDest(i) := inValid && needDestReg(fp = false, io.in(i).bits)
    fpFreeList.allocateReq(i) := needFpDest(i)
    intFreeList.allocateReq(i) := needIntDest(i)

    // no valid instruction from decode stage || all resources (dispatch1 + both free lists) ready
    io.in(i).ready := !hasValid || canOut

    // do checkpoints when a branch inst come
    // for(fl <- Seq(fpFreeList, intFreeList)){
    //   fl.cpReqs(i).valid := inValid
    //   fl.cpReqs(i).bits := io.in(i).bits.brTag
    // }

    uops(i).robIdx := robIdxHead + PopCount(io.in.take(i).map(_.valid))

    val intPhySrcVec = io.intReadPorts(i).take(2)
    val intOldPdest = io.intReadPorts(i).last
    intPsrc(i) := intPhySrcVec(0)
    val fpPhySrcVec = io.fpReadPorts(i).take(3)
    val fpOldPdest = io.fpReadPorts(i).last
    uops(i).psrc(0) := Mux(uops(i).ctrl.srcType(0) === SrcType.reg, intPhySrcVec(0), fpPhySrcVec(0))
    uops(i).psrc(1) := Mux(uops(i).ctrl.srcType(1) === SrcType.reg, intPhySrcVec(1), fpPhySrcVec(1))
    uops(i).psrc(2) := fpPhySrcVec(2)
    uops(i).old_pdest := Mux(uops(i).ctrl.rfWen, intOldPdest, fpOldPdest)

    if (i == 0) {
      // calculate meEnable
      meEnable(i) := isMove(i) && (!isMax(intPsrc(i)) || uops(i).ctrl.lsrc(0) === 0.U)
    } else {
      // compare psrc0
      psrc_cmp(i-1) := Cat((0 until i).map(j => {
        intPsrc(i) === intPsrc(j) && io.in(i).bits.ctrl.isMove && io.in(j).bits.ctrl.isMove
      }) /* reverse is not necessary here */)

      // calculate meEnable
      meEnable(i) := isMove(i) && (!(io.renameBypass.lsrc1_bypass(i-1).orR | psrc_cmp(i-1).orR | isMax(intPsrc(i))) || uops(i).ctrl.lsrc(0) === 0.U)
    }
    uops(i).eliminatedMove := meEnable(i) || (uops(i).ctrl.isMove && uops(i).ctrl.ldest === 0.U)

    // send psrc of eliminated move instructions to free list and label them as eliminated
    intFreeList.psrcOfMove(i).valid := meEnable(i)
    intFreeList.psrcOfMove(i).bits := intPsrc(i)

    // update pdest
    uops(i).pdest := Mux(meEnable(i), intPsrc(i), // move eliminated
                     Mux(needIntDest(i), intFreeList.allocatePhyReg(i), // normal int inst
                     Mux(uops(i).ctrl.ldest===0.U && uops(i).ctrl.rfWen, 0.U // int inst with dst=r0
                     /* default */, fpFreeList.allocatePhyReg(i)))) // normal fp inst

    // Assign performance counters
    uops(i).debugInfo.renameTime := GTimer()

    io.out(i).valid := io.in(i).valid && intFreeList.canAllocate && fpFreeList.canAllocate && !io.robCommits.isWalk
    io.out(i).bits := uops(i)

    // write speculative rename table
    // we update rat later inside commit code
    intSpecWen(i) := intFreeList.allocateReq(i) && intFreeList.canAllocate && intFreeList.doAllocate && !io.robCommits.isWalk
    fpSpecWen(i) := fpFreeList.allocateReq(i) && fpFreeList.canAllocate && fpFreeList.doAllocate && !io.robCommits.isWalk
  }

  // We don't bypass the old_pdest from valid instructions with the same ldest currently in rename stage.
  // Instead, we determine whether there're some dependencies between the valid instructions.
  for (i <- 1 until RenameWidth) {
    io.renameBypass.lsrc1_bypass(i-1) := Cat((0 until i).map(j => {
      val fpMatch  = needFpDest(j) && io.in(i).bits.ctrl.srcType(0) === SrcType.fp
      val intMatch = needIntDest(j) && io.in(i).bits.ctrl.srcType(0) === SrcType.reg
      (fpMatch || intMatch) && io.in(j).bits.ctrl.ldest === io.in(i).bits.ctrl.lsrc(0)
    }).reverse)
    io.renameBypass.lsrc2_bypass(i-1) := Cat((0 until i).map(j => {
      val fpMatch  = needFpDest(j) && io.in(i).bits.ctrl.srcType(1) === SrcType.fp
      val intMatch = needIntDest(j) && io.in(i).bits.ctrl.srcType(1) === SrcType.reg
      (fpMatch || intMatch) && io.in(j).bits.ctrl.ldest === io.in(i).bits.ctrl.lsrc(1)
    }).reverse)
    io.renameBypass.lsrc3_bypass(i-1) := Cat((0 until i).map(j => {
      val fpMatch  = needFpDest(j) && io.in(i).bits.ctrl.srcType(2) === SrcType.fp
      val intMatch = needIntDest(j) && io.in(i).bits.ctrl.srcType(2) === SrcType.reg
      (fpMatch || intMatch) && io.in(j).bits.ctrl.ldest === io.in(i).bits.ctrl.lsrc(2)
    }).reverse)
    io.renameBypass.ldest_bypass(i-1) := Cat((0 until i).map(j => {
      val fpMatch  = needFpDest(j) && needFpDest(i)
      val intMatch = needIntDest(j) && needIntDest(i)
      (fpMatch || intMatch) && io.in(j).bits.ctrl.ldest === io.in(i).bits.ctrl.ldest
    }).reverse)
  }

  // calculate lsq space requirement
  val isLs    = VecInit(uops.map(uop => FuType.isLoadStore(uop.ctrl.fuType)))
  val isStore = VecInit(uops.map(uop => FuType.isStoreExu(uop.ctrl.fuType)))
  val isAMO   = VecInit(uops.map(uop => FuType.isAMO(uop.ctrl.fuType)))
  io.dispatchInfo.lsqNeedAlloc := VecInit((0 until RenameWidth).map(i =>
    Mux(isLs(i), Mux(isStore(i) && !isAMO(i), 2.U, 1.U), 0.U)))

  /**
    * Instructions commit: update freelist and rename table
    */
  for (i <- 0 until CommitWidth) {

    Seq((io.intRenamePorts, false), (io.fpRenamePorts, true)) foreach { case (rat, fp) =>
      // is valid commit req and given instruction has destination register
      val commitDestValid = io.robCommits.valid(i) && needDestRegCommit(fp, io.robCommits.info(i))
      XSDebug(p"isFp[${fp}]index[$i]-commitDestValid:$commitDestValid,isWalk:${io.robCommits.isWalk}\n")

      /*
      I. RAT Update
       */

      // walk back write - restore spec state : ldest => old_pdest
      if (fp && i < RenameWidth) {
        // When redirect happens (mis-prediction), don't update the rename table
        rat(i).wen := fpSpecWen(i) && !io.flush && !io.redirect.valid
        rat(i).addr := uops(i).ctrl.ldest
        rat(i).data := fpFreeList.allocatePhyReg(i)
      } else if (!fp && i < RenameWidth) {
        rat(i).wen := intSpecWen(i) && !io.flush && !io.redirect.valid
        rat(i).addr := uops(i).ctrl.ldest
        rat(i).data := Mux(meEnable(i), intPsrc(i), intFreeList.allocatePhyReg(i))
      }

      /*
      II. Free List Update
       */
      if (fp) { // Float Point free list
        fpFreeList.freeReq(i)  := commitDestValid && !io.robCommits.isWalk
        fpFreeList.freePhyReg(i) := io.robCommits.info(i).old_pdest
      } else { // Integer free list

        // during walk process:
        // 1. for normal inst, free pdest + revert rat from ldest->pdest to ldest->old_pdest
        // 2. for ME inst, free pdest(commit counter++) + revert rat

        // conclusion:
        // a. rat recovery has nothing to do with ME or not
        // b. treat walk as normal commit except replace old_pdests with pdests and set io.walk to true
        // c. ignore pdests port when walking

        intFreeList.freeReq(i) := commitDestValid // walk or not walk
        intFreeList.freePhyReg(i)  := Mux(io.robCommits.isWalk, io.robCommits.info(i).pdest, io.robCommits.info(i).old_pdest)
        intFreeList.eliminatedMove(i) := io.robCommits.info(i).eliminatedMove
        intFreeList.multiRefPhyReg(i) := io.robCommits.info(i).pdest
      }
    }
  }


  /*
  Debug and performance counter
   */

  def printRenameInfo(in: DecoupledIO[CfCtrl], out: DecoupledIO[MicroOp]) = {
    XSInfo(
      in.valid && in.ready,
      p"pc:${Hexadecimal(in.bits.cf.pc)} in v:${in.valid} in rdy:${in.ready} " +
        p"lsrc(0):${in.bits.ctrl.lsrc(0)} -> psrc(0):${out.bits.psrc(0)} " +
        p"lsrc(1):${in.bits.ctrl.lsrc(1)} -> psrc(1):${out.bits.psrc(1)} " +
        p"lsrc(2):${in.bits.ctrl.lsrc(2)} -> psrc(2):${out.bits.psrc(2)} " +
        p"ldest:${in.bits.ctrl.ldest} -> pdest:${out.bits.pdest} " +
        p"old_pdest:${out.bits.old_pdest} " +
        p"out v:${out.valid} r:${out.ready}\n"
    )
  }

  for((x,y) <- io.in.zip(io.out)){
    printRenameInfo(x, y)
  }

  XSDebug(io.robCommits.isWalk, p"Walk Recovery Enabled\n")
  XSDebug(io.robCommits.isWalk, p"validVec:${Binary(io.robCommits.valid.asUInt)}\n")
  for (i <- 0 until CommitWidth) {
    val info = io.robCommits.info(i)
    XSDebug(io.robCommits.isWalk && io.robCommits.valid(i), p"[#$i walk info] pc:${Hexadecimal(info.pc)} " +
      p"ldest:${info.ldest} rfWen:${info.rfWen} fpWen:${info.fpWen} " + p"eliminatedMove:${info.eliminatedMove} " +
      p"pdest:${info.pdest} old_pdest:${info.old_pdest}\n")
  }

  XSDebug(p"inValidVec: ${Binary(Cat(io.in.map(_.valid)))}\n")
  XSInfo(!canOut, p"stall at rename, hasValid:${hasValid}, fpCanAlloc:${fpFreeList.canAllocate}, intCanAlloc:${intFreeList.canAllocate} dispatch1ready:${io.out(0).ready}, isWalk:${io.robCommits.isWalk}\n")

  XSPerfAccumulate("in", Mux(RegNext(io.in(0).ready), PopCount(io.in.map(_.valid)), 0.U))
  XSPerfAccumulate("utilization", PopCount(io.in.map(_.valid)))
  XSPerfAccumulate("waitInstr", PopCount((0 until RenameWidth).map(i => io.in(i).valid && !io.in(i).ready)))
  XSPerfAccumulate("stall_cycle_dispatch", hasValid && !io.out(0).ready && fpFreeList.canAllocate && intFreeList.canAllocate && !io.robCommits.isWalk)
  XSPerfAccumulate("stall_cycle_fp", hasValid && io.out(0).ready && !fpFreeList.canAllocate && intFreeList.canAllocate && !io.robCommits.isWalk)
  XSPerfAccumulate("stall_cycle_int", hasValid && io.out(0).ready && fpFreeList.canAllocate && !intFreeList.canAllocate && !io.robCommits.isWalk)
  XSPerfAccumulate("stall_cycle_walk", hasValid && io.out(0).ready && fpFreeList.canAllocate && intFreeList.canAllocate && io.robCommits.isWalk)

  XSPerfAccumulate("move_instr_count", PopCount(Seq.tabulate(RenameWidth)(i => io.out(i).fire() && io.in(i).bits.ctrl.isMove)))
  XSPerfAccumulate("move_elim_enabled", PopCount(Seq.tabulate(RenameWidth)(i => io.out(i).fire() && meEnable(i))))
  XSPerfAccumulate("move_elim_cancelled", PopCount(Seq.tabulate(RenameWidth)(i => io.out(i).fire() && io.in(i).bits.ctrl.isMove && !meEnable(i))))
  XSPerfAccumulate("move_elim_cancelled_psrc_bypass", PopCount(Seq.tabulate(RenameWidth)(i => io.out(i).fire() && io.in(i).bits.ctrl.isMove && !meEnable(i) && { if (i == 0) false.B else io.renameBypass.lsrc1_bypass(i-1).orR })))
  XSPerfAccumulate("move_elim_cancelled_cnt_limit", PopCount(Seq.tabulate(RenameWidth)(i => io.out(i).fire() && io.in(i).bits.ctrl.isMove && !meEnable(i) && isMax(io.out(i).bits.psrc(0)))))
  XSPerfAccumulate("move_elim_cancelled_inc_more_than_one", PopCount(Seq.tabulate(RenameWidth)(i => io.out(i).fire() && io.in(i).bits.ctrl.isMove && !meEnable(i) && { if (i == 0) false.B else psrc_cmp(i-1).orR })))

  // to make sure meEnable functions as expected
  for (i <- 0 until RenameWidth) {
    XSDebug(io.out(i).fire() && io.in(i).bits.ctrl.isMove && !meEnable(i) && isMax(io.out(i).bits.psrc(0)),
      p"ME_CANCELLED: ref counter hits max value (pc:0x${Hexadecimal(io.in(i).bits.cf.pc)})\n")
    XSDebug(io.out(i).fire() && io.in(i).bits.ctrl.isMove && !meEnable(i) && { if (i == 0) false.B else io.renameBypass.lsrc1_bypass(i-1).orR },
      p"ME_CANCELLED: RAW dependency (pc:0x${Hexadecimal(io.in(i).bits.cf.pc)})\n")
    XSDebug(io.out(i).fire() && io.in(i).bits.ctrl.isMove && !meEnable(i) && { if (i == 0) false.B else psrc_cmp(i-1).orR },
      p"ME_CANCELLED: psrc duplicates with former instruction (pc:0x${Hexadecimal(io.in(i).bits.cf.pc)})\n")
  }
  XSDebug(VecInit(Seq.tabulate(RenameWidth)(i => io.out(i).fire() && io.in(i).bits.ctrl.isMove && !meEnable(i))).asUInt().orR,
    p"ME_CANCELLED: pc group [ " + (0 until RenameWidth).map(i => p"fire:${io.out(i).fire()},pc:0x${Hexadecimal(io.in(i).bits.cf.pc)} ").reduceLeft(_ + _) + p"]\n")
  XSInfo(meEnable.asUInt().orR(), p"meEnableVec:${Binary(meEnable.asUInt)}\n")
}
