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
import xiangshan.backend.pubs.{BrSliceTable, ConfTable}
import xiangshan.backend.rename.freelist._

class Rename(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
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
  })

  // create free list and rat
  val intFreeList = Module(new MEFreeList(MEFreeListSize))
  val intRefCounter = Module(new RefCounter(MEFreeListSize))
  val fpFreeList = Module(new StdFreeList(StdFreeListSize))

  // decide if given instruction needs allocating a new physical register (CfCtrl: from decode; RobCommitInfo: from rob)
  def needDestReg[T <: CfCtrl](fp: Boolean, x: T): Bool = {
    {if(fp) x.ctrl.fpWen else x.ctrl.rfWen && (x.ctrl.ldest =/= 0.U)}
  }
  def needDestRegCommit[T <: RobCommitInfo](fp: Boolean, x: T): Bool = {
    if(fp) x.fpWen else x.rfWen
  }

  // connect [redirect + walk] ports for __float point__ & __integer__ free list
  Seq((fpFreeList, true), (intFreeList, false)).foreach{ case (fl, isFp) =>
    fl.io.redirect := io.redirect.valid
    fl.io.walk := io.robCommits.isWalk
    // when isWalk, use stepBack to restore head pointer of free list
    // (if ME enabled, stepBack of intFreeList should be useless thus optimized out)
    fl.io.stepBack := PopCount(io.robCommits.valid.zip(io.robCommits.info).map{case (v, i) => v && needDestRegCommit(isFp, i)})
  }
  // walk has higher priority than allocation and thus we don't use isWalk here
  // only when both fp and int free list and dispatch1 has enough space can we do allocation
  intFreeList.io.doAllocate := fpFreeList.io.canAllocate && io.out(0).ready
  fpFreeList.io.doAllocate := intFreeList.io.canAllocate && io.out(0).ready

  //           dispatch1 ready ++ float point free list ready ++ int free list ready      ++ not walk
  val canOut = io.out(0).ready && fpFreeList.io.canAllocate && intFreeList.io.canAllocate && !io.robCommits.isWalk


  // speculatively assign the instruction with an robIdx
  val validCount = PopCount(io.in.map(_.valid)) // number of instructions waiting to enter rob (from decode)
  val robIdxHead = RegInit(0.U.asTypeOf(new RobPtr))
  val lastCycleMisprediction = RegNext(io.redirect.valid && !io.redirect.bits.flushItself())
  val robIdxHeadNext = Mux(io.redirect.valid, io.redirect.bits.robIdx, // redirect: move ptr to given rob index
         Mux(lastCycleMisprediction, robIdxHead + 1.U, // mis-predict: not flush robIdx itself
                         Mux(canOut, robIdxHead + validCount, // instructions successfully entered next stage: increase robIdx
                      /* default */  robIdxHead))) // no instructions passed by this cycle: stick to old value
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
    uop.priority := DontCare
  })

  val needFpDest = Wire(Vec(RenameWidth, Bool()))
  val needIntDest = Wire(Vec(RenameWidth, Bool()))
  val hasValid = Cat(io.in.map(_.valid)).orR

  val isMove = io.in.map(_.bits.ctrl.isMove)
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
    fpFreeList.io.allocateReq(i) := needFpDest(i)
    intFreeList.io.allocateReq(i) := needIntDest(i) && !isMove(i)

    // no valid instruction from decode stage || all resources (dispatch1 + both free lists) ready
    io.in(i).ready := !hasValid || canOut

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
    uops(i).eliminatedMove := isMove(i)

    // update pdest
    uops(i).pdest := Mux(needIntDest(i), intFreeList.io.allocatePhyReg(i), // normal int inst
      // normal fp inst
      Mux(needFpDest(i), fpFreeList.io.allocatePhyReg(i),
        /* default */0.U))

    // Assign performance counters
    uops(i).debugInfo.renameTime := GTimer()

    io.out(i).valid := io.in(i).valid && intFreeList.io.canAllocate && fpFreeList.io.canAllocate && !io.robCommits.isWalk
    io.out(i).bits := uops(i)
    when (io.out(i).bits.ctrl.fuType === FuType.fence) {
      io.out(i).bits.ctrl.imm := Cat(io.in(i).bits.ctrl.lsrc(1), io.in(i).bits.ctrl.lsrc(0))
    }

    // write speculative rename table
    // we update rat later inside commit code
    intSpecWen(i) := needIntDest(i) && intFreeList.io.canAllocate && intFreeList.io.doAllocate && !io.robCommits.isWalk && !io.redirect.valid
    fpSpecWen(i) := needFpDest(i) && fpFreeList.io.canAllocate && fpFreeList.io.doAllocate && !io.robCommits.isWalk && !io.redirect.valid

    intRefCounter.io.allocate(i).valid := intSpecWen(i)
    intRefCounter.io.allocate(i).bits := io.out(i).bits.pdest
  }

  /**
    * How to set psrc:
    * - bypass the pdest to psrc if previous instructions write to the same ldest as lsrc
    * - default: psrc from RAT
    * How to set pdest:
    * - Mux(isMove, psrc, pdest_from_freelist).
    *
    * The critical path of rename lies here:
    * When move elimination is enabled, we need to update the rat with psrc.
    * However, psrc maybe comes from previous instructions' pdest, which comes from freelist.
    *
    * If we expand these logic for pdest(N):
    * pdest(N) = Mux(isMove(N), psrc(N), freelist_out(N))
    *          = Mux(isMove(N), Mux(bypass(N, N - 1), pdest(N - 1),
    *                           Mux(bypass(N, N - 2), pdest(N - 2),
    *                           ...
    *                           Mux(bypass(N, 0),     pdest(0),
    *                                                 rat_out(N))...)),
    *                           freelist_out(N))
    */
  // a simple functional model for now
  io.out(0).bits.pdest := Mux(isMove(0), uops(0).psrc.head, uops(0).pdest)
  val bypassCond = Wire(Vec(4, MixedVec(List.tabulate(RenameWidth-1)(i => UInt((i+1).W)))))
  for (i <- 1 until RenameWidth) {
    val fpCond = io.in(i).bits.ctrl.srcType.map(_ === SrcType.fp) :+ needFpDest(i)
    val intCond = io.in(i).bits.ctrl.srcType.map(_ === SrcType.reg) :+ needIntDest(i)
    val target = io.in(i).bits.ctrl.lsrc :+ io.in(i).bits.ctrl.ldest
    for ((((cond1, cond2), t), j) <- fpCond.zip(intCond).zip(target).zipWithIndex) {
      val destToSrc = io.in.take(i).zipWithIndex.map { case (in, j) =>
        val indexMatch = in.bits.ctrl.ldest === t
        val writeMatch =  cond2 && needIntDest(j) || cond1 && needFpDest(j)
        indexMatch && writeMatch
      }
      bypassCond(j)(i - 1) := VecInit(destToSrc).asUInt
    }
    io.out(i).bits.psrc(0) := io.out.take(i).map(_.bits.pdest).zip(bypassCond(0)(i-1).asBools).foldLeft(uops(i).psrc(0)) {
      (z, next) => Mux(next._2, next._1, z)
    }
    io.out(i).bits.psrc(1) := io.out.take(i).map(_.bits.pdest).zip(bypassCond(1)(i-1).asBools).foldLeft(uops(i).psrc(1)) {
      (z, next) => Mux(next._2, next._1, z)
    }
    io.out(i).bits.psrc(2) := io.out.take(i).map(_.bits.pdest).zip(bypassCond(2)(i-1).asBools).foldLeft(uops(i).psrc(2)) {
      (z, next) => Mux(next._2, next._1, z)
    }
    io.out(i).bits.old_pdest := io.out.take(i).map(_.bits.pdest).zip(bypassCond(3)(i-1).asBools).foldLeft(uops(i).old_pdest) {
      (z, next) => Mux(next._2, next._1, z)
    }
    io.out(i).bits.pdest := Mux(isMove(i), io.out(i).bits.psrc(0), uops(i).pdest)
  }

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
        rat(i).wen := fpSpecWen(i)
        rat(i).addr := uops(i).ctrl.ldest
        rat(i).data := fpFreeList.io.allocatePhyReg(i)
      } else if (!fp && i < RenameWidth) {
        rat(i).wen := intSpecWen(i)
        rat(i).addr := uops(i).ctrl.ldest
        rat(i).data := io.out(i).bits.pdest
      }

      /*
      II. Free List Update
       */
      if (fp) { // Float Point free list
        fpFreeList.io.freeReq(i)  := commitDestValid && !io.robCommits.isWalk
        fpFreeList.io.freePhyReg(i) := io.robCommits.info(i).old_pdest
      } else { // Integer free list
        intFreeList.io.freeReq(i) := intRefCounter.io.freeRegs(i).valid
        intFreeList.io.freePhyReg(i) := intRefCounter.io.freeRegs(i).bits
      }
    }
    intRefCounter.io.deallocate(i).valid := io.robCommits.valid(i) && needDestRegCommit(false, io.robCommits.info(i))
    intRefCounter.io.deallocate(i).bits := Mux(io.robCommits.isWalk, io.robCommits.info(i).pdest, io.robCommits.info(i).old_pdest)
  }

  val brSliceTab = Module(new BrSliceTable)
  val confTab = Module(new ConfTable)
  val defTab = Module(new DataModuleTemplate(UInt(VAddrBits.W), 32, 2*RenameWidth, RenameWidth, false, true))
  val isBranch = io.in.map(in => in.bits.ctrl.fuType === FuType.alu && ALUOpType.isBranch(in.bits.ctrl.fuOpType))
  for (i <- 0 until RenameWidth) {
    // The index of the def_tab is the logical destination register number of a decoding instruction,
    // and each entry has the PC of the instruction.
    defTab.io.raddr(2*i) := io.in(i).bits.ctrl.lsrc(0)
    defTab.io.raddr(2*i+1) := io.in(i).bits.ctrl.lsrc(1)
    val intDestValid = io.robCommits.info(i).rfWen && io.robCommits.info(i).ldest =/= 0.U
    defTab.io.wen(i) := Mux(io.robCommits.isWalk, io.robCommits.valid(i) && intDestValid, intSpecWen(i))
    defTab.io.waddr(i) := Mux(io.robCommits.isWalk, io.robCommits.info(i).ldest, io.in(i).bits.ctrl.ldest)
    defTab.io.wdata(i) := Mux(io.robCommits.isWalk, io.robCommits.info(i).pc, io.in(i).bits.cf.pc)

    confTab.io.read(i).enable := io.out(i).valid && canOut
    confTab.io.read(i).address := Mux(isBranch(i), io.in(i).bits.cf.pc, brSliceTab.io.read(i).data.pc_br)

    brSliceTab.io.read(i).enable := io.out(i).valid && canOut
    brSliceTab.io.read(i).address := io.in(i).bits.cf.pc
    val lowConf = confTab.io.read(i).data.isLowConf()
    val dataflowLowConf = lowConf(0) && brSliceTab.io.read(i).data.valid
    brSliceTab.io.write(2*i).enable := io.in(i).valid && canOut && Mux(isBranch(i), lowConf, dataflowLowConf) && io.in(i).bits.ctrl.srcType(0) === SrcType.reg
    brSliceTab.io.write(2*i).address := defTab.io.rdata(2*i)
    brSliceTab.io.write(2*i).data.tag := brSliceTab.pcTag(defTab.io.rdata(2*i))
    brSliceTab.io.write(2*i).data.valid := true.B
    brSliceTab.io.write(2*i).data.pc_br := Mux(isBranch(i), io.in(i).bits.cf.pc, brSliceTab.io.read(i).data.pc_br)
    brSliceTab.io.write(2*i+1).enable := io.in(i).valid && canOut && Mux(isBranch(i), lowConf, dataflowLowConf) && io.in(i).bits.ctrl.srcType(1) === SrcType.reg
    brSliceTab.io.write(2*i+1).address := defTab.io.rdata(2*i+1)
    brSliceTab.io.write(2*i+1).data.tag := brSliceTab.pcTag(defTab.io.rdata(2*i+1))
    brSliceTab.io.write(2*i+1).data.valid := true.B
    brSliceTab.io.write(2*i+1).data.pc_br := Mux(isBranch(i), io.in(i).bits.cf.pc, brSliceTab.io.read(i).data.pc_br)

    io.out(i).bits.priority := false.B//Mux(isBranch(i), lowConf, dataflowLowConf)
  }
  XSPerfAccumulate("pubs_high_priority", PopCount(io.out.map(out => out.fire && out.bits.priority)))
  XSPerfAccumulate("pubs_high_priority_branch", PopCount(io.out.zip(isBranch).map(out => out._1.fire && out._1.bits.priority && out._2)))
  XSPerfAccumulate("pubs_high_priority_normal", PopCount(io.out.zip(isBranch).map(out => out._1.fire && out._1.bits.priority && !out._2)))
  XSPerfAccumulate("pubs_low_priority", PopCount(io.out.map(out => out.fire && !out.bits.priority)))
  XSPerfAccumulate("pubs_low_priority_branch", PopCount(io.out.zip(isBranch).map(out => out._1.fire && !out._1.bits.priority && out._2)))
  XSPerfAccumulate("pubs_low_priority_normal", PopCount(io.out.zip(isBranch).map(out => out._1.fire && !out._1.bits.priority && !out._2)))

  /*
  Debug and performance counters
   */
  def printRenameInfo(in: DecoupledIO[CfCtrl], out: DecoupledIO[MicroOp]) = {
    XSInfo(out.fire, p"pc:${Hexadecimal(in.bits.cf.pc)} in(${in.valid},${in.ready}) " +
      p"lsrc(0):${in.bits.ctrl.lsrc(0)} -> psrc(0):${out.bits.psrc(0)} " +
      p"lsrc(1):${in.bits.ctrl.lsrc(1)} -> psrc(1):${out.bits.psrc(1)} " +
      p"lsrc(2):${in.bits.ctrl.lsrc(2)} -> psrc(2):${out.bits.psrc(2)} " +
      p"ldest:${in.bits.ctrl.ldest} -> pdest:${out.bits.pdest} " +
      p"old_pdest:${out.bits.old_pdest}\n"
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

  XSPerfAccumulate("in", Mux(RegNext(io.in(0).ready), PopCount(io.in.map(_.valid)), 0.U))
  XSPerfAccumulate("utilization", PopCount(io.in.map(_.valid)))
  XSPerfAccumulate("waitInstr", PopCount((0 until RenameWidth).map(i => io.in(i).valid && !io.in(i).ready)))
  XSPerfAccumulate("stall_cycle_dispatch", hasValid && !io.out(0).ready && fpFreeList.io.canAllocate && intFreeList.io.canAllocate && !io.robCommits.isWalk)
  XSPerfAccumulate("stall_cycle_fp", hasValid && io.out(0).ready && !fpFreeList.io.canAllocate && intFreeList.io.canAllocate && !io.robCommits.isWalk)
  XSPerfAccumulate("stall_cycle_int", hasValid && io.out(0).ready && fpFreeList.io.canAllocate && !intFreeList.io.canAllocate && !io.robCommits.isWalk)
  XSPerfAccumulate("stall_cycle_walk", hasValid && io.out(0).ready && fpFreeList.io.canAllocate && intFreeList.io.canAllocate && io.robCommits.isWalk)

  XSPerfAccumulate("move_instr_count", PopCount(io.out.map(out => out.fire() && out.bits.ctrl.isMove)))


  val intfl_perf     = intFreeList.perfEvents.map(_._1).zip(intFreeList.perfinfo.perfEvents.perf_events)
  val fpfl_perf      = fpFreeList.perfEvents.map(_._1).zip(fpFreeList.perfinfo.perfEvents.perf_events)
  val perf_list = Wire(new PerfEventsBundle(6))
  val perf_seq = Seq(
    ("rename_in                   ", PopCount(io.in.map(_.valid & io.in(0).ready ))                                                               ),
    ("rename_waitinstr            ", PopCount((0 until RenameWidth).map(i => io.in(i).valid && !io.in(i).ready))                                  ),
    ("rename_stall_cycle_dispatch ", hasValid && !io.out(0).ready &&  fpFreeList.io.canAllocate &&  intFreeList.io.canAllocate && !io.robCommits.isWalk ),
    ("rename_stall_cycle_fp       ", hasValid &&  io.out(0).ready && !fpFreeList.io.canAllocate &&  intFreeList.io.canAllocate && !io.robCommits.isWalk ),
    ("rename_stall_cycle_int      ", hasValid &&  io.out(0).ready &&  fpFreeList.io.canAllocate && !intFreeList.io.canAllocate && !io.robCommits.isWalk ),
    ("rename_stall_cycle_walk     ", hasValid &&  io.out(0).ready &&  fpFreeList.io.canAllocate &&  intFreeList.io.canAllocate &&  io.robCommits.isWalk ),
  ) 
  for (((perf_out,(perf_name,perf)),i) <- perf_list.perf_events.zip(perf_seq).zipWithIndex) {
    perf_out.incr_step := RegNext(perf)
  }

  val perfEvents_list = perf_list.perf_events ++
                        intFreeList.asInstanceOf[freelist.MEFreeList].perfinfo.perfEvents.perf_events ++
                        fpFreeList.perfinfo.perfEvents.perf_events

  val perfEvents = perf_seq ++ intfl_perf ++ fpfl_perf
  val perfinfo = IO(new Bundle(){
    val perfEvents = Output(new PerfEventsBundle(perfEvents_list.length))
  })
  perfinfo.perfEvents.perf_events := perfEvents_list
}
