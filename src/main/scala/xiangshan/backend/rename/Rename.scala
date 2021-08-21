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
import xiangshan.backend.roq.RoqPtr
import xiangshan.backend.dispatch.PreDispatchInfo

class RenameBypassInfo(implicit p: Parameters) extends XSBundle {
  val lsrc1_bypass = MixedVec(List.tabulate(RenameWidth-1)(i => UInt((i+1).W)))
  val lsrc2_bypass = MixedVec(List.tabulate(RenameWidth-1)(i => UInt((i+1).W)))
  val lsrc3_bypass = MixedVec(List.tabulate(RenameWidth-1)(i => UInt((i+1).W)))
  val ldest_bypass = MixedVec(List.tabulate(RenameWidth-1)(i => UInt((i+1).W)))
}

class Rename(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
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
    // for debug printing
    val debug_int_rat = Vec(32, Output(UInt(PhyRegIdxWidth.W)))
    val debug_fp_rat = Vec(32, Output(UInt(PhyRegIdxWidth.W)))
  })

  // create free list and rat
  val intFreeList = Module(new AlternativeFreeList)
  val fpFreeList = Module(new FreeList)

  val intRat = Module(new RenameTable(float = false))
  val fpRat = Module(new RenameTable(float = true))

  // connect flush and redirect ports for rat
  Seq(intRat, fpRat) foreach { case rat =>
    rat.io.redirect := io.redirect.valid
    rat.io.flush := io.flush
    rat.io.walkWen := io.roqCommits.isWalk
  }

  // connect flush and redirect ports for __float point__ free list
  fpFreeList.io.flush := io.flush
  fpFreeList.io.redirect := io.redirect.valid
  fpFreeList.io.walk.valid := io.roqCommits.isWalk

  // connect flush and redirect ports for __integer__ free list *(walk) is handled by dec
  intFreeList.io.flush := io.flush
  intFreeList.io.redirect := io.redirect.valid
  intFreeList.io.walk := io.roqCommits.isWalk

  //           dispatch1 ready ++ float point free list ready ++ int free list ready      ++ not walk
  val canOut = io.out(0).ready && fpFreeList.io.req.canAlloc && intFreeList.io.inc.canInc && !io.roqCommits.isWalk

  // decide if given instruction needs allocating a new physical register (CfCtrl: from decode; RoqCommitInfo: from roq)
  def needDestReg[T <: CfCtrl](fp: Boolean, x: T): Bool = {
    {if(fp) x.ctrl.fpWen else x.ctrl.rfWen && (x.ctrl.ldest =/= 0.U)}
  }
  def needDestRegCommit[T <: RoqCommitInfo](fp: Boolean, x: T): Bool = {
    {if(fp) x.fpWen else x.rfWen && (x.ldest =/= 0.U)}
  }

  // when roqCommits.isWalk, use walk.bits to restore head pointer of free list
  fpFreeList.io.walk.bits := PopCount(io.roqCommits.valid.zip(io.roqCommits.info).map{case (v, i) => v && needDestRegCommit(true, i)})


  // walk has higher priority than allocation and thus we don't use isWalk here
  // only when both fp and int free list and dispatch1 has enough space can we do allocation
  fpFreeList.io.req.doAlloc := intFreeList.io.inc.canInc && io.out(0).ready
  intFreeList.io.inc.doInc := fpFreeList.io.req.canAlloc && io.out(0).ready



  // speculatively assign the instruction with an roqIdx
  val validCount = PopCount(io.in.map(_.valid)) // number of instructions waiting to enter roq (from decode)
  val roqIdxHead = RegInit(0.U.asTypeOf(new RoqPtr))
  val lastCycleMisprediction = RegNext(io.redirect.valid && !io.redirect.bits.flushItself())
  val roqIdxHeadNext = Mux(io.flush, 0.U.asTypeOf(new RoqPtr), // flush: clear roq
              Mux(io.redirect.valid, io.redirect.bits.roqIdx, // redirect: move ptr to given roq index (flush itself)
         Mux(lastCycleMisprediction, roqIdxHead + 1.U, // mis-predict: not flush roqIdx itself
                         Mux(canOut, roqIdxHead + validCount, // instructions successfully entered next stage: increase roqIdx
                      /* default */  roqIdxHead)))) // no instructions passed by this cycle: stick to old value
  roqIdxHead := roqIdxHeadNext


  /**
    * Rename: allocate free physical register and update rename table
    */
  val uops = Wire(Vec(RenameWidth, new MicroOp))
  uops.foreach( uop => {
    uop.srcState(0) := DontCare
    uop.srcState(1) := DontCare
    uop.srcState(2) := DontCare
    uop.roqIdx := DontCare
    uop.diffTestDebugLrScValid := DontCare
    uop.debugInfo := DontCare
    uop.lqIdx := DontCare
    uop.sqIdx := DontCare
  })

  val needFpDest = Wire(Vec(RenameWidth, Bool()))
  val needIntDest = Wire(Vec(RenameWidth, Bool()))
  val hasValid = Cat(io.in.map(_.valid)).orR

  val isMove = io.in.map(_.bits.ctrl.isMove)
  val isMax = intFreeList.io.maxVec
  val meEnable = WireInit(VecInit(Seq.fill(RenameWidth)(false.B)))
  val psrc_cmp = Wire(MixedVec(List.tabulate(RenameWidth-1)(i => UInt((i+1).W))))

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
    fpFreeList.io.req.allocReqs(i) := needFpDest(i)
    intFreeList.io.inc.req(i) := needIntDest(i)

    // no valid instruction from decode stage || all resources (dispatch1 + both free lists) ready
    io.in(i).ready := !hasValid || canOut

    // do checkpoints when a branch inst come
    // for(fl <- Seq(fpFreeList, intFreeList)){
    //   fl.cpReqs(i).valid := inValid
    //   fl.cpReqs(i).bits := io.in(i).bits.brTag
    // }


    uops(i).roqIdx := roqIdxHead + i.U

    io.out(i).valid := io.in(i).valid && intFreeList.io.inc.canInc && fpFreeList.io.req.canAlloc && !io.roqCommits.isWalk
    io.out(i).bits := uops(i)


    // read rename table
    def readRat(lsrcList: List[UInt], ldest: UInt, fp: Boolean) = {
      val rat = if(fp) fpRat else intRat
      val srcCnt = lsrcList.size
      val psrcVec = Wire(Vec(srcCnt, UInt(PhyRegIdxWidth.W)))
      val old_pdest = Wire(UInt(PhyRegIdxWidth.W))
      for(k <- 0 until srcCnt+1){
        val rportIdx = i * (srcCnt+1) + k
        if(k != srcCnt){
          rat.io.readPorts(rportIdx).addr := lsrcList(k)
          psrcVec(k) := rat.io.readPorts(rportIdx).rdata
        } else {
          rat.io.readPorts(rportIdx).addr := ldest
          old_pdest := rat.io.readPorts(rportIdx).rdata
        }
      }
      (psrcVec, old_pdest)
    }
    val lsrcList = List(uops(i).ctrl.lsrc(0), uops(i).ctrl.lsrc(1), uops(i).ctrl.lsrc(2))
    val ldest = uops(i).ctrl.ldest
    val (intPhySrcVec, intOldPdest) = readRat(lsrcList.take(2), ldest, fp = false)
    val (fpPhySrcVec, fpOldPdest) = readRat(lsrcList, ldest, fp = true)
    uops(i).psrc(0) := Mux(uops(i).ctrl.srcType(0) === SrcType.reg, intPhySrcVec(0), fpPhySrcVec(0))
    uops(i).psrc(1) := Mux(uops(i).ctrl.srcType(1) === SrcType.reg, intPhySrcVec(1), fpPhySrcVec(1))
    uops(i).psrc(2) := fpPhySrcVec(2)
    uops(i).old_pdest := Mux(uops(i).ctrl.rfWen, intOldPdest, fpOldPdest)

    if (i == 0) {
      // calculate meEnable
      meEnable(i) := isMove(i) && !isMax(uops(i).psrc(0))      
    } else {
      // compare psrc0
      psrc_cmp(i-1) := Cat((0 until i).map(j => {
        uops(i).psrc(0) === uops(j).psrc(0)
      }) /* reverse is not necessary here */)

      // calculate meEnable
      meEnable(i) := isMove(i) && !(io.renameBypass.lsrc1_bypass(i-1).orR | psrc_cmp(i-1).orR | isMax(uops(i).psrc(0)))
    }
    uops(i).eliminatedMove := meEnable(i)

    // send psrc of eliminated move instructions to free list and label them as eliminated
    when (meEnable(i)) {
      intFreeList.io.inc.psrcOfMove(i).valid := true.B
      intFreeList.io.inc.psrcOfMove(i).bits := uops(i).psrc(0)
      XSInfo(io.in(i).valid && io.out(i).valid, p"Move instruction ${Hexadecimal(io.in(i).bits.cf.pc)} eliminated successfully! psrc:${uops(i).psrc(0)}\n")
    } .otherwise {
      intFreeList.io.inc.psrcOfMove(i).valid := false.B
      intFreeList.io.inc.psrcOfMove(i).bits := DontCare
      XSInfo(io.in(i).valid && io.out(i).valid && isMove(i), p"Move instruction ${Hexadecimal(io.in(i).bits.cf.pc)} failed to be eliminated! psrc:${uops(i).psrc(0)}\n")
    }

    // update pdest
    uops(i).pdest := Mux(meEnable(i), uops(i).psrc(0), // move eliminated
                     Mux(needIntDest(i), intFreeList.io.inc.pdests(i), // normal int inst
                     Mux(uops(i).ctrl.ldest===0.U && uops(i).ctrl.rfWen, 0.U // int inst with dst=r0
                     /* default */, fpFreeList.io.req.pdests(i)))) // normal fp inst

    // write speculative rename table
    intSpecWen(i) := intFreeList.io.inc.req(i) && intFreeList.io.inc.canInc && intFreeList.io.inc.doInc && !io.roqCommits.isWalk
    // intRat.io.specWritePorts(i).wen := intSpecWen
    // intRat.io.specWritePorts(i).addr := uops(i).ctrl.ldest
    // intRat.io.specWritePorts(i).wdata := Mux(meEnable(i), uops(i).psrc(0), intFreeList.io.inc.pdests(i))

    fpSpecWen(i) := fpFreeList.io.req.allocReqs(i) && fpFreeList.io.req.canAlloc && fpFreeList.io.req.doAlloc && !io.roqCommits.isWalk
    // fpRat.io.specWritePorts(i).wen := fpSpecWen
    // fpRat.io.specWritePorts(i).addr := uops(i).ctrl.ldest
    // fpRat.io.specWritePorts(i).wdata := fpFreeList.io.req.pdests(i)
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
    // when RenameWidth <= CommitWidth, there will be more write ports than read ports, which must be initialized
    // normally, they are initialized in 'normal write' section
    if (i >= RenameWidth) {
      Seq(intRat, fpRat) foreach { case rat =>
        rat.io.specWritePorts(i).wen   := false.B
        rat.io.specWritePorts(i).addr  := DontCare
        rat.io.specWritePorts(i).wdata := DontCare
      }
    }

    Seq((intRat, false), (fpRat, true)) foreach { case (rat, fp) => 
      // is valid commit req and given instruction has destination register
      val commitDestValid = io.roqCommits.valid(i) && needDestRegCommit(fp, io.roqCommits.info(i))
      XSDebug(p"isFp[${fp}]index[$i]-commitDestValid:$commitDestValid,isWalk:${io.roqCommits.isWalk}\n")

      /*
      I. RAT Update
       */

      // walk back write - restore spec state : ldest => old_pdest
      if (fp && i < RenameWidth) {
        rat.io.specWritePorts(i).wen := (commitDestValid && io.roqCommits.isWalk) || fpSpecWen(i)
        rat.io.specWritePorts(i).addr := Mux(fpSpecWen(i), uops(i).ctrl.ldest, io.roqCommits.info(i).ldest)
        rat.io.specWritePorts(i).wdata := Mux(fpSpecWen(i), fpFreeList.io.req.pdests(i), io.roqCommits.info(i).old_pdest)
      } else if (!fp && i < RenameWidth) {
        rat.io.specWritePorts(i).wen := (commitDestValid && io.roqCommits.isWalk) || intSpecWen(i)
        rat.io.specWritePorts(i).addr := Mux(intSpecWen(i), uops(i).ctrl.ldest, io.roqCommits.info(i).ldest)
        rat.io.specWritePorts(i).wdata := Mux(intSpecWen(i), Mux(meEnable(i), uops(i).psrc(0), intFreeList.io.inc.pdests(i)), io.roqCommits.info(i).old_pdest)
      } else if (fp && i >= RenameWidth) {
        rat.io.specWritePorts(i).wen := commitDestValid && io.roqCommits.isWalk
        rat.io.specWritePorts(i).addr := io.roqCommits.info(i).ldest
        rat.io.specWritePorts(i).wdata := io.roqCommits.info(i).old_pdest
      } else if (!fp && i >= RenameWidth) {
        rat.io.specWritePorts(i).wen := commitDestValid && io.roqCommits.isWalk
        rat.io.specWritePorts(i).addr := io.roqCommits.info(i).ldest
        rat.io.specWritePorts(i).wdata := io.roqCommits.info(i).old_pdest
      }

      when (commitDestValid && io.roqCommits.isWalk) {
        XSInfo({if(fp) p"[fp" else p"[int"} + p" walk] " +
          p"ldest:${rat.io.specWritePorts(i).addr} -> old_pdest:${rat.io.specWritePorts(i).wdata}\n")
      }

      // normal write - update arch state (serve as initialization)
      rat.io.archWritePorts(i).wen := commitDestValid && !io.roqCommits.isWalk
      rat.io.archWritePorts(i).addr := io.roqCommits.info(i).ldest
      rat.io.archWritePorts(i).wdata := io.roqCommits.info(i).pdest

      XSInfo(rat.io.archWritePorts(i).wen,
        {if(fp) p"[fp" else p"[int"} + p" arch rat update] ldest:${rat.io.archWritePorts(i).addr} ->" +
        p" pdest:${rat.io.archWritePorts(i).wdata}\n"
      )


      /*
      II. Free List Update
       */

      if (fp) { // Float Point free list
        fpFreeList.io.deallocReqs(i)  := commitDestValid && !io.roqCommits.isWalk
        fpFreeList.io.deallocPregs(i) := io.roqCommits.info(i).old_pdest
      } else { // Integer free list

        // during walk process:
        // 1. for normal inst, free pdest + revert rat from ldest->pdest to ldest->old_pdest
        // 2. for ME inst, free pdest(commit counter++) + revert rat

        // conclusion: 
        // a. rat recovery has nothing to do with ME or not
        // b. treat walk as normal commit except replace old_pdests with pdests and set io.walk to true
        // c. ignore pdests port when walking

        intFreeList.io.dec.req(i) := commitDestValid // walk or not walk
        intFreeList.io.dec.old_pdests(i)  := Mux(io.roqCommits.isWalk, io.roqCommits.info(i).pdest, io.roqCommits.info(i).old_pdest)
        intFreeList.io.dec.eliminatedMove(i) := io.roqCommits.info(i).eliminatedMove
        intFreeList.io.dec.pdests(i) := io.roqCommits.info(i).pdest
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

  XSDebug(io.roqCommits.isWalk, p"Walk Recovery Enabled\n")
  XSDebug(io.roqCommits.isWalk, p"validVec:${Binary(io.roqCommits.valid.asUInt)}\n")
  for (i <- 0 until CommitWidth) {
    val info = io.roqCommits.info(i)
    XSDebug(io.roqCommits.isWalk && io.roqCommits.valid(i), p"[#$i walk info] pc:${Hexadecimal(info.pc)} " +
      p"ldest:${info.ldest} rfWen:${info.rfWen} fpWen:${info.fpWen} eliminatedMove:${info.eliminatedMove} " +
      p"pdest:${info.pdest} old_pdest:${info.old_pdest}\n")
  }

  XSDebug(p"inValidVec: ${Binary(Cat(io.in.map(_.valid)))}\n")
  XSInfo(!canOut, p"stall at rename, hasValid:${hasValid}, fpCanAlloc:${fpFreeList.io.req.canAlloc}, intCanAlloc:${intFreeList.io.inc.canInc} dispatch1ready:${io.out(0).ready}, isWalk:${io.roqCommits.isWalk}\n")
  XSInfo(meEnable.asUInt().orR(), p"meEnableVec:${Binary(meEnable.asUInt)}\n")

  intRat.io.debug_rdata <> io.debug_int_rat
  fpRat.io.debug_rdata <> io.debug_fp_rat

  XSDebug(p"Arch Int RAT:" + io.debug_int_rat.zipWithIndex.map{ case (r, i) => p"#$i:$r " }.reduceLeft(_ + _) + p"\n")

  XSPerfAccumulate("in", Mux(RegNext(io.in(0).ready), PopCount(io.in.map(_.valid)), 0.U))
  XSPerfAccumulate("utilization", PopCount(io.in.map(_.valid)))
  XSPerfAccumulate("waitInstr", PopCount((0 until RenameWidth).map(i => io.in(i).valid && !io.in(i).ready)))
  XSPerfAccumulate("stall_cycle_dispatch", hasValid && !io.out(0).ready && fpFreeList.io.req.canAlloc && intFreeList.io.inc.canInc && !io.roqCommits.isWalk)
  XSPerfAccumulate("stall_cycle_fp", hasValid && io.out(0).ready && !fpFreeList.io.req.canAlloc && intFreeList.io.inc.canInc && !io.roqCommits.isWalk)
  XSPerfAccumulate("stall_cycle_int", hasValid && io.out(0).ready && fpFreeList.io.req.canAlloc && !intFreeList.io.inc.canInc && !io.roqCommits.isWalk)
  XSPerfAccumulate("stall_cycle_walk", hasValid && io.out(0).ready && fpFreeList.io.req.canAlloc && intFreeList.io.inc.canInc && io.roqCommits.isWalk)
}
