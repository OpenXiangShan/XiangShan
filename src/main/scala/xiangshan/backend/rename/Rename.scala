package xiangshan.backend.rename

import chisel3._
import chisel3.util._
import xiangshan._
import utils.XSInfo

class RenameBypassInfo extends XSBundle {
  val lsrc1_bypass = MixedVec(List.tabulate(RenameWidth-1)(i => UInt((i+1).W)))
  val lsrc2_bypass = MixedVec(List.tabulate(RenameWidth-1)(i => UInt((i+1).W)))
  val lsrc3_bypass = MixedVec(List.tabulate(RenameWidth-1)(i => UInt((i+1).W)))
  val ldest_bypass = MixedVec(List.tabulate(RenameWidth-1)(i => UInt((i+1).W)))
}

class Rename extends XSModule {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val roqCommits = Flipped(new RoqCommitIO)
    // from decode buffer
    val in = Vec(RenameWidth, Flipped(DecoupledIO(new CfCtrl)))
    // to dispatch1
    val out = Vec(RenameWidth, DecoupledIO(new MicroOp))
    val renameBypass = Output(new RenameBypassInfo)
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

  val fpFreeList, intFreeList = Module(new FreeList).io
  val fpRat = Module(new RenameTable(float = true)).io
  val intRat = Module(new RenameTable(float = false)).io

  fpFreeList.redirect := io.redirect
  intFreeList.redirect := io.redirect

  fpRat.redirect := io.redirect
  intRat.redirect := io.redirect
  fpRat.walkWen := io.roqCommits.isWalk
  intRat.walkWen := io.roqCommits.isWalk

  def needDestReg[T <: CfCtrl](fp: Boolean, x: T): Bool = {
    {if(fp) x.ctrl.fpWen else x.ctrl.rfWen && (x.ctrl.ldest =/= 0.U)}
  }
  def needDestRegCommit[T <: RoqCommitInfo](fp: Boolean, x: T): Bool = {
    {if(fp) x.fpWen else x.rfWen && (x.ldest =/= 0.U)}
  }
  fpFreeList.walk.valid := io.roqCommits.isWalk
  intFreeList.walk.valid := io.roqCommits.isWalk
  fpFreeList.walk.bits := PopCount((0 until CommitWidth).map(i => io.roqCommits.valid(i) && needDestRegCommit(true, io.roqCommits.info(i))))
  intFreeList.walk.bits := PopCount((0 until CommitWidth).map(i => io.roqCommits.valid(i) && needDestRegCommit(false, io.roqCommits.info(i))))
  // walk has higher priority than allocation and thus we don't use isWalk here
  fpFreeList.req.doAlloc := intFreeList.req.canAlloc && io.out(0).ready
  intFreeList.req.doAlloc := fpFreeList.req.canAlloc && io.out(0).ready

  val uops = Wire(Vec(RenameWidth, new MicroOp))

  uops.foreach( uop => {
//    uop.brMask := DontCare
//    uop.brTag := DontCare
    uop.src1State := DontCare
    uop.src2State := DontCare
    uop.src3State := DontCare
    uop.roqIdx := DontCare
    uop.diffTestDebugLrScValid := DontCare
    uop.lqIdx := DontCare
    uop.sqIdx := DontCare
  })

  val needFpDest = Wire(Vec(RenameWidth, Bool()))
  val needIntDest = Wire(Vec(RenameWidth, Bool()))
  val hasValid = Cat(io.in.map(_.valid)).orR
  val canOut = io.out(0).ready && fpFreeList.req.canAlloc && intFreeList.req.canAlloc && !io.roqCommits.isWalk
  for(i <- 0 until RenameWidth) {
    uops(i).cf := io.in(i).bits.cf
    uops(i).ctrl := io.in(i).bits.ctrl
    uops(i).brTag := io.in(i).bits.brTag

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

    io.out(i).valid := io.in(i).valid && intFreeList.req.canAlloc && fpFreeList.req.canAlloc && !io.roqCommits.isWalk
    io.out(i).bits := uops(i)

    // write rename table
    def writeRat(fp: Boolean) = {
      val rat = if(fp) fpRat else intRat
      val freeList = if(fp) fpFreeList else intFreeList
      // speculative inst write
      val specWen = freeList.req.allocReqs(i) && freeList.req.canAlloc && freeList.req.doAlloc && !io.roqCommits.isWalk
      // walk back write
      val commitDestValid = io.roqCommits.valid(i) && needDestRegCommit(fp, io.roqCommits.info(i))
      val walkWen = commitDestValid && io.roqCommits.isWalk

      rat.specWritePorts(i).wen := specWen || walkWen
      rat.specWritePorts(i).addr := Mux(specWen, uops(i).ctrl.ldest, io.roqCommits.info(i).ldest)
      rat.specWritePorts(i).wdata := Mux(specWen, freeList.req.pdests(i), io.roqCommits.info(i).old_pdest)

      XSInfo(walkWen,
        {if(fp) p"fp" else p"int "} + p"walk: " +
          p" ldest:${rat.specWritePorts(i).addr} old_pdest:${rat.specWritePorts(i).wdata}\n"
      )

      rat.archWritePorts(i).wen := commitDestValid && !io.roqCommits.isWalk
      rat.archWritePorts(i).addr := io.roqCommits.info(i).ldest
      rat.archWritePorts(i).wdata := io.roqCommits.info(i).pdest

      XSInfo(rat.archWritePorts(i).wen,
        {if(fp) p"fp" else p"int "} + p" rat arch: ldest:${rat.archWritePorts(i).addr}" +
          p" pdest:${rat.archWritePorts(i).wdata}\n"
      )

      freeList.deallocReqs(i) := rat.archWritePorts(i).wen
      freeList.deallocPregs(i) := io.roqCommits.info(i).old_pdest

    }

    writeRat(fp = false)
    writeRat(fp = true)

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
  }
}
