package xiangshan.backend.rename

import chisel3._
import chisel3.util._
import xiangshan._
import utils.XSInfo

class Rename extends XSModule {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val roqCommits = Vec(CommitWidth, Flipped(ValidIO(new RoqCommit)))
    val wbIntResults = Vec(NRIntWritePorts, Flipped(ValidIO(new ExuOutput)))
    val wbFpResults = Vec(NRFpWritePorts, Flipped(ValidIO(new ExuOutput)))
    val intRfReadAddr = Vec(NRIntReadPorts, Input(UInt(PhyRegIdxWidth.W)))
    val fpRfReadAddr = Vec(NRFpReadPorts, Input(UInt(PhyRegIdxWidth.W)))
    val intPregRdy = Vec(NRIntReadPorts, Output(Bool()))
    val fpPregRdy = Vec(NRFpReadPorts, Output(Bool()))
    // set preg to busy when replay
    val replayPregReq = Vec(ReplayWidth, Input(new ReplayPregReq))
    // from decode buffer
    val in = Vec(RenameWidth, Flipped(DecoupledIO(new CfCtrl)))
    // to dispatch1
    val out = Vec(RenameWidth, DecoupledIO(new MicroOp))
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
  val fpBusyTable = Module(new BusyTable(NRFpReadPorts, NRFpWritePorts)).io
  val intBusyTable = Module(new BusyTable(NRIntReadPorts, NRIntWritePorts)).io

  fpFreeList.redirect := io.redirect
  intFreeList.redirect := io.redirect

  val flush = io.redirect.valid && (io.redirect.bits.isException || io.redirect.bits.isFlushPipe) // TODO: need check by JiaWei
  fpRat.flush := flush
  intRat.flush := flush
  fpBusyTable.flush := flush
  intBusyTable.flush := flush

  def needDestReg[T <: CfCtrl](fp: Boolean, x: T): Bool = {
    {if(fp) x.ctrl.fpWen else x.ctrl.rfWen && (x.ctrl.ldest =/= 0.U)}
  }

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

  var lastReady = WireInit(io.out(0).ready)
  // debug assert
  val outRdy = Cat(io.out.map(_.ready))
  assert(outRdy===0.U || outRdy.andR())
  for(i <- 0 until RenameWidth) {
    uops(i).cf := io.in(i).bits.cf
    uops(i).ctrl := io.in(i).bits.ctrl
    uops(i).brTag := io.in(i).bits.brTag

    val inValid = io.in(i).valid

    // alloc a new phy reg
    val needFpDest = inValid && needDestReg(fp = true, io.in(i).bits)
    val needIntDest = inValid && needDestReg(fp = false, io.in(i).bits)
    fpFreeList.allocReqs(i) := needFpDest && lastReady
    intFreeList.allocReqs(i) := needIntDest && lastReady
    val fpCanAlloc = fpFreeList.canAlloc(i)
    val intCanAlloc = intFreeList.canAlloc(i)
    val this_can_alloc = Mux(
      needIntDest,
      intCanAlloc,
      Mux(
        needFpDest,
        fpCanAlloc,
        true.B
      )
    )
    io.in(i).ready := lastReady && this_can_alloc

    // do checkpoints when a branch inst come
    for(fl <- Seq(fpFreeList, intFreeList)){
      fl.cpReqs(i).valid := inValid
      fl.cpReqs(i).bits := io.in(i).bits.brTag
    }

    lastReady = io.in(i).ready

    uops(i).pdest := Mux(needIntDest,
      intFreeList.pdests(i),
      Mux(
        uops(i).ctrl.ldest===0.U && uops(i).ctrl.rfWen,
        0.U, fpFreeList.pdests(i)
      )
    )

    io.out(i).valid := io.in(i).fire()
    io.out(i).bits := uops(i)

    // write rename table
    def writeRat(fp: Boolean) = {
      val rat = if(fp) fpRat else intRat
      val freeList = if(fp) fpFreeList else intFreeList
      val busyTable = if(fp) fpBusyTable else intBusyTable
      // speculative inst write
      val specWen = freeList.allocReqs(i) && freeList.canAlloc(i)
      // walk back write
      val commitDestValid = io.roqCommits(i).valid && needDestReg(fp, io.roqCommits(i).bits.uop)
      val walkWen = commitDestValid && io.roqCommits(i).bits.isWalk

      rat.specWritePorts(i).wen := specWen || walkWen
      rat.specWritePorts(i).addr := Mux(specWen, uops(i).ctrl.ldest, io.roqCommits(i).bits.uop.ctrl.ldest)
      rat.specWritePorts(i).wdata := Mux(specWen, freeList.pdests(i), io.roqCommits(i).bits.uop.old_pdest)

      XSInfo(walkWen,
        {if(fp) p"fp" else p"int "} + p"walk: pc:${Hexadecimal(io.roqCommits(i).bits.uop.cf.pc)}" +
          p" ldest:${rat.specWritePorts(i).addr} old_pdest:${rat.specWritePorts(i).wdata}\n"
      )

      rat.archWritePorts(i).wen := commitDestValid && !io.roqCommits(i).bits.isWalk
      rat.archWritePorts(i).addr := io.roqCommits(i).bits.uop.ctrl.ldest
      rat.archWritePorts(i).wdata := io.roqCommits(i).bits.uop.pdest

      XSInfo(rat.archWritePorts(i).wen,
        {if(fp) p"fp" else p"int "} + p" rat arch: ldest:${rat.archWritePorts(i).addr}" +
          p" pdest:${rat.archWritePorts(i).wdata}\n"
      )

      freeList.deallocReqs(i) := rat.archWritePorts(i).wen
      freeList.deallocPregs(i) := io.roqCommits(i).bits.uop.old_pdest

      // set phy reg status to busy
      busyTable.allocPregs(i).valid := specWen
      busyTable.allocPregs(i).bits := freeList.pdests(i)
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


  def updateBusyTable(fp: Boolean) = {
    val wbResults = if(fp) io.wbFpResults else io.wbIntResults
    val busyTable = if(fp) fpBusyTable else intBusyTable
    for((wb, setPhyRegRdy) <- wbResults.zip(busyTable.wbPregs)){
      setPhyRegRdy.valid := wb.valid && needDestReg(fp, wb.bits.uop)
      setPhyRegRdy.bits := wb.bits.uop.pdest
    }
  }

  updateBusyTable(false)
  updateBusyTable(true)

  intBusyTable.rfReadAddr <> io.intRfReadAddr
  intBusyTable.pregRdy <> io.intPregRdy
  for(i <- io.replayPregReq.indices){
    intBusyTable.replayPregs(i).valid := io.replayPregReq(i).isInt
    fpBusyTable.replayPregs(i).valid := io.replayPregReq(i).isFp
    intBusyTable.replayPregs(i).bits := io.replayPregReq(i).preg
    fpBusyTable.replayPregs(i).bits := io.replayPregReq(i).preg
  }
  fpBusyTable.rfReadAddr <> io.fpRfReadAddr
  fpBusyTable.pregRdy <> io.fpPregRdy
}
