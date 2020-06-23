package xiangshan.backend.rename

import chisel3._
import chisel3.util._
import xiangshan._

class Rename extends XSModule {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val roqCommits = Vec(CommitWidth, Flipped(ValidIO(new RoqCommit)))
    val wbIntResults = Vec(NRWritePorts, Flipped(ValidIO(new ExuOutput)))
    val wbFpResults = Vec(NRWritePorts, Flipped(ValidIO(new ExuOutput)))
    val intRfReadAddr = Vec(NRReadPorts, Input(UInt(PhyRegIdxWidth.W)))
    val fpRfReadAddr = Vec(NRReadPorts, Input(UInt(PhyRegIdxWidth.W)))
    val intPregRdy = Vec(NRReadPorts, Output(Bool()))
    val fpPregRdy = Vec(NRReadPorts, Output(Bool()))
    // from decode buffer
    val in = Vec(RenameWidth, Flipped(DecoupledIO(new CfCtrl)))
    // to dispatch1
    val out = Vec(RenameWidth, DecoupledIO(new MicroOp))
  })

  val fpFreeList, intFreeList = Module(new FreeList).io
  val fpRat = Module(new RenameTable(float = true)).io
  val intRat = Module(new RenameTable(float = false)).io
  val fpBusyTable, intBusyTable = Module(new BusyTable).io

  fpFreeList.redirect := io.redirect
  intFreeList.redirect := io.redirect

  val flush = io.redirect.valid && io.redirect.bits.isException
  fpRat.flush := flush
  intRat.flush := flush
  fpBusyTable.flush := flush
  intBusyTable.flush := flush

  def needDestReg[T <: CfCtrl](fp: Boolean, x: T): Bool = {
    {if(fp) x.ctrl.fpWen else x.ctrl.rfWen && (x.ctrl.ldest =/= 0.U)}
  }

  val uops = Wire(Vec(RenameWidth, new MicroOp))

  uops.foreach( uop => {
    uop.brMask := DontCare
    uop.brTag := DontCare
    uop.src1State := DontCare
    uop.src2State := DontCare
    uop.src3State := DontCare
    uop.roqIdx := DontCare
  })

  var last_can_alloc = WireInit(true.B)
  for(i <- 0 until RenameWidth){
    uops(i).cf := io.in(i).bits.cf
    uops(i).ctrl := io.in(i).bits.ctrl

    // alloc a new phy reg
    val needFpDest = io.in(i).valid && needDestReg(fp = true, io.in(i).bits)
    val needIntDest = io.in(i).valid && needDestReg(fp = false, io.in(i).bits)
    fpFreeList.allocReqs(i) := needFpDest && last_can_alloc && io.out(i).ready
    intFreeList.allocReqs(i) := needIntDest && last_can_alloc && io.out(i).ready
    val fpCanAlloc = fpFreeList.canAlloc(i)
    val intCanAlloc = intFreeList.canAlloc(i)
    val this_can_alloc = Mux(needIntDest, intCanAlloc, fpCanAlloc)
    io.in(i).ready := this_can_alloc
    last_can_alloc = last_can_alloc && this_can_alloc
    uops(i).pdest := Mux(needIntDest, intFreeList.pdests(i), fpFreeList.pdests(i))
    uops(i).freelistAllocPtr := Mux(needIntDest, intFreeList.allocPtrs(i), fpFreeList.allocPtrs(i))

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

      rat.archWritePorts(i).wen := commitDestValid && !io.roqCommits(i).bits.isWalk
      rat.archWritePorts(i).addr := io.roqCommits(i).bits.uop.ctrl.ldest
      rat.archWritePorts(i).wdata := io.roqCommits(i).bits.uop.pdest

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
    uops(i).psrc2 := Mux(uops(i).ctrl.src1Type === SrcType.reg, intPhySrcVec(1), fpPhySrcVec(1))
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
  fpBusyTable.rfReadAddr <> io.fpRfReadAddr
  fpBusyTable.pregRdy <> io.fpPregRdy
}
