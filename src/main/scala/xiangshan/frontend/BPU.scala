package xiangshan.frontend

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.backend.ALUOpType
import xiangshan.backend.JumpOpType

class TableAddr(val idxBits: Int, val banks: Int) extends XSBundle {
  def tagBits = VAddrBits - idxBits - 1

  val tag = UInt(tagBits.W)
  val idx = UInt(idxBits.W)
  val offset = UInt(1.W)

  def fromUInt(x: UInt) = x.asTypeOf(UInt(VAddrBits.W)).asTypeOf(this)
  def getTag(x: UInt) = fromUInt(x).tag
  def getIdx(x: UInt) = fromUInt(x).idx
  def getBank(x: UInt) = getIdx(x)(log2Up(banks) - 1, 0)
  def getBankIdx(x: UInt) = getIdx(x)(idxBits - 1, log2Up(banks))
}

class PredictorResponse extends XSBundle {
  class UbtbResp extends XSBundle {
  // the valid bits indicates whether a target is hit
    val targets = Vec(PredictWidth, UInt(VAddrBits.W))
    val hits = Vec(PredictWidth, Bool())
    val takens = Vec(PredictWidth, Bool())
    val notTakens = Vec(PredictWidth, Bool())
    val is_RVC = Vec(PredictWidth, Bool())
  }
  class BtbResp extends XSBundle {
  // the valid bits indicates whether a target is hit
    val targets = Vec(PredictWidth, UInt(VAddrBits.W))
    val hits = Vec(PredictWidth, Bool())
    val types = Vec(PredictWidth, UInt(2.W))
    val isRVC = Vec(PredictWidth, Bool())
  }
  class BimResp extends XSBundle {
    val ctrs = Vec(PredictWidth, UInt(2.W))
  }
  class TageResp extends XSBundle {
  // the valid bits indicates whether a prediction is hit
    val takens = Vec(PredictWidth, Bool())
    val hits = Vec(PredictWidth, Bool())
  }
  class LoopResp extends XSBundle {
    val exit = Vec(PredictWidth, Bool())
  }

  val ubtb = new UbtbResp
  val btb = new BtbResp
  val bim = new BimResp
  val tage = new TageResp
  val loop = new LoopResp
}

abstract class BasePredictor extends XSModule {
  val metaLen = 0

  // An implementation MUST extend the IO bundle with a response
  // and the special input from other predictors, as well as
  // the metas to store in BRQ
  abstract class Resp extends XSBundle {}
  abstract class FromOthers extends XSBundle {}
  abstract class Meta extends XSBundle {}

  class DefaultBasePredictorIO extends XSBundle {
    val flush = Input(Bool())
    val pc = Flipped(ValidIO(UInt(VAddrBits.W)))
    val hist = Input(UInt(HistoryLength.W))
    val inMask = Input(UInt(PredictWidth.W))
    val update = Flipped(ValidIO(new BranchUpdateInfoWithHist))
  }

  val io = new DefaultBasePredictorIO

  // circular shifting
  def circularShiftLeft(source: UInt, len: Int, shamt: UInt): UInt = {
    val res = Wire(UInt(len.W))
    val higher = source << shamt
    val lower = source >> (len.U - shamt)
    res := higher | lower
    res
  }

  def circularShiftRight(source: UInt, len: Int, shamt: UInt): UInt = {
    val res = Wire(UInt(len.W))
    val higher = source << (len.U - shamt)
    val lower = source >> shamt
    res := higher | lower
    res
  }
}

class BPUStageIO extends XSBundle {
  val pc = UInt(VAddrBits.W)
  val mask = UInt(PredictWidth.W)
  val resp = new PredictorResponse
  val target = UInt(VAddrBits.W)
  val brInfo = Vec(PredictWidth, new BranchInfo)
}


abstract class BPUStage extends XSModule {
  class DefaultIO extends XSBundle {
    val flush = Input(Bool())
    val in = Flipped(Decoupled(new BPUStageIO))
    val pred = Decoupled(new BranchPrediction)
    val out = Decoupled(new BPUStageIO)
    val predecode = Flipped(ValidIO(new Predecode))
  }
  val io = IO(new DefaultIO)

  val predValid = RegInit(false.B)

  io.in.ready := !predValid || io.out.fire() && io.pred.fire() || io.flush

  def npc(pc: UInt, instCount: UInt) = pc + (instCount << 1.U)

  val inFire = io.in.fire()
  val inLatch = RegEnable(io.in.bits, inFire)

  val outFire = io.out.fire()

  // Each stage has its own logic to decide
  // takens, notTakens and target

  val takens = Wire(Vec(PredictWidth, Bool()))
  val notTakens = Wire(Vec(PredictWidth, Bool()))
  val jmpIdx = PriorityEncoder(takens)
  val hasNTBr = (0 until PredictWidth).map(i => i.U <= jmpIdx && notTakens(i)).reduce(_||_)
  val taken = takens.reduce(_||_)
  // get the last valid inst
  // val lastValidPos = MuxCase(0.U, (PredictWidth-1 to 0).map(i => (inLatch.mask(i), i.U)))
  val lastValidPos = PriorityMux(Reverse(inLatch.mask), (PredictWidth-1 to 0 by -1).map(i => i.U))
  val lastHit   = Wire(Bool())
  val lastIsRVC = Wire(Bool())
  // val lastValidPos = WireInit(0.U(log2Up(PredictWidth).W))
  // for (i <- 0 until PredictWidth) {
  //   when (inLatch.mask(i)) { lastValidPos := i.U }
  // }
  val targetSrc = Wire(Vec(PredictWidth, UInt(VAddrBits.W)))
  val target = Mux(taken, targetSrc(jmpIdx), npc(inLatch.pc, PopCount(inLatch.mask)))

  io.pred.bits <> DontCare
  io.pred.bits.redirect := target =/= inLatch.target
  io.pred.bits.taken := taken
  io.pred.bits.jmpIdx := jmpIdx
  io.pred.bits.hasNotTakenBrs := hasNTBr
  io.pred.bits.target := target
  io.pred.bits.saveHalfRVI := ((lastValidPos === jmpIdx && taken) || !taken ) && !lastIsRVC && lastHit

  io.out.bits <> DontCare
  io.out.bits.pc := inLatch.pc
  io.out.bits.mask := inLatch.mask
  io.out.bits.target := target
  io.out.bits.resp <> inLatch.resp
  io.out.bits.brInfo := inLatch.brInfo

  // Default logic
  //  pred.ready not taken into consideration
  //  could be broken
  when (io.flush)     { predValid := false.B }
  .elsewhen (inFire)  { predValid := true.B }
  .elsewhen (outFire) { predValid := false.B }
  .otherwise          { predValid := predValid }

  io.out.valid  := predValid && !io.flush
  io.pred.valid := predValid && !io.flush

  XSDebug(io.in.fire(), "in:(%d %d) pc=%x, mask=%b, target=%x\n",
    io.in.valid, io.in.ready, io.in.bits.pc, io.in.bits.mask, io.in.bits.target)
  XSDebug(io.out.fire(), "out:(%d %d) pc=%x, mask=%b, target=%x\n",
    io.out.valid, io.out.ready, io.out.bits.pc, io.out.bits.mask, io.out.bits.target)
  XSDebug("flush=%d\n", io.flush)
  XSDebug("taken=%d, takens=%b, notTakens=%b, jmpIdx=%d, hasNTBr=%d, lastValidPos=%d, target=%x\n",
    taken, takens.asUInt, notTakens.asUInt, jmpIdx, hasNTBr, lastValidPos, target)
  val p = io.pred.bits
  XSDebug(io.pred.fire(), "outPred: redirect=%d, taken=%d, jmpIdx=%d, hasNTBrs=%d, target=%x, saveHalfRVI=%d\n",
    p.redirect, p.taken, p.jmpIdx, p.hasNotTakenBrs, p.target, p.saveHalfRVI)
  XSDebug(io.pred.fire() && p.taken, "outPredTaken: fetchPC:%x, jmpPC:%x\n",
    inLatch.pc, inLatch.pc + (jmpIdx << 1.U))
  XSDebug(io.pred.fire() && p.redirect, "outPred: previous target:%x redirected to %x \n",
    inLatch.target, p.target)
  XSDebug(io.pred.fire(), "outPred targetSrc: ")
  for (i <- 0 until PredictWidth) {
    XSDebug(false, io.pred.fire(), "(%d):%x ", i.U, targetSrc(i))
  }
  XSDebug(false, io.pred.fire(), "\n")
}

class BPUStage1 extends BPUStage {

  // 'overrides' default logic
  // when flush, the prediction should also starts
  when (inFire)        { predValid := true.B }
  .elsewhen (io.flush) { predValid := false.B }
  .elsewhen (outFire)  { predValid := false.B }
  .otherwise           { predValid := predValid }
  // io.out.valid := predValid

  // ubtb is accessed with inLatch pc in s1, 
  // so we use io.in instead of inLatch
  val ubtbResp = io.in.bits.resp.ubtb
  // the read operation is already masked, so we do not need to mask here
  takens    := VecInit((0 until PredictWidth).map(i => ubtbResp.hits(i) && ubtbResp.takens(i)))
  notTakens := VecInit((0 until PredictWidth).map(i => ubtbResp.hits(i) && ubtbResp.notTakens(i)))
  targetSrc := ubtbResp.targets

  lastIsRVC := ubtbResp.is_RVC(lastValidPos)
  lastHit   := ubtbResp.hits(lastValidPos)

  // resp and brInfo are from the components,
  // so it does not need to be latched
  io.out.bits.resp <> io.in.bits.resp
  io.out.bits.brInfo := io.in.bits.brInfo

  XSDebug(io.pred.fire(), "outPred using ubtb resp: hits:%b, takens:%b, notTakens:%b, isRVC:%b\n",
    ubtbResp.hits.asUInt, ubtbResp.takens.asUInt, ubtbResp.notTakens.asUInt, ubtbResp.is_RVC.asUInt)
}

class BPUStage2 extends BPUStage {

  // Use latched response from s1
  val btbResp = inLatch.resp.btb
  val bimResp = inLatch.resp.bim
  takens    := VecInit((0 until PredictWidth).map(i => btbResp.hits(i) && (btbResp.types(i) === BTBtype.B && bimResp.ctrs(i)(1) || btbResp.types(i) =/= BTBtype.B)))
  notTakens := VecInit((0 until PredictWidth).map(i => btbResp.hits(i) && btbResp.types(i) === BTBtype.B && !bimResp.ctrs(i)(1)))
  targetSrc := btbResp.targets

  lastIsRVC := btbResp.isRVC(lastValidPos)
  lastHit   := btbResp.hits(lastValidPos)

  XSDebug(io.pred.fire(), "outPred using btb&bim resp: hits:%b, ctrTakens:%b\n",
    btbResp.hits.asUInt, VecInit(bimResp.ctrs.map(_(1))).asUInt)
}

class BPUStage3 extends BPUStage {

  io.out.valid := predValid && io.predecode.valid && !io.flush

  // TAGE has its own pipelines and the
  // response comes directly from s3,
  // so we do not use those from inLatch
  val tageResp = io.in.bits.resp.tage
  val tageValidTakens = VecInit((0 until PredictWidth).map( i => tageResp.takens(i) && tageResp.hits(i)))

  val loopResp = io.in.bits.resp.loop.exit

  val pdMask = io.predecode.bits.mask
  val pds    = io.predecode.bits.pd

  val btbHits   = inLatch.resp.btb.hits.asUInt
  val bimTakens = VecInit(inLatch.resp.bim.ctrs.map(_(1)))

  val brs   = pdMask & Reverse(Cat(pds.map(_.isBr)))
  val jals  = pdMask & Reverse(Cat(pds.map(_.isJal)))
  val jalrs = pdMask & Reverse(Cat(pds.map(_.isJalr)))
  // val calls = pdMask & Reverse(Cat(pds.map(_.isCall)))
  // val rets  = pdMask & Reverse(Cat(pds.map(_.isRet)))

  // val callIdx = PriorityEncoder(calls)
  // val retIdx  = PriorityEncoder(rets)
  
  val brTakens = brs &
    (if (EnableBPD) Reverse(Cat((0 until PredictWidth).map(i => tageValidTakens(i)))) else Reverse(Cat((0 until PredictWidth).map(i => bimTakens(i))))) &
    (if (EnableLoop) ~loopResp.asUInt else Fill(PredictWidth, 1.U(1.W)))
    // if (EnableBPD) {
    //   brs & Reverse(Cat((0 until PredictWidth).map(i => tageValidTakens(i))))
    // } else {
    //   brs & Reverse(Cat((0 until PredictWidth).map(i => bimTakens(i))))
    // }

  // predict taken only if btb has a target
  takens := VecInit((0 until PredictWidth).map(i => (brTakens(i) || jalrs(i)) && btbHits(i) || jals(i)))
  // Whether should we count in branches that are not recorded in btb?
  // PS: Currently counted in. Whenever tage does not provide a valid
  //     taken prediction, the branch is counted as a not taken branch
  notTakens := (if (EnableBPD) { VecInit((0 until PredictWidth).map(i => brs(i) && !tageValidTakens(i)))} 
                else           { VecInit((0 until PredictWidth).map(i => brs(i) && !bimTakens(i)))})
  targetSrc := inLatch.resp.btb.targets

  lastIsRVC := pds(lastValidPos).isRVC
  when (lastValidPos === 1.U) {
    lastHit := pdMask(1) |
      !pdMask(0) & !pdMask(1) |
      pdMask(0) & !pdMask(1) & (pds(0).isRVC | !io.predecode.bits.isFetchpcEqualFirstpc)
  }.elsewhen (lastValidPos > 0.U) {
    lastHit := pdMask(lastValidPos) |
      !pdMask(lastValidPos - 1.U) & !pdMask(lastValidPos) |
      pdMask(lastValidPos - 1.U) & !pdMask(lastValidPos) & pds(lastValidPos - 1.U).isRVC
  }.otherwise {
    lastHit := pdMask(0) | !pdMask(0) & !pds(0).isRVC
  }

  // Wrap tage resp and tage meta in
  // This is ugly
  io.out.bits.resp.tage <> io.in.bits.resp.tage
  io.out.bits.resp.loop <> io.in.bits.resp.loop
  for (i <- 0 until PredictWidth) {
    io.out.bits.brInfo(i).tageMeta := io.in.bits.brInfo(i).tageMeta
    io.out.bits.brInfo(i).specCnt := io.in.bits.brInfo(i).specCnt
  }

  XSDebug(io.predecode.valid, "predecode: pc:%x, mask:%b\n", inLatch.pc, io.predecode.bits.mask)
  for (i <- 0 until PredictWidth) {
    val p = io.predecode.bits.pd(i)
    XSDebug(io.predecode.valid && io.predecode.bits.mask(i), "predecode(%d): brType:%d, br:%d, jal:%d, jalr:%d, call:%d, ret:%d, RVC:%d, excType:%d\n",
      i.U, p.brType, p.isBr, p.isJal, p.isJalr, p.isCall, p.isRet, p.isRVC, p.excType)
  }
}

trait BranchPredictorComponents extends HasXSParameter {
  val ubtb = Module(new MicroBTB)
  val btb = Module(new BTB)
  val bim = Module(new BIM)
  val tage = (if(EnableBPD) { Module(new Tage) } 
              else          { Module(new FakeTage) })
  val loop = Module(new LoopPredictor)
  val preds = Seq(ubtb, btb, bim, tage, loop)
  preds.map(_.io := DontCare)
}

class BPUReq extends XSBundle {
  val pc = UInt(VAddrBits.W)
  val hist = UInt(HistoryLength.W)
  val inMask = UInt(PredictWidth.W)
}

class BranchUpdateInfoWithHist extends XSBundle {
  val ui = new BranchUpdateInfo
  val hist = UInt(HistoryLength.W)
}

object BranchUpdateInfoWithHist {
  def apply (brInfo: BranchUpdateInfo, hist: UInt) = {
    val b = Wire(new BranchUpdateInfoWithHist)
    b.ui <> brInfo
    b.hist := hist
    b
  }
}

abstract class BaseBPU extends XSModule with BranchPredictorComponents{
  val io = IO(new Bundle() {
    // from backend
    val inOrderBrInfo    = Flipped(ValidIO(new BranchUpdateInfoWithHist))
    val outOfOrderBrInfo = Flipped(ValidIO(new BranchUpdateInfoWithHist))
    // from ifu, frontend redirect
    val flush = Input(Vec(3, Bool()))
    // from if1
    val in = Flipped(ValidIO(new BPUReq))
    // to if2/if3/if4
    val out = Vec(3, Decoupled(new BranchPrediction))
    // from if4
    val predecode = Flipped(ValidIO(new Predecode))
    // to if4, some bpu info used for updating
    val branchInfo = Decoupled(Vec(PredictWidth, new BranchInfo))
  })

  def npc(pc: UInt, instCount: UInt) = pc + (instCount << 1.U)

  preds.map(_.io.update <> io.outOfOrderBrInfo)
  tage.io.update <> io.inOrderBrInfo

  val s1 = Module(new BPUStage1)
  val s2 = Module(new BPUStage2)
  val s3 = Module(new BPUStage3)

  s1.io.flush := io.flush(0)
  s2.io.flush := io.flush(1)
  s3.io.flush := io.flush(2)

  s1.io.in <> DontCare
  s2.io.in <> s1.io.out
  s3.io.in <> s2.io.out

  io.out(0) <> s1.io.pred
  io.out(1) <> s2.io.pred
  io.out(2) <> s3.io.pred

  s1.io.predecode <> DontCare
  s2.io.predecode <> DontCare
  s3.io.predecode <> io.predecode

  io.branchInfo.valid := s3.io.out.valid
  io.branchInfo.bits := s3.io.out.bits.brInfo
  s3.io.out.ready := io.branchInfo.ready

  XSDebug(io.branchInfo.fire(), "branchInfo sent!\n")
  for (i <- 0 until PredictWidth) {
    val b = io.branchInfo.bits(i)
    XSDebug(io.branchInfo.fire(), "brInfo(%d): ubtbWrWay:%d, ubtbHit:%d, btbWrWay:%d, bimCtr:%d\n",
      i.U, b.ubtbWriteWay, b.ubtbHits, b.btbWriteWay, b.bimCtr)
    val t = b.tageMeta
    XSDebug(io.branchInfo.fire(), "  tageMeta: pvder(%d):%d, altDiffers:%d, pvderU:%d, pvderCtr:%d, allocate(%d):%d\n",
      t.provider.valid, t.provider.bits, t.altDiffers, t.providerU, t.providerCtr, t.allocate.valid, t.allocate.bits)
  }
  val debug_verbose = false
}


class FakeBPU extends BaseBPU {
  io.out.foreach(i => {
    // Provide not takens
    i.valid := true.B
    i.bits <> DontCare
    i.bits.redirect := false.B
  })
  io.branchInfo <> DontCare
}

class BPU extends BaseBPU {

  //**********************Stage 1****************************//
  val s1_fire = s1.io.in.fire()
  val s1_resp_in = Wire(new PredictorResponse)
  val s1_brInfo_in = Wire(Vec(PredictWidth, new BranchInfo))

  s1_resp_in.tage := DontCare
  s1_resp_in.loop := DontCare
  s1_brInfo_in    := DontCare

  val s1_inLatch = RegEnable(io.in, s1_fire)
  ubtb.io.flush := io.flush(0) // TODO: fix this
  ubtb.io.pc.valid := s1_inLatch.valid
  ubtb.io.pc.bits := s1_inLatch.bits.pc
  ubtb.io.inMask := s1_inLatch.bits.inMask



  // Wrap ubtb response into resp_in and brInfo_in
  s1_resp_in.ubtb <> ubtb.io.out
  for (i <- 0 until PredictWidth) {
    s1_brInfo_in(i).ubtbWriteWay := ubtb.io.uBTBBranchInfo.writeWay(i)
    s1_brInfo_in(i).ubtbHits := ubtb.io.uBTBBranchInfo.hits(i)
  }

  btb.io.flush := io.flush(0) // TODO: fix this
  btb.io.pc.valid := io.in.valid
  btb.io.pc.bits := io.in.bits.pc
  btb.io.inMask := io.in.bits.inMask



  // Wrap btb response into resp_in and brInfo_in
  s1_resp_in.btb <> btb.io.resp
  for (i <- 0 until PredictWidth) {
    s1_brInfo_in(i).btbWriteWay := btb.io.meta.writeWay(i)
    s1_brInfo_in(i).btbHitJal   := btb.io.meta.hitJal(i)
  }

  bim.io.flush := io.flush(0) // TODO: fix this
  bim.io.pc.valid := io.in.valid
  bim.io.pc.bits := io.in.bits.pc
  bim.io.inMask := io.in.bits.inMask


  // Wrap bim response into resp_in and brInfo_in
  s1_resp_in.bim <> bim.io.resp
  for (i <- 0 until PredictWidth) {
    s1_brInfo_in(i).bimCtr := bim.io.meta.ctrs(i)
  }


  s1.io.in.valid := io.in.valid
  s1.io.in.bits.pc := io.in.bits.pc
  s1.io.in.bits.mask := io.in.bits.inMask
  s1.io.in.bits.target := npc(io.in.bits.pc, PopCount(io.in.bits.inMask)) // Deault target npc
  s1.io.in.bits.resp <> s1_resp_in
  s1.io.in.bits.brInfo <> s1_brInfo_in

  //**********************Stage 2****************************//
  tage.io.flush := io.flush(1) // TODO: fix this
  tage.io.pc.valid := s1.io.out.fire()
  tage.io.pc.bits := s1.io.out.bits.pc // PC from s1
  tage.io.hist := io.in.bits.hist // The inst is from s1
  tage.io.inMask := s1.io.out.bits.mask
  tage.io.s3Fire := s3.io.in.fire() // Tell tage to march 1 stage
  tage.io.bim <> s1.io.out.bits.resp.bim // Use bim results from s1

  //**********************Stage 3****************************//
  // Wrap tage response and meta into s3.io.in.bits
  // This is ugly

  loop.io.flush := io.flush(2)
  loop.io.pc.valid := s2.io.out.fire()
  loop.io.pc.bits := s2.io.out.bits.pc
  
  s3.io.in.bits.resp.tage <> tage.io.resp
  s3.io.in.bits.resp.loop <> loop.io.resp
  for (i <- 0 until PredictWidth) {
    s3.io.in.bits.brInfo(i).tageMeta := tage.io.meta(i)
    s3.io.in.bits.brInfo(i).specCnt := loop.io.meta.specCnts(i)
  }

  if (debug_verbose) {
    val uo = ubtb.io.out
    XSDebug("debug: ubtb hits:%b, takens:%b, notTakens:%b\n", uo.hits.asUInt, uo.takens.asUInt, uo.notTakens.asUInt)
    val bio = bim.io.resp
    XSDebug("debug: bim takens:%b\n", VecInit(bio.ctrs.map(_(1))).asUInt)
    val bo = btb.io.resp
    XSDebug("debug: btb hits:%b\n", bo.hits.asUInt)
  }

}
