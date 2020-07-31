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
    val targets = Vec(PredictWidth, ValidUndirectioned(UInt(VaddrBits.W)))
    val takens = Vec(PredictWidth, Bool())
    val notTakens = Vec(PredictWidth, Bool())
    val isRVC = Vec(PredictWidth, Bool())
  }
  class BtbResp extends XSBundle {
  // the valid bits indicates whether a target is hit
    val targets = Vec(PredictWidth, ValidUndirectioned(UInt(VaddrBits.W)))
    val types = Vec(PredictWidth, UInt(2.W))
    val isRVC = Vec(PredictWidth, Bool())
  }
  class BimResp extends XSBundle {
    val ctrs = Vec(PredictWidth, ValidUndirectioned(UInt(2.W)))
  }
  class TageResp extends XSBundle {
  // the valid bits indicates whether a prediction is hit
    val takens = Vec(PredictWidth, ValidUndirectioned(Bool()))
  }

  val ubtb = new UbtbResp
  val btb = new BtbResp
  val bim = new BimResp
  val tage = new TageResp
}

abstract class BasePredictor extends XSModule {
  val metaLen = 0

  // An implementation MUST extend the IO bundle with a response
  // and the special input from other predictors, as well as
  // the metas to store in BRQ
  abstract class Resp extends PredictorResponse {}
  abstract class FromOthers extends XSBundle {}
  abstract class Meta extends XSBundle {}

  class DefaultBasePredictorIO extends XSBundle {
    val flush = Input(Bool())
    val pc = Flipped(ValidIO(UInt(VAddrBits.W)))
    val hist = Input(UInt(HistoryLength.W))
    val inMask = Input(UInt(PredictWidth.W))
    val update = Flipped(ValidIO(new BranchUpdateInfoWithHist))
  }

  // circular shifting
  def circularShiftLeft(source: UInt, len: Int, shamt: UInt): UInt = {
    val res = Wire(UInt(len.W))
    val higher = source << shamt
    val lower = source >> (len.U - shamt)
    res := higher | lower
    res
  }
}

class BPUStageIO extends XSBundle {
  val pc = UInt(VAddrBits.W)
  val mask = UInt(PredictWidth.W)
  val resp = new PredictorResponse
  val target = UInt(VaddrBits.W)
  val brInfo = Vec(PredictWidth, new BranchInfo)
}


abstract class BPUStage extends XSModule {
  class DefaultIO extends XSBundle {
    val flush = Input(Bool())
    val in = Flipped(Decoupled(new BPUStageIO))
    val pred = Decoupled(new BranchPrediction)
    val out = Decoupled(new BPUStageIO)
  }
  def npc(pc: UInt, instCount: UInt) = pc + (instCount << 1.U)

  io.in.ready = !outValid || io.out.fire() && io.pred.fire()
  val inFire = io.in.fire()
  val inLatch = RegEnable(io.in.bits, inFire)

  val predValid = RegInit(false.B)
  val outFire = io.out.fire()

  // Each stage has its own logic to decide
  // takens, notTakens and target

  val takens = Vec(PredictWidth, Bool())
  val notTakens = Vec(PredictWidth, Bool())
  val hasNTBr = (0 until PredictWidth).map(i => i.U <= jmpIdx && notTakens(i)).reduce(_||_)
  val taken = takens.reduce(_||_)
  val jmpIdx = PriorityEncoder(takens)
  // get the last valid inst
  val lastValidPos = PriorityMux((PredictWidth-1 to 0).map(i => (inLatch.mask(i), i.U)))
  val target = UInt(VaddrBits.W)

  io.pred.bits <> DontCare
  io.pred.bits.taken := taken
  io.pred.bits.jmpIdx := jmpIdx
  io.pred.bits.hasNotTakenBrs := hasNTBr
  io.pred.bits.target := target

  io.out.bits <> DontCare
  io.out.bits.pc := inLatch.pc.bits
  io.out.bits.mask := inLatch.inMask
  io.out.bits.target := target
  io.out.bits.resp <> inLatch.resp
  io.out.bits.brInfo := inLatch.brInfo

  // Default logic
  //  pred.ready not taken into consideration
  //  could be broken
  when (io.flush) {
    predValid := false.B
  }.elsewhen (inFire) {
    predValid := true.B
  }.elsewhen (outFire) {
    predValid := false.B
  }.otherwise {
    predValid := outValid
  }

  io.out.valid := predValid && !io.flush
  io.pred.valid := predValid && !io.flush
}

class BPUStage1 extends BPUStage {

  val io = new DefaultIO

  // 'overrides' default logic
  // when flush, the prediction should also starts
  override val predValid = BoolStopWatch(io.flush || inFire, outFire, true)
  io.out.valid := predValid

  // ubtb is accessed with inLatch pc in s1, 
  // so we use io.in instead of inLatch
  val ubtbResp = io.in.bits.ubtb
  // the read operation is already masked, so we do not need to mask here
  takens    := VecInit((0 until PredictWidth).map(i => ubtbResp.targets(i).valid && ubtbResp.takens(i)))
  notTakens := VecInit((0 until PredictWidth).map(i => ubtbResp.targets(i).valid && ubtbResp.notTakens(i)))
  target    := Mux(taken, ubtbResp.targets(jmpIdx), npc(inLatch.pc, PopCount(inLatch.mask)))

  io.pred.bits.redirect := taken
  io.pred.bits.saveHalfRVI := ((lastValidPos === jmpIdx && taken) || !taken ) && !ubtbResp.isRVC(lastValidPos)

  // resp and brInfo are from the components,
  // so it does not need to be latched
  io.out.bits.resp <> io.in.bits.resp
  io.out.bits.brInfo := io.in.bits.brInfo
}

class BPUStage2 extends XSModule {
  val io = new DefaultIO

  // Use latched response from s1
  val btbResp = inLatch.btb
  val bimResp = inLatch.bim
  takens := VecInit((0 until PredictWidth).map(i => btbResp.targets(i).valid && bimResp.ctrs(i)(1)))
  notTakens := VecInit((0 until PredictWidth).map(i => btbResp.targets(i).valid && btbResp.types(i) === BrType.branch && !bimResp.ctrs(i)(1)))
  target := Mux(taken, btbResp.targets(jmpIdx), npc(inLatch.pc, PopCount(inLatch.mask)))

  io.pred.bits.redirect := target =/= inLatch.target
  io.pred.bits.saveHalfRVI := ((lastValidPos === jmpIdx && taken) || !taken ) && !btbResp.isRVC(lastValidPos)
}

class BPUStage3 extends XSModule {
  class S3IO extends DefaultIO {
    val predecode = Flipped(ValidIO(new Predecode))
  }
  val io = new S3IO
  io.out.valid := outValid && io.predecode.valid && !io.flush

  // TAGE has its own pipelines and the
  // response comes directly from s3,
  // so we do not use those from inLatch
  val tageResp = io.in.bits.tage
  val tageValidTakens = VecInit(tageResp.takens.map(t => t.valid && t.bits))

  val pdMask = io.predecode.bits.mask
  val pds    = io.predecode.bits.pd

  val btbHits   = VecInit(inLatch.btb.targets.map(_.valid)).asUInt
  val bimTakens = VecInit(inLatch.bim.ctrs.map(_.bits(1)))

  val brs   = pdMask & Reverse(Cat(pds.map(_.isBr)))
  val jals  = pdMask & Reverse(Cat(pds.map(_.isJal)))
  val jalrs = pdMask & Reverse(Cat(pds.map(_.isJalr)))
  val calls = pdMask & Reverse(Cat(pds.map(_.isCall)))
  val rets  = pdMask & Reverse(Cat(pds.map(_.isRet)))

  val callIdx = PriorityEncoder(calls)
  val retIdx  = PriorityEncoder(rets)
  
  val brTakens = 
    if (EnableBPD) {
      brs & Reverse(Cat((0 until PredictWidth).map(i => btbHits(i) && tageValidTakens(i))))
    } else {
      brs & Reverse(Cat((0 until PredictWidth).map(i => btbHits(i) && bimTakens(i))))
    }

  takens := VecInit((0 until PredictWidth).map(i => brTakens(i) || jals(i) || jalrs(i)))
  // Whether should we count in branches that are not recorded in btb?
  // PS: Currently counted in. Whenever tage does not provide a valid
  //     taken prediction, the branch is counted as a not taken branch
  notTakens := VecInit((0 until PredictWidth).map(i => brs(i) && !tageValidTakens(i)))
  target := Mux(taken, inLatch.btb.targets(jmpIdx), npc(inLatch.pc, PopCount(inLatch.mask)))
  
  io.pred.bits.redirect := target =/= inLatch.target
  io.pred.bits.saveHalfRVI := ((lastValidPos === jmpIdx && taken) || !taken ) && !pds(lastValidPos).isRVC

  // Wrap tage resp and tage meta in
  // This is ugly
  io.out.bits.resp.tage <> io.in.bits.tage
  for (i <- 0 until PredictWidth) {
    io.out.bits.brInfo(i).tageMeta := io.in.bits.brInfo(i).tageMeta
  }
}

trait BranchPredictorComponents extends HasXSParameter {
  val ubtb = new Module(MicroBTB)
  val btb = new Module(BTB)
  val bim = new Module(BIM)
  val tage = new Module(Tage)
  val preds = Seq(ubtb, btb, bim, tage)
  preds.map(_.io := DontCare)
}

class BPUReq extends XSBundle {
  val pc = UInt(VAddrBits.W)
  val hist = UInt(HistoryLength.W)
  val inMask = UInt(PredictWidth.W)
}

class BranchUpdateInfoWithHist extends BranchUpdateInfo {
  val hist = UInt(HistoryLength.W)
}

abstract class BaseBPU extends XSModule with BranchPredictorComponents{
  val io = IO(new Bundle() {
    // from backend
    val inOrderBrInfo = Flipped(ValidIO(new BranchUpdateInfoWithHist))
    // from ifu, frontend redirect
    val flush = Input(UInt(3.W))
    // from if1
    val in = Flipped(ValidIO(new BPUReq))
    // to if2/if3/if4
    val out = Vec(3, Decoupled(new BranchPrediction))
    // from if4
    val predecode = Flipped(ValidIO(new Predecode))
    // to if4, some bpu info used for updating
    val branchInfo = Decoupled(Vec(PredictWidth, new BranchInfo))
  })

  val s1 = Module(new BPUStage1)
  val s2 = Module(new BPUStage2)
  val s3 = Module(new BPUStage3)

  // TODO: whether to update ubtb when btb successfully 
  // corrects the wrong prediction from ubtb
  preds.map(_.io.update <> io.inOrderBrInfo)

  s1.io.flush := io.flush(0)
  s2.io.flush := io.flush(1)
  s3.io.flush := io.flush(2)

  s1.io.in <> DontCare
  s2.io.in <> s1.io.out
  s3.io.in <> s2.io.out

  io.out(0) <> s1.io.pred
  io.out(1) <> s2.io.pred
  io.out(2) <> s3.io.pred
  
  s3.io.predecode <> io.predecode

  io.branchInfo.valid := s3.io.out.valid
  io.branchInfo.bits := s3.io.out.bits.branchInfo.metas
  s3.io.out.ready := io.branchInfo.ready
  
}


class FakeBPU extends BaseBPU {
  io.out.foreach(i => {
    i <> DontCare
    i.redirect := false.B
  })
  io.branchInfo <> DontCare
}

class BPU extends BaseBPU {


  //**********************Stage 1****************************//
  val s1_fire = s1.io.in.fire()
  val s1_resp_in = new PredictorResponse
  val s1_brInfo_in = VecInit(0.U.asTypeOf(Vec(PredictWidth, new BranchInfo)))

  s1_resp_in := DontCare
  s1_brInfo_in := DontCare

  val s1_inLatch = RegEnable(io.in, s1_fire)
  ubtb.io.flush := io.flush(0) // TODO: fix this
  ubtb.io.pc.valid := s1_inLatch.valid
  ubtb.io.pc.bits := s1_inLatch.bits.pc
  ubtb.io.inMask := s1_inLatch.bits.inMask

  // Wrap ubtb response into resp_in and brInfo_in
  s1_resp_in.ubtb <> ubtb.io.out
  for (i <- 0 until PredictWidth) {
    s1_brInfo_in(i).ubtbWriteWay := ubtb.io.meta.writeWay(i)
    s1_brInfo_in(i).ubtbHits := ubtb.io.out.targets(i).valid
  }

  btb.io.flush := io.flush(0) // TODO: fix this
  btb.io.pc.valid := io.in.valid
  btb.io.pc.bits := io.in.bits.pc
  btb.io.inMask := io.in.bits.inMask

  // Wrap btb response into resp_in and brInfo_in
  s1_resp_in.btb <> btb.io.out
  for (i <- 0 until PredictWidth) {
    s1_brInfo_in(i).btbWriteWay := btb.io.meta.writeWay(i)
  }

  bim.io.flush := io.flush(0) // TODO: fix this
  bim.io.pc.valid := io.in.valid
  bim.io.pc.bits := io.in.bits.pc
  bim.io.inMask := io.in.bits.inMask

  // Wrap bim response into resp_in and brInfo_in
  s1_resp_in.bim <> bim.io.resp
  for (i <- 0 until PredictWidth) {
    s1_brInfo_in(i).bimCtr := bim.io.meta(i)
  }


  s1.io.in.valid := io.in.valid
  s1.io.in.bits.pc := io.in.pc.bits
  s1.io.in.bits.mask := io.in.mask
  s1.io.in.bits.target := DontCare
  s1.io.in.bits.resp := s1_resp_in
  s1.io.in.bits.brInfo <> s1_brInfo_in


  tage.io.flush := io.flush(1) // TODO: fix this
  tage.io.pc.valid := s1.io.out.fire()
  tage.io.pc.bits := s1.io.out.bits.pc // PC from s1
  tage.io.hist := io.in.hist // The inst is from s1
  tage.io.inMask := s1.io.out.bits.mask
  tage.io.s3Fire := s3.io.in.fire() // Tell tage to march 1 stage
  tage.io.bim <> s1.io.out.resp.bim // Use bim results from s1


  // Wrap tage response and meta into s3.io.in.bits
  // This is ugly
  s3.io.in.bits.resp.tage <> tage.io.resp
  for (i <- 0 until PredictWidth) {
    s3.io.in.bits.brInfo(i).tageMeta := tage.io.meta(i)
  }

}
