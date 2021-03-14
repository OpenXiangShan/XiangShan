package xiangshan.frontend

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.backend.ALUOpType
import xiangshan.backend.JumpOpType
import chisel3.experimental.chiselName

trait HasBPUParameter extends HasXSParameter {
  val BPUDebug = true && !env.FPGAPlatform
  val EnableCFICommitLog = true
  val EnbaleCFIPredLog = true
  val EnableBPUTimeRecord = (EnableCFICommitLog || EnbaleCFIPredLog) && !env.FPGAPlatform
  val EnableCommit = false
}

class TableAddr(val idxBits: Int, val banks: Int) extends XSBundle with HasIFUConst {
  def tagBits = VAddrBits - idxBits - instOffsetBits

  val tag = UInt(tagBits.W)
  val idx = UInt(idxBits.W)
  val offset = UInt(instOffsetBits.W)

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
    val brMask = Vec(PredictWidth, Bool())
    val is_RVC = Vec(PredictWidth, Bool())
  }
  class BtbResp extends XSBundle {
  // the valid bits indicates whether a target is hit
    val targets = Vec(PredictWidth, UInt(VAddrBits.W))
    val hits = Vec(PredictWidth, Bool())
    val isBrs = Vec(PredictWidth, Bool())
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

trait PredictorUtils {
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

  // To be verified
  def satUpdate(old: UInt, len: Int, taken: Bool): UInt = {
    val oldSatTaken = old === ((1 << len)-1).U
    val oldSatNotTaken = old === 0.U
    Mux(oldSatTaken && taken, ((1 << len)-1).U,
      Mux(oldSatNotTaken && !taken, 0.U,
        Mux(taken, old + 1.U, old - 1.U)))
  }

  def signedSatUpdate(old: SInt, len: Int, taken: Bool): SInt = {
    val oldSatTaken = old === ((1 << (len-1))-1).S
    val oldSatNotTaken = old === (-(1 << (len-1))).S
    Mux(oldSatTaken && taken, ((1 << (len-1))-1).S,
      Mux(oldSatNotTaken && !taken, (-(1 << (len-1))).S,
        Mux(taken, old + 1.S, old - 1.S)))
  }
}

trait HasIFUFire { this: MultiIOModule =>
  val fires = IO(Input(Vec(4, Bool())))
  val s1_fire  = fires(0)
  val s2_fire  = fires(1)
  val s3_fire  = fires(2)
  val out_fire = fires(3)
}

trait HasCtrl { this: BasePredictor =>
  val ctrl = IO(Input(new BPUCtrl))
}

abstract class BasePredictor extends XSModule
  with HasBPUParameter with HasIFUConst with PredictorUtils
  with HasIFUFire with HasCtrl {
  val metaLen = 0

  // An implementation MUST extend the IO bundle with a response
  // and the special input from other predictors, as well as
  // the metas to store in BRQ
  abstract class Resp extends XSBundle {}
  abstract class FromOthers extends XSBundle {}
  abstract class Meta extends XSBundle {}

  class DefaultBasePredictorIO extends XSBundle {
    val pc = Flipped(ValidIO(UInt(VAddrBits.W)))
    val hist = Input(UInt(HistoryLength.W))
    val inMask = Input(UInt(PredictWidth.W))
    val update = Flipped(ValidIO(new FtqEntry))
  }
  val io = new DefaultBasePredictorIO
  val in_ready = IO(Output(Bool()))
  in_ready := true.B
  val debug = true
}

class BrInfo extends XSBundle {
  val metas = Vec(PredictWidth, new BpuMeta)
  val rasSp = UInt(log2Ceil(RasSize).W)
  val rasTop = new RASEntry
  val specCnt = Vec(PredictWidth, UInt(10.W))
}
class BPUStageIO extends XSBundle {
  val pc = UInt(VAddrBits.W)
  val mask = UInt(PredictWidth.W)
  val resp = new PredictorResponse
  val brInfo = new BrInfo
}


abstract class BPUStage extends XSModule with HasBPUParameter
  with HasIFUConst with HasIFUFire {
  class DefaultIO extends XSBundle {
    val in = Input(new BPUStageIO)
    val inFire = Input(Bool())
    val pred = Output(new BranchPrediction) // to ifu
    val out = Output(new BPUStageIO)        // to the next stage
    val outFire = Input(Bool())

    val debug_hist = Input(UInt((if (BPUDebug) (HistoryLength) else 0).W))
  }
  val io = IO(new DefaultIO)

  val inLatch = RegEnable(io.in, io.inFire)

  // Each stage has its own logic to decide
  // takens, brMask, jalMask, targets and hasHalfRVI
  val takens = Wire(Vec(PredictWidth, Bool()))
  val brMask = Wire(Vec(PredictWidth, Bool()))
  val jalMask = Wire(Vec(PredictWidth, Bool()))
  val targets = Wire(Vec(PredictWidth, UInt(VAddrBits.W)))
  val hasHalfRVI = Wire(Bool())

  io.pred <> DontCare
  io.pred.takens := takens.asUInt
  io.pred.brMask := brMask.asUInt
  io.pred.jalMask := jalMask.asUInt
  io.pred.targets := targets
  io.pred.hasHalfRVI := hasHalfRVI

  io.out <> DontCare
  io.out.pc := inLatch.pc
  io.out.mask := inLatch.mask
  io.out.resp <> inLatch.resp
  io.out.brInfo := inLatch.brInfo

  if (BPUDebug) {
    val jmpIdx = io.pred.jmpIdx
    val taken  = io.pred.taken
    val target = Mux(taken, io.pred.targets(jmpIdx), snpc(inLatch.pc))
    XSDebug("in(%d): pc=%x, mask=%b\n", io.inFire, io.in.pc, io.in.mask)
    XSDebug("inLatch: pc=%x, mask=%b\n", inLatch.pc, inLatch.mask)
    XSDebug("out(%d): pc=%x, mask=%b, taken=%d, jmpIdx=%d, target=%x, hasHalfRVI=%d\n",
      io.outFire, io.out.pc, io.out.mask, taken, jmpIdx, target, hasHalfRVI)
    val p = io.pred
  }
}

@chiselName
class BPUStage1 extends BPUStage {

  // ubtb is accessed with inLatch pc in s1,
  // so we use io.in instead of inLatch
  val ubtbResp = io.in.resp.ubtb
  // the read operation is already masked, so we do not need to mask here
  takens    := VecInit((0 until PredictWidth).map(i => ubtbResp.takens(i)))
  // notTakens := VecInit((0 until PredictWidth).map(i => ubtbResp.hits(i) && !ubtbResp.takens(i) && ubtbResp.brMask(i)))
  brMask := ubtbResp.brMask
  jalMask := DontCare
  targets := ubtbResp.targets

  hasHalfRVI := ubtbResp.hits(PredictWidth-1) && !ubtbResp.is_RVC(PredictWidth-1) && HasCExtension.B

  // resp and brInfo are from the components,
  // so it does not need to be latched
  io.out.resp <> io.in.resp
  io.out.brInfo := io.in.brInfo

  // For perf counters
  if (!env.FPGAPlatform && env.EnablePerfDebug) {
    io.out.brInfo.metas.zipWithIndex.foreach{case (meta, i) =>
      // record ubtb pred result
      meta.ubtbAns.hit := ubtbResp.hits(i)
      meta.ubtbAns.taken := ubtbResp.takens(i)
      meta.ubtbAns.target := ubtbResp.targets(i)
    }
  }

  if (BPUDebug) {
    XSDebug(io.outFire, "outPred using ubtb resp: hits:%b, takens:%b, notTakens:%b, isRVC:%b\n",
      ubtbResp.hits.asUInt, ubtbResp.takens.asUInt, ~ubtbResp.takens.asUInt & brMask.asUInt, ubtbResp.is_RVC.asUInt)
  }
  if (EnableBPUTimeRecord) {
    io.out.brInfo.metas.map(_.debug_ubtb_cycle := GTimer())
  }
}
@chiselName
class BPUStage2 extends BPUStage {
  // Use latched response from s1
  val btbResp = inLatch.resp.btb
  val bimResp = inLatch.resp.bim
  takens    := VecInit((0 until PredictWidth).map(i => btbResp.hits(i) && (btbResp.isBrs(i) && bimResp.ctrs(i)(1) || !btbResp.isBrs(i))))
  targets := btbResp.targets
  brMask  := VecInit((0 until PredictWidth).map(i => btbResp.isBrs(i) && btbResp.hits(i)))
  jalMask := DontCare

  hasHalfRVI  := btbResp.hits(PredictWidth-1) && !btbResp.isRVC(PredictWidth-1) && HasCExtension.B

  // For perf counters
  if (!env.FPGAPlatform && env.EnablePerfDebug) {
    io.out.brInfo.metas.zipWithIndex.foreach{case (meta, i) =>
      // record btb pred result
      meta.btbAns.hit := btbResp.hits(i)
      meta.btbAns.taken := takens(i)
      meta.btbAns.target := btbResp.targets(i)
    }
  }

  if (BPUDebug) {
    XSDebug(io.outFire, "outPred using btb&bim resp: hits:%b, ctrTakens:%b\n",
      btbResp.hits.asUInt, VecInit(bimResp.ctrs.map(_(1))).asUInt)
  }
  if (EnableBPUTimeRecord) {
    io.out.brInfo.metas.map(_.debug_btb_cycle := GTimer())
  }
}
@chiselName
class BPUStage3 extends BPUStage {
  class S3IO extends XSBundle {
    val predecode = Input(new Predecode)
    val redirect =  Flipped(ValidIO(new Redirect))
    val ctrl = Input(new BPUCtrl)
  }
  val s3IO = IO(new S3IO)
  // TAGE has its own pipelines and the
  // response comes directly from s3,
  // so we do not use those from inLatch
  val tageResp = io.in.resp.tage
  val tageTakens = tageResp.takens

  val loopResp = io.in.resp.loop.exit

  val pdMask     = s3IO.predecode.mask
  val pdLastHalf = s3IO.predecode.lastHalf
  val pds        = s3IO.predecode.pd

  val btbResp   = WireInit(inLatch.resp.btb)
  val btbHits   = WireInit(btbResp.hits.asUInt)
  val bimTakens = VecInit(inLatch.resp.bim.ctrs.map(_(1)))

  val brs   = pdMask & Reverse(Cat(pds.map(_.isBr)))
  val jals  = pdMask & Reverse(Cat(pds.map(_.isJal)))
  val jalrs = pdMask & Reverse(Cat(pds.map(_.isJalr)))
  val calls = pdMask & Reverse(Cat(pds.map(_.isCall)))
  val rets  = pdMask & Reverse(Cat(pds.map(_.isRet)))
  val RVCs  = pdMask & Reverse(Cat(pds.map(_.isRVC)))

  val callIdx = PriorityEncoder(calls)
  val retIdx  = PriorityEncoder(rets)

  val brPred = (if(EnableBPD) tageTakens else bimTakens).asUInt
  val loopRes = (if (EnableLoop) loopResp else VecInit(Fill(PredictWidth, 0.U(1.W)))).asUInt
  val brTakens = ((brs & brPred) & ~loopRes)
  // we should provide btb resp as well
  btbHits := btbResp.hits.asUInt

  // predict taken only if btb has a target, jal and br targets will be provided by IFU
  takens := VecInit((0 until PredictWidth).map(i => jalrs(i) && btbHits(i) || (jals(i) || brTakens(i))))


  targets := inLatch.resp.btb.targets

  brMask  := WireInit(brs.asTypeOf(Vec(PredictWidth, Bool())))
  jalMask := WireInit(jals.asTypeOf(Vec(PredictWidth, Bool())))

  hasHalfRVI  := pdLastHalf && HasCExtension.B

  //RAS
  if(EnableRAS){
    val ras = Module(new RAS)
    ras.io <> DontCare
    ras.io.pc.bits := packetAligned(inLatch.pc)
    ras.io.pc.valid := io.outFire//predValid
    ras.io.is_ret := rets.orR  && (retIdx === io.pred.jmpIdx)
    ras.io.callIdx.valid := calls.orR && (callIdx === io.pred.jmpIdx)
    ras.io.callIdx.bits := callIdx
    ras.io.isRVC := (calls & RVCs).orR   //TODO: this is ugly
    ras.io.isLastHalfRVI := s3IO.predecode.hasLastHalfRVI
    ras.io.redirect := s3IO.redirect
    ras.fires <> fires
    ras.ctrl := s3IO.ctrl

    for(i <- 0 until PredictWidth){
      io.out.brInfo.rasSp :=  ras.io.meta.rasSp
      io.out.brInfo.rasTop :=  ras.io.meta.rasTop
    }
    takens := VecInit((0 until PredictWidth).map(i => {
      (jalrs(i) && btbHits(i)) ||
          jals(i) || brTakens(i) ||
          (ras.io.out.valid && rets(i)) ||
          (!ras.io.out.valid && rets(i) && btbHits(i))
      }
    ))

    for (i <- 0 until PredictWidth) {
      when(rets(i) && ras.io.out.valid){
        targets(i) := ras.io.out.bits.target
      }
    }

    // For perf counters
    if (!env.FPGAPlatform && env.EnablePerfDebug) {
      io.out.brInfo.metas.zipWithIndex.foreach{case (meta, i) =>
        // record tage pred result
        meta.tageAns.hit := tageResp.hits(i)
        meta.tageAns.taken := tageResp.takens(i)
        meta.tageAns.target := DontCare

        // record ras pred result
        meta.rasAns.hit := ras.io.out.valid
        meta.rasAns.taken := true.B
        meta.rasAns.target := ras.io.out.bits.target

        // record loop pred result
        meta.loopAns.hit := loopRes(i)
        meta.loopAns.taken := false.B
        meta.loopAns.target := DontCare
      }
    }
  }


  // Wrap tage resp and tage meta in
  // This is ugly
  io.out.resp.tage <> io.in.resp.tage
  io.out.resp.loop <> io.in.resp.loop
  for (i <- 0 until PredictWidth) {
    io.out.brInfo.metas(i).tageMeta := io.in.brInfo.metas(i).tageMeta
    io.out.brInfo.specCnt(i) := io.in.brInfo.specCnt(i)
  }

  if (BPUDebug) {
    XSDebug(io.inFire, "predecode: pc:%x, mask:%b\n", inLatch.pc, s3IO.predecode.mask)
    for (i <- 0 until PredictWidth) {
      val p = s3IO.predecode.pd(i)
      XSDebug(io.inFire && s3IO.predecode.mask(i), "predecode(%d): brType:%d, br:%d, jal:%d, jalr:%d, call:%d, ret:%d, RVC:%d, excType:%d\n",
        i.U, p.brType, p.isBr, p.isJal, p.isJalr, p.isCall, p.isRet, p.isRVC, p.excType)
    }
    XSDebug(p"brs:${Binary(brs)} jals:${Binary(jals)} jalrs:${Binary(jalrs)} calls:${Binary(calls)} rets:${Binary(rets)} rvcs:${Binary(RVCs)}\n")
    XSDebug(p"callIdx:${callIdx} retIdx:${retIdx}\n")
    XSDebug(p"brPred:${Binary(brPred)} loopRes:${Binary(loopRes)} brTakens:${Binary(brTakens)}\n")
  }

  if (EnbaleCFIPredLog) {
    val out = io.out
    XSDebug(io.outFire, p"cfi_pred: fetchpc(${Hexadecimal(out.pc)}) mask(${out.mask}) brmask(${brMask.asUInt}) hist(${Hexadecimal(io.debug_hist)})\n")
  }

  if (EnableBPUTimeRecord) {
    io.out.brInfo.metas.map(_.debug_tage_cycle := GTimer())
  }
}

trait BranchPredictorComponents extends HasXSParameter {
  val ubtb = Module(new MicroBTB)
  val btb = Module(new BTB)
  val bim = Module(new BIM)
  val tage = (if(EnableBPD) { if (EnableSC) Module(new Tage_SC)
                              else          Module(new Tage) }
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

class BPUCtrl extends XSBundle {
  val ubtb_enable = Bool()
  val btb_enable  = Bool()
  val bim_enable  = Bool()
  val tage_enable = Bool()
  val sc_enable   = Bool()
  val ras_enable  = Bool()
  val loop_enable = Bool()
}

abstract class BaseBPU extends XSModule with BranchPredictorComponents
  with HasBPUParameter with HasIFUConst {
  val io = IO(new Bundle() {
    // from backend
    val redirect = Flipped(ValidIO(new Redirect))
    val ctrl     = Input(new BPUCtrl)
    val commit   = Flipped(ValidIO(new FtqEntry))
    // from if1
    val in = Input(new BPUReq)
    val inFire = Input(Vec(4, Bool()))
    // to if1
    val in_ready = Output(Bool())
    // to if2/if3/if4
    val out = Vec(3, Output(new BranchPrediction))
    // from if4
    val predecode = Input(new Predecode)
    // to if4, some bpu info used for updating
    val brInfo = Output(new BrInfo)
  })

  preds.map(p => {
    p.io.update <> io.commit
    p.fires <> io.inFire
    p.ctrl <> io.ctrl
  })
  
  io.in_ready := preds.map(p => p.in_ready).reduce(_&&_)

  val s1 = Module(new BPUStage1)
  val s2 = Module(new BPUStage2)
  val s3 = Module(new BPUStage3)

  Seq(s1, s2, s3).foreach(s => s.fires <> io.inFire)

  val s1_fire = io.inFire(0)
  val s2_fire = io.inFire(1)
  val s3_fire = io.inFire(2)
  val s4_fire = io.inFire(3)

  s1.io.in <> DontCare
  s2.io.in <> s1.io.out
  s3.io.in <> s2.io.out

  s1.io.inFire := s1_fire
  s2.io.inFire := s2_fire
  s3.io.inFire := s3_fire

  s1.io.outFire := s2_fire
  s2.io.outFire := s3_fire
  s3.io.outFire := s4_fire

  io.out(0) <> s1.io.pred
  io.out(1) <> s2.io.pred
  io.out(2) <> s3.io.pred

  io.brInfo := s3.io.out.brInfo

  if (BPUDebug) {
    XSDebug(io.inFire(3), "bpuMeta sent!\n")
    for (i <- 0 until PredictWidth) {
      val b = io.brInfo.metas(i)
      XSDebug(io.inFire(3), "brInfo(%d): btbWrWay:%d, bimCtr:%d\n",
        i.U, b.btbWriteWay, b.bimCtr)
      val t = b.tageMeta
      XSDebug(io.inFire(3), "  tageMeta: pvder(%d):%d, altDiffers:%d, pvderU:%d, pvderCtr:%d, allocate(%d):%d\n",
        t.provider.valid, t.provider.bits, t.altDiffers, t.providerU, t.providerCtr, t.allocate.valid, t.allocate.bits)
    }
  }
  val debug_verbose = false
}


class FakeBPU extends BaseBPU {
  io.out.foreach(i => {
    // Provide not takens
    i <> DontCare
    i.takens := 0.U
  })
  io.brInfo <> DontCare
}
@chiselName
class BPU extends BaseBPU {

  //**********************Stage 1****************************//

  val s1_resp_in = Wire(new PredictorResponse)
  val s1_brInfo_in = Wire(new BrInfo)

  s1_resp_in.tage := DontCare
  s1_resp_in.loop := DontCare
  s1_brInfo_in    := DontCare

  val s1_inLatch = RegEnable(io.in, s1_fire)
  ubtb.io.pc.valid := s2_fire
  ubtb.io.pc.bits := s1_inLatch.pc
  ubtb.io.inMask := s1_inLatch.inMask



  // Wrap ubtb response into resp_in and brInfo_in
  s1_resp_in.ubtb <> ubtb.io.out
  for (i <- 0 until PredictWidth) {
    s1_brInfo_in.metas(i).ubtbHit := ubtb.io.out.hits(i)
  }

  btb.io.pc.valid := s1_fire
  btb.io.pc.bits := io.in.pc
  btb.io.inMask := io.in.inMask



  // Wrap btb response into resp_in and brInfo_in
  s1_resp_in.btb <> btb.io.resp
  for (i <- 0 until PredictWidth) {
    s1_brInfo_in.metas(i).btbWriteWay := btb.io.meta.writeWay(i)
    s1_brInfo_in.metas(i).btbHit := btb.io.meta.hits(i)
  }

  bim.io.pc.valid := s1_fire
  bim.io.pc.bits := io.in.pc
  bim.io.inMask := io.in.inMask


  // Wrap bim response into resp_in and brInfo_in
  s1_resp_in.bim <> bim.io.resp
  for (i <- 0 until PredictWidth) {
    s1_brInfo_in.metas(i).bimCtr := bim.io.meta.ctrs(i)
  }


  s1.io.inFire := s1_fire
  s1.io.in.pc := io.in.pc
  s1.io.in.mask := io.in.inMask
  s1.io.in.resp <> s1_resp_in
  s1.io.in.brInfo <> s1_brInfo_in

  val s1_hist = RegEnable(io.in.hist, enable=s1_fire)
  val s2_hist = RegEnable(s1_hist, enable=s2_fire)
  val s3_hist = RegEnable(s2_hist, enable=s3_fire)

  s1.io.debug_hist := s1_hist
  s2.io.debug_hist := s2_hist
  s3.io.debug_hist := s3_hist

  //**********************Stage 2****************************//
  tage.io.pc.valid := s2_fire
  tage.io.pc.bits := s2.io.in.pc // PC from s1
  tage.io.hist := s1_hist // The inst is from s1
  tage.io.inMask := s2.io.in.mask
  tage.io.bim <> s1.io.out.resp.bim // Use bim results from s1

  //**********************Stage 3****************************//
  // Wrap tage response and meta into s3.io.in.bits
  // This is ugly

  loop.io.pc.valid := s2_fire
  loop.io.if3_fire := s3_fire
  loop.io.pc.bits := s2.io.in.pc
  loop.io.inMask := io.predecode.mask
  loop.io.respIn.taken := s3.io.pred.taken
  loop.io.respIn.jmpIdx := s3.io.pred.jmpIdx
  loop.io.redirect := s3.s3IO.redirect


  s3.io.in.resp.tage <> tage.io.resp
  s3.io.in.resp.loop <> loop.io.resp
  for (i <- 0 until PredictWidth) {
    s3.io.in.brInfo.metas(i).tageMeta := tage.io.meta(i)
    s3.io.in.brInfo.specCnt(i) := loop.io.meta.specCnts(i)
  }

  s3.s3IO.predecode <> io.predecode
  s3.s3IO.redirect <> io.redirect
  s3.s3IO.ctrl <> io.ctrl
  

  if (BPUDebug) {
    if (debug_verbose) {
      val uo = ubtb.io.out
      XSDebug("debug: ubtb hits:%b, takens:%b, notTakens:%b\n", uo.hits.asUInt, uo.takens.asUInt, ~uo.takens.asUInt & uo.brMask.asUInt)
      val bio = bim.io.resp
      XSDebug("debug: bim takens:%b\n", VecInit(bio.ctrs.map(_(1))).asUInt)
      val bo = btb.io.resp
      XSDebug("debug: btb hits:%b\n", bo.hits.asUInt)
    }
  }



  if (EnableCFICommitLog) {
    val buValid = io.commit.valid
    val buinfo  = io.commit.bits
    for (i <- 0 until PredictWidth) {
      val cfi_idx = buinfo.cfiIndex
      val isTaken = cfi_idx.valid && cfi_idx.bits === i.U
      val isCfi = buinfo.valids(i) && (buinfo.br_mask(i) || cfi_idx.valid && cfi_idx.bits === i.U)
      val isBr = buinfo.br_mask(i)
      val pc = packetAligned(buinfo.ftqPC) + (i * instBytes).U - Mux((i==0).B && buinfo.hasLastPrev, 2.U, 0.U)
      val tage_cycle = buinfo.metas(i).debug_tage_cycle
      XSDebug(buValid && isCfi, p"cfi_update: isBr(${isBr}) pc(${Hexadecimal(pc)}) taken(${isTaken}) mispred(${buinfo.mispred(i)}) cycle($tage_cycle) hist(${Hexadecimal(buinfo.predHist.asUInt)})\n")
    }
  }

}

object BPU{
  def apply(enableBPU: Boolean = true) = {
      if(enableBPU) {
        val BPU = Module(new BPU)
        BPU
      }
      else {
        val FakeBPU = Module(new FakeBPU)
        FakeBPU
      }
  }
}
