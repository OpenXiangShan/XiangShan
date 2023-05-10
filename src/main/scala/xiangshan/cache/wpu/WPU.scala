package xiangshan.cache.wpu

import chipsalliance.rocketchip.config.{Field, Parameters}
import chisel3._
import chisel3.util._
import utils.XSPerfAccumulate
import xiangshan.cache.{HasL1CacheParameters, L1CacheParameters}
import xiangshan.{XSBundle, XSModule}

/*
// TODO: need to learn the specific grammar
abstract class WPUBaseModule[T <: Data](implicit P: Parameters) extends XSModule with HasWPUParameters{
  def apply[T <: Data]
  def pred(vaddr: UInt, en: Bool) : T
  def update(vaddr: UInt, data: T ,en: Bool)
}
*/

case object WPUParamsKey extends Field[WPUParameters]
case class WPUParameters
(
  enWPU: Boolean = true,
  algoName: String = "mru",
  enCfPred: Boolean = false,
  isICache: Boolean = false,
  // how to impelement a extend inlcude hasL1Cache and L2 Cache
)

trait HasWPUParameters extends HasL1CacheParameters{
  def AlgoWPUMap(wpuParam: WPUParameters, nPorts: Int): BaseWPU = {
    wpuParam.algoName.toLowerCase match {
      case "mru" => Module(new MruWPU(wpuParam, nPorts))
      case "mmru" => Module(new MmruWPU(wpuParam, nPorts))
      case "utag" => Module(new UtagWPU(wpuParam, nPorts))
      case t => throw new IllegalArgumentException(s"unknown WPU Algorithm $t")
    }
  }
}

abstract class BaseWPUBundle(implicit P: Parameters) extends XSBundle
abstract class WPUModule(implicit P: Parameters) extends XSModule with HasWPUParameters

class BaseWpuUpdateBundle(nWays: Int)(implicit p: Parameters) extends BaseWPUBundle{
  val en = Bool()
  val vaddr = UInt(VAddrBits.W)
  val way_en = UInt(nWays.W)
}

class LookupWpuUpdateBundle(nWays: Int)(implicit p: Parameters) extends BaseWpuUpdateBundle(nWays){
  val pred_way_en = UInt(nWays.W)
}

class BaseWpuPredictIO(nWays: Int)(implicit p: Parameters) extends BaseWPUBundle{
  val en = Input(Bool())
  val vaddr = Input(UInt(VAddrBits.W))
  val way_en = Output(UInt(nWays.W))
}

class WPUBaseIO(portNum: Int, nWays: Int)(implicit p:Parameters) extends BaseWPUBundle {
  val predVec = Vec(portNum, new BaseWpuPredictIO(nWays))
  val updLookup = Input(Vec(portNum, new LookupWpuUpdateBundle(nWays)))
  val updReplaycarry = Input(Vec(portNum, new BaseWpuUpdateBundle(nWays)))
  val updTagwrite = Input(Vec(portNum, new BaseWpuUpdateBundle(nWays)))
}

abstract class BaseWPU(wpuParam: WPUParameters, nPorts: Int)(implicit p:Parameters) extends WPUModule {
  val cacheParams: L1CacheParameters = if (wpuParam.isICache) icacheParameters else dcacheParameters

  val setSize = nSets
  val nTagIdx = nWays
  // auxiliary 1 bit is used to judge whether cache miss
  val auxWayBits = wayBits + 1
  val TagIdxBits = log2Up(nTagIdx)
  val utagBits = 8

  val io = IO(new WPUBaseIO(nPorts, nWays))

  def get_wpu_idx(addr: UInt): UInt = {
    addr(untagBits - 1, blockOffBits)
  }
}

class MruWPU(wpuParam: WPUParameters, nPorts: Int)(implicit p:Parameters) extends BaseWPU(wpuParam, nPorts){
  println("  WpuType: MruWPU")
  val predict_regs = RegInit(VecInit(Seq.fill(setSize)(0.U(wayBits.W))))

  def write(upd: BaseWpuUpdateBundle): Unit = {
    when(upd.en) {
      val upd_setIdx = get_wpu_idx(upd.vaddr)
      predict_regs(upd_setIdx) := OHToUInt(upd.way_en)
    }
  }

  def predict(pred: BaseWpuPredictIO): Unit = {
    val predSetIdx = get_wpu_idx(pred.vaddr)
    when(pred.en) {
      pred.way_en := UIntToOH(predict_regs(predSetIdx))
    }.otherwise {
      pred.way_en := 0.U(nWays.W)
    }
  }

  for(i <- 0 until nPorts){
    predict(io.predVec(i))
    write(io.updLookup(i))
    write(io.updReplaycarry(i))
    write(io.updTagwrite(i))
  }

}

class MmruWPU(wpuParam: WPUParameters, nPorts: Int)(implicit p:Parameters) extends BaseWPU(wpuParam, nPorts){
  println("  WpuType: MmruWPU")
  val predict_regs = RegInit(VecInit(Seq.fill(setSize)(VecInit(Seq.fill(nTagIdx)(0.U(auxWayBits.W))))))

  def write(upd: BaseWpuUpdateBundle): Unit = {
    when(upd.en) {
      val updSetIdx = get_wpu_idx(upd.vaddr)
      val updTagIdx = get_vir_tag(upd.vaddr)
      predict_regs(updSetIdx)(updTagIdx) := OHToUInt(upd.way_en)
    }
  }

  def predict(pred: BaseWpuPredictIO): Unit = {
    val predSetIdx = get_wpu_idx(pred.vaddr)
    val predTagIdx = get_vir_tag(pred.vaddr)
    when(pred.en) {
      //UIntToOH(8.U(4.W))=100000000.U(16.W)=00000000.U(8.W)
      //UIntToOH(8.U(4.W), 8)=00000001.U(8.W)
      pred.way_en := UIntToOH(predict_regs(predSetIdx)(predTagIdx))
    }.otherwise {
      pred.way_en := 0.U(nWays.W)
    }
  }

  for(i <- 0 until nPorts){
    predict(io.predVec(i))
    write(io.updLookup(i))
    write(io.updReplaycarry(i))
    write(io.updTagwrite(i))
  }

}

class UtagWPU(wpuParam: WPUParameters, nPorts: Int)(implicit p:Parameters) extends BaseWPU(wpuParam, nPorts){
  println("  WpuType: UtagWPU")
  val utag_regs = RegInit(VecInit(Seq.fill(setSize)(VecInit(Seq.fill(nWays)(0.U(utagBits.W))))))
  val valid_regs = RegInit(VecInit(Seq.fill(setSize)(VecInit(Seq.fill(nWays)(false.B)))))

  def get_hash_utag(addr: UInt): UInt = {
    val utagQuotient = vtagBits / utagBits
    val utagRemainder = vtagBits % utagBits
    val vtag = get_vir_tag(addr)

    /* old */
    vtag(utagBits * 2 - 1, utagBits) ^ vtag(utagBits - 1, 0)
    
    /* new */
    // val tmp = vtag(utagQuotient * utagBits - 1, 0).asTypeOf(Vec(utagQuotient, UInt(utagBits.W)))
    // val res1 = tmp.reduce(_ ^ _)
    // val res2 = Wire(UInt(utagRemainder.W))
    // if(utagRemainder!=0){
    //   res2 := res1(utagRemainder - 1, 0) ^ vtag(vtagBits - 1, utagBits * utagQuotient)
    //   Cat(res1(utagBits - 1, utagRemainder), res2)
    // }else{
    //   res1
    // }
  }

  def write_utag(upd: BaseWpuUpdateBundle): Unit = {
    when(upd.en){
      val upd_setIdx = get_wpu_idx(upd.vaddr)
      val upd_utag = get_hash_utag(upd.vaddr)
      val upd_way = OHToUInt(upd.way_en)
      utag_regs(upd_setIdx)(upd_way) := upd_utag
      valid_regs(upd_setIdx)(upd_way) := true.B
    }
  }

  def unvalid_utag(upd: LookupWpuUpdateBundle): Unit = {
    when(upd.en){
      val upd_setIdx = get_wpu_idx(upd.vaddr)
      val upd_way = OHToUInt(upd.pred_way_en)
      valid_regs(upd_setIdx)(upd_way) := false.B
    }
  }

  def predict(pred: BaseWpuPredictIO): Unit = {
    val req_setIdx = get_wpu_idx(pred.vaddr)
    val req_utag = get_hash_utag(pred.vaddr)
    val pred_way_en = Wire(UInt(nWays.W))
    when(pred.en) {
      pred_way_en := VecInit((0 until nWays).map(i => req_utag === utag_regs(req_setIdx)(i) && valid_regs(req_setIdx)(i))).asUInt
    }.otherwise {
      pred_way_en := 0.U(nWays.W)
    }
    // avoid hash conflict
    pred.way_en := UIntToOH(OHToUInt(pred_way_en))
  }

  val hash_conflict = Wire(Vec(nPorts, Bool()))
  for(i <- 0 until nPorts){
    predict(io.predVec(i))
    val real_way_en = io.updLookup(i).way_en
    val pred_way_en = io.updLookup(i).pred_way_en
    val pred_miss = io.updLookup(i).en && !pred_way_en.orR
    val real_miss = io.updLookup(i).en && !real_way_en.orR
    val way_match = io.updLookup(i).en && pred_way_en === real_way_en

    hash_conflict(i) := !pred_miss && !way_match
      // look up: vtag miss but tag hit
    when(pred_miss && !real_miss) {
      write_utag(io.updLookup(i))
    }
    // look up: vtag hit but other tag hit ==> unvalid pred way; write real way
    when(!pred_miss && !way_match) {
      unvalid_utag(io.updLookup(i))
    }
    when(!pred_miss && !real_miss && !way_match) {
      write_utag(io.updLookup(i))
    }
    // replay carry
    write_utag(io.updReplaycarry(i))
    // tag write
    write_utag(io.updTagwrite(i))
  }

  XSPerfAccumulate("utag_hash_conflict", PopCount(hash_conflict))
}