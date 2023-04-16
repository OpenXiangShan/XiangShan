package xiangshan.cache.wpu

import chipsalliance.rocketchip.config.{Field, Parameters}
import chisel3._
import chisel3.util._
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
  portNum: Int = 1
  // how to impelement a extend inlcude hasL1Cache and L2 Cache
)

trait HasWPUParameters extends HasL1CacheParameters{
  def AlgoWPUMap(wpuParam: WPUParameters): BaseWPU = {
    wpuParam.algoName.toLowerCase match {
      case "mru" => Module(new MruWPU(wpuParam))
      case "mmru" => Module(new MmruWPU(wpuParam))
      case "utag" => Module(new UtagWPU(wpuParam))
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

class BaseWpuPredictIO(nWays: Int)(implicit p: Parameters) extends BaseWPUBundle{
  val en = Input(Bool())
  val vaddr = Input(UInt(VAddrBits.W))
  val way_en = Output(UInt(nWays.W))
}

class WPUBaseIO(portNum: Int, nWays: Int)(implicit p:Parameters) extends BaseWPUBundle {
  val predVec = Vec(portNum, new BaseWpuPredictIO(nWays))
  val updLookup = Input(Vec(portNum, new BaseWpuUpdateBundle(nWays)))
  val updReplaycarry = Input(Vec(portNum, new BaseWpuUpdateBundle(nWays)))
  val updTagwrite = Input(Vec(portNum, new BaseWpuUpdateBundle(nWays)))
}

abstract class BaseWPU(wpuParam: WPUParameters)(implicit p:Parameters) extends WPUModule {
  val cacheParams: L1CacheParameters = if (wpuParam.isICache) icacheParameters else dcacheParameters

  val setSize = if (wpuParam.isICache) nSets/2 else nSets
  val nTagIdx = nWays
  // auxiliary 1 bit is used to judge whether cache miss
  val auxWayBits = wayBits + 1
  val TagIdxBits = log2Up(nTagIdx)
  val utagBits = 8

  val io = IO(new WPUBaseIO(wpuParam.portNum, nWays))

  def get_wpu_idx(addr: UInt): UInt = {
    if (wpuParam.isICache) {
      // NOTE: in icache, set[0] indicates which bank to choose
      addr(untagBits - 1, blockOffBits + 1)
    } else {
      addr(untagBits - 1, blockOffBits)
    }
  }
}

class MruWPU(wpuParam: WPUParameters)(implicit p:Parameters) extends BaseWPU(wpuParam){
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

  for(i <- 0 until wpuParam.portNum){
    predict(io.predVec(i))
    write(io.updLookup(i))
    write(io.updReplaycarry(i))
    write(io.updTagwrite(i))
  }

}

class MmruWPU(wpuParam: WPUParameters)(implicit p:Parameters) extends BaseWPU(wpuParam){
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

  for(i <- 0 until wpuParam.portNum){
    predict(io.predVec(i))
    write(io.updLookup(i))
    write(io.updReplaycarry(i))
    write(io.updTagwrite(i))
  }

}

class UtagWPU(wpuParam: WPUParameters)(implicit p:Parameters) extends BaseWPU(wpuParam){
  val utag_regs = RegInit(VecInit(Seq.fill(setSize)(VecInit(Seq.fill(nWays)(0.U(utagBits.W))))))
  val valid_regs = RegInit(VecInit(Seq.fill(setSize)(VecInit(Seq.fill(nWays)(false.B)))))

  def get_hash_utag(addr: UInt): UInt = {
    val vtag = get_vir_tag(addr)
    vtag(utagBits * 2 - 1, utagBits) ^ vtag(utagBits - 1, 0)
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

  def unvalid_utag(upd: BaseWpuUpdateBundle): Unit = {
    when(upd.en){
      val upd_setIdx = get_wpu_idx(upd.vaddr)
      val upd_way = OHToUInt(upd.way_en)
      valid_regs(upd_setIdx)(upd_way) := false.B
    }
  }

  def predict(pred: BaseWpuPredictIO): Unit = {
    val req_setIdx = get_wpu_idx(pred.vaddr)
    val req_utag = get_hash_utag(pred.vaddr)
    val pred_way_en = Wire(UInt(nWays.W))
    when(pred.en) {
      pred_way_en := Cat((0 until nWays).map(i => req_utag === utag_regs(req_setIdx)(i) && valid_regs(req_setIdx)(i)).reverse)
    }.otherwise {
      pred_way_en := 0.U(nWays.W)
    }
    // avoid hash conflict
    pred.way_en := UIntToOH(OHToUInt(pred_way_en))
  }

  for(i <- 0 until wpuParam.portNum){
    predict(io.predVec(i))
    // FIXME: There should be no known timing
    val s1_pred_way_en = RegNext(io.predVec(i).way_en)
    val s1_vtag_look_miss = !s1_pred_way_en.orR

    // look up: vtag miss but tag hit
    when(s1_vtag_look_miss && io.updLookup(i).way_en.orR) {
      write_utag(io.updLookup(i))
    }
    // look up: vtag hit but other tag hit
    when(!s1_vtag_look_miss && io.updLookup(i).way_en.orR && s1_pred_way_en =/= io.updLookup(i).way_en) {
      unvalid_utag(io.updLookup(i))
    }
    // replay carry
    write_utag(io.updReplaycarry(i))
    // tag write
    write_utag(io.updTagwrite(i))
  }

}