package xiangshan.cache.dcache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._

/*
// TODO: need to learn the specific grammar
abstract class WPUBaseModule[T <: Data](implicit P: Parameters) extends XSModule with HasWPUParameters{
  def apply[T <: Data]
  def pred(vaddr: UInt, en: Bool) : T
  def update(vaddr: UInt, data: T ,en: Bool)
}
*/
class BaseWpuUpdateBuddle(implicit p: Parameters) extends WPUBuddle{
  val update_en = Bool()
  val update_vaddr = UInt(VAddrBits.W)
  val update_way_en = UInt(nWays.W)
}

class WPUBaseIO(implicit p:Parameters) extends WPUBuddle {
  val pred_en = Input(Bool())
  val pred_vaddr = Input(UInt(VAddrBits.W))
  val pred_way_en = Output(UInt(nWays.W))

  val lookup_upd = Input(new BaseWpuUpdateBuddle)
  val replaycarry_upd = Input(new BaseWpuUpdateBuddle)
  val tagwrite_upd = Input(new BaseWpuUpdateBuddle)
}

abstract class BaseWPU(implicit p:Parameters) extends WPUModule{
  val io = IO(new WPUBaseIO)
}

class MruWPU (implicit p:Parameters) extends BaseWPU{
  val predict_regs = RegInit(VecInit(Seq.fill(nSets)(0.U(wayBits.W))))

  def write(upd: BaseWpuUpdateBuddle): Unit = {
    when(upd.update_en) {
      val upd_setIdx = get_idx(upd.update_vaddr)
      predict_regs(upd_setIdx) := OHToUInt(upd.update_way_en)
    }
  }

  val predSetIdx = get_idx(io.pred_vaddr)
  when(io.pred_en){
    io.pred_way_en := UIntToOH(predict_regs(predSetIdx))
  }.otherwise{
    io.pred_way_en := 0.U(nWays.W)
  }

  write(io.lookup_upd)
  write(io.replaycarry_upd)
  write(io.tagwrite_upd)
}

class MmruWPU(implicit p:Parameters) extends BaseWPU {
  val predict_regs = RegInit(VecInit(Seq.fill(nSets)(VecInit(Seq.fill(nTagIdx)(0.U(auxWayBits.W))))))

  def write(upd: BaseWpuUpdateBuddle): Unit = {
    when(upd.update_en) {
      val updSetIdx = get_idx(upd.update_vaddr)
      val updTagIdx = get_vir_tag(upd.update_vaddr)
      predict_regs(updSetIdx)(updTagIdx) := OHToUInt(upd.update_way_en)
    }
  }

  val predSetIdx = get_idx(io.pred_vaddr)
  val predTagIdx = get_vir_tag(io.pred_vaddr)
  when(io.pred_en) {
    //UIntToOH(8.U(4.W))=100000000.U(16.W)=00000000.U(8.W)
    //UIntToOH(8.U(4.W), 8)=00000001.U(8.W)
    io.pred_way_en := UIntToOH(predict_regs(predSetIdx)(predTagIdx))
  }.otherwise {
    io.pred_way_en := 0.U(nWays.W)
  }

  write(io.lookup_upd)
  write(io.replaycarry_upd)
  write(io.tagwrite_upd)
}

class UtagWPU(implicit p:Parameters) extends BaseWPU{
  val utag_regs = RegInit(VecInit(Seq.fill(nSets)(VecInit(Seq.fill(nWays)(0.U(utagBits.W))))))
  val valid_regs = RegInit(VecInit(Seq.fill(nSets)(VecInit(Seq.fill(nWays)(false.B)))))

  def get_hash_utag(addr: UInt): UInt = {
    val vtag = get_vir_tag(addr)
    vtag(utagBits * 2 - 1, utagBits) ^ vtag(utagBits - 1, 0)
  }

  def write_utag(upd: BaseWpuUpdateBuddle): Unit = {
    when(upd.update_en){
      val upd_setIdx = get_idx(upd.update_vaddr)
      val upd_utag = get_hash_utag(upd.update_vaddr)
      val upd_way = OHToUInt(upd.update_way_en)
      utag_regs(upd_setIdx)(upd_way) := upd_utag
      valid_regs(upd_setIdx)(upd_way) := true.B
    }
  }

  def unvalid_utag(upd: BaseWpuUpdateBuddle): Unit = {
    when(upd.update_en){
      val upd_setIdx = get_idx(upd.update_vaddr)
      val upd_way = OHToUInt(upd.update_way_en)
      valid_regs(upd_setIdx)(upd_way) := false.B
    }
  }

  val req_setIdx = get_idx(io.pred_vaddr)
  val req_utag = get_hash_utag(io.pred_vaddr)
  val pred_way_en = Wire(UInt(nWays.W))
  when(io.pred_en) {
    pred_way_en := Cat((0 until  nWays).map(i => req_utag === utag_regs(req_setIdx)(i) && valid_regs(req_setIdx)(i)).reverse)
  }.otherwise {
    pred_way_en := 0.U(nWays.W)
  }
  // avoid hash conflict
  io.pred_way_en := UIntToOH(OHToUInt(pred_way_en))

  val s1_pred_way_en = RegNext(io.pred_way_en)
  val s1_vtag_look_miss = !s1_pred_way_en.orR

  // look up: vtag miss but tag hit
  when(s1_vtag_look_miss && io.lookup_upd.update_way_en.orR){
    write_utag(io.lookup_upd)
  }

  // look up: vtag hit but other tag hit
  when(!s1_vtag_look_miss && io.lookup_upd.update_way_en.orR && s1_pred_way_en =/= io.lookup_upd.update_way_en) {
    unvalid_utag(io.lookup_upd)
  }

  // replay carry
  write_utag(io.replaycarry_upd)

  // tag write
  write_utag(io.tagwrite_upd)

}