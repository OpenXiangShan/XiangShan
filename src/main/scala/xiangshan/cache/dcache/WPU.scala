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

class WPUBaseIO(implicit p:Parameters) extends WPUBuddle {
  val pred_en = Input(Bool())
  val pred_vaddr = Input(UInt(VAddrBits.W))
  val pred_way_en = Output(UInt(nWays.W))

  val update_en = Input(Bool())
  val update_vaddr = Input(UInt(VAddrBits.W))
  val update_way_en = Input(UInt(nWays.W))
}

class MruWPU (implicit p:Parameters) extends WPUModule{
  val io = IO(new WPUBaseIO)

  println("  WPU: MRU")
  val predict_regs = RegInit(VecInit(Seq.fill(nSets)(0.U(wayBits.W))))

  val predSetIdx = get_idx(io.pred_vaddr)
  when(io.pred_en){
    io.pred_way_en := UIntToOH(predict_regs(predSetIdx))
  }.otherwise{
    io.pred_way_en := 0.U(nWays.W)
  }

  val updSetIdx = get_idx(io.update_vaddr)
  when(io.update_en) {
    predict_regs(updSetIdx) := OHToUInt(io.update_way_en)
  }
}

class MmruWPU(implicit p:Parameters) extends WPUModule {
  val io = IO(new WPUBaseIO)

  println("  WPU: MMRU")
  val predict_regs = RegInit(VecInit(Seq.fill(nSets)(VecInit(Seq.fill(nTagIdx)(0.U(auxWayBits.W))))))

  val predSetIdx = get_idx(io.pred_vaddr)
  val predTagIdx = get_vir_tag(io.pred_vaddr)
  when(io.pred_en) {
    //UIntToOH(8.U(4.W))=100000000.U(16.W)=00000000.U(8.W)
    //UIntToOH(8.U(4.W), 8)=00000001.U(8.W)
    io.pred_way_en := UIntToOH(predict_regs(predSetIdx)(predTagIdx))
  }.otherwise {
    io.pred_way_en := 0.U(nWays.W)
  }

  val updSetIdx = get_idx(io.update_vaddr)
  val updTagIdx = get_vir_tag(io.update_vaddr)
  when(io.update_en){
    predict_regs(updSetIdx)(updTagIdx) := OHToUInt(io.update_way_en)
  }/*.otherwise{
    predict_regs(updSetIdx)(updTagIdx) := nWays.asUInt
  }*/
}