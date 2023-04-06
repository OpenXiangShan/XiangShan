package xiangshan.cache.dcache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.IO
import chisel3.util.{Fill, log2Ceil}
import xiangshan._
import xiangshan.cache.HasDCacheParameters

trait HasVictimParameters extends HasDCacheParameters{
  // Victim List
  // (addr, counter)
  // WayConflictPredictTable
  // val VictimListSize = 16
  val VictimListSize = nSets
  val VictimWidth = 2
}

trait WayConflictPredictParameters extends HasDCacheParameters {
  val WCPSize = 1024
  val CounterSize = 2
  val IdxBits = log2Ceil(WCPSize)
  val PCOffBits = 2
  // TODO: not correct
  def get_pc_idx(addr: UInt) = { addr(IdxBits+PCOffBits, PCOffBits)}
}

case class VictimList(nSets: Int, width: Int = 2) {
  val victim_vec = RegInit(VecInit(Seq.fill(nSets)(0.U(width.W))))

  // replace to search
  def replace(set: UInt) = {
    when(victim_vec(set) =/= Fill(width, 1.U)) {
      victim_vec(set) := victim_vec(set) + 1.U
    }
  }

  def whether_sa(set:UInt) = victim_vec(set)(width-1)
}

abstract class WayConflictPredictorModule(implicit P: Parameters) extends XSModule with WayConflictPredictParameters

class WayConflictPredictor (implicit p: Parameters) extends WayConflictPredictorModule{
  val io = IO(new Bundle() {
    val pred_en = Input(Bool())
    val pred_pc = Input(UInt(VAddrBits.W))
    val pred_way_conflict = Output(Bool())

    val update_en = Input(Bool())
    val update_pc = Input(UInt(VAddrBits.W))
    val update_dm_hit = Input(Bool())
    val update_sa_hit = Input(Bool())
  })
  // TODO: how to design this? how to understand VictimList and WayConflictPredict ?
  val PredTable = RegInit(VecInit(Seq.fill(WCPSize)(0.U(CounterSize.W))))

  io.pred_way_conflict := io.pred_en & PredTable(get_pc_idx(io.pred_pc))(CounterSize-1)

  // saturation counter
  when(io.update_en && io.update_sa_hit){
    when(PredTable(get_pc_idx(io.update_pc)) === Fill(CounterSize, 1.U)){
      PredTable(get_pc_idx(io.update_pc)) := PredTable(get_pc_idx(io.update_pc))
    }.otherwise{
      PredTable(get_pc_idx(io.update_pc)) := PredTable(get_pc_idx(io.update_pc)) + 1.U
    }
  }.elsewhen(io.update_en && io.update_dm_hit){
    when(PredTable(get_pc_idx(io.update_pc)) === Fill(CounterSize, 0.U)) {
      PredTable(get_pc_idx(io.update_pc)) := PredTable(get_pc_idx(io.update_pc))
    }.otherwise {
      PredTable(get_pc_idx(io.update_pc)) := PredTable(get_pc_idx(io.update_pc)) - 1.U
    }
  }.otherwise{
    PredTable(get_pc_idx(io.update_pc)) := PredTable(get_pc_idx(io.update_pc))
  }

}