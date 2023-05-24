package xiangshan.cache.wpu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
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

abstract class WayConflictPredictorBundle(implicit P: Parameters) extends XSBundle with WayConflictPredictParameters
abstract class WayConflictPredictorModule(implicit P: Parameters) extends XSModule with WayConflictPredictParameters

class WayConflictPredIO (implicit p: Parameters) extends WayConflictPredictorBundle {
  val en = Input(Bool())
  val pc = Input(UInt(VAddrBits.W))
  val way_conflict = Output(Bool())
}

class WayConflictUpdIO (implicit p: Parameters) extends WayConflictPredictorBundle {
  val en = Input(Bool())
  val pc = Input(UInt(VAddrBits.W))
  val dm_hit = Input(Bool())
  val sa_hit = Input(Bool())
}

class WayConflictPredictor (nPorts: Int) (implicit p: Parameters) extends WayConflictPredictorModule{
  val io = IO(new Bundle() {
    val pred = Vec(nPorts, new WayConflictPredIO)
    val update = Vec(nPorts, new WayConflictUpdIO)
  })
  // TODO: how to design this? how to understand VictimList and WayConflictPredict ?
  val PredTable = RegInit(VecInit(Seq.fill(WCPSize)(0.U(CounterSize.W))))

  for (i <- 0 until nPorts){
    io.pred(i).way_conflict := io.pred(i).en & PredTable(get_pc_idx(io.pred(i).pc))(CounterSize-1)
    // saturation counter
    when(io.update(i).en && io.update(i).sa_hit) {
      when(PredTable(get_pc_idx(io.update(i).pc)) === Fill(CounterSize, 1.U)) {
        PredTable(get_pc_idx(io.update(i).pc)) := PredTable(get_pc_idx(io.update(i).pc))
      }.otherwise {
        PredTable(get_pc_idx(io.update(i).pc)) := PredTable(get_pc_idx(io.update(i).pc)) + 1.U
      }
    }.elsewhen(io.update(i).en && io.update(i).dm_hit) {
      when(PredTable(get_pc_idx(io.update(i).pc)) === Fill(CounterSize, 0.U)) {
        PredTable(get_pc_idx(io.update(i).pc)) := PredTable(get_pc_idx(io.update(i).pc))
      }.otherwise {
        PredTable(get_pc_idx(io.update(i).pc)) := PredTable(get_pc_idx(io.update(i).pc)) - 1.U
      }
    }.otherwise {
      PredTable(get_pc_idx(io.update(i).pc)) := PredTable(get_pc_idx(io.update(i).pc))
    }
  }

}