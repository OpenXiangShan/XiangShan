package xiangshan.cache.wpu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util.{Fill, log2Up}
import xiangshan._
import xiangshan.cache.HasDCacheParameters

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

trait WayConflictPredictParameters extends HasDCacheParameters {
  val WCPSize = DCacheSets
  val IdxBits = log2Up(WCPSize)
  val CounterSize = 2
  val PCOffBits = 2
  def get_pc_idx(pc: UInt) = { pc(IdxBits+PCOffBits, PCOffBits)}
  def get_addr_idx(addr: UInt) = { addr(IdxBits+DCacheSetOffset,DCacheSetOffset)}
}

abstract class WayConflictPredictorBundle(implicit P: Parameters) extends XSBundle with WayConflictPredictParameters
abstract class WayConflictPredictorModule(implicit P: Parameters) extends XSModule with WayConflictPredictParameters

class WayConflictPredIO (implicit p: Parameters) extends WayConflictPredictorBundle {
  val en = Input(Bool())
  val vaddr = Input(UInt(VAddrBits.W))
  val way_conflict = Output(Bool())
}

class WayConflictUpdIO (implicit p: Parameters) extends WayConflictPredictorBundle {
  val en = Input(Bool())
  val vaddr = Input(UInt(VAddrBits.W))
  val dm_hit = Input(Bool())
  val sa_hit = Input(Bool())
}

class WayConflictPredictor (nPorts: Int) (implicit p: Parameters) extends WayConflictPredictorModule{
  val io = IO(new Bundle() {
    val pred = Vec(nPorts, new WayConflictPredIO)
    val update = Vec(nPorts, new WayConflictUpdIO)
  })
  val PredTable = RegInit(VecInit(Seq.fill(WCPSize)(0.U(CounterSize.W))))

  for (i <- 0 until nPorts){
    io.pred(i).way_conflict := io.pred(i).en & PredTable(get_addr_idx(io.pred(i).vaddr))(CounterSize-1)
    // saturation counter
    when(io.update(i).en && io.update(i).sa_hit) {
      when(PredTable(get_addr_idx(io.update(i).vaddr)) === Fill(CounterSize, 1.U)) {
        PredTable(get_addr_idx(io.update(i).vaddr)) := PredTable(get_addr_idx(io.update(i).vaddr))
      }.otherwise {
        PredTable(get_addr_idx(io.update(i).vaddr)) := PredTable(get_addr_idx(io.update(i).vaddr)) + 1.U
      }
    }.elsewhen(io.update(i).en && io.update(i).dm_hit) {
      when(PredTable(get_addr_idx(io.update(i).vaddr)) === Fill(CounterSize, 0.U)) {
        PredTable(get_addr_idx(io.update(i).vaddr)) := PredTable(get_addr_idx(io.update(i).vaddr))
      }.otherwise {
        PredTable(get_addr_idx(io.update(i).vaddr)) := PredTable(get_addr_idx(io.update(i).vaddr)) - 1.U
      }
    }.otherwise {
      PredTable(get_addr_idx(io.update(i).vaddr)) := PredTable(get_addr_idx(io.update(i).vaddr))
    }
  }

}