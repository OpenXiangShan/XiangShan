package xiangshan.frontend

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._
import xiangshan.cache.mmu.TlbModule

// multi-read && single-write
// input is data, output is hot-code(not one-hot)
// Compare CAM, add i use index to obtain data
class IndexableCAMTemplate[T <: Data](
  val gen: T, val set: Int, val readWidth: Int,
  val isIndexable: Boolean = false)
  (implicit p: Parameters) extends TlbModule {
  val io = IO(new Bundle {
    val r = new Bundle {
      val req = Input(Vec(readWidth, gen))
      val resp = Output(Vec(readWidth, Vec(set, Bool())))
    }
    val w = Input(new Bundle {
      val valid = Bool()
      val bits = new Bundle {
        val index = UInt(log2Up(set).W)
        val data = gen
      }
    })
    val rdata = if(isIndexable) Some(Output(gen)) else None
    val ridx = if(isIndexable) Some(Input(UInt(log2Up(set).W))) else None
  })

  val wordType = UInt(gen.getWidth.W)
  val array = Reg(Vec(set, wordType))

  io.r.resp.zipWithIndex.map{ case (a,i) =>
    a := array.map(io.r.req(i).asUInt === _)
  }

  when (io.w.valid) {
    array(io.w.bits.index) := io.w.bits.data.asUInt
  }

  if(isIndexable){
    io.rdata.get := array(io.ridx.get).asTypeOf((gen))
  } else None
}