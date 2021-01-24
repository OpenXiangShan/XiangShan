package xiangshan.memend

import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.util._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import xiangshan._
import xiangshan.cache.{DCacheLineIO, DCacheWordReq}
import xiangshan.mem.{LoadForwardQueryIO, NewSbuffer}
import xiangshan.testutils._

import scala.util.Random

class SbufferWapper extends XSModule {
  val io = IO(new Bundle() {
    val in = Vec(StorePipelineWidth, Flipped(Decoupled(new DCacheWordReq)))
    val dcache = new DCacheLineIO
    val forward = Vec(LoadPipelineWidth, Flipped(new LoadForwardQueryIO))
    val flush = new Bundle {
      val valid = Input(Bool())
      val empty = Output(Bool())
    } // sbuffer flush
  })
  val sbuffer = Module(new NewSbuffer)
  io <> sbuffer.io

  // fake dcache
  sbuffer.io.dcache.req.ready := true.B
  sbuffer.io.dcache.resp.valid := RegNext(RegNext(RegNext(RegNext(sbuffer.io.dcache.req.valid))))
  sbuffer.io.dcache.resp.bits.id := RegNext(RegNext(RegNext(RegNext(sbuffer.io.dcache.req.bits.id))))
}

class SbufferTest extends AnyFlatSpec
  with ChiselScalatestTester
  with Matchers
  with HasXSParameter
  with ParallelTestExecution
  with HasPartialDecoupledDriver {


  top.Parameters.set(top.Parameters.debugParameters)


//  it should "random req" in {
//    test(new SbufferWapper{AddSinks()}){ c =>
//
//      def store_enq(addr: Seq[UInt], data: Seq[UInt], mask: Seq[UInt]) ={
//        (0 until StorePipelineWidth).map { i =>
//          c.io.in(i).valid.poke(true.B)
//          c.io.in(i).bits.pokePartial(chiselTypeOf(c.io.in(i).bits).Lit(
//            _.mask -> mask(i),
//            _.addr -> addr(i),
//            _.data -> data(i)
//          ))
//        }
//        c.clock.step(1)
//        for (in <- c.io.in){ in.valid.poke(false.B)}
//      }
//
//      def forward_req_and_resp(addr: Seq[UInt], data: Seq[UInt], mask:Seq[UInt]) = {
//        (0 until LoadPipelineWidth).map{ i =>
//          c.io.forward(i).paddr.poke(addr(i))
//          c.io.forward(i).mask.poke(mask(i))
//          if(c.io.in(i).ready.peek() == true.B) {
//            (0 until 8).map { j =>
//              c.io.forward(i).forwardData(j).expect(data(i)(j * 8 + 7, j * 8))
//            }
//          }
//        }
//      }
//
//      val TEST_SIZE = 100
//      for(i <- 0 until TEST_SIZE) {
//        val addr = Seq.fill(StorePipelineWidth)((Random.nextLong() & 0x7ffffffff8L).U)// align to block size
//        val data = Seq.fill(StorePipelineWidth)((Random.nextLong() & 0x7fffffffffffffffL).U)
//        val mask = Seq.fill(StorePipelineWidth)(0xff.U)
//        store_enq(addr, data, mask)
//        forward_req_and_resp(addr, data, mask)
//      }
//    }
//  }
//
//  it should "sequence req" in {
//    test(new SbufferWapper{AddSinks()}){ c =>
//
//      def store_enq(addr: Seq[UInt], data: Seq[UInt], mask: Seq[UInt]) = {
//        (0 until StorePipelineWidth).map { i =>
//          c.io.in(i).valid.poke(true.B)
//          c.io.in(i).bits.pokePartial(chiselTypeOf(c.io.in(i).bits).Lit(
//            _.mask -> mask(i),
//            _.addr -> addr(i),
//            _.data -> data(i)
//          ))
//        }
//        c.clock.step(1)
//        for (in <- c.io.in){ in.valid.poke(false.B)}
//      }
//
//      def forward_req_and_resp(addr: Seq[UInt], data: Seq[UInt], mask:Seq[UInt]) = {
//        (0 until LoadPipelineWidth).map{ i =>
//          c.io.forward(i).paddr.poke(addr(i))
//          c.io.forward(i).mask.poke(mask(i))
//          if(c.io.in(i).ready.peek() == true.B) {
//            (0 until 8).map { j =>
//              c.io.forward(i).forwardData(j).expect(data(i)(j * 8 + 7, j * 8))
//            }
//          }
//        }
//      }
//
//      val TEST_SIZE = 100
//      val start_addr = Random.nextLong() & 0x7ffffffff8L
//      for(i <- 0 until TEST_SIZE) {
//        val addr = Seq(((i<<4) + start_addr).U,((i<<4)+8+start_addr).U)
//        val data = Seq.fill(StorePipelineWidth)((Random.nextLong() & 0x7fffffffffffffffL).U)
//        val mask = Seq.fill(StorePipelineWidth)(0xff.U)
//        store_enq(addr, data, mask)
//        forward_req_and_resp(addr, data, mask)
//      }
//    }
//  }

  it should "sbuffer coherence" in {
    test(new SbufferWapper{AddSinks()}){ c =>
      def store_enq(addr: Seq[UInt], data: Seq[UInt], mask: Seq[UInt]) ={
        (0 until StorePipelineWidth).map { i =>
          c.io.in(i).valid.poke(true.B)
          c.io.in(i).bits.pokePartial(chiselTypeOf(c.io.in(i).bits).Lit(
            _.mask -> mask(i),
            _.addr -> addr(i),
            _.data -> data(i)
          ))
        }
        c.clock.step(1)
        for (in <- c.io.in){ in.valid.poke(false.B)}
      }
      def forward_req_and_resp(addr: Seq[UInt], data: Seq[UInt], mask:Seq[UInt]) = {
        (0 until LoadPipelineWidth).map{ i =>
          c.io.forward(i).paddr.poke(addr(i))
          c.io.forward(i).mask.poke(mask(i))
          if(c.io.in(i).ready.peek() == true.B) {
            (0 until 8).map { j =>
              c.io.forward(i).forwardData(j).expect(data(i)(j * 8 + 7, j * 8))
            }
          }
        }
      }
      val TEST_SIZE = 10
      for(i <- 0 until TEST_SIZE) {
        val addr = Seq.fill(StorePipelineWidth)((Random.nextLong() & 0x7ffffffff8L).U)// align to
        val data = Seq.fill(StorePipelineWidth)((Random.nextLong() & 0x7fffffffffffffffL).U)
        val mask = Seq.fill(StorePipelineWidth)(0xff.U)
        store_enq(addr, data, mask)
        forward_req_and_resp(addr, data, mask)
      }

      c.clock.step(512 + 10)
    }
  }
}
