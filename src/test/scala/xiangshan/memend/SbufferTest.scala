package xiangshan.memend

import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.util._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import xiangshan._
import xiangshan.mem.{LoadForwardQueryIO, NewSbuffer}
import xiangshan.testutils._

import scala.util.Random

class SbufferWapper extends XSModule {
  val sbuffer = Module(new NewSbuffer)
  val io = IO(sbuffer.io.cloneType)
  io <> sbuffer.io
  AddSinks()
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

  def make_store_req(addr: UInt, data: UInt, mask: UInt, portIdx: Int)
                    (implicit c: SbufferWapper) = {
    val port = c.io.in(portIdx)
    port.enqueuePartial(chiselTypeOf(port.bits).Lit(
      _.addr -> addr,
      _.data -> data,
      _.mask -> mask,
    ))
  }

  def make_forward_req
  (addr: UInt, mask: UInt, ref_data: UInt, portIdx: Int)
  (implicit c: SbufferWapper) = {
    val port = c.io.forward(portIdx)
    port.paddr.poke(addr)
    port.mask.poke(mask)
    c.clock.step(1)
    for(i <- 0 until 8){
      port.forwardData(i).expect(ref_data(i * 8 + 7, i * 8))
    }
  }


  it should "allow multi-inflight dcache requests" in {
    test(new SbufferWapper){ c =>
      implicit val circuit = c
      c.io.in.foreach(p => p.initSource().setSourceClock(c.clock))
      val TEST_SIZE = 1000
      var addr = 0
      for(_ <- 0 until TEST_SIZE){
        val data = (Random.nextLong() & 0x7fffffffffffffffL).U
        val mask = 0xff.U
        make_store_req(addr.U, data, mask, 0)
        addr += 512
      }
    }
  }

  it should "forward older store's data to younger load" in {
    test(new SbufferWapper){ c =>
      implicit val circuit = c
      c.io.in.foreach(p => p.initSource().setSourceClock(c.clock))
      val TEST_SIZE = 10
      def testPort(i : Int) = {
        for(_ <- 0 until TEST_SIZE){
          val addr = (Random.nextLong() & 0x7ffffffff8L).U
          val data = (Random.nextLong() & 0x7fffffffffffffffL).U
          val mask = 0xff.U
          make_store_req(addr, data, mask, i)
          make_forward_req(addr, mask, data, i)
        }
      }
      fork(
        testPort(0)
      ).fork(
        testPort(1)
      ).join()
    }
  }
}
