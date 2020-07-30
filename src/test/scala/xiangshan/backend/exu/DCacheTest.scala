package xiangshan.backend.exu

import org.scalatest._
import scala.collection.mutable.{Map, Queue}

import chisel3._
import chisel3.util.experimental.BoringUtils
import chisel3.experimental.BundleLiterals._
import chiseltest._

import xiangshan.XSModule
import xiangshan.utils.XSLogLevel
import xiangshan.mem.{LSUDMemIO, MemoryOpConstants}
import xiangshan.mem.cache.DCache
import bus.tilelink.FakeTLLLC
import device.AXI4RAM

class DCacheDut extends XSModule {
  val io = IO(new Bundle() {
    val in = Flipped(new LSUDMemIO)
  })

  val dcache = Module(new DCache)
  val mem = Module(new AXI4RAM(memByte = 128 * 1024 * 1024, useBlackBox = false))
  val tlToAXI = Module(new FakeTLLLC(l1BusParams))

  dcache.io.lsu <> io.in
  dcache.io.bus <> tlToAXI.io.in
  tlToAXI.io.out <> mem.in


  // log control
  val log_begin, log_end, log_level = Wire(UInt(64.W))
  log_begin := 0.U
  log_end := 0xfffffff.U
  log_level := XSLogLevel.DEBUG.id.U

  BoringUtils.addSource(log_begin, "DISPLAY_LOG_START")
  BoringUtils.addSource(log_end, "DISPLAY_LOG_END")
  BoringUtils.addSource(log_level, "DISPLAY_LOG_LEVEL")
}


case class Req(
  cmd: UInt,
  addr: Long,
  data: Long,
  mask: Long,
  meta: Long
) {
  override def toString() : String = {
    val cmd_name = MemoryOpConstants.getMemoryOpName(cmd)
    return f"cmd: $cmd_name%s addr: $addr%x data: $data%x mask: $mask%x meta: $meta%d"
  }
}

case class Resp(
  data: Long,
  meta: Long
) {
  override def toString() : String = {
    return f"data: $data%x meta: $meta%d"
  }
}

class DCacheTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "DCache"

  it should "do load store correctly" in {
    test(new DCacheDut) { c =>
      val CMD_READ = MemoryOpConstants.M_XRD
      val CMD_WRITE = MemoryOpConstants.M_XWR
      val FULL_MASK = 0xff

      val BASE_ADDR = 0x80000000L
      val MEM_SIZE = 128 * 1024 * 1024

      // for now, we only support load/store of 64bit integers
      val INTEGER_SIZE = 8
      val num_integers = MEM_SIZE / INTEGER_SIZE

      // data structures
      // our golden version cache
      val mem = new Array[Long](num_integers)
      var num_retired_reqs = 0

      // at each clock, we try to issue the request bundle at the head
      val issue_queue = Queue[Array[Req]]()
      // map that store all requests, map req id to req
      // whenever you want to replay a req, you can get the req with its id
      var all_requests:Map[Long,Req] = Map()

      // 之前的请求是否在等待req ready？
      var req_waiting:Boolean = false
      var global_clock:Long = 0


      def init_test = {
        req_waiting = false
        num_retired_reqs = 0
        issue_queue.clear
        all_requests.clear
      }

      // 向某个特定的channel上发送req
      def send_req_channel(req: Req, channel: Int) = {
        val r = c.io.in.req.bits(channel)
        r.bits.cmd.poke(req.cmd)
        r.bits.addr.poke(req.addr.U)
        r.bits.data.poke(req.data.U)
        r.bits.mask.poke(req.mask.U)
        r.bits.meta.poke(req.meta.U)
        r.valid.poke(true.B)
        println(s"clock: $global_clock channel: $channel req: $req")
      }

      // send a bundle of reqs in the same cycle
      def send_req_bundle(reqs: Array[Req]) = {
        println(s"send_req_bundle")
        for (i <- 0 to reqs.length - 1) {
          send_req_channel(reqs(i), i)
        }
        c.io.in.req.valid.poke(true.B)
      }

      def send_req: Unit = {
        // println(s"send_req")
        // no requests waiting for ready
        // reset valid signal
        if (!req_waiting) {
          c.io.in.req.valid.poke(false.B)
        }

        // no more requests to issue
        if (issue_queue.isEmpty)
          return

        // there are no requests waiting for handshake
        // we may send a new request during this clock
        if (!req_waiting) {
          req_waiting = true
          send_req_bundle(issue_queue.front)
        }

        // reqs can be fired
        if (c.io.in.req.ready.peek().litToBoolean) {
          println(s"req fired")
          req_waiting = false
          issue_queue.dequeue()
        }
      }

      def handle_resp = {
        for (i <- 0 to 1) {
          val resp = c.io.in.resp(i)
          if (resp.valid.peek().litToBoolean) {

            val data = resp.bits.data.peek().litValue.longValue
            val meta = resp.bits.meta.peek().litValue.longValue
            val nack = resp.bits.nack.peek().litToBoolean
            println(f"clock: $global_clock%d channel: $i%d nack: $nack%b data: $data%x meta: $meta%x")

            val original_req = all_requests(meta)
            // needs to be replayed
            if (nack) {
              issue_queue.enqueue(Array[Req](original_req))
            } else {
              num_retired_reqs += 1
              if (original_req.cmd.litValue == CMD_READ.litValue) {
                resp.bits.data.expect(mem(original_req.addr.toInt).U)
              }
            }
          }
        }
      }

      val r = scala.util.Random

      // ----------------------------------------
      // store test
      println(s"store test")

      init_test

      // first, initialize every memory cell with random numbers
      for (i <- 0 to num_integers - 1) {
        println(s"store $i")
        // only deal with unsigned numberss
        // we can not cast negative numbers to UInts
        val randomNumber = r.nextLong.abs
        val req = Req(CMD_WRITE, BASE_ADDR + i * INTEGER_SIZE, randomNumber, FULL_MASK, i)
        issue_queue.enqueue(Array[Req](req))
        all_requests += (i.toLong -> req)
        mem(i) = randomNumber
      }

      while (num_retired_reqs < num_integers) {
        send_req
        handle_resp
        c.clock.step()
        global_clock += 1
      }

      // read out every integer
      // ----------------------------------------
      // read test
      println(s"load test")//Hello,James

      init_test

      // first, initialize every memory cell with random numbers
      for (i <- 0 to num_integers - 1) {
        val req = Req(CMD_READ, BASE_ADDR + i * INTEGER_SIZE, 0, FULL_MASK, i)
        issue_queue.enqueue(Array[Req](req))
        all_requests += (i.toLong -> req)
      }

      while (num_retired_reqs < num_integers) {
        send_req
        handle_resp
        c.clock.step()
        global_clock += 1
      }
    }
  }
}
