//package xiangshan.backend.exu
//
//import org.scalatest._
//import scala.collection.mutable.{Map, Queue}
//import scala.collection.mutable.ArrayBuffer
//
//import chisel3._
//import chisel3.experimental.BundleLiterals._
//import chiseltest._
//
//import xiangshan.XSModule
//import xiangshan.cache.{DCacheToLsuIO, MemoryOpConstants, DCache}
//import bus.tilelink.FakeTLLLC
//import device.AXI4RAM
//import utils.GTimer
//
//class DCacheDut extends XSModule {
//  val io = IO(new Bundle() {
//    val in = Flipped(new DCacheToLsuIO)
//  })
//
//  val dcache = Module(new DCache)
//  val mem = Module(new AXI4RAM(memByte = 128 * 1024 * 1024, useBlackBox = false))
//  val tlToAXI = Module(new FakeTLLLC(l1BusParams))
//
//  dcache.io.lsu <> io.in
//  dcache.io.bus <> tlToAXI.io.in
//  tlToAXI.io.out <> mem.in
//
//
//  // log control
//  val logEnable = WireInit(true.B)
//  val logTimestamp = WireInit(0.U(64.W))
//  logTimestamp := GTimer()
//  ExcitingUtils.addSource(logEnable, "DISPLAY_LOG_ENABLE")
//  ExcitingUtils.addSource(logTimestamp, "logTimestamp")
//}
//
//
//case class Req(
//  cmd: UInt,
//  addr: Long,
//  data: Long,
//  mask: Long,
//  result: Long, // expected result
//  id: Int
//) {
//  override def toString() : String = {
//    val cmd_name = MemoryOpConstants.getMemoryOpName(cmd)
//    return f"cmd: $cmd_name%s addr: $addr%x data: $data%x mask: $mask%x id: $id%d"
//  }
//}
//
//case class InstructionQueueEntry(
//  issued: Boolean,
//  retired: Boolean,
//  req_id: Int
//) {
//  override def toString() : String = {
//    return f"issued: $issued%b retired: $retired%b req_id: $req_id%d"
//  }
//}
//
//case class IssueQueueEntry(
//  valid: Boolean,
//  req_id: Int,
//  revision_id: Int
//) {
//  override def toString() : String = {
//    return f"valid: $valid%b req_id: $req_id%d revision_id: $revision_id%d"
//  }
//}
//
//case class LsroqEntry(
//  valid: Boolean, // this entry is valid
//  arrived: Boolean, // req has passed load/store pipe and reached this entry
//  miss: Boolean, // there is a dcache miss(only load req use this)
//  finished: Boolean, // this req can be retired
//  req_id: Int,
//  revision_id: Int
//) {
//  override def toString() : String = {
//    return f"valid: $valid%b arrived: $arrived%b miss: $miss%b finished: $finished%b req_id: $req_id%d revision_id: $revision_id%d"
//  }
//}
//
//case class Resp(
//  data: Long,
//  meta: Long
//) {
//  override def toString() : String = {
//    return f"data: $data%x meta: $meta%d"
//  }
//}
//
//class DCacheTest extends FlatSpec with ChiselScalatestTester with Matchers {
//  behavior of "DCache"
//
//  /*
//  it should "do load store correctly" in {
//    test(new DCacheDut) { c =>
//      // ----------------------------------------
//      // basic configurations: things that you could touch
//      val MEM_SIZE = 128 * 1024 * 1024
//      // for now, we only support load/store of 64bit integers
//      val INTEGER_SIZE = 8
//      val ISSUE_QUEUE_SIZE = 16
//      val LSROQ_SIZE = 512
//      val NumLoadPipe = 2
//      val NumStorePipe = 2
//
//      // ----------------------------------------
//      // useful request parameter values
//      val CMD_READ = MemoryOpConstants.M_XRD
//      val CMD_WRITE = MemoryOpConstants.M_XWR
//      val FULL_MASK = 0xff
//      val BASE_ADDR = 0x0L
//
//      val r = scala.util.Random
//
//      // ----------------------------------------
//      // memory backend
//      // you should not touch this
//      val num_integers = MEM_SIZE / INTEGER_SIZE
//
//      // data structures
//      // our golden version cache
//      val mem = new Array[Long](num_integers)
//
//
//      // ----------------------------------------
//      // utility variables
//
//      // next request id to allocate
//      val next_req_id: Int = 0
//      def allocate_req_id(): Int = {
//        val old_id = next_req_id
//        // detect possible wrap around
//        assert(old_id >= 0)
//        next_req_id += 1
//        old_id
//      }
//
//      // whenever a flush is triggered
//      // flush_revision_id is incremented by 1
//      // any req that carries an revision id
//      // that's smaller flush_revision_id is considered as flushed out
//      val flush_revision_id: Int = 0
//      def allocate_revision_id(): Int = {
//        val old_id = flush_revision_id
//        // detect possible wrap around
//        assert(old_id >= 0)
//        flush_revision_id += 1
//        old_id
//      }
//      def is_req_flushed(id: Int) = id < flush_revision_id
//
//      // map that store all requests, map req id to req
//      // whenever you want to replay a req, you can get the req with its id
//      var all_requests:Map[Int,Req] = Map()
//
//      var global_clock:Long = 0
//
//      def init_reqs(): Unit = {
//        global_clock = 0
//        next_req_id = 0
//        flush_revision_id = 0
//        all_requests.clear()
//      }
//
//      // ----------------------------------------
//      // instruction queue
//
//      // next instruction to send to issue queue
//      val instruction_queue_dispatch_head = 0
//
//      // next instruction to retire
//      val instruction_queue_retire_head = 0
//
//      // do not use val or var, use def
//      // since next_req_id may change as user creates new request
//      def instruction_tail = next_req_id
//
//      val instruction_queue = new ArrayBuffer[InstructionQueueEntry]()
//
//      def init_instruction_queue(): Unit = {
//        instruction_queue_dispatch_head = 0
//        instruction_queue_retire_head = 0
//        instruction_queue.clear()
//      }
//
//      def instruction_queue_tick() = {
//        // try diapatch instructions to issue queue
//        instruction_queue_dispatch()
//      }
//
//      def instruction_queue_dispatch(): Unit = {
//        val instruction_to_dispatch = min(issue_queue_available_entries(), lsroq_available_entries())
//        for (i <- 0 until instruction_to_dispatch) {
//          val idx = instruction_queue_dispatch_head
//          issue_queue_enqueue(instruction_queue(idx).req_id, flush_revision_id)
//          lsroq_enqueue(instruction_queue(idx).req_id, flush_revision_id)
//          instruction_queue(idx).issued = true
//          instruction_queue_dispatch_head += 1
//        }
//      }
//
//
//      // ----------------------------------------
//      // issue queue
//      val load_pipe_req_waiting = new Array[Boolean](NumLoadPipe)
//      val load_pipe_req_idx = new Array[Int](NumLoadPipe)
//      val issue_queue = new Array[IssueQueueEntry](ISSUE_QUEUE_SIZE)
//
//      def issue_queue_tick(): Unit = {
//        for (i <- 0 until NumLoadPipe) {
//          issue_dcache_load_req(i)
//        }
//        for (i <- 0 until NumStorePipe) {
//          issue_dcache_store_req(i)
//        }
//      }
//
//      def init_issue_queue(): Unit = {
//        for (i <- 0 until NumLoadPipe) {
//          load_pipe_req_waiting(i) = false
//        }
//
//        for (i <- 0 until ISSUE_QUEUE_SIZE) {
//          issue_queue(i).valid = false
//        }
//      }
//
//      def issue_queue_available_entries(): Int = {
//        val num = ISSUE_QUEUE_SIZE
//        for (0 until ISSUE_QUEUE_SIZE) {
//          if (issue_queue(i).valid)
//            num -= 1
//        }
//        num
//      }
//
//      def issue_queue_enqueue(req_id: Int, revision_id: Int): Unit = {
//        // find an available slot to insert this entry
//        val no_available_slot = true
//        for (0 until ISSUE_QUEUE_SIZE) {
//          if (!issue_queue(i).valid) {
//            issue_queue(i).req_id = req_id
//            issue_queue(i).revision_id = revision_id
//            issue_queue(i).valid = true
//            no_available_slot = false
//          }
//        }
//        assert(!no_available_slot)
//      }
//
//      // select logic
//      def issue_queue_select_req(val cmd): Int = {
//        val load_reqs = new ArrayBuffer[Int]()
//        for (i <- 0 until ISSUE_QUEUE_SIZE) {
//          val is_valid = issue_queue(i).valid
//          val is_read = all_requests(issue_queue(i).req_id).cmd == cmd
//          if (is_valid&& is_read) {
//            load_reqs += i
//          }
//        }
//
//        // no load req in issue_queue
//        if (load_reqs.length == 0)
//          return -1
//        else
//          return load_reqs(r.nextInt.abs() % load_reqs.length)
//      }
//
//      def issue_queue_select_load_req(): Int = {
//        return issue_queue_select_req(CMD_READ)
//      }
//
//      def issue_queue_select_store_req(): Int = {
//        return issue_queue_select_req(CMD_WRITE)
//      }
//
//      def issue_dcache_load_req(channel: Int) = {
//        println(s"issue_dcache_load_req")
//
//        val load_req = c.io.load(channel).req
//
//        // has last cycle's req been fired?
//        if (load_pipe_req_waiting(channel) && load_req.ready.peek().litToBoolean) {
//          println(s"last cycle req fired")
//          // clear this req from issue queue
//          issue_queue(load_pipe_req_idx(channel)).valid = false
//          load_pipe_req_waiting(channel) = false
//          // no requests waiting on line
//          // reset valid signal
//          load_req.valid.poke(false.B)
//        }
//
//        // load pipe busy, can not send req in this cycle
//        if (load_pipe_req_waiting(channel)) {
//          return
//        }
//
//        // send a new request during this clock
//
//        // select a load req to issue
//        val load_req_idx = issue_queue_select_load_req()
//        // no more load requests to issue
//        if (load_req_idx == -1) {
//          return
//        }
//
//        load_pipe_req_waiting(channel) = true
//        load_pipe_req_idx(channel) = load_req_idx
//
//        val entry = issue_queue(load_pipe_req_idx(channel)
//          var id: Long = entry.revision_id
//          id = id << 32 | entry.req_id
//          val req = all_requests(entry.req_id)
//          load_req.bits.cmd.poke(req.cmd.U)
//          load_req.bits.data.poke(req.data.U)
//          load_req.bits.mask.poke(req.mask.U)
//          load_req.bits.meta.id.poke(id.U)
//          load_req.bits.meta.replay.poke(false.B)
//          load_req.valid.poke(true.B)
//
//          println(s"clock: $global_clock channel: $channel req: $req")
//      }
//
//      def issue_dcache_store_req(channel: Int) = {
//        println(s"issue_dcache_store_req")
//        // send a new request during this clock
//        // select a load req to issue
//        val store_req_idx = issue_queue_select_store_req()
//        // no more store requests to issue
//        if (store_req_idx == -1) {
//          return
//        }
//
//        val entry = issue_queue(store_req_idx)
//        // clear this req from issue queue
//        issue_queue(store_req_idx).valid = false
//        lsroq_store_arrive(entry.req_id, entry.revision_id)
//      }
//
//      // ----------------------------------------
//      // lsroq
//      val lsroq = new Array[LsroqEntry](LSROQ_SIZE)
//      val lsroq_head = 0
//      val lsroq_tail = 0
//
//      def lsroq_inc(val a: Int) = (a + 1) % LSROQ_SIZE
//      def lsroq_dec(val a: Int) = (a - 1) % LSROQ_SIZE
//      def lsroq_inc(val a: Int, val b: Int) = (a + b) % LSROQ_SIZE
//      def lsroq_dec(val a: Int, val b: Int) = (a - b) % LSROQ_SIZE
//      def lsroq_empty() = lsroq_head == lsroq_tail
//      def lsroq_full()  = lsroq_inc(lsroq_tail) === lsroq_head
//
//      def lsroq_used_entries() = (lsroq_tail - lsroq_head) % LSROQ_SIZE
//      def lsroq_available_entries() = LSROQ_SIZE - 1 - lsroq_used_entries
//
//      def init_lsroq(): Unit = {
//        lsroq.clear()
//      }
//
//      def lsroq_enqueue(req_id: Int, revision_id: Int): Unit = {
//        // find an available slot to insert this entry
//        val no_available_slot = true
//        for (0 until LSROQ_SIZE) {
//          if (!lsroq(i).valid) {
//            issue_queue(i).valid = true
//            issue_queue(i).arrived = false
//            issue_queue(i).miss = false
//            issue_queue(i).finished = false
//            issue_queue(i).req_id = req_id
//            issue_queue(i).revision_id = revision_id
//            no_available_slot = false
//          }
//        }
//        assert(!no_available_slot)
//      }
//
//      // a store has finished
//      // finish does not really mean it finishes,
//      // it just means it has arrived at lsroq
//      def lsroq_store_arrive(val req_id: Int, val revision_id: Int): Unit = {
//        // do not deal with outdated request
//        if (is_req_flushed(revision_id))
//          return
//
//        // find our entry
//        val not_found = true
//        var idx = lsroq_head
//        while (idx != lsroq_tail) {
//          val entry = lsroq(idx)
//          assert(entry.valid)
//          if (entry.req_id == req_id && entry.revision_id == revision_id) {
//            lsroq(idx).arrived = true
//            // store requests can be sent to store buffer when they reach the lsroq head
//            lsroq(idx).finished = true
//            not_found = false
//            break
//          }
//          idx = lsroq_inc(idx)
//        }
//        assert(!not_found)
//
//        // check for load, store order violations
//        while (idx != lsroq_tail) {
//          val entry = lsroq(idx)
//          assert(entry.valid)
//          if (entry.arrived) {
//            val my_req = all_requests(req_id)
//            val other_req = all_requests(entry.req_id)
//            if (other_req.valid entry.req_id == req_id && entry.revision_id == revision_id) {
//              lsroq(idx).finished = true
//              not_found = false
//            }
//          }
//          idx = lsroq_inc(idx)
//        }
//      }
//
//      // ----------------------------------------
//      // store buffer
//      def handle_resp = {
//        for (i <- 0 to 1) {
//          val resp = c.io.in.resp(i)
//          if (resp.valid.peek().litToBoolean) {
//
//            val data = resp.bits.data.peek().litValue.longValue
//            val meta = resp.bits.meta.peek().litValue.longValue
//            val nack = resp.bits.nack.peek().litToBoolean
//            println(f"clock: $global_clock%d channel: $i%d nack: $nack%b data: $data%x meta: $meta%x")
//
//            val original_req = all_requests(meta)
//            // needs to be replayed
//            if (nack) {
//              issue_queue.enqueue(Array[Req](original_req))
//            } else {
//              num_retired_reqs += 1
//              if (original_req.cmd.litValue == CMD_READ.litValue) {
//                resp.bits.data.expect(mem(original_req.addr.toInt / INTEGER_SIZE).U)
//              }
//            }
//          }
//        }
//      }
//
//      // ----------------------------------------
//      // memory backend interfaces
//      def init_test = {
//        init_reqs()
//        init_instruction_queue()
//        init_issue_queue()
//        init_lsroq()
//      }
//
//      def evaluate(cycles: Int) = {
//        instruction_queue_tick()
//        issue_queue_tick()
//        lsroq_tick()
//        store_buffer_tick()
//
//        c.clock.step()
//        global_clock += 1
//      }
//
//      def is_finish = instruction_queue_retire_head >= next_req_id
//
//      def create_write_req(addr: Long, data: Long, mask: Long) = {
//        val id = allocate_req_id()
//        val req = Req(CMD_WRITE, addr, data, mask, id)
//        instruction_queue += req
//        all_requests += (id -> RequestMeta(req, 0, false))
//        mem(addr) = data
//      }
//
//      def create_read_req(addr: Long, mask: Long) = {
//        val id = allocate_req_id()
//        val req = Req(CMD_READ, addr, 0, mask, id)
//        instruction_queue += req
//        all_requests += (id -> RequestMeta(req, mem(addr), false))
//      }
//
//      // ----------------------------------------
//      // Now, generate requests and let the memory backend process them
//
//      // ----------------------------------------
//      // scan test
//      // write every memory cell and then read out every memory cell
//      def scan_test = {
//        println(s"scan test")
//
//        init_test
//
//        // first, initialize every memory cell with random numbers
//        for (i <- 0 to num_integers - 1) {
//          println(s"store $i")
//          // only deal with unsigned numberss
//          // we can not cast negative numbers to UInts
//          val randomNumber = r.nextLong.abs
//          create_write_req(BASE_ADDR + i * INTEGER_SIZE, randomNumber, FULL_MASK)
//        }
//
//        // read them out
//        for (i <- 0 to num_integers - 1) {
//          create_read_req(BASE_ADDR + i * INTEGER_SIZE, FULL_MASK)
//        }
//
//        while (!is_finish()) {
//          evaluate()
//        }
//      }
//
//      // random read/write test
//    }
//  }
//  */
//}
