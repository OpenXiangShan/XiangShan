/***************************************************************************************
  * Copyright (c) 2020-2022 Institute of Computing Technology, Chinese Academy of Sciences
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  *          http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  ***************************************************************************************/

package device

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.experimental.{ExtModule, prefix}
import chisel3.util._
import difftest.common.DifftestMem
import freechips.rocketchip.amba.axi4.{AXI4MasterNode, AXI4Parameters, AXI4SlaveNode}
import freechips.rocketchip.diplomacy.{AddressSet, InModuleBody, LazyModule, LazyModuleImp}
import utility._

class MemoryRequestHelper(requestType: Int)
  extends ExtModule(Map("REQUEST_TYPE" -> requestType))
  with HasExtModuleInline
{
  val clock     = IO(Input(Clock()))
  val reset     = IO(Input(Reset()))
  val io = IO(new Bundle {
    val req = Flipped(ValidIO(new Bundle {
      val addr = UInt(64.W)
      val id   = UInt(32.W)
    }))
    val response = Output(Bool())
  })

  val verilogLines = Seq(
    "import \"DPI-C\" function bit memory_request (",
    "  input longint address,",
    "  input int id,",
    "  input bit isWrite",
    ");",
    "",
    "module MemoryRequestHelper #(",
    "  parameter REQUEST_TYPE",
    ")(",
    "  input             clock,",
    "  input             reset,",
    "  input             io_req_valid,",
    "  input      [63:0] io_req_bits_addr,",
    "  input      [31:0] io_req_bits_id,",
    "  output reg        io_response",
    ");",
    "",
    "always @(negedge clock or posedge reset) begin",
    "  if (reset) begin",
    "    io_response <= 1'b0;",
    "  end",
    "  else if (io_req_valid) begin",
    "    io_response <= memory_request(io_req_bits_addr, io_req_bits_id, REQUEST_TYPE);",
    "  end" +
    "  else begin",
    "    io_response <= 1'b0;",
    "  end",
    "end",
    "",
    "endmodule"
  )
  setInline(s"$desiredName.v", verilogLines.mkString("\n"))
}

class MemoryResponseHelper(requestType: Int)
  extends ExtModule(Map("REQUEST_TYPE" -> requestType))
  with HasExtModuleInline
{
  val clock    = IO(Input(Clock()))
  val reset    = IO(Input(Reset()))
  val enable   = IO(Input(Bool()))
  val response = IO(Output(UInt(64.W)))

  val verilogLines = Seq(
    "import \"DPI-C\" function longint memory_response (",
    "  input bit isWrite",
    ");",
    "",
    "module MemoryResponseHelper #(",
    "  parameter REQUEST_TYPE",
    ")(",
    "  input             clock,",
    "  input             reset,",
    "  input             enable,",
    "  output reg [63:0] response",
    ");",
    "",
    "always @(negedge clock or posedge reset) begin",
    "  if (reset) begin",
    "    response <= 64'b0;",
    "  end",
    "  else if (!reset && enable) begin",
    "    response <= memory_response(REQUEST_TYPE);",
    "  end",
    " else begin",
    "    response <= 64'b0;",
    "  end",
    "end",
    "",
    "endmodule"
  )
  setInline(s"$desiredName.v", verilogLines.mkString("\n"))
}

trait MemoryHelper { this: Module =>
  private def requestType(isWrite: Boolean): Int = if (isWrite) 1 else 0
  private def request(valid: Bool, addr: UInt, id: UInt, isWrite: Boolean): Bool = {
    val helper = Module(new MemoryRequestHelper(requestType(isWrite)))
    helper.clock := clock
    helper.reset := reset
    helper.io.req.valid := valid
    helper.io.req.bits.addr := addr
    helper.io.req.bits.id := id
    helper.io.response
  }
  protected def readRequest(valid: Bool, addr: UInt, id: UInt): Bool =
    request(valid, addr, id, false)
  protected def writeRequest(valid: Bool, addr: UInt, id: UInt): Bool =
    request(valid, addr, id, true)
  private def response(enable: Bool, isWrite: Boolean): (Bool, UInt) = {
    val helper = Module(new MemoryResponseHelper(requestType(isWrite)))
    helper.clock := clock
    helper.reset := reset
    helper.enable := enable
    (helper.response(32), helper.response(31, 0))
  }
  protected def readResponse(enable: Bool): (Bool, UInt) =
    response(enable, false)
  protected def writeResponse(enable: Bool): (Bool, UInt) =
    response(enable, true)
}

class AXI4MemoryImp[T <: Data](outer: AXI4Memory) extends AXI4SlaveModuleImp(outer) with MemoryHelper {
  val ramBaseAddr = outer.address.head.base
  val (ramIndexBits, ramOffsetBits) = (log2Ceil(outer.beatBytes), log2Ceil(outer.memByte))
  def ramIndex(addr: UInt) = ((addr - ramBaseAddr.U)(ramOffsetBits - 1, 0) >> ramIndexBits).asUInt
  val ramHelper = DifftestMem(outer.memByte, outer.beatBytes, 8, singlePort = false)

  // TODO: *NOTICE* The simulated behaviour of DRAM (or other external models) might be inaccurate,
  //       since we are not simulating the actual DRAM chip bits width.
  //       Only the first request / burst is sent to the external model, regardless of the channel
  //       count and AXI4 channel width.
  //       This feature was not changed since the original implementation to keep the performance 
  //       consistent with the legacy AXI4Memory SoC. Which was natural for write transactions, and
  //       patched by 'read_flap*' for read transactions.

  // !! This implementation should be only used for simulation purposes. !! 

  val numOutstanding = 64
  // Note: we are using in.ar.bits.addr.getWidth insead of ramOffsetBits here.
  // Why: the CPU may access out-of-range addresses. Let the RAM helper deal with it.
  val awQueue = Module(new Queue(chiselTypeOf(in.aw.bits), numOutstanding, flow = false))
  val wQueue = Module(new Queue(chiselTypeOf(in.w.bits), numOutstanding * 2, flow = false))
  val bQueue = Module(new Queue(chiselTypeOf(in.b.bits), numOutstanding, flow = false))
  val arQueue = Module(new Queue(chiselTypeOf(in.ar.bits), numOutstanding, flow = false))
  val rQueue = Module(new Queue(chiselTypeOf(in.r.bits), numOutstanding * 2, flow = false))

  val wTrackers = Module(new Queue(chiselTypeOf(in.aw.bits), numOutstanding, flow = true))
  val wTrackersDataID = RegInit(VecInit.fill(numOutstanding)(0.U(log2Up(outer.burstLen + 1).W)))

  val rTrackers = Mem(numOutstanding, chiselTypeOf(in.ar.bits))
  val rTrackersDataID = RegInit(VecInit.fill(numOutstanding)(0.U(log2Up(outer.burstLen + 1).W)))

  val w_count = RegInit(0.U(log2Up(numOutstanding + 1).W))
  val r_count = RegInit(0.U(log2Up(numOutstanding + 1).W))

  // accept a read request and send it to the external model
  in.ar.ready := arQueue.io.enq.ready
  arQueue.io.enq.valid := in.ar.valid
  arQueue.io.enq.bits := in.ar.bits

  when (arQueue.io.enq.fire) {
    rTrackers.write(in.ar.bits.id, in.ar.bits)
    rTrackersDataID(in.ar.bits.id) := 0.U
  }

  arQueue.io.deq.ready := readRequest(arQueue.io.deq.valid, arQueue.io.deq.bits.addr, arQueue.io.deq.bits.id)

  // read data response: resp from DRAMsim3; read data and response to in.r
  val rPipe = Module(new Queue(chiselTypeOf(in.r.bits), 1, pipe = true, flow = false))

  val read_flap = RegInit(false.B)
  val read_flap_tracker = Reg(chiselTypeOf(in.ar.bits))

  val (read_resp_valid, read_resp_id_ext) = readResponse((r_count =/= 0.U) && rPipe.io.enq.ready && !read_flap)
  val read_resp_id = Mux(
    read_flap,
    read_flap_tracker.id, // second and further read burst if necessary, bypassing external model read
    read_resp_id_ext // first read burst
  )
  val read_resp_addr = ramIndex(rTrackers(read_resp_id).addr) + rTrackersDataID(read_resp_id)

  rPipe.io.enq.valid := read_resp_valid || read_flap
  rPipe.io.enq.bits.id := read_resp_id
  rPipe.io.enq.bits.data := DontCare // read data is not available yet
  rPipe.io.enq.bits.resp := AXI4Parameters.RESP_OKAY
  rPipe.io.enq.bits.last := rTrackersDataID(read_resp_id) === rTrackers(read_resp_id).len

  when (rPipe.io.enq.fire) {
    when (!rPipe.io.enq.bits.last) {
      read_flap := true.B
      read_flap_tracker := Mux(read_flap, read_flap_tracker, rTrackers(read_resp_id))
    }.otherwise {
      read_flap := false.B
    }
    rTrackersDataID(read_resp_id) := rTrackersDataID(read_resp_id) + 1.U
  }

  rPipe.io.deq.ready := rQueue.io.enq.ready
  rQueue.io.enq.valid := rPipe.io.deq.valid
  rQueue.io.enq.bits.id := rPipe.io.deq.bits.id
  rQueue.io.enq.bits.data := ramHelper.readAndHold(read_resp_addr, rPipe.io.enq.fire).asUInt
  rQueue.io.enq.bits.resp := rPipe.io.deq.bits.resp
  rQueue.io.enq.bits.last := rPipe.io.deq.bits.last

  rQueue.io.deq.ready := in.r.ready
  in.r.valid := rQueue.io.deq.valid
  in.r.bits := rQueue.io.deq.bits

  // read request counter
  val r_count_inc = arQueue.io.deq.fire
  val r_count_dec = rQueue.io.enq.fire && rQueue.io.enq.bits.last
  when (r_count_inc ^ r_count_dec) {
    r_count := Mux(r_count_inc, r_count + 1.U, r_count - 1.U)
  }

  // accept a write request and send it to the external model
  in.aw.ready := awQueue.io.enq.ready && wTrackers.io.enq.ready && bQueue.io.enq.ready
  awQueue.io.enq.valid := in.aw.valid && wTrackers.io.enq.ready && bQueue.io.enq.ready
  awQueue.io.enq.bits := in.aw.bits

  wTrackers.io.enq.valid := in.aw.valid && bQueue.io.enq.ready
  wTrackers.io.enq.bits := in.aw.bits

  when (wTrackers.io.enq.fire) {
    wTrackersDataID(in.aw.bits.id) := 0.U
  }
  
  awQueue.io.deq.ready := writeRequest(awQueue.io.deq.valid, awQueue.io.deq.bits.addr, awQueue.io.deq.bits.id)

  // accept a write data and write it to the internal memory
  in.w.ready := wQueue.io.enq.ready
  wQueue.io.enq.valid := in.w.valid
  wQueue.io.enq.bits := in.w.bits

  wQueue.io.deq.ready := w_count =/= 0.U

  val write_req_addr = ramIndex(wTrackers.io.deq.bits.addr) + wTrackersDataID(wTrackers.io.deq.bits.id)
  when (wQueue.io.deq.fire) {
    ramHelper.write(
      addr = write_req_addr,
      data = wQueue.io.deq.bits.data.asTypeOf(Vec(outer.beatBytes, UInt(8.W))),
      mask = wQueue.io.deq.bits.strb.asBools
    )
    wTrackersDataID(wTrackers.io.deq.bits.id) := wTrackersDataID(wTrackers.io.deq.bits.id) + 1.U
  }

  assert(!wQueue.io.deq.valid || wTrackers.io.deq.valid, "AXI4Memory: write tracker underflow on W channel")

  // write request counter
  val w_count_inc = awQueue.io.deq.fire
  val w_count_dec = wQueue.io.deq.fire && wQueue.io.deq.bits.last
  when (w_count_inc ^ w_count_dec) {
    w_count := Mux(w_count_inc, w_count + 1.U, w_count - 1.U)
  }

  // generate write response
  bQueue.io.deq.ready := in.b.ready
  in.b.valid := bQueue.io.deq.valid
  in.b.bits := bQueue.io.deq.bits

  bQueue.io.enq.valid := wQueue.io.deq.fire && wQueue.io.deq.bits.last
  bQueue.io.enq.bits.id := wTrackers.io.deq.bits.id
  bQueue.io.enq.bits.resp := AXI4Parameters.RESP_OKAY

  wTrackers.io.deq.ready := bQueue.io.enq.fire

  assert(!bQueue.io.enq.valid || wTrackers.io.deq.valid, "AXI4Memory: write tracker underflow on B channel")
  assert(!bQueue.io.enq.valid || bQueue.io.enq.ready, "AXI4Memory: B channel overflow")
}

class AXI4Memory
(
  val address: Seq[AddressSet],
  val memByte: Long,
  val useBlackBox: Boolean = false,
  val executable: Boolean = true,
  val beatBytes: Int,
  val burstLen: Int,
)(implicit p: Parameters)
  extends AXI4SlaveModule(address, executable, beatBytes, burstLen)
{
  override lazy val module = new AXI4MemoryImp(this)
}

class AXI4MemoryWrapper (
  slave: AXI4SlaveNode,
  memByte: Long,
  useBlackBox: Boolean = false
)(implicit p: Parameters) extends AXI4MemorySlave(slave, memByte, useBlackBox) {
  val ram = LazyModule(new AXI4Memory(
    slaveParam.address,
    memByte,
    useBlackBox,
    slaveParam.executable,
    portParam.beatBytes,
    burstLen
  ))
  ram.node := master
}

abstract class AXI4MemorySlave (
  slave: AXI4SlaveNode,
  memByte: Long,
  useBlackBox: Boolean = false
)(implicit p: Parameters) extends LazyModule {
  val master = AXI4MasterNode(List(slave.in.head._2.master))

  val portParam = slave.portParams.head
  val slaveParam = portParam.slaves.head
  val burstLen = portParam.maxTransfer / portParam.beatBytes

  val io_axi4 = InModuleBody{ master.makeIOs() }

  lazy val module = new LazyModuleImp(this) { }
}

object AXI4MemorySlave {
  def apply(
    slave: AXI4SlaveNode,
    memByte: Long,
    useBlackBox: Boolean = false,
    dynamicLatency: Boolean = false
  )(implicit p: Parameters): AXI4MemorySlave = {
    val memory = if (dynamicLatency) {
      LazyModule(new AXI4MemoryWrapper(slave, memByte, useBlackBox))
    } else {
      LazyModule(new AXI4RAMWrapper(slave, memByte, useBlackBox))
    }
    memory
  }
}
