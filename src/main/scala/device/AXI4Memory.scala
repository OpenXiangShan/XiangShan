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
  val cppExtModule =
    """
      |void MemoryRequestHelper (
      |  int       REQUEST_TYPE,
      |  uint8_t   reset,
      |  uint8_t   io_req_valid,
      |  uint64_t  io_req_bits_addr,
      |  uint32_t  io_req_bits_id,
      |  uint8_t&  io_response
      |) {
      |  if (reset) io_response = 0;
      |  else if (io_req_valid) io_response = memory_request(io_req_bits_addr, io_req_bits_id, REQUEST_TYPE);
      |  else io_response = 0;
      |}
      |""".stripMargin
  difftest.DifftestModule.createCppExtModule("MemoryRequestHelper", cppExtModule, Some("\"ram.h\""))


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
    "  output            io_response",
    ");",
    "",
    " assign io_response = (reset ? 1'b0 :",
    "               (io_req_valid ? memory_request(io_req_bits_addr, io_req_bits_id, REQUEST_TYPE) : 1'b0));",
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

  val cppExtModule =
    """
      |void MemoryResponseHelper (
      |  int       REQUEST_TYPE,
      |  uint8_t   reset,
      |  uint8_t   enable,
      |  uint64_t& response
      |) {
      |  if (reset) response = 0;
      |  else if (!reset && enable) response = memory_response(REQUEST_TYPE);
      |  else response = 0;
      |}
      |""".stripMargin
  difftest.DifftestModule.createCppExtModule("MemoryResponseHelper", cppExtModule, Some("\"ram.h\""))


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
    "  output [63:0]     response",
    ");",
    "",
    "assign response = reset ? 64'b0 :",
    "((!reset && enable) ? memory_response(REQUEST_TYPE) : 64'b0);",
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
    val responseReg = RegNext(helper.io.response, false.B)
    responseReg
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
    val response32 = RegNext(helper.response(32), false.B)
    val response31_0 = RegNext(helper.response(31, 0), 0.U(32.W))
    (response32, response31_0)
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

  val numOutstanding = 1 << in.ar.bits.id.getWidth
  // Note: we are using in.ar.bits.addr.getWidth insead of ramOffsetBits here.
  // Why: the CPU may access out-of-range addresses. Let the RAM helper deal with it.
  val addressMem = Mem(numOutstanding, UInt((in.ar.bits.addr.getWidth - ramIndexBits).W))
  val arlenMem = Mem(numOutstanding, UInt(in.ar.bits.len.getWidth.W))

  // accept a read request and send it to the external model
  val pending_read_req_valid = RegInit(false.B)
  val pending_read_req_bits  = RegEnable(in.ar.bits, in.ar.fire)
  val pending_read_req_ready = Wire(Bool())
  val pending_read_need_req = pending_read_req_valid && !pending_read_req_ready
  val read_req_valid = pending_read_need_req || in.ar.valid
  val read_req_bits  = Mux(pending_read_need_req, pending_read_req_bits, in.ar.bits)
  pending_read_req_ready := readRequest(read_req_valid, read_req_bits.addr, read_req_bits.id)

  when (in.ar.fire) {
    pending_read_req_valid := true.B
    addressMem.write(read_req_bits.id, ramIndex(read_req_bits.addr))
    arlenMem.write(read_req_bits.id, read_req_bits.len)
  }.elsewhen (pending_read_req_ready) {
    pending_read_req_valid := false.B
  }
  in.ar.ready := !pending_read_req_valid || pending_read_req_ready

  // accept a write request (including address and data) and send it to the external model
  val pending_write_req_valid = RegInit(VecInit.fill(2)(false.B))
  val pending_write_req_bits  = RegEnable(in.aw.bits, in.aw.fire)
  val pending_write_req_data  = RegEnable(in.w.bits, in.w.fire)
  val pending_write_req_ready = Wire(Bool())
  val pending_write_need_req = pending_write_req_valid.last && !pending_write_req_ready
  val aw_and_w_last_arrive_at_same_time = in.aw.fire && in.w.fire && in.w.bits.last
  val w_last_arrive_before_aw = in.aw.fire && pending_write_need_req
  val aw_arrive_before_w_last = pending_write_req_valid.head && in.w.fire && in.w.bits.last
  val aw_arrive_before_w = pending_write_req_valid.head && pending_write_need_req
  val write_req_enq_pending = aw_arrive_before_w || aw_arrive_before_w_last
  val write_req_enq_no_pending = aw_and_w_last_arrive_at_same_time || w_last_arrive_before_aw
  val write_req_valid = write_req_enq_pending || RegNext(write_req_enq_no_pending, false.B)
  val write_req_enq_addr = Mux(write_req_enq_pending, pending_write_req_bits.addr, in.aw.bits.addr)
  val write_req_enq_id = Mux(write_req_enq_pending, pending_write_req_bits.id, in.aw.bits.id)
  pending_write_req_ready := writeRequest(write_req_valid, write_req_enq_addr, write_req_enq_id)

  when (in.aw.fire && !write_req_enq_no_pending) {
    pending_write_req_valid.head := true.B
  }.elsewhen (pending_write_req_ready) {
    pending_write_req_valid.head := false.B
  }
  val write_req_last = in.w.fire && in.w.bits.last
  when (write_req_last) {
    pending_write_req_valid.last := true.B
  }.elsewhen (pending_write_req_ready) {
    pending_write_req_valid.last := false.B
  }
  in.aw.ready := !pending_write_req_valid.head || pending_write_req_ready
  in.w.ready := in.aw.ready || !pending_write_req_valid.last

  // ram is written when write data fire
  val wdata_cnt = Counter(outer.burstLen)
  val write_req_addr = Mux(in.aw.fire, in.aw.bits.addr, pending_write_req_bits.addr)
  val write_req_index = ramIndex(write_req_addr) + wdata_cnt.value
  when (in.w.fire) {
    ramHelper.write(
      addr = write_req_index,
      data = in.w.bits.data.asTypeOf(Vec(outer.beatBytes, UInt(8.W))),
      mask = in.w.bits.strb.asBools
    )
  }
  when (write_req_last) {
    wdata_cnt.reset()
  }.elsewhen (in.w.fire) {
    wdata_cnt.inc()
  }

  // read data response: resp from DRAMsim3; read data and response to in.r
  // This is the output of the last pipeline before in.r. This is not the pipeline registers.
  val r_resp = Wire(Decoupled(chiselTypeOf(in.r.bits)))

  val pending_read_resp_valid = RegInit(false.B)
  val pending_read_resp_id = Reg(UInt(r_resp.bits.id.getWidth.W))
  val has_read_resp = Wire(Bool())
  val read_resp_last = r_resp.fire && r_resp.bits.last
  val read_request_cnt = RegInit(0.U(8.W))
  val read_have_req_cnt = read_request_cnt =/= 0.U
  val (read_resp_valid, read_resp_id) = readResponse((!has_read_resp || read_resp_last) && read_have_req_cnt)
  has_read_resp := (read_resp_valid && !read_resp_last) || pending_read_resp_valid
  val rdata_cnt = Counter(outer.burstLen)
  val read_resp_addr = addressMem(r_resp.bits.id) + rdata_cnt.value
  val read_resp_len = arlenMem(r_resp.bits.id)
  r_resp.valid := read_resp_valid || pending_read_resp_valid
  r_resp.bits.id := Mux(pending_read_resp_valid, pending_read_resp_id, read_resp_id)
  // We cannot get the read data this cycle because the RAM helper has one-cycle latency.
  r_resp.bits.data := DontCare
  r_resp.bits.resp := AXI4Parameters.RESP_OKAY
  r_resp.bits.last := (rdata_cnt.value === read_resp_len)

  // The return values of DPI-C are used to determine whether a request has been made or completed
  // pending_read_req_ready ---> readRequest()
  // read_resp_valid <--- readResponse()
  when (pending_read_req_ready && !read_resp_valid) {
    read_request_cnt := read_request_cnt + 1.U
  }.elsewhen (read_resp_valid && !pending_read_req_ready) {
    read_request_cnt := read_request_cnt - 1.U
  }
  when (!pending_read_resp_valid && read_resp_valid && !read_resp_last) {
    pending_read_resp_valid := true.B
    pending_read_resp_id := read_resp_id
  }.elsewhen (pending_read_resp_valid && !read_resp_valid && read_resp_last) {
    pending_read_resp_valid := false.B
  }
  when (read_resp_last) {
    rdata_cnt.reset()
  }.elsewhen (r_resp.fire) {
    rdata_cnt.inc()
  }

  // `r_pipe`: the extra pipeline registers for the read response `in.r`
  prefix("r_pipe") {
    val valid = RegInit(false.B)
    when (in.r.fire) { valid := false.B }
    when (r_resp.fire) { valid := true.B }
    in.r.valid := valid
    in.r.bits := RegEnable(r_resp.bits, r_resp.fire)
    r_resp.ready := !valid || in.r.ready

    // the data should be auto-hold
    in.r.bits.data := ramHelper.readAndHold(read_resp_addr, r_resp.fire).asUInt
  }

  // write response
  val pending_write_resp_valid = RegInit(false.B)
  val pending_write_resp_id = Reg(UInt(in.b.bits.id.getWidth.W))
  val has_write_resp = Wire(Bool())
  val write_request_cnt = RegInit(0.U(8.W))
  val write_have_req_cnt = write_request_cnt =/= 0.U
  val (write_resp_valid, write_resp_id) = writeResponse((!has_write_resp || in.b.fire) && write_have_req_cnt)
  has_write_resp := write_resp_valid || pending_write_resp_valid
  in.b.valid := write_resp_valid || pending_write_resp_valid
  in.b.bits.id := Mux(pending_write_resp_valid, pending_write_resp_id, write_resp_id)
  in.b.bits.resp := AXI4Parameters.RESP_OKAY

  // The return values of DPI-C are used to determine whether a request has been made or completed
  // pending_write_req_ready ---> writeRequest()
  // write_resp_valid <--- writeResponse()
  when (pending_write_req_ready && !write_resp_valid) {
    write_request_cnt := write_request_cnt + 1.U
  }.elsewhen (write_resp_valid && !pending_write_req_ready) {
    write_request_cnt := write_request_cnt - 1.U
  }
  when (!pending_write_resp_valid && write_resp_valid && !in.b.ready) {
    pending_write_resp_valid := true.B
    pending_write_resp_id := write_resp_id
  }.elsewhen (pending_write_resp_valid && !write_resp_valid && in.b.ready) {
    pending_write_resp_valid := false.B
  }
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
