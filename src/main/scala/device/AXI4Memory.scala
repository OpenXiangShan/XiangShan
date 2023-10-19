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
import chisel3.experimental.ExtModule
import chisel3.util._
import freechips.rocketchip.amba.axi4.{AXI4MasterNode, AXI4Parameters, AXI4SlaveNode}
import freechips.rocketchip.diplomacy.{AddressSet, InModuleBody, LazyModule, LazyModuleImp}
import utils._
import utility._

class MemoryRWHelper extends ExtModule with HasExtModuleInline {
  val DataBits = 64

  val clock = IO(Input(Clock()))
  val reset = IO(Input(Reset()))
  val ren   = IO(Input(Bool()))
  val rIdx  = IO(Input(UInt(DataBits.W)))
  val rdata = IO(Output(UInt(DataBits.W)))
  val wen   = IO(Input(Bool()))
  val wIdx  = IO(Input(UInt(DataBits.W)))
  val wdata = IO(Input(UInt(DataBits.W)))
  val wmask = IO(Input(UInt(DataBits.W)))

  def read(enable: Bool, address: UInt): UInt = {
    ren := enable
    rIdx := address
    rdata
  }
  def write(enable: Bool, address: UInt, data: UInt, mask: UInt): Unit = {
    wen := enable
    wIdx := address
    wdata := data
    wmask := mask
  }

  val verilogLines = Seq(
    "import \"DPI-C\" function longint difftest_ram_read(input longint rIdx);",
    "import \"DPI-C\" function void difftest_ram_write(",
    "  input  longint index,",
    "  input  longint data,",
    "  input  longint mask",
    ");",
    "module MemoryRWHelper(",
    "  input         clock,",
    "  input         reset,",
    "  input         ren,",
    "  input  [63:0] rIdx,",
    "  output [63:0] rdata,",
    "  input  [63:0] wIdx,",
    "  input  [63:0] wdata,",
    "  input  [63:0] wmask,",
    "  input         wen",
    ");",
    "",
    "  assign rdata = (!reset && ren) ? difftest_ram_read(rIdx) : 64'b0;",
    "",
    "  always @(posedge clock) begin",
    "    if (!reset && wen) begin",
    "      difftest_ram_write(wIdx, wdata, wmask);",
    "    end",
    "  end",
    "",
    "endmodule"
  )
  setInline(s"$desiredName.v", verilogLines.mkString("\n"))
}

object MemoryRWHelper {
  def apply(clock: Clock, reset: Reset): MemoryRWHelper = {
    val helper = Module(new MemoryRWHelper)
    helper.clock := clock
    helper.reset := reset
    helper
  }
}

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
    "always @(posedge clock or posedge reset) begin",
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
    "always @(posedge clock or posedge reset) begin",
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
  val ramWidth = 8
  val ramSplit = outer.beatBytes / ramWidth
  val ramBaseAddr = outer.address.head.base
  val ramOffsetBits = log2Ceil(outer.memByte)
  def ramIndex(addr: UInt) = ((addr - ramBaseAddr.U)(ramOffsetBits - 1, 0) >> log2Ceil(ramWidth)).asUInt
  val ramHelper = Seq.fill(ramSplit)(MemoryRWHelper(clock, reset))

  val numOutstanding = 1 << in.ar.bits.id.getWidth
  val addressMem = Mem(numOutstanding, UInt((in.ar.bits.addr.getWidth - log2Ceil(ramWidth)).W))
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
  XSError(in.aw.fire && in.aw.bits.len === 0.U, "data must have more than one beat now")
  val pending_write_req_ready = Wire(Bool())
  val pending_write_need_req = pending_write_req_valid.last && !pending_write_req_ready
  val write_req_valid = pending_write_req_valid.head && (pending_write_need_req || in.w.valid && in.w.bits.last)
  pending_write_req_ready := writeRequest(write_req_valid, pending_write_req_bits.addr, pending_write_req_bits.id)

  when (in.aw.fire) {
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
  val write_req_index = ramIndex(write_req_addr) + Cat(wdata_cnt.value, 0.U(log2Ceil(ramSplit).W))
  for ((ram, i) <- ramHelper.zipWithIndex) {
    val enable = in.w.fire
    val address = write_req_index + i.U
    val data = in.w.bits.data(ramWidth * 8 * i + 63, ramWidth * 8 * i)
    val mask = MaskExpand(in.w.bits.strb(i * 8 + 7, i * 8))
    ram.write(enable, address, data, mask)
  }
  when (write_req_last) {
    wdata_cnt.reset()
  }.elsewhen (in.w.fire) {
    wdata_cnt.inc()
  }

  // read data response
  val pending_read_resp_valid = RegInit(false.B)
  val pending_read_resp_id = Reg(UInt(in.r.bits.id.getWidth.W))
  val has_read_resp = Wire(Bool())
  val read_resp_last = in.r.fire && in.r.bits.last
  val (read_resp_valid, read_resp_id) = readResponse(!has_read_resp || read_resp_last)
  has_read_resp := (read_resp_valid && !read_resp_last) || pending_read_resp_valid
  val rdata_cnt = Counter(outer.burstLen)
  val read_resp_addr = addressMem(in.r.bits.id) + Cat(rdata_cnt.value, 0.U(log2Ceil(ramSplit).W))
  val read_resp_len = arlenMem(in.r.bits.id)
  in.r.valid := read_resp_valid || pending_read_resp_valid
  in.r.bits.id := Mux(pending_read_resp_valid, pending_read_resp_id, read_resp_id)
  val rdata = ramHelper.zipWithIndex.map{ case (ram, i) => ram.read(in.r.valid, read_resp_addr + i.U) }
  in.r.bits.data := VecInit(rdata).asUInt
  in.r.bits.resp := AXI4Parameters.RESP_OKAY
  in.r.bits.last := (rdata_cnt.value === read_resp_len)

  when (!pending_read_resp_valid && read_resp_valid && !read_resp_last) {
    pending_read_resp_valid := true.B
    pending_read_resp_id := read_resp_id
  }.elsewhen (pending_read_resp_valid && !read_resp_valid && read_resp_last) {
    pending_read_resp_valid := false.B
  }
  when (read_resp_last) {
    rdata_cnt.reset()
  }.elsewhen (in.r.fire) {
    rdata_cnt.inc()
  }

  // write response
  val pending_write_resp_valid = RegInit(false.B)
  val pending_write_resp_id = Reg(UInt(in.b.bits.id.getWidth.W))
  val has_write_resp = Wire(Bool())
  val (write_resp_valid, write_resp_id) = writeResponse(!has_write_resp || in.b.fire)
  has_write_resp := write_resp_valid || pending_write_resp_valid
  in.b.valid := write_resp_valid || pending_write_resp_valid
  in.b.bits.id := Mux(pending_write_resp_valid, pending_write_resp_id, write_resp_id)
  in.b.bits.resp := AXI4Parameters.RESP_OKAY

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
