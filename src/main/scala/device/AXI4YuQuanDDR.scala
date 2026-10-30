/***************************************************************************************
* Copyright (c) 2026 Beijing Institute of Open Source Chip (BOSC)
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

import chisel3._
import chisel3.experimental.dataview._
import chisel3.experimental.ExtModule
import chisel3.util._
import freechips.rocketchip.amba.axi4.{
  AXI4Bundle,
  AXI4BundleParameters,
  AXI4Parameters,
  AXI4SlaveNode,
  AXI4SlaveParameters,
  AXI4SlavePortParameters
}
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp, RegionType, TransferSizes}
import org.chipsalliance.cde.config.Parameters
import utility._
import utils.VerilogAXI4Record

class YuQuanPreloadDoneHelper extends ExtModule with HasExtModuleInline {
  val clock = IO(Input(Clock()))
  val reset = IO(Input(Reset()))
  val done  = IO(Output(Bool()))

  private val verilogLines = Seq(
    "import \"DPI-C\" function bit yuquan_preload_done();",
    "",
    "module YuQuanPreloadDoneHelper(",
    "  input  clock,",
    "  input  reset,",
    "  output reg done",
    ");",
    "",
    "always @(posedge clock or posedge reset) begin",
    "  if (reset) begin",
    "    done <= 1'b0;",
    "  end else begin",
    "    done <= yuquan_preload_done();",
    "  end",
    "end",
    "",
    "endmodule"
  )
  setInline(s"$desiredName.v", verilogLines.mkString("\n"))
}

class YuQuanDDRBlackBox extends ExtModule {
  override def desiredName: String = "ddr_wrapper"

  val aresetn_0 = IO(Input(Bool()))
  val aclk_0    = IO(Input(Clock()))

  val axi = FlatIO(Flipped(new VerilogAXI4Record(AXI4BundleParameters(
    addrBits = 36,
    dataBits = 256,
    idBits = 18
  ), portSuffix = "_0")))

  val awuser_0        = IO(Input(UInt(8.W)))
  val awregion_0      = IO(Input(UInt(4.W)))
  val awurgent_0      = IO(Input(Bool()))
  val awpoison_0      = IO(Input(Bool()))
  val awpoison_intr_0 = IO(Output(Bool()))
  val awautopre_0     = IO(Input(Bool()))

  val wuser_0  = IO(Input(UInt(8.W)))

  val buser_0  = IO(Output(UInt(8.W)))

  val aruser_0        = IO(Input(UInt(8.W)))
  val arregion_0      = IO(Input(UInt(4.W)))
  val arpoison_0      = IO(Input(Bool()))
  val arpoison_intr_0 = IO(Output(Bool()))
  val arurgent_0      = IO(Input(Bool()))
  val arautopre_0     = IO(Input(Bool()))

  val ruser_0  = IO(Output(UInt(8.W)))

  val training_over = IO(Output(Bool()))

  def axi4: AXI4Bundle = axi.viewAs[AXI4Bundle]
}

class AXI4MCDDR(
  val address: Seq[AddressSet],
  val memByte: Long,
  val useBlackBox: Boolean = true,
  val executable: Boolean = true,
  val beatBytes: Int,
  val burstLen: Int,
)(implicit p: Parameters)
  extends LazyModule
{
  val node = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
      address,
      regionType = RegionType.UNCACHED,
      executable = executable,
      supportsWrite = TransferSizes(1, beatBytes * burstLen),
      supportsRead = TransferSizes(1, beatBytes * burstLen),
      interleavedId = Some(0)
    )),
    beatBytes = beatBytes
  )))

  override lazy val module = new AXI4MCDDRImp(this)
}

class AXI4MCDDRImp(outer: AXI4MCDDR) extends LazyModuleImp(outer) {
  val (in, _) = outer.node.in.head
  chisel3.dontTouch(in)

  require(outer.beatBytes == 32, s"YuQuan ddr_wrapper uses a 256-bit AXI data bus, got ${outer.beatBytes * 8} bits")
  require(in.aw.bits.id.getWidth <= 18, s"YuQuan ddr_wrapper AXI ID width is 18, got ${in.aw.bits.id.getWidth}")
  require(in.ar.bits.id.getWidth <= 18, s"YuQuan ddr_wrapper AXI ID width is 18, got ${in.ar.bits.id.getWidth}")

  private def fitTo(x: UInt, width: Int): UInt = {
    if (x.getWidth >= width) {
      x(width - 1, 0)
    } else {
      Cat(0.U((width - x.getWidth).W), x)
    }
  }

  val ddr = Module(new YuQuanDDRBlackBox)
  val ddrAxi = ddr.axi4
  val preload = Module(new YuQuanPreloadDoneHelper)

  preload.clock := clock
  preload.reset := reset

  ddr.aclk_0 := clock
  ddr.aresetn_0 := !reset.asBool

  val mcReady = ddr.training_over && preload.done

  ddrAxi.aw.bits.id     := fitTo(in.aw.bits.id, 18)
  ddrAxi.aw.bits.addr   := fitTo(in.aw.bits.addr, 36)
  ddrAxi.aw.bits.len    := fitTo(in.aw.bits.len, 8)
  ddrAxi.aw.bits.size   := fitTo(in.aw.bits.size, 3)
  ddrAxi.aw.bits.burst  := fitTo(in.aw.bits.burst, 2)
  ddrAxi.aw.bits.lock   := fitTo(in.aw.bits.lock, 1)
  ddrAxi.aw.bits.cache  := fitTo(in.aw.bits.cache, 4)
  ddrAxi.aw.bits.prot   := fitTo(in.aw.bits.prot, 3)
  ddr.awuser_0   := 0.U
  ddrAxi.aw.valid := in.aw.valid && mcReady
  ddrAxi.aw.bits.qos := fitTo(in.aw.bits.qos, 4)
  ddr.awregion_0 := 0.U
  ddr.awurgent_0 := false.B
  ddr.awpoison_0 := false.B
  ddr.awautopre_0 := false.B
  in.aw.ready := ddrAxi.aw.ready && mcReady

  ddrAxi.w.bits.data := fitTo(in.w.bits.data, 256)
  ddrAxi.w.bits.strb := fitTo(in.w.bits.strb, 32)
  ddrAxi.w.bits.last := in.w.bits.last
  ddrAxi.w.valid := in.w.valid && mcReady
  ddr.wuser_0  := 0.U
  in.w.ready := ddrAxi.w.ready && mcReady

  in.b.valid := ddrAxi.b.valid && mcReady
  in.b.bits.id := fitTo(ddrAxi.b.bits.id, in.b.bits.id.getWidth)
  in.b.bits.resp := fitTo(ddrAxi.b.bits.resp, in.b.bits.resp.getWidth)
  in.b.bits.user := DontCare
  in.b.bits.echo := DontCare
  ddrAxi.b.ready := in.b.ready && mcReady

  ddrAxi.ar.bits.id     := fitTo(in.ar.bits.id, 18)
  ddrAxi.ar.bits.addr   := fitTo(in.ar.bits.addr, 36)
  ddrAxi.ar.bits.len    := fitTo(in.ar.bits.len, 8)
  ddrAxi.ar.bits.size   := fitTo(in.ar.bits.size, 3)
  ddrAxi.ar.bits.burst  := fitTo(in.ar.bits.burst, 2)
  ddrAxi.ar.bits.lock   := fitTo(in.ar.bits.lock, 1)
  ddrAxi.ar.bits.cache  := fitTo(in.ar.bits.cache, 4)
  ddrAxi.ar.bits.prot   := fitTo(in.ar.bits.prot, 3)
  ddr.aruser_0   := 0.U
  ddrAxi.ar.valid := in.ar.valid && mcReady
  ddrAxi.ar.bits.qos := fitTo(in.ar.bits.qos, 4)
  ddr.arregion_0 := 0.U
  ddr.arpoison_0 := false.B
  ddr.arurgent_0 := false.B
  ddr.arautopre_0 := false.B
  in.ar.ready := ddrAxi.ar.ready && mcReady

  in.r.valid := ddrAxi.r.valid && mcReady
  in.r.bits.id := fitTo(ddrAxi.r.bits.id, in.r.bits.id.getWidth)
  in.r.bits.data := fitTo(ddrAxi.r.bits.data, in.r.bits.data.getWidth)
  in.r.bits.resp := fitTo(ddrAxi.r.bits.resp, in.r.bits.resp.getWidth)
  in.r.bits.user := DontCare
  in.r.bits.echo := DontCare
  in.r.bits.last := ddrAxi.r.bits.last
  ddrAxi.r.ready := in.r.ready && mcReady

  when(in.aw.fire) {
    assert(in.aw.bits.burst === AXI4Parameters.BURST_INCR, "YuQuan ddr_wrapper only supports AXI INCR bursts")
  }
  when(in.ar.fire) {
    assert(in.ar.bits.burst === AXI4Parameters.BURST_INCR, "YuQuan ddr_wrapper only supports AXI INCR bursts")
  }
}

class AXI4MCDDRWrapper(
  slave: freechips.rocketchip.amba.axi4.AXI4SlaveNode,
  memByte: Long,
  useBlackBox: Boolean = true
)(implicit p: Parameters) extends AXI4MemorySlave(slave, memByte, useBlackBox) {
  val ddr = LazyModule(new AXI4MCDDR(
    slaveParam.address,
    memByte,
    useBlackBox,
    slaveParam.executable,
    portParam.beatBytes,
    burstLen
  ))
  ddr.node := master
}
