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
import chisel3.experimental.ExtModule
import chisel3.util._
import freechips.rocketchip.amba.axi4.{AXI4Parameters, AXI4SlaveNode, AXI4SlaveParameters, AXI4SlavePortParameters}
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp, RegionType, TransferSizes}
import org.chipsalliance.cde.config.Parameters
import utility._

class MCCosimPreloadDoneHelper extends ExtModule with HasExtModuleInline {
  val clock = IO(Input(Clock()))
  val reset = IO(Input(Reset()))
  val done  = IO(Output(Bool()))

  private val verilogLines = Seq(
    "import \"DPI-C\" function bit mc_cosim_preload_done();",
    "",
    "module MCCosimPreloadDoneHelper(",
    "  input  clock,",
    "  input  reset,",
    "  output reg done",
    ");",
    "",
    "always @(posedge clock or posedge reset) begin",
    "  if (reset) begin",
    "    done <= 1'b0;",
    "  end else begin",
    "    done <= mc_cosim_preload_done();",
    "  end",
    "end",
    "",
    "endmodule"
  )
  setInline(s"$desiredName.v", verilogLines.mkString("\n"))
}

class MCCosimDDRBlackBox extends ExtModule {
  override def desiredName: String = "ddr_wrapper"

  val aresetn_0 = IO(Input(Bool()))
  val aclk_0    = IO(Input(Clock()))

  val awid_0          = IO(Input(UInt(16.W)))
  val awaddr_0        = IO(Input(UInt(36.W)))
  val awlen_0         = IO(Input(UInt(8.W)))
  val awsize_0        = IO(Input(UInt(3.W)))
  val awburst_0       = IO(Input(UInt(2.W)))
  val awlock_0        = IO(Input(UInt(1.W)))
  val awcache_0       = IO(Input(UInt(4.W)))
  val awprot_0        = IO(Input(UInt(3.W)))
  val awuser_0        = IO(Input(UInt(8.W)))
  val awvalid_0       = IO(Input(Bool()))
  val awready_0       = IO(Output(Bool()))
  val awqos_0         = IO(Input(UInt(4.W)))
  val awregion_0      = IO(Input(UInt(4.W)))
  val awurgent_0      = IO(Input(Bool()))
  val awpoison_0      = IO(Input(Bool()))
  val awpoison_intr_0 = IO(Output(Bool()))
  val awautopre_0     = IO(Input(Bool()))

  val wdata_0  = IO(Input(UInt(256.W)))
  val wstrb_0  = IO(Input(UInt(32.W)))
  val wlast_0  = IO(Input(Bool()))
  val wvalid_0 = IO(Input(Bool()))
  val wready_0 = IO(Output(Bool()))
  val wuser_0  = IO(Input(UInt(8.W)))

  val bid_0    = IO(Output(UInt(16.W)))
  val bresp_0  = IO(Output(UInt(2.W)))
  val buser_0  = IO(Output(UInt(8.W)))
  val bvalid_0 = IO(Output(Bool()))
  val bready_0 = IO(Input(Bool()))

  val arid_0          = IO(Input(UInt(16.W)))
  val araddr_0        = IO(Input(UInt(36.W)))
  val arlen_0         = IO(Input(UInt(8.W)))
  val arsize_0        = IO(Input(UInt(3.W)))
  val arburst_0       = IO(Input(UInt(2.W)))
  val arlock_0        = IO(Input(UInt(1.W)))
  val arcache_0       = IO(Input(UInt(4.W)))
  val arprot_0        = IO(Input(UInt(3.W)))
  val aruser_0        = IO(Input(UInt(8.W)))
  val arvalid_0       = IO(Input(Bool()))
  val arready_0       = IO(Output(Bool()))
  val arqos_0         = IO(Input(UInt(4.W)))
  val arregion_0      = IO(Input(UInt(4.W)))
  val arpoison_0      = IO(Input(Bool()))
  val arpoison_intr_0 = IO(Output(Bool()))
  val arurgent_0      = IO(Input(Bool()))
  val arautopre_0     = IO(Input(Bool()))

  val rid_0    = IO(Output(UInt(16.W)))
  val rdata_0  = IO(Output(UInt(256.W)))
  val rresp_0  = IO(Output(UInt(2.W)))
  val ruser_0  = IO(Output(UInt(8.W)))
  val rlast_0  = IO(Output(Bool()))
  val rvalid_0 = IO(Output(Bool()))
  val rready_0 = IO(Input(Bool()))

  val training_over = IO(Output(Bool()))
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

  require(outer.beatBytes == 32, s"mc-cosim ddr_wrapper uses a 256-bit AXI data bus, got ${outer.beatBytes * 8} bits")
  require(in.aw.bits.id.getWidth <= 16, s"mc-cosim ddr_wrapper AXI ID width is 16, got ${in.aw.bits.id.getWidth}")
  require(in.ar.bits.id.getWidth <= 16, s"mc-cosim ddr_wrapper AXI ID width is 16, got ${in.ar.bits.id.getWidth}")

  private def fitTo(x: UInt, width: Int): UInt = {
    if (x.getWidth >= width) {
      x(width - 1, 0)
    } else {
      Cat(0.U((width - x.getWidth).W), x)
    }
  }

  val ddr = Module(new MCCosimDDRBlackBox)
  val preload = Module(new MCCosimPreloadDoneHelper)

  preload.clock := clock
  preload.reset := reset

  ddr.aclk_0 := clock
  ddr.aresetn_0 := !reset.asBool

  val mcReady = ddr.training_over && preload.done

  ddr.awid_0     := fitTo(in.aw.bits.id, 16)
  ddr.awaddr_0   := fitTo(in.aw.bits.addr, 36)
  ddr.awlen_0    := fitTo(in.aw.bits.len, 8)
  ddr.awsize_0   := fitTo(in.aw.bits.size, 3)
  ddr.awburst_0  := fitTo(in.aw.bits.burst, 2)
  ddr.awlock_0   := fitTo(in.aw.bits.lock, 1)
  ddr.awcache_0  := fitTo(in.aw.bits.cache, 4)
  ddr.awprot_0   := fitTo(in.aw.bits.prot, 3)
  ddr.awuser_0   := 0.U
  ddr.awvalid_0  := in.aw.valid && mcReady
  ddr.awqos_0    := fitTo(in.aw.bits.qos, 4)
  ddr.awregion_0 := 0.U
  ddr.awurgent_0 := false.B
  ddr.awpoison_0 := false.B
  ddr.awautopre_0 := false.B
  in.aw.ready := ddr.awready_0 && mcReady

  ddr.wdata_0  := fitTo(in.w.bits.data, 256)
  ddr.wstrb_0  := fitTo(in.w.bits.strb, 32)
  ddr.wlast_0  := in.w.bits.last
  ddr.wvalid_0 := in.w.valid && mcReady
  ddr.wuser_0  := 0.U
  in.w.ready := ddr.wready_0 && mcReady

  in.b.valid := ddr.bvalid_0 && mcReady
  in.b.bits.id := fitTo(ddr.bid_0, in.b.bits.id.getWidth)
  in.b.bits.resp := fitTo(ddr.bresp_0, in.b.bits.resp.getWidth)
  in.b.bits.user := DontCare
  in.b.bits.echo := DontCare
  ddr.bready_0 := in.b.ready && mcReady

  ddr.arid_0     := fitTo(in.ar.bits.id, 16)
  ddr.araddr_0   := fitTo(in.ar.bits.addr, 36)
  ddr.arlen_0    := fitTo(in.ar.bits.len, 8)
  ddr.arsize_0   := fitTo(in.ar.bits.size, 3)
  ddr.arburst_0  := fitTo(in.ar.bits.burst, 2)
  ddr.arlock_0   := fitTo(in.ar.bits.lock, 1)
  ddr.arcache_0  := fitTo(in.ar.bits.cache, 4)
  ddr.arprot_0   := fitTo(in.ar.bits.prot, 3)
  ddr.aruser_0   := 0.U
  ddr.arvalid_0  := in.ar.valid && mcReady
  ddr.arqos_0    := fitTo(in.ar.bits.qos, 4)
  ddr.arregion_0 := 0.U
  ddr.arpoison_0 := false.B
  ddr.arurgent_0 := false.B
  ddr.arautopre_0 := false.B
  in.ar.ready := ddr.arready_0 && mcReady

  in.r.valid := ddr.rvalid_0 && mcReady
  in.r.bits.id := fitTo(ddr.rid_0, in.r.bits.id.getWidth)
  in.r.bits.data := fitTo(ddr.rdata_0, in.r.bits.data.getWidth)
  in.r.bits.resp := fitTo(ddr.rresp_0, in.r.bits.resp.getWidth)
  in.r.bits.user := DontCare
  in.r.bits.echo := DontCare
  in.r.bits.last := ddr.rlast_0
  ddr.rready_0 := in.r.ready && mcReady

  when(in.aw.fire) {
    assert(in.aw.bits.burst === AXI4Parameters.BURST_INCR, "mc-cosim ddr_wrapper only supports AXI INCR bursts")
  }
  when(in.ar.fire) {
    assert(in.ar.bits.burst === AXI4Parameters.BURST_INCR, "mc-cosim ddr_wrapper only supports AXI INCR bursts")
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
