package pbus

import chisel3._
import chisel3.BlackBox
import chisel3.Clock
import chisel3.IO
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util._
import chisel3.util.HasBlackBoxResource
import coupledL2.tl2chi.PortIO
import freechips.rocketchip.amba.axi4.AXI4Bundle
import freechips.rocketchip.amba.axi4.AXI4BundleAW
import freechips.rocketchip.amba.axi4.AXI4BundleParameters
import freechips.rocketchip.amba.axi4.AXI4MasterNode
import freechips.rocketchip.amba.axi4.AXI4MasterParameters
import freechips.rocketchip.amba.axi4.AXI4MasterPortParameters
import freechips.rocketchip.amba.axi4.AXI4SlaveNode
import freechips.rocketchip.amba.axi4.AXI4SlaveParameters
import freechips.rocketchip.amba.axi4.AXI4SlavePortParameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomacy.IdRange
import freechips.rocketchip.diplomacy.InModuleBody
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.diplomacy.RegionType
import freechips.rocketchip.diplomacy.TransferSizes
import freechips.rocketchip.util.HeterogeneousBag
import org.chipsalliance.cde.config.Parameters
import system.HasSoCParameter
import utils.VerilogAXI4Record
import _root_.circt.stage._
import pbus.AplicParams



import chisel3.stage.ChiselGeneratorAnnotation

//case class AplicParams(
//    CFG_ADDR_WIDTH: Int = 40,
//    CFG_DATA_WIDTH: Int = 64,
//    CFG_ID_WIDTH:   Int = 16,
//    APLICAddrMap:     AddressSet = AddressSet(0x31100000L, 0x7fff),
//    MSI_DATA_WIDTH: Int = 32,
//    NumIntSrcs:     Int = 512
//)

class aplic_top(params: AplicParams)(implicit p: Parameters) extends LazyModule {
//  val o_axi4 = AXI4MasterNode(Seq(AXI4MasterPortParameters(
//    Seq(AXI4MasterParameters(
//      name = "s_axi_",
//      id = IdRange(0, 1 << params.CFG_ID_WIDTH)
//    ))
//  )))
//
//  val i_axi4 = AXI4SlaveNode(Seq(
//    AXI4SlavePortParameters(
//      slaves = Seq(AXI4SlaveParameters(
//        address = Seq(params.APLICAddrMap),
//        regionType = RegionType.UNCACHED,
//        executable = false,
//        supportsWrite = TransferSizes(1, params.CFG_DATA_WIDTH / 8),
//        supportsRead = TransferSizes(1, params.CFG_DATA_WIDTH / 8)

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val i_dft_icg_scan_en    = IO(Input(Bool()))
    val i_aplic_wire_int_vld = IO(Input(UInt(params.NumIntSrcs.W)))
    // instance aplic
    val u_aplic = Module(new apic_aplic_top(params))
    // connect aplic
    // imsic axi4 io
    val aplic_s = IO(Flipped(new AXI4Bundle(AXI4BundleParameters(
      addrBits = params.CFG_ADDR_WIDTH,dataBits=params.CFG_DATA_WIDTH,idBits = params.CFG_ID_WIDTH
    ))))
    val aplic_m = IO(new AXI4Bundle(AXI4BundleParameters(
      addrBits = params.CFG_ADDR_WIDTH, dataBits=params.MSI_DATA_WIDTH,idBits = params.CFG_ID_WIDTH
    )))
    u_aplic.io.i_aplic_mmcr_araddr := aplic_s.ar.bits.addr
    u_aplic.io.i_aplic_mmcr_arburst := aplic_s.ar.bits.burst
    u_aplic.io.i_aplic_mmcr_arcache := aplic_s.ar.bits.cache
    u_aplic.io.i_aplic_mmcr_arid    := aplic_s.ar.bits.id
    u_aplic.io.i_aplic_mmcr_arlen   := aplic_s.ar.bits.len
    u_aplic.io.i_aplic_mmcr_arlock  := aplic_s.ar.bits.lock
    u_aplic.io.i_aplic_mmcr_arprot  := aplic_s.ar.bits.prot
    u_aplic.io.i_aplic_mmcr_arsize  := aplic_s.ar.bits.size
    u_aplic.io.i_aplic_mmcr_arvalid := aplic_s.ar.valid
    // 写地址通道 (AW)
    u_aplic.io.i_aplic_mmcr_awaddr  := aplic_s.aw.bits.addr
    u_aplic.io.i_aplic_mmcr_awburst := aplic_s.aw.bits.burst
    u_aplic.io.i_aplic_mmcr_awcache := aplic_s.aw.bits.cache
    u_aplic.io.i_aplic_mmcr_awid    := aplic_s.aw.bits.id
    u_aplic.io.i_aplic_mmcr_awlen   := aplic_s.aw.bits.len
    u_aplic.io.i_aplic_mmcr_awlock  := aplic_s.aw.bits.lock
    u_aplic.io.i_aplic_mmcr_awprot  := aplic_s.aw.bits.prot
    u_aplic.io.i_aplic_mmcr_awsize  := aplic_s.aw.bits.size
    u_aplic.io.i_aplic_mmcr_awvalid := aplic_s.aw.valid
    // 响应与数据通道
    u_aplic.io.i_aplic_mmcr_bready := aplic_s.b.ready
    u_aplic.io.i_aplic_mmcr_rready := aplic_s.r.ready
    u_aplic.io.i_aplic_mmcr_wdata  := aplic_s.w.bits.data
    u_aplic.io.i_aplic_mmcr_wlast  := aplic_s.w.bits.last
    u_aplic.io.i_aplic_mmcr_wstrb  := aplic_s.w.bits.strb
    u_aplic.io.i_aplic_mmcr_wvalid := aplic_s.w.valid
    // MSI 子接口输入
    u_aplic.io.i_aplic_msi_awready := aplic_m.aw.ready
    u_aplic.io.i_aplic_msi_bid     := aplic_m.b.bits.id
    u_aplic.io.i_aplic_msi_bresp   := aplic_m.b.bits.resp
    u_aplic.io.i_aplic_msi_bvalid  := aplic_m.b.valid
    u_aplic.io.i_aplic_msi_wready  := aplic_m.w.ready
    // 输出信号
    aplic_s.ar.ready    := u_aplic.io.o_aplic_mmcr_arready
    aplic_s.aw.ready    := u_aplic.io.o_aplic_mmcr_awready
    aplic_s.b.bits.id   := u_aplic.io.o_aplic_mmcr_bid
    aplic_s.b.bits.resp := u_aplic.io.o_aplic_mmcr_bresp
    aplic_s.b.valid     := u_aplic.io.o_aplic_mmcr_bvalid
    aplic_s.r.bits.data := u_aplic.io.o_aplic_mmcr_rdata
    aplic_s.r.bits.id   := u_aplic.io.o_aplic_mmcr_rid
    aplic_s.r.bits.last := u_aplic.io.o_aplic_mmcr_rlast
    aplic_s.r.bits.resp := u_aplic.io.o_aplic_mmcr_rresp
    aplic_s.r.valid     := u_aplic.io.o_aplic_mmcr_rvalid
    aplic_s.w.ready     := u_aplic.io.o_aplic_mmcr_wready
    // MSI 子接口输出
    // write
    aplic_m.aw.bits.addr := u_aplic.io.o_aplic_msi_awaddr 
    aplic_m.aw.bits.id   := u_aplic.io.o_aplic_msi_awid
    aplic_m.aw.bits.prot := u_aplic.io.o_aplic_msi_awprot
    aplic_m.aw.bits.size := 2.U
    aplic_m.aw.bits.len := 0.U
    aplic_m.aw.bits.burst := 0.U
    aplic_m.aw.bits.lock := 0.U
    aplic_m.aw.bits.cache := 0.U
    aplic_m.aw.bits.qos := 0.U
    aplic_m.aw.valid     := u_aplic.io.o_aplic_msi_awvalid
    aplic_m.b.ready      := u_aplic.io.o_aplic_msi_bready
    aplic_m.w.bits.data  := u_aplic.io.o_aplic_msi_wdata
    aplic_m.w.bits.strb  := u_aplic.io.o_aplic_msi_wstrb
    aplic_m.w.bits.last  := true.B
    aplic_m.w.valid      := u_aplic.io.o_aplic_msi_wvalid
    // read
    aplic_m.ar.bits.addr := 0.U
    aplic_m.ar.bits.id   := 0.U
    aplic_m.ar.bits.prot := 0.U
    aplic_m.ar.bits.size := 2.U
    aplic_m.ar.bits.len := 0.U
    aplic_m.ar.bits.burst := 0.U
    aplic_m.ar.bits.lock := 0.U
    aplic_m.ar.bits.cache := 0.U
    aplic_m.ar.bits.qos := 0.U
    aplic_m.ar.valid := false.B
    aplic_m.r.ready := true.B
    u_aplic.io.i_aplic_wire_int_vld := i_aplic_wire_int_vld
    u_aplic.io.i_dft_icg_scan_en    := i_dft_icg_scan_en
    u_aplic.io.aclk                 := clock
    u_aplic.io.arst_n               := (!reset.asBool).asAsyncReset

  }
}

class apic_aplic_top(params: AplicParams) extends BlackBox {
  val io = IO(new Bundle {
    // 时钟与复位
    val aclk   = Input(Clock())
    val arst_n = Input(Reset())
    // 读地址通道 (AR)
    // val i_aplic_mmcr = IO(Input(AXI4BundleAW()))
    val i_aplic_mmcr_araddr  = Input(UInt(40.W)) // 39:0
    val i_aplic_mmcr_arburst = Input(UInt(2.W))  // 1:0
    val i_aplic_mmcr_arcache = Input(UInt(4.W))  // 3:0
    val i_aplic_mmcr_arid    = Input(UInt(16.W)) // 15:0
    val i_aplic_mmcr_arlen   = Input(UInt(8.W))  // 7:0
    val i_aplic_mmcr_arlock  = Input(Bool())     // 单bit
    val i_aplic_mmcr_arprot  = Input(UInt(3.W))  // 2:0
    val i_aplic_mmcr_arsize  = Input(UInt(3.W))  // 2:0
    val i_aplic_mmcr_arvalid = Input(Bool())     // 单bit

    // 写地址通道 (AW)
    val i_aplic_mmcr_awaddr  = Input(UInt(40.W)) // 39:0
    val i_aplic_mmcr_awburst = Input(UInt(2.W))  // 1:0
    val i_aplic_mmcr_awcache = Input(UInt(4.W))  // 3:0
    val i_aplic_mmcr_awid    = Input(UInt(16.W)) // 15:0
    val i_aplic_mmcr_awlen   = Input(UInt(8.W))  // 7:0
    val i_aplic_mmcr_awlock  = Input(Bool())     // 单bit
    val i_aplic_mmcr_awprot  = Input(UInt(3.W))  // 2:0
    val i_aplic_mmcr_awsize  = Input(UInt(3.W))  // 2:0
    val i_aplic_mmcr_awvalid = Input(Bool())     // 单bit

    // 响应与数据通道
    val i_aplic_mmcr_bready = Input(Bool())     // 单bit
    val i_aplic_mmcr_rready = Input(Bool())     // 单bit
    val i_aplic_mmcr_wdata  = Input(UInt(64.W)) // 63:0
    val i_aplic_mmcr_wlast  = Input(Bool())     // 7:0
    val i_aplic_mmcr_wstrb  = Input(UInt(8.W))  // 7:0
    val i_aplic_mmcr_wvalid = Input(Bool())     // 单bit

    // MSI 子接口输入
    val i_aplic_msi_awready  = Input(Bool())                      // 单bit
    val i_aplic_msi_bid      = Input(UInt(16.W))                  // 15:0
    val i_aplic_msi_bresp    = Input(UInt(2.W))                   // 1:0
    val i_aplic_msi_bvalid   = Input(Bool())                      // 单bit
    val i_aplic_msi_wready   = Input(Bool())                      // 单bit
    val i_aplic_wire_int_vld = Input(UInt(params.NumIntSrcs.W)) // 255:0
    val i_dft_icg_scan_en    = Input(Bool())                      // 单bit

    // 输出信号
    val o_aplic_mmcr_arready = Output(Bool())     // 单bit
    val o_aplic_mmcr_awready = Output(Bool())     // 单bit
    val o_aplic_mmcr_bid     = Output(UInt(16.W)) // 15:0
    val o_aplic_mmcr_bresp   = Output(UInt(2.W))  // 1:0
    val o_aplic_mmcr_bvalid  = Output(Bool())     // 单bit
    val o_aplic_mmcr_rdata   = Output(UInt(64.W)) // 63:0
    val o_aplic_mmcr_rid     = Output(UInt(16.W)) // 15:0
    val o_aplic_mmcr_rlast   = Output(Bool())     // 单bit
    val o_aplic_mmcr_rresp   = Output(UInt(2.W))  // 1:0
    val o_aplic_mmcr_rvalid  = Output(Bool())     // 单bit
    val o_aplic_mmcr_wready  = Output(Bool())     // 单bit

    // MSI 子接口输出
    val o_aplic_msi_awaddr  = Output(UInt(40.W)) // 39:0
    val o_aplic_msi_awid    = Output(UInt(16.W)) // 15:0
    val o_aplic_msi_awprot  = Output(UInt(3.W))  // 2:0
    val o_aplic_msi_awvalid = Output(Bool())     // 单bit
    val o_aplic_msi_bready  = Output(Bool())     // 31:0
    val o_aplic_msi_wdata   = Output(UInt(32.W)) // 31:0
    val o_aplic_msi_wstrb   = Output(UInt(4.W))  // 3:0
    val o_aplic_msi_wvalid  = Output(Bool())     // 单bit
  })
}
object AplicGen extends App {
  // Example configuration with mixed input widths
  val params = AplicParams()
  implicit val p: Parameters = Parameters.empty

  val pbusM = LazyModule(new aplic_top(params)(p))
  println("Generating the Pbus SystemVerilog...")
  val path = """./build/rtl/"""
  (new ChiselStage).execute(
    Array(
      "--target-dir", path,
      "--split-verilog"
    ),
    Seq(
      ChiselGeneratorAnnotation(() => pbusM.module),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("--disable-annotation-unknown"),
      FirtoolOption("--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"),
      _root_.circt.stage.CIRCTTargetAnnotation(_root_.circt.stage.CIRCTTarget.SystemVerilog)
    )
  )
  //println("Generating the Pbus SystemVerilog...")
  //(new ChiselStage).execute(
  //  args,
  //  Seq(chisel3.stage.ChiselGeneratorAnnotation(() => LazyModule(new uncoreTop(params)).module))
  //)
}