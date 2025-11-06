package pbus

import chisel3.{BlackBox, Clock, IO, _}
import chisel3.util.{HasBlackBoxResource, _}
import coupledL2.tl2chi.PortIO
import freechips.rocketchip.diplomacy.LazyModule
import org.chipsalliance.cde.config.Parameters
import system.HasSoCParameter
import freechips.rocketchip.amba.axi4.{AXI4Bundle, AXI4BundleAW, AXI4BundleParameters, AXI4MasterNode, AXI4MasterParameters, AXI4MasterPortParameters, AXI4SlaveNode, AXI4SlaveParameters, AXI4SlavePortParameters}
import freechips.rocketchip.diplomacy.{IdRange, InModuleBody, RegionType, TransferSizes}
import freechips.rocketchip.util.HeterogeneousBag
import utils.VerilogAXI4Record
import freechips.rocketchip.diplomacy._

case class AplicParams(
                        CFG_ADDR_WIDTH: Int = 40,
                        CFG_DATA_WIDTH: Int = 64,
                        CFG_ID_WIDTH: Int = 16,
                        AplicRange: AddressSet = AddressSet(0x31100000L,0x7FFF),
                        MSI_DATA_WIDTH: Int = 32,
                      )

class aplic_top(params: AplicParams)(implicit p: Parameters) extends LazyModule {
  val o_axi4 = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    Seq(AXI4MasterParameters(
      name = "s_axi_",
      id = IdRange(0, 1 << params.CFG_ID_WIDTH)
    )),
  )))

  val i_axi4 = AXI4SlaveNode(Seq(
    AXI4SlavePortParameters(
      slaves = Seq(AXI4SlaveParameters(
        address = Seq(params.AplicRange),
        regionType = RegionType.UNCACHED,
        executable = false,
        supportsWrite = TransferSizes(1, params.CFG_DATA_WIDTH / 8),
        supportsRead = TransferSizes(1, params.CFG_DATA_WIDTH / 8)
      )),
      beatBytes = 8
    )
  ))
//  val s_axi = InModuleBody(i_axi4.makeIOs())
//  val m_axi = InModuleBody(o_axi4.makeIOs())
  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val i_dft_icg_scan_en = IO(Input(Bool()))
    val i_aplic_wire_int_vld = IO(Input(UInt(512.W)))
    // instance aplic
    val u_aplic = Module(new apic_aplic_top)
//    i_axi4.in.head._1.params.addrBits := params.CFG_ADDR_WIDTH
//    o_axi4.out.head._1.params.addrBits := params.CFG_ADDR_WIDTH
//    o_axi4.out.head._1.params.dataBits := params.MSI_DATA_WIDTH
    // connect aplic
//    val s_axi = IO(Flipped(new VerilogAXI4Record(i_axi4.in.head._1.params.copy(addrBits = params.CFG_ADDR_WIDTH))))
//    val m_axi = IO(new VerilogAXI4Record(
//      o_axi4.out.head._1.params.copy(addrBits = params.CFG_ADDR_WIDTH, dataBits = params.MSI_DATA_WIDTH))
//    )

    u_aplic.io.i_aplic_mmcr_araddr := i_axi4.in.head._1.ar.bits.addr
    u_aplic.io.i_aplic_mmcr_arburst := i_axi4.in.head._1.ar.bits.burst
    u_aplic.io.i_aplic_mmcr_arcache := i_axi4.in.head._1.ar.bits.cache
    u_aplic.io.i_aplic_mmcr_arid := i_axi4.in.head._1.ar.bits.id
    u_aplic.io.i_aplic_mmcr_arlen := i_axi4.in.head._1.ar.bits.len
    u_aplic.io.i_aplic_mmcr_arlock := i_axi4.in.head._1.ar.bits.lock
    u_aplic.io.i_aplic_mmcr_arprot := i_axi4.in.head._1.ar.bits.prot
    u_aplic.io.i_aplic_mmcr_arsize := i_axi4.in.head._1.ar.bits.size
    u_aplic.io.i_aplic_mmcr_arvalid := i_axi4.in.head._1.ar.valid
    // 写地址通道 (AW)
    u_aplic.io.i_aplic_mmcr_awaddr := i_axi4.in.head._1.aw.bits.addr
    u_aplic.io.i_aplic_mmcr_awburst := i_axi4.in.head._1.aw.bits.burst
    u_aplic.io.i_aplic_mmcr_awcache := i_axi4.in.head._1.aw.bits.cache
    u_aplic.io.i_aplic_mmcr_awid := i_axi4.in.head._1.aw.bits.id
    u_aplic.io.i_aplic_mmcr_awlen := i_axi4.in.head._1.aw.bits.len
    u_aplic.io.i_aplic_mmcr_awlock := i_axi4.in.head._1.aw.bits.lock
    u_aplic.io.i_aplic_mmcr_awprot := i_axi4.in.head._1.aw.bits.prot
    u_aplic.io.i_aplic_mmcr_awsize := i_axi4.in.head._1.aw.bits.size
    u_aplic.io.i_aplic_mmcr_awvalid := i_axi4.in.head._1.aw.valid
    // 响应与数据通道
    u_aplic.io.i_aplic_mmcr_bready := i_axi4.in.head._1.b.ready
    u_aplic.io.i_aplic_mmcr_rready := i_axi4.in.head._1.r.ready
    u_aplic.io.i_aplic_mmcr_wdata := i_axi4.in.head._1.w.bits.data
    u_aplic.io.i_aplic_mmcr_wlast := i_axi4.in.head._1.w.bits.last
    u_aplic.io.i_aplic_mmcr_wstrb := i_axi4.in.head._1.w.bits.strb
    u_aplic.io.i_aplic_mmcr_wvalid := i_axi4.in.head._1.w.valid
    // MSI 子接口输入
    u_aplic.io.i_aplic_msi_awready := o_axi4.out.head._1.aw.ready
    u_aplic.io.i_aplic_msi_bid := o_axi4.out.head._1.b.bits.id
    u_aplic.io.i_aplic_msi_bresp := o_axi4.out.head._1.b.bits.resp
    u_aplic.io.i_aplic_msi_bvalid := o_axi4.out.head._1.b.valid
    u_aplic.io.i_aplic_msi_wready := o_axi4.out.head._1.w.ready
    // 输出信号
    i_axi4.in.head._1.ar.ready := u_aplic.io.o_aplic_mmcr_arready
    i_axi4.in.head._1.aw.ready := u_aplic.io.o_aplic_mmcr_awready
    i_axi4.in.head._1.b.bits.id := u_aplic.io.o_aplic_mmcr_bid
    i_axi4.in.head._1.b.bits.resp := u_aplic.io.o_aplic_mmcr_bresp
    i_axi4.in.head._1.b.valid := u_aplic.io.o_aplic_mmcr_bvalid
    i_axi4.in.head._1.r.bits.data := u_aplic.io.o_aplic_mmcr_rdata
    i_axi4.in.head._1.r.bits.id := u_aplic.io.o_aplic_mmcr_rid
    i_axi4.in.head._1.r.bits.last := u_aplic.io.o_aplic_mmcr_rlast
    i_axi4.in.head._1.r.bits.resp := u_aplic.io.o_aplic_mmcr_rresp
    i_axi4.in.head._1.r.valid := u_aplic.io.o_aplic_mmcr_rvalid
    i_axi4.in.head._1.w.ready := u_aplic.io.o_aplic_mmcr_wready
    // MSI 子接口输出
    o_axi4.out.head._1.aw.bits.addr := u_aplic.io.o_aplic_msi_awaddr  // addr is cut, TODO
    o_axi4.out.head._1.aw.bits.id := u_aplic.io.o_aplic_msi_awid
    o_axi4.out.head._1.aw.bits.prot := u_aplic.io.o_aplic_msi_awprot
    o_axi4.out.head._1.aw.bits.size := 2.U
    o_axi4.out.head._1.aw.valid := u_aplic.io.o_aplic_msi_awvalid
    o_axi4.out.head._1.b.ready := u_aplic.io.o_aplic_msi_bready
    o_axi4.out.head._1.w.bits.data := u_aplic.io.o_aplic_msi_wdata
    o_axi4.out.head._1.w.bits.strb := u_aplic.io.o_aplic_msi_wstrb
    o_axi4.out.head._1.w.bits.last := true.B
    o_axi4.out.head._1.w.valid := u_aplic.io.o_aplic_msi_wvalid
    u_aplic.io.i_aplic_wire_int_vld := i_aplic_wire_int_vld
    u_aplic.io.i_dft_icg_scan_en := i_dft_icg_scan_en
    u_aplic.io.aclk := clock
    u_aplic.io.arst_n := (!reset.asBool).asAsyncReset
  }
}

class apic_aplic_top extends BlackBox {
  val io = IO(new Bundle {
    // 时钟与复位
    val aclk = Input(Clock())
    val arst_n = Input(Reset())
    // 读地址通道 (AR)
    // val i_aplic_mmcr = IO(Input(AXI4BundleAW()))
    val i_aplic_mmcr_araddr = Input(UInt(40.W)) // 39:0
    val i_aplic_mmcr_arburst = Input(UInt(2.W)) // 1:0
    val i_aplic_mmcr_arcache = Input(UInt(4.W)) // 3:0
    val i_aplic_mmcr_arid = Input(UInt(16.W)) // 15:0
    val i_aplic_mmcr_arlen = Input(UInt(8.W)) // 7:0
    val i_aplic_mmcr_arlock = Input(Bool()) // 单bit
    val i_aplic_mmcr_arprot = Input(UInt(3.W)) // 2:0
    val i_aplic_mmcr_arsize = Input(UInt(3.W)) // 2:0
    val i_aplic_mmcr_arvalid = Input(Bool()) // 单bit

    // 写地址通道 (AW)
    val i_aplic_mmcr_awaddr = Input(UInt(40.W)) // 39:0
    val i_aplic_mmcr_awburst = Input(UInt(2.W)) // 1:0
    val i_aplic_mmcr_awcache = Input(UInt(4.W)) // 3:0
    val i_aplic_mmcr_awid = Input(UInt(16.W)) // 15:0
    val i_aplic_mmcr_awlen = Input(UInt(8.W)) // 7:0
    val i_aplic_mmcr_awlock = Input(Bool()) // 单bit
    val i_aplic_mmcr_awprot = Input(UInt(3.W)) // 2:0
    val i_aplic_mmcr_awsize = Input(UInt(3.W)) // 2:0
    val i_aplic_mmcr_awvalid = Input(Bool()) // 单bit

    // 响应与数据通道
    val i_aplic_mmcr_bready = Input(Bool()) // 单bit
    val i_aplic_mmcr_rready = Input(Bool()) // 单bit
    val i_aplic_mmcr_wdata = Input(UInt(64.W)) // 63:0
    val i_aplic_mmcr_wlast = Input(Bool()) // 7:0
    val i_aplic_mmcr_wstrb = Input(UInt(8.W)) // 7:0
    val i_aplic_mmcr_wvalid = Input(Bool()) // 单bit

    // MSI 子接口输入
    val i_aplic_msi_awready = Input(Bool()) // 单bit
    val i_aplic_msi_bid = Input(UInt(16.W)) // 15:0
    val i_aplic_msi_bresp = Input(UInt(2.W)) // 1:0
    val i_aplic_msi_bvalid = Input(Bool()) // 单bit
    val i_aplic_msi_wready = Input(Bool()) // 单bit
    val i_aplic_wire_int_vld = Input(UInt(512.W)) // 255:0
    val i_dft_icg_scan_en = Input(Bool()) // 单bit

    // 输出信号
    val o_aplic_mmcr_arready = Output(Bool()) // 单bit
    val o_aplic_mmcr_awready = Output(Bool()) // 单bit
    val o_aplic_mmcr_bid = Output(UInt(16.W)) // 15:0
    val o_aplic_mmcr_bresp = Output(UInt(2.W)) // 1:0
    val o_aplic_mmcr_bvalid = Output(Bool()) // 单bit
    val o_aplic_mmcr_rdata = Output(UInt(64.W)) // 63:0
    val o_aplic_mmcr_rid = Output(UInt(16.W)) // 15:0
    val o_aplic_mmcr_rlast = Output(Bool()) // 单bit
    val o_aplic_mmcr_rresp = Output(UInt(2.W)) // 1:0
    val o_aplic_mmcr_rvalid = Output(Bool()) // 单bit
    val o_aplic_mmcr_wready = Output(Bool()) // 单bit

    // MSI 子接口输出
    val o_aplic_msi_awaddr = Output(UInt(40.W)) // 39:0
    val o_aplic_msi_awid = Output(UInt(16.W)) // 15:0
    val o_aplic_msi_awprot = Output(UInt(3.W)) // 2:0
    val o_aplic_msi_awvalid = Output(Bool()) // 单bit
    val o_aplic_msi_bready = Output(Bool()) // 31:0
    val o_aplic_msi_wdata = Output(UInt(32.W)) // 31:0
    val o_aplic_msi_wstrb = Output(UInt(4.W)) // 3:0
    val o_aplic_msi_wvalid = Output(Bool()) // 单bit
  })
}

