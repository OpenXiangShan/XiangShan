/***************************************************************************************
 * Copyright (c) 2026 Beijing Institute of Open Source Chip (BOSC)
 *
 * XiangShan is licensed under Mulan PSL v2.
 ***************************************************************************************/

package top

import chisel3._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._

/** Raw port description of bosc-iommu-v2/iommu_wrap.sv. */
private class IommuWrapIO extends Bundle {
  val iommu_clk = Input(Clock())
  val iommu_rstn = Input(Bool())

  val iommu_penable_i = Input(Bool())
  val iommu_pwrite_i = Input(Bool())
  val iommu_paddr_i = Input(UInt(32.W))
  val iommu_psel_i = Input(Bool())
  val iommu_pwdata_i = Input(UInt(32.W))
  val iommu_prdata_o = Output(UInt(32.W))
  val iommu_pready_o = Output(Bool())
  val iommu_pslverr_o = Output(Bool())

  val iommu_slv_awid = Input(UInt(10.W))
  val iommu_slv_awaddr = Input(UInt(40.W))
  val iommu_slv_awlen = Input(UInt(8.W))
  val iommu_slv_awsize = Input(UInt(3.W))
  val iommu_slv_awburst = Input(UInt(2.W))
  val iommu_slv_awlock = Input(Bool())
  val iommu_slv_awcache = Input(UInt(4.W))
  val iommu_slv_awprot = Input(UInt(3.W))
  val iommu_slv_awregion = Input(UInt(4.W))
  val iommu_slv_awuser = Input(UInt(54.W))
  val iommu_slv_awvalid = Input(Bool())
  val iommu_slv_awsnoop = Input(UInt(4.W))
  val iommu_slv_awdomain = Input(UInt(2.W))
  val iommu_slv_awidunq = Input(Bool())
  val iommu_slv_awatop = Input(UInt(6.W))
  val iommu_slv_awloop = Input(Bool())

  val iommu_slv_arid = Input(UInt(10.W))
  val iommu_slv_araddr = Input(UInt(40.W))
  val iommu_slv_arlen = Input(UInt(8.W))
  val iommu_slv_arsize = Input(UInt(3.W))
  val iommu_slv_arburst = Input(UInt(2.W))
  val iommu_slv_arlock = Input(Bool())
  val iommu_slv_arcache = Input(UInt(4.W))
  val iommu_slv_arprot = Input(UInt(3.W))
  val iommu_slv_arregion = Input(UInt(4.W))
  val iommu_slv_aruser = Input(UInt(54.W))
  val iommu_slv_arvalid = Input(Bool())
  val iommu_slv_arsnoop = Input(UInt(4.W))
  val iommu_slv_ardomain = Input(UInt(2.W))
  val iommu_slv_aridunq = Input(Bool())
  val iommu_slv_arloop = Input(Bool())

  val iommu_slv_wdata = Input(UInt(256.W))
  val iommu_slv_wstrb = Input(UInt(32.W))
  val iommu_slv_wlast = Input(Bool())
  val iommu_slv_wvalid = Input(Bool())
  val iommu_slv_wuser = Input(UInt(8.W))
  val iommu_slv_rready = Input(Bool())
  val iommu_slv_bready = Input(Bool())
  val iommu_slv_awqos = Input(UInt(4.W))
  val iommu_slv_arqos = Input(UInt(4.W))
  val iommu_slv_awready = Output(Bool())
  val iommu_slv_arready = Output(Bool())
  val iommu_slv_wready = Output(Bool())
  val iommu_slv_rid = Output(UInt(10.W))
  val iommu_slv_rdata = Output(UInt(256.W))
  val iommu_slv_rresp = Output(UInt(2.W))
  val iommu_slv_rlast = Output(Bool())
  val iommu_slv_rvalid = Output(Bool())
  val iommu_slv_ruser = Output(UInt(8.W))
  val iommu_slv_ridunq = Output(Bool())
  val iommu_slv_rloop = Output(Bool())
  val iommu_slv_bid = Output(UInt(10.W))
  val iommu_slv_bvalid = Output(Bool())
  val iommu_slv_bresp = Output(UInt(2.W))
  val iommu_slv_buser = Output(UInt(8.W))
  val iommu_slv_bidunq = Output(Bool())
  val iommu_slv_bloop = Output(Bool())

  val iommu_mst_awid = Output(UInt(10.W))
  val iommu_mst_awaddr = Output(UInt(40.W))
  val iommu_mst_awlen = Output(UInt(8.W))
  val iommu_mst_awsize = Output(UInt(3.W))
  val iommu_mst_awburst = Output(UInt(2.W))
  val iommu_mst_awlock = Output(Bool())
  val iommu_mst_awcache = Output(UInt(4.W))
  val iommu_mst_awprot = Output(UInt(3.W))
  val iommu_mst_awregion = Output(UInt(4.W))
  val iommu_mst_awuser = Output(UInt(8.W))
  val iommu_mst_awqos = Output(UInt(4.W))
  val iommu_mst_awvalid = Output(Bool())
  val iommu_mst_awsnoop = Output(UInt(4.W))
  val iommu_mst_awdomain = Output(UInt(2.W))
  val iommu_mst_awidunq = Output(Bool())
  val iommu_mst_awatop = Output(UInt(6.W))
  val iommu_mst_awloop = Output(Bool())
  val iommu_mst_arid = Output(UInt(10.W))
  val iommu_mst_araddr = Output(UInt(40.W))
  val iommu_mst_arlen = Output(UInt(8.W))
  val iommu_mst_arsize = Output(UInt(3.W))
  val iommu_mst_arburst = Output(UInt(2.W))
  val iommu_mst_arlock = Output(Bool())
  val iommu_mst_arcache = Output(UInt(4.W))
  val iommu_mst_arprot = Output(UInt(3.W))
  val iommu_mst_arregion = Output(UInt(4.W))
  val iommu_mst_aruser = Output(UInt(8.W))
  val iommu_mst_arqos = Output(UInt(4.W))
  val iommu_mst_arvalid = Output(Bool())
  val iommu_mst_arsnoop = Output(UInt(4.W))
  val iommu_mst_ardomain = Output(UInt(2.W))
  val iommu_mst_aridunq = Output(Bool())
  val iommu_mst_arloop = Output(Bool())
  val iommu_mst_wdata = Output(UInt(256.W))
  val iommu_mst_wstrb = Output(UInt(32.W))
  val iommu_mst_wlast = Output(Bool())
  val iommu_mst_wvalid = Output(Bool())
  val iommu_mst_wuser = Output(UInt(8.W))
  val iommu_mst_rready = Output(Bool())
  val iommu_mst_bready = Output(Bool())
  val iommu_mst_awready = Input(Bool())
  val iommu_mst_arready = Input(Bool())
  val iommu_mst_wready = Input(Bool())
  val iommu_mst_rid = Input(UInt(10.W))
  val iommu_mst_rdata = Input(UInt(256.W))
  val iommu_mst_rresp = Input(UInt(2.W))
  val iommu_mst_rlast = Input(Bool())
  val iommu_mst_rvalid = Input(Bool())
  val iommu_mst_ruser = Input(UInt(8.W))
  val iommu_mst_ridunq = Input(Bool())
  val iommu_mst_rloop = Input(Bool())
  val iommu_mst_bid = Input(UInt(10.W))
  val iommu_mst_bvalid = Input(Bool())
  val iommu_mst_bresp = Input(UInt(2.W))
  val iommu_mst_buser = Input(UInt(8.W))
  val iommu_mst_bidunq = Input(Bool())
  val iommu_mst_bloop = Input(Bool())

  val iommu_tr_rvalid_i = Input(Bool())
  val iommu_tr_rready_o = Output(Bool())
  val iommu_tr_rdata_i = Input(UInt(64.W))
  val iommu_tr_rstrb_i = Input(UInt(8.W))
  val iommu_tr_rkeep_i = Input(UInt(8.W))
  val iommu_tr_rlast_i = Input(Bool())
  val iommu_tr_rid_i = Input(UInt(4.W))
  val iommu_tr_tvalid_o = Output(Bool())
  val iommu_tr_tready_i = Input(Bool())
  val iommu_tr_tdata_o = Output(UInt(64.W))
  val iommu_tr_tstrb_o = Output(UInt(8.W))
  val iommu_tr_tkeep_o = Output(UInt(8.W))
  val iommu_tr_tlast_o = Output(Bool())
  val iommu_tr_tid_o = Output(UInt(4.W))

  val iommu_ds_acvalid = Input(Bool())
  val iommu_ds_acready = Output(Bool())
  val iommu_ds_acaddr = Input(UInt(52.W))
  val iommu_ds_acvmidext = Input(UInt(4.W))
  val iommu_ds_acsnoop = Input(UInt(4.W))
  val iommu_ds_acprot = Input(UInt(3.W))
  val iommu_ds_crvalid = Output(Bool())
  val iommu_ds_crready = Input(Bool())
  val iommu_ds_crresp = Output(UInt(5.W))
  val iommu_ds_ardomain = Output(UInt(2.W))
  val iommu_ds_arsnoop = Output(UInt(4.W))
  val iommu_ds_arbar = Output(UInt(2.W))
  val iommu_ds_awatop = Output(UInt(6.W))
  val iommu_ds_awdomain = Output(UInt(2.W))
  val iommu_ds_awid = Output(UInt(6.W))
  val iommu_ds_awaddr = Output(UInt(56.W))
  val iommu_ds_awlen = Output(UInt(8.W))
  val iommu_ds_awsize = Output(UInt(3.W))
  val iommu_ds_awburst = Output(UInt(2.W))
  val iommu_ds_awlock = Output(Bool())
  val iommu_ds_awcache = Output(UInt(4.W))
  val iommu_ds_awprot = Output(UInt(3.W))
  val iommu_ds_awqos = Output(UInt(4.W))
  val iommu_ds_awvalid = Output(Bool())
  val iommu_ds_arid = Output(UInt(6.W))
  val iommu_ds_araddr = Output(UInt(56.W))
  val iommu_ds_arlen = Output(UInt(8.W))
  val iommu_ds_arsize = Output(UInt(3.W))
  val iommu_ds_arburst = Output(UInt(2.W))
  val iommu_ds_arlock = Output(Bool())
  val iommu_ds_arcache = Output(UInt(4.W))
  val iommu_ds_arprot = Output(UInt(3.W))
  val iommu_ds_arqos = Output(UInt(4.W))
  val iommu_ds_arvalid = Output(Bool())
  val iommu_ds_wdata = Output(UInt(256.W))
  val iommu_ds_wstrb = Output(UInt(32.W))
  val iommu_ds_wlast = Output(Bool())
  val iommu_ds_wvalid = Output(Bool())
  val iommu_ds_rready = Output(Bool())
  val iommu_ds_bready = Output(Bool())
  val iommu_ds_awready = Input(Bool())
  val iommu_ds_arready = Input(Bool())
  val iommu_ds_wready = Input(Bool())
  val iommu_ds_rid = Input(UInt(6.W))
  val iommu_ds_rdata = Input(UInt(256.W))
  val iommu_ds_rresp = Input(UInt(2.W))
  val iommu_ds_rlast = Input(Bool())
  val iommu_ds_rvalid = Input(Bool())
  val iommu_ds_bid = Input(UInt(6.W))
  val iommu_ds_bvalid = Input(Bool())
  val iommu_ds_bresp = Input(UInt(2.W))
}

private class IommuWrapBlackBox extends ExtModule {
  override def desiredName: String = "iommu_wrap"
  val io = FlatIO(new IommuWrapIO)
}

/**
  * Diplomacy adapter around the fixed-width IOMMU RTL.
  *
  * The DMA-facing port is 40-bit/10-bit-ID AXI4, the translated master is
  * 40-bit/10-bit-ID AXI4, and the page-table-walk (ds) master is
  * 56-bit/6-bit-ID AXI4. Both data paths are 256 bits wide.
  */
class IommuLazy(implicit p: Parameters) extends LazyModule {
  private val allMemory = Seq(AddressSet(0x0L, 0xffffffffffffL))

  val slaveNode = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    slaves = Seq(AXI4SlaveParameters(
      address = allMemory,
      regionType = RegionType.UNCACHED,
      executable = false,
      supportsRead = TransferSizes(1, 32),
      supportsWrite = TransferSizes(1, 32),
      interleavedId = Some(0))),
    beatBytes = 32)))

  val masterNode = AXI4MasterNode(Seq(AXI4MasterPortParameters(Seq(
    AXI4MasterParameters(name = "iommu-translated", id = IdRange(0, 1 << 10))))))

  val dsMasterNode = AXI4MasterNode(Seq(AXI4MasterPortParameters(Seq(
    AXI4MasterParameters(name = "iommu-page-table-walk", id = IdRange(0, 1 << 6))))))

  lazy val module = new Imp
  class Imp extends LazyModuleImp(this) {
    val apb = IO(Flipped(new freechips.rocketchip.amba.apb.APBBundle(
      freechips.rocketchip.amba.apb.APBBundleParameters(
        addrBits = 32, dataBits = 32, requestFields = Nil, responseFields = Nil))))

    private val bb = Module(new IommuWrapBlackBox)
    private val (slave, _) = slaveNode.in.head
    private val (master, _) = masterNode.out.head
    private val (ds, _) = dsMasterNode.out.head

    require(slave.params.dataBits == 256 && slave.params.idBits == 10)
    require(master.params.dataBits == 256 && master.params.idBits == 10)
    require(ds.params.dataBits == 256 && ds.params.idBits == 6)

    bb.io.iommu_clk := clock
    bb.io.iommu_rstn := !reset.asBool

    bb.io.iommu_penable_i := apb.penable
    bb.io.iommu_pwrite_i := apb.pwrite
    bb.io.iommu_paddr_i := apb.paddr
    bb.io.iommu_psel_i := apb.psel
    bb.io.iommu_pwdata_i := apb.pwdata
    apb.prdata := bb.io.iommu_prdata_o
    apb.pready := bb.io.iommu_pready_o
    apb.pslverr := bb.io.iommu_pslverr_o

    // DMA -> IOMMU slave. The current DMA uses ID 0 and has no DID/PID
    // sideband, so the 54-bit IOMMU user field is intentionally zero.
    bb.io.iommu_slv_awid := slave.aw.bits.id
    bb.io.iommu_slv_awaddr := slave.aw.bits.addr(39, 0)
    bb.io.iommu_slv_awlen := slave.aw.bits.len
    bb.io.iommu_slv_awsize := slave.aw.bits.size
    bb.io.iommu_slv_awburst := slave.aw.bits.burst
    bb.io.iommu_slv_awlock := slave.aw.bits.lock.asBool
    bb.io.iommu_slv_awcache := slave.aw.bits.cache
    bb.io.iommu_slv_awprot := slave.aw.bits.prot
    bb.io.iommu_slv_awregion := 0.U
    bb.io.iommu_slv_awuser := 0.U
    bb.io.iommu_slv_awvalid := slave.aw.valid
    bb.io.iommu_slv_awsnoop := 0.U
    bb.io.iommu_slv_awdomain := 0.U
    bb.io.iommu_slv_awidunq := false.B
    bb.io.iommu_slv_awatop := 0.U
    bb.io.iommu_slv_awloop := false.B
    bb.io.iommu_slv_awqos := slave.aw.bits.qos
    slave.aw.ready := bb.io.iommu_slv_awready

    bb.io.iommu_slv_arid := slave.ar.bits.id
    bb.io.iommu_slv_araddr := slave.ar.bits.addr(39, 0)
    bb.io.iommu_slv_arlen := slave.ar.bits.len
    bb.io.iommu_slv_arsize := slave.ar.bits.size
    bb.io.iommu_slv_arburst := slave.ar.bits.burst
    bb.io.iommu_slv_arlock := slave.ar.bits.lock.asBool
    bb.io.iommu_slv_arcache := slave.ar.bits.cache
    bb.io.iommu_slv_arprot := slave.ar.bits.prot
    bb.io.iommu_slv_arregion := 0.U
    bb.io.iommu_slv_aruser := 0.U
    bb.io.iommu_slv_arvalid := slave.ar.valid
    bb.io.iommu_slv_arsnoop := 0.U
    bb.io.iommu_slv_ardomain := 0.U
    bb.io.iommu_slv_aridunq := false.B
    bb.io.iommu_slv_arloop := false.B
    bb.io.iommu_slv_arqos := slave.ar.bits.qos
    slave.ar.ready := bb.io.iommu_slv_arready

    bb.io.iommu_slv_wdata := slave.w.bits.data
    bb.io.iommu_slv_wstrb := slave.w.bits.strb
    bb.io.iommu_slv_wlast := slave.w.bits.last
    bb.io.iommu_slv_wvalid := slave.w.valid
    bb.io.iommu_slv_wuser := 0.U
    slave.w.ready := bb.io.iommu_slv_wready

    slave.r.valid := bb.io.iommu_slv_rvalid
    slave.r.bits.id := bb.io.iommu_slv_rid
    slave.r.bits.data := bb.io.iommu_slv_rdata
    slave.r.bits.resp := bb.io.iommu_slv_rresp
    slave.r.bits.last := bb.io.iommu_slv_rlast
    slave.r.bits.user := 0.U.asTypeOf(slave.r.bits.user)
    slave.r.bits.echo := 0.U.asTypeOf(slave.r.bits.echo)
    bb.io.iommu_slv_rready := slave.r.ready

    slave.b.valid := bb.io.iommu_slv_bvalid
    slave.b.bits.id := bb.io.iommu_slv_bid
    slave.b.bits.resp := bb.io.iommu_slv_bresp
    slave.b.bits.user := 0.U.asTypeOf(slave.b.bits.user)
    slave.b.bits.echo := 0.U.asTypeOf(slave.b.bits.echo)
    bb.io.iommu_slv_bready := slave.b.ready

    when(slave.aw.valid) { assert(slave.aw.bits.addr(47, 40) === 0.U) }
    when(slave.ar.valid) { assert(slave.ar.bits.addr(47, 40) === 0.U) }

    // IOMMU translated AXI master -> IOPMP.
    master.aw.valid := bb.io.iommu_mst_awvalid
    master.aw.bits.id := bb.io.iommu_mst_awid
    master.aw.bits.addr := bb.io.iommu_mst_awaddr
    master.aw.bits.len := bb.io.iommu_mst_awlen
    master.aw.bits.size := bb.io.iommu_mst_awsize
    master.aw.bits.burst := bb.io.iommu_mst_awburst
    master.aw.bits.lock := bb.io.iommu_mst_awlock
    master.aw.bits.cache := bb.io.iommu_mst_awcache
    master.aw.bits.prot := bb.io.iommu_mst_awprot
    master.aw.bits.qos := bb.io.iommu_mst_awqos
    master.aw.bits.user := 0.U.asTypeOf(master.aw.bits.user)
    master.aw.bits.echo := 0.U.asTypeOf(master.aw.bits.echo)
    bb.io.iommu_mst_awready := master.aw.ready

    master.ar.valid := bb.io.iommu_mst_arvalid
    master.ar.bits.id := bb.io.iommu_mst_arid
    master.ar.bits.addr := bb.io.iommu_mst_araddr
    master.ar.bits.len := bb.io.iommu_mst_arlen
    master.ar.bits.size := bb.io.iommu_mst_arsize
    master.ar.bits.burst := bb.io.iommu_mst_arburst
    master.ar.bits.lock := bb.io.iommu_mst_arlock
    master.ar.bits.cache := bb.io.iommu_mst_arcache
    master.ar.bits.prot := bb.io.iommu_mst_arprot
    master.ar.bits.qos := bb.io.iommu_mst_arqos
    master.ar.bits.user := 0.U.asTypeOf(master.ar.bits.user)
    master.ar.bits.echo := 0.U.asTypeOf(master.ar.bits.echo)
    bb.io.iommu_mst_arready := master.ar.ready

    master.w.valid := bb.io.iommu_mst_wvalid
    master.w.bits.data := bb.io.iommu_mst_wdata
    master.w.bits.strb := bb.io.iommu_mst_wstrb
    master.w.bits.last := bb.io.iommu_mst_wlast
    master.w.bits.user := 0.U.asTypeOf(master.w.bits.user)
    bb.io.iommu_mst_wready := master.w.ready

    bb.io.iommu_mst_rid := master.r.bits.id
    bb.io.iommu_mst_rdata := master.r.bits.data
    bb.io.iommu_mst_rresp := master.r.bits.resp
    bb.io.iommu_mst_rlast := master.r.bits.last
    bb.io.iommu_mst_rvalid := master.r.valid
    bb.io.iommu_mst_ruser := 0.U
    bb.io.iommu_mst_ridunq := false.B
    bb.io.iommu_mst_rloop := false.B
    master.r.ready := bb.io.iommu_mst_rready

    bb.io.iommu_mst_bid := master.b.bits.id
    bb.io.iommu_mst_bvalid := master.b.valid
    bb.io.iommu_mst_bresp := master.b.bits.resp
    bb.io.iommu_mst_buser := 0.U
    bb.io.iommu_mst_bidunq := false.B
    bb.io.iommu_mst_bloop := false.B
    master.b.ready := bb.io.iommu_mst_bready

    // IOMMU downstream/page-table-walk AXI master -> memory crossbar.
    ds.aw.valid := bb.io.iommu_ds_awvalid
    ds.aw.bits.id := bb.io.iommu_ds_awid
    ds.aw.bits.addr := bb.io.iommu_ds_awaddr(47, 0)
    ds.aw.bits.len := bb.io.iommu_ds_awlen
    ds.aw.bits.size := bb.io.iommu_ds_awsize
    ds.aw.bits.burst := bb.io.iommu_ds_awburst
    ds.aw.bits.lock := bb.io.iommu_ds_awlock
    ds.aw.bits.cache := bb.io.iommu_ds_awcache
    ds.aw.bits.prot := bb.io.iommu_ds_awprot
    ds.aw.bits.qos := bb.io.iommu_ds_awqos
    ds.aw.bits.user := 0.U.asTypeOf(ds.aw.bits.user)
    ds.aw.bits.echo := 0.U.asTypeOf(ds.aw.bits.echo)
    bb.io.iommu_ds_awready := ds.aw.ready

    ds.ar.valid := bb.io.iommu_ds_arvalid
    ds.ar.bits.id := bb.io.iommu_ds_arid
    ds.ar.bits.addr := bb.io.iommu_ds_araddr(47, 0)
    ds.ar.bits.len := bb.io.iommu_ds_arlen
    ds.ar.bits.size := bb.io.iommu_ds_arsize
    ds.ar.bits.burst := bb.io.iommu_ds_arburst
    ds.ar.bits.lock := bb.io.iommu_ds_arlock
    ds.ar.bits.cache := bb.io.iommu_ds_arcache
    ds.ar.bits.prot := bb.io.iommu_ds_arprot
    ds.ar.bits.qos := bb.io.iommu_ds_arqos
    ds.ar.bits.user := 0.U.asTypeOf(ds.ar.bits.user)
    ds.ar.bits.echo := 0.U.asTypeOf(ds.ar.bits.echo)
    bb.io.iommu_ds_arready := ds.ar.ready

    ds.w.valid := bb.io.iommu_ds_wvalid
    ds.w.bits.data := bb.io.iommu_ds_wdata
    ds.w.bits.strb := bb.io.iommu_ds_wstrb
    ds.w.bits.last := bb.io.iommu_ds_wlast
    ds.w.bits.user := 0.U.asTypeOf(ds.w.bits.user)
    bb.io.iommu_ds_wready := ds.w.ready

    bb.io.iommu_ds_rid := ds.r.bits.id
    bb.io.iommu_ds_rdata := ds.r.bits.data
    bb.io.iommu_ds_rresp := ds.r.bits.resp
    bb.io.iommu_ds_rlast := ds.r.bits.last
    bb.io.iommu_ds_rvalid := ds.r.valid
    ds.r.ready := bb.io.iommu_ds_rready

    bb.io.iommu_ds_bid := ds.b.bits.id
    bb.io.iommu_ds_bvalid := ds.b.valid
    bb.io.iommu_ds_bresp := ds.b.bits.resp
    ds.b.ready := bb.io.iommu_ds_bready

    when(bb.io.iommu_ds_awvalid) { assert(bb.io.iommu_ds_awaddr(55, 48) === 0.U) }
    when(bb.io.iommu_ds_arvalid) { assert(bb.io.iommu_ds_araddr(55, 48) === 0.U) }

    // The simulation fabric is AXI4, not ACE. DVM coherency and trace are
    // disabled until a real ACE/trace adapter is provided.
    bb.io.iommu_ds_acvalid := false.B
    bb.io.iommu_ds_acaddr := 0.U
    bb.io.iommu_ds_acvmidext := 0.U
    bb.io.iommu_ds_acsnoop := 0.U
    bb.io.iommu_ds_acprot := 0.U
    bb.io.iommu_ds_crready := true.B

    bb.io.iommu_tr_rvalid_i := false.B
    bb.io.iommu_tr_rdata_i := 0.U
    bb.io.iommu_tr_rstrb_i := 0.U
    bb.io.iommu_tr_rkeep_i := 0.U
    bb.io.iommu_tr_rlast_i := false.B
    bb.io.iommu_tr_rid_i := 0.U
    bb.io.iommu_tr_tready_i := true.B
  }
}
