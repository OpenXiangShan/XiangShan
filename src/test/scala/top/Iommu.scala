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

private class IommuApbIO extends Bundle {
  val penable_i = Input(Bool())
  val pwrite_i = Input(Bool())
  val paddr_i = Input(UInt(32.W))
  val psel_i = Input(Bool())
  val pwdata_i = Input(UInt(32.W))
  val prdata_o = Output(UInt(32.W))
  val pready_o = Output(Bool())
  val pslverr_o = Output(Bool())
}

private class IommuSlaveIO extends Bundle {
  val awid = Input(UInt(10.W))
  val awaddr = Input(UInt(40.W))
  val awlen = Input(UInt(8.W))
  val awsize = Input(UInt(3.W))
  val awburst = Input(UInt(2.W))
  val awlock = Input(Bool())
  val awcache = Input(UInt(4.W))
  val awprot = Input(UInt(3.W))
  val awregion = Input(UInt(4.W))
  val awuser = Input(UInt(54.W))
  val awvalid = Input(Bool())
  val awsnoop = Input(UInt(4.W))
  val awdomain = Input(UInt(2.W))
  val awidunq = Input(Bool())
  val awatop = Input(UInt(6.W))
  val awloop = Input(Bool())
  val arid = Input(UInt(10.W))
  val araddr = Input(UInt(40.W))
  val arlen = Input(UInt(8.W))
  val arsize = Input(UInt(3.W))
  val arburst = Input(UInt(2.W))
  val arlock = Input(Bool())
  val arcache = Input(UInt(4.W))
  val arprot = Input(UInt(3.W))
  val arregion = Input(UInt(4.W))
  val aruser = Input(UInt(54.W))
  val arvalid = Input(Bool())
  val arsnoop = Input(UInt(4.W))
  val ardomain = Input(UInt(2.W))
  val aridunq = Input(Bool())
  val arloop = Input(Bool())
  val wdata = Input(UInt(256.W))
  val wstrb = Input(UInt(32.W))
  val wlast = Input(Bool())
  val wvalid = Input(Bool())
  val wuser = Input(UInt(8.W))
  val rready = Input(Bool())
  val bready = Input(Bool())
  val awqos = Input(UInt(4.W))
  val arqos = Input(UInt(4.W))
  val awready = Output(Bool())
  val arready = Output(Bool())
  val wready = Output(Bool())
  val rid = Output(UInt(10.W))
  val rdata = Output(UInt(256.W))
  val rresp = Output(UInt(2.W))
  val rlast = Output(Bool())
  val rvalid = Output(Bool())
  val ruser = Output(UInt(8.W))
  val ridunq = Output(Bool())
  val rloop = Output(Bool())
  val bid = Output(UInt(10.W))
  val bvalid = Output(Bool())
  val bresp = Output(UInt(2.W))
  val buser = Output(UInt(8.W))
  val bidunq = Output(Bool())
  val bloop = Output(Bool())
}

private class IommuMasterIO extends Bundle {
  val awid = Output(UInt(10.W))
  val awaddr = Output(UInt(40.W))
  val awlen = Output(UInt(8.W))
  val awsize = Output(UInt(3.W))
  val awburst = Output(UInt(2.W))
  val awlock = Output(Bool())
  val awcache = Output(UInt(4.W))
  val awprot = Output(UInt(3.W))
  val awregion = Output(UInt(4.W))
  val awuser = Output(UInt(8.W))
  val awqos = Output(UInt(4.W))
  val awvalid = Output(Bool())
  val awsnoop = Output(UInt(4.W))
  val awdomain = Output(UInt(2.W))
  val awidunq = Output(Bool())
  val awatop = Output(UInt(6.W))
  val awloop = Output(Bool())
  val arid = Output(UInt(10.W))
  val araddr = Output(UInt(40.W))
  val arlen = Output(UInt(8.W))
  val arsize = Output(UInt(3.W))
  val arburst = Output(UInt(2.W))
  val arlock = Output(Bool())
  val arcache = Output(UInt(4.W))
  val arprot = Output(UInt(3.W))
  val arregion = Output(UInt(4.W))
  val aruser = Output(UInt(8.W))
  val arqos = Output(UInt(4.W))
  val arvalid = Output(Bool())
  val arsnoop = Output(UInt(4.W))
  val ardomain = Output(UInt(2.W))
  val aridunq = Output(Bool())
  val arloop = Output(Bool())
  val wdata = Output(UInt(256.W))
  val wstrb = Output(UInt(32.W))
  val wlast = Output(Bool())
  val wvalid = Output(Bool())
  val wuser = Output(UInt(8.W))
  val rready = Output(Bool())
  val bready = Output(Bool())
  val awready = Input(Bool())
  val arready = Input(Bool())
  val wready = Input(Bool())
  val rid = Input(UInt(10.W))
  val rdata = Input(UInt(256.W))
  val rresp = Input(UInt(2.W))
  val rlast = Input(Bool())
  val rvalid = Input(Bool())
  val ruser = Input(UInt(8.W))
  val ridunq = Input(Bool())
  val rloop = Input(Bool())
  val bid = Input(UInt(10.W))
  val bvalid = Input(Bool())
  val bresp = Input(UInt(2.W))
  val buser = Input(UInt(8.W))
  val bidunq = Input(Bool())
  val bloop = Input(Bool())
}

private class IommuTraceIO extends Bundle {
  val rvalid_i = Input(Bool())
  val rready_o = Output(Bool())
  val rdata_i = Input(UInt(64.W))
  val rstrb_i = Input(UInt(8.W))
  val rkeep_i = Input(UInt(8.W))
  val rlast_i = Input(Bool())
  val rid_i = Input(UInt(4.W))
  val tvalid_o = Output(Bool())
  val tready_i = Input(Bool())
  val tdata_o = Output(UInt(64.W))
  val tstrb_o = Output(UInt(8.W))
  val tkeep_o = Output(UInt(8.W))
  val tlast_o = Output(Bool())
  val tid_o = Output(UInt(4.W))
}

private class IommuDownstreamIO extends Bundle {
  val acvalid = Input(Bool())
  val acready = Output(Bool())
  val acaddr = Input(UInt(52.W))
  val acvmidext = Input(UInt(4.W))
  val acsnoop = Input(UInt(4.W))
  val acprot = Input(UInt(3.W))
  val crvalid = Output(Bool())
  val crready = Input(Bool())
  val crresp = Output(UInt(5.W))
  val ardomain = Output(UInt(2.W))
  val arsnoop = Output(UInt(4.W))
  val arbar = Output(UInt(2.W))
  val awatop = Output(UInt(6.W))
  val awdomain = Output(UInt(2.W))
  val awid = Output(UInt(6.W))
  val awaddr = Output(UInt(56.W))
  val awlen = Output(UInt(8.W))
  val awsize = Output(UInt(3.W))
  val awburst = Output(UInt(2.W))
  val awlock = Output(Bool())
  val awcache = Output(UInt(4.W))
  val awprot = Output(UInt(3.W))
  val awqos = Output(UInt(4.W))
  val awvalid = Output(Bool())
  val arid = Output(UInt(6.W))
  val araddr = Output(UInt(56.W))
  val arlen = Output(UInt(8.W))
  val arsize = Output(UInt(3.W))
  val arburst = Output(UInt(2.W))
  val arlock = Output(Bool())
  val arcache = Output(UInt(4.W))
  val arprot = Output(UInt(3.W))
  val arqos = Output(UInt(4.W))
  val arvalid = Output(Bool())
  val wdata = Output(UInt(256.W))
  val wstrb = Output(UInt(32.W))
  val wlast = Output(Bool())
  val wvalid = Output(Bool())
  val rready = Output(Bool())
  val bready = Output(Bool())
  val awready = Input(Bool())
  val arready = Input(Bool())
  val wready = Input(Bool())
  val rid = Input(UInt(6.W))
  val rdata = Input(UInt(256.W))
  val rresp = Input(UInt(2.W))
  val rlast = Input(Bool())
  val rvalid = Input(Bool())
  val bid = Input(UInt(6.W))
  val bvalid = Input(Bool())
  val bresp = Input(UInt(2.W))
}

/** Hierarchical port description of OpenIOMMU/iommu_wrap.sv. */
private class IommuWrapIO extends Bundle {
  val iommu_clk = Input(Clock())
  val iommu_rstn = Input(Bool())
  val iommu = new IommuApbIO
  val iommu_slv = new IommuSlaveIO
  val iommu_mst = new IommuMasterIO
  val iommu_tr = new IommuTraceIO
  val iommu_ds = new IommuDownstreamIO
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
  val slaveIdBits = 10
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

    require(slave.params.dataBits == 256 && slave.params.idBits == slaveIdBits)
    require(master.params.dataBits == 256 && master.params.idBits == 10)
    require(ds.params.dataBits == 256 && ds.params.idBits == 6)

    bb.io.iommu_clk := clock
    bb.io.iommu_rstn := !reset.asBool

    bb.io.iommu.penable_i := apb.penable
    bb.io.iommu.pwrite_i := apb.pwrite
    bb.io.iommu.paddr_i := apb.paddr
    bb.io.iommu.psel_i := apb.psel
    bb.io.iommu.pwdata_i := apb.pwdata
    apb.prdata := bb.io.iommu.prdata_o
    apb.pready := bb.io.iommu.pready_o
    apb.pslverr := bb.io.iommu.pslverr_o

    // DMA -> IOMMU slave. The current DMA uses ID 0 and has no DID/PID
    // sideband, so the 54-bit IOMMU user field is intentionally zero.
    bb.io.iommu_slv.awid := slave.aw.bits.id
    bb.io.iommu_slv.awaddr := slave.aw.bits.addr(39, 0)
    bb.io.iommu_slv.awlen := slave.aw.bits.len
    bb.io.iommu_slv.awsize := slave.aw.bits.size
    bb.io.iommu_slv.awburst := slave.aw.bits.burst
    bb.io.iommu_slv.awlock := slave.aw.bits.lock.asBool
    bb.io.iommu_slv.awcache := slave.aw.bits.cache
    bb.io.iommu_slv.awprot := slave.aw.bits.prot
    bb.io.iommu_slv.awregion := 0.U
    bb.io.iommu_slv.awuser := 0.U
    bb.io.iommu_slv.awvalid := slave.aw.valid
    bb.io.iommu_slv.awsnoop := 0.U
    bb.io.iommu_slv.awdomain := 0.U
    bb.io.iommu_slv.awidunq := false.B
    bb.io.iommu_slv.awatop := 0.U
    bb.io.iommu_slv.awloop := false.B
    bb.io.iommu_slv.awqos := slave.aw.bits.qos
    slave.aw.ready := bb.io.iommu_slv.awready

    bb.io.iommu_slv.arid := slave.ar.bits.id
    bb.io.iommu_slv.araddr := slave.ar.bits.addr(39, 0)
    bb.io.iommu_slv.arlen := slave.ar.bits.len
    bb.io.iommu_slv.arsize := slave.ar.bits.size
    bb.io.iommu_slv.arburst := slave.ar.bits.burst
    bb.io.iommu_slv.arlock := slave.ar.bits.lock.asBool
    bb.io.iommu_slv.arcache := slave.ar.bits.cache
    bb.io.iommu_slv.arprot := slave.ar.bits.prot
    bb.io.iommu_slv.arregion := 0.U
    bb.io.iommu_slv.aruser := 0.U
    bb.io.iommu_slv.arvalid := slave.ar.valid
    bb.io.iommu_slv.arsnoop := 0.U
    bb.io.iommu_slv.ardomain := 0.U
    bb.io.iommu_slv.aridunq := false.B
    bb.io.iommu_slv.arloop := false.B
    bb.io.iommu_slv.arqos := slave.ar.bits.qos
    slave.ar.ready := bb.io.iommu_slv.arready

    bb.io.iommu_slv.wdata := slave.w.bits.data
    bb.io.iommu_slv.wstrb := slave.w.bits.strb
    bb.io.iommu_slv.wlast := slave.w.bits.last
    bb.io.iommu_slv.wvalid := slave.w.valid
    bb.io.iommu_slv.wuser := 0.U
    slave.w.ready := bb.io.iommu_slv.wready

    slave.r.valid := bb.io.iommu_slv.rvalid
    slave.r.bits.id := bb.io.iommu_slv.rid
    slave.r.bits.data := bb.io.iommu_slv.rdata
    slave.r.bits.resp := bb.io.iommu_slv.rresp
    slave.r.bits.last := bb.io.iommu_slv.rlast
    slave.r.bits.user := 0.U.asTypeOf(slave.r.bits.user)
    slave.r.bits.echo := 0.U.asTypeOf(slave.r.bits.echo)
    bb.io.iommu_slv.rready := slave.r.ready

    slave.b.valid := bb.io.iommu_slv.bvalid
    slave.b.bits.id := bb.io.iommu_slv.bid
    slave.b.bits.resp := bb.io.iommu_slv.bresp
    slave.b.bits.user := 0.U.asTypeOf(slave.b.bits.user)
    slave.b.bits.echo := 0.U.asTypeOf(slave.b.bits.echo)
    bb.io.iommu_slv.bready := slave.b.ready

    when(slave.aw.valid) { assert(slave.aw.bits.addr(47, 40) === 0.U) }
    when(slave.ar.valid) { assert(slave.ar.bits.addr(47, 40) === 0.U) }

    // IOMMU translated AXI master -> IOPMP.
    master.aw.valid := bb.io.iommu_mst.awvalid
    master.aw.bits.id := bb.io.iommu_mst.awid
    master.aw.bits.addr := bb.io.iommu_mst.awaddr
    master.aw.bits.len := bb.io.iommu_mst.awlen
    master.aw.bits.size := bb.io.iommu_mst.awsize
    master.aw.bits.burst := bb.io.iommu_mst.awburst
    master.aw.bits.lock := bb.io.iommu_mst.awlock
    master.aw.bits.cache := bb.io.iommu_mst.awcache
    master.aw.bits.prot := bb.io.iommu_mst.awprot
    master.aw.bits.qos := bb.io.iommu_mst.awqos
    master.aw.bits.user := 0.U.asTypeOf(master.aw.bits.user)
    master.aw.bits.echo := 0.U.asTypeOf(master.aw.bits.echo)
    bb.io.iommu_mst.awready := master.aw.ready

    master.ar.valid := bb.io.iommu_mst.arvalid
    master.ar.bits.id := bb.io.iommu_mst.arid
    master.ar.bits.addr := bb.io.iommu_mst.araddr
    master.ar.bits.len := bb.io.iommu_mst.arlen
    master.ar.bits.size := bb.io.iommu_mst.arsize
    master.ar.bits.burst := bb.io.iommu_mst.arburst
    master.ar.bits.lock := bb.io.iommu_mst.arlock
    master.ar.bits.cache := bb.io.iommu_mst.arcache
    master.ar.bits.prot := bb.io.iommu_mst.arprot
    master.ar.bits.qos := bb.io.iommu_mst.arqos
    master.ar.bits.user := 0.U.asTypeOf(master.ar.bits.user)
    master.ar.bits.echo := 0.U.asTypeOf(master.ar.bits.echo)
    bb.io.iommu_mst.arready := master.ar.ready

    master.w.valid := bb.io.iommu_mst.wvalid
    master.w.bits.data := bb.io.iommu_mst.wdata
    master.w.bits.strb := bb.io.iommu_mst.wstrb
    master.w.bits.last := bb.io.iommu_mst.wlast
    master.w.bits.user := 0.U.asTypeOf(master.w.bits.user)
    bb.io.iommu_mst.wready := master.w.ready

    bb.io.iommu_mst.rid := master.r.bits.id
    bb.io.iommu_mst.rdata := master.r.bits.data
    bb.io.iommu_mst.rresp := master.r.bits.resp
    bb.io.iommu_mst.rlast := master.r.bits.last
    bb.io.iommu_mst.rvalid := master.r.valid
    bb.io.iommu_mst.ruser := 0.U
    bb.io.iommu_mst.ridunq := false.B
    bb.io.iommu_mst.rloop := false.B
    master.r.ready := bb.io.iommu_mst.rready

    bb.io.iommu_mst.bid := master.b.bits.id
    bb.io.iommu_mst.bvalid := master.b.valid
    bb.io.iommu_mst.bresp := master.b.bits.resp
    bb.io.iommu_mst.buser := 0.U
    bb.io.iommu_mst.bidunq := false.B
    bb.io.iommu_mst.bloop := false.B
    master.b.ready := bb.io.iommu_mst.bready

    // IOMMU downstream/page-table-walk AXI master -> memory crossbar.
    ds.aw.valid := bb.io.iommu_ds.awvalid
    ds.aw.bits.id := bb.io.iommu_ds.awid
    ds.aw.bits.addr := bb.io.iommu_ds.awaddr(47, 0)
    ds.aw.bits.len := bb.io.iommu_ds.awlen
    ds.aw.bits.size := bb.io.iommu_ds.awsize
    ds.aw.bits.burst := bb.io.iommu_ds.awburst
    ds.aw.bits.lock := bb.io.iommu_ds.awlock
    ds.aw.bits.cache := bb.io.iommu_ds.awcache
    ds.aw.bits.prot := bb.io.iommu_ds.awprot
    ds.aw.bits.qos := bb.io.iommu_ds.awqos
    ds.aw.bits.user := 0.U.asTypeOf(ds.aw.bits.user)
    ds.aw.bits.echo := 0.U.asTypeOf(ds.aw.bits.echo)
    bb.io.iommu_ds.awready := ds.aw.ready

    ds.ar.valid := bb.io.iommu_ds.arvalid
    ds.ar.bits.id := bb.io.iommu_ds.arid
    ds.ar.bits.addr := bb.io.iommu_ds.araddr(47, 0)
    ds.ar.bits.len := bb.io.iommu_ds.arlen
    ds.ar.bits.size := bb.io.iommu_ds.arsize
    ds.ar.bits.burst := bb.io.iommu_ds.arburst
    ds.ar.bits.lock := bb.io.iommu_ds.arlock
    ds.ar.bits.cache := bb.io.iommu_ds.arcache
    ds.ar.bits.prot := bb.io.iommu_ds.arprot
    ds.ar.bits.qos := bb.io.iommu_ds.arqos
    ds.ar.bits.user := 0.U.asTypeOf(ds.ar.bits.user)
    ds.ar.bits.echo := 0.U.asTypeOf(ds.ar.bits.echo)
    bb.io.iommu_ds.arready := ds.ar.ready

    ds.w.valid := bb.io.iommu_ds.wvalid
    ds.w.bits.data := bb.io.iommu_ds.wdata
    ds.w.bits.strb := bb.io.iommu_ds.wstrb
    ds.w.bits.last := bb.io.iommu_ds.wlast
    ds.w.bits.user := 0.U.asTypeOf(ds.w.bits.user)
    bb.io.iommu_ds.wready := ds.w.ready

    bb.io.iommu_ds.rid := ds.r.bits.id
    bb.io.iommu_ds.rdata := ds.r.bits.data
    bb.io.iommu_ds.rresp := ds.r.bits.resp
    bb.io.iommu_ds.rlast := ds.r.bits.last
    bb.io.iommu_ds.rvalid := ds.r.valid
    ds.r.ready := bb.io.iommu_ds.rready

    bb.io.iommu_ds.bid := ds.b.bits.id
    bb.io.iommu_ds.bvalid := ds.b.valid
    bb.io.iommu_ds.bresp := ds.b.bits.resp
    ds.b.ready := bb.io.iommu_ds.bready

    when(bb.io.iommu_ds.awvalid) { assert(bb.io.iommu_ds.awaddr(55, 48) === 0.U) }
    when(bb.io.iommu_ds.arvalid) { assert(bb.io.iommu_ds.araddr(55, 48) === 0.U) }

    // The simulation fabric is AXI4, not ACE. DVM coherency and trace are
    // disabled until a real ACE/trace adapter is provided.
    bb.io.iommu_ds.acvalid := false.B
    bb.io.iommu_ds.acaddr := 0.U
    bb.io.iommu_ds.acvmidext := 0.U
    bb.io.iommu_ds.acsnoop := 0.U
    bb.io.iommu_ds.acprot := 0.U
    bb.io.iommu_ds.crready := true.B

    bb.io.iommu_tr.rvalid_i := false.B
    bb.io.iommu_tr.rdata_i := 0.U
    bb.io.iommu_tr.rstrb_i := 0.U
    bb.io.iommu_tr.rkeep_i := 0.U
    bb.io.iommu_tr.rlast_i := false.B
    bb.io.iommu_tr.rid_i := 0.U
    bb.io.iommu_tr.tready_i := true.B
  }
}
