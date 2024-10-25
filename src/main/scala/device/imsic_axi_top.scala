/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2024 Institute of Computing Technology, Chinese Academy of Sciences
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
import chisel3.util._
import chisel3.experimental.dataview._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.tilelink._
import utils.{AXI4LiteBundle, VerilogAXI4LiteRecord}

class imsic_axi_top(
  AXI_ID_WIDTH: Int = 5,
  AXI_ADDR_WIDTH: Int = 32,
  NR_INTP_FILES: Int = 7,
  NR_HARTS: Int = 1,
  NR_SRC: Int = 256,
  SETIP_KEEP_CYCLES: Int = 8
) extends BlackBox(Map(
  "AXI_ID_WIDTH" -> AXI_ID_WIDTH,
  "AXI_ADDR_WIDTH" -> AXI_ADDR_WIDTH,
  "NR_INTP_FILES" -> NR_INTP_FILES,
  "NR_HARTS" -> NR_HARTS,
  "NR_SRC" -> NR_SRC,
  "SETIP_KEEP_CYCLES" -> SETIP_KEEP_CYCLES
)) with HasBlackBoxResource {
  private val NR_SRC_WIDTH = log2Ceil(NR_SRC)
  private val NR_HARTS_WIDTH = if (NR_HARTS == 1) 1 else log2Ceil(NR_HARTS)
  private val INTP_FILE_WIDTH = log2Ceil(NR_INTP_FILES)
  private val MSI_INFO_WIDTH = NR_HARTS_WIDTH + INTP_FILE_WIDTH + NR_SRC_WIDTH
  val io = IO(new Bundle {
    // crg
    val axi_clk = Input(Clock())
    val axi_rstn = Input(AsyncReset())
    val fifo_rstn = Input(AsyncReset())
    // bus to access the m interrupt file
    val m_s = Flipped(new VerilogAXI4LiteRecord(AXI_ADDR_WIDTH, 32, AXI_ID_WIDTH))
    // bus to access the s interrupt file
    val s_s = Flipped(new VerilogAXI4LiteRecord(AXI_ADDR_WIDTH, 32, AXI_ID_WIDTH))
    // imsic_csr_top
    val o_msi_info = Output(UInt(MSI_INFO_WIDTH.W))
    val o_msi_info_vld = Output(Bool())
  })
  addResource("/aia/src/rtl/imsic/imsic_axi_top.sv")
  addResource("/aia/src/rtl/imsic/imsic_axi2reg.sv")
  addResource("/aia/src/rtl/imsic/imsic_regmap.sv")
  addResource("/aia/src/rtl/imsic/common/generic_fifo_dc_gray.sv")
  addResource("/aia/src/rtl/imsic/common/generic_dpram.sv")
}

class imsic_bus_top(
  useTL: Boolean = false,
  baseAddress: (BigInt, BigInt), /* (M-mode, S/VS-mode) */
  maxHarts: Int = 512,
  AXI_ID_WIDTH: Int = 5,
  AXI_ADDR_WIDTH: Int = 32,
  NR_INTP_FILES: Int = 7,
  NR_HARTS: Int = 1,
  NR_SRC: Int = 256,
  SETIP_KEEP_CYCLES: Int = 8
)(implicit p: Parameters) extends LazyModule {
  private val NR_SRC_WIDTH = log2Ceil(NR_SRC)
  private val NR_HARTS_WIDTH = if (NR_HARTS == 1) 1 else log2Ceil(NR_HARTS)
  private val INTP_FILE_WIDTH = log2Ceil(NR_INTP_FILES)
  private val MSI_INFO_WIDTH = NR_HARTS_WIDTH + INTP_FILE_WIDTH + NR_SRC_WIDTH

  private val m_base = baseAddress._1;
  private val m_size = maxHarts * 0x1000;
  private val s_base = baseAddress._2;
  private val s_size = maxHarts * 0x8000;

  println(f"IMSIC: address-mapping for ${maxHarts} HARTs")
  println(f"IMSIC:   M-mode:    [0x${m_base}%08X, 0x${m_base + m_size - 1}%08X]")
  println(f"IMSIC:   S/VS-mode: [0x${s_base}%08X, 0x${s_base + s_size - 1}%08X]")

  private val axi4nodes = Seq(
    AXI4SlaveNode(Seq(AXI4SlavePortParameters(
      Seq(AXI4SlaveParameters(
        Seq(AddressSet(m_base, m_size - 1)),
        regionType = RegionType.UNCACHED,
        supportsWrite = TransferSizes(1, 4),
        supportsRead = TransferSizes(1, 4),
        interleavedId = Some(0)
      )),
      beatBytes = 4
    ))),
    AXI4SlaveNode(Seq(AXI4SlavePortParameters(
      Seq(AXI4SlaveParameters(
        Seq(AddressSet(s_base, s_size - 1)),
        regionType = RegionType.UNCACHED,
        supportsWrite = TransferSizes(1, 4),
        supportsRead = TransferSizes(1, 4),
        interleavedId = Some(0)
      )),
      beatBytes = 4
    ))))

  val tl = Option.when(useTL) {
    val tlnodes = Seq.fill(2)(TLClientNode(Seq(TLMasterPortParameters.v1(
      clients = Seq(TLMasterParameters.v1(
        "tl",
        sourceId = IdRange(0, 16)
      ))
    ))))
    axi4nodes zip tlnodes foreach { case (axi4node, tlnode) =>
      axi4node :=
        AXI4IdIndexer(AXI_ID_WIDTH) :=
        AXI4Buffer() :=
        AXI4Buffer() :=
        AXI4UserYanker(Some(1)) :=
        TLToAXI4() :=
        TLWidthWidget(4) :=
        TLFIFOFixer() :=
        TLBuffer() :=
        tlnode
    }

    tlnodes
  }

  val tl_m = tl.map(x => InModuleBody(x(0).makeIOs()))
  val tl_s = tl.map(x => InModuleBody(x(1).makeIOs()))

  val axiMasterNode = Option.when(!useTL) {
    val node = AXI4MasterNode(Seq(AXI4MasterPortParameters(
      Seq(AXI4MasterParameters(
        name = "s_axi_",
        id = IdRange(0, 1 << AXI_ID_WIDTH)
      ))
    )))
    val xbar = AXI4Xbar(TLArbiter.lowestIndexFirst)
    axi4nodes.foreach { _ := xbar }
    xbar := AXI4Buffer() := node
    node
  }

  class imsic_bus_top_imp(wrapper: imsic_bus_top) extends LazyModuleImp(wrapper) {
    // imsic csr top io
    val o_msi_info = IO(Output(UInt(MSI_INFO_WIDTH.W)))
    val o_msi_info_vld = IO(Output(Bool()))

    // axi4lite io
    val axi4lite = Option.when(!useTL)(IO(Flipped(new VerilogAXI4LiteRecord(AXI_ADDR_WIDTH, 32, AXI_ID_WIDTH))))

    // imsic axi top
    val u_imsic_axi_top = Module(new imsic_axi_top)

    // connection: crg
    u_imsic_axi_top.io.axi_clk := clock
    u_imsic_axi_top.io.axi_rstn := (~reset.asBool).asAsyncReset
    u_imsic_axi_top.io.fifo_rstn := (~reset.asBool).asAsyncReset // TODO: axi_rstn & sw_rstn

    // connection: imsic csr top
    o_msi_info := u_imsic_axi_top.io.o_msi_info
    o_msi_info_vld := u_imsic_axi_top.io.o_msi_info_vld

    // connection: axi4lite
    axi4lite.foreach {
      _.viewAs[AXI4LiteBundle].connectToAXI4(wrapper.axiMasterNode.get.out.head._1)
    }

    // connection: axi4
    wrapper.axi4nodes.map(_.in.head._1) zip
      Seq(u_imsic_axi_top.io.m_s, u_imsic_axi_top.io.s_s) foreach {
        case (axi4, axi4lite) => axi4lite.viewAs[AXI4LiteBundle].connectFromAXI4(axi4)
    }
  }

  lazy val module = new imsic_bus_top_imp(this)
}
