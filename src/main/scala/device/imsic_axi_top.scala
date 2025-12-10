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
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.tilelink._
import system.HasSoCParameter

object IMSICBusType extends Enumeration {
  val NONE, TL, AXI = Value
}

class imsic_bus_top(implicit p: Parameters) extends LazyModule with HasSoCParameter {
  // Tilelink Bus
  val tl_reg_imsic = Option.when(soc.IMSICBusType == device.IMSICBusType.TL)(LazyModule(new aia.TLRegIMSIC(soc.IMSICParams, seperateBus = true)))

  val tl = tl_reg_imsic.map { tl_reg_imsic =>
    val tlnodes = Seq.fill(2)(TLClientNode(Seq(TLMasterPortParameters.v1(
      clients = Seq(TLMasterParameters.v1(
        "tl",
        sourceId = IdRange(0, 65536)
      ))
    ))))
    tl_reg_imsic.fromMem zip tlnodes foreach { case (fromMem, tlnode) =>
      fromMem :=
        TLWidthWidget(4) :=
        TLFIFOFixer() :=
        TLBuffer() :=
        tlnode
    }
    tlnodes
  }

  val tl_m = tl.map(x => InModuleBody(x(0).makeIOs()))
  val tl_s = tl.map(x => InModuleBody(x(1).makeIOs()))

  // AXI4 Bus
  val axi_reg_imsic = Option.when(soc.IMSICBusType == device.IMSICBusType.AXI)(LazyModule(new aia.AXIRegIMSIC_WRAP(soc.IMSICParams, seperateBus = false)))

  val axi = axi_reg_imsic.map { axi_reg_imsic =>
    val axinode = AXI4MasterNode(Seq(AXI4MasterPortParameters(
      Seq(AXI4MasterParameters(
        name = "s_axi_",
        id = IdRange(0, 65536),
        maxFlight = Some(0)
      ))
    )))
    axi_reg_imsic.imsic_xbar1to2 := AXI4Buffer() := axinode
    axinode
  }

  val axi4 = axi.map(x => InModuleBody(x.makeIOs()))

  class imsic_bus_top_imp(wrapper: imsic_bus_top) extends LazyModuleImp(wrapper) {
    val msiio = IO(Flipped(new aia.MSITransBundle(soc.IMSICParams)))
    val teemsiio = Option.when(soc.IMSICParams.HasTEEIMSIC)(IO(Flipped(new aia.MSITransBundle(soc.IMSICParams))))

    // No Bus
    val msi = Option.when(soc.IMSICBusType == device.IMSICBusType.NONE)(
      IO(new aia.MSITransBundle(soc.IMSICParams))
    )
    val teemsi = Option.when(soc.IMSICBusType == device.IMSICBusType.NONE & soc.IMSICParams.HasTEEIMSIC)(
      IO(new aia.MSITransBundle(soc.IMSICParams))
    )

    tl_reg_imsic.foreach(_.module.msiio <> msiio)
    axi_reg_imsic.foreach(_.module.msiio <> msiio)
    msi.foreach(_ <> msiio)

    axi_reg_imsic zip teemsiio foreach { case (axi_reg_imsic, teemsiio) =>
      axi_reg_imsic.module.teemsiio.foreach(_ <> teemsiio)
    }
    teemsi zip teemsiio foreach { case (teemsi, teemsiio) =>
      teemsi <> teemsiio
    }
  }

  lazy val module = new imsic_bus_top_imp(this)
}
