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

class imsic_bus_top(implicit p: Parameters) extends LazyModule with HasSoCParameter {
  val tl_reg_imsic = Option.when(soc.IMSICUseTL)(LazyModule(new aia.TLRegIMSIC(soc.IMSICParams, seperateBus = true)))
  val axi_reg_imsic = Option.when(!soc.IMSICUseTL)(LazyModule(new aia.AXIRegIMSIC(soc.IMSICParams, seperateBus = false)))

  val tl = tl_reg_imsic.map { tl_reg_imsic =>
    val tlnodes = Seq.fill(2)(TLClientNode(Seq(TLMasterPortParameters.v1(
      clients = Seq(TLMasterParameters.v1(
        "tl",
        sourceId = IdRange(0, 16)
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

  val axi = axi_reg_imsic.map { axi_reg_imsic =>
    val axinode = AXI4MasterNode(Seq(AXI4MasterPortParameters(
      Seq(AXI4MasterParameters(
        name = "s_axi_",
        id = IdRange(0, 16)
      ))
    )))
    axi_reg_imsic.axi4tolite.head.node := AXI4Buffer() := axinode
    axinode
  }

  val axi4 = axi.map(x => InModuleBody(x.makeIOs()))

  class imsic_bus_top_imp(wrapper: imsic_bus_top) extends LazyModuleImp(wrapper) {
    val msiio = IO(Flipped(new aia.MSITransBundle(soc.IMSICParams)))

    tl_reg_imsic.foreach(_.module.msiio <> msiio)
    axi_reg_imsic.foreach(_.module.msiio <> msiio)
  }

  lazy val module = new imsic_bus_top_imp(this)
}
