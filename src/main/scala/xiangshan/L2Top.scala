/***************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
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

package xiangshan

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import chisel3.util.{Valid, ValidIO}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tile.{BusErrorUnit, BusErrorUnitParams, BusErrors}
import freechips.rocketchip.tilelink._
import coupledL2.{L2ParamKey, EnableCHI}
import coupledL2.tl2tl.TL2TLCoupledL2
import coupledL2.tl2chi.{TL2CHICoupledL2, PortIO}
import huancun.BankBitsKey
import system.HasSoCParameter
import top.BusPerfMonitor
import utility.{DelayN, ResetGen, TLClientsMerger, TLEdgeBuffer, TLLogger}

class L1BusErrorUnitInfo(implicit val p: Parameters) extends Bundle with HasSoCParameter {
  val ecc_error = Valid(UInt(soc.PAddrBits.W))
}

class XSL1BusErrors()(implicit val p: Parameters) extends BusErrors {
  val icache = new L1BusErrorUnitInfo
  val dcache = new L1BusErrorUnitInfo
  val l2 = new L1BusErrorUnitInfo

  override def toErrorList: List[Option[(ValidIO[UInt], String, String)]] =
    List(
      Some(icache.ecc_error, "I_ECC", "Icache ecc error"),
      Some(dcache.ecc_error, "D_ECC", "Dcache ecc error"),
      Some(l2.ecc_error, "L2_ECC", "L2Cache ecc error")
    )
}

/**
  *   L2Top contains everything between Core and XSTile-IO
  */
class L2Top()(implicit p: Parameters) extends LazyModule
  with HasXSParameter
  with HasSoCParameter
{
  def chainBuffer(depth: Int, n: String): (Seq[LazyModule], TLNode) = {
    val buffers = Seq.fill(depth){ LazyModule(new TLBuffer()) }
    buffers.zipWithIndex.foreach{ case (b, i) => {
      b.suggestName(s"${n}_${i}")
    }}
    val node = buffers.map(_.node.asInstanceOf[TLNode]).reduce(_ :*=* _)
    (buffers, node)
  }
  val enableCHI = p(EnableCHI)
  val enableL2 = coreParams.L2CacheParamsOpt.isDefined
  // =========== Components ============
  val l1_xbar = TLXbar()
  val mmio_xbar = TLXbar()
  val mmio_port = TLIdentityNode() // to L3
  val memory_port = if (enableCHI && enableL2) None else Some(TLIdentityNode())
  val beu = LazyModule(new BusErrorUnit(
    new XSL1BusErrors(), BusErrorUnitParams(0x38010000)
  ))

  val i_mmio_port = TLTempNode()
  val d_mmio_port = TLTempNode()

  val misc_l2_pmu = BusPerfMonitor(name = "Misc_L2", enable = !debugOpts.FPGAPlatform) // l1D & l1I & PTW
  val l2_l3_pmu = BusPerfMonitor(name = "L2_L3", enable = !debugOpts.FPGAPlatform, stat_latency = true)
  val xbar_l2_buffer = TLBuffer()

  val enbale_tllog = !debugOpts.FPGAPlatform && debugOpts.AlwaysBasicDB
  val l1d_logger = TLLogger(s"L2_L1D_${coreParams.HartId}", enbale_tllog)
  val l1i_logger = TLLogger(s"L2_L1I_${coreParams.HartId}", enbale_tllog)
  val ptw_logger = TLLogger(s"L2_PTW_${coreParams.HartId}", enbale_tllog)
  val ptw_to_l2_buffer = LazyModule(new TLBuffer)
  val i_mmio_buffer = LazyModule(new TLBuffer)

  val clint_int_node = IntIdentityNode()
  val debug_int_node = IntIdentityNode()
  val plic_int_node = IntIdentityNode()

  println(s"enableCHI: ${enableCHI}")
  val tl2tl_l2cache = if (enableL2 && !enableCHI) {
    Some(LazyModule(new TL2TLCoupledL2()(new Config((_, _, _) => {
      case L2ParamKey => coreParams.L2CacheParamsOpt.get.copy(
        hartIds = Seq(p(XSCoreParamsKey).HartId),
        FPGAPlatform = debugOpts.FPGAPlatform
      )
    }))))
  } else None
  val tl2chi_l2cache = if (enableL2 && enableCHI) {
    Some(LazyModule(new TL2CHICoupledL2()(new Config((_, _, _) => {
      case L2ParamKey => coreParams.L2CacheParamsOpt.get.copy(
        hartIds = Seq(p(XSCoreParamsKey).HartId),
        FPGAPlatform = debugOpts.FPGAPlatform
      )
      case EnableCHI => true
      // case XSCoreParamsKey => p(XSCoreParamsKey)
      case BankBitsKey => log2Ceil(coreParams.L2NBanks)
    }))))
  } else None
  val l2_binder = coreParams.L2CacheParamsOpt.map(_ => BankBinder(coreParams.L2NBanks, 64))

  // =========== Connection ============
  // l2 to l2_binder, then to memory_port
  l2_binder match {
    case Some(binder) =>
      if (!enableCHI) {
        memory_port.get := l2_l3_pmu := TLClientsMerger() := TLXbar() :=* binder :*= tl2tl_l2cache.get.node
      }
    case None =>
      memory_port.get := l1_xbar
  }

  tl2chi_l2cache match {
    case Some(l2) =>
      l2.managerNode := TLXbar() :=* l2_binder.get :*= l2.node :*= l1_xbar
      l2.mmioNode := mmio_port
    case None =>
  }
  

  mmio_xbar := TLBuffer.chainNode(2) := i_mmio_port
  mmio_xbar := TLBuffer.chainNode(2) := d_mmio_port
  beu.node := TLBuffer.chainNode(1) := mmio_xbar
  mmio_port := TLBuffer() := mmio_xbar

  class L2TopImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
    val beu_errors = IO(Input(chiselTypeOf(beu.module.io.errors)))
    val reset_vector = IO(new Bundle {
      val fromTile = Input(UInt(PAddrBits.W))
      val toCore = Output(UInt(PAddrBits.W))
    })
    val hartId = IO(new Bundle() {
      val fromTile = Input(UInt(64.W))
      val toCore = Output(UInt(64.W))
    })
    val cpu_halt = IO(new Bundle() {
      val fromCore = Input(Bool())
      val toTile = Output(Bool())
    })
    val debugTopDown = IO(new Bundle() {
      val robHeadPaddr = Flipped(Valid(UInt(36.W)))
      val l2MissMatch = Output(Bool())
    })
    val chi = if (enableCHI) Some(IO(new PortIO)) else None

    val resetDelayN = Module(new DelayN(UInt(PAddrBits.W), 5))

    beu.module.io.errors <> beu_errors
    resetDelayN.io.in := reset_vector.fromTile
    reset_vector.toCore := resetDelayN.io.out
    hartId.toCore := hartId.fromTile
    cpu_halt.toTile := cpu_halt.fromCore
    dontTouch(hartId)
    dontTouch(cpu_halt)

    val l2_hint = IO(ValidIO(new L2ToL1Hint())) // TODO: parameterize this
    if (tl2tl_l2cache.isDefined) {
      l2_hint := tl2tl_l2cache.get.module.io.l2_hint
      // debugTopDown <> tl2tl_l2cache.get.module.io.debugTopDown
      tl2tl_l2cache.get.module.io.debugTopDown.robHeadPaddr := DontCare
      tl2tl_l2cache.get.module.io.hartId := hartId.fromTile
      tl2tl_l2cache.get.module.io.debugTopDown.robHeadPaddr.head := debugTopDown.robHeadPaddr
      debugTopDown.l2MissMatch := tl2tl_l2cache.get.module.io.debugTopDown.l2MissMatch.head
    } else if (tl2chi_l2cache.isDefined) {
      l2_hint := tl2chi_l2cache.get.module.io.l2_hint
      // debugTopDown <> tl2chi_l2cache.get.module.io.debugTopDown
      tl2chi_l2cache.get.module.io.debugTopDown.robHeadPaddr := DontCare
      tl2chi_l2cache.get.module.io.hartId := hartId.fromTile
      tl2chi_l2cache.get.module.io.debugTopDown.robHeadPaddr.head := debugTopDown.robHeadPaddr
      debugTopDown.l2MissMatch := tl2chi_l2cache.get.module.io.debugTopDown.l2MissMatch.head
    } else {
      l2_hint := 0.U.asTypeOf(l2_hint)
      debugTopDown <> DontCare
    }

    chi.foreach(_ <> tl2chi_l2cache.get.module.io.chi)
  }

  lazy val module = new L2TopImp(this)
}
