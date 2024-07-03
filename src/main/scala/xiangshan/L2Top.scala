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
import freechips.rocketchip.tile.{BusErrorUnit, BusErrorUnitParams, BusErrors, MaxHartIdBits}
import freechips.rocketchip.tilelink._
import coupledL2.{L2ParamKey, EnableCHI}
import coupledL2.tl2tl.TL2TLCoupledL2
import coupledL2.tl2chi.{TL2CHICoupledL2, PortIO}
import huancun.BankBitsKey
import system.HasSoCParameter
import top.BusPerfMonitor
import utility.{DelayN, ResetGen, TLClientsMerger, TLEdgeBuffer, TLLogger}
import xiangshan.cache.mmu.TlbRequestIO

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
  val l2cache = if (enableL2) {
    val config = new Config((_, _, _) => {
      case L2ParamKey => coreParams.L2CacheParamsOpt.get.copy(
        hartId = p(XSCoreParamsKey).HartId,
        FPGAPlatform = debugOpts.FPGAPlatform
      )
      case EnableCHI => p(EnableCHI)
      case BankBitsKey => log2Ceil(coreParams.L2NBanks)
      case MaxHartIdBits => p(MaxHartIdBits)
    })
    if (enableCHI) Some(LazyModule(new TL2CHICoupledL2()(new Config(config))))
    else Some(LazyModule(new TL2TLCoupledL2()(new Config(config))))
  } else None
  val l2_binder = coreParams.L2CacheParamsOpt.map(_ => BankBinder(coreParams.L2NBanks, 64))

  // =========== Connection ============
  // l2 to l2_binder, then to memory_port
  l2cache match {
    case Some(l2) =>
      l2_binder.get :*= l2.node :*= xbar_l2_buffer :*= l1_xbar :=* misc_l2_pmu
      l2 match {
        case l2: TL2TLCoupledL2 =>
          memory_port.get := l2_l3_pmu := TLClientsMerger() := TLXbar() :=* l2_binder.get
        case l2: TL2CHICoupledL2 =>
          l2.managerNode := TLXbar() :=* l2_binder.get
          l2.mmioNode := mmio_port
      }
    case None =>
      memory_port.get := l1_xbar
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
      val robTrueCommit = Input(UInt(64.W))
      val robHeadPaddr = Flipped(Valid(UInt(36.W)))
      val l2MissMatch = Output(Bool())
    })
    val chi = if (enableCHI) Some(IO(new PortIO)) else None
    val nodeID = if (enableCHI) Some(IO(Input(UInt(NodeIDWidth.W)))) else None
    val l2_tlb_req = IO(new TlbRequestIO(nRespDups = 2))
    val l2_hint = IO(ValidIO(new L2ToL1Hint()))

    val resetDelayN = Module(new DelayN(UInt(PAddrBits.W), 5))

    beu.module.io.errors <> beu_errors
    resetDelayN.io.in := reset_vector.fromTile
    reset_vector.toCore := resetDelayN.io.out
    hartId.toCore := hartId.fromTile
    cpu_halt.toTile := cpu_halt.fromCore
    dontTouch(hartId)
    dontTouch(cpu_halt)

    if (l2cache.isDefined) {
      val l2 = l2cache.get.module
      l2_hint := l2.io.l2_hint
      l2.io.debugTopDown.robHeadPaddr := DontCare
      l2.io.hartId := hartId.fromTile
      l2.io.debugTopDown.robHeadPaddr := debugTopDown.robHeadPaddr
      l2.io.debugTopDown.robTrueCommit := debugTopDown.robTrueCommit
      debugTopDown.l2MissMatch := l2.io.debugTopDown.l2MissMatch

      /* l2 tlb */
      l2_tlb_req.req.bits := DontCare
      l2_tlb_req.req.valid := l2.io.l2_tlb_req.req.valid
      l2_tlb_req.resp.ready := l2.io.l2_tlb_req.resp.ready
      l2_tlb_req.req.bits.vaddr := l2.io.l2_tlb_req.req.bits.vaddr
      l2_tlb_req.req.bits.cmd := l2.io.l2_tlb_req.req.bits.cmd
      l2_tlb_req.req.bits.size := l2.io.l2_tlb_req.req.bits.size
      l2_tlb_req.req.bits.kill := l2.io.l2_tlb_req.req.bits.kill
      l2_tlb_req.req.bits.no_translate := l2.io.l2_tlb_req.req.bits.no_translate
      l2_tlb_req.req_kill := l2.io.l2_tlb_req.req_kill
      l2.io.l2_tlb_req.resp.valid := l2_tlb_req.resp.valid
      l2.io.l2_tlb_req.req.ready := l2_tlb_req.req.ready
      l2.io.l2_tlb_req.resp.bits.paddr.head := l2_tlb_req.resp.bits.paddr.head
      l2.io.l2_tlb_req.resp.bits.miss := l2_tlb_req.resp.bits.miss
      l2.io.l2_tlb_req.resp.bits.excp.head <> l2_tlb_req.resp.bits.excp.head

      l2cache.get match {
        case l2cache: TL2CHICoupledL2 =>
          val l2 = l2cache.module
          l2.io_nodeID := nodeID.get
          chi.get <> l2.io_chi
        case l2cache: TL2TLCoupledL2 =>
      }
    } else {
      l2_hint := 0.U.asTypeOf(l2_hint)
      debugTopDown <> DontCare

      l2_tlb_req.req.valid := false.B
      l2_tlb_req.req.bits := DontCare
      l2_tlb_req.req_kill := DontCare
      l2_tlb_req.resp.ready := true.B
    }
  }

  lazy val module = new L2TopImp(this)
}
