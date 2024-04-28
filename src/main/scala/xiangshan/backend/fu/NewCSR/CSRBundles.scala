package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.fu.NewCSR.CSRDefines.{CSRROField => RO, CSRRWField => RW, CSRWARLField => WARL, _}
import xiangshan.backend.fu.NewCSR.CSRFunc._
import xiangshan.backend.fu.fpu.Bundles.Fflags
import xiangshan.backend.fu.vector.Bundles.{Vl, Vstart, Vxsat}
import xiangshan.frontend.BPUCtrl

object CSRBundles {
  class XtvecBundle extends CSRBundle {
    val mode = XtvecMode(1, 0, wNoFilter)
    val addr = WARL(63, 2, wNoFilter)
  }

  class CauseBundle extends CSRBundle {
    val Interrupt = RW(63)
    val ExceptionCode = RW(62, 0)
  }

  class Counteren extends CSRBundle {
    val CY = RW(0)
    val TM = RW(1)
    val IR = RW(2)
    val HPM = RW(31, 3)
  }

  class OneFieldBundle extends CSRBundle {
    val ALL = RW(63, 0)
  }

  class Envcfg extends CSRBundle {
    val STCE  = RO(    63).withReset(0.U)
    val PBMTE = RO(    62).withReset(0.U)
    val ADUE  = RO(    61).withReset(0.U)
    val PMM   = RO(33, 32).withReset(0.U)
    val CBZE  = RO(     7).withReset(0.U)
    val CBCFE = RO(     6).withReset(0.U)
    val CBIE  = RO( 5,  4).withReset(0.U)
    val FIOM  = RO(     0).withReset(0.U)
  }

  class PrivState extends Bundle {
    val PRVM = PrivMode(0)
    val V    = VirtMode(0)

    def isModeHU: Bool = this.V === VirtMode.Off && this.PRVM === PrivMode.U

    def isModeHS: Bool = this.V === VirtMode.Off && this.PRVM === PrivMode.S

    def isModeHUorHS: Bool = this.V === VirtMode.Off && this.PRVM.isOneOf(PrivMode.S, PrivMode.U)

    def isModeM: Bool = this.V === VirtMode.Off && this.PRVM === PrivMode.M

    def isModeVU: Bool = this.V === VirtMode.On && this.PRVM === PrivMode.U

    def isModeVS: Bool = this.V === VirtMode.On && this.PRVM === PrivMode.S

    def isModeHUorVU: Bool = this.PRVM === PrivMode.U

    def isVirtual: Bool = this.V === VirtMode.On

    // VU < VS < HS < M
    // HU < HS < M
    def < (that: PrivState): Bool = {
      (this.isVirtual && (that.isModeM || that.isModeHS)) ||
        (this.V === that.V && this.PRVM < that.PRVM)
    }

    def > (that: PrivState): Bool = {
      (that.isVirtual && (this.isModeM || this.isModeHS)) ||
        (that.V === this.V && that.PRVM < this.PRVM)
    }
  }

  object PrivState {
    def ModeM: PrivState = WireInit((new PrivState).Lit(
      _.PRVM -> PrivMode.M,
      _.V    -> VirtMode.Off,
    ))

    def ModeHS: PrivState = WireInit((new PrivState).Lit(
      _.PRVM -> PrivMode.S,
      _.V    -> VirtMode.Off,
    ))

    def ModeHU: PrivState = WireInit((new PrivState).Lit(
      _.PRVM -> PrivMode.U,
      _.V    -> VirtMode.Off,
    ))

    def ModeVS: PrivState = WireInit((new PrivState).Lit(
      _.PRVM -> PrivMode.S,
      _.V    -> VirtMode.On,
    ))

    def ModeVU: PrivState = WireInit((new PrivState).Lit(
      _.PRVM -> PrivMode.U,
      _.V    -> VirtMode.On,
    ))
  }

  class RobCommitCSR(implicit p: Parameters) extends Bundle {
    // need contain 8x8
    val instNum = ValidIO(UInt(7.W))
    val fflags  = ValidIO(Fflags())
    val fsDirty = Bool()
    val vxsat   = ValidIO(Vxsat())
    val vsDirty = Bool()
    val vtype   = ValidIO(new CSRVTypeBundle)
    val vl      = ValidIO(Vl())
    val vstart  = ValidIO(Vstart())
  }

  class CSRCustomState(implicit p: Parameters) extends Bundle {
    // Prefetcher
    val l1I_pf_enable = Output(Bool())
    val l2_pf_enable = Output(Bool())
    val l1D_pf_enable = Output(Bool())
    val l1D_pf_train_on_hit = Output(Bool())
    val l1D_pf_enable_agt = Output(Bool())
    val l1D_pf_enable_pht = Output(Bool())
    val l1D_pf_active_threshold = Output(UInt(4.W))
    val l1D_pf_active_stride = Output(UInt(6.W))
    val l1D_pf_enable_stride = Output(Bool())
    val l2_pf_store_only = Output(Bool())
    // ICache
    val icache_parity_enable = Output(Bool())
    // Labeled XiangShan
    val dsid = Output(UInt(8.W)) // TODO: DsidWidth as parameter
    // Load violation predictor
    val lvpred_disable = Output(Bool())
    val no_spec_load = Output(Bool())
    val storeset_wait_store = Output(Bool())
    val storeset_no_fast_wakeup = Output(Bool())
    val lvpred_timeout = Output(UInt(5.W))
    // Branch predictor
    val bp_ctrl = Output(new BPUCtrl)
    // Memory Block
    val sbuffer_threshold = Output(UInt(4.W))
    val ldld_vio_check_enable = Output(Bool())
    val soft_prefetch_enable = Output(Bool())
    val cache_error_enable = Output(Bool())
    val uncache_write_outstanding_enable = Output(Bool())
    // Rename
    val fusion_enable = Output(Bool())
    val wfi_enable = Output(Bool())
    // Decode
    val svinval_enable = Output(Bool())
  }
}
