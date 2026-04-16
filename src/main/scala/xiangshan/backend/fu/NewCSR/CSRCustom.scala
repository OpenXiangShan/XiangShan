package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import freechips.rocketchip.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.fu.NewCSR.CSRDefines.{
  CSRRWField => RW,
  CSRROField => RO,
}
import xiangshan.HasXSParameter

import scala.collection.immutable.SeqMap

trait CSRCustom { self: NewCSR =>
  // Supervisor Custom Read/Write
  val sbpctl = Module(new CSRModule("Sbpctl", new SbpctlBundle))
    .setAddr(0x5C0)

  val spfctl = Module(new CSRModule("Spfctl", new SpfctlBundle))
    .setAddr(0x5C1)

  // slvpredctl: load violation predict settings
  // Default reset period: 2^16
  // why this number: reset more frequently while keeping the overhead low
  // Overhead: extra two redirections in every 64K cycles => ~0.1% overhead
  val slvpredctl = Module(new CSRModule("Slvpredctl", new SlvpredctlBundle))
    .setAddr(0x5C2)

  // smblockctl: memory block configurations
  val smblockctl = Module(new CSRModule("Smblockctl", new SmblockctlBundle))
    .setAddr(0x5C3)

  val srnctl = Module(new CSRModule("Srnctl", new SrnctlBundle))
    .setAddr(0x5C4)

  // Machine Level Custom Read/Write

  // mcorepwr: Core Power Down Status Enable
  val mcorepwr = Module(new CSRModule("Mcorepwr", new McorepwrBundle))
    .setAddr(0xBC0)

  // mflushpwr: Flush L2 Cache Enable
  val mflushpwr = Module(new CSRModule("Mflushpwr", new MflushpwrBundle)
    with HasMachineFlushL2Bundle
  {
    regOut.L2_FLUSH_DONE := l2FlushDone
  })
    .setAddr(0xBC1)

  val customCSRMods = Seq(
    sbpctl,
    spfctl,
    slvpredctl,
    smblockctl,
    srnctl,
    mcorepwr,
    mflushpwr,
  )

  val customCSRMap: SeqMap[Int, (CSRAddrWriteBundle[_ <: CSRBundle], UInt)] = SeqMap.from(
    customCSRMods.map(csr => (csr.addr -> (csr.w -> csr.rdata))).iterator
  )

  val customCSROutMap: SeqMap[Int, UInt] = SeqMap.from(
    customCSRMods.map(csr => (csr.addr -> csr.regOut.asInstanceOf[CSRBundle].asUInt)).iterator
  )
}

class SbpctlBundle extends CSRBundle {
  val RAS_ENABLE    = RW(6).withReset(true.B).withDescription("Enable the return-address stack predictor.")
  val ITTAGE_ENABLE = RW(5).withReset(true.B).withDescription("Enable the indirect-target TAGE predictor.")
  val SC_ENABLE     = RW(4).withReset(true.B).withDescription("Enable the statistical corrector.")
  val TAGE_ENABLE   = RW(3).withReset(true.B).withDescription("Enable the TAGE predictor.")
  val MBTB_ENABLE   = RW(2).withReset(true.B).withDescription("Enable the macro-BTB predictor.")
  val ABTB_ENABLE   = RW(1).withReset(true.B).withDescription("Enable the alternate branch target buffer.")
  val UBTB_ENABLE   = RW(0).withReset(true.B).withDescription("Enable the micro-BTB predictor.")
}

class SpfctlBundle extends CSRBundle {
  val BERTI_ENABLE            = RW(    32).withReset(true.B).withDescription("Enable the Berti prefetcher.")
  val L2_PF_DELAY_LATENCY     = SpfctlL2PfDelayLatency(31, 22).withReset(SpfctlL2PfDelayLatency.initValue).withDescription("Delay latency used when training the L2 prefetcher.")
  val L2_PF_TP_ENABLE         = RW(    21).withReset(true.B).withDescription("Enable TP-based L2 training and L2 prefetching.")
  val L2_PF_VBOP_ENABLE       = RW(    20).withReset(true.B).withDescription("Enable VBOP-based L2 training and L2 prefetching.")
  val L2_PF_PBOP_ENABLE       = RW(    19).withReset(true.B).withDescription("Enable PBOP-based L2 training and L2 prefetching.")
  val L2_PF_RECV_ENABLE       = RW(    18).withReset(true.B).withDescription("Enable L2 prefetch requests received from the SMS/L1 training path.")
  val L2_PF_STORE_ONLY        = RW(    17).withReset(false.B).withDescription("Restrict L2 prefetching to store-triggered training.")
  val L1D_PF_ENABLE_STRIDE    = RW(    16).withReset(true.B).withDescription("Enable the L1D stride prefetcher.")
  val L1D_PF_ACTIVE_STRIDE    = SpfctlL1DPfActiveStride(15, 10).withReset(SpfctlL1DPfActiveStride.initValue).withDescription("Active-page stride threshold for the L1D prefetcher.")
  val L1D_PF_ACTIVE_THRESHOLD = SpfctlL1DPfActiveThreshold( 9,  6).withReset(SpfctlL1DPfActiveThreshold.initValue).withDescription("Active-page confidence threshold for the L1D prefetcher.")
  val L1D_PF_ENABLE_PHT       = RW(     5).withReset(true.B).withDescription("Enable the L1D PHT prefetcher.")
  val L1D_PF_ENABLE_AGT       = RW(     4).withReset(true.B).withDescription("Enable the L1D AGT prefetcher.")
  val L1D_PF_TRAIN_ON_HIT     = RW(     3).withReset(false.B).withDescription("Train the L1D prefetcher on cache hits.")
  val L1D_PF_ENABLE           = RW(     2).withReset(true.B).withDescription("Enable the L1D cache prefetcher.")
  val L2_PF_ENABLE            = RW(     1).withReset(true.B).withDescription("Master enable for the L2 cache prefetcher.")
  val L1I_PF_ENABLE           = RW(     0).withReset(true.B).withDescription("Enable the L1I cache prefetcher.")
}

class SlvpredctlBundle extends CSRBundle {
  val LVPRED_TIMEOUT          = SlvpredCtlTimeOut(8, 4).withReset(SlvpredCtlTimeOut.initValue).withDescription("Timeout period for the load-violation predictor.")
  val STORESET_NO_FAST_WAKEUP = RW(3).withReset(false.B).withDescription("Disable fast wake-up for StoreSet resets.")
  val STORESET_WAIT_STORE     = RW(2).withReset(false.B).withDescription("Require a store before resetting StoreSet state.")
  val NO_SPEC_LOAD            = RW(1).withReset(false.B).withDescription("Disable speculative loads.")
  val LVPRED_DISABLE          = RW(0).withReset(false.B).withDescription("Disable the load-violation predictor.")
}

class SmblockctlBundle extends CSRBundle {
  val SBUFFER_TIMEOUT                  = SbufferTimeout(31, 10).withReset(SbufferTimeout.initValue).withDescription("Store-buffer flush timeout.")
  val HD_MISALIGN_LD_ENABLE            = RW(   9).withReset(true.B).withDescription("Enable hardware handling of misaligned loads.")
  val HD_MISALIGN_ST_ENABLE            = RW(   8).withReset(true.B).withDescription("Enable hardware handling of misaligned stores.")
  val UNCACHE_WRITE_OUTSTANDING_ENABLE = RW(   7).withReset(false.B).withDescription("Enable outstanding uncacheable writes.")
  val CACHE_ERROR_ENABLE               = RW(   6).withReset(true.B).withDescription("Enable cache-error handling after reset.")
  val SOFT_PREFETCH_ENABLE             = RW(   5).withReset(true.B).withDescription("Enable software prefetch support after reset.")
  val LDLD_VIO_CHECK_ENABLE            = RW(   4).withReset(true.B).withDescription("Enable load-load violation checking after reset.")
  val SBUFFER_THRESHOLD                = SbufferThreshold(3, 0).withReset(SbufferThreshold.initValue).withDescription("Store-buffer flush threshold.")
}

class SrnctlBundle extends CSRBundle {
  val WFI_ENABLE     = RW(2).withReset(true.B).withDescription("Enable WFI execution.")
  val FUSION_ENABLE  = RW(0).withReset(true.B).withDescription("Enable instruction fusion.")
}

class McorepwrBundle extends CSRBundle {
  val POWER_DOWN_ENABLE = RW(0).withReset(false.B).withDescription("Enable core power-down requests.")
}

class MflushpwrBundle extends CSRBundle {
  val FLUSH_L2_ENABLE = RW(0).withReset(false.B).withDescription("Enable L2 cache flush requests.")
  val L2_FLUSH_DONE   = RO(1).withReset(false.B).withDescription("Indicates that the L2 cache flush has completed.")
}

object SbufferThreshold extends CSREnum with RWApply {
  val initValue = Value(7.U)
}

object SbufferTimeout extends CSREnum with RWApply {
  val initValue = Value((1<<20).U)
}

object SpfctlL1DPfActiveStride extends CSREnum with RWApply {
  val initValue = Value(30.U)
}

object SpfctlL1DPfActiveThreshold extends CSREnum with RWApply {
  val initValue = Value(12.U)
}

object SpfctlL2PfDelayLatency extends CSREnum with RWApply {
  val initValue = Value(0.U)
}

object SlvpredCtlTimeOut extends CSREnum with RWApply {
  val initValue = Value(3.U)
}
