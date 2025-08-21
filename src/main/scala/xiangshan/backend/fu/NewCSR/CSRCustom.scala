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
  val LOOP_ENABLE = RW(6).withReset(true.B)
  val RAS_ENABLE  = RW(5).withReset(true.B)
  val SC_ENABLE   = RW(4).withReset(true.B)
  val TAGE_ENABLE = RW(3).withReset(true.B)
  val BIM_ENABLE  = RW(2).withReset(true.B)
  val BTB_ENABLE  = RW(1).withReset(true.B)
  val UBTB_ENABLE = RW(0).withReset(true.B)
}

class SpfctlBundle extends CSRBundle {
  val L2_PF_TP_ENABLE         = RW(    21).withReset(true.B)  // (Train L2, Prefetch L2) TP
  val L2_PF_VBOP_ENABLE       = RW(    20).withReset(true.B)  // (Train L2, Prefetch L2) VBOP
  val L2_PF_PBOP_ENABLE       = RW(    19).withReset(true.B)  // (Train L2, Prefetch L2) PBOP
  val L2_PF_RECV_ENABLE       = RW(    18).withReset(true.B)  // (Train L1, Prefetch L2) receive from sms
  val L2_PF_STORE_ONLY        = RW(    17).withReset(false.B)     // L2 pf store only
  val L1D_PF_ENABLE_STRIDE    = RW(    16).withReset(true.B)      // L1D prefetch enable stride
  val L1D_PF_ACTIVE_STRIDE    = SpfctlL1DPfActiveStride(15, 10).withReset(SpfctlL1DPfActiveStride.initValue)   // L1D prefetch active page stride
  val L1D_PF_ACTIVE_THRESHOLD = SpfctlL1DPfActiveThreshold( 9,  6).withReset(SpfctlL1DPfActiveThreshold.initValue)   // L1D prefetch active page threshold
  val L1D_PF_ENABLE_PHT       = RW(     5).withReset(true.B)      // L1D prefetch enable pht
  val L1D_PF_ENABLE_AGT       = RW(     4).withReset(true.B)      // L1D prefetch enable agt
  val L1D_PF_TRAIN_ON_HIT     = RW(     3).withReset(false.B)     // L1D train prefetch on hit
  val L1D_PF_ENABLE           = RW(     2).withReset(true.B)      // L1D Cache Prefetcher Enable
  val L2_PF_ENABLE            = RW(     1).withReset(true.B)      // L2  Cache Prefetcher master Enable
  val L1I_PF_ENABLE           = RW(     0).withReset(true.B)      // L1I Cache Prefetcher Enable
}

class SlvpredctlBundle extends CSRBundle {
  val LVPRED_TIMEOUT          = SlvpredCtlTimeOut(8, 4).withReset(SlvpredCtlTimeOut.initValue)
  val STORESET_NO_FAST_WAKEUP = RW(3).withReset(false.B)
  val STORESET_WAIT_STORE     = RW(2).withReset(false.B)
  val NO_SPEC_LOAD            = RW(1).withReset(false.B)
  val LVPRED_DISABLE          = RW(0).withReset(false.B)
}

class SmblockctlBundle extends CSRBundle {
  val HD_MISALIGN_LD_ENABLE            = RW(   9).withReset(true.B)  // Enable hardware load misalign.
  val HD_MISALIGN_ST_ENABLE            = RW(   8).withReset(true.B)  // Enable hardware store misalign.
  val UNCACHE_WRITE_OUTSTANDING_ENABLE = RW(   7).withReset(false.B)  // Enable uncache write outstanding (0).
  val CACHE_ERROR_ENABLE               = RW(   6).withReset(true.B)   // Enable cache error after reset (CE).
  val SOFT_PREFETCH_ENABLE             = RW(   5).withReset(true.B)   // Enable soft-prefetch after reset (SP).
  val LDLD_VIO_CHECK_ENABLE            = RW(   4).withReset(true.B)   // Enable load load violation check after reset (LVC).
  val SBUFFER_THRESHOLD                = SbufferThreshold(3, 0).withReset(SbufferThreshold.initValue) // Store buffer flush threshold (Th).
}

class SrnctlBundle extends CSRBundle {
  val WFI_ENABLE     = RW(2).withReset(true.B)
  val FUSION_ENABLE  = RW(0).withReset(true.B)
}

class McorepwrBundle extends CSRBundle {
  val POWER_DOWN_ENABLE = RW(0).withReset(false.B)
}

class MflushpwrBundle extends CSRBundle {
  val FLUSH_L2_ENABLE = RW(0).withReset(false.B)
  val L2_FLUSH_DONE   = RO(1).withReset(false.B)
}

object SbufferThreshold extends CSREnum with RWApply {
  val initValue = Value(7.U)
}

object SpfctlL1DPfActiveStride extends CSREnum with RWApply {
  val initValue = Value(30.U)
}

object SpfctlL1DPfActiveThreshold extends CSREnum with RWApply {
  val initValue = Value(12.U)
}

object SlvpredCtlTimeOut extends CSREnum with RWApply {
  val initValue = Value(3.U)
}

