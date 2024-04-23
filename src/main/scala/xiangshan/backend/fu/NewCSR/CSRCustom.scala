package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import freechips.rocketchip.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.fu.NewCSR.CSRDefines.{CSRRWField => RW}
import xiangshan.HasXSParameter

import scala.collection.immutable.SeqMap

trait CSRCustom { self: NewCSR =>
  // Supervisor Custom Read/Write
  val sbpctl = Module(new CSRModule("sbpctl", new SbpctlBundle))
    .setAddr(0x5C0)

  val spfctl = Module(new CSRModule("spfctl", new SpfctlBundle))
    .setAddr(0x5C1)

  // slvpredctl: load violation predict settings
  // Default reset period: 2^16
  // why this number: reset more frequently while keeping the overhead low
  // Overhead: extra two redirections in every 64K cycles => ~0.1% overhead
  val slvpredctl = Module(new CSRModule("slvpredctl", new SlvpredctlBundle))
    .setAddr(0x5C2)

  // smblockctl: memory block configurations
  val smblockctl = Module(new CSRModule("smblockctl", new SmblockctlBundle))
    .setAddr(0x5C3)

  val srnctl = Module(new CSRModule("srnctl", new SrnctlBundle))
    .setAddr(0x5C4)

  // sdsid: Differentiated Services ID
  val sdsid = Module(new CSRModule("sdsid"))
    .setAddr(0x9C0)

  val sfetchctl = Module(new CSRModule("sfetchctl", new SfetchctlBundle))
    .setAddr(0x9E0)

  val customCSRMods = Seq(
    sbpctl,
    spfctl,
    slvpredctl,
    smblockctl,
    srnctl,
    sdsid,
    sfetchctl,
  )

  val customCSRMap: SeqMap[Int, (CSRAddrWriteBundle[_ <: CSRBundle], Data)] = SeqMap.from(
    customCSRMods.map(csr => (csr.addr -> (csr.w -> csr.rdata.asInstanceOf[CSRBundle].asUInt))).iterator
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
  // turn off L2 BOP, turn on L1 SMS by default
  val L2_PF_STORE_ONLY        = RW(    17).withReset(false.B)     // L2 pf store only
  val L1D_PF_ENABLE_STRIDE    = RW(    16).withReset(true.B)      // L1D prefetch enable stride
  val L1D_PF_ACTIVE_STRIDE    = RW(15, 10, /*resetVal= */ 30.U)   // L1D prefetch active page stride
  val L1D_PF_ACTIVE_THRESHOLD = RW( 9,  6, /*resetVal= */ 12.U)   // L1D prefetch active page threshold
  val L1D_PF_ENABLE_PHT       = RW(     5).withReset(true.B)      // L1D prefetch enable pht
  val L1D_PF_ENABLE_AGT       = RW(     4).withReset(true.B)      // L1D prefetch enable agt
  val L1D_PF_TRAIN_ON_HIT     = RW(     3).withReset(false.B)     // L1D train prefetch on hit
  val L1D_PF_ENABLE           = RW(     2).withReset(true.B)      // L1D Cache Prefetcher Enable
  val L2_PF_ENABLE            = RW(     1).withReset(true.B)      // L2  Cache Prefetcher Enable
  val L1I_PF_ENABLE           = RW(     0).withReset(true.B)      // L1I Cache Prefetcher Enable
}

class SlvpredctlBundle extends CSRBundle {
  val LVPRED_TIMEOUT          = RW(8, 4, /*resetVal= */ 3.U)
  val STORESET_NO_FAST_WAKEUP = RW(3).withReset(false.B)
  val STORESET_WAIT_STORE     = RW(2).withReset(false.B)
  val NO_SPEC_LOAD            = RW(1).withReset(false.B)
  val LVPRED_DISABLE          = RW(0).withReset(false.B)
}

class SmblockctlBundle extends CSRBundle {
  val UNCACHE_WRITE_OUTSTANDING_ENABLE = RW(   7).withReset(false.B)  // Enable uncache write outstanding (0).
  val CACHE_ERROR_ENABLE               = RW(   6).withReset(true.B)   // Enable cache error after reset (CE).
  val SOFT_PREFETCH_ENABLE             = RW(   5).withReset(true.B)   // Enable soft-prefetch after reset (SP).
  val LDLD_VIO_CHECK_ENABLE            = RW(   4).withReset(true.B)   // Enable load load violation check after reset (LVC).
  val SBUFFER_THRESHOLD                = RW(3, 0)// todo:, /*resetVal= */ 7.U) // Store buffer flush threshold (Th).
}

class SrnctlBundle extends CSRBundle {
  val WFI_ENABLE     = RW(2).withReset(true.B)
  val SVINVAL_ENABLE = RW(1).withReset(true.B)
  val FUSION_ENABLE  = RW(0).withReset(true.B)
}

class SfetchctlBundle extends CSRBundle {
  val ICACHE_PARITY_ENABLE = RW(0).withReset(false.B) // L1I Cache Parity check enable
}