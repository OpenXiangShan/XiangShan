package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.CSRs
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.fu.NewCSR.CSRDefines.{CSRROField => RO, CSRWARLField => WARL}
import xiangshan.backend.fu.NewCSR.CSRFunc._
import xiangshan.PMParameKey
import freechips.rocketchip.tile.XLen
import CSRConfig._

import scala.collection.immutable.SeqMap

trait CSRPMP { self: NewCSR =>
  val pmpcfg: Seq[CSRModule[_]] = Range(0, p(PMParameKey).NumPMP/8+1, 2).map(num =>
    Module(new CSRModule(s"Pmpcfg$num") with HasPMPCfgRSink {
      // read condition
      regOut := cfgRData(64*(num/2+1)-1, 64*num/2)
    })
      .setAddr(CSRs.pmpcfg0 + num)
  )

  // every pmpcfg has 8 cfgs
  val cfgs: Seq[CSRModule[_]] = Range(0, p(PMParameKey).NumPMP).map(num =>
    Module(new CSRModule(s"Pmp$num"+"cfg", new PMPCfgBundle) {
      when (w.wen && (!(!w.wdata(0).asBool && w.wdata(1).asBool))) {  // when R=0 W=1, reserved
        reg.W := w.wdata(1).asBool
      }.otherwise {
        reg.W := reg.W
      }
      reg.A := Mux(wen, Mux(w.wdata(4, 3) === 2.U, 3.U, w.wdata(4, 3).asUInt), reg.A.asUInt) // no support Na4
    })
  )

  val pmpaddr: Seq[CSRModule[_]] = Range(0, p(PMParameKey).NumPMP).map(num =>
    Module(new CSRModule(s"Pmpaddr$num", new PMPAddrBundle) with HasPMPAddrSink {
      // read condition
      rdata := addrRData(num)
    })
      .setAddr(CSRs.pmpaddr0 + num)
  )

  val pmpCSRMods: Seq[CSRModule[_]] = pmpcfg ++ pmpaddr

  val pmpCSRMap: SeqMap[Int, (CSRAddrWriteBundle[_], UInt)] = SeqMap.from(
    pmpCSRMods.map(csr => csr.addr -> (csr.w -> csr.rdata)).iterator
  )

  val pmpCSROutMap: SeqMap[Int, UInt] = SeqMap.from(
    pmpCSRMods.map(csr => csr.addr -> csr.regOut.asInstanceOf[CSRBundle].asUInt).iterator
  )

  private val pmpCfgRead = Cat(cfgs.map(_.rdata(7, 0)).reverse)

  pmpCSRMods.foreach { mod =>
    mod match {
      case m: HasPMPCfgRSink =>
        m.cfgRData := pmpCfgRead
      case _ =>
    }
  }
}

class PMPCfgBundle extends CSRBundle {
  override val len = 8
  val R      = WARL(           0, wNoFilter).withReset(false.B)
  val W      = WARL(           1, wNoFilter).withReset(false.B)
  val X      = WARL(           2, wNoFilter).withReset(false.B)
  val A      = PMPCfgAField(4, 3, wNoFilter).withReset(PMPCfgAField.OFF)
  val ATOMIC = RO(5).withReset(false.B)           // res(0), unuse in pmp
  val C      = RO(6).withReset(false.B)           // res(1), unuse in pmp
  val L      = PMPCfgLField(   7, wNoFilter).withReset(PMPCfgLField.UNLOCKED)
}

object PMPCfgLField extends CSREnum with WARLApply {
  val UNLOCKED = Value(0.U)

  def locked(cfg: PMPCfgBundle): Bool = cfg.L.asBool
  def addrLocked(cfg: PMPCfgBundle): Bool = locked(cfg)
  def addrLocked(cfg: PMPCfgBundle, next: PMPCfgBundle): Bool = locked(cfg) || (locked(next) && PMPCfgAField.tor(next))
}

object PMPCfgAField extends CSREnum with WARLApply {
  val OFF   = Value(0.U)  // Null region(disabled)
  val TOR   = Value(1.U)  // Top of range
  val NA4   = Value(2.U)  // Naturally aligned four-byte region
  val NAPOT = Value(3.U)  // Naturally aligned power-of-two region, ≥ 8 bytes

  def off(cfg: PMPCfgBundle): Bool = cfg.A.asUInt === 0.U
  def tor(cfg: PMPCfgBundle): Bool = cfg.A.asUInt === 1.U
  def na4  (cfg: PMPCfgBundle)(implicit p: Parameters): Bool = { if (CoarserGrain) false.B         else cfg.A.asUInt === 2.U }
  def napot(cfg: PMPCfgBundle)(implicit p: Parameters): Bool = { if (CoarserGrain) cfg.A.asUInt(1) else cfg.A.asUInt === 3.U }
  def isOffOrTor  (cfg: PMPCfgBundle): Bool = !cfg.A.asUInt(1)
  def isNa4OrNapot(cfg: PMPCfgBundle): Bool =  cfg.A.asUInt(1)

  val PMPOffBits = 2 // minimal 4bytes
  def CoarserGrain(implicit p: Parameters): Boolean = p(PMParameKey).PlatformGrain > PMPOffBits
}

class PMPAddrBundle extends CSRBundle {
  val ADDRESS  = WARL(PMPAddrBits-1,  0, wNoFilter).withReset(false.B)
}

trait HasPMPCfgRSink { self: CSRModule[_] =>
  val cfgRData = IO(Input(UInt((p(PMParameKey).NumPMP/8 * p(XLen)).W)))
}

trait HasPMPAddrSink { self: CSRModule[_] =>
  val addrRData = IO(Input(Vec(p(PMParameKey).NumPMP, UInt(64.W))))
}