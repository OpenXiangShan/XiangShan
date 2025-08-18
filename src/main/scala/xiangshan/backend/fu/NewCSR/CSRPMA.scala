package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.CSRs
import freechips.rocketchip.tile.XLen
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.fu.NewCSR.CSRDefines.{CSRWARLField => WARL}
import xiangshan.backend.fu.NewCSR.CSRFunc._
import xiangshan.backend.fu.PMAConfigEntry
import xiangshan.backend.fu.util.CSRConst
import xiangshan.{HasPMParameters, PMParameKey}

import scala.collection.immutable.SeqMap

trait CSRPMA { self: NewCSR =>
  val pmacfg: Seq[CSRModule[_]] = Range(0, p(PMParameKey).NumPMA/8+1, 2).map(num =>
    Module(new CSRModule(s"Pmacfg$num") with HasPMACfgRSink {
      // read condition
      regOut := cfgRData(64*(num/2+1)-1, 64*num/2)
    })
      .setAddr(CSRConst.PmacfgBase + num)
  )

  // every pmacfg has 8 cfgs
  val pmacfgs: Seq[CSRModule[_]] = Range(0, p(PMParameKey).NumPMA).map(num =>
    Module(new CSRModule(s"Pma$num"+"cfg", new PMACfgInitBundle(num)))
  )

  val pmaaddr: Seq[CSRModule[_]] = Range(0, p(PMParameKey).NumPMA).map(num =>
    Module(new CSRModule(s"Pmaaddr$num") with HasPMAAddrSink {
      // read condition
      regOut := addrRegOut(num)
      rdata := addrRData(num)
    })
      .setAddr(CSRConst.PmaaddrBase + num)
  )

  val pmaCSRMods: Seq[CSRModule[_]] = pmacfg ++ pmaaddr

  val pmaCSRMap: SeqMap[Int, (CSRAddrWriteBundle[_], UInt)] = SeqMap.from(
    pmaCSRMods.map(csr => csr.addr -> (csr.w -> csr.rdata)).iterator
  )

  val pmaCSROutMap: SeqMap[Int, UInt] = SeqMap.from(
    pmaCSRMods.map(csr => csr.addr -> csr.regOut.asInstanceOf[CSRBundle].asUInt).iterator
  )

  private val pmaCfgRead = Cat(pmacfgs.map(_.rdata(7, 0)).reverse)

  pmaCSRMods.foreach { mod =>
    mod match {
      case m: HasPMACfgRSink =>
        m.cfgRData := pmaCfgRead
      case _ =>
    }
  }
}

class PMACfgInitBundle(num: Int)(implicit val p: Parameters) extends PMACfgBundle with PMAInit {
  override val R      = WARL(0, wNoFilter).withReset(pmaInit(num).r.B)
  override val W      = WARL(1, wNoFilter).withReset(pmaInit(num).w.B)
  override val X      = WARL(2, wNoFilter).withReset(pmaInit(num).x.B)
  override val A      = PMPCfgAField(4, 3, wNoFilter).withReset(if (pmaInit(num).a > 0) pmaInit(num).a.U else 0.U)
  override val ATOMIC = WARL(5, wNoFilter).withReset(pmaInit(num).atomic.B)
  override val C      = WARL(6, wNoFilter).withReset(pmaInit(num).c.B)
  override val L      = PMPCfgLField(7, wNoFilter).withReset(pmaInit(num).l.B)
}

class PMACfgBundle extends PMPCfgBundle {
  override val ATOMIC = WARL(5, wNoFilter).withReset(false.B)
  override val C      = WARL(6, wNoFilter).withReset(false.B)
}

trait HasPMACfgRSink { self: CSRModule[_] =>
  val cfgRData = IO(Input(UInt((p(PMParameKey).NumPMA/8 * p(XLen)).W)))
}

trait HasPMAAddrSink { self: CSRModule[_] =>
  val addrRData = IO(Input(Vec(p(PMParameKey).NumPMA, UInt(64.W))))
  val addrRegOut = IO(Input(Vec(p(PMParameKey).NumPMA, UInt(64.W))))
}

trait PMAInit extends HasPMParameters with PMAReadWrite {
  def pmaInit: Seq[PMAConfigEntry] = (PMAConfigs ++ Seq.fill(NumPMA-PMAConfigs.length)(PMAConfigEntry(0))).reverse
}
