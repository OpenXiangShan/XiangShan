package xiangshan.backend.fu.NewCSR

import chisel3._
import xiangshan.backend.fu.NewCSR.CSRDefines._
import xiangshan.backend.fu.NewCSR.CSRDefines.{
  CSRWARLField => WARL,
  CSRROField => RO,
}
import xiangshan.backend.fu.NewCSR.CSRFunc._
import xiangshan.backend.fu.vector.Bundles._

import scala.collection.immutable.SeqMap

trait Unprivileged { self: NewCSR with MachineLevel with SupervisorLevel =>

  val fcsr = Module(new CSRModule("Fcsr", new CSRBundle {
    val NX = WARL(0, wNoFilter)
    val UF = WARL(1, wNoFilter)
    val OF = WARL(2, wNoFilter)
    val DZ = WARL(3, wNoFilter)
    val NV = WARL(4, wNoFilter)
    val FRM = WARL(7, 5, wNoFilter)
  }) {
    val wAliasFflags = IO(Input(new CSRAddrWriteBundle(new CSRBundle {
      val NX = WARL(0, wNoFilter)
      val UF = WARL(1, wNoFilter)
      val OF = WARL(2, wNoFilter)
      val DZ = WARL(3, wNoFilter)
      val NV = WARL(4, wNoFilter)
    })))
    val wAliasFfm = IO(Input(new CSRAddrWriteBundle(new CSRBundle {
      val FRM = WARL(2, 0, wNoFilter)
    })))
    val fflags = IO(Output(UInt(64.W)))
    val frm = IO(Output(UInt(64.W)))

    // write connection
    this.wfn(reg)(Seq(wAliasFflags, wAliasFfm))

    // read connection
    fflags := reg.asUInt(4, 0)
    frm := reg.FRM.asUInt
  }).setAddr(0x003)

  // vec
  val vstart = Module(new CSRModule("vstart"))
    .setAddr(0x008)

  val vcsr = Module(new CSRModule("Vcsr", new CSRBundle {
    val VXSAT = WARL(0, wNoFilter)
    val VXRM  = WARL(2, 1, wNoFilter)
  }) {
    val wAliasVxsat = IO(Input(new CSRAddrWriteBundle(new CSRBundle {
      val VXSAT = WARL(0, wNoFilter)
    })))
    val wAlisaVxrm = IO(Input(new CSRAddrWriteBundle(new CSRBundle {
      val VXRM = WARL(1, 0, wNoFilter)
    })))
    val vxsat = IO(Output(Vxsat()))
    val vxrm  = IO(Output(Vxrm()))

    // write connection
    this.wfn(reg)(Seq(wAliasVxsat, wAlisaVxrm))

    // read connection
    vxsat := reg.VXSAT.asUInt
    vxrm  := reg.VXRM.asUInt
  }).setAddr(0x00F)

  val vl = Module(new CSRModule("vl"))
    .setAddr(0xC20)

  val vtype = Module(new CSRModule("vtype", new VtypeBundle))
    .setAddr(0xC21)

  val vlenb = Module(new CSRModule("vlenb"))
    .setAddr(0xC22)

  val unprivilegedCSRMap: SeqMap[Int, (CSRAddrWriteBundle[_], Data)] = SeqMap(
    0x001 -> (fcsr.wAliasFflags -> fcsr.fflags),
    0x002 -> (fcsr.wAliasFfm    -> fcsr.frm),
    0x003 -> (fcsr.w            -> fcsr.rdata),
    0x008 -> (vstart.w          -> vstart.rdata),
    0x009 -> (vcsr.wAliasVxsat  -> vcsr.vxsat),
    0x00A -> (vcsr.wAlisaVxrm   -> vcsr.vxrm),
    0x00F -> (vcsr.w            -> vcsr.rdata),
    0xC20 -> (vl.w              -> vl.rdata),
    0xC21 -> (vtype.w           -> vtype.rdata),
    0xC22 -> (vlenb.w           -> vlenb.rdata),
  )

  val unprivilegedCSRMods: Seq[CSRModule[_]] = Seq(
    fcsr,
    vcsr,
    vstart,
    vl,
    vtype,
    vlenb,
  )

  val unprivilegedCSROutMap: SeqMap[Int, UInt] = SeqMap(
    0x001 -> fcsr.fflags.asUInt,
    0x002 -> fcsr.frm.asUInt,
    0x003 -> fcsr.rdata.asUInt,
    0x008 -> vcsr.rdata.asUInt,
    0x009 -> vcsr.vxsat.asUInt,
    0x00A -> vcsr.vxrm.asUInt,
    0x00F -> vcsr.rdata.asUInt,
    0xC20 -> vl.rdata.asUInt,
    0xC21 -> vtype.rdata.asUInt,
    0xC22 -> vlenb.rdata.asUInt,
  )
}

class VtypeBundle extends CSRBundle {
  val VILL  = RO(  63)
  val VMA   = RO(   7)
  val VTA   = RO(   6)
  val VSEW  = RO(5, 3)
  val VLMUL = RO(2, 0)
}
