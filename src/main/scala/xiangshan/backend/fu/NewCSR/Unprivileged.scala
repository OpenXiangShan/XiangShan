package xiangshan.backend.fu.NewCSR

import chisel3._
import xiangshan.backend.fu.NewCSR.CSRDefines._
import xiangshan.backend.fu.NewCSR.CSRDefines.{CSRROField => RO}
import xiangshan.backend.fu.NewCSR.CSRFunc._
import xiangshan.backend.fu.vector.Bundles._

import scala.collection.immutable.SeqMap

trait Unprivileged { self: NewCSR with MachineLevel with SupervisorLevel =>

  val fcsr = Module(new CSRModule("Fcsr", new CSRBundle {
    val NX = CSRWARLField(0, wNoFilter)
    val UF = CSRWARLField(1, wNoFilter)
    val OF = CSRWARLField(2, wNoFilter)
    val DZ = CSRWARLField(3, wNoFilter)
    val NV = CSRWARLField(4, wNoFilter)
    val FRM = CSRWARLField(7, 5, wNoFilter)
  }) {
    val wAliasFflags = IO(Input(new CSRAddrWriteBundle(new CSRBundle {
      val NX = CSRWARLField(0, wNoFilter)
      val UF = CSRWARLField(1, wNoFilter)
      val OF = CSRWARLField(2, wNoFilter)
      val DZ = CSRWARLField(3, wNoFilter)
      val NV = CSRWARLField(4, wNoFilter)
    })))
    val wAliasFfm = IO(Input(new CSRAddrWriteBundle(new CSRBundle {
      val FRM = CSRWARLField(2, 0, wNoFilter)
    })))
    val fflags = IO(Output(UInt(64.W)))
    val frm = IO(Output(UInt(64.W)))

    // write connection
    this.wfn(reg)(Seq(wAliasFflags, wAliasFfm))

    // read connection
    fflags := reg.asUInt(4, 0)
    frm := reg.FRM.asUInt
  }).setAddr(0x003)

  // vector
  val vstart = Module(new CSRModule("vstart"))
    .setAddr(0x008)

  val vcsr = Module(new CSRModule("Vcsr", new CSRBundle {
    val VXSAT = CSRWARLField(0, wNoFilter)
    val VXRM = CSRWARLField(2, 1, wNoFilter)
  }) {
    val wAliasVxsat = IO(Input(new CSRAddrWriteBundle(new CSRBundle {
      val VXSAT = CSRWARLField(0, wNoFilter)
    })))
    val wAliasVxrm = IO(Input(new CSRAddrWriteBundle(new CSRBundle {
      val VXRM = CSRWARLField(1, 0, wNoFilter)
    })))
    val vxrm = IO(Output(Vxrm()))
    val vxsat = IO(Output(Vxsat()))

    // write connection
    this.wfn(reg)(Seq(wAliasVxsat, wAliasVxrm))

    // read connection
    vxsat := reg.VXSAT.asUInt
    vxrm := reg.VXRM.asUInt
  }).setAddr(0x00F)

  val vl = Module(new CSRModule("vl"))
    .setAddr(0xC20)

  val vtype = Module(new CSRModule("vtype", new VtypeBundle))
    .setAddr(0xC21)

  val vlenb = Module(new CSRModule("vlenb"))
    .setAddr(0xC22)

  val unprivilegedCSRMap: SeqMap[Int, (CSRAddrWriteBundle[_], Data)] = SeqMap(
    0x001 -> (fcsr.wAliasFflags -> fcsr.fflags),
    0x002 -> (fcsr.wAliasFfm -> fcsr.frm),
    0x003 -> (fcsr.w -> fcsr.rdata),
    0x008 -> (vstart.w -> vstart.rdata),
    0x009 -> (vcsr.wAliasVxsat -> vcsr.vxsat),
    0x00A -> (vcsr.wAliasVxrm -> vcsr.vxrm),
    0x00F -> (vcsr.w -> vcsr.rdata),
    0xC20 -> (vl.w -> vl.rdata),
    0xC21 -> (vtype.w -> vtype.rdata),
    0xC22 -> (vlenb.w -> vlenb.rdata),
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
    0x008 -> vstart.rdata.asUInt,
    0x009 -> vcsr.vxsat.asUInt,
    0x00A -> vcsr.vxrm.asUInt,
    0x00F -> vcsr.rdata.asUInt,
    0xC20 -> vl.rdata.asUInt,
    0xC21 -> vtype.rdata.asUInt,
    0xC22 -> vlenb.rdata.asUInt,
  )
}

class VtypeBundle extends CSRBundle {
  val VLMUL = RO(2, 0)
  val VSEW  = RO(5, 3)
  val VTA   = RO(   6)
  val VMA   = RO(   7)
  val VILL  = RO(  63)
}