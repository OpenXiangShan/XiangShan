package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.CSRs
import utility.GatedValidRegNext
import xiangshan.backend.fu.NewCSR.CSRDefines.{CSRROField => RO, CSRRWField => RW, CSRWARLField => WARL}
import xiangshan.backend.fu.NewCSR.CSRFunc._
import xiangshan.backend.fu.vector.Bundles._
import xiangshan.backend.fu.NewCSR.CSRConfig._
import xiangshan.backend.fu.fpu.Bundles.{Fflags, Frm}
import xiangshan.backend.fu.NewCSR.CSREnumTypeImplicitCast._

import scala.collection.immutable.SeqMap

trait Unprivileged { self: NewCSR with MachineLevel with SupervisorLevel =>

  val fcsr = Module(new CSRModule("Fcsr", new CSRBundle {
    val NX = WARL(0, wNoFilter)
    val UF = WARL(1, wNoFilter)
    val OF = WARL(2, wNoFilter)
    val DZ = WARL(3, wNoFilter)
    val NV = WARL(4, wNoFilter)
    val FRM = WARL(7, 5, wNoFilter)
  }) with HasRobCommitBundle {
    val wAliasFflags = IO(Input(new CSRAddrWriteBundle(new CSRFFlagsBundle)))
    val wAliasFfm = IO(Input(new CSRAddrWriteBundle(new CSRFrmBundle)))
    val fflags = IO(Output(Fflags()))
    val frm = IO(Output(Frm()))
    val fflagsRdata = IO(Output(Fflags()))
    val frmRdata = IO(Output(Frm()))

    // write connection
    this.wfn(reg)(Seq(wAliasFflags, wAliasFfm))

    when (robCommit.fflags.valid) {
      reg.NX := robCommit.fflags.bits(0) || reg.NX
      reg.UF := robCommit.fflags.bits(1) || reg.UF
      reg.OF := robCommit.fflags.bits(2) || reg.OF
      reg.DZ := robCommit.fflags.bits(3) || reg.DZ
      reg.NV := robCommit.fflags.bits(4) || reg.NV
    }

    // read connection
    fflags := reg.asUInt(4, 0)
    frm := reg.FRM.asUInt

    fflagsRdata := fflags.asUInt
    frmRdata := frm.asUInt
  }).setAddr(CSRs.fcsr)

  // vec
  val vstart = Module(new CSRModule("Vstart", new CSRBundle {
    // vstart is not a WARL CSR.
    // Since we need to judge whether flush pipe by vstart being not 0 in DecodeStage, vstart must be initialized to some value at reset.
    val vstart = RW(VlWidth - 2, 0).withReset(0.U) // hold [0, 128)
  }) with HasRobCommitBundle {
    // Todo make The use of vstart values greater than the largest element index for the current SEW setting is reserved.
    // Not trap
    when (wen) {
      reg.vstart := this.w.wdata(VlWidth - 2, 0)
    }.elsewhen (robCommit.vsDirty && !robCommit.vstart.valid) {
      reg.vstart := 0.U
    }.elsewhen (robCommit.vstart.valid) {
      reg.vstart := robCommit.vstart.bits
    }.otherwise {
      reg := reg
    }
  })
    .setAddr(CSRs.vstart)

  val vcsr = Module(new CSRModule("Vcsr", new CSRBundle {
    val VXSAT = RW(   0)
    val VXRM  = RW(2, 1)
  }) with HasRobCommitBundle {
    val wAliasVxsat = IO(Input(new CSRAddrWriteBundle(new CSRBundle {
      val VXSAT = RW(0)
    })))
    val wAlisaVxrm = IO(Input(new CSRAddrWriteBundle(new CSRBundle {
      val VXRM = RW(1, 0)
    })))
    val vxsat = IO(Output(Vxsat()))
    val vxrm  = IO(Output(Vxrm()))

    // write connection
    this.wfn(reg)(Seq(wAliasVxsat, wAlisaVxrm))

    when(robCommit.vxsat.valid) {
      reg.VXSAT := reg.VXSAT.asBool || robCommit.vxsat.bits.asBool
    }

    // read connection
    vxsat := reg.VXSAT.asUInt
    vxrm  := reg.VXRM.asUInt
  }).setAddr(CSRs.vcsr)

  val vl = Module(new CSRModule("Vl", new CSRBundle {
    val VL = RO(VlWidth - 1, 0).withReset(0.U)
  }))
    .setAddr(CSRs.vl)

  val vtype = Module(new CSRModule("Vtype", new CSRVTypeBundle) with HasRobCommitBundle {
    when(robCommit.vtype.valid) {
      reg := robCommit.vtype.bits
    }
  })
    .setAddr(CSRs.vtype)

  val vlenb = Module(new CSRModule("Vlenb", new CSRBundle {
    val VLENB = VlenbField(63, 0).withReset(VlenbField.init)
  }))
    .setAddr(CSRs.vlenb)

  val cycle = Module(new CSRModule("cycle", new CSRBundle {
    val cycle = RO(63, 0)
  }) with HasMHPMSink {
    regOut.cycle := mHPM.cycle
  })
    .setAddr(CSRs.cycle)

  val time = Module(new CSRModule("time", new CSRBundle {
    val time = RO(63, 0)
  }) with HasMHPMSink {
    val updated = IO(Output(Bool()))
    val stime  = IO(Output(UInt(64.W)))
    val vstime = IO(Output(UInt(64.W)))

    val stimeTmp  = mHPM.time.bits
    val vstimeTmp = mHPM.time.bits + htimedelta

    when (mHPM.time.valid) {
      reg.time := Mux(v, vstimeTmp, stimeTmp)
    }

    updated := GatedValidRegNext(mHPM.time.valid)
    stime  := stimeTmp
    vstime := vstimeTmp
  })
    .setAddr(CSRs.time)

  val instret = Module(new CSRModule("instret", new CSRBundle {
    val instret = RO(63, 0)
  }) with HasMHPMSink {
    regOut.instret := mHPM.instret
  })
    .setAddr(CSRs.instret)

  val hpmcounters: Seq[CSRModule[_]] = (3 to 0x1F).map(num =>
    Module(new CSRModule(s"Hpmcounter$num", new CSRBundle {
      val hpmcounter = RO(63, 0).withReset(0.U)
    }) with HasMHPMSink {
      regOut.hpmcounter := mHPM.hpmcounters(num - 3)
    }).setAddr(CSRs.cycle + num)
  )

  val unprivilegedCSRMap: SeqMap[Int, (CSRAddrWriteBundle[_], UInt)] = SeqMap(
    CSRs.fflags -> (fcsr.wAliasFflags -> fcsr.fflagsRdata),
    CSRs.frm    -> (fcsr.wAliasFfm    -> fcsr.frmRdata),
    CSRs.fcsr   -> (fcsr.w            -> fcsr.rdata),
    CSRs.vstart -> (vstart.w          -> vstart.rdata),
    CSRs.vxsat  -> (vcsr.wAliasVxsat  -> vcsr.vxsat),
    CSRs.vxrm   -> (vcsr.wAlisaVxrm   -> vcsr.vxrm),
    CSRs.vcsr   -> (vcsr.w            -> vcsr.rdata),
    CSRs.vl     -> (vl.w              -> vl.rdata),
    CSRs.vtype  -> (vtype.w           -> vtype.rdata),
    CSRs.vlenb  -> (vlenb.w           -> vlenb.rdata),
    CSRs.cycle  -> (cycle.w           -> cycle.rdata),
    CSRs.time   -> (time.w            -> time.rdata),
    CSRs.instret -> (instret.w        -> instret.rdata),
  ) ++ hpmcounters.map(counter => (counter.addr -> (counter.w -> counter.rdata)))

  val unprivilegedCSRMods: Seq[CSRModule[_]] = Seq(
    fcsr,
    vcsr,
    vstart,
    vl,
    vtype,
    vlenb,
    cycle,
    time,
    instret,
  ) ++ hpmcounters

  val unprivilegedCSROutMap: SeqMap[Int, UInt] = SeqMap(
    CSRs.fflags  -> fcsr.fflags.asUInt,
    CSRs.frm     -> fcsr.frm.asUInt,
    CSRs.fcsr    -> fcsr.rdata.asUInt,
    CSRs.vstart  -> vstart.rdata.asUInt,
    CSRs.vxsat   -> vcsr.vxsat.asUInt,
    CSRs.vxrm    -> vcsr.vxrm.asUInt,
    CSRs.vcsr    -> vcsr.rdata.asUInt,
    CSRs.vl      -> vl.rdata.asUInt,
    CSRs.vtype   -> vtype.rdata.asUInt,
    CSRs.vlenb   -> vlenb.rdata.asUInt,
    CSRs.cycle   -> cycle.rdata,
    CSRs.time    -> time.rdata,
    CSRs.instret -> instret.rdata,
  ) ++ hpmcounters.map(counter => (counter.addr -> counter.rdata))
}

class CSRVTypeBundle extends CSRBundle {
  // vtype's vill is initialized to 1, when executing vector instructions
  // which depend on vtype, will raise illegal instruction exception
  val VILL  = RO(  63).withReset(1.U)
  val VMA   = RO(   7).withReset(0.U)
  val VTA   = RO(   6).withReset(0.U)
  val VSEW  = RO(5, 3).withReset(0.U)
  val VLMUL = RO(2, 0).withReset(0.U)
}

class CSRFrmBundle extends CSRBundle {
  val FRM = WARL(2, 0, wNoFilter)
}

class CSRFFlagsBundle extends CSRBundle {
  val NX = WARL(0, wNoFilter)
  val UF = WARL(1, wNoFilter)
  val OF = WARL(2, wNoFilter)
  val DZ = WARL(3, wNoFilter)
  val NV = WARL(4, wNoFilter)
}

object VlenbField extends CSREnum with ROApply {
  val init = Value((VLEN / 8).U)
}

trait HasMHPMSink { self: CSRModule[_] =>
  val mHPM = IO(Input(new Bundle {
    val cycle   = UInt(64.W)
    // ValidIO is used to update time reg
    val time    = ValidIO(UInt(64.W))
    val instret = UInt(64.W)
    val hpmcounters = Vec(perfCntNum, UInt(XLEN.W))
  }))
  val v = IO(Input(Bool()))
  val htimedelta = IO(Input(UInt(64.W)))
}
