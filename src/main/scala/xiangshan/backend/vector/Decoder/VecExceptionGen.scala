package xiangshan.backend.vector.Decoder

import chisel3._
import chisel3.experimental.hierarchy.{instantiable, public}
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import top.ArgParser
import xiangshan.XSModule
import xiangshan.backend.decode.isa.bitfield.{BitFieldsVec, Riscv32BitInst}
import xiangshan.backend.fu.vector.Bundles._
import xiangshan.XSCoreParamsKey
import xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel.VRegTypes

class VecExceptionDecodes extends Bundle {
  val isIllInst = Bool()
  val isVtypeIgnore = Bool()
  val isVecFPInst = Bool()
  val isVstartForceZero = Bool()
  val maskUsed = Bool()
  val vregTypes = new VRegTypes()
  val isSegment = Bool()
  val nffield = UInt(3.W)
  val isSrcDstOverlapIgnore = Bool()
  val isSrcdstOverlapStrict = Bool() // slideup and other
}

class VecExceptionContexts(implicit p: Parameters) extends Bundle {
  val isVsOff = Bool()
  val isFSOff = Bool()
  val isfrmIll = Bool()
  val vill = Bool()
//  val sew = VSew()
//  val lmul = VLmul()
  val vstart = Vstart()
  val csrAllowSrcDstOverlap = Bool() // Reserved
}

class VecExceptionGenInput(implicit p: Parameters) extends Bundle {
  val rawInst = UInt(32.W)
  val Decodes = new VecExceptionDecodes()
  val Contexts = new VecExceptionContexts()
}

class VecExceptionGenOutput extends Bundle {
  val exception = Bool()
  val mtAgnostic = Bool() // when src-dst overlap with eew diff
}

class VecExceptionGen(implicit p: Parameters) extends XSModule
{
    val in = IO(Input(new VecExceptionGenInput()))
    val out = IO(Output(new VecExceptionGenOutput()))

    val Dec = in.Decodes
    val Ctx = in.Contexts
    val Reg = Dec.vregTypes
    val instFields : Riscv32BitInst with BitFieldsVec = in.rawInst.asTypeOf(new Riscv32BitInst with BitFieldsVec)

    // 1.1 VS off
    val vsOff = Ctx.isVsOff

    // 1.2 FS off with vfp
    val fsOff = Ctx.isFSOff && Dec.isVecFPInst

    // 1.3 frm Ill with vfp
    val frmIll = Ctx.isfrmIll && Dec.isVecFPInst

    // 2. inst Ill
    val isIllInst = Dec.isIllInst

    // 3.1 vill set
    val villSet = Ctx.vill && !Dec.isVtypeIgnore

    // 3.2 vstart larger than vlmax

//    val vlenb = (p(XSCoreParamsKey).VLEN / 8).U
//    val baseElemMax = vlenb >> Ctx.sew
//    val lmulSInt = Ctx.lmul.asSInt
//    val lmulIsFrac = lmulSInt < 0.S
//    val lmulShift = Mux(lmulIsFrac, (-lmulSInt).asUInt, lmulSInt.asUInt)
//    val vlmax = Mux(lmulIsFrac,
//      baseElemMax >> lmulShift,
//      baseElemMax << lmulShift
//    )
//    val vstartTooLarge = Ctx.vstart >= vlmax

    // 3.3 vstart not zero
    val vstartNotZero = Ctx.vstart =/= 0.U && Dec.isVstartForceZero

    // 4.1 vreg not align
    def vregNotAlign(emul: UInt, rs: UInt): Bool = {
      val rmul = VLmul.makeNoLessThanM1(emul)
      MuxLookup(rmul, false.B)(Seq(
        VLmul.m2 -> rs(0),
        VLmul.m4 -> rs(1, 0).orR,
        VLmul.m8 -> rs(2, 0).orR,
      ))
    }

    val vs1NotAlign = Reg.useVs1 && vregNotAlign(Reg.emulVs1, instFields.VS1)
    val vs2NotAlign = Reg.useVs2 && vregNotAlign(Reg.emulVs2, instFields.VS2)
    val vs3NotAlign = Reg.useVs3 && vregNotAlign(Reg.emulVs3, instFields.VD)
    val vdNotAlign  = Reg.useVd  && vregNotAlign(Reg.emulVs3, instFields.VD)

    // 4.2 vreg out of bound

    def emulLoToHi(emul: UInt, Lo: UInt): UInt = {
        MuxLookup(emul, Lo(5,0))(Seq(
        VLmul.m2 -> Cat(Lo(5,1), 1.U(1.W)),
        VLmul.m4 -> Cat(Lo(5,2), 3.U(2.W)),
        VLmul.m8 -> Cat(Lo(5,3), 7.U(3.W)),
      ))
    }

    // helper: check if two register ranges [lo1, hi1] and [lo2, hi2] overlap
    def regRangeOverlap(lo1: UInt, hi1: UInt, lo2: UInt, hi2: UInt): Bool = {
      !(hi1 < lo2 || hi2 < lo1)
    }

    val vs1RegLo = Cat(0.U(1.W), instFields.VS1)
    val vs1RegHi = emulLoToHi(Reg.emulVs1, vs1RegLo)
    val vs2RegLo = Cat(0.U(1.W), instFields.VS2)
    val vs2RegHi = emulLoToHi(Reg.emulVs2, vs2RegLo)
    val vs3RegLo = Cat(0.U(1.W), instFields.VD)
    val vs3RegHi = emulLoToHi(Reg.emulVs3, vs3RegLo)

    val vs1OutOfBound = Reg.useVs1 && (vs1RegHi >= 32.U)
    val vs2OutOfBound = Reg.useVs2 && (vs2RegHi >= 32.U)
    val vs3OutOfBound = Reg.useVs3 && (vs3RegHi >= 32.U)
    val vdOutOfBound  = Reg.useVd  && (vs3RegHi >= 32.U)

    // 4.3 mask src overlap

// Reserved
//  val vm0vs1Overlap = Dec.maskUsed && Reg.useVs1 && vs1RegLo === 0.U && !Reg.eewVs1EqVm
//  val vm0vs2Overlap = Dec.maskUsed && Reg.useVs2 && vs2RegLo === 0.U && !Reg.eewVs2EqVm
//  val vm0vs3Overlap = Dec.maskUsed && Reg.useVs3 && vs3RegLo === 0.U && !Reg.eewVs3EqVm

    // 4.4 src src overlap

//    val vs1vs2Overlap = Dec.vs1.used && Dec.vs2.used && regRangeOverlap(vs1RegLo, vs1RegHi, vs2RegLo, vs2RegHi)
//    val vs1vs3Overlap = Dec.vs1.used && Dec.vs3.used && regRangeOverlap(vs1RegLo, vs1RegHi, vs3RegLo, vs3RegHi)
//    val vs2vs3Overlap = Dec.vs2.used && Dec.vs3.used && regRangeOverlap(vs2RegLo, vs2RegHi, vs3RegLo, vs3RegHi)

//    val vs1vs2OverlapIll = vs1vs2EEWDiff && vs1vs2Overlap
//    val vs1vs3OverlapIll = vs1vs3EEWDiff && vs1vs3Overlap
//    val vs2vs3OverlapIll = vs2vs3EEWDiff && vs2vs3Overlap

    // 4.5 mask dst overlap

    val vm0vdOverlap = Dec.maskUsed && Reg.useVd && vs3RegLo === 0.U
    val vm0vdOEEWDiffSpecial = Dec.isSrcDstOverlapIgnore

    val vm0vdOverlapIll = vm0vdOverlap && !Ctx.csrAllowSrcDstOverlap && 
                         (!Reg.eewVs3EqVm && !Dec.isSrcDstOverlapIgnore || Dec.isSrcdstOverlapStrict)

    // 4.6 src dst overlap

    val vs1vdOverlap = Reg.useVs1 && Reg.useVd && regRangeOverlap(vs1RegLo, vs1RegHi, vs3RegLo, vs3RegHi)
    val vs2vdOverlap = Reg.useVs2 && Reg.useVd && regRangeOverlap(vs2RegLo, vs2RegHi, vs3RegLo, vs3RegHi)

    val vs1vdEEWDiffSpecial = Reg.eewVs1LtVs3 && vs1RegHi === vs3RegHi && Reg.emulVs1(2) === 0.U || 
                              !Reg.eewVs1LtVs3 && vs1RegLo === vs3RegLo ||
                               Dec.isSrcDstOverlapIgnore
                               
    val vs2vdEEWDiffSpecial = Reg.eewVs2LtVs3 && vs2RegHi === vs3RegHi && Reg.emulVs2(2) === 0.U || 
                             !Reg.eewVs2LtVs3 && vs2RegLo === vs3RegLo ||
                              Dec.isSrcDstOverlapIgnore

    val vs1vdOverlapIll = vs1vdOverlap && !Ctx.csrAllowSrcDstOverlap &&
                         (!Reg.eewVs1EqVs3 && !vs1vdEEWDiffSpecial || Dec.isSrcdstOverlapStrict)
    val vs2vdOverlapIll = vs2vdOverlap && !Ctx.csrAllowSrcDstOverlap &&
                         (!Reg.eewVs2EqVs3 && !vs2vdEEWDiffSpecial || Dec.isSrcdstOverlapStrict)
    
    def emulNfLoToHi(emul: UInt, nf: UInt, Lo: UInt): UInt = {
        MuxLookup(emul, Lo(5,0) + nf(2, 0))(Seq(
        VLmul.m2 -> Cat(Lo(5,1) + nf(1, 0), 1.U(1.W)),
        VLmul.m4 -> Cat(Lo(5,2) + nf(0)   , 3.U(2.W)),
        VLmul.m8 -> Cat(Lo(5,3)           , 7.U(3.W)), // max len is 8
      ))
    }
    
    val vs3RegHiWithNf = emulNfLoToHi(Reg.emulVs3, Dec.nffield, vs3RegLo)
    val segmentOverlap = Reg.useVs2 && Reg.useVd && Dec.isSegment &&
                      regRangeOverlap(vs2RegLo, vs2RegHi, vs3RegLo, vs3RegHiWithNf)

    out.exception := Seq(
      vsOff,
      fsOff,
      frmIll,
      isIllInst,
      villSet,
      vstartNotZero,
      // vstartTooLarge,
      vs1NotAlign,
      vs2NotAlign,
      vs3NotAlign,
      vdNotAlign,
      vs1OutOfBound,
      vs2OutOfBound,
      vs3OutOfBound,
      vdOutOfBound,
      // vm0vs1Overlap,
      // vm0vs2Overlap,
      // vm0vs3Overlap,
      // vs1vs2OverlapIll,
      // vs1vs3OverlapIll,
      // vs2vs3OverlapIll,
      vm0vdOverlapIll,
      vs1vdOverlapIll,
      vs2vdOverlapIll,
      segmentOverlap,
    ).reduce(_ || _)

    out.mtAgnostic := vm0vdOverlap && !Reg.eewVs3EqVm  ||
                      vs1vdOverlap && !Reg.eewVs1EqVs3 || 
                      vs2vdOverlap && !Reg.eewVs2EqVs3
                      
    
    dontTouch(vs1RegLo)
    dontTouch(vs1RegHi)
    dontTouch(vs2RegLo)
    dontTouch(vs2RegHi)
    dontTouch(vs3RegLo)
    dontTouch(vs3RegHi)
    dontTouch(vs3RegHiWithNf)
}
