package xiangshan.backend.vector.Decoder.DecodeChannel

import chisel3._
import chisel3.experimental.hierarchy.{instantiable, public}
import chisel3.util._
import chisel3.util.experimental.decode.{DecodeBundle, DecodePattern, DecodeTable}
import freechips.rocketchip.rocket.Instructions
import xiangshan.backend.decode.isa.bitfield.{BitFieldsVec, Riscv32BitInst}
import xiangshan.backend.fu.vector.Bundles.{VLmul, VSew}
import xiangshan.backend.vector.Decoder.DecodeChannel.SplitCtlDecoderUtil.InstNfLmulSewPattern
import xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel.{EewField, FpWenField, GpWenField, Src12RevField, VpWenField, VxsatWenField}
import xiangshan.backend.vector.Decoder.DecodeFields._
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.RVVDecodeUtil._
import xiangshan.backend.vector.Decoder.Split.{SplitType, SplitTypeOH}
import xiangshan.backend.vector.Decoder.Types.EnumLMUL
import xiangshan.backend.vector.Decoder.Uop.UopInfoRenameWithIllegal
import xiangshan.backend.vector.Decoder._
import xiangshan.backend.vector._
import xiangshan.backend.vector.util.ChiselTypeExt._
import xiangshan.backend.vector.util.Verilog
import xiangshan.macros.InstanceNameMacro.getVariableName

@instantiable
class VecDecodeChannel(
  instSeq: Seq[VecInstPattern],
  enableM2M4M8: (Boolean, Boolean, Boolean),
) extends Module with HasVectorSettings {
  override def desiredName: String =
    s"VecDecodeChannel${if (m8Enable) "M8" else if (m4Enable) "M4" else if (m2Enable) "M2" else "M1" }"

  val m2Enable = enableM2M4M8._1
  val m4Enable = enableM2M4M8._2
  val m8Enable = enableM2M4M8._3
  val numUopOut: Int = {
    if (m8Enable) 8
    else if (m4Enable) 4
    else if (m2Enable) 2
    else 1
  }

  @public val in = IO(Input(new DecodeChannelInput))
  @public val out = IO(Output(new Bundle {
    val uop = Vec(numUopOut, ValidIO(new VecDecodeChannelOutputUop))
    val uopNumOH = UopNumOH()
  }))
  val instField: Riscv32BitInst with BitFieldsVec = in.rawInst.asTypeOf(new Riscv32BitInst with BitFieldsVec)

  val splitType = Wire(SplitType())
  val splitTypeOH = Wire(SplitTypeOH())

  val allInPatterns: Seq[VecInstPattern] = instSeq
  val allInstWithConfig: Seq[RVVInstWithConfigPattern] = Seq.tabulate(
    allInPatterns.length,
    Sews.all.length,
  ){
    // TODO: optimize scala run time, only vls use sew
    case (i, j) =>
      RVVInstWithConfigPattern(allInPatterns(i), Sews.all(j).toBitPat)
  }.flatten

  val allFields = Seq(
    Src12RevField,
    VdEew1bField,
  )

  println(s"The length of decodeTable in VecDecodeChannel: ${allInPatterns.length}")
  println(s"The length of splitTypeTable in VecDecodeChannel: ${allInPatterns.length}")

  val decodeTable = new DecodeTable(allInPatterns, allFields)
  val splitTypeTable = new DecodeTable(allInstWithConfig, Seq(SplitTypeField))
  val splitTypeOHTable = new DecodeTable(allInstWithConfig, Seq(SplitTypeOHField))

  val decodeResult: DecodeBundle = decodeTable.decode(in.rawInst)
  val splitTypeResult: DecodeBundle = splitTypeTable.decode(in.rawInst ## in.sew)
  val splitTypeOHResult: DecodeBundle = splitTypeOHTable.decode(in.rawInst ## in.sew)

  splitType := splitTypeResult(SplitTypeField)
  splitTypeOH := splitTypeOHResult(SplitTypeOHField)

  val instPatterns: Seq[VecInstPattern] = instSeq.collect {
    case x: VecArithInstPattern => x
    case x: VecMemInstPattern => x
  }

  val splitCtrlDecoder = Module(new SplitCtlDecoder(instPatterns))
  val uopNumDecoder = Module(new UopNumDecoder(splitTypeOneHot = true))
  val vsetDecoder = Module(new VsetDecoder)

  splitCtrlDecoder.in.splitTypeOH := splitTypeOH
  splitCtrlDecoder.in.inst := in.rawInst
  splitCtrlDecoder.in.lmul := in.lmul
  splitCtrlDecoder.in.sew := in.sew

  val eew: UInt = new DecodeTable(VecInstPattern.mem, Seq(EewField)).decode(in.rawInst)(EewField)
  uopNumDecoder.in.splitType := splitType
  uopNumDecoder.in.splitTypeOH := splitTypeOH
  uopNumDecoder.in.lmul := suppressEnumCastWarning(EnumLMUL(in.lmul))
  uopNumDecoder.in.sew := in.sew
  uopNumDecoder.in.eew := eew
  uopNumDecoder.in.nf := instField.NF

  vsetDecoder.in.rawInst := in.rawInst

  for (i <- 0 until numUopOut) {
    out.uop(i).valid := vsetDecoder.out.renameInfo.valid || splitCtrlDecoder.out.uopInfoRename(i).valid
    out.uop(i).bits.renameInfo.uop := Mux(vsetDecoder.out.renameInfo.valid, vsetDecoder.out.renameInfo.bits, splitCtrlDecoder.out.uopInfoRename(i).bits.uop)
    out.uop(i).bits.renameInfo.illegal := Mux(vsetDecoder.out.renameInfo.valid, false.B, splitCtrlDecoder.out.uopInfoRename(i).bits.illegal)

    out.uop(i).bits.src        := Mux(vsetDecoder.out.renameInfo.valid, vsetDecoder.out.src, splitCtrlDecoder.out.uopSrc(i))
    out.uop(i).bits.uopDepend  := Mux(vsetDecoder.out.renameInfo.valid, false.B, splitCtrlDecoder.out.uopDepend(i))
    out.uop(i).bits.vdAlloc    := splitCtrlDecoder.out.vdAlloc(i)
    out.uop(i).bits.src12Rev   := Mux(vsetDecoder.out.renameInfo.valid, false.B, decodeResult(Src12RevField))
    out.uop(i).bits.vdEew1b    := decodeResult(VdEew1bField)
    out.uop(i).bits.uopIdx     := i.U
    out.uop(i).bits.isLastUop  := Mux(vsetDecoder.out.renameInfo.valid, true.B, (i + 1).U === uopNumDecoder.out.uopNumOH)
    out.uop(i).bits.uopNumEncode := Mux1H(Seq(
      (uopNumDecoder.out.uopNumOH(0) || vsetDecoder.out.renameInfo.valid) -> UopNumEncode.n1,
      uopNumDecoder.out.uopNumOH(1) -> UopNumEncode.n2,
      uopNumDecoder.out.uopNumOH(2) -> UopNumEncode.n4,
      uopNumDecoder.out.uopNumOH(3) -> UopNumEncode.n8,
    ))
  }

  out.uopNumOH := uopNumDecoder.out.uopNumOH
}

class DecodeChannelInput extends Bundle {
  val rawInst = UInt(32.W)
  val sew = VSew()
  val lmul = VLmul()
}

class VecDecodeChannelOutputUop extends Bundle with HasVectorSettings {
  val renameInfo = new UopInfoRenameWithIllegal
  val src = new UopSrcBundle
  val uopDepend = Bool()
  val vdAlloc = Bool()
  val src12Rev = Bool()
  val vdEew1b = Bool()
  val uopIdx = UInt(3.W)
  val isLastUop = Bool()
  val uopNumEncode = new UopNumEncode {}
}

class SplitCtlDecoder(
  instSeq: Seq[VecInstPattern],
) extends Module with HasVectorSettings {
  import SplitCtlDecoderUtil._

  val in = IO(Input(new Bundle {
    val lmul = VLmul()
    val sew = VSew()
    val splitTypeOH = SplitTypeOH()
    val inst = UInt(32.W)
  }))

  val out = IO(Output(new Bundle {
    val uopInfoRename = Vec(maxSplitUopNum, ValidIO(new UopInfoRenameWithIllegal))
    val uopSrc = Vec(maxSplitUopNum, new UopSrcBundle)
    val uopDepend = UInt(maxSplitUopNum.W)
    val vdAlloc = UInt(maxSplitUopNum.W)
  }))

  val instField: Riscv32BitInst with BitFieldsVec = in.inst.asTypeOf(new Riscv32BitInst with BitFieldsVec)

  val nfSeq = Seq(
    BitPat("b000"),
    BitPat("b001"),
    BitPat("b010"),
    BitPat("b011"),
    BitPat("b100"),
    BitPat("b101"),
    BitPat("b110"),
    BitPat("b111"),
  )

  val decodeResult = {
    val allInPatterns: Seq[UopLmulNfSplitOHPattern] =
      for (
        lmul <- Lmuls.all;
        nf <- nfSeq;
        splitType <- SplitType.all
      ) yield {
        UopLmulNfSplitOHPattern(
          LmulPattern(lmul),
          NfPattern(nf),
          SplitTypeOHPattern(splitType)
        )
      }

    val allFields = Seq(
      SrcSplitSelectField,
      UopDependField,
      VdAllocField,
    )

    println(s"length of UopLmulNfSplitPattern in VecDecodeChannel: ${allInPatterns.length}")

    new DecodeTable(allInPatterns, allFields)
  }.decode(in.lmul ## instField.NF ## in.splitTypeOH.asUInt)

  val arithInstPattern: Seq[InstNfLmulSewPattern] = for {
    inst <- instSeq.collect { case x: VecArithInstPattern => x }
    lmul <- Lmuls.all
  } yield {
    InstNfLmulSewPattern(inst, NfPattern.dontCare, LmulPattern(lmul), SewPattern.dontCare)
  }

  val memWholeInstPattern = for {
    inst <- instSeq.collect {
      case x: VecMemWhole => x
    }
  } yield {
    InstNfLmulSewPattern(inst, NfPattern.dontCare, LmulPattern.dontCare, SewPattern.dontCare)
  }

  val memMaskInstPattern = for {
    inst <- instSeq.collect {
      case x: VecMemMask => x
    }
  } yield {
    InstNfLmulSewPattern(inst, NfPattern.dontCare, LmulPattern.dontCare, SewPattern.dontCare)
  }

  val memSegInstPattern: Seq[InstNfLmulSewPattern] = (
    for {
      inst <- instSeq.collect {
        case x: VecMemInstPattern if !x.isInstanceOf[VecMemWhole] && !x.isInstanceOf[VecMemMask] => x
      }
      nf: BitPat <- nfSeq
      lmul: Double <- Seq(1, 2, 4, 8, 0.125, 0.25, 0.5)
      sew: Int <- Seq(8, 16, 32, 64)
    } yield {
      val seg = nf.value.toInt + 1
      val eew = inst.eewValue
      val emul = lmul * eew / sew
      if (inst.isInstanceOf[VecMemIndex]) {
        val dEmul = lmul
        val iEmul = emul
        val uopNum = (1.0 max iEmul max dEmul).toInt * seg
        if (iEmul >= 0.125 && uopNum <= 8)
          InstNfLmulSewPattern(inst, NfPattern(nf), LmulPattern(lmul), SewPattern(sew))
        else
          null
      }
      else {
        if (seg * emul.max(1.0) <= 8)
          InstNfLmulSewPattern(inst, NfPattern(nf), LmulPattern(lmul), SewPattern(sew))
        else
          null
      }
    }
  ).filter(_ != null)

  val uopCtlResult: DecodeBundle = {
    val allInPatterns: Seq[InstNfLmulSewPattern] = arithInstPattern ++ memWholeInstPattern ++ memMaskInstPattern ++ memSegInstPattern

    val allFields = Seq(
      UopInfoField
    )

    val uopCtlTable = new DecodeTable(allInPatterns, allFields)

    println(s"The length of uopCtlTable in SplitCtlDecoder ${uopCtlTable.table.table.length}")
    uopCtlTable
  }.decode(in.inst ## instField.NF ## in.lmul.value ## in.sew.value)

  val srcDestSelVec: Vec[SrcDestSelect] = decodeResult(SrcSplitSelectField)

  for (i <- 0 until maxSplitUopNum) {
    out.uopSrc(i).dest := Mux1H(Seq(
      srcDestSelVec(i).dest.useVd  -> (instField.VD + srcDestSelVec(i).dest.bias),
      srcDestSelVec(i).dest.useVs1 -> (instField.VS1+ srcDestSelVec(i).dest.bias),
      srcDestSelVec(i).dest.useVs2 -> (instField.VS2+ srcDestSelVec(i).dest.bias),
    ))
    out.uopSrc(i).src1 := Mux1H(Seq(
      srcDestSelVec(i).src1.useVd  -> (instField.VD + srcDestSelVec(i).src1.bias),
      srcDestSelVec(i).src1.useVs1 -> (instField.VS1+ srcDestSelVec(i).src1.bias),
      srcDestSelVec(i).src1.useVs2 -> (instField.VS2+ srcDestSelVec(i).src1.bias),
    ))
    out.uopSrc(i).src2 := Mux1H(Seq(
      srcDestSelVec(i).src2.useVd  -> (instField.VD + srcDestSelVec(i).src2.bias),
      srcDestSelVec(i).src2.useVs1 -> (instField.VS1+ srcDestSelVec(i).src2.bias),
      srcDestSelVec(i).src2.useVs2 -> (instField.VS2+ srcDestSelVec(i).src2.bias),
    ))
  }
  out.uopInfoRename := uopCtlResult(UopInfoField)
  out.uopDepend := decodeResult(UopDependField)
  out.vdAlloc := decodeResult(VdAllocField)
}

object SplitCtlDecoderUtil {
  case class SplitTypeOHPattern(splitTypeOH: BitPat) extends DecodePattern {
    override def bitPat: BitPat = splitTypeOH

    lazy val value: Int = splitTypeOH.value.bitLength - 1
  }

  object SplitTypeOHPattern {
    def apply(splitTypeOH: SplitTypeOH.Type): SplitTypeOHPattern = new SplitTypeOHPattern(splitTypeOH.toBitPat)

    def apply(splitType: SplitType.Type): SplitTypeOHPattern = new SplitTypeOHPattern(
      new BitPat(
        value = BigInt(1) << splitType.litValue.toInt,
        mask = (BigInt(1) << (SplitType.maxValue + 1)) - 1,
        width = SplitType.maxValue + 1,
      )
    )
  }
  
  type UopLmulNfSplitOHPattern =
    DecodePatternComb[
      DecodePatternComb[
        LmulPattern,
        NfPattern,
      ],
      SplitTypeOHPattern
    ]

  object UopLmulNfSplitOHPattern {
    def apply(lmul: LmulPattern, nf: NfPattern, splitTypeOH: SplitTypeOHPattern): UopLmulNfSplitOHPattern = {
      lmul ## nf ## splitTypeOH
    }

    def unapply(arg: UopLmulNfSplitOHPattern): Option[Tuple3[LmulPattern, NfPattern, SplitTypeOHPattern]] = {
      Some(Tuple3(arg.p1.p1, arg.p1.p2, arg.p2))
    }
  }

  type InstNfLmulSewPattern =
    DecodePatternComb[
      DecodePatternComb[
        VecInstPattern,
        NfPattern,
      ],
      DecodePatternComb[
        LmulPattern,
        SewPattern,
      ]
    ]

  object InstNfLmulSewPattern {
    type Type = InstNfLmulSewPattern

    def apply(
      inst: VecInstPattern,
      nf: NfPattern,
      lmul: LmulPattern,
      sew: SewPattern,
    ): Type = (inst ## nf) ## (lmul ## sew)

    def unapply(arg: Type): Option[(VecInstPattern, NfPattern, LmulPattern, SewPattern)] = {
      Some((
        arg.p1.p1,
        arg.p1.p2,
        arg.p2.p1,
        arg.p2.p2,
      ))
    }
  }
}

class UopNumEncode extends Bundle {
  val value = UInt(UopNumEncode.width.W)
}

object UopNumEncode {
  def width = 2
  def n1: UopNumEncode = 0.U.asTypeOf(new UopNumEncode)
  def n2: UopNumEncode = 1.U.asTypeOf(new UopNumEncode)
  def n4: UopNumEncode = 2.U.asTypeOf(new UopNumEncode)
  def n8: UopNumEncode = 3.U.asTypeOf(new UopNumEncode)
}

class UopSrcBundle extends Bundle {
  val src1 = UInt(5.W)
  val src2 = UInt(5.W)
  val dest = UInt(5.W)
}

object VecDecoderChannel {
  def main(args: Array[String]): Unit = {
    Verilog.emitVerilog(
      new VecDecodeChannel(VecInstPattern.all, enableM2M4M8 = (true, true, true))
    )

    val inst = VecInstPattern(Instructions.VNCLIPU_WI).get
    inst.setName("VNCLIPU_WI")
    checkFieldsOfInst(inst)
  }

  def checkFieldsOfInst(inst: VecInstPattern): Unit = {
    val fields = Seq(
      GpWenField,
      FpWenField,
      VpWenField,
      Src1TypeField,
      Src2TypeField,
      Src12RevField,
      VdWidenField,
      Vs2WidenField,
      VdEew1bField,
      VxsatWenField,
      AlwaysReadVdField,
    )

    for (field <- fields) {
      println(s"${field.name} = ${field.genTable(inst)}")
    }
  }
}

object SplitCtlDecoderMain extends App {
  val instPatterns = VecInstPattern.all.filter(x => x.isInstanceOf[VecArithInstPattern] &&
    Seq("011" ,"000", "100", "001", "101", "010", "110").contains(x.asInstanceOf[VecArithInstPattern].category.rawString))

  Verilog.emitVerilog(
    new SplitCtlDecoder(instPatterns)
  )

  val inst = VecInstPattern(Instructions.VWREDSUMU_VS).get
  inst.setName(getVariableName(Instructions.VWREDSUMU_VS))
  val lmul = LmulPattern(VLmul.m4.toBitPat)

  checkFields(inst, NfPattern.dontCare, LmulPattern(VLmul.m4), SewPattern(VSew.e8))

  def checkFields(inst: VecInstPattern, nf: NfPattern, lmul: LmulPattern, sew: SewPattern) = {
    val fields = Seq(
      UopInfoField
    )

    for (field <- fields) {
      field.genUopSeq(InstNfLmulSewPattern(inst, nf, lmul, sew)).map(_.uopInfoRenameString).foreach(println)
    }
  }
}
