package xiangshan.backend.vector.Decoder.DecodeChannel

import chisel3._
import chisel3.experimental.hierarchy.{instantiable, public}
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import top.ArgParser
import utils.BundleUtils.makeValid
import xiangshan._
import xiangshan.backend.Bundles.UopIdx
import xiangshan.backend.decode.isa.bitfield.{BitFieldsVec, Riscv32BitInst}
import xiangshan.backend.decode.opcode.Opcode
import xiangshan.backend.decode.opcode.Opcode.Opcode
import xiangshan.backend.fu.FuType
import xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel.{CommitTypeField, FFlagsWenField, FrmRenField}
import xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel._
import xiangshan.backend.vector.Decoder.DecodePatterns.SewLmulPattern
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.RVVDecodeUtil._
import xiangshan.backend.vector.Decoder.Types.{DecodeSelImm, NumWB, VdDepElim}
import xiangshan.backend.vector.Decoder.Uop.UopInfoRenameWithIllegal
import xiangshan.backend.vector.Decoder._
import xiangshan.backend.vector.Decoder.util._
import xiangshan.backend.vector._
import xiangshan.backend.vector.util.Verilog
import xiangshan.{CommitType, XSCoreParameters, XSCoreParamsKey}

@instantiable
class VectorDecodeChannel(
  instSeq: Seq[VecInstPattern],
) (
  implicit val p: Parameters
) extends Module with HasVectorSettings {
  import VectorDecodeChannel._

  @public val in = IO(Input(new DecodeChannelInput))
  @public val out = IO(Output(new VecDecodeChannelOutput(maxSplitUopNum)))

  val rawInst = in.rawInst
  val sew = in.sew
  val lmul = in.lmul

  // Split raw instruction into bit fields for easier decoding
  val instFields: Riscv32BitInst with BitFieldsVec = in.rawInst.asTypeOf(new Riscv32BitInst with BitFieldsVec)

  val nf = instFields.NF

  val srcSelectModule = Module(new SrcSelectModule)

  val instPats: Seq[VecInstPattern] = instSeq

  val instSewPats: Seq[DecodePatternComb2[VecInstPattern, SewPattern]] =
    VecInstPattern.withSew(instSeq.filterNot(_.isInstanceOf[VecCryptoVVVVPattern])) ++
      instSeq.filter(_.isInstanceOf[VecCryptoVVVVPattern]).map(_ ## SewPattern.e32)

  val instSewLmulNfPats: Seq[DecodePatternComb4[VecInstPattern, SewPattern, LmulPattern, NfPattern]] = {
    instSeq.flatMap {
      vi =>
        vi match {
          case vai: VecArithInstPattern =>
            vai match {
              case viai: VecIntArithInstPattern =>
                viai match {
                  case VecIntVVWPattern() |
                       VecIntVVWWPattern() |
                       VecIntClipWVVPattern() |
                       VecIntNarrowShiftWVVPattern() |
                       VecIntWVWPattern() |
                       VecIntWRedPattern() |
                       VecIntS2DVExtF2Pattern() =>
                    for (sewlmul <- SewLmulPattern.widenAll) yield {
                      vi ## sewlmul ## NfPattern.dontCare
                    }
                  case VecIntS2DVExtF8Pattern() =>
                    for (sewlmul <- SewLmulPattern.e8All) yield {
                      vi ## sewlmul ## NfPattern.dontCare
                    }
                  case VecIntS2DVExtF4Pattern() =>
                    for (sewlmul <- SewLmulPattern.e8All ++ SewLmulPattern.e16All) yield {
                      vi ## sewlmul ## NfPattern.dontCare
                    }
                  case VecCryptoVVVVPattern() =>
                    for (sewlmul <- SewLmulPattern.e32All) yield {
                      vi ## sewlmul ## NfPattern.dontCare
                    }
                  case _ =>
                    for (sewlmul <- SewLmulPattern.all) yield {
                      vi ## sewlmul ## NfPattern.dontCare
                    }
                }
              case vfai: VecFpArithInstPattern =>
                vfai match {
                  case VecFpWRedPattern() |
                       VecFpOp2VVWPattern() |
                       VecFpOp2WVWPattern() |
                       VecFpOp3VVWPattern() |
                       VecFpS2VVWPattern() |
                       VecFpS2WVFpPattern() =>
                    for (sewlmul <- SewLmulPattern.fpWidenAll) yield {
                      vi ## sewlmul ## NfPattern.dontCare
                    }
                  case VecFpS2WVIntPattern() =>
                    for (sewlmul <- SewLmulPattern.widenAll) yield {
                      vi ## sewlmul ## NfPattern.dontCare
                    }
                  case _ =>
                    for (sewlmul <- SewLmulPattern.fpAll) yield {
                      vi ## sewlmul ## NfPattern.dontCare
                    }
                }
              case _ =>
                throw new IllegalArgumentException(s"Unsupported vector arith pattern $vai in VectorDecodeChannel")
            }
          case vmi: VecMemInstPattern =>
            vmi match {
              case _: VecMemMask =>
                for {
                  sewlmul <- SewLmulPattern.all
                } yield {
                  vi ## sewlmul ## NfPattern.dontCare
                }
              case _: VecMemWhole =>
                for {
                  sewlmul <- SewLmulPattern.all
                  nf <- NfPattern.pot
                } yield {
                  vi ## sewlmul ## nf
                }
              case _ =>
                for {
                  sewlmul <- SewLmulPattern.all
                  nf <- NfPattern.all
                } yield {
                  vi ## sewlmul ## nf
                }
            }
          case vci: VecConfigInstPattern =>
            Seq()
          case vii: VecIntInstPattern =>
            Seq(vi ## SewLmulPattern.dontCare ## NfPattern.dontCare)
        }
    }
  }

  // Generate decode tables. A decode field is a signal bundle that can be directly generated
  // from the instruction bits, with the the instruction bits itself as decode pattern.
  val instDecodeFields: Seq[DecodeField[VecInstPattern, Data]] = Seq(
    FrmRenField,
    FrmField,
    FFlagsWenField,
    Src12RevField,
    VdEew1bField,
    AlwaysTaField,
    CommitTypeField,
    IsVecMemContinousField,
    ImmIsSign5b,
    ImmIsUnsign5b,
    ExceptionIIField,
  )

  // Generate decode tables, using both instruction bits and sew as decode pattern.
  val instSewDecodeFields: Seq[DecodeField[DecodePatternComb2[VecInstPattern, SewPattern], UInt]] = Seq(
    Src1SelectField,
    Src2SelectField,
    DestSelectField,
  )

  val uopInfoFields = Seq.tabulate(maxSplitUopNum)(i => new UopInfoField(i))
  val opcodeFields = Seq.tabulate(maxSplitUopNum)(i => new OpcodeField(i))
  val fuTypeFields = Seq.tabulate(maxSplitUopNum)(i => new FuTypeField(i))

  // Generate decode tables, using instruction bits, sew, lmul and nf-bits (inst(31:29)) as decode pattern.
  val instSewLmulNfDecodeFields = uopInfoFields ++ opcodeFields ++ fuTypeFields ++ Seq(
    NumWbField,
    NumUopField,
    IllegalField,
    NumUopOhField,
    WritePartVdField,
  )

  println(s"instPats.length: ${instPats.length}")
  println(s"instSewPats.length: ${instSewPats.length}")
  println(s"instSewNfLmulPats.length: ${instSewLmulNfPats.length}")

  // Use different decode patterns and decode fields to prepare decode tables generation
  val instDecodeTable = new DecodeTable(instPats, instDecodeFields)
  val instSewDecodeTable = new DecodeTable(instSewPats, instSewDecodeFields)
  val instSewLmulNfDecodeTable = new DecodeTable(instSewLmulNfPats, instSewLmulNfDecodeFields)

  // Drive the output of decode tables from given input signals. `decode` method will generate decode table
  val instBundle = dontTouch(instDecodeTable.decode(rawInst))
  val instSewBundle = dontTouch(instSewDecodeTable.decode(rawInst ## sew))
  val instSewLmulNfBundle = dontTouch(instSewLmulNfDecodeTable.decode(rawInst ## sew ## lmul ## nf))

  println(s"the width of instBundle: ${instBundle.getWidth}")
  println(s"the width of instSewBundle: ${instSewBundle.getWidth}")
  println(s"the width of instSewNfLmulBundle: ${instSewLmulNfBundle.getWidth}")

  // Get diffrent decode fields from different decode tables as output
  val immIsSign5b = instBundle(ImmIsSign5b)
  val immIsUnsign5b = instBundle(ImmIsUnsign5b)
  val isVecMemContinous = instBundle(IsVecMemContinousField)

  val src1Sel = instSewBundle(Src1SelectField)
  val src2Sel = instSewBundle(Src2SelectField)

  val numWb = instSewLmulNfBundle(NumWbField)
  val numUop = instSewLmulNfBundle(NumUopField)
  val uopInfos = uopInfoFields.map(field => instSewLmulNfBundle(field))
  val opcodes = opcodeFields.map(field => instSewLmulNfBundle(field))
  val fuTypes = fuTypeFields.map(field => instSewLmulNfBundle(field))
  val uopIllegal = instSewLmulNfBundle(IllegalField)
  val numUopOH = instSewLmulNfBundle(NumUopOhField)
  val isWritePartVd = instSewLmulNfBundle(WritePartVdField)

  // Drive the source register select signals of `SrcSelectModule` from decode table output.
  srcSelectModule.in match {
    case in =>
      in.src1Sel := instSewBundle(Src1SelectField)
      in.src2Sel := instSewBundle(Src2SelectField)
      in.destSel := instSewBundle(DestSelectField)
      in.rs1 := instFields.RS1
      in.rs2 := instFields.RS2
      in.rd := instFields.RD
      in.uopNum := numUopOH
  }

  val alwaysTa = instBundle(AlwaysTaField)

  val ma = in.ma
  val ta = in.ta || alwaysTa
  val vm = instFields.VM.asBool
  val mu = !ma
  val tu = !ta

  val vdDepElim: UInt = Mux1H(Seq(
    (ta && (vm || ma)) -> VdDepElim.Always,
    (tu && (vm || ma)) -> VdDepElim.IfVlmax,
    (ta && !vm && mu)  -> VdDepElim.IfMaskOne,
    (tu && !vm && mu)  -> VdDepElim.IfVlmaxAndMaskOne
  ))

  // Drive output signals
  for (i <- 0 until maxSplitUopNum) {
    out.uop(i).valid := uopInfos(i).valid
    out.uop(i).bits.renameInfo.uop := uopInfos(i).bits
    out.uop(i).bits.renameInfo.illegal := uopIllegal
    out.uop(i).bits.fuType := fuTypes(i)
    out.uop(i).bits.opcode := opcodes(i)
    out.uop(i).bits.src := srcSelectModule.out.src(i)
    out.uop(i).bits.frmRen := instBundle(FrmRenField)
    out.uop(i).bits.v0Ren := !instFields.VM
    out.uop(i).bits.fflagsWen := instBundle(FFlagsWenField)
    out.uop(i).bits.frm := instBundle(FrmField)
    out.uop(i).bits.uopDepend := false.B // Todo
    out.uop(i).bits.src12Rev := instBundle(Src12RevField)
    out.uop(i).bits.vdEew1b := instBundle(VdEew1bField)
    out.uop(i).bits.numWb := numWb
    out.uop(i).bits.uopIdx := i.U
    out.uop(i).bits.isFirstUop := (i == 0).B
    out.uop(i).bits.isLastUop := i.U === numUop
    out.uop(i).bits.commitType := instBundle(CommitTypeField)
    out.uop(i).bits.vdDepElim := vdDepElim
    out.uop(i).bits.isWritePartVd := isWritePartVd
    out.uop(i).bits.isVset := false.B
    out.uop(i).bits.selImm.valid := Cat(
      isVecMemContinous,
      immIsUnsign5b,
      immIsSign5b,
    ).orR
    out.uop(i).bits.selImm.bits := Mux1H(Seq(
      isVecMemContinous -> DecodeSelImm.I,
      immIsUnsign5b -> DecodeSelImm.OPIVIU,
      immIsSign5b -> DecodeSelImm.OPIVIS,
    ))
    when (out.uop(i).valid) {
      assert(
        PopCount(Seq(isVecMemContinous, immIsUnsign5b, immIsSign5b)) <= 1.U,
        "The signal of imm select should be all 0s or one hot."
      )
    }

    out.uop(i).bits.imm := Mux1H(Seq(
      isVecMemContinous -> (VLENB * i).U,
      (immIsUnsign5b || immIsSign5b) -> instFields.IMM5_OPIVI
    ))
    out.uop(i).bits.vm := instFields.VM

    out.uop(i).bits.exceptionII := instBundle(ExceptionIIField)
  }
  out.uopNumOH := numUopOH

  uopInfos.map(dontTouch(_))
}

object VectorDecodeChannel {
  def main(args: Array[String]): Unit = {
    val instSeq = VecInstPattern.all.collect {
      case x: VecArithInstPattern => x
      case x: VecMemInstPattern => x
    }

    val targetDir = "build/decoderOld"

    val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(
      args :+ "--disable-always-basic-diff" :+ "--fpga-platform" :+ "--target" :+ "verilog")


    val defaultConfig = config.alterPartial({
      // Get XSCoreParams and pass it to the "small module"
      case XSCoreParamsKey => XSCoreParameters
    })


    Verilog.emitVerilog(
      new VectorDecodeChannel(instSeq)(defaultConfig),
      Array(
        "--throw-on-first-error",
        "--full-stacktrace",
        "--target-dir", targetDir,
      ),
    )

    println(s"Generate VectorDecodeChannel in dir $targetDir")
  }

  class VecDecodeChannelOutputUop extends Bundle with HasVectorSettings {
    val fuType: UInt = FuType()
    val opcode: UInt = Opcode()
    val renameInfo = new UopInfoRenameWithIllegal
    val src = new UopSrcBundle
    val v0Ren = Bool()
    val frmRen = Bool()
    val fflagsWen = Bool()
    val frm = Frm()
    val uopDepend = Bool()
    val src12Rev = Bool()
    val vdEew1b = Bool()
    // 0~7: 1~8 uops
    val numWb = NumWB()
    val uopIdx = UopIdx()
    val isFirstUop = Bool()
    val isLastUop = Bool()
    val commitType = CommitType()
    val vdDepElim = VdDepElim()
    val isWritePartVd = Bool()
    val isVset = Bool()
    val selImm = ValidIO(DecodeSelImm())
    val imm = UInt(32.W)
    val vm = Bool() // if vm is 0, need mask

    val exceptionII = Bool()
  }

  class VecDecodeChannelOutput(val uopWidth: Int) extends Bundle {
    val uop = Vec(uopWidth, ValidIO(new VecDecodeChannelOutputUop))
    val uopNumOH = NumUopOH()
  }
}


