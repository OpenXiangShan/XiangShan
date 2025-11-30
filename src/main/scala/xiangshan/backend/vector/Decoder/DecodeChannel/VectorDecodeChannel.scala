package xiangshan.backend.vector.Decoder.DecodeChannel

import chisel3._
import chisel3.experimental.hierarchy.{instantiable, public}
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import top.ArgParser
import xiangshan.{CommitType, XSCoreParameters, XSCoreParamsKey, XSTileKey}
import xiangshan.backend.decode.isa.bitfield.{BitFieldsVec, Riscv32BitInst}
import xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel.{CommitTypeField, FFlagsWenField, FrmRenField}
import xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel._
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.RVVDecodeUtil._
import xiangshan.backend.vector.Decoder.Types.VdDepElim
import xiangshan.backend.vector.Decoder._
import xiangshan.backend.vector.Decoder.util._
import xiangshan.backend.vector._
import xiangshan.backend.vector.util.Verilog

@instantiable
class VectorDecodeChannel(
  instSeq: Seq[VecInstPattern],
) (
  implicit val p: Parameters
) extends Module with HasVectorSettings {
  val UopWidth = 8

  @public val in = IO(Input(new DecodeChannelInput))
  @public val out = IO(Output(new Bundle {
    val uop = Vec(UopWidth, ValidIO(new VecDecodeChannelOutputUop))
    val uopNumOH = NumUopOH()
  }))

  val rawInst = in.rawInst
  val sew = in.sew
  val lmul = in.lmul

  val instFields: Riscv32BitInst with BitFieldsVec = in.rawInst.asTypeOf(new Riscv32BitInst with BitFieldsVec)

  val nf = instFields.NF

  val srcSelectModule = Module(new SrcSelectModule)
  val vsetDecoder = Module(new VsetDecoder)

  val instPats: Seq[VecInstPattern] = instSeq

  val instSewPats: Seq[DecodePatternComb2[VecInstPattern, SewPattern]] = VecInstPattern.withSew(instSeq)

  val instSewLmulNfPats: Seq[DecodePatternComb4[VecInstPattern, SewPattern, LmulPattern, NfPattern]] = {
    val vaiInstPats = for {
      inst <- instSeq.collect {
        case instP: VecArithInstPattern => instP
      }
      lmul <- LmulPattern.all
    } yield {
      inst ## SewPattern.dontCare ## lmul ## NfPattern.dontCare
    }

    val memNonSegInstPats = for {
      inst <- instSeq.collect {
        case instP: VecMemWhole => instP
        case instP: VecMemMask => instP
      }
    } yield {
      inst.asInstanceOf[VecMemInstPattern] ## SewPattern.dontCare ## LmulPattern.dontCare ## NfPattern.dontCare
    }

    val memSegInstPats = for {
      inst <- instSeq.collect {
        case x: VecMemInstPattern if !x.isInstanceOf[VecMemWhole] && !x.isInstanceOf[VecMemMask] => x
      }
      sew <- SewPattern.all
      nf <- NfPattern.all
      lmul <- LmulPattern.all
    } yield {
      inst ## sew ## lmul ## nf
    }

    (vaiInstPats ++ memNonSegInstPats ++ memSegInstPats)
      .map(_.asInstanceOf[DecodePatternComb4[VecInstPattern, SewPattern, LmulPattern, NfPattern]])
  }

  val instDecodeFields: Seq[DecodeField[VecInstPattern, UInt]] = Seq(
    FrmRenField,
    FFlagsWenField,
    Src12RevField,
    VdEew1bField,
    AlwaysTaField,
    CommitTypeField,
  )

  val instSewDecodeFields: Seq[DecodeField[DecodePatternComb2[VecInstPattern, SewPattern], UInt]] = Seq(
    Src1SelectField,
    Src2SelectField,
    DestSelectField,
  )

  val instSewLmulNfDecodeFields = Seq(
    NumUopField,
    UopInfoField,
    IllegalField,
    NumUopOhField,
    WritePartVdField,
  )

  println(s"instPats.length: ${instPats.length}")
  println(s"instSewPats.length: ${instSewPats.length}")
  println(s"instSewNfLmulPats.length: ${instSewLmulNfPats.length}")

  val instDecodeTable = new DecodeTable(instPats, instDecodeFields)
  val instSewDecodeTable = new DecodeTable(instSewPats, instSewDecodeFields)
  val instSewLmulNfDecodeTable = new DecodeTable(instSewLmulNfPats, instSewLmulNfDecodeFields)

  val instBundle = instDecodeTable.decode(rawInst)
  val instSewBundle = instSewDecodeTable.decode(rawInst ## sew)
  val instSewNfLmulBundle = instSewLmulNfDecodeTable.decode(rawInst ## sew ## lmul ## nf)

  val src1Sel = instSewBundle(Src1SelectField)
  val src2Sel = instSewBundle(Src2SelectField)

  val numUop = instSewNfLmulBundle(NumUopField)
  val uopInfo = instSewNfLmulBundle(UopInfoField)
  val uopIllegal = instSewNfLmulBundle(IllegalField)
  val numUopOH = instSewNfLmulBundle(NumUopOhField)
  val isWritePartVd = instSewNfLmulBundle(WritePartVdField)

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

  vsetDecoder.in.rawInst := rawInst

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

  val vsetDecoderValid = vsetDecoder.out.renameInfo.valid

  for (i <- 0 until UopWidth) {
    out.uop(i).valid := uopInfo(i).valid || vsetDecoderValid
    out.uop(i).bits.renameInfo.uop := Mux1H(Seq(
      uopInfo(i).valid -> uopInfo(i).bits,
      vsetDecoderValid -> vsetDecoder.out.renameInfo.bits
    ))
    out.uop(i).bits.renameInfo.illegal := Mux1H(Seq(
      uopInfo(i).valid -> uopIllegal,
      vsetDecoderValid -> false.B,
    ))
    out.uop(i).bits.fuType := 0.U
    out.uop(i).bits.opcode := 0.U
    out.uop(i).bits.src := Mux1H(Seq(
      uopInfo(i).valid -> srcSelectModule.out.src(i),
      vsetDecoderValid -> vsetDecoder.out.src,
    ))
    out.uop(i).bits.frmRen := Mux(vsetDecoderValid, false.B, instBundle(FrmRenField))
    out.uop(i).bits.fflagsWen := Mux(vsetDecoderValid, false.B, instBundle(FFlagsWenField))
    out.uop(i).bits.uopDepend := false.B // Todo
    out.uop(i).bits.src12Rev := Mux(vsetDecoderValid, false.B, instBundle(Src12RevField))
    out.uop(i).bits.vdEew1b := Mux(vsetDecoderValid, false.B, instBundle(VdEew1bField))
    out.uop(i).bits.numUop := Mux(vsetDecoderValid, 0.U, numUop)
    out.uop(i).bits.uopIdx := i.U
    out.uop(i).bits.isFirstUop := (i == 0).B
    out.uop(i).bits.isLastUop := Mux(vsetDecoderValid, (i == 0).B, (i + 1).U === numUopOH)
    out.uop(i).bits.commitType := Mux(vsetDecoderValid, CommitType.NORMAL, instBundle(CommitTypeField))
    out.uop(i).bits.vdDepElim := vdDepElim
    out.uop(i).bits.isWritePartVd := isWritePartVd
    out.uop(i).bits.isVset := vsetDecoderValid
  }
  out.uopNumOH := numUopOH
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
}
