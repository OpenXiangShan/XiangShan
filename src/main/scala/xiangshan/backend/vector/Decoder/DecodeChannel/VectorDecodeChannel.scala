package xiangshan.backend.vector.Decoder.DecodeChannel

import chisel3._
import chisel3.experimental.hierarchy.{instantiable, public}
import chisel3.util._
import chisel3.util.experimental.decode.{DecodeBundle, DecodeField, DecodePattern, DecodeTable}
import freechips.rocketchip.rocket.Instructions
import xiangshan.backend.decode.isa.bitfield.{BitFieldsVec, Riscv32BitInst}
import xiangshan.backend.fu.vector.Bundles.{VLmul, VSew}
import xiangshan.backend.vector.Decoder.DecodeChannel.SplitCtlDecoderUtil.InstNfLmulSewPattern
import xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel.{DestSelectEnum, DestSelectField, EewField, FpWenField, GpWenField, IllegalField, Src12RevField, Src1SelectEnum, Src1SelectField, Src2SelectEnum, Src2SelectField, UopInfoField, UopNumField, VdAllocFieldDeprecated, VdEew1bField, VpWenField, VxsatWenField}
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.RVVDecodeUtil._
import xiangshan.backend.vector.Decoder.Split.{SplitType, SplitTypeOH}
import xiangshan.backend.vector.Decoder.Types.EnumLMUL
import xiangshan.backend.vector.Decoder.Uop.{UopInfoRename, UopInfoRenameWithIllegal}
import xiangshan.backend.vector.Decoder._
import xiangshan.backend.vector._
import xiangshan.backend.vector.util.ChiselTypeExt._
import xiangshan.backend.vector.util.Select.Mux1HLookUp
import xiangshan.backend.vector.util.Verilog
import xiangshan.macros.InstanceNameMacro.getVariableName

@instantiable
class VectorDecodeChannel(
  instSeq: Seq[VecInstPattern],
) extends Module with HasVectorSettings {
  val UopWidth = 8

  @public val in = IO(Input(new DecodeChannelInput))
  @public val out = IO(Output(new Bundle {
    val uop = Vec(UopWidth, ValidIO(new VecDecodeChannelOutputUop))
    val uopNumOH = UopNumOH()
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

  val instSewNfLmulPats: Seq[DecodePatternComb4[VecInstPattern, SewPattern, NfPattern, LmulPattern]] = {
    val vaiInstPats = for {
      inst <- instSeq.collect {
        case instP: VecArithInstPattern => instP
      }
      lmul <- LmulPattern.all
    } yield {
      inst ## SewPattern.dontCare ## NfPattern.dontCare ## lmul
    }

    val memNonSegInstPats = for {
      inst <- instSeq.collect {
        case instP: VecMemWhole => instP
        case instP: VecMemMask => instP
      }
    } yield {
      inst.asInstanceOf[VecMemInstPattern] ## SewPattern.dontCare ## NfPattern.dontCare ## LmulPattern.dontCare
    }

    val memSegInstPats = for {
      inst <- instSeq.collect {
        case x: VecMemInstPattern if !x.isInstanceOf[VecMemWhole] && !x.isInstanceOf[VecMemMask] => x
      }
      sew <- SewPattern.all
      nf <- NfPattern.all
      lmul <- LmulPattern.all
    } yield {
      inst ## sew ## nf ## lmul
    }

    (vaiInstPats ++ memNonSegInstPats ++ memSegInstPats)
      .map(_.asInstanceOf[DecodePatternComb4[VecInstPattern, SewPattern, NfPattern, LmulPattern]])
  }

  val instDecodeFields = Seq(
    Src12RevField,
    VdEew1bField,
  )

  val instSewDecodeFields: Seq[DecodeField[DecodePatternComb2[VecInstPattern, SewPattern], UInt]] = Seq(
    Src1SelectField,
    Src2SelectField,
    DestSelectField,
  )

  val instSewNfLmulDecodeFields = Seq(
    UopInfoField,
    IllegalField,
    UopNumField,
  )

  println(s"instPats.length: ${instPats.length}")
  println(s"instSewPats.length: ${instSewPats.length}")
  println(s"instSewNfLmulPats.length: ${instSewNfLmulPats.length}")

  val instDecodeTable = new DecodeTable(instPats, instDecodeFields)
  val instSewDecodeTable = new DecodeTable(instSewPats, instSewDecodeFields)
  val instSewNfLmulDecodeTable = new DecodeTable(instSewNfLmulPats, instSewNfLmulDecodeFields)

  val instBundle = instDecodeTable.decode(rawInst)
  val instSewBundle = instSewDecodeTable.decode(rawInst ## sew)
  val instSewNfLmulBundle = instSewNfLmulDecodeTable.decode(rawInst ## sew ## nf ## lmul)

  val src1Sel = instSewBundle(Src1SelectField)
  val src2Sel = instSewBundle(Src2SelectField)

  val uopInfo = instSewNfLmulBundle(UopInfoField)
  val uopIllegal = instSewNfLmulBundle(IllegalField)
  val uopNum = instSewNfLmulBundle(UopNumField)

  srcSelectModule.in match {
    case in =>
      in.src1Sel := instSewBundle(Src1SelectField)
      in.src2Sel := instSewBundle(Src2SelectField)
      in.destSel := instSewBundle(DestSelectField)
      in.rs1 := instFields.RS1
      in.rs2 := instFields.RS2
      in.rd := instFields.RD
      in.uopNum := uopNum
  }

  vsetDecoder.in.rawInst := rawInst

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
    out.uop(i).bits.src := Mux1H(Seq(
      uopInfo(i).valid -> srcSelectModule.out.src(i),
      vsetDecoderValid -> vsetDecoder.out.src,
    ))
    out.uop(i).bits.uopDepend := false.B // Todo
    out.uop(i).bits.src12Rev := Mux(vsetDecoderValid, false.B, instBundle(Src12RevField))
    out.uop(i).bits.vdEew1b := Mux(vsetDecoderValid, false.B, instBundle(VdEew1bField))
    out.uop(i).bits.uopIdx := i.U
    out.uop(i).bits.isLastUop := Mux(vsetDecoderValid, (i == 0).B, (i + 1).U === uopNum)
  }
  out.uopNumOH := uopNum
}

object VectorDecodeChannel {
  def main(args: Array[String]): Unit = {
    val instSeq = VecInstPattern.all.collect {
      case x: VecArithInstPattern => x
      case x: VecMemInstPattern => x
    }

    Verilog.emitVerilog(
      new VectorDecodeChannel(instSeq),
      Array(
        "--throw-on-first-error",
        "--full-stacktrace",
        "--target-dir", "build/decoder"
      ),
    )
  }
}
