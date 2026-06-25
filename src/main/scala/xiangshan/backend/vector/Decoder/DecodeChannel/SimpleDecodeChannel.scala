package xiangshan.backend.vector.Decoder.DecodeChannel

import chisel3._
import chisel3.experimental.hierarchy.{instantiable, public}
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import top.ArgParser
import utility.LookupTree
import xiangshan.CommitType
import xiangshan.backend.Bundles.UopIdx
import xiangshan.backend.decode.ImmUnion
import xiangshan.backend.decode.isa.bitfield.XSInstBitFields
import xiangshan.backend.decode.opcode.Opcode
import xiangshan.backend.decode.opcode.Opcode.Opcode
import xiangshan.backend.fu.FuType
import xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel._
import xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel.Frm
import xiangshan.backend.vector.Decoder.DecodePatterns.RdZeroPattern
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.RVVDecodeUtil._
import xiangshan.backend.vector.Decoder.{DecodeChannelInput, NumUopOH, SrcRenType}
import xiangshan.backend.vector.Decoder.Types.{DecodeSelImm, NumWB}
import xiangshan.backend.vector.Decoder.Uop.UopInfoRenameSimple
import xiangshan.backend.vector.Decoder.util._
import xiangshan.backend.vector.util.Verilog
import xiangshan.backend.vector.HasSimpleSettings
import xiangshan._

@instantiable
class SimpleDecodeChannel(instSeq: Seq[InstPattern])(implicit val p: Parameters) extends Module with HasSimpleSettings with HasXSParameter {
  import xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel._
  import SimpleDecodeChannel._

  @public val in = IO(Input(new DecodeChannelInput))
  @public val out = IO(Output(new SimpleDecodeChannelOutput(maxSimpleSplitUopNum)))

  val rawInst = in.rawInst
  val instFields = rawInst.asTypeOf(new XSInstBitFields)

  val patterns = instSeq

  val uopInfoFields = Seq.tabulate(maxSimpleSplitUopNum)(i => new UopInfoField(i))
  val opcodeFields = Seq.tabulate(maxSimpleSplitUopNum)(i => new OpcodeField(i))
  val fuTypeFields = Seq.tabulate(maxSimpleSplitUopNum)(i => new FuTypeField(i))

  val isJRFields = Seq.tabulate(maxSimpleSplitUopNum)(i => new IsJRField(i))

  val fields = uopInfoFields ++ opcodeFields ++ fuTypeFields ++ isJRFields ++ Seq(
    FrmRenField,
    FFlagsWenField,
    SelImmField,
    CommitTypeField,
    CanRobCompressField,
    NumUopField,
    NumUopOhField,
    NeedFsField,
    PrivExceptionCauseField,
    NumWbField,
  )

  println(s"The length of DecodeTable in SimpleDecodeChannel: ${patterns.length}")
  val table = new DecodeTable(patterns, fields)

  // Get the decode result by generating a decode table by programming logic array (pla)
  val result = table.decode(in.rawInst)

  val uopInfos = uopInfoFields.map(field => result(field))
  val opcodes = opcodeFields.map(field => result(field))
  val fuTypes = fuTypeFields.map(field => result(field))

  val isJRs         = isJRFields.map(field => result(field))
  val frmRen         = result(FrmRenField)
  val fflagsWen      = result(FFlagsWenField)
  val selImm         = result(SelImmField)
  val commitType     = result(CommitTypeField)
  val canRobCompress = result(CanRobCompressField)
  val numUop         = result(NumUopField)
  val numUopOH       = result(NumUopOhField)
  val needFs         = result(NeedFsField)
  val privCause      = result(PrivExceptionCauseField)
  val numWb          = result(NumWbField)

  dontTouch(privCause)

  val imm = LookupTree(selImm.bits, ImmUnion.immSelMap.map {
    case (sel, enum) =>
      sel -> enum.minBitsFromInstr(in.rawInst).ensuring(_.getWidth == enum.len)
  })

  val fsOffExceptionII = in.fromCSR.illegalInst.fsIsOff && needFs

  val privExceptionSources = Seq(
    (PrivExceptionCause.sfenceVMA,  in.fromCSR.illegalInst.sfenceVMA,              in.fromCSR.virtualInst.sfenceVMA),
    (PrivExceptionCause.sfencePart, in.fromCSR.illegalInst.sfencePart,             in.fromCSR.virtualInst.sfencePart),
    (PrivExceptionCause.hfenceGVMA, in.fromCSR.illegalInst.hfenceGVMA,             in.fromCSR.virtualInst.hfence),
    (PrivExceptionCause.hfenceVVMA, in.fromCSR.illegalInst.hfenceVVMA,             in.fromCSR.virtualInst.hfence),
    (PrivExceptionCause.hlsv,       in.fromCSR.illegalInst.hlsv,                   in.fromCSR.virtualInst.hlsv),
    (PrivExceptionCause.wfi,        in.fromCSR.illegalInst.wfi,                    in.fromCSR.virtualInst.wfi),
    (PrivExceptionCause.wrsNto,     in.fromCSR.illegalInst.wrs_nto,                in.fromCSR.virtualInst.wrs_nto),
    (PrivExceptionCause.cboZ,       !HasCMO.B || in.fromCSR.illegalInst.cboZ,      in.fromCSR.virtualInst.cboZ),
    (PrivExceptionCause.cboCF,      !HasCMO.B || in.fromCSR.illegalInst.cboCF,     in.fromCSR.virtualInst.cboCF),
    (PrivExceptionCause.cboI,       !HasCMO.B || in.fromCSR.illegalInst.cboI,      in.fromCSR.virtualInst.cboI),
    (PrivExceptionCause.aes64ks1i,  true.B,                                        false.B),
    (PrivExceptionCause.amocasQ,    true.B,                                        false.B),
  )

  val privExceptionII = Mux1H(privExceptionSources.map {
    case (cause, illegal, _) => (privCause === cause) -> illegal
  })

  val privExceptionVI = Mux1H(privExceptionSources.map {
    case (cause, _, virtual) => (privCause === cause) -> virtual
  })

  for (i <- 0 until maxSimpleSplitUopNum) {
    val frmExceptionII = out.uop(i).bits.frmRen && (out.uop(i).bits.frmIll || (out.uop(i).bits.frm === Frm.DYN && in.fromCSR.illegalInst.frm))

    out.uop(i).valid := uopInfos(i).valid
    out.uop(i).bits.renameInfo := uopInfos(i).bits
    out.uop(i).bits.renameInfo.gpWen := uopInfos(i).bits.gpWen && instFields.RD =/= 0.U
    out.uop(i).bits.fuType := fuTypes(i)
    out.uop(i).bits.opcode := opcodes(i)
    out.uop(i).bits.lsrc1 := instFields.RS1
    out.uop(i).bits.lsrc2 := instFields.RS2
    out.uop(i).bits.lsrc3 := instFields.FS3
    out.uop(i).bits.frmRen := frmRen
    out.uop(i).bits.fflagsWen := fflagsWen
    out.uop(i).bits.ldest := instFields.RD
    out.uop(i).bits.frm := instFields.RM
    out.uop(i).bits.frmIll := instFields.RM === 5.U || instFields.RM === 6.U
    out.uop(i).bits.selImm := selImm
    out.uop(i).bits.imm := imm
    out.uop(i).bits.commitType := commitType
    out.uop(i).bits.canRobCompress := canRobCompress
    out.uop(i).bits.numWb := numWb
    out.uop(i).bits.uopIdx := i.U
    out.uop(i).bits.isFirstUop := (i == 0).B
    out.uop(i).bits.isLastUop := i.U === numUop
    out.uop(i).bits.isJr := isJRs(i)
    out.uop(i).bits.exceptionII := frmExceptionII || fsOffExceptionII || privExceptionII
    out.uop(i).bits.exceptionVI := privExceptionVI
  }

  out.uopNumOH := numUopOH
}

object SimpleDecodeChannel {
  class SimpleDecodeChannelOutputUop() extends Bundle {
    val fuType: UInt = FuType()
    val opcode: UInt = Opcode()
    val renameInfo = new UopInfoRenameSimple
    val lsrc1 = UInt(5.W)
    val lsrc2 = UInt(5.W)
    val lsrc3 = UInt(5.W)
    val frmRen = Bool()
    val fflagsWen = Bool()
    val ldest = UInt(5.W)
    val frm = Frm()
    val frmIll = Bool()
    val selImm = ValidIO(DecodeSelImm())
    val imm = UInt(32.W)
    val commitType = CommitType()
    val canRobCompress = Bool()
    val numWb = NumWB()
    val uopIdx = UopIdx()
    val isFirstUop = Bool()
    val isLastUop = Bool()
    val isJr = Bool()
    val exceptionII = Bool()
    val exceptionVI = Bool()
  }

  class SimpleDecodeChannelOutput(val uopWidth: Int) extends Bundle {
    val uop = Vec(uopWidth, ValidIO(new SimpleDecodeChannelOutputUop))
    val uopNumOH = NumUopOH()
  }
}

object SimpleDecodeChannelMain extends App {
  import xiangshan.backend.decode.isa.Extensions._

  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(
    args :+ "--disable-always-basic-diff" :+ "--fpga-platform" :+ "--target" :+ "verilog"
  )

  val defaultConfig = config.alterPartial({
    case XSCoreParamsKey => XSCoreParameters()
  })

  val insts: Seq[InstPattern] = InstPattern.extensionInsts(extensions: _*).collect { case x if !x.isInstanceOf[VecInstPattern] => x }

  println(s"number of insts: ${insts.size}")

  val targetDir = "build/decoder"

  Verilog.emitVerilog(
    new SimpleDecodeChannel(insts)(defaultConfig),
    Array("--full-stacktrace", "--target-dir", targetDir),
  )

  println(s"Generate SimpleDecodeChannel in dir $targetDir")
}
