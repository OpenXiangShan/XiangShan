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
import xiangshan.backend.decode.isa.Extensions.ExtBase
import xiangshan.backend.decode.isa.bitfield.XSInstBitFields
import xiangshan.backend.decode.isa.Instructions._
import xiangshan.backend.decode.opcode.Opcode
import xiangshan.backend.fu.FuType
import xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel.Frm
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.RVVDecodeUtil._
import xiangshan.backend.vector.Decoder.{DecodeChannelInput, NumUopOH}
import xiangshan.backend.vector.Decoder.Types.{DecodeSelImm, NumWB}
import xiangshan.backend.vector.Decoder.Uop.UopInfoRenameSimple
import xiangshan.backend.vector.Decoder.util._
import xiangshan.backend.vector.util.Verilog
import xiangshan.backend.vector.HasSimpleSettings
import xiangshan._

@instantiable
class SimpleDecodeChannel(instSeq: Seq[InstPattern], extensions: Seq[ExtBase])(implicit val p: Parameters) extends Module with HasSimpleSettings with HasXSParameter {
  import xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel._
  import SimpleDecodeChannel._
  override def maxSimpleSplitUopNum: Int = if (HasShadowStack) 5 else 2

  @public val in = IO(Input(new DecodeChannelInput))
  @public val out = IO(Output(new SimpleDecodeChannelOutput(maxSimpleSplitUopNum)))

  val isMove = BitPat("b000000000000_?????_000_?????_0010011")

  val rawInst = in.rawInst
  val instFields = rawInst.asTypeOf(new XSInstBitFields)

  val patterns = instSeq


  val patternsCboI2f: Seq[DecodePatternComb2[InstPattern, BoolPattern]] = for (
    instP <- instSeq.filter(x => ZICBOType.all.contains(x.bitPat));
    bool <- BoolPattern.all
  ) yield {
    instP ## bool
  }

  println("[tmp-SimpleDecodeChannel]")
  instSeq.foreach(println)
  println("[tmp-SimpleDecodeChannel]")
  patternsCboI2f.foreach(println)

  val uopInfoFields = Seq.tabulate(maxSimpleSplitUopNum)(i => new UopInfoField(i, extensions))
  val opcodeFields = Seq.tabulate(maxSimpleSplitUopNum)(i => new OpcodeField(i, extensions))
  val fuTypeFields = Seq.tabulate(maxSimpleSplitUopNum)(i => new FuTypeField(i, extensions))
  val lsrc1Fields = Seq.tabulate(maxSimpleSplitUopNum)(i => new LogicalRegField(i, 0, extensions))
  val lsrc2Fields = Seq.tabulate(maxSimpleSplitUopNum)(i => new LogicalRegField(i, 1, extensions))
  val lsrc3Fields = Seq.tabulate(maxSimpleSplitUopNum)(i => new LogicalRegField(i, 2, extensions))
  val ldestFields = Seq.tabulate(maxSimpleSplitUopNum)(i => new LogicalRegField(i, 3, extensions))
  val uopImmInfoFields = Seq.tabulate(maxSimpleSplitUopNum)(i => new UopImmInfoField(i, extensions, XLEN))
  val uopCommitTypeFields = Seq.tabulate(maxSimpleSplitUopNum)(i => new UopCommitTypeField(i, extensions))
  val numUopOhField = new NumUopOhField(extensions)
  val numUopField = new NumUopField(extensions)

  val isJFields = Seq.tabulate(maxSimpleSplitUopNum)(i => new IsJField(i))
  val isJrFields = Seq.tabulate(maxSimpleSplitUopNum)(i => new IsJrField(i))

  val fields = uopInfoFields ++ opcodeFields ++ fuTypeFields ++ lsrc1Fields ++ lsrc2Fields ++
    lsrc3Fields ++ ldestFields ++ uopImmInfoFields ++ uopCommitTypeFields ++ isJFields ++ isJrFields ++ Seq(
    FrmRenField,
    FFlagsWenField,
    CanRobCompressField,
    numUopField,
    numUopOhField,
    NeedFsField,
    PrivExceptionCauseField,
    NumWbField,
  )

  println(s"The length of DecodeTable in SimpleDecodeChannel: ${patterns.length}")
  val table = new DecodeTable(patterns, fields)
  val instCboI2fTable = new DecodeTable(patternsCboI2f, Seq(CboOpcodeField))

  // Get the decode result by generating a decode table by programming logic array (pla)
  val result = table.decode(in.rawInst)
  val resultInstCboI2f = instCboI2fTable.decode(in.rawInst ## in.fromCSR.special.cboI2F)

  val uopInfos = uopInfoFields.map(field => result(field))
  val opcodes = opcodeFields.map(field => result(field))
  val fuTypes = fuTypeFields.map(field => result(field))
  val lsrc1Specs = lsrc1Fields.map(field => result(field))
  val lsrc2Specs = lsrc2Fields.map(field => result(field))
  val lsrc3Specs = lsrc3Fields.map(field => result(field))
  val ldestSpecs = ldestFields.map(field => result(field))
  val uopImmInfos = uopImmInfoFields.map(field => result(field))
  val uopCommitTypes = uopCommitTypeFields.map(field => result(field))

  val cboOpcode = resultInstCboI2f(CboOpcodeField)

  val isJs          = isJFields.map(field => result(field))
  val isJrs         = isJrFields.map(field => result(field))
  val frmRen         = result(FrmRenField)
  val fflagsWen      = result(FFlagsWenField)
  val canRobCompress = result(CanRobCompressField)
  val numUop         = result(numUopField)
  val numUopOH       = result(numUopOhField)
  val numWb          = result(NumWbField)

  val uopImms = uopImmInfos.map { info =>
    val standard = LookupTree(info.selImm.bits, ImmUnion.immSelMap.map {
      case (sel, enum) => sel -> enum.minBitsFromInstr(rawInst).ensuring(_.getWidth == enum.len)
    })
    Mux(info.constant.valid, info.constant.bits, standard)
  }

  val needFs = result(NeedFsField)
  val privCause = result(PrivExceptionCauseField)

  dontTouch(privCause)

  val fsOffExceptionII = in.fromCSR.illegalInst.fsIsOff && needFs

  val privExceptionSources = Seq(
    (PrivExceptionCause.sfenceVMA,  in.fromCSR.illegalInst.sfenceVMA,              in.fromCSR.virtualInst.sfenceVMA),
    (PrivExceptionCause.sfencePart, in.fromCSR.illegalInst.sfencePart,             in.fromCSR.virtualInst.sfencePart),
    (PrivExceptionCause.hfenceGVMA, in.fromCSR.illegalInst.hfenceGVMA,             in.fromCSR.virtualInst.hfence),
    (PrivExceptionCause.hfenceVVMA, in.fromCSR.illegalInst.hfenceVVMA,             in.fromCSR.virtualInst.hfence),
    (PrivExceptionCause.ssamoswap, in.fromCSR.illegalInst.ssamoswap.getOrElse(false.B), in.fromCSR.virtualInst.ssamoswap.getOrElse(false.B)),
    (PrivExceptionCause.mfence,     in.fromCSR.illegalInst.mfence.getOrElse(false.B), false.B),
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
    val logicalSrc1 = LogicalRegField.decodeSelector(lsrc1Specs(i), instFields)
    val logicalSrc2 = LogicalRegField.decodeSelector(lsrc2Specs(i), instFields)
    val logicalSrc3 = LogicalRegField.decodeSelector(lsrc3Specs(i), instFields)
    val logicalDest = LogicalRegField.decodeSelector(ldestSpecs(i), instFields)

    out.uop(i).valid := uopInfos(i).valid
    out.uop(i).bits.renameInfo := uopInfos(i).bits
    out.uop(i).bits.renameInfo.gpWen := uopInfos(i).bits.gpWen && logicalDest =/= 0.U
    out.uop(i).bits.fuType := fuTypes(i)
    out.uop(i).bits.opcode := opcodes(i) | cboOpcode
    out.uop(i).bits.lsrc1 := logicalSrc1
    out.uop(i).bits.lsrc2 := logicalSrc2
    out.uop(i).bits.lsrc3 := logicalSrc3
    out.uop(i).bits.frmRen := frmRen
    out.uop(i).bits.fflagsWen := fflagsWen
    out.uop(i).bits.ldest := logicalDest
    out.uop(i).bits.frm := instFields.RM
    out.uop(i).bits.frmIll := instFields.RM === 5.U || instFields.RM === 6.U
    out.uop(i).bits.selImm := uopImmInfos(i).selImm
    out.uop(i).bits.imm := uopImms(i)
    out.uop(i).bits.commitType := uopCommitTypes(i)
    out.uop(i).bits.canRobCompress := canRobCompress
    out.uop(i).bits.numWb := numWb
    out.uop(i).bits.uopIdx := i.U
    out.uop(i).bits.isFirstUop := (i == 0).B
    out.uop(i).bits.isLastUop := i.U === numUop
    out.uop(i).bits.isJ := isJs(i)
    out.uop(i).bits.isJr := isJrs(i)
    out.uop(i).bits.isMove := rawInst === isMove && logicalDest =/= 0.U
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
    val lsrc1 = UInt(6.W)
    val lsrc2 = UInt(6.W)
    val lsrc3 = UInt(6.W)
    val frmRen = Bool()
    val fflagsWen = Bool()
    val ldest = UInt(6.W)
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
    val isJ = Bool()
    val isJr = Bool()
    val isMove = Bool()
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

  val coreParams = defaultConfig(XSCoreParamsKey)
  val simpleExts: Seq[ExtBase] = extensions(defaultConfig)
  val insts: Seq[InstPattern] = InstPattern.extensionInsts(simpleExts: _*).collect { case x if !x.isInstanceOf[VecInstPattern] => x }

  println(s"number of insts: ${insts.size}")

  val targetDir = "build/decoder"

  Verilog.emitVerilog(
    new SimpleDecodeChannel(insts, simpleExts)(defaultConfig),
    Array("--full-stacktrace", "--target-dir", targetDir),
  )

  println(s"Generate SimpleDecodeChannel in dir $targetDir")
}
