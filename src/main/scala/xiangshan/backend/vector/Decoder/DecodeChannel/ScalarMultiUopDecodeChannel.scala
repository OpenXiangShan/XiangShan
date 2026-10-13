package xiangshan.backend.vector.Decoder.DecodeChannel

import chisel3._
import chisel3.experimental.hierarchy.{instantiable, public}
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.LookupTree
import xiangshan.CommitType
import xiangshan.backend.Bundles.UopIdx
import xiangshan.backend.decode.opcode.Opcode
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.wrapper.CSRToDecode
import xiangshan.backend.decode.ImmUnion
import xiangshan.backend.decode.isa.Extensions.{ExtBase, Zicfiss}
import xiangshan.backend.decode.isa.bitfield.XSInstBitFields
import xiangshan.backend.vector.Decoder.DecodeFields.ScalarMultiUopDecodeChannel._
import xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel._
import xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel.Frm
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.NumUopOH
import xiangshan.backend.vector.Decoder.Types.{DecodeSelImm, NumWB}
import xiangshan.backend.vector.Decoder.Uop.UopInfoRenameSimple
import xiangshan.backend.vector.Decoder.util.DecodeTable
import xiangshan._
import xiangshan.backend.vector.Decoder.DecodeChannelInput
/**
 * Decodes scalar instruction families requiring per-uop operands, immediates and commit types.
 *
 * SSPUSH splits into five uops, SSPOPCHK into three and SSRDP into one. Each uop has its own
 * register selector, constant immediate and commit type, taken from ScalaUopTable.tableZicfiss.
 * SSRDP and SSAMOSWAP are kept here with the rest of Zicfiss even though they need only one uop.
 *
 * uopNum is the exact count as a 4-bit binary value, the encoding UopBufferCtrlDecoder matches
 * against. The one-hot NumUopOH only has slots for 1, 2, 4 and 8.
 */
@instantiable
class ScalarMultiUopDecodeChannel(instSeq: Seq[InstPattern], extensions: Seq[ExtBase])
  (implicit val p: Parameters) extends Module with HasXSParameter {
  import ScalarMultiUopDecodeChannel._

  private val maxSplit = splitCounts(instSeq, extensions).max
  require(instSeq.nonEmpty && maxSplit > 0 && maxSplit <= 8)

  @public val in = IO(Input(new DecodeChannelInput))
  @public val out = IO(Output(new ScalarMultiUopDecodeChannelOutput(maxSplit)))

  private val rawInst = in.rawInst
  private val instFields = rawInst.asTypeOf(new XSInstBitFields)

  private val uopInfoFields = Seq.tabulate(maxSplit)(i => new UopInfoField(i, extensions))
  private val opcodeFields = Seq.tabulate(maxSplit)(i => new OpcodeField(i, extensions))
  private val fuTypeFields = Seq.tabulate(maxSplit)(i => new FuTypeField(i, extensions))
  private val lsrc1Fields = Seq.tabulate(maxSplit)(i => new LogicalRegField(i, 0, extensions))
  private val lsrc2Fields = Seq.tabulate(maxSplit)(i => new LogicalRegField(i, 1, extensions))
  private val lsrc3Fields = Seq.tabulate(maxSplit)(i => new LogicalRegField(i, 2, extensions))
  private val ldestFields = Seq.tabulate(maxSplit)(i => new LogicalRegField(i, 3, extensions))
  private val immFields = Seq.tabulate(maxSplit)(i => new UopImmInfoField(i, extensions, XLEN))
  private val commitFields = Seq.tabulate(maxSplit)(i => new UopCommitTypeField(i, extensions))
  private val numUopField = new NumUopField(extensions)

  private val fields = uopInfoFields ++ opcodeFields ++ fuTypeFields ++
    lsrc1Fields ++ lsrc2Fields ++ lsrc3Fields ++ ldestFields ++
    immFields ++ commitFields ++ Seq(
      numUopField, ScalarNumWbField, ScalarCanRobCompressField, IsSsamoswapField,
      PrivExceptionCauseField, FrmRenField, FFlagsWenField, NeedFsField,
    )

  private val table = new DecodeTable(instSeq, fields)
  private val result = table.decode(rawInst)

  private val uopInfos = uopInfoFields.map(result(_))
  private val opcodes = opcodeFields.map(result(_))
  private val fuTypes = fuTypeFields.map(result(_))
  private val lsrc1Specs = lsrc1Fields.map(result(_))
  private val lsrc2Specs = lsrc2Fields.map(result(_))
  private val lsrc3Specs = lsrc3Fields.map(result(_))
  private val ldestSpecs = ldestFields.map(result(_))
  private val immInfos = immFields.map(result(_))
  private val commitTypes = commitFields.map(result(_))

  private val imms = immInfos.map { info =>
    val fromInst = LookupTree(info.selImm.bits, ImmUnion.immSelMap.map {
      case (sel, enum) => sel -> enum.minBitsFromInstr(rawInst).ensuring(_.getWidth == enum.len)
    })
    Mux(info.constant.valid, info.constant.bits, fromInst)
  }

  private val (privExceptionII, privExceptionVI) =
    privExceptions(result(PrivExceptionCauseField), in.fromCSR, HasCMO)
  private val ssamoswapExceptionII = if (HasShadowStack) {
    result(IsSsamoswapField) && in.fromCSR.illegalInst.ssamoswap.getOrElse(false.B)
  } else false.B
  private val ssamoswapExceptionVI = if (HasShadowStack) {
    result(IsSsamoswapField) && in.fromCSR.virtualInst.ssamoswap.getOrElse(false.B)
  } else false.B
  // The top-level selection only consumes this count when the first uop is valid.
  out.uopNum := result(numUopField) +& 1.U

  for (i <- 0 until maxSplit) {
    val lsrc1 = LogicalRegField.decodeSelector(lsrc1Specs(i), instFields)
    val lsrc2 = LogicalRegField.decodeSelector(lsrc2Specs(i), instFields)
    val lsrc3 = LogicalRegField.decodeSelector(lsrc3Specs(i), instFields)
    val ldest = LogicalRegField.decodeSelector(ldestSpecs(i), instFields)

    out.uop(i).valid := uopInfos(i).valid
    out.uop(i).bits := 0.U.asTypeOf(out.uop(i).bits)
    out.uop(i).bits.renameInfo := uopInfos(i).bits
    out.uop(i).bits.renameInfo.gpWen := uopInfos(i).bits.gpWen && ldest =/= 0.U
    out.uop(i).bits.fuType := fuTypes(i)
    out.uop(i).bits.opcode := opcodes(i)
    out.uop(i).bits.lsrc1 := lsrc1
    out.uop(i).bits.lsrc2 := lsrc2
    out.uop(i).bits.lsrc3 := lsrc3
    out.uop(i).bits.ldest := ldest
    out.uop(i).bits.selImm := immInfos(i).selImm
    out.uop(i).bits.imm := imms(i)
    out.uop(i).bits.commitType := commitTypes(i)
    out.uop(i).bits.canRobCompress := result(ScalarCanRobCompressField)
    out.uop(i).bits.numWb := result(ScalarNumWbField)
    out.uop(i).bits.uopIdx := i.U
    out.uop(i).bits.isFirstUop := (i == 0).B
    out.uop(i).bits.isLastUop := i.U === result(numUopField)
    out.uop(i).bits.frmRen := result(FrmRenField)
    out.uop(i).bits.fflagsWen := result(FFlagsWenField)
    out.uop(i).bits.frm := instFields.RM
    out.uop(i).bits.frmIll := instFields.RM === 5.U || instFields.RM === 6.U
    val frmExceptionII = out.uop(i).bits.frmRen &&
      (out.uop(i).bits.frmIll || (out.uop(i).bits.frm === Frm.DYN && in.fromCSR.illegalInst.frm))
    out.uop(i).bits.exceptionII := privExceptionII || ssamoswapExceptionII || frmExceptionII ||
      (result(NeedFsField) && in.fromCSR.illegalInst.fsIsOff)
    out.uop(i).bits.exceptionVI := privExceptionVI || ssamoswapExceptionVI
  }
}

object ScalarMultiUopDecodeChannel {
  // Route whole families here so their single-uop members use the same metadata and exception rules.
  def channelExtensions(available: Seq[ExtBase], hasShadowStack: Boolean): Seq[ExtBase] = {
    val families = if (hasShadowStack) Seq(Zicfiss) else Seq.empty
    available.intersect(families)
  }

  def splitCounts(insts: Seq[InstPattern], extensions: Seq[ExtBase]): Seq[Int] =
    insts.map(UopInfoFieldSimple.genUopSeq(_, extensions).length).distinct.sorted

  private def privExceptions(cause: PrivExceptionCause.Type, fromCSR: CSRToDecode,
    hasCMO: Boolean): (Bool, Bool) = {
    val sources = Seq(
      (PrivExceptionCause.sfenceVMA,  fromCSR.illegalInst.sfenceVMA,              fromCSR.virtualInst.sfenceVMA),
      (PrivExceptionCause.sfencePart, fromCSR.illegalInst.sfencePart,             fromCSR.virtualInst.sfencePart),
      (PrivExceptionCause.hfenceGVMA, fromCSR.illegalInst.hfenceGVMA,             fromCSR.virtualInst.hfence),
      (PrivExceptionCause.hfenceVVMA, fromCSR.illegalInst.hfenceVVMA,             fromCSR.virtualInst.hfence),
      (PrivExceptionCause.mfence,     fromCSR.illegalInst.mfence.getOrElse(false.B), false.B),
      (PrivExceptionCause.hlsv,       fromCSR.illegalInst.hlsv,                   fromCSR.virtualInst.hlsv),
      (PrivExceptionCause.wfi,        fromCSR.illegalInst.wfi,                    fromCSR.virtualInst.wfi),
      (PrivExceptionCause.wrsNto,     fromCSR.illegalInst.wrs_nto,                fromCSR.virtualInst.wrs_nto),
      (PrivExceptionCause.cboZ,       !hasCMO.B || fromCSR.illegalInst.cboZ,      fromCSR.virtualInst.cboZ),
      (PrivExceptionCause.cboCF,      !hasCMO.B || fromCSR.illegalInst.cboCF,     fromCSR.virtualInst.cboCF),
      (PrivExceptionCause.cboI,       !hasCMO.B || fromCSR.illegalInst.cboI,      fromCSR.virtualInst.cboI),
      (PrivExceptionCause.aes64ks1i,  true.B,                                     false.B),
      (PrivExceptionCause.amocasQ,    true.B,                                     false.B),
    )
    val illegal = Mux1H(sources.map { case (c, ii, _) => (cause === c) -> ii })
    val virtual = Mux1H(sources.map { case (c, _, vi) => (cause === c) -> vi })
    (illegal, virtual)
  }

  class ScalarMultiUopDecodeChannelOutputUop extends Bundle {
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

  class ScalarMultiUopDecodeChannelOutput(val uopWidth: Int) extends Bundle {
    val uop = Vec(uopWidth, ValidIO(new ScalarMultiUopDecodeChannelOutputUop))
    // Exact uop count, binary encoded; meaningful only when uop.head.valid is true.
    val uopNum = UInt(NumUopOH.width.W)
  }
}
