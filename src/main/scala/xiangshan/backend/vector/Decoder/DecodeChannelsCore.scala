package xiangshan.backend.vector.Decoder

import chisel3._
import chisel3.experimental.hierarchy.core.{Definition, Instance}
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utils.BundleUtils.makeValid
import xiangshan._
import xiangshan.backend.decode.isa.Extensions._
import xiangshan.backend.decode.isa.bitfield.{BitFieldsVec, Riscv32BitInst}
import xiangshan.backend.fu.wrapper.CSRToDecode
import xiangshan.backend.fu.vector.Bundles.{VType, Vstart}
import xiangshan.backend.vector.Decoder.DecodeChannel.SimpleDecodeChannel.SimpleDecodeChannelOutputUop
import xiangshan.backend.vector.Decoder.DecodeChannel.VectorDecodeChannel.VecDecodeChannelOutputUop
import xiangshan.backend.vector.Decoder.DecodeChannel._
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.Types._
import xiangshan.backend.vector._
import xiangshan.backend.vector.util.ScalaTypeExt.BooleanToExt

class DecodeChannelsCore(
  mopWidth: Int,
  uopWidth: Int,
  numM2M4M8Channel: (Int, Int, Int) = (8, 8, 8),
  postfix: String = "",
)(
  implicit val p: Parameters
) extends Module with HasVectorSettings with HasXSParameter {
  private val maxSimpleSplitUopNum = 2

  val MaxM2UopIdx = numM2M4M8Channel._1
  val MaxM4UopIdx = numM2M4M8Channel._2
  val MaxM8UopIdx = numM2M4M8Channel._3
  require(mopWidth >= MaxM8UopIdx && MaxM8UopIdx >= MaxM4UopIdx && MaxM4UopIdx >= MaxM2UopIdx)

  override def desiredName: String = s"DecodeChannelsCore${postfix}" +
    s"_MOP${mopWidth}_UOP${uopWidth}" +
    s"_M2x${MaxM2UopIdx}_M4x${MaxM4UopIdx}_M8x${MaxM8UopIdx}"

  require(extensions.distinct.size == extensions.size, "Duplicate extensions are not allowed")
  val vectorExtsSeq = Seq(V, Zvbb, Zvknha, Zacas, ZacasZabha)

  val simpleExts: Seq[ExtBase] = extensions.diff(vectorExtsSeq)
  val simpleInsts = InstPattern.extensionInsts(simpleExts: _*)

  val vectorExts = extensions.intersect(vectorExtsSeq)
  val vectorInsts = InstPattern.extensionInsts(vectorExts: _*).map(_.asInstanceOf[VecInstPattern])

  val pseudoInsts: Seq[InstPattern] = PseudoDecodeChannel.uopTable.keys.toSeq
  val vsetInsts: Seq[InstPattern] = vectorInsts.collect { case p: VecConfigInstPattern => p }

  val vecComputeInsts = vectorInsts.diff(vsetInsts)
  val allChannelInsts: Seq[InstPattern] = simpleInsts ++ pseudoInsts ++ vecComputeInsts ++ vsetInsts

  val in = IO(new Bundle {
    val mops = Input(Vec(mopWidth, Valid(new Bundle {
      val info = new DecodeChannelInput
    } )))
  })

  val out = IO(Output(new Bundle {
    val vecChannel = Vec(mopWidth, Vec(maxSplitUopNum, ValidIO(new DecodeChannelOutput)))
    val vsetChannel = Vec(mopWidth, ValidIO(new DecodeChannelOutput))
    val simChannel = Vec(mopWidth, Vec(maxSimpleSplitUopNum, ValidIO(new DecodeChannelOutput)))
    val psdChannel = Vec(mopWidth, ValidIO(new DecodeChannelOutput))
    val illegalChannel = Vec(mopWidth, ValidIO(new DecodeChannelOutput))
    val vecUopNumOH = Vec(mopWidth, NumUopOH())
    val simUopNumOH = Vec(mopWidth, NumUopOH())
  }))

  val instValids = in.mops.map(_.valid)
  val insts: Seq[Riscv32BitInst with BitFieldsVec] = in.mops.map(_.bits.info.rawInst.asTypeOf(new Riscv32BitInst with BitFieldsVec))

  val vectorDecodeChannelM8: Definition[VectorDecodeChannel] = Definition(new VectorDecodeChannel(vecComputeInsts))
  val vsetDecodeChannel: Definition[VsetDecoder] = Definition(new VsetDecoder)

  val simpleDecodeChannelM2: Definition[SimpleDecodeChannel] = Definition(new SimpleDecodeChannel(simpleInsts))
  val pseudoDecodeChannel: Definition[PseudoDecodeChannel] = Definition(new PseudoDecodeChannel())

  val vectorDecodeChannels: Seq[Instance[VectorDecodeChannel]] = Seq.tabulate(mopWidth) { i =>
    Instance(vectorDecodeChannelM8)
  }
  val vsetDecodeChannels = Seq.fill(mopWidth)(Instance(vsetDecodeChannel))
  val simpleDecodeChannels = Seq.fill(mopWidth)(Instance(simpleDecodeChannelM2))
  val pseudoDecodeChannels = Seq.fill(mopWidth)(Instance(pseudoDecodeChannel))

  val vecUopOuts: Seq[Seq[ValidIO[VecDecodeChannelOutputUop]]] = vectorDecodeChannels.map(_.out.uop)
  val vsetUopOuts: Seq[ValidIO[VsetDecoder.Out]] = vsetDecodeChannels.map(_.out)
  val simUopOuts: Seq[Seq[ValidIO[SimpleDecodeChannelOutputUop]]] = simpleDecodeChannels.map(_.out.uop)
  val psdUopOuts: Seq[ValidIO[PseudoDecodeChannel.Out]] = pseudoDecodeChannels.map(_.out)

  val vecUopNumOHs: Seq[NumUopOH.Type] = vectorDecodeChannels.map(_.out.uopNumOH)
  val simpleUopNumOHs: Seq[NumUopOH.Type] = simpleDecodeChannels.map(_.out.uopNumOH)

  val vecChannelOut: Seq[Seq[ValidIO[DecodeChannelOutput]]] =
    vecUopOuts.map(_.map(x => makeValid(x.valid, DecodeChannelOutput.fromVecChannelUop(x.bits))))
  val vsetChannelOut =
    vsetUopOuts.map(x => makeValid(x.valid, DecodeChannelOutput.fromVSetChannelUop(x.bits)))
  val simChannelOut: Seq[Seq[ValidIO[DecodeChannelOutput]]] =
    simUopOuts.map(_.map(x => makeValid(x.valid, DecodeChannelOutput.fromSimpleChannelUop(x.bits))))
  val psdChannelOut: Seq[ValidIO[DecodeChannelOutput]] =
    psdUopOuts.map(x => makeValid(x.valid, DecodeChannelOutput.fromPseudoChannelUop(x.bits, VLEN)))

  val illegalChannelOut: Seq[ValidIO[DecodeChannelOutput]] = (0 until mopWidth).map { i =>
    val hasDecodedUop = vecChannelOut(i).head.valid ||
                        vsetChannelOut(i).valid ||
                        simChannelOut(i).head.valid ||
                        psdChannelOut(i).valid
    makeValid(instValids(i) && !hasDecodedUop, DecodeChannelOutput.illegalUop())
  }

  // Connect inputs to channel modules
  val vecDecodeChannelsIn: Seq[DecodeChannelInput] = vectorDecodeChannels.map(_.in)
  vecDecodeChannelsIn.zipWithIndex.foreach { case (modIn, i) =>
    modIn.rawInst := in.mops(i).bits.info.rawInst
    modIn.vtype   := in.mops(i).bits.info.vtype
    modIn.fromCSR := in.mops(i).bits.info.fromCSR
    modIn.vstart  := in.mops(i).bits.info.vstart
  }

  val vsetDecodeChannelsIn: Seq[VsetDecoder.In] = vsetDecodeChannels.map(_.in)
  vsetDecodeChannelsIn.zipWithIndex.foreach { case (modIn, i) =>
    modIn.rawInst := in.mops(i).bits.info.rawInst
    modIn.vsIsOff := in.mops(i).bits.info.fromCSR.illegalInst.vsIsOff
  }

  val simDecodeChannelsIn: Seq[DecodeChannelInput] = simpleDecodeChannels.map(_.in)
  simDecodeChannelsIn.zipWithIndex.foreach { case (modIn, i) =>
    modIn.rawInst := in.mops(i).bits.info.rawInst
    modIn.vtype   := in.mops(i).bits.info.vtype
    modIn.fromCSR := in.mops(i).bits.info.fromCSR
    modIn.vstart  := in.mops(i).bits.info.vstart
  }

  val psdDecodeChannelsIn: Seq[PseudoDecodeChannel.In] = pseudoDecodeChannels.map(_.in)
  psdDecodeChannelsIn.zipWithIndex.foreach { case (modIn, i) =>
    modIn.rawInst := in.mops(i).bits.info.rawInst
    modIn.fromCSR := in.mops(i).bits.info.fromCSR
  }

  // Connect outputs
  for (i <- 0 until mopWidth) {
    for (j <- 0 until maxSplitUopNum) {
      out.vecChannel(i)(j) := vecChannelOut(i)(j)
    }
    out.vsetChannel(i) := vsetChannelOut(i)
    for (j <- 0 until maxSimpleSplitUopNum) {
      out.simChannel(i)(j) := simChannelOut(i)(j)
    }
    out.psdChannel(i) := psdChannelOut(i)
    out.illegalChannel(i) := illegalChannelOut(i)
    out.vecUopNumOH(i) := vecUopNumOHs(i)
    out.simUopNumOH(i) := simpleUopNumOHs(i)
  }

  // Channel exclusivity invariants (relied on by uopNumOH selection):
  // - at most one of {vset, vector, simple} channels is valid for a valid mop
  // - if the pseudo channel is valid, the simple channel must also be valid
  for (i <- 0 until mopWidth) {
    val vecValid  = vecChannelOut(i).head.valid
    val vsetValid = vsetChannelOut(i).valid
    val simValid  = simChannelOut(i).head.valid
    val psdValid  = psdChannelOut(i).valid
    assert(!instValids(i) || PopCount(Seq(vsetValid, vecValid, simValid)) <= 1.U,
      "DecodeChannelsCore: at most one of vset/vector/simple channels may be valid")
    assert(!instValids(i) || !psdValid || simValid,
      "DecodeChannelsCore: pseudo channel valid implies simple channel valid")
  }
}
