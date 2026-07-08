package xiangshan.backend.vector.Decoder

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import top.{ArgParser, Generator}
import utils.BundleUtils.makeValid
import xiangshan._
import xiangshan.backend.Bundles.{DecodeInMopDebug, UopIdx}
import xiangshan.backend.decode.isa.Extensions._
import xiangshan.backend.decode.isa.bitfield.{BitFieldsVec, Riscv32BitInst}
import xiangshan.backend.decode.opcode.Opcode
import xiangshan.backend.decode.opcode.Opcode.Opcode
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.wrapper.CSRToDecode
import xiangshan.backend.fu.vector.Bundles.{VType, Vstart}
import xiangshan.backend.vector.Decoder.DecodeChannel.VectorDecodeChannel.VecDecodeChannelOutputUop
import xiangshan.backend.vector.Decoder.DecodeChannel.SimpleDecodeChannel.SimpleDecodeChannelOutputUop
import xiangshan.backend.vector.Decoder.DecodeChannel._
import xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel.Frm
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.Select.{BufferSelectModule, UopSelectModule}
import xiangshan.backend.vector.Decoder.Split.VecUopSplitModule
import xiangshan.backend.vector.Decoder.Types._
import xiangshan.backend.vector._
import xiangshan.backend.vector.util.ScalaTypeExt.BooleanToExt
import xiangshan.backend.vector.util.Select.Mux1HLookUp
import xiangshan.backend.vector.util.Verilog
import xiangshan.frontend.ftq.FtqPtr

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.language.implicitConversions

class DecodeChannels(
  mopWidth: Int,
  uopWidth: Int,
  numM2M4M8Channel: (Int, Int, Int) = (8, 8, 8),
  postfix: String = "",
)(
  implicit p: Parameters
) extends Module with HasVectorSettings with HasSimpleSettings {
  val MaxM2UopIdx = numM2M4M8Channel._1
  val MaxM4UopIdx = numM2M4M8Channel._2
  val MaxM8UopIdx = numM2M4M8Channel._3
  require(mopWidth >= MaxM8UopIdx && MaxM8UopIdx >= MaxM4UopIdx && MaxM4UopIdx >= MaxM2UopIdx)

  override def desiredName: String = s"DecodeChannels${postfix}" +
    s"_MOP${mopWidth}_UOP${uopWidth}" +
    s"_M2x${MaxM2UopIdx}_M4x${MaxM4UopIdx}_M8x${MaxM8UopIdx}"

  require(extensions.distinct.size == extensions.size, "Duplicate extensions are not allowed")
  val simpleExts: Seq[ExtBase] = extensions.diff(Seq(V, Zvbb, Zvknha, Zacas, ZacasZabha))
  val simpleInsts = InstPattern.extensionInsts(simpleExts: _*)

  // Get vector instruction bits pattern by extensions
  val vectorExts = extensions.intersect(Seq(V, Zvbb, Zvknha, Zacas, ZacasZabha))
  val vectorInsts = InstPattern.extensionInsts(vectorExts: _*).map(_.asInstanceOf[VecInstPattern])

  val uopBufferSize = maxSplitUopNum - 1

  val in = IO(new Bundle {
    // used to flush UopBuffer
    val redirect = Input(Bool())
    // if rename can accept all UopWidth uops
    val renameCanAccept = Input(Bool())
    val mops = Flipped(Vec(mopWidth, DecoupledIO(new Bundle {
      val info = new DecodeChannelInput
      val ctrl = new MopCtrlBundle
    } )))
  })

  val out = IO(Output(new Bundle {
    val uops = Vec(uopWidth, ValidIO(new Bundle {
      val info = new DecodeChannelOutput
      val ctrl = new MopCtrlBundle
    }))
    val uopBufferNum = Option.when(p(DebugOptionsKey).EnableDifftest)(UopBufferNum())
    val accNum = Option.when(p(DebugOptionsKey).EnableDifftest)(UInt(log2Up(mopWidth + 1).W))
  }))

  val instValids = in.mops.map(_.valid)

  val decodeChannelsCore = Module(new DecodeChannelsCore(mopWidth, uopWidth, numM2M4M8Channel))
  decodeChannelsCore.in.mops.zip(in.mops).foreach { case (corePort, extPort) =>
    corePort.valid := extPort.valid
    corePort.bits.info := extPort.bits.info
  }

  val vecChannelOut: Seq[Vec[ValidIO[DecodeChannelOutput]]] = (0 until mopWidth).map(i => decodeChannelsCore.out.vecChannel(i))
  val vsetChannelOut    = (0 until mopWidth).map(i => decodeChannelsCore.out.vsetChannel(i))
  val simChannelOut: Seq[Vec[ValidIO[DecodeChannelOutput]]] = (0 until mopWidth).map(i => decodeChannelsCore.out.simChannel(i))
  val psdChannelOut     = (0 until mopWidth).map(i => decodeChannelsCore.out.psdChannel(i))
  val illegalChannelOut = (0 until mopWidth).map(i => decodeChannelsCore.out.illegalChannel(i))
  val vecUopNumOHs      = (0 until mopWidth).map(i => decodeChannelsCore.out.vecUopNumOH(i))
  val simpleUopNumOHs   = (0 until mopWidth).map(i => decodeChannelsCore.out.simUopNumOH(i))

  val uopBufferUpdateVec = Wire(Vec(uopBufferLength, Bool()))
  val uopBufferUpdate = Cat(uopBufferUpdateVec).orR

  // should be 0~7
  val uopBufferNumNext = Wire(UInt(log2Up(uopBufferSize).W))
  val uopBufferNum = RegEnable(uopBufferNumNext, 0.U(log2Up(uopBufferSize).W), out.uops.head.valid && in.renameCanAccept || in.redirect)

  val uopBufferValid = Wire(Vec(uopBufferSize, Bool()))
  val uopBufferNext = Wire(Vec(uopBufferSize, new DecodeChannelOutput))
  val uopBuffer: Seq[DecodeChannelOutput] = (uopBufferNext zip uopBufferUpdateVec).map {
    case (next, update) =>
      RegEnable(next, update)
  }

  val bufferedMopCtrlNext = Wire(new MopCtrlBundle)
  val bufferedMopCtrl = RegEnable(bufferedMopCtrlNext, uopBufferUpdate)

  val uopBufferCtrlDecoder = Module(new UopBufferCtrlDecoder(
    mopWidth = mopWidth,
    uopWidth = uopWidth,
    uopBufferLength = uopBufferSize,
    numM2M4M8Channel = numM2M4M8Channel,
  ))

  val uopSelectMod = Module(new UopSelectModule(
    uopBundle = new DecodeChannelOutput,
    mopBundle = new MopCtrlBundle,
    mopWidth = mopWidth,
    uopWidth = uopWidth,
    uopBufferSize = uopBufferSize,
    filteredPorts = uopBufferCtrlDecoder.uopUsedPorts,
  ))

  val bufferSelectMod = Module(new BufferSelectModule(
    uopBundle = new DecodeChannelOutput,
    mopBundle = new MopCtrlBundle,
    mopWidth = mopWidth,
    uopWidth = uopWidth,
    uopBufferLength = uopBufferSize,
    filteredPorts = uopBufferCtrlDecoder.bufferUsedPorts,
  ))

  /**
   * connection of [[uopBufferCtrlDecoder]]
   */

  uopBufferCtrlDecoder.in.uopBufferNum := uopBufferNum

  val gatedChannelUopNum = VecInit(in.mops.map(m => Mux(m.valid, m.bits.ctrl.uopNumOH, NumUopOH.N0)))
  uopBufferCtrlDecoder.in.channelUopNum := gatedChannelUopNum

  if (p(XSCoreParamsKey).backendParams.debugEn) {
    val recomputedChannelUopNum = (0 until mopWidth).map { i =>
      Mux(
        !instValids(i),
        NumUopOH.N0,
        Mux(
          illegalChannelOut(i).valid,
          NumUopOH.N1,
          Mux(
            psdChannelOut(i).valid,
            NumUopOH.N1,
            Mux1H(Seq(
              vecChannelOut(i).head.valid -> vecUopNumOHs(i),
              vsetChannelOut(i).valid     -> NumUopOH.N1,
              simChannelOut(i).head.valid -> simpleUopNumOHs(i),
            )),
          ),
        ),
      )
    }
    for (i <- 0 until mopWidth) {
      assert(
        gatedChannelUopNum(i) === recomputedChannelUopNum(i),
        "channelUopNum mismatch at mop %d: fromInput=%b recomputed=%b\n",
        i.U,
        gatedChannelUopNum(i),
        recomputedChannelUopNum(i),
      )
    }
  }

  uopBufferNumNext := Mux(in.redirect, 0.U, uopBufferCtrlDecoder.out.uopBufferNum)
  uopBufferUpdateVec := uopBufferCtrlDecoder.out.bufferValids.map(_ && in.renameCanAccept)
  uopBufferValid := uopBufferCtrlDecoder.out.bufferValids

  /**
   * connection of [[uopSelectMod]]
   */

  require(maxSimpleSplitUopNum <= maxSplitUopNum, "maxSimpleSplitUopNum should be less than or equal to maxSplitUopNum")
  for (i <- 0 until mopWidth) {
    uopSelectMod.in.uopFromChannel(i)(0) :=
      Mux(
        illegalChannelOut(i).valid,
        illegalChannelOut(i).bits,
        Mux(
          psdChannelOut(i).valid,
          psdChannelOut(i).bits,
          Mux1H(Seq(
            vecChannelOut(i)(0).valid -> vecChannelOut(i)(0).bits,
            vsetChannelOut(i).valid -> vsetChannelOut(i).bits,
            simChannelOut(i)(0).valid -> simChannelOut(i)(0).bits,
          ))
        )
      )
    for (j <- 1 until maxSimpleSplitUopNum) {
      uopSelectMod.in.uopFromChannel(i)(j) := Mux1H(Seq(
        vecChannelOut(i)(j).valid -> vecChannelOut(i)(j).bits,
        simChannelOut(i)(j).valid -> simChannelOut(i)(j).bits,
      ))
    }
    for (j <- maxSimpleSplitUopNum until maxSplitUopNum) {
      uopSelectMod.in.uopFromChannel(i)(j) := vecChannelOut(i)(j).bits
    }
  }
  uopSelectMod.in.uopFromBuffer := uopBuffer
  uopSelectMod.in.mopFromInput := in.mops.map(_.bits.ctrl)
  uopSelectMod.in.mopFromBuffer := bufferedMopCtrl
  uopSelectMod.in.uopSelect := uopBufferCtrlDecoder.out.selForUop

  /**
   * connection of [[bufferSelectMod]]
   */

  bufferSelectMod.in.uopFromChannel := uopSelectMod.in.uopFromChannel
  bufferSelectMod.in.uopFromBuffer := uopBuffer
  bufferSelectMod.in.mopFromInput := in.mops.map(_.bits.ctrl)
  bufferSelectMod.in.mopFromBuffer := bufferedMopCtrl
  bufferSelectMod.in.uopSelect := uopBufferCtrlDecoder.out.selForBuffer
  bufferSelectMod.in.mopAcceptVec := uopBufferCtrlDecoder.out.acceptVec.map(_ && in.renameCanAccept)

  uopBufferNext := bufferSelectMod.out.decodedInfoOut
  bufferedMopCtrlNext := bufferSelectMod.out.mopToBuffer


  for (i <- 0 until mopWidth) {
    in.mops(i).ready := uopBufferCtrlDecoder.out.acceptVec(i) && in.renameCanAccept
  }

  for (i <- 0 until uopWidth) {
    out.uops(i).valid := uopBufferCtrlDecoder.out.uopValids(i)
    out.uops(i).bits.info := uopSelectMod.out.decodedInfoOut(i)
    out.uops(i).bits.ctrl := uopSelectMod.out.bypassInfoOut(i)
  }

  if (p(DebugOptionsKey).EnableDifftest) {
    out.uopBufferNum.get := uopBufferNum
    out.accNum.get := uopBufferCtrlDecoder.out.accNum
  }
}

class DecodeChannelInput(implicit p: Parameters) extends Bundle {
  val rawInst = UInt(32.W)
  val vtype = VType()
  val fromCSR = new CSRToDecode
  val vstart = Vstart()
  def sew: UInt = vtype.vsew
  def lmul: UInt = vtype.vlmul
  def ma: Bool = vtype.vma
  def ta: Bool = vtype.vta
}

class DecodeChannelOutput extends Bundle {
  val fuType: UInt = FuType()
  val opcode: UInt = Opcode()
  val isVset: Bool = Bool()

  val src1Ren = Bool()
  val src1Type = DecodeSrcType()
  val src2Ren = Bool()
  val src2Type = DecodeSrcType()
  val src3Ren = Bool()
  val src3Type = DecodeSrcType()
  val lsrc1 = UInt(5.W)
  val lsrc2 = UInt(5.W)
  val lsrc3 = UInt(5.W)
  val vlRen = Bool()
  val v0Ren = Bool()
  val frmRen = Bool()
  val maskType = MaskTypeChiselEnum()
  val intRmRen = Bool()

  val gpWen = Bool()
  val fpWen = Bool()
  val vpWen = Bool()
  val ldest = UInt(5.W)

  val vlWen = Bool()
  val vxsatWen = Bool()
  val fflagsWen = Bool()
  val dirtyVs = Bool()

  val frm = Frm()
  val frmIll = Bool()

  val vm = Bool()

  val noSpec = Bool()
  val blockBack = Bool()
  val flushPipe = Bool()
  val selImm = ValidIO(DecodeSelImm())
  val imm = UInt(32.W)
  val commitType = CommitType()
  val vdDepElim = VdDepElim()
  val isWritePartVd = Bool()

  val canRobCompress = Bool()

  val numWb = NumWB()
  val uopIdx = UopIdx()
  val isFirstUop = Bool()
  val isLastUop = Bool()
  val src12Rev = Bool()
  val isMove = Bool()

  val isJ = Bool()
  val isJr = Bool()

  val exceptionII = Bool()
  val exceptionVI = Bool()
}

object DecodeChannelOutput {
  def illegalUop(): DecodeChannelOutput = {
    val uop = WireInit(0.U.asTypeOf(new DecodeChannelOutput))

    uop.commitType  := CommitType.NORMAL
    uop.isFirstUop  := true.B
    uop.isLastUop   := true.B
    uop.exceptionII := true.B

    uop
  }

  def fromVecChannelUop(vuop: VecDecodeChannelOutputUop): DecodeChannelOutput = {
    val uop = Wire(new DecodeChannelOutput)

    uop.fuType := vuop.fuType
    uop.opcode := vuop.opcode
    uop.isVset := vuop.isVset

    uop.src1Ren := Mux(vuop.src12Rev, vuop.renameInfo.src2Ren, vuop.renameInfo.src1Ren)
    uop.src1Type := Mux(vuop.src12Rev, vuop.renameInfo.src2Type, vuop.renameInfo.src1Type)
    uop.src2Ren := Mux(vuop.src12Rev, vuop.renameInfo.src1Ren,vuop.renameInfo.src2Ren)
    uop.src2Type := Mux(vuop.src12Rev, vuop.renameInfo.src1Type,vuop.renameInfo.src2Type)
    uop.src3Ren := vuop.renameInfo.readVdAsSrc || vuop.vdDepElim =/= VdDepElim.Always
    uop.src3Type.value := DecodeSrcType.VP
    uop.lsrc1 := Mux(vuop.src12Rev, vuop.src.src2, vuop.src.src1)
    uop.lsrc2 := Mux(vuop.src12Rev, vuop.src.src1, vuop.src.src2)
    uop.lsrc3 := vuop.src.dest
    uop.vlRen := vuop.renameInfo.vlRen
    uop.v0Ren := vuop.v0Ren
    uop.frmRen := vuop.frmRen
    uop.maskType := vuop.renameInfo.maskType
    uop.intRmRen := vuop.renameInfo.intRmRen
    uop.gpWen := vuop.renameInfo.gpWen
    uop.fpWen := vuop.renameInfo.fpWen
    uop.vpWen := vuop.renameInfo.vpWen

    uop.vlWen := vuop.renameInfo.vlWen
    uop.vxsatWen := vuop.renameInfo.vxsatWen
    uop.fflagsWen := vuop.fflagsWen
    uop.dirtyVs := vuop.dirtyVs
    uop.ldest := vuop.src.dest

    uop.frm := vuop.frm
    uop.frmIll := false.B

    uop.vm := vuop.vm

    uop.noSpec := vuop.renameInfo.noSpec
    uop.blockBack := vuop.renameInfo.blockBack

    uop.flushPipe := false.B
    uop.selImm := vuop.selImm
    uop.imm := vuop.imm
    uop.commitType := vuop.commitType
    uop.vdDepElim := vuop.vdDepElim
    uop.isWritePartVd := vuop.isWritePartVd

    uop.canRobCompress := false.B

    uop.numWb := vuop.numWb
    uop.uopIdx := vuop.uopIdx
    uop.isFirstUop := vuop.isFirstUop
    uop.isLastUop := vuop.isLastUop
    uop.src12Rev := vuop.src12Rev

    uop.isMove := false.B

    uop.isJ := false.B
    uop.isJr := false.B

    uop.exceptionII := vuop.exceptionII
    uop.exceptionVI := false.B

    uop
  }

  def fromVSetChannelUop(vuop: VsetDecoder.Out): DecodeChannelOutput = {
    val uop = Wire(new DecodeChannelOutput)

    uop.fuType := vuop.fuType
    uop.opcode := vuop.opcode
    uop.isVset := true.B

    uop.src1Ren := vuop.renameInfo.bits.src1Ren
    uop.src1Type := vuop.renameInfo.bits.src1Type
    uop.src2Ren := vuop.renameInfo.bits.src2Ren
    uop.src2Type := vuop.renameInfo.bits.src2Type
    uop.src3Ren := false.B
    uop.src3Type := 0.U.asTypeOf(uop.src3Type)
    uop.lsrc1 := vuop.src.src1
    uop.lsrc2 := vuop.src.src2
    uop.lsrc3 := 0.U
    uop.vlRen := vuop.renameInfo.bits.vlRen
    uop.v0Ren := false.B
    uop.frmRen := false.B
    uop.maskType := DontCare
    uop.intRmRen := false.B
    uop.gpWen := vuop.renameInfo.bits.gpWen
    uop.fpWen := vuop.renameInfo.bits.fpWen
    uop.vpWen := vuop.renameInfo.bits.vpWen

    uop.vlWen := vuop.renameInfo.bits.vlWen
    uop.vxsatWen := vuop.renameInfo.bits.vxsatWen
    uop.fflagsWen := false.B
    uop.dirtyVs := true.B

    uop.ldest := vuop.src.dest

    uop.frm := 0.U.asTypeOf(uop.frm)
    uop.frmIll := false.B

    uop.vm := false.B

    uop.noSpec := false.B
    uop.blockBack := false.B
    uop.flushPipe := vuop.flushPipe
    uop.selImm := vuop.selImm
    uop.imm := vuop.imm
    uop.commitType := CommitType.NORMAL
    uop.vdDepElim := VdDepElim.Always // never used
    uop.isWritePartVd := false.B

    // maybe true in the future
    uop.canRobCompress := false.B

    uop.numWb := NumWB.$1
    uop.uopIdx := 0.U
    uop.isFirstUop := true.B
    uop.isLastUop := true.B
    uop.src12Rev := false.B

    uop.isMove := false.B

    uop.isJ := false.B
    uop.isJr := false.B

    uop.exceptionII := vuop.illegal
    uop.exceptionVI := false.B

    uop
  }

  def fromSimpleChannelUop(suop: SimpleDecodeChannelOutputUop): DecodeChannelOutput = {
    val uop = Wire(new DecodeChannelOutput)
    uop.fuType := suop.fuType
    uop.opcode := suop.opcode
    uop.isVset := false.B

    uop.src1Ren := suop.renameInfo.src1Ren
    uop.src1Type := suop.renameInfo.src1Type
    uop.src2Ren := suop.renameInfo.src2Ren
    uop.src2Type := suop.renameInfo.src2Type
    uop.src3Ren := suop.renameInfo.src3Ren
    uop.src3Type := suop.renameInfo.src3Type
    uop.lsrc1 := suop.lsrc1
    uop.lsrc2 := suop.lsrc2
    uop.lsrc3 := suop.lsrc3
    uop.vlRen := false.B
    uop.v0Ren := false.B
    uop.frmRen := suop.frmRen
    uop.maskType := 0.U.asTypeOf(uop.maskType)
    uop.intRmRen := false.B
    uop.gpWen := suop.renameInfo.gpWen
    uop.fpWen := suop.renameInfo.fpWen
    uop.vpWen := false.B

    uop.vlWen := false.B
    uop.vxsatWen := false.B
    uop.fflagsWen := suop.fflagsWen
    uop.dirtyVs := false.B

    uop.ldest := suop.ldest

    uop.frm := suop.frm
    uop.frmIll := suop.frmIll

    uop.vm := false.B

    uop.noSpec := suop.renameInfo.noSpec
    uop.blockBack := suop.renameInfo.blockBack
    uop.flushPipe := suop.renameInfo.flushPipe
    uop.selImm := suop.selImm
    uop.imm := suop.imm
    uop.commitType := suop.commitType
    uop.vdDepElim := VdDepElim.Always // never used
    uop.isWritePartVd := false.B

    uop.canRobCompress := suop.canRobCompress

    uop.numWb := suop.numWb
    uop.uopIdx := suop.uopIdx
    uop.isFirstUop := suop.isFirstUop
    uop.isLastUop := suop.isLastUop
    uop.src12Rev := false.B

    uop.isMove := suop.isMove

    uop.isJ := suop.isJ
    uop.isJr := suop.isJr

    uop.exceptionII := suop.exceptionII
    uop.exceptionVI := suop.exceptionVI

    uop
  }

  def fromPseudoChannelUop(puop: PseudoDecodeChannel.Out, VLEN: Int): DecodeChannelOutput = {
    val uop = Wire(new DecodeChannelOutput)

    uop.fuType := puop.fuType
    uop.opcode := puop.opcode
    uop.isVset := false.B

    uop.src1Ren := puop.src1RenType.ren
    uop.src1Type := puop.src1RenType.typ
    uop.src2Ren := puop.src2RenType.ren
    uop.src2Type := puop.src2RenType.typ
    uop.src3Ren := false.B
    uop.src3Type.value := DecodeSrcType.NO
    uop.lsrc1 := puop.lsrc1
    uop.lsrc2 := puop.lsrc2
    uop.lsrc3 := 0.U
    uop.vlRen := false.B
    uop.v0Ren := false.B
    uop.frmRen := false.B
    uop.maskType := DontCare
    uop.intRmRen := false.B
    uop.gpWen := puop.gpWen
    uop.fpWen := puop.fpWen
    uop.vpWen := false.B

    uop.vlWen := false.B
    uop.vxsatWen := false.B
    uop.fflagsWen := false.B
    uop.dirtyVs := false.B

    uop.ldest := puop.ldest

    uop.frm := Frm.DYN
    uop.frmIll := false.B

    uop.vm := false.B

    uop.noSpec := puop.noSpec
    uop.blockBack := puop.blockBack
    uop.flushPipe := puop.flushPipe
    uop.selImm := puop.selImm
    uop.imm := puop.imm
    uop.commitType := puop.commitType
    uop.vdDepElim := VdDepElim.Always // never used
    uop.isWritePartVd := false.B
    uop.canRobCompress := puop.canRobCompress

    uop.numWb := NumWB.$1
    uop.uopIdx := 0.U
    uop.isFirstUop := true.B
    uop.isLastUop := true.B
    uop.src12Rev := false.B

    uop.isMove := false.B

    uop.isJ := puop.isJ
    uop.isJr := puop.isJr

    uop.exceptionII := puop.exceptionII
    uop.exceptionVI := false.B

    uop
  }
}

class MopCtrlBundle(implicit p: Parameters) extends XSBundle {
  val foldpc           = UInt(MemPredPCWidth.W)
  val exceptionVec     = ExceptSparseVec(ExceptionNO.fromFrontendSet)
  val satpFlushFirstFetchFault = Bool()
  val isFetchMalAddr   = Bool()
  val trigger          = TriggerAction()
  val isRVC            = Bool()
  val fixedTaken       = Bool()
  val predTaken        = Bool()
  val crossPageIPFFix  = Bool()
  val ftqPtr           = new FtqPtr
  val ftqOffset        = UInt(FetchBlockInstOffsetWidth.W)
  val isLastInFtqEntry = Bool()
  val vtype            = VType()
  val oldVType         = VType()
  val rawInst          = UInt(32.W)
  val uopNumOH         = NumUopOH()
  val debug            = Option.when(backendParams.debugEn)(new DecodeInMopDebug())
}

class SrcInfo extends Bundle {
  val typ = DecodeSrcType()
  val idx = UInt(5.W)
}

class SrcRenType extends Bundle {
  val ren = Bool()
  val typ = DecodeSrcType()
}

object SrcRenType {
  def genBitPat(
    typ: Option[OperandType],
  ): BitPat = {
    val typBP = OperandType.genBitPat(typ)
    typ.nonEmpty.toBitPat ## typBP
  }

  def genBitPat(
    typ: OperandType,
  ): BitPat = {
    genBitPat(Some(typ))
  }
}

object DecodeChannelsMain extends App {
  println(s"The width of Opcode: ${Opcode.getWidth}")

  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(
    args :+ "--disable-always-basic-diff" :+ "--dump-fir" :+ "--fpga-platform" :+ "--target" :+ "verilog")

  val defaultConfig = config.alterPartial({
    // Get XSCoreParams and pass it to the "small module"
    case XSCoreParamsKey => config(XSTileKey).head
    case XSVectorParamKey => XSVectorParameters(128)
  })

  // val extensions: Seq[ExtBase] = Seq(
  //   I, M, A, F, D, Zicsr,
  //   System, S,
  //   Za64rs, /*Zacas,*/ Zawrs,
  //   Zba, Zbb, Zbc, Zbs, Zbkb, Zbkc, Zbkx,
  //   V,
  //   Zvbb,
  //   Zvknha,
  //   XSTrap,
  //   // Zcb, Zcmop,
  //   // Zfa, Zfh, ZfaZfh, ZfaF, ZfaD, Zfhmin,
  // )

  Verilog.emitVerilog(
    new DecodeChannels(
      mopWidth = 8,
      uopWidth = 8,
      numM2M4M8Channel = (8, 8, 8),
      postfix = "_" + LocalDate.now().format(DateTimeFormatter.ofPattern("yyyyMMdd")) + "_1"
    )(defaultConfig),
    Array(
      "--full-stacktrace",
      "--target-dir", "build/decoder",
    ),
  )

  println(s"The width of Opcode: ${Opcode.getWidth}")
}

object UopSplitModuleMain extends App {
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(
    args :+ "--disable-always-basic-diff" :+ "--dump-fir" :+ "--fpga-platform" :+ "--target" :+ "verilog")

  val defaultConfig = config.alterPartial({
    // Get XSCoreParams and pass it to the "small module"
    case XSCoreParamsKey => config(XSTileKey).head
    case XSVectorParamKey => XSVectorParameters(128)
  })

  Generator.execute(
    firrtlOpts :+ "--full-stacktrace" :+ "--target-dir" :+ "backend",
    new VecUopSplitModule()(defaultConfig),
    firtoolOpts
  )
}
