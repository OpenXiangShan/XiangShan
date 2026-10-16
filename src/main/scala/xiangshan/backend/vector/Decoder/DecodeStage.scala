package xiangshan.backend.vector.Decoder

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.decode.isa.Extensions._
import xiangshan.backend.decode.SpecZicfilp
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.vector.Bundles.{Vl, Vstart}
import xiangshan.backend.fu.wrapper.CSRToDecode
import xiangshan.backend.rename.RatReadPort
import xiangshan.backend.vector.Decoder.Types.DecodeSelImm
import xiangshan.backend.vector.LatDecoder
import xiangshan.ExceptionNO._

class DecodeStage()(implicit p: Parameters) extends LazyModule {
  override def shouldBeInlined: Boolean = false

  lazy val module = new DecodeStageImp(this)(p)
}

class DecodeStageImp(
  override val wrapper: DecodeStage
)(
  implicit p: Parameters
) extends LazyModuleImp(wrapper)
  with HasXSParameter
  with HasPerfEvents {

  import DecodeStage._

  val in = IO(new In)
  val out = IO(new Out)

  val stallReason = IO(new Bundle {
    val in = Flipped(new StallReasonIO(DecodeWidth))
    val out = new StallReasonIO(DecodeWidth)
  })

  // io alias
  private val outReadys = out.uop.map(_.ready)
  private val inValids  = in.mop.map(_.valid)
  private val outValids = out.uop.map(_.valid)
  private val inValid   = VecInit(inValids).asUInt.orR
  private val outValid  = VecInit(outValids).asUInt.orR

  /** Assume number of ready channels be "RenameWidth" if the first output channel is ready. If not, assume that be 0 */
  val readyCounter = Mux(outReadys.head, RenameWidth.U, 0.U)

  val decodeChannels: DecodeChannels = Module(new DecodeChannels(
    mopWidth = DecodeWidth,
    uopWidth = RenameWidth,
    numM2M4M8Channel = (8, 8, 8),
  ))

  val debug_globalCounter = RegInit(0.U(XLEN.W))

  /** whether instruction decoded are illegal */
  // Register uop fire/exception/uopBits to cut timing path before trapInstInfo output
  val uopFire_reg   = RegNext(VecInit(out.uop.map(_.fire)))
  val uopExcII_reg  = RegNext(VecInit(out.uop.map(_.bits.exceptionVec(illegalInstr))))
  val uopExcVI_reg  = RegNext(VecInit(out.uop.map(_.bits.exceptionVec(virtualInstr))))
  val uopBits_reg   = RegNext(VecInit(out.uop.map(_.bits)))

  val isIllegalInstVec = VecInit(uopFire_reg.zip(uopExcII_reg).zip(uopExcVI_reg).map {
    case ((fire, excII), excVI) => fire && (excII || excVI)
  })
  val hasIllegalInst = isIllegalInstVec.reduce(_ || _)
  val illegalInst = PriorityMuxDefault(isIllegalInstVec.zip(uopBits_reg), 0.U.asTypeOf(new DecodeOutUop))

  val illegalTrapInstInfo = Wire(chiselTypeOf(out.toCSR.trapInstInfo.bits))
  illegalTrapInstInfo := DontCare
  illegalTrapInstInfo.fromDecodedInst(illegalInst)

  val redirect_reg = RegNext(in.redirect.valid)
  out.toCSR.trapInstInfo.valid := !redirect_reg && hasIllegalInst
  out.toCSR.trapInstInfo.bits  := illegalTrapInstInfo


  for (i <- decodeChannels.in.mops.indices) {
    val inMopBits = in.mop(i).bits

    decodeChannels.in.redirect := in.redirect.valid
    decodeChannels.in.renameCanAccept := out.uop.head.ready
    decodeChannels.in.mops(i).valid := in.mop(i).valid
    decodeChannels.in.mops(i).bits.info match {
      case info =>
        info.rawInst := inMopBits.instr
        info.vtype   := inMopBits.vtype
        info.fromCSR := in.fromCSR
        info.vstart  := in.vstart
    }
    decodeChannels.in.mops(i).bits.ctrl match {
      case ctrl =>
        ctrl.foldpc           := inMopBits.foldpc
        ctrl.exceptionVec     := inMopBits.exceptionVec
        ctrl.satpFlushFirstFetchFault := inMopBits.satpFlushFirstFetchFault
        ctrl.isFetchMalAddr   := inMopBits.isFetchMalAddr
        ctrl.trigger          := inMopBits.trigger
        ctrl.isRVC            := inMopBits.isRVC
        ctrl.predTaken        := inMopBits.predTaken
        ctrl.crossPageIPFFix  := inMopBits.crossPageIPFFix
        ctrl.ftqPtr           := inMopBits.ftqPtr
        ctrl.ftqOffset        := inMopBits.ftqOffset
        ctrl.isLastInFtqEntry := inMopBits.isLastInFtqEntry
        ctrl.vtype            := inMopBits.vtype
        ctrl.oldVType         := inMopBits.specvtype
        ctrl.rawInst          := inMopBits.instr
        ctrl.ZicfilpPCAligned.foreach(_ := inMopBits.ZicfilpPCAligned.get)
        ctrl.debug.foreach(_  := inMopBits.debug.get)
    }
  }

  in.mop.zipWithIndex.foreach {
    case (mop, i) =>
      mop.ready := decodeChannels.in.mops(i).ready
  }

  // LPAD 阅读入口：先看下方 if (HasZicfilp) 的两类编码识别，再读 SpecZicfilp.scala。
  // Spec 接在 DecodeChannels 的 uop 选择/拆分缓冲之后，按实际输出的指令顺序跟踪 ELP。
  // Option.when 使用 Scala 参数决定生成硬件；spec.io.enable 则是硬件运行中的使能信号。
  val specZicfilp = Option.when(HasZicfilp)(Module(new SpecZicfilp))
  specZicfilp.foreach { spec =>
    spec.io.enable := in.fromCSR.enableZicfilp.get
    spec.io.redirect := in.ZicfilpRedirect.get
    for (i <- out.uop.indices) {
      val coUop = decodeChannels.out.uops(i)
      // JALR now has link and jump uops. Advance ELP once per instruction,
      // including when its uops straddle a decoder stall or buffer boundary.
      // i 是本拍并行输出的位置。JALR link(first=1) 参与一次状态更新，jr(first=0) 跳过。
      // coUop.valid 可在 stall 时保持 1；fire 只有下一级接受它时才为 1。
      // redirect 期间这些 uop 将被清除，不能拿错误路径指令继续推进推测 ELP。
      spec.io.valid(i) := coUop.valid && coUop.bits.info.isFirstUop && !in.redirect.valid
      spec.io.fire(i) := spec.io.valid(i) && out.uop(i).ready
      spec.io.isJalr(i) := out.uop(i).bits.ZicfilpInfos.get.ZicfilpJalr
      spec.io.isLPAD(i) := out.uop(i).bits.ZicfilpLPAD.get
    }
  }

  out.uop.zipWithIndex.foreach { case (uop, i) =>
    val coUop = decodeChannels.out.uops(i)
    val mopInfo = coUop.bits.ctrl
    val uopInfo = coUop.bits.info

    uop.valid := coUop.valid
    uop.bits match {
      case bits =>
        bits.foldpc := mopInfo.foldpc
        bits.exceptionVec extendFrom mopInfo.exceptionVec
        bits.exceptionVec(illegalInstr) := mopInfo.exceptionVec(illegalInstr) || uopInfo.exceptionII
        bits.exceptionVec(virtualInstr) := uopInfo.exceptionVI
        bits.exceptionVec(breakPoint)   := TriggerAction.isExp(mopInfo.trigger)
        bits.satpFlushFirstFetchFault := mopInfo.satpFlushFirstFetchFault
        bits.isFetchMalAddr := mopInfo.isFetchMalAddr
        bits.trigger := mopInfo.trigger
        bits.isRVC := mopInfo.isRVC
        bits.predTaken := mopInfo.predTaken
        bits.crossPageIPFFix := mopInfo.crossPageIPFFix
        bits.ftqPtr := mopInfo.ftqPtr
        bits.ftqOffset := mopInfo.ftqOffset
        bits.isLastInFtqEntry := mopInfo.isLastInFtqEntry
        bits.instr := mopInfo.rawInst
        bits.commitType := uopInfo.commitType
        bits.srcType :=
          Seq(uopInfo.src1Ren, uopInfo.src2Ren, uopInfo.src3Ren)
            .zip(Seq(uopInfo.src1Type, uopInfo.src2Type, uopInfo.src3Type))
            .map{ case (ren, typ) => Mux(ren, typ.toSrcType, SrcType.no) }
        bits.v0Ren := uopInfo.v0Ren
        bits.lsrc := Seq(uopInfo.lsrc1, uopInfo.lsrc2, uopInfo.lsrc3)
        bits.ldest := uopInfo.ldest
        bits.fuType := uopInfo.fuType
        bits.fuOpType := uopInfo.opcode
        bits.rfWen := uopInfo.gpWen
        bits.fpWen := uopInfo.fpWen
        bits.vecWen := uopInfo.vpWen
        bits.v0Wen := uopInfo.vpWen && uopInfo.ldest === 0.U
        bits.vlWen := uopInfo.vlWen
        bits.vlRen := uopInfo.vlRen
        bits.waitForward := uopInfo.noSpec
        bits.blockBackward := uopInfo.blockBack
        bits.flushPipe := uopInfo.flushPipe
        bits.canRobCompress := uopInfo.canRobCompress
        // Simple instructions may share a high-density CROB segment. Complex
        // scalar instructions retain their own segment and observation slot.
        bits.simple := uopInfo.canRobCompress && in.fromCSR.custom.high_density_rob_compression_enable &&
          !FuType.isLoadStore(uopInfo.fuType) && !FuType.isBJU(uopInfo.fuType) &&
          !FuType.isAMO(uopInfo.fuType) && !FuType.isFence(uopInfo.fuType) &&
          !FuType.isCsr(uopInfo.fuType) && !FuType.isVset(uopInfo.fuType) &&
          !uopInfo.vpWen && !uopInfo.dirtyVs
        bits.selImm := Mux(uopInfo.selImm.valid, DecodeSelImm.toSelImm(uopInfo.selImm.bits), DecodeSelImm.NO)
        bits.imm := uopInfo.imm
        bits.src12Rev := uopInfo.src12Rev
        bits.frm := uopInfo.frm
        bits.vm := uopInfo.vm
        bits.vtype := mopInfo.vtype
        bits.oldVType := mopInfo.oldVType
        bits.vlsInstr := false.B // Todo: remove
        bits.fflagsWen := uopInfo.fflagsWen
        bits.vxsatWen := uopInfo.vxsatWen
        bits.dirtyVs := uopInfo.dirtyVs
        bits.isMove := uopInfo.isMove
        bits.uopIdx := uopInfo.uopIdx
        bits.uopSplitType := 0.U // Todo: remove
        bits.isVset := uopInfo.isVset
        bits.firstUop := uopInfo.isFirstUop
        bits.lastUop := uopInfo.isLastUop
        bits.isJ := uopInfo.isJ
        bits.isJr := uopInfo.isJr
        bits.numWB := uopInfo.numWb +& 1.U
        if (HasZicfilp) {
          val inst = mopInfo.rawInst
          // JALR 的 rs1 是 inst[19:15]。判断是否期待 LPAD 只看 rs1 的豁免集合，
          // 与 rd 无关：jalr x7,0(x10) 仍要求 LPAD，且会把返回地址写入 x7。
          bits.ZicfilpInfos.get.ZicfilpJalr :=
            inst(6, 0) === "b1100111".U && inst(14, 12) === 0.U &&
            inst(19, 15) =/= 1.U && inst(19, 15) =/= 5.U && inst(19, 15) =/= 7.U
          // LPAD 复用 AUIPC rd=x0 编码，inst[31:12] 为标签；此处只检查编码和 PC 对齐。
          // ZicfilpPCAligned 来自原始实际 PC，不依赖 debug.pc，也不是指令位的对齐。
          // 当前 PseudoDecodeChannel 没有 LPAD 表项：它先作为 AUIPC 经 Simple 通道译码。
          bits.ZicfilpLPAD.get :=
            inst(6, 0) === "b0010111".U && inst(11, 7) === 0.U && mopInfo.ZicfilpPCAligned.get
          val spec = specZicfilp.get
          // 这两条连线不构成组合环：Spec 仅用上面的指令分类计算，不依赖下面覆盖的 FU/源。
          // LPADValid 还要求 enable=1、进入当前指令前的推测 ELP=1，之后随 uop 保存。
          bits.ZicfilpInfos.get.ZicfilpLPADValid := spec.io.lpadValid(i)
          // 缺少/不对齐 LPAD 的错误在 Decode 标记；非零标签不匹配由 JumpUnit 标记。
          bits.exceptionVec(softwareCheck) := spec.io.exception(i)
          when(spec.io.lpadValid(i)) {
            // AUIPC moved to LinkUnit, which has no register operand. Execute
            // active LPADs on JumpUnit so the label can be checked against x7.
            // 这里给 Wire/输出字段覆盖赋值，生成的是“基础 AUIPC 控制/LPAD 控制”选择器，
            // 不会先执行 AUIPC 再执行 LPAD，也不会为了覆盖多插入一拍。
            // LinkUnit 没有 GPR 源，选择 JumpUnit 才能走现有的寄存器读与标签比较路径。
            bits.fuType := FuType.jmp.U
            bits.fuOpType := JumpOpType.j
            // 完成回写仍保留，仅关闭架构寄存器写；isJ/isJr 保持原 AUIPC 的非跳转分类。
            bits.rfWen := false.B
            // LPAD 的隐含源是 x7。这里填的是编号 7，实际值在 Rename/Issue/Datapath 后取得。
            // 当前即便标签为 0 也建立这个依赖，因此 lpad 0 仍可能等待 x7 的生产者。
            bits.lsrc(0) := 7.U
            bits.srcType(0) := SrcType.reg
          }
        }
        // 使用覆盖后的 FU/opcode，保证有效 LPAD 的延迟信息与 JumpUnit 一致。
        bits.latency := LatDecoder(bits.fuType, bits.fuOpType)
        bits.debug.foreach{ x =>
          x.pc := mopInfo.debug.get.pc
          x.debug_seqNum.seqNum := mopInfo.debug.get.debug_seqNum.seqNum
          x.debug_seqNum.uopIdx := uopInfo.uopIdx
        }
    }
  }

  for (i <- out.uop.indices) {
    // 对有效 LPAD，最终 lsrc(0)=7，因此这里查询 x7 的物理映射，不是读取 x7 数据。
    // 下游阻塞时 hold=1，与 Spec 的 fire=0 配合，保持这一组的映射查询/状态进度。
    out.intRat(i)(0).addr := out.uop(i).bits.lsrc(0)
    out.intRat(i)(1).addr := out.uop(i).bits.lsrc(1)
    out.intRat(i).foreach(_.hold := !out.uop(i).ready)

    out.fpRat(i)(0).addr := out.uop(i).bits.lsrc(0)
    out.fpRat(i)(1).addr := out.uop(i).bits.lsrc(1)
    out.fpRat(i)(2).addr := out.uop(i).bits.lsrc(2)
    out.fpRat(i).foreach(_.hold := !out.uop(i).ready)

    out.vecRat(i)(0).addr := out.uop(i).bits.lsrc(0) // vs1
    out.vecRat(i)(1).addr := out.uop(i).bits.lsrc(1) // vs2
    out.vecRat(i)(2).addr := out.uop(i).bits.lsrc(2) // old_vd
    out.vecRat(i).foreach(_.hold := !out.uop(i).ready)

    out.vlRat(i).addr := Vl_IDX.U
    out.vlRat(i).hold := !out.uop(i).ready
  }

  out.toFrontend.canAccept := !in.redirect.valid && out.uop.head.ready

  stallReason.out.reason := stallReason.in.reason

  val perfEvents = Seq()
  generatePerfEvent()
}

object DecodeStage {
  class In(implicit p: Parameters) extends XSBundle {
    val redirect = Input(ValidIO(new Redirect))
    val ZicfilpRedirect = Option.when(HasZicfilp)(Input(ValidIO(Bool())))

    // The ready of mop means this mop is accepted by DecodeStage
    // Ready signal depends on valid
    val mop = Vec(DecodeWidth, Flipped(DecoupledIO(new DecodeInUop)))
    // from FusionDecoder
    val fusion = Vec(DecodeWidth - 1, Input(Bool()))

    // from CSR
    val fromCSR = Input(new CSRToDecode)
    val vstart = Input(Vstart())
  }

  class Out(implicit p: Parameters) extends XSBundle {
    // params alias
    private val numIntRegSrc = backendParams.numIntRegSrc
    private val numIntRatPorts = numIntRegSrc
    private val numFpRegSrc = backendParams.numFpRegSrc
    private val numFpRatPorts = numFpRegSrc
    private val numVecRegSrc = backendParams.numVecRegSrc
    private val numVecRatPorts = numVecRegSrc

    // to Rename
    val uop = Vec(DecodeWidth, DecoupledIO(new DecodeOutUop))

    // to RAT
    val intRat = Vec(RenameWidth, Vec(numIntRatPorts, Flipped(new RatReadPort(log2Ceil(IntLogicRegs)))))
    val fpRat = Vec(RenameWidth, Vec(numFpRatPorts, Flipped(new RatReadPort(log2Ceil(FpLogicRegs)))))
    val vecRat = Vec(RenameWidth, Vec(numVecRatPorts, Flipped(new RatReadPort(log2Ceil(VecLogicRegs)))))
    val vlRat = Vec(RenameWidth, Flipped(new RatReadPort(log2Ceil(VlLogicRegs))))

    val toFrontend = Output(new Bundle {
      val canAccept = Bool()
    })

    val toCSR = new Bundle {
      val trapInstInfo = ValidIO(new TrapInstInfo)
    }
  }
}
