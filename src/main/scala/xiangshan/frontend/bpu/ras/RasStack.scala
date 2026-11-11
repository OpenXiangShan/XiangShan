// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.bpu.ras

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.HasCircularQueuePtrHelper
import utility.XSPerfAccumulate
import xiangshan.frontend.GuardedPc
import xiangshan.frontend.GuardedPcInit
import xiangshan.frontend.bpu.HalfAlignHelper

class RasStack(implicit p: Parameters) extends RasModule
    with HasCircularQueuePtrHelper
    with HalfAlignHelper
    with Helpers {
  class RasStackIO extends Bundle {
    class RasSpecIO extends Bundle {
      val fire:      Bool      = Input(Bool())
      val pushValid: Bool      = Input(Bool())
      val popValid:  Bool      = Input(Bool())
      val pushAddr:  GuardedPc = Input(GuardedPc())
      val popAddr:   GuardedPc = Output(GuardedPc())
    }

    class RasCommitIO extends Bundle {
      val valid:     Bool      = Input(Bool())
      val pushValid: Bool      = Input(Bool())
      val popValid:  Bool      = Input(Bool())
      val pushAddr:  GuardedPc = Input(GuardedPc())
      val metaTosw:  RasPtr    = Input(new RasPtr)
      // for debug purpose only
      val metaSsp: UInt = Input(UInt(StackPtrWidth.W))
    }

    class RasRedirectIO extends Bundle {
      val valid:    Bool            = Input(Bool())
      val isCall:   Bool            = Input(Bool())
      val callAddr: GuardedPc       = Input(GuardedPc())
      val isRet:    Bool            = Input(Bool())
      val meta:     RasInternalMeta = Input(new RasInternalMeta)
    }

    val spec:     RasSpecIO              = new RasSpecIO
    val s2SpecIn: Valid[RasPipeLineInfo] = Flipped(Valid(new RasPipeLineInfo))
    val s3SpecIn: Valid[RasPipeLineInfo] = Flipped(Valid(new RasPipeLineInfo))
    val commit:   RasCommitIO            = new RasCommitIO
    val redirect: RasRedirectIO          = new RasRedirectIO
    val specRead: ReadRetAddr            = new ReadRetAddr
    // Whether the speculative top (`spec.popAddr`) can be used to predict a return.
    val topRetAddrValid: Bool            = Output(Bool())
    val meta:            RasInternalMeta = Output(new RasInternalMeta)

    val debug: RasDebug = new RasDebug
  }
  val io: RasStackIO = IO(new RasStackIO)
  class NosEntry(implicit p: Parameters) extends RasBundle {
    val nos:    RasPtr = new RasPtr
    val inSpec: Bool   = Bool()
  }
  class RestoreEntry(implicit p: Parameters) extends RasBundle {
    val magicNum:     UInt      = UInt(log2Ceil(RestoreDepth).W)
    val nosEntry:     NosEntry  = new NosEntry
    val nextNosEntry: NosEntry  = new NosEntry
    val topAddr:      GuardedPc = GuardedPc()
    val nextTopAddr:  GuardedPc = GuardedPc()
  }

  private val commitStack =
    RegInit(VecInit(Seq.fill(CommitStackSize)(RasEntry(GuardedPcInit(0.U(GuardedVAddrBits.W))))))
  private val specQueue =
    RegInit(VecInit(Seq.fill(SpecQueueSize)(RasEntry(GuardedPcInit(0.U(GuardedVAddrBits.W))))))
  private val specNosList = RegInit(VecInit(Seq.fill(SpecQueueSize)(0.U.asTypeOf(new NosEntry))))

  // Wider than log2Up(CommitStackSize) so that `ssp - nsp` (the net in-flight push/pop
  // effect) can be sign-interpreted without a modular wrap ambiguity.
  private val nsp = RegInit(0.U(StackPtrWidth.W))
  private val ssp = RegInit(0.U(StackPtrWidth.W))

  // Number of valid entries in the committed stack (0..CommitStackSize).
  private val commitDepth = RegInit(0.U(CommitDepthWidth.W))

  private val tosr       = RegInit(RasPtr(true.B, (SpecQueueSize - 1).U))
  private val tosw       = RegInit(RasPtr(false.B, 0.U))
  private val bos        = RegInit(RasPtr(false.B, 0.U))
  private val nos        = RegInit(RasPtr(false.B, 0.U))
  private val tosrInSpec = RegInit(false.B)
  private val nosInSpec  = RegInit(false.B)

  private val magicNum = RegInit(0.U(log2Ceil(RestoreDepth).W))

  private val s2_rasMeta = RegInit(0.U.asTypeOf(new RasInternalMeta))
  private val s3_rasMeta = RegInit(0.U.asTypeOf(new RasInternalMeta))
  private val timingTop  = RegInit(0.U.asTypeOf(new RasEntry))
  when(io.spec.fire) {
    s2_rasMeta.tosw       := tosw
    s2_rasMeta.tosr       := tosr
    s2_rasMeta.nos        := nos
    s2_rasMeta.tosrInSpec := tosrInSpec
    s2_rasMeta.nosInSpec  := nosInSpec
    s2_rasMeta.ssp        := ssp
  }
  when(io.s2SpecIn.valid) {
    s3_rasMeta := s2_rasMeta
  }

  // 保存push之前一笔的nos指针数据和栈顶项数据
  private val s2_restoreEntry = RegInit(0.U.asTypeOf(Valid(new RestoreEntry)))
  private val s3_restoreEntry = RegInit(0.U.asTypeOf(Valid(new RestoreEntry)))

  def tosrInRange(currTosr: RasPtr, currTosw: RasPtr, currInSpec: Bool): Bool = {
    val inflightValid = WireInit(false.B)
    // if in range, tosr should be no younger than bos and strictly younger than tosw
    when(!isBefore(currTosr, bos) && isBefore(currTosr, currTosw) && currInSpec) {
      inflightValid := true.B
    }
    inflightValid
  }

  def commitStackIdx(ptr: UInt): UInt = ptr(CommitStackAddrWidth - 1, 0)

  def getCommitTop(currSsp: UInt): RasEntry = commitStack(commitStackIdx(currSsp))

  def getNextNos(
      currMagicNum:   UInt,
      currNos:        RasPtr,
      currTosr:       RasPtr,
      currTosw:       RasPtr,
      currTosrInSpec: Bool,
      currNosInSpec:  Bool,
      allowBypass:    Boolean
  ): NosEntry = {
    val ret = Wire(new NosEntry)
    if (allowBypass) {
      val hitVec          = Wire(Vec(2, Bool()))
      val restoreEntryVec = Wire(Vec(2, new RestoreEntry))
      restoreEntryVec(0) := s2_restoreEntry.bits
      restoreEntryVec(1) := s3_restoreEntry.bits
      hitVec(0)          := s2_restoreEntry.valid && (currMagicNum === s2_restoreEntry.bits.magicNum)
      hitVec(1)          := s3_restoreEntry.valid && (currMagicNum === s3_restoreEntry.bits.magicNum)
      val hit = hitVec.reduce(_ || _)
      when(hit) {
        val restore = Wire(new RestoreEntry)
        restore := Mux1H(hitVec, restoreEntryVec)
        ret     := restore.nosEntry
      }.elsewhen(tosrInRange(currTosr, currTosw, currTosrInSpec) && tosrInRange(currNos, currTosw, currNosInSpec)) {
        ret := specNosList(currNos.value)
      }.otherwise {
        ret := 0.U.asTypeOf(new NosEntry)
      }
    } else {
      when(tosrInRange(currTosr, currTosw, currTosrInSpec) && tosrInRange(currNos, currTosw, currNosInSpec)) {
        ret := specNosList(currNos.value)
      }.otherwise {
        ret := 0.U.asTypeOf(new NosEntry)
      }
    }
    ret
  }

  def getTop(
      currMagicNum: UInt,
      currSsp:      UInt,
      currTosr:     RasPtr,
      currTosw:     RasPtr,
      currInSpec:   Bool,
      allowBypass:  Boolean
  ): RasEntry = {
    val ret = Wire(new RasEntry)
    if (allowBypass) {
      val hitVec          = Wire(Vec(2, Bool()))
      val restoreEntryVec = Wire(Vec(2, new RestoreEntry))
      restoreEntryVec(0) := s2_restoreEntry.bits
      restoreEntryVec(1) := s3_restoreEntry.bits
      hitVec(0)          := s2_restoreEntry.valid && (currMagicNum === s2_restoreEntry.bits.magicNum)
      hitVec(1)          := s3_restoreEntry.valid && (currMagicNum === s3_restoreEntry.bits.magicNum)
      val hit = hitVec.reduce(_ || _)
      when(hit) {
        val restore = Wire(new RestoreEntry)
        restore     := Mux1H(hitVec, restoreEntryVec)
        ret.retAddr := restore.topAddr
      }.elsewhen(tosrInRange(currTosr, currTosw, currInSpec)) {
        ret := specQueue(currTosr.value)
      }.otherwise {
        ret := getCommitTop(currSsp)
      }
    } else {
      when(tosrInRange(currTosr, currTosw, currInSpec)) {
        ret := specQueue(currTosr.value)
      }.otherwise {
        ret := getCommitTop(currSsp)
      }
    }
    ret
  }

  def specPush(currSsp: UInt, currTosr: RasPtr, currInSpec: Bool, currTosw: RasPtr): Unit = {
    nos        := currTosr
    tosr       := currTosw
    tosw       := specPtrInc(currTosw)
    nosInSpec  := currInSpec
    tosrInSpec := true.B
    // spec sp should always be maintained
    ssp := ptrInc(currSsp)
  }

  def specPop(
      currSsp:        UInt,
      currTosr:       RasPtr,
      currTosw:       RasPtr,
      currTopNos:     RasPtr,
      currNosInSpec:  Bool,
      currTosrInSpec: Bool,
      nextNos:        RasPtr,
      nextNosInSpec:  Bool
  ): Unit = {
    // tosr is only maintained when spec queue is not empty
    when(tosrInRange(currTosr, currTosw, currTosrInSpec)) {
      tosr       := currTopNos
      tosrInSpec := currNosInSpec
      nos        := nextNos
      nosInSpec  := nextNosInSpec
    }
    // spec sp should always be maintained
    ssp := ptrDec(currSsp)
  }

  private val redirectNextNosEntry =
    getNextNos(
      currMagicNum = 0.U,
      io.redirect.meta.nos,
      io.redirect.meta.tosr,
      io.redirect.meta.tosw,
      io.redirect.meta.tosrInSpec,
      io.redirect.meta.nosInSpec,
      allowBypass = false
    )

  private val nextTopAddr  = getTop(magicNum - 1.U, ptrDec(ssp), nos, tosw, nosInSpec, allowBypass = true)
  private val nextNosEntry = getNextNos(magicNum - 1.U, nos, tosr, tosw, tosrInSpec, nosInSpec, allowBypass = true)
  private val s2_realPushAddr =
    getCfiPcFromPosition(GuardedPcInit(io.s2SpecIn.bits.startPc), io.s2SpecIn.bits.cfiPosition) + 2.U
  private val s3_realPushAddr =
    getCfiPcFromPosition(GuardedPcInit(io.s3SpecIn.bits.startPc), io.s3SpecIn.bits.cfiPosition) + 2.U

  // 指针更新
  when(io.redirect.valid) {
    tosr       := io.redirect.meta.tosr
    tosw       := io.redirect.meta.tosw
    nos        := io.redirect.meta.nos
    ssp        := io.redirect.meta.ssp
    tosrInSpec := io.redirect.meta.tosrInSpec
    nosInSpec  := io.redirect.meta.nosInSpec

    when(io.redirect.isCall) {
      specPush(io.redirect.meta.ssp, io.redirect.meta.tosr, io.redirect.meta.tosrInSpec, io.redirect.meta.tosw)
    }
    when(io.redirect.isRet) {
      specPop(
        io.redirect.meta.ssp,
        io.redirect.meta.tosr,
        io.redirect.meta.tosw,
        io.redirect.meta.nos,
        io.redirect.meta.nosInSpec,
        io.redirect.meta.tosrInSpec,
        redirectNextNosEntry.nos,
        redirectNextNosEntry.inSpec
      )
    }
  }.elsewhen(io.s3SpecIn.valid && io.s3SpecIn.bits.hasOverride) {
    tosr       := s3_rasMeta.tosr
    tosw       := s3_rasMeta.tosw
    nos        := s3_rasMeta.nos
    ssp        := s3_rasMeta.ssp
    tosrInSpec := s3_rasMeta.tosrInSpec
    nosInSpec  := s3_rasMeta.nosInSpec

    when(io.s3SpecIn.bits.attribute.isCall) {
      specPush(s3_rasMeta.ssp, s3_rasMeta.tosr, s3_rasMeta.tosrInSpec, s3_rasMeta.tosw)
    }
    when(io.s3SpecIn.bits.attribute.isReturn) {
      specPop(
        s3_rasMeta.ssp,
        s3_rasMeta.tosr,
        s3_rasMeta.tosw,
        s3_rasMeta.nos,
        s3_rasMeta.nosInSpec,
        s3_rasMeta.tosrInSpec,
        s3_restoreEntry.bits.nextNosEntry.nos,
        s3_restoreEntry.bits.nextNosEntry.inSpec
      )
    }
  }.elsewhen(io.s2SpecIn.valid && io.s2SpecIn.bits.hasOverride) {
    tosr       := s2_rasMeta.tosr
    tosw       := s2_rasMeta.tosw
    nos        := s2_rasMeta.nos
    ssp        := s2_rasMeta.ssp
    tosrInSpec := s2_rasMeta.tosrInSpec
    nosInSpec  := s2_rasMeta.nosInSpec

    when(io.s2SpecIn.bits.attribute.isCall) {
      specPush(s2_rasMeta.ssp, s2_rasMeta.tosr, s2_rasMeta.tosrInSpec, s2_rasMeta.tosw)
    }
    when(io.s2SpecIn.bits.attribute.isReturn) {
      specPop(
        s2_rasMeta.ssp,
        s2_rasMeta.tosr,
        s2_rasMeta.tosw,
        s2_rasMeta.nos,
        s2_rasMeta.nosInSpec,
        s2_rasMeta.tosrInSpec,
        s2_restoreEntry.bits.nextNosEntry.nos,
        s2_restoreEntry.bits.nextNosEntry.inSpec
      )
    }
  }.elsewhen(io.spec.fire) {
    when(io.spec.pushValid) {
      specPush(ssp, tosr, tosrInSpec, tosw)
    }
    when(io.spec.popValid) {
      specPop(ssp, tosr, tosw, nos, nosInSpec, tosrInSpec, nextNosEntry.nos, nextNosEntry.inSpec)
    }
  }

  // 地址更新
  when(io.redirect.valid) {
    when(io.redirect.isCall) {
      timingTop.retAddr := io.redirect.callAddr
    }.elsewhen(io.redirect.isRet) {
      // getTop using redirect Nos as tosr
      val nextReadSsp        = Wire(UInt(StackPtrWidth.W))
      val nextReadTosr       = io.redirect.meta.nos
      val nextReadTosw       = io.redirect.meta.tosw
      val nextReadTosrInSpec = io.redirect.meta.nosInSpec
      nextReadSsp := ptrDec(io.redirect.meta.ssp)
      timingTop := getTop(
        currMagicNum = 0.U, // DontCare
        nextReadSsp,
        nextReadTosr,
        nextReadTosw,
        nextReadTosrInSpec,
        allowBypass = false
      )
    }.otherwise {
      // Neither call nor ret
      val nextReadSsp        = io.redirect.meta.ssp
      val nextReadTosr       = io.redirect.meta.tosr
      val nextReadTosw       = io.redirect.meta.tosw
      val nextReadTosrInSpec = io.redirect.meta.tosrInSpec
      timingTop := getTop(
        currMagicNum = 0.U, // DontCare
        nextReadSsp,
        nextReadTosr,
        nextReadTosw,
        nextReadTosrInSpec,
        allowBypass = false
      )
    }
  }.elsewhen(io.s3SpecIn.valid && io.s3SpecIn.bits.hasOverride) {
    when(io.s3SpecIn.bits.attribute.isCall) {
      timingTop.retAddr := getCfiPcFromPosition(
        GuardedPcInit(io.s3SpecIn.bits.startPc),
        io.s3SpecIn.bits.cfiPosition
      ) + 2.U
    }.elsewhen(io.s3SpecIn.bits.attribute.isReturn) {
      timingTop.retAddr := s3_restoreEntry.bits.nextTopAddr
    }.otherwise {
      timingTop.retAddr := s3_restoreEntry.bits.topAddr
    }
  }.elsewhen(io.s2SpecIn.valid && io.s2SpecIn.bits.hasOverride) {
    when(io.s2SpecIn.bits.attribute.isCall) {
      timingTop.retAddr := getCfiPcFromPosition(
        GuardedPcInit(io.s2SpecIn.bits.startPc),
        io.s2SpecIn.bits.cfiPosition
      ) + 2.U
    }.elsewhen(io.s2SpecIn.bits.attribute.isReturn) {
      timingTop.retAddr := s2_restoreEntry.bits.nextTopAddr
    }.otherwise {
      timingTop.retAddr := s2_restoreEntry.bits.topAddr
    }
  }.elsewhen(io.spec.fire) {
    when(io.spec.pushValid) {
      timingTop.retAddr := io.spec.pushAddr
    }.elsewhen(io.spec.popValid) {
      val nextReadSsp        = ptrDec(ssp)
      val nextReadTosr       = nos
      val nextReadTosw       = tosw
      val nextReadTosrInSpec = nosInSpec
      timingTop := getTop(
        currMagicNum = magicNum - 1.U,
        nextReadSsp,
        nextReadTosr,
        nextReadTosw,
        nextReadTosrInSpec,
        allowBypass = true
      )
    }.otherwise {
      val nextReadSsp        = ssp
      val nextReadTosr       = tosr
      val nextReadTosw       = tosw
      val nextReadTosrInSpec = tosrInSpec
      // timingTop := timingTop
      // 添加一个测试比较，当RAS不为空的时候。
    }
  }

  when(io.s2SpecIn.valid) {
    s3_restoreEntry := s2_restoreEntry
  }

  // 备份的更新操作
  when(io.redirect.valid) {
    magicNum              := 0.U
    s2_restoreEntry.valid := false.B
    s3_restoreEntry.valid := false.B
  }.elsewhen(io.s3SpecIn.valid && io.s3SpecIn.bits.hasOverride) {
    when(io.s3SpecIn.bits.attribute.isCall) {
      magicNum              := s3_restoreEntry.bits.magicNum + 1.U
      s2_restoreEntry.valid := false.B
      s3_restoreEntry.valid := false.B
    }.elsewhen(io.s3SpecIn.bits.attribute.isReturn) {
      magicNum              := s3_restoreEntry.bits.magicNum - 1.U
      s2_restoreEntry.valid := false.B
      s3_restoreEntry.valid := false.B
    }.otherwise {
      magicNum              := s3_restoreEntry.bits.magicNum
      s2_restoreEntry.valid := false.B
      s3_restoreEntry.valid := false.B
    }
  }.elsewhen(io.s2SpecIn.valid && io.s2SpecIn.bits.hasOverride) {
    when(io.s2SpecIn.bits.attribute.isCall) {
      magicNum              := s2_restoreEntry.bits.magicNum + 1.U
      s2_restoreEntry.valid := false.B
      s3_restoreEntry       := s2_restoreEntry
      s3_restoreEntry.valid := true.B
    }.elsewhen(io.s2SpecIn.bits.attribute.isReturn) {
      magicNum              := s2_restoreEntry.bits.magicNum - 1.U
      s2_restoreEntry.valid := false.B
      s3_restoreEntry       := s2_restoreEntry
      s3_restoreEntry.valid := false.B
    }.otherwise {
      magicNum              := s2_restoreEntry.bits.magicNum
      s2_restoreEntry.valid := false.B
      s3_restoreEntry       := s2_restoreEntry
      s3_restoreEntry.valid := false.B
    }
  }.elsewhen(io.spec.fire) {
    s2_restoreEntry.bits.topAddr         := timingTop.retAddr
    s2_restoreEntry.bits.nextTopAddr     := nextTopAddr.retAddr
    s2_restoreEntry.bits.nosEntry.nos    := nos
    s2_restoreEntry.bits.nosEntry.inSpec := nosInSpec
    s2_restoreEntry.bits.nextNosEntry    := nextNosEntry
    s2_restoreEntry.bits.magicNum        := magicNum
    when(io.spec.pushValid) {
      s2_restoreEntry.valid := true.B
      magicNum              := magicNum + 1.U
    }.elsewhen(io.spec.popValid) {
      s2_restoreEntry.valid := false.B
      magicNum              := magicNum - 1.U
    }.otherwise {
      s2_restoreEntry.valid := false.B
    }
  }

  private val writeEntry    = Wire(new RasEntry)
  private val writeNosEntry = Wire(new NosEntry)
  writeEntry.retAddr := Mux(io.redirect.valid && io.redirect.isCall, io.redirect.callAddr, s3_realPushAddr)

  writeNosEntry.nos := Mux(io.redirect.valid && io.redirect.isCall, io.redirect.meta.tosr, s3_rasMeta.tosr)
  writeNosEntry.inSpec := Mux(
    io.redirect.valid && io.redirect.isCall,
    tosrInRange(io.redirect.meta.tosr, io.redirect.meta.tosw, io.redirect.meta.tosrInSpec),
    tosrInRange(s3_rasMeta.tosr, s3_rasMeta.tosw, s3_rasMeta.tosrInSpec)
  )

  private val realWriteEntry = RegNext(writeEntry)
  private val realNosEntry   = RegNext(writeNosEntry)

  private val realWriteAddr = RegEnable(
    Mux(io.redirect.valid && io.redirect.isCall, io.redirect.meta.tosw, s3_rasMeta.tosw),
    io.s3SpecIn.valid || (io.redirect.valid && io.redirect.isCall)
  )
  private val realPush =
    RegNext(io.s3SpecIn.valid && io.s3SpecIn.bits.attribute.isCall && !io.redirect.valid, init = false.B) ||
      RegNext(io.redirect.valid && io.redirect.isCall, init = false.B)
  when(realPush) {
    specQueue(realWriteAddr.value)   := realWriteEntry
    specNosList(realWriteAddr.value) := realNosEntry
  }

  private val specQueueRetAddr  = specQueue(io.specRead.req.tosr.value).retAddr
  private val specCommitRetAddr = commitStack(commitStackIdx(io.specRead.req.ssp)).retAddr
  private val isInQueue         = tosrInRange(io.specRead.req.tosr, tosw, io.specRead.req.tosrInSpec)
  private val specReadRetAddr   = Mux(isInQueue, specQueueRetAddr, specCommitRetAddr)
  io.spec.popAddr := timingTop.retAddr
  // One register per output port, duplicated on purpose for timing / placement.
  io.specRead.retAddr.foreach(_ := RegNext(specReadRetAddr, init = 0.U.asTypeOf(specReadRetAddr)))

  io.meta := s3_rasMeta

  // RAS is empty iff TOSR is out of range AND currSsp has not advanced commitDepth steps to reach nsp,
  // where circular pointer wrap is handled by comparing currSsp +& commitDepth against nsp (or 2^w + nsp).
  private def resolvedEmpty(
      currSsp:    UInt,
      currTosr:   RasPtr,
      currTosw:   RasPtr,
      currInSpec: Bool
  ): Bool = {
    val notInCommitStack = WireInit(false.B)
    when(nsp >= currSsp) {
      notInCommitStack := (currSsp +& commitDepth) <= nsp
    }.otherwise {
      notInCommitStack := (currSsp +& commitDepth) <= Cat(1.U(1.W), nsp)
    }
    !tosrInRange(currTosr, currTosw, currInSpec) && notInCommitStack
  }

  // `resolvedEmpty` is exact, but this flag is registered for timing and is only forced non-empty
  // on push / redirect-call. A valid top is therefore never reported empty, while a pop or ret
  // that empties the stack is noticed a cycle late.
  private val rasIsEmpty = RegInit(true.B)
  when(io.redirect.valid) {
    rasIsEmpty := Mux(
      io.redirect.isCall,
      false.B,
      // 会导致空判非空？其实比较无所谓，因为mbtb不太准，不要非空，判空就行
      resolvedEmpty(io.redirect.meta.ssp, io.redirect.meta.tosr, io.redirect.meta.tosw, io.redirect.meta.tosrInSpec)
    )
  }.elsewhen(io.s3SpecIn.valid && io.s3SpecIn.bits.hasOverride) {
    rasIsEmpty := Mux(
      io.s3SpecIn.bits.attribute.isCall,
      false.B,
      resolvedEmpty(s3_rasMeta.ssp, s3_rasMeta.tosr, s3_rasMeta.tosw, s3_rasMeta.tosrInSpec)
    )
  }.elsewhen(io.s2SpecIn.valid && io.s2SpecIn.bits.hasOverride) {
    rasIsEmpty := Mux(
      io.s2SpecIn.bits.attribute.isCall,
      false.B,
      resolvedEmpty(s2_rasMeta.ssp, s2_rasMeta.tosr, s2_rasMeta.tosw, s2_rasMeta.tosrInSpec)
    )
  }.otherwise {
    rasIsEmpty := Mux(
      io.spec.pushValid,
      false.B,
      resolvedEmpty(ssp, tosr, tosw, tosrInSpec)
    )
  }

  io.topRetAddrValid := !rasIsEmpty

  when(io.commit.popValid) {
    val nspUpdate = Wire(UInt(StackPtrWidth.W))
    when(io.commit.metaSsp =/= nsp) {
      // force set nsp to commit ssp to avoid permanent errors
      nspUpdate := io.commit.metaSsp
    }.otherwise {
      nspUpdate := nsp
    }

    nsp         := ptrDec(nspUpdate)
    commitDepth := Mux(commitDepth === 0.U, 0.U, commitDepth - 1.U)
  }

  private val commitPushAddr = specQueue(io.commit.metaTosw.value).retAddr

  when(io.commit.pushValid) {
    val nspUpdate = Wire(UInt(StackPtrWidth.W))
    when(io.commit.metaSsp =/= nsp) {
      // force set nsp to commit ssp to avoid permanent errors
      nspUpdate := io.commit.metaSsp
    }.otherwise {
      nspUpdate := nsp
    }

    nsp         := ptrInc(nspUpdate)
    commitDepth := Mux(commitDepth === CommitStackSize.U, commitDepth, commitDepth + 1.U)
    commitStack(commitStackIdx(ptrInc(nspUpdate))).retAddr := commitPushAddr
  }

  when(io.commit.pushValid) {
    bos                                          := io.commit.metaTosw
    specNosList(io.commit.metaTosw.value).inSpec := false.B
  }.elsewhen(io.commit.valid && (distanceBetween(io.commit.metaTosw, bos) > 2.U)) {
    bos := specPtrDec(io.commit.metaTosw)
  }

  io.debug.bos := bos
  io.debug.commitStack.zipWithIndex.foreach { case (a, i) => a := commitStack(i) }
  io.debug.specNos.zipWithIndex.foreach { case (a, i) => a := specNosList(i).nos }
  io.debug.specQueue.zipWithIndex.foreach { case (a, i) => a := specQueue(i) }

  // Unexpected: ssp and nsp advance on the same push/pop events.
  private val sspDiffNsp = (io.commit.popValid || io.commit.pushValid) && (io.commit.metaSsp =/= nsp)
  XSPerfAccumulate("ras_ssp_mismatch_with_nsp", sspDiffNsp)

}
