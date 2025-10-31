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

package xiangshan.frontend.ibuffer

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.CircularQueuePtr
import utility.InstSeqNum
import utils.EnumUInt
import xiangshan.CtrlFlow
import xiangshan.ExceptionNO
import xiangshan.ExceptionVec
import xiangshan.TriggerAction
import xiangshan.XSCoreParamsKey
import xiangshan.frontend.ExceptionType
import xiangshan.frontend.FetchToIBuffer
import xiangshan.frontend.PreDecodeInfo
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.ftq.FtqPtr

// FIXME: these ptrs have ambiguous names
// FIXME: if these ptrs are never used outside ibuffer, we can move them to class IBuffer as private inner classes
class IBufPtr(implicit p: Parameters) extends CircularQueuePtr[IBufPtr](p =>
      p(XSCoreParamsKey).frontendParameters.ibufferParameters.Size
    ) {}

class IBufInBankPtr(implicit p: Parameters) extends CircularQueuePtr[IBufInBankPtr](p =>
      p(XSCoreParamsKey).frontendParameters.ibufferParameters.Size /
        p(XSCoreParamsKey).frontendParameters.ibufferParameters.NumReadBank
    ) {}

class IBufBankPtr(implicit p: Parameters) extends CircularQueuePtr[IBufBankPtr](p =>
      p(XSCoreParamsKey).frontendParameters.ibufferParameters.NumReadBank
    ) {}

class IBufEntry(implicit p: Parameters) extends IBufferBundle {
  val inst:             UInt          = UInt(32.W)
  val pc:               PrunedAddr    = PrunedAddr(VAddrBits)
  val foldpc:           UInt          = UInt(MemPredPCWidth.W)
  val pd:               PreDecodeInfo = new PreDecodeInfo
  val predTaken:        Bool          = Bool()
  val fixedTaken:       Bool          = Bool()
  val ftqPtr:           FtqPtr        = new FtqPtr
  val instrEndOffset:   UInt          = UInt(FetchBlockInstOffsetWidth.W)
  val triggered:        UInt          = TriggerAction()
  val isLastInFtqEntry: Bool          = Bool()

  val debug_seqNum: InstSeqNum = InstSeqNum()

  def fromFetch(fetch: FetchToIBuffer, i: Int): IBufEntry = {
    inst             := fetch.instrs(i)
    pc               := fetch.pc(i)
    foldpc           := fetch.foldpc(i)
    pd               := fetch.pd(i)
    predTaken        := fetch.instrEndOffset(i).predTaken
    fixedTaken       := fetch.instrEndOffset(i).fixedTaken
    ftqPtr           := fetch.ftqPtr(i)
    instrEndOffset   := fetch.instrEndOffset(i).offset
    triggered        := fetch.triggered(i)
    isLastInFtqEntry := fetch.isLastInFtqEntry(i)
    debug_seqNum     := fetch.debug_seqNum(i)
    this
  }

  def toIBufOutEntry(
      exception: IBufExceptionEntry
  ): IBufOutEntry = {
    val result = Wire(new IBufOutEntry)
    result.inst               := inst
    result.pc                 := pc
    result.foldpc             := foldpc
    result.pd                 := pd
    result.predTaken          := predTaken
    result.fixedTaken         := fixedTaken
    result.ftqPtr             := ftqPtr
    result.exceptionType      := exception.exceptionType
    result.exceptionCrossPage := exception.exceptionCrossPage
    result.isBackendException := exception.isBackendException
    result.triggered          := triggered
    result.isLastInFtqEntry   := isLastInFtqEntry
    result.debug_seqNum       := debug_seqNum
    result.instrEndOffset     := instrEndOffset
    result
  }
}

class IBufExceptionEntry(implicit p: Parameters) extends IBufferBundle {
  val exceptionType:      ExceptionType = new ExceptionType
  val exceptionCrossPage: Bool          = Bool()
  val isBackendException: Bool          = Bool()

  def fromFetch(fetch: FetchToIBuffer): IBufExceptionEntry = {
    exceptionType      := fetch.exceptionType
    exceptionCrossPage := fetch.exceptionCrossPage
    isBackendException := fetch.isBackendException
    this
  }
}

// The definition of IBufOutEntry is currently retained.
// In the future, the backend will perform certain computations
// in the IBuffer, which will be differentiated from IBufEntry.
class IBufOutEntry(implicit p: Parameters) extends IBufferBundle {
  val inst:               UInt          = UInt(32.W)
  val pc:                 PrunedAddr    = PrunedAddr(VAddrBits)
  val foldpc:             UInt          = UInt(MemPredPCWidth.W)
  val pd:                 PreDecodeInfo = new PreDecodeInfo
  val predTaken:          Bool          = Bool()
  val fixedTaken:         Bool          = Bool()
  val ftqPtr:             FtqPtr        = new FtqPtr
  val exceptionType:      ExceptionType = new ExceptionType
  val exceptionCrossPage: Bool          = Bool()
  val isBackendException: Bool          = Bool()
  val triggered:          UInt          = TriggerAction()
  val isLastInFtqEntry:   Bool          = Bool()
  val instrEndOffset:     UInt          = UInt(FetchBlockInstOffsetWidth.W)
  val debug_seqNum:       InstSeqNum    = InstSeqNum()

  def toCtrlFlow: CtrlFlow = {
    val cf = Wire(new CtrlFlow)
    cf.instr                                         := inst
    cf.pc                                            := pc.toUInt
    cf.foldpc                                        := foldpc
    cf.exceptionVec                                  := 0.U.asTypeOf(ExceptionVec())
    cf.exceptionVec(ExceptionNO.instrPageFault)      := exceptionType.isPf
    cf.exceptionVec(ExceptionNO.instrGuestPageFault) := exceptionType.isGpf
    cf.exceptionVec(ExceptionNO.instrAccessFault)    := exceptionType.isAf
    cf.exceptionVec(ExceptionNO.illegalInstr)        := exceptionType.isIll
    cf.exceptionVec(ExceptionNO.hardwareError)       := exceptionType.isHwe
    cf.backendException                              := isBackendException
    cf.trigger                                       := triggered
    cf.pd                                            := pd
    cf.fixedTaken                                    := fixedTaken
    cf.predTaken                                     := predTaken
    cf.crossPageIPFFix                               := exceptionCrossPage
    cf.storeSetHit                                   := DontCare
    cf.waitForRobIdx                                 := DontCare
    cf.loadWaitBit                                   := DontCare
    cf.loadWaitStrict                                := DontCare
    cf.ssid                                          := DontCare
    cf.ftqPtr                                        := ftqPtr
    cf.ftqOffset                                     := instrEndOffset
    cf.isLastInFtqEntry                              := isLastInFtqEntry
    cf.debug_seqNum                                  := debug_seqNum
    cf
  }
}
