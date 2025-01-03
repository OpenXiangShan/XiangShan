/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.mem

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.mem.Bundles._
import xiangshan.cache._

class ForwardNetworkCommonOutBundle(implicit p: Parameters) extends XSBundle {
  val s2_fullForward = Bool()
  val s2_vpMatchFail = Bool()
  val s2_addrInvalid = Bool()
  val s2_dataInvalid = Bool()
  val s2_forwardFromMissQueueOrTL = Bool()
}

class ForwardNetworkIO(implicit p: Parameters) extends XSBundle {
  val s0_in = Flipped(ValidIO(new LsPipelineBundle))
  val s1_in = Flipped(ValidIO(new LsPipelineBundle))
  val s2_in = Flipped(ValidIO(new LsPipelineBundle))
  val s3_in = Flipped(ValidIO(new LsPipelineBundle))

  val dataOut = Output(UInt(VLEN.W))
  val misalignDataOut = Output(UInt(VLEN.W))
  val vecDataOut = Output(UInt(VLEN.W))

  // to
  val toLsq = ValidIO(new LoadForwardReqBundle)
  val toSBuffer = ValidIO(new LoadForwardReqBundle)
  val toUncache = ValidIO(new LoadForwardReqBundle)
  val toMissQueue = ValidIO(new MissQueueForwardReqBundle)

  // from
  val fromDCache = Flipped(ValidIO(new DCacheWordResp))
  val fromLsq = new Bundle() {
    val forward = Input(new LoadForwardRespBundle)
    val mmioLdData = Input(new LoadDataFromLQBundle)
  }
  val fromSBuffer = Input(new LoadForwardRespBundle)
  val fromUncache = Input(new LoadForwardRespBundle)
  val fromMissQueue = Input(new MissQueueForwardRespBundle)
  val fromTL = Input(new DcacheToLduForwardIO)

  // common
  val commonOut = Output(new ForwardNetworkCommonOutBundle)
}

class ForwardNetwork(implicit p: Parameters) extends XSModule with HasLoadHelper {
  // duplicate reg for ldout and vecldout
  private val LdDataDup = 3
  require(LdDataDup >= 2)

  val io = IO(new ForwardNetworkIO())

  private val (toLsq, fromLsq) = (io.toLsq, io.fromLsq)
  private val (toSBuffer, fromSBuffer) = (io.toSBuffer, io.fromSBuffer)
  private val (toUncache, fromUncache) = (io.toUncache, io.fromUncache)
  private val (toMissQueue, fromMissQueue) = (io.toMissQueue, io.fromMissQueue)
  private val fromDCache = io.fromDCache
  private val fromTL = io.fromTL
  private val commonOut = io.commonOut

  // Pipeline
  // -------------------------------------------------------------------
  // stage 1
  // -------------------------------------------------------------------
  val s1_in = io.s1_in
  toMissQueue.valid := s1_in.valid && s1_in.bits.forwardTLDchannel
  toMissQueue.bits := s1_in.bits.toMissQueueForwardReqBundle()

  val s1_sqIdxMask = RegEnable(UIntToMask(io.s0_in.bits.uop.sqIdx.value, StoreQueueSize), io.s0_in.valid)
  toLsq.valid := s1_in.valid
  toLsq.bits := s1_in.bits.toLoadForwardReqBundle()
  toLsq.bits.sqIdxMask := s1_sqIdxMask

  toSBuffer.valid := s1_in.valid
  toSBuffer.bits := s1_in.bits.toLoadForwardReqBundle()

  toUncache.valid := s1_in.valid
  toUncache.bits := s1_in.bits.toLoadForwardReqBundle()

  // Pipeline
  // -------------------------------------------------------------------
  // stage 2
  // -------------------------------------------------------------------
  val s2_in = io.s2_in

  val s2_dataSelect  = genRdataOH(s2_in.bits.uop)
  val s2_dataSelectByOffset = genDataSelectByOffset(s2_in.bits.paddr(2, 0))

  // forward from tl or miss queue
  val (s2_forwardFromTL, s2_forwardFromTLData) = fromTL.forward(s1_in.valid && s1_in.bits.forwardTLDchannel, s1_in.bits.mshrId, s1_in.bits.paddr)
  val (s2_forwardDataValid, s2_forwardFromMissQueue, s2_forwardFromMissQueueData) = fromMissQueue.forward()
  val s2_forwardFromMissQueueOrTL = s2_forwardDataValid && (s2_forwardFromTL || s2_forwardFromMissQueue)

  // forward from store queue or sbuffer
  // generate VLEN/8 Muxs
  val s2_forwardMask = Wire(Vec((VLEN/8), Bool()))
  val s2_forwardData = Wire(Vec((VLEN/8), UInt(8.W)))

  for (i <- 0 until VLEN / 8) {
    s2_forwardMask(i) := fromLsq.forward.forwardMask(i) || fromSBuffer.forwardMask(i) || fromUncache.forwardMask(i)
    s2_forwardData(i) := Mux(
        fromLsq.forward.forwardMask(i),
        fromLsq.forward.forwardData(i),
          Mux(
            s2_in.bits.isNoncacheable,
            fromUncache.forwardData(i),
            fromSBuffer.forwardData(i)
          )
        )
  }

  // data from uncache buffer
  val s2_forwardFromUncahce = shiftDataToHigh(s2_in.bits.paddr, s2_in.bits.data)

  // forward status check
  commonOut.s2_fullForward := ((~s2_forwardMask.asUInt).asUInt & s2_in.bits.mask) === 0.U && !fromLsq.forward.dataInvalid
  commonOut.s2_vpMatchFail := fromLsq.forward.matchInvalid || fromSBuffer.matchInvalid ||
    fromUncache.matchInvalid
  commonOut.s2_addrInvalid := fromLsq.forward.addrInvalid
  commonOut.s2_dataInvalid := fromLsq.forward.dataInvalid
  commonOut.s2_forwardFromMissQueueOrTL := s2_forwardFromMissQueueOrTL

  // Pipeline
  // -------------------------------------------------------------------
  // stage 3
  // -------------------------------------------------------------------
  val s3_in = io.s3_in

  // data merged
  val s3_dataSelect  = RegEnable(s2_dataSelect, 0.U(s2_dataSelect.getWidth.W), s2_in.valid)
  val s3_dataSelectByOffset = RegEnable(s2_dataSelectByOffset, 0.U.asTypeOf(s2_dataSelectByOffset), s2_in.valid)

  val s3_rawDataFromPipe = Wire(new LoadDataFromDcacheBundle)
  s3_rawDataFromPipe.dcacheData := Mux(s2_in.bits.isNoncacheable, s2_forwardFromUncahce, fromDCache.bits.data)
  s3_rawDataFromPipe.forwardDchan := s2_forwardFromTL && !s2_in.bits.isNoncacheable
  s3_rawDataFromPipe.forwardDataDchan := s2_forwardFromTLData
  s3_rawDataFromPipe.forwardMshr := s2_forwardFromMissQueue && !s2_in.bits.isNoncacheable
  s3_rawDataFromPipe.forwardDataMshr := s2_forwardFromMissQueueData
  s3_rawDataFromPipe.forwardResultValid := s2_forwardDataValid

  s3_rawDataFromPipe.forwardMask := RegEnable(s2_forwardMask, s2_in.valid)
  s3_rawDataFromPipe.forwardData := RegEnable(s2_forwardData, s2_in.valid)
  s3_rawDataFromPipe.uop := RegEnable(s2_in.bits.uop, s2_in.valid)
  s3_rawDataFromPipe.addrOffset  := RegEnable(s2_in.bits.paddr(3, 0), s2_in.valid)

  val s3_mergedDataFromTL = RegEnable(s3_rawDataFromPipe.mergeTLData(), s2_in.valid)
  val s3_mergedDataFromPipe = s3_rawDataFromPipe.mergeLsqFwdData(s3_mergedDataFromTL)

  // truncate forward data and cache data to XLEN width to writeback
  val s3_forwardMaskClip = VecInit(List.fill(LdDataDup)(
    RegEnable(Mux(
      s2_in.bits.paddr(3),
      (s2_forwardMask.asUInt)(VLEN / 8 - 1, 8),
      (s2_forwardMask.asUInt)(7, 0)
    ).asTypeOf(Vec(XLEN / 8, Bool())), s2_in.valid)
  ))
  val s3_forwardDataClip = VecInit(List.fill(LdDataDup)(
    RegEnable(Mux(
      s2_in.bits.paddr(3),
      (s2_forwardData.asUInt)(VLEN - 1, 64),
      (s2_forwardData.asUInt)(63, 0)
    ).asTypeOf(Vec(XLEN / 8, UInt(8.W))), s2_in.valid)
  ))
  val s3_mergedDataFromTLDClip = VecInit(List.fill(LdDataDup)(
    RegEnable(Mux(
      s2_in.bits.paddr(3),
      s3_rawDataFromPipe.mergeTLData()(VLEN - 1, 64),
      s3_rawDataFromPipe.mergeTLData()(63, 0)
    ).asTypeOf(Vec(XLEN / 8, UInt(8.W))), s2_in.valid)
  ))
  val s3_mergedDataFromPipeClip = VecInit((0 until LdDataDup).map(i => {
    VecInit((0 until XLEN / 8).map(j =>
      Mux(s3_forwardMaskClip(i)(j), s3_forwardDataClip(i)(j), s3_mergedDataFromTLDClip(i)(j))
    )).asUInt
  }))

  val s3_dataFromPipe = VecInit((0 until LdDataDup).map(i => {
    VecInit(Seq(
      s3_mergedDataFromPipeClip(i)(63,    0),
      s3_mergedDataFromPipeClip(i)(63,    8),
      s3_mergedDataFromPipeClip(i)(63,   16),
      s3_mergedDataFromPipeClip(i)(63,   24),
      s3_mergedDataFromPipeClip(i)(63,   32),
      s3_mergedDataFromPipeClip(i)(63,   40),
      s3_mergedDataFromPipeClip(i)(63,   48),
      s3_mergedDataFromPipeClip(i)(63,   56),
    ))
  }))
  val s3_pickedDataFromPipe = VecInit((0 until LdDataDup).map(i => {
    Mux1H(s3_dataSelectByOffset, s3_dataFromPipe(i))
  }))
  val s3_shiftData = Mux(
    s3_in.bits.misalignWith16Bytes,
    (s3_mergedDataFromPipe >> (s3_in.bits.vaddr(3, 0) << 3)).asUInt(63, 0),
    s3_pickedDataFromPipe(0)
  )

  val s3_dataFromCache = newRdataHelper(s3_dataSelect, s3_shiftData)

  // data from load queue refill
  val s3_rawDataFromMmio = RegNextN(fromLsq.mmioLdData, 3)
  val s3_mergedDataFromMmio = s3_rawDataFromMmio.mergedData()
  val s3_pickedDataFromMmio = LookupTree(s3_rawDataFromMmio.addrOffset, List(
    "b000".U -> s3_mergedDataFromMmio(63,  0),
    "b001".U -> s3_mergedDataFromMmio(63,  8),
    "b010".U -> s3_mergedDataFromMmio(63, 16),
    "b011".U -> s3_mergedDataFromMmio(63, 24),
    "b100".U -> s3_mergedDataFromMmio(63, 32),
    "b101".U -> s3_mergedDataFromMmio(63, 40),
    "b110".U -> s3_mergedDataFromMmio(63, 48),
    "b111".U -> s3_mergedDataFromMmio(63, 56)
  ))
  val s3_dataFromMmio = rdataHelper(s3_rawDataFromMmio.uop, s3_pickedDataFromMmio)
  val s3_vecData = rdataVecHelper(s3_in.bits.alignType(1,0), s3_pickedDataFromPipe(2))
  val s3_vecShiftData = (s3_mergedDataFromPipe >> (s3_in.bits.vaddr(3, 0) << 3)).asUInt(63, 0)

  // scalar data out
  io.dataOut := Mux(
    s3_in.valid,
    s3_dataFromCache,
    s3_dataFromMmio
  )

  // misalign data out
  io.misalignDataOut := Mux(
    s3_in.bits.misalignWith16Bytes,
    s3_mergedDataFromPipe,
    s3_pickedDataFromPipe(1)
  )

  // vector data out
  io.vecDataOut := Mux(
    s3_in.bits.misalignWith16Bytes,
    s3_vecShiftData,
    Mux(
      s3_in.bits.is128bit,
      s3_mergedDataFromPipe,
      s3_vecData
    )
  )
}
