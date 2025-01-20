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

  val io = IO(new ForwardNetworkIO()).suggestName("io")

  // Pipeline
  // -------------------------------------------------------------------
  // stage 1
  // -------------------------------------------------------------------
  io.toMissQueue.valid := io.s1_in.valid && io.s1_in.bits.forwardTLDchannel
  io.toMissQueue.bits := io.s1_in.bits.toMissQueueForwardReqBundle()

  val s1_sqIdxMask = RegEnable(UIntToMask(io.s0_in.bits.uop.sqIdx.value, StoreQueueSize), io.s0_in.valid)
  io.toLsq.valid := io.s1_in.valid
  io.toLsq.bits := io.s1_in.bits.toLoadForwardReqBundle()
  io.toLsq.bits.sqIdxMask := s1_sqIdxMask

  io.toSBuffer.valid := io.s1_in.valid
  io.toSBuffer.bits := io.s1_in.bits.toLoadForwardReqBundle()

  io.toUncache.valid := io.s1_in.valid
  io.toUncache.bits := io.s1_in.bits.toLoadForwardReqBundle()

  // Pipeline
  // -------------------------------------------------------------------
  // stage 2
  // -------------------------------------------------------------------
  val s2_dataSelect  = genRdataOH(io.s2_in.bits.uop)
  val s2_dataSelectByOffset = genDataSelectByOffset(io.s2_in.bits.paddr(2, 0))

  // forward from tl or miss queue
  val (s2_forwardFromTL, s2_forwardFromTLData) = io.fromTL.forward(io.s1_in.valid && io.s1_in.bits.forwardTLDchannel, io.s1_in.bits.mshrId, io.s1_in.bits.paddr)
  val (s2_forwardDataValid, s2_forwardFromMissQueue, s2_forwardFromMissQueueData) = io.fromMissQueue.forward()
  val s2_forwardFromMissQueueOrTL = s2_forwardDataValid && (s2_forwardFromTL || s2_forwardFromMissQueue)

  // forward from store queue or sbuffer
  // generate VLEN/8 Muxs
  val s2_forwardMask = Wire(Vec((VLEN/8), Bool()))
  val s2_forwardData = Wire(Vec((VLEN/8), UInt(8.W)))

  for (i <- 0 until VLEN / 8) {
    s2_forwardMask(i) := io.fromLsq.forward.forwardMask(i) || io.fromSBuffer.forwardMask(i) || io.fromUncache.forwardMask(i)
    s2_forwardData(i) := Mux(
        io.fromLsq.forward.forwardMask(i),
        io.fromLsq.forward.forwardData(i),
          Mux(
            io.s2_in.bits.isNoncacheable,
            io.fromUncache.forwardData(i),
            io.fromSBuffer.forwardData(i)
          )
        )
  }

  // data from uncache buffer
  val s2_dataFromNc = shiftDataToHigh(io.s2_in.bits.paddr, io.s2_in.bits.data)

  // forward status check
  io.commonOut.s2_fullForward := ((~s2_forwardMask.asUInt).asUInt & io.s2_in.bits.mask) === 0.U && !io.fromLsq.forward.dataInvalid
  io.commonOut.s2_vpMatchFail := io.fromLsq.forward.matchInvalid || io.fromSBuffer.matchInvalid ||
    io.fromUncache.matchInvalid
  io.commonOut.s2_addrInvalid := io.fromLsq.forward.addrInvalid
  io.commonOut.s2_dataInvalid := io.fromLsq.forward.dataInvalid
  io.commonOut.s2_forwardFromMissQueueOrTL := s2_forwardFromMissQueueOrTL

  // Pipeline
  // -------------------------------------------------------------------
  // stage 3
  // -------------------------------------------------------------------
  // data merged
  val s3_dataSelect  = RegEnable(s2_dataSelect, 0.U(s2_dataSelect.getWidth.W), io.s2_in.valid)
  val s3_dataSelectByOffset = RegEnable(s2_dataSelectByOffset, 0.U.asTypeOf(s2_dataSelectByOffset), io.s2_in.valid)

  val s2_rawDataFromPipe = Wire(new LoadDataFromDcacheBundle)
  s2_rawDataFromPipe.dcacheData := Mux(io.s2_in.bits.isNoncacheable, s2_dataFromNc, io.fromDCache.bits.data)
  s2_rawDataFromPipe.forwardDchan := s2_forwardFromTL && !io.s2_in.bits.isNoncacheable
  s2_rawDataFromPipe.forwardDataDchan := s2_forwardFromTLData
  s2_rawDataFromPipe.forwardMshr := s2_forwardFromMissQueue && !io.s2_in.bits.isNoncacheable
  s2_rawDataFromPipe.forwardDataMshr := s2_forwardFromMissQueueData
  s2_rawDataFromPipe.forwardResultValid := s2_forwardDataValid

  s2_rawDataFromPipe.forwardMask := RegEnable(s2_forwardMask, io.s2_in.valid)
  s2_rawDataFromPipe.forwardData := RegEnable(s2_forwardData, io.s2_in.valid)
  s2_rawDataFromPipe.uop := RegEnable(io.s2_in.bits.uop, io.s2_in.valid)
  s2_rawDataFromPipe.addrOffset  := RegEnable(io.s2_in.bits.paddr(3, 0), io.s2_in.valid)

  val s3_mergedDataFromTL = RegEnable(s2_rawDataFromPipe.mergeTLData(), io.s2_in.valid)
  val s3_mergedDataFromPipe = s2_rawDataFromPipe.mergeLsqFwdData(s3_mergedDataFromTL)

  // truncate forward data and cache data to XLEN width to writeback
  val s3_forwardMaskClip = VecInit(List.fill(LdDataDup)(
    RegEnable(Mux(
      io.s2_in.bits.paddr(3),
      (s2_forwardMask.asUInt)(VLEN / 8 - 1, 8),
      (s2_forwardMask.asUInt)(7, 0)
    ).asTypeOf(Vec(XLEN / 8, Bool())), io.s2_in.valid)
  ))
  val s3_forwardDataClip = VecInit(List.fill(LdDataDup)(
    RegEnable(Mux(
      io.s2_in.bits.paddr(3),
      (s2_forwardData.asUInt)(VLEN - 1, 64),
      (s2_forwardData.asUInt)(63, 0)
    ).asTypeOf(Vec(XLEN / 8, UInt(8.W))), io.s2_in.valid)
  ))
  val s3_mergedDataFromTLDClip = VecInit(List.fill(LdDataDup)(
    RegEnable(Mux(
      io.s2_in.bits.paddr(3),
      s2_rawDataFromPipe.mergeTLData()(VLEN - 1, 64),
      s2_rawDataFromPipe.mergeTLData()(63, 0)
    ).asTypeOf(Vec(XLEN / 8, UInt(8.W))), io.s2_in.valid)
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
    io.s3_in.bits.misalignWith16Bytes,
    (s3_mergedDataFromPipe >> (io.s3_in.bits.vaddr(3, 0) << 3)).asUInt(63, 0),
    s3_pickedDataFromPipe(0)
  )

  val s3_dataFromCache = newRdataHelper(s3_dataSelect, s3_shiftData)

  // data from load queue refill
  val s3_rawDataFromMmio = RegNextN(io.fromLsq.mmioLdData, 3)
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
  val s3_vecData = rdataVecHelper(io.s3_in.bits.alignType(1,0), s3_pickedDataFromPipe(2))
  val s3_vecShiftData = (s3_mergedDataFromPipe >> (io.s3_in.bits.vaddr(3, 0) << 3)).asUInt(63, 0)

  // scalar data out
  io.dataOut := Mux(
    io.s3_in.valid,
    s3_dataFromCache,
    s3_dataFromMmio
  )

  // misalign data out
  io.misalignDataOut := Mux(
    io.s3_in.bits.misalignWith16Bytes,
    s3_mergedDataFromPipe,
    s3_pickedDataFromPipe(1)
  )

  // vector data out
  io.vecDataOut := Mux(
    io.s3_in.bits.misalignWith16Bytes,
    s3_vecShiftData,
    Mux(
      io.s3_in.bits.is128bit,
      s3_mergedDataFromPipe,
      s3_vecData
    )
  )
}
