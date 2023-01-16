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

package xiangshan.backend.dispatch

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xiangshan._
import utils._
import utility._
import xiangshan.backend.exu.ExuConfig
import xiangshan.backend.rename.BusyTableReadIO
import xiangshan.mem.LsqEnqIO

class Dispatch2Rs(val configs: Seq[Seq[ExuConfig]])(implicit p: Parameters) extends LazyModule with HasXSParameter {
  val numIn = dpParams.IntDqDeqWidth

  val numOut = configs.length
  val numIntSrc = configs.map(_.map(_.intSrcCnt).max)
  val numFpSrc = configs.map(_.map(_.fpVecSrcCnt).max)

  val exuConfigCases = configs.distinct.sortBy(_.length).zipWithIndex
  val exuConfigTypes = configs.map(cfg => exuConfigCases.find(_._1 == cfg).get._2)

  // Different mode of dispatch
  // (1) isDistinct: no overlap
  val isDistinct = exuConfigCases.flatMap(_._1).distinct.length == exuConfigCases.flatMap(_._1).length && exuConfigCases.length > 1
  // (2) isLessExu: exu becomes less and less
  val isLessExu = configs.dropRight(1).zip(configs.tail).forall(x => x._2.toSet.subsetOf(x._1.toSet))
  val supportedDpMode = Seq(isDistinct, isLessExu)
  require(supportedDpMode.count(x => x) == 1, s"dispatch mode valid iff one mode is found in $supportedDpMode")

  val numIntStateRead = if (isLessExu) numIntSrc.max * numIn else numIntSrc.sum
  val numFpStateRead = if (isLessExu) numFpSrc.max * numIn else numFpSrc.sum

  val hasLoadStore = configs.exists(cfgs => cfgs.contains(LdExeUnitCfg) || cfgs.contains(StaExeUnitCfg))

  lazy val module = Dispatch2RsImp(this, supportedDpMode.zipWithIndex.filter(_._1).head._2)
}

class Dispatch2RsImp(outer: Dispatch2Rs)(implicit p: Parameters) extends LazyModuleImp(outer) with HasXSParameter {
  val numIntStateRead = outer.numIntStateRead
  val numFpStateRead = outer.numFpStateRead

  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val in = Flipped(Vec(outer.numIn, DecoupledIO(new MicroOp)))
    val readIntState = if (numIntStateRead > 0) Some(Vec(numIntStateRead, Flipped(new BusyTableReadIO))) else None
    val readFpState = if (numFpStateRead > 0) Some(Vec(numFpStateRead, Flipped(new BusyTableReadIO))) else None
    val out = Vec(outer.numOut, DecoupledIO(new MicroOp))
    val enqLsq = if (outer.hasLoadStore) Some(Flipped(new LsqEnqIO)) else None
  })

  val numInFire = PopCount(io.in.map(_.fire))
  val numStaFire = PopCount(io.out.zip(outer.configs).filter(_._2.contains(StaExeUnitCfg)).map(_._1.fire))
  val numStdFire = PopCount(io.out.zip(outer.configs).filter(_._2.contains(StdExeUnitCfg)).map(_._1.fire))
  // XSError(numStaFire =/= numStdFire, "sta_fire != std_fire\n")
  val numOutFire = PopCount(io.out.map(_.fire)) - numStdFire
  // XSError(numInFire =/= numOutFire, "in != out\n")

  XSPerfAccumulate("in_valid", PopCount(io.in.map(_.valid)))
  XSPerfAccumulate("in_fire", PopCount(io.in.map(_.fire)))
  XSPerfAccumulate("out_valid", PopCount(io.out.map(_.valid)))
  XSPerfAccumulate("out_fire", PopCount(io.out.map(_.fire)))
}

object Dispatch2RsImp {
  def apply(outer: Dispatch2Rs, conf: Int)(implicit p: Parameters): Dispatch2RsImp = {
    conf match {
      case 0 => new Dispatch2RsDistinctImp(outer)
      case 1 => new Dispatch2RsLessExuImp(outer)
      case _ => null
    }
  }
}

class Dispatch2RsLessExuImp(outer: Dispatch2Rs)(implicit p: Parameters) extends Dispatch2RsImp(outer) {
  val numIntSrc = outer.numIntSrc.max
  val numFpSrc = outer.numFpSrc.max

  // We always read physical register states when in gives the instructions.
  // This usually brings better timing.
  if (io.readIntState.isDefined) {
    val req = io.in.flatMap(in => in.bits.psrc.take(numIntSrc))
    io.readIntState.get.map(_.req).zip(req).foreach(x => x._1 := x._2)
  }
  if (io.readFpState.isDefined) {
    val req = io.in.flatMap(in => in.bits.psrc.take(numFpSrc))
    io.readFpState.get.map(_.req).zip(req).foreach(x => x._1 := x._2)
  }

  val enableLoadBalance = outer.numOut > 2
  val numPingPongBits = outer.numOut / 2
  val pingpong = Seq.fill(numPingPongBits)(RegInit(false.B))
  pingpong.foreach(p => p := !p)
  val pairIndex = (0 until outer.numOut).map(i => (i + 2) % outer.numOut)

  def needLoadBalance(index: Int): Bool = {
    val bitIndex = Seq(index, pairIndex(index), numPingPongBits - 1).min
    // When ping pong bit is set, use pairIndex
    if (enableLoadBalance) pingpong(bitIndex) && (index != pairIndex(index)).B else false.B
  }
  // out is directly connected from in for better timing
  // TODO: select critical instruction first
  val numMaxExuConfig = outer.exuConfigCases.last._1.length
  for ((config, i) <- outer.configs.zipWithIndex) {
    io.out(i) <> io.in(i)
    // When the corresponding execution units do not have full functionalities,
    // we have to filter out the instructions that these execution units does not accept.
    if (config.length < numMaxExuConfig) {
      val thisCanAccept = config.map(_.canAccept(io.in(i).bits.ctrl.fuType)).reduce(_ || _)
      io.out(i).valid := io.in(i).valid && thisCanAccept
      io.in(i).ready := io.out(i).ready && thisCanAccept
    }
  }
  // For load balance, the out port alternates between different in ports
  // It must be another for loop because the former for loop does not have any condition
  // and will override the assignments.
  for ((config, i) <- outer.configs.zipWithIndex) {
    when (needLoadBalance(i)) {
      io.out(i) <> io.in(pairIndex(i))
      if (config.length < numMaxExuConfig) {
        val thisCanAccept = config.map(_.canAccept(io.in(pairIndex(i)).bits.ctrl.fuType)).reduce(_ || _)
        io.out(i).valid := io.in(pairIndex(i)).valid && thisCanAccept
        io.in(pairIndex(i)).ready := io.out(i).ready && thisCanAccept
      }
      println(s"Dispatch2Rs ports balance between $i and ${pairIndex(i)}")
    }
  }

  // srcState is read from outside and connected directly
  if (io.readIntState.isDefined) {
    val intSrcStateVec = io.out.flatMap(_.bits.srcState.take(numIntSrc))
    io.readIntState.get.map(_.resp).zip(intSrcStateVec).foreach(x => x._2 := x._1)
    for (i <- 0 until outer.numOut) {
      val pairState = io.readIntState.get.slice(numIntSrc * pairIndex(i), numIntSrc * pairIndex(i) + numIntSrc)
      when (needLoadBalance(i)) {
        pairState.map(_.resp).zip(io.out(i).bits.srcState.take(numIntSrc)).foreach(x => x._2 := x._1)
      }
    }
  }
  if (io.readFpState.isDefined) {
    require(io.readIntState.isEmpty, "we do not implement int+fp in isLessExu")
    val fpSrcStateVec = io.out.flatMap(_.bits.srcState.take(numFpSrc))
    io.readFpState.get.map(_.resp).zip(fpSrcStateVec).foreach(x => x._2 := x._1)
    for (i <- 0 until outer.numOut) {
      val pairState = io.readFpState.get.slice(numFpSrc * pairIndex(i), numFpSrc * pairIndex(i) + numFpSrc)
      when (needLoadBalance(i)) {
        pairState.map(_.resp).zip(io.out(i).bits.srcState.take(numFpSrc)).foreach(x => x._2 := x._1)
      }
    }
  }

  // If io.out is wider than io.in, we need to set io.in.ready to false.B.
  for (i <- io.out.length until io.in.length) {
    io.in(i).ready := false.B
  }
}

class Dispatch2RsDistinctImp(outer: Dispatch2Rs)(implicit p: Parameters) extends Dispatch2RsImp(outer) {
  require(LoadPipelineWidth == StorePipelineWidth)
  // in: to deal with lsq
  // in.valid: can leave dispatch queue (not blocked by lsq)
  // in.ready: can enter rs
  val in = WireInit(io.in)
  in.foreach(_.ready := false.B)
  io.in.zip(in).foreach(x => x._1.ready := x._2.ready)

  // add one pipeline before out
  val s0_out = Wire(io.out.cloneType)
  // dirty code for lsq enq
  val is_blocked = WireDefault(VecInit(Seq.fill(io.in.length)(false.B)))
  if (io.enqLsq.isDefined) {
    val enqLsq = io.enqLsq.get
    val fuType = io.in.map(_.bits.ctrl.fuType)
    val isLs = fuType.map(f => FuType.isLoadStore(f))
    val isStore = fuType.map(f => FuType.isStoreExu(f))
    val isAMO = fuType.map(f => FuType.isAMO(f))

    val isLoadArrays = Seq.tabulate(io.in.length)(Seq.tabulate(_)(i => io.in(i).valid && !isStore(i)))
    val isStoreArrays = Seq.tabulate(io.in.length)(Seq.tabulate(_)(i => io.in(i).valid && isStore(i)))
    val blockLoads = isLoadArrays.map(PopCount(_) >= LoadPipelineWidth.U)
    val blockStores = isStoreArrays.map(PopCount(_) >= StorePipelineWidth.U)

    for (i <- io.in.indices) {
      is_blocked(i) := (
        if (i >= LoadPipelineWidth) Mux(isStore(i), blockStores(i), blockLoads(i)) || is_blocked(i - 1)
        else false.B
      )
      in(i).valid := io.in(i).valid && !is_blocked(i)
      io.in(i).ready := in(i).ready && !is_blocked(i)

      if (i < enqLsq.req.length) {
        enqLsq.needAlloc(i) := Mux(io.in(i).valid && isLs(i), Mux(isStore(i) && !isAMO(i), 2.U, 1.U), 0.U)
        enqLsq.req(i).bits := io.in(i).bits
        in(i).bits.lqIdx := enqLsq.resp(i).lqIdx
        in(i).bits.sqIdx := enqLsq.resp(i).sqIdx

        enqLsq.req(i).valid := in(i).valid && VecInit(s0_out.map(_.ready)).asUInt.andR
      }
    }
  }

  for ((config, i) <- outer.exuConfigCases) {
    val outIndices = outer.exuConfigTypes.zipWithIndex.filter(_._1 == i).map(_._2)
    val numOfThisExu = outIndices.length
    val canAccept = io.in.map(in => in.valid && config.map(_.canAccept(in.bits.ctrl.fuType)).reduce(_ || _))
    val select = SelectOne("naive", canAccept, numOfThisExu)
    for ((idx, j) <- outIndices.zipWithIndex) {
      val (selectValid, selectIdxOH) = select.getNthOH(j + 1)
      s0_out(idx).valid := selectValid && !Mux1H(selectIdxOH, is_blocked)
      s0_out(idx).bits := Mux1H(selectIdxOH, in.map(_.bits))
      // Special case for STD
      if (config.contains(StdExeUnitCfg)) {
        val sta = s0_out(idx - StorePipelineWidth)
        sta.valid := s0_out(idx).valid
        s0_out(idx).bits.ctrl.srcType(0) := s0_out(idx).bits.ctrl.srcType(1)
        s0_out(idx).bits.psrc(0) := s0_out(idx).bits.psrc(1)
        XSPerfAccumulate(s"st_rs_not_ready_$idx", selectValid && (!sta.ready || !s0_out(idx).ready))
        XSPerfAccumulate(s"sta_rs_not_ready_$idx", selectValid && !sta.ready && s0_out(idx).ready)
        XSPerfAccumulate(s"std_rs_not_ready_$idx", selectValid && sta.ready && !s0_out(idx).ready)
      }
      else {
        in.zip(selectIdxOH).foreach{ case (in, v) => when (v) { in.ready := s0_out(idx).ready }}
      }
    }
  }

  // dispatch is allowed when lsq and rs can accept all the instructions
  // TODO: better algorithm here?
  if (io.enqLsq.isDefined) {
    when (!VecInit(s0_out.map(_.ready)).asUInt.andR || !io.enqLsq.get.canAccept) {
      in.foreach(_.ready := false.B)
      s0_out.foreach(_.valid := false.B)
    }
  }

  // agreement with dispatch queue: don't enqueue when io.redirect.valid
  when (io.redirect.valid) {
    s0_out.foreach(_.valid := false.B)
  }

  // Note: the dispatch queue must not dequeue when io.redirect.valid
  val s1_rightFire = Wire(Vec(s0_out.length, Bool()))
  val s1_flush = Wire(Vec(s0_out.length, Bool()))
  val s1_out = io.out.indices.map(i => PipelineNext(s0_out(i), s1_rightFire(i), s1_flush(i)))
  for (i <- io.out.indices) {
    io.out(i).valid := s1_out(i).valid
    io.out(i).bits := s1_out(i).bits
    s1_out(i).ready := !s1_out(i).valid || io.out(i).ready
    s1_rightFire(i) := io.out(i).ready
    s1_flush(i) := s1_out(i).valid && s1_out(i).bits.robIdx.needFlush(io.redirect)
  }
  if (io.readIntState.isDefined) {
    val stateReadReq = io.out.zip(outer.numIntSrc).flatMap(x => x._1.bits.psrc.take(x._2))
    io.readIntState.get.map(_.req).zip(stateReadReq).foreach(x => x._1 := x._2)
    val stateReadResp = io.out.zip(outer.numIntSrc).flatMap(x => x._1.bits.srcState.take(x._2))
    io.readIntState.get.map(_.resp).zip(stateReadResp).foreach(x => x._2 := x._1)
  }

  if (io.readFpState.isDefined) {
    val stateReadReq = io.out.zip(outer.numFpSrc).flatMap(x => x._1.bits.psrc.take(x._2))
    io.readFpState.get.map(_.req).zip(stateReadReq).foreach(x => x._1 := x._2)
    val stateReadResp = io.out.zip(outer.numFpSrc).flatMap(x => x._1.bits.srcState.take(x._2))
    val srcTypeOut = io.out.zip(outer.numFpSrc).flatMap(x => x._1.bits.ctrl.srcType.take(x._2))
    // When both int and fp are needed, need Mux
    io.readFpState.get.map(_.resp).zip(stateReadResp).zip(srcTypeOut).foreach{
      case ((resp, state), srcType) =>
        when (!io.readIntState.isDefined.B || SrcType.isFp(srcType) || SrcType.isVp(srcType)) {
          state := resp
        }
    }
  }

}
