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
import xiangshan.backend.exu.ExuConfig
import xiangshan.backend.rename.BusyTableReadIO
import xiangshan.mem.LsqEnqIO

class Dispatch2Rs(val configs: Seq[Seq[ExuConfig]])(implicit p: Parameters) extends LazyModule with HasXSParameter {
  val numIn = dpParams.IntDqDeqWidth

  val numOut = configs.length
  val numIntSrc = configs.map(_.map(_.intSrcCnt).max)
  val numFpSrc = configs.map(_.map(_.fpSrcCnt).max)

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

class Dispatch2RsImp(outer: Dispatch2Rs)(implicit p: Parameters) extends LazyModuleImp(outer) {
  val numIntStateRead = outer.numIntStateRead
  val numFpStateRead = outer.numFpStateRead

  val io = IO(new Bundle() {
    val in = Flipped(Vec(outer.numIn, DecoupledIO(new MicroOp)))
    val readIntState = if (numIntStateRead > 0) Some(Vec(numIntStateRead, Flipped(new BusyTableReadIO))) else None
    val readFpState = if (numFpStateRead > 0) Some(Vec(numFpStateRead, Flipped(new BusyTableReadIO))) else None
    val out = Vec(outer.numOut, DecoupledIO(new MicroOp))
    val enqLsq = if (outer.hasLoadStore) Some(Flipped(new LsqEnqIO)) else None
  })

  val numInFire = PopCount(io.in.map(_.fire()))
  val numStaFire = PopCount(io.out.zip(outer.configs).filter(_._2.contains(StaExeUnitCfg)).map(_._1.fire()))
  val numStdFire = PopCount(io.out.zip(outer.configs).filter(_._2.contains(StdExeUnitCfg)).map(_._1.fire()))
  XSError(numStaFire =/= numStdFire, "sta_fire != std_fire\n")
  val numOutFire = PopCount(io.out.map(_.fire())) - numStdFire
  XSError(numInFire =/= numOutFire, "in != out\n")

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
  // in: to deal with lsq
  // in.valid: can leave dispatch queue (not blocked by lsq)
  // in.ready: can enter rs
  val in = WireInit(io.in)
  in.foreach(_.ready := false.B)
  io.in.zip(in).foreach(x => x._1.ready := x._2.ready)

  // dirty code for lsq enq
  if (io.enqLsq.isDefined) {
    val enqLsq = io.enqLsq.get
    val fuType = io.in.map(_.bits.ctrl.fuType)
    val isLs = fuType.map(f => FuType.isLoadStore(f))
    val isStore = fuType.map(f => FuType.isStoreExu(f))
    val isAMO = fuType.map(f => FuType.isAMO(f))

    def isBlocked(index: Int): Bool = {
      if (index >= 2) {
        val pairs = (0 until index).flatMap(i => (i + 1 until index).map(j => (i, j)))
        println(pairs)
        val foundLoad = pairs.map(x => io.in(x._1).valid && io.in(x._2).valid && !isStore(x._1) && !isStore(x._2))
        val foundStore = pairs.map(x => io.in(x._1).valid && io.in(x._2).valid && isStore(x._1) && isStore(x._2))
        Mux(isStore(index), VecInit(foundStore).asUInt.orR, VecInit(foundLoad).asUInt.orR) || isBlocked(index - 1)
      }
      else {
        false.B
      }
    }
    for (i <- io.in.indices) {
      // if at least two load/store is found in previous instructions,
      // the instruction is blocked.
      val blocked = isBlocked(i)
      in(i).valid := io.in(i).valid && !blocked && enqLsq.canAccept
      io.in(i).ready := in(i).ready && !blocked && enqLsq.canAccept

      enqLsq.needAlloc(i) := Mux(in(i).valid && isLs(i), Mux(isStore(i) && !isAMO(i), 2.U, 1.U), 0.U)
      enqLsq.req(i).bits := io.in(i).bits
      in(i).bits.lqIdx := enqLsq.resp(i).lqIdx
      in(i).bits.sqIdx := enqLsq.resp(i).sqIdx

      enqLsq.req(i).valid := in(i).valid && io.in(i).ready
    }
  }

  for ((config, i) <- outer.exuConfigCases) {
    val outIndices = outer.exuConfigTypes.zipWithIndex.filter(_._1 == i).map(_._2)
    val numOfThisExu = outIndices.length
    val canAccept = in.map(in => in.valid && config.map(_.canAccept(in.bits.ctrl.fuType)).reduce(_ || _))
    val select = SelectOne("naive", canAccept, numOfThisExu)
    for ((idx, j) <- outIndices.zipWithIndex) {
      val (selectValid, selectIdxOH) = select.getNthOH(j + 1)
      io.out(idx).valid := selectValid
      io.out(idx).bits := Mux1H(selectIdxOH, in.map(_.bits))
      // Special case for STD
      if (config.contains(StdExeUnitCfg)) {
        println(s"std: $idx")
        val sta = io.out(idx - 2)
        io.out(idx).valid := selectValid && sta.ready
        sta.valid := selectValid && io.out(idx).ready
        io.out(idx).bits.ctrl.srcType(0) := Mux1H(selectIdxOH, io.in.map(_.bits.ctrl.srcType(1)))
        io.out(idx).bits.psrc(0) := Mux1H(selectIdxOH, io.in.map(_.bits.psrc(1)))
        in.zip(selectIdxOH).foreach{ case (in, v) => when (v) { in.ready := io.out(idx).ready && sta.ready }}
        XSPerfAccumulate(s"st_rs_not_ready_$idx", selectValid && (!sta.ready || !io.out(idx).ready))
        XSPerfAccumulate(s"sta_rs_not_ready_$idx", selectValid && !sta.ready && io.out(idx).ready)
        XSPerfAccumulate(s"std_rs_not_ready_$idx", selectValid && sta.ready && !io.out(idx).ready)
      }
      else {
        in.zip(selectIdxOH).foreach{ case (in, v) => when (v) { in.ready := io.out(idx).ready }}
      }
    }
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
        when (!io.readIntState.isDefined.B || SrcType.isFp(srcType)) {
          state := resp
        }
    }
  }

  // dispatch is allowed when lsq and rs can accept all the instructions
  // TODO: better algorithm here?
  if (io.enqLsq.isDefined) {
    when (!VecInit(io.out.map(_.ready)).asUInt.andR) {
      in.foreach(_.ready := false.B)
      io.out.foreach(_.valid := false.B)
    }
  }
}
