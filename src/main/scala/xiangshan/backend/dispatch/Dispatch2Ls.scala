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
import xiangshan._
import utils._
import xiangshan.backend.regfile.RfReadPort
import xiangshan.backend.rename.BusyTableReadIO

class Dispatch2Ls(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val fromDq = Flipped(Vec(dpParams.LsDqDeqWidth, DecoupledIO(new MicroOp)))
    val readIntRf = Vec(NRMemReadPorts, Output(UInt(PhyRegIdxWidth.W)))
    val readFpRf = Vec(exuParameters.StuCnt, Output(UInt(PhyRegIdxWidth.W)))
    val readIntState = Vec(NRMemReadPorts, Flipped(new BusyTableReadIO))
    val readFpState = Vec(exuParameters.StuCnt, Flipped(new BusyTableReadIO))
    val enqIQCtrl = Vec(exuParameters.LsExuCnt, DecoupledIO(new MicroOp))
  })

  /**
    * Part 1: generate indexes for reservation stations
    */
  val loadIndexGen = Module(new IndexMapping(dpParams.LsDqDeqWidth, exuParameters.LduCnt, true))
  val loadCanAccept = VecInit(io.fromDq.map(deq => deq.valid && FuType.loadCanAccept(deq.bits.ctrl.fuType)))
  val (loadPriority, _) = PriorityGen((0 until exuParameters.LduCnt).map(i => 0.U))
  loadIndexGen.io.validBits := loadCanAccept
  loadIndexGen.io.priority := loadPriority

  val storeIndexGen = Module(new IndexMapping(dpParams.LsDqDeqWidth, exuParameters.StuCnt, true))
  val storeCanAccept = VecInit(io.fromDq.map(deq => deq.valid && FuType.storeCanAccept(deq.bits.ctrl.fuType)))
  val (storePriority, _) = PriorityGen((0 until exuParameters.StuCnt).map(i => 0.U))
  storeIndexGen.io.validBits := storeCanAccept
  storeIndexGen.io.priority := storePriority

  val allIndexGen = Seq(loadIndexGen, storeIndexGen)
  val validVec = allIndexGen.map(_.io.mapping.map(_.valid)).reduceLeft(_ ++ _)
  val indexVec = allIndexGen.map(_.io.mapping.map(_.bits)).reduceLeft(_ ++ _)
  for (i <- validVec.indices) {
    // XSDebug(p"mapping $i: valid ${validVec(i)} index ${indexVec(i)}\n")
  }

  /**
    * Part 2: assign regfile read ports (actually only reg states from rename)
    *
    * The four load/store issue queue
    */
  assert(exuParameters.LduCnt == 2)
  assert(exuParameters.StuCnt == 2)
  val readPort = Seq(0, 1, 2, 4)
  val firstStorePsrc2 = PriorityMux(storeCanAccept, io.fromDq.map(_.bits.psrc(1)))
  val secondStorePsrc2 = PriorityMux((1 until 4).map(i => Cat(storeCanAccept.take(i)).orR && storeCanAccept(i)), io.fromDq.drop(1).map(_.bits.psrc(1)))
  for (i <- 0 until exuParameters.LsExuCnt) {
    if (i < exuParameters.LduCnt) {
      io.readIntRf(readPort(i)) := io.fromDq(indexVec(i)).bits.psrc(0)
    }
    else {
      io.readFpRf(i - exuParameters.LduCnt) := io.fromDq(indexVec(i)).bits.psrc(1)
      io.readIntRf(readPort(i)  ) := io.fromDq(indexVec(i)).bits.psrc(0)
      io.readIntRf(readPort(i)+1) := io.fromDq(indexVec(i)).bits.psrc(1)
    }
  }
  // src1 always needs srcState but only store's src2 needs srcState
  for (i <- 0 until 4) {
    io.readIntState(i).req := io.fromDq(i).bits.psrc(0)
  }
  io.readIntState(4).req := firstStorePsrc2
  io.readIntState(5).req := secondStorePsrc2
  io.readFpState(0).req := firstStorePsrc2
  io.readFpState(1).req := secondStorePsrc2

  /**
    * Part 3: dispatch to reservation stations
    */
  val loadReady = Cat(io.enqIQCtrl.take(exuParameters.LduCnt).map(_.ready)).andR
  val storeReady = Cat(io.enqIQCtrl.drop(exuParameters.LduCnt).map(_.ready)).andR
  for (i <- 0 until exuParameters.LsExuCnt) {
    val enq = io.enqIQCtrl(i)
    if (i < exuParameters.LduCnt) {
      enq.valid := loadIndexGen.io.mapping(i).valid && loadReady
    }
    else {
      enq.valid := storeIndexGen.io.mapping(i - exuParameters.LduCnt).valid && storeReady
    }
    enq.bits := io.fromDq(indexVec(i)).bits
    enq.bits.srcState(0) := io.readIntState(indexVec(i)).resp
    if (i < exuParameters.LduCnt) {
      enq.bits.srcState(1) := DontCare
    }
    else {
      enq.bits.srcState(1) := Mux(io.fromDq(indexVec(i)).bits.ctrl.srcType(1) === SrcType.fp,
        Mux(storePriority(i-2) === 0.U, io.readFpState(0).resp, io.readFpState(1).resp),
        Mux(storePriority(i-2) === 0.U, io.readIntState(4).resp, io.readIntState(5).resp)
      )
    }
    enq.bits.srcState(2) := DontCare

    XSInfo(enq.fire(), p"pc 0x${Hexadecimal(enq.bits.cf.pc)} with type ${enq.bits.ctrl.fuType} " +
      p"srcState(${enq.bits.srcState(0)} ${enq.bits.srcState(1)}) " +
      p"enters issue queue $i from ${indexVec(i)}\n")
  }

  /**
    * Part 4: response to dispatch queue
    */
  val load2CanOut = !(loadCanAccept(0) && loadCanAccept(1))
  val load3CanOut = !(loadCanAccept(0) && loadCanAccept(1) || loadCanAccept(0) && loadCanAccept(2) || loadCanAccept(1) && loadCanAccept(2))
  val store2CanOut = !(storeCanAccept(0) && storeCanAccept(1))
  val store3CanOut = !(storeCanAccept(0) && storeCanAccept(1) || storeCanAccept(0) && storeCanAccept(2) || storeCanAccept(1) && storeCanAccept(2))
  for (i <- 0 until dpParams.LsDqDeqWidth) {
    io.fromDq(i).ready := loadCanAccept(i) && (if (i <= 1) true.B else if (i == 2) load2CanOut else load3CanOut) && loadReady ||
                          storeCanAccept(i) && (if (i <= 1) true.B else if (i == 2) store2CanOut else store3CanOut) && storeReady

    XSInfo(io.fromDq(i).fire(),
      p"pc 0x${Hexadecimal(io.fromDq(i).bits.cf.pc)} leaves Ls dispatch queue $i with nrob ${io.fromDq(i).bits.robIdx}\n")
    XSDebug(io.fromDq(i).valid && !io.fromDq(i).ready,
      p"pc 0x${Hexadecimal(io.fromDq(i).bits.cf.pc)} waits at Ls dispatch queue with index $i\n")
  }
  XSError(PopCount(io.fromDq.map(_.fire())) =/= PopCount(io.enqIQCtrl.map(_.fire())), "deq =/= enq\n")

  /**
    * Part 5: the second stage of dispatch 2 (send data to reservation station)
    */
//  val uopReg = Reg(Vec(exuParameters.LsExuCnt, new MicroOp))
//  val dataValidRegDebug = Reg(Vec(exuParameters.LsExuCnt, Bool()))
//  for (i <- 0 until exuParameters.LsExuCnt) {
//    uopReg(i) := io.enqIQCtrl(i).bits
//    dataValidRegDebug(i) := io.enqIQCtrl(i).fire()
//
//    io.enqIQData(i) := DontCare
//    // assert(uopReg(i).ctrl.srcType(0) =/= SrcType.pc)
//    io.enqIQData(i).src(0) := io.readIntRf(readPort(i)).data
//    if (i >= exuParameters.LduCnt) {
//      io.enqIQData(i).src(1) := Mux(
//        uopReg(i).ctrl.srcType(1) === SrcType.imm,
//        uopReg(i).ctrl.imm,
//        Mux(uopReg(i).ctrl.srcType(1) === SrcType.fp,
//          io.readFpRf(i - exuParameters.LduCnt).data,
//          io.readIntRf(readPort(i) + 1).data))
//    }
//
//    XSDebug(dataValidRegDebug(i),
//      p"pc 0x${Hexadecimal(uopReg(i).cf.pc)} reads operands from " +
//        p"(${readPort(i)  }, ${uopReg(i).psrc(0)}, ${Hexadecimal(io.enqIQData(i).src(0))}), " +
//        p"(${readPort(i)+1}, ${uopReg(i).psrc(1)}, ${Hexadecimal(io.enqIQData(i).src(1))})\n")
//  }

  XSPerfAccumulate("in", PopCount(io.fromDq.map(_.valid)))
  XSPerfAccumulate("out", PopCount(io.enqIQCtrl.map(_.fire())))
  XSPerfAccumulate("out_load0", io.enqIQCtrl(0).fire())
  XSPerfAccumulate("out_load1", io.enqIQCtrl(1).fire())
  XSPerfAccumulate("out_store0", io.enqIQCtrl(2).fire())
  XSPerfAccumulate("out_store1", io.enqIQCtrl(3).fire())
  val block_num = PopCount(io.fromDq.map(deq => deq.valid && !deq.ready))
  XSPerfAccumulate("blocked", block_num)
  XSPerfAccumulate("blocked_index", Mux(block_num =/= 0.U, PriorityEncoder(io.fromDq.map(deq => deq.valid && !deq.ready)), 0.U))
  XSPerfAccumulate("load_deq", PopCount(loadCanAccept))
  XSPerfAccumulate("load_deq_exceed_limit", Mux(PopCount(loadCanAccept) >= 2.U, PopCount(loadCanAccept) - 2.U, 0.U))
  XSPerfAccumulate("store_deq", PopCount(storeCanAccept))
  XSPerfAccumulate("store_deq_exceed_limit", Mux(PopCount(storeCanAccept) >= 2.U, PopCount(storeCanAccept) - 2.U, 0.U))
  XSPerfAccumulate("load0_blocked_by_load0", loadIndexGen.io.mapping(0).valid && !io.enqIQCtrl(0).ready && io.enqIQCtrl(1).ready)
  XSPerfAccumulate("load0_blocked_by_load1", loadIndexGen.io.mapping(0).valid && io.enqIQCtrl(0).ready && !io.enqIQCtrl(1).ready)
  XSPerfAccumulate("load1_blocked_by_load0", loadIndexGen.io.mapping(1).valid && !io.enqIQCtrl(0).ready && io.enqIQCtrl(1).ready)
  XSPerfAccumulate("load1_blocked_by_load1", loadIndexGen.io.mapping(1).valid && io.enqIQCtrl(0).ready && !io.enqIQCtrl(1).ready)
  XSPerfAccumulate("store0_blocked_by_store0", storeIndexGen.io.mapping(0).valid && !io.enqIQCtrl(2).ready && io.enqIQCtrl(3).ready)
  XSPerfAccumulate("store0_blocked_by_store1", storeIndexGen.io.mapping(0).valid && io.enqIQCtrl(2).ready && !io.enqIQCtrl(3).ready)
  XSPerfAccumulate("store1_blocked_by_store0", storeIndexGen.io.mapping(1).valid && !io.enqIQCtrl(2).ready && io.enqIQCtrl(3).ready)
  XSPerfAccumulate("store1_blocked_by_store1", storeIndexGen.io.mapping(1).valid && io.enqIQCtrl(2).ready && !io.enqIQCtrl(3).ready)
}
