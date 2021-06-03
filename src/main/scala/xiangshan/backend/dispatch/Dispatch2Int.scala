/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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
import xiangshan.backend.rename.BusyTableReadIO

class Dispatch2Int(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val fromDq = Flipped(Vec(dpParams.IntDqDeqWidth, DecoupledIO(new MicroOp)))
    val readRf = Vec(NRIntReadPorts - NRMemReadPorts, Output(UInt(PhyRegIdxWidth.W)))
    val readState = Vec(NRIntReadPorts - NRMemReadPorts, Flipped(new BusyTableReadIO))
    val numExist = Input(Vec(exuParameters.IntExuCnt, UInt(log2Ceil(IssQueSize).W)))
    val enqIQCtrl = Vec(exuParameters.IntExuCnt, DecoupledIO(new MicroOp))
    val readPortIndex = Vec(exuParameters.IntExuCnt, Output(UInt(log2Ceil(8 / 2).W)))
  })

  val jmpCnt = exuParameters.JmpCnt
  val mduCnt = exuParameters.MduCnt
  val aluCnt = exuParameters.AluCnt

  /**
    * Part 1: generate indexes for reservation stations
    */
  assert(jmpCnt == 1)
  val jmpCanAccept = VecInit(io.fromDq.map(deq => deq.valid && FuType.jmpCanAccept(deq.bits.ctrl.fuType)))
  val mduCanAccept = VecInit(io.fromDq.map(deq => deq.valid && FuType.mduCanAccept(deq.bits.ctrl.fuType)))
  val aluCanAccept = VecInit(io.fromDq.map(deq => deq.valid && FuType.aluCanAccept(deq.bits.ctrl.fuType)))

  val jmpIndexGen = Module(new IndexMapping(dpParams.IntDqDeqWidth, jmpCnt, false))
  val mduIndexGen = Module(new IndexMapping(dpParams.IntDqDeqWidth, mduCnt, true))
  // val aluIndexGen = Module(new IndexMapping(dpParams.IntDqDeqWidth, aluCnt, true))

  val (mduPriority, mduIndex) = PriorityGen(io.numExist.slice(jmpCnt, jmpCnt + mduCnt))
  val (aluPriority, aluIndex) = PriorityGen(io.numExist.drop(jmpCnt + mduCnt))
  jmpIndexGen.io.validBits := jmpCanAccept
  mduIndexGen.io.validBits := mduCanAccept
  // aluIndexGen.io.validBits := aluCanAccept
  jmpIndexGen.io.priority := DontCare
  mduIndexGen.io.priority := mduPriority
  // aluIndexGen.io.priority := aluPriority

  // val allIndexGen = Seq(jmpIndexGen, mduIndexGen, aluIndexGen)
  // val validVec = allIndexGen.flatMap(_.io.mapping.map(_.valid))
  // val indexVec = allIndexGen.flatMap(_.io.mapping.map(_.bits))

  /**
    * Part 2: assign regfile read ports
    */
  // val intStaticIndex = Seq(3, 4, 5, 6)
  // val intDynamicIndex = Seq(0, 1, 2)
  // val intStaticMappedValid = intStaticIndex.map(i => validVec(i))
  // val intDynamicMappedValid = intDynamicIndex.map(i => validVec(i))
  // val (intReadPortSrc, intDynamicExuSrc) = RegfileReadPortGen(intStaticMappedValid, intDynamicMappedValid)
  // val intStaticMapped = intStaticIndex.map(i => indexVec(i))
  // val intDynamicMapped = intDynamicIndex.map(i => indexVec(i))
  // for (i <- intStaticIndex.indices) {
  //   val index = WireInit(VecInit(intStaticMapped(i) +: intDynamicMapped))
  //   io.readRf(2*i  ) := io.fromDq(index(intReadPortSrc(i))).bits.psrc(0)
  //   io.readRf(2*i+1) := io.fromDq(index(intReadPortSrc(i))).bits.psrc(1)
  // }
  // val readPortIndex = Wire(Vec(exuParameters.IntExuCnt, UInt(2.W)))
  // intStaticIndex.zipWithIndex.map({case (index, i) => readPortIndex(index) := i.U})
  // intDynamicIndex.zipWithIndex.map({case (index, i) => readPortIndex(index) := intDynamicExuSrc(i)})
  io.readRf(0) := io.enqIQCtrl(3).bits.psrc(0)
  io.readRf(1) := io.enqIQCtrl(3).bits.psrc(1)
  io.readRf(2) := Mux(io.enqIQCtrl(4).valid, io.enqIQCtrl(4).bits.psrc(0), io.enqIQCtrl(0).bits.psrc(0))
  io.readRf(3) := io.enqIQCtrl(4).bits.psrc(1)
  io.readRf(4) := Mux(io.enqIQCtrl(5).valid, io.enqIQCtrl(5).bits.psrc(0), io.enqIQCtrl(1).bits.psrc(0))
  io.readRf(5) := Mux(io.enqIQCtrl(5).valid, io.enqIQCtrl(5).bits.psrc(1), io.enqIQCtrl(1).bits.psrc(1))
  io.readRf(6) := Mux(io.enqIQCtrl(6).valid, io.enqIQCtrl(6).bits.psrc(0), io.enqIQCtrl(2).bits.psrc(0))
  io.readRf(7) := Mux(io.enqIQCtrl(6).valid, io.enqIQCtrl(6).bits.psrc(1), io.enqIQCtrl(2).bits.psrc(1))

  for (i <- 0 until dpParams.IntDqDeqWidth) {
    io.readState(2*i  ).req := io.fromDq(i).bits.psrc(0)
    io.readState(2*i+1).req := io.fromDq(i).bits.psrc(1)
  }
  val src1Ready = VecInit((0 until 4).map(i => io.readState(i * 2).resp))
  val src2Ready = VecInit((0 until 4).map(i => io.readState(i * 2 + 1).resp))

  /**
    * Part 3: dispatch to reservation stations
    */
  val jmpReady = io.enqIQCtrl(0).ready
  val mduReady = Cat(io.enqIQCtrl.slice(jmpCnt, jmpCnt + mduCnt).map(_.ready)).andR
  // val aluReady = Cat(io.enqIQCtrl.drop(jmpCnt + mduCnt).map(_.ready)).andR
  for (i <- 0 until exuParameters.IntExuCnt) {
    val enq = io.enqIQCtrl(i)
    val deqIndex = if (i < jmpCnt) jmpIndexGen.io.mapping(0).bits else if (i < jmpCnt + mduCnt) mduIndexGen.io.mapping(i - jmpCnt).bits else aluPriority(i - (jmpCnt + mduCnt))
    if (i < jmpCnt) {
      enq.valid := jmpIndexGen.io.mapping(i).valid && !io.enqIQCtrl(4).valid
    }
    else if (i < jmpCnt + mduCnt) {
      enq.valid := mduIndexGen.io.mapping(i - jmpCnt).valid && mduReady && !io.enqIQCtrl(5).valid && !io.enqIQCtrl(6).valid
    }
    else { // alu
      enq.valid := aluCanAccept(aluPriority(i - (jmpCnt + mduCnt))) //aluIndexGen.io.mapping(i - (jmpCnt + mduCnt)).valid //&& aluReady
    }
    enq.bits := io.fromDq(deqIndex).bits

    enq.bits.srcState(0) := src1Ready(deqIndex)
    enq.bits.srcState(1) := src2Ready(deqIndex)
    enq.bits.srcState(2) := DontCare

    XSInfo(enq.fire(), p"pc 0x${Hexadecimal(enq.bits.cf.pc)} with type ${enq.bits.ctrl.fuType} " +
      p"srcState(${enq.bits.srcState(0)} ${enq.bits.srcState(1)}) " +
      p"enters reservation station $i from ${deqIndex}\n")
  }

  /**
    * Part 4: response to dispatch queue
    */
  val mdu2CanOut = !(mduCanAccept(0) && mduCanAccept(1))
  val mdu3CanOut = !(mduCanAccept(0) && mduCanAccept(1) || mduCanAccept(0) && mduCanAccept(2) || mduCanAccept(1) && mduCanAccept(2))
  val aluReadyVec = VecInit(io.enqIQCtrl.drop(jmpCnt + mduCnt).map(_.ready))
  for (i <- 0 until dpParams.IntDqDeqWidth) {
    io.fromDq(i).ready := jmpCanAccept(i) && (if (i == 0) true.B else !Cat(jmpCanAccept.take(i)).orR) && jmpReady && !io.enqIQCtrl(4).valid ||
                          aluCanAccept(i) && aluReadyVec(aluIndex(i)) ||
                          mduCanAccept(i) && (if (i <= 1) true.B else if (i == 2) mdu2CanOut else mdu3CanOut) && mduReady && !io.enqIQCtrl(5).valid && !io.enqIQCtrl(6).valid

    XSInfo(io.fromDq(i).fire(),
      p"pc 0x${Hexadecimal(io.fromDq(i).bits.cf.pc)} leaves Int dispatch queue $i with nroq ${io.fromDq(i).bits.roqIdx}\n")
    XSDebug(io.fromDq(i).valid && !io.fromDq(i).ready,
      p"pc 0x${Hexadecimal(io.fromDq(i).bits.cf.pc)} waits at Int dispatch queue with index $i\n")
  }
  XSError(PopCount(io.fromDq.map(_.fire())) =/= PopCount(io.enqIQCtrl.map(_.fire())), "deq =/= enq\n")

  /**
    * Part 5: send read port index of register file to reservation station
    */
  // io.readPortIndex := readPortIndex
  io.readPortIndex := DontCare
//  val readPortIndexReg = Reg(Vec(exuParameters.IntExuCnt, UInt(log2Ceil(NRIntReadPorts).W)))
//  val uopReg = Reg(Vec(exuParameters.IntExuCnt, new MicroOp))
//  val dataValidRegDebug = Reg(Vec(exuParameters.IntExuCnt, Bool()))
//  for (i <- 0 until exuParameters.IntExuCnt) {
//    readPortIndexReg(i) := readPortIndex(i)
//    uopReg(i) := io.enqIQCtrl(i).bits
//    dataValidRegDebug(i) := io.enqIQCtrl(i).fire()
//
//    io.enqIQData(i) := DontCare
//    io.enqIQData(i).src(0) := Mux(uopReg(i).ctrl.srcType(0) === SrcType.pc,
//      SignExt(uopReg(i).cf.pc, XLEN), io.readRf(readPortIndexReg(i)).data)
//    io.enqIQData(i).src(1) := Mux(uopReg(i).ctrl.srcType(1) === SrcType.imm,
//      uopReg(i).ctrl.imm, io.readRf(readPortIndexReg(i) + 1.U).data)
//
//    XSDebug(dataValidRegDebug(i),
//      p"pc 0x${Hexadecimal(uopReg(i).cf.pc)} reads operands from " +
//        p"(${readPortIndexReg(i)    }, ${uopReg(i).psrc(0)}, ${Hexadecimal(io.enqIQData(i).src(0))}), " +
//        p"(${readPortIndexReg(i)+1.U}, ${uopReg(i).psrc(1)}, ${Hexadecimal(io.enqIQData(i).src(1))})\n")
//  }

  XSPerfAccumulate("in", PopCount(io.fromDq.map(_.valid)))
  XSPerfAccumulate("out", PopCount(io.enqIQCtrl.map(_.fire())))
  XSPerfAccumulate("out_jmp", io.enqIQCtrl(0).fire())
  XSPerfAccumulate("out_mdu0", io.enqIQCtrl(1).fire())
  XSPerfAccumulate("out_mdu1", io.enqIQCtrl(2).fire())
  XSPerfAccumulate("out_alu0", io.enqIQCtrl(3).fire())
  XSPerfAccumulate("out_alu1", io.enqIQCtrl(4).fire())
  XSPerfAccumulate("out_alu2", io.enqIQCtrl(5).fire())
  XSPerfAccumulate("out_alu3", io.enqIQCtrl(6).fire())
  val block_num = PopCount(io.fromDq.map(deq => deq.valid && !deq.ready))
  XSPerfAccumulate("blocked", block_num)
  XSPerfAccumulate("blocked_index", Mux(block_num =/= 0.U, PriorityEncoder(io.fromDq.map(deq => deq.valid && !deq.ready)), 0.U))
  XSPerfAccumulate("jump_deq_try", PopCount(jmpCanAccept))
  XSPerfAccumulate("jump_deq_exceed_limit", Mux(PopCount(jmpCanAccept) >= 1.U, PopCount(jmpCanAccept) - 1.U, 0.U))
  XSPerfAccumulate("mdu_deq_try", PopCount(mduCanAccept))
  XSPerfAccumulate("mdu_deq_exceed_limit", Mux(PopCount(mduCanAccept) >= 2.U, PopCount(mduCanAccept) - 2.U, 0.U))
  XSPerfAccumulate("alu0_blocked_by_alu0", io.enqIQCtrl(3).valid && !io.enqIQCtrl(3).ready)
  XSPerfAccumulate("alu1_blocked_by_alu1", io.enqIQCtrl(4).valid && !io.enqIQCtrl(4).ready)
  XSPerfAccumulate("alu2_blocked_by_alu2", io.enqIQCtrl(5).valid && !io.enqIQCtrl(5).ready)
  XSPerfAccumulate("alu3_blocked_by_alu3", io.enqIQCtrl(6).valid && !io.enqIQCtrl(6).ready)
  XSPerfAccumulate("jump_blocked_by_alu1", jmpIndexGen.io.mapping(0).valid && io.enqIQCtrl(4).valid && io.enqIQCtrl(0).ready)
  XSPerfAccumulate("mdu0_blocked_by_alu", mduIndexGen.io.mapping(0).valid && mduReady && (io.enqIQCtrl(5).valid || io.enqIQCtrl(6).valid))
  XSPerfAccumulate("mdu0_blocked_by_alu2", mduIndexGen.io.mapping(0).valid && mduReady && io.enqIQCtrl(5).valid && !io.enqIQCtrl(6).valid)
  XSPerfAccumulate("mdu0_blocked_by_alu3", mduIndexGen.io.mapping(0).valid && mduReady && !io.enqIQCtrl(5).valid && io.enqIQCtrl(6).valid)
  XSPerfAccumulate("mdu0_blocked_by_mdu1", mduIndexGen.io.mapping(0).valid && io.enqIQCtrl(1).ready && !io.enqIQCtrl(2).ready && !io.enqIQCtrl(5).valid && !io.enqIQCtrl(6).valid)
  XSPerfAccumulate("mdu1_blocked_by_alu", mduIndexGen.io.mapping(1).valid && mduReady && (io.enqIQCtrl(5).valid || io.enqIQCtrl(6).valid))
  XSPerfAccumulate("mdu1_blocked_by_alu2", mduIndexGen.io.mapping(1).valid && mduReady && io.enqIQCtrl(5).valid && !io.enqIQCtrl(6).valid)
  XSPerfAccumulate("mdu1_blocked_by_alu3", mduIndexGen.io.mapping(1).valid && mduReady && !io.enqIQCtrl(5).valid && io.enqIQCtrl(6).valid)
  XSPerfAccumulate("mdu1_blocked_by_mdu0", mduIndexGen.io.mapping(1).valid && !io.enqIQCtrl(1).ready && io.enqIQCtrl(2).ready && !io.enqIQCtrl(5).valid && !io.enqIQCtrl(6).valid)
}
