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

class Dispatch2Fp(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val fromDq = Flipped(Vec(dpParams.FpDqDeqWidth, DecoupledIO(new MicroOp)))
    val readRf = Vec(NRFpReadPorts - exuParameters.StuCnt, Output(UInt(PhyRegIdxWidth.W)))
    val readState = Vec(NRFpReadPorts - exuParameters.StuCnt, Flipped(new BusyTableReadIO))
    val enqIQCtrl = Vec(exuParameters.FpExuCnt, DecoupledIO(new MicroOp))
    val readPortIndex = Vec(exuParameters.FpExuCnt, Output(UInt(log2Ceil((NRFpReadPorts - exuParameters.StuCnt) / 3).W)))
  })

  io.enqIQCtrl <> DontCare
  io.fromDq <> io.enqIQCtrl.take(4)
  io.enqIQCtrl(4).valid := false.B
  io.enqIQCtrl(5).valid := false.B

  for (i <- 0 until 4) {
    io.readRf(3*i) := io.enqIQCtrl(i).bits.psrc(0)
    io.readRf(3*i+1) := io.enqIQCtrl(i).bits.psrc(1)
    io.readRf(3*i+2) := io.enqIQCtrl(i).bits.psrc(2)
  }

  /**
    * Part 1: generate indexes for reservation stations
    */
  // val fmacIndexGen = Module(new IndexMapping(dpParams.FpDqDeqWidth, exuParameters.FmacCnt, true))
//  val fmacCanAccept = VecInit(io.fromDq.map(deq => deq.valid && FuType.fmacCanAccept(deq.bits.ctrl.fuType)))
//  val (fmacPriority, fmacIndex) = PriorityGen((0 until exuParameters.FmacCnt).map(i => 0.U))
  // fmacIndexGen.io.validBits := fmacCanAccept
  // fmacIndexGen.io.priority := fmacPriority

//  val fmiscIndexGen = Module(new IndexMapping(dpParams.FpDqDeqWidth, exuParameters.FmiscCnt, true))
//  val fmiscCanAccept = VecInit(io.fromDq.map(deq => deq.valid && FuType.fmiscCanAccept(deq.bits.ctrl.fuType)))
//  val (fmiscPriority, _) = PriorityGen((0 until exuParameters.FmiscCnt).map(i => 0.U))
//  fmiscIndexGen.io.validBits := fmiscCanAccept
//  fmiscIndexGen.io.priority := fmiscPriority

  // val allIndexGen = Seq(fmacIndexGen, fmiscIndexGen)
  // val validVec = allIndexGen.map(_.io.mapping.map(_.valid)).reduceLeft(_ ++ _)
  // val indexVec = allIndexGen.map(_.io.mapping.map(_.bits)).reduceLeft(_ ++ _)
  // for (i <- validVec.indices) {
    // XSDebug(p"mapping $i: valid ${validVec(i)} index ${indexVec(i)}\n")
  // }

  /**
    * Part 2: assign regfile read ports
    */
  // val fpStaticIndex = Seq(0, 1, 2, 3)
  // val fpDynamicIndex = Seq(4, 5)
  // val fpStaticMappedValid = fpStaticIndex.map(i => validVec(i))
  // val fpDynamicMappedValid = fpDynamicIndex.map(i => validVec(i))
  // val (fpReadPortSrc, fpDynamicExuSrc) = RegfileReadPortGen(fpStaticMappedValid, fpDynamicMappedValid)
  // val fpStaticMapped = fpStaticIndex.map(i => indexVec(i))
  // val fpDynamicMapped = fpDynamicIndex.map(i => indexVec(i))
  // for (i <- fpStaticIndex.indices) {
  //   val index = WireInit(VecInit(fpStaticMapped(i) +: fpDynamicMapped))
  //   io.readRf(3*i  ) := io.fromDq(index(fpReadPortSrc(i))).bits.psrc(0)
  //   io.readRf(3*i+1) := io.fromDq(index(fpReadPortSrc(i))).bits.psrc(1)
  //   io.readRf(3*i+2) := io.fromDq(index(fpReadPortSrc(i))).bits.psrc(2)
  // }
  // val readPortIndex = Wire(Vec(exuParameters.FpExuCnt, UInt(2.W)))
  // fpStaticIndex.zipWithIndex.map({case (index, i) => readPortIndex(index) := i.U})
  // fpDynamicIndex.zipWithIndex.map({case (index, i) => readPortIndex(index) := fpDynamicExuSrc(i)})

  for (i <- 0 until dpParams.IntDqDeqWidth) {
    io.readState(3*i  ).req := io.fromDq(i).bits.psrc(0)
    io.readState(3*i+1).req := io.fromDq(i).bits.psrc(1)
    io.readState(3*i+2).req := io.fromDq(i).bits.psrc(2)
    io.enqIQCtrl(i).bits.srcState(0) := io.readState(3*i).resp
    io.enqIQCtrl(i).bits.srcState(1) := io.readState(3*i+1).resp
    io.enqIQCtrl(i).bits.srcState(2) := io.readState(3*i+2).resp
  }
//  io.readRf(0)  := io.enqIQCtrl(0).bits.psrc(0)
//  io.readRf(1)  := io.enqIQCtrl(0).bits.psrc(1)
//  io.readRf(2)  := io.enqIQCtrl(0).bits.psrc(2)
//  io.readRf(3)  := io.enqIQCtrl(1).bits.psrc(0)
//  io.readRf(4)  := io.enqIQCtrl(1).bits.psrc(1)
//  io.readRf(5)  := io.enqIQCtrl(1).bits.psrc(2)
//  io.readRf(6)  := Mux(io.enqIQCtrl(2).valid, io.enqIQCtrl(2).bits.psrc(0), io.enqIQCtrl(4).bits.psrc(0))
//  io.readRf(7)  := Mux(io.enqIQCtrl(2).valid, io.enqIQCtrl(2).bits.psrc(1), io.enqIQCtrl(4).bits.psrc(1))
//  io.readRf(8)  := Mux(io.enqIQCtrl(2).valid, io.enqIQCtrl(2).bits.psrc(2), io.enqIQCtrl(4).bits.psrc(2))
//  io.readRf(9)  := Mux(io.enqIQCtrl(3).valid, io.enqIQCtrl(3).bits.psrc(0), io.enqIQCtrl(5).bits.psrc(0))
//  io.readRf(10) := Mux(io.enqIQCtrl(3).valid, io.enqIQCtrl(3).bits.psrc(1), io.enqIQCtrl(5).bits.psrc(1))
//  io.readRf(11) := Mux(io.enqIQCtrl(3).valid, io.enqIQCtrl(3).bits.psrc(2), io.enqIQCtrl(5).bits.psrc(2))

  /**
    * Part 3: dispatch to reservation stations
    */
  // val fmacReady = Cat(io.enqIQCtrl.take(exuParameters.FmacCnt).map(_.ready)).andR
//  val fmiscReady = Cat(io.enqIQCtrl.drop(exuParameters.FmacCnt).map(_.ready)).andR
//  for (i <- 0 until exuParameters.FpExuCnt) {
//    val enq = io.enqIQCtrl(i)
//    val deqIndex = if (i < exuParameters.FmacCnt) fmacPriority(i) else fmiscIndexGen.io.mapping(i-exuParameters.FmacCnt).bits
//    if (i < exuParameters.FmacCnt) {
//      enq.valid := fmacCanAccept(fmacPriority(i))//fmacIndexGen.io.mapping(i).valid && fmacReady
//    }
//    else {
//      enq.valid := fmiscIndexGen.io.mapping(i - exuParameters.FmacCnt).valid && fmiscReady && !io.enqIQCtrl(2).valid && !io.enqIQCtrl(3).valid
//    }
//    enq.bits := io.fromDq(deqIndex).bits
//
//    val src1Ready = VecInit((0 until 4).map(i => io.readState(i * 3).resp))
//    val src2Ready = VecInit((0 until 4).map(i => io.readState(i * 3 + 1).resp))
//    val src3Ready = VecInit((0 until 4).map(i => io.readState(i * 3 + 2).resp))
//    enq.bits.srcState(0) := src1Ready(deqIndex)
//    enq.bits.srcState(1) := src2Ready(deqIndex)
//    enq.bits.srcState(2) := src3Ready(deqIndex)
//
//    XSInfo(enq.fire(), p"pc 0x${Hexadecimal(enq.bits.cf.pc)} with type ${enq.bits.ctrl.fuType} " +
//      p"srcState(${enq.bits.srcState(0)} ${enq.bits.srcState(1)} ${enq.bits.srcState(2)}) " +
//      p"enters reservation station $i from ${deqIndex}\n")
//  }

  /**
    * Part 4: response to dispatch queue
    */
//  val fmisc2CanOut = !(fmiscCanAccept(0) && fmiscCanAccept(1))
//  val fmisc3CanOut = !(fmiscCanAccept(0) && fmiscCanAccept(1) || fmiscCanAccept(0) && fmiscCanAccept(2) || fmiscCanAccept(1) && fmiscCanAccept(2))
//  val fmacReadyVec = VecInit(io.enqIQCtrl.take(4).map(_.ready))
//  for (i <- 0 until dpParams.FpDqDeqWidth) {
//    io.fromDq(i).ready := fmacCanAccept(i) && fmacReadyVec(fmacIndex(i)) ||
//                          fmiscCanAccept(i) && (if (i <= 1) true.B else if (i == 2) fmisc2CanOut else fmisc3CanOut) && fmiscReady && !io.enqIQCtrl(2).valid && !io.enqIQCtrl(3).valid
//
//    XSInfo(io.fromDq(i).fire(),
//      p"pc 0x${Hexadecimal(io.fromDq(i).bits.cf.pc)} leaves Fp dispatch queue $i with nroq ${io.fromDq(i).bits.roqIdx}\n")
//    XSDebug(io.fromDq(i).valid && !io.fromDq(i).ready,
//      p"pc 0x${Hexadecimal(io.fromDq(i).bits.cf.pc)} waits at Fp dispatch queue with index $i\n")
//  }
  XSError(PopCount(io.fromDq.map(_.fire())) =/= PopCount(io.enqIQCtrl.map(_.fire())), "deq =/= enq\n")

  /**
    * Part 5: send read port index of register file to reservation station
    */
  // io.readPortIndex := readPortIndex
  io.readPortIndex := DontCare
//  val readPortIndexReg = Reg(Vec(exuParameters.FpExuCnt, UInt(log2Ceil(NRFpReadPorts - exuParameters.StuCnt).W)))
//  val uopReg = Reg(Vec(exuParameters.FpExuCnt, new MicroOp))
//  val dataValidRegDebug = Reg(Vec(exuParameters.FpExuCnt, Bool()))
//  for (i <- 0 until exuParameters.FpExuCnt) {
//    readPortIndexReg(i) := readPortIndex(i)
//    uopReg(i) := io.enqIQCtrl(i).bits
//    dataValidRegDebug(i) := io.enqIQCtrl(i).fire()
//
//    io.enqIQData(i) := DontCare
//    io.enqIQData(i).src(0) := io.readRf(readPortIndexReg(i)).data
//    io.enqIQData(i).src(1) := io.readRf(readPortIndexReg(i) + 1.U).data
//    io.enqIQData(i).src(2) := io.readRf(readPortIndexReg(i) + 2.U).data
//
//    XSDebug(dataValidRegDebug(i),
//      p"pc 0x${Hexadecimal(uopReg(i).cf.pc)} reads operands from " +
//        p"(${readPortIndexReg(i)    }, ${uopReg(i).psrc(0)}, ${Hexadecimal(io.enqIQData(i).src(0))}), " +
//        p"(${readPortIndexReg(i)+1.U}, ${uopReg(i).psrc(1)}, ${Hexadecimal(io.enqIQData(i).src(1))}), " +
//        p"(${readPortIndexReg(i)+2.U}, ${uopReg(i).psrc(2)}, ${Hexadecimal(io.enqIQData(i).src(2))})\n")
//  }

//  XSPerfAccumulate("in", PopCount(io.fromDq.map(_.valid)))
//  XSPerfAccumulate("out", PopCount(io.enqIQCtrl.map(_.fire())))
//  XSPerfAccumulate("out_fmac0", io.enqIQCtrl(0).fire())
//  XSPerfAccumulate("out_fmac1", io.enqIQCtrl(1).fire())
//  XSPerfAccumulate("out_fmac2", io.enqIQCtrl(2).fire())
//  XSPerfAccumulate("out_fmac3", io.enqIQCtrl(3).fire())
//  XSPerfAccumulate("out_fmisc0", io.enqIQCtrl(4).fire())
//  XSPerfAccumulate("out_fmisc1", io.enqIQCtrl(5).fire())
//  val block_num = PopCount(io.fromDq.map(deq => deq.valid && !deq.ready))
//  XSPerfAccumulate("blocked", block_num)
//  XSPerfAccumulate("blocked_index", Mux(block_num =/= 0.U, PriorityEncoder(io.fromDq.map(deq => deq.valid && !deq.ready)), 0.U))
//  XSPerfAccumulate("misc_deq", PopCount(fmiscCanAccept))
//  XSPerfAccumulate("misc_deq_exceed_limit", Mux(PopCount(fmiscCanAccept) >= 2.U, PopCount(fmiscCanAccept) - 2.U, 0.U))
//  XSPerfAccumulate("mac0_blocked_by_mac0", io.enqIQCtrl(0).valid && !io.enqIQCtrl(0).ready)
//  XSPerfAccumulate("mac1_blocked_by_mac1", io.enqIQCtrl(1).valid && !io.enqIQCtrl(1).ready)
//  XSPerfAccumulate("mac2_blocked_by_mac2", io.enqIQCtrl(2).valid && !io.enqIQCtrl(2).ready)
//  XSPerfAccumulate("mac3_blocked_by_mac3", io.enqIQCtrl(3).valid && !io.enqIQCtrl(3).ready)
//  XSPerfAccumulate("misc0_blocked_by_mac", fmiscIndexGen.io.mapping(0).valid && fmiscReady && (io.enqIQCtrl(2).valid || io.enqIQCtrl(3).valid))
//  XSPerfAccumulate("misc0_blocked_by_mac2", fmiscIndexGen.io.mapping(0).valid && fmiscReady && io.enqIQCtrl(2).valid && !io.enqIQCtrl(3).valid)
//  XSPerfAccumulate("misc0_blocked_by_mac3", fmiscIndexGen.io.mapping(0).valid && fmiscReady && !io.enqIQCtrl(2).valid && io.enqIQCtrl(3).valid)
//  XSPerfAccumulate("misc0_blocked_by_misc1", fmiscIndexGen.io.mapping(0).valid && io.enqIQCtrl(4).ready && !io.enqIQCtrl(5).ready && !io.enqIQCtrl(2).valid && !io.enqIQCtrl(3).valid)
//  XSPerfAccumulate("misc1_blocked_by_mac", fmiscIndexGen.io.mapping(1).valid && fmiscReady && (io.enqIQCtrl(2).valid || io.enqIQCtrl(3).valid))
//  XSPerfAccumulate("misc1_blocked_by_mac2", fmiscIndexGen.io.mapping(1).valid && fmiscReady && io.enqIQCtrl(2).valid && !io.enqIQCtrl(3).valid)
//  XSPerfAccumulate("misc1_blocked_by_mac3", fmiscIndexGen.io.mapping(1).valid && fmiscReady && !io.enqIQCtrl(2).valid && io.enqIQCtrl(3).valid)
//  XSPerfAccumulate("misc1_blocked_by_misc0", fmiscIndexGen.io.mapping(1).valid && !io.enqIQCtrl(4).ready && io.enqIQCtrl(5).ready && !io.enqIQCtrl(2).valid && !io.enqIQCtrl(3).valid)
}
