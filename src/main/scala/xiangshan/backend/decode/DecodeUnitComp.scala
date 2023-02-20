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

package xiangshan.backend.decode

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.Instructions
import freechips.rocketchip.util.uintToBitPat
import utils._
import utility._
import xiangshan.ExceptionNO.illegalInstr
import xiangshan._
import freechips.rocketchip.rocket.Instructions._

import scala.collection.Seq

class DecodeUnitCompIO(implicit p: Parameters) extends XSBundle {
  val enq = new Bundle { val ctrl_flow = Input(new CtrlFlow) }
  val vconfig = Input(new VConfig)
  val isComplex = Input(Vec(DecodeWidth - 1, Bool()))
  val validFromIBuf = Input(Vec(DecodeWidth, Bool()))
  val readyFromRename = Input(Vec(RenameWidth, Bool()))
  val deq = new Bundle {
    val cf_ctrl = Output(Vec(RenameWidth, new CfCtrl))
    val isVset = Output(Bool())
    val readyToIBuf = Output(Vec(DecodeWidth, Bool()))
    val validToRename = Output(Vec(RenameWidth, Bool()))
    val complexNum = Output(UInt(3.W))
  }
  val csrCtrl = Input(new CustomCSRCtrlIO)
}

class DecodeUnitComp(maxNumOfUop : Int)(implicit p : Parameters) extends XSModule with DecodeUnitConstants {
  val io = IO(new DecodeUnitCompIO)
  //input bits
  val ctrl_flow = Wire(new CtrlFlow)
  ctrl_flow := io.enq.ctrl_flow
  //output bits
  val cf_ctrl = Wire(Vec(RenameWidth, new CfCtrl()))
  val validToRename = Wire(Vec(RenameWidth, Bool()))
  val readyToIBuf = Wire(Vec(DecodeWidth, Bool()))
  val complexNum = Wire(UInt(3.W))

  //output of DecodeUnit
  val cf_ctrl_u = Wire(new CfCtrl)
  val isVset_u = Wire(Bool())
  val isComplex_u = Wire(Bool())

  //pre decode
  val simple = Module(new DecodeUnit)
  simple.io.enq.ctrl_flow := ctrl_flow
  simple.io.vconfig := io.vconfig
  simple.io.csrCtrl := io.csrCtrl
  cf_ctrl_u := simple.io.deq.cf_ctrl
  isVset_u := simple.io.deq.isVset
  isComplex_u := simple.io.deq.isComplex

  //Type of uop Div
  val typeOfDiv = cf_ctrl_u.ctrl.uopDivType

  //LMUL
  val lmul = MuxLookup(simple.io.vconfig.vtype.vlmul, 1.U, Array(
    "b001".U -> 2.U,
    "b010".U -> 4.U,
    "b011".U -> 8.U
  ))
  //number of uop
  val numOfUop = MuxLookup(typeOfDiv, 1.U, Array(
    UopDivType.DIR -> 2.U,
    UopDivType.VEC_LMUL -> lmul,
    UopDivType.VEC_MV_LMUL -> (lmul + 1.U)
  ))

  //uop div up to maxNumOfUop
  val csBundle = Wire(Vec(maxNumOfUop, new CfCtrl))
  csBundle.map { case dst => dst := cf_ctrl_u }

  switch(typeOfDiv) {
    is(UopDivType.DIR) {
      when(isVset_u) {
        csBundle(0).ctrl.uopIdx := 0.U
        csBundle(0).ctrl.flushPipe := false.B
        csBundle(0).ctrl.fuOpType := ALUOpType.vsetExchange(cf_ctrl_u.ctrl.fuOpType)
        csBundle(1).ctrl.ldest := 32.U
        csBundle(1).ctrl.flushPipe := FuType.isIntExu(cf_ctrl_u.ctrl.fuType) && ALUOpType.isVsetvli(cf_ctrl_u.ctrl.fuOpType) && cf_ctrl_u.ctrl.lsrc(0).orR
      }
    }
    is(UopDivType.VEC_LMUL) {
      for (i <- 0 until 8) {
        csBundle(i).ctrl.srcType(3) := 0.U
        csBundle(i).ctrl.lsrc(0) := ctrl_flow.instr(19, 15) + i.U
        csBundle(i).ctrl.lsrc(1) := ctrl_flow.instr(24, 20) + i.U
        csBundle(i).ctrl.ldest := ctrl_flow.instr(11, 7) + i.U
        csBundle(i).ctrl.uopIdx := i.U
      }
      csBundle(numOfUop - 1.U).ctrl.uopIdx := "b11111".U
    }
  }

  //uops dispatch
  val normal :: ext :: Nil = Enum(2)
  val stateReg = RegInit(normal)
  val uopRes = RegInit(0.U)

  //readyFromRename Counter
  val readyCounter = PriorityMuxDefault(io.readyFromRename.map(x => !x).zip((0 to (RenameWidth - 1)).map(_.U)), RenameWidth.U)

  switch(stateReg) {
    is(normal) {
      when(!io.validFromIBuf(0)) {
        stateReg := normal
        uopRes := 0.U
      }.elsewhen(numOfUop > readyCounter && !readyCounter){
        stateReg := ext
        uopRes := numOfUop - readyCounter
      }.otherwise {
        stateReg := normal
        uopRes := 0.U
      }
    }
    is(ext) {
      when(!io.validFromIBuf(0)) {
        stateReg := normal
        uopRes := 0.U
      }.elsewhen(uopRes > readyCounter) {
        stateReg := ext
        uopRes := uopRes - readyCounter
      }.otherwise {
        stateReg := normal
        uopRes := 0.U
      }
    }
  }

  for(i <- 0 until RenameWidth) {
    cf_ctrl(i) := MuxCase(csBundle(i), Seq(
      (stateReg === normal) -> csBundle(i),
      (stateReg === ext) -> Mux((i.U + numOfUop -uopRes) < maxNumOfUop.U, csBundle(i.U + numOfUop - uopRes), csBundle(maxNumOfUop - 1))
    ))
  }


  val validSimple = Wire(Vec(DecodeWidth - 1, Bool()))
  validSimple.zip(io.validFromIBuf.drop(1).zip(io.isComplex)).map{ case (dst, (src1, src2)) => dst := src1 && !src2 }
  val notInf = Wire(Vec(DecodeWidth - 1, Bool()))
  notInf.zip(io.validFromIBuf.drop(1).zip(validSimple)).map{ case (dst, (src1, src2)) => dst := !src1 || src2 }
  val notInfVec = Wire(Vec(DecodeWidth, Bool()))
  notInfVec.drop(1).zip(0 until DecodeWidth - 1).map{ case (dst, i) => dst := Cat(notInf.take(i + 1)).andR}
  notInfVec(0) := true.B

  complexNum := 1.U
  validToRename.map{ case dst => dst := false.B }
  readyToIBuf .map{ case dst => dst := false.B }
  switch(stateReg) {
    is(normal) {
      when(!io.validFromIBuf(0)) {
        complexNum := 1.U
        validToRename(0) := false.B
        for (i <- 1 until RenameWidth) {
          validToRename(i) := notInfVec(i - 1) && validSimple(i - 1)
        }
        readyToIBuf(0) := io.readyFromRename(0)
        for(i <- 1 until DecodeWidth) {
          readyToIBuf(i) := notInfVec(i - 1) && validSimple(i - 1) && io.readyFromRename(i)
        }
      }.elsewhen(numOfUop > readyCounter) {
        complexNum := Mux(readyCounter === 0.U, 1.U, readyCounter)
        for (i <- 0 until RenameWidth) {
          validToRename(i) := Mux(readyCounter > i.U, true.B, false.B)
        }
        readyToIBuf.map{ case dst => dst := false.B }
      }.otherwise {
        complexNum := numOfUop
        for (i <- 0 until RenameWidth) {
          validToRename(i) := Mux(complexNum > i.U, true.B, validSimple(i.U - complexNum) && notInfVec(i.U - complexNum) && io.readyFromRename(i))
        }
        readyToIBuf(0) := true.B
        for (i <- 1 until DecodeWidth) {
          readyToIBuf(i) := Mux(RenameWidth.U - complexNum >= i.U, notInfVec(i - 1) && validSimple(i - 1) && io.readyFromRename(i), false.B)
        }
      }
    }
    is(ext) {
      when(!io.validFromIBuf(0)) {
        complexNum := 1.U
        validToRename.map{ case dst => dst := false.B }
        readyToIBuf.map{ case dst => dst := true.B }
      }.elsewhen(uopRes > readyCounter) {
        complexNum := Mux(readyCounter === 0.U, 1.U, readyCounter)
        for (i <- 0 until RenameWidth) {
          validToRename(i) := Mux(readyCounter > i.U, true.B, false.B)
        }
        readyToIBuf.map{ case dst => dst := false.B }
      }.otherwise {
        complexNum := uopRes
        for (i <- 0 until RenameWidth) {
          validToRename(i) := Mux(complexNum > i.U, true.B, validSimple(i.U - complexNum) && notInfVec(i.U - complexNum) && io.readyFromRename(i))
        }
        readyToIBuf(0) := true.B
        for (i <- 1 until DecodeWidth) {
          readyToIBuf(i) := Mux(RenameWidth.U - complexNum >= i.U, notInfVec(i - 1) && validSimple(i - 1) && io.readyFromRename(i), false.B)
        }
      }
    }
  }

  io.deq.cf_ctrl := cf_ctrl
  io.deq.isVset := isVset_u
  io.deq.complexNum := complexNum
  io.deq.validToRename := validToRename
  io.deq.readyToIBuf := readyToIBuf

}

