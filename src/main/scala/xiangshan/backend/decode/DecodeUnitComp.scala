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
  val isComplex = Input(Vec(DecodeWidth, Bool()))
  val validFromIBuf = Input(Vec(DecodeWidth, Bool()))
  val readyFromRename = Input(Vec(DecodeWidth, Bool()))
  val deq = new Bundle {
    val cf_ctrl = Output(Vec(RenameWidth, new CfCtrl))
    val isVset = Output(Bool())
    val readyToIBuf = Output(Vec(RenameWidth, Bool()))
    val validToRename = Output(Vec(RenameWidth, Bool()))
    val complexNum = Output(UInt(3.W))
  }
  val csrCtrl = Input(new CustomCSRCtrlIO)
}

class DecodeUnitComp(implicit p : Parameters) extends XSModule with DecodeUnitConstants {
  val io = IO(new DecodeUnitCompIO)

  //input bits
  val ctrl_flow = Wire(new CtrlFlow)
  ctrl_flow := io.enq.ctrl_flow

  //output for sca
  val cf_ctrl_sca = Wire(new CfCtrl)
  val isVset_sca = Wire(Bool())
  val readyToIBuf_sca = Wire(Vec(RenameWidth, Bool()))
  val validToRename_sca = Wire(Vec(RenameWidth, Bool()))
  val complexNum_sca = Wire(UInt(3.W))
  //output for vec
  val cf_ctrl_vec = Wire(Vec(RenameWidth, new CfCtrl))
  val isVset_vec = Wire(Bool())
  val readyToIBuf_vec = Wire(Vec(RenameWidth, Bool()))
  val validToRename_vec = Wire(Vec(RenameWidth, Bool()))
  val complexNum_vec = Wire(UInt(3.W))
  //ins of simple
  val simple = Module(new DecodeUnit)
  simple.io.enq.ctrl_flow := ctrl_flow
  simple.io.vconfig := io.vconfig
  simple.io.csrCtrl := io.csrCtrl
  cf_ctrl_sca := simple.io.deq.cf_ctrl
  isVset_sca := simple.io.deq.isVset

  val validSimple = Wire(Vec(6, Bool()))
  validSimple.zip(io.validFromIBuf.zip(io.isComplex)).map { case (dst, (src1, src2)) => dst := src1 && !src2 }

  readyToIBuf_sca.zip((Seq(io.readyFromRename(0)) :+
    validSimple(1) :+
    ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) :+
    ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && validSimple(3)) :+
    ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && validSimple(4)) :+
    ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && (!io.validFromIBuf(4) || validSimple(4)) && validSimple(5))).zip(io.readyFromRename)).map { case (dst, (src1, src2)) => dst := src1 && src2 }
  validToRename_sca.zip(Seq(io.validFromIBuf(0)) :+
    validSimple(1) :+
    ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) :+
    ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && validSimple(3)) :+
    ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && validSimple(4)) :+
    ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && (!io.validFromIBuf(4) || validSimple(4)) && validSimple(5))).map { case (dst, src) => dst := src }
  complexNum_sca := 1.U

  //numOfUop TODO
  val numOfUop = Wire(UInt(4.W))
  when(ctrl_flow.instr(6, 0) === "b1010111".U && ctrl_flow.instr(14, 12) === "b111".U) {
    numOfUop := 2.U
  }.elsewhen(io.vconfig.vtype.vlmul === "b001".U) {
    numOfUop := 2.U
  }.elsewhen(io.vconfig.vtype.vlmul === "b010".U) {
    numOfUop := 4.U
  }.elsewhen(io.vconfig.vtype.vlmul === "b011".U) {
    numOfUop := 8.U
  } otherwise {
    numOfUop := 1.U
  }

  //is Vec or not TODO
  val isVec = Wire(Bool())
  //Opcode == 1010111
  when(ctrl_flow.instr(6, 0) === "b1010111".U) {
    isVec := true.B
  }.elsewhen(ctrl_flow.instr(6, 0) === "b0000111".U || ctrl_flow.instr(6, 0) === "b0100111".U) {
    when(ctrl_flow.instr(14, 12) > 0.U && ctrl_flow.instr(14, 12) < 5.U) {
      isVec := false.B
    }.otherwise {
      isVec := true.B
    }
  }.otherwise {
    isVec := false.B
  }

  //uop div up to 9
  val csBundle = Wire(Vec(9, new CfCtrl))

  //uop default value
  csBundle.map{ case x => x := cf_ctrl_sca}

  //uop gen
  switch(ctrl_flow.instr(6, 0)) {
    is("b1010111".U) {
      switch(ctrl_flow.instr(14, 12)) {
        is("b000".U) {
          switch(ctrl_flow.instr(31, 26)) {
            is("b000000".U) {
              for (i <- 0 until 9) {

                csBundle(i).ctrl.srcType(3) := 0.U
                csBundle(i).ctrl.lsrc(0) := ctrl_flow.instr(19, 15) + i.U
                csBundle(i).ctrl.lsrc(1) := ctrl_flow.instr(24, 20) + i.U
                csBundle(i).ctrl.ldest := ctrl_flow.instr(11, 7) + i.U
                csBundle(i).ctrl.uopIdx := i.U

              }

              csBundle(numOfUop - 1.U).ctrl.uopIdx := "b11111".U

            }

          }
        }
        is("b011".U) {
          switch(ctrl_flow.instr(31, 26)) {
            is("b000000".U) {
              for (i <- 0 until 9) {

                csBundle(i).ctrl.srcType(3) := 0.U
                csBundle(i).ctrl.lsrc(0) := ctrl_flow.instr(19, 15) + i.U
                csBundle(i).ctrl.lsrc(1) := ctrl_flow.instr(24, 20) + i.U
                csBundle(i).ctrl.ldest := ctrl_flow.instr(11, 7) + i.U
                csBundle(i).ctrl.uopIdx := i.U

              }

              csBundle(numOfUop - 1.U).ctrl.uopIdx := "b11111".U

            }

          }
        }
        is("b111".U) {
          csBundle(0).ctrl.uopIdx := 0.U
          csBundle(0).ctrl.flushPipe := false.B
          csBundle(0).ctrl.fuOpType := ALUOpType.vsetExchange(cf_ctrl_sca.ctrl.fuOpType)

          csBundle(1).ctrl.ldest := 32.U
          //isVsetvli && rs1NotZero
          csBundle(1).ctrl.flushPipe := FuType.isIntExu(cf_ctrl_sca.ctrl.fuType) && ALUOpType.isVsetvli(cf_ctrl_sca.ctrl.fuOpType) && cf_ctrl_sca.ctrl.lsrc(0).orR
        }
      }
    }

  }

  //uop dispatch
  val normal :: extension6 :: Nil = Enum(2)
  val stateReg = RegInit(normal)

  val readyCounter = Wire(UInt(3.W))
  when(!io.readyFromRename(0)) {
    readyCounter := 0.U
  }.elsewhen(!io.readyFromRename(1)) {
    readyCounter := 1.U
  }.elsewhen(!io.readyFromRename(2)) {
    readyCounter := 2.U
  }.elsewhen(!io.readyFromRename(3)) {
    readyCounter := 3.U
  }.elsewhen(!io.readyFromRename(4)) {
    readyCounter := 4.U
  }.elsewhen(!io.readyFromRename(5)) {
    readyCounter := 5.U
  }.otherwise {
    readyCounter := 6.U
  }

  switch(stateReg) {
    is(normal) {
      when(!io.validFromIBuf(0)) {
        stateReg := normal
      }.elsewhen(numOfUop > 6.U && readyCounter === 6.U) {
        stateReg := extension6
      }.otherwise {
        stateReg := normal
      }
    }
    is(extension6) {
      when(!io.validFromIBuf(0)) {
        stateReg := normal
      }.elsewhen(readyCounter >= (numOfUop - 6.U)) {
        stateReg := normal
      }.otherwise {
        stateReg := extension6
      }
    }
  }

  //vec default out
  cf_ctrl_vec.map { case x => x := cf_ctrl_sca }
  isVset_vec := isVset_sca
  complexNum_vec := 1.U
  validToRename_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
  readyToIBuf_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }

  when(stateReg === normal) {
    when(!io.validFromIBuf(0)) {
      cf_ctrl_vec.map { case x => x := cf_ctrl_sca }
      isVset_vec := false.B
      complexNum_vec := 1.U
      validToRename_vec.zip(Seq(false.B) :+
        validSimple(1) :+
        ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) :+
        ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && validSimple(3)) :+
        ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && validSimple(4)) :+
        ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && (!io.validFromIBuf(4) || validSimple(4)) && validSimple(5))).map { case (dst, src) => dst := src }
      readyToIBuf_vec.zip(Seq(true.B) :+
        validSimple(1) :+
        ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) :+
        ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && validSimple(3)) :+
        ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && validSimple(4)) :+
        ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && (!io.validFromIBuf(4) || validSimple(4)) && validSimple(5))).map { case (dst, src) => dst := src }
    }.elsewhen(readyCounter === 6.U) {
      switch(numOfUop) {
        is(9.U) {
          for (i <- 0 until RenameWidth) {
            cf_ctrl_vec(i) := csBundle(i)
          }
          isVset_vec := isVset_sca
          complexNum_vec := 6.U
          validToRename_vec.zip(Seq.fill(6)(true.B)).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
        }
        is(8.U) {
          for (i <- 0 until RenameWidth) {
            cf_ctrl_vec(i) := csBundle(i)
          }
          isVset_vec := isVset_sca
          complexNum_vec := 6.U
          validToRename_vec.zip(Seq.fill(6)(true.B)).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
        }
        is(4.U) {
          for (i <- 0 until 4) {
            cf_ctrl_vec(i) := csBundle(i)
          }
          isVset_vec := isVset_sca
          complexNum_vec := 4.U
          validToRename_vec.zip(Seq.fill(4)(true.B) :+ validSimple(1) :+ ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2))).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(true.B +:
            validSimple(1) +:
            ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) +:
            Seq.fill(3)(false.B)).map { case (dst, src) => dst := src }
        }
        is(2.U) {
          for (i <- 0 until 2) {
            cf_ctrl_vec(i) := csBundle(i)
          }
          isVset_vec := isVset_sca
          complexNum_vec := 2.U
          validToRename_vec.zip(Seq.fill(2)(true.B) :+
            validSimple(1) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && validSimple(3)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && validSimple(4))).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(true.B +:
            validSimple(1) +:
            ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) +:
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && validSimple(3)) +:
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && validSimple(4)) +:
            Seq(false.B)).map { case (dst, src) => dst := src }
        }
        is(1.U) {
          for (i <- 0 until 1) {
            cf_ctrl_vec(i) := csBundle(i)
          }
          isVset_vec := isVset_sca
          complexNum_vec := 1.U
          validToRename_vec.zip(Seq(true.B) :+
            validSimple(1) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && validSimple(3)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && validSimple(4)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && (!io.validFromIBuf(4) || validSimple(4)) && validSimple(5))).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(Seq(true.B) :+
            validSimple(1) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && validSimple(3)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && validSimple(4)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && (!io.validFromIBuf(4) || validSimple(4)) && validSimple(5))).map { case (dst, src) => dst := src }
        }
      }
    }.elsewhen(readyCounter === 5.U) {
      switch(numOfUop) {
        is(9.U) {
          cf_ctrl_vec.map { case x => x := cf_ctrl_sca }
          isVset_vec := isVset_sca
          complexNum_vec := 1.U
          validToRename_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
        }
        is(8.U) {
          cf_ctrl_vec.map { case x => x := cf_ctrl_sca }
          isVset_vec := isVset_sca
          complexNum_vec := 1.U
          validToRename_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
        }
        is(4.U) {
          for (i <- 0 until 4) {
            cf_ctrl_vec(i) := csBundle(i)
          }
          isVset_vec := isVset_sca
          complexNum_vec := 4.U
          validToRename_vec.zip(Seq.fill(4)(true.B) :+ validSimple(1) :+ ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2))).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(true.B +:
            validSimple(1) +:
            Seq.fill(4)(false.B)).map { case (dst, src) => dst := src }
        }
        is(2.U) {
          for (i <- 0 until 2) {
            cf_ctrl_vec(i) := csBundle(i)
          }
          isVset_vec := isVset_sca
          complexNum_vec := 2.U
          validToRename_vec.zip(Seq.fill(2)(true.B) :+
            validSimple(1) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && validSimple(3)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && validSimple(4))).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(true.B +:
            validSimple(1) +:
            ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) +:
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && validSimple(3)) +:
            Seq.fill(2)(false.B)).map { case (dst, src) => dst := src }
        }
        is(1.U) {
          for (i <- 0 until 1) {
            cf_ctrl_vec(i) := csBundle(i)
          }
          isVset_vec := isVset_sca
          complexNum_vec := 1.U
          validToRename_vec.zip(Seq(true.B) :+
            validSimple(1) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && validSimple(3)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && validSimple(4)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && (!io.validFromIBuf(4) || validSimple(4)) && validSimple(5))).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(Seq(true.B) :+
            validSimple(1) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && validSimple(3)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && validSimple(4)) :+
            false.B).map { case (dst, src) => dst := src }
        }
      }
    }.elsewhen(readyCounter === 4.U) {
      switch(numOfUop) {
        is(9.U) {
          cf_ctrl_vec.map { case x => x := cf_ctrl_sca }
          isVset_vec := isVset_sca
          complexNum_vec := 1.U
          validToRename_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
        }
        is(8.U) {
          cf_ctrl_vec.map { case x => x := cf_ctrl_sca }
          isVset_vec := isVset_sca
          complexNum_vec := 1.U
          validToRename_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
        }
        is(4.U) {
          for (i <- 0 until 4) {
            cf_ctrl_vec(i) := csBundle(i)
          }
          isVset_vec := isVset_sca
          complexNum_vec := 4.U
          validToRename_vec.zip(Seq.fill(4)(true.B) :+ validSimple(1) :+ ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2))).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(true.B +:
            Seq.fill(5)(false.B)).map { case (dst, src) => dst := src }
        }
        is(2.U) {
          for (i <- 0 until 2) {
            cf_ctrl_vec(i) := csBundle(i)
          }
          isVset_vec := isVset_sca
          complexNum_vec := 2.U
          validToRename_vec.zip(Seq.fill(2)(true.B) :+
            validSimple(1) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && validSimple(3)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && validSimple(4))).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(true.B +:
            validSimple(1) +:
            ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) +:
            Seq.fill(3)(false.B)).map { case (dst, src) => dst := src }
        }
        is(1.U) {
          for (i <- 0 until 1) {
            cf_ctrl_vec(i) := csBundle(i)
          }
          isVset_vec := isVset_sca
          complexNum_vec := 1.U
          validToRename_vec.zip(Seq(true.B) :+
            validSimple(1) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && validSimple(3)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && validSimple(4)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && (!io.validFromIBuf(4) || validSimple(4)) && validSimple(5))).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(Seq(true.B) :+
            validSimple(1) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && validSimple(3)) :+
            false.B :+
            false.B).map { case (dst, src) => dst := src }
        }
      }
    }.elsewhen(readyCounter === 3.U) {
      switch(numOfUop) {
        is(9.U) {
          cf_ctrl_vec.map { case x => x := cf_ctrl_sca }
          isVset_vec := isVset_sca
          complexNum_vec := 1.U
          validToRename_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
        }
        is(8.U) {
          cf_ctrl_vec.map { case x => x := cf_ctrl_sca }
          isVset_vec := isVset_sca
          complexNum_vec := 1.U
          validToRename_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
        }
        is(4.U) {
          cf_ctrl_vec.map { case x => x := cf_ctrl_sca }
          isVset_vec := isVset_sca
          complexNum_vec := 1.U
          validToRename_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
        }
        is(2.U) {
          for (i <- 0 until 2) {
            cf_ctrl_vec(i) := csBundle(i)
          }
          isVset_vec := isVset_sca
          complexNum_vec := 2.U
          validToRename_vec.zip(Seq.fill(2)(true.B) :+
            validSimple(1) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && validSimple(3)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && validSimple(4))).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(true.B +:
            validSimple(1) +:
            Seq.fill(4)(false.B)).map { case (dst, src) => dst := src }
        }
        is(1.U) {
          for (i <- 0 until 1) {
            cf_ctrl_vec(i) := csBundle(i)
          }
          isVset_vec := isVset_sca
          complexNum_vec := 1.U
          validToRename_vec.zip(Seq(true.B) :+
            validSimple(1) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && validSimple(3)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && validSimple(4)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && (!io.validFromIBuf(4) || validSimple(4)) && validSimple(5))).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(true.B +:
            validSimple(1) +:
            ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) +:
            Seq.fill(3)(false.B)).map { case (dst, src) => dst := src }
        }
      }
    }.elsewhen(readyCounter === 2.U) {
      switch(numOfUop) {
        is(9.U) {
          cf_ctrl_vec.map { case x => x := cf_ctrl_sca }
          isVset_vec := isVset_sca
          complexNum_vec := 1.U
          validToRename_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
        }
        is(8.U) {
          cf_ctrl_vec.map { case x => x := cf_ctrl_sca }
          isVset_vec := isVset_sca
          complexNum_vec := 1.U
          validToRename_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
        }
        is(4.U) {
          cf_ctrl_vec.map { case x => x := cf_ctrl_sca }
          isVset_vec := isVset_sca
          complexNum_vec := 1.U
          validToRename_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
        }
        is(2.U) {
          for (i <- 0 until 2) {
            cf_ctrl_vec(i) := csBundle(i)
          }
          isVset_vec := isVset_sca
          complexNum_vec := 2.U
          validToRename_vec.zip(Seq.fill(2)(true.B) :+
            validSimple(1) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && validSimple(3)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && validSimple(4))).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(true.B +:
            Seq.fill(5)(false.B)).map { case (dst, src) => dst := src }
        }
        is(1.U) {
          for (i <- 0 until 1) {
            cf_ctrl_vec(i) := csBundle(i)
          }
          isVset_vec := isVset_sca
          complexNum_vec := 1.U
          validToRename_vec.zip(Seq(true.B) :+
            validSimple(1) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && validSimple(3)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && validSimple(4)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && (!io.validFromIBuf(4) || validSimple(4)) && validSimple(5))).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(true.B +:
            validSimple(1) +:
            Seq.fill(4)(false.B)).map { case (dst, src) => dst := src }
        }
      }
    }.elsewhen(readyCounter === 1.U) {
      switch(numOfUop) {
        is(9.U) {
          cf_ctrl_vec.map { case x => x := cf_ctrl_sca }
          isVset_vec := isVset_sca
          complexNum_vec := 1.U
          validToRename_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
        }
        is(8.U) {
          cf_ctrl_vec.map { case x => x := cf_ctrl_sca }
          isVset_vec := isVset_sca
          complexNum_vec := 1.U
          validToRename_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
        }
        is(4.U) {
          cf_ctrl_vec.map { case x => x := cf_ctrl_sca }
          isVset_vec := isVset_sca
          complexNum_vec := 1.U
          validToRename_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
        }
        is(2.U) {
          cf_ctrl_vec.map { case x => x := cf_ctrl_sca }
          isVset_vec := false.B
          complexNum_vec := 1.U
          validToRename_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
        }
        is(1.U) {
          for (i <- 0 until 1) {
            cf_ctrl_vec(i) := csBundle(i)
          }
          isVset_vec := isVset_sca
          complexNum_vec := 1.U
          validToRename_vec.zip(Seq(true.B) :+
            validSimple(1) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && validSimple(3)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && validSimple(4)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && (!io.validFromIBuf(4) || validSimple(4)) && validSimple(5))).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(true.B +:
            Seq.fill(5)(false.B)).map { case (dst, src) => dst := src }
        }
      }
    }.otherwise {
      cf_ctrl_vec.map { case x => x := cf_ctrl_sca }
      isVset_vec := false.B
      complexNum_vec := 1.U
      validToRename_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
      readyToIBuf_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
    }
  }.otherwise {
    when(!io.validFromIBuf(0)) {
      cf_ctrl_vec.map { case x => x := cf_ctrl_sca }
      isVset_vec := false.B
      complexNum_vec := 1.U
      validToRename_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
      readyToIBuf_vec.zip(Seq.fill(6)(true.B)).map { case (dst, src) => dst := src }
    }.elsewhen(readyCounter === 6.U) {
      switch(numOfUop) {
        is(9.U) {
          for (i <- 0 until 3) {
            cf_ctrl_vec(i) := csBundle(i + 6)
          }
          isVset_vec := false.B
          complexNum_vec := 3.U
          validToRename_vec.zip(Seq.fill(3)(true.B) :+
            validSimple(1) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && validSimple(3))).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(Seq(true.B) :+
            validSimple(1) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && validSimple(3)) :+
            false.B :+
            false.B).map { case (dst, src) => dst := src }
        }
        is(8.U) {
          for (i <- 0 until 2) {
            cf_ctrl_vec(i) := csBundle(i + 6)
          }
          isVset_vec := false.B
          complexNum_vec := 2.U
          validToRename_vec.zip(Seq.fill(2)(true.B) :+
            validSimple(1) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && validSimple(3)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && validSimple(4))).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(Seq(true.B) :+
            validSimple(1) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && validSimple(3)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && validSimple(4)) :+
            false.B).map { case (dst, src) => dst := src }
        }
      }
    }.elsewhen(readyCounter === 5.U) {
      switch(numOfUop) {
        is(9.U) {
          for (i <- 0 until 3) {
            cf_ctrl_vec(i) := csBundle(i + 6)
          }
          isVset_vec := false.B
          complexNum_vec := 3.U
          validToRename_vec.zip(Seq.fill(3)(true.B) :+
            validSimple(1) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && validSimple(3))).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(Seq(true.B) :+
            validSimple(1) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) :+
            false.B :+
            false.B :+
            false.B).map { case (dst, src) => dst := src }
        }
        is(8.U) {
          for (i <- 0 until 2) {
            cf_ctrl_vec(i) := csBundle(i + 6)
          }
          isVset_vec := false.B
          complexNum_vec := 2.U
          validToRename_vec.zip(Seq.fill(2)(true.B) :+
            validSimple(1) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && validSimple(3)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && validSimple(4))).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(Seq(true.B) :+
            validSimple(1) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && validSimple(3)) :+
            false.B :+
            false.B).map { case (dst, src) => dst := src }
        }
      }
    }.elsewhen(readyCounter === 4.U) {
      switch(numOfUop) {
        is(9.U) {
          for (i <- 0 until 3) {
            cf_ctrl_vec(i) := csBundle(i + 6)
          }
          isVset_vec := false.B
          complexNum_vec := 3.U
          validToRename_vec.zip(Seq.fill(3)(true.B) :+
            validSimple(1) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && validSimple(3))).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(Seq(true.B) :+
            validSimple(1) :+
            false.B :+
            false.B :+
            false.B :+
            false.B).map { case (dst, src) => dst := src }
        }
        is(8.U) {
          for (i <- 0 until 2) {
            cf_ctrl_vec(i) := csBundle(i + 6)
          }
          isVset_vec := false.B
          complexNum_vec := 2.U
          validToRename_vec.zip(Seq.fill(2)(true.B) :+
            validSimple(1) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && validSimple(3)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && validSimple(4))).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(Seq(true.B) :+
            validSimple(1) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) :+
            false.B :+
            false.B :+
            false.B).map { case (dst, src) => dst := src }
        }
      }
    }.elsewhen(readyCounter === 3.U) {
      switch(numOfUop) {
        is(9.U) {
          for (i <- 0 until 3) {
            cf_ctrl_vec(i) := csBundle(i + 6)
          }
          isVset_vec := false.B
          complexNum_vec := 3.U
          validToRename_vec.zip(Seq.fill(3)(true.B) :+
            validSimple(1) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && validSimple(3))).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(Seq(true.B) :+
            false.B :+
            false.B :+
            false.B :+
            false.B :+
            false.B).map { case (dst, src) => dst := src }
        }
        is(8.U) {
          for (i <- 0 until 2) {
            cf_ctrl_vec(i) := csBundle(i + 6)
          }
          isVset_vec := false.B
          complexNum_vec := 2.U
          validToRename_vec.zip(Seq.fill(2)(true.B) :+
            validSimple(1) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && validSimple(3)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && validSimple(4))).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(Seq(true.B) :+
            validSimple(1) :+
            false.B :+
            false.B :+
            false.B :+
            false.B).map { case (dst, src) => dst := src }
        }
      }
    }.elsewhen(readyCounter === 2.U) {
      switch(numOfUop) {
        is(9.U) {
          cf_ctrl_vec.map { case x => x := cf_ctrl_sca }
          isVset_vec := false.B
          complexNum_vec := 1.U
          validToRename_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
        }
        is(8.U) {
          for (i <- 0 until 2) {
            cf_ctrl_vec(i) := csBundle(i + 6)
          }
          isVset_vec := false.B
          complexNum_vec := 2.U
          validToRename_vec.zip(Seq.fill(2)(true.B) :+
            validSimple(1) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && validSimple(2)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && validSimple(3)) :+
            ((!io.validFromIBuf(1) || validSimple(1)) && (!io.validFromIBuf(2) || validSimple(2)) && (!io.validFromIBuf(3) || validSimple(3)) && validSimple(4))).map { case (dst, src) => dst := src }
          readyToIBuf_vec.zip(Seq(true.B) :+
            false.B :+
            false.B :+
            false.B :+
            false.B :+
            false.B).map { case (dst, src) => dst := src }
        }
      }
    }.otherwise {
      cf_ctrl_vec.map { case x => x := cf_ctrl_sca }
      isVset_vec := false.B
      complexNum_vec := 1.U
      validToRename_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
      readyToIBuf_vec.zip(Seq.fill(6)(false.B)).map { case (dst, src) => dst := src }
    }
  }

  //output
  when(isVec){
    io.deq.cf_ctrl := cf_ctrl_vec
    io.deq.isVset := isVset_vec
    io.deq.readyToIBuf := readyToIBuf_vec
    io.deq.validToRename := validToRename_vec
    io.deq.complexNum := complexNum_vec
  }.otherwise {
    io.deq.cf_ctrl.map{ case x => x := cf_ctrl_sca}
    io.deq.isVset := isVset_sca
    io.deq.readyToIBuf := readyToIBuf_sca
    io.deq.validToRename := validToRename_sca
    io.deq.complexNum := complexNum_sca
  }


}

