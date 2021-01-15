package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.exu.Exu._
import xiangshan.backend.regfile.RfReadPort

class Dispatch2Int extends XSModule {
  val io = IO(new Bundle() {
    val fromDq = Flipped(Vec(dpParams.IntDqDeqWidth, DecoupledIO(new MicroOp)))
    val readRf = Vec(NRIntReadPorts - NRMemReadPorts, Flipped(new RfReadPort(XLEN)))
    val regRdy = Vec(NRIntReadPorts - NRMemReadPorts, Input(Bool()))
    val numExist = Input(Vec(exuParameters.IntExuCnt, UInt(log2Ceil(IssQueSize).W)))
    val enqIQCtrl = Vec(exuParameters.IntExuCnt, DecoupledIO(new MicroOp))
    val readPortIndex = Vec(exuParameters.IntExuCnt, Output(UInt(log2Ceil(8 / 2).W)))
  })

  /**
    * Part 1: generate indexes for reservation stations
    */
  val jmpCanAccept = VecInit(io.fromDq.map(deq => deq.valid && jumpExeUnitCfg.canAccept(deq.bits.ctrl.fuType)))
  val aluCanAccept = VecInit(io.fromDq.map(deq => deq.valid && aluExeUnitCfg.canAccept(deq.bits.ctrl.fuType)))
  val mduCanAccept = VecInit(io.fromDq.map(deq => deq.valid && mulDivExeUnitCfg.canAccept(deq.bits.ctrl.fuType)))
  assert(exuParameters.JmpCnt == 1)
  val jmpIndexGen = Module(new IndexMapping(dpParams.IntDqDeqWidth, exuParameters.JmpCnt, false))
  val aluIndexGen = Module(new IndexMapping(dpParams.IntDqDeqWidth, exuParameters.AluCnt, true))
  val mduIndexGen = Module(new IndexMapping(dpParams.IntDqDeqWidth, exuParameters.MduCnt, true))
  val aluPriority = PriorityGen((0 until exuParameters.AluCnt).map(i => io.numExist(i+exuParameters.JmpCnt)))
  val mduPriority = PriorityGen((0 until exuParameters.MduCnt).map(i => io.numExist(i+exuParameters.JmpCnt+exuParameters.AluCnt)))
  jmpIndexGen.io.validBits := jmpCanAccept
  aluIndexGen.io.validBits := aluCanAccept
  mduIndexGen.io.validBits := mduCanAccept
  jmpIndexGen.io.priority := DontCare
  aluIndexGen.io.priority := aluPriority
  mduIndexGen.io.priority := mduPriority

  val allIndexGen = Seq(jmpIndexGen, aluIndexGen, mduIndexGen)
  val validVec = allIndexGen.map(_.io.mapping.map(_.valid)).reduceLeft(_ ++ _)
  val indexVec = allIndexGen.map(_.io.mapping.map(_.bits)).reduceLeft(_ ++ _)
  for (i <- validVec.indices) {
    // XSDebug(p"mapping $i: valid ${validVec(i)} index ${indexVec(i)}\n")
  }

  /**
    * Part 2: assign regfile read ports
    */
  val intStaticIndex = Seq(1, 2, 3, 4)
  val intDynamicIndex = Seq(0, 5, 6)
  val intStaticMappedValid = intStaticIndex.map(i => validVec(i))
  val intDynamicMappedValid = intDynamicIndex.map(i => validVec(i))
  val (intReadPortSrc, intDynamicExuSrc) = RegfileReadPortGen(intStaticMappedValid, intDynamicMappedValid)
  val intStaticMapped = intStaticIndex.map(i => indexVec(i))
  val intDynamicMapped = intDynamicIndex.map(i => indexVec(i))
  for (i <- intStaticIndex.indices) {
    val index = WireInit(VecInit(intStaticMapped(i) +: intDynamicMapped))
    io.readRf(2*i  ).addr := io.fromDq(index(intReadPortSrc(i))).bits.psrc1
    io.readRf(2*i+1).addr := io.fromDq(index(intReadPortSrc(i))).bits.psrc2
  }
  val readPortIndex = Wire(Vec(exuParameters.IntExuCnt, UInt(2.W)))
  intStaticIndex.zipWithIndex.map({case (index, i) => readPortIndex(index) := i.U})
  intDynamicIndex.zipWithIndex.map({case (index, i) => readPortIndex(index) := intDynamicExuSrc(i)})

  /**
    * Part 3: dispatch to reservation stations
    */
  val jmpReady = io.enqIQCtrl(0).ready
  val aluReady = Cat(io.enqIQCtrl.take(exuParameters.JmpCnt + exuParameters.AluCnt).drop(exuParameters.JmpCnt).map(_.ready)).andR
  val mduReady = Cat(io.enqIQCtrl.drop(exuParameters.JmpCnt + exuParameters.AluCnt).map(_.ready)).andR
  for (i <- 0 until exuParameters.IntExuCnt) {
    val enq = io.enqIQCtrl(i)
    if (i < exuParameters.JmpCnt) {
      enq.valid := jmpIndexGen.io.mapping(i).valid// && jmpReady
    }
    else if (i < exuParameters.JmpCnt + exuParameters.AluCnt) {
      enq.valid := aluIndexGen.io.mapping(i - exuParameters.JmpCnt).valid && aluReady
    }
    else {
      enq.valid := mduIndexGen.io.mapping(i - (exuParameters.JmpCnt + exuParameters.AluCnt)).valid && mduReady
    }
    enq.bits := io.fromDq(indexVec(i)).bits
    
    val src1Ready = VecInit((0 until 4).map(i => io.regRdy(i * 2)))
    val src2Ready = VecInit((0 until 4).map(i => io.regRdy(i * 2 + 1)))
    enq.bits.src1State := src1Ready(readPortIndex(i))
    enq.bits.src2State := src2Ready(readPortIndex(i))

    XSInfo(enq.fire(), p"pc 0x${Hexadecimal(enq.bits.cf.pc)} with type ${enq.bits.ctrl.fuType} " +
      p"srcState(${enq.bits.src1State} ${enq.bits.src2State}) " +
      p"enters reservation station $i from ${indexVec(i)}\n")
  }

  /**
    * Part 4: response to dispatch queue
    */
  val mdu2CanOut = !(mduCanAccept(0) && mduCanAccept(1))
  val mdu3CanOut = !(mduCanAccept(0) && mduCanAccept(1) || mduCanAccept(0) && mduCanAccept(2) || mduCanAccept(1) && mduCanAccept(2))
  for (i <- 0 until dpParams.IntDqDeqWidth) {
    io.fromDq(i).ready := jmpCanAccept(i) && (if (i == 0) true.B else !Cat(jmpCanAccept.take(i)).orR) && jmpReady ||
                          aluCanAccept(i) && aluReady ||
                          mduCanAccept(i) && (if (i <= 1) true.B else if (i == 2) mdu2CanOut else mdu3CanOut) && mduReady

    XSInfo(io.fromDq(i).fire(),
      p"pc 0x${Hexadecimal(io.fromDq(i).bits.cf.pc)} leaves Int dispatch queue $i with nroq ${io.fromDq(i).bits.roqIdx}\n")
    XSDebug(io.fromDq(i).valid && !io.fromDq(i).ready,
      p"pc 0x${Hexadecimal(io.fromDq(i).bits.cf.pc)} waits at Int dispatch queue with index $i\n")
  }
  XSError(PopCount(io.fromDq.map(_.fire())) =/= PopCount(io.enqIQCtrl.map(_.fire())), "deq =/= enq\n")

  /**
    * Part 5: send read port index of register file to reservation station
    */
  io.readPortIndex := readPortIndex
//  val readPortIndexReg = Reg(Vec(exuParameters.IntExuCnt, UInt(log2Ceil(NRIntReadPorts).W)))
//  val uopReg = Reg(Vec(exuParameters.IntExuCnt, new MicroOp))
//  val dataValidRegDebug = Reg(Vec(exuParameters.IntExuCnt, Bool()))
//  for (i <- 0 until exuParameters.IntExuCnt) {
//    readPortIndexReg(i) := readPortIndex(i)
//    uopReg(i) := io.enqIQCtrl(i).bits
//    dataValidRegDebug(i) := io.enqIQCtrl(i).fire()
//
//    io.enqIQData(i) := DontCare
//    io.enqIQData(i).src1 := Mux(uopReg(i).ctrl.src1Type === SrcType.pc,
//      SignExt(uopReg(i).cf.pc, XLEN), io.readRf(readPortIndexReg(i)).data)
//    io.enqIQData(i).src2 := Mux(uopReg(i).ctrl.src2Type === SrcType.imm,
//      uopReg(i).ctrl.imm, io.readRf(readPortIndexReg(i) + 1.U).data)
//
//    XSDebug(dataValidRegDebug(i),
//      p"pc 0x${Hexadecimal(uopReg(i).cf.pc)} reads operands from " +
//        p"(${readPortIndexReg(i)    }, ${uopReg(i).psrc1}, ${Hexadecimal(io.enqIQData(i).src1)}), " +
//        p"(${readPortIndexReg(i)+1.U}, ${uopReg(i).psrc2}, ${Hexadecimal(io.enqIQData(i).src2)})\n")
//  }

  XSPerf("utilization", PopCount(io.fromDq.map(_.valid)))

}
