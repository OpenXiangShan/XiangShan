package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.exu.Exu._
import xiangshan.backend.regfile.RfReadPort
import xiangshan.backend.exu._

class Dispatch2Int extends XSModule {
  val io = IO(new Bundle() {
    val fromDq = Flipped(Vec(dpParams.IntDqDeqWidth, DecoupledIO(new MicroOp)))
    val readRf = Vec(NRIntReadPorts - NRMemReadPorts, Flipped(new RfReadPort))
    val regRdy = Vec(NRIntReadPorts - NRMemReadPorts, Input(Bool()))
    val numExist = Input(Vec(exuParameters.IntExuCnt, UInt(log2Ceil(IssQueSize).W)))
    val enqIQCtrl = Vec(exuParameters.IntExuCnt, DecoupledIO(new MicroOp))
    val enqIQData = Vec(exuParameters.IntExuCnt, Output(new ExuInput))
  })

  /**
    * Part 1: generate indexes for reservation stations
    */
  assert(exuParameters.JmpCnt == 1)
  val jmpIndexGen = Module(new IndexMapping(dpParams.IntDqDeqWidth, exuParameters.JmpCnt, false))
  val aluIndexGen = Module(new IndexMapping(dpParams.IntDqDeqWidth, exuParameters.AluCnt, true))
  val mduIndexGen = Module(new IndexMapping(dpParams.IntDqDeqWidth, exuParameters.MduCnt, true))
  val aluPriority = PriorityGen((0 until exuParameters.AluCnt).map(i => io.numExist(i+exuParameters.JmpCnt)))
  val mduPriority = PriorityGen((0 until exuParameters.MduCnt).map(i => io.numExist(i+exuParameters.JmpCnt+exuParameters.AluCnt)))
  for (i <- 0 until dpParams.IntDqDeqWidth) {
    jmpIndexGen.io.validBits(i) := io.fromDq(i).valid && jumpExeUnitCfg.canAccept(io.fromDq(i).bits.ctrl.fuType)
    aluIndexGen.io.validBits(i) := io.fromDq(i).valid && aluExeUnitCfg.canAccept(io.fromDq(i).bits.ctrl.fuType)
    mduIndexGen.io.validBits(i) := io.fromDq(i).valid && mulDivExeUnitCfg.canAccept(io.fromDq(i).bits.ctrl.fuType)
    // XSDebug(io.fromDq(i).valid,
    //   p"int dp queue $i: ${Hexadecimal(io.fromDq(i).bits.cf.pc)} type ${Binary(io.fromDq(i).bits.ctrl.fuType)}\n")
  }
  jmpIndexGen.io.priority := DontCare
  for (i <- 0 until exuParameters.AluCnt) {
    aluIndexGen.io.priority(i) := aluPriority(i)
  }
  for (i <- 0 until exuParameters.MduCnt) {
    mduIndexGen.io.priority(i) := mduPriority(i)
  }
  val allIndexGen = Seq(jmpIndexGen, aluIndexGen, mduIndexGen)
  val validVec = allIndexGen.map(_.io.mapping.map(_.valid)).reduceLeft(_ ++ _)
  val indexVec = allIndexGen.map(_.io.mapping.map(_.bits)).reduceLeft(_ ++ _)
  val rsValidVec = (0 until dpParams.IntDqDeqWidth).map(i => Cat(allIndexGen.map(_.io.reverseMapping(i).valid)).orR())
  val rsIndexVec = (0 until dpParams.IntDqDeqWidth).map({i =>
    val indexOffset = Seq(0, exuParameters.JmpCnt, exuParameters.JmpCnt + exuParameters.AluCnt)
    allIndexGen.zipWithIndex.map{
      case (index, j) => Mux(index.io.reverseMapping(i).valid,
        ZeroExt(index.io.reverseMapping(i).bits, log2Ceil(exuParameters.IntExuCnt)) + indexOffset(j).U,
        0.U)
    }.reduce(_ | _)
  })

  for (i <- validVec.indices) {
    // XSDebug(p"mapping $i: valid ${validVec(i)} index ${indexVec(i)}\n")
  }
  for (i <- rsValidVec.indices) {
    // XSDebug(p"jmp reverse $i: valid ${jmpIndexGen.io.reverseMapping(i).valid} index ${jmpIndexGen.io.reverseMapping(i).bits}\n")
    // XSDebug(p"alu reverse $i: valid ${aluIndexGen.io.reverseMapping(i).valid} index ${aluIndexGen.io.reverseMapping(i).bits}\n")
    // XSDebug(p"mdu reverse $i: valid ${mduIndexGen.io.reverseMapping(i).valid} index ${mduIndexGen.io.reverseMapping(i).bits}\n")
    // XSDebug(p"reverseMapping $i: valid ${rsValidVec(i)} index ${rsIndexVec(i)}\n")
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
  val readPortIndex = Wire(Vec(exuParameters.IntExuCnt, UInt(log2Ceil(NRIntReadPorts).W)))
  intStaticIndex.zipWithIndex.map({case (index, i) => readPortIndex(index) := (2*i).U})
  intDynamicIndex.zipWithIndex.map({case (index, i) => readPortIndex(index) := 2.U * intDynamicExuSrc(i)})

  /**
    * Part 3: dispatch to reservation stations
    */
  for (i <- 0 until exuParameters.IntExuCnt) {
    val enq = io.enqIQCtrl(i)
    enq.valid := validVec(i)
    enq.bits := io.fromDq(indexVec(i)).bits
    enq.bits.src1State := io.regRdy(readPortIndex(i))
    enq.bits.src2State := io.regRdy(readPortIndex(i) + 1.U)

    XSInfo(enq.fire(), p"pc 0x${Hexadecimal(enq.bits.cf.pc)} with type ${enq.bits.ctrl.fuType} " +
      p"srcState(${enq.bits.src1State} ${enq.bits.src2State}) " +
      p"enters reservation station $i from ${indexVec(i)}\n")
  }

  /**
    * Part 4: response to dispatch queue
    */
  for (i <- 0 until dpParams.IntDqDeqWidth) {
    io.fromDq(i).ready := rsValidVec(i) && io.enqIQCtrl(rsIndexVec(i)).ready

    XSInfo(io.fromDq(i).fire(),
      p"pc 0x${Hexadecimal(io.fromDq(i).bits.cf.pc)} leaves Int dispatch queue $i with nroq ${io.fromDq(i).bits.roqIdx}\n")
    XSDebug(io.fromDq(i).valid && !io.fromDq(i).ready,
      p"pc 0x${Hexadecimal(io.fromDq(i).bits.cf.pc)} waits at Int dispatch queue with index $i\n")
  }

  /**
    * Part 5: the second stage of dispatch 2 (send data to reservation station)
    */
  val readPortIndexReg = Reg(Vec(exuParameters.IntExuCnt, UInt(log2Ceil(NRIntReadPorts).W)))
  val uopReg = Reg(Vec(exuParameters.IntExuCnt, new MicroOp))
  val dataValidRegDebug = Reg(Vec(exuParameters.IntExuCnt, Bool()))
  for (i <- 0 until exuParameters.IntExuCnt) {
    readPortIndexReg(i) := readPortIndex(i)
    uopReg(i) := io.enqIQCtrl(i).bits
    dataValidRegDebug(i) := io.enqIQCtrl(i).fire()

    io.enqIQData(i) := DontCare
    io.enqIQData(i).src1 := Mux(uopReg(i).ctrl.src1Type === SrcType.pc,
      SignExt(uopReg(i).cf.pc, XLEN), io.readRf(readPortIndexReg(i)).data)
    io.enqIQData(i).src2 := Mux(uopReg(i).ctrl.src2Type === SrcType.imm,
      uopReg(i).ctrl.imm, io.readRf(readPortIndexReg(i) + 1.U).data)

    XSDebug(dataValidRegDebug(i),
      p"pc 0x${Hexadecimal(uopReg(i).cf.pc)} reads operands from " +
        p"(${readPortIndexReg(i)    }, ${uopReg(i).psrc1}, ${Hexadecimal(io.enqIQData(i).src1)}), " +
        p"(${readPortIndexReg(i)+1.U}, ${uopReg(i).psrc2}, ${Hexadecimal(io.enqIQData(i).src2)})\n")
  }
}
