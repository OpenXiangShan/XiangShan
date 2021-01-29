package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.exu.Exu._
import xiangshan.backend.regfile.RfReadPort
import xiangshan.backend.rename.BusyTableReadIO

class Dispatch2Int extends XSModule {
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
  val aluIndexGen = Module(new IndexMapping(dpParams.IntDqDeqWidth, aluCnt, true))

  val mduPriority = PriorityGen(io.numExist.slice(jmpCnt, jmpCnt + mduCnt))
  val aluPriority = PriorityGen(io.numExist.drop(jmpCnt + mduCnt))
  jmpIndexGen.io.validBits := jmpCanAccept
  mduIndexGen.io.validBits := mduCanAccept
  aluIndexGen.io.validBits := aluCanAccept
  jmpIndexGen.io.priority := DontCare
  mduIndexGen.io.priority := mduPriority
  aluIndexGen.io.priority := aluPriority

  val allIndexGen = Seq(jmpIndexGen, mduIndexGen, aluIndexGen)
  val validVec = allIndexGen.flatMap(_.io.mapping.map(_.valid))
  val indexVec = allIndexGen.flatMap(_.io.mapping.map(_.bits))

  /**
    * Part 2: assign regfile read ports
    */
  val intStaticIndex = Seq(3, 4, 5, 6)
  val intDynamicIndex = Seq(0, 1, 2)
  val intStaticMappedValid = intStaticIndex.map(i => validVec(i))
  val intDynamicMappedValid = intDynamicIndex.map(i => validVec(i))
  val (intReadPortSrc, intDynamicExuSrc) = RegfileReadPortGen(intStaticMappedValid, intDynamicMappedValid)
  val intStaticMapped = intStaticIndex.map(i => indexVec(i))
  val intDynamicMapped = intDynamicIndex.map(i => indexVec(i))
  for (i <- intStaticIndex.indices) {
    val index = WireInit(VecInit(intStaticMapped(i) +: intDynamicMapped))
    io.readRf(2*i  ) := io.fromDq(index(intReadPortSrc(i))).bits.psrc1
    io.readRf(2*i+1) := io.fromDq(index(intReadPortSrc(i))).bits.psrc2
  }
  val readPortIndex = Wire(Vec(exuParameters.IntExuCnt, UInt(2.W)))
  intStaticIndex.zipWithIndex.map({case (index, i) => readPortIndex(index) := i.U})
  intDynamicIndex.zipWithIndex.map({case (index, i) => readPortIndex(index) := intDynamicExuSrc(i)})

  for (i <- 0 until dpParams.IntDqDeqWidth) {
    io.readState(2*i  ).req := io.fromDq(i).bits.psrc1
    io.readState(2*i+1).req := io.fromDq(i).bits.psrc2
  }

  /**
    * Part 3: dispatch to reservation stations
    */
  val jmpReady = io.enqIQCtrl(0).ready
  val mduReady = Cat(io.enqIQCtrl.slice(jmpCnt, jmpCnt + mduCnt).map(_.ready)).andR
  val aluReady = Cat(io.enqIQCtrl.drop(jmpCnt + mduCnt).map(_.ready)).andR
  for (i <- 0 until exuParameters.IntExuCnt) {
    val enq = io.enqIQCtrl(i)
    if (i < jmpCnt) {
      enq.valid := jmpIndexGen.io.mapping(i).valid// && jmpReady
    }
    else if (i < jmpCnt + mduCnt) {
      enq.valid := mduIndexGen.io.mapping(i - jmpCnt).valid && mduReady
    }
    else { // alu
      enq.valid := aluIndexGen.io.mapping(i - (jmpCnt + mduCnt)).valid && aluReady
    }
    enq.bits := io.fromDq(indexVec(i)).bits
    
    val src1Ready = VecInit((0 until 4).map(i => io.readState(i * 2).resp))
    val src2Ready = VecInit((0 until 4).map(i => io.readState(i * 2 + 1).resp))
    enq.bits.src1State := src1Ready(indexVec(i))
    enq.bits.src2State := src2Ready(indexVec(i))
    enq.bits.src3State := DontCare

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
