package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.regfile.RfReadPort
import xiangshan.backend.exu.Exu._

class Dispatch2Fp extends XSModule {
  val io = IO(new Bundle() {
    val fromDq = Flipped(Vec(dpParams.FpDqDeqWidth, DecoupledIO(new MicroOp)))
    val readRf = Vec(NRFpReadPorts - exuParameters.StuCnt, Flipped(new RfReadPort(XLEN + 1)))
    val regRdy = Vec(NRFpReadPorts - exuParameters.StuCnt, Input(Bool()))
    val numExist = Input(Vec(exuParameters.FpExuCnt, UInt(log2Ceil(IssQueSize).W)))
    val enqIQCtrl = Vec(exuParameters.FpExuCnt, DecoupledIO(new MicroOp))
    val enqIQData = Vec(exuParameters.FpExuCnt, Output(new ExuInput))
  })

  /**
    * Part 1: generate indexes for reservation stations
    */
  val fmacIndexGen = Module(new IndexMapping(dpParams.FpDqDeqWidth, exuParameters.FmacCnt, true))
  val fmacCanAccept = VecInit(io.fromDq.map(deq => deq.valid && fmacExeUnitCfg.canAccept(deq.bits.ctrl.fuType)))
  val fmacPriority = PriorityGen((0 until exuParameters.FmacCnt).map(i => io.numExist(i)))
  fmacIndexGen.io.validBits := fmacCanAccept
  fmacIndexGen.io.priority := fmacPriority

  val fmiscIndexGen = Module(new IndexMapping(dpParams.FpDqDeqWidth, exuParameters.FmiscCnt, true))
  val fmiscCanAccept = VecInit(io.fromDq.map(deq => deq.valid && fmiscExeUnitCfg.canAccept(deq.bits.ctrl.fuType)))
  val fmiscPriority = PriorityGen((0 until exuParameters.FmiscCnt).map(i => io.numExist(i+exuParameters.FmacCnt)))
  fmiscIndexGen.io.validBits := fmiscCanAccept
  fmiscIndexGen.io.priority := fmiscPriority

  val allIndexGen = Seq(fmacIndexGen, fmiscIndexGen)
  val validVec = allIndexGen.map(_.io.mapping.map(_.valid)).reduceLeft(_ ++ _)
  val indexVec = allIndexGen.map(_.io.mapping.map(_.bits)).reduceLeft(_ ++ _)
  for (i <- validVec.indices) {
    // XSDebug(p"mapping $i: valid ${validVec(i)} index ${indexVec(i)}\n")
  }

  /**
    * Part 2: assign regfile read ports
    */
  val fpStaticIndex = Seq(0, 1, 2, 3)
  val fpDynamicIndex = Seq(4, 5)
  val fpStaticMappedValid = fpStaticIndex.map(i => validVec(i))
  val fpDynamicMappedValid = fpDynamicIndex.map(i => validVec(i))
  val (fpReadPortSrc, fpDynamicExuSrc) = RegfileReadPortGen(fpStaticMappedValid, fpDynamicMappedValid)
  val fpStaticMapped = fpStaticIndex.map(i => indexVec(i))
  val fpDynamicMapped = fpDynamicIndex.map(i => indexVec(i))
  for (i <- fpStaticIndex.indices) {
    val index = WireInit(VecInit(fpStaticMapped(i) +: fpDynamicMapped))
    io.readRf(3*i  ).addr := io.fromDq(index(fpReadPortSrc(i))).bits.psrc1
    io.readRf(3*i+1).addr := io.fromDq(index(fpReadPortSrc(i))).bits.psrc2
    io.readRf(3*i+2).addr := io.fromDq(index(fpReadPortSrc(i))).bits.psrc3
  }
  val readPortIndex = Wire(Vec(exuParameters.FpExuCnt, UInt(log2Ceil(NRFpReadPorts - exuParameters.StuCnt).W)))
  fpStaticIndex.zipWithIndex.map({case (index, i) => readPortIndex(index) := (3*i).U})
  fpDynamicIndex.zipWithIndex.map({case (index, i) => readPortIndex(index) := 3.U * fpDynamicExuSrc(i)})

  /**
    * Part 3: dispatch to reservation stations
    */
  val fmacReady = Cat(io.enqIQCtrl.take(exuParameters.FmacCnt).map(_.ready)).andR
  val fmiscReady = Cat(io.enqIQCtrl.drop(exuParameters.FmacCnt).map(_.ready)).andR
  for (i <- 0 until exuParameters.FpExuCnt) {
    val enq = io.enqIQCtrl(i)
    if (i < exuParameters.FmacCnt) {
      enq.valid := fmacIndexGen.io.mapping(i).valid && fmacReady
    }
    else {
      enq.valid := fmiscIndexGen.io.mapping(i - exuParameters.FmacCnt).valid && fmiscReady
    }
    enq.bits := io.fromDq(indexVec(i)).bits
    enq.bits.src1State := io.regRdy(readPortIndex(i))
    enq.bits.src2State := io.regRdy(readPortIndex(i) + 1.U)
    enq.bits.src3State := io.regRdy(readPortIndex(i) + 2.U)

    XSInfo(enq.fire(), p"pc 0x${Hexadecimal(enq.bits.cf.pc)} with type ${enq.bits.ctrl.fuType} " +
      p"srcState(${enq.bits.src1State} ${enq.bits.src2State} ${enq.bits.src3State}) " +
      p"enters reservation station $i from ${indexVec(i)}\n")
  }

  /**
    * Part 4: response to dispatch queue
    */
  val fmisc2CanOut = !(fmiscCanAccept(0) && fmiscCanAccept(1))
  val fmisc3CanOut = !(fmiscCanAccept(0) && fmiscCanAccept(1) || fmiscCanAccept(0) && fmiscCanAccept(2) || fmiscCanAccept(1) && fmiscCanAccept(2))
  for (i <- 0 until dpParams.FpDqDeqWidth) {
    io.fromDq(i).ready := fmacCanAccept(i) && fmacReady ||
                          fmiscCanAccept(i) && (if (i <= 1) true.B else if (i == 2) fmisc2CanOut else fmisc3CanOut) && fmiscReady

    XSInfo(io.fromDq(i).fire(),
      p"pc 0x${Hexadecimal(io.fromDq(i).bits.cf.pc)} leaves Fp dispatch queue $i with nroq ${io.fromDq(i).bits.roqIdx}\n")
    XSDebug(io.fromDq(i).valid && !io.fromDq(i).ready,
      p"pc 0x${Hexadecimal(io.fromDq(i).bits.cf.pc)} waits at Fp dispatch queue with index $i\n")
  }
  XSError(PopCount(io.fromDq.map(_.fire())) =/= PopCount(io.enqIQCtrl.map(_.fire())), "deq =/= enq\n")

  /**
    * Part 5: the second stage of dispatch 2 (send data to reservation station)
    */
  val readPortIndexReg = Reg(Vec(exuParameters.FpExuCnt, UInt(log2Ceil(NRFpReadPorts - exuParameters.StuCnt).W)))
  val uopReg = Reg(Vec(exuParameters.FpExuCnt, new MicroOp))
  val dataValidRegDebug = Reg(Vec(exuParameters.FpExuCnt, Bool()))
  for (i <- 0 until exuParameters.FpExuCnt) {
    readPortIndexReg(i) := readPortIndex(i)
    uopReg(i) := io.enqIQCtrl(i).bits
    dataValidRegDebug(i) := io.enqIQCtrl(i).fire()

    io.enqIQData(i) := DontCare
    io.enqIQData(i).src1 := io.readRf(readPortIndexReg(i)).data
    io.enqIQData(i).src2 := io.readRf(readPortIndexReg(i) + 1.U).data
    io.enqIQData(i).src3 := io.readRf(readPortIndexReg(i) + 2.U).data

    XSDebug(dataValidRegDebug(i),
      p"pc 0x${Hexadecimal(uopReg(i).cf.pc)} reads operands from " +
        p"(${readPortIndexReg(i)    }, ${uopReg(i).psrc1}, ${Hexadecimal(io.enqIQData(i).src1)}), " +
        p"(${readPortIndexReg(i)+1.U}, ${uopReg(i).psrc2}, ${Hexadecimal(io.enqIQData(i).src2)}), " +
        p"(${readPortIndexReg(i)+2.U}, ${uopReg(i).psrc3}, ${Hexadecimal(io.enqIQData(i).src3)})\n")
  }
}
