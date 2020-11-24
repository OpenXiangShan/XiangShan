package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.regfile.RfReadPort
import xiangshan.backend.exu.Exu._

class Dispatch2Ls extends XSModule {
  val io = IO(new Bundle() {
    val fromDq = Flipped(Vec(dpParams.LsDqDeqWidth, DecoupledIO(new MicroOp)))
    val readIntRf = Vec(NRMemReadPorts, Flipped(new RfReadPort))
    val readFpRf = Vec(exuParameters.StuCnt, Flipped(new RfReadPort))
    // val intRegAddr = Vec(NRMemReadPorts, Output(UInt(PhyRegIdxWidth.W)))
    // val fpRegAddr = Vec(exuParameters.StuCnt, Output(UInt(PhyRegIdxWidth.W)))
    val intRegRdy = Vec(NRMemReadPorts, Input(Bool()))
    val fpRegRdy = Vec(exuParameters.StuCnt, Input(Bool()))
    val numExist = Input(Vec(exuParameters.LsExuCnt, UInt(log2Ceil(IssQueSize).W)))
    val enqIQCtrl = Vec(exuParameters.LsExuCnt, DecoupledIO(new MicroOp))
    val enqIQData = Vec(exuParameters.LsExuCnt, Output(new ExuInput))
  })

  /**
    * Part 1: generate indexes for reservation stations
    */
  val loadIndexGen = Module(new IndexMapping(dpParams.LsDqDeqWidth, exuParameters.LduCnt, true))
  val storeIndexGen = Module(new IndexMapping(dpParams.LsDqDeqWidth, exuParameters.StuCnt, true))
  val loadPriority = PriorityGen((0 until exuParameters.LduCnt).map(i => io.numExist(i)))
  val storePriority = PriorityGen((0 until exuParameters.StuCnt).map(i => io.numExist(i+exuParameters.LduCnt)))
  for (i <- 0 until dpParams.LsDqDeqWidth) {
    loadIndexGen.io.validBits(i) := io.fromDq(i).valid && ldExeUnitCfg.canAccept(io.fromDq(i).bits.ctrl.fuType)
    storeIndexGen.io.validBits(i) := io.fromDq(i).valid && stExeUnitCfg.canAccept(io.fromDq(i).bits.ctrl.fuType)

    // XSDebug(io.fromDq(i).valid,
    //   p"ls dp queue $i: ${Hexadecimal(io.fromDq(i).bits.cf.pc)} type ${Binary(io.fromDq(i).bits.ctrl.fuType)}\n")
  }
  for (i <- 0 until exuParameters.LduCnt) {
    loadIndexGen.io.priority(i) := loadPriority(i)
  }
  for (i <- 0 until exuParameters.StuCnt) {
    storeIndexGen.io.priority(i) := storePriority(i)
  }
  val allIndexGen = Seq(loadIndexGen, storeIndexGen)
  val validVec = allIndexGen.map(_.io.mapping.map(_.valid)).reduceLeft(_ ++ _)
  val indexVec = allIndexGen.map(_.io.mapping.map(_.bits)).reduceLeft(_ ++ _)
  val rsValidVec = (0 until dpParams.LsDqDeqWidth).map(i => Cat(allIndexGen.map(_.io.reverseMapping(i).valid)).orR())
  val rsIndexVec = (0 until dpParams.LsDqDeqWidth).map({i =>
    val indexOffset = Seq(0, exuParameters.LduCnt)
    allIndexGen.zipWithIndex.map{
      case (index, j) => Mux(index.io.reverseMapping(i).valid,
        ZeroExt(index.io.reverseMapping(i).bits, log2Ceil(exuParameters.LsExuCnt)) + indexOffset(j).U,
        0.U)
    }.reduce(_ | _)
  })

  for (i <- validVec.indices) {
    // XSDebug(p"mapping $i: valid ${validVec(i)} index ${indexVec(i)}\n")
  }
  for (i <- rsValidVec.indices) {
    // XSDebug(p"load reverse $i: valid ${loadIndexGen.io.reverseMapping(i).valid} index ${loadIndexGen.io.reverseMapping(i).bits}\n")
    // XSDebug(p"store reverse $i: valid ${storeIndexGen.io.reverseMapping(i).valid} index ${storeIndexGen.io.reverseMapping(i).bits}\n")
    // XSDebug(p"reverseMapping $i: valid ${rsValidVec(i)} index ${rsIndexVec(i)}\n")
  }

  /**
    * Part 2: assign regfile read ports (actually only reg states from rename)
    *
    * The four load/store issue queue
    */
  assert(exuParameters.LduCnt == 2)
  assert(exuParameters.StuCnt == 2)
  val readPort = Seq(0, 1, 2, 4)
  for (i <- 0 until exuParameters.LsExuCnt) {
    if (i < exuParameters.LduCnt) {
      io.readIntRf(readPort(i)).addr := io.fromDq(indexVec(i)).bits.psrc1
    }
    else {
      io.readFpRf(i - exuParameters.LduCnt).addr := io.fromDq(indexVec(i)).bits.psrc2
      io.readIntRf(readPort(i)  ).addr := io.fromDq(indexVec(i)).bits.psrc1
      io.readIntRf(readPort(i)+1).addr := io.fromDq(indexVec(i)).bits.psrc2
    }
  }

  /**
    * Part 3: dispatch to reservation stations
    */
  for (i <- 0 until exuParameters.LsExuCnt) {
    val enq = io.enqIQCtrl(i)
    enq.valid := validVec(i)
    enq.bits := io.fromDq(indexVec(i)).bits
    enq.bits.src1State := io.intRegRdy(readPort(i))
    if (i < exuParameters.LduCnt) {
      enq.bits.src2State := DontCare
    }
    else {
      enq.bits.src2State := Mux(io.fromDq(indexVec(i)).bits.ctrl.src2Type === SrcType.fp,
        io.fpRegRdy(i - exuParameters.LduCnt), io.intRegRdy(readPort(i) + 1))
    }

    XSInfo(enq.fire(), p"pc 0x${Hexadecimal(enq.bits.cf.pc)} with type ${enq.bits.ctrl.fuType} " +
      p"srcState(${enq.bits.src1State} ${enq.bits.src2State}) " +
      p"enters issue queue $i from ${indexVec(i)}\n")
  }

  /**
    * Part 4: response to dispatch queue
    */
  for (i <- 0 until dpParams.LsDqDeqWidth) {
    io.fromDq(i).ready := rsValidVec(i) && io.enqIQCtrl(rsIndexVec(i)).ready

    XSInfo(io.fromDq(i).fire(),
      p"pc 0x${Hexadecimal(io.fromDq(i).bits.cf.pc)} leaves Ls dispatch queue $i with nroq ${io.fromDq(i).bits.roqIdx}\n")
    XSDebug(io.fromDq(i).valid && !io.fromDq(i).ready,
      p"pc 0x${Hexadecimal(io.fromDq(i).bits.cf.pc)} waits at Ls dispatch queue with index $i\n")
  }

  /**
    * Part 5: the second stage of dispatch 2 (send data to reservation station)
    */
  val uopReg = Reg(Vec(exuParameters.LsExuCnt, new MicroOp))
  val dataValidRegDebug = Reg(Vec(exuParameters.LsExuCnt, Bool()))
  for (i <- 0 until exuParameters.LsExuCnt) {
    uopReg(i) := io.enqIQCtrl(i).bits
    dataValidRegDebug(i) := io.enqIQCtrl(i).fire()

    io.enqIQData(i) := DontCare
    // assert(uopReg(i).ctrl.src1Type =/= SrcType.pc)
    io.enqIQData(i).src1 := io.readIntRf(readPort(i)).data
    if (i >= exuParameters.LduCnt) {
      io.enqIQData(i).src2 := Mux(
        uopReg(i).ctrl.src2Type === SrcType.imm,
        uopReg(i).ctrl.imm,
        Mux(uopReg(i).ctrl.src2Type === SrcType.fp,
          io.readFpRf(i - exuParameters.LduCnt).data,
          io.readIntRf(readPort(i) + 1).data))
    }

    XSDebug(dataValidRegDebug(i),
      p"pc 0x${Hexadecimal(uopReg(i).cf.pc)} reads operands from " +
        p"(${readPort(i)  }, ${uopReg(i).psrc1}, ${Hexadecimal(io.enqIQData(i).src1)}), " +
        p"(${readPort(i)+1}, ${uopReg(i).psrc2}, ${Hexadecimal(io.enqIQData(i).src2)})\n")
  }
}
