package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import xiangshan._
import utils.{XSDebug}

class RegfileReadPortGen extends XSModule {
  val io = IO(new Bundle() {
    // from dispatch queues
    val intIQEnqIndex = Flipped(Vec(exuParameters.IntExuCnt, ValidIO(UInt(log2Ceil(dpParams.IntDqDeqWidth).W))))
    val fpIQEnqIndex = Flipped(Vec(exuParameters.FpExuCnt, ValidIO(UInt(log2Ceil(dpParams.FpDqDeqWidth).W))))
    val lsIQEnqIndex = Flipped(Vec(exuParameters.LduCnt + exuParameters.StuCnt, ValidIO(UInt(log2Ceil(dpParams.LsDqDeqWidth).W))))
    // chooses dispatch queue dequeue indexs for regfile read ports
    val readIntRf = Output(Vec(NRIntReadPorts, UInt(log2Ceil(dpParams.IntDqDeqWidth).W)))
    val readFpRf = Output(Vec(NRFpReadPorts, UInt(log2Ceil(dpParams.IntDqDeqWidth).W)))
    // chooses regfile read ports for reservation stations
    val intIQRfSrc = Output(Vec(exuParameters.IntExuCnt, UInt(log2Ceil(NRIntReadPorts).W)))
    val fpIQRfSrc = Output(Vec(exuParameters.FpExuCnt, UInt(log2Ceil(NRFpReadPorts).W)))
    val lsIQRfSrc = Output(Vec(exuParameters.LsExuCnt + exuParameters.StuCnt, UInt(log2Ceil(NRIntReadPorts).W)))
  })

  def RegfileReadPortArbiter(staticMappedValid: Seq[Bool], dynamicMappedValid: Seq[Bool]) = {
    val choiceCount = dynamicMappedValid.length + 1
    // read port is assigned to readPortSrc
    val readPortSrc = Wire(Vec(staticMappedValid.length, UInt(log2Ceil(choiceCount).W)))
    var hasAssigned = (0 until choiceCount).map(_ => false.B)
    for (i <- 0 until staticMappedValid.length) {
      val valid = staticMappedValid(i) +: dynamicMappedValid
      val wantReadPort = (0 until choiceCount).map(j => valid(j) && ((j == 0).asBool() || !hasAssigned(j)))
      readPortSrc(i) := PriorityEncoder(wantReadPort)
      val onehot = UIntToOH(readPortSrc(i))
      hasAssigned = (0 until choiceCount).map(i => hasAssigned(i) || onehot(i))
      XSDebug("int %d: want %b, deqChoice: %d\n", i.U, Cat(wantReadPort), readPortSrc(i))
    }
    val dynamicExuSrc = Wire(Vec(dynamicMappedValid.length, UInt(log2Ceil(staticMappedValid.length).W)))
    for (i <- 0 until dynamicMappedValid.length) {
      val targetMatch = (0 until staticMappedValid.length).map(j => readPortSrc(j) === (i + 1).U)
      dynamicExuSrc(i) := PriorityEncoder(targetMatch)
      XSDebug(p"dynamicExuSrc $i: ${dynamicExuSrc(i)} ${Binary(Cat(targetMatch))}\n")
    }
    (readPortSrc, dynamicExuSrc)
  }

  val intStaticIndex = Seq(1, 2, 3, 4)
  val intDynamicIndex = Seq(0, 5, 6)
  val intStaticMappedValid = intStaticIndex.map(i => io.intIQEnqIndex(i).valid)
  val intDynamicMappedValid = intDynamicIndex.map(i => io.intIQEnqIndex(i).valid)
  val (intReadPortSrc, intDynamicExuSrc) = RegfileReadPortArbiter(intStaticMappedValid, intDynamicMappedValid)
  val intStaticMapped = intStaticIndex.map(i => io.intIQEnqIndex(i).bits)
  val intDynamicMapped = intDynamicIndex.map(i => io.intIQEnqIndex(i).bits)
  for (i <- 0 until intStaticIndex.length) {
    val index = WireInit(VecInit(intStaticMapped(i) +: intDynamicMapped))
    io.readIntRf(2*i) := index(intReadPortSrc(i))
    io.readIntRf(2*i + 1) := index(intReadPortSrc(i))
  }
  intStaticIndex.zipWithIndex.map({case (index, i) => io.intIQRfSrc(index) := (2*i).U})
  intDynamicIndex.zipWithIndex.map({case (index, i) => io.intIQRfSrc(index) := 2.U * intDynamicExuSrc(i)})

//  assert(exuParameters.FpExuCnt > 0)
  if (exuParameters.FpExuCnt > 0) {
    val fpStaticIndex = 0 until exuParameters.FmacCnt
    val fpDynamicIndex = exuParameters.FmacCnt until exuParameters.FpExuCnt
    val fpStaticMappedValid = fpStaticIndex.map(i => io.fpIQEnqIndex(i).valid)
    val fpDynamicMappedValid = fpDynamicIndex.map(i => io.fpIQEnqIndex(i).valid)
    val (fpReadPortSrc, fpDynamicExuSrc) = RegfileReadPortArbiter(fpStaticMappedValid, fpDynamicMappedValid)
    val fpStaticMapped = fpStaticIndex.map(i => io.fpIQEnqIndex(i).bits)
    val fpDynamicMapped = fpDynamicIndex.map(i => io.fpIQEnqIndex(i).bits)
    for (i <- 0 until fpStaticIndex.length) {
      val index = WireInit(VecInit(fpStaticMapped(i) +: fpDynamicMapped))
      io.readFpRf(i) := index(fpReadPortSrc(i))
      io.fpIQRfSrc(fpStaticIndex(i)) := (3 * i).U
    }
    fpDynamicIndex.zipWithIndex.map({ case (index, i) => io.fpIQRfSrc(index) := 3.U * fpDynamicExuSrc(i) })
  }
  else {
    io.fpIQRfSrc <> DontCare
    io.readFpRf <> DontCare
  }

//  io.readIntRf(8) := io.lsIQEnqIndex(0).bits
//  io.readIntRf(9) := io.lsIQEnqIndex(1).bits
//  io.readIntRf(10) := io.lsIQEnqIndex(2).bits
//  io.readIntRf(11) := io.lsIQEnqIndex(2).bits
//  io.readIntRf(12) := io.lsIQEnqIndex(3).bits
//  io.readIntRf(13) := io.lsIQEnqIndex(3).bits
//  io.readFpRf(12) := io.lsIQEnqIndex(2).bits
//  io.readFpRf(13) := io.lsIQEnqIndex(3).bits
//  io.lsIQRfSrc(0) := 8.U
//  io.lsIQRfSrc(1) := 9.U
//  io.lsIQRfSrc(2) := 10.U
//  io.lsIQRfSrc(3) := 12.U
//  // fpu src
//  io.lsIQRfSrc(4) := 12.U
//  io.lsIQRfSrc(5) := 13.U
//  assert(exuParameters.LsExuCnt == 1)
//  io.readIntRf(8) := io.lsIQEnqIndex(0).bits
//  io.readIntRf(9) := io.lsIQEnqIndex(0).bits
//  io.readFpRf(12) := io.lsIQEnqIndex(0).bits
  io.lsIQRfSrc(0) := 8.U
  io.lsIQRfSrc(1) := 12.U

//  val lsStaticIndex = 0 until exuParameters.LsExuCnt
//  val lsDynamicIndex = 0 until 0
//  val lsStaticMappedValid = lsStaticIndex.map(i => io.lsIQEnqIndex(i).valid)
//  val lsDynamicMappedValid = lsDynamicIndex.map(i => io.lsIQEnqIndex(i).valid)
//  val (lsReadPortSrc, lsDynamicExuSrc) = RegfileReadPortArbiter(lsStaticMappedValid, lsDynamicMappedValid)
//  val lsStaticMapped = lsStaticIndex.map(i => io.lsIQEnqIndex(i).bits)
//  val lsDynamicMapped = lsDynamicIndex.map(i => io.lsIQEnqIndex(i).bits)
//  for (i <- 0 until lsStaticIndex.length) {
//    val index = WireInit(VecInit(lsStaticMapped(i) +: lsDynamicMapped))
//    if (i < exuParameters.LduCnt) {
//      val start = intStaticIndex.length*2
//      io.readIntRf(start+i) := index(lsReadPortSrc(i))
//      io.lsIQRfSrc(lsStaticIndex(i)) := (start + i).U
//    }
//    else {
//      val start = intStaticIndex.length*2 + exuParameters.LduCnt
//      io.readIntRf(start + 2 * i) := index(lsReadPortSrc(i))
//      io.readIntRf(start + 2 * i + 1) := index(lsReadPortSrc(i))
//      io.readFpRf(exuParameters.FmacCnt*3+(i-exuParameters.LduCnt)) := index(lsReadPortSrc(i))
//      io.lsIQRfSrc(lsStaticIndex(i)) := (start + 2 * i).U
//      io.lsIQRfSrc(lsStaticIndex(i)) := (start + 2 * i).U
//    }
//  }
//  assert(lsDynamicIndex.length == 0)

  val intUsedPorts = intStaticIndex.length*2 + exuParameters.LduCnt + exuParameters.StuCnt*2
  for (i <- intUsedPorts until NRIntReadPorts) {
    io.readIntRf(i) := DontCare
  }
  val fpUsedPorts = exuParameters.FmacCnt*3 + exuParameters.StuCnt
  for (i <- fpUsedPorts until NRFpReadPorts) {
    io.readFpRf(i) := DontCare
  }
}
