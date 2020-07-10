package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.utils.{XSDebug}

class RegfileReadPortGen extends XSModule {
  val io = IO(new Bundle() {
    // from dispatch queues
    val enqIQIndex = Flipped(Vec(exuConfig.ExuCnt, ValidIO(UInt(log2Ceil(IntDqDeqWidth).W))))

    val readIntRf = Output(Vec(NRReadPorts, UInt(log2Ceil(IntDqDeqWidth).W)))
    val intIQRfSrc = Output(Vec(exuConfig.ExuCnt, UInt(log2Ceil(NRReadPorts).W)))
    val readFpRf = Output(Vec(NRReadPorts, UInt(log2Ceil(IntDqDeqWidth).W)))
  })
  io.readIntRf <> DontCare
  io.readFpRf <> DontCare
  io.intIQRfSrc <> DontCare

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
  val intStaticMappedValid = intStaticIndex.map(i => io.enqIQIndex(i).valid)
  val intDynamicMappedValid = intDynamicIndex.map(i => io.enqIQIndex(i).valid)
  val (intReadPortSrc, intDynamicExuSrc) = RegfileReadPortArbiter(intStaticMappedValid, intDynamicMappedValid)

  val intStaticMapped = intStaticIndex.map(i => io.enqIQIndex(i).bits)
  val intDynamicMapped = intDynamicIndex.map(i => io.enqIQIndex(i).bits)
  for (i <- 0 until intStaticIndex.length) {
    val index = WireInit(VecInit(intStaticMapped(i) +: intDynamicMapped))
    io.readIntRf(i) := index(intReadPortSrc(i))
  }
  io.intIQRfSrc(0) := 2.U * intDynamicExuSrc(0)
  io.intIQRfSrc(1) := 2.U * 0.U
  io.intIQRfSrc(2) := 2.U * 1.U
  io.intIQRfSrc(3) := 2.U * 2.U
  io.intIQRfSrc(4) := 2.U * 3.U
  io.intIQRfSrc(5) := 2.U * intDynamicExuSrc(1)
  io.intIQRfSrc(6) := 2.U * intDynamicExuSrc(2)
}
