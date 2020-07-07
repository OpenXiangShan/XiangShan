package xiangshan.frontend

import chisel3._
import chisel3.util._
import device.RAMHelper
import xiangshan._
import utils.{Debug, GTimer, XSDebug}
import xiangshan.backend.decode.isa
import package xiangshan.backend.decode

trait HasICacheConst { this: XSModule =>
  // 4-byte align * FetchWidth-inst
  val groupAlign = log2Up(FetchWidth * 4)
  def groupPC(pc: UInt): UInt = Cat(pc(VAddrBits-1, groupAlign), 0.U(groupAlign.W))
}

class FakeIcacheResp extends XSBundle {
  val icacheOut = Vec(FetchWidth, UInt(32.W))
  val predecode = new Predecode
}

class TempPreDecoder extends XSModule  {
  val io = IO(new Bundle() {
    val in = Input(Vec(FetchWidth,UInt(32.W)))
    val out = Output(new Predecode)
  })
  val tempPreDecoders = Seq.fill(FetchWidth)(Module(new Decoder))

  for (i <- 0 until FetchWidth) {
    tempPreDecoders(i).io.in <> DontCare
    tempPreDecoders(i).io.in.instr <> io.in.bits(i)
    io.out.bits.fuTypes(i) := tempPreDecoders(i).io.out.fuType
    io.out.bits.fuOpType(i) := tempPreDecoders(i).io.out.fuOpType
  }

  io.out.mask := DontCare
  io.in.ready := io.out.ready

}


class FakeCache extends XSModule with HasICacheConst {
  val io = IO(new Bundle {
    val in = Fipped(DecoupledIO(UInt(VAddrBits.W))
    val out = DecoupledIO(new FakeIcacheResp)
  })

  val memByte = 128 * 1024 * 1024

  val ramHelpers = Array.fill(FetchWidth/2)(Module(new RAMHelper(memByte)).io)
  ramHelpers.foreach(_.clk := clock)

  val gpc = groupPC(io.in.bits)

  val offsetBits = log2Up(memByte)
  val offsetMask = (1 << offsetBits) - 1
  def index(addr: UInt): UInt = ((addr & offsetMask.U) >> log2Ceil(DataBytes)).asUInt()
  def inRange(idx: UInt): Bool = idx < (memByte / 8).U

  val ramOut = WireInit(Seq.fill(FetchWidth)(0.U(32.W)))
  for(i <- ramHelpers.indices) {
    val rIdx = index(gpc) + i.U
    ramHelpers(i).rIdx := rIdx
    ramOut(2*i) := ramHelpers(i).rdata.tail(32)
    ramOut(2*i+1) := ramHelpers(i).rdata.head(32)
    Seq(
      ramHelpers(i).wmask,
      ramHelpers(i).wdata,
      ramHelpers(i).wen,
      ramHelpers(i).wIdx
    ).foreach(_ := 0.U)
  }

  //fake instruction fetch pipeline
  val in_valid_delay1 = RegNext(io.in.valid)
  val in_valid_delay2 = RegNext(in_valid_delay1)

  val ramOut_delay1 = RegEnable(ramOut,io.in.valid)
  val ramOut_delay2 = RegEnable(ramOut_delay1,in_valid_delay1)

  val tempPredecode = Module(new TempPreDecoder)
  tempPredecode.io.in := ramOut_delay2

  io.in.ready := true.B
  io.out.valid := in_valid_delay2
  io.out.bits.icacheOut := ramOut_delay2
  io.out.bits.predecode := tempPredecode.io.out
}