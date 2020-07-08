package xiangshan.frontend

import chisel3._
import chisel3.util._
import device.RAMHelper
import xiangshan._
import utils.{Debug, GTimer, XSDebug}
import xiangshan.backend.decode.isa
import xiangshan.backend.decode.Decoder

trait HasICacheConst { this: XSModule =>
  // 4-byte align * FetchWidth-inst
  val groupAlign = log2Up(FetchWidth * 4)
  def groupPC(pc: UInt): UInt = Cat(pc(VAddrBits-1, groupAlign), 0.U(groupAlign.W))
}

class FakeIcacheReq extends XSBundle {
  val addr = UInt(VAddrBits.W)
  val flush = Bool()
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
    tempPreDecoders(i).io.in.instr <> io.in(i)
    io.out.fuTypes(i) := tempPreDecoders(i).io.out.ctrl.fuType
    io.out.fuOpTypes(i) := tempPreDecoders(i).io.out.ctrl.fuOpType
  }

  io.out.mask := DontCare

}


class FakeCache extends XSModule with HasICacheConst {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new FakeIcacheReq))
    val out = DecoupledIO(new FakeIcacheResp)
  })

  val memByte = 128 * 1024 * 1024

  val ramHelpers = Array.fill(FetchWidth/2)(Module(new RAMHelper(memByte)).io)
  ramHelpers.foreach(_.clk := clock)

  //fake instruction fetch pipeline
  //----------------
  //  ICache Stage1
  //----------------
  val s1_valid = io.in.valid
  val s2_ready = WireInit(false.B)
  val s1_fire = s1_valid && s2_ready
  val gpc = groupPC(io.in.bits.addr)
  io.in.ready := s2_ready

  val offsetBits = log2Up(memByte)
  val offsetMask = (1 << offsetBits) - 1
  def index(addr: UInt): UInt = ((addr & offsetMask.U) >> log2Ceil(DataBytes)).asUInt()
  def inRange(idx: UInt): Bool = idx < (memByte / 8).U

  val ramOut = Wire(VecInit(Seq.fill(FetchWidth)(0.U(32.W))))
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

  //----------------
  //  ICache Stage2
  //----------------
  val s2_valid = RegEnable(next=s1_valid,init=false.B,enable=s1_fire)
  val s2_ram_out = RegEnable(next=ramOut,enable=s1_fire)
  val s3_ready = WireInit(false.B)
  val s2_fire  = s2_valid && s3_ready

  s2_ready := s2_fire || !s2_valid

  //----------------
  //  ICache Stage3
  //----------------
  val s3_valid = RegEnable(next=s2_valid,init=false.B,enable=s2_fire)
  val s3_ram_out = RegEnable(next=s2_ram_out,enable=s2_fire)

  s3_ready := io.out.ready

  val needflush = io.in.bits.flush

  when(needflush){
      s2_valid := false.B
      s3_valid := false.B
  }

  val tempPredecode = Module(new TempPreDecoder)
  tempPredecode.io.in := s3_ram_out

  io.out.valid := s3_valid
  io.out.bits.icacheOut := s3_ram_out
  io.out.bits.predecode := tempPredecode.io.out
}