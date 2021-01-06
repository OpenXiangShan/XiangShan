package xiangshan.frontend

import chisel3._
import chisel3.util._
import device.RAMHelper
import xiangshan._

trait HasICacheConst { this: XSModule =>
  // 4-byte align * FetchWidth-inst
  val groupAlign = log2Up(FetchWidth * 4 * 2)
  def groupPC(pc: UInt): UInt = Cat(pc(VAddrBits-1, groupAlign), 0.U(groupAlign.W))
  def mask(pc: UInt): UInt = (Fill(PredictWidth * 2, 1.U(1.W)) >> pc(groupAlign - 1, 1))(PredictWidth - 1, 0)
}

class FakeIcacheReq extends XSBundle {
  val addr = UInt(VAddrBits.W)
}

class FakeIcacheResp extends XSBundle {
  val data = UInt(64.W)
  val finish = Bool()

}

class FakeCache extends XSModule with HasICacheConst {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new FakeIcacheReq))
    val out = DecoupledIO(new FakeIcacheResp)
  })

  val memByte = 128 * 1024 * 1024

  val ramHelpers = Module(new RAMHelper(memByte)).io
  ramHelpers.clk := clock

  //fake instruction fetch pipeline
  //----------------
  //  ICache Stage1
  //----------------
  val gpc = io.in.bits.addr //use fetch pc
  io.in.ready := true.B

  val offsetBits = log2Up(memByte)
  val offsetMask = (1 << offsetBits) - 1
  def index(addr: UInt): UInt = ((addr & offsetMask.U) >> log2Ceil(DataBytes)).asUInt()
  def inRange(idx: UInt): Bool = idx < (memByte / 8).U

  val s_idle :: s_mem_read :: Nil = Enum(2)
  val state = RegInit(s_idle)
  val beatCounter = RegInit(0.U(3.W))
  
  io.out.bits.finish := false.B
  switch(state){
    is(s_idle) {when(io.in.fire){state := s_mem_read}}
    is(s_mem_read){
      beatCounter := beatCounter + 1.U
      when(beatCounter === 7.U){
        state := s_idle
        beatCounter := 0.U
        io.out.bits.finish := true.B
      }
    }
  }

  val rIdx = index(gpc) + beatCounter
  ramHelpers.rIdx := rIdx
  Seq(
      ramHelpers.wmask,
      ramHelpers.wdata,
      ramHelpers.wen,
      ramHelpers.wIdx
  ).foreach(_ := 0.U)

  io.out.valid := (state === s_mem_read)
  io.out.bits.data := ramHelpers.rdata
}