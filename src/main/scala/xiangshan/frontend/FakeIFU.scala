package xiangshan.frontend

import chisel3._
import chisel3.util._
import device.RAMHelper
import xiangshan._
import utils.{Debug, GTimer}

trait HasIFUConst { this: XSModule =>
  val resetVector = 0x80000000L//TODO: set reset vec

  // 4-byte align * FetchWidth-inst
  val groupAlign = log2Up(FetchWidth * 4)
  def groupPC(pc: UInt): UInt = Cat(pc(VAddrBits-1, groupAlign), 0.U(groupAlign.W))
}

class FakeCache extends XSModule with HasIFUConst {
  val io = IO(new Bundle {
    val addr = Input(UInt(VAddrBits.W))
    val rdata = Output(Vec(FetchWidth, UInt(32.W)))
  })

  val memByte = 128 * 1024 * 1024

  val ramHelpers = Array.fill(FetchWidth/2)(Module(new RAMHelper(memByte)).io)
  ramHelpers.foreach(_.clk := clock)

  val gpc = groupPC(io.addr)

  val offsetBits = log2Up(memByte)
  val offsetMask = (1 << offsetBits) - 1
  def index(addr: UInt): UInt = ((addr & offsetMask.U) >> log2Ceil(DataBytes)).asUInt()
  def inRange(idx: UInt): Bool = idx < (memByte / 8).U

  for(i <- ramHelpers.indices) {
    val rIdx = index(gpc) + i.U
    ramHelpers(i).rIdx := rIdx
    io.rdata(2*i) := ramHelpers(i).rdata.tail(32)
    io.rdata(2*i+1) := ramHelpers(i).rdata.head(32)
    Seq(
      ramHelpers(i).wmask,
      ramHelpers(i).wdata,
      ramHelpers(i).wen,
      ramHelpers(i).wIdx
    ).foreach(_ := 0.U)
  }
}

class FakeIFU extends XSModule with HasIFUConst {
  val io = IO(new Bundle() {
    val fetchPacket = DecoupledIO(new FetchPacket)
    val redirect = Flipped(ValidIO(new Redirect))
  })

  val pc = RegInit(resetVector.U(VAddrBits.W))
  val pcUpdate = io.redirect.valid || io.fetchPacket.fire()

  val gpc = groupPC(pc) // fetch group's pc

  val snpc = Cat(pc(VAddrBits-1, groupAlign) + 1.U, 0.U(groupAlign.W))  // sequential next pc

  // val bpu = Module(new BPU)
  // val predRedirect = bpu.io.predMask.asUInt.orR
  // val predTarget = PriorityMux(bpu.io.predMask, bpu.io.predTargets)

  val npc = Mux(io.redirect.valid, io.redirect.bits.target, snpc) // next pc
  // val npc = Mux(io.redirect.valid, io.redirect.bits.target, Mux(predRedirect, predTarget, snpc))

  // bpu.io.redirect := io.redirect
  // bpu.io.in.pc.valid := io.fetchPacket.fire()
  // bpu.io.in.pc.bits := npc

  when(pcUpdate){
    pc := npc
  }

  val fakeCache = Module(new FakeCache)
  fakeCache.io.addr := pc

  io.fetchPacket.valid := !io.redirect.valid && (GTimer() > 500.U)
  io.fetchPacket.bits.mask := Fill(FetchWidth*2, 1.U(1.W)) << pc(2+log2Up(FetchWidth)-1, 1)
  io.fetchPacket.bits.pc := pc
  io.fetchPacket.bits.instrs := fakeCache.io.rdata
  // io.fetchPacket.bits.pnpc := bpu.io.predTargets
  io.fetchPacket.bits.pnpc := DontCare

  Debug(cond=io.fetchPacket.fire()){
    printf(p"==========FetchGroup==========\nfirst pc:${Hexadecimal(pc)}\n")
    for(i <- io.fetchPacket.bits.instrs.indices){
      printf(p"inst$i: ${Hexadecimal(io.fetchPacket.bits.instrs(i))} v:${io.fetchPacket.bits.mask(i)} isRVC:${io.fetchPacket.bits.instrs(i)(1,0)=/="b11".U}\n")
    }
  }
}
