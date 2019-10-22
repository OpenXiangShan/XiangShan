package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
import bus.simplebus._

trait HasResetVector {
  val resetVector = 0x80000000L//TODO: set reset vec
}

class IFU extends NOOPModule with HasResetVector {
  val io = IO(new Bundle {

    val imem = new SimpleBusUC(userBits = AddrBits*2 + 4)
    // val pc = Input(UInt(AddrBits.W))
    val out = Decoupled(new CtrlFlowIO)

    val redirect = Flipped(new RedirectIO)
    val redirectRVC = Flipped(new RedirectIO)//priority: redirect > redirectRVC
    val flushVec = Output(UInt(4.W))
    val bpFlush = Output(Bool())
  })

  // pc
  val pc = RegInit(resetVector.U(AddrBits.W))
  val pcUpdate = io.redirect.valid || io.imem.req.fire()
  val snpc = Mux(pc(1), pc + 2.U, pc + 4.U)  // sequential next pc

  val bp1 = Module(new BPU1)

  //
  val lateJump = bp1.io.lateJump
  val lateJumpLatch = RegInit(false.B) 
  when(io.out.fire() || bp1.io.flush) {
    lateJumpLatch := Mux(bp1.io.flush, false.B, lateJump)
  }
  val lateJumpTarget = RegEnable(bp1.io.out.target, lateJump)
  val lateJumpForceSeq = lateJump && bp1.io.out.valid
  val lateJumpForceTgt = lateJumpLatch && !bp1.io.flush

  // predicted next pc
  val pnpc = Mux(lateJump, snpc, bp1.io.out.target)
  val pbrIdx = bp1.io.brIdx
  val npc = Mux(io.redirect.valid, io.redirect.target, Mux(lateJumpLatch, lateJumpTarget, Mux(bp1.io.out.valid, pnpc, snpc)))
  val npcIsSeq = Mux(io.redirect.valid , false.B, Mux(lateJumpLatch, false.B, Mux(lateJump, true.B, Mux(bp1.io.out.valid, false.B, true.B))))
  // val npc = Mux(io.redirect.valid, io.redirect.target, Mux(io.redirectRVC.valid, io.redirectRVC.target, snpc))
  val brIdx = Wire(UInt(4.W)) 
  // brIdx(0) -> branch at pc offset 0 (mod 4)
  // brIdx(1) -> branch at pc offset 2 (mod 4)
  // brIdx(2) -> branch at pc offset 6 (mod 8), and this inst is not rvc inst
  brIdx := Cat(npcIsSeq, Mux(io.redirect.valid, 0.U, pbrIdx))
  //TODO: BP will be disabled shortly after a redirect request

  bp1.io.in.pc.valid := io.imem.req.fire() // only predict when Icache accepts a request
  bp1.io.in.pc.bits := npc  // predict one cycle early
  // bp1.io.flush := io.redirect.valid 
  bp1.io.flush := io.redirect.valid
  //val bp2 = Module(new BPU2)
  //bp2.io.in.bits := io.out.bits
  //bp2.io.in.valid := io.imem.resp.fire()

  when (pcUpdate) { 
    pc := npc 
    // printf("[IF1] pc=%x\n", pc)
  }

  io.flushVec := Mux(io.redirect.valid, "b1111".U, 0.U)
  io.bpFlush := false.B

  io.imem.req.bits.apply(addr = Cat(pc(AddrBits-1,1),0.U(1.W)), //cache will treat it as Cat(pc(63,3),0.U(3.W))
    size = "b11".U, cmd = SimpleBusCmd.read, wdata = 0.U, wmask = 0.U, user = Cat(brIdx(3,0), npc, pc))
  io.imem.req.valid := io.out.ready

  io.imem.resp.ready := io.out.ready || io.flushVec(0)

  Debug(){
    when(io.imem.req.fire()){
      printf("[IFI] pc=%x user=%x %x %x %x\n", io.imem.req.bits.addr, io.imem.req.bits.user.getOrElse(0.U), io.redirect.valid, pbrIdx, brIdx)
    }
  }

  io.out.bits := DontCare
    //inst path only uses 32bit inst, get the right inst according to pc(2)

  Debug(){
    when (io.out.fire()) {
          printf("[IFO] pc=%x inst=%x\n", io.out.bits.pc, io.out.bits.instr)
    }
  }

  // io.out.bits.instr := (if (XLEN == 64) io.imem.resp.bits.rdata.asTypeOf(Vec(2, UInt(32.W)))(io.out.bits.pc(2))
                      //  else io.imem.resp.bits.rdata)
  io.out.bits.instr := io.imem.resp.bits.rdata
  io.imem.resp.bits.user.map{ case x =>
    io.out.bits.pc := x(AddrBits-1,0)
    io.out.bits.pnpc := x(AddrBits*2-1,AddrBits)
    io.out.bits.brIdx := x(AddrBits*2 + 3, AddrBits*2)
  }
  io.out.valid := io.imem.resp.valid && !io.flushVec(0)

  BoringUtils.addSource(BoolStopWatch(io.imem.req.valid, io.imem.resp.fire()), "perfCntCondMimemStall")
  BoringUtils.addSource(io.flushVec.orR, "perfCntCondMifuFlush")
}
