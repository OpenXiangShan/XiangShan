package xiangshan.mem.cache

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import chisel3.util.experimental.BoringUtils
import xiangshan.backend.decode.XSTrap
import xiangshan.mem._
import xiangshan.mem.pipeline._
import bus.simplebus._

trait HasPtwConst extends HasTLBConst{
  val PtwWidth = 2
}

abstract class PtwBundle extends XSBundle with HasPtwConst
abstract class PtwModule extends XSModule with HasPtwConst

class PteBundle extends PtwBundle{
  val reserved  = UInt(pteResLen.W)
  val ppn  = UInt(ppnLen.W)
  val rsw  = UInt(2.W)
  val perm = new Bundle {
    val d    = UInt(1.W)
    val a    = UInt(1.W)
    val g    = UInt(1.W)
    val u    = UInt(1.W)
    val x    = UInt(1.W)
    val w    = UInt(1.W)
    val r    = UInt(1.W)
    val v    = UInt(1.W)
  }
}

class PtwEntry extends PtwBundle {
  val tagLen = 0 // FIXME
  val tag = UInt(tagLen.W)
  val ppn = UInt(ppnLen.W)
  val perm = new PermBundle
}

class PtwReq extends PtwBundle {
  val vpn = UInt(vpnLen.W)
  val cmd = SimpleBusCmd()
}

class PtwResp extends PtwBundle {
  val pte = UInt(XLEN.W)
  val level = UInt(log2Up(Level).W)
}

class PtwIO extends PtwBundle {
  val req = Vec(PtwWidth, Flipped(Decoupled(new PtwReq)))
  val resp = Vec(PtwWidth, Decoupled(new PtwResp))
  val sfence = Flipped(ValidIO(new SfenceBundle))
  val csr = Flipped(new TlbCsrIO)
  val mem = new DCacheLoadIO // Use Dcache temp
}

// class SeperateValidSyncReadMem extends Module {
//   val io =

//   val ram = SyncReadMem()
// }

object ValidHold {
  def apply(infire: Bool, outfire: Bool, flush: Bool = false.B ) = {
    val valid = RegInit(false.B)
    when (outfire) { valid := false.B }
    when (infire) { valid := true.B }
    when (flush) { valid := false.B } // NOTE: the flush will flush in & out, is that ok?
    valid
  }
}

object OneCycleValid {
  def apply(fire: Bool, flush: Bool = false.B) = {
    val valid = RegInit(false.B)
    when (valid) { valid := false.B }
    when (fire) { valid := true.B }
    when (false.B) { valid := false.B }
    valid
  }
}

class PTW extends PtwModule {
  val io = IO(new PtwIO)

  // io <> DontCare

  val arb = Module(new Arbiter(io.req(0).bits.cloneType, PtwWidth))
  arb.io.in <> io.req
  val arbChosen = RegEnable(arb.io.chosen, arb.io.out.fire())
  val req = RegEnable(arb.io.out.bits, arb.io.out.fire())
  val valid = ValidHold(arb.io.out.fire(), io.resp(arbChosen).fire())
  val validOneCycle = OneCycleValid(arb.io.out.fire())
  arb.io.out.ready := !valid || io.resp(arbChosen).fire()

  val mem = io.mem

  // two level: l2-tlb-cache && pde/pte-cache
  // l2-tlb-cache is ram-larger-edition tlb
  // pde/pte-cache is cache of page-table, speeding up ptw

  // may seperate valid bits to speed up sfence's flush
  // Reg/Mem/SyncReadMem is not sure now
  val tlbl2 = SyncReadMem(L2TLBEntrySize, new TLBEntry)
  val tlbv  = RegInit(VecInit(Seq.fill(L2TLBEntrySize)(false.B)).asUInt)
  // val ptwl1 = SyncReadMem(PTWL1EntrySize, new PtwEntry) // TODO: 16, could use Reg
  val ptwl1 = Reg(VecInit(Seq.fill(PTWL1EntrySize)(false.B)))
  val l1v   = RegInit(VecInit(Seq.fill(PTWL1EntrySize)(false.B)).asUInt)
  val ptwl2 = SyncReadMem(PTWL2EntrySize, new PtwEntry)
  val l2v   = RegInit(VecInit(Seq.fill(PTWL2EntrySize)(false.B)).asUInt)
  val ptwl3 = SyncReadMem(PTWL3EntrySize, new PtwEntry)
  val l3v   = RegInit(VecInit(Seq.fill(PTWL3EntrySize)(false.B)).asUInt)

  // tlbl2
  val (tlbHit, tlbHitData) = {
    // tlbl2 is by addr
    // TODO: optimize tlbl2'l2 tag len
    val ramData = tlbl2.read(req.vpn(log2Up(L2TLBEntrySize)-1, 0), validOneCycle)
    (ramData.vpn === req.vpn, ramData)
    // TODO: add exception and refill
  }

  // ptwl1
  val (l1Hit, l1HitData) = {
    // TODO: add l1 check hit
    (false.B, false.B)
  }

  val (l2Hit, l2HitData) = {
    // TODO: add l2 checkt hit
    (false.B, false.B)
  }

  val (l3Hit, l3HitData) = {
    // TODO: add l3 check hit
    (false.B, false.B)
  }

  // fsm
  val state_idle :: state_tlb/*check tlbcache/l1*/ :: state_wait1/*mem*/ :: state_l2/*check l2*/:: state_wait2/*mem*/ :: state_l3/*check l3*/ :: state_wait3/*check_l3*/ :: Nil = Enum(7)
  // FIXME: the ptw cache is stored seperately, so the check hit is seperated, fsm is seperated, ugly
  // NOTE: very simple fsm, may optimize later
  // TODO: combine these state and use 'level' to choose
  val state = RegInit(state_idle)

  // TODO: add sfence/flush. add superpage support
  switch (state) {
    is (state_idle) {
      when (valid) {
        state := state_tlb // read tlb-cache, get data next cycle
      }
    }

    is (state_tlb) {
      when (tlbHit) {
        state := state_idle // tlbHit, return
      }.elsewhen (l1Hit) {
        state := state_l2 // l1Hit, read l2 cache, get data next cycle
      }.otherwise {
        state := state_wait1 // send mem.req and wait for resp
      }
    }

    is (state_wait1) {
      when (mem.resp.fire()) {
        state := state_l2 // mem resp, read l2-cache, get data next cycle
      }
    }

    is (state_l2) {
      when (l2Hit) {
        state := state_l3 // l2 hit, read l3-cache, get data next cycle
      }.otherwise {
        state := state_wait3 // send mem.req and wait for resp
      }
    }

    is (state_wait2) {
      when (mem.resp.fire()) {
        state := state_l3 // mem resp, read l3-cache, get data next cycle
      }
    }

    is (state_l3) {
      when (l3Hit) {
        state := state_idle // finish fsm
      }.otherwise {
        state := state_wait3
      }
    }

    is (state_wait3) {
      when (mem.resp.fire()) {
        state := state_idle
      }
    }
  }

  // mem:
  // io.mem.req.apply(
  //   paddr := 0.U // TODO: add paddr
  //   vaddr := DontCare
  //   miss := DontCare
  //   user := DontCare
  // )
  // if use Dcache, how to disable VIPT -> it is hard for tlb to mem with dcache
  io.mem.req.bits := DontCare
  io.mem.req.bits.paddr := 0.U // TODO: add paddr
  io.mem.req.valid := false.B // TODO: add req.valid

  // resp
  for(i <- 0 until PtwWidth) {
    io.resp(i).valid := valid && arbChosen===i.U && false.B // TODO: add resp valid logic
    io.resp(i).bits.pte := 0.U // TODO: add resp logic
    io.resp(i).bits.level := 0.U // TODO: add resp logic
  }

  // sfence
  val sfence = io.sfence
  // for ram is syncReadMem, so could not flush conditionally
  // l3 may be conflict with l2tlb??, may be we could combine l2-tlb with l3-ptw
  when (sfence.valid) {
    tlbv := 0.U
    l1v := 0.U
    l2v := 0.U
    l3v := 0.U
  }
}