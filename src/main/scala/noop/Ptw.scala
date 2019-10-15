package noop

import chisel3._
import chisel3.util._
import chisel3.util.random._
import chisel3.util.experimental.BoringUtils

import bus.simplebus._
import bus.axi4._
import utils._

trait Sv32Const {
  val debug = false

  val vpnLen = 20
  val ppnLen = 22
}

trait Sv39Const {
  val debug = true

  val vpnLen = 27
  val ppnLen = 44
}

trait pteSv32Const extends Sv32Const{
  val Level = 2 
  val PPN1Len = 12 
  val PPN0Len = 10
  val PageSizeLen = 12 //4K
  val satpLen = 32
  val paddrLen = 34
  val vaddrLen = 32
  val ptEntryLen = 32

  def pteBundle = new Bundle {
    val ppn1  = UInt(12.W)
    val ppn0  = UInt(10.W)
    val rsw   = UInt(2.W)
    val D     = UInt(1.W)
    val A     = UInt(1.W)
    val G     = UInt(1.W)
    val U     = UInt(1.W)
    val X     = UInt(1.W)
    val W     = UInt(1.W)
    val R     = UInt(1.W)
    val V     = UInt(1.W)
  }

  def vaBundle = new Bundle {
    val vpn1  = UInt(10.W)
    val vpn0  = UInt(10.W)
    val off = UInt(12.W)
  }

  def paBundle = new Bundle {
    val PPN1  = UInt(12.W)
    val PPN0  = UInt(10.W)
    val pgoff = UInt(12.W)
  }

  def satpBundle = new Bundle {
    val MODE  = UInt(1.W)
    val ASID  = UInt(9.W)
    val PPN0  = UInt(22.W)
  }
}

trait pteSv39Const extends Sv39Const{
  val Level = 3
  val ppn2Len = 26
  val ppn1Len = 9
  val ppn0Len = 9
  val offLen  = 12
  val vpn2Len = 9
  val vpn1Len = 9
  val vpn0Len = 9
  val vaResLen= 25
  val paResLen= 8

  val paddrLen = 64
  val vaddrLen = 64
  val satpLen = 64
  val satpModeLen = 4
  val asidLen = 16
  val ptEntryLen = 64

  def vaBundle = new Bundle {
    val reserverd = UInt(vaResLen.W)
    val vpn2 = UInt(vpn2Len.W)
    val vpn1 = UInt(vpn1Len.W)
    val vpn0 = UInt(vpn0Len.W)
    val off  = UInt( offLen.W)
  }

  def vaBundle2 = new Bundle {
    val reserverd = UInt(vaResLen.W)
    val vpn  = UInt(vpnLen.W)
    val off  = UInt(offLen.W)
  }

  def paBundle = new Bundle {
    val reserved = UInt(paResLen.W)
    val ppn2 = UInt(ppn2Len.W)
    val ppn1 = UInt(ppn1Len.W)
    val ppn0 = UInt(ppn0Len.W)
    val off  = UInt( offLen.W)
  }

  def paBundle2 = new Bundle {
    val reserved = UInt(paResLen.W)
    val ppn  = UInt(ppnLen.W)
    val off  = UInt(offLen.W)
  }

  def pteBundle = new Bundle {
    val reserved  = UInt(10.W)
    val ppn  = UInt(ppnLen.W)
    val rsw  = UInt(2.W)
    val D    = UInt(1.W)
    val A    = UInt(1.W)
    val G    = UInt(1.W)
    val U    = UInt(1.W)
    val X    = UInt(1.W)
    val W    = UInt(1.W)
    val R    = UInt(1.W)
    val V    = UInt(1.W)
  }

  def satpBundle = new Bundle {
    val mode = UInt(satpModeLen.W)
    val asid = UInt(asidLen.W)
    val ppn  = UInt(ppnLen.W)
  }
}

trait pteConst extends pteSv39Const

trait tlbSv32Const extends Sv32Const{
  val tlbEntryNum = 8
  val tlbEntryNumLen = 3
  val tlbEntryLen = 59
  val tlbAsidLen = 9

  def tlbBundle = new Bundle {
    val vpn = UInt(vpnLen.W)
    val asid = UInt(tlbAsidLen.W)
    val ppn = UInt(ppnLen.W)
    val D   = UInt(1.W)
    val A   = UInt(1.W)
    val G   = UInt(1.W)
    val U   = UInt(1.W)
    val X   = UInt(1.W)
    val W   = UInt(1.W)
    val R   = UInt(1.W)
    val V   = UInt(1.W)
  }
}

trait tlbSv39Const extends Sv39Const{
  val tlbEntryNum = 8
  val tlbEntryLen = 95
  val tlbAsidLen = 16

  def tlbBundle = new Bundle {
    val vpn  = UInt(vpnLen.W)
    val asid = UInt(tlbAsidLen.W)
    val ppn  = UInt(ppnLen.W)
    val D    = UInt(1.W) 
    val A    = UInt(1.W)
    val G    = UInt(1.W)
    val U    = UInt(1.W)
    val X    = UInt(1.W)
    val W    = UInt(1.W)
    val R    = UInt(1.W)
    val V    = UInt(1.W)
  }
}

trait tlbConst extends tlbSv39Const

class Ptw(name : String = "default", userBits:Int = 0) extends Module with pteConst with tlbConst {
  val io = IO(new Bundle {
    val satp = Input(UInt(satpLen.W))
    val flush = Input(Bool())
    val in   = Flipped(new SimpleBusUC(userBits))
    val out  = new SimpleBusUC(userBits)
  })

  val s_ready :: s_tran :: s_walk :: s_mem :: s_error :: s_notran :: Nil = Enum(6)
  val state = RegInit(s_ready)
  val phyNum = RegInit(0.U(paddrLen.W))
  val alreadyOutFire = RegEnable(true.B, io.out.req.fire())
  val __isWork = io.satp.asTypeOf(satpBundle).mode =/= 0.U
  val _isWork = RegEnable(__isWork, state===s_ready && io.in.req.fire()) //hold the satp(31) to aviod sudden change.
  val isWork = Mux(state===s_ready, __isWork, _isWork) //isWork control the 
  val needFlush = RegInit(false.B) // needFlush: set when encounter a io.flush; work when after an access memory series ends; reset when return to s_ready. the io.in.resp.valid is true at mem, so we can jump to s_ready directly or low down the valid.
  val flushEnable = needFlush || io.flush //use in s_walk s_mem s_notran, which needs several cycles

  val wire_tmp = 0.U(32.W)//Wire(0.U(34.W))

  val updateStore = state===s_ready && io.in.req.fire() && !io.flush
  val vaddr = RegEnable(io.in.req.bits.addr, updateStore) // maybe just need the fire() signal
  val inReqBitsCmd  = RegEnable(io.in.req.bits.cmd, updateStore)
  val inReqBitsWmask = RegEnable(io.in.req.bits.wmask, updateStore)
  val inReqBitsWdata = RegEnable(io.in.req.bits.wdata, updateStore)
  val inReqBitsUser = RegEnable(io.in.req.bits.user.getOrElse(wire_tmp), updateStore)
  val inReqBitsSize = RegEnable(io.in.req.bits.size, updateStore)
  //store end

  val tlbEntry = Mem(tlbEntryNum, UInt(tlbEntryLen.W))
  val tlbHitAll = (0 until tlbEntryNum).map(i => tlbEntry(i).asTypeOf(tlbBundle).vpn===vaddr.asTypeOf(vaBundle2).vpn && tlbEntry(i).asTypeOf(tlbBundle).V.asBool && tlbEntry(i).asTypeOf(tlbBundle).asid===io.satp.asTypeOf(tlbBundle).asid)
  val tlbHit = (state===s_tran) && tlbHitAll.reduce(_||_)
  val tlbHitIndex = Mux1H(tlbHitAll, (0 until tlbEntryNum).map(_.U))
  val tlbHitPPN = Mux1H(tlbHitAll, (0 until tlbEntryNum).map(i => tlbEntry(i).asTypeOf(tlbBundle).ppn))
  val rand3Bit = RegInit(0.U(3.W))

  io.in.resp.bits.rdata := io.out.resp.bits.rdata
  io.in.resp.bits.user.map(_ := io.out.resp.bits.user.getOrElse(wire_tmp))
  io.in.resp.bits.cmd   := io.out.resp.bits.cmd  
  io.in.resp.valid      := Mux(isWork, state===s_mem && !flushEnable && io.out.resp.valid, io.out.resp.valid && state===s_notran && !flushEnable)
  io.out.resp.ready     := Mux(isWork, (state===s_walk || state===s_mem), state===s_notran)
  
  io.out.req.bits.addr  := Mux(isWork, phyNum, vaddr)
  io.out.req.bits.cmd   := Mux(isWork, Mux(state===s_walk, SimpleBusCmd.read, inReqBitsCmd), inReqBitsCmd)
  io.out.req.bits.wmask := Mux(isWork, inReqBitsWmask, inReqBitsWmask)
  io.out.req.bits.wdata := Mux(isWork, inReqBitsWdata, inReqBitsWdata)
  io.out.req.bits.user.map(_ := Mux(isWork, inReqBitsUser, inReqBitsUser))
  io.out.req.bits.size  := Mux(isWork, inReqBitsSize, inReqBitsSize)
  io.out.req.valid      := Mux(isWork, (state===s_walk && !alreadyOutFire|| state===s_mem && !alreadyOutFire), state===s_notran && !alreadyOutFire)//need add state machine
  io.in.req.ready       := Mux(isWork, state===s_ready && io.out.req.ready, io.out.req.ready && state===s_ready)
  //connect end

  //s_ready : free state
  //s_tran  : judge if tlbhit or not
  //s_walk  : the work stage(go and get the pte). In Sv39..., have the third state or else
  //s_mem   : already get the paddr, then access the mem to get the data, maybe just 
  //s_error : error state, raise an exception, unknown how to do
  
  val level = RegInit(Level.U)
  switch (state) {
    is (s_ready) {
      when(io.in.req.fire() && __isWork && !io.flush ) {
        state := s_tran
        level := Level.U
      }.elsewhen(io.in.req.fire() && !__isWork && !io.flush) {
        state := s_notran
        alreadyOutFire := false.B
      }
    }

    is (s_tran) {
      when (io.flush) {
        state := s_ready
        alreadyOutFire := false.B
      }.elsewhen(tlbHit) {
        state := s_mem
        phyNum := Cat(0.U(paResLen.W), Cat(tlbHitPPN, vaddr.asTypeOf(vaBundle).off))
        alreadyOutFire := false.B
      }.otherwise {
        state := s_walk
        level := Level.U
        phyNum := Cat(0.U(paResLen.W), Cat(io.satp.asTypeOf(satpBundle).ppn, Cat(vaddr.asTypeOf(vaBundle).vpn2, 0.U(3.W))))
        alreadyOutFire := false.B
      }
    }

    is (s_walk) {
      when(/*level =/= 0.U && */io.out.resp.fire()) {
        when(flushEnable) {
          needFlush := false.B
          state := s_ready
          level := 2.U
          alreadyOutFire := false.B
        }.otherwise {
          switch(level) {
            is (3.U) {
              phyNum := Cat(0.U(paResLen.W), Cat(io.out.resp.bits.rdata.asTypeOf(pteBundle).ppn, Cat(vaddr.asTypeOf(vaBundle).vpn1, 0.U(3.W))))
            }
            is (2.U) {
              phyNum := Cat(0.U(paResLen.W), Cat(io.out.resp.bits.rdata.asTypeOf(pteBundle).ppn, Cat(vaddr.asTypeOf(vaBundle).vpn0, 0.U(3.W)))) //maybe wrong ,for page table has 2^9 entry not 2^10
            }
            is (1.U) {
              state := s_mem
              phyNum:= Cat(0.U(paResLen.W), Cat(io.out.resp.bits.rdata.asTypeOf(pteBundle).ppn, vaddr.asTypeOf(vaBundle).off))
              rand3Bit := rand3Bit+1.U
              tlbEntry(rand3Bit) := Cat( Cat(vaddr.asTypeOf(vaBundle2).vpn, io.satp.asTypeOf(tlbBundle).asid), Cat(io.out.resp.bits.rdata.asTypeOf(pteBundle).ppn, io.out.resp.bits.rdata(7,0))) //need change
            }
            is (0.U) {
              state := s_error
            }
          }
          level := level - 1.U
          alreadyOutFire := false.B
          //Sv32 page table entry: 0:V 1:R 2:W 3:X 4:U 5:G 6:A 7:D
        }
      }.elsewhen(io.flush) {
        needFlush := true.B
      }
    }
    is (s_error) {
      state := s_ready
    }
    
    is (s_mem) {
      when(io.out.resp.fire()) {
        state := s_ready
        level := 3.U
        alreadyOutFire := false.B
        needFlush := false.B
      }.elsewhen(io.flush) {
        needFlush := true.B
      }
    }

    is (s_notran) {
      when(io.out.resp.fire()) {
        alreadyOutFire := false.B
        state := s_ready
        needFlush := false.B
      }.elsewhen(io.flush) {
        needFlush := true.B
      }
    }
  }
  
  Debug(debug && name=="iptw") {
    
    val alreadyWork = RegInit(false.B)
    
    when( __isWork || alreadyWork) {
      printf(name + "%d: PTW state:%d lev:%d vaddr:%x phy:%x rdata:%x",GTimer(),state,level,vaddr,phyNum,io.out.resp.bits.rdata)
      printf(" needFlush:%d io.flush:%d ",needFlush,io.flush)
      printf(" inReqAddr: %x ", io.in.req.bits.addr)
      printf(" inReqFire:%d inRespFire:%d outReqFire:%d outRespFire:%d", io.in.req.fire(), io.in.resp.fire(),io.out.req.fire(),io.out.resp.fire())
      printf(" alreadyOutFire:%d", alreadyOutFire)
      //printf(" satp:%x ", io.satp)
      printf(" updateStore:%d __isWork:%d _isWork:%d isWork:%d",updateStore,__isWork,_isWork,isWork)
      //printf(" tlbEntry(%d):%x tlbHit:%d tlbvaddr:%x tlbpaddr:%x ", tlbHitIndex, tlbEntry(tlbHitIndex), tlbHit, tlbEntry(tlbHitIndex).asTypeOf(tlbBundle).vpn, tlbEntry(tlbHitIndex).asTypeOf(tlbBundle).ppn)
      printf("\n")
    }
    when(__isWork && !alreadyWork) {
      printf(name + "%d: alreadyWork", GTimer())
      printf("\n")
      alreadyWork := true.B
    }
  }

  Debug(debug) {
    when(state===s_mem && io.out.req.fire().asBool && vaddr=/=phyNum) {
      printf(name + "%d: state:%d, out.req.fire:%d, vaddr:%x, phyNum:%x\n",GTimer(),state,io.out.req.fire(),vaddr,io.out.req.bits.addr)
    }
    assert((state===s_mem && io.out.req.fire().asBool && vaddr===phyNum) || state=/=s_mem || !io.out.req.fire().asBool)
  }
}