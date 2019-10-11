/*
package noop

import chisel3._
import chisel3.util._
import chisel3.util.random._
import chisel3.util.experimental.BoringUtils

import bus.simplebus._
import bus.axi4._
import utils._

trait pteSv32Const {
  val Level = 2 //Sv32 two layer page tree
  val PPN1Len = 12 //12???
  val PPN0Len = 10
  val PageSizeLen = 12 //4K

  val debug = true

  def pteBundle = new Bundle {
    val PPN1  = UInt(12.W)
    val PPN2  = UInt(10.W)
    val RSW   = UInt(2.W)
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
    val VPN1  = UInt(10.W)
    val VPN0  = UInt(10.W)
    val pgoff = UInt(12.W)
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

trait tlbSv32Const {
  val VPNLen = 20
  val PPNLen = 22
  val PPNNum = 1 //1
  val tlbEntryNum = 8 //tmp
  val tlbEntryLen = 59 //
  
  def tlbBundle = new Bundle {
    val VPN = UInt(20.W)
    val ASID = UInt(9.W)
    val PPN = UInt(22.W)
    val D   = UInt(1.W)
    val A   = UInt(1.W)
    val G   = UInt(1.W)
    val U   = UInt(1.W)
    val X   = UInt(1.W)
    val W   = UInt(1.W)
    val R   = UInt(1.W)
    val V   = UInt(1.W)
  }

  //def isTlbEntryHit(tlbEntry:tlbBundle, vaddr:UInt) : UInt = {
  //  tlbEntry.VPN===vaddr(31,12) && tlbEntry.V.asBool
  //}
}

class PtwSv32(name : String = "default", userBits:Int) extends Module with pteSv32Const with tlbSv32Const {
  val io = IO(new Bundle {
    val satp = Input(UInt(32.W))
    val flush = Input(Bool())
    val in   = Flipped(new SimpleBusUC(userBits))
    val out  = new SimpleBusUC(userBits)
  })

  val s_ready :: s_tran :: s_walk :: s_mem :: s_error :: Nil = Enum(5)
  val state = RegInit(s_ready)
  val phyNum = Reg(UInt(32.W))
  val alreadyOutFire = RegEnable(true.B, io.out.req.fire())
  val _isWork = RegEnable(io.satp(31).asBool, state===s_ready && io.in.req.fire()) //hold the satp(31) to aviod sudden change.
  val isWork = Mux(state===s_ready, io.satp(31).asBool, _isWork) //isWork control the 
  val needFlush = RegInit(false.B) // needFlush: set when encounter a io.flush; work when after an access memory series ends; reset when return to s_ready. the io.in.resp.valid is true at mem, so we can jump to s_ready directly or low down the valid.
  
  val updateStore = state===s_ready && io.in.req.fire() && io.satp(31).asBool && !io.flush
  val vaddr = RegEnable(io.in.req.bits.addr, updateStore) // maybe just need the fire() signal
  val inReqBitsCmd  = RegEnable(io.in.req.bits.cmd, updateStore)
  val inReqBitsWmask = RegEnable(io.in.req.bits.wmask, updateStore)
  val inReqBitsWdata = RegEnable(io.in.req.bits.wdata, updateStore)
  val inReqBitsUser = RegEnable(io.in.req.bits.user, updateStore)
  val inReqBitsSize = RegEnable(io.in.req.bits.size, updateStore)
  //store end

  val tlbEntry = Mem(tlbEntryNum, UInt(tlbEntryLen.W))/*Seq(tlbEntryNum, RegInit(0.U(59.W))) *//*Seq.fill(tlbEntryNum)(RegInit(0.U(tlbEntryLen.W)))*/
  val tlbHitAll = (0 until tlbEntryNum).map(i => tlbEntry(i).asTypeOf(tlbBundle).VPN===vaddr(31,12) && tlbEntry(i).asTypeOf(tlbBundle).V.asBool)
  //val tlbHitAll = tlbEntry.map(a => (a(58,39)===vaddr(31,12)&&a(0).asBool))
  //val tlbHitAll = tlbEntry.map(a:UInt => a.asTypeOf(tlbBundle).VPN===vaddr(31.12) && a.asTypeOf(tlbBundle).V.asBool)
  val tlbHit = (state===s_tran) && tlbHitAll.reduce(_||_)
  val tlbHitIndex = Mux1H(tlbHitAll, (0 until tlbEntryNum).map(_.U))
  val tlbPPageNum = Mux1H(tlbHitAll, (0 until tlbEntryNum).map(i => tlbEntry(i).asTypeOf(tlbBundle).PPN))
  val rand3Bit = RegInit(0.U(3.W))
  //val rand3Bit = RegNext(GaloisLFSR.maxPeriod(3))
  //val tlbHit = state===s_tran && tlbEntry.map(this.asTypeOf(tlbBundle).VPN===vaddr(31,12) && this.asTypeOf(tlbBundle).V.asBool).reduce(_ | _)
  //val tlbHitIndex = tlbEntry.map(this.asTypeOf(tlbBundle
  //val tlbEntry = RegInit(0.U(59.W))
  //val tlbHit = state===s_tran && (vaddr(31,12)===tlbEntry.asTypeOf(tlbBundle).VPN && tlbEntry.asTypeOf(tlbBundle).V.asBool)

  //connect begin
  //out      <<     ptw     >>     in
  //out.resp.valid   >>     in.resp.valid
  //out.resp.ready   <<     in.resp.ready
  //out.resp.bits    >>     in.resp.bits
  io.in.resp.bits.rdata := io.out.resp.bits.rdata
  io.in.resp.bits.user  := io.out.resp.bits.user 
  io.in.resp.bits.cmd   := io.out.resp.bits.cmd  
  io.in.resp.valid      := Mux(isWork, state===s_mem && !needFlush && io.out.resp.valid, io.out.resp.valid)
  io.out.resp.ready     := Mux(isWork, (state===s_walk || state===s_mem), io.in.resp.ready)
  //out      <<     ptw     >>    in
  //out.req.valid    <<     in.req.valid
  //out.req.ready    >>     in.req.ready
  //out.req.bits     <<     in.req.bits
  io.out.req.bits.addr  := Mux(isWork, phyNum, io.in.req.bits.addr)
  io.out.req.bits.cmd   := Mux(isWork, Mux(state===s_walk, SimpleBusCmd.read, inReqBitsCmd), io.in.req.bits.cmd)
  io.out.req.bits.wmask := Mux(isWork, inReqBitsWmask, io.in.req.bits.wmask)
  io.out.req.bits.wdata := Mux(isWork, inReqBitsWdata, io.in.req.bits.wdata)
  io.out.req.bits.user  := Mux(isWork, inReqBitsUser, io.in.req.bits.user)
  io.out.req.bits.size  := Mux(isWork, inReqBitsSize, io.in.req.bits.size)
  io.out.req.valid      := Mux(isWork, (state===s_walk && !alreadyOutFire|| state===s_mem && !alreadyOutFire), io.in.req.valid)//need add state machine
  io.in.req.ready       := Mux(isWork, state===s_ready && io.out.req.ready, io.out.req.ready)
  //connect end

  val level = RegInit(2.U)

  //state machine: does instr and data need two ptw?? maybe one is enough, so how to handle two input
  //s_ready : free state
  //s_tran  : judge if tlbhit or not
  //s_walk  : the work stage(go and get the pte). In Sv39..., have the third state or else
  //s_mem   : already get the paddr, then access the mem to get the data, maybe just 
  //s_error : error state, raise an exception, unknown how to do

  val last_rdata = RegInit(0.U) //no use, debug
  
  switch (state) {
    is (s_ready) {
      when(io.in.req.fire() && io.satp(31).asBool && !io.flush ) {
        state := s_tran
      }
    }

    is (s_tran) {
      when (io.flush) {
        state := s_ready
        alreadyOutFire := false.B
      }.elsewhen(tlbHit) {
        state := s_mem
        //phyNum := Cat(tlbEntry.asTypeOf(tlbBundle).PPN(19,0), vaddr(11,0))
        phyNum := Cat(tlbPPageNum(19,0), vaddr(11,0))
        alreadyOutFire := false.B
      }.otherwise {
        state := s_walk
        phyNum := Cat(io.satp(19,0), Cat(vaddr(31,22), 0.U(2.W)))
        alreadyOutFire := false.B
      }
    }

    is (s_walk) {
      when(level =/= 0.U && io.out.resp.fire()/*访存page握手结束*/ /*&& phyNum(3,1)=/= 0.U(3.W)*/) {
        when(needFlush || io.flush) {
          needFlush := false.B
          state := s_ready
          level := 2.U
          alreadyOutFire := false.B
          last_rdata := 0.U
        }.otherwise {
          level := level - 1.U
          alreadyOutFire := false.B
          //Sv32 page table entry: 0:V 1:R 2:W 3:X 4:U 5:G 6:A 7:D
          state := Mux(level===1.U, s_mem, s_walk)
          phyNum := Mux(level===1.U, Cat(io.out.resp.bits.rdata(29,10), vaddr(11,0)), Cat(io.out.resp.bits.rdata(29,10), Cat(vaddr(21,12), 0.U(2.W)))) 
          when(level===1.U) {
            rand3Bit := rand3Bit+1.U
            tlbEntry(rand3Bit) := Cat( Cat(vaddr(31,12), 0.U(9.W)), Cat(io.out.resp.bits.rdata(31,10), io.out.resp.bits.rdata(7,0)))
            //tlbEntry(58,39) := vaddr(31,12)
            //tlbEntry(29,8) := io.out.resp.bits.rdata(31,10)
            //tlbEntry(7,0) := io.out.resp.bits.rdata(7,0)
          }
        }
        //state := s_mem
      }.elsewhen(io.flush) {
        needFlush := true.B
      }
    }
    is (s_error) {
      //raise an exception
      state := s_ready
    }
    
    is (s_mem) {
      when(io.out.resp.fire()) {
        state := s_ready
        level := 2.U
        last_rdata := 0.U
        alreadyOutFire := false.B
        needFlush := false.B
      }.elsewhen(io.flush) {
        needFlush := true.B
      }
    }
  }
  
  val count = RegInit(0.U(16.W))
  val isCount = RegInit(false.B)

  Debug(debug && name=="iptw" && false) {
    when( true.B/* && state===s_mem && io.out.req.fire().asBool*/) {
      printf(name + "%d: PTW state:%d lev:%d vaddr:%x phy:%x needFlush:%d io.flush:%d rdata:%x inRespFire:%d outReqFire:%d outRespFire:%d ",GTimer(),state,level,vaddr,phyNum,needFlush,io.flush,io.out.resp.bits.rdata,io.in.resp.fire(),io.out.req.fire(),io.out.resp.fire())
      printf(" tlbEntry(%d):%x tlbHit:%d tlbvaddr:%x tlbpaddr:%x ", tlbHitIndex, tlbEntry(tlbHitIndex), tlbHit, tlbEntry(tlbHitIndex)(58,39), tlbEntry(tlbHitIndex)(27,8))
      printf("\n")
        // printf("inReqValid:%d inReqReady:%d isWork:%d\n",io.in.req.valid, io.in.req.ready, isWork)
      //when(isCount===false.B) {isCount := true.B}
    }
    when(state===s_mem && io.out.req.fire().asBool && vaddr=/=phyNum) {
      //printf(p"${GTimer()}, state:${state}, out.resp.fire:${io.out.resp.fire()}, vaddr:${vaddr}, rdata:${io.out.resp.bits.rdata}\n")
      printf(name + "%d: state:%d, out.req.fire:%d, vaddr:%x, phyNum:%x\n",GTimer(),state,io.out.req.fire(),vaddr,io.out.req.bits.addr)
    }
    assert((state===s_mem && io.out.req.fire().asBool && vaddr===phyNum) || state=/=s_mem || !io.out.req.fire().asBool)
  }
  
  Debug(debug) {
    when(state===s_mem && io.out.req.fire().asBool && vaddr=/=phyNum) {
      //printf(p"${GTimer()}, state:${state}, out.resp.fire:${io.out.resp.fire()}, vaddr:${vaddr}, rdata:${io.out.resp.bits.rdata}\n")
      printf(name + "%d: state:%d, out.req.fire:%d, vaddr:%x, phyNum:%x\n",GTimer(),state,io.out.req.fire(),vaddr,io.out.req.bits.addr)
    }
    assert((state===s_mem && io.out.req.fire().asBool && vaddr===phyNum) || state=/=s_mem || !io.out.req.fire().asBool)
  }

  Debug(debug && name == "dptw" && false) {
    when(GTimer()>=1300.U) {
      printf(name + "%d: PTW state:%d lev:%d vaddr:%x phy:%x flush:%d rdata:%x inRespValid:%d inRespReady:%d outReqValid:%d outReqReady:%d outRespValid:%d outRespReady:%d ",GTimer(),state,level,vaddr,phyNum,needFlush,io.out.resp.bits.rdata,io.in.resp.valid,io.in.resp.ready,io.out.req.valid,io.out.req.ready,io.out.resp.valid,io.out.resp.ready)
      printf("alreadyOutFire:%d\n", alreadyOutFire)
    }
  }  
}
*/