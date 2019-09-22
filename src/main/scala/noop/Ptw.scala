package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import bus.simplebus._
import bus.axi4._
import utils._

class PTWIn extends Bundle {
  val va        = UInt(32.W)
  val op        = UInt(32.W)
  val satp      = UInt(32.W)
}

class PTWOut extends Bundle {
  val pa        = UInt(32.W)
}

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

class PtwSv32 extends Module with pteSv32Const{
  val io = IO(new Bundle {
    val satp = Input(UInt(32.W))
    val flush = Input(Bool())
    val in   = Flipped(new SimpleBusUC(dataBits=32, userBits=32))
    val out  = new SimpleBusUC(dataBits=32, userBits=32)
  })

  val s_ready :: s_walk :: s_mem :: s_error :: Nil = Enum(4)
  val state = RegInit(s_ready)
  val phyNum = Reg(UInt(32.W))
  val alreadyOutFire = RegEnable(true.B, io.out.req.fire())
  val _isWork = RegEnable(io.satp(31).asBool, state===s_ready && io.in.req.fire()) //hold the satp(31) to aviod sudden change.
  val isWork = Mux(state===s_ready, io.satp(31).asBool, _isWork) //isWork control the 
  val needFlush = RegInit(false.B) // needFlush: set when encounter a io.flush; work when after an access memory series ends; reset when return to s_ready. the io.in.resp.valid is true at mem, so we can jump to s_ready directly or low down the valid.
  //when (io.flush && (state =/= s_ready)) { needFlush := true.B }

  val updateStore = state===s_ready && io.in.req.fire() && io.satp(31).asBool
  val vaddr = RegEnable(io.in.req.bits.addr, updateStore) // maybe just need the fire() signal
  val inReqBitsCmd  = RegEnable(io.in.req.bits.cmd, updateStore)
  val inReqBitsWmask = RegEnable(io.in.req.bits.wmask, updateStore)
  val inReqBitsWdata = RegEnable(io.in.req.bits.wdata, updateStore)
  val inReqBitsUser = RegEnable(io.in.req.bits.user, updateStore)
  val inReqBitsSize = RegEnable(io.in.req.bits.size, updateStore)
  //store end

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
  io.out.req.valid      := io.in.req.valid && (state===s_walk && !alreadyOutFire|| state===s_mem && !alreadyOutFire || !io.satp(31).asBool)//need add state machine
  io.in.req.ready       := io.out.req.ready && (state===s_ready || !io.satp(31).asBool)
  //connect end

  val level = RegInit(2.U)

  //state machine: does instr and data need two ptw?? maybe one is enough, so how to handle two input
  //s_ready : free state
  //s_walk  : the work stage(go and get the pte). In Sv39..., have the third state or else
  //s_mem   : already get the paddr, then access the mem to get the data, maybe just 
  //s_error : error state, raise an exception, unknown how to do

  val last_rdata = RegInit(0.U) //no use, debug
  
  switch (state) {
    is (s_ready) {
      when(io.in.req.fire() && io.satp(31).asBool && !io.flush) {
        state := s_walk
        phyNum := Cat(io.satp(19,0), Cat(io.in.req.bits.addr(31,22), 0.U(2.W)))
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
          last_rdata := io.out.resp.bits.rdata //debug
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

  Debug(debug) {
    when(vaddr === "h80100000".U) {
      isCount := true.B
    }
    when( GTimer() >= 111500.U && isCount || (vaddr>="h80100170".U && vaddr<="h80100250".U )) {
      printf("%d: PTW state:%d lev:%d vaddr:%x phy:%x flush:%d rdata:%x inRespValid:%d inRespReady:%d outReqValid:%d outReqReady:%d outRespValid:%d outRespReady:%d\n",GTimer(),state,level,vaddr,phyNum,needFlush,io.out.resp.bits.rdata,io.in.resp.valid,io.in.resp.ready,io.out.req.valid,io.out.req.ready,io.out.resp.valid,io.out.resp.ready)
      //when(isCount===false.B) {isCount := true.B}
    }
    when(isCount && !(state===s_mem && io.out.req.fire().asBool && vaddr===phyNum)) {
      //printf(p"${GTimer()}, state:${state}, out.resp.fire:${io.out.resp.fire()}, vaddr:${vaddr}, rdata:${io.out.resp.bits.rdata}\n")
      printf("%d: state:%d, out.resp.fire:%d, vaddr:%x, rdata:%x\n",GTimer(),state,io.out.resp.fire(),vaddr,io.out.resp.bits.rdata)
    }
    assert((state===s_mem && io.out.req.fire().asBool && vaddr===phyNum) || state=/=s_mem || !io.out.req.fire().asBool)
  }
}
