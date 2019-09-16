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
    val in   = Flipped(new SimpleBusUC(dataBits=32, userBits=32))
    val out  = new SimpleBusUC(dataBits=32, userBits=32)
  })

  val s_ready :: s_walk :: s_mem :: s_error :: Nil = Enum(4)
  val state = RegInit(s_ready)
  val phyNum = Reg(UInt(32.W))
  val alreadyOutFire = RegEnable(true.B, io.out.req.fire())

  //connect begin
  //out     <<      ptw     >>     in
  //out.resp.valid   >>     in.resp.valid
  //out.resp.ready   <<     in.resp.ready
  //out.resp.bits    >>     in.resp.bits
  io.in.resp.bits.rdata := io.out.resp.bits.rdata
  io.in.resp.bits.user  := io.out.resp.bits.user  //an question, what does the user mean?
  io.in.resp.bits.cmd   := io.out.resp.bits.cmd   //but maybe unused
  io.in.resp.valid      := io.out.resp.valid && (state===s_mem || !io.satp(31).asBool)
  io.out.resp.ready     := io.in.resp.ready && (state===s_walk || state===s_mem || !io.satp(31).asBool)
  //out      <<     ptw     >>    in
  //out.req.valid    <<     in.req.valid
  //out.req.ready    >>     in.req.ready
  //out.req.bits     <<     in.req.bits
  io.out.req.bits.addr  := Mux(io.satp(31).asBool, phyNum, io.in.req.bits.addr)
  io.out.req.bits.cmd   := Mux(state===s_walk, SimpleBusCmd.read, io.in.req.bits.cmd) //need to be read when ptw workes
  io.out.req.bits.wmask := io.in.req.bits.wmask //unused in ifu, maybe need change in data access
  io.out.req.bits.wdata := io.in.req.bits.wdata //unused in ifu
  io.out.req.bits.user  := io.in.req.bits.user //what is user, the pc??
  io.out.req.bits.size  := io.in.req.bits.size
  //io.out.req.bits.burst := io.in.req.bits.burst //maybe unused
  //io.out.req.bits.wlast := io.in.req.bits.wlast //maybe unused
  io.out.req.valid      := io.in.req.valid && (state===s_walk && !alreadyOutFire|| state===s_mem && !alreadyOutFire || !io.satp(31).asBool)//need add state machine
  io.in.req.ready       := io.out.req.ready && (state===s_ready || !io.satp(31).asBool)
  //connect end

  val level = RegInit(2.U)

  //val phyNum = UInt(32.W) //is reg necessary
  //when(level===2.U) {
  //  phyNum := Cat(io.satp(19,0), Cat(io.in.req.bits.addr(31,22), 0.U(2.W)))
  //} .elsewhen(level===1.U) {
  //  phyNum := Cat(io.out.resp.bits.rdata(29,10), Cat(io.in.req.bits.addr(21,12), 0.U(2.W)))
  //} .otherwise {
  //  phyNum := Cat(io.out.resp.bits.rdata(29,10), io.in.req.bits.addr(11,0))
  //}

  //state machine: does instr and data need two ptw?? maybe one is enough, so how to handle two input
  //s_ready : free state
  //s_walk  : the work stage(go and get the pte). In Sv39..., have the third state or else
  //s_mem   : already get the paddr, then access the mem to get the data, maybe just 
  //s_error : error state, raise an exception, unknown how to do

  val last_rdata = RegInit(0.U) //debug
  
  switch (state) {
    is (s_ready) {
      when(io.in.req.valid && io.satp(31).asBool) {
        state := s_walk
        phyNum := Cat(io.satp(19,0), Cat(io.in.req.bits.addr(31,22), 0.U(2.W)))
        //level := level - 1.U
        alreadyOutFire := false.B
      }
    }
    is (s_walk) {
      when(level =/= 0.U && io.out.resp.valid && last_rdata=/=io.out.resp.bits.rdata/*访存page握手结束*/ /*&& phyNum(3,1)=/= 0.U(3.W)*/) {
        level := level - 1.U
        alreadyOutFire := false.B
        //需要进行权限检查,权限不符，state := s_error
        //Sv32 page table entry: 0:V 1:R 2:W 3:X 4:U 5:G 6:A 7:D
        //val is_error = !getPa.io.out.bits(0).asBool || !getPa.io.out.bits(1).asBool&&io.in.bits.op(1).asBool || ...
        //state := Mux(is_error, s_error, s_walk)
        state := Mux(level===1.U, s_mem, s_walk)
        phyNum := Mux(level===1.U, Cat(io.out.resp.bits.rdata(29,10), io.in.req.bits.addr(11,0)), Cat(io.out.resp.bits.rdata(29,10), Cat(io.in.req.bits.addr(21,12), 0.U(2.W)))) 
        last_rdata := io.out.resp.bits.rdata //debug
      }.elsewhen(level===0.U) {
        //也需要权限检查
        //检查level是否为0，如果为0，证明查询了两层页表，如果为1/2，说明出错/superpage
        state := s_mem
      }
    }
    is (s_error) {
      //raise an exception
      state := s_ready
    }
    
    is (s_mem) {
      when(io.out.resp.valid) {
        state := s_ready
        level := 2.U
        last_rdata := 0.U
        alreadyOutFire := false.B
      }
    }
  }
}
