package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import xiangshan._
import utils._

trait WaitTableParameters {
  val isSync = false // fixed
  val WaitTableSize = 1024
  val WaitTableAddrWidth = log2Up(WaitTableSize)
  val ResetTime2Pow = 17 //131072
}

// 21264-like wait table
class WaitTable extends XSModule with WaitTableParameters {
  val io = IO(new Bundle {
    val raddr = Vec(DecodeWidth, Input(UInt(WaitTableAddrWidth.W))) // decode pc(VaddrBits-1, 1)
    val rdata = Vec(DecodeWidth, Output(Bool())) // loadWaitBit
    val update = Vec(StorePipelineWidth, Input(new WaitTableUpdateReq)) // RegNext should be added outside
  })

  val data = Reg(Vec(WaitTableSize, Bool())) // init val false.B
  val resetCounter = RegInit(0.U(ResetTime2Pow.W))
  resetCounter := resetCounter + 1.U

  // read ports
  val raddr = if (isSync) (RegNext(io.raddr)) else io.raddr
  for (i <- 0 until DecodeWidth) {
    io.rdata(i) := data(raddr(i))
  }

  // write ports (with priority)
  (0 until StorePipelineWidth).map(i => {
    when(io.update(i).valid){
      data(io.update(i).waddr) := io.update(i).wdata
    }
  })


  // reset period: ResetTime2Pow
  when(resetCounter === 0.U) {
    for (j <- 0 until WaitTableSize) {
      data(j) := false.B
    }
  }

  // debug
  for (i <- 0 until StorePipelineWidth) {
    when (io.update(i).valid) {
      printf("%d: waittable update: pc %x data: %x\n", GTimer(), io.update(i).waddr, io.update(i).wdata)
    }
  }
}

