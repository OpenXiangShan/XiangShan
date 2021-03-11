package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import xiangshan._
import utils._

trait WaitTableParameters {
  val WaitTableSize = 1024
  val WaitTableAddrWidth = log2Up(WaitTableSize)
  val ResetTimeMax2Pow = 20 //1078576
  val ResetTimeMin2Pow = 10 //1024
}

// 21264-like wait table
class WaitTable extends XSModule with WaitTableParameters {
  val io = IO(new Bundle {
    val raddr = Vec(DecodeWidth, Input(UInt(WaitTableAddrWidth.W))) // decode pc(VaddrBits-1, 1)
    val rdata = Vec(DecodeWidth, Output(Bool())) // loadWaitBit
    val update = Vec(StorePipelineWidth, Input(new WaitTableUpdateReq)) // RegNext should be added outside
    val csrCtrl = Input(new CustomCSRCtrlIO)
  })

  val data = RegInit(VecInit(Seq.fill(WaitTableSize)(0.U(2.W))))
  val resetCounter = RegInit(0.U(ResetTimeMax2Pow.W))
  resetCounter := resetCounter + 1.U

  // read ports
  for (i <- 0 until DecodeWidth) {
    io.rdata(i) := (data(io.raddr(i))(1) || io.csrCtrl.no_spec_load) && !io.csrCtrl.lvpred_disable
  }

  // write ports (with priority)
  (0 until StorePipelineWidth).map(i => {
    when(io.update(i).valid){
      data(io.update(i).waddr) := Cat(data(io.update(i).waddr)(0), true.B)
    }
  })


  // reset period: ResetTimeMax2Pow
  when(resetCounter(ResetTimeMax2Pow-1, ResetTimeMin2Pow)(RegNext(io.csrCtrl.waittable_timeout))) {
    for (j <- 0 until WaitTableSize) {
      data(j) := 0.U
    }
    resetCounter:= 0.U
  }

  // debug
  for (i <- 0 until StorePipelineWidth) {
    when (io.update(i).valid) {
      XSDebug("%d: waittable update: pc %x data: %x\n", GTimer(), io.update(i).waddr, io.update(i).wdata)
    }
  }

  XSPerf("wait_table_bit_set", PopCount(data.map(d => d(1))))
}
