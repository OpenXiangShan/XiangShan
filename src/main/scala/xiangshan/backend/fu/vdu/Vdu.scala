package xiangshan.backend.fu.vdu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.fu._

class vdot_int8_Module(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val rs2_data = Input(UInt(XLEN.W))
    val rs1_data = Input(UInt(XLEN.W))
    val rd_data  = Output(UInt(XLEN.W))
  })

  val num_elem = 8
  val rs2_vec = Wire(Vec(8, SInt(8.W)))
  val rs1_vec = Wire(Vec(8, SInt(8.W)))
  val mul_vec = Wire(Vec(8, SInt(16.W)))

  for(i <- 0 until 8) {
    rs2_vec(i) := io.rs2_data(8*(i+1)-1, 8*i).asSInt
    rs1_vec(i) := io.rs1_data(8*(i+1)-1, 8*i).asSInt
    mul_vec(i) := rs2_vec(i) * rs1_vec(i)
  }

  val res = Wire(SInt(XLEN.W))
  res := mul_vec.reduce(_ +& _)

  io.rd_data := res.asUInt
}

class VduDataModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val src = Vec(2, Input(UInt(XLEN.W)))
    val OpType = Input(FuOpType())
    val result = Output(UInt(XLEN.W))
  })
  val (src1, src2) = (io.src(0), io.src(1))

  val vdot_i8_module = Module(new vdot_int8_Module)

  vdot_i8_module.io.rs2_data := src2
  vdot_i8_module.io.rs1_data := src1

  io.result := vdot_i8_module.io.rd_data
}


class Vdu(implicit p: Parameters) extends FunctionUnit {

  val dataModule = Module(new VduDataModule)

  dataModule.io.src := io.in.bits.src.take(2)
  dataModule.io.OpType := io.in.bits.uop.ctrl.fuOpType

  io.in.ready := io.out.ready
  io.out.valid := io.in.valid
  io.out.bits.uop <> io.in.bits.uop
  io.out.bits.data := dataModule.io.result
}