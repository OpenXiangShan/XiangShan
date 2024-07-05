package xiangshan.backend.fu.matu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import matu.DataBuffer._
import matu.SystolicArray._
import utils._
import xiangshan.{MicroOp, _}
import xiangshan.backend.exu.ExuParameters
import xiangshan.backend.fu._

class Matu(implicit p: Parameters) extends FunctionUnit(64, MatuExeUnitCfg) with HasXSParameter{
    val rf2D = Module(new Regfile_2D_wrapper)
    val scoreboard = Module(new Scoreboard)

    val newsdReq = io.in.fire() && io.in.bits.uop.ctrl.fuOpType === LSUOpType.sd
    val uop = io.in.bits.uop
    val uopReg = RegEnable(uop, newsdReq)

    val load_in_data_w = dontTouch(Wire(Vec(exuParameters.LduCnt, UInt(XLEN.W))))
    val load_in_valid_w = dontTouch(Wire(Vec(exuParameters.LduCnt, Bool())))
    val load_in_uop_w = dontTouch(Wire(Vec(exuParameters.LduCnt, new MicroOp)))

    for (i <- 0 until exuParameters.LduCnt) {
        load_in_data_w(i) := io.ldIn.get(i).bits.data
        load_in_valid_w(i) := io.ldIn.get(i).valid
        load_in_uop_w(i) := io.ldIn.get(i).bits.uop
    }

    for (i <- 0 until exuParameters.LduCnt) {
        scoreboard.io.ldIn.data_in(i) := load_in_data_w(i)
        scoreboard.io.ldIn.valid_in(i) := load_in_valid_w(i)
        scoreboard.io.ldIn.uop_in(i) := load_in_uop_w(i)
    }

    io.ldIn.get(0).ready := true.B
    io.ldIn.get(1).ready := true.B


    val commit_info_r = dontTouch(Reg(Vec(CommitWidth, UInt(VAddrBits.W))))
    commit_info_r <> io.commitIn_pc.get

    scoreboard.io.dpUopIn <> io.dpUopIn.get
    scoreboard.io.commitsIO.commits_pc <> io.commitIn_pc.get
    scoreboard.io.commitsIO.commits_valid <> io.commitIn_valid.get
    scoreboard.io.commitsIO.commits_robIdx <> io.commitIn_robIdx.get
    scoreboard.io.wbIn.wen(0) := rf2D.io.wbInfoOut.ld_wen
    scoreboard.io.wbIn.wen(1) := rf2D.io.wbInfoOut.fu_wen
    scoreboard.io.wbIn.waddr(0) := rf2D.io.wbInfoOut.ld_waddr
    scoreboard.io.wbIn.waddr(1) := rf2D.io.wbInfoOut.fu_waddr
    scoreboard.io.wbIn.woffset(0) := rf2D.io.wbInfoOut.ld_woffset
    scoreboard.io.wbIn.woffset(1) := 3.U
    scoreboard.io.flushIn.redirect <> io.redirectIn

    rf2D.io.ldIn.wdata_in := scoreboard.io.ldOut.data_out
    rf2D.io.ldIn.waddr_in := scoreboard.io.ldOut.addr_out
    rf2D.io.ldIn.woffset_in := scoreboard.io.ldOut.offset_out
    rf2D.io.ldIn.valid_in := scoreboard.io.ldOut.wen

    rf2D.io.stIO.roffset_in := scoreboard.io.stIO.roffset_out
    rf2D.io.stIO.raddr_in := scoreboard.io.stIO.raddr_out

    io.mpuOut_data.get := rf2D.io.stIO.rdata_out
    io.mpuOut_valid.get := scoreboard.io.stIO.store_flag
    io.mpuOut_uop.get <> uopReg
    io.mpuOut_pc.get := scoreboard.io.stIO.pc_out
    io.mpuOut_robIdx.get := scoreboard.io.stIO.robIdx_out
    io.mpuOut_canAccept.get := scoreboard.io.canAccept

    val ex_valid_w = dontTouch(Wire(Bool()))
    val ex_OpType_w = dontTouch(Wire(FuOpType()))
    val rs1_w = dontTouch(Wire(UInt(3.W)))
    val rs2_w = dontTouch(Wire(UInt(3.W)))
    val rd_r = dontTouch(Reg(UInt(3.W)))

    ex_valid_w := scoreboard.io.fuIO.valid_out
    ex_OpType_w := scoreboard.io.fuIO.OpType_out
    rs1_w := Mux(ex_valid_w, scoreboard.io.fuIO.rs1_out, 0.U)
    rs2_w := Mux(ex_valid_w, scoreboard.io.fuIO.rs2_out, 0.U)
    when (ex_valid_w) {
        rd_r := scoreboard.io.fuIO.rd_out
    }
    //rd_w := Mux(ex_valid_w, scoreboard.io.fuIO.rd_out, 0.U)

    rf2D.io.fuIO.raddr_in(0) := rs1_w
    rf2D.io.fuIO.raddr_in(1) := rs2_w
    rf2D.io.fuIO.waddr_in := rd_r

    val MPU = Module (new top_R(1, 8, 32, 2, 8, 2, 2))
    //val MADD = Module (new Mtest())
    when (ex_OpType_w === MATUOpType.mmul) {
    MPU.io.tpuIO.in.bits.in_a(0)(0) := rf2D.io.fuIO.rdata_out(0)(0)(7, 0).asSInt
    MPU.io.tpuIO.in.bits.in_a(0)(1) := rf2D.io.fuIO.rdata_out(0)(0)(15, 8).asSInt
    MPU.io.tpuIO.in.bits.in_a(0)(2) := rf2D.io.fuIO.rdata_out(0)(0)(23, 16).asSInt
    MPU.io.tpuIO.in.bits.in_a(0)(3) := rf2D.io.fuIO.rdata_out(0)(0)(31, 24).asSInt
    MPU.io.tpuIO.in.bits.in_a(0)(4) := rf2D.io.fuIO.rdata_out(0)(0)(39, 32).asSInt
    MPU.io.tpuIO.in.bits.in_a(0)(5) := rf2D.io.fuIO.rdata_out(0)(0)(47, 40).asSInt
    MPU.io.tpuIO.in.bits.in_a(0)(6) := rf2D.io.fuIO.rdata_out(0)(0)(55, 48).asSInt
    MPU.io.tpuIO.in.bits.in_a(0)(7) := rf2D.io.fuIO.rdata_out(0)(0)(63, 56).asSInt

    MPU.io.tpuIO.in.bits.in_a(1)(0) := rf2D.io.fuIO.rdata_out(0)(1)(7, 0).asSInt
    MPU.io.tpuIO.in.bits.in_a(1)(1) := rf2D.io.fuIO.rdata_out(0)(1)(15, 8).asSInt
    MPU.io.tpuIO.in.bits.in_a(1)(2) := rf2D.io.fuIO.rdata_out(0)(1)(23, 16).asSInt
    MPU.io.tpuIO.in.bits.in_a(1)(3) := rf2D.io.fuIO.rdata_out(0)(1)(31, 24).asSInt
    MPU.io.tpuIO.in.bits.in_a(1)(4) := rf2D.io.fuIO.rdata_out(0)(1)(39, 32).asSInt
    MPU.io.tpuIO.in.bits.in_a(1)(5) := rf2D.io.fuIO.rdata_out(0)(1)(47, 40).asSInt
    MPU.io.tpuIO.in.bits.in_a(1)(6) := rf2D.io.fuIO.rdata_out(0)(1)(55, 48).asSInt
    MPU.io.tpuIO.in.bits.in_a(1)(7) := rf2D.io.fuIO.rdata_out(0)(1)(63, 56).asSInt

    MPU.io.tpuIO.in.bits.in_b(0)(0) := rf2D.io.fuIO.rdata_out(1)(0)(7, 0).asSInt
    MPU.io.tpuIO.in.bits.in_b(0)(1) := rf2D.io.fuIO.rdata_out(1)(0)(15, 8).asSInt
    MPU.io.tpuIO.in.bits.in_b(0)(2) := rf2D.io.fuIO.rdata_out(1)(0)(23, 16).asSInt
    MPU.io.tpuIO.in.bits.in_b(0)(3) := rf2D.io.fuIO.rdata_out(1)(0)(31, 24).asSInt
    MPU.io.tpuIO.in.bits.in_b(0)(4) := rf2D.io.fuIO.rdata_out(1)(0)(39, 32).asSInt
    MPU.io.tpuIO.in.bits.in_b(0)(5) := rf2D.io.fuIO.rdata_out(1)(0)(47, 40).asSInt
    MPU.io.tpuIO.in.bits.in_b(0)(6) := rf2D.io.fuIO.rdata_out(1)(0)(55, 48).asSInt
    MPU.io.tpuIO.in.bits.in_b(0)(7) := rf2D.io.fuIO.rdata_out(1)(0)(63, 56).asSInt

    MPU.io.tpuIO.in.bits.in_b(1)(0) := rf2D.io.fuIO.rdata_out(1)(1)(7, 0).asSInt
    MPU.io.tpuIO.in.bits.in_b(1)(1) := rf2D.io.fuIO.rdata_out(1)(1)(15, 8).asSInt
    MPU.io.tpuIO.in.bits.in_b(1)(2) := rf2D.io.fuIO.rdata_out(1)(1)(23, 16).asSInt
    MPU.io.tpuIO.in.bits.in_b(1)(3) := rf2D.io.fuIO.rdata_out(1)(1)(31, 24).asSInt
    MPU.io.tpuIO.in.bits.in_b(1)(4) := rf2D.io.fuIO.rdata_out(1)(1)(39, 32).asSInt
    MPU.io.tpuIO.in.bits.in_b(1)(5) := rf2D.io.fuIO.rdata_out(1)(1)(47, 40).asSInt
    MPU.io.tpuIO.in.bits.in_b(1)(6) := rf2D.io.fuIO.rdata_out(1)(1)(55, 48).asSInt
    MPU.io.tpuIO.in.bits.in_b(1)(7) := rf2D.io.fuIO.rdata_out(1)(1)(63, 56).asSInt
    }.elsewhen(ex_OpType_w === MATUOpType.mvmul){
    MPU.io.tpuIO.in.bits.in_a(0)(0) := rf2D.io.fuIO.rdata_out(0)(0)(7, 0).asSInt
    MPU.io.tpuIO.in.bits.in_a(0)(1) := rf2D.io.fuIO.rdata_out(0)(0)(15, 8).asSInt
    MPU.io.tpuIO.in.bits.in_a(0)(2) := rf2D.io.fuIO.rdata_out(0)(0)(23, 16).asSInt
    MPU.io.tpuIO.in.bits.in_a(0)(3) := rf2D.io.fuIO.rdata_out(0)(0)(31, 24).asSInt
    MPU.io.tpuIO.in.bits.in_a(0)(4) := rf2D.io.fuIO.rdata_out(0)(0)(39, 32).asSInt
    MPU.io.tpuIO.in.bits.in_a(0)(5) := rf2D.io.fuIO.rdata_out(0)(0)(47, 40).asSInt
    MPU.io.tpuIO.in.bits.in_a(0)(6) := rf2D.io.fuIO.rdata_out(0)(0)(55, 48).asSInt
    MPU.io.tpuIO.in.bits.in_a(0)(7) := rf2D.io.fuIO.rdata_out(0)(0)(63, 56).asSInt

    MPU.io.tpuIO.in.bits.in_a(1)(0) := rf2D.io.fuIO.rdata_out(0)(1)(7, 0).asSInt
    MPU.io.tpuIO.in.bits.in_a(1)(1) := rf2D.io.fuIO.rdata_out(0)(1)(15, 8).asSInt
    MPU.io.tpuIO.in.bits.in_a(1)(2) := rf2D.io.fuIO.rdata_out(0)(1)(23, 16).asSInt
    MPU.io.tpuIO.in.bits.in_a(1)(3) := rf2D.io.fuIO.rdata_out(0)(1)(31, 24).asSInt
    MPU.io.tpuIO.in.bits.in_a(1)(4) := rf2D.io.fuIO.rdata_out(0)(1)(39, 32).asSInt
    MPU.io.tpuIO.in.bits.in_a(1)(5) := rf2D.io.fuIO.rdata_out(0)(1)(47, 40).asSInt
    MPU.io.tpuIO.in.bits.in_a(1)(6) := rf2D.io.fuIO.rdata_out(0)(1)(55, 48).asSInt
    MPU.io.tpuIO.in.bits.in_a(1)(7) := rf2D.io.fuIO.rdata_out(0)(1)(63, 56).asSInt

    MPU.io.tpuIO.in.bits.in_b(0)(0) := rf2D.io.fuIO.rdata_out(1)(0)(7, 0).asSInt
    MPU.io.tpuIO.in.bits.in_b(0)(1) := rf2D.io.fuIO.rdata_out(1)(0)(15, 8).asSInt
    MPU.io.tpuIO.in.bits.in_b(0)(2) := rf2D.io.fuIO.rdata_out(1)(0)(23, 16).asSInt
    MPU.io.tpuIO.in.bits.in_b(0)(3) := rf2D.io.fuIO.rdata_out(1)(0)(31, 24).asSInt
    MPU.io.tpuIO.in.bits.in_b(0)(4) := rf2D.io.fuIO.rdata_out(1)(0)(39, 32).asSInt
    MPU.io.tpuIO.in.bits.in_b(0)(5) := rf2D.io.fuIO.rdata_out(1)(0)(47, 40).asSInt
    MPU.io.tpuIO.in.bits.in_b(0)(6) := rf2D.io.fuIO.rdata_out(1)(0)(55, 48).asSInt
    MPU.io.tpuIO.in.bits.in_b(0)(7) := rf2D.io.fuIO.rdata_out(1)(0)(63, 56).asSInt
    MPU.io.tpuIO.in.bits.in_b(1)(0) := 0.asSInt
    MPU.io.tpuIO.in.bits.in_b(1)(1) := 0.asSInt
    MPU.io.tpuIO.in.bits.in_b(1)(2) := 0.asSInt
    MPU.io.tpuIO.in.bits.in_b(1)(3) := 0.asSInt
    MPU.io.tpuIO.in.bits.in_b(1)(4) := 0.asSInt
    MPU.io.tpuIO.in.bits.in_b(1)(5) := 0.asSInt
    MPU.io.tpuIO.in.bits.in_b(1)(6) := 0.asSInt
    MPU.io.tpuIO.in.bits.in_b(1)(7) := 0.asSInt
    }.otherwise{

    MPU.io.tpuIO.in.bits.in_a(0)(0) := rf2D.io.fuIO.rdata_out(0)(0)(7, 0).asSInt
    MPU.io.tpuIO.in.bits.in_a(0)(1) := rf2D.io.fuIO.rdata_out(0)(0)(15, 8).asSInt
    MPU.io.tpuIO.in.bits.in_a(0)(2) := rf2D.io.fuIO.rdata_out(0)(0)(23, 16).asSInt
    MPU.io.tpuIO.in.bits.in_a(0)(3) := rf2D.io.fuIO.rdata_out(0)(0)(31, 24).asSInt
    MPU.io.tpuIO.in.bits.in_a(0)(4) := rf2D.io.fuIO.rdata_out(0)(0)(39, 32).asSInt
    MPU.io.tpuIO.in.bits.in_a(0)(5) := rf2D.io.fuIO.rdata_out(0)(0)(47, 40).asSInt
    MPU.io.tpuIO.in.bits.in_a(0)(6) := rf2D.io.fuIO.rdata_out(0)(0)(55, 48).asSInt
    MPU.io.tpuIO.in.bits.in_a(0)(7) := rf2D.io.fuIO.rdata_out(0)(0)(63, 56).asSInt

    MPU.io.tpuIO.in.bits.in_a(1)(0) := rf2D.io.fuIO.rdata_out(0)(1)(7, 0).asSInt
    MPU.io.tpuIO.in.bits.in_a(1)(1) := rf2D.io.fuIO.rdata_out(0)(1)(15, 8).asSInt
    MPU.io.tpuIO.in.bits.in_a(1)(2) := rf2D.io.fuIO.rdata_out(0)(1)(23, 16).asSInt
    MPU.io.tpuIO.in.bits.in_a(1)(3) := rf2D.io.fuIO.rdata_out(0)(1)(31, 24).asSInt
    MPU.io.tpuIO.in.bits.in_a(1)(4) := rf2D.io.fuIO.rdata_out(0)(1)(39, 32).asSInt
    MPU.io.tpuIO.in.bits.in_a(1)(5) := rf2D.io.fuIO.rdata_out(0)(1)(47, 40).asSInt
    MPU.io.tpuIO.in.bits.in_a(1)(6) := rf2D.io.fuIO.rdata_out(0)(1)(55, 48).asSInt
    MPU.io.tpuIO.in.bits.in_a(1)(7) := rf2D.io.fuIO.rdata_out(0)(1)(63, 56).asSInt

    MPU.io.tpuIO.in.bits.in_b(0)(0) := rf2D.io.fuIO.rdata_out(1)(0)(7, 0).asSInt
    MPU.io.tpuIO.in.bits.in_b(0)(1) := rf2D.io.fuIO.rdata_out(1)(0)(15, 8).asSInt
    MPU.io.tpuIO.in.bits.in_b(0)(2) := rf2D.io.fuIO.rdata_out(1)(0)(23, 16).asSInt
    MPU.io.tpuIO.in.bits.in_b(0)(3) := rf2D.io.fuIO.rdata_out(1)(0)(31, 24).asSInt
    MPU.io.tpuIO.in.bits.in_b(0)(4) := rf2D.io.fuIO.rdata_out(1)(0)(39, 32).asSInt
    MPU.io.tpuIO.in.bits.in_b(0)(5) := rf2D.io.fuIO.rdata_out(1)(0)(47, 40).asSInt
    MPU.io.tpuIO.in.bits.in_b(0)(6) := rf2D.io.fuIO.rdata_out(1)(0)(55, 48).asSInt
    MPU.io.tpuIO.in.bits.in_b(0)(7) := rf2D.io.fuIO.rdata_out(1)(0)(63, 56).asSInt

    MPU.io.tpuIO.in.bits.in_b(1)(0) := rf2D.io.fuIO.rdata_out(1)(1)(7, 0).asSInt
    MPU.io.tpuIO.in.bits.in_b(1)(1) := rf2D.io.fuIO.rdata_out(1)(1)(15, 8).asSInt
    MPU.io.tpuIO.in.bits.in_b(1)(2) := rf2D.io.fuIO.rdata_out(1)(1)(23, 16).asSInt
    MPU.io.tpuIO.in.bits.in_b(1)(3) := rf2D.io.fuIO.rdata_out(1)(1)(31, 24).asSInt
    MPU.io.tpuIO.in.bits.in_b(1)(4) := rf2D.io.fuIO.rdata_out(1)(1)(39, 32).asSInt
    MPU.io.tpuIO.in.bits.in_b(1)(5) := rf2D.io.fuIO.rdata_out(1)(1)(47, 40).asSInt
    MPU.io.tpuIO.in.bits.in_b(1)(6) := rf2D.io.fuIO.rdata_out(1)(1)(55, 48).asSInt
    MPU.io.tpuIO.in.bits.in_b(1)(7) := rf2D.io.fuIO.rdata_out(1)(1)(63, 56).asSInt
    }
    MPU.io.tpuIO.in.bits.in_c(0) := 0.S
    MPU.io.tpuIO.in.bits.in_c(1) := 0.S
    MPU.io.tpuIO.in.valid := scoreboard.io.fuIO.valid_out
    scoreboard.io.fuIO.ready_in := MPU.io.tpuIO.in.ready

    rf2D.io.fuIO.valid_in := MPU.io.tpuIO.out.valid
    for (i <- 0 until 2) {
        rf2D.io.fuIO.wdata_in(i) <> Cat(MPU.io.tpuIO.out.bits.out_c(i)(1), MPU.io.tpuIO.out.bits.out_c(i)(0))
    }
    MPU.io.tpuIO.out.ready := true.B


/*  val MTU = Module (new Mtest)
    MTU.io.rs1_data <> rf2D.io.fuIO.rdata_out(0)
    MTU.io.rs2_data <> rf2D.io.fuIO.rdata_out(1)
    MTU.io.valid_in := scoreboard.io.fuIO.valid_out
    rf2D.io.fuIO.valid_in := MTU.io.valid_out
    rf2D.io.fuIO.wdata_in <> MTU.io.rd_data
    scoreboard.io.fuIO.ready_in := MTU.io.ready_out */

    io.in.ready := io.out.ready
    io.out.valid := io.in.valid
    io.out.bits.uop <> io.in.bits.uop
    io.out.bits.data := io.in.bits.src(0)

}

class miniTPUInput_R(val IN_WIDTH: Int, val C_WIDTH: Int, val INA_ROWS: Int, val INA_COLS: Int, val SA_ROWS: Int, val SA_COLS: Int) extends Bundle {
    val in_a = Input(Vec(INA_ROWS, Vec(INA_COLS, SInt(IN_WIDTH.W))))
    val in_b = Input(Vec(INA_ROWS, Vec(INA_COLS, SInt(IN_WIDTH.W))))
    val in_c = Input(Vec(SA_COLS, SInt(C_WIDTH.W)))
}

class miniTPUOutput_R(val IN_WIDTH: Int, val C_WIDTH: Int, val SA_ROWS: Int, val SA_COLS: Int) extends Bundle {
    val out_c = Output(Vec(SA_COLS, Vec(SA_ROWS, SInt(C_WIDTH.W))))
}

class miniTPUIO_R (val IN_WIDTH: Int, val C_WIDTH: Int, val INA_ROWS: Int, val INA_COLS: Int, val SA_ROWS: Int, val SA_COLS: Int) extends Bundle {
    val in = Flipped(DecoupledIO(new miniTPUInput_R(IN_WIDTH, C_WIDTH, INA_ROWS, INA_COLS, SA_ROWS, SA_COLS)))
    val out = DecoupledIO(new miniTPUOutput_R(IN_WIDTH, C_WIDTH, SA_ROWS, SA_COLS))
}


/*
   type-0 : pipeline systolic array
   type-1 : algorithm accelerator
   ...
*/
class top_R (val TYPE: Int, val IN_WIDTH: Int, val C_WIDTH: Int, val INA_ROWS: Int, val INA_COLS:Int, val SA_ROWS: Int, val SA_COLS: Int) extends Module {
    val io = IO(new Bundle {
        val tpuIO = new miniTPUIO_R(IN_WIDTH, C_WIDTH, INA_ROWS, INA_COLS, SA_ROWS, SA_COLS)
    })

    val sa = Module(new SystolicArray(IN_WIDTH,C_WIDTH,SA_ROWS,SA_COLS))
    val controller = Module(new Controller(INA_ROWS, INA_COLS, SA_ROWS, SA_COLS))
    val inBuffer_h   = Module(new SAInputBuffer(IN_WIDTH, INA_ROWS, INA_COLS))  // horizontal and vertical data buffer
    val inBuffer_v  = Module(new SAInputBuffer(IN_WIDTH, INA_ROWS, INA_COLS)) // TODO: add control logic to select data( B or D)
    val outBuffer  = Module(new SAOutputBuffer(TYPE, C_WIDTH, SA_COLS, SA_ROWS))

    inBuffer_h.io.data_in.valid := io.tpuIO.in.valid
    inBuffer_h.io.data_in.bits <> io.tpuIO.in.bits.in_a
    inBuffer_h.io.ctrl_ib_data_out := controller.io.ctrl_ib_data_out

    inBuffer_v.io.data_in.valid := io.tpuIO.in.valid
    inBuffer_v.io.data_in.bits <> io.tpuIO.in.bits.in_b
    inBuffer_v.io.ctrl_ib_data_out := controller.io.ctrl_ib_data_out
    io.tpuIO.in.ready := inBuffer_h.io.data_in.ready && inBuffer_v.io.data_in.ready && outBuffer.io.ob_empty && controller.io.ctrl_sa_isIdle

    io.tpuIO.out.valid := outBuffer.io.data_out.valid
    io.tpuIO.out.bits.out_c <> outBuffer.io.data_out.bits
    outBuffer.io.data_out.ready := io.tpuIO.out.ready
    outBuffer.io.ctrl_ob_data_in := controller.io.ctrl_ob_data_in

    sa.io.in_a := inBuffer_h.io.data_out
    sa.io.in_b := inBuffer_v.io.data_out
    sa.io.in_c := io.tpuIO.in.bits.in_c  // TODO: preload in_c as bias
    outBuffer.io.data_in := sa.io.out_c
    sa.io.in_control.foreach(_.ctrl_sa_send_data := controller.io.ctrl_sa_send_data)

    controller.io.ibh_data_in_done := inBuffer_h.io.ib_data_in_done
    controller.io.ibv_data_in_done := inBuffer_v.io.ib_data_in_done
    controller.io.ob_empty := outBuffer.io.ob_empty
}