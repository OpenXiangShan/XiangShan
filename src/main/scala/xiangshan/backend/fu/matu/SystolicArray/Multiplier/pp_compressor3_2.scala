package matu.SystolicArray.Multiplier

import chisel3._
import chisel3.util.Decoupled
import chisel3.util.Cat

class pp_compressor3_2InputBundle(val w:Int) extends Bundle{
    val pp0_in  = Input(SInt(w.W))
    val pp1_in  = Input(SInt(w.W))
    val pp2_in  = Input(SInt(w.W))
}

class pp_compressor3_2OutputBundle(val w:Int) extends Bundle{
    val S    = Output(SInt(w.W))
    val C    = Output(SInt(w.W))
}

class pp_compressor3_2(val C_WIDTH: Int) extends Module{
    val input  = IO(new pp_compressor3_2InputBundle(C_WIDTH))
    val output = IO(new pp_compressor3_2OutputBundle(C_WIDTH))

    val g_comb = Wire(Vec(C_WIDTH, Bool()))
    val p_comb = Wire(Vec(C_WIDTH, Bool()))
    val s_comb = Wire(Vec(C_WIDTH, Bool()))
    val c_comb = Wire(Vec(C_WIDTH, Bool()))
    
    for (i <- 0 until C_WIDTH){
        g_comb(i) := (input.pp0_in(i) & input.pp1_in(i)).asBool
        p_comb(i) := (input.pp0_in(i) ^ input.pp1_in(i)).asBool
        s_comb(i) := p_comb(i) ^ input.pp2_in(i).asBool
        c_comb(i) := input.pp2_in(i).asBool & p_comb(i) | g_comb(i) 
    }

    output.S := s_comb.asUInt.asSInt
    output.C := Cat((c_comb.asUInt)(C_WIDTH-2,0), 0.U(1.W)).asSInt

}