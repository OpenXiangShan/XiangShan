package matu.SystolicArray.Multiplier

import chisel3._
import chisel3.util.Decoupled
import chisel3.util.Cat

class rcaInputBundle(val w:Int) extends Bundle{
    val a_in  = Input(SInt(w.W))
    val b_in  = Input(SInt(w.W))
    val c_in   = Input(SInt(1.W))
}

class rcaOutputBundle(val w:Int) extends Bundle{
    val S    = Output(SInt(w.W))
    val C    = Output(SInt(w.W))
}

class RCA(val C_WIDTH: Int) extends Module{
    val input  = IO(new rcaInputBundle(C_WIDTH))
    val output = IO(new rcaOutputBundle(C_WIDTH))

    val g_comb = Wire(Vec(C_WIDTH  , Bool()))
    val p_comb = Wire(Vec(C_WIDTH  , Bool()))
    val s_comb = Wire(Vec(C_WIDTH  , Bool()))
    val c_comb = Wire(Vec(C_WIDTH+1, Bool()))

    c_comb(0) := input.c_in.asBool

    for (i <- 0 until C_WIDTH){
        g_comb(i)   := (input.a_in(i) & input.b_in(i)).asBool
        p_comb(i)   := (input.a_in(i) ^ input.b_in(i)).asBool
        s_comb(i)   := p_comb(i) ^ c_comb(i)
        c_comb(i+1) := c_comb(i) & p_comb(i) | g_comb(i) 
    }

    output.S := s_comb.asUInt.asSInt
    output.C := c_comb(C_WIDTH).asUInt.asSInt

}