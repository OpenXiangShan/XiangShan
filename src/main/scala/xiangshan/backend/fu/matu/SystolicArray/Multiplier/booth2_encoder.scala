package matu.SystolicArray.Multiplier

import chisel3._
import chisel3.util.Decoupled
import chisel3.util.Cat

//todo decoupledIO
class beInputBundle(val w:Int) extends Bundle{
    val multiplier = Input(SInt(w.W))
}

class beOutputBundle(val w:Int) extends Bundle{
    val X2   = Output(UInt(w.W))
    val inv  = Output(UInt(w.W))
    val set0 = Output(UInt(w.W))
}

class booth2Encoder(val IN_WIDTH: Int) extends Module{
    val input  = IO(new beInputBundle(IN_WIDTH))
    val output = IO(new beOutputBundle(IN_WIDTH/2))

    val multiplier2 = Wire(UInt((IN_WIDTH+1).W))
    val bits        = Wire(Vec(IN_WIDTH/2, UInt(3.W)))

    val X2Bools     = VecInit(Seq.fill(IN_WIDTH/2)(false.B))
    val invBools    = VecInit(Seq.fill(IN_WIDTH/2)(false.B))
    val set0Bools   = VecInit(Seq.fill(IN_WIDTH/2)(false.B))

    multiplier2 := Cat(input.multiplier, 0.U(1.W))

    for (i <- 0 until IN_WIDTH/2){
        bits(i) := multiplier2(2*i+2, 2*i)
    }

    for (i <- 0 until IN_WIDTH/2){
        X2Bools(i)   := (~(bits(i)(0)^bits(i)(1))).asBool
        invBools(i)  := bits(i)(2).asBool
        set0Bools(i) := bits(i).andR.asUInt | (~bits(i)).andR.asBool

    }
    
    output.X2   := X2Bools.asUInt
    output.inv  := invBools.asUInt
    output.set0 := set0Bools.asUInt

}
