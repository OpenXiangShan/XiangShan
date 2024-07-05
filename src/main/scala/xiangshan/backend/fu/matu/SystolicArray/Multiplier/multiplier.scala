package matu.SystolicArray.Multiplier

import chisel3._
import chisel3.util.Decoupled
import chisel3.util.Cat

//todo DecoupledIO
class MultiplierInputBundle(val w:Int) extends Bundle{
    val multiplicand = Input(SInt(w.W))
    val multiplier   = Input(SInt(w.W))
}

class MultiplierOutputBundle(val w:Int) extends Bundle{
    val product = Output(SInt(w.W))
}

class Multiplier(val IN_WIDTH: Int, val C_WIDTH: Int) extends Module{
    val input  = IO(new MultiplierInputBundle(IN_WIDTH))
    val output = IO(new MultiplierOutputBundle(C_WIDTH))

    val booth2_encoder   = Module(new booth2Encoder(IN_WIDTH))
    val pp_generator     = Module(new ppGenerator(IN_WIDTH, C_WIDTH))
    val wallace_tree     = Module(new wallaceTree(C_WIDTH, IN_WIDTH/2+1))

    booth2_encoder.input.multiplier := input.multiplier

    pp_generator.inputData.multiplicand := input.multiplicand
    pp_generator.inputCtrl.X2           := booth2_encoder.output.X2
    pp_generator.inputCtrl.inv          := booth2_encoder.output.inv
    pp_generator.inputCtrl.Set0         := booth2_encoder.output.set0

    for(i <- 0 until IN_WIDTH/2) {
        wallace_tree.io.data_i(i) := pp_generator.outputData.pp_out(i)
    }
    wallace_tree.io.data_i(IN_WIDTH/2) := pp_generator.outputData.sig_out

    output.product                      := wallace_tree.io.data_o


}