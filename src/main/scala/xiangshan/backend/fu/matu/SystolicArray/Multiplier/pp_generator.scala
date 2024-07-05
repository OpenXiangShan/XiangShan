package matu.SystolicArray.Multiplier

import chisel3._
import chisel3.util.Decoupled
import chisel3.util.Cat

//todo DecoupledIO
class pgInputDataBundle(val w:Int) extends Bundle{
    val multiplicand = Input(SInt(w.W))
}

class pgInputCtrlBundle(val w:Int) extends Bundle{
    val X2   = Input(UInt(w.W))
    val inv  = Input(UInt(w.W))
    val Set0 = Input(UInt(w.W))
}

class pgOutputDataBundle(val IN_WIDTH:Int, val C_WIDTH:Int) extends Bundle{
    val pp_out  = Output(Vec(IN_WIDTH/2, SInt(C_WIDTH.W)))
    val sig_out = Output(SInt(C_WIDTH.W))
}

class ppGenerator(val IN_WIDTH: Int, val C_WIDTH: Int) extends Module {
    val inputData  = IO(new pgInputDataBundle(IN_WIDTH))
    val inputCtrl  = IO(new pgInputCtrlBundle(IN_WIDTH/2))
    val outputData = IO(new pgOutputDataBundle(IN_WIDTH, C_WIDTH))

    val E          = Wire(Vec(IN_WIDTH/2, Bool()))
    val E_inv      = Wire(Vec(IN_WIDTH/2, Bool()))

    val pp_X2      = Wire(Vec(IN_WIDTH/2, SInt((IN_WIDTH+1).W)))
    val pp_set     = Wire(Vec(IN_WIDTH/2, SInt((IN_WIDTH+1).W)))
    val pp_inv     = Wire(Vec(IN_WIDTH/2, SInt((IN_WIDTH+1).W)))
    val pp_temp    = Wire(Vec(IN_WIDTH/2, SInt(C_WIDTH.W)))

    val pp         = RegInit(VecInit(Seq.fill(IN_WIDTH/2)(0.S(C_WIDTH.W))))
    val sign_com   = RegInit(0.S(C_WIDTH.W))

    for (i <- 0 until IN_WIDTH/2){
        pp_X2(i)  := Mux(inputCtrl.X2(i).asBool, Cat(inputData.multiplicand, 0.S(1.W)),
                        Cat(inputData.multiplicand(IN_WIDTH-1), inputData.multiplicand)).asSInt
        pp_set(i) := Mux(inputCtrl.Set0(i).asBool, 0.S((IN_WIDTH+1).W), pp_X2(i)).asSInt
        pp_inv(i) := Mux(inputCtrl.inv(i).asBool, ~(pp_set(i)), pp_set(i)).asSInt             //add 1 in next pipeline
        E_inv(i)  := Mux(inputCtrl.Set0(i).asBool, inputCtrl.inv(i).asBool, (inputCtrl.inv(i)^inputData.multiplicand(IN_WIDTH-1)).asBool)
        E(i)      := ~E_inv(i)
        
        if (i == 0){
            if(C_WIDTH-IN_WIDTH-4 == 0){
                pp_temp(i) := Cat(E(0), E_inv(0).asUInt, E_inv(0).asUInt, pp_inv(0)).asSInt
            } else {
                pp_temp(i) := Cat(0.U((C_WIDTH-IN_WIDTH-4).W), E(0), E_inv(0).asUInt, E_inv(0).asUInt, pp_inv(0)).asSInt
            }
        }
        else {
            pp_temp(i) := Cat(0.U((C_WIDTH-IN_WIDTH-3).W), 1.U(1.W), E(i).asUInt, pp_inv(i)).asSInt
        }
    }

    for (i <- 0 until IN_WIDTH/2){
        if(i == 0){
            pp(0) := pp_temp(0)
        } else if (i == 1) {
            pp(1) := Cat(pp_temp(i)(C_WIDTH-2*i-1,0), 0.U(1.W), inputCtrl.inv(i-1)).asSInt
        } else {
            pp(i) := Cat(pp_temp(i)(C_WIDTH-2*i-1,0), 0.U(1.W), inputCtrl.inv(i-1), 0.U((2*(i-1)).W)).asSInt
        }
    }
    sign_com := Cat(0.U((C_WIDTH-7).W), inputCtrl.inv(IN_WIDTH/2-1), 0.U((2*(IN_WIDTH/2-1)).W)).asSInt

    outputData.pp_out   := pp
    outputData.sig_out  := sign_com
}