package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.ALUOpType
import utils._

class RAS extends BasePredictor
    with RASParameter
{
    class RASResp extends Resp
    {
        val target =ValiIO(UInt(VAddrBits.W))
    }

    class RASBranchInfo extends Meta
    {
        val rasSp = UInt(log2Up(RasSize).W)
        val rasTopCtr = UInt(8.W)
    }

    class RASIO extends DefaultBasePredictorIO 
    {
        val is_ret = Input(Bool())
        val callIdx = Flipped(ValidIO(UInt(log2Ceil(PredictWidth).W)))
        val isRVC = Input(Bool())
        val redirect =  Flipped(ValidIO(new Redirect)))
        val out = ValidIO(new RASResp)
        val branchInfo = ValidIO(new RASBranchInfo)
    }

    def rasEntry() = new Bundle {
    val retAddr = UInt(VAddrBits.W)
    val ctr = UInt(8.W) // layer of nested call functions
    }
    val ras = RegInit(VecInit(RasSize, 0.U.asTypeOf(rasEntry())))
    val sp = RegInit(0.U(log2Up(RasSize).W))
    val ras_top_entry = ras(sp)
    val ras_top_addr = ras_top_entry.retAddr

    val is_empty = sp === 0.U
    val is_full = sp === (RasSize - 1).U
    // save ras checkpoint info
    io.out.bits.rasSp := sp
    io.out.bits.rasTopCtr := rasTop.ctr

    // update RAS
    // speculative update RAS
    when (!is_full && io.callIdx.valid) {
        //push
        io.out.bits.target := ras_top_addr
        val rasWrite = WireInit(0.U.asTypeOf(rasEntry()))
        val allocNewEntry = rasWrite.retAddr =/= rasTopAddr
        rasWrite.ctr := Mux(allocNewEntry, 1.U, rasTop.ctr + 1.U)
        rasWrite.retAddr := io.pc.bits + (io.callIdx.bits << 2.U) + 4.U
        ras(sp) := in.target
        when(allocNewEntry){sp := sp + 1.U }
    }.elsewhen ((!is_empty && io.retIdx.valid) {
        //pop
        when (ras_top_entry.ctr === 1.U) {
            sp := Mux(sp.value === 0.U, 0.U, sp - 1.U)
        }.otherwise {
            ras_top_entry.ctr := ras_top_entry.ctr - 1.U
        }
    }
    // TODO: back-up stack for ras
    // use checkpoint to recover RAS
    val recoverSp = io.redirect.rasSp
    val recoverCtr = io.redirect.rasTopCtr
    when (io.redirect.valid && io.redirect.isMisPred) {
        sp := recoverSp
        ras(recoverSp) := Cat(recoverCtr, ras(recoverSp).retAddr).asTypeOf(rasEntry())
    }

}