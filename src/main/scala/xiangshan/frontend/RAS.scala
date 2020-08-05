package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.ALUOpType
import utils._

class RAS extends BasePredictor
{
    class RASResp extends Resp
    {
        val target =UInt(VAddrBits.W)
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
        val redirect = Flipped(ValidIO(new Redirect))
        val recover =  Flipped(ValidIO(new BranchUpdateInfo))
        val out = ValidIO(new RASResp)
        val branchInfo = Output(new RASBranchInfo)
    }

    def rasEntry() = new Bundle {
        val retAddr = UInt(VAddrBits.W)
        val ctr = UInt(8.W) // layer of nested call functions
    }
    override val io = IO(new RASIO)

    val ras = RegInit(0.U)asTypeOf(Vec(RasSize,rasEntry))
    val sp = RegInit(0.U(log2Up(RasSize).W))
    val ras_top_entry = ras(sp)
    val ras_top_addr = ras_top_entry.retAddr

    val is_empty = sp === 0.U
    val is_full = sp === (RasSize - 1).U
    // save ras checkpoint info
    io.branchInfo.rasSp := sp
    io.branchInfo.rasTopCtr := ras_top_entry.ctr

    io.out.valid := !is_empty && io.is_ret

    // update RAS
    // speculative update RAS
    io.out.bits.target := 0.U
    when (!is_full && io.callIdx.valid) {
        //push
        //XDebug("d")
        val new_addr:= io.pc.bits + (io.callIdx.bits << 2.U) + 4.U
        val rasWrite = WireInit(0.U.asTypeOf(rasEntry()))
        val allocNewEntry = new_addr =/= ras_top_addr
        rasWrite.ctr := Mux(allocNewEntry, 1.U, ras_top_entry.ctr + 1.U)
        rasWrite.retAddr := Mux(allocNewEntry, new_addr, ras_top_addr)
        ras(sp) := rasWrite
        when(allocNewEntry){sp := sp + 1.U }
    }.elsewhen (!is_empty && io.is_ret) {
        //pop
        io.out.bits.target := ras_top_addr
        when (ras_top_entry.ctr === 1.U) {
            sp := Mux(sp === 0.U, 0.U, sp - 1.U)
        }.otherwise {
            ras_top_entry.ctr := ras_top_entry.ctr - 1.U
        }
    }
    // TODO: back-up stack for ras
    // use checkpoint to recover RAS
    val recoverSp = io.recover.bits.brInfo.rasSp
    val recoverCtr = io.recover.bits.brInfo.rasTopCtr
    when (io.redirect.valid && io.redirect.bits.isMisPred) {
        sp := recoverSp
        ras(recoverSp).ctr := recoverCtr
    }

}