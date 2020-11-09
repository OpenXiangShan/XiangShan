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
        val specEmpty = Bool()
    }

    class RASBranchInfo extends Meta
    {
        val rasSp = UInt(log2Up(RasSize).W)
        val rasTopCtr = UInt(8.W)
        val rasToqAddr = UInt(VAddrBits.W)
    }

    class RASIO extends DefaultBasePredictorIO 
    {
        val is_ret = Input(Bool())
        val callIdx = Flipped(ValidIO(UInt(log2Ceil(PredictWidth).W)))
        val isRVC = Input(Bool())
        val isLastHalfRVI = Input(Bool())
        val recover =  Flipped(ValidIO(new BranchUpdateInfo))
        val out = ValidIO(new RASResp)
        val branchInfo = Output(new RASBranchInfo)
    }

    def rasEntry() = new Bundle {
        val retAddr = UInt(VAddrBits.W)
        val ctr = UInt(8.W) // layer of nested call functions
    }
    override val io = IO(new RASIO)

    // val ras_0 = Reg(Vec(RasSize, rasEntry()))  //RegInit(0.U)asTypeOf(Vec(RasSize,rasEntry)) cause comb loop
    // val ras_1 = Reg(Vec(RasSize, rasEntry()))
    // val sp_0 = RegInit(0.U(log2Up(RasSize).W))
    // val sp_1 = RegInit(0.U(log2Up(RasSize).W))
    // val choose_bit = RegInit(false.B)   //start with 0
    // val spec_ras = Mux(choose_bit, ras_1, ras_0)
    // val spec_sp = Mux(choose_bit,sp_1,sp_0)
    // val commit_ras = Mux(choose_bit, ras_0, ras_1)
    // val commit_sp = Mux(choose_bit,sp_0,sp_1)

    val spec_ras = Reg(Vec(RasSize, rasEntry()))
    val spec_sp = RegInit(0.U(log2Up(RasSize).W))
    val commit_ras = Reg(Vec(RasSize, rasEntry()))
    val commit_sp = RegInit(0.U(log2Up(RasSize).W))


    val spec_is_empty = spec_sp === 0.U
    val spec_is_full = spec_sp === (RasSize - 1).U

    val spec_ras_top_entry = spec_ras(spec_sp-1.U)
    val spec_ras_top_addr = spec_ras_top_entry.retAddr
    val spec_ras_top_ctr = spec_ras_top_entry.ctr
    //no need to pass the ras branchInfo
    io.branchInfo.rasSp := DontCare
    io.branchInfo.rasTopCtr := DontCare
    io.branchInfo.rasToqAddr := DontCare

    io.out.valid := !spec_is_empty && io.is_ret
    io.out.bits.specEmpty := spec_is_empty

    // update spec RAS
    // speculative update RAS
    val spec_push = !spec_is_full && io.callIdx.valid && io.pc.valid
    val spec_pop = !spec_is_empty && io.is_ret && io.pc.valid
    val spec_new_addr = io.pc.bits + (io.callIdx.bits << 1.U) + Mux(io.isRVC,2.U,Mux(io.isLastHalfRVI, 2.U, 4.U))
    val spec_ras_write = WireInit(0.U.asTypeOf(rasEntry()))
    val sepc_alloc_new = spec_new_addr =/= spec_ras_top_addr
    when (spec_push) {
        //push
        spec_ras_write.ctr := 1.U
        spec_ras_write.retAddr := spec_new_addr
        when(sepc_alloc_new){
            spec_sp := spec_sp + 1.U 
            spec_ras(spec_sp) := spec_ras_write
        }.otherwise{ 
            spec_ras_top_ctr := spec_ras_top_ctr + 1.U
        }
    }
    
    when (spec_pop) {
        //pop
        when (spec_ras_top_ctr === 1.U) {
            spec_sp := Mux(spec_sp === 0.U, 0.U, spec_sp - 1.U)
        }.otherwise {
           spec_ras_top_ctr := spec_ras_top_ctr - 1.U
        }
    }
    io.out.bits.target := spec_ras_top_addr
    // TODO: back-up stack for ras
    // use checkpoint to recover RAS

    val commit_is_empty = commit_sp === 0.U
    val commit_is_full = commit_sp === (RasSize - 1).U
    val commit_ras_top_entry = commit_ras(commit_sp-1.U)
    val commit_ras_top_addr = commit_ras_top_entry.retAddr
    val commit_ras_top_ctr = commit_ras_top_entry.ctr
    //update commit ras
    val commit_push = !commit_is_full && io.recover.valid && io.recover.bits.pd.isCall
    val commit_pop = !commit_is_empty && io.recover.valid && io.recover.bits.pd.isRet
    val commit_new_addr = Mux(io.recover.bits.pd.isRVC,io.recover.bits.pc + 2.U,io.recover.bits.pc + 4.U) 
    val commit_ras_write = WireInit(0.U.asTypeOf(rasEntry()))
    val commit_alloc_new = commit_new_addr =/= commit_ras_top_addr
    when (commit_push) {
        //push
        commit_ras_write.ctr := 1.U
        commit_ras_write.retAddr := commit_new_addr
        when(commit_alloc_new){
            commit_sp := commit_sp + 1.U 
            commit_ras(commit_sp) := commit_ras_write
        }.otherwise{ 
            commit_ras_top_ctr := commit_ras_top_ctr + 1.U
        }
    }
    
    when (commit_pop) {
        //pop
        when (commit_ras_top_ctr === 1.U) {
            commit_sp := Mux(commit_sp === 0.U, 0.U, commit_sp - 1.U)
        }.otherwise {
           commit_ras_top_ctr := commit_ras_top_ctr - 1.U
        }
    }

    val copy_valid = io.recover.valid && io.recover.bits.isMisPred
    val copy_next = RegNext(copy_valid)
    when(copy_next)
    {
        for(i <- 0 until RasSize)
        {
            spec_ras(i) := commit_ras(i)
            spec_sp := commit_sp
        }
    }

    if (BPUDebug && debug) {
        XSDebug("----------------RAS(spec)----------------\n")
        XSDebug("  index       addr           ctr \n")
        for(i <- 0 until RasSize){
            XSDebug("  (%d)   0x%x      %d",i.U,spec_ras(i).retAddr,spec_ras(i).ctr)
            when(i.U === spec_sp){XSDebug(false,true.B,"   <----sp")}
            XSDebug(false,true.B,"\n")
        }
        XSDebug("----------------RAS(commit)----------------\n")
        XSDebug("  index       addr           ctr \n")
        for(i <- 0 until RasSize){
            XSDebug("  (%d)   0x%x      %d",i.U,commit_ras(i).retAddr,commit_ras(i).ctr)
            when(i.U === commit_sp){XSDebug(false,true.B,"   <----sp")}
            XSDebug(false,true.B,"\n")
        }

        XSDebug(spec_push, "(spec_ras)push  inAddr: 0x%x  inCtr: %d |  allocNewEntry:%d |   sp:%d \n",spec_ras_write.retAddr,spec_ras_write.ctr,sepc_alloc_new,spec_sp.asUInt)
        XSDebug(spec_pop, "(spec_ras)pop outValid:%d  outAddr: 0x%x \n",io.out.valid,io.out.bits.target)
        XSDebug(commit_push, "(commit_ras)push  inAddr: 0x%x  inCtr: %d |  allocNewEntry:%d |   sp:%d \n",commit_ras_write.retAddr,commit_ras_write.ctr,sepc_alloc_new,commit_sp.asUInt)
        XSDebug(commit_pop, "(commit_ras)pop outValid:%d  outAddr: 0x%x \n",io.out.valid,io.out.bits.target)
        XSDebug("copyValid:%d copyNext:%d \n",copy_valid,copy_next)
    }


    // val recoverSp = io.recover.bits.brInfo.rasSp
    // val recoverCtr = io.recover.bits.brInfo.rasTopCtr
    // val recoverAddr = io.recover.bits.brInfo.rasToqAddr
    // val recover_top = ras(recoverSp - 1.U)
    // when (recover_valid) {
    //     sp := recoverSp
    //     recover_top.ctr := recoverCtr
    //     recover_top.retAddr := recoverAddr
    //     XSDebug("RAS update: SP:%d , Ctr:%d \n",recoverSp,recoverCtr)
    // }
    // val recover_and_push = recover_valid && push
    // val recover_and_pop = recover_valid && pop
    // val recover_alloc_new = new_addr =/= recoverAddr
    // when(recover_and_push)
    // {
    //     when(recover_alloc_new){
    //         sp := recoverSp + 1.U
    //         ras(recoverSp).retAddr := new_addr
    //         ras(recoverSp).ctr := 1.U
    //         recover_top.retAddr := recoverAddr
    //         recover_top.ctr := recoverCtr
    //     } .otherwise{
    //         sp := recoverSp
    //         recover_top.ctr := recoverCtr + 1.U
    //         recover_top.retAddr := recoverAddr
    //     }
    // } .elsewhen(recover_and_pop)
    // {
    //     io.out.bits.target := recoverAddr
    //     when ( recover_top.ctr === 1.U) {
    //         sp := recoverSp - 1.U
    //     }.otherwise {
    //         sp := recoverSp
    //        recover_top.ctr := recoverCtr - 1.U
    //     }
    // }

}
