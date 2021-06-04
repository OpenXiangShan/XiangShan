/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.frontend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import chisel3.experimental.chiselName
import scala.tools.nsc.doc.base.comment.Bold

class RASEntry()(implicit p: Parameters) extends XSBundle {
    val retAddr = UInt(VAddrBits.W)
    val ctr = UInt(8.W) // layer of nested call functions
}

@chiselName
class RAS(implicit p: Parameters) extends BasePredictor
{
    class RASResp extends Resp
    {
        val target =UInt(VAddrBits.W)
    }

    class RASBranchInfo extends Meta
    {
        val rasSp = UInt(log2Up(RasSize).W)
        val rasTop = new RASEntry
    }

    class RASIO extends DefaultBasePredictorIO 
    {
        val is_ret = Input(Bool())
        val callIdx = Flipped(ValidIO(UInt(log2Ceil(PredictWidth).W)))
        val isRVC = Input(Bool())
        val isLastHalfRVI = Input(Bool())
        val redirect =  Flipped(ValidIO(new Redirect))
        val out = Output(new RASResp)
        val meta = Output(new RASBranchInfo)
    }

    
    def rasEntry() = new RASEntry

    object RASEntry {
        def apply(retAddr: UInt, ctr: UInt): RASEntry = {
            val e = Wire(rasEntry())
            e.retAddr := retAddr
            e.ctr := ctr
            e
        }
    }

    override val io = IO(new RASIO)
    override val debug = true

    @chiselName
    class RASStack(val rasSize: Int) extends XSModule {
        val io = IO(new Bundle {
            val push_valid = Input(Bool())
            val pop_valid = Input(Bool())
            val spec_new_addr = Input(UInt(VAddrBits.W))

            val recover_sp = Input(UInt(log2Up(rasSize).W))
            val recover_top = Input(rasEntry())
            val recover_valid = Input(Bool())
            val recover_push = Input(Bool())
            val recover_pop = Input(Bool())
            val recover_new_addr = Input(UInt(VAddrBits.W))

            val sp = Output(UInt(log2Up(rasSize).W))
            val top = Output(rasEntry())
        })
        val debugIO = IO(new Bundle{
            val push_entry = Output(rasEntry())
            val alloc_new = Output(Bool())
            val sp = Output(UInt(log2Up(rasSize).W))
            val topRegister = Output(rasEntry())
            val out_mem = Output(Vec(RasSize, rasEntry()))
        })

        val stack = Mem(RasSize, new RASEntry)
        val sp = RegInit(0.U(log2Up(rasSize).W))
        val top = RegInit(0.U.asTypeOf(new RASEntry))
        val topPtr = RegInit(0.U(log2Up(rasSize).W))
        
        def ptrInc(ptr: UInt) = Mux(ptr === (rasSize-1).U, 0.U, ptr + 1.U)
        def ptrDec(ptr: UInt) = Mux(ptr === 0.U, (rasSize-1).U, ptr - 1.U)

        val alloc_new = io.spec_new_addr =/= top.retAddr || top.ctr.andR
        val recover_alloc_new = io.recover_new_addr =/= io.recover_top.retAddr || io.recover_top.ctr.andR

        // TODO: fix overflow and underflow bugs
        def update(recover: Bool)(do_push: Bool, do_pop: Bool, do_alloc_new: Bool,
            do_sp: UInt, do_top_ptr: UInt, do_new_addr: UInt,
            do_top: RASEntry) = {
                when (do_push) {
                    when (do_alloc_new) {
                        sp     := ptrInc(do_sp)
                        topPtr := do_sp
                        top.retAddr := do_new_addr
                        top.ctr := 1.U
                        stack.write(do_sp, RASEntry(do_new_addr, 1.U))
                    }.otherwise {
                        when (recover) {
                            sp := do_sp
                            topPtr := do_top_ptr
                            top.retAddr := do_top.retAddr
                        }
                        top.ctr := do_top.ctr + 1.U
                        stack.write(do_top_ptr, RASEntry(do_new_addr, do_top.ctr + 1.U))
                    }
                }.elsewhen (do_pop) {
                    when (do_top.ctr === 1.U) {
                        sp     := ptrDec(do_sp)
                        topPtr := ptrDec(do_top_ptr)
                        top := stack.read(ptrDec(do_top_ptr))
                    }.otherwise {
                        when (recover) {
                            sp := do_sp
                            topPtr := do_top_ptr
                            top.retAddr := do_top.retAddr
                        }
                        top.ctr := do_top.ctr - 1.U
                        stack.write(do_top_ptr, RASEntry(do_top.retAddr, do_top.ctr - 1.U))
                    }
                }.otherwise {
                    when (recover) {
                        sp := do_sp
                        topPtr := do_top_ptr
                        top := do_top
                        stack.write(do_top_ptr, do_top)
                    }
                }
                XSPerfAccumulate("ras_overflow", do_push && do_alloc_new && ptrInc(do_sp) === 0.U)
                XSPerfAccumulate("ras_underflow", do_pop && do_top.ctr === 1.U && ptrDec(do_sp) === (rasSize-1).U)
            }

        update(io.recover_valid)(
            Mux(io.recover_valid, io.recover_push,     io.push_valid),
            Mux(io.recover_valid, io.recover_pop,      io.pop_valid),
            Mux(io.recover_valid, recover_alloc_new,   alloc_new),
            Mux(io.recover_valid, io.recover_sp,       sp),
            Mux(io.recover_valid, io.recover_sp - 1.U, topPtr),
            Mux(io.recover_valid, io.recover_new_addr, io.spec_new_addr),
            Mux(io.recover_valid, io.recover_top,      top))

        io.sp := sp
        io.top := top

        debugIO.push_entry := RASEntry(io.spec_new_addr, Mux(alloc_new, 1.U, top.ctr + 1.U))
        debugIO.alloc_new := alloc_new
        debugIO.sp := sp
        debugIO.topRegister := top
        for (i <- 0 until RasSize) {
            debugIO.out_mem(i) := stack.read(i.U)
        }

    }

    val spec = Module(new RASStack(RasSize))
    val spec_ras = spec.io


    val spec_push = WireInit(false.B)
    val spec_pop = WireInit(false.B)
    val jump_is_first = io.callIdx.bits === 0.U
    val call_is_last_half = io.isLastHalfRVI && jump_is_first
    val spec_new_addr = packetAligned(io.pc.bits) + (io.callIdx.bits << instOffsetBits.U) + Mux( (io.isRVC | call_is_last_half) && HasCExtension.B, 2.U, 4.U)
    spec_ras.push_valid := spec_push
    spec_ras.pop_valid  := spec_pop
    spec_ras.spec_new_addr   := spec_new_addr
    val spec_top_addr = spec_ras.top.retAddr

    spec_push := io.callIdx.valid && io.pc.valid
    spec_pop  := io.is_ret && io.pc.valid

    val redirect = RegNext(io.redirect)
    val copy_valid = redirect.valid
    val recover_cfi = redirect.bits.cfiUpdate

    val retMissPred  = copy_valid && redirect.bits.level === 0.U && recover_cfi.pd.isRet
    val callMissPred = copy_valid && redirect.bits.level === 0.U && recover_cfi.pd.isCall
    // when we mispredict a call, we must redo a push operation
    // similarly, when we mispredict a return, we should redo a pop
    spec_ras.recover_valid := copy_valid
    spec_ras.recover_push := callMissPred
    spec_ras.recover_pop  := retMissPred

    spec_ras.recover_sp  := recover_cfi.rasSp
    spec_ras.recover_top := recover_cfi.rasEntry
    spec_ras.recover_new_addr := recover_cfi.pc + Mux(recover_cfi.pd.isRVC, 2.U, 4.U)

    io.meta.rasSp := spec_ras.sp
    io.meta.rasTop := spec_ras.top

    io.out.target := spec_top_addr
    // TODO: back-up stack for ras
    // use checkpoint to recover RAS

    if (BPUDebug && debug) {
        val spec_debug = spec.debugIO
        XSDebug("----------------RAS----------------\n")
        XSDebug(" TopRegister: 0x%x   %d \n",spec_debug.topRegister.retAddr,spec_debug.topRegister.ctr)
        XSDebug("  index       addr           ctr \n")
        for(i <- 0 until RasSize){
            XSDebug("  (%d)   0x%x      %d",i.U,spec_debug.out_mem(i).retAddr,spec_debug.out_mem(i).ctr)
            when(i.U === spec_debug.sp){XSDebug(false,true.B,"   <----sp")}
            XSDebug(false,true.B,"\n")
        }
        XSDebug(spec_push, "(spec_ras)push  inAddr: 0x%x  inCtr: %d |  allocNewEntry:%d |   sp:%d \n",
            spec_new_addr,spec_debug.push_entry.ctr,spec_debug.alloc_new,spec_debug.sp.asUInt)
        XSDebug(spec_pop, "(spec_ras)pop  outAddr: 0x%x \n",io.out.target)
        val redirectUpdate = redirect.bits.cfiUpdate
        XSDebug("copyValid:%d recover(SP:%d retAddr:%x ctr:%d) \n",
            copy_valid,redirectUpdate.rasSp,redirectUpdate.rasEntry.retAddr,redirectUpdate.rasEntry.ctr)
    }

}
