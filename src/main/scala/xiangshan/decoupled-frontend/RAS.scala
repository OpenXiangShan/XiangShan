/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
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
import chisel3.experimental.chiselName
import chisel3.util._
import utils._
import xiangshan._

class RASEntry()(implicit p: Parameters) extends XSBundle {
    val retAddr = UInt(VAddrBits.W)
    val ctr = UInt(8.W) // layer of nested call functions
}

@chiselName
class RAS(implicit p: Parameters) extends BasePredictor {
  object RASEntry {
    def apply(retAddr: UInt, ctr: UInt): RASEntry = {
      val e = Wire(new RASEntry)
      e.retAddr := retAddr
      e.ctr := ctr
      e
    }
  }

  @chiselName
  class RASStack(val rasSize: Int) extends XSModule {
    val io = IO(new Bundle {
      val push_valid = Input(Bool())
      val pop_valid = Input(Bool())
      val spec_new_addr = Input(UInt(VAddrBits.W))

      val recover_sp = Input(UInt(log2Up(rasSize).W))
      val recover_top = Input(new RASEntry)
      val recover_valid = Input(Bool())
      val recover_push = Input(Bool())
      val recover_pop = Input(Bool())
      val recover_new_addr = Input(UInt(VAddrBits.W))

      val sp = Output(UInt(log2Up(rasSize).W))
      val top = Output(new RASEntry)
    })

    val debugIO = IO(new Bundle{
        val push_entry = Output(new RASEntry)
        val alloc_new = Output(Bool())
        val sp = Output(UInt(log2Up(rasSize).W))
        val topRegister = Output(new RASEntry)
        val out_mem = Output(Vec(RasSize, new RASEntry))
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
  // val jump_is_first = io.callIdx.bits === 0.U
  // val call_is_last_half = io.isLastHalfRVI && jump_is_first
  // val spec_new_addr = packetAligned(io.pc.bits) + (io.callIdx.bits << instOffsetBits.U) + Mux( (io.isRVC | call_is_last_half) && HasCExtension.B, 2.U, 4.U)
  val spec_new_addr = io.in.bits.resp_in(0).s2.fallThroughAddr
  spec_ras.push_valid := spec_push
  spec_ras.pop_valid  := spec_pop
  spec_ras.spec_new_addr := spec_new_addr
  val spec_top_addr = spec_ras.top.retAddr

  // confirm that the call/ret is the taken cfi
  spec_push := io.s2_fire && io.in.bits.resp_in(0).s2.hit_taken_on_call
  spec_pop  := io.s2_fire && io.in.bits.resp_in(0).s2.hit_taken_on_ret
  
  when (spec_pop) {
    io.out.resp.s2.preds.jmp_target := spec_top_addr
  }

  io.out.resp.s2.rasSp  := spec_ras.sp
  io.out.resp.s2.rasTop := spec_ras.top
  
  io.out.resp.s3 := RegEnable(io.out.resp.s2, io.s2_fire)

  val redirect = RegNext(io.redirect)
  val do_recover = redirect.valid
  val recover_cfi = redirect.bits.cfiUpdate

  val retMissPred  = do_recover && redirect.bits.level === 0.U && recover_cfi.pd.isRet
  val callMissPred = do_recover && redirect.bits.level === 0.U && recover_cfi.pd.isCall
  // when we mispredict a call, we must redo a push operation
  // similarly, when we mispredict a return, we should redo a pop
  spec_ras.recover_valid := do_recover
  spec_ras.recover_push := callMissPred
  spec_ras.recover_pop  := retMissPred

  spec_ras.recover_sp  := recover_cfi.rasSp
  spec_ras.recover_top := recover_cfi.rasEntry
  spec_ras.recover_new_addr := recover_cfi.pc + Mux(recover_cfi.pd.isRVC, 2.U, 4.U)

  // TODO: back-up stack for ras
  // use checkpoint to recover RAS

  if (debug && !env.FPGAPlatform && env.EnablePerfDebug) {
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
      XSDebug(spec_pop, "(spec_ras)pop  outAddr: 0x%x \n",io.out.resp.s2.target)
      val redirectUpdate = redirect.bits.cfiUpdate
      XSDebug("recoverValid:%d recover(SP:%d retAddr:%x ctr:%d) \n",
          do_recover,redirectUpdate.rasSp,redirectUpdate.rasEntry.retAddr,redirectUpdate.rasEntry.ctr)
  }
}
