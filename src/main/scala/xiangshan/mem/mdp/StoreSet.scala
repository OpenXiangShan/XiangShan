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

package xiangshan.mem.mdp

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.rob.RobPtr

// store set load violation predictor
// See "Memory Dependence Prediction using Store Sets" for details

// Store Set Identifier Table Entry
class SSITEntry(implicit p: Parameters) extends XSBundle {
  val valid = Bool()
  val ssid = UInt(SSIDWidth.W) // store set identifier
  val strict = Bool() // strict load wait is needed
}

// Store Set Identifier Table Entry
class SSITDataEntry(implicit p: Parameters) extends XSBundle {
  val ssid = UInt(SSIDWidth.W) // store set identifier
  val strict = Bool() // strict load wait is needed
}

// Store Set Identifier Table
class SSIT(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    // to decode
    val raddr = Vec(DecodeWidth, Input(UInt(MemPredPCWidth.W))) // xor hashed decode pc(VaddrBits-1, 1)
    // to rename
    val rdata = Vec(RenameWidth, Output(new SSITEntry))
    // misc
    val update = Input(new MemPredUpdateReq) // RegNext should be added outside
    val csrCtrl = Input(new CustomCSRCtrlIO)
  })

  // raddrs are sent to ssit in decode
  // rdata will be send to rename
  require(DecodeWidth == RenameWidth)

  // data sram read port allocate 
  //
  // SSIT update logic will reuse decode ssit read port.
  // If io.update.valid, a redirect will be send to frontend,
  // then decode will not need to read SSIT
  val SSIT_DECODE_READ_PORT_BASE = 0
  val SSIT_UPDATE_LOAD_READ_PORT = 0
  val SSIT_UPDATE_STORE_READ_PORT = 1
  val SSIT_READ_PORT_NUM = DecodeWidth

  // data sram write port allocate
  // load update and flush uses the same write port
  val SSIT_MISC_WRITE_PORT = 0
  val SSIT_UPDATE_LOAD_WRITE_PORT = 0
  val SSIT_UPDATE_STORE_WRITE_PORT = 1
  val SSIT_WRITE_PORT_NUM = 2

  val valid_sram = Module(new SyncDataModuleTemplate(
    Bool(),
    SSITSize,
    SSIT_READ_PORT_NUM,
    SSIT_WRITE_PORT_NUM
  ))

  val data_sram = Module(new SyncDataModuleTemplate(
    new SSITDataEntry,
    SSITSize,
    SSIT_READ_PORT_NUM,
    SSIT_WRITE_PORT_NUM
  ))

  (0 until SSIT_WRITE_PORT_NUM).map(i => {
    valid_sram.io.wen(i) := false.B
    valid_sram.io.waddr(i) := DontCare
    valid_sram.io.wdata(i) := DontCare
    data_sram.io.wen(i) := false.B
    data_sram.io.waddr(i) := DontCare
    data_sram.io.wdata(i) := DontCare
  })

  val debug_valid = RegInit(VecInit(Seq.fill(SSITSize)(false.B)))
  val debug_ssid = Reg(Vec(SSITSize, UInt(SSIDWidth.W)))
  val debug_strict = Reg(Vec(SSITSize, Bool()))
  if(!env.FPGAPlatform){
    dontTouch(debug_valid)
    dontTouch(debug_ssid)
    dontTouch(debug_strict)
  }

  val resetCounter = RegInit(0.U(ResetTimeMax2Pow.W))
  resetCounter := resetCounter + 1.U

  for (i <- 0 until DecodeWidth) {
    // io.rdata(i).valid := RegNext(valid(io.raddr(i)))
    // io.rdata(i).ssid := RegNext(ssid(io.raddr(i)))
    // io.rdata(i).strict := RegNext(strict(io.raddr(i)) && valid(io.raddr(i)))

    // read SSIT in decode stage
    valid_sram.io.raddr(i) := io.raddr(i)
    data_sram.io.raddr(i) := io.raddr(i)
    
    // gen result in rename stage
    io.rdata(i).valid := valid_sram.io.rdata(i)
    io.rdata(i).ssid := data_sram.io.rdata(i).ssid
    io.rdata(i).strict := data_sram.io.rdata(i).strict
  }

  // flush SSIT
  // reset period: ResetTimeMax2Pow
  val resetStepCounter = RegInit(0.U((log2Up(SSITSize)+1).W))
  val resetStepCounterFull = resetStepCounter(log2Up(SSITSize))
  val s_idle :: s_flush :: Nil = Enum(2)
  val state = RegInit(s_flush)
  
  switch (state) {
    is(s_idle) {
      when(resetCounter(ResetTimeMax2Pow-1, ResetTimeMin2Pow)(RegNext(io.csrCtrl.lvpred_timeout))) {
        state := s_flush
        resetCounter := 0.U
      }
    }
    is(s_flush) {
      when(resetStepCounterFull) {
        state := s_idle // reset finished
        resetStepCounter := 0.U
      }.otherwise{
        valid_sram.io.wen(SSIT_MISC_WRITE_PORT) := true.B
        valid_sram.io.waddr(SSIT_MISC_WRITE_PORT) := resetStepCounter
        valid_sram.io.wdata(SSIT_MISC_WRITE_PORT) := false.B
        debug_valid(resetStepCounter) := false.B
        resetStepCounter := resetStepCounter + 1.U
      }
    }
  }

  // update SSIT if load violation redirect is detected

  // update stage 0: read ssit
  val memPredUpdateReqValid = RegNext(io.update.valid)
  val memPredUpdateReqReg = RegEnable(io.update, enable = io.update.valid)

  // when io.update.valid, take over ssit read port
  when (io.update.valid) {
    valid_sram.io.raddr(SSIT_UPDATE_LOAD_READ_PORT) := io.update.ldpc
    valid_sram.io.raddr(SSIT_UPDATE_STORE_READ_PORT) := io.update.stpc
    data_sram.io.raddr(SSIT_UPDATE_LOAD_READ_PORT) := io.update.ldpc
    data_sram.io.raddr(SSIT_UPDATE_STORE_READ_PORT) := io.update.stpc
  }

  // update stage 1: get ssit read result, update ssit data_sram

  // Read result
  // load has already been assigned with a store set
  val loadAssigned = valid_sram.io.rdata(SSIT_UPDATE_LOAD_READ_PORT)
  val loadOldSSID = data_sram.io.rdata(SSIT_UPDATE_LOAD_READ_PORT).ssid
  val loadStrict = data_sram.io.rdata(SSIT_UPDATE_LOAD_READ_PORT).strict
  // store has already been assigned with a store set
  val storeAssigned = valid_sram.io.rdata(SSIT_UPDATE_STORE_READ_PORT)
  val storeOldSSID = data_sram.io.rdata(SSIT_UPDATE_STORE_READ_PORT).ssid
  val storeStrict = data_sram.io.rdata(SSIT_UPDATE_STORE_READ_PORT).strict
  // both the load and the store have already been assigned store sets
  // but load's store set ID is smaller
  val winnerSSID = Mux(loadOldSSID < storeOldSSID, loadOldSSID, storeOldSSID)
  val ssidIsSame = loadOldSSID === storeOldSSID

  // for now we just use lowest bits of ldpc as store set id
  val ssidAllocate = memPredUpdateReqReg.ldpc(SSIDWidth-1, 0)

  def update_ld_ssit_entry(pc: UInt, valid: Bool, ssid: UInt, strict: Bool) = {
    valid_sram.io.wen(SSIT_UPDATE_LOAD_WRITE_PORT) := true.B
    valid_sram.io.waddr(SSIT_UPDATE_LOAD_WRITE_PORT) := pc
    valid_sram.io.wdata(SSIT_UPDATE_LOAD_WRITE_PORT) := valid
    data_sram.io.wen(SSIT_UPDATE_LOAD_WRITE_PORT) := true.B
    data_sram.io.waddr(SSIT_UPDATE_LOAD_WRITE_PORT) := pc
    data_sram.io.wdata(SSIT_UPDATE_LOAD_WRITE_PORT).ssid := ssid
    data_sram.io.wdata(SSIT_UPDATE_LOAD_WRITE_PORT).strict := strict
    debug_valid(pc) := valid
    debug_ssid(pc) := ssid
    debug_strict(pc) := strict 
  }

  def update_st_ssit_entry(pc: UInt, valid: Bool, ssid: UInt, strict: Bool) = {
    valid_sram.io.wen(SSIT_UPDATE_STORE_WRITE_PORT) := true.B
    valid_sram.io.waddr(SSIT_UPDATE_STORE_WRITE_PORT) := pc
    valid_sram.io.wdata(SSIT_UPDATE_STORE_WRITE_PORT):= valid
    data_sram.io.wen(SSIT_UPDATE_STORE_WRITE_PORT) := true.B
    data_sram.io.waddr(SSIT_UPDATE_STORE_WRITE_PORT) := pc
    data_sram.io.wdata(SSIT_UPDATE_STORE_WRITE_PORT).ssid := ssid
    data_sram.io.wdata(SSIT_UPDATE_STORE_WRITE_PORT).strict := strict
    debug_valid(pc) := valid
    debug_ssid(pc) := ssid
    debug_strict(pc) := strict 
  }

  // update stage 1
  when(memPredUpdateReqValid){
    switch (Cat(loadAssigned, storeAssigned)) {
      // 1. "If neither the load nor the store has been assigned a store set,
      // one is allocated and assigned to both instructions."
      is ("b00".U(2.W)) {
        update_ld_ssit_entry(
          pc = memPredUpdateReqReg.ldpc,
          valid = true.B,
          ssid = ssidAllocate,
          strict = false.B
        )
        update_st_ssit_entry(
          pc = memPredUpdateReqReg.stpc,
          valid = true.B,
          ssid = ssidAllocate,
          strict = false.B
        )
      }
      // 2. "If the load has been assigned a store set, but the store has not,
      // the store is assigned the load’s store set."
      is ("b10".U(2.W)) {
        update_st_ssit_entry(
          pc = memPredUpdateReqReg.stpc,
          valid = true.B,
          ssid = loadOldSSID,
          strict = false.B
        )
      }
      // 3. "If the store has been assigned a store set, but the load has not,
      // the load is assigned the store’s store set."
      is ("b01".U(2.W)) {
        update_ld_ssit_entry(
          pc = memPredUpdateReqReg.ldpc,
          valid = true.B,
          ssid = storeOldSSID,
          strict = false.B
        )
      }
      // 4. "If both the load and the store have already been assigned store sets,
      // one of the two store sets is declared the "winner".
      // The instruction belonging to the loser’s store set is assigned the winner’s store set."
      is ("b11".U(2.W)) {
        update_ld_ssit_entry(
          pc = memPredUpdateReqReg.ldpc,
          valid = true.B,
          ssid = winnerSSID,
          strict = false.B
        )
        update_st_ssit_entry(
          pc = memPredUpdateReqReg.stpc,
          valid = true.B,
          ssid = winnerSSID,
          strict = false.B
        )
        when(ssidIsSame){
          data_sram.io.wdata(SSIT_UPDATE_LOAD_READ_PORT).strict := true.B
          debug_strict(memPredUpdateReqReg.ldpc) := false.B
        }
      }
    }
  }

  // make SyncDataModuleTemplate happy
  when(valid_sram.io.waddr(SSIT_UPDATE_LOAD_WRITE_PORT) === valid_sram.io.waddr(SSIT_UPDATE_STORE_WRITE_PORT)){
    valid_sram.io.wen(SSIT_UPDATE_STORE_WRITE_PORT) := false.B
  }

  when(data_sram.io.waddr(SSIT_UPDATE_LOAD_WRITE_PORT) === data_sram.io.waddr(SSIT_UPDATE_STORE_WRITE_PORT)){
    data_sram.io.wen(SSIT_UPDATE_STORE_WRITE_PORT) := false.B
  }

  XSPerfAccumulate("ssit_update_lxsx", memPredUpdateReqValid && !loadAssigned && !storeAssigned)
  XSPerfAccumulate("ssit_update_lysx", memPredUpdateReqValid && loadAssigned && !storeAssigned)
  XSPerfAccumulate("ssit_update_lxsy", memPredUpdateReqValid && !loadAssigned && storeAssigned)
  XSPerfAccumulate("ssit_update_lysy", memPredUpdateReqValid && loadAssigned && storeAssigned)
  XSPerfAccumulate("ssit_update_should_strict", memPredUpdateReqValid && ssidIsSame && loadAssigned && storeAssigned)
  XSPerfAccumulate("ssit_update_strict_failed", 
    memPredUpdateReqValid && ssidIsSame && loadStrict && loadAssigned && storeAssigned
  ) // should be zero


  // debug
  for (i <- 0 until StorePipelineWidth) {
    when (memPredUpdateReqReg.valid) {
      XSDebug("%d: SSIT update: load pc %x store pc %x\n", GTimer(), memPredUpdateReqReg.ldpc, memPredUpdateReqReg.stpc)
      XSDebug("%d: SSIT update: load valid %b ssid %x  store valid %b ssid %x\n", GTimer(), loadAssigned, loadOldSSID, storeAssigned, storeOldSSID)
    }
  }
}


// Last Fetched Store Table Entry
class LFSTEntry(implicit p: Parameters) extends XSBundle  {
  val valid = Bool()
  val robIdx = new RobPtr
}

class LFSTReq(implicit p: Parameters) extends XSBundle {
  val isstore = Bool()
  val ssid = UInt(SSIDWidth.W) // use ssid to lookup LFST
  val robIdx = new RobPtr
}

class LFSTResp(implicit p: Parameters) extends XSBundle {
  val shouldWait = Bool()
  val robIdx = new RobPtr
}

class DispatchLFSTIO(implicit p: Parameters) extends XSBundle {
  val req = Vec(RenameWidth, Valid(new LFSTReq))
  val resp = Vec(RenameWidth, Flipped(Valid(new LFSTResp)))
}

// Last Fetched Store Table
class LFST(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    // when redirect, mark canceled store as invalid
    val redirect = Input(Valid(new Redirect))
    val dispatch = Flipped(new DispatchLFSTIO)
    // when store issued, mark store as invalid
    val storeIssue = Vec(exuParameters.StuCnt, Flipped(Valid(new ExuInput)))
    val csrCtrl = Input(new CustomCSRCtrlIO)
  })

  val validVec = RegInit(VecInit(Seq.fill(LFSTSize)(VecInit(Seq.fill(LFSTWidth)(false.B)))))
  val robIdxVec = Reg(Vec(LFSTSize, Vec(LFSTWidth, new RobPtr)))
  val allocPtr = RegInit(VecInit(Seq.fill(LFSTSize)(0.U(log2Up(LFSTWidth).W))))
  val valid = Wire(Vec(LFSTSize, Bool()))
  (0 until LFSTSize).map(i => {
    valid(i) := validVec(i).asUInt.orR
  })

  // read LFST in rename stage
  for (i <- 0 until RenameWidth) {
    io.dispatch.resp(i).valid := io.dispatch.req(i).valid

    // If store-load pair is in the same dispatch bundle, loadWaitBit should also be set for load
    val hitInDispatchBundleVec = if(i > 0){
      WireInit(VecInit((0 until i).map(j =>
        io.dispatch.req(j).valid &&
        io.dispatch.req(j).bits.isstore &&
        io.dispatch.req(j).bits.ssid === io.dispatch.req(i).bits.ssid
      )))
    } else {
      WireInit(VecInit(Seq(false.B))) // DontCare
    }
    val hitInDispatchBundle = hitInDispatchBundleVec.asUInt.orR
    // Check if store set is valid in LFST
    io.dispatch.resp(i).bits.shouldWait := (
        (valid(io.dispatch.req(i).bits.ssid) || hitInDispatchBundle) && 
        io.dispatch.req(i).valid &&
        (!io.dispatch.req(i).bits.isstore || io.csrCtrl.storeset_wait_store)
      ) && !io.csrCtrl.lvpred_disable || io.csrCtrl.no_spec_load
    io.dispatch.resp(i).bits.robIdx := robIdxVec(io.dispatch.req(i).bits.ssid)(allocPtr(io.dispatch.req(i).bits.ssid)-1.U)
    if(i > 0){
      (0 until i).map(j =>
        when(hitInDispatchBundleVec(j)){
          io.dispatch.resp(i).bits.robIdx := io.dispatch.req(i).bits.robIdx
        }
      )
    }
  }

  // when store is issued, mark it as invalid
  (0 until exuParameters.StuCnt).map(i => {
    // TODO: opt timing
    (0 until LFSTWidth).map(j => {
      when(io.storeIssue(i).valid && io.storeIssue(i).bits.uop.robIdx.value === robIdxVec(io.storeIssue(i).bits.uop.cf.ssid)(j).value){
        validVec(io.storeIssue(i).bits.uop.cf.ssid)(j) := false.B
      }
    })
  })

  // when store is dispatched, mark it as valid
  (0 until RenameWidth).map(i => {
    when(io.dispatch.req(i).valid && io.dispatch.req(i).bits.isstore){
      val waddr = io.dispatch.req(i).bits.ssid
      val wptr = allocPtr(waddr)
      allocPtr(waddr) := allocPtr(waddr) + 1.U
      validVec(waddr)(wptr) := true.B
      robIdxVec(waddr)(wptr) := io.dispatch.req(i).bits.robIdx
    }
  })

  // when redirect, cancel store influenced
  (0 until LFSTSize).map(i => {
    (0 until LFSTWidth).map(j => {
      when(robIdxVec(i)(j).needFlush(io.redirect)){
        validVec(i)(j) := false.B
      }
    })
  })

  // recover robIdx after squash
  // behavior model, to be refactored later 
  when(RegNext(io.redirect.fire())) {
    (0 until LFSTSize).map(i => {
      (0 until LFSTWidth).map(j => {
        val check_position = WireInit(allocPtr(i) + (j+1).U)
        when(!validVec(i)(check_position)){
          allocPtr(i) := check_position
        }
      })
    })
  }
}