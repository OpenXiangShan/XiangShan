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
import utility._
import xiangshan.backend.rob.RobPtr
import xiangshan.v2backend.Bundles.DynInst

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

  val valid_array = Module(new SyncDataModuleTemplate(
    Bool(),
    SSITSize,
    SSIT_READ_PORT_NUM,
    SSIT_WRITE_PORT_NUM
  ))

  val data_array = Module(new SyncDataModuleTemplate(
    new SSITDataEntry,
    SSITSize,
    SSIT_READ_PORT_NUM,
    SSIT_WRITE_PORT_NUM
  ))

  // TODO: use SRAM or not?
  (0 until SSIT_WRITE_PORT_NUM).map(i => {
    valid_array.io.wen(i) := false.B
    valid_array.io.waddr(i) := DontCare
    valid_array.io.wdata(i) := DontCare
    data_array.io.wen(i) := false.B
    data_array.io.waddr(i) := DontCare
    data_array.io.wdata(i) := DontCare
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
    valid_array.io.raddr(i) := io.raddr(i)
    data_array.io.raddr(i) := io.raddr(i)

    // gen result in rename stage
    io.rdata(i).valid := valid_array.io.rdata(i)
    io.rdata(i).ssid := data_array.io.rdata(i).ssid
    io.rdata(i).strict := data_array.io.rdata(i).strict
  }

  // flush SSIT
  // reset period: ResetTimeMax2Pow
  val resetStepCounter = RegInit(0.U(log2Up(SSITSize + 1).W))
  val s_idle :: s_flush :: Nil = Enum(2)
  val state = RegInit(s_flush)

  switch (state) {
    is(s_idle) {
      when(resetCounter(ResetTimeMax2Pow - 1, ResetTimeMin2Pow)(RegNext(io.csrCtrl.lvpred_timeout))) {
        state := s_flush
        resetCounter := 0.U
      }
    }
    is(s_flush) {
      when(resetStepCounter === (SSITSize - 1).U) {
        state := s_idle // reset finished
        resetStepCounter := 0.U
      }.otherwise{
        resetStepCounter := resetStepCounter + 1.U
      }
      valid_array.io.wen(SSIT_MISC_WRITE_PORT) := true.B
      valid_array.io.waddr(SSIT_MISC_WRITE_PORT) := resetStepCounter
      valid_array.io.wdata(SSIT_MISC_WRITE_PORT) := false.B
      debug_valid(resetStepCounter) := false.B
    }
  }
  XSPerfAccumulate("reset_timeout", state === s_flush && resetCounter === 0.U)

  // update SSIT if load violation redirect is detected

  // update stage 0: read ssit
  val s1_mempred_update_req_valid = RegNext(io.update.valid)
  val s1_mempred_update_req = RegEnable(io.update, io.update.valid)

  // when io.update.valid, take over ssit read port
  when (io.update.valid) {
    valid_array.io.raddr(SSIT_UPDATE_LOAD_READ_PORT) := io.update.ldpc
    valid_array.io.raddr(SSIT_UPDATE_STORE_READ_PORT) := io.update.stpc
    data_array.io.raddr(SSIT_UPDATE_LOAD_READ_PORT) := io.update.ldpc
    data_array.io.raddr(SSIT_UPDATE_STORE_READ_PORT) := io.update.stpc
  }

  // update stage 1: get ssit read result

  // Read result
  // load has already been assigned with a store set
  val s1_loadAssigned = valid_array.io.rdata(SSIT_UPDATE_LOAD_READ_PORT)
  val s1_loadOldSSID = data_array.io.rdata(SSIT_UPDATE_LOAD_READ_PORT).ssid
  val s1_loadStrict = data_array.io.rdata(SSIT_UPDATE_LOAD_READ_PORT).strict
  // store has already been assigned with a store set
  val s1_storeAssigned = valid_array.io.rdata(SSIT_UPDATE_STORE_READ_PORT)
  val s1_storeOldSSID = data_array.io.rdata(SSIT_UPDATE_STORE_READ_PORT).ssid
  val s1_storeStrict = data_array.io.rdata(SSIT_UPDATE_STORE_READ_PORT).strict
  // val s1_ssidIsSame = s1_loadOldSSID === s1_storeOldSSID

  // update stage 2, update ssit data_array
  val s2_mempred_update_req_valid = RegNext(s1_mempred_update_req_valid)
  val s2_mempred_update_req = RegEnable(s1_mempred_update_req, s1_mempred_update_req_valid)
  val s2_loadAssigned = RegEnable(s1_loadAssigned, s1_mempred_update_req_valid)
  val s2_storeAssigned = RegEnable(s1_storeAssigned, s1_mempred_update_req_valid)
  val s2_loadOldSSID = RegEnable(s1_loadOldSSID, s1_mempred_update_req_valid)
  val s2_storeOldSSID = RegEnable(s1_storeOldSSID, s1_mempred_update_req_valid)
  val s2_loadStrict = RegEnable(s1_loadStrict, s1_mempred_update_req_valid)

  val s2_ssidIsSame = s2_loadOldSSID === s2_storeOldSSID
  // for now we just use lowest bits of ldpc as store set id
  val s2_ldSsidAllocate = XORFold(s1_mempred_update_req.ldpc, SSIDWidth)
  val s2_stSsidAllocate = XORFold(s1_mempred_update_req.stpc, SSIDWidth)
  // both the load and the store have already been assigned store sets
  // but load's store set ID is smaller
  val s2_winnerSSID = Mux(s2_loadOldSSID < s2_storeOldSSID, s2_loadOldSSID, s2_storeOldSSID)

  def update_ld_ssit_entry(pc: UInt, valid: Bool, ssid: UInt, strict: Bool) = {
    valid_array.io.wen(SSIT_UPDATE_LOAD_WRITE_PORT) := true.B
    valid_array.io.waddr(SSIT_UPDATE_LOAD_WRITE_PORT) := pc
    valid_array.io.wdata(SSIT_UPDATE_LOAD_WRITE_PORT) := valid
    data_array.io.wen(SSIT_UPDATE_LOAD_WRITE_PORT) := true.B
    data_array.io.waddr(SSIT_UPDATE_LOAD_WRITE_PORT) := pc
    data_array.io.wdata(SSIT_UPDATE_LOAD_WRITE_PORT).ssid := ssid
    data_array.io.wdata(SSIT_UPDATE_LOAD_WRITE_PORT).strict := strict
    debug_valid(pc) := valid
    debug_ssid(pc) := ssid
    debug_strict(pc) := strict
  }

  def update_st_ssit_entry(pc: UInt, valid: Bool, ssid: UInt, strict: Bool) = {
    valid_array.io.wen(SSIT_UPDATE_STORE_WRITE_PORT) := true.B
    valid_array.io.waddr(SSIT_UPDATE_STORE_WRITE_PORT) := pc
    valid_array.io.wdata(SSIT_UPDATE_STORE_WRITE_PORT):= valid
    data_array.io.wen(SSIT_UPDATE_STORE_WRITE_PORT) := true.B
    data_array.io.waddr(SSIT_UPDATE_STORE_WRITE_PORT) := pc
    data_array.io.wdata(SSIT_UPDATE_STORE_WRITE_PORT).ssid := ssid
    data_array.io.wdata(SSIT_UPDATE_STORE_WRITE_PORT).strict := strict
    debug_valid(pc) := valid
    debug_ssid(pc) := ssid
    debug_strict(pc) := strict
  }

  when(s2_mempred_update_req_valid){
    switch (Cat(s2_loadAssigned, s2_storeAssigned)) {
      // 1. "If neither the load nor the store has been assigned a store set,
      // two are allocated and assigned to each instruction."
      is ("b00".U(2.W)) {
        update_ld_ssit_entry(
          pc = s2_mempred_update_req.ldpc,
          valid = true.B,
          ssid = s2_ldSsidAllocate,
          strict = false.B
        )
        update_st_ssit_entry(
          pc = s2_mempred_update_req.stpc,
          valid = true.B,
          ssid = s2_stSsidAllocate,
          strict = false.B
        )
      }
      // 2. "If the load has been assigned a store set, but the store has not,
      // one is allocated and assigned to the store instructions."
      is ("b10".U(2.W)) {
        update_st_ssit_entry(
          pc = s2_mempred_update_req.stpc,
          valid = true.B,
          ssid = s2_stSsidAllocate,
          strict = false.B
        )
      }
      // 3. "If the store has been assigned a store set, but the load has not,
      // one is allocated and assigned to the load instructions."
      is ("b01".U(2.W)) {
        update_ld_ssit_entry(
          pc = s2_mempred_update_req.ldpc,
          valid = true.B,
          ssid = s2_ldSsidAllocate,
          strict = false.B
        )
      }
      // 4. "If both the load and the store have already been assigned store sets,
      // one of the two store sets is declared the "winner".
      // The instruction belonging to the loser’s store set is assigned the winner’s store set."
      is ("b11".U(2.W)) {
        update_ld_ssit_entry(
          pc = s2_mempred_update_req.ldpc,
          valid = true.B,
          ssid = s2_winnerSSID,
          strict = false.B
        )
        update_st_ssit_entry(
          pc = s2_mempred_update_req.stpc,
          valid = true.B,
          ssid = s2_winnerSSID,
          strict = false.B
        )
        when(s2_ssidIsSame){
          data_array.io.wdata(SSIT_UPDATE_LOAD_READ_PORT).strict := true.B
          debug_strict(s2_mempred_update_req.ldpc) := true.B
        }
      }
    }
  }

  // make SyncDataModuleTemplate happy
  when(valid_array.io.waddr(SSIT_UPDATE_LOAD_WRITE_PORT) === valid_array.io.waddr(SSIT_UPDATE_STORE_WRITE_PORT)){
    valid_array.io.wen(SSIT_UPDATE_STORE_WRITE_PORT) := false.B
  }

  when(data_array.io.waddr(SSIT_UPDATE_LOAD_WRITE_PORT) === data_array.io.waddr(SSIT_UPDATE_STORE_WRITE_PORT)){
    data_array.io.wen(SSIT_UPDATE_STORE_WRITE_PORT) := false.B
  }

  XSPerfAccumulate("ssit_update_lxsx", s2_mempred_update_req_valid && !s2_loadAssigned && !s2_storeAssigned)
  XSPerfAccumulate("ssit_update_lysx", s2_mempred_update_req_valid && s2_loadAssigned && !s2_storeAssigned)
  XSPerfAccumulate("ssit_update_lxsy", s2_mempred_update_req_valid && !s2_loadAssigned && s2_storeAssigned)
  XSPerfAccumulate("ssit_update_lysy", s2_mempred_update_req_valid && s2_loadAssigned && s2_storeAssigned)
  XSPerfAccumulate("ssit_update_should_strict", s2_mempred_update_req_valid && s2_ssidIsSame && s2_loadAssigned && s2_storeAssigned)
  XSPerfAccumulate("ssit_update_strict_failed",
    s2_mempred_update_req_valid && s2_ssidIsSame && s2_loadStrict && s2_loadAssigned && s2_storeAssigned
  ) // should be zero

  // debug
  when (s2_mempred_update_req.valid) {
    XSDebug("%d: SSIT update: load pc %x store pc %x\n", GTimer(), s2_mempred_update_req.ldpc, s2_mempred_update_req.stpc)
    XSDebug("%d: SSIT update: load valid %b ssid %x  store valid %b ssid %x\n", GTimer(), s2_loadAssigned, s2_loadOldSSID, s2_storeAssigned, s2_storeOldSSID)
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
    val storeIssue = Vec(backendParams.StuCnt, Flipped(Valid(new DynInst)))
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
          io.dispatch.resp(i).bits.robIdx := io.dispatch.req(j).bits.robIdx
        }
      )
    }
  }

  // when store is issued, mark it as invalid
  (0 until backendParams.StuCnt).map(i => {
    // TODO: opt timing
    (0 until LFSTWidth).map(j => {
      when(io.storeIssue(i).valid && io.storeIssue(i).bits.storeSetHit && io.storeIssue(i).bits.robIdx.value === robIdxVec(io.storeIssue(i).bits.ssid)(j).value){
        validVec(io.storeIssue(i).bits.ssid)(j) := false.B
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
      when(validVec(i)(j) && robIdxVec(i)(j).needFlush(io.redirect)){
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
