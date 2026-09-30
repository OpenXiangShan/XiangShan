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
*
*
* Acknowledgement
*
* This implementation is inspired by several key papers:
* [1] George Z. Chrysos, and Joel S. Emer. "[Memory dependence prediction using store sets.]
* (https://doi.org/10.1109/ISCA.1998.694770)" 25th Annual International Symposium on Computer Architecture (ISCA).
* 1998.
***************************************************************************************/

package xiangshan.mem.mdp

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._
import xiangshan.backend.Bundles._
import xiangshan.mem.SqPtr

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
}

// Store Set Identifier Table
class SSIT(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    // to decode
    val ren = Vec(DecodeWidth, Input(Bool()))
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

  private def hasRen: Boolean = true
  val valid_array = Module(new SyncDataModuleTemplate(
    Bool(),
    SSITSize,
    SSIT_READ_PORT_NUM,
    SSIT_WRITE_PORT_NUM,
    hasRen = hasRen,
  ))

  val data_array = Module(new SyncDataModuleTemplate(
    new SSITDataEntry,
    SSITSize,
    SSIT_READ_PORT_NUM,
    SSIT_WRITE_PORT_NUM,
    hasRen = hasRen,
  ))

  // TODO: use SRAM or not?
  (0 until SSIT_WRITE_PORT_NUM).foreach(i => {
    valid_array.io.wen(i) := false.B
    valid_array.io.waddr(i) := 0.U
    valid_array.io.wdata(i) := false.B
    data_array.io.wen(i) := false.B
    data_array.io.waddr(i) := 0.U
    data_array.io.wdata(i) := 0.U.asTypeOf(new SSITDataEntry)
  })

  val debug_valid = RegInit(VecInit(Seq.fill(SSITSize)(false.B)))
  val debug_ssid = Reg(Vec(SSITSize, UInt(SSIDWidth.W)))
  val strictArray = RegInit(VecInit(Seq.fill(SSITSize)(false.B)))
  if(!env.FPGAPlatform){
    dontTouch(debug_valid)
    dontTouch(debug_ssid)
    dontTouch(strictArray)
  }

  val resetCounter = RegInit(0.U(ResetTimeMax2Pow.W))
  resetCounter := resetCounter + 1.U

  val strictReadAddr = Wire(Vec(SSIT_READ_PORT_NUM, UInt(MemPredPCWidth.W)))
  val strictReadEnable = Wire(Vec(SSIT_READ_PORT_NUM, Bool()))
  val strictReadData = Wire(Vec(SSIT_READ_PORT_NUM, Bool()))

  for (i <- 0 until DecodeWidth) {
    // io.rdata(i).valid := RegNext(valid(io.raddr(i)))
    // io.rdata(i).ssid := RegNext(ssid(io.raddr(i)))
    // io.rdata(i).strict := RegNext(strict(io.raddr(i)) && valid(io.raddr(i)))

    // read SSIT in decode stage
    valid_array.io.ren.get(i) := io.ren(i)
    data_array.io.ren.get(i) := io.ren(i)
    valid_array.io.raddr(i) := io.raddr(i)
    data_array.io.raddr(i) := io.raddr(i)
    strictReadAddr(i) := io.raddr(i)
    strictReadEnable(i) := io.ren(i)
    strictReadData(i) := RegEnable(strictArray(strictReadAddr(i)), strictReadEnable(i))

    // gen result in rename stage
    io.rdata(i).valid := valid_array.io.rdata(i)
    io.rdata(i).ssid := data_array.io.rdata(i).ssid
    io.rdata(i).strict := strictReadData(i)
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
      debug_valid(resetStepCounter(log2Ceil(SSITSize) - 1, 0)) := false.B
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

    valid_array.io.ren.get(SSIT_UPDATE_LOAD_READ_PORT)  := true.B
    valid_array.io.ren.get(SSIT_UPDATE_STORE_READ_PORT) := true.B
    data_array.io.ren.get(SSIT_UPDATE_LOAD_READ_PORT)   := true.B
    data_array.io.ren.get(SSIT_UPDATE_STORE_READ_PORT)  := true.B
    strictReadAddr(SSIT_UPDATE_LOAD_READ_PORT) := io.update.ldpc
    strictReadAddr(SSIT_UPDATE_STORE_READ_PORT) := io.update.stpc
    strictReadEnable(SSIT_UPDATE_LOAD_READ_PORT) := true.B
    strictReadEnable(SSIT_UPDATE_STORE_READ_PORT) := true.B
  }

  // update stage 1: get ssit read result

  // Read result
  // load has already been assigned with a store set
  val s1_loadAssigned = valid_array.io.rdata(SSIT_UPDATE_LOAD_READ_PORT)
  val s1_loadOldSSID = data_array.io.rdata(SSIT_UPDATE_LOAD_READ_PORT).ssid
  val s1_loadStrict = strictReadData(SSIT_UPDATE_LOAD_READ_PORT)
  // store has already been assigned with a store set
  val s1_storeAssigned = valid_array.io.rdata(SSIT_UPDATE_STORE_READ_PORT)
  val s1_storeOldSSID = data_array.io.rdata(SSIT_UPDATE_STORE_READ_PORT).ssid
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
  val s2_ldSsidAllocate = XORFold(s2_mempred_update_req.ldpc, SSIDWidth)
  val s2_stSsidAllocate = XORFold(s2_mempred_update_req.stpc, SSIDWidth)
  val s2_allocSsid = Mux(s2_ldSsidAllocate < s2_stSsidAllocate, s2_ldSsidAllocate, s2_stSsidAllocate)
  // both the load and the store have already been assigned store sets
  // but load's store set ID is smaller
  val s2_winnerSSID = Mux(s2_loadOldSSID < s2_storeOldSSID, s2_loadOldSSID, s2_storeOldSSID)

  val strictTrain = s2_mempred_update_req_valid && s2_loadAssigned && s2_storeAssigned && s2_ssidIsSame
  val strictResetCounter = RegInit(0.U(log2Ceil(strictResetPeriod + 1).W))
  val strictClearTrigger = strictResetCounter === strictResetPeriod.U

  when(strictClearTrigger) {
    strictResetCounter := Mux(strictTrain, 1.U, 0.U)
  }.elsewhen(strictResetCounter =/= 0.U) {
    strictResetCounter := strictResetCounter + 1.U
  }.elsewhen(strictTrain) {
    strictResetCounter := 1.U
  }

  when(strictClearTrigger) {
    strictArray.foreach(_ := false.B)
  }

  val strictWriteData = WireInit(VecInit(Seq.fill(SSIT_WRITE_PORT_NUM)(false.B)))

  def update_ld_ssit_entry(pc: UInt, valid: Bool, ssid: UInt, strict: Bool) = {
    valid_array.io.wen(SSIT_UPDATE_LOAD_WRITE_PORT) := true.B
    valid_array.io.waddr(SSIT_UPDATE_LOAD_WRITE_PORT) := pc
    valid_array.io.wdata(SSIT_UPDATE_LOAD_WRITE_PORT) := valid
    data_array.io.wen(SSIT_UPDATE_LOAD_WRITE_PORT) := true.B
    data_array.io.waddr(SSIT_UPDATE_LOAD_WRITE_PORT) := pc
    data_array.io.wdata(SSIT_UPDATE_LOAD_WRITE_PORT).ssid := ssid
    strictWriteData(SSIT_UPDATE_LOAD_WRITE_PORT) := strict
    debug_valid(pc) := valid
    debug_ssid(pc) := ssid
  }

  def update_st_ssit_entry(pc: UInt, valid: Bool, ssid: UInt, strict: Bool) = {
    valid_array.io.wen(SSIT_UPDATE_STORE_WRITE_PORT) := true.B
    valid_array.io.waddr(SSIT_UPDATE_STORE_WRITE_PORT) := pc
    valid_array.io.wdata(SSIT_UPDATE_STORE_WRITE_PORT):= valid
    data_array.io.wen(SSIT_UPDATE_STORE_WRITE_PORT) := true.B
    data_array.io.waddr(SSIT_UPDATE_STORE_WRITE_PORT) := pc
    data_array.io.wdata(SSIT_UPDATE_STORE_WRITE_PORT).ssid := ssid
    strictWriteData(SSIT_UPDATE_STORE_WRITE_PORT) := strict
    debug_valid(pc) := valid
    debug_ssid(pc) := ssid
  }

  when(s2_mempred_update_req_valid){
    switch (Cat(s2_loadAssigned, s2_storeAssigned)) {
      // 1. "If neither the load nor the store has been assigned a store set,
      // two are allocated and assigned to each instruction."
      is ("b00".U(2.W)) {
        update_ld_ssit_entry(
          pc = s2_mempred_update_req.ldpc,
          valid = true.B,
          ssid = s2_allocSsid,
          strict = false.B
        )
        update_st_ssit_entry(
          pc = s2_mempred_update_req.stpc,
          valid = true.B,
          ssid = s2_allocSsid,
          strict = false.B
        )
      }
      // 2. "If the load has been assigned a store set, but the store has not,
      // one is allocated and assigned to the store instructions."
      is ("b10".U(2.W)) {
        update_st_ssit_entry(
          pc = s2_mempred_update_req.stpc,
          valid = true.B,
          ssid = s2_loadOldSSID,
          strict = false.B
        )
      }
      // 3. "If the store has been assigned a store set, but the load has not,
      // one is allocated and assigned to the load instructions."
      is ("b01".U(2.W)) {
        update_ld_ssit_entry(
          pc = s2_mempred_update_req.ldpc,
          valid = true.B,
          ssid = s2_storeOldSSID,
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
          strictWriteData(SSIT_UPDATE_LOAD_WRITE_PORT) := true.B
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

  for (i <- 0 until SSIT_WRITE_PORT_NUM) {
    val writeEnable = RegNext(data_array.io.wen(i), false.B)
    val writeAddr = RegEnable(data_array.io.waddr(i), data_array.io.wen(i))
    val writeData = RegEnable(strictWriteData(i), data_array.io.wen(i))
    when(writeEnable && !strictClearTrigger) {
      strictArray(writeAddr) := writeData
    }
  }

  // StoreSet ChiselDB trace
  val storeSetUpdateHartId = p(XSCoreParamsKey).HartId
  val storeSetUpdateTable = ChiselDB.createTable(s"StoreSetUpdateDB$storeSetUpdateHartId", new StoreSetUpdateDBEntry, basicDB = false)

  val storeSetUpdateTypeLxsx = 0.U(3.W)
  val storeSetUpdateTypeLysx = 1.U(3.W)
  val storeSetUpdateTypeLxsy = 2.U(3.W)
  val storeSetUpdateTypeLysyMerge = 3.U(3.W)
  val storeSetUpdateTypeSameSsidStrict = 4.U(3.W)

  val storeSetUpdateType = Wire(UInt(3.W))
  storeSetUpdateType := MuxCase(storeSetUpdateTypeLxsx, Seq(
    (s2_loadAssigned && !s2_storeAssigned) -> storeSetUpdateTypeLysx,
    (!s2_loadAssigned && s2_storeAssigned) -> storeSetUpdateTypeLxsy,
    (s2_loadAssigned && s2_storeAssigned && !s2_ssidIsSame) -> storeSetUpdateTypeLysyMerge,
    (s2_loadAssigned && s2_storeAssigned && s2_ssidIsSame) -> storeSetUpdateTypeSameSsidStrict
  ))

  val storeSetNewLoadSSID = MuxCase(s2_allocSsid, Seq(
    (s2_loadAssigned && !s2_storeAssigned) -> s2_loadOldSSID,
    (!s2_loadAssigned && s2_storeAssigned) -> s2_storeOldSSID,
    (s2_loadAssigned && s2_storeAssigned) -> s2_winnerSSID
  ))
  val storeSetNewLoadStrict = MuxCase(false.B, Seq(
    (s2_loadAssigned && !s2_storeAssigned) -> s2_loadStrict,
    (s2_loadAssigned && s2_storeAssigned && s2_ssidIsSame) -> true.B
  ))

  val storeSetUpdateEntry = Wire(new StoreSetUpdateDBEntry)
  storeSetUpdateEntry.timeCnt := GTimer()
  storeSetUpdateEntry.ldFoldPc := s2_mempred_update_req.ldpc
  storeSetUpdateEntry.stFoldPc := s2_mempred_update_req.stpc
  storeSetUpdateEntry.loadOldSSID := s2_loadOldSSID
  storeSetUpdateEntry.storeOldSSID := s2_storeOldSSID
  storeSetUpdateEntry.loadOldStrict := s2_loadStrict
  storeSetUpdateEntry.winnerSSID := s2_winnerSSID
  storeSetUpdateEntry.newLoadSSID := storeSetNewLoadSSID
  storeSetUpdateEntry.newLoadStrict := storeSetNewLoadStrict
  storeSetUpdateEntry.updateType := storeSetUpdateType
  storeSetUpdateTable.log(
    data = storeSetUpdateEntry,
    en = s2_mempred_update_req_valid,
    site = s"SSIT$storeSetUpdateHartId",
    clock = clock,
    reset = reset
  )

  XSPerfAccumulate("ssit_update_lxsx", s2_mempred_update_req_valid && !s2_loadAssigned && !s2_storeAssigned)
  XSPerfAccumulate("ssit_update_lysx", s2_mempred_update_req_valid && s2_loadAssigned && !s2_storeAssigned)
  XSPerfAccumulate("ssit_update_lxsy", s2_mempred_update_req_valid && !s2_loadAssigned && s2_storeAssigned)
  XSPerfAccumulate("ssit_update_lysy", s2_mempred_update_req_valid && s2_loadAssigned && s2_storeAssigned)
  XSPerfAccumulate("ssit_update_should_strict", s2_mempred_update_req_valid && s2_ssidIsSame && s2_loadAssigned && s2_storeAssigned)
  XSPerfAccumulate("ssit_update_strict_failed",
    s2_mempred_update_req_valid && s2_ssidIsSame && s2_loadStrict && s2_loadAssigned && s2_storeAssigned
  ) // should be zero
  XSPerfAccumulate("ssit_strict_clear", strictClearTrigger)

  val pred_dependence = io.ren.zip(io.rdata).map{case (v, rdata) =>
    RegNext(v) && rdata.valid
  }
  val pred_dependence_strict = io.ren.zip(io.rdata).map{case (v, rdata) =>
    RegNext(v) && rdata.valid && rdata.strict
  }

  XSPerfAccumulate("ssit_pred_dependence", PopCount(pred_dependence))
  XSPerfAccumulate("ssit_pred_strict", PopCount(pred_dependence_strict))

  // debug
  XSDebug(s2_mempred_update_req.valid, "%d: SSIT update: load pc %x store pc %x\n", GTimer(), s2_mempred_update_req.ldpc, s2_mempred_update_req.stpc)
  XSDebug(s2_mempred_update_req.valid, "%d: SSIT update: load valid %b ssid %x  store valid %b ssid %x\n", GTimer(), s2_loadAssigned, s2_loadOldSSID, s2_storeAssigned, s2_storeOldSSID)
}


// Last Fetched Store Table Entry
class LFSTEntry(implicit p: Parameters) extends XSBundle  {
  val valid = Bool()
  val sqIdx = new SqPtr
}

class LFSTReq(implicit p: Parameters) extends XSBundle {
  val isstore = Bool()
  val ssid = UInt(SSIDWidth.W) // use ssid to lookup LFST
  val sqIdx = new SqPtr
  val perfStrictPred = Bool()
}

class LFSTResp(implicit p: Parameters) extends XSBundle {
  val shouldWait = Bool()
  val strictShouldWait = Bool()
  val sqIdx = new SqPtr
  val perfNotIssuedStoreGt1 = Bool()
}

class DispatchLFSTIO(implicit p: Parameters) extends XSBundle {
  val req = Vec(RenameWidth, Valid(new LFSTReq))
  val resp = Vec(RenameWidth, Flipped(Valid(new LFSTResp)))
}

// Last Fetched Store Table
class LFST(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    // The pointer is the first free SQ entry after redirect recovery.
    val sqRedirectPtr = Input(Valid(new SqPtr))
    val dispatch = Flipped(new DispatchLFSTIO)
    // when store issued, mark store as invalid
    val storeIssue = Vec(backendParams.StaExuCnt, Flipped(Valid(new StoreUnitToLFST)))
    val csrCtrl = Input(new CustomCSRCtrlIO)
  })

  val validVec = RegInit(VecInit(Seq.fill(LFSTSize)(VecInit(Seq.fill(LFSTWidth)(false.B)))))
  val sqIdxVec = Reg(Vec(LFSTSize, Vec(LFSTWidth, new SqPtr)))
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
    val respSsid = io.dispatch.req(i).bits.ssid
    io.dispatch.resp(i).bits.sqIdx := sqIdxVec(respSsid)(allocPtr(respSsid) - 1.U)
    // A younger store may issue before an older one. Select the newest slot that is still valid.
    (0 until LFSTWidth).reverse.foreach { j =>
      val candidate = (allocPtr(respSsid) - (j + 1).U)(log2Up(LFSTWidth) - 1, 0)
      when(validVec(respSsid)(candidate)) {
        io.dispatch.resp(i).bits.sqIdx := sqIdxVec(respSsid)(candidate)
      }
    }
    if(i > 0){
      (0 until i).map(j =>
        when(hitInDispatchBundleVec(j)){
          io.dispatch.resp(i).bits.sqIdx := io.dispatch.req(j).bits.sqIdx
        }
      )
    }

    // Older stores in the same dispatch bundle become LFST entries in this cycle.
    val notIssuedStoreCount = PopCount(validVec(io.dispatch.req(i).bits.ssid)) + PopCount(hitInDispatchBundleVec)
    val notIssuedStoreGt1 = notIssuedStoreCount > 1.U
    io.dispatch.resp(i).bits.perfNotIssuedStoreGt1 := notIssuedStoreGt1
    io.dispatch.resp(i).bits.strictShouldWait := io.dispatch.req(i).valid &&
      !io.dispatch.req(i).bits.isstore && notIssuedStoreGt1
  }

  // when store is issued, mark it as invalid
  (0 until backendParams.StaExuCnt).map(i => {
    // TODO: opt timing
    (0 until LFSTWidth).map(j => {
      when(io.storeIssue(i).valid && io.storeIssue(i).bits.storeSetHit &&
        io.storeIssue(i).bits.sqIdx === sqIdxVec(io.storeIssue(i).bits.ssid)(j)) {
        validVec(io.storeIssue(i).bits.ssid)(j) := false.B
      }
    })
  })

  val overflowVec = WireInit(VecInit(Seq.fill(RenameWidth)(false.B)))
  // when store is dispatched, mark it as valid
  (0 until RenameWidth).map(i => {
    when(io.dispatch.req(i).valid && io.dispatch.req(i).bits.isstore){
      val waddr = io.dispatch.req(i).bits.ssid
      val olderSameSsidStores = if (i == 0) 0.U else PopCount((0 until i).map { j =>
        io.dispatch.req(j).valid && io.dispatch.req(j).bits.isstore &&
          io.dispatch.req(j).bits.ssid === waddr
      })
      val wptr = (allocPtr(waddr) + olderSameSsidStores)(log2Up(LFSTWidth) - 1, 0)
      allocPtr(waddr) := wptr + 1.U
      validVec(waddr)(wptr) := true.B
      sqIdxVec(waddr)(wptr) := io.dispatch.req(i).bits.sqIdx
      when(validVec(waddr)(wptr)) {
        overflowVec(i) := true.B
      }
    }
  })

  // Cancel stores at or after the recovered SQ tail.
  (0 until LFSTSize).map(i => {
    (0 until LFSTWidth).map(j => {
      when(validVec(i)(j) && sqIdxVec(i)(j).needFlush(io.sqRedirectPtr)) {
        validVec(i)(j) := false.B
      }
    })
  })

  // Repair allocation pointers after squash entries have been cleared.
  // behavior model, to be refactored later
  when(RegNext(io.sqRedirectPtr.fire)) {
    (0 until LFSTSize).map(i => {
      (0 until LFSTWidth).map(j => {
        val check_position = WireInit(allocPtr(i) + (j+1).U)
        when(!validVec(i)(check_position)){
          allocPtr(i) := check_position
        }
      })
    })
  }

  XSPerfAccumulate("LFST_Overflow_Count", PopCount(overflowVec))
  XSPerfAccumulate("lfst_strict_pred_not_issued_store_greater1", PopCount(io.dispatch.resp.zip(io.dispatch.req).map {
    case (resp, req) => resp.valid && req.bits.perfStrictPred && resp.bits.perfNotIssuedStoreGt1
  }))
  XSPerfAccumulate("lfst_strict_pred_filtered", PopCount(io.dispatch.resp.zip(io.dispatch.req).map {
    case (resp, req) => resp.valid && !req.bits.isstore && req.bits.perfStrictPred &&
      resp.bits.shouldWait && !resp.bits.strictShouldWait
  }))
}
