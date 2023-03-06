///***************************************************************************************
//* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
//* Copyright (c) 2020-2021 Peng Cheng Laboratory
//*
//* XiangShan is licensed under Mulan PSL v2.
//* You can use this software according to the terms and conditions of the Mulan PSL v2.
//* You may obtain a copy of Mulan PSL v2 at:
//*          http://license.coscl.org.cn/MulanPSL2
//*
//* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
//* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
//* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//*
//* See the Mulan PSL v2 for more details.
//***************************************************************************************/
//
//package xiangshan.backend.issue
//
//import chipsalliance.rocketchip.config.Parameters
//import chisel3._
//import chisel3.util._
//import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
//import xiangshan._
//
//case class JumpRSParams()
//
//class JumpRSWrapper(modGen: RSMod)(implicit p: Parameters) extends BaseReservationStationWrapper(modGen) {
//  params.numSrc = 2
//  params.exuCfg = Some(JumpCSRExeUnitCfg)
//  override lazy val module = new JumpRSImp(params, this)
//}
//
//class JumpRSImp(params: RSParams, wrapper: JumpRSWrapper) extends BaseReservationStationImp(params, wrapper) {
//  rs.foreach(_.extra.jump <> extra.jump)
//}
//
//class JumpRS(params: RSParams)(implicit p: Parameters) extends BaseReservationStation(params) {
//
//
//  // special case for jump's pc
//  val pcMem = Reg(Vec(params.numEntries, UInt(VAddrBits.W)))
//  val jalrMem = Reg(Vec(params.numEntries, UInt(VAddrBits.W)))
//  for (i <- 0 until params.numEntries) {
//    val writeEn = VecInit(dataArrayWrite.map(w => w.enable && w.addr(i))).asUInt.orR
//    when (writeEn) {
//      pcMem(i) := extra.jump.jumpPc
//      jalrMem(i) := extra.jump.jalr_target
//    }
//  }
//  for (i <- 0 until params.numDeq) {
//    // currently we assert there's only one enqueue.
//    require(params.numDeq == 1, "only one jump now")
//    val oldestPc = Mux1H(s1_in_oldestPtrOH.bits, pcMem)
//    val issuePc = Mux1H(s1_in_selectPtrOH(i), pcMem)
//    val pcRead = Mux(s1_issue_oldest(i), oldestPc, issuePc)
//    val pcBypass = Mux(s1_select_bypass_s0.asUInt.orR, extra.jump.jumpPc, pcRead)
//    io.deq(i).bits.uop.cf.pc := RegEnable(pcBypass, s1_out_fire(i))
//    immExts(i).jump_pc := pcBypass
//  }
//  for (i <- 0 until params.numDeq) {
//    require(params.numDeq == 1, "only one jump now")
//    val oldestJalr = Mux1H(s1_in_oldestPtrOH.bits, jalrMem)
//    val issueJalr = Mux1H(s1_in_selectPtrOH(i), jalrMem)
//    val jalrRead = Mux(s1_issue_oldest(i), oldestJalr, issueJalr)
//    val jalrBypass = Mux(s1_select_bypass_s0.asUInt.orR, extra.jump.jalr_target, jalrRead)
//    immExts(i).jalr_target := jalrBypass
//  }
//}