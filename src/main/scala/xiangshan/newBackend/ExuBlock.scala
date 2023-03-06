///***************************************************************************************
//  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
//  * Copyright (c) 2020-2021 Peng Cheng Laboratory
//  *
//  * XiangShan is licensed under Mulan PSL v2.
//  * You can use this software according to the terms and conditions of the Mulan PSL v2.
//  * You may obtain a copy of Mulan PSL v2 at:
//  *          http://license.coscl.org.cn/MulanPSL2
//  *
//  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
//  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
//  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//  *
//  * See the Mulan PSL v2 for more details.
//  ***************************************************************************************/
//
//package xiangshan.newBackend
//
//import chipsalliance.rocketchip.config.Parameters
//import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
//import chisel3._
//import chisel3.util._
//import xiangshan._
//
//class IntExuBlock(implicit p: Parameters) extends LazyModule {
//  lazy val module = new IntExuBlockImpl(this)
//}
//
//class IntExuBlockImpl(outer: IntExuBlock)(implicit p: Parameters) extends LazyModuleImp(outer)
//  with HasXSParameter
//{
//  val fromTop = new Bundle {
//    val hartId = Input(UInt(8.W))
//  }
//  val fromRob = new Bundle {
//    val redirect = Flipped(ValidIO(new Redirect))
//  }
//  val fromDispatch = new Bundle {
//    val allocPregs = Vec(RenameWidth, Input(new ResetPregStateReq()))
//    val in = scheduler.io.in.cloneType
//  }
//}
