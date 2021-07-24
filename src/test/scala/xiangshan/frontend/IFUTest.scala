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

// package xiangshan.frontend

// import chisel3._
// import chiseltest._
// import org.scalatest.flatspec.AnyFlatSpec
// import org.scalatest.matchers.must.Matchers

// class IFUTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
//   behavior of "IFU Test"

//   it should "test IFU pipeline" in {
//     test(new IFU) { c =>
//       //-----------------
//       //Cycle 0
//       //-----------------
//       //c.io.icacheReq.ready.poke(true.B)
//       c.io.icacheReq.ready.poke(false.B)
//       c.io.fetchPacket.ready.poke(true.B)
//       c.clock.step()
//       //-----------------
//       //Cycle 1
//       //-----------------
//       c.clock.step()
//       c.clock.step()
//       c.clock.step()
//       //-----------------
//       // Cycle 2
//       //-----------------
//       c.io.icacheReq.ready.poke(true.B)
//       c.clock.step()
//       //-----------------
//       // Cycle 3
//       //-----------------
//       c.clock.step()
//       //-----------------
//       // Cycle 4
//       //-----------------
//       c.io.icacheResp.valid.poke(true.B)
//       c.clock.step()
//       //-----------------
//       // Cycle 5
//       //-----------------
//       c.io.redirect.valid.poke(true.B)
//       c.io.redirect.bits.poke("h80002800".U)
//       c.clock.step()
//       //-----------------
//       // Cycle 6
//       //-----------------
//       c.io.redirect.valid.poke(false.B)
//       c.clock.step()
//       //-----------------
//       // Cycle 7
//       //-----------------
//       c.clock.step()
//     }
//   }
// }

