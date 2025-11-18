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
// import org.scalatest._
// import org.scalatest.flatspec.AnyFlatSpec
// import org.scalatest.matchers.must.Matchers
// import xiangshan.testutils._


// class uBTBTest extends AnyFlatSpec
// with ChiselScalatestTester 
// with Matchers 
// with ParallelTestExecution
// with HasPartialDecoupledDriver {
//   it should "test uBTBTest" in {
//     test(new MicroBTB) { c =>
//         def genUpdateReq(pc: Long,target: Long,taken: Boolean,fetchIdx: Int,isMiss: Boolean,write_way: Int,hit: Boolean) = {
//           c.io.update.valid.poke(true.B)
//           c.io.update.bits.pc.poke(pc.U)
//           c.io.update.bits.target.poke(target.U)
//           c.io.update.bits.taken.poke(taken.B)
//           c.io.update.bits.fetchIdx.poke(fetchIdx.U)
//           c.io.update.bits.isMisPred.poke(isMiss.B)
//           c.io.update.bits.bpuMeta.ubtbWriteWay.poke(write_way.U)
//           c.io.update.bits.bpuMeta.ubtbHits.poke(hit.B)
//           c.io.update.bits.pd.brType.poke(BrType.branch)
//         }

//         def genReadReq(fetchpc: Long){
//           c.io.pc.valid.poke(true.B)
//           c.io.pc.bits.poke(fetchpc.U)
//           c.io.inMask.poke("b1111111111111111".U)
//           c.clock.step(1)
//           c.io.pc.valid.poke(false.B)
//         }

//         def UpdateOnly(pc: Long,target: Long,taken: Boolean,fetchIdx: Int,isMiss: Boolean,write_way: Int,hit: Boolean){
//           genUpdateReq(pc,target,taken,fetchIdx,isMiss,write_way,hit)
//           c.clock.step(1)
//           c.io.update.valid.poke(false.B)
//         }

//         def Bypass(pc: Long,target: Long,taken: Boolean,fetchIdx: Int,isMiss: Boolean,write_way: Int,hit: Boolean){
//           genUpdateReq(pc,target,taken,fetchIdx,isMiss,write_way,hit)
//           genReadReq(fetchpc = pc + 2)
//           c.clock.step(1)
//           c.io.update.valid.poke(false.B)
//           c.io.pc.valid.poke(false.B)
//         }
//         genReadReq(fetchpc = 0x60002010)
//         UpdateOnly(pc=0x6000202a, target=0x60001000, taken = true , fetchIdx=6, isMiss = true , write_way=2, hit=false)
//         genReadReq(fetchpc = 0x60002010)
//         //Bypass(pc=0x60002034, target=0x600020b0, taken = true , fetchIdx=5, isMiss = true , write_way=5, hit=false)
    
//     }
//   }
// }
