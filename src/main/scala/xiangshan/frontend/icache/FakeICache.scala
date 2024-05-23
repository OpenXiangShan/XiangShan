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

package xiangshan.frontend.icache

import chisel3._
import chisel3.util._
import chisel3.experimental.ExtModule
import org.chipsalliance.cde.config.Parameters
import xiangshan.XSModule

class TraceICacheHelper extends ExtModule {
  val clock = IO(Input(Clock()))
  val enable = IO(Input(Bool()))
  val addr = IO(Input(UInt(64.W)))
  val data0 = IO(Output(UInt(64.W)))
  val data1 = IO(Output(UInt(64.W)))
  val data2 = IO(Output(UInt(64.W)))
  val data3 = IO(Output(UInt(64.W)))
  val data4 = IO(Output(UInt(64.W)))
  val data5 = IO(Output(UInt(64.W)))
  val data6 = IO(Output(UInt(64.W)))
  val data7 = IO(Output(UInt(64.W)))
  val legal_addr = IO(Output(Bool()))
}

// Replace ICache's data
// IFU1 --fire--> IFU2
class FakeICache()(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val req = Flipped(ValidIO(new Bundle {
      val addr = UInt(VAddrBits.W)
    }))
    val resp = Valid(new Bundle {
      val data0 = UInt(256.W)
      val data1 = UInt(256.W)
      val addr = UInt(VAddrBits.W)
    })
  })

  val helper = Module(new TraceICacheHelper)
  helper.clock := clock
  helper.enable := io.req.valid
  helper.addr := io.req.bits.addr
  io.resp.valid := helper.legal_addr(0) && RegNext(io.req.valid)
  io.resp.bits.data0 := Cat(helper.data3, helper.data2, helper.data1, helper.data0)
  io.resp.bits.data1 := Cat(helper.data7, helper.data6, helper.data5, helper.data4)
  io.resp.bits.addr := RegEnable(helper.addr, io.req.valid)

  // when (RegNext(io.req.valid)) {
    // printf(p"FakeICache: addr = 0x${Hexadecimal(io.req.bits.addr)}\n")
    // printf(p"FakeICache: data0 = 0x${Hexadecimal(io.resp.bits.data0)}\n")
    // printf(p"FakeICache: data1 = 0x${Hexadecimal(io.resp.bits.data1)}\n")
  // }
}