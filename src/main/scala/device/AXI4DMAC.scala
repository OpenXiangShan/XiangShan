/***************************************************************************************
* Copyright (c) 2026 Beijing Institute of Open Source Chip (BOSC)
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

package device

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.util._
import utility._

/*      AXI4 DMAC
  only support 32Byte transfer for test
  register:
  0x00: srcAddrReg
  0x08: dstAddrReg
  0x10: cfgReg
 */
class AXI4DMAC
(
  address: Seq[AddressSet]
)(implicit p: Parameters)
  extends AXI4SlaveModule(address, executable = false)
{

  val masterNode = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    Seq(AXI4MasterParameters(
      name = "dmac_master",
      id = IdRange(0, 1 << 14),
      aligned = true
    ))
  )))
  
  override lazy val module = new AXI4SlaveModuleImp[Null](this) {

    // ****************** registers ******************//
    // registers
    val srcAddrReg = RegInit(0.U(64.W))
    val dstAddrReg = RegInit(0.U(64.W))
    val cfg_reg = RegInit(0.U(64.W))

    val mapping = Map(
      RegMap(0x00, srcAddrReg),
      RegMap(0x08, dstAddrReg),
      RegMap(0x10, cfg_reg   )
    )

    RegMap.generate(mapping, raddr(11,0), in.r.bits.data,
      waddr(11,0), in.w.fire, in.w.bits.data, MaskExpand(in.w.bits.strb))

    // dly start
    val start_reg_dly = RegNext(cfg_reg(0))
   
    // 4k Byte Fifo
    val fifo = Module(new Queue(UInt(256.W), 128))

    // ****************** dma engine ******************//
    val (masterBundle, masterEdge) = masterNode.out.head

    //fsm
    object State_axi extends ChiselEnum {
      val sIdle, sAr, sRdata, sAw, sWdata, sB = Value
    }
    val state_axi = RegInit(State_axi.sIdle)

    val axi_start = cfg_reg(0) && !start_reg_dly
    val axi_ar_ok = masterBundle.ar.fire
    val axi_r_ok  = masterBundle.r.fire && masterBundle.r.bits.last
    val axi_aw_ok = masterBundle.aw.fire
    val axi_w_ok  = masterBundle.w.fire && masterBundle.w.bits.last
    val axi_b_ok  = masterBundle.b.fire

    switch(state_axi){
      is(State_axi.sIdle ){when(axi_start){state_axi := State_axi.sAr   }}
      is(State_axi.sAr   ){when(axi_ar_ok){state_axi := State_axi.sRdata}}
      is(State_axi.sRdata){when(axi_r_ok ){state_axi := State_axi.sAw   }}
      is(State_axi.sAw   ){when(axi_aw_ok){state_axi := State_axi.sWdata}}
      is(State_axi.sWdata){when(axi_w_ok ){state_axi := State_axi.sB    }}
      is(State_axi.sB    ){when(axi_b_ok ){state_axi := State_axi.sIdle }}
    }

    // ar ch
    masterBundle.ar.valid := (state_axi === State_axi.sAr)
    masterBundle.ar.bits.addr := srcAddrReg(47,0)
    masterBundle.ar.bits.len := 0.U
    masterBundle.ar.bits.size := 5.U
    masterBundle.ar.bits.burst := 1.U
    masterBundle.ar.bits.id := 0.U

    // r ch
    masterBundle.r.ready := (state_axi === State_axi.sRdata) && fifo.io.enq.ready
    fifo.io.enq.valid := (state_axi === State_axi.sRdata) && masterBundle.r.valid
    fifo.io.enq.bits := masterBundle.r.bits.data

    // aw ch
    masterBundle.aw.valid := (state_axi === State_axi.sAw)
    masterBundle.aw.bits.addr := dstAddrReg(47,0)
    masterBundle.aw.bits.len := 0.U
    masterBundle.aw.bits.size := 5.U
    masterBundle.aw.bits.burst := 1.U
    masterBundle.aw.bits.id := 0.U

    // w ch
    fifo.io.deq.ready := (state_axi === State_axi.sWdata) && masterBundle.w.ready
    masterBundle.w.valid := (state_axi === State_axi.sWdata) && fifo.io.deq.valid
    masterBundle.w.bits.strb := Fill(256 / 8, true.B)
    masterBundle.w.bits.data := fifo.io.deq.bits
    masterBundle.w.bits.last := masterBundle.w.fire
    
    // b ch
    masterBundle.b.ready := (state_axi === State_axi.sB)

    // end_reg
    when(state_axi === State_axi.sB && axi_b_ok){
      cfg_reg := 0x2.U
    }
  }
}