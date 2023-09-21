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

package device

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.ExtModule
import chisel3.util._
import freechips.rocketchip.diplomacy.AddressSet
import utility._

trait HasSDConst {
  def MemorySize = 4L * 1024 * 1024 * 1024 // 4GB
  def READ_BL_LEN = 15

  def BlockLen = (1 << READ_BL_LEN)

  def NrBlock = MemorySize / BlockLen

  def C_SIZE_MULT = 7 // only 3 bits
  def MULT = (1 << (C_SIZE_MULT + 2))

  def C_SIZE = NrBlock / MULT - 1
}

class SDHelper extends ExtModule with HasExtModuleInline {
  val clk = IO(Input(Clock()))
  val ren = IO(Input(Bool()))
  val data = IO(Output(UInt(32.W)))
  val setAddr = IO(Input(Bool()))
  val addr = IO(Input(UInt(32.W)))

  setInline("SDHelper.v",
    s"""
       |import "DPI-C" function void sd_setaddr(input int addr);
       |import "DPI-C" function void sd_read(output int data);
       |
       |module SDHelper (
       |  input clk,
       |  input setAddr,
       |  input [31:0] addr,
       |  input ren,
       |  output reg [31:0] data
       |);
       |
       |  always @(negedge clk) begin
       |    if (ren) sd_read(data);
       |  end
       |  always@(posedge clk) begin
       |    if (setAddr) sd_setaddr(addr);
       |  end
       |
       |endmodule
     """.stripMargin)
}

class AXI4DummySD
(
  address: Seq[AddressSet]
)(implicit p: Parameters)
  extends AXI4SlaveModule(address, executable = false) with HasSDConst
{
  override lazy val module = new AXI4SlaveModuleImp[Null](this) {
    val range = List.range(0, 21)
    val sdcmd :: sdarg :: sdtout :: sdcdiv :: sdrsp0 :: sdrsp1 :: sdrsp2 :: sdrsp3 :: sdhsts :: __pad0 :: __pad1 :: __pad2 :: sdvdd :: sdedm :: sdhcfg :: sdhbct :: sddata :: __pad10 :: __pad11 :: __pad12 :: sdhblc :: Nil = range

    val regs = List.fill(range.size)(RegInit(0.U(32.W)))
    val edmConst = (8 << 4).U(32.W) // number of data in fifo

    val MMC_SEND_OP_COND = 1
    val MMC_ALL_SEND_CID = 2
    val MMC_SEND_CSD = 9
    val MMC_SEND_STATUS = 13
    val MMC_READ_MULTIPLE_BLOCK = 18

    val setAddr = WireInit(false.B)

    def cmdWfn(wdata: UInt) = {
      val cmd = wdata(5, 0)
      switch(cmd) {
        is(MMC_SEND_OP_COND.U) {
          regs(sdrsp0) := "h80ff8000".U
        }
        is(MMC_ALL_SEND_CID.U) {
          regs(sdrsp0) := "h00000001".U
          regs(sdrsp1) := "h00000000".U
          regs(sdrsp2) := "h00000000".U
          regs(sdrsp3) := "h15000000".U
        }
        is(MMC_SEND_CSD.U) {
          regs(sdrsp0) := "h92404001".U
          regs(sdrsp1) := "h124b97e3".U | (C_SIZE.U(1, 0) << 30)
          regs(sdrsp2) := "h0f508000".U | C_SIZE.U(11, 2) | (READ_BL_LEN.U << 16)
          regs(sdrsp3) := "h8c26012a".U
        }
        is(MMC_SEND_STATUS.U) {
          regs(sdrsp0) := 0.U
          regs(sdrsp1) := 0.U
          regs(sdrsp2) := 0.U
          regs(sdrsp3) := 0.U
        }
        is(MMC_READ_MULTIPLE_BLOCK.U) {
          setAddr := true.B
        }
      }
      wdata
    }

    val sdHelper = Module(new SDHelper)
    sdHelper.clk := clock
    sdHelper.ren := (getOffset(raddr) === 0x40.U && in.ar.fire)
    sdHelper.setAddr := setAddr
    sdHelper.addr := regs(sdarg)

    def sdRead = sdHelper.data

    val mapping = Map(
      RegMap(0x00, regs(sdcmd), cmdWfn),
      RegMap(0x04, regs(sdarg)),
      RegMap(0x10, regs(sdrsp0), RegMap.Unwritable),
      RegMap(0x14, regs(sdrsp1), RegMap.Unwritable),
      RegMap(0x18, regs(sdrsp2), RegMap.Unwritable),
      RegMap(0x1c, regs(sdrsp3), RegMap.Unwritable),
      RegMap(0x20, regs(sdhsts)),
      RegMap(0x34, edmConst, RegMap.Unwritable),
      RegMap(0x38, regs(sdhcfg)),
      RegMap(0x38, regs(sdhbct)),
      RegMap(0x40, sdRead, RegMap.Unwritable),
      RegMap(0x50, regs(sdhblc))
    )

    def getOffset(addr: UInt) = addr(12, 0)

    val strb = Mux(waddr(2), in.w.bits.strb(7, 4), in.w.bits.strb(3, 0))
    val rdata = Wire(UInt(32.W))
    RegMap.generate(mapping, getOffset(raddr), rdata,
      getOffset(waddr), in.w.fire, in.w.bits.data(31, 0), MaskExpand(strb))

    in.r.bits.data := Fill(2, rdata)
  }
}
