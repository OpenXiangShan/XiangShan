package device

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.AddressSet
import utils._

class AXI4Flash
(
  address: Seq[AddressSet]
)(implicit p: Parameters)
  extends AXI4SlaveModule(address, executable = false)
{

  override lazy val module = new AXI4SlaveModuleImp(this){
    val jmpToDramInstr1 = "h0010029b".U  // addiw t0,zero,1
    val jmpToDramInstr2 = "h01f29293".U  // slli  t0,t0,0x1f
    val jmpToDramInstr3 = "h00028067".U  // jr t0

    val mapping = Map(
      RegMap(0x0, jmpToDramInstr1, RegMap.Unwritable),
      RegMap(0x4, jmpToDramInstr2, RegMap.Unwritable),
      RegMap(0x8, jmpToDramInstr3, RegMap.Unwritable)
    )
    def getOffset(addr: UInt) = addr(12,0)

    val rdata = Wire(UInt(64.W))
    RegMap.generate(mapping, getOffset(raddr), rdata,
      getOffset(waddr), in.w.fire(), in.w.bits.data, MaskExpand(in.w.bits.strb))

    in.r.bits.data := Fill(2, rdata(31,0))
  }
}
