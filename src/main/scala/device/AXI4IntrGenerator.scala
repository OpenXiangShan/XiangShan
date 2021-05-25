package device

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.AddressSet
import utils._

// we support 256 interrupt bits by default
class IntrGenIO extends Bundle {
  val intrVec = Output(UInt(256.W))
}

class AXI4IntrGenerator
(
  address: Seq[AddressSet]
)(implicit p: Parameters)
  extends AXI4SlaveModule(address, executable = false, _extra = new IntrGenIO)
{

  override lazy val module = new AXI4SlaveModuleImp(this){

    val intrReg = RegInit(VecInit(Seq.fill(8)(0.U(32.W))))
    io.extra.get.intrVec := Cat(intrReg.reverse)

    when (in.w.fire()) {
      intrReg(waddr(4, 2)) := in.w.bits.data(31, 0)
    }

    in.r.bits.data := intrReg(raddr)
  }
}
