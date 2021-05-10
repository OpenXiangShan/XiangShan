package xiangshan.backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._


class BypassInfo(numWays: Int, dataBits: Int) extends Bundle {
  val valid = Vec(numWays, Bool())
  val data = UInt(dataBits.W)

  override def cloneType: BypassInfo.this.type =
    new BypassInfo(numWays, dataBits).asInstanceOf[this.type]
}

class BypassNetworkIO(numWays: Int, numBypass: Int, dataBits: Int) extends Bundle {
  val hold = Input(Bool())
  val source = Vec(numWays, Input(UInt(dataBits.W)))
  val target = Vec(numWays, Output(UInt(dataBits.W)))
  val bypass = Vec(numBypass, Input(new BypassInfo(numWays, dataBits)))

  override def cloneType: BypassNetworkIO.this.type =
    new BypassNetworkIO(numWays, numBypass, dataBits).asInstanceOf[this.type]
}

class BypassNetwork(numWays: Int, numBypass: Int, dataBits: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new BypassNetworkIO(numWays, numBypass, dataBits))

  val target_reg = Reg(Vec(numWays, UInt(dataBits.W)))
  val bypass_reg = Reg(Vec(numBypass, new BypassInfo(numWays, dataBits)))

  // TODO: duplicate bypass mask to avoid FO4 or bypass at the previous cycle
  when (io.hold) {
    target_reg := io.target
    bypass_reg.map(_.valid.map(_ := false.B))
  }.otherwise {
    target_reg := io.source
    bypass_reg := io.bypass
  }

  // bypass data to target
  for (i <- 0 until numWays) {
    val mask = VecInit(bypass_reg.map(_.valid(i)))
    io.target(i) := Mux(mask.asUInt.orR, Mux1H(mask, bypass_reg.map(_.data)), target_reg(i))

    XSError(PopCount(mask) > 1.U, p"bypass mask ${Binary(mask.asUInt)} is not one-hot\n")
    mask.zipWithIndex.map { case (m, j) =>
      XSDebug(mask(j), p"target($i) bypassed from $j:0x${Hexadecimal(bypass_reg(j).data)}\n")
    }
  }
}
