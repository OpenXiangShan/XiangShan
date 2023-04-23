package device.lvna

import chisel3._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import huancun.{DsidKey}

class TokenBucketNode(implicit p: Parameters) extends LazyModule {
  val node = TLIdentityNode()
  lazy val module = new TokenBucketNodeImp(this)
}

class TokenBucketNodeImp(outer: TokenBucketNode) extends LazyModuleImp(outer) with HasControlPlaneParameters {
  val (bundleIn, _) = outer.node.in.unzip
  val (bundleOut, _) = outer.node.out.unzip

  val bucketIO = IO(Flipped(new BucketIO()))

  // require(bundleIn.size == 1, s"[TokenBucket] Only expect one link for a hart, current link count is ${bundleIn.size}")
  val (in, out) = (bundleIn zip bundleOut).head

  val phy = in.a.bits.address < 0x80000000L.U //Dcache always send DRAM for now

  val inform_req = bucketIO.informFireReq
  inform_req.valid := out.a.fire && !phy
  val in_dsid = in.a.bits.user.lift(DsidKey).getOrElse((nDSID-1).U)
  inform_req.bits.dsid := in_dsid
  inform_req.bits.size := (1.U << in.a.bits.size) >> 6

  val update_enable_req = bucketIO.updateEnableReq

  val enable_regs = RegInit(VecInit(Seq.fill(nDSID){true.B}))
  when(update_enable_req.valid) {
    enable_regs(update_enable_req.bits.dsid) := update_enable_req.bits.setFlag
  }

  out.a.valid := in.a.valid && (phy || enable_regs(in_dsid))
  in.a.ready := out.a.ready && (phy || enable_regs(in_dsid))
  val DEBUG_TB_FETCH = false
  if (DEBUG_TB_FETCH) {
    when(in.a.valid && !out.a.valid) {
      printf(p"request blocked by token bucket: 0x${Hexadecimal(in.a.bits.address)}\n")
    }
    when(out.a.valid && !in.a.ready) {
      printf(p"response blocked by token bucket: 0x${Hexadecimal(in.a.bits.address)}\n")
    }
  }
}
