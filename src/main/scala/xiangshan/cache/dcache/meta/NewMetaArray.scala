package xiangshan.cache

import freechips.rocketchip.tilelink.ClientMetadata
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import system.L1CacheErrorInfo

class Meta(implicit p: Parameters) extends DCacheBundle {
  val coh = new ClientMetadata
}

class MetaAndTag(implicit p: Parameters) extends DCacheBundle {
  val meta = new Meta
  val tag = UInt(tagBits.W)
}

object MetaAndTag {
  def apply(coh: ClientMetadata, tag: UInt)(implicit p: Parameters) = {
    val x = Wire(new MetaAndTag)
    x.meta.coh := coh
    x.tag := tag
    x
  }
}

class MetaReadReq(implicit p: Parameters) extends DCacheBundle {
  val idx = UInt(idxBits.W)
  val way_en = UInt(nWays.W)
}

class MetaWriteReq(implicit p: Parameters) extends MetaReadReq {
  val meta = new Meta
  val tag = UInt(tagBits.W) // used to calculate ecc
}

class NewMetaArray(readPorts: Int, writePorts: Int)(implicit p: Parameters) extends DCacheModule {
  def metaAndTagOnReset = MetaAndTag(ClientMetadata.onReset, 0.U)

  // enc bits encode both tag and meta, but is saved in meta array
  val encMetaBits = cacheParams.tagCode.width(metaAndTagOnReset.getWidth) - tagBits

  val io = IO(new Bundle() {
    // TODO: this is made of regs, so we don't need to use DecoupledIO
    val read = Vec(readPorts, Flipped(DecoupledIO(new MetaReadReq)))
    val resp = Output(Vec(readPorts, Vec(nWays, UInt(encMetaBits.W))))
    val write = Vec(writePorts, Flipped(DecoupledIO(new MetaWriteReq)))
    val errors = Output(Vec(readPorts, new L1CacheErrorInfo))
  })
  // TODO
}
