package xiangshan.cache

import freechips.rocketchip.tilelink.ClientMetadata
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import system.L1CacheErrorInfo

class Meta(implicit p: Parameters) extends DCacheBundle {
  val coh = new ClientMetadata
}

object Meta {
  def apply(meta: UInt)(implicit p: Parameters) = {
    val m = Wire(new Meta)
    m.coh := meta.asTypeOf(new ClientMetadata)
    m
  }
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

class AsynchronousMetaArray(readPorts: Int, writePorts: Int)(implicit p: Parameters) extends DCacheModule {
  def metaAndTagOnReset = MetaAndTag(ClientMetadata.onReset, 0.U)

  // enc bits encode both tag and meta, but is saved in meta array
  val metaAndTagBits = metaAndTagOnReset.getWidth
  val encMetaAndTagBits = cacheParams.tagCode.width(metaAndTagBits)
  val encMetaBits = encMetaAndTagBits - tagBits

  val io = IO(new Bundle() {
    // TODO: this is made of regs, so we don't need to use DecoupledIO
    val read = Vec(readPorts, Flipped(DecoupledIO(new MetaReadReq)))
    val resp = Output(Vec(readPorts, Vec(nWays, UInt(encMetaBits.W))))
    val write = Vec(writePorts, Flipped(DecoupledIO(new MetaWriteReq)))
    val errors = Output(Vec(readPorts, new L1CacheErrorInfo))
  })
  val array = VecInit(Seq.fill(nSets)(
    VecInit(Seq.fill(nWays)(
      RegInit(0.U(encMetaBits.W))))
  ))

  io.read.zip(io.resp).foreach {
    case (read, resp) =>
      read.ready := true.B
      resp := RegEnable(array(read.bits.idx), read.valid)
  }
  io.write.foreach {
    case write =>
      write.ready := true.B
      val ecc = cacheParams.tagCode.encode(MetaAndTag(write.bits.meta.coh, write.bits.tag).asUInt)(encMetaAndTagBits - 1, metaAndTagBits)
      val encMeta = Cat(ecc, write.bits.meta.asUInt)
      require(encMeta.getWidth == encMetaBits)
      write.bits.way_en.asBools.zipWithIndex.foreach {
        case (wen, i) =>
          when (write.valid && wen) {
            array(write.bits.idx)(i) := encMeta
          }
      }
  }
  // TODO
  io.errors.foreach {
    case error =>
      error := DontCare
      error.ecc_error.valid := false.B
  }
}
