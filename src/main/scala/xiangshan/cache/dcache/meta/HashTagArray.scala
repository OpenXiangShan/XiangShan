package xiangshan.cache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

class HashTagArray(readPorts: Int, hashBits: Int = 4)(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle {
    val read = Vec(readPorts, Flipped(DecoupledIO(new TagReadReq)))
    val resp = Output(Vec(readPorts, Vec(nWays, UInt(hashBits.W))))
    val write = Flipped(DecoupledIO(new TagWriteReq))
  })

  val table = Reg(Vec(nSets, Vec(nWays, UInt(hashBits.W))))

  private def foldXorHash(tag: UInt): UInt = {
    val nChunks = (tagBits + hashBits - 1) / hashBits
    val paddedBits = nChunks * hashBits
    val paddedTag = if (paddedBits == tagBits) tag else Cat(0.U((paddedBits - tagBits).W), tag)
    (0 until nChunks).map { i =>
      paddedTag((i + 1) * hashBits - 1, i * hashBits)
    }.reduce(_ ^ _)
  }

  for (i <- 0 until readPorts) {
    io.read(i).ready := true.B
    io.resp(i) := RegEnable(table(io.read(i).bits.idx), io.read(i).fire)
  }

  io.write.ready := true.B
  when (io.write.fire) {
    val hashedTag = foldXorHash(io.write.bits.tag)
    for (w <- 0 until nWays) {
      when (io.write.bits.way_en(w)) {
        table(io.write.bits.idx)(w) := hashedTag
      }
    }
  }
}
