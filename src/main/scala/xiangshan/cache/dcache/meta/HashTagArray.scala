package xiangshan.cache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

object XORFoldTA { // for Hash Tag Array of DCache
  def apply(input: UInt, resWidth: Int): UInt = {
    val nChunks = 4
    require(input.getWidth >= nChunks * resWidth)
    (0 until nChunks).map { i =>
      input((i + 1) * resWidth - 1, i * resWidth)
    }.reduce(_ ^ _)
  }
}
class HashTagArray(readPorts: Int, hashBits: Int = 4)(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle {
    val read = Vec(readPorts, Flipped(ValidIO(new TagReadReq)))
    val resp = Output(Vec(readPorts, Vec(nWays, UInt(hashBits.W))))
    val write = Flipped(ValidIO(new TagWriteReq))
  })

  val table = Reg(Vec(nSets, Vec(nWays, UInt(hashBits.W)))) 

  for (i <- 0 until readPorts) {
    io.resp(i) := RegEnable(table(io.read(i).bits.idx), io.read(i).valid)
  }

  when (io.write.valid) {
    val hashedTag = XORFoldTA(io.write.bits.tag, hashBits)
    for (w <- 0 until nWays) {
      when (io.write.bits.way_en(w)) {
        table(io.write.bits.idx)(w) := hashedTag
      }
    }
  }
}
