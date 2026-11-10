package xiangshan.backend.vector.Decoder.util

import chisel3.util.BitPat
import chisel3.util.experimental.decode.DecodePattern
import xiangshan.backend.vector.Decoder.RVVDecodeUtil.DecodePatternComb2

/**
 * Input pattern a DecoderField should match, e.g. an instruction
 */
trait DecodePattern {
  def bitPat: BitPat
}
