package xiangshan.backend.fu.vector

import chisel3._
import chisel3.util.Mux1H
import xiangshan.backend.fu.vector.Bundles.VSew

object Utils {
  def VecDataToMaskDataVec(vecData: UInt, vsew: UInt): Vec[UInt] = {
    val maskWidth = vecData.getWidth / 8
    val maskDataVec = Wire(Vec(8, UInt(maskWidth.W)))
    require(8 * maskWidth == vecData.getWidth, "can not split this vector data into mask data vec")
    for ((maskData, i) <- maskDataVec.zipWithIndex) {
      maskData := Mux1H(Seq(
        (vsew === VSew.e8)  -> vecData((i + 1) * maskWidth     - 1, i * maskWidth    ),
        (vsew === VSew.e16) -> vecData((i + 1) * maskWidth / 2 - 1, i * maskWidth / 2),
        (vsew === VSew.e32) -> vecData((i + 1) * maskWidth / 4 - 1, i * maskWidth / 4),
        (vsew === VSew.e64) -> vecData((i + 1) * maskWidth / 8 - 1, i * maskWidth / 8),
      ))
    }
    maskDataVec
  }
}
