package xiangshan.backend.fu.vector

import chisel3._

object Utils {
  def VecDataToMaskDataVec(vecData: UInt): Vec[UInt] = {
    val maskDataWidth = vecData.getWidth / 8
    val maskDataVec = Wire(Vec(8, UInt(maskDataWidth.W)))
    require(8 * maskDataWidth == vecData.getWidth, "can not split this vector data into mask data vec")
    for ((maskData, i) <- maskDataVec.zipWithIndex) {
      maskData := vecData((i + 1) * maskDataWidth - 1, i * maskDataWidth)
    }
    maskDataVec
  }
}
