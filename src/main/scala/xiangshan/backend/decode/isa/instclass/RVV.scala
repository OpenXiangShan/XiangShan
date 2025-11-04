package xiangshan.backend.decode.isa.instclass

import freechips.rocketchip.rocket.Instructions._
import xiangshan.macros.InstanceNameMacro.getVariableNameSeq

object RVV {
  val vvw: Seq[String] = getVariableNameSeq(
    VWADD_VV,
    VWADD_VX,
    VWADDU_VV,
    VWADDU_VX,
    VWSUB_VV,
    VWSUB_VX,
    VWSUBU_VV,
    VWSUBU_VX,
    VWMUL_VV,
    VWMUL_VX,
    VWMULSU_VV,
    VWMULSU_VX,
  )

  val wvw = getVariableNameSeq(
    VWADD_WV,
    VWADD_WX,
    VWADDU_WV,
    VWADDU_WX,
    VWSUB_WV,
    VWSUB_WX,
    VWSUBU_WV,
    VWSUBU_WX,
  )

  val wredo = getVariableNameSeq(
    VFWREDOSUM_VS
  )
}
