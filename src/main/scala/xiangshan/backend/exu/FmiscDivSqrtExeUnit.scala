package xiangshan.backend.exu

import chisel3._
import xiangshan.backend.exu.Exu.fmiscDivExeUnitCfg

class FmiscDivSqrtExeUnit extends Exu(fmiscDivExeUnitCfg){
  io <> DontCare
}
