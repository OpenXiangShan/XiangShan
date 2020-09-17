package xiangshan.backend.exu


import chisel3._
import xiangshan.backend.exu.Exu.fmiscExeUnitCfg

class FmiscExeUnit extends Exu(fmiscExeUnitCfg){
  io <> DontCare
}
