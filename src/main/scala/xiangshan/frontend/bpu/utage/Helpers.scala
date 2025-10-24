package xiangshan.frontend.bpu.utage

import chisel3._
import chisel3.util._
import xiangshan.HasXSParameter
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.FoldedHistoryInfo
import xiangshan.frontend.bpu.RotateHelper
import xiangshan.frontend.bpu.phr.PhrAllFoldedHistories

trait Helpers extends HasMicroTageParameters with RotateHelper {}
