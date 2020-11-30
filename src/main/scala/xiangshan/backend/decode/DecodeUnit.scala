package xiangshan.backend.decode

import chisel3._
import chisel3.util._

import xiangshan._
import utils._
import xiangshan.backend._

// TODO Add lookup table here
/**
 * Abstract trait giving defaults and other relevant values to different Decode constants/
 */
abstract trait DecodeConstants {
  def decodeDefault: List[BitPat] = 
    // TODO set default value for ctrl signal
    List(???)
  val table: Array[(BitPat, List[BitPat])]
}

/**
 * Decoded control signals
 * See xiangshan/package.scala
 */

/**
 * Decode constants for RV64
 */
object X64Decode extends DecodeConstants {
  val table: Array[(BitPat, List[BitPat])] = Array()
}

/**
 * Overall Decode constants
 */
object XDecode extends DecodeConstants {
  val table: Array[(BitPat, List[BitPat])] = Array()
}

/**
 * FP Decode constants
 */
object FDecode extends DecodeConstants{
  val table: Array[(BitPat, List[BitPat])] = Array()
}

/**
 * FP Divide SquareRoot Constants
 */
object FDivSqrtDecode extends DecodeConstants {
  val table: Array[(BitPat, List[BitPat])] = Array()
}



// TODO Add BitPat of Instructions somewhere else

/**
 * IO bundle for the Decode unit
 */
class DecodeUnitIO extends XSBundle {
  val enq = new Bundle { val ctrl_flow = Input(new CtrlFlow) }
  val deq = new Bundle { val cf_ctrl = Output(new CfCtrl) }
}

/**
 * Decode unit that takes in a single CtrlFlow and generates a CfCtrl.
 */
class DecodeUnit extends XSModule {
  val io = IO(new DecodeUnitIO)

  val ctrl_flow = Wire(new CtrlFlow)
  ctrl_flow := io.enq.ctrl_flow

  var decode_table = ??? // TODO build table

  val inst = ctrl_flow.instr

  val cs = ??? // TODO add ctrl signals

  

  //-------------------------------------------------------------
  // Debug Info

  // TODO add more debug info
}
