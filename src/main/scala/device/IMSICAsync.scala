/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2024 Institute of Computing Technology, Chinese Academy of Sciences
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package device

import chisel3._
import chisel3.util._

class IMSICAsync(
  NumVSIRFiles: Int = 5,
  NumHart: Int = 1,
  NumIRSrc: Int = 256,
) extends Module {
  private val NumIRFiles: Int = /*M*/ 1 + /*S*/ 1 + NumVSIRFiles
  private val NR_SRC_WIDTH = log2Up(NumIRSrc)
  private val NR_HARTS_WIDTH = log2Up(NumHart)
  private val INTP_FILE_WIDTH = log2Up(NumIRFiles)
  private val MSI_INFO_WIDTH = NR_HARTS_WIDTH + INTP_FILE_WIDTH + NR_SRC_WIDTH

  // has default clock and reset
  // input ports, ValidIO is an inner function
  val i = IO(Input(new Bundle {
    val msiInfo = ValidIO(new MsiInfoBundle(NumIRFiles = NumIRFiles, NumHart = NumHart, NumIRSrc = NumIRSrc))
  }))

  // output ports 
  val o = IO(Output(new Bundle {
    val msiInfo = ValidIO(new MsiInfoBundle(NumIRFiles = NumIRFiles, NumHart = NumHart, NumIRSrc = NumIRSrc))
  }))

  // code about msi_vld_sync, delay 3 cycles after i.msiInfo.valid.
  val validsync    = ShiftRegister(i.msiInfo.valid, 3, false.B, true.B)
  // delay one cycle after validsync.
  val validsyncdly = RegNext(validsync)
  // gen of Msivldsync
  val validsyncneg = (!validsync) && validsyncdly

  // RegNext: DFF; RegEnable: DFF with enable func.
  o.msiInfo.valid := RegNext(validsyncneg, init=false.B)
  o.msiInfo.bits  := RegEnable(i.msiInfo.bits, 0.U.asTypeOf(i.msiInfo.bits), validsyncneg)
}
