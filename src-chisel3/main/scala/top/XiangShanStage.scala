/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
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

package top

import chisel3.stage._
import firrtl.stage._
import firrtl.options.Shell
import xstransforms._

class XiangShanStage extends ChiselStage {
  override val shell: Shell = new Shell("xiangshan")
    with XiangShanCli
    with ChiselCli
    with FirrtlCli

  trait XiangShanCli { this: Shell =>
    parser.note("XiangShan Options")
    DisablePrintfAnnotation.addOptions(parser)
    EnablePrintfAnnotation.addOptions(parser)
    DisableAllPrintAnnotation.addOptions(parser)
    RemoveAssertAnnotation.addOptions(parser)
  }
}
