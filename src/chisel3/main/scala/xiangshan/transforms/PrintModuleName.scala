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

package xiangshan.transforms

import firrtl._
import firrtl.ir._
import firrtl.stage.TransformManager.TransformDependency
import utils.XSLog

class PrintModuleName extends Transform with DependencyAPIMigration {

  // avoid print's check
  override def prerequisites = firrtl.stage.Forms.Checks
  override def invalidates(a: Transform) = false
  override def optionalPrerequisiteOf: Seq[TransformDependency] = firrtl.stage.Forms.HighEmitters

  override protected def execute(state: CircuitState): CircuitState = {

    val c = state.circuit

    def onStmt(s: Statement): Statement = s match {
      case Print(info, StringLit(string), args, clk, en) =>
        Print(info, StringLit(string.replace(XSLog.MagicStr, "%m")), args, clk, en)
      case other: Statement =>
        other.mapStmt(onStmt)
    }

    state.copy(c.mapModule(m => m.mapStmt(onStmt)))

  }

}
