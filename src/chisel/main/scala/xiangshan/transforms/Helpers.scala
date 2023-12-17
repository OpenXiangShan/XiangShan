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

import firrtl.ir._

object Helpers {

  implicit class CircuitHelper(circuit: Circuit) {
    def mapModule(f: DefModule => DefModule): Circuit = circuit.copy(modules = circuit.modules.map(f))
  }

  implicit class DefModuleHelper(defModule: DefModule) {
    def mapStmt(f: Statement => Statement): DefModule = defModule match {
      case Module(info, name, ports, body) => Module(info, name, ports, f(body))
      case DefClass(info, name, ports, body) => DefClass(info, name, ports, f(body))
      case other: DefModule => other
    }

    def foreachStmt(f: Statement => Unit): Unit = defModule match {
      case Module(_, _, _, body) => f(body)
      case DefClass(_, _, _, body) => f(body)
      case _: DefModule =>
    }
  }

  implicit class StatementHelper(statement: Statement) {
    def mapStmt(f: Statement => Statement): Statement = statement match {
      case Conditionally(info, pred, conseq, alt) => Conditionally(info, pred, f(conseq), f(alt))
      case Block(stmts) => 
        val res = new scala.collection.mutable.ArrayBuffer[Statement]()
        var its = stmts.iterator :: Nil
        while (its.nonEmpty) {
          val it = its.head
          if (it.hasNext) {
            it.next() match {
              case EmptyStmt => // flatten out
              case b: Block =>
                its = b.stmts.iterator :: its
              case other =>
                res.append(f(other))
            }
          } else {
            its = its.tail
          }
        }
        Block(res.toSeq)
      case other: Statement => other
    }
  }
}
