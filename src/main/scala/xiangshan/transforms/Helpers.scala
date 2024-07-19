/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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

object Helpers {

  implicit class CircuitHelper(circuit: firrtl.ir.Circuit) {
    def mapModule(f: firrtl.ir.DefModule => firrtl.ir.DefModule): firrtl.ir.Circuit = circuit.copy(modules = circuit.modules.map(f))
  }

  implicit class DefModuleHelper(defModule: firrtl.ir.DefModule) {
    def mapStmt(f: firrtl.ir.Statement => firrtl.ir.Statement): firrtl.ir.DefModule = defModule match {
      case firrtl.ir.Module(info, name, ports, body) => firrtl.ir.Module(info, name, ports, f(body))
      case firrtl.ir.DefClass(info, name, ports, body) => firrtl.ir.DefClass(info, name, ports, f(body))
      case other: firrtl.ir.DefModule => other
    }

    def foreachStmt(f: firrtl.ir.Statement => Unit): Unit = defModule match {
      case firrtl.ir.Module(_, _, _, body) => f(body)
      case firrtl.ir.DefClass(_, _, _, body) => f(body)
      case _: firrtl.ir.DefModule =>
    }
  }

  implicit class StatementHelper(statement: firrtl.ir.Statement) {
    def mapStmt(f: firrtl.ir.Statement => firrtl.ir.Statement): firrtl.ir.Statement = statement match {
      case firrtl.ir.Conditionally(info, pred, conseq, alt) => firrtl.ir.Conditionally(info, pred, f(conseq), f(alt))
      case firrtl.ir.Block(stmts) => 
        val res = new scala.collection.mutable.ArrayBuffer[firrtl.ir.Statement]()
        var its = stmts.iterator :: Nil
        while (its.nonEmpty) {
          val it = its.head
          if (it.hasNext) {
            it.next() match {
              case firrtl.ir.EmptyStmt => // flatten out
              case b: firrtl.ir.Block =>
                its = b.stmts.iterator :: its
              case other =>
                res.append(f(other))
            }
          } else {
            its = its.tail
          }
        }
        firrtl.ir.Block(res.toSeq)
      case other: firrtl.ir.Statement => other
    }
  }
}
