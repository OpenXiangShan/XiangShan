/***************************************************************************************
* Copyright (c) 2025 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2025 Institute of Computing Technology, Chinese Academy of Sciences
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

package chisel3.internal.firrtl.xiangshan

import chisel3.internal.firrtl.ir._

object ChiselCircuitHelpers {

  implicit class CircuitHelper(circuit: Circuit) {
    def mapComponent(f: Component => Component): Circuit = circuit.copy(components = circuit.components.map(f))
  }

  implicit class ComponentHelper(component: Component) {
    def mapCommand(f: Command => Command): Component = component match {
      case DefModule(id, name, public, layers, ports, block) => DefModule(id, name, public, layers, ports, block.mapCommand(f))
      case DefClass(id, name, ports, block) => DefClass(id, name, ports, block.mapCommand(f))
      case other: Component => other
    }

    def foreachCommand(f: Command => Unit): Unit = component match {
      case DefModule(_, _, _, _, _, block) => block.foreachCommand(f)
      case DefClass(_, _, _, block) => block.foreachCommand(f)
      case _: Component =>
    }
  }

  implicit class CommandHelper(command: Command) {
    def mapCommand(f: Command => Command): Command = command match {
      case w: When =>
        val when = new When(w.sourceInfo, w.pred)
        blockMapCommand(w.ifRegion, when.ifRegion, f)
        if (w.hasElse) blockMapCommand(w.elseRegion, when.elseRegion, f)
        when
      case l: LayerBlock =>
        val layerBlock = new LayerBlock(l.sourceInfo, l.layer)
        blockMapCommand(l.region, layerBlock.region, f)
        layerBlock
      case Placeholder(sourceInfo, commands) =>
        val placeholder = new Placeholder(sourceInfo)
        commands.foreach(c => placeholder.getBuffer += f(c))
        placeholder
      case other: Command => other
    }
  }

  implicit class BlockHelper(block: Block) {
    def mapCommand(f: Command => Command): Block = {
      val nb = new Block(block.sourceInfo)
      blockMapCommand(block, nb, f)
      nb
    }

    def foreachCommand(f: Command => Unit): Unit = {
      block.getCommands().foreach(f)
      block.getSecretCommands().foreach(f)
    }
  }

  def blockMapCommand(oldBlk: Block, newBlk: Block, f: Command => Command): Unit = {
    oldBlk.getCommands().foreach(c => newBlk.addCommand(f(c)))
    oldBlk.getSecretCommands().foreach(sc => newBlk.addSecretCommand(f(sc)))
    newBlk.close()
  }
}
