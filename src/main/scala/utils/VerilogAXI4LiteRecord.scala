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

package utils

import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._
import scala.collection.immutable._

class VerilogAXI4LiteRecord(val addrWidth: Int, val dataWidth: Int, val idWidth: Int = 0) extends Record {
  private val axi4LiteBundle = new AXI4LiteBundle(addrWidth, dataWidth, idWidth)
  private def traverseAndMap(data: (String, Data)): SeqMap[String, Data] = {
    data match {
      case (name: String, node: Bundle) => SeqMap.from(
        node.elements.map(x => traverseAndMap(x)).flatten.map {
          case (nodeName, node) => (s"${name.replace("bits", "")}${nodeName}", node)
        }
      )
      case (name: String, node: Data) => SeqMap(name -> node)
    }
  }
  private val outputPattern = "^(aw|w|ar).*".r
  private val elems = traverseAndMap("", axi4LiteBundle) map { case (name, node) => name match {
    case outputPattern(_) => (name, Output(node))
    case _: String        => (name, Input (node))
  }} map { case (name, node) => name match {
    case s"${_}ready" => (name, Flipped(node))
    case _: String    => (name, node)
  }}
  def elements: SeqMap[String, Data] = elems
}

object VerilogAXI4LiteRecord {
  private val elementsMap: Seq[(VerilogAXI4LiteRecord, AXI4LiteBundle) => (Data, Data)] = {
    val names = (new VerilogAXI4LiteRecord(1, 1, 0)).elements.map(_._1)
    val pattern = "^(aw|w|b|ar|r)(.*)".r
    names.map { name => { (verilog: VerilogAXI4LiteRecord, chisel: AXI4LiteBundle) => {
      val (channel: Record, signal: String) = name match {
        case pattern(prefix, signal) =>
          (chisel.elements(prefix).asInstanceOf[Record], signal)
        case _: String => require(false, "unexpected prefix"); null
      }
      verilog.elements(name) -> channel.elements.applyOrElse(signal,
        channel.elements("bits").asInstanceOf[Record].elements)
    }}}.toSeq
  }
  implicit val axi4View: DataView[VerilogAXI4LiteRecord, AXI4LiteBundle] = DataView[VerilogAXI4LiteRecord, AXI4LiteBundle](
    vab => new AXI4LiteBundle(vab.addrWidth, vab.dataWidth, vab.idWidth), elementsMap: _*
  )
  implicit val axi4View2: DataView[AXI4LiteBundle, VerilogAXI4LiteRecord] =
    axi4View.invert(ab => new VerilogAXI4LiteRecord(ab.addrWidth, ab.dataWidth, ab.idWidth))
}
