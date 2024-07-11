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
import freechips.rocketchip.amba.axi4.AXI4Bundle

class AXI4LiteBundleA(addrWidth: Int, idWidth: Int = 0) extends Bundle {
  val id = UInt(idWidth.W)
  val addr = UInt(addrWidth.W)
}

class AXI4LiteBundleAR(addrWidth: Int, idWidth: Int = 0) extends AXI4LiteBundleA(addrWidth, idWidth)

class AXI4LiteBundleAW(addrWidth: Int, idWidth: Int = 0) extends AXI4LiteBundleA(addrWidth, idWidth)

class AXI4LiteBundleW(dataWidth: Int) extends Bundle {
  val data = UInt(dataWidth.W)
}

class AXI4LiteBundleR(dataWidth: Int, idWidth: Int = 0) extends Bundle {
  val id = UInt(idWidth.W)
  val data = UInt(dataWidth.W)
  val resp = UInt(2.W)
}

class AXI4LiteBundleB(idWidth: Int = 0) extends Bundle {
  val id = UInt(idWidth.W)
  val resp = UInt(2.W)
}

class AXI4LiteBundle(val addrWidth: Int, val dataWidth: Int, val idWidth: Int = 0) extends Bundle {
  val aw = Irrevocable(new AXI4LiteBundleAW(addrWidth, idWidth))
  val w  = Irrevocable(new AXI4LiteBundleW(dataWidth))
  val b  = Flipped(Irrevocable(new AXI4LiteBundleB(idWidth)))
  val ar = Irrevocable(new AXI4LiteBundleAR(addrWidth, idWidth))
  val r  = Flipped(Irrevocable(new AXI4LiteBundleR(dataWidth, idWidth)))

  private def connectExisting(left: Bundle, right: Bundle): Unit = {
    for ((name, data) <- left.elements)
      if (right.elements.contains(name))
        data := right.elements(name)
      else
        data := (name match {
          case "size" => log2Ceil(dataWidth).U
          case "last" => true.B.asTypeOf(data)
          case _: String => 0.U.asTypeOf(data)
        })
  }

  def connectToAXI4(axi4: AXI4Bundle): Unit = {
    axi4.aw.valid := aw.valid
    aw.ready := axi4.aw.ready
    connectExisting(axi4.aw.bits, aw.bits)

    axi4.w.valid := w.valid
    w.ready := axi4.w.ready
    connectExisting(axi4.w.bits, w.bits)

    axi4.ar.valid := ar.valid
    ar.ready := axi4.ar.ready
    connectExisting(axi4.ar.bits, ar.bits)

    b.valid := axi4.b.valid
    axi4.b.ready := b.ready
    connectExisting(b.bits, axi4.b.bits)

    r.valid := axi4.r.valid
    axi4.r.ready := r.ready
    connectExisting(r.bits, axi4.r.bits)
  }

  def connectFromAXI4(axi4: AXI4Bundle): Unit = {
    aw.valid := axi4.aw.valid
    axi4.aw.ready := aw.ready
    connectExisting(aw.bits, axi4.aw.bits)

    w.valid := axi4.w.valid
    axi4.w.ready := w.ready
    connectExisting(w.bits, axi4.w.bits)

    ar.valid := axi4.ar.valid
    axi4.ar.ready := ar.ready
    connectExisting(ar.bits, axi4.ar.bits)

    axi4.b.valid := b.valid
    b.ready := axi4.b.ready
    connectExisting(axi4.b.bits, b.bits)

    axi4.r.valid := r.valid
    r.ready := axi4.r.ready
    connectExisting(axi4.r.bits, r.bits)
  }
}
