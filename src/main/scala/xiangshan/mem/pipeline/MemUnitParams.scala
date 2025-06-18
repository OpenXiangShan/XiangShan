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

package xiangshan.mem

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.mem.Bundles._
import xiangshan.backend.fu.NewCSR.MemType

sealed trait MemUnitType

case class Std() extends MemUnitType
case class Sta() extends MemUnitType
case class Ldu() extends MemUnitType
case class Hyu() extends MemUnitType
case class Amo() extends MemUnitType

case class MemUnitParams(
  name:             String        = "MemUnit",
  unitType:         MemUnitType,
  dataBits:         Int           = 128, // ignore it
  exceptionOut:     Seq[Int]      = Seq(),
  triggerType:      Int           = MemType.LOAD
) {
  def isStd: Boolean = unitType == Std()

  def isSta: Boolean = unitType == Sta()

  def isLdu: Boolean = unitType == Ldu()

  def isHyu: Boolean = unitType == Hyu()

  def isAmo: Boolean = unitType == Amo()

  def unitTypeString: String = unitType match {
    case Std() => "Store Data"
    case Sta() => "Store Addr"
    case Ldu() => "Load"
    case Hyu() => "Hybrid"
    case Amo() => "Atomics"
    case _     => throw new IllegalArgumentException("Unknown unit type")
  }
}
