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

// See LICENSE.SiFive for license details.

package xiangshan

import freechips.rocketchip.diplomacy._

trait HasXSDts {
  this: XSCore =>

  val device: SimpleDevice = new SimpleDevice("cpu", Seq("ICT,xiangshan", "riscv")) {
    override def parent: Some[Device] = Some(ResourceAnchors.cpus)

    def cpuProperties: PropertyMap = Map(
      "device_type" -> "cpu".asProperty,
      "status" -> "okay".asProperty,
      "clock-frequency" -> 0.asProperty,
      "riscv,isa" -> "rv64imafdc".asProperty,
      "timebase-frequency" -> 1000000.asProperty
    )

    def tileProperties: PropertyMap = {
      val dcache = if(coreParams.dcacheParametersOpt.nonEmpty) Map(
        "d-cache-block-size" -> dcacheParameters.blockBytes.asProperty,
        "d-cache-sets" -> dcacheParameters.nSets.asProperty,
        "d-cache-size" -> (dcacheParameters.nSets * dcacheParameters.nWays * dcacheParameters.blockBytes).asProperty
      ) else Map()

      val icache = Map(
        "i-cache-block-size" -> icacheParameters.blockBytes.asProperty,
        "i-cache-sets" -> icacheParameters.nSets.asProperty,
        "i-cache-size" -> (icacheParameters.nSets * icacheParameters.nWays * icacheParameters.blockBytes).asProperty
      )

      val dtlb = Map(
        "d-tlb-size" -> (ldtlbParams.normalNSets * ldtlbParams.normalNWays).asProperty,
        "d-tlb-sets" -> 1.asProperty
      )

      val itlb = Map(
        "i-tlb-size" -> (itlbParams.normalNSets * itlbParams.normalNWays).asProperty,
        "i-tlb-sets" -> 1.asProperty
      )

      val mmu = Map(
        "tlb-split" -> Nil,
        "mmu-type" -> s"riscv,sv$VAddrBits".asProperty
      )

      val pmp = Nil

      dcache ++ icache ++ dtlb ++ itlb ++ mmu ++ pmp
    }

    def nextLevelCacheProperty: PropertyOption = {
      if(coreParams.dcacheParametersOpt.nonEmpty){
        val outer = memBlock.dcache.clientNode.edges.out.flatMap(_.manager.managers)
          .filter(_.supportsAcquireB)
          .flatMap(_.resources.headOption)
          .map(_.owner.label)
          .distinct
        if (outer.isEmpty) None
        else Some("next-level-cache" -> outer.map(l => ResourceReference(l)).toList)
      } else None
    }

    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      Description(name, mapping ++ cpuProperties ++ nextLevelCacheProperty ++ tileProperties)
    }
  }
  ResourceBinding {
    Resource(device, "reg").bind(ResourceAddress(hardId))
  }
}
