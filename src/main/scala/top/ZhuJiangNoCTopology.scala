package top

import xijiang.{Node, NodeParam, NodeType}
import zhujiang.ZJParameters
import zhujiang.device.AxiDeviceParams

object ZhuJiangNoCTopology {
  def apply(numCores: Int, base: ZJParameters = ZJParameters()): ZJParameters = {
    apply(numCores, base, base.cfgAxiDataBits)
  }

  def apply(numCores: Int, base: ZJParameters, cfgAxiDataBits: Int): ZJParameters = {
    require(numCores == 1 || numCores == 2, "ZhuJiang system config currently supports one or two cores")
    base.copy(
      nodeNidBits = 8,
      nodeAidBits = 3,
      cfgAxiDataBits = cfgAxiDataBits,
      nodeParams = if (numCores == 1) singleCoreNodeParams else dualCoreNodeParams,
      tfbParams = None
    )
  }

  private def singleCoreNodeParams: Seq[NodeParam] = Seq(
    NodeParam(nodeType = NodeType.HF, bankId = 0, hfpId = 0),
    NodeParam(nodeType = NodeType.CC, socket = "sync"),
    NodeParam(nodeType = NodeType.HF, bankId = 1, hfpId = 0),
    NodeParam(nodeType = NodeType.RI, axiDevParams = Some(AxiDeviceParams(wrapper = "east", attr = "main"))),
    NodeParam(nodeType = NodeType.HI, axiDevParams = Some(AxiDeviceParams(wrapper = "east", attr = "main")), defaultHni = true),
    NodeParam(nodeType = NodeType.HF, bankId = 1, hfpId = 1),
    NodeParam(nodeType = NodeType.S, axiDevParams = Some(AxiDeviceParams(wrapper = "south", attr = "mem_0"))),
    NodeParam(nodeType = NodeType.HF, bankId = 0, hfpId = 1),
    NodeParam(nodeType = NodeType.M, axiDevParams = Some(AxiDeviceParams(wrapper = "misc", attr = "hwa"))),
    NodeParam(nodeType = NodeType.P)
  )

  private def dualCoreNodeParams: Seq[NodeParam] = Seq(
    NodeParam(nodeType = NodeType.HF, bankId = 0, hfpId = 0),
    NodeParam(nodeType = NodeType.CC, socket = "sync"),
    NodeParam(nodeType = NodeType.HF, bankId = 1, hfpId = 0),
    NodeParam(nodeType = NodeType.CC, socket = "sync"),
    NodeParam(nodeType = NodeType.RI, axiDevParams = Some(AxiDeviceParams(wrapper = "east", attr = "main"))),
    NodeParam(nodeType = NodeType.HI, axiDevParams = Some(AxiDeviceParams(wrapper = "east", attr = "main")), defaultHni = true),
    NodeParam(nodeType = NodeType.HF, bankId = 1, hfpId = 1),
    NodeParam(nodeType = NodeType.S, axiDevParams = Some(AxiDeviceParams(wrapper = "south", attr = "mem_0"))),
    NodeParam(nodeType = NodeType.HF, bankId = 0, hfpId = 1),
    NodeParam(nodeType = NodeType.M, axiDevParams = Some(AxiDeviceParams(wrapper = "misc", attr = "hwa"))),
    NodeParam(nodeType = NodeType.P)
  )
}
