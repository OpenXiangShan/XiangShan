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

package utils

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.TLPermissions._
import freechips.rocketchip.tilelink.{TLBundle, TLBundleA, TLBundleB, TLBundleC, TLBundleD, TLBundleE, TLChannel}

trait HasTLDump {

  implicit val p: Parameters

  implicit class TLDump(channel: TLChannel) {
    def dump = channel match {
      case a: TLBundleA =>
        printChannelA(a)
      case b: TLBundleB =>
        printChannelB(b)
      case c: TLBundleC =>
        printChannelC(c)
      case d: TLBundleD =>
        printChannelD(d)
      case e: TLBundleE =>
        printChannelE(e)
    }
  }

  def printChannelA(a: TLBundleA): Unit = {
    switch(a.opcode) {
      is(PutFullData) {
        XSDebug(false, true.B,
          a.channelName + " PutFullData param: %x size: %x source: %d address: %x mask: %x data: %x corrupt: %b\n",
          a.param, a.size, a.source, a.address, a.mask, a.data, a.corrupt
        )
      }

      is(PutPartialData) {
        XSDebug(false, true.B,
          a.channelName + " PutPartialData param: %x size: %x source: %d address: %x mask: %x data: %x corrupt: %b\n",
          a.param, a.size, a.source, a.address, a.mask, a.data, a.corrupt
        )
      }

      is(ArithmeticData) {
        XSDebug(false, true.B,
          a.channelName + " ArithmeticData param: %x size: %x source: %d address: %x mask: %x data: %x corrupt: %b\n",
          a.param, a.size, a.source, a.address, a.mask, a.data, a.corrupt
        )
      }

      is(LogicalData) {
        XSDebug(false, true.B,
          a.channelName + " LogicalData param: %x size: %x source: %d address: %x mask: %x data: %x corrupt: %b\n",
          a.param, a.size, a.source, a.address, a.mask, a.data, a.corrupt
        )
      }

      is(Get) {
        XSDebug(false, true.B,
          a.channelName + " Get param: %x size: %x source: %d address: %x mask: %x data: %x corrupt: %b\n",
          a.param, a.size, a.source, a.address, a.mask, a.data, a.corrupt
        )
      }

      is(Hint) {
        XSDebug(false, true.B,
          a.channelName + " Intent param: %x size: %x source: %d address: %x mask: %x data: %x corrupt: %b\n",
          a.param, a.size, a.source, a.address, a.mask, a.data, a.corrupt
        )
      }

      is(AcquireBlock) {
        switch(a.param) {
          is(NtoB) {
            XSDebug(false, true.B,
              a.channelName + " AcquireBlock NtoB size: %x source: %d address: %x mask: %x data: %x corrupt: %b\n",
              a.size, a.source, a.address, a.mask, a.data, a.corrupt
            )
          }
          is(NtoT) {
            XSDebug(false, true.B,
              a.channelName + " AcquireBlock NtoT size: %x source: %d address: %x mask: %x data: %x corrupt: %b\n",
              a.size, a.source, a.address, a.mask, a.data, a.corrupt
            )
          }
          is(BtoT) {
            XSDebug(false, true.B,
              a.channelName + " AcquireBlock BtoT size: %x source: %d address: %x mask: %x data: %x corrupt: %b\n",
              a.size, a.source, a.address, a.mask, a.data, a.corrupt
            )
          }
        }
      }

      is(AcquirePerm) {
        switch(a.param) {
          is(NtoB) {
            XSDebug(false, true.B,
              a.channelName + " AcquirePerm NtoB size: %x source: %d address: %x mask: %x data: %x corrupt: %b\n",
              a.size, a.source, a.address, a.mask, a.data, a.corrupt
            )
          }
          is(NtoT) {
            XSDebug(false, true.B,
              a.channelName + " AcquirePerm NtoT size: %x source: %d address: %x mask: %x data: %x corrupt: %b\n",
              a.size, a.source, a.address, a.mask, a.data, a.corrupt
            )
          }
          is(BtoT) {
            XSDebug(false, true.B,
              a.channelName + " AcquirePerm BtoT size: %x source: %d address: %x mask: %x data: %x corrupt: %b\n",
              a.size, a.source, a.address, a.mask, a.data, a.corrupt
            )
          }
        }
      }

    }
  }

  def printChannelB(b: TLBundleB): Unit = {
    switch(b.opcode) {
      is(PutFullData) {
        XSDebug(false, true.B,
          b.channelName + " PutFullData param: %x size: %x source: %d address: %x mask: %x data: %x corrupt: %b\n",
          b.param, b.size, b.source, b.address, b.mask, b.data, b.corrupt
        )
      }

      is(PutPartialData) {
        XSDebug(false, true.B,
          b.channelName + " PutPartialData param: %x size: %x source: %d address: %x mask: %x data: %x corrupt: %b\n",
          b.param, b.size, b.source, b.address, b.mask, b.data, b.corrupt
        )
      }

      is(ArithmeticData) {
        XSDebug(false, true.B,
          b.channelName + " ArithmeticData param: %x size: %x source: %d address: %x mask: %x data: %x corrupt: %b\n",
          b.param, b.size, b.source, b.address, b.mask, b.data, b.corrupt
        )
      }

      is(LogicalData) {
        XSDebug(false, true.B,
          b.channelName + " LogicalData param: %x size: %x source: %d address: %x mask: %x data: %x corrupt: %b\n",
          b.param, b.size, b.source, b.address, b.mask, b.data, b.corrupt
        )
      }

      is(Get) {
        XSDebug(false, true.B,
          b.channelName + " Get param: %x size: %x source: %d address: %x mask: %x data: %x corrupt: %b\n",
          b.param, b.size, b.source, b.address, b.mask, b.data, b.corrupt
        )
      }

      is(Hint) {
        XSDebug(false, true.B,
          b.channelName + " Intent param: %x size: %x source: %d address: %x mask: %x data: %x corrupt: %b\n",
          b.param, b.size, b.source, b.address, b.mask, b.data, b.corrupt
        )
      }

      is(Probe) {
        switch(b.param) {
          is(toN) {
            XSDebug(false, true.B,
              b.channelName + " Probe toN size: %x source: %d address: %x mask: %x data: %x corrupt: %b\n",
              b.size, b.source, b.address, b.mask, b.data, b.corrupt
            )
          }
          is(toB) {
            XSDebug(false, true.B,
              b.channelName + " Probe toB size: %x source: %d address: %x mask: %x data: %x corrupt: %b\n",
              b.size, b.source, b.address, b.mask, b.data, b.corrupt
            )
          }
          is(toT) {
            XSDebug(false, true.B,
              b.channelName + " Probe toT size: %x source: %d address: %x mask: %x data: %x corrupt: %b\n",
              b.size, b.source, b.address, b.mask, b.data, b.corrupt
            )
          }
        }
      }

    }
  }

  def printChannelC(c: TLBundleC): Unit = {
    switch(c.opcode) {
      is(AccessAck) {
        XSDebug(false, true.B,
          c.channelName + " AccessAck param: %x size: %x source: %d address: %x data: %x corrupt: %b\n",
          c.param, c.size, c.source, c.address, c.data, c.corrupt
        )
      }

      is(AccessAckData) {
        XSDebug(false, true.B,
          c.channelName + " AccessAckData param: %x size: %x source: %d address: %x data: %x corrupt: %b\n",
          c.param, c.size, c.source, c.address, c.data, c.corrupt
        )
      }

      is(HintAck) {
        XSDebug(false, true.B,
          c.channelName + " HintAck param: %x size: %x source: %d address: %x data: %x corrupt: %b\n",
          c.param, c.size, c.source, c.address, c.data, c.corrupt
        )
      }

      is(ProbeAck) {
        switch(c.param) {
          is(TtoB) {
            XSDebug(false, true.B,
              c.channelName + " ProbeAck TtoB size: %x source: %d address: %x data: %x corrupt: %b\n",
              c.size, c.source, c.address, c.data, c.corrupt
            )
          }
          is(TtoN) {
            XSDebug(false, true.B,
              c.channelName + " ProbeAck TtoN size: %x source: %d address: %x data: %x corrupt: %b\n",
              c.size, c.source, c.address, c.data, c.corrupt
            )
          }
          is(BtoN) {
            XSDebug(false, true.B,
              c.channelName + " ProbeAck BtoN size: %x source: %d address: %x data: %x corrupt: %b\n",
              c.size, c.source, c.address, c.data, c.corrupt
            )
          }
          is(TtoT) {
            XSDebug(false, true.B,
              c.channelName + " ProbeAck TtoT size: %x source: %d address: %x data: %x corrupt: %b\n",
              c.size, c.source, c.address, c.data, c.corrupt
            )
          }
          is(BtoB) {
            XSDebug(false, true.B,
              c.channelName + " ProbeAck BtoB size: %x source: %d address: %x data: %x corrupt: %b\n",
              c.size, c.source, c.address, c.data, c.corrupt
            )
          }
          is(NtoN) {
            XSDebug(false, true.B,
              c.channelName + " ProbeAck NtoN size: %x source: %d address: %x data: %x corrupt: %b\n",
              c.size, c.source, c.address, c.data, c.corrupt
            )
          }
        }
      }

      is(ProbeAckData) {
        switch(c.param) {
          is(TtoB) {
            XSDebug(false, true.B,
              c.channelName + " ProbeAckData TtoB size: %x source: %d address: %x data: %x corrupt: %b\n",
              c.size, c.source, c.address, c.data, c.corrupt
            )
          }
          is(TtoN) {
            XSDebug(false, true.B,
              c.channelName + " ProbeAckData TtoN size: %x source: %d address: %x data: %x corrupt: %b\n",
              c.size, c.source, c.address, c.data, c.corrupt
            )
          }
          is(BtoN) {
            XSDebug(false, true.B,
              c.channelName + " ProbeAckData BtoN size: %x source: %d address: %x data: %x corrupt: %b\n",
              c.size, c.source, c.address, c.data, c.corrupt
            )
          }
          is(TtoT) {
            XSDebug(false, true.B,
              c.channelName + " ProbeAckData TtoT size: %x source: %d address: %x data: %x corrupt: %b\n",
              c.size, c.source, c.address, c.data, c.corrupt
            )
          }
          is(BtoB) {
            XSDebug(false, true.B,
              c.channelName + " ProbeAckData BtoB size: %x source: %d address: %x data: %x corrupt: %b\n",
              c.size, c.source, c.address, c.data, c.corrupt
            )
          }
          is(NtoN) {
            XSDebug(false, true.B,
              c.channelName + " ProbeAckData NtoN size: %x source: %d address: %x data: %x corrupt: %b\n",
              c.size, c.source, c.address, c.data, c.corrupt
            )
          }
        }
      }

      is(Release) {
        switch(c.param) {
          is(TtoB) {
            XSDebug(false, true.B,
              c.channelName + " Release TtoB size: %x source: %d address: %x data: %x corrupt: %b\n",
              c.size, c.source, c.address, c.data, c.corrupt
            )
          }
          is(TtoN) {
            XSDebug(false, true.B,
              c.channelName + " Release TtoN size: %x source: %d address: %x data: %x corrupt: %b\n",
              c.size, c.source, c.address, c.data, c.corrupt
            )
          }
          is(BtoN) {
            XSDebug(false, true.B,
              c.channelName + " Release BtoN size: %x source: %d address: %x data: %x corrupt: %b\n",
              c.size, c.source, c.address, c.data, c.corrupt
            )
          }
          is(TtoT) {
            XSDebug(false, true.B,
              c.channelName + " Release TtoT size: %x source: %d address: %x data: %x corrupt: %b\n",
              c.size, c.source, c.address, c.data, c.corrupt
            )
          }
          is(BtoB) {
            XSDebug(false, true.B,
              c.channelName + " Release BtoB size: %x source: %d address: %x data: %x corrupt: %b\n",
              c.size, c.source, c.address, c.data, c.corrupt
            )
          }
          is(NtoN) {
            XSDebug(false, true.B,
              c.channelName + " Release NtoN size: %x source: %d address: %x data: %x corrupt: %b\n",
              c.size, c.source, c.address, c.data, c.corrupt
            )
          }
        }
      }

      is(ReleaseData) {
        switch(c.param) {
          is(TtoB) {
            XSDebug(false, true.B,
              c.channelName + " ReleaseData TtoB size: %x source: %d address: %x data: %x corrupt: %b\n",
              c.size, c.source, c.address, c.data, c.corrupt
            )
          }
          is(TtoN) {
            XSDebug(false, true.B,
              c.channelName + " ReleaseData TtoN size: %x source: %d address: %x data: %x corrupt: %b\n",
              c.size, c.source, c.address, c.data, c.corrupt
            )
          }
          is(BtoN) {
            XSDebug(false, true.B,
              c.channelName + " ReleaseData BtoN size: %x source: %d address: %x data: %x corrupt: %b\n",
              c.size, c.source, c.address, c.data, c.corrupt
            )
          }
          is(TtoT) {
            XSDebug(false, true.B,
              c.channelName + " ReleaseData TtoT size: %x source: %d address: %x data: %x corrupt: %b\n",
              c.size, c.source, c.address, c.data, c.corrupt
            )
          }
          is(BtoB) {
            XSDebug(false, true.B,
              c.channelName + " ReleaseData BtoB size: %x source: %d address: %x data: %x corrupt: %b\n",
              c.size, c.source, c.address, c.data, c.corrupt
            )
          }
          is(NtoN) {
            XSDebug(false, true.B,
              c.channelName + " ReleaseData NtoN size: %x source: %d address: %x data: %x corrupt: %b\n",
              c.size, c.source, c.address, c.data, c.corrupt
            )
          }
        }
      }

    }
  }

  def printChannelD(d: TLBundleD): Unit = {
    switch(d.opcode) {
      is(AccessAck) {
        XSDebug(false, true.B,
          d.channelName + " AccessAck param: %x size: %x source: %d sink: %d denied: %b data: %x corrupt: %b\n",
          d.param, d.size, d.source, d.sink, d.denied, d.data, d.corrupt
        )
      }

      is(AccessAckData) {
        XSDebug(false, true.B,
          d.channelName + " AccessAckData param: %x size: %x source: %d sink: %d denied: %b data: %x corrupt: %b\n",
          d.param, d.size, d.source, d.sink, d.denied, d.data, d.corrupt
        )
      }

      is(HintAck) {
        XSDebug(false, true.B,
          d.channelName + " HintAck param: %x size: %x source: %d sink: %d denied: %b data: %x corrupt: %b\n",
          d.param, d.size, d.source, d.sink, d.denied, d.data, d.corrupt
        )
      }

      is(Grant) {
        switch(d.param) {
          is(toT) {
            XSDebug(false, true.B,
              d.channelName + " Grant toT size: %x source: %d sink: %d denied: %b data: %x corrupt: %b\n",
              d.size, d.source, d.sink, d.denied, d.data, d.corrupt
            )
          }
          is(toB) {
            XSDebug(false, true.B,
              d.channelName + " Grant toB size: %x source: %d sink: %d denied: %b data: %x corrupt: %b\n",
              d.size, d.source, d.sink, d.denied, d.data, d.corrupt
            )
          }
          is(toN) {
            XSDebug(false, true.B,
              d.channelName + " Grant toN size: %x source: %d sink: %d denied: %b data: %x corrupt: %b\n",
              d.size, d.source, d.sink, d.denied, d.data, d.corrupt
            )
          }
        }
      }

      is(GrantData) {
        switch(d.param) {
          is(toT) {
            XSDebug(false, true.B,
              d.channelName + " GrantData toT size: %x source: %d sink: %d denied: %b data: %x corrupt: %b\n",
              d.size, d.source, d.sink, d.denied, d.data, d.corrupt
            )
          }
          is(toB) {
            XSDebug(false, true.B,
              d.channelName + " GrantData toB size: %x source: %d sink: %d denied: %b data: %x corrupt: %b\n",
              d.size, d.source, d.sink, d.denied, d.data, d.corrupt
            )
          }
          is(toN) {
            XSDebug(false, true.B,
              d.channelName + " GrantData toN size: %x source: %d sink: %d denied: %b data: %x corrupt: %b\n",
              d.size, d.source, d.sink, d.denied, d.data, d.corrupt
            )
          }
        }
      }

      is(ReleaseAck) {
        XSDebug(false, true.B,
          d.channelName + " ReleaseAck param: %x size: %x source: %d sink: %d denied: %b data: %x corrupt: %b\n",
          d.param, d.size, d.source, d.sink, d.denied, d.data, d.corrupt
        )
      }

    }
  }

  def printChannelE(e: TLBundleE): Unit = {
    XSDebug(false, true.B, e.channelName + "GrantAck sink: %d\n", e.sink)
  }

}
