package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import bus.simplebus._
import bus.axi4._
import utils._

//no details about tlb of riscv32 is found. So just do as wanted
//only find instruction SFENCE.VMA(Supervisor fence. vma
//in mips, there is an MASK reg which control the page size. just ignore it and have 4KB for deault 
//mips gs132:
//| EntryHi     | PageMask |    |   EntryLo0   |   EntryLo1   |
//| VPN2 | ASID | PageMask | G  | PFN0 | C/D/V | PFN1 | C/D/V |
//| 19b  | 8b   | 12b      | 1b | 20b  | 5b    | 20b  | 5b    |

//fist step, the tlb only have one 
//riscv32 tlb:
//| VPN | ASID |  G  | PFN | D/A/U/X/W/R/V |
//| 20b | 9b   |  1b | 22b | 7b            |

object TLBOpType {
  def vma = "b0".U
}

