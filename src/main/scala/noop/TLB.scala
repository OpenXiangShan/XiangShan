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

trait tlbSv32Const {
  val debug = false

  val numEntry = 1
  
  val PPN1Len = 12
  val PPN0Len = 10
  val PageSizeLen = 12

  def tlbBundle = new Bundle {
    val VPN  = UInt(20.W)
    val ASID = UInt(9.W)
    val G    = UInt(1.W)
    val PFN  = UInt(22.W)
    val D    = UInt(1.W)
    val A    = UInt(1.W)
    val U    = UInt(1.W)
    val X    = UInt(1.W)
    val W    = UInt(1.W)
    val R    = UInt(1.W)
    val V    = UInt(1.W)
  }
}



