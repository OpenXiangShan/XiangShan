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

package device

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.AddressSet
import utils._
import utility._
import difftest._

class AXI4UART16550(
  address: Seq[AddressSet]
)(implicit p: Parameters)
  extends AXI4SlaveModule(address, executable = false, _extra = new UARTIO)
{
  override lazy val module = new AXI4SlaveModuleImp[UARTIO](this) {

    // 16550 UART Registers
    val RBR = RegInit(0.U(8.W))     // Receiver Buffer Register (read-only)
    val THR = RegInit(0.U(8.W))     // Transmitter Holding Register (write-only)
    val IER = RegInit(0.U(8.W))     // Interrupt Enable Register
    val FCR = RegInit(0.U(8.W))     // FIFO Control Register (write-only)
    val LCR = RegInit(0.U(8.W))     // Line Control Register (only bit 7 used)
    val LSR = RegInit(0.U(8.W))     // Line Status Register (only bits 0,5,6 used)
    val DLL = RegInit(0.U(8.W))     // Divisor Latch Low
    val DLH = RegInit(0.U(8.W))     // Divisor Latch High
    val MCR = RegInit(0.U(8.W))     // Modem Control Register
    val MSR = RegInit(0.U(8.W))     // Modem Status Register
    val SCR = RegInit(0.U(8.W))     // Scratch Register
    val USR = RegInit(0.U(8.W))     // USR register

    // Constants from serial-16550.c
    val DTR = 0x01.U(8.W)
    val RTS = 0x02.U(8.W)

    // LSR state management
    val THR_empty = RegInit(true.B)
    val transmitter_empty = RegInit(true.B)

    // RX FIFO (input only)
    val RX_FIFO_SIZE = 16
    val rx_fifo = RegInit(VecInit(Seq.fill(RX_FIFO_SIZE)(0.U(8.W))))
    val rx_head = RegInit(0.U(log2Ceil(RX_FIFO_SIZE).W))
    val rx_tail = RegInit(0.U(log2Ceil(RX_FIFO_SIZE).W))
    val rx_empty = rx_head === rx_tail

    // RBR reads from RX FIFO
    val RBR_data = Mux(rx_empty, 0.U(8.W), rx_fifo(rx_head))

    // LCR - Only implement DLAB bit (bit 7)
    val DLAB = LCR(7)  // Divisor Latch Access Bit

    // LSR - Only implement required bits
    val LSR_DR = 0x01.U(8.W)        // Bit 0: Data Ready
    val LSR_THRE = 0x20.U(8.W)      // Bit 5: Transmitter Holding Register Empty
    val LSR_TEMT = 0x40.U(8.W)      // Bit 6: Transmitter Empty (THR + TSR)

    // LSR final value - only set the required bits
    LSR := Mux(THR_empty, LSR_THRE, 0.U(8.W)) |         // Bit 5: THR Empty
           Mux(transmitter_empty, LSR_TEMT, 0.U(8.W))   // Bit 6: Transmitter Empty

    // THR write detection - THR is at offset 0x00 when DLAB=0
    val THR_write = (waddr(3,0) === 0x00.U) && in.w.fire && !DLAB
    val THR_data = in.w.bits.data(7,0)  // Always extract from first 8 bits

    // Debug: Log all writes to UART16550
    when (in.w.fire) {
      printf("UART16550: Write to addr=0x%x, data=0x%x, DLAB=%d, strb=0x%x\n", waddr(3,0), in.w.bits.data(7,0), DLAB, in.w.bits.strb)
    }

    // Update LSR state when THR is written
    when (THR_write) {
      THR_empty := false.B
      transmitter_empty := false.B
      printf("UART16550: THR write, data = 0x%x (char = %c)\n", THR_data, THR_data)
    }

    // Simulate immediate completion of transmission
    when (!THR_empty) {
      THR_empty := true.B
      transmitter_empty := true.B
    }

    // RX FIFO dequeue on read (RBR read)
    when ((raddr(3,0) === 0x00.U) && in.r.fire && !rx_empty && !DLAB) {
      rx_head := Mux(rx_head === (RX_FIFO_SIZE-1).U, 0.U, rx_head + 1.U)
    }

    // RX FIFO enqueue on external input
    when (io.extra.get.in.valid) {
      val next_tail = Mux(rx_tail === (RX_FIFO_SIZE-1).U, 0.U, rx_tail + 1.U)
      when (next_tail =/= rx_head) {  // Not full
        rx_fifo(rx_tail) := io.extra.get.in.ch
        rx_tail := next_tail
      }
    }

    // Input/Output signal management
    io.extra.get.out.valid := THR_write
    io.extra.get.out.ch := THR_data
    io.extra.get.in.valid := (raddr(3,0) === 0x00.U) && in.r.fire && !DLAB

    // Debug: Log output signals
    when (io.extra.get.out.valid) {
      printf("UART16550: Output valid=1, ch=0x%x (%c)\n", io.extra.get.out.ch, io.extra.get.out.ch)
    }

    // Create separate registers for DLAB-multiplexed values
    val reg_0x00 = RegInit(0.U(32.W))  // DLL or RBR
    val reg_0x04 = RegInit(0.U(32.W))  // DLH or IER
    
    // Update DLAB-multiplexed registers based on DLAB state
    reg_0x00 := Mux(DLAB, Cat(0.U(24.W), DLL), Cat(0.U(24.W), RBR_data))
    reg_0x04 := Mux(DLAB, Cat(0.U(24.W), DLH), Cat(0.U(24.W), IER))
    
    // Create 32-bit registers for all UART registers
    val reg_0x08 = RegInit(0.U(32.W))  // FCR
    val reg_0x0c = RegInit(0.U(32.W))  // LCR
    val reg_0x10 = RegInit(0.U(32.W))  // MCR
    val reg_0x14 = RegInit(0.U(32.W))  // LSR (read-only)
    val reg_0x18 = RegInit(0.U(32.W))  // MSR (read-only)
    val reg_0x1c = RegInit(0.U(32.W))  // SCR
    val reg_0x7c = RegInit(0.U(32.W))  // USR (read-only)
    
    // Update registers with 8-bit values padded to 32-bit
    reg_0x08 := Cat(0.U(24.W), FCR)
    reg_0x0c := Cat(0.U(24.W), LCR)
    reg_0x10 := Cat(0.U(24.W), MCR)
    reg_0x14 := Cat(0.U(24.W), LSR)
    reg_0x18 := Cat(0.U(24.W), MSR)
    reg_0x1c := Cat(0.U(24.W), SCR)
    reg_0x7c := Cat(0.U(24.W), USR)
    
    // Register mapping - 4-byte aligned as per serial-16550.c
    val mapping = Map(
      RegMap(0x00, reg_0x00),                                              // DLL or RBR
      RegMap(0x04, reg_0x04),                                              // DLH or IER
      RegMap(0x08, reg_0x08),                                              // FCR
      RegMap(0x0c, reg_0x0c),                                              // LCR
      RegMap(0x10, reg_0x10),                                              // MCR
      RegMap(0x14, reg_0x14, RegMap.Unwritable),                           // LSR (read-only)
      RegMap(0x18, reg_0x18, RegMap.Unwritable),                           // MSR (read-only)
      RegMap(0x1c, reg_0x1c),                                              // SCR
      RegMap(0x7c, reg_0x7c, RegMap.Unwritable)                            // USR (read-only)
    )

    // Handle writes to all registers
    when (in.w.fire) {
      switch (waddr(3,0)) {
        is (0x00.U) { 
          when (DLAB) { 
            DLL := in.w.bits.data(7,0)  // DLL write when DLAB=1
          }
        }
        is (0x04.U) { 
          when (DLAB) { 
            DLH := in.w.bits.data(7,0)  // DLH write when DLAB=1
          }
          .otherwise { 
            IER := in.w.bits.data(7,0)  // IER write when DLAB=0
          }
        }
        is (0x08.U) { FCR := in.w.bits.data(7,0) }  // FCR write
        is (0x0c.U) { LCR := in.w.bits.data(7,0) }  // LCR write
        is (0x10.U) { MCR := in.w.bits.data(7,0) }  // MCR write
        is (0x1c.U) { SCR := in.w.bits.data(7,0) }  // SCR write
        // LSR, MSR, USR are read-only
      }
    }
    
    // Debug: Print initialization sequence to match serial-16550.c
    when (in.w.fire) {
      switch (waddr(3,0)) {
        is (0x04.U) { 
          when (!DLAB) {
            printf("UART16550: IER write = 0x%x\n", in.w.bits.data(7,0))
          }
        }
        is (0x0c.U) { 
          printf("UART16550: LCR write = 0x%x\n", in.w.bits.data(7,0))
        }
        is (0x08.U) { 
          printf("UART16550: FCR write = 0x%x\n", in.w.bits.data(7,0))
        }
        is (0x10.U) { 
          printf("UART16550: MCR write = 0x%x\n", in.w.bits.data(7,0))
        }
      }
    }
    
    // Debug: Print USR register access
    when (in.r.fire && (raddr(3,0) === 0x7c.U)) {
      printf("UART16550: USR read, returning 0x%x\n", USR)
    }

    RegMap.generate(mapping, raddr(3,0), in.r.bits.data,
      waddr(3,0), in.w.fire, in.w.bits.data, MaskExpand(in.w.bits.strb >> waddr(2,0))
    )
  }
}