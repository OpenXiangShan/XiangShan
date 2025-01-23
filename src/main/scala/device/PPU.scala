package device

import chisel3._
import chisel3.util._
import chisel3.experimental._
import utility._

class PPU extends Module {
  val io = IO(new Bundle {
    // Inputs
    val coreActive = Input(Bool())  // Core active signal
    val coreWakeReq = Input(Bool()) // Core wake request signal

    // Outputs
    val coreReset = Output(Bool())  // Core reset output
    val coreIsolate = Output(Bool()) // Core isolation output 
    val corePowerOff = Output(Bool()) // Core power off output 
    val coreClken = Output(Bool())  // Core clock enable output 
    val coreHWstat = Output(UInt(8.W)) // Core hardware status output (POFF, PON, etc.)
  })

  dontTouch(io.coreIsolate)
  dontTouch(io.corePowerOff)
  dontTouch(io.coreHWstat)

  // FSM states
  val POFF     = "b00000001".U(8.W)  
  val POFF_CLK = "b00000010".U(8.W)  
  val POFF_ISO = "b00000100".U(8.W)  
  val POFF_RST = "b00001000".U(8.W)  
  val PON_ISO  = "b00100000".U(8.W)  
  val PON_RST  = "b01000000".U(8.W)  
  val PON      = "b10000000".U(8.W)  

  // Define the FSM
  val sPOFF :: sPOFF_CLK :: sPOFF_ISO :: sPOFF_RST :: sPON_WAIT :: sPON_ISO :: sPON_RST :: sPON :: Nil = Enum(8) 
  val state = RegInit(sPON)

  // Configurable delays in clock cycles (use appropriate values for custom design)
  val PON_DLY_CLKEN_ISO = 70.U(32.W) // PowerOn delay for clockEn -> Isolate
  val PON_DLY_ISO_RST = 70.U(32.W)   // PowerOn delay for Isolate -> Reset 
  val PON_DLY_ISO_CLKEN = 70.U(32.W) // PowerOff delay for Isolate -> clockEn
  val PON_DLY_CLKEN_RST = 70.U(32.W) // PowerOff delay for clockEn -> Reset
  val PON_DLY_POWER_STABLE = 70.U(32.W) // PowerOff delay for clockEn -> Reset

  // Delay counters (in clock cycles)
  val delayCounter = RegInit(0.U(32.W)) 
  val delayMax = 10.U 

  // Control signals
  val nResetReg = RegInit(true.B)       //1: no Reset       0: Reset
  val nIsolateReg = RegInit(true.B)     //1: no Isolate     0: Isolate
  val nPowerUpReg = RegInit(false.B)    //1: Power off      0: power on
  val coreClkenReg = RegInit(true.B)    //1: clock Enabled  0: clock not enable

  // Output control signals
  io.coreReset := !nResetReg   
  io.coreIsolate := nIsolateReg  
  io.corePowerOff := nPowerUpReg  
  io.coreClken := coreClkenReg
  io.coreHWstat := state

  // FSM logic for power on/off sequence
  switch(state) {
    //Power Off Sequence
    is(sPON) {
      // Set nPowerUpReg = 0 to Power on
      nPowerUpReg := false.B 
      
      when(io.coreActive === false.B) {
        state := sPOFF_CLK
      }
    }
    is(sPOFF_CLK) {
      // Step 1: Set coreClken = 0 to turn off clock
      coreClkenReg := false.B
      delayCounter := PON_DLY_CLKEN_ISO
      state := sPOFF_ISO
    }
    is(sPOFF_ISO) {
      when(delayCounter === 0.U) {
        // Step 2: Set nIsolateReg =0 to Isolate output after PDN_DLY_CLKEN_ISO
        delayCounter := PON_DLY_ISO_RST
        nIsolateReg := false.B
        state := sPOFF_RST
      }
    }
    is(sPOFF_RST) {
      when(delayCounter === 0.U) {
        // Step 3: Set nResetReg = 0 to Reset after PDN_DLY_ISO_RST
        delayCounter := 0.U
        nResetReg := false.B
        state := sPOFF
      }
    }

    //Power On Sequence
    is(sPOFF) {
      nPowerUpReg := true.B  // Set nPowerUpReg = 1 to Power off
      // Power on sequence (coreWakeReq = 1)
      when(io.coreWakeReq === true.B) {
        // Step 0:  Turn on the power and wait PON_DLY_POWER_STABLE to begin power-on sequence
        nPowerUpReg := false.B  //Set nPowerUpReg = 0 to Power on
        delayCounter := PON_DLY_POWER_STABLE
        state := sPON_WAIT 
      }
    }
    is(sPON_WAIT) {
      when(delayCounter === 0.U) {
        // Step 1: Set nIsolateReg = 1 to Disable isolation
        nIsolateReg := true.B
        delayCounter := PON_DLY_ISO_CLKEN
        state := sPON_ISO
      }
    }
    is(sPON_ISO) {
      when(delayCounter === 0.U) {
        // Step 2: Set coreClken =1 to turn on clock after PON_DLY_CLKEN_ISO
        delayCounter := PON_DLY_CLKEN_RST
        coreClkenReg := true.B
        state := sPON_RST
      }
    }
    is(sPON_RST) {
      when(delayCounter === 0.U) {
        // Step 3: Set nResetReg = 1 to exit Reset after PON_DLY_CLKEN_RST
        delayCounter := 0.U
        nResetReg := true.B
        state := sPON
      }
    }
  }

  // Delay counter management
  when(delayCounter > 0.U) {
    delayCounter := delayCounter - 1.U
  }

  //PCSM signals used to control power
  val nPOWERUP = RegNextN(nPowerUpReg, 5, Some(true.B))
  val nISOLATE = RegNextN(nIsolateReg, 5, Some(true.B))
}
