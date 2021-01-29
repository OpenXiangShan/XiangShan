package freechips.rocketchip.devices.debug

import chisel3._

// This file was auto-generated from the repository at https://github.com/riscv/riscv-debug-spec.git,
// 'make chisel'

object AC_RegAddrs {
}

class ACCESS_REGISTERFields extends Bundle {

  /* This is 0 to indicate Access Register Command.
  */
  val cmdtype = UInt(8.W)

  val reserved0 = UInt(1.W)

  /* 2: Access the lowest 32 bits of the register.

            3: Access the lowest 64 bits of the register.

            4: Access the lowest 128 bits of the register.

            If \Fsize specifies a size larger than the register's actual size,
            then the access must fail. If a register is accessible, then reads of \Fsize
            less than or equal to the register's actual size must be supported.

            This field controls the Argument Width as referenced in
            Table~\ref{tab:datareg}.
  */
  val size = UInt(3.W)

  val reserved1 = UInt(1.W)

  /* When 1, execute the program in the Program Buffer exactly once
            after performing the transfer, if any.
  */
  val postexec = Bool()

  /* 0: Don't do the operation specified by \Fwrite.

            1: Do the operation specified by \Fwrite.

            This bit can be used to just execute the Program Buffer without
            having to worry about placing valid values into \Fsize or \Fregno.
  */
  val transfer = Bool()

  /* When \Ftransfer is set:
            0: Copy data from the specified register into {\tt arg0} portion
               of {\tt data}.

            1: Copy data from {\tt arg0} portion of {\tt data} into the
               specified register.
  */
  val write = Bool()

  /* Number of the register to access, as described in
          Table~\ref{tab:regno}.
          \Rdpc may be used as an alias for PC if this command is
          supported on a non-halted hart.
  */
  val regno = UInt(16.W)

}

class QUICK_ACCESSFields extends Bundle {

  /* This is 1 to indicate Quick Access command.
  */
  val cmdtype = UInt(8.W)

  val reserved0 = UInt(24.W)

}

