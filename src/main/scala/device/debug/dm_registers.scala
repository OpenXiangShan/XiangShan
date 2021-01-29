package freechips.rocketchip.devices.debug

import chisel3._

// This file was auto-generated from the repository at https://github.com/riscv/riscv-debug-spec.git,
// 'make chisel'

object DMI_RegAddrs {
  /* This register reports status for the overall Debug Module as well as
        the currently selected harts, as defined in \Fhasel.  Its address will
        not change in the future, because it contains \Fversion.
  */
  def DMI_DMSTATUS =  0x11

  /* This register controls the overall Debug Module
        as well as the currently selected harts, as defined in \Fhasel.

\label{hartsel}
\index{hartsel}
        Throughout this document we refer to \Fhartsel, which is \Fhartselhi
        combined with \Fhartsello. While the spec allows for 20 \Fhartsel bits,
        an implementation may choose to implement fewer than that. The actual
        width of \Fhartsel is called {\tt HARTSELLEN}. It must be at least 0
        and at most 20. A debugger should discover {\tt HARTSELLEN} by writing
        all ones to \Fhartsel (assuming the maximum size) and reading back the
        value to see which bits were actually set. Debuggers must not change
        \Fhartsel while an abstract command is executing.

        \begin{commentary}
        There are separate \Fsetresethaltreq and \Fclrresethaltreq bits so that
        it is possible to write \Rdmcontrol without changing the halt-on-reset
        request bit for each selected hart, when not all selected harts have
        the same configuration.
        \end{commentary}

        On any given write, a debugger may only write 1 to at most one of the
        following bits: \Fresumereq, \Fhartreset, \Fackhavereset,
        \Fsetresethaltreq, and \Fclrresethaltreq. The others must be written 0.

\label{resethaltreq}
\index{resethaltreq}
        \Fresethaltreq is an optional internal bit of per-hart state that cannot be
        read, but can be written with \Fsetresethaltreq and \Fclrresethaltreq.
  */
  def DMI_DMCONTROL =  0x10

  /* This register gives information about the hart currently
      selected by \Fhartsel.

      This register is optional. If it is not present it should
      read all-zero.

      If this register is included, the debugger can do more with
      the Program Buffer by writing programs which
      explicitly access the {\tt data} and/or {\tt dscratch}
      registers.
  */
  def DMI_HARTINFO =  0x12

  /* This register selects which of the 32-bit portion of the hart array mask
      register (see Section~\ref{hartarraymask}) is accessible in \Rhawindow.
  */
  def DMI_HAWINDOWSEL =  0x14

  /* This register provides R/W access to a 32-bit portion of the
      hart array mask register (see Section~\ref{hartarraymask}).
      The position of the window is determined by \Rhawindowsel. I.e. bit 0
      refers to hart $\Rhawindowsel * 32$, while bit 31 refers to hart
      $\Rhawindowsel * 32 + 31$.

      Since some bits in the hart array mask register may be constant 0, some
      bits in this register may be constant 0, depending on the current value
      of \Fhawindowsel.
  */
  def DMI_HAWINDOW =  0x15

  /* Writing this register while an abstract command is executing causes
        \Fcmderr to be set to 1 (busy) if it is 0.

        \begin{commentary}
            \Fdatacount must be at least 1 to support RV32 harts, 2 to support
            RV64 harts, or 4 to support RV128 harts.
        \end{commentary}
  */
  def DMI_ABSTRACTCS =  0x16

  /* Writes to this register cause the corresponding abstract command to be
        executed.

        Writing this register while an abstract command is executing causes
        \Fcmderr to be set to 1 (busy) if it is 0.

        If \Fcmderr is non-zero, writes to this register are ignored.

        \begin{commentary}
            \Fcmderr inhibits starting a new command to accommodate debuggers
            that, for performance reasons, send several commands to be executed
            in a row without checking \Fcmderr in between. They can safely do
            so and check \Fcmderr at the end without worrying that one command
            failed but then a later command (which might have depended on the
            previous one succeeding) passed.
        \end{commentary}
  */
  def DMI_COMMAND =  0x17

  /* This register is optional. Including it allows more efficient burst
        accesses.  A debugger can detect whether it is support by setting bits
        and reading them back.

        Writing this register while an abstract command is executing causes
        \Fcmderr to be set to 1 (busy) if it is 0.
  */
  def DMI_ABSTRACTAUTO =  0x18

  /* When \Fconfstrptrvalid is set, reading this register returns bits 31:0
      of the configuration string pointer. Reading the other {\tt confstrptr}
      registers returns the upper bits of the address.

      When system bus mastering is implemented, this must be an
      address that can be used with the System Bus Access module. Otherwise,
      this must be an address that can be used to access the
      configuration string from the hart with ID 0.

      If \Fconfstrptrvalid is 0, then the {\tt confstrptr} registers
      hold identifier information which is not
      further specified in this document.

      The configuration string itself is described in the Privileged Spec.
  */
  def DMI_CONFSTRPTR0 =  0x19

  def DMI_CONFSTRPTR1 =  0x1a

  def DMI_CONFSTRPTR2 =  0x1b

  def DMI_CONFSTRPTR3 =  0x1c

  /* If there is more than one DM accessible on this DMI, this register
        contains the base address of the next one in the chain, or 0 if this is
        the last one in the chain.
  */
  def DMI_NEXTDM =  0x1d

  /* \Rdatazero through \Rdataeleven are basic read/write registers that may
        be read or changed by abstract commands. \Fdatacount indicates how many
        of them are implemented, starting at \Rdatazero, counting up.
        Table~\ref{tab:datareg} shows how abstract commands use these
        registers.

        Accessing these registers while an abstract command is executing causes
        \Fcmderr to be set to 1 (busy) if it is 0.

        Attempts to write them while \Fbusy is set does not change their value.

        The values in these registers may not be preserved after an abstract
        command is executed. The only guarantees on their contents are the ones
        offered by the command in question. If the command fails, no
        assumptions can be made about the contents of these registers.
  */
  def DMI_DATA0 =  0x04

  def DMI_DATA11 =  0x0f

  /* \Rprogbufzero through \Rprogbuffifteen provide read/write access to the
        optional program buffer. \Fprogbufsize indicates how many of them are
        implemented starting at \Rprogbufzero, counting up.

        Accessing these registers while an abstract command is executing causes
        \Fcmderr to be set to 1 (busy) if it is 0.

        Attempts to write them while \Fbusy is set does not change their value.
  */
  def DMI_PROGBUF0 =  0x20

  def DMI_PROGBUF15 =  0x2f

  /* This register serves as a 32-bit serial port to/from the authentication
        module.

        When \Fauthbusy is clear, the debugger can communicate with the
        authentication module by reading or writing this register. There is no
        separate mechanism to signal overflow/underflow.
  */
  def DMI_AUTHDATA =  0x30

  /* This register contains DM control and status bits that didn't easily
        fit in \Rdmcontrol and \Rdmstatus. All are optional.
  */
  def DMI_DMCS2 =  0x32

  /* Each bit in this read-only register indicates whether one specific hart
        is halted or not. Unavailable/nonexistent harts are not considered to
        be halted.

        The LSB reflects the halt status of hart \{hartsel[19:5],5'h0\}, and the
        MSB reflects halt status of hart \{hartsel[19:5],5'h1f\}.
  */
  def DMI_HALTSUM0 =  0x40

  /* Each bit in this read-only register indicates whether any of a group of
        harts is halted or not. Unavailable/nonexistent harts are not considered to
        be halted.

        This register may not be present in systems with fewer than
        33 harts.

        The LSB reflects the halt status of harts \{hartsel[19:10],10'h0\}
        through \{hartsel[19:10],10'h1f\}.
        The MSB reflects the halt status of harts \{hartsel[19:10],10'h3e0\}
        through \{hartsel[19:10],10'h3ff\}.
  */
  def DMI_HALTSUM1 =  0x13

  /* Each bit in this read-only register indicates whether any of a group of
        harts is halted or not. Unavailable/nonexistent harts are not considered to
        be halted.

        This register may not be present in systems with fewer than
        1025 harts.

        The LSB reflects the halt status of harts \{hartsel[19:15],15'h0\}
        through \{hartsel[19:15],15'h3ff\}.
        The MSB reflects the halt status of harts \{hartsel[19:15],15'h7c00\}
        through \{hartsel[19:15],15'h7fff\}.
  */
  def DMI_HALTSUM2 =  0x34

  /* Each bit in this read-only register indicates whether any of a group of
        harts is halted or not. Unavailable/nonexistent harts are not considered to
        be halted.

        This register may not be present in systems with fewer than
        32769 harts.

        The LSB reflects the halt status of harts 20'h0 through 20'h7fff.
        The MSB reflects the halt status of harts 20'hf8000 through 20'hfffff.
  */
  def DMI_HALTSUM3 =  0x35

  def DMI_SBCS =  0x38

  /* If \Fsbasize is 0, then this register is not present.

        When the system bus master is busy, writes to this register will set
        \Fsbbusyerror and don't do anything else.

        \begin{steps}{If \Fsberror is 0, \Fsbbusyerror is 0, and \Fsbreadonaddr
        is set then writes to this register start the following:}
            \item Set \Fsbbusy.
            \item Perform a bus read from the new value of {\tt sbaddress}.
            \item If the read succeeded and \Fsbautoincrement is set, increment
            {\tt sbaddress}.
            \item Clear \Fsbbusy.
        \end{steps}
  */
  def DMI_SBADDRESS0 =  0x39

  /* If \Fsbasize is less than 33, then this register is not present.

        When the system bus master is busy, writes to this register will set
        \Fsbbusyerror and don't do anything else.
  */
  def DMI_SBADDRESS1 =  0x3a

  /* If \Fsbasize is less than 65, then this register is not present.

        When the system bus master is busy, writes to this register will set
        \Fsbbusyerror and don't do anything else.
  */
  def DMI_SBADDRESS2 =  0x3b

  /* If \Fsbasize is less than 97, then this register is not present.

        When the system bus master is busy, writes to this register will set
        \Fsbbusyerror and don't do anything else.
  */
  def DMI_SBADDRESS3 =  0x37

  /* If all of the {\tt sbaccess} bits in \Rsbcs are 0, then this register
        is not present.

        Any successful system bus read updates {\tt sbdata}. If the width of
        the read access is less than the width of {\tt sbdata}, the contents of
        the remaining high bits may take on any value.

        If \Fsberror or \Fsbbusyerror both aren't 0 then accesses do nothing.

        If the bus master is busy then accesses set \Fsbbusyerror, and don't do
        anything else.

        \begin{steps}{Writes to this register start the following:}
            \item Set \Fsbbusy.
            \item Perform a bus write of the new value of {\tt sbdata} to {\tt sbaddress}.
            \item If the write succeeded and \Fsbautoincrement is set,
            increment {\tt sbaddress}.
            \item Clear \Fsbbusy.
        \end{steps}

        \begin{steps}{Reads from this register start the following:}
            \item ``Return'' the data.
            \item Set \Fsbbusy.
            \item If \Fsbreadondata is set, perform a system bus read from the
            address contained in {\tt sbaddress}, placing the result in {\tt
            sbdata}.
            \item If \Fsbautoincrement is set, increment {\tt sbaddress}.
            \item Clear \Fsbbusy.
        \end{steps}

        Only \Rsbdatazero has this behavior. The other {\tt sbdata} registers
        have no side effects. On systems that have buses wider than 32 bits, a
        debugger should access \Rsbdatazero after accessing the other {\tt
        sbdata} registers.
  */
  def DMI_SBDATA0 =  0x3c

  /* If \Fsbaccesssixtyfour and \Fsbaccessonetwentyeight are 0, then this
        register is not present.

        If the bus master is busy then accesses set \Fsbbusyerror, and don't do
        anything else.
  */
  def DMI_SBDATA1 =  0x3d

  /* This register only exists if \Fsbaccessonetwentyeight is 1.

        If the bus master is busy then accesses set \Fsbbusyerror, and don't do
        anything else.
  */
  def DMI_SBDATA2 =  0x3e

  /* This register only exists if \Fsbaccessonetwentyeight is 1.

        If the bus master is busy then accesses set \Fsbbusyerror, and don't do
        anything else.
  */
  def DMI_SBDATA3 =  0x3f

}

class DMSTATUSFields extends Bundle {

  val reserved0 = UInt(9.W)

  /* If 1, then there is an implicit {\tt ebreak} instruction at the
            non-existent word immediately after the Program Buffer. This saves
            the debugger from having to write the {\tt ebreak} itself, and
            allows the Program Buffer to be one word smaller.

            This must be 1 when \Fprogbufsize is 1.
  */
  val impebreak = Bool()

  val reserved1 = UInt(2.W)

  /* This field is 1 when all currently selected harts have been reset
            and reset has not been acknowledged for any of them.
  */
  val allhavereset = Bool()

  /* This field is 1 when at least one currently selected hart has been
            reset and reset has not been acknowledged for that hart.
  */
  val anyhavereset = Bool()

  /* This field is 1 when all currently selected harts have acknowledged
            their last resume request.
  */
  val allresumeack = Bool()

  /* This field is 1 when any currently selected hart has acknowledged
            its last resume request.
  */
  val anyresumeack = Bool()

  /* This field is 1 when all currently selected harts do not exist in
            this platform.
  */
  val allnonexistent = Bool()

  /* This field is 1 when any currently selected hart does not exist in
            this platform.
  */
  val anynonexistent = Bool()

  /* This field is 1 when all currently selected harts are unavailable.
  */
  val allunavail = Bool()

  /* This field is 1 when any currently selected hart is unavailable.
  */
  val anyunavail = Bool()

  /* This field is 1 when all currently selected harts are running.
  */
  val allrunning = Bool()

  /* This field is 1 when any currently selected hart is running.
  */
  val anyrunning = Bool()

  /* This field is 1 when all currently selected harts are halted.
  */
  val allhalted = Bool()

  /* This field is 1 when any currently selected hart is halted.
  */
  val anyhalted = Bool()

  /* 0: Authentication is required before using the DM.

            1: The authentication check has passed.

            On components that don't implement authentication, this bit must be
            preset as 1.
  */
  val authenticated = Bool()

  /* 0: The authentication module is ready to process the next
            read/write to \Rauthdata.

            1: The authentication module is busy. Accessing \Rauthdata results
            in unspecified behavior.

            \Fauthbusy only becomes set in immediate response to an access to
            \Rauthdata.
  */
  val authbusy = Bool()

  /* 1 if this Debug Module supports halt-on-reset functionality
            controllable by the \Fsetresethaltreq and \Fclrresethaltreq bits.
            0 otherwise.
  */
  val hasresethaltreq = Bool()

  /* 0: \Rconfstrptrzero--\Rconfstrptrthree hold information which
            is not relevant to the configuration string.

            1: \Rconfstrptrzero--\Rconfstrptrthree hold the address of the
            configuration string.
  */
  val confstrptrvalid = Bool()

  /* 0: There is no Debug Module present.

            1: There is a Debug Module and it conforms to version 0.11 of this
            specification.

            2: There is a Debug Module and it conforms to version 0.13 of this
            specification.

            15: There is a Debug Module but it does not conform to any
            available version of this spec.
  */
  val version = UInt(4.W)

}

class DMCONTROLFields extends Bundle {

  /* Writing 0 clears the halt request bit for all currently selected
            harts. This may cancel outstanding halt requests for those harts.

            Writing 1 sets the halt request bit for all currently selected
            harts. Running harts will halt whenever their halt request bit is
            set.

            Writes apply to the new value of \Fhartsel and \Fhasel.
  */
  val haltreq = Bool()

  /* Writing 1 causes the currently selected harts to resume once, if
            they are halted when the write occurs. It also clears the resume
            ack bit for those harts.

            \Fresumereq is ignored if \Fhaltreq is set.

            Writes apply to the new value of \Fhartsel and \Fhasel.
  */
  val resumereq = Bool()

  /* This optional field writes the reset bit for all the currently
            selected harts.  To perform a reset the debugger writes 1, and then
            writes 0 to deassert the reset signal.

            While this bit is 1, the debugger must not change which harts are
            selected.

            If this feature is not implemented, the bit always stays 0, so
            after writing 1 the debugger can read the register back to see if
            the feature is supported.

            Writes apply to the new value of \Fhartsel and \Fhasel.
  */
  val hartreset = Bool()

  /* 0: No effect.

            1: Clears {\tt havereset} for any selected harts.

            Writes apply to the new value of \Fhartsel and \Fhasel.
  */
  val ackhavereset = Bool()

  val reserved0 = UInt(1.W)

  /* Selects the definition of currently selected harts.

            0: There is a single currently selected hart, that is selected by \Fhartsel.

            1: There may be multiple currently selected harts -- the hart
            selected by \Fhartsel, plus those selected by the hart array mask
            register.

            An implementation which does not implement the hart array mask register
            must tie this field to 0. A debugger which wishes to use the hart array
            mask register feature should set this bit and read back to see if the functionality
            is supported.
  */
  val hasel = Bool()

  /* The low 10 bits of \Fhartsel: the DM-specific index of the hart to
            select. This hart is always part of the currently selected harts.
  */
  val hartsello = UInt(10.W)

  /* The high 10 bits of \Fhartsel: the DM-specific index of the hart to
            select. This hart is always part of the currently selected harts.
  */
  val hartselhi = UInt(10.W)

  val reserved1 = UInt(2.W)

  /* This optional field writes the halt-on-reset request bit for all
            currently selected harts, unless \Fclrresethaltreq is
            simultaneously set to 1.
            When set to 1, each selected hart will halt upon the next deassertion
            of its reset. The halt-on-reset request bit is not automatically
            cleared. The debugger must write to \Fclrresethaltreq to clear it.

            Writes apply to the new value of \Fhartsel and \Fhasel.

            If \Fhasresethaltreq is 0, this field is not implemented.
  */
  val setresethaltreq = Bool()

  /* This optional field clears the halt-on-reset request bit for all
            currently selected harts.

            Writes apply to the new value of \Fhartsel and \Fhasel.
  */
  val clrresethaltreq = Bool()

  /* This bit controls the reset signal from the DM to the rest of the
            system. The signal should reset every part of the system, including
            every hart, except for the DM and any logic required to access the
            DM.
            To perform a system reset the debugger writes 1,
            and then writes 0
            to deassert the reset.
  */
  val ndmreset = Bool()

  /* This bit serves as a reset signal for the Debug Module itself.

            0: The module's state, including authentication mechanism,
            takes its reset values (the \Fdmactive bit is the only bit which can
            be written to something other than its reset value).

            1: The module functions normally.

            No other mechanism should exist that may result in resetting the
            Debug Module after power up, with the possible (but not
            recommended) exception of a global reset signal that resets the
            entire platform.

            A debugger may pulse this bit low to get the Debug Module into a
            known state.

            Implementations may pay attention to this bit to further aid
            debugging, for example by preventing the Debug Module from being
            power gated while debugging is active.
  */
  val dmactive = Bool()

}

class HARTINFOFields extends Bundle {

  val reserved0 = UInt(8.W)

  /* Number of {\tt dscratch} registers available for the debugger
            to use during program buffer execution, starting from \Rdscratchzero.
            The debugger can make no assumptions about the contents of these
            registers between commands.
  */
  val nscratch = UInt(4.W)

  val reserved1 = UInt(3.W)

  /* 0: The {\tt data} registers are shadowed in the hart by CSRs.
            Each CSR is DXLEN bits in size, and corresponds
            to a single argument, per Table~\ref{tab:datareg}.

            1: The {\tt data} registers are shadowed in the hart's memory map.
            Each register takes up 4 bytes in the memory map.
  */
  val dataaccess = Bool()

  /* If \Fdataaccess is 0: Number of CSRs dedicated to
            shadowing the {\tt data} registers.

            If \Fdataaccess is 1: Number of 32-bit words in the memory map
            dedicated to shadowing the {\tt data} registers.

            Since there are at most 12 {\tt data} registers, the value in this
            register must be 12 or smaller.
  */
  val datasize = UInt(4.W)

  /* If \Fdataaccess is 0: The number of the first CSR dedicated to
            shadowing the {\tt data} registers.

            If \Fdataaccess is 1: Signed address of RAM where the {\tt data}
            registers are shadowed, to be used to access relative to \Rzero.
  */
  val dataaddr = UInt(12.W)

}

class HAWINDOWSELFields extends Bundle {

  val reserved0 = UInt(17.W)

  /* The high bits of this field may be tied to 0, depending on how large
          the array mask register is.  E.g.\ on a system with 48 harts only bit 0
          of this field may actually be writable.
  */
  val hawindowsel = UInt(15.W)

}

class HAWINDOWFields extends Bundle {

  val maskdata = UInt(32.W)

}

class ABSTRACTCSFields extends Bundle {

  val reserved0 = UInt(3.W)

  /* Size of the Program Buffer, in 32-bit words. Valid sizes are 0 - 16.
  */
  val progbufsize = UInt(5.W)

  val reserved1 = UInt(11.W)

  /* 1: An abstract command is currently being executed.

            This bit is set as soon as \Rcommand is written, and is
            not cleared until that command has completed.
  */
  val busy = Bool()

  val reserved2 = UInt(1.W)

  /* Gets set if an abstract command fails. The bits in this field remain set until
            they are cleared by writing 1 to them. No abstract command is
            started until the value is reset to 0.

            This field only contains a valid value if \Fbusy is 0.

            0 (none): No error.

            1 (busy): An abstract command was executing while \Rcommand,
            \Rabstractcs, or \Rabstractauto was written, or when one
            of the {\tt data} or {\tt progbuf} registers was read or written.
            This status is only written if \Fcmderr contains 0.

            2 (not supported): The requested command is not supported,
            regardless of whether the hart is running or not.

            3 (exception): An exception occurred while executing the command
            (e.g.\ while executing the Program Buffer).

            4 (halt/resume): The abstract command couldn't execute because the
            hart wasn't in the required state (running/halted), or unavailable.

            5 (bus): The abstract command failed due to a bus error (e.g.\ 
            alignment, access size, or timeout).

            7 (other): The command failed for another reason.
  */
  val cmderr = UInt(3.W)

  val reserved3 = UInt(4.W)

  /* Number of {\tt data} registers that are implemented as part of the
            abstract command interface. Valid sizes are 1 -- 12.
  */
  val datacount = UInt(4.W)

}

class COMMANDFields extends Bundle {

  /* The type determines the overall functionality of this
            abstract command.
  */
  val cmdtype = UInt(8.W)

  /* This field is interpreted in a command-specific manner,
            described for each abstract command.
  */
  val control = UInt(24.W)

}

class ABSTRACTAUTOFields extends Bundle {

  /* When a bit in this field is 1, read or write accesses to the
            corresponding {\tt progbuf} word cause the command in \Rcommand to
            be executed again.
  */
  val autoexecprogbuf = UInt(16.W)

  val reserved0 = UInt(4.W)

  /* When a bit in this field is 1, read or write accesses to the
            corresponding {\tt data} word cause the command in \Rcommand to be
            executed again.
  */
  val autoexecdata = UInt(12.W)

}

class CONFSTRPTR0Fields extends Bundle {

  val addr = UInt(32.W)

}

class NEXTDMFields extends Bundle {

  val addr = UInt(32.W)

}

class DATA0Fields extends Bundle {

  val data = UInt(32.W)

}

class PROGBUF0Fields extends Bundle {

  val data = UInt(32.W)

}

class AUTHDATAFields extends Bundle {

  val data = UInt(32.W)

}

class DMCS2Fields extends Bundle {

  val reserved0 = UInt(21.W)

  /* This field contains the currently selected external trigger.

            If a non-existent trigger value is written here, the hardware will
            change it to a valid one or 0 if no external triggers exist.
  */
  val exttrigger = UInt(4.W)

  /* When \Fhgselect is 0, contains the halt group of the hart
            specified by \Fhartsel.

            When \Fhgselect is 1, contains the halt group of the external
            trigger selected by \Fexttrigger.

            Writes only have an effect if \Fhgwrite is also written 1.

            An implementation may tie any number of upper bits in this field to
            0. If halt groups aren't implemented, then this entire field
            is 0.
  */
  val haltgroup = UInt(5.W)

  /* When \Fhgselect is 0, writing 1 changes the halt group of all
            selected harts to the value written to \Fhaltgroup.

            When \Fhgselect is 1, writing 1 changes the halt group of the
            external trigger selected by \Fexttrigger to the value written to
            \Fhaltgroup.

            Writing 0 has no effect.
  */
  val hgwrite = Bool()

  /* 0: Operate on harts.

            1: Operate on external triggers.

            If there are no external triggers, this field must be tied to 0.
  */
  val hgselect = Bool()

}

class HALTSUM0Fields extends Bundle {

  val haltsum0 = UInt(32.W)

}

class HALTSUM1Fields extends Bundle {

  val haltsum1 = UInt(32.W)

}

class HALTSUM2Fields extends Bundle {

  val haltsum2 = UInt(32.W)

}

class HALTSUM3Fields extends Bundle {

  val haltsum3 = UInt(32.W)

}

class SBCSFields extends Bundle {

  /* 0: The System Bus interface conforms to mainline drafts of this
            spec older than 1 January, 2018.

            1: The System Bus interface conforms to this version of the spec.

            Other values are reserved for future versions.
  */
  val sbversion = UInt(3.W)

  val reserved0 = UInt(6.W)

  /* Set when the debugger attempts to read data while a read is in
            progress, or when the debugger initiates a new access while one is
            already in progress (while \Fsbbusy is set). It remains set until
            it's explicitly cleared by the debugger.

            While this field is set, no more system bus accesses can be
            initiated by the Debug Module.
  */
  val sbbusyerror = Bool()

  /* When 1, indicates the system bus master is busy. (Whether the
            system bus itself is busy is related, but not the same thing.) This
            bit goes high immediately when a read or write is requested for any
            reason, and does not go low until the access is fully completed.

            Writes to \Rsbcs while \Fsbbusy is high result in undefined
            behavior.  A debugger must not write to \Rsbcs until it reads
            \Fsbbusy as 0.
  */
  val sbbusy = Bool()

  /* When 1, every write to \Rsbaddresszero automatically triggers a
            system bus read at the new address.
  */
  val sbreadonaddr = Bool()

  /* Select the access size to use for system bus accesses.

            0: 8-bit

            1: 16-bit

            2: 32-bit

            3: 64-bit

            4: 128-bit

            If \Fsbaccess has an unsupported value when the DM starts a bus
            access, the access is not performed and \Fsberror is set to 4.
  */
  val sbaccess = UInt(3.W)

  /* When 1, {\tt sbaddress} is incremented by the access size (in
            bytes) selected in \Fsbaccess after every system bus access.
  */
  val sbautoincrement = Bool()

  /* When 1, every read from \Rsbdatazero automatically triggers a
            system bus read at the (possibly auto-incremented) address.
  */
  val sbreadondata = Bool()

  /* When the Debug Module's system bus
            master encounters an error, this field gets set. The bits in this
            field remain set until they are cleared by writing 1 to them.
            While this field is non-zero, no more system bus accesses can be
            initiated by the Debug Module.

            An implementation may report ``Other'' (7) for any error condition.

            0: There was no bus error.

            1: There was a timeout.

            2: A bad address was accessed.

            3: There was an alignment error.

            4: An access of unsupported size was requested.

            7: Other.
  */
  val sberror = UInt(3.W)

  /* Width of system bus addresses in bits. (0 indicates there is no bus
            access support.)
  */
  val sbasize = UInt(7.W)

  /* 1 when 128-bit system bus accesses are supported.
  */
  val sbaccess128 = Bool()

  /* 1 when 64-bit system bus accesses are supported.
  */
  val sbaccess64 = Bool()

  /* 1 when 32-bit system bus accesses are supported.
  */
  val sbaccess32 = Bool()

  /* 1 when 16-bit system bus accesses are supported.
  */
  val sbaccess16 = Bool()

  /* 1 when 8-bit system bus accesses are supported.
  */
  val sbaccess8 = Bool()

}

class SBADDRESS0Fields extends Bundle {

  /* Accesses bits 31:0 of the physical address in {\tt sbaddress}.
  */
  val address = UInt(32.W)

}

class SBADDRESS1Fields extends Bundle {

  /* Accesses bits 63:32 of the physical address in {\tt sbaddress} (if
            the system address bus is that wide).
  */
  val address = UInt(32.W)

}

class SBADDRESS2Fields extends Bundle {

  /* Accesses bits 95:64 of the physical address in {\tt sbaddress} (if
            the system address bus is that wide).
  */
  val address = UInt(32.W)

}

class SBADDRESS3Fields extends Bundle {

  /* Accesses bits 127:96 of the physical address in {\tt sbaddress} (if
            the system address bus is that wide).
  */
  val address = UInt(32.W)

}

class SBDATA0Fields extends Bundle {

  /* Accesses bits 31:0 of {\tt sbdata}.
  */
  val data = UInt(32.W)

}

class SBDATA1Fields extends Bundle {

  /* Accesses bits 63:32 of {\tt sbdata} (if the system bus is that
            wide).
  */
  val data = UInt(32.W)

}

class SBDATA2Fields extends Bundle {

  /* Accesses bits 95:64 of {\tt sbdata} (if the system bus is that
            wide).
  */
  val data = UInt(32.W)

}

class SBDATA3Fields extends Bundle {

  /* Accesses bits 127:96 of {\tt sbdata} (if the system bus is that
            wide).
  */
  val data = UInt(32.W)

}

