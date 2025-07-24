#coding=utf-8

from XSPdb.cmd.util import message, error

class CmdCSR:
    """CSR command class"""

    def do_xset_mpc(self, arg):
        """Set the jump address (by mpc) after Flash initialization, default is 0x80000000

        Args:
            arg (string): Register name and value
        """
        args = arg.strip().split()
        if len(args) < 2:
            message("usage: xset_mpc <value>")
            return
        try:
            self.api_set_flash_int_regs({"mpc": int(args[1], 0)})
        except Exception as e:
            error(f"set_mpc fail: {str(e)}")

    def do_xget_mpc(self, arg):
        """Get the jump address after Flash initialization, default is 0x80000000

        Args:
            arg (None): No arguments
        """
        mpc = self.api_get_flash_init_iregs()
        for r in mpc:
            if r[0] == "mpc":
                message(f"mpc: {hex(r[1])}", end=" ")
        message("")
