#coding=utf-8

from XSPdb.cmd.util import dasm_bytes, error, info, message

class CmdDASM:

    def api_is_flash_address(self, address):
        """Check if the address is in Flash range

        Args:
            address (int): Target address

        Returns:
            bool: True if the address is in Flash range, False otherwise
        """
        return self.flash_base <= address < self.flash_ends

    def api_merge_asm_list_overlap_append(self, a, b):
        if len(b) == 0:
            return a
        if len(a) == 0:
            return b
        b_head = b[0][0]
        a_end_index = -1
        a_size = len(a)
        while abs(a_end_index) <= a_size:
            if a[a_end_index][0] < b_head:
                if a_end_index == -1:
                    return a + b
                else:
                    return a[:a_end_index + 1] + b
            a_end_index -= 1
        return b

    def api_all_data_to_asm(self, address, length):
        """Convert memory data to assembly instructions

        Args:
            address (int): Target memory address
            length (int): Target memory length

        Returns:
            list((address, hex, mnemonic, str)): Disassembly results
        """
        end_address = address + length
        if self.api_is_flash_address(address) and \
           not self.api_is_flash_address(end_address):
            return self.api_merge_asm_list_overlap_append(self.api_flash_data_to_asm(address, self.flash_ends - address),
                                                          self.api_mem_data_to_asm(self.flash_ends, end_address - self.flash_ends))

        if not self.api_is_flash_address(address) and \
           self.api_is_flash_address(end_address):
            return self.api_merge_asm_list_overlap_append(self.api_mem_data_to_asm(address, self.flash_base - address),
                                                          self.api_flash_data_to_asm(self.flash_base, end_address - self.flash_base))

        if self.api_is_flash_address(address):
            return self.api_flash_data_to_asm(address, length)
        else:
            return self.api_mem_data_to_asm(address, length)

    def api_flash_data_to_asm(self, address, length):
        """Convert Flash data to assembly instructions

        Args:
            address (int): Target Flash address
            length (int): Target Flash length

        Returns:
            list((address, hex, mnemonic, str)): Disassembly results
        """
        def _flash_read(addr):
            return self.df.FlashRead(max(0, addr - self.flash_base))
        return self.api_read_data_as_asm(address, length, _flash_read)

    def api_mem_data_to_asm(self, address, length):
        """Convert memory data to assembly instructions

        Args:
            address (int): Target memory address
            length (int): Target memory length

        Returns:
            list((address, hex, mnemonic, str)): Disassembly results
        """
        return self.api_read_data_as_asm(address, length, self.df.pmem_read)

    def api_dasm_from_bytes(self, bytes, start_address=0):
        """Convert binary data to assembly instructions

        Args:
            bytes (bytes): Binary data
            start_address (int): Starting address

        Returns:
            list((address, hex, mnemonic, str)): Disassembly results
        """
        return dasm_bytes(bytes, start_address)

    def api_read_data_as_asm(self, address, length, read_func):
        """Convert memory data to assembly instructions

        Args:
            address (int): Target memory address
            length (int): Target memory length
            read_func (function): Function to read uint64

        Returns:
            list((address, hex, mnemonic, str)): Disassembly results
        """
        dasm_list = []
        try:
            sta_address = address - address % 2                      # Starting memory address must be 2-byte aligned
            end_address = sta_address + (2 + length//2 + length % 2) # Ending memory address must be 2-byte aligned; read at least 2 bytes
            assert sta_address >=0 , "address need >=0 and not miss align"
            assert length >=0, "length need >=0 "

            pmem_sta_address = sta_address - sta_address % 8         # Physical memory reads 8 bytes at a time; must be 8-byte aligned
            pmem_end_address = end_address - end_address % 8         # Physical memory reads 8 bytes at a time; must be 8-byte aligned
            count = 1 + pmem_end_address - pmem_sta_address
            buffer = bytearray()
            for index in range(count):
                padd = pmem_sta_address + 8*index
                buffer += read_func(padd).to_bytes(8, byteorder='little', signed=False)
            # Calculate offset
            offset = sta_address - pmem_sta_address
            for instr in dasm_bytes(buffer[offset:], sta_address):
                    dasm_list.append(instr)
        except Exception as e:
            import traceback
            error(f"disasm fail: {str(e)} {traceback.print_exc()}")
        return dasm_list

    def do_xdasm(self, arg):
        """Disassemble memory data

        Args:
            arg (string): Memory address and length
        """
        if not arg:
            error("dasm <address> [length]")
            return
        args = arg.strip().split()
        length = 10
        if len(args) < 2:
            args.append(str(length))
        try:
            address = int(args[0], 0)
            length = int(args[1])
            for l in self.api_all_data_to_asm(address, length):
                message("0x%x: %s\t%s\t%s" % (l[0], l[1], l[2], l[3]))
        except Exception as e:
            error(f"convert {args[0]} or {args[1]} to number fail: {str(e)}")

    def do_xdasmflash(self, arg):
        """Disassemble Flash data

        Args:
            arg (string): Flash address and length
        """
        if not arg:
            error("dasmflash <address> [length]")
            return
        args = arg.strip().split()
        length = 10
        if len(args) < 2:
            args.append(str(length))
        try:
            address = int(args[0], 0)
            length = int(args[1])
            for l in self.api_flash_data_to_asm(address, length):
                message("0x%x: %s\t%s\t%s" % (l[0], l[1], l[2], l[3]))
        except Exception as e:
            error(f"convert {args[0]} or {args[1]} to number fail: {str(e)}")

    def do_xdasmbytes(self, arg):
        """Disassemble binary data

        Args:
            arg (string): Binary data
        """
        if not arg:
            error("dasmbytes <bytes> [address]")
            return
        try:
            params = arg.strip().split()
            address = 0
            if len(params) > 1:
                address = int(params[1], 0)
            data_bytes = params[0].strip()
            if not data_bytes.startswith("b'"):
                new_data_bytes = "b'"
                for i in range(0, len(data_bytes), 2):
                    new_data_bytes += "\\x%s" % params[0][i:i+2]
                data_bytes = new_data_bytes + "'"
            for i in self.api_dasm_from_bytes(eval(data_bytes), address):
                message("0x%x: %s\t%s\t%s" % (i[0], i[1], i[2], i[3]))
        except Exception as e:
            error(f"convert {arg} to bytes fail: {str(e)}")

    def do_xdasmnumber(self, arg):
        """Disassemble a number

        Args:
            arg (string): Number data
        """
        if not arg:
            error("dasmbytes <number> [address]")
            return
        try:
            params = arg.strip().split()
            address = 0
            if len(params) > 1:
                address = int(params[1], 0)
            for i in self.api_dasm_from_bytes(int(params[0], 0).to_bytes(4, byteorder="little", signed=False), address):
                message("0x%x: %s\t%s\t%s" % (i[0], i[1], i[2], i[3]))
        except Exception as e:
            error(f"convert {arg} to bytes fail: {str(e)}")

    def do_xclear_dasm_cache(self, arg):
        """Clear disassembly cache

        Args:
            arg (None): No arguments
        """
        self.info_cache_asm.clear()
