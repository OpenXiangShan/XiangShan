#coding=utf-8

from xspdb.cmd.util import error, message
import os

class CmdInstr:
    """Instruction command class
    """

    def api_decode_instr32(self, instr):
        """Decode a RISC-V instruction into its components.

        Args:
            instr: 32-bit integer/bytes representing the instruction

        Returns:
            Dictionary containing decoded instruction fields:
            {
                'type': str,   # Instruction type (R/I/S/B/U/J)
                'opcode': int, # 7-bit opcode
                'rd': int,     # Destination register (5 bits)
                'rs1': int,    # Source register 1 (5 bits)
                'rs2': int,    # Source register 2 (5 bits)
                'funct3': int, # 3-bit function code
                'funct7': int, # 7-bit function code
                'imm': int     # Immediate value (signed)
                'asm': str     # Assembly representation of the instruction
            }
        """
        if isinstance(instr, bytes):
            instr = int.from_bytes(instr, byteorder='little', signed=False)
        assert isinstance(instr, int), "instr must be a 32-bit integer or bytes"
        # Extract common fields
        opcode = instr & 0x7f
        rd = (instr >> 7) & 0x1f
        funct3 = (instr >> 12) & 0x7
        rs1 = (instr >> 15) & 0x1f
        rs2 = (instr >> 20) & 0x1f
        funct7 = (instr >> 25) & 0x7f

        # Determine instruction type
        instr_type = None
        imm = 0

        # Immediate handling for different formats
        if opcode in [0x37, 0x17]:  # U-type (LUI/AUIPC)
            instr_type = 'U'
            imm = (instr & 0xfffff000)
        elif opcode == 0x6f:        # J-type (JAL)
            instr_type = 'J'
            imm = ((instr >> 31) & 0x1) << 20
            imm |= ((instr >> 21) & 0x3ff) << 1
            imm |= ((instr >> 20) & 0x1) << 11
            imm |= ((instr >> 12) & 0xff) << 12
            imm = (imm << 11) >> 11  # Sign extend
        elif opcode in [0x67, 0x03, 0x13, 0x1b]:  # I-type
            instr_type = 'I'
            imm = (instr >> 20) & 0xfff
            if imm & 0x800:  # Sign extend
                imm |= 0xfffff000
        elif opcode == 0x63:        # B-type
            instr_type = 'B'
            imm = ((instr >> 31) & 0x1) << 12
            imm |= ((instr >> 25) & 0x3f) << 5
            imm |= ((instr >> 8) & 0xf) << 1
            imm |= ((instr >> 7) & 0x1) << 11
            imm = (imm << 19) >> 19  # Sign extend
        elif opcode == 0x23:        # S-type
            instr_type = 'S'
            imm = ((instr >> 25) & 0x7f) << 5
            imm |= (instr >> 7) & 0x1f
            if imm & 0x800:  # Sign extend
                imm |= 0xfffff000
        elif opcode == 0x33:        # R-type
            instr_type = 'R'
        else:
            instr_type = 'unknown'
        try:
            instr_asm   = self.api_dasm_from_bytes(instr.to_bytes(4, byteorder="little", signed=False), 0)[0][2]
        except Exception as e:
            instr_asm = f"unknown"
        return {
            'type': instr_type,
            'opcode': opcode,
            'rd': rd,
            'rs1': rs1,
            'rs2': rs2,
            'funct3': funct3,
            'funct7': funct7,
            'imm': imm,
            "asm": instr_asm
        }

    def do_xload_instr_file(self, arg):
        """Load uint64 strings into memory

        Args:
            arg (file): File to load
        """
        params = arg.strip().split()
        if not len(params) == 2:
            error("load_instr_file <address> <instr_file>")
            return
        if not os.path.exists(params[1]):
            error("file %s not found" % params[1])
            return
        try:
            address = int(params[0], 0)
            self.api_write_bytes(address, self.api_convert_uint64_bytes(params[1]))
            self.info_cache_asm.clear()
        except Exception as e:
            error(f"convert {params[0]} to number fail: {str(e)}")

    def complete_xload_instr_file(self, text, line, begidx, endidx):
        return self.api_complite_localfile(text)

