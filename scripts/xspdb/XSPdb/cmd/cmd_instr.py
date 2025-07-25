#coding=utf-8

from XSPdb.cmd.util import error, message
import os

class CmdInstr:
    """Instruction command class
    """

    def api_decode_instr16(self, instr):
        """Decode a RISC-V compressed (16-bit) instruction.

        Args:
            instr: 16-bit integer/bytes representing the compressed instruction

        Returns:
            Dictionary containing decoded fields:
            {
                'type': str,   # Instruction type (CR/CI/CIW/CL/CS/CA/CB/CJ)
                'opcode': int, # Primary opcode (2 bits)
                'funct3': int, # 3-bit function code
                'rd': int,     # Destination register (normal or compressed)
                'rs1': int,    # Source register 1 (normal or compressed)
                'rs2': int,    # Source register 2 (normal or compressed)
                'imm': int,    # Immediate value (signed)
                'is_compressed': True
                'asm': str,    # Assembly string
            }
        """
        if isinstance(instr, bytes):
            instr = int.from_bytes(instr, byteorder='little', signed=False)
        # Convert to 16-bit unsigned
        instr = instr & 0xffff
        fields = {
            'is_compressed': True,
            'type': 'Unknown',
            'opcode': (instr >> 13) & 0x3,  # Primary opcode (bits 15-13)
            'funct3': (instr >> 13) & 0x7,   # For some instruction types
            'rd': 0,
            'rs1': 0,
            'rs2': 0,
            'imm': 0
        }
        # Helper to expand compressed register numbers
        def expand_reg(compressed_reg):
            return 8 + (compressed_reg & 0x7)
        # Main decoding logic
        op = fields['opcode']
        if op == 0x0:  # CIW format (Quadrant 0)
            fields['type'] = 'CIW'
            fields['rd'] = expand_reg((instr >> 2) & 0x7)  # rd'
            fields['imm'] = ((instr >> 5) & 0x3) << 3 | (instr >> 10) << 5
            fields['imm'] = (fields['imm'] & 0x3f) << 2  # Zero-extended
        elif op == 0x1:  # CI/CL format
            funct3 = (instr >> 13) & 0x7
            if funct3 in [0, 2, 6]:  # CI format
                fields['type'] = 'CI'
                fields['rd'] = expand_reg((instr >> 7) & 0x7)
                imm = ((instr >> 12) & 0x1) << 5 | (instr >> 2) & 0x1f
                if funct3 == 0:  # C.ADDI
                    fields['imm'] = (imm << 26) >> 26  # Sign extend 6-bit
                else:  # C.LI etc.
                    fields['imm'] = imm
            elif funct3 in [1, 3, 5, 7]:  # CL format
                fields['type'] = 'CL'
                fields['rd'] = expand_reg((instr >> 7) & 0x7)
                fields['rs1'] = expand_reg((instr >> 10) & 0x7)
                imm = ((instr >> 5) & 0x3) << 6 | (instr >> 10) & 0x7
                imm = (imm << 25) >> 25  # Sign extend
        elif op == 0x2:  # CR/CS/CB format
            funct4 = (instr >> 12) & 0xf
            if funct4 == 0x8:  # CR format
                fields['type'] = 'CR'
                fields['rd'] = (instr >> 7) & 0x1f
                fields['rs1'] = (instr >> 7) & 0x1f
                fields['rs2'] = (instr >> 2) & 0x1f
            elif funct4 in [0x9, 0xa, 0xb]:  # CS format
                fields['type'] = 'CS'
                fields['rs1'] = expand_reg((instr >> 10) & 0x7)
                fields['rs2'] = expand_reg((instr >> 7) & 0x7)
                imm = (instr >> 2) & 0x1f
                fields['imm'] = imm
            else:  # CB format
                fields['type'] = 'CB'
                fields['rs1'] = expand_reg((instr >> 10) & 0x7)
                imm = ((instr >> 12) & 0x1) << 8 | (instr >> 2) & 0x7 | (instr >> 7) & 0x18
                imm = (imm << 23) >> 23  # Sign extend 9-bit
                fields['imm'] = imm
        elif op == 0x3:  # CJ format
            fields['type'] = 'CJ'
            imm = ((instr >> 12) & 0x1) << 11 | (instr >> 1) & 0x7ff
            imm = (imm << 19) >> 19  # Sign extend 12-bit
            fields['imm'] = imm
        try:
            fields['asm'] = self.api_dasm_from_bytes(instr.to_bytes(2, byteorder="little", signed=False), 0)[0][2]
        except Exception as e:
            fields['asm'] = f"unknown"
        return fields

    def api_encode_instr16(self, fields):
        """Encode compressed instruction fields back to 16-bit machine code.

        Args:
            fields: Dictionary containing decoded fields

        Returns:
            16-bit integer representing the compressed instruction
        """
        instr = 0
        typ = fields['type']
        # Common field handling
        def compress_reg(reg):
            return (reg - 8) & 0x7 if reg >= 8 else reg
        if typ == 'CIW':
            instr |= (0x0 << 13)
            instr |= (compress_reg(fields['rd']) & 0x7) << 2
            imm = (fields['imm'] >> 2) & 0x3f
            instr |= (imm & 0x3) << 5 | (imm >> 3) << 10
        elif typ == 'CI':
            instr |= (0x1 << 13)
            instr |= (fields['funct3'] & 0x7) << 13
            instr |= (compress_reg(fields['rd']) & 0x7) << 7
            imm = fields['imm'] & 0x3f
            instr |= (imm & 0x1f) << 2 | (imm >> 5) << 12
        elif typ == 'CL':
            instr |= (0x1 << 13)
            instr |= (fields['funct3'] & 0x7) << 13
            instr |= (compress_reg(fields['rs1']) & 0x7) << 10
            instr |= (compress_reg(fields['rd']) & 0x7) << 7
            imm = fields['imm'] & 0x7f
            instr |= (imm & 0x3) << 5 | (imm >> 2) << 10
        elif typ == 'CR':
            instr |= (0x2 << 13)
            instr |= 0x8 << 12
            instr |= (fields['rd'] & 0x1f) << 7
            instr |= (fields['rs2'] & 0x1f) << 2
        elif typ == 'CB':
            instr |= (0x2 << 13)
            imm = fields['imm'] & 0x1ff
            instr |= (imm & 0x7) << 2 | (imm >> 3) << 7 | (imm >> 8) << 12
            instr |= (compress_reg(fields['rs1']) & 0x7) << 10
        elif typ == 'CJ':
            instr |= (0x3 << 13)
            imm = fields['imm'] & 0xfff
            instr |= (imm & 0x7ff) << 1 | (imm >> 11) << 12
        return instr & 0xffff

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

    def api_encode_instr32(self, fields):
        """Encode instruction fields back into machine code.

        Args:
            fields: Dictionary containing instruction fields

        Returns:
            32-bit integer representions(int, bytes, asm) of the instruction
        """
        instr = 0
        opcode = fields['opcode']
        instr_type = fields['type']

        # Common fields
        instr |= (opcode & 0x7f)
        instr |= (fields['rd'] & 0x1f) << 7
        instr |= (fields['funct3'] & 0x7) << 12
        instr |= (fields['rs1'] & 0x1f) << 15
        instr |= (fields['rs2'] & 0x1f) << 20
        instr |= (fields['funct7'] & 0x7f) << 25

        # Immediate handling
        imm = fields.get('imm', 0)
        if instr_type == 'U':
            instr |= (imm & 0xfffff000)
        elif instr_type == 'J':
            imm_enc = (imm & 0x100000) >> 20
            imm_enc |= (imm & 0x3ff) << 21
            imm_enc |= (imm & 0x800) >> 1
            imm_enc |= (imm & 0x7ff000) >> 12
            instr |= imm_enc << 12
        elif instr_type == 'I':
            instr |= (imm & 0xfff) << 20
        elif instr_type == 'B':
            imm_enc = (imm & 0x1000) << 19
            imm_enc |= (imm & 0x7e0) << 20
            imm_enc |= (imm & 0x1e) << 7
            imm_enc |= (imm & 0x800) >> 4
            instr |= imm_enc
        elif instr_type == 'S':
            instr |= ((imm & 0xfe0) << 20) | ((imm & 0x1f) << 7)

        return instr & 0xffffffff  # Ensure 32-bit

    def do_xdecode_instr(self, arg):
        """Decode a binary instruction

        Args:
            arg (int or bytes): Instruction data
        """
        arg = arg.strip()
        if not arg:
            error("dasm_instr <instr>")
            return
        try:
            is_compressed = False
            if not arg.startswith("b'"):
                arg = int(arg, 0)
                if (arg & 0x3) != 0x3:
                    is_compressed = True
            else:
                arg = eval(arg)
                if len(arg) == 2:
                    is_compressed = True
            value = self.api_decode_instr16(arg) if is_compressed else self.api_decode_instr32(arg)
            message(str(value))
        except Exception as e:
            error(f"decode {arg} fail: {str(e)}")

    def do_xencode_instr(self, arg):
        """Encode a binary instruction

        Args:
            arg (dict): Instruction item data
        """
        arg = arg.strip()
        if not arg:
            error('encode_instr \{"instr": "<instr>","imm": <imm>,"rs1": <rs1>,"rs2": <rs2>,"rd": <rd>}')
            return
        try:
            arg = eval(arg)
            assert isinstance(arg, dict), "arg must be a dict"
            if arg.get("is_compressed"):
                instr = self.api_encode_instr16(arg)
                instr_bytes = instr.to_bytes(2, byteorder="little", signed=False)
            else:
                instr = self.api_encode_instr32(arg)
                instr_bytes = instr.to_bytes(4, byteorder="little", signed=False)
            try:
                instr_asm   = self.api_dasm_from_bytes(instr_bytes, 0)[0][2]
            except Exception as e:
                instr_asm = "unknown"
            instr_btext = "\\x".join([f"{i:02x}" for i in instr_bytes])
            message(f'asm: {instr_asm}  hex: 0x{instr:04x}    bytes: b\'{instr_btext}\'')
        except Exception as e:
            error(f"encode {arg} fail: {str(e)}")

    def do_xparse_instr_file(self, arg):
        """Parse uint64 strings

        Args:
            arg (file): File to parse
        """
        if not arg:
            message("usage: xparse_instr_file <instr_file>")
            return
        if not os.path.exists(arg):
            error("file %s not found" % arg)
            return
        hex_str = ''.join([f'\\x{byte:02x}' for byte in self.api_convert_uint64_bytes(arg)])
        message(hex_str)

    def complete_xparse_instr_file(self, text, line, begidx, endidx):
        return self.api_complite_localfile(text)

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

    def do_xnop_insert(self, arg):
        """Insert NOP instructions in a specified address range

        Args:
            start (int): Start address
            end (int): End address
        """
        if not arg:
            message("usage: xnop_at <start> <end>")
            return
        args = arg.strip().split()
        if len(args) < 2:
            message("usage: xnop_at <start> <end>")
            return
        try:
            start = int(args[0], 0)
            end = int(args[1], 0)
            assert start < end, "start address must less than end address"
            assert start % 2 == 0, "start address must be aligned to 2"
            assert end % 2 == 0, "end address must be aligned to 2"
            noop_data = bytearray()
            for i in range((end - start) // 2):
                noop_data += b'\x01\x00' # nop
            self.api_write_bytes(start, noop_data)
        except Exception as e:
            error(f"convert {args[0]} or {args[1]} to number fail: {str(e)}")
