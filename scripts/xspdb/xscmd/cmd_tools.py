#***************************************************************************************
# Copyright (c) 2025 Beijing Institute of Open Source Chip (BOSC)
# Copyright (c) 2025 Institute of Computing Technology, Chinese Academy of Sciences
#
# XiangShan is licensed under Mulan PSL v2.
# You can use this software according to the terms and conditions of the Mulan PSL v2.
# You may obtain a copy of Mulan PSL v2 at:
#          http://license.coscl.org.cn/MulanPSL2
#
# THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
# EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
# MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
#
# See the Mulan PSL v2 for more details.
#***************************************************************************************


from . import message, error
import ast

class CmdTools:

    def api_convert_uint64_bytes(self, file_name):
        """Parse uint64 strings

        Args:
            file_name (file): File to parse
        """
        ret = bytearray()
        with open(file_name, "r") as f:
            for l in f:
                l = l.strip()
                if not l:
                    continue
                for v in l.split():
                    if not v.startswith("0x"):
                        v = "0x" + v
                    ccount = len(v) - 2
                    assert ccount % 2 == 0, f"invalid hex string: {v}"
                    ret += int(v, 0).to_bytes(ccount//2, byteorder='little', signed=False)
        return ret

    def do_xbytes2number(self, arg):
        """Convert bytes to an integer

        Args:
            arg (string): Bytes data
        """
        if not arg:
            error("bytes2number <bytes>")
            return
        try:
            data_bytes = arg.strip()
            if not data_bytes.startswith("b'"):
                new_data_bytes = "b'"
                for i in range(0, len(data_bytes), 2):
                    new_data_bytes += "\\x%s" % data_bytes[i:i+2]
                data_bytes = new_data_bytes + "'"
            message(f'{int.from_bytes(ast.literal_eval(data_bytes), byteorder="little", signed=False):x}')
        except Exception as e:
            error(f"convert {arg} to bytes fail: {str(e)}")

    def do_xnumber2bytes(self, arg):
        """Convert an integer to bytes

        Args:
            arg (string): Integer data
        """
        if not arg:
            error("number2bytes <number>")
            return
        try:
            data = int(arg, 0)
            message(f'b"{data.to_bytes(4, byteorder="little", signed=False).hex()}"')
        except Exception as e:
            error(f"convert {arg} to bytes fail: {str(e)}")
