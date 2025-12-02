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
import os

class CmdComm:

    def api_complite_localfile(self, text):
        """Auto-complete local files

        Args:
            text (string): File name

        Returns:
            list(string): Completion list
        """
        text = text.strip()
        if not text:
            # os.listdir doesn't include '.' or '..', but keep a clear filter for safety
            return [f for f in os.listdir('.') if f not in ('.', '..')]
        path = ""
        fname = text
        if "/" in text:
            path, fname = text.rsplit("/", 1)
        ret = [os.path.join(path, f) for f in os.listdir(path if path else ".") if f.startswith(fname)]
        return [f + ("/" if os.path.isdir(f) else "") for f in ret]

