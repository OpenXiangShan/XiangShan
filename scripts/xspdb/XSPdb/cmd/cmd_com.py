#coding=utf-8

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
            return [f for f in os.listdir('.') if f != '.' or f != '..']
        path = ""
        fname = text
        if "/" in text:
            path, fname = text.rsplit("/", 1)
        ret = [os.path.join(path, f) for f in os.listdir(path if path else ".") if f.startswith(fname)]
        return [f + ("/" if os.path.isdir(f) else "") for f in ret]
