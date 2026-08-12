# coding=utf-8
"""
Frontend root environment exports.

This module mirrors the role of `MemBlock_env.py` at the package root while
keeping the actual toffee env implementation under `env/`.
"""

import os
import sys


_HERE = os.path.dirname(os.path.abspath(__file__))

if _HERE not in sys.path:
    sys.path.insert(0, _HERE)

from env.runtime.pylib import frontend_pylib_path

_PYLIB_PATH = str(frontend_pylib_path())

for _path in (_PYLIB_PATH,):
    if _path not in sys.path:
        sys.path.insert(0, _path)

from env.runtime.fixtures import env, full_env
from env.core.frontend_env import FrontendEnv


__all__ = ["FrontendEnv", "env", "full_env"]
