# coding=utf-8
"""Thin Frontend package env adapter."""

import os
import sys

_HERE = os.path.dirname(os.path.abspath(__file__))
_REPO_ROOT = os.path.abspath(os.path.join(_HERE, '..', '..', '..', '..'))
_PYLIB_PATH = os.path.join(_REPO_ROOT, 'build-frontend', 'pylib')

for _path in (_PYLIB_PATH, _HERE):
    if _path not in sys.path:
        sys.path.insert(0, _path)

from env.fixtures import env, full_env  # noqa: F401
from env.frontend_env import FrontendEnv  # noqa: F401
