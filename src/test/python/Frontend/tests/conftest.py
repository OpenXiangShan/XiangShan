# coding=utf-8
import os
import sys

_PARENT = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
if _PARENT not in sys.path:
    sys.path.insert(0, _PARENT)

pytest_plugins = [
    'Frontend_api',
    'Frontend_env',
]
