# coding=utf-8
"""Frontend verification environment pytest global config."""

import os
import sys

_TEST_ROOT = os.path.dirname(os.path.abspath(__file__))
_REPO_ROOT = os.path.abspath(os.path.join(_TEST_ROOT, '..', '..', '..', '..'))
_PYLIB_PATH = os.path.join(_REPO_ROOT, 'build-frontend', 'pylib')
_DATA_DIR = os.path.join(_TEST_ROOT, 'data')

for _path in (_PYLIB_PATH, _TEST_ROOT):
    if _path not in sys.path:
        sys.path.insert(0, _path)

os.makedirs(_DATA_DIR, exist_ok=True)
